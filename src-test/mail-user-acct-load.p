{globals.i}

DEFINE VARIABLE mString AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErr AS INT64 NO-UNDO.
DEFINE VARIABLE mOk AS INT64 NO-UNDO.
DEFINE VARIABLE mCreate AS LOG NO-UNDO.
DEFINE VARIABLE mFname AS CHARACTER NO-UNDO.


DEFINE BUFFER b-mail-user FOR mail-user.
DEFINE BUFFER b-catalog FOR catalog.


{getfile.i &filename='*.*' &mode=must-exist }

mFName = SEARCH(pick-value).

{setdest.i &filename = "'mail-user-acct-load.log'" }

IF mFName EQ ? THEN 
DO:
   PUT UNFORMATTED SUBSTITUTE("Файл не выбран!") SKIP.
   RETURN "".
END.


PUT UNFORMATTED TODAY SKIP.
INPUT FROM VALUE(mFName).

REPEAT:
   IMPORT UNFORMATTED mString.
   mErr = 0.
   mCreate = No.
   mAcct = "".

   FOR EACH acct WHERE
      acct.number   EQ TRIM(mString)
      NO-LOCK:

      ASSIGN
         mErr = mErr + 1
         mAcct = acct.acct
         mFil = acct.filial.

   END.

   IF mErr EQ 0 THEN
      PUT UNFORMATTED SUBSTITUTE("Счет &1 не найден!",mString) SKIP.

   IF mErr GT 1 THEN
      PUT UNFORMATTED SUBSTITUTE("Счет &1 найден в нескольких филиалах!",mString) SKIP.

   IF mErr EQ 1 THEN
   DO:
      FIND FIRST acct WHERE acct.acct EQ macct.
      IF GetxAttrValueEx("acct", macct + "," + acct.currency, "bclient", "") NE "Да" THEN
      DO:
         PUT UNFORMATTED SUBSTITUTE("Счет &1 не имеет признака BCLIENT !",mString) SKIP.
         NEXT.
      END.
      mCreate = Yes.
      FOR EACH mail-user WHERE      
         mail-user.op-kind-exp EQ "e-cl940"                                              
         AND mail-user.module EQ 'mail-cli'                                       
         NO-LOCK:      
         IF INDEX(mail-user.acct,mAcct) > 0 THEN 
         DO:
            PUT UNFORMATTED SUBSTITUTE("Уже есть правило обмена &2 со счетом &1 в филиале &3 !!!", mAcct, mail-user.mail-user-num,mail-user.filial-id) SKIP.
            mCreate = No.
            LEAVE.
         END. 
      END.
      IF mCreate THEN 
      DO:
      FOR EACH mail-user WHERE      
         mail-user.op-kind-exp EQ "e-cl940"                                              
         AND mail-user.filial-id EQ mfil
         AND mail-user.module EQ 'mail-cli'   
         AND mail-user.cust-cat EQ acct.cust-cat
         AND mail-user.cust-id EQ acct.cust-id
         EXCLUSIVE-LOCK:      
         mail-user.acct = TRIM(REPLACE(mail-user.acct + "," + mAcct,",,",","),","). 
         IF INDEX(mail-user.acct,mAcct) > 0 THEN 
         DO:
            PUT UNFORMATTED SUBSTITUTE("Счет &1 добавлен в правило обмена &2 в филиале &3", mAcct, mail-user.mail-user-num,mail-user.filial-id) SKIP.
            mOk = mOk + 1.
            mCreate = No.
            LEAVE.
         END. 
      END.
      IF mCreate THEN 
      DO:
      FOR EACH mail-user WHERE      
         mail-user.op-kind-exp EQ "e-cl940"                                              
         AND mail-user.module EQ 'mail-cli'                                       
         AND mail-user.filial-id EQ mfil
         AND  mail-user.cust-cat EQ acct.cust-cat
         AND  mail-user.file-exp NE ""
         NO-LOCK:      
         CREATE b-mail-user.

        BUFFER-COPY  mail-user TO b-mail-user.
        ASSIGN
        b-mail-user.acct          = "" 
        b-mail-user.mail-user-num = NEXT-VALUE(mail-user-num)
        b-mail-user.cust-cat      = acct.cust-cat
        b-mail-user.cust-id       = acct.cust-id
        b-mail-user.acct          = mAcct. 
        FOR EACH catalog WHERE catalog.mail-user-num EQ mail-user.mail-user-num NO-LOCK:
           CREATE b-catalog.
           BUFFER-COPY catalog TO b-catalog.
           b-catalog.mail-user-num = b-mail-user.mail-user-num.
        END.
        mOk = mOk + 1.
        RUN re-vyp14.p(mAcct).
        PUT UNFORMATTED SUBSTITUTE("Для счета &1 создано правило обмена &2 в филиале &3",
           mAcct, b-mail-user.mail-user-num,b-mail-user.filial-id) SKIP.
        LEAVE.
      END.
      END.
      END.
   END.
END.

PUT UNFORMATTED SUBSTITUTE("Успешно обработано &1 счетов ",mOk) SKIP.

{preview.i &filename = "'mail-user-acct-load.log'" }
