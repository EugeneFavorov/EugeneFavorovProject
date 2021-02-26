{globals.i}

DEFINE VARIABLE mString AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErr AS INT64 NO-UNDO.
DEFINE VARIABLE mFname AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLog AS CHARACTER NO-UNDO.


{getfile.i &filename='*.*' &mode=must-exist }

mFName = SEARCH(pick-value).


mLog = "mail-user-acct-del.log".

{setdest.i &filename = mlog }


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
   FOR EACH acct WHERE
      acct.number EQ TRIM(mString)
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

      FOR EACH mail-user WHERE      
         mail-user.op-kind-exp EQ "e-cl940"                                              
          AND mail-user.filial-id EQ mfil
         AND mail-user.module EQ 'mail-cli'                                       
         EXCLUSIVE-LOCK:      
         IF INDEX(mail-user.acct,mAcct) > 0 THEN 
         DO:
            mAcctList = TRIM(REPLACE(REPLACE(mail-user.acct,mAcct,""),",,",","),","). 
            PUT UNFORMATTED SUBSTITUTE("Счет &1 Правило &2", mAcct, mail-user.mail-user-num) SKIP.
            PUT UNFORMATTED SUBSTITUTE("Было &1", mail-user.acct) SKIP.
            PUT UNFORMATTED SUBSTITUTE("Стало &1", mAcctList) SKIP.
            PUT UNFORMATTED " " SKIP.
            mail-user.acct = mAcctList.
         END. 
      END.
   END.
END.
message 1111 view-as alert-box.
{preview.i &filename = mlog}
