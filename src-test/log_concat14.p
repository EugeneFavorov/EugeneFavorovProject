/*
  Глухов
*/
{globals.i}

DEFINE VARIABLE mFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mreadyFName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMask         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpStr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFirst        AS LOG       NO-UNDO.
DEFINE VARIABLE mExclNach     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mExclAll      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLineN        AS INT64     NO-UNDO.

mExclNach = "
Ведомость начи,
Плановая дата,
НОМЕР ВКЛАДА,
--------------,
ОШИБКИ ПРИ НАЧ".
mExclAll = "
Рублевые догов
,Исполнитель:,
Контролер:,
ВСЕГО ПО ВАЛЮТ".





DEFINE STREAM rmdir.
DEFINE STREAM out1.

ASSIGN
mDir  = "./"
mMask = "*.log*".

INPUT STREAM rmdir FROM OS-DIR(mDir) NO-ATTR-LIST.

REPEAT:
   IMPORT STREAM rmdir mFileName.

   IF CAN-DO(mMask, mFileName) AND ENTRY(2,mFileName,".") NE "log" AND NUM-ENTRIES(mFileName,"_") EQ 4 THEN
   DO:
      mreadyFName = ENTRY(1,mFileName,".") + ".log".
      mFirst = SEARCH(mreadyFName) EQ ?.
      mLineN = 0.
      OUTPUT STREAM out1 TO VALUE(mreadyFName) APPEND.
      INPUT FROM VALUE(mDir + mFileName).
      REPEAT:
         IMPORT UNFORMATTED mTmpStr. 
         mLineN = mLineN + 1.
         IF TRIM(mTmpStr) NE "" 
         THEN
         DO:
            IF mFirst THEN
            DO:
               run dbgprint.p("",SUBSTR(TRIM(mTmpStr),1,14)).
               IF LOOKUP(TRIM(SUBSTR(TRIM(mTmpStr),1,14)),mExclAll) EQ 0 THEN
                  PUT STREAM out1 UNFORMATTED mTmpStr SKIP.
            END.
            ELSE 
            DO:
               IF INDEX(mreadyFName,"template") GT 0 THEN
               DO:
                  IF mLineN GT 3  THEN 
                     PUT STREAM out1 UNFORMATTED mTmpStr SKIP.
               END. 
               ELSE 
               DO: 
                  IF LOOKUP(TRIM(SUBSTR(TRIM(mTmpStr),1,14)),mExclNach + "," + mExclAll) EQ 0 THEN
                     PUT STREAM out1 UNFORMATTED mTmpStr SKIP.
               END.
                  
            END.
            mTmpStr = "".
         END.
      END.
      INPUT CLOSE.
      OS-DELETE VALUE(mDir + mFileName).
      output stream out1 CLOSE.
   END.

END.

message "Логи собраны!" view-as alert-box.


