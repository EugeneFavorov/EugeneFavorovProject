/*
  Глухов
*/

{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mStr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPriznak AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE mDr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI       AS INT64     NO-UNDO.
DEFINE VARIABLE fname1 AS CHARACTER  INIT "./mark_loan.csv"  NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE STREAM vvs.


{getfile.i &filename="'/data/home/bq41d/samba/192.168.137.217/expo/123.csv'" &mode=must-exist }

mPriznak = "Экспо1".

DEFINE FRAME iParam 
   mPriznak   LABEL "Признак "
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ Установка признака продажи ]".

PAUSE 0.
UPDATE mPriznak
   WITH FRAME iParam. 
IF LAST-EVENT:FUNCTION  EQ "END-ERROR" THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.



INPUT FROM VALUE(fname) CONVERT SOURCE "1251".

OUTPUT STREAM vvs TO VALUE (fname1)
   UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

REPEAT:
   IMPORT UNFORMATTED mStr.
   FIND FIRST loan
      WHERE loan.filial-id EQ shFilial
      AND loan.contract EQ "Кредит"
      AND loan.cont-code EQ AddFilToLoan(mStr, shFilial)
      NO-LOCK NO-ERROR.
   IF AVAILABLE(loan) THEN
   DO:
      mDr = GetXattrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         "priznak",
         "").
      IF  mDr NE "" THEN
      DO:
         PUT STREAM vvs UNFORMATTED
            ENTRY(1, loan.cont-code, "@") ";" 
            "Ошибка! Уже есть признак " mDr SKIP.
      END.
      ELSE
      DO:
         IF UpdateSigns(loan.class-code,
            loan.contract + "," + loan.cont-code,
            "priznak",
            mPriznak,
            ?)
         THEN
         DO:
            mI = mI  + 1.
            PUT STREAM vvs UNFORMATTED
               ENTRY(1, loan.cont-code, "@")   ";" 
               SKIP.

         END.
         ELSE
            PUT STREAM vvs UNFORMATTED
               ENTRY(1, loan.cont-code, "@")   ";" 
               "Ошибка!  Признак не удалось установить"
               SKIP.
      END.    
   END.
   ELSE
      PUT STREAM vvs UNFORMATTED
         mStr ";"
         "Ошибка!  Договор не найден"
         SKIP.
            
END.    
INPUT CLOSE.
IF mI GT 0 THEN
   PUT STREAM vvs UNFORMATTED "Признаков успешно установлено " mI  SKIP.
RUN sndbispc ("file=" + fname1 + ";class=bq").
{intrface.del}
