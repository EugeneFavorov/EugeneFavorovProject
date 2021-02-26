/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: PRC-ACTS.P
      Comment: Создание счетов процентов для выбранных лицевых счетов.
   Parameters:
         Uses:
      Used by:
      Created: 24.11.2006 16:03 Vasov  (0066344) 
     Modified:
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}     

{topkind.def}
{tmprecid.def}

DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mErrMsg  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mPrcAcct AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCont    AS LOGICAL     NO-UNDO.

DEFINE BUFFER bAcct FOR acct.

DEFINE TEMP-TABLE ttReport
   FIELD acct     LIKE acct.acct
   FIELD msg      AS CHARACTER
.

SUBSCRIBE TO "NEW-ACCT-CREATED" ANYWHERE.

RUN browseld.p ("acct",
                "acct-cat" + CHR(1) + "cust-cat" + CHR(1) + "contract" + CHR(1) + "RidRest",
                "b"        + CHR(1) + "Ч"        + CHR(1) + "Текущ"    + CHR(1) + "YES",
                "acct-cat" + CHR(1) + "cust-cat" + CHR(1) + "contract",
                4).

mCont = KEYFUNCTION (LASTKEY) NE "END-ERROR".

IF mCont THEN
DO:

   RUN SetSysConf IN h_base ("ДатаСообщЛС","YES").
   FOR EACH tmprecid,
      FIRST acct WHERE RECID (acct)  EQ tmprecid.id
         NO-LOCK:

      CREATE ttReport.

      mPrcAcct = GetXAttrValueEx ("acct", 
                                  acct.acct + "," + acct.currency,
                                  "Сч_проц",
                                  "").
      IF mPrcAcct NE "" THEN
      DO:
         {find-act.i
            &bact = bAcct
            &acct = ENTRY(1,mPrcAcct)
            &curr = ENTRY(2,mPrcAcct)
         }
         IF AVAIL bAcct THEN
         DO:
            ASSIGN 
               ttReport.acct = acct.acct
               ttReport.msg  = " - уже есть счет процентов " + ENTRY(1,mPrcAcct)
            .
            NEXT.
         END.
         ELSE
            mPrcAcct = "".
      END.

gbeg-date = acct.open-date.
gend-date = TODAY.

/*{getdates.i &TitleLabel = "[ДОП. СОГЛ. 'СЧЕТ ПЛЮС' ]" &BegLabel = "Дата начала:" &EndLabel = "Дата окончания:"}*/
      {empty tOpKindParams} /* очистить таблицу параметров */

      ASSIGN
         mRes = TDAddParam ("in-acct", acct.acct)                    AND
                TDAddParam ("in-curr", acct.currency)                AND
                TDAddParam ("in-cust-cat",  acct.cust-cat)           AND
                TDAddParam ("in-cust-id",  STRING (acct.cust-id))    AND
                TDAddParam ("in-branch-id", acct.branch-id)          AND
                TDAddParam ("in-open-date", STRING (beg-date)) AND
                TDAddParam ("in-rate-type", acct.rate-type)
      NO-ERROR.
      
      IF NOT mRes THEN 
      DO:
         ASSIGN
            ttReport.acct = acct.acct
            ttReport.msg  = " - ошибка передачи параметров. Счет процентов не создан"
         .
         NEXT.
      END.

      RUN ex-trans.p ("Act47411", TODAY, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

      IF mOK THEN
      DO:
         UpdateSigns ("acct", 
                      acct.acct + "," + acct.currency, 
                      "Сч_проц", 
                      mPrcAcct + "," + acct.currency, 
                      NO).
         UpdateSigns ("acct", 
                      acct.acct + "," + acct.currency, 
                      "Миност", 
                       "0.01" , 
                      YES).
         UpdateSigns ("acct", 
                      acct.acct + "," + acct.currency, 
                      "Сч_Дата_Н", 
                       STRING(beg-date, "99.99.9999") , 
                      NO).
         UpdateSigns ("acct", 
                      acct.acct + "," + acct.currency, 
                      "Сч_Дата_К", 
                       STRING(end-date, "99.99.9999") , 
                      NO).

         ASSIGN
            ttReport.acct = acct.acct
            ttReport.msg  = " - создан счет процентов " + mPrcAcct
         .
      END.
      ELSE
      DO:
         ASSIGN
            ttReport.acct = acct.acct
            ttReport.msg  = " - " + mErrMsg + ". Счет процентов не создан"
         .
      END.
   END. /* for */
   RUN DeleteOldDataProtocol IN h_base ("ДатаСообщЛС").
END.

IF mCont THEN
DO:
   {setdest.i}
   PUT UNFORMATTED "СОЗДАНИЕ СЧЕТОВ ПРОЦЕНТОВ" SKIP (1).
   FOR EACH ttReport:
      PUT UNFORMATTED ttReport.acct ttReport.msg SKIP.
   END.
   {preview.i}
END.

{empty tmprecid}
{intrface.del}          /* Выгрузка инструментария. */ 

PROCEDURE NEW-ACCT-CREATED:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER   NO-UNDO.
   mPrcAcct = iAcct.
END PROCEDURE.





