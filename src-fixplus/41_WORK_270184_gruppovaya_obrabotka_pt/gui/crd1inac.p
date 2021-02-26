{globals.i}

/* +++ crd1inac.p was humbly modified by (c)blodd converter v.1.09 on 1/25/2017 1:02pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: crd1inac.p
      Comment: Групповая транзакция для обработки платежных требований.
   Parameters:
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}
{intrface.get flt}
{topkind.def}
{tmprecid.def}
{tmpobj.def}
{intrface.get tmess}

define input param iOpDate like op.op-date no-undo.
define input param iOpRid  as recid        no-undo.

DEFINE VARIABLE mRes       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mErrMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mPlanDate  AS DATE        NO-UNDO.
DEFINE VARIABLE mDocNum    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCancel    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE h_kauproc  AS HANDLE      NO-UNDO.
DEFINE VARIABLE mKart1     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mOpl       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mGroup     AS CHARACTER   NO-UNDO INIT "*".

DEFINE VARIABLE proc-name  AS CHARACTER   NO-UNDO.

DEFINE BUFFER   bloan      FOR loan.

DEFINE TEMP-TABLE ttReport
   FIELD doc-num  LIKE op.doc-num
   FIELD msg      AS CHARACTER
.

SUBSCRIBE TO "CRD1-IN" ANYWHERE.

RUN kauproc.p PERSISTENT SET h_kauproc.

MAIN:
DO:
   IF (shFilial EQ "0500")
   THEN DO:
       RUN g-prompt.p ("char", "Группы ", "x(100)", "*", "Введите группы счетов (F1 - выбор)", 50, ",", "F1=pb_getgroup.p",?,?,OUTPUT mGroup).
       IF (mGroup EQ ?)
       THEN DO:
          mCancel = YES.
          LEAVE MAIN.
       END.
   END.

   {empty tmprecid}
   RUN SelectFltObject IN h_flt("op",
                   "op-date1"      + CHR(1) + "op-date2"       + CHR(1) + "RidRest" + CHR(1) + "doc-type" + CHR(1) + "op-status"    + CHR(1) + "GroupList",
                   STRING(iOpDate) + CHR(1) + STRING (iOpDate) + CHR(1) + "YES"     + CHR(1) + "02"       + CHR(1) + "СКАН,Ф,ФБК,В" + CHR(1) + mGroup,
                   "op-date1"      + CHR(1) + "op-date2").

   FOR EACH tmprecid,
      FIRST op WHERE RECID(op) EQ tmprecid.id
         NO-LOCK:

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "ChgSts") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "Ann") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "ChgDt") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      CREATE TmpObj.
      TmpObj.Rid = RECID(op).
   END.

   {empty tmprecid}

   RUN browseld.p ("op",
                   "UseTmpObjInQuery"  + CHR(1) + "RidRest" + CHR(1) + "FieldSort" /* + CHR(1) + "GroupList" */,
                   STRING(mTmpObjHand) + CHR(1) + "YES"     + CHR(1) + "acct-db"   /* + CHR(1) + mGroup */,
                   "",
                   4).

   IF {&KEY_FUNCTION}({&LAST_KEY}) EQ "END-ERROR" THEN
      LEAVE MAIN.

   RUN GetPlanDate IN h_kauproc (INPUT-OUTPUT mPlanDate).
   IF mPlanDate EQ ? THEN
   DO:
      mCancel = YES.
      LEAVE MAIN.
   END.

   /* Бежим по выбранным документам */
   FOR EACH tmprecid,
      FIRST op WHERE RECID(op) EQ tmprecid.id
         NO-LOCK:

      mDocNum = "".
      CREATE ttReport.

      {empty tOpKindParams} /* очистить таблицу параметров */
      ASSIGN
         mRes = TDAddParam ("CurOp", STRING(op.op))     AND
                TDAddParam ("PD",    STRING(mPlanDate))
      NO-ERROR.

      IF NOT mRes THEN
      DO:
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg     = "Ошибка передачи параметров. Документ пропущен."
         .
         NEXT.
      END.

      RELEASE bloan NO-ERROR.
      mKart1 = NO.
      mOpl   = NO.

      /* Ищем проводку */
      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.

      /* Ищем доп.соглашения  */
      FOR FIRST loan-acct
         WHERE loan-acct.acct     EQ op-entry.acct-db
           AND loan-acct.contract EQ "Расчет"
         NO-LOCK:

         FIND FIRST bloan
            WHERE bloan.contract         EQ 'КассСогАкц'
              AND bloan.parent-contract  EQ loan-acct.contract
              AND bloan.parent-cont-code EQ loan-acct.cont-code
              AND bloan.open-date        LE gend-date
              AND (bloan.end-date        GE gend-date
                OR bloan.end-date        EQ ?)
              AND bloan.close-date       EQ ?
            NO-LOCK NO-ERROR.
      END.

      /* Не нашли доп.соглашения */
      IF NOT AVAIL bloan THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", "К счету №" + DelFilFromAcct(op-entry.acct-db) + " нет соглашений о заранее данном акцепте.").
         RUN Fill-SysMes IN h_tmess ("","","4", "Поставить документ №" + op.doc-num + " на Картотеку 1?").
         IF pick-value EQ "YES"
            THEN mKart1 = YES.
      END.
      /* Нашли доп.соглашения */
      ELSE DO:
         RUN f-ac_kas2.p(op.op).

         proc-name = GET-CLASS-METHOD(op.class-code, "Look").
         IF proc-name EQ ? THEN  proc-name = "op#v1".
         RUN VALUE(proc-name + ".p") (gend-date, op.user-id, op.op, 3).

         RUN Fill-SysMes IN h_tmess ("","","3","Выберите действие|Оплатить,Поставить на К1,Отменить").

         IF pick-value EQ "1"
            THEN mOpl = YES.
            ELSE IF pick-value EQ "2"
               THEN mKart1 = YES.
               ELSE mCancel = YES.
      END.

      /* Если выбрали Оплатить */
      IF mOpl EQ YES THEN
      DO:
         RUN ex-trans.p ("_inCrd2ac", iOpDate, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

         IF NOT mOK THEN
         DO:
            ASSIGN
               ttReport.doc-num = op.doc-num
               ttReport.msg  = "Ошибка транзакции _inCrd2ac (см. " +
                               GetXAttrValueEx ("op-kind", "_inCrd1", "СС_ФайлПрткл", "") +
                               "). Документ пропущен."
            .
            NEXT.
         END.
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg  = "Документ оплачен."
         .
      END.

      /* Если выбрали Поставить документ на Картотеку */
      IF mKart1 EQ YES THEN
      DO:
         RUN ex-trans.p ("_inCrd1ac", iOpDate, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

         IF NOT mOK THEN
         DO:
            ASSIGN
               ttReport.doc-num = op.doc-num
               ttReport.msg  = "Ошибка транзакции _inCrd1ac (см. " +
                               GetXAttrValueEx ("op-kind", "_inCrd1", "СС_ФайлПрткл", "") +
                               "). Документ пропущен."
            .
            NEXT.
         END.
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg  = "Документ поставлен на картотеку. Документ постановки " + mDocNum + "."
         .
      END.
   END.
END.

{setdest.i}
PUT UNFORMATTED "Групповая обработка платежных требований" SKIP (1).
IF mCancel THEN
   PUT UNFORMATTED "------ Прервана по желанию пользователя." SKIP (1).
FOR EACH ttReport:
   PUT UNFORMATTED STRING (ttReport.doc-num, "X(10)") ttReport.msg SKIP.
END.
{preview.i}

{empty tmprecid}
{intrface.del}          /* Выгрузка инструментария. */

PROCEDURE CRD1-IN:
DEFINE INPUT  PARAMETER iDocNum AS CHARACTER   NO-UNDO.
      mDocNum = iDocNum.
END PROCEDURE.

/* --- crd1inac.p was humbly modified by (c)blodd converter v.1.09 on 1/25/2017 1:02pm --- */
