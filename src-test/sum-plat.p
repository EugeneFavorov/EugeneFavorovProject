
define input param ipCountTypeChar as char no-undo. /* Алгоритм расчета ведомости */

{client.i}
{tmprecid.def}  /* Исходные договоры */
{param-dog.p}

define var vCountInt    as INT64 INIT 0 no-undo. /* Счетчик */
DEFINE VARIABLE vCustName  AS CHAR NO-UNDO.   /* Наименование клиента */
DEFINE VARIABLE out_Result  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vDbOpDec        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vCrOpDec        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mSum-prosr AS DECIMAL label "" init 0  NO-UNDO.
DEFINE VARIABLE mSum-all AS DECIMAL label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mSum-annu AS DECIMAL label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mdate AS DATE  NO-UNDO.
DEFINE VARIABLE mRs-acct  AS CHAR NO-UNDO.   /* Расчетный счет */
DEFINE VARIABLE e-date AS DATE LABEL "" NO-UNDO.
DEFINE VARIABLE mRs-ost AS DECIMAL label "" INIT 0  NO-UNDO.
DEF BUFFER bterm-obl for term-obl.
DEFINE VARIABLE mPlatType AS CHAR INIT "аннуитет" NO-UNDO.   /* Расчетный счет */

DEFINE BUFFER bLA    FOR loan-acct.
DEFINE BUFFER term-obl    FOR term-obl.

/* Временная таблица для отчета */
DEFINE TEMP-TABLE ttReport NO-UNDO
   FIELD NameKlient  AS CHAR /* Наименование клиента */
   FIELD cont-code   AS CHAR /* Номер договора */
   FIELD contract    AS CHAR /* Назначение договора */
   FIELD since       AS DATE LABEL "Срок оплаты" /* Дата пересчета договора */
   FIELD param_id    AS CHAR /* Идентификатор параметра */
   FIELD param_value AS DEC FORMAT "->>>>>>>>9.99" LABEL "Сумма" /* Значение параметра */
   FIELD name-par    AS CHAR LABEL "Наименование параметра" /* наименование параметра (параметров). Если параметров много, то выводим наименования параметров через "," */
   .

FOR EACH tmprecid NO-LOCK:  /* 1 */
   vCountInt = vCountInt + 1.
END. /* 1 */

IF vCountInt EQ 1 THEN
DO:  /* 2 */

   FOR EACH tmprecid,
      FIRST loan WHERE
      RECID(loan) EQ tmprecid.id
      NO-LOCK:      /* 3 */

      /* Бегущая строка - индикатор работы процесса */
      /*       {move-bar.i
                 vLnCountInt
                 vLnTotalInt
             }
      */
      RUN RE_CLIENT (loan.cust-cat, loan.cust-id, INPUT-OUTPUT vCustName).

      DO vCountInt = 1 TO NUM-ENTRIES (ipCountTypeChar,"+"): /* 4 */
            
         /* Получение значения параметра */
         RUN PRM(
            loan.Contract,            /* Назначение договора */
            loan.Cont-Code,            /* Номер договора */
            INTEGER(ENTRY(vCountInt,ipCountTypeChar,"+")),        /* Код параметра  */
            loan.since,                /* Значение параметра на дату состояния договора */
            TRUE,                   /* считать % */
            OUTPUT out_result).    /* Значение параметра без loan.interest[i] */

         CREATE ttReport.
         ASSIGN
            ttReport.contract   = loan.contract
            ttReport.cont-code  = loan.cont-code
            ttReport.since      = loan.since 
            ttReport.NameKlient = vCustName
            ttReport.param_id  = ENTRY(vCountInt,ipCountTypeChar,"+")
            ttReport.param_value  = out_result
            .
         IF vCountInt EQ 1 THEN
            mdate      = loan.since.

         /*Получим наименование параметра по справочнику*/
         FIND FIRST loan-par WHERE loan-par.amt-id EQ INTEGER(ttReport.param_id) NO-LOCK NO-ERROR.
   
         IF AVAIL loan-par THEN
            ttReport.name-par   = ttReport.param_id + " - " + loan-par.NAME. 

      END. /* 4 */

   END. /* 3 */
   FIND FIRST tmprecid.
   FIND FIRST loan WHERE
      RECID(loan) EQ tmprecid.id
      NO-LOCK NO-ERROR.
   CREATE ttReport.
   ASSIGN
      ttReport.contract   = loan.contract
      ttReport.cont-code  = loan.cont-code
      ttReport.since      = loan.since 
      ttReport.NameKlient = vCustName
      ttReport.param_id  = "555"
      ttReport.param_value  = 0.00
      ttReport.name-par   = " Итого просроченная задолженность - " 
      .

   /* Поиск первой незакрытой плановой сущности по договору */
   mSum-annu = 0.

     FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract 
                             AND loan-cond.cont-code EQ loan.cont-code 
                             AND loan-cond.since     LE loan.since
      NO-LOCK NO-ERROR.
      
      IF AVAIL loan-cond AND GetXAttrValueEx("loan-cond",
                            loan-cond.contract  + "," + 
                            loan-cond.cont-code + "," + 
                            STRING(loan-cond.since),
                            "СхемаПлат","?") NE "Аннуитетная" THEN 
DO:
    FIND FIRST term-obl WHERE
        term-obl.contract  EQ loan.contract AND
        term-obl.cont-code EQ loan.cont-code AND
        term-obl.idnt      EQ 3 AND
        term-obl.dsc-beg-date  GE loan.since AND
        term-obl.sop-date  EQ ?
    NO-LOCK NO-ERROR.

    FIND FIRST bterm-obl WHERE
        bterm-obl.contract  EQ loan.contract AND
        bterm-obl.cont-code EQ loan.cont-code AND
        bterm-obl.idnt      EQ 1 AND
        bterm-obl.dsc-beg-date  GE loan.since AND
        bterm-obl.sop-date  EQ ?
    NO-LOCK NO-ERROR.
mPlatType = "дифф.".
END.
ELSE
DO:
   RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract,
      loan.cont-code,
      3,
      loan.since,
      BUFFER term-obl).
   RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract,
      loan.cont-code,
      1,
      loan.since,
      BUFFER bterm-obl).

END.

   IF AVAIL term-obl AND
      term-obl.dsc-beg-date <= 
        (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
      THEN ASSIGN
         e-date   = term-obl.dsc-beg-date
         mSum-annu = mSum-annu + term-obl.AMT-RUB.

   IF AVAIL bterm-obl AND
      bterm-obl.dsc-beg-date <= 
        (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
      THEN ASSIGN
         e-date   = bterm-obl.dsc-beg-date
         mSum-annu = mSum-annu + bterm-obl.AMT-RUB.

   CREATE ttReport.
   ASSIGN
      ttReport.contract    = loan.contract
      ttReport.cont-code   = loan.cont-code
      ttReport.since       = e-date 
      ttReport.NameKlient  = vCustName
      ttReport.param_id    = "888"
      ttReport.param_value = mSum-annu
          ttReport.name-par    = "  Очередной платеж по кредиту (" + mPlatType  + ")  "
      .

   {empty tmprecid}

   FOR EACH acct NO-LOCK
      WHERE acct.cust-cat EQ "Ч"
      AND acct.cust-id EQ loan.cust-id
      AND acct.bal-acct EQ 47423:
      RUN acct-pos IN h_base (acct.acct,
         acct.currency,
         mdate,
         mdate,
         ?).
      CREATE ttReport.
      ASSIGN
         ttReport.contract     = loan.contract
         ttReport.cont-code    = loan.cont-code
         ttReport.since        = loan.since 
         ttReport.NameKlient   = vCustName
         ttReport.param_id     = "777"
         ttReport.param_value  = ABSOLUTE(sh-bal)
         /*ttReport.name-par   = " Комиссия со счета " + ENTRY(1, acct.acct, "@") + " - "*/
         /*1234567890123456789012345678901234567890*/
         ttReport.name-par     = " Платеж процентов за первый процентный период"
         .
   /*    mSum-com2 = ABSOLUTE(sh-bal). */

   END. /* 10 */

   FOR EACH ttReport SHARE-LOCK by INTEGER(ttReport.param_id): /* 5 */

      IF INTEGER(ttReport.param_id) LT 555 THEN
         mSum-prosr = mSum-prosr + ttReport.param_value.
      IF INTEGER(ttReport.param_id) EQ 555 THEN
      DO:
         ttReport.param_value = mSum-prosr.
      END.
      ELSE
         mSum-all = mSum-all + ttReport.param_value.
   END.  /* 5 */
END.  /* 2 */

RUN SetSysConf IN h_base ("SumAllPar",STRING(mSum-all)).

RETURN.

