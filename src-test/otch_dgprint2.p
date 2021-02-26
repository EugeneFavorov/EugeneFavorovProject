/* pda
   отчет по ДГ
   ccc
*/

{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get loan}
{prn-doc.def &with_proc=YES}
{sh-defs.i}
{ksh-defs.i NEW}
{param-dog.p}

/* {filleps.def} */
{norm.i NEW}

{svarloan.def NEW}

DEFINE VARIABLE datedp    AS DATE      NO-UNDO FORMAT "99/99/9999".
DEFINE VARIABLE sumdp     AS DECIMAL   NO-UNDO FORMAT ">>>>>>>>>>>>>>>>>9.99".

DEFINE VARIABLE vTplName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vShPlat   AS CHARACTER NO-UNDO. /*схема платежа*/
DEFINE VARIABLE c1        AS CHARACTER NO-UNDO.
DEFINE VARIABLE c2        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateCHDG AS DATE      NO-UNDO.
DEFINE VARIABLE mDateBP   AS DATE      NO-UNDO.
DEFINE VARIABLE mSumBP    AS DECIMAL   NO-UNDO.  
DEFINE VARIABLE mSumBP1   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mSumBP2   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mSumPP    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mSumCHDG  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mDatePDG  AS DATE      NO-UNDO.
DEFINE VARIABLE mRecName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecBIK   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBank     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTypeDG   AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX INNER-LINES 2 PFCOLOR 6 DCOLOR 3.

DEFINE VARIABLE mCustID   AS INT64     NO-UNDO.
DEFINE VARIABLE mName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLoanList AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX INNER-LINES 5 PFCOLOR 6 DCOLOR 3.
DEFINE VARIABLE mLoanStr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLoan     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCnt      AS INT64     NO-UNDO.
DEFINE VARIABLE mPlanDate AS DATE      NO-UNDO.
DEFINE VARIABLE mYesNo    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mPercDate AS DATE      NO-UNDO.
DEFINE VARIABLE mCommRate AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mPrmDec   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mDbSumDec AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCrSumDec AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mNumDay   AS INT64     NO-UNDO.

DEFINE VARIABLE mParList  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mIntK     AS INT64     NO-UNDO.
DEFINE VARIABLE mTmpSum   AS DECIMAL   NO-UNDO.   
DEFINE VARIABLE mTmpSum2  AS DECIMAL   NO-UNDO.   
DEFINE VARIABLE mTmpSum1  AS DECIMAL   NO-UNDO EXTENT 2.
DEFINE VARIABLE mSumPerc  AS DECIMAL   NO-UNDO.  
DEFINE VARIABLE mDateZ    AS DATE      NO-UNDO. 

/* Временная таблица для отчета */
DEFINE TEMP-TABLE ttReportpdg NO-UNDO
   FIELD since       AS DATE LABEL "Срок оплаты" /* Дата пересчета договора */
   FIELD param_id    AS CHARACTER /* Идентификатор параметра */
   FIELD param_value AS DEC  FORMAT "->>>>>>>>9.99" LABEL "Сумма" /* Значение параметра */
   FIELD name-par    AS CHARACTER LABEL "Наименование параметра" /* наименование параметра (параметров). Если параметров много, то выводим наименования параметров через "," */
.

sumdp = 0.

/* Функция, прибавляет заданное количество рабочих дней к указанной дате */
FUNCTION AddWorkDay RETURN DATE (INPUT vDateIn AS DATE,INPUT amtWorkDay AS INT64).
   DEF VAR vDate AS DATE  NO-UNDO.
   DEF VAR i     AS INT64 NO-UNDO.
   
   vDate = vDateIn.
   DO i = 1 TO amtWorkDay:
      vDate = vDate + 1. 
      IF HOLIDAY(vDate) OR CAN-DO("1,7",STRING(WEEKDAY(vDate))) THEN i = i - 1.
      /* MESSAGE i skip STRING(vDate) skip HOLIDAY(vDate) VIEW-AS ALERT-BOX. */
   END.
   RETURN vDate.
END FUNCTION.

/*Функция возвращает ближайшую плановую дату*/
FUNCTION getPlanDate RETURN DATE (INPUT vDatePDG AS DATE).
   DEF VAR vPlanDate AS DATE INIT ? NO-UNDO.
   
   end-date = vDatePDG.
   FIND FIRST loan WHERE RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
      /* ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ */
      FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
               term-obl.contract  = loan.contract
           AND term-obl.cont-code = loan.cont-code
           AND term-obl.idnt = 3
           AND term-obl.end-date <> ?
           AND term-obl.end-date >= end-date NO-LOCK
      BY term-obl.end-date:
         vPlanDate = term-obl.end-date.
         LEAVE.
      END.
      /* ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ */
      FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
               term-obl.contract  = 'Кредит'
           AND term-obl.cont-code = loan.cont-code
           AND term-obl.idnt = 1
           AND term-obl.end-date <> ?
           AND term-obl.end-date >= end-date NO-LOCK
      BY term-obl.end-date:
         IF vPlanDate EQ ? THEN
            vPlanDate = term-obl.end-date.
         ELSE
            vPlanDate = MIN(term-obl.end-date,vPlanDate).
         LEAVE.
      END.
   END.
   RETURN vPlanDate.
END FUNCTION.

/*Функция возвращает количество дней со дня открытия КД*/
FUNCTION getAgeCr RETURN INT64.
   DEF VAR vCount AS INT64 NO-UNDO.
   
   FIND FIRST loan WHERE RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
   IF AVAIL loan THEN 
      vCount = TODAY - loan.open-date.
      
   RETURN vCount.
END FUNCTION.

FUNCTION getDateStr RETURN CHARACTER PRIVATE(INPUT iDate AS DATE):
   IF iDate = ? THEN
      RETURN ?.
   RETURN (STRING(DAY(iDate),"99") + " " + ENTRY(MONTH(iDate),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") + " " + STRING(YEAR(iDate)) + " г.").
END FUNCTION.

FUNCTION calcppdg RETURNS DECIMAL:
   
DEF VAR ipCountTypeChar AS CHARACTER NO-UNDO.
ipCountTypeChar = "0+2+7+8+233+9+10+12+18+26+82+210+16+13+14+15+48+248+29+229+519+509+530+373+777+4".

DEFINE VARIABLE vCountInt   as INT64    INIT 0 no-undo. /* Счетчик */
DEFINE VARIABLE vCustName   AS CHARACTER     NO-UNDO.   /* Наименование клиента */
DEFINE VARIABLE out_Result  AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vDbOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vCrOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE mSum-prosr  AS DECIMAL  label "" init 0  NO-UNDO.
DEFINE VARIABLE mSum-all    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mSum-annu   AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mdate       AS DATE     NO-UNDO.
DEFINE VARIABLE mRs-acct    AS CHARACTER     NO-UNDO.   /* Расчетный счет */
DEFINE VARIABLE mVkl-acct   AS CHARACTER     NO-UNDO.   /* Расчетный счет */
DEFINE VARIABLE e-date      AS DATE     LABEL "" NO-UNDO.
DEFINE VARIABLE mRs-ost     AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mVkl-ost    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE i           AS INTEGER  NO-UNDO.
DEFINE VARIABLE iPar        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE mSumFullComm AS DECIMAL INIT 0  NO-UNDO.
DEFINE VARIABLE mSumComm    AS DECIMAL  INIT 0  NO-UNDO.
DEFINE VARIABLE firstDate   AS DATE     NO-UNDO.
DEFINE VARIABLE not0        AS LOGICAL  NO-UNDO.

DEFINE BUFFER   term-obl    FOR term-obl.
DEFINE BUFFER   bterm-obl   FOR term-obl.
DEFINE BUFFER   bLA         FOR loan-acct.

not0 = TRUE.
{empty ttReportpdg}
/* Бегущая строка - индикатор работы процесса */

/*      {move-bar.i vLnCountInt vLnTotalInt}
*/
   mdate = loan.since.
   mSum-all = 0.
   mSumComm = 0.
   mSumFullComm = 0.
   mSum-prosr = 0.
   out_result = 0.
   mSum-annu = 0.
   DO vCountInt = 1 TO NUM-ENTRIES (ipCountTypeChar,"+"):

      /* Получение значения параметра */
      iPar = ENTRY(vCountInt, ipCountTypeChar, "+").
      RUN PRM(loan.Contract,          /* Назначение договора */
            loan.Cont-Code,         /* Номер договора */
            INTEGER(iPar),          /* Код параметра  */
            loan.since,             /* Значение параметра на дату пересчета договора */
            TRUE,                   /* считать % */
            OUTPUT out_result).     /* Значение параметра без loan.interest[i] */


      CREATE ttReportpdg.
      ASSIGN
         ttReportpdg.since       = loan.since
         ttReportpdg.param_id    = iPar
         ttReportpdg.param_value = out_result
         .

      /* Получим наименование параметра по справочнику */
      FIND FIRST loan-par
         WHERE loan-par.amt-id EQ INTEGER(iPar)
         NO-LOCK NO-ERROR.
      IF AVAIL loan-par
      THEN ttReportpdg.name-par = ttReportpdg.param_id + " - " + loan-par.NAME.

      /* Корректировка 4 параметра */
      IF (iPar EQ "4")
      THEN DO:
         DO i = 32 TO 35:
            RUN PRM(loan.Contract, loan.Cont-Code, i, loan.since, TRUE, OUTPUT out_result).
            ttReportpdg.param_value = ttReportpdg.param_value + out_result.
         END.
      END.

      /* Корректировка 29 параметра */
      IF (iPar EQ "29")
      THEN DO:
         FOR EACH loan-int OF loan
            WHERE (loan-int.mdate   EQ loan.since)
            NO-LOCK,
         FIRST chowhe
            WHERE (chowhe.id-d      EQ loan-int.id-d)
              AND (chowhe.id-k      EQ loan-int.id-k)
              AND (chowhe.id-op     EQ 83)
            NO-LOCK:

            ttReportpdg.param_value = ttReportpdg.param_value - loan-int.amt-rub.
         END.
      END.
      
      /* Корректировка 229 параметра */
      IF (iPar EQ "229")
      THEN DO:
         FOR EACH loan-int OF loan
            WHERE (loan-int.mdate   EQ loan.since)
            NO-LOCK,
         FIRST chowhe
            WHERE (chowhe.id-d      EQ loan-int.id-d)
              AND (chowhe.id-k      EQ loan-int.id-k)
              AND (chowhe.id-op     EQ 283)
            NO-LOCK:

            ttReportpdg.param_value = ttReportpdg.param_value - loan-int.amt-rub.
         END.
      END.
   END. /* DO vCountInt = 1 TO ... */

   mSumComm = 0.
   FIND FIRST term-obl
      WHERE term-obl.contract  EQ loan.contract
        AND term-obl.cont-code EQ loan.cont-code
        AND term-obl.idnt      EQ 10
        NO-LOCK NO-ERROR.
   IF AVAIL term-obl
   THEN DO:
      mSumFullComm = term-obl.amt-rub.

      RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 3, loan.open-date, BUFFER term-obl).
      RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 1, loan.open-date, BUFFER bterm-obl).

      IF AVAIL term-obl AND
         term-obl.dsc-beg-date <=
         (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
      THEN firstDate   = term-obl.dsc-beg-date.

      IF AVAIL bterm-obl AND
         bterm-obl.dsc-beg-date <=
            (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
      THEN firstDate   = bterm-obl.dsc-beg-date.

      IF firstDate > loan.since
      THEN DO:
         mSumComm = (mSumFullComm / (firstDate - loan.open-date)) * (loan.since - loan.open-date).
         FOR EACH loan-int OF loan
            WHERE loan-int.id-k = 377
              AND loan-int.mdate <= loan.since
              NO-LOCK:
            
            mSumComm = mSumComm - loan-int.amt-rub.
         END.
         not0 = FALSE.
      END.
   END.

   FIND FIRST loan-acct OF loan
      WHERE loan-acct.acct-type = 'КредБудКом'
      NO-LOCK NO-ERROR.
   IF AVAIL loan-acct
   THEN DO:
      RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, mdate, mdate, ?).
      IF not0 OR mSumComm > ABSOLUTE(sh-bal)
      THEN mSumComm = ABSOLUTE(sh-bal).

      CREATE ttReportpdg.
      ASSIGN
         ttReportpdg.since        = loan.since
         ttReportpdg.param_id     = "777"
         ttReportpdg.param_value  = mSumComm
         /* 1234567890123456789012345678901234567890 */
         ttReportpdg.name-par     = " Платеж процентов за первый процентный период"
         .
   END.

   RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"КредРасч",loan.since,BUFFER bLA).
   IF AVAILABLE bLA
   THEN DO:
      mRs-acct = "   Текущий счет " + ENTRY(1, bLA.acct, "@").
      RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
      mRs-ost = ABSOLUTE(sh-bal).
   END.
   ELSE mRs-acct = "   Расчетный счет НЕ ПРИВЯЗАН К ДОГОВОРУ".

   RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"КредРасч1",loan.since,BUFFER bLA).
   IF AVAILABLE bLA
   THEN DO:
      mVkl-acct = "Обязательства заемщика вклад " + ENTRY(1, bLA.acct, "@").
      RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
      mVkl-ost = ABSOLUTE(sh-bal).
   END.
   ELSE mVkl-acct = "   Вклад НЕ ПРИВЯЗАН К ДОГОВОРУ".

   FOR EACH ttReportpdg
      SHARE-LOCK
      BY INTEGER(ttReportpdg.param_id): /* 5 */

      IF INTEGER(ttReportpdg.param_id) LT 555
      THEN mSum-prosr = mSum-prosr + ttReportpdg.param_value.

      IF INTEGER(ttReportpdg.param_id) EQ 555
      THEN DO:
         ttReportpdg.param_value = mSum-prosr.
       /*   PUT UNFORMATTED FILL("-",71). */
      END.
      ELSE mSum-all = mSum-all + ttReportpdg.param_value.
     
   END.  /* 5 */

   RETURN mSum-all.
END.

/*=============================================форма для ДГ=============================================*/

PAUSE 0.
FORM
   mCustID     FORMAT ">>>>>>>9"        LABEL "Номер клиента"   HELP "F1 - выбор клиента "
   mName       FORMAT "x(55)"           LABEL "ФИО"             HELP "ФИО клиента "
   mLoanList   FORMAT "x(25)"           LABEL "Договор(ы)"      HELP "Выберите договор"
   mTypeDG     FORMAT "X(5)"            LABEL "Тип ДГ"          HELP "Тип досрочного погашения"
   mDateBP     FORMAT "99/99/9999"      LABEL "Дата  БП"        HELP "Дата ближайшего платежа"
   mSumBP      FORMAT ">>>>>>>>>>>9.99" LABEL "Сумма БП"        HELP "Сумма ближайшего платежа"
   mDatePDG    FORMAT "99/99/9999"      LABEL "Дата ДГ"         HELP "Введите дату ДГ (F1 - календарь)"
   mSumCHDG    FORMAT ">>>>>>>>>>>9.99" LABEL "Сумма ЧДГ"       HELP "Введите сумму ЧДГ"
   mRecName    FORMAT "X(50)"           LABEL "Получатель"      HELP "Получатель"
   mRecAcct    FORMAT "X(20)"           LABEL "Счет получателя" HELP "Счет получателя"
   mRecBIK     FORMAT "X(9)"            LABEL "БИК"             HELP "Введите БИК банка получателя"
   mCorAcct    FORMAT "X(20)"           LABEL "Кор. счет №"     HELP "Коррсчет банка получателя"
   mBank       FORMAT "X(50)"           LABEL "Банк получателя" HELP "Наименование банка получателя"
WITH FRAME frame-par OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
COLOR MESSAGE TITLE "[ ЗАДАЙТЕ РЕКВИЗИТЫ ]".

FIND FIRST tmprecid NO-LOCK NO-ERROR.
IF AVAIL(tmprecid) AND KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
DO:
   FIND FIRST person WHERE
      RECID(person) EQ tmprecid.id NO-LOCK NO-ERROR.
   IF AVAIL(person) THEN
   DO:
      ASSIGN
         mCustID            = person.person-id
         mName              = person.name-last + " " + person.first-names
         mName:SCREEN-VALUE IN FRAME frame-par = person.name-last + " " + person.first-names
         mName:SENSITIVE IN FRAME frame-par = NO.
      mLoanList:LIST-ITEMS = "".
      mCnt = 0.
      FOR EACH loan WHERE
            (loan.close-date EQ ? OR
             loan.close-date GE TODAY)
         AND loan.contract   EQ "КРЕДИТ"
         AND loan.cust-cat   EQ "Ч"
         AND loan.cust-id    EQ mCustID
         AND loan.filial-id  NE "0400"
         NO-LOCK:
         mCnt = mCnt + 1.
         IF mCnt EQ 1 
         THEN 
         ASSIGN
            mLoanList:LIST-ITEMS = TRIM(loan.cont-code)
            mLoanStr = TRIM(loan.cont-code).   
         ELSE
         ASSIGN
            mLoanList:LIST-ITEMS = mLoanList:LIST-ITEMS + "," + TRIM(loan.cont-code)
            mLoanStr = mLoanStr + "," + TRIM(loan.cont-code).
      END.
   END.
   ELSE
   DO:
      ASSIGN
         mCustID              = 0
         mName                = ""
         mName:SCREEN-VALUE IN FRAME frame-par = ""
         mName:SENSITIVE IN FRAME frame-par = NO
         mLoanList:LIST-ITEMS = ""
         mLoanStr             = "".
   END.
END.

ON 'F1':U OF mCustID IN FRAME frame-par
DO:
   RUN person.p (4).
   IF KEYFUNCTION (LASTKEY) NE "end-error" AND
      pick-value            NE ? THEN
   DO:
      mCustID = INT64 (pick-value).
      SELF:SCREEN-VALUE = STRING(mCustID).
      APPLY "LEAVE" TO mCustID.
      APPLY "ENTRY" TO mLoanList.
      RETURN NO-APPLY.
   END.
END.

ON "ENTRY":U OF mCustID
DO:
   ASSIGN
      mName:SENSITIVE    IN FRAME frame-par = NO
      mDateBP:SENSITIVE  IN FRAME frame-par = NO
      mSumBP:SENSITIVE   IN FRAME frame-par = NO
      mDatePDG:SENSITIVE IN FRAME frame-par = NO
      mSumCHDG:SENSITIVE IN FRAME frame-par = NO
      mRecName:SENSITIVE IN FRAME frame-par = NO
      mRecAcct:SENSITIVE IN FRAME frame-par = NO
      mRecBIK:SENSITIVE  IN FRAME frame-par = NO 
      mCorAcct:SENSITIVE IN FRAME frame-par = NO
      mBank:SENSITIVE    IN FRAME frame-par = NO.
END.

ON LEAVE OF mCustID
DO:
   FIND FIRST person WHERE
      person.person-id EQ INT64(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAIL(person) THEN
   DO:
      ASSIGN
         mCustID            = person.person-id
         mName              = person.name-last + " " + person.first-names
         mName:SCREEN-VALUE IN FRAME frame-par = person.name-last + " " + person.first-names
         mName:SENSITIVE IN FRAME frame-par = NO.
      mLoanList:LIST-ITEMS = "".
      mCnt = 0.
      FOR EACH loan WHERE
            (loan.close-date EQ ? OR
             loan.close-date GE TODAY)
         AND loan.contract   EQ "КРЕДИТ"
         AND loan.cust-cat   EQ "Ч"
         AND loan.cust-id    EQ mCustID
         AND loan.filial-id  NE "0400"
         NO-LOCK:
         mCnt = mCnt + 1.
         IF mCnt EQ 1 
         THEN
         DO:
            ASSIGN
               mLoanList:LIST-ITEMS = TRIM(loan.cont-code)
               mLoanStr = TRIM(loan.cont-code).
         END.   
         ELSE
         ASSIGN
            mLoanList:LIST-ITEMS = mLoanList:LIST-ITEMS + "," + TRIM(loan.cont-code)
            mLoanStr = mLoanStr + "," + TRIM(loan.cont-code).
      END.
      DISPLAY mCustID WITH FRAME frame-par.
      DISPLAY mLoanList WITH FRAME frame-par.
      APPLY "ENTRY" TO mLoanList.
      RETURN NO-APPLY.
   END.
   ELSE
   DO:
      ASSIGN
         mCustID              = 0
         mName                = ""
         mName:SCREEN-VALUE IN FRAME frame-par = ""
         mName:SENSITIVE IN FRAME frame-par = NO
         mLoanList:LIST-ITEMS = ""
         mLoanStr             = "".
      APPLY "ENTRY" TO mLoanList.
   END.
END.

ON 'ENTER':U OF mLoanList
DO:
   APPLY "TAB" TO SELF.
   RETURN NO-APPLY.
END.

ON LEAVE OF mLoanList
DO:
   mCnt = LOOKUP(mLoanList:SCREEN-VALUE,mLoanStr).
   mLoan = ENTRY(mCnt,mLoanStr,",").
   FIND FIRST loan WHERE TRUE
      AND loan.contract  EQ "Кредит"
      AND loan.cont-code EQ mLoan
   NO-LOCK NO-ERROR.
   IF AVAIL(loan) THEN 
   ASSIGN
      rid-p    = RECID(loan).
   APPLY "TAB" TO SELF.
   RETURN NO-APPLY.
END.

mTypeDG:LIST-ITEMS = "ЧДГ,ПДГ".

ON 'ENTER':U OF mTypeDG
DO:
   APPLY "TAB" TO SELF.
   RETURN NO-APPLY.
END.

ON LEAVE OF mTypeDG
DO:
   mCnt = LOOKUP(mTypeDG:SCREEN-VALUE,mTypeDG:LIST-ITEMS).
   CASE mCnt:
   WHEN 1 THEN
      DO:
         ASSIGN
            mDatePDG:SENSITIVE IN FRAME frame-par = NO
            mDatePDG:VISIBLE   IN FRAME frame-par = NO
            mSumCHDG:VISIBLE   IN FRAME frame-par = YES
            mSumCHDG:SENSITIVE IN FRAME frame-par = YES
            mRecName:SENSITIVE IN FRAME frame-par = NO
            mRecAcct:SENSITIVE IN FRAME frame-par = NO
            mRecBIK:SENSITIVE  IN FRAME frame-par = NO 
            mCorAcct:SENSITIVE IN FRAME frame-par = NO
            mBank:SENSITIVE    IN FRAME frame-par = NO.
         APPLY "ENTRY" TO mSumCHDG.
   
         vShPlat = "".
         mDateBP = DATE("01.01.1970").
         mSumBP  = 0.00.
         mSumBP1 = 0.00.
         mSumBP2 = 0.00.
         mSumPP  = 0.00.
         
         /*Дата БП (Ближайшего платежа)*/
         mDateBP = getPlanDate(TODAY).
         IF mDateBP NE ? THEN mDateBP:SCREEN-VALUE = STRING(mDateBP,"99.99.9999").
         
         /*Сумма БП (Ближайшего платежа)*/
         FIND FIRST loan WHERE RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
         FIND LAST loan-cond WHERE TRUE
            AND loan-cond.contract   EQ "Кредит" 
            AND loan-cond.cont-code  EQ loan.cont-code 
            AND loan-cond.class-code EQ "an-cond" 
         NO-LOCK NO-ERROR. 
         IF AVAIL(loan-cond) THEN
            vShPlat = GetXAttrValueEx("loan-cond",
                                      loan-cond.contract  + "," + 
                                      loan-cond.cont-code + "," + 
                                      STRING(loan-cond.since),
                                      "СхемаПлат","?").
      
         IF vShPlat EQ "Дифференцированная" 
            OR NOT AVAIL(loan-cond)
         THEN
         DO:
            /* ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ */
            FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
                  term-obl.contract = loan.contract
               AND term-obl.cont-code = loan.cont-code
               AND term-obl.idnt = 3
               AND term-obl.end-date <> ?
               AND term-obl.end-date >= mDateBP  NO-LOCK
               BY term-obl.end-date:
                  mSumBP1 = term-obl.amt-rub.
                  LEAVE.
            END.
            /* ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ */
            FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
                  term-obl.contract = 'Кредит'
               AND term-obl.cont-code = loan.cont-code
               AND term-obl.idnt = 1
               AND term-obl.end-date <> ?
               AND term-obl.end-date >= mDateBP NO-LOCK
               BY term-obl.end-date:
                 mSumBP2 = term-obl.amt-rub.
                 LEAVE.
            END.
         END.
         ELSE IF vShPlat EQ "Аннуитетная" THEN
         DO:
            /* ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ */
            FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
                  term-obl.contract = 'Кредит'
               AND term-obl.cont-code = loan.cont-code
               AND term-obl.idnt = 1
               AND term-obl.end-date <> ?
               AND term-obl.end-date >= mDateBP 
               NO-LOCK BY term-obl.end-date:
               mSumBP2 = term-obl.amt-rub.
               mPercDate = term-obl.end-date.
               LEAVE.
            END.
            /* ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ */
            FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
                  term-obl.contract = loan.contract
               AND term-obl.cont-code = loan.cont-code
               AND term-obl.idnt = 3
               AND term-obl.end-date <> ?
               AND term-obl.end-date EQ mPercDate
               NO-LOCK BY term-obl.end-date:
               mSumBP1 = term-obl.amt-rub.
               LEAVE.
            END.
         END.
         
         mSumBP = mSumBP1 + mSumBP2.
         mSumBP:SCREEN-VALUE = STRING(mSumBP,"->>>>>>>>>9.99").
         
         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
            "~nmSumBP1 = " + STRING(mSumBP1,"->>>>>>>>>9.99") + 
            "~nmSumBP2 = " + STRING(mSumBP2,"->>>>>>>>>9.99") + 
            "~nmSumBP  = " + STRING(mSumBP, "->>>>>>>>>9.99")).
      END.
   WHEN 2 THEN
      DO:
      ASSIGN
         mDatePDG:SENSITIVE IN FRAME frame-par = YES
         mDatePDG:VISIBLE   IN FRAME frame-par = YES
         mSumCHDG:VISIBLE   IN FRAME frame-par = NO
         mSumCHDG:SENSITIVE IN FRAME frame-par = NO
         mRecName:SENSITIVE IN FRAME frame-par = YES
         mRecAcct:SENSITIVE IN FRAME frame-par = YES
         mRecBIK:SENSITIVE  IN FRAME frame-par = YES 
         mCorAcct:SENSITIVE IN FRAME frame-par = YES
         mBank:SENSITIVE    IN FRAME frame-par = YES.
         APPLY "ENTRY" TO mDatePDG.
      END.
   END CASE.
   
   RETURN NO-APPLY.
END.

ON "F1" OF mDatePDG
DO:            
   /* вызвать календарь */
   RUN calend.p.
   IF pick-value <> ? THEN DO:
      mDatePDG:SCREEN-VALUE = pick-value.
   END.
END.

 ON LEAVE OF mDatePDG
DO:
   IF DATE(mDatePDG:SCREEN-VALUE) EQ ? THEN
   DO:
      MESSAGE "Дата должна быть заполнена" 
      VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END. 

   FIND FIRST loan WHERE RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
      FIND LAST loan-cond WHERE loan-cond.contract = loan.contract 
   	and loan-cond.cont-code = loan.cont-code
   	and loan-cond.class-code = 'cd-cond'	
   	NO-LOCK NO-ERROR.

   IF DATE(mDatePDG:SCREEN-VALUE) < TODAY
   THEN DO:
      MESSAGE "Дата не может быть меньше текущего рабочего дня, проставьте верную дату" 
      VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   ELSE IF AVAIL(loan-cond)
      THEN DO:
         MESSAGE "Досрочное погашение будет проведено не позднее следующего дня с даты приема заявления. - " + STRING(AddWorkday(TODAY,1),"99.99.9999") 
         VIEW-AS ALERT-BOX INFORMATION.
         mDatePDG:SCREEN-VALUE = STRING(AddWorkday(TODAY,1)).
      END.
   ELSE IF mTypeDG:SCREEN-VALUE EQ "ПДГ"
      AND getAgeCr() > 30
      AND AddWorkday(TODAY,3) > DATE(mDatePDG:SCREEN-VALUE)
      and not avail loan-cond
      THEN DO:
         MESSAGE "По условиям кредитования досрочное погашение разрешено не ранее третьего рабочего дня с даты приема заявления на ДГ, проставьте верную дату" 
         VIEW-AS ALERT-BOX INFORMATION.
         RETURN NO-APPLY.
      END.
   ELSE IF getPlanDate(TODAY) <= DATE(mDatePDG:SCREEN-VALUE)
      THEN DO:
         MESSAGE "Банк может исполнить ДГ в ближайшую плановую дату - " + STRING(getPlanDate(TODAY),"99.99.9999") 
         VIEW-AS ALERT-BOX INFORMATION.
         mDatePDG:SCREEN-VALUE = STRING(getPlanDate(TODAY)).
      END.
   ELSE IF getAgeCr() < 31
      THEN DO:
         MESSAGE "В соответствии с № 353-ФЗ Банк исполнит ДГ не позднее следующего рабочего дня с даты получения Заявления - " + STRING(AddWorkday(TODAY,1),"99.99.9999") 
         VIEW-AS ALERT-BOX INFORMATION.
         mDatePDG:SCREEN-VALUE = STRING(AddWorkday(TODAY,1)).
      END.
   ASSIGN mDatePDG = DATE(mDatePDG:SCREEN-VALUE). 
END.

ON LEAVE OF mSumCHDG
DO: 
   IF DECIMAL(mSumCHDG:SCREEN-VALUE) EQ 0.0
   THEN DO:
      MESSAGE "Введите сумму ДГ" 
      VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   ASSIGN mSumCHDG = DECIMAL(mSumCHDG:SCREEN-VALUE).

   IF getAgeCr() < 31 THEN
   DO:
      mYesNo = NO.
      MESSAGE "Сумма ЧДГ включает ближайший аннуитет?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.
      RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
      "~nmYesNo = " + STRING(mYesNo)).
      IF mYesNo EQ YES THEN
      DO:
         FIND FIRST loan WHERE RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
         DO:
            RUN l-calc2.p ("Кредит",            /* Назначение договора. */
                           loan.cont-code,      /* Номер договора. */
                           AddWorkday(TODAY,1), /* Следующий рабочий день */
                           FALSE,               /* включать/не включать пересчет течений договора */
                           TRUE).               /* выводить/ не выводить протокол на экран */
            mParList = "4,33,8,233,229,10,210,48,248,16".
            mTmpSum = 0.
            mSumPerc = 0.
            DO mIntK = 1 TO NUM-ENTRIES(mParList,","):     /* остатки по параметрам */
               RUN STNDRT_PARAM IN h_Loan  ("Кредит",     /* Назначение договора */
                             loan.cont-code,              /* Номер договора */
                             ENTRY(mIntK,mParList,","),    /* Код параметра  */
                             mDateZ,                      /* На дату (вх.остаток) */
                             OUTPUT mTmpSum,
                             OUTPUT mTmpSum1[1], OUTPUT mTmpSum1[2]).
               RUN inter_current(BUFFER loan, ENTRY(mIntK,mParList,","), OUTPUT mTmpSum2).
               mSumPerc = mSumPerc + mTmpSum + mTmpSum2.
               FOR EACH loan-int WHERE TRUE
                  AND loan-int.cont-code = loan.cont-code 
                  AND STRING(loan-int.id-d) = ENTRY(mIntK,mParList,",")
                  AND (loan-int.id-k = 30 OR loan-int.id-k = 32 OR loan-int.id-k = 8)
                  AND loan-int.mdate = mDateZ NO-LOCK:
                  mSumPerc = mSumPerc - loan-int.amt-rub.
               END.
            END.
            RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
               "~nmSumCHDG = " + STRING(mSumCHDG,"->>>>>>>>>9.99") + 
               "~nmSumBP   = " + STRING(mSumBP,  "->>>>>>>>>9.99") + 
               "~nmSumPerc = " + STRING(mSumPerc,"->>>>>>>>>9.99")).
            ASSIGN mSumCHDG = mSumCHDG - mSumBP + mSumPerc.  /*переделать*/
            RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
               "~nmSumCHDG = " + STRING(mSumCHDG,"->>>>>>>>>9.99")).
            IF mSumCHDG LE 0 THEN
            DO:
               MESSAGE "Суммы не достаточно для ближайшего платежа и досрочного гашения.~nВведите верную сумму!" 
               VIEW-AS ALERT-BOX ERROR TITLE " Внимание ".
               RETURN NO-APPLY.
            END.
            /*ASSIGN mSumCHDG:SCREEN-VALUE = STRING(mSumCHDG,"->>>>>>>>>9.99").*/
         END.
      END.
   END.
END.

ON LEAVE OF mRecBIK
DO:
   mOk = YES.
   IF mRecBIK:SCREEN-VALUE NE "" THEN
   DO:
      FIND FIRST banks-code WHERE
                 banks-code.bank-code-type EQ "МФО-9"
             AND banks-code.bank-code      EQ mRecBIK:SCREEN-VALUE
      NO-LOCK NO-ERROR.
      
      FIND banks OF banks-code NO-LOCK NO-ERROR.
      FIND FIRST banks-corr WHERE 
                 banks-corr.bank-corr EQ banks.bank-id 
      NO-LOCK NO-ERROR.
      
      IF AVAIL banks-code THEN
      DO:
         ASSIGN
            mBank:SCREEN-VALUE = banks.name
            mCorAcct:SCREEN-VALUE = IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "".
      END.
      ELSE
      DO:
         mCorAcct:SCREEN-VALUE = "".
         mBank:SCREEN-VALUE = "Банк не определен".
         RETURN NO-APPLY.
      END.
   END.
END.

ON "GO" OF FRAME frame-par  
DO:

   APPLY "LEAVE" TO mTypeDG. 
   
   IF mTypeDG:SCREEN-VALUE EQ "ЧДГ"
   AND DECIMAL(mSumCHDG:SCREEN-VALUE) EQ 0.0
   THEN DO:
      MESSAGE "Введите сумму ДГ"
      VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO mSumCHDG.
      RETURN NO-APPLY.
   END.

   IF mTypeDG:SCREEN-VALUE EQ "ПДГ" THEN
   DO:
      APPLY "LEAVE" TO  mRecBIK.
      IF mOK EQ NO THEN
      DO:
         APPLY "ENTRY" TO mRecBIK.
         RETURN NO-APPLY.
      END.
   END.
END.

ON "ESC" OF FRAME frame-par 
DO:
   RETURN.
END.

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frame-par.
      RETURN ERROR.
   END.
   UPDATE
      mCustID
      mName
      mLoanList
      mTypeDG
      mDateBP
      mSumBP
      mDatePDG
      mSumCHDG
      mRecName
      mRecAcct
      mRecBIK 
      mCorAcct
      mBank
   WITH FRAME frame-par.

   RUN Insert_TTName("DatePDG",GetDateStr(DATE(mDatePDG))).
   RUN Insert_TTName("RecName",mRecName).
   RUN Insert_TTName("RecAcct",mRecAcct).
   RUN Insert_TTName("RecBIK",mRecBIK).
   RUN Insert_TTName("CorAcct",mCorAcct).
   RUN Insert_TTName("Bank",mBank).

   sumdp  = mSumCHDG.
   datedp = mDatePDG.
   
END.

HIDE FRAME frame-par NO-PAUSE.

/*IF LASTKEY EQ 13 THEN                     */
/*DO:                                       */
/*   MESSAGE 13 LASTKEY KEYFUNCTION(LASTKEY)*/
/*   VIEW-AS ALERT-BOX.                     */
/*   RETURN.                                */
/*END.                                      */

IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN RETURN.

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
   "~nmCustID = " + STRING(mCustID) + 
   "~nmName = " + mName + 
   "~nmLoanList = " + mLoanList:SCREEN-VALUE + 
   "~nmLoan = " + mLoan).

/**/
FIND FIRST loan WHERE TRUE 
   AND loan.contract  EQ "Кредит"
   AND loan.cont-code EQ mLoan
NO-LOCK NO-ERROR.

IF AVAIL(loan) THEN 
DO:
   IF mTypeDG:SCREEN-VALUE EQ "ПДГ" THEN
   DO:
      IF loan.since <> datedp AND datedp NE ? THEN 
         RUN l-calc2.p ("Кредит",       /* Назначение договора. */
                        loan.cont-code, /* Номер договора. */
                        datedp,         /* Окончание договора + день для выполнения автом. */
                        FALSE,          /* включать/не включать пересчет течений договора */
                        TRUE).          /* выводить/ не выводить протокол на экран */
      
      sumdp = calcppdg().
   END.
   
   /*сумма прописью*/
   RUN "x-amtstr.p" (sumdp,
                     "",
                     NO,
                     YES,
                     OUTPUT c1,OUTPUT c2).
   RUN Insert_TTName("amtstrrub",c1).
   RUN Insert_TTName("amtstrkop",c2).
   /*сумма цифрами*/
   RUN Insert_TTName("amtdec",STRING(sumdp,"->>>>>>>>>9.99")).
   /*дата досрочного погашения*/
   RUN Insert_TTName("datedp",GetDateStr(DATE(datedp))).
        
   FIND LAST loan-cond WHERE loan-cond.contract   EQ loan.contract 
                         AND loan-cond.cont-code  EQ loan.cont-code 
                         AND loan-cond.class-code EQ "an-cond" 
   NO-LOCK NO-ERROR. 
   IF AVAIL loan-cond THEN
   DO:
      vShPlat = GetXAttrValueEx("loan-cond",
                                loan-cond.contract  + "," + 
                                loan-cond.cont-code + "," + 
                                STRING(loan-cond.since),
                                "СхемаПлат","?").

      IF vShPlat EQ "Аннуитетная" THEN  
         vTplName = "zay_dgan".
      ELSE IF vShPlat EQ "Дифференцированная" THEN
         vTplName = "zay_dgdif".
   END.
   ELSE vTplName = "zay_dgdif".

   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
      "~nvTplName = " + vTplName + "~nShPlat = " + vShPlat + "~nsumdp = " + STRING(sumdp)).

   /* Обработка процедурами bankinfo,userinfo,dog,lgarterm */
   RUN loanagval2.p (vTplName,
                    INPUT-OUTPUT TABLE ttnames).

   RUN printvd.p (vTplName,
                  INPUT TABLE ttnames).
                  
END. /* if avail loan then do: */

{intrface.del}     
