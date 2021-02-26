/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ТОО "Банковские информационные системы"
     Filename: precrdprint.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 06/08/2008 feok
     Modified: 14/10/2009 Jadv (0108906)
*/


&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{filleps.def}
{norm.i NEW}
{svarloan.def NEW}
{intrface.get loan}
{lshpr.pro}
{def_work.i new}        /* Объявление переменных для работы с
                         начислением процентов. */
{intrface.get date} /* .нструменты для работы с датами */
{sh-defs.i}         /* .пределение переменных, приобретающих значения остатка */                 
/*--------------------*/
{intrface.get comm}
/*----------------------------*/

/*
&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{filleps.def}
{norm.i NEW}
{svarloan.def NEW}
{sh-defs.i new}

{intrface.get loan}

*/


 
   /* Строка параметров */
/*
DEF INPUT PARAM iStr AS CHAR NO-UNDO.
*/
&IF DEFINED(gdTplName) 
  &THEN DEFINE VARIABLE iStr AS CHARACTER INIT {&gdTplName} NO-UNDO.
  &ELSE DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
&ENDIF


DEF NEW SHARED VAR rid_loan AS RECID. 

DEF NEW GLOBAL SHARED VAR sStr AS CHAR NO-UNDO. /* для корректной работы loanagval.p при замене залога*/
ASSIGN
  sStr = iStr
.


DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
DEF VAR vAmt as decimal   NO-UNDO.
   DEF var List     as char INITIAL "КредВГар,Кред91418,Кредит,КредПр,КредТ,КредПр%,КредТВ,КредПр%В,КредТОВ,КредПр%ОВ" NO-UNDO.
   DEF var ListPar  as char INITIAL "16,9,12,15,18" NO-UNDO.
   DEF var ListParIskl  as char INITIAL "22,88,47" NO-UNDO.
   DEF var Par_     as Decimal NO-UNDO.
   DEF VAR a1       AS DECIMAL NO-UNDO.
   DEF VAR a2       AS DECIMAL NO-UNDO.
   DEF var i        as int64 NO-UNDO.
   DEF var iAmt     AS DECIMAL NO-UNDO.
   DEF var iAmtIskl AS DECIMAL NO-UNDO.

/* выбор подписанта */
IF CAN-DO('0300,0500,0000',shFilial) THEN
DO:
  {sign_select.i}
END.

   /* Если договор один, то создаем отметки */
IF     NUM-ENTRIES(iStr)   GE 2 
   AND ENTRY(2, iStr, "|") NE "" THEN
DO:
   {empty tmprecid}
   FIND FIRST loan WHERE
              loan.contract  EQ ENTRY(1, ENTRY(2, iStr, "|")) 
      AND     loan.cont-code EQ ENTRY(2, ENTRY(2, iStr, "|"))
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
      CREATE tmprecid.
      tmprecid.id = RECID(loan).
   END.
END.

   /* По отмеченным договорам */
FOR EACH tmprecid NO-LOCK,
    EACH loan WHERE 
    RECID(loan) EQ tmprecid.id 
NO-LOCK:
   ASSIGN
      rid-p    = RECID(loan)
      rid_loan = RECID(loan)
   . 
   {norm-beg.i }
     
   FIND LAST loan-cond WHERE
             loan-cond.contract  EQ loan.contract
      AND    loan-cond.cont-code EQ loan.cont-code
      AND    loan-cond.since     LE gend-date
   NO-LOCK NO-ERROR.
   IF AVAIL loan-cond THEN
      rid-t = RECID(loan-cond).

      /* Обработка процедурами bankinfo,userinfo,dog,lgarterm */
   RUN loanagval.p (ENTRY(1, iStr, "|"),
                    INPUT-OUTPUT TABLE ttnames).

      /* Заполнение таблицы данными ЭПС */
   RUN FillTables (loan.contract,
                   loan.cont-code).
   if ENTRY(1, iStr, "|") = "sprssudv02" then do:
      iAmt = 0.  
      i = 0.
      do i=1 to num-entries(List):
         find last loan-acct where loan-acct.contract = loan.contract
                               and loan-acct.cont-code = loan.cont-code
                               and loan-acct.acct-type = entry(i,List)
                               no-lock no-error.
         if avail loan-acct then do:
            run acct-pos in h_base (loan-acct.acct,
                                   loan-acct.currency,
                                   today, today, ?).
            iAmt = iAmt + abs((if loan-acct.currency = "" then sh-bal else sh-val)).
         end.
      end.
/* ivv */
      i = 0.
      iAmtIskl = 0.
      do i=1 to num-entries(ListPar):
         RUN ALL_PARAM IN h_Loan (loan.contract,         /* Тип договора */
                                  loan.cont-code,   /* Номер договора */
                                  entry(i,ListPar),                /* Код параметра */
                                  today,
                                  OUTPUT par_,     /* Сумма параметра */
                                  OUTPUT a1,        /* Валюта параметра */
                                  OUTPUT a2
                                 ).                 /* Сумма параметра в рублях */

            iAmt = iAmt + par_.
      end.
      i = 0.
      do i=1 to num-entries(ListParIskl):
         RUN ALL_PARAM IN h_Loan (loan.contract,         /* Тип договора */
                                  loan.cont-code,   /* Номер договора */
                                  entry(i,ListParIskl),                /* Код параметра */
                                  today,
                                  OUTPUT par_,     /* Сумма параметра */
                                  OUTPUT a1,        /* Валюта параметра */
                                  OUTPUT a2
                                 ).                 /* Сумма параметра в рублях */
            iAmtIskl = iAmtIskl + par_.
      end.

      if iAmt - iAmtIskl <> 0 then do:
         message "Есть не нулевой остаток ссудной задолженности" iAmt  view-as alert-box. 
         return.
      end.
/* end ivv */
   end.
   OUTPUT STREAM fil CLOSE.

   {norm-end.i &nofil=YES &nopreview=YES} 

      /* Вывод данных по шаблону iStr (до "|") в файл отчета */
   RUN printvd.p (ENTRY(1, iStr, "|"),
                  INPUT TABLE ttnames).
	{empty ttnames}
END.

   /* Заполнение таблицы данными ЭПС */
PROCEDURE FillTables:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
     
   RUN filleps.p (loan.contract, 
                  loan.cont-code, 
                  loan.since, 
                  OUTPUT TABLE ttReportTable).
  
   RUN Insert_TTName ("info", ""). 
   
   FIND FIRST ttNames WHERE
              ttnames.tname EQ 'info'
   NO-LOCK NO-ERROR.
   
   FOR EACH ttReportTable 
   BREAK BY ttReportTable.tf_payment-date:
      ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_payment-date)   + '\n'
                                      + STRING(ttReportTable.tf_sum-percent)    + '\n'
                                      + STRING(ttReportTable.tf_basic-sum-loan) + '\n'
                                      + STRING(ttReportTable.tf_actual-payment) + '\n' 
                                      + STRING(ttReportTable.tf_sum-percent     +
                                               ttReportTable.tf_basic-sum-loan  +
                                               ttReportTable.tf_actual-payment) + '\n'.
   END.

END PROCEDURE.

