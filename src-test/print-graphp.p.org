/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "Плюс Банк" kam
     Filename: fill-graphp.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 10/05/2013
*/

&GLOB nodate YES

{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{norm.i NEW}
{intrface.get pqres}

{fill-graphp.def}
{svarloan.def NEW}
{intrface.get xclass}

   /* Строка параметров */
DEF INPUT PARAM iStr AS CHAR NO-UNDO.

DEF NEW SHARED VAR rid_loan AS RECID. 


def var vEps as decimal no-undo.
def var tmpSign as char no-undo.

DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
def var typeTable as char no-undo init "1". /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */

/*
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
*/


IF NUM-ENTRIES(iStr,"|") GE 2 
   AND ENTRY(2, iStr, "|") NE "" THEN
DO:
	typeTable = ENTRY(2, iStr, "|").
end.

   /* По отмеченным договорам */
FOR EACH tmprecid NO-LOCK,
    EACH loan WHERE 
    RECID(loan) EQ tmprecid.id 
NO-LOCK:

/* ayv убрана запись ПСК в доп.реквизиты*/
/*RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).

tmpSign = GetXattrValueEx("loan",
                loan.contract + "," + loan.cont-code,
                "ПСК",
                "").
                
IF   tmpSign = "" THEN DO:              

    UpdateSigns('loan',
          loan.contract + "," + loan.cont-code,
          'ПСК',
          string(vEps),
          no).


	UpdateSigns('loan',
                  loan.contract + "," + loan.cont-code,
                  'ЭПС',
                  string(vEps),
                  yes).


    RUN SetSysConf IN h_base ("ПСКБезСтрах","1").  	
    		
	RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).
	   UpdateSigns('loan',
                  loan.contract + "," + loan.cont-code,
                  'ПСКБезСтрах',
                  string(vEps),
                  yes).	
	run deleteolddataprotocol in h_base("ПСКБезСтрах") .

END.*/

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
                   loan.cont-code,
                   loan.cont-type).

   OUTPUT STREAM fil CLOSE.

   {norm-end.i &nofil=YES &nopreview=YES} 

      /* Вывод данных по шаблону iStr (до "|") в файл отчета */
   RUN printvd.p (ENTRY(1, iStr, "|"),
                  INPUT TABLE ttnames).

END.

{intrface.del}

   /* Заполнение таблицы данными ЭПС */
PROCEDURE FillTables:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iContType AS CHAR NO-UNDO.
   
	def var fullKasko as decimal no-undo.
	DEF BUFFER bloan FOR loan.
	def var c1 as char no-undo.
	def var c2 as char no-undo.
	DEF var vidstr AS CHAR NO-UNDO.
	def var vTmpDec as decimal no-undo.
	
	vidstr = "КАСКО_К".
	vTmpDec = 0.
	FOR EACH bloan
		WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
			IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.
    	    FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

    	    IF AVAILABLE term-obl THEN
    		vTmpDec = term-obl.amt-rub.
			ELSE
        	vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    END.
	fullKasko = vTmpDec.
	
	vidstr = "КАСКО_Н".
	vTmpDec = 0.

	FOR EACH bloan
		WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
			IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.
									 
    	    FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

    	    IF AVAILABLE term-obl THEN
    		vTmpDec = term-obl.amt-rub.
			ELSE
        	vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
	end.

	fullKasko = fullKasko + vTmpDec.
   
   RUN fill-graphp.p(loan.contract, 
                    loan.cont-code, 
                    loan.since, 
					fullKasko,
					typeTable,
                    OUTPUT TABLE ttReportTable
                    ).
	
	find first ttReportTable where ttReportTable.tf_id = -1 no-error.
	if avail ttReportTable then do:
		RUN Insert_TTName ("lastDate", string(year(ttReportTable.tf_payment-date),"9999") + '-' + string(month(ttReportTable.tf_payment-date),"99") + '-' + string(day(ttReportTable.tf_payment-date),"99")). 					
		delete ttReportTable.
	end.
		
	
   RUN Insert_TTName ("graph", ""). 
   
   FIND FIRST ttNames WHERE
              ttnames.tname EQ 'graph'
   NO-LOCK NO-ERROR.

 /* if loan.cont-type = 'АвтоПлюс' then do:      */
 /* if can-do( 'Авто+Р*,Авто+М*', loan.cont-type) then */
 if typeTable = '2' then
   DO: 
      FOR EACH ttReportTable 
      BREAK BY ttReportTable.tf_payment-date:
         ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_id)                         + '\n'
                                         + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                                         + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                                         + STRING(ttReportTable.tf_sum-percent)                + '\n'
                                         + STRING(ttReportTable.tf_additional-charge1)         + '\n'
										 + '\n'
 										 + STRING(ttReportTable.tf_additional-charge2)         + '\n'
										 + STRING(ttReportTable.tf_sum-payment)                + '\n'
										 + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                                         + STRING(ttReportTable.tf_actual-payment)             + '\n'
                                         .
      END.
   END.
/*
   IF CAN-DO("Avto*,Авто*",iContType) and loan.cont-type <> 'Авто+М' and loan.cont-type <> 'Авто+Р' THEN */
   if typeTable = '1' then
   DO:
      FOR EACH ttReportTable 
      BREAK BY ttReportTable.tf_payment-date:
         ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_id)                         + '\n'
                                         + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                                         + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                                         + STRING(ttReportTable.tf_sum-percent)                + '\n'
                                         + STRING(ttReportTable.tf_additional-charge1)         + '\n'
 										 + STRING(ttReportTable.tf_additional-charge2)         + '\n'
										 + STRING(ttReportTable.tf_sum-payment)                + '\n'
										 + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                                         + STRING(ttReportTable.tf_actual-payment)             + '\n'
                                         .
      END.
   END.
   /* ayv для нового графика 15.09.14 */
   if typeTable = '4' then
   DO:
      FOR EACH ttReportTable 
      BREAK BY ttReportTable.tf_payment-date:
         ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_id)                         + '\n'
                                         + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                                         + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                                         + STRING(ttReportTable.tf_sum-percent)                + '\n'
                                         + STRING(ttReportTable.tf_additional-charge1)         + '\n'
 										 + STRING(ttReportTable.tf_sum-payment)                + '\n'
										 + STRING(ttReportTable.tf_rest-debts)                 + '\n'
										 + '\n'
                                         + STRING(ttReportTable.tf_actual-payment)             + '\n'
                                         .
      END.
   END.
   /*IF CAN-DO("MБ+(ФИЗ)",iContType) THEN  */
   if typeTable = '3' then
   DO:
      FOR EACH ttReportTable BREAK BY ttReportTable.tf_payment-date:
         ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_id)                         + '\n'
                                         + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                                         + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                                         + STRING(ttReportTable.tf_sum-percent)                + '\n'
                                         + STRING(ttReportTable.tf_sum-payment)                + '\n'
                                         + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                                         .
      END.
   END.

END PROCEDURE.

