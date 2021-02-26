/*
               Банковская интегрированная система БИСквит
    Copyright: ПАО "Плюс Банк" 
     Filename: rasppdg.p
      Comment: Процедура формирования распоряжения на списание залога при ПГ кредита
   Parameters: 
         Uses:
      Used by: 
      Created: pda
*/

{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{intrface.get loan}

DEF TEMP-TABLE ttApp NO-UNDO
   FIELD num    AS INT	    /*порядковый номер строки*/
   FIELD fio    AS CHAR     /*фио кредитуемого*/
   FIELD numkd  AS CHAR     /*номер кд*/
   FIELD datekd AS DATE     /*дата кд*/
   FIELD sumob  AS DECIMAL  /*сумма обеспечения*/
   FIELD sumob2 AS DECIMAL  /*сумма обеспечения*/
   .
   
DEF BUFFER bloan      FOR loan.
DEF BUFFER bperson    FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF INPUT PARAMETER iParam AS CHAR NO-UNDO.

DEF VAR i        AS INT INIT 0 NO-UNDO.
DEF VAR vFio     AS CHAR       NO-UNDO.
DEF VAR a1       AS CHAR       NO-UNDO.
DEF VAR a2       AS CHAR       NO-UNDO.
DEF VAR par_28   AS DECIMAL    NO-UNDO.
DEF VAR par_40   AS DECIMAL    NO-UNDO.

/* {getdate.i} */

/*формирование данных*/
FOR EACH tmprecid NO-LOCK,
   EACH bloan WHERE 
   RECID(bloan) EQ tmprecid.id 
NO-LOCK:
	
   /* определяем фио (на всякий случай проверяем и cust-corp) */
   IF bloan.cust-cat = 'Ч' THEN 
   DO:
      FIND FIRST bperson WHERE
         bperson.person-id EQ bloan.cust-id
      NO-LOCK NO-ERROR.
	   IF AVAIL bperson THEN 
	      vFio = bperson.name-last + ' ' + bperson.first-names.
   END.
   ELSE DO:
      FIND FIRST bcust-corp WHERE
	      bcust-corp.cust-id EQ bloan.cust-id
	   NO-LOCK NO-ERROR.
	   IF AVAIL bcust-corp THEN
	      vFio = bcust-corp.name-short.
   END.
   
   i = i + 1.

   RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                            bloan.cont-code,  /* Номер договора */
                            28,               /* Код параметра */
                            TODAY,
                            OUTPUT par_28,    /* Сумма параметра */
                            OUTPUT a1,        /* Валюта параметра */
                            OUTPUT a2
                           ).                 /* Сумма параметра в рублях */

   RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                            bloan.cont-code,  /* Номер договора */
                            40,               /* Код параметра */
                            TODAY,
                            OUTPUT par_40,    /* Сумма параметра */
                            OUTPUT a1,        /* Валюта параметра */
                            OUTPUT a2
                           ).                 /* Сумма параметра в рублях */

   /*заполняем temp-table*/
   CREATE ttApp.
   ASSIGN
      ttApp.num    = i
	   ttApp.fio    = vFio
	   ttApp.numkd  = bloan.doc-ref
	   ttApp.datekd = bloan.open-date
      ttApp.sumob  = par_28
      ttApp.sumob2 = par_40
   .
   
END.

/*заполняем таблицу для шаблона*/
RUN Insert_TTName ("graph", ""). 
   
FIND FIRST ttNames WHERE
   ttnames.tname EQ 'graph'
NO-LOCK NO-ERROR.
   
FOR EACH ttApp 
   BREAK BY ttApp.num:

   ttnames.tvalue = ttnames.tvalue + STRING(ttApp.num)                                       + '\n'
                                   + STRING(ttApp.numkd) + ' от ' 
                                                         + STRING(ttApp.datekd, "99.99.9999")
                                                         + ' г.' + '\n'
                                   + STRING(ttApp.fio)                                       + '\n'
                                   + STRING(ttApp.sumob)                                     + '\n'
                                   + STRING(ttApp.sumob2)                                    + '\n'
                                   .
END.

FIND FIRST _user WHERE 
           _userid EQ userid("bisquit")
NO-LOCK NO-ERROR.

IF AVAIL _user THEN 
   RUN Insert_TTName("username", _user._user-name).

RUN Insert_TTName ("date",STRING(DAY(TODAY),"99") + '.' +
						  STRING(MONTH(TODAY),"99") + '.' +
						  STRING(YEAR(TODAY),"9999") + ' г.').
						  
RUN printvd.p(iParam, INPUT TABLE ttnames).