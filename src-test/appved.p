/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "Плюс Банк" 
     Filename: .p
      Comment: Процедура формирования АПП страховых полюсов
   Parameters: 
         Uses:
      Used by: 
      Created: ayv
*/

{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEF TEMP-TABLE ttApp NO-UNDO
   FIELD num    AS INT	 /*порядковый номер строки*/
   FIELD fio    AS CHAR  /*фио кредитуемого*/
   FIELD numkd  AS CHAR  /*номер кд*/
   FIELD datekd AS DATE  /*дата кд*/
   .
   
DEF BUFFER bloan      FOR loan.
DEF BUFFER bperson    FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF VAR i    AS INT INIT 0 NO-UNDO.
DEF VAR vFio AS CHAR       NO-UNDO.

/*формирование данных*/
FOR EACH tmprecid NO-LOCK,
   EACH bloan WHERE 
   RECID(bloan) EQ tmprecid.id 
NO-LOCK:
	
   /* определяем фио (на всякий случай проверяем и cust-corp) */
   IF bloan.cust-cat = 'Ч' THEN DO:
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
   
   /*заполняем temp-table*/
   CREATE ttApp.
   ASSIGN
      ttApp.num    = i
	  ttApp.fio    = vFio
	  ttApp.numkd  = bloan.doc-ref
	  ttApp.datekd = bloan.open-date
   .
   
END.

/*заполняем таблицу для шаблона*/
RUN Insert_TTName ("graph", ""). 
   
FIND FIRST ttNames WHERE
   ttnames.tname EQ 'graph'
NO-LOCK NO-ERROR.
   
FOR EACH ttApp 
   BREAK BY ttApp.num:

   ttnames.tvalue = ttnames.tvalue + STRING(ttApp.num)                  + '\n'
                                   + STRING(ttApp.fio)                  + '\n'
                                   + STRING(ttApp.numkd)                + '\n'
                                   + STRING(ttApp.datekd, "99.99.9999") + '\n'
                                   + 'страховой сертификат'             + '\n'
								   + '\n'
                                   .
   
END.

RUN Insert_TTName ("date",STRING(DAY(TODAY))   + '.' +
						  STRING(MONTH(TODAY)) + '.' +
						  STRING(YEAR(TODAY))).
						  
RUN printvd.p('appved',INPUT TABLE ttnames).