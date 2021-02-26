/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: loandoc2_vvv.p
      Comment: Процедура дошаблонной обработки ВыпДо
               Печать заявления на расторжение договора
   Parameters:
         Uses:
      Used by:
      Created:
     Modified:

 */

{globals.i}
{prn-doc.def &with_proc=YES}
{norm.i NEW}
{loan_sn.i}

DEF INPUT  PARAMETER op-templ-rid AS RECID   NO-UNDO.
DEF OUTPUT PARAMETER oResult      AS INTEGER NO-UNDO.

DEF VAR tmp      AS CHAR.
DEF	VAR tmp_name AS CHAR.
DEF BUFFER loan FOR loan.
DEF BUFFER person FOR person.
DEF BUFFER cust-corp FOR cust-corp.
DEF VAR h_templ AS HANDLE  NO-UNDO.

oResult = 0.

/* ищем вклад */
RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).
IF NOT VALID-HANDLE(h_templ) THEN RETURN.
FIND FIRST loan WHERE loan.contract  EQ "dps"
    AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
    NO-LOCK NO-ERROR.
IF NOT AVAIL loan THEN RETURN.

/* считываем номер договора */
/* rid_loan = INT64(GetSysConf("vvv_loan")) NO-ERROR. */

/* IF ERROR-STATUS:ERROR THEN RETURN. */

/* очистим использованную временную переменную */
RUN deleteolddataprotocol in h_base("vvv_loan") .

RUN messmenu.p(10,"","Распечатать заявление на расторжение договора?","Да,Нет" ). 

IF INT64(pick-value) EQ 1 THEN DO:
	/* найдем запись договора */
	/* FIND FIRST loan WHERE RECID (loan) EQ rid_loan NO-LOCK NO-ERROR. */

	/* данные о клиенте */
	CASE loan.cust-cat:
	   WHEN "Ч" THEN
		  FIND FIRST person    WHERE
			 person.person-id    EQ loan.cust-id
		  NO-LOCK NO-ERROR.
	   WHEN "Ю" THEN
		  FIND FIRST cust-corp WHERE
			 cust-corp.cust-id EQ loan.cust-id
		  NO-LOCK NO-ERROR.
	END CASE.

	/* фамилия */
	RUN Insert_TTName("person_f", person.name-last).

	/* имя и отчество*/
	RUN Insert_TTName("person_io", person.first-names).

	/* дата рождения */
	RUN Insert_TTName("person_bdate", STRING( person.birthday , '99.99.9999' )).

	/* ИНН */
	IF person.inn = '' THEN
		tmp = 'нет'.
	ELSE 
		tmp = person.inn.
		
	RUN Insert_TTName("person_inn", tmp).

	/* наименование документа */
	FIND FIRST code WHERE code.class EQ "КодДокум"
					AND   code.code  EQ person.document-id 
	NO-LOCK NO-ERROR.
						  
	RUN Insert_TTName("person_docname", code.name).


	/* серия, номер */
	RUN Insert_TTName("person_docnum", person.document).
	/* кем выдан */
	RUN Insert_TTName("person_docwhom", person.issue).
	/* когда выдан */
	FIND FIRST signs WHERE signs.file-name = "person"
					AND   signs.surrogate  EQ string (person.person-id)
					AND signs.code EQ "Document4Date_vid"
	NO-LOCK NO-ERROR.

	RUN Insert_TTName("person_docwhen", STRING( signs.code-value /*, '99.99.9999'*/ )).

	/* место жительства */
	RUN Insert_TTName("person_when", person.address[1]).

	/* телефон */
	RUN Insert_TTName("person_phone", person.phone[1]).

	/* название вклада */
	FIND FIRST code WHERE code.class EQ "cont-type"
					AND   code.code  EQ loan.cont-type
	NO-LOCK NO-ERROR.
	
	/* если название содержит ';'*/
	if INDEX(code.name, ';') <> 0 then
		tmp_name = SUBSTRING( code.name, 1, INDEX(code.name, ';') - 1 ).
	else
		tmp_name = code.name.

	RUN Insert_TTName("loan_name", tmp_name).

	/* номер вклада */
	RUN Insert_TTName("loan_docnum", loan.doc-ref).
	/* дата вклада */
	RUN Insert_TTName("loan_date", STRING( loan.open-date, '99.99.9999' )).

	RUN printvd.p("loandoc12",
				  INPUT TABLE ttnames).		
	
END.
