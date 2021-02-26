/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: LOANDOC1.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 18.04.2008 12:15 fEAk    
     Modified: 18.04.2008 12:15 fEAk     <comment>
*/

{globals.i}
{prn-doc.def &with_proc=YES}
{norm.i NEW}

def input parameter in-branch-id like DataBlock.branch-id no-undo.

DEF  VAR rid_loan  AS RECID.

DEF VAR cur_loan  AS RECID.
def var tmp	as char.
def	var tmp_name as char.

   
/* ���뢠�� ����� ������� */
rid_loan = INT64(GetSysConf("vvv_loan")) NO-ERROR.

IF ERROR-STATUS:ERROR THEN RETURN.

/* ���⨬ �ᯮ�짮������ �६����� ��६����� */
run deleteolddataprotocol in h_base("vvv_loan") .

/* �᫨ ��諨 ����� ������� */

IF rid_loan NE ? THEN
	DO:
	/*
	MESSAGE "��ᯥ���� ������ �� ���থ��� �������?"
	VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
	TITLE "" UPDATE choice AS LOGICAL.
	*/
	run messmenu.p(10,"","��ᯥ���� ������ �� ���থ��� �������?","��,���" ).
	
	CASE INT64(pick-value):
	WHEN 1 THEN /* Yes */
		DO:
			/* ������ ������ ������� */
			FIND FIRST loan WHERE RECID (loan) EQ rid_loan NO-LOCK NO-ERROR.

			/* ����� � ������ */
			CASE loan.cust-cat:
			   WHEN "�" THEN
				  FIND FIRST person    WHERE
					 person.person-id    EQ loan.cust-id
				  NO-LOCK NO-ERROR.
			   WHEN "�" THEN
				  FIND FIRST cust-corp WHERE
					 cust-corp.cust-id EQ loan.cust-id
				  NO-LOCK NO-ERROR.
			END CASE.

			/* 䠬���� */
			RUN Insert_TTName("person_f", person.name-last).

			/* ��� � ����⢮*/
			RUN Insert_TTName("person_io", person.first-names).

			/* ��� ஦����� */
			RUN Insert_TTName("person_bdate", STRING( person.birthday , '99.99.9999' )).

			/* ��� */
			if person.inn = '' then
				tmp = '���'.
			else 
				tmp = person.inn.
				
			RUN Insert_TTName("person_inn", tmp).

			/* ������������ ���㬥�� */
			FIND FIRST code WHERE code.class EQ "�������"
							AND   code.code  EQ person.document-id 
			NO-LOCK NO-ERROR.
								  
			RUN Insert_TTName("person_docname", code.name).


			/* ���, ����� */
			RUN Insert_TTName("person_docnum", person.document).
			/* ��� �뤠� */
			RUN Insert_TTName("person_docwhom", person.issue).
			/* ����� �뤠� */
			FIND FIRST signs WHERE signs.file-name = "person"
							AND   signs.surrogate  EQ string (person.person-id)
							AND signs.code EQ "Document4Date_vid"
			NO-LOCK NO-ERROR.

			RUN Insert_TTName("person_docwhen", STRING( signs.code-value /*, '99.99.9999'*/ )).

			/* ���� ��⥫��⢠ */
			RUN Insert_TTName("person_when", person.address[1]).

			/* ⥫�䮭 */
			RUN Insert_TTName("person_phone", person.phone[1]).

			/* �������� ������ */
			FIND FIRST code WHERE code.class EQ "cont-type"
							AND   code.code  EQ loan.cont-type
			NO-LOCK NO-ERROR.
			
			/* �᫨ �������� ᮤ�ন� ';'*/
			if INDEX(code.name, ';') <> 0 then
				tmp_name = SUBSTRING( code.name, 1, INDEX(code.name, ';') - 1 ).
			else
				tmp_name = code.name.

			RUN Insert_TTName("loan_name", tmp_name).

			/* ����� ������ */
			RUN Insert_TTName("loan_docnum", loan.doc-ref).
			/* ��� ������ */
			RUN Insert_TTName("loan_date", STRING( loan.open-date, '99.99.9999' )).

			RUN printvd.p("loandoc12",
						  INPUT TABLE ttnames).		
		
		END.
	END CASE.
	END.	  