/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: loandoc2_vvv.p
      Comment: ��楤�� ��蠡������ ��ࠡ�⪨ �믄�
               ����� ������ �� ���থ��� �������
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

/* �饬 ����� */
RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).
IF NOT VALID-HANDLE(h_templ) THEN RETURN.
FIND FIRST loan WHERE loan.contract  EQ "dps"
    AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
    NO-LOCK NO-ERROR.
IF NOT AVAIL loan THEN RETURN.

/* ���뢠�� ����� ������� */
/* rid_loan = INT64(GetSysConf("vvv_loan")) NO-ERROR. */

/* IF ERROR-STATUS:ERROR THEN RETURN. */

/* ���⨬ �ᯮ�짮������ �६����� ��६����� */
RUN deleteolddataprotocol in h_base("vvv_loan") .

RUN messmenu.p(10,"","��ᯥ���� ������ �� ���থ��� �������?","��,���" ). 

IF INT64(pick-value) EQ 1 THEN DO:
	/* ������ ������ ������� */
	/* FIND FIRST loan WHERE RECID (loan) EQ rid_loan NO-LOCK NO-ERROR. */

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
	IF person.inn = '' THEN
		tmp = '���'.
	ELSE 
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
