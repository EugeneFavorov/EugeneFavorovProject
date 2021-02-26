/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: ved_insur.p
      Comment: ���� �� �������� ���客���� ��� ��業��
   Parameters:
         Uses:
      Used by:
      Created: 11/03/13
     Modified: 11/03/13 Serge
*/
{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{intrface.get lngar}
{intrface.get chwch}
{intrface.get xobj}     /* ������⥪� ��� ࠡ��� � ��ꥪ⠬�. */
{client.i}
{lshpr.pro}
{tmprecid.def}
{wordwrap.def}
{svarloan.def}
{navigate.def}
{loan_par.def &new = new} 
{flt-file.i}
{sh-defs.i}

def new shared stream vvs.
def var fname as char  init "./pros_kasko.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

DEF BUFFER bloan   FOR loan.
DEF BUFFER bbloan   FOR loan.
DEF BUFFER bcloan   FOR loan.
DEF BUFFER bperson FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF VAR cname AS CHAR NO-UNDO.
DEF VAR cdetails AS CHAR NO-UNDO.
DEF VAR cont-code AS CHAR NO-UNDO.
DEF VAR doc-ref AS CHAR NO-UNDO.
DEF VAR g AS LOGICAL NO-UNDO.

DEF VAR tmp_fear 	AS CHAR NO-UNDO.
DEF VAR tmp_client 	AS CHAR NO-UNDO.

DEF VAR tmp_contact AS CHAR NO-UNDO.

DEFINE TEMP-TABLE Kasko
	FIELD cont-code AS CHARACTER
	FIELD doc-ref   AS CHARACTER
	FIELD cust-id   AS INTEGER
	FIELD open-date AS DATE
	FIELD close-date  AS DATE
	FIELD end-date  AS DATE
	FIELD cdetails  AS CHARACTER
	FIELD fear_name AS CHARACTER
	FIELD client_name AS CHARACTER
.


{getdates.i}

{setdest.i &col=170 }
PUT UNFORMATTED "������ ESC ��� ���㧪� ���� � BisPC" skip(1).

{spinner.i "���㭤��� ... " }	
/**/
FOR EACH bloan
WHERE bloan.contract EQ '�।��'
/* plus.vvv */
/* add 24/06/2014 */
AND bloan.cont-code MATCHES('*���*')
/**/
AND bloan.end-date GE end-date
AND (bloan.close-date GE end-date OR bloan.close-date = ?)
NO-LOCK:
	/**/
	cont-code = bloan.cont-code.
	/**/
	FOR EACH bbloan
		WHERE bbloan.parent-contract eq bloan.contract
		AND bbloan.parent-cont-code eq cont-code
		/**/
		/*AND CAN-DO("*���*", bbloan.parent-cont-code)*/ :
		/**/
	/*BY bbloan.end-date DESCENDING:*/

			/* �ਥ��஢�筮 �� 20.02.2013 �. �� ������� ������客���� � ����� �뫨 �� 㬮�砭�� �����, ���. ४������ � ��� ���� ��������� �� ᮡ�ࠥ���, ���⮬� ⠪ */
			IF bbloan.open-date LT DATE(02,20,2013) OR GetXAttrValueEx("loan", STRING("�����," + bbloan.cont-code), "VidStr", "") begins "�����" THEN 
				DO:
					/*--------------------------------------------*/
					/* ��� �� ���� �� 㪠������ ��� �室�� � ��ਮ� ����⢨� ������� ���客���� ���� ��� ���� ��� ��ਮ�� �� ࠧ�� ��஭�*/
					IF ( beg-date >= bbloan.open-date AND beg-date <= bbloan.end-date ) OR ( end-date >= bbloan.open-date AND end-date <= bbloan.end-date )
						OR ( beg-date <= bbloan.open-date AND end-date >= bbloan.end-date ) THEN DO:
						/**/
						RUN GetName(INPUT bbloan.cust-cat,INPUT bbloan.cust-id,OUTPUT tmp_fear,OUTPUT tmp_contact).
						/**/
						RUN GetName(INPUT bloan.cust-cat,INPUT bloan.cust-id,OUTPUT tmp_client,OUTPUT tmp_contact).
						/**/
						CREATE Kasko.
						ASSIGN
							kasko.cont-code = bbloan.parent-cont-code
							kasko.fear_name = tmp_fear
							kasko.client_name = tmp_client
							kasko.cdetails = tmp_contact
							kasko.doc-ref = bbloan.cont-code
							kasko.close-date  = bbloan.end-date
							kasko.open-date = bbloan.open-date
						.
						g = true.
						/*LEAVE.*/
					END.
					/*--------------------------------------------*/
				END.
    END.
    /*END.*/
END.

IF g THEN DO:
	PUT UNFORMATTED
	  "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
	  "�                                                                                                ��������� �� ������ࠬ �����                                                                                                              �" SKIP
	  "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
	  "�� �।�⭮�� ������� �                 ����騪                     �� ������� ���客����                ���客騪                  ������. ��������. �� �                            ���⠪�� ����騪�                              �" SKIP
	  "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
	output stream vvs to value (fname)
	UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
	/**/
	put stream vvs unformatted
		"��������� �� ������ࠬ �����" eol.
	/**/
	put stream vvs unformatted
		"� ��" delim
		"����騪" delim
		"� ��" delim
		"���客騪" delim
		"��� ������ ��" delim
		"��� ����砭�� ��" delim
		"���⠪�� ����騪�" delim
		eol.
	/**/
	for each Kasko
	no-lock by Kasko.doc-ref by Kasko.open-date by Kasko.end-date:
		PUT UNFORMATTED
			"�"  STRING(kasko.cont-code, "x(22)")
			"�"  STRING(kasko.client_name, "x(45)")
			"�"  STRING(kasko.doc-ref, "x(22)")
			"|"  STRING(kasko.fear_name, "x(44)")
			"�"  STRING(kasko.open-date, "99/99/9999")
			"�"  STRING(kasko.close-date, "99/99/9999")
			"�"  STRING(kasko.cdetails, "x(75)")
			"�" skip.
		/**/	
		put stream vvs unformatted
			kasko.cont-code delim
			kasko.client_name delim
			kasko.doc-ref delim
			kasko.fear_name delim	
			kasko.open-date delim
			kasko.close-date delim			
			kasko.cdetails delim
			eol.
	end.
	output stream vvs close.
	PUT UNFORMATTED
	  "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP.
	{preview.i &col=170}
	MESSAGE "����� ���㦥�� � 䠩� " + fname + "." VIEW-AS ALERT-BOX.
	RUN sndbispc ("file=" + fname + ";class=bq").
END.
ELSE MESSAGE "������ �� �������" VIEW-AS ALERT-BOX.

/* ��室�� ������������ ������ */
PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 DEF OUTPUT PARAMETER cdetails AS CHARACTER.
 
 IF cat = "�" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN DO:
		/* ��� ������ */
		sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		/* ���⠪�� */
		cdetails = '����: ' + person.address[1] + '; ����䮭: ' + person.phone[1] + ' ' + person.phone[2].
	END.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN DO:
		/* ������������ �࣠����樨 */
		sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		/* ���⠪�� */
		cdetails = '����: ' + cust-corp.addr-of-low[1] + '; ����: ' + cust-corp.fax.
	END.
  END.
END.

{intrface.del}
