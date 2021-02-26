DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mTypeAnnuit AS CHARACTER INIT "1" NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan1 FOR loan.
DEFINE BUFFER loan-cond FOR loan-cond.
DEFINE BUFFER loan-cond1 FOR loan-cond.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan-var FOR loan-var.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op-entry FOR op-entry.
def var opendate as date no-undo.
/*
OUTPUT TO VALUE("tag1.txt") CONVERT TARGET "1251".
*/
FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	IF loan.open-date LT DATE("30.04.2015") THEN
		FIND FIRST loan1
			WHERE loan1.filial-id EQ "0400"
			AND loan1.contract EQ loan.contract
			AND loan1.cont-code EQ ENTRY(1, loan.cont-code, "@") + "@0400"
		NO-LOCK NO-ERROR.
	IF NOT AVAILABLE(loan1) THEN
		FIND FIRST loan1
		    WHERE RECID(loan1) EQ RECID(loan)
		    NO-LOCK NO-ERROR.

	FIND FIRST loan-cond
		WHERE loan-cond.contract EQ loan.contract
		AND loan-cond.cont-code EQ loan.cont-code
	    NO-LOCK NO-ERROR.

	FIND LAST loan-cond1
		WHERE loan-cond1.contract EQ loan.contract
		AND loan-cond1.cont-code EQ loan.cont-code
	    NO-LOCK NO-ERROR.
/*	
	FOR EACH loan-cond1
		WHERE loan-cond1.contract EQ loan1.contract
		AND loan-cond1.cont-code EQ loan1.cont-code
		BY loan-cond1.since DESC:
		LEAVE.
	END.
*/
/*
MESSAGE loan-cond.cont-code SKIP loan-cond1.since VIEW-AS ALERT-BOX.
*/
	FIND FIRST term-obl
		WHERE term-obl.contract EQ loan.contract
		AND term-obl.cont-code EQ loan.cont-code
		AND term-obl.idnt = 2
/*		AND term-obl.fop-date EQ loan-cond.since */
		AND term-obl.end-date EQ loan-cond.since
	    NO-LOCK NO-ERROR.

	mTypeAnnuit = IF GetXAttrValueEx("loan-cond",
					loan-cond1.contract + "," + loan-cond1.cont-code + "," + STRING(loan-cond1.since),
					"int-offset",
					"0"
					) EQ "->" THEN "1" ELSE "0".

    opendate = loan.open-date.
   /* IF loan.open-date > DATE("30.04.2015") THEN  */ do:
        find first loan-int where loan-int.cont-code = loan.cont-code 
            and loan-int.contract = '�।��'
            and loan-int.id-k = 3
            and loan-int.id-d = 0
            and loan-int.mdate >= loan.open-date
            and loan-int.amt-rub <> 0
            no-lock no-error.
        if avail loan-int then opendate = loan-int.mdate.
    end.
	PUT UNFORMATTED
/* �����䨪��� ������� �� ���譥� ��⥬� */
	iRecIDloan 
	"^"
/* ������  */
	""
	"^"
/* ����� ������� */
	TRIM(ENTRY(1, loan.doc-ref))
	"^"
/* ������᪨� �த�� */
	IF loan.class-code EQ "loan_aig" THEN "���⥪�" ELSE "��⮪।��"
	"^"
/* ��� �������  */
	STRING(YEAR(loan.open-date), "9999") + STRING(MONTH(loan.open-date), "99") + STRING(DAY(loan.open-date), "99")
	"^"
/* ��� �����஢���� */
	STRING(YEAR(opendate), "9999") + STRING(MONTH(opendate), "99") + STRING(DAY(opendate), "99")
	"^"
/* ��� ����襭��  ������� */
	STRING(YEAR(loan.end-date), "9999") + STRING(MONTH(loan.end-date), "99") + STRING(DAY(loan.end-date), "99")
	"^"
/* ��� �����襭��  ������� */
	"19000101"
	"^"
/* �㬬� */
	TRIM(STRING(term-obl.amt-rub, ">>>>>>>>>>>9.99"))
	"^"
/* �㬬� (�������⥫쭮) */
	"0"
	"^"
/* ����� */
	IF loan1.currency EQ "" THEN "810" ELSE loan1.currency
	"^"
/* ������ ����襭�� �᭮����� ����� */
	"1"
	"^"
/* ��������� ���⥦ (�㬬�) */
	REPLACE(GetXAttrValueEx("loan-cond",
		loan-cond1.contract + "," + loan-cond1.cont-code + "," + STRING(loan-cond1.since),
		"����⏫��",
		"0"
		), ",", "")
	"^"
/* ��� �������筮�� ���⥦� */
	loan-cond1.cred-date
	"^"
/* �ਬ�砭�� */
	""
	"^"
/* ������ �� �������� (�����䨪��� ������ �� ���譥� ��⥬�) */
	STRING(loan1.cust-id)
	"^"
/* �ਧ��� ����⢨� ������� */
	"2"
	"^"
/* ����⥫�᪨� ������� */
	""
	"^"
/* ����� */
	"128"
	"^"
/* ��� �죮⭮�� ��ਮ�� */
	""
	"^"
/* �㬬� ���⥦� �죮⭮�� ��ਮ�� */
	""
	"^"
/* ����稭� ������ ����筮�� ����襭�� */
	""
	"^"
/* ��� �ப� ������ ����筮�� ����襭�� */
	""
	"^"
/* �����⥫� ������� */
	""
	"^"
/* �⤥�����  */
	""
	"^"
/* ��� ������� */
	"0"
	"^"
/* ��� ������ */
	mTypeAnnuit
	"^"
/* ���⥬� */
	'��� "���� ����"'
	CHR(13) + CHR(10)
	.

END.
/*
OUTPUT CLOSE.
*/
{intrface.del}
