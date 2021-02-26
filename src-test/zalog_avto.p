DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
DEFINE INPUT  PARAMETER iHeader AS INT64 NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mGar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGarVid AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGarDias AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDogType AS CHARACTER NO-UNDO.
DEFINE VARIABLE mObType AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNum AS INT64 NO-UNDO.
DEF VAR NumDog AS CHAR NO-UNDO.

/* �������樮��� ����� ᢨ��⥫��⢠ � ॣ����樨 � ���ਠ� */
DEF VAR	regNum AS CHAR NO-UNDO.
/* ��� ���ਠ�쭮�� 㤮�⮢�७�� */
DEF VAR	regDate AS CHAR NO-UNDO.
/* VIN */
DEF VAR vin AS CHAR NO-UNDO.
/* ��ઠ ��⮬����� */
DEF VAR	mark AS CHAR NO-UNDO.
/* ���� ��⮬����� */
DEF VAR colour AS CHAR NO-UNDO.
/* ��� ���᪠ */
DEF VAR	yearAuto AS CHAR NO-UNDO.
/* �����⥫�� */
DEF VAR	engine AS CHAR NO-UNDO.
/* ������ ��⮬����� */
DEF VAR	model AS CHAR NO-UNDO.
/* ���� � */
DEF VAR shassis AS CHAr NO-UNDO.
/* ��� ����*/
DEF VAR	ptsSer AS CHAR NO-UNDO.
/* ��� ����� */
DEF VAR	ptsNum AS CHAR NO-UNDO.
/* ��� ��� �뤠� */
DEF VAR	ptsDate AS CHAR NO-UNDO.
/* ��� ��� �뤠� */
DEF VAR	ptsWho AS CHAR INIT " " NO-UNDO.



DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER term-obl1 FOR term-obl.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op-entry FOR op-entry.

ASSIGN
mGar = "�।��,�।��,�।���,�।���"
mGarVid = "��⮬�����;!��⮬�����,*"
mGarDias = "��⮬�����,�����⢮,��,�����⥫��⢮"
.
/*
OUTPUT TO VALUE("tag8.txt") CONVERT TARGET "1251".
*/
IF iHeader EQ 1 THEN
DO:
{zalog_avto.i}
END.
FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FOR EACH term-obl 
		WHERE term-obl.contract EQ loan.contract
		AND term-obl.cont-code EQ loan.cont-code
		AND term-obl.idnt EQ 5
		AND term-obl.sop-date EQ ?
		NO-LOCK
		BY term-obl.nn:
		mSurr = term-obl.contract + "," + term-obl.cont-code + "," + "5" + "," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn).
		mDogType = GetXAttrValueEx ("term-obl", 
									mSurr, 
									"��������", 
									"").
		DO mI = 1 TO NUM-ENTRIES(mGar):
			IF ENTRY(mI, mGar) EQ mDogType THEN
				ASSIGN
					mNum = mI
					mI = 5.
		END.
		IF mNum EQ 1 OR mNum EQ 2 THEN
		DO:
			mNum = (IF GetXAttrValueEx ("term-obl", 
										mSurr, 
										"�����", 
										"") EQ "��⮬�����" THEN 1 ELSE 2).
		END.
		mObType = ENTRY(mNum, mGarDias).

		FIND LAST loan-acct
			WHERE loan-acct.contract EQ loan.contract
			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ IF term-obl.nn EQ 0 THEN ENTRY(mNum, mGar) ELSE ENTRY(mNum, mGar) + TRIM(STRING(term-obl.nn))
		    NO-LOCK NO-ERROR.

		FIND LAST loan-cond
			WHERE loan-cond.contract EQ loan.contract
			AND loan-cond.cont-code EQ loan.cont-code
		    NO-LOCK NO-ERROR.

		FOR EACH term-obl1
			WHERE term-obl1.contract EQ loan.contract
			AND term-obl1.cont-code EQ loan.cont-code
			AND term-obl1.idnt = 2
			NO-LOCK
			BY term-obl1.end-date:
/*			AND term-obl1.end-date EQ loan-cond.since
		    NO-LOCK NO-ERROR.
*/
			LEAVE.
		END.
/* ---------------------------------------------------------------------------------------------------------------- */

		FIND FIRST signs WHERE signs.file-name = 'term-obl'
			AND signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
			AND signs.code = 'reg-zalog' NO-LOCK NO-ERROR.
		IF AVAIL signs THEN DO:
			regNum = ENTRY(2,signs.xattr-value).
			regDate = STRING(YEAR(signs.date-value), "9999") + STRING(MONTH(signs.date-value), "99") + STRING(DAY(signs.date-value), "99").
		END.
		ELSE DO:
			regNum = ''.
			regDate = ''.
		END.
		NumDog = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"��������",
		"").
		vin = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCVIN",
		"").
		mark = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCbrand",
		"").
		colour = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCcolor",
		"").
		yearAuto = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCyear",
		"").		
		engine = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmotor",
		"").
		model = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmodel",
		"").
		shassis = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCchassis",
		"").
		ptsSer = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCSER",
		"").
		ptsNum = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCNUMB",
		"").
		FIND FIRST signs WHERE signs.file-name = 'term-obl'
			AND signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
			AND signs.code = 'TCDATE' NO-LOCK NO-ERROR.
		IF AVAIL signs THEN DO:
			ptsDate = STRING(signs.date-value, "99.99.9999").
		END.
		ELSE DO:
			ptsDate = ''.
		END.
		

/* ---------------------------------------------------------------------------------------------------------------- */

	PUT UNFORMATTED
/* �������� ID �।�⭮�� ������� � ��襩 �� (�᫨ ���㧪� �ந�室�� � ��᪮�쪨� �������� ���, ���ਬ�� ����� ����� 䨫��� ����� �� ᢮�� ���� - ID ����� ���ᥪ�����.) */
	iRecIDloan 
	";"
	RECID(term-obl)
	";"
/* �����䨪��� ��������⥫� �� �������� */
	STRING(term-obl.fop)
	";"
	"1"
	";"
	"��⮬�����"
	";"
/* ����� ������� */
	GetXAttrValueEx ("term-obl", 
					mSurr, 
					"��������", 
					"")
	";"
/* ��� �������  */
	STRING(term-obl.fop-date, "99.99.9999")
	";"
/* �㬬�, �ਭ��� � ���ᯥ祭��    (��������� �⮨�����) */
	TRIM(STRING(term-obl.amt-rub, ">>>>>>>>>>>9.99"))
	";"
/* ��� ����*/
	ptsSer
	";"
/* ��� ����� */
	ptsNum
	";"
/* ��� ��� �뤠� */
	ptsDate
	";"
/* ��� ��� �뤠� */
	ptsWho
	";"
/* ��ઠ ��⮬����� */
	mark
	";"
/* ��� ���᪠ */
	yearAuto
	";"
/* ���� ��⮬����� */
	colour
	";"
/* �����⥫�� */
	engine
	";"
/* ������ ��⮬����� */
	model
	";"
/* ���� � */
	shassis
	";"
/* VIN */
	vin
	CHR(13) + CHR(10)
	.
	END.
END.
/*
OUTPUT CLOSE.
*/
{intrface.del}
