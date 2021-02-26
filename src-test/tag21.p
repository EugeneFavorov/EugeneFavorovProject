
DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{tmprecid.def}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER bloan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan-cond FOR loan-cond.
DEFINE BUFFER comm-rate FOR comm-rate.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER op-entry FOR op-entry.
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
DEF VAR	ptsWho AS CHAR NO-UNDO.


FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FOR EACH term-obl WHERE
		term-obl.cont-code EQ loan.cont-code
		AND term-obl.contract EQ loan.contract
		AND term-obl.idnt EQ 5
		AND (term-obl.end-date = ? OR term-obl.end-date >= TODAY) 
		NO-LOCK BY term-obl.fop-date:
		
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
		    FIND FIRST signs WHERE signs.file-name = 'term-obl'
		    and signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
		    and signs.code = 'NotifRefNumber' no-lock no-error.
		    IF AVAIL signs THEN DO:
			    regNum = signs.xattr-value.
			END.
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
			ptsDate = STRING(YEAR(signs.date-value), "9999") + STRING(MONTH(signs.date-value), "99") + STRING(DAY(signs.date-value), "99").
		END.
		ELSE DO:
			ptsDate = ''.
		END.
		
	PUT UNFORMATTED
	/* �����䨪��� ��������⥫� �� �������� ���ᯥ祭�� */
	loan.cust-id 
	"^"
	/* �����䨪��� ������� ���ᯥ祭�� �� ���譥� ��⥬� */
	recid(term-obl)
	"^"
	/* ����� ������� ���ᯥ祭�� */
	NumDog
	"^"
	/* ��� � �६� ॣ����樨 ������ � ���ਠ� */
	"^"
	/* �����஢� ����� ᢨ��⥫��⢠ � ॣ����樨 � ���ਠ� */
	"^"
	/* �������樮��� ����� ᢨ��⥫��⢠ � ॣ����樨 � ���ਠ� */
	regNum
	"^"
	/* ��� ���ਠ�쭮�� 㤮�⮢�७�� */
	regDate
	"^"
	/* VIN */
	vin
	"^"
	/* ��ઠ ��⮬����� */
	mark
	"^"
	/* ���� ��⮬����� */
	colour
	"^"
	/* ��� ���᪠ */
	yearAuto
	"^"
	/* �����⥫�� */
	engine
	"^"
	/* ������ ��⮬����� */
	model
	"^"
	/* ���� � */
	shassis
	"^"
	/* ��� ����*/
	ptsSer
	"^"
	/* ��� ����� */
	ptsNum
	"^"
	/* ��� ��� �뤠� */
	ptsDate
	"^"	
	/* ��� ��� �뤠� */
	ptsWho
	"^"
	/* ���⥬� */
	'��� "���� ����"'
	CHR(13) + CHR(10)
	.
END.
END.
RETURN.

{intrface.del}
