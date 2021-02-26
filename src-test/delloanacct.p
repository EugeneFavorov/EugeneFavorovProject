/*   
     Filename: delloanacct.p
	 kam
*/
{globals.i}
{intrface.get xclass}

DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.

def var det as char no-undo init "".
DEF BUFFER loan-acct1 FOR loan-acct.
DEF BUFFER acct1 FOR acct.

/* VIN */
DEF VAR vin AS CHAR NO-UNDO.
/* ��ઠ ��⮬����� */
DEF VAR mark AS CHAR NO-UNDO.
/* ��� ���᪠ */
DEF VAR yearAuto AS CHAR NO-UNDO.
/* ������ ��⮬����� */
DEF VAR model AS CHAR NO-UNDO.

  if in-rid <> 0 then
     find op-templ where recid(op-templ) = in-rid no-lock no-error .
  find op-entry where recid(op-entry)  = in-rid1 no-lock no-error .
  if (in-rid <> 0 and not avail op-templ) or not avail op-entry then do :
/*    message '�訡�� ����஫� �஢���� ' view-as alert-box error.  */
    return .
  end.

FIND FIRST loan-acct WHERE loan-acct.contract = '�।��'
	AND loan-acct.acct-type = '�।��'
	AND loan-acct.acct = op-entry.acct-cr
	NO-LOCK NO-ERROR.
IF AVAIL loan-acct THEN DO:
	FIND LAST loan-acct1 WHERE loan-acct1.contract = '�।��'
		AND loan-acct1.cont-code = loan-acct.cont-code
		AND loan-acct1.acct-type = '�।62101'
		AND loan-acct1.acct = op-entry.acct-db NO-ERROR.
	IF AVAIL loan-acct1 THEN DELETE loan-acct1.
	FIND LAST term-obl WHERE term-obl.cont-code EQ loan-acct.cont-code
		AND term-obl.contract EQ '�।��'
		AND term-obl.idnt EQ 5 NO-LOCK NO-ERROR.
	IF AVAIL term-obl THEN DO:
		yearAuto = GetXAttrValueEx("term-obl",
		term-obl.contract + "," + term-obl.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCyear",
		"").
		vin = GetXAttrValueEx("term-obl",
		term-obl.contract + "," + term-obl.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCVIN",
		"").
		model = GetXAttrValueEx("term-obl",
		term-obl.contract + "," + term-obl.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmodel",
		"").
		mark = GetXAttrValueEx("term-obl",
		term-obl.contract + "," + term-obl.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCbrand",
		"").
		
	END.
		
		
		
	FIND FIRST acct1 WHERE acct1.acct = op-entry.acct-db NO-ERROR.
	IF AVAIL acct1 THEN DO:
		acct1.branch-id = '0009'.
		acct1.details = '�।�⢠ ��㤠, ����祭�� �� ������ࠬ ����㯭���, ������, �����祭�� ������ �� ��।�����, ��⮬����� ' + yearAuto + ' ' + model + ' ' + mark + ' VIN ' + vin.
	END.
	
END.

{intrface.del}

