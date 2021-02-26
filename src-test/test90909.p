{tmprecid.def}
{globals.i}

DEF VAR vac-db like op-entry.acct-db.
DEF VAR vdop as char no-undo.
DEF VAR vstr as char no-undo.

FOR EACH tmprecid NO-LOCK,
	FIRST op WHERE RECID(op) EQ tmprecid.id  
	exclusive-lock:
	vdop = "".
	vstr = "".
	find first op-entry where op-entry.op = op.op NO-LOCK.
	vac-db = op-entry.acct-db.
	find first loan-acct where loan-acct.acct = vac-db no-lock.
	vdop = getxattrvalue ("loan", string("dps," + loan-acct.cont-code), "ДатаНачПереч").
	vstr = op.details.
	vdop = "от " + vdop.
	vstr = replace(vstr,"от  ", vdop).
	op.details = vstr.


END.