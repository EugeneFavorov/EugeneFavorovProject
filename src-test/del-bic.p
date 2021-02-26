{globals.i} 

DEFINE BUFFER op FOR op.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.
/*
{tmprecid.def}

{setdest.i}
*/
FOR EACH op
	WHERE op.op-date EQ TODAY
	AND op.doc-type EQ "01Š‹" 
	NO-LOCK:
	FIND FIRST op-bank1
	WHERE op-bank1.op EQ op.op
	AND op-bank1.bank-code-type EQ "BIC"
	EXCLUSIVE-LOCK NO-ERROR.
	IF AVAILABLE(op-bank1) THEN
	DO:
	PUT UNFORMATTED op.doc-num SKIP.
	DELETE op-bank1.
	END.
END.
/*
{preview.i}
*/

