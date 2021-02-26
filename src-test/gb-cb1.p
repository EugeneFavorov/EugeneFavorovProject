{globals.i}
/*
DEFINE INPUT PARAMETER iOp AS INT64.
*/

DEFINE BUFFER op FOR op.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.

{tmprecid.def}

FOR EACH tmprecid,
    FIRST op
	WHERE RECID(op) EQ tmprecid.id
/*	and op.op-kind EQ "i-ed101-0"
	AND op.doc-num EQ "932" */
	NO-LOCK:
	FOR EACH op-bank
		WHERE op-bank.op EQ op.op
		NO-LOCK:
		FIND FIRST op-bank1 
			WHERE RECID(op-bank1) EQ RECID(op-bank)
		EXCLUSIVE-LOCK NO-ERROR.
		IF AVAILABLE(op-bank1) THEN 
		DO:
			IF op-bank1.bank-code NE "045209884" THEN
				op-bank1.op-bank-type = "send".
			ELSE
				op-bank1.op-bank-type = "rec".
		END.		
	END.	
	FIND FIRST op-bank1 
		WHERE op-bank1.op EQ op.op
			AND op-bank1.op-bank-type EQ "rec"
	NO-LOCK NO-ERROR.
	IF NOT AVAILABLE(op-bank1) THEN 
	DO:
		CREATE op-bank1.
		ASSIGN
			op-bank1.op = op.op
			op-bank1.bank-code-type = "åîé-9"
			op-bank1.bank-code = "045209884"
			op-bank1.corr-acct = "30101810152090000884"
			op-bank1.op-bank-type = "rec"
			op-bank1.bank-name = 'éåëäàâ îàãàÄã éÄé "èãûë ÅÄçä", É éåëä'
			.
	END.
END.
