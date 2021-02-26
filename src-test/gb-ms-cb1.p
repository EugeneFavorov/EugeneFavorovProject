{globals.i}
/* DEFINE INPUT PARAMETER iOp AS INT64. */

DEFINE BUFFER op FOR op.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.
DEFINE BUFFER xlink FOR xlink.
DEFINE BUFFER links FOR links.


DEFINE VARIABLE mOp AS INT64 NO-UNDO.
DEFINE VARIABLE mOp_old AS INT64 NO-UNDO.
{tmprecid.def}
{setdest.i}
FOR EACH tmprecid:
	FIND FIRST op
	WHERE RECID(op) EQ tmprecid.id
	NO-LOCK NO-ERROR.

	FOR EACH xlink
		WHERE xlink.link-code EQ "op-mf"
		NO-LOCK,
		FIRST links
		WHERE links.link-id EQ xlink.link-id
		AND link.target-id EQ STRING(op.op)
		NO-LOCK:
		LEAVE.
	END.
	IF AVAILABLE(xlink)	THEN
	DO:
		FOR FIRST op-bank
			WHERE op-bank.op EQ INT64(links.source-id)
			AND op-bank.op-bank-type NE "send"
		NO-LOCK:
			FIND FIRST op-bank1
				WHERE op-bank1.op EQ op.op
			EXCLUSIVE-LOCK NO-ERROR.
			IF AVAILABLE(op-bank1)	THEN
			DO:

				ASSIGN
					op-bank1.bank-code-type = op-bank.bank-code-type
					op-bank1.bank-code = op-bank.bank-code
					op-bank1.corr-acct = op-bank.corr-acct
					op-bank1.op-bank-type = op-bank.op-bank-type
					op-bank1.bank-name = op-bank.bank-name
					.
			END.
		END.
	END.
/*	FIND CURRENT op
	EXCLUSIVE-LOCK NO-ERROR.
	IF AVAILABLE(op) THEN
		op.user-id = "K0400MVS". */

END.
{preview.i}