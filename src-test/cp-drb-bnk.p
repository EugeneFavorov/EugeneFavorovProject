{globals.i}
DEFINE INPUT PARAMETER iOp AS CHARACTER.

DEFINE BUFFER op FOR op.
DEFINE BUFFER op1 FOR op.
DEFINE BUFFER signs FOR signs.
DEFINE BUFFER signs1 FOR signs.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.


FOR EACH signs
	WHERE signs.file-name EQ "op"
	AND signs.surrogate EQ ENTRY(1, iOp)
	AND signs.code NE "acct-cr"
NO-LOCK:
	CREATE signs1.
	ASSIGN
	signs1.code = signs.code
	signs1.code-value = signs.code-value
	signs1.dec-value = signs.dec-value
	signs1.file-name = signs.file-name
	signs1.surrogate = ENTRY(2, iOp)
	signs1.xattr-value = signs.xattr-value.
END.

FOR EACH op-bank
	WHERE op-bank.op EQ INT64(ENTRY(1, iOp))
	NO-LOCK:
	CREATE op-bank1.
	ASSIGN
		op-bank1.op = INT64(ENTRY(2, iOp))
		op-bank1.bank-code-type = op-bank.bank-code-type
		op-bank1.bank-code = op-bank.bank-code
		op-bank1.corr-acct = op-bank.corr-acct
		op-bank1.op-bank-type = op-bank.op-bank-type
		op-bank1.bank-name = op-bank.bank-name
		.
END.
