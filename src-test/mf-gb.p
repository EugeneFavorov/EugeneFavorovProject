{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mAcctRec AS CHARACTER NO-UNDO.

DEFINE BUFFER op FOR op.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.

/*
{setdest.i} */
{getdate.i}
message shFilial view-as alert-box.
IF shFilial EQ "0400" THEN
FOR EACH op-entry
	WHERE op-entry.filial-id EQ shFilial
	AND op-entry.op-date EQ end-date
	AND CAN-DO("30102*129*", op-entry.acct-db) 
	AND CAN-DO("30301*129*", op-entry.acct-cr) 
NO-LOCK,
FIRST op
	WHERE op.op EQ op-entry.op
	AND op.op-date EQ end-date
	AND op.op-kind BEGINS "i-ed1"
	AND NOT (op.op-status BEGINS "Ä")
NO-LOCK:
/*	mAcctRec = GetXAttrValueEx("op",
								STRING(op.op),
								"acct-rec",
								"NO"
								)
	.
*/	
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
				op-bank1.bank-code = "044525129"
				op-bank1.corr-acct = "30101810945250000129"
				op-bank1.op-bank-type = "rec"
				op-bank1.bank-name = 'éÄé "èãûë ÅÄçä" É åéëäÇÄ'
				.
				PUT UNFORMATTED 
				"ÑÆ°†¢´•≠®• °†≠™†"
				op.op-date
				" "
				op.doc-num
				" "
				"044525129"
				SKIP.
		END.
END.
/*
{preview.i}
*/
