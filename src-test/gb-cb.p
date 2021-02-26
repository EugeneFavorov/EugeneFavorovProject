{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mAcctRec AS CHARACTER NO-UNDO.

DEFINE BUFFER op FOR op.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER op-bank1 FOR op-bank.


{setdest.i}
{getdate.i}
message end-date view-as alert-box.
FOR EACH op-entry
	WHERE op-entry.filial-id EQ shFilial
	AND op-entry.op-date EQ end-date
	AND CAN-DO("30102*", op-entry.acct-db) 
	AND CAN-DO("30301*", op-entry.acct-cr) 
NO-LOCK,
FIRST op
	WHERE op.op EQ op-entry.op
	AND op.op-date EQ end-date
	AND op.op-kind BEGINS "i-ed1"
	AND NOT (op.op-status BEGINS "Ä")
NO-LOCK:
	mAcctRec = GetXAttrValueEx("op",
								STRING(op.op),
								"acct-rec",
								"NO"
								)
	.
	IF mAcctRec NE "NO" THEN
	DO:
		FIND FIRST acct
			WHERE acct.filial-id EQ shFilial
			AND acct.acct BEGINS TRIM(mAcctRec)
		NO-LOCK NO-ERROR.
		IF AVAILABLE(acct) THEN
		DO:
			FIND FIRST op-entry1
				WHERE RECID(op-entry1) EQ RECID(op-entry)
			EXCLUSIVE-LOCK NO-ERROR.
			IF AVAILABLE(op-entry1) THEN
			DO:
				op-entry1.acct-cr = acct.acct.
				PUT UNFORMATTED 
				"á†¨•≠† ·Á•‚†"
				op.op-date
				" "
				op.doc-num
				" "
				op-entry1.acct-cr
				SKIP.
			END.
			FIND CURRENT op-entry1
			NO-LOCK NO-ERROR.		
		END.
		ELSE
		DO:
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
					PUT UNFORMATTED 
					"ÑÆ°†¢´•≠®• °†≠™†"
					op.op-date
					" "
					op.doc-num
					" "
					"045209884"
					SKIP.
			END.
		END.
	END.
END.
{preview.i}
