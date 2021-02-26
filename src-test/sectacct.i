DEFINE TEMP-TABLE ttSectAcct
	FIELD num AS INT64
	FIELD order AS INT64
	FIELD acct-cat   AS CHARACTER
	FIELD razdel1    AS CHARACTER INIT " "
	FIELD name1       AS CHARACTER INIT " "
	FIELD razdel2    AS CHARACTER INIT " "
	FIELD name2       AS CHARACTER INIT " "
	FIELD bal-acct1  AS CHARACTER
.

DEFINE VARIABLE mSect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRazdel AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mRazdelName AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mi2 AS INT64 NO-UNDO.

FOR EACH tmp-code
	WHERE tmp-code.class EQ "acct-cat"
	AND tmp-code.beg-date LE end-date
	AND tmp-code.misc[2] EQ "êèÅì"
	NO-LOCK
	BREAK BY tmp-code.code BY tmp-code.beg-date:
	IF LAST-OF(tmp-code.code) THEN
	DO:
		IF NUM-ENTRIES(tmp-code.misc[1]) EQ 1 THEN
		DO:
			FOR EACH bal-acct 
				WHERE bal-acct.acct-cat EQ tmp-code.code
				NO-LOCK:
				CREATE ttSectAcct.
				ASSIGN
					ttSectAcct.acct-cat = tmp-code.code
					ttSectAcct.razdel1 = tmp-code.val
					ttSectAcct.name1 = tmp-code.name
					ttSectAcct.bal-acct1 = SUBSTRING(STRING(bal-acct.bal-acct), 1, 3)
					.
			END.
		END.
		ELSE
		DO:
			ASSIGN
				mSect = ENTRY(1, tmp-code.misc[1])
				mAcct1 = ENTRY(NUM-ENTRIES(tmp-code.misc[1]), tmp-code.misc[1])
			.
			FOR EACH signs
				WHERE signs.file-name EQ mAcct1
				NO-LOCK:
				CREATE ttSectAcct.
				FIND FIRST code
					WHERE code.class EQ signs.code
					AND code.parent EQ signs.code
					AND code.code EQ signs.code-value
				NO-LOCK NO-ERROR.
				IF signs.code EQ mSect THEN
				DO:
					ASSIGN
						ttSectAcct.acct-cat = tmp-code.code
						ttSectAcct.razdel1 = signs.code-value
						ttSectAcct.name1 = REPLACE(code.name, CHR(10), " ")
						ttSectAcct.bal-acct1 = signs.surrogate
						.
				END.
				ELSE
				DO:
					ASSIGN
						ttSectAcct.acct-cat = tmp-code.code
						ttSectAcct.razdel2 = signs.code-value
						ttSectAcct.name2 =  REPLACE(code.name, CHR(10), " ")
						ttSectAcct.bal-acct1 = signs.surrogate
						.
					FIND FIRST signs1
						WHERE signs1.file-name EQ signs.code
						AND signs1.surrogate EQ signs.code-value
					NO-LOCK NO-ERROR.
					FIND FIRST code1
						WHERE code1.class EQ mSect
						AND code1.parent EQ mSect
						AND code1.code EQ signs1.code-value
					NO-LOCK NO-ERROR.
					ASSIGN
						ttSectAcct.razdel1 = signs1.code-value
						ttSectAcct.name1 = REPLACE(code1.name, CHR(10), " ")
						.
				END.

			END.
		END.
	END.
END.

FOR EACH ttSectAcct
	WHERE ttSectAcct.acct-cat EQ "f"
	AND ttSectAcct.razdel1 EQ "5"
	AND ttSectAcct.bal-acct1 EQ "999"
	EXCLUSIVE-LOCK
	BY ttSectAcct.bal-acct1	:
	ASSIGN
		ttSectAcct.razdel1 = " "
		ttSectAcct.name1 = " "
		.
END.

mi2 = 1.
FOR EACH ttSectAcct
	EXCLUSIVE-LOCK
	BY ttSectAcct.bal-acct1
	BY ttSectAcct.bal-acct:
	
	ttSectAcct.num = mi2.
	mi2 = mi2 + 1.
END.
mi2 = 0.
FOR EACH ttSectAcct
	EXCLUSIVE-LOCK
	BREAK BY num
	BY razdel1 + razdel2:
	IF FIRST-OF(razdel1 + razdel2) THEN
		mi2 = mi2 + 1.
	ttSectAcct.order = mi2.
END.



