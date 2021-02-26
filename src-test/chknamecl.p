{globals.i}

DEFINE INPUT PARAMETER iAcct AS CHARACTER.
DEFINE INPUT PARAMETER iClient AS CHARACTER.

DEFINE BUFFER person FOR person.
DEFINE BUFFER acct FOR acct.

DEFINE VARIABLE mName1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName2 AS CHARACTER NO-UNDO.

mName1 = REPLACE(iClient, ",", " ").
mName1 = REPLACE(mName1, "ñ", "¥").

pick-value = "0".

FIND FIRST acct
	WHERE acct.acct BEGINS iAcct
	NO-LOCK NO-ERROR.
IF AVAILABLE(acct) THEN
DO:
	IF acct.cust-cat EQ "—" THEN
	DO:
		FIND FIRST person
			WHERE person.person-id EQ acct.cust-id
		NO-LOCK NO-ERROR.
		IF AVAILABLE(person) THEN
		DO:
			mName2 = REPLACE(person.name-last, "ñ", "¥") + " " + REPLACE(TRIM(person.first-names), "ñ", "¥").
			IF mName1 EQ mName2 THEN
				pick-value = "1".
		END.
	END.
	ELSE
		pick-value = "1".
END.

RETURN pick-value.

