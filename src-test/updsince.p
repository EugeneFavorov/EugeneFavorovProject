{globals.i}

DEFINE INPUT  PARAMETER iDateDelay   AS INT64   NO-UNDO.
DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-cond FOR loan-cond.

MESSAGE  
iDateDelay  SKIP
iContract SKIP
iContCode
VIEW-AS ALERT-BOX.


FIND FIRST loan-cond
	WHERE loan-cond.contract EQ iContract
	AND loan-cond.cont-code EQ iContCode
EXCLUSIVE-LOCK NO-ERROR.

MESSAGE loan-cond.since VIEW-AS ALERT-BOX.
ASSIGN
loan-cond.since = loan-cond.since + iDateDelay.
MESSAGE loan-cond.since VIEW-AS ALERT-BOX.

FIND CURRENT loan-cond NO-LOCK NO-ERROR.

FIND FIRST loan
	WHERE loan.contract EQ iContract
	AND loan.cont-code EQ iContCode
EXCLUSIVE-LOCK NO-ERROR.

MESSAGE loan.open-date VIEW-AS ALERT-BOX.

ASSIGN
loan.open-date = loan.open-date + iDateDelay
loan.end-date = loan.end-date + iDateDelay.

MESSAGE loan.open-date VIEW-AS ALERT-BOX.

FIND CURRENT loan-cond NO-LOCK NO-ERROR.

