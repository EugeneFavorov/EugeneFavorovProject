{globals.i}

DEFINE INPUT  PARAMETER iRoleAcct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE mFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctRole AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mTime AS INT64 NO-UNDO.
DEFINE VARIABLE mTime1 AS INT64 NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER person FOR person.

DEFINE STREAM sout.

IF iRoleAcct EQ ? THEN 
	mAcctRole = "КредРасч,Кредит,КредТ,КредПр,КредПр%,КредПр%В,КредТВ,КредБудКом".
ELSE
	mAcctRole = iRoleAcct.

{getdate.i}

beg-date = end-date.

{tmprecid.def}
mTime = ETIME(yes).
FOR EACH tmprecid,
    FIRST loan
    WHERE
        RECID(loan) EQ tmprecid.id
    NO-LOCK:
    FIND FIRST person 
    	WHERE person.person-id EQ loan.cust-id
    NO-LOCK NO-ERROR.

    mFile = TRIM(ENTRY(1, loan.cont-code, "@")) + "_" + TRIM(person.name-last) + ".csv".
/*	OUTPUT TO VALUE("1.tmp") CONVERT TARGET "ibm866".
	PUT UNFORMATTED mFile SKIP.
	OUTPUT CLOSE.
	INPUT FROM VALUE("1.tmp").
	IMPORT UNFORMATTED mFile.
	INPUT CLOSE.
  MESSAGE "Обрабатывается договор " TRIM(ENTRY(1, loan.cont-code, "@")).
*/	OUTPUT 
/*	STREAM sOut  */
	TO VALUE(mFile) CONVERT TARGET "1251".

	DO mI = 1 TO NUM-ENTRIES(mAcctRole):
		FOR EACH loan-acct
			WHERE loan-acct.contract EQ loan.contract
			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ ENTRY(mI, mAcctRole)
			NO-LOCK BY loan-acct.since DESC:
			LEAVE.
		END.
		IF AVAILABLE(loan-acct) THEN
		DO:
			FIND FIRST acct
				WHERE acct.acct EQ loan-acct.acct
			NO-LOCK NO-ERROR.
			RUN VALUE("vyp_abs.p") (RECID(acct)).
		END.
	END.
	OUTPUT CLOSE.    
/*
RUN sndbispc ("file=" + mFile + ".csv" + ";class=bq").
*/
END.

/*
MESSAGE ETIME SKIP 

 VIEW-AS ALERT-BOX.
*/