{globals.i}

DEFINE INPUT  PARAMETER iRoleAcct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE mFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctRole AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mTime AS INT64 NO-UNDO.
DEFINE VARIABLE mTime1 AS INT64 NO-UNDO.
DEFINE VARIABLE mAcctYes AS LOGICAL NO-UNDO.
DEFINE VARIABLE mListAcct AS CHAR NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER person FOR person.
DEFINE BUFFER cust-corp FOR cust-corp.

DEFINE STREAM sout.

IF iRoleAcct EQ ? THEN 
	mAcctRole = "Šà¥¤¨â,Šà¥¤’,Šà¥¤’‚".
ELSE
	mAcctRole = iRoleAcct.

{getdates.i}

/* beg-date = end-date. */

{tmprecid.def}

mTime = ETIME(yes).

FOR EACH tmprecid,
    FIRST loan
    WHERE
        RECID(loan) EQ tmprecid.id
    NO-LOCK:
    mListAcct = ''.
    FIND FIRST cust-corp 
    	WHERE cust-corp.cust-id EQ loan.cust-id
    NO-LOCK NO-ERROR.
    IF AVAIL cust-corp THEN
    mFile = TRIM(REPLACE(loan.cont-code, " ", "_")) + "_" + TRIM(REPLACE(REPLACE(cust-corp.name-corp, " ", "_"), '"', '')) + ".csv".
    ELSE
    DO:
       FIND FIRST banks WHERE banks.bank-id = loan.cust-id NO-LOCK NO-ERROR.
       IF AVAIL banks THEN
       ASSIGN
          mFile = TRIM(REPLACE(loan.cont-code, " ", "_")) + "_" + TRIM(REPLACE(REPLACE(banks.name, " ", "_"), '"', '')) + ".csv".
    END.
    mFile = REPLACE(REPLACE(REPLACE(mFile,"(","_"),")","_"),"/","-").

    FIND FIRST person 
    	WHERE person.person-id EQ loan.cust-id
    NO-LOCK NO-ERROR.

    mFile = TRIM(ENTRY(1, loan.cont-code, "@")) + "_" + TRIM(person.name-last) + ".csv".

	OUTPUT 
	TO VALUE(mFile) CONVERT TARGET "1251".

	DO mI = 1 TO NUM-ENTRIES(mAcctRole):
		FOR EACH loan-acct
			WHERE loan-acct.contract EQ loan.contract
/*			AND ENTRY(1, loan-acct.cont-code, "@") = loan.doc-ref */
                        AND (loan-acct.cont-code = loan.cont-code or loan-acct.cont-code = (loan.doc-ref + '@0400'))

/*			AND loan-acct.acct-type EQ ENTRY(mI, mAcctRole) */
			AND IF R-INDEX(ENTRY(mI, mAcctRole), "*") GT 0 THEN loan-acct.acct-type BEGINS SUBSTRING(ENTRY(mI, mAcctRole), 1, LENGTH(ENTRY(mI, mAcctRole)) - 1) ELSE loan-acct.acct-type EQ ENTRY(mI, mAcctRole)
			NO-LOCK BY loan-acct.since DESC:

         IF index(mListAcct,TRIM(ENTRY(1, loan-acct.acct, "@"))) = 0 THEN DO:
         mListAcct = mListAcct + TRIM(ENTRY(1, loan-acct.acct, "@")) + ','.
		DO:
			FIND FIRST acct
				WHERE acct.acct EQ loan-acct.acct
			NO-LOCK NO-ERROR.
			RUN VALUE("vyp_abs.p") (RECID(acct)).
		END.
	END.
end.
   end.

	OUTPUT CLOSE.    
/*
RUN sndbispc ("file=" + mFile + ".csv" + ";class=bq").
*/
END.

mTime = ETIME(yes).

