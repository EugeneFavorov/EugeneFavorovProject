/* kam */



{globals.i}
{sh-defs.i}

{intrface.get olap}
{intrface.get loan}
{intrface.get pint}
{tmprecid.def}

end-date = today.
{getdate.i
   &DateLabel = "Дата прощения"
} 

FUNCTION fu-par returns decimal (
    BUFFER iloan FOR loan,
    INPUT vSince AS DATE,
    INPUT vParm AS INT):

    DEF VAR vAmt AS DEC NO-UNDO.
    DEF VAR vAmt2 AS DEC NO-UNDO.
    DEF VAR vAmt3 AS DEC NO-UNDO.
    DEF VAR vDbSumDec AS DEC NO-UNDO.
    DEF VAR vCrSumDec AS DEC NO-UNDO.
    DEF VAR vTmpCurr        AS CHAR   NO-UNDO.

    run STNDRT_PARAM in h_loan (
	iloan.contract, iloan.cont-code, vParm,
	IF iloan.since >= vSince THEN vSince ELSE iloan.since,
                              output vAmt, output vDbSumDec, output vCrSumDec).
    RUN inter_current(BUFFER iloan, vParm, OUTPUT vAmt2).
    vAmt = vAmt + vAmt2.

    IF iloan.since < vSince THEN DO:
	/* надо добавить операции за будущие дни */
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-d = vParm NO-LOCK:
	      vAmt = vAmt + loan-int.amt-rub.
	  END.
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-k = vParm NO-LOCK:
	      vAmt = vAmt - loan-int.amt-rub.
	  END.
    END.
    RETURN vAmt.
END.


for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:

    RUN createl(16,loan.contract,loan.cont-code).
    RUN createl(4,loan.contract,loan.cont-code).
	RUN createl(22,loan.contract,loan.cont-code).
	RUN createl(32,loan.contract,loan.cont-code).
	RUN createl(88,loan.contract,loan.cont-code).
	RUN createl(109,loan.contract,loan.cont-code).
	RUN createl(173,loan.contract,loan.cont-code).

END.


PROCEDURE createl:

DEF INPUT PARAM iPar      AS INT64    NO-UNDO. 
DEF INPUT PARAM iContract AS CHAR NO-UNDO.
DEF INPUT PARAM iContCode AS CHAR NO-UNDO.

def buffer bli for loan-int.
def var ddd as decimal no-undo.

     ddd = fu-par(BUFFER loan,today,iPar).
     IF ddd > 0 THEN DO:
      FIND FIRST chowhe WHERE 
                 chowhe.id-op EQ iD-op
      NO-LOCK NO-ERROR.

        find last bli where bli.cont-code = iContCode
        and bli.mdate = end-date
        no-lock no-error.

	CREATE loan-int.
	ASSIGN loan-int.op = -2
	       loan-int.nn = (IF AVAIL bLI
               THEN bLI.nn + 1
               ELSE 1)
	       loan-int.op-entry = -1
               loan-int.contract = iContract
               loan-int.cont-code = iContCode
               loan-int.id-d = 5
               loan-int.id-k = iPar
               loan-int.op-date = end-date
	       loan-int.mdate = end-date
	       loan-int.amt-rub = ddd
	       loan-int.user-id = USERID("bisquit")
	.
	RELEASE loan-int.
     END.	

     IF ddd < 0 AND (iPar = 88 or iPar = 22 or iPar = 32) THEN DO:
      FIND FIRST chowhe WHERE 
                 chowhe.id-op EQ iD-op
      NO-LOCK NO-ERROR.

        find last bli where bli.cont-code = iContCode
        and bli.mdate = end-date
        no-lock no-error.

	CREATE loan-int.
	ASSIGN loan-int.op = -2
	       loan-int.nn = (IF AVAIL bLI
               THEN bLI.nn + 1
               ELSE 1)
	       loan-int.op-entry = -1
               loan-int.contract = iContract
               loan-int.cont-code = iContCode
               loan-int.id-d = iPar
               loan-int.id-k = 5
               loan-int.op-date = end-date
	       loan-int.mdate = end-date
	       loan-int.amt-rub = ABS(ddd)
	       loan-int.user-id = USERID("bisquit")
	.
	RELEASE loan-int.
     END.	


END PROCEDURE.

