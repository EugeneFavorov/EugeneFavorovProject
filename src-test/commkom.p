/*
kam
*/


{globals.i}

{setdest.i}

DEF INPUT PARAM in-op-date AS DATE  NO-UNDO. /* Дата опер. дня */
DEF INPUT PARAM oprid      AS RECID NO-UNDO. /* Recid op-kind */
def buffer bli for loan-int.
def buffer bli2 for loan-int.

for each comm-rate where comm-rate.COMMISSION = 'КомСч'
and comm-rate.rate-fixed = YES
and comm-rate.filial-id = '0500' no-lock:
    if num-entries(comm-rate.kau) > 1 then do:
        find first loan where loan.contract = ENTRY(1,comm-rate.kau) 
		and loan.cont-code = ENTRY(2,comm-rate.kau)  no-lock no-error.
	if avail loan and loan.close-date = ? THEN DO:
	    find first loan-int where loan-int.cont-code = loan.cont-code
		and loan-int.contract = loan.contract
		and loan-int.id-d = 96
        and loan-int.id-k = 5
		and loan-int.mdate = in-op-date no-lock no-error.
		if not avail loan-int then do:
		        find last bli where bli.cont-code = loan.Cont-Code
		        and bli.contract = loan.contract
		        and bli.mdate = in-op-date
		        no-lock no-error.

			CREATE loan-int.
			     /*
                ASSIGN loan-int.nn = 5 (IF AVAIL bLI2
                       THEN bLI2.nn + 1
                       ELSE 1)
                */
				ASSIGN loan-int.nn = 4
			       loan-int.op = -2
			       loan-int.op-entry = -1
		               loan-int.contract = loan.Contract
		               loan-int.cont-code = loan.Cont-Code
		               loan-int.id-d = 96
		               loan-int.id-k = 5
		               loan-int.op-date = in-op-date
			       loan-int.mdate = in-op-date
			       loan-int.amt-rub = comm-rate.rate-comm
			       loan-int.user-id = USERID("bisquit")
			.
			    
			    find last bli2 where bli2.cont-code = loan.Cont-Code
                and bli2.contract = loan.contract
                and bli2.mdate = in-op-date
                no-lock no-error.
			CREATE loan-int.
			    /*
                ASSIGN loan-int.nn = 5 (IF AVAIL bLI2
                       THEN bLI2.nn + 1
                       ELSE 1)
                */
				   ASSIGN 
				    loan-int.nn = 5 
			       loan-int.op = -2
			       loan-int.op-entry = -1
		               loan-int.contract = loan.Contract
		               loan-int.cont-code = loan.Cont-Code
		               loan-int.id-d = 373
		               loan-int.id-k = 96
		               loan-int.op-date = in-op-date
			       loan-int.mdate = in-op-date
			       loan-int.amt-rub = comm-rate.rate-comm
			       loan-int.user-id = USERID("bisquit")
			.
			END.
	   END.
	   put unformatted loan.cont-code + ' ' + string(comm-rate.rate-comm) skip.
    end.
end.

{preview.i}
