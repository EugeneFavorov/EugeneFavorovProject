/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: 
      Comment: 
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/



{globals.i}
{getdate.i}
{sh-defs.i}
{ksh-defs.i NEW}
{tmprecid.def}
{intrface.get xclass}
  FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id EXCLUSIVE-LOCK:

    for each loan-acct of loan where loan-acct.acct-type <> 'КредРасч' 
	AND loan-acct.acct-type <> 'КредРасч1' :
        find first acct of loan-acct.
        if avail acct and acct.cust-cat = loan.cust-cat and acct.cust-id = loan.cust-id then do:
		RUN acct-pos IN h_base (acct.acct, '', end-date, end-date, ?).
		IF abs(sh-bal) = 0 THEN DO:
			acct.close-date = end-date.
		END.
	end.
    end.
    loan.close-date = end-date.
    loan.loan-status = 'ЗАКР'.
  END.
{intrface.del}
    
    
    
















