/* kam */



{globals.i}

{tmprecid.def}
{setdest.i}

def var i  as int no-undo init 0.

for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:

/*	find last loan-acct of loan where loan-acct.acct-type = 'КредРасч' NO-lock NO-ERROR. */
for each loan-acct of loan no-lock:
	IF AVAIL loan-acct then do:
		find first acct of loan-acct no-error.
		if avail acct and acct.close-date = date("07/08/2017") then do:
			i = i + 1.			
			acct.close-date = ?.  
			put unformatted string(i) + ',' + acct.acct skip.
		end.

	end.

end.

END.

{preview.i}