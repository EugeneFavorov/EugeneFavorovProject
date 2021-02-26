{globals.i}

{tmprecid.def}

{getdate.i}
{setdest.i}

FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    find first loan-int where loan-int.contract = loan.contract and
	loan-int.cont-code = loan.cont-code and
	loan-int.mdate = end-date and
	loan-int.id-k = 6 and
	loan-int.id-d = 5 exclusive-lock no-error.
	if avail loan-int then do:
		delete loan-int.
		put unformatted string(loan.cont-code) skip.
	end.
end.

			   
{preview.i}

