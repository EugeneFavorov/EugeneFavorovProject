
{globals.i}

{intrface.get xclass}


{getdate.i}


{setdest.i}

{tmprecid.def}



DEF VAR tmpdate AS DATE NO-UNDO.


FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    FIND LAST loan-acct OF loan
        WHERE loan-acct.acct-type = "КредРасч" NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
       find last acct where acct.acct = loan-acct.acct no-lock no-error.

       if avail acct then do:

	UpdateSigns('acct', acct.acct + ',', "DateMessNord", string(end-date,"99.99.9999"), ?) .
        PUT UNFORMATTED acct.acct SKIP.
       end.




    END.
END.


{preview.i}