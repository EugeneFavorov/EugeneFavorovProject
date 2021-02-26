{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get dps}
{intrface.get dpspr}

{setdest.i}
{sh-defs.i}
{dpsproc.def}

for each tmprecid,
    first acct where
            recid(acct) eq tmprecid.id
            and acct.close-date EQ ?
                no-lock:
FIND FIRST loan-acct where
loan-acct.acct EQ acct.acct
and loan-acct.acct-type EQ 'loan-dps-p'
no-lock NO-ERROR.
IF AVAIL(loan-acct) THEN NEXT.
 
PUT UNFORMATTED acct.acct ' ' acct.open-date skip.
END.
{preview.i}
{intrface.del}