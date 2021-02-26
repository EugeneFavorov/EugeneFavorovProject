{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}
{intrface.get dps}
{intrface.get dpspr}

{sh-defs.i}
{dpsproc.def}
/*
{getdates.i}
 */
for each tmprecid,
    first loan where
    recid(loan) eq tmprecid.id
    EXCLUSIVE-LOCK:
    ASSIGN
    loan.loan-status = 'û'.
    end.