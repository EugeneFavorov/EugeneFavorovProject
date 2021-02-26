{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get dps}
{intrface.get dpspr}

{setdest.i}
{sh-defs.i}
{dpsproc.def}

def var myOst as dec no-undo.

for each tmprecid,
    first acct where
            recid(acct) eq tmprecid.id
            and acct.close-date EQ ?
                no-lock:
FIND FIRST loan-acct where
loan-acct.acct EQ acct.acct
no-lock NO-ERROR.
IF AVAIL(loan-acct) THEN NEXT.
 
RUN acct-pos IN h_base (acct.acct,
                              acct.currency,
                              today,
                              today,
                              "√").
IF acct.currency = '' THEN
        myOst = abs(sh-bal).
ELSE myOst = abs(sh-val).
PUT UNFORMATTED acct.acct '    ' acct.open-date '    ' trim(string(myOst, "->>>>>>>>>9.99"))skip.
END.
{preview.i}
{intrface.del}