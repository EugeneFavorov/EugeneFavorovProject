/* kam */



{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

{setdest.i}

def var firstdate as date no-undo.
def var lastdate as date no-undo.
def var acctBal as decimal no-undo.


lastdate = date("09/01/2017").
firstdate = date("05/01/2017").


for each loan where loan.contract = 'Кредит' and loan.close-date = ? and loan.currency = '' no-lock:
    find last loan-acct of loan where loan-acct.acct-type = 'КредПр' no-lock no-error.
    if avail loan-acct then do:
            find first acct of loan-acct no-lock.
            /* if acct.open-date > firstdate then firstdate = acct.open-date. */ 
            RUN acct-pos IN h_base (loan-acct.acct, '', firstdate, firstdate, ?).
            acctBal = abs(sh-bal).
            if acctBal = 0 then do:
                RUN acct-pos IN h_base (loan-acct.acct, '', lastdate, lastdate, ?).
                acctBal = abs(sh-bal).
                if acctBal > 0 then do:
                    FIND first term-obl where term-obl.contract = 'Кредит' 
                        and term-obl.cont-code = loan.cont-code
                        and term-obl.idnt = 3
                        and term-obl.end-date = lastdate no-lock no-error.
                        if not avail term-obl then do:
                            put unformatted loan.cont-code skip.                            
                        end.
                    END.
                end.
    end.
end.


{preview.i}