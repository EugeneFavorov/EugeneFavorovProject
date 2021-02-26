
/*  */
/* kam */



{globals.i}

{setdest.i new}


def buffer bloan-acct for loan-acct.
def buffer bloan for loan.
def var countloan as int no-undo.
def var strloan as char no-undo.

     FOR EACH loan-acct where 
          loan-acct.contract = 'Кредит'
          and loan-acct.acct-type = 'КредРасч'
          and substring(loan-acct.acct,1,5) = '40817'
          no-lock, first loan where
          loan.contract = loan-acct.contract and
          loan.cont-code = loan-acct.cont-code and
          loan.filial-id <> '0400' and
          loan.close-date = ?
          no-lock:
            countloan = 1.
            strloan = loan-acct.acct + ';' + loan-acct.cont-code + ';'.
            FOR EACH bloan-acct where
            bloan-acct.contract = 'Кредит'
            and bloan-acct.acct-type = 'КредРасч'
            and bloan-acct.cont-code <> loan-acct.cont-code
            and bloan-acct.acct = loan-acct.acct
            no-lock, first bloan where
            bloan.contract = bloan-acct.contract and
            bloan.cont-code = bloan-acct.cont-code and
            bloan.filial-id <> '0400' and
            bloan.close-date = ?
            no-lock:
                countloan = countloan + 1.
                strloan = strloan + bloan.cont-code + ';'.
		    end.  
	   if countloan > 1 then do:
             put unformatted strloan skip.
       end.
    end.
      
    {preview.i}
