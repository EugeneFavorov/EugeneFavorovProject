
/*  */
/* kam */



{globals.i}

{setdest.i new}
{tmprecid.def}


def buffer bloan-acct for loan-acct.
def buffer bloan for loan.
def var countloan as int no-undo.
def var strloan as char no-undo.
def new shared stream vvs.
def var fname as char  init "./one408loan.csv"  no-undo.


output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:


     Find last loan-acct where 
          loan-acct.contract = loan.contract
          and loan-acct.cont-code = loan.cont-code
          and loan-acct.acct-type = 'КредРасч'
          and substring(loan-acct.acct,1,5) = '40817'
          no-lock no-error.
        if avail loan-acct then do:
            countloan = 1.
            strloan = trim(entry(1,loan-acct.acct,'@')) + ';' + trim(entry(1,loan-acct.cont-code,'@')) + ';'.
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
                strloan = strloan + trim(entry(1,bloan.cont-code,'@')) + ';'.
	    end.  
	   if countloan > 1 then do:
	       put stream vvs unformatted strloan + chr(13) + chr(10).
	       
             put unformatted strloan skip.
           end.
       end.
end.
      
    {preview.i}
/*
RUN sndbispc ("file=" + fname + ";class=bq").
  */  