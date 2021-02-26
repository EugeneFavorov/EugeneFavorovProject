{globals.i}


{tmprecid.def}


{setdest.i} 
 
for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:

FOR EACH loan-int WHERE 
    loan-int.cont-code = loan.cont-code
    and loan-int.contract = 'Кредит' and loan-int.id-k = 48
    and loan-int.id-d = ? :
    
    loan-int.id-d =5.	

end.

put unformatted loan.doc-ref skip.
end.

