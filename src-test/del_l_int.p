/* 㤠�塞 l-int servsouz */

{globals.i}

{tmprecid.def}

{getdates.i}

{setdest.i} 
 
for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
       for each loan-int of loan where
        loan-int.mdate >= beg-date
        and loan-int.mdate <= end-date
        and loan-int.op        = -2
        and loan-int.op-entry  = -1
        and loan-int.avt       = FALSE
        and loan-int.user-id   = 'SERVSOUZ':
            put unformatted loan-int.cont-code + '     ' + string(loan-int.amt-rub) skip.    
            delete loan-int.
        end.
end.
{preview.i} 


   
