/* DEFINE INPUT PARAMETER in-op-date   LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER in-rec-kind  AS   RECID      NO-UNDO.
*/

def var in-beg-date as date no-undo.
def var in-end-date as date no-undo.
{globals.i}


{getdates.i}
output to "status.txt".

assign in-beg-date = beg-date
       in-end-date = end-date.
for each history where modif-date >= beg-date 
                   and modif-date <= end-date
                   and file-name = "op"
                   and field-name = "op.op-status"
                   no-lock.
   export history.
end. 

output close.


