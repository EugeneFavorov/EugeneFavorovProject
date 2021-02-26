{globals.i} 
DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.

{getdate.i}
{tmprecid.def}
message ENTRY(1, iTag) skip ENTRY(2, iTag) view-as alert-box.
OUTPUT TO VALUE(ENTRY(1, iTag) + ".txt") CONVERT TARGET "1251".
mI = 1.
FOR EACH tmprecid: 
    RUN VALUE(ENTRY(2, iTag) + ".p") (tmprecid.id, mI).
    mI = mI + 1.
END.
OUTPUT CLOSE.
