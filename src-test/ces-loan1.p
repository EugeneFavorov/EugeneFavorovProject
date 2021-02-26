{globals.i} 
DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNumTag AS CHARACTER NO-UNDO.

mNumTag = TRIM(STRING(iTag)).

{getdate.i}
{tmprecid.def}
OUTPUT TO VALUE("tag" + mNumTag + ".txt") CONVERT TARGET "1251".

FOR EACH tmprecid: 
    RUN VALUE("tag" + mNumTag + ".p") (tmprecid.id).
END.
OUTPUT CLOSE.
