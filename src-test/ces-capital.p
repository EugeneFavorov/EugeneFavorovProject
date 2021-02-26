{globals.i} 
DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNumTag AS CHARACTER NO-UNDO.

def var i as int no-undo init 0.

mNumTag = TRIM(STRING(iTag)).

{tmprecid.def}
OUTPUT TO VALUE("CRED_" + mNumTag + ".txt") CONVERT TARGET "1251".

FOR EACH tmprecid: 
    RUN VALUE("cred_" + mNumTag + ".p") (tmprecid.id,i). 
    i = i + 1.
END.
OUTPUT CLOSE.
