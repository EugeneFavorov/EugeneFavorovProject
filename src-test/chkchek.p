{globals.i}
DEFINE INPUT PARAMETER iClass AS character NO-UNDO.
DEFINE INPUT PARAMETER iSurr  AS character NO-UNDO.
DEFINE INPUT PARAMETER iVal   AS character NO-UNDO.

DEFINE VARIABLE mErrMess     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL   NO-UNDO.

IF 
   INDEX(iVal,".")   NE 3  
OR INDEX(iVal,".",4) NE 6
OR INDEX(iVal,",")   NE 11
OR INDEX(iVal,":")   NE 14
OR INDEX(iVal,"-")   NE 22
THEN
mErrMess = "‡­ ç¥­¨¥ " + iVal + " ­¥ á®®â¢¥âáâ¢ã¥â ä®à¬ âã:~n" +
   "„„.ŒŒ.ƒƒƒƒ;••:•••••••-•••••••".

RETURN mErrMess.





