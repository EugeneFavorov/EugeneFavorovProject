   {globals.i}
   {xattr-ed.def}
   {intrface.get terr}
   {intrface.get xclass}
   {intrface.get tmess}
DEFINE INPUT PARAMETER iClass AS character NO-UNDO.
DEFINE INPUT PARAMETER iSurr  AS character NO-UNDO.
DEFINE INPUT PARAMETER iVal   AS character NO-UNDO.

DEFINE VARIABLE mUR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDR          AS CHARACTER INIT "¨αªβ¬λΆ" NO-UNDO.


IF {assigned iVal}  THEN 
do: 
    IF (iClass EQ 'person') then
        if  GetXAttrValueEx(iClass,iSurr, "‘γ΅κ¥ªβ","") EQ "”‹" THEN
            mDR  =  "¨αªβ¬λΆ".     
  
    if CAN-DO(",­¨§ª¨©", GetTempXAttrValueEx(iClass,iSurr,  mDR , gend-date,"")) and (GetXAttrValueEx(iClass,iSurr, "ζ¥­ª ¨αª ","") ne iVal) then 
    do:
        FOR EACH Xattr-value where (Xattr-value.code eq mDR)   EXCLUSIVE-LOCK:             
            ASSIGN 
                Xattr-value.code-value = "‚λα®ª¨©".
            UpdateTempSignsEx(iClass,iSurr, mDR ,gend-date ,'‚λα®ª¨©',?).          
        end. 
        if not AVAILABLE Xattr-value then  UpdateTempSignsEx(iClass,iSurr, mDR ,gend-date ,'‚λα®ª¨©',?).      
    end.      
end.
