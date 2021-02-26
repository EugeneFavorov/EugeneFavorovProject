
DEFINE INPUT  PARAMETER iLevel    AS INT64    NO-UNDO.

DEFINE VARIABLE mRet AS CHARACTER NO-UNDO.

{globals.i}
{ttretval.def}
{empty ttRetVal}

RUN browseld.p ("code",  
                "class" + CHR(1) + "parent" + CHR(1) + "ActionLock" + CHR(1) +
                "RetRcp" + CHR(1) + "RetType" + CHR(1) + "retFld",

                
                "УрРиска" + CHR(1) + "УрРиска" + CHR(1) + "Enter,Del,F3,Ins" + CHR(1) +
                STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1) +
                "Multi" + CHR(1) + "code",

               
                 ?,         
                iLevel).      

pick-value = "".
IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN
DO:
                       
   FOR EACH ttRetVal:
       pick-value = pick-value  + (if pick-value ne "" then ";" else "") + ttRetVal.PickValue.
    
   END.
   
  /* If (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","") MATCHES "низкий") or (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","") eq "") then
        UpdateTempSignsEx('cust-corp',STRING(cust-corp.cust-id),"РискОтмыв",Today,"Высокий",?).
   */
END.
