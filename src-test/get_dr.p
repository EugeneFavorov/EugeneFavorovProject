/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: U:\GUVA\RVPS\KOEF_REZ.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 28.03.2012 11:47 gva     
     Modified: 28.03.2012 17:50 gva      
     Modified: 28.11.2012 16:07 gva      
     Modified: 28.11.2012 16:07 gva      
     Modified: 28.11.2012 16:26 gva      
     Modified: 28.11.2012 16:06 gva      
     Modified: 28.11.2012 16:27 gva      
     Modified: 
*/

   
{globals.i}
DEFINE INPUT  PARAMETER ipFile AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipSurrogate AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipCode AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipCont-type AS CHARACTER  NO-UNDO.

DEFINE SHARED VARIABLE pick-value AS CHARACTER INIT "" NO-UNDO.
DEFINE SHARED VARIABLE vCont-type AS CHARACTER INIT "" NO-UNDO.

FIND FIRST loan
    WHERE loan.contract EQ ENTRY(1, ipSurrogate, "|")
    AND loan.cont-code EQ ENTRY(2, ipSurrogate, "|")
    AND loan.filial-id EQ shFilial
     NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
    IF CAN-DO(replace(ipCont-type,"|",","), loan.cont-type) THEN DO:
        pick-value = GetXAttrValueEx(
		ipFile,
		replace(ipSurrogate,"|",","),
		ipCode,
		"").
    END.

END.

RETURN pick-value.

