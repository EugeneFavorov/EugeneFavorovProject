/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: getomskcyber.p
      Comment: Загружаем заготовки смартфлоу
   Parameters:  
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

{globals.i}
{intrface.get xclass}

{ksh-defs.i}

DEF INPUT PARAM iForm AS CHAR.

DEF VAR retHandle AS CHAR NO-UNDO.
DEFINE VARIABLE shFilial-defore AS CHARACTER NO-UNDO.

shFilial-defore = shFilial.
shFilial = "0500".
   
DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
   IF NOT CONNECTED("bank")
      THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   IF NOT CONNECTED("bank") THEN 
   DO:
      MESSAGE "нет соединения" VIEW-AS ALERT-BOX.
      shFilial = shFilial-defore.
      RETURN.
   END.

   RUN getomskcashin2.p(INPUT iForm, OUTPUT retHandle).

   shFilial = shFilial-defore.

   FINALLY:
   IF iForm NE 'YES' THEN DO:
       IF CONNECTED("bank") THEN DISCONNECT bank.
       IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
       shFilial = shFilial-defore.
   END.
   END FINALLY.
END.

RETURN retHandle.
