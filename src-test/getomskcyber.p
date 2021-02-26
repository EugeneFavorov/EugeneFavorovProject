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


DEF INPUT PARAM iForm AS CHAR.

DEF VAR retHandle AS CHAR NO-UNDO.

/*IF iForm = 'YES' THEN DO:  */  

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
    IF NOT CONNECTED("bank")
     THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
     IF NOT CONNECTED("bank") THEn message "нет соединения" view-as alert-box.
/* END. */


    RUN getomskcyber2.p(INPUT iForm, OUTPUT retHandle).

FINALLY:
IF iForm NE 'YES' THEN DO:
    IF CONNECTED("bank") THEN DISCONNECT bank.
    IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
END.
END FINALLY.
END.

RETURN retHandle.
