/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: get350.p
      Comment: Загружаем заготовки 350
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/


DEF INPUT PARAM iForm AS CHAR.

/*DEFINE VARIABLE iForm AS CHARACTER NO-UNDO.*/
/*iForm = 'YES'.*/

DEF VAR retHandle AS CHAR NO-UNDO.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
    IF NOT CONNECTED("bank")
     THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
     IF NOT CONNECTED("bank") THEn message "нет соединения" view-as alert-box.


   RUN get350-2.p(INPUT iForm, OUTPUT retHandle).

   FINALLY:
      IF iForm NE 'YES' THEN
      DO:
          IF CONNECTED("bank") THEN DISCONNECT bank.
          IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
      END.
   END FINALLY.
END.

RETURN retHandle.
