/*
   Банковская интегрированная система БИСквит
   Copyright: 
*/

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
   
   IF NOT CONNECTED("bank")
   THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   
   IF NOT CONNECTED("bank") 
   THEN MESSAGE "Нет соединения." VIEW-AS ALERT-BOX.
   ELSE RUN comp-cyber.p.

   FINALLY:
      IF CONNECTED("bank")   THEN DISCONNECT bank.
      IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
   END FINALLY.
END.

RETURN.
