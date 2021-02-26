/*
Банковская интегрированная система БИСквит
*/
DEFINE VARIABLE mResult AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOK     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mErrMsg AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
   IF NOT CONNECTED("bank")
   THEN   CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   IF NOT CONNECTED("bank") THEN 
   DO:
      MESSAGE "Нет соединения c bank".
   END.
   ELSE
   DO:
      RUN rep-61011-run.p.
   END.

   FINALLY:
      IF CONNECTED("bank")   THEN DISCONNECT bank.
      IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
   END FINALLY.
END.

RETURN.
