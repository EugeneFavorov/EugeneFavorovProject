/*
ОАО "Плюс Банк"
*/

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE:
   IF NOT CONNECTED("bank")
      THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   IF NOT CONNECTED("bank") THEN
   DO:
      MESSAGE "Не удалось подключиться к bank."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   RUN rep-notary.p.
   CATCH eAnyError AS Progress.Lang.Error:
      PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
   END CATCH.
   FINALLY:
   	IF CONNECTED("bank")   THEN DISCONNECT bank.
   	IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
   END FINALLY.
END.

RETURN.
