{globals.i}

/*
IF CONNECTED("bank")
THEN
DO:
   MESSAGE 
   "Connected"
   VIEW-AS ALERT-BOX.
   RETURN.
END.
ELSE
   MESSAGE 
   "Not Connected"
   VIEW-AS ALERT-BOX.
*/

IF CONNECTED("bank") THEN
DO:
END.
ELSE
DO:
   CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
END.


/*
IF CONNECTED("bank")
THEN
   MESSAGE 
   "Connected"
   VIEW-AS ALERT-BOX.
ELSE
   MESSAGE 
   "NOT Connected"
   VIEW-AS ALERT-BOX.
RETURN.
*/