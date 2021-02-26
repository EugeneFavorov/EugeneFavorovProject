{globals.i}

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :

    IF NOT CONNECTED("bank")
     THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

    RUN imp-eldoc2.p.
    CATCH eAnyError AS Progress.Lang.Error:
	MESSAGE RETURN-VALUE + " " + eAnyError:GetMessage(1) VIEW-AS ALERT-BOX.
    END CATCH.
    FINALLY:
	IF CONNECTED("bank") THEN DISCONNECT bank.
	IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
    END FINALLY.
END. /* DO */

