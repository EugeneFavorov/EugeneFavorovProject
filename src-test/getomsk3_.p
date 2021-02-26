/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: personomsk.p
      Comment: Ищем клиента в базе BANK и создаем в БИСе
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

DEFINE VAR bAllClients AS LOGICAL NO-UNDO INIT FALSE.
DEFINE VAR iCurrentCount AS INT64 NO-UNDO INIT 0.
DEFINE VAR iCountClients AS INT64 NO-UNDO INIT 0.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE:
    IF NOT CONNECTED("bank")
     THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
    DO ON ERROR UNDO, THROW:
        RUN getomskop2.p(2).
    END.
    FINALLY:
	IF CONNECTED("bank") THEN DISCONNECT bank.
	IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
    END FINALLY.
END. /* DO */

