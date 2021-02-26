/*
               ОАО "Плюс Банк"
    Copyright: 
     Filename: kredprodtranz.p
      Comment: Ищет сумму которую следует списать с клиента по договору
   Parameters: 
         Uses:
      Used by:
      Created: kau
     Modified: 
*/



DEF INPUT PARAM iParam AS DATE NO-UNDO.

DEF VAR iDate AS DATE NO-UNDO.

iDate = iParam.

/*MESSAGE iDate VIEW-AS ALERT-BOX.*/



IF NOT CONNECTED("bank") THEN DO:
    CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf") NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Не удалось соединиться с базой BANK" VIEW-AS ALERT-BOX.
        RETURN.


/*
IF NOT CONNECTED("developer") THEN DO:
    CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf") NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Не удалось соединиться с базой BANK" VIEW-AS ALERT-BOX.
        RETURN.

*/
    END.
END.

RUN kredprodtranz_.p (iDate).

IF CONNECTED("bank") THEN
    DISCONNECT bank.
IF CONNECTED("bismfr") THEN
    DISCONNECT bismfr.
    
/*   
IF CONNECTED("developer") THEN
    DISCONNECT bank.
IF CONNECTED("bismfr") THEN
    DISCONNECT bismfr.
*/
    
















