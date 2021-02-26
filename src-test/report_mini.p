/*
               ОАО "Плюс Банк"
    Copyright: 
     Filename: report_mini.p
      Comment: Данные о состоянии КД на указанную дату
   Parameters: 
         Uses:
      Used by:
      Created: vvv
     Modified: 
*/
{globals.i}

/* спросим дату */
{getdate.i}

IF end-date >= TODAY THEN
	DO:
		/**/
		MESSAGE "Расчет задолженности возможен на дату не позднее " + STRING(TODAY, "99/99/9999") VIEW-AS ALERT-BOX TITLE " Ошибка ".
		UNDO, RETRY.	
		/**/
	END.

IF NOT CONNECTED("bank") THEN
	DO:
	{spinner.i "Соединение..."}
    CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf") NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Не удалось соединиться с базой BANK" VIEW-AS ALERT-BOX.
        RETURN.
    END.
END.

	/* для выделенных КД  находим инфу */
	RUN mini_com.p (end-date).


IF CONNECTED("bank") THEN
    DISCONNECT bank.
IF CONNECTED("bismfr") THEN
    DISCONNECT bismfr.

    
















