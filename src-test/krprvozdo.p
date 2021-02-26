{globals.i}

DEF INPUT PARAM iCont-code AS CHAR NO-UNDO.



DEFINE NEW GLOBAL SHARED temp-table ttentry no-undo    /* операции по параметрам до даты возникновения просрочки */
  field acct 		as char
  FIELD amt-rub		AS DEC
  FIELD cont-code	AS CHAR
 .


FIND FIRST loan-acct where loan-acct.cont-code EQ iCont-code
			AND (loan-acct.acct BEGINS '40817'
				or loan-acct.acct BEGINS '40820') NO-LOCK NO-ERROR.

/*MESSAGE loan-acct.acct VIEW-AS ALERT-BOX.*/
FIND FIRST ttentry WHERE ttentry.acct EQ loan-acct.acct AND ttentry.cont-code EQ iCont-code NO-LOCK NO-ERROR.
IF AVAIL ttentry THEN DO:
MESSAGE ttentry.amt-rub VIEW-AS ALERT-BOX.
	RUN SetSysConf IN h_base ("оплпродкред",STRING(ttentry.amt-rub)).
	RUN SetSysConf IN h_base ("счетпродкред",STRING(ttentry.acct)).
END.










/*

FOR EACH ttentry where ttentry.acct EQ loan-acct.acct :

	RUN SetSysConf IN h_base ("оплпродкред",STRING(ttentry.amt-rub)).
	RUN SetSysConf IN h_base ("счетпродкред",STRING(ttentry.acct)).
END.
*/