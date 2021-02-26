{globals.i}
DEF INPUT PARAM iCont-code AS CHAR NO-UNDO.

DEFINE NEW GLOBAL SHARED temp-table debtkr no-undo    /* операции по параметрам до даты возникновения просрочки */
  field id 		as int64
  field debt-date 	as date
  field acct 		as char
  FIELD p0		AS DEC
  FIELD p7		AS DEC
  FIELD p10		AS DEC
  FIELD p48		AS DEC
  FIELD p109		AS DEC
  FIELD p173		AS DEC
  FIELD p209		AS DEC
  FIELD p210		AS DEC
  FIELD p229		AS DEC
  FIELD p248		AS DEC
  FIELD p301		AS DEC
  FIELD debt-sum	AS DEC
  FIELD cont-code	AS CHAR
  FIELD filial		AS CHAR
 .

FOR EACH debtkr where debtkr.cont-code EQ iCont-code :
	
	RUN SetSysConf IN h_base ("погашпродкред",STRING(debtkr.debt-sum)).
	RUN SetSysConf IN h_base ("счетпродкред",STRING(debtkr.acct)).
MESSAGE debtkr.debt-sum " " debtkr.acct VIEW-AS ALERT-BOX.

END.
/*
FOR EACH debtkr:
   MESSAGE debtkr.cont-code " " debtkr.debt-sum VIEW-AS ALERT-BOX.
END.
*/