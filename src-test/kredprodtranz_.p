/*DEF INPUT PARAM iAcct AS CHAR NO-UNDO.*/
DEF INPUT PARAM iDate AS DATE NO-UNDO.



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


{empty debtkr}

FOR EACH bank.debts WHERE 
	bank.debts.debt_date EQ iDate
SHARE-LOCK:     
 MYNEXT: 
  DO:
CREATE debtkr.
ASSIGN	debtkr.id 	= bank.debts.id
	debtkr.debt-date = bank.debts.debt_date
	debtkr.acct	= bank.debts.acct
	debtkr.p0	= bank.debts.p0
	debtkr.p7	= bank.debts.p7
	debtkr.p10	= bank.debts.p10
	debtkr.p48	= bank.debts.p48
	debtkr.p109	= bank.debts.p109
	debtkr.p173	= bank.debts.p173
	debtkr.p209	= bank.debts.p209
	debtkr.p210	= bank.debts.p210
	debtkr.p229	= bank.debts.p229
	debtkr.p248	= bank.debts.p248
	debtkr.p301	= bank.debts.p301
	debtkr.debt-sum = bank.debts.debt_sum
	debtkr.cont-code = bank.debts.cont_code
	debtkr.filial	= bank.debts.filial
	.
  END.
 END.
/*
FOR EACH debtkr :
    MESSAGE debtkr.acct " " debtkr.debt-sum VIEW-AS ALERT-BOX.
END.
*/