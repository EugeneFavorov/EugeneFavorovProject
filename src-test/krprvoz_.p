/*DEF INPUT PARAM iAcct AS CHAR NO-UNDO.*/
DEF INPUT PARAM iDate AS DATE NO-UNDO.



DEFINE NEW GLOBAL SHARED temp-table ttentry no-undo    /* операции по параметрам до даты возникновения просрочки */
  field acct 		as char
  FIELD amt-rub		AS DEC
  FIELD cont-code	AS CHAR
 .



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

/*
DEF NEW GLOBAL SHARED TEMP-TABLE top-entry LIKE qbis.op-entry.
DEF NEW GLOBAL SHARED TEMP-TABLE top LIKE qbis.op.
DEF NEW GLOBAL SHARED TEMP-TABLE top-bank LIKE qbis.op-bank.
*/

{empty ttentry}
/*
{empty top-entry}
{empty top}
{empty top-bank}
*/




							/*СЧЕТ ЮРЛИЦА!!!*/ /*СЧЕТ ЮРЛИЦА!!!*/ /*СЧЕТ ЮРЛИЦА!!!*/
FOR EACH qbiswork.op_entry WHERE qbiswork.op_entry.acct_cr BEGINS '40701810804000005136'  AND 
			qbiswork.op_entry.op_date EQ iDate
	 NO-LOCK:

/*Проверяем есть ли счет в таблице мфр дата или же он пришёл по межфилу*/
FOR EACH debtkr :

END.
	FIND FIRST debtkr WHERE SUBSTRING(debtkr.acct,1,20) EQ SUBSTRING(qbiswork.op_entry.acct_db,1,20) NO-LOCK NO-ERROR.
	IF NOT AVAIL debtkr and NOT (qbiswork.op_entry.acct_db BEGINS '30')
	THEN NEXT.
	FIND qbiswork.op WHERE qbiswork.op.op EQ qbiswork.op_entry.op NO-LOCK NO-ERROR.		
	IF qbiswork.op_entry.acct_db BEGINS '30' THEN DO:
		FIND FIRST debtkr WHERE SUBSTRING(debtkr.acct,1,20) EQ substring(qbiswork.op.ben_acct,1,20)
					AND debtkr.debt-sum EQ qbiswork.op_entry.amt_rub.
		IF NOT AVAIL debtkr 
		THEN NEXT.
		ELSE DO:
			find qbis.acct where SUBSTRING(qbis.acct.acct,1,20) EQ SUBSTRING(qbiswork.op.ben_acct,1,20) NO-LOCK NO-ERROR.
			IF NOT AVAIL qbis.acct then DO:
				MESSAGE SUBSTRING(qbiswork.op.ben_acct,1,20) ' СЧЕТ НЕ НАЙДЕН В БАЗЕ! ПРОПУСКАЕМ!' VIEW-AS ALERT-BOX.
				NEXT.
			END.
			CREATE ttentry.
			ASSIGN ttentry.acct = qbis.acct.acct
				ttentry.amt-rub = qbiswork.op_entry.amt_rub
				ttentry.cont-code = debtkr.cont-code
			.
		END. /*Если счет по межфилу и он есть среди таблицы в мфр*/

	END. /*если счет по межфилу*/

	ELSE DO:

		CREATE ttentry.
		ASSIGN ttentry.acct = qbiswork.op_entry.acct_db
			ttentry.amt-rub = qbiswork.op_entry.amt_rub
			ttentry.cont-code = debtkr.cont-code
		.
	END. /* Если не по межфилу а наш бисовский*/







/*			 
	BUFFER-COPY qbiswork.op to top.
	BUFFER-COPY qbiswork.op_entry to top-entry.
	BUFFER-COPY qbiswork.op_bank to top-bank.
*/
END.


/*
FOR EACH qbiswork.op WHERE 
	qbiswork.op.op_kind EQ "k030113" /*'prkrperdo'*/
	AND qbiswork.op.op_date EQ iDate    
SHARE-LOCK:     
 MYNEXT: 
  DO:
	FIND FIRST qbiswork.op_entry where op.op EQ op_entry.op NO-LOCK NO-ERROR.
	CREATE ttentry.
	ASSIGN ttentry.acct = qbiswork.op_entry.acct_db
		ttentry.amt-rub = qbiswork.op_entry.amt_rub
		.
  END.
 END.
*/