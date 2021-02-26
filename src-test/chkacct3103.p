{globals.i}
{sh-defs.i}

/*DEF VAR ost AS NO-UNDO.*/

DEF NEW SHARED STREAM vvs.

OUTPUT STREAM vvs TO VALUE ('./acct.csv')
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

PUT STREAM vvs UNFORMATTED 
		'Счет;Наименование;Остаток;Кредитный;\r\n' 
	.

FOR EACH acct WHERE (acct.bal-acct EQ 40817
	          		 OR
	          		 acct.bal-acct EQ 40820
	          		 OR
	          		 acct.bal-acct EQ 42301
	          		 OR
	          		 acct.bal-acct EQ 42601
	          		 )
			  AND    acct.open-date LE 01/01/2014
			  AND    acct.close-date EQ ?
			  AND    acct.filial-id EQ '0400'
	NO-LOCK:

/*	FOR FIRST op-entry WHERE ((NOT (op-entry.acct-db MATCHES '47411*')) AND
							   (NOT (op-entry.acct-db MATCHES '70606*')) AND
							    op-entry.acct-cr EQ acct.acct)
					    OR   ((NOT (op-entry.acct-cr MATCHES '70601*')) AND
					    	   op-entry.acct-db EQ acct.acct)
					  AND op-entry.op-date LE 12/31/2014
					  AND op-entry.op-date GE 01/01/2014
					  AND op-entry.filial-id EQ '0400'
		NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
		LEAVE.
	END. 
*/
	FOR FIRST op-entry WHERE ((NOT (op-entry.acct-db MATCHES '47411*')) AND
							   (NOT (op-entry.acct-db MATCHES '70606*')) AND
							    op-entry.acct-cr EQ acct.acct)
					  AND op-entry.op-date LE 12/31/2014
					  AND op-entry.op-date GE 01/01/2014
					  AND op-entry.filial-id EQ '0400'
		NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
		LEAVE.
	END. 
	IF AVAIL(op-entry) THEN NEXT.

	FOR FIRST op-entry WHERE ((NOT (op-entry.acct-cr MATCHES '70601*')) AND
					    	   op-entry.acct-db EQ acct.acct)
					  AND op-entry.op-date LE 12/31/2014
					  AND op-entry.op-date GE 01/01/2014
					  AND op-entry.filial-id EQ '0400'
		NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
		LEAVE.
	END. 
	IF AVAIL(op-entry) THEN NEXT.


	IF NOT AVAIL(op-entry) THEN DO:

		FIND FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
		
		IF acct.bal-acct EQ 42301 OR acct.bal-acct EQ 42601 THEN
			FIND FIRST loan-acct WHERE loan-acct.acct EQ acct.acct 
								 AND   loan-acct.contract EQ 'Кредит'
				USE-INDEX acct
				NO-LOCK NO-ERROR.

		RUN acct-pos IN h_base (acct.acct,acct.currency,TODAY,TODAY,?).
		IF sh-bal < 0 THEN
		PUT STREAM vvs UNFORMATTED 
		 '`'	+ ENTRY(1,acct.acct,' ') + ';' + person.name-last + ' ' + person.first-names + ';' + STRING(ABS(sh-bal)) + ';' 
		 + (IF AVAIL(loan-acct) THEN 'КРЕДИТ' ELSE '') + ';\r\n' 
		.

	END.

END.

OUTPUT STREAM vvs CLOSE.

RUN sndbispc ("file=./acct.csv;class=bq").
