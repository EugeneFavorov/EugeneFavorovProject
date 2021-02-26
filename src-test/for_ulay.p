{globals.i}
{intrface.get tmess}
{intrface.get xclass}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: данные по КД
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

{intrface.get loan}
{getdate.i}

DEF BUFFER LLL FOR LOAN-ACCT.
DEF VAR TMP_LOAN AS CHAR NO-UNDO.
DEF VAR TOTAL AS INT64 INIT 0 NO-UNDO.
DEF VAR FL AS INT64 INIT 0 NO-UNDO.
DEF VAR POL_SUM AS DEC INIT 0 NO-UNDO.
DEF VAR OTR_SUM AS DEC INIT 0 NO-UNDO.
/**/
DEF VAR par9 		AS DECIMAL NO-UNDO.
DEF VAR a1 		   	AS DECIMAL NO-UNDO.
DEF VAR a2 		   	AS DECIMAL NO-UNDO.
/**/

def temp-table ZZ NO-UNDO
  field CD		AS CHAR
.

def var fname as char  init "./avto_ru.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def new shared stream vvs.

/* идем по кредитам */
{spinner.i "Поиск КД..."}
/**/
FOR EACH LOAN
WHERE LOAN.CONTRACT = "Кредит"
AND LOAN.CLOSE-DATE = ?
/* минус ипотека */
AND NOT CAN-FIND
	(
	FIRST LLL OF LOAN
	WHERE SUBSTRING(LLL.ACCT, 1, 5) = '91315'
	)
/* минус течения */
AND LOAN.CLASS-CODE <> "loan-transh"
/* есть несписанный залог */
AND CAN-FIND
	(
	FIRST LLL OF LOAN
	WHERE LOOKUP( SUBSTRING(LLL.ACCT, 1, 3), '913,914') > 0
	/**/
	AND 
		(	/* 913 */
			(
			SUBSTRING(LLL.ACCT, 1, 3) = '913'
			/* залог */
			/* принят */
			AND CAN-FIND
					(
					FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-CR = LLL.ACCT
					AND OP-ENTRY.OP-DATE <> ?
					)
			/* но не списан */
			AND NOT CAN-FIND
					(
					FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-DB = LLL.ACCT
					AND OP-ENTRY.OP-DATE <> ?
					)
			)
			OR
			(
			/* 914 */
			SUBSTRING(LLL.ACCT, 1, 3) = '914'
			/* залог */
			/* принят */
			AND CAN-FIND
					(
					FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-DB = LLL.ACCT
					)
			/* но не списан */
			AND NOT CAN-FIND
					(
					FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-CR = LLL.ACCT
					)		
			)
		)
	)
NO-LOCK:
		/**/
		TMP_LOAN = LOAN.CONT-CODE.
		FL = 0.
		/**/
		/* найдем остатки по счетам на последний закрытый день */
		/**/
		FOR EACH LOAN-ACCT OF LOAN
		WHERE( LOOKUP( SUBSTRING(LOAN-ACCT.ACCT, 1, 3), '457') > 0
		OR LOOKUP( SUBSTRING(LOAN-ACCT.ACCT, 1, 4), '4550') > 0
		OR LOOKUP( SUBSTRING(LOAN-ACCT.ACCT, 1, 5), '47427,47423,45915,91604,45815') > 0 )
		NO-LOCK,
			LAST ACCT-POS
			WHERE ACCT-POS.ACCT = LOAN-ACCT.ACCT
			AND ACCT-POS.CURRENCY = LOAN-ACCT.CURRENCY
			AND ACCT-POS.SINCE < end-date:
			
				/**/
				POL_SUM = 0.
				
				/* найдем оборот по незакрытым дням */
				FOR EACH OP-ENTRY
				WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
				AND OP-ENTRY.OP-DATE > ACCT-POS.SINCE
				AND OP-ENTRY.OP-DATE <= end-date
				NO-LOCK:
					/**/
					POL_SUM = POL_SUM + OP-ENTRY.AMT-RUB.
					/**/
				END.
				/**/
				OTR_SUM = 0.
				/**/
				FOR EACH OP-ENTRY
				WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
				AND OP-ENTRY.OP-DATE > ACCT-POS.SINCE
				AND OP-ENTRY.OP-DATE <= end-date
				NO-LOCK:
					/**/
					OTR_SUM = OTR_SUM + OP-ENTRY.AMT-RUB.
					/**/
				END.
				
				/* получим окончательный остаток по счету */
				IF ACCT-POS.BALANCE + POL_SUM - OTR_SUM <> 0 THEN
					DO:
						/**/
						FL = 0.
						LEAVE.
					END.
				ELSE
					DO:
						FL = 1.
					END.		
				/**/				
				
		END.
		/**/
		IF FL = 1 THEN 
			DO:
				/**/
				CREATE ZZ.
				/**/
				ASSIGN
					ZZ.CD = TMP_LOAN.
				/**/
			END.
		/**/
END.

/*
run instview.p(TEMP-TABLE ZZ:HANDLE). 
*/
FOR EACH ZZ
	NO-LOCK:
			/* значение параметра 9 = 0*/
			RUN RE_PARAM IN h_Loan
					(9, 			/* код параметра */
					TODAY, 			/* дата */
					"Кредит", 		/* назначение договора */
					ZZ.CD, 			/* код договора */
					OUTPUT par9, 	/* значение параметра */
					OUTPUT a1,
					OUTPUT a2).
			/**/
			IF par9 <> 0 THEN
				DO:
					/**/
					delete ZZ.
					/**/
				END.
			/**/
END.

/*
run instview.p(TEMP-TABLE ZZ:HANDLE). 
*/

/**/
IF CAN-FIND(FIRST ZZ WHERE ZZ.CD <> ?) THEN
	DO:
		/**/
		fname = "./bad_credits_"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
		/**/
		FOR EACH ZZ
			NO-LOCK:
					/**/
					put stream vvs unformatted
					ZZ.CD 
					eol.
					/**/
			END.
		/**/							
		output stream vvs close.

		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
	END.
ELSE
	MESSAGE "Не найдено несписанных залогов!" VIEW-AS ALERT-BOX.


