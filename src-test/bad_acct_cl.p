{globals.i}
{intrface.get tmess}
{intrface.get xclass}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: ведомость по несоответствию счетов с резиденством клиента
   Parameters:
         Uses:
		 Date: 04/07/2014
      Used by:
      Created: vvv
*/
DEF VAR vH AS INT64 INIT 0 NO-UNDO.

DEF VAR REZ 	AS CHAR INIT "423,40817,70606*1601401,70606*1601401,40909,40912" NO-UNDO.
DEF VAR NREZ 	AS CHAR INIT "426,40820,70606*1602201,70606*1602101,40910,40913,40807" NO-UNDO.

DEF BUFFER GG FOR ACCT.
DEF BUFFER LL FOR LOAN.
DEF BUFFER AA FOR LOAN-ACCT.

DEF TEMP-TABLE FF NO-UNDO
	/* данные по счетам */
	field jTYPE			AS CHAR		/* Резидент / Нерезидент */
	field jCUSTCAT		AS CHAR		/* Тип клиента */
	field jCUSTID		AS INT64	/* Номер клиента */
	field jNAME			AS CHAR		/* ФИО клиента */
	field jCOUNTRY		AS CHAR		/* Код страны */
	field jDOCTYPE		AS CHAR 	/* Тип документа */
	field jDOCNUM		AS CHAR		/* Номер документа */	
	field jDOCWHOM		AS CHAR		/* Кем выдан документ */
	field jACCTS		AS CHAR		/* Список неверных счетов */
	field jCREDITS		AS CHAR		/* Список неверных КД */
	field jDEPOSITS		AS CHAR		/* Список неверных DPS*/
.	

{empty FF}

DEF VAR fname 	AS CHAR  	INIT "./bad_acc_cl.csv"  	NO-UNDO.
def var delim 	as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

def new shared stream vvs.

/*------------------------------------------------------------*/
/* находим наименование клиента */
FUNCTION GetName RETURNS CHAR
	(
	cat AS CHARACTER,
	id AS INT64
	):
	/**/
	DEF VAR sNAME AS CHAR NO-UNDO.
	/**/
	IF cat = "Ч" THEN
		DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL PERSON THEN
				/* ФИО клиента */
				sNAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
	ELSE
		DO:
			FIND FIRST CUST-CORP 
			WHERE CUST-CORP.CUST-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL CUST-CORP THEN
				/* наименование организации */
				sNAME = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		END.
	/**/
	RETURN sNAME.
	/**/
END.

/*------------------------- Поиск по ФЛ резидентам -------------------------*/
{spinner.i "Поиск по счетам ФЛ резидентов..."}
/* идем по резидентам */
FOR EACH PERSON
	WHERE PERSON.COUNTRY-ID = 'RUS'
	AND CAN-FIND
		(
		FIRST ACCT
		WHERE ACCT.CUST-CAT = 'Ч'
		AND ACCT.CUST-ID = PERSON.PERSON-ID
		AND ( ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1,1,2014))
		/**/
		AND ( SUBSTRING(ACCT.ACCT, 1, 3) = "426"
			  OR
			  LOOKUP( SUBSTRING(ACCT.ACCT, 1, 5), '40820,40910,40913,40807' ) > 0
			)
		/**/
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( FIRST FF
						 WHERE FF.jCUSTCAT = 'Ч'
						 AND FF.jCUSTID = PERSON.PERSON-ID
						) THEN 	DO:
									/**/
									CREATE FF.
									/**/
									ASSIGN
										FF.jTYPE = 'Резидент'
										FF.jCUSTCAT = 'Ч'
										FF.jCUSTID = PERSON.PERSON-ID
										FF.jCOUNTRY = PERSON.COUNTRY-ID
										FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
										FF.jDOCTYPE = PERSON.DOCUMENT-ID
										FF.jDOCNUM = PERSON.DOCUMENT
										FF.jDOCWHOM = PERSON.ISSUE
									.
									/* найдем счета */
									FOR EACH GG
									WHERE GG.CUST-CAT = 'Ч'
									AND GG.CUST-ID = PERSON.PERSON-ID
									AND ( GG.CLOSE-DATE = ? OR GG.CLOSE-DATE >= DATE(1,1,2014))
									/**/
									AND ( SUBSTRING(GG.ACCT, 1, 3) = "426"
										  OR
										  LOOKUP( SUBSTRING(GG.ACCT, 1, 5), '40820,40910,40913,40807' ) > 0
										)
									NO-LOCK:
										/**/
										FF.jACCTS = (IF FF.jACCTS = "" THEN "'" + GG.NUMBER ELSE FF.jACCTS + ",'" + GG.NUMBER).
										/**/
									END.
									/**/
								END.
END.

/*------------------------- Поиск по ФЛ нерезидентам -------------------------*/
{spinner.i "Поиск по счетам ФЛ нерезидентов..."}
/* идем по нерезидентам */
FOR EACH PERSON
	WHERE PERSON.COUNTRY-ID <> 'RUS'
	AND CAN-FIND
		(
		FIRST ACCT
		WHERE ACCT.CUST-CAT = 'Ч'
		AND ACCT.CUST-ID = PERSON.PERSON-ID
		AND ( ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1,1,2014))
		/**/
		AND ( SUBSTRING(ACCT.ACCT, 1, 3) = "423"
			  OR
			  LOOKUP( SUBSTRING(ACCT.ACCT, 1, 5), '40817,40909,40912' ) > 0
			)
		/**/
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( FIRST FF
						 WHERE FF.jCUSTCAT = 'Ч'
						 AND FF.jCUSTID = PERSON.PERSON-ID
						) THEN 	DO:
									/**/
									CREATE FF.
									/**/
									ASSIGN
										FF.jTYPE = 'НЕрезидент'
										FF.jCUSTCAT = 'Ч'
										FF.jCUSTID = PERSON.PERSON-ID
										FF.jCOUNTRY = PERSON.COUNTRY-ID
										FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
										FF.jDOCTYPE = PERSON.DOCUMENT-ID
										FF.jDOCNUM = PERSON.DOCUMENT
										FF.jDOCWHOM = PERSON.ISSUE
									.
									/* найдем счета */
									FOR EACH GG
									WHERE GG.CUST-CAT = 'Ч'
									AND GG.CUST-ID = PERSON.PERSON-ID
									AND ( GG.CLOSE-DATE = ? OR GG.CLOSE-DATE >= DATE(1,1,2014))
									/**/
									AND ( SUBSTRING(GG.ACCT, 1, 3) = "423"
										  OR
										  LOOKUP( SUBSTRING(GG.ACCT, 1, 5), '40817,40909,40912' ) > 0
										)
									NO-LOCK:
										/**/
										FF.jACCTS = (IF FF.jACCTS = "" THEN "'" + GG.NUMBER ELSE FF.jACCTS + ",'" + GG.NUMBER).
										/**/
									END.
									/**/
								END.
END.

/*------------------------- Поиск по ЮЛ резидентам -------------------------*/
{spinner.i "Поиск по счетам ЮЛ резидентов..."}
/* идем по резидентам */
FOR EACH CUST-CORP
	WHERE CUST-CORP.COUNTRY-ID = 'RUS'
	AND CAN-FIND
		(
		FIRST ACCT
		WHERE ACCT.CUST-CAT = 'Ю'
		AND ACCT.CUST-ID = CUST-CORP.CUST-ID
		AND ( ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1,1,2014))
		/**/
		AND ( SUBSTRING(ACCT.ACCT, 1, 3) = "426"
			  OR
			  LOOKUP( SUBSTRING(ACCT.ACCT, 1, 5), '40820,40910,40913,40807' ) > 0
			)
		/**/
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( FIRST FF
						 WHERE FF.jCUSTCAT = 'Ю'
						 AND FF.jCUSTID = CUST-CORP.CUST-ID
						) THEN 	DO:
									/**/
									CREATE FF.
									/**/
									ASSIGN
										FF.jTYPE = 'Резидент'
										FF.jCUSTCAT = 'Ю'
										FF.jCUSTID = CUST-CORP.CUST-ID
										FF.jCOUNTRY = CUST-CORP.COUNTRY-ID
										FF.jNAME = GetName('Ю', PERSON.PERSON-ID)
									.
									/* найдем счета */
									FOR EACH GG
									WHERE GG.CUST-CAT = 'Ю'
									AND GG.CUST-ID = CUST-CORP.CUST-ID
									AND ( GG.CLOSE-DATE = ? OR GG.CLOSE-DATE >= DATE(1,1,2014))
									/**/
									AND ( SUBSTRING(GG.ACCT, 1, 3) = "426"
										  OR
										  LOOKUP( SUBSTRING(GG.ACCT, 1, 5), '40820,40910,40913,40807' ) > 0
										)
									NO-LOCK:
										/**/
										FF.jACCTS = (IF FF.jACCTS = "" THEN "'" + GG.NUMBER ELSE FF.jACCTS + ",'" + GG.NUMBER).
										/**/
									END.
									/**/
								END.
END.

/*------------------------- Поиск по ЮЛ нерезидентам -------------------------*/
{spinner.i "Поиск по счетам ЮЛ нерезидентов..."}
/* идем по нерезидентам */
FOR EACH CUST-CORP
	WHERE CUST-CORP.COUNTRY-ID <> 'RUS'
	AND CAN-FIND
		(
		FIRST ACCT
		WHERE ACCT.CUST-CAT = 'Ю'
		AND ACCT.CUST-ID = CUST-CORP.CUST-ID
		AND ( ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1,1,2014))
		/**/
		AND ( SUBSTRING(ACCT.ACCT, 1, 3) = "423"
			  OR
			  LOOKUP( SUBSTRING(ACCT.ACCT, 1, 5), '40817,40909,40912' ) > 0
			)
		/**/
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( FIRST FF
						 WHERE FF.jCUSTCAT = 'Ю'
						 AND FF.jCUSTID = CUST-CORP.CUST-ID
						) THEN 	DO:
									/**/
									CREATE FF.
									/**/
									ASSIGN
										FF.jTYPE = 'НЕрезидент'
										FF.jCUSTCAT = 'Ю'
										FF.jCUSTID = CUST-CORP.CUST-ID
										FF.jCOUNTRY = CUST-CORP.COUNTRY-ID
										FF.jNAME = GetName('Ю', CUST-CORP.CUST-ID)
									.
									/* найдем счета */
									FOR EACH GG
									WHERE GG.CUST-CAT = 'Ю'
									AND GG.CUST-ID = CUST-CORP.CUST-ID
									AND ( GG.CLOSE-DATE = ? OR GG.CLOSE-DATE >= DATE(1,1,2014))
									/**/
									AND ( SUBSTRING(GG.ACCT, 1, 3) = "423"
										  OR
										  LOOKUP( SUBSTRING(GG.ACCT, 1, 5), '40817,40909,40912' ) > 0
										)
									NO-LOCK:
										/**/
										FF.jACCTS = (IF FF.jACCTS = "" THEN "'" + GG.NUMBER ELSE FF.jACCTS + ",'" + GG.NUMBER).
										/**/
									END.
									/**/
								END.
END.

/*------------------------- Поиск по кредитам ФЛ резидентов -------------------------*/
{spinner.i "Поиск по кредитам ФЛ резидентов..."}
/* идем по резидентам */
FOR EACH LOAN
	WHERE LOAN.CUST-CAT = 'Ч'
	AND CAN-FIND
		(
		FIRST PERSON
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		AND PERSON.COUNTRY-ID = 'RUS'
		)
	AND LOAN.CONTRACT = 'Кредит'
	AND ( LOAN.CLOSE-DATE >= DATE(1,1,2014) OR LOAN.CLOSE-DATE = ?)
	AND CAN-FIND
		(
		/* счет доходов НЕРЕЗИДЕНТОВ */
		FIRST LOAN-ACCT OF LOAN
		WHERE LOAN-ACCT.ACCT = '70601810304001117101     @0400'
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( 
						FIRST FF
						WHERE FF.jCUSTCAT = 'Ч'
						AND FF.jCUSTID = LOAN.CUST-ID
						)
						THEN 	DO:	
									/**/
									FIND FIRST PERSON
									WHERE PERSON.PERSON-ID = LOAN.CUST-ID
									NO-LOCK NO-ERROR.
									/**/
										IF AVAIL PERSON THEN
											DO:
												/**/
												CREATE FF.
												/**/
												ASSIGN
													FF.jTYPE = 'Резидент'
													FF.jCUSTCAT = 'Ч'
													FF.jCUSTID = PERSON.PERSON-ID
													FF.jCOUNTRY = PERSON.COUNTRY-ID
													FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
													FF.jDOCTYPE = PERSON.DOCUMENT-ID
													FF.jDOCNUM = PERSON.DOCUMENT
													FF.jDOCWHOM = PERSON.ISSUE										
												.
											END.
								END.
								/**/
		/* запомним КД */
		FF.jCREDITS = (IF FF.jCREDITS = "" THEN "" + LOAN.DOC-REF ELSE FF.jCREDITS + "," + LOAN.DOC-REF).
		/* и счет доходов */
		FF.jACCTS = "'" + "70601810304001117101".
		/**/		
END.	

/*------------------------- Поиск по кредитам ФЛ НЕрезидентов -------------------------*/
{spinner.i "Поиск по кредитам ФЛ НЕрезидентов..."}
/* идем по НЕрезидентам */
FOR EACH LOAN
	WHERE LOAN.CUST-CAT = 'Ч'
	AND CAN-FIND
		(
		FIRST PERSON
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		AND PERSON.COUNTRY-ID <> 'RUS'
		)
	AND LOAN.CONTRACT = 'Кредит'
	AND ( LOAN.CLOSE-DATE >= DATE(1,1,2014) OR LOAN.CLOSE-DATE = ?)
	AND CAN-FIND
		(
		/* счет доходов РЕЗИДЕНТОВ */
		FIRST LOAN-ACCT OF LOAN
		WHERE LOAN-ACCT.ACCT = '70601810704001115101     @0400'
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( 
						FIRST FF
						WHERE FF.jCUSTCAT = 'Ч'
						AND FF.jCUSTID = LOAN.CUST-ID
						)
						THEN 	DO:
									/**/
									FIND FIRST PERSON
									WHERE PERSON.PERSON-ID = LOAN.CUST-ID
									NO-LOCK NO-ERROR.									
										/**/
										IF AVAIL PERSON THEN
											DO:
												/**/
												CREATE FF.
												/**/
												ASSIGN
													FF.jTYPE = 'НЕрезидент'
													FF.jCUSTCAT = 'Ч'
													FF.jCUSTID = PERSON.PERSON-ID
													FF.jCOUNTRY = PERSON.COUNTRY-ID
													FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
													FF.jDOCTYPE = PERSON.DOCUMENT-ID
													FF.jDOCNUM = PERSON.DOCUMENT
													FF.jDOCWHOM = PERSON.ISSUE										
												.
											END.
								END.
								/**/
		/* запомним КД */
		FF.jCREDITS = (IF FF.jCREDITS = "" THEN "" + LOAN.DOC-REF ELSE FF.jCREDITS + "," + LOAN.DOC-REF).
		/* и счет */
		FF.jACCTS = "'" + "70601810704001115101     @0400".
		/**/		
END.	

/*------------------------- Поиск по вкладам ФЛ резидентов -------------------------*/
{spinner.i "Поиск по вкладам ФЛ резидентов..."}
/* идем по резидентам */
FOR EACH LOAN
	WHERE LOAN.CUST-CAT = 'Ч'
	AND CAN-FIND
		(
		FIRST PERSON
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		AND PERSON.COUNTRY-ID = 'RUS'
		)
	AND LOAN.CONTRACT = 'dps'
	AND ( LOAN.CLOSE-DATE >= DATE(1,1,2014) OR LOAN.CLOSE-DATE = ?)
	AND CAN-FIND
		(
		/* счет доходов НЕРЕЗИДЕНТОВ */
		FIRST LOAN-ACCT OF LOAN
		WHERE LOAN-ACCT.ACCT MATCHES("70606*1602201")
		OR LOAN-ACCT.ACCT MATCHES("70606*1602101")
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( 
						FIRST FF
						WHERE FF.jCUSTCAT = 'Ч'
						AND FF.jCUSTID = LOAN.CUST-ID
						)
						THEN 	DO:	
									/**/
									FIND FIRST PERSON
									WHERE PERSON.PERSON-ID = LOAN.CUST-ID
									NO-LOCK NO-ERROR.
									/**/
										IF AVAIL PERSON THEN
											DO:
												/**/
												CREATE FF.
												/**/
												ASSIGN
													FF.jTYPE = 'Резидент'
													FF.jCUSTCAT = 'Ч'
													FF.jCUSTID = PERSON.PERSON-ID
													FF.jCOUNTRY = PERSON.COUNTRY-ID
													FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
													FF.jDOCTYPE = PERSON.DOCUMENT-ID
													FF.jDOCNUM = PERSON.DOCUMENT
													FF.jDOCWHOM = PERSON.ISSUE										
												.
											END.
								END.
								/**/
		/* запомним DPS */
		FF.jDEPOSITS = (IF FF.jDEPOSITS = "" THEN "" + LOAN.DOC-REF ELSE FF.jDEPOSITS + "," + LOAN.DOC-REF).
		/* и счет доходов */
		FF.jACCTS = "'" + "70606*1602201" + " или " + "70606*1602101".
		/**/		
END.

/*------------------------- Поиск по вкладам ФЛ НЕрезидентов -------------------------*/
{spinner.i "Поиск по вкладам ФЛ НЕрезидентов..."}
/* идем по НЕрезидентам */
FOR EACH LOAN
	WHERE LOAN.CUST-CAT = 'Ч'
	AND CAN-FIND
		(
		FIRST PERSON
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		AND PERSON.COUNTRY-ID <> 'RUS'
		)
	AND LOAN.CONTRACT = 'dps'
	AND ( LOAN.CLOSE-DATE >= DATE(1,1,2014) OR LOAN.CLOSE-DATE = ?)
	AND CAN-FIND
		(
		/* счет доходов РЕЗИДЕНТОВ */
		FIRST LOAN-ACCT OF LOAN
		WHERE LOAN-ACCT.ACCT MATCHES("70606*1601401")
		OR LOAN-ACCT.ACCT MATCHES("70606*1601101")
		)
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( 
						FIRST FF
						WHERE FF.jCUSTCAT = 'Ч'
						AND FF.jCUSTID = LOAN.CUST-ID
						)
						THEN 	DO:	
									/**/
									FIND FIRST PERSON
									WHERE PERSON.PERSON-ID = LOAN.CUST-ID
									NO-LOCK NO-ERROR.
									/**/
										IF AVAIL PERSON THEN
											DO:
												/**/
												CREATE FF.
												/**/
												ASSIGN
													FF.jTYPE = 'НЕрезидент'
													FF.jCUSTCAT = 'Ч'
													FF.jCUSTID = PERSON.PERSON-ID
													FF.jCOUNTRY = PERSON.COUNTRY-ID
													FF.jNAME = GetName('Ч', PERSON.PERSON-ID)
													FF.jDOCTYPE = PERSON.DOCUMENT-ID
													FF.jDOCNUM = PERSON.DOCUMENT
													FF.jDOCWHOM = PERSON.ISSUE										
												.
											END.
								END.
								/**/
		/* запомним DPS */
		FF.jDEPOSITS = (IF FF.jDEPOSITS = "" THEN "" + LOAN.DOC-REF ELSE FF.jDEPOSITS + "," + LOAN.DOC-REF).
		/* и счет доходов */
		FF.jACCTS = "'" + "70606*1601401" + " или " + "70606*1601101".
		/**/		
END.

/*
run instview.p(TEMP-TABLE FF:HANDLE). 
*/

IF AVAIL FF THEN
	DO:
		/**/
		fname = "./bad_acct_cl"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
			/**/
			/*
			field jTYPE			AS CHAR		/* Резидент / Нерезидент */
			field jCUSTCAT		AS CHAR		/* Тип клиента */
			field jCUSTID		AS INT64	/* Номер клиента */
			field jNAME			AS CHAR		/* ФИО клиента */
			field jCOUNTRY		AS CHAR		/* Код страны */
			field jDOCTYPE		AS CHAR 	/* Тип документа */
			field jDOCNUM		AS CHAR		/* Номер документа */	
			field jDOCWHOM		AS CHAR		/* Кем выдан документ */
			field jACCTS		AS CHAR		/* Список неверных счетов */			
			*/
			/**/
			put stream vvs unformatted
								"Резидент/Нерезидент" delim
								"Тип клиента" delim
								"Номер клиента" delim
								"ФИО клиента" delim
								"Код страны" delim
								"Тип документа" delim
								"Номер документа" delim
								"Кем выдан документ" delim
								"Список неверных счетов" delim
								"Список КД" delim
								"Список Депозитов"
								eol.
			/**/						
			FOR EACH FF
				NO-LOCK:
					/**/
					put stream vvs unformatted
								FF.jTYPE delim
								FF.jCUSTCAT delim
								FF.jCUSTID delim
								FF.jNAME delim
								FF.jCOUNTRY delim
								FF.jDOCTYPE delim
								FF.jDOCNUM delim
								FF.jDOCWHOM delim
								FF.jACCTS delim
								FF.jCREDITS delim
								FF.jDEPOSITS
								eol.					
					/**/
			END.	

		output stream vvs close.

		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
	END.
ELSE
	DO:
		/**/
		MESSAGE "Расхождений по счетам не найдено!" VIEW-AS ALERT-BOX.
		/**/
	END.

