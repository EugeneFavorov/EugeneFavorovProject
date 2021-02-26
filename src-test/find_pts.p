{globals.i}
{intrface.get tmess}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: ПТС
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/
DEF VAR EVENT1 	AS CHAR 	INIT 'СрокСдачи' 	NO-UNDO.
DEF VAR EVENT2 	AS CHAR 	INIT 'ПостБанк'	 	NO-UNDO.
DEF VAR vH		AS INT64 	INIT	0			NO-UNDO.
DEF VAR TMP_CD	AS CHAR		INIT ""				NO-UNDO.

DEF BUFFER FF FOR PL_INDOCSREG.

DEF VAR fname 	AS CHAR  	INIT "./PTS.csv"  	NO-UNDO.
def var delim 			as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

def new shared stream vvs.

DEF TEMP-TABLE CRD NO-UNDO
	FIELD CODE 	AS CHAR					/* № КД*/
	FIELD FIO	AS CHAR					/* ФИО */
	FIELD STS 	AS CHAR					/* Статус */
	FIELD SUM	AS DEC					/* Сумма проводок по 40817 */
.
{empty CRD}

{getdates.i}

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
/*------------------------------------------------------------*/
{justasec}
/**/
FOR EACH LOAN
WHERE LOAN.CONTRACT = 'Кредит'
/**/
AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		)
AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT2
		)
		:
		/**/
		FIND FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		NO-LOCK NO-ERROR.
			/**/
			IF AVAIL PL_INDOCSREG THEN
				DO:
					/**/
					FIND FF
					WHERE FF.SURROGATE = LOAN.CONT-CODE + " 0"
					AND FF.EVENT = EVENT2
					NO-LOCK NO-ERROR.					
					/**/
						/**/
						IF AVAIL FF THEN
							DO:
								/* условие событий */
								IF PL_INDOCSREG.DATE_VALUE >= FF.DATE_VALUE THEN
									DO:
										/* посмотрим есть ли проводка по начислению штрафов за ПТС */
										/* найдем счета 40817 */
										FOR EACH LOAN-ACCT OF LOAN
										WHERE SUBSTRING(LOAN-ACCT.ACCT, 1, 5) = "40817"
										AND LOAN-ACCT.ACCT-TYPE MATCHES("КредРасч*")
										NO-LOCK:
											/* 40817 - 70601 "+" */
											FOR EACH OP-ENTRY
											WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
											AND OP-ENTRY.OP-DATE >= beg-date
											AND OP-ENTRY.OP-DATE <= end-date
											AND OP-ENTRY.ACCT-CR = "70601810204007101101     @0400"
											NO-LOCK:
												/**/
												IF TMP_CD <> LOAN.CONT-CODE OR TMP_CD = "" THEN
													DO:
														/**/
														CREATE CRD.
														/**/
														ASSIGN
															CRD.CODE = LOAN.CONT-CODE
															CRD.STS	= LOAN.LOAN-STATUS
															CRD.FIO = GetName(LOAN.CUST-CAT, LOAN.CUST-ID)
															CRD.SUM = OP-ENTRY.AMT-RUB
														.
														/**/
														TMP_CD = LOAN.CONT-CODE.
														/**/
													END.
												ELSE
													DO:
														/**/
														ASSIGN
															CRD.SUM = CRD.SUM + OP-ENTRY.AMT-RUB
														.
														/**/
													END.
												/**/
											END.
											/* 70601 - 40817 "-" */
											FOR EACH OP-ENTRY
											WHERE OP-ENTRY.ACCT-DB = "70601810204007101101     @0400"
											AND OP-ENTRY.OP-DATE >= beg-date
											AND OP-ENTRY.OP-DATE <= end-date
											AND OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
											AND CAN-FIND
													(
													FIRST CRD
													WHERE CRD.CODE = LOAN-ACCT.CONT-CODE
													)
											NO-LOCK:											
												/**/
												ASSIGN
													CRD.SUM = CRD.SUM - OP-ENTRY.AMT-RUB
												.
												/**/
											END.
											/**/
										END.
										/**/
									END.
								/**/
							END.
						/**/
				END.
		/**/
END.

/*
run instview.p(TEMP-TABLE CRD:HANDLE). 
*/

IF CAN-FIND ( FIRST CRD WHERE CRD.SUM <> 0 ) THEN
	DO:
		/**/
		fname = "./PTS"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
			/**/
			put stream vvs unformatted
								"№ КД" delim
								"ФИО" delim
								"СТАТУС" delim
								"СУММА ПРОВОДОК 40817"
								eol.
			/**/						
			FOR EACH CRD
				WHERE CRD.SUM <> 0
				NO-LOCK:
					/**/
					put stream vvs unformatted
										CRD.CODE delim
										CRD.FIO delim
										CRD.STS delim
										CRD.SUM
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
		MESSAGE "Расхождений не найдено!" VIEW-AS ALERT-BOX.
		/**/
	END.




