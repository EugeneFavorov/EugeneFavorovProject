{globals.i}
{intrface.get tmess}
{intrface.get xclass}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: данные ПТС
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/
DEF VAR sumOPL AS DEC INIT 0 NO-UNDO.
DEF BUFFER FF FOR PL_INDOCSREG.
/**/
def temp-table ZZ NO-UNDO
  field CONT_CODE	AS CHAR		/* + № кредитного договора */
  field FIO			AS CHAR		/* + ФИО заемщика */
  field OPEN_DATE	AS DATE		/* + дата кредитного договора */
  field SROK_PTS	AS DATE		/* + срок сдачи ПТС */
  field POST_BANK	AS DATE		/* + дата поступления ПТС в Банк */
  field U_POSTB		AS CHAR		/* + кто создал событие <ПостБанк> */
  field Z_POSTB		AS DATETIME	/* + дата создания события <ПостБанк> */
  field SUM_PROV	AS DEC		/* + сумма проводки по списанию пени (Дт 40817 Кт 70601) */
  field PROV_DATE	AS DATE  	/* + дата проводки по списанию пени (Дт 40817 Кт 70601) */
.
/**/
{empty ZZ}
/**/
DEF VAR TMP_SM 	AS DEC	INIT 0 NO-UNDO.
DEF VAR TMP_ST 	AS DEC	INIT 0 NO-UNDO.
DEF VAR EVENT1 	AS CHAR 	INIT 'СрокСдачи' 	NO-UNDO.
DEF VAR EVENT2 	AS CHAR 	INIT 'ПостБанк'	 	NO-UNDO.
DEF VAR TMP_DATE AS DATE  NO-UNDO.

def var fname as char  init "./ff.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def new shared stream vvs.


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

/* ФИО пользователя */
FUNCTION Get_UserName RETURN CHAR
	(
	USR AS CHAR
	):
	/**/
	FIND FIRST _USER
	WHERE _USER._USERID EQ USR
	NO-LOCK NO-ERROR.
		/**/
		IF AVAIL _USER THEN
			DO:
				/**/
				RETURN _USER._USER-NAME.
				/**/
			END.
		ELSE
			RETURN ?.
END.
	

{getdates.i}
{spinner.i "Поиск КД..."}
/* идем по 40817 */
FOR EACH LOAN-ACCT
	WHERE LOAN-ACCT.CONTRACT = 'Кредит'
	AND LOAN-ACCT.CONT-CODE MATCHES("*АП*")
	AND ( LOAN-ACCT.ACCT-TYPE = 'КредРасч' OR LOAN-ACCT.ACCT-TYPE = 'КредРасч1' )
	AND SUBSTRING(LOAN-ACCT.ACCT, 1, 5) = '40817'
	/* есть проводка списания пени за ПТС */
	AND CAN-FIND
		(
		FIRST OP-ENTRY
		WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
		AND OP-ENTRY.ACCT-CR MATCHES("70601*7101101*")
		AND OP-ENTRY.OP-DATE >= beg-date
		AND OP-ENTRY.OP-DATE <= end-date
		)
	/* не закрытый КД */
	AND CAN-FIND
		(
		FIRST LOAN 
		WHERE LOAN.CONTRACT = LOAN-ACCT.CONTRACT
		AND LOAN.CONT-CODE = LOAN-ACCT.CONT-CODE
		AND LOAN.CLOSE-DATE = ?
		)
	/* есть событие 1 */
	AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		)
	/* есть событие 2 */
	AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT2
		)
		NO-LOCK:
		
		/**/
		FIND FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		NO-LOCK NO-ERROR.
			/**/
			IF AVAIL PL_INDOCSREG THEN
				DO:
					/**/
					FIND FF
					WHERE FF.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
					AND FF.EVENT = EVENT2
					NO-LOCK NO-ERROR.					
					/**/
						/**/
						IF AVAIL FF THEN
							DO:
								/* условие событий  = если все вовремя сделал, но были проводки списания за штраф */
								IF PL_INDOCSREG.DATE_VALUE >= FF.DATE_VALUE THEN
									DO:
										TMP_SM = 0.
										/* найдем суммы проводок */
										FOR EACH OP-ENTRY
										WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
										AND OP-ENTRY.ACCT-CR MATCHES("70601*7101101*")
										AND OP-ENTRY.OP-DATE >= beg-date
										AND OP-ENTRY.OP-DATE <= end-date
										NO-LOCK:
											/**/
											TMP_SM = TMP_SM + OP-ENTRY.AMT-RUB.
											TMP_DATE = OP-ENTRY.OP-DATE.
											/**/
										END.
										/**/
										TMP_ST = 0.
										/* а также суммы сторнирующих проводок */
										FOR EACH OP-ENTRY
										WHERE OP-ENTRY.ACCT-DB MATCHES("70601*7101101*")
										AND OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
										AND OP-ENTRY.OP-DATE >= beg-date
										AND OP-ENTRY.OP-DATE <= end-date
										NO-LOCK:
											/**/
											TMP_ST = TMP_ST + OP-ENTRY.AMT-RUB.
											/**/
										END.
										/**/
										IF TMP_ST <> TMP_SM THEN
											DO:
												/**/
												CREATE ZZ.
												/**/
												ASSIGN
													ZZ.CONT_CODE = LOAN-ACCT.CONT-CODE  	/* + № кредитного договора */
													ZZ.SUM_PROV = TMP_SM					/* + сумма проводки по списанию пени (Дт 40817 Кт 70601) */
													ZZ.SROK_PTS = PL_INDOCSREG.DATE_VALUE	/* + срок сдачи ПТС */
													ZZ.POST_BANK = FF.DATE_VALUE			/* + дата поступления ПТС в Банк */
													ZZ.PROV_DATE = TMP_DATE					/* + дата проводки по списанию пени (Дт 40817 Кт 70601) */
													ZZ.U_POSTB = Get_UserName(FF.USER_ID)	/* + кто создал событие <ПостБанк> */
													ZZ.Z_POSTB = FF.CREATE_DATE				/* + дата создания события <ПостБанк> */
												.
												/**/
												FIND LOAN
												WHERE LOAN.CONTRACT = LOAN-ACCT.CONTRACT
												AND LOAN.CONT-CODE = LOAN-ACCT.CONT-CODE
												NO-LOCK NO-ERROR.
													/**/
													IF AVAIL LOAN THEN
														DO:
															/**/
															ZZ.OPEN_DATE = LOAN.OPEN-DATE.					/* + дата кредитного договора */
															ZZ.FIO = GetName(LOAN.CUST-CAT, LOAN.CUST-ID).	/* + ФИО заемщика */
															/**/
														END.
													/**/
												/**/
											END.
										/**/
									END.
							END.
				END.
		/**/
END.

/*
run instview.p(TEMP-TABLE ZZ:HANDLE).
*/

/* вывод */
IF AVAIL ZZ THEN 
	DO:
		/**/
		fname = "./pts_peni_"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
			/**/
			put stream vvs unformatted
			"ФИО заемщика " delim
			"№ КД" delim
			"Дата КД" delim
			"Срок сдачи ПТС" delim
			"Дата поступления ПТС в Банк" delim
			"Кто создал событие <ПостБанк" delim
			"Дата создания события <ПостБанк>" delim
			"Сумма проводки по списанию пени (Дт 40817 Кт 70601)" delim
			"дата проводки по списанию пени (Дт 40817 Кт 70601)" delim
			eol.
			/**/
			FOR EACH ZZ
			NO-LOCK:
						/**/
						put stream vvs unformatted
						ZZ.FIO delim
						ZZ.CONT_CODE delim
						STRING(ZZ.OPEN_DATE, "99.99.9999") delim
						STRING(ZZ.SROK_PTS, "99.99.9999") delim
						STRING(ZZ.POST_BANK, "99.99.9999") delim
						STRING(ZZ.U_POSTB) delim
						STRING(ZZ.Z_POSTB, "99.99.9999 HH:MM:SS") delim
						ZZ.SUM_PROV delim
						STRING(ZZ.PROV_DATE, "99.99.9999")
						eol.
						/**/
						/*
						  field CONT_CODE	AS CHAR		/* + № кредитного договора */
						  field FIO			AS CHAR		/* + ФИО заемщика */
						  field OPEN_DATE	AS DATE		/* + дата кредитного договора */
						  field SROK_PTS	AS DATE		/* + срок сдачи ПТС */
						  field POST_BANK	AS DATE		/* + дата поступления ПТС в Банк */
						  field U_POSTB		AS CHAR		/* + кто создал событие <ПостБанк> */
						  field Z_POSTB		AS DATETIME	/* + дата создания события <ПостБанк> */
						  field SUM_PROV	AS DEC		/* + сумма проводки по списанию пени (Дт 40817 Кт 70601) */
						  field PROV_DATE	AS DATE  	/* + дата проводки по списанию пени (Дт 40817 Кт 70601) */						
						*/
			END.	

		output stream vvs close.

		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
	END.
ELSE
	MESSAGE "Ничего не найдено!" VIEW-AS ALERT-BOX.

