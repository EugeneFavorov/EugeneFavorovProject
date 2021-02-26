{globals.i}
{intrface.get bicq}


/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: Отправка сообщений кассирам об изменении курса валют
   Parameters: дата и время изменения курса валют
         Uses:
      Used by:
      Created: vvv
*/

DEF VAR mDate 	AS DATE INIT TODAY NO-UNDO.  /* дата */
DEF VAR mTimes 	AS CHARACTER NO-UNDO.  		 /* время */
DEF VAR QP 		AS DATETIME NO-UNDO.
DEF VAR SF		AS DATETIME NO-UNDO.
DEF VAR sell 	AS CHAR INIT "Продажа" NO-UNDO.
DEF VAR buy 	AS CHAR INIT "Покупка" NO-UNDO.
/**/
DEF VAR tmp_str	AS CHAR INIT "" NO-UNDO.
DEF VAR tmp_usr	AS CHAR INIT "" NO-UNDO.
/**/
DEFINE TEMP-TABLE frates
    FIELD branch 	AS CHAR		/* подразделение */
	FIELD datetime	AS DATETIME /* дата и время действия*/
	FIELD type		AS CHAR		/* тип курса */
	FIELD cur		AS CHAR		/* валюта */
	FIELD code		AS CHAR		/* код валюты */
	FIELD val		AS DECIMAL	/* значение */
	FIELD usr		AS CHAR		/* кому отправляем сообщения */
.

DEFINE TEMP-TABLE mess
	FIELD branch 	AS CHAR
	FIELD usr		AS CHAR
	FIELD MESS		AS CHAR
.

{empty frates}

/**/
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").

/* нарисуем формочку для запроса данных */
DEFINE FRAME fGet  
   mDate 			LABEL			"   Дата       " 							SKIP
   mTimes			LABEL			"   Время      " 	FORMAT "99:99:99" 		SKIP

   WITH WIDTH 30 /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Изменение курса валют ]" .

/* Время*/
ON LEAVE OF mTimes IN FRAME fGet  
DO:	  

	 ERROR-STATUS:ERROR = NO.
	 
     SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + SELF:SCREEN-VALUE) NO-ERROR.
	 
		IF ERROR-STATUS:ERROR = YES THEN DO:
			MESSAGE "Неверно указано время!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			RETURN NO-APPLY.
		END.	
   mTimes = SELF:SCREEN-VALUE.
END.		
		
MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mDate
			mTimes
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.	
	
	/* попробуем найти изменения курсов валют, произведенных в указанное время */	
	
	FOR EACH irate-time
		WHERE irate-time.iratedatetime = DATETIME( STRING(mDATE, "99-99-9999") + " " + STRING(mTimes,"99:99:99") )
		AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
		NO-LOCK BY irate-time.branch-id :
		
			CREATE frates.
			
			ASSIGN
				frates.branch = irate-time.branch-id
				frates.datetime = irate-time.iratedatetime
				frates.type = irate-time.rate-type
				frates.code = irate-time.instr-code
				frates.val = irate-time.rate-instr
			.
			/**/
			FIND FIRST currency 
				WHERE currency.currency = irate-time.instr-code
			NO-LOCK NO-ERROR.
			
			/**/
			IF AVAIL currency THEN
				ASSIGN 
					frates.cur = currency.name-currenc
				.
			/**/	
	END.
	

	/**/
	IF AVAIL frates THEN
		DO:
			/* сгруппируем сообщения по подразделению */
			FOR EACH frates
				BREAK BY frates.branch:
					/**/
					IF FIRST-OF(frates.branch) THEN 
						DO:
							/**/
							tmp_str = "".
							/**/
							CREATE mess.
							/**/
							ASSIGN
								mess.branch = frates.branch
							.
						END.				
					/**/	
					IF tmp_str = "" THEN
						tmp_str = "|" + STRING(frates.cur, "x(13)") + "|" + STRING(frates.type, "x(13)") + "|" + STRING(STRING(mTimes,"99:99:99"),"x(9)")+ "|" + STRING(STRING(frates.val,"-99.9999"), "x(12)") + "|" + "@".
					ELSE
						tmp_str = tmp_str + "|" + STRING(frates.cur, "x(13)") + "|" + STRING(frates.type, "x(13)") + "|" + STRING(STRING(mTimes,"99:99:99"),"x(9)")+ "|" + STRING(STRING(frates.val,"-99.9999"), "x(12)") + "|" + "@".
					/**/
					IF LAST-OF(frates.branch) THEN
						DO:
							ASSIGN
								mess.mess = tmp_str.
						END.
					/**/
			END.

			/* идем по таблице */
			FOR EACH mess
				BY mess.branch:
				/**/
				tmp_usr = "".
				/* находим открытые смены кассиров этого подразделения */
				FOR EACH sessions
					WHERE sessions.op-date = mDate
					AND sessions.branch-id = mess.branch
					AND sessions.dpr-status = "ОТКРЫТА":
					/**/
					tmp_usr = tmp_usr + (IF tmp_usr = "" THEN "" ELSE ",") + sessions.user-id.
				END.
				/**/
				ASSIGN
					mess.usr = tmp_usr
				.
			END.
			/**/

			/* отправляем сообщения */
			FOR EACH mess :
				RUN MyGroupSendMess in h_Bicq(mess.usr, "", mess.mess, "currency").
			END.
			/**/
			MESSAGE "Сообщения успешно отправлены!" VIEW-AS ALERT-BOX.
			/**/
		END.
	ELSE
		MESSAGE "За указанное дату и время изменение курса не найдено ни у одного подразделения!" VIEW-AS ALERT-BOX.
	
	/*
	RUN MyGroupSendMess in h_Bicq("0_TEST", "", "|   Доллар    | Продажа/Покупка | 35.1242/38.4120  |@|    ЕВРО     | Продажа/Покупка | 35.1242/38.4120  |", "currency").
	*/
	
END.


