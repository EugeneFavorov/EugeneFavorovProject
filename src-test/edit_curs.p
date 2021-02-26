{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{pick-val.i}
{refer.i}
{tmprecid.def}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: процедура редактирования курсов валют
   Parameters:
         Uses:
		 Date: 03/07/2014
      Used by:
      Created: vvv
*/

DEF VAR mAction 	AS CHAR 		INIT "Удаление"		NO-UNDO.	/* действие */
DEF VAR mDate 		AS DATE 		INIT TODAY 			NO-UNDO.  	/* дата */
DEF VAR mTimes 		AS CHARACTER 						NO-UNDO.  	/* время */
DEF VAR mTimes2 	AS CHARACTER 						NO-UNDO.  	/* новое время */
DEF VAR QP 			AS DATETIME 						NO-UNDO.	/* текущее время */
DEF VAR nCNT		AS INTEGER 		INIT 0				NO-UNDO.	/* счетчик измененных записей */

QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").
/* округлим до часа */
mTimes2 = SUBSTRING( REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":",""), 1, 2) + "0000".


/* форма выбора */
DEFINE FRAME fChoose
	mAction		VIEW-AS COMBO-BOX LIST-ITEMS 'Удаление','Редактирование' FORMAT "x(14)"
				LABEL "Действие"	
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ Выбор действия ]" .	

/* форма удаления */
DEFINE FRAME fDelete
   mDate 			LABEL			"Дата       " 							SKIP
   mTimes			LABEL			"Время      " 	FORMAT "99:99:99"
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ Удаление курсов ]" .	

/* форма редактирования */
DEFINE FRAME fEdit
   mDate 			LABEL			"Дата       " 							SKIP
   mTimes			LABEL			"Время      " 	FORMAT "99:99:99"		SKIP
   "---------------------" SKIP
   mTimes2			LABEL			"Новое время" 	FORMAT "99:99:99"
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ Редактирование ]" .		
		
/**/
MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
		/**/
		PAUSE 0.
		/**/
		UPDATE
		mAction
		WITH FRAME fChoose.	
		/**/
	END.
	/**/
	HIDE FRAME fChoose NO-PAUSE.
	/**/
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN
		LEAVE MAIN_BLOCK.
	/**/
	/* удалить курсы валют */
	IF mAction = 'Удаление' THEN
		DO:
			/**/
			IF LASTKEY EQ 27 THEN
				RETURN.
			/**/
			PAUSE 0.
			nCNT = 0.
			/**/
			UPDATE
			mDate
			mTimes
			WITH FRAME fDelete.	
			
			/**/
			HIDE FRAME fDelete NO-PAUSE.
			/**/
			IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
				DO:
					/* браузер подразделений */
					RUN browseld.p ("branch",
								   "",
								   "",
								   "branch-type",
								   2).
					/**/
					/* идем по всем отмеченным подразделениям */
					FOR EACH tmprecid
					NO-LOCK,
						/* находим подразделение */
						FIRST branch
						WHERE RECID(branch) EQ  tmprecid.id
						NO-LOCK:
							
							/* находим курсы валют для данного подразделения */
							/* на указанное время */
							FOR EACH irate-time
							WHERE irate-time.branch-id  = branch.branch-id
							AND irate-time.instr-cat  = "currency"
							AND irate-time.iratedatetime  = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99"))
							EXCLUSIVE-LOCK:
							/**/
								/* удаляем записи */
								DELETE irate-time NO-ERROR.
								/**/
								nCNT = nCNT + 1.
								/**/
							END.
							/**/
					END.		
					/**/
					MESSAGE "Удалено " + STRING(nCNT) + ( IF nCNT = 1 THEN " запись!" ELSE IF nCNT > 1 AND nCNT < 5 THEN " записи!" ELSE " записей!" ) VIEW-AS ALERT-BOX.
					/**/
				END.
		END.
	/**/
	/* изменение времени курса валют */
	IF mAction = 'Редактирование' THEN
		DO:
			/**/
			IF LASTKEY EQ 27 THEN
				RETURN.		
			/**/
			PAUSE 0.
			nCNT = 0.
			/**/
			UPDATE
			mDate
			mTimes
			mTimes2
			WITH FRAME fEdit.	
			/**/
			IF mTimes2 = "" OR mTimes2 = mTimes THEN
				DO:
					/**/
					MESSAGE "Укажите новое время!" VIEW-AS ALERT-BOX TITLE "Ошибка".
					UNDO, RETRY.
					/**/
				END.
			/**/
			HIDE FRAME fDelete NO-PAUSE.
			/**/
			IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
				DO:
					/* браузер подразделений */
					RUN browseld.p ("branch",
								   "",
								   "",
								   "branch-type",
								   2).
					/**/
					/* идем по всем отмеченным подразделениям */
					FOR EACH tmprecid
					NO-LOCK,
						/* находим подразделение */
						FIRST branch
						WHERE RECID(branch) EQ  tmprecid.id
						NO-LOCK:
							/* находим курсы валют для данного подразделения */
							/* на указанное время */
							FOR EACH irate-time
							WHERE irate-time.branch-id  = branch.branch-id
							AND irate-time.instr-cat  = "currency"
							AND irate-time.iratedatetime  = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99"))
							EXCLUSIVE-LOCK:
							/**/
								/**/
								ASSIGN
									irate-time.iratedatetime = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes2, "99:99:99"))
								.
								/**/
								nCNT = nCNT + 1.
								/**/
							END.
							/**/		
					END.		
					/**/
					MESSAGE "Изменено время у " + STRING(nCNT) + (IF nCNT = 1 THEN " записи!" ELSE " записей!") VIEW-AS ALERT-BOX.
					/**/
				END.			
		END.	
/**/
END.




