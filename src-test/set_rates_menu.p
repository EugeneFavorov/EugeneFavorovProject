/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: importpol.p
      Comment: Проверка кредитов
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

/*
{globals.i}
{setdest.i}
{intrface.get count}
*/

{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{pick-val.i}
{refer.i}
{tmprecid.def}

    
DEF VAR mBranch AS CHARACTER  NO-UNDO.  /* подразделение */
DEF VAR mDate AS DATE INIT TODAY NO-UNDO.  /* дата */
DEF VAR mTimes AS CHARACTER NO-UNDO.  /* время */
DEF VAR mCrossRate AS LOGICAL INITIAL YES. /* Кросс курс */ 
DEF VAR mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* Значение */ 
/**/
DEF VAR mValName1 AS CHAR INIT "Тенге" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName2 AS CHAR INIT "Доллар США" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName3 AS CHAR INIT "Евро" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mVal1 AS CHAR INIT "398" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal2 AS CHAR INIT "840" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal3 AS CHAR INIT "978" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mValBuy1 AS DEC NO-UNDO.
DEF VAR mValBuy2 AS DEC NO-UNDO.
DEF VAR mValBuy3 AS DEC NO-UNDO.
/**/
DEF VAR mValSell1 AS DEC NO-UNDO.
DEF VAR mValSell2 AS DEC NO-UNDO.
DEF VAR mValSell3 AS DEC NO-UNDO.
/**/
DEF VAR mValCnt1 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt2 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt3 AS DEC INIT 1 NO-UNDO.
/**/

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEFINE BUFFER xxx FOR irate-time.
DEFINE BUFFER yyy FOR irate-time.
DEFINE BUFFER zzz FOR irate-time.

/**/

DEF VAR choiceMess AS LOGICAL INIT FALSE NO-UNDO.

DEFINE BUTTON button1 LABEL "Обновить".
DEFINE BUTTON button2 LABEL "Отменить".
DEFINE VAR mMESS AS CHARACTER VIEW-AS TEXT /*FORMAT "x(20)"*/ NO-UNDO.

/**/
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").


/* без этого не работает выбор валюты */
RUN SetSysConf IN h_base("IdBranch2",shfilial).

/*---------------------------------*/
/* форма установки данных */

DEFINE FRAME fGet  
	/* верхняя строка */
	SPACE(15) 
	mDate 			LABEL			"Дата  " 							
	mTimes			LABEL			"Время   " 	FORMAT "99:99:99" 		SKIP
	/**/
	"-------------------------------------------------------"		SKIP
	"|      Валюта     | Код |  Покупка |  Продажа |   За  |"		SKIP
	"-------------------------------------------------------"		SKIP	
	"|" SPACE(1) mValName1 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal1 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy1 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell1	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt1	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP	
	/**/
	"|" SPACE(1) mValName2 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal2 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy2 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell2	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt2	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/
	"|" SPACE(1) mValName3 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal3 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy3 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell3	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt3	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/
	WITH /*WIDTH 62 */ /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 6
        TITLE COLOR BRIGHT-WHITE "[ Установка курсов валют ]" .

/**/
DEFINE FRAME Qwy
	mMESS		NO-LABEL /*FORMAT "X(25)"*/ SKIP(3)
	button1 	SPACE(5)
	button2
WITH /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9 
TITLE COLOR BRIGHT-WHITE "[ Редактирование курсов валют ]".	
		
/* Время*/
ON LEAVE OF mTimes IN FRAME fGet  
DO:	  

	 ERROR-STATUS:ERROR = NO.
	 
     SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + SELF:SCREEN-VALUE) NO-ERROR.
	 
		IF ERROR-STATUS:ERROR = YES THEN DO:
			MESSAGE "Неверно указано время!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			RETURN NO-APPLY.
		END.	
END.

/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  PAUSE 0.
		UPDATE
			mDate
			mTimes
			/**/
			mValName1
			mVal1
			mValBuy1
			mValSell1
			mValCnt1
			/**/
			mValName2
			mVal2
			mValBuy2
			mValSell2
			mValCnt2
			/**/
			mValName3
			mVal3
			mValBuy3
			mValSell3
			mValCnt3			
			/**/
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* нужно проверить указанные значения */
	/*------------------------------------*/
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59").
	/*---------*/
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99")).
	/*---------*/	
	/* проверка на прошлый день */
	IF mDate < DATE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 1, 10)) THEN
		DO:
			MESSAGE "Нельзя устанавливать курсы валют для прошлых дней!!!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.			
		END.
		
	/* все значения нулевые */	
	IF mValBuy1 = 0 AND mValBuy2 = 0 AND mValBuy3 = 0 AND mValSell1 = 0 AND mValSell2 = 0 AND mValSell3 = 0 THEN
		DO:
			MESSAGE "Не установлен ни один курс валют!!!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.		
		END.
	
	/* plus.vvv 29/04/2014 */
	/* проверка Покупки/Продажи с курсом ЦБ */
	/*
	/* 1. Тенге */
	/* найдем курс ЦБ на данную дату по определенной валюте */
	IF mValBuy1 > 0 OR mValSell1 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ 'currency'
				AND xxx.instr-code EQ mVal1
				AND xxx.rate-type EQ 'Учетный'
				AND xxx.iratedate > DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00")
				AND xxx.iratedate < DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59")
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy1 >= xxx.rate-instr AND mValBuy1 > 0 THEN
							DO:
								MESSAGE "Курс покупки " + STRING(mValName1) + " не может превышать курс ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell1 <= xxx.rate-instr AND mValSell1 > 0 THEN
							DO:
								MESSAGE "Курс продажи " + STRING(mValName1) + " не может быть меньше курса ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.			
				END.
		END.

	/* 2. Доллар США */
	/* найдем курс ЦБ на данную дату по определенной валюте */
	IF mValBuy2 > 0 OR mValSell2 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ 'currency'
				AND xxx.instr-code EQ mVal2
				AND xxx.rate-type EQ 'Учетный'
				AND xxx.iratedate > DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00")
				AND xxx.iratedate < DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59")
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy2 >= xxx.rate-instr AND mValBuy2 > 0 THEN
							DO:
								MESSAGE "Курс покупки " + STRING(mValName2) + " не может превышать курс ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell2 <= xxx.rate-instr AND mValSell2 > 0 THEN
							DO:
								MESSAGE "Курс продажи " + STRING(mValName2) + " не может быть меньше курса ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.			
				END.
		END.

	/* 3. Евро */
	/* найдем курс ЦБ на данную дату по определенной валюте */
	IF mValBuy3 > 0 OR mValSell3 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ 'currency'
				AND xxx.instr-code EQ mVal3
				AND xxx.rate-type EQ 'Учетный'
				AND xxx.iratedate > DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00")
				AND xxx.iratedate < DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59")
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy3 >= xxx.rate-instr AND mValBuy3 > 0 THEN
							DO:
								MESSAGE "Курс покупки " + STRING(mValName3) + " не может превышать курс ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell3 <= xxx.rate-instr AND mValSell3 > 0 THEN
							DO:
								MESSAGE "Курс продажи " + STRING(mValName3) + " не может быть меньше курса ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.			
				END.	
		END.
	*/
	/*------------------------------------*/
	RUN browseld.p ("branch",
				   "",
				   "",
				   "branch-type",
				   4).
	/**/
	IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
	DO:
		/* идем по всем отмеченным подразделениям */
		FOR EACH tmprecid
			NO-LOCK:
		  FIND FIRST branch WHERE 
			RECID(branch) EQ  tmprecid.id
			NO-LOCK NO-ERROR.
			/**/
			IF AVAIL branch THEN
				DO:
					/* посмотрим нет ли на этом дне курсов валют, установленных позднее */
					/* 1. Тенге */					
					IF mValBuy1 > 0 OR mValSell1 > 0 THEN 
						DO:
							/**/
							FIND LAST yyy
								WHERE yyy.instr-code = mVal1
								AND ( yyy.rate-type = "Продажа" OR yyy.rate-type = "Покупка" )
								AND yyy.branch-id  = branch.branch-id
								AND yyy.instr-cat  = "currency"
								AND yyy.iratedatetime > SF
								AND yyy.iratedatetime <= CM
							NO-LOCK NO-ERROR.
							/**/
							IF AVAIL yyy THEN
								DO:
									MESSAGE "Есть курсы валют для " + mValName1 + ", установленные на эту дату, но позднее по времени!" VIEW-AS ALERT-BOX.
									RETURN.
								END.
						END.
						
					/* 2. Доллар США */
					IF mValBuy2 > 0 OR mValSell2 > 0 THEN 
						DO:
							/**/
							FIND LAST yyy
								WHERE yyy.instr-code = mVal2
								AND ( yyy.rate-type = "Продажа" OR yyy.rate-type = "Покупка" )
								AND yyy.branch-id  = branch.branch-id
								AND yyy.instr-cat  = "currency"
								AND yyy.iratedatetime > SF
								AND yyy.iratedatetime <= CM
							NO-LOCK NO-ERROR.
							/**/
							IF AVAIL yyy THEN
								DO:
									MESSAGE "Есть курсы валют для " + mValName2 + ", установленные на эту дату, но позднее по времени!" VIEW-AS ALERT-BOX.
									RETURN.
								END.						
							/**/
						END.

					/* 3. Евро */
					IF mValBuy3 > 0 OR mValSell3 > 0 THEN 
						DO:
							/**/
							FIND LAST yyy
								WHERE yyy.instr-code = mVal3
								AND ( yyy.rate-type = "Продажа" OR yyy.rate-type = "Покупка" )
								AND yyy.branch-id  = branch.branch-id
								AND yyy.instr-cat  = "currency"
								AND yyy.iratedatetime > SF
								AND yyy.iratedatetime <= CM
							NO-LOCK NO-ERROR.
							/**/
							IF AVAIL yyy THEN
								DO:
									MESSAGE "Есть курсы валют для " + mValName3 + ", установленные на эту дату, но позднее по времени!" VIEW-AS ALERT-BOX.
									RETURN.
								END.						
							/**/
						END.
					
					/* посмотрим нет ли уже такой записи */
					
					/* 1. "Покупка" 398 */
					IF mValBuy1 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal1
								AND zzz.rate-type = "Покупка"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName1) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValBuy1.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal1
										irate-time.rate-type  = "Покупка"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValBuy1
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt1
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.

					/* 2. "Продажа" 398 */
					IF mValSell1 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal1
								AND zzz.rate-type = "Продажа"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName1) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValSell1.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal1
										irate-time.rate-type  = "Продажа"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValSell1
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt1
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.


					/* 3. "Покупка" 840 */
					IF mValBuy2 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal2
								AND zzz.rate-type = "Покупка"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName2) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValBuy2.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal2
										irate-time.rate-type  = "Покупка"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValBuy2
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt2
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.


					/* 4. "Продажа" 840 */
					IF mValSell2 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal2
								AND zzz.rate-type = "Продажа"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName2) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValSell2.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal2
										irate-time.rate-type  = "Продажа"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValSell2
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt2
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.
						

					/* 5. "Покупка" 978 */
					IF mValBuy3 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal3
								AND zzz.rate-type = "Покупка"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName3) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValBuy3.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal3
										irate-time.rate-type  = "Покупка"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValBuy3
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt3
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.

						
					/* 6. "Продажа" 978 */
					IF mValSell3 > 0 THEN 
						DO:
							/**/
							FIND FIRST zzz
								WHERE zzz.instr-code = mVal3
								AND zzz.rate-type = "Продажа"
								AND zzz.branch-id  = branch.branch-id
								AND zzz.instr-cat  = "currency"
								AND zzz.iratedatetime = SF
							EXCLUSIVE-LOCK NO-ERROR.
							
							/* если есть - то спросим обновлять данные или нет? */
							IF AVAIL zzz THEN
								DO:
									mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mValName3) + ". Обновить?". 
									 
									MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
									TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
									  
									IF choiceMess = TRUE THEN
										DO:
											ASSIGN
											zzz.rate-instr = mValSell3.
											/**/
											VALIDATE zzz.
											/*
											RELEASE zzz.
											*/
										END.  
									
									IF LASTKEY EQ 27 THEN
										RETURN.
										
								END.
							ELSE
								/* иначе просто */
								DO:
									/* создаем запись */
									CREATE irate-time NO-ERROR.
									/* присваиваем значения */
									ASSIGN
										irate-time.instr-code = mVal3
										irate-time.rate-type  = "Продажа"
										irate-time.branch-id  = branch.branch-id
										irate-time.rate-instr = mValSell3
										irate-time.instr-cat  = "currency"
										irate-time.per 		  = mValCnt3
										irate-time.bound-summ = 0
										irate-time.iratedatetime = SF
									.
									/* сохраняем */
									VALIDATE irate-time.
									/*
									RELEASE irate-time.
									*/
								END.
							/**/
						END.	
					/**/
						
				END.
		END.
	END.
	/**/	
	IF LASTKEY EQ 27 THEN
		RETURN.
	ELSE
		MESSAGE "Установка курсов проведена успешно!" VIEW-AS ALERT-BOX TITLE "Информация".
END.

{intrface.del}          /* Выгрузка инструментария.  */    
