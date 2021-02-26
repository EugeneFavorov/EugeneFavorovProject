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

    
DEFINE VARIABLE mBranch AS CHARACTER  NO-UNDO.  /* подразделение */
DEFINE VARIABLE mCurr	AS CHARACTER  NO-UNDO.  /* валюта 1 */
DEFINE VARIABLE mCurr-2 AS CHARACTER  NO-UNDO.  /* валюта 2 */
DEFINE VARIABLE mCurrName AS CHARACTER NO-UNDO.  /* наименование валюты 1 */
DEFINE VARIABLE mCurrName-2 AS CHARACTER NO-UNDO.  /* наименование валюты 2 */
DEFINE VARIABLE mDate AS DATE INIT TODAY NO-UNDO.  /* дата */
DEFINE VARIABLE mTimes AS CHARACTER NO-UNDO.  /* время */
DEFINE VARIABLE mType AS CHARACTER  NO-UNDO.  /* тип операции */
DEFINE VARIABLE mCrossRate AS LOGICAL INITIAL YES. /* Кросс курс */ 
DEFINE VARIABLE mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* Значение */ 
DEFINE VARIABLE mZZ AS DECIMAL INITIAL 1.00 FORMAT ">>>999.99" NO-UNDO. /* За сколько */ 

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEFINE BUFFER xxx FOR irate-time.
DEFINE BUFFER yyy FOR irate-time.
DEFINE BUFFER zzz FOR irate-time.

/**/

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
   mCurr 			LABEL 			"Валюта     " 	FORMAT "X(3)"
   mCurrName		NO-LABEL						FORMAT "X(20)"		  	SKIP 
   mDate 			LABEL			"Дата       " 							SKIP
   mTimes			LABEL			"Время      " 	FORMAT "99:99:99" 		SKIP
   mType			LABEL			"Тип курса  " 	FORMAT "x(12)" 			SKIP
   mValue			LABEL			"Значение   " 							SKIP
   mZZ				LABEL			"За         " 							SKIP

   WITH WIDTH 40 /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Установка курсов валют ]" .

/**/
DEFINE FRAME Qwy
	mMESS		NO-LABEL /*FORMAT "X(25)"*/ SKIP(3)
	button1 	SPACE(5)
	button2
WITH /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9 
TITLE COLOR BRIGHT-WHITE "[ Редактирование курсов валют ]".	
		
/* Валюта 1 */

ON F1 OF mCurr IN FRAME fGet  
DO:
   pick-value = "".
   DO WITH FRAME fGet :
      DO TRANSACTION:
         RUN vokcurx.p (mType:SCREEN-VALUE,"",4).
         IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE "" THEN 
         DO:
            FIND FIRST currency WHERE
               currency.currency EQ pick-value
               NO-LOCK NO-ERROR.
            IF AVAILABLE currency THEN
            DO:
               SELF:SCREEN-VALUE      = currency.currency.
			   mCurrName:SCREEN-VALUE = currency.name-currenc.
            END.
         END.
      END.
   END.
   RETURN NO-APPLY.
END.

/* Валюта 1 */
ON LEAVE OF mCurr IN FRAME fGet 
DO:
   IF LASTKEY EQ 27 THEN
      RETURN.
	
   FIND FIRST currency WHERE
      currency.currency EQ SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE currency OR SELF:SCREEN-VALUE = "" THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      -1,
                      "Валюта " + SELF:SCREEN-VALUE + " не найдена"). 
      RETURN NO-APPLY.
   END.
   mCurr = SELF:SCREEN-VALUE.
   mCurrName:SCREEN-VALUE = currency.name-currenc.
END.

/* Тип курса */
ON F1 OF mType IN FRAME fGet 
DO:
   pick-value = "".
   FIND FIRST code WHERE 
          code.class EQ ""
      AND code.code EQ "Курсы ВОК" NO-LOCK NO-ERROR.
   IF AVAIL code THEN
   DO:
      DO TRANSACTION:
         RUN vokxtype.p (code.code,
                         code.code,
                         "Выбери вид курса валют",
                         4).
      END.
      IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE "" THEN 
      DO:
         SELF:SCREEN-VALUE = pick-value.
      END.
   END.
   RETURN NO-APPLY.
END.

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

/* Тип курса*/
ON LEAVE OF mType IN FRAME fGet  
DO:	  
		IF SELF:SCREEN-VALUE = "" THEN DO:
			MESSAGE "Необходимо указать тип курса!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			RETURN NO-APPLY.
		END.	
   mType = SELF:SCREEN-VALUE.
END.
/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mCurr
			mDate
			mTimes
			mType
			mValue
			mZZ
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
	
	IF mType = "" THEN DO:
		MESSAGE "Необходимо указать тип курса!" VIEW-AS ALERT-BOX TITLE "Ошибка".
		UNDO, RETRY.
	END.
	
	IF mValue = 0 THEN DO:
		MESSAGE "Значение должно быть отлично от нуля!" VIEW-AS ALERT-BOX TITLE "Ошибка".
		UNDO, RETRY.
	END.
	
	IF mZZ <> 1 AND mZZ <> 100 THEN DO:
		MESSAGE "Значение кол-ва единиц либо 1, либо 100." VIEW-AS ALERT-BOX TITLE "Ошибка".
		UNDO, RETRY.
	END.
	
	/* если Кросс курсы, то нужно проверить валюты, чтобы не были одинаковы!! */
	IF mType <> "" THEN
		DO:
			FIND FIRST code
				WHERE code.class = "Курсы ВОК"
				AND	  code.code = mType
				NO-LOCK NO-ERROR.
			
			IF AVAIL code AND code.val = mCurr THEN DO:
				MESSAGE "Неверно выбрана Валюта и Тип курса!" VIEW-AS ALERT-BOX TITLE "Ошибка".
				UNDO, RETRY.
			END.
		END.
		
	/* plus.vvv 29/04/2014 */
	/* проверка Покупки/Продажи с курсом ЦБ */
	
	IF mType EQ "Покупка" OR mType EQ "Продажа" THEN 
		DO:
			/* найдем курс ЦБ на данную дату по определенной валюте */
			FIND LAST xxx
				WHERE xxx.instr-cat EQ 'currency'
				AND xxx.instr-code EQ mCurr
				AND xxx.rate-type EQ 'Учетный'
				AND xxx.iratedate > DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00")
				AND xxx.iratedate < DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59")
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					IF mType EQ "Покупка" THEN 
						IF mValue >= xxx.rate-instr THEN
							DO:
								MESSAGE "Курс покупки " + STRING(mCurrName) + " не может превышать курс ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					.
					/**/
					IF mType EQ "Продажа" THEN 
						IF mValue <= xxx.rate-instr THEN
							DO:
								MESSAGE "Курс продажи " + STRING(mCurrName) + " не может быть меньше курса ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					.					
				END.
		END.
		
	/* проверка на прошлый день */
	
	IF mDate < DATE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 1, 10)) THEN
		DO:
			MESSAGE "Нельзя устанавливать курсы валют для прошлых дней!!!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.			
		END.
	
	/*------------------------------------*/
	RUN browseld.p ("branch",
				   "",
				   "",
				   "branch-type",
				   4).
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
					FIND LAST yyy
						WHERE yyy.instr-code = mCurr
						AND yyy.rate-type = mType
						AND yyy.branch-id  = branch.branch-id
						AND yyy.instr-cat  = "currency"
						AND yyy.iratedatetime > SF
						AND yyy.iratedatetime <= CM
					NO-LOCK NO-ERROR.
					/**/
					IF AVAIL yyy THEN DO:
						MESSAGE "Есть курсы валют, установленные на эту дату, но позднее по времени!" VIEW-AS ALERT-BOX.
						RETURN.
					END.
					
					/* посмотрим нет ли уже такой записи */
					FIND FIRST zzz
						WHERE zzz.instr-code = mCurr
						AND zzz.rate-type = mType
						AND zzz.branch-id  = branch.branch-id
						AND zzz.instr-cat  = "currency"
						AND zzz.iratedatetime = SF
					EXCLUSIVE-LOCK NO-ERROR.
					
					/* если есть - то спросим обновлять данные или нет? */
					IF AVAIL zzz THEN
						DO:
							/*
							DO ON ENDKEY UNDO, LEAVE:
							  pick-value = ?.
							  */
							mMESS = "Для подразделения " + STRING(zzz.branch-id) + " на данное время установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(mCurr) + ". Обновить?". 
							 
							MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
							TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess AS LOGICAL.
							  
							IF choiceMess = TRUE THEN
								DO:
									ASSIGN
									zzz.rate-instr = mValue.
									/**/
									VALIDATE zzz.
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
								irate-time.instr-code = mCurr
								irate-time.rate-type  = mType
								irate-time.branch-id  = branch.branch-id
								irate-time.rate-instr = mValue
								irate-time.instr-cat  = "currency"
								irate-time.per 		  = mZZ
								irate-time.bound-summ = 0
								irate-time.iratedatetime = SF
							.
							/* сохраняем */
							VALIDATE irate-time.
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
