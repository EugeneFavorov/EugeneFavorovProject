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
DEF VAR mCrossRate AS LOGICAL INITIAL YES. /* Кросс курс */ 
DEF VAR mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* Значение */ 
/**/
DEF VAR mValName1 AS CHAR INIT "Тенге" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName2 AS CHAR INIT "Доллар США" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName3 AS CHAR INIT "Евро" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName4 AS CHAR INIT "Юань" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mVal1 AS CHAR INIT "398" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal2 AS CHAR INIT "840" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal3 AS CHAR INIT "978" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal4 AS CHAR INIT "156" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mValBuy1 AS DEC NO-UNDO.
DEF VAR mValBuy2 AS DEC NO-UNDO.
DEF VAR mValBuy3 AS DEC NO-UNDO.
DEF VAR mValBuy4 AS DEC NO-UNDO.
/**/
DEF VAR mValSell1 AS DEC NO-UNDO.
DEF VAR mValSell2 AS DEC NO-UNDO.
DEF VAR mValSell3 AS DEC NO-UNDO.
DEF VAR mValSell4 AS DEC NO-UNDO.
/**/
DEF VAR mValCnt1 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt2 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt3 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt4 AS DEC INIT 1 NO-UNDO.
/**/

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEFINE BUFFER xxx FOR instr-rate.
DEFINE BUFFER yyy FOR instr-rate.
DEFINE BUFFER zzz FOR instr-rate.

/**/

DEF VAR choiceMess AS LOGICAL INIT FALSE NO-UNDO.

DEFINE BUTTON button1 LABEL "Обновить".
DEFINE BUTTON button2 LABEL "Отменить".
DEFINE VAR mMESS AS CHARACTER VIEW-AS TEXT /*FORMAT "x(20)"*/ NO-UNDO.

/**/
/*---------------------------------*/
/* форма установки данных */

DEFINE FRAME fGet  
	/* верхняя строка */
	SPACE(15) 
	mDate 			LABEL			"Дата  "  		SKIP
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
	"|" SPACE(1) mValName4 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal4 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy4 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell4	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt4	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/	
	WITH /*WIDTH 62 */ /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 6
        TITLE COLOR BRIGHT-WHITE "[ Установка безналичных курсов валют ]" .

/**/
DEFINE FRAME Qwy
	mMESS		NO-LABEL /*FORMAT "X(25)"*/ SKIP(3)
	button1 	SPACE(5)
	button2
WITH /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9 
TITLE COLOR BRIGHT-WHITE "[ Редактирование курсов валют ]".	
		
/*---------------------------------*/
ON F1 OF mDate IN FRAME fGet  
DO:
   pick-value = "".
   DO WITH FRAME fGet :
      DO TRANSACTION:
		/**/
        RUN calend.p.
		/**/
		IF (lastkey eq 13 or lastkey eq 10) and pick-value ne ? then
			SELF:SCREEN-VALUE = string(date(pick-value), "99/99/9999").		
		/**/
      END.
   END.
   RETURN NO-APPLY.
END.

/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  PAUSE 0.
		UPDATE
			mDate
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
			mValName4
			mVal4
			mValBuy4
			mValSell4
			mValCnt4			
			/**/			
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* нужно проверить указанные значения */
	/*------------------------------------*/
	/* проверка на прошлый день */
	IF mDate < TODAY THEN
		DO:
			MESSAGE "Нельзя устанавливать безналичные курсы валют для прошлых дней!!!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.			
		END.
		
	/* все значения нулевые */	
	IF mValBuy1 = 0 AND mValBuy2 = 0 AND mValBuy3 = 0 AND mValBuy4 = 0
		AND mValSell1 = 0 AND mValSell2 = 0 AND mValSell3 = 0 AND mValSell4 = 0 THEN
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
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal1
				AND xxx.rate-type EQ "Учетный"
				AND xxx.since EQ mDATE
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
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal2
				AND xxx.rate-type EQ "Учетный"
				AND xxx.since EQ mDATE
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
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal3
				AND xxx.rate-type EQ "Учетный"
				AND xxx.since EQ mDATE
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
		
	/* 4. Юань */
	/* найдем курс ЦБ на данную дату по определенной валюте */
	IF mValBuy4 > 0 OR mValSell4 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal4
				AND xxx.rate-type EQ "Учетный"
				AND xxx.since EQ mDATE
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy4 >= xxx.rate-instr AND mValBuy4 > 0 THEN
							DO:
								MESSAGE "Курс покупки " + STRING(mValName4) + " не может превышать курс ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell4 <= xxx.rate-instr AND mValSell4 > 0 THEN
							DO:
								MESSAGE "Курс продажи " + STRING(mValName4) + " не может быть меньше курса ЦБ " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "Ошибка".
								UNDO, RETRY.							
							END.			
				END.	
		END.	
	*/	
	/*------------------------------------*/
	/**/
	IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
	DO:
		/**/
		RUN SetRatesWTF(INPUT "840",INPUT "БНалПок", INPUT mValBuy2,INPUT mValCnt2).
		RUN SetRatesWTF(INPUT "840",INPUT "БНалПр", INPUT mValSell2,INPUT mValCnt2).
		RUN SetRatesWTF(INPUT "978",INPUT "БНалПок", INPUT mValBuy3,INPUT mValCnt3).
		RUN SetRatesWTF(INPUT "978",INPUT "БНалПр", INPUT mValSell3,INPUT mValCnt3).
		/**/
		RUN SetRatesWTF(INPUT "156",INPUT "БНалПок", INPUT mValBuy4,INPUT mValCnt4).
		RUN SetRatesWTF(INPUT "156",INPUT "БНалПр", INPUT mValSell4,INPUT mValCnt4).
		RUN SetRatesWTF(INPUT "398",INPUT "БНалПок", INPUT mValBuy1,INPUT mValCnt1).
		RUN SetRatesWTF(INPUT "398",INPUT "БНалПр", INPUT mValSell1,INPUT mValCnt1).
		/**/
	END.
	/**/	
	IF LASTKEY EQ 27 THEN
		RETURN.
	ELSE
		MESSAGE "Установка курсов проведена успешно!" VIEW-AS ALERT-BOX TITLE "Информация".
END.

PROCEDURE SetRatesWTF:
	DEF INPUT PARAMETER val_code  AS CHARACTER.
    DEF INPUT PARAMETER rate_type AS CHARACTER.
	DEF INPUT PARAMETER set_value AS DECIMAL.
	DEF INPUT PARAMETER per_value AS DECIMAL.
	
		/**/
		IF set_value > 0 THEN 
			DO:
				/**/
				FIND FIRST zzz
					WHERE zzz.instr-code = val_code
					AND zzz.rate-type = rate_type
					AND zzz.instr-cat  = "currency"
					AND zzz.since = mDATE
				EXCLUSIVE-LOCK NO-ERROR.
				
				/* если есть - то спросим обновлять данные или нет? */
				IF AVAIL zzz THEN
					DO:
						mMESS = "На данный день установлен курс " + STRING(zzz.rate-instr) + " по валюте " + STRING(val_code) + ". Обновить?". 
						 
						MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
						TITLE "[ Редактирование курсов валют ]" UPDATE choiceMess.
						  
						IF choiceMess = TRUE THEN
							DO:
								ASSIGN
								zzz.rate-instr = set_value.
								/**/
								VALIDATE zzz.
								/**/
							END.  
						
						IF LASTKEY EQ 27 THEN
							RETURN.
							
					END.
				ELSE
					/* иначе просто */
					DO:
						/* создаем запись */
						CREATE instr-rate NO-ERROR.
						/* присваиваем значения */
						ASSIGN
							instr-rate.instr-code = val_code
							instr-rate.rate-type  = rate_type
							instr-rate.rate-instr = set_value
							instr-rate.instr-cat  = "currency"
							instr-rate.per 		  = per_value
							instr-rate.since 	  = mDATE
						.
						/* сохраняем */
						VALIDATE instr-rate.
						/**/
					END.
				/**/
			END.
		/**/
END.

{intrface.del}          /* Выгрузка инструментария.  */    
