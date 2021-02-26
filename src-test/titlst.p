/*
							 Банковская интегрированная система БИСквит
		 Filename: titlst.p
			Comment: Титульный лист
	 Parameters:
				 Uses:
			Used by:
			Created: ayv
		 Modified: pda
	 Настроечные параметры: БухЖур(b), БухЖур(o)
	 Классификаторы: elhran(elhran.i/def), ХозДок(hozdoc.i/def), ТитПодразд 
*/

{globals.i}
{tit-op.def}

DEF VAR iFilID      AS CHAR           NO-UNDO. /* код подразделения при вводе */
DEF VAR mBranchT    AS CHAR           NO-UNDO. /* branch-type */
DEF VAR mBranch     AS CHAR INIT ''   NO-UNDO. /* коды подразделения, для которых ищем проводки */
DEF VAR mParBr      AS CHAR INIT ''   NO-UNDO. /* филиал родителя */
DEF VAR mFilial     AS LOG            NO-UNDO. /* филиал/гб или кко */
DEF VAR mUserBr     AS CHAR		        NO-UNDO. /* др "отделение" пользователя */
DEF VAR mNameB      AS CHAR		        NO-UNDO. /* название отделения */
DEF VAR i           AS INT64          NO-UNDO. /* для определения сшива */
DEF VAR mSumm       AS DEC            NO-UNDO. /* для подсчета итогов в tt-day-itog */
DEF VAR vDateString AS CHAR           NO-UNDO. /* дата опердня прописью */
DEF VAR tStr        AS CHAR           NO-UNDO.
DEF VAR arhInd      AS CHAR           NO-UNDO. /* архивный индекс*/

DEF VAR cShiv AS CHAR INIT 'Агент,Подотчет,Хоздок,Докдн' NO-UNDO. /* для перебора сшивов */

/* для работы op-cash*/
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.
DEF VAR isCash  AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-op LIKE op.
DEF BUFFER b-branch FOR branch.

{getdate.i}

/* переменные для определения кассового документа */
{op-cash.def}

/* инициализация справочника эл.док-ов */
{elhran.def}

/* инициализация справочника хоз.док-ов */
{hozdoc.def}

/* возможность вводить код подразделения*/
PAUSE 0.

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune:
	UPDATE
		iFilID LABEL "Код подразделения" HELP "Введите код подразделения для отчета"   
	WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
	TITLE "[ Параметры Отчета ]".
END.
HIDE FRAME ftune NO-PAUSE.

/* добавление архивного индекса для 0300 (pda) */
IF iFilID EQ "0300" THEN
	arhInd = " 5089".

IF LASTKEY EQ KEYCODE("ESC") THEN
		RETURN.

/* ищем branch-type и по нему определяем    */
/* список подразделений для поиска проводок */
FIND FIRST branch WHERE branch.branch-id EQ iFilID
		 NO-LOCK NO-ERROR.
IF AVAILABLE(branch) THEN
DO:
	mBranchT = branch.branch-type.
	/* определяем коды подразделений для поиска */
	CASE mBranchT:
		WHEN '11' OR 
		WHEN '10' THEN DO:
			mFilial = TRUE.
			mBranch = branch.branch-id.
		END.
		WHEN '23' THEN DO:
			mFilial = FALSE.
			mBranch = branch.branch-id.
			mParBr  = branch.parent-id.
		END.
		OTHERWISE
			MESSAGE 'Неверный код подразделения!' VIEW-AS ALERT-BOX ERROR.
	END CASE.
END.

/* создаем буфер для op*/
FOR EACH op
	 WHERE op.op-date EQ end-date
	 AND   op.op-status BEGINS "√"
	 AND   op.acct-cat NE 'd'
NO-LOCK:
	CREATE tt-op.
	BUFFER-COPY op TO tt-op.
END.

IF  beg-date >= DATE(04/01/2017) THEN
DO:
  FOR EACH tt-op 
     WHERE tt-op.user-id BEGINS 'SERV' AND tt-op.op NE 94861997
  NO-LOCK:
    ASSIGN
    	tt-op.branch-id = tt-op.filial-id
    .
    IF tt-op.user-id EQ 'SERV0400' THEN
        tt-op.user-id = 'SERV0000' + tt-op.filial-id.
  END.
END.

IF  beg-date >= DATE(12/13/2015)
AND end-date <= DATE(02/05/2016) THEN
DO:
  FOR EACH tt-op 
     WHERE tt-op.user-inspector NE ''
       AND tt-op.user-id EQ '_SERV'
       AND tt-op.doc-type EQ '01'
       AND CAN-DO('*auto',tt-op.op-kind)
  NO-LOCK:
    ASSIGN
    	tt-op.branch-id = GetXAttrValueEx("_user",STRING(tt-op.user-inspector),"Отделение","")
    	tt-op.user-id = tt-op.user-inspector
    .
  END.
END.

IF  beg-date >= DATE(03/16/2017)
AND end-date <= DATE(03/23/2017) THEN
DO:
	FOR EACH tt-op 
		WHERE CAN-DO('SERV0500,SERV0300', tt-op.user-id)
		AND tt-op.op-kind EQ 'routMF_all'
		AND tt-op.filial-id EQ '0000'
	NO-LOCK:
		ASSIGN
			tt-op.branch-id = '0000'
			tt-op.user-id = 'SERV0000'
		.
	END.
END.

/* находим связь ОтдПол */
FIND FIRST xlink WHERE xlink.link-code EQ 'ОтдПол' NO-LOCK NO-ERROR.

/* отбираем проводки */
FOR EACH  tt-op 
	/*WHERE op.op-date EQ end-date
		AND   op.op-status BEGINS "√"
		AND   op.acct-cat NE 'd'*/
		WHERE (IF mFilial 
					THEN
					 tt-op.filial-id EQ mBranch
					ELSE
					 tt-op.filial-id EQ mParBr)
		AND   (IF mFilial
					THEN
					 TRUE
					ELSE
					 tt-op.branch EQ mBranch)
NO-LOCK:
		
		FOR EACH  op-entry 
				WHERE op-entry.op EQ tt-op.op
				AND   op-entry.op-date EQ end-date
		NO-LOCK:

			ASSIGN
					adb = op-entry.acct-db
					acr = op-entry.acct-cr
				.

			/* определение кассового документа */
			{op-cash.i} 

			/* электронный ли документ */
			{elhran.i}
			/* переносим документ SERV к контроллеру, если он в бумаге */
			IF isAuto AND NOT isElH AND tt-op.user-inspector NE '' THEN
				tStr = tt-op.user-inspector.
			ELSE
				ASSIGN
					tStr = tt-op.user-id
					tt-op.branch-id = tt-op.filial-id.
			
			/* ayv 01.09.2015 проводки IRBIS и BIS добавляются к SERV филиала */
			/*IF CAN-DO('*SERV*,IRBIS,BIS',tt-op.user-id) THEN*/
			/*IF CAN-DO('IRBIS,BIS',tt-op.user-id) THEN*/
			/*IF CAN-DO('IRBIS,BIS',tStr) THEN*/
			IF CAN-DO('IRBIS',tStr) THEN /* pda 26.02.2016 проводки BIS добавляются к ГБ Омск */
				tStr = 'SERV' + shFilial.

			/* находим связь ОтдПол на пользователе */
			IF AVAIL(xlink) THEN
				FIND FIRST links WHERE links.link-id EQ xlink.link-id
												 AND   links.source-id EQ tStr
												/* AND   links.beg-date LE end-date*/
												 AND  (links.end-date EQ ?
																OR    
															 links.end-date GE end-date)
						NO-LOCK NO-ERROR.

			CREATE tt-op-day.
			ASSIGN
						tt-op-day.op        = op-entry.op
						tt-op-day.op-cid    = RECID(op)
						tt-op-day.ope-cid   = RECID(op-entry)
						tt-op-day.kko       = IF AVAIL(links) AND AVAIL(xlink) THEN ENTRY(2,links.target-id) ELSE ''
						tt-op-day.currency  = IF op-entry.currency NE "" AND op-entry.currency NE ? THEN "VAL" ELSE "810"
						tt-op-day.acct-cat  = IF tt-op.acct-cat EQ 'b' THEN tt-op.acct-cat ELSE 'o'
						tt-op-day.razdel    = IF isCash THEN "k" ELSE "b" 
						tt-op-day.acct-db   = op-entry.acct-db
						tt-op-day.acct-cr   = op-entry.acct-cr
						tt-op-day.doc-num   = tt-op.doc-num
						tt-op-day.amt-rub   = IF op-entry.acct-db EQ ? THEN 0.00 ELSE op-entry.amt-rub
						tt-op-day.save-type = IF isElH THEN "e" ELSE "p"
						tt-op-day.auto-t    = IF isAuto THEN TRUE ELSE FALSE
						tt-op-day.shiv      = '-'
			.

			/* определение сшива */
			DO i = 1 TO NUM-ENTRIES(cShiv):
			
					{hozdoc.i &hoz-type = ENTRY(i,cShiv)}
					/* при ДокДн отмечаем все документы, не входящие в классификатор ХозДок */
					IF ENTRY(i,cShiv) = 'Докдн' THEN DO:
						IF NOT isHoz THEN DO:
								tt-op-day.shiv = ENTRY(i,cShiv).
								LEAVE.
						END.
					END.
					ELSE DO:
						IF isHoz THEN DO:
								tt-op-day.shiv = ENTRY(i,cShiv).
								LEAVE.
						END.
					END.

			END. /*do*/

		END. /* for each op-entry */

END. /* for each op */

/* список отделений для титульного листа */
FOR EACH tt-op-day NO-LOCK
		BREAK BY tt-op-day.kko
				BY tt-op-day.acct-cat
				BY tt-op-day.save-type 
				BY tt-op-day.currency
				BY tt-op-day.razdel
				BY tt-op-day.acct-db
				BY tt-op-day.acct-cr:

		IF FIRST-OF(tt-op-day.kko) THEN DO:
			
			/* заполняем поля из классификатора ТитПодразд */
			FIND FIRST code WHERE code.class EQ 'ТитПодразд'
											AND   code.code EQ tt-op-day.kko
				NO-LOCK NO-ERROR.

			IF AVAIL(code) THEN DO:
				CREATE tt-kko.
				ASSIGN 
						tt-kko.city-id = code.misc[1]
						tt-kko.city    = code.name
						tt-kko.kko     = code.code
						tt-kko.shiv    = code.val
				.
			END.

		END. /* if first-of */

END. /* for each tt-op-day */

/* подсчет итогов по отделениям */
mSumm = 0.

FOR EACH tt-op-day NO-LOCK
		BREAK BY TRIM(tt-op-day.kko + 
				 tt-op-day.acct-cat + 
				 tt-op-day.save-type + 
				 tt-op-day.currency +
				 STRING(tt-op-day.auto-t) + 
				 tt-op-day.razdel +
				 tt-op-day.shiv):

	IF FIRST-OF(TRIM(tt-op-day.kko + 
				tt-op-day.acct-cat + 
				tt-op-day.save-type + 
				tt-op-day.currency +
				STRING(tt-op-day.auto-t) + 
				tt-op-day.razdel +
				tt-op-day.shiv)) THEN
			mSumm = 0.

	mSumm = mSumm + tt-op-day.amt-rub.

	IF LAST-OF(TRIM(tt-op-day.kko + 
				 tt-op-day.acct-cat + 
				 tt-op-day.save-type + 
				 tt-op-day.currency +
				 STRING(tt-op-day.auto-t) + 
				 tt-op-day.razdel +
				 tt-op-day.shiv)) THEN DO:

		CREATE tt-day-itog.
		ASSIGN 
				tt-day-itog.kko       = tt-op-day.kko
				tt-day-itog.currency  = tt-op-day.currency
				tt-day-itog.acct-cat  = tt-op-day.acct-cat
				tt-day-itog.razdel    = tt-op-day.razdel
				tt-day-itog.save-type = tt-op-day.save-type
				tt-day-itog.auto-t    = tt-op-day.auto-t
				tt-day-itog.shiv      = tt-op-day.shiv
				tt-day-itog.itog      = mSumm
			.
		
	END. /* if last-of */

END. /* for each tt-op-day */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-day-itog NO-LOCK:
	PUT UNFORMATTED tt-day-itog.kko ";" tt-day-itog.currency ";" tt-day-itog.acct-cat ";" tt-day-itog.razdel ";" tt-day-itog.save-type ";" tt-day-itog.auto-t ";" tt-day-itog.shiv ";" tt-day-itog.itog SKIP.
END.
{preview.i &file-name = "111.log"} */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-op-day NO-LOCK:
	PUT UNFORMATTED tt-op-day.kko ";" tt-op-day.user-id ";" tt-op-day.currency ";" tt-op-day.acct-cat ";" tt-op-day.razdel ";" tt-op-day.doc-num ";" tt-op-day.save-type ";" tt-op-day.auto-t ";" tt-op-day.amt-rub ";" tt-op-day.shiv ";" tt-op-day.op SKIP.
END.
{preview.i &file-name = "111.log"} */

/* расчет общего итога */
{tititog.i}

vDateString = GetDateString(end-date).

/* вывод титульника в excel */
{titxls.i}