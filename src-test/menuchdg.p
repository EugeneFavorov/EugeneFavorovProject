/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: menubagoper.p
      Comment: Меню операци над портфелфми.
   Parameters:
         Uses:
      Used by:
      Created: 23/01/2011 Om
     Modified: 
*/

{pick-val.i} 
{globals.i}
{tmprecid.def &NGSH =  "LOCAL"}  /* Таблица отметок. */
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */
{intrface.get loan}
{loan.pro}
{intrface.get date}
{intrface.get separate}
 {topkind.def} 

DEF INPUT PARAM TABLE FOR tmprecid BIND.  /* Получаем таблицу по указателю. */

DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.

DEF VAR dateZay AS DATE NO-UNDO.
DEF VAR dateChdg AS DATE NO-UNDO.
DEF VAR typeChdg AS LOGICAL NO-UNDO INIT TRUE.
DEF VAR summChdg AS DECIMAL NO-UNDO FORMAT ">>>>>>>>>>>>>9.99".
DEF BUFFER bloan-cond FOR loan-cond.
DEF VAR vSurrLoanCond AS CHAR NO-UNDO.
DEF VAR vSurrLoanCond2 AS CHAR NO-UNDO.
DEF VAR osnComm AS DECIMAL NO-UNDO.
DEF VAR vCredOffs AS CHAR INIT "->" NO-UNDO.
DEF VAR vIntOffs AS CHAR INIT "->" NO-UNDO.
DEF VAR vAmt AS DECIMAL NO-UNDO.
DEF VAR vAmt2 AS DECIMAL NO-UNDO.
DEF VAR vDbSumDec AS DECIMAL NO-UNDO.
DEF VAR vCrSumDec AS DECIMAL NO-UNDO.
DEF VAR in-op-date AS DATE NO-UNDO.
DEF VAR in-cont-code AS CHAR NO-UNDO.
DEF VAR in-acct-db AS CHAR NO-UNDO.
DEF VAR in-acct-cr AS CHAR NO-UNDO.
DEF VAR in-nn AS DECIMAL NO-UNDO.


BLCK:
DO
ON ERROR    UNDO BLCK, LEAVE BLCK
ON ENDKEY   UNDO BLCK, LEAVE BLCK:

FOR EACH tmprecid NO-LOCK, FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

dateZay = loan.since.
dateChdg = loan.since.

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
IF AVAIL loan-acct THEN DO:
	in-acct-db = loan-acct.acct.
	RUN acct-pos IN h_base (loan-acct.acct, '', dateChdg, dateChdg, ?).
	summChdg = abs(sh-bal).
END.
ELSE summChdg = 0.

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'Кредит' NO-LOCK NO-ERROR.
IF AVAIL loan-acct THEN DO:
	in-acct-cr = loan-acct.acct.
END.


PAUSE 0.

DEFINE FRAME frame_date_codes 
         dateZay LABEL "Дата заявления"
	 dateChdg LABEL "Дата ЧДГ"	
	 typeChdg LABEL "Тип ЧДГ" FORMAT "Аннуитет/Срок"
	 summChdg LABEL "Сумма"
	 WITH 1  COL 1 DOWN
	 
WIDTH 50 CENTERED OVERLAY ROW 10 TITLE "Условия ЧДГ, договор " + loan.doc-ref.

  DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	dateZay
	dateChdg
	typeChdg
	summChdg

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   IF LASTKEY EQ KEYCODE("ESC") THEN
	RETURN.
   IF LASTKEY EQ KEYCODE("F1")
		THEN DO:
			CASE FRAME-FIELD:
			WHEN "dateZay" THEN
				DO:
				RUN calend.p.
				IF (LASTKEY EQ 13 OR
				LASTKEY EQ 10) AND
				pick-value NE ?
				THEN FRAME-VALUE = string(date(pick-value), "99/99/9999").
			END.
			WHEN "dateChdg" THEN
				DO:
				RUN calend.p.
				IF (LASTKEY EQ 13 OR
				LASTKEY EQ 10) AND
				pick-value NE ?
				THEN FRAME-VALUE = string(date(pick-value), "99/99/9999").
				FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
					IF AVAIL loan-acct THEN DO:
					RUN acct-pos IN h_base (loan-acct.acct, '', dateChdg, dateChdg, ?).
					summChdg = abs(sh-bal).
/*					summChdg:FRAME-VALUE = abs(sh-bal).  */
			END.

			END.
			WHEN "typeChdg" THEN
				DO:
					IF FRAME-VALUE = 'Срок' THEN FRAME-VALUE = 'Аннуитет'.
					ELSE FRAME-VALUE = 'Срок'.
				END.
			END CASE.

		END.
/*
   IF LASTKEY = KEYCODE("ENTER")  THEN 
	DO: 
		IF FRAME-FIELD = 'dateChdg' THEN DO:
		find last loan-acct of loan where loan-acct.acct-type = 'КредРасч' no-lock no-error.
			if avail loan-acct then do:
				RUN acct-pos IN h_base (loan-acct.acct, '', (dateChdg:SCREEN-VALUE in frame frame_date_codes), (dateChdg:SCREEN-VALUE in frame frame_date_codes), ?).
				summChdg = abs(sh-bal).
				summChdg:SCREEN-VALUE in frame frame_date_codes = STRING(abs(sh-bal)).
			end.
		END.
		APPLY LASTKEY.
	END.	
*/
		ELSE APPLY LASTKEY.

ON LEAVE OF dateChdg IN FRAME frame_date_codes
DO:
		FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
			IF AVAIL loan-acct THEN DO:
				RUN acct-pos IN h_base (loan-acct.acct, '', (dateChdg:SCREEN-VALUE IN FRAME frame_date_codes), (dateChdg:SCREEN-VALUE IN FRAME frame_date_codes), ?).
				summChdg = abs(sh-bal).
				summChdg:SCREEN-VALUE IN FRAME frame_date_codes = STRING(abs(sh-bal)).
			END.

END.

   END. /* EDITING: */
  END.  /* do on */	

   IF summChdg <= 0 THEN 
   DO:
      MESSAGE "Сумма ЧДГ 0" VIEW-AS ALERT-BOX.
      RETURN.		
   END.

   IF Chk_Date_Cat(dateChdg,"b") THEN 
   DO:
      MESSAGE "Операционный день " + string(dateChdg) + " закрыт, невозможно сделать ЧДГ" VIEW-AS ALERT-BOX.
      RETURN.		
   END.

   FIND FIRST loan-cond WHERE loan-cond.contract EQ loan.contract
                        AND loan-cond.cont-code EQ loan.cont-code
			AND loan-cond.since  EQ dateChdg NO-LOCK NO-ERROR.
   IF AVAIL loan-cond THEN 
   DO:
      MESSAGE "Уже есть условие на выбранную дату: " + string(dateChdg)  VIEW-AS ALERT-BOX.
      RETURN.		
   END.

/*   def var choice2 as logical .
   choice2 = false. */
   FIND FIRST op-entry WHERE op-entry.acct-db = in-acct-db 
	AND op-entry.acct-cr = in-acct-cr
	AND op-entry.amt-rub > 0
	AND op-entry.op-date = dateChdg
	AND op-entry.op-status >= "√"  NO-LOCK NO-ERROR.
  IF NOT AVAIL op-entry THEN DO:
         MESSAGE COLOR NORMAL SKIP "Не обнаружено проводки по гашению ОД" SKIP "Проводить ЧДГ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice2 AS LOGICAL.
         IF choice2 NE YES THEN RETURN.
  END.




IF loan.since <> dateChdg THEN DO:
			RUN l-calc2.p ("Кредит",       /* Назначение договора. */
               loan.cont-code,      /* Номер договора. */
               date(dateChdg),   /* Окончание договора + день для выполнения автом. */
               FALSE,		/* включать/не включать пересчет течений договора */
               TRUE).		/* выводить/ не выводить протокол на экран */
END.



    /*------- создаем условия на договоре -------*/
    FIND LAST bloan-cond WHERE   bloan-cond.contract  EQ loan.contract
                                 AND bloan-cond.cont-code EQ loan.cont-code
        /*                         AND bloan-cond.since     EQ loan.open-date */
	  NO-LOCK NO-ERROR.
    IF AVAIL bloan-cond THEN DO:

	 FIND FIRST loan-cond WHERE     loan-cond.contract  EQ loan.contract
                                 AND loan-cond.cont-code EQ loan.cont-code
                                 AND loan-cond.since   EQ dateChdg
	 EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL loan-cond THEN DO:

        CREATE loan-cond NO-ERROR.
        ASSIGN
         loan-cond.contract    = loan.contract
         loan-cond.cont-code   = loan.cont-code
         loan-cond.since       = dateChdg
         loan-cond.class-code  = bloan-cond.class-code
        .
      END.
    vSurrLoanCond = loan-cond.contract  + "," + loan-cond.cont-code + "," + STRING(loan-cond.since).
    vSurrLoanCond2 = bloan-cond.contract  + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since).


    /*------- Копируем обязательные доп.реквизиты на условия -------*/
    FOR EACH xattr WHERE xattr.class-code EQ loan-cond.class-code
                     AND xattr.mandatory  EQ YES 
                     AND xattr.system EQ NO
	       NO-LOCK:
			
		IF GetXAttrValue ("loan-cond",vSurrLoanCond,xattr.xattr-code) NE "" THEN
		    NEXT.
		UpdateSigns(loan-cond.class-code,
                  vSurrLoanCond,
                  xattr.xattr-code,
                  GetXattrValue("loan-cond",vSurrLoanCond2,xattr.xattr-code),
                  ?) NO-ERROR.
    END.

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "PayType", "ДосрПогаш", ?).
        vCredOffs = GetXattrValue("loan-cond",vSurrLoanCond2,"cred-offset").
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-offset", GetXattrValue("loan-cond",vSurrLoanCond2,"int-offset"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-mode",  GetXattrValue("loan-cond",vSurrLoanCond2,"cred-mode"), ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-offset", GetXattrValue("loan-cond",vSurrLoanCond2,"cred-offset"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "delay-offset", GetXattrValue("loan-cond",vSurrLoanCond2,"delay-offset"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "delay-offset-int", GetXattrValue("loan-cond",vSurrLoanCond2,"delay-offset-int"), ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-work-calend", GetXattrValue("loan-cond",vSurrLoanCond2,"cred-work-calend"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "end-date",         GetXattrValue("loan-cond",vSurrLoanCond2,"end-date"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "CondEndDate",      GetXattrValue("loan-cond",vSurrLoanCond2,"CondEndDate"), ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NDays", GetXattrValue("loan-cond",vSurrLoanCond2,"NDays"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NMonthes", GetXattrValue("loan-cond",vSurrLoanCond2,"NMonthes"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NYears", GetXattrValue("loan-cond",vSurrLoanCond2,"NYears"), ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "ИсклМес", GetXattrValue("loan-cond",vSurrLoanCond2,"ИсклМес"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "СхемаПлат", GetXattrValue("loan-cond",vSurrLoanCond2,"СхемаПлат"), ?).

    DEF VAR iLgot AS INT64 NO-UNDO.
    iLgot = 0.
    IF dateChdg - loan.open-date < 31 THEN DO:
        IF MONTH(dateChdg) <> MONTH(loan.open-date) THEN DO:
            IF DAY(dateChdg) < bloan-cond.int-date THEN iLgot = INT64(GetXattrValueEx("loan-cond",vSurrLoanCond2,"КолЛьгтПер","0")).
        END.
        ELSE iLgot = INT64(GetXattrValueEx("loan-cond",vSurrLoanCond2,"КолЛьгтПер","0")).        
    END.        
    
        
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "КолЛьгтПер",     STRING(iLgot), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "КолЛьгтПерПрц",  "0", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-mode",       GetXattrValue("loan-cond",vSurrLoanCond2,"int-mode"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-curr-next", GetXattrValue("loan-cond",vSurrLoanCond2,"cred-curr-next"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-curr-next",  GetXattrValue("loan-cond",vSurrLoanCond2,"int-curr-next"),  ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "DateDelay",      GetXattrValue("loan-cond",vSurrLoanCond2,"DateDelay"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "DateDelayInt",   GetXattrValue("loan-cond",vSurrLoanCond2,"DateDelayInt"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "ПродТрф",        GetXattrValue("loan-cond",vSurrLoanCond2,"ПродТрф"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "PayType", "ДосрПогаш", ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "PaySum", TRIM(STRING(summChdg,">>>,>>>,>>9.99")), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "АннуитПлат", 	   GetXattrValue("loan-cond",vSurrLoanCond2,"АннуитПлат"), ?) NO-ERROR.


        loan-cond.disch-type = bloan-cond.disch-type.
        loan-cond.cred-period = bloan-cond.cred-period.
	loan-cond.int-period  = bloan-cond.int-period.
	loan-cond.cred-date = bloan-cond.cred-date.
        loan-cond.cred-month = bloan-cond.cred-month.
	loan-cond.int-date = bloan-cond.int-date.

	osnComm = 0.
	DEF BUFFER xcomm-rate FOR comm-rate.
	FOR EACH xcomm-rate
	  WHERE xcomm-rate.kau = loan.contract + ',' + loan.cont-code
	    AND xcomm-rate.filial-id EQ shFilial
	    AND xcomm-rate.since = bloan-cond.since
	    AND xcomm-rate.currency EQ loan.currency
	    NO-LOCK:
	    IF xcomm-rate.commission = '%Кред' THEN DO:
	        osnComm = xcomm-rate.rate-comm.
	    END.
	    CREATE comm-rate.
	    ASSIGN
		comm-rate.commission = xcomm-rate.commission
		comm-rate.currency = xcomm-rate.currency
		comm-rate.min-value = xcomm-rate.min-value
		comm-rate.acct = xcomm-rate.acct
      		comm-rate.since = loan-cond.since
		comm-rate.rate-comm = xcomm-rate.rate-comm
		comm-rate.rate-fixed = xcomm-rate.rate-fixed
		comm-rate.period = xcomm-rate.period
		comm-rate.kau = xcomm-rate.kau
		comm-rate.filial-id = xcomm-rate.filial-id
      		comm-rate.branch-id = xcomm-rate.branch-id
      		.
	    VALIDATE comm-rate.
	END.

	/* расчет графика */

	DEF VAR vCredSumm AS DEC NO-UNDO.
	DEF VAR vAnnuitCorr AS INT NO-UNDO.
	DEF VAR vSummDepos AS DEC NO-UNDO.
	DEF VAR vFirstPeriod AS INT NO-UNDO.
	DEF VAR vPartAmount AS DEC NO-UNDO.
	DEF VAR vAnnSumm AS DEC NO-UNDO.
    def buffer tterm-obl for term-obl.

    RUN STNDRT_PARAM IN h_loan (
	loan.contract, loan.cont-code, 0,
	loan.since, OUTPUT vAmt, OUTPUT vDbSumDec, OUTPUT vCrSumDec).
    vCredSumm = vAmt + vAmt2.

	

        vAmt = 0.
        FIND FIRST term-obl WHERE 
		term-obl.cont-code = loan.cont-code
		AND term-obl.end-date = dateChdg 
		AND term-obl.idnt = 3
		AND term-obl.amt-rub <> 0
	        NO-LOCK NO-ERROR.
	IF AVAIL term-obl THEN vAmt = term-obl.amt-rub.
	
        vCredSumm = vCredSumm - summChdg - vAmt.

        IF typeChdg AND vCredSumm > 0 THEN DO:
	vAnnuitCorr  = INT(GetXattrValueEx("loan-cond",vSurrLoanCond,"АннуитКорр",?)).
	vSummDepos   = DEC(GetXAttrValueEx("loan",loan.contract + ',' + loan.cont-code,"Sum-depos","0")).
	vFirstPeriod = INT(GetXattrValueEx("loan-cond",vSurrLoanCond,"FirstPeriod","0")).
	vPartAmount  = DEC(GetXattrValueEx("loan-cond",vSurrLoanCond,"PartAmount","0")).

/*
    if /* datechdg - loan.open-date < 31 and  */ day(datechdg) <> loan-cond.cred-date then do:
	vAnnuitCorr  = 1.
	*/
	
	find first tterm-obl of loan where tterm-obl.amt-rub <> 0 
	   and (tterm-obl.idnt = 3 or tterm-obl.idnt = 1) 
	   and tterm-obl.end-date = datechdg no-lock no-error.
	if not avail tterm-obl then do: 
	   vAnnuitCorr  = 1.
	   
	
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "АннуитКорр", '1', ?) NO-ERROR.
    END.

	RUN CalcAnnuitet (loan.contract,
                              loan.cont-code,
                              loan-cond.since,
                              loan.end-date,
                              vCredSumm,
                              osnComm,
                              loan-cond.cred-date, 
                              loan-cond.cred-period,  
                              loan-cond.cred-month,
                              0,
                              STRING(LOOKUP(vCredOffs,"--,->,<-")),
                              vAnnuitCorr, 
                              vSummDepos,  
                              vFirstPeriod,
                              vPartAmount, 
                              OUTPUT vAnnSumm).
	/* Пересчитываем график платежей по процентам. */
	END.


    CALCTERM:
    DO ON ERROR UNDO, THROW:
	IF typeChdg THEN 
		UpdateSigns("loan-cond", vSurrLoanCond, "АннуитПлат", STRING(vAnnSumm), ?).
	RUN SetSysConf IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ",STRING(LOOKUP(vCredOffs,"--,->,<-"))).
	RUN SetSysConf IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ",STRING(LOOKUP(vIntOffs,"--,->,<-"))).
	RUN SetSysConf IN h_base ("ОБЯЗ. ПО ВОЗВРАТУ СДВИГ ОКОН.СРОКА", "1").
	RUN SetSysConf IN h_base ("ПЛАТ. ПО ПРОЦ. СДВИГ ОКОН.СРОКА", "1").
	/* Давим вывод на экран графиков для корректировки  */
	/*DEF VAR mRisk AS DEC NO-UNDO.
	mRisk = LnRsrvRate(loan.contract, loan.cont-code, loan.open-date).*/
	RUN SetSysConf IN h_base ("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН", "ДА").
	RUN mm-to.p(RECID(loan),
                  RECID(loan-cond),
                   vCredSumm,
                   1,
                   YES,
                   YES,
                   YES,
                   YES,
                   ? /*mRisk*/,
                   1) NO-ERROR.
	/* Чистим все... */
	RUN DeleteOldDataProtocol IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
	RUN DeleteOldDataProtocol IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").
	RUN DeleteOldDataProtocol IN h_base ("ОБЯЗ. ПО ВОЗВРАТУ СДВИГ ОКОН.СРОКА").
	RUN DeleteOldDataProtocol IN h_base ("ПЛАТ. ПО ПРОЦ. СДВИГ ОКОН.СРОКА").
	RUN DeleteOldDataProtocol IN h_base ("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН").
	IF ERROR-STATUS:ERROR THEN
    	    UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
    /* не работает, доп.реквизит оказывается пустым.
	DEF VAR vflagRecalc AS LOG NO-UNDO.
	vflagRecalc = false.
	FOR EACH term-obl
	 WHERE term-obl.contract EQ loan.contract
	   AND term-obl.cont-code EQ loan.cont-code
	   AND term-obl.idnt EQ 1
	   AND term-obl.end-date GE loan.open-date NO-LOCK:
	   IF vAnnSumm < term-obl.amt-rub THEN DO:
	    put unformatted "сумма аннуитета увеличена с " vAnnSumm " до " term-obl.amt-rub skip.
	    vAnnSumm = term-obl.amt-rub.
	    vflagRecalc = true.
	   END.
	END.
	IF vflagRecalc THEN UNDO, RETRY CALCTERM.
    */
    END.

  END.
                in-nn = 1.
                FIND LAST loan-int WHERE loan-int.contract = 'Кредит' 	
			AND loan-int.cont-code = loan.cont-code 
			AND loan-int.mdate = dateChdg NO-LOCK NO-ERROR.
		IF AVAIL loan-int THEN in-nn = loan-int.nn + 1.

						{empty tOpKindParams}     /* очистить таблицу параметров */
						mRes = TDAddParam("in-doc-ref",loan.doc-ref).
						mRes = TDAddParam("in-nn",string(in-nn)).
 						mRes = TDAddParam("in-cont-code",loan.cont-code).
						mRes = TDAddParam("in-open-date",STRING(loan.open-date,"99.99.9999")).
						mRes = TDAddParam("in-date-zay",STRING(dateZay,"99.99.9999")).
						mRes = TDAddParam("in-acctdb",in-acct-db).
						mRes = TDAddParam("in-acctcr",in-acct-cr).
						mRes = TDAddParam("in-curr",'').
						mRes = TDAddParam("in-filial",loan.filial-id).
						mRes = TDAddParam("in-sSum",STRING(summChdg)).
						mRes = TDAddParam("in-op-date",STRING(dateChdg,"99.99.9999")).

						RUN ex-trans.p(INPUT 'chdg_new', 
							INPUT date(dateChDg), 
							INPUT TABLE tOpKindParams, 
							OUTPUT mOK, 
							OUTPUT mMessage).
						IF NOT mOK THEN
						DO:
							{setdest.i}
							PUT UNFORMATTED 'документ не создан, ' + mMessage SKIP.
							{preview.i}
						END.
						ELSE DO:
							{setdest.i}
							PUT UNFORMATTED 'документ создан, сумма ' + string(summChdg) SKIP.
							{preview.i}
						END.


LEAVE.
END.
END.



{intrface.del}          /* Выгрузка инструментария. */ 
RETURN.			


