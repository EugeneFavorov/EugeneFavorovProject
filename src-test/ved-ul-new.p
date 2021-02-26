{globals.i}
{sh-defs.i}     								/* Необходимы для подсчета остатков на счете */
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{intrface.get lngar}
{intrface.get chwch}
{intrface.get xobj}     						/* Библиотека для работы с объектами. */
{client.i}
{lshpr.pro}
{wordwrap.def}
{svarloan.def}
{navigate.def}
{loan_par.def &new = new}
{flt-file.i}
{omsk.pro} 
{param-dog.p}

def temp-table Kmpul-l NO-UNDO
	FIELD k 						AS INT						/* Порядковый номер записи*/
    FIELD cid 						AS CHARACTER				/* Поле CID'a */
    FIELD cust-cat 					AS CHARACTER				
    FIELD cust-id 					AS int64
    FIELD vidstr 					AS int64
    FIELD cont-type 				like loan.cont-type			/* Поле cont-type */
    FIELD loan-status 				LIKE loan.loan-status		/* Поле loan-status*/
    FIELD cr-stavka 				AS DECIMAL                  /* Ставка %*/
	FIELD cr-proc 					AS DECIMAL 
	FIELD penya 					AS DECIMAL 
	FIELD penya-proc 				AS DECIMAL 
    FIELD cont-code 				AS CHARACTER 
	FIELD org-name 					as CHARACTER
    FIELD product 					AS CHARACTER
    FIELD mAcctCrR 					AS CHARACTER
	FIELD proc-amt-bal-pros 		AS DECIMAL					/*Проц. бал. проср.*/
    FIELD proc-amt-vbal-pros 		AS DECIMAL 					/*Проц. внеб. проср.*/
	FIELD proc-amt-pros-sum 		AS DECIMAL 					/*Проц. проср.*/
	FIELD proc-shtraf 				AS DECIMAL 					/*Проц. штраф*/
	FIELD od-penya 					AS DECIMAL 					/*пеня од*/
	FIELD proc-penya-amt 			AS DECIMAL 					/*Проц. пени*/
    FIELD doc-ref 					AS CHARACTER 				/*номер дог*/
    FIELD open-date 				AS date 					/*Дата договора*/
    FIELD vid-date 					AS date 					/*Дата выдачи*/
    FIELD end-date 					AS date  					/*Дата окончания*/
    FIELD procstav 					AS CHARACTER 				/*Ставка %*/
    FIELD summ 						AS DECIMAL 					/*Сумма кредита (транша)*/
    FIELD ostcur 					like loan-var.balance 		/*Остаток ссудной задолженности (в валюте кредита)*/
    FIELD ostamt 					like loan-var.balance 		/*Остаток ссудной задолженности в рублях*/
    FIELD ostpros  					LIKE loan-var.balance 		/*Остаток просроченной ссудной задолженности*/
    FIELD ost91311 					AS DECIMAL 					/*Остаток 91311*/
    FIELD ost91414 					AS DECIMAL 					/*Остаток 91414*/
    FIELD ost91312 					AS DECIMAL 					/*Остаток 91312*/
	FIELD ost40817 					AS DECIMAL 					/*Остаток по 40817 */	
    FIELD nachproc 					AS DECIMAL 					/*Начисленные текущие проценты*/
    FIELD percentsum 				AS DECIMAL 					/*Просроченные проценты*/
    FIELD nachtekproc 				AS DECIMAL  				/*Начисленные текущие проценты на просроченную ссудную задолженность*/
    FIELD ost91317 					AS DECIMAL					/*Остаток на счете 91317*/
    FIELD ost91316 					AS DECIMAL 					/*Остаток на счете 91316*/
    FIELD kat_sud 					AS DECIMAL  				/*Категория качества ссуды*/
    FIELD st_rez 					AS DECIMAL 					/*Ставка резерва*/
    FIELD tip_kr 					AS DECIMAL 					/*Тип кредита*/
    FIELD ism-date 					AS date 					/*Дата изменения портфеля*/
	/*
    FIELD osn_dol 					AS DECIMAL 					/*Резерв осн. долг*/
    FIELD res_pr_dol 				AS DECIMAL 					/*Резерв просроч. осн. долг*/
	*/
    FIELD res_NKL 					AS DECIMAL					/*Резерв НКЛ*/
    FIELD day_pros 					AS INTEGER					/*Дней просрочки*/
    FIELD day_pros_180 				AS INTEGER 					/*Дней просрочки за полгода*/
    FIELD day_pros_tek 				AS INTEGER 					/*Дней тек. просрочки*/
    FIELD day_pros_nep 				AS INTEGER 					/*Дней тек. непогашенной просрочки*/	
    FIELD prosr 					AS INTEGER 					/*Просрочек*/
    FIELD prosr_180 				AS INTEGER 					/*Просрочек за полгода*/	
    FIELD day_pros_per 				AS date 					/*Дата первой непогашенной просрочки*/	
    FIELD rate 						AS DECIMAL 					/*Начисленные комисии*/
    FIELD pr_rate 					AS DECIMAL 					/*Просроченные комиссии*/
    FIELD rezerv 					AS DECIMAL 					/*Резерв по комиссиям*/ 
	FIELD comm 						AS DECIMAL 					/*Комиссия*/ 
	FIELD rko 						AS DECIMAL 					/*РКО*/ 
    FIELD acct-rs 					LIKE acct-pos.balance 		/*  */
    FIELD acct-vkl 					LIKE acct-pos.balance 		/* Ю╕кЁк■· Є·√ЁЇ */
    FIELD proc-amt-obor 			LIKE acct-pos.balance 		/* Яи■бї¤  ■√║вї¤■*/
    FIELD acct45515 				LIKE acct-pos.balance 		/* Остаток по 45515 */
    FIELD acct45518 				LIKE acct-pos.balance 		/* Остаток по 45518 */
    FIELD acct47425_45918 			LIKE acct-pos.balance 		/* Остатки по 47425_45918 */
    FIELD srok-plat 				LIKE loan.open-date 		/* Срок платежа */
    FIELD od-plat 					LIKE acct-pos.balance 		/* ЮФ ╕║№№Ё */
    FIELD proc-plat 				LIKE acct-pos.balance 		/*Яи■б ╕║№№Ё*/
    FIELD annu-plat 				LIKE acct-pos.balance 		/* Я√ЁкїЎ ╕║№№Ё */
    FIELD first-plat 				LIKE loan.open-date 		/* Первый платеж */
    FIELD adres-fact 				AS CHARACTER 				/* Фактический адрес клиента */
	FIELD adres-reg 				AS CHARACTER  				/* Адрес регистрации */
	FIELD telephone					AS CHARACTER				/* Телефон организации */
    FIELD tel 						AS CHARACTER 				/* Телефон */
	FIELD sred-ssud 				LIKE acct-pos.balance		/* Средняя задолжность */
	FIELD acctvklad					AS CHARACTER 				/* Счет вклада */
	FIELD proc-amt-nbal				AS DECIMAL					/* Проц. нбал. просроч. */
	FIELD acct-od					AS CHARACTER				/* ОД счет */
	FIELD acct-od-pros				AS CHARACTER				/* ОД просроч. счет */
	FIELD acct-rasch				AS CHARACTER				/* Расч. счет */
	FIELD ost0						AS DECIMAL					/* ОД сумма */
	FIELD ost4						AS DECIMAL					/* Проц. сумма */
.

def new shared stream puk.
def var fname as char  init "./ved-ul.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

DEF VAR mSumm          AS DECIMAL NO-UNDO.
DEF VAR mAdrJur		   AS CHARACTER NO-UNDO.
DEF VAR mAdrFact	   AS CHARACTER NO-UNDO.
DEF VAR mName          AS CHARACTER NO-UNDO.
DEF VAR oName		   AS CHARACTER NO-UNDO.
DEF VAR mAcctSsud 	   AS CHARACTER NO-UNDO.
DEF VAR mAcctCrR 	   AS CHARACTER NO-UNDO.
DEF VAR mAcctCrRBal    AS DECIMAL NO-UNDO.
DEF VAR mAcctCrRCur    AS CHARACTER NO-UNDO.
DEF VAR mCID 		   AS CHARACTER NO-UNDO.
DEF VAR rSum 		   AS DECIMAL NO-UNDO.
DEF VAR o91311 		   AS DECIMAL NO-UNDO.
DEF VAR o91312 		   AS DECIMAL NO-UNDO.
DEF VAR o91316 		   AS DECIMAL NO-UNDO.
DEF VAR o91317 		   AS DECIMAL NO-UNDO.
DEF VAR o91414 		   AS DECIMAL NO-UNDO.
DEF VAR o40817 		   AS DECIMAL NO-UNDO.
DEF VAR rOst 		   AS DECIMAL NO-UNDO.
DEF VAR vStrh 		   AS CHARACTER NO-UNDO.
DEF VAR rDate 		   AS DATE NO-UNDO.
DEF VAR mDay 		   AS INT NO-UNDO.
DEF VAR rDays 		   AS INT NO-UNDO.
DEF VAR kat_sud 	   AS INT NO-UNDO.
DEF VAR err-since 	   AS INT NO-UNDO.
DEF VAR dat 		   AS date NO-UNDO.
DEF VAR dog-date 	   AS date NO-UNDO.
DEF VAR beg-date	   AS date NO-UNDO.
def var choice 		   AS log no-undo.
DEF VAR g 		       AS LOGICAL NO-UNDO.
DEF VAR k 			   AS INT NO-UNDO INIT 1.
DEF VAR mFiltCr        AS CHARACTER  NO-UNDO.

DEF VAR res_NKL 	   AS DECIMAL.
DEF VAR rez 		   AS DECIMAL NO-UNDO.
DEF VAR osn 		   AS DECIMAL NO-UNDO.
DEF VAR st_rez 		   AS DECIMAL NO-UNDO.
DEF VAR nachproc 	   AS DECIMAL NO-UNDO.
DEF VAR pr_dol 		   AS DECIMAL NO-UNDO.
DEF VAR pr_rate 	   AS DECIMAL NO-UNDO.
DEF VAR nachtekproc    AS DECIMAL NO-UNDO.
DEF VAR prosr 	       AS INT NO-UNDO.
DEF VAR prosr_180 	   AS INT NO-UNDO.
DEF VAR cr-stavka 	   AS DECIMAL NO-UNDO.          
DEF VAR cr-proc 	   AS DECIMAL NO-UNDO.
DEF VAR penya 		   AS DECIMAL NO-UNDO.
DEF VAR penya-proc     AS DECIMAL NO-UNDO.
DEF VAR comm 		   AS DECIMAL NO-UNDO.
DEF VAR rko 		   AS DECIMAL NO-UNDO.
DEF VAR tel			   AS CHARACTER NO-UNDO.

/*затычки для  вызова RE_PARAM*/
DEF VAR a1 			   AS DECIMAL NO-UNDO.
DEF VAR a2 			   AS DECIMAL NO-UNDO.
DEF VAR par_0  		   AS DECIMAL NO-UNDO.
DEF VAR par_4  		   AS DECIMAL NO-UNDO.
DEF VAR par_13 		   AS DECIMAL NO-UNDO.
DEF VAR par_210 	   AS DECIMAL NO-UNDO.
DEF VAR par_710 	   AS DECIMAL NO-UNDO.
DEF VAR par_47423 	   AS DECIMAL NO-UNDO.
DEF VAR ostpros 	   AS DECIMAL NO-UNDO.
DEF VAR opOborot       AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE BUFFER b_l_acct FOR loan-acct.
/*-------------------------GetCountV---------------------------*/
/*===========================================================================*/
/*===== Параметры по договору ===============================================*/
/*===========================================================================*/
FUNCTION LN_GetParams RETURNS DEC
  (iContract AS CHAR,
   iContCode AS CHAR,
   iParams   AS CHAR,
   iDate     AS DATE):

   DEF VAR vParSumm AS DEC NO-UNDO.
   DEF VAR vSumma   AS DEC NO-UNDO.
   DEF VAR vCode    AS INT64 NO-UNDO.
   DEF VAR vDb      AS DEC NO-UNDO.
   DEF VAR vCr      AS DEC NO-UNDO.
   DEF VAR vCounter AS INT64 NO-UNDO.

   DO vCounter = 1 TO NUM-ENTRIES(iParams):

      vCode = INT64(ENTRY(vCounter,iParams)).

      RUN RE_PARAM IN h_Loan (vCode,
                              iDate,
                              iContract,
                              iContCode,
                              OUTPUT vParSumm,
                              OUTPUT vDb,
                              OUTPUT vCr).
      vSumma = vSumma + vParSumm.
   END.

   RETURN vSumma.

END FUNCTION.
/*===========================================================================*/
FUNCTION GetCountV RETURNS INTEGER
   (
    ipFiltDb  AS CHAR,
    iBeg-date AS DATE,
    iEnd-date AS DATE
    ):
DEFINE VARIABLE opCount AS INT64  INIT 0 NO-UNDO.
    FOR EACH op-entry NO-LOCK
        WHERE ipFiltDb EQ op-entry.acct-db
        AND op-entry.op-date GE iBeg-date
        AND op-entry.op-date LE iEnd-date
        AND (op-entry.op-status EQ "√" OR op-entry.op-status EQ "√√"):
			opCount = opCount + 1.
    END.
   RETURN opCount.
END FUNCTION.
/*-----------------------GetCountV-----------------------------*/
/*===========================================================================*/
/*===== Дней просросчки основного долга (ДниПросрСсуды) =====================*/
/*===========================================================================*/
FUNCTION LN_GetPrsDolgDays RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    DEF VAR vDb     AS DEC  NO-UNDO.
    DEF VAR vCr     AS DEC  NO-UNDO.
    DEF VAR vParam  AS DEC  NO-UNDO.
    DEF VAR vParam1 AS DEC  NO-UNDO.

    DEF BUFFER loan-int FOR loan-int.

    RELEASE loan-int.

    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.mdate    <= iDate
         AND (CAN-DO("7,13",STRING(loan-int.id-d)) OR
              CAN-DO("7,13",STRING(loan-int.id-k)))
    NO-LOCK BY loan-int.mdate DESCENDING:

       IF LN_GetParams (iContract,
                        iContCode,
                        "7,13",
                        loan-int.mdate - 1) = 0
       THEN RETURN (iDate - loan-int.mdate).
    END.

    RETURN 0.

END FUNCTION.
/*===========================================================================*/
/*===== Дней просрочки %% (ДниПросрПроц) ====================================*/
/*===========================================================================*/
FUNCTION LN_GetPrsProcDays RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    DEF VAR vDb     AS DEC  NO-UNDO.
    DEF VAR vCr     AS DEC  NO-UNDO.
    DEF VAR vParam  AS DEC  NO-UNDO.
    DEF VAR vParam1 AS DEC  NO-UNDO.

    DEF BUFFER loan-int FOR loan-int.

    RELEASE loan-int.

    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.mdate    <= iDate
         AND (CAN-DO("10,16",STRING(loan-int.id-d)) OR
              CAN-DO("10,16",STRING(loan-int.id-k)))
    NO-LOCK BY loan-int.mdate DESCENDING:

       IF LN_GetParams (iContract,
                        iContCode,
                        "10,16",
                        loan-int.mdate - 1) = 0
       THEN RETURN (iDate - loan-int.mdate).
    END.

    RETURN 0.

END FUNCTION.
/*===========================================================================*/
{getdate.i}
{setdest.i &col=170 }
{spinner.i "Отчет формируется..."}

beg-date = end-date.

FOR EACH loan WHERE loan.contract EQ 'Кредит'
				AND loan.cust-cat EQ 'Ю'	
				AND loan.open-date <= end-date
				AND (loan.close-date EQ ? OR loan.close-date >= end-date)
				/*AND loan.cont-code EQ '038-КЛ@0400 1'*/
NO-LOCK:   
	
	/*счет*/
	FIND FIRST loan-cond
	WHERE loan.cont-code EQ loan-cond.cont-code
	NO-LOCK NO-ERROR.
	
	/*Категория качества ссуды*/
	kat_sud = loan.gr-riska.
	st_rez = loan.risk.
	
	

	FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id 
	NO-LOCK NO-ERROR.
	IF AVAILABLE cust-corp THEN DO:    
		oName = cust-corp.cust-stat + " " + cust-corp.name-corp.
		
		/* телефон организации */
		FIND FIRST tmpsigns
		WHERE tmpsigns.file-name = "cust-corp"
		AND tmpsigns.surrogate = STRING(cust-corp.cust-id)
		AND tmpsigns.code = "tel"
		NO-LOCK NO-ERROR.
			IF AVAIL tmpsigns THEN
				tel = tmpsigns.xattr-value.
		/* сумма остатков по расчетному счету клиента */
		o40817 = 0.
		
		FOR EACH acct WHERE acct.cust-cat EQ loan.cust-cat
		and acct.cust-id EQ loan.cust-id
		and acct.contract EQ "Расчет"
		NO-LOCK:
			RUN acct-pos IN h_base (acct.acct,
								acct.currency,
								end-date,
								end-date,
								?).	
			o40817 = o40817 + ABSOLUTE(sh-bal).			
		END.
		/**/
		FOR EACH loan-acct  WHERE loan.cont-code EQ  loan-acct.cont-code
		and loan-acct.acct-type EQ "Кредит"
		no-lock:
				mAcctCrR = loan-acct.acct.
				mAcctCrR ="'" + TRIM(ENTRY(1, mAcctCrR, "@")).
		END.
	END.
	
	dat = end-date.
	CREATE Kmpul-l.
	
	/* вкладной счет */
	FOR EACH loan-acct  WHERE loan.cont-code EQ  loan-acct.cont-code
    and (loan-acct.acct-type EQ "loan-dps-t" OR loan-acct.acct-type EQ "loan-dps-p")
    no-lock:
			Kmpul-l.acctvklad = loan-acct.acct.
			Kmpul-l.acctvklad ="'" + TRIM(ENTRY(1, Kmpul-l.acctvklad, "@")).
	END.
	

	/*срочная задолженность*/
	RUN RE_PARAM IN h_Loan
        (0, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_0, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (4, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_4, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).		
	RUN RE_PARAM IN h_Loan
        (13, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_13, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	/*просроченная задолженность по основному долгу*/
	RUN RE_PARAM IN h_Loan
        (7, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT ostpros, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (27, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT o91316, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (19, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT o91317, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (21, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT osn, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (356, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT rez, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (46, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT pr_dol, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (5004, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT nachproc, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (10, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT nachtekproc, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (210, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_210, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (710, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_710, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (203, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT pr_rate, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).
	RUN RE_PARAM IN h_Loan
        (47423, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT par_47423, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).	
		
	/* 91311, 91312, 91414, res_NKL */
	o91311 = 0.
	FOR EACH loan-acct WHERE loan-acct.cont-code EQ loan.cont-code
	AND loan-acct.contract EQ loan.contract
	AND loan-acct.acct BEGINS '91311'
	NO-LOCK:
		RUN acct-pos IN h_base (loan-acct.acct,
								loan-acct.currency,
								end-date,
								end-date,
								?).	
		o91311 = o91311 + ABSOLUTE(sh-bal).
	END.
	
	o91312 = 0.
	FOR EACH loan-acct WHERE loan-acct.cont-code EQ loan.cont-code
	AND loan-acct.contract EQ loan.contract
	AND loan-acct.acct BEGINS '91312'
	NO-LOCK:
		RUN acct-pos IN h_base (loan-acct.acct,
							loan-acct.currency,
							end-date,
							end-date,
							?).	
		o91312 = o91312 + ABSOLUTE(sh-bal).
	END.
	
	o91414 = 0.
	FOR EACH loan-acct WHERE loan-acct.cont-code EQ loan.cont-code
	AND loan-acct.contract EQ loan.contract
	AND loan-acct.acct BEGINS '91414'
	NO-LOCK:
		RUN acct-pos IN h_base (loan-acct.acct,
							loan-acct.currency,
							end-date,
							end-date,
							?).	
		o91414 = o91414 + ABSOLUTE(sh-bal).	
	END.
    
	res_NKL = 0.
	FOR EACH loan-acct WHERE loan-acct.cont-code EQ loan.cont-code
	AND loan-acct.contract EQ loan.contract
	AND loan-acct.acct BEGINS '91316'
	NO-LOCK:
		RUN acct-pos IN h_base (loan-acct.acct,
							loan-acct.currency,
							end-date,
							end-date,
							?).	
		res_NKL = res_NKL + ABSOLUTE(sh-bal).	
	END.
	

/*   Запись данных по КД   */
	
FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредТ" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.proc-amt-bal = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.proc-amt-bal = ABSOLUTE(sh-val).
          mFiltCr =  acct.acct. 
      END.

/* 91604** */
FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредТВ" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.		
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.proc-amt-vbal = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.proc-amt-vbal = ABSOLUTE(sh-val).
          mFiltCr = mFiltCr + "," + acct.acct. 
      END.
      Kmpul-l.proc-amt-pros-sum = Kmpul-l.proc-amt-bal + Kmpul-l.proc-amt-vbal.		
/*---------------------------------------------------*/
/* Проц. нбал. просроч. - остатки по счетам 91604 с типом "КредПр%В" */
Kmpul-l.proc-amt-nbal = 0.

FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредПр%В" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.		
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.proc-amt-nbal = Kmpul-l.proc-amt-nbal + ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.proc-amt-nbal = Kmpul-l.proc-amt-nbal + ABSOLUTE(sh-val).
      END.
/*---------------------------------------------------*/
/* raschet schet */
FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРасч" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.	
		IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct-rs = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct-rs = ABSOLUTE(sh-val).
        END.

/*		
/* raschet vklad */

FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРасч1" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.	 
        IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct-vkl = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct-vkl = ABSOLUTE(sh-val).
		END.
*/
/*	 Остаток вклад - сумма остатков по депозитным счетам клиента (421,422 ..) */

	Kmpul-l.acct-vkl = 0.
	FOR EACH acct
	WHERE acct.cust-cat EQ loan.cust-cat
	AND acct.cust-id EQ loan.cust-id
	AND	(acct.acct BEGINS '421' OR acct.acct BEGINS '422')
	AND acct.close-date = ?,
		FIRST loan-acct
		WHERE loan-acct.acct = acct.acct
		AND (loan-acct.acct-type EQ "loan-dps-p" OR loan-acct.acct-type EQ "loan-dps-t")
	NO-LOCK:
		RUN acct-pos IN h_base (loan-acct.acct,
							loan-acct.currency,
							end-date,
							end-date,
							?).	
		Kmpul-l.acct-vkl = Kmpul-l.acct-vkl + ABSOLUTE(sh-bal).	
	END.

/*-----------------------------------------------------------------------------*/
/* получение оборотов */
opOborot = 0.

FOR EACH op-entry NO-LOCK
	WHERE op-entry.op-date GE Beg-Date
	AND op-entry.op-date LE end-date
	AND (op-entry.op-status EQ "√" OR op-entry.op-status EQ "√√")
	AND SUBSTRING(op-entry.acct-db, 1, 3) NE "459"
	AND	SUBSTRING(op-entry.acct-db, 1, 5) NE "91604"
	AND mFiltCr EQ op-entry.acct-cr
	:
	IF loan.currency EQ "" THEN
		opOborot = opOborot + op-entry.amt-rub.
	ELSE
		opOborot = opOborot + op-entry.amt-cur.
END.

Kmpul-l.proc-amt-obor = opOborot.
	
/* 45515 */
FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРез" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.			 
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct45515 = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct45515 = ABSOLUTE(sh-val).	
      END.
	  

	  

/* 45818 */
FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРез1" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct45518 = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct45518 = ABSOLUTE(sh-val).
      END.

/* 47425 */

FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРезП" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct47425_45918 = ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct47425_45918 = ABSOLUTE(sh-val).
      END.

/* 45918 */
Kmpul-l.acct47425_45918 = 0.

FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРезПр" AND
    b_l_acct.since     LE end-date
NO-LOCK NO-ERROR.					 
      IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    Kmpul-l.acct47425_45918 = Kmpul-l.acct47425_45918 + ABSOLUTE(sh-bal).
                ELSE
                    Kmpul-l.acct47425_45918 = Kmpul-l.acct47425_45918 +  ABSOLUTE(sh-val).
      END.

      /* Поиск первой незакрытой плановой сущности по договору */
      RUN RE_TERM_OBL IN h_loan (loan.contract,
							     loan.cont-code,
							     3,
							     end-date,
							     BUFFER term-obl).
      IF AVAIL term-obl
      THEN DO: 
          ASSIGN
              Kmpul-l.srok-plat = term-obl.end-date
              Kmpul-l.od-plat   = term-obl.amt-rub.
      END.

      RUN RE_TERM_OBL IN h_loan (loan.contract,
							     loan.cont-code,
							     1,
							     end-date,
							     BUFFER term-obl).
      IF AVAIL term-obl
      THEN ASSIGN
              Kmpul-l.proc-plat   = term-obl.amt-rub.
              annu-plat = Kmpul-l.od-plat + Kmpul-l.proc-plat.

    FIND FIRST term-obl WHERE
        term-obl.contract  EQ loan.contract AND
        term-obl.cont-code EQ loan.cont-code AND
        term-obl.idnt      EQ 3 
    NO-LOCK NO-ERROR.
      IF AVAIL term-obl
      THEN ASSIGN
              Kmpul-l.first-plat   = term-obl.end-date.

	FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK.
	/*
	Kmpul-l.adres-fact  = cust-corp.addr-of-low[1].
	*/	
	mAdrJur = "".
	mAdrFact = "".
	/* АдрЮр */
	RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрЮр",?,OUTPUT mAdrJur).
	/* АдрФакт */	
	RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрФакт",?,OUTPUT mAdrFact).
	/**/
	IF mAdrFact <> ? THEN
		Kmpul-l.adres-fact = mAdrFact.
	ELSE
		Kmpul-l.adres-fact = cust-corp.addr-of-low[1].
	/**/
	IF mAdrJur <> ? THEN
		Kmpul-l.adres-reg = mAdrJur.
	ELSE
		Kmpul-l.adres-reg = cust-corp.addr-of-low[1].
	/*
	Kmpul-l.sred-ssud = GetSredSsudDecToStr(loan.contract,loan.cont-code,Beg-date,end-date,loan.currency).		
	*/
/*
MESSAGE GetSredSsudDecToStr(loan.contract,loan.cont-code,Beg-date,end-date,loan.currency) VIEW-AS ALERT-BOX.
*/
	
/* 458 */
	prosr = 0.
	prosr_180 = 0.
	/*
	Kmpul-l.day_pros_tek = MAX( LN_GetPrsDolgDays ( loan.contract, loan.cont-code, end-date),  LN_GetPrsProcDays ( loan.contract, loan.cont-code, end-date)).
	
	IF Kmpul-l.day_pros_tek > 0 THEN DO:
		
		MESSAGE loan.cont-code VIEW-AS ALERT-BOX.
		RETURN.
		
	END.
	*/	
	
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредПр" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN DO:
			FIND FIRST acct WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
			IF AVAILABLE acct THEN DO:
				ASSIGN
				/*Дней просрочки*/
				Kmpul-l.day_pros = GetDayProsPos(acct.acct, acct.currency, loan.open-date, end-date).
				/*Дней тек. непогашенной просрочки*/	
                Kmpul-l.day_pros_nep = GetDayPros(acct.acct, acct.currency, loan.open-date, end-date).
				/*Дней тек. просрочки*/
                Kmpul-l.day_pros_tek = GetDayProsTek(acct.acct, acct.currency, loan.open-date, end-date).			
				/*Дней просрочки за полгода*/				
                Kmpul-l.day_pros_180 = GetDayProsPos(acct.acct, acct.currency, end-date - 180, end-date).
				
				IF Kmpul-l.day_pros_nep NE 0 THEN
					Kmpul-l.day_pros_per = end-date - Kmpul-l.day_pros_nep.
				.
				IF Kmpul-l.day_pros > 0 THEN DO:
					prosr = prosr + GetCountV(acct.acct, loan.open-date, end-date).
					prosr_180 = prosr_180 + GetCountV(acct.acct, end-date - 180, end-date).
				END.
				/*
				MESSAGE STRING(Kmpul-l.day_pros_180) + " * " + STRING(acct.acct) VIEW-AS ALERT-BOX.				
				*/
			END.
		END.		
	
	
	
/* 459 */
/*-------------------------*/	
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредПр%" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN DO:
			FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            IF AVAILABLE(acct) THEN
            DO:
			    RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
				mDay = GetDayProsPos(acct.acct, acct.currency, loan.open-date, end-date).
                IF mDay GT Kmpul-l.day_pros THEN
						ASSIGN
						Kmpul-l.day_pros = mDay.
						.

					mDay = GetDayPros(acct.acct, acct.currency, loan.open-date, end-date).

                IF mDay GT Kmpul-l.day_pros_nep THEN
					ASSIGN
                    Kmpul-l.day_pros_nep = mDay.
					IF Kmpul-l.day_pros_nep NE 0 THEN Kmpul-l.day_pros_per = end-date - Kmpul-l.day_pros_nep.
					.
					mDay = GetDayProsTek(acct.acct, acct.currency, loan.open-date, end-date).
                IF mDay GT Kmpul-l.day_pros_tek THEN
					ASSIGN
					Kmpul-l.day_pros_tek = mDay.
					.
                IF (end-date - 180) GT beg-Date THEN
					beg-Date = end-date - 180.
					mDay = GetDayProsPos(acct.acct, acct.currency, beg-Date, end-date).
                IF mDay GT Kmpul-l.day_pros_180 THEN
						ASSIGN
						Kmpul-l.day_pros_180 = mDay.
						.
				IF Kmpul-l.day_pros > 0 THEN DO:
					prosr = prosr + GetCountV(acct.acct, loan.open-date, end-date).
					prosr_180 = prosr_180 + GetCountV(acct.acct, end-date - 180 , end-date).
				END.				
            END.
        END.
	/*
	MESSAGE Kmpul-l.day_pros_180 VIEW-AS ALERT-BOX.	
	*/	
/*-----*/
		
/* 91604 */
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредПр%В" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN DO:
            FIND FIRST acct 
            WHERE acct.acct EQ b_l_acct.acct
            NO-LOCK NO-ERROR.
            IF AVAILABLE(acct) THEN
            DO:
                mDay = GetDayProsPos(acct.acct, acct.currency, loan.open-date, end-date).
                IF mDay GT Kmpul-l.day_pros THEN
						ASSIGN
						Kmpul-l.day_pros = mDay.
						.

					mDay = GetDayPros(acct.acct, acct.currency, loan.open-date, end-date).

					
                IF mDay GT Kmpul-l.day_pros_nep THEN
					ASSIGN
                    Kmpul-l.day_pros_nep = mDay.
					IF Kmpul-l.day_pros_nep NE 0 THEN Kmpul-l.day_pros_per = end-date - Kmpul-l.day_pros_nep.
					.
					mDay = GetDayProsTek(acct.acct, acct.currency, loan.open-date, end-date).
                IF mDay GT Kmpul-l.day_pros_tek THEN
					ASSIGN
                    Kmpul-l.day_pros_tek = mDay.
					.
                IF (end-date - 180) GT beg-Date THEN
                    beg-Date = end-date - 180.
					mDay = GetDayProsPos(acct.acct, acct.currency, beg-Date, end-date).
                IF mDay GT Kmpul-l.day_pros_180 THEN 
						ASSIGN
						Kmpul-l.day_pros_180 = mDay.
						.
				IF Kmpul-l.day_pros > 0 THEN DO:
					prosr = prosr + GetCountV(acct.acct, loan.open-date, end-date).
					prosr_180 = prosr_180 + GetCountV(acct.acct, end-date - 180 , end-date).
				END.
            END.
        END.
		
	   IF loan.since NE end-date THEN
	   DO:
			RUN RE_PARAM IN h_Loan
			(233, /*код параметра*/
			dat, /*дата*/
			loan.contract, /*назначение договора*/
			loan.cont-code, /*код договора*/
			OUTPUT Kmpul-l.proc-shtraf, /*значение параметра*/
			OUTPUT a1,
			OUTPUT a2).
			
			RUN RE_PARAM IN h_Loan
			(9, /*код параметра*/
			dat, /*дата*/
			loan.contract, /*назначение договора*/
			loan.cont-code, /*код договора*/
			OUTPUT Kmpul-l.od-penya, /*значение параметра*/
			OUTPUT a1,
			OUTPUT a2).
			
			RUN RE_PARAM IN h_Loan
			(12, /*код параметра*/
			dat, /*дата*/
			loan.contract, /*назначение договора*/
			loan.cont-code, /*код договора*/
			OUTPUT Kmpul-l.proc-penya, /*значение параметра*/
			OUTPUT a1,
			OUTPUT a2).
	   END.
	   /*
	   MESSAGE Kmpul-l.day_pros_180 VIEW-AS ALERT-BOX.
	   */
/*--------------*/
/* процентная ставка по кредиту */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "%Кред"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN cr-stavka = comm-rate.rate-comm.
	
/* процентная ставка по кредиту за проср. ОД */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "%КрПр"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN cr-proc = comm-rate.rate-comm.
	
/* ставка пени за проср. кредит */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "Пеня-К"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN penya = comm-rate.rate-comm.
	
/* ставка пени проср. % */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "Пеня%К"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN penya-proc = comm-rate.rate-comm.
	/* переопределяем как суммы
	/* комиссия */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "Комсч"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN comm = comm-rate.rate-comm.

	/* ком. РКО */
	FIND LAST comm-rate WHERE comm-rate.commission     EQ "%РКО"
    AND comm-rate.kau       EQ loan.contract + "," + loan.cont-code
    AND comm-rate.currency  EQ loan.currency
    AND comm-rate.acct      EQ "0"
    AND comm-rate.min-value EQ 0
    AND comm-rate.period    EQ 0
    AND comm-rate.since     LE loan.open-date
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE comm-rate THEN rko = comm-rate.rate-comm.
	*/
	/*--------------------------------------------------*/
	RUN RE_PARAM IN h_Loan
        (377, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT comm, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).	

	RUN RE_PARAM IN h_Loan
        (301, /*код параметра*/
        dat, /*дата*/
        loan.contract, /*назначение договора*/
        loan.cont-code, /*код договора*/
        OUTPUT rko, /*значение параметра*/
        OUTPUT a1,
        OUTPUT a2).	
		
	/*--------------------------------------------------*/
	/* ОД счет */
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "Кредит" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN
			Kmpul-l.acct-od = "'" + SUBSTRING(b_l_acct.acct, 1, 20).
	/* ОД просроч. счет */
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредПр" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN
			Kmpul-l.acct-od-pros = "'" + SUBSTRING(b_l_acct.acct, 1, 20).	
	/* Расчетный счет */
	FIND LAST b_l_acct WHERE
    b_l_acct.contract  EQ loan.Contract AND
    b_l_acct.cont-code EQ loan.Cont-Code AND
    b_l_acct.acct-type EQ "КредРасч" AND
    b_l_acct.since     LE end-date
	NO-LOCK NO-ERROR.
		IF AVAILABLE b_l_acct THEN
			Kmpul-l.acct-rasch = "'" + SUBSTRING(b_l_acct.acct, 1, 20).
	/*----------------*/
	/* сумма кредита */
	FIND FIRST term-obl 
	WHERE term-obl.contract  EQ loan.contract
    AND term-obl.cont-code EQ loan.cont-code
    AND term-obl.idnt      EQ 2
    /*AND term-obl.end-date  GE loan-cond.since */
	NO-LOCK NO-ERROR.
	  	
	ASSIGN
		/*
		Kmpul-l.mAcctCrR=mAcctCrR /*loan-acct.acct*/
		*/	
		Kmpul-l.cust-id   = loan.cust-id
		Kmpul-l.org-name  = oName	
		Kmpul-l.cont-code = loan.doc-ref
		Kmpul-l.cont-type = loan.cont-type
		Kmpul-l.end-date  = (IF loan.close-date <> ? THEN loan.close-date ELSE loan.end-date) 
		Kmpul-l.open-date = loan.open-date   
		Kmpul-l.vid-date = IF AVAIL op-entry THEN op-entry.op-date else loan.open-date
		Kmpul-l.product = if avail loan then /*GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "ПродКод", "")*/ LnInBagOnDate(loan.contract,loan.cont-code,end-date) else "1"
		Kmpul-l.cr-stavka = cr-stavka
		Kmpul-l.cr-proc = cr-proc
		Kmpul-l.penya = penya
		Kmpul-l.penya-proc = penya-proc
		Kmpul-l.summ = if avail term-obl then term-obl.amt else 0
		Kmpul-l.ostamt = par_0 + par_13
		Kmpul-l.ostpros = ostpros
		Kmpul-l.ost91316 = IF o91316 < 0 THEN -1 * o91316 ELSE o91316		
		Kmpul-l.ost91317 = IF o91317 < 0 THEN -1 * o91317 ELSE o91317		
		Kmpul-l.ost91311 = IF o91311 < 0 THEN -1 * o91311 ELSE o91311	
		Kmpul-l.ost91414 = IF o91414 < 0 THEN -1 * o91414 ELSE o91414	
		Kmpul-l.ost91312 = IF o91312 < 0 THEN -1 * o91312 ELSE o91312	
		Kmpul-l.ost40817 = o40817
		Kmpul-l.rezerv = IF rez < 0 THEN -1 * rez ELSE rez	
		/*
		Kmpul-l.osn_dol = IF osn < 0 THEN -1 * osn ELSE osn
		Kmpul-l.res_pr_dol = IF pr_dol < 0 THEN -1 * pr_dol ELSE pr_dol		
		*/	
		Kmpul-l.nachproc = nachproc
		Kmpul-l.kat_sud = kat_sud
		Kmpul-l.st_rez = st_rez
		Kmpul-l.nachtekproc = nachtekproc
		Kmpul-l.percentsum = par_210 + par_710 + nachtekproc
		Kmpul-l.pr_rate = pr_rate
		Kmpul-l.rate = par_47423
		Kmpul-l.res_NKL = res_NKL
		Kmpul-l.prosr = prosr
		Kmpul-l.prosr_180 = prosr_180
		Kmpul-l.ost0 = par_0
		Kmpul-l.ost4 = par_4
		Kmpul-l.telephone = tel
	.	
END.

IF AVAIL Kmpul-l THEN DO:
	PUT UNFORMATTED "Нажмите ESC для выгрузки отчета в BisPC" skip(1).
	output stream puk to value (fname)
	UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
	put stream puk unformatted
		/*
		"Счет"  delim
		*/
		"CID" delim
		"Наименование заемщика" delim
		"КД" delim
		"Тип КД" delim
		
		"Дата КД" delim
		"Дата выд." delim
		"Дата оконч." delim
		"Ставка" delim	
		"Ставка просроч." delim
		
		"Ставка пени ОД" delim
		"Ставка пени проц." delim
		"Резерв" delim
		
		"Портфель" delim
		
		"Сумма КД" delim
		
		"Вклад" delim
		
		"ОД сумма" delim
		
		"Проц. бал." delim
		"Проц. нбал." delim
		"Проц" delim
		
		"Комиссия" delim
		"Комиссия РКО" delim
		"ОДпросроч." delim
		"Проц. бал. просроч." delim
		"Проц. нбал. просроч." delim
		"Проц. просроч." delim
		"Проц. штраф" delim
		
		"ОД пени" delim
		"Проц. пени" delim
		
		"Остаток 40817" delim
		"Остаток вклад" delim
		"Проц. получено" delim
		
		"РВПС ОД" delim
		"РВПС ОД просроч." delim
		"РВПС" delim		
		
		"Вклад счет" delim
		/*+++++++++++++++++++*/
		"ОД счет" delim
		"ОД просроч. счет" delim
		"Расч. счет" delim
		/*+++++++++++++++++++*/
		"Дней тек. просрочки" delim	
		"Дней тек. непогашенной просрочки" delim			
		
		"Дней просроч. за последние 180 дней" delim
		"Кол-во просроч." delim
		"Кол-во просроч. за последние 180 дней" delim		
		"Платеж дата" delim
		"ОД сумма" delim
		"Проц. сумма" delim
		"Платеж сумма" delim
		"Первый платеж" delim
		"Адрес рег." delim
		"Адрес факт." delim
		"Тел." delim
		/*
		"Средняя задолженность" delim
		*/
		"Остаток 91311" delim
		"Остаток 91312" delim
		"Остаток 91414" delim
		"Остаток на счете 91317" delim
		"Остаток на счете 91316" delim
		"Резерв НКЛ" delim
		/*
		"" delim
		
		"Остаток ссудной задолж. в рублях" delim
		
		
		
		"Нач. тек. %" delim
		
		"Нач. тек. % на проср. ссудную задолженность" delim

		"Категория качества ссуды" delim
		"Тип кредита" delim
		
		/*
		"Резерв осн. долг" delim	
		"Резерв просроч. осн. долг" delim	
		*/
		
		"Дней просрочки" delim	


		"Дата первой непогашенной просрочки" delim
		"Начисленные ком." delim	
		"Просроченные ком." delim
		"Резерв по ком."    delim	
		"Остаток РС" delim

		"Проц. сумма" delim
		*/
	eol.

	for each Kmpul-l no-lock by Kmpul-l.k by Kmpul-l.open-date:
		IF Kmpul-l.cust-id <> 0 THEN DO:		
		put stream puk unformatted
			/*
			Kmpul-l.mAcctCrR delim	
			*/
			Kmpul-l.cust-id delim
			Kmpul-l.org-name delim
			/*
			ENTRY(1,Kmpul-l.cont-code,"@") delim
			*/
			Kmpul-l.cont-code delim
			
			Kmpul-l.cont-type delim
			Kmpul-l.open-date delim
			Kmpul-l.vid-date delim
			Kmpul-l.end-date delim 
			Kmpul-l.cr-stavka "%" delim
			Kmpul-l.cr-proc "%" delim
			Kmpul-l.penya "%" delim
			Kmpul-l.penya-proc "%" delim
			Kmpul-l.st_rez "%" delim
			
			Kmpul-l.product delim 
			Kmpul-l.summ delim
			"" delim
			/*
			Kmpul-l.od-plat delim
			*/
			Kmpul-l.ostamt  delim

			Kmpul-l.proc-amt-bal-pros delim
			Kmpul-l.proc-amt-vbal-pros delim
			Kmpul-l.proc-amt-pros-sum delim			
			
			Kmpul-l.comm delim
			Kmpul-l.rko delim
			
			Kmpul-l.ostpros delim
			Kmpul-l.percentsum delim
			Kmpul-l.proc-amt-nbal delim
			Kmpul-l.percentsum + Kmpul-l.proc-amt-nbal  delim
			Kmpul-l.proc-shtraf delim
			
			Kmpul-l.od-penya delim
			Kmpul-l.proc-penya-amt delim
			
			Kmpul-l.ost40817 delim
			Kmpul-l.acct-vkl delim
			Kmpul-l.proc-amt-obor delim

			Kmpul-l.acct45515 delim
			Kmpul-l.acct45518 delim
			Kmpul-l.acct47425_45918 delim

			Kmpul-l.acctvklad delim

			Kmpul-l.acct-od delim				/* ОД счет */
			Kmpul-l.acct-od-pros delim			/* ОД просроч. счет */
			Kmpul-l.acct-rasch delim			/* Расч. счет */			
			/*++++++++++++++++++*/
			Kmpul-l.day_pros_tek delim
			Kmpul-l.day_pros_nep delim
			/*++++++++++++++++++*/
			Kmpul-l.day_pros_180 delim
			Kmpul-l.prosr delim
			Kmpul-l.prosr_180 delim
			Kmpul-l.srok-plat delim
			Kmpul-l.ost0 delim
			Kmpul-l.ost4 delim
			Kmpul-l.annu-plat delim
			Kmpul-l.first-plat delim
			Kmpul-l.adres-reg delim
			Kmpul-l.adres-fact delim
			Kmpul-l.telephone delim
			/*
			Kmpul-l.sred-ssud 
			*/
			
			Kmpul-l.ost91311 delim 
			Kmpul-l.ost91312 delim
			
			Kmpul-l.ost91414 delim
			Kmpul-l.ost91317 delim
			Kmpul-l.ost91316 delim
			Kmpul-l.res_NKL delim
			
			/*
			"" delim
								
			Kmpul-l.nachproc delim
			
			Kmpul-l.nachtekproc delim

			Kmpul-l.kat_sud delim
			Kmpul-l.cont-type delim
			
			/*
			Kmpul-l.osn_dol delim		
			Kmpul-l.res_pr_dol delim	
			*/			
			
			Kmpul-l.day_pros delim

			Kmpul-l.day_pros_per delim
			Kmpul-l.rate delim
			Kmpul-l.pr_rate delim
			Kmpul-l.rezerv delim
			Kmpul-l.acct-rs delim
			
			Kmpul-l.proc-plat delim
			*/
			eol.
		END.
	end.
	output stream puk close.
	MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.
	g = true.
	{preview.i &col=170}
END.
ELSE DO:
	MESSAGE "За указанный период не было открыто ни одного КД на ЮЛ" VIEW-AS ALERT-BOX.
	PUT UNFORMATTED "Для выхода нажмите ESC" skip(1).
END.

IF g THEN
	RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}
