/*                                               
        ОАО "Плюс банк"
      Comment: Библиотека для расчета излишне начисленных процентов 
   Parameters: 
         Uses:
      Used by: ExcessProc.p
      Created: в году так 2013 KAM
     Modifier: 24/10/2014 KAU Рефакторинг кода 
*/


DEF VAR listContType AS CHAR NO-UNDO.
DEF VAR listContType_str AS CHAR NO-UNDO.
DEF VAR listContType_str01 AS CHAR NO-UNDO.
DEF VAR listContType_invest AS CHAR NO-UNDO.
DEF VAR listContType_gold AS CHAR NO-UNDO.
DEF VAR listContType_hitr2 AS CHAR NO-UNDO.

/* Список типов вкладов, у которых при любом снятии будет штрафная ставка */
listContType_str = "НадежПлКарт,НадежныйПлюс,Надежный,Дюжина,Плюс,НадежПлюсПов,НадПлПовКарт,НадПлюсДовНе,НадПлюсКарНе,МаксПлюсКарт,МаксПлюсДв".

/* Список типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1% */
listContType_str01 = "ChempPlus,LiderPlus".

/* Список типов вкладов, у которых штрафная ставка зависит от даты снятия */
listContType_invest = "Inves".

/* Список типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1%, далее штрафная ставка зависит от даты снятия */
listContType_gold = "gold,gold_card,gold_card_pr,gold_card_st,gold_dv,gold_dv_pr,gold_dv_st,gold_card_no,gold_card_po,gold_dv_no,gold_dv_po". 

/*При первом изъятии до первой капитализации 0.1% далее ставка другая штрафная*/
listContType_hitr2 = "Пенсионный,ПенсионКарт,Накопит". /*пенсионный*/

/* Список типов вкладов, для которых есть расчет */
listContType = listContType_str + "," + listContType_str01 + "," + listContType_invest + "," + listContType_gold + "," + listContType_hitr2.


FUNCTION IsShtrIzVkl RETURNS LOGICAL
    ( INPUT r_loan AS RECID, INPUT iDate AS DATE, INPUT acct42 AS CHAR):
    
    DEF VAR dateKap AS DATE NO-UNDO.
    DEF VAR shtr_st AS DEC NO-UNDO.
    DEF VAR osn_st AS DEC NO-UNDO.
    DEF VAR shtrComm AS CHAR NO-UNDO.
    DEF VAR osnComm AS CHAR NO-UNDO.
    
    FIND FIRST loan WHERE RECID(loan) EQ r_loan NO-LOCK.
	dateKap = DATE_CORRECT(MONTH(loan.open-date),1,31,YEAR(loan.open-date)).
    /* Список типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1% */
    IF LOOKUP(loan.cont-type,listContType_str01) > 0 
    THEN DO:
        IF iDate < dateKap 
        THEN RETURN TRUE.
    END.
    
    /* Список типов вкладов с штрафной ставкой при любом изъятии */
    IF LOOKUP(loan.cont-type,listContType_str) > 0 
        OR LOOKUP(loan.cont-type,listContType_invest) > 0 
    THEN DO:
        IF iDate < loan.end-date 
        THEN RETURN TRUE.
    END.
    
    /* типы вкладов с штрафной ставкой при любом изъятии до 1ой кап и начиная со второго периода (gold) */
    IF LOOKUP(loan.cont-type,listContType_gold) > 0 
    THEN DO:
    	IF iDate < dateKap THEN RETURN TRUE.
    	RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "pen-commi",
                                    OUTPUT shtrComm). /* код штрафной коммисии */
    	/* штрафная */
    	shtr_st = ?.
    	FOR EACH commission WHERE commission.commission EQ shtrComm
    	       AND commission.currency EQ loan.currency
    	       AND commission.period <= (iDate - loan.open-date) NO-LOCK
        BREAK BY commission.period DESC:
    	    IF commission.period > 0 
    	    THEN DO:
        		/* в первом периоде штрафной нет */
        		FIND FIRST comm-rate
        		    WHERE comm-rate.commission EQ commission.commission
        			  AND comm-rate.since <= loan.open-date
        			  AND comm-rate.acct EQ acct42
        			  AND comm-rate.currency EQ commission.currency
        			  AND comm-rate.period EQ commission.period
    			NO-LOCK NO-ERROR.
        		IF AVAIL comm-rate 
        		THEN shtr_st = comm-rate.rate-comm.
        		ELSE
                    IF NOT AVAIL comm-rate THEN DO:
                        FOR EACH comm-rate WHERE comm-rate.commission EQ commission.commission
                                AND comm-rate.since <= loan.open-date
                                AND comm-rate.acct EQ '0'
                                AND comm-rate.currency EQ commission.currency
                                AND comm-rate.period EQ commission.period
        			    NO-LOCK BREAK BY comm-rate.since DESC:
                            shtr_st = comm-rate.rate-comm.
                            LEAVE.
                        END.
                    END.
    	    END.
    	    LEAVE.
    	END.
    
    	RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "commission",
                                    OUTPUT osnComm). /* код штрафной коммисии */
    
    	/* основная */
    	osn_st = ?.
    	FOR EACH commission WHERE commission.commission EQ osnComm
                AND commission.currency EQ loan.currency
                AND commission.period <= iDate - loan.open-date NO-LOCK
	    BREAK BY commission.period DESC:
    	    LEAVE.
    	END.
    	FIND FIRST comm-rate WHERE comm-rate.commission EQ commission.commission
                AND comm-rate.since <= loan.open-date
                AND comm-rate.acct EQ acct42
                AND comm-rate.currency EQ commission.currency
                AND comm-rate.period EQ commission.period
        NO-LOCK NO-ERROR.
    	IF AVAIL comm-rate 
    	THEN osn_st = comm-rate.rate-comm.
    	ELSE
            IF NOT AVAIL comm-rate THEN DO:
                FOR EACH comm-rate
    		    WHERE comm-rate.commission EQ commission.commission
    		      AND comm-rate.since <= loan.open-date
    		      AND comm-rate.acct EQ '0'
    		      AND comm-rate.currency EQ commission.currency
    		      AND comm-rate.period EQ commission.period
            NO-LOCK BREAK BY comm-rate.since DESC:
    		    osn_st = comm-rate.rate-comm.
    		    LEAVE.
    		END.
    	END.
    
    	IF osn_st <> shtr_st AND shtr_st <> ? THEN
    	    RETURN TRUE.
    END.
    
    /*Пенсионный*/
    IF LOOKUP(loan.cont-type,listContType_hitr2) > 0 THEN DO:
        IF iDate < dateKap THEN
            RETURN TRUE.
        RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "pen-commi",
                                    OUTPUT shtrComm). /* код штрафной коммисии */
    
        /* штрафная */
        shtr_st = ?.
        FOR EACH commission WHERE commission.commission = shtrComm
            AND commission.currency = loan.currency
            AND commission.period <= (iDate - loan.open-date)
            NO-LOCK
            BREAK BY commission.period DESC:
            /* в первом периоде штрафная ставка есть! */
            FIND FIRST comm-rate
                WHERE comm-rate.commission = commission.commission
                  AND comm-rate.since <= loan.open-date
                  AND comm-rate.acct = acct42
                  AND comm-rate.currency = commission.currency
                  AND comm-rate.period = commission.period
                NO-LOCK NO-ERROR.
            IF AVAIL comm-rate THEN
                shtr_st = comm-rate.rate-comm.
            ELSE
            IF NOT AVAIL comm-rate THEN DO:
                FOR EACH comm-rate
                    WHERE comm-rate.commission = commission.commission
                      AND comm-rate.since <= loan.open-date
                      AND comm-rate.acct = '0'
                      AND comm-rate.currency = commission.currency
                      AND comm-rate.period = commission.period
                    NO-LOCK BREAK BY comm-rate.since DESC:
                    shtr_st = comm-rate.rate-comm.
                    LEAVE.
                END.
            END.
            LEAVE.
        END.
    
        RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "commission",
                                    OUTPUT osnComm). /* код штрафной коммисии */
    
        /* основная */
        osn_st = ?.
        FOR EACH commission WHERE commission.commission = osnComm
            AND commission.currency = loan.currency
            AND commission.period <= iDate - loan.open-date
            NO-LOCK
            BREAK BY commission.period DESC:
            LEAVE.
        END.
        FIND FIRST comm-rate
            WHERE comm-rate.commission = commission.commission
              AND comm-rate.since <= loan.open-date
              AND comm-rate.acct = acct42
              AND comm-rate.currency = commission.currency
              AND comm-rate.period = commission.period
            NO-LOCK NO-ERROR.
        IF AVAIL comm-rate THEN
            osn_st = comm-rate.rate-comm.
        ELSE
        IF NOT AVAIL comm-rate THEN DO:
            FOR EACH comm-rate
                WHERE comm-rate.commission = commission.commission
                  AND comm-rate.since <= loan.open-date
                  AND comm-rate.acct = '0'
                  AND comm-rate.currency = commission.currency
                  AND comm-rate.period = commission.period
                NO-LOCK BREAK BY comm-rate.since DESC:
                osn_st = comm-rate.rate-comm.
                LEAVE.
            END.
        END.
        IF osn_st <> shtr_st AND shtr_st <> ? THEN
            RETURN TRUE.
    END.
    RETURN FALSE.
END.


/* считаем остатки по плановым датам (op.contract-date) */
PROCEDURE my_acct_pos.
    DEF INPUT PARAM cAccountNumber  AS CHAR NO-UNDO. /* номер счета клиента */
    DEF INPUT PARAM dateLoan        AS DATE NO-UNDO. /* дата открытия вклада */
    DEF INPUT PARAM dateRasch       AS DATE NO-UNDO. /* дата операционного дня  */
    DEF INPUT PARAM iCurrency       LIKE loan.currency NO-UNDO. /* валюта */
    DEF OUTPUT PARAM dOstatok       AS DEC NO-UNDO. /* исходящий дебет           */
    dOstatok = 0.
    
        FOR EACH op-entry WHERE 
            (op-entry.op-status = "√" OR op-entry.op-status = "√√")
            AND (op-entry.acct-db = cAccountNumber OR op-entry.acct-cr = cAccountNumber),
                FIRST op OF op-entry WHERE op.contract-date >= dateLoan AND op.contract-date <= dateRasch NO-LOCK:
                    IF op-entry.acct-db = cAccountNumber THEN DO:
                        IF iCurrency = '' THEN dOstatok = dOstatok - abs(op-entry.amt-rub).
                            ELSE dOstatok = dOstatok - abs(op-entry.amt-cur).
                    END.
                    ELSE DO:
                        IF iCurrency = '' THEN dOstatok = dOstatok + abs(op-entry.amt-rub).
                            ELSE dOstatok = dOstatok + abs(op-entry.amt-cur).
                    END.
        END.
END PROCEDURE.

