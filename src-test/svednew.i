/*                                               
        ��� "���� ����"
      Comment: ������⥪� ��� ���� ����譥 ���᫥���� ��業⮢ 
   Parameters: 
         Uses:
      Used by: ExcessProc.p
      Created: � ���� ⠪ 2013 KAM
     Modifier: 24/10/2014 KAU ��䠪�ਭ� ���� 
*/


DEF VAR listContType AS CHAR NO-UNDO.
DEF VAR listContType_str AS CHAR NO-UNDO.
DEF VAR listContType_str01 AS CHAR NO-UNDO.
DEF VAR listContType_invest AS CHAR NO-UNDO.
DEF VAR listContType_gold AS CHAR NO-UNDO.
DEF VAR listContType_hitr2 AS CHAR NO-UNDO.

/* ���᮪ ⨯�� �������, � ������ �� �� ��⨨ �㤥� ���䭠� �⠢�� */
listContType_str = "�����������,������멏���,�������,���,����,��������Ꮾ�,������������,������ᄮ���,������አ���,���Ꮻ�አ��,���Ꮻ�ᄢ".

/* ���᮪ ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1% */
listContType_str01 = "ChempPlus,LiderPlus".

/* ���᮪ ⨯�� �������, � ������ ���䭠� �⠢�� ������ �� ���� ���� */
listContType_invest = "Inves".

/* ���᮪ ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1%, ����� ���䭠� �⠢�� ������ �� ���� ���� */
listContType_gold = "gold,gold_card,gold_card_pr,gold_card_st,gold_dv,gold_dv_pr,gold_dv_st,gold_card_no,gold_card_po,gold_dv_no,gold_dv_po". 

/*�� ��ࢮ� ����⨨ �� ��ࢮ� ����⠫���樨 0.1% ����� �⠢�� ��㣠� ���䭠�*/
listContType_hitr2 = "���ᨮ���,���ᨮ�����,�������". /*���ᨮ���*/

/* ���᮪ ⨯�� �������, ��� ������ ���� ���� */
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
    /* ���᮪ ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1% */
    IF LOOKUP(loan.cont-type,listContType_str01) > 0 
    THEN DO:
        IF iDate < dateKap 
        THEN RETURN TRUE.
    END.
    
    /* ���᮪ ⨯�� ������� � ���䭮� �⠢��� �� �� ����⨨ */
    IF LOOKUP(loan.cont-type,listContType_str) > 0 
        OR LOOKUP(loan.cont-type,listContType_invest) > 0 
    THEN DO:
        IF iDate < loan.end-date 
        THEN RETURN TRUE.
    END.
    
    /* ⨯� ������� � ���䭮� �⠢��� �� �� ����⨨ �� 1�� ��� � ��稭�� � ��ண� ��ਮ�� (gold) */
    IF LOOKUP(loan.cont-type,listContType_gold) > 0 
    THEN DO:
    	IF iDate < dateKap THEN RETURN TRUE.
    	RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "pen-commi",
                                    OUTPUT shtrComm). /* ��� ���䭮� �����ᨨ */
    	/* ���䭠� */
    	shtr_st = ?.
    	FOR EACH commission WHERE commission.commission EQ shtrComm
    	       AND commission.currency EQ loan.currency
    	       AND commission.period <= (iDate - loan.open-date) NO-LOCK
        BREAK BY commission.period DESC:
    	    IF commission.period > 0 
    	    THEN DO:
        		/* � ��ࢮ� ��ਮ�� ���䭮� ��� */
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
                                    OUTPUT osnComm). /* ��� ���䭮� �����ᨨ */
    
    	/* �᭮���� */
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
    
    /*���ᨮ���*/
    IF LOOKUP(loan.cont-type,listContType_hitr2) > 0 THEN DO:
        IF iDate < dateKap THEN
            RETURN TRUE.
        RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                    loan.open-date,
                                    loan.open-date,
                                    "pen-commi",
                                    OUTPUT shtrComm). /* ��� ���䭮� �����ᨨ */
    
        /* ���䭠� */
        shtr_st = ?.
        FOR EACH commission WHERE commission.commission = shtrComm
            AND commission.currency = loan.currency
            AND commission.period <= (iDate - loan.open-date)
            NO-LOCK
            BREAK BY commission.period DESC:
            /* � ��ࢮ� ��ਮ�� ���䭠� �⠢�� ����! */
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
                                    OUTPUT osnComm). /* ��� ���䭮� �����ᨨ */
    
        /* �᭮���� */
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


/* ��⠥� ���⪨ �� ������� ��⠬ (op.contract-date) */
PROCEDURE my_acct_pos.
    DEF INPUT PARAM cAccountNumber  AS CHAR NO-UNDO. /* ����� ��� ������ */
    DEF INPUT PARAM dateLoan        AS DATE NO-UNDO. /* ��� ������ ������ */
    DEF INPUT PARAM dateRasch       AS DATE NO-UNDO. /* ��� ����樮����� ���  */
    DEF INPUT PARAM iCurrency       LIKE loan.currency NO-UNDO. /* ����� */
    DEF OUTPUT PARAM dOstatok       AS DEC NO-UNDO. /* ��室�騩 �����           */
    dOstatok = 0.
    
        FOR EACH op-entry WHERE 
            (op-entry.op-status = "�" OR op-entry.op-status = "��")
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

