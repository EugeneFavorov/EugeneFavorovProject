/*
               ОАО "Плюс Банк"
    Copyright: 
     Filename: kredprodtranz.p
      Comment: Ищет сумму которую следует списать с клиента по договору
   Parameters: 
         Uses:
      Used by:
      Created: kau
     Modified: 
*/

{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}
{pp-corr.p}

DEFINE INPUT PARAMETER iDate  AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER iOprid AS RECID NO-UNDO.

DEF VAR acctBal    AS DEC  NO-UNDO.
DEF VAR maxacctBal AS DEC  NO-UNDO.
DEF VAR sumKom     AS DEC  NO-UNDO.
DEF VAR vSummSp    AS DEC  NO-UNDO.
DEF VAR vAcct      AS CHAR NO-UNDO.
DEF VAR vAcctFirst AS CHAR NO-UNDO.
DEF VAR iDateN     AS DATE NO-UNDO.
DEF VAR iDateX     AS DATE NO-UNDO.



iDateN = DATE("01/" + STRING(MONTH(iDate)) + '/' + STRING(YEAR(iDate))).
MESSAGE "Создавать документ в случае отсутствия средств?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "" UPDATE choice AS LOGICAL.
IF choice EQ ?
THEN LEAVE.
RUN SetSysConf IN h_base ("пакетсозддок", STRING(choice)).



FOR EACH cust-corp WHERE cust-corp.date-out > iDate OR cust-corp.date-out EQ ?
                   /*AND cust-corp.cust-id = 7852*/
NO-LOCK:
	vSummSp = 0.
	maxAcctBal = 0.
	FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
		             AND tmpsigns.code      EQ 'Пакет'
			     AND tmpsigns.surrogate EQ string(cust-corp.cust-id)
			     AND tmpsigns.since < iDateN
                             /*AND (((tmpsigns.since < iDateN) AND (tmpsigns.xattr-value NE 'Стартовый+')) OR ((tmpsigns.since >= date("01/01/2017")) AND (tmpsigns.xattr-value EQ 'Стартовый+')))*/
	NO-LOCK NO-ERROR.

	IF NOT AVAIL tmpsigns OR tmpsigns.xattr-value EQ ''
	THEN NEXT.

/*/*===================== Стартовый+ =====================*/
        iDateX = date_correct(month(tmpsigns.since), 3, 1, year(tmpsigns.since)).
        IF tmpsigns.xattr-value = "Стартовый+"
           AND iDate < iDateX
           AND tmpsigns.since <= date("31/12/2016")
        THEN 
             DO:
                 /*MESSAGE string(iDate) " " string(iDateX) " " string("Комиссию НЕ БЕРЁМ!") view-as alert-box.*/
                 NEXT.
             END.
/*========================= end ========================*/*/

	FIND FIRST commission WHERE commission.commission BEGINS "Pac"
				AND commission.name-comm[1] BEGINS 'Пакет услуг ' + tmpsigns.xattr-value
	NO-LOCK NO-ERROR.
	FIND LAST comm-rate WHERE comm-rate.commission EQ commission.commission
	                      AND comm-rate.since <= iDate
	NO-LOCK NO-ERROR.
	IF AVAIL comm-rate THEN sumKom = comm-rate.rate-comm.
	FOR EACH acct WHERE acct.cust-cat    EQ 'Ю'
			AND acct.cust-id     EQ cust-corp.cust-id
                        AND acct.filial-id   EQ shFilial
         		AND acct.contract    EQ 'Расчет'
			AND acct.currency    EQ ''
			AND (acct.close-date EQ ? OR acct.close-date > iDate)
	NO-LOCK
	BREAK BY acct.close-date BY acct.acct:

            FIND FIRST signs WHERE signs.file-name  EQ 'acct' 
                               AND signs.surrogate  EQ acct.acct + "," + acct.currency
                               AND signs.code       EQ "КорпКарт"
                               AND signs.code-value EQ "Да" NO-LOCK NO-ERROR.
            IF AVAIL signs THEN next.

	    FOR EACH op-entry WHERE op-entry.op-date >= iDateN
                                AND op-entry.op-date <= iDate
	                        AND op-entry.acct-db EQ acct.acct
	                        AND op-entry.acct-cr BEGINS '47423'
	                        AND NOT (op-entry.op-status BEGINS 'А')
	        NO-LOCK:
	        FIND FIRST op WHERE op.op EQ op-entry.op NO-LOCK.
	        IF AVAIL op AND op.op-kind EQ '1617'
	        THEN DO:
	            vSummSp = vSummSp + op-entry.amt-rub.
	        END.
	    END. 
		RUN acct-pos IN h_base (acct.acct, '', iDate, iDate, ?).
		acctBal = abs(sh-bal).
		vSummSp = SumKom - vSummSp.
		
                IF acctBal >= sumKom
		THEN DO:
			vAcct = acct.acct.
			LEAVE.
		END.
		
                IF maxAcctBal <= acctBal
                THEN DO:
                         vAcctFirst = acct.acct.
                END.
	END.

        /*message "vSummSp =" string(vSummSp) "SumKom =" string(SumKom) "vAcct =" string(vAcct) view-as alert-box.*/

	IF acctbal < sumKom THEN vAcct = vAcctFirst.
        IF vSummSp <= 0 THEN NEXT.

        RUN SetSysConf IN h_base ("пакетсуммсп", STRING(vSummSp)).
        RUN SetSysConf IN h_base ("пакетсумм",   STRING(sumKom)).
        RUN SetSysConf IN h_base ("пакетсчет",   STRING(vAcct)).
        RUN g-trans.p (iDate,iOprid).

/*/*===================== fev =====================*/
IF tmpsigns.xattr-value <> ""
THEN
  DO:
    iDateX = date_correct(month(tmpsigns.since), 3, 1, year(tmpsigns.since)).

    IF tmpsigns.xattr-value = "Стартовый+"
       AND iDate < iDateX
       AND tmpsigns.since <= date("31/12/2016")
    THEN 
        DO:
           /* MESSAGE string(iDate) " " string(iDateX) " " string("Комиссию НЕ БЕРЁМ!") view-as alert-box. */
           NEXT.
        END.
    ELSE 
        DO:
           /* MESSAGE string(iDate) " " string(iDateX) " " string(commission.commission) " " string("Комиссию БЕРЁМ!") view-as alert-box. */
           RUN SetSysConf IN h_base ("пакетсуммсп", STRING(vSummSp)).
           RUN SetSysConf IN h_base ("пакетсумм",   STRING(sumKom)).
           RUN SetSysConf IN h_base ("пакетсчет",   STRING(vAcct)).
           RUN SetSysConf IN h_base ("пакет",       STRING(tmpsigns.xattr-value)).
           RUN g-trans.p (iDate,iOprid).
        END.
  END.
/*===================== end =====================*/*/

END.



FOR EACH person WHERE person.date-out > iDate OR person.date-out EQ ?
NO-LOCK:
    vSummSp = 0.
    maxAcctBal = 0.
    FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                         AND tmpsigns.code      EQ 'Пакет'
                         AND tmpsigns.surrogate EQ string(person.person-id)
                         AND tmpsigns.since < iDateN
    NO-LOCK NO-ERROR.

    IF NOT AVAIL tmpsigns OR tmpsigns.xattr-value EQ '' 
    THEN NEXT.
    FIND FIRST commission WHERE commission.commission BEGINS "Pac"
                            AND commission.name-comm[1] BEGINS 'Пакет услуг ' + tmpsigns.xattr-value
    NO-LOCK NO-ERROR.
    FIND LAST comm-rate WHERE comm-rate.commission EQ commission.commission
                        AND comm-rate.since <= iDate
    NO-LOCK NO-ERROR.
    IF AVAIL comm-rate THEN sumKom = comm-rate.rate-comm.
    FOR EACH acct WHERE acct.cust-cat    EQ 'Ч'
                    AND acct.cust-id     EQ person.person-id
                    AND acct.filial-id   EQ shFilial
                    AND acct.contract    EQ 'Расчет'
                    AND acct.currency    EQ ''
                    AND (acct.close-date EQ ?
                    OR acct.close-date > iDate
                        )
    NO-LOCK
    BREAK BY acct.close-date BY acct.acct:

        FIND FIRST signs WHERE signs.file-name  EQ 'acct' 
                           AND signs.surrogate  EQ acct.acct + "," + acct.currency
                           AND signs.code       EQ "КорпКарт"
                           AND signs.code-value EQ "Да" NO-LOCK NO-ERROR.
        IF AVAIL signs THEN next.

        FOR EACH op-entry WHERE op-entry.op-date >= iDateN
                       AND op-entry.op-date <= iDate
                       AND op-entry.acct-db EQ acct.acct
                       AND op-entry.acct-cr BEGINS '47423'
                       AND NOT (op-entry.op-status BEGINS 'А')
        NO-LOCK:
            FIND FIRST op WHERE op.op      EQ op-entry.op
                            AND op.op-kind EQ '1617' NO-LOCK.
            IF AVAIL op THEN
            DO:
                vSummSp = vSummSp + op-entry.amt-rub.
            END.
        END. 

        RUN acct-pos IN h_base (acct.acct, '', iDate, iDate, ?).
        acctBal = abs(sh-bal).
        vSummSp = SumKom - vSummSp.

        IF acctBal >= sumKom
        THEN DO:
            vAcct = acct.acct.
            LEAVE.
        END.

        IF maxAcctBal <= acctBal
        THEN DO:
            vAcctFirst = acct.acct.
        END.
    END.

    /*message "vSummSp =" string(vSummSp) "SumKom =" string(SumKom) "vAcct =" string(vAcct) view-as alert-box.*/

    IF acctbal < sumKom THEN vAcct = vAcctFirst.
    IF vSummSp <= 0 THEN NEXT.
     
    RUN SetSysConf IN h_base ("пакетсуммсп", STRING(vSummSp)).
    RUN SetSysConf IN h_base ("пакетсумм",   STRING(sumKom)).
    RUN SetSysConf IN h_base ("пакетсчет",   STRING(vAcct)).
    RUN SetSysConf IN h_base ("пакет",       STRING(tmpsigns.xattr-value)).

    RUN g-trans.p (iDate,iOprid).

END.
