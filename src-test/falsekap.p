DEF VAR acpos   AS DEC NO-UNDO.
{setdest.i}

put unformatted "Список вкладов с ненулевым остатком на счете ДВ" SKIP.
FOR EACH loan where loan.contract = "dps"
                and loan.close-date = ?
                /*and loan.loan-status = "Ф"*/
                and loan.cont-type <> "ДВ" 
NO-LOCK:
    find first signs where signs.file-name = 'loan'
                    and signs.code = 'ДатаНачПереч'
                    and signs.surrogate = loan.contract + ',' + loan.cont-code
    NO-LOCK NO-ERROR.
    if NOT avail signs then DO: 
/*        MESSAGE "1234" loan.cont-code VIEW-AS ALERT-BOX.*/
        next.
    END.
    find first loan-acct where loan-acct.contract = loan.contract
                           and loan-acct.cont-code = loan.cont-code
                           and LOAN-ACCT.ACCT-TYPE = "loan-dps-tr"
    NO-LOCK NO-ERROR.
    IF NOT AVAIL loan-acct then next.
    find last acct-pos where acct-pos.acct = loan-acct.acct NO-LOCK NO-ERROR.
    IF avail acct-pos then DO:
	acpos = ABS(acct-pos.balance).
	FOR EACH op-entry where op-entry.op-date > acct-pos.since
				and op-entry.acct-db = loan-acct.acct
				and op-entry.op-status BEGINS "√"  NO-LOCK:
		
		/*if loan.doc-ref begins '1-62479'
		THEN MESSAGE acct-pos.currency " " op-entry.amt-cur VIEW-AS ALERT-BOX.
		
		if acct-pos.currency EQ ''
		THEN*/ acpos = acpos - op-entry.amt-rub.
		/*ELSE acpos = acpos - op-entry.amt-cur.*/
	END.
	FOR EACH op-entry where op-entry.op-date > acct-pos.since
				and op-entry.acct-cr = loan-acct.acct 
				and op-entry.op-status BEGINS "√"  NO-LOCK:
		/*if acct-pos.currency EQ ''
		THEN*/ acpos = acpos + op-entry.amt-rub.
		/*ELSE acpos = acpos + op-entry.amt-cur.*/

	END.
	
    END.
    ELSE acpos = 0.
    /*IF acpos = 0 then NEXT.*/
     PUT UNFORMATTED loan.doc-ref " " acpos SKIP.
END.

{preview.i}