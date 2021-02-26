DEF VAR acpos   AS DEC NO-UNDO.
DEF VAR mAcct AS CHARACTER NO-UNDO.
DEF VAR vStartMonth AS DATE NO-UNDO.
{setdest.i}
{globals.i}
{intrface.get xclass}
{sh-defs.i}
DEF VAR mAcctCur AS CHARACTER NO-UNDO.
DEF VAR mAcctBal AS DECIMAL NO-UNDO.
{getdate.i}
vStartMonth = DATE("01/" + STRING(MONTH(end-date)) + '/' + STRING(YEAR(end-date))).
put unformatted "Список вкладов с ненулевым остатком на счете ДВ" SKIP.
FOR EACH loan where loan.contract = "dps"
                and loan.close-date = ?
                /*and loan.loan-status = "Ф"*/
                and loan.cont-type <> "ДВ"
/*                and loan.cont-code begins "1-10979/RUR@0000"*/
                and loan.open-date < vStartMonth
                and loan.cont-type <>"gold_dv_no"
NO-LOCK:

    find first signs where signs.file-name = 'loan'
                    and signs.code = 'ДатаНачПереч'
                    and signs.surrogate = loan.contract + ',' + loan.cont-code
    NO-LOCK NO-ERROR.
    if NOT avail signs then DO: 
/*        MESSAGE "1234" loan.cont-code VIEW-AS ALERT-BOX.*/
        next.
    END.
    
    find last loan-cond where loan-cond.contract = loan.contract
				and loan-cond.cont-code = loan.cont-code
				and loan-cond.since >= vStartMonth
				no-lock no-error.
    if avail loan-cond then next.
    FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
			    AND loan-acct.cont-code EQ loan.cont-code
			    AND loan-acct.acct-type EQ "loan-dps-t"
			    and loan-acct.since LE end-date
			    NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN do:
	mAcct = loan-acct.acct.
	mAcctCur = loan-acct.currency.
/*	MESSAGE "1234" loan-acct.acct  " loan-dps-t" VIEW-AS ALERT-BOX.*/
	end.
	else
	next.
	if mAcct <> "" then do:
	RUN acct-pos IN h_base (mAcct, mAcctCur, end-date, end-date, ?).
	IF mAcctCur EQ "" THEN DO:
	mAcctBal =  abs(sh-bal).
	end.
	else DO:
	mAcctBal =  abs(sh-val).
	end.
	end.
	ELSE mAcctBal = 0.
	
	if mAcctBal < 50 then next.
    find last loan-acct where loan-acct.contract = loan.contract
                           and loan-acct.cont-code = loan.cont-code
                           and LOAN-ACCT.ACCT-TYPE = "loan-dps-tr"
                           and loan-acct.since LE end-date
    NO-LOCK NO-ERROR.
    IF NOT AVAIL loan-acct then next.
    else do:
/*    MESSAGE "1234" loan-acct.acct  " loan-dps-tr" VIEW-AS ALERT-BOX.*/
	find first op-entry where op-entry.op-date eq end-date
				and op-entry.acct-db = loan-acct.acct
				and op-entry.acct-cr = mAcct
				and op-entry.op-status BEGINS "√" 
				NO-LOCK NO-ERROR.
				
    if avail op-entry then do:
/*    MESSAGE "нету" VIEW-AS ALERT-BOX.*/
     /*PUT UNFORMATTED loan.doc-ref " " SKIP.*/
     next.
     end.
     else do:
     /*MESSAGE "есть" VIEW-AS ALERT-BOX.*/
     PUT UNFORMATTED loan.cont-code skip.
     end.
     end.
     
END.

{preview.i}