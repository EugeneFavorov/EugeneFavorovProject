/*

               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: Отчет по последней операции на счетах.
   Parameters:
         Uses:
      Used by:
      Created: kau
     Modified: 
*/

{client.i}

DEF Input Param vAc as CHARACTER no-undo.
/*DEF VAR vAc	as CHAR NO-UNDO.*/
DEF VAR vBr	as CHAR NO-UNDO.
DEF VAR vCName	as CHAR NO-UNDO.
DEF VAR vDateOp	as CHAR NO-UNDO.
DEF VAR vBranOp	as CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-sch NO-UNDO
    FIELD acct LIKE acct.acct
    FIELD number LIKE acct.number
    FIELD branch like acct.branch-id
    FIELD cust AS CHARACTER
    FIELD open-date AS DATE
    FIELD user-id AS CHARACTER
    FIELD last-date AS CHAR 
    FIELD opbranch AS CHARACTER
    INDEX acct branch.

vBr = '0400'.
pause 0.

Do on error undo, leave on endkey undo, leave with frame ftune:
  Update
    vAc label "Счет" help "Первые цифры счета"   
    vBr label "Код подразделения" help "Введите код подразделения для выгрузки информации по счетам"   
  with centered row 10 overlay side-labels 1 col
  title "[   Параметры Отчета   ]".
End.
Hide frame ftune no-pause.

if LASTKEY EQ KEYCODE("ESC") THEN
	return.

def var fname as char no-undo.


FOR EACH acct WHERE acct.acct-cat = 'b'
		AND acct.acct begins vAc
		and acct.branch-id = vBr
		and acct.close-date EQ ?
		NO-LOCK:

	RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT vCName).

        CREATE tt-sch.
        ASSIGN
		tt-sch.acct = acct.acct
		tt-sch.number = acct.number
		tt-sch.branch = vBr
		tt-sch.cust = vCName
		tt-sch.open-date = acct.open-date
	 	tt-sch.user-id = acct.user-id
		.
FIND last op-entry where op-entry.op-date >= tt-sch.open-date
		AND ((op-entry.acct-cr = tt-sch.acct and op-entry.acct-db begins '202')
		or (op-entry.acct-db = tt-sch.acct and op-entry.acct-cr begins '202'))
		AND op-entry.op-status BEGINS "√" NO-LOCK NO-ERROR.
if avail op-entry then 
DO:
	FIND op where op.op = op-entry.op NO-LOCK NO-ERROR.
	if avail op then DO:
	vDateOp = string(op.op-date).
	vBranOp = op.branch-id.
	END.
	ELSE DO:
		vDateOP = 'Отсутствуют проводки'.
		vBranOp = ''.
	END.
END.
ELSE DO:
	vDateOP = 'Отсутствуют проводки'.
	vBranOp = ''.
END.
	ASSIGN
		tt-sch.last-date = vDateOp
		tt-sch.opbranch = vBranOp
		.

END.


{vedkkoacct.i}
