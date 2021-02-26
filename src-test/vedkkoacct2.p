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

DEF VAR vBr	as CHAR NO-UNDO.
DEF VAR vCName	as CHAR NO-UNDO.
DEF VAR vDateOp	as CHAR NO-UNDO.
DEF VAR vBranOp	as CHAR NO-UNDO.
DEF VAR fname 	as CHAR NO-UNDO.
DEF VAR iCount  AS INTEGER NO-UNDO.
DEF VAR iCurrentCount  AS INTEGER INIT 0 NO-UNDO.
DEF VAR zz  AS INT64 NO-UNDO.
DEF BUFFER ent FOR op-entry.

DEFINE TEMP-TABLE tt-sch NO-UNDO
    FIELD acct LIKE acct.acct
    FIELD number LIKE acct.number
    FIELD branch like acct.branch-id
    FIELD cust AS CHARACTER
    FIELD open-date AS DATE
    FIELD user-id AS CHARACTER
    FIELD last-date AS CHAR 
    FIELD opbranch AS CHARACTER
    INDEX acct branch
.

{empty tt-sch}

vBr = '0400'.
pause 0.

Do on error undo, leave on endkey undo, leave with frame ftune:
  Update
    vAc label "Счет" help "Первые цифры счета" 
    vBr label "Подразделение" help "Код подразделения для выгрузки информации по счетам"   
  with centered row 10 overlay side-labels 1 col
  title "[   Параметры Отчета   ]".
End.
Hide frame ftune no-pause.

if LASTKEY EQ KEYCODE("ESC") THEN
	RETURN.
	
/* найдем кол-во всего */	
SELECT COUNT(*)
INTO iCount 
FROM acct
WHERE acct.branch-id = vBr
	AND acct.acct-cat = 'b'
	AND acct.acct begins vAc
	AND acct.close-date is null.
	
{bar-beg2.i
  &BarTotal     = iCount
  &BarMessage   = "'Обработка счетов'"
 }	

FOR EACH acct
	WHERE acct.branch-id = vBr
	AND acct.acct-cat = 'b'
	AND acct.acct begins vAc
	AND acct.close-date EQ ?
	NO-LOCK:

		RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT vCName).
		
		iCurrentCount = iCurrentCount + 1.
			
		{bar2.i
			&BarPointer = iCurrentCount
		}

        CREATE tt-sch.
		/**/
        ASSIGN
			tt-sch.acct = acct.acct
			tt-sch.number = acct.number
			tt-sch.branch = vBr
			tt-sch.cust = vCName
			tt-sch.open-date = acct.open-date
			tt-sch.user-id = acct.user-id
			.
			
		/* сначала 1 */
		FIND LAST op-entry
			WHERE op-entry.op-date >= tt-sch.open-date
			AND op-entry.acct-cr = tt-sch.acct
			AND op-entry.acct-db begins '202'
			AND op-entry.op-status BEGINS "√"
		NO-LOCK NO-ERROR.
		
		/* потом 2 */
		FIND LAST ent
			WHERE ent.op-date >= tt-sch.open-date
			AND ent.acct-db = tt-sch.acct
			AND ent.acct-cr begins '202'
			AND ent.op-status BEGINS "√"
		NO-LOCK NO-ERROR.

		/**/
		if avail op-entry or avail ent then 
			DO:
				/**/
				if avail op-entry and avail ent then
					zz = MAXIMUM(op-entry.op, ent.op).
				/**/	
				if avail op-entry and not avail ent then
					zz = op-entry.op.
				/**/
				if not avail op-entry and avail ent then
					zz = ent.op.				
				/**/
				FIND op
					where op.op = zz
				NO-LOCK NO-ERROR.
				/**/
				if avail op then
					DO:
						vDateOp = string(op.op-date).
						vBranOp = op.branch-id.
					END.
				/**/
				ELSE
					DO:
						vDateOP = 'Отсутствуют проводки'.
						vBranOp = ''.
					END.
			END.
		ELSE
			DO:
				vDateOP = 'Отсутствуют проводки'.
				vBranOp = ''.
			END.
			/**/
			ASSIGN
				tt-sch.last-date = vDateOp
				tt-sch.opbranch = vBranOp
				.

END.

{del-bar.i}
{init-bar.i "Выгрузка данных в Excel"}
{vedkkoacct2.i}
{del-bar.i}
