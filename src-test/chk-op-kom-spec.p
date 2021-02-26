/* ayv
   Поиск оборотов по счету без комиссий для ежемесячных комиссий
   Используется транзакциями _16m*
*/

{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iAcct AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER iBegDate AS Date  NO-UNDO.
DEFINE INPUT PARAMETER iEndDate AS Date  NO-UNDO.

DEFINE SHARED VARIABLE pick-value AS CHARACTER NO-UNDO.

DEF VAR Summa AS DEC NO-UNDO.

pick-value = 'NO'.
Summa = 0.

find first acct where acct.acct eq iAcct no-lock no-error.

for each op-entry where op-entry.filial-id eq     shFilial
                    and op-entry.op-status begins "√"
		    and op-entry.op-date   ge     iBegDate
		    and op-entry.op-date   le     iEndDate
		    and (op-entry.acct-db  eq     acct.acct and op-entry.acct-cr ne acct.acct)
	            and can-do('!70601*,!60322*,!47423*,*', op-entry.acct-cr)
				  no-lock,
		first op of op-entry where op.op-status ne 'А'
		no-lock:
		Summa = Summa + op-entry.amt-rub.
		if op-entry.currency eq acct.currency and
		   (
		   	(op-entry.currency eq '' and 
		   	 op-entry.amt-rub > 0)
		   or
		   	(op-entry.currency ne '' and
		   	 op-entry.amt-cur > 0)
		   ) 
                then pick-value = 'YES'.

	end.

RUN SetSysConf IN h_base ("Summa", Summa).

RETURN pick-value.