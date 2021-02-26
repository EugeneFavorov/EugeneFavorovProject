
{globals.i}

Define Input Parameter sParams As char. /* счДБ, параметр(что возвращаем) */
Define Output Parameter sOutput As Char.
sOutput = ''.

FIND LAST op-entry WHERE
	op-entry.acct-cr begins entry(3,sParams) 
	and op-entry.acct-db begins entry(1,sParams) 
	and op-entry.op-status >= "√" no-lock no-error.
IF AVAIL op-entry THEN DO:
	CASE entry(2,sParams):
		WHEN 'acct-cr'  THEN sOutput = entry(1,op-entry.acct-cr,' ').
		WHEN 'amt-rub'  THEN sOutput = STRING(abs(op-entry.amt-rub)).
		WHEN 'op-date'  THEN sOutput = STRING(op-entry.op-date).
		WHEN 'details'  THEN DO:
			FIND FIRST op OF op-entry no-lock no-error.
			sOutput = op.details.
		END.
	END CASE.



END.




