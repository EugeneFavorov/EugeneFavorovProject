/* Ищем косяки по гашениям (сумма гашения основного долга не равна сумме по графику)  */

{globals.i}

{tmprecid.def}

def var acct_prosr as char no-undo.
def var acct_prosr_date as date no-undo.
def var acct_rasch as char no-undo.
def var acct_credit as char no-undo.

{setdest.i} 
 
for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
	acct_prosr = ''.
	acct_credit = ''.
	acct_rasch = ''.
	find last loan-acct where loan-acct.contract = 'Кредит' and loan-acct.cont-code = loan.cont-code
        and loan-acct.Acct-type = 'Кредит' no-lock no-error.
	if avail loan-acct then acct_credit = loan-acct.acct.
	find last loan-acct where loan-acct.contract = 'Кредит' and loan-acct.cont-code = loan.cont-code
        and loan-acct.Acct-type = 'КредРасч' no-lock no-error.
	if avail loan-acct then acct_rasch = loan-acct.acct.
	if acct_credit <> '' and acct_rasch <> '' then do:
		find last loan-acct where loan-acct.contract = 'Кредит' and loan-acct.cont-code = loan.cont-code
			and loan-acct.Acct-type = 'КредПр' no-lock no-error.
		if avail loan-acct then do:
			acct_prosr = loan-acct.acct.
			acct_prosr_date = loan-acct.since.
		for each term-obl where term-obl.cont-code = loan.cont-code 
				and term-obl.idnt = 3 no-lock by term-obl.end-date:
				if term-obl.end-date = acct_prosr_date then do:
					find first op-entry where (op-entry.op-status = "√" or op-entry.op-status = "√√")
						and (op-entry.acct-db = acct_rasch and op-entry.acct-cr = acct_credit)
						and op-entry.op-date = acct_prosr_date and op-entry.amt-rub >= term-obl.amt-rub no-lock no-error.
					if avail op-entry then do:
						put unformatted string(loan.cont-code,"x(30)") + string(term-obl.end-date, "99/99/9999")
							+ string(term-obl.amt-rub,  "->,>>>,>>>,>>9.99") + string(op-entry.amt-rub,  "->,>>>,>>>,>>9.99")skip.
					end.
				end.
				leave.
			end.
		end.
	end.
	else do:
        put unformatted string(loan.cont-code,"x(30)") + string(loan.open-date, "99/99/9999") + ' не найдены счета' skip.
	end.
end.
{preview.i}


   
