/*
    Copyright: (C) +
     Filename: 
      Comment: Поиск счетов для комиссий Клиент-банк
   Parameters:
         Uses:
      Used by:
      Created: 11.09.2013 kam     
     Modified: 

*/

{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iAcct AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER iBegDate AS Date  NO-UNDO.
DEFINE INPUT PARAMETER iEndDate AS Date  NO-UNDO.
DEFINE INPUT PARAMETER iOpDate AS Date  NO-UNDO.
DEFINE INPUT PARAMETER iSumm AS Decimal  NO-UNDO. 

DEFINE SHARED VARIABLE pick-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE myDetails as char no-undo init ''.
DEFINE VARIABLE newAcct as char no-undo init ''.
DEFINE VARIABLE ValCode as char no-undo init ''.
DEFINE VARIABLE klb as logical no-undo init false.

pick-value = "0".

for each op-entry where
	(op-entry.op-status = "√" or op-entry.op-status = "√√") and
	op-entry.op-date >= iBegDate and
	op-entry.op-date <= iEndDate and
	(op-entry.acct-db = iAcct or op-entry.acct-cr = iAcct) and
	can-do('!70601*,!60322*,!47423*,*',op-entry.acct-cr):
	klb = true.
	leave.
end.
if klb then do:
	run getAcct ( input iAcct, input iOpDate, input iSumm, output newAcct). 
	run getDetails ( input iAcct, input newAcct, input iBegDate, input iEndDate, output myDetails, output ValCode).
	if myDetails <> '' and newAcct <> '' then pick-value = newAcct + '|' + ValCode + '|' + myDetails.
end.

RETURN pick-value.

/* Формируем назначение платежа */
Procedure getDetails.
	DEFINE INPUT PARAMETER i_Acct AS CHARACTER  NO-UNDO.
	DEFINE INPUT PARAMETER i_nAcct AS CHARACTER NO-UNDO.
	DEFINE INPUT PARAMETER i_BegDate AS Date  NO-UNDO.
	DEFINE INPUT PARAMETER i_EndDate AS Date  NO-UNDO.
	DEFINE OUTPUT PARAMETER i_Details as Char NO-UNDO.
	DEFINE OUTPUT PARAMETER i_ValCode as CHAR NO-UNDO.
	
	DEFINE VARIABLE dateDogAcct as char no-undo.
	DEFINE VARIABLE rez         as log  no-undo.

	find first acct where acct.acct = i_nAcct no-lock no-error.
	if avail acct then do:
		if acct.cust-cat eq 'Ч' then do:
			find first person where person.person-id eq acct.cust-id no-lock no-error.
			if person.country-id eq 'RUS' then rez = TRUE.
		end.
		if acct.cust-cat eq 'Ю' then do:
			find first cust-corp where cust-corp.cust-id eq acct.cust-id no-lock no-error.
			if cust-corp.country-id eq 'RUS' then rez = TRUE.
		end. 
		if rez then do:
			if acct.currency ne '' then
			 i_ValCode = '\{VO80150\}'.
		end.
		else do:
			if acct.currency eq '' then
			 i_ValCode = '\{VO80050\}'.
			else
			 i_ValCode = '\{VO99090\}'.
		end.
		i_Details = 'Комиссия за ежемесячное обслуживание счета ' + substring(i_Acct,1,20) +
			' по системе ДБО "iBank2" за период с ' +
			string(i_BegDate, "99/99/9999") + ' по ' +
			string(i_EndDate, "99/99/9999") +
			', согласно тарифам банка. Ндс не облагается.'.
	end.
end procedure.

/* ищем счет для проводки */
Procedure getAcct.
	DEFINE INPUT PARAMETER i_Acct AS CHARACTER  NO-UNDO.
	DEFINE INPUT PARAMETER i_OpDate AS Date  NO-UNDO.
	DEFINE INPUT PARAMETER i_Summ as Decimal NO-UNDO.
	DEFINE OUTPUT PARAMETER i_newAcct as CHARACTER NO-UNDO.

	DEFINE VARIABLE acctBal as Decimal no-undo init 0.
	DEFINE VARIABLE acctTmp as CHARACTER no-undo init ''.
	DEFINE VARIABLE acctCustId as int no-undo init 0.
	DEFINE VARIABLE acctCustCat as CHARACTER no-undo init ''.	
	
	find first acct where acct.acct = i_Acct no-lock no-error.
	if avail acct then do:
		acctCustId = acct.cust-id.
		acctCustCat = acct.cust-cat.
		if acct.currency ne '' and acct.contract eq 'Текущ' then do:
			RUN acct-pos IN h_base (i_Acct, acct.currency, i_OpDate, i_OpDate, ?).
			acctBal = abs(sh-bal).
			if acctBal >= i_Summ then
				i_newAcct = acct.acct.
			else do:
				find first acct where 
						acct.cust-cat = acctCustCat and
						acct.cust-id = acctCustId and
						acct.close-date = ? and
						acct.currency = '' and
						acct.contract = 'Расчет' and
						acct.filial-id = shFilial
					no-lock no-error.
				if avail(acct) then
					i_newAcct = acct.acct.
			end.
		end.
		if acct.currency eq '' and can-do('СпецПА,Текущ,Расчет',acct.contract) then do:
			find first acct where 
					acct.cust-cat = acctCustCat and
					acct.cust-id = acctCustId and
					acct.close-date = ? and
					acct.currency = '' and
					acct.contract = 'Расчет' and
					acct.filial-id = shFilial
				no-lock no-error.
				if avail(acct) then
					i_newAcct = acct.acct.
		end.
	end.
end procedure.