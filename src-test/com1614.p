/*
    Copyright: (C) +
     Filename: 
      Comment: Поиск счетов для комиссий по ведению счета
   Parameters:
         Uses:
      Used by:
      Created: 
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

pick-value = "0".

run getAcct ( input iAcct, input iOpDate, input iSumm, output newAcct). 
run getDetails ( input iAcct, input newAcct, input iBegDate, input iEndDate, output myDetails, output ValCode).
if myDetails <> '' and newAcct <> '' then pick-value = newAcct + ',' + ValCode + ',' + myDetails.


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
		i_Details = 'Комиссия за ведение счета ' + substring(i_Acct,1,20) +
			' за период с ' +
			string(i_BegDate, "99/99/9999") + ' по ' +
			string(i_EndDate, "99/99/9999") +
			' согласно тарифам банка. Ндс нет.'.
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
			find first blockobject where blockobject.file-name eq 'acct'
								   and   blockobject.surrogate eq acct.acct + ',' + acct.currency
								   and   blockobject.end-datetime eq ?
				no-lock no-error.
			if (acctBal >= i_Summ) and (not avail(blockobject)) then
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


/*{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT  PARAMETER i_Acct AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER i_OpDate AS Date  NO-UNDO.
DEFINE INPUT  PARAMETER i_Summ as Decimal NO-UNDO.

DEFINE SHARED VARIABLE pick-value AS CHARACTER NO-UNDO.

pick-value = "0".

DEFINE VARIABLE acctBal as Decimal no-undo init 0.
DEFINE VARIABLE acctTmp as CHARACTER no-undo init ''.
DEFINE VARIABLE acctCustId as int no-undo init 0.
DEFINE VARIABLE acctCustCat as CHARACTER no-undo init ''.
DEFINE VARIABLE i_newAcct as CHARACTER NO-UNDO init ''.	

find first acct where acct.acct = i_Acct no-lock no-error.
if avail acct then do:
	/*if acct.currency = '' then do:*/
		acctTmp = i_Acct.
		RUN acct-pos IN h_base (i_Acct, acct.currency, i_OpDate, i_OpDate, ?).
		acctBal = abs(sh-bal).
		if acctBal >= i_Summ then
			i_newAcct = i_Acct.
	/*end.*/
	if i_newAcct = '' then do:
		acctCustId = acct.cust-id.
		acctCustCat = acct.cust-cat.
		for each acct where 
			acct.close-date = ? and
			acct.currency = '' and
			acct.cust-cat = acctCustCat and
			acct.cust-id = acctCustId and
			( acct.bal-acct = 40502 or
			acct.bal-acct = 40602 or
			acct.bal-acct = 40702 or
			acct.bal-acct = 40802 or
			acct.bal-acct = 40703 or
			acct.bal-acct = 40807 or
			acct.bal-acct = 40814 or
			acct.bal-acct = 40701)
			and acct.acct <> i_Acct
			no-lock:
			
			if acctTmp = '' then acctTmp = acct.acct.
			RUN acct-pos IN h_base (acct.acct, '', i_OpDate, i_OpDate, ?).
			acctBal = abs(sh-bal).
			if acctBal >= i_Summ then do:
				i_newAcct = acct.acct.
				leave.
			end.
		end.
		if i_newAcct = '' then do:
			find first acct where 
				acct.cust-cat = acctCustCat and
				acct.cust-id = acctCustId and
				acct.close-date = ? and
				acct.currency = '' and
				acct.contract = 'Расчет'
			no-lock no-error.
			if avail(acct) then
				i_newAcct = acct.acct.
		end.
	end. /* if i_newAcct = '' then do: */
end.

if i_newAcct <> '' then pick-value = i_newAcct.

RETURN pick-value.*/