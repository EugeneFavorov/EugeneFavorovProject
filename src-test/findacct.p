
{globals.i}
Define Input Parameter custid As INT. /* cust-id */
Define Output Parameter mAcct As Char.
mAcct = ''.

FIND FIRST cust-corp WHERE cust-corp.cust-id = custid NO-LOCk NO-ERROR.
IF NOT AVAIL cust-corp THEN RETURN.
Find first acct where acct.cust-cat = 'ž' 
	and acct.cust-id = cust-corp.cust-id
	and acct.bal-acct = 40702 
	and acct.filial-id = shFilial no-lock no-error.
IF not avail acct then return.
	mAcct = acct.number.





                                                   


