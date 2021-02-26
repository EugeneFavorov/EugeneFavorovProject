/*   
kam
закрываем договор и счет 40817 после ПДГ
*/


DEFINE SHARED STREAM err.
def var opdate as date no-undo.
def var mycontcode as char no-undo.
def var mymess as char no-undo.

{globals.i}
{sh-defs.i}
{intrface.get loan}
{intrface.get ovl}
{intrface.get pbase}

opdate = GetOpDate().
/*
mycontcode = mcontcode.
*/

mycontcode = GetSysConf("dogpdg").
mymess = ''.
  
/*
message string(opdate) + mycontcode view-as alert-box.
  */

FIND FIRST loan WHERE loan.cont-code = mycontcode and loan.contract = 'Кредит' NO-LOCK NO-ERROR.
IF AVAIL loan THEN DO:

/*    {setdest.i &stream="stream err" &filename='_spool1.tmp'} */
    RUN CloseLoan IN h_loan(loan.contract,
                        loan.cont-code,
                        opdate,
                        0).
/*    DISP STREAM err "". */
    
    IF loan.close-date <> ? Then do: 
	mymess = 'Договор закрыт'. 
        FIND last loan-acct where loan-acct.contract = loan.contract
            and loan-acct.cont-code <> loan.cont-code
            and loan-acct.acct-type = 'КредРасч'
            no-lock no-error.
        IF not avail loan-acct then do:
            RUN acct-pos IN h_base (loan-acct.acct, '', opdate + 1, opdate + 1, ?).
            IF abs(sh-bal) = 0 THEN DO:
                find first acct where acct.acct = loan-acct.acct.
                if avail acct then acct.close-date = opdate.
		mymess = mymess + ', счет ' + acct.number + ' закрыт'.
            end.
	end.
    end.
    else do:
        mymess = 'Договор не закрыт'.
    end.
/*    {preview.i &stream="stream err" &filename='_spool1.tmp'} */

END.    

message mymess view-as alert-box.

run deleteolddataprotocol in h_base("dogpdg") .

{intrface.del}
