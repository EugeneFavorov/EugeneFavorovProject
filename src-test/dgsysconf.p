/*   
kam

setsysconf

*/


DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.
def var accttype as char no-undo.

{globals.i}
{intrface.get pbase}
/*
run deleteolddataprotocol in h_base("dogpdg") .
  */
if in-rid <> 0 then
     find op-templ where recid(op-templ) = in-rid no-lock no-error .
     if avail op-templ then do:
         accttype = replace(op-templ.acct-cr,'Роль("','').
         accttype = replace(op-templ.acct-cr,'РольС("','').
	 accttype = replace(accttype,')','').
         message accttype view-as alert-box. 
     end.

  find op-entry where recid(op-entry)  = in-rid1 no-lock no-error .
  if (in-rid <> 0 and not avail op-templ) or not avail op-entry then do :
/*    message 'Ошибка контроля проводки ' view-as alert-box error.  */
    return .
  end.



find last loan-acct where loan-acct.acct = (entry(1,op-entry.acct-cr,' ')  +  '     @' + shfilial)
	and loan-acct.acct-type = accttype no-lock no-error.
 if avail loan-acct then do:
	run deleteolddataprotocol in h_base("dogpdg") .
	RUN SetSysConf IN h_base ("dogpdg", loan-acct.cont-code).
end.
