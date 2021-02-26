/*   
kam


*/


DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.


{globals.i}
{intrface.get pbase}
{intrface.get xclass}

def var formtype as char no-undo.
def var fl as char no-undo.

if in-rid <> 0 then
     find op-templ where recid(op-templ) = in-rid no-lock no-error .
  find op-entry where recid(op-entry)  = in-rid1 no-lock no-error .
  if (in-rid <> 0 and not avail op-templ) or not avail op-entry then do :
/*    message 'Ошибка контроля проводки ' view-as alert-box error.  */
    return .
  end.

if shFilial = '0000' then do:
    fl = 'ГБ. '.
    formtype = 'ГБ4000000003'.
end.
if shFilial = '0500' then do:
    fl = 'ОФ. '.
    formtype = 'ОФ405001000003'.
end.
if shFilial = '0300' then do:
    fl = 'ТФ. '.
    formtype = 'ТФ4050010003'.
end.



find last acct where acct.acct = (entry(1,op-entry.acct-cr,' ')  +  '     @' + shfilial) no-error.
if avail acct then do:
	acct.details = fl + acct.details.
	UpdateSigns ("acct",acct.acct + "," + acct.currency,"form-type-code",formtype,?).
end.
