/*   
     Filename: setbenacct.p
     Comment:  Устанавливаем реквизит ben-acct на созданном документе.
     Чтобы вызвать: на шаблоне документа надо заполнить допрек ВыпПосле: Выполнить после обработки шаблона : setbenacct
     и заполнить допрек Постоянный получатель : 045209783^61209810401400000013^......
*/


DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.

def var benacct as char no-undo init "".
def var constrecip as char no-undo.

  if in-rid <> 0 then
     find op-templ where recid(op-templ) = in-rid no-lock no-error .
  find op-entry where recid(op-entry)  = in-rid1 no-lock no-error .
  if (in-rid <> 0 and not avail op-templ) or not avail op-entry then do :
/*    message 'Ошибка контроля проводки ' view-as alert-box error.  */
    return .
  end.

  find first op of op-entry no-error.
  if avail op then do:
    FIND FIRST op-template WHERE 
              op-template.op-kind     EQ op.op-kind 
          AND op-template.op-template EQ op.op-templ NO-LOCK NO-ERROR.
    IF NOT AVAIL op-template THEN DO:
   /*   MESSAGE "Не могу найти запись шаблона!"  
         VIEW-AS ALERT-BOX ERROR.                */
    return.
    END.
  
    find first signs
         where file-name = 'op-template'
	 and surrogate = string(op-template.op-kind) + ',' + string(op-templ.op-template)
	 and code = 'const-recip'
    no-lock no-error.

    if avail signs then do:
    constrecip = signs.xattr-value.
     if num-entries(constrecip,'^') >= 2 then do:
         benacct = entry(2,constrecip,'^').
         if length(benacct)=20 then do:
           op.ben-acct = benacct.
         end.
     end.
    end.
    if (substring(op-entry.acct-db,1,5) = '30302' or substring(op-entry.acct-db,1,5) = '30306') then do:
        op.doc-kind = 'send'.
    end.
  end.

