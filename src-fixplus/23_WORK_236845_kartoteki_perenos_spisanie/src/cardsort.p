{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */


def input parameter entry-rid as recid no-undo.
def input parameter in-class like code.class no-undo.
def input parameter in-parent like code.code no-undo.
def input parameter level as INT64 no-undo.
def shared var pick-value as char no-undo.
def shared var auto as logical no-undo.

find code where code.class eq in-class and code.code eq in-parent no-lock.
pick-value = if (num-entries(code.val) ge 2) then entry (2, code.val) else "".
if not can-find(code where code.class eq in-parent and code.code eq pick-value)
   then pick-value = "".
if entry-rid ne 0 and not auto then do:
   if FGetSetting("КартПриоритет",?,"") EQ "Нет" then
      pick-value = "Прочие".
   else 
      do with frame sort row 10 1 columns no-labels centered overlay on error undo,leave on endkey undo,leave:  
        pause 0.
        display code.name + ":" @ code.name pick-value.
        update pick-value help "Введите значение КАУ. F1-выбор из списка возможных значений."
        validate(pick-value eq "" or can-find(code where code.class eq in-parent and
        code.code eq pick-value), "В списке нет такого значения")
        editing:
        readkey.
        if lastkey eq keycode("F1") then do:
        run cardsor#.p (in-class, in-parent, level).
        display pick-value.
        end.
        else if lastkey eq keycode("F8") then frame-value = "".
        else apply lastkey. 
      end.   
  end.
      hide frame sort.
   end. 
else if not auto then run cardsor#.p (in-class, in-parent, level).
else.
