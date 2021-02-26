{globals.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */


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
   if FGetSetting("����ਮ���",?,"") EQ "���" then
      pick-value = "��稥".
   else 
      do with frame sort row 10 1 columns no-labels centered overlay on error undo,leave on endkey undo,leave:  
        pause 0.
        display code.name + ":" @ code.name pick-value.
        update pick-value help "������ ���祭�� ���. F1-�롮� �� ᯨ᪠ ��������� ���祭��."
        validate(pick-value eq "" or can-find(code where code.class eq in-parent and
        code.code eq pick-value), "� ᯨ᪥ ��� ⠪��� ���祭��")
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
