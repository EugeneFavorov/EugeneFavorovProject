/* 16/04/97 - Dima
   �����誠 ��� �맮�� ��楤�� ������� ���⮢

   � User-Proc: stub (( Proc_Name
*/

def input param s as char no-undo.

find first op no-lock.

run value(s + ".p") (recid(op)).
