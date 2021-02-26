/* 16/04/97 - Dima
   Заглушка для вызова процедур генератора отчетов

   В User-Proc: stub (( Proc_Name
*/

def input param s as char no-undo.

find first op no-lock.

run value(s + ".p") (recid(op)).
