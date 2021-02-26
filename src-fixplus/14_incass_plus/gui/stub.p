{globals.i}
{intrface.get tmess}

/* +++ stub.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:37am +++ */

/* 16/04/97 - Dima
   Заглушка для вызова процедур генератора отчетов

   В User-Proc: stub (( Proc_Name
*/

def input param s as char no-undo.

find first op no-lock.

run value(s + ".p") (recid(op)).

/* --- stub.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:37am --- */
