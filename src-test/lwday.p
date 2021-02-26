/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1999 ТОО "Банковские информационные системы"
     Filename: is100.p
      Comment: Начисление процентов на реальный остаток на счете.
               Начисление будет производится только в том случае,
               если остаток на счете не снижался меньше минимального.
   Parameters:
      Created: Om 25/12/2003
     Modified:
*/
{globals.i}
{justasec}
{intrface.get refer}    /* Библиотека службы справочников. */
{intrface.get date}
DEFINE INPUT  PARAM curr_date AS DATE      NO-UNDO. /* Текущая дата */
   def var i             as INT64   no-undo .
   def var j             as INT64   no-undo .
   DEF VAR vWork         AS LOG     NO-UNDO.
   def var iDate         as date    no-undo.

   j = 0.
   do i= 1 to 20 :
      iDate = curr_date - i.
      ASSIGN
        vWork = IsWorkDayBranch (iDate, "0100")
        vWork = IF  vWork EQ ? THEN NOT holidayru(iDate) ELSE vWork
      .
      if not vWork then do:
         j = j + 1.
      end.
      else  do:
         pick-value =  string(j).
         RETURN pick-value.
      end.
   end.
