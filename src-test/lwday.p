/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1999 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: is100.p
      Comment: ���᫥��� ��業⮢ �� ॠ��� ���⮪ �� ���.
               ���᫥��� �㤥� �ந�������� ⮫쪮 � ⮬ ��砥,
               �᫨ ���⮪ �� ��� �� ᭨����� ����� �������쭮��.
   Parameters:
      Created: Om 25/12/2003
     Modified:
*/
{globals.i}
{justasec}
{intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
{intrface.get date}
DEFINE INPUT  PARAM curr_date AS DATE      NO-UNDO. /* ������ ��� */
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
