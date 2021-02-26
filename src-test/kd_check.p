/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2002 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: rsrv-chk.p
      Comment: �᪫�祭�� �� �६����� ⠡���� �� ������஢, �� �����
               ��� ������ � ⥪�騩 ����.

   Parameters:
      Used by:
      Created: 
     Modified: xaro 01.12.2005 ��� 0054846
*/

{globals.i}
{flt-file.i}        /* ������� 䨫��� �� ������ࠬ */
{all_note.def}      /* ������ � recid, ��࠭��� �� 䨫���� ����ᥩ Shared
                       � ��⮩ ⥪�饣� ����.��� - in_op_date */
{sh-defs.i}

{intrface.get olap}
{intrface.get loan}
{intrface.get pint}
{checkov.i}


DEFINE INPUT PARAMETER iParam  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iOpRid  AS RECID  NO-UNDO.

DEF VAR mFlDel AS LOG NO-UNDO.
def var vkol-bal-tot as int no-undo.
def var vkol-bal as int no-undo.
DEF VAR vR AS LOG NO-UNDO.


/* �஢�ઠ ������⢠ �⮡࠭��� ����ᥩ */
FIND LAST all_recids NO-ERROR.
IF NOT AVAIL all_recids THEN
DO:
   {intrface.del }
   RETURN.
END.



FOR EACH all_recids, loan WHERE RECID(loan) = all_recids.rid NO-LOCK:


/* kam */
   IF checkov(BUFFER loan) THEN DELETE all_recids.

END. /*FOR EACH*/

vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
/* message "�⮡࠭� " + string(vkol-bal-tot) + " ������஢"  view-as alert-box. */

   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES     FILL (" ",79).
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 FILL (" ",79).
   PAUSE 0.  

{intrface.del}
