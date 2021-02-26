/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2002 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: rsrv-chk.p
      Comment: �᪫�祭�� �� �६����� ⠡���� �� ������஢, �� �����
               �뫮 �ॣ㫨஢���� १�ࢮ�.

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

DEFINE INPUT PARAMETER iParam  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iOpRid  AS RECID  NO-UNDO.

DEF VAR mFlDel AS LOG NO-UNDO.
def var vkol-bal-tot as int no-undo.
def var vkol-bal as int no-undo.
DEF VAR vR AS LOG NO-UNDO.

DEF BUFFER bloan FOR loan.
DEF BUFFER xterm-obl FOR term-obl.
DEF var datesince AS date no-undo.

/*
run getdate.p("���, �� ������ ���뢠�� �஢����", OUTPUT end-date).
datesince = end-date.
*/


/* �஢�ઠ ������⢠ �⮡࠭��� ����ᥩ */
FIND LAST all_recids NO-ERROR.
IF NOT AVAIL all_recids THEN
DO:
   {intrface.del }
   RETURN.
END.

IF in_op_date EQ ? THEN
   in_op_date = gend-date.

vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
   {bar-beg2.i
        &BarTotal     = vkol-bal-tot
        &BarMessage   = """�⡮� ������஢ ��� �ॣ㫨஢���� १�ࢮ�"""
   }

vkol-bal = 0.
FOR EACH all_recids, loan WHERE RECID(loan) = all_recids.rid NO-LOCK:
      mFlDel = FALSE.
      FOR EACH loan-acct of loan WHERE LENGTH(loan-acct.acct-type)>=7 AND SUBSTRING(loan-acct.acct-type,1,7) = "�।���" NO-LOCK:
          FIND FIRST op-entry WHERE 
            op-entry.op-date = in_op_date
            AND (op-entry.op-status = "�" OR op-entry.op-status = "��")
            AND  ((op-entry.acct-db = loan-acct.acct AND SUBSTRING(op-entry.acct-cr,1,3) = "706")
                OR (op-entry.acct-cr = loan-acct.acct AND SUBSTRING(op-entry.acct-db,1,3) = "706"))
            NO-LOCK NO-ERROR.
          IF AVAIL op-entry THEN DO:
            mFlDel = TRUE.
            LEAVE.
          END.
      END. 
      
      vkol-bal = vkol-bal + 1.
      /* ��㥬 �����-��� */
      {bar2.i 
          &BarPointer = vkol-bal
      }

   IF mFlDel THEN
   DO:
      DELETE all_recids.
      NEXT.
   END.

END. /*FOR EACH*/
vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.

message "�⮡࠭� " + string(vkol-bal-tot) + " ������஢"  view-as alert-box.

   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES     FILL (" ",79).
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 FILL (" ",79).
   PAUSE 0.  

{intrface.del}
