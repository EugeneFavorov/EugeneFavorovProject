/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2012 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: vp_otdocfvi.p
      Comment: �����䨪��� "��_�⡮����"
   Parameters:
         Uses:
      Used by:
      Created: 18.03.2013     
     Modified: 18.03.2013   
*/

{vp_otdoc.def }
{globals.i}
{intrface.get xclass}
{ttretval.def}

DEFINE INPUT  PARAMETER ipTitle  AS CHARACTER NO-UNDO. /* ��������� ��� �३�� */
DEFINE INPUT  PARAMETER ipLevel  AS INT64     NO-UNDO.  /* �஢��� �३�� */
DEFINE OUTPUT PARAMETER opResult AS INT64     NO-UNDO.

ASSIGN
   opResult = -1 /* ��室 �� ESC */
   mMode    = IF NUM-ENTRIES(ipTitle,"|") GT 1
              THEN ENTRY(2,ipTitle,"|")
              ELSE ""
   ipTitle  = ENTRY(1,ipTitle,"|")
.

IF ipTitle EQ ? OR ipTitle EQ "" THEN 
   ipTitle = "��ࠬ���� 䨫���".

FORM

   "������ �� ������: " vBalDb                    
       HELP "������ ���� ��⮢ 2�� ���浪� �� ������"  SKIP
   "�/���: " vTypRun   
       FORMAT "�/���"                                     SKIP
   "������ �� �।���: "  vBalCr
       HELP "������ ���� ��⮢ 2�� ���浪� �� �।���" SKIP(1)
   "����ঠ��� ����樨 (���. ॣ.): "   vDetailReg      SKIP(1)
   "����ঠ��� ����樨 (�� ���. ॣ.): " vDetailNoReg   SKIP(1) 
   "�㬬� ��: "                             vSumOt        SKIP(1)
   "�㬬� ��: "                             vSumDo        SKIP 

WITH FRAME fFilterFrame NO-LABELS TITLE COLOR bright-white "[ " + ipTitle + " ]" CENTERED ROW ipLevel OVERLAY.


ON 'F1' OF vTypRun IN FRAME fFilterFrame 
DO:
   vTypRun = NOT vTypRun.
   DISPLAY vTypRun WITH FRAME fFilterFrame.
   RETURN NO-APPLY.
END.

PAUSE 0.
sss:
DO ON ERROR UNDO sss, LEAVE sss ON ENDKEY UNDO sss, LEAVE sss:
    DO WITH FRAME fFilterFrame:
        DISP vBalDb  vTypRun vBalCr vDetailReg vDetailNoReg vSumOt vSumDo  WITH FRAME fFilterFrame.

        IF mMode NE "modeview" THEN DO:
           ENABLE vBalDb vTypRun vBalCr vDetailReg vDetailNoReg vSumOt vSumDo.
           WAIT-FOR GO, WINDOW-CLOSE OF CURRENT-WINDOW FOCUS vBalDb.
        END.
    END.
END.

ASSIGN vBalDb vBalCr vDetailReg vDetailNoReg vTypRun vSumOt vSumDo.

HIDE FRAME fFilterFrame.
IF LASTKEY NE 10 AND LASTKEY NE 13 THEN RETURN.
opResult = 0.
