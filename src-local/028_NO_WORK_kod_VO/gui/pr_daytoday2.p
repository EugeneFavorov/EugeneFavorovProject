{globals.i}
{intrface.get tmess}

/* +++ pr_daytoday2.p was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:58pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pr_setdr_op.p
      Comment: ��楤�� ����஫�
   Parameters:
         Uses:
      Used by:
      Created: 19.08.2010 MUTA 0119107
     Modified:
*/

{globals.i}
{intrface.get xclass}

DEFINE PARAMETER BUFFER bOp FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT NO.

IF bop.op-date > today
THEN
DO:
/*MESSAGE "�� ��⠥��� �ப���஫�஢��� ���㬥�� � ���饬 ���!!!" VIEW-AS ALERT-BOX.*/
DO:
   mblodd_char_Tmp01 = pick-value.
   RUN Fill-AlertSysMes IN h_tmess("","",4,"�� ��⠥��� �ப���஫�஢��� ���㬥�� � ���饬 ���!" + CHR(32) + "~n" + CHR(32) + "�஢����� ���㬥��?").
   oResult = (pick-value = "YES").
   pick-value = mblodd_char_Tmp01.
END.

END.
ELSE oResult = YES.

oResult = IF oResult THEN oResult ELSE ?.

/* --- pr_daytoday2.p was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:58pm --- */
