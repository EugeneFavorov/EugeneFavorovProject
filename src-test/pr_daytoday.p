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
MESSAGE "�� ��⠥��� �ப���஫�஢��� ���㬥�� � ���饬 ���!" SKIP 
  	"�஢����� ���㬥��?" view-as alert-box warning buttons yes-no update oResult.
END.
ELSE oResult = YES.
