/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cfobrw.eh
      Comment: ��� - �ਣ���� ���
   Parameters:
         Uses:
      Used by:
      Created: ayv   
     Modified:    
*/

ON LEAVE OF mBranchID IN FRAME edit
DO:
    mDummy = mBranchID:SCREEN-VALUE.
	IF SUBSTRING(mDummy,1,1) NE "0" THEN
		MESSAGE "���ࠢ���� ��� ���ࠧ�������!" VIEW-AS ALERT-BOX.
END.