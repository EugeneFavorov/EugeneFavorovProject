/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cfobrw.FRM
      Comment: ��� - ���
   Parameters:
         Uses:
      Used by:
      Created: ayv
     Modified:    
*/

FORM
   mBranchID	
	  VIEW-AS FILL-IN SIZE 4 BY 1
      FORMAT "x(4)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ�������"
   mCFO
      VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "��� ���"
      HELP "��� ���"
   code.{&ShortName}
	  VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(150)"
      LABEL "��⪮� ����."
      HELP "��⪮� ������������ ���ࠧ������� (�� ���!)"
  code.{&Status}
      FORMAT "x(3)"
      VIEW-AS COMBO-BOX LIST-ITEMS "��", "�", "�"
      LABEL "���⨥"
      HELP "���⨥ ��� � �ਢ��祭�� � ���㦨�����"
  mStartDate
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� �����"
	  COLUMN-LABEL "���� �����"
      HELP "��� ����� ��� � ����⢨�"
   mEndDate
	  VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� ����砭��"
	  COLUMN-LABEL "���� ���������"
      HELP "��� ����砭�� ����⢨� ���� ���"
   code.{&ProdType}	
      VIEW-AS COMBO-BOX LIST-ITEMS "��⮪।", "��稥", "��⮪।,��稥"
      FORMAT "x(40)"
      LABEL "���� �த�⮢"
      HELP "���� �த�⮢"
WITH FRAME edit WIDTH 80 side-label
     TITLE COLOR bright-white "[ ��� ��� ]".

FORM
   str-recid 
	  COLUMN-LABEL "�"
   mBranchID	
      FORMAT "x(4)"
	  LABEL "��� ���ࠧ�������"
      COLUMN-LABEL "���"
	  HELP "��� ���ࠧ�������"
   code.{&ShortName}
	  FORMAT "x(90)"
      LABEL "��⪮� ����."
	  COLUMN-LABEL "������� ������������ �������������"
      HELP "��⪮� ������������ ���ࠧ�������"
   mCFO
      FORMAT "x(15)"
      LABEL "��� ���"
	  COLUMN-LABEL "��� ���"
      HELP "��� ���"
   code.{&Status}
      FORMAT "x(3)"
      LABEL "���⨥"
	  COLUMN-LABEL "�������"
      HELP "���⨥ ��� � �ਢ��祭�� � ���㦨�����"
   mStartDate
	  FORMAT "99/99/9999"
      LABEL "��� �����"
	  COLUMN-LABEL "���� �����"
      HELP "��� ����� ��� � ����⢨�"
   mEndDate
	  FORMAT "99/99/9999"
      LABEL "��� ����砭��"
	  COLUMN-LABEL "���� ���������"
      HELP "��� ����砭�� ����⢨� ���� ���"
   code.{&ProdType}	
      FORMAT "x(50)"
      LABEL "���� �த�⮢"
	  COLUMN-LABEL "���� ���������"
      HELP "���� �த�⮢"  
   SPACE(0)
WITH FRAME browse1 WIDTH 190
     TITLE COLOR bright-white "[ ���������� ��� ]".

FORM
   code.code   
      COLUMN-LABEL  "��� ���������" 
      FORMAT "x(13)" 
   code.name   
      COLUMN-LABEL  "������������ ���������" 
      FORMAT "x(39)"
      VIEW-AS FILL-IN SIZE 39 BY 1  
   code.val SPACE(0)
   str-recid 
      COLUMN-LABEL "�"
WITH FRAME browse2 WIDTH 190
     TITLE COLOR bright-white "[ ���������� ��� ]".

FORM
   mFltBranchID	
	  VIEW-AS FILL-IN SIZE 4 BY 1
      FORMAT "x(4)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ�������"
   mFltShortName
	  VIEW-AS FILL-IN SIZE 40 BY 1
      FORMAT "x(150)"
      LABEL "��⪮� ����."
      HELP "��⪮� ������������ ���ࠧ�������"
   mFltCFO
      VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "��� ���"
      HELP "��� ���"
   mFltStatus
      FORMAT "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "���⨥"
      HELP "���⨥ ��� � �ਢ��祭�� � ���㦨�����"
   mFltStartDate1
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� �����  �"
	  COLUMN-LABEL "���� ����� �"
      HELP "��� ����� ��� � ����⢨�"
   mFltStartDate2
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� ����� ��"
	  COLUMN-LABEL "���� ����� ��"
      HELP "��� ����� ��� � ����⢨�"
   mFltEndDate1
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� �����.  �"
	  COLUMN-LABEL "���� ��������� �"
      HELP "��� ����砭�� ����⢨� ���� ���" 
   mFltEndDate2
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "��� �����. ��"
	  COLUMN-LABEL "���� ��������� ��"
      HELP "��� ����砭�� ����⢨� ���� ���" 
   mFltProdType	
      VIEW-AS FILL-IN SIZE 40 BY 1
      FORMAT "x(40)"
      LABEL "���� �த�⮢"
      HELP "���� �த�⮢"
WITH FRAME flt-fr 1 COL OVERLAY CENTERED side-label ROW 6
	TITLE COLOR bright-white "[ ������ ]".      
