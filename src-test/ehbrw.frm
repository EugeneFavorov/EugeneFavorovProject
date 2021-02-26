/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ehbrw.FRM
      Comment: ���㬥��� ��� ���஭���� �࠭���� - ���
   Parameters:
         Uses:
      Used by:
      Created: ayv
     Modified:    
*/

FORM
   code.{&BranchDb}
     VIEW-AS FILL-IN SIZE 20 BY 1
      FORMAT "x(20)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ������� ��� �����"
   code.{&AcctDb}
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� �����"
      HELP "��᪠ ��⮢ �����"
   code.{&BranchCr}
     VIEW-AS FILL-IN SIZE 20 BY 1
      FORMAT "x(20)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ������� ��� �।��"
   code.{&AcctCr}
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� �।��"
      HELP "��᪠ ��⮢ �।��"    
   code.{&DocType}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "��� ���㬥��"
      HELP "��� ���㬥��"
   code.{&Currency}
     VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "�����"
      HELP "����� ���㬥��"
   code.{&OpKind}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "�࠭�����"
      HELP "�࠭�����, ᮧ����� ���㬥��"
   code.{&Details}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "������������ ���⥦�"
      HELP "������������ ���⥦�"
   code.{&Param}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "��ࠬ���"
      HELP "�������⥫�� ��ࠬ��� (��. ���୥�-����)"
   mStartDate
     VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT "99/99/9999"
      LABEL "��� �����"
      HELP "��� ����� �᫮��� � ����⢨�"
   mEndDate
     VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT "99/99/9999"
      LABEL "��� ����砭��"
      HELP "��� ����砭�� ����⢨� �᫮���"
WITH FRAME edit WIDTH 80 side-label
     TITLE COLOR bright-white "[ ������� ]".

FORM
   str-recid 
	   COLUMN-LABEL "�"
   code.{&BranchDb}
      FORMAT "x(10)"
      COLUMN-LABEL "��� ���."
      HELP "��� ���ࠧ������� ��� �����"
   code.{&AcctDb}
      FORMAT "x(20)"
      LABEL "��� �����"
      COLUMN-LABEL "�����"
      HELP "��᪠ ��⮢ �����" 
   code.{&BranchCr}
      FORMAT "x(10)"
      COLUMN-LABEL "��� ���."
      HELP "��� ���ࠧ������� ��� �।��"
   code.{&AcctCr}
      FORMAT "x(20)"
      LABEL "��� �।��"
      COLUMN-LABEL "������"
      HELP "��᪠ ��⮢ �।��"   
   code.{&DocType}
      FORMAT "x(10)"
      LABEL "��� ���㬥��"
      COLUMN-LABEL "��� ���."
      HELP "��� ���㬥��"
   code.{&Currency}
      FORMAT "x(10)"
      LABEL "�����"
      COLUMN-LABEL "������"
      HELP "����� ���㬥��"
   code.{&OpKind}
      FORMAT "x(10)"
      LABEL "�࠭�����"
      COLUMN-LABEL "����������"
      HELP "�࠭�����, ᮧ����� ���㬥��"
   code.{&Details}
      FORMAT "x(15)"
      LABEL "������������ ���⥦�"
      COLUMN-LABEL "����. �������"
      HELP "������������ ���⥦�"
   code.{&Param}
      FORMAT "x(15)"
      LABEL "��ࠬ���"
      COLUMN-LABEL "��������"
      HELP "�������⥫�� ��ࠬ��� (��. ���୥�-����)"
   mStartDate
      FORMAT "99/99/9999"
      LABEL "��� �����"
      COLUMN-LABEL "���� �����"
      HELP "��� ����� �᫮��� � ����⢨�"
   mEndDate
      FORMAT "99/99/9999"
      LABEL "��� ����砭��"
      COLUMN-LABEL "���� ���������"
      HELP "��� ����砭�� ����⢨� �᫮���"
   /*SPACE(0)*/
WITH FRAME browse1 WIDTH 300
     TITLE COLOR bright-white "[ ��������� ��� ������������ �������� ]".

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
     TITLE COLOR bright-white "[ ��������� ��� ������������ �������� ]".

FORM
   mFltBranchDb
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ������� ��⮢ �����" 
   mFltAcctDb
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� �����"
      HELP "��᪠ ��⮢ �����" 
   mFltBranchCr
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� ���ࠧ�������"
      HELP "��� ���ࠧ������� ��⮢ �।��" 
   mFltAcctCr
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "��� �।��"
      HELP "��᪠ ��⮢ �।��"  
   mFltDocType
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "��� ���㬥��"
      HELP "��� ���㬥��"
   mFltCurrency
     VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "�����"
      HELP "����� ���㬥��"
   mFltOpKind
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "�࠭�����"
      HELP "�࠭�����, ᮧ����� ���㬥��"
   mFltOpKind
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "�࠭�����"
      HELP "�࠭�����, ᮧ����� ���㬥��"
   mFltDetails
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "������. ���⥦�"
      HELP "������������ ���⥦�"
   mFltParam
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "��ࠬ���"
      HELP "�������⥫�� ��ࠬ��� (��. ���୥�-����)"
   mFltActual
     VIEW-AS COMBO-BOX LIST-ITEMS "��", "���"
      FORMAT "x(4)"
      LABEL "���㠫�� �᫮���"
      HELP "�����뢠�� �� ⮫쪮 ���㠫�� �᫮���"
   mFltStartDate1
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "��� �����  �"
	   HELP "��� ����� ��� � ����⢨�"
   mFltStartDate2
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "��� ����� ��"
	   HELP "��� ����� ��� � ����⢨�"
   mFltEndDate1
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "��� �����.  �"
	   HELP "��� ����砭�� ����⢨� ���� ���" 
   mFltEndDate2
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "��� �����. ��"
	   HELP "��� ����砭�� ����⢨� ���� ���" 
WITH FRAME flt-fr 1 COL OVERLAY CENTERED side-label ROW 6
	TITLE COLOR bright-white "[ ������ ]".