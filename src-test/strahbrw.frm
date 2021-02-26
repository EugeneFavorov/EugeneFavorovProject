/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: strahbrw.FRM
      Comment: �����⥫� ���客�� �६�� - ���
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

FORM
   mCustCat
      FORMAT "x(1)"
      VIEW-AS COMBO-BOX LIST-ITEMS "�", "�", "�","�"
      LABEL "���"
      HELP "��� ������"
   mCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "���"
      HELP "��� ������"
   code.{&WhiteINN}                             /* ��� */
      VIEW-AS FILL-IN SIZE 12 BY 1
      FORMAT "x(12)"
      LABEL "���"
      HELP "���"
   code.{&ShortName}                            /* ��⪮� ������������ */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "��⪮� ������������"
      HELP "��⪮� ������������ ������"
   code.{&WhiteName}                            /* ���/������������ */
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "���/������������"
      HELP "��� ��� ������������ ������"
   mRaschAcct                                   /* ������ ��� */
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "����.���"
      HELP "����� ���⭮�� ���"
   code.{&BankBIC}                              /* ��� ����� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "��� �����"
      HELP "��� �����"
   code.{&BankName}                            /* ������������ ����� */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "������������ �����"
      HELP "������������ �����"
   code.{&CorrAcct}                           /* ����. ��� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "�������"
      HELP "����ᯮ�����᪨� ��� � ���"
   code.{&Telefax}                            /* ����䮭,䠪� */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "����䮭, 䠪�"
      HELP "����䮭,䠪�"
   code.{&DocType}                            /* ��� ���㬥�� */
      FORMAT "x(50)"
      VIEW-AS FILL-IN SIZE 50 BY 1
      LABEL "��� ���㬥��"
      HELP "��� ���㬥��"
   code.{&DocCustCode}                        /* N ���㬥�� */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "N ���㬥��"
      HELP "N ���㬥��"
   code.{&DocOpenDate}                        /* ��� �뤠� ���㬥�� */
      FORMAT "99.99.9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "��� �뤠�"
      HELP "��� �뤠� ���㬥��"
   code.{&DocIssue}                        /* �뤠� ���㬥�� */
      FORMAT "x(300)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "�뤠�"
      HELP "���㬥�� �뤠�"
   code.{&BirthDay}                        /* ��� ஦����� */
      FORMAT "99.99.9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "��� ஦�����"
      HELP "��� ஦�����"
   code.{&Address}                        /* ���� */
      FORMAT "x(300)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "����"
      HELP "����"
WITH FRAME edit.

FORM                  
   mCustCat
      FORMAT "x(1)"
      LABEL "���"
      COLUMN-LABEL "���"
      HELP "��� ������"
   mCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "���"
      COLUMN-LABEL "���"
      HELP "��� ������"
   code.{&WhiteINN}                             /* ��� */
      FORMAT "x(12)"
      VIEW-AS FILL-IN SIZE 12 BY 1
      LABEL "���"
      COLUMN-LABEL "���"
      HELP "���"
   code.{&ShortName}                            /* ��⪮� ������������ */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "��⪮� ������������"
      COLUMN-LABEL "��⪮� ������������"
      HELP "��⪮� ������������ ������"
   code.{&WhiteName}                            /* ���/������������ */
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "���/������������"
      COLUMN-LABEL "���/������������"
      HELP "��� ��� ������������ ������"
   mRaschAcct                                   /* ������ ��� */
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "����.���"
      COLUMN-LABEL "����.���"
      HELP "����� ���⭮�� ���"
   code.{&BankBIC}                              /* ��� ����� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "��� �����"
      HELP "��� �����"
   code.{&BankName}                            /* ������������ ����� */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "������������ �����"
      HELP "������������ �����"
   code.{&CorrAcct}                           /* ����. ��� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "�������"
      HELP "����ᯮ�����᪨� ��� � ���"
   code.{&Telefax}                            /* ����䮭,䠪� */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "����䮭, 䠪�"
      HELP "����䮭,䠪�"      
   ch COLUMN-LABEL ""
   SPACE(0)
WITH FRAME browse1 TITLE "" WIDTH 205.

FORM
   code.code   
      COLUMN-LABEL  "��� ���������" 
      FORMAT "x(13)" 
   code.name   
      COLUMN-LABEL  "������������ ���������" 
      FORMAT "x(39)"
      VIEW-AS FILL-IN SIZE 39 BY 1  
   code.val SPACE(0)
   ch 
      COLUMN-LABEL ""
WITH FRAME browse2 TITLE COLOR bright-white "".

FORM
   mFltCustCat
      FORMAT "x(1)"
      LABEL "���"
      HELP "��� ������"
   mFltCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "���"
      HELP "��� ������"
   mFltInn
      FORMAT "x(12)"
      VIEW-AS FILL-IN SIZE 12 BY 1
      LABEL "���"
      HELP "���"
   mFltShortName
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "��⪮� ������������"
      HELP "��⪮� ������������ ������"
   mFltName
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "���/������������"
      HELP "��� ��� ������������ ������"
   mFltRaschAcct
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "����.���"
      HELP "����� ���⭮�� ���"
   mFltBankBIC                              /* ��� ����� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "��� �����"
      HELP "��� �����"
   mFltBankName                            /* ������������ ����� */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "������������ �����"
      HELP "������������ �����"
   mFltCorrAcct                           /* ����. ��� */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "�������"
      HELP "����ᯮ�����᪨� ��� � ���"
   mFltTelefax                            /* ����䮭,䠪� */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "����䮭, 䠪�"
      HELP "����䮭,䠪�"  
WITH CENTERED 1 COLUMNS 
     FRAME flt-fr OVERLAY ROW 7 SIDE-LABELS TITLE "������" WIDTH 205 .
      
