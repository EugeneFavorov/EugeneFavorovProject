/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: title_doc.p
      Comment: �����䨪��� "������ਢ�" ����� �ਢ��祭�� ������
   Parameters:
         Uses:
      Used by:
      Created: ayv 
     Modified: 
*/
&GLOBAL-DEFINE PClassDRDynamic YES
&GLOBAL-DEFINE SpecClass       "������ਢ�"
&GLOBAL-DEFINE wherecd0        AND DATE(code.misc[1]) LE TODAY AND (code.misc[2] EQ '' OR DATE(code.misc[2]) GT TODAY)
&GLOBAL-DEFINE help-label      "F1|F3 �� ����|F6 䨫���|F9|Ins|Del|' ',^A,-,* �뤥�."

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.code
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(13)
&GLOBAL-DEFINE DRVarLabel{&num}    ���!���.��ࠬ��� 
&GLOBAL-DEFINE DRVarELabel{&num}   ��� 
&GLOBAL-DEFINE DRVarHelp{&num}     ��� �������⥫쭮�� ��ࠬ���
&GLOBAL-DEFINE DRVarNoUpdate{&num} YES

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    ������������!������
&GLOBAL-DEFINE DRVarELabel{&num}   ������������
&GLOBAL-DEFINE DRVarHelp{&num}     ������������ ������

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    ���!�����
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ����� 
&GLOBAL-DEFINE DRVarHelp{&num}     ��� �����

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    ���!�뢮��
&GLOBAL-DEFINE DRVarELabel{&num}   ��� �뢮�� 
&GLOBAL-DEFINE DRVarHelp{&num}     ��� �뢮��

{pclassdr.p}
