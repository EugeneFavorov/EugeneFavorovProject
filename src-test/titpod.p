/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: title_doc.p
      Comment: �����䨪��� "���㫄�����" ����ன�� ���쭮�� ���� �訢� ���㬥�⮢ ���
   Parameters:
         Uses:
      Used by:
      Created: 14.02.2011 kraa 0130908 
     Modified: 
*/
&GLOBAL-DEFINE PClassDRDynamic YES
&GLOBAL-DEFINE SpecClass       "��⏮�ࠧ�"

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.code
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(10)
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    ��� ����.
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ���ࠧ�����.
&GLOBAL-DEFINE DRVarHelp{&num}     ��� ���ࠧ�������

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    ������������
&GLOBAL-DEFINE DRVarHelp{&num}     ������������ ���ࠧ�������

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.val
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 45 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    �訢
&GLOBAL-DEFINE DRVarHelp{&num}     �訢�, ����� ����室��� ������ ��� ���ࠧ�������
 
{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(1)
&GLOBAL-DEFINE DRVarFormat{&num}   x(1)
&GLOBAL-DEFINE DRVarLabel{&num}    ���冷�
&GLOBAL-DEFINE DRVarHelp{&num}     ���冷� ���஢��

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     LOGICAL
&GLOBAL-DEFINE DRVarFormat{&num}   ��/���
&GLOBAL-DEFINE DRVarLabel{&num}    �����
&GLOBAL-DEFINE DRVarHelp{&num}     ������ �� ���ࠧ������� � ᮡ�⢥���� ���쭮� ����

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[3]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(10)
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    ������
&GLOBAL-DEFINE DRVarELabel{&num}   ������
&GLOBAL-DEFINE DRVarHelp{&num}     ������, � ���஬� �⭮���� ���ࠧ�������

{pclassdr.p}
