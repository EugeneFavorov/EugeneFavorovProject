/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: OPKAUFLT.P
      Comment:
   Parameters:
         Uses:
      Used by:
      Created: ... ����� �����
     Modified: 21.02.2003 16:48 SEMA     �� ��� 0014217 ���������� ����� ��ࠬ��஢ � 䨫��� �� ���㬥�⠬
     Modified: 05.05.2003 15:27 SEMA     �� ��� 0014217 �������� ���ᠭ��
     Modified: 14.07.2003 kraw (0018600) �⮡� or&and �� ���ਭ������ ��� ���४�����
     Modified: 23.07.2003 ilvi (14217) ��������� ���� � 䨫��� PrintProc - ��楤�� ����
     Modified: 09.07.2004 ilvi (21766) �������஢�� ���. ��������� ���� �㡠����⨪�
     Modified: 24.04.2005 xaro         �� ��� 0034389
     Modified: 09.07.2005 Om  ��ࠡ�⪠.
                        � �������� "������/��ᯮ��" ��������� ���� "�६� �ᯮ��".
                        ��� ����� "�६� �ᯮ��" � "�६� ������" ����� ��������
                        ���ࢠ�.
     Modified: 09.10.2007 Muta 0080920 ��� ��㧥� �஢���� ��������� ����������� �⮡���
                                       �஢���� ����騥/�� ����騥 �易���� ��ꥪ⮢.
*/

DEFINE INPUT PARAMETER iClassCode AS CHAR NO-UNDO.

{globals.i}
{flt-file.i}
{intrface.get tmess}

DEFINE VARIABLE list-class AS CHARACTER NO-UNDO.    /* ����室��� ��� flt-file.end */
DEFINE VARIABLE num-class  AS INTEGER   NO-UNDO.    /* ���稪 */

{flt-file.add
   &cat     = 10
   &asgn    = YES
   &tablef  = "'op'"
   &labelt  = "'��������'"
   &include = ""op,mPos,op-date,user-id,user-inspector,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,name-ben,op-error,details,inn""
   &double  =  ""mPos""
   &classf  = "'opokau'"
   &sortf   = ""mPos""
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op'"
   &a-code-value = "'*'"
   &a-initial    = "'*'"
   &a-datatype   = "'character'"
   &a-multi      = YES
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'mPos1'"
   &a-basic      = "''"
   &a-datatype   = "'decimal'"
   &a-format     = "'>>>>>>>>9.99'"
   &a-label      = "'���. �� ���.��. ��:'"
   &a-help       = "'���⮪ �� �����ᮢ�� ��� ��'"
   &a-param      = "'mPos1'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'mPos2'"
   &a-basic      = "''"
   &a-datatype   = "'decimal'"
   &a-format     = "'>>>>>>>>9.99'"
   &a-initial    = "'999999999.99'"
   &a-label      = "'���. �� ���.��. ��:'"
   &a-help       = "'���⮪ �� �����ᮢ�� ��� ��'"
   &a-param      = "'mPos2'"
}

{flt-file.end}
