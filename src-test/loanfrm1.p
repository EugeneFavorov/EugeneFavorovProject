/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: loanfrm.p
      Comment: ���� ��砫쭮� ���ଠ樨 ��� 䨫��� �� ������ࠬ.
   Parameters: "-1" - �⬥�� �����.
         Uses: all_flt.p
      Used by: 
      Created: 06/03/00 Om 
     Modified: 16/05/00 Om ���� ��ࢮ�� ����� ���㬥��.
     Modified: 27/11/01 Om ��ࠡ�⪠: ��ࠡ�⪠ �������� ����.
     Modified: 27/11/01 Om �訡��: �� �⬥�� ����� ��� �訡�� �� �����
                                   ���� ����� �� ����뢠����.
*/
form "~n@(#) loanfrm.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{flt_var.def} /* ��।������ Shared ��६����� */

all_settings.flt_first_num = 1.
svPlanDate = today.
return.
