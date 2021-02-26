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
/*
form "~n@(#) loanfrm.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{flt_var.def} /* ��।������ Shared ��६����� */

do transaction
on error  undo, leave
on endkey undo, leave:

    update 
        all_settings.flt_first_num
            label "����� ���㬥��"
            help  "����� ��ࢮ�� ���㬥��"
        svPlanDate
            label "�������� ���"
            help "�������� ��� ���㬥��"
    with frame in_flt 1 col overlay centered row 11 title "[ �������� ������ ]".
end.

/* ����뢠� �३� */
hide frame in_flt no-pause.



/* �᫨ �訡�� ��� �⬥�� �����,
** � ��宦�  */
if last-event:function ne "GO"
then return "-1".
*/

{flt_var.def} /* ��।������ Shared ��६����� */


/* in-op-date = today - 1.        */
IF NUM-ENTRIES(session:parameter) GE 8 THEN 
ASSIGN
   svPlanDate = IF ENTRY(8,session:parameter) NE "*" THEN DATE(ENTRY(8,session:parameter))  ELSE  today - 1.
ELSE 
   svPlanDate = today - 1.

all_settings.flt_first_num = 1.
  
return.
