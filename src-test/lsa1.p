
                   /*******************************************
                    *                                         *
                    *  ������� ������������ � �������������!  *
                    *                                         *
                    *  ������������� ������ ���� ����������,  *
                    *  �.�. �� ��������� ����������� �������  *
                    *             �������������!              *
                    *                                         *
                    *******************************************/

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2011 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: lsa1.p
      Comment: ����, ᮧ����� ������஬ ���⮢
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 15/02/11 16:26:28
     Modified:
*/
Form "~n@(#) lsa1.p 1.0 RGen 15/02/11 RGen 15/02/11 [ AutoReport By R-Gen ]"
     with frame sccs-id stream-io width 250.

{globals.i}
{chkacces.i}
/*-------------------- �室�� ��ࠬ���� --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- ������� ��६����� --------------------*/

Define Buffer buf_0_op               For op.

/*--------------- ���� ��� ����� ��: ---------------*/

/*--------------- ��६���� ��� ᯥ樠���� �����: ---------------*/
Define Variable Bank-name        As Character            No-Undo.
Define Variable drag-b           As Decimal              No-Undo.
Define Variable drag-o           As Decimal              No-Undo.
Define Variable k-b              As Decimal              No-Undo.
Define Variable k-drag-b         As Decimal              No-Undo.
Define Variable k-drag-o         As Decimal              No-Undo.
Define Variable k-o              As Decimal              No-Undo.
Define Variable kinv-b           As Decimal              No-Undo.
Define Variable kinv-o           As Decimal              No-Undo.
Define Variable tot-b            As Decimal              No-Undo.
Define Variable tot-o            As Decimal              No-Undo.
Define Variable val-b            As Decimal              No-Undo.
Define Variable val-o            As Decimal              No-Undo.
Define Variable vBalSum          As Decimal              No-Undo.
Define Variable vDateString      As Character            No-Undo.
Define Variable vNBalSum         As Decimal              No-Undo.

/*--------------- ��।������ �� ��� 横��� ---------------*/

/* ��砫�� ����⢨� */
{lsa.i}

/*-----------------------------------------
   �஢�ઠ ������ ����� ������� ⠡����,
   �� ������ 㪠�뢠�� Input Param RID
-------------------------------------------*/
Find op Where RecID(op) = RID no-lock no-error.
If Not Avail(op) then do:
  message "��� ����� <op>".
  Return.
end.

/*------------------------------------------------
   ���⠢��� buffers �� �����, ��������
   � ᮮ⢥��⢨� � ������묨 � ���� �ࠢ�����
------------------------------------------------*/
/* �.�. �� ������ �ࠢ��� ��� �롮ન ����ᥩ �� ������� ⠡����,
   ���� ���⠢�� ��� buffer �� input RecID                    */
find buf_0_op where RecID(buf_0_op) = RecID(op) no-lock.

/*------------------------------------------------
   ���᫨�� ���祭�� ᯥ樠���� �����
   � ᮮ⢥��⢨� � ������묨 � ���� �ࠢ�����
------------------------------------------------*/
/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� Bank-name */
{get_set.i "����"}

    assign

       bank-name = setting.val

    .

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� drag-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� drag-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� k-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� k-drag-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� k-drag-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� k-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� kinv-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� kinv-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� tot-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� tot-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� val-b */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� val-o */
/* ����� ��砫�� ����⢨� */

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� vBalSum */
VBalSum = vBal.

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� vDateString */
vDateString = GetDateString(end-date).

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� vNBalSum */
VNBalSum = vNBal.

/*-------------------- ��ନ஢���� ���� --------------------*/
{strtout3.i &cols=83 &option=Paged}

put unformatted "�ப �࠭����   __________________________________________________" skip.
put unformatted " " skip.
put unformatted "��娢�� ������ __________________________________________________" skip.
put unformatted " " skip.
put skip(1).
put unformatted "" Bank-name Format "x(82)"
                "" skip.
put unformatted "   (������ ��� ᮪�饭��� �ଥ���� ������������ �।�⭮� �࣠����樨 �(���)" skip.
put unformatted "                              ������������ 䨫����)" skip.
put skip(3).
put unformatted "���㬥��� �� " vDateString Format "x(27)"
                "" skip.
put skip(3).
put unformatted "                                �� �����ᮢ� ��⠬       �� ��������ᮢ� ��⠬" skip.
put skip(1).
put unformatted " " skip.
put unformatted "�㬬�                       " tot-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " tot-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(2).
put unformatted "�� ���:" skip.
put unformatted "�࠭���� �� �㬠���� ���⥫� � ��室���� � �⤥���� ������:" skip.
put skip(1).
put unformatted "���ᮢ� ���㬥���          " k-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " k-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(2).
put unformatted "    �� ������ � �����࠭��� ����⮩:" skip.
put skip(1).
put unformatted "��壠���᪨� ���㬥���     " val-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " val-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(1).
put unformatted "     �ப �࠭����  ____________" skip.
put unformatted " " skip.
put unformatted "���ᮢ� ���㬥���          " kinv-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " kinv-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(1).
put unformatted "     �ப �࠭����  ____________" skip.
put unformatted " " skip.
put skip(1).
put unformatted "    �� ������ � �ࠣ�業�묨 ��⠫����:" skip.
put unformatted " " skip.
put unformatted "��壠���᪨� ���㬥���     " drag-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " drag-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(1).
put unformatted "     �ப �࠭����  ____________" skip.
put skip(1).
put unformatted "���ᮢ� ���㬥���          " k-drag-b Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " k-drag-o Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(1).
put unformatted "     �ப �࠭����  ____________" skip.
put unformatted " " skip.
put unformatted "�࠭���� � ���஭��� ����:" skip.
put unformatted " " skip.
put unformatted "���ᮢ� ���㬥���          _______________________________________________________" skip.
put skip(1).
put unformatted "��壠���᪨� ���㬥���     " vBalSum Format "->>>,>>>,>>>,>>9.99"
                "  ��.     " vNBalSum Format "->>>,>>>,>>>,>>9.99"
                "  ��." skip.
put skip(1).
put unformatted " " skip.
put skip(1).
put unformatted "���㬥��� ����஢��� � ������ __________________________________________________" skip.
put skip(1).
put unformatted "___________________________________________________________________________________" skip.
put unformatted "     (������� ��壠���᪮�� ࠡ�⭨��, �����⢨�襣� �訢 � �஢��� �������" skip.
put unformatted "                            ����஢����� ���㬥�⮢)" skip.
put skip(1).
put unformatted "� ����묨 ��壠���᪮�� ��� ᢥ७� ___________________" skip.
put unformatted "                                            (�������)" skip.

/* ������ ����⢨� */
{lb_dtl.i}


{endout3.i &nofooter=yes}

