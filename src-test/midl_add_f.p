 /*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1999 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: midl_add.p
      Comment: ���� %-�� �⠢��, ��室� �� �।���� ���⪠ �� ��ਮ�.
      Comment: ���᫥��� % �� ��������� ��業⮭�� �⠢��
      Comment: �� �������騩�� ���⮪.
      Comment: ������ fost ������� � infiltr.p, ���㫥��� �ந��������
      Comment: �� ������ �롮� ������ ���.
   Parameters:
      Created: Om 30/08/99
     Modified: Om 01/09/99
     Modified: Om 02/11/99 "�室�騩/��室�騩" ���⮪ �१ ���४.
     Modified: ���४�஢�� ���᫥��� �।���� ���⪠.
     Modified: Om 18/11/99
     Modified: Om 09/11/00 ������祭�� �����㬥�� ���᪠ %% �⠢��.
     Modified: Om 06/12/00 ��ࠡ�⪠: ��ॢ�� �� �����㬥���� �஢���.
     Modified: Om 05/02/01 ��ࠡ�⪠: ���४�஢�� �室��� ��ࠬ��஢.
     Modified: Om 06/04/01 ��ࠡ�⪠: ��ࠡ�⪠ �訡�� ���᪠ �����ᨨ.
*/

Form "~n@(#) midl_add.p 1.0 Om  30/08/99 Om 01/09/99 Om 06/12/00"
     with frame sccs-id stream-io width 250.

{globals.i}
DEF  STREAM err.
output stream err to "spooln.tmp".
   PUT stream err UNFORMATTED
       '����'              AT 1
       '�'                 AT 25
       '��'                AT 35
       '���-�� ����'       AT 44
       '�������'           AT 63
       '������� �������'   AT 78
       '������'            AT 102
       '���������'         AT 120
       '�����'             AT 140
       SKIP.         
   PUT stream err UNFORMATTED FILL("-", 147) SKIP.
   output stream err  close.

return "".
