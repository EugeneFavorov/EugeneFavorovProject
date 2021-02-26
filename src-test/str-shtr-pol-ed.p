/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2009 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: str-shtr-pol-ed.p
      Comment: ������஢���� ����� �������� ���媮��
   Parameters:
         Uses:
      Used by:
      Created: 19.08.2009 ushd 102466
     Modified: 
*/

DEFINE INPUT PARAMETER vclass    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER in-parent AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER in-rec-id AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER level     AS INT64 NO-UNDO.

{globals.i}
{intrface.get tmess}
{intrface.get xclass}

DEFINE VAR vcodeval   AS INT64 NO-UNDO.
DEFINE VAR vcodemisc1 AS INT64 NO-UNDO.
DEFINE VAR vcodemisc2 AS LOGICAL NO-UNDO.
DEFINE BUFFER xxcode FOR code.

FORM
   vcodeval
      LABEL  "�����"
      FORMAT ">9"
      HELP   "���浪��� ����� ���� ����-����"
   vcodemisc1
      LABEL  "������"
      HELP   "������⢮ ������ ���� ����-����"
      FORMAT ">>9"
   vcodemisc2
      LABEL  "�����"
      HELP   "����஫쭮� ���� ����-����"
      FORMAT "��/���"
   code.misc[3]
      LABEL  "��������"
      HELP   "�������� ���⥦�, ������塞� �� ���뢠���"
      FORMAT "x(30)"
WITH FRAME edit 1 COL 1 DOWN SIDE-LABELS TITLE COLOR BRIGHT-WHITE "".

{rec-ed.i
   &file      = code
   &surrogate = "IF AVAIL code THEN code.class + ',' + code.code ELSE ''"
   &in-title  = "���� ��������� �����-����"
   &ef        = "str-shtr-pol-ed.uf "
   &befupd    = "str-shtr-pol-ed.bup "
   &eh        = "str-shtr-pol-ed.eh "
   &update    = "str-shtr-pol-ed.upd "
   &lookup    = "str-shtr-pol-ed.nau &class-code=vclass "
   &noxatte-ed = YES
   {&*}
}

{intrface.del}
