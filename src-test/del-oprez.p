/*��� 㤠����� �஢���� �� ���� १�ࢮ�*/
{globals.i}
{rsrv.def}
{sh-defs.i}

DEF INPUT  PARAM pDate    AS DATE NO-UNDO.
DEF INPUT  PARAM pBefDate AS DATE NO-UNDO.

DEF INPUT-OUTPUT PARAM TABLE FOR tProv.

    
DEFINE BUFFER xop FOR op.

FIND FIRST op-kind WHERE op-kind.op-kind EQ 'rsrv_pr7' NO-LOCK NO-ERROR.

{op(ok.del
   &date     = pDate
   &op-kind  = 'rsrv_pr7'
   &set-code = "'���ᓤ��'"
   &mes1     = "� �⮬ ��� 㦥 �஢������ ����"
   &mes2     = "१�ࢮ�! �������?"
   &mes3     = "�������� ���㬥�⮢..."
}