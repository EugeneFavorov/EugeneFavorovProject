/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     ��������� ����� �����
���� ����᪠:  ��������� - Ctrl-G
������:         24.10.2017 ���ᮢ �.�.
*/

{globals.i}
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */

DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

FOR FIRST tmprecid 
    NO-LOCK,
FIRST Packet
    WHERE (RECID(Packet)        EQ tmprecid.id)
    EXCLUSIVE-LOCK:

    RUN g-prompt.p ("CHARACTER", "�����", "x(20)", "����", "������ ���� �����", 25, ",", "", ?,?,OUTPUT cTmp).
    IF (cTmp EQ ?) THEN RETURN.

    Packet.State = cTmp.
END.
