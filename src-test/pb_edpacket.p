/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     ������஢���� ����� ����ᮢ 440-�
���� ����᪠:  ��������� - Ctrl-G
������:         24.10.2017 ���ᮢ �.�.
*/

{globals.i}
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */

DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE VARIABLE J           AS INTEGER      NO-UNDO.

FOR FIRST tmprecid 
    NO-LOCK,
FIRST Packet
    WHERE (RECID(Packet)        EQ tmprecid.id)
    NO-LOCK,
FIRST PacketText
    WHERE (PacketText.PacketID  EQ Packet.PacketID)
    EXCLUSIVE-LOCK:

    RUN g-prompt.p ("CHARACTER", "����", "x(20)", "�������", "����� ���� ���� ��ࠢ���", 40, ",", "", ?,?,OUTPUT cTmp).
    IF (cTmp EQ ?) THEN RETURN.

    I = INDEX(PacketText.Contents, CODEPAGE-CONVERT(cTmp, "1251")).
    IF (I = 0)
    THEN DO:
        MESSAGE "���� '" + cTmp + "' �� �������."
            VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
        RETURN.
    END.

    I = INDEX(PacketText.Contents, '"', I) + 1.
    J = INDEX(PacketText.Contents, '"', I).
    IF (I = 1) OR (J = 0) THEN RETURN.

    MESSAGE "Point - " I
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
    MESSAGE SUBSTRING(CODEPAGE-CONVERT(PacketText.Contents, SESSION:CHARSET, "1251"), I)
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
    RUN g-prompt.p ("CHARACTER", "���祭��", "x(200)",
        CODEPAGE-CONVERT(SUBSTRING(PacketText.Contents, I, J - I), SESSION:CHARSET, "1251"),
        "������ ����� ���祭��", 70, ",", "", ?,?,OUTPUT cTmp).
    IF (cTmp EQ ?) THEN RETURN.

    MESSAGE "Point - " + SUBSTRING(PacketText.Contents, I, J - I)
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
    
    SUBSTRING(PacketText.Contents, I, J - I) = CODEPAGE-CONVERT(cTmp, "1251").
END.
