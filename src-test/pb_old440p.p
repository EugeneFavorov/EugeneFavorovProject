/**
����᪨� �ࠢ� �ਭ�������: ��吝�� ���� (���)
�� ������:     ����㦠�� � ��᪢�� ᮤ�ন��� 䠩��� 440-�
��� ࠡ�⠥�:   ������� ���������騥 PacketText
��ࠬ����:      
������:         30.11.2017 ���ᮢ �.�.
*/

{globals.i}
{pb_logit.i}
{intrface.get pack}

/* ������ 䠩��� �� ��⠫��� */
DEFINE VARIABLE cDir        AS CHARACTER    NO-UNDO INIT "/home2/bis/quit41d/imp-exp/0500/440p/old/".
DEFINE VARIABLE cFName      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFFull      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFType      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cStr2       AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cStr        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cText       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iFile       AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE iOK         AS INTEGER      NO-UNDO INIT 0.
DEFINE STREAM inf.

cLog = cDir + "log/"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + "-"
     + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".log".

INPUT FROM OS-DIR(cDir + "in").
REPEAT:
    IMPORT cFName cFFull cFType.

    IF (cFType = "F") AND CAN-DO("*.xml", cFName)
    THEN DO:
        RUN Logit("������ 䠩� " + cFName, cLog).
        iFile = iFile + 1.

        FOR EACH FileExch
            WHERE FileExch.Name     = cFName
            NO-LOCK,
        FIRST Packet
            WHERE Packet.FileExchID = FileExch.FileExchID
            NO-LOCK:

            FIND FIRST PacketText
                WHERE (PacketText.PacketID  = Packet.PacketID)
                NO-LOCK NO-ERROR.
            IF (AVAIL PacketText)
            THEN DO:
                RUN Logit(" - ������ ����� c ⥪�⮬, 䠩� �� ���������.", cLog).
            END.
            ELSE DO:
                cText = "".
                INPUT STREAM inf FROM VALUE(cFFull) BINARY NO-ECHO NO-CONVERT.
                READKEY STREAM inf PAUSE 0.
                DO WHILE (LASTKEY > 0):
                    cText = cText + CHR(LASTKEY).
                    READKEY STREAM inf PAUSE 0.
                END.
                INPUT STREAM inf CLOSE.

                CREATE PacketText.
                ASSIGN
                    PacketText.PacketTextID = next-value(pack-id)
                    PacketText.PacketID     = Packet.PacketID
                    PacketText.Order        = 1
                    PacketText.Contents     = REPLACE(REPLACE(cText, "~r~n", "~r"), "~r", "~r~n")
                    .
                RUN Logit(" + ������ ����� ��� ⥪��. ���� ��������.", cLog).
                iOK = iOK + 1.
            END.
        END.

        IF (NOT AVAIL Packet)
        THEN DO:
            RUN Logit(" - ����� �� ������.", cLog).
        END.

        OS-COPY VALUE(cFFull)  VALUE(cDir + "arc").
        OS-DELETE VALUE(cFFull).
    END.
END.

INPUT CLOSE.
MESSAGE "��ࠡ�⠭� " + STRING(iFile) + " 䠩���. �ᯥ譮 ����㦥�� " + STRING(iOK) + " ��."
    VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
{intrface.del}
