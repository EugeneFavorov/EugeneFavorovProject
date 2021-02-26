/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �����.521
�� ������:     ���� �� ᮮ�饭�� PB2
��� ࠡ�⠥�:   
��ࠬ����:      
���� ����᪠:  
������:         20.04.2017 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get netw}     /* ��ࠢ�� � bispc */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{getdates.i}

FUNCTION PoleTxt    RETURNS CHARACTER
   (INPUT  iTxt     AS CHARACTER,       /* PacketText.Contents */
    INPUT  iPole    AS CHARACTER ).     /* ��� ���� */

    DEFINE VARIABLE I1          AS INTEGER      NO-UNDO.

    I1 = INDEX(iTxt, iPole).
    If (I1 = 0) THEN RETURN "".
    I1 = I1 + LENGTH(iPole).
    RETURN SUBSTRING(iTxt, I1, INDEX(iTxt, "~n", I1) - I1).
END FUNCTION.

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNI     AS CHARACTER NO-UNDO.   /* ���⠭�� �� ��������� */
DEFINE VARIABLE cErr    AS CHARACTER NO-UNDO.
DEFINE BUFFER   bPObj   FOR PackObject.
DEFINE BUFFER   bPack   FOR Packet.

cFL = "./pb2-report.xml".
OUTPUT TO VALUE(cFL).
/*
REPEAT:
    {getfile.i &filename = cFL &mode = create}
    LEAVE.
END.
*/
/******************************************* ��������� */
PUT UNFORMATTED XLHead("PB2", "CCCCCC", "127,213,290,85,97,185").
cXL = XLCellHat("����� P�2 �� "
              + (IF (beg-date EQ end-date)
                 THEN STRING(beg-date, "99.99.9999")
                 ELSE ("��ਮ� � " + STRING(beg-date, "99.99.9999")
                     + " �� "      + STRING(end-date, "99.99.9999"))), 5).
PUT UNFORMATTED XLRowH(0, 34) cXL XLRowEnd().
cXL = XLCellHead("��� � �६� �ନ஢���� 䠩��",0,0,0)
    + XLCellHead("����� ���⭮�� ��� ",0,0,0)
    + XLCellHead("��� ��室�饣� 䠩��",0,0,0)
    + XLCellHead("����� ��室�饣� 䠩��",0,0,0)
    + XLCellHead("���⠭�� �� ��������� �� ��2",0,0,0)
    + XLCellHead("����������� �� �訡��� ��ࠡ�⪨/�ନ஢����",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH Packet
    WHERE (Packet.Class-Code    BEGINS 'PTAX')
      AND (Packet.Kind          BEGINS 'ETAX')
      AND (Packet.PackDate      GE beg-date)
      AND (Packet.PackDate      LE end-date)
      AND (Packet.ParentID      EQ 0)
      AND (Packet.filial-id     EQ shFilial)
    NO-LOCK,
FIRST Seance
    WHERE (Seance.SeanceID      EQ Packet.SeanceID)
      AND (Seance.op-kind       EQ "e-pb2crt")
    NO-LOCK,
EACH PackObject
    WHERE (PackObject.PacketID  EQ Packet.PacketID)
      AND (PackObject.file-name EQ 'op')
    NO-LOCK,
FIRST op
    WHERE (op.op                EQ INT64(PackObject.Surrogate))
    NO-LOCK,
FIRST op-entry OF op
    NO-LOCK:
    
    cNI  = "".
    cErr = "".
    FOR FIRST bPObj
        WHERE (bPObj.PacketID  EQ Packet.PacketID)
          AND (bPObj.file-name EQ "Packet")
        NO-LOCK,
    FIRST bPack
        WHERE (bPack.Class-Code     EQ "PTaxKwt")
          AND (bPack.PacketID       EQ INT64(bPObj.Surrogate))
        NO-LOCK,
    FIRST PacketText
        WHERE (PacketText.PacketID  EQ bPack.PacketID)
        NO-LOCK:

        cNI  = IF (INDEX(PacketText.Contents, "~n20@@@~n") = 0) THEN "�訡��" ELSE "20".
        cErr = PoleTxt(PacketText.Contents, "��������::").
    END.

    cXL = XLCell(STRING(Packet.PackDate, "99.99.9999") + " "
               + STRING(Packet.PackTime, "HH:MM:SS"))
        + XLCell(op-entry.acct-db)
        + XLCell(GetXAttrValue("Packet", STRING(Packet.PacketID), "FileName"))
        + XLCell(Packet.State)
        + XLCell(cNI)
        + XLCellWrap(cErr)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFL + ";class=bq").

{intrface.del}
