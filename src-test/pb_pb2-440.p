/**
���������:    zsv-rep440.p
�� ������:    ����஫쭠� ��������� ᮮ�饭�� 440-�
��ࠬ����:     
���� ����᪠: 
*/

DEFINE VARIABLE mInt             AS INT64     NO-UNDO.
DEFINE VARIABLE vNum             AS INT64     NO-UNDO.
DEFINE VARIABLE mSpin            AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mFileRep         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL              AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-zsv
   FIELD id             AS INT64
   FIELD dt-pb2         AS CHARACTER
   FIELD name-zsv       AS CHARACTER
   FIELD acct-pb2       AS CHARACTER
   FIELD name-pb2       AS CHARACTER
   FIELD stat-pb2       AS CHARACTER
   FIELD kwt-pb2        AS CHARACTER
   FIELD pb2            AS CHARACTER.

{globals.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{pb_pack440.pro}

{getdates.i}

mSpin[1] = ".   ".
mSpin[2] = "..  ".
mSpin[3] = "... ".
mSpin[4] = "....".
mSpin[5] = " ...".
mSpin[6] = "  ..".
mSpin[7] = "   .".
mSpin[8] = ".   ".

vNum = 0.
mInt = 0.

FOR EACH Packet
   WHERE CAN-DO ("PTaxPB", TRIM(Packet.Class-Code))
     AND Packet.PackDate      GE beg-date
     AND Packet.PackDate      LE end-date
     AND Packet.filial-id     EQ shFilial
   NO-LOCK,
FIRST FileExch
   WHERE FileExch.FileExchID  EQ Packet.FileExchID
     AND FileExch.Name    BEGINS "PB2"
   NO-LOCK,
FIRST PacketText OF Packet
   NO-LOCK
   BY Packet.PackDate:

   CREATE tt-zsv.
   ASSIGN
      vNum              = vNum + 1
      tt-zsv.id         = Packet.PacketID
      tt-zsv.dt-pb2     = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss")
      tt-zsv.name-zsv   = GetFileVar440(STRING(PacketText.Contents),"�����","") + ".xml"
      tt-zsv.acct-pb2   = IF (GetFileVar440(STRING(PacketText.Contents),"�������","") BEGINS "�����")
                          THEN GetFileAllVar440(STRING(PacketText.Contents),"���琥��","")
                          ELSE ""
      tt-zsv.name-pb2   = FileExch.Name
      tt-zsv.stat-pb2   = Packet.State
      tt-zsv.pb2        = GetFileAllVar440(STRING(PacketText.Contents),"���᭥���","").
   RUN NIanswer(Packet.PacketID, OUTPUT tt-zsv.kwt-pb2).

   IF (tt-zsv.acct-pb2 = "")
   THEN ASSIGN
         cXL             = ENTRY(2, tt-zsv.pb2, " ")
         tt-zsv.acct-pb2 = IF (LENGTH(cXL) = 20) THEN cXL ELSE "".

   IF vNum MOD 8 EQ 0
   THEN DO:
      mInt = mInt + 1.
      PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 '���� �ନ�����' + mSpin[mInt MOD 8 + 1] COLOR bright.
   END.
END.

/* ************************************************************************************************ */
mFileRep = "pb2-"
         + STRING( YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(  DAY(TODAY),"99") + "-"
         + TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileRep = REPLACE(mFileRep,":","").
OUTPUT TO VALUE(mFileRep) /* UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866" */ .

PUT UNFORMATTED XLHead("PB2", "CCCCCCCCCC", "127,263,150,333,85,97,365").

cXL = XLCellHat("���� �� 䠩��� PB2 �� ��ਮ� � " + STRING(beg-date, "99.99.9999") + " �� " + STRING(end-date, "99.99.9999"),10).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("��� � �६�~n�ନ஢���� 䠩��",0,0,0)
    + XLCellHead("��� �室�饣� 䠩��",0,0,0)
    + XLCellHead("����� ������ ��⮢",0,0,0)
    + XLCellHead("��� ��室�饣� 䠩��",0,0,0)
    + XLCellHead("����� ��室�饣� 䠩��",0,0,0)
    + XLCellHead("���⠭�� �� ��������� �� PB2",0,0,0)
    + XLCellHead("����������� �� �訡��� ��ࠡ�⪨/�ନ஢����",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BREAK BY tt-zsv.id:

   cXL = XLCell(tt-zsv.dt-pb2)
       + XLCell(tt-zsv.name-zsv)
       + XLCellWrap(tt-zsv.acct-pb2)
       + XLCell(tt-zsv.name-pb2)
       + XLCell(tt-zsv.stat-pb2)
       + XLCell(tt-zsv.kwt-pb2)
       + XLCellWrap(tt-zsv.pb2)
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
{intrface.del}          /* ���㧪� �����㬥����. */
