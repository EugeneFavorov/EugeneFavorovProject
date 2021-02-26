/**
���������:    zsv-rep440.p
�� ������:    ����஫쭠� ��������� ᮮ�饭�� 440-�
��ࠬ����:     
���� ����᪠: 
*/

DEFINE VARIABLE mInt             AS INT64     NO-UNDO.
DEFINE VARIABLE vNum             AS INT64     NO-UNDO.
DEFINE VARIABLE mSpin            AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mAcctBV          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTBV     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileRep         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL              AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-zsv
   FIELD packet-id      AS INT64
   FIELD name-zsv       AS CHARACTER
   FIELD dt-bv          AS CHARACTER
   FIELD name-bv        AS CHARACTER
   FIELD stat-bv        AS CHARACTER
   FIELD acct-bv        AS CHARACTER
   FIELD kwt-bv         AS CHARACTER.

DEF BUFFER xPacketBVS        FOR Packet.
DEF BUFFER xPacketBVSText    FOR PacketText.
DEF BUFFER xFileExchBVS      FOR FileExch.

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

/*BVS1_ZBV ��� BVD1_ZBV*/
FOR EACH xPacketBVS
   WHERE CAN-DO ("PTax440,PTaxPB", TRIM(xPacketBVS.Class-Code))
     AND xPacketBVS.PackDate     >= beg-date
     AND xPacketBVS.PackDate     <= end-date
     AND xPacketBVS.filial-id    = shFilial
   NO-LOCK,
FIRST xFileExchBVS
   WHERE xFileExchBVS.FileExchID = xPacketBVS.FileExchID
     AND CAN-DO("...._ZBV*", xFileExchBVS.Name)
   NO-LOCK:

   mAcctBV = "".
   FIND FIRST xPacketBVSText OF xPacketBVS
      NO-LOCK NO-ERROR.
   IF xPacketBVS.mail-format EQ "XFNSBVS440"
   THEN mAcctBV = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"�����","") ELSE "".
   ELSE mAcctBV = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"���������� �����","") ELSE "".

   RUN NIanswer(xPacketBVS.PacketID, OUTPUT mResultKWTBV).

   CREATE tt-zsv.
   ASSIGN
      vNum             = vNum + 1
      tt-zsv.name-zsv  = SUBSTRING(xFileExchBVS.Name, 6, 45)
      tt-zsv.packet-id = xPacketBVS.PacketID
      tt-zsv.dt-bv     = STRING(xPacketBVS.PackDate,"99.99.9999") + " " + STRING(xPacketBVS.PackTime,"hh:mm:ss")
      tt-zsv.name-bv   = xFileExchBVS.Name
      tt-zsv.stat-bv   = xPacketBVS.State
      tt-zsv.acct-bv   = mAcctBV
      tt-zsv.kwt-bv    = mResultKWTBV.

    IF vNum MOD 8 EQ 0
    THEN DO:
       mInt = mInt + 1.
       PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 '���� �ନ�����' + mSpin[mInt MOD 8 + 1] COLOR bright.
    END.
END.

/* ************************************************************************************************ */
mFileRep = "zbv-"
         + STRING( YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(  DAY(TODAY),"99") + "-"
         + TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileRep = REPLACE(mFileRep,":","").
OUTPUT TO VALUE(mFileRep).

PUT UNFORMATTED XLHead("ZBV", "CCCCC", "127,505,51,182,97").

cXL = XLCellHat("���� �� 䠩��� ZBV �� ��ਮ� � " + STRING(beg-date, "99.99.9999") + " �� " + STRING(end-date, "99.99.9999"),4).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("��� ��ࠢ�� 䠩�� � ���������",0,0,0)
    + XLCellHead("��� ��室�饣� 䠩�� BVS1_ZBV, BVD1_ZBV",0,0,0)
    + XLCellHead("����� 䠩�� �⢥�",0,0,0)
    + XLCellHead("����� ��� � 䠩�� BVD",0,0,0)
    + XLCellHead("���⠭�� �� ��������� �� BVS � BVD",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BY tt-zsv.packet-id:

   cXL = XLCell(tt-zsv.dt-bv)
       + XLCell(tt-zsv.name-bv)
       + XLCell(tt-zsv.stat-bv)
       + XLCellWrap(tt-zsv.acct-bv)
       + XLCell(tt-zsv.kwt-bv)
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
{intrface.del}          /* ���㧪� �����㬥����. */
