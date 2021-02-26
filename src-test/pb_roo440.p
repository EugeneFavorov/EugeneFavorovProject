/**
���������:    zsv-rep440.p
�� ������:    ����஫쭠� ��������� ᮮ�饭�� 440-�
��ࠬ����:     
���� ����᪠: 
*/

DEFINE VARIABLE iParam           AS CHARACTER NO-UNDO INIT "roo".
DEFINE VARIABLE mDateTimeBV      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStateBV         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileNameBV      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt             AS INT64     NO-UNDO.
DEFINE VARIABLE vNum             AS INT64     NO-UNDO.
DEFINE VARIABLE mSpin            AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mAcctBV          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTBV     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileRep         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT3              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT4              AS CHARACTER NO-UNDO.
DEFINE VARIABLE dT1              AS DATE      NO-UNDO.
DEFINE VARIABLE I                AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE tt-zsv
   FIELD id             AS INT64
   FIELD dt-zsv         AS CHARACTER
   FIELD name-zsv       AS CHARACTER
   FIELD stat-zsv       AS CHARACTER
   FIELD acct-zsv       AS CHARACTER INIT " "
   FIELD acct-gr        AS CHARACTER
   FIELD pb1            AS CHARACTER
   FIELD st-pb1         AS CHARACTER
   FIELD kwt-pb1        AS CHARACTER
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
   WHERE CAN-DO ("PTax440", TRIM(Packet.Class-Code))
     AND Packet.PackDate      GE beg-date
     AND Packet.PackDate      LE end-date
     AND Packet.filial-id     EQ shFilial
   NO-LOCK,
FIRST FileExch
   WHERE FileExch.FileExchID  EQ Packet.FileExchID
     AND FileExch.Name    BEGINS iParam
   NO-LOCK,
FIRST PacketText OF Packet
   NO-LOCK:

   CREATE tt-zsv.
   ASSIGN
      vNum              = vNum + 1
      tt-zsv.id         = Packet.PacketID
      tt-zsv.dt-zsv     = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss")
      tt-zsv.name-zsv   = FileExch.Name
      tt-zsv.stat-zsv   = Packet.State
      tt-zsv.acct-zsv   = GetFileAllVar440(STRING(PacketText.Contents),"�����"," ").
   RUN PacketPB1(Packet.PacketID, OUTPUT dT1, OUTPUT tt-zsv.pb1, OUTPUT tt-zsv.st-pb1, OUTPUT tt-zsv.kwt-pb1).
   RUN PacketPB2(Packet.PacketID, OUTPUT cT1, OUTPUT cT2, OUTPUT cT3, OUTPUT tt-zsv.pb2, OUTPUT cT4).

   DO I = 1 TO NUM-ENTRIES(tt-zsv.acct-zsv, "~n"):
      FIND FIRST acct
         WHERE (acct.acct    BEGINS ENTRY(I, tt-zsv.acct-zsv, "~n"))
           AND (acct.filial-id   <> "0400")
         NO-LOCK NO-ERROR.
      tt-zsv.acct-gr  = tt-zsv.acct-gr  + (IF (tt-zsv.acct-gr  = "") THEN "" ELSE "~n")
                      + IF (AVAIL acct) THEN GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "groupOABS", ".")
                                        ELSE ".".
   END.

   IF vNum MOD 8 EQ 0
   THEN DO:
      mInt = mInt + 1.
      PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 '���� �ନ�����' + mSpin[mInt MOD 8 + 1] COLOR bright.
   END.
END.

/* ************************************************************************************************ */
mFileRep = iParam + "-"
         + STRING( YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(  DAY(TODAY),"99") + "-"
         + TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileRep = REPLACE(mFileRep,":","").
OUTPUT TO VALUE(mFileRep).

RUN XLAddStyle('<Style ss:ID="s80yel"><Interior ss:Color="#FFFF00" ss:Pattern="Solid"/></Style>').
RUN XLAddStyle('<Style ss:ID="s81red"><Font ss:Color="#FF0000"/></Style>').
PUT UNFORMATTED XLHead(CAPS(iParam), "CCCCCCCCC", "127,259,78,150,53,109,50,97,365").

cXL = XLCellHat("���� �� 䠩��� " + CAPS(iParam) + " �� ��ਮ� � " + STRING(beg-date, "99.99.9999") + " �� " + STRING(end-date, "99.99.9999"),8).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("��� � �६�~n����㧪� 䠩��",0,0,0)
    + XLCellHead("��� �室�饣� 䠩��",0,0,0)
    + XLCellHead("����� �室�饣� 䠩��",0,0,0)
    + XLCellHead("����� ��⮢ � 䠩�� ROO",0,0,0)
    + XLCellHead("��㯯�",0,0,0)
    + XLCellHead("����������� � �ନ஢���� ��1",0,0,0)
    + XLCellHead("����� ��1",0,0,0)
    + XLCellHead("���⠭�� �� ��������� �� ��1",0,0,0)
    + XLCellHead("����������� �� �訡��� ��ࠡ�⪨/�ନ஢����",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BY tt-zsv.id:

   DO I = 1 TO NUM-ENTRIES(tt-zsv.acct-zsv, "~n"):
      IF (I = 1)
      THEN DO:
         cXL = XLCell(tt-zsv.dt-zsv)
             + XLCell(tt-zsv.name-zsv)
             + (IF (tt-zsv.stat-zsv = "����")         THEN
               XLCellStyle("s81red", tt-zsv.stat-zsv) ELSE
               XLCell(tt-zsv.stat-zsv))
             + (IF CAN-DO("596,599", ENTRY(I, tt-zsv.acct-gr, "~n"))  THEN
              (XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-zsv, "~n"))
             + XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-gr, "~n"))) ELSE
              (XLCell(ENTRY(I, tt-zsv.acct-zsv, "~n"))
             + XLCell(REPLACE(ENTRY(I, tt-zsv.acct-gr, "~n"),".",""))))
             + XLCell(tt-zsv.pb1)
             + XLCell(tt-zsv.st-pb1)
             + XLCell(tt-zsv.kwt-pb1)
             + XLCellWrap(tt-zsv.pb2)
             .
      END.
      ELSE DO:
        cXL = XLEmptyCells(3)
             + (IF CAN-DO("596,599", ENTRY(I, tt-zsv.acct-gr, "~n"))  THEN
              (XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-zsv, "~n"))
             + XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-gr, "~n"))) ELSE
              (XLCell(ENTRY(I, tt-zsv.acct-zsv, "~n"))
             + XLCell(REPLACE(ENTRY(I, tt-zsv.acct-gr, "~n"),".",""))))
             .
      END.
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
{intrface.del}          /* ���㧪� �����㬥����. */
