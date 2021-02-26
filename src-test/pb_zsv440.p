/**
Базируется:    zsv-rep440.p
Что делает:    Контрольная ведомость сообщений 440-П
Параметры:     
Место запуска: 
*/

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

DEFINE TEMP-TABLE tt-zsv
   FIELD id             AS INT64
   FIELD packet-id      AS INT64
   FIELD dt-zsv         AS CHARACTER
   FIELD name-zsv       AS CHARACTER
   FIELD stat-zsv       AS CHARACTER
   FIELD dt-bv          AS CHARACTER
   FIELD name-bv        AS CHARACTER
   FIELD stat-bv        AS CHARACTER
   FIELD acct-bv        AS CHARACTER
   FIELD pb1            AS CHARACTER
   FIELD st-pb1         AS CHARACTER
   FIELD kwt-pb1        AS CHARACTER
   FIELD kwt-bv         AS CHARACTER
   FIELD pb2            AS CHARACTER.

DEF BUFFER xPacket           FOR Packet.
DEF BUFFER xPacketBVS        FOR Packet.
DEF BUFFER xPacketBVSText    FOR PacketText.
DEF BUFFER xFileExchBVS      FOR FileExch.

{globals.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
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
     AND FileExch.Name    BEGINS "ZSV"
   NO-LOCK
   BY Packet.PackDate:

   CREATE tt-zsv.
   ASSIGN
      vNum              = vNum + 1
      tt-zsv.id         = Packet.PacketID
      tt-zsv.packet-id  = Packet.PacketID
      tt-zsv.dt-zsv     = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss")
      tt-zsv.name-zsv   = FileExch.Name
      tt-zsv.stat-zsv   = Packet.State.
   RUN PacketPB1(Packet.PacketID, OUTPUT cXL, OUTPUT tt-zsv.pb1, OUTPUT tt-zsv.st-pb1, OUTPUT tt-zsv.kwt-pb1).
   RUN PacketPB2(Packet.PacketID, OUTPUT tt-zsv.dt-bv, OUTPUT tt-zsv.name-bv, OUTPUT tt-zsv.stat-bv, OUTPUT tt-zsv.pb2, OUTPUT cT1).

   /*BVS or BVD*/
   FOR EACH PackObject
      WHERE PackObject.PacketID  = Packet.PacketID
        AND PackObject.file-name = "Packet"
      NO-LOCK,
   FIRST xPacket
      WHERE xPacket.PacketID     = INT64(PackObject.Surrogate)
        AND xPacket.class-code   = "PTax440"
        AND CAN-DO("XFNSBVS440,XFNSBVD440", xPacket.mail-format)
      NO-LOCK,
   FIRST xPacketBVS
      WHERE xPacketBVS.PacketID  = INT64(PackObject.Surrogate)
      NO-LOCK:

      mDateTimeBV = STRING(xPacketBVS.PackDate,"99.99.9999") + " " + STRING(xPacketBVS.PackTime,"hh:mm:ss").
      mStateBV    = xPacketBVS.State.

      mAcctBV = "".
      FIND FIRST xPacketBVSText OF xPacketBVS
         NO-LOCK NO-ERROR.
      IF xPacket.mail-format EQ "XFNSBVS440"
      THEN mAcctBV = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"НомСч","") ELSE "".
      ELSE mAcctBV = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"ВЫПБНДОПОЛ НомСч","") ELSE "".

      FIND FIRST xFileExchBVS
         WHERE xFileExchBVS.FileExchID = xPacketBVS.FileExchID
         NO-LOCK NO-ERROR.
      IF AVAIL(xFileExchBVS)
      THEN mFileNameBV = xFileExchBVS.Name.
      ELSE mFileNameBV = GetXAttrValue("Packet",STRING(xPacketBVS.PacketID),"FileName").

      RUN NIanswer(xPacketBVS.PacketID, OUTPUT mResultKWTBV).

      FIND FIRST tt-zsv
         WHERE tt-zsv.name-bv   = mFileNameBV
           AND tt-zsv.packet-id = xPacketBVS.PacketID
         NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-zsv)
      THEN DO:
         CREATE tt-zsv.
         ASSIGN
            tt-zsv.id        = Packet.PacketID
            tt-zsv.packet-id = xPacketBVS.PacketID
            tt-zsv.dt-bv     = mDateTimeBV
            tt-zsv.name-bv   = mFileNameBV
            tt-zsv.stat-bv   = mStateBV
            tt-zsv.acct-bv   = mAcctBV
            tt-zsv.kwt-bv    = mResultKWTBV.
      END.
   END.

   IF vNum MOD 8 EQ 0
   THEN DO:
      mInt = mInt + 1.
      PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 'Отчет формируется' + mSpin[mInt MOD 8 + 1] COLOR bright.
   END.
END.

/* ************************************************************************************************ */
mFileRep = "zsv-"
         + STRING( YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(  DAY(TODAY),"99") + "-"
         + TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileRep = REPLACE(mFileRep,":","").
OUTPUT TO VALUE(mFileRep).

PUT UNFORMATTED XLHead("ZSV", "CCCCCCCCCCCC", "127,259,78,127,505,51,182,109,50,97,97,365").

cXL = XLCellHat("Отчет по файлам ZSV за период с " + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"),11).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Дата и время~nзагрузки файла",0,0,0)
    + XLCellHead("Имя входящего файла",0,0,0)
    + XLCellHead("Статус входящего файла",0,0,0)
    + XLCellHead("Дата отправки файла в налоговую",0,0,0)
    + XLCellHead("Имя исходящего файла BVS, BVD, PB2",0,0,0)
    + XLCellHead("Статус файла ответа",0,0,0)
    + XLCellHead("Номер счета в файле BVD",0,0,0)
    + XLCellHead("Уведомление о формировании РВ1",0,0,0)
    + XLCellHead("Статус РВ1",0,0,0)
    + XLCellHead("Квитанция из налоговой на РВ1",0,0,0)
    + XLCellHead("Квитанция из налоговой на BVS и BVD",0,0,0)
    + XLCellHead("Уведомление об ошибках обработки/формирования",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BREAK BY tt-zsv.id
         BY tt-zsv.packet-id:

   IF FIRST-OF(tt-zsv.id)
   THEN DO:
      cXL = XLCell(tt-zsv.dt-zsv)
          + XLCell(tt-zsv.name-zsv)
          + XLCell(tt-zsv.stat-zsv)
          + XLCell(tt-zsv.dt-bv)
          + XLCell(tt-zsv.name-bv)
          + XLCell(tt-zsv.stat-bv)
          + XLEmptyCell()
          + XLCell(tt-zsv.pb1)
          + XLCell(tt-zsv.st-pb1)
          + XLCell(tt-zsv.kwt-pb1)
          + XLEmptyCell()
          + XLCellWrap(tt-zsv.pb2)
          .
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
   ELSE DO:
      cXL = XLEmptyCells(3)
          + XLCell(tt-zsv.dt-bv)
          + XLCell(tt-zsv.name-bv)
          + XLCell(tt-zsv.stat-bv)
          + XLCellWrap(tt-zsv.acct-bv)
          + XLEmptyCells(3)
          + XLCell(tt-zsv.kwt-bv)
          + XLEmptyCell()
          .
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
{intrface.del}          /* Выгрузка инструментария. */
