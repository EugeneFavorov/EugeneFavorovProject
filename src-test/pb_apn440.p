/**
Базируется:    zsv-rep440.p
Что делает:    Контрольная ведомость сообщений 440-П
Параметры:     
Место запуска: 
*/

DEFINE INPUT PARAM iParam        AS CHARACTER NO-UNDO.   /* варианты: apn, apo, ppd */

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

DEFINE TEMP-TABLE tt-zsv
   FIELD id             AS INT64
   FIELD dt-zsv         AS CHARACTER
   FIELD name-zsv       AS CHARACTER
   FIELD stat-zsv       AS CHARACTER
   FIELD dt-pb1         AS CHARACTER
   FIELD pb1            AS CHARACTER
   FIELD st-pb1         AS CHARACTER
   FIELD kwt-pb1        AS CHARACTER
   FIELD pb2            AS CHARACTER.

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
     AND FileExch.Name    BEGINS iParam
   NO-LOCK:

   CREATE tt-zsv.
   ASSIGN
      vNum              = vNum + 1
      tt-zsv.id         = Packet.PacketID
      tt-zsv.dt-zsv     = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss")
      tt-zsv.name-zsv   = FileExch.Name
      tt-zsv.stat-zsv   = Packet.State.
   RUN PacketPB1(Packet.PacketID, OUTPUT tt-zsv.dt-pb1, OUTPUT tt-zsv.pb1, OUTPUT tt-zsv.st-pb1, OUTPUT tt-zsv.kwt-pb1).
   RUN PacketPB2(Packet.PacketID, OUTPUT cT1, OUTPUT cT2, OUTPUT cT3, OUTPUT tt-zsv.pb2, OUTPUT cT4).

   IF vNum MOD 8 EQ 0
   THEN DO:
      mInt = mInt + 1.
      PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 'Отчет формируется' + mSpin[mInt MOD 8 + 1] COLOR bright.
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

PUT UNFORMATTED XLHead(CAPS(iParam), "CCCCCCCC", "127,259,78,127,109,50,97,365").

cXL = XLCellHat("Отчет по файлам " + CAPS(iParam) + " за период с " + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"),7).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Дата и время~nзагрузки файла",0,0,0)
    + XLCellHead("Имя входящего файла",0,0,0)
    + XLCellHead("Статус входящего файла",0,0,0)
    + XLCellHead("Дата отправки файла в налоговую",0,0,0)
    + XLCellHead("Уведомление о формировании РВ1",0,0,0)
    + XLCellHead("Статус РВ1",0,0,0)
    + XLCellHead("Квитанция из налоговой на РВ1",0,0,0)
    + XLCellHead("Уведомление об ошибках обработки/формирования",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BY tt-zsv.id:

   cXL = XLCell(tt-zsv.dt-zsv)
       + XLCell(tt-zsv.name-zsv)
       + XLCell(tt-zsv.stat-zsv)
       + XLCell(tt-zsv.dt-pb1)
       + XLCell(tt-zsv.pb1)
       + XLCell(tt-zsv.st-pb1)
       + XLCell(tt-zsv.kwt-pb1)
       + XLCellWrap(tt-zsv.pb2)
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
{intrface.del}          /* Выгрузка инструментария. */
