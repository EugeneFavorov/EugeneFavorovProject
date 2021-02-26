/**
Базируется:    zsv-rep440.p
Что делает:    Контрольная ведомость сообщений 440-П
Параметры:     
Место запуска: 
*/

DEFINE INPUT PARAM iParam        AS CHARACTER NO-UNDO.   /* варианты: rpo, zso */

DEFINE VARIABLE mInt             AS INT64     NO-UNDO.
DEFINE VARIABLE vNum             AS INT64     NO-UNDO.
DEFINE VARIABLE mSpin            AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mFileRep         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cT3              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLocalTime       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I                AS INTEGER   NO-UNDO.
DEFINE BUFFER PObos     FOR PackObject.
DEFINE BUFFER FileBOS   FOR FileExch.
DEFINE BUFFER PackBOS   FOR Packet.
DEFINE BUFFER BOSText   FOR PacketText.
DEFINE BUFFER POKWT     FOR PackObject.
DEFINE BUFFER PackKWT   FOR Packet.
DEFINE BUFFER KWTText   FOR PacketText.

DEFINE TEMP-TABLE tt-zsv    NO-UNDO
   FIELD id             AS INT64
   FIELD dt-zsv         AS CHARACTER
   FIELD name-zsv       AS CHARACTER
   FIELD stat-zsv       AS CHARACTER
   FIELD dt-bos         AS CHARACTER
   FIELD name-bos       AS CHARACTER
   FIELD stat-bos       AS CHARACTER
   FIELD acct-bos       AS CHARACTER INIT " "
   FIELD acct-gr        AS CHARACTER
   FIELD acct-pos       AS CHARACTER
   FIELD bos-pos        AS CHARACTER
   FIELD pb1            AS CHARACTER
   FIELD st-pb1         AS CHARACTER
   FIELD kwt-pb1        AS CHARACTER
   FIELD pb2            AS CHARACTER
   FIELD kwt-bos        AS CHARACTER.

{globals.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{pb_pack440.pro}
{sh-defs.i}
{440p-rep.fun}

{getdates.i}

mSpin[1] = ".   ".
mSpin[2] = "..  ".
mSpin[3] = "... ".
mSpin[4] = "....".
mSpin[5] = " ...".
mSpin[6] = "  ..".
mSpin[7] = "   .".
mSpin[8] = ".   ".
vNum     = 0.
mInt     = 0.

CASE shFilial:
    WHEN "0000" THEN mLocalTime =     0.
    WHEN "0300" THEN mLocalTime =  7200.
    WHEN "0500" THEN mLocalTime = 10800.
END CASE.

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
      tt-zsv.stat-zsv   = Packet.State
      tt-zsv.acct-bos   = " ".
   RUN PacketPB1(Packet.PacketID, OUTPUT cT1, OUTPUT tt-zsv.pb1, OUTPUT tt-zsv.st-pb1, OUTPUT tt-zsv.kwt-pb1).
   RUN PacketPB2(Packet.PacketID, OUTPUT tt-zsv.dt-bos, OUTPUT tt-zsv.name-bos, OUTPUT tt-zsv.stat-bos, OUTPUT tt-zsv.pb2, OUTPUT tt-zsv.kwt-bos).
/*
   FIND FIRST PacketText OF Packet
      NO-LOCK NO-ERROR.
   IF (AVAIL PacketText)
   THEN DO:
      tt-zsv.acct-bos   = GetFileAllVar440(PacketText.Contents,"НомСч"," ").
   END.
*/
   FOR EACH PObos
      WHERE PObos.PacketID       = Packet.PacketID
        AND PObos.file-name      = "Packet"
/*      AND PObos.Kind           = "RTaxBOSAcct"
*/    NO-LOCK,
   FIRST PackBOS
      WHERE (PackBOS.PacketID    = INT64(PObos.Surrogate))
      NO-LOCK,
   FIRST FileBOS
      WHERE (FileBOS.FileExchID  = PackBOS.FileExchID)
        AND (FileBOS.Name   BEGINS "BOS")
      NO-LOCK,
   FIRST BOSText OF PackBOS
      NO-LOCK:

      ASSIGN
         tt-zsv.dt-bos     = STRING(PackBOS.PackDate,"99.99.9999") + " " + STRING(PackBOS.PackTime,"hh:mm:ss")
         tt-zsv.name-bos   = FileBOS.Name
         tt-zsv.stat-bos   = PackBOS.State
         tt-zsv.acct-bos   = GetFileAllVar440(STRING(BOSText.Contents),"НомСч"," ")
         tt-zsv.bos-pos    = GetFileAllVar440(STRING(BOSText.Contents),"Остаток","")
         tt-zsv.acct-gr    = ""
         tt-zsv.acct-pos   = "".

      FOR EACH POKWT
         WHERE POKWT.PacketID    = PackBOS.PacketID
           AND POKWT.file-name   = "Packet"
           AND POKWT.Kind        = "TTaxKWT"
         NO-LOCK,
      FIRST PackKWT
         WHERE PackKWT.PacketID  = INT64(POKWT.Surrogate)
         NO-LOCK,
      FIRST KWTText OF PackKWT
         NO-LOCK:

         tt-zsv.kwt-bos = GetFileVar440(STRING(KWTText.Contents),"КодРезПроверки","").
         LEAVE.
      END.

      LEAVE.
   END.

   IF (tt-zsv.acct-bos <> " ") THEN
   DO I = 1 TO NUM-ENTRIES(tt-zsv.acct-bos, "~n"):
      FIND FIRST acct
         WHERE (acct.acct    BEGINS ENTRY(I, tt-zsv.acct-bos, "~n"))
           AND (acct.filial-id   <> "0400")
         NO-LOCK NO-ERROR.
      tt-zsv.acct-gr  = tt-zsv.acct-gr  + (IF (tt-zsv.acct-gr  = "") THEN "" ELSE "~n")
                      + IF (AVAIL acct) THEN GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "groupOABS", ".")
                                        ELSE ".".
      IF (tt-zsv.bos-pos <> "") THEN      /* Если есть отаток BOS, то вычисляем реальный */
      tt-zsv.acct-pos = tt-zsv.acct-pos + (IF (tt-zsv.acct-pos = "") THEN "" ELSE "~n")
                      + IF (AVAIL acct) THEN STRING(- PosOnTime(acct.acct, acct.currency, DATETIME(Packet.PackDate, (Packet.PackTime - mLocalTime) * 1000)))
                                        ELSE "0".
   END.

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

RUN XLAddStyle('<Style ss:ID="s80yel"><Interior ss:Color="#FFFF00" ss:Pattern="Solid"/></Style>').
RUN XLAddStyle('<Style ss:ID="s81red"><Font ss:Color="#FF0000"/></Style>').
PUT UNFORMATTED XLHead(CAPS(iParam), "CCCCCCCCNNCCCCC", "127,262,78,127,407,51,150,53,122,128,109,50,97,97,365").

cXL = XLCellHat("Отчет по файлам " + CAPS(iParam) + " за период с " + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"),14).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Дата и время~nзагрузки файла",0,0,0)
    + XLCellHead("Имя входящего файла",0,0,0)
    + XLCellHead("Статус входящего файла",0,0,0)
    + XLCellHead("Дата отправки файла в налоговую",0,0,0)
    + XLCellHead("Имя файла BOS/PB2",0,0,0)
    + XLCellHead("Статус файла ответа",0,0,0)
    + XLCellHead("Номера счетов в файле BOS",0,0,0)
    + XLCellHead("Группа",0,0,0)
    + XLCellHead("Остаток на счете в момент формирования файла BOS",0,0,0)
    + XLCellHead("Остаток сформированный в файле BOS",0,0,0)
    + XLCellHead("Уведомление о формировании РВ1",0,0,0)
    + XLCellHead("Статус РВ1",0,0,0)
    + XLCellHead("Квитанция из налоговой на РВ1",0,0,0)
    + XLCellHead("Квитанция из налоговой на BOS/PB2",0,0,0)
    + XLCellHead("Уведомление об ошибках обработки/формирования",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-zsv
   NO-LOCK
   BY tt-zsv.id:

   DO I = 1 TO NUM-ENTRIES(tt-zsv.acct-bos, "~n"):
      IF (I = 1)
      THEN DO:
         cXL = XLCell(tt-zsv.dt-zsv)
             + XLCell(tt-zsv.name-zsv)
             + (IF (tt-zsv.stat-zsv = "ОШБК")         THEN
               XLCellStyle("s81red", tt-zsv.stat-zsv) ELSE
               XLCell(tt-zsv.stat-zsv))
             + XLCell(tt-zsv.dt-bos)
             + XLCell(tt-zsv.name-bos)
             + (IF (tt-zsv.stat-bos = "ОШБК")         THEN
               XLCellStyle("s81red", tt-zsv.stat-bos) ELSE
               XLCell(tt-zsv.stat-bos))
             + (IF CAN-DO("596,599", ENTRY(I, tt-zsv.acct-gr, "~n"))  THEN
              (XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-bos, "~n"))
             + XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-gr, "~n"))) ELSE
              (XLCell(ENTRY(I, tt-zsv.acct-bos, "~n"))
             + XLCell(REPLACE(ENTRY(I, tt-zsv.acct-gr, "~n"),".",""))))

             + (IF (tt-zsv.acct-pos = " ") THEN XLEmptyCells(2) ELSE
               (IF (DEC(ENTRY(I, tt-zsv.acct-pos, "~n")) <> DEC(ENTRY(I, tt-zsv.bos-pos, "~n"))) THEN
              (XLNumCellStyle("s81red", DEC(ENTRY(I, tt-zsv.acct-pos, "~n")))
             + XLNumCellStyle("s81red", DEC(ENTRY(I, tt-zsv.bos-pos, "~n")))) ELSE
              (XLNumCell(DEC(ENTRY(I, tt-zsv.acct-pos, "~n")))
             + XLNumCell(DEC(ENTRY(I, tt-zsv.bos-pos, "~n"))))))

             + XLCell(tt-zsv.pb1)
             + XLCell(tt-zsv.st-pb1)
             + XLCell(tt-zsv.kwt-pb1)
             + XLCell(tt-zsv.kwt-bos)
             + XLCellWrap(tt-zsv.pb2)
             .
      END.
      ELSE DO:
        cXL = XLEmptyCells(6)
             + (IF CAN-DO("596,599", ENTRY(I, tt-zsv.acct-gr, "~n"))  THEN
              (XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-bos, "~n"))
             + XLCellStyle("s80yel", ENTRY(I, tt-zsv.acct-gr, "~n"))) ELSE
              (XLCell(ENTRY(I, tt-zsv.acct-bos, "~n"))
             + XLCell(REPLACE(ENTRY(I, tt-zsv.acct-gr, "~n"),".",""))))
             + (IF (DEC(ENTRY(I, tt-zsv.acct-pos, "~n")) <> DEC(ENTRY(I, tt-zsv.bos-pos, "~n"))) THEN
              (XLNumCellStyle("s81red", DEC(ENTRY(I, tt-zsv.acct-pos, "~n")))
             + XLNumCellStyle("s81red", DEC(ENTRY(I, tt-zsv.bos-pos, "~n")))) ELSE
              (XLNumCell(DEC(ENTRY(I, tt-zsv.acct-pos, "~n")))
             + XLNumCell(DEC(ENTRY(I, tt-zsv.bos-pos, "~n")))))
             .
      END.
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").
