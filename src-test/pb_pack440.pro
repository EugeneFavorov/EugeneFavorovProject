/**
Базируется:    
Что делает:    
*/

FUNCTION GetFileVar440 RETURNS char
   (INPUT iContent  AS CHARACTER,
    INPUT iTag      AS CHARACTER,
    INPUT iDefault  AS CHARACTER ).

    DEFINE VARIABLE mInt1 AS INT64 NO-UNDO.
    DEFINE VARIABLE mInt2 AS INT64 NO-UNDO.
    DEFINE VARIABLE mInt3 AS INT64 NO-UNDO.
    DEFINE VARIABLE oRet  AS CHAR  NO-UNDO.

    iTag  = CODEPAGE-CONVERT(iTag,"1251","ibm866").
    mInt1 = INDEX(iContent,iTag).
    IF mInt1 GT 0
    THEN DO:
        mInt2 = INDEX(iContent,'"',mInt1).
        mInt3 = INDEX(iContent,'"',mInt2 + 1).
        oRet  = IF (mInt3 - mInt2 < 0) THEN "" ELSE SUBSTRING(iContent,mInt2 + 1,(mInt3 - mInt2 - 1)).
        oRet  = REPLACE(oRet, "&amp;",  '&').
        oRet  = REPLACE(oRet, "&quot;", '"').
        oRet  = CODEPAGE-CONVERT(oRet,"ibm866","1251").
        RETURN oRet.
    END.
    ELSE RETURN iDefault.
END FUNCTION.

FUNCTION GetFileAllVar440 RETURNS char
  (INPUT iContent AS CHARACTER,
   INPUT iTag     AS CHARACTER,
   INPUT iDefault AS CHARACTER).

   DEFINE VARIABLE mInt1 AS INT64 NO-UNDO.
   DEFINE VARIABLE mInt2 AS INT64 NO-UNDO.
   DEFINE VARIABLE mInt3 AS INT64 NO-UNDO.
   DEFINE VARIABLE mInt4 AS INT64 NO-UNDO.
   DEFINE VARIABLE oRet1 AS CHAR  NO-UNDO.
   DEFINE VARIABLE oRet2 AS CHAR  NO-UNDO.

   iTag = CODEPAGE-CONVERT(iTag,"1251","ibm866").

   DO WHILE TRUE:
      mInt1 = INDEX(iContent,iTag,mInt4 + 1).
      IF mInt1 GT 0 THEN
      DO:
         mInt2 = INDEX(iContent,'"',mInt1).
         mInt3 = INDEX(iContent,'"',mInt2 + 1).
         oRet1 = IF (mInt3 - mInt2 < 0) THEN "" ELSE SUBSTRING(iContent,mInt2 + 1,(mInt3 - mInt2 - 1)).
         oRet1 = CODEPAGE-CONVERT(oRet1,"ibm866","1251").
         oRet1 = REPLACE(oRet1, "&amp;",  '&').
         oRet1 = REPLACE(oRet1, "&quot;", '"').
         oRet2 = IF oRet2 EQ "" THEN oRet1 ELSE oRet2 + "~n" + oRet1.
         mInt4 = mInt1.
      END.
      ELSE LEAVE.
   END.
   IF oRet2 NE "" THEN RETURN oRet2.
   ELSE RETURN iDefault.
END FUNCTION.

PROCEDURE PacketPB1:
   DEFINE INPUT  PARAMETER PackID   AS INT64     NO-UNDO. /* Исходное сообщение */
   DEFINE OUTPUT PARAMETER oDatPb1  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oRezPb1  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oStPb1   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oRezKwt  AS CHARACTER NO-UNDO.

   DEF BUFFER xPackObject       FOR PackObject.
   DEF BUFFER xPacket           FOR Packet.
   DEF BUFFER xPacketPB1        FOR Packet.
   DEF BUFFER xPacketPB1Text    FOR PacketText.
   DEF BUFFER xFileExch         FOR FileExch.
   DEF BUFFER xFileExchPB1      FOR FileExch.
   DEF BUFFER xPackObjectPB1    FOR PackObject.
   DEF BUFFER xPacketKWT        FOR Packet.
   DEF BUFFER xPacketKWTText    FOR PacketText.

   oDatPb1 = "".
   FOR EACH xPackObject
      WHERE xPackObject.PacketID    = PackID
        AND xPackObject.file-name   = "Packet"
        AND xPackObject.Kind        = "TTAXConf"
      NO-LOCK,
   FIRST xPacketPB1
      WHERE xPacketPB1.PacketID     = INT64(xPackObject.Surrogate)
      NO-LOCK,
   FIRST xFileExchPB1
      WHERE xFileExchPB1.FileExchID = xPacketPB1.FileExchID
        AND xFileExchPB1.Name  BEGINS "PB1"
      NO-LOCK,
   FIRST xPacketPB1Text OF xPacketPB1
      NO-LOCK:

      oDatPb1 = STRING(xPacketPB1.PackDate,"99.99.9999") + " " + STRING(xPacketPB1.PackTime,"hh:mm:ss").
      oRezPb1 = GetFileVar440(STRING(xPacketPB1Text.Contents),   "КодРезПроверки","").
      oStPb1  = xPacketPB1.State.

      FOR EACH xPackObjectPB1
         WHERE xPackObjectPB1.PacketID  = xPacketPB1.PacketID
           AND xPackObjectPB1.file-name = "Packet"
           AND xPackObjectPB1.Kind      = "TTaxKWT"
         NO-LOCK,
      FIRST xPacketKWT
         WHERE xPacketKWT.PacketID      = INT64(xPackObjectPB1.Surrogate)
         NO-LOCK,
      FIRST xPacketKWTText OF xPacketKWT
         NO-LOCK:

         oRezKwt = GetFileVar440(STRING(xPacketKWTText.Contents),"КодРезПроверки","").
         LEAVE.
      END.
   END.

   /* Если отсутствует объект, ищем файл с заданным именем */
   IF (oDatPb1 = "")
   THEN DO:
      FOR FIRST xPacket
         WHERE xPacket.PacketID        = PackID
         NO-LOCK,
      FIRST xFileExch
         WHERE xFileExch.FileExchID    = xPacket.FileExchID
         NO-LOCK,
      EACH xPacketPB1
         WHERE xPacketPB1.PackDate     = xPacket.PackDate
         NO-LOCK,
      FIRST xFileExchPB1
         WHERE xFileExchPB1.FileExchID = xPacketPB1.FileExchID
           AND xFileExchPB1.Name       = "PB1_" + xFileExch.Name
         NO-LOCK,
      FIRST xPacketPB1Text OF xPacketPB1
         NO-LOCK:

         oDatPb1 = STRING(xPacketPB1.PackDate,"99.99.9999") + " " + STRING(xPacketPB1.PackTime,"hh:mm:ss").
         oRezPb1 = GetFileVar440(STRING(xPacketPB1Text.Contents),   "КодРезПроверки","").
         oStPb1  = xPacketPB1.State.

         FOR EACH xPackObjectPB1
            WHERE xPackObjectPB1.PacketID  = xPacketPB1.PacketID
              AND xPackObjectPB1.file-name = "Packet"
              AND xPackObjectPB1.Kind      = "TTaxKWT"
            NO-LOCK,
         FIRST xPacketKWT
            WHERE xPacketKWT.PacketID      = INT64(xPackObjectPB1.Surrogate)
            NO-LOCK,
         FIRST xPacketKWTText OF xPacketKWT
            NO-LOCK:

            oRezKwt = GetFileVar440(STRING(xPacketKWTText.Contents),"КодРезПроверки","").
            LEAVE.
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE PacketPB2:
   DEFINE INPUT  PARAMETER PackID   AS INT64     NO-UNDO. /* Исходное сообщение */
   DEFINE OUTPUT PARAMETER oPB2Date AS CHARACTER NO-UNDO INIT "".
   DEFINE OUTPUT PARAMETER oPB2File AS CHARACTER NO-UNDO INIT "".
   DEFINE OUTPUT PARAMETER oPB2Stat AS CHARACTER NO-UNDO INIT "".
   DEFINE OUTPUT PARAMETER oPB2Comm AS CHARACTER NO-UNDO INIT "".
   DEFINE OUTPUT PARAMETER oPB2Kwt  AS CHARACTER NO-UNDO INIT "".

   DEFINE VARIABLE oPB2Acct     AS CHARACTER NO-UNDO INIT "".
   DEFINE VARIABLE I            AS INTEGER   NO-UNDO.
   DEF BUFFER xPackObject       FOR PackObject.
   DEF BUFFER xPacketPB2        FOR Packet.
   DEF BUFFER xPacketPB2Text    FOR PacketText.
   DEF BUFFER xFileExchPB2      FOR FileExch.
   DEF BUFFER xPackObjectPB2    FOR PackObject.
   DEF BUFFER xPacketKWT        FOR Packet.
   DEF BUFFER xPacketKWTText    FOR PacketText.

   FOR EACH xPackObject
      WHERE xPackObject.PacketID    = PackID
        AND xPackObject.file-name   = "Packet"
        AND xPackObject.Kind        = "TTAXConf"
      NO-LOCK,
   FIRST xPacketPB2
      WHERE xPacketPB2.PacketID     = INT64(xPackObject.Surrogate)
      NO-LOCK,
   FIRST xFileExchPB2
      WHERE xFileExchPB2.FileExchID = xPacketPB2.FileExchID
        AND xFileExchPB2.Name  BEGINS "PB2"
      NO-LOCK,
   FIRST xPacketPB2Text OF xPacketPB2
      NO-LOCK:

      oPB2Date = oPB2Date + (IF (oPB2Date = "") THEN "" ELSE "~n")
               + STRING(xPacketPB2.PackDate,"99.99.9999") + " " + STRING(xPacketPB2.PackTime,"hh:mm:ss").
      oPB2File = oPB2File + (IF (oPB2File = "") THEN "" ELSE "~n")
               + xFileExchPB2.Name.
      oPB2Stat = oPB2Stat + (IF (oPB2Stat = "") THEN "" ELSE "~n")
               + xPacketPB2.State.
      oPB2Comm = oPB2Comm + (IF (oPB2Comm = "") THEN "" ELSE "~n")
               + GetFileAllVar440(STRING(xPacketPB2Text.Contents),"Пояснение","").
      oPB2Acct = oPB2Acct + (IF (oPB2Acct = "") THEN "" ELSE "~n")
               + GetFileAllVar440(STRING(xPacketPB2Text.Contents),"ЗначРекв","").

     FOR EACH xPackObjectPB2
        WHERE xPackObjectPB2.PacketID  = xPacketPB2.PacketID
          AND xPackObjectPB2.file-name = "Packet"
          AND xPackObjectPB2.Kind      = "TTaxKWT"
        NO-LOCK,
     FIRST xPacketKWT
        WHERE xPacketKWT.PacketID      = INT64(xPackObjectPB2.Surrogate)
        NO-LOCK,
     FIRST xPacketKWTText OF xPacketKWT
        NO-LOCK:

        oPB2Kwt = GetFileVar440(STRING(xPacketKWTText.Contents),"КодРезПроверки","").
        LEAVE.
     END.
   END.

   DO I = 1 TO NUM-ENTRIES(oPB2Acct, "~n"):
      ENTRY(I, oPB2Comm, "~n") = ENTRY(I, oPB2Acct, "~n") + " - " + ENTRY(I, oPB2Comm, "~n").
   END.
END PROCEDURE.

PROCEDURE NIanswer:
   DEFINE INPUT  PARAMETER PackID   AS INT64     NO-UNDO. /* Исходное сообщение */
   DEFINE OUTPUT PARAMETER oNIkwt   AS CHARACTER NO-UNDO.

   DEF BUFFER xPackObject       FOR PackObject.
   DEF BUFFER xPacketKWT        FOR Packet.
   DEF BUFFER xPacketKWTText    FOR PacketText.

   oNIkwt = "".

   FOR EACH xPackObject
      WHERE xPackObject.PacketID  = PackID
        AND xPackObject.Kind      = "TTaxKWT"
        AND xPackObject.file-name = "Packet"
      NO-LOCK,
   FIRST xPacketKWT
      WHERE xPacketKWT.PacketID   = INT64(xPackObject.Surrogate)
      NO-LOCK,
   FIRST xPacketKWTText OF xPacketKWT
      NO-LOCK:

      oNIkwt = GetFileVar440(STRING(xPacketKWTText.Contents),"КодРезПроверки","").
      LEAVE.
   END.
END PROCEDURE.
