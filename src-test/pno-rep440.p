/*
     Filename: pno-rep440.p
      Comment: Контрольная ведомость сообщений 440-П
   Parameters:
         Uses:
      Used by:
      Created:
     Modified: 
*/

DEFINE VARIABLE vPackFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNum          AS INT64     NO-UNDO.
DEFINE VARIABLE vNameOrg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTypeInf      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidZapr      AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE mTypZapr      AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE dt            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGroup        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateTimePNO  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateTimeBNP  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStatePNO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStateBNP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileNamePNO  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileNameBNP  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLocalTime    AS INT64     NO-UNDO.

DEFINE VARIABLE rowStyleGroup    AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStylePB2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleStatePNO AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleStateBNP AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleOst      AS CHARACTER NO-UNDO.

DEFINE VARIABLE mINNPP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpStr          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpResult       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpAcct         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpOst          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt             AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2            AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt             AS INT64     NO-UNDO.
DEFINE VARIABLE mSpin            AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mPB2Acct         AS CHARACTER NO-UNDO.

DEFINE VARIABLE mResultBVS       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTBVS    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultPB1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTPB1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultPB2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTPB2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResultKWTBNP    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mOstForm         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mOstFormLocal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mOstFile         AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFileName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileRep         AS CHARACTER NO-UNDO.

DEF VAR rowAcctStyle AS CHAR NO-UNDO.

DEF VAR rowStyle AS CHAR NO-UNDO.
DEF VAR rowStyle77 AS CHAR NO-UNDO.
DEF VAR rowStyle78 AS CHAR NO-UNDO.
DEF VAR rowStyle79 AS CHAR NO-UNDO.

DEF VAR zAcct AS CHAR NO-UNDO.

DEF BUFFER xPacket           FOR Packet.

DEF BUFFER xPacketBVS        FOR Packet.
DEF BUFFER xPacketBVSText    FOR PacketText.
DEF BUFFER xFileExchBVS      FOR FileExch.
DEF BUFFER xPacketKWTBVS     FOR Packet.
DEF BUFFER xPacketKWTBVSText FOR PacketText.
DEF BUFFER xFileExchKWTBVS   FOR FileExch.

DEF BUFFER xPacketPB1        FOR Packet.
DEF BUFFER xPacketPB1Text    FOR PacketText.
DEF BUFFER xFileExchPB1      FOR FileExch.
DEF BUFFER xPacketKWTPB1     FOR Packet.
DEF BUFFER xPacketKWTPB1Text FOR PacketText.
DEF BUFFER xFileExchKWTPB1   FOR FileExch.

DEF BUFFER xPacketPB2        FOR Packet.
DEF BUFFER xPacketPB2Text    FOR PacketText.
DEF BUFFER xFileExchPB2      FOR FileExch.
DEF BUFFER xPacketKWTPB2     FOR Packet.
DEF BUFFER xPacketKWTPB2Text FOR PacketText.
DEF BUFFER xFileExchKWTPB2   FOR FileExch.

DEF BUFFER xPacketBNP        FOR Packet.
DEF BUFFER xPacketBNPText    FOR PacketText.
DEF BUFFER xFileExchBNP      FOR FileExch.
DEF BUFFER xPacketKWTBNP     FOR Packet.
DEF BUFFER xPacketKWTBNPText FOR PacketText.
DEF BUFFER xFileExchKWTBNP   FOR FileExch.

DEF BUFFER xPackObjectBVS    FOR PackObject.
DEF BUFFER xPackObjectPB1    FOR PackObject.
DEF BUFFER xPackObjectPB2    FOR PackObject.
DEF BUFFER xPackObjectBNP    FOR PackObject.

DEF VAR postcmd AS c NO-UNDO. /* д.р. PostCmd */

FUNCTION GetFileVar440 RETURNS char
   (INPUT iContent AS CHARACTER, 
   INPUT iTag      AS CHARACTER,
   INPUT iDefault  AS CHARACTER).
   DEFINE VARIABLE mInt1 AS INT64 NO-UNDO.
   DEFINE VARIABLE mInt2 AS INT64 NO-UNDO.
   DEFINE VARIABLE mInt3 AS INT64 NO-UNDO.
   DEFINE VARIABLE oRet  AS CHAR  NO-UNDO.
   iTag = CODEPAGE-CONVERT(iTag,"1251","ibm866").
   mInt1 = INDEX(iContent,iTag).
   IF mInt1 GT 0 THEN
   DO:
      mInt2 = INDEX(iContent,'"',mInt1).    
      mInt3 = INDEX(iContent,'"',mInt2 + 1).
      oRet = SUBSTRING(iContent,mInt2 + 1,(mInt3 - mInt2 - 1)).
      oRet = CODEPAGE-CONVERT(oRet,"ibm866","1251").
      RETURN oRet.
   END.
   ELSE RETURN iDefault.
END FUNCTION.

FUNCTION GetFileAllVar440 RETURNS char
   (INPUT iContent AS CHARACTER, 
   INPUT iTag      AS CHARACTER,
   INPUT iDefault  AS CHARACTER).
   
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
         oRet1 = SUBSTRING(iContent,mInt2 + 1,(mInt3 - mInt2 - 1)).
         oRet1 = CODEPAGE-CONVERT(oRet1,"ibm866","1251").
         oRet2 = IF oRet2 EQ "" THEN oRet1 ELSE oRet2 + "," + oRet1.
         mInt4 = mInt1. 
      END.
      ELSE LEAVE.
   END.
   IF oRet2 NE "" THEN RETURN oRet2.
   ELSE RETURN iDefault.
END FUNCTION.

{globals.i}
{parsin.def}
{intrface.get prnvd}
{sh-defs.i}
{440p-rep.fun}
{debug.equ}

{getdates.i}

/*beg-date = DATE("23/08/2017").*/
/*end-date = DATE("09/09/2017").*/

mFileRep = "pno-" + 
   STRING(YEAR(TODAY),"9999") + 
   STRING(MONTH(TODAY),"99") + 
   STRING(DAY(TODAY),"99") + "-" +
   TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileRep = REPLACE(mFileRep,":","").

OUTPUT TO VALUE(mFileRep) UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

mSpin[1] = ".   ".
mSpin[2] = "..  ".
mSpin[3] = "... ".
mSpin[4] = "....".
mSpin[5] = " ...".
mSpin[6] = "  ..".
mSpin[7] = "   .".
mSpin[8] = "    ".

{pno-rep440.hdr}

vNum = 0.
mInt = 0.
RUN dbgprint.p ("pno-rep440 ","Begin " + STRING(TIME,"hh:mm:ss")).

FOR EACH Packet WHERE TRUE
   AND CAN-DO ("PTax440",TRIM(Packet.Class-Code))
   /*AND Packet.PacketID EQ 17837382   /*ZNO*/*/
   /*AND Packet.PacketID EQ 34847243*/
   /*AND Packet.PacketID EQ 35773268*/   /*PNO*/
   AND Packet.PackDate  GE beg-date 
   AND Packet.PackDate  LE end-date 
   AND Packet.filial-id EQ shFilial NO-LOCK
   BY Packet.PackDate:

   vNum = vNum + 1.

/*   RUN dbgprint.p ("pno-rep440 ","Packet.PacketID = " + STRING(Packet.PacketID) + "~nvNum = " + STRING(vNum)).*/
      
   FIND FIRST PacketText OF Packet NO-LOCK NO-ERROR.

   FIND FIRST FileExch WHERE
      FileExch.FileExchID EQ Packet.FileExchID NO-LOCK NO-ERROR.
   IF AVAILABLE(FileExch)
   THEN mFileNamePNO = FileExch.Name.
   ELSE mFileNamePNO = "".
   
   RELEASE xPacket.

   RELEASE xPacketBVS.
   RELEASE xPacketBVSText.
   RELEASE xFileExchBVS.
   RELEASE xPacketKWTBVS.
   RELEASE xPacketKWTBVSText.
   RELEASE xFileExchKWTBVS.

   RELEASE xPacketPB1.
   RELEASE xPacketPB1Text.
   RELEASE xFileExchPB1.
   RELEASE xPacketKWTPB1.
   RELEASE xPacketKWTPB1Text.
   RELEASE xFileExchKWTPB1.

   RELEASE xPacketPB2.
   RELEASE xPacketPB2Text.
   RELEASE xFileExchPB2.
   RELEASE xPacketKWTPB2.
   RELEASE xPacketKWTPB2Text.
   RELEASE xFileExchKWTPB2.
   
   RELEASE xPacketBNP.
   RELEASE xPacketBNPText.
   RELEASE xFileExchBNP.
   RELEASE xPacketKWTBNP.
   RELEASE xPacketKWTBNPText.
   RELEASE xFileExchKWTBNP.
   
   RELEASE xPackObjectBVS.
   RELEASE xPackObjectPB1.
   RELEASE xPackObjectPB2.
   RELEASE xPackObjectBNP.
	
   IF AVAIL(PacketText) THEN
   DO:
      mTypeInf = GetFileVar440(STRING(PacketText.Contents),"ТипИнф","").
      mVidZapr = GetFileVar440(STRING(PacketText.Contents),"ВидЗапр","").
      mTypZapr = GetFileVar440(STRING(PacketText.Contents),"ТипЗапр","").
   END.
   
   RUN dbgprint.p ("pno-rep440 ",
      "Packet.PacketID = " + STRING(Packet.PacketID) +
      "~nFileExch = " + STRING(AVAIL(FileExch)) +
      "~nmFileNamePNO = " + mFileNamePNO + 
      "~nPacketText = " + STRING(AVAIL(PacketText)) +
      "~nВидЗапр = " + mVidZapr +
      "~nТипЗапр = " + mTypZapr +
      "~nТипИнф  = " + mTypeInf
      ).
   
   IF mTypeInf EQ 'ПОРУЧСЧЕТН' OR SUBSTRING(mFileNamePNO,1,3) EQ "PNO" THEN
   DO:

      mDateTimePNO = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss").
      
      mStatePNO    = Packet.State.
      IF mStatePNO EQ "ОШБК"
      THEN rowStyleStatePNO = "s78red".
      ELSE rowStyleStatePNO = "s78".
      
      mFileName = ''.
      FOR EACH PackObject
         WHERE PackObject.PacketID = Packet.PacketID
         AND PackObject.file-name  = "Packet" 
         NO-LOCK:
   
         FIND FIRST xPacket WHERE 
            xPacket.PacketID = INT64(PackObject.Surrogate) 
         NO-LOCK NO-ERROR.
   
         /*BVS*/
         mFileName = "".
         IF AVAIL(xPacket) THEN 
         DO:
            /*RUN dbgprint.p ("pno-rep440 " + " {&line-number}","mFileName = " + mFileName).*/
            IF xPacket.class-code EQ "PTax440"
               AND xPacket.mail-format EQ "XFNSBOS440" THEN 
            DO:
               FIND FIRST xPacketBVS WHERE xPacketBVS.PacketID = INT64(PackObject.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketBVSText OF xPacketBVS NO-LOCK NO-ERROR.
               FIND FIRST xFileExchBVS WHERE xFileExchBVS.FileExchID EQ xPacketBVS.FileExchID NO-LOCK NO-ERROR.
               FOR EACH xPackObjectBVS WHERE TRUE
                  AND xPackObjectBVS.PacketID EQ xPacketBVS.PacketID 
                  AND xPackObjectBVS.Kind      EQ "TTaxKWT" 
                  AND xPackObjectBVS.file-name EQ "Packet" 
                  NO-LOCK:
                  LEAVE.
               END.
               FIND FIRST xPacketKWTBVS WHERE xPacketKWTBVS.PacketID = INT64(xPackObjectBVS.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketKWTBVSText OF xPacketKWTBVS NO-LOCK NO-ERROR.
               FIND FIRST xFileExchKWTBVS WHERE xFileExchKWTBVS.FileExchID EQ xPacketKWTBVS.FileExchID NO-LOCK NO-ERROR.
            END.
         END.
         
         /*PB1*/
         mFileName = "".
         IF AVAIL(xPacket) THEN 
         DO:
            mFileName = GetXAttrValueEx("Packet",STRING(xPacket.PacketID),"FileName","").
            /*RUN dbgprint.p ("pno-rep440 " + " {&line-number}","mFileName = " + mFileName).*/
            IF mFileName BEGINS 'PB1' THEN
            DO:
               FIND FIRST xPacketPB1 WHERE xPacketPB1.PacketID = INT64(PackObject.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketPB1Text OF xPacketPB1 NO-LOCK NO-ERROR.
               FIND FIRST xFileExchPB1 WHERE xFileExchPB1.FileExchID EQ xPacketPB1.FileExchID NO-LOCK NO-ERROR.
               FOR EACH xPackObjectPB1 WHERE TRUE 
                  AND xPackObjectPB1.PacketID  EQ xPacketPB1.PacketID
                  AND xPackObjectPB1.Kind      EQ "TTaxKWT" 
                  AND xPackObjectPB1.file-name EQ "Packet" 
                  NO-LOCK:
                  LEAVE.
               END.
               FIND FIRST xPacketKWTPB1 WHERE xPacketKWTPB1.PacketID = INT64(xPackObjectPB1.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketKWTPB1Text OF xPacketKWTPB1 NO-LOCK NO-ERROR.
               FIND FIRST xFileExchKWTPB1 WHERE xFileExchKWTPB1.FileExchID EQ xPacketKWTPB1.FileExchID NO-LOCK NO-ERROR.
            END.
         END.
         
         /*PB2*/
         mFileName = "".
         IF AVAIL(xPacket) THEN
         DO:
            mFileName = GetXAttrValueEx("Packet",STRING(xPacket.PacketID),"FileName","").
            /*RUN dbgprint.p ("pno-rep440 " + " {&line-number}","mFileName = " + mFileName).*/
            IF mFileName BEGINS 'PB2' THEN
            DO:
               FIND FIRST xPacketPB2 WHERE xPacketPB2.PacketID = INT64(PackObject.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketPB2Text OF xPacketPB2 NO-LOCK NO-ERROR.
               FIND FIRST xFileExchPB2 WHERE xFileExchPB2.FileExchID EQ xPacketPB2.FileExchID NO-LOCK NO-ERROR.
               FOR EACH xPackObjectPB2 WHERE TRUE 
                  AND xPackObjectPB2.PacketID  EQ xPacketPB2.PacketID
                  AND xPackObjectPB2.Kind      EQ "TTaxKWT" 
                  AND xPackObjectPB2.file-name EQ "Packet" 
                  NO-LOCK:
                  LEAVE.
               END.
               FIND FIRST xPacketKWTPB2 WHERE xPacketKWTPB2.PacketID = INT64(xPackObjectPB2.Surrogate) NO-LOCK NO-ERROR.
               FIND FIRST xPacketKWTPB2Text OF xPacketKWTPB2 NO-LOCK NO-ERROR.
               FIND FIRST xFileExchKWTPB2 WHERE xFileExchKWTPB2.FileExchID EQ xPacketKWTPB2.FileExchID NO-LOCK NO-ERROR.
            END.
         END.
      END.
      
      /*BNP*/
      FOR EACH xFileExchBNP WHERE
         xFileExchBNP.Name BEGINS "BNP" 
         NO-LOCK:
         IF INDEX(xFileExchBNP.Name,RIGHT-TRIM(mFileNamePNO,".xml")) GT 0 THEN
         LEAVE.
      END.
      IF AVAIL(xFileExchBNP) THEN
      DO:
         FIND FIRST xPacketBNP WHERE xPacketBNP.FileExchID EQ xFileExchBNP.FileExchID NO-LOCK NO-ERROR.
         FIND FIRST xPacketBNPText OF xPacketBNP NO-LOCK NO-ERROR.
         FOR EACH xPackObjectBNP WHERE TRUE 
            AND xPackObjectBNP.PacketID  EQ xPacketBNP.PacketID
            AND xPackObjectBNP.Kind      EQ "TTaxKWT" 
            AND xPackObjectBNP.file-name EQ "Packet" 
            NO-LOCK:
            LEAVE.
         END.
         FIND FIRST xPacketKWTBNP WHERE xPacketKWTBNP.PacketID = INT64(xPackObjectBNP.Surrogate) NO-LOCK NO-ERROR.
         FIND FIRST xPacketKWTBNPText OF xPacketKWTBNP NO-LOCK NO-ERROR.
         FIND FIRST xFileExchKWTBNP WHERE xFileExchKWTBNP.FileExchID EQ xPacketKWTBNP.FileExchID NO-LOCK NO-ERROR.
      END.
      
      RUN dbgprint.p ("pno-rep440 ","Packet.PacketID = " + STRING(Packet.PacketID) + 
         "~nxPacketBVS        = " + STRING(AVAIL(xPacketBVS))  +
         "~nxPacketBVSText    = " + STRING(AVAIL(xPacketBVSText)) +
         "~nxPackObjectBVS    = " + STRING(AVAIL(xPackObjectBVS)) +
         "~nxPacketKWTBVS     = " + STRING(AVAIL(xPacketKWTBVS)) +
         "~nxPacketKWTBVSText = " + STRING(AVAIL(xPacketKWTBVSText)) +
          
         "~nxPacketPB1        = " + STRING(AVAIL(xPacketPB1))  +
         "~nxPacketPB1Text    = " + STRING(AVAIL(xPacketPB1Text))  +
         "~nxPackObjectPB1    = " + STRING(AVAIL(xPackObjectPB1)) +
         "~nxPacketKWTPB1     = " + STRING(AVAIL(xPacketKWTPB1)) +
         "~nxPacketKWTPB1Text = " + STRING(AVAIL(xPacketKWTPB1Text)) +
         
         "~nxPacketPB2        = " + STRING(AVAIL(xPacketPB2))  +
         "~nxPacketPB2Text    = " + STRING(AVAIL(xPacketPB2Text))  +
         "~nxPackObjectPB2    = " + STRING(AVAIL(xPackObjectPB2)) +
         "~nxPacketKWTPB2     = " + STRING(AVAIL(xPacketKWTPB2)) +
         "~nxPacketKWTPB2Text = " + STRING(AVAIL(xPacketKWTPB2Text)) + 
         
         "~nxPacketBNP        = " + STRING(AVAIL(xPacketBNP))  +
         "~nxPacketBNPText    = " + STRING(AVAIL(xPacketBNPText))  +
         "~nxPackObjectBNP    = " + STRING(AVAIL(xPackObjectBNP)) +
         "~nxPacketKWTBNP     = " + STRING(AVAIL(xPacketKWTBNP)) +
         "~nxPacketKWTBNPText = " + STRING(AVAIL(xPacketKWTBNPText))  
         ).
   
      mDateTimeBNP = "".
      mStateBNP    = "".
      mFileNameBNP = "".
      rowStyleStateBNP = "s78".
      IF AVAIL xPacketBNP THEN
      DO:
         mDateTimeBNP = STRING(xPacketBNP.PackDate,"99.99.9999") + " " + STRING(xPacketBNP.PackTime,"hh:mm:ss").
         mStateBNP    = xPacketBNP.State.
         IF mStateBNP EQ "ОШБК"
         THEN rowStyleStateBNP = "s78red".
         ELSE rowStyleStateBNP = "s78".
         mFileNameBNP = IF AVAIL(xFileExchBNP) THEN xFileExchBNP.Name ELSE GetXAttrValueEx("Packet",STRING(xPacketBNP.PacketID),"FileName",""). 
      END.
   
      RUN dbgprint.p ("pno-rep440 ",
         "~nmDateTimeBNP  = " + mDateTimeBNP +
         "~nmStateBNP     = " + mStateBNP    +
         "~nmFileNameBNP  = " + mFileNameBNP
         ).
   
      rowStyle   = "s68".
      rowStyle77 = "s77".
      rowStyle78 = "s78".
      rowStyle79 = "s79".
      
      PUT UNFORMATTED
         '   <Row ss:AutoFitHeight="1">~n' +
      	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimePNO + '</Data></Cell>~n' +
      	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNamePNO + '</Data></Cell>~n' +
      	'    <Cell ss:StyleID="' + rowStyleStatePNO + '"><Data ss:Type="String">' + mStatePNO + '</Data></Cell>~n'.
   
   
      IF AVAIL(xPacketPB2Text) THEN rowStylePB2 = "s77red". ELSE rowStylePB2 = "s77".
   
      mResultPB1    = IF AVAIL(xPacketPB1Text)    THEN GetFileVar440(STRING(xPacketPB1Text.Contents),"КодРезПроверки","") ELSE "".
      mResultKWTPB1 = IF AVAIL(xPacketKWTPB1Text) THEN GetFileVar440(STRING(xPacketKWTPB1Text.Contents),"КодРезПроверки","") ELSE "".
      mResultKWTBVS = IF AVAIL(xPacketKWTBVSText) THEN GetFileVar440(STRING(xPacketKWTBVSText.Contents),"КодРезПроверки","") ELSE "".
      mResultKWTBNP = IF AVAIL(xPacketKWTBNPText) THEN GetFileVar440(STRING(xPacketKWTBNPText.Contents),"КодРезПроверки","") ELSE "".
      
      PUT UNFORMATTED
         '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultPB1    + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultKWTPB1 + '</Data></Cell>~n'.
   
      mPB2Acct   = "".
      mResultPB2 = "".
      
      mPB2Acct   = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"ЗначРекв","") ELSE "".
      mTmpResult = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"Пояснение","") ELSE "".
      
      mResultPB2 = "".
      
      PUT UNFORMATTED
         '    <Cell ss:StyleID="' + rowStyle77  + '"><Data ss:Type="String">' + mTmpResult    + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + rowStyle77  + '"><Data ss:Type="String">' + mFileNameBNP  + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultKWTBNP + '</Data></Cell>~n' +
         '   </Row>'
      SKIP.

      IF vNum MOD 8 EQ 0 THEN
      DO:
         mInt = mInt + 1.
         PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 'Отчет формируется' + mSpin[mInt MOD 8 + 1] COLOR bright.
      END.
   END.
END.

PUT UNFORMATTED
   '  </Table>~n' + 
   '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">~n' +
   '   <PageSetup><Layout x:Orientation="Landscape"/></PageSetup>~n' +
   '   <Unsynced/>~n' +
   '   <FitToPage/>~n' +
   '   <Selected/>~n' +
   '  </WorksheetOptions>~n' +
   ' </Worksheet>~n' +
   '</Workbook>'
SKIP.

OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileRep + ";class=bq").

{intrface.del}          /* Выгрузка инструментария. */ 



