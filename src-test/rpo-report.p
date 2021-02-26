/*
               Å†≠™Æ¢·™†Ô ®≠‚•£‡®‡Æ¢†≠≠†Ô ·®·‚•¨† Åàë™¢®‚
    Copyright:
     Filename: rpo-report.p
      Comment: äÆ≠‚‡Æ´Ï≠†Ô ¢•§Æ¨Æ·‚Ï ·ÆÆ°È•≠®© 365Ø
   Parameters:
         Uses:
      Used by:
      Created:
     Modified: 
*/

/*DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.*/

DEFINE VARIABLE iParam AS CHARACTER NO-UNDO.

iParam = "í®ØÇ•§=XLS".

DEFINE VARIABLE vStat         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRepFileName  AS CHARACTER NO-UNDO INIT "rpo-report.xml".
DEFINE VARIABLE vPackFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNum          AS INT64     NO-UNDO.
DEFINE VARIABLE vNameOrg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTypeInf      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidZapr      AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE mTypZapr      AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE dt            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGroup        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateTimeRPO  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateTimeBOS  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStateRPO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStateBOS     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileNameRPO  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileNameBOS  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLocalTime    AS INT64     NO-UNDO.

DEFINE VARIABLE rowStyleGroup    AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStylePB2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleStateRPO AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleStateBOS AS CHARACTER NO-UNDO.
DEFINE VARIABLE rowStyleOst      AS CHARACTER NO-UNDO.

DEF VAR shb  AS DECIMAL NO-UNDO.
DEF VAR shb1 AS DECIMAL NO-UNDO.
DEF VAR innnp AS CHAR NO-UNDO.
DEF VAR rowAcctStyle AS CHAR NO-UNDO.
DEF VAR sprOst AS CHAR NO-UNDO.
DEF VAR rowStyle AS CHAR NO-UNDO.
DEF VAR rowStyle77 AS CHAR NO-UNDO.
DEF VAR rowStyle78 AS CHAR NO-UNDO.
DEF VAR rowStyle79 AS CHAR NO-UNDO.
DEF VAR mFileName AS CHAR NO-UNDO.
DEF VAR zAcct AS CHAR NO-UNDO.

DEF BUFFER xPacket           FOR Packet.

DEF BUFFER xPacketSpr        FOR Packet.
DEF BUFFER xPacketSprText    FOR PacketText.
DEF BUFFER xPacketKwSpr      FOR Packet.
DEF BUFFER xPacketKwSprText  FOR PacketText.
DEF BUFFER xFileExchKwSpr    FOR FileExch.

DEF BUFFER xPacketOtv1       FOR Packet.
DEF BUFFER xPacketOtvText1   FOR PacketText.
DEF BUFFER xFileExchOtv1     FOR FileExch.
DEF BUFFER xPacketKwOtv1     FOR Packet.
DEF BUFFER xPacketKwOtvText1 FOR PacketText.
DEF BUFFER xFileExchKwOtv1   FOR FileExch.

DEF BUFFER xPacketOtv2       FOR Packet.
DEF BUFFER xPacketOtvText2   FOR PacketText.
DEF BUFFER xFileExchOtv2     FOR FileExch.
DEF BUFFER xPacketKwOtv2     FOR Packet.
DEF BUFFER xPacketKwOtvText2 FOR PacketText.
DEF BUFFER xFileExchKwOtv2   FOR FileExch.

DEF BUFFER xPackObjectOtv1   FOR PackObject.
DEF BUFFER xPackObjectOtv2   FOR PackObject.

DEF VAR Prc  AS CHAR INIT "~\~|~/-" NO-UNDO.
DEF VAR postcmd AS c NO-UNDO. /* §.‡. PostCmd */

{globals.i}
{parsin.def}
{intrface.get prnvd}
{sh-defs.i}
{365p-rep.fun}
{debug.equ}

ASSIGN
   vStat   = GetParamByNameAsChar(iParam,"ë‚†‚„·","")
   vFile   = GetParamByNameAsChar(iParam,"î†©´","")
   vType   = GetParamByNameAsChar(iParam,"í®ØÇ•§","")
.
dt = ";".

{getdates.i}

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="rpo-report.xml" }

{rpo-report.hdr}

FOR EACH Packet WHERE
   Packet.Class-Code BEGINS 'PTAX'
   /*AND Packet.PacketID EQ 17837382   /*ZNO*/*/
   /*AND Packet.PacketID EQ 25940295   /*RPO*/*/
   AND CAN-DO( "!ETaxKwt,ETAX*", Packet.Kind) 
   AND Packet.PackDate  GE beg-date 
   AND Packet.PackDate  LE end-date 
   AND Packet.filial-id EQ shFilial NO-LOCK,
   FIRST Seance WHERE
   Seance.SeanceID EQ Packet.SeanceID 
   AND Packet.State EQ "áÄÉê" 
   AND Seance.direct EQ "à¨ØÆ‡‚"
   NO-LOCK BY Packet.PackDate:

   RUN dbgprint.p ("rpo-report ","Packet.PacketID: " + STRING(Packet.PacketID)).

   FIND FIRST FileExch WHERE
      FileExch.FileExchID EQ Packet.FileExchID NO-LOCK NO-ERROR.
   IF AVAILABLE(FileExch) 
   THEN mFileNameRPO = FileExch.Name.
   ELSE mFileNameRPO = "". 
   
   FIND FIRST PacketText OF Packet NO-LOCK NO-ERROR.
	
   RELEASE xPacket.
   
   RELEASE xPacketSpr.
   RELEASE xPacketSprText.
   RELEASE xPacketKwSpr.
   RELEASE xPacketKwSprText.
   RELEASE xFileExchKwSpr.
   
   RELEASE xPacketOtv1.
   RELEASE xPacketOtvText1.
   RELEASE xFileExchOtv1.
   RELEASE xPacketKwOtv1.
   RELEASE xPacketKwOtvText1.
   RELEASE xFileExchKwOtv1.
   
   RELEASE xPacketOtv2.
   RELEASE xPacketOtvText2.
   RELEASE xFileExchOtv2.
   RELEASE xPacketKwOtv2.
   RELEASE xPacketKwOtvText2.
   RELEASE xFileExchKwOtv2.
   
   mTypeInf = GetFileVar( STRING(PacketText.Contents),"í®Øà≠‰",'').
   mVidZapr = GetFileVar( STRING(PacketText.Contents),"Ç®§á†Ø‡",'').
   mTypZapr = GetFileVar( STRING(PacketText.Contents),"í®Øá†Ø‡",'').
   
   IF mTypeInf NE 'êÖòÖçèêàéëí' THEN NEXT.

   mDateTimeRPO = STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss").
   
   
   mStateRPO    = Packet.State.
   IF mStateRPO EQ "éòÅä"
   THEN rowStyleStateRPO = "s78red".
   ELSE rowStyleStateRPO = "s78".
   
   mFileName = ''.
   FOR EACH PackObject
      WHERE PackObject.PacketID = Packet.PacketID
      AND PackObject.file-name  = "Packet" 
      NO-LOCK:
      
      /*BOS*/
      FIND FIRST xPacket WHERE 
         xPacket.PacketID = INT64(PackObject.Surrogate) 
      NO-LOCK NO-ERROR.
      IF AVAIL(xPacket) 
         AND xPacket.class-code = "PTaxRSSprav" THEN
      DO:
         FIND FIRST xPacketSpr WHERE 
            xPacketSpr.PacketID = INT64(PackObject.Surrogate) 
         NO-LOCK NO-ERROR.
      END.
      
      /*PB1*/
      FIND FIRST xPacketOtv1 WHERE 
         xPacketOtv1.PacketID = INT64(PackObject.Surrogate) 
      NO-LOCK NO-ERROR.
      IF AVAIL(xPacketOtv1) THEN
      DO:
         mFileName = GetXAttrValueEx("Packet",STRING(xPacketOtv1.PacketID),"FileName","").
         IF xPacketOtv1.class-code EQ "PTax" 
            AND mFileName BEGINS 'PB1' THEN 
         DO:   
            FIND FIRST xPacketOtvText1 OF xPacketOtv1 NO-LOCK NO-ERROR.
            FIND FIRST xFileExchOtv1 WHERE
               xFileExchOtv1.FileExchID EQ xPacketOtv1.FileExchID 
            NO-LOCK NO-ERROR.
            FIND LAST xPackObjectOtv1 WHERE 
                   xPackObjectOtv1.Surrogate EQ STRING(xPacketOtv1.PacketID) 
               AND xPackObjectOtv1.Kind      EQ "TTaxKWT" 
               AND xPackObjectOtv1.file-name EQ "Packet" 
            NO-LOCK NO-ERROR.
            FIND FIRST xPacketKwOtv1 WHERE 
               xPacketKwOtv1.PacketID = xPackObjectOtv1.PacketID 
            NO-LOCK NO-ERROR.
            FIND FIRST xPacketKwOtvText1 OF xPacketKwOtv1 NO-LOCK NO-ERROR.
            FIND FIRST xFileExchKwOtv1 WHERE 
               xFileExchKwOtv1.FileExchID EQ xPacketKwOtv1.FileExchID 
            NO-LOCK NO-ERROR.
         END.
      END.
      
      /*PB2*/
      FIND FIRST xPacketOtv2 WHERE 
         xPacketOtv2.PacketID = INT64(PackObject.Surrogate) 
      NO-LOCK NO-ERROR.
      IF AVAIL(xPacketOtv2) THEN
      DO:
         mFileName = GetXAttrValueEx("Packet",STRING(xPacketOtv2.PacketID),"FileName","").
         IF xPacketOtv2.class-code EQ "PTax" 
            AND mFileName BEGINS 'PB2' THEN 
         DO:   
            FIND FIRST xPacketOtvText2 OF xPacketOtv2 NO-LOCK NO-ERROR.
            FIND FIRST xFileExchOtv2 WHERE
               xFileExchOtv2.FileExchID EQ xPacketOtv2.FileExchID 
            NO-LOCK NO-ERROR.
            FIND LAST xPackObjectOtv2 WHERE 
                   xPackObjectOtv2.Surrogate EQ STRING(xPacketOtv2.PacketID) 
               AND xPackObjectOtv2.Kind      EQ "TTaxKWT" 
               AND xPackObjectOtv2.file-name EQ "Packet" 
            NO-LOCK NO-ERROR.
            FIND FIRST xPacketKwOtv2 WHERE 
               xPacketKwOtv2.PacketID = xPackObjectOtv2.PacketID 
            NO-LOCK NO-ERROR.
            FIND FIRST xPacketKwOtvText2 OF xPacketKwOtv2 NO-LOCK NO-ERROR.
            FIND FIRST xFileExchKwOtv2 WHERE 
               xFileExchKwOtv2.FileExchID EQ xPacketKwOtv2.FileExchID 
            NO-LOCK NO-ERROR.
         END.
      END.
   END.

   RUN dbgprint.p ("rpo-report ", "xPacketSpr = " + STRING(AVAIL(xPacketSpr))).

   mDateTimeBOS = "".
   mStateBOS    = "".
   mFileNameBOS = "".
   rowStyleStateBOS = "s78".
   IF AVAIL xPacketSpr THEN
   DO:
      mDateTimeBOS = STRING(xPacketSpr.PackDate,"99.99.9999") + " " + STRING(xPacketSpr.PackTime,"hh:mm:ss").
      mStateBOS    = xPacketSpr.State.
      IF mStateBOS EQ "éòÅä"
      THEN rowStyleStateBOS = "s78red".
      ELSE rowStyleStateBOS = "s78".
      mFileNameBOS = GetXAttrValueEx("Packet",STRING(xPacketSpr.PacketID),"FileName","").
      FIND FIRST xPacketSprText OF xPacketSpr NO-LOCK NO-ERROR.
      FIND LAST PackObject WHERE
         PackObject.Surrogate = STRING(xPacketSpr.PacketID)
         AND PackObject.Kind = "TTaxKWT"
         AND PackObject.file-name = "Packet"
         NO-LOCK NO-ERROR.
      FIND FIRST xPacketKwSpr WHERE
         xPacketKwSpr.PacketID = PackObject.PacketID
         NO-LOCK NO-ERROR.
      FIND FIRST xPacketKwSprText OF xPacketKwSpr NO-LOCK NO-ERROR.
      FIND FIRST xFileExchKwSpr WHERE
         xFileExchKwSpr.FileExchID EQ xPacketKwSpr.FileExchID
         NO-LOCK NO-ERROR.
      RUN dbgprint.p ("rpo-report ", "xPacketSpr.PacketID = " + STRING(xPacketSpr.PacketID)).
      RUN dbgprint.p ("rpo-report ", "PackObject = " + STRING(AVAIL(PackObject))).
   END.

   RUN dbgprint.p ("rpo-report ","xPacketSprText = " + STRING(AVAIL(xPacketSprText))).
   RUN dbgprint.p ("rpo-report ","xPacketKwSpr = " + STRING(AVAIL(xPacketKwSpr))).
   RUN dbgprint.p ("rpo-report ","xFileExchKwSpr = " + STRING(AVAIL(xFileExchKwSpr))).
   
   vPackFileName = GetXAttrValueEx("Packet", STRING(Packet.PacketID), "FileName", "").
   IF AVAILABLE(FileExch) AND CAN-DO("SBF*",FileExch.Name) THEN NEXT.
   
   vNum = vNum + 1.                

    

   
   rowStyle = "s68".
   rowStyle77 = "s77".
   rowStyle78 = "s78".
   rowStyle79 = "s79".
   
/*   IF AVAIL xPacketOtv AND xPacketOtv.State NE "ëäÇí" THEN rowStyle = "s68wait".    */
/* IF not AVAIL xPacketOtv OR xPacketOtv.State NE "ëéáÑ"  THEN rowStyle = "s68error". */
/*   IF AVAIL xPacketOtv AND xPacketOtv.State EQ "éòÅä" THEN rowStyle = "s68error".   */

/*   IF CAN-DO( "áÄèêéëçé,êÖòÖçèêàéëí", mTypeInf) THEN                                */
/*   DO:                                                                              */
/*      IF AVAIL xPacketSpr AND xPacketSpr.State NE "ëäÇí" THEN rowStyle = "s68wait". */
/*      IF AVAIL xPacketSpr AND xPacketSpr.State EQ "éòÅä" THEN rowStyle = "s68error".*/
/*   END.                                                                             */

   /* ≠†§Æ Ø•‡•°‡†‚Ï ¢·• ·Á•‚† ß†Ø‡Æ·† */
   
   zAcct = ''.
   IF (CAN-DO( "áÄèêéëçé", mTypeInf) 
      AND (mVidZapr EQ '2' OR mVidZapr EQ '3') /* ß†Ø‡Æ· ¢ÎØ®·™® ØÆ ÆØ•‡†Ê®Ô¨ ≠† ·Á•‚• */
      )
      OR (mTypeInf EQ 'êÖòÖçèêàéëí') THEN 
   DO:
      /*DEF VAR mTypZapr AS CHAR NO-UNDO INIT ''.                        */
      /*mTypZapr = GetFileVar( STRING(PacketText.Contents),"í®Øá†Ø‡",'').*/
      IF mTypZapr EQ '1' THEN zAcct = '*'.
      IF mTypZapr EQ '2' OR (mTypeInf EQ 'êÖòÖçèêàéëí') THEN 
      DO:
         DEF VAR ii AS INT NO-UNDO.
         ii = 1.
         DO WHILE YES:
            DEF VAR x AS CHAR NO-UNDO.
            x = GetFileVarEntry(STRING(PacketText.Contents),"çÆ¨ëÁ",ii,'').
            IF x EQ '' THEN LEAVE.
            zAcct = (IF LENGTH(zAcct) > 0 THEN zAcct + ',' ELSE '') + x.
            ii = ii + 1.
         END.
      END.
   END.
   
/*   {find-act.i                                                                          */
/*      &acct = zAcct                                                                     */
/*   }                                                                                    */
/*                                                                                        */
/*   IF AVAIL(acct)                                                                       */
/*   THEN mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").*/
/*   ELSE mGroup = "".                                                                    */
/*                                                                                        */

   RUN dbgprint.p ("rpo-report","~n" +
                   "zAcct : " + zAcct
                   ).
   
   IF zAcct NE "" THEN
   DO:
      
      innnp = GetFileVar( STRING(PacketText.Contents),"àçççè",'X').

      FOR EACH cust-corp WHERE cust-corp.inn EQ innnp NO-LOCK,
         EACH acct WHERE acct.filial-id EQ shFilial
         AND acct.cust-cat EQ 'û'
         AND acct.cust-id  EQ cust-corp.cust-id
         AND CAN-DO("30109*,405*,406*,407*,40802*,40807*,40821*,420*,421*,422*,4250*",acct.acct)
         AND CAN-DO(zAcct,acct.number)
         NO-LOCK BREAK BY cust-corp.cust-id:
         
         IF FIRST-OF(cust-corp.cust-id) THEN
         DO:
            PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyleStateRPO + '"><Data ss:Type="String">' + mStateRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeBOS + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameBOS + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyleStateBOS + '"><Data ss:Type="String">' + mStateBOS + '</Data></Cell>~n'.            
         END.
         ELSE
         DO:
            PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.            
         END.

         mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").  
         
         IF INT64(mGroup) EQ 599 OR INT64(mGroup) EQ 596 
         THEN 
         ASSIGN
            rowStyleGroup = "s78yellow"
            rowStyleAcct  = "s77yellow". 
         ELSE 
         ASSIGN
            rowStyleGroup = "s78"
            rowStyleAcct  = "s77". 
         
         IF mTypeInf EQ 'êÖòÖçèêàéëí' 
         OR 
           (mTypeInf EQ 'áÄèêéëçé' AND mVidZapr EQ "2" AND mTypZapr EQ "2") THEN 
         DO:
            IF      shFilial EQ "0000" THEN mLocalTime = 0. 
            ELSE IF shFilial EQ "0300" THEN mLocalTime = 7200.
            ELSE IF shFilial EQ "0500" THEN mLocalTime = 10800.
            
            shb = PosOnTime(acct.acct, acct.currency,DATETIME(Packet.PackDate,(Packet.PackTime + mLocalTime) * 1000)).
            
            shb1 = PosOnTime(acct.acct, acct.currency,DATETIME(Packet.PackDate,(Packet.PackTime) * 1000)).

            sprOst = ''.
            ii = 1.
            DO WHILE YES:
               IF AVAIL xPacketSprText THEN
                  x = GetFileVarEntry(STRING(xPacketSprText.Contents),"çÆ¨ëÁ",ii,'').
               ELSE x = ''.
               IF x EQ '' THEN LEAVE.
               IF x EQ acct.number THEN 
               DO:
                  IF AVAIL xPacketSprText THEN
                     sprOst = GetFileVarEntry(STRING(xPacketSprText.Contents),"é·‚†‚Æ™",ii,'').
                  ELSE sprOst = ''.
               END.
               ii = ii + 1.
            END.
            IF DEC(sprOst) NE - shb 
            THEN rowStyleOst = "s79red".
  
            ELSE rowStyleOst = "s79".
               
/*            MESSAGE                                               */
/*               "¢Î¢Æ§;"                                           */
/*               shFilial                                        ";"*/
/*               STRING(Packet.PackTime,"hh:mm:ss")              ";"*/
/*               STRING(mLocalTime,"hh:mm:ss")                   ";"*/
/*               STRING(Packet.PackTime + mLocalTime,"hh:mm:ss") ";"*/
/*               "¢ ‰†©´• " DEC(sprOst) ";"                         */
/*               "Æ·‚ ´Æ™†´ "shb         ";"                        */
/*               "Æ·‚ "shb         ";"                              */
/*                                                                  */
/*            VIEW-AS ALERT-BOX.                                    */
            
         END.
         
         PUT UNFORMATTED
         '    <Cell ss:StyleID="' + rowStyleAcct  + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + rowStyleGroup + '"><Data ss:Type="String">' + mGroup      + '</Data></Cell>~n'.

         IF mTypeInf EQ 'êÖòÖçèêàéëí' 
         OR 
           (mTypeInf EQ 'áÄèêéëçé' AND mVidZapr EQ "2" AND mTypZapr EQ "2") THEN   
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
               + TRIM(STRING( - shb,"->>>,>>>,>>>,>>9.99"))
               + '</Data></Cell>~n' + 
               '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
               + TRIM(STRING(DEC(sprOst),"->>>,>>>,>>>,>>9.99")) 
               + '</Data></Cell>~n'.
         ELSE
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle79 + '">0.00<Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle79 + '">0.00<Data ss:Type="String"></Data></Cell>~n'.
         IF FIRST-OF(cust-corp.cust-id) THEN
         DO:
            IF AVAIL xPacketOtvText2 
            THEN rowStylePB2 = "s77red".
            ELSE rowStylePB2 = "s77".
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketOtvText1   THEN GetFileStr(STRING(xPacketOtvText1.Contents),  2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwOtvText1 THEN GetFileStr(STRING(xPacketKwOtvText1.Contents),2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwSprText  THEN GetFileStr(STRING(xPacketKwSprText.Contents),2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStylePB2 + '"><Data ss:Type="String">' + (IF AVAIL xPacketOtvText2   THEN GetFileStr(STRING(xPacketOtvText2.Contents),  2,"") ELSE "") + '</Data></Cell>~n' +
               '   </Row>'
            SKIP.
         END.
         ELSE
         DO:
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '   </Row>'
            SKIP.
         END.
      END.
      
      FOR EACH person WHERE person.inn EQ innnp NO-LOCK,
         EACH acct WHERE acct.filial-id EQ shFilial
         AND acct.cust-cat EQ 'ó'
         AND acct.cust-id  EQ person.person-id
         AND CAN-DO( "30109*,405*,406*,407*,40802*,40807*,40821*,420*,421*,422*,4250*", acct.acct)
         AND CAN-DO(zAcct,acct.number)
         NO-LOCK BREAK BY person.person-id:
         
         IF FIRST-OF(person.person-id) THEN
         DO:
            PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyleStateRPO + '"><Data ss:Type="String">' + mStateRPO + '</Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeBOS + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameBOS + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyleStateBOS + '"><Data ss:Type="String">' + mStateBOS + '</Data></Cell>~n'.            
         END.
         ELSE
         DO:
            PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
         	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
            '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.            
         END.

         mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").

         IF INT64(mGroup) EQ 599 OR INT64(mGroup) EQ 596 
         THEN 
         ASSIGN
            rowStyleGroup = "s78yellow"
            rowStyleAcct  = "s77yellow". 
         ELSE 
         ASSIGN
            rowStyleGroup = "s78"
            rowStyleAcct  = "s77".
           
/*         rowAcctStyle = "s68grey".*/

         IF mTypeInf EQ 'êÖòÖçèêàéëí' 
         OR 
           (mTypeInf EQ 'áÄèêéëçé' AND mVidZapr EQ "2" AND mTypZapr EQ "2") THEN
         DO:
            shb = PosOnTime(acct.acct, acct.currency, DATETIME( Packet.PackDate, Packet.PackTime * 1000)).
            sprOst = ''.
            ii = 1.
            DO WHILE YES:
               IF AVAIL xPacketSprText THEN
                  x = GetFileVarEntry(STRING(xPacketSprText.Contents),"çÆ¨ëÁ",ii,'').
               ELSE x = ''.
               IF x EQ '' THEN LEAVE.
               IF x EQ acct.number THEN 
               DO:
                  IF AVAIL xPacketSprText THEN
                     sprOst = GetFileVarEntry(STRING(xPacketSprText.Contents),"é·‚†‚Æ™",ii,'').
                  ELSE sprOst = ''.
               END.
               ii = ii + 1.
            END.
            IF DEC(sprOst) NE - shb 
            THEN rowStyleOst = "s79red".
            ELSE rowStyleOst = "s79".
         END.
         
         PUT UNFORMATTED
         '    <Cell ss:StyleID="' + rowStyleAcct  + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + rowStyleGroup + '"><Data ss:Type="String">' + mGroup + '</Data></Cell>~n'.

         IF mTypeInf EQ 'êÖòÖçèêàéëí' 
         OR 
           (mTypeInf EQ 'áÄèêéëçé' AND mVidZapr EQ "2" AND mTypZapr EQ "2") THEN   
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
               + TRIM(STRING( - shb,"->>>,>>>,>>>,>>9.99"))
               + '</Data></Cell>~n' + 
               '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
               + TRIM(STRING(DEC(sprOst),"->>>,>>>,>>>,>>9.99")) 
               + '</Data></Cell>~n'.
         ELSE
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle79 + '">0.00<Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle79 + '">0.00<Data ss:Type="String"></Data></Cell>~n'.
         IF FIRST-OF(person.person-id) THEN
         DO:
            IF AVAIL xPacketOtvText2 
            THEN rowStylePB2 = "s77red".
            ELSE rowStylePB2 = "s77".
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketOtvText1   THEN GetFileStr(STRING(xPacketOtvText1.Contents),  2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwOtvText1 THEN GetFileStr(STRING(xPacketKwOtvText1.Contents),2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwSprText  THEN GetFileStr(STRING(xPacketKwSprText.Contents) ,2,"") ELSE "") + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStylePB2 + '"><Data ss:Type="String">' + (IF AVAIL xPacketOtvText2   THEN GetFileStr(STRING(xPacketOtvText2.Contents),  2,"") ELSE "") + '</Data></Cell>~n' +
               '   </Row>'
            SKIP.
         END.
         ELSE
         DO:
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
               '   </Row>'
            SKIP.
         END.   
      END.
   END.
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 'é‚Á•‚ ‰Æ‡¨®‡„•‚·Ô... (' + SubStr(Prc,vNum MOD 4 + 1,1) + ')' COLOR bright.
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

{preview2.i &filename="rpo-report.xml" }

RUN sndbispc ("file=" + "rpo-report.xml" + ";class=bq").

{intrface.del}          /* ÇÎ£‡„ß™† ®≠·‚‡„¨•≠‚†‡®Ô. */ 


