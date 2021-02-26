/*
               Банковская интегрированная система БИСквит
    Copyright:
     Filename: 365p-rep.p
      Comment: Контрольная ведомость сообщений 365п
   Parameters:
         Uses:
      Used by:
      Created:
     Modified: 
*/

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE vStat         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRepFileName  AS CHARACTER NO-UNDO INIT "rep365p.xml".
DEFINE VARIABLE vPackFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNum          AS INT64     NO-UNDO.
DEFINE VARIABLE vNameOrg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTypeInf      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vZapr         AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE tZapr         AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE dt            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGroup        AS CHARACTER NO-UNDO.

DEF BUFFER xPacket          FOR Packet.
DEF BUFFER xPacketOtv       FOR Packet.
DEF BUFFER xPacketKwOtv     FOR Packet.
DEF BUFFER xPacketOtvText   FOR PacketText.
DEF BUFFER xPacketKwOtvText FOR PacketText.
DEF BUFFER xPacketSpr       FOR Packet.
DEF BUFFER xPacketKwSpr     FOR Packet.
DEF BUFFER xPacketSprText   FOR PacketText.
DEF BUFFER xPacketKwSprText FOR PacketText.
DEF BUFFER xFileExchKwOtv   FOR FileExch.
DEF BUFFER xFileExchKwSpr   FOR FileExch.
DEF VAR Prc  AS CHAR INIT "~\~|~/-" NO-UNDO.
DEF VAR postcmd AS c NO-UNDO. /* д.р. PostCmd */


{globals.i}
{parsin.def}
{intrface.get prnvd}
{getdates.i}
{sh-defs.i}

{365p-rep.fun}

{debug.equ}

/*
def var gsc as i no-undo.
gsc = int(getsysconf("user-proc-id")).
find first user-proc where recid(user-proc) = gsc no-lock no-error.
if not avail(user-proc) then return.
postcmd = GetXAttrValue("user-proc",string(user-proc.public-number),"postcmd").
*/
 
ASSIGN
   vStat   = GetParamByNameAsChar(iParam,"Статус","")
   vFile   = GetParamByNameAsChar(iParam,"Файл","")
   vType   = GetParamByNameAsChar(iParam,"ТипВед","")
.
/*
RUN Insert_TTName("dateIn",STRING(beg-date,"99.99.99")).                             
RUN Insert_TTName("dateOut",STRING(end-date,"99.99.99")).                             
RUN BeginCircle_TTName ("mess").   
*/

{setdest2.i  &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="rep365p.xml" }

/*
    put unformatted 
	"Дата" format "x(9)"
	"Статус" format "x(7)"
	"Принятый файл" format "x(42)"
	"Тип" format "x(12)"
	"Наименование организации" format "x(80)" " "
	"файл ответа в ФНС" format "x(42)"
	"код ответа" format "x(11)"
	"квитанция" format "x(48)"
	"код ответа" format "x(31)"
	"справка/выписка" format "x(42)"
	"квитанция" format "x(48)"
	"код ответа" format "x(31)"
	skip.
*/

dt = ";".

/* put unformatted
"Дата"
dt "Статус"
dt "Принятый файл из ФНС"
dt "Тип сообщения"
dt "Наименование организации"
dt "ответ в ФНС"
dt "код ответа в ФНС"
dt "квитанция ФНС на ответ банка"
dt "код ответа ФНС на ответ банка"
dt "дата ответа ФНС на ответ банка"
dt "время ответа ФНС на ответ банка"
dt "справка/выписка в ФНС"
dt "квитанция ФНС на справку банка"
dt "код ответа на справку"
dt "дата ответа на справку"
dt "время ответа на справку"
   skip. */

{365p-rep.hdr}

DEF BUFFER xFileExchOtv FOR FileExch.
DEF BUFFER xPackObjectOtv FOR PackObject.

FOR EACH Packet WHERE
   Packet.Class-Code BEGINS 'PTAX'
/*   AND Packet.PacketID EQ 17837382   /*ZNO*/*/
   AND CAN-DO( "!ETaxKwt,ETAX*", Packet.Kind) 
   AND Packet.PackDate  GE beg-date 
   AND Packet.PackDate  LE end-date 
   AND Packet.filial-id EQ shFilial NO-LOCK,
   FIRST Seance WHERE
   Seance.SeanceID EQ Packet.SeanceID 
   AND Packet.State EQ "ЗАГР" 
   AND Seance.direct EQ "Импорт"
   NO-LOCK BY Packet.PackDate:

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ",STRING(Packet.PacketID)).
&ENDIF

   FIND FIRST FileExch WHERE
      FileExch.FileExchID EQ Packet.FileExchID NO-LOCK NO-ERROR.
   
   FIND FIRST PacketText OF Packet NO-LOCK NO-ERROR.
	
   RELEASE xPacket.
   RELEASE xPacketOtv.
   RELEASE xPacketOtvText.
   RELEASE xPacketSpr.
   RELEASE xPacketSprText.
   RELEASE xPacketKwOtv.
   RELEASE xPacketKwOtvText.
   RELEASE xPacketKwSpr.
   RELEASE xPacketKwSprText.
   RELEASE xFileExchKwOtv.
   RELEASE xFileExchKwSpr.
   RELEASE xFileExchOtv.

   vTypeInf = GetFileVar( STRING(PacketText.Contents),"ТипИнф",'').
   vZapr = GetFileVar( STRING(PacketText.Contents),"ВидЗапр",'').
   tZapr = GetFileVar( STRING(PacketText.Contents),"ТипЗапр",'').
   
   /* if can-do( "ПОРУЧЕНИЕНО,РЕШЕНОТМЕНА", vTypeInf) then next.*/
   /* if can-do( "РЕШЕНОТМЕНА", vTypeInf) then next.*/
   
   DEF VAR xff AS CHAR NO-UNDO.
   xff = ''.
   FOR EACH PackObject
      WHERE PackObject.PacketID = Packet.PacketID
      AND PackObject.file-name = 'Packet' NO-LOCK:
         
&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ", "PackObject.Surrogate = " + PackObject.Surrogate).
&ENDIF    
         
      FIND FIRST xPacket WHERE xPacket.PacketID = int(PackObject.Surrogate) NO-LOCK NO-ERROR.
      IF xPacket.class-code = "PTax" 
         AND (
             /* (NOT AVAIL xFileExchOtv)
              OR xFileExchOtv.name BEGINS 'PB1' */
         xff EQ '' OR xff BEGINS 'PB1'
         ) THEN 
      DO:
         FIND FIRST xPacketOtv WHERE xPacketOtv.PacketID = int(PackObject.Surrogate) NO-LOCK.
         FIND FIRST xPacketOtvText OF xPacketOtv NO-LOCK NO-ERROR.
         FIND FIRST xFileExchOtv
            WHERE xFileExchOtv.FileExchID EQ xPacketOtv.FileExchID NO-LOCK NO-ERROR.
         xff = GetXAttrValueEx("Packet", STRING(xPacketOtv.PacketID), "FileName", "").
         FIND LAST xPackObjectOtv WHERE xPackObjectOtv.Surrogate = STRING(xPacketOtv.PacketID) AND
            xPackObjectOtv.Kind = "TTaxKWT" AND
            xPackObjectOtv.file-name = "Packet" NO-LOCK NO-ERROR.
         FIND FIRST xPacketKwOtv
            WHERE xPacketKwOtv.PacketID = xPackObjectOtv.PacketID NO-LOCK NO-ERROR.
         FIND FIRST xPacketKwOtvText OF xPacketKwOtv NO-LOCK NO-ERROR.
         FIND FIRST xFileExchKwOtv
            WHERE xFileExchKwOtv.FileExchID EQ xPacketKwOtv.FileExchID NO-LOCK NO-ERROR.
      END.
      IF xPacket.class-code = "PTaxRSSprav" THEN
         FIND FIRST xPacketSpr WHERE xPacketSpr.PacketID = int(PackObject.Surrogate) NO-LOCK.
   END.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ", "xPacketSpr = " + STRING(AVAIL(xPacketSpr))).
&ENDIF   

   IF AVAIL xPacketSpr THEN
   DO:
&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ", "xPacketSpr.PacketID = " + STRING(xPacketSpr.PacketID)).
&ENDIF      
      FIND FIRST xPacketSprText OF xPacketSpr NO-LOCK NO-ERROR.
      FIND LAST PackObject WHERE
         PackObject.Surrogate = STRING(xPacketSpr.PacketID)
         AND PackObject.Kind = "TTaxKWT"
         AND PackObject.file-name = "Packet"
         NO-LOCK NO-ERROR.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ", "PackObject = " + STRING(AVAIL(PackObject))).
&ENDIF
      
      FIND FIRST xPacketKwSpr WHERE
         xPacketKwSpr.PacketID = PackObject.PacketID
         NO-LOCK NO-ERROR.
      
      FIND FIRST xPacketKwSprText OF xPacketKwSpr NO-LOCK NO-ERROR.
      
      FIND FIRST xFileExchKwSpr WHERE
         xFileExchKwSpr.FileExchID EQ xPacketKwSpr.FileExchID
         NO-LOCK NO-ERROR.
   END.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ","xPacketSprText = " + STRING(AVAIL(xPacketSprText))).
&ENDIF

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ","xPacketKwSpr = " + STRING(AVAIL(xPacketKwSpr))).
&ENDIF

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("365p-rep ","xFileExchKwSpr = " + STRING(AVAIL(xFileExchKwSpr))).
&ENDIF
   
   vPackFileName = GetXAttrValueEx("Packet", STRING(Packet.PacketID), "FileName", "").
   IF AVAILABLE(FileExch) AND CAN-DO("SBF*",FileExch.Name) THEN NEXT.
   
   vNum = vNum + 1.                

   vNameOrg = GetFileVar( STRING(PacketText.Contents),"НаимНП",'-').
   IF vNameOrg = '-' THEN
      vNameOrg = GetFileVar( STRING(PacketText.Contents),"ФИОИП",'-').
   IF vTypeInf = "ПОРУЧЕНИЕНО" THEN
      vNameOrg = GetFileVar( STRING(PacketText.Contents),"Плательщ",'-').

   DEF VAR rowStyle AS CHAR NO-UNDO.
   rowStyle = "s68".
   IF AVAIL xPacketOtv AND xPacketOtv.State NE "СКВТ" THEN rowStyle = "s68wait".
   /* IF not AVAIL xPacketOtv OR xPacketOtv.State NE "СОЗД"  THEN rowStyle = "s68error". */
   IF AVAIL xPacketOtv AND xPacketOtv.State EQ "ОШБК" THEN rowStyle = "s68error".

   IF CAN-DO( "ЗАПРОСНО,РЕШЕНПРИОСТ", vTypeInf) THEN 
   DO:
      IF AVAIL xPacketSpr AND xPacketSpr.State NE "СКВТ" THEN rowStyle = "s68wait".
      IF AVAIL xPacketSpr AND xPacketSpr.State EQ "ОШБК" THEN rowStyle = "s68error".
   END.

   /* надо перебрать все счета запроса */
   DEF VAR zAcct AS CHAR NO-UNDO.
   zAcct = ''.
   IF (CAN-DO( "ЗАПРОСНО", vTypeInf) 
      AND (vZapr EQ '2' OR vZapr EQ '3') /* запрос выписки по операциям на счете */
      )
      OR (vTypeInf EQ 'РЕШЕНПРИОСТ') THEN 
   DO:
      /*DEF VAR tZapr AS CHAR NO-UNDO INIT ''.                        */
      /*tZapr = GetFileVar( STRING(PacketText.Contents),"ТипЗапр",'').*/
      IF tZapr EQ '1' THEN zAcct = '*'.
      IF tZapr EQ '2' OR (vTypeInf EQ 'РЕШЕНПРИОСТ') THEN 
      DO:
         DEF VAR ii AS INT NO-UNDO.
         ii = 1.
         DO WHILE YES:
            DEF VAR x AS CHAR NO-UNDO.
            x = GetFileVarEntry(STRING(PacketText.Contents),"НомСч",ii,'').
            IF x EQ '' THEN LEAVE.
            zAcct = (IF LENGTH(zAcct) > 0 THEN zAcct + ',' ELSE '') + x.
            ii = ii + 1.
         END.
      END.
   END.
   
   {find-act.i
      &acct = zAcct
   }
   
   IF AVAIL(acct) THEN
   DO:
      mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").
   END.
   
   IF NOT (AVAILABLE(FileExch) AND FileExch.Name BEGINS 'IZVTUB_AFN') THEN
      PUT UNFORMATTED
      '<Row ss:AutoFitHeight="1">
		 <Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + STRING(Packet.PackDate,"99.99.99") + '</Data></Cell>~n
		 <Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + Packet.State + '</Data></Cell>~n
		 <Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAILABLE(FileExch)	THEN FileExch.Name ELSE "") + '</Data></Cell>~n
		 <Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL(PacketText) THEN vTypeInf  ELSE "") + '</Data></Cell>~n'
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + replace( vNameOrg, '"', '""') + '</Data></Cell>~n'
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + zAcct  + '</Data></Cell>~n' +
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + mGroup + '</Data></Cell>~n' +
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + GetFileVar( STRING(PacketText.Contents),"НомРешПр",'')  + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + GetFileVar( STRING(PacketText.Contents),"ДатаРешПр",'') + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + GetFileVar( STRING(PacketText.Contents),"СумВзыск",GetFileVar( STRING(PacketText.Contents),"ВидРеш",'')) + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + '</Data>"12"</Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketSprText THEN REPLACE(Substring(xPacketSprText.Contents,1,100),">",REPLACE(Substring(xPacketSprText.Contents,1,100),">","")) ELSE "") + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + GetFileVar( STRING(PacketText.Contents),"НомРешОт",'') + '</Data></Cell>~n' +  
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + GetFileVar( STRING(PacketText.Contents),"ДатаРешОт",'') + '</Data></Cell>~n' +
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketOtvText THEN GetFileStr( STRING(xPacketOtvText.Contents), 2, "") ELSE "") + '</Data></Cell>~n' +
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwOtvText THEN GetFileStr( STRING(xPacketKwOtvText.Contents), 2, "") ELSE "") + '</Data></Cell>~n' +
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwOtvText THEN GetFileStr( STRING(xPacketKwOtvText.Contents), 3, "") ELSE "") + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwOtvText THEN GetFileStr( STRING(xPacketKwOtvText.Contents), 4, "") ELSE "") + '</Data></Cell>~n' + 
      '<Cell ss:StyleID="' +
         (IF AVAIL xPacketSpr AND CAN-DO("ОТПР,СКВТ", xPacketSpr.State) THEN 's68' ELSE 's68red') +
         '"><Data ss:Type="String">' + (IF AVAIL xPacketSpr THEN GetXAttrValueEx("Packet", STRING(xPacketSpr.PacketID), "FileName", "?") ELSE "") + '</Data></Cell>~n' + 
/*код ответа на справку*/   '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwSprText THEN GetFileStr(STRING(xPacketKwSprText.Contents), 2, "") ELSE "") + '</Data></Cell>~n' + 
/*дата ответа на справку*/  '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwSprText THEN GetFileStr(STRING(xPacketKwSprText.Contents), 3, "") ELSE "") + '</Data></Cell>~n' + 
/*время ответа на справку*/ '<Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + (IF AVAIL xPacketKwSprText THEN GetFileStr(STRING(xPacketKwSprText.Contents), 4, "") ELSE "") + '</Data></Cell>~n</Row>~n' 
    SKIP.
   IF zAcct NE '' THEN
   DO:
      DEF VAR innnp AS CHAR NO-UNDO.
      DEF VAR rowAcctStyle AS CHAR NO-UNDO.
      innnp = GetFileVar( STRING(PacketText.Contents),"ИНННП",'X').

      FOR EACH cust-corp WHERE cust-corp.inn EQ innnp NO-LOCK,
         EACH acct WHERE acct.filial-id EQ shFilial
         AND acct.cust-cat EQ 'Ю'
         AND acct.cust-id  EQ cust-corp.cust-id
         AND CAN-DO( "30109*,405*,406*,407*,40802*,40807*,40821*,420*,421*,422*,4250*", acct.acct)
         AND CAN-DO( zAcct, acct.number)
         NO-LOCK:
         DEF VAR shb AS DECIMAL NO-UNDO.
         FIND FIRST blockobject
            WHERE blockobject.class-code EQ 'BlockAcct'
            AND blockobject.file-name EQ 'acct'
            AND blockobject.surrogate EQ acct.acct + ',' + acct.currency 
            AND blockobject.txt[2] EQ 'ИМНС'
            AND blockobject.txt[3] EQ 'Решение'
            AND blockobject.txt[4] EQ GetFileVar( STRING(PacketText.Contents),"НомРешПр",'-') + ';' + GetFileVar( STRING(PacketText.Contents),"КодНО",'-')
            AND blockobject.txt[8] EQ 'Импорт'   
            AND blockobject.val[4] EQ - DEC(GetFileVar( STRING(PacketText.Contents),"СумВзыск",'0'))
            NO-LOCK NO-ERROR.

         mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").  
         
         rowAcctStyle = "s68grey".
         /*IF vTypeInf EQ 'РЕШЕНПРИОСТ' THEN*/
         IF vTypeInf EQ 'РЕШЕНПРИОСТ' 
         OR 
           (vTypeInf EQ 'ЗАПРОСНО' AND vZapr EQ "2" AND tZapr EQ "2") THEN 
         DO:
            /*run acct-pos in h_base(acct.acct,acct.currency,Packet.PackDate,Packet.PackDate, ?).*/
            DEFINE VARIABLE mLocalTime AS INT64 NO-UNDO.
            IF      shFilial EQ "0000" THEN mLocalTime = 0. 
            ELSE IF shFilial EQ "0300" THEN mLocalTime = 7200.
            ELSE IF shFilial EQ "0500" THEN mLocalTime = 10800.
            shb = PosOnTime(acct.acct, acct.currency, DATETIME(Packet.PackDate,(Packet.PackTime + mLocalTime) * 1000)).
            
            /*MESSAGE shb ";" STRING(DATETIME(Packet.PackDate,(Packet.PackTime + 7200) * 1000),"99/99/99 HH:MM:SS")*/
            /*VIEW-AS ALERT-BOX.                                                                                   */
            DEF VAR sprOst AS CHAR NO-UNDO.
            sprOst = ''.
            ii = 1.
            DO WHILE YES:
               /* DEF VAR x AS CHAR NO-UNDO. */
               IF AVAIL xPacketSprText THEN
                  x = GetFileVarEntry(STRING(xPacketSprText.Contents),"НомСч",ii,'').
               ELSE x = ''.
               IF x EQ '' THEN LEAVE.
               IF x EQ acct.number THEN 
               DO:
                  IF AVAIL xPacketSprText THEN
                     sprOst = GetFileVarEntry(STRING(xPacketSprText.Contents),"Остаток",ii,'').
                  ELSE sprOst = ''.
               END.
               /* zAcct = (IF LENGTH(zAcct) > 0 THEN zAcct + ',' ELSE '') + x. */
               ii = ii + 1.
            END.
            IF DEC(sprOst) NE - shb /*IF acct.currency EQ "" THEN abs(sh-bal) ELSE abs(sh-val)*/ 
               THEN rowAcctStyle = "s68red".
         END.
         PUT UNFORMATTED
         '<Row ss:AutoFitHeight="0">
          <Cell ss:MergeAcross="4" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + mGroup + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + (IF AVAIL blockobject AND blockobject.beg-datetime NE ? THEN STRING(blockobject.beg-datetime) ELSE '') + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + (IF AVAIL blockobject AND blockobject.end-datetime NE ? THEN STRING(blockobject.end-datetime) ELSE '') + '</Data></Cell>~n
          <Cell ss:MergeAcross="2" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n'
            .
         /*IF vTypeInf EQ 'РЕШЕНПРИОСТ' THEN*/
         IF vTypeInf EQ 'РЕШЕНПРИОСТ' 
         OR 
           (vTypeInf EQ 'ЗАПРОСНО' AND vZapr EQ "2" AND tZapr EQ "2") THEN   
            PUT UNFORMATTED

               '<Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="Number">' 
               /* + STRING(IF acct.currency EQ "" THEN abs(sh-bal) ELSE abs(sh-val)) */
               + STRING( - shb)
               + '</Data></Cell>~n
           <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="Number">' + sprOst + '</Data></Cell>~n'
               .
         ELSE
            PUT UNFORMATTED
               '<Cell ss:MergeAcross="1" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + '</Data></Cell>~n'
               .
         PUT UNFORMATTED
            '<Cell ss:MergeAcross="9" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n
          </Row>'
            SKIP.
      END.

      FOR EACH person WHERE person.inn EQ innnp NO-LOCK,
         EACH acct WHERE acct.filial-id EQ shFilial
         AND acct.cust-cat EQ 'Ч'
         AND acct.cust-id  EQ person.person-id
         AND CAN-DO( "30109*,405*,406*,407*,40802*,40807*,40821*,420*,421*,422*,4250*", acct.acct)
         AND CAN-DO( zAcct, acct.number)
         NO-LOCK:
         /* DEF VAR shb AS DECIMAL NO-UNDO. */
         FIND FIRST blockobject
            WHERE blockobject.class-code EQ 'BlockAcct'
            AND blockobject.file-name EQ 'acct'
            AND blockobject.surrogate EQ acct.acct + ',' + acct.currency 
            AND blockobject.txt[2] EQ 'ИМНС'
            AND blockobject.txt[3] EQ 'Решение'
            AND blockobject.txt[4] EQ GetFileVar( STRING(PacketText.Contents),"НомРешПр",'-') + ';' + GetFileVar( STRING(PacketText.Contents),"КодНО",'-')
            AND blockobject.txt[8] EQ 'Импорт'
            AND blockobject.val[4] EQ - DEC(GetFileVar( STRING(PacketText.Contents),"СумВзыск",'0'))
            NO-LOCK NO-ERROR.

         mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").  
  
         rowAcctStyle = "s68grey".
         /*IF vTypeInf EQ 'РЕШЕНПРИОСТ' THEN*/ 
         IF vTypeInf EQ 'РЕШЕНПРИОСТ' 
         OR 
           (vTypeInf EQ 'ЗАПРОСНО' AND vZapr EQ "2" AND tZapr EQ "2") THEN
         DO:
            /* run acct-pos in h_base(acct.acct,acct.currency,Packet.PackDate,Packet.PackDate, ?).  */
            shb = PosonTime(acct.acct, acct.currency, DATETIME( Packet.PackDate, Packet.PackTime * 1000)).
            /* DEF VAR sprOst AS CHAR NO-UNDO. */
            sprOst = ''.
            ii = 1.
            DO WHILE YES:
               /* DEF VAR x AS CHAR NO-UNDO. */
               IF AVAIL xPacketSprText THEN
                  x = GetFileVarEntry(STRING(xPacketSprText.Contents),"НомСч",ii,'').
               ELSE x = ''.
               IF x EQ '' THEN LEAVE.
               IF x EQ acct.number THEN 
               DO:
                  IF AVAIL xPacketSprText THEN
                     sprOst = GetFileVarEntry(STRING(xPacketSprText.Contents),"Остаток",ii,'').
                  ELSE sprOst = ''.
               END.
               /* zAcct = (IF LENGTH(zAcct) > 0 THEN zAcct + ',' ELSE '') + x. */
               ii = ii + 1.
            END.
            IF DEC(sprOst) NE /* IF acct.currency EQ "" THEN abs(sh-bal) ELSE abs(sh-val)  */ - shb
               THEN rowAcctStyle = "s68red".
         END.
         PUT UNFORMATTED
            '<Row ss:AutoFitHeight="0">
          <Cell ss:MergeAcross="4" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowStyle + '"><Data ss:Type="String">' + mGroup + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + (IF AVAIL blockobject AND blockobject.beg-datetime NE ? THEN STRING(blockobject.beg-datetime) ELSE '') + '</Data></Cell>~n
          <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">' + (IF AVAIL blockobject AND blockobject.end-datetime NE ? THEN STRING(blockobject.end-datetime) ELSE '') + '</Data></Cell>~n
          <Cell ss:MergeAcross="2" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n'
            .
         /*IF vTypeInf EQ 'РЕШЕНПРИОСТ' THEN*/
         IF vTypeInf EQ 'РЕШЕНПРИОСТ' 
            OR 
           (vTypeInf EQ 'ЗАПРОСНО' AND vZapr EQ "2" AND tZapr EQ "2") THEN
            PUT UNFORMATTED
               '<Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="Number">' 
               /* + STRING(IF acct.currency EQ "" THEN abs(sh-bal) ELSE abs(sh-val)) */
               + STRING( - shb)
               + '</Data></Cell>~n
           <Cell ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="Number">' + sprOst + '</Data></Cell>~n'
               .
         ELSE 
            PUT UNFORMATTED
               '<Cell ss:MergeAcross="1" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n'
               .
         PUT UNFORMATTED
            '<Cell ss:MergeAcross="9" ss:StyleID="' + rowAcctStyle + '"><Data ss:Type="String">'  + '</Data></Cell>~n
          </Row>'
            SKIP.
      END.
   END.
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 COL 1 'Минуточку... (' + SubStr(Prc,vNum MOD 4 + 1,1) + ')' COLOR bright.
/*{justasec}*/
END.
PUT /* stream vvs */ UNFORMATTED
   '
  </Table>\n
  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">
   <PageSetup>
    <Layout x:Orientation="Landscape"/>
   </PageSetup>
   <Unsynced/>
   <FitToPage/>
   <Selected/>
  </WorksheetOptions>
 </Worksheet>\n
</Workbook>\n
'.

{preview2.i &filename="rep365p.xml" }

RUN sndbispc ("file=" + "rep365p.xml" + ";class=bq").

{intrface.del}          /* Выгрузка инструментария. */ 
