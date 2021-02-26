DEFINE STREAM sDir.

{chckbnkcos.fun}

PROCEDURE ESIDExportED503 :
   DEFINE INPUT PARAMETER iClass AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER hIns   AS HANDLE    NO-UNDO.

   DEFINE VARIABLE vFlagSet   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VARIABLE vHExch     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vHDoc      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vFormat    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vInPath    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vInMask    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFName     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFPath     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFType     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vArchDir   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vArchPath  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRef       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBICRec    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUISRec    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vType      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAmount    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vErrLst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vEndDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vChoice    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vWarnings  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBadErrors AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSizeLimit AS INT64     NO-UNDO.
   DEFINE VARIABLE vSize      AS INT64     NO-UNDO.
   DEFINE VARIABLE vDocTmp    AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE vPropDoc   AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE vHQry      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vCurBIC    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOK        AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vHTrans    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vPos       AS INT64     NO-UNDO.
   DEFINE VARIABLE vTmpFile   AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE vSenderBIC  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSenderBICf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vService   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTail      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTailPresent AS LOGICAL NO-UNDO.

   DEFINE BUFFER Seance    FOR Seance.
   DEFINE BUFFER mail-user FOR mail-user.
   DEFINE BUFFER code      FOR code.
   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bCode50x  FOR code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   /* получение каталога */
   ASSIGN
      vHExch    = hIns:DEFAULT-BUFFER-HANDLE
      vSeanceID = vHExch::SeanceID
      vFormat   = vHExch::mail-format
      vHDoc     = ObjectValueHandle("ExchRKCSw")
      vHTrans   = GetBaseProcedure()
      vErrLst   = ""
   NO-ERROR. {&ON-ERROR}

   vSizeLimit = INT64 (fGetSetting("УФЭБС","ED503Limit","15000")) NO-ERROR.
   vTail      = fGetSetting("УФЭБС","ED503Tail","") NO-ERROR.
   IF NOT EXCH-MSGBuff(vFormat, BUFFER bCode) THEN 
      UNDO MAIN, RETRY MAIN.

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID 
      NO-LOCK NO-ERROR. {&ON-ERROR}
   FIND FIRST mail-user WHERE mail-user.op-kind-exp EQ Seance.op-kind
      NO-LOCK NO-ERROR. {&ON-ERROR}
   vHExch::mail-user-num = mail-user.mail-user-num.

   IF NOT CatalogGetBoth(mail-user.mail-user-num,
                         INPUT  "Импорт",
                         OUTPUT vInPath,
                         OUTPUT vInmask)
      THEN UNDO MAIN, RETRY MAIN.

   vArchDir = CatalogGetPath (mail-user.mail-user-num,"ImpArch","Path").

   /* чтение списка файлов из каталога и заполнение временной таблицы */
   file-info:file-name = vInPath NO-ERROR.
   INPUT STREAM sDIR FROM os-dir(vInPath).
   REPEAT:
      IMPORT STREAM sDIR vFName vFPath vFType.
      IF vFType NE "F" THEN NEXT.
      IF NOT CAN-DO(vInMask,vFName) THEN NEXT.

      /* получение BIC получателя, референса сообщения, суммы            */
      RUN GetSWInfo IN THIS-PROCEDURE (vFPath,
                                       OUTPUT vRef,
                                       OUTPUT vBICRec,
                                       OUTPUT vType,
                                       OUTPUT vAmount,
                                       OUTPUT vTailPresent,
                                       OUTPUT vSenderBIC,
                                       INPUT-OUTPUT vErrLst) NO-ERROR.

      RUN InstanceCreate IN h_exch (?, vHDoc).
      ASSIGN
         FILE-INFO:FILE-NAME  = vFPath
         vHDoc::FPath         = FILE-INFO:FULL-PATHNAME
         vHDoc::FName         = vFName
         vHDoc::FSize         = IF {assigned vTail} AND 
                                   NOT vTailPresent THEN FILE-INFO:FILE-SIZE + LENGTH(vTail)
                                                    ELSE FILE-INFO:FILE-SIZE 
         vHDoc::Ref           = vRef
         vHDoc::bank-code-rec = vBICRec
         vHDoc::DType         = vType
         vHDoc::amt-rub       = vAmount
      NO-ERROR.

      IF {assigned vRef} AND 
         NOT CAN-DO("300,320,399,600",vType) THEN DO:
         FIND FIRST op-impexp where op-impexp.bank-reference EQ vRef
            NO-LOCK NO-ERROR.
         IF AVAIL op-impexp THEN
            FIND FIRST op WHERE op.op EQ op-impexp.op NO-LOCK NO-ERROR.
         IF AVAIL op THEN
            ASSIGN
               vHDoc::op        = op.op
               vHDoc::doc-num   = op.doc-num
               vHDoc::File-Name = "op-entry"
               vHDoc::Surrogate = STRING(op.op) + ",1" 
            NO-ERROR.
      END.

      ASSIGN
         vUISRec = ""
         vSenderBICf = vSenderBIC
      .

      RUN GetClearBIC(vSenderBIC, OUTPUT vSenderBIC).
      RUN GetClearBIC(bank-bic, OUTPUT vCurBIC).

      vCurBIC = SUBSTR(vCurBIC, 1, 8) + SUBSTR(vCurBIC, 10).

      IF vSenderBIC NE vCurBIC THEN DO:
         {additem.i vErrLst '"u2880"'}
      END.
      vCurBIC = "". 


      IF vHDoc::FSize GT vSizeLimit THEN DO:
         {additem.i vErrLst '"b8201"'}
      END.

      RUN GetUIS-COS IN THIS-PROCEDURE (vHDoc::bank-code-rec, BUFFER code, BUFFER bCode50x).
      IF NOT AVAIL code OR 
         NOT AVAIL bCode50x 
         THEN DO:
         {additem.i vErrLst '"b8211"'}
      END.
      ELSE DO:
         ASSIGN
            vBegDate = DATE(bCode50x.Description[1])
            vEndDate = DATE(bCode50x.Description[2])
            vUISRec  = code.code
         NO-ERROR.
         IF  (vBegDate NE ? AND mOpDate LT vBegDate) OR
             (vEndDate NE ? AND mOpDate GE vEndDate) THEN DO:
            {additem.i vErrLst '"b8212"'}
         END.
         IF vHDoc::bank-code-rec NE code.val THEN DO:
            {additem.i vErrLst '"u2878"'}
         END.
      END.

      RUN GetUIS-COS IN THIS-PROCEDURE (vSenderBICf, BUFFER code, BUFFER bCode50x).
      IF NOT AVAIL code OR
         NOT AVAIL bCode50x 
         THEN DO:
         {additem.i vErrLst '"b8221"'}
      END.
      ELSE DO:
         ASSIGN 
            vBegDate    = DATE(bCode50x.Description[1])
            vEndDate    = DATE(bCode50x.Description[2])
            vService    = bCode50x.name
            vSenderBIC  = code.val 
         NO-ERROR.

         IF ((vBegDate NE ? AND mOpDate LT vBegDate) OR
            (vEndDate NE ? AND mOpDate GE vEndDate))  OR
            vService NE "1"
            THEN DO:
            {additem.i vErrLst '"b8222"'}
         END.
          
         IF vSenderBICf NE vSenderBIC THEN DO:
            {additem.i vErrLst '"u2879"'}
         END.
      END.

      IF IsStopCOSExch(vUISRec) THEN DO:
         {additem.i vErrLst '"rbn25"'}
      END.
      IF IsStopCOSExch(vHExch::SendID) THEN DO:
         {additem.i vErrLst '"sbn21"'}
      END.

      RUN CheckErrorInst IN h_exch (vHDoc::ErrorClass,
                                    vErrLst,
                                    OUTPUT vWarnings,
                                    OUTPUT vBadErrors).

      ASSIGN
        vHDoc::UIS       = vUISRec
        vHDoc::ErrorList = vErrLst
        vHDoc::BadErrors = vBadErrors
      NO-ERROR.

   END.
   INPUT STREAM sDir CLOSE.

   vOK = vHDoc:FIND-FIRST("where __ID GT 0 ", NO-LOCK) NO-ERROR.
   IF NOT vOK then do:
      RUN Fill-SysMes("","","-1", "Не найдено ни одного файла для отправки.").
      RUN SetBreak IN vHTrans (1) NO-ERROR.
      vFlagSet = YES.
      LEAVE MAIN.
   END.

   RUN ed503exp-rep.p (vHDoc:TABLE-HANDLE) NO-ERROR.

   vOK = vHDoc:FIND-FIRST("where __ID gt 0 and BadErrors eq '' ", NO-LOCK) 
         NO-ERROR.
   IF NOT vOK THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Отсутствуют сообщения " + 
                                  "SWIFT, пригодные для экспорта.").
      RUN SetBreak IN vHTrans (1) NO-ERROR.
      vFlagSet = YES.
      LEAVE MAIN.
   END.
   ELSE DO:
      vChoice = YES.
      MESSAGE COLOR NORMAL
         "Выполнить создание сообщений?" SKIP
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
         SET vChoice.
      IF NOT vChoice THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "Транзакция прервана " + 
                                     "пользователем.").
         RUN SetBreak IN vHTrans (1) NO-ERROR.
         vFlagSet = YES.
         LEAVE MAIN.
      END.
   END.
                                       
   /* проход по временной таблице, отсортированной по BIC получателя  */
   CREATE QUERY vHQry.
   vHQry:ADD-BUFFER(vHDoc).
   vHQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &1.__ID GT 0 " + 
                                  "AND &1.BadErrors EQ '' " +
                                  "EXCLUSIVE-LOCK BY &1.bank-code-rec ",
                                   vHDoc:NAME)).
   vHQry:QUERY-OPEN().
   vHQry:GET-FIRST().
   ASSIGN
      vSize   = 0
      vCurBIC = "".
   DO WHILE NOT vHQry:QUERY-OFF-END :
      IF vHDoc::bank-code-rec NE vCurBIC OR 
        (vSize + vHDoc::FSize GT vSizeLimit) THEN DO:
         vSize = 0.
         /* создание основной ТФ         */
         RUN InstanceCreate IN h_exch (?, vHExch).
         /* заполнение ТФ данными */
         RUN ESIDExportREF IN THIS-PROCEDURE (vHExch,
                                              BUFFER bCode).
         ASSIGN
            vHExch::RecvID         = vHDoc::UIS
            vHExch::bank-code-send = vSenderBIC
            vHExch::bank-code-rec  = vHDoc::bank-code-rec
            vHExch::TotalCnt       = 0
         .   
      END.

      /* привязка вложенной ТФ */
      ASSIGN
         vHDoc::__UpID   = vHExch::__ID
         vHDoc::doc-type = "MT" + vHDoc::DType
         vHDoc::SendREF  = STRING(GetCounterNextValue("RKC503Sesn",
                                                          mOpDate),
                                     "9999") +
                              STRING(GetCounterNextValue("RKC503Seq",
                                                          mOpDate),
                                     "999999")
      NO-ERROR. {&ON-ERROR}

       
      IF NOT {assigned vHDoc::SendREF} THEN DO:
         {additem.i vErrLst '"u2877"'}
      END.
                           
      /* чтение файла */
      COPY-LOB FROM FILE vHDoc::FPath TO vTmpFile NO-CONVERT NO-ERROR. {&ON-ERROR}

      vPos = INDEX(STRING(vTmpFile), "\{5:\{").
      IF {assigned vTail} AND vPos EQ 0 THEN vTmpFile = TRIM(vTmpFile) + vTail.
      
      vPos = INDEX(string(vTmpFile), "~{1:").
      IF vPos GT 0 THEN DO:
         ASSIGN
            vPos  = INDEX(vTmpFile, "}")
            OVERLAY(vTmpFile, vPos - 10, 10) = vHDoc::SendREF
         NO-ERROR.
      END.

      /* кодирование файла */
      COPY-LOB FROM vTmpFile TO vDocTmp NO-CONVERT NO-ERROR. {&ON-ERROR}
      vPropDoc = BASE64-ENCODE (vDocTmp).
      SET-SIZE (vDocTmp) = 0.

      vHDoc::details = STRING(vPropDoc) NO-ERROR. {&ON-ERROR}

      ASSIGN
         vSize             = vSize + vHDoc::FSize
         vHExch::TotalCnt  = vHExch::TotalCnt + 1.

      /* сохранение списка файлов для данного сообщения */
      {additem3.i vHExch::FileList vHDoc::FName 1}

      /* перенос файлов в архивный каталог */
      IF {assigned vArchDir} THEN DO:
         vArchPath = MakeFileName (vArchDir,vHDoc::FName).

         OS-COPY VALUE(vHDoc::FPath) VALUE(vArchPath).
         IF OS-ERROR NE 0 THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
                     SUBSTITUTE ("Невозможно скопировать файл &1 в " + 
                                 "архивный каталог &2. Код ошибки: &3",
                                 vHDoc::FPath, vArchPath, STRING(OS-ERROR))).
         END.
         ELSE DO:
         OS-DELETE VALUE(vHDoc::FPath).
         IF OS-ERROR NE 0 THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
                     SUBSTITUTE ("Невозможно удалить файл &1 " + 
                                 "Код ошибки: &2",
                                 vHDoc::FPath, STRING(OS-ERROR))).
         END.
         END.
      END.
      vCurBIC = vHDoc::bank-code-rec.
      vHQry:GET-NEXT().
   END.
   vHQry:QUERY-CLOSE().
   DELETE OBJECT vHQry NO-ERROR.


   vFlagSet = YES.

END. /* MAIN */

{doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/

PROCEDURE ESIDImportSWIFTCont:
   DEFINE INPUT PARAMETER iFormat AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER hIns    AS HANDLE    NO-UNDO.

   DEFINE VARIABLE vFlagSet   AS LOGICAL   INIT ? NO-UNDO.
   DEFINE VARIABLE vHExch     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vType      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRef       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBICRec    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAmount    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vOutPath   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOutMask   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFName     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPropDoc   AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE vDocTmp    AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE vSenderBIC AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTailPresent AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vErrLst    AS CHARACTER NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER Packet    FOR Packet.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      vHExch    = hIns:DEFAULT-BUFFER-HANDLE
      vType     = SUBSTR(vHExch::doc-type, 3)
      vPropDoc  = vHExch::details
   NO-ERROR. {&ON-ERROR}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   /* основное правило обмена */

   /* Глухов. Оптимизация для оракла */
   FOR EACH Packet WHERE Packet.SeanceID   EQ vHExch::SeanceID
                       AND Packet.ParentID   EQ 0
                       AND Packet.FileExchID GT 0
   NO-LOCK query-tuning(NO-INDEX-HINT):
      LEAVE.
   END. 
   IF NOT AVAIL Packet THEN UNDO MAIN, RETRY MAIN.


   IF NOT CatalogGetBoth(Packet.mail-user-num,
                         "Экспорт",
                         OUTPUT vOutPath,
                         OUTPUT vOutMask)
      THEN UNDO MAIN, RETRY MAIN.

   /* правило обмена для SWIFTContainer*/
   GetEXCHRule (vHExch:buffer-field("SeanceID"):buffer-value,
                {&DIR-IMPORT},
                CHR(1) + iFormat,
                BUFFER mail-user).
   IF NOT AVAIL mail-user THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не найдено правило обмена. "+
                                  "Невозможно сохранить файл.").
      UNDO MAIN, RETRY MAIN.
   END.

   RUN PacketCreate IN h_pack (vHExch::SeanceID,
                               -1,
                               mail-user.mail-user-num,
                               vHExch:NAME,
                               OUTPUT vPacketID) NO-ERROR. {&ON-ERROR}

   vFName   = "SW" + vType + "-" + STRING(vPacketID, "999999999") + ".txt".
   vOutPath = RIGHT-TRIM(vOutPath, "/") + "/" + vFName.
   vDocTmp  = BASE64-DECODE(vPropDoc) NO-ERROR.

   COPY-LOB FROM vDocTmp TO FILE vOutPath NO-CONVERT NO-ERROR.
  
   /* получение BIC получателя, референса сообщения, суммы            */
   RUN GetSWInfo IN THIS-PROCEDURE (vOutPath,
                                    OUTPUT vRef,
                                    OUTPUT vBICRec,
                                    OUTPUT vType,
                                    OUTPUT vAmount,
                                    OUTPUT vTailPresent,
                                    OUTPUT vSenderBIC,
                                    INPUT-OUTPUT vErrLst) NO-ERROR.

   FIND FIRST Packet WHERE Packet.PacketID EQ vPacketID 
      NO-LOCK NO-ERROR. {&ON-ERROR}
   UpdateSigns  (Packet.Class-Code, STRING(vPacketID), "SWType", vType, ?).
   UpdateSigns  (Packet.Class-Code, STRING(vPacketID), "SWFileName", vFName, ?).
   UpdateSigns  (Packet.Class-Code, STRING(vPacketID), "SWRef", vRef, ?).
   UpdateSigns  (Packet.Class-Code, STRING(vPacketID), "SWAmt", STRING(vAmount), ?).

   ASSIGN
      vHExch::doc-type       = ""
      vHExch::bank-code-send = ""
      vHExch::details        = ""
   NO-ERROR.

   vFlagSet = YES.

END. /* MAIN */

{doreturn.i vFlagSet}

END PROCEDURE.

PROCEDURE ESIDImportED503 :
   DEFINE INPUT PARAMETER iFormat AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER hExch   AS HANDLE    NO-UNDO.

   DEFINE VARIABLE vFlagSet   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VARIABLE vHExch     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vHMain     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vParentID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vBICSend   AS CHARACTER NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER Packet    FOR Packet.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}


   IF NOT EXCH-MSGBuff(iFormat, BUFFER bCode) THEN 
      UNDO MAIN, RETRY MAIN.

   RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                          OUTPUT       vHMain).

   RUN ESIDPacketCreate  (INPUT  hExch,
                          INPUT  vHMain,
                          BUFFER bCode,
                          OUTPUT vPacketID,
                          OUTPUT vParentID).

   vBICSend  = hExch::bank-code-send.
   FIND FIRST Packet WHERE Packet.PacketID EQ vPacketID 
      NO-LOCK NO-ERROR. {&ON-ERROR}

   UpdateSigns  (Packet.Class-Code, STRING(vPacketID), "SWBICSend", vBICSend, ?).

   RUN PacketRKCTextSave(vPacketID).

   RUN PacketInclude  (vPacketID,
                       vParentID,
                      {&STATE-FIN}).

   /* собираем вложенные пакеты-контейнеры */
   FOR EACH Packet WHERE Packet.SeanceID EQ hExch::SeanceID
                     AND Packet.ParentID EQ 0
                     AND Packet.mail-format EQ "XML-SWIFTContainer"
      	NO-LOCK:
      RUN PacketInclude(Packet.PacketID,
                        vPacketID,
                        {&STATE-FIN}).
   END.

   RUN XRKCServiceDelete (iFormat, hExch:TABLE-HANDLE).


   vFlagSet = YES.

END. /* MAIN */

{doreturn.i vFlagSet}

END PROCEDURE.

PROCEDURE GetSWInfo:
   DEFINE INPUT  PARAMETER iFName AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oRef   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oBIC   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oDType AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oAmt   AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oTailPresent AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oSenderBIC AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioErrLst AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vBuffer   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPos      AS INT64     NO-UNDO.
   DEFINE VARIABLE vBuf3Present AS LOGICAL   NO-UNDO INITIAL NO.

   INPUT FROM VALUE (iFName).
   REPEAT:
      IMPORT UNFORMATTED vBuffer NO-ERROR.
      ASSIGN
         oTailPresent = INDEX(vBuffer,"\{5:\{") NE 0
         vPos = R-INDEX(vBuffer, "~{1:")
      NO-ERROR.
      IF NOT vBuf3Present THEN vBuf3Present = INDEX(vBuffer,"\{3:\{") NE 0.
      IF vPos GT 0 THEN
         ASSIGN          
            oSenderBIC = SUBSTR(SUBSTR(vBuffer, vPos + 6, INDEX(vBuffer, "}") - 17), 1, 12)
            oSenderBIC = SUBSTR(oSenderBIC, 1, 8) + SUBSTR(oSenderBIC, 10)
         NO-ERROR.
      vPos = R-INDEX(vBuffer, "~{2:").
      IF vPos GT 0 THEN
         ASSIGN
            oBIC   = SUBSTR(vBuffer, vPos + 7, 12)
            oBIC   = SUBSTR(oBIC, 1, 8) + SUBSTR(oBIC, 10)
            oDType = SUBSTR(vBuffer, vPos + 4, 3)
         NO-ERROR.
      ELSE DO:
         vPos = INDEX(vBuffer, ":20:").
         IF vPos GT 0 THEN DO:
            oRef = SUBSTR(vBuffer, vPos + 4).
            oRef = TRIM(oRef).
         END.
         ELSE DO:
            vPos = INDEX(vBuffer, ":32A:").
            IF vPos GT 0 THEN DO:
               vBuffer = SUBSTR(vBuffer, vPos + 5).
               vBuffer = TRIM(vBuffer).
               vBuffer = SUBSTR(vBuffer, 10).
               vBuffer = REPLACE(vBuffer, ",", ".").
               oAmt = DECIMAL(vBuffer) NO-ERROR.
            END.
         END.
      END.           

   END.
   INPUT CLOSE.
   IF NOT vBuf3Present THEN {additem.i ioErrLst '"b8202"'}
   IF R-INDEX(oBIC, "}") GT 0 THEN
      oBIC = SUBSTRING(oBIC, 1, R-INDEX(oBIC, "}") - 1).

END PROCEDURE.

PROCEDURE GetUIS-COS:
   DEFINE INPUT PARAMETER iBIC   AS CHARACTER NO-UNDO.
   DEFINE       PARAMETER BUFFER code FOR code.
   DEFINE       PARAMETER BUFFER bCode50x FOR code.
    
   DEFINE VARIABLE vUIS          AS CHARACTER NO-UNDO.

   FIND FIRST code WHERE code.class EQ "УИС-ЦОС" AND
                         code.val   EQ iBIC
                   NO-LOCK NO-ERROR. 

   IF NOT AVAIL(code) THEN DO:
      RUN GetClearBIC (iBIC, OUTPUT iBIC).

      FIND FIRST code WHERE code.class EQ "УИС-ЦОС" AND
                            code.val   BEGINS iBIC
                      NO-LOCK NO-ERROR.
   END.
   IF AVAIL code THEN DO:
      FIND FIRST bCode50x WHERE
                 bCode50x.class EQ "УИС-ЦОСED50x" AND
                 bCode50x.code  BEGINS code.code
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE GetClearBIC:
   DEFINE INPUT  PARAMETER iBIC AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oBIC AS CHARACTER NO-UNDO.

   DO WHILE iBIC GT "" :
      IF SUBSTR(iBIC, LENGTH(iBIC), 1) NE "X" THEN
         LEAVE.
      iBIC = SUBSTR(iBIC, 1, LENGTH(iBIC) - 1). 
   END.
   oBIC = TRIM(iBIC).
END PROCEDURE.

/******************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/12/2015 09:47:20.950+04:00' */
/* $LINTUSER='voob' */
/* $LINTMODE='1' */
/* $LINTFILE='ed503.pro' */
/*prosignumu6VAhj+fO3cgOX4bqTMg*/