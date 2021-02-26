DEFINE STREAM sDir.

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

   DEFINE BUFFER Seance    FOR Seance.
   DEFINE BUFFER mail-user FOR mail-user.
   DEFINE BUFFER code      FOR code.
   DEFINE BUFFER bCode     FOR code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   /* получение каталога */
   ASSIGN
      vHExch    = hIns:DEFAULT-BUFFER-HANDLE
      vSeanceID = vHExch::SeanceID
      vFormat   = vHExch::mail-format
      vHDoc     = ObjectValueHandle("ExchRKCSw")
   NO-ERROR. {&ON-ERROR}

   vSizeLimit = INT64 (fGetSetting("УФЭБС","ED503Limit","15000")) NO-ERROR.
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
                                       OUTPUT vAmount) NO-ERROR.


      RUN InstanceCreate IN h_exch (?, vHDoc).
      ASSIGN
         FILE-INFO:FILE-NAME  = vFPath
         vHDoc::FPath         = FILE-INFO:FULL-PATHNAME
         vHDoc::FName         = vFName
         vHDoc::FSize         = FILE-INFO:FILE-SIZE
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
         vErrLst = ""
         vUISRec = "".

      IF vHDoc::FSize GT vSizeLimit THEN DO:
         {additem.i vErrLst '"b8201"'}
      END.

      RUN GetUIS-COS IN THIS-PROCEDURE (vHDoc::bank-code-rec, BUFFER code).
      IF NOT AVAIL code THEN DO:
         {additem.i vErrLst '"b8211"'}
      END.
      ELSE DO:
         ASSIGN
            vBegDate = DATE(code.Description[2])
            vEndDate = DATE(code.Description[3])
            vUISRec  = code.val
         NO-ERROR.
         IF  (vBegDate NE ? AND mOpDate LT vBegDate) OR
             (vEndDate NE ? AND mOpDate GE vEndDate) THEN DO:
            {additem.i vErrLst '"b8212"'}
         END.
      END.

      RUN GetUIS-COS IN THIS-PROCEDURE (bank-bic, BUFFER code).
      IF NOT AVAIL code THEN DO:
         {additem.i vErrLst '"b8221"'}
      END.
      ELSE DO:
         ASSIGN 
            vBegDate = DATE(code.Description[2])
            vEndDate = DATE(code.Description[3])
         NO-ERROR.
         IF  (vBegDate NE ? AND mOpDate LT vBegDate) OR
             (vEndDate NE ? AND mOpDate GE vEndDate) THEN DO:
            {additem.i vErrLst '"b8222"'}
         END.
      END.
      RUN CheckErrorInst IN h_exch (vHDoc::ErrorClass,
                                    vErrLst,
                                    OUTPUT vWarnings,
                                    OUTPUT vBadErrors).
      ASSIGN
        vHDoc::UIS       = vUISRec
        vHDoc::ErrorList = vErrLst
        vHDoc::BadErrors = vBadErrors.

   END.
   INPUT STREAM sDir CLOSE.

   vOK = vHDoc:FIND-FIRST("where __ID GT 0 ", NO-LOCK) NO-ERROR.
   IF NOT vOK then do:
      RUN Fill-SysMes("","","-1", "Не найдено ни одного файла для отправки.").
      UNDO MAIN, LEAVE MAIN.
   END.

   RUN ed503exp-rep.p (vHDoc:TABLE-HANDLE) NO-ERROR.

   vOK = vHDoc:FIND-FIRST("where __ID gt 0 and BadErrors eq '' ", NO-LOCK) 
         NO-ERROR.
   IF NOT vOK THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Отсутствуют сообщения " + 
                                  "SWIFT, пригодные для экспорта.").
      UNDO MAIN, LEAVE MAIN.
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
         UNDO MAIN, LEAVE MAIN.
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
            vHExch::bank-code-send = bank-bic
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

      /* чтение файла */
      COPY-LOB FROM FILE vHDoc::FPath TO vDocTmp NO-CONVERT NO-ERROR. {&ON-ERROR}

      /* кодирование файла */
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
            UNDO MAIN, RETRY MAIN.
         END.
         OS-DELETE VALUE(vHDoc::FPath).
         IF OS-ERROR NE 0 THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
                     SUBSTITUTE ("Невозможно удалить файл &1 " + 
                                 "Код ошибки: &2",
                                 vHDoc::FPath, STRING(OS-ERROR))).
            UNDO MAIN, RETRY MAIN.
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
                                    OUTPUT vAmount) NO-ERROR.

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

   DEFINE VARIABLE vBuffer   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPos      AS INT64     NO-UNDO.

   INPUT FROM VALUE (iFName).
   REPEAT:
      IMPORT UNFORMATTED vBuffer NO-ERROR.
      vPos = R-INDEX(vBuffer, "~{2:").
      IF vPos GT 0 THEN
         ASSIGN
            oBIC   = SUBSTR(vBuffer, vPos + 7, 11)
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

END PROCEDURE.

PROCEDURE GetUIS-COS:
   DEFINE INPUT PARAMETER iBIC   AS CHARACTER NO-UNDO.
   DEFINE       PARAMETER BUFFER code FOR code.

   DO WHILE iBIC GT "" :
      FIND FIRST code WHERE code.class EQ "УИС-ЦОС" AND
                            code.code  EQ iBIC
         NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         LEAVE.
      IF SUBSTR(iBIC, LENGTH(iBIC), 1) NE "X" THEN
         LEAVE.
      iBIC = SUBSTR(iBIC, 1, LENGTH(iBIC) - 1). 
   END.

END PROCEDURE.

/******************************************************************************/
/* $LINTUSER='VASOV' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 12:52:26.986+04:00' */
/* $LINTFILE='ed503.pro' */
/*prosignNOVoeWjZFtEZ7q5ZtooTfQ*/