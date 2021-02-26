/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: ESID-IMP.PRO
      Comment: Библиотека импорта электронных служебно-информационных
               сообщений УФЭБС
         Uses: BY DYNAMIC
      Used BY:
      Created: 20.08.2005 NIK
     Modified: 30/11/2005 NIK Доработан импорт форм ED201, ED205, ED207
     Modified: 14/12/2005 NIK Для документов выписки заполняются даты
                              due-date и doc-date
     Modified: 15/04/2006 NIK Импорт сообщения ED206
     Modified: 06/05/2006 NIK Сообщения с ошибками - в статус {&STATE-ERR}
     Modified: 25/05/2006 NIK Доработан импорт форм ED207, ED209, ED217
     Modified: 08/06/2006 NIK Контроль vInitialID
     Modified: 21/08/2006 NIK Создание ссылок при импорте выписки
     Modified: 27/09/2006 NIK 1.Управление статусами документов
                              2.Контроль последовательности квитанций
     Modified: 05/12/2006 NIK Верификация выписки
     Modified: 07/12/2006 NIK Конвертация выписки
     Modified: 09/01/2007 NIK 1. Использование МЦИСтатусы
                              2. Контроль документов с ошибками
     Modified: 30/01/2007 NIK Изменение статуса только начальных Сообщений
     Modified: 12/02/2007 NIK Пропуск квитанций на пакеты
     Modified: 30.05.2008 MUTA 0088648 Импорт ED332.
     Modified:
*/
&GLOBAL-DEFINE NO-BASE-PROC
&GLOBAL-DEFINE REP-LIMIT 30000
{sh-defs.i}
{intrface.get crd}

DEFINE Temp-Table ttLim NO-UNDO
   FIELD LimitTransKind     AS CHAR 
   FIELD LimitSum           AS DECIMAL    
   FIELD PURBIC             AS CHAR
   FIELD EDDate             AS CHAR
   FIELD EndTime            AS CHAR
   FIELD LiquiditySum       AS CHAR
   FIELD Debt               AS CHAR 
   FIELD OutBal             AS CHAR
   FIELD CreditLimitSum     AS CHAR
   FIELD ReservedSum        AS CHAR
   FIELD RTGSOutBal         AS CHAR
   FIELD RTGSReservedSum    AS CHAR
   FIELD TUTurnPayment      AS CHAR
   FIELD RTGSTurnPayment    AS CHAR
   FIELD EstimatedIncomeSum AS CHAR
          
.

DEFINE Temp-Table tt273 NO-UNDO
   FIELD Id          AS INT64
   FIELD PId         AS INT64
   FIELD mail-format AS CHAR
INDEX id mail-format Id.

DEFINE TEMP-TABLE ttEDRep NO-UNDO
   FIELD Date        AS CHAR
   FIELD Ref         AS CHAR
   FIELD Mes         AS CHAR
.

DEFINE VAR mId273       AS INT64  NO-UNDO.
DEFINE VARIABLE mEDReport   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mImpSt1-276 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mImpSt2-276 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mImpED510   AS LONGCHAR    NO-UNDO INIT "".
DEFINE VARIABLE mSWIFTList  AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE mSWIFTMess  AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE mText273_274  AS LOGICAL   NO-UNDO.

ASSIGN
   mImpSt1-276 = FGetSetting("УФЭБС", "Имп276Ст1", "")
   mImpSt2-276 = FGetSetting("УФЭБС", "Имп276Ст2", "")
   mText273_274 = FGetSetting("УФЭБС", "Text273_274", "НЕТ") =  "ДА"
.

FUNCTION EDRepKeep RETURNS CHARACTER (INPUT iEDReport AS CHARACTER,
                                      INPUT iAddTxt   AS CHARACTER,
                                      INPUT iPacketID AS INT64) FORWARD.

FUNCTION ESIDStatusDefine CHAR (INPUT  hWOP     AS handle,
                                INPUT  iStatus  AS CHAR,
                                BUFFER op-entry FOR op-entry)  forward.

FUNCTION GetXMLTag CHAR (INPUT         iTag   AS CHAR,
                         INPUT         iLen   AS INT64,
                         INPUT-OUTPUT  iText  AS CHAR) forward.

FUNCTION CheckXMLTag CHAR (INPUT       iTag1   AS CHAR,
                           INPUT       iVal1   AS CHAR,
                           INPUT       iTag2   AS CHAR,
                           INPUT       iVal3   AS CHAR) forward.

FUNCTION GetTTEDRep CHAR (INPUT iEDRepDate AS CHAR,
                          INPUT iEDRepRef  AS CHAR) FORWARD.

FUNCTION IsED222 LOGICAL (INPUT iMailFormat AS CHAR) FORWARD.

/*----------------------------------------------------------------------------*/
/* Импорт выписки в целом                                                     */
/* Метод INITIAL на классe XML-ED211                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDInitialSTMT:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER bXAttr    FOR xattr.
   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vRetValue    AS CHAR           NO-UNDO.
   DEFINE VAR hMain        AS handle         NO-UNDO.
   DEFINE VAR hAcct        AS handle         NO-UNDO.
   DEFINE VAR vAcct        AS CHAR           NO-UNDO.
   DEFINE VAR vPacketID    AS INT64          NO-UNDO.
   DEFINE VAR vParentID    AS INT64          NO-UNDO.
   DEFINE VAR vValue       AS CHAR           NO-UNDO.
   DEFINE VAR vBuffer      AS CHAR           NO-UNDO.
   DEFINE VAR vResult      AS CHAR           NO-UNDO.
   DEFINE VAR vMainFormat  AS CHAR           NO-UNDO.
   DEFINE VAR vStmtRest AS decimal           NO-UNDO.
   {profile ST001}

   DEFINE VARIABLE vRepPrefix    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI            AS INT64       NO-UNDO.
   DEFINE VARIABLE vRep          AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vDescr        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStmtDate     AS DATE        NO-UNDO.
   DEFINE VARIABLE vCancel       AS LOGICAL     NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN XRKCPacketCheck (iFormat, hWOP).       /* Начальный контроль        */
      vRetValue = RETURN-VALUE.
      IF {assigned vRetValue} THEN DO:
         vFlagSet = YES.
         LEAVE MAIN.
      END.

      RUN ESIDGetMain (INPUT-OUTPUT hWOP, OUTPUT hMain).

      ASSIGN
         vAcct       = hWOP:buffer-field("STMAcct"):buffer-value
         mStmtKind   = hWOP:buffer-field("STMKind"):buffer-value
         vStmtDate   = hWOP:buffer-field("STMDate"):buffer-value
         vStmtRest   = hWOP:buffer-field("STMRestOut"):buffer-value
         mStmtStatus = GetCode("ESIDStmt",mStmtKind)
         vRepPrefix  = TRNSettingValue("","StmtRepPrefix","STM")
         vMainFormat = hMain:buffer-field("mail-format"):buffer-value
      NO-ERROR. {&ON-ERROR}

      IF vAcct <> bank-acct THEN DO:             /* Контроль корсчета         */
         RUN Fill-SysMes("","ExchRKC25","","%s=" + GetNullStr(vAcct) +
                                           "%s=" + GetNullStr(bank-acct)).
         vRetValue = "SKIP".
         vFlagSet  = YES.
         LEAVE MAIN.
      END.

      IF NOT EXCH-MSGBuff (INPUT  iFormat,       /* Контроль формата обмена   */
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      IF      mStmtKind =  "0" /* окончательная выписка */ 
          AND vStmtDate <> mOpDate THEN DO:

         vCancel = NO.

         IF auto <> YES AND
            vMainFormat <> "XML-PacketESID"
         THEN DO:
            vCancel = YES.
            RUN xrkc-date-req.p(INPUT hWOP,
                                INPUT vStmtDate,
                                INPUT mOpDate,
                                INPUT-OUTPUT vCancel).

            IF vCancel =  YES THEN DO:
               RUN Fill-SysMes("","ExchRKC30","",
                               "%s=" + iFormat               +
                               "%s=" + GetNullStr(STRING(vStmtRest))  +
                               "%s=" + GetNullDat(vStmtDate) +
                               "%s=" + GetNullDat(mOpDate)).
               vRetValue = "SKIP".
               vFlagSet  = YES.
               LEAVE MAIN.
            END.
         END.
      END.

/*------------------------ Подготовка к конвертации выписки в другой формат --*/
      IF NOT {assigned mStmtKind} THEN 
         ASSIGN
            mStmtKind = "2"                /* ED221 */
            mStmtStatus = GetCode("ESIDStmt",mStmtKind)
            vRepPrefix  = vRepPrefix + ",CHK" 
         .

      IF mStmtKind =  "0"      AND
         {assigned mfStmtConv} THEN
         mhStmtConv = ObjectValueHandle("ExchSTMCnv").

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      IF {assigned mCorrAcct} THEN               /* Привязка к корсчету       */
         RUN PacketCreateLink(vPacketID,
                              "acct",
                              mCorrAcct + ",",
                              ENTRY(2,bCode.Description[1]) + "-" + GetNullStr(mStmtKind)).

/*------------------------------- Сохранение и контроль остатка по корсчету --*/
      IF NOT UpdateSigns("Packet",
                         string(vPacketID),
                         "StmtRest",
                         string(vStmtRest),
                         YES) THEN
         UNDO MAIN, RETRY MAIN.

      {profile ST003}
      
      RUN acct-pos IN h_base (mCorrAcct, "", mOpDate, mOpDate, "П").
      IF sh-bal <> vStmtRest THEN
         RUN Fill-SysMes("","ExchRKC43","",
                         "%s=" + GetNullStr(mCorrAcct)         +
                         "%s=" + GetNullNum(sh-bal)            +
                         "%s=" + GetNullStr(vAcct)             +
                         "%s=" + GetNullNum(vStmtRest)         +
                         "%s=" + GetNullNum(sh-bal - vStmtRest)).
                         
      {profile ST004}
/*----------------------------------------------- Сохранение данных выписки --*/
      vResult = "".
      RUN PacketTextKeep (INPUT        vPacketID,
                          INPUT        "ВЫПИСКА ПО СЧЕТУ~n~n",
                          INPUT-OUTPUT vResult).

XATTR-LOOP:
      FOR EACH bXAttr WHERE                      /* Основные данные - в тексте*/
               bXAttr.class-code =      hWOP:Name
               NO-LOCK
            BY bXAttr.Order:

         vRep = NO.
         DO vI =1 TO NUM-ENTRIES(vRepPrefix):
            IF bXAttr.Xattr-Code BEGINS ENTRY (vI,vRepPrefix) THEN
            DO:
               vRep = YES.
               LEAVE.
            END.
         END.
         IF vRep THEN
         DO:
            vValue  = FormatValueEx(hWOP:buffer-field(bXAttr.xattr-code):buffer-value,
                                    bXAttr.Data-Type,
                                    bXAttr.Data-Format).

            IF NOT {assigned vValue} THEN NEXT XATTR-LOOP.

            IF {assigned bXAttr.Domain-Code} THEN DO:
               vDescr = GetCodeNameEx (bXAttr.Domain-Code, vValue, "").
               IF {assigned vDescr} THEN
                  vValue = vValue + " - " + vDescr.
            END.

            vBuffer = string(bXAttr.xattr-label,"x(32)") + ":" + vValue + "~n".

            RUN PacketTextKeep (INPUT        vPacketID,
                                INPUT        vBuffer,
                                INPUT-OUTPUT vResult).
         END.
      END.

      RUN PacketTextSave (vPacketID,
                          vResult).
      {profile ST006}
/*------------------------------------------ Включение в головное сообщение --*/
      RUN PacketInclude  (vPacketID,
                          vParentID,
                          {&STATE-FIN}).

      ASSIGN
         hWOP:buffer-field("PacketID"):buffer-value  = vParentID
         hMain:buffer-field("PacketID"):buffer-value = vPacketID
      NO-ERROR. {&ON-ERROR}

      vFlagSet = YES.
   END.
   {profile ST010}
   {doreturn.i vFlagSet vRetValue}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт выписки в целом                                                     */
/* Метод IMPORT на классe XML-ED211 (Завершение обработки выписки)            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmSTMT:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR vArrestSum AS DECIMAL        NO-UNDO.
   DEFINE VAR vBeg       AS DATETIME       NO-UNDO.
   DEFINE VAR vEnd       AS DATETIME       NO-UNDO.
   DEFINE VAR vBlockID   AS INT64          NO-UNDO.

   DEFINE BUFFER BlockObject FOR BlockObject.

   {profile ST011}
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).
      ASSIGN
         mStmtStatus = ?
         mStmtKind   = ""
         vParentID   = hWOP:buffer-field("PacketID"):buffer-value
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vArrestSum  = hWOP::STMArrestSum
      NO-ERROR. {&ON-ERROR}

      ASSIGN
         hWOP:buffer-field("PacketID"):buffer-value  = vPacketID
         hMain:buffer-field("PacketID"):buffer-value = vParentID
      NO-ERROR. {&ON-ERROR}

      IF vArrestSum >  0 AND vArrestSum <> ? THEN DO:
         ASSIGN 
            vBeg = DATETIME(mOpDate, 0)
            vEnd = DATETIME(mOpDate + 1, 0)
         .
         FIND FIRST BlockObject WHERE BlockObject.file-name    =  "acct"
                                  AND Blockobject.surrogate    =  mCorrAcct + ","
                                  AND BlockObject.beg-datetime =  vBeg
                                  AND BlockObject.block-type   =  "БлокСумм"
            NO-LOCK NO-ERROR.

         IF NOT AVAIL BlockObject THEN DO:
            RUN CreateBlockObj IN h_blkob ("acct",
                                            mCorrAcct + ",",
                                            vBeg,
                                            "БлокСумм") NO-ERROR. {&ON-ERROR}
         END.
         
         FOR FIRST BlockObject WHERE BlockObject.file-name    =  "acct"
                                 AND Blockobject.surrogate    =  mCorrAcct + ","
                                 AND BlockObject.beg-datetime =  vBeg
                                 AND BlockObject.block-type   =  "БлокСумм"
            EXCLUSIVE-LOCK:
            ASSIGN
               vBlockID                 = BlockObject.BlockObjectID
               BlockObject.val[3]       = vArrestSum
               BlockObject.val[4]       = vArrestSum
               BlockObject.end-datetime = vEnd
            NO-ERROR.  {&ON-ERROR}
            VALIDATE BlockObject NO-ERROR. {&ON-ERROR}
         END.

         RUN PacketCreateLink IN h_pack (vPacketID, "BlockObject", STRING(vBlockID), "ED211").
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Установлена блокировка на корсчете на сумму " + STRING(vArrestSum)).

      END.

/*------------------------------- Подготовка данных для конвертации выписки --*/
      IF {assigned mfStmtConv}    AND            /* Задана транзакция         */
         valid-handle(mhStmtConv) THEN DO:       /* И буфер конвертации       */

         ASSIGN
            hWOP:buffer-field("Surrogate"):buffer-value     = {&NO-MAKE-LINK}
            hWOP:buffer-field("File-Name"):buffer-value     = {&NO-MAKE-LINK}
            hWOP:buffer-field("mail-format"):buffer-value   = FGetSetting("УФЭБС","StmCnvFormFH","POSmciRVVFH")
            hWOP:buffer-field("mail-user-num"):buffer-value = ? 
            hWOP:buffer-field("ExchMain"):buffer-value      = ""
            hWOP:buffer-field("State"):buffer-value         = {&STATE-CRT}
         NO-ERROR. {&ON-ERROR}

         RUN RunTransaction(mfStmtConv).         /* Транзакция экспорта итога */
      END.

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      hWOP:buffer-delete().

      vFlagSet = YES.
   END.
   {profile ST020}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Подтверждение документов при импорте выписки                               */
/* Метод IMPORT на класcе XML-TransInfo                                       */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmTRANS:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bop       FOR op.
   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER bOpEntry  FOR op-entry.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagDel  AS LOGICAL        NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vKind     AS CHAR           NO-UNDO.
   DEFINE VAR vSurr     AS CHAR           NO-UNDO.
   DEFINE VAR vStatus   AS CHAR           NO-UNDO.
   DEFINE VAR vState    AS CHAR           NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR vInitID   AS INT64        NO-UNDO.
   DEFINE VAR vErrorCls AS CHAR           NO-UNDO.
   DEFINE VAR vErrorLst AS CHAR           NO-UNDO.
   DEFINE VAR vErrorBIS AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorRKC AS LOGICAL        NO-UNDO.
   DEFINE VAR vRecvREF  AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDAT  AS DATE           NO-UNDO.
   DEFINE VAR vRecvID   AS CHAR           NO-UNDO.
   DEFINE VAR vStrOp    AS CHAR           NO-UNDO.
   
   {profile ST021}
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

/*------------------------------------------ Определение начальных значений --*/
      IF NOT EXCH-MSGBuff   (INPUT  iFormat,
                             BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

/*----------------------------------------------- Поиск исходного документа --*/
      RUN ESIDOperationFind  (INPUT  hWOP,
                              INPUT  vPacketID,
                              BUFFER bCode,
                              OUTPUT vInitID,
                              OUTPUT vSurr).

/*----------------------------------------------- Контроль ошибок в выписке --*/
      RUN ESIDOperCompare    (INPUT  hWOP,
                              INPUT  vSurr,
                              BUFFER bOpEntry).

      RUN ESIDErrorPrepare   (INPUT  hWOP,
                              INPUT  "u",
                              OUTPUT vErrorCls,
                              OUTPUT vErrorRKC,
                              OUTPUT vErrorBIS,
                              OUTPUT vErrorLst).

      RUN PacketSetError     (INPUT  vPacketID,
                              INPUT  vErrorCls,
                              INPUT  vErrorLst,
                              OUTPUT vState).

      RUN PacketInclude      (INPUT  vPacketID,
                              INPUT  vParentID,
                              INPUT  {&STATE-FIN}).

/*--------------------------------------------- Изменение статуса документа --*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmTRANS","4 vErrorBIS:" + string(vErrorBIS) +
                                              " vSurr:" + GetNullStr(vSurr) +
                                        " mStmtStatus:" + GetNullStr(mStmtStatus)).
      &ENDIF

      ASSIGN
         vRecvDAT   = hWOP:buffer-field("RecvDate"):buffer-value
         vRecvID    = hWOP:buffer-field("RecvID"):buffer-value
         vRecvREF   = hWOP:buffer-field("RecvREF"):buffer-value
      .

      {profile ST024}


      IF NOT AVAIL bOpEntry THEN 
         RUN Fill-SysMes ("","ExchRKC32c","","%s=" + GetNullStr(iFormat)    +
                                             "%s=" + GetNullStr(vRecvID)    +
                                             "%s=" + GetNullStr(REPLACE(vRecvREF,"|",":"))   +
                                             "%s=" + GetNullDat(vRecvDAT)
                         ).
      ELSE IF vErrorBIS    =  NO AND
         {assigned vSurr}        AND
         {assigned mStmtStatus}  THEN DO:
 
         IF mStmtSCheck          OR
            {assigned mStateErr} THEN
            vStatus = ESIDStatusDefine(hWOP, mStmtStatus, BUFFER bOpEntry).
 
         IF {assigned vStatus} THEN
            FOR EACH  op WHERE
                      op.op =  INT64(ENTRY(1,vSurr))
                      EXCLUSIVE-LOCK:
               UpdateSigns("Packet",string(vPacketID),"StateOBJ",op.op-status,NO).
               FIND FIRST op-entry OF op EXCLUSIVE-LOCK NO-ERROR.
               IF vStatus BEGINS "А" THEN DO:
                  ASSIGN
                     op.op-date = ?
                     op-entry.op-date = op.op-date.
               END.

               {opent-st.i &Status = vStatus}
               /* Изменение статуса связанного документа по ED104 при загрузке ED211*/
               /* Ищем линк документа по ED104 */
               vStrOp = GetLinks(op.class-code,
                  STRING(op.op),
                  ?,
                  "link104",
                  ",",
                  op.op-date).
                  
               IF {assigned vStrOp} THEN DO:
                  
                  FIND FIRST bop WHERE bop.op =  INT64(vStrOp) NO-LOCK NO-ERROR.
                  IF AVAIL bop THEN DO:
                     FIND CURRENT bop EXCLUSIVE-LOCK.
                     {opent-st.i &pre = "b" &Status = op.op-status} /* ставим статус */
                  END.
               END.
               
               IF op.class-code =  "Opb-reestrRKC" THEN DO:  /* сводное п/п с реестром */
                  RUN ReestrChgStatus (op.op, vStatus).
               END.
               
            END.
      END.
      {profile ST025}
/*------------------------------- Контроль необходимости создания документа --*/
      IF mStmtKind =  "0" THEN DO:               /* Выписка окончательная     */

         IF valid-handle(mhStmtConv) THEN
            RUN ESIDConvertSTMT(hWOP,mhStmtConv).

         IF {assigned vSurr} THEN                /* Исходный документ найден  */
            vFlagDel = YES.
         ELSE DO:                                /*Исходный документ не найден*/

            GetEXCHRule (INPUT  hWOP:buffer-field("SeanceID"):buffer-value,
                         INPUT  {&DIR-IMPORT},
                         INPUT  chr(1) + iFormat,
                         BUFFER mail-user).

            vFlagDel = NO.
            ASSIGN
               vRecvREF                                        = hWOP:buffer-field("RecvREF"):buffer-value
               vRecvDAT                                        = hWOP:buffer-field("RecvDate"):buffer-value
               vRecvID                                         = hWOP:buffer-field("RecvID"):buffer-value
               hWOP:buffer-field("SendREF"):buffer-value       = vRecvREF
               hWOP:buffer-field("SendID"):buffer-value        = vRecvID
               hWOP:buffer-field("SendDate"):buffer-value      = vRecvDAT
               hWOP:buffer-field("mail-format"):buffer-value   = "XML-ED110"
               hWOP:buffer-field("mail-user-num"):buffer-value =  mail-user.mail-user-num 
               hWOP:buffer-field("ErrorList"):buffer-value     = ""

               hWOP:buffer-field("due-date"):buffer-value      = hWOP:buffer-field("op-date"):buffer-value
               hWOP:buffer-field("doc-date"):buffer-value      = hWOP:buffer-field("op-date"):buffer-value
               hWOP:buffer-field("order-pay"):buffer-value     = 5
               hWOP:buffer-field("op"):buffer-value            = next-value(op-id)
            NO-ERROR. {&ON-ERROR}

            RUN PacketCreateRef (vRecvDAT,
                                 vPacketID,
                                 bCode.Misc[{&RKC-REPLY}],
                                 vRecvID + "|" +
                                 ReferenceFormatValue(bCode.Misc[{&RKC-REPLY}],vRecvREF)).
         END.
      END.
      ELSE                                       /* Неокончательная выписка   */
         vFlagDel = YES.

      IF vFlagDel THEN DO:
         hWOP:buffer-delete()               NO-ERROR. {&ON-ERROR}
         hWOP:find-first("where __id EQ 0") NO-ERROR. {&ON-ERROR}
      END.

      {profile ST029}

      vFlagSet = YES.
   END.
   {profile ST030}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 1. Извещение о результатах контроля документа (пакета документов) (ED201)  */
/* 2. Извещение о статусе документа (пакета документов)              (ED205)  */
/* 3. Извещение о статусе группы документов                          (ED207)  */
/* 4. Извещение о подтверждении дебета-кредита документа             (ED206)  */
/*                                                                            */
/* Параметры:                                                                 */
/*    iFormat   - Формат XML-объекта                                          */
/*    iInstance - Сущность, содержащая импортированные данные                 */
/*                                                                            */
/* Метод IMPORT на классах XML-ED20x                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmSTAT:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER bop       FOR op.
   DEFINE BUFFER op-entry  FOR op-entry.
   DEFINE BUFFER kau-entry FOR kau-entry.
   DEFINE BUFFER bOpEntry  FOR op-entry.
   DEFINE BUFFER chPacket  FOR Packet.
   DEFINE BUFFER chReference FOR Reference.
   DEFINE BUFFER sCode     FOR Code.
   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER bPacket   FOR Packet.
   DEFINE BUFFER iPacket   FOR Packet.
   DEFINE BUFFER Reference FOR Reference.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagFnd     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagBeg     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vIsPack      AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain        AS handle         NO-UNDO.
   DEFINE VAR vSurr        AS CHAR           NO-UNDO.
   DEFINE VAR v__ID        AS INT64          NO-UNDO.
   DEFINE VAR v__UP        AS INT64          NO-UNDO.
   DEFINE VAR vPacketID    AS INT64          NO-UNDO.
   DEFINE VAR vParentID    AS INT64          NO-UNDO.
   DEFINE VAR vInitialID   AS INT64          NO-UNDO.
   DEFINE VAR vErrorCls    AS CHAR           NO-UNDO.
   DEFINE VAR vErrorRKC    AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorBIS    AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorLst    AS CHAR           NO-UNDO.

   DEFINE VAR vStateKO     AS CHAR           NO-UNDO.
   DEFINE VAR vStateUBR    AS CHAR           NO-UNDO.
   DEFINE VAR vStatus      AS CHAR           NO-UNDO.

   DEFINE VAR vNote        AS CHAR           NO-UNDO.
   DEFINE VAR vResult      AS CHAR           NO-UNDO.
   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mRunKau      AS LOGICAL        NO-UNDO.
   DEFINE VAR vKauErr      AS INT64          NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vRecvREF     AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDAT     AS DATE           NO-UNDO.
   DEFINE VAR vRecvID      AS CHAR           NO-UNDO.

   DEFINE VARIABLE vRecvDate     AS DATE        NO-UNDO.
   DEFINE VARIABLE vInitClassRef AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInitPackID   AS INT64       NO-UNDO.
   DEFINE VARIABLE vReport       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCtrlCode     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSendID       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSendDate     AS DATE        NO-UNDO.
   DEFINE VARIABLE vSendREF      AS INT64       NO-UNDO.
   DEFINE VARIABLE vSendCls      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOld201       AS INT64       NO-UNDO.
   DEFINE VARIABLE vState201     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRef          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStrOp        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vEDDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vHsrv         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vState540     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPPacketID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vEDNo         AS CHARACTER   NO-UNDO.
     
   DEFINE VARIABLE vErrCode      AS CHARACTER   NO-UNDO. 
   DEFINE VARIABLE chPacketID    AS INT64       NO-UNDO.          
   
   mOpKind     = GetBaseOpKind().
   mSETClass   = GetXAttrValue("op-kind",mOpKind,"SETClass").
   mRunKAU     = TRNSettingValue(mSETClass,"RunKAU","NO") =  "YES".

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      ASSIGN
         hWOP:buffer-field("acct-rec"):buffer-value = ""
      NO-ERROR.

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

    
      IF iFormat BEGINS "XML-ED201" THEN DO:

         ASSIGN
            vSendID   = hWOP:buffer-field("SendID"):buffer-value
            vSendDate = hWOP:buffer-field("SendDate"):buffer-value
            vSendREF  = hWOP:buffer-field("SendREF"):buffer-value
            vSendCls  = bCode.misc[{&RKC-REPLY}]
            vRecvDate = hWOP::RecvDate
            vRecvREF  = hWOP::RecvREF
            vInitClassRef = bCode.misc[4]
         NO-ERROR. {&ON-ERROR}

         /* дополнительная проверка на повторную загрузку  */
         vRef = vSendID  + "|" + ReferenceFormatValue(vSendCls,string(vSendREF)).

         FOR EACH Reference WHERE
                  Reference.op-date     =  vSendDate
              AND Reference.Class-Code  =  vSendCls
              AND Reference.RefValue    =  vRef
              AND Reference.PacketID    <  vPacketID
            NO-LOCK,
            FIRST PackObject WHERE PackObject.File-Name =  "op-entry" 
                               AND PackObject.PacketID  =  Reference.PacketID
            NO-LOCK QUERY-TUNING(HINT "RULE"):

            vOld201 = Reference.PacketID.
            LEAVE.
         END.
         IF vOld201 >  0 THEN DO:

            RUN Fill-SysMes("","ExchRKC02","","%s=" + iFormat + "%s=" + GetNullStr(vSendID) +
                               "%i=" + GetNullInt(vSendREF) + "%s=" + GetNullDat(vSendDate) +
                               "%i=" + GetNullInt(vOld201)).

            vErrorLst = "b8070" + CHR(1) +
                         "Сообщение " + iFormat + "(" + GetNullStr(vSendID) + "|" + GetNullInt(vSendREF) + ")" +
                         " от " + GetNullDat(vSendDate) + " уже принято." +
                         " Идентификатор в базе " + GetNullInt(vOld201).

            RUN PacketSetError in h_pack (vPacketID,
                                          hWOP::ErrorClass,
                                          vErrorLst,
                                          OUTPUT vState201).
            vFlagSet = YES.
            LEAVE MAIN.
         END.
         

         RUN ESIDInitFind (vRecvDate, vInitClassRef, ReferenceFormatValue(vInitClassRef, vRecvREF), 
                           OUTPUT vInitPackID).

         &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p("ESIDConfirmSTAT", "vInitPackID:" + GetNullInt(vInitPackID)).
         &ENDIF

         IF vInitPackID <> ? THEN DO:
            FIND FIRST iPacket WHERE iPacket.PacketID =  vInitPackID NO-LOCK NO-ERROR.
            IF AVAIL iPacket THEN DO:
               IF iPacket.mail-format BEGINS "XML-ED318" THEN DO:
                  FIND FIRST bPacket WHERE bPacket.PacketID =  iPacket.PacketID NO-ERROR.
                  IF AVAIL bPacket THEN bPacket.State = "ОШБК".
                  vFlagSet = YES.
                  LEAVE MAIN.
               END.
               ELSE IF iPacket.mail-format BEGINS "XML-ED393" THEN DO:
                  hWOP::ParentID = vParentID.
                  RUN ESIDConfirmCHANNEL(iFormat, hWOP) NO-ERROR. {&ON-ERROR}
                  vFlagSet = YES.
                  LEAVE MAIN.
               END.
               ELSE IF iPacket.mail-format BEGINS "XML-ED273" THEN DO:
                  hWOP::ParentID = vParentID.
                  RUN ESIDConfirmCHANNEL(iFormat, hWOP) NO-ERROR. {&ON-ERROR}
                  vFlagSet = YES.
                  LEAVE MAIN.
               END.
            END.

         END.
      END.

      IF iFormat BEGINS "XML-ED201" THEN DO:
         vCtrlCode = hWOP::ErrorList.
         RUN PacketTextKeep (vPacketID, "Код контроля:" + vCtrlCode + "~n", INPUT-OUTPUT vResult).
      END.
      ASSIGN vNote = hWOP:buffer-field("Note"):buffer-value        NO-ERROR.
      IF {assigned vNote} THEN
         RUN PacketTextKeep (vPacketID, vNote + "~n", INPUT-OUTPUT vResult).
      ASSIGN vNote = hWOP:buffer-field("Description"):buffer-value NO-ERROR.
      IF {assigned vNote} THEN
         RUN PacketTextKeep (vPacketID, vNote + "~n", INPUT-OUTPUT vResult).

      IF iFormat BEGINS "XML-ED530" THEN DO:
         RUN PacketInclude    (INPUT  vPacketID,
                               INPUT  vParentID,
                               INPUT  {&STATE-FIN}).

         RUN PacketTextSave (vPacketID, vResult).
         IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
            RUN PacketBSPTextSave(vParentID).

         vFlagSet = YES.
         LEAVE MAIN.
      END.


/*----------------------------------------------- Поиск исходного документа --*/
      RUN ESIDOperationFind  (INPUT  hWOP,
                              INPUT  vPacketID,
                              BUFFER bCode,
                              OUTPUT vInitialID,
                              OUTPUT vSurr).     /* Суррогат объекта          */

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmSTAT","vInitialID:" + GetNullInt(vInitialID) +
                                            " vSurr:" + GetNullStr(vSurr)  +
                                           "iFormat:" + iFormat).
      &ENDIF

      IF vInitialID <> 0 AND 
         vInitialID <> ? AND 
         iFormat BEGINS "XML-ED201" THEN DO:
         RUN ESIDOperationED201 (INPUT  hWOP,
                                 BUFFER bCode,
                                 INPUT  vPacketID,
                                 INPUT  vInitialID).
      END.
/*----------------------------------------------------- Сравнение документа --*/
      IF iFormat =  "XML-ED206" OR
         iFormat =  "XML-ED206BSP" THEN
         RUN ESIDOperCompare    (INPUT  hWOP,
                                 INPUT  vSurr,
                                 BUFFER bOpEntry).

/*--------------------------------------------- Контроль ошибок в квитанции --*/
      RUN ESIDErrorPrepare (INPUT  hWOP,
                            INPUT  "u",
                            OUTPUT vErrorCls,
                            OUTPUT vErrorRKC,
                            OUTPUT vErrorBIS,
                            OUTPUT vErrorLst).

      RUN PacketSetError   (INPUT  vPacketID,
                            INPUT  vErrorCls,
                            INPUT  vErrorLst,
                            OUTPUT vStateKO).

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      IF ENTRY(2,bCode.Description[1])  =  "ED207" THEN DO:
         FIND FIRST Packet WHERE Packet.PacketID =  vPacketID
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL Packet THEN
            ASSIGN
               iFormat            = "XML-ED207" 
               Packet.mail-format = iFormat
            .

         FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.
         RUN SetSysConf IN h_base ("ED207ParentID", STRING(vParentID)).

         RUN CreateEDInfoReport ("XMLAttr-EDInfo",   hWOP, INPUT-OUTPUT vReport).
         RUN CreateEDInfoReport ("XMLAttr-RefIDSrv", hWOP, INPUT-OUTPUT vReport).

         RUN PacketTextSave (vPacketID, vReport).

      END.
/*------------------------------------------------ Определим целевой статус --*/
      IF vErrorBIS =  NO THEN CASE iFormat:
         WHEN "XML-ED201" OR
         WHEN "XML-ED201BSP" THEN                /* Извещение о контроле      */
            IF vErrorRKC THEN ASSIGN             /* Статус с ошибками         */
               vStatus  = mStateRtrn
               vFlagBeg = YES
               vIsPack  = YES
            .
            ELSE ASSIGN
               vStatus  = mStateRecv
               vFlagBeg = YES
               vIsPack  = YES
            .
         WHEN "XML-ED206" OR
         WHEN "XML-ED206BSP" THEN DO:               /* Извещение о дебете счета  */
            IF vErrorRKC  THEN ASSIGN            /* Статус с ошибками         */
               vStatus  = mStateRtrn
               vFlagBeg = NO
               vIsPack  = YES
            .
            ELSE ASSIGN
               vStatus  = mStateDone
               vFlagBeg = NO
               vIsPack  = YES
            .
            ASSIGN
               vRecvDAT   = hWOP:buffer-field("RecvDate"):buffer-value
               vRecvID    = hWOP:buffer-field("RecvID"):buffer-value
               vRecvREF   = hWOP:buffer-field("RecvREF"):buffer-value
            .
            IF NOT AVAIL bOpEntry THEN
               RUN Fill-SysMes ("","ExchRKC32c","","%s=" + GetNullStr(iFormat)    +
                                                   "%s=" + GetNullStr(vRecvID)    +
                                                   "%s=" + GetNullStr(REPLACE(vRecvREF,"|",":"))  +
                                                   "%s=" + GetNullDat(vRecvDAT)).


            ELSE IF mStmtSCheck      OR
               {assigned mStateErr}  OR 
               {assigned mStateChgd} THEN          /* Контроль по МЦИСтатус     */

               vStatus = ESIDStatusDefine(hWOP, vStatus, BUFFER bOpEntry).
         END.
         OTHERWISE DO:                           /* Извещение о состоянии     */
            ASSIGN                               /* Статус документа в РКЦ    */
               vStateUBR = hWOP:buffer-field("State"):buffer-value
               vFlagBeg  = YES
            NO-ERROR. {&ON-ERROR}
                                                 /* Статус документа в БИСКвит*/
            if GetCodeBuff("ESIDState",vStateUBR, buffer sCode) then assign
               vStatus = sCode.Val
               vIsPack = NOT (sCode.Misc[1] =  "НЕТ" or sCode.Misc[1] =  "NO")
            .
            else assign
               vStatus = ?
               vIsPack = NO
            .

            RUN PacketTextKeep (vPacketID,
                                vStateUBR                          + " "  +
                                GetCodeName("ESIDState",vStateUBR) + "~n",
                                INPUT-OUTPUT vResult).
         END.
      END CASE.
      ELSE DO:               /* Есть сомнения, что ссылка на верный документ  */
         IF NOT (vSurr =  "" AND vInitialID <> 0) THEN DO: /* служебные сообщ.*/
            RUN Fill-SysMes("","ExchRKC41","","%s=" + GetNullStr(iFormat)).
            vStatus = ?.
         END.
         ELSE vStatus = {&STATE-FIN}.
      END.

      IF NOT {assigned vStatus} THEN DO:
         RUN Fill-SysMes("","ExchRKC31","", "%s=" + iFormat              +
                                            "%s=" + GetNullStr(vStateKO) +
                                            "%s=" + GetNullStr(vStateUBR)).
         RUN PacketTextSave (vPacketID, vResult).
         vFlagSet =  YES.
         LEAVE MAIN.
      END.
/*---------------------------- Измененим статус документа/пакета документов --*/
      IF {assigned trim(vSurr)} THEN DO:         /* Документ                  */

         IF vFlagBeg THEN                        /* Начальные документы       */
            IF vStatus <  mStateSend THEN        /* Исх. сообщение - ошибка   */
               RUN PacketSetState(vInitialID, {&STATE-ERR}).
            ELSE
               RUN PacketSetState(vInitialID, {&STATE-FIN}).

         FIND FIRST Packet WHERE Packet.PacketID =  vInitialID NO-LOCK NO-ERROR.

         IF AVAIL Packet AND NOT CAN-DO(mEDNoSetStat, Packet.mail-format)
         THEN 
            FOR FIRST op WHERE op.op =  INT64(ENTRY(1,vSurr))
                      EXCLUSIVE-LOCK,
            FIRST op-entry OF op EXCLUSIVE-LOCK:
               UpdateSigns("Packet",string(vPacketID),"StateOBJ",op.op-status,NO).

               {opent-st.i &status=vStatus}         /* Изменение статусов док-та */
               /* Изменение статуса связанного документа по ED104 - ED206*/
               /* Ищем линк документа по ED104 */
               vStrOp = GetLinks(op.class-code,
                  STRING(op.op),
                  ?,
                  "link104",
                  ",",
                  op.op-date).
                  
               IF {assigned vStrOp} THEN DO:
                  FIND FIRST bop WHERE bop.op =  INT64(vStrOp) NO-LOCK NO-ERROR.
                  IF AVAIL bop THEN DO:
                     FIND CURRENT bop EXCLUSIVE-LOCK.
                     {opent-st.i &pre = "b" &Status = op.op-status} /* ставим статус */
                  END.
               END.

               IF mRunKAU THEN DO:
                  RUN Kau-Trigger IN h_op (INPUT  recid(op-entry),
                                           OUTPUT vKauErr,
                                           INPUT  NO).
                  IF vKauErr <> 0 THEN
                     RUN AddErrorFormat(hWOP, "Ошибка обработки субаналитики (" +
                                             string(vKauErr)            + ") " +
                                             GetNullStr(pick-value)).
               END.

               IF op.class-code =  "Opb-reestrRKC" THEN DO: /* это сводное п/п с реестром */
                  RUN ReestrChgStatus (op.op, vStatus).
               END.

            END.
      END.
      ELSE IF vInitialID <> ?   AND              /* Пакет документов          */
              vInitialID <> 0   AND
              vIsPack    =  YES THEN DO:

         vStatus = ENTRY(1,vStatus).
         FOR EACH Packet WHERE
                  Packet.ParentID =  vInitialID
                  EXCLUSIVE-LOCK,
            FIRST PackObject WHERE
                  PackObject.PacketID  =  Packet.PacketID
              AND PackObject.File-Name =  "op-entry"
                  NO-LOCK,
             FIRST op WHERE
                   op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                   EXCLUSIVE-LOCK,
             FIRST op-entry OF op
                   EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

            {opent-st.i &status=vStatus}         /* Изменение статусов док-та */
            IF mRunKAU THEN DO:
               RUN Kau-Trigger IN h_op (INPUT  recid(op-entry),
                                        OUTPUT vKauErr,
                                        INPUT  NO).
               IF vKauErr <> 0 THEN
                  RUN AddErrorFormat(hWOP, "Ошибка обработки субаналитики (" +
                                          string(vKauErr)            + ") " +
                                          GetNullStr(pick-value)).
            END.

            IF vStatus <  mStateSend  THEN ASSIGN
               Packet.State = {&STATE-ERR}.      /* Отменим исх. сообщение    */
         END.
      END.

      RUN PacketTextSave (vPacketID, vResult).

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vParentID).

      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт отчетной формы                                                      */
/* Метод IMPORT на реквизите ED219:ReportContent                              */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImportFORM:
   DEFINE INPUT PARAMETER hSTM   AS HANDLE   NO-UNDO.
   DEFINE INPUT PARAMETER pData  AS MEMPTR   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vFormat   AS CHAR           NO-UNDO.
   DEFINE VAR vSurr     AS CHAR           NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.

   DEFINE VAR vSize     AS INT64        NO-UNDO.
   DEFINE VAR vStep     AS INT64        NO-UNDO.
   DEFINE VAR vPos      AS INT64        NO-UNDO.
   DEFINE VAR vBuffer   AS CHAR           NO-UNDO.
   DEFINE VAR vResult   AS CHAR           NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vFormat = hSTM:buffer-field("mail-format"):buffer-value
         hSTM    = hSTM:table-handle
      NO-ERROR.

      IF NOT EXCH-MSGBuff (INPUT  vFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hSTM,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hSTM,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImportFORM","vPacketID:" + GetNullInt(vPacketID) + 
                         " vParentID:" + GetNullInt(vParentID)).    
      &ENDIF

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      vSize = get-size(pData).
      vPos  = 1.
      DO WHILE vPos <  vSize:
         vStep   = minimum(vSize - vPos, {&PACK-LIMIT}).
         vBuffer = get-string(pData,vPos,vStep).
         RUN PacketTextKeep (vPacketID,vBuffer,INPUT-OUTPUT vResult).
         vPos  = vPos + {&PACK-LIMIT}.
      END.

      RUN PacketTextSave(vPacketID,vResult).

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Извещение о режиме обмена/работы счета                                     */
/* Метод IMPORT на классе XML-ED209                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmACCT:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR v__ID     AS INT64        NO-UNDO.
   DEFINE VAR v__UP     AS INT64        NO-UNDO.
   DEFINE VAR vCode     AS CHAR           NO-UNDO.
   DEFINE VAR vNote     AS CHAR           NO-UNDO.
   DEFINE VAR vDateBeg  AS DATE           NO-UNDO.
   DEFINE VAR vDateEnd  AS DATE           NO-UNDO.
   DEFINE VAR vResult   AS CHAR           NO-UNDO.
   DEFINE VAR vStReason AS CHAR           NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      RUN PacketInclude     (INPUT  vPacketID,
                             INPUT  vParentID,
                             INPUT  {&STATE-FIN}).

      RUN PacketCreateLink  (INPUT  vPacketID,
                             INPUT  "acct",
                             INPUT  mCorrAcct + ",",
                             INPUT  ENTRY(2,bCode.Description[1])).

      ASSIGN
         vCode     = hWOP:buffer-field("SystemCode"):buffer-value
         vDateBeg  = hWOP:buffer-field("DateBeg"):buffer-value
         vDateEnd  = hWOP:buffer-field("DateBeg"):buffer-value
         vStReason = hWOP:buffer-field("StoppageReason"):buffer-value
         vNote     = TRIM(hWOP:buffer-field("Description"):buffer-value)
      NO-ERROR. {&ON-ERROR}

      RUN PacketTextKeep(vPacketID,
                         GetNullStr(vCode) + " - " + GetNullStr(GetCodeName("ESIDMode",vCode)) + "~n" +
                         (IF {assigned vStReason} THEN
                             "Причина введения ограничения участия " + GetNullStr(vStReason)
                                                  ELSE "")                                     + "~n" +
                         GetNullStr(vNote)                                                     + "~n" +
                         "Дата начала действия режима:" + GetNullDat(vDateBeg)                 + "~n" +
                         "Дата конца  действия режима:" + GetNullDat(vDateEnd)                 + "~n",
                         INPUT-OUTPUT vResult).
      RUN PacketTextSave(vPacketID,vResult).

      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP) NO-ERROR.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Текст описания изменения расписания                                        */
/* Метод IMPORT на классе XML-ED330                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDimpED330:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR v__ID     AS INT64        NO-UNDO.
   DEFINE VAR v__UP     AS INT64        NO-UNDO.
   DEFINE VAR vCode     AS CHAR           NO-UNDO.
   DEFINE VAR vNote     AS CHAR           NO-UNDO.
   DEFINE VAR vDateBeg  AS DATE           NO-UNDO.
   DEFINE VAR vDateEnd  AS DATE           NO-UNDO.
   DEFINE VAR vResult   AS CHAR           NO-UNDO.

   DEFINE VAR vCreateProc  AS CHAR        NO-UNDO.
   DEFINE VAR vCreateParms AS CHAR        NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      RUN PacketInclude     (INPUT  vPacketID,
                             INPUT  vParentID,
                             INPUT  {&STATE-FIN}).


      ASSIGN
         vNote     = TRIM(hWOP:buffer-field("Description"):buffer-value)
      NO-ERROR. {&ON-ERROR}

      RUN PacketTextKeep(vPacketID,
                         "Информация о корректировке временного регламента функционирования ЦОиР:"  + "~n" +
                         GetNullStr(vNote),   
                         INPUT-OUTPUT vResult).
      RUN PacketTextSave(vPacketID,vResult).

      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/

   RUN GetClassMethod IN h_xclass (INPUT  iFormat,
                                   INPUT  "Create",
                                   INPUT  "",
                                   INPUT  "",
                                   OUTPUT vCreateProc,
                                   OUTPUT vCreateParms).

   /* удаляем только созданные методом Create инстансы*/
   IF {assigned vCreateProc} THEN DO: 
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
      IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP) NO-ERROR.
   END.


   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Текст описания изменения расписания                                        */
/* Метод IMPORT на классе XML-ED330BSP                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDimpED330BSP:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR v__ID     AS INT64        NO-UNDO.
   DEFINE VAR v__UP     AS INT64        NO-UNDO.
   DEFINE VAR vCode     AS CHAR           NO-UNDO.
   DEFINE VAR vNote     AS CHAR           NO-UNDO.
   DEFINE VAR vDateBeg  AS DATE           NO-UNDO.
   DEFINE VAR vDateEnd  AS DATE           NO-UNDO.
   DEFINE VAR vResult   AS CHAR           NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain  :BUFFER-FIELD("PacketID") :BUFFER-VALUE
         vNote       = TRIM(hWOP:buffer-field("Description"):buffer-value)
      NO-ERROR. {&ON-ERROR}

      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         NO-ERROR.
      RUN PacketTextKeep(vPacketID,
                         "Информация о корректировке временного регламента функционирования ЦОиР:"  + "~n" +
                         GetNullStr(vNote),
                         INPUT-OUTPUT vResult).
      RUN PacketTextSave(vPacketID,vResult).

      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.

   IF v__UP <> 0 THEN 
      RUN InstanceJunk(hWOP, v__UP) NO-ERROR.
 
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Определение конечного статуса при квитовке документов                      */
/*----------------------------------------------------------------------------*/
FUNCTION ESIDStatusDefine CHAR (INPUT  hWOP     AS handle,
                                INPUT  iStatus  AS CHAR,
                                BUFFER op-entry FOR op-entry):
   DEFINE BUFFER bCode  FOR Code.
   DEFINE BUFFER sCode  FOR Code.
   DEFINE VAR vStatus   AS CHAR  NO-UNDO.
   DEFINE VAR vStatus206 AS CHAR  NO-UNDO.
   DEFINE VAR vStmtDir  AS CHAR  NO-UNDO.

   {profile ST101}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDStatusDefine","1 op-status:" + GetNullStr(op-entry.op-status) +
                                            " iStatus:" + GetNullStr(iStatus)            +
                                          " mStateErr:" + GetNullStr(mStateErr)).
      &ENDIF

      IF op-entry.op-status <= mStateErr THEN DO:
         vStatus = ?.
         LEAVE MAIN.
      END.

      IF hWOP:BUFFER-FIELD("mail-format"):BUFFER-VALUE BEGINS "XML-ED206" AND
         op-entry.op-status >= mStateChgd THEN DO:
         vStatus = ?.
         LEAVE MAIN.
      END.

      vStatus = iStatus.

      ASSIGN                                     /* Направление               */
         vStmtDir = hWOP:buffer-field("Direct"):buffer-value
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDStatusDefine","2 vStmtDir:" + GetNullStr(vStmtDir)).
      &ENDIF

      /*Установка статуса документам, квитующимся начальными ED206*/
      IF hWOP:BUFFER-FIELD("mail-format"):BUFFER-VALUE BEGINS "XML-ED206" AND
         vStmtDir =  "1" THEN DO:
         FOR EACH bCode WHERE
                  bCode.class  =  "МЦИСтат-Нач"
              AND bCode.parent =  "МЦИСтат-Нач"
              AND bCode.Code   =  op-entry.op-status
                  NO-LOCK,
             LAST sCode WHERE
                  sCode.class  =  "МЦИСтат-Нач"
              AND sCode.parent =  bCode.Code
              AND CAN-DO(sCode.val,op-entry.acct-cr)
                  NO-LOCK:
            vStatus206 = sCode.Code.
         END.
         IF {assigned vStatus206} THEN 
            vStatus = vStatus206.
         ELSE 
            vStatus = FGetSetting("УФЭБС","СтатусИспл",chr(251)).
      END.

      IF vStmtDir =  "2" THEN                    /* Ответные документы        */
         FOR EACH bCode WHERE
                  bCode.class  =  "МЦИСтатусы"
              AND bCode.parent =  "МЦИСтатусы"
              AND bCode.Code   =  iStatus
                  NO-LOCK,
             LAST sCode WHERE
                  sCode.class  =  "МЦИСтатусы"
              AND sCode.parent =  bCode.Code
              AND CAN-DO(sCode.val,op-entry.acct-cr)
                  NO-LOCK:
            vStatus = sCode.Code.
         END.
         
      IF vStmtDir =  "3" 
         AND hWOP:BUFFER-FIELD("mail-format"):BUFFER-VALUE BEGINS "XML-ED206" THEN                    /* Ответные документы        */
            vStatus = mStateDone.        
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDStatusDefine","5 vStatus:" + GetNullStr(vStatus)).
   &ENDIF

   {profile ST110}
   RETURN vStatus.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Формирование выписки для конвертации                                       */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConvertSTMT:
   DEFINE INPUT PARAMETER hWOP   AS handle   NO-UNDO.
   DEFINE INPUT PARAMETER hSTM   AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vDC       AS CHAR           NO-UNDO.
   {profile ST031}
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vDC = hWOP:buffer-field("Direct"):buffer-value
      NO-ERROR. {&ON-ERROR}

      RUN InstanceCreateEx(?, hSTM, 0).

      ASSIGN
         hSTM:buffer-field("SendDate"):buffer-value       = hWOP:buffer-field("RecvDate"):buffer-value
         hSTM:buffer-field("SendID"):buffer-value         = hWOP:buffer-field("RecvID"):buffer-value
         hSTM:buffer-field("SendREF"):buffer-value        = hWOP:buffer-field("RecvREF"):buffer-value
         hSTM:buffer-field("doc-num"):buffer-value        = hWOP:buffer-field("doc-num"):buffer-value
         hSTM:buffer-field("doc-type"):buffer-value       = hWOP:buffer-field("doc-type"):buffer-value

         hSTM:buffer-field("bank-code"):buffer-value      = hWOP:buffer-field("bank-code-rec"):buffer-value

         hSTM:buffer-field("bank-corr-acct"):buffer-value = hWOP:buffer-field("bank-corr-acct-rec"):buffer-value
      NO-ERROR.

      IF vDC =  "1" OR
         vDC =  "Начальные"            /* исходный документ отсутствует */
      THEN ASSIGN
         hSTM:buffer-field("acct-clnt"):buffer-value      = hWOP:buffer-field("acct-send"):buffer-value
         hSTM:buffer-field("acct-corr"):buffer-value      = hWOP:buffer-field("acct-rec"):buffer-value
         hSTM:buffer-field("amt-db"):buffer-value         = hWOP:buffer-field("amt-rub"):buffer-value
      NO-ERROR.
      ELSE ASSIGN
         hSTM:buffer-field("acct-clnt"):buffer-value      = hWOP:buffer-field("acct-rec"):buffer-value
         hSTM:buffer-field("acct-corr"):buffer-value      = hWOP:buffer-field("acct-send"):buffer-value
         hSTM:buffer-field("amt-cr"):buffer-value         = hWOP:buffer-field("amt-rub"):buffer-value
      NO-ERROR.
      {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject(hSTM).
      &ENDIF

      vFlagSet = YES.
   END.
   {profile ST040}
   {doreturn.i vFlagSet}
END PROCEDURE.
/******************************************************************************/

PROCEDURE ESIDInitReport:
   DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
   DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

   DEFINE BUFFER bCode  FOR code.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hMain    AS HANDLE      NO-UNDO.
   DEFINE VAR vPacketID     AS INT64        NO-UNDO.
   DEFINE VAR vParentID     AS INT64        NO-UNDO.


MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExchTT,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hExchTT,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      mEDReport = "".
      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Проверка на ED222                                                          */
/*----------------------------------------------------------------------------*/
FUNCTION IsED222 RETURNS LOGICAL (INPUT iMailFormat AS CHAR):
   RETURN iMailFormat =  "XML-ED222".
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Получение текста сообщения по дате и референсу                             */
/*----------------------------------------------------------------------------*/
FUNCTION GetTTEDRep RETURNS CHAR (INPUT iEDRepDate AS CHAR,
                                  INPUT iEDRepRef  AS CHAR):
   FIND FIRST ttEDRep WHERE
              ttEDRep.Date =  iEDRepDate
          AND ttEDRep.Ref  =  iEDRepRef
   NO-LOCK NO-ERROR.
   IF AVAIL ttEDRep THEN
      RETURN ttEDRep.Mes.
   ELSE
      RETURN "".

END FUNCTION.

PROCEDURE ESIDCreateReport:
   DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
   DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

   DEFINE VARIABLE hExch     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vClass    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTitle    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFlagSet  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vValue    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRep      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParents  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPar      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vttDate   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vttRef    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMailF    AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bXAttr FOR Xattr.

MAIN:

   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CASE hExchTT:TYPE:
         WHEN "TEMP-TABLE" THEN
            hExch = hExchTT:DEFAULT-BUFFER-HANDLE.
         WHEN "BUFFER" THEN
            hExch = hExchTT.
      END CASE.
      vMailF = hExch:BUFFER-FIELD("mail-format"):BUFFER-VALUE.
      IF IsED222 (vMailF) THEN
         mEDReport = "".
      vPacketID = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE.

      FIND FIRST Class WHERE Class.Class-Code =  iFormat
         NO-LOCK NO-ERROR.
      IF AVAIL Class THEN
         vTitle = IF iFormat BEGINS "XML-ED" 
                     THEN CAPS (Class.Name)
                     ELSE Class.Name.

      vTitle = SUBSTRING (vTitle, INDEX (vTitle, ">") + 1).
      vRep = "~n" + vTitle + "~n~n".
      mEDReport = EDRepKeep (mEDReport, vRep, vPacketID).

      FOR EACH Xattr WHERE Xattr.Class-Code  =  iFormat AND
                           XAttr.Description =  "ATTRLIST"
         NO-LOCK, 
         EACH bXattr WHERE bXAttr.Class-Code =   Xattr.xattr-label AND
                           bXAttr.description =  "ATTR"
            NO-LOCK  
            BY Xattr.order BY bXAttr.order:

            ASSIGN
               vValue = STRING (hExch:BUFFER-FIELD(bXattr.xattr-clabel):BUFFER-VALUE)
               vRep   = bXAttr.Name + ":" + GetNullStr (vValue) + "~n" 
               mEDReport = EDRepKeep (mEDReport, vRep, vPacketID)
            NO-ERROR.

            IF IsED222 (vMailF) THEN
               CASE bXattr.XAttr-clabel:
                  WHEN "RecvDate" THEN
                     vttDate = vValue.
                  WHEN "RecvREF"  THEN 
                     vttRef  = vValue.
               END CASE.

      END.

      FOR EACH Xattr WHERE Xattr.Class-Code  =  iFormat AND
                           Xattr.Description =  "ATTR"
         NO-LOCK:

         ASSIGN
            vValue    = STRING (hExch:BUFFER-FIELD (Xattr.xattr-clabel):BUFFER-VALUE)
            vRep      = Xattr.Name + ":" + GetNullStr (vValue) + "~n" 
            mEDReport = EDRepKeep (mEDReport, vRep, vPacketID)
         NO-ERROR.
      END.
      IF IsED222(vMailF) THEN
      DO:
         CREATE ttEDRep.
         ASSIGN
            ttEDRep.Date = vttDate
            ttEDRep.Ref  = vttRef
            ttEDRep.Mes  = mEDReport
         NO-ERROR.
      END.

      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}

END PROCEDURE.


PROCEDURE ESIDConfirmReport:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR v__ID        AS INT64        NO-UNDO.
   DEFINE VAR v__UP        AS INT64        NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64      NO-UNDO.
   DEFINE VARIABLE vBuf      AS CHARACTER    NO-UNDO.
   DEFINE VARIABLE hExchRKC  AS HANDLE       NO-UNDO.
   DEFINE VARIABLE vMailF    AS CHARACTER    NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmReport","START").    
   &ENDIF
                                       /* в методе Initial теперь вызывается другая процедура */
   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.

   vMailF = hExch:BUFFER-FIELD("mail-format"):BUFFER-VALUE.
   IF NOT IsED222(vMailF) THEN
      RUN ESIDInitReport (iFormat,hExch) NO-ERROR.

   MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      vPacketID = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE.

      RUN ESIDCreateReport(iFormat,hExch) NO-ERROR. {&ON-ERROR}
      IF NOT IsED222(vMailF) THEN
         RUN PacketTextSave (vPacketID, mEDReport).

                                       /* Замена реквизитов для квитовки ED211 */ 

     IF iFormat =  "XML-ED211" THEN
         hExch:BUFFER-FIELD("SendID"):BUFFER-VALUE =
            hExch:BUFFER-FIELD("SendIDEsid"):BUFFER-VALUE.

      vBuf = hExch:BUFFER-FIELD("SendREF"):BUFFER-VALUE.
      hExch:BUFFER-FIELD("SendREF"):BUFFER-VALUE =
               hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE.
      hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE = vBuf.

      vBuf = hExch:BUFFER-FIELD("SendDate"):BUFFER-VALUE.
      hExch:BUFFER-FIELD("SendDate"):BUFFER-VALUE =
               hExch:BUFFER-FIELD("RecvDate"):BUFFER-VALUE.
      hExch:BUFFER-FIELD("RecvDate"):BUFFER-VALUE = vBuf.

      IF hExch:BUFFER-FIELD("DIRECT"):BUFFER-VALUE =  "1" THEN DO:
         vBuf = hExch:BUFFER-FIELD("bank-code-rec"):BUFFER-VALUE.
         hExch:BUFFER-FIELD("bank-code-rec"):BUFFER-VALUE = bank-mfo-9.
         hExch:BUFFER-FIELD("bank-code-send"):BUFFER-VALUE = vBuf.
         hExch:BUFFER-FIELD("bank-corr-acct-send"):BUFFER-VALUE =
               hExch:BUFFER-FIELD("bank-corr-acct-rec"):BUFFER-VALUE.
         hExch:BUFFER-FIELD("bank-corr-acct-rec"):BUFFER-VALUE = "".
         hExch:BUFFER-FIELD("acct-send"):BUFFER-VALUE =
               hExch:BUFFER-FIELD("acct-rec"):BUFFER-VALUE.
         hExch:BUFFER-FIELD("acct-rec"):BUFFER-VALUE = "".
         
      END.

      vFlagSet = YES.
   END.
                                       /* экземпляр не удалаем, он нужен для создания документа */ 
   &IF DEFINED (IS-DEBUG) &THEN
      RUN DumpObject(hExch).
      RUN dbgprint.p ("ESIDConfirmReport","END").    
   &ENDIF

   {doreturn.i vFlagSet}

END.

FUNCTION EDRepKeep RETURNS CHARACTER (INPUT iEDReport AS CHARACTER,
                                      INPUT iAddTxt   AS CHARACTER,
                                      INPUT iPacketID AS INT64):

   DEFINE VARIABLE vRes AS CHARACTER   NO-UNDO.

   IF LENGTH (iEDReport) + LENGTH (iAddTxt) >  {&REP-LIMIT} THEN
   DO:
      RUN PacketTextSave (iPacketID, iEDReport).
      vRes = iAddTxt.
   END.
   ELSE
      vRes = iEDReport + iAddTxt.
   RETURN vRes.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Импорт Информация об установленных лимитах                                 */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmLimit:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR  hQuery           AS handle         NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vParentID         AS INT64        NO-UNDO.
   DEFINE VAR v__ID             AS INT64        NO-UNDO.
   DEFINE VAR v__UP             AS INT64        NO-UNDO.
   DEFINE VAR vcPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vResult           AS CHAR           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vFlagFind         AS LOGICAL        NO-UNDO.
   DEFINE VAR vFlagFindLim      AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorClass       AS CHAR           NO-UNDO.
   DEFINE VAR vErrorList        AS CHAR           NO-UNDO.
   DEFINE VAR vFake             AS CHAR           NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.



MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}



      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWOP) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vClassRef   = bcode.misc[4]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hWOP:buffer-field("RecvDate"):buffer-value
      NO-ERROR. {&ON-ERROR}
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN DO:
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         .
      END.
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.
      {empty ttLim}

      IF PAcket.mail-format =  "XML-ED332" THEN DO:

         CASE ExchQueryOpen (INPUT  hWOP,
                             INPUT  "FOR EACH " + hWOP:Name + " WHERE "    +
                                                  hWOP:Name + ".__ID GT 0" +
                                         " BY " + hWOP:Name + ".__ID",
                             OUTPUT hQuery):
            WHEN ?   THEN UNDO MAIN, RETRY MAIN.
            WHEN NO  THEN.
            WHEN YES THEN DO WHILE NOT hQuery:query-off-end:
             
               CREATE ttLim.
               ASSIGN  
                  ttLim.EDDate              = hWOP:buffer-field("SendDate"):buffer-value 
                  ttLim.EndTime             = hWOP:buffer-field("EndTime"):buffer-value
                  ttLim.LiquiditySum        = hWOP:buffer-field("LiquiditySum"):buffer-value       
                  ttLim.Debt                = hWOP:buffer-field("Debt"):buffer-value      
                  ttLim.OutBal              = hWOP:buffer-field("OutBal"):buffer-value      
                  ttLim.CreditLimitSum      = hWOP:buffer-field("LimitSum"):buffer-value      
                  ttLim.ReservedSum         = hWOP:buffer-field("ReservedSum"):buffer-value     
                  ttLim.RTGSOutBal          = hWOP:buffer-field("RTGSOutBal"):buffer-value     
                  ttLim.RTGSReservedSum     = hWOP:buffer-field("RTGSReservedSum"):buffer-value 
                  ttLim.TUTurnPayment       = hWOP:buffer-field("TUTurnPayment"):buffer-value 
                  ttLim.RTGSTurnPayment     = hWOP:buffer-field("RTGSTurnPayment"):buffer-value 
                  ttLim.EstimatedIncomeSum  = hWOP:buffer-field("EstimatedIncSum"):buffer-value 

               NO-ERROR.
                  
               hQuery:get-next().
            END.
         END CASE.
         hQuery:query-close().
   
         vResult = "ТАБЛИЦА РЕКВИЗИТОВ СООБЩЕНИЯ" +  '~n~n'.
         FOR EACH ttLim:
             vResult = vResult +     
                       "EDDate:             " + GetNullStr(STRING(ttLim.EDDate)) + '~n'   +
                       "EndTime:            " + GetNullStr(STRING(ttLim.EndTime)) + '~n'  +
                       "LiquiditySum:       " + GetNullStr(STRING(ttLim.LiquiditySum)) + '~n'   +
                       "Debt:               " + GetNullStr(STRING(ttLim.Debt)) + '~n'   +
                       "OutBal:             " + GetNullStr(STRING(ttLim.OutBal)) + '~n'   +
                       "CreditLimitSum:     " + GetNullStr(STRING(ttLim.CreditLimitSum)) + '~n'   +
                       "ReservedSum:        " + GetNullStr(STRING(ttLim.ReservedSum)) + '~n'   +
                       "RTGSOutBal:         " + GetNullStr(STRING(ttLim.RTGSOutBal)) + '~n'   +
                       "RTGSReservedSum:    " + GetNullStr(STRING(ttLim.RTGSReservedSum)) + '~n'   +
                       "TUTurnPayment:      " + GetNullStr(STRING(ttLim.TUTurnPayment)) + '~n'   +
                       "RTGSTurnPayment:    " + GetNullStr(STRING(ttLim.RTGSTurnPayment)) + '~n'   +
                       "EstimatedIncomeSum: " + GetNullStr(STRING(ttLim.EstimatedIncomeSum)) + '~n'   .
         END.

      END.

      ELSE DO:
         CASE ExchQueryOpen (INPUT  hWOP,
                             INPUT  "FOR EACH " + hWOP:Name + " WHERE "    +
                                                  hWOP:Name + ".__ID GT 0" +
                                         " BY " + hWOP:Name + ".__ID",
                             OUTPUT hQuery):
            WHEN ?   THEN UNDO MAIN, RETRY MAIN.
            WHEN NO  THEN.
            WHEN YES THEN DO WHILE NOT hQuery:query-off-end:
               CREATE ttLim.
               ASSIGN 
                  ttLim.LimitTransKind = '"' + hWOP:buffer-field("LimTransKind"):buffer-value + '"'  
                  ttLim.LimitSum       = hWOP:buffer-field("LimitSum"):buffer-value 
                  ttLim.PURBIC         = hWOP:buffer-field("PURBIC"):buffer-value    
               NO-ERROR.                  
               hQuery:get-next().
            END.
         END CASE.
         hQuery:query-close().
   
         vResult = "ТАБЛИЦА РЕКВИЗИТОВ СООБЩЕНИЯ" +  '~n~n'.
         FOR EACH ttLim:
             vResult = vResult +     
                       "Бик:         " + GetNullStr(ttLim.PURBIC)           + '~n'   +
                       "Вид лимита:  " + GetNullStr(ttLim.LimitTransKind)   + '~n'   +
                       "Сумма лимита:" + GetNullStr(STRING(ttLim.LimitSum)) + '~n'.
   
         END.
      END.

      vResult = vResult + FILL("-",76) + '~n~n'.    

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmLimit"," iFormat:"          + GetNullStr(iFormat)        +
                                         " hWOP:name:"        + GetNullStr(hWOP:name)      +
                                         " vClassRef:"        + GetNullStr(vClassRef)      +
                                         " vRecvRef:"         + GetNullStr(vRecvRef)       +
                                         " vRecvDate:"        + STRING(vRecvDate)          +
                                         " bCode.Description[1]):" + ENTRY(2,bCode.Description[1])).
      &ENDIF

      vFlagFind = NO.
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH  PacketText WHERE
                PacketText.PacketID =  Packet.PacketID NO-LOCK:
         ASSIGN
            vcPacketID       = Packet.PacketID
         NO-ERROR.

         vFlagFind = YES.
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDConfirmLimit","PacketText.Contents:" + PacketText.Contents).
         &ENDIF

         vResult = vResult +
                   "НАЙДЕНО СООБЩЕНИЕ:"                           + '~n'   +
                   "Код класса:    " + Packet.Class-Code          + '~n'   +
                   "Формат обмена: " + Packet.Mail-Format         + '~n'   + 
                   "Дата:          " + STRING(Reference.op-date)  + '~n'   +
                   "Cсылка:        " + Reference.RefValue         + '~n~n'.

         vFlagFindLim = NO.
         FOR EACH ttLim:
            IF     INDEX(PacketText.Contents,"LimitTransKind")  <> 0
               AND INDEX(PacketText.Contents,ttLim.LimitTransKind)   <> 0
               AND (INDEX(PacketText.Contents,"LimitSum")       =  0 OR
                   (INDEX(PacketText.Contents,"LimitSum")       <> 0
               AND INDEX(PacketText.Contents,STRING(ttLim.LimitSum)) <> 0))
               AND (NOT {assigned ttLim.PURBIC} OR
                   (INDEX(PacketText.Contents,"PURBIC")         <> 0
               AND  INDEX(PacketText.Contents,ttLim.PURBIC)          <> 0))
            THEN DO:
               vResult = vResult + '~n'   +
                         "LimitTransKind=" +  ttLim.LimitTransKind   + '~n'  +
                         "LimitSum="       + '"' + STRING(ttLim.LimitSum) + '~n'  +
                         "PURBIC="         + '"' + ttLim.PURBIC           + '"~n'.
            vFlagFindLim = YES.
            END.
         END.
         IF NOT vFlagFindLim THEN DO:
            vResult = vResult + '~n'       +
                      "НЕ СОВПАЛИ ДАННЫЕ:"  + '~n'.
            FOR EACH ttLim:
               IF     INDEX(PacketText.Contents,"LimitTransKind")       <> 0
                  AND INDEX(PacketText.Contents,ttLim.LimitTransKind)   =  0
                      THEN vResult = vResult + '~n' +
                           "LimitTransKind=" + ttLim.LimitTransKind   + '~n'.
               IF ttLim.LimitSum <> 0
                  AND INDEX(PacketText.Contents,"LimitSum")             <> 0
                  AND INDEX(PacketText.Contents,STRING(ttLim.LimitSum)) =  0
                      THEN vResult = vResult + '~n' +
                           "LimitSum="       + '"' + STRING(ttLim.LimitSum) + '"~n'.
               IF {assigned ttLim.PURBIC} 
                  AND INDEX(PacketText.Contents,"PURBIC")               <> 0
                  AND INDEX(PacketText.Contents,ttLim.PURBIC)           =  0
                      THEN vResult = vResult + '~n' + 
                           "PURBIC="         + '"' + ttLim.PURBIC           + '"~n'.
               END.
            END.
         vResult =  vResult + "~n" +  PacketText.Contents.
   
         &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("ESIDConfirmLimit","vResult:" + vResult).
         &ENDIF
 
    
         RUN PacketCreateLink  (INPUT  vcPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         RUN PacketSetState (vcPacketID,{&STATE-FIN}).
      END.

      RUN PacketTextSave(vPacketID,vResult).

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      IF NOT vFlagFind THEN DO:
         RUN PacketSetState (vPacketID,{&STATE-ERR}).

         ASSIGN
            vErrorClass = "ErrorEsid"
            vErrorList  = "b8060"
         NO-ERROR.

         RUN PacketSetError (INPUT  vPacketID,
                             INPUT  vErrorClass,
                             INPUT  vErrorList,
                             OUTPUT vFake) NO-ERROR.
      END.
      {empty ttLim}
      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   RUN InstanceDelete(hWOP).
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт подтверждения обработки распоряжений на управление очередности ED385*/
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmOpEntryType:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO. /* ExchRKCDoc */

   DEFINE VAR mHLimSrv              AS handle   NO-UNDO.
   DEFINE VAR vFlagSet              AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain                 AS handle   NO-UNDO.
   DEFINE VAR vPacketID             AS INT64  NO-UNDO.
   DEFINE VAR vcPacketID            AS INT64  NO-UNDO.
   DEFINE VAR vFlagFind             AS LOGICAL  NO-UNDO.
   DEFINE VAR vResult               AS CHAR     NO-UNDO.
   DEFINE VAR vSendRef              AS CHAR     NO-UNDO.
   DEFINE VAR vSendDate             AS DATE     NO-UNDO.
   DEFINE VAR vErrorClass           AS CHAR     NO-UNDO.
   DEFINE VAR vErrorList            AS CHAR     NO-UNDO.
   DEFINE VAR vClassRef             AS CHAR     NO-UNDO.
   DEFINE VAR vEsidPacketID         AS INT64  NO-UNDO.
   DEFINE VAR vESIDType             AS CHAR     NO-UNDO.
   DEFINE VAR vESIDAuthor           AS CHAR     NO-UNDO.
   DEFINE VAR vEDAuthor             AS CHAR     NO-UNDO.
   DEFINE VAR vESIDDate             AS CHAR     NO-UNDO.
   DEFINE VAR vEDDate               AS CHAR     NO-UNDO.
   DEFINE VAR vESIDNo               AS CHAR     NO-UNDO.
   DEFINE VAR vEDNo                 AS CHAR     NO-UNDO.
   DEFINE VAR vText                 AS CHAR     NO-UNDO.
   DEFINE VAR vI                    AS INT64  NO-UNDO.
   DEFINE VAR vPayPrior             AS CHAR     NO-UNDO.
   DEFINE VAR vOpEntryType          AS CHAR     NO-UNDO.
   DEFINE VAR vFake                 AS CHAR     NO-UNDO.

   DEFINE VARIABLE vInitClRef AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
      ASSIGN
         mHLimSrv = ObjectValueHandle("ExchBSPSrvLim").

      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWop) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vClassRef   = bcode.misc[4]  /* класс ссылки полученного ЭСИД */
         vInitClRef  = bCode.misc[3]  /* класс ссылки исходного   ЭСИД */ 
         vSendRef    = ReferenceFormatValue(vClassRef,
                                            hMain:buffer-field("SendRef"):buffer-value) 
         vSendDate   = hMain:buffer-field("SendDate"):buffer-value
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmOpEntryType",
                      " vPacketID:" + GetNullInt(vPacketID)  +
                      " vClassRef:" + GetNullStr(vClassRef)  +
                     " vInitClRef:" + GetNullStr(vInitClRef) +
                      " vSendRef:"  + GetNullStr(vSendRef)   +
                      " vSendDate:" + GetNullDat(vSendDate)).
      &ENDIF

      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN DO:
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         .
      END.
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.

/*------------------------------------ Поиск исходного ED382, ED383 ----------*/

      vFlagFind = NO.
      FOR FIRST Reference WHERE
                Reference.op-date    =  vSendDate
            AND Reference.Class-Code =  vInitClRef
            AND Reference.RefValue   =  vSendRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                EXCLUSIVE-LOCK,
          EACH  PacketText WHERE
                PacketText.PacketID  =  Packet.PacketID NO-LOCK,
          FIRST PackObject WHERE
                PackObject.PacketID  =  Packet.PacketID
            AND PackObject.File-Name =  "op-entry"
                NO-LOCK,
           FIRST op WHERE
                 op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                 NO-LOCK,
           FIRST op-entry OF op EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

         ASSIGN
            vEsidPacketID = Packet.PacketID
         NO-ERROR.
         ASSIGN
            vFlagFind   = YES
            vText       = REPLACE(PacketText.Contents,"~n","")
            vText       = REPLACE(PacketText.Contents,'"' ,"")
         .
         vESIDType   = "ED" + GetXMLTag("<ED",      3,vText).
         vESIDAuthor = GetXMLTag("EDAuthor=",      10,vText).
         vESIDDate   = GetXMLTag("EDDate=",        10,vText).
         vESIDNo     = GetXMLTag("EDNo=",           6,vText).
         vPayPrior   = GetXMLTag("PaymentPrioryty=",1,vText).
         vEDAuthor   = GetXMLTag("EDAuthor=",      10,vText).
         vEDDate     = GetXMLTag("EDDate=",        10,vText).
         vEDNo       = GetXMLTag("EDNo=",           6,vText).

         RUN PacketCreateLink (Packet.PacketID, "Packet", STRING (vPacketID), ENTRY(2,bCode.Description[1])).


         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDConfirmOpEntryType",
                         " vESIDType:"   + GetNullStr(vESIDType)   +  
                         " vESIDAuthor:" + GetNullStr(vESIDAuthor) + " " +  hMain:buffer-field("SendID"):buffer-value   +      
                         " vESIDDate:"   + GetNullStr(vESIDDate)   + " " +  hMain:buffer-field("SendDate"):buffer-value +     
                         " vESIDNo:"     + GetNullStr(vESIDNo)     + " " +  hMain:buffer-field("SendRef"):buffer-value  +
                         " vEDAuthor:"   + GetNullStr(vEDAuthor)   + " " +  hWop:buffer-field("SendID"):buffer-value    +      
                         " vEDDate:"     + GetNullStr(vEDDate)     + " " +  hWop:buffer-field("SendDate"):buffer-value  +      
                         " vEDNo:"       + GetNullStr(vEDNo)       + " " +  hWop:buffer-field("SendRef"):buffer-value ).
         &ENDIF

         IF     vESIDAuthor =   hMain:buffer-field("SendID"):buffer-value
            AND DateFromString(vESIDDate,"yyyy-mm-dd") =   DATE(hMain:buffer-field("SendDate"):buffer-value)
            AND vESIDNo     =   hMain:buffer-field("SendRef"):buffer-value
            AND vEDAuthor   =   hWop:buffer-field("SendID"):buffer-value
            AND DateFromString(vEDDate,"yyyy-mm-dd")   =   DATE(hWop:buffer-field("SendDate"):buffer-value)
            AND vEDNo       =   hWop:buffer-field("SendRef"):buffer-value
         THEN DO:

            CASE vESIDType:
               WHEN "ED382" THEN DO:
                  IF {assigned vPayPrior} THEN DO:
                     vOpEntryType = GetCode("BESP.Code382",vPayPrior).
                     IF NOT AvailCode("Маршруты",vOpEntryType) THEN DO:
                        RUN Fill-SysMes("","","","%s=ТП ~"" + vOpEntryType +
                                        "~"нет в классификаторе" +
                                        '"' + "Маршруты." + '"').
                        UNDO MAIN, RETRY MAIN.
                     END.
                     ELSE 
                        ASSIGN
                          op-entry.type = vOpEntryType.
                  END.
               END.
               WHEN "ED383" THEN DO:
               END.
               OTHERWISE DO:
                  RUN Fill-SysMes("","","","%s=Сообщение вида " + vESIDType +
                                           " не обрабатываются при импорте ED385!").
                  UNDO MAIN, RETRY MAIN.
               END.
            END CASE.

            Packet.State = {&STATE-FIN}.      

            RUN PacketCreateLink (vPacketID,
                                  PackObject.File-Name,
                                  PackObject.Surrogate,
                                  ENTRY(2,bCode.Description[1])).

         END.
         ELSE DO:
            CheckXMLTag("ESIDAuthor",
                        vESIDAuthor,
                        "SendID",
                        hMain:buffer-field("SendID"):buffer-value).

            CheckXMLTag("ESIDDate",
                        STRING(DateFromString(vESIDDate,"yyyy-mm-dd")),
                        "SendDate",
                        STRING(DATE(hMain:buffer-field("SendDate"):buffer-value))).

            CheckXMLTag("ESIDNo",
                        vESIDNo,
                        "SendRef",
                        hMain:buffer-field("SendRef"):buffer-value).

            CheckXMLTag("EDAuthor",
                        vEDAuthor,
                        "SendID", 
                        hWop:buffer-field("SendID"):buffer-value).

            CheckXMLTag("EDDate",
                        STRING(DateFromString(vEDDate,"yyyy-mm-dd")),
                        "SendDate",
                        STRING(DATE(hWop:buffer-field("SendDate"):buffer-value))).

            CheckXMLTag("EDDate",
                        STRING(DateFromString(vEDDate,"yyyy-mm-dd")),
                        "SendDate",
                        STRING(DATE(hWop:buffer-field("SendDate"):buffer-value))).
         END.
      END.
      IF NOT vFlagFind THEN DO:
         RUN Fill-SysMes("","","","Не найдено сообщение по реквизитам " +
                                  " Дата:"         + STRING(vSendDate)  +
                                  " Класс ссылки:" + vInitClRef         +
                                  " Ссылка:"       + vSendRef).

         RUN PacketSetState (vPacketID,{&STATE-ERR}).

         ASSIGN
            vErrorClass = "ErrorEsid"
            vErrorList  = "b8060"
         NO-ERROR.

         RUN PacketSetError (INPUT  vPacketID,
                             INPUT  vErrorClass,
                             INPUT  vErrorList,
                             OUTPUT vFake) NO-ERROR.

      END.

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      vFlagSet = YES.
   END.
   RUN InstanceDelete(hWOP).
   {doreturn.i vFlagSet}

END PROCEDURE.
/******************************************************************************/
/* Возвращает значение тэга из xml-строки и удалает тэг из строки             */
/******************************************************************************/
FUNCTION GetXMLTag CHAR (INPUT         iTag   AS CHAR,
                         INPUT         iLen   AS INT64,
                         INPUT-OUTPUT  iText  AS CHAR):

   DEFINE VAR oTegVal                  AS CHAR NO-UNDO.
   DEFINE VAR vI                       AS INT64  NO-UNDO.

   oTegVal = "".
   vI = INDEX(iText,iTag).
   IF vI <> 0 THEN DO:
      ASSIGN
         oTegVal = SUBSTRING(iText,vI + LENGTH(iTag),  iLen)
         iText   = SUBSTRING(iText,vI + LENGTH(iTag) + iLen)  
      .
   END.
   RETURN oTegVal.
END FUNCTION.
/******************************************************************************/
/* Сравнение тэгов и печать протокола о несопадении                           */
/******************************************************************************/
FUNCTION CheckXMLTag CHAR (INPUT       iTag1   AS CHAR,
                           INPUT       iVal1   AS CHAR,
                           INPUT       iTag2   AS CHAR,
                           INPUT       iVal2   AS CHAR):
  IF iVal1 <> iVal2 THEN
     RUN Fill-SysMes("","","","Не совпали реквизиты "  +
                                      iTag1 + ":" + iVal1 +
                              " и " + iTag2 + ":" + iVal2 + "!").
END FUNCTION.

/******************************************************************************/
/*                 ED374 - Информация об участниках БЭСП                      */
/******************************************************************************/
PROCEDURE ESIDInitialPARTIC.
DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

DEFINE BUFFER bCode  FOR code.

DEFINE VARIABLE vFlagSet    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vPacketID   AS INT64     NO-UNDO.
DEFINE VARIABLE vSeanceDate AS DATE        NO-UNDO.
DEFINE VARIABLE vEdDate     AS DATE        NO-UNDO.
DEFINE VARIABLE vErrCls     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hMain       AS HANDLE      NO-UNDO.
DEFINE VARIABLE vFormat     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vSendREF    AS INT64     NO-UNDO.
DEFINE VARIABLE vClassRef   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vSendID     AS CHARACTER   NO-UNDO.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDInitialPart","START").    
&ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   RUN XRKCPacketCheck IN h_xrkc (iFormat, hExchTT) NO-ERROR. {&ON-ERROR}

/*----------------------------------------- Регистрация полученного сообщения */
   RUN ESIDGetMain       (INPUT-OUTPUT hExchTT,
                          OUTPUT       hMain).

   ASSIGN
      vFormat     = hExchTT:BUFFER-FIELD("mail-format"):BUFFER-VALUE
      vPacketID   = hMain  :BUFFER-FIELD("PacketID")   :BUFFER-VALUE
      vSeanceDate = hExchTT:BUFFER-FIELD("SeanceDate") :BUFFER-VALUE
      vEDDate     = hExchTT:BUFFER-FIELD("SendDate")   :BUFFER-VALUE
      vClassRef   = bCode.misc[5]
      vSendID     = hExchTT:BUFFER-FIELD("SendID")     :BUFFER-VALUE
      vErrCls     = hExchTT:BUFFER-FIELD("ErrorClass") :BUFFER-VALUE
   NO-ERROR. {&ON-ERROR}

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDInitialPart","hExchTT" + hExchTT:name).    
      RUN dbgprint.p ("ESIDInitialPart","vEDDate" + STRING(vEDDate)).    
      RUN dbgprint.p ("ESIDInitialPart","vSeanceDate" + STRING(vSeanceDate)).
   &ENDIF

   FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-ERROR.
   IF AVAIL Packet THEN
      Packet.mail-format = vFormat.
/*-------------------------------------------------------------------- Ссылка */
   RUN PacketCreateRef IN h_rfrnc (vEDDate, vPacketID, vClassRef,
                                   vSendID + "|" + 
                                   ReferenceFormatValue(vClassRef,hExchTT:BUFFER-FIELD("SendREF"):BUFFER-VALUE)).
/*------------------------------------------------------------- Проверка даты */
   IF vEdDate <  vSeanceDate THEN
   DO:
      RUN AddErrorFormat IN h_exch (hExchTT, "u8200").
      RUN Fill-SysMes IN h_tmess ("",
                                  "ExchBSP01",
                                  "","%s=ED374" +
                                  "%s=" + STRING(vEDDate) + 
                                  "%s=" + STRING(vSeanceDate) +
                                  "%s=" + IF GetXAttrInit (vErrCls, "u8200") =  "Ошибка" 
                                             THEN "Импорт не выполнен"
                                             ELSE "").
   END.
   ASSIGN
      hExchTT:BUFFER-FIELD("PacketID")  :BUFFER-VALUE = vPacketID
   NO-ERROR. {&ON-ERROR}

   vFlagSet = YES.
END.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDInitialPart","END").    
&ENDIF

{doreturn.i vFlagSet}

END PROCEDURE.

PROCEDURE ESIDConfirmPARTIC:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR v__ID        AS INT64        NO-UNDO.
   DEFINE VAR v__UP        AS INT64        NO-UNDO.

   DEFINE VARIABLE vPacketID   AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrClass   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrList    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vState      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInitPack   AS INT64     NO-UNDO.
   DEFINE VARIABLE vHasInitial AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vClassRef   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRecvRef    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRecvDate   AS DATE        NO-UNDO.  
   DEFINE VARIABLE vFound      AS LOGICAL     NO-UNDO.

   DEFINE BUFFER bCode FOR code.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPARTIC","START").    
   &ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.
   ASSIGN
      vPacketID   = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vHasInitial = (hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE <> 0) AND 
                    (hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE <> ?)
      /* класс ссылки исходного ЭСИД */
      vClassRef   = bCode.misc[4]              
      /* ссылка исходного ЭСИД */
      vRecvRef    = ReferenceFormatValue(vClassRef,hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE) 
      vRecvDate   = hExch:BUFFER-FIELD("RecvDate"):BUFFER-VALUE 
   .

   /* Поиск исходного ЭСИД */
   IF vHasInitial THEN
   DO:
      FOR FIRST Reference WHERE Reference.class-code =  vClassRef
                          AND   Reference.op-date    =  vRecvDate
                          AND   Reference.RefValue   =  vRecvRef
               NO-LOCK,
          FIRST Packet WHERE Packet.PacketID =  Reference.PacketID 
               NO-LOCK:
         vInitPack = Packet.PacketID.
         vFound = YES.
      END.
      IF vFound THEN
      DO:
         /* изменение статуса */
         RUN PacketSetState (vInitPack,{&STATE-FIN}).
         /* создание связи с исходным сообщением */
         RUN PacketCreateLink (vPacketID, "Packet", STRING(vInitPack), "ED374"). 
      END.
      ELSE
         RUN AddErrorFormat IN h_exch (hExch, "b8060").
   END.

   ASSIGN
      vErrClass   = hExch:BUFFER-FIELD("ErrorClass"):BUFFER-VALUE
      vErrList    = hExch:BUFFER-FIELD("ErrorList"):BUFFER-VALUE
   .
   RUN PacketSetError (vPacketID, vErrClass, vErrList, OUTPUT vState).

   IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
      RUN PacketBSPTextSave(vPacketID).

   vFlagSet = YES.
END. /* MAIN */
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPARTIC","END").    
   &ENDIF
   
   {doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDConfirmPayeeInfo:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBankID   AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vPayeeBIC      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vamt-total     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vamt-max       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUnlimFMMax    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUnlimFMTotal  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAccPayee      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBIC           AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bBanks     FOR banks.
   DEFINE BUFFER bBanksCode FOR banks-code.
   DEFINE BUFFER bBanksCorr FOR banks-corr.
   DEFINE BUFFER acct       FOR acct.

   DEFINE VARIABLE vTUCode          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMember          AS CHARACTER   NO-UNDO.
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPayeeInfo","START").    
   &ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.

   ASSIGN
      vPacketID        = hExch:BUFFER-FIELD("PacketID")       :BUFFER-VALUE
      vErrLst          = hExch:BUFFER-FIELD("ErrorList")      :BUFFER-VALUE
      vErrCls          = hExch:BUFFER-FIELD("ErrorClass")     :BUFFER-VALUE

      vPayeeBIC        = hExch:BUFFER-FIELD("PayeeBIC")    :BUFFER-VALUE
      vamt-total       = hExch:BUFFER-FIELD("amt-total")   :BUFFER-VALUE
      vamt-max         = hExch:BUFFER-FIELD("amt-max")     :BUFFER-VALUE
      vUnlimFMMax      = hExch:BUFFER-FIELD("UnlimFMMax")  :BUFFER-VALUE
      vUnlimFMTotal    = hExch:BUFFER-FIELD("UnlimFMTotal"):BUFFER-VALUE
      vAccPayee        = hExch:BUFFER-FIELD("AccPayee")    :BUFFER-VALUE
      vBIC             = hExch:BUFFER-FIELD("PUR-BIC"):BUFFER-VALUE
   .
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPayeeInfo","vBIC:" + GetNullStr(vBIC)).
   &ENDIF
   IF {assigned vBIC} AND vBIC =  FGetSetting("БанкМФО","","") THEN DO:
      /*занести запись в классификатор CreateREf */
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vPayeeBIC:" + GetNullStr(vPayeeBIC)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vAccPayee:" + GetNullStr(vAccPayee)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vUnlimFMMax:" + GetNullStr(vUnlimFMMax)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vamt-max:" + GetNullStr(vamt-max)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vUnlimFMTotal:" + GetNullStr(vUnlimFMTotal)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","vamt-total:" + GetNullStr(vamt-total)).
         RUN dbgprint.p ("ESIDConfirmPayeeInfo","mOpDate:" + String(mOpDate)).

      &ENDIF
      IF {assigned vPayeeBIC} OR {assigned vAccPayee} OR {assigned vUnlimFMMax} THEN
      DO:
         RUN CreateCriterialStringEx IN h_refer
             ("PayeeInfoMax", 
              vBIC + CHR(1) + vPayeeBIC + CHR(1) + vAccPayee + CHR(1) + vUnlimFMMax  + CHR(1) + 
              (IF vUnlimFMMax =  "1" THEN "0" ELSE vamt-max),
              mOpDate,
              "EDIT") NO-ERROR. {&ON-ERROR}
         IF  {assigned return-value} THEN {&ON-ERROR}
      END.
      IF {assigned vPayeeBIC} OR {assigned vAccPayee} OR {assigned vUnlimFMTotal} THEN
      DO:
         RUN CreateCriterialStringEx IN h_refer
             ("PayeeInfoTot", 
              vBIC + CHR(1) + vPayeeBIC + CHR(1) + vAccPayee + CHR(1) + vUnlimFMTotal  + CHR(1) + 
              (IF vUnlimFMTotal =  "1" THEN "0" ELSE vamt-total),
              mOpDate,
              "EDIT") NO-ERROR. {&ON-ERROR}
         IF {assigned return-value} THEN {&ON-ERROR}
      END.
   END.
   vFlagSet = YES.
END.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDConfirmPayeeInfo","END").    
&ENDIF

{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDConfirmPayeeAcc:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBankID   AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bBanks     FOR banks.
   DEFINE BUFFER bBanksCode FOR banks-code.
   DEFINE BUFFER bBanksCorr FOR banks-corr.
   DEFINE BUFFER acct       FOR acct.

   DEFINE VARIABLE vTUCode          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMember          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAccPayee      AS CHARACTER   NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPayeeAcc","START").    
   &ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.

   ASSIGN
      vPacketID        = hExch:BUFFER-FIELD("PacketID")       :BUFFER-VALUE
      vErrLst          = hExch:BUFFER-FIELD("ErrorList")      :BUFFER-VALUE
      vErrCls          = hExch:BUFFER-FIELD("ErrorClass")     :BUFFER-VALUE

      vAccPayee        = hExch:BUFFER-FIELD("AccPayee")    :BUFFER-VALUE

   .

   vFlagSet = YES.
END.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDConfirmPayeeAcc","END").    
&ENDIF

{doreturn.i vFlagSet}
END PROCEDURE.


PROCEDURE ESIDConfirmCUSTINFO:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBankID   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUpdFlag  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vLinked   AS LOGICAL     NO-UNDO.

   DEFINE BUFFER bBanks     FOR banks.
   DEFINE BUFFER bBanksCode FOR banks-code.
   DEFINE BUFFER bBanksCorr FOR banks-corr.
   DEFINE BUFFER acct       FOR acct.
   DEFINE BUFFER PackObject FOR PackObject.

   DEFINE VARIABLE vTUCode          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPart            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMember          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUIS             AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBIC             AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOURBIC          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOCBIC           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegMode         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegdate         AS DATE        NO-UNDO.
   DEFINE VARIABLE vExcepDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vDiscMode        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDiscDate        AS DATE        NO-UNDO.
   DEFINE VARIABLE vConDate         AS DATE        NO-UNDO.
   DEFINE VARIABLE vStopMode        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUrgent          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCross           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSuspMode        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSWIFTMode       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLimMode         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClearList       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hFld             AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vI               AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrStr          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStoppageReason  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vReasonAddText   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStoppageDate    AS DATE        NO-UNDO.
   DEFINE VARIABLE vStoppageEndDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vRTGSLiqBfOD     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRTGSLiqInOD     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRTGSPayeePayts  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRTGSDispPayeePayts AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRTGSForeignPayts   AS CHARACTER   NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmCUSTINFO","START").    
   &ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.

   ASSIGN
      vPacketID        = hExch:BUFFER-FIELD("PacketID")       :BUFFER-VALUE
      vErrLst          = hExch:BUFFER-FIELD("ErrorList")      :BUFFER-VALUE
      vErrCls          = hExch:BUFFER-FIELD("ErrorClass")     :BUFFER-VALUE
      vTUCode          = hExch:BUFFER-FIELD("TUCode")         :BUFFER-VALUE
      vPart            = hExch:BUFFER-FIELD("Participat")     :BUFFER-VALUE
      vMember          = hExch:BUFFER-FIELD("MemberType")     :BUFFER-VALUE
      vAcct            = hExch:BUFFER-FIELD("Acct")           :BUFFER-VALUE
      vUIS             = hExch:BUFFER-FIELD("UIS")            :BUFFER-VALUE
      vBIC             = hExch:BUFFER-FIELD("PUR-BIC")        :BUFFER-VALUE
      vOURBIC          = hExch:BUFFER-FIELD("OUR-BIC")        :BUFFER-VALUE
      vOCBIC           = hExch:BUFFER-FIELD("OCBIC")          :BUFFER-VALUE
      vRegMode         = hExch:BUFFER-FIELD("RegMode")        :BUFFER-VALUE
      vRegDate         = hExch:BUFFER-FIELD("RegDate")        :BUFFER-VALUE
      vExcepDate       = hExch:BUFFER-FIELD("ExceptDate")     :BUFFER-VALUE
      vDiscMode        = hExch:BUFFER-FIELD("DisconMode")     :BUFFER-VALUE
      vDiscDate        = hExch:BUFFER-FIELD("DisconDate")     :BUFFER-VALUE
      vConDate         = hExch:BUFFER-FIELD("ConnDate")       :BUFFER-VALUE
      vStopMode        = hExch:BUFFER-FIELD("StopMode")       :BUFFER-VALUE
      vUrgent          = hExch:BUFFER-FIELD("UrgentPays")     :BUFFER-VALUE
      vSuspMode        = hExch:BUFFER-FIELD("SuspMode")       :BUFFER-VALUE
      vSWIFTMode       = hExch:BUFFER-FIELD("SWIFTMode")      :BUFFER-VALUE
      vLimMode         = hExch:BUFFER-FIELD("LimitMode")      :BUFFER-VALUE
      vStoppageReason  = hExch:BUFFER-FIELD("StoppageReason") :BUFFER-VALUE
      vReasonAddText   = hExch:BUFFER-FIELD("ReasonAddText")  :BUFFER-VALUE
      vStoppageDate    = hExch:BUFFER-FIELD("StoppageDate")   :BUFFER-VALUE
      vStoppageEndDate = hExch:BUFFER-FIELD("StoppageEndDate"):BUFFER-VALUE
      vRTGSLiqBfOD     = hExch:BUFFER-FIELD("RTGSLiqBfOD")    :BUFFER-VALUE
      vRTGSLiqInOD     = hExch:BUFFER-FIELD("RTGSLiqInOD")    :BUFFER-VALUE
      vRTGSPayeePayts     = hExch:BUFFER-FIELD("RTGSPayeePayts") :BUFFER-VALUE
      vRTGSDispPayeePayts = hExch:BUFFER-FIELD("RTGSDispPayeePayts") :BUFFER-VALUE
      vRTGSForeignPayts   = hExch:BUFFER-FIELD("RTGSForeignPayts") :BUFFER-VALUE
   .  
/*------------------------------------------------------------------ Проверки */
   IF CAN-DO (vErrLst, "u8200") AND GetXAttrInit (vErrCls, "u8200") =  "Ошибка" THEN
   DO:
      vFlagSet = YES.
      LEAVE MAIN.
   END.
   CASE vMember:
      WHEN "1" THEN     /* АУР */
      DO:
         IF {assigned vBIC} THEN DO:
            RUN GetBank IN h_base (BUFFER bBanks,
                                   BUFFER bBanksCode,
                                   vBIC,
                                   "МФО-9",
                                   NO).
         END.
         ELSE DO:
            FOR EACH bBanksCorr WHERE bBanksCorr.corr-acct =  vAcct
               NO-LOCK,
               FIRST bBanks WHERE bBanks.bank-id = bBanksCorr.bank-id 
                            AND   bBanks.flag-rkc                 
                  NO-LOCK,
               FIRST bBanksCode WHERE bBanksCode.bank-id        =  bBanks.bank-id 
                                AND   bBanksCode.bank-code-type =  "МФО-9"
                  NO-LOCK:

               IF SUBSTRING (bBanksCode.bank-code, 3, 2) =  vTUCode THEN
               DO:
                  vBankID = STRING (bBanksCorr.bank-corr).
                  LEAVE.
               END.
            END.
            IF {assigned vBankID} THEN
               FIND FIRST bBanks WHERE bBanks.bank-id =  INT64 (vBankID)
                  NO-LOCK NO-ERROR.   
         END.
      END.
      WHEN "2" THEN     /* ПУР */
         RUN GetBank IN h_base (BUFFER bBanks,
                                BUFFER bBanksCode,
                                vBIC,
                                "МФО-9",
                                NO).
      WHEN "3" THEN     /* ОУР */
         RUN GetBank IN h_base (BUFFER bBanks,
                                BUFFER bBanksCode,
                                vOURBIC,
                                "МФО-9",
                                NO).
      WHEN "6" THEN     /* ОУР-ФК */
         RUN GetBank IN h_base (BUFFER bBanks,
                                BUFFER bBanksCode,
                                vOURBIC,
                                "МФО-9",
                                NO).
      OTHERWISE .
   END CASE.

   IF NOT AVAIL bBanks THEN
   DO:
      IF vMember =  "1" THEN
         vErrStr = "UIS " + vUIS.
      ELSE IF vMember =  "2" THEN
         vErrStr = "BIC " + vBIC.
      ELSE
         vErrStr = "BIC " + vOURBIC.
      IF vErrStr =  ? THEN vErrStr = "ни одному реквизиту".
      RUN Fill-SysMes IN h_tmess ("","","0","ED374: Не найден банк по " + vErrStr + ".").
      vFlagSet = YES.
      LEAVE MAIN.
   END.

   vBankID = STRING (bBanks.bank-id).
/*-------------------------------------------------------------------- Импорт */

   IF vMember =  "1" AND NOT {assigned vBIC} THEN
   DO:
      FIND FIRST bBanksCode WHERE bBanksCode.bank-id        =  INT64 (vBankID) 
                            AND   bBanksCode.bank-code-type =  "UIS"
                                    
         NO-ERROR.
      IF AVAIL bBanksCode AND {assigned vUIS} THEN
      DO:
         IF vUIS <> bBanksCode.bank-code THEN
         DO:      
            RUN Fill-SysMes IN h_tmess ("","","0","Банк " + vBankID + ": Код UIS АУР (" + vUIS + ")" + 
                                                  " не совпадает с найденным в БД (" + 
                                                  bBanksCode.bank-code + "). Заменен.").
            ASSIGN 
               bBanksCode.bank-code = vUIS
            NO-ERROR. {&ON-ERROR}
         END.
      END.
      ELSE IF {assigned vUIS} THEN
      DO:
         FIND FIRST bBanksCode WHERE bBanksCode.bank-code-type =  "UIS" AND
                                     bBanksCode.bank-code      =  vUIS
         NO-ERROR.
         IF AVAIL bBanksCode  THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","0","Уже есть запись banks-code c UIS " + vUIS).
            UNDO MAIN, RETRY MAIN.
         END.
         CREATE bBanksCode.
         ASSIGN
            bBanksCode.bank-id        = INT64 (vBankID)
            bBanksCode.bank-code-type = "UIS"
            bBanksCode.bank-code      = vUIS
         NO-ERROR.  {&ON-ERROR} 
      END.
   END.

   vUpdFlag = YES.
   vLinked = CAN-FIND (FIRST PackObject WHERE 
                             PackObject.PacketID  = vPacketID AND
                             PackObject.file-name = "banks"   AND
                             PackObject.Surrogate = vBankId   AND
                             PackObject.Kind      = "ED374"
                       NO-LOCK).

   IF vMember = "6" THEN DO:
      IF GetXAttrValueEx ("banks", vBankId, "RTGSMemberType", "") = "3" AND 
         vLinked THEN
         vUpdFlag = NO.
   END.

   IF vUpdFlag THEN DO:
   
      UpdateSigns ("banks", vBankId, "RTGSMemberType", vMember, ?). 

      IF {assigned vRTGSForeignPayts} THEN
         UpdateSigns ("banks", vBankId, "RTGSForeignPayts", vRTGSForeignPayts, ?). 
   
   IF {assigned vPart} THEN
      UpdateSigns ("banks", vBankId, "RTGSParticip", vPart, ?).
   IF {assigned vOCBIC} THEN
      UpdateSigns ("banks", vBankId, "RTGSOCBIC", vOCBIC, ?).
   IF {assigned vRegMode} THEN                                /* CustomerInfo */
     UpdateSigns ("banks", vBankId, "RTGSRegMode", vRegMode, ?).
      IF vRegDate <> ? THEN
      UpdateSigns ("banks", vBankId, "RTGSRegDate", STRING (vRegDate,"99/99/9999"), ?).
      IF vExcepDate <> ? THEN
      UpdateSigns ("banks", vBankId, "RTGSExcepDate", STRING (vExcepDate,"99/99/9999"), ?).
   IF {assigned vDiscMode} THEN
      UpdateSigns ("banks", vBankId, "RTGSDiscMode", vDiscMode, ?).
      IF vDiscDate <> ? AND vConDate <> ? THEN
   DO:
      UpdateSigns ("banks", vBankId, "RTGSDiscDate", STRING (vDiscDate,"99/99/9999"), ?).
      UpdateSigns ("banks", vBankId, "RTGSConDate", STRING (vConDate,"99/99/9999"), ?).
   END.
      ELSE IF vDiscDate <> ? THEN
   DO:
      UpdateSigns ("banks", vBankId, "RTGSDiscDate", STRING (vDiscDate,"99/99/9999"), ?).
      UpdateSigns ("banks", vBankId, "RTGSConDate", "", ?).
   END.
      ELSE IF vConDate <> ? THEN
   DO:
      UpdateSigns ("banks", vBankId, "RTGSDiscDate", "", ?).
      UpdateSigns ("banks", vBankId, "RTGSConDate", STRING (vConDate,"99/99/9999"), ?).
   END.
   IF {assigned vStopMode} THEN                       /* AdditionalConditions */
      UpdateSigns ("banks", vBankId, "RTGSStopMode", vStopMode, ?).
   IF {assigned vUrgent} THEN
      UpdateSigns ("banks", vBankId, "RTGSUrgent", vUrgent, ?).
   IF {assigned vCross} THEN
      UpdateSigns ("banks", vBankId, "RTGSCross", vCross, ?).
   IF {assigned vSuspMode} THEN
      UpdateSigns ("banks", vBankId, "RTGSSuspMode", vSuspMode, ?).
   IF {assigned vSWIFTMode} THEN
      UpdateSigns ("banks", vBankId, "RTGSSWIFTMode", vSWIFTMode, ?).
   IF {assigned vLimMode} THEN
      UpdateSigns ("banks", vBankId, "RTGSLimMode", vLimMode, ?).
   IF {assigned vStoppageReason} THEN
      UpdateSigns ("banks", vBankId, "RTGSStoppageReason", vStoppageReason, ?).
   IF {assigned vReasonAddText}  THEN 
      UpdateSigns ("banks", vBankId, "RTGSReasonAddText", vReasonAddText, ?).

      IF vStoppageDate <> ?  THEN
      UpdateSigns ("banks", vBankId, "RTGSStoppageDate",    STRING(vStoppageDate,   "99/99/9999"), ?).
      IF vStoppageENDDate <> ?  THEN
      UpdateSigns ("banks", vBankId, "RTGSStoppageEndDate", STRING(vStoppageEndDate,"99/99/9999"), ?).

   IF {assigned vRTGSLiqBfOD} THEN
      UpdateSigns ("banks", vBankId, "RTGSLiqBfOD", vRTGSLiqBfOD, ?).
   IF {assigned vRTGSLiqInOD} THEN
      UpdateSigns ("banks", vBankId, "RTGSLiqInOD", vRTGSLiqInOD, ?).

   IF {assigned vRTGSPayeePayts} AND
         vRTGSPayeePayts <> "0" THEN
      UpdateSigns ("banks", vBankId, "RTGSPayeePayts", vRTGSPayeePayts, ?).
   IF {assigned vRTGSDispPayeePayts} AND
         vRTGSDispPayeePayts <> "0" THEN
      UpdateSigns ("banks", vBankId, "RTGSDispPayeePayts", vRTGSDispPayeePayts, ?).
   END.

   IF NOT vLinked THEN
   RUN PacketCreateLink (vPacketID, "banks", vBankId, "ED374"). 

   vClearList = "OUR-BIC,PUR-BIC,UIS,MemberType,RegMode,RegDate,ExceptDate,DisconMode,DisconDate,ConnDate," +
                "StopMode,UrgentPays,CrossPays,SuspMode,SWIFTMode,LimitMode".
   DO vI = 1 TO NUM-ENTRIES(vClearList):
      hFld = hExch:BUFFER-FIELD(ENTRY(vI,vClearList)).
      IF hFld:TYPE =  "CHARACTER" THEN
         hFld:BUFFER-VALUE = "".
      ELSE
         hFld:BUFFER-VALUE = ?.
   END.

   ASSIGN
      hExch:BUFFER-FIELD("TUCode")         :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("MemberType")     :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("Acct")           :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("UIS")            :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("PUR-BIC")            :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("OUR-BIC")        :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("RegMode")        :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("RegDate")        :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("ExceptDate")     :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("DisconMode")     :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("DisconDate")     :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("ConnDate")       :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("StopMode")       :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("UrgentPays")     :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("CrossPays")      :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("SuspMode")       :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("SWIFTMode")      :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("LimitMode")      :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("StoppageReason") :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("ReasonAddText")  :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("StoppageDate")   :BUFFER-VALUE = ?
      hExch:BUFFER-FIELD("StoppageEndDate"):BUFFER-VALUE = ?
   .

   vFlagSet = YES.
END.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDConfirmCUSTINFO","END").    
&ENDIF

{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDConfirmTUINFO.
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.
   DEFINE VARIABLE vFlagSet  AS LOGICAL INIT ? NO-UNDO.
MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   CASE hExch:TYPE:
      WHEN "TEMP-TABLE" THEN
         hExch = hExch:DEFAULT-BUFFER-HANDLE.
      WHEN "BUFFER" THEN .
   END CASE.
   hExch:BUFFER-FIELD("TUCode"):BUFFER-VALUE = "".
   hExch:BUFFER-FIELD("Participat"):BUFFER-VALUE = "".
   vFlagSet = YES.
END.  /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.


PROCEDURE ESIDOperationED201.
   DEFINE INPUT PARAMETER hExch          AS handle   NO-UNDO.
   DEFINE       PARAMETER BUFFER bCode   FOR Code.
   DEFINE INPUT PARAMETER iPacketID      AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER iLinkPacketID  AS INT64  NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vErrorCls AS CHAR           NO-UNDO.
   DEFINE VAR vErrorLst AS CHAR           NO-UNDO.
   DEFINE VAR vErrorBIS AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorRKC AS LOGICAL        NO-UNDO.
   DEFINE VAR vState    AS CHAR           NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationED201"," iPacketID:" + GetNullInt(iPacketID)     +
                                       " iLinkPacketID:" + GetNullInt(iLinkPacketID)).
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      FIND FIRST Packet WHERE Packet.PacketID =  iLinkPacketID NO-LOCK NO-ERROR.

      IF Packet.mail-format =  "XML-ED499" OR
         Packet.mail-format =  "XML-ED999" OR
         Packet.mail-format BEGINS "XML-ED599" THEN DO:

         RUN ESIDErrorPrepare   (INPUT  hExch,
                                 INPUT  "u",
                                 OUTPUT vErrorCls,
                                 OUTPUT vErrorRKC,
                                 OUTPUT vErrorBIS,
                                 OUTPUT vErrorLst).
   
         RUN PacketSetError     (INPUT  iPacketID,
                                 INPUT  vErrorCls,
                                 INPUT  vErrorLst,
                                 OUTPUT vState).

         IF CAN-DO(vErrorLst,"u2999")
            THEN RUN PacketSetState (iLinkPacketID,{&STATE-FIN}).
            ELSE RUN PacketSetState (iLinkPacketID,{&STATE-ERR}).

      END.
      ELSE
         RUN PacketSetState (iLinkPacketID,{&STATE-ERR}).

  
      RUN PacketCreateLink  (INPUT  iPacketID,
                             INPUT  "Packet",
                             INPUT  STRING(iLinkPacketID),
                             INPUT  ENTRY(2,bCode.Description[1])).
   
      RUN PacketCreateLink  (INPUT  iLinkPacketID,
                             INPUT  "Packet",
                             INPUT  STRING(iPacketID),
                             INPUT  ENTRY(2,bCode.Description[1])).

      vFlagSet = YES.
   END.
{doreturn.i vFlagSet}
END PROCEDURE.

/* -------------------------------------------------------------------------- */
/*            ED209BSP                                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmMODE:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain     AS handle         NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR v__ID     AS INT64        NO-UNDO.
   DEFINE VAR v__UP     AS INT64        NO-UNDO.
   DEFINE VAR vCode     AS CHAR           NO-UNDO.
   DEFINE VAR vNote     AS CHAR           NO-UNDO.
   DEFINE VAR vDateBeg  AS DATE           NO-UNDO.
   DEFINE VAR vDateEnd  AS DATE           NO-UNDO.
   DEFINE VAR vResult   AS CHAR           NO-UNDO.

   DEFINE BUFFER bBanks     FOR banks.
   DEFINE BUFFER bBanksCode FOR banks-code.
   DEFINE BUFFER bPacket    FOR Packet.


   DEFINE VARIABLE vBIC      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFormat   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClassRef AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSendID   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSendDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vRecvDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vRecvREF  AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vInitPackID   AS INT64       NO-UNDO.
   DEFINE VARIABLE vInitClassRef AS CHARACTER   NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).
      ASSIGN
         vFormat        = hExch:BUFFER-FIELD("mail-format"):BUFFER-VALUE
         vPacketID      = hMain  :BUFFER-FIELD("PacketID") :BUFFER-VALUE
         vClassRef      = bCode.misc[5]
         vInitClassRef  = bCode.misc[4]
         vSendID        = hExch:BUFFER-FIELD("SendID")      :BUFFER-VALUE
         vSendDate      = hExch:BUFFER-FIELD("SendDate")    :BUFFER-VALUE
         vCode          = hExch:buffer-field("OperModeCode"):buffer-value
         vDateBeg       = hExch:buffer-field("DateBeg"):buffer-value
         vDateEnd       = hExch:buffer-field("DateEnd"):buffer-value
         vNote          = TRIM(hExch:buffer-field("StoppageReason"):buffer-value)
         vBIC           = hExch:buffer-field("BIC"):buffer-value
         vRecvDate      = hExch:BUFFER-FIELD("RecvDate"):BUFFER-VALUE
         vRecvREF       = hExch:BUFFER-FIELD("RecvREF"):BUFFER-VALUE
      NO-ERROR. {&ON-ERROR}

      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-ERROR.
      IF AVAIL Packet THEN
         Packet.mail-format = vFormat.
/*-------------------------------------------------------------------- Ссылка */
      RUN PacketCreateRef IN h_rfrnc (vSendDate, vPacketID, vClassRef,
                                      vSendID + "|" + 
                                      ReferenceFormatValue(vClassRef,hExch:BUFFER-FIELD("SendREF"):BUFFER-VALUE)).
      ASSIGN
         hExch:BUFFER-FIELD("PacketID")  :BUFFER-VALUE = vPacketID
      NO-ERROR. {&ON-ERROR}

      RUN ESIDInitFind (vRecvDate, vInitClassRef, ReferenceFormatValue(vInitClassRef, vRecvREF), 
                        OUTPUT vInitPackID).
      IF vInitPackID <> ? THEN DO:
         FIND FIRST bPacket WHERE bPacket.PacketID =  vInitPackID NO-LOCK NO-ERROR.
         IF AVAIL bPacket THEN DO:
            IF bPacket.mail-format BEGINS "XML-ED393" THEN DO:
               RUN ESIDConfirmCHANNEL (iFormat,
                                       hExch)   NO-ERROR. {&ON-ERROR}
               vFlagSet = YES.
               LEAVE MAIN.
            END.
         END.
      END.

      RUN GetBank IN h_base (BUFFER bBanks,
                             BUFFER bBanksCode,
                             vBIC,
                             "МФО-9",
                             NO).
     IF AVAIL bBanks THEN
        RUN PacketCreateLink  (vPacketID,
                               "banks",
                               STRING (bBanks.bank-id),
                               ENTRY(2,bCode.Description[1])).

      RUN PacketTextKeep(vPacketID,
                         "БИК:" + GetNullStr(vBIC)                                             + "~n" +
                         GetNullStr(vCode) + " - " + GetNullStr(GetCodeName("ESIDMode",vCode)) + "~n" +
                         IF {assigned vNote} THEN
                             GetNullStr(vNote) + " - " + GetNullStr(GetCodeName("ПричВведОгр",vNote)) 
                                             ELSE ""                                           + "~n" +
                         "Дата начала действия режима:" + GetNullDat(vDateBeg)                 + "~n" +
                         IF vDateEnd <> ? THEN 
                             "Дата конца  действия режима:" + GetNullDat(vDateEnd)                
                                          ELSE ""                                              + "~n",
                         INPUT-OUTPUT vResult).
      RUN PacketTextSave(vPacketID,vResult).

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP) NO-ERROR.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт выписки в целом                                                     */
/* Метод IMPORT на классe XML-ED221 (Завершение обработки выписки)            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDCheckSTMT:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet    AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain       AS handle         NO-UNDO.
   DEFINE VAR mhRKCDoc    AS handle         NO-UNDO.
   DEFINE VAR mhBSPDoc    AS handle         NO-UNDO.
   DEFINE VAR oPacketID   AS INT64        NO-UNDO.
   DEFINE VAR vPacketID   AS INT64        NO-UNDO.
   DEFINE VAR vParentID   AS INT64        NO-UNDO.
   DEFINE VAR vInitID     AS INT64        NO-UNDO.
   DEFINE VAR vRecvRef    AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDate   AS DATE           NO-UNDO.
   DEFINE VAR vRecvID     AS CHAR           NO-UNDO.
   DEFINE VAR vClassRef   AS CHAR           NO-UNDO.
   DEFINE VAR vErrorCls   AS CHAR           NO-UNDO.
   DEFINE VAR vErrorLst   AS CHAR           NO-UNDO.
   DEFINE VAR vErrorBIS   AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorRKC   AS LOGICAL        NO-UNDO.
   DEFINE VAR vSurr       AS CHAR           NO-UNDO.
   DEFINE VAR vStatus     AS CHAR           NO-UNDO.
   DEFINE VAR vState      AS CHAR           NO-UNDO.
   DEFINE VAR vResult     AS CHAR           NO-UNDO.
   DEFINE VAR vCtrlCode   AS CHAR           NO-UNDO.
   DEFINE VAR vBuffer     AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER bOpEntry  FOR op-entry.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

     IF NOT EXCH-MSGBuff (INPUT  iFormat,
                          BUFFER bCode) THEN
           UNDO MAIN, RETRY MAIN.

     RUN ESIDGetMain     (INPUT-OUTPUT hWOP,
                          OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/

     ASSIGN    
        vPacketID = hMain:BUFFER-FIELD("PacketID"):BUFFER-VALUE
        vClassRef = bCode.misc[4]        
        vRecvRef  = ReferenceFormatValue(vClassRef,
                                         hMain:buffer-field("SendRef"):buffer-value) 
        vRecvDate = hMain:buffer-field("SendDate"):buffer-value
     NO-ERROR. {&ON-ERROR}

     &IF DEFINED(IS-DEBUG) &THEN
     RUN dbgprint.p ("ESIDCheckSTMT"," hWOP:Name:" + GetNullStr(hWop:Name)    +
                                     " hMain:"     + GetNullStr(hMain:Name)   +
                                     " vParentID:" + GetNullInt(vParentID)    +
                                     " vPacketID:" + GetNullInt(vPacketID)    +
                                     " vClassRef:" + GetNullStr(vClassRef)    +
                                     " vRecvRef:"  + GetNullStr(vRecvRef)     +
                                     " vRecvDate:" + GetNullStr(STRING(vRecvDate))).
     &ENDIF
                                       /* поиск ED210 */
     FOR EACH  Reference WHERE
               Reference.op-date    =  vRecvDate
           AND Reference.Class-Code =  vClassRef
           AND Reference.RefValue   =  vRecvREF
               NO-LOCK,
         FIRST Packet WHERE
               Packet.PacketID      =  Reference.PacketID
               NO-LOCK,
         EACH  PacketText WHERE
               PacketText.PacketID =  Packet.PacketID NO-LOCK:
    
           RUN dbgprint.p ("ESIDCheckSTMT"," vPacketID:"      + STRING(vPacketID) +
                                           " Packet.PacketID" + STRING(Packet.PacketID)).

           RUN PacketCreateLink  (INPUT  vPacketID,
                                  INPUT  "Packet",
                                  INPUT  STRING(Packet.PacketID),
                                  INPUT  ENTRY(2,bCode.Description[1])).
     END.
                                       /* обработка ошибок документа */
      mhBSPDoc  = ObjectValueHandle("ExchBSPDoc").
      vCtrlCode = IF valid-handle(mhBSPDoc) THEN
                     mhBSPDoc:BUFFER-FIELD("State"):BUFFER-VALUE
                  ELSE "".

      mhRKCDoc = ObjectValueHandle("ExchRKCDoc").
      IF valid-handle(mhRKCDoc) THEN DO:         /* поиск по <ED101> */
         ASSIGN    
            vClassRef = bCode.misc[4]        
            vRecvRef  = ReferenceFormatValue(vClassRef,
                                            mhRKCDoc:buffer-field("SendRef"):buffer-value) 
            vRecvDate = mhRKCDoc:buffer-field("SendDate"):buffer-value
         NO-ERROR. {&ON-ERROR}
   
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDCheckSTMT"," mhRKCDoc:Name:" + GetNullStr(mhRKCDoc:Name)    +
                                        " hMain:"     + GetNullStr(hMain:Name)   +
                                        " vPacketID:" + GetNullInt(vPacketID)    +
                                        " vClassRef:" + GetNullStr(vClassRef)    +
                                        " vRecvRef:"  + GetNullStr(vRecvRef)     +
                                        " vRecvDate:" + GetNullStr(STRING(vRecvDate))).
         &ENDIF
   /*----------------------------------------------- Поиск исходного документа --*/
         vSurr  = "".
         FOR EACH  Reference WHERE
                   Reference.op-date    =  vRecvDate
               AND Reference.Class-Code =  vClassRef
               AND Reference.RefValue   =  vRecvREF
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID      =  Reference.PacketID
                   NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  =  Packet.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
             FIRST op WHERE
                   op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                   NO-LOCK,
             FIRST op-entry OF OP NO-LOCK QUERY-TUNING(HINT "RULE"):
             vSurr  = STRING(op.op) + "," + STRING(op-entry.op-entry).
             mhRKCDoc:BUFFER-FIELD("ErrorList"):BUFFER-VALUE = vCtrlCode.

             RUN ESIDErrorPrepare   (INPUT  mhRKCDoc,
                                     INPUT  "u",
                                     OUTPUT vErrorCls,
                                     OUTPUT vErrorRKC,
                                     OUTPUT vErrorBIS,
                                     OUTPUT vErrorLst).


             RUN PacketSetError     (INPUT  vPacketID,
                                     INPUT  vErrorCls,
                                     INPUT  vErrorLst,
                                     OUTPUT vState).

      RUN dbgprint.p ("ESIDCheckSTMT","3 vErrorRKC:" + STRING(vErrorRKC) +
                                      " vErrorBIS:"  + STRING(vErrorBIS) +
                                      " vErrorLst:"  + GetNullStr(vErrorLst)).


             IF {assigned vErrorLst} THEN DO:
                mhRKCDoc:BUFFER-FIELD("ErrorList"):BUFFER-VALUE = "".


                RUN PacketCreateLink (vPacketID,
                                      "op-entry",
                                      vSurr,
                                      ENTRY(2,bCode.Description[1] + "-ERROR")).
                RUN Fill-SysMes("","ExchRKC52","","%s=" + GetNullStr(vSurr) +
                                                  "%s=" + GetNullStr(vErrorLst)).
             END.
             ELSE DO:
                RUN PacketCreateLink (vPacketID,
                                      "op-entry",
                                      vSurr,
                                      ENTRY(2,bCode.Description[1])).
                RUN PacketCreateLink  (INPUT  vPacketID,
                                       INPUT  "Packet",
                                       INPUT  STRING(Packet.PacketID),
                                       INPUT  ENTRY(2,bCode.Description[1])).
             END.
             RUN ESIDOperCompare    (INPUT  mhRKCDoc,
                                     INPUT  vSurr,
                                     BUFFER bOpEntry).
         END.
         IF {assigned vSurr}
            THEN  mhRKCDoc:BUFFER-FIELD("op-kind"):BUFFER-VALUE = "".
         ELSE IF NOT {assigned vSurr} AND  /* документа нет в базе */
                 {assigned vCtrlCode} THEN DO:

            FIND FIRST Packet WHERE
                       Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.
   
            RUN PacketCreateF   (INPUT  Packet.SeanceID,
                                 BUFFER bCode,
                                 OUTPUT oPacketID).

            FIND FIRST Packet WHERE
                       Packet.PacketID =  oPacketID EXCLUSIVE-LOCK NO-WAIT.
            ASSIGN
               Packet.ParentID = vPacketID NO-ERROR.

            vResult = "".
            RUN PacketTextKeep (INPUT        oPacketID,
                                INPUT        "НОМЕР  ТИП СТАТ СЧЕТ ДЕБЕТА          СЧЕТ КРЕДИТА         КОД БАНКА СУММА             ЭЛ. НОМЕР~n" +
                                             "------ --- ---- -------------------- -------------------- --------- ----------------- --------------------~n",
                                INPUT-OUTPUT vResult).

            vBuffer =  TRIM(mhRKCDoc:BUFFER-FIELD("doc-num"):BUFFER-VALUE)        + "     " +
                       TRIM(mhRKCDoc:BUFFER-FIELD("doc-type"):BUFFER-VALUE)       + "   "   +
                       TRIM(mhRKCDoc:BUFFER-FIELD("op-status"):BUFFER-VALUE)      + "    "  +
                       TRIM(mhRKCDoc:BUFFER-FIELD("acct-send-out"):BUFFER-VALUE)  + " "     +
                       TRIM(mhRKCDoc:BUFFER-FIELD("acct-rec"):BUFFER-VALUE)       + " "     +
                       TRIM(mhRKCDoc:BUFFER-FIELD("bank-code-send"):BUFFER-VALUE) + "   "   +
                       STRING(DEC(TRIM(mhRKCDoc:BUFFER-FIELD("amt-rub"):BUFFER-VALUE)),"->>>,>>>,>>9.99")     + " "    +    
                       TRIM(mhRKCDoc:BUFFER-FIELD("sendref"):BUFFER-VALUE)        +     "~n" +
                       "------ --- ---- -------------------- -------------------- --------- ----------------- --------------------" + "~n" +
                       "ИТОГО           1                   " +
                       STRING(DEC(TRIM(mhRKCDoc:BUFFER-FIELD("amt-rub"):BUFFER-VALUE)),"->,>>>,>>>,>>>,>>9.99") + "~n" .

            RUN PacketTextKeep (INPUT        oPacketID,
                                INPUT        vBuffer,
                                INPUT-OUTPUT vResult).

            RUN PacketTextSave (oPacketID,
                                vResult).
         END.
      END.
   
   /*--------------------------------------------- Изменение статуса документа --*/

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDCheckSTMT","4 vErrorBIS:" + string(vErrorBIS) +
                                              " vSurr:" + GetNullStr(vSurr) +
                                        " mStmtStatus:" + GetNullStr(mStmtStatus)).
      &ENDIF

      {profile ST024}
      IF vErrorBIS         =  NO AND
         vErrorRKC         =  NO AND
         {assigned vSurr}        AND
         {assigned mStmtStatus}  THEN DO:

         IF mStmtSCheck          OR
            {assigned mStateErr} THEN
            vStatus = ESIDStatusDefine(mhRKCDoc, mStmtStatus, BUFFER bOpEntry).

         IF {assigned vStatus} THEN
            FOR FIRST op WHERE
                      op.op =  INT64(ENTRY(1,vSurr))
                      EXCLUSIVE-LOCK:
               UpdateSigns("Packet",string(vPacketID),"StateOBJ",op.op-status,NO).
               {opent-st.i &Status = vStatus}
            END.
      END.

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      vFlagSet = YES.
   END.
 
   {doreturn.i vFlagSet}    
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт  ED101  - сохранение сообщения импорта в PacketText                 */
/*----------------------------------------------------------------------------*/ 
PROCEDURE ESIDSaveMessage:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDSaveMessage","start").
      &ENDIF
      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWOP) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):BUFFER-VALUE
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDSaveMessage","RUN PacketRKCTextSave:" + GetNullStr(string(vPacketID)) + " iFormat:" + GetNullStr(iFormat)).
      &ENDIF

      IF iFormat =  "XML-ED108" THEN
         RUN PacketRKCTextSave (vPacketID).
      ELSE IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN 
         RUN PacketBSPTextSave(vPacketID). 

      vFlagSet = YES.
   END.
 
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт  ED306 Подтверждение исполнения распоряжения                        */
/*               для управления позицией ликвидности                          */
/*----------------------------------------------------------------------------*/ 
PROCEDURE ESIDConfirmLiquidity:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR  hQuery           AS handle         NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vParentID         AS INT64        NO-UNDO.
   DEFINE VAR v__ID             AS INT64        NO-UNDO.
   DEFINE VAR v__UP             AS INT64        NO-UNDO.
   DEFINE VAR vcPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vResult           AS CHAR           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vFlagFind         AS LOGICAL        NO-UNDO.
   DEFINE VAR vFlagFindLim      AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorClass       AS CHAR           NO-UNDO.
   DEFINE VAR vErrorList        AS CHAR           NO-UNDO.
   DEFINE VAR vFake             AS CHAR           NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vLiquiditySum     AS DECIMAL        NO-UNDO.
   DEFINE VAR vPURBIC           AS CHAR           NO-UNDO.


MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWOP) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vClassRef   = bcode.misc[4]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hWOP:buffer-field("RecvDate"):buffer-value
      NO-ERROR. {&ON-ERROR}
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN DO:
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         .
      END.
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.
               ASSIGN 
                  vLiquiditySum  = hWOP:buffer-field("LiquiditySum"):buffer-value 
                  vPURBIC        = hWOP:buffer-field("PURBIC"):buffer-value    
               NO-ERROR.                  
   
      vResult = "ТАБЛИЦА РЕКВИЗИТОВ СООБЩЕНИЯ" +  '~n~n' +     
                "Бик:         " + GetNullStr(vPURBIC)               + '~n'   +
                "Сумма лимита:" + GetNullStr(STRING(vLiquiditySum)) + '~n'.

      vResult = vResult + FILL("-",76) + '~n~n'.    

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmLiquidity"," iFormat:"          + GetNullStr(iFormat)        +
                                             " hWOP:name:"        + GetNullStr(hWOP:name)      +
                                             " vClassRef:"        + GetNullStr(vClassRef)      +
                                             " vRecvRef:"         + GetNullStr(vRecvRef)       +
                                             " vRecvDate:"        + STRING(vRecvDate)          +
                                             " bCode.Description[1]):" + ENTRY(2,bCode.Description[1])).
      &ENDIF

      vFlagFind = NO.
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH  PacketText WHERE
                PacketText.PacketID =  Packet.PacketID NO-LOCK:
         ASSIGN
            vcPacketID       = Packet.PacketID
         NO-ERROR.

         vFlagFind = YES.
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDConfirmLiquidity","PacketText.Contents:" + PacketText.Contents).
         &ENDIF

         vResult = vResult +
                   "НАЙДЕНО СООБЩЕНИЕ:"                           + '~n'   +
                   "Код класса:    " + Packet.Class-Code          + '~n'   +
                   "Формат обмена: " + Packet.Mail-Format         + '~n'   + 
                   "Дата:          " + STRING(Reference.op-date)  + '~n'   +
                   "Cсылка:        " + Reference.RefValue         + '~n'.

         vFlagFindLim = NO.
         IF     INDEX(PacketText.Contents,"SUM")                 <> 0
            AND INDEX(PacketText.Contents,STRING(vLiquiditySum)) <> 0
            THEN DO:
               vResult = vResult + '~n'   +
                         "SUM="  + '"' + STRING(vLiquiditySum) + '"~n~n'.
            vFlagFindLim = YES.
         END.
         IF NOT vFlagFindLim THEN DO:
            vResult = vResult + '~n'       +
                      "НЕ СОВПАЛИ ДАННЫЕ:"  + '~n'.
            IF vLiquiditySum <> 0
               AND INDEX(PacketText.Contents,"SUM")        <> 0
               AND INDEX(PacketText.Contents,STRING(vLiquiditySum)) =  0
                   THEN vResult = vResult + '~n' +
                        "SUM=" + '"' + STRING(vLiquiditySum) + '"~n'.
            vResult =  vResult + "~n" +  PacketText.Contents.
      
            &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("ESIDConfirmLiquidity","vResult:" + vResult).
            &ENDIF
         END.

         RUN PacketCreateLink  (INPUT  vPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vcPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         RUN PacketSetState (vcPacketID,{&STATE-FIN}).
      END.

      RUN PacketTextSave(vPacketID,vResult).

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      IF NOT vFlagFind THEN DO:
         RUN PacketSetState (vPacketID,{&STATE-ERR}).

         ASSIGN
            vErrorClass = "ErrorEsid"
            vErrorList  = "b8060"
         NO-ERROR.

         RUN PacketSetError (INPUT  vPacketID,
                             INPUT  vErrorClass,
                             INPUT  vErrorList,
                             OUTPUT vFake) NO-ERROR.
      END.
      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   RUN InstanceDelete(hWOP).
   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* ЭСИД НСИ ED231, ED232, ED233                                               */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDInitialNSI:
   DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
   DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

   DEFINE BUFFER bCode  FOR code.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hMain    AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hIns     AS HANDLE      NO-UNDO.

   DEFINE VAR vPacketID        AS INT64     NO-UNDO.
   DEFINE VAR vParentID        AS INT64     NO-UNDO.
   DEFINE VARIABLE vSeanceDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vEdDate     AS DATE        NO-UNDO.
   DEFINE VARIABLE vErrCls     AS CHARACTER   NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDInitialNSI","START").    
   &ENDIF

   MAIN:
      DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
         {do-retry.i MAIN}

         IF NOT EXCH-MSGBuff (INPUT  iFormat,
                              BUFFER bCode) THEN
            UNDO MAIN, RETRY MAIN.

         RUN XRKCPacketCheck IN h_xrkc (iFormat, hExchTT) NO-ERROR. {&ON-ERROR}

         RUN ESIDGetMain       (INPUT-OUTPUT hExchTT,
                                OUTPUT       hMain).
   /*----------------------------------------- Регистрация полученного сообщения */
         RUN ESIDPacketCreate  (INPUT  hExchTT,
                                INPUT  hMain,
                                BUFFER bCode,
                                OUTPUT vPacketID,
                                OUTPUT vParentID).

         RUN PacketInclude    (INPUT  vPacketID,
                               INPUT  vParentID,
                               INPUT  {&STATE-FIN}).
   /*------------------------------------------------------------- Проверка даты */
         ASSIGN
            vSeanceDate = hExchTT:BUFFER-FIELD("RecvDate")  :BUFFER-VALUE
            vEDDate     = hExchTT:BUFFER-FIELD("SendDate")  :BUFFER-VALUE
            vErrCls     = hExchTT:BUFFER-FIELD("ErrorClass"):BUFFER-VALUE
         NO-ERROR. {&ON-ERROR}
         IF vEdDate <  vSeanceDate THEN
         DO:
            RUN AddErrorFormat IN h_exch (hExchTT, "u8100").
            RUN Fill-SysMes IN h_tmess ("","ExchRKC50","","%s=" + REPLACE(iFormat, "XML-","") + 
                                                          "%s=" + STRING(vEDDate) + 
                                                          "%s=" + STRING(vSeanceDate) +
                                                          "%s=" + IF GetXAttrInit (vErrCls, "u8100") =  "Ошибка" 
                                                                     THEN "Импорт не выполнен"
                                                                     ELSE ""
                                        ).
         END.

         ASSIGN
            hExchTT:BUFFER-FIELD("PacketID"):BUFFER-VALUE = vPacketID
         NO-ERROR. {&ON-ERROR}

         vFlagSet = YES.
   END.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDInitialNSI","END").    
   &ENDIF

   {doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDConfirmNSI:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR v__ID        AS INT64        NO-UNDO.
   DEFINE VAR v__UP        AS INT64        NO-UNDO.

   DEFINE VARIABLE vPacketID   AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrClass   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrList    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vState      AS CHARACTER   NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmNSI","START").    
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CASE hExch:TYPE:
         WHEN "TEMP-TABLE" THEN
             hExch = hExch:DEFAULT-BUFFER-HANDLE.
         WHEN "BUFFER" THEN .
      END CASE.

      ASSIGN
         vErrClass   = hExch:BUFFER-FIELD("ErrorClass"):BUFFER-VALUE
         vErrList    = hExch:BUFFER-FIELD("ErrorList"):BUFFER-VALUE
         vPacketID   = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      .
      RUN PacketSetError (vPacketID, vErrClass, vErrList, OUTPUT vState).

       vFlagSet = YES.
   END. /* MAIN */

   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmNSI","END").    
   &ENDIF

   {doreturn.i vFlagSet}
END PROCEDURE.


PROCEDURE ESIDConfirmBNKS:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vBIC      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBankID   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vCorrAcct AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIntReg   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUER      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vReal     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNOP      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOtzv     AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bBanks     FOR banks.
   DEFINE BUFFER bBanksCorr FOR banks-corr.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmBNKS","START").    
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CASE hExch:TYPE:
         WHEN "TEMP-TABLE" THEN
            hExch = hExch:DEFAULT-BUFFER-HANDLE.
         WHEN "BUFFER" THEN .
      END CASE.
      ASSIGN
         vPacketID = hExch:BUFFER-FIELD("PacketID")    :BUFFER-VALUE
         vBIC      = hExch:BUFFER-FIELD("BIC")         :BUFFER-VALUE
         vErrLst   = hExch:BUFFER-FIELD("ErrorList")   :BUFFER-VALUE
         vErrCls   = hExch:BUFFER-FIELD("ErrorClass")  :BUFFER-VALUE
         vCorrAcct = hExch:BUFFER-FIELD("CorrAcct")    :BUFFER-VALUE
         vIntReg   = hExch:BUFFER-FIELD("inter-region"):BUFFER-VALUE
         vOtzv     = hExch:BUFFER-FIELD("Otzv")        :BUFFER-VALUE
         vReal     = hExch:BUFFER-FIELD("real")        :BUFFER-VALUE
         vNOP      = hExch:BUFFER-FIELD("nop")         :BUFFER-VALUE
      .
   /*------------------------------------------------------------------ Проверки */
      IF CAN-DO (vErrLst, "u8100") AND GetXAttrInit (vErrCls, "u8100") =  "Ошибка" THEN
      DO:
         vFlagSet = YES.
         LEAVE MAIN.
      END.

      vBankID = GetNeedBankCode ("МФО-9", vBIC, "bank-id").
      IF NOT {assigned vBankId} THEN
      DO:
         vFlagSet = YES.
         LEAVE MAIN.
      END.
      IF {assigned vCorrAcct} THEN
      DO:
         FIND FIRST bBanks WHERE bBanks.bank-id =  INT64(vBankId)
            NO-LOCK NO-ERROR.
         IF GetCorrRKC (BUFFER bBanks, BUFFER bBanksCorr) THEN
            IF bBanksCorr.corr-acct <> vCorrAcct THEN
            DO:
               vSkip = (GetXAttrInit (vErrCls, "u8110") =  "Ошибка"). 
               RUN Fill-SysMes IN h_tmess ("", "ExchRKC51", "", "%s=" + vBIC + 
                                                                "%s=" + vCorrAcct +
                                                                "%s=" + IF vSkip 
                                                                        THEN "Импорт не выполнен для данного банка." 
                                                                        ELSE ""
                                          ).
               RUN AddErrorFormat (hExch,"u8110").
               IF vSkip THEN
               DO:
                  vFlagSet = YES.
                  LEAVE MAIN.
               END.
            END.
      END.
   /*-------------------------------------------------------------------- Импорт */
      IF {assigned vIntReg} THEN
         UpdateSigns ("banks", vBankId, "inter-region", vIntReg, ?).
      IF {assigned vOtzv} THEN
         UpdateSigns ("banks", vBankId, "Otzv", vOtzv, ?).
      IF {assigned vReal} AND vReal = "0" THEN
         UpdateSigns ("banks", vBankId, "real", "ИЗМР", ?).
      IF {assigned vNOP} THEN
         UpdateSigns ("banks", vBankId, "УчастникНОП", vNOP, ?).

      RUN PacketCreateLink (vPacketID, "banks", vBankId, "ED231").

       vFlagSet = YES.
   END.

   ASSIGN
      hExch:BUFFER-FIELD("BIC")         :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("CorrAcct")    :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("inter-region"):BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("uer")         :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("real")        :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("nop")         :BUFFER-VALUE = ""
   .
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmBNKS","END").    
   &ENDIF

   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Метод INITIAL на классе XML-PLAN                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDInitialPLAN:
   DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
   DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO.

   RUN ESIDInitialNSI(iFormat,hExchTT).

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF FGetSetting("УФЭБС","ImpED232","Обновление") =  "Замена" THEN
         FOR EACH code WHERE code.class  =  "BS20.nsi"
                         AND code.parent =  "BS20.nsi" EXCLUSIVE-LOCK:
            DELETE code.
         END.

      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-PLAN                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmPLAN:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBS       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRKC      AS CHARACTER   NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPLAN","START").    
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CASE hExch:TYPE:
         WHEN "TEMP-TABLE" THEN
            hExch = hExch:DEFAULT-BUFFER-HANDLE.
         WHEN "BUFFER" THEN .
      END CASE.
      ASSIGN
         vPacketID = hExch:BUFFER-FIELD("PacketID")  :BUFFER-VALUE
         vErrLst   = hExch:BUFFER-FIELD("ErrorList") :BUFFER-VALUE
         vErrCls   = hExch:BUFFER-FIELD("ErrorClass"):BUFFER-VALUE
         vBS       = hExch:BUFFER-FIELD("BS")        :BUFFER-VALUE
         vRKC      = hExch:BUFFER-FIELD("RKC")       :BUFFER-VALUE
      .
   /*------------------------------------------------------------------ Проверки */
      IF CAN-DO (vErrLst, "u8100") AND GetXAttrInit (vErrCls, "u8100") =  "Ошибка" THEN
      DO:
         vFlagSet = YES.
         LEAVE MAIN.
      END.
   /*-------------------------------------------------------------------- Импорт */
      FIND FIRST code WHERE code.class =  "BS20.nsi" AND
                            code.code  =  vBS
         NO-ERROR.
      IF NOT AVAIL code THEN
      DO:
         CREATE code.
         ASSIGN
            code.class  = "BS20.nsi"
            code.parent = "BS20.nsi"
            code.code   = vBS
         NO-ERROR. {&ON-ERROR}
      END.
      ASSIGN
         code.val = vRKC
      NO-ERROR. {&ON-ERROR}

      RUN PacketCreateLink (vPacketID, "code", "BS20.nsi," + vBS, "ED232").

       vFlagSet = YES.
   END.

   ASSIGN
      hExch:BUFFER-FIELD("BS")  :BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("Type"):BUFFER-VALUE = ""
      hExch:BUFFER-FIELD("RKC") :BUFFER-VALUE = ""
   .

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmPLAN","END").    
   &ENDIF

   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* XML-ZAK                                                                    */
/*----------------------------------------------------------------------------*/
FUNCTION LastCodeNum INT64 ():
   DEF VAR vNum   AS INT64 NO-UNDO.
   DEF BUFFER zcode FOR code.

   vNum = 0.

   FOR LAST zcode WHERE
            zcode.class  =  "Zaks20.nsi"
        AND zcode.parent =  "Zaks20.nsi"
            NO-LOCK:
      vNum = INT64 (zcode.code).
   END.
   RETURN vNum.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Метод INITIAL на классе XML-ZAK                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDInitialZAK:
   DEFINE INPUT PARAMETER iFormat      AS CHARACTER     NO-UNDO.
   DEFINE INPUT PARAMETER hExchTT      AS handle        NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO.

   RUN ESIDInitialNSI(iFormat,hExchTT).

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF FGetSetting("УФЭБС","ImpED233","Обновление") =  "Замена" THEN
         FOR EACH code WHERE code.class  =  "Zaks20.nsi"
                         AND code.parent =  "Zaks20.nsi" EXCLUSIVE-LOCK:
            DELETE code.
         END.

      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ZAK                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmZAK:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS handle   NO-UNDO.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR v__ID        AS INT64        NO-UNDO.
   DEFINE VAR v__UP        AS INT64        NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrLst   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrCls   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip     AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vOldBIC    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOldAcc    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNewBIC    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNewAcc    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNewDate   AS DATE        NO-UNDO.
   DEFINE VARIABLE vNum       AS INT64     NO-UNDO.

   DEF BUFFER bcode FOR code.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmZAK","START").    
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CASE hExch:TYPE:
         WHEN "TEMP-TABLE" THEN
            hExch = hExch:DEFAULT-BUFFER-HANDLE.
         WHEN "BUFFER" THEN .
      END CASE.
      ASSIGN
         vPacketID  = hExch:BUFFER-FIELD("PacketID")  :BUFFER-VALUE
         vErrLst    = hExch:BUFFER-FIELD("ErrorList") :BUFFER-VALUE
         vErrCls    = hExch:BUFFER-FIELD("ErrorClass"):BUFFER-VALUE
         vOldBIC    = hExch:BUFFER-FIELD("OldBIC")    :BUFFER-VALUE
         vOldAcc    = hExch:BUFFER-FIELD("OldAcc")    :BUFFER-VALUE
         vNewBIC    = hExch:BUFFER-FIELD("NewBIC")    :BUFFER-VALUE
         vNewAcc    = hExch:BUFFER-FIELD("NewAcc")    :BUFFER-VALUE
         vNewDate   = hExch:BUFFER-FIELD("NewDate")   :BUFFER-VALUE
      .
     IF  vNewDate =  ? THEN 
         vNewDate   = hExch:BUFFER-FIELD("DateClose") :BUFFER-VALUE.
     IF  NOT {assigned vOldBIC} THEN 
         vOldBIC    = hExch:BUFFER-FIELD("BIC")       :BUFFER-VALUE.
     IF  NOT {assigned vOldAcc} THEN
         vOldAcc    = hExch:BUFFER-FIELD("CorrAcct")  :BUFFER-VALUE.

     IF NOT {assigned vNewBIC} AND
        NOT {assigned vOldBIC}
     THEN DO:
        vFlagSet = YES.
        LEAVE MAIN.
     END.

   /*------------------------------------------------------------------ Проверки */

      RUN DumpObject(hExch).

      &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmZAK","vPacketID:" + GetNullInt(vPacketID) +  
                                       " vErrLst:"  + GetNullStr(vErrLst)).    
      &ENDIF

      IF CAN-DO (vErrLst, "u8100") AND GetXAttrInit (vErrCls, "u8100") =  "Ошибка" THEN
      DO:
         vFlagSet = YES.
         LEAVE MAIN.
      END.
   /*-------------------------------------------------------------------- Импорт */
      FIND FIRST bcode WHERE 
                 bcode.class  =  "Zaks20.nsi"
             AND bcode.parent =  "Zaks20.nsi"
             AND bcode.name   =  TRIM (vOldBic + " " + vOldAcc)
                 NO-LOCK NO-ERROR.

      &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmZAK","vOldBic:" + vOldBic + " " + vOldAcc + " " + STRING(AVAIL bcode)).
      RUN dbgprint.p ("ESIDConfirmZAK","vNewBic:" + vNewBic + " " + vNewAcc + " " + STRING(AVAIL bcode)).
      &ENDIF

      IF NOT AVAIL bcode THEN DO: 
         CREATE code.
         vNum = LastCodeNum() + 1.
      END.
      ELSE DO:
         vNum = INT64(bcode.code).
         FIND FIRST code WHERE RECID(code) =  RECID(bcode)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF NOT AVAIL code AND LOCKED code THEN DO:
            RUN wholocks2.p (RECID(bcode),(BUFFER bcode:TABLE),"").
            UNDO MAIN, RETRY MAIN.
         END.
      END.
      ASSIGN
         code.class  = "Zaks20.nsi"
         code.parent = "Zaks20.nsi"
         code.code   = STRING(vNum, "999999")
         code.name   = TRIM (vOldBic + " " + vOldAcc)
         code.val    = TRIM (vNewBIC + " " + vNewAcc + " " + STRING (vNewDate))
      NO-ERROR. /* {&ON-ERROR} */

      RUN PacketCreateLink (vPacketID, "code", "Zaks20.nsi," + code.code, "ED233").

      VALIDATE code NO-ERROR.

      vFlagSet = YES.
   END.
   
      ASSIGN
         hExch:BUFFER-FIELD("OldBIC") :BUFFER-VALUE = ""
         hExch:BUFFER-FIELD("OldAcc") :BUFFER-VALUE = ""
         hExch:BUFFER-FIELD("NewBIC") :BUFFER-VALUE = ""
         hExch:BUFFER-FIELD("NewAcc") :BUFFER-VALUE = ""
         hExch:BUFFER-FIELD("NewDate"):BUFFER-VALUE = ?
      .
   
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDConfirmZAK","END").    
      &ENDIF

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED241                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirmINFO:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR  hQuery           AS handle         NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vParentID         AS INT64        NO-UNDO.
   DEFINE VAR v__ID             AS INT64        NO-UNDO.
   DEFINE VAR v__UP             AS INT64        NO-UNDO.
   DEFINE VAR vcPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vResult           AS CHAR           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvID           AS CHAR           NO-UNDO.
   DEFINE VAR vSendRef          AS CHAR           NO-UNDO.
   DEFINE VAR vSendDate         AS DATE           NO-UNDO.
   DEFINE VAR vSendID           AS CHAR           NO-UNDO.
   DEFINE VAR vFlagFind         AS LOGICAL        NO-UNDO.
   DEFINE VAR vFlagFindLim      AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorClass       AS CHAR           NO-UNDO.
   DEFINE VAR vErrorList        AS CHAR           NO-UNDO.
   DEFINE VAR vFake             AS CHAR           NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWOP) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vClassRef   = bcode.misc[4]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hWOP:buffer-field("RecvDate"):buffer-value
         vRecvID     = hWOP:buffer-field("RecvID"):buffer-value
         vSendRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("SendRef"):buffer-value) 
         vSendDate   = hWOP:buffer-field("SendDate"):buffer-value
         vSendID     = hWOP:buffer-field("SendID"):buffer-value

      NO-ERROR. {&ON-ERROR}
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN DO:
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         .
      END.
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmINFO"," iFormat:"           + GetNullStr(iFormat)             +
                                         " hWOP:name:"        + GetNullStr(hWOP:name)           +
                                         " vClassRef:"        + GetNullStr(vClassRef)           +
                                         " vRecvRef:"         + GetNullStr(vRecvRef)            +
                                         " vRecvDate:"        + GetNullStr(STRING(vRecvDate))  +
                                         " bCode.Description[1]):" +  GetNullStr(ENTRY(2,bCode.Description[1]))).
      &ENDIF


      vFlagFind = NO.
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH  PacketText WHERE
                PacketText.PacketID =  Packet.PacketID NO-LOCK:
         ASSIGN
            vcPacketID       = Packet.PacketID
         NO-ERROR.

         vFlagFind = YES.
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDConfirmINFO","PacketText.Contents:" + PacketText.Contents).
         &ENDIF

         vResult = vResult +
                   "НАЙДЕНО СООБЩЕНИЕ:"                           + '~n'   +
                   "Код класса:    " + Packet.Class-Code          + '~n'   +
                   "Формат обмена: " + Packet.Mail-Format         + '~n'   + 
                   "Дата:          " + STRING(Reference.op-date)  + '~n'   +
                   "Cсылка:        " + Reference.RefValue         + '~n~n'.

         vResult =  vResult + "~n" +  PacketText.Contents.
   
         &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("ESIDConfirmLimit","vResult:" + vResult).
         &ENDIF
 
         RUN PacketCreateLink  (INPUT  vcPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         IF iFormat BEGINS "XML-ED422" OR
            iFormat BEGINS "XML-ED432" THEN 
         DO:
            RUN PacketCreateLink  (INPUT  vPacketID,
                                   INPUT  "Packet",
                                   INPUT  STRING(vcPacketID),
                                   INPUT  ENTRY(2,bCode.Description[1])).

            RUN PacketCreateRef (vSendDate,
                                 VPacketId,
                                 bCode.Misc[{&RKC-REPLY}],
                                 TRIM(vSendID) + "|" +
                                 ReferenceFormatValue(bCode.Misc[{&RKC-REPLY}],vSendREF)).

            RUN PacketSetState (vcPacketID,{&STATE-CNF}).
         END.
         ELSE
            RUN PacketSetState (vcPacketID,{&STATE-FIN}).
      END.

      IF NOT vFlagFind THEN DO:
         RUN PacketSetState (vPacketID,{&STATE-ERR}).

         ASSIGN
            vErrorClass = "ErrorEsid"
            vErrorList  = "b8060"
         NO-ERROR.

         RUN PacketSetError (INPUT  vPacketID,
                             INPUT  vErrorClass,
                             INPUT  vErrorList,
                             OUTPUT vFake) NO-ERROR.
      END.
      RUN PacketTextSave(vPacketID,vResult).

      RUN PacketRKCTextSave(vPacketID).

      vFlagSet = YES.

   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/

   RUN InstanceDelete(hWOP).
   {doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDConfirmENVEL.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hMain        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vPacketID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vParentID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vPrDoc       AS LONGCHAR    NO-UNDO.
   DEFINE VARIABLE vDocTmp      AS MEMPTR      NO-UNDO.
   DEFINE VARIABLE vPackTxt     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBinFileName AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFName       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPos         AS INT64       NO-UNDO.
   DEFINE VARIABLE vXMLCP       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSrcCP       AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL  INIT ?   NO-UNDO.

   DEFINE BUFFER bCode  FOR code.
   DEFINE BUFFER Packet FOR Packet.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN ASSIGN 
         mPropDoc = ""
         mPropAtt = "".
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,       /* Контроль формата обмена   */
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain (INPUT-OUTPUT hExch, 
                       OUTPUT hMain).

      RUN ESIDPacketCreate (hExch, hMain, 
                            BUFFER bCode, 
                            OUTPUT vPacketID, 
                            OUTPUT vParentID).

      IF LENGTH(mPropAtt) >  0 THEN DO:
         vDocTmp = BASE64-DECODE (mPropAtt) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","Ошибка декодирования приложения.").
            UNDO MAIN, RETRY MAIN.
         END.
         vBinFileName = TRNSettingValue("","AttachFileName", "./attach").
         vFName = GetPureName (vBinFileName).
         vPos = R-INDEX (vBinFileName, vFName).
         IF NUM-ENTRIES(vFName, ".") >= 2 THEN
            ENTRY(1, vFName, ".") = ENTRY(1, vFName, ".") + "_" + STRING(vPacketID).
         ELSE
            vFName = vFName + "_" + STRING(vPacketID).
         vBinFileName = SUBSTR(vBinFileName, 1, vPos - 1) + vFName.
         COPY-LOB vDocTmp TO FILE vBinFileName NO-CONVERT NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","Ошибка сохранения приложения.").
            UNDO MAIN, RETRY MAIN.
         END.
         SET-SIZE(vDocTmp) = 0.
         mPropAtt = "".
         vPackTxt = "Приложение сохранено в файл " + vBinFileName + "~n~n".
      END.

      vDocTmp = BASE64-DECODE (mPropDoc) NO-ERROR.
      IF ERROR-STATUS:ERROR OR GET-SIZE(vDocTmp) =  ? THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","Ошибка декодирования документа.").
         UNDO MAIN, RETRY MAIN.
      END.

      vXMLCP = GetSysConf("InXMLCodePage").
      IF {assigned vXMLCP} THEN DO:
         {cpname_xml2abl.i
            &MODE         = "'XML2ABL'"
            &XML_CODEPAGE = vXMLCP
            &ABL_CODEPAGE = vSrcCP }
         COPY-LOB vDocTmp TO vPrDoc 
            CONVERT SOURCE CODEPAGE vSrcCP NO-ERROR.
      END.
      ELSE
         COPY-LOB vDocTmp TO vPrDoc NO-CONVERT.

      SET-SIZE (vDocTmp) = 0.
      mPropDoc = "".
      vPrDoc   = vPackTxt + vPrDoc NO-ERROR.
      DO WHILE LENGTH (vPrDoc) >  0 :
         vPackTxt = SUBSTR  (vPrDoc, 1, {&PACK-LIMIT}).
         RUN PacketTextSave (vPacketID, vPackTxt).
         vPrDoc = SUBSTR(vPrDoc, {&PACK-LIMIT} + 1).
      END.

      FOR FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK:
         ASSIGN
            Packet.ParentID = vParentID
            Packet.State    = "ОБРБ".
      END.

      vFlagSet = YES.
   END. /* MAIN */
   {doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE CDATAImportDOC.
   DEFINE INPUT  PARAMETER hExch AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER iDoc  AS MEMPTR      NO-UNDO.

   DEFINE VARIABLE vZero AS LOGICAL     NO-UNDO.

   vZero = (GET-BYTE (iDoc, GET-SIZE(iDoc)) =  0).
   IF vZero THEN
      PUT-BYTE (iDoc, GET-SIZE(iDoc)) = ASC("0").
   COPY-LOB FROM iDoc TO mPropDoc NO-CONVERT. 
   IF vZero THEN
      mPropDoc = SUBSTR (mPropDoc, 1, LENGTH(mPropDoc) - 1).
   SET-SIZE(iDoc) = 0.
   
END PROCEDURE.

PROCEDURE CDATAImportATT.
   DEFINE INPUT  PARAMETER hExch AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER iAtt  AS MEMPTR      NO-UNDO.

   DEFINE VARIABLE vZero AS LOGICAL     NO-UNDO.

   vZero = (GET-BYTE (iAtt, GET-SIZE(iAtt)) =  0).
   IF vZero THEN
      PUT-BYTE (iAtt, GET-SIZE(iAtt)) = ASC("0").
   COPY-LOB FROM iAtt TO mPropAtt NO-CONVERT. 
   IF vZero THEN
      mPropAtt = SUBSTR (mPropAtt, 1, LENGTH(mPropAtt) - 1).
   SET-SIZE(iAtt) = 0.
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 1. Импорт запроса на уточнение реквизитов документа               (ED243)  */
/* 2. Импорт ответа  на запрос от другого участникаа                 (ED244)  */
/*                                                                            */
/* Параметры:                                                                 */
/*    iFormat   - Формат XML-объекта                                          */
/*    iInstance - Сущность, содержащая импортированные данные                 */
/*                                                                            */
/* Метод IMPORT на классах XML-ED24x                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirm243:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER op-entry  FOR op-entry.

   DEFINE BUFFER sCode     FOR Code.
   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER bPacket   FOR Packet.

   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagFnd     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagBeg     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vIsPack      AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain        AS handle         NO-UNDO.
   DEFINE VAR hExchRKCDoc  AS handle         NO-UNDO.
   DEFINE VAR vSurr        AS CHAR           NO-UNDO.
   DEFINE VAR v__ID        AS INT64        NO-UNDO.
   DEFINE VAR v__UP        AS INT64        NO-UNDO.
   DEFINE VAR vPacketID    AS INT64        NO-UNDO.
   DEFINE VAR vParentID    AS INT64        NO-UNDO.
   DEFINE VAR vInitialID   AS INT64        NO-UNDO.
   DEFINE VAR vErrorCls    AS CHAR           NO-UNDO.
   DEFINE VAR vErrorRKC    AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorBIS    AS LOGICAL        NO-UNDO.
   DEFINE VAR vErrorLst    AS CHAR           NO-UNDO.
   DEFINE VAR vTmpStr      AS CHAR           NO-UNDO.

   DEFINE VAR vStateKO     AS CHAR           NO-UNDO.
   DEFINE VAR vStateUBR    AS CHAR           NO-UNDO.
   DEFINE VAR vStatus      AS CHAR           NO-UNDO.

   DEFINE VAR vNote        AS CHAR           NO-UNDO.
   DEFINE VAR vResult      AS CHAR           NO-UNDO.
   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mRunKau      AS LOGICAL        NO-UNDO.
   DEFINE VAR vKauErr      AS INT64          NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vsendREF     AS CHAR           NO-UNDO.
   DEFINE VAR vsendID      AS CHAR           NO-UNDO.
   DEFINE VAR vRecvREF     AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDAT     AS DATE           NO-UNDO.
   DEFINE VAR vRecvID      AS CHAR           NO-UNDO.
   DEFINE VAR vPacketID243 AS INT64          NO-UNDO.
   DEFINE VAR mCodePage    AS CHAR           NO-UNDO.
   
   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").
   mCodePage    = TRNSettingValue(mSETClass, "XMLCodePage",   "ibm866").

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmSTAT", "1 hWOP:"  + hWOP:NAME  + 
                                         " hMain:" + hMain:NAME +
                                         " vInitialID:" + GetNullInt(vInitialID) +
                                         " vSurr:" + GetNullStr(vSurr)).
      &ENDIF

      ASSIGN
         vSendREF   = hWOP:buffer-field("SendREF"):buffer-value
         vSendID    = hWOP:buffer-field("SendID"):buffer-value
         vRecvDat   = hWOP:buffer-field("RecvDate"):buffer-value
         vRecvID    = hWOP:buffer-field("RecvID"):buffer-value
         vRecvREF   = hWOP:buffer-field("RecvREF"):buffer-value
      NO-ERROR.

/*----------------------------------------------- Поиск исходного документа --*/
      RUN ESIDOperationFind  (INPUT  hWOP,
                              INPUT  vPacketID,
                              BUFFER bCode,
                              OUTPUT vInitialID,
                              OUTPUT vSurr).     /* Суррогат объекта          */

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirmSTAT", "2 hWOP:"  + hWOP:NAME  + 
                                         " hMain:" + hMain:NAME +
                                         " vInitialID:" + GetNullInt(vInitialID) +
                                         " vSurr:" + GetNullStr(vSurr)).
      &ENDIF
      IF {assigned vSurr} THEN DO:

         IF CAN-DO("XML-ED244*",iFormat) THEN DO:
            vPacketID243 = 0.
            FOR EACH PackObject WHERE
                     PackObject.file-name =  'op-entry'
                 AND PackObject.Surrogate =  vSurr
                     NO-LOCK,
                EACH Packet WHERE
                     Packet.PacketID    =  PackObject.PacketID
                 AND Packet.mail-format BEGINS "XML-ED243" 
                     NO-LOCK,
                EACH Seance WHERE
                     Seance.DIRECT = {&DIR-EXPORT}
                 AND Seance.SeanceID = Packet.SeanceID NO-LOCK QUERY-TUNING(HINT "RULE"):

                ASSIGN
                   vPacketID243    = Packet.PacketID
                NO-ERROR.
            END.

            IF vPacketID243 <> 0 THEN DO:
               RUN PacketCreateLink (vPacketID243,
                                     "Packet",
                                      vPacketID,
                                     "ED244").
               RUN PacketSetState (vPacketID243,
                                   GetCodeEx("ESIDCode244","1","ПОДТ")).
            END.
         END.
      END.
      ELSE DO:
         RUN Fill-SysMes ("","ExchRKC32c","","%s=" + GetNullStr(iFormat)    +
                                       "%s=" + GetNullStr(vRecvID)    +
                                       "%s=" + GetNullStr(REPLACE(vRecvREF,"|",":"))   +
                                       "%s=" + GetNullDat(vRecvDAT)
                   ).
      END.
      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).


      RUN SetSysConf IN h_base ("InXMLCodePage",mCodePage).

      RUN PacketRKCTextSaveEX(vPacketID,iFormat,vSendID,vSendREF).
      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Метод IMPORT на классе XML-ED208                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirm208:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vReport           AS CHAR           NO-UNDO.

   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vParentID    AS INT64        NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm208 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm208 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vClassRef   = bcode.misc[4]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hWOP:buffer-field("RecvDate"):buffer-value
         vResultCode = hWOP:buffer-field("ResulrCode "):buffer-value
         vState      = GetCode("ESIDCode208",vResultCode)
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm208 ", "vClassRef:"  + string(vClassRef) +
                                         " vRecvRef:" + string(vRecvRef) 
                                         + " " + STRING(vRecvDate)).
      &ENDIF
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-EXPORT}
           AND Seance.SeanceID =  Packet.SeanceID /*NO-LOCK,
          EACH  PacketText WHERE
                PacketText.PacketID EQ Packet.PacketID*/
      NO-LOCK:
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm208 ", "4 vState:"  + GetNullStr(vState) +
                                         " vPPacketID:" + GetNullInt(vPPacketID)).
      &ENDIF
         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

      END.

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      vReport = "ResultCode:" + vResultCode.
      RUN PacketTextSave(vPacketID, vReport).

      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Метод IMPORT на классе XML-ED319                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirm319:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vSurr             AS CHAR           NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.

   DEFINE VAR mSETClass         AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind           AS CHAR           NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm319 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm319 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vClassRef   = bcode.misc[4]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hWOP:buffer-field("RecvDate"):buffer-value
         vResultCode = "1"
         vState      = GetCode("ESIDCode208",vResultCode)
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm319 ", "vClassRef:"  + string(vClassRef) +
                                         " vRecvRef:" + string(vRecvRef) 
                                         + " " + STRING(vRecvDate)).
      &ENDIF
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
           EACH Seance WHERE
                Seance.DIRECT =  {&DIR-EXPORT}
            AND Seance.SeanceID =  Packet.SeanceID
      NO-LOCK:
         ASSIGN
            vPPacketID       = Packet.PacketID
            vSurr            = PackObject.Surr
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm319 ", "4 vState:"  + GetNullStr(vState) +
                                         " vPPacketID:" + GetNullInt(vPPacketID)).
      &ENDIF

      IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

      END.

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      RUN PacketRKCTextSave(vPacketID).


      vFlagSet = YES.
   END.

   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm319 ", "5 vPPacketID:" + GetNullInt(vPPacketID)).
      &ENDIF
   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Извещение выполнении регламента системы БЭСП                               */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirm333:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR  hQuery           AS handle         NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vParentID         AS INT64        NO-UNDO.
   DEFINE VAR vResult           AS CHAR           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvID           AS CHAR           NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vPocCode          AS CHAR           NO-UNDO.
   DEFINE VAR vPocDesc          AS CHAR           NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}


      RUN XRKCPacketCheck IN h_xrkc (iFormat, hWOP) NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN 
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         vPacketID   = hMain:buffer-field("PacketID"):buffer-value
         vClassRef   = bcode.misc[5]
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hWOP:buffer-field("SendRef"):buffer-value)
         vRecvID     = hWOP:buffer-field("SendID"):buffer-value
         vRecvDate   = hWOP:buffer-field("SendDate"):buffer-value
         vPocCode    = hWOP:buffer-field("ProcessCode"):buffer-value
         vPocDesc    = hWOP:buffer-field("ProcessDescription"):buffer-value
      NO-ERROR. {&ON-ERROR}

      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID EXCLUSIVE-LOCK NO-WAIT.
      IF AVAIL Packet THEN DO:
         ASSIGN
            Packet.mail-format =
                       hWOP:buffer-field("mail-format"):buffer-value
         .
      END.
      FIND FIRST Packet WHERE Packet.PacketID =  vPacketID NO-LOCK NO-ERROR.
   
         vResult = "Код реквизита:" + vPocCode + '~n' +
                    vPocDesc + '~n'.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm333",  " iFormat:"          + GetNullStr(iFormat)        +
                                         " hWOP:name:"        + GetNullStr(hWOP:name)      +
                                         " vClassRef:"        + GetNullStr(vClassRef)      +
                                         " vRecvRef:"         + GetNullStr(vRecvRef)       +
                                         " bCode.Description[1]):" + ENTRY(2,bCode.Description[1]) +
                                         " vPocCode:"         + GetNullStr(vPocCode)       +
                                         " vPocDesc:"         + GetNullStr(vPocDesc) 
                     ).
      &ENDIF

      &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDConfirm333","vResult:" + vResult).
      &ENDIF

      RUN PacketTextSave(vPacketID,vResult).

      RUN PacketCreateRef (vRecvDate,
                           vPacketID,
                           vClassRef,
                           vRecvID + "|" +
                           ReferenceFormatValue(vClassRef,vRecvREF)).

      IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN
         RUN PacketBSPTextSave(vPacketID).

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   RUN InstanceDelete(hWOP). 
   {doreturn.i vFlagSet}
END PROCEDURE.
/******************************************************************************/

PROCEDURE ESIDConfirmCHANNEL.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.

   DEFINE VARIABLE vRecvDate     AS DATE        NO-UNDO.
   DEFINE VARIABLE vRecvREF      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInitClassRef AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPacketID     AS INT64       NO-UNDO.
   DEFINE VARIABLE vInitPackID   AS INT64       NO-UNDO.
   DEFINE VARIABLE vParentID     AS INT64       NO-UNDO.
   DEFINE VARIABLE vCode         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vResult       AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bPacket   FOR Packet.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   ASSIGN
      vRecvDate      = hExch::RecvDate
      vRecvREF       = hExch::RecvREF
      vInitClassRef  = bCode.misc[4]
      vPacketID      = hExch::PacketID
      vParentID      = hExch::ParentID
      vCode          = hExch::OperModeCode
   NO-ERROR. {&ON-ERROR}

   RUN ESIDInitFind (vRecvDate, vInitClassRef, ReferenceFormatValue(vInitClassRef, vRecvREF), 
                     OUTPUT vInitPackID).

   FIND FIRST bPacket WHERE bPacket.PacketID =  vInitPackID NO-ERROR.
   IF iFormat BEGINS "XML-ED209" THEN
      bPacket.State = "ОБРБ".
   ELSE IF iFormat BEGINS "XML-ED201" THEN
      bPacket.State = "ОШБК".

   RUN PacketCreateLink (vPacketID, "Packet", STRING(vInitPackID), 
                         "ED393") NO-ERROR.
   RUN PacketCreateLink (vInitPackID, "Packet", STRING(vPacketID), 
                         SUBSTR (ENTRY(2, iFormat, "-"), 1, 5)) NO-ERROR.

   IF vParentID >  0 THEN
   RUN PacketInclude    (INPUT  vPacketID,
                         INPUT  vParentID,
                         INPUT  {&STATE-FIN}).

   IF iFormat BEGINS "XML-ED209" THEN
      RUN PacketTextKeep(vPacketID,
                         GetNullStr(vCode) + " - " + GetNullStr(GetCodeName("ESIDMode",vCode)) + "~n" +
                         "Описание: " + GetNullStr(hExch::Description) + "~n" +
                         (IF hExch::DateBeg <> ?
                           THEN "Дата начала действия режима:" + GetNullDat(hExch::DateBeg)  
                           ELSE ""    ) + "~n" +
                         (IF hExch::DateEnd <> ? 
                            THEN "Дата конца  действия режима:" + GetNullDat(hExch::DateEnd)                
                            ELSE ""   ) + "~n~n",
                         INPUT-OUTPUT vResult).

   IF iFormat BEGINS "XML-ED201" THEN DO:
      vCode = hExch::ErrorList.
      RUN PacketTextKeep(vPacketID,
                         "Код: " + GetNullStr(vCode) +  " - " + GetXattrEx(hExch::ErrorClass, "u" + vCode, "Name") + "~n" +
                         "Описание: " + GetNullStr(hExch::Note) + "~n" +
                         GetNullStr(hExch::Description) + "~n~n",
                         INPUT-OUTPUT vResult).
   END.

   RUN PacketTextSave(vPacketID,vResult).

   IF NOT CAN-DO(TRNSettingValue("","MessagesNoSave",""),SUBSTRING(iFormat,5,5)) THEN DO:
      RUN PacketBSPTextSave(vPacketID).
   END.

   vFlagSet = YES.
END.  /* MAIN  */
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ESIDImport114:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR vStrTmp AS CHAR           NO-UNDO.

   hWOP::id273 = mId273 + 1.

END PROCEDURE.

PROCEDURE GetTT273:
   DEFINE INPUT PARAMETER iID   AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAM oIdP AS INT64 NO-UNDO.

   DEFINE VAR vStrTmp AS CHAR           NO-UNDO.


   FIND FIRST tt273 WHERE
              tt273.mail-format =  "XML-ED273"
          AND tt273.id          =  iId
   NO-LOCK NO-ERROR.
   IF AVAIL tt273 THEN
      oIdP = tt273.Pid.
   ELSE oIdP = 0.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED273                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport273:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR hDoc              AS handle         NO-UNDO.
   DEFINE VAR hSrv              AS handle         NO-UNDO.
   DEFINE VAR hRkc              AS handle         NO-UNDO.

   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDateDoc      AS DATE           NO-UNDO.
   DEFINE VAR vRecvRefDoc       AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vStateOp          AS CHAR           NO-UNDO.
   DEFINE VAR vSurr             AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDAT  AS DATE           NO-UNDO.
   DEFINE VAR vRecvID   AS CHAR           NO-UNDO.
   DEFINE VAR v__ID     AS INT64        NO-UNDO.
   DEFINE VAR v__UP     AS INT64        NO-UNDO.
   DEFINE VAR vSendREF     AS CHAR         NO-UNDO.
   DEFINE VAR vSendID      AS CHAR         NO-UNDO.
   DEFINE VAR mCodePage    AS CHAR         NO-UNDO.

   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vParentID    AS INT64        NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE BUFFER bpacket FOR packet.
   DEFINE BUFFER spacket FOR packet.

   ASSIGN
      mOpKind      = GetBaseOpKind()
      mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass")
      mCodePage    = TRNSettingValue(mSETClass, "XMLCodePage",   "ibm866")
      mCodePage    = "ibm866"          /* windows-1251 - не подходит */
      hSrv         = hWOP:DEFAULT-BUFFER-HANDLE
   NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport273 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      &IF DEFINED(IS-DEBUG) &THEN
         RUN DumpObject(hSrv).
      &ENDIF

      ASSIGN
         mId273    = mId273 + 1
         vSendREF  = hSrv:buffer-field("SendREF"):buffer-value
         vSendID   = hSrv:buffer-field("SendID"):buffer-value
      NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p (program-name(1),  "vSendID:" + vSendID).
         RUN dbgprint.p (program-name(1), "vSendREF:" + vSendREF).
      &ENDIF

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).
      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      CREATE tt273.
      ASSIGN
         tt273.Id  = mId273
         tt273.PId = vPacketID
         tt273.mail-format = "XML-ED273"
      .

      FIND FIRST spacket WHERE
                 spacket.PacketId =  vPacketID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL spacket THEN
         spacket.ParentID = vParentID.
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport273", "hWOP:" + hWOP:name + " __ID:" + STRING(hWoP::__ID) + " __UpID:" + STRING(hWoP::__UpID)).
      RUN dbgprint.p ("ESIDImport273", "hMain:" + hMain:name).
      RUN dbgprint.p ("ESIDImport273", "hMain::PacketId:" + string(hMain::PacketId)).
      RUN dbgprint.p ("ESIDImport273", "vPacketID:" + string(vPacketID)).
      RUN dbgprint.p ("ESIDImport273", "vParentID:" + string(vParentID)).
      IF AVAIL spacket THEN DO:
         RUN dbgprint.p ("ESIDImport273", "spacket.ParentID:" + string(spacket.ParentID)).
         RUN dbgprint.p ("ESIDImport273", "spacket.mail-format:" + spacket.mail-format).
      END.

      &ENDIF

      IF mText273_274 THEN DO:
         RUN SetSysConf IN h_base ("InXMLCodePage",mCodePage).
         RUN PacketRKCTextSaveEX(vPacketID,iFormat,vSendID,vSendREF).
      END.
      /*поиск документа*/
      vFlagSet = YES.
   END.

/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   ASSIGN
      hSrv:buffer-field("SendREF"):buffer-value   = ""
      hSrv:buffer-field("SendDate"):buffer-value  = ""
      hSrv:buffer-field("SendID"):buffer-value    = ""
   NO-ERROR.

   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*  аналог ESIDInitialSTMT ESIDConfirmSTMT                                    */
/* Метод IMPORT на классе XML-ED274                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport274:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR hDoc              AS handle         NO-UNDO.
   DEFINE VAR hSrv              AS handle         NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDateDoc      AS DATE           NO-UNDO.
   DEFINE VAR vRecvRefDoc       AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vStateOp          AS CHAR           NO-UNDO.
   DEFINE VAR vSurr             AS CHAR           NO-UNDO.

   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vParentID    AS INT64          NO-UNDO.
   DEFINE VAR vsendREF     AS CHAR           NO-UNDO.
   DEFINE VAR vsendID      AS CHAR           NO-UNDO.
   DEFINE VAR v__ID        AS INT64          NO-UNDO.
   DEFINE VAR v__UP        AS INT64          NO-UNDO.

   DEFINE VAR vAcptDate    AS DATE           NO-UNDO.
   DEFINE VAR vAcptSum     AS DECIMAL        NO-UNDO.
   DEFINE VAR vSumPT       AS DECIMAL        NO-UNDO.
   DEFINE VAR vCode        AS CHAR           NO-UNDO.
   DEFINE VAR vErrors      AS CHAR           NO-UNDO.
   DEFINE VAR vPckState    AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport274 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport274 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         hDoc         = ObjectValueHandle("ExchRKCDoc")             /* Общее сообщение */
         vClassRef    = bcode.misc[4]
         vRecvRefDoc  = ReferenceFormatValue(vClassRef,
                                             hDoc:buffer-field("RecvRef"):buffer-value) 
         vRecvDateDoc = hDoc:buffer-field("RecvDate"):buffer-value
         vState       = "ПОДТ"
         hSrv = ObjectValueHandle("ExchRKCSrv")             /* Общее сообщение */
         vRecvRef    = ReferenceFormatValue(vClassRef,
                                            hSrv:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hSrv:buffer-field("RecvDate"):buffer-value
         vSendREF    = hWOP:buffer-field("SendREF"):buffer-value
         vSendID     =  hWOP:buffer-field("SendID"):buffer-value
         vAcptDate   = hWOP:buffer-field("AcptDate"):buffer-value
         vAcptSum    = hWOP:buffer-field("AcptSum"):buffer-value
         vSumPT      = hWOP:buffer-field("SumPT"):buffer-value
         vCode       = hWOP:buffer-field("ErrorList"):buffer-value
      NO-ERROR. {&ON-ERROR}

      vStateOp = GetCode("ESIDCode274", vCode).
      IF NOT {assigned vStateOp} THEN
         vStateOp = TRNSettingValue(mSETClass, "PostOpStat","").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport274 2-1", "vClassRef:"  + string(vClassRef) +
                                         " vRecvRef:" + string(vRecvRef) 
                                         + " " + STRING(vRecvDate) + 
                                         " valid-handle:" + string(valid-handle(hSrv))).
      RUN dbgprint.p ("ESIDImport274 2-2", "vClassRefd:"  + string(vClassRef) +
                                         " vRecvRefd:" + string(vRecvRefDoc) 
                                         + " " + STRING(vRecvDateDoc) + 
                                         " valid-handle:" + string(valid-handle(hDoc))).
      &ENDIF
      /* поиск сообщения 273 */
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDateDoc
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRefDoc
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-EXPORT}
           AND Seance.SeanceID =  Packet.SeanceID 
               NO-LOCK:
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.

         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         RUN PacketCreateLink  (INPUT  vPacketID, 
                                INPUT  "Packet",
                                INPUT  STRING(vPPacketID),
                                INPUT  "ED273").

         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

      END.

      /*поиск сообщения 113/114 */
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
           EACH Seance WHERE
                Seance.DIRECT =  {&DIR-EXPORT}
            AND Seance.SeanceID =  Packet.SeanceID 
                NO-LOCK:

         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
         RUN PacketCreateLink  (INPUT  STRING(vPacketID),
                                INPUT  "Packet",
                                INPUT  vPPacketID,
                                INPUT  ENTRY(2,bCode.Description[1])).

         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

         FOR FIRST PackObject WHERE
                   PackObject.PacketID  =  Packet.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
              FIRST op WHERE
                    op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                    EXCLUSIVE-LOCK,
              FIRST op-entry OF op
         EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

            RUN PacketCreateLink  (INPUT  STRING(vPacketID),
                                   INPUT  "op-entry",
                                   INPUT  PackObject.Surrogate,
                                   INPUT  ENTRY(2,bCode.Description[1])).

            IF vCode =  "4" THEN DO: /* проверки только при частичном акцепте */

               IF vAcptSum <> op-entry.amt-rub THEN DO:
                  {additem.i vErrors '"b8100"'}
               END.
               IF vSumPT <> op-entry.amt-rub THEN DO:
                  {additem.i vErrors '"b8110"'}
               END.
               IF {assigned vErrors} THEN
                  RUN PacketSetError in h_pack (vPacketID,
                                                hWOP::ErrorClass,
                                                vErrors,
                                                OUTPUT vPckState).
            END.

            IF {assigned vStateOp} THEN DO:
               {opent-st.i &status=vStateOp}       /* Изменение статусов док-та */
            END.

            IF vAcptDate <> ? THEN
               UpdateSigns("op", STRING(op.op), "AcptDate", STRING(vAcptDate), YES).

            IF vAcptSum <> ? THEN
               UpdateSigns("op", STRING(op.op), "AcptSum", STRING(vAcptSum), YES).

            IF vSumPT <> ? THEN
               UpdateSigns("op", STRING(op.op), "SumPT", STRING(vSumPT), YES).

         END.

      END.

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      IF mText273_274 THEN 
      RUN PacketRKCTextSaveEX(vPacketID,iFormat,vSendID,vSendREF).

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*  аналог ESIDInitialSTMT ESIDConfirmSTMT                                    */
/* Метод IMPORT на классе XML-ED275                                           */
/*----------------------------------------------------------------------------*/

PROCEDURE ESIDImport275:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR hDoc              AS handle         NO-UNDO.
   DEFINE VAR hSrv              AS handle         NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64        NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64        NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvId           AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDateDoc      AS DATE           NO-UNDO.
   DEFINE VAR vRecvRefDoc       AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vStateOp          AS CHAR           NO-UNDO.
   DEFINE VAR vSurr             AS CHAR           NO-UNDO.
   DEFINE VAR vFlagRefFind      AS LOG            NO-UNDO.
   DEFINE VAR vFlagOpFind       AS LOG            NO-UNDO.

  
   DEFINE VAR vAmtRub           AS DEC            NO-UNDO.
   DEFINE VAR vBenAcct          AS CHAR           NO-UNDO.
   DEFINE VAR vBicRec           AS CHAR           NO-UNDO.
   DEFINE VAR vSendAcct         AS CHAR           NO-UNDO.

   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vParentID    AS INT64          NO-UNDO.
   DEFINE VAR v__ID        AS INT64          NO-UNDO.
   DEFINE VAR v__UP        AS INT64          NO-UNDO.
   DEFINE VAR vStateOp1    AS CHAR           NO-UNDO.
   DEFINE VAR vStateOp2    AS CHAR           NO-UNDO.
   DEFINE VAR vStateOpNot  AS CHAR           NO-UNDO.
   DEFINE VAR vInfoCode    AS CHAR           NO-UNDO.
   DEFINE VAR vRetValue    AS CHAR           NO-UNDO.
   DEFINE VAR vK2          AS CHAR           NO-UNDO.
   DEFINE VAR vReport      AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   ASSIGN
      mOpKind      = GetBaseOpKind()
      mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass")
      vStateOp1    = FGetSetting("УФЭБС","Статус275-1","")
      vStateOp2    = FGetSetting("УФЭБС","Статус275-2","")
      vStateOpNot  = FGetSetting("УФЭБС","СтБезотз","!*")
   .
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport275 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport275 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         hDoc         = ObjectValueHandle("ExchRKCDoc")             /* Общее сообщение */
         vClassRef    = bcode.misc[{&RKC-REPLY}]
         vRecvRefDoc  = ReferenceFormatValue(vClassRef,
                                             hDoc:buffer-field("SendRef"):buffer-value) 
         vRecvDateDoc = hDoc:buffer-field("SendDate"):buffer-value
         vState       = "ПОДТ"
         hSrv         = ObjectValueHandle("ExchRKCSrv")             /* данные для поиска */
         vRecvRef     = ReferenceFormatValue(vClassRef,
                                            hSrv:buffer-field("RecvRef"):buffer-value) 
         vRecvDate   = hSrv:buffer-field("RecvDate"):buffer-value
         vRecvID     = hSrv:buffer-field("RecvID"):buffer-value
         vAmtRub     = hDoc:buffer-field("amt-rub"):buffer-value
         vBenAcct    = hDoc:buffer-field("acct-rec"):buffer-value
         vBicRec     = hDoc:buffer-field("bank-code-rec"):buffer-value
         vSendAcct   = hDoc:buffer-field("acct-send-out"):buffer-value
      NO-ERROR. {&ON-ERROR}

      vReport = "EDNo    :" + vRecvRefDoc               + "~n" +
                "EDDate  :" + STRING(vRecvDateDoc)      + "~n" +
                "EDAuthor:" + hDoc::SendID              + "~n" +
                "Sum     :" + hDoc::amt-rub             + "~n" +
                "PayerPersonalAcc:" + vSendAcct         + "~n" +
                "PayeeBIC        :" + vBICRec           + "~n" +
                "PayeePersonalAcc:" + vBenAcct          + "~n" +
                "EDRefID.EDNo    :" + vRecvRef          + "~n" +
                "EDRefID.EDDate  :" + STRING(vRecvDate) + "~n".

      RUN PacketTextSave(vPacketID, vReport).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport275 2-1", "vClassRef:"  + string(vClassRef) +
                                         " vRecvRef:" + string(vRecvRef) 
                                         + " " + STRING(vRecvDate) + 
                                         " valid-handle:" + string(valid-handle(hSrv)) + 
                                         " vRecvID:" +  vRecvID).
      RUN dbgprint.p ("ESIDImport275 2-2", "vClassRefd:"  + string(vClassRef) +
                                         " vRecvRefd:" + string(vRecvRefDoc) 
                                         + " " + STRING(vRecvDateDoc) + 
                                         " valid-handle:" + string(valid-handle(hDoc))).
      &ENDIF
      /*поиск документа*/

      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvID + "|" + vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-IMPORT}
           AND Seance.SeanceID =  Packet.SeanceID 
      NO-LOCK:
         vFlagRefFind = YES.
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])) NO-ERROR. {&ON-ERROR}
   
         FOR FIRST PackObject WHERE
                   PackObject.PacketID  =  Packet.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
              FIRST op WHERE
                    op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                    EXCLUSIVE-LOCK,
              FIRST op-entry OF op
         EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):


            FIND FIRST op-bank OF op WHERE
                       op-bank.op-bank-type =  "" NO-LOCK NO-ERROR.
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDImport275 ", "AVAIL op-bank:"  + STRING(AVAIL op-bank) +
                                              " vAmtRub:" + STRING(vAmtRub) +
                                              " op-entry.amt-rub:" + STRING(op-entry.amt-rub) +
                                              " vBenAcct:" + GetNullStr(vBenAcct) +
                                              " op.ben-acct:" + GetNullStr(op.ben-acct) +
                                              " op-bank.bank-code:" + (if avail op-bank then GetNullStr(op-bank.bank-code) else "") +
                                              " vBicRec:" + GetNullStr(vBicRec) +
                                              " vSendAcct:" + GetNullStr(vSendAcct) +
                                              " op-entry.acct-db:" + GetNullStr(op-entry.acct-db) +
                                              " op.Send-Acct:" + GetNullStr(GetXattrValueEx("op", STRING(op.op),"acct-send",""))
                           ).
            &ENDIF

            IF AVAIL op-bank AND
               (vAmtRub  =  op-entry.amt-rub  OR
                vAmtRub  =  DEC(GetXattrValueEx("op", STRING(op.op), "amt-rub", "0"))
               ) AND
               vBenAcct  =  op.ben-acct AND
               vBicRec   =  op-bank.bank-code AND
              (vSendAcct =  GetXattrValueEx("op", STRING(op.op),"acct-send","") OR 
               vSendAcct =  DelFilFromAcct(op-entry.acct-db))     THEN
            DO:

               vFlagOpFind = YES.
               RUN PacketCreateLink  (INPUT  vPacketID,
                                      INPUT  "op-entry",
                                      INPUT  PackObject.Surrogate,
                                      INPUT  ENTRY(2,bCode.Description[1])).
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("ESIDImport275 ", "vStateOpNot:"  + GetNullStr(vStateOpNot) +
                                                 " op.op-status:" + GetNullStr(op.op-status)).
               &ENDIF
               IF CAN-DO(vStateOpNot,op.op-status) THEN DO:
                  IF NOT UpdateSigns("op",
                                     string(op.op),
                                     "InfoCode276",
                                     "2",
                                     YES) THEN
                     UNDO MAIN, RETRY MAIN.
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("ESIDImport275 ", "UpdS:yes").
                  &ENDIF
               END.
               ELSE DO:
                  vK2 = CheckED275K2(RECID(op)).
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("ESIDImport275 ", "vK2: " + vK2).
                  &ENDIF
                  IF NOT {assigned vK2} THEN vK2 = "2".
                  vK2 = IF vK2 =  "1" THEN "1" ELSE "2".
                  vStateOp = IF vK2 =  "1" THEN vStateOp1 ELSE vStateOp2.
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("ESIDImport275 ", "vInfoCode:"  + GetNullStr(vInfoCode) +
                                                    " vStateOp1:" + GetNullStr(vStateOp1) + 
                                                    " vStateOp2:" + GetNullStr(vStateOp2) + 
                                                    " vStateOp:"  + GetNullStr(vStateOp)).
                  &ENDIF
                  IF NOT UpdateSigns("op",
                                     string(op.op),
                                     "InfoCode276",
                                     vK2,
                                     YES) THEN
                     UNDO MAIN, RETRY MAIN.
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("ESIDImport275 ", "UpdS2:yes").
                  &ENDIF
                  IF {assigned vStateOp} THEN DO:

                     {opent-st.i &status=vStateOp}       /* Изменение статусов док-та */

                     &IF DEFINED(IS-DEBUG) &THEN
                     RUN dbgprint.p ("ESIDImport275 ", "After opent-st.i").
                     &ENDIF
                  END.
               END.
            END.
         END.
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport275 ", "3 vState:"  + GetNullStr(vState) +
                                            " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport275 ", "vFlagOpFind:"  + STRING(vFlagOpFind) +
                                        " vFlagRefFind:" + STRING(vFlagRefFind)).
      &ENDIF
      IF NOT vFlagRefFind OR NOT vFlagOpFind THEN DO:
         IF NOT vFlagRefFind THEN
            RUN Fill-SysMes("","ExchRKC53","","%s=" + GetNullStr(vRecvRef) +
                                              "%s=" + GetNullStr(STRING(vRecvDate)) +
                                              "%s=" + GetNullStr(vRecvID) +
                                              "%s=" + GetNullStr(vClassRef)
                           ).
         ELSE
            RUN Fill-SysMes("","ExchRKC52","","%s=" + STRING(vAmtRub) +
                                              "%s=" + GetNullStr(vBenAcct) +
                                              "%s=" + GetNullStr(vBicRec) +
                                              "%s=" + GetNullStr(vSendAcct)
                           ).
         vRetValue = "SKIP".
         /*
         vFlagSet  = YES.
         LEAVE MAIN.*/
      END.

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*  аналог ESIDInitialSTMT ESIDConfirmSTMT                                    */
/* Метод IMPORT на классе XML-ED276                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport276:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR hDoc              AS handle         NO-UNDO.
   DEFINE VAR hSrv              AS handle         NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vRecvDateDoc      AS DATE           NO-UNDO.
   DEFINE VAR vRecvRefDoc       AS CHAR           NO-UNDO.
   DEFINE VAR vResultCode       AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.
   DEFINE VAR vStateOp          AS CHAR           NO-UNDO.
   DEFINE VAR vSurr             AS CHAR           NO-UNDO.
   DEFINE VAR vFlAvailOp        AS LOG            NO-UNDO.
   DEFINE VAR vReport           AS CHAR           NO-UNDO.

   DEFINE VAR mSETClass         AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind           AS CHAR           NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.

   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport276 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport276 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN
         hDoc         = ObjectValueHandle("ExchRKCDoc")             /* InitialED */
         vClassRef    = bcode.misc[4]
         vRecvRefDoc  = ReferenceFormatValue(vClassRef,
                                             hDoc:buffer-field("RecvRef"):buffer-value) 
         vRecvDateDoc = hDoc:buffer-field("RecvDate"):buffer-value
         vState       = "ПОДТ"
         hSrv         = ObjectValueHandle("ExchRKCSrv")             /* Общее сообщение, EDRefID */
         vRecvRef     = ReferenceFormatValue(vClassRef,
                                            hSrv:buffer-field("RecvRef"):buffer-value) 
         vRecvDate    = hSrv:buffer-field("RecvDate"):buffer-value
         vStateOp     = hSrv::ErrorList 
      NO-ERROR. {&ON-ERROR}

      vReport = "EDNo    :" + vRecvRef + "~n" +
                "EDDate  :" + STRING(vRecvDate) + "~n" +
                "InfoCode:" + vStateOp.

      RUN PacketTextSave (vPacketID, vReport).


      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport276 2-1", "vClassRef:"  + string(vClassRef) +
                                         " vRecvRef:" + string(vRecvRef) 
                                         + " " + STRING(vRecvDate) + 
                                         " valid-handle:" + string(valid-handle(hSrv))).
      RUN dbgprint.p ("ESIDImport276 2-2", "vClassRefd:"  + string(vClassRef) +
                                         " vRecvRefd:" + string(vRecvRefDoc) 
                                         + " " + STRING(vRecvDateDoc) + 
                                         " valid-handle:" + string(valid-handle(hDoc))).
      &ENDIF
      /*поиск документа*/
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDateDoc
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRefDoc
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-EXPORT}
           AND Seance.SeanceID =  Packet.SeanceID
      NO-LOCK:
         /* нашли 275 сообщение */
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport276 ", "3 vState:"  + GetNullStr(vState) +
                                            " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).

      END.
      /*поиск сообщения 113/114*/
      FOR FIRST Reference WHERE
                Reference.op-date    =  vRecvDate
            AND Reference.Class-Code =  vClassRef
            AND Reference.RefValue   =  vRecvRef
                NO-LOCK,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-EXPORT}
           AND Seance.SeanceID =  Packet.SeanceID
      NO-LOCK:
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.

         FOR FIRST PackObject WHERE
                   PackObject.PacketID  =  Packet.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
              FIRST op WHERE
                    op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                    EXCLUSIVE-LOCK,
              FIRST op-entry OF op
         EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):
            vFlAvailOp = YES.
            IF vStateOp =  "1" AND {assigned mImpSt1-276} THEN DO:
               {opent-st.i &status=mImpSt1-276}       /* Изменение статусов док-та */
            END.
            ELSE IF vStateOp =  "2" AND {assigned mImpSt2-276} THEN DO:
               {opent-st.i &status=mImpSt2-276}       /* Изменение статусов док-та */
            END.

            RUN PacketCreateLink  (INPUT  STRING(vPacketID),
                                   INPUT  "op-entry",
                                   INPUT  PackObject.Surrogate,
                                   INPUT  ENTRY(2,bCode.Description[1])).
         END.

         RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
         RUN PacketCreateLink  (INPUT  STRING(vPacketID),
                                INPUT  "Packet",
                                INPUT  vPPacketID,
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDInitial276 ", "3 vState:"  + GetNullStr(vState) +
                                            " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
         /*
         IF {assigned vState} THEN
            RUN PacketSetState (vPPacketID,vState).
         */
      END.
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDInitial276 ", "vFlAvailOp:"  + STRING(vFlAvailOp)).
      &ENDIF
      IF vFlAvailOp =  NO THEN
         RUN Fill-SysMes("","","","Не найден документ при импорте ED276" +
                                  " Дата:"         + STRING(vRecvDate)   +
                                  " Класс ссылки:" + vClassRef           +
                                  " Ссылка:"       + vRecvRef).
 
      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*  аналог ESIDInitialSTMT ESIDConfirmSTMT                                    */
/* Метод IMPORT на классе XML-ED274                                           */
/*----------------------------------------------------------------------------*/
/*

Export       ESIDRequest243,esid
FltStructX   xatrspc-f
FltXTrapX    xatrxml-t
Import       ESIDConfirm243,esid
Initial      XRKCPacketCheck,xrk


Export       ESIDRequest244,esid
FltStructX   xatrspc-f
FltXTrapX    xatrxml-t
Import       ESIDConfirm243,esid
Initial      emptymtd
 */

PROCEDURE ED207Import.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hWOP    AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vPacketID AS INT64       NO-UNDO.
   DEFINE VARIABLE vReport   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hExch     AS HANDLE      NO-UNDO.

   vPacketID = INT64(GetSysConf("ED207ParentID")) NO-ERROR.
   hExch     = hWOP:DEFAULT-BUFFER-HANDLE.

   IF vPacketID <> ? AND vPacketID <> 0 THEN DO:
      IF NOT CAN-FIND(FIRST PacketText WHERE PacketText.PacketID =  vPacketID NO-LOCK) THEN DO:
         RUN CreateED207Report (hExch, OUTPUT vReport).
         RUN PacketTextSave (vPacketID, vReport).
      END.
   END.

   RUN SetSysConf IN h_base ("ED207ParentID", ?).

   RUN XRKCServiceDelete IN h_xrkc (iFormat, hWOP).

END PROCEDURE.


PROCEDURE CreateED207Report.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.
   DEFINE OUTPUT PARAMETER oReport AS CHARACTER   NO-UNDO.

   DEFINE BUFFER Xattr FOR Xattr.

   FOR EACH Xattr WHERE Xattr.Class-Code  =  "XMLAttr-ED207"
                    AND Xattr.Description =  "ATTR"
      NO-LOCK
      BREAK BY Xattr.order:
      oReport = oReport + Xattr.Name + ":" + 
                GetNullStr(STRING(hExch:BUFFER-FIELD(Xattr.xattr-clabel):BUFFER-VALUE)) + "~n".
   END.

END PROCEDURE.

PROCEDURE CreateEDInfoReport.
   DEFINE INPUT        PARAMETER iCls    AS CHARACTER   NO-UNDO.
   DEFINE INPUT        PARAMETER hExch   AS HANDLE      NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER oReport AS CHARACTER   NO-UNDO.

   DEFINE BUFFER Xattr FOR Xattr.

   FOR EACH Xattr WHERE Xattr.Class-Code  =  iCls
                    AND Xattr.Description =  "ATTR"
      NO-LOCK
      BREAK BY Xattr.order:
      oReport = oReport + IF {assigned Xattr.Name} THEN Xattr.Name ELSE Xattr.xattr-code.
      oReport = oReport + ":" + 
                GetNullStr(STRING(hExch:BUFFER-FIELD(Xattr.xattr-clabel):BUFFER-VALUE)) + "~n".
   END.

END PROCEDURE.


PROCEDURE ReestrChgStatus.
   DEFINE INPUT  PARAMETER iOpSv AS INT64       NO-UNDO.
   DEFINE INPUT  PARAMETER iStat AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.
   DEFINE VARIABLE vLinks   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI       AS INT64       NO-UNDO.
   DEFINE VARIABLE vOp      AS INT64       NO-UNDO.
   
   DEFINE BUFFER op  FOR op.
   DEFINE BUFFER xop FOR op.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   FIND FIRST op WHERE op.op =  iOpSv NO-LOCK NO-ERROR. {&ON-ERROR}

   vLinks = GetLinks (op.class-code,
                      STRING(op.op),
                      ?,
                      "reestr_rkc",
                      ",",
                      op.op-date).

   DO vI = 1 TO NUM-ENTRIES(vLinks) :
      vOp = INT64(ENTRY(vI, vLinks)).
      FIND FIRST xop WHERE xop.op =  vOp EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL xop THEN DO:
         xop.op-status = iStat.
         VALIDATE xop NO-ERROR. {&ON-ERROR}
      END.
      ELSE IF LOCKED xop THEN DO:
         FIND FIRST xop WHERE xop.op =  vOp NO-LOCK NO-ERROR. {&ON-ERROR}
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Невозможно установть статус " + iStat + " для документа реестра " + 
                                                   xop.doc-num + ": документ заблокирован другим пользователем.").
      END.
      ELSE UNDO MAIN, RETRY MAIN.

   END.

   vFlagSet = YES.
END. /* MAIN */

{doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED408                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport408:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS HANDLE   NO-UNDO.


   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.
   DEFINE VAR vReport           AS CHARACTER      NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vState            AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE BUFFER Reference FOR Reference.   
   DEFINE BUFFER Packet    FOR Packet.   
   DEFINE BUFFER Seance    FOR Seance.   

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).


      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport408 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

      IF {assigned hExch::RecvRef} THEN DO:
         ASSIGN
            vClassRef   = bCode.misc[4]
            vRecvDate   = hExch:buffer-field("RecvDate"):buffer-value
            vRecvRef    = ReferenceFormatValue(vClassRef,
                                               hExch:buffer-field("RecvRef"):buffer-value) 
         NO-ERROR. {&ON-ERROR}

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport408 2", "vClassRef:"  + string(vClassRef) +
                                            " vRecvRef:" + string(vRecvRef) 
                                             + " " + STRING(vRecvDate)).
         &ENDIF

         /* поиск исходного сообщения */
         vPPacketID = -1.
         FOR FIRST Reference WHERE
                   Reference.op-date    =  vRecvDate
               AND Reference.Class-Code =  vClassRef
               AND Reference.RefValue   =  vRecvRef
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID      =  Reference.PacketID
                   NO-LOCK,
             EACH Seance WHERE
                  Seance.DIRECT =  {&DIR-EXPORT}
              AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK:

            ASSIGN
               vPPacketID       = Packet.PacketID
            NO-ERROR.

            RUN PacketCreateLink  (INPUT  vPacketID,
                                   INPUT  "Packet",
                                   INPUT  STRING(vPPacketID),
                                   INPUT  ENTRY(1,bCode.Description[1])).
            RUN PacketCreateLink  (INPUT  vPPacketID,
                                   INPUT  "Packet",
                                   INPUT  STRING(vPacketID),
                                   INPUT  ENTRY(2,bCode.Description[1])).

            CASE STRING(hExch::ResulrCode) :
               WHEN "1" THEN
                  vState = "ОБРБ".
               WHEN "2" OR 
               WHEN "6" THEN
                  vState = "ОШБК".
               WHEN "3" THEN
                  IF (Packet.mail-format =  "XML-ED428") OR
                     (Packet.mail-format =  "XML-ED429")THEN
                     vState = "ПОДТ".
               WHEN "5" THEN
                  IF Packet.mail-format <> "XML-ED429" THEN
                     vState = "ОШБК".
               WHEN "7" THEN 
                  IF Packet.mail-format =  "XML-ED425" THEN
                     vState = "ПОДТ".
            END CASE.
      
            IF {assigned vState} THEN
               RUN PacketSetState (vPPacketID,vState).

         END.  /* FOR */

         IF vPPacketID =  -1 THEN 
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не найдено исходное сообщение ED4XX.").
      END.     /* IF */


      /*  Создание текста отчета  */
      RUN CreateEDInfoReport("XMLAttr-ED408",    hExch, INPUT-OUTPUT vReport).
      RUN CreateEDInfoReport("XML-ED408",        hExch, INPUT-OUTPUT vReport).
      RUN CreateEDInfoReport("XMLAttr-RefIDSrv", hExch, INPUT-OUTPUT vReport).

      RUN PacketTextSave (vPacketID,
                          vReport).

      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED499                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImportED499.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.


   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bPacket   FOR Packet.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                          OUTPUT       hMain).

   RUN ESIDPacketCreate  (INPUT  hExch,
                          INPUT  hMain,
                          BUFFER bCode,
                          OUTPUT vPacketID,
                          OUTPUT vParentID).

   FOR FIRST Packet WHERE 
             Packet.PacketID =  vPacketID
             EXCLUSIVE-LOCK:
      ASSIGN
         Packet.ParentID  = vParentID
      NO-ERROR.
   END.

   RUN PacketRKCTextSave(vPacketID).

   vFlagSet = YES.
END.  /* MAIN  */
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE ED213Import.

   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.

   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR hDoc              AS HANDLE         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bPacket   FOR Packet.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.

   RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                          OUTPUT       hMain).

   RUN ESIDPacketCreate  (INPUT  hExch,
                          INPUT  hMain,
                          BUFFER bCode,
                          OUTPUT vPacketID,
                          OUTPUT vParentID).

   FOR FIRST Packet WHERE 
             Packet.PacketID =  vPacketID
             EXCLUSIVE-LOCK:
      ASSIGN
         Packet.ParentID  = vParentID
      NO-ERROR.
   END.

   RUN PacketRKCTextSave(vPacketID).

   hDoc = ObjectValueHandle("ExchRKCDoc").
   hDoc::Id213 = vPacketID.

   RUN XRKCServiceDelete (iFormat, hExch:TABLE-HANDLE).

   vFlagSet = YES.
END.  /* MAIN  */

{doreturn.i vFlagSet}
END PROCEDURE.



/*----------------------------------------------------------------------------*/
/*  аналог ESIDInitialSTMT ESIDConfirmSTMT                                    */
/* Метод IMPORT на классе XML-ED541                                         */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport541:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE VAR hMain             AS handle         NO-UNDO.
   DEFINE VAR hSrv              AS handle         NO-UNDO.
   DEFINE VAR vInitClassRef     AS CHAR           NO-UNDO.
   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vRef              AS CHARACTER      NO-UNDO.
   DEFINE VAR mSETClass    AS CHAR           NO-UNDO.
   DEFINE VAR mOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vParentID    AS INT64          NO-UNDO.
   DEFINE VAR v__ID        AS INT64          NO-UNDO.
   DEFINE VAR v__UP        AS INT64          NO-UNDO.
   DEFINE VARIABLE vEDDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vHsrv         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vState540     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPPacketID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vEDNo         AS CHARACTER   NO-UNDO.
   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport274 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hWOP,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hWOP,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport274 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
    
/*---------------------------------------- Регистрация полученной квитанции --*/
      ASSIGN 
                  vhSrv     =    ObjectValueHandle("ExchRKCStm")
                  vEDDate   =     DATE(vhSrv:buffer-field("RecvDate"):buffer-value)
                  vInitClassRef =  bcode.misc[4]
                  vRef    = ReferenceFormatValue(vInitClassRef,
                                            vhSrv:buffer-field("RecvRef"):buffer-value)  
                  vState540       = "ОБРБ"      .     
            FOR FIRST Reference WHERE
                Reference.op-date    =  vEDDate
            AND Reference.Class-Code =  vInitClassRef
            AND Reference.RefValue   =  vRef
                NO-LOCK
                 ,
          FIRST Packet WHERE
                Packet.PacketID      =  Reference.PacketID
                NO-LOCK,
          EACH Seance WHERE
               Seance.DIRECT =  {&DIR-EXPORT}
           AND Seance.SeanceID =  Packet.SeanceID 
                NO-LOCK  :
                
           ASSIGN
            vPPacketID       = Packet.PacketID
              NO-ERROR.
            RUN PacketCreateLink  (INPUT  vPPacketID,
                                INPUT  "Packet",
                                INPUT  STRING(vPacketID),
                                INPUT  ENTRY(2,bCode.Description[1])).
   
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport541 ", "3 vState540:"  + GetNullStr(vState540) +
                                            " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
         IF {assigned vState540} THEN
            RUN PacketSetState (vPPacketID,vState540).
          
                   
          END.  
                                           
            RUN PacketInclude    (INPUT  vPacketID,
                                  INPUT  vParentID,
                                  INPUT  {&STATE-FIN}). 
                                RUN PacketRKCTextSave (vPacketID).
          
         
         vFlagSet = YES.
        
      END. /* ED541 */
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hWOP:buffer-field("__ID"):buffer-value
      v__UP = hWOP:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hWOP, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED574                                                               */
/* Метод IMPORT на классe XML-ED574                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport574:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE hMain             AS handle              NO-UNDO.
   DEFINE VARIABLE hStm              AS handle              NO-UNDO.  
   DEFINE VARIABLE vInitClassRef     AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE vPacketID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vRef              AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mSETClass         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mOpKind           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vParentID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vEDDate           AS DATE                NO-UNDO.
   DEFINE VARIABLE vhSrv             AS HANDLE              NO-UNDO.
   DEFINE VARIABLE vState573         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vPPacketID        AS INT64               NO-UNDO.
   DEFINE VARIABLE vEDNo             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE v__ID             AS INT64               NO-UNDO.
   DEFINE VARIABLE v__UP             AS INT64               NO-UNDO.   
   
   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport574 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport574 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
      
      ASSIGN 
         hStm           = ObjectValueHandle("ExchRKCStm")
         vEDDate        = DATE(hStm:buffer-field("RecvDate"):buffer-value)
         vInitClassRef  = bcode.misc[4]
         vRef           = ReferenceFormatValue(vInitClassRef,
                                   hStm:buffer-field("RecvREF"):buffer-value)  
         vState573      = "ОБРБ"                
      .                 

/*---------------------------------------- Сквитовка с запросом ED573 --*/            
  
      FOR FIRST Reference WHERE
          Reference.op-date    =  vEDDate
      AND Reference.Class-Code =  vInitClassRef
      AND Reference.RefValue   =  vRef
          NO-LOCK
         ,
      FIRST Packet WHERE
         Packet.PacketID      =  Reference.PacketID
         NO-LOCK,
      EACH Seance WHERE
         Seance.DIRECT =  {&DIR-EXPORT}
      AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK  :
             
         ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                             INPUT  "Packet",
                             INPUT  STRING(vPacketID),
                             INPUT  ENTRY(2,bCode.Description[1])).
                             
         RUN PacketCreateLink  (INPUT  vParentID,
                             INPUT  "Packet",
                             INPUT  STRING(vPPacketID),
                             INPUT  "ED573").                             

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport574 ", "3 vState573:"  + GetNullStr(vState573) +
                                         " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
      
         IF {assigned vState573} THEN
            RUN PacketSetState (vPPacketID,vState573).
                          
      END.               

      RUN PacketInclude    (INPUT  vPacketID,
                           INPUT  vParentID,
                           INPUT  {&STATE-FIN}).
                                                              
      RUN PacketRKCTextSave (vPacketID).
                   
      vFlagSet = YES.
              
   END. /* ED574 */   
   
/*-------------------------------------------- Удаление текущего экземпляра --*/      
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).
   

   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED574    аналог ESIDInitialSTMT ESIDConfirmSTMT                      */
/* Метод IMPORT на классe ImportED574                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ImportED574:
   DEFINE INPUT PARAMETER iFormat      AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE hStm              AS handle              NO-UNDO.
   DEFINE VARIABLE hExchSrv          AS handle              NO-UNDO.
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE mSETClass         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mOpKind           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vUIS              AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vBegDate          AS DATE                NO-UNDO.
   DEFINE VARIABLE vEndDate          AS DATE                NO-UNDO.   
   DEFINE VARIABLE vSWIFTBIC         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vBIC              AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vServicesCode     AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vESSize           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vARMNo            AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vIsFreeServices   AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vName             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vIsSuspensedExchange   AS INT64          NO-UNDO.
   DEFINE VARIABLE vIsNotDailyDispatchES  AS INT64          NO-UNDO.
   DEFINE VARIABLE v__ID             AS INT64               NO-UNDO.
   DEFINE VARIABLE v__UP             AS INT64               NO-UNDO.      
   
   DEFINE BUFFER bCode FOR Code.
   DEFINE BUFFER dCode FOR Code.   

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ImportED574 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ImportED574 ", "Update УИС-ЦОС:" + STRING(mUISCOS) ).
      &ENDIF

      /* Удаление  классификаторов УИС-ЦОС и УИС-ЦОСED50x */
      IF mUISCOS =  "YES" THEN DO:
         FOR EACH dCode WHERE
            dCode.class =  "УИС-ЦОС"
            EXCLUSIVE-LOCK :
            DELETE dCode .
         END. 
      FOR EACH dCode WHERE
            dCode.class =  "УИС-ЦОСED50x"
         EXCLUSIVE-LOCK :
         DELETE dCode .
      END. 
      END. 

      ASSIGN
         hExchSrv      = ObjectValueHandle("ExchCOSEsExchangeInf")
         vServicesCode = hExchSrv:buffer-field("ServicesCode"):buffer-value  
         vBegDate      = hExchSrv:buffer-field("BeginDate"):buffer-value 
         vEndDate      = hExchSrv:buffer-field("EndDate"):buffer-value       
         vESSize       = hExchSrv:buffer-field("ESSize"):buffer-value        
         vARMNo        = hExchSrv:buffer-field("ARMNo"):buffer-value         
      NO-ERROR.

            
      ASSIGN 
         hExchSrv        = ObjectValueHandle("ExchRKCStm")
         vName           = hExchSrv:buffer-field("Name"):buffer-value
         vIsFreeServices = hExchSrv:buffer-field("IsFreeServices"):buffer-value  
         vUIS            = hExchSrv:buffer-field("UIS"):buffer-value
         vSWIFTBIC       = hExchSrv:buffer-field("SWIFTBIC"):buffer-value
         vBIC            = hExchSrv:buffer-field("BIC"):buffer-value
         hExchSrv:buffer-field("SWIFTBIC"):buffer-value = ""
         mUISCOS        = "NO"                                        
         vIsSuspensedExchange  = hExchSrv:buffer-field("IsSuspensedExchange"):buffer-value
         vIsNotDailyDispatchES = hExchSrv:buffer-field("IsNotDailyDispatchES"):buffer-value                                      
      NO-ERROR.
      
      /*Обработка классификатора УИС-ЦОС*/

      FIND FIRST bCode WHERE
                 bcode.class =  'УИС-ЦОС'  
             AND bCode.code  =  vUIS 
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE bCode THEN
      DO:
         ASSIGN 
            bCode.name     = TRIM(vName)
            bCode.val            = vSWIFTBIC
            bCode.description[1] = vBIC
            bCode.description[2] = vIsFreeServices
            bCode.misc[1] = STRING(vIsSuspensedExchange)
            bCode.misc[2] = STRING(vIsNotDailyDispatchES)
         NO-ERROR.
      END.
      IF NOT AVAILABLE bCode THEN
      DO:
         CREATE bCode .
         ASSIGN
            bCode.class    = 'УИС-ЦОС'
            bCode.parent   = 'УИС-ЦОС'
            bCode.code           = vUIS
            bCode.name     = TRIM(vName)
            bCode.val            = vSWIFTBIC
            bCode.description[1] = vBIC
            bCode.description[2] = vIsFreeServices
            bCode.misc[1] = STRING(vIsSuspensedExchange)
            bCode.misc[2] = STRING(vIsNotDailyDispatchES)
         NO-ERROR.
      END.                                 
      ASSIGN
         mSWIFTList = ""
         mSWIFTMess = ""
      vFlagSet = YES.
   END. /* ED574 */
   
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).
   
   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED574    аналог ESIDInitialSTMT ESIDConfirmSTMT                      */
/* Метод IMPORT на классe ImportED574                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESExchangeInfo:
   DEFINE INPUT PARAMETER iFormat      AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE hStm              AS handle              NO-UNDO.
   DEFINE VARIABLE hExchSrv          AS handle              NO-UNDO.
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE mSETClass         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mOpKind           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vUIS              AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vBegDate          AS DATE                NO-UNDO.
   DEFINE VARIABLE vEndDate          AS DATE                NO-UNDO.   
   DEFINE VARIABLE vSWIFTBIC         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vServicesCode     AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vESSize           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vARMNo            AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vIsFreeServices   AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vName             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE v__ID             AS INT64               NO-UNDO.
   DEFINE VARIABLE v__UP             AS INT64               NO-UNDO.      
   
   DEFINE BUFFER bCode FOR code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ImportED574 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                      " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hExchSrv      = ObjectValueHandle("ExchCOSEsExchangeInf")
         vServicesCode = hExchSrv:buffer-field("ServicesCode"):buffer-value  
         vBegDate      = hExchSrv:buffer-field("BeginDate"):buffer-value 
         vEndDate      = hExchSrv:buffer-field("EndDate"):buffer-value       
         vESSize       = hExchSrv:buffer-field("ESSize"):buffer-value        
         vARMNo        = hExchSrv:buffer-field("ARMNo"):buffer-value         
      NO-ERROR.


      ASSIGN 
         hExchSrv        = ObjectValueHandle("ExchRKCStm")
         vName           = hExchSrv:buffer-field("Name"):buffer-value
         vIsFreeServices = hExchSrv:buffer-field("IsFreeServices"):buffer-value  
         vUIS            = hExchSrv:buffer-field("UIS"):buffer-value
         vSWIFTBIC       = hExchSrv:buffer-field("SWIFTBIC"):buffer-value
      NO-ERROR.
        
      /* Обработка классификатора УИС-ЦОСED50x */
      vUIS = vUIS + "-" + TRIM(vServicesCode).
      FIND FIRST bCode WHERE
                 bcode.class =  'УИС-ЦОСED50x'  
             AND bCode.code  =  vUIS 
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         
      IF AVAILABLE bCode THEN
      DO:
         ASSIGN 
            bcode.code           = vUIS                           /* "УИС"        */
            bcode.name           = vServicesCode                  /*  "Service"   */
            bcode.description[1] = STRING(vBegDate,"99/99/9999")  /*  "OpenDate"  */
            bcode.description[2] = STRING(vEndDate,"99/99/9999")  /*  "CloseDate" */
            bcode.val            = vESSize                        /*  "MaxSize"   */
            bcode.misc[1]        = vARMNo                         /*  "ARMNo"     */
            bcode.misc[2]        = mSWIFTList                     /*  "SwiftList" */
         NO-ERROR.
      END.
      ELSE DO:
         CREATE bCode.
         ASSIGN
            bCode.class          = 'УИС-ЦОСED50x'
            bCode.parent         = 'УИС-ЦОСED50x'
            bcode.code           = vUIS                           
            bcode.name           = vServicesCode                  
            bcode.description[1] = STRING(vBegDate,"99/99/9999")  
            bcode.description[2] = STRING(vEndDate,"99/99/9999")  
            bcode.val            = vESSize                        
            bcode.misc[1]        = vARMNo                         
            bcode.misc[2]        = mSWIFTList                     
         NO-ERROR.
      END.
      ASSIGN
         mSWIFTList = ""
         mSWIFTMess = ""
         vFlagSet   = YES.
   END. /* ED574 */
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).
   
   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт SwiftTypeList                                                       */
/* Метод IMPORT на классe ImportED574                                         */
/*----------------------------------------------------------------------------*/
PROCEDURE COSSenderInfo:
   DEFINE INPUT PARAMETER iFormat    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN                                
         hExch      = hExch:DEFAULT-BUFFER-HANDLE
         mSWIFTList = IF {assigned mSWIFTList} 
                          THEN mSWIFTList + ";" + REPLACE(TRIM(hExch::UIS) + 
                               "(" + TRIM(hExch::SWIFTBIC) + "):","~n","") + mSWIFTMess
                          ELSE                    REPLACE(TRIM(hExch::UIS) + 
                               "(" + TRIM(hExch::SWIFTBIC) + "):","~n","") + mSWIFTMess
      NO-ERROR.
      ASSIGN
         mSWIFTMess = ""
         vFlagSet = YES
      NO-ERROR.
   END. 
   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт SwiftTypeList                                                       */
/* Метод IMPORT на классe ImportED574                                         */
/*----------------------------------------------------------------------------*/
PROCEDURE SwiftTypeList:
   DEFINE INPUT PARAMETER iFormat    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN                                
         hExch      = hExch:DEFAULT-BUFFER-HANDLE.
         mSWIFTMess = IF {assigned mSWIFTMess} 
                      THEN  mSWIFTMess + "," + hExch::SWIFTTypeNo
                      ELSE  hExch::SWIFTTypeNo  
      NO-ERROR.
      vFlagSet = YES.
   END. 
   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED508                                                               */
/* Метод IMPORT на классe XML-ED508                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport508:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch      AS handle   NO-UNDO.

   DEFINE VARIABLE hMain             AS handle              NO-UNDO.
   DEFINE VARIABLE hSrv              AS handle              NO-UNDO.  
   DEFINE VARIABLE vInitClassRef     AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE vPacketID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vRef              AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mSETClass         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mOpKind           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vParentID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vEDDate           AS DATE                NO-UNDO.
   DEFINE VARIABLE vhSrv             AS HANDLE              NO-UNDO.
   DEFINE VARIABLE vState508         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vPPacketID        AS INT64               NO-UNDO.
   DEFINE VARIABLE vEDNo             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE v__ID             AS INT64               NO-UNDO.
   DEFINE VARIABLE v__UP             AS INT64               NO-UNDO.    
   
   DEFINE BUFFER bCode FOR Code.

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport508 ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport508 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
                            
      RUN PacketInclude    (INPUT  vPacketID,
                           INPUT  vParentID,
                           INPUT  {&STATE-FIN}).
                                  
      ASSIGN 
         hSrv           = ObjectValueHandle("ExchRKCSrv")
         vEDDate        = DATE(hSrv:buffer-field("RecvDate"):buffer-value)
         vInitClassRef  = bcode.misc[{&RKC-REPLY}]
         vRef           = ReferenceFormatValue(vInitClassRef,
                                   hSrv:buffer-field("RecvREF"):buffer-value)                 
      .                 
      
/*---------------------------------------- Сквитовка с запросом ED503 --*/            
  
      FOR FIRST Reference WHERE
          Reference.op-date    =  vEDDate
      AND Reference.Class-Code =  vInitClassRef
      AND Reference.RefValue   =  vRef
          NO-LOCK
         ,
      FIRST Packet WHERE
         Packet.PacketID      =  Reference.PacketID
         NO-LOCK,
      EACH Seance WHERE
         Seance.DIRECT =  {&DIR-EXPORT}
      AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK  :
   
   ASSIGN
            vPPacketID       = Packet.PacketID
         NO-ERROR.
         RUN PacketCreateLink  (INPUT  vPPacketID,
                             INPUT  "Packet",
                             INPUT  STRING(vPacketID),
                             INPUT  ENTRY(2,bCode.Description[1])).
                             
         RUN PacketCreateLink  (INPUT  vParentID,
                             INPUT  "Packet",
                             INPUT  STRING(vPPacketID),
                             INPUT  "ED508").                             

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport508 ", "3 vState508:"  + GetNullStr({&STATE-FIN}) +
                                         " vPPacketID:" + GetNullInt(vPPacketID)).
         &ENDIF
      
         RUN PacketSetState (vPPacketID,{&STATE-FIN}).
                          
      END.               
                                                              
      RUN PacketRKCTextSave (vPacketID).
                   
      vFlagSet = YES.
              
   END. /* ED508 */   
   
/*-------------------------------------------- Удаление текущего экземпляра --*/      
   /*ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID EQ v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).*/
   

   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED508    аналог ESIDInitialSTMT ESIDConfirmSTMT                      */
/* Метод IMPORT на классe XML-SWIFTDocCtrlInfo                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ImpCtrlInfo:
   DEFINE INPUT PARAMETER iFormat      AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hIns         AS handle   NO-UNDO.

   DEFINE VARIABLE hExch               AS handle               NO-UNDO.
   DEFINE VARIABLE hSrv                AS handle               NO-UNDO.  
   DEFINE VARIABLE hMain               AS handle               NO-UNDO.         
   DEFINE VARIABLE vFlagSet            AS LOGICAL INITIAL ?    NO-UNDO.
   DEFINE VARIABLE mSETClass           AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE mOpKind             AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCtrlCode508        AS CHARACTER            NO-UNDO. 
   DEFINE VARIABLE vCtrlCode           AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vNote508            AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vErrorList          AS CHARACTER            NO-UNDO.   
   DEFINE VARIABLE vTermSessNum        AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vPacketID           AS INT64                NO-UNDO.
   DEFINE VARIABLE vParentID           AS INT64                NO-UNDO.   
   DEFINE VARIABLE vInitialID          AS INT64                NO-UNDO.   
   DEFINE VARIABLE vInitClassRef       AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vRef                AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vEDDate             AS DATE                 NO-UNDO.   
   DEFINE VARIABLE vResult             AS CHARACTER            NO-UNDO.     
 
   DEFINE BUFFER Packet       FOR Packet.  
   DEFINE BUFFER bPacket      FOR Packet.
   DEFINE BUFFER xPacket      FOR Packet.   
   DEFINE BUFFER bCode        FOR Code.
   DEFINE BUFFER Reference    FOR  Reference.   
   DEFINE BUFFER bReference   FOR  Reference. 
   DEFINE BUFFER Seance       FOR  Seance. 
   DEFINE BUFFER PackObject   FOR  PackObject.          
   

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ImpErrList ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

   IF NOT EXCH-MSGBuff (INPUT  iFormat,
                        BUFFER bCode) THEN
      UNDO MAIN, RETRY MAIN.                   
                                                                       
      RUN ESIDGetMain       (INPUT-OUTPUT hIns,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hIns,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).                                                                                        
                                                                                                                                        
      ASSIGN   
         hSrv              = ObjectValueHandle("ExchRKCSrv")      
         vEDDate           = DATE(hSrv:buffer-field("RecvDate"):buffer-value)
         vInitClassRef     = bcode.misc[4]
         vRef              = ReferenceFormatValue(vInitClassRef,
                                   hSrv:buffer-field("RecvREF"):buffer-value)            
         hExch             = ObjectValueHandle("ExchRKCDCI")
         vCtrlCode508      = hExch:buffer-field("CtrlCode"):buffer-value
         vNote508          = hExch:buffer-field("Note508"):buffer-value
         vErrorList        = hExch:buffer-field("ErrorList"):buffer-value           
         vTermSessNum      = STRING(hExch:buffer-field("TerminalSessionNum"):buffer-value,"9999999999")
         vResult           = "TerminalSessionNum = " + vTermSessNum + "~n" +
                             "CtrlCode = " + vCtrlCode508 + "~n" +
                             "Annotation = " + vNote508 + "~n" +
                             "ErrorList = " + vErrorList + "~n"                                             
      NO-ERROR.      

      RUN PacketTextSave(vPacketID, vResult).                                                                               

      FOR FIRST Reference WHERE
          Reference.op-date    =  vEDDate
      AND Reference.Class-Code =  vInitClassRef
      AND Reference.RefValue   =  vRef
          NO-LOCK
         ,
      FIRST Packet WHERE
         Packet.PacketID      =  Reference.PacketID
         NO-LOCK,
      EACH Seance WHERE
         Seance.DIRECT =  {&DIR-EXPORT}
      AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK  :             
         ASSIGN
            vInitialID       = Packet.PacketID
         NO-ERROR.                            
                          
      END.
              
      FIND FIRST bPacket WHERE bPacket.PacketID =  vInitialID NO-LOCK NO-ERROR.

      IF AVAIL bPacket THEN 
      DO:
         RUN PacketSetState (vInitialID,{&STATE-FIN}).         
         FOR EACH xPacket WHERE                /*поиск исходного документа*/
            xPacket.ParentID = bPacket.PacketID 
            NO-LOCK,
               EACH bReference 
                  WHERE bReference.PacketID =  xPacket.PacketID
                  AND bReference.class-code =  "RSessionNum"
         NO-LOCK:                                        
            IF bReference.RefValue =  vTermSessNum THEN
            DO:  
               FOR FIRST PackObject WHERE
                      PackObject.PacketID  =  xPacket.PacketID
                  AND PackObject.File-Name =  "op-entry"
                      NO-LOCK,
                 FIRST op WHERE
                       op.op =  INT64(ENTRY(1,PackObject.Surrogate))
                       EXCLUSIVE-LOCK,
                 FIRST op-entry OF op
               EXCLUSIVE-LOCK:                
                  RUN PacketCreateLink  (INPUT  STRING(vPacketID),
                             INPUT  "op-entry",
                             INPUT  PackObject.Surrogate,
                             INPUT  ENTRY(1,bCode.Description[1])).                                
                  IF {assigned vCtrlCode508} OR
                     {assigned vNote508} OR
                     {assigned vErrorList} THEN
                  DO:
                     IF mStatusERR508 <> "" THEN                                              
                        {opent-st.i &status=mStatusERR508}  /* Изменение статусов док-та */                                                                                                                                                                 
                  END.
                  ELSE
                  DO:
                     IF mStatus508 <> "" THEN                            
                        {opent-st.i &status=mStatus508}  /* Изменение статусов док-та */                                                              
                  END.
               END.                     
            END.
         END.
      END.
      
      RUN PacketSetState (vPacketID,{&STATE-FIN}).
            
      ASSIGN
         hExch:buffer-field("ErrorList"):buffer-value = ""  
         vFlagSet = YES
      NO-ERROR.
        
   END. /* ED508 */

   {doreturn.i vFlagSet}
   
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Импорт ED508    аналог ESIDInitialSTMT ESIDConfirmSTMT                      */
/* Метод IMPORT на классe XML-SwErrCodeList                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ImpErrList:
   DEFINE INPUT PARAMETER iFormat      AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hIns         AS handle   NO-UNDO.

   DEFINE VARIABLE hExch             AS handle              NO-UNDO.
   DEFINE VARIABLE hSrv               AS handle               NO-UNDO.   
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE mSETClass         AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mOpKind           AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vErrCode          AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vErrorList          AS CHARACTER           NO-UNDO.             

   mOpKind      = GetBaseOpKind().
   mSETClass    = GetXAttrValue("op-kind",mOpKind,"SETClass").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ImpErrList ", "0 mOpKind:"  + GetNullStr(mOpKind) +
                                         " mSETClass:" + GetNullStr(mSETClass)).
      &ENDIF
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
            
      ASSIGN   
         hSrv        = ObjectValueHandle("ExchRKCDCI") 
         hExch       = hIns:DEFAULT-BUFFER-HANDLE         
         vErrCode    = hExch:buffer-field("ErrorCode"):buffer-value
         vErrorList  = hSrv:buffer-field("ErrorList"):buffer-value                                      
      NO-ERROR.    
                                  
      IF {assigned vErrorList} THEN
         hSrv:buffer-field("ErrorList"):buffer-value = vErrorList + "," + vErrCode .
      ELSE
         hSrv:buffer-field("ErrorList"):buffer-value = vErrCode .
         
      vFlagSet = YES.        
   END. /* ED508 */
   
   {doreturn.i vFlagSet}
   
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Импорт ED510                                                               */
/* Метод IMPORT на классe XML-ED510                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport510:
   DEFINE INPUT PARAMETER iFormat    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iHExch     AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vHMain            AS handle              NO-UNDO.
   DEFINE VARIABLE vPacketID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vParentID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vHSrv             AS HANDLE              NO-UNDO.  
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   
   DEFINE BUFFER bCode FOR Code.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT iHExch,
                             OUTPUT       vHMain).

      RUN ESIDPacketCreate  (INPUT  iHExch,
                             INPUT  vHMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport510 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
                            
      RUN PacketInclude    (INPUT vPacketID,
                           INPUT  vParentID,
                           INPUT  {&STATE-FIN}).

      /*
           - chr(1);   - chr(2);   - chr(3)
         Текст пакета переформатируется из исходного xml к виду (без переносов строки):
         ServicesCode ReceiverSWIFTBIC ServicesDate ServicesQuantity ServicesRate Sum 
         ...
         ServicesCode ReceiverSWIFTBIC ServicesDate ServicesQuantity ServicesRate Sum 
          
         SenfREF(EDNo) SendDate(EDDate) SendID(EDAuthor) RecvID(EDReciver) 
         RegisterNo TotalServicesQuantity TotalSum
      */

      ASSIGN 
         vHSrv     = ObjectValueHandle("ExchRKCSrv510")
         mImpED510 = mImpED510 + CHR(3) +
             STRING(vHSrv:buffer-field("SendREF"):buffer-value)               + CHR(1) + 
             STRING(vHSrv:buffer-field("SendDate"):buffer-value)              + CHR(1) + 
             STRING(vHSrv:buffer-field("SendID"):buffer-value)                + CHR(1) + 
             STRING(vHSrv:buffer-field("RecvID"):buffer-value)                + CHR(1) + 
             STRING(vHSrv:buffer-field("RegisterNo"):buffer-value)            + CHR(1) + 
             STRING(vHSrv:buffer-field("TotalServicesQuantity"):buffer-value) + CHR(1) + 
             STRING(vHSrv:buffer-field("TotalSum"):buffer-value)
      NO-ERROR.

      /*Если нужно будет сохранить пакет в изначальном xml-виде, то нужно вместо
        PacketTextSave запустить:
        RUN PacketRKCTextSave (vPacketID).
      */

      RUN PacketTextSave (vPacketID, mImpED510).

      ASSIGN
         vFlagSet  = YES
         mImpED510 = ""   /*очищаем, т.к. переменная глобальная*/
      NO-ERROR.
              
   END. /* ED510 */   
   
   {doreturn.i vFlagSet}

END PROCEDURE.


/*----------------------------------------------------------------------------*/
/* Импорт ED511                                                               */
/* Метод IMPORT на классe XML-ED511                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport511:
   DEFINE INPUT PARAMETER iFormat    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iHExch     AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vHMain            AS handle              NO-UNDO.
   DEFINE VARIABLE vPacketID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vParentID         AS INT64               NO-UNDO.
   DEFINE VARIABLE vHSrv             AS HANDLE              NO-UNDO.  
   DEFINE VARIABLE vFlagSet          AS LOGICAL INITIAL ?   NO-UNDO.
   DEFINE VARIABLE vStr              AS CHAR                NO-UNDO.  
   
   DEFINE BUFFER bCode FOR Code.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT iHExch,
                             OUTPUT       vHMain).

      RUN ESIDPacketCreate  (INPUT  iHExch,
                             INPUT  vHMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport511 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
                            
      RUN PacketInclude    (INPUT vPacketID,
                           INPUT  vParentID,
                           INPUT  {&STATE-FIN}).
      ASSIGN 
         vHSrv = ObjectValueHandle("ExchRKCSrv511")
         vStr  = 
             STRING(vHSrv:buffer-field("SendREF"):buffer-value)       + CHR(1) + 
             STRING(vHSrv:buffer-field("SendDate"):buffer-value)      + CHR(1) + 
             STRING(vHSrv:buffer-field("SendID"):buffer-value)        + CHR(1) + 
             STRING(vHSrv:buffer-field("ServBeginDate"):buffer-value) + CHR(1) + 
             STRING(vHSrv:buffer-field("ServEndDate"):buffer-value)   + CHR(1) + 
             STRING(vHSrv:buffer-field("BIC"):buffer-value)           + CHR(1) + 
             STRING(vHSrv:buffer-field("INN"):buffer-value)           + CHR(1) + 
             STRING(vHSrv:buffer-field("KPP"):buffer-value)           + CHR(1) + 
             STRING(vHSrv:buffer-field("PayeePersAcc"):buffer-value)  + CHR(1) + 
             STRING(vHSrv:buffer-field("ChargedSum"):buffer-value)    + CHR(1) + 
             STRING(vHSrv:buffer-field("PrepaymentSum"):buffer-value) + CHR(1) + 
             STRING(vHSrv:buffer-field("OverpaySum"):buffer-value)    + CHR(1) + 
             STRING(vHSrv:buffer-field("Sum"):buffer-value)           + CHR(1) + 
             STRING(vHSrv:buffer-field("ExtraChargeSum"):buffer-value)
      NO-ERROR.

      /*Если нужно будет сохранить пакет в изначальном xml-виде, то нужно вместо
        PacketTextSave запустить:
        RUN PacketRKCTextSave (vPacketID).
      */

      RUN PacketTextSave (vPacketID, vStr).

      ASSIGN
         vFlagSet  = YES
      NO-ERROR.
              
   END. /* ED510 */   
   
   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Импорт ED510                                                               */
/* Метод IMPORT на классe XML-PaySrvCodLst                                    */
/*----------------------------------------------------------------------------*/
PROCEDURE PaySrvCodLst:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iHExch    AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vHSrv              AS HANDLE NO-UNDO.  

   ASSIGN
      vHSrv = ObjectValueHandle("ExchRKCPaySrvCodLst")
      mImpED510 = mImpED510 + 
          STRING(vHSrv:buffer-field("ServicesCode"):buffer-value)     + CHR(1) + 
          STRING(vHSrv:buffer-field("ReceiverSWIFTBIC"):buffer-value) + CHR(1) + 
          STRING(vHSrv:buffer-field("ServicesDate"):buffer-value)     + CHR(1) + 
          STRING(vHSrv:buffer-field("ServicesQuantity"):buffer-value) + CHR(1) + 
          STRING(vHSrv:buffer-field("ServicesRate"):buffer-value)     + CHR(1) + 
          STRING(vHSrv:buffer-field("Sum"):buffer-value)              + CHR(2)
   NO-ERROR.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Импорт ED245                                                               */
/* Метод IMPORT на классe XML-ED245                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDConfirm245:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iWOP      AS handle   NO-UNDO.

   DEF VAR vMain           AS handle            NO-UNDO.
   DEF VAR vSETClass       AS CHAR              NO-UNDO.
   DEF VAR vOpKind         AS CHAR              NO-UNDO.
   DEF VAR vPacketID       AS INT64             NO-UNDO.
   DEF VAR vParentID       AS INT64             NO-UNDO.
   DEF VAR vResult         AS CHAR              NO-UNDO.

   DEF VAR vFlagSet        AS LOGICAL INIT ?    NO-UNDO.
   DEF VAR v__ID           AS INT64             NO-UNDO.
   DEF VAR v__UP           AS INT64             NO-UNDO.   

   DEFINE BUFFER bCode     FOR Code.

   vOpKind      = GetBaseOpKind().
   vSETClass    = GetXAttrValue("op-kind",vOpKind,"SETClass").

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ESIDConfirm245 ", "0 vOpKind:"  + GetNullStr(vOpKind) +
                                      " vSETClass:" + GetNullStr(vSETClass)).
   &ENDIF

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT iFormat, BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm245", "1 iWOP:"   + iWOP:NAME  + 
                                        " iFormat:" + iFormat).
      &ENDIF

      RUN ESIDGetMain(INPUT-OUTPUT iWOP, OUTPUT vMain).

      RUN ESIDPacketCreate  (INPUT  iWOP,
                             INPUT  vMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDConfirm245 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF
                            
      RUN PacketInclude    (INPUT  vPacketID,
                           INPUT  vParentID,
                           INPUT  {&STATE-FIN}).

      RUN PacketRKCTextSave (vPacketID).

      vFlagSet = YES.
   END.
  
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED463                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport463:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS HANDLE   NO-UNDO.


   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.
   DEFINE VAR vReport           AS CHARACTER      NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE BUFFER Reference FOR Reference.   
   DEFINE BUFFER Packet    FOR Packet.   
   DEFINE BUFFER Seance    FOR Seance.   

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).
      RUN PacketRKCTextSave (vPacketID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport463 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

      IF {assigned hExch::RecvRef} THEN DO:
         ASSIGN
            vClassRef   = bCode.misc[4]
            vRecvDate   = hExch:buffer-field("RecvDate"):buffer-value
            vRecvRef    = ReferenceFormatValue(vClassRef,
                                               hExch:buffer-field("RecvRef"):buffer-value) 
         NO-ERROR. {&ON-ERROR}

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport463 2", "vClassRef:"  + string(vClassRef) +
                                            " vRecvRef:" + string(vRecvRef) 
                                             + " " + STRING(vRecvDate)).
         &ENDIF

         /* поиск исходного сообщения */
         vPPacketID = -1.
         FOR FIRST Reference WHERE
                   Reference.op-date    =  vRecvDate
               AND Reference.Class-Code =  vClassRef
               AND Reference.RefValue   =  vRecvRef
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID      =  Reference.PacketID
                   NO-LOCK,
             EACH Seance WHERE
                  Seance.DIRECT =  {&DIR-EXPORT}
              AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK:

            ASSIGN
               vPPacketID       = Packet.PacketID
            NO-ERROR.

            IF Packet.mail-format =  "XML-ED462"THEN DO:

               RUN PacketCreateLink  (INPUT  vPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPPacketID),
                                      INPUT  ENTRY(1,bCode.Description[1])).
               RUN PacketCreateLink  (INPUT  vPPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPacketID),
                                      INPUT  ENTRY(2,bCode.Description[1])).

               RUN PacketSetState (vPPacketID,{&STATE-ERR}).

            END.            

         END.  /* FOR */

         IF vPPacketID =  -1 THEN 
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не найдено исходное сообщение ED462.").
      END.     /* IF */

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).
   
   {doreturn.i vFlagSet}
   
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED463                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport280:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS HANDLE   NO-UNDO.


   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.
   DEFINE VAR vReport           AS CHARACTER      NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.
   DEFINE BUFFER Reference FOR Reference.   
   DEFINE BUFFER Packet    FOR Packet.   
   DEFINE BUFFER Seance    FOR Seance.  
   DEFINE BUFFER bIndocs   FOR indocs. 
   DEFINE BUFFER bPackObject FOR PackObject.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).
      RUN PacketRKCTextSave (vPacketID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport280 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

      IF {assigned hExch::RecvRef} THEN DO:
         ASSIGN
            vClassRef   = bCode.misc[4]
            vRecvDate   = hExch:buffer-field("RecvDate"):buffer-value
            vRecvRef    = ReferenceFormatValue(vClassRef,
                                               hExch:buffer-field("RecvRef"):buffer-value) 
         NO-ERROR. {&ON-ERROR}

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport280 2", "vClassRef1:"  + string(vClassRef) +
                                            " vRecvRef1:" + string(vRecvRef) 
                                             + " " + STRING(vRecvDate)).
         &ENDIF

         /* поиск исходного сообщения */
         vPPacketID = -1.
         FOR FIRST Reference WHERE
                   Reference.op-date    =  vRecvDate
               AND Reference.Class-Code =  vClassRef
               AND Reference.RefValue   =  vRecvRef
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID      =  Reference.PacketID
                   NO-LOCK,
             EACH Seance WHERE
                  Seance.DIRECT =  {&DIR-EXPORT}
              AND Seance.SeanceID =  Packet.SeanceID 
         NO-LOCK:

            ASSIGN
               vPPacketID       = Packet.PacketID
            NO-ERROR.

            IF Packet.mail-format =  "XML-ED462"THEN DO:

               RUN PacketCreateLink  (INPUT  vPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPPacketID),
                                      INPUT  ENTRY(1,bCode.Description[1])).
               RUN PacketCreateLink  (INPUT  vPPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPacketID),
                                      INPUT  ENTRY(2,bCode.Description[1])).

               RUN PacketSetState (vPPacketID,{&STATE-CNF}).
               FIND FIRST bPackObject WHERE
                  bPackObject.PacketID = vPPacketID
                  AND bPackObject.file-name = "indocs" NO-LOCK NO-ERROR.
               IF AVAILABLE(bPackObject) THEN   
               DO:               
                  FIND FIRST bIndocs WHERE 
                     bIndocs.Indoc-id = INT64(bPackObject.Surrogate) 
                     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                  IF AVAILABLE(bIndocs) THEN
                     bIndocs.doc-status = {&STATE-CNF}.
                  ELSE               
                  IF LOCKED bIndocs THEN 
                  DO:
                     RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                        SUBSTITUTE("Документ АС ЭКР ED462 с номером &1 заблокирован.",
                           bPackObject.Surrogate)).
                  END.                  
               END.
               ELSE
                  RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                     SUBSTITUTE("Не найден документ АС ЭКР ED462 связанный с сообщением &1.",
                        STRING(vPPacketID))).  

            END.            

         END.  /* FOR */

         IF vPPacketID =  -1 THEN 
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не найдено исходное сообщение ED462.").
      END.     /* IF */

      vFlagSet = YES.
   END.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).

   {doreturn.i vFlagSet}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Метод IMPORT на классе XML-ED465                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDImport465:
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hExch     AS HANDLE   NO-UNDO.


   DEFINE VAR vFlagSet          AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hMain             AS HANDLE         NO-UNDO.
   DEFINE VAR vHRkcSrv          AS HANDLE         NO-UNDO.   
   DEFINE VAR vPacketID         AS INT64          NO-UNDO.
   DEFINE VAR vParentID         AS INT64          NO-UNDO.
   DEFINE VAR vReport           AS CHARACTER      NO-UNDO.
   DEFINE VAR v__ID             AS INT64          NO-UNDO.
   DEFINE VAR v__UP             AS INT64          NO-UNDO.
   DEFINE VAR vClassRef         AS CHAR           NO-UNDO.
   DEFINE VAR vPPacketID        AS INT64          NO-UNDO.
   DEFINE VAR vRecvDate         AS DATE           NO-UNDO.
   DEFINE VAR vRecvRef          AS CHAR           NO-UNDO.
   DEFINE VAR vStatusStateCode  AS CHAR           NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE BUFFER Reference  FOR Reference.   
   DEFINE BUFFER Packet     FOR Packet.   
   DEFINE BUFFER Seance     FOR Seance.   
   DEFINE BUFFER PackObject FOR PackObject.
   DEFINE BUFFER indocs     FOR indocs.

   RUN ObjectValueInit IN h_exch.
   
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff (INPUT  iFormat,
                           BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      RUN ESIDGetMain       (INPUT-OUTPUT hExch,
                             OUTPUT       hMain).

      RUN ESIDPacketCreate  (INPUT  hExch,
                             INPUT  hMain,
                             BUFFER bCode,
                             OUTPUT vPacketID,
                             OUTPUT vParentID).
      RUN PacketInclude    (INPUT  vPacketID,
                            INPUT  vParentID,
                            INPUT  {&STATE-FIN}).
      RUN PacketRKCTextSave (vPacketID).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDImport465 ", "1 vPacketID:" + GetNullInt(vPacketID)).
      &ENDIF

      vHRkcSrv = ObjectValueHandle("ExchRKCSrv").            
            
      IF {assigned vHRkcSrv::RecvRef} THEN DO:
         ASSIGN
            vClassRef   = bCode.misc[4]
            vRecvDate   = vHRkcSrv:buffer-field("RecvDate"):buffer-value
            vRecvRef    = ReferenceFormatValue(vClassRef,
                                               vHRkcSrv:buffer-field("RecvRef"):buffer-value) 
         NO-ERROR. {&ON-ERROR}

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport465 2", "vClassRef:" + string(vClassRef) +
                                            " vRecvRef:" + string(vRecvRef) 
                                                   + " " + STRING(vRecvDate)).
         &ENDIF

         /* поиск исходного сообщения ED464 */
         vPPacketID = -1.
         FOR FIRST Reference WHERE
                   Reference.op-date    = vRecvDate
               AND Reference.Class-Code = vClassRef
               AND Reference.RefValue   = vRecvRef
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID      = Reference.PacketID
                   NO-LOCK,
             EACH Seance WHERE
                  Seance.DIRECT   = {&DIR-EXPORT}
              AND Seance.SeanceID = Packet.SeanceID 
         NO-LOCK:

            ASSIGN
               vPPacketID = Packet.PacketID
            NO-ERROR.

            FOR FIRST PackObject WHERE 
                      PackObject.PacketID = vPPacketID
                      NO-LOCK,
                FIRST indocs WHERE indocs.indoc-id = INT64(PackObject.Surrogate)
                      EXCLUSIVE-LOCK:
               indocs.doc-status = {&STATE-CNF}.
            END.

            IF Packet.mail-format = "XML-ED464" THEN DO:

               RUN PacketCreateLink  (INPUT  vPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPPacketID),
                                      INPUT  ENTRY(1,bCode.Description[1])).
               RUN PacketCreateLink  (INPUT  vPPacketID,
                                      INPUT  "Packet",
                                      INPUT  STRING(vPacketID),
                                      INPUT  ENTRY(2,bCode.Description[1])).

               RUN PacketSetState (vPPacketID,{&STATE-CNF}).

            END.            

         END.  /* FOR */

         IF vPPacketID =  -1 THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не найдено исходное сообщение ED464.").
         END.

         /* поиск сообщения ED462 */
         ASSIGN
            vRecvDate   = hExch:buffer-field("RecvDate"):buffer-value
            vRecvRef    = ReferenceFormatValue(vClassRef,
                                               hExch:buffer-field("RecvRef"):buffer-value)
            vStatusStateCode = GetCode("ESIDState465",hExch::StatusStateCode)                                    
         NO-ERROR. {&ON-ERROR}

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDImport465 3", "vClassRef:" + string(vClassRef) +
                                            " vRecvRef:" + string(vRecvRef) 
                                                   + " " + STRING(vRecvDate) + 
                                            "vStatusStateCode:" + GetNullStr(vStatusStateCode)).
         &ENDIF

         IF {assigned vStatusStateCode} THEN DO:
            FOR FIRST Reference WHERE
                      Reference.op-date    = vRecvDate
                  AND Reference.Class-Code = vClassRef
                  AND Reference.RefValue   = vRecvRef
                      NO-LOCK,
                FIRST Packet WHERE
                      Packet.PacketID      = Reference.PacketID
                      NO-LOCK,
                 EACH Seance WHERE
                      Seance.DIRECT   = {&DIR-EXPORT}
                  AND Seance.SeanceID = Packet.SeanceID 
                      NO-LOCK,
                FIRST PackObject WHERE 
                      PackObject.PacketID = Packet.PacketId
                      NO-LOCK,
                FIRST indocs WHERE indocs.indoc-id = INT64(PackObject.Surrogate)
                      EXCLUSIVE-LOCK:               
               IF Packet.mail-format = "XML-ED462" THEN DO:
                  ASSIGN
                     vPPacketID        = Packet.PacketID
                     indocs.doc-status = vStatusStateCode
                  NO-ERROR.                           
               END.            
            END.  /* FOR */
         END.
      END.

      vFlagSet = YES.
   END.

   RUN ObjectValueDown IN h_exch.
/*-------------------------------------------- Удаление текущего экземпляра --*/
   ASSIGN
      v__ID = hExch:buffer-field("__ID"):buffer-value
      v__UP = hExch:buffer-field("__UpID"):buffer-value
   NO-ERROR.
   
   IF v__ID =  v__UP THEN v__UP = 0.
   RUN InstanceJunk(hExch, v__UP).

   {doreturn.i vFlagSet}
END PROCEDURE.

/******************************************************************************/
/* $LINTFILE='esid-imp.pro' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq/' */
/* $LINTUSER='vasov' */
/* $LINTDATE='09/08/2017 15:45:20.733+03:00' */
/*prosigni9tJnrgRhCanA1TxsdnlJw*/