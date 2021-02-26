/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: core365p.pro
      Comment: Положение 365-П. Общая функциональность.
   Parameters:
      Created: 01.03.2012 krok
     Modified: 12.09.2013 10:56 KSV     
*/

&IF DEFINED(CORE365P_PRO_) = 0 &THEN

&GLOBAL-DEFINE CORE365P_PRO_ YES

{365p.def}

&SCOPED-DEFINE tt-cust365p-name    tt-cust365p
&SCOPED-DEFINE tt-acct365p-name    tt-acct365p
&SCOPED-DEFINE tt-acctflt365p-name tt-acctflt365p

&SCOPED-DEFINE trans-table-prefix  trans-

{globals.i}
{sh-defs.i}
{tmprecid.def}
{intrface.get blkob}
{intrface.get data}
{intrface.get db2l}
{intrface.get exch}
{intrface.get flt}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}

DEFINE VARIABLE mMFMode365p AS LOGICAL NO-UNDO.

FUNCTION limitStr RETURN CHARACTER (INPUT xs AS CHARACTER,
                                    INPUT l  AS INT64):
    RETURN (IF LENGTH(xs) > l THEN
                (SUBSTRING(xs, 1, l - LENGTH("[...]")) + "[...]")
            ELSE
                xs).
END FUNCTION.

FUNCTION fst RETURN CHARACTER (INPUT xs AS CHARACTER):
    RETURN (IF xs = ? THEN "" ELSE ENTRY(1, xs)).
END FUNCTION.

FUNCTION date2id RETURN CHARACTER (INPUT iDate AS DATE):
    RETURN (IF iDate = ? THEN
                FILL("?", 8)
            ELSE
                (STRING(YEAR(iDate) , "9999") +
                 STRING(MONTH(iDate), "99")   +
                 STRING(DAY(iDate)  , "99"))).
END FUNCTION.

FUNCTION date2str RETURN CHARACTER (INPUT iDate AS DATE):
    RETURN (IF iDate = ? THEN "" ELSE STRING(iDate, {&DATE-FORMAT-365P})).
END FUNCTION.

FUNCTION getBaseTableName RETURN CHARACTER (INPUT xs AS CHARACTER):
    RETURN (IF xs BEGINS "{&TRANS-TABLE-PREFIX}" THEN
                SUBSTRING(xs, LENGTH("{&TRANS-TABLE-PREFIX}") + 1)
            ELSE
                xs).
END FUNCTION.

FUNCTION getTransTableName RETURN CHARACTER (INPUT xs AS CHARACTER):
    RETURN (IF xs BEGINS "{&TRANS-TABLE-PREFIX}" THEN
                xs
            ELSE
                "{&TRANS-TABLE-PREFIX}" + xs).
END FUNCTION.

FUNCTION getCustTableName RETURN CHARACTER (INPUT iCustCat AS CHARACTER):
    CASE iCustCat:
        WHEN "Ч" THEN
            RETURN "person".
        WHEN "Ю" THEN
            RETURN "cust-corp".
        WHEN "Б" THEN
            RETURN "banks".
        WHEN "В" THEN
            RETURN "branch".
        OTHERWISE
            RETURN "".
    END.
END FUNCTION.

FUNCTION getCustStr RETURN CHARACTER (INPUT iCustCat AS CHARACTER,
                                      INPUT iCustId  AS INT64):
    RETURN "(" + GetNullStr(iCustCat) + ", " + STRING(iCustId) + ")".
END FUNCTION.

FUNCTION isPersonINN RETURN LOGICAL (INPUT iCustINN AS CHARACTER,
                                     INPUT iCustCat AS CHARACTER):
   IF iCustINN = {&NO-INN} THEN DO:
      IF {assigned iCustCat} THEN
         RETURN (iCustCat = "Ч").
   END.
   ELSE IF {assigned iCustINN} THEN
      RETURN (LENGTH(iCustINN) = {&INN-LENGTH-PERSON}).
   RETURN ?.
END FUNCTION.

FUNCTION getFilialNum RETURN CHARACTER (INPUT iREGN AS CHARACTER):
    IF NUM-ENTRIES(iREGN, "/") > 1 THEN DO:
        IF LEFT-TRIM(ENTRY(2, iREGN, "/"), "0") = "" THEN
            RETURN "0".
        RETURN LEFT-TRIM(ENTRY(2, iREGN, "/"), "0").
    END.
    RETURN "0".
END FUNCTION.

FUNCTION unsafeSimplifyCustName RETURN CHARACTER (INPUT xs AS CHARACTER):
    DEFINE VARIABLE ys AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i  AS INT64     NO-UNDO.

    IF NOT {assigned xs} THEN
        RETURN "".
    ys = xs.
    DO i = 1 TO LENGTH({&special-cust-name-chars}):
        ys = REPLACE(ys, SUBSTRING({&special-cust-name-chars}, i, 1), "").
    END.
    ys = LC(REPLACE(ys, "0", "о")).
    RETURN ys.
END FUNCTION.

FUNCTION qualifiesAsError RETURN LOGICAL (INPUT iFeature     AS INT64,
                                          INPUT iInfoType    AS CHARACTER,
                                          INPUT iRequestKind AS INT64,
                                          INPUT iRequestType AS INT64):
    IF iFeature = {&FEAT-OK} THEN
        RETURN NO.
    IF (iFeature = {&FEAT-NOT-OPEN} OR
        iFeature = {&FEAT-CLOSED})
       AND
       (iRequestKind = {&REQ-KIND-TICLAC} OR
        iRequestKind = {&REQ-KIND-UNDEFINED})
    THEN
        RETURN NO.
    RETURN YES.
END FUNCTION.

FUNCTION getAcctErrorCode RETURN CHARACTER (INPUT iFeature AS INT64):
    CASE iFeature:
        WHEN {&FEAT-OK} THEN
            RETURN {&ERR-SUCCESS}.
        WHEN {&FEAT-WRONG-CUST} THEN
            RETURN {&ERR-CUST}.
        WHEN {&FEAT-NOT-FOUND} OR
        WHEN {&FEAT-NOT-OPEN}  OR
        WHEN {&FEAT-CLOSED}
        THEN
            RETURN {&ERR-ACCT}.
        OTHERWISE
            RETURN {&ERR-OTHER}.
    END.
END.

FUNCTION getAcctErrorText RETURN CHARACTER (INPUT iAcct    AS CHARACTER,
                                            INPUT iFeature AS INT64,
                                            INPUT iDetails AS CHARACTER):
    CASE iFeature:
        WHEN {&FEAT-OK} THEN
            RETURN "".
        WHEN {&FEAT-NOT-FOUND} THEN
            RETURN "Счет "                                             +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "не найден".
        WHEN {&FEAT-WRONG-CUST} THEN
            RETURN "Счет "                                             +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "не открыт для клиента"                             +
                   (IF {assigned iDetails} THEN
                        (" " + GetNullStr(iDetails))
                    ELSE
                        "").
        WHEN {&FEAT-NOT-OPEN} THEN
            RETURN "Счет "                                             +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "не открыт"                                         +
                   (IF {assigned iDetails} THEN (" на " + GetNullStr(iDetails))
                                           ELSE "").
        WHEN {&FEAT-CLOSED} THEN
            RETURN "Счет "                                             +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "закрыт"                                            +
                   (IF {assigned iDetails} THEN (" на " + GetNullStr(iDetails))
                                           ELSE "").
        WHEN {&FEAT-DUPLICATE} THEN
            RETURN "Счет "                                             +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "указан в запросе более одного раза".
        WHEN {&FEAT-ERROR} THEN
            RETURN "Ошибка проверки счета"                             +
                   (IF {assigned iAcct} THEN (" " + GetNullStr(iAcct))
                                        ELSE "").
        WHEN {&FEAT-UNKNOWN} THEN
            RETURN "Проверка счета "                                   +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "не проводилась".
        WHEN {&FEAT-FALSE-365P} THEN
            RETURN "Тип счёта "                                        +
                   (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                        ELSE "")                       +
                   "не соответствует п.2 статьи 86 НК".
        WHEN {&FEAT-TRANS-365P} THEN
            RETURN  "Счет " + 
                    (IF {assigned iAcct} THEN (GetNullStr(iAcct) + " ")
                                         ELSE "")                         +
                    " не является предметом договора банковского счета. " +
                    "Инкассовое поручение не обработано".                            
        OTHERWISE
            RETURN "Неизвестный результат проверки счета"                                   +
                   (IF {assigned iAcct} THEN (" " + GetNullStr(iAcct))
                                        ELSE "").
    END.
END FUNCTION.

FUNCTION getReplyInfoType RETURN CHARACTER (INPUT iRequestKind AS INT64):
    CASE iRequestKind:
        WHEN {&REQ-KIND-TICLAC} THEN
            RETURN {&INFO-TYPE-TICLAC}.
        WHEN {&REQ-KIND-TICLAS} THEN
            RETURN {&INFO-TYPE-TICLAS}.
        WHEN {&REQ-KIND-TICLAM} THEN
            RETURN {&INFO-TYPE-TICLAM}.
        OTHERWISE
            RETURN {&INFO-TYPE-UNDEFINED}.
    END.
END FUNCTION.

FUNCTION fixTagValue365p RETURN CHARACTER (INPUT iStr AS CHARACTER):

   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DO vI = 1 TO LENGTH({&RESTRICTED-CHARS-365P}):
      iStr = REPLACE(iStr,
                     SUBSTRING({&RESTRICTED-CHARS-365P}, vI, 1),
                     "").
   END.
   RETURN iStr.
END FUNCTION.

{spec365p.pro}

PROCEDURE CheckHandle365p.
    DEFINE INPUT PARAMETER iHandle AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iErrMsg AS CHARACTER NO-UNDO.

    IF NOT VALID-HANDLE(iHandle) THEN
        RETURN ERROR (IF {assigned iErrMsg} THEN iErrMsg ELSE "").
END PROCEDURE.

PROCEDURE GetAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oAttrValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vValidAttr AS LOGICAL NO-UNDO.

    RUN CheckHandle365p(iHObject, "Объект не найден") {&RAISE-ERROR}.
    vValidAttr = VALID-HANDLE(iHObject:BUFFER-FIELD(GetMangledName(iAttrName)))
                 NO-ERROR.
    IF vValidAttr <> YES THEN
        RETURN ERROR "Неизвестный атрибут: " + QUOTER(GetNullStr(iAttrName)).
    oAttrValue = iHObject:BUFFER-FIELD(GetMangledName(iAttrName)):BUFFER-VALUE.
END PROCEDURE.

PROCEDURE GetCharAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oAttrValue AS CHARACTER NO-UNDO.

    RUN GetAttrValue365p(iHObject, iAttrName, OUTPUT oAttrValue) {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE GetIntAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oAttrValue AS INT64     NO-UNDO.

    DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.

    RUN GetAttrValue365p(iHObject, iAttrName, OUTPUT vStr) {&RAISE-ERROR}.
    oAttrValue = INT64(vStr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR "Ошибка преобразования значения атрибута " +
                     QUOTER(GetNullStr(iAttrName))              +
                     " к типу INT64".
END PROCEDURE.

PROCEDURE GetDecAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oAttrValue AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.

    RUN GetAttrValue365p(iHObject, iAttrName, OUTPUT vStr) {&RAISE-ERROR}.
    oAttrValue = DECIMAL(vStr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR "Ошибка преобразования значения атрибута " +
                     QUOTER(GetNullStr(iAttrName))              +
                     " к типу DECIMAL".
END PROCEDURE.

PROCEDURE GetDateAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oAttrValue AS DATE      NO-UNDO.

    DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.

    RUN GetAttrValue365p(iHObject, iAttrName, OUTPUT vStr) {&RAISE-ERROR}.
    oAttrValue = DATE(vStr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR "Ошибка преобразования значения атрибута " +
                     QUOTER(GetNullStr(iAttrName))              +
                     " к типу DATE".
END PROCEDURE.

PROCEDURE DeleteObject365p.
    DEFINE INPUT PARAMETER iHObject AS HANDLE NO-UNDO.

    IF VALID-HANDLE(iHObject) THEN
        DELETE OBJECT iHObject.
END PROCEDURE.

PROCEDURE GetNumRecords365p.
    DEFINE INPUT  PARAMETER iHTable     AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER oNumRecords AS INT64  NO-UNDO.

    DEFINE VARIABLE vHQuery AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(iHTable) THEN DO:
        oNumRecords = ?.
        RETURN.
    END.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(iHTable:DEFAULT-BUFFER-HANDLE).
    vHQuery:QUERY-PREPARE("FOR EACH "                        +
                          iHTable:DEFAULT-BUFFER-HANDLE:NAME +
                          " NO-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        oNumRecords = oNumRecords + 1.
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
END PROCEDURE.

PROCEDURE CreateTTAcct365p.
    DEFINE OUTPUT PARAMETER oHTable AS HANDLE NO-UNDO.

    CREATE TEMP-TABLE oHTable.
    IF VALID-HANDLE(oHTable)                           AND
       oHTable:ADD-LIKE-FIELD("acct", "acct.acct")     AND
       oHTable:ADD-NEW-FIELD("feature", "INT64")       AND
       oHTable:ADD-NEW-INDEX("primary", NO, YES)       AND
       oHTable:ADD-INDEX-FIELD("primary", "acct")      AND
       oHTable:ADD-NEW-INDEX("secondary")              AND
       oHTable:ADD-INDEX-FIELD("secondary", "feature") AND
       oHTable:TEMP-TABLE-PREPARE("{&tt-acct365p-name}")
    THEN
        RETURN.
    RUN DeleteObject365p(oHTable).
    RETURN ERROR "Ошибка создания {&tt-acct365p-name}".
END PROCEDURE.

PROCEDURE CreateTTAcctFlt365p.
    DEFINE OUTPUT PARAMETER oHTable AS HANDLE NO-UNDO.

    RUN _CreateFilterTable IN h_trans (OUTPUT oHTable).
    IF VALID-HANDLE(oHTable)                               AND
       oHTable:ADD-LIKE-FIELD("acct", "acct.acct")         AND
       oHTable:ADD-LIKE-FIELD("currency", "acct.currency") AND
       oHTable:ADD-NEW-INDEX("secondary")                  AND
       oHTable:ADD-INDEX-FIELD("secondary", "acct")        AND
       oHTable:ADD-INDEX-FIELD("secondary", "currency")    AND
       oHTable:TEMP-TABLE-PREPARE("{&tt-acctflt365p-name}")
    THEN
        RETURN.
    RUN DeleteObject365p(oHTable).
    RETURN ERROR "Ошибка создания {&tt-acctflt365p-name}".
END PROCEDURE.

PROCEDURE CreateTTCust365p.
    DEFINE OUTPUT PARAMETER oHTable AS HANDLE NO-UNDO.

    CREATE TEMP-TABLE oHTable.
    IF VALID-HANDLE(oHTable)                               AND
       oHTable:ADD-LIKE-FIELD("cust-cat", "acct.cust-cat") AND
       oHTable:ADD-LIKE-FIELD("cust-id", "acct.cust-id")   AND
       oHTable:ADD-NEW-INDEX("primary", YES, YES)          AND
       oHTable:ADD-INDEX-FIELD("primary", "cust-cat")      AND
       oHTable:ADD-INDEX-FIELD("primary", "cust-id")       AND
       oHTable:TEMP-TABLE-PREPARE("{&tt-cust365p-name}")
    THEN
        RETURN.
    RUN DeleteObject365p(oHTable).
    RETURN ERROR "Ошибка создания {&tt-cust365p-name}".
END PROCEDURE.

PROCEDURE CheckReqServ365p.
    DEFINE INPUT PARAMETER iHReqServ AS HANDLE NO-UNDO.

    RUN CheckHandle365p(iHReqServ,
                        "Не найдена служебная часть запроса")
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE CheckReqInfo365p.
    DEFINE INPUT PARAMETER iHReqInfo AS HANDLE NO-UNDO.

    RUN CheckHandle365p(iHReqInfo,
                        "Не найдена информационная часть запроса")
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE CheckReqAcct365p.
    DEFINE INPUT PARAMETER iHReqAcct AS HANDLE NO-UNDO.

    RUN CheckHandle365p(iHReqAcct,
                        "Не найден блок сведений о счетах")
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE GetRequestServicePart365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER oHReqServ AS HANDLE NO-UNDO.

    DEFINE VARIABLE vServiceClass AS CHARACTER NO-UNDO.

    RUN GetCharAttrValue365p(iHReqInfo,
                             "ExchMain",
                             OUTPUT vServiceClass)
    {&RAISE-ERROR}.
    oHReqServ = ObjectValueHandle(vServiceClass) {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE GetBaseRequestData365p.
    DEFINE INPUT  PARAMETER iHReqInfo    AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oInfoType    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oRequestKind AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oRequestType AS INT64     NO-UNDO.

    DEFINE VARIABLE vHReqServ AS HANDLE NO-UNDO.

    RUN CheckReqInfo365p(iHReqInfo) {&RAISE-ERROR}.
    RUN GetRequestServicePart365p(iHReqInfo, OUTPUT vHReqServ) {&RAISE-ERROR}.
    RUN CheckReqServ365p(vHReqServ) {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(vHReqServ, "ТипИнф", OUTPUT oInfoType) NO-ERROR.
    IF NOT CAN-DO({&INFO-TYPES}, oInfoType) THEN
        RETURN ERROR "Неизвестный тип запрашиваемой информации: " +
                     QUOTER(GetNullStr(oInfoType)).
    CASE oInfoType:
        WHEN {&INFO-TYPE-ZNO} THEN DO:
            RUN GetIntAttrValue365p(iHReqInfo,
                                    "ВидЗапр",
                                    OUTPUT oRequestKind)
            NO-ERROR.
            IF NOT CAN-DO({&REQ-KINDS}, STRING(oRequestKind)) THEN
                RETURN ERROR "Неизвестный вид запроса: " +
                             QUOTER(STRING(oRequestKind)).
            RUN GetIntAttrValue365p(iHReqInfo,
                                    "ТипЗапр",
                                    OUTPUT oRequestType)
            NO-ERROR.
            IF NOT CAN-DO({&REQ-TYPES}, STRING(oRequestType)) THEN
                RETURN ERROR "Неизвестный тип запроса: " +
                             QUOTER(STRING(oRequestType)).
        END.
        WHEN {&INFO-TYPE-RPO} THEN
            ASSIGN
                oRequestKind = {&REQ-KIND-TICLAS}
                oRequestType = {&REQ-TYPE-SELECTED}
            .
        OTHERWISE
            ASSIGN
                oRequestKind = {&REQ-KIND-UNDEFINED}
                oRequestType = {&REQ-TYPE-UNDEFINED}
            .
    END.
END PROCEDURE.

PROCEDURE PreValidateRequest365p.
    DEFINE INPUT  PARAMETER iHReqInfo  AS HANDLE                           NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.
    DEFINE VARIABLE vTMFO        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vMFO         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFilialNum   AS CHARACTER NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    vTMFO = (IF vInfoType = {&INFO-TYPE-PNO} THEN "БИКБПл" ELSE "БИК").
    RUN GetCharAttrValue365p(iHReqInfo, vTMFO, OUTPUT vMFO) NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo, "НомФ", OUTPUT vFilialNum) NO-ERROR.
    RUN ValidateFilial365p(vMFO,
                           vFilialNum,
                           OUTPUT oErrorCode,
                           OUTPUT oErrorText).
    IF oErrorCode <> {&ERR-SUCCESS} THEN
        RETURN.
    RUN ValidateRequestFileExch365p(iHReqInfo,
                                    OUTPUT oErrorCode,
                                    OUTPUT oErrorText).
END PROCEDURE.

PROCEDURE ValidateRequestFileExch365p.
    DEFINE INPUT  PARAMETER iHReqInfo  AS HANDLE                           NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE BUFFER FileExch FOR FileExch.
    DEFINE BUFFER Packet   FOR Packet.

    DEFINE VARIABLE vInfoType    AS   CHARACTER           NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64               NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64               NO-UNDO.
    DEFINE VARIABLE vHReqServ    AS   HANDLE              NO-UNDO.
    DEFINE VARIABLE vFileExchID  LIKE FileExch.FileExchID NO-UNDO.
    DEFINE VARIABLE vFileName    LIKE FileExch.Name       NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    IF vInfoType <> {&INFO-TYPE-PNO} THEN
        RETURN.
    RUN GetRequestServicePart365p(iHReqInfo, OUTPUT vHReqServ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    RUN GetIntAttrValue365p(vHReqServ,
                            "FileExchID",
                            OUTPUT vFileExchID)
    NO-ERROR.
    FIND FIRST FileExch WHERE
        FileExch.FileExchID = vFileExchID
    NO-LOCK NO-ERROR.
    IF NOT (AVAILABLE FileExch AND {assigned FileExch.Name}) THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = (IF AVAILABLE FileExch THEN
                              "В наборе данных запроса не указано имя файла"
                          ELSE
                              "Не найден набор данных запроса")
        .
        RETURN.
    END.
    vFileName = FileExch.Name.
    FOR EACH FileExch WHERE
        FileExch.FileExchID <> vFileExchID AND
        FileExch.Name       =  vFileName
    NO-LOCK,
    FIRST Packet WHERE
        Packet.FileExchID = FileExch.FileExchID
    NO-LOCK,
    FIRST PackObject WHERE 
          PackObject.PacketID  EQ Packet.PacketID AND
          PackObject.file-name EQ "op" 
    NO-LOCK:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = "Повторная загрузка файла " + vFileName + ". "   +
                         "Файл уже загружен " + date2str(Packet.PackDate) +
                         " в " + STRING(Packet.PackTime, "HH:MM:SS").
        .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE ValidateFilial365p.
    DEFINE INPUT  PARAMETER iMFO       AS CHARACTER                        NO-UNDO.
    DEFINE INPUT  PARAMETER iFilialNum AS CHARACTER                        NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE VARIABLE vFilialNum AS CHARACTER NO-UNDO.

    IF iMFO = mMFO THEN DO:
        RUN GetThisFilialNum(OUTPUT vFilialNum).
        IF iFilialNum <> vFilialNum THEN
            oErrorText = "Номер филиала не соответствует " + iFilialNum.
    END.
    ELSE
        oErrorText = iMFO.
    IF {assigned oErrorText} THEN
        oErrorCode = {&ERR-FILIAL}.
END PROCEDURE.

PROCEDURE CheckCustCat365p.
    DEFINE INPUT PARAMETER iCustCat LIKE acct.cust-cat NO-UNDO.

    IF NOT CAN-DO({&SUPPORTED-CUST-CATS}, iCustCat) THEN
        RETURN ERROR "Неподдерживаемая категория клиента: " +
                     QUOTER(GetNullStr(iCustCat)).
END PROCEDURE.

PROCEDURE GetCustINN365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustINN  AS CHARACTER NO-UNDO.

    RUN CheckReqInfo365p(iHReqInfo) {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "ИНННП",
                             OUTPUT oCustINN)
    {&RAISE-ERROR}.
    IF oCustINN = {&NO-INN} THEN
       oCustINN = "".
    ELSE DO:
       IF NOT {assigned oCustINN} THEN
           RETURN ERROR "В запросе не указан ИНН клиента".
       IF LENGTH(oCustINN) <> {&INN-LENGTH-PERSON} AND
          LENGTH(oCustINN) <> {&INN-LENGTH-CORP}   AND
          LENGTH(oCustINN) <> {&INN-LENGTH-FCORP}
       THEN
           RETURN ERROR "Недопустимая длина ИНН клиента: " +
                        STRING(LENGTH(oCustINN)).
    END.
END PROCEDURE.

PROCEDURE GetCustName365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.
    DEFINE VARIABLE vCustINN     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTCustName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vI           AS INT64     NO-UNDO.
    DEFINE VARIABLE vC           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vSkip        AS LOGICAL   NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    RUN GetCustINN365p(iHReqInfo, OUTPUT vCustINN) {&RAISE-ERROR}.
    CASE vInfoType:
        WHEN {&INFO-TYPE-PNO} THEN
            vTCustName = "Плательщ".
        WHEN {&INFO-TYPE-TRB} OR
        WHEN {&INFO-TYPE-TRG}
        THEN
            vTCustName = IF isPersonINN(vCustINN, ?)
                         THEN "ФИОФЛ"
                         ELSE "НаимЮЛ".
        WHEN {&INFO-TYPE-RPO} OR
        WHEN {&INFO-TYPE-ROO} OR
        WHEN {&INFO-TYPE-ZNO}
        THEN
            vTCustName = IF isPersonINN(vCustINN, ?)
                         THEN "ФИОИП"
                         ELSE "НаимНП".
    END.
    RUN GetCharAttrValue365p(iHReqInfo, vTCustName, OUTPUT vCustName) NO-ERROR.
    IF CAN-DO("ФИОФЛ,ФИОИП", vTCustName) THEN
        vCustName = REPLACE(vCustName, ",", " ").
    ASSIGN
        vCustName = TRIM(vCustName)
        oCustName = ""
        vSkip     = NO
    .
    DO vI = 1 TO LENGTH(vCustName):
        vC = SUBSTRING(vCustName, vI, 1).
        IF TRIM(vC) = "" THEN DO:
            IF NOT vSkip THEN
                ASSIGN
                    vSkip     = YES
                    oCustName = oCustName + vC
                .
        END.
        ELSE
            ASSIGN
                vSkip     = NO
                oCustName = oCustName + vC
            .
    END.
    IF NOT {assigned oCustName} THEN
        RETURN ERROR "В запросе не указано наименование клиента с ИНН " +
                     vCustINN.
END PROCEDURE.

PROCEDURE GetCust365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqAcct      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oHCust365p     AS HANDLE  NO-UNDO.

    DEFINE BUFFER banks      FOR banks.
    DEFINE BUFFER cust-corp  FOR cust-corp.
    DEFINE BUFFER cust-ident FOR cust-ident.
    DEFINE BUFFER person     FOR person.

    DEFINE VARIABLE vCustINN      AS CHARACTER             NO-UNDO.
    DEFINE VARIABLE vCustKPP      AS CHARACTER             NO-UNDO.
    DEFINE VARIABLE vCustName     AS CHARACTER             NO-UNDO.
    DEFINE VARIABLE vPersCnt      AS INT64     INITIAL 0   NO-UNDO.
    DEFINE VARIABLE vCorpCnt      AS INT64     INITIAL 0   NO-UNDO.
    DEFINE VARIABLE vBankCnt      AS INT64     INITIAL 0   NO-UNDO.
    DEFINE VARIABLE vCorrectName  AS LOGICAL   INITIAL YES NO-UNDO.
    DEFINE VARIABLE vI            AS INT64                 NO-UNDO.
    DEFINE VARIABLE vL            AS LOGICAL               NO-UNDO.
    DEFINE VARIABLE vS            AS CHARACTER             NO-UNDO.
    DEFINE VARIABLE vINN-KPP      AS LOGICAL   INIT NO     NO-UNDO.
    DEFINE VARIABLE vAllFilials   AS LOGICAL               NO-UNDO.

    RUN CheckReqInfo365p(iHReqInfo) {&RAISE-ERROR}.
    RUN GetCustINN365p(iHReqInfo, OUTPUT vCustINN) {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo, "КППНП", OUTPUT vCustKPP) NO-ERROR.
    RUN GetCustName365p(iHReqInfo, OUTPUT vCustName) {&RAISE-ERROR}.
    RUN CreateTTCust365p(OUTPUT oHCust365p) {&RAISE-ERROR}.
    RUN GetMFMode365p(OUTPUT vAllFilials) NO-ERROR.
    DO vI = 1 TO NUM-ENTRIES(vCustName, " "):
        FOR EACH person WHERE
            person.name-last = ENTRY(vI, vCustName, " ") AND
            person.inn       = vCustINN AND
            CAN-FIND(FIRST cust-role WHERE cust-role.cust-cat   EQ "Ч" 
                                       AND cust-role.cust-id    EQ STRING(person.person-id)
                                       AND (vAllFilials 
                                            OR
                                            (cust-role.file-name  EQ "branch" AND
                                             cust-role.surrogate  EQ shFilial)
                                            )
                                       AND cust-role.Class-Code EQ "ImaginClient"
                      NO-LOCK NO-WAIT)
        NO-LOCK:

            RUN AddCustRecord365p(oHCust365p,
                                  "Ч",
                                  person.person-id,
                                  YES)
            NO-ERROR.
            IF NOT (ERROR-STATUS:ERROR OR {assigned RETURN-VALUE}) THEN
                vPersCnt = vPersCnt + 1.
        END.
    END.
    FOR EACH cust-corp WHERE
        cust-corp.inn = vCustINN AND
        CAN-FIND(FIRST cust-role WHERE cust-role.cust-cat   EQ "Ю"                      
                                   AND cust-role.cust-id    EQ STRING(cust-corp.cust-id)
                                   AND (vAllFilials 
                                        OR
                                        (cust-role.file-name  EQ "branch" AND
                                         cust-role.surrogate  EQ shFilial)
                                        )
                                   AND cust-role.Class-Code EQ "ImaginClient"
                  NO-LOCK NO-WAIT)
    NO-LOCK:
       IF iCheckCorpName = YES THEN DO:
          RUN VerifyCorpName365p(cust-corp.cust-id,
                                 vCustName,
                                 OUTPUT vL)
             NO-ERROR.
          IF vL <> YES THEN DO:
             vCorrectName = NO.
             NEXT.
          END.
       END.
          RUN AddCustRecord365p(oHCust365p,
                                "Ю",
                                cust-corp.cust-id,
                                YES)
             NO-ERROR.
       IF NOT (ERROR-STATUS:ERROR OR {assigned RETURN-VALUE}) THEN
          vCorpCnt = vCorpCnt + 1.
    END.
    FOR EACH cust-ident WHERE
        cust-ident.cust-code-type = "ИНН"    AND
        cust-ident.cust-code      = vCustINN AND
        cust-ident.cust-cat       = "Б"
    NO-LOCK,
    EACH banks WHERE
        banks.bank-id = cust-ident.cust-id AND
        banks.client
    NO-LOCK:
        IF iCheckCorpName = YES AND banks.name <> vCustName THEN DO:
            vCorrectName = NO.
            NEXT.
        END.
        RUN AddCustRecord365p(oHCust365p,
                              "Б",
                              banks.bank-id,
                              YES)
        NO-ERROR.
        IF NOT (ERROR-STATUS:ERROR OR {assigned RETURN-VALUE}) THEN
            vBankCnt = vBankCnt + 1.
    END.
    IF vPersCnt + vCorpCnt + vBankCnt > 1 THEN DO:
        RUN FilterCustByAcct365p(iHReqInfo,
                                 iHReqAcct,
                                 INPUT-OUTPUT oHCust365p)
        NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
            RUN FilterCustByKPP365p(iHReqInfo,
                                    vCustKPP,
                                    INPUT-OUTPUT oHCust365p,
                                    OUTPUT vINN-KPP)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            vS = RETURN-VALUE.
            RUN DeleteObject365p(oHCust365p).
            RETURN ERROR vS.
        END.
    END.
    IF NOT oHCust365p:HAS-RECORDS THEN DO:
        IF vCorpCnt = 0 AND NOT vCorrectName THEN
            RETURN "В запросе указано неверное наименование клиента с ИНН = " +
                   vCustINN + " (" + vCustName + ")".
        RETURN "Не найден клиент с ИНН = " + vCustINN +
               (IF vINN-KPP = YES THEN (" и КПП = " + vCustKPP)
                                       ELSE "") + " " + vCustName.
    END.
    RUN GetNumRecords365p(oHCust365p, OUTPUT vI) NO-ERROR.
    IF vI > 1 THEN DO:
        vS = mMnozhC_365.
        IF CAN-DO(vS, "П") THEN
           vI = vI + 1 - vPersCnt - vCorpCnt.
        ELSE DO:
            IF CAN-DO(vS, "Ч") THEN
                vI = vI + 1 - vPersCnt.
            IF CAN-DO(vS, "Ю") THEN
                vI = vI + 1 - vCorpCnt.
        END.
        IF CAN-DO(vS, "Б") THEN
            vI = vI + 1 - vBankCnt.
    END.
    IF vI > 1 THEN
        RETURN "Найдено более одного клиента с ИНН " + vCustINN +
               (IF vINN-KPP = YES THEN (" и КПП " + vCustKPP)
                                       ELSE "").
END PROCEDURE.

PROCEDURE ExtractOneCust365p.
    DEFINE INPUT  PARAMETER iHCust365p AS   HANDLE        NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustCat   LIKE acct.cust-cat NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustId    LIKE acct.cust-id  NO-UNDO.

    DEFINE VARIABLE vHBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE vHQuery  AS HANDLE NO-UNDO.

    RUN CheckHandle365p(iHCust365p,
                        "Не найдена таблица {&tt-cust365p-name}")
    {&RAISE-ERROR}.
    IF NOT iHCust365p:HAS-RECORDS THEN
        RETURN ERROR "Не найдено ни одной записи клиента".
    vHBuffer = iHCust365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
    vHQuery:QUERY-OPEN().
    vHQuery:GET-FIRST().
    RUN GetCharAttrValue365p(vHBuffer, "cust-cat", OUTPUT oCustCat) NO-ERROR.
    RUN GetIntAttrValue365p(vHBuffer, "cust-id", OUTPUT oCustId) NO-ERROR.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    IF NOT {assigned oCustCat} THEN
        RETURN ERROR "Ошибка поиска записи клиента".
END PROCEDURE.

PROCEDURE FindCustRecord365p.
    DEFINE INPUT  PARAMETER iHCust365p       AS   HANDLE        NO-UNDO.
    DEFINE INPUT  PARAMETER iCustCat         LIKE acct.cust-cat NO-UNDO.
    DEFINE INPUT  PARAMETER iCustId          LIKE acct.cust-id  NO-UNDO.
    DEFINE OUTPUT PARAMETER oHCustBuffer365p AS   HANDLE        NO-UNDO.

    DEFINE VARIABLE vHQuery AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(iHCust365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-cust365p-name}".
    RUN CheckCustCat365p(iCustCat) {&RAISE-ERROR}.
    oHCustBuffer365p = iHCust365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(oHCustBuffer365p).
    vHQuery:QUERY-PREPARE("FOR EACH " + oHCustBuffer365p:NAME     +
                          " WHERE "                               +
                          oHCustBuffer365p:NAME + ".cust-cat = '" +
                          iCustCat                                +
                          "' AND "                                +
                          oHCustBuffer365p:NAME + ".cust-id = "   +
                          STRING(iCustId)                         +
                          " NO-LOCK").
    vHQuery:QUERY-OPEN().
    vHQuery:GET-FIRST().
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    IF NOT oHCustBuffer365p:AVAILABLE THEN
        oHCustBuffer365p = ?.
END PROCEDURE.

PROCEDURE AddCustRecord365p.
    DEFINE INPUT PARAMETER iHCust365p AS   HANDLE        NO-UNDO.
    DEFINE INPUT PARAMETER iCustCat   LIKE acct.cust-cat NO-UNDO.
    DEFINE INPUT PARAMETER iCustId    LIKE acct.cust-id  NO-UNDO.
    DEFINE INPUT PARAMETER iSilent    AS   LOGICAL       NO-UNDO.

    DEFINE VARIABLE vHBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE vHQuery  AS HANDLE NO-UNDO.

    RUN FindCustRecord365p(iHCust365p,
                           iCustCat,
                           iCustId,
                           OUTPUT vHBuffer)
    {&RAISE-ERROR}.
    IF VALID-HANDLE(vHBuffer) THEN DO:
        IF iSilent THEN
            RETURN.
        RETURN "Запись клиента уже существует: " +
               getCustStr(iCustCat, iCustId).
    END.
    vHBuffer = iHCust365p:DEFAULT-BUFFER-HANDLE.
    IF NOT (VALID-HANDLE(vHBuffer) AND vHBuffer:BUFFER-CREATE()) THEN
        RETURN ERROR "Ошибка создания записи клиента " +
                     getCustStr(iCustCat, iCustId).
    ASSIGN
        vHBuffer:BUFFER-FIELD("cust-cat"):BUFFER-VALUE = iCustCat
        vHBuffer:BUFFER-FIELD("cust-id"):BUFFER-VALUE  = iCustId
    NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND vHBuffer:BUFFER-RELEASE() THEN
        RETURN.
    vHBuffer:BUFFER-DELETE().
    RETURN ERROR "Ошибка сохранения записи клиента " +
                 getCustStr(iCustCat, iCustId).
END PROCEDURE.

PROCEDURE VerifyCorpName365p.
    DEFINE INPUT  PARAMETER iCustId   LIKE cust-corp.cust-id NO-UNDO.
    DEFINE INPUT  PARAMETER iCustName AS   CHARACTER         NO-UNDO.
    DEFINE OUTPUT PARAMETER oSuccess  AS   LOGICAL           NO-UNDO.

    DEFINE BUFFER cust-corp FOR cust-corp.

    DEFINE VARIABLE vNameCorp  LIKE cust-corp.name-corp  NO-UNDO.
    DEFINE VARIABLE vNameShort LIKE cust-corp.name-short NO-UNDO.

    FIND FIRST cust-corp WHERE cust-corp.cust-id = iCustId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cust-corp THEN
        RETURN ERROR "Не найден клиент " + getCustStr("Ю", iCustId).
    ASSIGN
        iCustName  = unsafeSimplifyCustName(iCustName)
        vNameCorp  = unsafeSimplifyCustName(cust-corp.name-corp)
        vNameShort = unsafeSimplifyCustName(cust-corp.name-short)
    .
    oSuccess = INDEX(iCustName, vNameCorp)  +
               INDEX(iCustName, vNameShort) +
               INDEX(vNameCorp, iCustName)  +
               INDEX(vNameShort, iCustName) > 0.
END PROCEDURE.

PROCEDURE FilterCustByAcct365p.
    DEFINE INPUT        PARAMETER iHReqInfo   AS HANDLE NO-UNDO.
    DEFINE INPUT        PARAMETER iHReqAcct   AS HANDLE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioHCust365p AS HANDLE NO-UNDO.

    DEFINE VARIABLE vInfoType    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64     NO-UNDO.
    DEFINE VARIABLE vHFCust365p  AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vAcctTags    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vAcct        LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery      AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vI           AS   INT64     NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    IF vInfoType = {&INFO-TYPE-PNO} THEN DO:
        RUN CreateTTCust365p(OUTPUT vHFCust365p) {&RAISE-ERROR}.
        vAcctTags = "НомСчПл,НомВалСч".
        DO vI = 1 TO NUM-ENTRIES(vAcctTags):
            RUN GetCharAttrValue365p(iHReqInfo,
                                     ENTRY(vI, vAcctTags),
                                     OUTPUT vAcct)
            {&RAISE-ERROR}.
            RUN TransferAcctCust365p(vHFCust365p,
                                     ioHCust365p,
                                     vAcct)
            {&RAISE-ERROR}.
        END.
    END.
    IF vRequestType = {&REQ-TYPE-SELECTED} THEN DO:
        RUN CreateTTCust365p(OUTPUT vHFCust365p) {&RAISE-ERROR}.
        RUN CheckReqAcct365p(iHReqAcct) {&RAISE-ERROR}.
        vHBuffer = iHReqAcct:DEFAULT-BUFFER-HANDLE.
        CREATE QUERY vHQuery.
        vHQuery:SET-BUFFERS(vHBuffer).
        vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
        vHQuery:QUERY-OPEN().
        REPEAT:
            vHQuery:GET-NEXT().
            IF vHQuery:QUERY-OFF-END THEN
                LEAVE.
            IF vHBuffer:AVAILABLE THEN DO:
                RUN GetCharAttrValue365p(vHBuffer,
                                         "НомСч",
                                         OUTPUT vAcct)
                NO-ERROR.
                RUN TransferAcctCust365p(vHFCust365p,
                                         ioHCust365p,
                                         vAcct)
                {&RAISE-ERROR}.
            END.
        END.
        vHQuery:QUERY-CLOSE().
        DELETE OBJECT vHQuery.
    END.
    IF VALID-HANDLE(vHFCust365p) THEN DO:
        RUN DeleteObject365p(ioHCust365p).
        ioHCust365p = vHFCust365p.
    END.
END PROCEDURE.

PROCEDURE TransferAcctCust365p.
    DEFINE INPUT PARAMETER iHDstCust365p AS   HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iHSrcCust365p AS   HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iAcct         LIKE acct.acct NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vHCustBuffer365p AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(iHSrcCust365p) THEN
        RETURN ERROR "Не найден источник записей {&tt-cust365p-name}".
    IF NOT VALID-HANDLE(iHDstCust365p) THEN
        RETURN ERROR "Не найден приемник записей {&tt-cust365p-name}".
    IF NOT iHSrcCust365p:HAS-RECORDS THEN
        RETURN.
    RUN FindAcct365p(BUFFER acct, iAcct) NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN.
    RUN FindCustRecord365p(iHSrcCust365p,
                           acct.cust-cat,
                           acct.cust-id,
                           OUTPUT vHCustBuffer365p)
    {&RAISE-ERROR}.
    IF VALID-HANDLE(vHCustBuffer365p) THEN DO:
        RUN AddCustRecord365p(iHDstCust365p,
                              acct.cust-cat,
                              acct.cust-id,
                              YES)
        {&RAISE-ERROR}.
    END.
END PROCEDURE.

PROCEDURE FilterCustByKPP365p.
    DEFINE INPUT        PARAMETER iHReqInfo   AS HANDLE    NO-UNDO.
    DEFINE INPUT        PARAMETER iKPP        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioHCust365p AS HANDLE    NO-UNDO.
    DEFINE OUTPUT       PARAMETER oDidFilter  AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE vInfoType    AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64         NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64         NO-UNDO.
    DEFINE VARIABLE vCustKPP     AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustCat     LIKE acct.cust-cat NO-UNDO.
    DEFINE VARIABLE vCustId      LIKE acct.cust-id  NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS   HANDLE        NO-UNDO.
    DEFINE VARIABLE vHQuery      AS   HANDLE        NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    IF vInfoType = {&INFO-TYPE-ZNO} THEN DO TRANSACTION:
        IF NOT {assigned iKPP} OR
           vRequestType = {&REQ-TYPE-ALL} AND
           mIgnorKPP_365 = "Да"
        THEN
            RETURN.
        vHBuffer = ioHCust365p:DEFAULT-BUFFER-HANDLE.
        CREATE QUERY vHQuery.
        vHQuery:SET-BUFFERS(vHBuffer).
        vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " EXCLUSIVE-LOCK").
        vHQuery:QUERY-OPEN().
        REPEAT:
            vHQuery:GET-NEXT().
            IF vHQuery:QUERY-OFF-END THEN
                LEAVE.
            IF vHBuffer:AVAILABLE THEN DO:
                RUN GetCharAttrValue365p(vHBuffer,
                                         "cust-cat",
                                         OUTPUT vCustCat)
                NO-ERROR.
                RUN GetIntAttrValue365p(vHBuffer,
                                        "cust-id",
                                        OUTPUT vCustId)
                NO-ERROR.
                vCustKPP = ClientXAttrVal(vCustCat, vCustId, "КПП").
                IF NOT CAN-DO(vCustKPP, iKPP) THEN
                    vHBuffer:BUFFER-DELETE().
            END.
        END.
        vHQuery:QUERY-CLOSE().
        DELETE OBJECT vHQuery.
        oDidFilter = YES.
    END.
END PROCEDURE.

PROCEDURE GetMFMode365p.
   DEFINE OUTPUT PARAMETER oAllFilials AS LOGICAL NO-UNDO.

   oAllFilials = mMFMode365p.
END PROCEDURE.

PROCEDURE SetMFMode365p.
   DEFINE INPUT PARAMETER iHReqInfo AS HANDLE NO-UNDO.

   DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
   DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.

   RUN GetBaseRequestData365p(iHReqInfo,
                              OUTPUT vInfoType,
                              OUTPUT vRequestKind,
                              OUTPUT vRequestType)
   {&RAISE-ERROR}.
   mMFMode365p = NOT shMode OR
                 (vInfoType    = {&INFO-TYPE-ZNO} AND
                  vRequestType = {&REQ-TYPE-ALL}  AND
                  mAllFil_365 = "Да" ) .
   &IF DEFINED(ExtBase) &THEN
   RUN SetEnv365p IN h_plbnk(vInfoType, mMFMode365p).
   &ENDIF
END PROCEDURE.

PROCEDURE FindAcct365p.
   DEFINE       PARAMETER BUFFER acct FOR  acct.
   DEFINE INPUT PARAMETER iAcct       LIKE acct.acct NO-UNDO.

   DEFINE VARIABLE vAllFilials AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vClsFilLst  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcct       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI          AS INT64     NO-UNDO.

   RUN GetMFMode365p(OUTPUT vAllFilials) NO-ERROR.
   {find-365.i &acct   = iAcct
               &allfil = vAllFilials}

   IF NOT AVAIL acct THEN
   DO:
       RUN GetCloseFilialsNum(OUTPUT vClsFilLst).
       IF {assigned vClsFilLst } THEN
       loopacct:
       DO vI = 1 TO NUM-ENTRIES(vClsFilLst) :
          vAcct = AddFilToAcct(DelFilFromAcct(iAcct), 
                               ENTRY(vI, vClsFilLst)).
          {find-act.i &acct = vAcct }
          IF AVAILABLE acct 
          THEN LEAVE loopacct.
       END.
   END.
END PROCEDURE.

PROCEDURE FindAcctCurr365p.
   DEFINE       PARAMETER BUFFER acct FOR  acct.
   DEFINE INPUT PARAMETER iAcct       LIKE acct.acct     NO-UNDO.
   DEFINE INPUT PARAMETER iCurrency   LIKE acct.currency NO-UNDO.

   DEFINE VARIABLE vAllFilials AS LOGICAL NO-UNDO.

   RUN GetMFMode365p(OUTPUT vAllFilials) NO-ERROR.
   {find-365.i &acct   = iAcct
               &curr   = iCurrency
               &allfil = vAllFilials}
END PROCEDURE.

PROCEDURE FindAcctOld365p.
   DEFINE PARAMETER BUFFER acct      FOR  acct.
   DEFINE PARAMETER BUFFER oacct     FOR  acct.

   DEFINE           BUFFER cust-role FOR cust-role.

   DEFINE VARIABLE vOldAcct  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOldAcctF AS CHARACTER NO-UNDO.

   IF NOT AVAILABLE acct THEN
   RETURN.

   vOldAcct = GetXAttrValueEx("acct",
                              Surrogate(BUFFER acct:HANDLE),
                              mParSchStN,
                              "").
   IF NOT {assigned vOldAcct } THEN
   RETURN.

   FOR EACH cust-role NO-LOCK
      WHERE cust-role.cust-cat   EQ acct.cust-cat
        AND cust-role.cust-id    EQ STRING(acct.cust-id)
        AND cust-role.file-name  EQ "branch" 
        AND cust-role.Class-Code EQ "ImaginClient" 
        AND cust-role.surrogate  NE acct.filial-id :

      vOldAcctF = AddFilToAcct(vOldAcct, cust-role.surrogate).
      {find-act.i &bact   = oacct
                  &acct   = vOldAcctF
      }
      IF AVAILABLE oacct THEN LEAVE.
   END.

END PROCEDURE.

PROCEDURE GetAcct365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqAcct      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iHCust365p     AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oHAcct365p     AS HANDLE  NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    &IF DEFINED(ExtBase) &THEN
    DEFINE VARIABLE vTmpStr      AS   CHARACTER NO-UNDO.
    &ENDIF
    DEFINE VARIABLE vInfoType    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64     NO-UNDO.
    DEFINE VARIABLE vRecalcCust  AS   LOGICAL   NO-UNDO.
    DEFINE VARIABLE vS           AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vReqDate     AS   DATE      NO-UNDO.
    DEFINE VARIABLE vBegDate     AS   DATE      NO-UNDO.
    DEFINE VARIABLE vEndDate     AS   DATE      NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery      AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vAcct        LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vFeature     AS   INT64     NO-UNDO.
    DEFINE VARIABLE vUserFilter  AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vHAcct       AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vAllFilials  AS   LOGICAL   NO-UNDO.
    DEFINE VARIABLE vFilialLst   AS   CHARACTER NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    RUN GetRequestPeriod365p(iHReqInfo,
                             OUTPUT vReqDate,
                             OUTPUT vBegDate,
                             OUTPUT vEndDate)
    {&RAISE-ERROR}.
    IF vRequestType = {&REQ-TYPE-SELECTED} THEN DO:
        RUN CheckReqAcct365p(iHReqAcct) {&RAISE-ERROR}.
    END.
    RUN CreateTTAcct365p(OUTPUT oHAcct365p) {&RAISE-ERROR}.
    IF vRequestType = {&REQ-TYPE-UNDEFINED} THEN
        RETURN.
    IF NOT VALID-HANDLE(iHCust365p) THEN DO:
        RUN GetCust365p(iHReqInfo,
                        iHReqAcct,
                        iCheckCorpName,
                        OUTPUT iHCust365p)
        NO-ERROR.
        ASSIGN
            vS          = RETURN-VALUE
            vRecalcCust = YES
        .
        IF ERROR-STATUS:ERROR THEN DO:
            RUN DeleteObject365p(oHAcct365p).
            RETURN ERROR vS.
        END.
        IF NOT iHCust365p:HAS-RECORDS THEN DO:
            RUN DeleteObject365p(iHCust365p).
            RETURN vS.
        END.
    END.
    RUN GetMFMode365p(OUTPUT vAllFilials) NO-ERROR.
    IF vRequestType = {&REQ-TYPE-ALL} THEN DO:
        CREATE QUERY vHQuery.
        vHBuffer = iHCust365p:DEFAULT-BUFFER-HANDLE.
        vHQuery:SET-BUFFERS(vHBuffer, BUFFER acct:HANDLE).
        RUN GetCloseFilialsNum(OUTPUT vFilialLst).
        vFilialLst = IF {assigned vFilialLst} 
                     THEN shFilial + "," + vFilialLst
                     ELSE shFilial.
        vS = "FOR EACH " + vHBuffer:NAME                               +
             " NO-LOCK, "                                              +
             "EACH acct WHERE "                                        +
             (IF vAllFilials
              THEN ""
              ELSE ("CAN-DO(" + QUOTER(vFilialLst) + ",acct.filial-id) AND ")) +
             "acct.cust-cat = " + vHBuffer:NAME + ".cust-cat"          +
             " AND "                                                   +
             "acct.cust-id = " + vHBuffer:NAME + ".cust-id"            +
             " NO-LOCK".
        vHQuery:QUERY-PREPARE(vS).
        vHQuery:QUERY-OPEN().
        ACCTQ-ALL:
        REPEAT:
            vHQuery:GET-NEXT().
            IF vHQuery:QUERY-OFF-END THEN
                LEAVE ACCTQ-ALL.
            vAcct = acct.acct.
            RUN ValidateAcct365p(vAcct,
                                 iHCust365p,
                                 vBegDate,
                                 vEndDate,
                                 NO,
                                 OUTPUT vFeature).
            RUN AddAcctRecord365p(oHAcct365p, vAcct, vFeature) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                vS = RETURN-VALUE.
                RUN DeleteObject365p(oHAcct365p).
                IF vRecalcCust THEN
                    RUN DeleteObject365p(iHCust365p).
                RETURN ERROR vS.
            END.
        END.
        vHQuery:QUERY-CLOSE().
        DELETE OBJECT vHQuery.
        vUserFilter = TRNSettingValue("ExchSET-TAX", "AcctFilter", "").
        IF {assigned vUserFilter} THEN DO:
            RUN FilterAcctByUserConfig365p(vUserFilter,
                                           INPUT-OUTPUT oHAcct365p)
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                vS = RETURN-VALUE.
                RUN DeleteObject365p(oHAcct365p).
                IF vRecalcCust THEN
                    RUN DeleteObject365p(iHCust365p).
                RETURN ERROR vS.
            END.
        END.
        &IF DEFINED(ExtBase) &THEN
        {empty ttExtAcct}
        RUN FindAcctEx IN h_plbnk(iHCust365p, "*", OUTPUT TABLE ttExtAcct) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            vS = RETURN-VALUE.
            RUN DeleteObject365p(oHAcct365p).
            IF vRecalcCust THEN
                RUN DeleteObject365p(iHCust365p).
            RETURN ERROR vS.
        END.
        FOR EACH ttExtAcct NO-LOCK:
           vAcct = SUBSTR(ttExtAcct.number, 1, 20).
           RUN ValidateAcct365p(vAcct,
                                iHCust365p,
                                vBegDate,
                                vEndDate,
                                NO,
                                OUTPUT vFeature).
           RUN AddAcctRecord365p(oHAcct365p, vAcct, vFeature) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
               vS = RETURN-VALUE.
               RUN DeleteObject365p(oHAcct365p).
               IF vRecalcCust THEN
                   RUN DeleteObject365p(iHCust365p).
               RETURN ERROR vS.
           END.
        END. /* FOR EACH ttExtAcct NO-LOCK: */
        {empty ttExtAcct}
        &ENDIF
    END.
    ELSE IF vRequestType = {&REQ-TYPE-SELECTED} THEN DO:
        CREATE QUERY vHQuery.
        vHBuffer = iHReqAcct:DEFAULT-BUFFER-HANDLE.
        vHQuery:SET-BUFFERS(vHBuffer).
        vS = "FOR EACH " + vHBuffer:NAME + " NO-LOCK".
        vHQuery:QUERY-PREPARE(vS).
        vHQuery:QUERY-OPEN().
        ACCTQ-SELECTED:
        REPEAT:
            vHQuery:GET-NEXT().
            IF vHQuery:QUERY-OFF-END THEN
                LEAVE ACCTQ-SELECTED.
            RUN GetCharAttrValue365p(vHBuffer, "НомСч", OUTPUT vAcct) NO-ERROR.
            vAcct = DelFilFromAcct(vAcct).
            RUN FindAcctRecord365p(oHAcct365p,
                                   vAcct,
                                   OUTPUT vHAcct)
            {&RAISE-ERROR}.
            IF VALID-HANDLE(vHAcct) THEN
                vFeature = {&FEAT-DUPLICATE}.
            ELSE DO:
                RUN ValidateAcct365p(vAcct,
                                     iHCust365p,
                                     vBegDate,
                                     vEndDate,
                                     NO,
                                     OUTPUT vFeature).
                IF vFeature <> {&FEAT-NOT-FOUND} THEN DO:
                   RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
                   /* {find-act.i &acct = vAcct} */
                   IF NOT AVAILABLE acct 
                   &IF DEFINED(ExtBase) &THEN
                      AND NOT (    RunExtQuery(vBegDate) 
                               AND IsAcctEists(vAcct, OUTPUT vTmpStr))
                   &ENDIF
                   THEN
                      NEXT ACCTQ-SELECTED.
                END.
            END.
            RUN AddAcctRecord365p(oHAcct365p, vAcct, vFeature) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                vS = RETURN-VALUE.
                RUN DeleteObject365p(oHAcct365p).
                IF vRecalcCust THEN
                    RUN DeleteObject365p(iHCust365p).
                RETURN ERROR vS.
            END.
        END.
        RUN FilterAcct365p(oHAcct365p).
    END.
    IF vRecalcCust THEN
        RUN DeleteObject365p(iHCust365p).
END PROCEDURE.

PROCEDURE FilterAcctByUserConfig365p.
    DEFINE INPUT        PARAMETER iUserConfig AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioHAcct365p AS HANDLE    NO-UNDO.

    DEFINE BUFFER acct     FOR acct.
    DEFINE BUFFER tmprecid FOR tmprecid.

    DEFINE VARIABLE vHAcctFlt365p AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vS            AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vHFAcct365p   AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHBuffer      AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery       AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vAcct         LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vFeature      AS   INT64     NO-UNDO.

    IF NOT VALID-HANDLE(ioHAcct365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-cust365p-name}".
    IF NOT {assigned iUserConfig} THEN
        RETURN.
    RUN CheckUserConfig365p(iUserConfig) {&RAISE-ERROR}.
    RUN CreateTTAcctFlt365p(OUTPUT vHAcctFlt365p) {&RAISE-ERROR}.
    RUN FillTTAcctFlt365p(vHAcctFlt365p, ioHAcct365p) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        vS = RETURN-VALUE.
        RUN DeleteObject365p(vHAcctFlt365p).
        RETURN ERROR vS.
    END.
    {empty tmprecid}
    /*
       Добавляем условие PermTable = "*", чтобы подавить ограничение на 
       равенство acct.filial-id коду текущего филиала, возникающее по умолчанию
    */
    RUN SelectFltObject IN h_flt ("acct",
                                  "FilterTable"
                                  + CHR(1) +
                                  "UserConf"
                                  + CHR(1) +
                                  "PermTable",
                                  STRING(vHAcctFlt365p)
                                  + CHR(1) +
                                  iUserConfig
                                  + CHR(1) +
                                  "*",
                                  "").
    RUN DeleteObject365p(vHAcctFlt365p).
    RUN CreateTTAcct365p(OUTPUT vHFAcct365p) {&RAISE-ERROR}.
    vHBuffer = ioHAcct365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
        RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
        RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
        IF AVAILABLE acct
           AND
           NOT CAN-FIND(FIRST tmprecid WHERE tmprecid.id = RECID(acct))
        THEN
            NEXT.
        RUN AddAcctRecord365p(vHFAcct365p, vAcct, vFeature) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            vS = RETURN-VALUE.
            RUN DeleteObject365p(vHFAcct365p).
            RETURN ERROR vS.
        END.
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    {empty tmprecid}
    IF VALID-HANDLE(vHFAcct365p) THEN DO:
        RUN DeleteObject365p(ioHAcct365p).
        ioHAcct365p = vHFAcct365p.
    END.
END PROCEDURE.

PROCEDURE CheckUserConfig365p.
    DEFINE INPUT PARAMETER iUserConfig AS CHARACTER NO-UNDO.

    DEFINE BUFFER user-config FOR user-config.

    DEFINE VARIABLE vUserId   LIKE user-config.user-id   NO-UNDO.
    DEFINE VARIABLE vProcName LIKE user-config.proc-name NO-UNDO.
    DEFINE VARIABLE vDescr    LIKE user-config.descr     NO-UNDO.
    DEFINE VARIABLE vSubCode  LIKE user-config.sub-code  NO-UNDO.

    IF NOT {assigned iUserConfig} THEN
        RETURN ERROR "Не указано наименование пользовательской настройки".
    IF NUM-ENTRIES(iUserConfig) <> 4
       OR
       NUM-ENTRIES(iUserConfig) = 4 AND ENTRY(2, iUserConfig) <> "acct.p"
    THEN
        RETURN ERROR "Неверный формат наименования " +
                     "пользовательской настройки: "  +
                     QUOTER(GetNullStr(iUserConfig)).
    ASSIGN
        vUserId   = ENTRY(1, iUserConfig)
        vProcName = ENTRY(2, iUserConfig)
        vDescr    = ENTRY(3, iUserConfig)
        vSubCode  = ENTRY(4, iUserConfig)
    .
    FIND FIRST user-config WHERE
        user-config.user-id   = vUserId   AND
        user-config.proc-name = vProcName AND
        user-config.descr     = vDescr    AND
        user-config.sub-code  = vSubCode
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE user-config THEN
        RETURN ERROR "Не найдена пользовательская настройка: " +
                     QUOTER(GetNullStr(iUserConfig)).
END PROCEDURE.

PROCEDURE FillTTAcctFlt365p.
    DEFINE INPUT PARAMETER iHAcctFlt365p AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iHAcct365p    AS HANDLE NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vHBuffer  AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery   AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHFBuffer AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vFilterId AS   INT64     NO-UNDO.
    DEFINE VARIABLE vAcct     LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vFeature  AS   INT64     NO-UNDO.

    IF NOT VALID-HANDLE(iHAcctFlt365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-acctflt365p-name}".
    IF NOT VALID-HANDLE(iHAcct365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-acct365p-name}".
    IF NOT iHAcct365p:HAS-RECORDS THEN
        RETURN.
    ASSIGN
        vHBuffer  = iHAcct365p:DEFAULT-BUFFER-HANDLE
        vHFBuffer = iHAcctFlt365p:DEFAULT-BUFFER-HANDLE
        vFilterId = 0
    .
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
        RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
        RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
        IF NOT AVAILABLE acct THEN
            NEXT.
        IF vHFBuffer:BUFFER-CREATE() THEN DO:
            ASSIGN
                vHFBuffer:BUFFER-FIELD("__filterid"):BUFFER-VALUE = vFilterId
                vHFBuffer:BUFFER-FIELD("acct"):BUFFER-VALUE = acct.acct
                vHFBuffer:BUFFER-FIELD("currency"):BUFFER-VALUE = acct.currency
                vFilterId = vFilterId + 1
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                vHFBuffer:BUFFER-DELETE().
                RETURN ERROR "Ошибка заполнения записи {&tt-acctflt365p-name}".
            END.
            IF NOT vHFBuffer:BUFFER-RELEASE() THEN DO:
                vHFBuffer:BUFFER-DELETE().
                RETURN ERROR "Ошибка сохранения записи {&tt-acctflt365p-name}".
            END.
        END.
        ELSE
            RETURN ERROR "Ошибка создания записи {&tt-acctflt365p-name}".
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
END PROCEDURE.

PROCEDURE AddAcctRecord365p.
    DEFINE INPUT PARAMETER iHAcct365p AS   HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iAcct      LIKE acct.acct NO-UNDO.
    DEFINE INPUT PARAMETER iFeature   AS   INT64     NO-UNDO.

    DEFINE VARIABLE vHBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE vHQuery  AS HANDLE NO-UNDO.

    vHBuffer = iHAcct365p:DEFAULT-BUFFER-HANDLE.
    IF NOT (VALID-HANDLE(vHBuffer) AND vHBuffer:BUFFER-CREATE()) THEN
        RETURN ERROR "Ошибка создания записи счета " + GetNullStr(iAcct).
    ASSIGN
        vHBuffer:BUFFER-FIELD("acct"):BUFFER-VALUE   = iAcct
        vHBuffer:BUFFER-FIELD("feature"):BUFFER-VALUE = iFeature
    NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND vHBuffer:BUFFER-RELEASE() THEN
        RETURN.
    vHBuffer:BUFFER-DELETE().
    RETURN ERROR "Ошибка сохранения записи счета " + GetNullStr(iAcct).
END PROCEDURE.

PROCEDURE FindAcctRecord365p.
    DEFINE INPUT  PARAMETER iHAcct365p       AS   HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAcct            LIKE acct.acct NO-UNDO.
    DEFINE OUTPUT PARAMETER oHAcctBuffer365p AS   HANDLE    NO-UNDO.

    DEFINE VARIABLE vHQuery AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(iHAcct365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-acct365p-name}".
    oHAcctBuffer365p = iHAcct365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(oHAcctBuffer365p).
    vHQuery:QUERY-PREPARE("FOR EACH " + oHAcctBuffer365p:NAME +
                          " WHERE "                           +
                          oHAcctBuffer365p:NAME + ".acct = '" +
                          iAcct                               +
                          "' NO-LOCK").
    vHQuery:QUERY-OPEN().
    vHQuery:GET-FIRST().
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    IF NOT oHAcctBuffer365p:AVAILABLE THEN
        oHAcctBuffer365p = ?.
END PROCEDURE.

PROCEDURE GetRequestPeriod365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER oReqDate  AS DATE   NO-UNDO.
    DEFINE OUTPUT PARAMETER oBegDate  AS DATE   NO-UNDO.
    DEFINE OUTPUT PARAMETER oEndDate  AS DATE   NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.
    DEFINE VARIABLE vTReqDate    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTBegDate    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTEndDate    AS CHARACTER NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    ASSIGN
        vTReqDate = "ДатаРешПр" WHEN vInfoType = {&INFO-TYPE-RPO}
        vTReqDate = "ДатаРешОт" WHEN vInfoType = {&INFO-TYPE-ROO}
        vTReqDate = "ДатаЗапр"  WHEN vInfoType = {&INFO-TYPE-ZNO}
    .
    IF vRequestKind = {&REQ-KIND-TICLAM} THEN
        ASSIGN
            vTBegDate = "ДатаНач"
            vTEndDate = "ДатаКон"
        .
    ELSE
        ASSIGN
            vTBegDate = vTReqDate
            vTEndDate = vTReqDate
        .
    IF {assigned vTReqDate} THEN DO:
        RUN GetDateAttrValue365p(iHReqInfo,
                                 vTReqDate,
                                 OUTPUT oReqDate)
        {&RAISE-ERROR}.
    END.
    IF {assigned vTBegDate} THEN DO:
        RUN GetDateAttrValue365p(iHReqInfo,
                                 vTBegDate,
                                 OUTPUT oBegDate)
        {&RAISE-ERROR}.
    END.
    IF {assigned vTEndDate} THEN DO:
        RUN GetDateAttrValue365p(iHReqInfo,
                                 vTEndDate,
                                 OUTPUT oEndDate)
        {&RAISE-ERROR}.
    END.
    IF vRequestKind =  {&REQ-KIND-TICLAM} AND
       oBegDate     <> ?                  AND
       oEndDate     <> ?                  AND
       oBegDate     >  oEndDate
    THEN
        RETURN ERROR vTBegDate + ": " + STRING(oBegDate) +
                     " больше "                          +
                     vTEndDate + ": " + STRING(oEndDate).
END PROCEDURE.

PROCEDURE EvaluateReqAcct365p.
    DEFINE INPUT PARAMETER iHReqInfo AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iHReqAcct AS HANDLE NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vAcctCur     AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vInfoType    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64     NO-UNDO.
    DEFINE VARIABLE vAcctTags    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vAcct        LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery      AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vI           AS   INT64     NO-UNDO.
    DEFINE VARIABLE vOK          AS   LOGICAL   NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    IF vInfoType = {&INFO-TYPE-PNO} THEN DO:
        vAcctTags = "НомСчПл,НомВалСч".
        DO vI = 1 TO NUM-ENTRIES(vAcctTags):
            RUN GetCharAttrValue365p(iHReqInfo,
                                     ENTRY(vI, vAcctTags),
                                     OUTPUT vAcct)
            {&RAISE-ERROR}.
            vAcct = DelFilFromAcct(vAcct).
            RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
            IF AVAILABLE acct THEN DO:
                vOK = YES.
                LEAVE.
            END.
        END.
    END.
    ELSE IF vRequestType = {&REQ-TYPE-SELECTED} THEN DO:
        RUN CheckReqAcct365p(iHReqAcct) {&RAISE-ERROR}.
        vHBuffer = iHReqAcct:DEFAULT-BUFFER-HANDLE.
        CREATE QUERY vHQuery.
        vHQuery:SET-BUFFERS(vHBuffer).
        vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
        vHQuery:QUERY-OPEN().
        REPEAT:
            vHQuery:GET-NEXT().
            IF vHQuery:QUERY-OFF-END THEN
                LEAVE.
            IF vHBuffer:AVAILABLE THEN DO:
                RUN GetCharAttrValue365p(vHBuffer,
                                         "НомСч",
                                         OUTPUT vAcct)
                NO-ERROR.
            END.
            RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
            IF AVAILABLE acct THEN DO:
                vOK = YES.
                LEAVE.
            END.
            &IF DEFINED(ExtBase) &THEN
            IF IsAcctEists(vAcct, OUTPUT vAcctCur) THEN
            DO:
                /* Данные по счету есть во внешней системе */
                vOK = YES.
                LEAVE.
            END.
            &ENDIF
        END.
        vHQuery:QUERY-CLOSE().
        DELETE OBJECT vHQuery.
    END.
    ELSE
        vOK = YES.
    IF vOK <> YES THEN
        RETURN ERROR "Указанные в запросе номера счетов не соотвествуют " +
                     "ни одному из счетов, существующих в системе".
END PROCEDURE.

PROCEDURE ValidateAcct365p.
    DEFINE INPUT  PARAMETER iAcct      LIKE acct.acct NO-UNDO.
    DEFINE INPUT  PARAMETER iHCust365p AS   HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iBegDate   AS   DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER iEndDate   AS   DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER iStrict    AS   LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oFeature   AS   INT64     NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vHCustBuffer365p AS HANDLE NO-UNDO.

    RUN FindAcct365p(BUFFER acct, iAcct) NO-ERROR.
    IF NOT AVAILABLE acct THEN DO:
        oFeature = {&FEAT-NOT-FOUND}.
        &IF DEFINED(ExtBase) &THEN
        IF RunExtQuery(iBegDate) THEN
        DO:
           /* Все проверки счета объединим в 1 внешнюю процедуру */
           RUN ExtValidateAcct IN h_plbnk(iAcct,
                                          iHCust365p,
                                          iBegDate,
                                          iEndDate,
                                          iStrict,
                                          INPUT-OUTPUT oFeature)
                                         {&RAISE-ERROR}.
        END.
        &ENDIF
        RETURN.
    END.
    RUN FindCustRecord365p(iHCust365p,
                           acct.cust-cat,
                           acct.cust-id,
                           OUTPUT vHCustBuffer365p)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        oFeature = {&FEAT-ERROR}.
        RETURN.
    END.
    IF NOT VALID-HANDLE(vHCustBuffer365p) THEN DO:
        oFeature = {&FEAT-WRONG-CUST}.
        RETURN.
    END.
    IF iBegDate        <> ? AND
       acct.close-date <> ? AND
       (iStrict OR
        acct.close-date <= iBegDate)
    THEN DO:
        oFeature = {&FEAT-CLOSED}.
        RETURN.
    END.
    IF iEndDate       <> ? AND
       acct.open-date <> ? AND
       acct.open-date >  iEndDate
    THEN DO:
        oFeature = {&FEAT-NOT-OPEN}.
        RETURN.
    END.
    oFeature = {&FEAT-OK}.
END PROCEDURE.

PROCEDURE MainValidateRequest365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqAcct      AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL                          NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode     AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText     AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE VARIABLE vInfoType      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind   AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType   AS INT64     NO-UNDO.
    DEFINE VARIABLE vCustINN       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vHCust365p     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE vHGoodAcct365p AS HANDLE    NO-UNDO.
    DEFINE VARIABLE vHBadAcct365p  AS HANDLE    NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    RUN GetCustINN365p(iHReqInfo, OUTPUT vCustINN) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    RUN EvaluateReqAcct365p(iHReqInfo, iHReqAcct) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    RUN GetCust365p(iHReqInfo,
                    iHReqAcct,
                    iCheckCorpName,
                    OUTPUT vHCust365p)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RUN DeleteObject365p(vHCust365p).
        RETURN.
    END.
    IF NOT vHCust365p:HAS-RECORDS THEN DO:
        ASSIGN
            oErrorCode = {&ERR-CUST}
            oErrorText = RETURN-VALUE
        .
        RUN DeleteObject365p(vHCust365p).
        RETURN.
    END.
    IF vInfoType = {&INFO-TYPE-ZNO} AND {assigned RETURN-VALUE} THEN DO:
        ASSIGN
            oErrorCode = {&ERR-SPECIAL}
            oErrorText = RETURN-VALUE
        .
        RUN DeleteObject365p(vHCust365p).
        RETURN.
    END.
    RUN GetAcctSplit365p(iHReqInfo,
                         iHReqAcct,
                         vHCust365p,
                         iCheckCorpName,
                         OUTPUT vHGoodAcct365p,
                         OUTPUT vHBadAcct365p)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
    ELSE IF NOT (vRequestType = {&REQ-TYPE-UNDEFINED} OR
                 vHGoodAcct365p:HAS-RECORDS)
    THEN DO:
        IF vRequestType <> {&REQ-TYPE-UNDEFINED} AND
           vHBadAcct365p:HAS-RECORDS
        THEN
            RUN GetAcctError365p(iHReqInfo,
                                 vHBadAcct365p,
                                 iCheckCorpName,
                                 OUTPUT oErrorCode,
                                 OUTPUT oErrorText).
        ELSE
            oErrorText = "Нет счетов для обработки".
        oErrorCode = {&ERR-OTHER}.
    END.
    RUN DeleteObject365p(vHCust365p).
    RUN DeleteObject365p(vHGoodAcct365p).
    RUN DeleteObject365p(vHBadAcct365p).
END PROCEDURE.

PROCEDURE PostValidateRequest365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iHCust365p     AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL                          NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode     AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText     AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE VARIABLE vInfoType     AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vRequestKind  AS   INT64                          NO-UNDO.
    DEFINE VARIABLE vRequestType  AS   INT64                          NO-UNDO.
    DEFINE VARIABLE vReqDate      AS   DATE                           NO-UNDO.
    DEFINE VARIABLE vBegDate      AS   DATE                           NO-UNDO.
    DEFINE VARIABLE vEndDate      AS   DATE                           NO-UNDO.
    DEFINE VARIABLE vCustINN      AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vCustName     AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vDocDate      LIKE op.doc-date                    NO-UNDO.
    DEFINE VARIABLE vDocNum       LIKE op.doc-num                     NO-UNDO.
    DEFINE VARIABLE vAmtRub       LIKE op-entry.amt-rub               NO-UNDO.
    DEFINE VARIABLE vAcctR        LIKE acct.acct                      NO-UNDO.
    DEFINE VARIABLE vAcctC        LIKE acct.acct                      NO-UNDO.
    DEFINE VARIABLE vAcct         LIKE acct.acct                      NO-UNDO.
    DEFINE VARIABLE vFeature      AS   INT64       INITIAL {&FEAT-OK} NO-UNDO.
    DEFINE VARIABLE vReceivership AS   LOGICAL                        NO-UNDO.
    DEFINE VARIABLE vInitDate     AS   DATE                           NO-UNDO.
    DEFINE VARIABLE vDate         AS   DATE                           NO-UNDO.
    DEFINE VARIABLE vPriority     AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vErrorDetails AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vUserFilter   AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vIsFeat-Trans AS   LOGICAL                        NO-UNDO.

    DEFINE BUFFER bacct FOR acct.

    IF NOT VALID-HANDLE(iHCust365p) THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = "Ошибка передачи параметров в процедуру " +
                         "окончательной проверки запроса".
        .
        RETURN.
    END.
    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    RUN GetCustINN365p(iHReqInfo,
                       OUTPUT vCustINN)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    CASE vInfoType:
        WHEN {&INFO-TYPE-PNO} THEN DO:
            RUN GetDateAttrValue365p(iHReqInfo,
                                     "ДатаПоруч",
                                     OUTPUT vDocDate)
            NO-ERROR.
            RUN GetCharAttrValue365p(iHReqInfo,
                                     "НомПоруч",
                                     OUTPUT vDocNum)
            NO-ERROR.
            RUN GetDecAttrValue365p(iHReqInfo,
                                    "СумПоруч",
                                    OUTPUT vAmtRub)
            NO-ERROR.
            RUN GetCharAttrValue365p(iHReqInfo,
                                     "ОчерПл",
                                     OUTPUT vPriority).
            RUN ValidatePNOOpAttrs(iHReqInfo,
                                   vDocDate,
                                   vDocNum,
                                   vAmtRub / 100.0,
                                   vPriority,
                                   OUTPUT oErrorCode,
                                   OUTPUT oErrorText)
            NO-ERROR.
            IF oErrorCode <> {&ERR-SUCCESS} THEN
                RETURN.
            RUN GetCharAttrValue365p(iHReqInfo,
                                     "НомСчПл",
                                     OUTPUT vAcctR)
            NO-ERROR.
            RUN GetCharAttrValue365p(iHReqInfo,
                                     "НомВалСч",
                                     OUTPUT vAcctC)
            NO-ERROR.
            RUN GetDateAttrValue365p(iHReqInfo,
                                     "ДатаТреб",
                                     OUTPUT vDocDate)
            NO-ERROR.
            vDate = vDocDate.
            IF {assigned vAcctR} OR {assigned vAcctC} THEN DO:
                IF {assigned vAcctR} THEN DO:
                    vAcct = vAcctR.
                    RUN ValidateAcct365p(vAcct,
                                         iHCust365p,
                                         gend-date,
                                         gend-date,
                                         YES,
                                         OUTPUT vFeature).
                    IF vFeature = {&FEAT-OK} THEN DO:
                        RUN GetDateAttrValue365p(iHReqInfo,
                                                 "ДатаПорВал",
                                                 OUTPUT vDocDate)
                        NO-ERROR.
                        RUN GetCharAttrValue365p(iHReqInfo,
                                                 "НомПорВал",
                                                 OUTPUT vDocNum)
                        NO-ERROR.
                        RUN ValidatePNOCurrAttrs(vAcctR,
                                                 vAcctC,
                                                 vDocDate,
                                                 vDocNum,
                                                 OUTPUT oErrorCode,
                                                 OUTPUT oErrorText)
                        NO-ERROR.
                        IF oErrorCode <> {&ERR-SUCCESS} THEN
                            RETURN.
                        RUN CheckReceivershipByAcct365p(vAcct,
                                                        vDate,
                                                        OUTPUT oErrorCode,
                                                        OUTPUT oErrorText).
                        IF oErrorCode <> {&ERR-SUCCESS} THEN
                            RETURN.

                        RUN FindAcct365p(BUFFER bacct, vAcct).
                        vUserFilter = TRNSettingValue("ExchSET-TAX", 
                                                      "AcctFilterStrict", 
                                                      "").
                        IF {assigned vUserFilter} THEN
                        DO:
                           RUN TestRecord  IN h_flt
                                  ("acct",
                                   Surrogate(BUFFER bacct:HANDLE),
                                   vUserFilter,
                                   OUTPUT vIsFeat-Trans).
                           IF vIsFeat-Trans THEN
                              vFeature = {&FEAT-TRANS-365P}.

                        END.
                        
                    END.
                END.
                IF {assigned vAcctC} AND vFeature = {&FEAT-OK} THEN DO:
                    vAcct = vAcctC.
                    RUN ValidateAcct365p(vAcct,
                                         iHCust365p,
                                         gend-date,
                                         gend-date,
                                         YES,
                                         OUTPUT vFeature).
                    IF vFeature = {&FEAT-OK} THEN DO:
                        RUN CheckReceivershipByAcct365p(vAcct,
                                                        vDate,
                                                        OUTPUT oErrorCode,
                                                        OUTPUT oErrorText).
                        IF oErrorCode <> {&ERR-SUCCESS} THEN
                            RETURN.
                    END.
                END.
                IF vFeature <> {&FEAT-OK} THEN DO:
                    IF vFeature = {&FEAT-WRONG-CUST} THEN DO:
                       IF iCheckCorpName THEN DO:
                          RUN GetCustName365p(iHReqInfo,
                                              OUTPUT vCustName)
                          {&RAISE-ERROR}.
                          vErrorDetails = QUOTER(vCustName).
                       END.
                       vErrorDetails = vErrorDetails + " с ИНН = " + vCustINN.
                    END.
                    ASSIGN
                        oErrorCode = getAcctErrorCode(vFeature)
                        oErrorText = getAcctErrorText(vAcct,
                                                      vFeature,
                                                      TRIM(vErrorDetails))
                    .
                END.
                ELSE IF vReceivership = YES THEN
                    ASSIGN
                        oErrorCode = {&ERR-OTHER}
                        oErrorText = "По данному клиенту " +
                                     "введено конкурсное управление"
                    .
            END.
            ELSE
                ASSIGN
                    oErrorCode = {&ERR-ACCT}
                    oErrorText = "Не указан счет плательщика".
                .
        END.
        WHEN {&INFO-TYPE-ZNO} THEN DO:
            IF vRequestKind = {&REQ-KIND-TICLAM} THEN DO:
                RUN GetRequestPeriod365p(iHReqInfo,
                                         OUTPUT vReqDate,
                                         OUTPUT vBegDate,
                                         OUTPUT vEndDate)
                NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    ASSIGN
                        oErrorCode = {&ERR-OTHER}
                        oErrorText = RETURN-VALUE
                    .
                    RETURN.
                END.
                vInitDate = DATE(mDate_NR) NO-ERROR.
                IF vBegDate < vInitDate THEN
                    RUN Fill-SysMes IN h_tmess ("",
                                                "",
                                                "0",
                                                "Диапазон запроса ДатаНач:"    +
                                                STRING(vBegDate, "99.99.9999") +
                                                " выходит за рамки начального" +
                                                " решения Дата_НР:"            +
                                                STRING(vInitDate, "99.99.9999")).
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE ValidatePNOOpAttrs.
    DEFINE INPUT  PARAMETER iHReqInfo  AS HANDLE                                    NO-UNDO.
    DEFINE INPUT  PARAMETER iDocDate   LIKE op.doc-date                             NO-UNDO.
    DEFINE INPUT  PARAMETER iDocNum    LIKE op.doc-num                              NO-UNDO.
    DEFINE INPUT  PARAMETER iAmtRub    LIKE op-entry.amt-rub                        NO-UNDO.
    
    DEFINE INPUT  PARAMETER iPriority  AS   CHARACTER                               NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS   CHARACTER        INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText AS   CHARACTER        INITIAL ""             NO-UNDO.

    DEFINE BUFFER FileExch   FOR FileExch.
    DEFINE BUFFER Packet     FOR Packet.
    DEFINE BUFFER PackObject FOR PackObject.
    DEFINE BUFFER op         FOR op.
    DEFINE BUFFER op-entry   FOR op-entry.

    DEFINE VARIABLE vDelta    AS INT64 NO-UNDO.
    DEFINE VARIABLE vLimit    AS CHAR  NO-UNDO.
    DEFINE VARIABLE vPriority AS CHAR  NO-UNDO.
    
    DEFINE VARIABLE vPNOFName AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE vInfoType    AS   CHARACTER           NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64               NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64               NO-UNDO.
    DEFINE VARIABLE vHReqServ    AS   HANDLE              NO-UNDO.
    DEFINE VARIABLE vFileExchID  LIKE FileExch.FileExchID NO-UNDO.
    DEFINE VARIABLE vFileName    LIKE FileExch.Name       NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.
    IF vInfoType <> {&INFO-TYPE-PNO} THEN
        RETURN.
    RUN GetRequestServicePart365p(iHReqInfo, OUTPUT vHReqServ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = RETURN-VALUE
        .
        RETURN.
    END.

    ASSIGN
        oErrorCode = {&ERR-OTHER}
        vDelta     = TODAY - iDocDate
        iDocNum    = ""
                     WHEN iDocNum = ?
        vLimit     = mKontrDP_365
        vPriority  = mKontrOP_365
    .
    IF vDelta = ? THEN DO:
        oErrorText = "Не указана дата поручения".
        RETURN.
    END.
    IF {assigned vLimit} AND vDelta > INT64(vLimit) THEN DO:
        oErrorText = "Прошло " + STRING(vDelta) + " дней от даты поручения".
        RETURN.
    END.
    IF NOT CAN-DO(vPriority, iPriority) THEN DO:
        oErrorText = "Значение очередности платежа " + STRING(iPriority) + " не соответствует нормативному".
        RETURN.
    END.
    /**/
    RUN GetIntAttrValue365p(vHReqServ,
                            "FileExchID",
                            OUTPUT vFileExchID)
    NO-ERROR.
    FIND FIRST FileExch WHERE
        FileExch.FileExchID = vFileExchID
    NO-LOCK NO-ERROR.
    IF NOT (AVAILABLE FileExch AND {assigned FileExch.Name}) THEN DO:
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = (IF AVAILABLE FileExch THEN
                              "В наборе данных запроса не указано имя файла"
                          ELSE
                              "Не найден набор данных запроса")
        .
        RETURN.
    END.
    vPNOFName = FileExch.Name.
    /**/
    FOR EACH FileExch WHERE
        FileExch.Name BEGINS "PNO"
    NO-LOCK,
    EACH Packet WHERE
        Packet.FileExchID = FileExch.FileExchID
    NO-LOCK,
    EACH PackObject WHERE
        PackObject.PacketID  = Packet.PacketID AND
        PackObject.file-name = "op"
    NO-LOCK,
    FIRST op WHERE
        op.op       = INT64(PackObject.surrogate) AND
        op.doc-date = iDocDate                    AND
        op.doc-num  = iDocNum
    NO-LOCK,
    FIRST op-entry OF op WHERE
        op-entry.amt-rub = iAmtRub
    NO-LOCK:
        IF SUBSTRING(FileExch.Name,13,4) EQ SUBSTRING(vPNOFName,13,4) THEN
        DO:  
           oErrorText = "Повторная загрузка поручения"                                   +
                        " № " + iDocNum                                                  +
                        " от " + date2str(iDocDate)                                      +  
                        ", сумма в нац. валюте: " + STRING(iAmtRub) + ","                +
                        " номер налоговой инспекции " + SUBSTRING(vPNOFName,13,4) + ". " +
                        "Документ уже загружен " + date2str(Packet.PackDate)             +
                        " в " + STRING(Packet.PackTime, "HH:MM:SS").
           RETURN.
        END.
    END.
    oErrorCode = {&ERR-SUCCESS}.
END PROCEDURE.

PROCEDURE ValidatePNOCurrAttrs.
    DEFINE INPUT  PARAMETER iAcctR      LIKE acct.acct                          NO-UNDO.
    DEFINE INPUT  PARAMETER iAcctC      LIKE acct.acct                          NO-UNDO.
    DEFINE INPUT  PARAMETER iCurDocDate LIKE op.doc-date                        NO-UNDO.
    DEFINE INPUT  PARAMETER iCurDocNum  LIKE op.doc-num                         NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode  AS   CHARACTER   INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText  AS   CHARACTER   INITIAL ""             NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    RUN FindAcct365p(BUFFER acct, iAcctR) NO-ERROR.
    IF {assigned acct.currency} AND
       (NOT {assigned iAcctC} OR
        iCurDocDate = ?       OR
        NOT {assigned iCurDocNum})
    THEN
        ASSIGN
            oErrorCode = {&ERR-OTHER}
            oErrorText = "Отсутствует блок, " +
                         "связанный с поручением на продажу валюты"
        .
END PROCEDURE.

PROCEDURE CheckReceivershipByAcct365p.
    DEFINE INPUT  PARAMETER iAcct      LIKE acct.acct     NO-UNDO.
    DEFINE INPUT  PARAMETER iDate      AS   DATE          NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS   CHARACTER     NO-UNDO INITIAL "10".
    DEFINE OUTPUT PARAMETER oErrorText AS   CHARACTER     NO-UNDO.

    DEFINE BUFFER acct365p  FOR acct.
    DEFINE BUFFER code      FOR code.
    DEFINE BUFFER loan      FOR loan.
    DEFINE BUFFER signs     FOR signs.

    DEFINE VARIABLE vBStage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

    RUN FindAcct365p(BUFFER acct, iAcct) NO-ERROR.
    IF NOT CAN-DO({&BANKRUPCY-CUST-CATS}, acct.cust-cat) THEN
        RETURN.
    vBStage = ClientXAttrVal(acct.cust-cat, acct.cust-id, "КонУпр").
    FIND FIRST code WHERE
        code.class = "НБКИ_ТипБанкрот" AND
        code.name  = vBStage
    NO-LOCK NO-ERROR.
    IF NOT (AVAILABLE code) THEN
        RETURN.
    FOR EACH loan WHERE
        loan.cust-cat   = acct.cust-cat AND
        loan.cust-id    = acct.cust-id  AND
        loan.class-code = "bankrupt"    AND
        (loan.end-date = ? OR loan.end-date > iDate)
    NO-LOCK,
    FIRST signs WHERE
        signs.file-name  = "loan"                               AND
        signs.surrogate  = loan.contract + "," + loan.cont-code AND
        signs.code       = "ТипБанкр"                           AND
        signs.code-value = code.code
    NO-LOCK
    BY loan.end-date DESCENDING:
        ASSIGN
            vCustName  = GetCliName(acct.cust-cat,
                                    STRING(acct.cust-id),
                                    OUTPUT vS,
                                    OUTPUT vS,
                                    OUTPUT vS,
                                    INPUT-OUTPUT vS,
                                    OUTPUT vS,
                                    OUTPUT vS)
            vS         = "решения №" + loan.doc-num +
                         " от " + STRING(loan.end-date, "99.99.9999") +
                         " " + loan.user-o[1]
            oErrorCode = "35"
            oErrorText = "По " + (IF {assigned vCustName} THEN vCustName
                                                          ELSE "клиенту") +
                         " введено " + vBStage +
                         (IF {assigned vS} THEN (" на основании " + vS)
                                           ELSE "")
        .
        LEAVE.
    END.
END PROCEDURE.

PROCEDURE GetAcctSplit365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqAcct      AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iHCust365p     AS HANDLE  NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oHGoodAcct365p AS HANDLE  NO-UNDO.
    DEFINE OUTPUT PARAMETER oHBadAcct365p  AS HANDLE  NO-UNDO.

    DEFINE VARIABLE vInfoType    AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS   INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS   INT64     NO-UNDO.
    DEFINE VARIABLE vS           AS   CHARACTER NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery      AS   HANDLE    NO-UNDO.
    DEFINE VARIABLE vAcct        LIKE acct.acct NO-UNDO.
    DEFINE VARIABLE vFeature     AS   INT64     NO-UNDO.

    RUN GetAcct365p(iHReqInfo,
                    iHReqAcct,
                    iHCust365p,
                    iCheckCorpName,
                    OUTPUT oHBadAcct365p)
    {&RAISE-ERROR}.
    RUN CreateTTAcct365p(OUTPUT oHGoodAcct365p) {&RAISE-ERROR}.
    IF NOT oHBadAcct365p:HAS-RECORDS THEN
        RETURN.
    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    vHBuffer = oHBadAcct365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " EXCLUSIVE-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
        RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
        IF NOT qualifiesAsError(vFeature,
                                vInfoType,
                                vRequestKind,
                                vRequestType)
        THEN DO:
            RUN AddAcctRecord365p(oHGoodAcct365p, vAcct, vFeature) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                vHQuery:QUERY-CLOSE().
                DELETE OBJECT vHQuery.
                RETURN ERROR RETURN-VALUE.
            END.
            vHBuffer:BUFFER-DELETE().
        END.
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
END PROCEDURE.

PROCEDURE ValidateRequest365p.
    DEFINE INPUT  PARAMETER iHReqInfo  AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqAcct  AS HANDLE                           NO-UNDO.
    DEFINE INPUT  PARAMETER iConfig    AS CHARACTER                        NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS CHARACTER INITIAL {&ERR-SUCCESS} NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText AS CHARACTER INITIAL ""             NO-UNDO.

    DEFINE VARIABLE vS             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCheckCorpName AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vHCust365p     AS HANDLE    NO-UNDO.

    RUN GetValidationParameter365p(iConfig, "CheckCorpName", "YES", OUTPUT vS).
    vCheckCorpName = (vS = "YES").
    RUN PreValidateRequest365p(iHReqInfo,
                               OUTPUT oErrorCode,
                               OUTPUT oErrorText).
    IF oErrorCode <> {&ERR-SUCCESS} THEN
        RETURN.
    RUN MainValidateRequest365p(iHReqInfo,
                                iHReqAcct,
                                vCheckCorpName,
                                OUTPUT oErrorCode,
                                OUTPUT oErrorText).
    IF oErrorCode <> {&ERR-SUCCESS} THEN
        RETURN.
    RUN GetCust365p(iHReqInfo,
                    iHReqAcct,
                    vCheckCorpName,
                    OUTPUT vHCust365p)
    NO-ERROR.
    RUN PostValidateRequest365p(iHReqInfo,
                                vHCust365p,
                                vCheckCorpName,
                                OUTPUT oErrorCode,
                                OUTPUT oErrorText).
    RUN DeleteObject365p(vHCust365p).
END PROCEDURE.

PROCEDURE GetAcctError365p.
    DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iHAcct365p     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorText     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.
    DEFINE VARIABLE vCustINN     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vReqDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE vBegDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE vEndDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE vHBuffer     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE vHQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE vAcct        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCAcct       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFeature     AS INT64     NO-UNDO.
    DEFINE VARIABLE vCFeature    AS INT64     NO-UNDO.
    DEFINE VARIABLE vDetails     AS CHARACTER NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    IF NOT VALID-HANDLE(iHAcct365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-acct365p-name}".
    RUN GetCustINN365p(iHReqInfo, OUTPUT vCustINN) {&RAISE-ERROR}.
    IF iCheckCorpName THEN DO:
       RUN GetCustName365p(iHReqInfo, OUTPUT vCustName) {&RAISE-ERROR}.
    END.
    RUN GetRequestPeriod365p(iHReqInfo,
                             OUTPUT vReqDate,
                             OUTPUT vBegDate,
                             OUTPUT vEndDate)
    {&RAISE-ERROR}.
    vHBuffer = iHAcct365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " WHERE "             +
                          vHBuffer:NAME + ".feature <> " + STRING({&FEAT-OK}) +
                          " NO-LOCK BREAK "                                   +
                          "BY " + vHBuffer:NAME + ".feature "                 +
                          "BY " + vHBuffer:NAME + ".acct ").
    vHQuery:QUERY-OPEN().
    ASSIGN
        vCFeature  = {&FEAT-UNDEFINED}
        vCAcct     = ""
        oErrorCode = ""
        oErrorText = ""
    .
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer,
                                 "acct",
                                 OUTPUT vAcct)
        NO-ERROR.
        vAcct = DelFilFromAcct(vAcct).
        RUN GetIntAttrValue365p(vHBuffer,
                                "feature",
                                OUTPUT vFeature)
        NO-ERROR.
        IF vCFeature <> vFeature THEN DO:
            ASSIGN
                oErrorCode = oErrorCode                            +
                             (IF oErrorCode = "" THEN "" ELSE ",") +
                             getAcctErrorCode(vFeature)
                             WHEN NOT CAN-DO(oErrorCode,
                                             getAcctErrorCode(vFeature))
                vCFeature  = vFeature
                vDetails   = (IF iCheckCorpName
                              THEN (QUOTER(vCustName) + " ")
                              ELSE "") +
                             "с ИНН = " + vCustINN
                             WHEN vFeature = {&FEAT-WRONG-CUST}
                vDetails   = STRING(vBegDate)
                             WHEN vFeature = {&FEAT-CLOSED}
                vDetails   = STRING(vEndDate)
                             WHEN vFeature = {&FEAT-NOT-OPEN}
                oErrorText = TRIM(oErrorText, ", ")
            .
            oErrorText = oErrorText                               +
                         (IF oErrorText = "" THEN "" ELSE ". ")   +
                         getAcctErrorText("", vFeature, vDetails) +
                         ": ".
        END.
        IF vCAcct <> vAcct THEN
            ASSIGN
                vCAcct     = vAcct
                oErrorText = oErrorText + vAcct + ", "
            .
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    oErrorText = TRIM(oErrorText, ", ") + ".".
    ASSIGN
        oErrorCode = {&ERR-OTHER} WHEN NUM-ENTRIES(oErrorCode) > 1
        oErrorText = limitStr(oErrorText, {&MAX-ERROR-TEXT-LENGTH-365P})
    .
END PROCEDURE.

PROCEDURE SetValidationParameter365p.
    DEFINE INPUT-OUTPUT PARAMETER ioConfig AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iName    AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iValue   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vI      AS INT64     NO-UNDO.
    DEFINE VARIABLE vS      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vConfig AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFound  AS LOGICAL   NO-UNDO.

    ASSIGN
        vConfig = ""
        vFound  = NO
    .
    DO vI = 1 TO NUM-ENTRIES(ioConfig, CHR(1)):
        vS = ENTRY(vI, ioConfig, CHR(1)).
        IF ENTRY(1, vS, "=") = iName THEN
            ASSIGN
                ENTRY(2, vS, "=") = iValue
                vFound            = YES
            .
        vConfig = vConfig + (IF vConfig = "" THEN "" ELSE CHR(1)) + vS.
    END.
    IF NOT vFound THEN
        vConfig = vConfig                               +
                  (IF vConfig = "" THEN "" ELSE CHR(1)) +
                  iName + "=" + iValue.
    ioConfig = vConfig.
END PROCEDURE.

PROCEDURE GetValidationParameter365p.
    DEFINE INPUT  PARAMETER iConfig  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iName    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iDefault AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oValue   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vI AS INT64     NO-UNDO.
    DEFINE VARIABLE vS AS CHARACTER NO-UNDO.

    DO vI = 1 TO NUM-ENTRIES(iConfig, CHR(1)):
        vS = ENTRY(vI, iConfig, CHR(1)).
        IF ENTRY(1, vS, "=") = iName THEN DO:
            oValue = ENTRY(2, vS, "=").
            RETURN.
        END.
    END.
    oValue = iDefault.
END PROCEDURE.

PROCEDURE CheckClass365p.
    DEFINE INPUT PARAMETER iClassCode LIKE class.class-code NO-UNDO.

    DEFINE BUFFER class FOR class.

    FIND FIRST class WHERE
        class.class-code = iClassCode
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE class THEN
        RETURN ERROR "Неизвестный класс объекта: " +
                     QUOTER(GetNullStr(iClassCode)).
END PROCEDURE.

PROCEDURE CreateTransportTable365p.
    DEFINE INPUT  PARAMETER iClassCode LIKE class.class-code NO-UNDO.
    DEFINE OUTPUT PARAMETER oHTable    AS   HANDLE           NO-UNDO.

    DEFINE BUFFER xattr FOR xattr.

    DEFINE VARIABLE vTableName AS CHARACTER NO-UNDO.

    RUN CheckClass365p(iClassCode) {&RAISE-ERROR}.
    vTableName = "trans-" + iClassCode.
    CREATE TEMP-TABLE oHTable.
    RUN CheckHandle365p(oHTable,
                        "Ошибка создания транспортной таблицы " +
                        GetNullStr(vTableName))
    {&RAISE-ERROR}.
    FOR EACH xattr WHERE
        xattr.class-code = iClassCode              AND
        NOT CAN-DO("class,group", xattr.data-type) AND
        (CAN-DO("ID,UpID", xattr.xattr-code) OR
         xattr.order <> ? AND xattr.order <> 0)
    NO-LOCK
    BY xattr.order:
        IF NOT oHTable:ADD-NEW-FIELD(GetMangledName(xattr.xattr-code),
                                     xattr.data-type,
                                     0,
                                     xattr.data-format)
        THEN DO:
            RUN DeleteObject365p(oHTable).
            RETURN ERROR "Ошибка создания поля "    +
                         QUOTER(xattr.xattr-code)   +
                         " в транспортной таблице " +
                         QUOTER(vTableName).
        END.
    END.
    IF oHTable:TEMP-TABLE-PREPARE(vTableName) THEN
        RETURN.
    RUN DeleteObject365p(oHTable).
    RETURN ERROR "Ошибка подготовки транспортной таблицы " +
                 GetNullStr(vTableName).
END PROCEDURE.

PROCEDURE SetAttrValueVerbose365p.
    DEFINE INPUT PARAMETER iObject    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAttrValue AS CHARACTER NO-UNDO.

    RUN SetAttrValue365p(iObject, iAttrName, iAttrValue) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
    IF RETURN-VALUE <> "" THEN
        RUN Fill-SysMes IN h_tmess ("", "", "0", RETURN-VALUE).
END PROCEDURE.

PROCEDURE SetAttrValue365p.
    DEFINE INPUT  PARAMETER iHObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrValue AS CHARACTER NO-UNDO.

    DEFINE BUFFER class FOR class.
    DEFINE BUFFER xattr FOR xattr.

    DEFINE VARIABLE vValidAttr AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vClassName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vMessage   AS CHARACTER NO-UNDO.

    RUN CheckHandle365p(iHObject, "Объект не найден") {&RAISE-ERROR}.
    vValidAttr = VALID-HANDLE(iHObject:BUFFER-FIELD(iAttrName))
                 NO-ERROR.
    IF vValidAttr <> YES THEN
        RETURN ERROR "Неизвестный атрибут: " + QUOTER(GetNullStr(iAttrName)).
    vClassName = getBaseTableName(iHObject:NAME).
    FIND FIRST class WHERE
        class.class-code = vClassName
    NO-LOCK NO-ERROR.
    IF AVAILABLE class THEN DO:
        FIND FIRST xattr OF class WHERE
            xattr.xattr-code = iAttrName
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE xattr THEN
        FIND FIRST xattr OF class WHERE
            xattr.xattr-code = GetOriginalName(iAttrName)
        NO-LOCK NO-ERROR.
        IF AVAILABLE xattr THEN DO:
            IF (xattr.mandatory OR CAN-DO({&CONDITIONAL-ATTRS-365P}, iAttrName))
               AND
               NOT {assigned iAttrValue}
            THEN
                ASSIGN
                    iAttrValue = xattr.initial
                    vMessage   = "Пустое значение обязательного реквизита " +
                                 QUOTER(GetNullStr(iAttrName))              +
                                 " класса "                                 +
                                 QUOTER(GetNullStr(vClassName))             +
                                 ". Использовано значение по умолчанию "    +
                                 QUOTER(GetNullStr(xattr.initial))
                .
        END.
        ELSE
            vMessage = "Неизвестный реквизит "        +
                        QUOTER(GetNullStr(iAttrName)) +
                        " класса "                    +
                        QUOTER(GetNullStr(vClassName)).
    END.
    ELSE
        vMessage = "Неизвестный класс объекта: " +
                   QUOTER(GetNullStr(vClassName)).
    iHObject:BUFFER-FIELD(iAttrName):BUFFER-VALUE = iAttrValue.
    IF {assigned vMessage} THEN
        RETURN vMessage.
END PROCEDURE.

PROCEDURE GenGUID.
    DEFINE OUTPUT PARAMETER oGUID AS CHARACTER NO-UNDO.

    oGUID = GUID.
END PROCEDURE.

PROCEDURE GetCorrAcct.
    DEFINE OUTPUT PARAMETER oCorrAcct LIKE acct.acct NO-UNDO.

    DEFINE BUFFER acct     FOR acct.
    DEFINE BUFFER c-nostro FOR c-nostro.

    oCorrAcct = mKorrACCT.
    {find-act.i &acct = oCorrAcct}
    IF NOT AVAILABLE acct THEN DO:
        FIND FIRST c-nostro WHERE
            c-nostro.corr-acct = oCorrAcct AND
            NOT {assigned c-nostro.currency}
        NO-LOCK NO-ERROR.
        IF AVAILABLE c-nostro THEN
            {find-act.i &acct = c-nostro.acct}
    END.
    oCorrAcct = IF AVAILABLE acct THEN acct.acct ELSE "".
END PROCEDURE.

PROCEDURE CreateObject365p.
    DEFINE INPUT  PARAMETER iOpKind    LIKE op-kind.op-kind NO-UNDO.
    DEFINE INPUT  PARAMETER iOpDate    LIKE op-date.op-date NO-UNDO.
    DEFINE OUTPUT PARAMETER oSurrogate AS   CHARACTER       NO-UNDO.

    DEFINE BUFFER op-kind-tmpl   FOR op-kind-tmpl.
    DEFINE BUFFER op-kind-tmpl-1 FOR op-kind-tmpl.

    DEFINE VARIABLE vHObj AS HANDLE.
    DEFINE VARIABLE vHSub AS HANDLE.

    DEFINE VARIABLE vI            AS INT64     NO-UNDO.
    DEFINE VARIABLE vObjRowid     AS ROWID     NO-UNDO.

    FIND FIRST op-kind-tmpl WHERE
        op-kind-tmpl.op-kind = iOpKind AND
        op-kind-tmpl.parent  = 0
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE op-kind-tmpl THEN
        RETURN.
    RUN CreateObjectFromTemplate365p((BUFFER op-kind-tmpl:HANDLE),
                                     iOpDate,
                                     ?,
                                     OUTPUT vHObj).
    IF NOT VALID-HANDLE(vHObj) THEN
        RETURN.
    vObjRowid = vHObj:ROWID.
    vHObj:BUFFER-RELEASE().
    vHObj:FIND-BY-ROWID(vObjRowid, EXCLUSIVE-LOCK, NO-WAIT).
    FOR EACH op-kind-tmpl-1 WHERE
        op-kind-tmpl-1.op-kind = iOpKind AND
        op-kind-tmpl-1.parent  = op-kind-tmpl.tmpl-id
    NO-LOCK:
        RUN CreateObjectFromTemplate365p((BUFFER op-kind-tmpl-1:HANDLE),
                                         iOpDate,
                                         vHObj,
                                         OUTPUT vHSub).
        IF NOT VALID-HANDLE(vHSub) THEN
            NEXT.
    END.
    oSurrogate = Surrogate(vHObj).
END PROCEDURE.

PROCEDURE CreateObjectFromTemplate365p.
    DEFINE INPUT  PARAMETER iHTemplate AS   HANDLE          NO-UNDO.
    DEFINE INPUT  PARAMETER iOpDate    LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT  PARAMETER iHUpObject AS   HANDLE          NO-UNDO.
    DEFINE OUTPUT PARAMETER oHObject   AS   HANDLE          NO-UNDO.

    DEFINE BUFFER op-kind-tmpl-ln FOR op-kind-tmpl-ln.

    DEFINE VARIABLE vI            AS INT64     NO-UNDO.
    DEFINE VARIABLE vS            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vOK           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vTableName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vObjTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vObjTableSurr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFieldName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFieldList    AS CHARACTER NO-UNDO.

    IF NOT VALID-HANDLE(iHTemplate) THEN
        RETURN.
    vS = iHTemplate:BUFFER-FIELD("work-class-code"):BUFFER-VALUE.
   vTableName = GetXClassProgress (vS).
    IF NOT {assigned vTableName} THEN
        RETURN.
    RUN PrepareInstance IN h_data ("*").
    RUN GetInstance IN h_data (vTableName,
                               ?,
                               OUTPUT oHObject,
                               OUTPUT vOK)
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.
    oHObject = oHObject:DEFAULT-BUFFER-HANDLE.
    oHObject:FIND-FIRST().
    RUN SetInstanceProp IN h_data (oHObject,
                                   "__template",
                                   "NO",
                                   OUTPUT vOk).
    RUN SetInstanceProp IN h_data (oHObject,
                                   "__mode",
                                   {&MOD_EDIT},
                                   OUTPUT vOk).
    CASE vTableName:
        WHEN "op" THEN
            ASSIGN
                oHObject:BUFFER-FIELD("op-kind"):BUFFER-VALUE =
                    iHTemplate:BUFFER-FIELD("op-kind"):BUFFER-VALUE
                oHObject:BUFFER-FIELD("op-transaction"):BUFFER-VALUE =
                    NEXT-VALUE(op-transaction-id)
            .
        WHEN "op-entry" THEN
            oHObject:BUFFER-FIELD("op-entry"):BUFFER-VALUE = 1.
    END.
    FOR EACH op-kind-tmpl-ln WHERE
        op-kind-tmpl-ln.tmpl-id = iHTemplate:BUFFER-FIELD("tmpl-id"):BUFFER-VALUE
    NO-LOCK:
        vFieldName = op-kind-tmpl-ln.xattr-code.
        {additem.i vFieldList vFieldName}
        oHObject:BUFFER-FIELD(GetMangledName(vFieldName)):BUFFER-VALUE =
            op-kind-tmpl-ln.xattr-value.
    END.
    DO vI = 1 TO oHObject:NUM-FIELDS:
        CASE oHObject:BUFFER-FIELD(vI):NAME:
            WHEN "class-code" THEN
                oHObject:BUFFER-FIELD(vI):BUFFER-VALUE = iHTemplate:BUFFER-FIELD("work-class-code"):BUFFER-VALUE.
            WHEN "op-date" THEN
                oHObject:BUFFER-FIELD(vI):BUFFER-VALUE = iOpDate.
            WHEN "filial-id" THEN DO:
                RUN GetThisFilialId(OUTPUT vS).
                oHObject:BUFFER-FIELD(vI):BUFFER-VALUE = vS.
            END.
        END.
    END.
    IF VALID-HANDLE(iHUpObject) THEN DO:
      vObjTableName = GetXClassProgress (iHUpObject:BUFFER-FIELD("class-code"):BUFFER-VALUE).
        vObjTableSurr = GetTableSurrogate(vObjTableName).
        DO vI = 1 TO NUM-ENTRIES(vObjTableSurr):
            vFieldName = ENTRY(vI, vObjTableSurr).
            oHObject:BUFFER-FIELD(vFieldName):BUFFER-VALUE = iHUpObject:BUFFER-FIELD(vFieldName):BUFFER-VALUE.
        END.
    END.
    RUN SetExtAttrsToSaveInInstance IN h_data (oHObject:TABLE-HANDLE,
                                               vFieldList).
    RUN SetInstance IN h_data (vTableName,
                               oHObject:TABLE-HANDLE,
                               OUTPUT vOK)
    NO-ERROR.
END PROCEDURE.

PROCEDURE GetThisFilialNum.
    DEFINE OUTPUT PARAMETER oFilialNum AS CHARACTER NO-UNDO.

    DEFINE BUFFER branch FOR branch.

    DEFINE VARIABLE vFilialId AS CHARACTER NO-UNDO.

    RUN GetThisFilialId(OUTPUT vFilialId).
    FIND FIRST branch WHERE branch.branch-id = vFilialId NO-LOCK NO-ERROR.
    IF AVAILABLE branch THEN DO:
        oFilialNum = GetXAttrValueEx("branch",
                                     Surrogate(BUFFER branch:HANDLE),
                                     "REGN",
                                     "").
        oFilialNum = getFilialNum(oFilialNum).
    END.
END PROCEDURE.

PROCEDURE GetCloseFilialsNum.
    DEFINE OUTPUT PARAMETER oClsFilLst AS CHARACTER NO-UNDO.

    DEFINE BUFFER branch FOR branch.

    DEFINE VARIABLE vFilialId  AS CHARACTER NO-UNDO.

    RUN GetThisFilialId(OUTPUT vFilialId).
    FIND FIRST branch WHERE branch.branch-id = vFilialId NO-LOCK NO-ERROR.
    IF AVAILABLE branch THEN 
       oClsFilLst = GetXAttrValueEx("branch",
                                    Surrogate(BUFFER branch:HANDLE),
                                    "CloseFilials",
                                    "").
END PROCEDURE.

PROCEDURE FixCustName365p.
    DEFINE INPUT-OUTPUT PARAMETER ioCustName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vI    AS INT64                NO-UNDO.
    DEFINE VARIABLE vJ    AS INT64     INITIAL 0  NO-UNDO.
    DEFINE VARIABLE vS    AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE vT    AS CHARACTER INITIAL "" NO-UNDO.
    DEFINE VARIABLE vSkip AS LOGICAL   INITIAL NO NO-UNDO.

    DO vI = 1 TO LENGTH(ioCustName):
        IF vJ > 1 THEN DO:
            vT = vT + RIGHT-TRIM(SUBSTRING(ioCustName, vI)).
            LEAVE.
        END.
        vS = SUBSTRING(ioCustName, vI, 1).
        IF vS = "" THEN DO:
            IF NOT vSkip THEN
                ASSIGN
                    vJ    = vJ + 1
                    vT    = vT + ","
                    vSkip = YES
                .
        END.
        ELSE
            ASSIGN
                vSkip = NO
                vT = vT + vS
            .
    END.
    ASSIGN
        ioCustName = IF {assigned vT} THEN TRIM(vT, ",") ELSE ""
        ioCustName = ",," WHEN NOT {assigned ioCustName}
        ioCustName = ioCustName + FILL(",", 3 - NUM-ENTRIES(ioCustName))
    .
END PROCEDURE.

PROCEDURE TrimZNOCustName365p.
    DEFINE INPUT-OUTPUT PARAMETER ioCustName AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iINN       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iKPP       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE xs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vI AS INT64     NO-UNDO.

    ASSIGN
        iINN = IF {assigned iINN} THEN TRIM(iINN)
                                  ELSE ""
        iKPP = IF {assigned iKPP} THEN TRIM(iKPP)
                                  ELSE ""
    .
    REPEAT:
        ioCustName = LEFT-TRIM(ioCustName).
        DO vI = 1 TO LENGTH(ioCustName):
            xs = SUBSTRING(ioCustName, vI, 1).
            IF TRIM(xs) = "" THEN
                LEAVE.
        END.
        xs = RIGHT-TRIM(SUBSTRING(ioCustName, 1, vI)).
        IF xs = "" THEN
            RETURN.
        CASE xs:
            WHEN "ИНН" THEN
                ioCustName = LEFT-TRIM(LEFT-TRIM(SUBSTRING(ioCustName,
                                                           LENGTH("ИНН") + 1)),
                                       "0123456789").
            WHEN "КПП" THEN
                ioCustName = LEFT-TRIM(LEFT-TRIM(SUBSTRING(ioCustName,
                                                           LENGTH("КПП") + 1)),
                                       "0123456789").
            WHEN iINN THEN
                ioCustName = LEFT-TRIM(SUBSTRING(ioCustName,
                                                 LENGTH(iINN) + 1)).
            WHEN iKPP THEN
                IF iKPP <> iINN THEN
                    ioCustName = LEFT-TRIM(SUBSTRING(ioCustName,
                                                     LENGTH(iKPP) + 1)).
            OTHERWISE
                LEAVE.
        END.
    END.
    ioCustName = TRIM(ioCustName).
END PROCEDURE.

PROCEDURE CheckZNOOpLimit365p.
    DEFINE INPUT PARAMETER iRequestKind AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER iNumDocs     AS INT64 NO-UNDO.

    DEFINE VARIABLE vMaxNumDocs AS INT64 NO-UNDO.

    IF iRequestKind = {&REQ-KIND-TICLAM} THEN DO:
        vMaxNumDocs = {&ZNO-MAX-PARTS} *
                      INT64(mKolDoc_365)
                      NO-ERROR.
        IF iNumDocs > vMaxNumDocs AND vMaxNumDocs > 0 THEN
            RETURN ERROR "Невозможно создать выписку в электронном виде, " + 
                         "т.к. количество операций за период превышает "   +
                         "допустимое (" + STRING(vMaxNumDocs) + ")".
    END.
END PROCEDURE.

PROCEDURE LinkCust365p.
    DEFINE INPUT PARAMETER iHCust365p AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iPacketID  AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER iRole      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vHBuffer   AS   HANDLE                   NO-UNDO.
    DEFINE VARIABLE vHQuery    AS   HANDLE                   NO-UNDO.
    DEFINE VARIABLE vCustCat   LIKE acct.cust-cat            NO-UNDO.
    DEFINE VARIABLE vCustId    LIKE acct.cust-id             NO-UNDO.
    DEFINE VARIABLE vError     AS   LOGICAL       INITIAL NO NO-UNDO.

    RUN CheckHandle365p(iHCust365p,
                        "Не найдена таблица {&tt-cust365p-name}")
    {&RAISE-ERROR}.
    vHBuffer = iHCust365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer,
                                 "cust-cat",
                                 OUTPUT vCustCat)
        {&RAISE-ERROR}.
        RUN GetIntAttrValue365p(vHBuffer,
                                "cust-id",
                                OUTPUT vCustId)
        {&RAISE-ERROR}.
        RUN PacketCreateLink IN h_pack (iPacketID,
                                        getCustTableName(vCustCat),
                                        STRING(vCustId),
                                        iRole)
        NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            vError = YES.
            LEAVE.
        END.
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    IF vError THEN
        RETURN ERROR "Ошибка создания связи пакета " + STRING(iPacketID) +
                     " с клиентом " + getCustStr(vCustCat, vCustId).
END PROCEDURE.

PROCEDURE CalcPosDelta365p.
    DEFINE INPUT  PARAMETER iAcct       LIKE acct.acct       NO-UNDO.
    DEFINE INPUT  PARAMETER iCurrency   LIKE acct.currency   NO-UNDO.
    DEFINE INPUT  PARAMETER iBegDate    LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT  PARAMETER iEndDate    LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT  PARAMETER iStatusList AS   CHARACTER       NO-UNDO.
    DEFINE OUTPUT PARAMETER oPosDelta   AS   DECIMAL         NO-UNDO.

    DEFINE BUFFER acct     FOR acct.
    DEFINE BUFFER op-date  FOR op-date.
    DEFINE BUFFER op       FOR op.
    DEFINE BUFFER op-entry FOR op-entry.

    DEFINE VARIABLE vStatusList LIKE iStatusList  INITIAL "" NO-UNDO.
    DEFINE VARIABLE vI          AS   INT64                   NO-UNDO.
    DEFINE VARIABLE vStatus     LIKE op.op-status            NO-UNDO.

    oPosDelta = 0.
    DO vI = 1 TO NUM-ENTRIES(iStatusList):
        vStatus = ENTRY(vI, iStatusList).
        IF vStatus < gop-status THEN
            vStatusList = (IF vStatusList = "" THEN ""
                                               ELSE (vStatusList + ",")) +
                          vStatus.
    END.
    IF vStatusList = "" THEN
        RETURN.
    RUN FindAcctCurr365p(BUFFER acct, iAcct, iCurrency) NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN ERROR "Счет ("                                         +
                     GetNullStr(iAcct) + ", " + GetNullStr(iCurrency) +
                     ") не найден".
    FOR EACH op-date WHERE
        op-date.op-date >= iBegDate AND
        op-date.op-date <= iEndDate
    NO-LOCK,
    EACH op-entry WHERE
        op-entry.op-date = op-date.op-date AND
        op-entry.acct-db = acct.acct
        OR
        op-entry.op-date = op-date.op-date AND
        op-entry.acct-cr = acct.acct
    NO-LOCK,
    EACH op OF op-entry WHERE
        CAN-DO(vStatusList, op.op-status)
    NO-LOCK:
        oPosDelta = oPosDelta
                    +
                    (IF {assigned acct.currency} THEN op-entry.amt-cur
                                                 ELSE op-entry.amt-rub)
                    *
                    (IF op-entry.acct-cr = acct.acct THEN 1
                                                     ELSE -1)
                    *
                    (IF acct.side = "П" THEN 1
                                        ELSE -1).
    END.
END PROCEDURE.

PROCEDURE GetBalanceType365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oBalType  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    RUN GetBalanceTypeManual365p(vInfoType,
                                 vRequestKind,
                                 OUTPUT oBalType)
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE GetBalanceTypeManual365p.
    DEFINE INPUT  PARAMETER iInfoType    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iRequestKind AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oBalType     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vBalTypeConfig AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vSyntaxError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vI             AS INT64     NO-UNDO.
    DEFINE VARIABLE vS             AS CHARACTER NO-UNDO.

    IF iRequestKind <> {&REQ-KIND-TICLAS} THEN
        RETURN ERROR "Неверный вид запроса: " + STRING(iRequestKind) +
                     ". Настройка типа остатка возможна "            +
                     "только для справок об остатках".
    ASSIGN
        oBalType       = {&BALANCE-CF}
        vBalTypeConfig =mTipeOst_365
        vSyntaxError   = NO
    .
    DO vI = 1 TO NUM-ENTRIES(vBalTypeConfig, ";"):
        vS = ENTRY(vI, vBalTypeConfig, ";").
        IF NUM-ENTRIES(vS, "=") <> 2 THEN DO:
            vSyntaxError = YES.
            LEAVE.
        END.
        IF TRIM(ENTRY(1, vS, "=")) = iInfoType THEN DO:
            oBalType = TRIM(ENTRY(2, vS, "=")).
            vSyntaxError = NOT CAN-DO({&BALANCE-TYPES}, oBalType).
            LEAVE.
        END.
    END.
    IF vSyntaxError THEN
        RETURN ERROR "Настроечный параметр Настройка_365П.ТипОстатка: " +
                     "синтаксическая ошибка".
END PROCEDURE.

PROCEDURE WarnIfRedSaldo365p.
    DEFINE INPUT        PARAMETER iAcct     LIKE acct.acct       NO-UNDO.
    DEFINE INPUT        PARAMETER iCurrency LIKE acct.currency   NO-UNDO.
    DEFINE INPUT        PARAMETER iBegDate  LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT        PARAMETER iEndDate  LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT        PARAMETER iReqDate  LIKE op-date.op-date NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioPos     AS   DECIMAL         NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vPos          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vOver         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vRedSaldo     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vSideRepr     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vSignRepr     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCheckLimOver AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vS            AS CHARACTER NO-UNDO.

    RUN FindAcctCurr365p(BUFFER acct, iAcct, iCurrency) NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN ERROR "Не найден счет " + QUOTER(iAcct).
    RUN acct-pos IN h_base (acct.acct,
                            acct.currency,
                            iBegDate,
                            iEndDate,
                            gop-status).
    ASSIGN
        vPos      = (IF {assigned acct.currency} THEN sh-val ELSE sh-bal) -
                    (IF mVBO_365 = "Да"
                     THEN
                         GetBlockPositionAll(acct.acct,
                                             acct.currency,
                                             iEndDate)
                     ELSE
                         0)
        vRedSaldo = (IF acct.side = "П" THEN -1 ELSE 1) * vPos < 0
    .
    IF vRedSaldo THEN DO:
        CASE acct.side:
            WHEN "А" THEN
                ASSIGN
                    vSideRepr = "активном "
                    vSignRepr = "Кр"
                .
            WHEN "П" THEN
                ASSIGN
                    vSideRepr = "пассивном "
                    vSignRepr = "Дб"
                .
            OTHERWISE
                ASSIGN
                    vSideRepr = ""
                    vSignRepr = ""
                .
        END.
        ASSIGN
            ioPos         = 0
            vCheckLimOver = (mProvOvrd_365 = "Да")
        .
        IF vCheckLimOver THEN DO:
            RUN getoversm.p (RECID(acct),
                             iReqDate,
                             OUTPUT vOver,
                             OUTPUT vS).
            IF (IF acct.side = "П" THEN 1 ELSE -1) * vPos < vOver THEN
                RETURN.
        END.
        IF acct.close-date <> ? AND acct.close-date < iReqDate THEN
            RETURN.
        RUN Fill-SysMes IN h_tmess ("",
                                    "",
                                    "0",
                                    "Красное сальдо "                          +
                                    TRIM(STRING(ABSOLUTE(vPos),
                                                ">>>,>>>,>>>,>>>,>>>,>>9.99")) +
                                    vSignRepr                                  +
                                    " на "                                     +
                                    vSideRepr                                  +
                                    "счете "                                   +
                                    acct.acct).
    END.
END PROCEDURE.

PROCEDURE FilterAcct365p.
   DEF INPUT PARAMETER iHAcct365p AS HANDLE NO-UNDO. /* Входной HANDLE */

   
   /* Дополнительные переменные */
   DEF VAR vHBuffer     AS   HANDLE    NO-UNDO.
   DEF VAR vHQuery      AS   HANDLE    NO-UNDO.
   DEF VAR oHAcct365p   AS   HANDLE    NO-UNDO.
   DEF VAR vHAcct       AS   HANDLE    NO-UNDO.
   DEF VAR vAcct        LIKE acct.acct NO-UNDO.
   DEF VAR vFeature     AS   INT64     NO-UNDO.
   DEF VAR vUserFilter  AS   CHARACTER NO-UNDO.

   vHBuffer = iHAcct365p:DEFAULT-BUFFER-HANDLE.
   
   /* Создание */
   RUN CreateTTAcct365p(OUTPUT oHAcct365p) NO-ERROR.
   
   /* Запрос */

   CREATE QUERY vHQuery.
   vHQuery:SET-BUFFERS(vHBuffer).
   vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
   vHQuery:QUERY-OPEN().
   REPEAT:
      vHQuery:GET-NEXT().
      IF vHQuery:QUERY-OFF-END THEN
         LEAVE.
      RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
      RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
      RUN AddAcctRecord365p(oHAcct365p, vAcct, vFeature).
   END.
   vHQuery:QUERY-CLOSE().

   /* Фильтрация */
   vUserFilter = TRNSettingValue("ExchSET-TAX", "AcctFilterStrict", "").
   IF {assigned vUserFilter} THEN
      RUN FilterAcctByUserConfig365p(vUserFilter, INPUT-OUTPUT oHAcct365p) NO-ERROR.
   
   /* Отбор */
   vHQuery:SET-BUFFERS(vHBuffer).
   vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " EXCLUSIVE-LOCK").
   vHQuery:QUERY-OPEN().
   DO TRANSACTION:
      REPEAT:
         vHQuery:GET-NEXT().
         IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
         RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
         RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
         IF vFeature EQ {&FEAT-OK} THEN DO:
            RUN FindAcctRecord365p(oHAcct365p, vAcct, OUTPUT vHAcct) NO-ERROR.
            IF NOT VALID-HANDLE(vHAcct) THEN
               RUN SetAttrValue365p(vHBuffer, "feature", STRING({&FEAT-FALSE-365P})) NO-ERROR.
         END.
      END.
   END.
   vHQuery:QUERY-CLOSE().
   DELETE OBJECT vHQuery.
   
   /* Удаление */
   RUN DeleteObject365p(oHAcct365p).
   
END PROCEDURE.

PROCEDURE ListAcct365p.
   DEF INPUT PARAMETER iHAcct365p AS HANDLE NO-UNDO.

   DEF VAR vHBuffer     AS   HANDLE    NO-UNDO.
   DEF VAR vHQuery      AS   HANDLE    NO-UNDO.
   DEF VAR oHAcct365p   AS   HANDLE    NO-UNDO.
   DEF VAR vHAcct       AS   HANDLE    NO-UNDO.
   DEF VAR vAcct        LIKE acct.acct NO-UNDO.
   DEF VAR vFeature     AS   INT64     NO-UNDO.
   DEF VAR vUserFilter  AS   CHARACTER NO-UNDO.

   vHBuffer = iHAcct365p:DEFAULT-BUFFER-HANDLE.
   
   CREATE QUERY vHQuery.
   vHQuery:SET-BUFFERS(vHBuffer).
   vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
   vHQuery:QUERY-OPEN().
   RUN Fill-SysMes IN h_tmess ("", "", "0", "ListAcct365p START").
   REPEAT:
      vHQuery:GET-NEXT().
      IF vHQuery:QUERY-OFF-END THEN
         LEAVE.
      RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) NO-ERROR.
      RUN GetIntAttrValue365p(vHBuffer, "feature", OUTPUT vFeature) NO-ERROR.
      RUN Fill-SysMes IN h_tmess ("", "", "0", SUBSTITUTE("ListAcct365p acct = &1, feature = &2", QUOTER(vAcct), STRING(vFeature))).
   END.
   RUN Fill-SysMes IN h_tmess ("", "", "0", "ListAcct365p END").
   vHQuery:QUERY-CLOSE().
   DELETE OBJECT vHQuery.
END PROCEDURE.

&ENDIF /* CORE365P_PRO_ */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:13:52.607+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='core365p.pro' */
/*prosignHlP6fh4vNcfsSLjJxSYbMg*/