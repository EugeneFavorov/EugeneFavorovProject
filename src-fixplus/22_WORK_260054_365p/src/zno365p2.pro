&IF DEFINED(ZNO365P2_PRO_) = 0 &THEN

&GLOBAL-DEFINE ZNO365P2_PRO_ YES

&GLOBAL-DEFINE FILE_sword_p  YES

{globals.i}
{sh-defs.i}
{intrface.get count}
{intrface.get db2l}
{intrface.get instrum}
{intrface.get op}
{intrface.get strng}
{intrface.get separate}
{pp-uni.var}
{pp-uni.prg}

{core365p.pro}

PROCEDURE FillZNOServicePart365p.
    DEFINE INPUT PARAMETER iHRepServ AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iHReqInfo AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iGUID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iNumParts AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER iPart     AS INT64     NO-UNDO.

    DEFINE VARIABLE vInfoType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vRequestKind AS INT64     NO-UNDO.
    DEFINE VARIABLE vRequestType AS INT64     NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    RUN CheckHandle365p(iHRepServ,
                        "Не найдена служебная часть ответа")
    {&RAISE-ERROR}.
    FIND FIRST _user WHERE
        _user._userid = USERID("bisquit")
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE _user THEN
        RETURN ERROR "Невозможно определить текущего пользователя".
    RUN SetAttrValue365p(iHRepServ,
                         "idfawil$",
                         mINN          +
                         "**"                                +
                         fst(mKPP) +
                         date2id(TODAY)                      +
                         iGUID)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "tipinf$",
                         getReplyInfoType(vRequestKind))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "versprog$",
                         "БИСКВИТ " + version)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "telotpr$",
                         getThisUserXAttrValue("Телефон"))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "dolwznotpr$",
                         getThisUserXAttrValue("Должность"))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "famotpr$",
                         TRIM(ENTRY(1, _user._user-name, " ")))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepServ,
                         "koldok$",
                         STRING(iNumParts))
    {&RAISE-ERROR}.
    IF iNumParts > 1 THEN DO:
        RUN SetAttrValue365p(iHRepServ,
                             "nomwcasti$",
                             STRING(iPart))
        {&RAISE-ERROR}.
    END.
    RUN SetAttrValue365p(iHRepServ,
                         "versform$",
                         {&ZNO-FORMAT-VERSION})
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE FillZNOInfoPart365p.
    DEFINE INPUT PARAMETER iHRepInfo     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iHReqInfo     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iHCust365p    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iGUID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iUserName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iUserPosition AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vInfoType     AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vRequestKind  AS   INT64         NO-UNDO.
    DEFINE VARIABLE vRequestType  AS   INT64         NO-UNDO.
    DEFINE VARIABLE vFilialNum    AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustCat      LIKE acct.cust-cat NO-UNDO.
    DEFINE VARIABLE vCustId       LIKE acct.cust-id  NO-UNDO.
    DEFINE VARIABLE vCustINN      AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustKPP      AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustKPPReq   AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustName     AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCustNameAttr AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCounterName  AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vCounterAttr  AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vReqNumAttr   AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vRepNumAttr   AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vReqDateAttr  AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vRepDateAttr  AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vTmpStr       AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vTmpDate      AS   DATE          NO-UNDO.

    RUN GetBaseRequestData365p(iHReqInfo,
                               OUTPUT vInfoType,
                               OUTPUT vRequestKind,
                               OUTPUT vRequestType)
    {&RAISE-ERROR}.
    RUN CheckHandle365p(iHRepInfo,
                        "Не найдена информационная часть ответа")
    {&RAISE-ERROR}.
    RUN ExtractOneCust365p(iHCust365p,
                           OUTPUT vCustCat,
                           OUTPUT vCustId)
    {&RAISE-ERROR}.
    RUN GetThisFilialNum(OUTPUT vFilialNum).
    IF vRequestKind = {&REQ-KIND-TICLAM} THEN
        ASSIGN
            vCounterName = "Выписки365"
            vCounterAttr = "nomvypis$"
        .
    ELSE
        ASSIGN
            vCounterName = "Справки365"
            vCounterAttr = "nomsprav$"
        .
    RUN SetAttrValue365p(iHRepInfo,
                         "iddok$",
                         iGUID)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         vCounterAttr,
                         STRING(GetCounterNextValue(vCounterName, TODAY)))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "innko$",
                         mINN)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "kppko$",
                         fst(mKPP))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "bik$",
                         mMFO)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "naimko$",
                         mBank)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "nomf$",
                         vFilialNum)
    {&RAISE-ERROR}.
    ASSIGN
        vRepNumAttr  = "Ном"
        vRepDateAttr = "Дата"
    .
    IF vInfoType = {&INFO-TYPE-RPO} OR vInfoType = {&INFO-TYPE-ROO} THEN
        ASSIGN
            vRepNumAttr  = vRepNumAttr + "Реш"
            vRepDateAttr = vRepDateAttr + "Реш"
        .
    ELSE
        ASSIGN
            vRepNumAttr  = vRepNumAttr + "Запр"
            vRepDateAttr = vRepDateAttr + "Запр"
        .
    ASSIGN
        vReqNumAttr  = vRepNumAttr
        vReqDateAttr = vRepDateAttr
    .
    IF vInfoType = {&INFO-TYPE-RPO} THEN
        ASSIGN
            vReqNumAttr  = vReqNumAttr + "Пр"
            vReqDateAttr = vReqDateAttr + "Пр"
        .
    ELSE IF vInfoType = {&INFO-TYPE-ROO} THEN
        ASSIGN
            vReqNumAttr  = vReqNumAttr + "От"
            vReqDateAttr = vReqDateAttr + "От"
        .
    RUN GetCharAttrValue365p(iHReqInfo,
                             vReqNumAttr,
                             OUTPUT vTmpStr)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         GetMangledName(vRepNumAttr),
                         vTmpStr)
    {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "КодНО",
                             OUTPUT vTmpStr)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "kodno$",
                         vTmpStr)
    {&RAISE-ERROR}.
    RUN GetCustINN365p(iHReqInfo,
                       OUTPUT vCustINN)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "innnp$",
                         vCustINN)
    {&RAISE-ERROR}.
    RUN GetDateAttrValue365p(iHReqInfo,
                             vReqDateAttr,
                             OUTPUT vTmpDate)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         GetMangledName(vRepDateAttr),
                         date2str(vTmpDate))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "datasprav$",
                         date2str(vTmpDate))
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "datasoob$",
                         date2str(TODAY))
    {&RAISE-ERROR}.
    RUN FixCustName365p(INPUT-OUTPUT iUserName).
    RUN SetAttrValue365p(iHRepInfo,
                         "fioprb$",
                         iUserName)
    {&RAISE-ERROR}.
    RUN SetAttrValue365p(iHRepInfo,
                         "dolwznprb$",
                         iUserPosition)
    {&RAISE-ERROR}.
    RUN GetRealCustName365p(vCustCat,
                            vCustId,
                            OUTPUT vCustName,
                            INPUT-OUTPUT vCustINN).
    vCustNameAttr = IF isPersonINN(vCustINN, vCustCat)
                    THEN "fioip$"
                    ELSE "naimnp$".
    IF isPersonINN(vCustINN, vCustCat) THEN
        RUN FixCustName365p(INPUT-OUTPUT vCustName).
    RUN SetAttrValue365p(iHRepInfo,
                         vCustNameAttr,
                         vCustName)
    {&RAISE-ERROR}.
    IF isPersonINN(vCustINN, vCustCat) <> YES THEN DO:
        RUN GetCharAttrValue365p(iHReqInfo,
                                 "КППНП",
                                 OUTPUT vCustKPPReq)
        NO-ERROR.
        vCustKPP = GetXAttrValueEx(getCustTableName(vCustCat),
                                   STRING(vCustId),
                                   "КПП",
                                   "").
        RUN SetAttrValue365p(iHRepInfo,
                             "kppnp$",
                             IF CAN-DO(vCustKPP, vCustKPPReq) THEN
                                 vCustKPPReq
                             ELSE
                                 fst(vCustKPP))
        {&RAISE-ERROR}.
    END.
END PROCEDURE.

PROCEDURE FillZNOAcctPart365p.                       
    DEFINE INPUT  PARAMETER iHRepAcct  AS HANDLE NO-UNDO.  
    DEFINE INPUT  PARAMETER iHRepOp    AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER iHReqInfo  AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER iHAcct365p AS HANDLE NO-UNDO. 
    DEFINE OUTPUT PARAMETER oNumDocs   AS INT64  NO-UNDO.
                                                                  
    DEFINE BUFFER acct FOR acct.          
    DEFINE BUFFER oacct FOR acct.          

    &IF DEFINED(ExtBase) &THEN
    DEFINE BUFFER acct-ext FOR acct.

    DEFINE VARIABLE vExtBufRID      AS   ROWID         NO-UNDO.
    DEFINE VARIABLE vExtAmtIn       AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vExtAmt         AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vExtAmtDb       AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vExtAmtCr       AS   DECIMAL       NO-UNDO.
    &ENDIF
    DEFINE VARIABLE vInfoType       AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vRequestKind    AS   INT64         NO-UNDO.
    DEFINE VARIABLE vRequestType    AS   INT64         NO-UNDO.
    DEFINE VARIABLE vReqDate        AS   DATE          NO-UNDO.
    DEFINE VARIABLE vBegDate        AS   DATE          NO-UNDO.
    DEFINE VARIABLE vEndDate        AS   DATE          NO-UNDO.
    DEFINE VARIABLE vHQuery         AS   HANDLE        NO-UNDO.
    DEFINE VARIABLE vHBuffer        AS   HANDLE        NO-UNDO.
    DEFINE VARIABLE vHRepAcctBuffer AS   HANDLE        NO-UNDO.
    DEFINE VARIABLE vHRepOpBuffer   AS   HANDLE        NO-UNDO.
    DEFINE VARIABLE vAcct           LIKE acct.acct     NO-UNDO.
    DEFINE VARIABLE vCurrency       LIKE acct.currency NO-UNDO.
    DEFINE VARIABLE vNCCode         LIKE acct.currency NO-UNDO.
    DEFINE VARIABLE vAcctCnt        AS   INT64         NO-UNDO.
    DEFINE VARIABLE vAmtIn          AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmt            AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtDb          AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtCr          AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtInO         AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtO           AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtDbO         AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vAmtCrO         AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vNumDocs        AS   INT64         NO-UNDO.
    DEFINE VARIABLE vDeltaAmt       AS   DECIMAL       NO-UNDO.
    DEFINE VARIABLE vLastCloseDate  AS   DATE          NO-UNDO.
    DEFINE VARIABLE vBalanceType    AS   CHARACTER     NO-UNDO.
    DEFINE VARIABLE vNumYears       AS   INT64         NO-UNDO.
    DEFINE VARIABLE vDataPereh      AS   DATE          NO-UNDO.

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
    vLastCloseDate = Get_CloseDate_Cat("b").
    IF vLastCloseDate = ? THEN
        vLastCloseDate = {&BQ-MIN-DATE}.
    vLastCloseDate = MINIMUM(vLastCloseDate, vEndDate).
    RUN CheckHandle365p(iHAcct365p,
                        "Не найдена таблица {&tt-acct365p-name}")
    {&RAISE-ERROR}.
    RUN CheckHandle365p(iHRepAcct,
                        "Не найден блок счетов ответа")
    {&RAISE-ERROR}.
    IF vRequestKind = {&REQ-KIND-TICLAM} THEN DO:
        RUN CheckHandle365p(iHRepOp,
                            "Не найден блок операций ответа")
        {&RAISE-ERROR}.
        vHRepOpBuffer = iHRepOp:DEFAULT-BUFFER-HANDLE.
    END.
    ASSIGN
        vHBuffer        = iHAcct365p:DEFAULT-BUFFER-HANDLE
        vHRepAcctBuffer = iHRepAcct:DEFAULT-BUFFER-HANDLE
        vNCCode         = mNCCode 
        oNumDocs        = 0
    .
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(vHBuffer).
    vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
    vHQuery:QUERY-OPEN().
    REPEAT:
        vHQuery:GET-NEXT().
        IF vHQuery:QUERY-OFF-END THEN
            LEAVE.
        RUN GetCharAttrValue365p(vHBuffer, "acct", OUTPUT vAcct) {&RAISE-ERROR}.
        RUN FindAcct365p(BUFFER acct, vAcct) NO-ERROR.
        IF NOT AVAILABLE acct THEN
        DO:
           &IF DEFINED(ExtBase) &THEN
           IF RunExtQuery(vBegDate) THEN
           DO:
              RUN FillZNOExtAcct IN h_plbnk(vRequestKind,
                                            iHRepAcct,
                                            iHReqInfo,
                                            iHRepOp,
                                            vAcct,
                                            vReqDate,
                                            vBegDate,
                                            vEndDate ,
                                            mSuperPacketID,
                                            INPUT-OUTPUT vAcctCnt,
                                            OUTPUT vNumDocs)
                                           {&RAISE-ERROR}.
              oNumDocs = oNumDocs + vNumDocs.
           END.
           &ENDIF
            NEXT.
        END.
        RUN FindAcctOld365p(BUFFER acct, BUFFER oacct) NO-ERROR.
        vDataPereh = IF AVAILABLE oacct 
                     THEN DATE(FGetSettingMF("Настройка_365П", "ДатаПерехода", "", oacct.filial-id, NO))
                     ELSE ?.          
        &IF DEFINED(ExtBase) &THEN
        vExtBufRID = IF AVAIL(oacct) THEN ROWID(oacct) 
                                     ELSE ROWID(acct).
        FIND FIRST acct-ext WHERE ROWID(acct-ext) EQ vExtBufRID 
                            NO-LOCK NO-ERROR.
        &ENDIF
        vHRepAcctBuffer:BUFFER-CREATE().
        ASSIGN
            vAcct     = getAcctNumber(acct.acct)
            vCurrency = (IF {assigned acct.currency} THEN acct.currency
                                                     ELSE vNCCode)
            vAcctCnt  = vAcctCnt + 1
        .
        RUN SetAttrValue365p(vHRepAcctBuffer,
                             "nomswc$",
                             vAcct)
        {&RAISE-ERROR}.
        RUN SetAttrValue365p(vHRepAcctBuffer,
                             "valswc$",
                             vCurrency)
        {&RAISE-ERROR}.
        RUN SetAttrValue365p(vHRepAcctBuffer,
                             "ID",
                             STRING(vAcctCnt))
        {&RAISE-ERROR}.
        IF vRequestKind <> {&REQ-KIND-TICLAM} THEN DO:
            RUN SetAttrValue365p(vHRepAcctBuffer,
                                 "vidswc$",
                                 acct.contract)
            {&RAISE-ERROR}.
        END.
        IF vRequestKind <> {&REQ-KIND-TICLAC} THEN DO:

           IF AVAILABLE oacct 
           THEN
           DO:
               RUN acct-pos IN h_base (oacct.acct,
                                       oacct.currency,
                                       vBegDate,
                                       vEndDate,
                                       gop-status).
               ASSIGN
                  vAmtInO = IF {assigned oacct.currency} THEN sh-in-val
                                                         ELSE sh-in-bal
                  vAmtO   = IF {assigned oacct.currency} THEN sh-val
                                                         ELSE sh-bal
                  vAmtDbO = IF {assigned oacct.currency} THEN sh-vdb
                                                         ELSE sh-db
                  vAmtCrO = IF {assigned oacct.currency} THEN sh-vcr
                                                         ELSE sh-cr
               .
           END.
           ELSE ASSIGN
                   vAmtInO = 0
                   vAmtO   = 0
                   vAmtDbO = 0
                   vAmtCrO = 0
                .

            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    vBegDate,
                                    vEndDate,
                                    gop-status).
            ASSIGN
                vAmtIn = IF {assigned acct.currency} THEN sh-in-val
                                                     ELSE sh-in-bal
                vAmt   = IF {assigned acct.currency} THEN sh-val
                                                     ELSE sh-bal
                vAmtDb = IF {assigned acct.currency} THEN sh-vdb
                                                     ELSE sh-db
                vAmtCr = IF {assigned acct.currency} THEN sh-vcr
                                                     ELSE sh-cr
            .
            &IF DEFINED(ExtBase) &THEN
            IF RunExtQuery(vBegDate) THEN
            DO:
               /* Заполняем данные из внешней системы */
               IF AVAIL(acct-ext) THEN 
               DO:
                  RUN AcctPosBuf IN h_plbnk(vBegDate,
                                            vEndDate,
                                            BUFFER acct-ext,
                                            OUTPUT vExtAmtIn,
                                            OUTPUT vExtAmtDb,
                                            OUTPUT vExtAmtCr,
                                            OUTPUT vExtAmt) 
                                           {&RAISE-ERROR}.
                  ASSIGN
                     vAmtIn = vExtAmtIn
                     vAmtDb = vAmtDb + vExtAmtDb
                     vAmtCr = vAmtCr + vExtAmtCr
                     vAmt   = vExtAmt             WHEN RunExtQuery(vEndDate)
                  .
               END. /* IF AVAIL(acct-ext) THEN  */
            END. /* IF IsLessDateNR(vBegDate) THEN */
            &ENDIF
        END.    
        CASE vRequestKind:
            WHEN {&REQ-KIND-TICLAC} THEN DO:
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                     "dataotkrswc$",
                                     date2str(acct.open-date))
                {&RAISE-ERROR}.
                IF acct.close-date <> ? THEN DO:
                    RUN SetAttrValue365p(vHRepAcctBuffer,
                                        "datazakrswc$",
                                         date2str(acct.close-date))
                    {&RAISE-ERROR}.
                END.
            END.
            WHEN {&REQ-KIND-TICLAS} THEN DO:
                RUN GetBalanceType365p(iHReqInfo,
                                       OUTPUT vBalanceType)
                {&RAISE-ERROR}.

                IF AVAILABLE oacct          AND 
                   vDataPereh NE ?          AND
                   vEndDate   LT vDataPereh 
                THEN
                DO:
                    RUN CalcPosDelta365p(oacct.acct,
                                         oacct.currency,
                                         vLastCloseDate,
                                         vEndDate,
                                         mDSD_365,
                                         OUTPUT vDeltaAmt)
                    {&RAISE-ERROR}.

                    vAmtO = ABSOLUTE((IF vBalanceType = {&BALANCE-BF} THEN vAmtInO
                                                                      ELSE vAmtO)
                                     -
                                    (IF mVBO_365 = "Да"
                                     THEN
                                        GetBlockPositionAll(oacct.acct,
                                                            oacct.currency,
                                                            vEndDate)
                                     ELSE
                                        0))
                           +
                           vDeltaAmt.

                    RUN WarnIfRedSaldo365p(oacct.acct,
                                           oacct.currency,
                                           vBegDate,
                                           vEndDate,
                                           vReqDate,
                                           INPUT-OUTPUT vAmtO)
                    {&RAISE-ERROR}.

                    RUN SetAttrValue365p(vHRepAcctBuffer,
                                         "ostatok$",
                                         STRING(ABSOLUTE(vAmtO)))
                    {&RAISE-ERROR}.
                END.
                ELSE 
                DO:
                RUN CalcPosDelta365p(acct.acct,
                                     acct.currency,
                                     vLastCloseDate,
                                     vEndDate,
                                         mDSD_365,
                                     OUTPUT vDeltaAmt)
                {&RAISE-ERROR}.
                vAmt = ABSOLUTE((IF vBalanceType = {&BALANCE-BF} THEN vAmtIn
                                                                 ELSE vAmt)
                                -
                                (IF mVBO_365 = "Да"
                                 THEN
                                     GetBlockPositionAll(acct.acct,
                                                         acct.currency,
                                                         vEndDate)
                                 ELSE
                                     0))
                       +
                       vDeltaAmt.
                RUN WarnIfRedSaldo365p(acct.acct,
                                       acct.currency,
                                       vBegDate,
                                       vEndDate,
                                       vReqDate,
                                       INPUT-OUTPUT vAmt)
                {&RAISE-ERROR}.
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                         "ostatok$",
                                     STRING(ABSOLUTE(vAmt)))
                {&RAISE-ERROR}.
            END.
            END.
            WHEN {&REQ-KIND-TICLAM} THEN DO:

               IF mPrArch_365 = "Да" THEN DO:

                     vNumYears = INT64(mArchSr_365) NO-ERROR. 
                  IF vNumYears <> ? AND vNumYears > 0 AND
                     vReqDate > DATE(MONTH(vBegDate),DAY(vBegDate),YEAR(vBegDate) + vNumYears) THEN DO:
                            
                     RUN TAXConfirmCreate (mSuperPacketID, "35", "Запрошенный период " + 
                                           "выписки превышает архивный срок хранения. " + 
                                           "Выписка предоставлена за последние " + 
                                           STRING(vNumYears) + "лет", NOW) NO-ERROR. 
                     vBegDate = DATE(MONTH(vReqDate),DAY(vReqDate),YEAR(vReqDate) - vNumYears).
                  END.
               END.

                RUN SetAttrValue365p(vHRepAcctBuffer,
                                    "datanawcala$",
                                     date2str(vBegDate))
                {&RAISE-ERROR}.
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                    "datakonca$",
                                     date2str(vEndDate))
                {&RAISE-ERROR}.

               IF AVAILABLE oacct          AND 
                  vDataPereh NE ?          AND
                  vBegDate   LE vDataPereh THEN
               DO:
                    RUN SetAttrValue365p(vHRepAcctBuffer,
                                         "ostatnawc$",
                                         STRING(ABSOLUTE(vAmtInO)))
                    {&RAISE-ERROR}.
               END.
               ELSE 
               DO:
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                     "ostatnawc$",
                                     STRING(ABSOLUTE(vAmtIn)))
                {&RAISE-ERROR}.
               END.
               IF AVAILABLE oacct          AND 
                  vDataPereh NE ?          AND
                  vEndDate   LT vDataPereh THEN 
               DO: 
                    RUN SetAttrValue365p(vHRepAcctBuffer,
                                         "ostatkon$",
                                         STRING(ABSOLUTE(vAmtO)))
                    {&RAISE-ERROR}.
               END.
               ELSE 
               DO:
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                     "ostatkon$",
                                     STRING(ABSOLUTE(vAmt)))
                {&RAISE-ERROR}.
               END.
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                     "summadeb$",
                                    STRING(ABSOLUTE(vAmtDb + vAmtDbO)))
                {&RAISE-ERROR}.
                RUN SetAttrValue365p(vHRepAcctBuffer,
                                     "summakred$",
                                    STRING(ABSOLUTE(vAmtCr + vAmtCrO)))
                {&RAISE-ERROR}.
               &IF DEFINED(ExtBase) &THEN
               IF RunExtQuery(vBegDate) THEN
               DO:
                  /* Заполняем данные из внешней системы */
                  RUN FillOpBuf IN h_plbnk(vHRepOpBuffer,
                                           vBegDate,
                                           vEndDate,
                                           vAcctCnt,
                                           BUFFER acct-ext,
                                           OUTPUT vNumDocs) 
                                          {&RAISE-ERROR}.
                  IF vNumDocs EQ ? THEN
                     vNumDocs = 0.
                  oNumDocs = oNumDocs + vNumDocs.
               END. /* IF RunExtQuery(vBegDate) THEN */
               &ENDIF
               IF AVAILABLE oacct THEN
               DO:
                  RUN FillZNOOpPart365p(vHRepOpBuffer,
                                        vBegDate,
                                        vEndDate,
                                        vAcctCnt,
                                        BUFFER oacct,
                                        OUTPUT vNumDocs).
                  oNumDocs = oNumDocs + vNumDocs.
               END.
                RUN FillZNOOpPart365p(vHRepOpBuffer,
                                      vBegDate,
                                      vEndDate,
                                      vAcctCnt,
                                      BUFFER acct,
                                      OUTPUT vNumDocs).
                oNumDocs = oNumDocs + vNumDocs.
            END.
        END.
        vHRepAcctBuffer:BUFFER-RELEASE().
    END.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
END PROCEDURE.

PROCEDURE FillZNOOpPart365p.
    DEFINE INPUT  PARAMETER iHRepOp   AS  HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER iBegDate  AS  DATE   NO-UNDO.
    DEFINE INPUT  PARAMETER iEndDate  AS  DATE   NO-UNDO.
    DEFINE INPUT  PARAMETER iUpID     AS  INT64  NO-UNDO.
    DEFINE PARAMETER BUFFER acct      FOR acct.
    DEFINE OUTPUT PARAMETER oNumDocs  AS  INT64  NO-UNDO.

    DEFINE BUFFER acct-corr FOR acct.
    DEFINE BUFFER cust-role FOR cust-role.

    DEFINE VARIABLE vDocDate  LIKE op.doc-date      NO-UNDO.
    DEFINE VARIABLE vDetails  LIKE op.details       NO-UNDO.
    DEFINE VARIABLE vDigital  LIKE doc-type.digital NO-UNDO.
    DEFINE VARIABLE vINN      AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vKPP      AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vPMFO     AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vPName    AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vPAcct    AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vPCAcct   AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vPRKC     AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vCatCorr  AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vAddress  AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vCustCode AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vType     AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vInternal AS   LOGICAL          NO-UNDO.
    DEFINE VARIABLE vCash     AS   LOGICAL          NO-UNDO.
    DEFINE VARIABLE vDecTmp   AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE vAmtDb    AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE vAmtCr    AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE vNoReval  AS   LOGICAL          NO-UNDO.

    ASSIGN
       oNumDocs = 0
       vNoReval = TRNSettingValue("ExchSET-TAX", "NoRevaluation", "NO") = "YES"
    .
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
        op.op-status >= gop-status
    NO-LOCK
    BY op.op-date:
        IF vNoReval             AND
           op-entry.amt-cur = 0 AND
           {assigned op-entry.currency}
        THEN
           NEXT.
        iHRepOp:BUFFER-CREATE().
        oNumDocs = oNumDocs + 1.
        vDocDate = op.doc-date.
        IF vDocDate = ? THEN DO:
            vDocDate = op.op-date.
            RUN Fill-SysMes IN h_tmess ("",
                                        "",
                                        "0",
                                        "У документа №"                +
                                        (IF {assigned op.doc-num} THEN
                                             op.doc-num
                                         ELSE
                                             "<не определен>")         +
                                        " от " + date2str(op.op-date)  +
                                        " отсутствует дата документа." +
                                        " Использована дата опердня.").
        END.
        RUN GetDocTypeDigital(op.doc-type, OUTPUT vDigital).
        RUN GetOpDetails365p(BUFFER op, OUTPUT vDetails).
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "dataoper$",
                                    date2str(op-entry.op-date))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "viddok$",
                                    vDigital)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "nomdok$",
                                    op.doc-num)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "datadok$",
                                    date2str(vDocDate))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "naznpl$",
                                    vDetails)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "ID",
                                    STRING(oNumDocs))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "UpID",
                                    STRING(iUpID))
        {&RAISE-ERROR}.
        ASSIGN
            vINN    = ""
            vKPP    = ""
            vPName  = ""
            vPAcct  = ""
            vPRKC   = ""
            vPCAcct = ""
            vPMFO   = ""
            vType   = ""
            vDecTmp = (IF {assigned acct.currency} THEN op-entry.amt-cur
                                                   ELSE op-entry.amt-rub)
        . 
        IF op-entry.currency <> acct.currency AND {assigned acct.currency} THEN
            vDecTmp = CurToCur("Учетный",
                               op-entry.currency,
                               acct.currency,
                               op-entry.op-date,
                               IF {assigned op-entry.currency} THEN
                                   op-entry.amt-cur
                               ELSE
                                   op-entry.amt-rub).
        {empty Info-Store}
        RUN Collection-Info.
        ASSIGN
            vCatCorr = ""
            vAmtDb   = 0
            vAmtCr   = 0
        .
        IF acct.acct = op-entry.acct-cr THEN DO:
            ASSIGN
                vAmtCr = vDecTmp
                vKPP   = GetXAttrValueEx("op",
                                         Surrogate(BUFFER op:HANDLE),
                                         "Kpp-send",
                                         "")
            .
            {find-act.i &bact = acct-corr
                        &acct = op-entry.acct-db
                        &curr = op-entry.currency}
            IF AVAILABLE acct-corr THEN
                vCatCorr = acct-corr.cust-cat.
            RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
                        "ПП",
                        OUTPUT vPName,
                        OUTPUT vPAcct,
                        OUTPUT vPRKC,
                        OUTPUT vPCAcct,
                        OUTPUT vPMFO).
            FIND FIRST Info-Store WHERE
                Info-Store.info-id = "Плательщик"
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Info-Store THEN
                FIND FIRST Info-Store WHERE
                    Info-Store.info-id = "Дебет"
                NO-LOCK NO-ERROR.
        END.
        ELSE IF acct.acct = op-entry.acct-db THEN DO:
            ASSIGN
                vAmtDb = vDecTmp
                vKPP   = GetXAttrValueEx("op",
                                         Surrogate(BUFFER op:HANDLE),
                                         "Kpp-rec",
                                         "")
            .
            {find-act.i &bact = acct-corr
                        &acct = op-entry.acct-cr
                        &curr = op-entry.currency}
            IF AVAILABLE acct-corr THEN
                vCatCorr = acct-corr.cust-cat.
            RUN for-rec("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
                        "ПП",
                        OUTPUT vPName,
                        OUTPUT vPAcct,
                        OUTPUT vPRKC,
                        OUTPUT vPCAcct,
                        OUTPUT vPMFO).
            FIND FIRST Info-Store WHERE
                Info-Store.info-id = "Получатель"
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Info-Store THEN
                FIND FIRST Info-Store WHERE
                    Info-Store.info-id = "Кредит"
                NO-LOCK NO-ERROR.
        END.
        IF AVAILABLE Info-Store AND {assigned Info-Store.inn} THEN
            vINN = Info-Store.inn.
        FIND FIRST cust-role WHERE
            cust-role.file-name  = "op"                        AND
            cust-role.surrogate  = Surrogate(BUFFER op:HANDLE) AND
            cust-role.class-code = (IF acct.acct = op-entry.acct-db THEN
                                        "Benef-"
                                    ELSE
                                        "Order-") + "cust"
        NO-LOCK NO-ERROR.
        IF AVAILABLE cust-role THEN
            ASSIGN
                vPName = cust-role.cust-name
                vINN   = cust-role.inn
                vKPP   = cust-role.kpp
            .
        IF vKPP = ? OR vKPP = "0" THEN
            vKPP = "".
        vCash = AVAILABLE acct-corr AND
                (CAN-DO(mNaznSchKas,
                       acct-corr.contract) = YES).
        IF vCash THEN
            ASSIGN
                vPAcct = acct-corr.acct
                vPName = GetCliName(acct.cust-cat,
                                    STRING(acct.cust-id),
                                    OUTPUT vAddress,
                                    OUTPUT vINN,
                                    OUTPUT vKPP,
                                    INPUT-OUTPUT vType,
                                    OUTPUT vCustCode,
                                    OUTPUT vPCAcct)
            .
        vInternal = NOT vCash            AND
                    vCatCorr = "В"       AND
                    (NOT AVAILABLE acct-corr
                     OR
                     AVAILABLE acct-corr AND
                     NOT CAN-DO(mNaznSchMBR,
                                acct-corr.contract)).
        IF vInternal                  AND
           NOT AVAILABLE cust-role    AND
           NOT {assigned op.name-ben} AND
           GetXAttrValueEx("op",
                           Surrogate(BUFFER op:HANDLE),
                           "name-" +
                           (IF acct.acct = op-entry.acct-db THEN "rec"
                                                            ELSE "send"),
                           "") = ""
        THEN
            ASSIGN
                vINN   = FGetSettingMF("ИНН", "", "", op.filial-id, NO)
                vKPP   = fst(FGetSettingMF("БанкКПП", "", "", op.filial-id, NO))
                vPName = TRIM(FGetSettingMF("Банк", "", "", op.filial-id, NO))
            .
        ELSE
            RUN TrimZNOCustName365p(INPUT-OUTPUT vPName, vINN, vKPP).
        IF isPersonINN(vINN, acct.cust-cat) THEN
            vKPP = "".
        IF vInternal THEN DO:
            RUN SetAttrValueVerbose365p(iHRepOp,
                                        "naimpp$",
                                        vPName)
            {&RAISE-ERROR}.
        END.
        ELSE DO:
            IF isPersonINN(vINN, acct.cust-cat) THEN DO:
                RUN SetAttrValueVerbose365p(iHRepOp,
                                            "fiopp$",
                                            vPName)
                {&RAISE-ERROR}.
                RUN GetCharAttrValue365p(iHRepOp,
                                         "ФИОПП",
                                         OUTPUT vPName)
                {&RAISE-ERROR}.
                RUN FixCustName365p(INPUT-OUTPUT vPName).
                RUN SetAttrValue365p(iHRepOp,
                                     "fiopp$",
                                     vPName)
                {&RAISE-ERROR}.
            END.
            ELSE DO:
                RUN SetAttrValueVerbose365p(iHRepOp,
                                            "naimpp$",
                                            vPName)
                {&RAISE-ERROR}.
            END.
        END.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "nomkorswc$",
                                    getAcctNumber(vPCAcct))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "naimbp$",
                                    vPRKC)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "bikbp$",
                                    vPMFO)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "innpp$",
                                    vINN)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "kpppp$",
                                    vKPP)
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "nomswcpp$",
                                    getAcctNumber(vPAcct))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "debet$",
                                    STRING(vAmtDb))
        {&RAISE-ERROR}.
        RUN SetAttrValueVerbose365p(iHRepOp,
                                    "kredit$",
                                    STRING(vAmtCr))
        {&RAISE-ERROR}.
        iHRepOp:BUFFER-RELEASE().
    END.
END PROCEDURE.

&ENDIF /* ZNO365P2_PRO_ */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:14:28.818+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='zno365p2.pro' */
/*prosignA9ng7ftDpYCNeoQzODEsUg*/