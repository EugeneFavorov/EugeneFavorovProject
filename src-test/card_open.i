/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ТОО "Банковские информационные системы"
     Filename: card_open.i
      Comment:
   Parameters:
         Uses:
      Used by: pp-card.p
      Created:
     Modified:
*/
/*****************************************************************************/
/*                          ОТКРЫТИЕ ДОГОВОРОВ И КАРТ                        */
/*****************************************************************************/
/* Возвращает дату окончания действия ПК */
/* Входные параметры: <Код цен. бумаги (loan.sec-code)>, <Дата начала (loan.open-date)>
** Выходные параметры: <Дата окончания>, <Ошибка (? - если без ошибок; текст ошибки - если была)> */
PROCEDURE GetCardEndDate:
   DEF INPUT PARAM  iSecCode   AS CHAR NO-UNDO.
   DEF INPUT PARAM  iOpenDate  AS DATE NO-UNDO.
   DEF OUTPUT PARAM oEndDate   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oErr       AS CHARACTER NO-UNDO.

   DEF VAR lTerm AS INT64 NO-UNDO.

   ASSIGN oErr = ?.
   FIND FIRST code WHERE code.class EQ "КартыБанка"
                     AND code.code  EQ iSecCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE code THEN DO:
     ASSIGN oErr = "Тип карты [" + iSecCode + "] не найден в классификаторе КартыБанка".
     RETURN.
   END.

   IF code.misc[3] <> "" AND INT64(code.misc[3]) = ? THEN DO:
     ASSIGN oErr = "Некорректно указан срок типа карты [" + iSecCode + "] в классификаторе КартыБанка".
     RETURN.
   END.

   IF code.misc[3] <> "" THEN DO:
     lTerm = INT64(code.misc[3]) NO-ERROR.

     ASSIGN oEndDate = date_correct(MONTH(iOpenDate), lTerm, 31, YEAR(iOpenDate)).
   END.
END PROCEDURE.












/*------------------------------------------------------------------------------
  Purpose: Возвращает маскированный номер карты вне модуля ПК
  Parameters:  немаскированный номер
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetNumCardOnUser RETURNS CHARACTER (INPUT iCardNumber AS CHARACTER ):

   DEFINE VARIABLE vCardNumberMask AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCardNumber     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCnt            AS INT64       NO-UNDO.

   IF work-module EQ "card"  THEN
      vCardNumberMask = "*".
   ELSE
   DO:
      FIND FIRST _user WHERE
         _user._userid EQ USERID("bisquit")
         NO-LOCK NO-ERROR.
      IF AVAIL _user THEN
         vCardNumberMask = GetXattrValue("_user",_user._Userid,"МаскаКартНом").
   END.

   CASE vCardNumberMask:
      WHEN "*" THEN
         vCardNumber = iCardNumber.
      WHEN ""  THEN
         vCardNumber = "".
      OTHERWISE
      DO:
         IF iCardNumber NE "" THEN
         DO vCnt = 1 TO LENGTH(vCardNumberMask):
            IF SUBSTRING(vCardNumberMask, vCnt, 1) = "." THEN
              vCardNumber = vCardNumber + SUBSTRING(iCardNumber, vCnt, 1).
            ELSE
            DO:
              IF SUBSTRING(iCardNumber, vCnt, 1) NE "" THEN
                vCardNumber = vCardNumber + SUBSTRING(vCardNumberMask, vCnt, 1).
            END.
         END.
      END.
   END CASE.

   RETURN vCardNumber.

END FUNCTION.






/*****************************************************************************/
/* Создание карты для договора ПК */
/* Входные параметры: <буфер op-template с найденным шаблоном карты>,
                      <RECID loan к которому привязывается карта>
                      <Дата создания>
** Выходные параметры: <Ошибка (? - если без ошибок; текст ошибки - если была)> */
PROCEDURE CR_Card:
   DEFINE PARAM BUFFER bOpTempl FOR op-template.
   DEFINE INPUT  PARAM iLoanRec  AS RECID             NO-UNDO.
   DEFINE INPUT  PARAM iOpenDate AS DATE              NO-UNDO.
   DEFINE OUTPUT PARAM oCardRec  AS RECID             NO-UNDO.
   DEFINE OUTPUT PARAM oErr      AS CHARACTER INIT ?  NO-UNDO.

   DEF VAR vErr       AS CHARACTER INIT ? NO-UNDO.
   DEF VAR vEndDate   AS DATE             NO-UNDO.
   DEF VAR vSetStatus AS CHARACTER        NO-UNDO.
   DEF VAR vRetVal    AS CHARACTER        NO-UNDO.
   DEF VAR vAcctSurr  AS CHARACTER        NO-UNDO.

   DEFINE BUFFER xloan       FOR loan.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      FIND FIRST loan WHERE RECID(loan) EQ iLoanRec NO-LOCK NO-ERROR.


      RUN GetCardEndDate (loan.trade-sys,
                          iOpenDate,
                          OUTPUT vEndDate,
                          OUTPUT vErr).
      IF vErr NE ? THEN
      DO:
         ASSIGN
            vErr    = {&NoEndDate} + " " + vErr
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      /* Статус карты */
      vSetStatus = GetXAttrValueEx("op-template", bOpTempl.op-kind + "," +
                                      STRING(bOpTempl.op-template), 'set-status', "").
      IF NOT CAN-FIND (FIRST xstatus WHERE xstatus.class-code  EQ "card"
                                       AND xstatus.status-code EQ vSetStatus NO-LOCK) THEN
      DO:
         ASSIGN
            vErr    = {&WrongNewCardStatus}
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      CREATE xloan.
      xloan.cont-code = STRING(GetCounterNextValue("Карты", iOpenDate)).
      IF NOT {assigned xloan.cont-code} THEN
      DO:
         ASSIGN
            vErr    = "Ошибка создания карты. Проверьте настройки счетчика <Карты>.".
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      ASSIGN
         xloan.Class-Code       = "card"
         xloan.parent-contract  = loan.contract
         xloan.parent-cont-code = loan.cont-code
         xloan.doc-num          = ""
         xloan.sec-code         = loan.trade-sys
         xloan.contract         = "card"
         xloan.currency         = loan.currency
         xloan.cust-cat         = loan.cust-cat
         xloan.cust-id          = loan.cust-id
         xloan.conf-date        = iOpenDate
         xloan.open-date        = iOpenDate
         xloan.end-date         = vEndDate
         xloan.deal-type        = loan.deal-type
         xloan.flag-acsept      = loan.loan-work
         xloan.user-o[1]        = loan.sec-code
         xloan.branch-id        = loan.branch-id
         xloan.loan-status      = vSetStatus
         .
      xloan.loan-work = LOGICAL(GetXAttrValueEx("op-template", bOpTempl.op-kind + "," +
                                   STRING(bOpTempl.op-template), 'main-card', ?), "Да/Нет").
      IF xloan.loan-work EQ ? THEN
         xloan.loan-work = NO.
      xloan.user-o[2] = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        'lat-line1',
                                        "").
      xloan.user-o[3] = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        'lat-line2',
                                        "").

      RUN CreateCardSPCLink IN THIS-PROCEDURE (xloan.contract,
                                               xloan.cont-code,
                                                ?,
                                                xloan.loan-work,
                                                Loan.contract,
                                                Loan.cont-code,
                                                iOpenDate,
                                                ?,
                                                OUTPUT vAcctSurr,
                                                OUTPUT vErr).
           IF {assigned vErr} THEN
           DO:
              vRetVal = "-2".
         UNDO MAIN, LEAVE MAIN.
      END.
      IF vErr EQ "" THEN
         vErr = ?.

      oCardRec  = RECID(xloan).
      RELEASE xloan.
   END.

   oErr = vErr.
   IF {assigned vRetVal} THEN
      RETURN vRetVal.

END PROCEDURE. /* CR_Card */

/*****************************************************************************/
/* Обработка шаблона договора ПК (меняет статус) */
/* Входные параметры: <буфер op-template с AVAIL шаблоном карты>,
                      <буфер loan к которому привязывается карта (с AVAIL loan)>
** Выходные параметры: <Ошибка (? - если без ошибок; текст ошибки - если была)> */
PROCEDURE ProcessLoanSettings:
   DEF INPUT PARAM iOpTemplRec AS RECID NO-UNDO.
   DEF PARAM BUFFER bloan FOR loan.
   DEF OUTPUT PARAM oErr AS CHARACTER NO-UNDO.

   DEF VAR vNewStatus AS CHARACTER NO-UNDO.

   FIND FIRST op-template WHERE RECID(op-template) EQ iOpTemplRec NO-LOCK NO-ERROR.
   IF NOT AVAIL op-template THEN
       RETURN.

   ASSIGN
       oErr = ?
       vNewStatus = GetXAttrValueEx("op-template", op-template.op-kind + "," +
                                STRING(op-template.op-template), "set-status", ?)
       .

   IF NOT CAN-FIND (FIRST xstatus WHERE xstatus.class-code EQ bloan.Class-Code
                                    AND xstatus.status-code EQ vNewStatus NO-LOCK) THEN
   DO:
      ASSIGN oErr = {&WrongNewLoanStatus}.
      RETURN.
   END.

   ASSIGN bloan.loan-status = vNewStatus.
END PROCEDURE. /* ProcessLoanSettings */

/*****************************************************************************/
/* Создание условия для договора ПК (если класс будущего условия передан в iCrClass -
   берет его, иначе пытается найти на самом договоре) */
/* Входные параметры: <буфер loan к которому привязывается карта (с AVAIL loan)>,
                      <буфер loan-cond (возвращает созданый loan-cond)>,
                      <дата создания>,
                      <класс условия (если присутствует шаблон стандартных условий,
                        то здесь надо передать класс условий, который им создается)>
** Выходные параметры: <Ошибка (? - если без ошибок; текст ошибки - если была)> */
PROCEDURE CreateLoanCond4CardLoan.
    DEFINE PARAM BUFFER bloan FOR loan.
    DEFINE PARAM BUFFER bloan-cond FOR loan-cond.
    DEFINE INPUT PARAM iDate AS DATE NO-UNDO.
    DEFINE INPUT PARAM iCrClass AS CHAR NO-UNDO.

    DEFINE OUTPUT PARAM oErr AS CHARACTER NO-UNDO.

    DEF VAR vCond AS CHARACTER NO-UNDO.

    ASSIGN oErr = ?.

    ASSIGN vCond = iCrClass.
    IF vCond EQ ? THEN
        ASSIGN vCond = GetXAttrValueEx("loan", loan.contract + "," +
                                     loan.cont-code, "default-cond", ?).

    FIND FIRST Class WHERE class.class-code EQ vCond NO-LOCK NO-ERROR.
    IF NOT AVAIL Class THEN DO:
        RETURN.
    END.

    CREATE bloan-cond.
    ASSIGN
        bloan-cond.class-code = vCond
        bloan-cond.contract = bloan.contract
        bloan-cond.cont-code = bloan.cont-code
        bloan-cond.since = iDate
        .
END PROCEDURE. /* CreateLoanCond4CardLoan */
/*****************************************************************************/

/* Функция генерации номера счета процессинга */
FUNCTION GenNewSPCNumber RETURNS CHAR (BUFFER loan FOR loan,        /* Буфер карты */
                                       INPUT  iOpDate AS DATE ,     /* Опер. дата */
                                       INPUT  iPCCode AS CHARACTER, /* Код процессинга */
                                       OUTPUT oErr    AS INT64,   /* Признак ошибки */
                                       OUTPUT oErrMes AS CHARACTER  /* Сообщение об ошибке */

                                       ):

   DEF VAR vSPCMeth  AS CHARACTER   NO-UNDO. /* Метод генерации */
   DEF VAR vSPCProc  AS CHARACTER   NO-UNDO. /* Процедура генерации */
   DEF VAR vRetVal   AS CHARACTER   NO-UNDO. /* Возвращаемое значение */
   DEF VAR vAcct     AS CHARACTER   NO-UNDO. /* Счет СКС */
   DEF VAR vACurr    AS CHARACTER   NO-UNDO. /* Валюта счета */
   DEF VAR vErrLocal AS INT64     NO-UNDO. /* Локальное значение ошибки */
   DEF VAR vCardType AS CHARACTER   NO-UNDO. /* Тип карты */

   oErr = -1.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* Поиск типа карты в классификаторе "КартыБанка" */
      IF AVAIL loan THEN
      DO:
         vCardType = GetCodeMisc ("КартыБанка",
                                  loan.sec-code,
                                  2).
         IF NOT {assigned vCardType} THEN
         DO:
            oErrMes = "Отстутсвует тип карты <" + loan.sec-code + "> в классификаторе <КартыБанка>.".
            UNDO MAIN, LEAVE MAIN.
         END.
      END.
      ELSE
      DO:
         vCardType = iPCCode.
         IF NOT {assigned vCardType} THEN
         DO:
            oErrMes = "Не указан ни процессинг, ни карта.".
            UNDO MAIN, LEAVE MAIN.
         END.
      END.

      /* Поиск процессинга в классификаторе "Процессинги" */
      FIND FIRST Code WHERE Code.CLASS  EQ "Процессинги"
                        AND Code.CODE   EQ vCardType
         NO-LOCK NO-ERROR.
      IF NOT AVAIL Code THEN
      DO:
         oErrMes = "Не найден классификатор с кодом <" + vCardType + "> указаный в типе карты <" + loan.sec-code + ">.".
         UNDO MAIN, LEAVE MAIN.
      END.

      vSPCMeth = GetXAttrValueEx ("CODE",
                                  CODE.CLASS + "," + CODE.CODE,
                                  "СПЦМетод",
                                  GetXAttrInit("Processing", "СПЦМетод")).

      CASE vSPCMeth:
         WHEN "ПЦ" THEN
            vRetVal = "".
         WHEN "КАРТА" THEN
         DO:
            IF AVAIL loan THEN
               vRetVal = loan.doc-num.
            ELSE
            DO:
               oErrMes = "Карта не определена.".
               UNDO MAIN, LEAVE MAIN.
            END.
         END.
         WHEN "СКС" THEN
         DO:
            IF AVAIL loan THEN
            DO:
               RUN GetRoleAcct IN THIS-PROCEDURE (loan.parent-contract + "," + loan.parent-cont-code,
                                                  iOpDate,
                                                  "SCS",
                                                  loan.currency,
                                                  OUTPUT vAcct,
                                                  OUTPUT vACurr).
               IF {assigned vAcct} THEN
                  vRetVal = vAcct.
               ELSE
               DO:
                  oErrMes = "Не найден счет с ролью СКС.".
                  UNDO MAIN, LEAVE MAIN.
               END.
            END.
            ELSE
            DO:
               oErrMes = "Карта не определена.".
               UNDO MAIN, LEAVE MAIN.
            END.
         END.
         WHEN "БАНК" THEN
         DO:
            vSPCProc = GetXAttrValueEx ("code",
                                        CODE.CLASS + "," + CODE.CODE,
                                        "СПЦПроцедура",
                                        GetXAttrInit("Processing", "СПЦПроцедура")).
            IF NOT SearchPFile(vSPCProc) THEN
            DO:
               oErrMes = "Не найдена процедура генерации номера СПЦ.".
               UNDO MAIN, LEAVE MAIN.
            END.
            ELSE
            DO:
               RUN VALUE(vSPCProc + ".p") (BUFFER loan,
                                           iOpDate,
                                           OUTPUT vRetVal,
                                           OUTPUT vErrLocal,
                                           OUTPUT oErrMes).
               IF vErrLocal LT 0 THEN
                  UNDO MAIN, LEAVE MAIN.
            END.
         END.
         OTHERWISE
         DO:
            oErrMes = IF {assigned vSPCMeth} THEN "Указан не верный метод генерации номера <" + vSPCMeth + ">" ELSE "Не указан метод генерации номера".
            UNDO MAIN, LEAVE MAIN.
         END.
      END CASE.
      oErr = 0.
   END.

   RETURN vRetVal.
END FUNCTION.
/*****************************************************************************/

/* Формирует номер договора по шаблону */
FUNCTION GenNewLoanNumber RETURNS CHAR (INPUT iClassCode AS CHARACTER, /* Класс договора */
                                        INPUT iCardType  AS CHARACTER, /* Тип карты */
                                        INPUT iCurrency  AS CHARACTER, /* Валюта договора */
                                        INPUT iZplLoan   AS CHARACTER, /* Номер зарплатного договора */
                                        INPUT iDate      AS DATE,      /* дата для формирования */
                                        INPUT iCustCode  AS CHARACTER  /* Код клиента */
                                        ):

   DEF VAR vTemplate  AS CHARACTER   NO-UNDO.
   DEF VAR vCounter   AS CHARACTER   NO-UNDO.
   DEF VAR vCounter2  AS CHARACTER   NO-UNDO.
   DEF VAR vPrefix    AS CHARACTER   NO-UNDO.
   DEF VAR vCnt       AS CHARACTER   NO-UNDO.
   DEF VAR vCurr      AS CHARACTER   NO-UNDO.
   DEF VAR vPaySys    AS CHARACTER   NO-UNDO. /* Платежная система */
   DEF VAR vBranch    AS CHARACTER   NO-UNDO.
   DEF VAR vNum       AS INT64       NO-UNDO. /* Текущее значение счётчика */
   DEF VAR vTypeResid AS CHARACTER   NO-UNDO.
   DEF VAR vCriter    AS CHARACTER   NO-UNDO.

   RUN GetClassTemplatePars IN h_jloan (iClassCode,
                                        OUTPUT vTemplate,
                                        OUTPUT vCounter).

   FIND FIRST person WHERE person.person-id = INT64 (iCustCode)  NO-LOCK NO-ERROR .
   IF AVAILABLE person THEN
      IF person.country-id EQ FGetSetting("КодРез","","")
         THEN vTypeResid = "Р" .
         ELSE vTypeResid = "НР" .
   ASSIGN
   vCriter     = SUBSTR(vTypeResid,1,1)
   vPrefix     = GetCodeMisc("КартыБанка", iCardType, 7)
   vPrefix     = IF vPrefix EQ ?
                    THEN ""
                    ELSE vPrefix
   vCounter2   = ?
   .
   XXX: 
      FOR EACH code WHERE code.class EQ "СчПрВал" 
                   AND code.parent EQ "СчПрВал" 
                   AND CAN-DO(ENTRY(1,code.code),vPrefix)
                   AND (NUM-ENTRIES(code.code) LE 1 OR CAN-DO(ENTRY(2,code.code),iCurrency))
                   AND (NUM-ENTRIES(code.code) LE 2 OR CAN-DO(ENTRY(3,code.code),vCriter))
                   NO-LOCK:
                   vCounter2   = code.val.
                   LEAVE XXX. 
      END.
   ASSIGN
      vBranch     = GetXattrValue("_user", USERID("bisquit"), "Отделение")
      vCnt        = IF {assigned vCounter2}
                    THEN vCounter2
                    ELSE GetCodeMisc("КартыБанка", iCardType, 8)
      vPaySys     = REPLACE(GetCodeMisc("КартыБанка", iCardType, 4),",","/")
      vPaySys     = IF vPaySys EQ ?
                    THEN ""
                    ELSE vPaySys
      vCounter    = IF {assigned vCnt}
                    THEN vCnt
                    ELSE vCounter

      vTemplate   = ReplaceBasicTags (vTemplate, vCounter, iDate)
      vNum        = GetCounterCurrentValue (vCounter, iDate)
      vTemplate   = ReplaceTag (vTemplate, "н", STRING(vNum), NO)
      vTemplate   = ReplaceTag (vTemplate, "ф", vBranch  , YES)
      vTemplate   = ReplaceTag (vTemplate, "з", iZplloan , YES)
      vCurr       = GetISOCode (iCurrency)
      vTemplate   = ReplaceTag (vTemplate, "в", vCurr    , YES)
      vTemplate   = ReplaceTag (vTemplate, "п", vPrefix  , NO)
      vTemplate   = ReplaceTag (vTemplate, "у", iCustCode, YES)
      vTemplate   = ReplaceTag (vTemplate, "л", vPaySys  , NO)
      vTemplate   = ReplaceTag (vTemplate, "р", vTypeResid , NO). /* русское р */
      vTemplate   = IF shmode THEN AddFilToLoan(vTemplate,shfilial) ELSE vTemplate.
      .
   RETURN vTemplate.
END FUNCTION. /* GenNewLoanNumber */
/*****************************************************************************/
/*------------------------------------------------------------------------------
  Purpose:     Признак карты  без эмбоссирования     
------------------------------------------------------------------------------*/
FUNCTION fGetCardNoEmb RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.   
   DEFINE BUFFER code FOR code.

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND FIRST code WHERE
             code.Class EQ "КартыБанка"
         AND code.Code  EQ iTypeCard
             NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
      oRetVal = GetXAttrValueEx("code",
                                code.class + "," + code.code,
                                "БезЭмбосс",
                                "Нет") EQ "Да".      
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Признак предоплаченной карты      
------------------------------------------------------------------------------*/
FUNCTION fGetPredoplCard RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.
   
   DEFINE BUFFER code FOR code.

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND FIRST code WHERE
             code.Class EQ "КартыБанка"
         AND code.Code  EQ iTypeCard
             NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
      oRetVal = GetXAttrValueEx("code",
                                code.class + "," + code.code,
                                "Предоплаченные",
                                "Нет") EQ "Да".      
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

FUNCTION fNotPersCard RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):
   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.   

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      oRetVal = fGetCardNoEmb(iTypeCard) OR fGetPredoplCard(iTypeCard). 
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

/* Необходимость создавать СПЦ до создания СКС */
FUNCTION IsNeedSPCnoSCS RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE vRetVal   AS LOGICAL NO-UNDO INIT NO.
   
   DEFINE VARIABLE vPC       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNotPers  AS LOGICAL     NO-UNDO INIT NO.
   
   DEF BUFFER bCode FOR code.
   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      vPC = GetCodeMisc("КартыБанка", iTypeCard,2).

      FIND FIRST bCode WHERE bCode.class EQ "Процессинги"
                         AND bCode.code  EQ vPC
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
         UNDO MAIN, LEAVE MAIN.

      vNotPers = fNotPersCard(iTypeCard).
      
      vRetVal = GetXAttrValueEx("code",
                                bcode.class + "," + bcode.code,
                                "СПЦдоСКС",
                                "") EQ "Да" AND vNotPers.
                               
   END.

   RETURN vRetVal.
END FUNCTION.

PROCEDURE CreateCardSPCLink:
DEFINE INPUT  PARAMETER iCardContract AS CHARACTER   NO-UNDO. /* Суррогат карты: назначение */
DEFINE INPUT  PARAMETER iCardContCode AS CHARACTER   NO-UNDO. /* Суррогат карты: внутренний идентификатор */
DEFINE INPUT  PARAMETER iOldCard      AS CHARACTER   NO-UNDO. /* Номер старой карты (если карта создается с помощью перевыпуска) */
DEFINE INPUT  PARAMETER iMain         AS LOGICAL     NO-UNDO. /* Признак "Главная карта" */
DEFINE INPUT  PARAMETER iLoanContract AS CHARACTER   NO-UNDO. /* Суррогат договора: назначение  */
DEFINE INPUT  PARAMETER iLoanContCode AS CHARACTER   NO-UNDO. /* Суррогат договора: номер  */
DEFINE INPUT  PARAMETER iOpDate       AS DATE        NO-UNDO. /* Дата операции */
DEFINE INPUT  PARAMETER iSPC          AS CHARACTER   NO-UNDO. /* СПЦ, к которому нужно привязать карту */

DEFINE OUTPUT PARAMETER oAcctSurr     AS CHARACTER   NO-UNDO. /* Суррогат СПЦ, к которому привязана карта */
DEFINE OUTPUT PARAMETER oErrMessage   AS CHARACTER   NO-UNDO. /* Сообщение об ошибке, если была ошибка */

   DEFINE VAR vSPCList AS CHARACTER NO-UNDO.
   DEFINE VAR vSPCSurr AS CHARACTER NO-UNDO.
   DEFINE VAR vSPC     AS CHARACTER NO-UNDO.
   DEFINE VAR vSPCCur  AS CHARACTER NO-UNDO.
   DEFINE VAR vi       AS INT64   NO-UNDO.
   DEFINE VAR vOldCardNum  AS CHARACTER NO-UNDO.
   DEFINE VAR vOldCardSurr AS CHARACTER NO-UNDO.

   DEF VAR vAcctSurr AS CHARACTER NO-UNDO.
   DEF VAR vAcct     AS CHARACTER NO-UNDO.
   DEF VAR vACurr    AS CHARACTER NO-UNDO.
   DEF VAR vSPCNum   AS CHARACTER NO-UNDO.
   DEF VAR vErr      AS INT64   NO-UNDO.
   DEF VAR vSPCMeth  AS CHARACTER NO-UNDO.
   DEF VAR vErrMes   AS CHARACTER NO-UNDO.
   DEF VAR vOffSpc   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE mCrStatus AS CHARACTER  NO-UNDO INITIAL "АКТ".

   DEF BUFFER card   FOR loan.
   DEF BUFFER card2  FOR loan.
   DEF BUFFER loan   FOR loan.
   DEF BUFFER acct   FOR acct.
   DEF BUFFER spc    FOR acct.

   MAIN:
   DO ON ERROR  UNDO MAIN, RETRY MAIN
      ON ENDKEY UNDO MAIN, RETRY MAIN:

      IF RETRY THEN DO:
        oErrMessage = "ПРИВЯЗКА СПЦ К КАРТЕ.~nПроизошла системная ошибка: " + ERROR-STATUS:GET-MESSAGE(1).
        LEAVE MAIN.
      END.

      vOffSpc = FGetSetting("СПЦ", "ОтклСпц", ?) NO-ERROR.
      IF vOffSpc EQ "Да" THEN UNDO MAIN, LEAVE MAIN.

      FIND FIRST loan WHERE loan.contract  EQ iLoanContract
                        AND loan.cont-code EQ iLoanContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan THEN DO:
         oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~Не найден договор ПК [&1,&2]", iLoanContract, iLoanContCode).
         UNDO MAIN, LEAVE MAIN.
      END.

      FIND FIRST card WHERE card.contract  EQ iCardContract
                        AND card.cont-code EQ iCardContCode
                      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE card THEN DO:
         oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~Не найденa карта [&1,&2], к которой привязывается СПЦ", iCardContract, iCardContCode).
         UNDO MAIN, LEAVE MAIN.
      END.

      vOldCardNum  = ENTRY(1, iOldCard) NO-ERROR.
      vOldCardSurr = ENTRY(2, iOldCard) NO-ERROR.
      /* 1. Карта является перевыпущенной - просто привязываем к ней СПЦ перевыпускаемой карты */
      IF {assigned vOldCardNum} OR {assigned vOldCardSurr} THEN DO:
         IF {assigned vOldCardNum} THEN DO:
            FIND FIRST card2 WHERE card2.contract         EQ "card"
                               AND card2.doc-num          EQ vOldCardNum
                               /* Дополнительный контроль: карта должна принадлежать тому же договору */
                               AND card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
               NO-LOCK NO-ERROR.
         END.
         ELSE DO: /* Для случая перевыпуска карт без номера... Это надо иначе обрабатывать, но пока так */
            FIND FIRST card2 WHERE card2.contract         EQ "card"
                               AND card2.cont-code        EQ vOldCardSurr
                               /* Дополнительный контроль: карта должна принадлежать тому же договору */
                               AND card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
               NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE card2 THEN DO:
            oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nНевозможно найти перевыпускаемую карту [&1] в договоре [&2,&3]",
                                iOldCard, loan.contract, loan.cont-code).
            UNDO MAIN, LEAVE MAIN.
         END.

         vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "КартаПроц", ";", iOpDate).
         IF NOT {assigned vSPCList} THEN DO:
            oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nК перевыпускаемой карте [&1] не привязано ни одного СПЦ", iOldCard).
            UNDO MAIN, LEAVE MAIN.
         END.

         /* Привязываем все найденные СПЦ перевыпускаемой карты к новой карте */
         DO vi = 1 TO NUM-ENTRIES(vSPCList, ";"):
            vSPCSurr = ENTRY(vi, vSPCList, ";").
            ASSIGN
               vSPC    = ENTRY(1, vSPCSurr)
               vSPCCur = ENTRY(2, vSPCSurr)
            NO-ERROR.

            {find-act.i
               &bact = spc
               &acct = vSPC
               &curr = vSPCCur
            }
            IF NOT AVAILABLE spc THEN DO:
                oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nСПЦ [&1] перевыпускаемой карты [&2] не найден в БД", vSPCSurr, iOldCard).
                UNDO MAIN, LEAVE MAIN.
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
         END.
         LEAVE MAIN. /* все */
      END.

      /* 2. Карта не является перевыпущенной - алгоритм будет посложнее...
         2.1. Если на договоре невозможно несколько СПЦ
               - для первой карты договора создаем СПЦ в валюте договора
               - все остальные карты привязываем к этому СПЦ */
      IF NOT loan.loan-work THEN DO:
            /* Проверяем, существует ли в нашем договоре
               еще хотя бы одна карта, кроме создаваемой */
            FIND FIRST card2 WHERE card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
                               AND card2.contract         EQ "card"
                               AND card2.cont-code        NE card.cont-code
               NO-LOCK NO-ERROR.
            /* 2.1.1 На договоре создаваемая карта не первая.
               Берем СПЦ с найденной старой карты и привязываем его к новой */
            IF AVAILABLE card2 THEN DO:
               vSPCSurr = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "КартаПроц", ";", iOpDate).
               IF NOT {assigned vSPCSurr} THEN DO:
                  oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nПопытка копировать СПЦ с карты [&1,&2] завершена с ошибкой. СПЦ к этой карте не привязан.",
                                      card2.contract, card2.cont-code).
                  UNDO MAIN, LEAVE MAIN.
               END.

               ASSIGN
                  vSPC    = ENTRY(1, ENTRY(1, vSPCSurr, ";"))
                  vSPCCur = ENTRY(2, ENTRY(1, vSPCSurr, ";"))
               NO-ERROR.
               {find-act.i
                   &bact = spc
                   &acct = vSPC
                   &curr = vSPCCur
               }
               IF NOT AVAILABLE spc THEN DO:
                  oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nСПЦ [&1] не найден в БД", vSPCSurr).
                  UNDO MAIN, LEAVE MAIN.
               END.

               RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                     spc.class-code,
                                                     card.contract + "," + card.cont-code,
                                                     loan.contract + "," + loan.cont-code,
                                                     iOpDate,
                                                     ?).
               LEAVE MAIN. /* все */
            END.

            /* 2.1.2. На договоре не найдено других карт, кроме создаваемой.
               Это значит, СПЦ нужно создавать новый
                (!!!!!!! на самом деле перед созданием нужно попробовать найти существующий СПЦ, но пока это опустим) */
            /* настала пора проверять наличие СПЦ */
            vSPCSurr = GetLinks ("card", card.contract + "," + card.cont-code, ?, "КартаПроц", ";", iOpDate).
            IF {assigned vSPCSurr} THEN
            DO:
               ASSIGN
                  vSPC    = ENTRY(1, ENTRY(1, vSPCSurr, ";"))
                  vSPCCur = ENTRY(2, ENTRY(1, vSPCSurr, ";"))
               NO-ERROR.
               {find-act.i
                   &bact = spc
                   &acct = vSPC
                   &curr = vSPCCur
               }               
            END.

            IF NOT AVAILABLE spc THEN
            DO:            
               RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("СПЦ", "СПЦ2Пор", ?),
                                         loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                         OUTPUT vAcct, BUFFER spc,
                                         ?, ?, ?,
                                         "СПЦ",
                                         USERID("bisquit"),
                                         GetUserBranchId(USERID("bisquit")),
                                         YES) NO-ERROR.
               IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
                     oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nОшибка при создании нового СПЦ", ERROR-STATUS:GET-MESSAGE(1)).
                     UNDO MAIN, LEAVE MAIN.
               END.
   
               RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
               spc.acct-status = mCrStatus.
               vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
               UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "НомерСПЦ", iOpDate, vSPCNum, ?).
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
            LEAVE MAIN. /* все */
      END.

      /*  2.2. ЕСЛИ НА ДОГОВОРЕ ВОЗМОЖНО НЕСКОЛЬКО СПЦ - тоже несколько вариантов:

          2.2.1 На карте указан конкретный номер СПЦ.
                В этом случае нужно именно этот конкретный СПЦ и привязать.
                Причем, не имеет значения, существует такой СПЦ или нет
                (если не существует - надо создать).
             !!!!! в идеале искать надо не по ИД СПЦ, а по его реквизиту НомерСПЦ! И из формы карты передавать его же.
             !!!!! и при создании СПЦ указывать ему НомерСПЦ */
      IF {assigned iSPC} THEN DO:
         /* Проверяем, надо ли создавать счет */
         FIND FIRST spc WHERE spc.acct = iSPC NO-LOCK NO-ERROR.
         IF NOT AVAILABLE spc THEN DO:
            /* ... создаем */
            RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("СПЦ", "СПЦ2Пор", ?),
                                      loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                      OUTPUT vAcct, BUFFER spc,
                                      ?, ?, ?,
                                      "СПЦ",
                                      USERID("bisquit"),
                                      GetUserBranchId(USERID("bisquit")),
                                      YES) NO-ERROR.
            IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
                  oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nОшибка при создании нового СПЦ", ERROR-STATUS:GET-MESSAGE(1)).
                  UNDO MAIN, LEAVE MAIN.
            END.

            RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
            spc.acct-status = mCrStatus.
            vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
            UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "НомерСПЦ", iOpDate, vSPCNum, ?).
         END.
         /* Привязываем */
         RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                               spc.class-code,
                                               card.contract + "," + card.cont-code,
                                               loan.contract + "," + loan.cont-code,
                                               iOpDate,
                                               ?).

         LEAVE MAIN. /* все */
      END.

      /* 2.2.2 Конкретного номера СПЦ не указано - самая сложная ситуация

         2.2.2.а Карта дополнительная
                 - ищем СПЦ на главной ДЕЙСТВУЮШЕЙ карте договора, берем с нее СПЦ, привязываем
                 - если найдена действующая главная карта договора, но на ней нет СПЦ -
                   не привязываем СПЦ
                 - если действующая главная карта не найдена - ищем главную карту,
                   действавшую ранее
                 - если главная карта не найдена или на ней нет СПЦ - не привязываем СПЦ
       */
      IF NOT card.loan-work THEN DO:
        /* Ищем СПЦ на главной карте - сначала на действующей главной карте... */
        FIND FIRST card2 WHERE card2.parent-contract  = card.parent-contract
                           AND card2.parent-cont-code = card.parent-cont-code
                           AND card2.contract         EQ "card"
                           AND card2.cont-code        NE card.cont-code
                           AND card2.loan-work
                           AND card2.open-date        LE iOpDate
                           AND (card2.end-date   GE iOpDate OR card2.end-date   EQ ?)
                           AND (card2.close-date GE iOpDate OR card2.close-date EQ ?)
                         NO-LOCK NO-ERROR.
        IF AVAILABLE card2 THEN DO:
           vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "КартаПроц", ";", iOpDate).
           IF NOT {assigned vSPCList} THEN DO:
              oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nДля создаваемой дополнительной карты с номером [&3] [&1,&2]" +
                                  "найдена действующая главная на дату &4, но эта карта не имеет привязки к СПЦ.",
                                  card.contract, card.cont-code, card.doc-num, iOpDate).
              UNDO MAIN, LEAVE MAIN.
           END.
        END.
        /* ... потом на любой из главных, ранее действовавших */
        IF NOT AVAILABLE card2 THEN DO:
           FIND LAST card2 WHERE card2.parent-contract  = card.parent-contract
                             AND card2.parent-cont-code = card.parent-cont-code
                             AND card2.contract         EQ "card"
                             AND card2.cont-code        NE card.cont-code
                             AND card2.loan-work
                             AND card2.open-date        LE iOpDate
                           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE card2 THEN DO:
              oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nДля создаваемой дополнительной карты с номером [&3] [&1,&2]" +
                                  "невозможно найти главную до даты [&4]",
                                  card.contract, card.cont-code, card.doc-num, iOpDate).
              UNDO MAIN, LEAVE MAIN.
           END.
           vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "КартаПроц", ";", iOpDate).
        END.

        IF NOT {assigned vSPCList} THEN DO:
            oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nДля создаваемой дополнительной карты с номером [&3] [&1,&2]" +
                                "найдена главная карта [&7] [&5,&6] до даты [&4], но к ней не привязан СПЦ.",
                                card.contract,  card.cont-code,  card.doc-num, iOpDate,
                                card2.contract, card2.cont-code, card2.doc-num).
            UNDO MAIN, LEAVE MAIN.
        END.

         /* Привязываем все найденные СПЦ главной карты к новой карте */
         DO vi = 1 TO NUM-ENTRIES(vSPCList, ";"):
            vSPCSurr = ENTRY(vi, vSPCList, ";").
            ASSIGN
               vSPC    = ENTRY(1, vSPCSurr)
               vSPCCur = ENTRY(2, vSPCSurr)
            NO-ERROR.

            {find-act.i
               &bact = spc
               &acct = vSPC
               &curr = vSPCCur
            }
            IF NOT AVAILABLE spc THEN DO:
                oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nСПЦ [&1] главной карты с номером [&2] [&3,&4] не найден в БД",
                                    vSPCSurr, card2.doc-num, card2.contract, card2.cont-code).
                UNDO MAIN, LEAVE MAIN.
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
         END.
         LEAVE MAIN. /* все */
      END.

      /* 2.2.2 Конкретного номера СПЦ не указано - самая сложная ситуация

         2.2.2.b Карта основная
                 - если других основных карт нет - создаем СПЦ в валюте договора
                 - есть другие основные карты - выбор предоставляем пользователю
       */
      FIND FIRST card2 WHERE card2.parent-contract  = card.parent-contract
                         AND card2.parent-cont-code = card.parent-cont-code
                         AND card2.contract         EQ "card"
                         AND card2.cont-code        NE card.cont-code
                         AND card2.loan-work
                         AND card2.open-date        LE iOpDate
                         AND (card2.end-date   GE iOpDate OR card2.end-date   EQ ?)
                         AND (card2.close-date GE iOpDate OR card2.close-date EQ ?)
                       NO-LOCK NO-ERROR.
      IF AVAILABLE card2 THEN DO:
         oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nУсловия договора допускают несколько СПЦ, но для карты с номером [&3] [&1,&2]" +
                             "не указано, какой СПЦ привязать",
                             card.contract, card.cont-code, card.doc-num).
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("СПЦ", "СПЦ2Пор", ?),
                                loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                OUTPUT vAcct, BUFFER spc,
                                ?, ?, ?,
                                "СПЦ",
                                USERID("bisquit"),
                                GetUserBranchId(USERID("bisquit")),
                                YES) NO-ERROR.
      IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
            oErrMessage = SUBST("ПРИВЯЗКА СПЦ К КАРТЕ.~nОшибка при создании нового СПЦ", ERROR-STATUS:GET-MESSAGE(1)).
            UNDO MAIN, LEAVE MAIN.
      END.

      RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
      spc.acct-status = mCrStatus.
      vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
      UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "НомерСПЦ", iOpDate, vSPCNum, ?).

      RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                            spc.class-code,
                                            card.contract + "," + card.cont-code,
                                            loan.contract + "," + loan.cont-code,
                                            iOpDate,
                                            ?).
      LEAVE MAIN. /* все */
   END.

   IF AVAIL spc THEN
      oAcctSurr = spc.acct + "," + spc.currency.

   RETURN.
END PROCEDURE. /* CreateCardSPCLink */
/*****************************************************************************/

PROCEDURE CreateSPCLinks.
   DEF INPUT  PARAM iAcctSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iAcctClass AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iCardSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iLoanSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iBegDate   AS DATE        NO-UNDO.
   DEF INPUT  PARAM iEndDate   AS DATE        NO-UNDO.

   DEF VAR vAcctSCS AS CHARACTER NO-UNDO.
   DEF VAR vCurrSCS AS CHARACTER NO-UNDO.
   DEF VAR vNotPers AS LOGICAL   NO-UNDO.


   DEF BUFFER loan FOR loan.
   DEF BUFFER card FOR loan.

   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1, iLoanSurr)
                     AND loan.cont-code EQ ENTRY(2, iLoanSurr)
      NO-LOCK NO-ERROR.
   FIND FIRST card WHERE card.contract  EQ ENTRY(1, iCardSurr)
                     AND card.cont-code EQ ENTRY(2, iCardSurr)
      NO-LOCK NO-ERROR.
   IF AVAIL card AND AVAIL loan THEN
   DO:
      /* Связь с картой */
      RUN CreateLinks IN h_xclass (iAcctClass,
                                   "КартаПроц",
                                   iAcctSurr,
                                   iCardSurr,
                                   iBegDate,
                                   iEndDate,
                                   ?).

      

      /* Связь со счетом СКС, если неперсон. карта в низком статусе, 
      то СКС ещё нет */
      vNotPers = fNotPersCard(card.sec-code).
      IF (vNotPers AND card.loan-status NE fGetSetting("СПЦ","СтатКартБезСПЦ","БЭМ")) OR NOT vNotPers THEN
      DO:
         RUN GetRoleAcct IN THIS-PROCEDURE (iLoanSurr,
                                         iBegDate,
                                         "SCS",
                                         loan.currency,
                                         OUTPUT vAcctSCS,
                                         OUTPUT vCurrSCS).
         RUN CreateLinks IN h_xclass (iAcctClass,
                                      "СчетПроц",
                                      iAcctSurr,
                                      vAcctSCS + "," + vCurrSCS,
                                      iBegDate,
                                      iEndDate,
                                      ?).
      END.
      
      /* Связь с карточным договором */
      UpdateSigns (iAcctClass,
                   iAcctSurr,
                   "loan-surrogate",
                   iLoanSurr,
                   ?).
   END.

END PROCEDURE.
/*****************************************************************************/

PROCEDURE Chk_CardType.
   DEF INPUT  PARAM iLoanSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iLoanType  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oErrMes    AS CHARACTER   NO-UNDO.

   DEF VAR vBankProd    AS CHARACTER   NO-UNDO.
   DEF VAR vValidTypes  AS CHARACTER   NO-UNDO.

   vBankProd = GetXAttrValue ("loan",
                              iLoanSurr,
                              "БанкПродукт").
   IF {assigned vBankProd} THEN
   DO:
      vValidTypes = getTCodeFld ("Val",
                                 "БанкПродукты",
                                 vBankProd,
                                 iDate).
      IF {assigned vValidTypes} THEN
      DO:
         IF NOT CAN-DO(vValidTypes, iLoanType) THEN
            oErrMes = "Тип карты указаный на договоре <" + iLoanSurr
                         + "> не соответствует допустимым <" + vValidTypes
                         + "> в классификаторе кода банковского продукта <"
                         + vBankProd + ">.".
      END.
   END.

END PROCEDURE.
/*****************************************************************************/

PROCEDURE GenNewCardNumber:
DEF INPUT  PARAM iClassCode AS CHARACTER   NO-UNDO.
DEF INPUT  PARAM iBranchID  AS CHARACTER   NO-UNDO.
DEF INPUT  PARAM iDate      AS DATE        NO-UNDO.
DEF OUTPUT PARAM oNumber    AS CHARACTER   NO-UNDO.
DEF OUTPUT PARAM oMessage   AS CHARACTER   NO-UNDO.

   DEF VAR       vTemplate  AS CHARACTER   NO-UNDO.
   DEF VAR       vCounter   AS CHARACTER   NO-UNDO.
   DEF VAR       vCounter2  AS CHARACTER   NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
      /* получаем шаблон номера с класса, счетчик нас не интересует */
      RUN GetClassTemplatePars IN h_jloan (       iClassCode,
                                           OUTPUT vTemplate,
                                           OUTPUT vCounter).

      IF NOT {assigned vTemplate} THEN
      DO:
         oMessage = "Невозможно определить шаблон номера карты на классе <" + iClassCode + ">".
         UNDO MAIN, LEAVE MAIN.
      END.

      /* Пытаемся из классификатора УНКарты получить счетчик по коду подразделения.
      ** Если такой счетчик НЕ найден - берем vCounter.
      ** Если ни один из счетчиков не найден - возвращаем ? */

      vCounter2 = GetCode("УНКарты", iBranchID).

      IF     NOT {assigned vCounter}
         AND NOT {assigned vCounter2} THEN
      DO:
         oMessage = "Невозможно определить ни счетчик для подразделения <" + iBranchID + ">. Ни счетчик на классе <" + iClassCode + ">".
         UNDO MAIN, LEAVE MAIN.
      END.

      ASSIGN
         vTemplate = ReplaceBasicTags (vTemplate, (IF {assigned vCounter2} THEN vCounter2 ELSE vCounter), iDate)
         vTemplate = ReplaceTag (vTemplate, "ф", iBranchID , YES)
         oNumber   = vTemplate
      .
      IF NOT {assigned oNumber} THEN
      DO:
         oMessage = "Невозможно сформировать номер для карты, проверьте, работает ли служба счетчиков".
         UNDO MAIN, LEAVE MAIN.
      END.
   END.

   RETURN.
END PROCEDURE.
/*****************************************************************************/

PROCEDURE CalcCardEndDate:
DEF INPUT  PARAM iCardType  AS CHAR NO-UNDO.
DEF INPUT  PARAM iDateBegin AS DATE NO-UNDO.
DEF OUTPUT PARAM oDateEnd   AS DATE NO-UNDO.
DEF OUTPUT PARAM oMessage   AS CHAR NO-UNDO.

   DEF VAR       mResult    AS CHAR NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE:

      IF iCardType EQ ""
      THEN DO:
         oMessage = "Не указан тип карты".
         LEAVE MAIN.
      END.

      mResult = GetCodeMisc("КартыБанка", iCardType, 3).

      IF mResult EQ ? THEN          /* если код классификатора не найден */
      DO:
         oMessage = "Тип карты [" + mResult + "] не найден в классификаторе КартыБанка".
         LEAVE MAIN.
      END.
      IF mResult          NE "" AND   /* если значение кода классификатора неверно */
         INT64(mResult) EQ ?
      THEN DO:
         oMessage = "Некорректно указан срок типа карты [" + mResult + "] в классификаторе КартыБанка".
         LEAVE MAIN.
      END.
      IF iDateBegin NE ? THEN
         oDateEnd = date_correct(MONTH(iDateBegin), INT64(mResult), 31, YEAR(iDateBegin)).
      ELSE
         oMessage = "Не установлена дата открытия карты.".
   END.
END PROCEDURE.
/*****************************************************************************/
/*==========================================================================*/
PROCEDURE GetSPCMethod:
   DEF INPUT  PARAM iCardContract AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iCardContCode AS CHARACTER   NO-UNDO.
   DEF OUTPUT PARAM oSPCMeth      AS CHARACTER   NO-UNDO.
   DEF OUTPUT PARAM oErrMes       AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vCardType    AS CHARACTER    NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bCard     FOR Loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bcard WHERE bcard.contract  EQ iCardContract
                         AND bcard.cont-code EQ iCardContCode
      NO-LOCK NO-ERROR.

      /* Поиск типа карты в классификаторе "КартыБанка" */
      vCardType = GetCodeMisc ("КартыБанка",
                              bcard.sec-code,
                              2).
      IF NOT {assigned vCardType} THEN
      DO:
         oErrMes = "Отстутсвует тип карты <" + bcard.sec-code + "> в классификаторе <КартыБанка>.".
         UNDO MAIN, LEAVE MAIN.
      END.

        /* Поиск процессинга в классификаторе "Процессинги" */
      FIND FIRST bCode WHERE bCode.CLASS  EQ "Процессинги"
                        AND bCode.CODE   EQ vCardType
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
      DO:
         oErrMes = "Не найден классификатор с кодом <" + vCardType + "> указаный в типе карты <" + bcard.sec-code + ">.".
         UNDO MAIN, LEAVE MAIN.
      END.

      oSPCMeth = GetXAttrValueEx ("CODE",
                                  bCode.CLASS + "," + bCode.CODE,
                                  "СПЦМетод",
                                  GetXAttrInit("Processing", "СПЦМетод")).


   END.
END PROCEDURE. /* GetSPCMethod */

/* Проверка допустимости рекламной акции для карты */
FUNCTION IsAdvAllowed RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                       INPUT iContCode AS CHARACTER,
                                       INPUT iAdv      AS CHARACTER):

   DEF VAR vRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bCode FOR code.
   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST card WHERE card.contract  EQ iContract
                        AND card.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL card THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bCode WHERE bCode.class EQ "РеклАкцииПК"
                         AND bCode.code  EQ iAdv
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
         UNDO MAIN, LEAVE MAIN.

      /* Проверка филиала на карте */
      IF     {assigned bCode.misc[1]}
         AND NOT CAN-DO(bCode.misc[1], card.filial-id) THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bLoan WHERE bLoan.contract  EQ card.parent-contract
                         AND bLoan.cont-code EQ card.parent-cont-code
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.

      /* Провека банковского продукта договора карты */
      IF     {assigned bCode.misc[2]}
         AND NOT CAN-DO(bCode.misc[2], GetXAttrValueEx("loan",
                                                       bLoan.contract + "," + bLoan.cont-code,
                                                       "БанкПродукт",
                                                       "?")) THEN
         UNDO MAIN, LEAVE MAIN.

      /* Проверка типа карты */
      IF     {assigned bCode.misc[3]}
         AND NOT CAN-DO(bCode.misc[3], card.sec-code) THEN
         UNDO MAIN, LEAVE MAIN.

      vRetVal = YES.
   END.

   RETURN vRetVal.
END FUNCTION.

/* Проверка, является ли карта зарплатной */
FUNCTION IsGrCard RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                   INPUT iContCode AS CHARACTER):

   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST card WHERE card.contract  EQ iContract
                        AND card.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL card THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bLoan WHERE bLoan.contract  EQ card.parent-contract
                         AND bLoan.cont-code EQ card.parent-cont-code
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.

      vLinks = GetLinks (bLoan.Class-Code,
                         bLoan.contract + "," + bLoan.cont-code,
                         ?,
                         "ДогПолучателя",
                         ";",
                         card.open-date).
      IF {assigned vLinks} THEN
         vRetVal = YES.
   END.

   RETURN vRetVal.
END FUNCTION.

/* Проверка, является ли договор зарплатным */
FUNCTION IsGrLoan_stnd RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                        INPUT iContCode AS CHARACTER,
                                        INPUT iDate     AS DATE):
   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.
   DEF VAR vOk     AS LOGICAL     NO-UNDO.
   DEF VAR vProc   AS CHARACTER   NO-UNDO.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vLinks = GetLinks (bLoan.Class-Code,
                         bLoan.contract + "," + bLoan.cont-code,
                         "t",
                         "ДогПолучателя",
                         ";",
                         iDate).
      IF {assigned vLinks} THEN
         vRetVal = YES.
   END.
   RETURN vRetVal.
END FUNCTION.

FUNCTION IsGrLoan RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                   INPUT iContCode AS CHARACTER,
                                   INPUT iDate     AS DATE):

   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.
   DEF VAR vOk     AS LOGICAL     NO-UNDO.
   DEF VAR vProc   AS CHARACTER   NO-UNDO.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vProc = Get-Class-Method(bLoan.Class-Code,"IsGrLoan").
      IF {assigned vProc} AND vProc NE "?" THEN
      DO:
         OUTPUT TO "errmsg.txt" KEEP-MESSAGES.
         vOk = NO.
         DO ON STOP UNDO,LEAVE
            ON ERROR UNDO, LEAVE:
            RUN VALUE(vProc)(iContract,iContCode,iDate,OUTPUT vRetVal) NO-ERROR.
            vOk = YES.
         END.
         OUTPUT CLOSE.
         IF vOk <> YES OR ERROR-STATUS:ERROR THEN
         DO:
           MESSAGE
              PROGRAM-NAME(1) SKIP
              "Ошибка вызова метода IsGrLoan для класса " + bLoan.Class-Code SKIP
              ERROR-STATUS:GET-MESSAGE(1)
              VIEW-AS ALERT-BOX.
           vRetVal = ?.
         END.
      END.
      ELSE
         vRetVal = IsGrLoan_stnd(iContract,iContCode,iDate).
   END.

   RETURN vRetVal.
END FUNCTION.

PROCEDURE cpers_isgrrshb:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bLoan FOR loan.
   DEF BUFFER tmp-code FOR tmp-code.
   DEF VAR vLinkId  AS INT64   NO-UNDO.
   DEF VAR vCode AS CHAR NO-UNDO.
   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vCode = GetXattrValue("loan",iContract + "," + iContCode,"банкпродукт").
      FIND LAST tmp-code WHERE tmp-code.class    EQ "БанкПродукты"
                           AND tmp-code.code     EQ vCode
                           AND tmp-code.beg-date <= iDate
      NO-LOCK NO-ERROR.
      RUN GetXLink IN h_xclass ("card-loan-gr",
                                "ДогПолучателя",
                                OUTPUT vLinkId,
                                BUFFER xlink).
      oRetVal =
         (AVAIL tmp-code AND
          GetXattrValue("tmp-code",GetSurrogate("tmp-code",ROWID(tmp-code)),"категория") = "зарплатный" AND
         CAN-FIND(FIRST links  WHERE
                        links.link-id   EQ vLinkId
                    AND links.target-id EQ bLoan.contract + "," + bLoan.cont-code
                    AND links.beg-date < iDate
                    AND links.end-date < iDate
                    AND links.end-date <> ?)
         )
         OR
         IsGrLoan_stnd(iContract,iContCode,iDate).
   END.
END PROCEDURE.

PROCEDURE cpers_isgr:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   oRetVal = IsGrLoan_stnd(iContract,iContCode,iDate).
END PROCEDURE.


PROCEDURE ccorp_isgr:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bLoan FOR loan.
   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
   END.
END PROCEDURE.

PROCEDURE GetInitStatusSPCEx:
   DEFINE PARAMETER BUFFER card           FOR loan.
   DEFINE INPUT-OUTPUT PARAMETER oStatus  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE mInitStatus   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mTypeCard     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mPC           AS CHARACTER  NO-UNDO.

   mInitStatus = oStatus.

   /* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

      IF NOT AVAILABLE card THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.

      mTypeCard = card.sec-code.
      mPC       = GetCodeMisc("КартыБанка",mTypeCard,2).
      oStatus   = GetXAttrValueEx("code","Процессинги," + mPC ,"InitStatusSPC",mInitStatus).
   END.
   /* *************************  End of Main Block  ********************** */
END PROCEDURE.
