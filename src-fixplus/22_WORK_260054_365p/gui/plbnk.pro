
/* +++ plbnk.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: plbnk.pro
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Процедуры бибилиотеки pp-plbnk.p
     Modified: 
*/
/*===============================================================================================*/
/*=== Заполняем глобальные переменные обмена по 365П ============================================*/
PROCEDURE SetEnv365p:
   DEF INPUT  PARAM iInfType AS  CHAR  NO-UNDO. /* Тип информационного запроса                   */
   DEF INPUT  PARAM iAllFil  AS  LOG   NO-UNDO. /* Поиск идет по всем филиалам: Да/Нет           */

   /* Запросы к внешней базе идут только в случае ЗНО */
   IF {assigned iInfType} THEN 
      mZaprosNO = CAN-DO({&INFO-TYPE-ZNO}, iInfType).

   /* Поиск счетов во всех филиалах */
   IF iAllFil NE ? THEN
      mAllFil  = iAllFil.

END PROCEDURE. /* SetEnv365p */

/*===============================================================================================*/


/*===============================================================================================*/
/*=== Поиск счета во внешней системе ============================================================*/
PROCEDURE FindAcct:
   DEF INPUT  PARAM iAcct AS  CHAR  NO-UNDO. /* Номер счета                                      */
   DEF OUTPUT PARAM TABLE FOR ttExtAcct.     /* Таблица по найденным счетам                      */

   IF     {assigned mAcctFnd} 
      AND {assigned iAcct}
   THEN
   DO:
      /* Запускаем поиск счета во внешней таблице */
      RUN VALUE(mAcctFnd)(iAcct, 
                          mAllFil, 
                          OUTPUT TABLE ttExtAcct) 
                         {&RAISE-ERROR}.
     
   END. /* IF {assigned mAcctFndExt} */

END PROCEDURE. /* FindAcct */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Поиск счета с учетом реквизитов клиента ===================================================*/
PROCEDURE FindAcctEx:
   DEF INPUT  PARAM iHReqInfo  AS  HANDLE NO-UNDO. /* Указатель на информационную часть запроса  */
   DEF INPUT  PARAM iAcct      AS  CHAR   NO-UNDO. /* Валюта счета                               */
   DEF OUTPUT PARAM TABLE      FOR ttExtAcct.      /* Таблица по найденным счетам                */

DEF VAR vInn    AS  CHAR  NO-UNDO. /* ИНН клиента                                                */
DEF VAR vKpp    AS  CHAR  NO-UNDO. /* КПП клиента                                                */
DEF VAR vName   AS  CHAR  NO-UNDO. /* Наименование клиента                                       */

   vInn  = iHReqInfo::innnp$  NO-ERROR.
   vKpp  = iHReqInfo::kppnp$  NO-ERROR.
   vName = iHReqInfo::naimnp$ NO-ERROR.
   ASSIGN
      vInn  = fStrNvl(vInn , "")
      vKpp  = fStrNvl(vKpp , "")
      vName = fStrNvl(vName, "")
   .
   IF     {assigned mAcctFndEx} 
      AND {assigned iAcct}
   THEN
   DO:
      /* Запускаем поиск счета во внешней таблице */
      RUN VALUE(mAcctFndEx)(vInn, 
                            vKpp, 
                            vName,
                            iAcct,
                            mAllFil,
                            OUTPUT TABLE ttExtAcct) 
                           {&RAISE-ERROR}.
   END. /* IF {assigned mAcctFndExtEx} */

END PROCEDURE. /* FindAcctEx */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Получаем остатки и обороты по счету =======================================================*/
PROCEDURE AcctPos:
   DEF INPUT  PARAM iBegDate AS  DATE NO-UNDO.  /* Дата начала выгрузки                         */
   DEF INPUT  PARAM iEndDate AS  DATE NO-UNDO.  /* Дата окончания выгрузки                      */
   DEF PARAM BUFFER bExtAcct FOR ttExtAcct.     /* Cчет                                         */
   DEF OUTPUT PARAM oAmtIn   AS  DEC   NO-UNDO. /* Входящий остаток                             */
   DEF OUTPUT PARAM oAmbDb   AS  DEC   NO-UNDO. /* Обороты по дебету                            */
   DEF OUTPUT PARAM oAmtCr   AS  DEC   NO-UNDO. /* Обороты по кредиту                           */
   DEF OUTPUT PARAM oAmt     AS  DEC   NO-UNDO. /* Исходящий остаток                            */

   IF {assigned mAcctPos} THEN
   DO:
      /* Получаем обороты и остатки  */
      RUN VALUE(mAcctPos)(iBegDate,
                          iEndDate,
                          mAllFil,
                          INPUT TABLE bExtAcct,
                          OUTPUT oAmtIn,
                          OUTPUT oAmbDb,
                          OUTPUT oAmtCr,
                          OUTPUT oAmt) 
                         {&RAISE-ERROR}.
   END. /* IF {assigned mAcctPos} THEN */

END PROCEDURE. /* AcctPos */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Переходник для получения остатков и обороты по счету через буффер таблицы acct ============*/
PROCEDURE AcctPosBuf:
   DEF INPUT  PARAM  iBegDate AS  DATE NO-UNDO.  /* Дата начала выгрузки                         */
   DEF INPUT  PARAM  iEndDate AS  DATE NO-UNDO.  /* Дата окончания выгрузки                      */
   DEF PARAM BUFFER  acct     FOR acct.          /* Cчет                                         */
   DEF OUTPUT PARAM  oAmtIn   AS  DEC   NO-UNDO. /* Входящий остаток                             */
   DEF OUTPUT PARAM  oAmbDb   AS  DEC   NO-UNDO. /* Обороты по дебету                            */
   DEF OUTPUT PARAM  oAmtCr   AS  DEC   NO-UNDO. /* Обороты по кредиту                           */
   DEF OUTPUT PARAM  oAmt     AS  DEC   NO-UNDO. /* Исходящий остаток                            */

DEF BUFFER bExtAcct FOR ttExtAcct.

   IF AVAIL(acct) THEN
   DO:

      CREATE bExtAcct.
      /* Копируем данные из таблицы в наше результат */
      BUFFER-COPY acct TO bExtAcct.
      /* Сохраняем изменения в таблицу */
      VALIDATE bExtAcct NO-ERROR.

      /* Получаем обороты и остатки  */
      RUN AcctPos IN THIS-PROCEDURE(iBegDate,
                                    iEndDate,
                                    BUFFER bExtAcct,
                                    OUTPUT oAmtIn,
                                    OUTPUT oAmbDb,
                                    OUTPUT oAmtCr,
                                    OUTPUT oAmt) 
                                   {&RAISE-ERROR}.
      DELETE bExtAcct.
   END. /* IF AVAIL(acct) THEN */

END PROCEDURE. /* AcctPosBuf */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Заполнение блока операция по счету ========================================================*/
PROCEDURE FillOp:
   DEF INPUT  PARAM iHRepOp  AS  HANDLE NO-UNDO. /* Указатель на таблицу с данными              */
   DEF INPUT  PARAM iBegDate AS  DATE   NO-UNDO. /* Дата начала выгрузки                        */ 
   DEF INPUT  PARAM iEndDate AS  DATE   NO-UNDO. /* Дата окончания выгрузки                     */
   DEF INPUT  PARAM iUpID    AS  INT64  NO-UNDO. /* ID связанного счета                         */
   DEF PARAM BUFFER bExtAcct FOR ttExtAcct.      /* Cчет                                        */
   DEF OUTPUT PARAM oNumDocs AS  INT64  NO-UNDO. /* Количество созданный документов             */

   IF {assigned mFillOp} THEN
   DO:
      /* Заполняем данные из внешней системы */
      RUN VALUE(mFillOp)(iHRepOp,
                         iBegDate,
                         iEndDate,
                         iUpID,
                         mAllFil,
                         INPUT TABLE bExtAcct,
                         OUTPUT oNumDocs) 
                        NO-ERROR.
      IF    oNumDocs EQ ? 
         OR ERROR-STATUS:ERROR NE NO
      THEN
      DO:
         oNumDocs = 0.
         RETURN ERROR {&RETURN_VALUE}.
      END.
   END. /* IF {assigned mFillOp} THEN */

END PROCEDURE. /* FillOp */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Заполнение блока операций по счету через буфер счета ======================================*/
PROCEDURE FillOpBuf:
   DEF INPUT  PARAM iHRepOp   AS  HANDLE NO-UNDO. /* Указатель на таблицу с данными              */
   DEF INPUT  PARAM iBegDate  AS  DATE   NO-UNDO. /* Дата начала выгрузки                        */ 
   DEF INPUT  PARAM iEndDate  AS  DATE   NO-UNDO. /* Дата окончания выгрузки                     */
   DEF INPUT  PARAM iUpID     AS  INT64  NO-UNDO. /* ID связанного счета                         */
   DEF PARAM BUFFER acct     FOR acct.           /* Cчет                                        */
   DEF OUTPUT PARAM oNumDocs  AS  INT64  NO-UNDO. /* Количество созданный документов             */

DEF BUFFER bExtAcct FOR ttExtAcct.

   IF AVAIL(acct) THEN
   DO:
      CREATE bExtAcct.
      /* Копируем данные из таблицы в наше результат */
      BUFFER-COPY acct TO bExtAcct.
      /* Сохраняем изменения в таблицу */
      VALIDATE bExtAcct.

      /* Заполняем данные из внешней системы */
      RUN FillOp IN THIS-PROCEDURE(iHRepOp,
                                   iBegDate,
                                   iEndDate,
                                   iUpID,
                                   BUFFER bExtAcct,
                                   OUTPUT oNumDocs) 
                                  {&RAISE-ERROR}.
      DELETE bExtAcct.

   END. /* IF AVAIL(acct) THEN */

END PROCEDURE. /* FillOpBuf */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== Проверка счета для 365П: принадлежность клиенту/даты открытия =============================*/
PROCEDURE ExtValidateAcct:
   DEF INPUT        PARAM iAcct      LIKE acct.acct NO-UNDO. /* Номер счета                      */
   DEF INPUT        PARAM iHCust365p AS   HANDLE    NO-UNDO. /* Указатель на данные клиента      */
   DEF INPUT        PARAM iBegDate   AS   DATE      NO-UNDO. /* Начало периода                   */
   DEF INPUT        PARAM iEndDate   AS   DATE      NO-UNDO. /* Окончание периода                */
   DEF INPUT        PARAM iStrict    AS   LOG       NO-UNDO.
   DEF INPUT-OUTPUT PARAM oFeature   AS   INT64     NO-UNDO.

/* Процедура создана на основе ValidateAcct365p из core365p.pro */

DEF VAR vHCustBuffer365p AS HANDLE NO-UNDO.
DEF VAR vTmpStr          AS CHAR   NO-UNDO.

DEF BUFFER bExtAcct FOR ttExtAcct.

   /* Запускаем поиск счета во внешней таблице */
   RUN FindAcct IN THIS-PROCEDURE(iAcct, 
                                  OUTPUT TABLE bExtAcct) 
                                 {&RAISE-ERROR}.

   FIND FIRST bExtAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL(bExtAcct) THEN 
      RETURN. /* Счет не найден и во внешней системе */

   /* Если найдено несколько счетов переберм их все  */
   lFndAcct:
   FOR EACH bExtAcct NO-LOCK:
      RUN FindCustRecord365p(iHCust365p,
                             bExtAcct.cust-cat,
                             bExtAcct.cust-id,
                             OUTPUT vHCustBuffer365p)
                            NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          oFeature = {&FEAT-ERROR}.
          RETURN.
      END.
      IF VALID-HANDLE(vHCustBuffer365p) THEN 
         LEAVE lFndAcct.
  END. /* lFndAcct: FOR EACH bExtAcct NO-LOCK: */
  IF NOT VALID-HANDLE(vHCustBuffer365p) THEN DO:
     /* Счет не соответсвует клиенту */
      oFeature = {&FEAT-WRONG-CUST}.
      RETURN.
  END.
  IF iBegDate            <> ? AND
     bExtAcct.close-date <> ? AND
     (iStrict OR
      bExtAcct.close-date <= iBegDate)
  THEN DO:
      /* Счет закрыт на дату начала отчета */
      oFeature = {&FEAT-CLOSED}.
      RETURN.
  END.
  IF iEndDate           <> ? AND
     bExtAcct.open-date <> ? AND
     bExtAcct.open-date >  iEndDate
  THEN DO:
      /* Счет еще не открыт на дату окончания отчета */
      oFeature = {&FEAT-NOT-OPEN}.
      RETURN.
  END.
  oFeature = {&FEAT-OK}.

END PROCEDURE. /* ExtValidateAcct */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Аналог FillZNOAcctPart365p из zno365p2.pro ================================================*/
PROCEDURE FillZNOExtAcct:
   DEF INPUT        PARAM iRequestKind AS INT64  NO-UNDO. 
   DEF INPUT        PARAM iHRepAcct    AS HANDLE NO-UNDO.  
   DEF INPUT        PARAM iHReqInfo    AS HANDLE NO-UNDO.
   DEF INPUT        PARAM iHRepOp      AS HANDLE NO-UNDO.
   DEF INPUT        PARAM iAcct        AS CHAR   NO-UNDO.
   DEF INPUT        PARAM iReqDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iBegDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iEndDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iSPacketID   AS INT64  NO-UNDO.
   DEF INPUT-OUTPUT PARAM pAcctCnt     AS INT64  NO-UNDO.
   DEF OUTPUT       PARAM oNumDocs     AS INT64  NO-UNDO.

DEF BUFFER bExtAcct FOR ttExtAcct.

DEF VAR vHRepAcctBuffer AS  HANDLE  NO-UNDO.
DEF VAR vHRepOpBuffer   AS  HANDLE  NO-UNDO.
DEF VAR vAcct           AS  CHAR    NO-UNDO.
DEF VAR vCurrency       AS  CHAR    NO-UNDO.
DEF VAR vNCCode         AS  CHAR    NO-UNDO.
DEF VAR vNumYears       AS  INT64   NO-UNDO.
DEF VAR vAmtIn          AS  DEC     NO-UNDO.
DEF VAR vAmt            AS  DEC     NO-UNDO.
DEF VAR vAmtDb          AS  DEC     NO-UNDO.
DEF VAR vAmtCr          AS  DEC     NO-UNDO.
DEF VAR vTmpDate        AS  DATE    NO-UNDO.

   ASSIGN
      oNumDocs        = 0
      vNCCode         = fGetSetting("КодНацВал0406007", "", "")
      vHRepAcctBuffer = iHRepAcct:DEFAULT-BUFFER-HANDLE
      vHRepOpBuffer   = iHRepOp:DEFAULT-BUFFER-HANDLE
   .

   /* Поиск счета по номеру и реквизитам клиента */
   RUN FindAcctEx IN THIS-PROCEDURE(iHReqInfo,
                                    iAcct,
                                    OUTPUT TABLE bExtAcct)
                                   {&RAISE-ERROR}.
   FIND FIRST bExtAcct NO-LOCK NO-ERROR.

   IF NOT AVAIL(bExtAcct) THEN
      RETURN. /* Счет во внешней системе не обнаружен */

   vHRepAcctBuffer:BUFFER-CREATE().
   ASSIGN
      vAcct     = bExtAcct.number
      vCurrency = ( IF {assigned bExtAcct.currency} THEN bExtAcct.currency
                                                   ELSE vNCCode)
      pAcctCnt  = pAcctCnt + 1
   .

   RUN SetAttrValue365p(vHRepAcctBuffer, "НомСч", vAcct)         {&RAISE-ERROR}.
   RUN SetAttrValue365p(vHRepAcctBuffer, "ВалСч", vCurrency)     {&RAISE-ERROR}.
   RUN SetAttrValue365p(vHRepAcctBuffer, "ID", STRING(pAcctCnt)) {&RAISE-ERROR}.

   IF iRequestKind <> {&REQ-KIND-TICLAM} THEN 
   DO:
      RUN SetAttrValue365p(vHRepAcctBuffer,"ВидСч", bExtAcct.contract) {&RAISE-ERROR}.

   END. /* IF iRequestKind <> {&REQ-KIND-TICLAM} THEN  */

   IF iRequestKind <> {&REQ-KIND-TICLAC} THEN 
   DO:
      /* Получаем остатки/обороты из внешней системы */
      RUN AcctPos IN THIS-PROCEDURE(iBegDate,
                                    iEndDate,
                                    BUFFER bExtAcct,
                                    OUTPUT vAmtIn,
                                    OUTPUT vAmtDb,
                                    OUTPUT vAmtCr,
                                    OUTPUT vAmt) 
                                   {&RAISE-ERROR}.

   END. /* IF iRequestKind <> {&REQ-KIND-TICLAC} THEN */

   CASE iRequestKind:
      WHEN {&REQ-KIND-TICLAC} THEN 
      DO:
          RUN SetAttrValue365p(vHRepAcctBuffer, "ДатаОткрСч", date2str(bExtAcct.open-date))
              {&RAISE-ERROR}.
          IF bExtAcct.close-date <> ? THEN 
          DO:
             RUN SetAttrValue365p(vHRepAcctBuffer, "ДатаЗакрСч", date2str(bExtAcct.close-date))
                 {&RAISE-ERROR}.

          END. /* IF acct.close-date <> ? THEN */
      END. /* WHEN {&REQ-KIND-TICLAC} THEN */

      WHEN {&REQ-KIND-TICLAS} THEN 
      DO:
         RUN SetAttrValue365p(vHRepAcctBuffer, "Остаток", STRING(ABSOLUTE(vAmt)))
             {&RAISE-ERROR}.
      END. /* WHEN {&REQ-KIND-TICLAS} THEN  */

      WHEN {&REQ-KIND-TICLAM} THEN 
      DO:
         IF FGetSetting("Настройка_365П", "ПровАрхив","") = "Да" THEN 
         DO:

            vNumYears = INT64(FGetSetting("Настройка_365П", "АрхивСрок","0")) NO-ERROR. 
            vTmpDate  = DATE(MONTH(iBegDate), 
                             DAY  (iBegDate), 
                             YEAR (iBegDate) + vNumYears).

            IF     vNumYears <> ? 
               AND vNumYears > 0 
               AND iReqDate  > vTmpDaTe 
            THEN 
            DO:
               RUN TAXConfirmCreate (iSPacketID, "35", "Запрошенный период "      + 
                                     "выписки превышает архивный срок хранения. " + 
                                     "Выписка предоставлена за последние "        + 
                                     STRING(vNumYears) + "лет", NOW) NO-ERROR. 
               iBegDate = DATE(MONTH(iReqDate),
                               DAY(iReqDate), 
                               YEAR(iReqDate) - vNumYears).

            END. /* IF     vNumYears <> ? */
         END. /* IF FGetSetting("Настройка_365П", "ПровАрхив","") = "Да" THEN  */

         RUN SetAttrValue365p(vHRepAcctBuffer,"ДатаНачала", date2str(iBegDate))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"ДатаКонца",  date2str(iEndDate))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"ОстатНач",   STRING(ABS(vAmtIn)))
             {&RAISE-ERROR}.                               
         RUN SetAttrValue365p(vHRepAcctBuffer,"ОстатКон",   STRING(ABS(vAmt)))
             {&RAISE-ERROR}.                               
         RUN SetAttrValue365p(vHRepAcctBuffer,"СуммаДеб",   STRING(ABS(vAmtDb)))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"СуммаКред",  STRING(ABS(vAmtCr)))
             {&RAISE-ERROR}.

         /* Заполняем данные из внешней системы */
         RUN FillOp IN THIS-PROCEDURE (vHRepOpBuffer,
                                       iBegDate,
                                       iEndDate,
                                       pAcctCnt,
                                       BUFFER bExtAcct,
                                       OUTPUT oNumDocs) 
                                      {&RAISE-ERROR}.
      END. /* WHEN {&REQ-KIND-TICLAM} THEN DO: */
   END CASE. /* iRequestKind: */

END PROCEDURE. /* FillZNOExtAcct */

/*===============================================================================================*/

/* --- plbnk.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am --- */
