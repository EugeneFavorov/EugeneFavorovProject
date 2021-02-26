/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pays.pro
      Comment: Библиотека процедур обмена с платежными системами онлайн
   Parameters: нет
         Uses:
      Used BY: pp-pays.p
      Created: 23.04.2015 KMBIS TT:0236973 Миграция. Прием платежей QIWI, Рапида,Уралсиб, КиберПлат
     Modified: 
*/

/*===============================================================================================*/
/*=== Данные подразделения-получателя ===========================================================*/
PROCEDURE CorrBranch:
   DEF INPUT  PARAM iBic     AS  CHAR  NO-UNDO. /* БИК банка         */
   DEF INPUT  PARAM iPSys    AS  CHAR  NO-UNDO. /* Платежная система */
   DEF INPUT  PARAM iDate    AS  DATE  NO-UNDO. /* Дата расчета      */
   DEF OUTPUT PARAM oFilial  AS  CHAR  NO-UNDO. /* Код подразделения */
   DEF OUTPUT PARAM oAcctCor AS  CHAR  NO-UNDO. /* Корр.счет         */
   DEF OUTPUT PARAM oAcctTr  AS  CHAR  NO-UNDO. /* Транзитный счет   */

DEF BUFFER bBankBic  FOR Banks-code.
DEF BUFFER bBankCor  FOR banks-corr.
DEF BUFFER bBranch   FOR Branch.

   /* Ищем Подразделение получателя  */
   FOR FIRST bBankBic WHERE bBankBic.bank-code-type = "МФО-9"
                        AND bBankBic.bank-code      = iBic
                      NO-LOCK,
      FIRST bBankCor WHERE bBankCor.bank-corr EQ bBankBic.bank-id
                     NO-LOCK,
      FIRST bBranch WHERE bBranch.bank-id = bBankBic.bank-id
                    NO-LOCK:
      ASSIGN
         oFilial   = bBranch.branch-id
         oAcctCor  = bBankCor.corr-acct
         oAcctTr   = GetRefVal("МФР", iDate, SUBST("&1,&2", iPSys, iBic))
      .

   END. /* FOR FIRST bBankBic WHERE bBankBic.bank-code-type = "МФО-9" */

END PROCEDURE. /* CorrBranch */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Создаем узел XML со значением =============================================================*/
PROCEDURE CreateXmlNode:
   DEF INPUT PARAM iDoc       AS  HANDLE  NO-UNDO.
   DEF INPUT PARAM iNode      AS  HANDLE  NO-UNDO.
   DEF INPUT PARAM iNodeName  AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iNodeVal   AS  CHAR    NO-UNDO.

DEF VAR vNode   AS  HANDLE  NO-UNDO.
DEF VAR vValue  AS  HANDLE  NO-UNDO.

   CREATE X-NODEREF vValue.
   CREATE X-NODEREF vNode.

   iDoc:CREATE-NODE(vNode, iNodeName, "ELEMENT"). /* Новый тег     */
   iDoc:CREATE-NODE(vValue, ""      , "TEXT").    /* Значение тега */

   vValue:NODE-VALUE = iNodeVal.

   iNode:APPEND-CHILD(vNode).
   vNode:APPEND-CHILD(vValue).

END PROCEDURE. /* CreateXmlNode */ 

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Контроль ошибок платежа в реестра Рапиды ==================================================*/
PROCEDURE RapidaChkErr:
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vErrCode  AS  CHAR  NO-UNDO.
DEF VAR vPayDT    AS  DATETIME  NO-UNDO.
DEF VAR vAmt      AS  DEC       NO-UNDO.
DEF VAR vTmpStr   AS  CHAR      NO-UNDO.

DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPers     FOR person.


/*=== Проверка на ошибки реестра ================================================================*/
lFndErr:
DO:

   /*=== 0200 Не распознан формат сообщения =====================================================*/
   /* Дата и время регистрации перевода в Рапида */
   vPayDT = DATETIME(iExch::DTPay) NO-ERROR.
   IF vPayDT EQ ? THEN
   DO:
      vErrCode = "0200".
      LEAVE lFndErr.
   END.

   /* Сумма перевода */
   ASSIGN
      vAmt    = DEC(iExch::amt-rub) 
      vTmpStr = TRIM(STRING(vAmt, ">>>>>>>>>>>9.99"), "?")
   NO-ERROR.
   IF vAmt LE 0 OR NOT {assigned vTmpStr}  THEN
   DO:
      vErrCode = "0200".
      LEAVE lFndErr.
   END.

   /* Номер счета */
   IF LENGTH(iExch::OrderAcct) NE 20 THEN
   DO:
      vErrCode = "0200".
      LEAVE lFndErr.
   END.

   /*=== 0500 Отсутствует (поле пустое) уникальный идентификатор назначения перевода ============*/
   IF NOT {assigned iExch::SendREF} THEN 
   DO:
      vErrCode = "0500".
      LEAVE lFndErr.
   END.
   ELSE
   DO:
      /* Уникальный номер перевода в НКО */
      ASSIGN
         vAmt    = DEC(iExch::SendREF) 
         vTmpStr = TRIM(STRING(vAmt, ">>>>>>>>>>>9.99"), "?")
      NO-ERROR.
      IF vAmt LE 0 OR NOT {assigned vTmpStr}  THEN
      DO:
         vErrCode = "0200".
         LEAVE lFndErr.
      END.
   END.
   /*=== 0700 Нет счета =========================================================================*/
   IF NOT {assigned iExch::acct-rec} THEN 
   DO:
      vErrCode = "0700".
      LEAVE lFndErr.

   END. /* IF NOT {assigned iExch::acct-rec} THEN  */
   ELSE
   DO:
      {find-act.i &filial = iExch::FilialId
                  &acct   = iExch::acct-rec
                  &curr   = "''"
                  &bact   = bAcct
                  &NoFindInNatCurr = YES}
      IF NOT AVAIL(bAcct) THEN
      DO:
         vErrCode = "0700".
         LEAVE lFndErr.
      END.

      /*=== 0710 Счет закрыт ====================================================================*/
      IF bAcct.close-date NE ? THEN
      DO:
         vErrCode = "0710".
         LEAVE lFndErr.
      END.

      /*=== 0720 Счет заблокирован ==============================================================*/
      vTmpStr = BlockAcct(SUBST("&1,&2", bAcct.acct, bAcct.currency), DATETIME(gend-date + 1, -1)).
      IF {assigned vTmpStr} THEN
      DO:
         vErrCode = "0720".
         LEAVE lFndErr.
      END.

      /*=== 0730 Счет открыт более поздней датой ================================================*/
      IF bAcct.open-date GT gend-date THEN
      DO:
         vErrCode = "0730".
         LEAVE lFndErr.
      END.

   END. /* IF NOT {assigned iExch::acct-rec} THEN ... ELSE */

   /* Проверяем не обязательные ошибки */
   IF    {assigned iExch::name-rec} 
      AND bAcct.cust-cat EQ "Ч"
   THEN
   DO:
      FOR FIRST bPers WHERE bPers.person-id EQ bAcct.cust-id
                      NO-LOCK:
         ASSIGN
            vTmpStr = SUBST("&1 &2", bPers.name-last, bPers.first-names)
            vTmpStr = DelDoubleChars(vTmpStr, " ")
         .
         IF vTmpStr NE iExch::name-rec THEN
         DO:
            /*=== 0800 ФИО клиента не соответствует номеру счета =============================*/
            vErrCode = "0800".
            LEAVE lFndErr.
       
         END. /* ELSE IF NOT {assigned assigned iExch::name-rec} THEN */
      END. /* FOR FIRST bPers WHERE bPers.person-id EQ bAcct.cust-id */
   END. /* IF {assigned iExch::name-rec} THEN */
   ELSE IF NOT {assigned iExch::name-rec} THEN
   DO:
      /*=== 0810 ФИО клиента отсутствует ========================================================*/
      vErrCode = "0810".
      LEAVE lFndErr.

   END. /* ELSE IF NOT {assigned iExch::name-rec} THEN */
   
END. 

iExch::RapidaErr = fStrNvl(vErrCode, "").

END PROCEDURE. /* RapidaChkErr */ 

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Рапида: Сохраняем данные платежа на пакете ================================================*/
PROCEDURE SaveAttrRapida:
   DEF INPUT PARAM iExch   AS  HANDLE  NO-UNDO. /* Указатель на набор данных                     */
   DEF INPUT PARAM iClass  AS  CHAR    NO-UNDO. /* Класс пакета                                  */
   DEF INPUT PARAM iPackId AS  INT64   NO-UNDO. /* Номер пакета                                  */

DEF VAR vPackId  AS  CHAR  NO-UNDO.
DEF VAR vFldList AS  CHAR  NO-UNDO.
DEF VAR vFldName AS  CHAR  NO-UNDO.
DEF VAR vTmpStr  AS  CHAR  NO-UNDO.
DEF VAR vI       AS  INT64 NO-UNDO.

   ASSIGN
      vPackId  = STRING(iPackId)
      vFldList = SUBST("&1,&2",
                       "DTPay,SendRef,SendId,amt-rub,OrderAcct,bank-code-rec,name-rec,name-send",
                       "addr-send,RapidaErr")
   .

   DO vI = 1 TO NUM-ENTRIES(vFldList):

      ASSIGN
         vFldName = ENTRY(vI, vFldList)
         vTmpStr  = ""
         vTmpStr  = iExch:BUFFER-FIELD(vFldName):BUFFER-VALUE
      NO-ERROR.

      UpdateSigns(iClass, vPackId, vFldName, vTmpStr, ?).

   END. /* DO vI = 1 TO NUM-ENTRIES(vFldList): */

END PROCEDURE. /* SaveAttrRapida */

/*===============================================================================================*/
