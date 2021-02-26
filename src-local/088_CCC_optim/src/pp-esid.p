/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: PP-ESID.P
      Comment: Библиотека обработки электронных служебно-информационных
               документов УФЭБС
   Parameters: нет
         Uses: BY DYNAMIC
      Used BY:
      Created: 15.08.2005 NIK
     Modified: 30.11.2005 NIK 1. Изменен поиск исходного сообщения
                              2. Обработка статусов УФЭБС
                              3. Использование PacketCreateF
     Modified: 14.12.2005 NIK Поиск транзакции создания документа
     Modified: 15/04/2006 NIK Импорт сообщения ED206
                              Доработка контроля реквизитов документа
     Modified: 26.05.2006 NIK PROGRESS v10.
     Modified: 17/05/2006 NIK Реструктуризация pp-pack.p
     Modified: 30/06/2006 NIK Поиск по ссылке ответных документов
     Modified: 03/07/2006 REVV К частичной проверке номера документа добавлена
                               полная проверка номера документа (u8003)
     Modified: 02/08/2006 NIK Учет при сравнении ED206 ответных документов
     Modified: 17/08/2006 NIK Изменение прототипа GetEXCHOpKind
     Modified: 27/09/2006 NIK 1.Управление статусами документов
                              2.Контроль последовательности квитанций
     Modified: 07/12/2006 NIK Конвертация выписки
     Modified: 09/01/2007 NIK 1. Использование МЦИСтатусы
                              2. Контроль документов с ошибками
     Modified: 18/01/2007 NIK Сравнение позиционированных документов
     Modified: 30/01/2007 NIK Сравнение начальных при пустом счете получателя
     Modified:
*/
{globals.i}

{form.def}
{g-trans.equ}
{exchange.equ}

DEFINE VAR mCorrAcct    AS CHAR     NO-UNDO.
DEFINE VAR mOpDate      AS DATE     NO-UNDO.
DEFINE VAR mFake        AS INT64  NO-UNDO.

DEFINE VAR mSelfID      AS CHAR     NO-UNDO.
DEFINE VAR mSettID      AS CHAR     NO-UNDO.
DEFINE VAR mBespID      AS CHAR     NO-UNDO.

DEFINE VAR mStmtKind    AS CHAR     NO-UNDO.
DEFINE VAR mStmtStatus  AS CHAR     NO-UNDO.
DEFINE VAR mStmtSCheck  AS LOGICAL  NO-UNDO.
DEFINE VAR mhStmtConv   AS handle   NO-UNDO.
DEFINE VAR mfStmtConv   AS CHAR     NO-UNDO.

DEFINE VAR mStateRecv   AS CHAR     NO-UNDO.
DEFINE VAR mStateErr    AS CHAR     NO-UNDO.
DEFINE VAR mStateRtrn   AS CHAR     NO-UNDO.
DEFINE VAR mStateSend   AS CHAR     NO-UNDO.
DEFINE VAR mStateDone   AS CHAR     NO-UNDO.
DEFINE VAR mStateChgd   AS CHAR     NO-UNDO.
DEFINE VAR mDBTransAcct AS CHAR     NO-UNDO.
DEFINE VAR mEDNoSetStat AS CHAR     NO-UNDO.
DEFINE VAR mState276Op1 AS CHAR     NO-UNDO.
DEFINE VAR mState276Op2 AS CHAR     NO-UNDO.
DEFINE VAR mUISCOS      AS CHAR     NO-UNDO.
DEFINE VARIABLE mStatus508    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mStatusERR508 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderRez     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderMail    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderDef     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mIntRezLim    AS INT64       NO-UNDO.
DEFINE VARIABLE mRezFlag      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mRezREF       AS INT64       NO-UNDO.


DEFINE VAR mTxt274      AS CHAR     NO-UNDO.
DEFINE VAR mInfoCode274 AS CHAR     NO-UNDO.


DEFINE VARIABLE mPropDoc AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE mPropAtt AS LONGCHAR    NO-UNDO.

{intrface.get xclass}
{intrface.get tmess}
{intrface.get strng}

{intrface.get pbase}
{intrface.get trans}
{intrface.get blkob}

{intrface.get exch}
{intrface.get pack}
{intrface.get rfrnc}
{intrface.get filex} 

{intrface.get xrkc}
{intrface.get op}
{intrface.get cust}
{intrface.get count}

{intrface.get bank}
{intrface.get refer}
{intrface.get email}
{intrface.get separate}
{bank-id.i}                                      /*Идентификаторы нашего банка*/
{ttretval.def}

&GLOB ON-ERROR IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

DEFINE TEMP-TABLE ttAttr NO-UNDO
   FIELD attr-class AS CHAR
   FIELD attr-code  AS CHAR
   FIELD attr-name  AS CHAR
   FIELD attr-value AS CHAR
   FIELD exch-class AS CHAR
   FIELD exch-code  AS CHAR
   FIELD is-view    AS LOGICAL
   FIELD order      AS INT64
   INDEX order order.

DEFINE TEMP-TABLE ttPars NO-UNDO
   FIELD PasrsField  AS CHAR
   FIELD FieldValue  AS CHAR
   FIELD Ind1        AS INT64
   FIELD Ind2        AS INT64
.
DEFINE temp-table tt501 NO-UNDO
    FIELD BranchID      AS CHAR
    FIELD OpenDate      AS DATE
    FIELD CloseDate     AS DATE
    FIELD Code          AS CHAR
.
DEFINE temp-table tt503 NO-UNDO
    FIELD BranchID      AS CHAR
    FIELD OpenDate      AS DATE
    FIELD CloseDate     AS DATE
    FIELD OurSWIFTBIC   AS CHAR
    FIELD Code          AS CHAR
    FIELD BIC           AS CHAR
    FIELD SwiftTypeList AS CHAR
.                         


{esid-exp.pro}
{esid-imp.pro}

DEFINE TEMP-TABLE ttInitPack
   FIELD PacketID AS INT64
   INDEX PacketID PacketID.


/*============================================================================*/
mStateErr   = FGetSetting("УФЭБС","СтатусОшбк","В").
mStateRecv  = FGetSetting("УФЭБС","СтатусПрин","ФДД").
mStateRtrn  = FGetSetting("УФЭБС" + (IF work-module =  "mail-mbr" 
                                        THEN "-МБР" 
                                        ELSE ""),
                          "СтатусВозв","ФБКВ").
mStateSend  = FGetSetting("УФЭБС","СтатусОтпр","ФБП").
mStateDone  = FGetSetting("УФЭБС" + (IF work-module =  "mail-mbr" 
                                        THEN "-МБР" 
                                        ELSE ""),
                          "СтатусИспл",chr(251)).
mStateChgd  = FGetSetting("УФЭБС","СтатусИзм",chr(251)).
IF NOT {assigned mStateChgd} THEN
   mStateChgd = CHR(251).
mfStmtConv  = FGetSetting("УФЭБС","StmCnvOpKind","").
mDBTransAcct = FGetSetting("УФЭБС","DBITransAcctList",?).
mStmtSCheck = CAN-FIND(FIRST code WHERE
                             code.class =  ""
                         AND code.code  =  "МЦИСтатусы").
mSelfID     = RKCGetSendID().
mSettID     = FGetSetting("УФЭБС","SendID",mSelfID).
mBespID     = FGetSetting("БЭСП","SendID", "").

mEDNoSetStat = FGetSetting("БЭСП","ED201NoSetOpStat","").
mState276Op1 = FGetSetting("УФЭБС","Эксп276Ст1","").
mState276Op2 = FGetSetting("УФЭБС","Эксп276Ст2","").
mUISCOS = FGetSetting("УИС-ЦОС",?,"NO").
mStatus508 = FGetSetting("УФЭБС","Статус508","").
mStatusERR508 = FGetSetting("УФЭБС","СтатОшбк508","").

ASSIGN
   mOrderRez  = FGetSetting("УФЭБС","OrderRez","")
   mOrderMail = FGetSetting("УФЭБС","OrderMail","")
   mOrderDef  = FGetSetting("УФЭБС","OrderDef","")
   mIntRezLim = INT64(GetEntries(2, mOrderRez, "-", "0"))
   mRezREF    = INT64(GetEntries(1, mOrderRez, "-", "0"))
NO-ERROR.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-esid.p","mStateErr:" + GetNullStr(mStateErr) + 
                             " mStateRecv:" + getNullStr(mStateRecv) + 
                             " mStateRtrn:" + GetNullStr(mStateRtrn) +
                             " mStateSend:"  + getNullStr(mStateSend) + 
                             " mStateDone:" + getNullStr(mStateDone) + 
                             " mfStmtConv:" + getNullStr(mfStmtConv) + 
                            " mStmtSCheck:" + STRING(mStmtSCheck) +
                           " mEDNoSetStat:" + GetNullStr(mEDNoSetStat)).    
&ENDIF

RUN ParsFunc-Дата(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

DEFINE BUFFER c-nostro FOR c-nostro.
FOR FIRST c-nostro WHERE
          c-nostro.corr-acct =  bank-acct
          NO-LOCK:
   mCorrAcct = c-nostro.acct.
END.

{pfuncdef
   &DefLib="esid" 
   &Description="Библиотека обработки электронных служебно-информационных
               документов УФЭБС"}
   
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CheckEmptyFld LOGICAL (INPUT iVal  AS CHAR,
                                INPUT iName AS CHAR):
   IF iVal <> "?" THEN
      RETURN YES.
   ELSE DO:
      RUN Fill-SysMes("","ExchRKC34","","%s=" + iName).
      RETURN NO.
   END.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Сравнение документа в квитанции (выписке) и в базе                         */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDOperCompare:
   DEFINE INPUT  PARAMETER hWOP     AS handle NO-UNDO.
   DEFINE INPUT  PARAMETER iSurr    AS CHAR   NO-UNDO.
   DEFINE PARAMETER BUFFER op-entry FOR op-entry.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vAcctSend AS CHAR           NO-UNDO.
   DEFINE VAR vAcctRecv AS CHAR           NO-UNDO.
   DEFINE VAR vBaseRecv AS CHAR           NO-UNDO.
   DEFINE VAR vBankCode AS CHAR           NO-UNDO.
   DEFINE VAR vAmtRub   AS decimal        NO-UNDO.
   DEFINE VAR vDC       AS CHAR           NO-UNDO.
   DEFINE VAR vDocNum   AS CHAR           NO-UNDO.
   DEFINE VAR vSeanceID AS INT64        NO-UNDO.
   DEFINE VAR vDocNumBs AS CHAR           NO-UNDO.
   DEFINE VAR vDocNumDc AS CHAR           NO-UNDO.
   DEFINE VAR vTail     AS INT64            NO-UNDO.
   DEFINE VAR vAcctXtr  AS CHAR           NO-UNDO.
   {profile ST041}

   DEFINE BUFFER op-bank   FOR op-bank.
   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER mail-user FOR mail-user.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vAcctSend = hWOP:buffer-field("acct-send"):buffer-value
         vAcctRecv = hWOP:buffer-field("acct-rec"):buffer-value
         vBankCode = hWOP:buffer-field("bank-code-rec"):buffer-value
         vAmtRub   = hWOP:buffer-field("amt-rub"):buffer-value
         vDC       = hWOP:buffer-field("Direct"):buffer-value
         vDocNum   = hWOP:buffer-field("doc-num"):buffer-value
         vSeanceID = hWOP:buffer-field("SeanceID"):buffer-value

      NO-ERROR. {&ON-ERROR}
         IF NOT {assigned vDocNum} THEN
            vDocNum = hWOP:buffer-field(GetMangledName("НомДок")):buffer-value.

/*-------- Нет исходного документа, "переворачиваем" реквизиты для ответных --*/
      IF NOT {assigned iSurr} THEN DO:

         /* Выполнение маршрутизации   */
         hWOP:buffer-field("op-kind"):buffer-value = GetEXCHOpKind(vSeanceID,
                                                                   {&DIR-IMPORT},
                                                                   BUFFER mail-user).
         CASE vDC:
            WHEN "1" THEN ASSIGN
               hWOP:buffer-field("Direct"):buffer-value = "Начальные"
            NO-ERROR.
            WHEN "2" THEN ASSIGN
               hWOP:buffer-field("Direct"):buffer-value = "Ответные"
            NO-ERROR.
         END CASE.
         vFlagSet = YES.
         LEAVE MAIN.
      END.

/*-------------------------------------------------- Поиск документа в базе --*/
      FIND FIRST op WHERE
                 op.op =  INT64(ENTRY(1,iSurr))
                 NO-LOCK NO-ERROR. {&ON-ERROR}
      FIND FIRST op-entry OF op WHERE
                 op-entry.op-entry =  INT64(ENTRY(2,iSurr))
                 NO-LOCK NO-ERROR. {&ON-ERROR}
      FIND FIRST op-bank OF op
                 NO-LOCK NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperCompare","==" + string(string(hWOP:buffer-field("RecvREF"):buffer-value),"x(25)") +
                                         " " + string(string(hWOP:buffer-field("SendREF"):buffer-value),"x(25)") +
                                     " err:" + GetNullStr(hWOP:buffer-field("ErrorList"):buffer-value)).
      RUN dbgprint.p ("ESIDOperCompare","DOC " + string(op.doc-num,"x(25)") +
                                           " " + string(vDocNum,"x(25)")).
      RUN dbgprint.p ("ESIDOperCompare","AMT " + string(string(op-entry.amt-rub),"x(25)") +
                                           " " + string(string(vAmtRub),"x(25)")          +
                                           " " + string(op-entry.amt-rub <> vAmtRub)).
      &ENDIF

/*--------------------------------- Проверка на совпадение номера документа --*/
      ASSIGN   vDocNumBs = TRIM(op.doc-num)
               vDocNumDc = TRIM(vDocNum)
               vTail     = LENGTH(LEFT-TRIM(vDocNumDc,"0")).

      IF vDocNumBs <> vDocNumDc
      THEN DO:
         IF ((LENGTH(vDocNumBs) >= vTail)                                    AND
             (SUBSTRING(vDocNumBs,LENGTH(vDocNumBs) - vTail + 1, vTail) EQ
              SUBSTRING(vDocNumDc,LENGTH(vDocNumDc) - vTail + 1, vTail)))
         THEN
            RUN AddErrorFormat(hWOP,"b8002"). /* совпадение "хвоста"          */
         ELSE
            RUN AddErrorFormat(hWOP,"b8003"). /* несовпадение "хвоста"        */
      END.

      IF op-entry.amt-rub <> vAmtRub THEN RUN AddErrorFormat(hWOP,"b8004").

/*----------------------------- Сравнение реквизитов плательщика/получателя --*/
      CASE vDC:
/*-------------------------------------------------------- Начальный платеж --*/
         WHEN "1" THEN DO:
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperCompare","N-DB " + string(op-entry.acct-db,"x(25)")         +
                                                  " " + string(vAcctSend,"x(25)")                +
                                                  " " + string(op-entry.acct-db <> vAcctSend)).
            RUN dbgprint.p ("ESIDOperCompare","N-CR " + string(op.ben-acct,"x(25)")              +
                                                  " " + string(vAcctRecv,"x(25)")                +
                                                  " " + string(op.ben-acct <> vAcctRecv)).
            &ENDIF

            IF {assigned vAcctSend}          AND /* Счет плательщика          */
               NOT CAN-DO(mDBTransAcct,DelFilFromAcct(op-entry.acct-db)) AND
               DelFilFromAcct(op-entry.acct-db) <> vAcctSend THEN DO:
               RUN AddErrorFormat(hWOP,"b8010").
               vAcctXtr = GetXAttrValueEX("op",string(op.op),"acct-send","").
               IF {assigned vAcctXtr} AND vAcctXtr <> vAcctSend THEN
                  RUN AddErrorFormat(hWOP,"b8014").
            END.

            IF {assigned op.ben-acct} THEN       /* Счет получателя           */
               IF {assigned vAcctRecv}     AND
                  op.ben-acct <> vAcctRecv THEN
                  RUN AddErrorFormat(hWOP,"b8012").
               ELSE.
            ELSE                                 /* Счета получателя нет      */
               IF {assigned vAcctRecv} THEN      /* В выписке счет есть       */
                  IF vAcctRecv BEGINS "3010" THEN.
                  ELSE
                     RUN AddErrorFormat(hWOP,"b8012").
               ELSE.
         END.
/*--------------------------------------------------------- Ответный платеж --*/
         WHEN "2" THEN DO:
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperCompare","R-CR " + string(op-entry.acct-cr,"x(25)")         +
                                                  " " + string(vAcctRecv,"x(25)")                +
                                                  " " + string(op-entry.acct-cr <> vAcctRecv)).
            RUN dbgprint.p ("ESIDOperCompare","R-DB " + string(op.ben-acct,"x(25)")              +
                                                  " " + string(vAcctSend,"x(25)")                +
                                                  " " + string(op.ben-acct <> vAcctSend)).
            &ENDIF

            IF {assigned vAcctSend}     AND      /* Счет плательщика          */
               op.ben-acct <> vAcctSend THEN DO:
               IF NOT (vAcctSend =  bank-acct AND  /* При загрузке ED211 счет отправителя может быть к/с банка в РКЦ */
                       hWOP:buffer-field("mail-format"):buffer-value =  "XML-TransInfo") THEN
                  RUN AddErrorFormat(hWOP,"b8020").
            END.
            IF {assigned vAcctRecv}          AND /* Счет получателя           */
               DelFilFromAcct(op-entry.acct-cr) <> vAcctRecv THEN DO:
               vBaseRecv = GetXAttrValue("op",string(op.op),"acct-rec").
               IF vAcctRecv BEGINS "3010"  AND
                  NOT {assigned vBaseRecv} THEN. /* Норма                     */
               ELSE DO:
                  RUN AddErrorFormat(hWOP,"b8022").
                  IF vBaseRecv <> vAcctRecv THEN
                     RUN AddErrorFormat(hWOP,"b8024").
               END.
            END.
         END.
      END CASE.

      IF {assigned vBankCode}            AND
        (NOT AVAILABLE(op-bank)          OR
         op-bank.bank-code <> vBankCode) THEN
         RUN AddErrorFormat(hWOP,"b8030").

      vFlagSet = YES.
   END.
   {profile ST050}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Регистрация входящего сообщения                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDPacketCreate:
   DEFINE INPUT  PARAMETER hWOP        AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER hMain       AS handle   NO-UNDO.
   DEFINE PARAMETER BUFFER bCode       FOR Code.
   DEFINE OUTPUT PARAMETER oPacketID   AS INT64  NO-UNDO.
   DEFINE OUTPUT PARAMETER oParentID   AS INT64  NO-UNDO.

   DEFINE BUFFER bPacket FOR Packet.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vMailUser AS INT64        NO-UNDO.
   DEFINE VAR vSeanceID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vSendID   AS CHAR           NO-UNDO.
   DEFINE VAR vSendREF  AS CHAR           NO-UNDO.
   DEFINE VAR vSendDate AS DATE           NO-UNDO.
   {profile ST051}

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vSeanceID = hMain:buffer-field("SeanceID"):buffer-value
         oParentID = hMain:buffer-field("PacketID"):buffer-value
         vSendID   = hWOP:buffer-field("SendID"):buffer-value
         vSendREF  = hWOP:buffer-field("SendREF"):buffer-value
         vSendDate = hWOP:buffer-field("SendDate"):buffer-value
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDPacketCreate","iFormat:" + GetNullStr(bCode.Code)        +
                                        " op-kind:" + GetBaseOpKind()               +
                                      " vSeanceID:" + GetNullStr(string(vSeanceID)) +
                                      " vParentID:" + GetNullStr(string(vParentID)) +
                                        " vSendID:" + GetNullStr(vSendID)           +
                                       " vSendREF:" + GetNullStr(vSendREF)          +
                                      " vSendDate:" + GetNullStr(string(vSendDate))).
      &ENDIF

PACK:
      DO TRANSACTION ON ERROR UNDO PACK, RETRY PACK:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.
                                                 /* Создаем сообщение         */
         RUN PacketCreateF   (INPUT  vSeanceID,
                              BUFFER bCode,
                              OUTPUT oPacketID).

         IF vSendDate      <> ? AND              /* Ссылка принятого сообшения*/
            {assigned vSendREF} AND
            {assigned vSendID}  THEN
            RUN PacketCreateRef (INPUT  vSendDate,
                                 INPUT  oPacketID,
                                 INPUT  bCode.Misc[{&RKC-REPLY}],
                                 INPUT  vSendID + "|" +
                                        ReferenceFormatValue(bCode.Misc[{&RKC-REPLY}],vSendREF)) NO-ERROR.

         ASSIGN
            hWOP:buffer-field("PacketID"):buffer-value = oPacketID
            hWOP:buffer-field("SeanceID"):buffer-value = vSeanceID
         NO-ERROR. {&ON-ERROR}

      END.

      vFlagSet = YES.
   END.
   {profile ST060}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Поиск исходного документа                                                  */
/*----------------------------------------------------------------------------*/

PROCEDURE ESIDOperationFind:
   DEFINE INPUT  PARAMETER hWOP        AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER iPacketID   AS INT64    NO-UNDO.
   DEFINE PARAMETER BUFFER bCode FOR Code.
   DEFINE OUTPUT PARAMETER oInitialID  AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER oSurr       AS CHAR     NO-UNDO.

   DEFINE BUFFER Reference    FOR Reference.
   DEFINE BUFFER PackObject   FOR PackObject.
   DEFINE BUFFER Packet       FOR Packet.
   DEFINE BUFFER op           FOR op.
   DEFINE BUFFER bPacket      FOR Packet.

   DEFINE VAR vFlagSet   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagFnd   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagBPack AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vRecvID    AS CHAR           NO-UNDO.
   DEFINE VAR vRecvREF   AS CHAR           NO-UNDO.
   DEFINE VAR vRecvFMT   AS CHAR           NO-UNDO.  /* Форматированная сслыка */
   DEFINE VAR vListCls   AS CHAR           NO-UNDO.  /* Список классов ссылок  */
   DEFINE VAR vRetCls    AS CHAR           NO-UNDO.  /* Класс ссылки импорта   */
   DEFINE VAR vRecvCls   AS CHAR           NO-UNDO.  /* Текущий класс ссылки   */
   DEFINE VAR vClsFirst  AS CHAR           NO-UNDO.  
   DEFINE VAR vRecvDate  AS DATE           NO-UNDO.
   DEFINE VAR vItm       AS INT64          NO-UNDO.
   DEFINE VAR vDisable   AS CHAR           NO-UNDO.

   DEFINE VARIABLE vFormat   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStateUBR AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMakePack AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vNumEntry AS INT64       NO-UNDO.

   DEFINE BUFFER sCode FOR code.
   DEFINE BUFFER bOp       FOR op.
   DEFINE BUFFER bOp-entry FOR op-entry.
   {profile ST061}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN ParsFunc-Дата(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.

      ASSIGN
         oSurr      = ""
         vRecvDate  = hWOP:buffer-field("RecvDate"):buffer-value
         vRecvID    = hWOP:buffer-field("RecvID"):buffer-value
         vRecvREF   = hWOP:buffer-field("RecvREF"):buffer-value
         vFormat    = hWOP:BUFFER-FIELD("mail-format"):BUFFER-VALUE
         vListCls   = bCode.Misc[{&RKC-MAIN}]          /* Несколько классов    */
         vRetCls    = bCode.Misc[{&RKC-REPLY}]
         vFlagFnd   = NO
         vFlagBPack = NO
         oInitialID = ?
      NO-ERROR. {&ON-ERROR}

      IF vFormat =  "XML-ED201"    OR       /* для некоторых статусов, пришедших из РКЦ в ED205 на пакет, */
         vFormat =  "XML-ED201BSP" OR       /* ЭСИД не обрабатывается (не привязывается к документам,     */
         vFormat =  "XML-ED206"    OR       /* не проверяется на последовательность, не меняется статус)  */
         vFormat =  "XML-ED206BSP" OR
         vFormat =  "XML-ED508" THEN
         vMakePack = YES.
      ELSE DO:
         vStateUBR = hWOP:buffer-field("State"):BUFFER-VALUE.
         IF GetCodeBuff("ESIDState",vStateUBR, buffer sCode) THEN ASSIGN
            vMakePack = NOT (sCode.Misc[1] =  "НЕТ" or sCode.Misc[1] =  "NO").
         ELSE 
            vMakePack = NO.
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvDate:" + GetNullDat(vRecvDate) +
                                           " vRecvID:" + GetNullStr(vRecvID)   +
                                           " mSelfID:" + GetNullStr(mSelfID)   +
                                          " vRecvREF:" + GetNullStr(vRecvREF)  +
                                          " vListCls:" + GetNullStr(vListCls)).
      &ENDIF
    
/*----------------------------------- Контроль заданности реквизитов поиска --*/
      CheckEmptyFld(GetNullStr(vRecvID),  "EDAuthor") AND
      CheckEmptyFld(GetNullStr(vRecvREF), "EDNo")     AND
      CheckEmptyFld(GetNullDat(vRecvDate),"EDDate").

      IF vRecvDate <  mOpDate THEN               /* Квитанция на вчерашний док*/
         RUN AddErrorFormat(hWOP,"b8001").

/*----------------------------------------- Определим формат и класс ссылки --*/
      IF (vRecvID =  mSelfID) THEN DO:
         ASSIGN        /* Квитанция на наш документ */
         vRecvCls = ENTRY(1,vListCls)
         vRecvFMT = ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.                                  
         {&ON-ERROR}
      END.
      ELSE DO: /*Квитанция на чужой документ*/
         IF NOT ((NUM-ENTRIES(vListCls) =  2) OR (NUM-ENTRIES(vListCls) =  4)) THEN DO:
            RUN Fill-SysMes("","ExchRKC35","",
                            "%s=" + GetNullStr(bCode.Code) +
                            "%s=" + GetNullStr(vRecvID)    +
                            "%s=" + GetNullStr(vRecvREF)   +
                            "%s=" + GetNullDat(vRecvDate)  +
                            "%s=" + GetNullStr(vListCls)).
            LEAVE MAIN.
         END.

         ASSIGN
            vRecvCls = ENTRY(2,vListCls)
            vRecvFMT = vRecvID + "|" + ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.
         {&ON-ERROR}
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvDate:" + GetNullDat(vRecvDate) +
                                          " vRecvFMT:" + GetNullStr(vRecvFMT)  +
                                          " vRecvCls:" + vRecvCls).
      RUN dbgprint.p ("ESIDOperationFind","vFormat:"   + vFormat).
      &ENDIF
      
/*----------------------------------------------- Найдем исходное сообщение --*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","FindOpEntryByREF START").
      &ENDIF

      vClsFirst = vRecvCls.
         RUN  FindOpEntryByREF(INPUT        hWOP,
                               INPUT        iPacketID,
                               BUFFER       bCode,
                               INPUT        vRecvDate,
                            INPUT        vClsFirst,
                               INPUT        vRecvFMT,
                               OUTPUT       oInitialID,
                               OUTPUT       oSurr,
                               OUTPUT       vFlagFnd,
                               INPUT-OUTPUT vFlagBPack).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vFlagFnd:" + STRING(vFlagFnd)).
      &ENDIF

         IF NOT vFlagFnd THEN
FND_LST:
         DO vNumEntry = 1 TO NUM-ENTRIES(vListCls):
         vRecvCls = ENTRY(vNumEntry, vListCls).

            &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDOperationFind","vRecvID:" + GetNullStr(vRecvID)   +
                                           " vRecvCls:" + GetNullStr(vRecvCls)).
            &ENDIF
      
         IF (vRecvCls =  vClsFirst) THEN /* уже проверяли */
            NEXT FND_LST.
         /* для исходного документа не подходят классы ссылок *Return */
         IF (vRecvID =  mSelfID) AND (vRecvCls MATCHES "*Return") THEN 
            NEXT FND_LST.
         /* для ответного документа не подходят классы ссылок *Begin */
         IF (vRecvID <> mSelfID) AND (vRecvCls MATCHES "*Begin") THEN
            NEXT FND_LST.
      
         ASSIGN
            vRecvFMT = (IF vRecvCls MATCHES "*Return"  
                        THEN vRecvID + "|" 
                        ELSE "" ) + 
                       ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("pp-esid.p","vRecvCls: " + vRecvCls).
         RUN dbgprint.p ("pp-esid.p","vRecvFMT: " + vRecvFMT).
      &ENDIF
      
         RUN  FindOpEntryByREF(INPUT        hWOP,
                               INPUT        iPacketID,
                               BUFFER       bCode,
                               INPUT        vRecvDate,
                               INPUT        vRecvCls,
                               INPUT        vRecvFMT,
                               OUTPUT       oInitialID,
                               OUTPUT       oSurr,
                               OUTPUT       vFlagFnd,
                               INPUT-OUTPUT vFlagBPack).
         /* Если нашли документ, выходим */
         IF vFlagFnd =  YES THEN
            LEAVE FND_LST.

      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvID:"    + GetNullStr(vRecvID)   +
                                          " vRecvDate:" + GetNullDat(vRecvDate) +
                                          " vRecvFMT:"  + GetNullStr(vRecvFMT)  +
                                          " vRecvCls:"  + vRecvCls              +
                                          " vMakePack:"  + STRING(vMakePack)              +
                                          " NOT vFlagBPack:"  + STRING(NOT vFlagBPack)              +
                                          " vFlagFnd:"  + STRING(vFlagFnd)).
      &ENDIF
      
      IF NOT vFlagBPack      /* если найден не пакет           */ 
         OR vMakePack THEN   /* или разрешена обработка пакета */
         FOR EACH ttInitPack NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  =  ttInitPack.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
             FIRST op WHERE
                   op.op = INT64(ENTRY(1,PackObject.Surrogate))
                   NO-LOCK  QUERY-TUNING(HINT "RULE"):
                                                 /* Связь квит. с документом  */

            FIND FIRST packet WHERE
                       Packet.PacketID =  ttInitPack.PacketID NO-LOCK NO-ERROR.

            IF avail packet AND
               (packet.mail-format BEGINS "XML-ED244" OR
                packet.mail-format BEGINS "XML-ED243") AND
               GetEntries(2,bCode.Code,"-","") BEGINS "ED201" THEN
            DO:
               RUN PacketCreateLink (Packet.PacketID,
                                     "Packet",
                                     iPacketID,
                                     ENTRY(2,bCode.Description[1])).
            END.
            ELSE DO:
               RUN PacketCreateLink (iPacketID,
                                     PackObject.File-Name,
                                     PackObject.Surrogate,
                                     ENTRY(2,bCode.Description[1])).
               ASSIGN oSurr      = PackObject.Surrogate
                      vFlagFnd   = YES.

            END.

             IF GetEntries(2,bCode.Code,"-","") BEGINS "ED508" THEN
               vFlagBPack = NO.

/*--------------------------- Контроль последовательности импорта квитанций --*/
            CASE GetEntries(2,bCode.Code,"-",""):
               WHEN "ED201"   THEN vDisable = "ED211,ED206,ED207,ED205".
               WHEN "ED205"   THEN vDisable = "ED211,ED206".
               WHEN "EDInfo"  THEN vDisable = "ED211,ED206".
               WHEN "ED206"   THEN vDisable = "ED211".
               OTHERWISE           vDisable = "".
            END CASE.

            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperationFind","bCode.Code:" + bCode.Code +
                                                 " vDisable:" + GetNullStr(vDisable)).
            &ENDIF

            IF NOT PacketEnableExhange ("op-entry",
                                    PackObject.Surrogate,
                                    vDisable,
                                    "",
                                    0) THEN
               RUN AddErrorFormat(hWOP,"b8050").
         END.                                 /* FOR FIRST PackObject ...  */

      IF vFlagFnd <> YES     AND
         {assigned vRecvREF} THEN
         RUN AddErrorFormat(hWOP,"b8000").

      IF vFlagBPack THEN
         ASSIGN
            oSurr       = ""
         .
      vFlagSet = YES.
   END.                                          /* MAIN: DO ON ERROR ...     */
   {empty ttInitPack}
   {profile ST070}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE FindOpEntryByREF.
   DEFINE INPUT        PARAMETER hWOP        AS handle  NO-UNDO.
   DEFINE INPUT        PARAMETER iPacketID   AS INT64 NO-UNDO.
   DEFINE PARAMETER    BUFFER bCode FOR Code.
   DEFINE INPUT        PARAMETER iRecvDate   AS DATE    NO-UNDO.
   DEFINE INPUT        PARAMETER iRecvCls    AS CHAR    NO-UNDO.
   DEFINE INPUT        PARAMETER iRecvFMT    AS CHAR    NO-UNDO.
   DEFINE OUTPUT       PARAMETER oInitialID  AS INT64 NO-UNDO.
   DEFINE OUTPUT       PARAMETER oSurr       AS CHAR    NO-UNDO.
   DEFINE OUTPUT       PARAMETER oFlagFnd    AS LOGICAL NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioFlagBPack AS LOGICAL NO-UNDO.

   DEFINE VAR vDisable  AS CHAR           NO-UNDO.

   DEFINE BUFFER bPacket FOR Packet.
   DEFINE BUFFER Reference FOR Reference.
   DEFINE BUFFER Packet    FOR Packet.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("FindOpEntryByREF","iRecvFMT:" + GetNullStr(iRecvFMT)).    
   &ENDIF

   oFlagFnd = NO.
   FOR EACH  Reference WHERE
             Reference.op-date    =  iRecvDate
         AND Reference.Class-Code =  iRecvCls
         AND Reference.RefValue   =  iRecvFMT
             NO-LOCK,
       EACH  Packet WHERE
             Packet.PacketID      =  Reference.PacketID
         AND (IF shMode THEN Packet.Filial-Id =  shFilial ELSE YES)
             NO-LOCK:

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","Packet.PacketID:" + GetNullInt(Packet.PacketID) + 
                                         " Packet.mail-format:" + GetNullStr(Packet.mail-format)).    
      &ENDIF
      
      ASSIGN
         oFlagFnd   = YES
         oInitialID = Packet.PacketID
         oSurr      = "".

      {empty ttInitPack}
      FOR EACH bPacket WHERE   /* находим вложенные пакеты, если есть */
               bPacket.ParentID =  Packet.PacketID NO-LOCK:
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","bPacket.PacketID:" + GetNullInt(bPacket.PacketID) + 
                                         " bPacket.mail-format:" + GetNullStr(bPacket.mail-format)).    
      &ENDIF
         CREATE ttInitPack.
         ASSIGN
            ttInitPack.PacketID = bPacket.PacketID
            ioFlagBPack          = YES
         .
      END.
      IF NOT CAN-FIND (FIRST ttInitPack NO-LOCK) THEN DO:
         CREATE ttInitPack.
         ASSIGN
            ttInitPack.PacketID = Packet.PacketID.
      END.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","oInitialID:" + GetNullInt(oInitialID)).
      &ENDIF
   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDErrorPrepare:
   DEFINE INPUT  PARAMETER hWOP  AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER iPrf  AS CHAR     NO-UNDO.
   DEFINE OUTPUT PARAMETER oCls  AS CHAR     NO-UNDO.
   DEFINE OUTPUT PARAMETER oLstR AS LOGICAL  NO-UNDO. /* Флаг ошибки РКЦ      */
   DEFINE OUTPUT PARAMETER oLstB AS LOGICAL  NO-UNDO. /* Флаг ошибки БИС      */
   DEFINE OUTPUT PARAMETER oLst  AS CHAR     NO-UNDO. /* Все ошибки           */

   DEFINE VAR vItm AS INT64 NO-UNDO.
   DEFINE VAR vOne AS CHAR    NO-UNDO.
   DEFINE VAR vLst AS CHAR    NO-UNDO.
   DEFINE VAR vWrn AS CHAR    NO-UNDO.
   {profile ST071}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN RETURN ERROR ERROR-STATUS:get-message(1).

      ASSIGN
         oCls = "ErrorESID" /* hWOP:buffer-field("ErrorClass"):buffer-value */
         vLst = hWOP:buffer-field("ErrorList"):buffer-value
      NO-ERROR. {&ON-ERROR}

      oLst = "".                                 /* Все ошибки                */
      DO vItm = 1 TO NUM-ENTRIES(vLst):
         vOne = ENTRY(vItm,vLst).
         IF SUBSTRING(vOne,1,1) <> "b" THEN vOne = iPrf + vOne.
         {additem.i oLst vOne}
      END.

      RUN CheckErrorInst (INPUT  oCls,           /* Разделим на ошибки и пред.*/
                          INPUT  oLst,
                          OUTPUT vWrn,
                          OUTPUT vLst).          /* Все ошибки                */
      oLstR = NO.
      oLstB = NO.
      DO vItm = 1 TO NUM-ENTRIES(vLst):          /* Разделим ошибки           */
         vOne = ENTRY(vItm,vLst).
         IF vOne BEGINS "b" THEN oLstB = YES.    /* Ошибки БИС                */
                            ELSE oLstR = YES.    /* Ошибки РКЦ                */
      END.

      oLst = TRIM(vLst + "," + vWrn,",").
   END.
   {profile ST080}
   RETURN.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Опеределяет указатель на головное сообщений (файл PacketESID)              */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDGetMain:
   DEFINE INPUT-OUTPUT PARAMETER hWOP  AS handle   NO-UNDO.
   DEFINE OUTPUT       PARAMETER hMain AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vMain     AS CHAR           NO-UNDO.
   DEFINE VAR vWarning  AS CHAR           NO-UNDO.
   {profile ST081}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hWOP  = hWOP:default-buffer-handle                /* Квитанция       */
         vMain = hWOP:buffer-field("ExchMain"):buffer-value
         hMain = ObjectValueHandle(vMain)                  /* Общее сообщение */
      NO-ERROR. {&ON-ERROR}

      IF NOT valid-handle(hMain) THEN DO:
         RUN Fill-SysMes("","","","%s=" + vMain +
                                  "%s=" + hWOP:Name).
         UNDO MAIN, RETRY MAIN.
      END.

      vFlagSet = YES.
   END.
   {profile ST090}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDExportREF:
   DEFINE INPUT PARAMETER  hOBJ AS handle NO-UNDO.
   DEFINE PARAMETER BUFFER bCode FOR Code.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vSendREF  AS INT64        NO-UNDO.
   DEFINE VAR vLastREF  AS INT64        NO-UNDO.
   DEFINE VAR vStep        AS INT64          NO-UNDO.   
   DEFINE VAR vRetValue AS CHAR           NO-UNDO.
   DEFINE VAR vRecvID   AS CHAR           NO-UNDO.
   DEFINE VAR vSendID   AS CHAR           NO-UNDO.
   DEFINE VAR vOrderLim  AS CHAR           NO-UNDO.
   DEFINE VAR vOrderStep   AS CHAR           NO-UNDO.
   DEFINE VAR vOrderBeg    AS CHAR           NO-UNDO.
   DEFINE VAR vRange       AS CHAR           NO-UNDO.         
   DEFINE VAR vRef       AS CHAR           NO-UNDO.
   DEFINE VAR vOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vOK          AS LOGICAL        NO-UNDO.
   DEFINE VAR vFnd         AS LOGICAL        NO-UNDO.
   DEFINE VAR vMess        AS CHAR           NO-UNDO.    
   DEFINE VAR vHWork       AS HANDLE         NO-UNDO.
  

   DEFINE BUFFER Reference FOR Reference.

   {profile ST091}

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN DO:
         vRetValue = ERROR-STATUS:get-message(1) + " " + RETURN-VALUE.
         LEAVE MAIN.
      END.

      RUN ParsFunc-Дата(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.

      IF NOT RLockCreate(bCode.Misc[{&RKC-BEGIN}]) THEN
         UNDO MAIN, RETRY MAIN.

      ASSIGN
         vOrderLim   = TRNSettingValue("", "OrderLim", "")  
         vOrderBeg   = TRNSettingValue("", "OrdBegin", "")
         vOrderStep  = TRNSettingValue("", "OrderStep", "")           
         vLastREF = hOBJ:buffer-field("__id"):buffer-value
         vOpKind     = GetBaseOpKind() 
         vStep       = IF {assigned vOrderStep} 
                       THEN INT64(vOrderStep) 
                       ELSE INT64(GetXattrEx(bCode.Misc[{&RKC-BEGIN}],
                                    "Step", "Initial"))         
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "1 vOrderLim:" + GetNullStr(vOrderLim) +
                                        " vOrderBeg:" + GetNullStr(vOrderBeg) + 
                                            " vStep:" + GetNullInt(vStep)).
      &ENDIF

      CREATE BUFFER vHWork FOR TABLE hObj:TABLE-HANDLE.

      IF {assigned vOrderBeg} AND {assigned vOrderLim} THEN
         vRange      = vOrderBeg + "-" + vOrderLim .
      ELSE DO:
         vRange = mOrderDef.
         IF {assigned vRange} THEN 
      ASSIGN
               vOrderBeg   = GetEntries(1,vRange,"-","")
               vOrderLim   = GetEntries(2,vRange,"-","").
      END.

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "2 vOrderLim:" + GetNullStr(vOrderLim) +
                                        " vOrderBeg:" + GetNullStr(vOrderBeg) + 
                                           " vRange:" + GetNullStr(vRange)).
      &ENDIF

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "3 vLastREF:"  + GetNullInt(vLastREF)).
      &ENDIF

      IF {assigned vRange} THEN DO:
         IF vLastREF =  0 THEN
            vLastREF = 1. 
         IF mRezFlag THEN
            vSendREF = mRezREF + 1.
         ELSE DO:

            &IF DEFINED(IS-DEBUG) > 0 &THEN
            RUN dbgprint.p ("ESIDExportREF", 
                          "4 ReferenceGetLastEX:"  + 
                          GetNullStr(ReferenceGetLastEX(
                                          bCode.Misc[{&RKC-BEGIN}],
                                          gend-date,vRange)) + 
                          " vLastREF * vStep:" + GetNullInt(vLastREF * vStep)).
            &ENDIF

            vSendREF = INT64(ReferenceGetLastEX(bCode.Misc[{&RKC-BEGIN}],gend-date,vRange)) +
                       (vLastREF * vStep ) 
                       NO-ERROR.

            &IF DEFINED(IS-DEBUG) > 0 &THEN
            RUN dbgprint.p ("ESIDExportREF", "5 vSendREF:" + GetNullInt(vSendREF)).
            &ENDIF
         END.
      END.
      ELSE vSendREF = INT64(ReferenceGetLast(bCode.Misc[{&RKC-BEGIN}],gend-date)) + 
                      vLastREF + vStep .

      ASSIGN
         vSendREF  = 0 WHEN vSendREF =  ?
         vRecvID   = hOBJ:buffer-field("RecvID"):buffer-value
         vSendID   = hOBJ:buffer-field("SendID"):buffer-value
      NO-ERROR. {&ON-ERROR}

      ASSIGN
         hOBJ:buffer-field("RecvID"):buffer-value   = RKCGetRecvID() WHEN NOT {assigned vRecvID} /* РКЦ  */
         hOBJ:buffer-field("SendID"):buffer-value   = RKCGetSendID() WHEN NOT {assigned vSendID} /* Банк */
         hOBJ:buffer-field("SendREF"):buffer-value  = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                                                           STRING(vSendREF))
         hOBJ:buffer-field("SendDate"):buffer-value = mOpDate
         vSendREF                                   = hOBJ:buffer-field("SendREF"):buffer-value
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF mRezFlag THEN DO:  /* ищем сразу в резерве */
         vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                         STRING(vSendREF)).
         vFnd = YES.
         DO WHILE vFnd:
            vFnd = CAN-FIND(FIRST Reference WHERE
                                  Reference.op-date    =  mOpDate
                              AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                              AND Reference.RefValue   =  vRef).
            IF NOT vFnd THEN
               vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
            IF NOT vFnd THEN 
               LEAVE.
            vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).                       
            IF vSendREF >  mIntRezLim THEN 
               LEAVE.
         END.
         IF vSendREF >  mIntRezLim THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                                        "Резервный диапазон номеров полностью использован." + 
                                        "Экспорт остановлен.").
            LEAVE MAIN.
         END.
         mRezREF = vSendREF. 
      END.
      ELSE DO:
                       /* поиск дырок в основном интервале при достижении лимита */ 
         IF vSendREF >= INT64(vOrderLim) THEN DO:
            vSendREF = IF {assigned vRange} 
                       THEN INT64(vOrderBeg)
                       ELSE 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                            STRING(vSendREF)).
            vFnd = YES.
            DO WHILE vFnd:
               vFnd = CAN-FIND(FIRST Reference WHERE
                                     Reference.op-date    =  mOpDate
                                 AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                                 AND Reference.RefValue   =  vRef).
               IF NOT vFnd THEN
                  vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
               IF NOT vFnd THEN
                  LEAVE.
               vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
               IF vSendREF >  INT64(vOrderLim) THEN
                  LEAVE.
            END.
         END.
                                          /* переключение на резерв */
         IF vSendREF >  INT64(vOrderLim) THEN DO:

            IF NOT {assigned mOrderRez} THEN DO:  /* резерв не определен */
               RUN FillSysMes IN h_tmess ("", "", "-1",
                                          "Превышение допустимого диапазона электронных номеров").
               LEAVE MAIN.
            END.
            /* блокировка резервного диапазона */
            vOK = LockObjectEx ("RLock",
                          bCode.Misc[{&RKC-BEGIN}] + "," + mOrderRez,
                          YES,
                          shFilial,
                          OUTPUT vMess) NO-ERROR.
            IF NOT vOK THEN DO:
               RUN Fill-SysMes IN h_tmess ("", "", "-1", SUBSTITUTE("Резервный диапазон &1 
                                          заблокирован другим пользователем",mOrderRez)). 
               LEAVE MAIN.
            END.

            vSendREF = INT64(GetEntries(1,mOrderRez,"-","")) NO-ERROR.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
            vFnd = YES.
            DO WHILE vFnd:
               vFnd = CAN-FIND(FIRST Reference WHERE
                                     Reference.op-date    =  mOpDate
                                 AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                                 AND Reference.RefValue   =  vRef).
               IF NOT vFnd THEN
                  vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
               IF NOT vFnd THEN
                  LEAVE.
            vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
               IF vSendREF >  mIntRezLim THEN
                  LEAVE.
         END.
            IF vSendREF >  mIntRezLim THEN DO:
               RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                                           "Резервный диапазон номеров полностью использован." + 
                                           "Экспорт остановлен.").
               LEAVE MAIN.
      END.

            mRezFlag = YES.
            IF {assigned mOrderMail} THEN
               RUN SendFMail IN h_email (mOrderMail,
                             "Интервал номеров полностью использован, задействован резерв . ",
                  SUBSTITUTE("Для транзакции &1  указанный  интервал номеров полностью использован. 
                              Экспорт выполнен с использованием резервных номеров",vOpKind),
                             "",
                             "")
               NO-ERROR. {&ON-ERROR}          
            RUN SetRezFlag IN h_rfrnc (YES).

         END. /* переключение на резерв */
      END.   /* NOT mRezFlag */

      hOBJ:buffer-field("SendREF"):buffer-value  =
            ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
      vFlagSet = YES.
   END.

/* Разблокировать номера здесь нельзя, т.к.
   записи Reference еще не созданы */

   IF VALID-HANDLE(vHWork) THEN
      DELETE OBJECT vHWork NO-ERROR.

   {profile ST100}
   IF vFlagSet =  ?
      THEN RETURN ERROR vRetValue.
      ELSE RETURN.

END PROCEDURE.
/******************************************************************************/

PROCEDURE ESIDInitFind.
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iClassREF   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iREF        AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPacketID   AS INT64       NO-UNDO.

   DEFINE BUFFER Reference FOR  Reference.

   oPacketID = ?.
   FIND FIRST Reference WHERE Reference.op-date    =  iDate
                          AND Reference.class-code =  iClassREF
                          AND Reference.RefValue   =  iREF
      NO-LOCK NO-ERROR.
   IF AVAIL Reference THEN
      oPacketID = Reference.PacketID.

END PROCEDURE.


PROCEDURE ESIDExportInitial.
   ASSIGN
      mTxt274      = ""
      mInfoCode274 = ""
   .
END PROCEDURE.

PROCEDURE ESIDExportShutDown.

END PROCEDURE.

{ed503.pro}
{ed434.pro}

/******************************************************************************/
/* $LINTFILE='pp-esid.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.991+03:00' */
/*prosignThfsRzYwFAzpGPjaYNQbeA*/