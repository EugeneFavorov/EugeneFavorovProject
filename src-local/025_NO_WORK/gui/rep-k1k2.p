{globals.i}
{intrface.get tmess}

/* +++ rep-k1k2.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am +++ */

/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: rep-k1k2.p
      Comment: Протокол работы транзакций одиночного и группового списания 
               с картотеки 2 и картотеки блокированных счетов, а также 
               переноса между этими картотеками.
   Parameters: Строка формата
               "Режим|Маска клиентского счёта|Маска назначений|Статус"
      Created: 26.06.2014 stss
     Modified: 09.10.2014 krok
     Modified: 
*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

&SCOPED-DEFINE AcctO-Type-Crd2 Карт2ВнСчет
&SCOPED-DEFINE AcctO-Type-CrdB КартБВнСчет

&SCOPED-DEFINE Kau-Id-Crd2     Карт-ка2
&SCOPED-DEFINE Kau-Id-CrdB     КартБлСч

&SCOPED-DEFINE E-NONE          0
&SCOPED-DEFINE E-NOACCT        1
&SCOPED-DEFINE E-CLOSED        2
&SCOPED-DEFINE E-STOPPED       3
&SCOPED-DEFINE E-NOOP          4
&SCOPED-DEFINE E-NO47423       5
&SCOPED-DEFINE E-BADKAU        6
&SCOPED-DEFINE E-BANKRUPT      7
&SCOPED-DEFINE E-BADNALSGN     8

&SCOPED-DEFINE nn              10

&SCOPED-DEFINE line0           PUT UNFORMATTED "┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐" SKIP.
&SCOPED-DEFINE line1           PUT UNFORMATTED "┌─────┬───────────────────────────────┬───────────────────────┬──────────────────────┬─────────┬────────────┬─────────────────────┬────────────────────────────────────────────┐" SKIP.
&SCOPED-DEFINE line2           PUT UNFORMATTED "├─────┴───────────────────────────────┴───────────────────────┴──────────────────────┴─────────┴────────────┴─────────────────────┴────────────────────────────────────────────┤" SKIP.
&SCOPED-DEFINE line3           PUT UNFORMATTED "├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤" SKIP.
&SCOPED-DEFINE line4           PUT UNFORMATTED "├─────┼───────────────────────────────┼───────────────────────┼──────────────────────┼─────────┼────────────┼─────────────────────┼────────────────────────────────────────────┤" SKIP.
&SCOPED-DEFINE line5           PUT UNFORMATTED "├─────┬───────────────────────────────┬───────────────────────┬──────────────────────┬─────────┬────────────┬─────────────────────┬────────────────────────────────────────────┤" SKIP.
&SCOPED-DEFINE line6           PUT UNFORMATTED "└─────┴───────────────────────────────┴───────────────────────┴──────────────────────┴─────────┴────────────┴─────────────────────┴────────────────────────────────────────────┘" SKIP.
&SCOPED-DEFINE line7           PUT UNFORMATTED "└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘" SKIP.

{globals.i}
{sh-defs.i}
{wordwrap.def}
{intrface.get acct}
{intrface.get blkob}
{intrface.get data}
{intrface.get db2l}
{intrface.get xclass}
{intrface.get strng}

DEFINE TEMP-TABLE tt-cust NO-UNDO
   FIELD cust-cat  LIKE acct.cust-cat
   FIELD cust-id   LIKE acct.cust-id
   FIELD cust-name AS   CHARACTER
.

DEFINE TEMP-TABLE tt-acct NO-UNDO
   FIELD cust-cat  LIKE acct.cust-cat
   FIELD cust-id   LIKE acct.cust-id
   FIELD acct      LIKE acct.acct
   FIELD currency  LIKE acct.currency
   FIELD acct-crd2 LIKE acct.acct
   FIELD acct-crdb LIKE acct.acct
.

DEFINE TEMP-TABLE tt-lock NO-UNDO
   FIELD acct     LIKE tt-acct.acct
   FIELD currency LIKE tt-acct.currency
   FIELD type     AS   CHARACTER
   FIELD amt      AS   DECIMAL

   INDEX primary IS PRIMARY UNIQUE
         acct
         currency
         type
.

DEFINE TEMP-TABLE tt-opo NO-UNDO
   FIELD op       LIKE op.op
   FIELD accto    LIKE tt-acct.acct
   FIELD currency LIKE tt-acct.currency
   FIELD doc-num  LIKE op.doc-num
   FIELD doc-date LIKE op.doc-date
   FIELD amt      AS   DECIMAL
   FIELD reason   AS   INT64
   FIELD s-reason AS   CHARACTER

   INDEX primary IS PRIMARY UNIQUE
         op
.

&SCOPED-DEFINE FilterClass FilterProc

DEFINE VARIABLE mOK        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mInstance  AS HANDLE    NO-UNDO.
DEFINE VARIABLE mHTable    AS HANDLE    NO-UNDO.
DEFINE VARIABLE mMode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskContr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStatus    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChkBankr  AS LOGICAL   NO-UNDO.

IF NUM-ENTRIES(iParams,"|") < 4 THEN DO:
   RUN Fill-AlertSysMes IN h_tmess("","",1,"Ошибка параметров!").

   RETURN.
END.
ASSIGN
   mMode      = ENTRY(1, iParams, "|")
   mMaskAcct  = REPLACE(ENTRY(2, iParams, "|"), ";", ",")
   mMaskContr = REPLACE(ENTRY(3, iParams, "|"), ";", ",")
   mStatus    = ENTRY(4, iParams, "|")
   mChkBankr  = toLogical(ENTRY(5, iParams, "|"))
                WHEN NUM-ENTRIES(iParams, "|") > 4
.

RUN PrepareInstance IN h_data ("").
RUN GetEmptyInstance IN h_data ("{&FilterClass}",
                                ?,
                                0,
                                "",
                                OUTPUT mInstance,
                                OUTPUT mOK).

IF mOK THEN DO:
   RUN SetSysConf IN h_base ("crda-flt,ignore-posb", "YES").
   RUN crda-flt.p ("{&FilterClass}", mInstance) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(mInstance) THEN
      mHTable = mInstance::FilterTable.
   RUN DeleteOldDataProtocol IN h_base ("crda-flt,ignore-posb").
END.
IF VALID-HANDLE(mHTable) THEN DO:
   RUN FillData(mHTable, gend-date).
   {setdest.i &filename = "'rep-k1k2.txt'"}
   RUN Print.
   {preview.i &filename = "'rep-k1k2.txt'"}
END.

RETURN.

PROCEDURE AddCust.
   DEFINE INPUT-OUTPUT PARAMETER iCustCat LIKE tt-cust.cust-cat NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER iCustId  LIKE tt-cust.cust-id  NO-UNDO.
   DEFINE INPUT        PARAMETER iAcct    LIKE acct.acct        NO-UNDO.

   DEFINE BUFFER tt-cust FOR tt-cust.
   DEFINE BUFFER acct    FOR acct.

   IF NOT CAN-FIND(FIRST tt-cust WHERE
                      tt-cust.cust-cat = iCustCat AND
                      tt-cust.cust-id  = iCustId
                   NO-LOCK)
   THEN DO TRANSACTION:
      CREATE tt-cust.
      ASSIGN
         tt-cust.cust-cat = iCustCat
         tt-cust.cust-id  = iCustId
      .
      RUN GetCustName0(iCustCat,
                       iCustId,
                       iAcct,
                       OUTPUT tt-cust.cust-name).
      IF NOT {assigned tt-cust.cust-name} THEN DO:
         {find-act.i &acct = iAcct}
         IF AVAILABLE acct AND acct.cust-cat = "В" THEN
            tt-cust.cust-name = acct.details.
      END.
      ASSIGN
         iCustCat = tt-cust.cust-cat
         iCustId  = tt-cust.cust-id
      .
      RELEASE tt-cust.
   END.
END PROCEDURE.

PROCEDURE GetCustName0.
   DEFINE INPUT  PARAMETER iCustCat  LIKE acct.cust-cat NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId   LIKE acct.cust-id  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct     LIKE acct.acct     NO-UNDO.
   DEFINE OUTPUT PARAMETER oCustName AS   CHARACTER     NO-UNDO.

   DEFINE VARIABLE vCustName1 LIKE oCustName.
   DEFINE VARIABLE vCustName2 LIKE oCustName.
   DEFINE VARIABLE vDummy     AS   CHARACTER NO-UNDO.

   RUN GetCustName IN h_base (iCustCat,
                              iCustId,
                              iAcct,
                              OUTPUT vCustName1,
                              OUTPUT vCustName2,
                              INPUT-OUTPUT vDummy).
   oCustName = TRIM(TRIM(vCustName1) + " " + TRIM(vCustName2)).
END PROCEDURE.

PROCEDURE AddSpecial.
   DEFINE INPUT PARAMETER iAcctMask  AS   CHARACTER        NO-UNDO.
   DEFINE INPUT PARAMETER iContract  AS   CHARACTER        NO-UNDO.
   DEFINE INPUT PARAMETER iXAttrCode AS   CHARACTER        NO-UNDO.
   DEFINE INPUT PARAMETER iOpDate    LIKE op-date.op-date  NO-UNDO.
   DEFINE INPUT PARAMETER iOpStatus  LIKE op.op-status     NO-UNDO.

   DEFINE BUFFER acct  FOR acct.
   DEFINE BUFFER acctb FOR acct.

   DEFINE VARIABLE vPos      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vAcct     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCurrency AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcctSurr AS CHARACTER NO-UNDO.

   /* По всем внебалансовым счетам с заданной маской и назначением */
   FOR EACH acct WHERE
      acct.acct-cat = "o"          AND
      CAN-DO(iAcctMask, acct.acct) AND
      CAN-DO(iContract, acct.contract)
   NO-LOCK:
      /* Исключаем те, на которых нулевой остаток (нет документов на картотеке) */
      RUN acct-pos IN h_base (acct.acct,
                              acct.currency,
                              iOpDate,
                              iOpDate,
                              iOpStatus).
      vPos = IF {assigned acct.currency}
             THEN sh-val
             ELSE sh-bal.
      IF vPos = 0 THEN
         NEXT.
      /* Пробуем определить связанный балансовый счёт */
      RUN FindAcctB(iXAttrCode,
                    Surrogate(BUFFER acct:HANDLE),
                    BUFFER acctb).
      IF NOT AVAILABLE acctb THEN
         RUN FindAcctB(iXAttrCode,
                       acct.acct,
                       BUFFER acctb).
      IF AVAILABLE acctb THEN DO:
         IF acctb.close-date <= iOpDate THEN
            /* Связанный балансовый счёт найден, но закрыт */
            RUN ProcessAcct(acct.cust-cat,
                            acct.cust-id,
                            acctb.acct,
                            acctb.currency,
                            acct.acct,
                            iXAttrCode,
                            iOpDate,
                            {&E-CLOSED}).
      END.
      ELSE
         /* Связанный балансовый счёт не найден */
         RUN ProcessAcct(acct.cust-cat,
                         acct.cust-id,
                         "",
                         "",
                         acct.acct,
                         iXAttrCode,
                         iOpDate,
                         {&E-NOACCT}).
   END.
END PROCEDURE.

PROCEDURE FindAcctB.
   DEFINE INPUT PARAMETER        iXAttrCode  LIKE signs.code       NO-UNDO.
   DEFINE INPUT PARAMETER        iXAttrValue LIKE signs.code-value NO-UNDO.
   DEFINE       PARAMETER BUFFER acct        FOR  acct.

   DEFINE BUFFER signs FOR signs.

   DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
   DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.

   RELEASE acct.
   FIND FIRST signs WHERE
      signs.file-name  = "acct"     AND
      signs.code       = iXAttrCode AND
      signs.code-value = iXAttrValue
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE signs THEN
      FIND FIRST signs WHERE
         signs.file-name   = "acct"     AND
         signs.code        = iXAttrCode AND
         signs.xattr-value = iXAttrValue
      NO-LOCK NO-ERROR.
   IF AVAILABLE signs THEN DO:
      ASSIGN
         vAcct     = ENTRY(1, signs.surrogate)
         vCurrency = ENTRY(2, signs.surrogate)
                     WHEN NUM-ENTRIES(signs.surrogate) > 1
      .
      {find-act.i &acct = vAcct
                  &curr = vCurrency}
   END.
END PROCEDURE.

PROCEDURE ProcessAcct.
   DEFINE INPUT PARAMETER iCustCat   LIKE tt-acct.cust-cat NO-UNDO.
   DEFINE INPUT PARAMETER iCustid    LIKE tt-acct.cust-id  NO-UNDO.
   DEFINE INPUT PARAMETER iAcct      LIKE tt-acct.acct     NO-UNDO.
   DEFINE INPUT PARAMETER iCurrency  LIKE tt-acct.currency NO-UNDO.
   DEFINE INPUT PARAMETER iAcctO     AS   CHARACTER        NO-UNDO.
   DEFINE INPUT PARAMETER iAcctOType AS   CHARACTER        NO-UNDO.
   DEFINE INPUT PARAMETER iOpDate    LIKE op-date.op-date  NO-UNDO.
   DEFINE INPUT PARAMETER iReason    AS   INT64            NO-UNDO.

   DEFINE BUFFER tt-acct FOR tt-acct.
   DEFINE BUFFER acct    FOR acct.
   DEFINE BUFFER code    FOR code.

   DEFINE VARIABLE vIsBankrupt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vStage      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDocNum     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE vSReason    AS CHARACTER NO-UNDO.

   RUN AddCust(INPUT-OUTPUT iCustCat,
               INPUT-OUTPUT iCustId,
               IF {assigned iAcct}
               THEN iAcct
               ELSE iAcctO).
   DO TRANSACTION:
      FIND FIRST tt-acct WHERE
         tt-acct.cust-cat = iCustCat AND
         tt-acct.cust-id  = iCustId  AND
         tt-acct.acct     = iAcct    AND
         tt-acct.currency = iCurrency
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAILABLE tt-acct THEN DO:
         CREATE tt-acct.
         ASSIGN
            tt-acct.cust-cat = iCustCat
            tt-acct.cust-id  = iCustId
            tt-acct.acct     = iAcct
            tt-acct.currency = iCurrency
         .
         RUN AddLocks(tt-acct.acct,
                      tt-acct.currency,
                      iOpDate).
      END.
      ASSIGN
         tt-acct.acct-crd2 = iAcctO
                             WHEN iAcctOType = "{&AcctO-Type-Crd2}"
         tt-acct.acct-crdb = iAcctO
                             WHEN iAcctOType = "{&AcctO-Type-CrdB}"
      .
      RELEASE tt-acct.
      /* Если это счёт банкрота, то причина несписания очевидна */
      IF mChkBankr           AND
         iReason = {&E-NONE} AND
         toLogical(FGetSetting("БанкрКонтр", "КонтрВкл", "Нет"))
      THEN DO:
         {find-act.i &acct = iAcct
                     &curr = iCurrency}
         IF AVAILABLE acct THEN DO:
            IF CAN-DO(FGetSetting("БанкрКонтр", "НазнСпецСч", ""),
                      acct.contract)
            THEN
               vIsBankrupt = YES.
            ELSE DO:
/*=== Отключено до выяснения предназначения ===
               FIND FIRST code WHERE
                  code.class  = "РКО47423" AND
                  code.parent = "РКО47423" AND
                  (ENTRY(1, code.code) = acct.number OR
                   code.name           = acct.number)
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE code THEN
*/
                  RUN GetBankrupcyInfo IN h_acct (acct.acct,
                                                  acct.currency,
                                                  iOpDate,
                                                  OUTPUT vIsBankrupt,
                                                  OUTPUT vStage,
                                                  OUTPUT vDocNum,
                                                  OUTPUT vEndDate).
            END.
         END.
         IF vIsBankrupt THEN DO:
            vSReason = SUBSTITUTE("Счёт &1. Клиент - банкрот!", acct.number).
            IF {assigned vDocNum} THEN
               vSReason = SUBSTITUTE("&1. Номер дела: &2",
                                     vSReason,
                                     vDocNum).
            IF vEndDate <> ? THEN
               vSReason = SUBSTITUTE("&1. Дата решения: &2",
                                     vSReason,
                                     STRING(vEndDate, "99.99.99")).
            iReason = {&E-BANKRUPT}.
         END.
      END.
      RUN AddOpO(iAcctO,
                 iCurrency,
                 iOpDate,
                 iReason,
                 vSReason).
   END.
END PROCEDURE.

PROCEDURE AddOpO.
   DEFINE INPUT PARAMETER iAcctO    LIKE tt-opo.accto    NO-UNDO.
   DEFINE INPUT PARAMETER iCurrency LIKE tt-opo.currency NO-UNDO.
   DEFINE INPUT PARAMETER iOpDate   LIKE op-date.op-date NO-UNDO.
   DEFINE INPUT PARAMETER iReason   AS   INT64           NO-UNDO.
   DEFINE INPUT PARAMETER iSReason  AS   CHARACTER       NO-UNDO.

   DEFINE BUFFER kau      FOR kau.
   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER op-entry FOR op-entry.
   DEFINE BUFFER acct     FOR acct.

   DEFINE VARIABLE vOpOSurr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpBSurr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIsCrdCrd AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vAcctCr   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBadKau   AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE vStrNalErr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChkNalOk  AS LOGICAL   NO-UNDO.

   FOR EACH kau WHERE
      kau.acct     =  iAcctO    AND
      kau.currency =  iCurrency AND
      kau.zero-bal <> YES
   NO-LOCK:
      ASSIGN
         vOpOSurr  = ENTRY(1, kau.kau)
         vIsCrdCrd = NO
         vBadKau   = NO
      .
      FIND LAST op-entry WHERE
         op-entry.acct-db =  kau.acct AND
         op-entry.kau-db  =  kau.kau  AND
         op-entry.kau-cr  <> kau.kau  AND
         op-entry.op-date =  iOpDate
      NO-LOCK NO-ERROR.
      IF AVAILABLE op-entry THEN
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST op WHERE
            op.op = INT64(vOpOSurr)
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE op OR
         CAN-FIND(FIRST tt-opo WHERE
                     tt-opo.op = op.op
                  NO-LOCK)
      THEN
         NEXT.
      /*
         Документы переноса между картотеками исключаем из рассмотрения, 
         иначе они будут ошибочно приняты за несписанные
      */
      FOR EACH op-entry OF op NO-LOCK:
         RUN IsCrdCrdEntry(BUFFER op-entry,
                           "{&Kau-Id-Crd2},{&Kau-Id-CrdB}",
                           OUTPUT vIsCrdCrd).
         IF vIsCrdCrd THEN
            LEAVE.
      END.
      IF vIsCrdCrd THEN DO:
         IF op-entry.kau-db = op-entry.kau-cr THEN
            NEXT.
         ELSE
            vBadKau = YES.
      END.
      DO TRANSACTION:
         CREATE tt-opo.
         ASSIGN
            tt-opo.op       = op.op
            tt-opo.accto    = iAcctO
            tt-opo.currency = iCurrency
            tt-opo.doc-num  = op.doc-num
            tt-opo.doc-date = op.doc-date
            tt-opo.amt      = kau.balance
            tt-opo.reason   = {&E-NONE}
            tt-opo.s-reason = iSReason
         .
         IF iReason = {&E-NONE} AND NOT vBadKau THEN DO:
            vOpBSurr = GetXAttrValueEx("op", vOpOSurr, "op-bal", "0").
            FIND FIRST op WHERE
               op.op = INT64(vOpBSurr)
            NO-LOCK NO-ERROR.
            IF AVAILABLE op THEN DO:
               IF GetXAttrValueEx("op",
                                  vOpOSurr,
                                  "ПриостСпис",
                                  "Нет") = "Да"
                  AND
                  kau.kau-id = "{&Kau-Id-CrdB}"
               THEN
                  tt-opo.reason = {&E-STOPPED}.
               ELSE IF op.doc-date < DATE(MONTH(iOpDate),
                                          1,
                                          YEAR(iOpDate))
               THEN DO:
                  FOR EACH op-entry OF op
                  NO-LOCK,
                  FIRST acct WHERE
                     acct.acct     = op-entry.acct-cr AND
                     acct.bal-acct = 47423
                  NO-LOCK:
                     vAcctCr = DelFilFromAcct(acct.acct).
                     IF NOT CAN-FIND(FIRST code WHERE
                                        code.class  =      "РКО47423" AND
                                        code.parent =      "РКО47423" AND
                                        code.code   BEGINS vAcctCr
                                     NO-LOCK)
                     THEN DO:
                        tt-opo.reason = {&E-NO47423}.
                        LEAVE.
                     END.
                  END.
               END.

               /* Проверки налоговых реквизитов по 148-Н */
               IF tt-opo.reason EQ {&E-NONE} THEN DO:
                  RUN chksgnnalplatin.p(INT64(vOpBSurr),
                                        NO,
                                        OUTPUT vStrNalErr,
                                        OUTPUT vChkNalOk).
                  IF vChkNalOk AND
                     {assigned vStrNalErr} THEN
                     ASSIGN
                       tt-opo.reason   = {&E-BADNALSGN}
                       tt-opo.s-reason = vStrNalErr.
               END.
            END.
            ELSE
               tt-opo.reason = {&E-NOOP}.
         END.
         ELSE
            tt-opo.reason = IF iReason = {&E-NONE}
                            THEN {&E-BADKAU}
                            ELSE iReason.
         RELEASE tt-opo.
      END.
   END.
END PROCEDURE.

PROCEDURE IsCrdCrdEntry.
   DEFINE        PARAMETER BUFFER op-entry FOR op-entry.
   DEFINE INPUT  PARAMETER iKauIdList      AS  CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oIsCrdCrdEntry  AS  LOGICAL   NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   DEFINE VARIABLE vKauIdDb LIKE acct.kau-id NO-UNDO.
   DEFINE VARIABLE vKauIdCr LIKE acct.kau-id NO-UNDO.

   IF AVAILABLE op-entry THEN DO:
      RUN FindAcct(BUFFER acct, BUFFER op-entry, YES).
      RUN GetKauId(BUFFER acct, OUTPUT vKauIdDb).
      IF CAN-DO(iKauIdList, vKauIdDb) THEN DO:
         RUN FindAcct(BUFFER acct, BUFFER op-entry, NO).
         RUN GetKauId(BUFFER acct, OUTPUT vKauIdCr).
         oIsCrdCrdEntry = CAN-DO(iKauIdList, vKauIdCr) AND
                          vKauIdCr <> vKauIdDb.
      END.
   END.
   ELSE
      oIsCrdCrdEntry = ?.
END PROCEDURE.

PROCEDURE FindAcct.
   DEFINE       PARAMETER BUFFER acct     FOR acct.
   DEFINE       PARAMETER BUFFER op-entry FOR op-entry.
   DEFINE INPUT PARAMETER iDbCr           AS  LOGICAL NO-UNDO.

   DEFINE VARIABLE vAcct LIKE acct.acct NO-UNDO.

   IF AVAILABLE op-entry THEN DO:
      vAcct = IF iDbCr
              THEN op-entry.acct-db
              ELSE op-entry.acct-cr.
      IF type-balance AND NOT type-curracct THEN DO:
         {find-act.i &acct = "vAcct"
                     &curr = "op-entry.currency"}
      END.
      ELSE
         FIND FIRST acct WHERE
            acct.acct     = vAcct AND
            acct.currency = op-entry.currency
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE GetKauId.
   DEFINE        PARAMETER BUFFER acct FOR  acct.
   DEFINE OUTPUT PARAMETER oKauId      LIKE acct.kau-id NO-UNDO.

   DEFINE BUFFER bal-acct FOR bal-acct.

   IF AVAILABLE acct THEN DO:
      IF {assigned acct.kau-id} THEN
         oKauId = acct.kau-id.
      ELSE DO:
         FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
         IF AVAILABLE bal-acct THEN
            oKauId = bal-acct.kau-id.
      END.
   END.
END PROCEDURE.

PROCEDURE FillData.
   DEFINE INPUT PARAMETER iHFilterTable AS   HANDLE          NO-UNDO.
   DEFINE INPUT PARAMETER iOpDate       LIKE op-date.op-date NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   DEFINE VARIABLE vHQuery   AS HANDLE NO-UNDO.
   DEFINE VARIABLE vHFBuffer AS HANDLE NO-UNDO.

   RUN AddSpecial("90902810*",
                  "Карт2",
                  "{&AcctO-Type-Crd2}",
                  iOpDate,
                  mStatus).
   RUN AddSpecial("90901810*",
                  "КартБл,КартРд",
                  "{&AcctO-Type-CrdB}",
                  iOpDate,
                  mStatus).
   vHFBuffer = iHFilterTable:DEFAULT-BUFFER-HANDLE.
   CREATE QUERY vHQuery.
   vHQuery:SET-BUFFERS(vHFBuffer).
   vHQuery:QUERY-PREPARE("FOR EACH " + vHFBuffer:NAME + " NO-LOCK").
   vHQuery:QUERY-OPEN().
   REPEAT:
      vHQuery:GET-NEXT().
      IF vHQuery:QUERY-OFF-END THEN
         LEAVE.
      {find-act.i &acct = vHFBuffer::acct
                  &curr = vHFBuffer::currency}
      IF NOT AVAILABLE acct THEN
         NEXT.
      IF vHFBuffer::pos-crd2 <> 0 THEN
         RUN ProcessAcct(acct.cust-cat,
                         acct.cust-id,
                         acct.acct,
                         acct.currency,
                         ENTRY(1, GetXAttrValueEx("acct",
                                                  Surrogate(BUFFER acct:HANDLE),
                                                  "{&AcctO-Type-Crd2}",
                                                  "")),
                         "{&AcctO-Type-Crd2}",
                         iOpDate,
                         {&E-NONE}).
      IF vHFBuffer::pos-crdb <> 0 THEN
         RUN ProcessAcct(acct.cust-cat,
                         acct.cust-id,
                         acct.acct,
                         acct.currency,
                         ENTRY(1, GetXAttrValueEx("acct",
                                                  Surrogate(BUFFER acct:HANDLE),
                                                  "{&AcctO-Type-CrdB}",
                                                  "")),
                         "{&AcctO-Type-CrdB}",
                         iOpDate,
                         {&E-NONE}).
   END.
   FINALLY:
      vHQuery:QUERY-CLOSE().
      DELETE OBJECT vHQuery.
   END FINALLY.
END PROCEDURE.

PROCEDURE AddLocks.
   DEFINE INPUT PARAMETER iAcct     LIKE tt-acct.acct     NO-UNDO.
   DEFINE INPUT PARAMETER iCurrency LIKE tt-acct.currency NO-UNDO.
   DEFINE INPUT PARAMETER iOpDate   LIKE op-date.op-date  NO-UNDO.

   DEFINE BUFFER acct    FOR acct.
   DEFINE BUFFER tt-acct FOR tt-acct.
   DEFINE BUFFER tt-lock FOR tt-lock.

   DEFINE VARIABLE vLockList    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLock        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI           AS INT64     NO-UNDO.
   DEFINE VARIABLE vIsLockedAmt AS LOGICAL   NO-UNDO.

   {find-act.i &acct = iAcct
               &curr = iCurrency}
   IF NOT AVAILABLE acct THEN
      RETURN.
   FIND FIRST tt-acct WHERE
      tt-acct.acct     = iAcct AND
      tt-acct.currency = iCurrency
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tt-acct THEN
      RETURN.
   vLockList = BlockAcct(Surrogate(BUFFER acct:HANDLE), iOpDate).
   DO vI = 1 TO NUM-ENTRIES(vLockList):
      vLock = ENTRY(vI, vLockList).
      IF NOT CAN-FIND(FIRST tt-lock WHERE
                         tt-lock.acct     = tt-acct.acct     AND
                         tt-lock.currency = tt-acct.currency AND
                         tt-lock.type     = vLock
                      NO-LOCK)
      THEN DO TRANSACTION:
         CREATE tt-lock.
         ASSIGN
            tt-lock.acct     = tt-acct.acct
            tt-lock.currency = tt-acct.currency
            tt-lock.type     = vLock
            tt-lock.amt      = IF GetCodeMisc("acct-status",
                                              vLock,
                                              1) = "БлокСумм"
                               THEN ABSOLUTE(GetBlockPositionAll(tt-acct.acct,
                                                                 tt-acct.currency,
                                                                 iOpDate))
                               ELSE ?
         .
         RELEASE tt-lock.
      END.
   END.
END PROCEDURE.

PROCEDURE GetLockList.
   DEFINE INPUT  PARAMETER iAcct     LIKE tt-lock.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency LIKE tt-acct.currency NO-UNDO.
   DEFINE OUTPUT PARAMETER oLockList AS   CHARACTER        NO-UNDO.

   DEFINE BUFFER tt-lock FOR tt-lock.

   FOR EACH tt-lock WHERE
      tt-lock.acct     = iAcct AND
      tt-lock.currency = iCurrency
   NO-LOCK:
      oLockList = oLockList
                  + ", " +
                  tt-lock.type + IF tt-lock.amt = ?
                                 THEN ""
                                 ELSE ("=" + STRING(tt-lock.amt)).
   END.
   oLockList = LEFT-TRIM(oLockList, ", ").
END PROCEDURE.

/* Формирование строки описания причины несписания документа */
FUNCTION strReason RETURN CHARACTER (INPUT iReason AS INT64,
                                     INPUT iStrReason AS CHARACTER):
   CASE iReason:
      WHEN {&E-NONE}    THEN RETURN "Документ не может быть списан, по расчётному счёту установлены ограничения".
      WHEN {&E-NOACCT}  THEN RETURN "Не найден расчётный счёт клиента".
      WHEN {&E-CLOSED}  THEN RETURN "Расчётный счёт клиента закрыт".
      WHEN {&E-STOPPED} THEN RETURN "По документу приостановлено списание".
      WHEN {&E-NOOP} OR
      WHEN {&E-BADKAU}  THEN RETURN "Для формирования расчётного документа нет реквизитов первоначального документа, помещённого в картотеку".
      WHEN {&E-NO47423} THEN RETURN "Счёт не содержится в справочнике РКО47423".
      WHEN {&E-BANKRUPT} THEN RETURN iStrReason.
      WHEN {&E-BADNALSGN} THEN RETURN iStrReason.
   END CASE.
   RETURN "".
END FUNCTION.

PROCEDURE Print.

   DEFINE VARIABLE vName   AS CHARACTER EXTENT {&nn} NO-UNDO.
   DEFINE VARIABLE vWBlock AS CHARACTER EXTENT {&nn} NO-UNDO.
   DEFINE VARIABLE vReason AS CHARACTER EXTENT {&nn} NO-UNDO.
   DEFINE VARIABLE vInt    AS INT64                  NO-UNDO.
   DEFINE VARIABLE vInt1   AS INT64                  NO-UNDO.
   DEFINE VARIABLE vNeedLn AS LOGICAL                NO-UNDO INITIAL YES.
   DEFINE VARIABLE vNeedNm AS LOGICAL                NO-UNDO.

   PUT SKIP(1).
   PUT UNFORMATTED "┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐" SKIP.
   PUT UNFORMATTED "│ Отчет по документам, не списанным с внебалансовых счетов № 90901 (лицевой счет по расчетным документам, ожидающим разрешения на проведение операций) и № 90902               │" SKIP.
   PUT UNFORMATTED "├─────┬───────────────────────────────┬───────────────────────┬──────────────────────┬─────────┬────────────┬─────────────────────┬────────────────────────────────────────────┤" SKIP.
   PUT UNFORMATTED "│  №  │ Краткое наименование клиента  │     Счет клиента      │ Наличие ограничений  │  Номер  │  Дата м/о  │   Сумма остатка     │ Основание, по которому расчетный документ  │" SKIP.
   PUT UNFORMATTED "│     │                               │                       │  на расчетном счете  │    м/о  │            │    платежа по       │          не списан с картотеки             │" SKIP.
   PUT UNFORMATTED "│     │                               │                       │  (тип блокировки)    │         │            │расчетному документу │                                            │" SKIP.
   PUT UNFORMATTED "├─────┼───────────────────────────────┼───────────────────────┼──────────────────────┼─────────┼────────────┼─────────────────────┼────────────────────────────────────────────┤" SKIP.
   PUT UNFORMATTED "│  1  │              2                │           3           │           4          │    5    │      6     │          7          │                    8                       │" SKIP.

   vInt = 1.
   FOR EACH tt-cust NO-LOCK,
   EACH tt-acct WHERE
      tt-acct.cust-cat = tt-cust.cust-cat AND
      tt-acct.cust-id  = tt-cust.cust-id  AND
      (mMode = "*" OR
       mMode = "1" AND CAN-DO(mMaskAcct, tt-acct.acct))
   NO-LOCK
   BY tt-cust.cust-cat BY tt-cust.cust-id:
      IF NOT ({assigned tt-acct.acct-crd2} AND
              CAN-FIND(FIRST tt-opo WHERE
                          tt-opo.accto    = tt-acct.acct-crd2 AND
                          tt-opo.currency = tt-acct.currency
                       NO-LOCK)
              OR
              {assigned tt-acct.acct-crdb} AND
              CAN-FIND(FIRST tt-opo WHERE
                          tt-opo.accto    = tt-acct.acct-crdb AND
                          tt-opo.currency = tt-acct.currency
                       NO-LOCK))
      THEN
         NEXT.
      IF vNeedLn THEN
         {&line4}
      vNeedLn = (tt-cust.cust-cat <> "В").
      vNeedNm = NO.
      vName[1] = tt-cust.cust-name.
      RUN GetLockList(tt-acct.acct, tt-acct.currency, OUTPUT vWBlock[1]).
      {wordwrap.i &s=vName &n={&nn} &l=30}
      {wordwrap.i &s=vWBlock &n={&nn} &l=20}
      PUT UNFORMATTED
         "│" AT 1   STRING(vInt) AT 4
         "│" AT 7   vName[1]     AT 9
         "│" AT 39  tt-acct.acct AT 41
         "│" AT 63  vWBlock[1]   AT 65 
         "│" AT 86
         "│" AT 96
         "│" AT 109
         "│" AT 131
         "│" AT 176
      SKIP.
      vInt = vInt + 1.
      DO vInt1 = 2 TO EXTENT(vName):
         IF vName[vInt1] NE "" OR vWBlock[vInt1] NE "" THEN
         PUT UNFORMATTED
            "│" AT 1
            "│" AT 7   vName[vInt1]   AT 9
            "│" AT 39
            "│" AT 63  vWBlock[vInt1] AT 65
            "│" AT 86
            "│" AT 96
            "│" AT 109
            "│" AT 131
            "│" AT 176
         SKIP.
      END.
      IF {assigned tt-acct.acct-crd2} THEN DO:
         FOR EACH tt-opo WHERE
            tt-opo.currency = tt-acct.currency AND
            tt-opo.accto    = tt-acct.acct-crd2
         NO-LOCK
         BREAK BY tt-opo.amt:
            vReason[1] = strReason(tt-opo.reason, tt-opo.s-reason).
            {wordwrap.i &s=vReason &n={&nn} &l=40}
            IF FIRST(tt-opo.amt) THEN DO:
               PUT UNFORMATTED
                  "│" AT 1 ( IF vNeedNm THEN STRING(vInt) ELSE "") AT 4
                  "│" AT 7
                  "│" AT 39 tt-opo.accto AT 41
               .
               IF vNeedNm THEN
                  vInt = vInt + 1.
               ELSE
                  vNeedNm = NOT vNeedLn.
            END.
            ELSE
               PUT UNFORMATTED
                  "│" AT 1
                  "│" AT 7
                  "│" AT 39
               .
            PUT UNFORMATTED
               "│" AT 63
               "│" AT 86  TRIM(tt-opo.doc-num)                    TO 94
               "│" AT 96  STRING(tt-opo.doc-date,"99/99/9999")    AT 98
               "│" AT 109 STRING(tt-opo.amt,">>>,>>>,>>>,>>9.99") AT 111
               "│" AT 131 vReason[1]                              AT 133
               "│" AT 176
            SKIP.
            DO vInt1 = 2 TO EXTENT(vReason):
               IF vReason[vInt1] NE "" THEN
               PUT UNFORMATTED
                  "│" AT 1
                  "│" AT 7
                  "│" AT 39
                  "│" AT 63
                  "│" AT 86
                  "│" AT 96
                  "│" AT 109
                  "│" AT 131 vReason[vInt1] AT 133
                  "│" AT 176
               SKIP.
            END.
         END.
         IF tt-acct.cust-cat = "В" THEN
            {&line4}
      END.
      IF {assigned tt-acct.acct-crdb} THEN DO:
         FOR EACH tt-opo WHERE
            tt-opo.currency = tt-acct.currency AND
            tt-opo.accto    = tt-acct.acct-crdb
         NO-LOCK
         BREAK BY tt-opo.amt:
            vReason[1] = strReason(tt-opo.reason, tt-opo.s-reason).
            {wordwrap.i &s=vReason &n={&nn} &l=40}
            IF FIRST(tt-opo.amt) THEN DO:
               PUT UNFORMATTED
                  "│" AT 1 ( IF vNeedNm THEN STRING(vInt) ELSE "") AT 4
                  "│" AT 7
                  "│" AT 39 tt-opo.accto AT 41
               .
               IF vNeedNm THEN
                  vInt = vInt + 1.
               ELSE
                  vNeedNm = NOT vNeedLn.
            END.
            ELSE
               PUT UNFORMATTED
                  "│" AT 1
                  "│" AT 7
                  "│" AT 39
               .
            PUT UNFORMATTED
               "│" AT 63
               "│" AT 86 TRIM(tt-opo.doc-num)                     TO 94
               "│" AT 96  STRING(tt-opo.doc-date,"99/99/9999")    AT 98
               "│" AT 109 STRING(tt-opo.amt,">>>,>>>,>>>,>>9.99") AT 111
               "│" AT 131 vReason[1]                              AT 133
               "│" AT 176
            SKIP.
            DO vInt1 = 2 TO EXTENT(vReason):
               IF vReason[vInt1] NE "" THEN
               PUT UNFORMATTED
                  "│" AT 1
                  "│" AT 7
                  "│" AT 39
                  "│" AT 63
                  "│" AT 86
                  "│" AT 96
                  "│" AT 109
                  "│" AT 131 vReason[vInt1] AT 133
                  "│" AT 176
               SKIP.
            END.
         END.
         IF tt-acct.cust-cat = "В" THEN
            {&line4}
      END.
   END.

   {&line6}
   
   PUT SKIP(1).
   
END PROCEDURE.

FINALLY:
   IF VALID-HANDLE(mInstance) THEN
      RUN DelEmptyInstance IN h_data (mInstance).
   {intrface.del}
END FINALLY.
/* $LINTENV ='rshb' */
/* $LINTVSS ='$/ws3-dpl/rshb/bq/' */
/* $LINTDATE='30/03/2016 07:24:51.940+04:00' */
/* $LINTUSER='paus' */
/* $LINTMODE='1' */
/* $LINTFILE='rep-k1k2.p' */
/*prosignk7Qubms2PdXQk4tOABA9qg*/
/* --- rep-k1k2.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am --- */
