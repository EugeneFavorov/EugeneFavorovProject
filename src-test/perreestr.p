/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: perreestr.p
      Comment: Перечисление реестров платежей
   Parameters: iReesTableHdl, iOpDate
         Uses:
      Used by:
      Created: 21.10.2009 ushd 110129
     Modified: 
     Modified: 07.05.2014 guva 0215822
*/
DEFINE INPUT PARAMETER iReesTableHdl AS CHAR NO-UNDO. /* Созданные реестры */
DEFINE INPUT PARAMETER iOpDate       AS DATE NO-UNDO. /* Дата опердня */

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get refer}
{intrface.get op}
{intrface.get pbase}
{intrface.get ppn}
{intrface.get trans}
{intrface.get count}
{intrface.get parsr}
{intrface.get pn}
{exchange.equ}

&GLOBAL-DEFINE NO-BASE-PROC YES
&GLOBAL-DEFINE NalAttrs "ПокДД,ПокНД,ПокНП,ПокОП,ПокСт,ПокТП,КБК,ОКАТО-НАЛОГ,п106н_СтатПлат"
&GLOBAL-DEFINE ParsAttrs "ПокДД,ПокНД,ПокНП,ПокОП,ПокСт,ПокТП,КБК,ОКАТО-НАЛОГ"

DEFINE VAR mReesTable      AS HANDLE    NO-UNDO.
DEFINE VAR mReesBuffer     AS HANDLE    NO-UNDO.
DEFINE VAR mQuery          AS HANDLE    NO-UNDO.
DEFINE VAR mIsOk           AS LOGICAL.
DEFINE VAR mContract       AS CHAR      NO-UNDO.
DEFINE VAR mContCode       AS CHAR      NO-UNDO.
DEFINE VAR mLinkedOps      AS CHAR      NO-UNDO.
DEFINE VAR mInt            AS INT64     NO-UNDO.
DEFINE VAR mSeparate       AS LOGICAL   NO-UNDO.
DEFINE VAR mTypPl          AS CHAR      NO-UNDO.
DEFINE VAR mInternal       AS LOGICAL   NO-UNDO.
DEFINE VAR mDocNum         AS CHAR      NO-UNDO.
DEFINE VAR mAcctDb         AS CHAR      NO-UNDO.
DEFINE VAR mAcctCr         AS CHAR      NO-UNDO.
DEFINE VAR mLinkSurr       AS CHAR      NO-UNDO.
DEFINE VAR mBIK            AS CHAR      NO-UNDO.
DEFINE VAR mBenAcct        AS CHAR      NO-UNDO.
DEFINE VAR mBenName        AS CHAR      NO-UNDO.
DEFINE VAR mKPPSend        AS CHAR      NO-UNDO.
DEFINE VAR mINNSend        AS CHAR      NO-UNDO.
DEFINE VAR mNameSend       AS CHAR      NO-UNDO.
DEFINE VAR mNameSendSp     AS CHAR      NO-UNDO.
DEFINE VAR mNameSendId     AS CHAR      NO-UNDO.
DEFINE VAR mCountry        AS CHAR      NO-UNDO.
DEFINE VAR mPaspType       AS CHAR      NO-UNDO.
DEFINE VAR mPaspNum        AS CHAR      NO-UNDO.
DEFINE VAR mDetails        AS CHAR      NO-UNDO.
DEFINE VAR mKPP            AS CHAR      NO-UNDO.
DEFINE VAR mINN            AS CHAR      NO-UNDO.
DEFINE VAR mLastOp         AS INT64     NO-UNDO.
DEFINE VAR mLAOrder        AS INT64     NO-UNDO.
DEFINE VAR mLASurr         AS CHAR      NO-UNDO.
DEFINE VAR mLARwd          AS ROWID     NO-UNDO.
DEFINE VAR mUniqNumber     AS CHAR      NO-UNDO.
DEFINE VAR mParSurr        AS CHAR      NO-UNDO.
DEFINE VAR mTransSum       AS DEC       NO-UNDO.
DEFINE VAR mComSum         AS DEC       NO-UNDO.
DEFINE VAR mOpSum          AS DEC       NO-UNDO.
DEFINE VAR mParsOk         AS INT64     NO-UNDO.
DEFINE VAR mOrderPay       AS CHAR      NO-UNDO.
DEFINE VAR mDefaultOrd     AS CHAR      NO-UNDO.
DEFINE VAR mDRVal          AS CHAR      NO-UNDO.
DEFINE VAR mDRValPars      AS CHAR      NO-UNDO.
DEFINE VAR mAnyNal         AS LOGICAL   NO-UNDO.
DEFINE VAR flager          AS INT64     NO-UNDO.
DEFINE VAR mBreakMaxFrm    AS CHAR      NO-UNDO.
DEFINE VAR mBreakVal       AS CHAR      NO-UNDO.
DEFINE VAR mBreakStr       AS CHAR      NO-UNDO.
DEFINE VAR mDocTemplExt    AS HANDLE    NO-UNDO.
DEFINE VAR mDocTemplInt    AS HANDLE    NO-UNDO.
DEFINE VAR mDocTemplUse    AS HANDLE    NO-UNDO.
DEFINE VAR mNewDocHdl      AS HANDLE    NO-UNDO.
DEFINE VAR mNumDocs        AS INT64     NO-UNDO.
DEFINE VAR mFldHdl         AS HANDLE    NO-UNDO.
DEFINE VAR mTmplId         AS INT64     NO-UNDO.
DEFINE VAR mT-loan-doc-num AS CHAR      NO-UNDO.
DEFINE VAR mStr_err        AS CHAR      NO-UNDO.
DEFINE VAR mF107Hbos       AS LOG       NO-UNDO. /* 107н Приложение 1 */
DEFINE VAR mFillSend       AS CHARACTER NO-UNDO.
DEFINE VAR mFlErr          AS LOGICAL   NO-UNDO.
DEFINE VAR mOutRes         AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE mtDocs
   FIELD contract   AS CHARACTER
   FIELD cont-code  AS CHARACTER
   FIELD op         AS INT64
   FIELD branch-id  AS CHARACTER
   FIELD BreakList  AS CHAR
   FIELD BreakVal   AS CHAR
INDEX ByPar contract cont-code BreakVal
INDEX ByOp op.
DEFINE TEMP-TABLE mtDocsPar
   FIELD contract   AS CHARACTER
   FIELD cont-code  AS CHARACTER
   FIELD CalcComSum AS LOGICAL INIT YES
   FIELD Tarif      AS DECIMAL
   FIELD TarifType  AS LOGICAL
INDEX ByPar contract cont-code.
DEFINE TEMP-TABLE mtCommonDR
   FIELD xattr-code AS CHARACTER
   FIELD Val        AS CHAR
   FIELD ValPars    AS CHAR
.
DEFINE BUFFER mxacct        FOR acct.
DEFINE BUFFER mxop          FOR op.
DEFINE BUFFER mxop-com      FOR op.
DEFINE BUFFER mxop-pay      FOR op.
DEFINE BUFFER mxop-entry    FOR op-entry.
DEFINE BUFFER mxsigns       FOR signs.
DEFINE BUFFER mxloan-reestr FOR loan.
DEFINE BUFFER mxloan-par    FOR loan.
DEFINE BUFFER mxloan-main   FOR loan.
DEFINE BUFFER mxloan-acct   FOR loan-acct.
DEFINE BUFFER mxxcode       FOR code.
DEFINE BUFFER mxperson      FOR person.
DEFINE BUFFER mxtDocs       FOR mtDocs.
DEFINE BUFFER mxxattr1      FOR xattr.
DEFINE BUFFER mxxattr2      FOR xattr.
DEFINE BUFFER mlinks        FOR links.
DEFINE BUFFER mxlinks       FOR links.
DEFINE BUFFER mxxlinks      FOR links.
DEFINE BUFFER mxlink        FOR xlink.
DEFINE BUFFER mxxlink       FOR xlink.

MAIN:
DO TRANSACTION ON ERROR  UNDO MAIN, RETRY MAIN
               ON ENDKEY UNDO MAIN, RETRY MAIN
               ON STOP   UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   FIND FIRST mxxcode WHERE mxxcode.class = "order-pay"
                       AND mxxcode.code  = "5"
   NO-LOCK NO-ERROR.
   IF AVAIL mxxcode THEN mDefaultOrd = mxxcode.code.
   ELSE DO:
      FIND FIRST mxxcode WHERE mxxcode.class = "order-pay"
                          AND mxxcode.code  = "05"
      NO-LOCK NO-ERROR.
      IF AVAIL mxxcode THEN mDefaultOrd = mxxcode.code.
   END.

   mDocTemplExt = GetTransObjectN("opbp-transfer",1) NO-ERROR.
   IF VALID-HANDLE(mDocTemplExt) 
      THEN 
      mDocTemplExt = mDocTemplExt:DEFAULT-BUFFER-HANDLE NO-ERROR.
   IF NOT VALID-HANDLE(mDocTemplExt) 
      THEN 
   DO:
      RUN Fill-SysMes("","","-1",
                      "В транзакции не найден объект класса opbp-transfer " +
                      "для внешних платежей (первый opbp-transfer).").
      UNDO MAIN, LEAVE MAIN.
   END.

   mDocTemplInt = GetTransObjectN("opbp-transfer",2) NO-ERROR.
   IF VALID-HANDLE(mDocTemplInt) 
      THEN 
      mDocTemplInt = mDocTemplInt:DEFAULT-BUFFER-HANDLE NO-ERROR.
   IF NOT VALID-HANDLE(mDocTemplInt) 
      THEN 
   DO:
      RUN Fill-SysMes("","","-1","В транзакции не найден объект класса " +
                      "opbp-transfer для внутренних платежей " + 
                      "(второй opbp-transfer).").
      UNDO MAIN, LEAVE MAIN.
   END.
   
   FIND FIRST param-op-kind   WHERE
            param-op-kind.op-kind    EQ "ПНПерРс1"
      AND   param-op-kind.param-code EQ "FillSend"
   NO-LOCK NO-ERROR.
   IF AVAIL(param-op-kind) 
   THEN mFillSend = param-op-kind.param-value.
   ELSE mFillSend = "Да".

   mReesTable = WIDGET-HANDLE(iReesTableHdl) NO-ERROR. {&ON-ERROR}
   mReesBuffer = mReesTable:DEFAULT-BUFFER-HANDLE.
   CREATE QUERY mQuery.
   mQuery:SET-BUFFERS(mReesBuffer).
   mQuery:QUERY-PREPARE("FOR EACH " + mReesBuffer:NAME).
   mQuery:QUERY-OPEN().
   mQuery:GET-FIRST().
   REPEAT:
      IF mQuery:QUERY-OFF-END THEN LEAVE.
      ASSIGN
         mContract = mReesBuffer:BUFFER-FIELD("contract"):BUFFER-VALUE
         mContCode = mReesBuffer:BUFFER-FIELD("cont-code"):BUFFER-VALUE
      NO-ERROR. {&ON-ERROR}
      FIND FIRST mxloan-reestr WHERE mxloan-reestr.contract   = mContract
                                AND mxloan-reestr.cont-code  = mContCode
                                AND mxloan-reestr.class-code = "pn-reestr"
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR. {&ON-ERROR}
      mLinkedOps = GetLinks(mxloan-reestr.class-code,
                            GetSurrogateBuffer("loan",
                                               (BUFFER mxloan-reestr:HANDLE)),
                            "t",
                            "reestr_pay",
                            CHR(1),
                            {&BQ-MAX-DATE}).
      mt-loan-doc-num = mxloan-reestr.doc-num.
      {empty mtDocs}
      {empty mtDocsPar}
      {empty mtCommonDR}
      DO mInt = 1 TO NUM-ENTRIES(mLinkedOps,CHR(1)):
         FIND FIRST mxop 
            WHERE mxop.op = INT64(ENTRY(mInt,mLinkedOps,CHR(1))) 
            NO-LOCK NO-ERROR.
         IF AVAIL mxop THEN DO:
            mParSurr = GetXAttrValue("op",
                                     STRING(mxop.op),
                                     "ПНКодПараметраПлатежа").
            IF NUM-ENTRIES(mParSurr) <> 2 THEN DO:
               RUN Fill-SysMes("","","-1","В документе №" + 
                               GetNullStr(mxop.doc-num) + 
                               " от " + 
                               GetNullStr(STRING(mxop.op-date)) + 
                               " неверно указан код параметра платежа.").
               UNDO MAIN, LEAVE MAIN.
            END.
            CREATE mtDocs.
            ASSIGN
               mtDocs.contract   = ENTRY(1,mParSurr)
               mtDocs.cont-code  = ENTRY(2,mParSurr)
               mtDocs.op         = mxop.op
               mtDocs.branch-id  = mxop.branch-id
            .
            IF NOT CAN-FIND(FIRST mtDocsPar 
                            WHERE mtDocsPar.contract EQ mtDocs.contract
                                             AND mtDocsPar.cont-code EQ mtDocs.cont-code 
                            NO-LOCK)
            THEN DO:
               CREATE mtDocsPar.
               ASSIGN
                  mtDocsPar.contract  = mtDocs.contract
                  mtDocsPar.cont-code = mtDocs.cont-code
               .
               RELEASE mtDocsPar.
            END.
            RELEASE mtDocs.
         END.
      END.
   
      RUN crepncomm.p ((TEMP-TABLE mtDocs:HANDLE),
                       GetSurrogateBuffer("loan",(BUFFER mxloan-reestr:HANDLE)),
                       iOpDate
                      ) NO-ERROR. {&ON-ERROR}

      /* Группировка */
      FOR EACH mtDocsPar NO-LOCK:
         mTypPl = GetXAttrValue("loan",
                                mtDocsPar.contract + "," + mtDocsPar.cont-code,
                                "ПНВидПлатежа").
         IF {assigned mTypPl} THEN DO:
            FIND FIRST mxxcode WHERE mxxcode.class = "ПНВидПлатежа"
                                AND mxxcode.code  = mTypPl
            NO-LOCK NO-ERROR.
            IF AVAIL mxxcode
            AND {assigned mxxcode.description[3]} THEN DO:
               mBreakMaxFrm = "x(" + STRING(INT64(150 / NUM-ENTRIES(mxxcode.description[3]))) + ")".
               FOR EACH mtDocs WHERE mtDocs.contract  = mtDocsPar.contract
                                AND mtDocs.cont-code = mtDocsPar.cont-code
               EXCLUSIVE-LOCK,
               FIRST op WHERE op.op = mtDocs.op NO-LOCK BY ROWID(mtDocs):
                  mBreakStr = "".
                  DO mInt = 1 TO NUM-ENTRIES(mxxcode.description[3]):
                     FIND FIRST mxxattr1 WHERE mxxattr1.class      = op.class-code
                                          AND mxxattr1.xattr-code = ENTRY(mInt,mxxcode.description[3])
                     NO-LOCK NO-ERROR.
                     IF AVAIL mxxattr1 THEN DO:
                        IF mxxattr1.Progress-Field THEN DO:
                           mBreakVal = "".
                           mBreakVal = BUFFER op:BUFFER-FIELD(mxxattr1.xattr-code):BUFFER-VALUE NO-ERROR.
                        END.
                        ELSE mBreakVal = GetXAttrValue("op",STRING(op.op),mxxattr1.xattr-code).
                     END.
                     ELSE mBreakVal = "".
                     IF NOT {assigned mBreakVal}
                     THEN mBreakVal = GetXAttrValue("code","ПНВидПлатежа," + mTypPl,"PARSSEN_" + ENTRY(mInt,mxxcode.description[3])).
                     IF mBreakVal = ? THEN mBreakVal = "?".
                     mBreakStr = mBreakStr + STRING(mBreakVal,mBreakMaxFrm).
                  END.
                  ASSIGN
                     mtDocs.BreakList = mxxcode.description[3]
                     mtDocs.BreakVal  = mBreakStr
                  .
               END.
            END.
         END.

         RUN PNGetParComis(mtDocsPar.contract, 
                           mtDocsPar.cont-code, 
                           iOpDate,
                           OUTPUT mtDocsPar.Tarif,
                           OUTPUT mtDocsPar.TarifType).

         IF mtDocsPar.Tarif > 0.0
         THEN _sel_com_meth: DO:
            /* Определение правила съема комиссии */
            FOR EACH mtDocs WHERE mtDocs.contract  = mtDocsPar.contract
                             AND mtDocs.cont-code = mtDocsPar.cont-code
            NO-LOCK,
            FIRST mxop WHERE mxop.op = mtDocs.op NO-LOCK,
            FIRST mxlink WHERE mxlink.class-code     = mxop.class-code
                          AND mxlink.link-code      = "pn_doc_com"
                          AND mxlink.link-direction = "s"
            NO-LOCK,
            EACH mlinks WHERE mlinks.link-id   = mxlink.link-id
                         AND mlinks.source-id = STRING(mtDocs.op)
            NO-LOCK BREAK BY mlinks.target-id:
               IF FIRST-OF(mlinks.target-id) THEN DO:
                  FIND FIRST mxop-com WHERE mxop-com.op = INT64(mlinks.target-id) NO-LOCK NO-ERROR.
                  IF AVAIL mxop-com THEN DO:
                     FIND FIRST mxxlink WHERE mxxlink.class-code     = mxop-com.class-code
                                         AND mxxlink.link-code      = "pn_doc_com"
                                         AND mxxlink.link-direction = "t"
                     NO-LOCK NO-ERROR.
                     IF AVAIL mxxlink THEN DO:
                        mtDocsPar.CalcComSum = NO.
                        FOR EACH mxlinks WHERE mxlinks.link-id   = mxlink.link-id
                                          AND mxlinks.target-id = mlinks.target-id
                        NO-LOCK:
                           IF NOT CAN-FIND(FIRST mtDocs WHERE mtDocs.op = INT64(mxlinks.source-id)
                                                         AND mtDocs.contract  = mtDocsPar.contract
                                                         AND mtDocs.cont-code = mtDocsPar.cont-code
                                           NO-LOCK)
                           THEN DO:
                              mtDocsPar.CalcComSum = YES.
                              LEAVE _sel_com_meth.
                           END.
                        END.
                     END.
                  END.
               END.
            END.
         END.
      END.

      _pars:
      FOR EACH mtDocsPar NO-LOCK,
      EACH mtDocs WHERE mtDocs.contract  = mtDocsPar.contract
                   AND mtDocs.cont-code = mtDocsPar.cont-code
      NO-LOCK,
      FIRST mxop WHERE mxop.op = mtDocs.op NO-LOCK
      BREAK BY mtDocs.contract BY mtDocs.cont-code BY mtDocs.BreakVal:

         mTransSum = 0.0.
         FOR EACH mxop-entry OF mxop NO-LOCK:
            mTransSum = mTransSum + mxop-entry.amt-rub.
         END.
         IF NOT mTransSum > 0 THEN NEXT _pars.

         FIND FIRST mxloan-par WHERE mxloan-par.contract   = mtDocs.contract
                                 AND mxloan-par.cont-code  = mtDocs.cont-code
                                 AND mxloan-par.class-code = "loan-pn-par"
         NO-LOCK NO-ERROR.
         IF FIRST-OF(mtDocs.contract)
         OR FIRST-OF(mtDocs.cont-code)
         THEN DO:
            FIND FIRST mxloan-main WHERE mxloan-main.contract   = mxloan-par.parent-contract
                                     AND mxloan-main.cont-code  = mxloan-par.parent-cont-code
                                     AND mxloan-main.class-code = "loan-pn"
            NO-LOCK NO-ERROR.
            mUniqNumber = GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER mxloan-par:HANDLE)),"ПНСчет",?).
            IF {assigned mUniqNumber}
            THEN RUN FindSignsByVal IN h_xclass ("loan-acct", "УникКодСчета", mUniqNumber, OUTPUT mLAOrder, OUTPUT mLASurr).
            mLARwd = ?.
            IF {assigned mLASurr} THEN mLARwd = GetRowidBySurrogate("loan-acct", mLASurr).
            FIND FIRST mxloan-acct WHERE ROWID(mxloan-acct)    = mLARwd
                                    AND mxloan-acct.contract  = "pn"
                                    AND mxloan-acct.acct-type = "ПлатНас"
            NO-LOCK NO-ERROR.
            /* р/с */
            mBenAcct = mxloan-acct.acct.
            /* БИК */
            mBIK = GetXAttrValue("loan-acct",
                                 GetSurrogateBuffer("loan-acct",(BUFFER mxloan-acct:HANDLE)),
                                 "БИК").
            /* обслуживание в том же банке или в другом */
            mInternal = (mBIK = FGetSetting("БанкМФО", "", ?)).
            IF mInternal THEN mAcctCr = mBenAcct.
            ELSE DO:
               mAcctCr = FGetSetting("КорСчПН", "", "").
               IF {assigned mAcctCr} THEN DO:
                  {find-act.i &bact=mxacct &acct=mAcctCr}
                  IF NOT AVAIL mxacct THEN DO:
                     RUN Fill-SysMes("", "", "-1",
                                     "В настроечном параметре КорСчПН указан несуществующий счет.").
                     UNDO MAIN, LEAVE MAIN.
                  END.
               END.
               ELSE DO:
                  mAcctCr = FGetSetting("КорСч", "", "").
                  IF NOT mAcctCr BEGINS "30102" THEN DO:
                     mAcctCr = "".
                     FIND FIRST mxacct WHERE mxacct.bal-acct = 30102
                                        AND mxacct.currency = ""
                     NO-LOCK NO-ERROR.
                     mAcctCr = mxacct.acct.
                  END.
               END.
            END.
            /* клиент */
            mBenName = GetXAttrValue("loan",mxloan-main.contract + "," + mxloan-main.cont-code,"ПНПолучательНаименование").
            mKPP     = GetXAttrValue("loan",mxloan-main.contract + "," + mxloan-main.cont-code,"ПНПолучательКПП").
            mINN     = GetXAttrValue("loan",mxloan-main.contract + "," + mxloan-main.cont-code,"ПНПолучательИНН").

            /* Отдельными суммами */
            mSeparate = (GetXAttrValue("loan",mxloan-par.contract + "," + mxloan-par.cont-code,"ПНОтдСуммами") = "Да").
            /* вид платежа */
            mTypPl    = GetXAttrValue("loan",mxloan-par.contract + "," + mxloan-par.cont-code,"ПНВидПлатежа").

            /* счет дебета */
            mAcctDb   = GetRefVal("СчетаПодразд", iOpDate, mTypPl + CHR(44) + mxloan-reestr.branch-id + CHR(44) + "Транзитный").
            IF {assigned mAcctDb} THEN DO:
               _fnd_by_mask:
               FOR EACH mxacct WHERE mxacct.acct-cat   = "b"
                                AND CAN-DO(mAcctDb,mxacct.number)
                                AND mxacct.currency   = ""
                                AND mxacct.filial-id  = shFilial
                                AND mxacct.open-date <= iOpDate
                                AND mxacct.close-date = ?
               NO-LOCK:
                  LEAVE _fnd_by_mask.
               END.
               IF AVAIL mxacct
               THEN mAcctDb = mxacct.acct.
               ELSE mAcctDb = "".
            END.
            IF NOT {assigned mAcctDb} THEN DO:
               RUN Fill-SysMes("","","-1","Невозможно определить транзитный счет (для дебета) по виду платежа """ + GetNullStr(mTypPl) + """ и подразделению """ + mxloan-reestr.branch-id + """").
               UNDO MAIN, LEAVE MAIN.
            END.
            {find-act.i &bact=mxacct &acct=mAcctDb}
            IF NOT AVAIL mxacct THEN DO:
               RUN Fill-SysMes("","","-1","Невозможно найти транзитный счет (для дебета) по виду платежа """ + GetNullStr(mTypPl) + """ и подразделению """ + mxloan-reestr.branch-id + """").
               UNDO MAIN, LEAVE MAIN.
            END.

/*            mINNSend = GetXAttrValueEx("op",
                                       STRING(mxop.op),
                                       "ПНИНН",
                                       "NO"
                                       ).
/*            if mINNSend and ((mxop.inn <> "" or mxop.inn <> ? ) and lenght(mxop.inn) = 12) then do:
*/

            if mINNSend  EQ "NO" and (mxop.inn <> ? or mxop.inn <> "") and length(mxop.inn) = 12 then do:
               mINNSend = mxop.inn.
            end.
            mF107Hbos = mSeparate /* AND 
                       (mBenAcct BEGINS "40101") */. 

            IF     mINNSend EQ "NO" 
               AND mF107Hbos EQ NO
            THEN
               mINNSend = GetXAttrValueEx("branch",mxacct.branch-id,"ИНН",FGetSetting("ИНН","","")).
*/
            mKPPSend = GetXAttrValueEx("branch",mxacct.branch-id,"КПП",FGetSetting("БанкКПП","","")).

            /* Очередность платежа */
            mOrderPay = GetXAttrValueEx("code","ПНВидПлатежа," + mTypPl,"ПНОчерПлат",mDefaultOrd).
         END.

         IF mSeparate
         OR FIRST-OF(mtDocs.contract)
         OR FIRST-OF(mtDocs.cont-code)
         OR FIRST-OF(mtDocs.BreakVal)
         THEN DO:

            mINNSend = GetXAttrValueEx("op",
                                       STRING(mxop.op),
                                       "ПНИНН",
                                       "NO"
                                       ).
            if mINNSend  EQ "NO" and (mxop.inn <> ? or mxop.inn <> "") and length(mxop.inn) = 12 then do:
               mINNSend = mxop.inn.
            end.
            mF107Hbos = mSeparate /* AND 
                       (mBenAcct BEGINS "40101") */. 

            IF     mINNSend EQ "NO" 
               AND mF107Hbos EQ NO
            THEN
               mINNSend = GetXAttrValueEx("branch",mxacct.branch-id,"ИНН",FGetSetting("ИНН","","")).


            IF mF107Hbos THEN 
            DO:
               mKPPSend = "0".
               IF mINNSend EQ "NO" THEN 
                  mINNSend = "0".
            END.

            mNumDocs = 0.
            /* Плательщик */
            ASSIGN
               mNameSend   = dept.name-bank
               mNameSendSp = ""
            .
            IF mSeparate THEN DO:
               RELEASE mxperson.
               ASSIGN
                  mPaspType = ""
                  mPaspNum  = ""
                  mCountry  = ""
               .
               mPaspType = GetXAttrValueEx("op",STRING(mxop.op),"document-id","").
               IF {assigned mPaspType} THEN mPaspNum = GetXAttrValueEx("op",STRING(mxop.op),"Докум","").
               IF {assigned mPaspNum}  THEN mCountry = GetXAttrValueEx("op",STRING(mxop.op),"country-pers","").
               IF     {assigned mPaspType}
                  AND {assigned mPaspNum}
               AND {assigned mCountry} THEN DO:
                  FIND FIRST mxperson WHERE mxperson.country-id  = mCountry
                                        AND mxperson.document-id = mPaspType
                                        AND mxperson.document    = mPaspNum
                                      NO-LOCK NO-ERROR.
               END.
               mDetails = mxop.details.

               RUN PARSFUNC-НаимПлат IN h_ppn (mxop.op, ?, OUTPUT mNameSendSp, OUTPUT mParsOk).
               IF mParsOk <> 0 THEN DO:
                  RUN Fill-SysMes("","","-1","Ошибка парсерной функции НаимПлат").
                  UNDO MAIN, LEAVE MAIN.
               END.
               IF NOT {assigned mNameSendSp} THEN DO:
                  mNameSendSp = GetXAttrValueEx("op",STRING(mxop.op),"ФИО","").
                  IF     {assigned mxop.inn} 
                     AND mF107Hbos EQ NO
                  THEN 
                     mNameSendId = mxop.inn.
                  ELSE
                     mNameSendId = GetXAttrValueEx("op",STRING(mxop.op),"Адрес","").
                  IF  NOT {assigned mNameSendId}
                  AND AVAIL mxperson THEN DO:
                     /* адреса нет, берем адрес с клиента */
                     mNameSendId = mxperson.address[1].
                  END.
                  IF NOT {assigned mNameSendId} THEN DO:
                     /* адреса совсем нет , пробуем место и дату рождения */
                     mNameSendId = STRING(DATE(GetXAttrValueEx("op",
                                                               STRING(mxop.op),
                                                               "BirthDay",
                                                               ?
                                                               )
                                               ),"99.99.9999") + " " + 
                        GetXAttrValueEx("op",STRING(mxop.op),"BirthPlace",?).
                     /* рождения нет, берем рождение с клиента */
                     IF  NOT {assigned mNameSendId}
                     AND AVAIL mxperson THEN DO:
                        mNameSendId = STRING(mxperson.birthday,"99.99.9999") + 
                           " " + 
                           GetXAttrValueEx("person",
                                           STRING(mxperson.person-id),
                                           "BirthPlace",
                                           ?
                                           ).
                     END.
                  END.
                  IF mNameSendId = ? THEN mNameSendId = "".
                  mNameSend = mNameSend + " //" + mNameSendSp + "//" + mNameSendId + "//".
               END.
               ELSE mNameSend = mNameSendSp.
            END.

            mOpSum = 0.0.
            CREATE op.
            mNewDocHdl = (BUFFER op:HANDLE).
            mDocTemplUse = (IF mInternal THEN mDocTemplInt ELSE mDocTemplExt).
            op.class-code = mDocTemplUse::class-code.

            /* Копируем основные реквизиты */
            mNewDocHdl:BUFFER-COPY(mDocTemplUse,"op").
            ASSIGN
               op.branch-id = mxloan-reestr.branch-id
               op.ben-acct  = mBenAcct
               op.order-pay = mOrderPay
               op.name-ben  = mBenName
               op.inn       = mINN
            .

            IF {assigned mDocTemplUse::pnswcetwciknomera$} /* ПНСчетчикНомера */ THEN DO:
               op.doc-num = STRING(GetCounterNextValue(mDocTemplUse::pnswcetwciknomera$, op.op-date)).
               IF op.doc-num = ? THEN DO:
                  RUN Fill-SysMes IN h_tmess ("","","-1","Невозможно получить значение счетчика " + mDocTemplUse::pnswcetwciknomera$ + ".").
                  UNDO MAIN, LEAVE MAIN.
               END.
            END.
            ELSE DO:
               IF NOT {assigned op.doc-num} THEN DO:
                  {auto-num.i &DefCount  = "op.op"
                              &DefNum    = "''"
                              &DateCount = "iOpDate"
                              &AssignVar = "mDocNum"
                  }
                  op.doc-num = mDocNum.
               END.
            END.

            /* Копируем доп. реквизиты и переопределенные основные */
            mTmplId = ?.
            mTmplId = mDocTemplUse:BUFFER-FIELD("__tmpl-id"):BUFFER-VALUE NO-ERROR.
            IF mTmplId > 0
            THEN
            FOR EACH op-kind-tmpl-ln WHERE op-kind-tmpl-ln.tmpl-id = mTmplId NO-LOCK,
            FIRST mxxattr1 WHERE mxxattr1.class-code    = op.class-code
                            AND mxxattr1.xattr-code    = op-kind-tmpl-ln.xattr-code
                            AND mxxattr1.xattr-code    <> "op"
                            AND mxxattr1.xattr-code    <> "ПНСчетчикНомера"
                            AND NOT CAN-DO("class,group",mxxattr1.data-type)
            NO-LOCK
            BY op-kind-tmpl-ln.order:
               mFldHdl = mDocTemplUse:BUFFER-FIELD(GetMangledName(mxxattr1.xattr-code)) NO-ERROR.
               IF  mFldHdl       <> ?
               AND mFldHdl:EXTENT = 0 THEN DO:
                  IF mxxattr1.Progress-Field = YES THEN DO:
                     BUFFER op:BUFFER-FIELD(mFldHdl:NAME):BUFFER-VALUE = mFldHdl:BUFFER-VALUE.
                  END.
                  ELSE IF mxxattr1.Progress-Field = NO THEN DO:
                     IF {assigned mFldHdl:BUFFER-VALUE}
                     THEN UpdateSigns(op.class-code, STRING(op.op), mxxattr1.xattr-code, mFldHdl:BUFFER-VALUE, ?).
                  END.
               END.
            END.

            VALIDATE op NO-ERROR. {&ON-ERROR}

            mAnyNal = NO.
            IF mSeparate THEN DO:
               FOR EACH mxxattr1 WHERE mxxattr1.class-code = op.class
                                  AND CAN-DO({&NalAttrs},mxxattr1.xattr-code)
               NO-LOCK:
                  mDRVal = GetXAttrValueEx("op",STRING(mxop.op),mxxattr1.xattr-code,"").
                  IF {assigned mDRVal} THEN DO:
                     mAnyNal = YES.
                     IF NOT UpdateSigns(op.class-code, STRING(op.op), mxxattr1.xattr-code, mDRVal, ?) THEN DO:
                        RUN Fill-SysMes("","","-1","Ошибка реквизита " + mxxattr1.xattr-code + ".").
                        UNDO MAIN, LEAVE MAIN.
                     END.
                  END.
               END.
            END.
            /**/
            IF mFillSend EQ "Да" THEN
            DO:
               IF {assigned mNameSend} THEN DO:
                  IF NOT UpdateSigns(op.class-code, STRING(op.op), "name-send", mNameSend, ?) THEN DO:
                     RUN Fill-SysMes("","","-1","Ошибка реквизита name-send.").
                     UNDO MAIN, LEAVE MAIN.
                  END.
                  IF NOT UpdateSigns(op.class-code, STRING(op.op), "acct-send", DelFilFromAcct(mAcctDb), ?) THEN DO:
                     RUN Fill-SysMes("","","-1","Ошибка реквизита acct-send.").
                     UNDO MAIN, LEAVE MAIN.
                  END.
               END.
               /* inn-send */

               IF {assigned mINNSend} THEN DO:
                  IF NOT UpdateSigns(op.class-code, STRING(op.op), "inn-send", mINNSend , ?) THEN DO:
                     RUN Fill-SysMes("","","-1","Ошибка реквизита inn-send.").
                     UNDO MAIN, LEAVE MAIN.
                  END.
               END.
            END.

            /* при обслуживании в том же самом банке op-bank не создаем */
            IF NOT mInternal THEN DO:
               RELEASE banks-corr.
               FIND FIRST banks-code WHERE banks-code.bank-code-type = "МФО-9"
                                       AND banks-code.bank-code      = STRING(INT64(mBIK),"999999999")
               NO-LOCK NO-ERROR.
               IF AVAIL banks-code
               THEN FIND FIRST banks-corr WHERE banks-corr.bank-corr = banks-code.bank-id
                                            AND CAN-FIND(FIRST banks OF banks-corr WHERE banks.flag-rkc = YES NO-LOCK)
               NO-LOCK NO-ERROR.

               {opbnkcr.i op.op "''" "'МФО-9'" mBIK "(IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE '')"}
            END.

            {empty mtCommonDR}
            FOR EACH mxxattr1 WHERE mxxattr1.class-code = op.class-code
                               AND mxxattr1.xattr-code <> "dpr-id"
                               AND mxxattr1.xattr-code <> "ПНКодДокументаКомиссии"
                               AND NOT CAN-DO("class,group",mxxattr1.data-type)
            NO-LOCK,
            FIRST mxxattr2 WHERE mxxattr2.class-code = mxop.class-code
                            AND mxxattr2.xattr-code = mxxattr1.xattr-code
                            AND NOT CAN-DO("class,group",mxxattr2.data-type)
            NO-LOCK:
               IF NOT mSeparate AND CAN-DO({&ParsAttrs},mxxattr1.xattr-code) 
               THEN 
                  mDRValPars = ENTRY(1,GetXAttrValue("loan",
                                                     GetSurrogateBuffer("loan",
                                                                        (BUFFER mxloan-par:HANDLE)),
                                                     "PARSSEN_" + mxxattr1.xattr-code)).
               ELSE mDRValPars = "".
               IF {assigned mDRValPars} THEN mDrVal = "".
               ELSE
               mDRVal = GetXAttrValueEx("op",STRING(op.op),mxxattr1.xattr-code,"").
               IF NOT {assigned mDRVal} THEN DO:
                  IF NOT {assigned mDRValPars} THEN
                  mDRValPars = GetXAttrValue("code","ПНВидПлатежа," + mTypPl,"PARSSEN_" + mxxattr1.xattr-code).

                  IF TRIM(mDRValPars,"0123456789") <> "" THEN DO:
                     RUN ParsMain IN h_parsr (mDRValPars,?,?,OUTPUT mFlErr,OUTPUT mOutRes).
                     IF NOT mFlErr AND {assigned mOutRes} THEN mDRValPars = moutRes.
                  END.

                  IF  NOT mSeparate
                  AND NOT {assigned mDRValPars} THEN DO:
                     IF NOT CAN-DO(mtDocs.BreakList,mxxattr1.xattr-code)
                     THEN NEXT.
                  END.
                  mDRVal = GetXAttrValueEx("op",STRING(mxop.op),mxxattr1.xattr-code,"").
                  IF NOT {assigned mDRVal} OR 
                     (CAN-DO({&ParsAttrs},mxxattr1.xattr-code) AND {assigned mDRValPars})
                  THEN mDRVal = mDRValPars.
                  IF {assigned mDRVal} THEN DO:
                     CREATE mtCommonDR.
                     ASSIGN
                        mtCommonDR.xattr-code = mxxattr1.xattr-code
                        mtCommonDR.ValPars    = mDRValPars
                        mtCommonDR.Val        = mDRVal
                     .
                  END.
               END.
            END.

            mLastOp = op.op.
         END.
         ELSE DO:
            FIND FIRST op WHERE op.op = mLastOp EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            FOR EACH mtCommonDR EXCLUSIVE-LOCK:
               mDRVal = GetXAttrValueEx("op",STRING(mxop.op),mtCommonDR.xattr-code,"").
               IF NOT {assigned mDRVal} OR 
                  (CAN-DO({&ParsAttrs},mtCommonDR.xattr-code) AND {assigned mDRValPars})
               THEN mDRVal = mtCommonDR.ValPars.
               IF mDRVal <> mtCommonDR.Val
               THEN DELETE mtCommonDR.
            END.
         END.

         ASSIGN
            mOpSum   = mOpSum + mTransSum
            mNumDocs = mNumDocs + 1.

         IF NOT UpdateSigns(mxop.class-code, STRING(mxop.op), "ПНСтатусПлатежа", "ПРЧ", ?) THEN DO:
            RUN Fill-SysMes("","","-1","Ошибка реквизита ПНСтатусПлатежа.").
            UNDO MAIN, LEAVE MAIN.
         END.

         FIND FIRST mxlink WHERE mxlink.class-code     = mxop.class-code
                            AND mxlink.link-code      = "pn_doc_com"
                            AND mxlink.link-direction = "s"
         NO-LOCK NO-ERROR.
         IF AVAIL mxlink THEN DO:
            FOR EACH mlinks WHERE mlinks.link-id   = mxlink.link-id
                             AND mlinks.source-id = STRING(mxop.op)
            NO-LOCK:
               mTransSum = mTransSum - DECIMAL(mlinks.link-val) NO-ERROR. {&ON-ERROR}
            END.
         END.

         /* Связь платеж - перечисление */
         RUN CreateLinksRetSurr IN h_xclass (mxop.class,
                                             "pn_doc_tr",
                                             STRING(mxop.op),
                                             STRING(op.op),
                                             iOpDate,
                                             ?,
                                             STRING(mTransSum),
                                             OUTPUT mLinkSurr).
         IF NOT {assigned mLinkSurr} THEN DO:
            RUN Fill-SysMes("","","-1","Невозможно создать связь платежа с документом перечисления (pn_doc_tr).").
            UNDO MAIN, LEAVE MAIN.
         END.

         IF mSeparate
         OR LAST-OF(mtDocs.contract)
         OR LAST-OF(mtDocs.cont-code)
         OR LAST-OF(mtDocs.BreakVal)
         THEN DO:
            IF mtDocsPar.Tarif > 0.0 THEN DO:
               IF  mtDocsPar.CalcComSum = NO
               AND mSeparate           = NO
               AND mtDocs.BreakVal      = "" THEN DO:
                  /* Сумма комиссии - берется из суммы документа-комиссии */
                  FOR EACH mxtDocs WHERE mxtDocs.contract  = mtDocsPar.contract
                                    AND mxtDocs.cont-code = mtDocsPar.cont-code
                  NO-LOCK,
                  FIRST mxop-pay WHERE mxop-pay.op = mxtDocs.op NO-LOCK,
                  FIRST mxlink WHERE mxlink.class-code     = mxop-pay.class-code
                                AND mxlink.link-code      = "pn_doc_com"
                                AND mxlink.link-direction = "s"
                  NO-LOCK,
                  EACH mlinks WHERE mlinks.link-id   = mxlink.link-id
                               AND mlinks.source-id = STRING(mxtDocs.op)
                  NO-LOCK BREAK BY mlinks.target-id: 
                     IF FIRST-OF(mlinks.target-id) THEN DO:
                        FIND FIRST mxop-com WHERE mxop-com.op = INT64(mlinks.target-id) NO-LOCK NO-ERROR.
                        IF AVAIL mxop-com THEN DO:
                           FOR EACH mxop-entry WHERE mxop-entry.op = mxop-com.op NO-LOCK:
                              mOpSum = mOpSum - mxop-entry.amt-rub.
                           END.
                        END.
                     END.
                  END.
               END.
               ELSE DO:
                  /* Сумма комиссии - рассчитывается */
                  mComSum = IF mtDocsPar.TarifType THEN mtDocsPar.tarif * mNumDocs ELSE ROUND(mOpSum * (mtDocsPar.Tarif / 100),2).   
                  IF mComSum = ? THEN mComSum = 0.0.
                  mOpSum = mOpSum - mComSum.
               END.
            END.

            CREATE op-entry.
            ASSIGN
               op-entry.acct-cat  = op.acct-cat
               op-entry.user-id   = op.user-id
               op-entry.op        = op.op
               op-entry.op-entry  = 1
               op-entry.currency  = ""
               op-entry.acct-cr   = mAcctCr
               op-entry.acct-db   = mAcctDb
               op-entry.amt-rub   = mOpSum
               op-entry.type      = (IF mInternal THEN "ВН" ELSE "НЕ")
            .

            FOR EACH mtCommonDR NO-LOCK:
               IF CAN-DO({&NalAttrs},mtCommonDR.xattr-code)
               THEN mAnyNal = YES.
               IF NOT UpdateSigns(op.class-code, STRING(op.op), mtCommonDR.xattr-code, mtCommonDR.Val, ?) THEN DO:
                  RUN Fill-SysMes("","","-1","Ошибка реквизита " + mtCommonDR.xattr-code + ".").
                  UNDO MAIN, LEAVE MAIN.
               END.
            END.

            IF mAnyNal THEN DO:
               /* kpp-send */
               IF {assigned mKPPSend} 
                  AND mFillSend EQ "Да"
               THEN DO:
                  IF NOT UpdateSigns(op.class-code, STRING(op.op), "Kpp-send", mKPPSend, ?) THEN DO:
                     RUN Fill-SysMes("","","-1","Ошибка реквизита Kpp-send.").
                     UNDO MAIN, LEAVE MAIN.
                  END.
               END.
               /* kpp-rec */
            END.
            IF {assigned mKPP} THEN DO:
               IF NOT UpdateSigns(op.class-code, STRING(op.op), "Kpp-rec", mKPP, ?) THEN DO:
                  RUN Fill-SysMes("","","-1","Ошибка реквизита Kpp-rec.").
                  UNDO MAIN, LEAVE MAIN.
               END.
            END.

            /* Назначение платежа */
            IF mSeparate THEN DO:
               mDetails = mxop.details.
            end.
            else do:
               RUN PARSFUNC-СодержОпер IN h_ppn (mxop.op, op.op, OUTPUT mDetails, OUTPUT mParsOk).
            end.
            IF mParsOk <> 0 THEN DO:
               RUN Fill-SysMes("","","-1","Ошибка парсерной функции СодержОпер").
               UNDO MAIN, LEAVE MAIN.
            END.
            IF {assigned mDetails} THEN op.details = mDetails.

            {op.upd
               &undo      = "UNDO MAIN, RETRY MAIN"
               &chkupd    = "Ins"
            }

            RUN CreateLinksRetSurr IN h_xclass (op.class,
                                                "pn_transfer",
                                                STRING(op.op),
                                                GetSurrogateBuffer("loan",(BUFFER mxloan-reestr:HANDLE)),
                                                mxloan-reestr.open-date,
                                                ?,
                                                "",
                                                OUTPUT mLinkSurr).
            IF NOT {assigned mLinkSurr} THEN DO:
               RUN Fill-SysMes("","","-1","Невозможно создать связь документа перечисления с реестром (pn_transfer).").
               UNDO MAIN, LEAVE MAIN.
            END.
         END.

      END.

      mxloan-reestr.loan-status = "ПРЧ".
      RELEASE mxloan-reestr.

      mQuery:GET-NEXT().
   END.

   mIsOk = YES.
END.
IF VALID-HANDLE(mQuery) THEN DELETE OBJECT mQuery.

{intrface.del}

if avail op then do:
   mstr_err = string(op.op) + ";" + op.doc-num + ";" + mt-loan-doc-num.
end.
else do:
   mstr_err = "".
end.

IF mIsOk = YES
   THEN RETURN mstr_err.
   ELSE RETURN ERROR.
/* $LINTUSER='SHOI' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='11/12/2014 21:58:29.394+04:00' */
/* $LINTFILE='perreestr.p' */
