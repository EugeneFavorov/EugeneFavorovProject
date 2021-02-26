&GLOBAL-DEFINE LAW_318p YES
&GLOBAL-DEFINE op-entry yes
&GLOBAL-DEFINE tt-op-entry yes
&SCOP OFFsigns YES

DEFINE INPUT PARAMETER RID AS RECID NO-UNDO.

{globals.i}
{intrface.get prnvd}
{intrface.get pbase}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{intrface.get op}
{parsin.def}
{chkacces.i}
{signature.pro}
{branch.pro}
{pp-uni.var
   &multiline-author = YES,
   &FILE_sword_p     = YES
}                           /* определение переменных        */
{pp-uni.err}                /* сообщения об ошибках          */
{pp-uni.prg}                /* описание стандартных процедур */

{mo-uni.dec}

{def-wf.i NEW}
{mo-pars.i}

DEFINE VARIABLE mdeDocSum      AS DECIMAL NO-UNDO.
DEFINE VARIABLE mdeNatSum      AS DECIMAL NO-UNDO.
DEFINE VARIABLE mdeDragSum     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mDeSymSumIn    AS DECIMAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE mDeSymSumOut   AS DECIMAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE mChSymCodIn    AS CHARACTER EXTENT 50 NO-UNDO.
DEFINE VARIABLE mChSymCodOut   AS CHARACTER EXTENT 50 NO-UNDO.
DEFINE VARIABLE minCount       AS INT64    NO-UNDO.
DEFINE VARIABLE mchPayer       AS CHARACT EXTENT 3 INIT "" NO-UNDO.
DEFINE VARIABLE mchReceiver    AS CHARACT EXTENT 4 INIT "" NO-UNDO.
DEFINE VARIABLE mchRecBank     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE AcctCr         LIKE op-entry.acct-cr  NO-UNDO.
DEFINE VARIABLE AcctCr2        AS CHARACTER EXTENT 2  NO-UNDO.
DEFINE VARIABLE AcctDb         LIKE op-entry.acct-db  NO-UNDO.
DEFINE VARIABLE AcctDbCur      LIKE op-entry.currency NO-UNDO.
DEFINE VARIABLE AcctCrCur      LIKE op-entry.currency NO-UNDO.
DEFINE VARIABLE AcctKomis      LIKE op-entry.acct-db NO-UNDO.
DEFINE VARIABLE DocCur         LIKE op-entry.currency NO-UNDO.
DEFINE VARIABLE TmpSymbol      LIKE op-entry.symbol   NO-UNDO.
DEFINE VARIABLE mNumArray      AS INT64     NO-UNDO.
DEFINE VARIABLE mdtDateDoc     AS DATE      NO-UNDO.
DEFINE VARIABLE mchOrdTypeDb   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchOrdTypeCr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchIdentCard   AS CHARACTER NO-UNDO.
DEFINE VARIABLE CrCustCat      LIKE acct.cust-cat NO-UNDO.
DEFINE VARIABLE CrCustCat1     LIKE acct.cust-cat NO-UNDO.
DEFINE VARIABLE DbCustCat      LIKE acct.cust-cat NO-UNDO.
DEFINE VARIABLE DbCustCat1     LIKE acct.cust-cat NO-UNDO.
DEFINE VARIABLE CrContract     LIKE acct.contract NO-UNDO.
DEFINE VARIABLE mDbContract    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchBankName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchBankBIK     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mlgChoise      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mchBankSity    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVOKDprID      AS CHARACTER NO-UNDO. /* ИД смены ВОК в документе */
DEFINE VARIABLE mDocument-id   AS CHARACTER NO-UNDO. /* Тип документа */
DEFINE VARIABLE mDocCodName    AS CHARACTER NO-UNDO. /* Код документа */
DEFINE VARIABLE mDetails       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPersonId      AS INT64     NO-UNDO.
DEFINE VARIABLE mIdCustAttr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustTable1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINN           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDbBranchNam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCrBranchOKATO AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCrBranchNam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpStr        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocDate       AS CHARACTER NO-UNDO. /* Дата выдачи документа */
DEFINE VARIABLE mDocNum        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPassKP        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustDocWho    AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mKasSchPol     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInnKas        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecKPP        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecOKATO      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecAcct       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecAcct2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPayBank       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mPayBankBik    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchFIO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchWorkerBuhP  AS CHARACTER EXTENT 3 NO-UNDO. /* Должность */
DEFINE VARIABLE mchWorkerKontP AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mchWorkerKasP  AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mchWorkerBuh   AS CHARACTER EXTENT 3 NO-UNDO. /* ФИО */ 
DEFINE VARIABLE mchWorkerKont  AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mchWorkerKas   AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mchWorkerBuhPs AS CHARACTER EXTENT 3 NO-UNDO. /* Должность */
DEFINE VARIABLE mchWorkerKasPs AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mchWorkerBuhs  AS CHARACTER EXTENT 3 NO-UNDO. /* ФИО */ 
DEFINE VARIABLE mchWorkerKass  AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mIsPrtCity     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mAgentID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProxyCode     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDrowerID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchSymFromNP   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mIsInOut       AS LOGICAL INITIAL FALSE NO-UNDO. 
DEFINE VARIABLE mIsSymIn       AS LOGICAL INITIAL FALSE NO-UNDO. 
DEFINE VARIABLE mCodeDoc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDrag          AS CHARACTER NO-UNDO. /* Валюта драг. металла  */
DEFINE VARIABLE NalRKROtr      AS CHARACTER NO-UNDO. /* НП НалРКРОтр */
DEFINE VARIABLE NalRKRPol      AS CHARACTER NO-UNDO. /* НП НалРКРПол */
DEFINE VARIABLE mCounter       AS INT64     NO-UNDO INIT 0.
DEFINE VARIABLE mAmtDif        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mHalf          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE AcRepPolKas    AS CHARACTER NO-UNDO. /* НП СчЗапПолКас */
DEFINE VARIABLE mKasKas        AS CHARACTER NO-UNDO. /* НП КОВывНБКасКас */
DEFINE VARIABLE mKasNoFill     AS LOGICAL   NO-UNDO. /* Заполнять или нет реквизиты получателя */
DEFINE VARIABLE mStrPar        AS CHARACTER NO-UNDO. /* Значение НП для заполнения свободного поля */
DEFINE VARIABLE mEmptyField    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mEmptyField2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlagOSP       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFlagPackPrn   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOurBank       AS LOGICAL   INIT YES 
                                            NO-UNDO.
DEFINE VARIABLE mCorNameOb     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFldNameBnk    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKasOrdNS      AS CHARACTER NO-UNDO.
&IF DEFINED(f2in1) &THEN
DEFINE VARIABLE mDocCount      AS INT64     NO-UNDO.
&ENDIF

DEFINE BUFFER xop FOR op.
DEFINE BUFFER xop-entry FOR op-entry.

DEFINE VARIABLE mdeCrMainSum AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE mdeCrCommSum AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE mdeCrCommNatSum AS DECIMAL INITIAL 0 NO-UNDO.

DEFINE VARIABLE mdeCrExtSum AS DECIMAL EXTENT 2 NO-UNDO.

&GLOBAL-DEFINE cr-main-sum mdeCrMainSum
&GLOBAL-DEFINE cr-comm-sum mdeCrCommSum
&GLOBAL-DEFINE cr-comm-nat-sum mdeCrCommNatSum

&IF DEFINED(ORACLE) <> 0 &THEN
DEFINE VARIABLE mRwd           AS ROWID     NO-UNDO.
&ENDIF
DEFINE BUFFER komis-op-entry FOR op-entry.
DEFINE BUFFER xhistory       FOR history.
DEFINE BUFFER xcust-ident    FOR cust-ident.
DEFINE BUFFER bProxy         FOR loan.
DEFINE BUFFER bAgent         FOR person.
DEFINE BUFFER bCustIdent     FOR cust-ident.
DEFINE BUFFER bDrower        FOR person.
DEFINE BUFFER cacct          FOR acct.
DEFINE BUFFER kurs-op-entry  FOR op-entry.

DEFINE VARIABLE mMonthStr AS CHARACTER INITIAL "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря" NO-UNDO.

FUNCTION _date_2_Str RETURNS CHARACTER (iDate AS DATE):
   RETURN STRING(DAY(iDate))
        + " "
        + ENTRY(MONTH(iDate), mMonthStr)
        + " "
        + STRING(YEAR(iDate))
        + " г.".
END FUNCTION.

FUNCTION NumArraySymCod RETURNS INT64 (iStr  AS CHARACTER EXTENT 50,
                                       iSymb AS CHARACTER):
 DEF VAR vJ        AS INT64 NO-UNDO.
 DEF VAR vNumArray AS INT64 NO-UNDO INIT 0.
     DO vJ = 1 TO EXTENT(iStr):
        IF iStr[vJ] =  iSymb THEN DO:
           vNumArray = vJ.
           LEAVE.
        END.
     END.
 RETURN vNumArray.
END FUNCTION.

FUNCTION _str_2_DATE RETURNS DATE (iDate AS CHARACTER):
   IF NUM-ENTRIES(iDate,"/") <> 3 AND
      NUM-ENTRIES(iDate," ") >= 3 THEN 
      RETURN DATE(ENTRY(1,iDate," ") + "/" + STRING(LOOKUP(ENTRY(2,iDate," "),mMonthStr)) + "/" + TRIM(ENTRY(3,iDate," ")," г.")).
   ELSE
      RETURN DATE(iDate).
END FUNCTION.


&IF DEFINED(LAW_318p) <> 0 &THEN
FUNCTION LocalGetRAcct RETURNS CHAR PRIVATE (INPUT iCustCat AS CHAR, INPUT iCustId AS INT64, INPUT iOpOp AS INT64, INPUT iOpDate AS DATE):
   DEFINE BUFFER recacct FOR acct.
   FIND FIRST recacct WHERE recacct.cust-cat = iCustCat
                        AND recacct.cust-id  = iCustId
                        AND recacct.acct-cat = "b"
                        AND CAN-FIND(FIRST op-entry WHERE op-entry.op = iOpOp
                                                      AND (   op-entry.acct-db = recacct.acct
                                                           OR op-entry.acct-cr = recacct.acct) NO-LOCK)
   NO-LOCK NO-ERROR.
   IF NOT AVAIL recacct
      THEN FIND FIRST recacct WHERE recacct.cust-cat   = iCustCat
                                AND recacct.cust-id    = iCustId
                                AND recacct.acct-cat   = "b"
                                AND recacct.open-date <= op.op-date
                                AND (   recacct.close-date = ?
                                     OR recacct.close-date > iOpDate)
           NO-LOCK NO-ERROR.
   IF AVAIL recacct THEN RETURN STRING(recacct.number, GetAcctFmt(recacct.acct-cat)).
   RETURN "".
END FUNCTION.
&ENDIF

FUNCTION LocalGetCrAcct2 RETURNS CHAR PRIVATE (INPUT iOpOp AS INT64,
                                               INPUT iAcctDb AS CHAR,
                                               INPUT iAcctCr AS CHAR,
                                               INPUT iAcctKomis AS CHAR):
   DEFINE BUFFER cracct    FOR acct.
   DEFINE BUFFER cop-entry FOR op-entry.
   DEFINE VARIABLE CrNum AS CHARACTER NO-UNDO.
   
   FOR EACH cop-entry WHERE
            cop-entry.op      =  iOpOp
        AND cop-entry.acct-db =  iAcctDb
        AND cop-entry.acct-cr <> iAcctCr
        AND cop-entry.acct-cr <> iAcctKomis NO-LOCK,
   FIRST cracct WHERE
         cracct.currency =  cop-entry.currency
     AND CAN-DO(cop-entry.acct-cr, cracct.acct) 
   NO-LOCK:
      CrNum = CrNum + "       " + STRING(cracct.number, GetAcctFmt(cracct.acct-cat)).
   END.
   IF AVAIL cracct THEN RETURN CrNum.
   RETURN "".
END FUNCTION.

FIND FIRST op WHERE RECID(op) = RID NO-LOCK NO-ERROR.

/* ПРоверка валюты проводок документа */
&IF DEFINED(valcashord) <> 0 &THEN
   FOR EACH op-entry OF op NO-LOCK:
      IF op-entry.currency <> "" THEN
         mCounter = mCounter + 1.
   END.
   IF mCounter =  0 THEN DO:
      MESSAGE "Рублевые документы не обрабатываются!" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
&ENDIF

/* определяем счет дебета */
FIND FIRST op-entry OF op WHERE op-entry.acct-db <> ? NO-LOCK NO-ERROR.
IF NOT AVAIL(op-entry) THEN
   DO:
      MESSAGE "Проводок по дебету по документу не найдено" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
ELSE
   AcctDb = op-entry.acct-db.

IF NOT type-curracct THEN
   {find-act.i &fila=LAST &acct=AcctDb &curr=op-entry.currency}
ELSE
   FIND FIRST acct WHERE acct.acct =  AcctDb
               AND acct.currency =  op-entry.currency
      NO-LOCK NO-ERROR.

mCorNameOb  = FGetSetting("ПлатДок", "КорНаимОбВН", "?").
mKasKas     = FGetSetting("ПлатДок", "КОВывНБКасКас","").
mFldNameBnk = IF mCorNameOb =  "Да" THEN "short-name"
                                    ELSE "name".
IF AVAIL acct THEN
   DO:
      IF acct.contract BEGINS "Касса" THEN
         mchOrdTypeDb = "приходный".
      ASSIGN
         AcctDbCur = acct.currency
         DbCustCat = acct.cust-cat
         mDbContract = acct.contract.
      FOR FIRST branch WHERE branch.branch-id =  acct.branch-id NO-LOCK:
          mDbBranchNam = IF mCorNameOb =  "Да" THEN branch.short-name
                                               ELSE branch.name.
      END.
   END.
ELSE DO:
   MESSAGE "Счет по дебету " + AcctDb + " не найден"
      VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* определяем счет кредита */
FIND FIRST op-entry OF op WHERE op-entry.acct-cr <> ? NO-LOCK NO-ERROR.
IF NOT AVAIL(op-entry) THEN DO:
   MESSAGE "Проводок по кредиту по документу не найдено" VIEW-AS ALERT-BOX ERROR.
   RETURN.
   END.
ELSE
   AcctCr = op-entry.acct-cr.

IF NOT type-curracct THEN
   {find-act.i &fila=LAST &acct=AcctCr &curr=op-entry.currency}
ELSE
   FIND FIRST acct WHERE acct.acct =  AcctCr
               AND acct.currency =  op-entry.currency
     NO-LOCK NO-ERROR.

IF AVAIL acct THEN
   DO:
      IF acct.contract BEGINS "Касса" THEN
         mchOrdTypeCr = "расходный".
      AcctCrCur = acct.currency.
      CrCustCat = acct.cust-cat.
      CrContract = acct.contract.
      FOR FIRST branch WHERE branch.branch-id =  acct.branch-id NO-LOCK:
         mCrBranchNam = IF mCorNameOb =  "Да" THEN branch.short-name
                                              ELSE branch.name.
         mCrBranchOKATO = GetXAttrValueEx("branch",acct.branch-id,"ОКАТО-НАЛОГ","").
      END.
   END.
ELSE DO:
   MESSAGE "Счет по кредиту " + AcctCr + " не найден"
      VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

&IF DEFINED(LAW_318p) <> 0 &THEN
   IF  AcctDbCur    =  AcctCrCur
   AND mchOrdTypeDb =  "приходный"
   AND mchOrdTypeCr =  "расходный" THEN DO:
      ASSIGN
         mlgChoise = TRUE
         mIsInOut  = TRUE
      .
      MESSAGE "Печатать документ" op.doc-num "как ПРИХОДНО-РАСХОДНЫЙ ордер?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mlgChoise.
      IF mlgChoise THEN ASSIGN
         mchOrdTypeDb = "приходно─расходный"
         mchOrdTypeCr = "приходно─расходный"
      .
   END.
&ENDIF

/* 0033333: Если ни один из корреспондирующих счетов не имеет признак "касса" */
/* и оба в одинаковой валюте, то придется спросить какой ордер нужен */
mlgChoise = TRUE.
IF  AcctDbCur =  AcctCrCur
AND (   (    mchOrdTypeCr =  ""
         AND mchOrdTypeDb =  "")
     OR (    mchOrdTypeDb =  "приходный"
         AND mchOrdTypeCr =  "расходный")
    )
   THEN DO:
      MESSAGE "Печатать документ" op.doc-num "как ПРИХОДНЫЙ ордер?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mlgChoise.
      IF mlgChoise THEN DO:
         mchOrdTypeDb = "приходный".
         mchOrdTypeCr = "".
      END.
      ELSE DO:
         mchOrdTypeCr = "расходный".
         mchOrdTypeDb = "".
      END.
   END.

mdtDateDoc = IF op.doc-date = ? THEN op.op-date ELSE op.doc-date.
mVOKDprID  = GetXAttrValueEx("op", STRING(op.op), "dpr-id",""). /* код смены ВОК */

/* Выводим подписную часть */
RUN GetRepFioByRef(&IF DEFINED(valcashord) &THEN "valcashord"
                      &ELSE ENTRY(1,SOURCE-PROCEDURE:FILE-NAME, ".") &ENDIF,
                   GetUserBranchID(USERID('bisquit')),
                   INT64(op.op),
                   INT64(mVOKDprID)) NO-ERROR.

ASSIGN
   mchBankName = FGetSetting("Банк",    ?,"")
   mchBankBIK  = FGetSetting("БанкМФО", ?,"")
   mchBankSity = ", " + FGetSetting("БанкГород", ?,"")
   mKasSchPol  = FGetSetting("КасСчПол", "","")
   mInnKas     = FGetSetting("ПлатДок", "ВывИННКас","")
   &IF DEFINED(LAW_318p) <> 0 &THEN
      mIsPrtCity = (FGetSetting("ПлатДок", "ВыводМест","Да") = "Да")
   &ELSE
      mIsPrtCity = YES
   &ENDIF
   mchWorkerBuhP[1]  = mPostInRep[1]
   mchWorkerBuh[1]   = mFIOInRep[1]
   mchWorkerKontP[1] = mPostInRep[2]
   mchWorkerKont[1]  = mFIOInRep[2]
   mchWorkerKasP[1]  = mPostInRep[3]
   mchWorkerKas[1]   = mFIOInRep[3]
.

IF NOT {assigned mchWorkerBuhP[1]} OR NOT {assigned mchWorkerBuh[1]} THEN
DO:
FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK NO-ERROR.
IF AVAIL _user THEN
   ASSIGN
      mchWorkerBuhP[1] = GetXAttrValueEx("_user", _user._userid, "Должность", "Бухгалтер")
      mchWorkerBuh[1]  = _user._user-Name
   .
END.

IF NOT {assigned mchWorkerKontP[1]} OR NOT {assigned mchWorkerKont[1]} THEN
DO:
mTmpStr = GetXAttrValueEx("op", STRING(op.op), "user-direct", "").
FIND FIRST _user WHERE _user._userid = mTmpStr NO-LOCK NO-ERROR.
IF AVAIL _user THEN
   ASSIGN
      mchWorkerKontP[1] = GetXAttrValueEx("_user", _user._userid, "Должность", "Контролер")
      mchWorkerKont[1]  = _user._user-Name
   .
END.
IF NOT {assigned mchWorkerKasP[1]} OR NOT {assigned mchWorkerKas[1]} THEN
DO:
FIND FIRST _user WHERE _user._userid = op.user-inspector NO-LOCK NO-ERROR.
IF AVAIL _user THEN
   ASSIGN
      mchWorkerKasP[1] = GetXAttrValueEx("_user", _user._userid, "Должность", "Кассир")
      mchWorkerKas[1]  = _user._user-Name
   .
END.

IF mchWorkerBuhP[1]  =  "@" THEN mchWorkerBuhP[1]  = "".
IF mchWorkerBuh[1]   =  "@" THEN mchWorkerBuh[1]   = "".
IF mchWorkerKontP[1] =  "@" THEN mchWorkerKontP[1] = "".
IF mchWorkerKont[1]  =  "@" THEN mchWorkerKont[1]  = "".
IF mchWorkerKasP[1]  =  "@" THEN mchWorkerKasP[1]  = "".
IF mchWorkerKas[1]   =  "@" THEN mchWorkerKas[1]   = "".

ASSIGN
   mchWorkerBuhPs = mchWorkerBuhP
   mchWorkerKasPs = mchWorkerKasP
   mchWorkerBuhs  = mchWorkerBuh
   mchWorkerKass  = mchWorkerKas  
.


&IF DEFINED(valcashord) <> 0 &THEN
   {sumstrfm.i}
   DEFINE VARIABLE mSumFormat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSumSep    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAcctDb    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAcctCr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAcctDb2   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAcctCr2   AS CHARACTER NO-UNDO.
   
   RUN GetSeparator318p(OUTPUT mSumSep).
   mSumFormat = getFormatStr(22, ",", YES).
   NalRKROtr = FGetSetting("НалРКРОтр","","").
   NalRKRPol = FGetSetting("НалРКРПол","","").
      
   /* Если курсовая разница. Определяем буфер xop */
   IF CAN-FIND(FIRST op-entry OF op WHERE
              CAN-DO( NalRKRPol, op-entry.acct-cr )
           OR CAN-DO( NalRKRPol, op-entry.acct-db )
           OR CAN-DO( NalRKROtr, op-entry.acct-cr )
           OR CAN-DO( NalRKROtr, op-entry.acct-db )
           NO-LOCK)
   THEN
      FIND FIRST xop WHERE xop.op =  op.op NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST xop WHERE xop.op <> op.op
           AND xop.op-transaction =  op.op-transaction NO-LOCK NO-ERROR.

   FOR EACH kurs-op-entry OF xop NO-LOCK:

      IF    CAN-DO(NalRKROtr,kurs-op-entry.acct-db) 
         OR CAN-DO(NalRKRPol,kurs-op-entry.acct-db) THEN
      DO:
         mAcctDb = DelFilFromAcct(kurs-op-entry.acct-db).
         RUN Insert_TTName("difOut",
         TRIM(AmtStrSepFormat(kurs-op-entry.amt-rub, mSumSep, mSumFormat))).
      END.
      IF    CAN-DO(NalRKROtr,kurs-op-entry.acct-cr) 
         OR CAN-DO(NalRKRPol,kurs-op-entry.acct-cr) THEN
      DO:
         mAcctCr = DelFilFromAcct(kurs-op-entry.acct-cr).
         RUN Insert_TTName("difIn",
         TRIM(AmtStrSepFormat(kurs-op-entry.amt-rub, mSumSep, mSumFormat))).
      END.
   END.

   FOR EACH kurs-op-entry OF op NO-LOCK:
      IF    CAN-DO(NalRKROtr,kurs-op-entry.acct-db) 
         OR CAN-DO(NalRKRPol,kurs-op-entry.acct-db) 
         OR CAN-DO(NalRKROtr,kurs-op-entry.acct-cr) 
         OR CAN-DO(NalRKRPol,kurs-op-entry.acct-cr)
          THEN
      DO: END. ELSE
      DO:
         IF kurs-op-entry.currency <> "" THEN
         DO:
            IF mAcctDb =  "" THEN
            DO:
               IF     kurs-op-entry.acct-db <> ? 
                  AND AcctDbCur             <> "" THEN  
                  mAcctDb2 = DelFilFromAcct(kurs-op-entry.acct-db).
               ELSE
               DO:
                  IF AcctDbCur =  "" OR AcctCrCur =  "" THEN
                  DO:
                     IF     kurs-op-entry.acct-cr <> ? 
                        AND AcctCrCur             <> "" THEN 
                     mAcctDb2 = DelFilFromAcct(kurs-op-entry.acct-cr).
                  END.
               END.
            END.
            IF mAcctCr =  "" THEN
            DO:
               IF     kurs-op-entry.acct-cr <> ?
                  AND AcctCrCur             <> "" THEN  
               mAcctCr2 = DelFilFromAcct(kurs-op-entry.acct-cr).
               ELSE
               DO:
                  IF AcctDbCur =  "" OR AcctCrCur =  "" THEN
                  DO:
                     IF     kurs-op-entry.acct-db <> ? 
                        AND AcctDbCur             <> "" THEN 
                     mAcctCr2 = DelFilFromAcct(kurs-op-entry.acct-db).
                  END.
               END.
            END.
         END.
      END.
   END.
   
   IF mAcctDb =  "" THEN mAcctDb = mAcctDb2.
   IF mAcctCr =  "" THEN mAcctCr = mAcctCr2.
   
   IF    CAN-DO(NalRKROtr,mAcctDb) 
      OR CAN-DO(NalRKRPol,mAcctDb) THEN
   DO:
      RUN Insert_TTName("difacctDbOut", DelFilFromAcct(mAcctDb)).
      RUN Insert_TTName("difacctCrOut", DelFilFromAcct(mAcctCr)).
   END.
   IF    CAN-DO(NalRKROtr,mAcctCr) 
      OR CAN-DO(NalRKRPol,mAcctCr) THEN
   DO:
      RUN Insert_TTName("difacctDbIn", DelFilFromAcct(mAcctDb)).
      RUN Insert_TTName("difacctCrIn", DelFilFromAcct(mAcctCr)).
   END.
   
&ENDIF

IF FirstPrint OR NOT PackagePrint THEN DO:
   OUTPUT TO _spool.tmp.
   OUTPUT CLOSE.
END.

ASSIGN
   mFlagOSP     = yes
   mFlagPackPrn = PackagePrint
&IF DEFINED(valcashord) =  0 &THEN
/*   PackagePrint = yes */
&ENDIF
.   
&IF DEFINED(f2in1) <> 0 &THEN
   minCount = 0.
   IF mchOrdTypeDb = "приходный" THEN 
      FOR EACH op-entry OF op WHERE op-entry.acct-db = AcctDb NO-LOCK:
         IF (DocCur =  "" AND op-entry.amt-rub <> 0) OR (DocCur <> "" AND op-entry.amt-cur <> 0)
            THEN minCount = minCount + 1.
      END.
   IF mchOrdTypeCr = "расходный" THEN 
      FOR EACH op-entry OF op WHERE op-entry.acct-cr = AcctCr NO-LOCK:
         IF (DocCur =  "" AND op-entry.amt-rub <> 0) OR (DocCur <> "" AND op-entry.amt-cur <> 0)
            THEN minCount = minCount + 1.
      END.
   IF minCount >  3 OR mchOrdTypeDb =  "приходно─расходный"
   THEN 
      mFlagOSP = yes.
   ELSE
      ASSIGN 
         mTmpStr  = ""
         mFlagOSP = NO
         PackagePrint = mFlagPackPrn
      .
   mDocCount = INT64(GetSysConf("DocCount")) NO-ERROR.
   IF mDocCount =  ? THEN
      mDocCount = 0.

   {strtout3.i &cols=98 &filename='_f2in1.tmp' &option=Paged}

   IF PackagePrint THEN DO:
      {setdest.i &cols=98 &filename='_f2in1.tmp' &option=Paged} 
   END.
&ENDIF


IF mchOrdTypeDb = "приходный" THEN DO:
   &IF DEFINED(valcashord) <> 0 &THEN
      mStrPar = FGetSetting("ПлатДок", "ВалКО", "").
   &ELSE
      mStrPar = FGetSetting("ПлатДок", "ПрихКО", "").
   &ENDIF
   RUN ProcProcessFreeField(mStrPar,?,?).
   &IF DEFINED(LAW_318p) <> 0 &THEN
      FIND FIRST komis-op-entry OF op WHERE komis-op-entry.acct-db  = AcctDb
                                        AND komis-op-entry.acct-cr <> AcctCr
                                        AND CAN-FIND(FIRST code WHERE code.class = "ПрСуммы"
                                                                  AND CAN-DO(code.val,komis-op-entry.acct-cr))
         NO-LOCK NO-ERROR.
        IF AVAILABLE komis-op-entry THEN DO:
            AcctKomis = komis-op-entry.acct-cr.
            IF komis-op-entry.currency = "" THEN
                mdeCrCommSum = komis-op-entry.amt-rub.
            ELSE DO:
                mdeCrCommSum = komis-op-entry.amt-cur.
                mdeCrCommNatSum = komis-op-entry.amt-rub.
            END.
        END.
        ELSE
            AcctKomis = "".
   &ENDIF

   mCounter = 0.
   FOR EACH bop-entry OF op WHERE
            bop-entry.acct-db =  AcctDb
        AND bop-entry.acct-cr <> AcctCr
        AND bop-entry.acct-cr <> AcctKomis
   NO-LOCK:
      mCounter = mCounter + 1.
      AcctCr2[mCounter] = bop-entry.acct-cr.
      IF {assigned bop-entry.acct-cr} THEN
         mdeCrExtSum[mCounter] = IF {assigned bop-entry.currency}
                                 THEN bop-entry.amt-cur
                                 ELSE bop-entry.amt-rub.
   END.
   
   mchRecBank[1] = mchBankName.
   IF mIsPrtCity THEN mchRecBank[1] = mchRecBank[1] + mchBankSity.

   mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
   IF mchPayer[1] = "" THEN
      mchPayer[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
   IF mchPayer[1] = "" THEN DO:
      {find-act.i &acct=AcctCr &curr=AcctCrCur}
      IF AVAIL(acct) THEN
      DO:
         IF acct.cust-cat =  "Ю" THEN 
         DO:
            RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mchPayer[1]).
            RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mchPayer[2]).    
         END.
         ELSE
            {getcust.i &name=mchPayer}
         mchPayer[1] = mchPayer[1] + " " + mchPayer[2].
         IF CAN-DO(FGetSetting("КассСчИНН","",?), SUBSTR(acct.acct,1,5))
         OR GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да" THEN DO:
            mINN        = IF FGetSetting("ИНН",?,?) <> "" THEN FGetSetting("ИНН",?,?) ELSE "000000000000".
            mchPayer[1] = (IF mINN <> "000000000000" THEN "ИНН " + mINN + " " ELSE "").
            mchPayer[1] = mchPayer[1] + mchBankName.
         END.
      END.
   END.

   IF INDEX(mchPayer[1],"ИНН  ") = 1 THEN
      mchPayer[1] = SUBSTR(mchPayer[1],6,LENGTH(mchPayer[1])).

   IF CAN-DO(mInnKas,SUBSTR(AcctCr,1,5)) AND INDEX(mchPayer[1],"ИНН ") = 1 THEN
      mchPayer[1] = TRIM(SUBSTRING(mchPayer[1],INDEX(mchPayer[1]," ",5))).

   /* пасп данные плательщика */
   IF fGetSetting ("ПлатДок", "ВыводПасп", "") =  "Да" THEN
   DO:
      /* идент.типа документа */
      mchIdentCard = GetXAttrValueEx("op", STRING(op.op), "document-id", "").
      /* тип документа из классиф. КодДокум */
      mchIdentCard = IF mchIdentCard <> "" THEN GetCodeName("КодДокум", mchIdentCard) ELSE "".

      /* сливаем тип документа со значением ДР Докум (там обычно номер и кем выдан) */
      mchIdentCard = TRIM(mchIdentCard +
                          " " +
                          GetXAttrValueEx("op", STRING(op.op), "Докум", "")).

      mchPayer[1] = mchPayer[1] + " " + mchIdentCard.
   END.

   &IF DEFINED(LAW_318p) <> 0 &THEN
      IF AVAIL(acct) AND acct.cust-cat <> "В" THEN
         mRecINN   = FGetSetting("ИНН",?,"").
      mRecKPP   = FGetSetting("БанкКПП",?,"").
      mRecOKATO = FGetSetting("БанкОКАТО",?,"").
      IF {assigned mCrBranchOKATO}
         THEN mRecOKATO = mCrBranchOKATO.
   &ENDIF
   ASSIGN
      mchReceiver[1] = mchBankName
      mKasOrdNS      = FGetSetting("КасОрдНС",?,"")
   .

   CASE CrCustCat:
      WHEN "В" THEN DO: /* внутрибанк */
         IF mKasOrdNS =  "банк" THEN
            ASSIGN
               mchReceiver[1] = FGetSetting("БанкС", "", "")
               mRecINN        = FGetSetting("ИНН", "", "")
               mRecKPP        = FGetSetting("БанкКПП", "", "")
            .
         ELSE IF mKasOrdNS =  "счет" THEN DO:
            {find-act.i &acct=AcctCr &curr=AcctCrCur}
            IF AVAIL acct THEN
               mchReceiver[1] = acct.Details.
         END.
         ELSE IF mKasOrdNS = "Подразд" THEN DO:
            {find-act.i &acct=AcctCr
                        &curr=AcctCrCur}
            IF AVAILABLE acct THEN
               FIND FIRST branch WHERE
                  branch.branch-id = acct.branch-id
               NO-LOCK NO-ERROR.
            IF AVAILABLE branch THEN
               ASSIGN
                  mRecINN        = GetXAttrValueEx("branch",
                                                   Surrogate(BUFFER branch:HANDLE),
                                                   "ИНН",
                                                   "")
                  mRecKPP        = GetXAttrValueEx("branch",
                                                   Surrogate(BUFFER branch:HANDLE),
                                                   "КПП",
                                                   "")
               .
            IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  'Да' THEN
               RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.Branch-Id,
                                                      OUTPUT mchReceiver[1]).
            ELSE 
               mchReceiver[1] = branch.name.

         END.
         ELSE DO:
            RUN GetCustIdCli IN h_acct (INPUT  AcctCr + "," + AcctCrCur,
                                        OUTPUT CrCustCat1,
                                        OUTPUT mIdCustAttr).
            RUN GetCustName IN h_base (CrCustCat1, mIdCustAttr, ?,
                                       OUTPUT mchReceiver[1],
                                       OUTPUT mchReceiver[2],
                                       INPUT-OUTPUT mINN).
            mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
            IF NOT {assigned mchReceiver[1]} THEN DO:
               IF {assigned mDbBranchNam}
               THEN mchReceiver[1] = mDbBranchNam.
               ELSE mchReceiver[1] = mchBankName.
            END.
            ELSE DO:
               &IF DEFINED(LAW_318p) <> 0 &THEN
                  CASE CrCustCat1:
                     WHEN "Ю" THEN mCustTable1 = "cust-corp".
                     WHEN "Ч" THEN mCustTable1 = "person".
                     WHEN "Б" THEN mCustTable1 = "banks".
                  END CASE.
                  IF  mCustTable1 <> ""
                  AND {assigned mIdCustAttr} THEN DO:
                     mRecKPP   = GetXAttrValueEx(mCustTable1, mIdCustAttr, "КПП", "").
                     mRecOKATO = GetXAttrValueEx(mCustTable1, mIdCustAttr, "ОКАТО-НАЛОГ", "").
                     mRecAcct  = LocalGetRAcct(CrCustCat1, INT64(mIdCustAttr), op.op, op.op-date).
                     mRecAcct2 = LocalGetCrAcct2(op.op, AcctDb, AcctCr, AcctKomis).
                  END.
                  ELSE ASSIGN
                     mRecKPP   = ""
                     mRecOKATO = ""
                     mRecAcct  = ""
                     mRecAcct2 = ""
                  .
               &ENDIF
            END.
         END.
      END.
      WHEN "Ю" OR WHEN "Ч" OR WHEN "Б" THEN DO: /* юр, физлица и банки */
         IF CAN-DO(mKasSchPol,SUBSTR(AcctCr,1,5)) THEN DO:
            mchReceiver[1] = mDbBranchNam.
            IF TRIM(mchReceiver[1]) =  "" THEN
               mchReceiver[1] = mchBankName.
         END.
         ELSE DO:
            ASSIGN
               mchReceiver[1] = ""
               mRecKPP        = ""
               mRecOKATO      = ""
               mRecAcct       = ""
               mRecAcct2      = ""
            .
            IF  AcctCr <> ""
            AND AcctCr <> ? THEN DO:
               {find-act.i &acct=AcctCr &curr=AcctCrCur}
               IF  AVAIL(acct) AND acct.cust-cat = "Ю" THEN DO:
                  &IF DEFINED(LAW_318p) <> 0 &THEN
                     RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mRecInn).
                     RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mchReceiver[2]).
                  &ELSE
                     RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mchReceiver[1]).
                     RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mchReceiver[2]).
                  &ENDIF
               END.
               ELSE DO:
                  &IF DEFINED(LAW_318p) <> 0 &THEN
                     {getcust.i &name=mchReceiver &OFFinn=yes &inn=mRecInn &findcust=YES}
                  &ELSE
                     {getcust.i &name=mchReceiver}
                  &ENDIF
               END.
               mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
               &IF DEFINED(LAW_318p) <> 0 &THEN
                  IF  CrCustCat = "Ч" THEN DO:
                     mRecKPP   = GetXAttrValueEx("person", STRING(acct.cust-id), "КПП", "").
                     mRecOKATO = GetXAttrValueEx("person", STRING(acct.cust-id), "ОКАТО-НАЛОГ", "").
                     mRecAcct  = LocalGetRAcct(CrCustCat, acct.cust-id, op.op, op.op-date).
                     mRecAcct2 = LocalGetCrAcct2(op.op, AcctDb, AcctCr, AcctKomis).
                  END.
                  IF  CrCustCat = "Ю" THEN DO:
                     mRecKPP   = GetXAttrValueEx("cust-corp", STRING(acct.cust-id), "КПП", "").
                     mRecOKATO = GetXAttrValueEx("cust-corp", STRING(acct.cust-id), "ОКАТО-НАЛОГ", "").
                     mRecAcct  = LocalGetRAcct(CrCustCat, acct.cust-id, op.op, op.op-date).
                     mRecAcct2 = LocalGetCrAcct2(op.op, AcctDb, AcctCr, AcctKomis).
                  END.
                  IF  CrCustCat = "Б" THEN DO:
                     mRecKPP   = GetXAttrValueEx("banks", STRING(acct.cust-id), "КПП", "").
                     mRecOKATO = GetXAttrValueEx("banks", STRING(acct.cust-id), "ОКАТО-НАЛОГ", "").
                     mRecAcct  = LocalGetRAcct(CrCustCat, acct.cust-id, op.op, op.op-date).
                     mRecAcct2 = LocalGetCrAcct2(op.op, AcctDb, AcctCr, AcctKomis).
                  END.
               &ENDIF
            END.
         END.
      END.
   END CASE.
   IF mRecINN = ? THEN mRecINN = "".

   IF INDEX(mchReceiver[1],"ИНН  ") = 1 THEN
      mchReceiver[1] = SUBSTR(mchReceiver[1],6,LENGTH(mchReceiver[1])).

   IF CAN-DO(mInnKas,SUBSTR(AcctCr,1,5)) AND INDEX(mchReceiver[1],"ИНН ") = 1 THEN
      mchReceiver[1] = TRIM(SUBSTRING(mchReceiver[1],INDEX(mchReceiver[1]," ",5))).

   ASSIGN
      DocCur     = AcctDbCur
      minCount   = 0
      mdeDocSum  = 0
      mdeNatSum  = 0
      mdeDragSum = 0
      mDrag      = GetXAttrValueEx("currency",DocCur,"Драг","")
   .
   _symbin:
   FOR EACH op-entry OF op WHERE op-entry.acct-db = AcctDb 
   &IF DEFINED(valcashord) <> 0 &THEN
      AND NOT CAN-DO(NalRKROtr,(IF op-entry.acct-cr =  ? THEN "*" ELSE op-entry.acct-cr))
      AND NOT CAN-DO(NalRKRPol,(IF op-entry.acct-cr =  ? THEN "*" ELSE op-entry.acct-cr))
   &ENDIF
   NO-LOCK:
       IF NOT mIsInOut THEN
       DO:

        IF GetTCodeFld
         ("val",
          "КасСимволы",
          op-entry.symbol,
          op-entry.op-date) BEGINS "прих" THEN
          mIsSymIn = YES.
       END.

       IF op-entry.symbol <> "" AND mIsInOut THEN
       DO:
         IF GetTCodeFld
         ("val",
          "КасСимволы",
          op-entry.symbol,
          op-entry.op-date)

         BEGINS "прих"
         THEN
         DO:
            mchSymFromNP = FGetSetting("КасСимволы", "прих-" + op-entry.symbol,"").
            mIsSymIn = YES.
         END.
         ELSE
         DO:
            mchSymFromNP = FGetSetting("КасСимволы", "расх-" + op-entry.symbol ,"").
            mIsSymIn = NO.
         END.
      END.

      IF DocCur = "" THEN
         mdeDocSum = mdeDocSum + op-entry.amt-rub.
      ELSE IF (op.op-date  <> ? AND
               op.op-date  <  DATE("01/11/2014")) OR
               op.doc-date <  DATE("01/11/2014")
      THEN
         ASSIGN
            mdeNatSum = mdeNatSum + op-entry.amt-rub
            mdeDocSum = mdeDocSum + op-entry.amt-cur
         .
      ELSE DO:
         ASSIGN
            mdeNatSum  = mdeNatSum + op-entry.amt-rub
            &IF DEFINED(valcashord) =  0 &THEN
               mdeDocSum  = IF mDrag =  "Да" THEN mdeDocSum  + op-entry.amt-rub 
                                             ELSE mdeDocSum  + op-entry.amt-cur
            &ELSE
               mdeDocSum = mdeDocSum + op-entry.amt-cur
            &ENDIF
            mdeDragSum = IF mDrag =  "Да" THEN mdeDragSum + op-entry.amt-cur
                                          ELSE 0
         .
      END.

       IF NOT mIsSymIn THEN
          TmpSymbol = IF mchOrdTypeDb =  "расходный" THEN op-entry.symbol
                                                     ELSE mchSymFromNP.
       ELSE
          TmpSymbol = IF mchOrdTypeDb =  "приходный" THEN op-entry.symbol
                                                     ELSE mchSymFromNP.
       IF TmpSymbol <> "" THEN DO:
           IF NOT GetTCodeFld("val",
                              "КасСимволы",
                              TmpSymbol,
                              op-entry.op-date) BEGINS "прих"
           THEN
               NEXT _symbin.
           IF minCount = EXTENT(mdeSymSumIn) THEN DO:
               MESSAGE
                   "Не могу обработать более"
                   STRING(EXTENT(mdeSymSumIn))
                   "символов"
               VIEW-AS ALERT-BOX.
               LEAVE.
           END.

           mNumArray = NumArraySymCod(mchSymCodIn,TmpSymbol).
           IF mNumArray <> 0 THEN
               mdeSymSumIn[mNumArray] = mdeSymSumIn[mNumArray] +
                                             IF DocCur = "" THEN op-entry.amt-rub
                                                            ELSE op-entry.amt-cur.
           ELSE
             ASSIGN
               minCount              = minCount + 1
               mchSymCodIn[minCount] = TmpSymbol
               mdeSymSumIn[minCount] = IF DocCur = "" THEN op-entry.amt-rub
                                                      ELSE op-entry.amt-cur
           .
       END.
   END.
   mDetails = GetXAttrValueEx("op", STRING(op.op), "Основание", "").
   mDetails = (IF LENGTH(mDetails) >  2 THEN mDetails + "~n" ELSE "") +
              op.Details
   .

   &IF DEFINED(LAW_318p) <> 0 &THEN
      ASSIGN
         mPayBank[1] = mchRecBank[1]
         mPayBankBik = mchBankBIK
      .
      IF CAN-DO(FGetSetting("ПлатДок", "ВывИННПол",""), SUBSTR(acct.acct,1,5)) THEN ASSIGN
         mRecInn   = ""
         mRecKPP   = ""
         mRecOKATO = ""
         mRecAcct  = ""
         mRecAcct2 = ""
      .

      mTmpStr = GetXAttrValueEx("op", STRING(op.op), "name-rec", "").
      IF {assigned mTmpStr} THEN DO:
         mchReceiver = mTmpStr.
         mRecINN     = GetXAttrValueEx("op", STRING(op.op), "INN-rec", "").
         mRecKPP     = GetXAttrValueEx("op", STRING(op.op), "KPP-rec", "").
         mRecOKATO   = GetXAttrValueEx("op", STRING(op.op), "OKATO-rec", "").
      END.

      IF   {assigned op.ben-acct}
      THEN mRecAcct = op.ben-acct.

      FIND op-bank OF op NO-LOCK NO-ERROR.
      IF AMBIGUOUS op-bank
      THEN FIND FIRST op-bank OF op WHERE op-bank.op-bank-type = "" NO-LOCK NO-ERROR.
      IF AVAIL op-bank THEN 
         ASSIGN
            mOurBank      = (op-bank.bank-code =  mchBankBIK)
            mchRecBank[1] = op-bank.bank-name
            mchBankBIK    = op-bank.bank-code
         .

   &ENDIF
   mdeCrMainSum = mdeDocSum - mdeCrCommSum.
   &IF DEFINED(valcashord) =  0 &THEN DO:
      IF mDbContract =  "Касса" AND CrContract =  "Касса" AND
        (GetXAttrValueEx("acct",AcctDb + ',' + AcctDbCur,"ВечерКас","") =  "Да" OR
         GetXAttrValueEx("acct",AcctCr + ',' + AcctCrCur,"ВечерКас","") =  "Да") AND
         (mKasKas =  "Нет" OR mKasKas =  "") THEN
      DO:
         mKasNoFill = YES.
           
      end.   
      AcRepPolKas = FGetSetting("ПлатДок","СчЗапПолКас","70601*,454*,47427*").
      /* Оплата комиссий, кредитов банка, процентов */
      FIND FIRST bop-entry OF op WHERE
        (CAN-DO(AcRepPolKas,bop-entry.acct-db)  OR
         CAN-DO(AcRepPolKas,bop-entry.acct-cr))
      NO-LOCK NO-ERROR.
      IF AVAIL(bop-entry) OR mKasNoFill THEN DO:
         ASSIGN
            mchReceiver[1] = mchBankName
            mchRecBank     = ""
            mchBankBIK     = ""
            mPayBank       = ""
            mPayBankBik    = ""
            mRecInn        = ""
            mRecAcct       = ""
         .
         CASE mKasOrdNS:
            WHEN "банк" THEN mchReceiver[1] = FGetSetting("БанкС", "", "").
            WHEN "счет" 
               THEN DO:
                  {find-act.i &acct=AcctCr &curr=AcctCrCur}
                  IF AVAIL acct THEN
                     mchReceiver[1] = acct.Details.
               END.
            WHEN "Подразд" 
               THEN DO:
                  {find-act.i &acct=AcctCr &curr=AcctCrCur}
                  IF AVAILABLE acct THEN DO:
                     IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  'Да' THEN
                        RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.Branch-Id,
                                                               OUTPUT mchReceiver[1]).
                     ELSE DO:
                        FIND FIRST branch WHERE branch.branch-id = acct.branch-id NO-LOCK NO-ERROR.
                        IF AVAILABLE branch THEN
                           mchReceiver[1] = branch.name.
                     END.
                  END.
               END.
         END CASE.
      END.
      ELSE DO:
         /* Внесение на счет/вклад Банка */
      /*   mchReceiver[1] = mchBankName  + " " + mchReceiver[1].*/
         /*IF {assigned mRecAcct} THEN DO:*/
            {find-act.i &acct=AcctDb &curr=AcctDbCur}
            IF AVAIL(acct) THEN DO:
              IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
                RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
                                                       OUTPUT mPayBank[1]).
                IF mIsPrtCity THEN mPayBank[1] = mPayBank[1] + mchBankSity.
              END.
              ELSE
                mPayBank[1]   = GetValueAttr("branch", acct.branch-id, mFldNameBnk) 
                                             + ", " + mPayBank[1].
            END.
            IF mOurBank THEN
            DO:
               {find-act.i &acct=AcctCr &curr=AcctCrCur}
               IF AVAIL(acct) THEN DO:
                  IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
/* Замена Плюс банк
                     RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
*/                   RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.filial-id,
/* Конец замены Плюс банк */
                                                            OUTPUT mchRecBank[1]).
                     IF mIsPrtCity THEN mchRecBank[1] = mchRecBank[1] + mchBankSity.
                  END.
                  ELSE
                     mchRecBank[1] = GetValueAttr("branch", acct.branch-id, mFldNameBnk) 
                                     + ", " + mchRecBank[1].
               END.
            END.
         /*END.*/
      END.
   END.
   &ENDIF

   FIND FIRST op-entry OF op WHERE op-entry.acct-db =  AcctDb NO-LOCK NO-ERROR.
   {find-act.i &acct=AcctDb}
   IF AVAIL(acct) AND AVAIL(op-entry) THEN
      RUN GetCashDocTypeDigital IN h_op (BUFFER op-entry,
                                         acct.acct,
                                         OUTPUT mCodeDoc).

   CREATE tt-op-entry.
   BUFFER-COPY op-entry TO tt-op-entry NO-ERROR.
   ASSIGN
      tt-op-entry.op-entry-half-db = op-entry.op-entry
   .
   RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
   &IF DEFINED(valcashord) &THEN
      mEmptyField = mVarVal[22].
   &ELSE
      mEmptyField = mVarVal[24].
   &ENDIF

   &IF DEFINED(LAW_318p) <> 0 &THEN
   IF mDbContract =  "Касса" AND CrContract =  "Касса" AND
      GetXAttrValueEx("acct",AcctDb + ',' + AcctDbCur,"ВечерКас","") NE
      GetXAttrValueEx("acct",AcctCr + ',' + AcctCrCur,"ВечерКас","")
   THEN
   DO:
      mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "name-send","").
      IF NOT {assigned mchPayer[1]} THEN
      DO:
         FIND FIRST _user WHERE _user._userid =  op.user-id NO-LOCK NO-ERROR.
         IF AVAIL _user THEN mchPayer[1] =_user._user-Name.
      END.
   END.
   &ENDIF
   
   {docform_new.i
      &cashord      = ""приходный""
      &docdate      = mdtDateDoc
      &docnum       = op.doc-num
      &codedoc      = mCodeDoc
      &payer        = mchPayer
      &receiver     = mchReceiver
      &recbank      = mchRecBank
      &dbacct       = AcctDb
      &cracct       = AcctCr
      &cracct2      = AcctCr2
      &docsum       = mdeDocSum
      &dragsum      = mdeDragSum
      &doccur       = DocCur
      &symsumin     = mdeSymSumIn
      &symsumout    = mdeSymSumOut
      &natsum       = mdeNatSum
      &symcodin     = mchSymCodIn
      &symcodout    = mchSymCodOut
      &details      = mDetails
      &identcard    = mchIdentCard
      &documentid   = mDocCodName
      &documentnum  = mDocNum
      &documentwho  = mCustDocWho
      &documentdate = _str_2_DATE(mDocDate)
      &recinn       = mRecInn
      &recbankbik   = mchBankBIK
      &reckpp       = mRecKPP
      &recokato     = mRecOKATO
      &recacct      = mRecAcct
      &recacct2     = mRecAcct2
      &paybank      = mPayBank
      &paybankbik   = mPayBankBik
      &acctkomis    = AcctKomis
      &inc_part_fio = mchFIO
      &emptyfield   = mEmptyField
      &emptyfield2  = mEmptyField2
      &worker_buh_post  = mchWorkerBuhP
      &worker_buh_fio   = mchWorkerBuh
      &worker_kont_post = mchWorkerKontP
      &worker_kont_fio  = mchWorkerKont
      &worker_kas_post  = mchWorkerKasP
      &worker_kas_fio   = mchWorkerKas
      &wordwrap = YES
   }
END.

IF mchOrdTypeCr = "расходный" THEN DO:
   mTmpStr = FGetSetting("КасОрдНС",?,"").
   &IF DEFINED(valcashord) <> 0 &THEN
      mStrPar = FGetSetting("ПлатДок", "ВалКО", "").
   &ELSE
      mStrPar = FGetSetting("ПлатДок", "РасхКО", "").
   &ENDIF
   RUN ProcProcessFreeField(mStrPar,?,?).

   IF (mTmpStr =  "банк") AND {assigned TRIM(mCrBranchNam)} THEN
      mchRecBank[1] = mCrBranchNam + ", " + mchBankName.
   ELSE 
      mchRecBank[1] = mchBankName.
   
   IF mTmpStr =  "счет" THEN DO:
      {find-act.i &acct=AcctCr &curr=AcctCrCur}
      IF AVAIL acct THEN 
         mchPayer[1] = acct.Details.
   END.
   ELSE
      mchPayer[1] = "".

   IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
      {find-act.i &acct=AcctCr &curr=AcctCrCur}
      IF AVAIL(acct) THEN
         RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
                                                OUTPUT mchRecBank[1]).
   END.

   IF mIsPrtCity THEN mchRecBank[1] = mchRecBank[1] + mchBankSity.

   /* получатель */
   mchReceiver[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
   IF mchReceiver[1] = "" THEN
      mchReceiver[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
   IF mchReceiver[1] = "" THEN DO:
      IF DbCustCat =  "В" THEN DO:
         RUN GetCustIdCli IN h_acct (INPUT  AcctDb + "," + AcctDbCur,
                                     OUTPUT DbCustCat1,
                                     OUTPUT mIdCustAttr).
         RUN GetCustName IN h_base (DbCustCat1, mIdCustAttr, ?,
                                    OUTPUT mchReceiver[1],
                                    OUTPUT mchReceiver[2],
                                    INPUT-OUTPUT mINN
         ).
         mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
      END.
      IF TRIM(mchReceiver[1]) =  "" THEN DO:
         {find-act.i &acct=Acctdb &bact=cacct }
         IF AVAIL(cacct) AND cacct.cust-cat =  "Ю" THEN DO:
            RUN GetCustInfo2 (13, cacct.acct, cacct.currency, OUTPUT mchReceiver[1]).
            RUN GetCustNameFormatted (cacct.cust-cat, cacct.cust-id, OUTPUT mchReceiver[2]).
         END.
         ELSE DO:       
            {getcust2.i AcctDb mchReceiver}
         END.
         mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
      END.
   END.

   IF INDEX(mchReceiver[1],"ИНН  ") = 1 THEN
      mchReceiver[1] = SUBSTR(mchReceiver[1],6,LENGTH(mchReceiver[1])).

   IF CAN-DO(mInnKas,SUBSTR(AcctDb,1,5)) AND INDEX(mchReceiver[1],"ИНН ") = 1 THEN
      mchReceiver[1] = TRIM(SUBSTRING(mchReceiver[1],INDEX(mchReceiver[1]," ",5))).

   mDocument-id = GetXAttrValueEx("op",STRING(Op.op),"document-id","").
   mDocNum      = GetXAttrValueEx("op",STRING(op.op), "Докум", "").
   RUN ParseDocum (INPUT-OUTPUT mDocument-id, INPUT-OUTPUT mDocNum, OUTPUT mDocCodName, OUTPUT mCustDocWho[1], OUTPUT mDocDate, OUTPUT mPassKP).
   mCustDocWho[1] = GetXAttrValueEx("op",STRING(Op.op),"cust-doc-who",mCustDocWho[1]).
   mDocDate       = GetXAttrValueEx("op",STRING(Op.op),"Document4Date_vid",mDocDate).
   IF mVOKDprID <> "" THEN /* документ не созданный ВОК */
   /* документ, созданный в ВОК */
   DO:
      mPassKP = GetXAttrValueEx("op",STRING(Op.op),"Подразд",mPassKP).
      IF NUM-ENTRIES(mCustDocWho[1]) > 1 THEN
      DO:
         IF mPassKP = "" THEN
            mPassKP = ENTRY(2,mCustDocWho[1]).
         ENTRY(2,mCustDocWho[1]) = "".
      END.   
         
      mchIdentCard = (IF mDocCodName <> ? 
                         THEN mDocCodName 
                         ELSE mDocument-id) +
                     (IF mDocNum <> ""
                         THEN " № " + mDocNum
                         ELSE "") +
                     (IF mCustDocWho[1] <> ""
                         THEN "~nВыдан " + mCustDocWho[1]
                         ELSE "") +
                     (IF mPassKP <> ""
                         THEN " к~/п " + mPassKP
                         ELSE "") +                         
                     IF mDocDate <> "" THEN (", дата выдачи:" + mDocDate) ELSE "".
   END. /* документ, созданный в ВОК */
   ELSE
   DO:
   /* пасп данные получателя */
   /* идент.типа документа */
      /* тип документа из классиф. КодДокум */
      /* сливаем тип документа со значением ДР Докум (там обычно номер и кем выдан) */
      mPassKP = GetXAttrValueEx("op",STRING(Op.op),"Подразд",mPassKP).
      mchIdentCard = TRIM(mDocCodName + " " + GetXAttrValueEx("op", STRING(op.op), "Докум", "")).
   END.
   IF NOT {assigned mchIdentCard} THEN DO:
      /* доп.рек. Докум не указан */
      mchIdentCard = GetXAttrValueEx("op", STRING(op.op), "Passport", "").
      IF {assigned mchIdentCard} THEN DO:
         /* доп.рек. Passport указан */
         ASSIGN
            mDocument-id = ""
            mDocNum      = mchIdentCard
         .
         RUN ParseDocum (INPUT-OUTPUT mDocument-id, INPUT-OUTPUT mDocNum, OUTPUT mDocCodName, OUTPUT mCustDocWho[1], OUTPUT mDocDate, OUTPUT mPassKP).
      END.
      ELSE DO:
         /* доп.рек. Passport не указан */
         FIND FIRST acct WHERE acct.acct = AcctDb NO-LOCK NO-ERROR.
         IF AVAILABLE acct THEN DO:
            mPersonId = IF acct.cust-cat = "Ч" THEN /* клиента будем искать по cust-id счета */
                           acct.cust-id
                        ELSE IF acct.cust-cat = "В" AND
                                GetXattrValueEx("acct", AcctDb + "," + AcctDbCur, "ТипКл", "") =  "Ч"
                             THEN /* клиента будем искать по д.р. IDCust счета */
                                INT64(GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "IDCust", ?))
                        ELSE
                           ?
            .
            IF mPersonId <> ? THEN DO:
               FIND FIRST person WHERE person.person-id = mPersonId NO-LOCK NO-ERROR.
               IF AVAILABLE person THEN DO:
                  mDocCodName    = GetCodeName("КодДокум", person.document-id).
                  mDocNum        = person.document.
                  IF  {assigned mDocCodName}
                  AND {assigned mDocNum} THEN DO:
                  END.
                  FIND FIRST xcust-ident WHERE xcust-ident.cust-cat       = "Ч"
                                           AND xcust-ident.cust-id        = person.person-id
                                           AND xcust-ident.cust-code-type = person.document-id
                                           AND xcust-ident.cust-code      = person.document
                  NO-LOCK NO-ERROR.
                  IF AVAIL xcust-ident THEN DO:
                     IF {assigned xcust-ident.issue}
                     THEN mCustDocWho[1] = xcust-ident.issue.
                     ELSE mCustDocWho[1] = person.issue.
                     IF xcust-ident.open-date <> ?
                     THEN mDocDate = _date_2_Str(xcust-ident.open-date).
                     ELSE mDocDate = GetXattrValueEx("person", STRING(person.person-id), "Document4Date_Vid", "").
                     mPassKP = GetXattrValueEx("cust-ident",
                                               GetSurrogateBuffer("cust-ident",(BUFFER xcust-ident:HANDLE)),
                                               "Подразд",
                                               "").
                  END.
                  ELSE DO:
                     mCustDocWho[1] = person.issue.
                     mDocDate       = GetXattrValueEx("person", STRING(person.person-id), "Document4Date_Vid", "").
                  END.
                  mchIdentCard = (IF mDocCodName <> ? THEN mDocCodName ELSE person.document-id)
                               + " N " + mDocNum + ", выдан "
                               + TRIM(person.issue + " " + mDocDate).
                  IF  {assigned mCustDocWho[1]}
                  AND {assigned mPassKP} THEN DO:
                     IF  NUM-ENTRIES(mCustDocWho[1]) >= 2
                     AND TRIM(ENTRY(NUM-ENTRIES(mCustDocWho[1]),mCustDocWho[1])) = TRIM(mPassKP)
                     THEN ASSIGN
                        ENTRY(NUM-ENTRIES(mCustDocWho[1]),mCustDocWho[1]) = ""
                        mCustDocWho[1] = TRIM(mCustDocWho[1],",")
                     .
                  END.
               END.
            END.
         END.
      END.
   END.
   
   IF  {assigned mPassKP}
   AND INDEX(mCustDocWho[1]," к/п ") = 0
   THEN ASSIGN mCustDocWho[1] = mCustDocWho[1] + " к/п " + mPassKP.
/*
   IF {assigned mDocDate}
      THEN mDocDate = term2str(_str_2_DATE(mDocDate),_str_2_DATE(mDocDate)) NO-ERROR.
*/

   DO mInCount = 1 TO EXTENT(mdeSymSumOut):
      /* чистим массивы после печати прих.ордера */
      mdeSymSumOut[mInCount] = 0.
      mchSymCodOut[mInCount] = "".
   END.
   ASSIGN
      DocCur     = AcctCrCur
      mdeNatSum  = 0
      minCount   = 0
      mdeDocSum  = 0
      mdeDragSum = 0
      mDrag      = GetXAttrValueEx("currency",DocCur,"Драг","")
   .

   mProxyCode = GetXAttrValue ("op",
                               STRING(op.op),
                               "proxy-code").
   IF NUM-ENTRIES(mProxyCode) >= 2 THEN
   mProxyCode = ENTRY(2,mProxyCode).   
   IF {assigned mProxyCode} THEN       
   DO:
      FIND FIRST bProxy WHERE bProxy.contract   =  "proxy"
                          AND bProxy.cont-code  =  mProxyCode
         NO-LOCK NO-ERROR.
      IF AVAIL bProxy THEN
      DO:
         mAgentID = GetXAttrValue ("loan",
                                   bProxy.contract + "," + bProxy.cont-code,
                                   "agent-id").
         IF {assigned mAgentID} THEN
         DO:
            FIND FIRST bAgent WHERE bAgent.person-id =  INT64(mAgentID)
               NO-LOCK NO-ERROR.
            IF AVAIL bAgent THEN
            DO:
               mDrowerID = GetXAttrValue ("loan",
                                          bProxy.contract + "," + bProxy.cont-code,
                                          "drower-id").
               IF {assigned mDrowerID} THEN                  
                  FIND FIRST bDrower WHERE bDrower.person-id =  INT64(mDrowerID)
                     NO-LOCK NO-ERROR.

               ASSIGN
                  mchReceiver[1]  = bAgent.name-last + " " + bAgent.first-names.

               IF FGetSetting("КасОрдНП",?,"") =  "Да" THEN
                  mchDetails[1] = "Выдача денежной наличности по доверенности от " 
                                       + STRING(bProxy.open-date) 
                                       + (IF {assigned bProxy.doc-num} THEN " N " + bProxy.doc-num
                                                                       ELSE "")
                                       + " за " + (IF AVAIL bDrower THEN bDrower.name-last + " " + bDrower.first-names
                                                                                               ELSE "").
               ELSE
                  mchDetails[1] = op.details.

               FIND FIRST bCustIdent WHERE bCustIdent.class-code     =  "p-cust-ident"
                                       AND bCustIdent.cust-code-type =  bAgent.document-id
                                       AND bCustIdent.cust-cat       =  "Ч"
                                       AND bCustIdent.cust-id        =  bAgent.person-id
                                       AND bCustIdent.close-date =  ?
                  NO-LOCK NO-ERROR.
               IF AVAIL bCustIdent THEN
               DO:
                  mDocument-id = GetXAttrValueEx("op",STRING(Op.op),"document-id",""). 
                  mDocNum      = GetXAttrValueEx("op",STRING(op.op), "Докум", "").
                  IF {assigned mDocNum} THEN
                  DO:
                     RUN ParseDocum (INPUT-OUTPUT mDocument-id, INPUT-OUTPUT mDocNum, OUTPUT mDocCodName, OUTPUT mCustDocWho[1], OUTPUT mDocDate, OUTPUT mPassKP).
                     IF NOT {assigned  mCustDocWho[1]} THEN
                        mCustDocWho[1] = GetXAttrValueEx("op",
                                                         STRING(Op.op),
                                                         "cust-doc-who",
                                                         "").
                     IF NOT {assigned  mDocDate} THEN
                        mDocDate    = GetXAttrValueEx("op",
                                                      STRING(Op.op),
                                                      "Document4Date_vid",
                                                      "").
                     IF NOT {assigned  mPassKP} THEN
                        mPassKP = GetXAttrValueEx("op",STRING(Op.op),"Подразд","").
                  END.
                  ELSE
                     ASSIGN
                        mDocCodName    = GetCodeName ("КодДокум", bAgent.document-id)
                        mDocNum        = bCustIdent.cust-code
                        mCustDocWho[1] = bCustIdent.issue
                        mDocDate       = _date_2_Str(bCustIdent.open-date)
                        mPassKP        = GetXattrValueEx("cust-ident",
                                                         GetSurrogateBuffer("cust-ident",
                                                               (BUFFER bCustIdent:HANDLE)),
                                                         "Подразд",
                                                         "")
                        .

                  IF  {assigned mPassKP}
                  AND INDEX(mCustDocWho[1]," к/п ") = 0
                  THEN ASSIGN mCustDocWho[1] = mCustDocWho[1] + " к/п " + mPassKP.

               END.
            END.
            
         END.
      END.
   END.
   ELSE
      mChDetails[1] = op.details.

   _symbout:
   FOR EACH op-entry OF op WHERE op-entry.acct-cr = AcctCr
   &IF DEFINED(valcashord) <> 0 &THEN
      AND NOT CAN-DO(NalRKROtr,(IF op-entry.acct-db =  ? THEN "*" ELSE op-entry.acct-db))
      AND NOT CAN-DO(NalRKRPol,(IF op-entry.acct-db =  ? THEN "*" ELSE op-entry.acct-db))
   &ENDIF
   NO-LOCK:
      IF DocCur = "" THEN
         mdeDocSum = mdeDocSum + op-entry.amt-rub.
      ELSE IF (op.op-date  <> ? AND
               op.op-date  <  DATE("01/11/2014")) OR
               op.doc-date <  DATE("01/11/2014")
      THEN
         ASSIGN
            mdeNatSum = mdeNatSum + op-entry.amt-rub
            mdeDocSum = mdeDocSum + op-entry.amt-cur
         .
      ELSE DO:
         ASSIGN
            mdeNatSum  = mdeNatSum + op-entry.amt-rub
            &IF DEFINED(valcashord) =  0 &THEN
               mdeDocSum  = IF mDrag =  "Да" THEN mdeDocSum  + op-entry.amt-rub 
                                             ELSE mdeDocSum  + op-entry.amt-cur
            &ELSE
               mdeDocSum = mdeDocSum + op-entry.amt-cur
            &ENDIF
            mdeDragSum = IF mDrag =  "Да" THEN mdeDragSum + op-entry.amt-cur
                                          ELSE 0
         .
      END.

       IF NOT mIsSymIn THEN
          TmpSymbol = IF mchOrdTypeCr =  "расходный" THEN op-entry.symbol
                                                     ELSE mchSymFromNP.
       ELSE
          TmpSymbol = IF mchOrdTypeCr =  "приходный" THEN op-entry.symbol
                                                     ELSE mchSymFromNP.

       IF TmpSymbol <> "" THEN DO:
           IF NOT GetTCodeFld("val",
                              "КасСимволы",
                              TmpSymbol,
                              op-entry.op-date) BEGINS "расх"
           THEN
               NEXT _symbout.
           IF minCount = EXTENT(mdeSymSumOut) THEN DO:
               MESSAGE
                   "Не могу обработать более"
                   STRING(EXTENT(mdeSymSumOut))
                   "символов"
               VIEW-AS ALERT-BOX.
               LEAVE.
           END.

           mNumArray = NumArraySymCod(mchSymCodOut,TmpSymbol).
           IF mNumArray <> 0 THEN
              mdeSymSumOut[mNumArray] = mdeSymSumOut[mNumArray] +
                                             IF DocCur = "" THEN op-entry.amt-rub
                                                            ELSE op-entry.amt-cur.
           ELSE
             ASSIGN
               minCount               = minCount + 1
               mchSymCodOut[minCount] = TmpSymbol
               mdeSymSumOut[minCount] = IF DocCur = "" THEN op-entry.amt-rub
                                                       ELSE op-entry.amt-cur
           .
       END.
   END.
   mdeCrMainSum = mdeDocSum - mdeCrCommSum.
   FIND FIRST op-entry OF op WHERE op-entry.acct-cr =  AcctCr NO-LOCK NO-ERROR.
   {find-act.i &acct=AcctCr}
   IF AVAIL(acct) AND AVAIL(op-entry) THEN
      RUN GetCashDocTypeDigital IN h_op (BUFFER op-entry,
                                         acct.acct,
                                         OUTPUT mCodeDoc).

   CREATE tt-op-entry.
   BUFFER-COPY op-entry TO tt-op-entry NO-ERROR.
   ASSIGN
      tt-op-entry.op-entry-half-db = op-entry.op-entry
   .
   RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
   mEmptyField = mVarVal[22].

   ASSIGN
      mchWorkerBuhP = mchWorkerBuhPs
      mchWorkerKasP = mchWorkerKasPs
      mchWorkerBuh  = mchWorkerBuhs
      mchWorkerKas  = mchWorkerKass.

   {docform_new.i
      &cashord      = ""расходный""
      &nodef        = yes
      &docdate      = mdtDateDoc
      &docnum       = op.doc-num
      &codedoc      = mCodeDoc
      &payer        = mchPayer
      &receiver     = mchReceiver
      &recbank      = mchRecBank
      &dbacct       = AcctDb
      &cracct       = AcctCr
      &docsum       = mdeDocSum
      &dragsum      = mdeDragSum
      &doccur       = DocCur
      &symsumin     = mdeSymSumIn
      &symsumout    = mdeSymSumOut
      &natsum       = mdeNatSum
      &symcodin     = mchSymCodIn
      &symcodout    = mchSymCodOut
      &details      = mChDetails[1]
      &identcard    = mchIdentCard
      &documentid   = mDocCodName
      &documentnum  = mDocNum
      &documentwho  = mCustDocWho
      &documentdate = _str_2_DATE(mDocDate)
      &recinn       = mRecInn
      &recbankbik   = mchBankBIK
      &reckpp       = mRecKPP
      &recokato     = mRecOKATO
      &recacct      = mRecAcct
      &paybank      = mPayBank
      &paybankbik   = mPayBankBik
      &acctkomis    = AcctKomis
      &inc_part_fio = mchFIO
      &emptyfield   = mEmptyField
      &emptyfield2  = mEmptyField2
      &worker_buh_post  = mchWorkerBuhP
      &worker_buh_fio   = mchWorkerBuh
      &worker_kont_post = mchWorkerKontP
      &worker_kont_fio  = mchWorkerKont
      &worker_kas_post  = mchWorkerKasP
      &worker_kas_fio   = mchWorkerKas
      &wordwrap = YES
   }
END.

&IF DEFINED(LAW_318p) <> 0 &THEN
   IF mchOrdTypeCr = "приходно─расходный" THEN DO:
   mTmpStr = FGetSetting("КасОрдНС",?,"").
   &IF DEFINED(valcashord) <> 0 &THEN
      mStrPar = FGetSetting("ПлатДок", "ВалКО", "").
   &ELSE
      mStrPar = FGetSetting("ПлатДок", "ПрРасКО", "").
   &ENDIF
   RUN ProcProcessFreeField(mStrPar,?,?).

      IF mIsPrtCity THEN mchBankName = mchBankName + mchBankSity.
      ASSIGN
         mchRecBank[1] = mDbBranchNam + ", " + mchBankName
         mPayBank[1]   = mCrBranchNam + ", " + mchBankName
      .
      IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
         {find-act.i &acct=AcctCr &curr=AcctCrCur}
         IF AVAIL(acct) THEN
            RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
                                                   OUTPUT mchRecBank[1]).
         {find-act.i &acct=AcctDb &curr=AcctDbCur}
         IF AVAIL(acct) THEN
            RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
                                                   OUTPUT mPayBank[1]).
      END.

      mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "name-send", "").
      IF NOT {assigned mchPayer[1]} THEN DO:
         mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
         IF mchPayer[1] = "" THEN
            mchPayer[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
         IF mchPayer[1] = "" THEN DO:
            {find-act.i &acct=Acctcr &bact=cacct}
            IF AVAIL(cacct) AND cacct.cust-cat =  "Ю" THEN DO:
               RUN GetCustInfo2 (13, cacct.acct, cacct.currency, OUTPUT mchPayer[1]).
               RUN GetCustNameFormatted (cacct.cust-cat, cacct.cust-id, OUTPUT mchPayer[2]).
            END.
            ELSE DO:       
               {getcust2.i AcctCr mchPayer}
            END.
            mchPayer[1] = mchPayer[1] + mchPayer[2].
            IF CAN-DO(FGetSetting("КассСчИНН","",?), SUBSTR(acct.acct,1,5))
            OR GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да" THEN DO:
               mINN        = IF FGetSetting("ИНН",?,?) <> "" THEN FGetSetting("ИНН",?,?) ELSE "000000000000".
               mchPayer[1] = (IF mINN <> "000000000000" THEN "ИНН " + mINN + " " ELSE "").
               mchPayer[1] = mchPayer[1] + mchBankName.
            END.
         END.
         /* пасп данные плательщика */
         IF fGetSetting ("ПлатДок", "ВыводПасп", "") =  "Да" THEN
         DO:
            /* идент.типа документа */
            mchIdentCard = GetXAttrValueEx("op", STRING(op.op), "document-id", "").
            /* тип документа из классиф. КодДокум */
            mchIdentCard = IF mchIdentCard <> "" THEN GetCodeName("КодДокум", mchIdentCard) ELSE "".
            /* сливаем тип документа со значением ДР Докум (там обычно номер и кем выдан) */
            mchIdentCard = TRIM(mchIdentCard + " " +
                                GetXAttrValueEx("op", STRING(op.op), "Докум", "")).
            mchPayer[1] = mchPayer[1] + " " + mchIdentCard.
         END.
      END.

      IF INDEX(mchPayer[1],"ИНН  ") = 1 THEN
         mchPayer[1] = SUBSTR(mchPayer[1],6,LENGTH(mchPayer[1])).

      IF CAN-DO(mInnKas,SUBSTR(acct.acct,1,5)) AND INDEX(mchPayer[1],"ИНН ") = 1 THEN
         mchPayer[1] = TRIM(SUBSTRING(mchPayer[1],INDEX(mchPayer[1]," ",5))).

      /* получатель */
      mchFIO = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
      mchReceiver[1] = mchFIO.
      IF mchReceiver[1] = "" THEN
         mchReceiver[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
      IF mchReceiver[1] = "" THEN DO:
         IF DbCustCat =  "В" THEN DO:
            RUN GetCustIdCli IN h_acct (INPUT  AcctDb + "," + AcctDbCur,
                                        OUTPUT DbCustCat1,
                                        OUTPUT mIdCustAttr).
            RUN GetCustName IN h_base (DbCustCat1, mIdCustAttr, ?,
                                       OUTPUT mchReceiver[1],
                                       OUTPUT mchReceiver[2],
                                       INPUT-OUTPUT mINN
            ).
            mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
         END.
         IF TRIM(mchReceiver[1]) =  "" THEN DO:
            {find-act.i &acct=Acctdb &bact=cacct}
            IF AVAIL(cacct) AND cacct.cust-cat =  "Ю" THEN DO:
               RUN GetCustInfo2 (13, cacct.acct, cacct.currency, OUTPUT mchReceiver[1]).
               RUN GetCustNameFormatted (cacct.cust-cat, cacct.cust-id, OUTPUT mchReceiver[2]).
            END.
            ELSE DO:       
               {getcust2.i AcctDb mchReceiver}
            END.
            mchReceiver[1] = mchReceiver[1] + " " + mchReceiver[2].
         END.
      END.

      IF INDEX(mchReceiver[1],"ИНН  ") = 1 THEN
         mchReceiver[1] = SUBSTR(mchReceiver[1],6,LENGTH(mchReceiver[1])).

      IF CAN-DO(mInnKas,SUBSTR(AcctDb,1,5)) AND INDEX(mchReceiver[1],"ИНН ") = 1 THEN
         mchReceiver[1] = TRIM(SUBSTRING(mchReceiver[1],INDEX(mchReceiver[1]," ",5))).

      mVOKDprID    = GetXAttrValueEx("op", STRING(op.op), "dpr-id",""). /* код смены ВОК */

      mDocument-id = GetXAttrValueEx("op",STRING(Op.op),"document-id","").
      mDocNum      = GetXAttrValueEx("op",STRING(op.op), "Докум", "").
      RUN ParseDocum (INPUT-OUTPUT mDocument-id, INPUT-OUTPUT mDocNum, OUTPUT mDocCodName, OUTPUT mCustDocWho[1], OUTPUT mDocDate, OUTPUT mPassKP).
      mCustDocWho[1] = GetXAttrValueEx("op",STRING(Op.op),"cust-doc-who",mCustDocWho[1]).
      mDocDate       = GetXAttrValueEx("op",STRING(Op.op),"Document4Date_vid",mDocDate).
      IF mVOKDprID <> "" THEN /* документ не созданный ВОК */
      /* документ, созданный в ВОК */
      DO:
         mPassKP = GetXAttrValueEx("op",STRING(Op.op),"Подразд",mPassKP).
         IF NUM-ENTRIES(mCustDocWho[1]) > 1 THEN
         DO:
            IF mPassKP = "" THEN
               mPassKP = ENTRY(2,mCustDocWho[1]).
            ENTRY(2,mCustDocWho[1]) = "".
         END.   
            
         mchIdentCard = (IF mDocCodName <> ? 
                            THEN mDocCodName 
                            ELSE mDocument-id) +
                        (IF mDocNum <> ""
                            THEN " № " + mDocNum
                            ELSE "") +
                        (IF mCustDocWho[1] <> ""
                            THEN "~nВыдан " + mCustDocWho[1]
                            ELSE "") +
                        (IF mPassKP <> ""
                            THEN "к/п " + mPassKP
                            ELSE "") +                            
                        IF mDocDate <> "" THEN (", дата выдачи:" + mDocDate) ELSE "".
      END. /* документ, созданный в ВОК */
      ELSE
      DO:
      /* пасп данные получателя */
      /* идент.типа документа */
         /* тип документа из классиф. КодДокум */
         /* сливаем тип документа со значением ДР Докум (там обычно номер и кем выдан) */
         mchIdentCard = TRIM(mDocCodName + " " + GetXAttrValueEx("op", STRING(op.op), "Докум", "")).
      END.
      IF NOT {assigned mchIdentCard} THEN DO:
         /* доп.рек. Докум не указан */
         mchIdentCard = GetXAttrValueEx("op", STRING(op.op), "Passport", "").
         IF {assigned mchIdentCard} THEN DO:
            /* доп.рек. Passport указан */
            ASSIGN
               mDocument-id = ""
               mDocNum      = mchIdentCard
            .
            RUN ParseDocum (INPUT-OUTPUT mDocument-id, INPUT-OUTPUT mDocNum, OUTPUT mDocCodName, OUTPUT mCustDocWho[1], OUTPUT mDocDate, OUTPUT mPassKP).
         END.
         ELSE DO:
            FIND FIRST acct WHERE acct.acct = AcctDb NO-LOCK NO-ERROR.
            IF AVAILABLE acct THEN DO:
               mPersonId = IF acct.cust-cat = "Ч" THEN /* клиента будем искать по cust-id счета */
                              acct.cust-id
                           ELSE IF acct.cust-cat = "В" AND
                                   GetXattrValueEx("acct", AcctDb + "," + AcctDbCur, "ТипКл", "") =  "Ч"
                                THEN /* клиента будем искать по д.р. IDCust счета */
                                   INT64(GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "IDCust", ?))
                           ELSE
                              ?
               .
               IF mPersonId <> ? THEN DO:
                  FIND FIRST person WHERE person.person-id = mPersonId NO-LOCK NO-ERROR.
                  IF AVAILABLE person THEN DO:
                     mDocCodName    = GetCodeName("КодДокум", person.document-id).
                     mDocNum        = person.document.
                     FIND FIRST xcust-ident WHERE xcust-ident.cust-cat       = "Ч"
                                              AND xcust-ident.cust-id        = person.person-id
                                              AND xcust-ident.cust-code-type = person.document-id
                                              AND xcust-ident.cust-code      = person.document
                     NO-LOCK NO-ERROR.
                     IF AVAIL xcust-ident THEN DO:
                        IF {assigned xcust-ident.issue}
                        THEN mCustDocWho[1] = xcust-ident.issue.
                        ELSE mCustDocWho[1] = person.issue.
                        IF xcust-ident.open-date <> ?
                        THEN mDocDate = _date_2_Str(xcust-ident.open-date).
                        ELSE mDocDate = GetXattrValueEx("person", STRING(person.person-id), "Document4Date_Vid", "").
                        mPassKP = GetXattrValueEx("cust-ident",
                                                  GetSurrogateBuffer("cust-ident",(BUFFER xcust-ident:HANDLE)),
                                                  "Подразд",
                                                  "").                          
                     END.
                     ELSE DO:
                        mCustDocWho[1] = person.issue.
                        mDocDate       = GetXattrValueEx("person", STRING(person.person-id), "Document4Date_Vid", "").
                     END.
                     mchIdentCard = (IF mDocCodName <> ? THEN mDocCodName ELSE person.document-id)
                                  + " N " + mDocNum + ", выдан "
                                  + TRIM(person.issue + " " + mDocDate).
                     IF  {assigned mCustDocWho[1]}
                     AND {assigned mPassKP} THEN DO:
                        IF  NUM-ENTRIES(mCustDocWho[1]) >= 2
                        AND TRIM(ENTRY(NUM-ENTRIES(mCustDocWho[1]),mCustDocWho[1])) = TRIM(mPassKP)
                        THEN ASSIGN
                           ENTRY(NUM-ENTRIES(mCustDocWho[1]),mCustDocWho[1]) = ""
                           mCustDocWho[1] = TRIM(mCustDocWho[1],",")
                        .
                     END.
                  END.
               END.
            END.
         END.
      END.

      IF  {assigned mPassKP}
      AND INDEX(mCustDocWho[1]," к/п ") = 0
      THEN ASSIGN mCustDocWho[1] = mCustDocWho[1] + " к/п " + mPassKP.

      mchIdentCard = TRIM(mDocCodName + " N " + mDocNum + ", выдан " + TRIM(mCustDocWho[1] + " " + mDocDate)).

      DocCur = AcctCrCur.
      DO mInCount = 1 TO EXTENT(mdeSymSumIn):
         /* чистим массивы после печати прих. ордера */
         ASSIGN
            mdeSymSumIn[mInCount]  = 0
            mchSymCodIn[mInCount]  = ""
         .
      END.
      DO mInCount = 1 TO EXTENT(mdeSymSumOut):
         /* чистим массивы после печати расх. ордера */
         ASSIGN
            mdeSymSumOut[mInCount] = 0
            mchSymCodOut[mInCount] = ""
         .
      END.
      minCount = 0.
_symbin1:
      FOR EACH op-entry OF op WHERE op-entry.acct-db = AcctDb
      &IF DEFINED(valcashord) <> 0 &THEN
         AND NOT CAN-DO(NalRKROtr,(IF op-entry.acct-cr =  ? THEN "*" ELSE op-entry.acct-cr))
         AND NOT CAN-DO(NalRKRPol,(IF op-entry.acct-cr =  ? THEN "*" ELSE op-entry.acct-cr))
      &ENDIF
      NO-LOCK:

          TmpSymbol = op-entry.symbol.


          IF TmpSymbol <> "" THEN DO:
              IF NOT GetTCodeFld("val",
                                 "КасСимволы",
                                 TmpSymbol,
                                 op-entry.op-date) BEGINS "прих"
              THEN
                  TmpSymbol = FGetSetting("КасСимволы", "расх-" + op-entry.symbol ,"").

              IF TmpSymbol =  "" THEN
                 NEXT _symbin1.

              IF minCount = EXTENT(mdeSymSumIn) THEN DO:
                  MESSAGE
                      "Не могу обработать более"
                      STRING(EXTENT(mdeSymSumIn))
                      "символов"
                  VIEW-AS ALERT-BOX.
                  LEAVE.
              END.

              mNumArray = NumArraySymCod(mchSymCodIn,TmpSymbol).
              IF mNumArray <> 0 THEN
                  mdeSymSumIn[mNumArray] = mdeSymSumIn[mNumArray] +
                                                IF DocCur = "" THEN op-entry.amt-rub
                                                               ELSE op-entry.amt-cur.
              ELSE
                ASSIGN
                  minCount              = minCount + 1
                  mchSymCodIn[minCount] = TmpSymbol
                  mdeSymSumIn[minCount] = IF DocCur = "" THEN op-entry.amt-rub
                                                         ELSE op-entry.amt-cur
              .
          END.
      END.
      ASSIGN
         minCount   = 0
         mdeDocSum  = 0
         mdeNatSum  = 0
         mdeDragSum = 0
         mDrag      = GetXAttrValueEx("currency",DocCur,"Драг","")
      .
_symbout1:
      FOR EACH op-entry OF op WHERE op-entry.acct-cr = AcctCr
      &IF DEFINED(valcashord) <> 0 &THEN
         AND NOT CAN-DO(NalRKROtr,(IF op-entry.acct-db =  ? THEN "*" ELSE op-entry.acct-db))
         AND NOT CAN-DO(NalRKRPol,(IF op-entry.acct-db =  ? THEN "*" ELSE op-entry.acct-db))
      &ENDIF
      NO-LOCK:
         IF DocCur = "" THEN
            mdeDocSum = mdeDocSum + op-entry.amt-rub.
         ELSE IF (op.op-date  <> ? AND
                  op.op-date  <  DATE("01/11/2014")) OR
                  op.doc-date <  DATE("01/11/2014")
         THEN
            ASSIGN
               mdeNatSum = mdeNatSum + op-entry.amt-rub
               mdeDocSum = mdeDocSum + op-entry.amt-cur
            .
         ELSE DO:
            ASSIGN
               mdeNatSum  = mdeNatSum + op-entry.amt-rub
               &IF DEFINED(valcashord) =  0 &THEN
                  mdeDocSum  = IF mDrag =  "Да" THEN mdeDocSum  + op-entry.amt-rub 
                                                ELSE mdeDocSum  + op-entry.amt-cur
               &ELSE
                  mdeDocSum = mdeDocSum + op-entry.amt-cur
               &ENDIF
               mdeDragSum = IF mDrag =  "Да" THEN mdeDragSum + op-entry.amt-cur
                                             ELSE 0
            .
         END.

         TmpSymbol = op-entry.symbol.
         IF TmpSymbol <> "" THEN DO:
            IF NOT GetTCodeFld("val",
                               "КасСимволы",
                               TmpSymbol,
                               op-entry.op-date) BEGINS "расх"
            THEN
               TmpSymbol = FGetSetting("КасСимволы", "прих-" + op-entry.symbol,"").

            IF TmpSymbol =  "" THEN
               NEXT _symbout1.

            IF minCount = EXTENT(mdeSymSumOut) THEN DO:
               MESSAGE
                  "Не могу обработать более"
                  STRING(EXTENT(mdeSymSumOut))
                  "символов"
               VIEW-AS ALERT-BOX.
               LEAVE.
            END.
            mNumArray = NumArraySymCod(mchSymCodOut,TmpSymbol).
            IF mNumArray <> 0 THEN
                mdeSymSumOut[mNumArray] = mdeSymSumOut[mNumArray] +
                                              IF DocCur = "" THEN op-entry.amt-rub
                                                             ELSE op-entry.amt-cur.
            ELSE
              ASSIGN
               minCount               = minCount + 1
               mchSymCodOut[minCount] = TmpSymbol
               mdeSymSumOut[minCount] = IF DocCur = "" THEN op-entry.amt-rub
                                                       ELSE op-entry.amt-cur
            .
         END.
      END.

      FIND FIRST op-entry OF op WHERE op-entry.acct-db =  AcctDb NO-LOCK NO-ERROR.
      CREATE tt-op-entry.
      BUFFER-COPY op-entry TO tt-op-entry NO-ERROR.
      ASSIGN
         tt-op-entry.op-entry-half-db = op-entry.op-entry
      .
      RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
      &IF DEFINED(valcashord) &THEN
         mEmptyField = mVarVal[22].
      &ELSE
         mEmptyField = mVarVal[21].
      &ENDIF

      FIND FIRST op-entry OF op WHERE op-entry.acct-cr =  AcctCr NO-LOCK NO-ERROR.
      CREATE tt-op-entry.
      BUFFER-COPY op-entry TO tt-op-entry NO-ERROR.
      ASSIGN
         tt-op-entry.op-entry-half-db = op-entry.op-entry
      .
      RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
      &IF DEFINED(valcashord) &THEN
         mEmptyField2 = mVarVal[22].
      &ELSE
         mEmptyField2 = mVarVal[21].
      &ENDIF

      mdeCrMainSum = mdeDocSum - mdeCrCommSum.
      {docform_new.i
         &cashord      = ""приходно─расходный""
         &nodef        = yes
         &docnum       = op.doc-num
         &codedoc      = mCodeDoc
         &docdate      = mdtDateDoc
         &payer        = mchPayer
         &receiver     = mchReceiver
         &recbank      = mchRecBank
         &dbacct       = AcctDb
         &cracct       = AcctCr
         &docsum       = mdeDocSum
         &dragsum      = mdeDragSum
         &doccur       = DocCur
         &symsumin     = mdeSymSumIn
         &symsumout    = mdeSymSumOut
         &natsum       = mdeNatSum
         &symcodin     = mchSymCodIn
         &symcodout    = mchSymCodOut
         &details      = op.details
         &identcard    = mchIdentCard
         &documentid   = mDocCodName
         &documentnum  = mDocNum
         &documentwho  = mCustDocWho
         &documentdate = _str_2_DATE(mDocDate)
         &recinn       = mRecInn
         &recbankbik   = mchBankBIK
         &reckpp       = mRecKPP
         &recokato     = mRecOKATO
         &recacct      = mRecAcct
         &paybank      = mPayBank
         &paybankbik   = mPayBankBik
         &acctkomis    = AcctKomis
         &inc_part_fio = mchFIO
         &emptyfield   = mEmptyField
         &emptyfield2  = mEmptyField2
         &worker_buh_post  = mchWorkerBuhP
         &worker_buh_fio   = mchWorkerBuh
         &worker_kont_post = mchWorkerKontP
         &worker_kont_fio  = mchWorkerKont
         &worker_kas_post  = mchWorkerKasP
         &worker_kas_fio   = mchWorkerKas
         &wordwrap = YES
      }
   END.
&ENDIF

{docform_i_new.i
   &dublicate = YES
}

&IF DEFINED(f2in1) <> 0 AND DEFINED(valcashord) =  0 &THEN
   {endout3.i &nofooter=yes &NoPreview=yes &NoDefs=/*}
   IF NoPreview THEN
      OUTPUT CLOSE.
&ENDIF
   
&IF DEFINED(valcashord) <> 0 &THEN
   IF FGetSetting("КасВОПечРазб","","НЕТ") EQ "ДА" THEN
      RUN Insert_TTName("SUBPAGE", "<!PAGE!>").
   ELSE 
      RUN Insert_TTName("SUBPAGE", "").
   RUN printvd.p ("valcashord", INPUT TABLE ttNames).
&ELSE
   IF mFlagOSP AND NOT mFlagPackPrn AND NOT {assigned mDocTempl} THEN DO:
      {setdest.i &append=" APPEND "} 
      {preview.i} 
   END.
&ENDIF
   
PackagePrint = mFlagPackPrn.
IF NOT PackagePrint THEN DO:
   FirstPrint = yes.
   RUN SetSysConf IN h_base("DocCount","").
   RUN SetSysConf IN h_base("printvd_file","").
   RUN SetSysConf IN h_base("printvd_options","").
END.

PROCEDURE ParseDocum:
   DEFINE INPUT-OUTPUT PARAMETER ioDocument-id AS CHAR NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioDocNum      AS CHAR NO-UNDO.
   DEFINE       OUTPUT PARAMETER oDocCodName   AS CHAR NO-UNDO.
   DEFINE       OUTPUT PARAMETER oCustDocWho   AS CHAR NO-UNDO.
   DEFINE       OUTPUT PARAMETER oDocDate      AS CHAR NO-UNDO.
   DEFINE       OUTPUT PARAMETER oPaspKP       AS CHAR NO-UNDO.
   DEFINE BUFFER xxcode FOR code.
   
   DEFINE VARIABLE vDateTMP AS DATE NO-UNDO.

   ioDocNum = TRIM(ioDocNum).
   IF  NOT {assigned ioDocument-id}
   AND     {assigned ioDocNum} THEN DO:
      IF ioDocNum BEGINS "Паспорт серии" THEN _dr_passport: DO:
         ioDocNum = TRIM(SUBSTR(ioDocNum,14)).
         /* Возможно Паспорт гражданина РФ */
         INT64(ENTRY(1,ioDocNum," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN DO:
            FOR EACH xxcode WHERE xxcode.class = "КодДокум"
                              AND xxcode.name  MATCHES "*паспорт* РФ*"
            NO-LOCK BY (xxcode.code = "Паспорт") DESCENDING BY xxcode.name MATCHES "Паспорт*гражданина*" DESCENDING:
               ioDocument-id = xxcode.code.
               LEAVE _dr_passport.
            END.
         END.
         /* Возможно Паспорт гражданина СССР */
         IF ioDocNum BEGINS "I"
         OR ioDocNum BEGINS "V"
         OR ioDocNum BEGINS "X"
         OR ioDocNum BEGINS "L"
         OR ioDocNum BEGINS "C"
         OR ioDocNum BEGINS "D"
         OR ioDocNum BEGINS "M" THEN DO:
            FOR EACH xxcode WHERE xxcode.class = "КодДокум"
                              AND xxcode.name  MATCHES "*паспорт*СССР*"
            NO-LOCK BY (xxcode.code = "Документ") DESCENDING BY xxcode.name MATCHES "Паспорт*гражданина*" DESCENDING:
               ioDocument-id = xxcode.code.
               LEAVE _dr_passport.
            END.
         END.
         /* Значит какой-то любой паспорт */
         FOR EACH xxcode WHERE xxcode.class = "КодДокум"
                           AND xxcode.name  MATCHES "*паспорт*"
         NO-LOCK BY (xxcode.code = "Паспорт") DESCENDING BY (xxcode.code = "Документ") DESCENDING BY xxcode.name MATCHES "Паспорт*гражданина*" DESCENDING:
            ioDocument-id = xxcode.code.
            LEAVE _dr_passport.
         END.
      END.
      ELSE DO:
         _try_assign:
         FOR EACH xxcode WHERE xxcode.class = "КодДокум"
                           AND ioDocNum BEGINS xxcode.name
         NO-LOCK BY LENGTH(xxcode.name) DESCENDING:
            ASSIGN
               ioDocument-id = xxcode.code
               ioDocNum      = TRIM(SUBSTR(ioDocNum,LENGTH(xxcode.name) + 1))
            .
            LEAVE _try_assign.
         END.
      END.
   END.
   oDocCodName = IF   {assigned ioDocument-id}
                 THEN GetCodeName("КодДокум", ioDocument-id)
                 ELSE ""
   .
   ASSIGN
      oDocDate    = ""
      oCustDocWho = ""
   .
   IF INDEX(ioDocNum," выдан ") > 0 THEN DO:
      ASSIGN
         oCustDocWho = TRIM(SUBSTR(ioDocNum,INDEX(ioDocNum," выдан ") + 7))
         ioDocNum    = TRIM(SUBSTR(ioDocNum,1,INDEX(ioDocNum," выдан ") - 1))
      .
      /* "... выдан ОВД Городское 01.01.2001 к/п 001-002 */
      /* "... выдан 01.01.2001 ОВД Городское к/п 001-002 */
      /* "... выдан ОВД Городское к/п 001-002 01.01.2001 */
      IF INDEX(oCustDocWho," к/п ") > 0 THEN ASSIGN
         oPaspKP     = TRIM(SUBSTR(oCustDocWho,INDEX(oCustDocWho," к/п ") + 5))
         oCustDocWho = TRIM(SUBSTR(oCustDocWho,1,INDEX(oCustDocWho," к/п ") - 1))
      .
      /* "... выдан ОВД Городское 01.01.2001 */
      oDocDate = STRING(DATE(ENTRY(NUM-ENTRIES(oCustDocWho," "),oCustDocWho," "))) NO-ERROR.
      IF  oDocDate <> ?
      AND NOT ERROR-STATUS:ERROR THEN ASSIGN
         oDocDate = ENTRY(NUM-ENTRIES(oCustDocWho," "),oCustDocWho," ")
         ENTRY(NUM-ENTRIES(oCustDocWho," "),oCustDocWho," ") = ""
         oCustDocWho = TRIM(oCustDocWho)
      .
      /* "... выдан 01.01.2001 ОВД Городское */
      ELSE DO:
         IF NUM-ENTRIES(ENTRY(1,oCustDocWho," "),"/") EQ 2 OR
            NUM-ENTRIES(ENTRY(1,oCustDocWho," "),".") EQ 2 THEN 
            oDocDate = STRING(DATE(ENTRY(1,oCustDocWho," "))) NO-ERROR.
         IF  oDocDate <> ?
         AND NOT ERROR-STATUS:ERROR THEN ASSIGN
            oDocDate = ENTRY(1,oCustDocWho," ")
            ENTRY(1,oCustDocWho," ") = ""
            oCustDocWho = TRIM(oCustDocWho)
         .
         /* "... 001-002 01.01.2001 */
         ELSE IF  {assigned oPaspKP}
              AND NUM-ENTRIES(oPaspKP," ") >= 2 THEN DO:
            oDocDate = STRING(DATE(ENTRY(NUM-ENTRIES(oPaspKP," "),oPaspKP," "))) NO-ERROR.
            IF  oDocDate <> ?
            AND NOT ERROR-STATUS:ERROR THEN ASSIGN
               oDocDate = ENTRY(NUM-ENTRIES(oPaspKP," "),oPaspKP," ")
               ENTRY(NUM-ENTRIES(oPaspKP," "),oPaspKP," ") = ""
               oPaspKP = TRIM(oPaspKP)
            .
         END.
      END.
   END.

   vDateTMP = DATE(oDocDate) NO-ERROR.

   IF NOT ERROR-STATUS:ERROR AND oDocDate NE "" THEN
      oDocDate = _date_2_Str(vDateTMP).
   RETURN.
END PROCEDURE.

{intrface.del}
/* $LINTFILE='cashord_new_main.i' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='16/10/2017 12:29:52.505+03:00' */
/*prosignwIeOaIV3vTiUL+11rCodrA*/