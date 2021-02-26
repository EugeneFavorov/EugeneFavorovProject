/*  Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-xfts.p
      Comment: Библиотека импорта экспорта из ФТС
   Parameters: нет
         Uses:
      Used by:
      Created: 06/08/2014 ahra
     Modified: 
*/

{pfuncdef
 &DefLib="XFTS" 
 &Description="Библиотека импорта экспорта из ФТС"}

&GLOBAL-DEFINE NO-BASE-PROC YES

{globals.i}             /* Глобальные переменные сессии. */

{form.def}
{g-trans.equ}
{exchange.equ}
{365p.def}

{intrface.get tmess} 
{intrface.get strng}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass} 
{intrface.get pack} 
{intrface.get filex}
{intrface.get cust}
{intrface.get instrum}

{intrface.get pack}
{intrface.get rfrnc}

{intrface.get data}
{intrface.get pbase}
{intrface.get db2l}
{intrface.get acct}

{debug.equ}

DEFINE STREAM   sImport.
DEFINE STREAM   sReq.
DEFINE STREAM   sReport.

DEFINE VARIABLE mAcctCount       AS INT64       NO-UNDO.
DEFINE VARIABLE mAcctDone        AS INT64       NO-UNDO.

DEFINE VARIABLE oOp              AS INT64       NO-UNDO.

DEFINE VARIABLE mCustCat         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCustId          AS INT64       NO-UNDO.

DEFINE VARIABLE mSuperPacketID   AS INT64       NO-UNDO.
DEFINE VARIABLE mNewPacketID     AS INT64       NO-UNDO.

DEFINE VARIABLE m365_Join        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m365_BalAcct     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m365_Contract    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mSummVZ          AS DECIMAL     NO-UNDO.

DEFINE VARIABLE vMode               AS CHARACTER   NO-UNDO.

DEFINE VARIABLE vResult             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mMess               AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mDSD_365       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVBO_365       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrArch_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mArchSr_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMnozhC_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIgnorKPP_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAllFil_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrDP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrOP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKolDoc_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTipeOst_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProvOvrd_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mZamRVSO_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mShift_365     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mINN           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFO           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBank          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNCCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchKas    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchMBR    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate_NR       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorrACCT      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcNSpis       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParSchStN     AS CHARACTER NO-UNDO.

ASSIGN
   m365_Join     = FGetSetting("Настройка_365П", "Объединять", "И")
   m365_BalAcct  = FGetSetting("Настройка_365П", "ИсклБС",     "")
   m365_Contract = FGetSetting("Настройка_365П", "ИсклНазн",   "")
   mINN          = FGetSetting("ИНН", "", "")
   mKPP          = FGetSetting("БанкКПП", "", "")
   mMFO          = FGetSetting("БанкМФО", "", "")
   mBank         = FGetSetting("Банк", "", "") 
   mNCCode       = FGetSetting("КодНацВал0406007", "", "")
   mDSD_365      = FGetSetting("Настройка_365П","ДопСтатусыДок","")
   mVBO_365      = FGetSetting("Настройка_365П","ВычБлокОст","Нет")
   mPrArch_365   = FGetSetting("Настройка_365П", "ПровАрхив","")
   mArchSr_365   = FGetSetting("Настройка_365П", "АрхивСрок","0")
   mNaznSchKas   = FGetSetting("НазнСчКас", "", "Касса") 
   mNaznSchMBR   = FGetSetting("НазнСчМБР", "", "")   
   mMnozhC_365   = FGetSetting("Настройка_365П", "МножКл", "") 
   mIgnorKPP_365 = FGetSetting("Настройка_365П", "ИгнорКППНП", "Нет") 
   mAllFil_365   = FGetSetting("Настройка_365П", "ВсеФилиалы", "Нет") 
   mDate_NR      = FGetSetting("Дата_НР", "", "") 
   mKontrDP_365  = FGetSetting("Настройка_365П", "КонтрДатыПоруч", "")
   mKontrOP_365  = FGetSetting("Настройка_365П", "КонтрОчерПл", "*")   
   mKorrACCT     = FGetSetting("КорСч", "", "") 
   mKolDoc_365   = FGetSetting("Настройка_365П", "КолДок", "0")
   mTipeOst_365  = FGetSetting("Настройка_365П", "ТипОстатка", "")   
   mProvOvrd_365 = FGetSetting("Настройка_365П","ПровОврд","Нет")
   mZamRVSO_365  = FGetSetting("Настройка_365П","ЗамРаздВСодОпер",{&SEPARATOR-365P})       
   mParSchStN    = FGetSetting("Настройка_365П","ПарамСчСтНом","Счет_СтарНом")
   mShift_365    = FGetSetting("Настройка_365П","Сдвиг","0")
   mAcNSpis      = FGetSetting("СчНСпис", "", "")             
NO-ERROR.

DEFINE TEMP-TABLE ttBlockAcct
   FIELD packetid      AS INT64
   FIELD num           AS INT64
   FIELD acct          AS CHARACTER
   FIELD currency      AS CHARACTER
   FIELD cont-name     AS CHARACTER
   FIELD acct-status   AS CHARACTER
   FIELD msg           AS CHARACTER
   FIELD error-code    AS CHARACTER
   FIELD type          AS CHARACTER
   FIELD open-date     AS CHARACTER
   FIELD close-date    AS CHARACTER
.

DEFINE TEMP-TABLE ttLevel
   FIELD Fmt   AS CHARACTER
   FIELD Level AS INT64
   FIELD Point AS INT64
   FIELD Kind  AS CHARACTER.

DEFINE TEMP-TABLE ttKvt  NO-UNDO
   FIELD AttrCode   AS CHARACTER
   FIELD AttrValue  AS CHARACTER.

{365p.def}
{core365p.pro}
{spec365p.pro}
{pno365p2.pro}
{zno365p2.pro}
{log365p.def}

/*----------------------------------------------------------------------------*/
/* Метод IMPORT для класса XMLFTSCancel - РЕШЕНИЕ ОБ ОТМЕНЕ ПРИОСТАНОВЛЕНИЯ  */
/*----------------------------------------------------------------------------*/
PROCEDURE IXMLFTSCancel:
   DEFINE INPUT PARAMETER iClass       AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iInstance    AS HANDLE      NO-UNDO.
   
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:

   vMode = "Cancel".
   RUN IXMLFTSStay(iClass,iInstance).

END.  /* MAIN */
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Метод IMPORT для класса XMLFTSStay - РЕШЕНИЕ О ПРИОСТАНОВЛЕНИИ ОПЕРАЦИЙ    */
/*----------------------------------------------------------------------------*/
PROCEDURE IXMLFTSStay:
   
DEFINE INPUT PARAMETER iClass       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER iInstance    AS HANDLE      NO-UNDO.

DEFINE VARIABLE vhExch              AS HANDLE      NO-UNDO.
DEFINE VARIABLE vHCust              AS HANDLE      NO-UNDO.
DEFINE VARIABLE vHBuffer            AS HANDLE      NO-UNDO.
DEFINE VARIABLE vHTable             AS HANDLE      NO-UNDO.

DEFINE VARIABLE vPacketID           AS INT64       NO-UNDO.
DEFINE VARIABLE vSeanceID           AS INT64       NO-UNDO.
DEFINE VARIABLE vCreateConf         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vOK                 AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vBIK                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vNomF               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vBankMFO            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vBranch             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFilial             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAcctNotFound       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAcctExcluded       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAcctNotClient      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCheck33            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCheckCorpName      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vPlatUL             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vINNUL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vNaimUL             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vKPPUL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vPlatFL             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vINNFL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFamFL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vNamFL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vOtcFL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFIOFL              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCustID             AS INT64       NO-UNDO.
DEFINE VARIABLE vCustCat            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCustName           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCustName2          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vINN                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vCorpNameCorrect    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vI                  AS INT64       NO-UNDO.
DEFINE VARIABLE vTmpRez             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vTmpTxt             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vTmpStr             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vS                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vConUpr             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vInfoBankrupt       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vBlkType            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vUserType           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vUserSel            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vClassCode          AS CHARACTER   NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:        

   ASSIGN
      vhExch      = iInstance:DEFAULT-BUFFER-HANDLE
      vPacketID   = vhExch::PacketID
      vSeanceID   = INT64(vhExch::SeanceID)
      vBIK        = vhExch:BUFFER-FIELD(GetMangledName("БИК")):BUFFER-VALUE
      vNomF       = vhExch:BUFFER-FIELD(GetMangledName("НомФ")):BUFFER-VALUE
      vINNFL      = vhExch:BUFFER-FIELD(GetMangledName("ИННФЛ")):BUFFER-VALUE
      vFamFL      = vhExch:BUFFER-FIELD(GetMangledName("ФамФЛ")):BUFFER-VALUE
      vNamFL      = vhExch:BUFFER-FIELD(GetMangledName("ИмяФЛ")):BUFFER-VALUE
      vOtcFL      = vhExch:BUFFER-FIELD(GetMangledName("ОтчФЛ")):BUFFER-VALUE
      vINNUL      = vhExch:BUFFER-FIELD(GetMangledName("ИННЮЛ")):BUFFER-VALUE
      vNaimUL     = vhExch:BUFFER-FIELD(GetMangledName("НаимЮЛ")):BUFFER-VALUE
      vKPPUL      = vhExch:BUFFER-FIELD(GetMangledName("КППЮЛ")):BUFFER-VALUE
      vCreateConf = IF FGetSetting("Настройка_390П","ПодтвТО1","ОТКЛ") EQ "" 
                    THEN "ОТКЛ" 
                    ELSE FGetSetting("Настройка_390П","ПодтвТО1","ОТКЛ") 
      vCheck33    = TRNSettingValue("ExchSET-BNK", "Check33", "Наим")
      mAcctCount  = 0
      vOK         = YES
      vI          = 0
      vMode       = IF iClass EQ "XMLFTSStay" THEN "Stay" ELSE vMode
      .

   RUN SetMFMode365p IN THIS-PROCEDURE (vHExch).
   
   IF    {assigned vFamFL} 
      OR {assigned vNamFL} 
      OR {assigned vOtcFL} THEN
      vFIOFL = vFamFL + "," + vNamFL + "," + vOtcFL.
   ELSE 
      vFIOFL = "".
   
   mCustCat = IF vFIOFL NE "" THEN  "Ч" ELSE "Ю". 

   vCheckCorpName = (vCheck33 <> "ИНН").
   
   /*1.Подтверждение об успешном приеме*/
   IF vCreateConf EQ "ДА" THEN
      RUN FTSConfirmCreate(vPacketID,"1","01","").
      
   /*2.Проверка реквизитов банка*/
   vBankMFO  = FGetSetting("БанкМФО","","").
   vBranch   = FGetSetting("КодФил","","").
   
   vFilial = GetXattrValueEx("branch",vBranch,"REGN","").
   IF INDEX(vFilial,"/") GT 0 THEN 
      vFilial = ENTRY(2,vFilial,"/").
   ELSE
      vFilial = "0".
   IF    vBIK  NE vBankMFO 
      OR vNomF NE vFilial THEN
   DO:
      ASSIGN
         vTmpRez = IF vTmpRez EQ "" THEN "21" ELSE vTmpRez + CHR(1) + "21"
         vTmpStr = "Электронный документ ошибочно направлен таможенным органом не в тот банк."
         vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
         vOK     = NO.
      LEAVE MAIN.
   END.
   /*3. Проверка счетов */
   FOR EACH ttBlockAcct NO-LOCK:
      IF ttBlockAcct.error-code EQ "22" THEN
      DO:
         vAcctNotFound = IF vAcctNotFound EQ "" 
                         THEN ttBlockAcct.acct 
                         ELSE vAcctNotFound + "," + ttBlockAcct.acct.
         vOK = NO.
      END.   
      IF ttBlockAcct.error-code EQ "31" 
         AND vMode NE "Cancel" THEN
      DO:
         vAcctExcluded = IF vAcctExcluded EQ "" 
                   THEN DelFilFromAcct(ttBlockAcct.acct) 
                   ELSE vAcctExcluded + "," + DelFilFromAcct(ttBlockAcct.acct).
         vOK = NO.
      END.
   END.
   /*3.Подтверждение для ненайденных счетов*/
   IF vAcctNotFound NE "" THEN
      ASSIGN
         vTmpRez = IF vTmpRez EQ "" THEN "22" ELSE vTmpRez + CHR(1) + "22"
         vTmpStr = vAcctNotFound
         vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
         vOK     = NO.
   /*3.Подтверждение для исключенных счетов*/      
   IF vAcctExcluded NE "" 
      AND vMode NE "Cancel" THEN
      ASSIGN
         vTmpRez = IF vTmpRez EQ "" THEN "31" ELSE vTmpRez + CHR(1) + "31"
         vTmpStr = vAcctExcluded
         vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
         vOK     = NO.
   /*4.Проверка клиента */
   vClassCode = "TAXTOOAC".
   RUN CreateTransportTable365p IN THIS-PROCEDURE 
      (vClassCode,OUTPUT vHTable) NO-ERROR.
   
   FOR EACH ttBlockAcct WHERE 
      NOT {assigned ttBlockAcct.error-code} 
      NO-LOCK:
   
      vHTable:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().

      {365p.set &obj = "vHTable:DEFAULT-BUFFER-HANDLE"
                &fld = "НомСч"
                &val = "ttBlockAcct.acct"}
   END.
   
   RUN GetCust390p(vhExch,
                   vHTable,
                   vCheckCorpName,
                   OUTPUT vHCust,
                   OUTPUT vI) NO-ERROR.
   
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("IXMLFTSStay",SUBSTITUTE("~n
         vI = &1~n
         vCheckCorpName = &2~n
         ERROR-STATUS:ERROR = &2~n
         VALID-HANDLE(vHCust) = &3~n",
         STRING(vCheckCorpName),
         STRING(ERROR-STATUS:ERROR),
         STRING(VALID-HANDLE(vHCust)))).
   &ENDIF                   
   
   IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(vHCust) THEN
   DO:
      IF NOT vHCust:HAS-RECORDS 
         OR vI GT 1 THEN 
      DO:
         RUN DeleteObject365p(vHCust).
         ASSIGN
            vTmpRez = IF vTmpRez EQ "" THEN "24" ELSE vTmpRez + CHR(1) + "24"
            vTmpStr = SUBSTITUTE("Не найден клиент &1 &2 &3.",
                      (IF mCustCat EQ "Ю" THEN TRIM(vNaimUL) ELSE TRIM(vFIOFL)),
                      (IF mCustCat EQ "Ю" THEN vINNUL ELSE vINNFL),
                      (IF mCustCat EQ "Ю" THEN vKPPUL ELSE ""))
            vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
            vOK     = NO.
         LEAVE MAIN.
      END.
      /**/
      IF vI EQ 1 THEN
      DO:
         RUN FindFirstCustRecord390p(vHCust,
            OUTPUT vHBuffer,
            OUTPUT vCustID,
            OUTPUT vCustCat) NO-ERROR.
   
         IF vCustCat EQ "Ю" THEN
            vConUpr = GetXAttrValueEx("cust-corp",STRING(vCustID),"КонУпр","").
         ELSE IF vCustCat EQ "Ч" THEN
            vConUpr = GetXAttrValueEx("person",STRING(vCustID),"КонУпр","").
         FIND FIRST code WHERE
                  code.class  EQ "НБКИ_ТипБанкрот"
            AND   code.name   EQ vConUpr
         NO-LOCK NO-ERROR.
         IF AVAIL(code)
            AND code.val EQ "1" THEN
         DO:
            FOR FIRST loan WHERE
                     loan.class-code   EQ "bankrupt"
               AND   loan.contract     EQ "bankrupt"
               AND   loan.cust-cat     EQ vCustCat
               AND   loan.cust-id      EQ vCustID
               AND   loan.filial-id    EQ shFilial NO-LOCK:
   
               vTmpStr = "По " + 
                         (IF vCustCat EQ "Ю" THEN TRIM(vNaimUL) ELSE TRIM(vFIOFL)) + 
                         " принято " + 
                         vConUpr +
                         " на основании решения № " + 
                         TRIM(loan.doc-num) + 
                         " от " +
                         STRING(loan.end-date,"99/99/9999") + " " + 
                         TRIM(loan.user-o[1]) + ".".
            END.
            ASSIGN
               vTmpRez = IF vTmpRez EQ "" THEN "31" ELSE vTmpRez + CHR(1) + "31"
               vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
               vOK     = NO.
            RUN DeleteObject365p(vHCust).
            LEAVE MAIN.         
         END.
      END.
      RUN DeleteObject365p(vHCust).
   END.
   /*4.Проверка привязки клиента к счету */
   vAcctNotClient = "".
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("IXMLFTSStay",SUBSTITUTE("~n
         vCustID = &1~n
         vCustCat = &2~n",
         STRING(vCustID),vCustCat)).
   &ENDIF
   FOR EACH ttBlockAcct WHERE ttBlockAcct.error-code EQ "" EXCLUSIVE-LOCK:
      
      {find-act.i &acct = ttBlockAcct.acct
                  &curr = ttBlockAcct.currency}
                  
      IF NOT  (
         AVAIL(acct) 
         AND   acct.cust-id   EQ vCustID 
         AND   acct.cust-cat  EQ vCustCat
               ) THEN
      DO:
         ASSIGN
            ttBlockAcct.msg = "Счет не соответствует клиенту" 
            ttBlockAcct.error-code = "23"
            vAcctNotClient = IF vAcctNotClient EQ "" 
                             THEN DelFilFromAcct(ttBlockAcct.acct) 
                             ELSE vAcctNotClient + "," + DelFilFromAcct(ttBlockAcct.acct)
            vOK = NO.
      END.  
   END.  /*FOR EACH ttBlockAcct*/
   /*5.Подтверждение для счетов, не соответствующих клиенту*/
   IF vAcctNotClient NE "" THEN
      ASSIGN
         vTmpRez = IF vTmpRez EQ "" THEN "23" ELSE vTmpRez + CHR(1) + "23"
         vTmpStr = "Наименование клиента " + 
                (IF vCustCat EQ "Ю" THEN TRIM(vNaimUL) ELSE TRIM(vFIOFL)) + 
                " не соответствует номеру счета клиента " + vAcctNotClient + 
                ", указанному в электронном документе."
         vTmpTxt = IF vTmpTxt EQ "" THEN vTmpStr ELSE vTmpTxt + CHR(1) + vTmpStr
         vOK     = NO.
   
   /*Блокировка*/
   IF vMode NE "Cancel" THEN
   DO:
      vBlkType  = TRNSettingValue("ExchSET-BNK","BlockType","БлокТамож").
      vUserType = TRNSettingValue("ExchSET-BNK","UserSelType","Да").
      vUserSel  = (vUserType EQ "ДА") OR (vUserType EQ "YES").
      FOR EACH ttBlockAcct WHERE ttBlockAcct.error-code EQ "" NO-LOCK:
         RUN FTSBlockAcct(vhExch,
                          BUFFER ttBlockAcct,
                          vBlkType,
                          vUserSel).
      END.
   END.
   /*Снятие блокировки*/
   IF vMode EQ "Cancel" THEN
   DO:
      FOR EACH ttBlockAcct WHERE ttBlockAcct.error-code EQ "" NO-LOCK:
         RUN FTSUnblockAcct(vhExch,
                            BUFFER ttBlockAcct).
      END.
   END.
         


END.  /* MAIN */

/*Отчет*/
RUN FTSReport (vhExch,
               BUFFER ttBlockAcct,
               vSeanceID,
               vPacketID).
                  
/*1.Подтверждение об успешном приеме в случае отсутствия ошибок*/
IF vCreateConf EQ "НЕТ"
   AND vOK     EQ YES THEN
   RUN FTSConfirmCreate(vPacketID,"1","01","").
      
IF vOK EQ NO THEN
   RUN FTSConfirmCreate(vPacketID,"2",vTmpRez,vTmpTxt).

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Обработка счетов                                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE IXMLFTSAcct:
   
DEFINE INPUT PARAMETER iClass    AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER iInstance AS HANDLE      NO-UNDO.   

DEFINE VARIABLE vhTAXTOOAC       AS HANDLE      NO-UNDO.
DEFINE VARIABLE vhExch           AS HANDLE      NO-UNDO.

DEFINE VARIABLE vExchMain        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vPacketID        AS INT64       NO-UNDO.
DEFINE VARIABLE vInfoType        AS CHARACTER   NO-UNDO.   
DEFINE VARIABLE vAcctNum         AS CHARACTER   NO-UNDO. 
DEFINE VARIABLE vAcctVid         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAcctNumFil      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m365_Join        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m365_BalAcct     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m365_Contract    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vMess            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vError           AS CHARACTER   NO-UNDO.

DEFINE BUFFER acct FOR acct .

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:

   /*3.Проверка счета*/
   ASSIGN
      vhTAXTOOAC    = ObjectValueHandle ("TAXTOOAC")
      vExchMain     = vhTAXTOOAC:BUFFER-FIELD("ExchMain"):BUFFER-VALUE     
      vAcctNum      = vhTAXTOOAC:BUFFER-FIELD(GetMangledName("НомСч")):BUFFER-VALUE
      vAcctVid      = vhTAXTOOAC:BUFFER-FIELD(GetMangledName("ВидСч")):BUFFER-VALUE
      m365_Join     = FGetSetting("Настройка_365П", "Объединять", "И")
      m365_BalAcct  = FGetSetting("Настройка_365П", "ИсклБС",     "")
      m365_Contract = FGetSetting("Настройка_365П", "ИсклНазн",   "")
      vhExch        = ObjectValueHandle (vExchMain) 
      vPacketID     = vhExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE.

   RUN FindAcct365p IN THIS-PROCEDURE (BUFFER acct, vAcctNum) NO-ERROR.

   ASSIGN
      mAcctCount = mAcctCount + 1
      vMess  = ""
      vError = "".
   IF AVAIL acct THEN
   DO:
      vInfoType = IF vMode NE "Cancel" THEN "РешПриост" ELSE "РешОтмена" .
      RUN PacketCreateLink IN h_pack (vPacketID,
                                              "acct",
                                              Acct.acct + "," + Acct.currency,
                                              vInfoType).   
   END.
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("IXMLFTSAcct",SUBSTITUTE("~n
         vAcctNum = &1~n
         m365_Join = &2~n
         m365_BalAcct = &3~n
         m365_Contract = &4~n
         avail(acct) = &5~n",
         STRING(vAcctNum),
         STRING(m365_Join),
         STRING(m365_BalAcct),
         STRING(m365_Contract),
         STRING(avail(acct)))).
   &ENDIF
   IF AVAIL acct 
      AND vMode NE "Cancel" THEN 
   DO:
      vAcctNumFil = AddFilToAcct(vAcctNum, shFilial).
      
      IF (m365_Join EQ "И" AND
                (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                 AND CAN-DO(m365_Contract, acct.contract)))
        OR (m365_Join EQ "ИЛИ" AND
                (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                  OR CAN-DO(m365_Contract, acct.contract)))
      THEN 
      ASSIGN
         vMess  = "Счет исключен"
         vError = "31".
   END.
   ELSE
      ASSIGN
         vMess  = "Счет не найден"
         vError = "22".
   IF AVAIL(acct) 
      AND acct.close-date NE ? THEN
   ASSIGN
      vMess  = "Счет закрыт"
      vError = "31".

   IF mNewPacketID NE vPacketID THEN {empty ttBlockAcct}
   
   CREATE ttBlockAcct.
   ASSIGN
      ttBlockAcct.packetid   = vPacketID 
      ttBlockAcct.num        = mAcctCount
      ttBlockAcct.acct       = IF AVAIL(acct) THEN acct.acct     ELSE vAcctNum
      ttBlockAcct.currency   = IF AVAIL(acct) THEN acct.currency ELSE ""
      ttBlockAcct.cont-name  = vAcctVid
      ttBlockAcct.msg        = vMess
      ttBlockAcct.error-code = vError
   NO-ERROR.
   VALIDATE ttBlockAcct.
   
   mNewPacketID = vPacketID.

END.  /* MAIN */
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Создание подтверждений                                                     */
/*----------------------------------------------------------------------------*/
PROCEDURE FTSConfirmCreate:
   DEFINE INPUT PARAMETER iPackID      AS INT64       NO-UNDO.
   DEFINE INPUT PARAMETER iPT          AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iKodRezProv  AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iExplanation AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE vFileName           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPT                 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDateTimeProv       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOpKind             AS CHARACTER   NO-UNDO.   
      
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   FOR FIRST Packet WHERE
             Packet.PacketId EQ iPackID EXCLUSIVE-LOCK,
      FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID  NO-LOCK:
      ASSIGN vFileName = ENTRY(1,FileExch.name,".") + ".xml"
             Packet.State = "ЗАГР"
      .
   END.
   
   ASSIGN
      vPT           = "PT" + iPT + "_"
      vDateTimeProv = STRING(YEAR(TODAY),"9999") + "-" +
                      STRING(MONTH(TODAY),"99")  + "-" +
                      STRING(DAY(TODAY),"99")    + " " + 
                      STRING(TIME,"hh:mm:ss").
      
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"PT",vPT).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"OrigFileName",vFileName).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"DateTimeProv",vDateTimeProv).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"KodRezProv",iKodRezProv).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"Explanation",iExplanation).
   IF AVAIL Packet THEN
      RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"PacketIdImp", STRING(Packet.PacketId)).
   vOpKind = "e-txrscnf".
   /* Транзакция экспорта подтверждения 
   TAXExpCnf процедура из pp-itax.p     */

   RUN RunTransaction  IN h_pbase (vOpKind) NO-ERROR.

END.  /* MAIN */
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*  Проверка клиента                                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetCust390p.
   DEFINE INPUT  PARAMETER iHReqInfo      AS HANDLE  NO-UNDO.
   DEFINE INPUT  PARAMETER iHReqAcct      AS HANDLE  NO-UNDO.
   DEFINE INPUT  PARAMETER iCheckCorpName AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oHCust365p     AS HANDLE  NO-UNDO.
   DEFINE OUTPUT PARAMETER oI             AS INT64   NO-UNDO.
   
   DEFINE BUFFER banks      FOR banks.
   DEFINE BUFFER cust-corp  FOR cust-corp.
   DEFINE BUFFER cust-ident FOR cust-ident.
   DEFINE BUFFER person     FOR person.

   DEFINE VARIABLE vCustINN      AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vCustKPP      AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vCustCName    AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vCustFam      AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vCustNam      AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vCustOtc      AS CHARACTER             NO-UNDO.
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
   RUN GetMFMode365p IN THIS-PROCEDURE (OUTPUT vAllFilials) NO-ERROR.

   IF mCustCat EQ "Ю" THEN
   DO:
      RUN GetCharAttrValue365p(iHReqInfo, "ИННЮЛ", OUTPUT vCustINN) NO-ERROR.
      RUN GetCharAttrValue365p(iHReqInfo, "КППЮЛ", OUTPUT vCustKPP) NO-ERROR.
      RUN GetCharAttrValue365p(iHReqInfo, "НаимЮЛ", OUTPUT vCustCName) NO-ERROR.
   END.
   ELSE IF mCustCat EQ "Ч" THEN
   DO:
      RUN GetCharAttrValue365p(iHReqInfo, "ИННФЛ",  OUTPUT vCustINN) NO-ERROR.
      RUN GetCharAttrValue365p(iHReqInfo, "ФамФЛ", OUTPUT vCustFam) NO-ERROR.
      RUN GetCharAttrValue365p(iHReqInfo, "ИмяФЛ", OUTPUT vCustNam) NO-ERROR.
      RUN GetCharAttrValue365p(iHReqInfo, "ОтчФЛ", OUTPUT vCustOtc) NO-ERROR.
      vCustCName = TRIM(vCustFam) + " " + TRIM(vCustNam) + " " + TRIM(vCustOtc).
   END.
   
   RUN CreateTTCust365p(OUTPUT oHCust365p) {&RAISE-ERROR}.
   
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("GetCust390p",SUBSTITUTE("~n
         vCustINN   = &1~n
         vCustKPP   = &2~n
         vCustCName = &3~n",
         vCustINN,vCustKPP,vCustCName)).
   &ENDIF
   
   DO vI = 1 TO NUM-ENTRIES(vCustCName, " "):
       FOR EACH person WHERE
           person.name-last = ENTRY(vI, vCustCName, " ") AND
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
                                vCustCName,
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

   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("GetCust390p",SUBSTITUTE("~n
         vCorrectName = &1~n
         vCorpCnt     = &2~n",
         STRING(vCorrectName),STRING(vCorpCnt))).
   &ENDIF
   
   FOR EACH cust-ident WHERE
       cust-ident.cust-code-type = "ИНН"    AND
       cust-ident.cust-code      = vCustINN AND
       cust-ident.cust-cat       = "Б"
   NO-LOCK,
   EACH banks WHERE
       banks.bank-id = cust-ident.cust-id AND
       banks.client
   NO-LOCK:
       IF iCheckCorpName = YES AND banks.name <> vCustCName THEN DO:
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
   
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("GetCust390p",SUBSTITUTE("~n
         vPersCnt  = &1~n
         vCorpCnt  = &2~n
         vBankCnt  = &3~n
         vCustKPP  = &4~n",
         STRING(vPersCnt),
         STRING(vCorpCnt),
         STRING(vBankCnt),
         vCustKPP)).
   &ENDIF
   
   IF vPersCnt + vCorpCnt + vBankCnt > 1 THEN 
   DO:
      RUN FilterCustByAcct390p IN THIS-PROCEDURE (
         iHReqAcct,
         INPUT-OUTPUT oHCust365p)
      NO-ERROR.
   END.
   IF ERROR-STATUS:ERROR THEN 
   DO:
      vS = RETURN-VALUE.
      RUN DeleteObject365p(oHCust365p).
      RETURN ERROR vS.
   END.
   
   RUN GetNumRecords365p(oHCust365p, OUTPUT vI) NO-ERROR.
   
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("GetCust390p",SUBSTITUTE("~n1.vI = &1~n",STRING(vI))).
   &ENDIF
   
   IF vI > 1 THEN DO:
       vS = FGetSetting("Настройка_365П", "МножКл", "").
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
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("GetCust390p",SUBSTITUTE("~n2.vI = &1~n",STRING(vI))).
   &ENDIF
   oI = vI.
END PROCEDURE.
/**/
PROCEDURE FindFirstCustRecord390p.
    DEFINE INPUT  PARAMETER iHCust365p       AS   HANDLE        NO-UNDO.
    DEFINE OUTPUT PARAMETER oHCustBuffer365p AS   HANDLE        NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustID          AS   INT64         NO-UNDO.
    DEFINE OUTPUT PARAMETER oCustCat         AS   CHARACTER     NO-UNDO.

    DEFINE VARIABLE vHQuery          AS HANDLE NO-UNDO.

    IF NOT VALID-HANDLE(iHCust365p) THEN
        RETURN ERROR "Не найдена таблица {&tt-cust365p-name}".
    oHCustBuffer365p = iHCust365p:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY vHQuery.
    vHQuery:SET-BUFFERS(oHCustBuffer365p).
    vHQuery:QUERY-PREPARE("FOR EACH " + oHCustBuffer365p:NAME     +
                          " NO-LOCK").
    vHQuery:QUERY-OPEN().
    vHQuery:GET-FIRST().
    oCustID  = oHCustBuffer365p:BUFFER-FIELD("cust-id"):BUFFER-VALUE.
    oCustCat = oHCustBuffer365p:BUFFER-FIELD("cust-cat"):BUFFER-VALUE.
    vHQuery:QUERY-CLOSE().
    DELETE OBJECT vHQuery.
    IF NOT oHCustBuffer365p:AVAILABLE THEN
        oHCustBuffer365p = ?.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Процедура блокировки счетов                                                */
/*----------------------------------------------------------------------------*/
PROCEDURE FTSBlockAcct.
   DEFINE INPUT  PARAMETER hRExch   AS HANDLE      NO-UNDO.  /* Ссылка на врем. таблицу */
   DEFINE        PARAMETER BUFFER   ttBlockAcct    FOR ttBlockAcct.
   DEFINE INPUT  PARAMETER iBlkType AS CHARACTER   NO-UNDO.  /*  Тип блокировки  */
   DEFINE INPUT  PARAMETER iUserSel AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vQuant        AS INT64                NO-UNDO.
   DEFINE VARIABLE vAddr         AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vINN          AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vKPP          AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vType         AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCode         AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vBegPeriod    AS DATETIME             NO-UNDO.
   DEFINE VARIABLE vEndPeriod    AS DATETIME             NO-UNDO.
   DEFINE VARIABLE vStr          AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vPriority     AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustTable    AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vContrOrg     AS LOGICAL              NO-UNDO.
   DEFINE VARIABLE vBegDateBl    AS CHARACTER            NO-UNDO.   
   DEFINE VARIABLE vTmpDT        AS DATETIME             NO-UNDO.
   DEFINE VARIABLE vCustINN      AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustKPP      AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustCName    AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustFam      AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustNam      AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vCustOtc      AS CHARACTER            NO-UNDO.
   
   IF mCustCat EQ "Ю" THEN
   DO:
      RUN GetCharAttrValue365p(hRExch, "ИННЮЛ", OUTPUT vCustINN) NO-ERROR.
      RUN GetCharAttrValue365p(hRExch, "КППЮЛ", OUTPUT vCustKPP) NO-ERROR.
      RUN GetCharAttrValue365p(hRExch, "НаимЮЛ", OUTPUT vCustCName) NO-ERROR.
   END.
   ELSE IF mCustCat EQ "Ч" THEN
   DO:
      RUN GetCharAttrValue365p(hRExch, "ИННФЛ",  OUTPUT vCustINN) NO-ERROR.
      RUN GetCharAttrValue365p(hRExch, "ФамФЛ", OUTPUT vCustFam) NO-ERROR.
      RUN GetCharAttrValue365p(hRExch, "ИмяФЛ", OUTPUT vCustNam) NO-ERROR.
      RUN GetCharAttrValue365p(hRExch, "ОтчФЛ", OUTPUT vCustOtc) NO-ERROR.
      vCustCName = TRIM(vCustFam) + " " + TRIM(vCustNam) + " " + TRIM(vCustOtc).
   END.

   IF NOT (AVAILABLE ttBlockAcct AND VALID-HANDLE(hRExch)) THEN
       RETURN.
       
   &IF DEFINED(IS-DEBUG) > 0 &THEN
   RUN dbgprint.p ("FTSBlockAcct", "~n" + 
                   "iBlkType        :" + GetNullStr(iBlkType)         + "~n" +
                   "iUserSel        :" + GetNullStr(STRING(iUserSel)) + "~n" +
                   "ttBlockAcct.acct:" + GetNullStr(ttBlockAcct.acct) + "~n"
                  ).
   &ENDIF
   
   ASSIGN
       vQuant    = 0
       vContrOrg = (TRIM(TRNSettingValue("ExchSET-BNK","ContrOrg","")) = "Код")
       vPriority = GetCode("acct-status", iBlkType)
   .

   IF INDEX(vPriority, "ОчПлат(") > 0 THEN
       ASSIGN
           vPriority = SUBSTRING(vPriority,  INDEX(vPriority, "ОчПлат(") + 7)
           vPriority = SUBSTRING(vPriority, 1, INDEX(vPriority, ")") - 1)
           vPriority = TRIM(vPriority)
       .

   FIND FIRST acct WHERE
       acct.acct     = ttBlockAcct.acct AND
       acct.currency = ttBlockAcct.currency
   NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN DO:
      IF acct.close-date <> ?
         AND
         acct.close-date <= gend-date
         AND
         FGetSetting("Настройка_365П", "ИмпБлокЗакр", "Нет") <> "Да"
      THEN DO:
          ASSIGN
              ttBlockAcct.error-code = "95"
              ttBlockAcct.msg        = "Счет закрыт на дату " +
                                       date2str(gend-date)    +
                                       ". Блокировка не обработана."
          .
          RETURN.
      END.
      vBegPeriod = DATE(hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE).
      vEndPeriod = gend-date + 1.
      vEndPeriod = vEndPeriod - 1.

      vStr  =  hRExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE + ";" +
               hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE.
      FOR EACH BlockObject WHERE BlockObject.class-code   EQ "blockacct"
                             AND BlockObject.file-name    EQ "acct"
                             AND BlockObject.surrogate    EQ ttBlockAcct.acct + "," + ttBlockAcct.currency
                             AND BlockObject.block-type   EQ iBlkType
                             AND BlockObject.txt[4]       EQ vStr
      NO-LOCK:
         /*
            0200763: Условие для момента начала действия блокировки
            перенесено внутрь цикла. В противном случае на некоторых
            версиях OE DataServer возможна ошибка (Memory Violation).
         */
         IF BlockObject.beg-datetime >= vBegPeriod AND
            BlockObject.beg-datetime <= vEndPeriod
         THEN
            LEAVE.
      END.
      
      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("FTSBlockAcct", "~n" + 
                      "vBegPeriod       :" + GetNullStr(STRING(vBegPeriod))                                           + "~n" +
                      "НомРешПр         :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE) + "~n" +
                      "КодТО            :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE)    + "~n" +
                      "КонтрОрг         :" + GetNullStr(STRING(vContrOrg))                                            + "~n" +
                      "Очередность      :" + GetNullStr(vPriority)                                                    + "~n" +
                      "AVAIL BlockObject:" + GetNullStr(STRING(AVAIL(BlockObject)))                                   + "~n"
                     ).
      &ENDIF
      
      IF NOT AVAIL BlockObject THEN
      DO:
         vBegDateBl = TRNSettingValue("ExchSET-BNK", "BegDateBl","Опердень2").
         IF vBegDateBl EQ "Из файла" AND 
            DATE(vBegPeriod) < gend-date
         THEN DO:
            ttBlockAcct.error-code = "31".
            ttBlockAcct.msg = "Внимание! Импорт блокировки за прошедшую дату " + STRING(vBegPeriod) + " невозможен.".
             MESSAGE ttBlockAcct.msg VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN.
         END.
         ELSE DO:
            
            CREATE BlockObject.
            ASSIGN
               BlockObject.file-name     = "acct"
               BlockObject.surrogate     = ttBlockAcct.acct + "," + ttBlockAcct.currency
               BlockObject.block-type    = iBlkType  /*Тип блокировки*/
               BlockObject.class-code    = "blockacct"
               BlockObject.user-id       = IF iUserSel THEN USERID("bisquit") ELSE acct.user-id /*ОИ*/
               BlockObject.txt[1]        = vPriority /*Очередность*/  
               BlockObject.txt[3]        = "Решение" /*Тип постановления*/
               BlockObject.txt[5]        = ""
               BlockObject.txt[6]        = ""
               BlockObject.txt[7]        = ""
               BlockObject.txt[8]        = "Импорт"  /*Доп.информация*/
               BlockObject.txt[9]        = ""
               BlockObject.txt[10]       = ""
               BlockObject.val[1]        = 0.0
               BlockObject.val[2]        = 0.0
               BlockObject.val[4]        = 0.0 
               BlockObject.val[5]        = 0.0 
               BlockObject.val[6]        = 0.0 
               BlockObject.val[7]        = 0.0 
               BlockObject.val[8]        = 0.0 
               BlockObject.val[9]        = 0.0 
               BlockObject.val[10]       = 0.0 
            .
            /*Орган*/
            BlockObject.txt[2] = IF vContrOrg THEN hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE ELSE "ТО".
            
            IF NOT {assigned vBegDateBl} OR vBegDateBl EQ "Опердень" THEN
                vTmpDT = DATETIME(gend-date,MTIME).
            ELSE IF vBegDateBl EQ "Из файла" THEN DO:
                vTmpDT = vBegPeriod.
            END. 
            ELSE IF vBegDateBl = "Смешанный" THEN
                vTmpDT = IF DATE(vBegPeriod) <= gend-date THEN
                             DATETIME(gend-date, MTIME)
                         ELSE
                             vBegPeriod.
            /*Начало блокировки*/
            RUN SetBlockObjectBegDT(BUFFER BlockObject, vTmpDT) NO-ERROR.  
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE BlockObject.
                ASSIGN
                    ttBlockAcct.error-code = "31"
                    ttBlockAcct.msg        = "Невозможно создать блокировку " +
                                             "типа " + BlockObject.block-type +
                                             " с датой начала "               +
                                             STRING(DATE(vTmpDT))             +
                                             ". Достигнуто максимальное "     +
                                             "количество блокировок данного " +
                                             "счёта на указанную дату"
                .
                RETURN.
            END.
            /*Постановление*/
            BlockObject.txt[4] = hRExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE + ";" +
                                 hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE.
            /*Лимит остатка*/
            mSummVZ = hRExch:BUFFER-FIELD(GetMangledName("СуммВзыск")):BUFFER-VALUE.
            IF mSummVZ EQ ? 
               THEN mSummVZ = 0.
            IF ttBlockAcct.currency EQ "" THEN
               BlockObject.val[3]        = (-1) * mSummVZ.
            ELSE
               BlockObject.val[3] = (-1) * CurFromBase("УЧЕТНЫЙ", ttBlockAcct.currency, gend-date, mSummVZ).
            /*Рублевый эквивалент*/
            BlockObject.val[4] = (-1) * mSummVZ.
            /*ДР Дата принятия решения о блокировке*/
            UpdateSigns("blockacct",
                        STRING(BlockObject.BlockObjectID),
                        "ДатаРешПр",
                        STRING(DATE(
                        hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE),
                        "99/99/9999"),
                        ?).
            vQuant = vQuant + 1.
            mAcctDone = mAcctDone + 1.
            ttBlockAcct.error-code = "".
            ttBlockAcct.msg = "Счет блокирован c " + STRING(BlockObject.beg-datetime).
            RELEASE BlockObject.

            IF      acct.cust-cat = "Ю" THEN vCustTable = "cust-corp".
            ELSE IF acct.cust-cat = "Ч" THEN vCustTable = "person".
            IF {assigned vCustTable} THEN 
               UpdateSigns(vCustTable,
                           STRING(acct.cust-id),
                           "ЗапрОткрСч",
                           "Да",
                           ?).
         END.                            
      END.   /*IF NOT AVAIL BlockObject*/
      ELSE
         ASSIGN
            ttBlockAcct.error-code = "31"
            ttBlockAcct.msg        = "Блокировка уже установлена.".
   END. /*IF AVAILABLE acct*/

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Процедура разблокировки счетов                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE FTSUnblockAcct.
   DEFINE INPUT  PARAMETER hRExch   AS HANDLE      NO-UNDO.
   DEFINE        PARAMETER BUFFER   ttBlockAcct    FOR ttBlockAcct.

   DEFINE VARIABLE vQuant   AS INT64   NO-UNDO.
   DEFINE VARIABLE vAddr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vINN     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKPP     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcct    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate    AS DATE      NO-UNDO .
   DEFINE VARIABLE vPrDate  AS DATETIME  NO-UNDO .
   DEFINE VARIABLE vStr     AS CHARACTER NO-UNDO .
   DEFINE VARIABLE vIsBlck    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vCustTable AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vContrOrg  AS LOGICAL     NO-UNDO.
   DEFINE BUFFER bAcct FOR acct.

   &IF DEFINED(IS-DEBUG) > 0 &THEN
   &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("FTSUnBlockAcct", "~n" + 
                      "Start" + "~n"
                     ).
      &ENDIF
   &ENDIF
   
   IF NOT (AVAILABLE ttBlockAcct AND VALID-HANDLE(hRExch)) THEN
       RETURN.

   ASSIGN
       vQuant    = 0
       vContrOrg = (TRIM(TRNSettingValue("ExchSET-BNK", "ContrOrg", "")) = "Код")
   .

   FIND FIRST acct WHERE
       acct.acct     = ttBlockAcct.acct AND
       acct.currency = ttBlockAcct.currency
   NO-LOCK NO-ERROR.

   IF AVAIL(acct) THEN DO:

       IF acct.close-date <> ?
          AND
          acct.close-date <= gend-date
          AND
          FGetSetting("Настройка_365П", "ИмпБлокЗакр", "Нет") <> "Да"
          AND
          FGetSetting("Настройка_365П", "ОтмБлокЗакр", "Нет") <> "Да"
       THEN DO:
           ASSIGN
               ttBlockAcct.error-code = "31"
               ttBlockAcct.msg        = "Счет закрыт на дату " +
                                        date2str(gend-date)    +
                                        ". Блокировка не обработана."
           .
           RETURN.
       END.

      vPrDate = hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE.
      vStr    = TRIM(hRExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE).

      FOR EACH BlockObject WHERE   BlockObject.class-code   EQ "blockacct"
                               AND BlockObject.file-name    EQ "acct"
                               AND BlockObject.surrogate    EQ ttBlockAcct.acct + "," + ttBlockAcct.currency
                               AND ENTRY(1, BlockObject.txt[4], ";") EQ vStr
      NO-LOCK:
         vDate = DATE(GetXattrValueEx("BlockObject",STRING(BlockObject.BlockObjectID),"ДатаРешПр","")) NO-ERROR.
         IF vDate NE ? AND vDate EQ DATE(hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE) THEN
         LEAVE.
      END.
      
      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("FTSUnBlockAcct", "~n" + 
                      "ДатаРешПр        :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE) + "~n" +
                      "НомРешПр         :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE)  + "~n" +
                      "НомРешОт         :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("НомРешОт")):BUFFER-VALUE)  + "~n" +
                      "ДатаРешОт        :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("ДатаРешОт")):BUFFER-VALUE) + "~n" +
                      "КодТО            :" + GetNullStr(hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE)     + "~n" +
                      "КонтрОрг         :" + GetNullStr(STRING(vContrOrg))                                             + "~n" +
                      "AVAIL BlockObject:" + GetNullStr(STRING(AVAIL(BlockObject)))                                    + "~n"
                     ).
      &ENDIF
 
      IF NOT AVAILABLE BlockObject THEN DO:
         ttBlockAcct.error-code = "31".
         ttBlockAcct.msg = "Не найдена блокировка для счета. " +
                           "Блокировка не обработана."         + "~n" + 
                           "Решение о приостановлении " + 
            hRExch:BUFFER-FIELD(GetMangledName("НомРешПр" )):BUFFER-VALUE + " от " + 
            hRExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE +
                           " Решение об отмене " +        
            hRExch:BUFFER-FIELD(GetMangledName("НомРешОт" )):BUFFER-VALUE + " от " + 
            hRExch:BUFFER-FIELD(GetMangledName("ДатаРешОт")):BUFFER-VALUE.
         RETURN.
      END.

      IF BlockObject.END-datetime NE ? THEN DO:
         ttBlockAcct.error-code = "31".
         ttBlockAcct.msg = "Блокировка по счету уже закрыта.".
         RETURN.
      END.

      vDate = hRExch:BUFFER-FIELD(GetMangledName("ДатаРешОт")):BUFFER-VALUE .
      DO TRANSACTION:
         FIND CURRENT BlockObject EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            BlockObject.END-datetime  = DATETIME(gend-date,MTIME)
            BlockObject.txt[6]        = "Решение"
         .
         BlockObject.txt[5]        = IF vContrOrg THEN  hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE ELSE "ТО".
         BlockObject.txt[7]        = hRExch:BUFFER-FIELD(GetMangledName("НомРешОт")):BUFFER-VALUE + ";" + hRExch:BUFFER-FIELD(GetMangledName("КодТО")):BUFFER-VALUE.

         UpdateSigns("blockacct",
                     STRING(BlockObject.BlockObjectID),
                     "ДатаРешОт",
                     STRING(vDate,"99/99/9999"),
                     ?).
      END.
      vQuant = vQuant + 1.
      mAcctDone = mAcctDone + 1.
      ttBlockAcct.error-code = "".
      ttBlockAcct.msg = "Счет разблокирован с " + STRING(BlockObject.end-datetime).

      vIsBlck = NO.
      FOR EACH bAcct WHERE
               bAcct.cust-cat EQ acct.cust-cat 
           AND bAcct.cust-id  EQ acct.cust-id
               NO-LOCK,
          EACH BlockObject WHERE BlockObject.class-code   EQ "blockacct"
                             AND BlockObject.file-name    EQ "acct"
                             AND BlockObject.surrogate    EQ bAcct.acct + "," + bAcct.currency
      NO-LOCK:
         /*
            0200763: Условие для момента окончания действия блокировки
            перенесено внутрь цикла. В противном случае на некоторых
            версиях OE DataServer возможна ошибка (Memory Violation).
         */
         IF BlockObject.end-datetime  = ? OR
            BlockObject.end-datetime >= DATETIME(gend-date, MTIME)
         THEN DO:
            vIsBlck = YES.
            LEAVE.
         END.
      END.
      IF NOT vIsBlck THEN DO:

         IF      acct.cust-cat = "Ю" THEN vCustTable = "cust-corp".
         ELSE IF acct.cust-cat = "Ч" THEN vCustTable = "person".
         IF {assigned vCustTable} THEN 
            UpdateSigns(vCustTable,
                        STRING(acct.cust-id),
                        "ЗапрОткрСч",
                        "Нет",
                        ?).
         
         /*обработка документов*/
         
         {empty tmprecid}
         
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 gend-date,
                                 gend-date,
                                 "√").
         
         IF    (sh-bal NE 0.0 AND acct.currency EQ "")
            OR (sh-val NE 0.0 AND acct.currency NE "") THEN
         DO:
            CREATE tmprecid.
            tmprecid.id = RECID(acct).
         END.
         
         RUN trnfrc_2.p.

      END. 
   END.

END PROCEDURE.
/**/
PROCEDURE SetBlockObjectBegDT.
    DEFINE PARAMETER BUFFER BlockObject FOR  BlockObject.
    DEFINE INPUT  PARAMETER iDateTime   LIKE BlockObject.beg-datetime NO-UNDO.

    DEFINE BUFFER xBlockObject FOR BlockObject.

    DEFINE VARIABLE vDTInf LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTSup LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTM   LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTP   LIKE BlockObject.beg-datetime NO-UNDO.

    ASSIGN
        vDTP = iDateTime
        vDTM = iDateTime
    .
    FOR EACH xBlockObject WHERE
        ROWID(xBlockObject)             <> ROWID(BlockObject)     AND
        xBlockObject.class-code         =  BlockObject.class-code AND
        xBlockObject.file-name          =  BlockObject.file-name  AND
        xBlockObject.surrogate          =  BlockObject.surrogate  AND
        xBlockObject.block-type         =  BlockObject.block-type AND
        DATE(xBlockObject.beg-datetime) =  DATE(iDateTime)
    NO-LOCK
    BY ABSOLUTE(xBlockObject.beg-datetime - iDateTime):
        IF xBlockObject.beg-datetime >= iDateTime THEN DO:
            vDTSup = xBlockObject.beg-datetime.
            IF vDTP < vDTSup THEN DO:
                vDTM = ?.
                LEAVE.
            END.
            vDTP = vDTP + 1.
        END.
        IF xBlockObject.beg-datetime <= iDateTime THEN DO:
            vDTInf = xBlockObject.beg-datetime.
            IF vDTM > vDTInf THEN DO:
                vDTP = ?.
                LEAVE.
            END.
            vDTM = vDTM - 1.
        END.
    END.
    IF vDTP <> ? AND vDTM <> ? THEN DO:
        ASSIGN
            vDTSup = DATETIME(DATE(iDateTime) + 1, 0) - 1
            vDTInf = DATETIME(DATE(iDateTime)    , 0)
        .
        IF vDTP > vDTSup THEN
            vDTP = ?.
        ELSE IF vDTM < vDTInf THEN
            vDTM = ?.
        ELSE DO:
            IF ABSOLUTE(vDTP - iDateTime) < ABSOLUTE(vDTM - iDateTime) THEN
                vDTM = ?.
            ELSE
                vDTP = ?.
        END.
    END.
    ASSIGN
        iDateTime = ?
        iDateTime = vDTP WHEN vDTP <> ?
        iDateTime = vDTM WHEN vDTM <> ?
    .
    IF iDateTime = ? THEN
        RETURN ERROR.
    BlockObject.beg-datetime = iDateTime.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Процедура вывода отчета                                                    */
/*----------------------------------------------------------------------------*/
PROCEDURE FTSReport.
   DEFINE INPUT PARAMETER ihExch    AS HANDLE   NO-UNDO.
   DEFINE       PARAMETER BUFFER ttBlockAcct FOR ttBlockAcct.
   DEFINE INPUT PARAMETER iSeanceID AS INT64    NO-UNDO.
   DEFINE INPUT PARAMETER iPacketID AS INT64    NO-UNDO.
   
   DEFINE VARIABLE mShow     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE mBankName AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mUser     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mFName    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mI        AS INT64       NO-UNDO.
   DEFINE VARIABLE mStr      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mINN      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mSumVz    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mNomResh  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mDatResh  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mNaimNP   AS CHARACTER   NO-UNDO  EXTENT 5.
   DEFINE VARIABLE mFileName AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBuffer   AS CHARACTER   NO-UNDO.

   FIND FIRST Seance WHERE Seance.SeanceID EQ iSeanceID NO-LOCK NO-ERROR.

   ASSIGN
      mAcctCount = 0
      mAcctDone  = 0. 
   FOR EACH ttBlockAcct NO-LOCK:
      mAcctCount = mAcctCount + 1.
      IF ttBlockAcct.error-code EQ "" THEN
         mAcctDone = mAcctDone + 1.
   END.
   IF vMode EQ "Cancel" THEN
   ASSIGN
      mSumVz =  ""
      mNomResh = ihExch:BUFFER-FIELD(GetMangledName("НомРешОт")):BUFFER-VALUE
      mDatResh = ihExch:BUFFER-FIELD(GetMangledName("ДатаРешОт")):BUFFER-VALUE.
   ELSE
   ASSIGN
      mSumVz =  ihExch:BUFFER-FIELD(GetMangledName("СуммВзыск")):BUFFER-VALUE
      mNomResh = ""
      mDatResh = "".
   IF mCustCat EQ "Ю" THEN
      ASSIGN
         mNaimNP[1] = ihExch:BUFFER-FIELD(GetMangledName("НаимЮЛ")):BUFFER-VALUE
         mINN       = ihExch:BUFFER-FIELD(GetMangledName("ИННЮЛ")):BUFFER-VALUE.
   ELSE IF mCustCat EQ "Ч" THEN
      ASSIGN
         mNaimNP[1] = ihExch:BUFFER-FIELD(GetMangledName("ФамФЛ")):BUFFER-VALUE + " " +
                      ihExch:BUFFER-FIELD(GetMangledName("ИмяФЛ")):BUFFER-VALUE + " " +
                      ihExch:BUFFER-FIELD(GetMangledName("ОтчФЛ")):BUFFER-VALUE
         mINN       = ihExch:BUFFER-FIELD(GetMangledName("ИННФЛ")):BUFFER-VALUE.

   mINN       = GetNullStr(mINN).
   mNaimNP[1] = GetNullStr(mNaimNP[1]).
   
   {wordwrap.i 
      &s = mNaimNP
      &n = 5
      &l = 50
   }
   
   RUN ReportTextKeep(
      iPacketID,
      GetNullStr(STRING(Seance.SeanceDate,"99/99/9999")) + " " + 
      GetNullStr(STRING(Seance.SeanceTime,"HH:MM:SS")), 
      INPUT-OUTPUT vResult).

   RUN ReportTextKeep(
      iPacketID,
      fStrPad(mINN,12,YES) + " " + 
      fStrPad(mNaimNP[1],50,YES) + " " +
      fStrPad(GetNullStr(ihExch:BUFFER-FIELD(GetMangledName("НомРешПр")):BUFFER-VALUE),15,YES) + " " +
      fStrPad(GetNullStr(ihExch:BUFFER-FIELD(GetMangledName("ДатаРешПр")):BUFFER-VALUE),10,YES) + " " +
      fStrPad(GetNullStr(ihExch:BUFFER-FIELD(GetMangledName("ИдРеш")):BUFFER-VALUE),20,YES) + " " +
      fStrPad(GetNullStr(mSumVz),20,YES) + " " +
      fStrPad(GetNullStr(mNomResh),10,YES) + " " +
      fStrPad(GetNullStr(mDatResh),10,YES) + " " +
      fStrPad(GetNullStr(STRING(mAcctCount,">>>9")),10,YES),
      INPUT-OUTPUT vResult).
   DO mI = 2 TO 5 :
      IF {assigned mNaimNP[mI]} THEN
         RUN ReportTextKeep (iPacketID, FILL(" ", 13) + mNaimNP[mI], INPUT-OUTPUT vResult).
   END.
   RUN ReportTextKeep (iPacketID," ", INPUT-OUTPUT vResult).
   FOR EACH ttBlockAcct WHERE ttBlockAcct.packetid EQ iPacketID NO-LOCK:
      RUN ReportTextKeep (iPacketID, GetNullStr(DelFilFromAcct(ttBlockAcct.acct)) + "    " +
                                               ttBlockAcct.msg, INPUT-OUTPUT vResult).
   END.
   RUN ReportTextKeep (iPacketID, "Обработано счетов: " + GetNullInt(mAcctDone), INPUT-OUTPUT vResult).
   RUN ReportTextKeep (iPacketID," ", INPUT-OUTPUT vResult).
   /*Сохраняем содержимое файла в Пакете*/
   FOR FIRST Packet WHERE
             Packet.PacketId EQ iPacketID NO-LOCK,
      FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID  NO-LOCK:
      ASSIGN mFileName = FileExch.Path.
   END.
   INPUT STREAM sImport FROM VALUE(mFileName).
   REPEAT:
      IMPORT STREAM sImport UNFORMATTED mBuffer.
      IF NOT {assigned mBuffer} THEN NEXT.
      mMess = mMess + (IF mMess NE "" THEN "~n" ELSE "") + mBuffer.
   END. /* REPEAT */
   INPUT STREAM sImport CLOSE.
   RUN PacketTextSave IN h_pack (iPacketID, mMess).
END PROCEDURE.
/*Вывод в протокол*/
PROCEDURE ReportTextKeep: /* -0161924-- в PacketText протокол больше не пишем */
   DEFINE INPUT        PARAMETER iPacketID   AS INT64  NO-UNDO.
   DEFINE INPUT        PARAMETER iText       AS CHAR     NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pResult     AS CHAR     NO-UNDO.
   /* ... а пишем во временную sysconf */
   RUN SaveDataProtocol IN h_base ("PacketText_" + STRING(iPacketID), iText).
   pResult = pResult + iText.
END PROCEDURE.
{pfuncdef 
 &DefProc="FilterCustByAcct390p"
 &Description="Отфильтровывает клиентов по счету
              "
 &Parameters="INPUT        PARAMETER iHReqAcct   - 
              Handle на временную таблицу со счетами
              INPUT-OUTPUT PARAMETER pHCust365p - 
              Handle на временную таблицу с клиентами"
 &Result="pHCust365p"
 &Sample="RUN FilterCustByAcct390p(
            iHReqAcct,
            INPUT-OUTPUT oHCust365p)
         NO-ERROR."}
 PROCEDURE FilterCustByAcct390p:
   DEFINE INPUT        PARAMETER iHReqAcct   AS HANDLE NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pHCust365p AS HANDLE NO-UNDO.

   DEFINE VARIABLE vHFCust365p  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vAcct        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vHBuffer     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vHQuery      AS HANDLE    NO-UNDO.

   RUN CreateTTCust365p IN THIS-PROCEDURE (OUTPUT vHFCust365p) {&RAISE-ERROR}.
   
   RUN CheckReqAcct365p IN THIS-PROCEDURE (iHReqAcct) {&RAISE-ERROR}.
   
   vHBuffer = iHReqAcct:DEFAULT-BUFFER-HANDLE.
   CREATE QUERY vHQuery.
   vHQuery:SET-BUFFERS(vHBuffer).
   vHQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK",vHBuffer:NAME)).
   vHQuery:QUERY-OPEN().
   ChkAcct:
   REPEAT:
      vHQuery:GET-NEXT().
      IF vHQuery:QUERY-OFF-END THEN
         LEAVE ChkAcct.
      IF vHBuffer:AVAILABLE THEN 
      DO:
         RUN GetCharAttrValue365p IN THIS-PROCEDURE (vHBuffer,
            "НомСч",
            OUTPUT vAcct)
            NO-ERROR.
         RUN TransferAcctCust365p IN THIS-PROCEDURE (vHFCust365p,
            pHCust365p,
            vAcct)
            {&RAISE-ERROR}.
      END.
   END.
   vHQuery:QUERY-CLOSE().
   DELETE OBJECT vHQuery.

   IF VALID-HANDLE(vHFCust365p) THEN 
   DO:
      RUN DeleteObject365p IN THIS-PROCEDURE (pHCust365p).
      pHCust365p = vHFCust365p.
   END.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:20:04.701+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-xfts.p' */
/*prosignuG8xIB2TOodvLwtIBynuBw*/