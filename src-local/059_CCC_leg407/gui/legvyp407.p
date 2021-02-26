/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: legvyp407.p
      Comment: <Выписка по операциям по счетам 407-П 365-П>
   Parameters: Нет
      Created: 18.05.2014 12:17 GUVA
     Modified: 18.05.2014 12:17 GUVA     <0217858>
     Modified: 07.10.2015 17:45 ANBA     
*/

/* ***************************  Parameters  *************************** */
/* ************************  Global Definitions  ********************** */
{globals.i}
{form.def}
{g-trans.equ}
{exchange.equ}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get cust}
{intrface.get count}
{ttretval.def}
{intrface.get exch}
{intrface.get pack}
{intrface.get strng}
{intrface.get exch}
{intrface.get trans}
{intrface.get filex}
{intrface.get instrum}
{intrface.get pack}
{intrface.get rfrnc}
{intrface.get pbase}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */
{intrface.get plbnk}
 
{365p.def}

&GLOB    REQESTEDINFO-LINE  "Сведения о запрашиваемой информации"
&GLOBAL-DEFINE VYP-NUM-FORMAT 999

DEFINE BUFFER acct-ext FOR acct.
   
DEFINE STREAM   sImport.
DEFINE STREAM   sReq.

DEFINE VARIABLE mResult          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mReqTrans        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnsTrans        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMess            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFile1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountAll        AS INT64     NO-UNDO.

DEFINE VARIABLE mSuperPacketID   AS INT64     NO-UNDO.

DEFINE VARIABLE m365_Join        AS CHARACTER NO-UNDO.
DEFINE VARIABLE m365_BalAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE m365_Contract    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTmp          AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE mExchMain        AS HANDLE    NO-UNDO.
DEFINE VARIABLE mInf             AS HANDLE    NO-UNDO.
DEFINE VARIABLE mFlag            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDummy           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINNNP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPPNP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBaseName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDirOut          AS CHARACTER INIT ""  NO-UNDO.
DEFINE VARIABLE mRid             AS INT64     NO-UNDO.
DEFINE VARIABLE mVypNum          AS INT64     NO-UNDO.
DEFINE VARIABLE mHCust365p       AS HANDLE    NO-UNDO.
DEFINE VARIABLE mH               AS HANDLE    NO-UNDO.
DEFINE VARIABLE mCustNameTag     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustNameAltTag  AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE mKolMaxDoc     AS INT64     NO-UNDO.

DEFINE VARIABLE mINN           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFO           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBank          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNCCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchKas    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchMBR    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate_NR       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorrACCT      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParSchStN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNumDocs       AS INT64     NO-UNDO.
DEFINE VARIABLE mBaseName0     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFileName0     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlLst         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt           AS INT64     NO-UNDO.
DEFINE VARIABLE mNumFls        AS INT64     NO-UNDO.

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
   mKolMaxDoc    = INT64(TRIM(FGetSetting("Настройка_365П", "КолМаксДок", "0")))
NO-ERROR.

DEFINE BUFFER   user-proc    FOR user-proc.
DEFINE BUFFER   acct         FOR acct.
DEFINE BUFFER   op           FOR op.
DEFINE BUFFER   op-entry     FOR op-entry.
DEFINE BUFFER   op-bank      FOR op-bank.
DEFINE BUFFER   op-kind      FOR op-kind.
DEFINE BUFFER   op-date      FOR op-date.
DEFINE BUFFER   doc-type     FOR doc-type.
DEFINE BUFFER   signs        FOR signs.
DEFINE BUFFER   class        FOR class.
DEFINE BUFFER   DataCli      FOR DataLine.
DEFINE BUFFER   DataReq      FOR DataLine.

DEFINE TEMP-TABLE ttBlockAcct
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

DEFINE VARIABLE oHTable2 AS HANDLE      NO-UNDO. /* исходящие счета*/
DEFINE VARIABLE oHTable3 AS HANDLE      NO-UNDO. /* исходящие документы*/

DEFINE VARIABLE mReqKind AS CHARACTER NO-UNDO INITIAL "{&REQ-KIND-TICLAM}".
DEFINE VARIABLE mReqType AS CHARACTER NO-UNDO INITIAL "{&REQ-TYPE-SELECTED}".

DEFINE VARIABLE mRsSrv   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsInf   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsAcc   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsDoc   AS CHARACTER NO-UNDO.

{core365p.pro}
{zno365p2.pro}
{taxrsexp.i}
{taxrsexp2.i}

/*&global-define tmprecid yes

DEFINE INPUT  PARAMETER iClass      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iInstance   AS HANDLE     NO-UNDO.
*/

/* ************************  Local Definitions  *********************** */

   DEFINE VARIABLE oHTable      AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vErrCode     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrText     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mCustID      AS INT64       NO-UNDO.
   DEFINE VARIABLE mCustCat     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mDataBlockID AS INT64       NO-UNDO.
   DEFINE VARIABLE mParams      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mExistClient AS LOGICAL     NO-UNDO.

   
   mDataBlockID = INT64(GetSysconf("DATABLOCKID")).
   FIND FIRST DataBlock WHERE DataBlock.Data-Id EQ mDataBlockID NO-LOCK NO-ERROR.

   DEFINE VARIABLE mAccts AS CHARACTER FORMAT "X(25)":U 
        LABEL "Счета" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 32 BY 1
        &ELSE SIZE 32 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mBIC AS CHARACTER FORMAT "X(9)":U 
        LABEL "БИК" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
        &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mDate-beg AS DATE FORMAT "99/99/9999":U 
        LABEL "Период" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
        &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mDate-end AS DATE FORMAT "99/99/9999":U 
        LABEL "по" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
        &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mDateReq AS DATE FORMAT "99/99/9999":U 
        LABEL "Дата запроса" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
        &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mDateVyp AS DATE FORMAT "99/99/9999":U 
        LABEL "Дата выписки" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
        &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

   DEFINE VARIABLE mNumberReq AS CHARACTER FORMAT "X(17)":U 
        LABEL "Номер запроса" 
        VIEW-AS FILL-IN 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
        &ELSE SIZE 26 BY 1 &ENDIF NO-UNDO.

/* *****************************  Frames  ***************************** */
/*   DEFINE FRAME VYP-FRAME */
   FORM
        mDate-beg
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 15 COLON-ALIGNED
             &ELSE AT ROW 3 COL 15 COLON-ALIGNED &ENDIF
        mDate-end
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 30 COLON-ALIGNED
             &ELSE AT ROW 3 COL 30 COLON-ALIGNED &ENDIF
        mAccts
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 15 COLON-ALIGNED
             &ELSE AT ROW 4 COL 15 COLON-ALIGNED &ENDIF
        mDateReq
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 15 COLON-ALIGNED
             &ELSE AT ROW 5 COL 15 COLON-ALIGNED &ENDIF
        mNumberReq
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 15 COLON-ALIGNED
             &ELSE AT ROW 6 COL 15 COLON-ALIGNED &ENDIF
        mBIC
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 15 COLON-ALIGNED
             &ELSE AT ROW 7 COL 15 COLON-ALIGNED &ENDIF
        mDateVyp
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 15 COLON-ALIGNED
             &ELSE AT ROW 8 COL 15 COLON-ALIGNED &ENDIF
        "Задайте параметры:" VIEW-AS TEXT
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
             &ELSE SIZE 20 BY 1 &ENDIF
             &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 10
             &ELSE AT ROW 1 COL 10 &ENDIF
        SPACE(21.28) SKIP(8.84)
   WITH FRAME VYP-FRAME 
   OVERLAY
   CENTERED
   SIDE-LABELS
      TITLE "[ 407-П. Выписка по лицевым счетам (по 365-П) ]".


/* ****************************  Functions  *************************** */
   FUNCTION ValiDateDate RETURN LOGICAL(ipDate AS CHARACTER)
      FORWARD.
   FUNCTION JoinPaths RETURN CHARACTER (INPUT iDir  AS CHARACTER,
                                        INPUT iFile AS CHARACTER)
      FORWARD.

FUNCTION getFileNamePrefix407p RETURN CHARACTER (INPUT iReqDate   AS DATE,
                                                 INPUT iVypDate   AS DATE,
                                                 INPUT iReqNumber AS CHARACTER,
                                                 INPUT iMFO       AS CHARACTER):
   iReqNumber = TRIM(iReqNumber).
   RETURN SUBSTITUTE("INC_04_&1_&2_&3&4_",
                     SUBSTITUTE("&1&2&3",
                                STRING(DAY(iReqDate), "99"),
                                STRING(MONTH(iReqDate), "99"),
                                STRING(YEAR(iReqDate), "9999")),
                     SUBSTITUTE("&1&2",
                                IF LENGTH(iReqNumber) < 17
                                THEN FILL("0", 17 - LENGTH(iReqNumber))
                                ELSE "",
                                REPLACE(iReqNumber, "/", "_")),
                     TRIM(iMFO),
                     SUBSTITUTE("&1&2&3",
                                STRING(YEAR(iVypDate), "9999"),
                                STRING(MONTH(iVypDate), "99"),
                                STRING(DAY(iVypDate), "99"))).
END FUNCTION.

/* ****************************  Procedures  ************************** */
PROCEDURE GetFileName407p:
   DEFINE INPUT        PARAMETER iReqDate   AS DATE      NO-UNDO.
   DEFINE INPUT        PARAMETER iVypDate   AS DATE      NO-UNDO.
   DEFINE INPUT        PARAMETER iReqNumber AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER iMFO       AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pCounter   AS INT64     NO-UNDO.

   DEFINE BUFFER user-proc FOR user-proc.

   DEFINE VARIABLE vRecId  AS RECID     NO-UNDO.
   DEFINE VARIABLE vPrefix AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDirOut AS CHARACTER NO-UNDO.

   IF pCounter = ? OR pCounter < 0 THEN
      pCounter = 0.
   ASSIGN
      vPrefix  = getFileNamePrefix407p(iReqDate, iVypDate, iReqNumber, iMFO)
      pCounter = pCounter + 1
   .
   IF NOT {assigned vPrefix} THEN
      RETURN.
   ASSIGN
      mBaseName = SUBSTITUTE("&1&2.txt",
                             vPrefix,
                             STRING(pCounter, "{&VYP-NUM-FORMAT}"))
      vRecId    = INT64(GetSysConf("user-proc-id"))
   .
   FIND FIRST user-proc WHERE RECID(user-proc) = vRecId NO-LOCK NO-ERROR.
   IF AVAILABLE user-proc THEN
      vDirOut = GetXAttrValueEx("user-proc",
                                STRING(user-proc.public-number), 
                                "Дир", 
                                "").
   mFileName = JoinPaths(vDirOut, mBaseName).  
   IF LOOKUP(mFileName, mFlLst) = 0 THEN
      {additem.i mFlLst mFileName}
   IF pCounter = 1 THEN
      ASSIGN
         mBaseName0 = mBaseName
         mFileName0 = mFileName
      .  
END PROCEDURE.

PROCEDURE SwitchFile407p:
   DEFINE INPUT PARAMETER iDryRun AS LOGICAL NO-UNDO.

   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   vI = mKolMaxDoc. 
   IF ERROR-STATUS:ERROR OR vI < 1 THEN
      vI = 0.
   IF mVypNum = 0 OR (vI > 0 AND mNumDocs >= vI) THEN DO:
      vI = mVypNum.
      RUN GetFileName407p IN THIS-PROCEDURE (mDateReq,
                                             mDateVyp,
                                             mNumberReq,
                                             mBIC,
                                             INPUT-OUTPUT mVypNum).
                                                                   
      IF mVypNum > {&VYP-NUM-FORMAT} THEN DO:
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "-1",
                                     "Количество фрагментов выписки "    +
                                     "превысило максимально допустимое " +
                                     "значение {&VYP-NUM-FORMAT}").
         RETURN ERROR.
      END.
      
      IF mNumDocs <> 0 THEN
      DO:
          RUN PrintStr IN THIS-PROCEDURE("@@@").
          RUN PrintStr IN THIS-PROCEDURE("===").
      END.     
      IF iDryRun THEN
         mVypNum = vI.
      ELSE DO:
         {setdest.i &nodef    = "/*"
                    &filename = "mFileName"
                    &custom   = "0 *"
                    &stream   = "STREAM sReq"}
         mNumDocs = 0.
      END.      
   END.

END PROCEDURE.

/* ****************************  Triggers  **************************** */
   
ON F1 OF FRAME VYP-FRAME  ANYWHERE  /* 407-П. Выписка по лицевым счетам (по 365-П) */
DO:
   DEFINE VARIABLE vCust-Id      AS INT64   INIT 0  NO-UNDO.
   DEFINE VARIABLE vCust-IDDL    AS INT64           NO-UNDO.
   DEFINE VARIABLE vCust-CatDL   AS CHARACTER       NO-UNDO.

   DEFINE BUFFER   bAcct  FOR acct.
   DEFINE BUFFER   bfacct FOR acct.
   DEFINE BUFFER   bMainDataLine    FOR DataLine.
   DEFINE BUFFER   bClientDataLine  FOR DataLine.

   pick-value = ?.
   IF SELF:DATA-TYPE EQ "DATE" THEN
   DO:
      RUN calend.p.
      IF (   LASTKEY EQ 10
          OR LASTKEY EQ 13)
         AND pick-value NE ? 
      THEN 
      DO:
         SELF:SCREEN-VALUE = pick-value.
         APPLY "LEAVE" TO SELF  IN FRAME VYP-FRAME.  
         RETURN NO-APPLY.
      END.
   END.
   IF SELF:NAME EQ "mAccts" THEN
   DO:
      EMPTY TEMP-TABLE ttRetVal.
      DO TRANSACTION:
         RUN browseld.p("acct",
                    "RetRcp" + CHR(1) + 
                    "RetFld" + CHR(1) + 
                    "RetType",
                    STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1) + 
                    "acct,currency" + CHR(1) + 
                    "multi",
                        "",
                        4).
      END. 
    IF KEYFUNCTION (LASTKEY) NE "END-ERROR" THEN
    DO:
       FOR EACH ttRetVal WHERE NO-LOCK:
          FIND FIRST bacct 
             WHERE bacct.acct EQ ENTRY (1, ttRetVal.PickValue)
             AND bacct.currency EQ ENTRY (2, ttRetVal.PickValue)
             NO-LOCK NO-ERROR.
          IF AVAILABLE bacct THEN
          DO :
             IF vCust-Id EQ 0 
                THEN
                vCust-Id = bacct.cust-id.
             IF vCust-Id NE bacct.cust-id
                THEN
             DO:
                MESSAGE "Выбраны счета разных клиентов."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
                vErrCode = "ERROR".
                APPLY "GO" TO FRAME VYP-FRAME.  
                RETURN NO-APPLY.
             END.
          END.
       END. /* FOR EACH: */
       IF    AVAIL DataBlock  
       THEN DO:
         FIND FIRST ttRetVal NO-ERROR.
          
         FIND FIRST bfacct WHERE 
                    bfacct.acct     EQ ENTRY (1, ttRetVal.PickValue)
                AND bfacct.currency EQ ENTRY (2, ttRetVal.PickValue)
         NO-LOCK NO-ERROR.
         IF AVAILABLE bfacct THEN
         DO:
            fndClientLine:
            FOR EACH bClientDataLine WHERE 
                     bClientDataLine.Data-Id EQ DataBlock.Data-id
                 AND bClientDataLine.Sym2    EQ "Сведения о клиенте" 
            NO-LOCK:
               ASSIGN
                  vCust-CatDL  =  GetEntries(1,bClientDataLine.Txt,"~n","")
                  vCust-IDDL   =  INT64(GetEntries(2,bClientDataLine.Txt,"~n",""))
               NO-ERROR.  
               IF     vCust-CatDL EQ bfacct.cust-cat 
                  AND vCust-IDDL  EQ bfacct.cust-id 
               THEN DO:
                  FIND FIRST bMainDataLine WHERE 
                             bMainDataLine.Data-Id EQ  DataBlock.Data-id
                         AND bMainDataLine.Sym1    EQ  bClientDataLine.Sym1 
                         AND bMainDataLine.Sym2    EQ  "Общие данные"
                  NO-LOCK NO-ERROR.
                  IF AVAIL bMainDataLine THEN 
                  ASSIGN 
                      mDateReq:SCREEN-VALUE   = GetEntries( 4,  bMainDataLine.Txt, "~n","")
                      mNumberReq:SCREEN-VALUE = GetEntries( 5,  bMainDataLine.Txt, "~n","") NO-ERROR.
                  LEAVE fndClientLine.
               END. 
               
            END.  


            
         END.
       END.   
       
       SELF:SCREEN-VALUE = "Выбраны".
       APPLY "LEAVE" TO SELF  IN FRAME VYP-FRAME.  
    END.
   END.
END.


ON LEAVE OF FRAME VYP-FRAME ANYWHERE /* 407-П. Выписка по лицевым счетам (по 365-П) */
DO:
   IF SELF:DATA-TYPE EQ "DATE" THEN
   DO:
      IF NOT ValidateDate(SELF:SCREEN-VALUE) THEN
      DO:
        SELF:SCREEN-VALUE = STRING(DATE(""), "99/99/9999").
        APPLY "ENTRY" TO SELF IN FRAME VYP-FRAME. 
        RETURN NO-APPLY.
      END.
   END.
   IF SELF:NAME EQ mAccts THEN
   DO:
      FIND FIRST ttRetVal NO-LOCK NO-ERROR.
      IF AVAILABLE ttRetVal THEN
         mAccts:SCREEN-VALUE = "Выбраны".
   END.

END. 

ON GO OF FRAME VYP-FRAME ANYWHERE /* 407-П. Выписка по лицевым счетам (по 365-П) */
DO:
 ASSIGN 
    mDate-beg 
    mDate-end 
    mAccts 
    mDateReq 
    mNumberReq 
    mBIC 
    mDateVyp.
  
END.


/* ***************************  Main Block  *************************** */
RUN Init-SysMes("","","").
RUN SetCloseOnGoSemaphore IN h_base ( THIS-PROCEDURE, YES ).

MAIN-BLOCK:
DO 
   ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   ASSIGN
      mExchMain = ObjectValueHandle("ETAXFile")
      mInf      = ObjectValueHandle("ETAXAcctZNO")
      mReqTrans = "ETAXAcctZNO,ETAXAcctBlk"
      mAnsTrans = "ETAXRSCommon,ETAXRsInfVyp,ETAXRsDataVyp"
      mInfoType = {&INFO-TYPE-ZNO}
   .
   RUN GetDate.

   IF vErrCode EQ "ERROR" THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.         

   mVypNum = GetCounterNextValue("Выписка407", TODAY).

   mBaseName = "INC_04_" + 
      STRING(DAY(mDateReq), "99") + 
      STRING(MONTH(mDateReq), "99") + 
      STRING(YEAR(mDateReq), "9999") +
      "_" + 
      (IF LENGTH(TRIM(mNumberReq)) LT 17 
      THEN FILL("0", 17 - LENGTH(TRIM(mNumberReq))) ELSE "") +
      REPLACE(TRIM(mNumberReq),  "/", "_") + 
      "_" + 
      TRIM(mBIC) +
      STRING(YEAR(mDateVyp), "9999") +
      STRING(MONTH(mDateVyp), "99") + 
      STRING(DAY(mDateVyp), "99") + 
      "_" + 
      STRING(mVypNum, "999") + 
      ".txt".

   mRid = INT64(GetSysConf("user-proc-id")).

   FIND FIRST user-proc WHERE RECID(user-proc) EQ mRid NO-LOCK NO-ERROR.
   IF AVAILABLE user-proc THEN
      mDirOut = GetXAttrValueEx("user-proc",
                                STRING(user-proc.public-number), 
                                "Дир", 
                                "").
   mFileName = JoinPaths(mDirOut, mBaseName).

   FOR EACH ttRetVal NO-LOCK:
      CREATE ttBlockAcct.
      ttBlockAcct.acct = ENTRY(1, ttRetVal.PickValue).
      IF NOT mFlag THEN DO:
         {find-act.i &acct = ttBlockAcct.acct}
         IF AVAILABLE acct THEN DO:
            ASSIGN
               mCustID   = acct.cust-id
               mCustCat  = acct.cust-cat
               mCustName = GetCliName(acct.cust-cat,
                                      STRING(acct.cust-id),
                                      OUTPUT       mDummy,
                                      OUTPUT       mINNNP,
                                      OUTPUT       mKPPNP,
                                      INPUT-OUTPUT mDummy,
                                      OUTPUT       mDummy,
                                      OUTPUT       mDummy).
            RUN GetCustName IN h_base (acct.cust-cat,
                                       acct.cust-id,
                                       acct.acct,
                                       OUTPUT mDummy,
                                       OUTPUT mDummy,
                                       INPUT-OUTPUT mINNNP).
            mFlag = YES.
         END.
      END.
      RELEASE ttBlockAcct.
   END.
   /*
      Если у клиента нет ИНН, подменяем его значение в транспортной форме 
      специальной константой, игнорируемой при проверках, а также формируем 
      представление клиента, совместимое с инструментами 365-П, прямо здесь
   */
   IF NOT {assigned mINNNP} THEN DO:
      mINNNP = {&NO-INN}.
      RUN CreateTTCust365p(OUTPUT mHCust365p) NO-ERROR.
      mH = mHCust365p:DEFAULT-BUFFER-HANDLE.
      mH:BUFFER-CREATE().
      ASSIGN
         mH::cust-cat = mCustCat
         mH::cust-id  = mCustID
      .
      mH:BUFFER-RELEASE().
   END.
   ASSIGN
      mExchMain:BUFFER-FIELD(GetMangledName("ТипИнф")):BUFFER-VALUE = {&INFO-TYPE-ZNO}

      mInf:BUFFER-FIELD(GetMangledName("ДатаЗапр")):BUFFER-VALUE    = mDateVyp:SCREEN-VALUE
      mInf:BUFFER-FIELD(GetMangledName("ДатаНач")):BUFFER-VALUE     = mDate-beg:SCREEN-VALUE
      mInf:BUFFER-FIELD(GetMangledName("ДатаКон")):BUFFER-VALUE     = mDate-end:SCREEN-VALUE
      mInf:BUFFER-FIELD(GetMangledName("ВидЗапр")):BUFFER-VALUE     = mReqKind
      mInf:BUFFER-FIELD(GetMangledName("ТипЗапр")):BUFFER-VALUE     = mReqType
      mInf:BUFFER-FIELD(GetMangledName("ИНННП")):BUFFER-VALUE       = mINNNP
      mInf:BUFFER-FIELD(GetMangledName("КППНП")):BUFFER-VALUE       = mKPPNP
      mInf:BUFFER-FIELD(GetMangledName(IF isPersonINN(mINNNP, mCustCat)
                                       THEN "ФИОИП"
                                       ELSE "НаимНП")):BUFFER-VALUE = mCustName
      .

   RUN CreateBExch_ZNO1(mInf,
                        mReqKind,
                        OUTPUT oHTable).
   RUN CreateBExch_ZNO2(mInf,
                        mHCust365p,
                        oHTable,
                        mReqKind,
                        ?,
                        OUTPUT mCountAll,
                        INPUT-OUTPUT vErrCode,
                        INPUT-OUTPUT vErrText).
                     
   IF {assigned vErrCode} AND vErrCode <> {&ERR-SUCCESS} THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ошибка: " + GetNullStr(vErrText)).
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Выписка не сформирована.").
      RETURN.
   END.

   /*
      Тег "ИдФайл" служебной части ответа
      в выписках 407-П заполняется именем файла
   */
   mH = ObjectValueHandle(ENTRY(1, mAnsTrans)).
   IF VALID-HANDLE(mH) THEN DO:
      RUN SwitchFile407p IN THIS-PROCEDURE (YES).
      RUN SetAttrValue365p IN THIS-PROCEDURE (mH, "idfawil$", mBaseName0).
   END.
   /* Информационная часть: заполнение тегов "НаимНП"/"ФИОИП" */
   mH = ObjectValueHandle(ENTRY(2, mAnsTrans)).
   IF VALID-HANDLE(mH) THEN DO:
      IF isPersonINN(mINNNP, mCustCat) THEN DO:
         RUN FixCustName365p(INPUT-OUTPUT mCustName).
         ASSIGN
            mCustNameTag    = "fioip$"
            mCustNameAltTag = "naimnp$"
         .
      END.
      ELSE
         ASSIGN
            mCustNameTag    = "naimnp$"
            mCustNameAltTag = "fioip$"
         .
      RUN SetAttrValue365p(mH, mCustNameTag, mCustName).
      RUN SetAttrValue365p(mH, mCustNameAltTag, "").
   END.
   RUN  FormattedAttrValue365p IN THIS-PROCEDURE (oHTable3, 
                                                  "naimpp$",
                                                  "x(160)" ).                                             
   RUN  FormattedAttrValue365p IN THIS-PROCEDURE (oHTable3, 
                                                  "fioip$",
                                                  "x(160)").
   RUN TAXExpSprav2 IN THIS-PROCEDURE (?, ?, mFileName).
   OUTPUT STREAM sReq CLOSE. /*Чтобы правильно определялся размер файла*/
   COPY-LOB FROM FILE mFileName TO mStrTmp NO-ERROR.

   OUTPUT CLOSE. /*Чтобы правильно определялся размер файла*/
   
   COPY-LOB FROM FILE mFileName TO mStrTmp NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RUN Fill-SysMes('',
                      '',
                      '-1',
                      'Выписка не сформирована. ~n').
   ELSE
   DO:
      DO mCnt = 1 TO NUM-ENTRIES(mFlLst): 
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "1",
                                     'Сформирована выписка в файле. ' + 
                                     ENTRY(mCnt,mFlLst)).
      END.
      /*0230124*/
      mParams = GetSysconf("IPARAMS").
      IF mParams EQ "СохрКлД" THEN
      DO:
         CASE mCustCat:
            WHEN "Ю" THEN
            DO:
               FIND FIRST cust-corp WHERE cust-corp.cust-id EQ mCustID NO-LOCK NO-ERROR.
               IF AVAIL(cust-corp) THEN mExistClient = YES.
            END.
            WHEN "Ч" THEN
            DO:
               FIND FIRST person WHERE person.person-id EQ mCustID NO-LOCK NO-ERROR.
               IF AVAIL(person) THEN mExistClient = YES.
            END.
            WHEN "Б" THEN
            DO:
               FIND FIRST banks WHERE banks.bank-id EQ mCustID NO-LOCK NO-ERROR.
               IF AVAIL(banks) THEN mExistClient = YES.
            END.
         END CASE.
         IF mExistClient EQ YES THEN
         DO:
            IF AVAIL(DataBlock) THEN
            FOR EACH DataCli OF DataBlock NO-LOCK:
               IF           GetEntries(1,DataCli.Txt,"~n","")  EQ mCustCat 
                  AND INT64(GetEntries(2,DataCli.Txt,"~n","")) EQ mCustID THEN
               DO TRANSACTION:
                  FILE-INFO:FILE-NAME = mFileName0.
                  CREATE DataReq.
                  ASSIGN
                     DataReq.Data-ID = mDataBlockID
                     DataReq.Sym1    = DataCli.Sym1
                     DataReq.Sym2    = {&REQESTEDINFO-LINE}
                     DataReq.Txt     = "04"                                                         + "~n" +
                                       ""                                                           + "~n" +
                                       "|1|" + FILE-INFO:FILE-NAME + "|2|"                          + "~n" +
                                       "|1|" + STRING(FILE-INFO:FILE-SIZE) + "|2|"                  + "~n" +
                                       "|1|" + STRING(FILE-INFO:FILE-MOD-DATE,"99/99/9999") + "|2|" + "~n" +
                                       "|1|" + STRING(FILE-INFO:FILE-MOD-TIME,"hh:mm:ss") + "|2|"   + "~n" +
                                       ""                                                           + "~n" 
                     DataReq.Sym3    = STRING(RECID(DataReq)).
               END.
            END.
         END.
      END.
      /*0230124*/
   END.
END. /* MAIN-BLOCK: */
/* {preview.i &filename = "mFileName0"}  */

RETURN.

FINALLY:
   RUN DeleteObject365p(mHCust365p).
   RUN End-SysMes.
   {intrface.del}
END FINALLY.

/* ***************************  Procedures  *************************** */
/* корректность даты
*/
FUNCTION ValiDateDate RETURN LOGICAL(ipDate AS CHARACTER):
   DEFINE VARIABLE vDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vRet AS LOGICAL  INIT YES   NO-UNDO.
   ASSIGN
      vDate = DATE(ipDate) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN Fill-SysMes IN h_tmess ( "", "", "1" ,"Введена неверная дата" ).
      vRet = NO.
   END.

   RETURN vRet.
END FUNCTION.

/* Объединение путей */

&SCOPED-DEFINE PathSep "/"

FUNCTION JoinPaths RETURN CHARACTER (INPUT iDir  AS CHARACTER,
                                     INPUT iFile AS CHARACTER):
   RETURN (IF {assigned iDir}
           THEN (iDir +
                 (IF SUBSTRING(iDir,
                               LENGTH(iDir),
                               1) = {&PathSep}
                  THEN ""
                  ELSE {&PathSep}) +
                 iFile)
           ELSE iFile).
END FUNCTION.

&UNDEFINE PathSep

/* Ввод параметров выписки */
PROCEDURE GetDate:
   ASSIGN
      mBIC = FGetSetting("БанкМФО","","")
      mDateVyp = gend-date
      .

   PAUSE 0.
   DISPLAY mDate-beg mDate-end mAccts mDateReq mNumberReq mBIC mDateVyp 
       WITH FRAME VYP-FRAME.

   ENABLE mDate-beg mDate-end mAccts mDateReq mNumberReq mBIC mDateVyp 
      WITH FRAME VYP-FRAME.   
   
   WAIT-FOR END-ERROR, GO OF FRAME VYP-FRAME /* FOCUS mDate-beg */.
   HIDE FRAME VYP-FRAME NO-PAUSE.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:16:17.694+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='legvyp407.p' */
/*prosignnVwA+kb1PSdG4rHrOa80xQ*/