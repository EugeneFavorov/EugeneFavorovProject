&GLOBAL-DEFINE NO-BASE-PROC YES
&GLOBAL-DEFINE CLASS-LNK    xattr-label
&GLOBAL-DEFINE FIELD-LNK    xattr-clabel


DEFINE VARIABLE mFlagSet       AS LOGICAL     NO-UNDO INIT ?.
DEFINE VARIABLE hExch          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hExchTmp       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hComm          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hInf           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hData          AS HANDLE      NO-UNDO.
DEFINE VARIABLE mSrcCP         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFmt           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mSep           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mPacketID      AS INT64       NO-UNDO.
DEFINE VARIABLE mFilter        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hFilter        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFiltQry       AS HANDLE      NO-UNDO.
DEFINE VARIABLE mKind          AS CHAR        NO-UNDO.
DEFINE VARIABLE mOpKindKind    AS CHAR        NO-UNDO.
DEFINE VARIABLE mSeanceID      AS INT64       NO-UNDO.
DEFINE VARIABLE mBuf           AS CHAR        NO-UNDO.
DEFINE VARIABLE m32            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mInfoType      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mCntDocFileMax AS INT64       NO-UNDO. /* кол-во документов в файле */
DEFINE VARIABLE mCntDocFile    AS INT64       NO-UNDO. /* текущий документ в файле */
DEFINE VARIABLE mCntFile       AS INT64       NO-UNDO. /* текущий файл */
DEFINE VARIABLE mFileLength    AS INT64       NO-UNDO. /* длина фрагмента */
DEFINE VARIABLE mFileLengthMax AS INT64       NO-UNDO. /* максимальная длина файла */
DEFINE VARIABLE mCntFileMax    AS INT64       NO-UNDO. /* максимальное число файлов */

mCntDocFileMax = INT64(FGetSetting("Настройка_365П","КолДок","2")). /* заменить на настройку ~ 26 документов*/
mFileLengthMax = INT64(FGetSetting("Настройка_365П","МаксДлинаФл","500")) * 1024. 
mCntFileMax    = INT64(FGetSetting("Настройка_365П","ЧислоФайлов","100")). 

mResult = "".

PROCEDURE TAXExpSprav:

DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

DEFINE VARIABLE vTmplID1   AS INT64       NO-UNDO.
DEFINE VARIABLE vOpKind1   AS CHAR        NO-UNDO.
DEFINE VARIABLE vFileNm    AS CHAR        NO-UNDO.
DEFINE VARIABLE vPreFile   AS CHAR        NO-UNDO.
DEFINE VARIABLE vNumCode   AS CHARACTER   NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   ASSIGN
      hExch       = hIns:DEFAULT-BUFFER-HANDLE                        
      mSrcCP      = TRNSettingValue("", "CodePage", "DOS")            
      mSep        = TRNSettingValue("", "TagSep", ":")                
      mFmt        = hExch::mail-format                                
      mPacketID   = hExch::PacketID                                   
      mKind       = hExch::Kind                                       
      mSeanceID   = INT64(hExch::SeanceID)                            
      mResult     = ""                                                
      m32         = NO
      mCntFile    = 0
      mCntDocFile = 0
      vTmplID1 = hExch:buffer-field("__tmpl-ID"):buffer-value
      vOpKind1 = GetBaseOpKind()
      vFileNm  = GetAttrValue(vOpKind1,vTmplID1,"FileName")
      vPreFile = GetAttrValue(vOpKind1,vTmplID1,"$PreF")
   NO-ERROR. {&ON-ERROR}

   ASSIGN
      hComm  = ObjectValueHandle(mRsSrv)
      hInf   = ObjectValueHandle(mRsInf)
      mInfoType  = hComm:BUFFER-FIELD(GetMangledName("ТипИнф")):BUFFER-VALUE
/*      hData  = ObjectValueHandle(mRsAcc)*/
   NO-ERROR. {&ON-ERROR}
/*
message 
"idf: " hComm:BUFFER-FIELD(GetMangledName("ИдФайл")):BUFFER-VALUE skip
"idd: " hInf:BUFFER-FIELD(GetMangledName("ИдДок")):BUFFER-VALUE skip
"acct: " hData:BUFFER-FIELD(GetMangledName("НомСч")):BUFFER-VALUE 
view-as alert-box.
*/

   DEFINE BUFFER Packet       FOR Packet.

   FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID
                       AND (IF   shMode
                           THEN Seance.Filial-ID EQ shFilial
                           ELSE YES)
      NO-LOCK NO-ERROR.
   FIND FIRST Mail-User WHERE Mail-User.op-kind-exp  EQ Seance.op-kind
                          AND Mail-User.mail-format  EQ mFmt
                          AND (IF   shMode
                               THEN Mail-User.Filial-ID EQ shFilial
                               ELSE YES)
   NO-LOCK NO-ERROR.

   {taxrsexp.cr}

   DEFINE VARIABLE vHBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE vHQuery  AS HANDLE NO-UNDO.
   DEFINE VARIABLE vHBufferDoc AS HANDLE NO-UNDO.
   DEFINE VARIABLE vHQueryDoc  AS HANDLE NO-UNDO.
   DEFINE VARIABLE vTmpAcct   AS CHARACTER NO-UNDO.

   vHBuffer = oHTable2:DEFAULT-BUFFER-HANDLE.
   vHBufferDoc = oHTable3:DEFAULT-BUFFER-HANDLE.

   CREATE QUERY vHQuery.
   vHQuery:SET-BUFFERS(vHBuffer).
   vHQuery:QUERY-PREPARE("FOR EACH " + vHBuffer:NAME + " NO-LOCK").
   vHQuery:QUERY-OPEN().

   CREATE QUERY vHQueryDoc.
   vHQueryDoc:SET-BUFFERS(vHBufferDoc).

   REPEAT:
       vHQuery:GET-NEXT().
       IF vHQuery:QUERY-OFF-END THEN
           LEAVE.

     {taxrsexp.acc}

     vHQueryDoc:QUERY-PREPARE("FOR EACH " + vHBufferDoc:NAME + " WHERE " +
                               vHBufferDoc:NAME + ".UpID EQ " + 
                               vHBuffer::ID + " NO-LOCK").
     vHQueryDoc:QUERY-OPEN().

     REPEAT:
         vHQueryDoc:GET-NEXT().
         IF vHQueryDoc:QUERY-OFF-END THEN
             LEAVE.

         mCntDocFile = mCntDocFile + 1. /* подсчитываем кол-во документов */

         &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("taxrsexp.i", "mCntDocFile:"  + STRING(mCntDocFile)).
         RUN dbgprint.p ("taxrsexp.i", "mCntDocFileMax:"  + STRING(mCntDocFileMax)).
         RUN dbgprint.p ("taxrsexp.i", "LEN0 mResult:"  + STRING(LENGTH(mResult))).
         RUN dbgprint.p ("taxrsexp.i", "LEN1 mFileLengthMax:"  + STRING(mFileLengthMax)).
         &ENDIF


         IF mCntDocFile     > mCntDocFileMax  OR
            LENGTH(mResult) > mFileLengthMax  THEN DO:
            /* если достигнуто кол-во документов из настройки или превышена длина файла */
           mCntDocFile = 1.

           /*RUN AddTxt32 ("###~n").*/

           RUN AddTxt32 ("@@@~n===~n").
           RUN PacketTextSave (mPacketID, mResult).

           &IF DEFINED (IS-DEBUG) &THEN
           RUN dbgprint.p ("taxrsexp.i", "LENGTH FILE:"  + STRING(LENGTH(mResult))).
           &ENDIF

           {taxrsexp.cr}
           {taxrsexp.acc}
         END.
         FOR EACH Xattr WHERE Xattr.Class-Code EQ mRsDoc NO-LOCK BY Xattr.order:
            IF Xattr.order NE 0 THEN DO:

               mBuf = vHBufferDoc:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE NO-ERROR.
               IF Xattr.Data-Type EQ "DECIMAL" THEN
                  mBuf = TRIM(GetNullStr(STRING(DEC(mBuf),Xattr.Data-Format))).

               IF mBuf EQ ? THEN DO:
            &IF DEFINED (IS-DEBUG) &THEN
               RUN dbgprint.p ("*** ERROR taxrsexp.i", " Xattr.Xattr-code:"  + GetNullStr(Xattr.Xattr-code) +
                                               " value: " + GetNullStr(mBuf)).
            &ENDIF
                  mBuf = "".
               END.

               IF xattr.xattr-label EQ "П"
                  AND NOT {assigned mBuf} THEN DO:
                  mBuf = Xattr.Xattr-code +  mSep + "~n".
               END.
               ELSE IF Xattr.Mandatory OR {assigned mBuf} THEN
                  mBuf = SUBSTITUTE("&1&2&3~n",
                                    xattr.xattr-code,
                                    mSep,
                                    fixTagValue365p(mBuf)).
               RUN AddTxt32 (mBuf).
            &IF DEFINED (IS-DEBUG) &THEN
               RUN dbgprint.p ("XattrTAXRS-DOC", " Xattr.Xattr-code:"  + GetNullStr(Xattr.Xattr-code) +
                                               " Xattr.Description:" + GetNullStr(Xattr.Description) +
                                               " value: " + GetNullStr(fixTagValue365p(mBuf))).
            &ENDIF
            END.
         END.
         RUN AddTxt32 ("###~n").
     END.
     vHQueryDoc:QUERY-CLOSE().

   END.
   vHQuery:QUERY-CLOSE().

   RUN AddTxt32 ("@@@~n===~n").

   RUN PacketTextSave (mPacketID, mResult).

   RUN PacketCreateRef(TODAY,
                       mPacketID,
                       "RTaxFileRS",
                        ReferenceFormatValue("RTaxFileRS",
                                             STRING(INT64(ReferenceGetLast("RTaxFileRS",TODAY)) + 1))).

/*
   RUN InstanceDelete(oHTable2    ).
   RUN InstanceDelete(oHTable3    ).
   RUN InstanceDelete(vHQueryDoc  ).
   RUN InstanceDelete(vHQuery     ).
   RUN InstanceDelete(vHBufferDoc ).
   RUN InstanceDelete(vHBuffer    ).
   RUN InstanceDelete(hIns        ).
   RUN InstanceDelete(hExch       ).

   DELETE OBJECT oHTable2    NO-ERROR.
   DELETE OBJECT oHTable3    NO-ERROR.
   DELETE OBJECT vHQueryDoc  NO-ERROR.
   DELETE OBJECT vHQuery     NO-ERROR.
   DELETE OBJECT HQ          NO-ERROR.
   DELETE OBJECT HB          NO-ERROR.
   DELETE OBJECT vHBufferDoc NO-ERROR.
   DELETE OBJECT vHBuffer    NO-ERROR.
   DELETE OBJECT hIns        NO-ERROR.
   DELETE OBJECT hExch       NO-ERROR.
*/

   mFlagSet = YES.
END.

{doreturn.i mFlagSet}

END PROCEDURE.


/*----------------------------------------------------------------------------*/
/*          Прикладная часть                                                  */
/*----------------------------------------------------------------------------*/

PROCEDURE AddTxt32:
   DEF INPUT PARAM iBuf AS CHAR NO-UNDO.

   RUN PacketTextKeep IN h_pack (mPacketID, 
                                 iBuf,
                                 INPUT-OUTPUT mResult).

END PROCEDURE.

/* Процедуры создания ТФ ответов */

PROCEDURE CreateBOSExch_RPO.

   DEFINE INPUT  PARAMETER hDataExch AS HANDLE      NO-UNDO.      /* ETAXResolutn */

   DEFINE VARIABLE hAnsExchS     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hAnsExchI     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vGuid         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE oHTable       AS HANDLE      NO-UNDO. /* входящие счета*/
   DEFINE VARIABLE vUserPosition AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUserName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumDocs      AS INT64       NO-UNDO.
   DEFINE VARIABLE vDateTmp      AS DATE        NO-UNDO.
   DEFINE VARIABLE vCheckCorpName AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vHCust        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHAcct        AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet      AS LOGICAL     NO-UNDO     INIT ?.

   DEFINE BUFFER setting FOR setting.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   FIND FIRST setting WHERE
              setting.code      EQ "Настройка_365П" AND
              setting.sub-code  EQ "Тр_ЗНО2" AND
              setting.filial-id EQ shFilial
   NO-LOCK NO-ERROR.

   IF NOT AVAIL Setting THEN
   FIND FIRST setting WHERE
              setting.code      EQ "Настройка_365П" AND
              setting.sub-code  EQ "Тр_ЗНО2"
   NO-LOCK NO-ERROR.

   IF AVAIL setting THEN
   ASSIGN
      mRsSrv = GetEntries(1,setting.details,",","ETAXRSCommon")
      mRsInf = GetEntries(2,setting.details,",","ETAXRsInfAvAc")
      mRsAcc = GetEntries(3,setting.details,",","ETAXRsDataAvAc")
      mRsDoc = GetEntries(4,setting.details,",","ETAXRsDataDoc")
   .

   ASSIGN
      hAnsExchS  = ObjectValueHandle (mRsSrv)
      hAnsExchI  = ObjectValueHandle (mRsInf)
      hAnsExchS:BUFFER-FIELD(GetMangledName("__id")):BUFFER-VALUE = "1"
      hAnsExchI:BUFFER-FIELD(GetMangledName("__id")):BUFFER-VALUE = "1"
   . {&ON-ERROR}

   RUN CreateTransportTable365p (mRsAcc, OUTPUT  oHTable2) NO-ERROR. {&ON-ERROR}
   RUN CreateTransportTable365p (mRsDoc, OUTPUT  oHTable3) NO-ERROR. {&ON-ERROR}

   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", 
                                   "YES").
   RUN GenGUID(OUTPUT vGuid).

   RUN FillZNOServicePart365p(hAnsExchS,
                              hDataExch,
                              vGUID,
                              1,
                              1)
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       RETURN ERROR RETURN-VALUE.

   RUN CreateTransportTable365p (ENTRY(2,mReqTrans), OUTPUT  oHTable) NO-ERROR. {&ON-ERROR}

   FOR EACH ttBlockAcct WHERE NOT {assigned ttBlockAcct.error-code} NO-LOCK:
      oHTable:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().

      {365p.set &obj = "oHTable:DEFAULT-BUFFER-HANDLE"
                &fld = "НомСч"
                &val = "ttBlockAcct.acct"}
   END.

   run ticlaxr2.p (output vUserName,
                   output vUserPosition
                  ).

   vCheckCorpName = (TRNSettingValue("ExchSET-TAX", "Check33", "Наим") <> "ИНН").
   RUN GetCust365p(hDataExch,
                   oHTable,
                   vCheckCorpName,
                   OUTPUT vHCust)
   NO-ERROR.
   RUN dbgprint.p ("CreateBOSExch_RPO","~n" +
                   "vCheckCorpName: " + GetNullStr(STRING(vCheckCorpName)) + "~n" +
                   "ERROR-STATUS:ERROR: " + GetNullStr(STRING(ERROR-STATUS:ERROR)) + "~n" +
                   "RETURN-VALUE: " + GetNullStr(RETURN-VALUE)
                   ).
   IF ERROR-STATUS:ERROR THEN
       RETURN ERROR RETURN-VALUE.

   RUN FillZNOInfoPart365p(hAnsExchI,
                           hDataExch,
                           vHCust,
                           vGUID,
                           vUserName,
                           vUserPosition)
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       RETURN ERROR RETURN-VALUE.

   RUN GetAcct365p(hDataExch,
                   oHTable,
                   vHCust,
                   vCheckCorpName,
                   OUTPUT vHAcct)
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       RETURN ERROR RETURN-VALUE.

   {365p.set &obj = "hDataExch"
             &fld = "ДатаРешПр"
             &val = "gend-date"}

   RUN dbgprint.p ("CreateBOSExch_RPO before RUN FillZNOAcctPart365p","~n" + 
                   "" + GetNullStr(STRING(""))
                   ).

   RUN FillZNOAcctPart365p(oHTable2,
                           oHTable3,
                           hDataExch,
                           vHAcct,
                           OUTPUT vNumDocs)
   NO-ERROR.
   
   RUN dbgprint.p ("CreateBOSExch_RPO after RUN FillZNOAcctPart365p","~n" + 
                   "vNumDocs" + GetNullStr(STRING(vNumDocs))
                   ).
   
   IF ERROR-STATUS:ERROR THEN
       RETURN ERROR RETURN-VALUE.

   {365p.get &obj = "hAnsExchI"
             &fld = "ДатаСправ"
             &val = "vDateTmp"}
   {365p.set &obj = "hAnsExchI"
             &fld = "ДатаСправ"
             &val = "date2str(gend-date)"}
   {365p.set &obj = "hDataExch"
             &fld = "ДатаРешПр"
             &val = "date2str(vDateTmp)"}
             
   vFlagSet = YES.

END. /* MAIN */

{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE CreateBExch_ZNO1.
   DEFINE INPUT  PARAMETER hDataExch AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER mReqType  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oHTable   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   RUN CreateTransportTable365p (ENTRY(2,mReqTrans), OUTPUT  oHTable) NO-ERROR. {&ON-ERROR}

   for each ttBlockAcct:
      oHTable:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().
      {365p.set &obj = "oHTable:DEFAULT-BUFFER-HANDLE"
                &fld = "НомСч"
                &val = "ttBlockAcct.acct"}
   end.

   vFlagSet = YES.
END.
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE CreateBExch_ZNO2.
   DEFINE INPUT        PARAMETER hDataExch  AS HANDLE    NO-UNDO.
   DEFINE INPUT        PARAMETER iHCust365p AS HANDLE    NO-UNDO.
   DEFINE INPUT        PARAMETER oHTable    AS HANDLE    NO-UNDO.
   DEFINE INPUT        PARAMETER mReqType   AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER vPacketID  AS INT64     NO-UNDO.
   DEFINE OUTPUT       PARAMETER oCountAll  AS INT64     NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioErrCode  AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioErrText  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE hAnsExchS     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hAnsExchI     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vGuid         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUserPosition AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUserName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumDocs      AS INT64       NO-UNDO.
   DEFINE VARIABLE vExternalCust AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vCheckCorpName AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vHGoodAcct    AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHBadAcct     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vAcctErrorCode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcctErrorText AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.
   
   DEFINE BUFFER setting FOR setting.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

      FIND FIRST setting WHERE
                 setting.code      EQ "Настройка_365П" AND
                 setting.sub-code  EQ "Тр_ЗНО" + mReqType AND
                 setting.filial-id EQ shFilial
      NO-LOCK NO-ERROR.
      IF NOT AVAIL Setting THEN
      FIND FIRST setting WHERE
                 setting.code      EQ "Настройка_365П" AND
                 setting.sub-code  EQ "Тр_ЗНО" + mReqType 
      NO-LOCK NO-ERROR.

      IF AVAIL setting THEN
      ASSIGN
         mRsSrv = GetEntries(1,setting.details,",","ETAXRSCommon")
         mRsInf = GetEntries(2,setting.details,",","ETAXRsInfAvAc")
         mRsAcc = GetEntries(3,setting.details,",","ETAXRsDataAvAc")
         mRsDoc = GetEntries(4,setting.details,",","ETAXRsDataDoc")
      .

      ASSIGN
         hAnsExchS  = ObjectValueHandle (mRsSrv)
         hAnsExchI  = ObjectValueHandle (mRsInf)
         hAnsExchS:BUFFER-FIELD(GetMangledName("__id")):BUFFER-VALUE = "1"
         hAnsExchI:BUFFER-FIELD(GetMangledName("__id")):BUFFER-VALUE = "1"
      . {&ON-ERROR}

      RUN CreateTransportTable365p(mRsAcc,
                                   OUTPUT oHTable2)
      NO-ERROR.
      {&ON-ERROR}
      RUN CreateTransportTable365p(mRsDoc,
                                   OUTPUT oHTable3)
      NO-ERROR.
      {&ON-ERROR}

      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", 
                                      mReqType).
      vCheckCorpName = (TRNSettingValue("ExchSET-TAX", "Check33", "Наим")
                        <>
                        "ИНН").
      RUN SetMFMode365p(hDataExch).
      IF VALID-HANDLE(iHCust365p) THEN
         vExternalCust = YES.
      ELSE DO:
         RUN GetCust365p(hDataExch,
                         oHTable,
                         vCheckCorpName,
                         OUTPUT iHCust365p)
         NO-ERROR.
         {&ON-ERROR}
      END.
      
      RUN dbgprint.p ("CreateBExch_ZNO2 before RUN GetAcctSplit365p","~n" + 
                      ":" + GetNullStr(STRING(""))
                      ).
      
      RUN GetAcctSplit365p(hDataExch,
                           oHTable,
                           iHCust365p,
                           vCheckCorpName,
                           OUTPUT vHGoodAcct,
                           OUTPUT vHBadAcct)
      NO-ERROR.
      
      RUN dbgprint.p ("CreateBExch_ZNO2 after RUN GetAcctSplit365p","~n" + 
                      ":" + GetNullStr(STRING(""))
                      ).

      {&ON-ERROR}
      IF NOT vHGoodAcct:HAS-RECORDS THEN DO:
          RUN GetAcctError365p(hDataExch,
                               vHBadAcct,
                               vCheckCorpName,
                               OUTPUT ioErrCode,
                               OUTPUT ioErrText)
          NO-ERROR.
          vFlagSet = YES.
          RETURN.
      END.
      RUN GenGUID(OUTPUT vGuid).
      RUN ticlaxr2.p (OUTPUT vUserName, OUTPUT vUserPosition).
      RUN FillZNOServicePart365p(hAnsExchS,
                                 hDataExch,
                                 vGUID,
                                 1,
                                 1)
      NO-ERROR.
      {&ON-ERROR}
      RUN FillZNOInfoPart365p(hAnsExchI,
                              hDataExch,
                              iHCust365p,
                              vGUID,
                              vUserName,
                              vUserPosition)
      NO-ERROR.
      {&ON-ERROR}

      RUN dbgprint.p ("CreateBExch_ZNO2 before RUN FillZNOAcctPart365p","~n" + 
                      ":" + GetNullStr(STRING(""))
                     ).
      RUN FillZNOAcctPart365p(oHTable2,
                              oHTable3,
                              hDataExch,
                              vHGoodAcct,
                              OUTPUT vNumDocs)
      NO-ERROR.
      {&ON-ERROR}
      
      RUN dbgprint.p ("CreateBExch_ZNO2 after RUN FillZNOAcctPart365p","~n" + 
                      "vNumDocs:" + GetNullStr(STRING(vNumDocs))
                     ).
      oCountAll = vNumDocs.

      &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("taxrsexp.i", " mReqType:"  + STRING(mReqType)).
      RUN dbgprint.p ("taxrsexp.i", " oCountAll:"  + STRING(oCountAll)).
      &ENDIF

      RUN CheckZNOOpLimit365p(INT64(mReqType), vNumDocs) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
          ASSIGN
              ioErrCode = {&ERR-OTHER}
              ioErrText = RETURN-VALUE
          .
          RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", "").
      END.
      ELSE IF vPacketID <> ? THEN 
      DO:
          IF vHBadAcct:HAS-RECORDS THEN DO:
              RUN GetAcctError365p(hDataExch,
                                   vHBadAcct,
                                   vCheckCorpName,
                                   OUTPUT vAcctErrorCode,
                                   OUTPUT vAcctErrorText)
              NO-ERROR.
              RUN Add2ZNOLog365p(mFile1,
                                 mReqType,
                                 hDataExch,
                                 vAcctErrorCode,
                                 vAcctErrorText).
              RUN TaxConfirmCreate(vPacketID,
                                   vAcctErrorCode,
                                   vAcctErrorText,
                                   NOW)
              NO-ERROR.
              {&ON-ERROR}
          END.
          RUN LinkCust365p(iHCust365p, vPacketID, "TTAXConf") NO-ERROR. {&ON-ERROR}
          IF NOT vExternalCust THEN
             RUN DeleteObject365p(iHCust365p).
      END.

   vFlagSet = YES.
END.  /* MAIN */

{doreturn.i vFlagSet}
END PROCEDURE.
/* $LINTFILE='taxrsexp.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='mike' */
/* $LINTDATE='04/10/2016 09:34:59.690+03:00' */
/*prosignVek88X4omDKlZp7HowC9DQ*/