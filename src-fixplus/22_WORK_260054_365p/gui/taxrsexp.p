{globals.i}

/* +++ taxrsexp.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:13am +++ */

/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: taxrsexp.p
      Comment: Экспорт остатков по счетам в МНС
   Parameters: INPUT PARAMETER iClass, hIns
      Created: 27.10.2011 11:17 Mike
     Modified: <date> <who>
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get tmess}
{intrface.get exch} 
{intrface.get xclass}
{intrface.get strng}
{intrface.get filex} 
{intrface.get trans}
{intrface.get strng}
{intrface.get pack}
{intrface.get rfrnc}
{intrface.get cust}
{intrface.get separate}

{form.def}
{g-trans.equ}
{exchange.equ}
{sh-defs.i}
{365p.def}

&GLOBAL-DEFINE NO-BASE-PROC YES
&GLOBAL-DEFINE CLASS-LNK    xattr-label
&GLOBAL-DEFINE FIELD-LNK    xattr-clabel

DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

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
DEFINE VARIABLE mReqPackID     AS INT64       NO-UNDO.
DEFINE VARIABLE mFilter        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hFilter        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFiltQry       AS HANDLE      NO-UNDO.
DEFINE VARIABLE mKind          AS CHAR        NO-UNDO.
DEFINE VARIABLE mOpKindKind    AS CHAR        NO-UNDO.
DEFINE VARIABLE mSeanceID      AS INT64       NO-UNDO.
DEFINE VARIABLE mResult        AS CHAR        NO-UNDO.
DEFINE VARIABLE mBuf           AS CHAR        NO-UNDO.
DEFINE VARIABLE mFlagFirst     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vCustID        AS INT64       NO-UNDO.
DEFINE VARIABLE mDateSp        AS DATE        NO-UNDO.

DEFINE VARIABLE mVBO_365       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMnozhC_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIgnorKPP_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAllFil_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrDP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrOP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKolDoc_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTipeOst_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProvOvrd_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mZamRVSO_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParSchStN     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mDate_NR       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorrACCT      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFO           AS CHARACTER NO-UNDO.

ASSIGN
   mMFO          = FGetSetting("БанкМФО", "", "")
   mVBO_365      = FGetSetting("Настройка_365П","ВычБлокОст","Нет")  
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
NO-ERROR.

{core365p.pro}
   
FUNCTION FioIp CHAR (INPUT iResult AS CHAR):
   DEF VAR vI AS INT64 NO-UNDO.

   iResult = TRIM(iResult).            /* всего должно быть две запятых    */ 
   IF {assigned iResult} THEN DO:

      iResult = REPLACE(iResult,","," ").
      vI = INDEX(iResult," ").         /* первый пробел заменим на запятую */      
      IF vI NE 0 AND vI NE ? THEN OVERLAY(iResult,vI) = ",".
      vI = INDEX(iResult," ").         /* второй пробел заменим на запятую */
      IF vI NE 0 AND vI NE ? THEN OVERLAY(iResult,vI) = ",".

      IF NUM-ENTRIES(iResult) < 3 THEN
         iResult = iResult + FILL( ",", 3 - NUM-ENTRIES(iResult)).
   END.
   RETURN iResult.

END FUNCTION.


MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
   ASSIGN
      hExch       = hIns:DEFAULT-BUFFER-HANDLE                        
      mSrcCP      = TRNSettingValue("", "CodePage", "DOS")            
      mSep        = TRNSettingValue("", "TagSep", ":")                
      mDateSp     = DATE(TRNSettingValue("", "DateSp", STRING(TODAY,"99/99/9999")))
      mFmt        = hExch::mail-format                                
      mPacketID   = hExch::PacketID                                   
      mKind       = hExch::Kind                                       
      mSeanceID   = INT64(hExch::SeanceID)                            
      mResult     = ""                                                
      mFilter     = TRNSettingValue("","FilterTable",{&RET-ERROR})
      hFilter     = WIDGET-HANDLE(mFilter)
      hFilter     = hFilter:DEFAULT-BUFFER-HANDLE
      NO-ERROR. {&ON-ERROR}

   ASSIGN
      hComm  = ObjectValueHandle("ETAXRSCommon")
      hInf   = ObjectValueHandle("ETAXRSInf")
      hData  = ObjectValueHandle("ETAXRSData")
   NO-ERROR. {&ON-ERROR}

   CREATE WIDGET-POOL "EXPS-TAXRS".

   CREATE QUERY hFiltQry IN WIDGET-POOL "EXPS-TAXRS".
   hFiltQry:ADD-BUFFER(hFilter).
   hFiltQry:QUERY-PREPARE("for EACH " + hFilter:Name).
   hFiltQry:QUERY-OPEN().
   mFlagSet = hFiltQry:GET-FIRST(NO-LOCK).

   DEFINE BUFFER Packet       FOR Packet.

   FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID
                       AND ( IF   shMode
                           THEN Seance.Filial-ID EQ shFilial
                           ELSE YES)
      NO-LOCK NO-ERROR.
   FIND FIRST Mail-User WHERE Mail-User.op-kind-exp  EQ Seance.op-kind
                          AND ( IF   shMode
                               THEN Mail-User.Filial-ID EQ shFilial
                               ELSE YES)
   NO-LOCK NO-ERROR.

   hFiltQry:GET-FIRST(NO-LOCK).
   FIND FIRST acct WHERE
              acct.acct EQ hFilter::acct
         AND  acct.acct-cat NE "В"
              NO-LOCK NO-ERROR.
   IF AVAIL acct THEN vCustID = acct.cust-id.

   DO WHILE NOT hFiltQry:QUERY-OFF-END:
      FOR FIRST acct WHERE
                acct.acct EQ hFilter::acct
           AND  acct.acct-cat NE "В"
                NO-LOCK:

            IF acct.cust-id NE vCustID THEN DO:
               RUN Fill-SysMes("","","","Счета должны быть одного клиента !").
            mFlagFirst = NO.
            UNDO MAIN, RETRY MAIN.
         END.
      END.
      hFiltQry:GET-NEXT(NO-LOCK).
   END.

   mFlagSet = hFiltQry:GET-FIRST(NO-LOCK).

   RUN PacketCreate     (INPUT  Seance.SeanceID,
                         INPUT  0,
                         INPUT  Mail-User.Mail-User-num,
                         INPUT  mKind,
                         OUTPUT mPacketID) NO-ERROR.

   IF NOT  UpdateSigns("Packet",
                      STRING(mPacketID),
                      "FileName",
                      hExch:buffer-field("FileName"):buffer-value, 
                      ?)  THEN
      UNDO MAIN, RETRY MAIN.

   mReqPackID = INT64(TRNSettingValue("","ReqPack","0")) NO-ERROR.
   IF mReqPackID GT 0 THEN DO:   /* Создание связи с исходным сообщением */
      RUN PacketCreateLink(mPacketID,  "Packet", STRING(mReqPackID), "TTAXConf").
      RUN PacketCreateLink(mReqPackID, "Packet", STRING(mPacketID),  "TTAXConf").
   END.

   RUN PacketTextClear(mPacketID).

   RUN ETAXRSExportCommon  (hComm).

   mFlagFirst = NO.

   qry:
   DO WHILE NOT hFiltQry:QUERY-OFF-END:

      FOR FIRST acct WHERE
                acct.acct EQ hFilter::acct
           AND  acct.acct-cat NE "В"
                NO-LOCK:

         IF NOT mFlagFirst THEN
            RUN ETAXRSExportInf     (hInf). 

         RUN ETAXRSExportDat     (hData). 
         mFlagFirst = YES.

      END.
      hFiltQry:GET-NEXT(NO-LOCK).
   END.
   &IF DEFINED (IS-DEBUG) &THEN
      RUN DumpObject   (hComm).
      RUN DumpTRNObject(hInf). 
   &ENDIF

   RUN XattrTAXRS(hExchTmp,mSep,mFmt).
   RUN PacketTextSave (mPacketID, mResult).
   RUN PacketCreateRef(TODAY,
                       mPacketID,
                       "RTaxFileRS",
                        ReferenceFormatValue("RTaxFileRS",
                                             STRING(INT64(ReferenceGetLast("RTaxFileRS",TODAY)) + 1))).

   RUN InstanceDelete(hComm).
   RUN InstanceDelete(hInf).
   mFlagSet = YES.
END.

{intrface.del}          /* Выгрузка инструментария. */ 
{doreturn.i mFlagSet}

/*----------------------------------------------------------------------------*/
PROCEDURE XattrTAXRS.                
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER iSep    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER ioFmt   AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vTag       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVal       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vETurn     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hETrn      AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hQ         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hB         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFlagSet   AS LOGICAL     NO-UNDO  INIT ?.
   DEFINE VARIABLE counter    AS INT64       NO-UNDO.

   DEFINE BUFFER Xattr  FOR Xattr.
   DEFINE BUFFER bXattr FOR Xattr.
   DEFINE BUFFER bCode  FOR code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   FOR EACH Xattr WHERE Xattr.Class-Code EQ ioFmt NO-LOCK BY Xattr.order:
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("XattrTAXRS", "ioFmt:" + GetNullStr(ioFmt) +
                                         " Xattr.Xattr-code:"  + GetNullStr(Xattr.Xattr-code) +
                                         " Xattr.Description:" + GetNullStr(Xattr.Description)).
      &ENDIF
      IF Xattr.Description EQ "OBJECT" OR
         Xattr.Description EQ "SPOOL" THEN DO:
         ioFmt = Xattr.{&CLASS-LNK}.
         IF NOT Exch-MsgBuff (ioFmt, BUFFER bCode) THEN
            UNDO MAIN, RETRY MAIN.

         vETurn = bCode.misc[{&RKC-KIND}].
         IF {assigned vETurn} THEN DO:
             hETrn  = ObjectValueHandle(vETurn).
             CREATE BUFFER hB FOR TABLE hETrn:TABLE-HANDLE.
             CREATE QUERY hQ.
             hQ:ADD-BUFFER(hB).
             hQ:QUERY-PREPARE("FOR EACH " + hETrn:NAME + " WHERE " +
                               hETrn:NAME + ".__ID GT 0 NO-LOCK ").
             hQ:QUERY-OPEN().
             hQ:GET-FIRST().
             DO WHILE NOT hQ:QUERY-OFF-END :
                FOR EACH bXattr WHERE
                         bXattr.Class-Code EQ vETurn
                     AND bXattr.order NE 0
                     NO-LOCK BY bXattr.order:
                   RUN TAG_TAXRSPutValue(hB,BUFFER bXattr,iSep).
                END.
                hQ:GET-NEXT().
             END.
             hQ:QUERY-CLOSE().
             IF VALID-HANDLE(hQ) THEN
                DELETE OBJECT hQ.
             IF VALID-HANDLE(hB) THEN
                DELETE OBJECT hB.
         END.
         RUN XattrTAXRS (hExch,iSep,ioFmt). 
      END.

      IF Xattr.Description EQ "ATTR" THEN /* Установка значения */
         RUN TAG_TAXRSPutValue(hExch,BUFFER Xattr,iSep).

      IF Xattr.Description EQ "STATIC" THEN DO:
         mBuf = GetNullStr(IF (Xattr.Xattr-code  EQ     "footer__"  OR
                               Xattr.Xattr-code  EQ     "header__")
                               THEN Xattr.Initial
                               ELSE Xattr.Xattr-code + iSep + Xattr.Initial)  + "~n".
         RUN PacketTextKeep (INPUT        mPacketID,
                             INPUT        mBuf,
                             INPUT-OUTPUT mResult).
         /* Возврат к предыдущему уровню */


         IF Xattr.Xattr-code EQ "footer__" THEN DO:
         &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p ("XattrTAXRS", "Xattr.Class-Code:"   + GetNullStr(Xattr.Class-Code) +
                                          " Xattr.Xattr-code:"  + GetNullStr(Xattr.Xattr-code)).
         &ENDIF
            vFlagSet = YES.
            LEAVE MAIN.
         END.
      END.
   END.
   vFlagSet = YES.
END. /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.


PROCEDURE TAG_TAXRSPutValue.
   DEFINE INPUT  PARAMETER hExch AS HANDLE      NO-UNDO.
   DEFINE PARAMETER BUFFER Xattr FOR Xattr.
   DEFINE INPUT PARAMETER iSep    AS CHARACTER   NO-UNDO.

   DEF VAR vValue AS CHAR NO-UNDO.

   IF NOT VALID-HANDLE(hExch)
      THEN hExch = ObjectValueHandle(Xattr.{&CLASS-LNK}).
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("TAG_TAXRSGetValue","hExch:" + GetNullStr(hExch:NAME) + 
                      " FIELD:" + GetNullStr(Xattr.Xattr-Code)).
   &ENDIF

   vValue = hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE.
   IF xattr.xattr-label EQ "П"
      AND NOT {assigned vValue} THEN.
   ELSE DO:

      mBuf = "".
      CASE Xattr.Data-Type:
         WHEN "CHARACTER" THEN DO:
            mBuf = IF Xattr.Xattr-code  BEGINS "Конец"
                   THEN 
                   (GetNullStr(STRING(hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE,
                   Xattr.Data-Format)) + "~n")
                   ELSE (Xattr.Xattr-code +  iSep +
                    TRIM(GetNullStr(STRING(hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE,
                   Xattr.Data-Format))) + "~n").
         END.
         
         WHEN "INTEGER" THEN
            mBuf = Xattr.Xattr-code +  iSep +
                   TRIM(GetNullStr(STRING(INT64(hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE),
                                   Xattr.Data-Format))) + "~n".
         WHEN "DECIMAL" THEN
            mBuf =  Xattr.Xattr-code +  iSep +
                    TRIM(GetNullStr(STRING(DEC(hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE),
                                    Xattr.Data-Format))) + "~n".
         WHEN "DATE" THEN
            mBuf = Xattr.Xattr-code +  iSep +
                   GetNullStr(STRING(DATE(hExch:BUFFER-FIELD(GetMangledName(Xattr.Xattr-Code)):BUFFER-VALUE),"99.99.9999")) + "~n".
      END CASE.
      IF GetEntries(2, TRIM(mBuf), iSep, "") NE "" OR 
         TRIM(mBuf) EQ "###" THEN
      RUN PacketTextKeep (INPUT        mPacketID,
                             INPUT        mBuf,
                             INPUT-OUTPUT mResult).
                             
   END.
END PROCEDURE.

PROCEDURE DumpTrnObject.
   DEFINE INPUT  PARAMETER hExch AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hQ AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hB AS HANDLE      NO-UNDO.

   CREATE BUFFER hB FOR TABLE hExch:TABLE-HANDLE.
   CREATE QUERY hQ.
   hQ:ADD-BUFFER(hB).
   hQ:QUERY-PREPARE("FOR EACH " + hExch:NAME + " WHERE " + hExch:NAME + ".__ID GT 0 NO-LOCK ").
   hQ:QUERY-OPEN().
   hQ:GET-FIRST().
   DO WHILE NOT hQ:QUERY-OFF-END :
      RUN DumpObject(hExch).
      hQ:GET-NEXT().
   END.
   hQ:QUERY-CLOSE().
   IF VALID-HANDLE(hQ) THEN
      DELETE OBJECT hQ.
   IF VALID-HANDLE(hB) THEN
      DELETE OBJECT hB.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*          Прикладная часть                                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE ETAXRSExportCommon.
   DEFINE INPUT  PARAMETER hComm AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vIdFile AS CHARACTER NO-UNDO.
   RUN InstanceCreate (?, hComm).
   vIdFile   =   hComm:BUFFER-FIELD(GetMangledName("ИдФайл"   )):BUFFER-VALUE.

   IF  NOT {assigned  vIdFile }
   THEN   
   hComm:BUFFER-FIELD(GetMangledName("ИдФайл"   )):BUFFER-VALUE =
         SUBSTRING(FGetSetting("ИНН",?,""),1,10) + 
         "**" + FGetSetting("БанкКПП",?,"")      +
         STRING(YEAR(TODAY),"9999")                     + 
         STRING(MONTH(TODAY),"99")                    +
         STRING(DAY(TODAY),"99")                      +
         SUBSTRING(STRING(TIME,"hh:mm:ss"),1,2)  +     
         SUBSTRING(STRING(TIME,"hh:mm:ss"),4,2)  +     
         SUBSTRING(STRING(TIME,"hh:mm:ss"),7,2)  +
         STRING(INT64(ReferenceGetLast("RTaxFileRS",TODAY)) + 1,"999999")
         NO-ERROR. 


END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ETAXRSExportInf.
   DEFINE INPUT  PARAMETER hInf AS HANDLE      NO-UNDO.

   DEFINE VAR mCustName    AS CHARACTER   NO-UNDO.
   DEFINE VAR mINN         AS CHARACTER   NO-UNDO.
   DEFINE VAR mKPP         AS CHARACTER   NO-UNDO.
   DEFINE VAR mAddress     AS CHARACTER   NO-UNDO.
   DEFINE VAR mType        AS CHARACTER   NO-UNDO.
   DEFINE VAR mCode        AS CHARACTER   NO-UNDO.
   DEFINE VAR mAcct        AS CHARACTER   NO-UNDO.
   DEFINE VAR mText        AS CHARACTER   NO-UNDO.

   RUN InstanceCreate (?, hInf).

   mCustName = GetCliName(                acct.cust-cat,
                                          STRING(acct.cust-id),
                          OUTPUT          mAddress,
                          OUTPUT          mINN,
                          OUTPUT          mKPP,
                          INPUT-OUTPUT    mType,
                          OUTPUT          mCode,
                          OUTPUT          mAcct).

   IF acct.cust-cat EQ "Ю" AND LENGTH(mINN) EQ 12
      THEN  mCustName = SUBSTRING(mCustName,INDEX(mCustName," ") + 1).

   hInf:BUFFER-FIELD(GetMangledName("НаимНП")):BUFFER-VALUE =
        IF acct.cust-cat EQ "Ю" AND LENGTH(mINN) NE 12
           THEN  mCustName ELSE "" NO-ERROR.   
   hInf:BUFFER-FIELD(GetMangledName("ФИОИП" )):BUFFER-VALUE =
        IF acct.cust-cat EQ "Ч" OR LENGTH(mINN) EQ 12
           THEN FioIp(mCustName) ELSE "" NO-ERROR.   

   IF mKPP EQ "0"         THEN  mKPP = "".
   IF LENGTH(mINN) EQ 12  THEN  mKPP = "".
   ELSE
   IF (LENGTH(mINN) EQ 5   OR
       LENGTH(mINN) EQ 10) AND
       NOT {assigned mKPP}
      THEN
      ASSIGN
        mINN = ""
        mKPP = "".

   hInf:BUFFER-FIELD(GetMangledName("ИНННП" )):BUFFER-VALUE =  mINN NO-ERROR.
   hInf:BUFFER-FIELD(GetMangledName("КППНП" )):BUFFER-VALUE =  mKPP NO-ERROR.


END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ETAXRSExportDat.
   DEFINE INPUT  PARAMETER hDat AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vAmtIn         AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vAmt           AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vDeltaAmt      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vLastCloseDate AS DATE      NO-UNDO.
   DEFINE VARIABLE vBalanceType   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vHPTaxRS       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vFileName      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOrigFileName  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOrigInfoType  AS CHARACTER NO-UNDO.

   RUN InstanceCreate (?, hDat).

   RUN acct-pos IN h_base (acct.acct,acct.currency,mDateSp,mDateSp,"√").
   vLastCloseDate = Get_CloseDate_Cat("b").
   IF vLastCloseDate = ? THEN
       vLastCloseDate = {&BQ-MIN-DATE}.
   vLastCloseDate = MINIMUM(vLastCloseDate, mDateSp).
   vHPTaxRS = ObjectValueHandle("PTaxRS") NO-ERROR.
   RUN GetCharAttrValue365p(vHPTaxRS, "FileName", OUTPUT vFileName) NO-ERROR.
   IF NUM-ENTRIES(vFileName, "_") > 1 THEN
       vOrigFileName = SUBSTRING(vFileName, INDEX(vFileName, "_") + 1) NO-ERROR.
   ASSIGN
       vOrigInfoType = {&INFO-TYPE-UNDEFINED}
       vOrigInfoType = {&INFO-TYPE-RPO}
                       WHEN vOrigFileName BEGINS "RBN"
                            OR
                            vOrigFileName BEGINS "RPO"
       vOrigInfoType = {&INFO-TYPE-ZNO}
                       WHEN vOrigFileName BEGINS "ZNO"
   .
   RUN GetBalanceTypeManual365p(vOrigInfoType,
                                {&REQ-KIND-TICLAS},
                                OUTPUT vBalanceType)
   NO-ERROR.
   RUN CalcPosDelta365p(acct.acct,
                        acct.currency,
                        vLastCloseDate,
                        mDateSp,
                        FGetSetting("Настройка_365П",
                                    "ДопСтатусыДок",
                                    ""),
                        OUTPUT vDeltaAmt)
   NO-ERROR.
   ASSIGN
       vAmtIn = IF {assigned acct.currency} THEN sh-in-val
                                            ELSE sh-in-bal
       vAmt   = IF {assigned acct.currency} THEN sh-val
                                            ELSE sh-bal
   .
   vAmt = ABSOLUTE(IF vBalanceType = {&BALANCE-BF} THEN vAmtIn
                                                   ELSE vAmt)
          +
          vDeltaAmt.

   hDat:BUFFER-FIELD(GetMangledName("НомСч"    )):BUFFER-VALUE =  acct.acct     NO-ERROR.
   hDat:BUFFER-FIELD(GetMangledName("ВидСч"    )):BUFFER-VALUE =  acct.contract NO-ERROR.
   hDat:BUFFER-FIELD(GetMangledName("ВалСч"    )):BUFFER-VALUE =
        IF acct.currency EQ "" THEN "643"
                               ELSE acct.currency NO-ERROR.
   hDat:BUFFER-FIELD(GetMangledName("Остаток"  )):BUFFER-VALUE =
        TRIM(STRING(ABSOLUTE(vAmt), ">>>>>>>>>>>9.99")) NO-ERROR.
END PROCEDURE.
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:20:59.471+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='taxrsexp.p' */
/*prosignClh7Ga6BA7Z4PtCFjIathw*/
/* --- taxrsexp.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:13am --- */
