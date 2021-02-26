/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: tax-ptaxx-ref.p
      Comment: ФНС (версия 5.10)
   Parameters: нет
         Uses:
      Used by:
      Created: 30.06.2014 zhua
     Modified: 15.01.2015 zhua
*/

DEFINE VARIABLE mRefCls   AS CHAR  NO-UNDO.
DEFINE VARIABLE mRefValue AS INT64 NO-UNDO.
DEFINE VARIABLE mPacketID AS INT64 NO-UNDO.
DEFINE VARIABLE mNomSoob  AS CHAR  NO-UNDO.
DEFINE VARIABLE mTypeSoob AS CHAR  NO-UNDO.
DEFINE VARIABLE mSeanceD  AS DATE  NO-UNDO.
DEFINE VARIABLE mOpKindKind AS CHAR  NO-UNDO. /*Вид транзакции экспорта*/
DEFINE VARIABLE mFlagError  AS LOGICAL NO-UNDO.
DEFINE VARIABLE mErrorClass AS CHAR    NO-UNDO.
DEFINE VARIABLE mErrorList  AS CHAR    NO-UNDO.
DEFINE VARIABLE hTaxExpCom  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hTaxExpInf  AS HANDLE  NO-UNDO.
DEFINE VARIABLE mKind       AS CHAR    NO-UNDO.
DEFINE VARIABLE mResult     AS CHAR INIT "" NO-UNDO.
DEFINE VARIABLE mPrvCntVal  AS CHAR    NO-UNDO INIT "".

DEFINE VARIABLE mValue        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mPacketState  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mTmpCount     AS INT64    NO-UNDO.

DEFINE VARIABLE mTmp AS CHARACTER   NO-UNDO.

{globals.i}
{exchange.equ}
{form.def}
{intrface.get xclass}
{intrface.get pbase}
{intrface.get exch}
{intrface.get trans}
{intrface.get strng}
{intrface.get rfrnc}
{intrface.get db2l}
{intrface.get tmess}
{intrface.get pack}
{intrface.get count}
{intrface.get acctn}
/* Вставка Плюс банк */
{debug.equ}
/* Конец вставки Плюс банк */

DEFINE VARIABLE mBaseOpKind AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mBaseTempl  AS INT64       NO-UNDO.
DEFINE VARIABLE mKindInfo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mIsCorrect  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mRefOff     AS LOGICAL     NO-UNDO INIT ?.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("tax-ptaxx-ref.p","program-name(1):" + program-name(1)).
   RUN dbgprint.p("tax-ptaxx-ref.p","program-name(2):" + program-name(2)).
   RUN dbgprint.p("tax-ptaxx-ref.p","program-name(3):" + program-name(3)).
&ENDIF

ASSIGN
   mBaseOpKind = GetBaseOpKind()
   mBaseTempl  = GetBaseTemplate()
   mPacketID   = INT64(GetAttrValue2(mBaseOpKind,mBaseTempl,"PacketID"))
   mNomSoob    = GetAttrValue2(mBaseOpKind,mBaseTempl,"$NomSoob")
   mTypeSoob   = GetAttrValue2(mBaseOpKind,mBaseTempl,"$TypeSoob")
   mSeanceD    = DATE(GetAttrValue2(mBaseOpKind,mBaseTempl,"$MySeanceD"))
   mOpKindKind = GetAttrValue2(mBaseOpKind,mBaseTempl,"OpkindKind")
   mErrorClass = GetAttrValue2(mBaseOpKind,mBaseTempl,"ClassError")
   mKind       = GetAttrValue2(mBaseOpKind,mBaseTempl,"Kind")
   mKindInfo   = GetAttrValue2(mBaseOpKind,mBaseTempl,"$KindInfo")
   hTaxExpCom  = GetTransObject(mKind)
   hTaxExpCom  = hTaxExpCom:default-buffer-handle
   mErrorList  = hTaxExpCom:buffer-field("ErrorList"):buffer-value
   mRefCls     = GetCode("MaskRefernce","ч")
   mIsCorrect  = TRNSettingValue("ExchSET-BNK","Correction","НЕТ") =  "YES"
   mRefOff     = GetAttrValue2(mBaseOpKind,mBaseTempl,"$RefOff") =  "YES"
NO-ERROR.

IF mKindInfo =  {&RET-ERROR} THEN
   mKindInfo = "TaxExpInf5X".
ASSIGN
   hTaxExpInf  = GetTransObject(mKindInfo) 
   hTaxExpInf  = hTaxExpInf:default-buffer-handle
NO-ERROR.
IF hTaxExpInf:buffer-field("ErrorList"):buffer-value <> ? THEN DO:
   DO mTmpCount = 1 TO NUM-ENTRIES(hTaxExpInf:buffer-field("ErrorList"):buffer-value):
      IF INDEX(ENTRY(mTmpCount,hTaxExpInf:buffer-field("ErrorList"):buffer-value),"Tax-") <> 0 THEN
         {additem.i mErrorList ENTRY(mTmpCount,hTaxExpInf:buffer-field('ErrorList'):buffer-value)}
   END.
END.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("tax-ptaxx-ref.p","mErrorList:" + mErrorList).
&ENDIF

IF {assigned mOpKindKind} THEN
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "OpKindKind", mOpKindKind).
   /*RUN SetSysConf IN h_base ("TaxOpKindKind",mOpKindKind).*/
RUN SetSysConf IN h_base ("TaxCommonHdl",STRING(hTaxExpCom)).
RUN SetSysConf IN h_base ("TaxInfoHdl", STRING(hTaxExpInf)).
RUN SetSysConf IN h_base ("NetINN", GetAttrValue2("", 0, "NetINN")).

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p("tax-ptaxx-ref.p","bef PObj").
      &ENDIF
      FOR EACH PackObject WHERE
          PackObject.PacketID  =  mPacketID AND
          PackObject.file-name =  "acct"
      NO-LOCK:
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p("tax-ptaxx-ref.p",
                           "PackObject.Surrogate:" + GetNullStr(PackObject.Surrogate)).
         &ENDIF
         RUN fill-pack-tt-dp IN h_acctn (ENTRY(1,PackObject.Surrogate),
                                         ENTRY(2,PackObject.Surrogate),
                                         mPacketID).
      END.
      IF NOT mRefOff THEN
      DO:
         mRefValue = INT64(ReferenceGetLast(mRefCls,mSeanceD)).
         RUN PacketCreateRef IN h_rfrnc (mSeanceD,
                                         mPacketID,
                                         mRefCls,
                                         ReferenceFormatValue(mRefCls,
                                          STRING(mRefValue + 1))).
      END.

      /* Проверка реквизитов */
      RUN CheckPart(mKind,     hTaxExpCom, INPUT-OUTPUT mErrorList).
      RUN CheckPart(mKindInfo, hTaxExpInf, INPUT-OUTPUT mErrorList).
      

      IF {assigned mErrorList} THEN
         RUN PacketSetError IN h_pack (mPacketID,
                                       mErrorClass,
                                       mErrorList,
                                       OUTPUT mPacketState).

      IF NOT mIsCorrect THEN
      DO:
         ASSIGN mPrvCntVal = GetAttrValue2("",0,"PrvCntVal") NO-ERROR.
         IF mPacketState =  {&STATE-ERR} AND
            {assigned mPrvCntVal}       THEN
               SetCounterValue (FGetSetting("ГНИ","КодНом1114301","ГНИ4"),
                                INT64(mPrvCntVal), TODAY).
         RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "PrvCntVal","").
      END.

      UpdateSigns("Packet",STRING(mPacketID),"StateOBJ",
                   mNomSoob + "," + mTypeSoob,?).

   END.       /*DO TRANSACTION*/

RUN SetSysConf IN h_base ("TaxCommonHdl", "").
RUN SetSysConf IN h_base ("TaxInfoHdl",   "").
RUN SetSysConf IN h_base ("NetINN",       "").

{intrface.del}
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CheckTaxAttr.
   DEFINE PARAMETER BUFFER ErrXattr FOR xattr.  /* код ошибки  */
   DEFINE PARAMETER BUFFER xattr    FOR xattr.  /* реквизит ТФ */
   DEFINE INPUT  PARAMETER iValue     AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrorList AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER oOK        AS LOGICAL NO-UNDO INITIAL YES.



   DEFINE VAR vLength    AS INT64                NO-UNDO.
   DEFINE VAR vProcName  AS CHAR                 NO-UNDO.
   DEFINE VAR vParams    AS CHAR                 NO-UNDO.
   DEFINE VAR vErrorList AS CHAR                 NO-UNDO.
   DEFINE VAR vLenErr    AS LOGICAL              NO-UNDO.

   
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("CheckTaxAttr", xattr.Class-Code + "." + xattr.Xattr-Code + 
                                  " ivalue:" + getNullStr(iValue) + " " + ErrXattr.Xattr-Code).
   &ENDIF
   
MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CheckTaxAttr","xattr.xattr-code:" + GetNullStr(xattr.xattr-code) +
                                  " iValue:" + GetNullStr(iValue)).
   &ENDIF

   /* Проверка на непревышение длины */
   IF ErrXattr.xattr-code =  "Tax-000" OR 
      ErrXattr.xattr-code =  "Tax-999" THEN
   DO:
/* Вставка Плюс банк */
      DEFINE VARIABLE vLoanNum AS CHARACTER NO-UNDO.
      DEFINE VARIABLE mNom     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE mSymb    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE mInt     AS INT64     NO-UNDO.

      IF xattr.xattr-code EQ "НомерДог" THEN vLoanNum = iValue. 
      
      IF xattr.xattr-code EQ "НомерДог"
         AND LENGTH(vLoanNum) GT 20 THEN
      DO:
         mNom = vLoanNum.
         REPEAT mInt = 1 TO 100:
            IF LENGTH(mNOm) EQ 1 THEN LEAVE.
            mSymb = SUBSTRING(mNom,LENGTH(mNom)).
            IF mSymb NE "/"
            THEN mNom = SUBSTRING(mNom,1,LENGTH(mNOm) - 1 ). 
            ELSE LEAVE.
         END.       
         mNom = TRIM(mNom,"/").
         vLoanNum = mNom.
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("my","xattr.xattr-code:"    + GetNullStr(xattr.xattr-code) +
                                        " iValue:"   + GetNullStr(iValue) + 
                                        " vLoanNum:" + GetNullStr(vLoanNum)).
         &ENDIF
         iValue = vLoanNum.
      END.
/* Конец вставки Плюс банк */
      vLength = IF INDEX(xattr.data-format,"(") <> 0
                   THEN INT64(SUBSTR(xattr.data-format,
                                   INDEX(xattr.data-format,"(") + 1,
                                   LENGTH(xattr.data-format) - 3))
                   ELSE LENGTH(xattr.data-format).

      IF ErrXattr.xattr-code =  "Tax-000"  AND
         (LENGTH(iValue) > vLength)        THEN
         vLenErr = YES.

      IF ErrXattr.xattr-code =  "Tax-999"  AND
         {assigned iValue}                 AND
         (LENGTH(iValue) < vLength)        THEN
            vLenErr = YES.
          
      IF vLenErr THEN
      DO:
         RUN Fill-SysMes("","","",ErrXattr.xattr-code + "[" +
                                  ErrXattr.name       + " " +
                                  xattr.class-code    + ":" +
                                  xattr.xattr-code    + " " +
                                  ErrXattr.Initial    + " " +
                                  GetNullStr(iValue)  + "]").
         vErrorList = ErrXattr.xattr-code + CHR(1) +
                      xattr.class-code  + ":" + xattr.xattr-code.
         {additem.i oErrorList vErrorList}
         oOK = ErrXattr.Initial <> "Ошибка".
      END.
   END. 
   ELSE DO:
      RUN GetClassMethod IN h_xclass (INPUT  ErrXattr.class-code,
                                      INPUT  "chkupd",
                                      INPUT  ErrXattr.xattr-code,
                                      INPUT  "",
                                      OUTPUT vProcName,
                                      OUTPUT vParams).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ExportOneClass","vProcName:" + GetNullStr(vProcName)).
      &ENDIF

      IF {assigned vProcName} THEN DO:              /* Вызов найденного метода   */
         {exch-run.i &Proc      = vProcName
                     &Parm      = "iValue" 
                     &NoMessage = "YES"
         }
         IF ERROR-STATUS:ERROR THEN DO:
            RUN Fill-SysMes("","ComnExc01","","%s=" + vProcName +
                                              "%s=" + ERROR-STATUS:get-message(1)).
            oOk = NO.
            LEAVE MAIN.
         END.

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CheckTaxAttr","xattr.xattr-code:" + GetNullStr(xattr.xattr-code) +
                                        " RETURN-VALUE:" + GetNullStr(RETURN-VALUE)).
         &ENDIF

         IF {assigned RETURN-VALUE} THEN DO:
            RUN Fill-SysMes("","","", ErrXattr.xattr-code + "[" +
                                      ErrXattr.name       + " " +
                                      xattr.class-code    + ":" +
                                      xattr.xattr-code    + " " +
                                      ErrXattr.Initial    + " " +
                                      GetNullStr(iValue)  + "]").
            oErrorList = ErrXattr.Xattr-Code.
            oOk = ErrXattr.Initial <> "Ошибка".
         END.
      END.

   END.
                                   /* индивидуальный метод на ошибке */
END. /* MAIN */

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CheckPart.
   DEFINE INPUT         PARAMETER iEClass       AS CHARACTER   NO-UNDO.
   DEFINE INPUT         PARAMETER hExch         AS HANDLE      NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER ioErrorList   AS CHARACTER   NO-UNDO.

   DEFINE BUFFER Xattr  FOR Xattr.
   DEFINE BUFFER eXattr FOR Xattr.

   DEFINE VARIABLE vValue AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOK    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vError AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI     AS INT64       NO-UNDO.

   /* Проход по реквизитам, для которых определены ошибки */
   FOR EACH eXAttr WHERE eXAttr.Class-Code =  iEClass 
                     AND eXattr.xattr-clabel >  ""
      NO-LOCK:
      vValue = hExch:BUFFER-FIELD(GetMangledName(eXattr.Xattr-Code)):BUFFER-VALUE.
      /*  Проход по кодам ошибок */
      DO  vI = 1 TO NUM-ENTRIES(eXattr.xattr-clabel) :
         FOR FIRST Xattr WHERE Xattr.Class-Code =  mErrorClass
                           AND Xattr.Xattr-Code =  ENTRY(vI, eXattr.xattr-clabel)
                           AND Xattr.Initial    >  "" 
         NO-LOCK:
            RUN CheckTaxAttr(BUFFER Xattr,
                             BUFFER eXattr,
                             vValue,
                             OUTPUT vError,
                             OUTPUT vOK).
            IF NOT vOK THEN DO:
               {additem.i ioErrorList vError}
            END.
         END.
      END.
   END.

END PROCEDURE.

/******************************************************************************/
/* $LINTFILE='tax-ptaxx-ref.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:45.955+03:00' */
/*prosignGx09KM6nmmVRjVgWXDP9Hg*/