/*              
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 ЗАО "Банковские информационные системы"
     Filename: ac-order.p
      Comment: Сортировка и отбор счетов для экспортав МНС
               Является методом (код UPDATE) класса, унаследованого от Filter

               Выполняемые действия:

               К моменту вызова должны существовать инстансы:

   Parameters: iClass      - код класса
               iInstance   - содержимое класса iClass
         Uses:
      Used BY:
      Created: 25.03.2004 NIK
     Modified: 09.03.2005 Mike - клон FX-ORDER.P
     Modified: 07.10.2005 NIK "Переоформление" рейсов экспорта
     Modified: 29.07.2008 12:48 MUTA 0096028 Использование  RetRcp
*/
{globals.i}
DEFINE NEW GLOBAL SHARED STREAM debug-stream.

DEFINE INPUT PARAMETER iClass       AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iInstance    AS handle   NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}
{intrface.get xclass}

{intrface.get tmess}
{intrface.get pbase}
{intrface.get trans}
{intrface.get data}

{intrface.get pack}
{intrface.get exch}
{intrface.get edeal}
{intrface.get flt}
{intrface.get strng}

{sh-defs.i}

{debug.equ}

DEFINE VAR hFilterTable AS handle            NO-UNDO.
DEFINE VAR hFilter      AS handle            NO-UNDO.

DEFINE VAR hBuffer      AS handle            NO-UNDO.
DEFINE VAR mRetryErr    AS CHAR              NO-UNDO.
DEFINE VAR mFlagSet     AS LOGICAL  INIT ?   NO-UNDO.
DEFINE VAR mFileFind    AS LOGICAL           NO-UNDO.

DEFINE VAR mOpKind      AS CHAR              NO-UNDO.
DEFINE VAR mTmplID      AS INT64             NO-UNDO.
DEFINE VAR mRoleMess    AS CHAR              NO-UNDO.
DEFINE VAR mAttrList    AS CHAR              NO-UNDO.
DEFINE VAR mValList     AS CHAR              NO-UNDO.
DEFINE VAR mSeanceID    AS INT64             NO-UNDO.
DEFINE VAR mReForm      AS LOGICAL           NO-UNDO.
DEFINE VAR mRefusal     AS LOGICAL           NO-UNDO.
DEFINE VAR mIsCorrect   AS LOGICAL           NO-UNDO.
DEFINE VAR mOpCl        AS CHAR              NO-UNDO.
DEFINE VAR mFirstFilter AS LOGICAL           NO-UNDO.
DEFINE VAR mBanExp      AS LOGICAL           NO-UNDO.

DEFINE VARIABLE mSilent      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCodeLstChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mValLstChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateModif   AS DATE      NO-UNDO.

DEF BUFFER Seance       FOR Seance.

DEFINE temp-table ttObj no-undo
    FIELD Acct    AS CHAR
    FIELD CustId  AS INT64
    FIELD FName   AS CHAR
    FIELD DateExp AS DATE
    FIELD TimeExp AS INT64
    FIELD StateExp AS CHAR
    INDEX CustId CustId.

   
{ttretval.def}
&GLOB RETRY-ERROR  mRetryErr                     /* для do-retry.i            */
{ac-ordsni.fun}
/*============================================================================*/
FUNCTION LastTaxImp RETURN CHAR (INPUT iAcct      AS CHAR,
                                 INPUT iCurrency  AS CHAR):

   DEF VAR vResult AS CHAR INIT "" NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.
   DEF BUFFER Packet     FOR Packet.
   DEF BUFFER FileExch   FOR FileExch.

   FOR EACH PackObject WHERE
            PackObject.file-name EQ 'acct'
        AND PackObject.Surrogate EQ iAcct + "," + iCurrency  NO-LOCK,
       EACH Packet     WHERE
            Packet.PacketID      EQ PackObject.PacketID
            NO-LOCK,
      FIRST FileExch   WHERE
            FileExch.FileExchID  EQ packet.FileExchID        NO-LOCK
            BY Packet.PackDate DESCENDING
            BY Packet.PackTime DESCENDING:

      IF  Packet.Kind EQ "TaxImp" THEN
         vResult =  SUBSTRING(FileExch.name,3,1).
         LEAVE.
   END.

   RETURN vResult.
END FUNCTION.

/*происк предыдущих сообщений*/
FUNCTION LastTaxImp2 RETURN CHAR PRIVATE (
   INPUT iAcct      AS CHAR,  /* Счет. */
   INPUT iCurrency  AS CHAR,  /* Валюта счета. */
   INPUT iKindExp   AS CHAR,
   INPUT iKindImp   AS CHAR
):
   DEF VAR vResult   AS CHAR  INIT ""   NO-UNDO.
   DEF VAR vDate     AS DATE            NO-UNDO.
   DEF VAR vTime     AS INT64           NO-UNDO.
   DEF VAR vOpCl     AS CHAR INIT ""    NO-UNDO.
   DEF VAR vSign     AS CHAR INIT ""    NO-UNDO. /*признак эл. сообщения:
                                                   3 - отменяющее*/
   DEF VAR vFileName AS CHAR INIT ""    NO-UNDO.
   DEF VAR vStat     AS CHAR INIT ""    NO-UNDO.

   DEF BUFFER PackObject   FOR PackObject.   /* Локалиация буфера. */
   DEF BUFFER Packet       FOR Packet.       /* Локалиация буфера. */
   DEF BUFFER Seance       FOR Seance.       /* Локалиация буфера. */
   DEF BUFFER FileExch     FOR FileExch.     /* Локалиация буфера. */

                        /* Поиск последнего сообщения экспорта. */
   BLCK_GET_LST_EXP:
   /* SORT-ACCESS Packet вынужденно */
   FOR EACH PackObject  WHERE
            PackObject.file-name EQ "acct"
      AND   PackObject.Surrogate EQ iAcct + "," + iCurrency
   NO-LOCK,
   EACH Packet          WHERE
            Packet.PacketID      EQ PackObject.PacketID
      AND   Packet.Kind          EQ iKindExp
   NO-LOCK,
   FIRST Seance         WHERE 
            Seance.SeanceID      EQ Packet.SeanceID
      AND   Seance.Direct        EQ "Экспорт"
   NO-LOCK,
   FIRST FileExch       WHERE
            FileExch.FileExchID  EQ packet.FileExchID
   NO-LOCK
      BY Packet.PackDate DESC
      BY Packet.PackTime DESC
   :
      ASSIGN
         vDate  = Packet.PackDate
         vTime  = Packet.PackTime
         vOpCl = SUBSTRING (FileExch.name, 4, 1)
         vSign = SUBSTRING (FileExch.name, 5, 1)
         vFileName = "*" + SUBSTRING(FileExch.name,4,LENGTH(FileExch.name) - 3)
         vStat = Packet.State
      .
      LEAVE BLCK_GET_LST_EXP.
   END.
                        /* Поиск сообщения импорта
                        ** по найденному сообщению экспорта. */
   /* SORT-ACCESS Packet вынужденно */
   IF AVAIL Packet THEN
      FOR EACH PackObject  WHERE
               PackObject.file-name EQ "acct"
         AND   PackObject.Surrogate EQ iAcct + "," + iCurrency
      NO-LOCK,
      EACH Packet          WHERE
               Packet.PacketID      EQ PackObject.PacketID
         AND   Packet.Kind          EQ iKindImp
         AND   Packet.PackDate      GE vDate
         AND  (IF Packet.PackDate   EQ vDate
                  THEN Packet.PackTime GT vTime
                  ELSE YES
              )
      NO-LOCK,
      FIRST Seance         WHERE 
               Seance.SeanceID      EQ Packet.SeanceID
         AND   Seance.Direct        EQ "Импорт"
      NO-LOCK,
      FIRST FileExch       WHERE
               FileExch.FileExchID  EQ packet.FileExchID AND
               FileExch.name   MATCHES vFileName
      NO-LOCK
         BY Packet.PackDate DESC
         BY Packet.PackTime DESC
      :
         vResult = vResult + SUBSTRING (FileExch.name, 3, 1).
      END.

   vResult = (IF {assigned vOpCl} THEN vOpCl ELSE "") + CHR(1) + vResult + CHR(1) + 
             (IF {assigned vSign} THEN vSign ELSE "") + CHR(1) +
             vStat.

   RETURN vResult.
END FUNCTION.

/* ----------------------------------------------------------------------------------------- */
FUNCTION LastTaxImp3 RETURN LOGICAL PRIVATE (
   INPUT iAcct      AS CHAR,  /* Счет. */
   INPUT iCurrency  AS CHAR,  /* Валюта счета. */
   INPUT iKindExp   AS CHAR,
   INPUT iKindImp   AS CHAR,
   INPUT iSNIKORR   AS DATE   
):
   DEF VAR vResult   AS CHAR  INIT ""   NO-UNDO.
   DEF VAR vDate     AS DATE            NO-UNDO.
   DEF VAR vTime     AS INT64           NO-UNDO.

   DEFINE VARIABLE vFlagFind AS LOGICAL INIT NO NO-UNDO.

   DEF BUFFER PackObject    FOR PackObject.   /* Локалиация буфера. */
   DEF BUFFER Packet        FOR Packet.       /* Локалиация буфера. */
   DEF BUFFER xSeance       FOR Seance.       /* Локалиация буфера. */
   DEF BUFFER FileExch      FOR FileExch.     /* Локалиация буфера. */
   DEF BUFFER bPackObject   FOR PackObject.   /* Локалиация буфера. */
   DEF BUFFER bPacket       FOR Packet.       /* Локалиация буфера. */
   DEF BUFFER bSeance       FOR Seance.       /* Локалиация буфера. */
   DEF BUFFER bFileExch     FOR FileExch.     /* Локалиация буфера. */

   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} ===== iAcct", iAcct).
      run dbgprint.p(program-name(1) + " {&line-number} iCurrency", iCurrency).
      run dbgprint.p(program-name(1) + " {&line-number} iKindExp", iKindExp).
      run dbgprint.p(program-name(1) + " {&line-number} iKindImp", iKindImp).
      run dbgprint.p(program-name(1) + " {&line-number} iSNIKORR", string(iSNIKORR)).
   &ENDIF 
                        /* Поиск последнего сообщения экспорта. */
   BLCK_GET_LST_EXP:
   /* SORT-ACCESS Packet */
   FOR EACH bPackObject  WHERE
      bPackObject.file-name EQ "acct" AND
      bPackObject.Surrogate EQ iAcct + "," + iCurrency
         NO-LOCK,
      EACH bPacket WHERE
         bPacket.PacketID      EQ bPackObject.PacketID AND
         bPacket.Kind          MATCHES iKindExp
            NO-LOCK,
      FIRST bSeance WHERE 
         bSeance.SeanceID      EQ bPacket.SeanceID AND
         bSeance.Direct        EQ "Экспорт"
            NO-LOCK,
      FIRST bFileExch WHERE
         bFileExch.FileExchID  EQ bpacket.FileExchID
            NO-LOCK
      BY bPacket.PackDate DESC
      BY bPacket.PackTime DESC:

      ASSIGN
         vDate  = bPacket.PackDate
         vTime  = bPacket.PackTime
      NO-ERROR.

      LEAVE BLCK_GET_LST_EXP.
   END.
                        /* Поиск сообщения импорта
                        ** по найденному сообщению экспорта. */
   IF AVAIL bPacket THEN DO:
      &IF DEFINED(IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} vDate", string(vDate)).
         run dbgprint.p(program-name(1) + " {&line-number} vTime", string(vTime,"hh:mm:ss")).
         run dbgprint.p(program-name(1) + " {&line-number} bPacket.PacketID", 
            string(bPacket.PacketID)). 
         run dbgprint.p(program-name(1) + " {&line-number} bSeance.Number", string(bSeance.Number)). 
      &ENDIF
   END.

   IF AVAIL bPacket THEN DO:
      /* SORT-ACCESS Packet */
      FOR EACH PackObject  WHERE
         PackObject.file-name EQ "acct" AND
         PackObject.Surrogate EQ iAcct + "," + iCurrency
            NO-LOCK,
         EACH Packet WHERE
            Packet.PacketID      EQ PackObject.PacketID AND
            Packet.Kind          EQ iKindImp AND
            Packet.PackDate      GE vDate AND
            (IF Packet.PackDate  EQ vDate
                  THEN Packet.PackTime GT vTime
                  ELSE YES)
            NO-LOCK,
      FIRST xSeance WHERE 
         xSeance.SeanceID      EQ Packet.SeanceID AND
         xSeance.Direct        EQ "Импорт"
            NO-LOCK,
      FIRST FileExch WHERE
         FileExch.FileExchID  EQ packet.FileExchID
            NO-LOCK
      BY Packet.PackDate DESC
      BY Packet.PackTime DESC:
         vResult = SUBSTRING (FileExch.name, 3, 1).

         &IF DEFINED(IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} vResult", vResult).
            run dbgprint.p(program-name(1) + " {&line-number} FileExch.name", FileExch.name).
            run dbgprint.p(program-name(1) + " {&line-number} Packet.PackDate", 
               string(Packet.PackDate)).
            run dbgprint.p(program-name(1) + " {&line-number} Packet.PackTime", 
               string(Packet.PackTime,"hh:mm:ss")).
         &ENDIF

         IF vResult EQ 'K' THEN
            vFlagFind = YES.

         ELSE IF (vResult EQ 'F') AND (vDate LT iSNIKORR) THEN
            vFlagFind = YES.
      END.
   END.

   IF iAcct EQ "42301810505160012371     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810905160012138     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810105090013138     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810105090012901     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810705120013466     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810405120013240     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810005110014379     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810605110013801     @0500" THEN vFlagFind = YES.

   IF iAcct EQ "42306810005980016984     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810205980017568     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810905980017567     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810305980016985     @0500" THEN vFlagFind = YES.
   
   IF iAcct EQ "42307810205120010057     @0500" THEN vFlagFind = YES.
   
   IF iAcct EQ "42301810605160011544     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810005160011372     @0500" THEN vFlagFind = YES.



   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} vFlagFind", string(vFlagFind)).
   &ENDIF

   RETURN vFlagFind.
END FUNCTION.
/* ----------------------------------------------------------------------------------------- */

{empty ttObj}
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hFilter     = iInstance:default-buffer-handle
      mTmplID     = INT64(hFilter:buffer-field("__tmpl-ID"):buffer-value)
      mOpKind     = GetBaseOpKind()
      mRoleMess   = GetAttrValue(mOpKind, mTmplID, "$RoleMessage")
      mReForm     = TRNSettingValue("","SeanceReform","НЕТ") EQ "ДА" 
      mRefusal    = TRNSettingValue("","Refusal","НЕТ") EQ "ДА"
      mIsCorrect  = TRNSettingValue("","Correction","НЕТ") EQ "YES"
      mOpCl       = GetAttrValue(mOpKind, mTmplID, "$OpCl")
      mBanExp     = FGetSetting("ЗапретОдСообщ","ГНИ","Нет") EQ "ДА"                
      mSeanceID   = INT64(GetAttrValue2(mOpKind,mTmplID,"$SeanceID"))
      mSilent     = CAN-DO("Да,YES", GetAttrValue2(mOpKind, mTmplID, "$Silent"))   
      mDateModif  = DATE(FGetSetting("ГНИ","Дата311","01/07/2014")) 
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.   
   
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "OpKindKind", SUBSTRING(mRoleMess,14)).

   mReForm = NO. 
   MESSAGE COLOR NORMAL
      "Выполнить переформирование сеанса "                SKIP
      "по текущему фильтру " + ENTRY(3,hFilter::FiltInfo)  + " ? "       SKIP
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE mReForm.

   RUN GetParentXMethod (iClass,"update", BUFFER class-method).
   IF AVAILABLE(class-method) THEN DO:

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","class-method:" + class-method.PROCEDURE).
   &ENDIF

      {exch-run.i &Proc = class-method.PROCEDURE
                  &Parm = "iClass, iInstance"}
      IF ERROR-STATUS:ERROR THEN DO:
         mRetryErr = "Невозможно выполнить метод """ + iClass +
                                                 "." + class-method.PROCEDURE + """".
         mFlagSet  = NO.
         UNDO MAIN, RETRY MAIN.
      END.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","END:" + class-method.PROCEDURE).
   &ENDIF

   ASSIGN
      hFilterTable = widget-handle(hFilter:buffer-field("FilterTable"):buffer-value)
   NO-ERROR.

   IF NOT valid-handle(hFilterTable) THEN DO:
      mRetryErr = "Нет данных для экспорта".
      mFlagSet  = YES.
      UNDO MAIN, LEAVE MAIN.
   END.

   hBuffer = hFilterTable:default-buffer-handle.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","mIsCorrect:" + STRING(mIsCorrect)).
   &ENDIF
/*   message GetAttrValue(mOpKind, mTmplID, "$VersForm") mOpKind mTmplID view-as alert-box. */
   IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
   DO:
      IF NOT mRefusal THEN       
         IF mIsCorrect THEN 
            RUN AcctDeleteNoSend(hBuffer,mRoleMess).
         ELSE
            RUN AcctDelete510(hBuffer).           
      ELSE
         RUN AcctDeleteRefusal(hBuffer).
      mFirstFilter =
       GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$FirstFilter") EQ "ДА".

      IF mFirstFilter THEN
      DO:
         FIND FIRST Seance WHERE
                    Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
         IF AVAIL(Seance) THEN 
            RUN snc-cncl-exh.p (BUFFER Seance, NO).
      END.
      IF mBanExp THEN
         RUN AcctDeleteBan(hBuffer).
   END.
   ELSE
      IF mIsCorrect THEN 
         RUN AcctDeleteNoSend(hBuffer,mRoleMess).
      ELSE
         RUN AcctDeleteSend(hBuffer,mRoleMess).

   hBuffer:find-first() NO-ERROR.         
   IF ERROR-STATUS:ERROR THEN DO:
      mRetryErr = "Нет данных для экспорта".
      mFlagSet  = YES.
      UNDO MAIN, LEAVE MAIN.
   END.
   
   mAttrList = GetInstanceProp(iInstance,"AttrList").
   IF {assigned mAttrList} THEN
      ASSIGN
         mValList  = CHR(1) + GetInstanceProp(iInstance,"ValList")
         mAttrList = CHR(1) + mAttrList.

   ASSIGN
       mCodeLstChar = "FilterTable" + CHR(1) +
                      "RidRest"     + CHR(1) +
                      "RetRcp"      + CHR(1) +
                      "RetFld"      + CHR(1) +
                      "RetType"              +
                      mAttrList
       mValLstChar  = STRING(hFilterTable)               + CHR(1) +
                      "YES"                              + CHR(1) +
                      STRING(TEMP-TABLE ttRetVal:HANDLE) + CHR(1) +
                      "ROWID"                            + CHR(1) +
                      "Multi"                                     +
                      mValList
   .

USR-SEL:
   REPEAT:
   {empty ttRetVal}

   IF mSilent THEN DO:
       RUN SelectFltObject IN h_flt ("acct", mCodeLstChar, mValLstChar, "").
       LEAVE USR-SEL.
   END.
   ELSE
      RUN browseld.p ("acct", mCodeLstChar, mValLstChar, "", 4).

      IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:

            MESSAGE COLOR NORMAL
                    "Выполнить создание сообщений для отмеченных счетов ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            SET vChoice AS LOGICAL.
            CASE vChoice:
               WHEN YES THEN DO:
                  RUN KeepSelectFilter(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO  THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
         ELSE DO:
            MESSAGE COLOR NORMAL
                    "Не отмечен  ни один счет для экспорта." SKIP
                    "Продолжить выполнение транзакции ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  SET vChoice.
            CASE vChoice:
               WHEN YES THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO THEN DO:
                  RUN DelFilterTable(hBuffer).
                  mRetryErr = "Отказ от выполнения экспорта".
                  mFlagSet  = NO.
                  UNDO MAIN, RETRY MAIN.
               END.
               WHEN ?  THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
      ELSE IF KEYFUNCTION(LASTKEY) EQ "GO" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:
            RUN KeepSelectFilter(hBuffer).
            LEAVE USR-SEL.
         END.
         ELSE DO:
            MESSAGE COLOR NORMAL
                    "Выполнить экспорт всех счетов ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            SET vChoice.
            CASE vChoice:
               WHEN YES THEN DO:
                  IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
                     RUN KeepSelectFilterForAll(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO  THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
   END.                                          /* USR-SEL: REPEAT:          */

   mFlagSet = YES.
END.                                             /* MAIN:  DO:                */

&IF DEFINED(IS-DEBUG) &THEN
RUN PrintFilterLog (hBuffer).
&ENDIF

{intrface.del}

{doreturn.i mFlagSet}
/*----------------------------------------------------------------------------*/
/* Сохранение отобраных значений                                              */
/*----------------------------------------------------------------------------*/
PROCEDURE KeepSelectFilter:
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vacct     AS CHAR              NO-UNDO.
   DEFINE VAR vcurrnecy AS CHAR              NO-UNDO.

   CREATE QUERY hQuery.
   hQuery:ADD-BUFFER(hBuffer).
   IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
      OR NOT hQuery:QUERY-OPEN()  THEN RETURN.

   hQuery:GET-FIRST().
   DO WHILE hBuffer:AVAILABLE:
      ASSIGN
         vacct     = hBuffer:buffer-field("acct"):buffer-value
         vcurrnecy = hBuffer:buffer-field("currency"):buffer-value
      NO-ERROR.

      FIND FIRST acct WHERE acct.acct     EQ vacct
                        AND acct.currency EQ vCurrnecy NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         IF NOT CAN-FIND(FIRST ttRetVal WHERE
                               ttRetVal.FileRowId EQ ROWID(acct)) THEN
            hBuffer:BUFFER-DELETE().
         ELSE
            IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
               IF NOT isSNI (vacct,vCurrnecy) THEN 
                  hBuffer:BUFFER-DELETE().
      hQuery:GET-NEXT().
   END.
   hQuery:QUERY-CLOSE().

   DELETE OBJECT hQuery.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                                                                                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteSend:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE INPUT PARAMETER iDisable  AS CHAR     NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vDisable  AS CHAR              NO-UNDO.
   DEFINE VAR vEnable   AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         vItem = LastTaxImp(vAcct,vCurrency).

   &IF DEFINED(IS-DEBUG) &THEN
   run dbgprint.p ("AcctDeleteSend","vAcct:"      + vAcct  +
                                    " vItem:"     + vItem  +
                                    " mRoleMess:" +  mRoleMess).
   &ENDIF
  
         IF mRoleMess MATCHES "*close*"
         THEN DO:
            IF vItem EQ "K"  OR
               vItem EQ "N"  OR
               vItem EQ "T"  OR
               vItem EQ "E" 
               THEN DO:
                  hQuery:GET-NEXT() NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                  NEXT LOOP.
               END.
         END.
         ELSE DO:
            IF vItem EQ "E"  
               THEN DO:
                  hQuery:GET-NEXT() NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                  NEXT LOOP.
               END.
         END.

         IF mRefusal THEN DO:
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vacct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE IF NOT mReForm THEN DO:

            IF NOT PacketEnableExhange ("acct",
                                        vacct + "," + vCurrency,
                                        iDisable,
                                        {&NOTHING},
                                        0) THEN DO:

               hBuffer:BUFFER-DELETE() NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
         END.
         ELSE DO:                      /* Удалить все сообщения, кроме текущего рейса */
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vacct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:

               vFlagFind = YES.    
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().

      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */

   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Коррекция                                                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteNoSend:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE INPUT PARAMETER iDisable  AS CHAR     NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vDisable  AS CHAR              NO-UNDO.
   DEFINE VAR vEnable   AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vSNIKORR  AS DATE              NO-UNDO.
   DEFINE VAR vAcct-Surr AS CHAR             NO-UNDO.      

   DEF BUFFER Packet     FOR Packet.
   DEF BUFFER PackObject FOR PackObject.
   DEF BUFFER FileExch   FOR FileExch.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
            vAcct-Surr = SUBSTITUTE("&1,&2",vAcct,vCurrency)
            vSNIKORR  = DATE(GetXAttrValueEX("acct",vAcct-Surr,"СНИКорр",?))            
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         ASSIGN
            vFlagFind = LastTaxImp3(vAcct, vCurrency, "TaxExp*5*", "TaxImp", vSNIKORR)
         NO-ERROR.

         IF NOT vFlagFind THEN DO:          
            &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("AcctDeleteSend","acct:" + vacct + "->delete").
            &ENDIF
            
            hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().

      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */

   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*Отбор счетов с учетом уже выгруженных сообщений                             */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDelete510:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vSign     AS CHAR              NO-UNDO.
   DEFINE VAR vStat     AS CHAR     INIT ""  NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.
   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} AcctDelete510", "START").
   &ENDIF

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).
         &IF DEFINED(IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} ===== vAcct", vAcct).
         &ENDIF
         {find-act.i &acct = vAcct
                     &curr = vCurrency
         }
         IF mReform THEN                /*если пользователь ответил ДА*/
         DO:                            /*на переформирование по фильтру*/
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vAcct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE
         DO:
            {empty ttObj}
            ASSIGN
               vItem = LastTaxImp2(vAcct,           vCurrency,
                                    "TaxExpCommon5X","TaxImp")
            .
            &IF DEFINED(IS-DEBUG) &THEN
               run dbgprint.p(program-name(1) + " {&line-number} vItem", vItem).
            &ENDIF

            ASSIGN
               vOpCl = ENTRY(1,vItem,CHR(1))
               vSign = ENTRY(3,vItem,CHR(1))
               vStat = GetENTRies(4,vItem,CHR(1),"")
               vItem = ENTRY(2,vItem,CHR(1))
            .

            &IF DEFINED(IS-DEBUG) &THEN
               run dbgprint.p(program-name(1) + " {&line-number} mOpCl", mOpCl).
               run dbgprint.p(program-name(1) + " {&line-number} vOpCl", vOpCl).
               run dbgprint.p(program-name(1) + " {&line-number} vSign", vSign).
               run dbgprint.p(program-name(1) + " {&line-number} vItem", vItem).
               run dbgprint.p(program-name(1) + " {&line-number} vStat", vStat).
            &ENDIF

            IF {assigned vOpCl} THEN        /*если есть экспорт и*/
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
                  run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 1").
               &ENDIF
               IF vItem EQ ""  OR       /*нет импорта*/
                  (
                   (INDEX(vItem,"F") NE 0 OR       /*или импорт есть с типами F, R или K*/
                    INDEX(vItem,"R") NE 0 OR
                    INDEX(vItem,"P") NE 0 OR
                    INDEX(vItem,"N") NE 0 OR
                    INDEX(vItem,"K") NE 0
                   )                      AND
                    INDEX(vItem,"E") EQ 0          /*но при этом нет импорта с типом E*/
                  )            OR
                   (INDEX(vItem,"E") NE 0   AND  /*или есть c типом E, но */
                    mOpCl            EQ "0" AND  /*транзакция открытия*/
                    vOpCl            NE "0"      /*а посл сообщ (с ошиб импортом)  - не открытие*/
                   ) THEN
               DO:                   /*то удаляем счет из выборки по условию:*/
                  &IF DEFINED(IS-DEBUG) &THEN
                     run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 2").
                  &ENDIF
                /*Если транзакция открытия (0) и посл. сообщение экспорта любое
                  ИЛИ
                  транз. закрытия и посл. сообщение экспорта тоже закрытие (2)
                    */
                  IF mOpCl EQ "0"    OR 
                    (mOpCl EQ "2"    AND
                     mOpCl EQ vOpCl) THEN
                  DO:
                     &IF DEFINED(IS-DEBUG) &THEN
                        run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 3").
                     &ENDIF
                     IF
                        ((mOpCl EQ "0"        OR /*если транзакция открытия и */
                          mOpCl EQ "2")       AND /*посл. сообщение об открытии*/
                         mOpCl EQ vOpCl)      AND /*или если транзакция закрытия и */ 
                                                  /*посл. сообщение о закрытии*/
                        vSign EQ "3"          AND  /*при этом отменяющее и */
                        {assigned vItem}      AND  /*сквитованное*/
                        INDEX(vItem,"E") EQ 0 THEN /*успешно*/
                     .   /*то не удаляем из выборки*/
                     ELSE
                     DO:
                        &IF DEFINED(IS-DEBUG) &THEN
                           run dbgprint.p(program-name(1) + " {&line-number} AcctDelete510",
                                          "DELETE !").
                        &ENDIF
                        hBuffer:BUFFER-DELETE() NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                     END.
                  END.
                  ELSE IF mOpCl EQ "2" THEN
                  DO:
                     &IF DEFINED(IS-DEBUG) &THEN
                        run dbgprint.p(program-name(1) + " {&line-number} 2 vOpCl ",  vOpCl).
                        run dbgprint.p(program-name(1) + " {&line-number} 2 vItem ",  vItem).
                     &ENDIF 
                     IF ((vOpCl EQ "0" OR vOpCl EQ "1") AND
                         (NOT {assigned vItem} OR
                          (INDEX(vItem,"F") EQ 0 AND
                           INDEX(vItem,"E") EQ 0 
                          )
                         ) AND NOT CAN-DO("Подт,СКВТ",vStat)
                        ) OR vOpCl EQ "" THEN
                     DO:
                        &IF DEFINED(IS-DEBUG) &THEN
                           run dbgprint.p(program-name(1) + " {&line-number} ",
                                          "Закрытие а открытие не сквитовано").
                        &ENDIF
                        /*Переделать для открытия и закрытия в один день*/  
                        /*hBuffer:BUFFER-DELETE() NO-ERROR.*/
                        IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                     END.
                  END.
               END.
            END.
            ELSE IF mOpCl EQ "2" AND
               NOT {assigned vOpCl} AND
               acct.open-date >= mDateModif THEN
            DO:
               RUN dbgprint.p(program-name(1) + " {&line-number} ",
                              "нет ничего").
               IF  shFilial NE "0500" 
               AND shFilial NE "0300" 
               AND shFilial NE "0000" THEN
                  hBuffer:BUFFER-DELETE() NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.

         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*Отбор счетов для отменяющих: есть экспорт и импорт на него не ошибочный     */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteRefusal:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.
      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).
         IF mReform THEN                /*если пользователь ответил ДА*/
         DO:                            /*на переформирование по фильтру*/
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vAcct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE
         DO:
            ASSIGN
               vItem = LastTaxImp2(vAcct,           vCurrency,
                                    "TaxExpCommon5X","TaxImp")
               vOpCl = ENTRY(1,vItem,CHR(1))
               vItem = ENTRY(2,vItem,CHR(1))
            .
            IF NOT {assigned vOpCl} THEN    /*если нет экспорта или*/
            DO:
              hBuffer:BUFFER-DELETE() NO-ERROR.   /*то удаляем счет из выборки*/
              IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
            ELSE
               IF      {assigned vOpCl} AND    /*экспорт есть, но */
                  (NOT {assigned vItem} OR     /*импорта нет или  */
                       INDEX(vItem,"E") EQ LENGTH(vItem)) /*импорт в ошибке  */
            THEN   
            DO:                                /*(SFE,SBE),       */
              hBuffer:BUFFER-DELETE() NO-ERROR.   /*то удаляем счет из выборки*/
              IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Запрет разных сообщений по одному счету в один день                        */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteBan:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

/*       Cчета, по которым в текущем дне была отправка другого вида сообщения, должны быть исключены из отбора. */

         vFlagFind = NO.
         FOR EACH PackObject WHERE
                  PackObject.file-name EQ "acct"
              AND PackObject.surrogate EQ vAcct + "," + vCurrency
                  NO-LOCK,
            FIRST Packet WHERE
                  Packet.PacketID   EQ PackObject.PacketID
              AND Packet.Kind       EQ "TaxExpCommon5X"
                  NO-LOCK,
             EACH Seance WHERE 
                  Seance.SeanceID   EQ Packet.SeanceID
              AND Seance.op-kind    NE mOpKind   
              AND Seance.SeanceDate EQ TODAY
                  NO-LOCK,
            FIRST FileExch   WHERE
                  FileExch.FileExchID EQ packet.FileExchID
              AND FileExch.name BEGINS "SFC"
            NO-LOCK:

               ASSIGN
                  vFlagFind = YES
               NO-ERROR.
               LEAVE.
         END.
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ac-order.p","vAcct:" + vAcct + 
                                      " mOpKind:" + mOpKind +
                                      " vFlagFind:" + STRING(vFlagFind)).
         &ENDIF
         IF vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.

         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
&IF DEFINED(IS-DEBUG) &THEN
PROCEDURE PrintFilterLog:
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.
   DEFINE VAR vItm   AS INT64  NO-UNDO.
   DEFINE VAR hField AS handle   NO-UNDO.

   DEFINE VAR hQuery  AS handle NO-UNDO.

   CREATE QUERY hQuery.
   hQuery:add-buffer(hBuffer).
   IF NOT hQuery:query-prepare("for EACH " + hBuffer:name) THEN MESSAGE "ERROR 1" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   IF NOT hQuery:query-open()                              THEN MESSAGE "ERROR 2" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   hQuery:get-first().
   DO WHILE hBuffer:AVAILABLE:
      DO vItm = 1 TO hBuffer:num-fields:
         hField = hBuffer:buffer-field(vItm).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("PrintFilterLog",string(hField:Name,"x(30)") + " " +
                                          hField:buffer-value).
         &ENDIF
      END.
      hQuery:get-next().
   END.
   hQuery:query-close().

   DELETE object hQuery.
END PROCEDURE.
&ENDIF
/******************************************************************************/


PROCEDURE DelFilterTable.
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.

   IF valid-handle(hBuffer) THEN 
      hBuffer:empty-temp-table() NO-ERROR.

END PROCEDURE.
/* $LINTFILE='ac-order.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTUSER='mkv' */
/* $LINTDATE='15/12/2016 11:53:43.227+03:00' */
/*prosignIoZAWuY4+Ri1eG0vLLgbWA*/