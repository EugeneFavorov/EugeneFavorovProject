/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-sbrf.p
      Comment: Библиотека работы с SBRF.
   Parameters: нет
      Created: 09.10.2015 ahra   
     Modified:    
     Modified: 26.05.2006 NIK PROGRESS v10.
     Modified: 17/05/2006 NIK Реструктуризация pp-pack.p
     Modified: 17/08/2006 NIK Изменение прототипа GetEXCHOpKind
*/
{globals.i}

DEFINE NEW GLOBAL SHARED STREAM debug-stream.

{g-trans.equ}
{exchange.equ}

{intrface.get xclass}
{intrface.get strng}
{intrface.get instrum}

{intrface.get tmess}
{intrface.get trans}
{intrface.get pbase}

{intrface.get exch}
{intrface.get pack}
{intrface.get rfrnc}

{intrface.get xrkc}
{intrface.get wop}
{intrface.get ewop}
{intrface.get echck}

{sbrf.def}

{pfuncdef  
&DefLib="sbrf"
&Description="Библиотека работы с SBRF"}

DEFINE VAR mStatusFU    AS CHAR           NO-UNDO.
DEFINE VAR mStatusEnd   AS CHAR           NO-UNDO.
DEFINE VAR mReferIncr   AS INT64 INIT 0 NO-UNDO.
DEFINE VAR mSK          AS CHAR           NO-UNDO.

DEFINE VARIABLE mCurrency  AS CHARACTER NO-UNDO.

ASSIGN
   mStatusEnd = FGetSetting("МЦИ","st-done","√")
   mStatusFU  = FGetSetting("МЦИ","st-fu","ФУ")
.

DEFINE TEMP-TABLE ttSBRFTag NO-UNDO
            FIELD Class-Code     AS CHAR
            FIELD XAttr-Code     AS CHAR
            FIELD ObjectClass    AS CHAR
            FIELD ObjectField    AS CHAR
            FIELD ObjectHandle   AS handle
            FIELD ObjectIndex    AS INT64
            FIELD DataType       AS CHAR
            FIELD DataFormat     AS CHAR
            FIELD Order          AS INTEGER
            INDEX Class Class-Code XAttr-Code
            INDEX Order Class-Code Order
.

/*----------------------------------------------------------------------------*/
/* Возвращает требуемый идентификатор банка по используемому                  */
/*----------------------------------------------------------------------------*/
FUNCTION GetBankCodeSBRF3ByMFO CHAR (INPUT  iBankCode AS CHAR):

   DEFINE VAR    vBankCode    AS    CHAR INIT ? NO-UNDO.
   DEFINE VAR    vUni         AS    CHAR INIT ? NO-UNDO.
   DEFINE BUFFER code         FOR   code.
   DEFINE BUFFER banks        FOR   banks.
   DEFINE BUFFER banks-code   FOR   banks-code.

   DEFINE VARIABLE iNeedCodeType AS CHAR INIT "SBRF3" NO-UNDO.
   DEFINE VARIABLE iBankCodeType AS CHAR INIT "МФО-9" NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN GetBank IN h_base (BUFFER banks,
                             BUFFER banks-code,
                             INPUT  iBankCode,
                             INPUT  iBankCodeType,
                             INPUT  YES).

      IF NOT AVAILABLE(banks) THEN DO:
         RUN Fill-SysMes("","ComnExc08","","%s=" + GetNullStr(iBankCodeType) +
                                           "%s=" + GetNullStr(iBankCode)     +
                                           "%s=" + program-name(2)           +
                                           "%s=" + program-name(3)           +
                                           "%s=" + program-name(4)).
         LEAVE MAIN.
      END.

      FIND FIRST banks-code WHERE
                 banks-code.bank-id        EQ banks.bank-id
             AND banks-code.bank-code-type EQ iNeedCodeType
                 NO-LOCK NO-ERROR.

      IF NOT AVAILABLE(banks-code) THEN DO:
         vBankCode = GetXattrValueEx("banks", STRING(banks.bank-id),"sbrf3",?).
         IF vBankCode EQ ? THEN DO:
            RUN Fill-SysMes("","ComnExc09","","%s=" + GetNullStr(banks.name) +
                                              "%s=" + GetNullStr(iNeedCodeType)).
         END.
         LEAVE MAIN.
      END.

      vBankCode = banks-code.bank-code.
   END.

   RETURN vBankCode.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Возвращает наименование банка по коду                                      */
/*----------------------------------------------------------------------------*/
FUNCTION GetBankName CHAR (INPUT  iBankCodeType AS CHAR,
                           INPUT  iBankCode     AS CHAR):

   DEFINE VAR    vBankName    AS    CHAR INIT ? NO-UNDO.
   DEFINE BUFFER banks        FOR   banks.
   DEFINE BUFFER banks-code   FOR   banks-code.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN GetBank IN h_base (BUFFER banks,
                             BUFFER banks-code,
                             INPUT  iBankCode,
                             INPUT  iBankCodeType,
                             INPUT  YES).

      IF NOT AVAILABLE(banks) THEN DO:
         RUN Fill-SysMes("","ComnExc08","","%s=" + GetNullStr(iBankCodeType) +
                                           "%s=" + GetNullStr(iBankCode)     +
                                           "%s=" + program-name(2)           +
                                           "%s=" + program-name(3)           +
                                           "%s=" + program-name(4)).
         LEAVE MAIN.
      END.

      vBankName = banks.name.
   END.

   RETURN vBankName.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Создание исходящего платежного сообщения для терминала SBRF-3              */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFExportCreate:
   DEFINE INPUT PARAMETER iClass    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iInstance AS handle   NO-UNDO.

   DEFINE BUFFER bCode FOR Code.
   DEFINE VAR vFlagSet     AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR hWOP         AS handle         NO-UNDO.
   DEFINE VAR vOp          AS INT64          NO-UNDO.
   DEFINE VAR vOpEntry     AS INT64          NO-UNDO.
   DEFINE VAR vDirect      AS CHAR           NO-UNDO.
   DEFINE VAR vEXCHClass   AS CHAR           NO-UNDO.
   DEFINE VAR vRefClass    AS CHAR           NO-UNDO.
   DEFINE VAR vReference   AS INT64          NO-UNDO.
   DEFINE VAR vDocNum     AS CHAR           NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hWOp        = iInstance:default-buffer-handle
         vOp         = hWOP:buffer-field("op"):buffer-value
         vOpEntry    = hWOP:buffer-field("op-entry"):buffer-value
         vDirect     = hWOP:buffer-field("Direct"):buffer-value
      NO-ERROR. {&ON-ERROR}

      vEXCHClass  = GetTAGFormat(0,{&DIR-EXPORT},BUFFER mail-user).

      FOR FIRST op-entry WHERE
                op-entry.op-entry EQ vOpEntry
            AND op-entry.op       EQ vOp
                NO-LOCK,
          FIRST op OF op-entry
                NO-LOCK:

         hWOp:buffer-copy(BUFFER op:handle)        NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

         hWOp:buffer-copy(BUFFER op-entry:handle)  NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

         RUN WOPExportCompose (BUFFER op,
                               BUFFER op-entry,
                               INPUT  vDirect,
                               INPUT  hWOp).

         IF NOT EXCH-MSGBuff(INPUT  vEXCHClass,
                             BUFFER bCode) THEN
            UNDO MAIN, RETRY MAIN.

         vRefClass = bCode.Misc[{&RKC-BEGIN}].
         ASSIGN
            vReference = INT64(ReferenceGetLast(vRefClass,op.op-date))
            mReferIncr = mReferIncr + 1
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN vReference = 0.

         mSK = GetXAttrEx(vEXCHClass,"SK","INITIAL").
         ASSIGN
            hWOP:buffer-field("bank-corr-acct-send"):buffer-value = mSK WHEN mSK NE "" /* SK */
            hWOP:buffer-field("acct-send"):buffer-value = DelFilFromAcct(op-entry.acct-db)
            hWOp:buffer-field("SendREF"):buffer-value   = ReferenceFormatValue(vRefClass,
                                                                              string(vReference + mReferIncr))
            hWOP:buffer-field("SendID"):buffer-value   = SBRFGetBankCode(0)
            hWOP:buffer-field("SendDate"):buffer-value = string(op.op-date,"99/99/9999")
            hWOP:buffer-field("RecvID"):buffer-value   = "6786340000"  /* GetBankCodeSBRF3ByMFO(hWOP:buffer-field("bank-code-rec"):buffer-value) */
            hWOP:buffer-field("amt-rub"):buffer-value  = DEC(hWOP:buffer-field("amt-rub"):buffer-value) * 100
            hWOP:buffer-field("ins-date"):buffer-value =  TODAY /* op.ins-date */

            hWOP:buffer-field("type"):buffer-value           = IF GetCodeMisc("Маршруты",op-entry.type,7) EQ "4" THEN "14" ELSE "13"  /* SC2 */
            hWOP:buffer-field("mess-type"):buffer-value      = GetXAttrEx(vEXCHClass,"MT","INITIAL")  /* MT3 */
            hWOP:buffer-field("bank-name-rec"):buffer-value  = GetBankName("МФО-9",hWOP:buffer-field("bank-code-rec"):buffer-value)    /* BN */
            hWOP:buffer-field("bank-name-send"):buffer-value = GetBankName("МФО-9",hWOP:buffer-field("bank-code-send"):buffer-value)   /* SB */

            hWOP:buffer-field("currency-send"):buffer-value  = GetXAttrEx(vEXCHClass,"CU","INITIAL")   /* CU3 */
            hWOP:buffer-field("rs-send"):buffer-value        = GetXAttrEx(vEXCHClass,"SS","INITIAL")   /* SS6 */
            hWOP:buffer-field("rs-rec"):buffer-value         = GetXAttrEx(vEXCHClass,"RF","INITIAL")   /* SS6 */
            vDocNum                                          = IF LENGTH(op.doc-num) GT 6 THEN SUBSTRING(op.doc-num,LENGTH(op.doc-num) - 5)
                                                                                          ELSE op.doc-num                    
            hWOP:buffer-field("NumDatOrd"):buffer-value      = "000"  +
                                                               STRING(INT64(vDocNum),"999999")   + 
                                                               REPLACE(STRING(op.doc-date,"99/99/99"),"/","") +
                                                               STRING(INT64(op.order-pay),"9")           /* IN16 */

         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("wop-crt.p","mReference:" + string(vReference + 1)).
         &ENDIF

      END.                                       /* FOR FIRST op-entry WHERE  */

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Регистрация исходящего сообщения для терминала SBRF-3                      */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFPacketCreate:
   DEFINE INPUT PARAMETER iClass    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iInstance AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.
   DEFINE VAR hPack        AS handle            NO-UNDO.
   DEFINE VAR hWOP         AS handle            NO-UNDO.
   DEFINE VAR mSeanceID    AS INT64           NO-UNDO.
   DEFINE VAR vPacketID    AS INT64           NO-UNDO.
   DEFINE VAR vKind        AS CHAR              NO-UNDO.
   DEFINE VAR vFileName    AS CHAR              NO-UNDO.
   DEFINE VAR vSurrogate   AS CHAR              NO-UNDO.
   DEFINE VAR vEXCHClass   AS CHAR              NO-UNDO.
   DEFINE VAR vRetryErr    AS CHAR              NO-UNDO.
   DEFINE VAR vErrorClass  AS CHAR              NO-UNDO.
   DEFINE VAR vErrorList   AS CHAR              NO-UNDO.
   DEFINE VAR vResult      AS CHAR              NO-UNDO.
   DEFINE VAR vBuffer      AS CHAR              NO-UNDO.
   DEFINE VAR vSendREF     AS CHAR              NO-UNDO.
   DEFINE VAR vFake        AS CHAR              NO-UNDO.

   DEF BUFFER TagXattr FOR xattr.

   &SCOP RETRY-ERROR vRetryErr
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hPack       = iInstance:default-buffer-handle
         vKind       = hPack:buffer-field("Kind"):buffer-value
         mSeanceID   = hPack:buffer-field("SeanceID"):buffer-value
      NO-ERROR. {&ON-ERROR}

      hWOP = GetTransObject(vKind).              /* Указатель на документ     */
      ASSIGN
         hWOP       = hWOP:default-buffer-handle
         vFileName  = hWOP:buffer-field("File-Name"):buffer-value
         vSurrogate = hWOP:buffer-field("Surrogate"):buffer-value
         vSendREF   = hWOP:buffer-field("SendREF"):buffer-value
      NO-ERROR. {&ON-ERROR}

      FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
      vEXCHClass  = GetTAGFormat(mSeanceID,      /* Формат обмена             */
                                {&DIR-EXPORT},
                                BUFFER mail-user).

                                       /* реквизит "bis-type" влияет на vEXCHClass */
      RUN GetXAttr IN h_xclass (vEXCHClass, "DT", BUFFER TagXattr).
      IF AVAIL TagXattr THEN DO:

         IF {assigned Tagxattr.INITIAL} THEN 
         ASSIGN
            hWOP:buffer-field("bis-type"):buffer-value  = TagXattr.INITIAL
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.

      RUN SBRFPrepareTag (vEXCHClass).           /* Кэш тегов                 */

      DISABLE triggers FOR load OF Packet.
      DISABLE triggers FOR load OF PackObject.
      DISABLE triggers FOR load OF PacketText.

/*--------------- Создаем запись о пакете экспорта и все его связи и ссылки --*/
MAKE:
      DO TRANSACTION ON ERROR UNDO MAKE, RETRY MAKE:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         IF NOT EXCH-MSGBuff(INPUT  vEXCHClass,
                             BUFFER bCode) THEN
            UNDO MAKE, RETRY MAKE.

         vSendREF   = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                           vSendREF).
/*--------------------------------------------------------- Текст сообщения --*/
         ASSIGN
            vResult = " "                                                               +
                      GetXattrValueEx("doc-type",hWOP:buffer-field("Doc-Type"):buffer-value,"SBRFSign","0B1") +
                      string(DATE(hWOP:buffer-field("SendDate"):buffer-value),"999999") +
                      string(hWOP:buffer-field("SendID"):buffer-value,"x(10)")          +
                      vSendREF
         NO-ERROR. {&ON-ERROR}

         RUN SBRFExportTag (INPUT  hWOP,
                            INPUT  vEXCHClass,
                            OUTPUT vBuffer).
         vResult = vResult + vBuffer + "|EE:".

/*------------------------------------------------------ Запись о сообщении --*/
         RUN PacketCreate (INPUT  mSeanceID,
                           INPUT  -1,
                           INPUT  mail-user.mail-user-num,
                           INPUT  vKind,
                           OUTPUT vPacketID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAKE, RETRY MAKE.

         IF NOT UpdateSigns(INPUT "Packet",
                            INPUT STRING(vPacketID),
                            INPUT "op-kind",
                            INPUT TRNSettingValue("","OpKindEdit",""),
                            INPUT ?) THEN
            UNDO MAKE, RETRY MAKE.
/*----------------------------------------------- Связь с объектом экспорта --*/
         RUN PacketCreateLink (vPacketID,
                               vFileName,
                               vSurrogate,
                               ENTRY(1,bCode.Description[1])) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAKE, RETRY MAKE.

         RUN PacketCreateRef  (gend-date,
                               vPacketID,
                               bCode.Misc[{&RKC-BEGIN}],
                               vSendREF).
         mReferIncr = 0.
/*----------------------------------------------------- Ошибки формирования --*/
         ASSIGN
            vErrorClass = hWOP:buffer-field("ErrorClass"):buffer-value
            vErrorList  = hWOP:buffer-field("ErrorList" ):buffer-value
         NO-ERROR.
         IF NOT ERROR-STATUS:ERROR AND
            {assigned vErrorClass} AND
            {assigned vErrorList } THEN DO:

            RUN PacketSetError (INPUT  vPacketID,
                                INPUT  vErrorClass,
                                INPUT  vErrorList,
                                OUTPUT vFake) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO MAKE, RETRY MAKE.
         END.

         vBuffer = vResult.
         RUN PacketTextClear (vPacketID).
         vResult = "".
         RUN PacketTextKeep  (vPacketID, vBuffer, INPUT-OUTPUT vResult).
         RUN PacketTextSave  (vPacketID, vResult).

      END.                                       /* MAKE: DO TRANSACTION      */

      vFlagSet = YES.
   END.                                          /* MAIN: DO ... :            */

   {doreturn.i vFlagSet}

   &undefine RETRY-ERROR
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Создание итогового исходящего сообщения для терминала SBRF-3               */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFPacketExport:
   DEFINE INPUT PARAMETER iClass    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iInstance AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.

   DEFINE BUFFER onePacket FOR Packet.           /* One document Packet       */
   DEFINE BUFFER allPacket FOR Packet.           /* All document Packet       */

   DEFINE VAR hPacket     AS handle   NO-UNDO.
   DEFINE VAR mSeanceID   AS INT64    NO-UNDO.
   DEFINE VAR vPacketID   AS INT64    NO-UNDO.
   DEFINE VAR vState      AS CHAR     NO-UNDO.
   DEFINE VAR vKind       AS CHAR     NO-UNDO.
   DEFINE VAR vOneOp      AS CHAR     NO-UNDO.
   DEFINE VAR vAllOp      AS CHAR     NO-UNDO.
   DEFINE VAR vSBRFSendID AS CHAR     NO-UNDO.
   DEFINE VAR vSBRFRecID  AS CHAR     NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hPacket     = iInstance:default-buffer-handle
         vState      = hPacket:buffer-field("State"):buffer-value
         vKind       = hPacket:buffer-field("Kind"):buffer-value
         mSeanceID   = hPacket:buffer-field("SeanceID"):buffer-value
         vSBRFSendID = TRNSettingValue("","SBRFSendID","000")
         vSBRFRecID  = TRNSettingValue("","SBRFRecID","000")
      NO-ERROR. {&ON-ERROR}

      {getsncmlu.i MAIN exp}                     /* FIND seance AND mail-user */

      FIND FIRST onePacket WHERE
                 onePacket.SeanceID EQ mSeanceID
             AND onePacket.State    EQ vState 
                 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(onePacket) THEN DO:
         vFlagSet = NO.
         UNDO MAIN, RETRY MAIN.
      END.

      FOR EACH onePacket WHERE
               onePacket.SeanceID EQ mSeanceID
           AND onePacket.State    EQ {&STATE-CRT}
               EXCLUSIVE-LOCK:
         DELETE onePacket.
      END.

MOVE:
      DO TRANSACTION ON ERROR UNDO MOVE, RETRY MOVE:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         CREATE allPacket.                       /* Создаем пакет экспорта    */
         ASSIGN vPacketID               = next-value(pack-id)
                allPacket.PacketID      = vPacketID
                allPacket.AbonentID     = -1
                allPacket.mail-user-num = mail-user.mail-user-num
                allPacket.SeanceID      = mSeanceID
                allPacket.State         = {&STATE-CRT}
                allPacket.PackDate      = today
                allPacket.mail-format   = mail-user.mail-format
                allPacket.Kind          = vKind
                allPacket.Class-Code    = iClass
                allPacket.ParentID      = 0
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MOVE, RETRY MOVE.

         vAllOp = "".
         RUN PacketTextClear (vPacketID).
         RUN PacketTextKeep (INPUT        vPacketID,
                             INPUT        "SBRF3:" + vSBRFRecID + vSBRFSendID + "~r~n",
                             INPUT-OUTPUT vAllOp).    
READ-LIST:
         FOR EACH onePacket WHERE
                  onePacket.SeanceID EQ mSeanceID
              AND onePacket.State    EQ vState
                  EXCLUSIVE-LOCK:

            IF onePacket.PacketID EQ vPacketID THEN NEXT READ-LIST.

            IF NOT PacketReadOpen(onePacket.PacketID) THEN UNDO MOVE, RETRY MOVE.

MESS-READ:
            REPEAT:
               IF NOT PacketReadLine(onePacket.PacketID, OUTPUT vOneOp) THEN LEAVE MESS-READ.
               RUN PacketTextKeep (INPUT        vPacketID,
                                   INPUT        vOneOp + "~r~n",
                                   INPUT-OUTPUT vAllOp).
            END.                                 /* MESS-READ:                */

            PacketReadClose(onePacket.PacketID).

            ASSIGN onePacket.ParentID = allPacket.PacketID
                   onePacket.State    = {&STATE-FIN}.
         END.                                    /* MOVE:                     */

         RUN PacketTextKeep (INPUT        vPacketID,
                             INPUT        "EOF~r~n",
                             INPUT-OUTPUT vAllOp).
         RUN PacketTextSave  (vPacketID, vAllOp).

         vFlagSet = YES.
      END.

   END.                                          /* MAIN:                     */

   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*  Импорт одного платежного сообщения                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFPacketImport:
   DEFINE INPUT PARAMETER iClass    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iInstance AS handle   NO-UNDO.
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iBuffer   AS CHAR     NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VAR hWOP      AS handle   NO-UNDO.
   DEFINE VAR vSign     AS CHAR     NO-UNDO.
   DEFINE VAR vSeanceID AS INT64  NO-UNDO.

   DEFINE VAR vNumDat   AS CHAR     NO-UNDO.
   DEFINE VAR vOpDate   AS DATE     NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFPacketImport","iBuffer:" + GetNullStr(iBuffer)).
      &ENDIF

      hWOP = iInstance:default-buffer-handle.
      RUN SBRFIdentImport (INPUT hWOP, INPUT iBuffer).

/*------------------------------------------ Сообщение было принято ранее ? --*/
      RUN XRKCPacketCheck (iFormat, iInstance) NO-ERROR.
      IF ERROR-STATUS:ERROR      THEN DO:
         vFlagSet = YES.
         hWOP:buffer-delete().
         LEAVE MAIN.
      END.

      CASE RETURN-VALUE:
         WHEN "SKIP"  THEN.
         OTHERWISE DO:

            RUN SBRFImportTag (INPUT hWOP,
                               INPUT iFormat,
                               INPUT SUBSTRING(iBuffer,27)).

            ASSIGN
               vNumDat   = hWOP:buffer-field("NumDatOrd"):buffer-value
               vSeanceID = hWOP:buffer-field("SeanceID"):buffer-value
               hWOP:buffer-field("amt-rub"):buffer-value  = DEC(hWOP:buffer-field("amt-rub"):buffer-value) / 100
            NO-ERROR. {&ON-ERROR}

            RUN SBRFCalcAmount (INPUT hWOP).     /* Сумма - Валюта            */

            ASSIGN
               hWOP:buffer-field("doc-num"):buffer-value   = SUBSTRING(vNumDat,4,6)
               hWOP:buffer-field("doc-date"):buffer-value  = string(DATE(SUBSTRING(vNumDat,10,2) + "." +
                                                                         SUBSTRING(vNumDat,12,2) + "." +
                                                                         SUBSTRING(vNumDat,14,2)))
               hWOP:buffer-field("order-pay"):buffer-value = string(INT64(SUBSTRING(vNumDat,16)))

               hWOP:buffer-field("bis-type"):buffer-value  =
                                    hWOP:buffer-field("bis-type"):buffer-value +
                                    hWOP:buffer-field("bis-add"):buffer-value
               hWOP:buffer-field("op-kind"):buffer-value  = GetEXCHOpKind(vSeanceID,
                                                                          {&DIR-IMPORT},
                                                                   buffer mail-user)
               hWOP:buffer-field("contract-date"):buffer-value  =  STRING(TODAY)
               hWOP:buffer-field("senddate"):buffer-value       =  STRING(TODAY)
               hWOP:buffer-field("SBRFID"):buffer-value         =  SUBSTRING(iBuffer,5,22)
                           NO-ERROR. {&ON-ERROR}
             
                                       /* импорт подтверждений */ 

            IF CAN-DO("100,101",hWOP:buffer-field("bis-type"):buffer-value) THEN DO:
               RUN SBRFConfirm (hWOP,
                                iFormat).

               vFlagSet = YES.
               hWOP:buffer-delete().
               LEAVE MAIN.
            END.

            IF CAN-DO("601,602,603,604,605",hWOP:buffer-field("bis-type"):buffer-value) THEN DO:
               RUN SBRFDenial (hWOP,
                                iFormat,
                                iBuffer).

               vFlagSet = YES.
               hWOP:buffer-delete().
               LEAVE MAIN.
            END.

            IF NOT {assigned hWOP:buffer-field('op-kind'):buffer-value} THEN DO:
               RUN Fill-SysMes("","SBRF03","","%s=" + hWOP:buffer-field("bis-type"):buffer-value +
                                              "%s=" + GetBaseOpKind()).
               hWOP:buffer-delete().
            END.
         END.
      END CASE.

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFStListImport:
   DEFINE INPUT        PARAMETER iClass      AS CHAR     NO-UNDO.
   DEFINE INPUT        PARAMETER hWOP        AS handle   NO-UNDO.
   DEFINE INPUT        PARAMETER hHED        AS handle   NO-UNDO.
   DEFINE INPUT        PARAMETER iFormat     AS CHAR     NO-UNDO.
   DEFINE INPUT        PARAMETER iBuffer     AS CHAR     NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pFlagPos    AS CHAR     NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VAR vSendID   AS CHAR     NO-UNDO.
   DEFINE VAR vSign     AS CHAR     NO-UNDO.
   DEFINE VAR vSeanceID AS INT64  NO-UNDO.

   DEFINE VAR vCorrAcct AS CHAR     NO-UNDO.
   DEFINE VAR vRecvAcct AS CHAR     NO-UNDO.
   DEFINE VAR vBisType  AS CHAR     NO-UNDO.
   DEFINE VAR vCorrAcctSend AS CHAR     NO-UNDO.   

   DEFINE VAR vVal      AS CHAR     NO-UNDO.
   DEFINE VAR vTag      AS CHAR     NO-UNDO.
   DEFINE VAR vDir      AS INT64  NO-UNDO.

   DEFINE VARIABLE vBankID       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBankIDClass  AS CHARACTER NO-UNDO.  
   
   DEFINE BUFFER banks FOR banks.
   DEFINE BUFFER banks-corr FOR banks-corr.

/*============================================================================*/
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFStListImport","pFlagPos:" + pFlagPos           +
                                         " iBuffer:" + GetNullStr(iBuffer)).
      &ENDIF

      CASE pFlagPos:
         WHEN "S" THEN DO:                       /* Выписка уже принята       */
            vFlagSet = YES.
            LEAVE MAIN.
         END.
         WHEN "" THEN DO:                        /* Идентификация             */
            RUN SBRFIdentImport (INPUT hHED, INPUT iBuffer).

            RUN XRKCPacketCheck (iFormat, hHED:table-handle) NO-ERROR.
            IF ERROR-STATUS:ERROR      THEN DO:
               pFlagPos = "S".
               hWOP:buffer-delete().
            END.
            ELSE
               pFlagPos = "H".

            vFlagSet = YES.
            LEAVE MAIN.
         END.
         WHEN "H" THEN DO:
            ASSIGN
               vTag = TRIM(SUBSTRING(iBuffer,1,20))
               vVal = TRIM(SUBSTRING(iBuffer,21))
            .
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("SBRFStListImport","vTag:" + GetNullStr(vTag) +
                                              " vVal:" + GetNullStr(vVal)).
            &ENDIF
            CASE vTag:
               WHEN ""                    THEN.
               WHEN "Отправитель"         THEN
                  hHED:buffer-field("SendID"):buffer-value   = vVal.
               WHEN "Получатель"          THEN
                  hHED:buffer-field("RecvID"):buffer-value   = vVal.
               WHEN "Тип выписки"         THEN.
               WHEN "Код выписки"         THEN.
               WHEN "Номер выписки"       THEN
                  hHED:buffer-field("SendREF"):buffer-value  = vVal.
               WHEN "Код валюты"          THEN
               DO:
                  mCurrency = vVal .
                  hHED:buffer-field("currency"):buffer-value = vVal.
               END.
               WHEN "За период"           THEN.
               WHEN "Дата предыд.выписки" THEN.
               WHEN "Отчет сформирован"   THEN
                  hHED:buffer-field("SendDate"):buffer-value = DATE(vVal) NO-ERROR.
               WHEN "Раздел"              THEN
                  IF INT64(ENTRY(2,iBuffer," ")) NE 1 THEN DO:
                     RUN Fill-SysMes("","","","%s=" + iBuffer).
                     UNDO MAIN, RETRY MAIN.
                  END.
               WHEN "Счет №"              THEN DO:
                  FOR FIRST c-nostro WHERE
                            c-nostro.corr-acct EQ vVal
                            NO-LOCK:
                     hHED:buffer-field("acct-db"):buffer-value = c-nostro.acct.
                  END.
               END.
               WHEN "Входящее сальдо"     THEN.
               WHEN "№      Вид оп. Кл.№" THEN DO: /* Дальше - документы      */
                  hWOP:buffer-delete().
                  pFlagPos = "D".
               END.
            END CASE.
         END.
         WHEN "D" THEN DO:                       /* Документы                 */
            IF iBuffer BEGINS "Операции" THEN DO:     /* Раздел окончен       */
               hWOP:buffer-delete().
               pFlagPos = "E".
               vFlagSet = YES.
               LEAVE MAIN.
            END.

            ASSIGN vDir = INT64(SUBSTRING(iBuffer,76,1)) NO-ERROR. {&ON-ERROR}

            CASE vDir:
               WHEN 0 THEN ASSIGN                /* Ответные документы        */
                  hWOP:buffer-field("SendDate"):buffer-value  = string(DATE(SUBSTRING(iBuffer,99,6)))
                  hWOP:buffer-field("SendID"):buffer-value    = SUBSTRING(iBuffer,106,10)
                  hWOP:buffer-field("SendREF"):buffer-value   = SUBSTRING(iBuffer,117,6)
               NO-ERROR.
               WHEN 1 THEN ASSIGN                /* Начальные документы       */
                  hWOP:buffer-field("RecvDate"):buffer-value  = string(DATE(SUBSTRING(iBuffer,99,6)))
                  hWOP:buffer-field("RecvID"):buffer-value    = SUBSTRING(iBuffer,106,10)
                  hWOP:buffer-field("RecvREF"):buffer-value   = SUBSTRING(iBuffer,117,6)
               NO-ERROR.
            END CASE.
            {&ON-ERROR}

/*------------------------------------------- Выполнение квитовки документа --*/
            RUN SPRFPacketConfirm (vDir,
                                   iFormat,
                                   hWOP).
            IF NOT hWOP:AVAILABLE THEN DO:       /* Документ сквитован        */
               vFlagSet = YES.
               LEAVE MAIN.
            END.

/*------------------ "Досоздание" документа, который не найден в нашей базе --*/
            ASSIGN
               vCorrAcct = SUBSTRING(iBuffer,34,20)
               vRecvAcct = SUBSTRING(iBuffer,55,20)
               vBisType  = SUBSTRING(iBuffer,124,3)
            .

            vBankID = GetNeedBankCode("SBRF3",SUBSTRING(iBuffer,106,10),"МФО-9") . 
          
            IF NOT {assigned vBankID} THEN
            DO:
               ASSIGN 
                  vBankID = SUBSTRING(iBuffer,106,10)
                  vBankIDClass = "SBRF3"
                  hWOP:buffer-field("ErrorList" ):buffer-value = 
                     hWOP:buffer-field("ErrorList" ):buffer-value + "," + "sbrf01"
                  hWOP:buffer-field("contract-date"):buffer-value        = today
               NO-ERROR.            
            END.
            ELSE 
            DO:
               {getbank.i "banks" "vBankID"}  
               IF AVAILABLE banks THEN
               DO:
                  FIND FIRST banks-corr WHERE
                     banks-corr.bank-corr EQ banks.bank-id
                     AND CAN-FIND(FIRST banks OF banks-corr WHERE banks.flag-rkc)
                     NO-LOCK NO-ERROR.
                  IF AVAILABLE banks-corr THEN
                     vCorrAcctSend = banks-corr.corr-acct.                                    
               END.           
            END. 

            CASE vDir:
               WHEN 0 THEN ASSIGN                /* Кредитование корсчета     */
                  hWOP:buffer-field("Direct"):buffer-value         = "Ответные"
                  hWOP:buffer-field("bis-type"):buffer-value       = SUBSTRING(iBuffer,124,3) + "-R"
                  hWOP:buffer-field("acct-send"):buffer-value      = vCorrAcct
                  hWOP:buffer-field("acct-rec"):buffer-value       = vRecvAcct
                  hWOP:buffer-field("rs-send"):buffer-value        = vBankIDClass                 
                  hWOP:buffer-field("bank-code-send"):buffer-value = vBankID
                  hWOP:buffer-field("bank-corr-acct-send"):buffer-value = vCorrAcctSend
               NO-ERROR.
               WHEN 1 THEN DO:                   /* Дебетование корсчета      */
                  CASE vBisType:
                     WHEN "004" THEN ASSIGN      /* Дебетовое авизо           */
                        hWOP:buffer-field("Direct"):buffer-value         = "Начальные"
                        hWOP:buffer-field("bis-type"):buffer-value       = vBisType + "-N"
                        hWOP:buffer-field("ben-acct"):buffer-value       = vCorrAcct
                        hWOP:buffer-field("acct-cr"):buffer-value        = hWOP:buffer-field("acct-db"):buffer-value
                        hWOP:buffer-field("acct-send"):buffer-value      = ""
                        hWOP:buffer-field("acct-db"):buffer-value        = ""
                        hWOP:buffer-field("bank-code-send"):buffer-value = ""
                        hWOP:buffer-field("rs-rec"):buffer-value        = vBankIDClass                  
                        hWOP:buffer-field("bank-code-rec"):buffer-value = vBankID 
                     NO-ERROR.
                     WHEN "001" THEN ASSIGN      /* Списание корсчета         */
                        hWOP:buffer-field("Direct"):buffer-value         = "Начальные"
                        hWOP:buffer-field("bis-type"):buffer-value       = vBisType + "-N"
                        hWOP:buffer-field("ben-acct"):buffer-value       = vCorrAcct
                        hWOP:buffer-field("acct-rec"):buffer-value       = vRecvAcct
                        hWOP:buffer-field("acct-cr"):buffer-value        = vRecvAcct
                        hWOP:buffer-field("acct-db"):buffer-value        = ""
                        hWOP:buffer-field("acct-send"):buffer-value      = ""
                        hWOP:buffer-field("bank-code-send"):buffer-value = ""
                        hWOP:buffer-field("rs-rec"):buffer-value        = vBankIDClass                  
                        hWOP:buffer-field("bank-code-rec"):buffer-value = vBankID 
                     NO-ERROR.
                  END CASE.
               END.                              /* WHEN 1 THEN DO:           */
            END CASE.

            ASSIGN
               vSeanceID = INT64(hWOP:buffer-field("SeanceID"):buffer-value)
               hWOP:buffer-field("doc-type"):buffer-value  = SUBSTRING(iBuffer,8,2)
               hWOP:buffer-field("doc-num"):buffer-value   = TRIM(SUBSTRING(iBuffer,11,11))
               hWOP:buffer-field("doc-date"):buffer-value  = SUBSTRING(iBuffer,23,10)
               hWOP:buffer-field("op-date"):buffer-value   = SUBSTRING(iBuffer,23,10)
               hWOP:buffer-field("amt-rub"):buffer-value   = SUBSTRING(iBuffer,78,20)
               hWOP:buffer-field("details"):buffer-value   = SUBSTRING(iBuffer,151)
               hWOP:buffer-field("op-status"):buffer-value = mStatusFU
               hWOP:buffer-field("op-kind"):buffer-value   = GetEXCHOpKind(vSeanceID,
                                                                           {&DIR-IMPORT},
                                                                     buffer mail-user)
            NO-ERROR. {&ON-ERROR}

            RUN SBRFCalcAmount (INPUT hWOP).     /* Сумма - Валюта            */

            IF hWOP:buffer-field("op-kind"):buffer-value EQ "" THEN DO:
               RUN Fill-SysMes("","SBRF003","","%s=" + TRIM(SUBSTRING(iBuffer,11,11)) +
                                               "%s=" + SUBSTRING(iBuffer,23,10)       +
                                               "%s=" + hWOP:buffer-field("bis-type"):buffer-value).
               hWOP:buffer-delete().
            END.
         END.                                    /* WHEN "D" THEN DO          */
      END CASE.

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SPRFPacketConfirm:
   DEFINE INPUT PARAMETER iDirect   AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.

   DEFINE BUFFER smlPacket FOR Packet.
   DEFINE BUFFER bCode     FOR Code.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.

   DEFINE VAR vParentID    AS INT64     NO-UNDO.
   DEFINE VAR vPacketID    AS INT64     NO-UNDO.

   DEFINE VAR vRefDate AS DATE            NO-UNDO.
   DEFINE VAR vRefID   AS CHAR            NO-UNDO.
   DEFINE VAR vRefVal  AS CHAR            NO-UNDO.
   DEFINE VAR vRefCls  AS CHAR            NO-UNDO.
/*============================================================================*/
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vParentID  = hWOP:buffer-field("PacketID"):buffer-value
      NO-ERROR. {&ON-ERROR}

      IF NOT EXCH-MSGBuff(INPUT  iFormat,
                          BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      CASE iDirect:
         WHEN 0 THEN DO:
            ASSIGN
               vRefDate = hWOP:buffer-field("SendDate"):buffer-value
               vRefVal  = hWOP:buffer-field("SendREF"):buffer-value
               vRefID   = hWOP:buffer-field("SendID"):buffer-value
               vRefCls  = bCode.Misc[{&RKC-REPLY}]
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

            vRefVal = vRefID + "|" + ReferenceFormatValue(vRefCls,vRefVal).
         END.
         WHEN 1 THEN DO:
            ASSIGN
               vRefDate = hWOP:buffer-field("RecvDate"):buffer-value
               vRefVal  = hWOP:buffer-field("RecvREF"):buffer-value
               vRefCls  = bCode.Misc[{&RKC-BEGIN}]
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

            vRefVal = ReferenceFormatValue(vRefCls,vRefVal).
         END.
      END CASE.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFPacketConfirm","code:" + bCode.Code           +
                                     " vRefDate:" + GetNullDat(vRefDate) +
                                      " vRefVal:" + GetNullStr(vRefVal)  +
                                      " vRefCls:" + GetNullStr(vRefCls)  +
                                          " ref:" + string(CAN-FIND(FIRST Reference WHERE
                                                                          Reference.op-date    EQ vRefDate
                                                                      AND Reference.Class-Code EQ vRefCls
                                                                      AND Reference.RefValue   EQ vRefVal))).
      &ENDIF

CNF:
      DO TRANSACTION ON ERROR UNDO CNF, RETRY CNF:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         FOR FIRST Reference WHERE
                   Reference.op-date    EQ vRefDate
               AND Reference.Class-Code EQ vRefCls
               AND Reference.RefValue   EQ vRefVal
                   NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  EQ Reference.PacketID
               AND PackObject.file-name EQ "op-entry"
                   NO-LOCK,
             FIRST op WHERE
                   op.op EQ INT64(ENTRY(1,PackObject.Surrogate))
                   EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

            FIND FIRST Packet WHERE
                       Packet.PacketID EQ vParentID
                       NO-LOCK NO-ERROR.

            {opent-st.i &status=mStatusEnd}

            RUN PacketCreate     (INPUT  Packet.SeanceID,
                                  INPUT  Packet.AbonentID,
                                  INPUT  Packet.mail-user-num,
                                  INPUT  hWOP:name,
                                  OUTPUT vPacketID).

            RUN PacketCreateLink (INPUT  vPacketID,
                                  INPUT  PackObject.File-Name,
                                  INPUT  PackObject.Surrogate,
                                  INPUT  ENTRY(2,bCode.Description[1])).

            FOR FIRST smlPacket WHERE
                      smlPacket.PacketID EQ vPacketID
                      EXCLUSIVE-LOCK:
               ASSIGN
                  smlPacket.State       = {&STATE-FIN}
                  smlPacket.ParentID    = vParentID
               NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO CNF, RETRY CNF.
            END.

            hWOP:buffer-delete().
         END.
      END.                                    /* CNF-RET: DO TRANSACTION   */

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFIdentImport:
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.
   DEFINE INPUT PARAMETER iBuffer   AS CHAR     NO-UNDO.

   ASSIGN
      hWOP:buffer-field("Pref"):buffer-value     = SUBSTRING(iBuffer,2,3)
      hWOP:buffer-field("SendDate"):buffer-value = string(DATE(SUBSTRING(iBuffer,5,2) + "." +
                                                               SUBSTRING(iBuffer,7,2) + "." +
                                                               SUBSTRING(iBuffer,9,2)))
      hWOP:buffer-field("SendID"):buffer-value   = SUBSTRING(iBuffer,11,10)
      hWOP:buffer-field("SendREF"):buffer-value  = SUBSTRING(iBuffer,21,6)
      hWOP:buffer-field("RecvID"):buffer-value   = ""
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("SBRFIdentImport","Date:" + SUBSTRING(iBuffer,5,2) + "." +
                                               SUBSTRING(iBuffer,7,2) + "." +
                                               SUBSTRING(iBuffer,9,2) +
                                  " SendID:" + SUBSTRING(iBuffer,11,10) +
                                 " SendREF:" + SUBSTRING(iBuffer,21,6)).
   &ENDIF
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFPrepareTag:
   DEFINE INPUT   PARAMETER iClassCode AS CHAR     NO-UNDO.

   IF NOT CAN-FIND(FIRST ttSBRFTag WHERE
                         ttSBRFTag.Class-Code EQ iClassCode) THEN DO:
      FOR EACH xattr WHERE
               xattr.class-code  EQ iClassCode
           AND xattr.description EQ "ATTR"
               NO-LOCK:

         IF NOT {assigned xattr.xattr-label}  OR
            NOT {assigned xattr.xattr-clabel} THEN DO:
            RUN Fill-SysMes("","ComnExc40","","%s=(" + xattr.class-code         +
                                              "%s="  + xattr.xattr-code   + ")" +
                                              "%s=(" + xattr.xattr-label        +
                                              "%s="  + xattr.xattr-clabel + ")").
            NEXT.
         END.

         CREATE ttSBRFTag.
         ASSIGN ttSBRFTag.Class-Code   = xattr.class-code
                ttSBRFTag.XAttr-Code   = xattr.xattr-code
                ttSBRFTag.ObjectClass  = xattr.xattr-label
                ttSBRFTag.DataType     = xattr.data-type
                ttSBRFTag.DataFormat   = xattr.data-format
                ttSBRFTag.Order        = xattr.order.
         ASSIGN ttSBRFTag.ObjectField  = GetMangledName(ENTRY(1,xattr.xattr-clabel,"["))
                ttSBRFTag.ObjectHandle = ObjectValueHandle(xattr.xattr-label)
                ttSBRFTag.ObjectIndex  = GetFieldIndex (xattr.xattr-clabel).
      END.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFExportTag:
   DEFINE INPUT   PARAMETER hWOP      AS handle   NO-UNDO.
   DEFINE INPUT   PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE OUTPUT  PARAMETER oBuffer   AS CHAR     NO-UNDO.

   DEFINE VAR vValue AS CHAR NO-UNDO.
   DEFINE VAR vTag   AS CHAR NO-UNDO.

   RUN ObjectValueInit.

   oBuffer = "".

CRT:
   FOR EACH ttSBRFTag WHERE
            ttSBRFTag.class-code  EQ iFormat
            NO-LOCK
         BY ttSBRFTag.order:

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFExportTag",string(ttSBRFTag.xattr-code, "x(20)") +
                                " " + string(ttSBRFTag.ObjectClass,"x(20)") +
                                " " + string(ttSBRFTag.ObjectField,"x(20)")).
      &ENDIF

      vValue = ?.
      IF ttSBRFTag.ObjectIndex EQ 0 THEN
         vValue = ttSBRFTag.ObjectHandle:buffer-field(ttSBRFTag.ObjectField):buffer-value  NO-ERROR.
      ELSE
         vValue = ttSBRFTag.ObjectHandle:buffer-field(ttSBRFTag.ObjectField):buffer-value(ttSBRFTag.ObjectIndex) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
         RUN Fill-SysMes("","ComnExc41","","%s=" + ttSBRFTag.ObjectClass +
                                           "%s=" + GetOriginalName(ttSBRFTag.ObjectField) +
                                           "%s=" + ERROR-STATUS:get-message(1)).

      IF NOT {assigned vValue} THEN NEXT CRT.

      vValue  = TRIM(FormatValueEx(vValue,
                                   ttSBRFTag.DataType,
                                   ttSBRFTag.DataFormat)).

      vTag    = "|" + ttSBRFTag.XAttr-Code + string(length(vValue)) + ":".
      oBuffer = oBuffer + vTag + vValue.

   END.

   RUN ObjectValueDown.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFImportTag:
   DEFINE INPUT PARAMETER hWOP      AS handle   NO-UNDO.
   DEFINE INPUT PARAMETER iFormat   AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iBuffer   AS CHAR     NO-UNDO.

   DEFINE VAR vPos AS INT64 NO-UNDO.
   DEFINE VAR vPVl AS INT64 NO-UNDO.
   DEFINE VAR vLen AS INT64 NO-UNDO.
   DEFINE VAR vAll AS INT64 NO-UNDO.
   DEFINE VAR vTag AS CHAR    NO-UNDO.
   DEFINE VAR vVal AS CHAR    NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("SBRFImportTag","iBuffer:" + GetNullStr(iBuffer)).
   &ENDIF

   vAll = length(iBuffer).
   vPos = 1.
   DO WHILE YES:
      ASSIGN
         vPVl = INDEX(iBuffer,":",vPos)
         vTag = ENTRY(1,SUBSTRING(iBuffer,vPos),":")
         vLen = INT64(SUBSTRING(vTag,4))
         vVal = SUBSTRING(iBuffer, vPVl + 1, vLen)
      NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFImportTag","vTag:" + GetNullStr(vTag) +
                                     " vVal:" + GetNullStr(vVal) +
                                     " vLen:" + GetNullNum(decimal(vLen))).
      &ENDIF

      IF vTag EQ "|EE" THEN LEAVE.

      RUN ObjectValueSet (iFormat,               /* Класс                     */
                          SUBSTRING(vTag,2,2),   /* Тэг                       */
                          vVal,                  /* Значение                  */
                          1,
                          "").

      vPos = vPos + length(vTag) + 1 + vLen.
      IF vPos GT vAll THEN LEAVE.
   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFCalcAmount:
   DEFINE INPUT PARAMETER hWOP AS handle NO-UNDO.

   DEFINE VAR vCurrency AS CHAR     NO-UNDO.
   DEFINE VAR vOpDate   AS DATE     NO-UNDO.
   DEFINE VAR vAmtCur   AS decimal  NO-UNDO.
   DEFINE VAR vAmtRub   AS decimal  NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN RETURN ERROR ERROR-STATUS:get-message(1).

      ASSIGN
         vOpDate   = hWOP:buffer-field("op-date"):buffer-value
         vCurrency = IF hWOP:buffer-field("currency-send"):buffer-value NE "" THEN
                        hWOP:buffer-field("currency-send"):buffer-value
                     ELSE mCurrency
         vAmtRub   = hWOP:buffer-field("amt-rub"):buffer-value
      NO-ERROR.  {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFCalcAmount","1 vAmtRub:" + GetNullNum(vAmtRub) +
                                        " vAmtCur:" + GetNullNum(vAmtCur) +
                                      " vCurrency:" + GetNullStr(vCurrency)).
      &ENDIF

      vCurrency = GetFromISOCode(vCurrency).

      IF {assigned vCurrency}                          AND
         vCurrency NE FGetSetting("КодНацВал",?,"810") THEN ASSIGN
         vAmtCur = vAmtRub
         vAmtRub = vAmtCur * FindRate("УЧЕТНЫЙ",vCurrency,vOpDate)
      .
      ELSE
         vAmtCur = 0.00.


      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("SBRFCalcAmount","2 vAmtRub:" + GetNullNum(vAmtRub) +
                                        " vAmtCur:" + GetNullNum(vAmtCur) +
                                      " vCurrency:" + GetNullStr(vCurrency)).
      &ENDIF

      ASSIGN
         hWOP:buffer-field("amt-rub"):buffer-value   = string(vAmtRub)
         hWOP:buffer-field("amt-cur"):buffer-value   = string(vAmtCur)
         hWOP:buffer-field("currency"):buffer-value  = vCurrency
      NO-ERROR. {&ON-ERROR}
   END.

   RETURN.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION SBRFGetBankCode CHAR (INPUT iBankID AS INT64):
   DEFINE VAR vSBRF AS CHAR NO-UNDO.

   IF iBankID EQ 0 THEN DO:                      /* Наш банк                  */
      vSBRF = FGetSetting("БанкСМБР",?,?).
      IF NOT {assigned vSBRF} THEN
         vSBRF = GetXAttrValue("branch",
                               FGetSetting("КодФил",?,?),
                               "БанкСМБР").
      IF NOT {assigned vSBRF} THEN
         vSBRF = GetNeedBankCode ("МФО-9",
                                  FGetSetting("БанкМФО",?,?),
                                  "SBRF3").
   END.
   ELSE                                          /* Другой банк               */
      vSBRF = GetNeedBankCode ("BANK-ID",
                               string(iBankID),
                               "SBRF3").

   IF NOT {assigned vSBRF} THEN
      RUN Fill-SysMes("","SBRF002","","%s=" + string(iBankID)).

   RETURN vSBRF.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFConfirm.
   DEFINE INPUT PARAMETER hWOP      AS handle NO-UNDO.
   DEFINE INPUT PARAMETER iFormat   AS CHAR   NO-UNDO.

   DEFINE VAR vRef        AS CHAR             NO-UNDO.
   DEFINE VAR vRefDate    AS DATE             NO-UNDO.
   DEFINE VAR vRefID      AS CHAR             NO-UNDO.
   DEFINE VAR vRefVal     AS CHAR             NO-UNDO.
   DEFINE VAR vRefCls     AS CHAR             NO-UNDO.
   DEFINE VAR vOpStatNew  AS CHAR             NO-UNDO.
   DEFINE VAR vPacketID   AS INT64            NO-UNDO.
   DEFINE VAR vFlagSet    AS LOGICAL INIT ?   NO-UNDO.
   DEFINE VAR vFlagFnd    AS LOGICAL INIT ?   NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff(INPUT  iFormat,
                          BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      ASSIGN
         vOpStatNew = TRNSettingValue(?,"StatConf","ФБК")
         vRef       = hWOP:buffer-field("SBRFID"):buffer-value
         vRefDate   = DATE(SUBSTRING(vRef,1,6))
         vRefVal    = SUBSTRING(vRef,17,6)
         vRefCls    = bCode.Misc[{&RKC-BEGIN}]
         vPacketID  = hWOP:buffer-field("PacketID"):buffer-value
         vFlagFnd   = NO
      NO-ERROR.

CNF:
      DO TRANSACTION ON ERROR UNDO CNF, RETRY CNF:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         FOR FIRST Reference WHERE
                   Reference.op-date    EQ vRefDate
               AND Reference.Class-Code EQ vRefCls
               AND Reference.RefValue   EQ vRefVal
                   NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  EQ Reference.PacketID
               AND PackObject.file-name EQ "op-entry"
                   NO-LOCK,
             FIRST Packet WHERE
                   Packet.PacketID EQ PackObject.PacketID
                   NO-LOCK,
             FIRST op WHERE
                   op.op EQ INT64(ENTRY(1,PackObject.Surrogate))
                   EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

            {opent-st.i &status=vOpStatNew}

            RUN PacketCreateLink (vPacketID,
                                  "op-entry",
                                  STRING(op.op) + "," + "1",
                                 "RKCconfirm").

             vFlagFnd = YES.
         END.
      END.                                    /* CNF-RET: DO TRANSACTION   */
      IF NOT vFlagFnd THEN 
         RUN Fill-SysMes("","SBRF005","","%s=" + vRefVal +
                                        "%s=" + GetBaseOpKind()).
      ELSE
         RUN Fill-SysMes("","SBRF006","","%s=" + vRefVal +
                                        "%s=" + GetBaseOpKind()).
      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFPacketExportCNF:
   DEFINE INPUT PARAMETER iClass    AS CHAR     NO-UNDO.
   DEFINE INPUT PARAMETER iInstance AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.

   DEFINE BUFFER onePacket FOR Packet.           /* One document Packet       */
   DEFINE BUFFER allPacket FOR Packet.           /* All document Packet       */
   DEFINE BUFFER bPacket   FOR Packet. 

   DEFINE VAR hPacket     AS handle   NO-UNDO.
   DEFINE VAR mSeanceID   AS INT64    NO-UNDO.
   DEFINE VAR mPacketID   AS INT64    NO-UNDO.
   DEFINE VAR vPacketID   AS INT64    NO-UNDO.
   DEFINE VAR vBPacketID  AS INT64    NO-UNDO.
   DEFINE VAR vState      AS CHAR     NO-UNDO.
   DEFINE VAR vKind       AS CHAR     NO-UNDO.
   DEFINE VAR vSurr       AS CHAR     NO-UNDO.
   DEFINE VAR vRefVal     AS CHAR     NO-UNDO.
   DEFINE VAR vRefDate    AS DATE     NO-UNDO.
   DEFINE VAR vOneOp      AS CHAR     NO-UNDO.
   DEFINE VAR vAllOp      AS CHAR     NO-UNDO.
   DEFINE VAR mSeanceImp  AS INT64    NO-UNDO.
   DEFINE VAR vSBRFSendID AS CHAR     NO-UNDO.
   DEFINE VAR vSBRFRecID  AS CHAR     NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:
      {do-retry.i MAIN}
      ASSIGN
         hPacket     = iInstance:default-buffer-handle
         vState      = hPacket:buffer-field("State"):buffer-value
         vKind       = hPacket:buffer-field("Kind"):buffer-value
         mSeanceID   = hPacket:buffer-field("SeanceID"):buffer-value
         mSeanceImp  = INT64(TRNSettingValue("","SeanceImp",""))
         vSBRFSendID = TRNSettingValue("","SBRFSendID","000")
         vSBRFRecID  = TRNSettingValue("","SBRFRecID","000")
         vAllOp      = ""
      NO-ERROR. {&ON-ERROR}

      {getsncmlu.i MAIN exp}                     /* FIND seance AND mail-user */

MOVE:
      DO TRANSACTION ON ERROR UNDO MOVE, RETRY MOVE:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         CREATE Packet.                       /* Создаем пакет экспорта    */
         ASSIGN vPacketID               = next-value(pack-id)
                Packet.PacketID      = vPacketID
                Packet.AbonentID     = -1
                Packet.mail-user-num = mail-user.mail-user-num
                Packet.filial-id     = mail-user.filial-id
                Packet.SeanceID      = mSeanceID
                Packet.State         = {&STATE-CRT}
                Packet.PackDate      = today
                Packet.mail-format   = mail-user.mail-format
                Packet.Kind          = vKind
                Packet.Class-Code    = iClass
                Packet.ParentID      = 0 /* vBPacketID */
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MOVE, RETRY MOVE.

         VALIDATE Packet.

         RUN PacketTextClear (vPacketID).
         RUN PacketTextKeep (INPUT        vPacketID,
                             INPUT        "SBRF3:" + vSBRFRecID + vSBRFSendID + "~r~n",
                             INPUT-OUTPUT vAllOp).

         FOR EACH Seance WHERE
                  Seance.DIRECT EQ {&DIR-IMPORT}
              AND Seance.SeanceID EQ mSeanceImp NO-LOCK,
             EACH bPacket WHERE
                  bPacket.SeanceID EQ Seance.SeanceID NO-LOCK,
             EACH PackObject WHERE
                  PackObject.file-name EQ 'op-entry'
              AND PackObject.PacketID  EQ bPacket.PacketID
                  NO-LOCK,
             EACH Reference WHERE
                  Reference.PacketID EQ bPacket.PacketID NO-LOCK:
                     ASSIGN
                        mPacketID = PackObject.PacketID
                        vSurr     = PackObject.Surrogate  
                        vRefVal   = Reference.RefValue
                        vRefDate  = Reference.op-date
                     NO-ERROR.

            FIND FIRST op WHERE op.op EQ INT64(ENTRY(1,vSurr)) NO-LOCK NO-ERROR.
            IF AVAIL op THEN FIND FIRST op-bank OF op NO-LOCK NO-ERROR.


            RUN PacketTextKeep (INPUT        vPacketID,
                                INPUT        " 0BB" + GetXattrValue("op",ENTRY(1,vSurr),"SBRFID") + "|" +
                                             "CR3:000" + "|" +
                                             "DT3:101" + "|" + 
                                             "LD6:"    + STRING(TODAY,"999999") + "|" +
                                             "LT12:"   + STRING(TODAY,"999999") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") +  "|" +
                                             "OC10:"   + SBRFGetBankCode(0) +  "|" +
                                             "OD6:"    + STRING(TODAY,"999999") +   "|" +
                                             "ON6:"    + ENTRY(2,vRefVal,"|") +   "|" +
                                             "PA10:"   + SBRFGetBankCode(0)   +   "|" +
                                             "RC10:"   + GetNullStr(GetNeedBankCode("МФО-9",
                                                                                  op-bank.bank-code,
                                                                                   "SBRF3")) +  "|EE:" +
                                             "~r~n",
                              INPUT-OUTPUT    vAllOp).

         RUN PacketCreateLink (vPacketID,
                               "op-entry",
                               vSurr,
                               "RKCconfirm").

      END.

      RUN PacketTextKeep (INPUT        vPacketID,
                          INPUT        "EOF~r~n",
                          INPUT-OUTPUT vAllOp).
      RUN PacketTextSave  (vPacketID, vAllOp).



      vFlagSet = YES.
   END.                                          /* MAIN:                     */
END.

   {doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Загрузка отказов в формате SBRF-3                          */
/*----------------------------------------------------------------------------*/
PROCEDURE SBRFDenial.
   DEFINE INPUT PARAMETER hWOP    AS HANDLE    NO-UNDO.
   DEFINE INPUT PARAMETER iFormat AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iBuffer AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vRef       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRefDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vRefID     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRefVal    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRefCls    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpStatNew AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPacketText AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAllOp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vFlagSet   AS LOGICAL   INITIAL ? NO-UNDO.
   DEFINE VARIABLE vFlagFnd   AS LOGICAL   INITIAL ? NO-UNDO.

   DEFINE BUFFER bCode     FOR Code.

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      IF NOT EXCH-MSGBuff(INPUT  iFormat,
                          BUFFER bCode) THEN
         UNDO MAIN, RETRY MAIN.

      ASSIGN
         vOpStatNew = TRNSettingValue(?,"StatDen ","В")
         vRef       = hWOP:buffer-field("SBRFID"):buffer-value
         vRefDate   = DATE(SUBSTRING(vRef,1,6))
         vRefVal    = SUBSTRING(vRef,17,6)
         vRefCls    = bCode.Misc[{&RKC-BEGIN}]
         vPacketID  = hWOP:buffer-field("PacketID"):buffer-value
         vFlagFnd   = NO
         vPacketText = iBuffer
         vAllOp = ""
      NO-ERROR.

   CNF:
      DO TRANSACTION ON ERROR UNDO CNF, RETRY CNF:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.

         FOR FIRST Reference WHERE
                   Reference.op-date    EQ vRefDate
               AND Reference.Class-Code EQ vRefCls
               AND Reference.RefValue   EQ vRefVal
                   NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  EQ Reference.PacketID
               AND PackObject.file-name EQ "op-entry"
                   NO-LOCK,
             FIRST Packet WHERE
                  Packet.PacketID EQ PackObject.PacketID
                  NO-LOCK,
             FIRST op WHERE
                   op.op EQ INT64(ENTRY(1,PackObject.Surrogate))
                   EXCLUSIVE-LOCK QUERY-TUNING(HINT "RULE"):

            {opent-st.i &status=vOpStatNew}

            RUN PacketCreateLink (vPacketID,
                                  "op-entry",
                                  STRING(op.op) + "," + "1",
                                ENTRY(2,bCode.Description[1])) NO-ERROR.

             vFlagFnd = YES.
         END.
      END.                                    /* CNF-RET: DO TRANSACTION   */

      IF NOT vFlagFnd THEN 
         RUN Fill-SysMes("","SBRF005","","%s=" + vRefVal +
                                        "%s=" + GetBaseOpKind()).
      ELSE
      DO:
         RUN PacketTextKeep (INPUT        vPacketID,
                             INPUT        vPacketText,
                             INPUT-OUTPUT vAllOp).
         RUN PacketTextSave  (vPacketID, vAllOp).
         RUN Fill-SysMes("","SBRF007","","%s=" + vRefVal +
                                        "%s=" + GetBaseOpKind()).
      END.

      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.
/******************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='08/12/2015 11:02:53.734+04:00' */
/* $LINTUSER='ahra' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-sbrf.p' */
/*prosigntZgCR4c1Oa1S+GfKJmVsSQ*/