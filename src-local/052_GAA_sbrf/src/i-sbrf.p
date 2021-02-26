/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 ЗАО "Банковские информационные системы"
     Filename: I-SBRF.P
      Comment: Выполняет разбор одного файла от терминала SBRF-3

               Выполняемые действия:

   Parameters: iClass    - имя класса
               iInstance - содержимое класса
         Uses:
      Used BY:
      Created: 01.11.2004 NIK
     Modified: 26.05.2006 NIK PROGRESS v10.
     Modified:
*/

{globals.i}
&GLOBAL-DEFINE IS-DEBUG YES

DEFINE INPUT  PARAMETER iClass      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iInstance   AS HANDLE     NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}

{intrface.get xclass}
{intrface.get strng}

{intrface.get tmess}
{intrface.get pbase}
{intrface.get trans}

{intrface.get filex}
{intrface.get exch}
{intrface.get rule}
{intrface.get pack}
{intrface.get echck}

{intrface.get xrkc}
{intrface.get wop}
{intrface.get sbrf}
{intrface.get rfrnc}

DEFINE VAR hExch         AS handle            NO-UNDO.      /* Объект          */
DEFINE VAR hExchBig      AS handle            NO-UNDO.      /* Головной объект */
DEFINE VAR hKeep         AS handle            NO-UNDO.      /* Копия           */
DEFINE VAR mNext         AS LOGICAL INIT NO   NO-UNDO.
                       
DEFINE VAR mSeanceID     AS INT64           NO-UNDO.
DEFINE VAR mPacketID     AS INT64           NO-UNDO.
DEFINE VAR mFileExchID   AS INT64           NO-UNDO.
DEFINE VAR mMailUserID   AS INT64           NO-UNDO.
DEFINE VAR mPath         AS CHAR              NO-UNDO.
DEFINE VAR mFileName     AS CHAR              NO-UNDO.
DEFINE VAR mFormat       AS CHAR              NO-UNDO.
DEFINE VAR mKind         AS CHAR              NO-UNDO.
DEFINE VAR mBuffer       AS CHAR              NO-UNDO.
DEFINE VAR mHead         AS LOGICAL  INIT NO  NO-UNDO.

DEFINE VAR mFormatID     AS CHAR              NO-UNDO.
DEFINE VAR mSelfID       AS CHAR              NO-UNDO.
DEFINE VAR mRecvID       AS CHAR              NO-UNDO.
DEFINE VAR mSendID       AS CHAR              NO-UNDO.
DEFINE VAR mFlagPos      AS CHAR              NO-UNDO.
DEFINE VAR mOpDatePeriod AS INT64  NO-UNDO.

DEFINE VAR mFlagSet  AS LOGICAL INIT ? NO-UNDO.

DEFINE STREAM sImport.
/*============================================================================*/
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   {profile S00}

   ASSIGN
      hExchBig    = iInstance:default-buffer-handle
      mSeanceID   = hExchBig:buffer-field("SeanceID"):buffer-value
      mPacketID   = hExchBig:buffer-field("PacketID"):buffer-value
      mFileExchID = hExchBig:buffer-field("FileExchID"):buffer-value
      mMailUserID = hExchBig:buffer-field("mail-user-num"):buffer-value
   NO-ERROR. {&ON-ERROR}

   FIND FIRST mail-user WHERE mail-user.mail-user-num EQ mMailUserID NO-LOCK NO-ERROR.
   {&ON-ERROR}
/*------------------------ Определим класс и объект для  импорта документов --*/
   ASSIGN
      mFormat   = mail-user.mail-format
      mPath     = FileGetPath(mFileExchID)
      mKind     = GetCodeMisc("EXCH-MSG",mFormat,{&RKC-KIND})     /* Класс      */
      mFileName = GetPureName(mPath)
   .

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-sbrf.p","mFileExchID:" + string(mFileExchID) +
                                   " mPath:" + GetNullStr(mPath)   +
                                 " mFormat:" + GetNullStr(mFormat) +
                                   " mKind:" + GetNullStr(mKind)).
   &ENDIF
                                       /* Повторный импорт */
   FOR FIRST Packet WHERE
             Packet.PacketID  EQ mPacketID
       NO-LOCK,
       FIRST Seance   WHERE 
             Seance.SeanceID EQ Packet.SeanceID   
             NO-LOCK:
      mOpDatePeriod = INT64(TRNSettingValue("","OpDatePeriod","")).
      IF mOpDatePeriod EQ 0 THEN mOpDatePeriod = INT64(Seance.SeanceDate).

      IF CAN-FIND(FIRST Reference
                  WHERE Reference.Class-Code EQ "RSBRFFile"
                    AND Reference.RefValue   EQ mFileName
                    AND Reference.op-date    GE Seance.SeanceDate - mOpDatePeriod
                    AND Reference.op-date    LE Seance.SeanceDate
                 )  THEN DO:
         RUN Fill-SysMes IN h_tmess ("","FXExch47","","%s=" + mPath).
         IF auto THEN RUN FileMoveArchive IN h_filex  (mPacketID,"ImpArch").
         mFlagSet = NO.
         LEAVE MAIN.
      END.

   END.


   ASSIGN
      hExch   = GetTransObject(mKind)
      hExch   = hExch:default-buffer-handle                     /* Объект     */

      hExch:buffer-field("mail-user-num"):buffer-value =        /* Поля       */
         hExchBig:buffer-field("mail-user-num"):buffer-value
      hExch:buffer-field("PacketID"):buffer-value      =
         hExchBig:buffer-field("PacketID"):buffer-value
      hExch:buffer-field("FileExchID"):buffer-value    =
         hExchBig:buffer-field("FileExchID"):buffer-value
      hExch:buffer-field("SeanceID"):buffer-value      =
         hExchBig:buffer-field("SeanceID"):buffer-value
   NO-ERROR. {&ON-ERROR}

   IF NOT {assigned mPath} THEN UNDO MAIN, RETRY MAIN.

   CREATE WIDGET-POOL "I-SBRF-P".
   CREATE BUFFER hKeep FOR TABLE hExch IN widget-pool "I-SBRF-P".

   RUN ObjectValueInit.                          /* Инициализация запросов    */

   {profile S10}

/*-------------------------------------- Выполнение импорта данных из файла --*/
   mNext = YES.
   INPUT STREAM sImport FROM VALUE(mPath) convert source "1251".
IMP:
   REPEAT:
      IMPORT STREAM sImport UNFORMATTED mBuffer. /* Очередная строка          */

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("i-sbrf.p","mFormatID:" + GetNullStr(mFormatID) +
                                    " mHead:" + string(mHead)         +
                                 " mFlagPos:" + GetNullStr(mFlagPos)  +
                                  " mBuffer:" + GetNullStr(mBuffer)).
      &ENDIF

      IF mBuffer BEGINS "EOF" THEN LEAVE IMP.    /* Конец файла               */

      IF NOT mHead THEN DO:                      /* Еще не заголовок          */
         IF mBuffer BEGINS "SBRF3" THEN ASSIGN
            mFormatID = SUBSTRING(mBuffer, 1,5)
            mRecvID   = SUBSTRING(mBuffer, 7,3)
            mSendID   = SUBSTRING(mBuffer,10,3)
         .
         ELSE IF mBuffer BEGINS "STLIST" THEN ASSIGN
            mFormatID = SUBSTRING(mBuffer, 1,6)
            mRecvID   = SUBSTRING(mBuffer, 8,3)
            mSendID   = SUBSTRING(mBuffer,11,3)
            mFlagPos  = ""
         .

         IF LOOKUP(mFormatID,"SBRF3,STLIST") EQ 0 THEN DO:
            RUN Fill-SysMes IN h_tmess ("","SBRF001","","%s=" + mFormatID).
            UNDO MAIN, RETRY MAIN.
         END.

         mSelfID = SBRFGetBankCode(0).

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-sbrf.p","mFormatID:" + GetNullStr(mFormatID) +
                                     " mRecvID:" + GetNullStr(mRecvID)   +
                                     " mSelfID:" + GetNullStr(mSelfID)   +
                                     " mSendID:" + GetNullStr(mSendID)).
         &ENDIF

         IF mRecvID NE "966" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","ExchRKC03","","%s=" + mPath          +
                                              "%s=" + mRecvID        +
                                              "%s=" + "966").
            UNDO MAIN, RETRY MAIN.
         END.
         mHead = YES.
         NEXT IMP.
      END.
/*---------------------------------------- Обработка строк текста сообщения --*/
      IF mNext THEN
         RUN InstanceCreate(hKeep,hExch).        /* Восстановление шаблона    */

      {profile S20}

      CASE mFormatID:
         WHEN "SBRF3" THEN DO:                   /* Документы                 */
            RUN SBRFPacketImport (INPUT        mKind,
                                  INPUT        hExch:table-handle,
                                  INPUT        mFormat,
                                  INPUT        mBuffer).
            mNext = YES.
         END.
         WHEN "STLIST" THEN DO:                  /* Выписка                   */
            RUN SBRFStListImport (INPUT        mKind,
                                  INPUT        hExch,
                                  INPUT        hExchBig,
                                  INPUT        mFormat,
                                  INPUT        mBuffer,
                                  INPUT-OUTPUT mFlagPos).
            IF mFlagPos EQ "D" THEN mNext = YES. /* Тело выписки              */
         END.
      END CASE.

      {profile S30}

   END.                                          /* IMP: REPEAT:              */
   INPUT STREAM sImport CLOSE.

   RUN XRKCRunOpKind       (mKind).              /* Вызов итоговых транзакций */

   RUN XRKCPacketEndStatus (mPacketID).          /* Статус общего сообщения   */
   RUN PacketCreateRef IN h_rfrnc (TODAY,
                                   mPacketID,
                                   "RSBRFFile",
                                   mFileName) NO-ERROR.
   RUN FileMoveArchive IN h_filex (mPacketID,"ImpArch").
   mFlagSet = YES.
END.                                             /* MAIN: DO ...              */

DELETE WIDGET-POOL "I-SBRF-P".

RUN ObjectValueDown.                   /* Закрытие системы получения данных   */

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-sbrf.p","mFlagSet:" + GetNullStr(string(mFlagSet))).
&ENDIF

{intrface.del}

{doreturn.i mFlagSet}
/******************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='07/04/2015 12:35:28.911+04:00' */
/* $LINTUSER='mike' */
/* $LINTMODE='1' */
/* $LINTFILE='i-sbrf.p' */
/*prosign37BNI83Ng3tPOQxSuxfzwA*/