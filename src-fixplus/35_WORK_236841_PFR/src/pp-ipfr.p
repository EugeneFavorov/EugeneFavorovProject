/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-ipfr.p
      Comment: 0161576 Создать функционал. обмен с ПФР - договор о доставке пенсий
   Parameters: нет
         Uses:
      Used by:
      Created: 16/05/2013 KMBIS 0161576 Создать функционал. обмен с ПФР - договор о доставке пенсий
                                        Библиотека для создания документов по полученным данным из 
                                        ПФР (формат СПб)
     Modified: 03/11/2015 KMBIS 0236841 Миграция. Загрузка реестра зачисления пенсий из ПФ
                                        Обмен с ПФ Омск. Остальные форматы удалены.
     Modified: 
*/
{globals.i}

{g-trans.equ}
{intrface.get pbase}
{intrface.get op}
{intrface.get exch}
{intrface.get pack}
{intrface.get count}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get filex}
{intrface.get trans}
{intrface.get strng} 
{intrface.get xclass} 

{intrface.get tmess}
{intrface.get instrum}

DEF VAR mCount     AS INT64 NO-UNDO.   
DEF VAR mCountDoc  AS INT64 NO-UNDO.   /* Счетчик документов со статусом выше В                      */
DEF VAR mCountErr  AS INT64 NO-UNDO.   /* Счетчик документов в статусе В                             */
DEF VAR mCountAnn  AS INT64 NO-UNDO.   /* Количество аннулированных документов                       */
DEF VAR mContDef   AS INT64 NO-UNDO.   /* Номер в дефектной ведомости                                */
DEF VAR mTotalCnt  AS INT64 NO-UNDO.   /* Количество обработанных клиентов                           */
DEF VAR mAmt       AS DEC   NO-UNDO.   /* Сумма зачисленных документов                               */
DEF VAR mAmtErr    AS DEC   NO-UNDO.   /* Сумма документов созданных в статусе В                     */
DEF VAR mFileName  AS CHAR  NO-UNDO.                                                                
DEF VAR mFileLog   AS CHAR  NO-UNDO.                                                                
DEF VAR mVedDef    AS CHAR  NO-UNDO.   /* Коды ошибок для дефектной ведомости                        */
DEF VAR mVedClose  AS CHAR  NO-UNDO.   /* Коды ошибок для ведомости незачисленных сумм (счет закрыт) */

ASSIGN
   mVedDef    = fGetSetting("ОбменПФР","ВедДефект", "" )
   mVedClose  = fGetSetting("ОбменПФР","ВедЗакр",   "" )
.

/*----------------------------------------------------------------------------*/
/* Заполняем на пакете реквизиты в соответсвии с импортом                     */
/*----------------------------------------------------------------------------*/

PROCEDURE UpdPacketSigns:
   DEF INPUT PARAM iHBuff AS HANDLE NO-UNDO.

DEF VAR vPackSur  AS CHAR  NO-UNDO.
DEF VAR vSeanceID AS INT64 NO-UNDO.
DEF VAR vPackId   AS CHAR  NO-UNDO.
DEF VAR vClass    AS CHAR  NO-UNDO.
DEF VAR vDefErr   AS CHAR  NO-UNDO.
DEF VAR vDefClose AS CHAR  NO-UNDO.

DEF BUFFER bPackObject  FOR PackObject.
DEF BUFFER bPpfr        FOR Packet.

   ASSIGN
       vPackSur  = SUBST("&1,1", TRIM(iHBuff::op))
       vSeanceID = INT64(TRIM(iHBuff::SeanceID))
   NO-ERROR.

   FOR FIRST bPackObject WHERE bPackObject.SeanceID  EQ vSeanceId
                           AND bPackObject.file-name EQ "op-entry"  
                           AND bPackObject.Surrogate EQ vPackSur
                         NO-LOCK,
      FIRST bPpfr WHERE bPpfr.PacketId EQ bPackObject.PacketId
                  NO-LOCK:
      ASSIGN
         vPackId            = STRING(bPpfr.PacketId)
         vClass             = bPpfr.class-code
         vDefErr            = ""
         vDefClose          = ""
         /*=== Запомним на всякий случай пакет ===*/
         iHBuff::RetPacketId = vPackId
      NO-ERROR.

      UpdateSignsEx(vClass, vPackId, "Hash",        TRIM(iHBuff::Hash)).
      UpdateSignsEx(vClass, vPackId, "RegNumPFR",   TRIM(iHBuff::RegNumPFR)).
      UpdateSignsEx(vClass, vPackId, "Year",        TRIM(iHBuff::Year)).
      UpdateSignsEx(vClass, vPackId, "Month",       TRIM(iHBuff::Month)).
      UpdateSignsEx(vClass, vPackId, "InsurNum",    TRIM(iHBuff::InsurNum)).
      UpdateSignsEx(vClass, vPackId, "name-rec",    TRIM(iHBuff::name-rec)).
      UpdateSignsEx(vClass, vPackId, "amt-rub",     TRIM(iHBuff::amt-rub)).
      UpdateSignsEx(vClass, vPackId, "Code",        TRIM(iHBuff::Code)).
      UpdateSignsEx(vClass, vPackId, "CodeOpr",     TRIM(iHBuff::CodeOpr)).
      UpdateSignsEx(vClass, vPackId, "CodeInf",     TRIM(iHBuff::CodeInf)).
      UpdateSignsEx(vClass, vPackId, "V_BNK",       TRIM(iHBuff::V_BNK)).
      UpdateSignsEx(vClass, vPackId, "Acct",        TRIM(iHBuff::Acct)).
      UpdateSignsEx(vClass, vPackId, "acct-cr",     TRIM(iHBuff::acct-cr)).
      UpdateSignsEx(vClass, vPackId, "details",     TRIM(iHBuff::details)).
      UpdateSignsEx(vClass, vPackId, "RegPFR",      TRIM(iHBuff::RegPFR)).
      UpdateSignsEx(vClass, vPackId, "ImpPacketId", TRIM(iHBuff::ImpPacketId)).
      UpdateSignsEx(vClass, vPackId, "RetCodePFR",  TRIM(iHBuff::RetCodePFR)).

      IF {assigned bPpfr.PackError} THEN
      DO:
         vDefClose = ListsCrossing(bPpfr.PackError, mVedClose, ",").
         IF NOT {assigned vDefClose} THEN
            vDefErr = ListsCrossing(bPpfr.PackError, mVedDef  , ",").

         IF {assigned vDefErr} THEN
         DO:
            mContDef = mContDef + 1.
            UpdateSignsEx(vClass, vPackId, "VedNum", STRING(mContDef)).

         END. /* IF {assigned vDefClose} THEN */
      END. /* IF {assigned bPpfr.PackError} THEN */
   END. /* FOR FIRST bPackObject WHERE bPackObject.SeanceID  EQ vSeanceId */

END PROCEDURE. /* UpdPacketSigns */

/*===============================================================================================*/

/*----------------------------------------------------------------------------*/
/* Пишем в лог-файл                                                           */
/*----------------------------------------------------------------------------*/

PROCEDURE WriteLog:
   DEF INPUT PARAM iStr AS CHAR NO-UNDO.

   OUTPUT TO VALUE(mFileLog) APPEND.
   IF iStr EQ "" THEN
      PUT UNFORMATTED SKIP(1).

   IF {assigned iStr}THEN
      PUT UNFORMATTED iStr SKIP. 

   OUTPUT CLOSE.

END PROCEDURE. /* WriteLog */

/*===============================================================================================*/

/*----------------------------------------------------------------------------*/
/* Сброс глобальных переменных на значение по умолчанию                       */
/*----------------------------------------------------------------------------*/

PROCEDURE NulMainVar:

   ASSIGN
      mCount    = 0
      mCountDoc = 0
      mCountErr = 0
      mCountAnn = 0
      mContDef  = 0
      mAmt      = 0
      mAmtErr   = 0
      mTotalCnt = 0  
      mFileLog  = ""
      mFileName = ""
   .

END PROCEDURE. /* NulMainVar */

/*===============================================================================================*/


/*===============================================================================================*/
/*=== Инициализация импрорта файла: Омск. Метод на заголовке format-FH ==========================*/
PROCEDURE PFRInitOmsFH:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet   AS LOG   NO-UNDO INIT ?.
DEF VAR vTimeStamp AS CHAR  NO-UNDO. 
DEF VAR vLogName   AS CHAR  NO-UNDO. 

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      /*=== Обнуляем переменные ===*/
      RUN NulMainVar IN THIS-PROCEDURE.

      ASSIGN
         /*=== Имя загружаемого файла ===*/
         mFileName  = FileGetPath(INT64(iHBuff::FileExchID))
         mFileName  = GetPureName(mFileName)

         /*=== Временная отметка загрузки ===*/
         vTimeStamp = REPLACE(STRING(TIME, "HH:MM:SS"), ":", "")

         /*=== Имя лог-файла */
         vLogName   = SUBST("&1_&2-&3.log", mFileName, iHBuff::SeanceID, vTimeStamp)

         /*=== Полный путь к лог-файлу ===*/
         mFileLog   = CatalogGetPath(INT64(iHBuff::mail-user-num), "LogArch", "Path")
         mFileLog   = MakeFileName(mFileLog, vLogName)
      NO-ERROR.

      IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN  
      DO:
         IF NOT {assigned mFileLog} THEN  
            RUN Fill-SysMes IN h_tmess ("","","-1", "Не задан архивный каталог лог файлов").

         IF NOT {assigned mFileName} THEN  
            RUN Fill-SysMes IN h_tmess ("","","-1", "Имя импортируемого файла не определено").
      END. /* IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN */
      ELSE
      DO:
         /*=== Пишем в лог имя импортируемого файла ===*/
         RUN WriteLog IN THIS-PROCEDURE(SUBST("Файл: &1", mFileName)).
         RUN WriteLog IN THIS-PROCEDURE("") .
         vFlagSet = YES.
      END. /* IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN ... ELSE */ 

   END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRInitOmsFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Выполняет импорт одного документа: Омск ===================================================*/
PROCEDURE PFRImportOmsDT:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet     AS LOG    NO-UNDO INIT ?.
                              
DEF VAR vYear        AS CHAR   NO-UNDO.
DEF VAR vMonth       AS INT64  NO-UNDO.
DEF VAR vFIO         AS CHAR   NO-UNDO.
DEF VAR vAcct        AS CHAR   NO-UNDO.
DEF VAR vRegPfr      AS CHAR   NO-UNDO.
DEF VAR vDetails     AS CHAR   NO-UNDO.
DEF VAR vCounter     AS CHAR   NO-UNDO.
DEF VAR vAmt         AS DEC    NO-UNDO.
DEF VAR vDoc-Num     AS INT64  NO-UNDO.
DEF VAR vTmpStr      AS CHAR   NO-UNDO.

DEF BUFFER bOP FOR op.

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         /*=== Считаем строки ===*/
         mTotalCnt      = mTotalCnt + 1

         vMonth         = INT64(TRIM(iHBuff::Month))
         vYear          = SUBST("20&1", TRIM(iHBuff::Year))
         vAcct          = TRIM(iHBuff::acct)
         vFIO           = SUBST("&1 &2 &3", iHBuff::fam, iHBuff::name, iHBuff::surname)
         vFIO           = RemoveDoubleChars(TRIM(vFIO), " ")
         vAmt           = DEC(iHBuff::Amt)

         /*=== Cчетчик нумерации документов ===*/
         vCounter       = iHBuff::counter

         /* Считаем общую сумму по реестру ===*/
         mAmt           = mAmt + vAmt
      NO-ERROR. {&ON-ERROR}

      RUN WriteLog IN THIS-PROCEDURE(SUBST("Строка &1 &2 &3",
                                           STRING(mTotalCnt, ">>>>"),
                                           vAcct,
                                           STRING(vFIO, "x(30)"))).

      IF {assigned vCounter} THEN
      DO:
         /*=== Счетчик - ищем значение по классификатору Counters ===*/
         vDoc-Num = SetCounterValue(vCounter, ?, gend-date) NO-ERROR. 
         IF vDoc-Num EQ ? THEN
         DO:
            vTmpStr = SUBST("Ошибка получения/присвоения значения для счетчика [&1].", vCounter).
            RUN WriteLog IN THIS-PROCEDURE(vTmpStr).
            RUN WriteLog IN THIS-PROCEDURE("").
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vTmpStr).
            RETURN ERROR vTmpStr.
         END.
      END.
      ELSE /*=== Ручная генерация номера документа ===*/
         vDoc-Num = mCountDoc + mCountAnn + mCountErr + 1.

      /*=== Сохраним район ПФР ===*/
      IF LENGTH(mFileName) GE 8 THEN 

      ASSIGN
         vRegPFR       = SUBSTR(mFileName, 7,2)    WHEN LENGTH(mFileName) GE 8
         vDetails      = SUBST("Перечисление пенсии за &1 &2 года согласно реестра &3",
                               {rsdate.i vMonth}, /* месяц прописью */
                               vYear,
                               STRING(gend-date, "99.99.9999"))
         iHBuff::acct-cr  = vAcct
         iHBuff::acct-rec = vAcct
         iHBuff::name-rec = vFIO
         iHBuff::RegPFR   = vRegPFR
         iHBuff::amt-rub  = STRING(vAmt)
         iHBuff::details  = vDetails
         iHBuff::doc-num  = STRING(vDoc-Num)
      .

      /*=== Создаем документ ===*/
      RUN RunTransaction IN h_pbase(iHBuff::op-kind) NO-ERROR. 

      FIND FIRST bOP WHERE bOP.op EQ INT64(iHBuff::op) 
                       AND bOP.op NE 0
                     NO-LOCK NO-ERROR.

      IF AVAIL(bOP) THEN
      DO:
         /*=== Сохраним данные платежа на ДР пакета ===*/
         RUN UpdPacketSigns IN THIS-PROCEDURE(iHBuff). 

         /*=== Считаем успешно созданные документы ===*/
         mCountDoc = mCountDoc + 1.

      END. /* IF AVAIL(op) THEN */
      ELSE
      DO:
         /*=== Если по строке не создали проводку, создадим пакет сами ===*/
         vTmpStr = "Не удалось создать докумет".
         RUN WriteLog IN THIS-PROCEDURE(vTmpStr).
         RUN WriteLog IN THIS-PROCEDURE("").
         RUN Fill-SysMes IN h_tmess ("", "", "-1", vTmpStr).
         RETURN ERROR vTmpStr.
      END. /* IF AVAIL(op) THEN */

      /* Особенность crdiresp: после того как строка обработана и не требуется, нужно удалить ее. */
      /* Иначе может быть сломано определение PacketId при импорте нескольких файлов одновременно */
      RUN InstanceJunk IN h_exch(iHBuff, 0).

      vFlagSet = YES.

   END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN. */

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRImportOmsDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Выполняет импорт хвостовика: Омск =========================================================*/
PROCEDURE PFRImportOmsFF:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet         AS LOG      NO-UNDO INIT ?.
DEF VAR vPackID          AS INT64      NO-UNDO.

DEF VAR vAmtOp           AS DEC      NO-UNDO.
DEF VAR vCntOp           AS INT64    NO-UNDO.

DEF BUFFER bPacket      FOR Packet.
DEF BUFFER bPackObject  FOR PackObject.
DEF BUFFER bOp          FOR op.
DEF BUFFER bOpEntry     FOR op-entry.

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vPackID = INT64(TRIM(iHBuff::PacketID))
      NO-ERROR.

      RUN WriteLog IN THIS-PROCEDURE("").

      FOR EACH bPacket WHERE bPacket.ParentID EQ vPackID
                         AND vPackID          GT 0
                       NO-LOCK,
         FIRST bPackObject WHERE bPackObject.PacketId  EQ bPacket.PacketId
                             AND bPackObject.file-name EQ "op-entry"  
                            NO-LOCK,
         FIRST bOp WHERE bOp.op EQ INT64(ENTRY(1, bPackObject.Surrogate))
                   NO-LOCK,
         FIRST bOpEntry of bOp NO-LOCK:

         IF NOT (bOp.op-status GT "В") THEN
         DO:
            ASSIGN
               mCountErr = mCountErr + 1
               mAmtErr   = mAmtErr   + bOpEntry.amt-rub
            .

            RUN WriteLog IN THIS-PROCEDURE(SUBST("Документ номер &1 сумма &2 ошибки:",
                                                 bOp.doc-num,
                                                 STRING(bOpEntry.amt-rub, ">>>,>>>,>>9.99"))).
            {empty ttError}
            RUN FillErrorTable IN h_exch(bPacket.ClassError, 
                                         bPacket.PackError, 
                                         OUTPUT TABLE ttError).
            FOR EACH ttError NO-LOCK:
               RUN WriteLog IN THIS-PROCEDURE(SUBST("     &1", ttError.name)).

            END. /* FOR EACH ttError NO-LOCK: */
         END. /* IF NOT (bOp.op-status GT "В") THEN */
         ELSE
         DO:
            ASSIGN
               vCntOp = vCntOp + 1               
               vAmtOp = vAmtOp + bOpEntry.amt-rub
            .
         END. /* IF NOT (bOp.op-status GT "В") THEN ... ELSE */
      END. /* FOR FIRST bPackObject */

      FOR FIRST bPacket WHERE bPacket.PacketId EQ vPackID
                          AND vPackID          GT 0
                        NO-LOCK:
         UpdateSignsEx(bPacket.Class-code, STRING(vPackId), "TotalCnt", STRING(mTotalCnt)).
         FILE-INFO:FILE-NAME = mFileLog.
         UpdateSignsEx(bPacket.Class-code, STRING(vPackId), "LogName", FILE-INFO:FULL-PATHNAME).
      END. /* FOR FIRST bPacket WHERE bPacket.PacketId EQ vPackID */

      RUN WriteLog IN THIS-PROCEDURE("").
      RUN WriteLog IN THIS-PROCEDURE(SUBST("Всего строк:      &1", STRING(mTotalCnt))).
      RUN WriteLog IN THIS-PROCEDURE(SUBST("Всего по реестру: &1", STRING(mAmt))).
      RUN WriteLog IN THIS-PROCEDURE("").

      RUN WriteLog IN THIS-PROCEDURE(SUBST("Документов без ошибок: &1 в сумме &2", 
                                     STRING(vCntOp, ">>>9"), 
                                     STRING(vAmtOp,   ">>>,>>>,>>9.99"))).
      RUN WriteLog IN THIS-PROCEDURE(SUBST("Документов с ошибками: &1 в сумме &2", 
                                     STRING(mCountErr, ">>>9"), 
                                     STRING(mAmtErr,">>>,>>>,>>9.99"))).
      RUN WriteLog IN THIS-PROCEDURE("").

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRImportOmsFF */

/*===============================================================================================*/
