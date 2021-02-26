/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: i-gcrown.p
      Comment: Выполняет импорт одного файла с ответами на поручения
               Является методом "Import" класса "ECardMast" и унаследованных
               от него.
   Parameters: iClass
               iInstance
         Uses:
      Used BY:
      Created: 26.05.2015 KMBIS TT:0236973 Миграция.Прием платежей QIWI, Рапида, Уралсиб, КиберПлат
                                На основе crdiresp.p. Для импорта реестра "Золотой короны".
     Modified: 
*/
{globals.i}

DEFINE NEW GLOBAL SHARED STREAM debug-stream.

DEFINE INPUT PARAMETER iClass       AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iInstance    AS handle   NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}

DEFINE BUFFER bFilCode FOR Code.
DEFINE BUFFER bPckCode FOR Code.
DEFINE BUFFER bDatCode FOR Code.

DEFINE VAR mFlagSet        AS LOGICAL INIT ?   NO-UNDO.
DEFINE VAR hPack           AS handle           NO-UNDO.
DEFINE VAR hCrdKeep        AS handle           NO-UNDO.
                          
DEFINE VAR mFormat         AS CHAR             NO-UNDO EXTENT 5.
DEFINE VAR hCrd            AS handle           NO-UNDO EXTENT 3.
DEFINE VAR mFlg            AS LOGICAL          NO-UNDO EXTENT 5.
DEFINE VAR mSig            AS CHAR             NO-UNDO EXTENT 5.
DEFINE VAR mPos            AS INT64            NO-UNDO EXTENT 5.
DEFINE VAR mLen            AS INT64            NO-UNDO EXTENT 5.
DEFINE VAR mProcIni        AS CHAR             NO-UNDO EXTENT 5.
DEFINE VAR mProcImp        AS CHAR             NO-UNDO EXTENT 5.
DEFINE VAR mItm            AS INT64            NO-UNDO.
DEFINE VAR mHnd            AS INT64            NO-UNDO.
DEFINE VAR mIDSelf         AS INT64            NO-UNDO.

DEFINE VAR mFileID         AS INT64             NO-UNDO.
DEFINE VAR mParentID       AS INT64             NO-UNDO.
DEFINE VAR mSeanceID       AS INT64             NO-UNDO.
DEFINE VAR mMailUser       AS INT64             NO-UNDO.
DEFINE VAR mItem           AS INT64             NO-UNDO.
DEFINE VAR mSign           AS CHAR              NO-UNDO.
DEFINE VAR mFilePath       AS CHAR              NO-UNDO.
DEFINE VAR mBuffer         AS CHAR              NO-UNDO.
DEFINE VAR mHeadSym        AS CHAR              NO-UNDO.
DEFINE VAR mBufferHead     AS CHAR              NO-UNDO.
DEFINE VAR mHeadLines      AS INT64             NO-UNDO.
DEFINE VAR mHeadCount      AS INT64             NO-UNDO.

DEFINE VAR mSeparate       AS CHAR              NO-UNDO.
DEFINE VAR mFlagSep        AS LOGICAL           NO-UNDO.
DEFINE VAR mThis           AS LOGICAL           NO-UNDO.
DEFINE VAR mCodePage       AS CHAR              NO-UNDO.
DEFINE VAR mNeedImpFF      AS CHARACTER         NO-UNDO.
DEFINE VAR mPackState      AS CHARACTER         NO-UNDO.
DEFINE VAR mCheckFirstLine AS CHARACTER         NO-UNDO. /* проверка правильности заголовка файла */
DEFINE VAR mFirstLine      AS LOGICAL           NO-UNDO.
DEFINE VAR mErrLine        AS CHARACTER         NO-UNDO.
DEFINE VAR mLineType       AS INT64             NO-UNDO.
DEFINE VAR mFileReName     AS CHARACTER         NO-UNDO.
DEFINE VAR mInitHead       AS CHARACTER         NO-UNDO. /* принудительный запуск метода инициализации заголовка */
DEFINE VAR mError          AS LOGICAL           NO-UNDO. /* Не ошибка, а отказ от импорта или тестовый импорт */
DEFINE VAR mErrorStatus    AS LOGICAL           NO-UNDO. /* Для запоминания ERROR-STATUS:ERROR */
DEFINE VAR mNewPath        AS CHARACTER         NO-UNDO.

DEFINE VAR h_dbfread       AS HANDLE            NO-UNDO.
DEFINE VAR mDBFError       AS INT64             NO-UNDO.

DEFINE VAR mInitH          AS LOGICAL   INIT NO NO-UNDO.  

DEFINE VARIABLE mSaveFile  AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mPacketID  AS INT64             NO-UNDO.
DEFINE VARIABLE mFileText  AS CHARACTER         NO-UNDO.

DEFINE VARIABLE mLine AS MEMPTR     NO-UNDO.


{intrface.get strng}
{intrface.get xclass}
{intrface.get tmess}

{intrface.get pbase}
{intrface.get trans}

{intrface.get filex}
{intrface.get exch}
{intrface.get pack}

{intrface.get xpos}

DEFINE STREAM sImport.

/*============================================================================*/
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hPack      = iInstance:default-buffer-handle
      mFileID    = hPack:buffer-field("FileExchID"):buffer-value
      mSeanceID  = hPack:buffer-field("SeanceID"):buffer-value
      mMailUser  = hPack:buffer-field("mail-user-num"):buffer-value
      mParentID  = hPack:buffer-field("PacketID"):buffer-value
      mSign      = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$SignItem")
      mSeparate  = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$Separate")
      mNeedImpFF = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$NeedImpFF")
      mErrLine   = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$IndefiniteLine") 
      mInitHead  = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$InitHeader")
      mHeadSym   = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$HeadSymbol")
      mHeadLines = INT64(GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$HeadLines"))
      mCodePage  = TRNSettingValue("","CodePage","ibm866")
      mCheckFirstLine = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$CheckFirstLine")
      mFileReName = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$FileReName")
      mPackState = {&STATE-FIN}
      mFirstLine = YES
      mSaveFile  = TRNSettingValue("","SaveFile","NO")
      mPacketID  = hPack:BUFFER-FIELD("PacketID"):BUFFER-VALUE
   NO-ERROR. {&ON-ERROR}

   mFilePath = FileGetPath(mFileID).             /* Обрабатываемый файл       */

   file-info:file-name = mFilePath NO-ERROR.
   IF NOT file-info:file-type BEGINS "F" THEN DO:
      RUN Fill-SysMes("","ComnExc05","","%s=" + GetNullStr(mFilePath)).
      UNDO MAIN, RETRY MAIN.
   END.

   IF mSign EQ {&RET-ERROR} THEN
      mItem = 1.
   ELSE DO:
      ASSIGN mItem = INT64(mSign) NO-ERROR. {&ON-ERROR}
   END.

   IF mSeparate EQ {&RET-ERROR} THEN DO:
     ASSIGN
      mSeparate = ""
      mFlagSep  = NO
    .
   END.
   ELSE DO:
      mFlagSep  = YES.
      IF mSeparate EQ 'DBF' THEN mFlagSep = ?.
   END.

   IF mErrLine EQ {&RET-ERROR} THEN
      mLineType = 0.
   ELSE DO:
      ASSIGN mLineType = INT64(mErrLine) NO-ERROR. {&ON-ERROR}
   END.

   {getsncmlu.i MAIN imp}                        /* Get Seance AND mail-user  */

/*------------- Определение всех реквизитов для разбора  и обработки данных --*/
   {crdfrmt.i &op-kind = GetBaseOpKind()
              &op-tmpl = GetBaseTemplate()
              &frm     = mFormat
              &hdl     = hCrd
              &code    = Code
              &flg     = mFlg
              &sgn     = mSig
              &pos     = mPos
              &len     = mLen
              &procini = mProcIni
              &procimp = mProcImp
              &itm     = mItem
   }

   CREATE                                      widget-pool "I-GCROWN-P" NO-ERROR.
   CREATE BUFFER hCrdKeep FOR TABLE hCrd[3] IN widget-pool "I-GCROWN-P" NO-ERROR.

   IF mFlagSep NE ? THEN DO:
      INPUT FROM VALUE(mFilePath) BINARY.
      SEEK INPUT TO END .
      SEEK INPUT TO (SEEK(INPUT) - 1) .
      SET-SIZE(mLine) = 0 .
      SET-SIZE(mLine) = 1 .
      IMPORT UNFORMATTED mLine .
      INPUT CLOSE .
      IF get-byte(mLine,1) <> 10 THEN DO:  
        OUTPUT TO VALUE(mFilePath) APPEND .
        PUT UNFORMATTED "~n" .
        OUTPUT CLOSE .
      END.
      SET-SIZE(mLine) = 0 . 
   END.

/*-------------------------------------------------- Чтение данных из файла --*/
   IF mFlagSep NE ? THEN DO:
      INPUT STREAM sImport FROM VALUE(mFilePath) CONVERT SOURCE mCodePage.
   END.
   ELSE DO:
      RUN dbfread.p PERSISTENT SET h_dbfread NO-ERROR.
      IF NOT VALID-HANDLE(h_dbfread) THEN DO:
         RUN Fill-SysMes("","","-1","Ошибка при загрузке dbf-библиотеки.").
         UNDO MAIN, RETRY MAIN.
      END.
      RUN DBFOpenRead IN h_dbfread (mFilePath, OUTPUT mDBFError).
      IF mDBFError NE 0 THEN DO:
        RUN Fill-SysMes("","","-1","Ошибка при открытии dbf-файла.").
        UNDO MAIN, RETRY MAIN.
      END.
   END.

   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:

      RUN SetSysConf IN h_base ("Error","No").

      READ:
      REPEAT ON ERROR UNDO READ, RETRY READ:
         IF RETRY THEN DO:
            IF mSaveFile EQ "Да" OR mSaveFile EQ "YES" THEN
               RUN PacketTextSave IN h_pack (mPacketID, mFileText).
            IF mFlagSep NE ? THEN DO:
               INPUT STREAM sImport CLOSE.
            END.
            ELSE DO:
               RUN DBFCloseRead IN h_dbfread.
               DELETE OBJECT h_dbfread.
            END.
            UNDO MAIN, RETRY MAIN.
         END.
      
         IF mFlagSep NE ? THEN DO:
            IMPORT STREAM sImport UNFORMATTED mBuffer. /* Текущая строка            */
         END.
         ELSE DO:
            RUN DBFRead IN h_dbfread (OUTPUT mDBFError).
            IF mDBFError NE 0 THEN LEAVE READ.
            RUN DBFGetRecord IN h_dbfread (OUTPUT mBuffer, OUTPUT mDBFError).
         END.
         
         IF mSaveFile EQ "Да" OR mSaveFile EQ "YES" THEN
            RUN PacketTextKeep (mPacketID, mBuffer + "~n", INPUT-OUTPUT mFileText).
      
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-gcrown.p","mBuffer:" + GetNullStr(mBuffer)).
         &ENDIF
         /*---------------------------------------- Определим формат для текущей строки --*/
         mThis = NO.
         IF mLineType < 10 THEN DO:
             FND-FMT:
             DO mItm = 1 TO 5:
             
                IF NOT mFlg[mItm] THEN NEXT FND-FMT.    /* Формат не обрабатывается  */
               
                IF mFlagSep THEN
                DO:
                   IF mBuffer BEGINS mHeadSym THEN
                   DO:
                      ASSIGN
                         mBufferHead = mBufferHead + mSeparate WHEN {assigned mBufferHead}
                         mBufferHead = mBufferHead + TRIM(ENTRY(1, mBuffer, mSeparate), mHeadSym)
                         mHeadCount  = mHeadCount  + 1
                      .
                      IF mHeadLines NE mHeadCount THEN
                         NEXT READ.
                      ELSE /* IF mHeadLines NE mHeadCount THEN */ 
                      DO:
                         ASSIGN
                            mBuffer = mBufferHead
                            mThis   = YES
                         .
                      END. /* IF mHeadLines NE mHeadCount THEN ... ELSE */
                   END. /* IF mBuffer BEGINS mHeadSym THEN */
                   ELSE 
                      mThis = TRIM(ENTRY(mItem,mBuffer,mSeparate))     EQ TRIM(mSig[mItm]).
                END. /* IF mFlagSep THEN */
                ELSE IF mFlagSep EQ ? THEN
                   mThis = (mItm EQ 3).
                ELSE 
                   mThis = SUBSTRING(mBuffer,mPos[mItm],mLen[mItm]) EQ mSig[mItm].
                
                IF mFirstLine THEN DO:
                   IF NOT mThis AND mItm EQ 1 AND mCheckFirstLine EQ "YES" THEN DO:  /* Неправильный заголовок файла */
                       RUN fill-SysMes("","","-1","Неправильная первая запись, файл не обрабатывается.").
                       UNDO MAIN, RETRY MAIN.
                   END.
                   mFirstLine = NO.
                END.
                
                IF mThis THEN LEAVE FND-FMT.
             END.                                       /* DO mItm = 1 TO 5:         */
         END.
         ELSE DO:
           mItm = mLineType MOD 10.
           mThis = YES.
         END.

         IF NOT mThis THEN DO:                      /* Неизвестная строка        */
            IF mLineType EQ 0 OR mLineType > 5 THEN NEXT READ.
            ELSE mItm = mLineType.
         END.
      
         /*-------------------------------------------------- Выполним обработку строки --*/
         CASE mItm:
            WHEN 4 THEN mHnd = 2.
            WHEN 5 THEN mHnd = 1.
            OTHERWISE   mHnd = mItm.
         END CASE.
      
         /*------------------------------------------------- Вызов метода инициализации --*/
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-gcrown.p","mItm:" + GetNullInt(mItm) +
                                     " mHnd:" + GetNullInt(mHnd) +
                                   " FORMAT:" + GetNullStr(mFormat[mItm]) +
                                    " Class:" + GetNullStr(hCrd[mHnd]:Name) +
                                     " INIT:" + GetNullStr(mProcIni[mItm]) +
                                      " Imp:" + GetNullStr(mProcImp[mItm])).
         &ENDIF
         
         IF (mInitHead EQ "yes" OR mInitHead EQ "да") AND 
            (NOT mInitH) AND (mItm NE 1)              AND 
            ({assigned mProcIni[1]})                  THEN 
         DO:
            mInitH = YES.
            {exch-run.i &Proc = mProcIni[1]
                        &Parm = "mFormat[1], hCrd[1]"
            }
            {&ON-ERROR}
         END.
      
         IF {assigned mProcIni[mItm]} THEN DO:
            {exch-run.i &Proc = mProcIni[mItm]
                        &Parm = "mFormat[mItm], hCrd[mHnd]"
                        &RunNoDef   = YES
            }
            {&ON-ERROR}
         END.                                       /* IF {assigned mProc[mItm]} */
      
         /*--------------------------------------- Создание экземпляра для строк данных --*/
         IF mItm EQ 3 THEN DO:
            ASSIGN
               mIDSelf = hCrd[3]:buffer-field("ID-Self"):buffer-value
            NO-ERROR.
      
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("I-GCROWN-P","mIDSelf:" + GetNullInt(mIDSelf) +
                                            " err:" + ERROR-STATUS:get-message(1)).
            &ENDIF
      
            IF ERROR-STATUS:ERROR OR mIDSelf EQ ? THEN mIDSelf = 0.
            RUN InstanceCreateEx (hCrdKeep, hCrd[3], mIDSelf).
      
            ASSIGN
               hCrd[3]::PacketID = hCrd[1]::PacketID
               hCrd[3]::ExchMain = STRING(hCrd[1])
            NO-ERROR. {&ON-ERROR}
         END.
      
         /*--------------------------------------------------- Разбор строки по формату --*/
         IF mFlagSep THEN
            RUN XSEPMakeImport (mFormat[mItm],
                                mBuffer,
                                mSeparate).
         ELSE
            RUN XPOSMakeImport (mFormat[mItm],
                                mBuffer).
      
         /*------------------------------------------------------- Вызов метода импорта --*/
         IF {assigned mProcImp[mItm]} THEN DO:
            {exch-run.i &Proc       = mProcImp[mItm]
                        &Parm       = "mFormat[mItm], hCrd[mHnd]"
                        &RunNoDef   = YES
            }
      
            IF ERROR-STATUS:ERROR THEN 
            DO:
               IF mFileReName EQ "Yes" THEN 
               DO:
                  mError = GetSysConf("Error") EQ "Yes".
                  FOR FIRST Packet WHERE Packet.PacketID EQ mParentID
                                   NO-LOCK, 
                     FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID 
                                    EXCLUSIVE-LOCK:
                  
                     OS-COPY VALUE(FileExch.Path) VALUE(SUBSTRING(FileExch.Path,1,r-index(FileExch.Path,"/")) + "ошибка_" + FileExch.NAME).
                  
                     IF os-error NE 0 THEN
                        RUN fill-SysMes("","","-1","Невозможно выполнить команду OS-COPY. Код ошибки " + string(os-error)).
                  
                     RUN FileMoveArchive(mParentID, "ImpArch").
                  END. /* FOR FIRST Packet WHERE Packet.PacketID EQ mParentID */
               END. /* IF mFileReName EQ "Yes" THEN  */

               UNDO MAIN, RETRY MAIN. 
            END. /* IF ERROR-STATUS:ERROR THEN */

            IF mItm         EQ 1 AND
               RETURN-VALUE EQ "SKIP" THEN DO:
               mPackState =  {&STATE-VER}.
               RUN Fill-SysMes ("","ExchCRD14","","%s=" + mFilePath).
               LEAVE READ.
            END. 
            IF mItm         EQ 3 AND
               RETURN-VALUE EQ "SKIP" THEN DO:
               mPackState =  {&STATE-ERR}.
               RUN Fill-SysMes ("","ExchCRD14","","%s=" + mFilePath).
               LEAVE READ.
            END.   
         END.                                 /* IF {assigned mProc[mItm]} */
      END.                                          /* READ: REPEAT              */

      IF mSaveFile EQ "Да" OR mSaveFile EQ "YES" THEN
         RUN PacketTextSave IN h_pack (mPacketID, mFileText).
     
      IF mNeedImpFF EQ "yes" THEN /* выполнить метод импорта хвостовика (проверка сумм) */
      DO:  

         {exch-run.i &Proc       = mProcImp[5]
                     &Parm       = "mFormat[5], hCrd[1]"
                     &RunNoDef   = YES}                                                                           

         IF ERROR-STATUS:ERROR THEN
         DO:
            IF mFileReName EQ "Yes" THEN 
            DO:
               FIND FIRST Packet   WHERE Packet.PacketID     EQ mParentID         NO-LOCK NO-ERROR. 
               IF AVAIL(Packet) THEN
                  FIND FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID EXCLUSIVE-LOCK NO-ERROR. 
               IF AVAIL(FIleExch) THEN
               DO:
                  mNewPath = SUBSTRING(FileExch.Path,1,R-INDEX(FileExch.Path,"/")) + "ошибка_" + FileExch.NAME.
                  OS-RENAME VALUE(FileExch.Path) VALUE(mNewPath).
                  FileExch.Path = mNewPath.
                  RUN FileMoveArchive(mParentID,"ImpArch").
               END.
               RUN SetSysConf IN h_base ("Error","No").
            END.
            UNDO MAIN, RETRY MAIN. 
         END. /* IF ERROR-STATUS:ERROR THEN */
      END. /* IF mNeedImpFF EQ "yes" THEN */

      IF ERROR-STATUS:ERROR THEN
      DO:
         UNDO TR, LEAVE TR.
      END.
      
   END.  /* End of TR BLOCK */

   IF mFlagSep NE ? THEN
      INPUT STREAM sImport CLOSE.
   ELSE DO:
      RUN DBFCloseRead IN h_dbfread.
      DELETE OBJECT h_dbfread.
   END.

   RUN PacketSetState IN h_pack (mParentID,mPackState) NO-ERROR.
   RUN FileMoveArchive IN h_filex (mParentID,"ImpArch") NO-ERROR.

   mFlagSet = YES.
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

/* Не забываем закрыть за собой файлы */
IF mFlagSet NE YES THEN 
DO:
   IF mFlagSep NE ? THEN 
      INPUT STREAM sImport CLOSE.
   ELSE 
   DO:
      RUN DBFCloseRead IN h_dbfread.
      DELETE OBJECT h_dbfread.
   END.
END. /* IF mFlagSet NE YES THEN  */

DELETE widget-pool "I-GCROWN-P" NO-ERROR.

{intrface.del}
{doreturn.i mFlagSet}
/******************************************************************************/
