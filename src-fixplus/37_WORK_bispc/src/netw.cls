/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: netw.cls
      Comment: <A.Krajuschkin>
   Parameters: нет
         Uses: nwsocket.cls
      Used by: netwx.cls
      Created: 25.04.2014 12:00 Krag    
     Modified: 25.04.2014 14:56 Krag     <0228509 обновление bispc>
     Modified: 27.11.2014 11:23 Krag     <0245758 исправил работу с bq-sshtnl.pl>
*/
   {nwtrace.i}
   &SCOPED-DEFINE LIB_VERSION           "2.4"
   &SCOPED-DEFINE DEFAULT_IP_ADDRESS    "127.0.0.1"
   &SCOPED-DEFINE DEFAULT_IP_PORT       515
/*----------------------------------------------------------------
File: netw.cls
Package:
Description: Базоый класс для работы с утилитой.
----------------------------------------------------------------*/
CLASS netw INHERITS nwsocket:
   {pfuncdef &defclass="netw"}
   /*----------------------------------------------------------------
   Интерфейсы                                                
   ----------------------------------------------------------------*/
   {intrface.obj}
   /*----------------------------------------------------------------
   Таблицы                                                
   ----------------------------------------------------------------*/
   DEFINE PRIVATE TEMP-TABLE finger NO-UNDO
      FIELD login      AS CHARACTER
      FIELD device     AS CHARACTER
      FIELD idle       AS CHARACTER
      FIELD login-time AS CHARACTER
      FIELD machine    AS CHARACTER
      INDEX i1 device INDEX i2 login INDEX i3 machine.
   /*----------------------------------------------------------------
   Поля                                               
   ----------------------------------------------------------------*/
   DEFINE PROTECTED VARIABLE mBufferI           AS CHARACTER NO-UNDO.
   DEFINE PROTECTED VARIABLE mFlag              AS INT64 NO-UNDO.
   DEFINE PROTECTED VARIABLE mCmdBufferSize     AS INT64 NO-UNDO.
   DEFINE PROTECTED VARIABLE mDataBufferSize    AS INT64 NO-UNDO.
   DEFINE PROTECTED VARIABLE mServVer           AS CHARACTER NO-UNDO.
   /*----------------------------------------------------------------
   Свойства       
   ----------------------------------------------------------------*/
   /* Адрес */
   DEFINE PUBLIC PROPERTY Address AS CHARACTER NO-UNDO 
                                     INIT {&DEFAULT_IP_ADDRESS} 
                                     GET. PRIVATE SET.
   /* Порт */
   DEFINE PUBLIC PROPERTY Port AS INTEGER NO-UNDO
                                          INIT {&DEFAULT_IP_PORT}
                                          GET. SET.
   /* SSH */
   DEFINE PUBLIC PROPERTY UseCustomPort AS LOGICAL NO-UNDO 
                                         GET. PRIVATE SET.

   /* Время паузы в миличекундах */
   DEFINE PUBLIC PROPERTY BloddPingTime AS INT64 INIT 10 NO-UNDO 
                                         GET. PRIVATE SET.

   DEFINE PRIVATE STATIC VARIABLE mGUID AS CHAR INIT ? NO-UNDO.

   /* Размер переданных ServerBvn файлов */
   DEFINE PUBLIC STATIC PROPERTY TransSize AS INT64 INIT 1 NO-UNDO GET. SET. 

   /* GUID сессии */
   DEFINE PUBLIC STATIC PROPERTY GUID AS CHAR INIT ? NO-UNDO 
   GET:
      IF NOT {assigned mGUID} THEN
         mGUID = GUID.
      RETURN mGUID.  
   END GET. PRIVATE SET.

   /*----------------------------------------------------------------
   Конструкторы
   ----------------------------------------------------------------*/
   CONSTRUCTOR PUBLIC netw():
      {&INIT_ALL_LIBS} 
      ASSIGN 
         mCmdBufferSize  = 128
         mDataBufferSize = 1024.

      BloddPingTime = INT64(FGetSetting ("BISSMART", "PingTime", "50")) NO-ERROR.
      Initialize().
   END CONSTRUCTOR.
   /*----------------------------------------------------------------
   Деструктор
   ----------------------------------------------------------------*/
   DESTRUCTOR netw():
      {empty finger}
      {&DELETE_ALL_LIBS}
   END DESTRUCTOR.
   /*----------------------------------------------------------------
   Методы
   ----------------------------------------------------------------*/
   /*----------------------------------------------------------------
   Инициализация   
   ----------------------------------------------------------------*/
   METHOD PROTECTED VOID Initialize():
   &IF DEFINED(SESSION-REMOTE) = 0 &THEN
      ASSIGN
         UseCustomPort = FGetSetting("TermPC",
                                     "SSHTunnel",
                                     "NO") EQ "YES"
         Address = GetUserHost()
         Port = GetUserPort().
   &ELSE
      Address = GetUserTerm(?).
   &ENDIF
   END METHOD.
   /*----------------------------------------------------------------
   Установка адреса
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="getuserhost"
    &description="устанавливает адрес клиента"}
   METHOD PUBLIC CHARACTER GetUserHost():
      DEFINE VARIABLE vIp AS CHARACTER  NO-UNDO INIT ?.
      /* устанавливаем адрес хоста юзера, который запустил */
      IF UseCustomPort THEN
         vIp = {&DEFAULT_IP_ADDRESS}.
      ELSE DO:
         INPUT THROUGH 'bq-ip'.
         IMPORT vIp NO-ERROR.
         INPUT CLOSE.
         vIp = TRIM(vIp).
      END.
      FINALLY:
         RETURN vIp.
      END FINALLY.
   END METHOD.
   /*----------------------------------------------------------------
   Ждет ответа от клиента
   ----------------------------------------------------------------*/
   METHOD PROTECTED OVERRIDE INT64 AwaitResponce():
      ASSIGN 
      mBufferI = ?
      mFlag    = 1.
      SUPER:AwaitResponce().
      RETURN mFlag.
   END METHOD.
   /*----------------------------------------------------------------
   Установка порта
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="getuserport"
    &description="устанавливает порт клиента"}
   METHOD PUBLIC INTEGER GetUserPort():
      DEFINE VARIABLE vPort      AS INTEGER NO-UNDO INIT -1.
      DEFINE VARIABLE vPortList  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vItem      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE vRet       AS CHARACTER NO-UNDO.
      /* устанавливаем порт подключения к  */
      IF UseCustomPort EQ FALSE THEN DO:
         vPort = INTEGER(FGetSetting("TermPC",
                                     "PortNum",
                                     "{&DEFAULT_IP_PORT}")) NO-ERROR.
      END.
      ELSE DO:         
         /* если идет проброс порта через ssh клиент */
         vPortList = GetPortList().
         IF NOT {assigned vPortList} THEN
            RETURN -1.
         /* находим открытый порт клиента */
         PORT_REPEAT:
         DO vItem = 1 TO NUM-ENTRIES(vPortList):
            vPort = INTEGER(ENTRY(vItem, vPortList)) NO-ERROR.
            IF vPort EQ 0 THEN 
               NEXT PORT_REPEAT.
            IF THIS-OBJECT:Ping(Address,vPort,OUTPUT vRet) EQ 0
            AND vRet BEGINS "bispc-" THEN
               LEAVE PORT_REPEAT.
         END.
      END.
      RETURN vPort.
   END METHOD.
   /*----------------------------------------------------------------
   Возвращает список портов подключения при работе через ssh-тунели
   ----------------------------------------------------------------*/
   METHOD PRIVATE CHARACTER GetPortList():
      DEFINE VARIABLE vScript    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vPortList  AS CHARACTER NO-UNDO.
      /* поиск скрипта в рабочем каталоге */
      vScript = filehelper:GetAbsolutePath('bq-sshtnl.pl').
      /* если не находим, берем из path */
      IF vScript EQ ? THEN 
         vScript = 'bq-sshtnl.pl'.
      INPUT THROUGH VALUE(vScript).
      IMPORT vPortList NO-ERROR.
      INPUT CLOSE.
      FINALLY:
         RETURN vPortList.
      END FINALLY.
   END METHOD.
   /*----------------------------------------------------------------
   Возвращает адрес терминала
   Parameters:  iUser - имя пользователя
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="getuserterm"
    &description="возвращает адрес терминала"
    &parameters="пользователь"
    &result="адрес"}
   METHOD PUBLIC CHARACTER GetUserTerm(INPUT iUser AS CHARACTER):
      DEFINE VARIABLE vDev    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vIP     AS CHARACTER NO-UNDO.
      &IF DEFINED(SESSION-REMOTE) &THEN
      RUN GetBloddConnectParams(OUTPUT vIP,OUTPUT vDev).
      &ELSE
      vIP = Address.
      IF NOT(NUM-ENTRIES(vIP, ".") GT 2 AND INDEX(vIP, " ") EQ 0) THEN
      DO:
         IF OPSYS EQ "win32" THEN 
            vIP = "localhost".
         ELSE DO:
            makeFigerTable().
            IF NOT {assigned iUser}  THEN iUser = OS-GETENV("BQWhoAmI").
            IF NOT {assigned iUser}  THEN iUser = OS-GETENV("LOGNAME").
            vDev = REPLACE(OS-GETENV("BQTTY"), "/", "").
            FIND FIRST finger WHERE finger.login EQ iUser 
                              AND finger.device EQ vDev NO-ERROR.
            IF NOT AVAILABLE finger THEN 
               FIND FIRST finger WHERE finger.login EQ iUser NO-ERROR.
            IF AVAILABLE finger THEN
              vIP = finger.machine.
         END.
      END.
      &ENDIF
      RETURN vIP.
   END METHOD.
   /*----------------------------------------------------------------
   Создает таблицу пользователей finger
   ----------------------------------------------------------------*/
   METHOD PRIVATE VOID MakeFigerTable():
      DEFINE VARIABLE vServName AS CHARACTER NO-UNDO.
      IF opsys = "unix" THEN
      DO:
         INPUT THROUGH 'uname -n'.
         IMPORT vServName.
         INPUT CLOSE.
      END.
      ELSE 
         IF INDEX(DBPARAM("bisquit"),"-H") > 0 THEN
         DO:
            vServName = ENTRY(1,
                        SUBSTRING(DBPARAM("bisquit"),
                        INDEX(DBPARAM("bisquit"),"-H") + 3)).
         END.
   
      {empty finger}
      INPUT THROUGH 'bq-finger' VALUE(vServName).
      REPEAT ON ERROR UNDO, LEAVE:
         CREATE finger.
         IMPORT finger.
         IF finger.device = "" OR finger.device = "TTY" THEN
            DELETE finger.
         ELSE
         DO:
           ASSIGN
               finger.machine  = TRIM(finger.machine)
               finger.device   = REPLACE(finger.device,"/","").
           IF INDEX(finger.device,"pts") GT 0 THEN 
              finger.device   = REPLACE(finger.device,
                                SUBSTRING(finger.device,4),
                                IF LEFT-TRIM(SUBSTRING(finger.device,
                                                       4),"0") NE ""
                                      THEN LEFT-TRIM(SUBSTRING(finger.device,
                                                               4),"0")
                                      ELSE "0").
         END.
      END.
      INPUT CLOSE.
   END METHOD.

   /*----------------------------------------------------------------
   Проверка соединения с клиентом
   Parameters:  oRet - ответ   
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="ping"
    &description="проверяет соединене"
    &parameters="ответ"}
   METHOD PUBLIC INT64 Ping(OUTPUT oRet AS CHARACTER):
     RETURN THIS-OBJECT:Ping(Address,Port,OUTPUT oRet).
   END METHOD.
   /*----------------------------------------------------------------
   Проверка соединения с клиентом
   Parameters:  iHost - адрес
                iPort - порт
                oRet - ответ   
   ----------------------------------------------------------------*/
   METHOD PRIVATE INT64 Ping(INPUT iHost AS CHARACTER,
                             INPUT iPort AS INTEGER,
                             OUTPUT oRet AS CHARACTER):
      DEFINE VARIABLE vResult  AS INT64   NO-UNDO.
      IF THIS-OBJECT:CONNECT(iHost,iPort,FALSE) EQ FALSE THEN 
         RETURN {&ERROR_CONNECT_FAIL}.
      WriteSocket(1, -1, {&LIB_VERSION}).
      vResult = THIS-OBJECT:AwaitResponce().
      IF vResult EQ 0 THEN
         oRet = mBufferI.
      FINALLY:
         THIS-OBJECT:DISCONNECT().
         RETURN vResult.
      END FINALLY.
   END METHOD.
   /*----------------------------------------------------------------
   Возвращает версию клиента
   Parameters:  iVer - ответ от клиента
   ----------------------------------------------------------------*/
   METHOD PRIVATE INT64 GetVersion(INPUT iVer AS CHARACTER):
      DEFINE VARIABLE vVersNumb AS INT64      NO-UNDO.
      DEFINE VARIABLE vItem     AS INT64      NO-UNDO.
      DEFINE VARIABLE vStrINT   AS INT64      NO-UNDO.
      DEFINE VARIABLE vItemThou AS INT64      NO-UNDO.
      DEFINE VARIABLE vStrTMP   AS CHARACTER  NO-UNDO.
      ASSIGN
         vStrTMP   = SUBSTRING(iVer, 7)
         vItemThou = 100000000.
       DO vItem = 1 TO NUM-ENTRIES(vStrTMP, "."):
          vStrINT = INT64(ENTRY(vItem, vStrTMP, ".")) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
             vVersNumb = vVersNumb + vItemThou * vStrINT.
          vItemThou = vItemThou / 100.
       END.
      RETURN vVersNumb.
   END METHOD.
   /*----------------------------------------------------------------
   Отправляет команду на клиент
   Parameters:  iCommand - номер команды
                iCount - количество
                iParam - параметры
   ----------------------------------------------------------------*/
   METHOD PROTECTED VOID WriteSocket(INPUT iCommand AS INT64,
                                     INPUT iCount   AS INT64,
                                     INPUT iParam   AS CHARACTER):
      DEFINE VARIABLE vBuffer         AS MEMPTR    NO-UNDO.
      DEFINE VARIABLE vStrPar         AS CHARACTER NO-UNDO.
      IF VALID-HANDLE(mSocket) THEN 
      DO:
         ASSIGN
            SET-SIZE(vBuffer)   = mCmdBufferSize
            PUT-BYTE(vBuffer,1) = iCommand
            vStrPar             = IF iCount < 0 THEN
                                  iParam ELSE
                                  SUBSTITUTE("&1 &2",
                                             STRING(iCount),
                                             iParam)
            vStrPar             = vStrPar + CHR(0)
            PUT-STRING(vBuffer,2) = vStrPar.
         mSocket:WRITE(vBuffer, 1, LENGTH(vStrPar) + 2) NO-ERROR.
         asyncrunhelper:Sleep (BloddPingTime, NO).
         SET-SIZE(vBuffer) = 0.
      END.
   END METHOD.
   /*----------------------------------------------------------------
   Устанавливает соедниение
   Parameters:  iEcho - ответ
                oVersion - версия
   ----------------------------------------------------------------*/
   METHOD PRIVATE INT64 CONNECT(INPUT iEcho     AS CHARACTER,
                                OUTPUT oVersion AS CHARACTER):

    DEFINE VARIABLE vIsInit   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vResult   AS INT64 NO-UNDO.
    IF THIS-OBJECT:CONNECT(Address,Port,FALSE) EQ FALSE THEN 
      RETURN {&ERROR_CONNECT_FAIL}.
    WriteSocket(1, -1, iEcho).
    IF AwaitResponce() NE 0 THEN
        vResult = {&ERROR_RESPONCE_FAIL}.
    ELSE DO:
      oVersion = TRIM(mBufferI).
      IF NOT oVersion BEGINS "bispc-" THEN
         vResult = {&ERROR_VERSION}.
      ELSE
         vIsInit = YES.
      IF NOT vIsInit THEN
         THIS-OBJECT:DISCONNECT().
    END.
    RETURN vResult.
   END METHOD.
   /*----------------------------------------------------------------
   Устанавливает соедниение
   ----------------------------------------------------------------*/
   METHOD PROTECTED LOGICAL CONNECT():
      GetUserTerm(?).
      RETURN SUPER:CONNECT(Address,Port,FALSE).
   END METHOD.
   /*----------------------------------------------------------------
   Синхронизирует файлы
   Parameters:  iLocalDirectory  - каталог на сервере
                iRemoteDirectory - каталог на клиенте
                iRemoteFile      - маска или имя файла
  ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="syncfiles"
    &description="синхронизирует файлы"
    &parameters="лок.каталог,клиент.каталог,маска"
    &result="код"}
   METHOD PUBLIC INT64 SyncFiles(INPUT iLocalDirectory AS CHARACTER,
                                 INPUT iRemoteDirectory AS CHARACTER,
                                 INPUT iRemoteFile AS CHARACTER):
      RETURN {&ACTION_UNSUPPORTED}.
   END METHOD.
   /*----------------------------------------------------------------
   Выполняет подписание на клиенте
   Parameters:  iSubject - текст поиска сертификата
                iLocal - каталог на сервере
                iMask - маска
                iCrl - файл отзыва
                iTime - таймаут
                iData - данные результата
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="signdirectory"
    &description="подписывает файлы"
    &parameters="сертификат,каталог.сервера,маска,отзыв,таймаут,результат"
    &result="код"}
   METHOD PUBLIC INT64 SignDirectory(INPUT iSubject   AS CHARACTER,
                                     INPUT iLocal AS CHARACTER,
                                     INPUT iMask   AS CHARACTER,
                                     INPUT iCrl AS CHARACTER,
                                     INPUT iTime AS INT64,
                                     INPUT iData AS MEMPTR):
      RETURN {&ACTION_UNSUPPORTED}.
   END METHOD.
   /*----------------------------------------------------------------
   Отправляет файл на клиент
   Parameters:  iFile    - имя файла
                iFileType   - класс
                iParams  - дополнительные параметры
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="sendfile"
    &description="отправляет файл"
    &parameters="файл,тип,параметры"
    &result="код"}
   METHOD PUBLIC INT64 SendFile(INPUT iFile AS CHARACTER,
                                INPUT iFileType AS CHARACTER,
                                INPUT iParams AS CHARACTER):
      DEFINE VARIABLE vLen          AS INT64       NO-UNDO.
      DEFINE VARIABLE vVersion      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vShrtFile     AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vReturnCode   AS INTEGER     NO-UNDO.
      DEFINE VARIABLE vBytes        AS INT64       NO-UNDO.
      MAIN-BLOCK:
      DO:
         /* проверяем файл */
         IF NOT filehelper:FileExists(iFile) THEN DO:
            vReturnCode = {&ERROR_FILE_FAIL}.
            LEAVE MAIN-BLOCK.
         END.
         /* устанавливаем соединение */
         vReturnCode = CONNECT(iFileType,OUTPUT vVersion).
         IF vReturnCode GT 0 THEN 
            LEAVE MAIN-BLOCK.
         /* отправляем имя файла */
         vShrtFile = filehelper:GetShortFilename(iFile).
         WriteSocket(2, -1, vShrtFile).
         IF AwaitResponce() NE 0 OR mBufferI NE vShrtFile THEN DO:
            vReturnCode = {&ERROR_SEND_FAIL}.
            LEAVE MAIN-BLOCK.
         END.
         IF {assigned iFileType} THEN DO:
           WriteSocket(4, -1, iFileType).
           AwaitResponce().
         END.
         IF GetVersion(vVersion) GE 2000000 THEN DO:
           IF {assigned iParams} THEN DO:
              WriteSocket(5, -1, iParams).
              IF AwaitResponce() NE 0 THEN DO:
                 vReturnCode = {&ERROR_RESPONCE_FAIL}.
                 LEAVE MAIN-BLOCK.
              END.
           END.
         END.
         SendFileData(iFile,
                      mDataBufferSize,
                      OUTPUT vBytes,
                      OUTPUT vLen).
         IF mFlag NE 0 OR vBytes NE vLen THEN DO:
           vReturnCode = {&ERROR_SEND_FAIL}.
           LEAVE MAIN-BLOCK.
         END.
      END.
      FINALLY:
         THIS-OBJECT:DISCONNECT().
         RETURN vReturnCode.
      END FINALLY.
   END METHOD.
   /*----------------------------------------------------------------
   Отправляет файл на клиент
   ----------------------------------------------------------------*/
   METHOD PRIVATE VOID SendFileData(INPUT iFile    AS CHARACTER,
                                    INPUT iBufSize AS INT64,
                                    OUTPUT oByte   AS INT64,
                                    OUTPUT oLen    AS INT64):
      DEFINE VARIABLE vBuffer     AS MEMPTR NO-UNDO.
      DEFINE VARIABLE vBufferSize AS INT64 NO-UNDO.
      DEFINE VARIABLE vFileOffset AS INT64 NO-UNDO.
      DEFINE VARIABLE vRemainSize AS INT64   NO-UNDO.
   
       IF mSocket:CONNECTED() AND {assigned iFile} THEN
       DO:
          ASSIGN
            FILE-INFO:FILE-NAME = iFile
            oLen = FILE-INFO:FILE-SIZE.
          WriteSocket(3, oLen, filehelper:GetShortFilename(iFile)).
          SENDFILE_REPEAT:
          REPEAT WHILE vFileOffset LT oLen:
            vRemainSize = oLen - vFileOffset.
            IF vRemainSize < iBufSize THEN DO:
               vBufferSize = vRemainSize.
            END.
            ELSE 
               vBufferSize = iBufSize.
            SET-SIZE(vBuffer) = vBufferSize.
            COPY-LOB FROM FILE iFile STARTING AT vFileOffset + 1 
                     FOR vBufferSize TO vBuffer NO-CONVERT NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               LEAVE SENDFILE_REPEAT.
            END.
            mSocket:WRITE(vBuffer, 1, vBufferSize) NO-ERROR.
            asyncrunhelper:Sleep (BloddPingTime, NO).
            IF ERROR-STATUS:ERROR THEN DO:
               LEAVE SENDFILE_REPEAT.
            END.
            vFileOffset = vFileOffset + vBufferSize.
         END.

         DEF VAR vI AS INT64 NO-UNDO.
         DO vI = 1 TO 3:
            SUPER:AwaitResponce(). 
            oByte = INT64(TRIM(mBufferI)).
            IF oByte <= 0 THEN DO:
               PAUSE 1 NO-MESSAGE.
            END.
            ELSE 
               LEAVE.
         END.

         ASSIGN
            SET-SIZE(vBuffer) = 0
            oByte = INT64(TRIM(mBufferI))
            NO-ERROR.
       END.
   END METHOD.
   /*----------------------------------------------------------------
   Возвращает данные из сокета
   ----------------------------------------------------------------*/
   {pfuncdef
    &defmeth="readresponseprocedure"
    &defover="true"
    &description="возвращает данные из сокета"}
   METHOD PUBLIC OVERRIDE VOID ReadResponseProcedure():
      DEFINE VARIABLE vBuffer AS MEMPTR  NO-UNDO.
      DEFINE VARIABLE vLen    AS INT64   NO-UNDO.
      DEFINE VARIABLE vItem    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE vStrTMP  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vIntTMP  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE vIsRead  AS LOGICAL   NO-UNDO.
      IF NOT VALID-HANDLE(mSocket) THEN 
         RETURN.
      IF NOT mSocket:CONNECTED() THEN 
         RETURN.
      ASSIGN
         SET-SIZE(vBuffer) = mDataBufferSize
         vLen = 1.
      mSocket:READ(vBuffer, 1, vLen, READ-EXACT-NUM).
      ASSIGN
         mBufferI = CHR(GET-BYTE(vBuffer,1))
         vStrTMP  = SUBSTRING(mBufferI, 1, 1)
         vIsRead  = YES
         vItem    = vLen
         vIntTMP  = 1.
      IF (mServVer EQ "" AND vStrTMP EQ "b") THEN
      DO:
         DO WHILE (vIsRead):
            vItem = vItem + 1.
            mSocket:READ(vBuffer, vItem, 1, READ-EXACT-NUM).
            ASSIGN
               vIntTMP  = GET-BYTE(vBuffer, vItem)
               vStrTMP  = CHR(vIntTMP)
               mBufferI = mBufferI + vStrTMP.
            IF (vItem GE 6 AND 
               NOT mBufferI BEGINS "bispc-") THEN
               vIsRead = NO.
            IF (vItem LE 6 AND 
                SUBSTRING("bispc-", 1, vItem) NE mBufferI) THEN
               vIsRead = NO.
            IF mBufferI BEGINS "bispc-0." THEN
            DO:
               vIsRead = NO.
            END.
            IF vIntTMP EQ 0 THEN
            DO:
               vIsRead = NO.
            END.
            mServVer = SUBSTRING(mBufferI, 7).
         END.
         IF NOT mBufferI BEGINS "bispc-" THEN
         DO:
            SET-SIZE(vBuffer) = 0.
            RETURN.
         END.
      END.
      IF SUBSTRING(mServVer, 1, 1) EQ "0" THEN 
      DO:
         vLen = SELF:GET-BYTES-AVAILABLE().
         IF vLen GT 0 THEN
         DO:
            mSocket:READ(vBuffer, 2, vLen, READ-EXACT-NUM).
            mBufferI = GET-STRING(vBuffer,1).
         END.
      END.
      vIntTMP = INTEGER(SUBSTRING(mServVer, 1, 1)) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND vIntTMP GT 0 THEN
      DO:
         DO WHILE (vIsRead AND vIntTMP NE 0):
            vItem = vItem + 1.
            mSocket:READ(vBuffer, vItem, 1, READ-EXACT-NUM).
            ASSIGN
               vIntTMP = GET-BYTE(vBuffer, vItem)
               vStrTMP = CHR(vIntTMP)
               mBufferI = mBufferI + vStrTMP.
         END.
      END.
      ASSIGN
         mFlag = 0
         SET-SIZE(vBuffer) = 0.
   END METHOD.
   {pfuncdef &defover="true" &reflection}
END CLASS.
/* $LINTFILE='netw.cls' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTUSER='mete' */
/* $LINTDATE='28/07/2016 13:15:48.477+03:00' */
/*prosignSncv3oaNu03f96n1noNMRg*/