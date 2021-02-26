/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: asyncrunhelper.cls
      Comment: Асинхронный запуск задачи
   Parameters: нет
         Uses:
      Used by:
      Created: 14.06.2013 10:22 Metlashevskiy E.

*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

/**
 * Для асинхронного запуска процедуры выполняем скрипт запуска BQ в асинхронном режиме 
 * средствами операционной системы однако, должны быть предусмотрены функции контроля
 * запущеной задачи, ее останова серией kill в ночном режиме, а также отложенный запуск.
 * В run-once.i добавляется специальная ветка для обработки асинхронного запуска процедур,
 * указанных с префиксом '&'
 * Для реализации асинхронного запуска реализуется статический класс asyncrunhelper.cls с методом AsyncRun. 
 * Метод должен
 *     Проверить, что предыдущий инстанс этой задачи уже завершился
 *     Если предыдущая задача еще активна - сдвигаем время ее запуска на не некоторый период, 
 *     который задается НП Scheduler/AsyncOffset ( 60 - 120 секунд по умолчанию), и прекращаем работу метода
 *     Если задача неактивна запускаем новую задачу, 
 *     запоминаем ее.
 *     Запускаемая процедура должна запускаться через специальную оболочку asyncrunner.p, 
 *     которая, во-первых, создает для нее корректный контекст запуска
 *
 *  Методы:
 *     AsyncRun  - асинхронный запуск задачи
 *     IsActive  - true, если задача активна  (проверка + останов задач)
 *     IsRuning  - true, если задача запущена (более легкая проверка, ничего не делает только возвращает статус)
 *     KillTask  - останов задачи
 *     Shutdown  - останов всех задач
 */
CLASS asyncrunhelper FINAL: 
 
   {intrface.obj xclass}                    /* подключаем интерфейс pp-xclas.p */
   {intrface.obj strng}                     /* подключаем интерфейс для работы со строками */
   {asyncrunhelper.def}                     /* определения асинхронного запуска */
   {processlist.def STATIC}                 /* список процессов */
   {async_bookmark.i &no-boomark-func=yes}  /* определения асинхронного запуска для bis-temp-table */

   &GLOBAL-DEFINE xSCHED-DEBUG Y  /* Дополнительная отладка для класса */

   {pfuncdef 
     &DefClass="asyncrunhelper" 
     &LibName="Упраление асинхронным запуском"
     &DESCRIPTION="Упраление асинхронным запуском задач"}

   /* Временная таблица задач */
   DEFINE STATIC TEMP-TABLE ttRun NO-UNDO
      FIELD run-id      AS INT64
      FIELD schedule-id AS CHARACTER
      FIELD filial      AS CHARACTER
      FIELD tproc       AS CHARACTER
      FIELD tparam      AS CHARACTER
      FIELD run-dt      AS DATETIME-TZ
      FIELD cmd         AS CHARACTER
      FIELD pid         AS INT64
      FIELD logfile     AS CHARACTER
      FIELD tmpdir      AS CHARACTER
      INDEX schedule-id schedule-id filial
      INDEX run-id IS PRIMARY UNIQUE run-id
      INDEX filial filial
      INDEX pid pid
   .
   DEFINE STATIC VARIABLE mRunIdSeq AS INT64 INIT 0 NO-UNDO.
   DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS asyncrunhelper NO-UNDO GET. PRIVATE SET.

   /* Список выполненых задач с ошибками */
   DEFINE PUBLIC STATIC PROPERTY WAIT-FOR-FAILURE AS CHARACTER INITIAL "" NO-UNDO GET. PRIVATE SET.

   /* Задачи завершены */
   DEFINE PUBLIC STATIC PROPERTY WAIT-FOR-COMPLETE AS LOGICAL INITIAL FALSE NO-UNDO GET. SET.

   /* По умолчанию всё складываем под ноги */
   DEFINE PUBLIC STATIC PROPERTY INIT-LOG-DIR AS CHARACTER INITIAL "~." NO-UNDO GET. SET.

   /* Задачи можно группировать в заданную группу чтоб потом модно было проверять завершила ли своё выполнение группа или нет */
   DEFINE PUBLIC STATIC PROPERTY TASK-GROUP AS CHARACTER INITIAL "~." NO-UNDO GET. SET.

   /* Имя файла выходного потока задачи */
   DEFINE PUBLIC STATIC PROPERTY TASK-STREAM AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Рабочая директория задачи */
   DEFINE PUBLIC STATIC PROPERTY TASK-WORKDIR AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Команда SHELL */
   DEFINE PUBLIC STATIC PROPERTY TASK-COMMAND AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   DEFINE STATIC BUFFER bbistt FOR bis-temp-table.

   DEFINE STATIC BUFFER setting FOR setting.

   /* Время выполнения задачи */
   DEFINE PUBLIC PROPERTY AsyncMaxTime AS CHARACTER INITIAL ? NO-UNDO
   GET:
      RETURN FGetSetting ({&SCHED-GROUP}, {&SCHED-ASYNCMAXTIME}, "7200").
   END GET.
   PRIVATE SET.

   /* Отложенный запуск */
   DEFINE PUBLIC PROPERTY IAsyncOffset AS CHARACTER INITIAL ? NO-UNDO
   GET:
      RETURN FGetSetting ({&SCHED-GROUP}, {&SCHED-ASYNCOFFSET}, "60").
   END GET.
   PRIVATE SET.

   /* Отложенный запуск */
   DEFINE PUBLIC STATIC PROPERTY AsyncOffset AS INTEGER INITIAL ? NO-UNDO
   GET:
      RETURN INTEGER(Instance:IAsyncOffset).
   END GET.
   PRIVATE SET. 

   /* Доп. параметры командной строки */
   DEFINE PUBLIC STATIC PROPERTY ScriptParam AS CHARACTER INITIAL "" NO-UNDO
   GET:
      RETURN FGetSettingLocal ("AsyncRun", "ScriptParam", "").
   END GET.
   PRIVATE SET.

   /* Возвращает настройку вне контекста, без филиала */
   METHOD PRIVATE STATIC CHAR FGetSettingLocal (INPUT iCode AS CHAR, 
                                                INPUT iSubCode AS CHAR,
                                                INPUT iDef AS CHAR):
      DEF BUFFER GB4PL_setting FOR setting.
      FIND FIRST setting WHERE
                 setting.CODE      EQ iCode
           AND   setting.sub-Code  EQ iSubCode
                 NO-LOCK NO-ERROR.
      RETURN IF AVAIL setting THEN setting.val ELSE iDef.
   END METHOD.

   /*--------------------------------------------------------------------------
     Purpose: Конструктор    
     Parameters:  
     Notes:       
   ---------------------------------------------------------------------------*/
   CONSTRUCTOR STATIC asyncrunhelper ():
      asyncrunhelper:Instance = NEW asyncrunhelper( ).
   END CONSTRUCTOR.

   CONSTRUCTOR PRIVATE asyncrunhelper( ):
      {&INIT_ALL_LIBS} /* Загружаем все интерфейсы */
   END CONSTRUCTOR.

   DESTRUCTOR asyncrunhelper():
      /* Останавливаем все задачи */
      asyncrunhelper:Shutdown().

      {&DEL_ALL_LIBS}  /* Выгружаем загруженные интерфейсы */
   END DESTRUCTOR.

   /*---------------------------------------------------------------------------
     Purpose: Ожидание выполнения запущенных задач    
     Parameters: ipSchedList - список запущенных задач через запятую
                 ipInterval  - интевал проверки задачи в секундах
                               через  заданный интервал приверяются задачи на предмет выполнения
                               чем больше интервал тем менее нагруженный будет WaitFor.
                               рекомендуется указывать значение не менее 15 секунд.
     Notes: Самый простой и стабильный WaitFor через список процессов ps.
            По логам задачи и по bis-temp-table смотреть нестабильно, т.к задача может запустится (процесс)
            но логин ещё не выполнятся, или задача может упасть с прогрессовой ошибкой, 
            тогда наоборот не завершится WaitFor.  
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="WaitFor"
      &Description="Ожидание выполнения задачи"
      &Parameters="INPUT <Список задач> AS CHAR, INPUT <Интервал> AS CHAR"
      &Result="Статус успеха операции (лог.)"
      &Sample="asyncrunhelper:WaitFor('task1,task2,task3', 1)."}

   METHOD PUBLIC STATIC LOGICAL WaitFor ( INPUT ipSchedList AS CHARACTER, INPUT ipInterval AS INT64):
       DEFINE VARIABLE vNowTask AS CHARACTER INIT TRUE NO-UNDO.
       vNowTask          = ipSchedList.
       WAIT-FOR-COMPLETE = FALSE.      /* на всякий случай */
       DO WHILE NOT WAIT-FOR-COMPLETE:
          IF NOT WaitFor (vNowTask, ipInterval, TRUE) THEN RETURN FALSE.
       END. 
       RETURN TRUE.
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL WaitFor ( INPUT-OUTPUT ipSchedList AS CHARACTER,  /* Список ожидаемых задач */
                                          INPUT ipInterval AS INT64,              /* Интервал проверки */
                                          INPUT ipKeep AS LOGICAL):               /* Выходить из цикла*/

      DEFINE VARIABLE vWaitStart   AS DATETIME-TZ    NO-UNDO. /* Время начала WaitFor */
      DEFINE VARIABLE vDoNow       AS CHARACTER   NO-UNDO. /* Список запущенных задач */
      DEFINE VARIABLE vMem         AS CHARACTER   NO-UNDO. /* Временная переменная */
      DEFINE VARIABLE vTask        AS CHARACTER   NO-UNDO. /* Временная переменная */
      DEFINE VARIABLE vProcessList AS processlist NO-UNDO. /* Список процессов */
      DEFINE VARIABLE vGrep        AS CHARACTER   NO-UNDO. /* Строка выборки grep */
      DEFINE VARIABLE vI           AS INTEGER     NO-UNDO. /* Локальная переменная */
      DEFINE VARIABLE vCount       AS INTEGER     NO-UNDO. /* Локальная переменная */
      DEFINE VARIABLE vExists      AS LOGICAL     NO-UNDO. /* true, если процесс выполняется */

      WAIT-FOR-FAILURE  = "".                                              /* Очистим список неуспешно выполненных задач */
      vWaitStart        = NOW.                                             /* Запомним время старта WaitFor, через AsyncMaxTime будем выходить по таймауту */
      vDoNow            = ipSchedList.                                     /* Заполним текущий список выполняемых процессов */
      vGrep             = "~'~\s*-b -param {&SCHED-PREFIX}*~'".            /* Будем фильтровать по параметрам скрипта, по самому скрипту не получится :( */
      WAIT-FOR-COMPLETE = FALSE.                                           /* Задачи не завершены */

      DO WHILE LENGTH(vDoNow) > 0:    /* Начинаем WaitFor пока все процессы по списку не завершатся */
         PAUSE ipInterval NO-MESSAGE. /* Выполняем паузу чтоб не нагружать процесс */

         /* Смотрим текущий список процессов, получим список только один раз для производительности, через IsActive будет медленнее */
         vProcessList = NEW processlist (vGrep).
         vProcessList:GetList (OUTPUT TABLE ttProcessList).
         vCount = NUM-ENTRIES(vDoNow). 
         vMem   = "".

         /* Проверим каждую задачу, выполняется ли она сейчас? */
         DO vI = 1 TO vCount:
            vTask = ENTRY(vI, vDoNow).
            FOR EACH ttRun WHERE ttRun.schedule-id EQ vTask,
               FIRST ttProcessList WHERE NOT ttProcessList.Cmd BEGINS "grep" 
                                         AND ttProcessList.Cmd MATCHES "*" + ttRun.cmd + "*":
               LEAVE.
            END.
            vExists = AVAIL(ttRun) AND AVAIL(ttProcessList).
            IF vExists THEN 
            DO:
               {additem.i vMem vTask} /* Задача ещё выполняется, добавим её в список следующей проверки */
            END.
            ELSE
            DO:
               /**
                * Задача завершилась, но она могла упасть, проверим что задача завершилась корректно!
                * Если задача упадет, то запись в bis-temp-table останется.
                */
               FIND FIRST ttRun WHERE ttRun.schedule-id EQ vTask NO-ERROR. /* найдем запись задачи */
               /* Задача завершилась, проверим что задача не упала с системной ошибкой, а завершилась корректно */
               FOR EACH bbistt WHERE bbistt.surrogate-id BEGINS {&ASYNC_BOOKMARK_CODE} AND
                                     bbistt.arbitrary-text1 EQ ttRun.tparam NO-LOCK:
                  /* Задача упала, сохраним в отчет */
                  {additem.i WAIT-FOR-FAILURE vTask}
                  LEAVE.
               END.
            END.
         END.
         vDoNow         = vMem.   /* актуализируем список выполняемых задач */
         vProcessList:Dispose().  /* очистим список процессов */

         /* На всякий случай будем выходить по таймауту, вдруг прцесс зависнет не будем же его ждать вечно. */
         IF ABS(INTERVAL(vWaitStart,NOW,"seconds")) > INTEGER(Instance:AsyncMaxTime) THEN RETURN FALSE.
         
         /* Если Keep то предоставим функционал обработки ожидания */
         IF ipKeep THEN 
         DO:
            WAIT-FOR-COMPLETE = FALSE.
            ipSchedList       = vDoNow.
            RETURN TRUE.
         END.
      END.
      WAIT-FOR-COMPLETE = TRUE.
      RETURN TRUE.
   END METHOD.


   /*---------------------------------------------------------------------------
     Purpose: Взвращает скрипт запуска    
     Parameters:  
     Notes:
   ---------------------------------------------------------------------------*/
   METHOD PRIVATE STATIC CHARACTER GetScript():
      DEFINE VARIABLE vScript AS CHARACTER NO-UNDO.
      /* Ищем скрипт запуска */
      vScript = OS-GETENV("BQCaller").
      IF NOT {assigned vScript} OR NOT filehelper:FileExists(vScript) THEN
         vScript = OS-GETENV("BISHOME") + filehelper:GetSlash() + OS-GETENV("BQNAME").
      IF NOT filehelper:FileExists(vScript) THEN
         vScript = OS-GETENV("BQ") + filehelper:GetSlash() + OS-GETENV("BQNAME").
      RETURN vScript.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose: Взвращает true если эта сессия запущена асинхронно    
     Parameters:  
     Notes:
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="AsyncSession"
      &Description="Идентификация текущей асинхронной сессии"
      &Parameters="нет"
      &Result="Возращает true если эта сессия запущена асинхронно (лог.)"
      &Sample="asyncrunhelper:AsyncSession()."}

   METHOD PUBLIC STATIC LOGICAL AsyncSession ():
      DEFINE VARIABLE vN AS INTEGER INIT 0 NO-UNDO.
      vN = 0.
      DO WHILE PROGRAM-NAME(vN + 1) <> ?:
         vN = vN + 1.
         IF PROGRAM-NAME(vN + 1) MATCHES "*asyncrunner*" THEN
            RETURN TRUE.
      END.
      RETURN FALSE.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose: Асинхронный запуск задачи    
     Parameters:  
     Notes:  0 - Задача успешно запущена
             2 - Задача не запущена т.к активен предыдущий экземпляр задачи
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="AsyncRun"
      &Description="Асинхронный запуск"
      &Parameters="INPUT <ID задачи> AS CHAR, ~
         INPUT <Филиал> AS CHAR, ~
         INPUT <Имя процедуры/Транзакции> AS CHAR, ~
         INPUT <Параметры процедуры/транзакции> AS CHAR"
      &Result="Взвращает статус успеха (целое.)"
      &Sample="asyncrunhelper:AsyncRun('task1', '002', 'test', '')."}

   METHOD PUBLIC STATIC INTEGER AsyncRun (
      iSchedId AS CHARACTER,
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iParam   AS CHARACTER
   ):
      DEFINE VARIABLE vRet      AS INTEGER INIT 0    NO-UNDO. /* Возвращаемое значение */
      DEFINE VARIABLE vLogFile  AS CHARACTER         NO-UNDO. /* Файл логирования */
      DEFINE VARIABLE vCmdParam AS CHARACTER         NO-UNDO. /* Параметры комманды запуска */
      DEFINE VARIABLE vCmdParS  AS CHARACTER         NO-UNDO. /* Параметры комманды запуска safe */
      DEFINE VARIABLE vTmpDir   AS CHARACTER         NO-UNDO. /* Директория выполнения */
      DEFINE VARIABLE vScript   AS CHARACTER         NO-UNDO. /* Скрипт запуска */
      DEFINE VARIABLE vGuid     AS CHARACTER         NO-UNDO. /* GUID задачи */
      DEFINE VARIABLE vBrPass   AS CHARACTER INIT "" NO-UNDO. /* хэш текущего пользователя */
      DEFINE VARIABLE hBrPass   AS HANDLE            NO-UNDO. /* хэндл для получения shared переменной */

      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Попытка асинхронного запуска задачи " + iProc + " с параметрами " + iParam + " для филиала " + iFilial + ".").
      &ENDIF

      /* Если предыдущий инстанс еще активен, перепланируем его через Scheduler/AsyncOffset секунд */
      IF isActive(iSchedId, iFilial) THEN
         RETURN {&ASYNC-RUN-OFFSET}.

      /* Предустановим директорию выполнения */
      vTmpDir  = INIT-LOG-DIR + filehelper:GetSlash() + iFilial + "_" + iSchedId.

      /* Предустановим файл логирования для асинхронной задачи */ 
      ASSIGN
         vGuid    = "{&SCHED-PREFIX}" + REPLACE(GUID, "-", "")
         TASK-STREAM = "sched_" + iSchedId + "_" + vGuid + ".log"
         vLogFile = ".~/" + TASK-STREAM
      .

      /* получим хэш текущего пользователя */
      RUN asyncrunner.p PERSISTENT SET hBrPass.
      IF VALID-HANDLE(hBrPass) THEN RUN GetBrPass IN hBrPass (OUTPUT vBrPass).

      /* Если vBrPass равен ? то вся строка сломается и в итоге asyncrunner.p не отработает, 
         получим превышение таймаута, и убивания самого планировщика!!! */
      IF NOT {assigned vBrPass}    THEN vBrPass    = "".
      IF NOT {assigned iFilial}    THEN iFilial    = "". /* на всякий случай */
      IF NOT {assigned TASK-GROUP} THEN TASK-GROUP = "". /* на всякий случай */
      IF NOT {assigned iParam}     THEN iParam     = "". /* на всякий случай */
      IF NOT {assigned iSchedId}   THEN iSchedId   = "". /* на всякий случай */

      /* Формируем параметры для старта асинхронной процедуры */
      vCmdParam = vGuid             + "~001" +   /* Идентификатор сессии */
                  iFilial           + "~001" +   /* Код филиала */
                  userid('bisquit') + "~001" +   /* Пользователь */
                  iProc             + "~001" +   /* Задача планировщика */
                  vBrPass           + "~001" +   /* Хэш */
                  iParam            + "~001" +   /* Параметры задачи */
                  TASK-GROUP        + "~001" +   /* Запускаемая задача относится к группе TASK-GROUP */
                  iSchedId          + "~001"     /* SchedId задачи */
      .
      vCmdParS   = vCmdParam.
      ENTRY(5, vCmdParS, "~001") = "". /* Сбросим хэш */
      TASK-GROUP = "".                 /* Сбросим группу на всякий случай */
      vCmdParam  = REPLACE(vCmdParam, " ", "~003").
      vCmdParam  = REPLACE(vCmdParam, "~/","~004").

      /* запишем в историю для отчетности */
      IF VALID-HANDLE(hBrPass) THEN RUN AsyncLogHistory IN hBrPass (INPUT iSchedId, INPUT iFilial, {&SCHED-STATE-START} + "~003" + vCmdParS).
      DELETE PROCEDURE hBrPass.

      /* Сохраняем информацию по задаче для контроля */
      CREATE ttRun.
      ASSIGN
         ttRun.schedule-id = iSchedId
         ttRun.filial      = iFilial
         ttRun.Cmd         = ENTRY(1, vCmdParam, "~001")
         ttRun.logfile     = vLogFile
         ttRun.tmpdir      = vTmpDir
         ttRun.pid         = ?
         ttRun.tproc       = iProc
         ttRun.tparam      = iParam
         ttRun.run-dt      = NOW
         ttRun.run-id      = mRunIdSeq
      .
      mRunIdSeq = mRunIdSeq + 1. /* Увеличим локальный сиквенс */

      /* Дополнительная отладка */
      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Асинхронный запуск задачи " + ttRun.tproc + " с параметрами " + ttRun.tparam + " для филиала " + iFilial + ".").
      &ENDIF

      vScript = GetScript().
      IF NOT filehelper:FileExists(vScript) THEN
         BatchLog("Скрипт запуска не найден, проверте корректность переменных окружения ~$BQ~/~$BQNAME, ~$BISHOME~/~$BQNAME").

      /* Запустим задачу в батч режиме */
      filehelper:CreateFolderIfNotExists (vTmpDir).
      ASSIGN
         TASK-WORKDIR = vTmpDir
         TASK-COMMAND = "cd " + vTmpDir + ";" + vScript + " " + ScriptParam +  
                        " -b -param ~"" + vCmdParam + "~" -p asyncrunner.p > " + "." + 
                        vLogFile + " &"
         TASK-STREAM = INIT-LOG-DIR + filehelper:GetSlash() + TASK-STREAM
         INIT-LOG-DIR = "~.".
      .
      OS-COMMAND SILENT VALUE(TASK-COMMAND).
      /* Дополнительная отладка */
      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Успешный асинхронный запуск задачи " + ttRun.tproc + " с параметрами " + ttRun.tparam + 
                   " для филиала " + iFilial + "").
      &ENDIF

      RETURN {&ASYNC-RUN-SUCESS}. /* Задача успешно запущена ассинхронно */
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Останавливает выполнение процесса
     Parameters: iPid - идентификатор процесса
     Notes:      Иногда мягкое снятие kill -18 не дает эффект, 
                 тем неменне команда выполняется успешно.
                 Поэтому предусмотрим кол-во попыток убивания процесса, 
                 на вторую попытку выполняем жесткое убивание.
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="KillPid"
      &Description="Останов задачи"
      &Parameters="INPUT <PID задачи> AS CHAR, INPUT <Кол-во попыток> AS CHAR"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:KillPid(12345, 3)."}

   METHOD PUBLIC STATIC LOGICAL KillPid ( INPUT iPid AS INT64, INPUT iTryNum AS INTEGER):
      DEFINE VARIABLE vOk AS INT64 NO-UNDO. 
      IF NOT syshelper:ProcessExists (iPid) THEN RETURN TRUE.
      vOk = processlist:KillPid (INPUT "15", INPUT iPid).
      PAUSE 2 NO-MESSAGE. /* Подождем, на всякий случай */
      IF vOk NE 0 OR iTryNum > 1 THEN
      DO:
         BatchLog ("** Попытка мягкой (kill -18) остановки задачи неуспешна для задачи PID " + STRING(iPid)).
         vOk = processlist:KillPid (INPUT "8", INPUT iPid). /* жесткое снятие задачи */
         PAUSE 2 NO-MESSAGE. /* Подождем, на всякий случай */
      END.
      IF vOk NE 0 THEN BatchLog ("** Попытка жесткой (kill -8) остановки задачи неуспешна для задачи PID " + STRING(iPid)).
      RETURN vOk EQ 0.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Останавливает выполнение задачи
     Parameters: iSchedId - задача
                 iFilial  - филиал
     Notes:
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="KillTask"
      &Description="Останов задачи"
      &Parameters="INPUT <ID задачи> AS CHAR, INPUT <Филиал> AS CHAR"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:KillTask('task1', '002')."}

   METHOD PUBLIC STATIC LOGICAL KillTask (INPUT iSchedId AS CHARACTER, INPUT iFilial AS CHARACTER):
      DEFINE VARIABLE vStoped AS LOG INIT FALSE NO-UNDO. /* Задача действительно остановлена */
      DEFINE VARIABLE vTryNum AS INTEGER        NO-UNDO. /* Попытки останова задачи */
      DEFINE VARIABLE vPID    AS CHARACTER      NO-UNDO.

      FOR FIRST ttRun WHERE ttRun.schedule-id EQ iSchedId AND ttRun.filial EQ iFilial:
         vPID = STRING(ttRun.PID).
         /* Здесь пытаемся остановить задачу */
         DO vTryNum = 1 TO 3:
            IF vTryNum >= 1 THEN BatchLog ("Попытка останова задачи " + ttRun.tproc + " PID = " + STRING(ttRun.PID)  +  " номер #" + STRING(vTryNum)).
            KillPid (ttRun.PID, vTryNum).

            /* Проверим действительно ли задача остановлена */
            vStoped = IsRuning(ttRun.run-id) EQ ?.
            IF vStoped OR NOT AVAIL(ttRun) THEN LEAVE.
         END.
      END.

      /* И так внимание! задачу возможно принудительно сняли, на всякий случай принудительно и разрегистрируем её */
      IF vStoped THEN
      DO:
         DEFINE VARIABLE mRgCode AS CHARACTER NO-UNDO.
         mRgCode = {&ASYNC_BOOKMARK_CODE} + "|" + vPID.
         /* удалим запись об открытой вкладки */
         DO TRANSACTION ON ERROR UNDO, LEAVE
                        ON STOP  UNDO, LEAVE:
            /* поиск уже зарегистрированной задачи */
            FIND FIRST bbistt WHERE bbistt.surrogate-id EQ mRgCode EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE(bbistt) THEN
            DO:
               DELETE bbistt.
            END.
         END.
      END.
      RETURN vStoped.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Останавливает все задачи
     Parameters: 
     Notes:
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="Shutdown"
      &Description="Останов всех задач запущенных в рамках текущей сессии"
      &Parameters="нет"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:Shutdown()."}

   METHOD PUBLIC STATIC LOGICAL Shutdown():
      DEFINE VARIABLE vRet AS LOGICAL INIT TRUE NO-UNDO. /* Результат */
      /* Останавливаем все задачи */
      FOR EACH ttRun NO-LOCK:
         IF IsRuning(ttRun.run-id) NE ? THEN
            vRet = vRet AND KillTask (INPUT ttRun.schedule-id, INPUT ttRun.filial).
      END.
      RETURN vRet.
   END METHOD.
   
   /*---------------------------------------------------------------------------
     Purpose:    Проверяет выполняется ли задача
     Parameters: iRunId - идентификатор задачи
     Notes:       
   ---------------------------------------------------------------------------*/
   METHOD PRIVATE STATIC INT64 IsRuning ( INPUT iRunId AS INT64):
      DEFINE VARIABLE vProcessList AS processlist NO-UNDO. /* Список процессов */

      DO ON ERROR UNDO, LEAVE
         ON STOP  UNDO, LEAVE:

         FOR FIRST ttRun WHERE ttRun.run-id EQ iRunId:
            /* Проверяем выполняется ли задача, если не найдена в списке процессов значит завершилась */
            vProcessList = NEW processlist (ttRun.cmd).
            vProcessList:GetList (OUTPUT TABLE ttProcessList).
            /* отсеим процессы grep выборки */
            FIND FIRST ttProcessList WHERE NOT ttProcessList.Cmd BEGINS "grep" NO-ERROR.
            IF AVAIL(ttProcessList) THEN 
            DO:  
               IF ttRun.PID EQ ? THEN ttRun.PID = ttProcessList.PID.
               RETURN ttProcessList.PID.
            END.
            ELSE
               RunCompleted (ttRun.logfile, ttRun.run-id).  /* Удалим задачу, зачистим лог */
            vProcessList:Dispose().
         END.

         CATCH e AS sysmessexception:
            e:Show().
         END CATCH.
      END.
      RETURN ?.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Возвращает true, в случае если задача зарегистрирована
                 в контексте bis-temp-table или группа задач.
                 Процедура сразу после запуска может сказать FALSE когда идет логин, но
                 скажет TRUE, если перезашли сессию, или задача упала или зависла.
     Parameters: iSchedId - идентификатор в планировщике  
                 iFilial  - филиал
     Notes:       
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="IsRegister"
      &Description="Проверяет выполняется ли задача в разрезе всех сессий"
      &Parameters="INPUT <ID задачи/ID группы задач> AS CHAR, ~
         INPUT <Филиал> AS CHAR, ~
         OUTPUT <Дата запуска задачи> AS DATETIME"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:IsRegister('task1', '002', OUTPUT vStartDt)."}

   METHOD PUBLIC STATIC LOGICAL IsRegister ( INPUT-OUTPUT iSchedIdOrGroup AS CHARACTER, INPUT iFilial AS CHARACTER, OUTPUT oStartDt AS DATETIME):
      DEFINE VARIABLE vParam       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vTaskOrGroup AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vFilial      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vPID         AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vEqual       AS CHARACTER NO-UNDO.
      
      vEqual = iSchedIdOrGroup.
      IF iSchedIdOrGroup BEGINS "&" THEN vEqual = LEFT-TRIM (vEqual,'&').

      /* проходим по всем ASX процессам */
      FOR EACH bbistt WHERE bbistt.surrogate-id BEGINS {&ASYNC_BOOKMARK_CODE} NO-LOCK:
         vParam         = bbistt.arbitrary-text1.
         vPID           = ENTRY (2, bbistt.surrogate-id, "|").
         vTaskOrGroup   = ?.
         IF NUM-ENTRIES(vParam, "|") >= 7 THEN DO:
            IF iSchedIdOrGroup BEGINS "&" THEN
               vTaskOrGroup   = ENTRY(6, vParam, "|"). /* Проверяем группу */
            ELSE
               vTaskOrGroup   = ENTRY(7, vParam, "|"). /* Проверяем задачу */
            vFilial        = ENTRY(2, vParam, "|").
         END.
         IF vTaskOrGroup MATCHES vEqual AND vFilial EQ iFilial THEN
         DO:
            iSchedIdOrGroup = ENTRY(7, vParam, "|"). /* возвращаем задачу которая ещё выполняется */
            oStartDt = DATETIME(bbistt.status-bis-templ) NO-ERROR.
            RETURN TRUE. 
         END.
      END.
      RETURN FALSE.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Возвращает true, в случае если задача активна, также
                 проверяет некоторые условия выполнения задачи    
     Parameters: iSchedId - идентификатор в планировщике  
                 iFilial  - филиал
     Notes:       
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="IsActive"
      &Description="Проверяет выполняется ли задача в разрезе текущей сессии"
      &Parameters="INPUT <ID задачи> AS CHAR, INPUT <Филиал> AS CHAR"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:IsActive('task1', '002')."}

   METHOD PUBLIC STATIC LOGICAL IsActive ( INPUT iSchedId AS CHARACTER, INPUT iFilial AS CHARACTER ):
      DEFINE VARIABLE vStoped     AS LOG INIT FALSE NO-UNDO. /* Задача действительно остановлена */
      DEFINE VARIABLE vProc       AS CHARACTER      NO-UNDO. /* Имя задачи для вывода в лог */
      DEFINE VARIABLE vParam      AS CHARACTER      NO-UNDO. /* Параметры задачи для вывода в лог */
      DEFINE VARIABLE vTryNum     AS INTEGER        NO-UNDO. /* Попытки останова задачи */

      FOR FIRST ttRun WHERE ttRun.schedule-id EQ iSchedId AND ttRun.filial EQ iFilial:
         vProc   = ttRun.tproc.
         vParam  = ttRun.tparam.

         /* Проверяем выполняется ли задача, если не найдена в списке процессов значит завершилась */
         IF IsRuning(ttRun.run-id) EQ ? THEN RETURN FALSE.

         /* Проверяем отведенное время для выполнения активной задачи */
         IF ABS(INTERVAL(ttRun.run-dt,NOW,"seconds")) > INTEGER(Instance:AsyncMaxTime) THEN
         DO:
            BatchLog ("** Время выполнение задачи планировщика " + vProc + " с параметрами " + vParam + 
                      " истекло, т.к задача выполняется больше " + STRING(ABS(INTERVAL(ttRun.run-dt,NOW,"seconds"))) + " секунд. Задача будет остановлена.").
               
            /* Здесь пытаемся остановить задачу */
            vStoped = KillTask (iSchedId, iFilial).

            IF vStoped THEN DO:
               BatchLog("** Задача " + vProc + " с параметрами " + vParam + " успешно остановлена").
               RETURN FALSE.
            END.
         END.
         RETURN TRUE.
      END.
      RETURN FALSE.
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:     
     Parameters:  
     Notes:       
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="RunCompleted"
      &Description="Метод обработки завершения задачи"
      &Parameters="INPUT <Лог> AS CHAR, INPUT <ID задачи> AS INT64"
      &Result="Взвращает статус успеха (лог.)"
      &Sample="asyncrunhelper:RunCompleted('task1', 12345) ~
         /* для будующих расширений */."}

   METHOD PUBLIC STATIC VOID RunCompleted ( INPUT iLog AS CHARACTER, INPUT iRunId AS INT64 ):
      /* Дополнительная отладка */
      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Завершение задачи " + iLog + ".").
      &ENDIF

      /* После завершения задачи, копируем лог задачи в текущий лог */
      FIND FIRST ttRun WHERE
         ttRun.run-id = iRunId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE ttRun THEN
      DO:
         /* Дополнительная отладка */
         &IF DEFINED(SCHED-DEBUG) &THEN
            BatchLog ("Задача " + ttRun.tproc + " с параметрами " + ttRun.tparam + " завершена.").
         &ENDIF      
         DELETE ttRun.

         /* Копируем содержимое лога задачи в текущий поток в батч режиме */
         IF filehelper:FileExists(iLog) AND SESSION:BATCH-MODE THEN 
         DO:
            /* Выведем содержимое файла логирования асинхронной задачи */
            INPUT FROM VALUE( iLog ).
            USE_STREAM_sInp:
            REPEAT:
               DEFINE VARIABLE vFileLine AS CHARACTER   NO-UNDO.
               IMPORT UNFORMATTED vFileLine.
               PUT UNFORMATTED vFileLine SKIP.
            END. /* USE_STREAM_sInp: */
            INPUT CLOSE. 
         END.

         /* Удалим лог задачи он больше не нужен */
         IF filehelper:FileExists(iLog) THEN OS-DELETE VALUE(iLog) NO-ERROR.
      END.
   END METHOD.

   {pfuncdef 
      &DefMeth="SaveResult"
      &Description="Метод для сохранения результата в задаче"
      &Parameters="INPUT <Имя> AS CHAR, INPUT <Значение> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:SaveResult('Res1', 'any-text')"."}
   &GLOBAL-DEFINE RESULT-BEG-TAG SUBSTITUTE("<asyncrunhelper:Result name='&1'>", iName)
   &GLOBAL-DEFINE RESULT-END-TAG SUBSTITUTE("</asyncrunhelper:Result name='&1'>", iName)

   METHOD PUBLIC STATIC VOID SaveResult ( INPUT iName AS CHAR, INPUT iValue AS CHAR ):
      PUT UNFORMATTED {&RESULT-BEG-TAG}.
      PUT UNFORMATTED iValue.
      PUT UNFORMATTED {&RESULT-END-TAG} SKIP.
   END METHOD.

   {pfuncdef 
      &DefMeth="GetResult"
      &Description="Метод для сохранения результата в задаче"
      &Parameters="INPUT <Имя> AS CHAR, INPUT <Значение> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:SaveResult('Res1', 'any-text')"."}
   METHOD PUBLIC STATIC CHAR GetResult ( INPUT iFile AS CHAR, INPUT iName AS CHAR ):
      DEF VAR vLob  AS LONGCHAR NO-UNDO.
      DEF VAR vFrom AS INT64    NO-UNDO.
      DEF VAR vTo   AS INT64    NO-UNDO.
      IF NOT filehelper:FileExists (iFile) THEN RETURN ?.
      COPY-LOB FILE iFile TO vLob NO-ERROR.
      vFrom = INDEX(vLob, {&RESULT-BEG-TAG}).
      IF vFrom <= 0 THEN RETURN ?.
      ASSIGN
         vTo = INDEX(vLob, {&RESULT-END-TAG})
         vFrom = vFrom + LENGTH({&RESULT-BEG-TAG}). 
      RETURN STRING(SUBSTRING(vLob, vFrom, vTo - vFrom)).
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:    Вывод информации в лог если работает в батч режиме из под планировщика
     Parameters:
     Notes:       
   ---------------------------------------------------------------------------*/
   METHOD PRIVATE STATIC VOID BatchLog ( INPUT iMessage AS CHARACTER ):
      IF SESSION:BATCH-MODE THEN PUT UNFORMATTED iMessage SKIP. 
      &IF DEFINED(SCHED-DEBUG) &THEN
      ELSE
         (NEW sysmessexception(iMessage)):Show().
      &ENDIF
   END METHOD.

   /*---------------------------------------------------------------------------
     Purpose:   Пауза
     Parameters:
     Notes:       
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="Sleep"
      &Description="Метод паузы в милисекундах"
      &Parameters="INPUT <Кол-во милисекунд> AS INT64, INPUT <События> AS LOG"
      &Result=" "
      &Sample="asyncrunhelper:Sleep(100)."}
   METHOD PUBLIC STATIC VOID Sleep ( INPUT iMillisec AS INT64 ):
      Sleep (iMillisec, YES).
   END METHOD.

      /* Метод с доп параметрами */
   METHOD PUBLIC STATIC VOID Sleep ( INPUT iMillisec AS INT64, INPUT iDoEvents AS LOG ):
      DEFINE VARIABLE vTimeStart AS DATETIME NO-UNDO.
      vTimeStart = NOW.
      DO WHILE ABS(INTERVAL(vTimeStart,NOW,"milliseconds")) < iMillisec:
         IF iDoEvents THEN
            PROCESS EVENTS. 
      END.
   END METHOD.

END CLASS. /* asyncrun_helper */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='30/10/2015 09:05:44.193+04:00' */
/* $LINTUSER='mete' */
/* $LINTMODE='1' */
/* $LINTFILE='asyncrunhelper.cls' */
/*prosignh7pm2pSXrg64o7zFpWuwbg*/