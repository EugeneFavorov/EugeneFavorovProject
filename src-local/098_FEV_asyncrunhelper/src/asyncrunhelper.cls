/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
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
 * Для реализации асинхронного запуска реализуется статический класс asyncrunhelper.cls с методом 
 * AsyncRun. 
 * Метод должен
 *     Проверить, что предыдущий инстанс этой задачи уже завершился
 *     Если предыдущая задача еще активна - сдвигаем время ее запуска на не некоторый период, 
 *     который задается НП Scheduler/AsyncOffset ( 60 - 120 секунд по умолчанию), и прекращаем 
 *     работу метода
 *     Если задача неактивна запускаем новую задачу, 
 *     запоминаем ее.
 *     Запускаемая процедура должна запускаться через специальную оболочку asyncrunner.p, 
 *     которая, во-первых, создает для нее корректный контекст запуска
 *
 *  Методы:
 *     AsyncRun  - асинхронный запуск задачи
 *     IsActive  - true, если задача активна  (проверка + останов задач)
 *     IsRuning  - true, если задача запущена (более легкая проверка, ничего не делает 
 *                                             только возвращает статус)
 *     KillTask  - останов задачи
 *     Shutdown  - останов всех задач
 */
CLASS asyncrunhelper FINAL: 

   &GLOBAL-DEFINE STATIC_MODIFIER STATIC

   {intrface.obj lic}
   {intrface.obj hist}
   {intrface.obj parsr}
   {intrface.obj xclass}                    /* подключаем интерфейс pp-xclas.p */
   {intrface.obj strng}                     /* подключаем интерфейс для работы со строками */
   {asyncrunhelper.def}                     /* определения асинхронного запуска */
   {processlist.def STATIC}                 /* список процессов */
   {async_bookmark.i &no-boomark-func=yes}  /* определения асинхронного запуска */

   &GLOBAL-DEFINE xSCHED-DEBUG Y  /* Дополнительная отладка для класса */

   {pfuncdef 
     &DefClass="asyncrunhelper" 
     &LibName="Управление асинхронными задачами"
     &DESCRIPTION="Управление асинхронными задачами"}

   /* Временная таблица задач */
   DEFINE STATIC TEMP-TABLE ttRun NO-UNDO
      FIELD run-id      AS INT64
      FIELD schedule-id AS CHARACTER
      FIELD filial      AS CHARACTER
      FIELD tproc       AS CHARACTER
      FIELD tparam      AS CHARACTER
      FIELD tstream     AS CHARACTER
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

   DEFINE STATIC VARIABLE mRunIdSeq AS INT64 INIT 0  NO-UNDO.
   DEFINE STATIC VARIABLE mMaxCores AS INT64 INIT -1 NO-UNDO.
   DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS asyncrunhelper NO-UNDO GET. PRIVATE SET.

   /* Список выполненых задач с ошибками */
   DEFINE PUBLIC STATIC PROPERTY WAIT-FOR-FAILURE AS CHARACTER INITIAL "" NO-UNDO GET. PRIVATE SET.

   /* Задачи завершены */
   DEFINE PUBLIC STATIC PROPERTY WAIT-FOR-COMPLETE AS LOGICAL INITIAL FALSE NO-UNDO GET. SET.

   /* По умолчанию всё складываем под ноги */
   DEFINE PUBLIC STATIC PROPERTY INIT-LOG-DIR AS CHARACTER INITIAL "~." NO-UNDO GET. SET.

   /* Каталог ноги */
   DEFINE PUBLIC STATIC PROPERTY TOCHKA-DIR AS CHARACTER INITIAL "" NO-UNDO GET. SET.

   /* Задачи можно группировать в заданную группу чтоб 
      потом можно было проверять завершила ли своё выполнение группа */
   DEFINE PUBLIC STATIC PROPERTY TASK-GROUP AS CHARACTER INITIAL "~." NO-UNDO GET. SET.

   /* Имя файла выходного потока задачи */
   DEFINE PUBLIC STATIC PROPERTY TASK-STREAM AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Имя файла выходного потока задачи */
   DEFINE PUBLIC STATIC PROPERTY REDIRECT-STREAM AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Рабочая директория задачи */
   DEFINE PUBLIC STATIC PROPERTY TASK-WORKDIR AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Команда SHELL */
   DEFINE PUBLIC STATIC PROPERTY TASK-COMMAND AS CHARACTER INITIAL ? NO-UNDO GET. SET.

   /* Код задачи */
   DEFINE PUBLIC STATIC PROPERTY LAST-TASK AS CHARACTER INITIAL "" NO-UNDO GET. SET.

   /* Список завершенных задач */
   DEFINE PUBLIC STATIC PROPERTY TASK-COMPLETE AS CHARACTER INITIAL "" NO-UNDO GET. SET.

   /* Интервал ожидания */
   DEFINE PUBLIC STATIC PROPERTY WAIT-FOR-INTERVAL AS INT64 INITIAL 1 NO-UNDO GET. SET.

   /* Список всех задач */
   DEFINE PUBLIC STATIC PROPERTY ALL-TASK AS CHARACTER INITIAL "" NO-UNDO 
   GET:
      DEF VAR vTasks AS CHAR NO-UNDO.
      FOR EACH ttRun:
         IF isActive(ttRun.schedule-id, ttRun.filial) THEN DO:
            {additem.i vTasks ttRun.schedule-id}
         END.
      END.
      RETURN vTasks.
   END GET. SET.

   /* Список завершенных задач */
   DEFINE PRIVATE STATIC PROPERTY TASK-ACCUM AS CHARACTER INITIAL "" NO-UNDO GET. SET.

   DEFINE STREAM sOut.

   /* Время выполнения задачи */
   DEFINE PUBLIC PROPERTY AsyncMaxTime AS CHARACTER INITIAL ? NO-UNDO
   GET:
      RETURN FGetSettingLocal ({&SCHED-GROUP}, {&SCHED-ASYNCMAXTIME}, "7200").
   END GET.
   PRIVATE SET.

   /* Отложенный запуск */
   DEFINE PUBLIC PROPERTY IAsyncOffset AS CHARACTER INITIAL ? NO-UNDO
   GET:
      RETURN FGetSettingLocal ({&SCHED-GROUP}, {&SCHED-ASYNCOFFSET}, "60").
   END GET.
   PRIVATE SET.

   /* Доп. настройки */
   DEFINE PUBLIC STATIC PROPERTY ScriptParam AS CHARACTER INITIAL "" NO-UNDO
   GET:
      RETURN FGetSettingLocal ("AsyncRun", "ScriptParam", "").
   END GET.
   PRIVATE SET.

   /* Идентификор следующей задачи */
   DEFINE STATIC PUBLIC PROPERTY RunIdSeq AS INT64 NO-UNDO
   GET:
      RETURN mRunIdSeq.
   END GET.
   PRIVATE SET.

   /* Отложенный запуск */
   DEFINE PUBLIC STATIC PROPERTY AsyncOffset AS INT64 INITIAL ? NO-UNDO
   GET:
      RETURN INT64 (Instance:IAsyncOffset).
   END GET.
   PRIVATE SET.

      /* Обработка событий завершения задачи */
   DEFINE PUBLIC STATIC EVENT OnTaskComplete SIGNATURE VOID ( INPUT iTask AS CHAR, 
                                                              OUTPUT oStop AS LOG ). 

      /* Обработка событий изменения % завершения */
   DEFINE PUBLIC STATIC EVENT OnPercentChange SIGNATURE VOID ( INPUT iPercent AS INT64 ). 

      /* Обработка событий изменения % завершения */
   DEFINE PUBLIC STATIC EVENT OnTimeOut SIGNATURE VOID ( OUTPUT oStop AS LOG, 
                                                         OUTPUT oNewTimeout AS INT64 ). 

   /* Признак асинхронной сессии */
   DEFINE PUBLIC STATIC PROPERTY AsyncSession AS LOGICAL NO-UNDO GET. PRIVATE SET.
   /* Количество потоков (для многопоточной задачи) */
   DEFINE PUBLIC STATIC PROPERTY ThreadsQuantity AS INT64 NO-UNDO GET. SET.
   /* Номер потока (для многопоточной задачи) */
   DEFINE PUBLIC STATIC PROPERTY ThreadNumber AS INT64 NO-UNDO GET. SET.
   /* Дополнительные параметры потока (для многопоточной задачи) */
   DEFINE PUBLIC STATIC PROPERTY ThreadParam AS CHARACTER NO-UNDO GET. SET.
   /* Уникальный номер группы потоколов в истории */
   DEFINE PUBLIC STATIC PROPERTY ThreadGuid AS CHARACTER NO-UNDO GET. SET.

   /* Возвращает настройку вне контекста, без филиала */
   METHOD PRIVATE STATIC CHAR FGetSettingLocal (INPUT iCode AS CHAR, 
                                                INPUT iSubCode AS CHAR,
                                                INPUT iDef AS CHAR):
      DEFINE VARIABLE vDynQuery  AS HANDLE NO-UNDO.
      DEFINE VARIABLE vDynBuffer AS HANDLE NO-UNDO.
      CREATE BUFFER vDynBuffer FOR TABLE "setting":U.
      CREATE QUERY vDynQuery.
      vDynQuery:SET-BUFFERS (vDynBuffer:HANDLE).
      vDynQuery:QUERY-PREPARE (SUBSTITUTE (
         "FOR EACH setting NO-LOCK WHERE setting.CODE = '&1' AND setting.sub-Code = '&2'":U,
         iCode,
         iSubCode)).
      vDynQuery:QUERY-OPEN().
      vDynQuery:GET-FIRST().
      RETURN IF vDynBuffer:AVAILABLE THEN vDynBuffer::val ELSE iDef.
      FINALLY:
         vDynQuery:QUERY-CLOSE() NO-ERROR.
         vDynBuffer:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT vDynQuery NO-ERROR.
         DELETE OBJECT vDynBuffer NO-ERROR. 
      END FINALLY.
   END METHOD.

   /*--------------------------------------------------------------------------
     Purpose: Конструктор    
     Parameters:  
     Notes:       
   ---------------------------------------------------------------------------*/
   CONSTRUCTOR STATIC asyncrunhelper ():
      DEFINE VARIABLE vN       AS INT64     NO-UNDO.
      asyncrunhelper:Instance = NEW asyncrunhelper( ).
      bldo:
      DO WHILE PROGRAM-NAME(vN + 1) <> ?:
         vN = vN + 1.
         IF PROGRAM-NAME(vN + 1) MATCHES "*asyncrunner*" THEN
         DO:
            AsyncSession = TRUE.
            LEAVE bldo.
         END.
      END.
      ASSIGN
         ThreadsQuantity = ?
         ThreadNumber    = ?
      .
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
     Purpose: Хэш функция для символьной строки    
     Parameters: iChar   - Символьная строка (Символьный суррогат записи)
                 iModulo - Граница диапазона / Число потоков
     Result: Число от 1 до верхней границы диапазона (включительно)                 
     Notes: При одининаковых параметрах всегда вернет то же самое число
            Для использования при параллельном запуске нескольких потоков
            Статистика:
            Для N потоков и N * 2000 записей перекос распределения менее 10%  
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="CharHash"
      &Description="Хэш функция для символьной строки"
      &Parameters="INPUT <Строка> AS CHAR, INPUT <Граница диапазона> AS INT64"
      &Result="Число от 1 до верхней границы диапазона (включительно)"
      &Sample="asyncrunhelper:CharHesh (loan.Cont-code, 10)."}
  
   METHOD PUBLIC STATIC INT64 CharHash (iChar AS CHARACTER, iModulo AS INT64):
       RETURN 1 + (GET-UNSIGNED-SHORT (MD5-DIGEST (iChar), 15) MODULO iModulo).
   END METHOD.

   /* Проверка хеша Char */
   METHOD PUBLIC STATIC LOGICAL CharHash (iId AS CHARACTER):
       RETURN 
         IF CharHash(iId, asyncrunhelper:ThreadsQuantity) 
            = asyncrunhelper:ThreadNumber THEN YES ELSE NO.
   END METHOD.
   
   /* Проверка хеша Int64 */
   METHOD PUBLIC STATIC LOGICAL CharHash (iId AS INT64):
       RETURN 
         IF CharHash(STRING(iId), asyncrunhelper:ThreadsQuantity) 
            = asyncrunhelper:ThreadNumber THEN YES ELSE NO.
   END METHOD.   
   
   /*---------------------------------------------------------------------------
     Purpose: Ожидание выполнения запущенных задач    
     Parameters: ipSchedList - список запущенных задач через запятую
                 ipInterval  - интевал проверки задачи в секундах
                               через  заданный интервал приверяются задачи на предмет выполнения
                               чем больше интервал тем менее нагруженный будет WaitFor.
                               рекомендуется указывать значение не менее 15 секунд.
     Notes: Самый простой и стабильный WaitFor через список процессов ps.
            По логам задачи и по bis-temp-table смотреть нестабильно, т.к задача может запустится 
            (процесс)
            но логин ещё не выполнятся, или задача может упасть с прогрессовой ошибкой, 
            тогда наоборот не завершится WaitFor.  
   ---------------------------------------------------------------------------*/
   {pfuncdef 
      &DefMeth="WaitFor"
      &Description="Ожидание выполнения задачи"
      &Parameters="INPUT <Список задач> AS CHAR, INPUT <Интервал> AS CHAR"
      &Result="Статус успеха операции (лог.)"
      &Sample="asyncrunhelper:WaitFor('task1,task2,task3', 1)."}


   METHOD PUBLIC STATIC LOGICAL WaitFor ():
      DEF VAR vTasks AS CHAR NO-UNDO.
      IF {assigned TASK-COMPLETE} THEN DO:
         ClearTasks (TASK-COMPLETE).
      END.
      FOR EACH ttRun:
         {additem.i vTasks ttRun.schedule-id}
      END. 
      RETURN WaitFor(?, vTasks).
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL WaitFor (INPUT iTimeOut AS INT64, INPUT iTasks AS CHAR ):
      DEF VAR vOldPercent AS INT64 INIT ?  NO-UNDO.
      DEF VAR vPercent    AS INT64 INIT 0  NO-UNDO.
      DEF VAR vStop       AS LOG   INIT NO NO-UNDO.
      DEF VAR vStart      AS DATETIME-TZ   NO-UNDO.
      DEF VAR vPause      AS INT64         NO-UNDO.

      WAIT-FOR-COMPLETE = FALSE.      
      vStart = NOW.

      DO WHILE Wait(iTasks):
         IF {assigned TASK-COMPLETE} THEN DO: 
            asyncrunhelper:OnTaskComplete:Publish (TASK-COMPLETE, OUTPUT vStop) NO-ERROR.
            IF vStop THEN DO: 
               KillTask (iTasks).
               RETURN FALSE.
            END.
         END.

         IF {assigned "TRIM(iTasks)"} THEN DO:
            vPercent = GetTaskPercent (iTasks).
            IF vPercent <> vOldPercent THEN 
               asyncrunhelper:OnPercentChange:Publish (vPercent) NO-ERROR.
         END.

         IF iTimeOut <> ? THEN DO:
            IF ABS(INTERVAL(vStart,NOW,"seconds")) > iTimeOut THEN DO:
               asyncrunhelper:OnTimeout:Publish (OUTPUT vStop, OUTPUT iTimeOut) NO-ERROR.
               IF vStop THEN DO: 
                  KillTask (iTasks).
                  RETURN FALSE.
               END.
            END.
         END.
         vPause = WAIT-FOR-INTERVAL - 1.
         IF vPause > 1 THEN
            PAUSE vPause NO-MESSAGE.
         vOldPercent = vPercent.
      END. 
      RETURN YES.
   END METHOD.

   {pfuncdef 
      &DefMeth="Wait"
      &Description="Ожидание выполнения задачи в блоке DO WHILE"
      &Parameters="INPUT <Список задач> AS CHAR, INPUT <Интервал> AS CHAR"
      &Result="Статус успеха операции (лог.)"
      &Sample="DO WHILE asyncrunhelper:Wait('task1,task2,task3')."}
   METHOD PUBLIC STATIC LOGICAL Wait ():
      DEF VAR vTasks AS CHAR NO-UNDO.
      IF {assigned TASK-COMPLETE} THEN DO:
         ClearTasks (TASK-COMPLETE).
      END.
      FOR EACH ttRun:
         {additem.i vTasks ttRun.schedule-id}
      END.
      RETURN Wait (INPUT-OUTPUT vTasks).
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL Wait ( INPUT-OUTPUT pTasks AS CHARACTER ):
      RETURN Wait (pTasks, NO).
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL Wait ( INPUT-OUTPUT pTasks AS CHARACTER, 
                                       INPUT iWaitForComplete AS LOG  ):
       IF {assigned TASK-ACCUM} THEN RETURN YES.
       IF WAIT-FOR-COMPLETE     THEN RETURN FALSE.
       IF NOT {assigned "TRIM(pTasks)"} THEN RETURN FALSE.

       TASK-COMPLETE = "".  /* Очистим список завершенных */

       IF iWaitForComplete THEN DO:
          DO WHILE NOT {assigned TASK-COMPLETE}: 
             IF NOT WaitFor (pTasks, 1, TRUE) THEN
                RETURN ERROR SUBSTITUTE("Ошибка ожидания задачи &1", pTasks).
          END. 
       END.
       ELSE DO:
          IF NOT WaitFor (pTasks, 1, TRUE) THEN
             RETURN ERROR SUBSTITUTE("Ошибка ожидания задачи &1", pTasks).
       END.

       {additem.i TASK-ACCUM TASK-COMPLETE}
       RETURN TRUE.
       FINALLY:
          DEF VAR vTask AS CHAR NO-UNDO.
          vTask = ENTRY(1, TASK-ACCUM).
          ENTRY(1, TASK-ACCUM) = "".
          TASK-ACCUM = LEFT-TRIM(TRIM(TASK-ACCUM), ",").
          TASK-COMPLETE = vTask.
          LAST-TASK = TASK-COMPLETE.
       END FINALLY.
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL WaitFor ( INPUT iSchedList AS CHARACTER, 
                                          INPUT iInterval AS INT64):
       DEFINE VARIABLE vNowTask AS CHARACTER INIT TRUE NO-UNDO.
       vNowTask          = iSchedList.
       WAIT-FOR-COMPLETE = FALSE.      /* на всякий случай */
       DO WHILE NOT WAIT-FOR-COMPLETE:
          IF NOT WaitFor (vNowTask, iInterval, TRUE) THEN RETURN FALSE.
       END. 
       RETURN TRUE.
   END METHOD.
         
   METHOD PRIVATE STATIC VOID ClearTasks (INPUT iTasks AS CHAR):
      DEF VAR vTask AS CHAR  NO-UNDO.
      DEF VAR vI    AS INT64 NO-UNDO.
      IF {assigned iTasks} THEN DO:
         DO vI = 1 TO NUM-ENTRIES (iTasks):
            vTask = ENTRY(vI, iTasks).
            FIND FIRST ttRun WHERE ttRun.schedule-id = vTask NO-ERROR. 
            IF AVAIL ttRun THEN DELETE ttRun.
         END.
      END.
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL WaitFor ( INPUT-OUTPUT ipSchedList AS CHARACTER,  /* Список ожидаемых задач */
                                          INPUT ipInterval AS INT64,              /* Интервал проверки */
                                          INPUT ipKeep AS LOGICAL):               /* Выходить из цикла*/

      DEFINE VARIABLE vWaitStart   AS DATETIME    NO-UNDO. /* Время начала WaitFor */
      DEFINE VARIABLE vDoNow       AS CHARACTER   NO-UNDO. /* Список запущенных задач */
      DEFINE VARIABLE vMem         AS CHARACTER   NO-UNDO. /* Временная переменная */
      DEFINE VARIABLE vTask        AS CHARACTER   NO-UNDO. /* Временная переменная */
      DEFINE VARIABLE vProcessList AS processlist NO-UNDO. /* Список процессов */
      DEFINE VARIABLE vGrep        AS CHARACTER   NO-UNDO. /* Строка выборки grep */
      DEFINE VARIABLE vI           AS INT64       NO-UNDO. /* Локальная переменная */
      DEFINE VARIABLE vCount       AS INT64       NO-UNDO. /* Локальная переменная */
      DEFINE VARIABLE vExists      AS LOGICAL     NO-UNDO. /* true, если процесс выполняется */

      IF {assigned TASK-COMPLETE} THEN DO:
         ClearTasks (TASK-COMPLETE).
      END.

      WAIT-FOR-COMPLETE = FALSE.
      TASK-COMPLETE     = "".                                              /* Очистим список завершенных */
      WAIT-FOR-FAILURE  = "".                                              /* Очистим список неуспешно выполненных задач */
      vWaitStart        = NOW.                                             /* Запомним время старта WaitFor, через AsyncMaxTime будем выходить по таймауту */
      vDoNow            = ipSchedList.                                     /* Заполним текущий список выполняемых процессов */
      vGrep             = "~'~\s*-b -param {&SCHED-PREFIX}*~'".            /* Будем фильтровать по параметрам скрипта, по самому скрипту не получится :( */
      WAIT-FOR-COMPLETE = FALSE.                                           /* Задачи не завершены */

      DO WHILE LENGTH(vDoNow) > 0:    /* Начинаем WaitFor пока все процессы по списку не завершатся */
         /* Смотрим текущий список процессов, получим список только один раз для производительности, через IsActive будет медленнее */
         vProcessList = NEW processlist (vGrep).
         vProcessList:GetList (OUTPUT TABLE ttProcessList).
         vCount = NUM-ENTRIES(vDoNow). 
         vMem   = "".

         /* Проверим каждую задачу, выполняется ли она сейчас? */
         DO vI = 1 TO vCount:
            vTask = ENTRY(vI, vDoNow).
            FOR EACH ttRun WHERE ttRun.schedule-id =  vTask,
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
               {additem.i TASK-COMPLETE vTask}
               /**
                * Задача завершилась, но она могла упасть, проверим что задача завершилась корректно!
                * Если задача упадет, то запись в bis-temp-table останется.
                */
               DO ON ERROR UNDO, LEAVE
                  ON STOP  UNDO, LEAVE:
                  DEFINE VARIABLE vDynQuery  AS HANDLE NO-UNDO.
                  DEFINE VARIABLE vDynBuffer AS HANDLE NO-UNDO.
                  CREATE BUFFER vDynBuffer FOR TABLE "bis-temp-table":U.
                  CREATE QUERY vDynQuery.
                  vDynQuery:SET-BUFFERS (vDynBuffer:HANDLE).
                  vDynQuery:QUERY-PREPARE (SUBSTITUTE (
                     "FOR EACH bis-temp-table USE-INDEX surrogate NO-LOCK WHERE " + 
                        "bis-temp-table.surrogate-id BEGINS '&1' AND " + 
                        "bis-temp-table.arbitrary-text1 MATCHES '&2'":U,
                     {&ASYNC_BOOKMARK_CODE},
                     "*" + vTask)).
                  vDynQuery:QUERY-OPEN().
                  vDynQuery:GET-FIRST().
                  IF vDynBuffer:AVAILABLE THEN DO:
                     /* Задача упала, сохраним в отчет */
                     {additem.i WAIT-FOR-FAILURE vTask}
                  END.
   
                  FINALLY:
                     vDynQuery:QUERY-CLOSE() NO-ERROR.
                     vDynBuffer:BUFFER-RELEASE() NO-ERROR.
                     DELETE OBJECT vDynQuery NO-ERROR.
                     DELETE OBJECT vDynBuffer NO-ERROR. 
                  END FINALLY.
               END.
            END.
         END.
         vDoNow         = vMem.   /* актуализируем список выполняемых задач */
         vProcessList   = ?.      /* очистим список процессов */

         /* На всякий случай будем выходить по таймауту, вдруг прцесс зависнет не будем же его ждать вечно. */
         IF ABS(INTERVAL(vWaitStart,NOW,"seconds")) > 
            INT64(Instance:AsyncMaxTime) THEN RETURN FALSE.
         
         IF LENGTH(vDoNow) > 0 THEN
            PAUSE ipInterval NO-MESSAGE. /* Выполняем паузу чтоб не нагружать процесс */

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
   {pfuncdef 
      &DefMeth="GetScript"
      &Description="Скрипт запуска"
      &Parameters="нет"
      &Result=" "
      &Sample="asyncrunhelper:GetScript()."}
   METHOD PUBLIC STATIC CHARACTER GetScript():
      DEFINE VARIABLE vScript AS CHARACTER NO-UNDO.
      vScript = FGetSettingLocal ("AsyncRun", "ScriptFile", "").

      IF NOT {assigned vScript} THEN
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
      RETURN AsyncSession.
   END METHOD.

   DEFINE STATIC VARIABLE mLoadAvergaesOSCmd AS CHARACTER   NO-UNDO.

   {pfuncdef 
      &DefMeth       = "GetLoadAverage"
      &Description   = "Получает среднюю загрузку системы"
      &Parameters    = "нет"
      &Result        = "нагрузка на систему (DECIMAL)"
      &Sample        = "asyncrunhelper:GetLoadAverage()."
   }
   METHOD PUBLIC STATIC DECIMAL GetLoadAverage ( ):
      DEFINE VARIABLE vOSCommand AS CHARACTER   NO-UNDO.
      IF NOT {assigned mLoadAvergaesOSCmd} THEN
      DO:
         vOSCommand = FGetSettingLocal (
            "AsyncRun", 
            "LoadAveragesCmd",
            "uptime | $DLC/perl/bin/perl -ne " + 
            "'if(m/load average[s]*~\: ([~\d~\.]+)~\,/i)~{ print $1 . ~"~\n~"; ~}'").
         mLoadAvergaesOSCmd = vOSCommand.
      END.
      ELSE
         vOSCommand = mLoadAvergaesOSCmd.
      INPUT THROUGH VALUE(vOSCommand).
      DEFINE VARIABLE vLoadAverages AS DECIMAL NO-UNDO.
      IMPORT UNFORMATTED vLoadAverages NO-ERROR.
      INPUT CLOSE.
      RETURN vLoadAverages.
   END METHOD.

   {pfuncdef 
      &DefMeth       = "GetNumCores"
      &Description   = "Возвращает количество логических процессоров на сервере ~
для задачи рапараллеливания. Для настройки команды, возвращающей число процессоров ~
используйте НП AsyncRun/NumCoresCmd. Для HP-UX и AIX настройка не требуется."
      &Parameters    = "нет"
      &Result        = "Количество логических процессоров"
      &Sample        = "asyncrunhelper:GetNumCores()."
   }

   METHOD PUBLIC STATIC INT64 GetNumCores
                            ( INPUT iSchedId AS CHAR,   
                              INPUT iFilial AS CHAR, 
                              INPUT iDepthCounter AS INT64 ):
      DEF VAR vRet AS INT64 NO-UNDO.
      DEF VAR vAnaliz AS asyncanalize NO-UNDO.
      vAnaliz = NEW asyncanalize().
      vRet = vAnaliz:GetNumCores (iSchedId, iFilial, iDepthCounter) NO-ERROR.
         /* Подстрахуемся */
      IF ERROR-STATUS:ERROR THEN vRet = asyncrunhelper:GetNumCores().
      RETURN vRet.
      CATCH vEx AS Progress.Lang.Error:
         RETURN asyncrunhelper:GetNumCores().
      END CATCH.
      FINALLY:
         IF VALID-OBJECT(vAnaliz) THEN DELETE OBJECT vAnaliz NO-ERROR.
      END FINALLY.
   END METHOD.

   METHOD PUBLIC STATIC INT64 GetNumCores ( ):
      DEFINE VARIABLE vOSCommand AS CHARACTER   NO-UNDO.

      IF mMaxCores > 0 THEN RETURN mMaxCores.
      vOSCommand = FGetSettingLocal ("AsyncRun", "NumCoresCmd", ?).

      IF NOT {assigned vOSCommand} THEN
      DO:
         INPUT THROUGH VALUE("uname").
         DEFINE VARIABLE vOSName AS CHARACTER   NO-UNDO.
         IMPORT UNFORMATTED vOSName.
         INPUT CLOSE.
         CASE vOSName:
            WHEN "HP-UX" THEN
               vOSCommand = "ioscan -k -C processor | grep processor | wc -l".
            WHEN "AIX" THEN
               vOSCommand = "pmcycles -m | wc -l".
            WHEN "Linux" THEN
               vOSCommand = "/usr/bin/getconf _NPROCESSORS_ONLN".
            OTHERWISE 
               RETURN 1.
         END CASE.
      END.
      
      INPUT THROUGH VALUE(vOSCommand).
      DEFINE VARIABLE vSNumCores AS CHARACTER   NO-UNDO.
      IMPORT UNFORMATTED vSNumCores.
      INPUT CLOSE.

      DEFINE VARIABLE vNumCores AS INT64 NO-UNDO  INIT 1.
      vNumCores = INT64(vSNumCores) NO-ERROR.
      mMaxCores = vNumCores.
      RETURN vNumCores.

      FINALLY:
         INPUT CLOSE.
      END FINALLY.
      
   END METHOD.

   {pfuncdef 
      &DefMeth       = "MergeAsyncFiles"
      &Description   = "Объединяет файлы из локальных каталогов асинхронных процессов ~
в один файл. Должен вызываться сразу после метода WaitFor."
      &Parameters    = "<СПИСОК АСИНХРОННЫХ ЗАДАЧ>,<ЛОКАЛЬНЫЙ ФАЙЛ>,<ОБЪЕДИНЕННЫЙ ФАЙЛ>"
      &Result        = "НЕТ"
      &Sample        = "asyncrunhelper:MergeAsyncFiles(vAsyncTaskList,'_spool.tmp','_megaspool.tmp')."
   }
   METHOD PUBLIC STATIC VOID MergeAsyncFiles(ipSchedList AS CHARACTER,
                                             iFileForMerge AS CHARACTER,
                                             iMergedFile AS CHARACTER):
      DEFINE VARIABLE vNumTasks AS INT64  NO-UNDO.
      vNumTasks = NUM-ENTRIES(ipSchedList).

      DEFINE VARIABLE vTaskCnt AS INT64 NO-UNDO.
      DO vTaskCnt = 1 TO vNumTasks:
         FIND FIRST ttRun WHERE
            ttRun.schedule-id = ENTRY(vTaskCnt,ipSchedList)
            NO-LOCK NO-ERROR.
         DEFINE VARIABLE vOsCommand AS CHARACTER   NO-UNDO INIT "/usr/bin/cat".
         vOsCommand = vOsCommand + " " + ttRun.tmpdir + "~/" + iFileForMerge. 
      END.
      vOsCommand = vOsCommand + "> " + iMergedFile + " 2> /dev/null".
      OS-COMMAND SILENT VALUE(vOsCommand).

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
      &Parameters="INPUT <ID задачи> AS CHAR, INPUT <Филиал> AS CHAR, ~
INPUT <Имя процедуры/Транзакции> AS CHAR, INPUT <Параметры процедуры/транзакции> AS CHAR ~
[, INPUT <Параметры многопоточности> AS CHAR, INPUT <Доп. параметры потока> AS CHAR]"
      &Result="Возвращает статус успеха (целое.)"
      &Sample="asyncrunhelper:AsyncRun('task1', '002', 'test', '')."}

   METHOD PUBLIC STATIC INT64 AsyncRun (
      iSchedId AS CHARACTER,
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iParam   AS CHARACTER,
      iThreads AS INT64,
      iThrNum  AS INT64,
      iThrEPar AS CHARACTER
   ):
      DEFINE VARIABLE vLogFile  AS CHARACTER         NO-UNDO. /* Файл логирования */
      DEFINE VARIABLE vCmdParam AS CHARACTER         NO-UNDO. /* Параметры комманды запуска */
      DEFINE VARIABLE vCmdParS  AS CHARACTER         NO-UNDO. /* Параметры комманды запуска safe */
      DEFINE VARIABLE vTmpDir   AS CHARACTER         NO-UNDO. /* Директория выполнения */
      DEFINE VARIABLE vScript   AS CHARACTER         NO-UNDO. /* Скрипт запуска */
      DEFINE VARIABLE vGuid     AS CHARACTER         NO-UNDO. /* GUID задачи */
      DEFINE VARIABLE vBrPass   AS CHARACTER INIT "" NO-UNDO. /* хэш текущего пользователя */
      DEFINE VARIABLE hBrPass   AS HANDLE            NO-UNDO. /* хэндл для получения shared переменной */
      DEFINE VARIABLE vOutLog   AS CHARACTER         NO-UNDO. /* выходной поток задачи */
      DEFINE VARIABLE vOk       AS LOG               NO-UNDO. /* признак успешности запуска */

      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Попытка асинхронного запуска задачи " + iProc + " с параметрами " + iParam + " для филиала " + iFilial + ".").
      &ENDIF

      IF iThreads > 1 THEN ASSIGN
         iSchedId   = SUBSTITUTE("&1#&2", iSchedId, iThrNum)
         ThreadGuid = GUID  WHEN iThrNum =  1.
      ELSE ASSIGN
         ThreadGuid = "".

      /* хелпер получения шаренных переменных */
      RUN asyncrunner.p PERSISTENT SET hBrPass.
      IF iFilial = ? AND VALID-HANDLE(hBrPass) THEN RUN GetFilial IN hBrPass (OUTPUT iFilial).
      IF NOT {assigned iFilial} THEN iFilial = "". /* на всякий случай */

      /* Если предыдущий инстанс еще активен, перепланируем его через Scheduler/AsyncOffset секунд */
      IF isActive(iSchedId, iFilial) THEN
         RETURN {&ASYNC-RUN-OFFSET}.

      vTmpDir  = INIT-LOG-DIR + filehelper:GetSlash() + iFilial + "_" + iSchedId.

      IF TOCHKA-DIR <> "" 
      THEN DO:
         vTmpDir = REPLACE(vTmpDir,".",TOCHKA-DIR).
         vTmpDir = REPLACE(vTmpDir,"~/~/","~/").
         INIT-LOG-DIR = REPLACE(INIT-LOG-DIR,".",TOCHKA-DIR).
         INIT-LOG-DIR = REPLACE(INIT-LOG-DIR,"~/~/","~/").
      END.
      /* Предустановим файл логирования для асинхронной задачи */ 
      ASSIGN
         vGuid       = "{&SCHED-PREFIX}" + REPLACE(GUID, "-", "")
         TASK-STREAM = "sched_" + iSchedId + "_" + vGuid + ".log"
         TASK-STREAM = REDIRECT-STREAM WHEN {assigned REDIRECT-STREAM}
         vLogFile    = ".~/" + iFilial + "_" + iSchedId + "~/" + TASK-STREAM
         REDIRECT-STREAM = ?
         LAST-TASK = ""
      .
 
      IF VALID-HANDLE(hBrPass) THEN RUN GetBrPass IN hBrPass (OUTPUT vBrPass).

      /* Если vBrPass равен ? то вся строка сломается и в итоге asyncrunner.p не отработает, 
         получим превышение таймаута, и убивания самого планировщика!!! */
      IF NOT {assigned vBrPass}    THEN vBrPass    = "".
      IF NOT {assigned TASK-GROUP} THEN TASK-GROUP = "". /* на всякий случай */
      IF NOT {assigned iParam}     THEN iParam     = "". /* на всякий случай */
      IF NOT {assigned iSchedId}   THEN iSchedId   = "". /* на всякий случай */

      /* Формируем параметры для старта асинхронной процедуры */
      vCmdParam = vGuid             + "~001" +   /* Идентификатор сессии */
                  iFilial           + "~001" +   /* Код филиала */
                  userid('bisquit') + "~001" +   /* Пользователь */
                  iProc             + "~001" +   /* Задача планировщика */
                  vBrPass           + "~001" +   /* Хэш */
                  ""                + "~001" +   /* Параметры задачи, вынесены в поток */
                  TASK-GROUP        + "~001" +   /* Запускаемая задача относится к группе TASK-GROUP */
                  iSchedId          + "~001"     /* SchedId задачи */
      .
      vCmdParS   = vCmdParam.
      ENTRY(5, vCmdParS, "~001") = "". /* Сбросим хэш */
      TASK-GROUP = "".                 /* Сбросим группу на всякий случай */
      vCmdParam  = REPLACE(vCmdParam, " ", "~003").
      vCmdParam  = REPLACE(vCmdParam, "~/","~004").

      /* запишем в историю для отчетности */
      IF VALID-HANDLE(hBrPass) THEN LogHistory  
        ( INPUT iSchedId, 
          INPUT iFilial, 
          {&SCHED-STATE-START} + "~003" + vCmdParS ).

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
         BatchLog ("Асинхронный запуск задачи " + ttRun.tproc + " с параметрами " + 
                   ttRun.tparam + " для филиала " + iFilial + ".").
      &ENDIF

      vScript = GetScript().
      IF NOT filehelper:FileExists(vScript) THEN
         BatchLog("Скрипт запуска не найден, проверте корректность " + 
                  "переменных окружения ~$BQ~/~$BQNAME, ~$BISHOME~/~$BQNAME").

      /* Запустим задачу в батч режиме */
      filehelper:CreateFolderIfNotExists (INIT-LOG-DIR).
      filehelper:CreateFolderIfNotExists (vTmpDir).
      vOutLog = SUBSTITUTE("&1&2&3",vTmpDir,filehelper:GetSlash(),TASK-STREAM).

      /* Передадим параметры через файл во избежания обрезания командной строки операционкой */
      OUTPUT STREAM sOut TO VALUE (vOutLog).
      PUT STREAM sOut UNFORMATTED "=== ПОДГОТОВКА К ЗАПУСКУ ===" SKIP.
      SaveResultB ({&SCHED-PARAM-TAG}, iParam).
      SaveResultB ({&SCHED-PARAM-STREAM}, SUBSTITUTE ("&1,&2":U, iThreads, iThrNum)).
      SaveResultB ({&SCHED-PARAM-STREAM-OTHER}, iThrEPar).
      SaveResultB ({&SCHED-PARAM-STREAM-ID}, ThreadGuid).
      OUTPUT STREAM sOut CLOSE.

      /* Если включено профилирование, то добавить профилирование в параметры асинх. сессии. */
      /* Параметры профилирования наследуются.  Результаты будут в директории асинх. сессии. */
      /* Имена файлов и директории листингов для асинх. сессии: prof.cfg, prof.out, listings */
      IF PROFILER:ENABLED AND PROFILER:PROFILING AND INDEX (ScriptParam, " -profile ":U) = 0 THEN
      DO:
         DEFINE VARIABLE vProfCfg  AS LONGCHAR          NO-UNDO. /* Параметры профилирования */
         ASSIGN
            ScriptParam = IF LENGTH (ScriptParam) > 0 THEN
               ScriptParam + " -profile prof.cfg":U ELSE "":U
            .
         IF PROFILER:COVERAGE THEN vProfCfg =
            SUBSTITUTE ("&1-COVERAGE~n":U, vProfCfg).
         IF LENGTH (PROFILER:DESCRIPTION) > 0 THEN vProfCfg =
            SUBSTITUTE ("&1-DESCRIPTION ~"&2~"~n":U, vProfCfg, PROFILER:DESCRIPTION).
         vProfCfg =
            SUBSTITUTE ("&1-FILENAME prof.out~n":U, vProfCfg).
         IF PROFILER:LISTINGS THEN vProfCfg =
            SUBSTITUTE ("&1-LISTINGS listings~n":U, vProfCfg).
         vProfCfg =
            SUBSTITUTE ("&1-PROFILING yes~n":U, vProfCfg).
         IF LENGTH (PROFILER:TRACE-FILTER) > 0 THEN vProfCfg =
            SUBSTITUTE ("&1-TRACE-FILTER ~"&2~"~n":U, vProfCfg, PROFILER:TRACE-FILTER).
         IF LENGTH (PROFILER:TRACING) > 0 THEN vProfCfg =
            SUBSTITUTE ("&1-TRACING ~"&2~"~n":U, vProfCfg, PROFILER:TRACING).
         IF PROFILER:LISTINGS THEN filehelper:CreateFolderIfNotExists (
            vTmpDir + filehelper:GetSlash() + "listings":U).
         COPY-LOB vProfCfg TO FILE vTmpDir + filehelper:GetSlash() + "prof.cfg":U.
      END.

      ASSIGN
         TASK-WORKDIR = vTmpDir
         TASK-COMMAND = "cd " + vTmpDir + ";" + vScript + " " + ScriptParam +  
                        " -b -param ~"" + vCmdParam + "~" -p asyncrunner.p >> " + "." + 
                        /* Выходной поток положим внутрь папки с задачей */
                        vLogFile + " &"

         TASK-STREAM = INIT-LOG-DIR + filehelper:GetSlash() + 
                       iFilial + "_" + iSchedId + filehelper:GetSlash() + 
                       TASK-STREAM
         INIT-LOG-DIR = "~."
         LAST-TASK    = iSchedid
         ttRun.tstream = TASK-STREAM
      .

      /* Проверим директорию выполнения */
      IF {assigned INIT-LOG-DIR} AND INIT-LOG-DIR <> "." THEN
         filehelper:CreateFolderIfNotExists (INIT-LOG-DIR).

      OS-COMMAND SILENT VALUE(TASK-COMMAND).
      vOk = YES.

      /* Дополнительная отладка */
      &IF DEFINED(SCHED-DEBUG) &THEN
         BatchLog ("Успешный асинхронный запуск задачи " + 
                   ttRun.tproc + " с параметрами " + ttRun.tparam + 
                   " для филиала " + iFilial + "").
      &ENDIF

      RETURN {&ASYNC-RUN-SUCESS}. /* Задача успешно запущена ассинхронно */

      CATCH vErr AS Progress.Lang.Error:
         RETURN ERROR SUBSTITUTE("Ошибка запуска задачи &1 в филиале &2. &3", 
                                  iSchedId, iFilial, vErr:GetMessage(1)).
      END CATCH.

      FINALLY:
         IF NOT vOk THEN RETURN ERROR 
            SUBSTITUTE("Ошибка запуска задачи &1 в филиале &2. &3", 
                       iSchedId, iFilial, ERROR-STATUS:GET-MESSAGE(1)).
      END FINALLY.
   END METHOD.
   

   /* Укороченный вызов */
   {pfuncdef 
      &DefMeth="Parallel"
      &Description="Укороченный вызов AsyncRun"
      &Parameters="INPUT <Филиал> AS CHAR, ~
                   INPUT <Имя процедуры/Транзакции/Парсер> AS CHAR, ~
                   INPUT <Число ядер> AS INT64"
      &Result="Взвращает список задач (строка.)"
      &Sample="asyncrunhelper:Parallel('002', 'pars: message(1,1);', 4)."}

   METHOD PUBLIC STATIC CHAR Parallel (
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iCores   AS INT64
   ):
      RETURN Parallel (?, iFilial, iProc, iCores).
   END METHOD.

   METHOD PUBLIC STATIC CHAR Parallel (
      iSched   AS CHARACTER,
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iCores   AS INT64
   ):
      DEF VAR vTasks AS CHAR INIT "" NO-UNDO.
      DEF VAR vTask  AS CHAR INIT "" NO-UNDO.
      DEF VAR vI     AS INT64 NO-UNDO.
      WAIT-FOR-COMPLETE = FALSE.
      DO vI = 1 TO iCores:
         vTask = IF {assigned iSched} THEN SUBSTITUTE("&1#&2", iSched, vI) ELSE GUID.
         IF AsyncRun (vTask, iFilial, iProc, "":U, iCores, vI, "":U) = {&ASYNC-RUN-SUCESS} THEN
         DO:
            {additem.i vTasks LAST-TASK}
         END.
         ELSE 
            RETURN ERROR SUBSTITUTE("Ошибка запуска потока №&1", vI).
      END.
      LAST-TASK = vTasks.
      RETURN vTasks. 
   END METHOD.

   METHOD PUBLIC STATIC CHAR Parallel (
      iProc    AS CHARACTER,
      iCores   AS INT64
   ):
      RETURN Parallel (?, iProc, iCores).
   END METHOD.

   METHOD PUBLIC STATIC CHAR Parallel (
      iProc    AS CHARACTER
   ):
      WAIT-FOR-COMPLETE = FALSE.
      IF AsyncRun (GUID, ?, iProc, "":U) = {&ASYNC-RUN-SUCESS} THEN
      DO:
         RETURN LAST-TASK.
      END.
      ELSE
         RETURN ERROR SUBSTITUTE("Ошибка запуска потока").
   END METHOD.

   /* Укороченный вызов + обратная совместимость */
   {pfuncdef 
      &DefOver="AsyncRun"}
   METHOD PUBLIC STATIC INT64 AsyncRun (
      iSchedId AS CHARACTER,
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iParam   AS CHARACTER
   ):
      RETURN AsyncRun (iSchedId, iFilial, iProc, iParam, 1, 1, "":U).
   END METHOD.
   
   /* Укороченный вызов */
   {pfuncdef 
      &DefOver="AsyncRun"}
   METHOD PUBLIC STATIC INT64 AsyncRun (
      iSchedId AS CHARACTER,
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iParam   AS CHARACTER,
      iThreads AS INT64,
      iThrNum  AS INT64
   ):
      RETURN AsyncRun (iSchedId, iFilial, iProc, iParam, iThreads, iThrNum, "":U).
   END METHOD.

   {pfuncdef 
      &DefMeth="NewSession"
      &Description="Синхронный запуск в новой сесиии"
      &Parameters="INPUT <ID задачи> AS CHAR, INPUT <Филиал> AS CHAR, ~
                   INPUT <Имя процедуры/Транзакции> AS CHAR, ~
                   INPUT <Параметры процедуры/транзакции> AS CHAR"
      &Result="Взвращает статус успеха (целое.)"
      &Sample="asyncrunhelper:NewSession('task1', '002', 'test', '')."}

   METHOD PUBLIC STATIC INT64 NewSession (
      iFilial  AS CHARACTER,
      iProc    AS CHARACTER,
      iParam   AS CHARACTER,
      iScriptParam AS CHARACTER 
   ):
      RETURN ?. /* Не используется */
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

   METHOD PUBLIC STATIC LOGICAL KillPid ( INPUT iPid AS INT64, INPUT iTryNum AS INT64):
      DEFINE VARIABLE vOk AS INT64 NO-UNDO. 
      IF NOT syshelper:ProcessExists (iPid) THEN RETURN TRUE.
      vOk = processlist:KillPid (INPUT "TERM", INPUT iPid). /* kill -15 */
      PAUSE 2 NO-MESSAGE. /* Подождем, на всякий случай */
      IF vOk <> 0 OR iTryNum > 1 THEN
      DO:
         BatchLog ("** Попытка мягкой (kill -TERM) остановки " + 
                   "задачи неуспешна для задачи PID " + STRING(iPid)).
         vOk = processlist:KillPid (INPUT "FPE", INPUT iPid). /* жесткое снятие задачи kill -8 */
         PAUSE 2 NO-MESSAGE. /* Подождем, на всякий случай */
      END.
      IF vOk <> 0 THEN BatchLog ("** Попытка жесткой (kill -FPE) остановки " +
                                 "задачи неуспешна для задачи PID " + STRING(iPid)).
      RETURN vOk =  0.
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

        /* Укороченный выаоз по списку задач */
   METHOD PUBLIC STATIC LOGICAL KillTask (INPUT iTasks AS CHARACTER):
      DEF VAR vI    AS INT64 NO-UNDO.
      DEF VAR vTask AS CHAR  NO-UNDO.
      DEF VAR vRet  AS LOG   NO-UNDO.
      vRet = YES.
      DO vI = 1 TO NUM-ENTRIES (iTasks):
         vTask = ENTRY(vI, iTasks).
         FIND FIRST ttRun WHERE ttRun.schedule-id = vTask NO-ERROR.
         IF AVAIL ttRun THEN
            vRet = vRet AND KillTask (vTask, ttRun.filial).
      END.
      RETURN vRet.
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL KillTask (INPUT iSchedId AS CHARACTER, INPUT iFilial AS CHARACTER):
      DEFINE VARIABLE vStoped AS LOG INIT FALSE NO-UNDO. /* Задача действительно остановлена */
      DEFINE VARIABLE vTryNum AS INT64        NO-UNDO. /* Попытки останова задачи */
      DEFINE VARIABLE vPID    AS CHARACTER      NO-UNDO.

      FOR FIRST ttRun WHERE ttRun.schedule-id = iSchedId AND ttRun.filial = iFilial:
         vPID = STRING(ttRun.PID).
         IF vPID =  ? THEN vPID = STRING(IsRuning(ttRun.run-id)).
         IF vPID =  ? THEN LEAVE.
         /* Здесь пытаемся остановить задачу */
         DO vTryNum = 1 TO 3:
            IF vTryNum >= 1 THEN BatchLog ("Попытка останова задачи " + ttRun.tproc + 
                                           " PID = " + STRING(ttRun.PID)  +  
                                           " номер #" + STRING(vTryNum)).
            KillPid (ttRun.PID, vTryNum).
            /* Проверим действительно ли задача остановлена */
            vStoped = IsRuning(ttRun.run-id) =  ?.
            IF vStoped OR NOT AVAIL(ttRun) THEN LEAVE.
         END.
      END.

      /* И так внимание! задачу возможно принудительно сняли, 
         на всякий случай принудительно и разрегистрируем её */
      IF vStoped THEN
      DO:
         DEFINE VARIABLE mRgCode AS CHARACTER NO-UNDO.
         mRgCode = {&ASYNC_BOOKMARK_CODE} + "|" + vPID.
         /* удалим запись об открытой вкладки */
         DO TRANSACTION ON ERROR UNDO, LEAVE
                        ON STOP  UNDO, LEAVE:
            DEFINE VARIABLE vDynQuery  AS HANDLE NO-UNDO.
            DEFINE VARIABLE vDynBuffer AS HANDLE NO-UNDO.
            CREATE BUFFER vDynBuffer FOR TABLE "bis-temp-table":U.
            CREATE QUERY vDynQuery.
            vDynQuery:SET-BUFFERS (vDynBuffer:HANDLE).
            vDynQuery:QUERY-PREPARE (SUBSTITUTE (
               "FOR EACH bis-temp-table EXCLUSIVE-LOCK WHERE " + 
                  "bis-temp-table.surrogate-id ='&1'":U,
               mRgCode)).
            vDynQuery:QUERY-OPEN().
            vDynQuery:GET-FIRST().
            IF vDynBuffer:AVAILABLE THEN DO:
               vDynBuffer:BUFFER-DELETE() NO-ERROR.
            END.
            FINALLY:
               vDynQuery:QUERY-CLOSE() NO-ERROR.
               vDynBuffer:BUFFER-RELEASE() NO-ERROR.
               DELETE OBJECT vDynQuery NO-ERROR.
               DELETE OBJECT vDynBuffer NO-ERROR. 
            END FINALLY.
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
         IF IsRuning(ttRun.run-id) <> ? THEN
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

         FOR FIRST ttRun WHERE ttRun.run-id =  iRunId:
            /* Проверяем выполняется ли задача, 
               если не найдена в списке процессов значит завершилась */
            vProcessList = NEW processlist (ttRun.cmd).
            vProcessList:GetList (OUTPUT TABLE ttProcessList).
            /* отсеим процессы grep выборки */
            FIND FIRST ttProcessList WHERE NOT ttProcessList.Cmd BEGINS "grep" NO-ERROR.
            IF AVAIL(ttProcessList) THEN 
            DO:  
               IF ttRun.PID =  ? THEN ttRun.PID = ttProcessList.PID.
               RETURN ttProcessList.PID.
            END.
            ELSE
               RunCompleted (ttRun.logfile, ttRun.run-id). /* Удалим задачу, зачистим лог */
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

   METHOD PUBLIC STATIC LOGICAL IsRegister ( INPUT-OUTPUT iSchedIdOrGroup AS CHARACTER, 
                                             INPUT iFilial AS CHARACTER, 
                                             OUTPUT oStartDt AS DATETIME ):

      DEFINE VARIABLE vParam       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vTaskOrGroup AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vFilial      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vPID         AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vPIDn        AS INT64     NO-UNDO.
      DEFINE VARIABLE vEqual       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vCommonNum   AS INT64     NO-UNDO.
      
      vEqual = iSchedIdOrGroup.
      IF iSchedIdOrGroup BEGINS "&" THEN vEqual = LEFT-TRIM (vEqual,'&').

      DEFINE VARIABLE vDynQuery  AS HANDLE NO-UNDO.
      DEFINE VARIABLE vDynBuffer AS HANDLE NO-UNDO.
      DEFINE VARIABLE vConQuery  AS HANDLE NO-UNDO.
      DEFINE VARIABLE vConBuffer AS HANDLE NO-UNDO.

      CREATE BUFFER vDynBuffer FOR TABLE "bis-temp-table":U.
      CREATE QUERY vDynQuery.
      vDynQuery:SET-BUFFERS (vDynBuffer:HANDLE).
      vDynQuery:QUERY-PREPARE (SUBSTITUTE (
         "FOR EACH bis-temp-table NO-LOCK WHERE " + 
            "bis-temp-table.surrogate-id BEGINS '&1'",
         {&ASYNC_BOOKMARK_CODE})).

      vDynQuery:QUERY-OPEN().
      vDynQuery:GET-FIRST().

      REPEAT WHILE NOT vDynQuery:QUERY-OFF-END:
         vParam         = vDynBuffer::arbitrary-text1.
         vPID           = ENTRY (2, vDynBuffer::surrogate-id, "|").
         vTaskOrGroup   = ?.
         IF NUM-ENTRIES(vParam, "|") >= 7 THEN DO:
            IF iSchedIdOrGroup BEGINS "&" THEN
               vTaskOrGroup   = ENTRY(6, vParam, "|"). /* Проверяем группу */
            ELSE
               vTaskOrGroup   = ENTRY(7, vParam, "|"). /* Проверяем задачу */
            vFilial        = ENTRY(2, vParam, "|").
         END.
         IF vTaskOrGroup MATCHES vEqual AND vFilial =  iFilial THEN
         DO:
            iSchedIdOrGroup = ENTRY(7, vParam, "|"). /* возвращаем задачу которая ещё выполняется */
            oStartDt = DATETIME(vDynBuffer::status-bis-templ) NO-ERROR.
            vCommonNum  = vDynBuffer::bis-templ-id.

            /* задача зарегестрирована, проверим дополнительно жива ли сессия */
            vPIDn = INT64(vPID).
            CREATE BUFFER vConBuffer FOR TABLE "_Connect":U.
            CREATE QUERY vConQuery.
            vConQuery:SET-BUFFERS (vConBuffer:HANDLE).
            vConQuery:QUERY-PREPARE (SUBSTITUTE (
               "FOR EACH _Connect NO-LOCK WHERE " + 
                  "_Connect._Connect-Id = '&1' AND _Connect._Connect-Pid = '&2'":U,
               vCommonNum + 1, vPIDn)).
      
            vConQuery:QUERY-OPEN().
            vConQuery:GET-FIRST().

            RETURN vConBuffer:AVAILABLE. 
         END.
         vDynQuery:GET-NEXT().
      END.


      RETURN FALSE.

      FINALLY:
         vDynQuery:QUERY-CLOSE()     NO-ERROR.
         vDynBuffer:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT vDynQuery     NO-ERROR.
         DELETE OBJECT vDynBuffer    NO-ERROR. 
         vConQuery:QUERY-CLOSE()     NO-ERROR.
         vConBuffer:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT vConQuery     NO-ERROR.
         DELETE OBJECT vConBuffer    NO-ERROR. 
      END FINALLY.
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
   METHOD PUBLIC STATIC LOGICAL IsActive ( INPUT iSchedId AS CHARACTER, 
                                           INPUT iFilial AS CHARACTER ):
      RETURN IsActive (iSchedId, iFilial, FALSE).
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL IsActive ( INPUT iSchedId AS CHARACTER, 
                                           INPUT iFilial  AS CHARACTER,
                                           INPUT iExistOnlyTT AS LOG  ):
      DEFINE VARIABLE vStoped     AS LOG INIT FALSE NO-UNDO. /* Задача действительно остановлена */
      DEFINE VARIABLE vProc       AS CHARACTER      NO-UNDO. /* Имя задачи для вывода в лог */
      DEFINE VARIABLE vParam      AS CHARACTER      NO-UNDO. /* Параметры задачи для вывода в лог */

      FOR FIRST ttRun WHERE ttRun.schedule-id =  iSchedId AND ttRun.filial =  iFilial:
         vProc   = ttRun.tproc.
         vParam  = ttRun.tparam.

         /* Проверяем выполняется ли задача, 
            если не найдена в списке процессов значит завершилась */
         IF IsRuning(ttRun.run-id) =  ? THEN RETURN FALSE.
         IF iExistOnlyTT THEN RETURN TRUE. /* проверять только временную таблицу */

         /* Проверяем отведенное время для выполнения активной задачи */
         IF ABS(INTERVAL(ttRun.run-dt,NOW,"seconds")) > INT64(Instance:AsyncMaxTime) THEN
         DO:
            BatchLog ("** Время выполнение задачи планировщика " + vProc + 
                      " с параметрами " + vParam + 
                      " истекло, т.к задача выполняется больше " + 
                      STRING(ABS(INTERVAL(ttRun.run-dt,NOW,"seconds"))) + 
                      " секунд. Задача будет остановлена.").
               
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
      &Sample="asyncrunhelper:RunCompleted('task1', 12345) /* для будующих расширений */."}

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
      END.
   END METHOD.

   {pfuncdef 
      &DefMeth="SavePercent"
      &Description="Метод для сохранения % выполнения задачи"
      &Parameters="INPUT <Процент> AS INT64"
      &Result=" "
      &Sample="asyncrunhelper:SavePercent(100)."}
   METHOD PUBLIC STATIC VOID SavePercent ( INPUT iPercent AS INT64 ):
      asyncrunhelper:SaveResult ("Percent", STRING(iPercent)).
   END METHOD.

   {pfuncdef 
      &DefMeth="SaveResult"
      &Description="Метод для сохранения результата в задаче"
      &Parameters="INPUT <Имя> AS CHAR, INPUT <Значение> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:SaveResult('Res1', 'any-text')."}
   &GLOBAL-DEFINE RESULT-BEG-TAG SUBSTITUTE("<asyncrunhelper:Result name='&1'>", iName)
   &GLOBAL-DEFINE RESULT-END-TAG SUBSTITUTE("</asyncrunhelper:Result name='&1'>", iName)

   METHOD PUBLIC STATIC VOID SaveResult ( INPUT iName AS CHAR, INPUT iValue AS CHAR ):
      PUT UNFORMATTED {&RESULT-BEG-TAG}.
      PUT UNFORMATTED iValue.
      PUT UNFORMATTED {&RESULT-END-TAG} SKIP.
   END METHOD.

   METHOD PRIVATE STATIC VOID SaveResultB ( INPUT iName AS CHAR, INPUT iValue AS CHAR ):
      PUT STREAM sOut UNFORMATTED {&RESULT-BEG-TAG}.
      PUT STREAM sOut UNFORMATTED iValue.
      PUT STREAM sOut UNFORMATTED {&RESULT-END-TAG} SKIP.
   END METHOD.

   {pfuncdef 
      &DefMeth="GetResult"
      &Description="Метод для сохранения результата в задаче"
      &Parameters="INPUT <Имя> AS CHAR, INPUT <Значение> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:SaveResult('Res1', 'any-text')."}
   METHOD PUBLIC STATIC CHAR GetResult ( INPUT iFile AS CHAR, INPUT iName AS CHAR ):
      DEF VAR vLob  AS LONGCHAR NO-UNDO.
      DEF VAR vFrom AS INT64    NO-UNDO.
      DEF VAR vTo   AS INT64    NO-UNDO.
      IF NOT filehelper:FileExists (iFile) THEN RETURN ?.
      COPY-LOB FILE iFile TO vLob NO-ERROR.
      vFrom = R-INDEX(vLob, {&RESULT-BEG-TAG}).
      IF vFrom <= 0 THEN RETURN ?.
      ASSIGN
         vTo = R-INDEX(vLob, {&RESULT-END-TAG})
         vFrom = vFrom + LENGTH({&RESULT-BEG-TAG}). 
      IF /* этот IF нужно захватить по 0281128 */
         vTo < vFrom 
      THEN 
         RETURN ?.
      RETURN STRING(SUBSTRING(vLob, vFrom, vTo - vFrom)).
   END METHOD.

   {pfuncdef 
      &DefMeth="GetTaskResult"
      &Description="Метод получения результата из задачи или списка задач"
      &Parameters="INPUT <Задача/Список задач> AS CHAR, INPUT <Имя параметра> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:SaveResult('Res1', 'any-text')."}
   METHOD PUBLIC STATIC CHAR GetTaskResult ( INPUT iTask AS CHAR, INPUT iName AS CHAR ):
      IF NUM-ENTRIES(iTask) > 1 THEN DO:
         DEF VAR vI    AS INT64 NO-UNDO.
         DEF VAR vTask AS CHAR  NO-UNDO.
         DEF VAR vRet  AS CHAR  NO-UNDO.
         DEF VAR vVal  AS CHAR  NO-UNDO.
         DO vI = 1 TO NUM-ENTRIES(iTask):
            vTask = ENTRY(vI, iTask).
            FIND FIRST ttRun WHERE ttRun.schedule-id = vTask NO-ERROR.
            IF AVAIL ttRun THEN DO:
               vVal = GetResult(ttRun.tstream, iName).
               {additem.i vRet "SUBSTITUTE('&1', vVal)"}
            END.
            ELSE DO:
               {additem.i vRet "SUBSTITUTE(' ')"}
            END.
         END.
         RETURN vRet.
      END.
      ELSE DO:
         FIND FIRST ttRun WHERE ttRun.schedule-id = iTask NO-ERROR.
         IF NOT AVAIL ttRun THEN RETURN ?.
         RETURN GetResult(ttRun.tstream, iName).
      END.
   END METHOD.

   {pfuncdef 
      &DefMeth="GetTaskPercent"
      &Description="Возвращает процент выполненных задач"
      &Parameters="INPUT <Задача/Список задач> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:GetTaskPercent(asyncrunhelper:LAST-TASK)."}
   METHOD PUBLIC STATIC INT64 GetTaskPercent (INPUT iTasks AS CHAR):
      DEF VAR vRet AS CHAR  NO-UNDO.
      DEF VAR vPer AS INT64 NO-UNDO.
      DEF VAR vVal AS INT64 NO-UNDO.
      DEF VAR vCal AS CHAR  NO-UNDO.
      DEF VAR vI   AS INT64 NO-UNDO.
      vRet = GetTaskResult (iTasks, "Percent").
      IF NOT {assigned vRet} THEN RETURN 0.
      DO vI = 1 TO NUM-ENTRIES(vRet):
         vVal = 100.
         vCal = TRIM(ENTRY(vI, vRet)).
         vVal = INT64(ENTRY(vI, vRet)) NO-ERROR.
         IF vCal = "?" THEN vVal = 0.
         IF vCal = ""  THEN vVal = 100.
         vPer = vPer + vVal.
      END.
      RETURN INT64(vPer / NUM-ENTRIES(vRet)).
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
      DEFINE VARIABLE vTimeStart AS DATETIME-TZ NO-UNDO.
      vTimeStart = NOW.
      DO WHILE ABS(INTERVAL(vTimeStart,NOW,"milliseconds")) < iMillisec:
         IF iDoEvents THEN
            PROCESS EVENTS. 
      END.
   END METHOD.

      /* Запись истории */
   {pfuncdef 
      &DefMeth="LogHistory"
      &Description="Запись истории"
      &Parameters="INPUT <Код задачи> AS INT64, INPUT <Филиал> AS LOG, INPUT <Описание> AS CHAR"
      &Result=" "
      &Sample="asyncrunhelper:LogHistory('task1','002','')."}
   METHOD PUBLIC STATIC VOID LogHistory ( INPUT iSchedId AS CHAR, 
                                          INPUT iFilial AS CHAR, 
                                          INPUT iDescription AS CHAR ):

      DEF VAR vThreads   AS INT64 NO-UNDO.
      DEF VAR vThreadNum AS INT64 NO-UNDO.
      ASSIGN vThreads = asyncrunhelper:ThreadsQuantity
             vThreadNum = asyncrunhelper:ThreadNumber
      .
         /* записиваем в историб с общим кодом задачи */
      IF vThreads > 1 AND INDEX(iSchedId, "#") > 0 THEN DO:
         ENTRY(NUM-ENTRIES(iSchedId, "#"), iSchedId, "#") = "".
         iSchedId = RIGHT-TRIM(iSchedId, "#").
      END.
   
      DEFINE VARIABLE vHistory AS HANDLE NO-UNDO.

      DO TRANSACTION ON ERROR UNDO, LEAVE:

         CREATE BUFFER vHistory FOR TABLE "history":U.
         vHistory:BUFFER-CREATE().
         ASSIGN
         vHistory::file-name     = {&ASYNC-JOURNAL-FILENAME}
         vHistory::field-name    = {&ASYNC-JOURNAL-FIELDNAME}
         vHistory::field-value   = 
            iSchedId + "~003" + iFilial + "~003" + iDescription
         vHistory::modif-date    = TODAY
         vHistory::user-id       = USERID("bisquit")
         vHistory::field-ref     = 
            STRING(vThreads) + "~003" + PDBNAME("dictdb")
         vHistory::modif-time    = TIME
         vHistory::modify        = asyncrunhelper:ThreadGuid
         vHistory::history-id    = 
            INTERVAL(NOW, DATETIME("01/01/1900"), "milliseconds":U)
         vHistory::db-trans-id   = TODAY
         .
      END.

      FINALLY:
         vHistory:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT vHistory NO-ERROR.
      END FINALLY.
   END METHOD.


      /* Запускатель сессии */
   {pfuncdef 
      &DefMeth="SetContext"
      &Description="Установка контекста"
      &Parameters=" "
      &Result=" "
      &Sample="asyncrunhelper:SetContext()"}
   METHOD PRIVATE STATIC VOID Title (INPUT iBrPas  AS HANDLE, 
                                     INPUT iProc   AS CHAR,
                                     INPUT iParam  AS CHAR,
                                     INPUT iFilial AS CHAR, 
                                     INPUT iLog    AS CHAR,
                                     INPUT iUserId AS CHAR,
                                     INPUT iLength AS INT64):

         /* Пишем в лог заголовок запуска */
         DEF VAR vDateTz   AS DATE  NO-UNDO.
         DEF VAR vTimeTz   AS INT64 NO-UNDO.
         DEF VAR vTimeZone AS CHAR  NO-UNDO.

         IF VALID-HANDLE(iBrPas) THEN RUN GetDate     IN iBrPas (OUTPUT vDateTz).
         IF VALID-HANDLE(iBrPas) THEN RUN GetTime     IN iBrPas (OUTPUT vTimeTz).
         IF VALID-HANDLE(iBrPas) THEN RUN GetTimeZone IN iBrPas (OUTPUT vTimeZone).

         PUT UNFORMATTED "=== АСИНХРОННЫЙ ЗАПУСК " string(vDateTz,"99/99/9999") " " 
                                                   string(vTimeTz,"hh:mm:ss") 
                                                   "(" vTimeZone ") ===" SKIP.
         PUT UNFORMATTED "  ФИЛИАЛ                   : " iFilial SKIP.
         PUT UNFORMATTED "  ДИРЕКТОРИЯ               : " iLog SKIP.
         PUT UNFORMATTED "  "

            (IF iProc BEGINS "PARS:" THEN
                           "ПАРСЕР     "
            ELSE IF filehelper:GetFilenameExt(iProc) =  "" THEN
                           "ТРАНЗАКЦИЯ "
            ELSE
                           "ПРОЦЕДУРА  ") "              : " iProc SKIP.
         PUT UNFORMATTED "  ПАРАМЕТРЫ                : " iParam SKIP.
         PUT UNFORMATTED "  PID                      : " 
                         STRING(processlist:GetCurrentProcessId()) SKIP.
         PUT UNFORMATTED "  ЗАПУСТИВШИЙ ПОЛЬЗОВАТЕЛЬ : " iUserId SKIP.
         PUT UNFORMATTED "  ДЛИНА ХЭША ПОЛЬЗОВАТЕЛЯ  : " iLength SKIP.
         PUT UNFORMATTED "  ТЕКУЩИЙ ПОЛЬЗОВАТЕЛЬ     : " USERID ("bisquit") SKIP.
         PUT UNFORMATTED "======================================================" SKIP.
   END METHOD.


   METHOD PRIVATE STATIC VOID Footer (INPUT iBrPas     AS HANDLE, 
                                      INPUT iProc      AS CHAR, 
                                      INPUT iParam     AS CHAR, 
                                      INPUT iStartTime AS DATETIME-TZ): 

         /* Пишем в лог заголовок запуска */
      DEF VAR vDateTz   AS DATE  NO-UNDO.
      DEF VAR vTimeTz   AS INT64 NO-UNDO.
      DEF VAR vTimeZone AS CHAR  NO-UNDO.


         /* Пишем в лог информацию об окончании */
      IF VALID-HANDLE(iBrPas) THEN RUN GetDate   IN iBrPas (OUTPUT vDateTz).
      IF VALID-HANDLE(iBrPas) THEN RUN GetTime   IN iBrPas (OUTPUT vTimeTz).
      IF VALID-HANDLE(iBrPas) THEN RUN GetTimeZone IN iBrPas (OUTPUT vTimeZone).

      PUT UNFORMATTED "АСИНХРОННОЕ ВЫПОЛНЕНИЕ " 
                      iProc "(" iParam ") ЗАВЕРШЕНО: " 
                      string(vDateTz,"99/99/9999") " " 
                      string(vTimeTz,"hh:mm:ss") "(" vTimeZone ") " SKIP.
      PUT UNFORMATTED "ВРЕМЯ ВЫПОЛНЕНИЯ ЗАДАЧИ: " + 
          STRING(INTERVAL(NOW, iStartTime, "seconds"), "hh:mm:ss")  SKIP.

   END METHOD.


   METHOD PUBLIC STATIC VOID SetContext ():
      
      IF PROGRAM-NAME(2) <> "asyncrunner.p" THEN RETURN.

      /* Входящие параметры */
      DEFINE VARIABLE vSessionID  AS CHARACTER            NO-UNDO. /* Идентификатор сессии */
      DEFINE VARIABLE vFilial     AS CHARACTER            NO-UNDO. /* Филиал */
      DEFINE VARIABLE vUserId     AS CHARACTER            NO-UNDO. /* Пользователь */
      DEFINE VARIABLE vProc       AS CHARACTER            NO-UNDO. /* Процедура/Транзакция */
      DEFINE VARIABLE vParam      AS CHARACTER            NO-UNDO. /* Параметры */
      DEFINE VARIABLE vLog        AS CHARACTER INIT ".~/" NO-UNDO. /* Файл логирования */
      DEFINE VARIABLE vCmdLine    AS CHARACTER            NO-UNDO. /* Командная строка */
      DEFINE VARIABLE vCmdLinSafe AS CHARACTER            NO-UNDO. /* Командная строка без хэша */
      DEFINE VARIABLE vTaskGroup  AS CHARACTER INIT ""    NO-UNDO. /* Группа задачи */
      DEFINE VARIABLE vSchedId    AS CHARACTER INIT ""    NO-UNDO. /* ID задачи */
      DEFINE VARIABLE vStartTime  AS DATETIME-TZ          NO-UNDO. /* Старт задачи */
      DEFINE VARIABLE vBrPas      AS CHARACTER            NO-UNDO. /* Пароль пользовaтля */
      DEFINE VARIABLE vBrPasDec   AS CHARACTER            NO-UNDO. /* Пароль пользовaтля декодированный */
      DEFINE VARIABLE vCloseDay   AS LOG                  NO-UNDO. /* Признак закрытия опер. дня */
      DEFINE VARIABLE vStream     AS CHARACTER            NO-UNDO. /* Собственный поток вывода */
      DEFINE VARIABLE vMThrEnv    AS CHARACTER            NO-UNDO. /* Собственный поток вывода */
      DEFINE VARIABLE vShMode     AS LOG                  NO-UNDO.
      
      /* Разбор параметров */
      vCmdLine = SESSION:PARAMETER.
      vCmdLine = REPLACE(vCmdLine, "~003", " ").
      vCmdLine = REPLACE(vCmdLine, "~004", "~/").
      
      /* Получение параметров */
      vSessionID  = ENTRY(1,  vCmdLine, "~001").
      vFilial     = ENTRY(2,  vCmdLine, "~001").
      vUserId     = ENTRY(3,  vCmdLine, "~001").
      vProc       = ENTRY(4,  vCmdLine, "~001").
      vBrPas      = ENTRY(5,  vCmdLine, "~001").
      vParam      = ENTRY(6,  vCmdLine, "~001").
      vTaskGroup  = ENTRY(7,  vCmdLine, "~001").
      vSchedId    = ENTRY(8,  vCmdLine, "~001").

      /* Получим параметры запуска */
      IF NOT {assigned vParam} THEN DO:
         vStream = "sched_" + vSchedId + "_" + vSessionID + ".log".
         IF NOT filehelper:FileExists (vStream) THEN DO:
            PUT UNFORMATTED "** Файл заголовка потока не найден: " vStream SKIP.
            RETURN.
         END.
         vParam = asyncrunhelper:GetResult(vStream, {&SCHED-PARAM-TAG}).
         vMThrEnv = asyncrunhelper:GetResult(vStream, {&SCHED-PARAM-STREAM}).
         IF NUM-ENTRIES(vMThrEnv) >= 2 THEN
         ASSIGN
            asyncrunhelper:ThreadParam     = asyncrunhelper:GetResult(vStream, 
                                             {&SCHED-PARAM-STREAM-OTHER})
            asyncrunhelper:ThreadsQuantity = INT64 (ENTRY (1, vMThrEnv))
            asyncrunhelper:ThreadNumber    = INT64 (ENTRY (2, vMThrEnv))
            asyncrunhelper:ThreadGuid      = asyncrunhelper:GetResult(vStream, 
                                            {&SCHED-PARAM-STREAM-ID})
            NO-ERROR.
      END.

      /* Сформируем параметры без хэша пароля для логирования */
      vCmdLinSafe = vCmdLine.
      ENTRY(5, vCmdLinSafe, "~001") = "".
      
      MAIN-BLOCK:
      DO 
         ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
         ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

         DEFINE VARIABLE vDynQuery   AS HANDLE NO-UNDO.
         DEFINE VARIABLE vDynBuffer  AS HANDLE NO-UNDO.
         DEFINE VARIABLE vKindQuery  AS HANDLE NO-UNDO. 
         DEFINE VARIABLE vKindBuffer AS HANDLE NO-UNDO.

         /* br-pass = vBrPas */
         DEFINE VARIABLE vHelper AS HANDLE NO-UNDO. /* хэндл для получения shared переменной */
         RUN asyncrunner.p PERSISTENT SET vHelper.

         IF VALID-HANDLE(vHelper) THEN RUN SetBrPass IN vHelper (INPUT vBrPas).
         IF VALID-HANDLE(vHelper) THEN RUN Decrypt IN vHelper (INPUT vBrPas, OUTPUT vBrPasDec).

         /* Начинаем логин под пользователем */         
         /* 1. Установим пользователя */
         IF {assigned vBrPas} THEN
         DO:
            IF NOT SetUserId(vUserID, vBrPasDec, "bisquit") THEN   
            DO:   
               PUT UNFORMATTED "Ошибка установки пользователя через SetUserId, " + 
                               "попытаемся установить пользователя через Impersonalize " 
                                vUserID " филиала " vFilial SKIP.
               IF NOT crypthelper:Impersonalize() THEN
               DO:
                  PUT UNFORMATTED "** Ошибка установки пользователя через Impersonalize " 
                                  vUserID " филиала " vFilial SKIP.
                  RETURN.
               END.
            END.
         END.
         ELSE IF NOT crypthelper:Impersonalize() THEN 
         DO:
            PUT UNFORMATTED "** Ошибка установки пользователя через Impersonalize " 
                            vUserID " филиала " vFilial SKIP.
            RETURN.
         END.
        
         RUN SetUserContext IN h_base (vFilial) NO-ERROR.    /* 3. Сделаем установку контекста */
         IF ERROR-STATUS:ERROR THEN
         DO:
            PUT UNFORMATTED "** Ошибка установки контекста пользователя " 
                             vUserID " филиала " vFilial SKIP.
            RETURN.
         END.
         RUN SetUser        IN h_base.                       /* 4. Установим пользователя */
         IF VALID-HANDLE(vHelper) THEN RUN SetPrinter IN vHelper (INPUT getPrinter()).
         IF VALID-HANDLE(vHelper) THEN RUN GetShMode IN vHelper (OUTPUT vShMode).

         RUN GetLic IN h_lic (IF vShMode THEN vFilial ELSE "", ?, ?) NO-ERROR.
         IF ERROR-STATUS:ERROR
            THEN RETURN.

         CREATE BUFFER vDynBuffer FOR TABLE "_user":U.
         CREATE QUERY vDynQuery.
         vDynQuery:SET-BUFFERS (vDynBuffer:HANDLE).
         vDynQuery:QUERY-PREPARE (SUBSTITUTE (
            "FOR EACH _user NO-LOCK WHERE " + 
               "_user._UserId = '&1'",
            vUserId)).

         vDynQuery:QUERY-OPEN().
         vDynQuery:GET-FIRST().
         IF NOT vDynBuffer:AVAILABLE THEN DO:
            PUT UNFORMATTED "** Ползователь " vUserId " не найден" SKIP.
            RETURN.
         END.

         /* Записываем в историю информацию о инициализации asyncrunner.p кроме хэша пароля */
         asyncrunhelper:LogHistory (vSchedId, vFilial, {&SCHED-STATE-INIT} + "~003" + vCmdLinSafe).

         /* Сформируем абсолютный путь сессии */
         FILE-INFO:FILE-NAME = ".".
         IF vLog BEGINS ".~/" AND FILE-INFO:FULL-PATHNAME <> ? THEN
            vLog = FILE-INFO:FULL-PATHNAME + filehelper:GetSlash() + 
                   SUBSTRING(vLog, 3, LENGTH(vLog) - 2).
      
         /* Зарегистрируем асинхронный процесс */
         IF VALID-HANDLE(vHelper) THEN RUN UnRegister IN vHelper.
         IF VALID-HANDLE(vHelper) THEN RUN Register IN vHelper (
                                vSessionID + "|" + 
                                vFilial    + "|" + 
                                vUserId    + "|" + 
                                vProc      + "|" + 
                                vParam     + "|" +
                                vTaskGroup + "|" + 
                                vSchedId,
                                vLog).

         /* Запишем в историю старт задачи */
         vCmdLinSafe = SUBSTITUTE("&1~001&2", vCmdLinSafe, 
                                  STRING(asyncrunhelper:GetLoadAverage ())).
         asyncrunhelper:LogHistory (vSchedId, vFilial, 
                                    {&SCHED-STATE-BEGIN} + "~003" + vCmdLinSafe).
         vStartTime = NOW. /* Запомним время */
         asyncrunhelper:Title(vHelper, vProc, vParam, vFilial, vLog, vUserId, LENGTH(vBrPas)).
     
         /* Определяем что запускать, транзакцию или процедуру */
         IF filehelper:GetFilenameExt(vProc) = "" AND 
            NOT vProc BEGINS "PROC:"              AND
            NOT vProc BEGINS "PARS:"   
         THEN
         DO:
            CREATE BUFFER vKindBuffer FOR TABLE "op-kind":U.
            CREATE QUERY vKindQuery.
            vKindQuery:SET-BUFFERS (vKindBuffer:HANDLE).
            vKindQuery:QUERY-PREPARE (SUBSTITUTE (
               "FOR FIRST op-kind NO-LOCK WHERE " + 
                  "op-kind.op-kind = '&1'",
               vProc)).
   
            vKindQuery:QUERY-OPEN().
            vKindQuery:GET-FIRST().

            IF vKindBuffer:AVAILABLE THEN 
            DO: 
               IF SearchPFile(vKindBuffer::proc) THEN
               DO: 
                  DEFINE VARIABLE vDate  AS DATE  NO-UNDO.
                  DEFINE VARIABLE vRecid AS INT64 NO-UNDO.
      
                  IF NUM-ENTRIES(vParam) >= 2 THEN
                  DO:
                     vDate  = DATE(ENTRY(1, vParam))  NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN PUT UNFORMATTED 
                        "** Первый параметр имеет тип DATA" SKIP.
      
                     vRecid = INT64(ENTRY(2, vParam)) NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN PUT UNFORMATTED 
                        "** Второй параметр имеет тип RECID" SKIP.
      
                     vCloseDay = NUM-ENTRIES(vParam) >= 3 AND ENTRY(3, vParam) = "ЗАКРЫТИЕОД".
                     RUN RunKind IN vHelper (vDate, vRecId). 
                  END.
                  ELSE 
                     PUT UNFORMATTED "** Кол-во параметров не соответствует, ожидается " + 
                                     "2 параметра для запуска транзакции" SKIP.
               END.
               ELSE 
                  PUT UNFORMATTED "** Не найдена процедура транзакции" + vKindBuffer::proc SKIP.
            END.
            ELSE
            DO:
               IF SearchPFile(vProc) THEN
                     /* Запуск процедуры с неопределённым кол-вом параметров */
                  RUN RUN_IO_Params IN h_xclass (INPUT vProc, INPUT-OUTPUT vParam, ".p", ?). 
               ELSE
                  PUT UNFORMATTED "** Не найдена процедура " + vProc SKIP.
            END.
         END.
         ELSE
         DO:
            IF vProc BEGINS "PROC:" THEN
            DO:
               vProc = SUBSTRING(filehelper:GetName(vProc), 6).
               IF SearchPFile(vProc) THEN
                     /* Запуск процедуры с неопределённым кол-вом параметров */
                  RUN RUN_IO_Params IN h_xclass (INPUT vProc, INPUT-OUTPUT vParam, ".p", ?). 
               ELSE
                  PUT UNFORMATTED "** Не найдена процедура " + vProc SKIP.
            END.
            ELSE IF vProc BEGINS "PARS:" THEN
            DO:
               DEF VAR vFlErr  AS INT64 NO-UNDO.
               DEF VAR vResult AS CHAR  NO-UNDO.

               PUT UNFORMATTED "Выполнение парсера: " SUBSTR(vProc, 6) SKIP.

               RUN ParsMain IN h_parsr (SUBSTR(vProc, 6), 
                               "", 
                               "",
                               OUTPUT vFlErr, 
                               OUTPUT vResult) NO-ERROR.

               PUT UNFORMATTED "Результат парсера (флаг) : " vFlErr  SKIP.
               PUT UNFORMATTED "Результат парсера (текст): " vResult SKIP.
            END.
            ELSE
            DO:
               IF {assigned vParam} THEN
                     /* Запуск процедуры планировщика, у него один параметр */
                  RUN VALUE(vProc) (vParam).  
               ELSE
                  RUN VALUE(vProc).
            END.    
         END.

         FINALLY:
            
            /* Пишем в лог информацию об окончании */
            asyncrunhelper:Footer (vHelper, vProc, vParam, vStartTime).
         
            /* Запишем в историю окончание задачи */
            ENTRY(9, vCmdLinSafe, "~001") = STRING(asyncrunhelper:GetLoadAverage ()) NO-ERROR. 
            asyncrunhelper:LogHistory (vSchedId, vFilial, {&SCHED-STATE-END} + "~003" + 
                                                 vCmdLinSafe + "~003" + 
                                                 STRING(INTERVAL(NOW, vStartTime, "seconds"), 
                                                 "hh:mm:ss")).

            /* Разрегистрация асинхронной задачи */
            IF VALID-HANDLE(vHelper) THEN RUN UnRegister IN vHelper.
            IF VALID-HANDLE(vHelper) THEN DELETE PROCEDURE vHelper.

            vDynQuery:QUERY-CLOSE()      NO-ERROR.
            vDynBuffer:BUFFER-RELEASE()  NO-ERROR.
            DELETE OBJECT vDynQuery      NO-ERROR.
            DELETE OBJECT vDynBuffer     NO-ERROR. 
            vKindQuery:QUERY-CLOSE()     NO-ERROR.
            vKindBuffer:BUFFER-RELEASE() NO-ERROR.
            DELETE OBJECT vKindQuery     NO-ERROR.
            DELETE OBJECT vKindBuffer    NO-ERROR. 
         END FINALLY.
      END.

   END METHOD.
      
   {pfuncdef &Reflection}
END CLASS. /* asyncrun_helper */
/* $LINTFILE='asyncrunhelper.cls' */
/* $LINTMODE='1,4,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='mete' */
/* $LINTDATE='23/06/2017 11:27:31.908+03:00' */
/*prosignfkqGdXLwMjKP7P8qGmGzow*/