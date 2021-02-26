/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: TMESS.PRO
      Comment: Процедуры интерфейса системных сообщений
   Parameters:
         Uses:
      Used BY:
      Created: 21.08.2003 TSL
     Modified: 12.05.2005 14:39 KSV      (0044952) Добавлена возможность
                                         запомнить ответы пользователя на сист.
                                         сообщения в процессе протоколирования,
                                         и затем автоматически применять их при
                                         возникновении сист. сообщений.
     Modified: 19.05.2005 14:39 NIK      Вывод протокола по "авторам".
     Modified: 08.06.2005 14:39 SADM     (0017074) Возможность отключения
                                         вопроса о сохранении ответов на
                                         транзакции в ДР СС_АвтоОтвет.
     Modified: 23.07.2005 Om  Доработка.
                        Увеличен формат поля для вывода номера сообщения.
     Modified: 14.02.2006 Om  Ошибка.
                        Некорректное управление описаниями процессов.
     Modified: 29.09.2008 17:37 KSV      (0098571) Добавлена функция
                                         Fill-AlertSysMes.
     Modified: 30.09.2008 15:46 KSV      (0098571) Исправление c PROGRAM-NAME
     Modified: 29.01.2009 17:52 buan     Выдача отчетов об операциях (_spoolm.tmp)
                                         в формате LANDSCAPE (126 символов) для
                                         QBIS (аявка 0098399)
     Modified: 11.06.2010 17:56 ksv      (0129243) Исправлена работа 
                                         Fill-AlertSysMes

   Init-SysMes       Инициализация
      Параметры:
         iProcBaseId    - Указатель на запись <имя таблицы>,<код записи>
         iProcName      - Название процесса (необязателен,если есть указатель)
         iMesClassCode  - Код классификатора сообщений

   Fill-SysMes       Создание и вывод сообщения
      Параметры:
         iAuthor     - Идентификатор бизнес-службы
         iMesCode    - Код сообщения из справочника
         iMesType    - Тип сообщения (необязателен при наличии кода)
         iMesText    - Текст сообщения или значения для шаблона сообщения
                       (необязателен при наличии кода)

   Fill-ProgressErr  Сохраняет и выводит прогрессовые сообщения об ошибках
      Параметры:
         iAuthor     - Идентификатор бизнес-службы

   End-SysMes          Вывод протокола
   Log-AutorMes        Вывод протокола по авторам

*/
/* Инициализация процесса протоколирования. */
PROCEDURE Init-SysMes.
   DEFINE INPUT PARAMETER iProcBaseId   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iProcName     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesClassCode AS CHARACTER NO-UNDO.

   DEF VAR vPClass      AS CHAR    NO-UNDO. /* Класс процесса. */
   DEF VAR vPID         AS CHAR    NO-UNDO. /* Идентификатор процесса. */
   DEF VAR vProcBaseId  AS CHAR    NO-UNDO.
   DEF VAR vLogBIS      AS LOGICAL NO-UNDO.

   DEFINE BUFFER OldStack FOR tt-SettStack. /* Локализация буфера. */
   DEFINE BUFFER op-kind  FOR op-kind.

   /* Код классификатора сообщений. */
   vMesClassCode =   IF iMesClassCode GT ""
                        THEN iMesClassCode
                        ELSE vMesClassCodeDef.
   /* Если протоколирование процесса уже идет,
   ** то необходимо скорректирвоать описание свойств.*/
   IF vLogFlag
   THEN DO:
      /* Поиск описания предыдущего процеса. */
      FIND LAST OldStack NO-ERROR.
      IF AVAIL OldStack
      THEN DO:
         CREATE tt-SettStack.
         BUFFER-COPY OldStack EXCEPT OldStack.ProcDesc TO tt-SettStack.
         /* Сохраняем новый код классификатора сообщений. */
         ASSIGN
            tt-SettStack.ProcDesc      = OldStack.ProcDesc + 1
            tt-SettStack.MesClassCode  = vMesClassCode
         .
      END.
      RETURN.
   END.
   IF (iProcBaseId EQ ""
   AND iProcName   EQ "")
    OR iProcBaseId EQ "AUTO,ИНИЦИАЛИЗАЦИЯ"
      /* общесистемный протокол (типа Процесс БИСквит) */
   THEN
      vLogBIS = YES.
      /* Протоколирование нового процесса. */
   ELSE
      ASSIGN
         vLogFlag = YES
         vLogBIS  = NO
      .
   /* запоминаем начальную процедуру процесса */
   vCurProc = PROGRAM-NAME(2).
   ASSIGN
      /* Определение класса процесса. */
      vPClass  =  ENTRY(1, iProcBaseId)
      /* Определение идентификатора процесса. */
      vPID     =  SUBSTRING(iProcBaseId, LENGTH(vPClass) + 2)
   .
   /* Для совмещения с предыдущими настройками
   ** корректируем текущий алгоритм. */
   IF  NUM-ENTRIES(iProcBaseId) EQ 1
   AND vPClass                  NE ""
   THEN ASSIGN
      vPID     = vPClass
      vPClass  = "op-kind"
   .
   /* Формирвоание названия процесса. */
   mProcName = iProcName.
   /* Получение настроек процесса. */
   RUN GetProcSetting (vPClass, vPID).
   /* Транзакция нужна для формирования наименования процесса. */
   FIND FIRST op-kind WHERE op-kind.op-kind EQ vPID NO-LOCK NO-ERROR.
   IF vLogBIS
   THEN DO:
      FIND FIRST tt-ProcMes WHERE
         tt-ProcMes.Proc-Id EQ 0
      NO-ERROR.
      IF NOT AVAILABLE tt-ProcMes
         THEN mProcName = "Общесистемный процесс.".
   END.

   IF    (vLogBIS
         AND NOT AVAILABLE tt-ProcMes)
      OR vLogFlag THEN
   DO:
      /* Создание записи о процессе */
      CREATE tt-ProcMes.
      ASSIGN tt-ProcMes.Proc-Id      = (IF vLogBIS THEN 0 ELSE SOURCE-PROCEDURE:UNIQUE-ID)
             tt-ProcMes.Proc-Date    = TODAY
             tt-ProcMes.Proc-TimeBeg = TIME
             tt-ProcMes.Op-Kind      = IF AVAILABLE op-kind
                                          THEN op-kind.op-kind
                                          ELSE ""
             tt-ProcMes.Proc-Name    = mProcName.
      CREATE tt-SettStack.
      /* Запоминаем данные процесса */
      ASSIGN
         tt-SettStack.Proc-Id    = tt-ProcMes.Proc-Id
         tt-SettStack.ProcDesc   = 1
         tt-SettStack.CurProc    = vCurProc
         tt-SettStack.ToScreen   = vToScreen
         tt-SettStack.ToFile     = vToFile
         tt-SettStack.DebugLev   = vDebugLev
         tt-SettStack.DelLog     = vDelLog
         tt-SettStack.LogFile    = vLogFile
         tt-SettStack.ViewLog    = vViewLog
         tt-SettStack.AutoAnswer = vAutoAnswer
         tt-SettStack.Answers    = vAnswers
         tt-SettStack.ProcHdl    = SOURCE-PROCEDURE:HANDLE
         tt-SettStack.Stck2Fl    = vStck2Fl
         /* При открытии нового процесса обнуляем счетчик. */
         vMesNum                 = 0
         vLogProcMes             = YES
      .
   END.
   /* Инициализируем ответы на сист. сообщения */
   RUN PresetAnswers(vAnswers).
   /* Вывод заголовка процесса в файл протокола */
   RUN Log-ProcMes.
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Помещает сообщение в системный лог и при необходимости выводит
               его на экран. 
  Parameters:  iAuthor   - код процесса, публикующего сообщение 
                           ("" - процесс по умолчанию)
               iMesCode  - код сообщения в классификаторе сообщений (необяз)
               iMesType  - тип сообщения 
                           < 0 - ошибка
                             0 - предупреждение
                           1,2 - информационные сообщения
                           3,4 - вопросы  
               iMesText  - текст сообщения, либо если указан iMesCode, параметры
                           к сообщению указанного в классификаторе
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-SysMes:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   
   RUN Fill-SysMes-Basic(iAuthor,iMesCode,iMesType,iMesText,NO).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Помещает сообщение в системный лог и ВСЕГДА выводит его на экран.
               Необходима для QBIS, в качестве замены ALERT-BOX 
  Parameters:  iAuthor   - код процесса, публикующего сообщение 
                           ("" - процесс по умолчанию)
               iMesCode  - код сообщения в классификаторе сообщений (необяз)
               iMesType  - тип сообщения 
                           < 0 - ошибка
                             0 - предупреждение
                           1,2 - информационные сообщения
                           3,4 - вопросы  
               iMesText  - текст сообщения, либо если указан iMesCode, параметры
                           к сообщению указанного в классификаторе
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-AlertSysMes:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   
   RUN Fill-SysMes-Basic(iAuthor,iMesCode,iMesType,iMesText,YES).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Помещает сообщение в системный лог и при необходимости выводит
               его на экран.
  Parameters:  iAuthor   - код процесса, публикующего сообщение 
                           ("" - процесс по умолчанию)
               iMesCode  - код сообщения в классификаторе сообщений (необяз)
               iMesType  - тип сообщения 
                           < 0 - ошибка
                             0 - предупреждение
                           1,2 - информационные сообщения
                           3,4 - вопросы  
               iMesText  - текст сообщения, либо если указан iMesCode, параметры
                           к сообщению указанного в классификаторе
               iMesShow  - yes - всегда выводить сообщение на экран          
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-SysMes-Basic PRIVATE:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesShow AS LOGICAL   NO-UNDO.
   
   DEF VAR vMesCode     AS CHAR     NO-UNDO.
   DEF VAR vMesText     AS CHAR     NO-UNDO.
   DEF VAR vMesDesc     AS CHAR     NO-UNDO.
   DEF VAR vMesType     AS CHAR     NO-UNDO.
   DEF VAR vproc-name   AS CHAR     NO-UNDO.
   DEF VAR vparam       AS CHAR     NO-UNDO.
   DEF VAR vI           AS INT64      NO-UNDO.
   DEF VAR vtmp_sh      AS CHAR     NO-UNDO.
   DEF VAR vtmp_cs      AS CHAR     NO-UNDO.
   DEF VAR vMenuRow     AS CHAR     NO-UNDO.
   DEF VAR vMenuTitle   AS CHAR     NO-UNDO.
   DEF VAR vMenuChoises AS CHAR     NO-UNDO.
   DEF VAR vPickValue   AS CHAR     NO-UNDO.
   DEF VAR hNewMess     AS HANDLE   NO-UNDO.
   DEF VAR vScreen      AS LOG      NO-UNDO. /* Признак вывода сообщения на экран. */
   

   ASSIGN
      vMesText     = ENTRY(1,iMesText,"|")
      vMesType     = iMesType
      vMenuChoises = IF NUM-ENTRIES(iMesText,"|") > 1
                        THEN ENTRY(2,iMesText,"|")
                        ELSE ""
      iMesText     = ENTRY(1,iMesText,"|")
      .

   RELEASE code.
   IF iMesCode > "" THEN
   DO:
      /* чтение текста сообщения из классификатора сообщений*/
      IF NUM-ENTRIES(iMesCode) > 1 THEN
      DO:
         vMesCode = ENTRY(2,iMesCode).
         /* ищем по указанному классификатору */
         FIND FIRST code WHERE code.class = ENTRY(1,iMesCode)
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.
      END.
      ELSE
         vMesCode = iMesCode.

      IF NOT AVAILABLE code AND
         vMesClassCodeDef <> vMesClassCode THEN
         /* ... и если класификатор, указанный при инициализации
            отличается от классификатора по умолчанию, то ищем
            по классификатору, указанному при инициализации */
         FIND FIRST code WHERE code.class = vMesClassCode
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.

      IF NOT AVAILABLE code THEN
         /* ищем по классификатору по умолчанию */
         FIND FIRST code WHERE code.class = vMesClassCodeDef
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.

      /* Выдаем сообщение о неверном коде ошибки. */
      IF NOT AVAILABLE code
      THEN DO:
         RUN Fill-SysMes ("iAuthor", "", "-1",
                          "Кода сообщения '" + vMesCode +
                          "', нет в классификаторе видов сообщений.").
         RETURN.
      END.

      ASSIGN
         vMesText = REPLACE(code.description[1],"~~n","~n")
         vMesDesc = code.description[2]
         vMesType = IF vMesType > "" THEN vMesType ELSE code.val.

      /* обработка шаблонов */
      IF INDEX(vMesText,"%") > 0 AND
         INDEX(iMesText,"%") > 0 THEN
      DO vI = 2 TO NUM-ENTRIES(iMesText,"%"):
         vtmp_sh = SUBSTRING(ENTRY(vI,iMesText,"%"),3).
         vtmp_cs = "%" + SUBSTRING(ENTRY(vI,iMesText,"%"),1,1).
         IF INDEX(vMesText,vtmp_cs) > 0 THEN
            SUBSTRING(vMesText,INDEX(vMesText,vtmp_cs),2) = vtmp_sh.
      END.

   END.
   ELSE vMesCode = "".

   /* Отсекаем сообщения по уровню отладки и при условии, что
   ** выключен обязательный показ сообщений */
   IF NOT iMesShow AND 
      INT64(vMesType) GT vDebugLev AND 
      NOT CAN-DO ("3,4", vMesType)
      THEN RETURN.

   /* номер сообщения */
   vMesNum = vMesNum + 1.

   /* добавляем в сообщение код сообщения */
   IF iMesCode > "" AND
      INT64(vMesType) < 3 THEN
      vMesText = vMesText + " (" + iMesCode + ")".

   /* Создание записи сообщения */
   CREATE tt-SysMes.
   ASSIGN tt-SysMes.Proc-Id    = tt-ProcMes.Proc-Id
          tt-SysMes.Mes-Num    = vMesNum
          tt-SysMes.Mes-Date   = TODAY
          tt-SysMes.Mes-Time   = TIME
          tt-SysMes.Mes-Class-Code = IF AVAIL code
                                       THEN code.class
                                       ELSE ""
          tt-SysMes.Mes-Code   = vMesCode
          tt-SysMes.Mes-Text   = vMesText
          tt-SysMes.Mes-Author = IF iAuthor > ""
                                    THEN iAuthor
                                    ELSE PROGRAM-NAME(3)
          tt-SysMes.Mes-Stack  = GetStack()
          tt-SysMes.Mes-Type   = vMesType
   .
   /* Возможный внешний обработчик для сообщения */
   hNewMess = BUFFER tt-SysMes:HANDLE.
   PUBLISH "tmess-fill-sysmes-event" (hNewMess).
   
   /* вывод сообщения в файл */
   RUN Log-ProcMes.
   IF vToFile = "Да" THEN
   DO:
      vMesText = REPLACE(vMesText,"~n","").
      OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
      PUT STREAM LogStream UNFORMATTED
         STRING(tt-SysMes.Mes-Num,">>>>>>>9") " "
         /*tt-SysMes.Mes-Date " " */
         STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
         tt-SysMes.Mes-Type   FORMAT "x(2)" " "
         vMesText             /*FORMAT "X(100)" */ " "
         FILL(" ",62 - LENGTH(vMesText)) "<"
         tt-SysMes.Mes-Author /*FORMAT "x(40)"*/ ">"  +  
         (IF vStck2Fl
            THEN (" Стек: " + QUOTER (tt-SysMes.Mes-Stack))
            ELSE ""
         )
         SKIP.
      OUTPUT STREAM LogStream CLOSE.
   END.
                        /* Формируем признак вывода сообщений на экран. */
   vScreen =   iMesShow OR      
               (vToScreen EQ "Да" AND NOT SESSION:REMOTE).
   
   PUBLISH "tmess-fill-sysmes-event-no-screen" (hNewMess, INPUT-OUTPUT vScreen).
   
                        /* Блок формированя автоответов
                        ** и вывода на экран. */
   BLCK_VIEW:
   DO
   ON ERROR    UNDO BLCK_VIEW, LEAVE BLCK_VIEW
   ON ENDKEY   UNDO BLCK_VIEW, LEAVE BLCK_VIEW:
      RELEASE code.
                        /* Определение типа вывода на экран. */
      IF vMesTypeClassCode NE vMesTypeClassCodeDef
                        /* Ищем в заданном классификаторе типов сообщений. */
         THEN FIND FIRST code WHERE
                  code.class  EQ vMesTypeClassCode
            AND   code.code   EQ vMesType
         NO-LOCK NO-ERROR.
                        /* Ищем в классификаторе типов сообщений по умолчанию */
      IF NOT AVAILABLE code
         THEN FIND FIRST code WHERE
                  code.class  EQ vMesTypeClassCodeDef
            AND   code.code   EQ vMesType
         NO-LOCK NO-ERROR.
                        /* Тип вывода на экран не определен,
                        ** Выдаем сообщение с настройками по умолчанию. */
      IF NOT AVAILABLE code
      THEN DO:
         IF vScreen
            THEN RUN 
            message.p (tt-SysMes.Mes-Text, "BRIGHT-", "INFO", "", "", NO).
         LEAVE BLCK_VIEW.
      END.
                        /* Тип "сообщение". */
      IF code.val BEGINS "message"
      THEN DO:
                        /* Поиск предустановленного ответа. */
         RUN SetAction (
            tt-ProcMes.Proc-Id,  /* ID процесса. */
            vMesType,            /* Код типа сообщения.  */
            vMesCode,            /* Код сообщения. */
            ""                   /* Список ответов. */
         ).
                        /* Если получен ответ,
                        ** или выводить на экран запрещено, то выходим. */
         IF NOT iMesShow AND (RETURN-VALUE EQ "" OR NOT vScreen) THEN 
            LEAVE BLCK_VIEW.
         RUN message.p (tt-SysMes.Mes-Text,
                        ENTRY(2,code.val),
                        ENTRY(3,code.val),
                        ENTRY(4,code.val),
                        ENTRY(5,code.val),
                        ENTRY(6,code.val)).
                        /* Предлагаем пользователю применять его ответ
                        ** ко всем таким сообщениями */
         RUN SetAnswer(vMesType,vMesCode,tt-SysMes.Mes-Text,"").
      END.
                        /* Тип "меню". */
      ELSE IF code.val BEGINS "messmenu"
      THEN DO:
         ASSIGN
            vMenuRow     = ENTRY(1,vMesDesc,";")
            vMenuTitle   = IF NUM-ENTRIES(vMesDesc,";") > 1
                           THEN ENTRY(2,vMesDesc,";")
                           ELSE "".

         IF vMenuChoises = "" THEN
            vMenuChoises = IF NUM-ENTRIES(vMesDesc,";") > 2
                           THEN ENTRY(3,vMesDesc,";")
                           ELSE "".
         IF vMenuRow = "" THEN
            vMenuRow = ENTRY(3,ENTRY(1,code.val),"(").
         IF vMenuRow = "" THEN
            vMenuRow = "9".
         IF vMenuTitle = "" THEN
            vMenuTitle = ENTRY(2,code.val).
         IF vMenuTitle = "" THEN
            vMenuTitle = "[МЕНЮ]".
                        /* Поиск предустановленного ответа. */
         RUN SetAction (
            tt-ProcMes.Proc-Id,  /* ID процесса. */
            vMesType,            /* Код типа сообщения.  */
            vMesCode,            /* Код сообщения. */
            vMenuChoises         /* Список ответов. */
         ).
                        /* Если получен ответ,
                        ** или выводить на экран запрещено, то выходим. */
         IF NOT iMesShow AND (RETURN-VALUE EQ "" OR NOT vScreen) THEN 
            LEAVE BLCK_VIEW.
         
         RUN messmenu.p (INT64(vMenuRow),
                         vMenuTitle,
                         tt-SysMes.Mes-Text,
                         vMenuChoises).

         vPickValue =   IF INT64(pick-value) > 0
                           THEN ENTRY(INT64(pick-value),vMenuChoises)
                           ELSE "Отказ от выбора".
         RUN LogAnswer (vPickValue).
         /* Commented BY KSV: Предлагаем пользователю применять его ответ
         ** ко всем таким сообщениями */
         RUN SetAnswer(vMesType,vMesCode,tt-SysMes.Mes-Text,vMenuChoises).
      END.
                        /* Если можно выводить сообщение на экран,
                        ** только в этом случае запускаем метод. */
      ELSE IF vScreen
      THEN DO:
         RUN procname  IN h_xclass  (INPUT  code.val,
                                    OUTPUT vproc-name,
                                    OUTPUT vparam,
                                    INPUT  "",
                                    INPUT  "").
         IF vproc-name > "" THEN
            RUN run_params IN h_xclass (vproc-name,vparam,"",?) NO-ERROR.
      END.
   END.
   RETURN.
END PROCEDURE. /* Fill-SysMes */

/*----------------------------------------------------------------------------*/
/* Сохранение прогрессовых ошибок                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE Fill-ProgressErr:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vI AS INT64    NO-UNDO.

   IF iAuthor = "" THEN
      iAuthor = PROGRAM-NAME(2).

   DO vI = 1 TO ERROR-STATUS:NUM-MESSAGES:
      RUN Fill-SysMes (iAuthor,"","-1",ERROR-STATUS:GET-MESSAGE(vI)).
   END.

END PROCEDURE. /* Fill-ProgressErr */

/************************************************************************/
/* Возвращает код последнего сообщения                                  */
/************************************************************************/
PROCEDURE Get-LastSysMes.
DEFINE OUTPUT PARAMETER oMesCode   AS CHARACTER  NO-UNDO.

   FIND LAST tt-SysMes NO-ERROR.
   IF AVAILABLE tt-SysMes THEN
      oMesCode = tt-SysMes.Mes-Code.

END PROCEDURE. /* Get-LastSysMes */

/******************************************************************************/
/* Вывод протокола                                                            */
/******************************************************************************/
PROCEDURE End-SysMes-Local:
   IF AVAIL tt-ProcMes THEN
   DO:
      {setdest.i
         &filename = "'_spoolm.tmp'"
         &stream   = "STREAM ProtStream"
         &nodef    = "/*"}
   
      PUT STREAM ProtStream UNFORMATTED
         "                             ПРОТОКОЛ СООБЩЕНИЙ"            SKIP
         tt-ProcMes.Proc-Date                                         " "
         tt-ProcMes.Op-Kind   FORMAT "X(10)"
         tt-ProcMes.Proc-Name                                         SKIP
         "Процесс начат: " STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS")
         " Окончен: " STRING(tt-ProcMes.Proc-TimeEnd,"HH:MM:SS")      SKIP.
   
      FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
         PUT STREAM ProtStream UNFORMATTED
            STRING(tt-SysMes.Mes-Num,">>>>>>>9")    " "
            STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
            tt-SysMes.Mes-Type   FORMAT "x(2)"    " "
            tt-SysMes.Mes-Text                    " "
            FILL(" ",62 - LENGTH(tt-SysMes.Mes-Text))
            "<" tt-SysMes.Mes-Author  ">"         SKIP.
      END.
      {preview.i
         &filename = "'_spoolm.tmp'"
         &stream = "STREAM ProtStream"}
   END.
END PROCEDURE.                                   /* End-SysMes-Local          */

/******************************************************************************/
/* Вывод протокола, завершение протоколирования процесса                      */
/************************************************************************&*****/
PROCEDURE End-SysMes:
   DEFINE VARIABLE vStack     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vInClosing AS LOGICAL    NO-UNDO.

   DEFINE BUFFER SameDescr FOR tt-SettStack. /* Локализация буфера. */
   DEFINE BUFFER op-kind   FOR op-kind.

   /* Проверка на окончание процесса. */
   vStack = GetStack().
   IF LOOKUP(vCurProc,vStack) LT NUM-ENTRIES(vStack)
   THEN DO:
      /* Восстанавливаем данные о классификаторе сообщений
      ** для предыдущей процедуры. */
      FIND LAST tt-SettStack NO-ERROR.
      IF AVAILABLE tt-SettStack
         THEN vMesClassCode = tt-SettStack.MesClassCode.
         /* Т.к. процесс не закончен,
         ** закончился только рекурсивный вызов. */
      RETURN.
   END.

   /* Закрываем протоколирование и все вложеннные протоколирования */
   _prot_closing:
   REPEAT:
      FIND LAST tt-ProcMes WHERE tt-ProcMes.Proc-Id >= SOURCE-PROCEDURE:UNIQUE-ID
                             AND CAN-FIND(FIRST tt-SettStack WHERE tt-SettStack.Proc-Id = tt-ProcMes.Proc-Id)
      NO-LOCK NO-ERROR.
      IF AVAIL tt-ProcMes THEN DO:
         vInClosing = YES.

         FIND FIRST tt-SettStack WHERE tt-SettStack.Proc-Id = tt-ProcMes.Proc-Id.
         
         /* Commented BY KSV: Если включен режим авт. ответа на сист. сообщения,
         ** пытаемся сохранить ответы от пользователя на сообщения в текущей сессии
         ** в д.р. транзакции */
         /* Commented by SHIB: пытаемся сохранить эти ответы, 
         ** только если пользователь имеет право на это действие (GetSurrPermission)*/
         IF  tt-SettStack.AutoAnswer
         AND {assigned tt-ProcMes.Op-Kind} THEN
         autoans:
         DO:
            /* Commented BY KSV: Получаем список ответов на сист. сообщения */
            RUN GetAnswers (OUTPUT vTmp).
            /* Commented BY KSV: Если список ответов не пуст, процесс протоколирования
            ** связан с транзакцией и списки ответов до и после транзакции отличаются
            ** записываем ответы в доп.реквизит */
            IF  vTmp <> "" AND
                vTmp <> vAnswers AND
                NOT CAN-DO(vAnswers, "NOTSAVE")
            THEN DO:
               FIND FIRST op-kind WHERE
                          op-kind.op-kind EQ tt-ProcMes.Op-Kind NO-LOCK NO-ERROR.
               IF  AVAIL op-kind 
               AND 
                   /* Проверка доступа к таблице. */
                   (NOT GetTablePermission ("op-kind", "w")
                   /* Проверка доступа к объектам класса. */
                   OR NOT GetSurrPermission ("class", op-kind.class-code, "w"))
               THEN
                  LEAVE autoans.
               /* Commented BY KSV: Спрашиваем пользователя о желании сохранить ответы
               ** */
               pick-value = "NO".

/*
               RUN message.p("Применять сохраненные ответы на системные сообщения~n" +
                             "для всех запусков транзакции [" + tt-ProcMes.Op-Kind + "]?",
                             "","QUESTION","YES-NO","",NO).
*/

               /* Commented BY KSV: Сохраняем ответы, если пользователь согласен */
               IF pick-value = "YES" THEN
               DO TRANSACTION
                  ON ERROR UNDO,LEAVE
                  ON STOP  UNDO,LEAVE:
/*
                  UpdateSigns("op-kind",tt-ProcMes.Op-Kind,"СС_АвтоОтвет",vTmp,NO).
*/
               END.  /* END OF BLOCK */
            END.
         END.
         
         /*------------------------------ Удаление персональных настроек по процессу --*/
         FOR EACH tt-MsgSet WHERE
                  tt-MsgSet.Proc-Id EQ tt-ProcMes.Proc-id:
            DELETE tt-MsgSet.
         END.
         
         IF tt-ProcMes.Proc-Id > 0 THEN
            tt-ProcMes.Proc-TimeEnd = TIME.
         
         /*--------------------------------------------------------- Вывод протокола --*/
         IF  tt-SettStack.ViewLog NE "Нет"
         AND tt-ProcMes.Proc-Id   GT 0 /* этот протокол не общессистемный */
         THEN DO:
            IF tt-SettStack.ViewLog NE "Да" THEN
               RUN Log-AutorMes(tt-SettStack.ViewLog). /* Авторские сообщения */
         
            IF LOOKUP("Да",tt-SettStack.ViewLog) NE 0 AND 
               NOT SESSION:REMOTE         THEN   /* Не AppServer                    */
            DO:
               {setdest.i
                  &filename = "'_spoolm.tmp'"
                  &stream   = "STREAM ProtStream"
                  &cols     = 126
                  &option   = "LANDSCAPE"
                  &nodef    = "/*"}
         
               PUT STREAM ProtStream UNFORMATTED
                  "                             ПРОТОКОЛ СООБЩЕНИЙ"            SKIP
                  tt-ProcMes.Proc-Date                                         " "
                  tt-ProcMes.Op-Kind   FORMAT "X(10)"
                  tt-ProcMes.Proc-Name                                         SKIP
                  "Процесс начат: " STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS")
                  " Окончен: " STRING(tt-ProcMes.Proc-TimeEnd,"HH:MM:SS")      SKIP.
         
               FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
                  PUT STREAM ProtStream UNFORMATTED
                     STRING(tt-SysMes.Mes-Num,">>>>>>>9")    " "
                     STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
                     tt-SysMes.Mes-Type   FORMAT "x(2)"    " "
                     tt-SysMes.Mes-Text                    " "
                     FILL(" ",62 - LENGTH(tt-SysMes.Mes-Text))
                     "<" tt-SysMes.Mes-Author  ">"         SKIP.
               END.
               {preview.i
                  &filename = "'_spoolm.tmp'"
                  &stream = "STREAM ProtStream"}
            END.                                       /* IF LOOKUP("Да",vViewLog)  */
         END.                                          /* IF vViewLog NE "Нет"      */

         ASSIGN
            vLogFlag    = NO
            vLogProcMes = tt-ProcMes.Proc-Id NE 0
         .
         /*------------------------------------------------------ удаление протокола --*/
         IF tt-SettStack.DelLog = "Да" THEN DO:
            FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
               DELETE tt-SysMes.
            END.
            DELETE tt-ProcMes.
         END.
         
         /*----------------------------------------------- Закрытие протоколирования --*/
         IF AVAILABLE tt-SettStack THEN
         DO:
            FOR EACH SameDescr WHERE
               SameDescr.Proc-Id EQ tt-SettStack.Proc-Id:
               DELETE SameDescr.
            END.
         END.
      END.
      ELSE vInClosing = NO.

      /* Настройки последнего протоколирования */
      FIND LAST tt-SettStack NO-ERROR.
      IF AVAIL tt-SettStack THEN
      DO:
         FIND FIRST tt-ProcMes WHERE
                    tt-ProcMes.Proc-Id = tt-SettStack.Proc-Id
         NO-ERROR.
         ASSIGN
            vMesNum     = tt-SettStack.MesNum
            vCurProc    = tt-SettStack.CurProc
            vToScreen   = tt-SettStack.ToScreen
            vToFile     = tt-SettStack.ToFile
            vDebugLev   = tt-SettStack.DebugLev
            vDelLog     = tt-SettStack.DelLog
            vLogFile    = tt-SettStack.LogFile
            vViewLog    = tt-SettStack.ViewLog
            vAutoAnswer = tt-SettStack.AutoAnswer
            vAnswers    = tt-SettStack.Answers
            vStck2Fl    = tt-SettStack.Stck2Fl
         .
      END.

      IF NOT vInClosing THEN LEAVE _prot_closing.
   END.

   RETURN.
END PROCEDURE.                                   /* End-SysMes                */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE Log-AutorMes:
   DEFINE INPUT PARAMETER iAutorList AS CHAR NO-UNDO.

   DEFINE VAR vAutorOne AS CHAR                    NO-UNDO.
   DEFINE VAR vAutorItm AS INT64                 NO-UNDO.
   DEFINE VAR vAll      AS CHAR                    NO-UNDO.
   DEFINE VAR vItm      AS INT64                 NO-UNDO.
   DEFINE VAR printer-c AS CHAR     case-sensitive NO-UNDO.

   IF NOT SESSION:REMOTE THEN /* Не AppServer   */
   DO:
      {setdest.i  &filename = "'_spoola.tmp'"
                  &stream   = "STREAM ProtStream"
                  &nodef    = "/*"}
   
      DO vAutorItm = 1 TO NUM-ENTRIES(iAutorList):
         vAutorOne = ENTRY(vAutorItm,iAutorList).
   
         IF vAutorOne EQ "Да" OR vAutorOne EQ "Нет" THEN NEXT.
   
         PUT STREAM ProtStream UNFORMATTED
            "                             ПРОТОКОЛ СООБЩЕНИЙ"    SKIP
            tt-ProcMes.Proc-Date                 " "
            tt-ProcMes.Op-Kind   FORMAT "X(10)"  " "
            "АВТОР ПРОТОКОЛА: "  vAutorOne                       SKIP(2).
   
         FOR EACH tt-SysMes WHERE
                  tt-SysMes.Proc-Id    EQ tt-ProcMes.Proc-id
              AND tt-SysMes.Mes-Author EQ vAutorOne:
   
            vAll = replace(tt-SysMes.Mes-Text,"(" + tt-SysMes.Mes-Code + ")","").
   
            DO vItm = 1 TO NUM-ENTRIES(vAll,"~n"):
               PUT STREAM ProtStream UNFORMATTED
                  string("<" + tt-SysMes.Mes-Code + ">","x(12)") " "
                  ENTRY(vItm,vAll,"~n")                          SKIP.
            END.
         END.
   
         PUT STREAM ProtStream UNFORMATTED SKIP(2).
   
      END.
   
      {preview.i &filename = "'_spoola.tmp'"
                 &stream   = "STREAM ProtStream"
      }
   END.
END PROCEDURE.
/*============================================================================*/
/*      Процедуры для внутреннего использования                               */
/*============================================================================*/
PROCEDURE Get-BufMes:
   DEFINE OUTPUT PARAMETER TABLE FOR tt-ProcMes.
   DEFINE OUTPUT PARAMETER TABLE FOR tt-SysMes.
END PROCEDURE.                                   /*                           */

/* процедура добавляет записи переданной входным параметром таблицы
** в таблицу tt-SysMes как есть */
PROCEDURE Add-BufMes.
   DEFINE INPUT PARAMETER TABLE FOR tt-SysMes APPEND. /* таблица с добавляемыми сообщениями */
END PROCEDURE.

/* процедура добавляет записи переданной входным параметром таблицы
** в таблицу tt-SysMes с заменой идентификатора процесса Proc-Id */
PROCEDURE Add-BufMesEx.
   DEFINE INPUT PARAMETER TABLE FOR tt-AddSysMes.     /* таблица с добавляемыми сообщениями */
   DEFINE INPUT PARAMETER iProcID AS INT64     NO-UNDO. /* идентификатор процесса */

   FOR EACH tt-AddSysMes:
      tt-AddSysMes.Proc-Id = iProcID.
   END.
   RUN Add-BufMes(TABLE tt-AddSysMes).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Вывод заголовка процесса в файл протокола                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE Log-ProcMes:

   IF       vToFile  EQ "Да"
      AND   vLogProcMes       THEN
   DO:
                        /* Не AppServer. */
      IF NOT SESSION:REMOTE THEN
      DO:
         OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
         PUT STREAM LogStream UNFORMATTED
            (IF auto THEN "-" ELSE FILL("-",150)) SKIP
            tt-ProcMes.Proc-Date " "
            STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS") " "
            tt-ProcMes.Op-Kind FORMAT "X(10)"
            tt-ProcMes.Proc-Name SKIP.

         IF vStck2Fl THEN
         DO:
            PUT STREAM LogStream UNFORMATTED
            tt-ProcMes.Proc-Date " "
            STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS") " "
            tt-ProcMes.Op-Kind FORMAT "X(10)"
            "Включен режим вывода стека в файл ~"СС_СтекВФайл~"."
            SKIP.
         END.
         OUTPUT STREAM LogStream CLOSE.
      END.
      vLogProcMes = NO.
   END.
END PROCEDURE.                                   /*                           */
/*----------------------------------------------------------------------------*/
/* Получение настроек процесса.                                               */
/*----------------------------------------------------------------------------*/
PROCEDURE GetProcSetting.

   DEF INPUT PARAM iPType AS CHAR NO-UNDO. /* Класс процесса. */
   DEF INPUT PARAM iPID   AS CHAR NO-UNDO. /* Идентификатор  процесса. */

   DEF VAR vFileName AS CHAR   NO-UNDO. /* Имя файла протокола. */

   DEF BUFFER op-kind FOR op-kind. /* Локализация буфера. */

   /* Читаем системные настройки протоколирования. */
   ASSIGN
      vToScreen         =  FGetSetting("СистСообщения","СС_ВыводНаЭкран","Нет")
      vToFile           =  FGetSetting("СистСообщения","СС_ВыводВФайл","Нет")
      vDebugLev         =  INT64(FGetSetting("СистСообщения","СС_УровеньОтладк","0"))
      vViewLog          =  FGetSetting("СистСообщения","СС_ВыводПрткл","Нет")
      vDelLog           =  FGetSetting("СистСообщения","СС_УдалятьПрткл","Да")
      vMesTypeClassCode =  FGetSetting("СистСообщения", "СС_КлТиповСообщ","КодОшТип")
      vStck2Fl          =  FGetSetting("СистСообщения","СС_СтекВФайл","Нет")  EQ "Да"
   .
   /* Если передан идентификатор процесса,
   ** то пробуем найти его описание. */
   IF LENGTH (iPID) GT 0
      THEN FIND FIRST op-kind WHERE
         op-kind.op-kind EQ iPID
      NO-LOCK NO-ERROR.
   /* Если найден идентификатор процесса,
   ** то берем описание процесса. */
   IF AVAIL op-kind
   THEN DO:
      ASSIGN
         vToScreen         =  GetXattrValueEx("op-kind", iPID, "СС_ВыводНаЭкран", vToScreen)
         vToFile           =  GetXattrValueEx("op-kind", iPID, "СС_ВыводВФайл",   vToFile)
         vViewLog          =  GetXattrValueEx("op-kind", iPID, "СС_ВыводПрткл",   vViewLog)
         vDelLog           =  GetXattrValueEx("op-kind", iPID, "СС_УдалятьПрткл", vDelLog)
         vFileName         =  GetXattrValueEx("op-kind", iPID, "СС_ФайлПрткл",    vLogFile)
         vAnswers          =  GetXattrValueEx("op-kind", iPID, "СС_АвтоОтвет",    "")
         vDebugLev         =  INT64 (GetXattrValueEx("op-kind", iPID, "СС_УровеньОтладк", STRING (vDebugLev)))
         vMesTypeClassCode =  GetXattrValueEx("op-kind", iPID, "СС_КлТиповСообщ",vMesTypeClassCode)
         vStck2Fl          =  GetXattrValueEx("op-kind", iPID, "СС_СтекВФайл", STRING (vStck2Fl,"Да/Нет")) EQ "Да"
      .
      /* Если в настройке указан парсерное выражение,
      ** то запускаем обработчик парсера.
                        ** Пока обрабатывается только название файла. */
      IF INDEX (vFileName, "<") GT 0
         THEN RUN pptmess.p (vFileName, OUTPUT vLogFile).
         ELSE vLogFile = vFileName.
   END.
   /* Разбираем тип процесса. */
   CASE iPType:
      /* Процесс: "стандартная транзакция". */
      WHEN "op-kind"
      THEN DO:
         IF NOT AVAILABLE op-kind
            THEN RETURN.
                        /* Корректируем название файла. Используется в APPL-SRV. */
         IF       LENGTH   (mPrefLog)  GT 0
            AND   NOT (vLogFile        BEGINS mPrefLog)
            THEN vLogFile =  mPrefLog + vLogFile.
                        /* Если в реквизите СС_АвтоОтвет значение НЕТ/NO
                        ** выключаем возможность авт. ответов, при этом учитывается
                        ** возможность вывода сообщений на экран. */
         vAutoAnswer = NOT CAN-DO ("NO,НЕТ",vAnswers) AND vToScreen EQ "Да".
         IF mProcName EQ ""
            THEN mProcName = op-kind.name-opkind.
      END.
      /* Настройки "batch" режима. */
      WHEN "AUTO"
      THEN ASSIGN
         vToScreen   = "Нет"
         vViewLog    = "Нет"
         mProcName   = "Сервер почтовых сообщений (" + iPID  + ")."
      .
   END CASE.
   RETURN.
END PROCEDURE.

/* Сохранение ответа в таблице и вывод в файл. */
PROCEDURE LogAnswer.
   DEF INPUT PARAM iAnswer AS CHAR NO-UNDO. /* ответ на сообщение. */

   tt-SysMes.Mes-Text = tt-SysMes.Mes-Text + " Выбрано: " + iAnswer.

   IF vToFile = "Да"
   THEN DO:
      OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
      PUT STREAM LogStream
         UNFORMATTED
         SPACE(28) " Выбрано: " + iAnswer FORMAT "X(90)"
         SKIP
      .
      OUTPUT STREAM LogStream CLOSE.
   END.
   RETURN.
END PROCEDURE.

/* Производит установку ответа, если таковой есть. */
PROCEDURE SetAction.
   DEF INPUT PARAM iPId       AS INT64   NO-UNDO. /* Идентификатор процесса. */
   DEF INPUT PARAM iMsgType   AS CHAR  NO-UNDO. /* Код типа сообщения. */
   DEF INPUT PARAM iMsgCode   AS CHAR  NO-UNDO. /* Код сообщения. */
   DEF INPUT PARAM iMsgAnsw   AS CHAR  NO-UNDO. /* Ответы для сообщения. */

   DEF VAR vAnswChar AS CHAR NO-UNDO. /* Ответ для логирования. */

   DEF BUFFER tt-MsgSet FOR tt-MsgSet. /* Локализация буфера. */

   /* Если сообщения требуют ответа и ответ предустановлен,
   ** то формируем ответ и выходим. Сначала ищем ответы с точным соответствием
   ** кода сообщения в ответе и текущего кода сообщения, если подходящий
   ** ответ не найден, ищем среди общих ответов (код = '*') */
   IF CAN-DO ("3,4", iMsgType) THEN
   FOR EACH tt-MsgSet WHERE
      (tt-MsgSet.Proc-Id    EQ iPId  AND
       tt-MsgSet.Mes-Code   EQ iMsgCode) OR
      (tt-MsgSet.Proc-Id    EQ iPId  AND
       tt-MsgSet.Mes-Code   EQ "*")
      BY tt-MsgSet.Mes-Code DESC BY tt-MsgSet.Mes-Order:
      
      /* Устанавливаем ответ для логирования. */
      IF iMsgAnsw EQ ""
      THEN IF tt-MsgSet.MsgAnsw EQ "YES"
         THEN ASSIGN
            vAnswChar   = "ДА"
            pick-value  = "YES"
         .
         ELSE ASSIGN
            vAnswChar   = IF tt-MsgSet.MsgAnsw EQ "NO" THEN "НЕТ" ELSE "Отказ от выбора"
            pick-value  = IF tt-MsgSet.MsgAnsw EQ "NO" THEN "NO"  ELSE ?
         .
      ELSE ASSIGN
         vAnswChar   =  IF LOOKUP (tt-MsgSet.MsgAnsw, iMsgAnsw) NE 0
                           THEN tt-MsgSet.MsgAnsw
                           ELSE "Отказ от выбора"
         pick-value  = STRING (LOOKUP (tt-MsgSet.MsgAnsw, iMsgAnsw))
      .
      IF vAnswChar NE "Отказ от выбора"
         THEN LEAVE.
   END.
   IF vAnswChar NE ""
   THEN DO:
      /* Сохранение ответа в файл ответа. */
      RUN LogAnswer (vAnswChar).
      RETURN.
   END.
   RETURN "Ответ не найден.".
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Запрашивает пользователя о примении его ответа на последнее
               сообщение ко всем сообщениям такого типа
  Parameters:  iMessType   - тип сообщения, обрабатывает только сообщения с
                             типами 3 и 4.
               iMessCode   - код сообщения
               iMessText   - текст сообщения
               iAnswers    - варианты ответов
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE SetAnswer:
   DEFINE INPUT  PARAMETER iMessType AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iMessCode AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iMessText AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAnswers  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vPickValue    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAnswer       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDispAnswer   AS CHARACTER  NO-UNDO.

   /* Commented BY KSV: Если для данного процесса возможность сохранить ответ на
   ** вопрос запрещена или это сообщение не является вопросом, выходим */
   IF vAutoAnswer <> YES OR NOT CAN-DO("3,4",iMessType)  THEN RETURN.

   /* Commented BY KSV: Если сообщение не имеет кода, либо последним ответом
   ** было нажатие ESC, то выход */
   IF pick-value = ? OR NOT {assigned iMessCode} THEN RETURN.
   /* Commented BY KSV: Получаем текст ответа, для сообщения с типом 4, оно
   ** определяется как ENTRY(INT64(pick-value),iAnswers), для сообщений 3, просто
   ** pick-value */
   IF {assigned iAnswers} THEN
   DO:
      vAnswer = ENTRY(INT64(pick-value),iAnswers) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN.
      vDispAnswer = vAnswer.
   END.
   ELSE
   DO:
      vAnswer = pick-value.
      vDispAnswer = (IF pick-value = "YES" THEN "ДА" ELSE "НЕТ").
   END.

   /* Commented BY KSV: Запоминаем текущий ответ пользователя на сист. сообщение
   ** */
   vPickValue = pick-value.
   /* Commented BY KSV: Спрашиваем пользователя, что делать с его ответом */
   pick-value = "1".
/*
   RUN messmenu.p (9,"[Вопрос]",
                   "Применить ответ [" + vDispAnswer +
                   "] ко всем сообщениям [" + iMessCode + "]" + "~n" +
                   "в текущей транзакции?",
                   "Применить,Не применять,Больше не предлагать").
*/
   CASE pick-value:
      /* Commented BY KSV: Запоминаем ответ пользователя на сист. сообщение */
      WHEN "1" THEN
         RUN SetMsgPrm (iMessCode, "MsgAnsw", vAnswer).
      /* Commented BY KSV: Отключаем запуск этой процедуры */
      WHEN "3" THEN
         vAutoAnswer = NO.
   END CASE.
   /* Commented BY KSV: Восстанавливаем ответ пользователя на сист. сообщение */
   pick-value = vPickValue.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='07/08/2015 17:39:24.839+04:00' */
/* $LINTUSER='ariz' */
/* $LINTMODE='1' */
/* $LINTFILE='tmess.pro' */
/*prosignRiGbMsPvhxKPZyT2483EYA*/