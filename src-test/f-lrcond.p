&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loanr-cond NO-UNDO LIKE loan-cond
       FIELD EndDateBeforeProl AS DATE /* EndDateBeforeProl */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD month-cred AS CHARACTER /* month-cred */
       FIELD month-int AS CHARACTER /* month-int */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD prev-type AS CHARACTER /* prev-type */
       FIELD Prolong AS LOGICAL /* Prolong */
       FIELD annuitplat$ AS DECIMAL /* АннуитПлат */
       FIELD kollw#gtper$ AS INT64 /* КолЛьгтПер */
       FIELD konvertaciwa$ AS LOGICAL /* Конвертация */
       FIELD shemaplat$ AS LOGICAL /* СхемаПлат */
       FIELD nomersogl$ AS CHARACTER /* НомерСогл */
       FIELD tarifplan$ AS CHARACTER /* ТарифПлан */       
       FIELD cond-cr-date AS DATE /* cond-cr-date */
       FIELD end-date AS DATE /* end-date */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loanr-cond" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: CCOND-ED.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created:     
     Modified: 
*/
/*          This file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Commented by KSV: Данный шаблон предназначен для создания экранной формы
** осуществляющей добавление, изменение и просмотр информации об объекте
** метасхемы БИСКВИТА без непосредственного обращения к базе данных.
**
** Шаги по созданию экранной формы:
**    0. Настройте PROPATH AppBuilder на SRC каталог БИСКВИТа. ВАЖНО, чтобы
**       служебные каталоги PROGRESS находились после каталога SRC/TOOLS.
**       Подключитесь к базе данных БИСКВИТа. 
**    1. Выберите пункт меню AppBuilder Tools - Procedure Settings. Далее 
**       нажмите кнопку Temp-Table Definition, в появившемся диалоге нажмите
**       кнопку BISQUIT и выберите класс метасхемы, объект которого будет
**       обрабатываться формой. На основе выбранного класса в форму добавится
**       объявления временных таблиц как для выбранного класса, так и для
**       всех аггрегированных на нем классов.
**    2. Разместите поля временных таблиц во фрейме. Для связи виджета с 
**       полем из временной таблицы в форме свойств поля щелкните по кнопке 
**       Database Field правой копкой мыши и в появившемся меню выберите 
**       пункт Bisquit.
**       Вы  можете создать специальные поля разделители, для этого необходимо 
**       создать FILL-IN c идентификатором SEPARATOR# (где # - любое число от
**       2, первый FILL-IN имеет идентифкатор без номера) и аттрибутом 
**       VIES-AS TEXT. С помощью разделителей вы можете визуально выделять
**       группы полей.
**    3. Объедините поля в списки в зависимости от того в каком из режимов
**       поле должно быть доступно для редактирования. Для добавления поля
**       в список в диалоге его атрибутов нажмите кнопку Advanced и поставьте 
**       галки в полях LIST-1, LIST-2 или LIST-3. Назначение списков:
**       -  LIST-1 - поля доступные для редактирования в режиме добавления 
**                   записи 
**       -  LIST-2 - поля доступные для редактирования в режиме редактирования 
**                   записи 
**       -  LIST-3 - поля доступные для редактирования в режиме просмотра. 
**                   (Обычно это поля, отображаемы в виджете EDITOR для 
**                   запрещения их изменения воспользуйтесь атрибутом READ-ONLY)
**       -  LIST-4 - поля для которых атрибут формат определяется в форме.
**                   Для других он заполняется из метасхемы. 
**    4. Контроль за значением полей должен быть определен на триггере LEAVE 
**       поля, который в случае несоответствия значения требуемому должен 
**       возвращать значение {&RET-ERROR}.
**       Правильная конструкция триггера:

   .......

   IF <ОШИБКА> THEN
   DO:
      MESSAGE '......'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   APPLY "LEAVE" TO FRAME {&MAIN-FRAME}. /* стандартная проверка */
   IF RETURN-VALUE EQ {&RET-ERROR}
      THEN RETURN NO-APPLY.

**    5. Для выбора значения поля из списка должен быть опредлен триггер F1 поля
**       (не путать с триггером на событие HELP в TTY - это разные события)
**    6. Если в форме присутсвуют виджеты не относящиеся к полям временной
**       таблицы, например кнопки, но которые д.б. доступны в режимах 
**       редактирования и добавления поместите их в список LIST-4.
**    7. Более тонкую настройку поведения формы вы можете указать в процедуре
**       LocalEnableDisable, которая будет вызываться, в cлучае если она
**       определена, в конце EnableDisable.
**    8. Используйте процедуру LocalSetObject, которая будет вызываться,
**       в cлучае если она определена, перед записью данных в БД.
**    9. Для передачи специфических параметров процедуре экранной формы
**       воспользуйтесб функциями библиотеки PP-TPARA.P
**   10. Описание переменных для управления экранной формой находится в секции
**       Definitions библиотеки bis-tty.pro 
**   11. Описание TEMP-TABL'ов
*/
{globals.i}
{flt-file.i}
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
CREATE WIDGET-POOL.
&ENDIF
/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE MAIN-FRAME fMain
/* Расскомментировать в случае вызова из NAVIGATE.CQR
{navigate.cqr
   ...
   &UseBisTTY=YES
   &edit=bis-tty.ef
   ...
}
   Если определена &UseBisTTY - то ссылка на динамическую таблицу верхнего класса
будет храниться в переменной IInstance.
   Если определена &InstanceFile - то будет определена и заполнена статическая
TEMP-TABLE tt-instance LIKE {&InstanceFile}

&GLOBAL-DEFINE UseBisTTY 
&GLOBAL-DEFINE InstanceFile ИМЯ_ТАБЛИЦЫ_ПРОГРЕСС_ДЛЯ_ВЕРХНЕГО_КЛАССА
*/

/* Для просмотра полученной mInstance в GetObject */
/* &GLOBAL-DEFINE DEBUG-INSTANCE-GET */

/* Для просмотра mInstance перед записью в базу в SetObject */
/* &GLOBAL-DEFINE DEBUG-INSTANCE-SET */

/* Безусловное включение\отключение вызова xattr-ed 
(иначе он вызывается при наличие незаполненных обязательных реквизитов */
/*
&GLOBAL-DEFINE XATTR-ED-OFF
&GLOBAL-DEFINE XATTR-ED-ON 
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loanr-cond

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loanr-cond
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loanr-cond
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loanr-cond SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loanr-cond SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loanr-cond
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loanr-cond


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loanr-cond.since  ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-TABLES tt-loanr-cond
&Scoped-define FIRST-ENABLED-TABLE tt-loanr-cond
&Scoped-Define ENABLED-OBJECTS  
&Scoped-Define DISPLAYED-FIELDS tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define DISPLAYED-TABLES tt-loanr-cond
&Scoped-define FIRST-DISPLAYED-TABLE tt-loanr-cond
&Scoped-Define DISPLAYED-OBJECTS  

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define List-2 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define List-3 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date ~

/*&Scoped-define List-4 tt-loanr-cond.class-code  */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loanr-cond SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loanr-cond.since
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 18 COLON-ALIGNED
          &ELSE AT ROW 1 COL 18 COLON-ALIGNED &ENDIF HELP
          "Начало действия условий договора"
          LABEL "Начинается" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF 
     tt-loanr-cond.nomersogl$ 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 18 COLON-ALIGNED
          &ELSE AT ROW 2 COL 18 COLON-ALIGNED &ENDIF HELP
          "Доп. соглашение"
          LABEL "Доп. соглашение"  FORMAT "X(30)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loanr-cond.tarifplan$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18 COLON-ALIGNED
          &ELSE AT ROW 3 COL 18 COLON-ALIGNED &ENDIF HELP
          "Код тарифного плана"
          LABEL "Код тариф. плана" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
          &ELSE SIZE 30 BY 1 &ENDIF             
     tt-loanr-cond.int-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 18 COLON-ALIGNED
          &ELSE AT ROW 4 COL 18 COLON-ALIGNED &ENDIF HELP
          "Срок"
          LABEL "Срок" FORMAT "9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF             
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 3
         SIZE 62 BY 6
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loanr-cond T "?" NO-UNDO bisquit loan-cond
      ADDITIONAL-FIELDS:
          FIELD EndDateBeforeProl AS DATE /* EndDateBeforeProl */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD month-cred AS CHARACTER /* month-cred */
          FIELD month-int AS CHARACTER /* month-int */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD prev-type AS CHARACTER /* prev-type */
          FIELD Prolong AS LOGICAL /* Prolong */
          FIELD commission AS CHARACTER /* commission */
          FIELD commi_cur AS CHARACTER /* commi_cur */
          FIELD cond-sch AS CHARACTER /* cond-sch */
          FIELD interest AS CHARACTER /* interest */
          FIELD annuitplat$ AS DECIMAL /* АннуитПлат */
          FIELD kollw#gtper$ AS INT64 /* КолЛьгтПер */
          FIELD konvertaciwa$ AS LOGICAL /* Конвертация */
          FIELD shemaplat$ AS LOGICAL /* СхемаПлат */
          FIELD cond-cr-date AS DATE /* cond-cr-date */
          FIELD end-date AS DATE /* end-date */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loanr-cond" "" }
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 21
         WIDTH              = 80.43
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21
         VIRTUAL-WIDTH      = 80.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB TERMINAL-SIMULATION 
/* ************************* Included-Libraries *********************** */

{bis-tty.pro}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN tt-loanr-cond.class-code IN FRAME fMain
   3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-loanr-cond.cond-cr-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-loanr-cond.since IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loanr-cond"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF FRAME fMain ANYWHERE DO:
   mRetVal = IF mOnlyForm THEN
      {&RET-ERROR}
      ELSE 
         "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.
&ENDIF
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").

   /* Commented by KSV: Корректируем вертикальную позицию фрейма */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.

   /* Commented by KSV: Читаем данные */
   RUN GetObject.

   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Подсветка полей из LIST-5 (настроить для себя )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

   /* Commented by KSV: Показываем экранную форму */
   STATUS DEFAULT "".
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
   RUN enable_UI.
&ENDIF
   /* Commented by KSV: Открываем те поля, которые разрешено изменять
   ** в зависимости от режима открытия */
   RUN EnableDisable.
&IF DEFINED(SESSION-REMOTE) &THEN
   LEAVE MAIN-BLOCK.
&ENDIF

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END.

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

&IF DEFINED(SESSION-REMOTE) = 0 &THEN
RUN disable_ui.
&ENDIF

RUN EndBisTTY.

/* Commented by KSV: Выгружаем библиотеки */
{intrface.del}

/* Commented by KSV: Возвращаем значение вызывающей процедуре */
RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI TERMINAL-SIMULATION  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI TERMINAL-SIMULATION  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  IF AVAILABLE tt-loanr-cond THEN 
    DISPLAY tt-loanr-cond.since  tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date  
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION 
PROCEDURE LocalEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR lLoanCode AS CHAR NO-UNDO.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-cond FOR loan-cond.
   DEFINE VARIABLE vContract AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vContCode AS CHARACTER   NO-UNDO.

   IF iMode EQ {&MOD_ADD} THEN
   DO:
       ASSIGN
          vContract = GetFltVal("contract")
          vContCode = GetFltVal("cont-code")
          tt-loanr-cond.contract  = vContract
          tt-loanr-cond.cont-code = vContCode 
       . 
   
   END.
   
   FIND FIRST b-loan WHERE b-loan.contract  = tt-loanr-cond.contract
                       AND b-loan.cont-code = tt-loanr-cond.cont-code
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN RETURN. 
   
   IF iMode = {&MOD_ADD} THEN DO:
     FIND FIRST b-loan-cond WHERE b-loan-cond.contract  = b-loan.contract
                              AND b-loan-cond.cont-code = b-loan.cont-code
                            NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b-loan-cond THEN tt-loanr-cond.since = b-loan.open-date.
     ELSE                              tt-loanr-cond.since = gend-date. 
     DISPLAY tt-loanr-cond.since WITH FRAME fMain.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


