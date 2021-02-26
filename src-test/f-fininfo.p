&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-ident NO-UNDO LIKE cust-ident
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD country-id AS CHARACTER /* country-id */
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-cust-ident" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-FININFO.P
      Comment: (0202227) Экранная форма для ввода
               финансовой информации по юр. лицу.
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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE mCustCat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mclass-code         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mStr                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mStrTMP             AS CHARACTER NO-UNDO. 

&GLOBAL-DEFINE MAIN-FRAME fMain

&GLOBAL-DEFINE table      tt-cust-ident

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

{intrface.get cust}     /* Библиотека для работы с клиентами. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cust-ident

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-ident
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-ident SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-ident SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-ident


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-TABLES tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE tt-cust-ident
&Scoped-Define ENABLED-OBJECTS vsource vdoc-num vdoc-date vinfodesc ~
fCustName separator 
&Scoped-Define DISPLAYED-FIELDS ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define DISPLAYED-TABLES tt-cust-ident
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-ident
&Scoped-Define DISPLAYED-OBJECTS vsource vdoc-num vdoc-date vinfodesc ~
fCustName separator 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 
&Scoped-define List-2 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 
&Scoped-define List-3 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE vinfodesc AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 5000 SCROLLBAR-VERTICAL
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 57 BY 4
     &ELSE SIZE 57 BY 4 &ENDIF NO-UNDO.

DEFINE VARIABLE fCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 1
     &ELSE SIZE 39 BY 1 &ENDIF.

DEFINE VARIABLE separator AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vdoc-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Дата документа" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vdoc-num AS CHARACTER FORMAT "X(100)":U 
     LABEL "Номер документа" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 61 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vsource AS CHARACTER FORMAT "x(1000)" 
     LABEL "Источник" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 61 BY 1 &ENDIF.
     
DEFINE VARIABLE vSourceDetails AS CHARACTER FORMAT "X(500)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 70 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-ident SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     vsource
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 8
          &ELSE AT ROW 2.99 COL 8 &ENDIF HELP
          "Источник"          
     vSourceDetails
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF NO-LABEL
     vdoc-num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 16 COLON-ALIGNED
          &ELSE AT ROW 4 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер документа"
     vdoc-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 16 COLON-ALIGNED
          &ELSE AT ROW 5 COL 16 COLON-ALIGNED &ENDIF HELP
          "Дата документа"
     vinfodesc
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 21
          &ELSE AT ROW 7 COL 21 &ENDIF HELP
          "Описание информации" NO-LABEL
     tt-cust-ident.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "Дата получения информации"
          LABEL "Дата получения" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 45 COLON-ALIGNED
          &ELSE AT ROW 11 COL 45 COLON-ALIGNED &ENDIF HELP
          "Дата окончания действия"
          LABEL "Дата окончания" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCustName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 38 COLON-ALIGNED
          &ELSE AT ROW 1 COL 38 COLON-ALIGNED &ENDIF NO-LABEL
     tt-cust-ident.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 21
          &ELSE AT ROW 1 COL 21 &ENDIF HELP
          "Порядковый номер клиента"
          LABEL "Клиент" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 14 COLON-ALIGNED
          &ELSE AT ROW 1 COL 14 COLON-ALIGNED &ENDIF HELP
          "" FORMAT "!"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     separator
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 1
          &ELSE AT ROW 2 COL 1 &ENDIF NO-LABEL
     "Описание информации:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 14
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-cust-ident T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD country-id AS CHARACTER /* country-id */
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-cust-ident" "" }
          
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
         HEIGHT             = 26.64
         WIDTH              = 84.71
         MAX-HEIGHT         = 26.64
         MAX-WIDTH          = 84.71
         VIRTUAL-HEIGHT     = 26.64
         VIRTUAL-WIDTH      = 84.71
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.close-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-cat IN FRAME fMain
   EXP-FORMAT EXP-HELP                                                  */
ASSIGN 
       tt-cust-ident.cust-cat:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code-type IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-id IN FRAME fMain
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
ASSIGN 
       tt-cust-ident.cust-id:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fCustName:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-cust-ident.open-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN separator IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN vdoc-date IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN vdoc-num IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR EDITOR vinfodesc IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN vsource IN FRAME fMain
   ALIGN-L 1 2 3                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-cust-ident"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
/*&Scoped-define SELF-NAME tt-cust-ident.cust-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* Вид информации */
DO:
  IF mclass-code EQ "ФинПолож" THEN
  DO TRANS:
         RUN browseld.p ("code",  /* Класс объекта. */
                "class" + CHR(1) + "parent" + CHR(1) + "title",

                /* Поля для предустановки. */
                "ФинПолож" + CHR(1) + "ФинПолож" + CHR(1) + "Финансовое положение клиента",

                /* Список значений полей. */
                 ?,          /* Поля для блокировки. */
                iLevel).      /* Строка отображения фрейма. */
  END.
  ELSE
  DO TRANS:
       RUN browseld.p ("code",  /* Класс объекта. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* Поля для предустановки. */
              "ДелРепутац" + CHR(1) + "ДелРепутац" + CHR(1) + "Деловая репутация клиента",

              /* Список значений полей. */
               ?,          /* Поля для блокировки. */
              iLevel).      /* Строка отображения фрейма. */
   END.

   IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
END.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code-type IN FRAME fMain /* Вид информации */
DO:
  {&BEG_BT_LEAVE}
  IF mclass-code EQ "ФинПолож" THEN 
     FIND FIRST CODE WHERE
         CODE.class  = "ФинПолож" AND
         CODE.parent = "ФинПолож" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST CODE WHERE
         CODE.class  = "ДелРепутац" AND
         CODE.parent = "ДелРепутац" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL CODE THEN DO:
     RUN Fill-SysMes IN h_tmess ("", "", "-1", "Значения реквизита " + SELF:SCREEN-VALUE + " нет в классификаторе").
     RETURN NO-APPLY {&RET-ERROR}.
  END.
  {&END_BT_LEAVE}
END.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME vsource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsource TERMINAL-SIMULATION
ON F1 OF vsource IN FRAME fMain /* Источник */
DO:   
  CASE mclass-code:  
  WHEN "ДелРепут" THEN
  DO TRANS:
       RUN browseld.p ("code",  /* Класс объекта. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* Поля для предустановки. */
              "ИстДелРеп" + CHR(1) + "ИстДелРеп" + CHR(1) + "Источники деловой репутации клиента",

              /* Список значений полей. */
               ?,          /* Поля для блокировки. */
              iLevel).      /* Строка отображения фрейма. */

   IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
    
    
      IF vsource:SCREEN-VALUE EQ "Иное" THEN
            DO:
                MESSAGE "Для вввода данных нажмите F2"  VIEW-AS ALERT-BOX .    
                mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR.
                vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("ИстДелРеп",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.
            END. 
            ELSE
                vSourceDetails:SCREEN-VALUE =  GetCodeDesc("ИстДелРеп",SELF:SCREEN-VALUE,1,"") .
    
   END.
    WHEN "ФинПолож" THEN
  DO TRANS:
      
     RUN browseld.p ("code",  /* Класс объекта. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* Поля для предустановки. */
              "ДокФинПолож" + CHR(1) + "ДокФинПолож" + CHR(1) + "Сведения (документы) о финансовом положении",

              /* Список значений полей. */
               ?,          /* Поля для блокировки. */
              iLevel).      /* Строка отображения фрейма. */
   
   
    IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
   END.
   END CASE.
   
END.


ON F2 OF vsource IN FRAME fMain /* Код цели */
    DO:    
        If vsource:SCREEN-VALUE EQ "Иное" THEN Do: RUN  OpenWin. 
        mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR. 
        vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("ИстДелРеп",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.   
END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsource TERMINAL-SIMULATION
ON LEAVE OF vsource IN FRAME fMain /* Источник */
DO:
      
  {&BEG_BT_LEAVE}
  CASE mclass-code: 
      WHEN "ДелРепут" THEN
  DO TRANS:
      FIND FIRST CODE WHERE
         CODE.class  = "ИстДелРеп" AND
         CODE.parent = "ИстДелРеп" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL CODE THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "Значения реквизита " + SELF:SCREEN-VALUE + " нет в классификаторе").
         RETURN NO-APPLY {&RET-ERROR}.
     END.
     else
     do:
   IF vsource EQ "Иное" THEN
   DO:
       mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR. 
       vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("ИстДелРеп",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.
   END. 
    /*ELSE
        vVidContDetails:SCREEN-VALUE =  GetCodeDesc("ВидПланДок",SELF:SCREEN-VALUE,1,"") "123". */
     
      end.
  END.
  
    WHEN "ФинПолож" THEN
  DO TRANS:
        /*   FIND FIRST CODE WHERE
         CODE.class  = "ДокФинПолож" AND
         CODE.parent = "ДокФинПолож" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL CODE THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "Значения реквизита " + SELF:SCREEN-VALUE + " нет в классификаторе").
         RETURN NO-APPLY {&RET-ERROR}.
     END. */     
      end.
  END CASE.
   ASSIGN
     vsource     = vsource:SCREEN-VALUE
     .  
  {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */

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

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").

   /* Commented by KSV: Корректируем вертикальную позицию фрейма */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.
   
   ASSIGN
         mCustCat  = ENTRY(2, iInstanceList,CHR(3))
         mCustId   = ENTRY(3, iInstanceList,CHR(3))
         mclass-code = ENTRY(4, iInstanceList,CHR(3))
         mSymbol   = ENTRY(5, iInstanceList,CHR(3))
   .
   /* Commented by KSV: Читаем данные */
   RUN GetObject.
   
   IF     mCustCat NE "*"
      AND TRIM(mCustCat) GT "" 
   THEN 
      tt-cust-ident.cust-cat = mCustCat.
  
   IF     mCustId NE "*" 
      AND TRIM(mCustId) GT "" 
   THEN
      tt-cust-ident.cust-id = INT64(mCustId) NO-ERROR.

   RUN DispCustName.
   
   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Commented by KSV: Показываем экранную форму */
   STATUS DEFAULT "".
   RUN enable_UI.

   /* Commented by KSV: Открываем те поля, которые разрешено изменять
   ** в зависимости от режима открытия */
   RUN EnableDisable.

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
  DO:
   WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
  END.
END.

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

RUN disable_ui.

/* Commented by KSV: Удаляем экземпляр объекта */
IF VALID-HANDLE(mInstance) AND NOT mOnlyForm THEN 
   RUN DelEmptyInstance(mInstance).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispCustName TERMINAL-SIMULATION 
PROCEDURE DispCustName :
/*------------------------------------------------------------------------------
  Purpose:     Отображает наименование клиента
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain :
  DEFINE VARIABLE vTmp        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vCustName   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vCustName2  AS CHARACTER  NO-UNDO.
  /* ASSIGN FRAME fMain tt-cust-ident.cust-cat tt-cust-ident.cust-id. */
  fCustName:SCREEN-VALUE = "".
  RUN GetCustName IN h_base ( tt-cust-ident.cust-cat,
                              tt-cust-ident.cust-id,
                             ?,
                             OUTPUT vCustName,OUTPUT vCustName2,INPUT-OUTPUT vTmp).
  fCustName = trim(vCustName) + " " + vCustName2.
END.   
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
mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR.
If vsource EQ "Иное" THEN   vSourceDetails = "(F2) " + vsource + ": " + mStrTMP. 
else vSourceDetails = "".
IF iMode EQ {&MOD_ADD} THEN
    tt-cust-ident.open-date = today.

  DISPLAY vsource vSourceDetails vdoc-num vdoc-date vinfodesc fCustName separator 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-ident THEN 
    DISPLAY tt-cust-ident.open-date 
          tt-cust-ident.close-date tt-cust-ident.cust-id tt-cust-ident.cust-cat 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE vsource vdoc-num vdoc-date vinfodesc 
         tt-cust-ident.open-date tt-cust-ident.close-date fCustName 
         tt-cust-ident.cust-id tt-cust-ident.cust-cat separator 
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
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local_GO TERMINAL-SIMULATION 
PROCEDURE local_GO :
/*------------------------------------------------------------------------------
  Purpose:      
  Parameters:  <none> 
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME fMain:
      ASSIGN
         vdoc-num    = vdoc-num:SCREEN-VALUE
         vdoc-date   = date(vdoc-date:SCREEN-VALUE) 
         vinfodesc   = vinfodesc:SCREEN-VALUE
         vsource     = vsource:SCREEN-VALUE
      NO-ERROR.
      ASSIGN
         tt-cust-ident.cust-code = vdoc-num + mSymbol + (IF vdoc-date EQ ? THEN "" ELSE string(vdoc-date)) + mSymbol + STRING(TODAY) + STRING(TIME)
         tt-cust-ident.issue     = vsource + mSymbol + vinfodesc                    
         .
         CASE mclass-code:  
              WHEN "ДелРепут" THEN tt-cust-ident.cust-code-type = "FinInfoRep". 
              WHEN "ФинПолож" THEN tt-cust-ident.cust-code-type = "FinInfoPos". 
            END CASE. 
       END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none> 
  Notes:       
------------------------------------------------------------------------------*/
IF iMode EQ {&MOD_ADD} THEN
   tt-cust-ident.open-date = TODAY .  
ELSE DO:
   ASSIGN 
      vdoc-num = ENTRY(1,tt-cust-ident.cust-code,mSymbol)
      vdoc-date = date(ENTRY(2,tt-cust-ident.cust-code,mSymbol))
      vinfodesc = ENTRY(2,tt-cust-ident.issue,mSymbol)
      vsource   = ENTRY(1,tt-cust-ident.issue,mSymbol)
      .
END.
END PROCEDURE.

PROCEDURE OpenWin :   
DEFINE VARIABLE mTmpStr                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mTmpStr2                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mTmpStr3                AS CHARACTER NO-UNDO.                
PAUSE 0.

FORM                                            
     mStr VIEW-AS FILL-IN SIZE 40 BY 1 AT ROW 1 COL 1 LABEL  "Иное" FORMAT "x(255)"

    WITH FRAME fM CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE "[ ВВЕДИТЕ ЗНАЧЕНИЕ ]" .

UPDATE    
    mStr
    WITH FRAME fM. 
    mTmpStr3 = GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other","").  
    IF mTmpStr3 NE "" THEN
    DO:
        mTmpStr = entry(4,mTmpStr3,";") NO-ERROR.      
        if  mTmpStr = "" THEN 
            mTmpStr2 = entry(1,mTmpStr3,";") + ";" + entry(2,mTmpStr3,";") + ";" + entry(3,mTmpStr3,";")  + mStr + ";" + ";" + entry(5,mTmpStr3,";") + ";" + entry(6,mTmpStr3,";") + ";".
        else                                                           
            mTmpStr2 =  REPLACE(mTmpStr3,mTmpStr,mStr).     
    END.
    ELSE     
        mTmpStr2 = ";;;" + mStr + ";;;".       
    UpdateSignsEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",mTmpStr2).
    HIDE FRAME fM.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='10/10/2014 12:36:29.770+04:00' */
/* $LINTFILE='f-fininfo.p' */
/*prosign13ixAudWITtOWFZuZdYfzg*/