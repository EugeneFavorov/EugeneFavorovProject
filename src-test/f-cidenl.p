&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-ident NO-UNDO LIKE cust-ident
       FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
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
     Filename: F-CIDENL.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 14.04.2006 17:02 ILVI    
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
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
CREATE WIDGET-POOL.
&ENDIF
/* ***************************  Definitions  ************************** */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */

DEFINE VARIABLE mCustCat  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustId   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustCodeTypeDomain AS CHARACTER NO-UNDO.

DEF BUFFER bCustIdent FOR cust-ident. /* Локализация буфера. */

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
&Scoped-define INTERNAL-TABLES tt-cust-ident

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date ~
tt-cust-ident.licenzorg$ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date ~
tt-cust-ident.licenzorg$ 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-ident
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-ident SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-ident SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-ident


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.issue ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.licenzorg$ 
&Scoped-define ENABLED-TABLES tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE tt-cust-ident
&Scoped-Define ENABLED-OBJECTS fCustname fCodeName 
&Scoped-Define DISPLAYED-FIELDS tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date ~
tt-cust-ident.licenzorg$ 
&Scoped-define DISPLAYED-TABLES tt-cust-ident
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-ident
&Scoped-Define DISPLAYED-OBJECTS fCustname fCodeName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.issue ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.licenzorg$ 
&Scoped-define List-2 tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.issue ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.licenzorg$ 
&Scoped-define List-3 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type fCodeName tt-cust-ident.cust-code ~
tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date ~
tt-cust-ident.licenzorg$ 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fCodeName AS CHARACTER 
     VIEW-AS EDITOR
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 56 BY 5
     &ELSE SIZE 56 BY 5 &ENDIF NO-UNDO.

DEFINE VARIABLE fCustname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
     &ELSE SIZE 28 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-ident SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-cust-ident.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 17 COLON-ALIGNED
          &ELSE AT ROW 1 COL 17 COLON-ALIGNED &ENDIF HELP
          "Частное (Ч), юридическое (Ю) лицо"
          LABEL "Тип клиента" FORMAT "!"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-cust-ident.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 31 COLON-ALIGNED
          &ELSE AT ROW 1 COL 31 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Клиент" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCustname
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 42 COLON-ALIGNED
          &ELSE AT ROW 1 COL 42 COLON-ALIGNED &ENDIF NO-LABEL
     tt-cust-ident.cust-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 17 COLON-ALIGNED
          &ELSE AT ROW 2.01 COL 17 COLON-ALIGNED &ENDIF
          LABEL "             Тип"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     fCodeName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18
          &ELSE AT ROW 3 COL 18 &ENDIF NO-LABEL
     tt-cust-ident.cust-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 17 COLON-ALIGNED
          &ELSE AT ROW 8 COL 17 COLON-ALIGNED &ENDIF
          LABEL "            Код"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 52 BY 1
          &ELSE SIZE 52 BY 1 &ENDIF
     tt-cust-ident.issue
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 17 COLON-ALIGNED
          &ELSE AT ROW 9 COL 17 COLON-ALIGNED &ENDIF
          LABEL " Выдан"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 40 BY 1
          &ELSE SIZE 40 BY 1 &ENDIF
     tt-cust-ident.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 17 COLON-ALIGNED
          &ELSE AT ROW 10 COL 17 COLON-ALIGNED &ENDIF
          LABEL "       Дата c"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     tt-cust-ident.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 17 COLON-ALIGNED
          &ELSE AT ROW 11 COL 17 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     tt-cust-ident.licenzorg$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 18
          &ELSE AT ROW 12 COL 18 &ENDIF HELP
          "Дополнительные сведения о лицензии" NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 1000
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 56 BY 6
          &ELSE SIZE 56 BY 6 &ENDIF TOOLTIP "Дополнения"
     "Дополнения:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 7
          &ELSE AT ROW 12 COL 7 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-cust-ident T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
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
         HEIGHT             = 20.99
         WIDTH              = 80.38
         MAX-HEIGHT         = 20.99
         MAX-WIDTH          = 80.38
         VIRTUAL-HEIGHT     = 20.99
         VIRTUAL-WIDTH      = 80.38
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
/* SETTINGS FOR FILL-IN tt-cust-ident.close-date IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR COMBO-BOX tt-cust-ident.cust-cat IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code-type IN FRAME fMain
   1 3 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-id IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR EDITOR fCodeName IN FRAME fMain
   3                                                                    */
ASSIGN 
       fCodeName:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fCustname:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-cust-ident.issue IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR EDITOR tt-cust-ident.licenzorg$ IN FRAME fMain
   1 2 3 EXP-HELP                                                       */
/* SETTINGS FOR FILL-IN tt-cust-ident.open-date IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
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

&Scoped-define SELF-NAME tt-cust-ident.cust-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code IN FRAME fMain /*             Код */
DO:
   {&BEG_BT_LEAVE}
   IF     TRIM(SELF:SCREEN-VALUE) = ""
      AND NOT(tt-cust-ident.cust-code-type:SCREEN-VALUE BEGINS '0') 
   THEN DO:
      RUN Fill-SysMes("", "","-1","Номер лицензии не указан!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-ident.cust-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON ENTRY OF tt-cust-ident.cust-code-type IN FRAME fMain /*              Тип */
,tt-cust-ident.cust-code, 
 tt-cust-ident.open-date, 
 tt-cust-ident.issue, 
 tt-cust-ident.open-date,
 tt-cust-ident.close-date,
 tt-cust-ident.licenzorg$
DO:
   tt-cust-ident.cust-code-type:SCREEN-VALUE = TRIM(tt-cust-ident.cust-code-type:SCREEN-VALUE,"*").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.cust-code-type IN FRAME fMain /*              Тип */
DO:
   IF iMode EQ {&MOD_VIEW} THEN
      RUN shifr.p((IF tt-cust-ident.cust-cat:SCREEN-VALUE = "Б"
                  THEN "ВидБанкЛиц" 
                  ELSE "ВидЛицДеят"),
                  SELF:SCREEN-VALUE,
                  iLevel + 1).
   ELSE DO TRANS:
      RUN codelay.p ((IF tt-cust-ident.cust-cat:SCREEN-VALUE EQ "Б" 
                      THEN "ВидБанкЛиц" 
                      ELSE "ВидЛицДеят"),
                     (IF tt-cust-ident.cust-cat:SCREEN-VALUE EQ "Б" 
                      THEN "ВидБанкЛиц" 
                      ELSE "ВидЛицДеят"),
                     "Виды лицензируемой деятельности", 
                     iLevel + 1).
      IF     LASTKEY EQ 10          
         AND pick-value NE ? THEN DO:
         DISPLAY
            pick-value @ tt-cust-ident.cust-code-type
         WITH FRAME {&FRAME-NAME}.
         RUN Local_FillLinkField_cust-code-type.     
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code-type IN FRAME fMain /*              Тип */
DO:
   {&BEG_BT_LEAVE}
   IF GetCode((IF tt-cust-ident.cust-cat:SCREEN-VALUE = "Б"
               THEN "ВидБанкЛиц" 
               ELSE "ВидЛицДеят"),
               TRIM(SELF:SCREEN-VALUE,"*")) EQ ? 
   THEN DO:
      MESSAGE "Указанный вид лицензии отсутствует в справочнике!"
         VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   RUN Local_FillLinkField_cust-code-type.
   IF iMode NE {&MOD_VIEW} THEN 
      SELF:SCREEN-VALUE = TRIM(SELF:SCREEN-VALUE,"*") + "*".
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
   
   ASSIGN
      mCustCat = ENTRY(2, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 1
      mCustId  = ENTRY(3, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 2
   .
   
   IF mCustCat NE "*" THEN 
      tt-cust-ident.cust-cat = mCustCat.
   
   IF       mCustId        NE "*"
      AND   TRIM (mCustId) GT "" THEN
      tt-cust-ident.cust-id = INT64(mCustId) NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispCodeName TERMINAL-SIMULATION 
PROCEDURE DispCodeName :
/*------------------------------------------------------------------------------
  Purpose:     Отображает наименование Классификатора
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain :
   fCodeName:SCREEN-VALUE = GetCodeName(IF tt-cust-ident.cust-cat = "Б"
                                        THEN "ВидБанкЛиц" 
                                        ELSE "ВидЛицДеят",
                                        TRIM(tt-cust-ident.cust-code-type:SCREEN-VALUE,"*")
                            ).   
    /* 0160865 возможность выбора из ВидЛицДеят для банков */
    /* --------------------------------------------------- */
   IF fCodeName:SCREEN-VALUE EQ "?" OR fCodeName:SCREEN-VALUE EQ "" THEN
      fCodeName:SCREEN-VALUE = GetCodeName("ВидЛицДеят",
                                           TRIM(tt-cust-ident.cust-code-type:SCREEN-VALUE,"*")
                               ).   

   IF fCodeName:SCREEN-VALUE EQ "?" THEN
      fCodeName:SCREEN-VALUE = "".
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
  DISPLAY fCustname fCodeName 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-ident THEN 
    DISPLAY tt-cust-ident.cust-cat tt-cust-ident.cust-id 
          tt-cust-ident.cust-code-type tt-cust-ident.cust-code 
          tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date 
          tt-cust-ident.licenzorg$ 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-cust-ident.cust-cat tt-cust-ident.cust-id fCustname 
         tt-cust-ident.cust-code-type fCodeName tt-cust-ident.cust-code 
         tt-cust-ident.issue tt-cust-ident.open-date tt-cust-ident.close-date 
         tt-cust-ident.licenzorg$ 
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
    
   mCustCodeTypeDomain = GetXAttrEx(iClass,
                                    "cust-code-type",
                                    "Domain-Code"
                         ).
   
   DISABLE 
      tt-cust-ident.cust-cat WHEN mCustCat NE "*"
      tt-cust-ident.cust-id  WHEN mCustId  NE "*"
   WITH FRAME {&FRAME-NAME}.

   RUN DispCodeName.
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
   tt-cust-ident.cust-code-type = TRIM(tt-cust-ident.cust-code-type,"*").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_FillLinkField_cust-code-type TERMINAL-SIMULATION 
PROCEDURE Local_FillLinkField_cust-code-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF LAST-EVENT:FUNCTION NE "GO" THEN 
   DO WITH FRAME {&FRAME-NAME}:
      tt-cust-ident.cust-code-type:SCREEN-VALUE = TRIM(tt-cust-ident.cust-code-type:SCREEN-VALUE,"*").
   END.
   RUN DispCodeName.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_GO TERMINAL-SIMULATION 
PROCEDURE Local_GO :
   DO WITH FRAME fMain:   
      IF     iMode EQ {&MOD_EDIT} 
         AND tt-cust-ident.cust-code NE tt-cust-ident.cust-code:SCREEN-VALUE 
      THEN
         tt-cust-ident.cust-type-num = ?.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

