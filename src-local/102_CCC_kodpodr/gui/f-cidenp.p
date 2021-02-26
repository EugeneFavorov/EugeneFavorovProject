&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-ident NO-UNDO LIKE cust-ident
       FIELD ExpBKI AS LOGICAL /* ExpBKI */
       FIELD podrazd$ AS CHARACTER /* Подразд */
       FIELD end-date AS DATE /* end-date */
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
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: f-cidenp.p
      Comment: (0019947) Экранная форма для ввода
               идентификатора субъекта.
   Parameters: нет
         Uses:
      Used by:
      Created: 18.03.2004 19:55 KSV     
     Modified: 22.03.2004 15:11 KSV      (0019947) Экранная форма для ввода
                                         идентификатора субъекта.
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
{intrface.get cust}
{intrface.get date}
{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}
{chknumberpfr.pro}
/* Вставка Плюс банк */
DEFINE VARIABLE mDbgPrint AS LOGICAL NO-UNDO.
mDbgPrint = NO.
{chk-black-list.pro}
/* Конец вставки Плюс банк */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE mCodeType AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustCat  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustId   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustCodeTypeDomain AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountry  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBirthday  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vCountryId AS CHARACTER NO-UNDO.
DEFINE VARIABLE mEgrInfo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPredpr    AS LOGICAL   NO-UNDO.
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
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-ident
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-ident SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-ident SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-ident


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue 
&Scoped-define ENABLED-TABLES tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE tt-cust-ident
&Scoped-Define ENABLED-OBJECTS fCustname fCodeName separator separator-2 
&Scoped-Define DISPLAYED-FIELDS tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define DISPLAYED-TABLES tt-cust-ident
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-ident
&Scoped-Define DISPLAYED-OBJECTS fCustname fCodeName separator separator-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-2 tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-3 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-4 tt-cust-ident.podrazd$ 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fCodeName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Наименование" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 56 BY 1
     &ELSE SIZE 56 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCustname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
     &ELSE SIZE 28 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator-2 AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-ident SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-cust-ident.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 20 COLON-ALIGNED
          &ELSE AT ROW 1 COL 20 COLON-ALIGNED &ENDIF HELP
          "Частное (Ч), юридическое (Ю) лицо"
          LABEL "Тип клиента" FORMAT "!"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-cust-ident.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 35 COLON-ALIGNED
          &ELSE AT ROW 1 COL 35 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Клиент" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCustname
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 48 COLON-ALIGNED
          &ELSE AT ROW 1 COL 48 COLON-ALIGNED &ENDIF NO-LABEL
     tt-cust-ident.cust-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 20 COLON-ALIGNED
          &ELSE AT ROW 2 COL 20 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Тип идентификации" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-cust-ident.cust-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 48 COLON-ALIGNED
          &ELSE AT ROW 2 COL 48 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Идентификатор" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
          &ELSE SIZE 28 BY 1 &ENDIF
     fCodeName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 20 COLON-ALIGNED
          &ELSE AT ROW 3 COL 20 COLON-ALIGNED &ENDIF
     tt-cust-ident.podrazd$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 20 COLON-ALIGNED
          &ELSE AT ROW 5 COL 20 COLON-ALIGNED &ENDIF HELP
          "Код подразделения"
          LABEL "Код подразделения" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 52 COLON-ALIGNED
          &ELSE AT ROW 5 COL 52 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Дата выдачи" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 52 COLON-ALIGNED
          &ELSE AT ROW 6 COL 52 COLON-ALIGNED &ENDIF HELP
          "Дата окончания"
          LABEL "Дата окончания" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.issue
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 8
          &ELSE AT ROW 8 COL 8 &ENDIF NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 70 BY 4
          &ELSE SIZE 70 BY 4 &ENDIF
     separator
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 1
          &ELSE AT ROW 4 COL 1 &ENDIF NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     separator-2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     "Выдан:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 2
          &ELSE AT ROW 9 COL 2 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-cust-ident T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD ExpBKI AS LOGICAL /* ExpBKI */
          FIELD podrazd$ AS CHARACTER /* Подразд */
          FIELD end-date AS DATE /* end-date */
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
/* SETTINGS FOR COMBO-BOX tt-cust-ident.cust-cat IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code-type IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-id IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.end-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
ASSIGN 
       fCodeName:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fCustname:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR EDITOR tt-cust-ident.issue IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-ident.open-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.podrazd$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN separator IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator-2 IN FRAME fMain
   ALIGN-L                                                              */
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
ON LEAVE OF tt-cust-ident.cust-code IN FRAME fMain /* Идентификатор */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE vRegExpr           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrMes            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackListNameChar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackListCodeChar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackWhyCodeChar  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackWhyNameChar  AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
   DEFINE VARIABLE vAnswer            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMess              AS CHARACTER NO-UNDO.
/* Конец вставки Плюс банк */
   DEFINE VARIABLE choose-button      AS LOGICAL   NO-UNDO.

   /* проверка номера документа по рег.выражению, заданном в справочнике КодДокум */
   vRegExpr = GetCodeMisc("КодДокум", tt-cust-ident.cust-code-type:SCREEN-VALUE, 3).
   IF NOT DYNAMIC-FUNCTION("ereg":U, vRegExpr, SELF:SCREEN-VALUE, OUTPUT vResult, INPUT-OUTPUT vErrMes) THEN DO:
      RUN Fill-SysMes("base", "", -1, "Номер документа не соответствует правилу валидации, указанном в классификаторе КодДокум"). 
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
/* Вставка Плюс банк */
   IF tt-cust-ident.cust-code-type:SCREEN-VALUE NE "Паспорт" THEN
   DO:
/* Конец вставки Плюс банк */
   ASSIGN
      vBlackListCodeChar = SELF:SCREEN-VALUE
      vBlackListNameChar = GetCodeName("black-list",vBlackListCodeChar)
   .
   IF     (vBlackListNameChar <> ?         
      AND  FGetSetting("black-list", "КонтрТипДок", "") =  "*"
          )
       OR 
          (vBlackListNameChar =  "Паспорт" 
      AND  FGetSetting("black-list", "КонтрТипДок", "") =  "Паспорт"
          ) THEN
   DO:
      ASSIGN
         vBlackWhyCodeChar  = GetCode("black-list",vBlackListCodeChar)
         vBlackWhyCodeChar  = IF vBlackWhyCodeChar = ? THEN "?" ELSE vBlackWhyCodeChar
         vBlackWhyNameChar  = GetCodeName("black-why",vBlackWhyCodeChar)
         vBlackWhyNameChar  = IF vBlackWhyNameChar = ? THEN "?" ELSE vBlackWhyNameChar
      .
      MESSAGE SUBSTR("    ДОКУМЕНТ: " + SUBSTR(vBlackListNameChar,1,12)
                                      + FILL(" ",45),1,45) skip
              SUBSTR("СЕРИЯ, НОМЕР: " + SUBSTR(vBlackListCodeChar,1,25)
                                      + FILL(" ",45),1,45) skip (1)
              "*** СОДЕРЖИТСЯ В ЧЕРНОМ СПИСКЕ ДОКУМЕНТОВ ***" skip (1)
             SUBSTR("    ПРИЧИНА : "  + vBlackWhyCodeChar
                                      + FILL(" ",45),1,45) skip
              SUBSTR("            - " + SUBSTR(vBlackWhyNameChar,1,45)
                                      + FILL(" ",45),1,45) skip (1)
             "Продолжить?"
      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.
      IF choose-button =  NO OR choose-button =  ? THEN
      RETURN NO-APPLY {&RET-ERROR}.
   END.
/* Вставка Плюс банк */
   END.
   ELSE
   DO: 
      RUN chk-pipe IN THIS-PROCEDURE
         (INPUT  SELF:SCREEN-VALUE,
          OUTPUT vAnswer,
          OUTPUT vMess).
      IF vAnswer EQ "1" THEN 
      DO:
         MESSAGE SUBSTR("    ДОКУМЕНТ: " + SUBSTR(tt-cust-ident.cust-code-type:SCREEN-VALUE,1,12)
                                      + FILL(" ",45),1,45) skip
              SUBSTR("СЕРИЯ, НОМЕР: " + SUBSTR(SELF:SCREEN-VALUE,1,25)
                                      + FILL(" ",45),1,45) skip (1)
              "*** СОДЕРЖИТСЯ В ЧЕРНОМ СПИСКЕ ДОКУМЕНТОВ ***" skip (1)
             SUBSTR("    ПРИЧИНА : Паспорт недействителен."
                                      + FILL(" ",45),1,45)
         VIEW-AS ALERT-BOX WARNING.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      ELSE IF vAnswer EQ "-1" THEN 
      DO:
         MESSAGE vMess SKIP (1)
             "Продолжить?"
         VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.
         IF choose-button EQ NO OR choose-button EQ ? THEN
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.   
/* Конец вставки Плюс банк */
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* Тип идентификации */
DO:  
DO TRANSACTION:
   RUN codelay ("КодДокум","КодДокум", "Типы документов" ,3). 
   IF    (LASTKEY =  13 OR LASTKEY =  10)
     AND pick-value <> ?
   THEN 
      SELF:SCREEN-VALUE = pick-value.
END.
RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.podrazd$ IN FRAME fMain /* Код подразделения */
DO:  
DO TRANSACTION:
   RUN codelay ("КодПодр","КодПодр","Код подразделения",3). 
   IF    (LASTKEY =  13 OR LASTKEY =  10)
     AND pick-value <> ?
   THEN
   DO:
      FIND FIRST code WHERE TRUE
         AND code.class  EQ "КодПодр"
         AND code.parent EQ "КодПодр"
         AND code.code EQ pick-value
      NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN
      DO:   
         SELF:SCREEN-VALUE = code.name.
         tt-cust-ident.issue:SCREEN-VALUE = code.val.  
      END.
   END.
END.
RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-ident.cust-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code-type IN FRAME fMain /* Тип идентификации */
DO:
   {&BEG_BT_LEAVE}
   RUN DispCodeName.
   IF tt-cust-ident.cust-code-type:SCREEN-VALUE =  "СНИЛС" THEN
      tt-cust-ident.cust-code:FORMAT = "xxx-xxx-xxx xx".
   ELSE
      tt-cust-ident.cust-code:FORMAT = "x(20)".
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON U1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* Тип идентификации */
DO:
   RUN DispCodeName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-cust-ident.cust-code-type IN FRAME fMain /* Тип идентификации */
DO:
   RUN DispCodeName.
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

   /* Commented by KSV: Читаем данные */
   RUN GetObject.
   
   ASSIGN
      mCustCat = ENTRY(2, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  1
      mCustId  = ENTRY(3, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  2
   .
   IF mCustCat <> "*" THEN 
      tt-cust-ident.cust-cat = mCustCat.
   
   IF       mCustId        <> "*"
      AND   TRIM (mCustId) >  "" THEN
      tt-cust-ident.cust-id = INT64(mCustId) NO-ERROR.

   /* обновление реквизитов */
   mEgrInfo = GetSysConf("EgrDocPerson").
   IF iMode =  {&MOD_ADD} OR iMode =  {&MOD_EDIT} THEN
   DO:
      IF     NUM-ENTRIES(mEgrInfo,";") >= 1
         AND ENTRY(1,mEgrInfo,";") =  "YES" THEN
         ASSIGN
            tt-cust-ident.cust-code-type = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 2 THEN ENTRY(2,mEgrInfo,";") ELSE ""
            tt-cust-ident.cust-code      = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 3 THEN ENTRY(3,mEgrInfo,";") ELSE ""
            tt-cust-ident.podrazd$       = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 4 THEN ENTRY(4,mEgrInfo,";") ELSE ""
            tt-cust-ident.open-date      = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 5 THEN DATE(ENTRY(5,mEgrInfo,";")) ELSE ?
            tt-cust-ident.issue          = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 6 THEN ENTRY(6,mEgrInfo,";") ELSE ""
         NO-ERROR.
      RUN SetSysConf IN h_base ("EgrDocPerson","").
   END.

   mCountry = GetSysConf("FPersonIsRezident").    /*из карточкм клиента*/
   mBirthday = GetSysConf("PBirthday").	/* дата рождения клиента */

   IF NOT {assigned mCountry} THEN                    /*из формы редактирования документа*/
   DO:
      IF tt-cust-ident.cust-cat =  "Ч" THEN DO:
         FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN
            vCountryId = GetXattrValueEx("person",STRING(person.person-id),"country-id2",person.country-id).
      END.
      IF tt-cust-ident.cust-cat =  "Ю" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN
            vCountryId = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"country-id2",cust-corp.country-id).
      END.
      IF NOT {assigned vCountryId} THEN
            mCountry  = "Stateless".
         ELSE
            mCountry = IF vCountryId =  "rus" THEN "CitizenRF" ELSE "Foreigner".
   END.
   RUN SetSysConf IN h_base ("IsRezident",mCountry ).

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
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END.

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

RUN disable_ui.

/* Commented by KSV: Удаляем экземпляр объекта */
IF VALID-HANDLE(mInstance) AND NOT mOnlyForm THEN 
   RUN DelEmptyInstance(mInstance).

RUN DeleteOldDataProtocol IN h_base  ("IsRezident").

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
DEF BUFFER code FOR code.
DO WITH FRAME fMain :
   fCodeName:SCREEN-VALUE = GetCodeName(mCustCodeTypeDomain,
                                        tt-cust-ident.cust-code-type:SCREEN-VALUE
                            ). 

   IF fCodeName:SCREEN-VALUE =  "?" THEN
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
  DISPLAY fCustname fCodeName separator separator-2 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-ident THEN 
    DISPLAY tt-cust-ident.cust-cat tt-cust-ident.cust-id 
          tt-cust-ident.cust-code-type tt-cust-ident.cust-code 
          tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date 
          tt-cust-ident.issue 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-cust-ident.cust-cat tt-cust-ident.cust-id fCustname 
         tt-cust-ident.cust-code-type tt-cust-ident.cust-code fCodeName 
         tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date 
         tt-cust-ident.issue separator separator-2 
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
      tt-cust-ident.cust-cat WHEN mCustCat <> "*"
      tt-cust-ident.cust-id  WHEN mCustId  <> "*"
   WITH FRAME {&FRAME-NAME}.

   mCodeType = GetSysConf("CUST-IDENT-CODE-TYPE").
   IF {assigned mCodeType} THEN
   DO:
      tt-cust-ident.cust-code-type:SCREEN-VALUE IN FRAME fMain = mCodeType.
      DISABLE tt-cust-ident.cust-code-type WITH FRAME fMain.
   END.

   IF tt-cust-ident.cust-code-type:SCREEN-VALUE =  "СНИЛС" THEN
      tt-cust-ident.cust-code:FORMAT = "xxx-xxx-xxx xx".
   ELSE
      tt-cust-ident.cust-code:FORMAT = "x(20)".

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
   DEFINE VARIABLE vFio     AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vYesNo   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vAvtoSrok AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vOldEndDate  AS DATE   NO-UNDO.
   DEFINE VARIABLE vFIOCl       AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vSnils       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrSnils    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTemp        AS DATE      NO-UNDO.
   DEFINE VARIABLE vTmpDate    AS CHARACTER NO-UNDO.
   DEFINE BUFFER person     FOR person. /* Локализация буфера. */
   DEFINE BUFFER cust-corp  FOR cust-corp.
   DEFINE BUFFER cust-ident FOR cust-ident. 
   DEFINE BUFFER bCustIdent FOR cust-ident.

   IF tt-cust-ident.cust-code-type =  "СНИЛС" THEN
      tt-cust-ident.cust-code = tt-cust-ident.cust-code:SCREEN-VALUE IN FRAME fMain.

   IF  iMode                 =  {&MOD_ADD}
   AND tt-cust-ident.cust-id <> ?
   THEN DO:
      /* проверка на наличие у человека такого же типа документа */
      FOR EACH cust-ident WHERE cust-ident.cust-code-type =  tt-cust-ident.cust-code-type
                            AND cust-ident.cust-cat       =  tt-cust-ident.cust-cat
                            AND cust-ident.cust-id        =  tt-cust-ident.cust-id
                            AND cust-ident.close-date     =  ? 
                            AND cust-ident.class-code     =  tt-cust-ident.class-code
      EXCLUSIVE-LOCK:
          
         vYesNo = NO.
         
         MESSAGE 
            "Есть не закрытый документ с кодом ~"" cust-ident.cust-code-type "~"" SKIP
            "номер " cust-ident.cust-code " от " cust-ident.open-date SKIP
            "Закрыть его?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ Внимание ]"
         UPDATE vYesNo.
         
         IF vYesNo <> YES THEN
            RETURN ERROR "Документ не введен".
         
         cust-ident.close-date = tt-cust-ident.open-date.
         RELEASE cust-ident.
      END.
   END.
   vAvtoSrok = GetXattrValueEx("code","КодДокум" + "," + tt-cust-ident.cust-code-type,"АвтоСрок","").
   IF tt-cust-ident.cust-cat = "Ю" THEN
   DO:
      FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id NO-LOCK NO-ERROR.
      IF     AVAIL cust-corp
         AND (GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "Предпр", "") <> ""
          OR GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "Субъект", "") = "ФЛП"
          OR CAN-DO(FGetSetting("СтандТр","СтатусФЛЧП",""),cust-corp.cust-stat)) THEN
         ASSIGN
            vTemp   = DATE(GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"BirthDay",""))   
            mPredpr = YES.
   END.
   ELSE
      FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   IF {assigned vAvtoSrok} THEN DO:
      IF (AVAIL person AND person.birthday <> ?) OR (mPredpr AND vTemp <> ?) THEN DO:
         vTemp = IF mPredpr THEN vTemp ELSE person.birthday.
         vEndDate = CalcSrokForDocument(vAvtoSrok,tt-cust-ident.open-date,vTemp).         
         IF vEndDate <> ? AND vEndDate <> tt-cust-ident.end-date 
         AND vEndDate <> tt-cust-ident.open-date THEN DO:
            MESSAGE 
               "Для документа ~"" tt-cust-ident.cust-code-type "~"" SKIP
               "скорректировать дату окончания на " vEndDate "?"    SKIP
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ Внимание ]"
            UPDATE vYesNo.
            IF vYesNo =  YES THEN
            tt-cust-ident.end-date = vEndDate.    
            END.

         ELSE IF vEndDate = tt-cust-ident.open-date THEN DO:
          tt-cust-ident.open-date = tt-cust-ident.open-date + 1.
          vOldEndDate = CalcSrokForDocument(vAvtoSrok,tt-cust-ident.open-date,vTemp).
           MESSAGE 
               "Для документа ~"" tt-cust-ident.cust-code-type "~"" SKIP
               "скорректировать дату окончания на " vOldEndDate "?"    SKIP
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ Внимание ]"
            UPDATE vYesNo.
            tt-cust-ident.open-date = tt-cust-ident.open-date - 1.
            IF vYesNo =  YES THEN
            tt-cust-ident.end-date = vOldEndDate.  
         END.
  
         IF tt-cust-ident.end-date <  TODAY THEN DO:
            CASE FGetSetting("СтандТр","СрокДокум","Нет"):
               WHEN "ДА" THEN DO:
                  MESSAGE 
                     "Для документа ~"" tt-cust-ident.cust-code-type "~"" SKIP
                     "просрочен" STRING(tt-cust-ident.end-date,"99/99/9999")        SKIP
                  VIEW-AS ALERT-BOX TITLE "[ Внимание ]".                              
                  RETURN ERROR "Документ не введен".
               END.
               WHEN "Пр" THEN DO:
                  MESSAGE 
                     "Для документа ~"" tt-cust-ident.cust-code-type "~"" SKIP
                     "просрочен" STRING(tt-cust-ident.end-date,"99/99/9999") "?"    SKIP
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ Внимание ]"
                  UPDATE vYesNo FORMAT "Продолжить/Отменить".               
                  if pick-value  <> "yes" THEN
                     RETURN ERROR "Документ не введен".
               END.
            END CASE.
         END.
         
      END.
   END.

   IF mPredpr THEN
      FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   ELSE IF tt-cust-ident.cust-cat  = "Ч" THEN
      FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   IF AVAIL person OR (AVAIL cust-corp AND mPredpr) THEN 
   DO: 
      /*Можно вводить документы с одинаковым номером и разной датой выдачи*/
      FOR EACH bCustIdent WHERE bCustIdent.cust-code-type =   tt-cust-ident.cust-code-type
                            AND bCustIdent.cust-code      =   tt-cust-ident.cust-code
                            AND bCustIdent.class-code     =   tt-cust-ident.class-code
                            AND RECID(bCustIdent)         <>  RECID(tt-cust-ident)
                            AND bCustIdent.cust-cat       =   tt-cust-ident.cust-cat
                            AND bCustIdent.cust-id        <>  tt-cust-ident.cust-id 
         NO-LOCK:
      
         RUN GetCustName IN h_base (bCustIdent.cust-cat,
                                    bCustIdent.cust-id,
                                    ?,
                                    OUTPUT         vFIOCl[1],
                                    OUTPUT         vFIOCl[2],
                                    INPUT-OUTPUT   vFIOCl[3]).
         vFIOCl [1] =  vFIOCl [1] +  " "   +  vFIOCl [2].
         IF bCustIdent.open-date <> tt-cust-ident.open-date THEN
         DO:
            pick-value = "NO".
            RUN Fill-SysMes IN h_tmess (  "", "", "4",
               SUBSTITUTE(
                  "Документ &1 с номером &2 уже введен для клиента ~n &3 (номер: &4). Изменить номер документа?",
                  bCustIdent.cust-code-type, bCustIdent.cust-code, 
                  vFIOCl [1], bCustIdent.cust-id)).                    
            IF  pick-value =  "YES" THEN
            DO:   
               APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain. 
               RETURN ERROR "Документ не введен".
            END.
         END.
         ELSE
            RETURN ERROR
               SUBSTITUTE(
                  "Документ &1 с номером &2 и датой выдачи &3 уже введен для клиента ~n &4 (номер: &5).",
                  bCustIdent.cust-code-type, bCustIdent.cust-code,
                  STRING(bCustIdent.open-date,"99/99/9999"), vFIOCl [1],
                  bCustIdent.cust-id).
      END.
   END.
   /* Проверка контрольного разряда СНИЛС */
   IF tt-cust-ident.cust-code-type =  "СНИЛС" THEN DO:
      RUN ChkNumberPFR IN THIS-PROCEDURE (tt-cust-ident.cust-code,
                                          OUTPUT vSnils,
                                          OUTPUT vErrSnils).
      IF {assigned vErrSnils} THEN DO:
         APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain. 
         RETURN ERROR vErrSnils.
      END.
   END.

   IF {assigned tt-cust-ident.cust-code-type } THEN
   DO:
      FIND FIRST code WHERE 
                 code.class =  mCustCodeTypeDomain 
             AND code.code =  tt-cust-ident.cust-code-type 
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
      DO:
         IF NOT( (    code.misc[7]         <> "YES" 
                  AND code.misc[8]         <> "YES" 
                  AND code.description[2]  <> "YES" )
              OR (    code.misc[7]         =  "YES" 
                  AND mCountry             =  "CitizenRF")
              OR (    code.misc[8]         =  "YES" 
                  AND mCountry             =  "Foreigner")
              OR (    code.description[2]  =  "YES" 
                  AND mCountry             =  "Stateless"))
         THEN DO: 
            RUN Fill-SysMes("base", "", 0,  "Некорректный вид документа!").
            RETURN ERROR.
         END.
      END.
   END.

   /* проверка даты выдачи */
   vTmpDate = IF AVAIL person THEN STRING(person.birthday) ELSE mbirthday.
   IF {assigned vTmpDate} THEN 
      IF tt-cust-ident.cust-code-type = "Паспорт" THEN
      DO:
         vTmpDate = SUBSTR(vTmpDate,1,6) + STRING(INT64(SUBSTR(vTmpDate,7)) + 14).          /* + 14 лет к дате */
         IF (DATE(tt-cust-ident.open-date) <= DATE(vTmpDate)     OR 
             DATE(tt-cust-ident.open-date) <= DATE("01.01.1997") OR
             DATE(tt-cust-ident.open-date) > TODAY
            ) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
               "Дата выдачи паспорта РФ выходит за возможный диапазон").
            APPLY "ENTRY" TO tt-cust-ident.open-date IN FRAME fMain. 
            RETURN ERROR.
         END.
      END.
      ELSE
      DO:
         IF DATE(tt-cust-ident.open-date) < DATE(vTmpDate) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
                "Дата выдачи документа выходит за возможный диапазон").
            APPLY "ENTRY" TO tt-cust-ident.open-date IN FRAME fMain. 
            RETURN ERROR.
         END.
      END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_Go TERMINAL-SIMULATION 
PROCEDURE Local_Go :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE mErrorStatus AS LOGICAL      NO-UNDO INIT NO.
   DEFINE VARIABLE mErrorMess   AS CHARACTER    NO-UNDO.
   DEFINE VARIABLE mErrorHfld   AS HANDLE       NO-UNDO.
   DEFINE VARIABLE vAdmCard     AS CHARACTER    NO-UNDO.

   /* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
       
      ASSIGN 
         mErrorStatus = NO
         mErrorMess   = ""
         mErrorHfld   = ?
         .

      mErrorHfld = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE,"tt-cust-ident.end-date").
      IF  NOT {assigned mErrorHfld:SCREEN-VALUE} AND tt-cust-ident.cust-code-type =  "МиграцКарта" AND tt-cust-ident.cust-code-type =  "РазрБезГражд" THEN
      DO:
         ASSIGN
            mErrorStatus = YES
            mErrorMess   = "Значение для поля <" + mErrorHfld:LABEL + "> обязательно для заполнения"
            .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.         
      END.
   END.
   /* *************************  End of Main Block  ********************** */
   
   IF NOT mErrorStatus THEN
      RETURN "".
   ELSE
   DO:
      IF mErrorMess <> "" THEN
         RUN Fill-SysMes("","","-1",mErrorMess).
      APPLY "ENTRY" TO mErrorHfld.
      RETURN ERROR.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
PROCEDURE PostSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR vPersIss     AS CHAR NO-UNDO.
   DEFINE VAR vStr         AS CHAR NO-UNDO.
   DEFINE VAR vRezult      AS LOG  NO-UNDO.
   DEFINE VAR vStatIP      AS CHAR NO-UNDO.
   DEFINE VAR vFIO         AS CHAR NO-UNDO.
   DEFINE VAR vTable       AS CHAR NO-UNDO.
   DEFINE VAR vSurr        AS CHAR NO-UNDO.
   DEFINE BUFFER person FOR person. /* Локализация буфера. */
   DEFINE BUFFER cust-corp FOR cust-corp.

   /* синхронизация с person, cust-corp*/
   IF  CAN-DO("Ч,Ю",tt-cust-ident.cust-cat)
   AND NOT mOnlyForm
   THEN DO:

    IF    (tt-cust-ident.end-date = ?
       OR tt-cust-ident.end-date > TODAY)
      AND (tt-cust-ident.close-date =  ?
       OR tt-cust-ident.close-date  >= gend-date)
    THEN DO:  

      IF mPredpr THEN
      DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            vTable = "cust-corp"
            vSurr  = IF AVAIL cust-corp THEN STRING(cust-corp.cust-id) ELSE "" NO-ERROR.
      END.
      ELSE IF tt-cust-ident.cust-cat = "Ч" THEN
      DO:
         FIND FIRST person WHERE person.person-id   = tt-cust-ident.cust-id
                             AND person.document-id = tt-cust-ident.cust-code-type 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            vTable = "person"
            vSurr  = IF AVAIL person THEN STRING(person.person-id) ELSE "" NO-ERROR.
      END.
      IF AVAIL person OR (AVAIL cust-corp AND mPredpr) THEN DO:
         IF mPredpr THEN
            RUN Fill-SysMes IN h_tmess ("", "", "4",
                  SUBSTITUTE(
                     "Сделать основным документ &1 с номером &2 для клиента (номер: &3)?",
                     tt-cust-ident.cust-code-type, tt-cust-ident.cust-code, tt-cust-ident.cust-id)).                    
         IF AVAIL person OR pick-value = "YES" THEN
         DO:
            IF NOT UpdateSigns(vTable, 
                               vSurr,
                               "Document4Date_vid", 
                               STRING(tt-cust-ident.open-date, "99/99/9999"),
                               ?) THEN DO:
               RUN Fill-SysMes IN h_tmess 
                  ("", "", "-1", 
                   "Ошибка обновления доп.реквизита Document4Date_vid в карточке " 
                   + IF mPredpr THEN "юрлица!" ELSE "физлица!").
               RETURN ERROR.
            END.
            IF mPredpr THEN
            DO:
               UpdateSigns(vTable, vSurr, "document", tt-cust-ident.cust-code, ?) NO-ERROR.
               UpdateSigns(vTable, vSurr, "document-id", tt-cust-ident.cust-code-type, ?) NO-ERROR.
            END.
            ELSE                  
               ASSIGN
                  person.document    = tt-cust-ident.cust-code
                  person.document-id = tt-cust-ident.cust-code-type
               NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN DO:
               RUN MakeIssue IN h_cust (tt-cust-ident.issue, tt-cust-ident.podrazd$, OUTPUT vPersIss).
               IF mPredpr THEN
                  UpdateSigns(vTable, vSurr, "issue", vPersIss, ?) NO-ERROR.
               ELSE IF tt-cust-ident.cust-cat = "Ч" THEN
                  person.issue = vPersIss NO-ERROR.
            END.
            IF NOT ERROR-STATUS:ERROR THEN
               IF mPredpr THEN VALIDATE cust-corp NO-ERROR.
                          ELSE VALIDATE person    NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               RUN Fill-SysMes IN h_tmess 
                  ("", "", "-1", "Ошибка синхронизации с карточкой " 
                   + (IF mPredpr THEN "юрлица: " ELSE "физлица: ")
                   + (IF RETURN-VALUE > "" THEN GetNullStr(RETURN-VALUE) 
                      ELSE ERROR-STATUS:GET-MESSAGE(1)) ).
               RETURN ERROR.
            END.
         END.
      END.
      ELSE IF LOCKED person THEN DO:
         FIND FIRST person WHERE person.person-id   = tt-cust-ident.cust-id
                             AND person.document-id = tt-cust-ident.cust-code-type
         NO-LOCK NO-ERROR.
         IF AVAIL person THEN DO:
            vStr = "Карточку физлица редактирует другой пользователь,~nсинхронизация документа невозможна".
            WhoLocks2(RECID(person),"person",INPUT-OUTPUT vStr).
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vStr).
            RETURN ERROR.
         END.
      END.
      ELSE IF LOCKED cust-corp THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id  = tt-cust-ident.cust-id
                                AND GetXattrValueEx
                                       ("cust-corp", 
                                        STRING(cust-corp.cust-id), 
                                        "document-id", "") = tt-cust-ident.cust-code-type
         NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN DO:
            vStr = "Карточку юрлица редактирует другой пользователь,~nсинхронизация документа невозможна".
            WhoLocks2(RECID(cust-corp),"cust-corp",INPUT-OUTPUT vStr).
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vStr).
            RETURN ERROR.
         END.
      END.
    END. 

   END.


   /*Проверка по стоп-листам*/
   IF iMode =  {&MOD_ADD}   OR iMode =  {&MOD_EDIT} 
   THEN DO:
      RUN ChkByStopList (tt-cust-ident.cust-cat,tt-cust-ident.cust-id,"КонтрВводРед_Кл", "CLNT",OUTPUT vRezult).
      IF vRezult THEN 
      DO:
         {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
         RUN PrintClientSLRep.
         {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
      END.
   END. 

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   IF iMode =  {&MOD_ADD} OR iMode =  {&MOD_EDIT} 
   THEN DO:
      IF tt-cust-ident.cust-cat =  "Ю" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN
            ASSIGN
               vStr    = GetXattrValueEX("cust-corp", STRING(tt-cust-ident.cust-id), "Предпр", "")
               vStatIP = FGetSetting("СтандТр","СтатусФЛЧП","")
               vRezult = FGetSetting("ОбменФМС","ФМСРед_КлЮ","") =  "Да" AND
                         (vStr <> "" OR CAN-DO(vStatIP,cust-corp.cust-stat))
               vStr    = "cust-corp"
               vFIO    = cust-corp.name-corp 
            .
         ELSE
            vRezult = no.    
      END.   
      ELSE IF tt-cust-ident.cust-cat =  "Ч" THEN DO:
              FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
              IF AVAIL person THEN
                 ASSIGN
                    vRezult = FGetSetting("ОбменФМС","ФМСРед_Кл","") =  "Да"
                    vStr    = "person"
                    vFIO    = person.name-last + " " + person.first-names
                 .
              ELSE
                 vRezult = no.
           END.
           ELSE 
              vRezult = no.
      
      IF vRezult THEN DO:
         {cl-fmschk.i
            "vStr"
            "tt-cust-ident.cust-id"
            "tt-cust-ident.cust-code-type"
            "tt-cust-ident.cust-code"
            "STRING(tt-cust-ident.open-date,'99/99/9999')"
            "vFIO"
         }
         IF    (tt-cust-ident.cust-cat = "Ч"   
            AND FGetSetting("ОбменФМС","ФМСВвод_Кл","") = "Да")
            OR (tt-cust-ident.cust-cat = "Ю"
            AND FGetSetting("ОбменФМС","ФМСВвод_КлЮ","") = "Да") THEN
         DO:
            IF AVAILABLE tt-res THEN DO:
               IF CAN-DO({&KodIsNotValidDoc},tt-res.fChkKodSt) THEN
               DO:   
                  APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain.
                  RETURN ERROR.
               END.    
            END.
         END.              
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-cidenp.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='31/07/2017 13:30:41.808+03:00' */
/*prosignoYn5zXbMhgPEccopcWkaQQ*/