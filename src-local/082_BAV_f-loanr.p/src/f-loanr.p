&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan-acct-main NO-UNDO LIKE loan-acct
       FIELD class-code AS CHARACTER /* class-code */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loan-acct-main" "loan-acct-main" }
       .
DEFINE TEMP-TABLE tt-loanr NO-UNDO LIKE loan
       FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
       FIELD grup_dog$ AS CHARACTER /* Груп_дог */
       FIELD datasogl$ AS DATE /* ДатаСогл */
       FIELD data_uar$ AS CHARACTER /* Дата_УАР */
       FIELD dosroka$ AS CHARACTER /* ДоСРОКА */
       FIELD igndtokwc$ AS LOGICAL /* ИгнДтОкч */
       FIELD ovrpr$ AS INT64 /* ОврПр */
       FIELD ovrstop$ AS INT64 /* ОврСтоп */
       FIELD okrugsum$ AS LOGICAL /* ОкругСум */
       FIELD rewzim$ AS CHARACTER /* Режим */
       FIELD sindkred$ AS LOGICAL /* СиндКред */
       FIELD BankCust AS CHARACTER /* BankCust */
       FIELD Bfnc AS CHARACTER /* Bfnc */
       FIELD CallAcct AS CHARACTER /* CallAcct */
       FIELD cred-offset AS CHARACTER /* cred-offset */
       FIELD dateend AS DATE /* dateend */
       FIELD delay AS INT64 /* delay */
       FIELD delay1 AS INT64 /* delay1 */
       FIELD DTKind AS CHARACTER /* DTKind */
       FIELD DTType AS CHARACTER /* DTType */
       FIELD Exec_D AS LOGICAL /* Exec_D */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD int-offset AS CHARACTER /* int-offset */
       FIELD IntAcct AS CHARACTER /* IntAcct */
       FIELD list_type AS CHARACTER /* list_type */
       FIELD loan-acct AS CHARACTER /* loan-acct */
       FIELD loan-par-group AS CHARACTER /* loan-par-group */
       FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
       FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
       FIELD OblAcct AS CHARACTER /* OblAcct */
       FIELD op-date AS CHARACTER /* op-date */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
       FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
       FIELD qqq AS CHARACTER /* qqq */
       FIELD rate-list AS CHARACTER /* rate-list */
       FIELD rel_type AS CHARACTER /* rel_type */
       FIELD ReplDate AS DATE /* ReplDate */
       FIELD RevRef1 AS CHARACTER /* RevRef1 */
       FIELD RevRef2 AS CHARACTER /* RevRef2 */
       FIELD round AS LOGICAL /* round */
       FIELD TermKey AS CHARACTER /* TermKey */
       FIELD TicketNumber AS CHARACTER /* TicketNumber */
       FIELD UniformBag AS CHARACTER /* UniformBag */
       FIELD WorkDelay AS LOGICAL /* WorkDelay */
       FIELD convert AS LOGICAL /* convert */
       FIELD f634_val AS CHARACTER /* f634_val */
       FIELD dogviduwceta$ AS CHARACTER /* ДогВидУчета */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loanr" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-LOANR.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 16.11.2006 17:05 Vasov   
     Modified: 31.05.2007 MUTA 0077173  При вводе нового договора на РКО
                               категория счетов предустановлена в "b" 
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
{intrface.get tmess}    
{intrface.get terr}
{intrface.get xclass}
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */

DEFINE TEMP-TABLE t-obj NO-UNDO
         FIELD rec AS recid.
DEFINE VARIABLE cDR     AS CHARACTER    NO-UNDO.

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

DEFINE VARIABLE mContract    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcctChanged AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mErrMsg      AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttRes NO-UNDO
  FIELD FileRowId AS ROWID
  FIELD PickValue AS CHARACTER
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loanr tt-loan-acct-main

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loan-acct-main.acct ~
tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loan-acct-main.acct ~
tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loan-acct-main tt-loanr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loan-acct-main
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-fMain tt-loanr
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loanr SHARE-LOCK, ~
      EACH tt-loan-acct-main WHERE TRUE /* Join to tt-loanr incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loanr SHARE-LOCK, ~
      EACH tt-loan-acct-main WHERE TRUE /* Join to tt-loanr incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loanr tt-loan-acct-main
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loanr
&Scoped-define SECOND-TABLE-IN-QUERY-fMain tt-loan-acct-main


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-TABLES tt-loan-acct-main tt-loanr
&Scoped-define FIRST-ENABLED-TABLE tt-loan-acct-main
&Scoped-define SECOND-ENABLED-TABLE tt-loanr
&Scoped-Define DISPLAYED-FIELDS tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define DISPLAYED-TABLES tt-loan-acct-main tt-loanr
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan-acct-main
&Scoped-define SECOND-DISPLAYED-TABLE tt-loanr


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define List-2 tt-loanr.end-date tt-loanr.close-date ~
tt-loanr.comment 
&Scoped-define List-3 tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define List-4 tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loanr, 
      tt-loan-acct-main SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loan-acct-main.acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     tt-loanr.doc-ref
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 16 COLON-ALIGNED
          &ELSE AT ROW 3 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
     tt-loanr.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 16 COLON-ALIGNED
          &ELSE AT ROW 4 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS COMBO-BOX 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-loanr.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 16 COLON-ALIGNED
          &ELSE AT ROW 5 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loanr.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF FORMAT "x(3)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-loanr.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 16 COLON-ALIGNED
          &ELSE AT ROW 7 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 16 COLON-ALIGNED
          &ELSE AT ROW 9 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.comment
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 16 COLON-ALIGNED
          &ELSE AT ROW 10 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 32 BY 1
          &ELSE SIZE 32 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 15 ROW 5
         SIZE 52 BY 14
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan-acct-main T "?" NO-UNDO bisquit loan-acct
      ADDITIONAL-FIELDS:
          FIELD class-code AS CHARACTER /* class-code */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loan-acct-main" "loan-acct-main" }
          
      END-FIELDS.
      TABLE: tt-loanr T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
          FIELD grup_dog$ AS CHARACTER /* Груп_дог */
          FIELD datasogl$ AS DATE /* ДатаСогл */
          FIELD data_uar$ AS CHARACTER /* Дата_УАР */
          FIELD dosroka$ AS CHARACTER /* ДоСРОКА */
          FIELD igndtokwc$ AS LOGICAL /* ИгнДтОкч */
          FIELD ovrpr$ AS INT64 /* ОврПр */
          FIELD ovrstop$ AS INT64 /* ОврСтоп */
          FIELD okrugsum$ AS LOGICAL /* ОкругСум */
          FIELD rewzim$ AS CHARACTER /* Режим */
          FIELD sindkred$ AS LOGICAL /* СиндКред */
          FIELD BankCust AS CHARACTER /* BankCust */
          FIELD Bfnc AS CHARACTER /* Bfnc */
          FIELD CallAcct AS CHARACTER /* CallAcct */
          FIELD cred-offset AS CHARACTER /* cred-offset */
          FIELD dateend AS DATE /* dateend */
          FIELD delay AS INT64 /* delay */
          FIELD delay1 AS INT64 /* delay1 */
          FIELD DTKind AS CHARACTER /* DTKind */
          FIELD DTType AS CHARACTER /* DTType */
          FIELD Exec_D AS LOGICAL /* Exec_D */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD int-offset AS CHARACTER /* int-offset */
          FIELD IntAcct AS CHARACTER /* IntAcct */
          FIELD list_type AS CHARACTER /* list_type */
          FIELD loan-acct AS CHARACTER /* loan-acct */
          FIELD loan-par-group AS CHARACTER /* loan-par-group */
          FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
          FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
          FIELD OblAcct AS CHARACTER /* OblAcct */
          FIELD op-date AS CHARACTER /* op-date */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
          FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
          FIELD qqq AS CHARACTER /* qqq */
          FIELD rate-list AS CHARACTER /* rate-list */
          FIELD rel_type AS CHARACTER /* rel_type */
          FIELD ReplDate AS DATE /* ReplDate */
          FIELD RevRef1 AS CHARACTER /* RevRef1 */
          FIELD RevRef2 AS CHARACTER /* RevRef2 */
          FIELD round AS LOGICAL /* round */
          FIELD TermKey AS CHARACTER /* TermKey */
          FIELD TicketNumber AS CHARACTER /* TicketNumber */
          FIELD UniformBag AS CHARACTER /* UniformBag */
          FIELD WorkDelay AS LOGICAL /* WorkDelay */
          FIELD convert AS LOGICAL /* convert */
          FIELD f634_val AS CHARACTER /* f634_val */
          FIELD dogviduwceta$ AS CHARACTER /* ДогВидУчета */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loanr" "" }
          
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
/* SETTINGS FOR FILL-IN tt-loan-acct-main.acct IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-loanr.close-date IN FRAME fMain
   1 2 3 4 EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN tt-loanr.comment IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.currency IN FRAME fMain
   1 3 4 EXP-FORMAT                                                     */
/* SETTINGS FOR COMBO-BOX tt-loanr.cust-cat IN FRAME fMain
   1 3 4                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.cust-id IN FRAME fMain
   1 3 4                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.doc-ref IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-loanr.end-date IN FRAME fMain
   1 2 3 4 EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN tt-loanr.open-date IN FRAME fMain
   1 3 4 EXP-FORMAT                                                     */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loanr,Temp-Tables.tt-loan-acct-main WHERE Temp-Tables.tt-loanr ..."
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-loan-acct-main.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON F1 OF tt-loan-acct-main.acct IN FRAME fMain /* Лицевой счет */
DO:
   IF iMode EQ {&MOD_ADD} THEN
   DO:
      {empty ttRes}
      RUN browseld.p ("acct",
                      "RetRcp"                         + CHR(1) + "RetFld" + CHR(1) + "acct-cat",
                      STRING (TEMP-TABLE ttRes:HANDLE) + CHR(1) + "acct,currency,cust-cat,cust-id" + CHR(1) + "b",
                      "",
                      4).
      IF KEYFUNCTION (LASTKEY) NE "END-ERROR" THEN
         FOR FIRST ttRes:
            IF ENTRY(3,ttRes.PickValue) = "В" THEN DO:
               RUN Fill-SysMes IN h_tmess ("","","-1","Нельзя открыть договор для счета с типом клиента - В").
               RETURN.
            END.
            ASSIGN
               tt-loan-acct-main.acct = ENTRY (1, ttRes.PickValue)
               tt-loanr.currency      = ENTRY (2, ttRes.PickValue)
               tt-loanr.cust-cat      = ENTRY (3, ttRes.PickValue)
               tt-loanr.cust-id       = INT64 (ENTRY (4, ttRes.PickValue))
               cDR                    = GetXAttrValue("acct", tt-loan-acct-main.acct + "," + tt-loanr.currency, "ДогОткрЛС")
               tt-loanr.doc-ref       = IF (NUM-ENTRIES(cDR) GE 2) THEN TRIM(ENTRY(2, cDR)) ELSE tt-loanr.doc-ref
               tt-loanr.open-date     = IF (cDR EQ "") THEN tt-loanr.open-date ELSE DATE(TRIM(ENTRY(1, cDR)))
            NO-ERROR.
            DISPLAY tt-loan-acct-main.acct
                    tt-loanr.currency
                    tt-loanr.cust-cat
                    tt-loanr.cust-id
                    tt-loanr.doc-ref
                    tt-loanr.open-date
            WITH FRAME fMain.
            mAcctChanged = FALSE.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON LEAVE OF tt-loan-acct-main.acct IN FRAME fMain /* Лицевой счет */
DO:
   {&BEG_BT_LEAVE}
   IF mAcctChanged THEN
   DO:
      {find-act.i
         &acct = SELF:SCREEN-VALUE
      }
      IF AVAIL acct THEN
      DO:
         IF acct.cust-cat = "В" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","Нельзя открыть договор для счета с типом клиента - В").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
         ASSIGN
             tt-loanr.currency  = acct.currency
             tt-loanr.cust-cat  = acct.cust-cat
             tt-loanr.cust-id   = acct.cust-id
             cDR                = GetXAttrValue("acct", acct.acct + "," + acct.currency, "ДогОткрЛС")
             tt-loanr.doc-ref   = IF (NUM-ENTRIES(cDR) GE 2) THEN TRIM(ENTRY(2, cDR)) ELSE tt-loanr.doc-ref
             tt-loanr.open-date = IF (cDR EQ "") THEN tt-loanr.open-date ELSE DATE(TRIM(ENTRY(1, cDR)))
         .
         DISPLAY tt-loanr.currency
                 tt-loanr.cust-cat
                 tt-loanr.cust-id
                 tt-loanr.doc-ref
                 tt-loanr.open-date
         WITH FRAME fMain.
         mAcctChanged = FALSE.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-loan-acct-main.acct IN FRAME fMain /* Лицевой счет */
DO:
   mAcctChanged = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loanr.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-loanr.open-date IN FRAME fMain /*  */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE mDate AS CHARACTER        NO-UNDO.
   DEFINE VARIABLE iDate AS INTEGER          NO-UNDO.
   DEFINE VARIABLE lDate AS LOGICAL INIT YES NO-UNDO.
   ASSIGN mDate = SELF:SCREEN-VALUE      NO-ERROR.
   iDate = INTEGER(SUBSTRING(mDate,1,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1 OR iDate > 31 THEN lDate = NO.
   iDate = INTEGER(SUBSTRING(mDate,4,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1 OR iDate > 12 THEN lDate = NO.
   iDate = INTEGER(SUBSTRING(mDate,7,4)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1900            THEN lDate = NO.
   IF lDate = NO THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1","Неверная дата!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&SCOPED-DEFINE LstSymNotAllowed "," + CHR(1) + ";" + CHR(1) + "~~~{" + CHR(1) + "~~~}" + CHR(1) + '~"' + CHR(1) + "~'"

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loanr.doc-ref TERMINAL-SIMULATION
ON LEAVE OF tt-loanr.doc-ref IN FRAME fMain /*  */
DO:
   DEFINE VARIABLE vCnt    AS INT64     NO-UNDO.
   DEFINE VARIABLE vNum    AS INT64     NO-UNDO.
   DEFINE VARIABLE vDocRef AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErr    AS CHARACTER NO-UNDO.

   IF iMode EQ {&MOD_ADD} THEN
   DO:
      {&BEG_BT_LEAVE}
      ASSIGN
         vDocRef = SELF:SCREEN-VALUE
         vNum = NUM-ENTRIES({&LstSymNotAllowed},CHR(1))
      NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND {assigned vDocRef} THEN
      DO:                                                  
         DO vCnt = 1 TO vNum:
            IF INDEX(vDocRef,ENTRY(vCnt,{&LstSymNotAllowed},CHR(1))) > 0 
               THEN vErr = vErr + " " + ENTRY(vCnt,{&LstSymNotAllowed},CHR(1)).
         END.
         IF vErr <> "" THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1","В номере договора недопустимые символы: ~n" + TRIM(vErr)).
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      {&END_BT_LEAVE}
   END.
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
  IF AVAILABLE tt-loan-acct-main THEN 
    DISPLAY tt-loan-acct-main.acct 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loanr THEN 
    DISPLAY tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency 
          tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date 
          tt-loanr.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loan-acct-main.acct tt-loanr.doc-ref tt-loanr.cust-cat 
         tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date 
         tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
DEFINE BUFFER bLoanAcct FOR loan-acct.
DEFINE BUFFER bLoan     FOR loan.

DEFINE VARIABLE vHoliday  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vIsClosed AS LOGICAL     NO-UNDO.

IF iMode EQ {&MOD_ADD} THEN
DO:

   tt-loan-acct-main.acct = AddFilToAcct(tt-loan-acct-main.acct, tt-loanr.filial-id).
   tt-loanr.cont-code = FmtMskAddSuffix(tt-loanr.doc-ref,"cont-code").

   IF NOT {assigned tt-loan-acct-main.acct} THEN
   DO:
      mErrMsg = "Задайте номер расчетного счета!".
      RETURN ERROR mErrMsg.
   END.
   {find-act.i
      &acct = tt-loan-acct-main.acct
      &curr = tt-loanr.currency
   }
   IF NOT AVAIL acct THEN
   DO:
      mErrMsg = "Нет такого счета! - " + tt-loan-acct-main.acct +
                " с валютой '" + (IF tt-loanr.currency EQ ""
                                   THEN "{&in-LF-NCN}"
                                    ELSE tt-loanr.currency) + "'".
      RETURN ERROR mErrMsg.
   END.

   IF acct.cust-id  NE tt-loanr.cust-id OR
      acct.cust-cat NE tt-loanr.cust-cat
   THEN
   DO:
      mErrMsg = "У расчетного счета " + tt-loan-acct-main.acct + " другой хозяин!".
      RETURN ERROR mErrMsg.
   END.

   IF acct.currency NE tt-loanr.currency THEN
   DO:
      mErrMsg = "Счет " + tt-loan-acct-main.acct + " и договор открыты в разных валютах!".
      RETURN ERROR mErrMsg.
   END.

   IF CAN-FIND (FIRST bLoan WHERE bLoan.contract  EQ mContract
                         AND   bLoan.doc-ref   EQ tt-loanr.doc-ref
                         AND   bLoan.filial-id EQ tt-loanr.filial-id
                         AND   ROWID (bLoan)   NE tt-loanr.local__rowid
               NO-LOCK)
   THEN
   DO:
      mErrMsg = "Уже есть договор с назначением " + mContract + " и номером " + 
                tt-loanr.doc-ref + ".~nЗадайте другой номер договора".
      RETURN ERROR mErrMsg.
   END.


   IF tt-loanr.cust-cat EQ "В" THEN
   DO:
      mErrMsg = "Нельзя открыть договор для счета с типом клиента - В".
      RETURN ERROR mErrMsg.
   END.

   /* проверка на предмет закрытости клиента */
   RUN custools.p (tt-loanr.cust-cat, tt-loanr.cust-id, OUTPUT vIsClosed).
   IF vIsClosed THEN
   DO:
      mErrMsg = "Клиент закрыт".
      RETURN ERROR mErrMsg.
   END.

   FIND FIRST bLoanAcct WHERE bLoanAcct.contract  EQ mContract
                        AND   bLoanAcct.acct-type EQ mContract
                        AND   bLoanAcct.currency  EQ tt-loanr.currency
                        AND   bLoanAcct.acct      EQ tt-loan-acct-main.acct
                        AND   CAN-FIND (FIRST bLoan WHERE bLoan.contract   EQ mContract
                                                    AND   bLoan.cont-code  EQ bLoanAcct.cont-code
                                                    AND   bLoan.cont-code  NE tt-loanr.cont-code
                                                    AND   bLoan.close-date EQ ?)
      NO-LOCK NO-ERROR.
   IF AVAIL bLoanAcct THEN
   DO:
      mErrMsg = "Не может быть несколько договоров для одного и того же счета".
      tt-loan-acct-main.acct = ?.
      RETURN ERROR mErrMsg.
   END.

END.     /*MOD_ADD*/

IF iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT} THEN
DO:
   vHoliday = Holiday (tt-loanr.end-date).
   IF vHoliday THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","4","Дата окончания попала на праздничный или выходной день! Оставить?").
      IF NOT LOGICAL (pick-value) THEN
         RETURN ERROR.
   END.

   IF tt-loanr.end-date LT tt-loanr.open-date THEN
   DO:
      mErrMsg = "Дата окончания договора меньше даты начала договора".
      RETURN  ERROR mErrMsg.
   END.

   IF tt-loanr.close-date LT tt-loanr.open-date THEN
   DO:
      mErrMsg = "Дата закрытия договора меньше даты начала договора".
      RETURN  ERROR mErrMsg.
   END.

   /* Проверки успешно завершены */
   ASSIGN
      tt-loan-acct-main.contract  = tt-loanr.contract
      tt-loan-acct-main.cont-code = tt-loanr.cont-code
      tt-loan-acct-main.acct-type = tt-loanr.contract
      tt-loan-acct-main.currency  = tt-loanr.currency
      tt-loan-acct-main.since     = tt-loanr.open-date
      NO-ERROR
   .
   
   FOR EACH term-obl OF tt-loanr WHERE term-obl.sop-date EQ ?
      EXCLUSIVE-LOCK:
      term-obl.sop-date = tt-loanr.close-date.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
DEFINE BUFFER bLoan FOR loan.
DEFINE VARIABLE vCountCode AS DECIMAL     NO-UNDO.

IF iMode EQ {&MOD_EDIT} OR iMode EQ {&MOD_VIEW} THEN
DO:
   ASSIGN
      mContract  = ENTRY (2, iInstanceList, CHR(3)) WHEN NUM-ENTRIES (iInstanceList, CHR (3)) GE 2.
END.

IF iMode EQ {&MOD_ADD} THEN
DO:
   ASSIGN
      mContract  = ENTRY(2, iInstanceList, CHR(3)) WHEN NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
      vCountCode = DECIMAL(YEAR(TODAY) MODULO 100)
   .
   FIND LAST bLoan WHERE bLoan.contract        EQ mContract
                     AND YEAR(bLoan.open-date) EQ YEAR (TODAY)
      NO-LOCK NO-ERROR.
   _try_get_last_num:
   DO WHILE AVAIL bLoan :
      DECIMAL(bLoan.doc-ref) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         FIND PREV bLoan WHERE bLoan.contract        EQ mContract
                           AND YEAR(bLoan.open-date) EQ YEAR (TODAY)
         NO-LOCK NO-ERROR.
         NEXT _try_get_last_num.
      END.
      vCountCode = DECIMAL(bLoan.doc-ref).
      LEAVE _try_get_last_num.
   END.
   RELEASE bLoan.
   ASSIGN
      vCountCode           = vCountCode + 0.00001
      tt-loanr.contract    = mContract
      tt-loanr.doc-ref     = TRIM (STRING (vCountCode,'>>9.99999'))
      tt-loanr.filial-id   = dept.branch
      tt-loanr.cust-cat    = "Ю"
      tt-loanr.branch-id   = ""
      tt-loanr.loan-status = ""
      tt-loanr.currency    = ""
   .
   tt-loanr.cont-code   = FmtMskAddSuffix(tt-loanr.doc-ref,"cont-code").
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

   DEF VAR vName AS CHAR extent 3 NO-UNDO.
   DEF VAR vDogOtkrLS AS CHAR NO-UNDO.
   
   FIND FIRST acct WHERE acct.acct EQ tt-loan-acct-main.acct.
   IF AVAIL(acct) THEN DO:
      vDogOtkrLS = GetXAttrValueEx("acct",SURROGATE(BUFFER acct:HANDLE),"ДогОткрЛС","").
      IF NOT {assigned vDogOtkrLS} THEN DO:
         vDogOtkrLS = STRING(tt-loanr.open-date,"99/99/9999") + "," + STRING(tt-loanr.doc-ref).
         UpdateSigns(acct.class-code,SURROGATE(BUFFER acct:HANDLE),"ДогОткрЛС",vDogOtkrLS,?).
      END.
   END.


   RUN GetCustName IN h_base (tt-loanr.cust-cat,
                              tt-loanr.cust-id,
                              ?,
                              OUTPUT vName[1],
                              OUTPUT vName[2],
                              INPUT-OUTPUT vName[3]
                             ).
   vName[1] = TRIM(vName[1] + " " + vNAme[2]).                       
   {lg7001cl.i
      &in-class  = 'loan'
      &surrogate = "tt-loanr.contract + ',' + tt-loanr.cont-code"
      &cl_name1  = "vName[1]"
      &nodefpesr = YES
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
