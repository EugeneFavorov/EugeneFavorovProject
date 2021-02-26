&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan NO-UNDO LIKE loan
       FIELD op-kind-allowed AS CHARACTER /* op-kind-allowed */
       FIELD agent-cat AS CHARACTER /* agent-cat */
       FIELD agent-id AS INT64 /* agent-id */
       FIELD RevRef2 AS CHARACTER /* RevRef2 */
       FIELD round AS LOGICAL /* round */
       FIELD TermKey AS CHARACTER /* TermKey */
       FIELD TicketNumber AS CHARACTER /* TicketNumber */
       FIELD UniformBag AS CHARACTER /* UniformBag */
       FIELD int-offset AS CHARACTER /* int-offset */
       FIELD IntAcct AS CHARACTER /* IntAcct */
       FIELD list_type AS CHARACTER /* list_type */
       FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
       FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
       FIELD OblAcct AS CHARACTER /* OblAcct */
       FIELD op-date AS DATE /* op-date */
       FIELD Op-kind_Acct AS CHARACTER /* Op-kind_Acct */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
       FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
       FIELD rate-list AS CHARACTER /* rate-list */
       FIELD rel_type AS CHARACTER /* rel_type */
       FIELD ReplDate AS DATE /* ReplDate */
       FIELD RevRef1 AS CHARACTER /* RevRef1 */
       FIELD rewzim$ AS CHARACTER /* Режим */
       FIELD sindkred$ AS LOGICAL /* СиндКред */
       FIELD BankCust AS CHARACTER /* BankCust */
       FIELD Bfnc AS CHARACTER /* Bfnc */
       FIELD CallAcct AS CHARACTER /* CallAcct */
       FIELD cred-offset AS CHARACTER /* cred-offset */
       FIELD dateend AS DATE /* dateend */
       FIELD DTKind AS CHARACTER /* DTKind */
       FIELD DTType AS CHARACTER /* DTType */
       FIELD Exec_D AS LOGICAL /* Exec_D */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
       FIELD grup_dog$ AS CHARACTER /* Груп_дог */
       FIELD datasogl$ AS DATE /* ДатаСогл */
       FIELD data_uar$ AS CHARACTER /* Дата_УАР */
       FIELD dop_proc$ AS CHARACTER /* Доп_Проц */
       FIELD dosroka$ AS CHARACTER /* ДоСРОКА */
       FIELD igndtokwc$ AS LOGICAL /* ИгнДтОкч */
       FIELD nesno$ AS CHARACTER /* НеснО */
       FIELD ovrpr$ AS INT64 /* ОврПр */
       FIELD ovrstop$ AS INT64 /* ОврСтоп */
       FIELD okrugsum$ AS LOGICAL /* ОкругСум */
       FIELD general-mark AS LOGICAL /* general-mark */
       FIELD loan-allowed AS CHARACTER /* loan-allowed */
       FIELD details AS CHARACTER /* details */
       FIELD can-redraw-mark AS LOGICAL /* can-redraw-mark */
       FIELD single-mark AS LOGICAL /* single-mark */
       FIELD create-date AS DATE /* create-date */
       FIELD drower-cat AS CHARACTER /* drower-cat */
       FIELD drower-id AS INT64 /* drower-id */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loan" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: f-trust.p
      Comment: Форма ввода доверенности
   Parameters:
         Uses:
      Used by:
      Created: 21.12.2004 Илюха
     Modified: 01.04.2009 12:09 BIS      <comment>
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

&GLOBAL-DEFINE MAIN-FRAME fMain

{globals.i}
{intrface.get xclass} 
{intrface.get tparam}
{intrface.get loan}
{intrface.get cdrep}
{intrface.get trust}
{intrface.get count} /* Чтобы работал GetCounterNextValue */

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

DEF VAR mAgentId  AS INT64  NO-UNDO INIT ?.
DEF VAR mDrowerId AS INT64  NO-UNDO INIT ?.
DEF VAR mCustId   AS INT64  NO-UNDO INIT ?.
DEF VAR mParent   AS CHAR NO-UNDO.
DEF VAR mDate     AS DATE NO-UNDO.

DEFINE VAR mMinDate AS DATE NO-UNDO INIT ?.
DEFINE VAR mMaxDate AS DATE NO-UNDO INIT ?.

DEFINE VAR mFlagCloseDate AS LOGICAL INIT NO NO-UNDO.

DEF TEMP-TABLE tFields NO-UNDO
   FIELD tHandle    AS HANDLE
   FIELD tSensitive AS LOG 
   FIELD tVisible   AS LOG
   FIELD tName      AS CHAR
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
&Scoped-define INTERNAL-TABLES tt-loan

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loan.create-date ~
tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id ~
tt-loan.agent-id tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loan.create-date ~
tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id ~
tt-loan.agent-id tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loan
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loan
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loan SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loan SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loan
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loan


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-TABLES tt-loan
&Scoped-define FIRST-ENABLED-TABLE tt-loan
&Scoped-Define ENABLED-OBJECTS separator2 fRedraw separator1 fProxy ~
fCliName1 
&Scoped-Define DISPLAYED-FIELDS tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define DISPLAYED-TABLES tt-loan
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan
&Scoped-Define DISPLAYED-OBJECTS separator2 fRedraw separator1 fProxy ~
fCliName1 fCliName2 fCliName3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.loan-allowed tt-loan.close-date ~
tt-loan.comment 
&Scoped-define List-2 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.end-date tt-loan.single-mark tt-loan.can-redraw-mark ~
tt-loan.loan-allowed tt-loan.close-date tt-loan.comment 
&Scoped-define List-3 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.loan-allowed tt-loan.close-date ~
tt-loan.comment 
&Scoped-define List-4 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.close-date tt-loan.comment 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fCliName1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCliName2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCliName3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fProxy AS CHARACTER FORMAT "x(22)" 
     LABEL "на основании" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U INITIAL "---------------------------------------------------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U INITIAL "-------------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fRedraw AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
     &ELSE SIZE 4 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loan SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 1
          &ELSE AT ROW 10 COL 1 &ENDIF NO-LABEL
     fRedraw
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 25
          &ELSE AT ROW 2 COL 25 &ENDIF
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     tt-loan.create-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 18 COLON-ALIGNED
          &ELSE AT ROW 1 COL 18 COLON-ALIGNED &ENDIF HELP
          "Дата оформления доверенности"
          LABEL "Дата оформления" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.doc-num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 38 COLON-ALIGNED
          &ELSE AT ROW 1 COL 38 COLON-ALIGNED &ENDIF HELP
          "Номер"
          LABEL "НОМЕР" FORMAT "X(25)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan.loan-status
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 60 COLON-ALIGNED
          &ELSE AT ROW 1 COL 60 COLON-ALIGNED &ENDIF HELP
          "Статус доверенности"
          LABEL "СТАТУС" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     fProxy
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 43 COLON-ALIGNED
          &ELSE AT ROW 2 COL 43 COLON-ALIGNED &ENDIF HELP
          "Доверенность, на основании которой выдется данная доверенность"
     tt-loan.cont-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18 COLON-ALIGNED
          &ELSE AT ROW 3 COL 18 COLON-ALIGNED &ENDIF HELP
          "Тип договора1."
          LABEL "Вид доверенности" FORMAT "x(42)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 42 BY 1
          &ELSE SIZE 42 BY 1 &ENDIF
     tt-loan.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 18 COLON-ALIGNED
          &ELSE AT ROW 4 COL 18 COLON-ALIGNED &ENDIF HELP
          "Порядковый номер клиента (нажмите F1 для выбора)"
          LABEL "Клиент" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCliName1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 29 COLON-ALIGNED
          &ELSE AT ROW 4 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.drower-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 18 COLON-ALIGNED
          &ELSE AT ROW 5 COL 18 COLON-ALIGNED &ENDIF HELP
          "Код клиента, выписавшего доверенность"
          LABEL "Доверитель" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCliName2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 29 COLON-ALIGNED
          &ELSE AT ROW 5 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.agent-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 18 COLON-ALIGNED
          &ELSE AT ROW 6 COL 18 COLON-ALIGNED &ENDIF HELP
          "Код представителя доверенности"
          LABEL "Представитель" FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     fCliName3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 29 COLON-ALIGNED
          &ELSE AT ROW 6 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 15 COLON-ALIGNED
          &ELSE AT ROW 7.99 COL 15 COLON-ALIGNED &ENDIF HELP
          "Дата начала действия доверенности"
          LABEL "Начало" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 15 COLON-ALIGNED
          &ELSE AT ROW 9 COL 15 COLON-ALIGNED &ENDIF HELP
          "Дата окончания действия доверенности"
          LABEL "Окончание" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.single-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 72
          &ELSE AT ROW 8 COL 72 &ENDIF HELP
          "Отметка о разовой доверенности"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-loan.can-redraw-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 72
          &ELSE AT ROW 9 COL 72 &ENDIF HELP
          "Отметка о праве передоверия"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-loan.general-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 72
          &ELSE AT ROW 11 COL 72 &ENDIF HELP
          "Отметка о генеральной доверенности"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan.loan-allowed
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 14
          &ELSE AT ROW 12 COL 14 &ENDIF NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 64 BY 4
          &ELSE SIZE 64 BY 4 &ENDIF
     tt-loan.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 15 COLON-ALIGNED
          &ELSE AT ROW 11 COL 15 COLON-ALIGNED &ENDIF HELP
          "Дата закрытия договора"
          LABEL "Дата закрытия" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.comment
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 14
          &ELSE AT ROW 15.99 COL 14.01 &ENDIF HELP
          "Введите комментарий." NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 64 BY 4
          &ELSE SIZE 64 BY 4 &ENDIF
     "Отметка о разовой доверенности" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
          &ELSE SIZE 30 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 41
          &ELSE AT ROW 8 COL 41 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "Отметка о праве передоверия" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 44
          &ELSE AT ROW 9 COL 44 &ENDIF
     "Генеральная доверенность" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 47
          &ELSE AT ROW 11 COL 47 &ENDIF
     "В порядке передоверия" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 21 BY 1
          &ELSE SIZE 21 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 3
          &ELSE AT ROW 2 COL 3 &ENDIF
     "Вклады:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 2
          &ELSE AT ROW 12 COL 2 &ENDIF
     "прекращения" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 2
          &ELSE AT ROW 17 COL 2 &ENDIF
     "Основание" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 2
          &ELSE AT ROW 16 COL 2 &ENDIF
     "действия:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 2
          &ELSE AT ROW 18 COL 2 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD op-kind-allowed AS CHARACTER /* op-kind-allowed */
          FIELD agent-cat AS CHARACTER /* agent-cat */
          FIELD agent-id AS INT64 /* agent-id */
          FIELD RevRef2 AS CHARACTER /* RevRef2 */
          FIELD round AS LOGICAL /* round */
          FIELD TermKey AS CHARACTER /* TermKey */
          FIELD TicketNumber AS CHARACTER /* TicketNumber */
          FIELD UniformBag AS CHARACTER /* UniformBag */
          FIELD int-offset AS CHARACTER /* int-offset */
          FIELD IntAcct AS CHARACTER /* IntAcct */
          FIELD list_type AS CHARACTER /* list_type */
          FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
          FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
          FIELD OblAcct AS CHARACTER /* OblAcct */
          FIELD op-date AS DATE /* op-date */
          FIELD Op-kind_Acct AS CHARACTER /* Op-kind_Acct */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
          FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
          FIELD rate-list AS CHARACTER /* rate-list */
          FIELD rel_type AS CHARACTER /* rel_type */
          FIELD ReplDate AS DATE /* ReplDate */
          FIELD RevRef1 AS CHARACTER /* RevRef1 */
          FIELD rewzim$ AS CHARACTER /* Режим */
          FIELD sindkred$ AS LOGICAL /* СиндКред */
          FIELD BankCust AS CHARACTER /* BankCust */
          FIELD Bfnc AS CHARACTER /* Bfnc */
          FIELD CallAcct AS CHARACTER /* CallAcct */
          FIELD cred-offset AS CHARACTER /* cred-offset */
          FIELD dateend AS DATE /* dateend */
          FIELD DTKind AS CHARACTER /* DTKind */
          FIELD DTType AS CHARACTER /* DTType */
          FIELD Exec_D AS LOGICAL /* Exec_D */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
          FIELD grup_dog$ AS CHARACTER /* Груп_дог */
          FIELD datasogl$ AS DATE /* ДатаСогл */
          FIELD data_uar$ AS CHARACTER /* Дата_УАР */
          FIELD dop_proc$ AS CHARACTER /* Доп_Проц */
          FIELD dosroka$ AS CHARACTER /* ДоСРОКА */
          FIELD igndtokwc$ AS LOGICAL /* ИгнДтОкч */
          FIELD nesno$ AS CHARACTER /* НеснО */
          FIELD ovrpr$ AS INT64 /* ОврПр */
          FIELD ovrstop$ AS INT64 /* ОврСтоп */
          FIELD okrugsum$ AS LOGICAL /* ОкругСум */
          FIELD general-mark AS LOGICAL /* general-mark */
          FIELD loan-allowed AS CHARACTER /* loan-allowed */
          FIELD details AS CHARACTER /* details */
          FIELD can-redraw-mark AS LOGICAL /* can-redraw-mark */
          FIELD single-mark AS LOGICAL /* single-mark */
          FIELD create-date AS DATE /* create-date */
          FIELD drower-cat AS CHARACTER /* drower-cat */
          FIELD drower-id AS INT64 /* drower-id */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loan" "" }
          
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
         HEIGHT             = 21.14
         WIDTH              = 80.43
         MAX-HEIGHT         = 21.14
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21.14
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN tt-loan.agent-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR TOGGLE-BOX tt-loan.can-redraw-mark IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-loan.close-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR EDITOR tt-loan.comment IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR COMBO-BOX tt-loan.cont-type IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.create-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.cust-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.drower-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.end-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN fCliName2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCliName3 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fCliName3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tt-loan.general-mark IN FRAME fMain
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR EDITOR tt-loan.loan-allowed IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loan.loan-status IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.open-date IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX tt-loan.single-mark IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loan"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-loan.close-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.close-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.close-date IN FRAME fMain /* Дата закрытия */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.cust-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.cust-id TERMINAL-SIMULATION
ON LEAVE OF tt-loan.cust-id IN FRAME fMain /* Клиент */
,agent-id, drower-id
DO:
   /* В дальнейшем постаратться вынести содеожимое триггера в
      процедуру метода chkupd соответствующего реквизита 
   */
   {&BEG_BT_LEAVE}
   
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.end-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.end-date IN FRAME fMain /* Окончание */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fProxy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fProxy TERMINAL-SIMULATION
ON F1 OF fProxy IN FRAME fMain /* на основании */
DO:
  /* запустить форму для просмотра доверенности-основания */
  DEFINE VAR lContext  AS CHAR NO-UNDO.
  DEFINE VAR lCustId   AS CHAR NO-UNDO.
  DEFINE VAR lDrowerId AS CHAR NO-UNDO.
  DEFINE VAR lAgentId  AS CHAR NO-UNDO.
  DEFINE VAR lParent   AS CHAR NO-UNDO.
  DEFINE VAR lTitle    AS CHAR NO-UNDO.
  
  DEFINE BUFFER b-loan FOR loan.
  
  lContext  = tGetParam("Контекст",        "dps","").
  lCustId   = tGetParam("Cust-Id",         "dps","").
  lDrowerId = tGetParam("Drower-Id",       "dps","").
  lAgentId  = tGetParam("Agent-Id",        "dps","").
  lParent   = tGetParam("Parent-Cont-Code","dps","").

  FIND FIRST b-loan WHERE b-loan.contract  = "proxy"
                      AND b-loan.cont-code = TRIM(lParent)
                     NO-LOCK NO-ERROR.
  IF NOT AVAILABLE b-loan THEN RETURN NO-APPLY.
  
  lTitle = tGetParam("TITLE","dps","").
  RUN tSetParam IN h_tparam ("TITLE", "ДОВЕРЕННОСТЬ-ОСНОВАНИЕ", "dps","").
  
  RUN RunClassMethod(b-loan.class-code,
                     "Form",
                     "","",
                     "",
                     CHR(1)                                   + "," 
                                                              + "," +
                     b-loan.class-code                        + "," +
                     b-loan.contract + ";" + b-loan.cont-code + "," +
                     STRING({&MOD_VIEW})                      + "," +
                     STRING(iLevel + 1)) NO-ERROR.

  RUN tSetParam IN h_tparam ("Контекст",        lContext, "dps","").
  RUN tSetParam IN h_tparam ("Cust-Id",         lCustId,  "dps","").
  RUN tSetParam IN h_tparam ("Drower-Id",       lDrowerId,"dps","").
  RUN tSetParam IN h_tparam ("Agent-Id",        lAgentId, "dps","").
  RUN tSetParam IN h_tparam ("Parent-Cont-Code",lParent,  "dps","").
  RUN tSetParam IN h_tparam ("TITLE",           lTitle,   "dps","").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fRedraw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fRedraw TERMINAL-SIMULATION
ON LEAVE OF fRedraw IN FRAME fMain
DO:
   
   {&BEG_BT_LEAVE}
   
   RUN LocalFormActions(SELF).
   
   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
   RUN LocalFormBehavior(SELF).
   
   {&END_BT_LEAVE}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.loan-allowed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-allowed TERMINAL-SIMULATION
ON F1 OF tt-loan.loan-allowed IN FRAME fMain
DO:
   DEFINE BUFFER b-loan FOR loan.
   DEFINE VAR vLoans AS CHAR NO-UNDO.
   IF mParent <> ? AND mParent <> "" THEN DO:
      vLoans = GetXAttrValueEx("loan","proxy," + mParent, "loan-allowed","").
   END.

   DO TRANSACTION:
     pick-value = "".
     RUN browseld.p ("dep_person", 
                     "icust-cat" + CHR(1) + 
                     "icust-id"  + 
                     IF vLoans = "" THEN "" ELSE CHR(1) + "cont-code"
                     ,
                     "Ч" + CHR(1) +
                     STRING(tt-loan.cust-id)  + 
                     IF vLoans = "" THEN "" ELSE CHR(1) + vLoans
                     ,
                     ?,
                     2).

     FOR EACH tmprecid NO-LOCK:
         FIND FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
         DO:
            IF NOT {assigned SELF:SCREEN-VALUE} THEN
               SELF:SCREEN-VALUE = loan.cont-code.
            ELSE
               SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "," + loan.cont-code.
         END.
      END.

      ASSIGN tt-loan.loan-allowed.
      EMPTY TEMP-TABLE tmprecid.     /* Обнуление отметок. */
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.doc-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.doc-num TERMINAL-SIMULATION
ON F1 OF tt-loan.doc-num IN FRAME fMain /* Номер */
DO:
DEFINE VAR vCtr AS INT64 NO-UNDO.
   IF iMode NE {&MOD_VIEW} THEN
   DO TRANS:
      vCtr = GetCounterNextValue("Доверенности", TODAY).
      IF vCtr = ? THEN DO:
         MESSAGE "Невозможно сформировать номер доверенности,"
                 "т.к. в системе не настроена служба счетчиков."
                 "Для решения проблемы обратитесь к администратору системы."
                 VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ASSIGN 
        tt-loan.doc-num:SCREEN-VALUE = STRING(vCtr, "9999999999")
        tt-loan.doc-num
      .
   END.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-status TERMINAL-SIMULATION
ON F1 OF tt-loan.loan-status IN FRAME fMain /* СТАТУС */
DO:
   IF iMode NE {&MOD_VIEW} THEN
   DO TRANS:

      RUN "xstat(br.p" ("proxy", iLevel).
      IF LASTKEY EQ 10
      OR
      LASTKEY EQ 13 THEN
      IF {assigned pick-value} THEN
         SELF:SCREEN-VALUE = pick-value.
   END.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-loan.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-status TERMINAL-SIMULATION
ON LEAVE OF tt-loan.loan-status IN FRAME fMain
DO:
   IF tt-loan.loan-status:MODIFIED THEN
   DO:
       ASSIGN
          tt-loan.loan-status.
       IF    tt-loan.loan-status EQ CHR(251) 
          OR tt-loan.loan-status EQ "ОТЗ" THEN
          mFlagCloseDate = yes.            
   END.    
END.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.open-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.open-date IN FRAME fMain /* Начало */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
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

RUN StartBisTTY.

RUN tSetParam IN h_tparam ("Контекст","f-proxy","dps","").

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

   RUN LocalInitValues.
   
   /* Commented by KSV: Показываем экранную форму */
   STATUS DEFAULT "".
   RUN enable_UI.
   RUN BeforeEnableDisable.
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

RUN EndBisTTY.

RUN tDelParam IN h_tparam ("Контекст","dps","").

/* Commented by KSV: Выгружаем библиотеки */
{intrface.del}

/* Commented by KSV: Возвращаем значение вызывающей процедуре */
RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeEnableDisable TERMINAL-SIMULATION 
PROCEDURE BeforeEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:  Скрытие/показ полей в зависимости от контекста запуска формы   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT} THEN
   DO WITH FRAME {&MAIN-FRAME}:
      
      IF tt-loan.general-mark THEN
         tt-loan.loan-allowed:SENSITIVE = NO.
      
      /* Броузер отфильтрован по Вкладчику - отключить поле "Клиент" */
      IF mCustId <> ? THEN DO:
        tt-loan.cust-id:SENSITIVE   = NO.
      END.
      
      /* Поле Доверитель отключить всегда (оно либо равно вкладчику, либо берется из доверенности-основания) */
      tt-loan.drower-id:SENSITIVE = NO.
      
      /* Броузер отфильтрован по доверенности-основанию */
      IF mParent <> "*" AND mParent <> "" THEN 
         ASSIGN 
            fRedraw:SENSITIVE = NO
            fProxy:SENSITIVE  = TRUE
            fProxy:VISIBLE    = TRUE
            fProxy:READ-ONLY  = TRUE
         .
      
      
      ASSIGN 
         tt-loan.agent-id:SENSITIVE  = NO WHEN mAgentId  <> ?
         .
     
      IF mParent = "" AND tt-loan.parent-cont-code = "" THEN
          ASSIGN 
             fRedraw:SENSITIVE                  = NO
             fProxy:VISIBLE   = NO
             .
      
   END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY separator2 fRedraw separator1 fProxy fCliName1 fCliName2 fCliName3 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loan THEN 
    DISPLAY tt-loan.create-date tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type 
          tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id tt-loan.open-date 
          tt-loan.end-date tt-loan.single-mark tt-loan.can-redraw-mark 
          tt-loan.general-mark tt-loan.loan-allowed tt-loan.close-date 
          tt-loan.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE separator2 fRedraw separator1 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num 
         fProxy tt-loan.cont-type tt-loan.cust-id fCliName1 tt-loan.drower-id 
         tt-loan.agent-id tt-loan.open-date tt-loan.end-date 
         tt-loan.single-mark tt-loan.can-redraw-mark tt-loan.general-mark 
         tt-loan.loan-allowed tt-loan.close-date tt-loan.comment 
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
   DEF VAR vH AS HANDLE NO-UNDO.
   
   vH = FRAME {&MAIN-FRAME}:HANDLE.
   vH = vH:FIRST-CHILD. 
   vH = vH:FIRST-CHILD.
   
   DO WHILE VALID-HANDLE(vH):
      CREATE tFields.
      ASSIGN 
         tFields.tHandle    = vH
         tFields.tVisible   = vH:VISIBLE
         tFields.tSensitive = vH:SENSITIVE
         tFields.tName      = vH:NAME
         .
      vH = vH:NEXT-SIBLING.
   END.
   
   RUN BeforeEnableDisable.
   
   vH = FRAME {&MAIN-FRAME}:HANDLE.
   vH = vH:FIRST-CHILD. 
   vH = vH:FIRST-CHILD.
   DO WHILE VALID-HANDLE(vH):
      RUN LocalFormBehavior(vH).
      vH = vH:NEXT-SIBLING.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalFormActions TERMINAL-SIMULATION 
PROCEDURE LocalFormActions :
/*------------------------------------------------------------------------------
  Purpose:     Проверки заполнения полей
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM iSelf AS HANDLE NO-UNDO.

   DEF VAR vCliExist AS LOG    NO-UNDO.
   DEF VAR vCliName  AS CHAR   NO-UNDO.
   DEF VAR vH        AS HANDLE NO-UNDO.
   DEF VAR vId       AS INT64    NO-UNDO.
   DEF VAR vName     AS CHAR   NO-UNDO.
   DEF VAR vSurr     AS CHAR   NO-UNDO.

   DEF BUFFER loan FOR loan.

   vH = iSelf.

   CASE iSelf:NAME:
      WHEN "cust-id"   OR
      WHEN "agent-id"  OR
      WHEN "drower-id" THEN
      DO WITH FRAME {&MAIN-FRAME}:
         
         IF NOT({assignex iSelf:SCREEN-VALUE}) AND iSelf:NAME <> "agent-id" THEN
            RETURN "Заполните поле".
         
         vCliExist = CliExist("Ч",iSELF:SCREEN-VALUE).

         IF NOT vCliExist THEN DO:
            IF iSELF:SCREEN-VALUE = "" OR iSELF:SCREEN-VALUE = "0" OR
               iSELF:SCREEN-VALUE = ?  OR iSELF:SCREEN-VALUE = "?" THEN
              RETURN "Укажите представителя по доверенности".
            RETURN "Нет физического лица с таким кодом".
         END.
         
         vCliName = LN_GetClient("Ч",INT64(iSELF:SCREEN-VALUE)).
         IF iSELF:NEXT-TAB-ITEM:NAME MATCHES "*CliName*" THEN
            iSELF:NEXT-TAB-ITEM:SCREEN-VALUE = vCliName.
         /* если не отмечено поле в порядке передоверия
            то клиент совпадает доверителем */
         IF iSELF:NAME = "drower-id" AND
            fRedraw:SCREEN-VALUE = "NO"
         THEN
            ASSIGN
               tt-loan.cust-id              = INT64(iSELF:SCREEN-VALUE)
               tt-loan.cust-id:SCREEN-VALUE = iSELF:SCREEN-VALUE
               fCliName1:SCREEN-VALUE       = fCliName2:SCREEN-VALUE
               .
         IF iSELF:NAME = "drower-id"                 AND
            fRedraw:SCREEN-VALUE = "NO"              AND 
            tt-loan.cust-id:SENSITIVE = NO           AND 
            ({assignex tt-loan.cust-id:INPUT-VALUE}) AND 
            ({assignex iSelf:INPUT-VALUE})           AND 
            tt-loan.cust-id:INPUT-VALUE <> iSelf:INPUT-VALUE
         THEN
            RETURN "Доверитель и клиент должны быть одним лицом".
      END.
      WHEN "fRedraw" THEN
      DO WITH FRAME {&MAIN-FRAME}:
         IF iSelf:SCREEN-VALUE = "NO" THEN
         DO:
            ASSIGN
               tt-loan.parent-cont-code = ""
               tt-loan.parent-contract  = ""
               fRedraw:SCREEN-VALUE = "" 
               .
            IF  tt-loan.cust-id:SENSITIVE = NO             AND 
                tt-loan.drower-id:SENSITIVE = NO           AND 
                ({assignex tt-loan.drower-id:INPUT-VALUE}) AND 
                ({assignex tt-loan.cust-id:INPUT-VALUE})   AND 
                tt-loan.cust-id:INPUT-VALUE <> tt-loan.drower-id:INPUT-VALUE
            THEN
               RETURN "Клиент и представитель различные люди ~n" + 
                      "Установите признак в порядке передоверия ~n" + 
                      "или отмените ввод".
            IF tt-loan.cust-id:SENSITIVE = YES THEN
               ASSIGN 
                  tt-loan.cust-id:SCREEN-VALUE = tt-loan.drower-id:SCREEN-VALUE
                  fCliName1:SCREEN-VALUE       = LN_GetClient("Ч",INT64(tt-loan.cust-id:SCREEN-VALUE)) 
                  fCliName2:SCREEN-VALUE       = LN_GetClient("Ч",INT64(tt-loan.cust-id:SCREEN-VALUE))
                  . 

         END.
      END.
      WHEN "fProxy" THEN
      DO:
         /*
         IF NOT {assignex fProxy:SCREEN-VALUE} THEN
            RETURN "Укажите доверенность основание".

         /* В лом было писать динамический запрос */
         RUN RE_B_LOAN IN h_Loan("proxy",fProxy:INPUT-VALUE,BUFFER loan).

         IF NOT AVAIL loan THEN
            RETURN "Нет доверенности с таким кодом".

         IF tGetParam("Контекст","dps","") = "f-proxy"  AND
            BT_Modify(iSELF)                             AND
            (iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT})
         THEN
         DO:
            vSurr = "proxy," + fProxy:INPUT-VALUE.

            vId = INT64(GetXattrValue("loan",vSurr,"agent-id")) NO-ERROR.

            IF NOT ERROR-STATUS:ERROR AND
               vId <> 0               AND
               vId <> ?
            THEN DO:
               vName = LN_GetClient("Ч",INT64(vId)).
               DO WHILE VALID-HANDLE(vH):
                  IF vH:NAME = "drower-id" AND vH:SENSITIVE THEN
                  DO:
                     vH:SCREEN-VALUE = STRING(vId).
                     IF vH:NEXT-TAB-ITEM:NAME MATCHES "*CliName*" THEN
                        vH:NEXT-TAB-ITEM:SCREEN-VALUE = vName.
                     LEAVE.
                  END.
                  vH = vH:NEXT-SIBLING.
               END.
            END.
         END.
         */
      END.
      WHEN "open-date" THEN
      DO:
         IF DATE(iSelf:SCREEN-VALUE)               <> ? AND
            DATE(tt-loan.create-date:SCREEN-VALUE) <> ? AND
            DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.create-date:SCREEN-VALUE)   
         THEN
            RETURN "Дата начала не может быть меньше даты оформления".
         IF DATE(iSelf:SCREEN-VALUE)               <> ? AND
            mMinDate                               <> ? AND
            DATE(iSelf:SCREEN-VALUE)  < mMinDate  
         THEN DO:
             iSelf:SCREEN-VALUE = STRING(mMinDate).
             RETURN "Доверенность выдается на праве передоверия. Действие доверенности не может начаться раньше начала действия основной доверенности. Исправляю.".
         END.
      END.
      WHEN "end-date" THEN
      DO:
        IF DATE(iSelf:SCREEN-VALUE) = ? THEN RETURN "Укажите окончание периода действия доверенности".
        
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           DATE(tt-loan.open-date:SCREEN-VALUE)  <> ? AND 
           DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.open-date:SCREEN-VALUE)   
        THEN
           RETURN "Дата окончания не может быть меньше даты начала".
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           mMaxDate                              <> ? AND
           DATE(iSelf:SCREEN-VALUE)  > mMaxDate   
        THEN DO:
            iSelf:SCREEN-VALUE = STRING(mMaxDate).
            RETURN "Доверенность выдается на праве передоверия. Действие доверенности не может закончиться позже, чем заканчивается основная доверенность. Исправляю.".
        END.
      END.
      WHEN "close-date" THEN
      DO:
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           DATE(tt-loan.open-date:SCREEN-VALUE)  <> ? AND 
           DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.open-date:SCREEN-VALUE)   
        THEN
           RETURN "Дата закрытия не может быть меньше даты начала".
      END.

   END CASE.

   RETURN.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalFormBehavior TERMINAL-SIMULATION 
PROCEDURE LocalFormBehavior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM iH AS HANDLE NO-UNDO.
   
   FIND FIRST tFields WHERE tFields.tHandle = iH NO-ERROR.
   IF NOT AVAIL tFields THEN RETURN.
   
   DO WITH FRAME {&MAIN-FRAME}:
   
   CASE tFields.tName:
      WHEN "can-redraw-mark" THEN DO:
        IF mParent <> "" AND mParent <> ? AND mParent <> "*" THEN DO:
          tt-loan.can-redraw:SENSITIVE = NO.
        END.
      END.
      WHEN "fRedraw" THEN
      DO:
         IF iH:SCREEN-VALUE = "NO" THEN
         DO:
            ASSIGN 
               fProxy:VISIBLE = NO WHEN tt-loan.parent-cont-code = "" 
               tt-loan.cust-id:SENSITIVE        = NO WHEN iMode <> {&MOD_VIEW}
               .
            
         END.
         ELSE 
         DO:
         
            ASSIGN 
               fProxy:VISIBLE = YES WHEN tFields.tVisible
               tt-loan.cust-id:SENSITIVE        = YES WHEN tFields.tVisible   AND 
                                                           tFields.tSensitive AND
                                                           iMode <> {&MOD_VIEW}
               .
         END.
      END.
   END CASE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalInitValues TERMINAL-SIMULATION 
PROCEDURE LocalInitValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER b-loan FOR loan.

   mAgentId  = INT64(TRIM(tGetParam("Agent-Id","dps","")))  NO-ERROR.
   mDrowerId = INT64(TRIM(tGetParam("Drower-Id","dps",""))) NO-ERROR. 
   mCustId   = INT64(TRIM(tGetParam("Cust-Id","dps","")))   NO-ERROR.
   mDate     = DATE(tGetParam("p-date","dps",""))         NO-ERROR.
   ASSIGN 
      mParent   = tGetParam("Parent-Cont-Code","dps","")
      mDate     = TODAY WHEN mDate = ?
      mAgentId  = ? WHEN mAgentId  = 0
      mDrowerId = ? WHEN mDrowerId = 0
      mCustId   = ? WHEN mCustId   = 0
   .
   
   IF mParent <> ? AND mParent <> "*" THEN DO:
      FIND FIRST b-loan WHERE b-loan.contract  = "proxy" 
                          AND b-loan.cont-code = mParent
                        NO-LOCK NO-ERROR.
      IF AVAILABLE b-loan THEN DO:
         mCustID   = b-loan.cust-id.
         mDrowerID = INT64(GetXAttrValue("loan", b-loan.contract + "," + b-loan.cont-code, "drower-id")).
         mMinDate  = b-loan.open-date.
         mMaxDate  = b-loan.end-date.
      END.
   END.
   
   ASSIGN    
      tt-loan.parent-cont-code = mParent WHEN mParent <> "*"  AND 
                                              mParent <> ""   AND  
                                              iMode   = {&MOD_ADD}  
      fProxy = "доверенности"
      tt-loan.general-mark    = NO WHEN tt-loan.general-mark    = ? 
      tt-loan.single-mark     = NO WHEN tt-loan.single-mark     = ? 
      tt-loan.can-redraw-mark = NO WHEN tt-loan.can-redraw-mark = ? 
   .
   IF iMode = {&MOD_ADD} THEN
   ASSIGN 
         tt-loan.create-date     = tt-loan.open-date
         tt-loan.general-mark    = NO WHEN tt-loan.general-mark    = ? 
         tt-loan.single-mark     = NO WHEN tt-loan.single-mark     = ? 
         tt-loan.can-redraw-mark = NO WHEN tt-loan.can-redraw-mark = ? 
         tt-loan.agent-id        = mAgentId  WHEN mAgentId  <> ? 
         tt-loan.drower-id       = mDrowerId WHEN mDrowerId <> ?
         tt-loan.cust-id         = IF mCustId <> ? THEN mCustId ELSE mDrowerId.
   .

   IF iMode = {&MOD_ADD} THEN
   DO:
      IF tt-loan.parent-cont-code <> "" THEN 
      DO:
         tt-loan.drower-id =INT64(GetXattrValue(
                  "loan",
                  "proxy," + tt-loan.parent-cont-code,
                  "agent-id")) NO-ERROR.

      END.

      IF tt-loan.general-mark THEN 
         ASSIGN 
             tt-loan.loan-allowed = "*"
             .
      IF mMinDate <> ? THEN tt-loan.open-date = mMinDate.
      IF mMaxDate <> ? THEN tt-loan.end-date  = mMaxDate.
   END.
   IF fRedraw = NO AND iMode = {&MOD_ADD} THEN
      ASSIGN 
         tt-loan.drower-id = tt-loan.cust-id WHEN mCustId <> ? AND 
                                                  mDrowerId = ?
         .

   ASSIGN 
      fCliName1 = LN_GetClient("Ч",tt-loan.cust-id)
      fCliName2 = LN_GetClient("Ч",tt-loan.drower-id)
      fCliName3 = LN_GetClient("Ч",tt-loan.agent-id)
      fRedraw   = YES WHEN tt-loan.parent-cont-code <> ""
      .

   ASSIGN
     fCliName1 = IF fCliName1 = ? OR fCliName1 = "?" THEN "" ELSE fCliName1
     fCliName2 = IF fCliName2 = ? OR fCliName2 = "?" THEN "" ELSE fCliName2
     fCliName3 = IF fCliName3 = ? OR fCliName3 = "?" THEN "" ELSE fCliName3
   .
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
   IF iMode = {&MOD_ADD} AND NOT mBT_IsTrans THEN DO:
       tt-loan.cont-code = NextContCode("PROXY",16).
       /*tt-loan.close-date = tt-loan.end-date.*/
   END.
   
   IF     mFlagCloseDate
      AND iMode EQ {&MOD_EDIT} THEN
      tt-loan.close-date = tt-loan.end-date.

   IF {assignex tt-loan.parent-cont-code} THEN
      tt-loan.parent-contract = "proxy".
   ELSE 
      tt-loan.parent-contract = "".

END PROCEDURE.

PROCEDURE LocalPutTitle:
DEFINE VAR vTitle AS CHAR NO-UNDO.
   IF tGetParam("TITLE", "dps","") = ""  OR tGetParam("TITLE", "dps","") = ? OR 
      tGetParam("TITLE", "dps","") = "?" THEN RETURN.
   FRAME fMain:TITLE = "[" + tGetParam("TITLE", "dps","") + "]".
   RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='11/08/2015 12:02:03.355+04:00' */
/* $LINTUSER='fiyu' */
/* $LINTMODE='1' */
/* $LINTFILE='f-trust.p' */
/*prosigndmc5Hp+sKJDEBJjBJFlFMg*/