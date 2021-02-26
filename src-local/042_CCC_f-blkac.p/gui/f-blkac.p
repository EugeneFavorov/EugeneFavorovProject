{globals.i}
{intrface.get tmess}

/* +++ f-blkac.p was humbly modified by (c)blodd converter v.1.09 on 12/2/2016 11:50am +++ */

&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-BlockAcct NO-UNDO LIKE BlockObject
       FIELD datarewspr$     AS DATE      /* ДатаРешПр */
       FIELD datarewsot$     AS DATE      /* ДатаРешОт */       
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-BlockAcct" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-BLKAC.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 27.11.2007 19:10 ilvi    
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
{intrface.get instrum}
CREATE WIDGET-POOL.
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

DEFINE VARIABLE mFile      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurrogate AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNum       AS INT64   NO-UNDO.
DEFINE VARIABLE mTmpStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNO_OT  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNoOK   AS LOGICAL   NO-UNDO INIT YES.

/* "Приемник" возвращаемых значений. */
{ttretval.def}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-BlockAcct

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.datarewsot$~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-BlockAcct
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-BlockAcct
&Scoped-define QUERY-STRING-fMain FOR EACH tt-BlockAcct SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-BlockAcct SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-BlockAcct
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-BlockAcct


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define ENABLED-TABLES tt-BlockAcct
&Scoped-define FIRST-ENABLED-TABLE tt-BlockAcct
&Scoped-Define DISPLAYED-FIELDS tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define DISPLAYED-TABLES tt-BlockAcct
&Scoped-define FIRST-DISPLAYED-TABLE tt-BlockAcct


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-BlockAcct.block-type tt-BlockAcct.beg-datetime ~
tt-BlockAcct.val[1] tt-BlockAcct.val[2] tt-BlockAcct.val[3] ~
tt-BlockAcct.txt[1] tt-BlockAcct.user-id tt-BlockAcct.txt[2] ~
tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] ~
tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define List-2 tt-BlockAcct.end-datetime tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8]
&Scoped-define List-3 tt-BlockAcct.block-type tt-BlockAcct.beg-datetime ~
tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] ~
tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id ~
tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT ~
tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define List-4 tt-BlockAcct.val[1] tt-BlockAcct.val[2] ~
tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.txt[2] ~
tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] ~
tt-BlockAcct.txt[8] tt-BlockAcct.val[4]

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-BlockAcct SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-BlockAcct.block-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 21 COLON-ALIGNED
          &ELSE AT ROW 1 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.beg-datetime
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 21 COLON-ALIGNED
          &ELSE AT ROW 2 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.end-datetime
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 21 COLON-ALIGNED
          &ELSE AT ROW 3 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 21 COLON-ALIGNED
          &ELSE AT ROW 4 COL 21 COLON-ALIGNED &ENDIF HELP
          "Сумма по дебету"
          LABEL "Сумма по дебету"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 21 COLON-ALIGNED
          &ELSE AT ROW 5 COL 21 COLON-ALIGNED &ENDIF HELP
          "Сумма по кредиту"
          LABEL "Сумма по кредиту"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[3]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 21 COLON-ALIGNED
          &ELSE AT ROW 6 COL 21 COLON-ALIGNED &ENDIF HELP
          "Лимит остатка"
          LABEL "Лимит  остатка"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[4]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 57 COLON-ALIGNED
          &ELSE AT ROW 6 COL 57 COLON-ALIGNED &ENDIF HELP
          "Рублевый эквививалент"
          LABEL "Руб.эквив."
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 19 BY 1
          &ELSE SIZE 19 BY 1 &ENDIF
     tt-BlockAcct.txt[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 10
          &ELSE AT ROW 7 COL 10 &ENDIF HELP
          "Очередность"
          LABEL "Очередность"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
     tt-BlockAcct.user-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 21 COLON-ALIGNED
          &ELSE AT ROW 8 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
     tt-BlockAcct.txt[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 12
          &ELSE AT ROW 10 COL 12 &ENDIF HELP
          "     Орган"
          LABEL "     Орган"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[3]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 5
          &ELSE AT ROW 11 COL 5 &ENDIF HELP
          "Тип постановления"
          LABEL "Тип постановления"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[4]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 9
          &ELSE AT ROW 12 COL 9 &ENDIF HELP
          "Постановление" FORMAT "x(600)"
          LABEL "Постановление"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
      mKodNO
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 47
          &ELSE AT ROW 12 COL 47 &ENDIF HELP
          "Код органа" FORMAT "x(4)"
          LABEL "Код органа"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
      
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-BlockAcct.datarewspr$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 66 COLON-ALIGNED
          &ELSE AT ROW 12 COL 66 COLON-ALIGNED &ENDIF
          LABEL "От" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-BlockAcct.txt[5]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 12
          &ELSE AT ROW 14 COL 12 &ENDIF HELP
          "     Орган"
          LABEL "     Орган"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[6]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 5
          &ELSE AT ROW 15 COL 5 &ENDIF HELP
          "Тип постановления"
          LABEL "Тип постановления"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[7]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 9
          &ELSE AT ROW 16 COL 9 &ENDIF HELP
          "Постановление" FORMAT "x(600)"
          LABEL "Постановление"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
      mKodNO_OT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 47
          &ELSE AT ROW 16 COL 47 &ENDIF HELP
          "Код органа" FORMAT "x(4)"
          LABEL "Код органа"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF

          tt-BlockAcct.datarewsot$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 66 COLON-ALIGNED
          &ELSE AT ROW 16 COL 66 COLON-ALIGNED &ENDIF
          LABEL "От" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
          
     tt-BlockAcct.txt[8]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 8
          &ELSE AT ROW 17 COL 8 &ENDIF
          LABEL "Доп.информация" FORMAT "x(120)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 54 BY 1
          &ELSE SIZE 54 BY 1 &ENDIF
     "Информация о снятии ограничений" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 1
          &ELSE SIZE 39 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 2
          &ELSE AT ROW 13 COL 2 &ENDIF
     "Информация об установлении ограничений" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 76 BY 1
          &ELSE SIZE 76 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 2
          &ELSE AT ROW 9 COL 2 &ENDIF
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
      TABLE: tt-BlockAcct T "?" NO-UNDO bisquit BlockObject
      ADDITIONAL-FIELDS:
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-BlockAcct" "" }
          
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
/* SETTINGS FOR FILL-IN tt-BlockAcct.beg-datetime IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.block-type IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.datarewspr$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN tt-BlockAcct.end-datetime IN FRAME fMain
   2 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[1] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[2] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[3] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[4] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[5] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[6] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[7] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[8] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL                                            */
/* SETTINGS FOR FILL-IN tt-BlockAcct.user-id IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[1] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[2] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[3] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[4] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-BlockAcct"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME mKodNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo TERMINAL-SIMULATION
ON F1 OF mKodNo IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "КонтрОрган" 
      AND   code.parent EQ "КонтрОрган" 
      AND   code.code   EQ tt-BlockAcct.txt[2]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      IF iMode NE {&MOD_VIEW} 
      THEN DO:
         {empty ttRetVal}
         RUN browseld.p ("code",
                         "class~001parent~001RetFld~001RetType~001RetRcp",
                         TRIM(code.val) + "~001" + TRIM(code.val) + 
                         "~001code~001Singl~001" + 
                         STRING(TEMP-TABLE ttRetVal:HANDLE),
                         "class",
                         iLevel + 1).
         FOR EACH ttRetVal:
            SELF:SCREEN-VALUE = ttRetVal.PickValue.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME mKodNo_OT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo_OT TERMINAL-SIMULATION
ON F1 OF mKodNo_OT IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "КонтрОрган" 
      AND   code.parent EQ "КонтрОрган" 
      AND   code.code   EQ tt-BlockAcct.txt[5]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      IF iMode NE {&MOD_VIEW} 
      THEN DO:
         {empty ttRetVal}
         RUN browseld.p ("code",
                         "class~001parent~001RetFld~001RetType~001RetRcp",
                         TRIM(code.val) + "~001" + TRIM(code.val) + 
                         "~001code~001Singl~001" + 
                         STRING(TEMP-TABLE ttRetVal:HANDLE),
                         "class",
                         iLevel + 1).
         FOR EACH ttRetVal:
            SELF:SCREEN-VALUE = ttRetVal.PickValue.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[1] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[1] IN FRAME fMain /* Очередность */
DO:
   IF iMode NE {&MOD_VIEW} 
   THEN DO :
      {empty ttRetVal}
      RUN browseld.p ("code",
                      "class~001parent~001RetFld~001RetType~001RetRcp",
                      "order-pay~001order-pay~001code~001Multi~001" + STRING(TEMP-TABLE ttRetVal:HANDLE),
                      "class",
                      iLevel + 1).
      FOR EACH ttRetVal:
         {additem.i SELF:SCREEN-VALUE ttRetVal.PickValue}
      END.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.block-type IN FRAME fMain /* Очередность */
DO:
   mTmpStr = REPLACE(GetCode("acct-status",SELF:SCREEN-VALUE),"ОчПлат(","").
   mTmpStr = REPLACE(mTmpStr,")","").
   tt-BlockAcct.txt[1]:SCREEN-VALUE = mTmpStr.

   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.block-type IN FRAME fMain /* Очередность */
DO:
   {&BEG_BT_LEAVE}

   ASSIGN tt-BlockAcct.block-type.

   FIND FIRST code WHERE
       code.class EQ "acct-status"
   AND code.code  EQ tt-BlockAcct.block-type
   NO-LOCK NO-ERROR.
   IF AVAIL(code) THEN
   DO:
      IF NOT GetPermission(
         "code",
         code.class + "," + tt-BlockAcct.block-type,
         "w")
      THEN
      DO:
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.

   IF INDEX(SELF:SCREEN-VALUE,"*")  > 0 THEN DO:

      RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                  "Тип блокировки содержит недопустимый символ '*'!"). 
      {return_no_apply.i '{&RET-ERROR}'}

   END.
   {&END_BT_LEAVE}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.end-datetime IN FRAME fMain /* Очередность */
DO:
   {&BEG_BT_LEAVE}

   ASSIGN tt-BlockAcct.end-datetime.

   FIND FIRST code WHERE
       code.class EQ "acct-status"
   AND code.code  EQ tt-BlockAcct.block-type:SCREEN-VALUE
   NO-LOCK NO-ERROR.
   IF AVAIL(code) 
   AND {assigned tt-BlockAcct.end-datetime:SCREEN-VALUE}THEN
   DO:
      IF NOT GetPermission(
         "code",
         code.class + "," + tt-BlockAcct.block-type:SCREEN-VALUE,
         "w")
      THEN
      DO:
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.

   {&END_BT_LEAVE}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.beg-datetime TERMINAL-SIMULATION
ON ENTRY OF tt-BlockAcct.beg-datetime IN FRAME fMain
DO:
   IF NOT {assigned SELF:SCREEN-VALUE} THEN
      SELF:SCREEN-VALUE = STRING(TODAY, "99/99/9999") + " " + STRING(TIME, "HH:MM").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[1] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[1] IN FRAME fMain /* Очередность */
DO:
   {&BEG_BT_LEAVE}
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      DO mNum = 1 TO NUM-ENTRIES(SELF:SCREEN-VALUE):
         IF GetCodeName("order-pay",
                        ENTRY(mNum,SELF:SCREEN-VALUE))EQ ? THEN 
         DO:
             RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                         "Очередность платежа с кодом " + ENTRY(mNum,SELF:SCREEN-VALUE) + " отсутствует!"). 
             {return_no_apply.i '{&RET-ERROR}'}
         END.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo TERMINAL-SIMULATION
ON LEAVE OF mKodNo IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "КонтрОрган" 
      AND   code.parent EQ "КонтрОрган" 
      AND   code.code   EQ tt-BlockAcct.txt[2]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      {&BEG_BT_LEAVE}
      IF {assigned SELF:SCREEN-VALUE} THEN DO:
            IF GetCodeName(TRIM(code.val),
                           SELF:SCREEN-VALUE)EQ ? THEN 
            DO:
                RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                            "Орган " + 
                                            tt-BlockAcct.txt[2]:SCREEN-VALUE + 
                                            " с кодом " + SELF:SCREEN-VALUE + 
                                            " отсутствует!").
                mKodNoOK = NO. 
                {return_no_apply.i '{&RET-ERROR}'}
            END.
            ELSE mKodNoOK = YES.
      END.
      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo_OT TERMINAL-SIMULATION
ON LEAVE OF mKodNo_OT IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "КонтрОрган" 
      AND   code.parent EQ "КонтрОрган" 
      AND   code.code   EQ tt-BlockAcct.txt[5]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      {&BEG_BT_LEAVE}
      IF {assigned SELF:SCREEN-VALUE} THEN DO:
            IF GetCodeName(TRIM(code.val),
                           SELF:SCREEN-VALUE)EQ ? THEN 
            DO:
                RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                            "Орган " + 
                                            tt-BlockAcct.txt[5]:SCREEN-VALUE + 
                                            " с кодом " + SELF:SCREEN-VALUE + 
                                            " отсутствует!").
                mKodNoOK = NO. 
                {return_no_apply.i '{&RET-ERROR}'}
            END.
            ELSE mKodNoOK = YES.
      END.
      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[2] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[2] IN FRAME fMain /* Код органа */
DO:
  IF {assigned SELF:SCREEN-VALUE} THEN DO:
     RUN shifr.p("КонтрОрган",SELF:SCREEN-VALUE,iLevel + 1).
  END.
  ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
      RUN codelay.p("КонтрОрган",?,?,iLevel + 1).
      IF     {&LAST_KEY} = 10 
         AND pick-value NE ? THEN
        SELF:SCREEN-VALUE = pick-value. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[3] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[3] IN FRAME fMain /* Тип постановления */
DO:
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       RUN shifr.p("Предписания",SELF:SCREEN-VALUE,iLevel + 1).
    END.
    ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
        RUN codelay.p("Предписания",?,?,iLevel + 1).
        IF     {&LAST_KEY} = 10 
           AND pick-value NE ? THEN
          SELF:SCREEN-VALUE = pick-value. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[3] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[3] IN FRAME fMain /* Тип постановления */
DO:
   {&BEG_BT_LEAVE}
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      IF GetCodeName("Предписания",
                     SELF:SCREEN-VALUE)EQ ? THEN 
      DO:
          RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                      "Тип предписания с кодом " + SELF:SCREEN-VALUE + " отсутствует!"). 
          {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-BlockAcct.txt[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[5] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[5] IN FRAME fMain /* Код органа */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      RUN shifr.p("КонтрОрган",SELF:SCREEN-VALUE,iLevel + 1).
   END.
   ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
      RUN codelay.p("КонтрОрган",?,?,iLevel + 1).
      IF     {&LAST_KEY} = 10 
         AND pick-value NE ? THEN
         SELF:SCREEN-VALUE = pick-value. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-BlockAcct.txt[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[6] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[6] IN FRAME fMain /* Тип постановления */
DO:
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       RUN shifr.p("Предписания",SELF:SCREEN-VALUE,iLevel + 1).
    END.
    ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
        RUN codelay.p("Предписания",?,?,iLevel + 1).
        IF     {&LAST_KEY} = 10 
           AND pick-value NE ? THEN
          SELF:SCREEN-VALUE = pick-value. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[6] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[6] IN FRAME fMain /* Тип постановления */
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       IF GetCodeName("Предписания",
                      SELF:SCREEN-VALUE)EQ ? THEN 
       DO:
           RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                       "Тип предписания с кодом " + SELF:SCREEN-VALUE + " отсутствует!"). 
           {return_no_apply.i '{&RET-ERROR}'}
       END.
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.val[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  tt-BlockAcct.val TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.val[3] IN FRAME fMain
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} AND iMode EQ {&MOD_ADD} THEN DO:
         IF ENTRY(2,tt-blockacct.surrogate) EQ "" 
         THEN tt-BlockAcct.val[4] = tt-BlockAcct.val[3].
         ELSE tt-BlockAcct.val[4] = ROUND(100 * DEC(tt-BlockAcct.val[3]:SCREEN-VALUE)
                                              / CurFromBase("УЧЕТНЫЙ", 
                                                              ENTRY(2,tt-blockacct.surrogate), 
                                                              gend-date, 
                                                              100),2).
         tt-BlockAcct.val[4]:SCREEN-VALUE = STRING(tt-BlockAcct.val[4]).
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.val[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  tt-BlockAcct.val[4] TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.val[4] IN FRAME fMain
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} AND iMode EQ {&MOD_ADD} THEN DO:
         IF ENTRY(2,tt-blockacct.surrogate) EQ "" 
         THEN tt-BlockAcct.val[3] = DEC(tt-BlockAcct.val[4]:SCREEN-VALUE).
         ELSE tt-BlockAcct.val[3] = ROUND(CurFromBase("УЧЕТНЫЙ",
                                                          ENTRY(2,tt-blockacct.surrogate), 
                                                          gend-date, 
                                                          DEC(tt-BlockAcct.val[4]:SCREEN-VALUE)),2).
         tt-BlockAcct.val[3]:SCREEN-VALUE = STRING(tt-BlockAcct.val[3]).
         tt-BlockAcct.val[4] = DEC(tt-BlockAcct.val[4]:SCREEN-VALUE).
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(IF_DEFINED_THEN_THIS_BLOCK_CAN_BE_DELETED) EQ 0 &THEN
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
   {return_no_apply.i}
END.
ON ENDKEY, END-ERROR OF FRAME fMain ANYWHERE DO:
   mRetVal = IF mOnlyForm THEN
      {&RET-ERROR}
      ELSE 
         "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   {return_no_apply.i}
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
      mFile      = ENTRY(2, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 1
      mSurrogate = ENTRY(3, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 2
   .
   IF iMode = {&MOD_ADD} THEN
      ASSIGN
         tt-blockacct.FILE      = mFile
         tt-blockacct.surrogate = mSurrogate
      .

   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Подсветка полей из LIST-5 (настроить для себя )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

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
   {wait_for.i 
      &THIS_FRAME  = "fMain"
      &ENTRY_FOCUS = "mFirstTabItem"
      &EXTEXT      = "CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem"
      {&*}}.
END.

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

RUN disable_ui.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
PROCEDURE Local_go :
   /* izm SAM */
   DEFINE BUFFER code FOR code.

   FOR EACH code WHERE code.class  EQ "acct-status"
                   AND code.parent EQ "acct-status"
                   AND code.code   EQ tt-BlockAcct.block-type:SCREEN-VALUE IN FRAME fMain NO-LOCK:
      IF code.misc[1] EQ "БлокСумм" AND
         DECIMAL(tt-BlockAcct.val[3]:SCREEN-VALUE IN FRAME fMain) GE 0 THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                     "Для данного типа блокировки в поле лимит остатка" + CHR(10) +
                                     "сумма должна быть отрицательной!").
         RETURN ERROR.
      END.
   END.
     
   /* end izm SAM */

    IF tt-BlockAcct.block-type:SCREEN-VALUE IN FRAME fMain = "БлокСумм" AND
       DECIMAL(tt-BlockAcct.val[3]:SCREEN-VALUE IN FRAME fMain) = 0 THEN DO:
          RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                    "Для блокировки типа \"БлокСумм\" " +
                                    "сумма блокировки не может быть нулевой").
        RETURN ERROR.
    END.

   DO WITH FRAME fMain:
      IF mKodNoOK THEN
      ASSIGN
         tt-BlockAcct.txt[4]:SCREEN-VALUE = tt-BlockAcct.txt[4]:SCREEN-VALUE + ";" + mKodNo:SCREEN-VALUE
         tt-BlockAcct.txt[7]:SCREEN-VALUE = tt-BlockAcct.txt[7]:SCREEN-VALUE + ";" + mKodNo_OT:SCREEN-VALUE
      .   
   END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject:
   DO WITH FRAME fMain:      
      IF NUM-ENTRIES(tt-BlockAcct.txt[4],";") GE 2 THEN DO:
         mKodNo    = ENTRY(2,tt-BlockAcct.txt[4],";").
         DISPLAY
            mKodNo
         .
      END.
      IF NUM-ENTRIES(tt-BlockAcct.txt[7],";") GE 2 THEN DO:
         mKodNo_OT = ENTRY(2,tt-BlockAcct.txt[7],";").
         DISPLAY
            mKodNo
         .
      END.
      DISPLAY
         ENTRY(1,tt-BlockAcct.txt[4],";") @ tt-BlockAcct.txt[4]
         ENTRY(1,tt-BlockAcct.txt[7],";") @ tt-BlockAcct.txt[7]
      .
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
  IF AVAILABLE tt-BlockAcct THEN 
    DISPLAY tt-BlockAcct.block-type tt-BlockAcct.beg-datetime 
          tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] 
          tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id 
          tt-BlockAcct.txt[2] tt-BlockAcct.txt[3]  mKodNo mKodNo_OT 
          tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] 
          tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-BlockAcct.block-type tt-BlockAcct.beg-datetime 
         tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] 
         tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id 
         tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT
         tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] 
         tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-blkac.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='06/04/2016 10:53:18.490+04:00' */
/*prosignjoEu5bYgzoEXtmv8Fg2Gcg*/
/* --- f-blkac.p was humbly modified by (c)blodd converter v.1.09 on 12/2/2016 11:50am --- */
