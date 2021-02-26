{globals.i}

/* +++ f-l-gar.p was humbly modified by (c)blodd converter v.1.11 on 6/27/2017 2:41pm +++ */

&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION

DEF NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-term-obl-gar NO-UNDO LIKE term-obl
       FIELD valuwcetaobesp$ AS CHARACTER /* ВалУчетаОбесп */
       FIELD viddogob$ AS CHARACTER /* ВидДогОб */
       FIELD vidob$ AS CHARACTER /* ВидОб */
       FIELD datapost$ AS DATE /* ДатаПост */
       FIELD dopinfo$ AS CHARACTER /* ДопИнфо */
       FIELD kawcestvoobesp$ AS CHARACTER /* КачествоОбесп */
       FIELD kursvalobesp$ AS DECIMAL /* КурсВалОбесп */
       FIELD mestonahowzdenie$ AS CHARACTER /* Местонахождение */
       FIELD nomdogob$ AS CHARACTER /* НомДогОб */
       FIELD nomerpp$ AS INT64 /* НомерПП */
       FIELD opisanie$ AS CHARACTER /* Описание */
       FIELD summanacval$ AS DECIMAL /* СуммаНацВал */
       FIELD plavob$ AS LOGICAL /* Плавающее обеспечение */
       FIELD CustSurr AS CHARACTER /* CustSurr */
       FIELD sss AS DATE /* sss */
       FIELD term-obl-status AS CHARACTER /* term-obl-status */
       FIELD XSysPledgeID AS CHARACTER /* XSysPledgeID */
       FIELD AgrCounter      AS CHARACTER /* Номер договора по счетчику */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       FIELD fine            AS CHARACTER /* Штраф за невыполнение */
       FIELD minsummf        AS CHARACTER /* Минимальная сумма штрафа */
       /* Additional fields you should place here                      */

       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-term-obl-gar" "" }
       .

       
DEFINE TEMP-TABLE tt-term-obl-gar-old NO-UNDO LIKE term-obl
       .

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: f-t-obl.p
      Comment: Экранная форма редактирования для классов term-obl
   Parameters:
         Uses:
      Used by:
      Created: 22.05.2009 11:11 Jadv (0077883)
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
{client.i}
{intrface.get tmess}
{intrface.get loan}
{intrface.get acct}
{intrface.get i254}
{intrface.get lngar}   /* Библиотека для работы с гарантиями */

CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

DEF VAR mAutoGenNum   AS LOG   NO-UNDO. /* Автогенерация номера договора */
DEF VAR iContract     AS CHAR  NO-UNDO.
DEF VAR iContcode     AS CHAR  NO-UNDO.
DEF VAR mSurr         AS CHAR  NO-UNDO.
DEF VAR mPrevEndDate  AS DATE  NO-UNDO.
DEF VAR mSetFI        AS LOG   NO-UNDO. /* устанавливать ДР по финансовым инструментам */
DEF VAR mPrevNN       AS INT64 NO-UNDO.
DEF VAR mNum          AS CHAR  NO-UNDO. /* Номер договора обеспечения */
DEF VAR mCounterValue AS INT64 NO-UNDO. /* Значение счётчика */

DEF BUFFER b-loan       FOR loan.
DEF BUFFER b-term-obl   FOR term-obl.
DEF BUFFER b-acct       FOR acct.
DEF BUFFER b-instr-rate FOR instr-rate.
DEF BUFFER b-comm-rate  FOR comm-rate.

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
&Scoped-define INTERNAL-TABLES tt-term-obl-gar

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-term-obl-gar.fop-date ~
tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.currency tt-term-obl-gar.amt-rub ~
tt-term-obl-gar.sop-date tt-term-obl-gar.sop-offbal
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-term-obl-gar.fop-date ~
tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.currency tt-term-obl-gar.amt-rub ~
tt-term-obl-gar.sop-date tt-term-obl-gar.sop-offbal
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-term-obl-gar
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-term-obl-gar
&Scoped-define QUERY-STRING-fMain FOR EACH tt-term-obl-gar SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-term-obl-gar SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-term-obl-gar
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-term-obl-gar


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-term-obl-gar.fop-date tt-term-obl-gar.symbol ~
tt-term-obl-gar.fop tt-term-obl-gar.currency tt-term-obl-gar.amt-rub ~
tt-term-obl-gar.sop-date tt-term-obl-gar.sop-offbal tt-term-obl-gar.end-date ~
tt-term-obl-gar.fuser-id
&Scoped-define ENABLED-TABLES tt-term-obl-gar
&Scoped-define FIRST-ENABLED-TABLE tt-term-obl-gar
&Scoped-Define ENABLED-OBJECTS mVidName mCustName mKeepCurrV mCurRate ~
mSumRUR mObName mAcct mMarkModeV mFICategoryV mQualityCategory mFICodeName ~
mQuotTypeName mVidObespech
&Scoped-Define DISPLAYED-FIELDS tt-term-obl-gar.fop-date tt-term-obl-gar.symbol tt-term-obl-gar.fuser-id ~
tt-term-obl-gar.fop tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.sop-date ~
tt-term-obl-gar.sop-offbal tt-term-obl-gar.end-date tt-term-obl-gar.nomdogob$ tt-term-obl-gar.viddogob$ ~
tt-term-obl-gar.nomerpp$ tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.suser-id ~
tt-term-obl-gar.opisanie$ tt-term-obl-gar.mestonahowzdenie$ tt-term-obl-gar.dopinfo$ tt-term-obl-gar.plavob$
&Scoped-define DISPLAYED-TABLES tt-term-obl-gar
&Scoped-define FIRST-DISPLAYED-TABLE tt-term-obl-gar
&Scoped-Define DISPLAYED-OBJECTS mCustName mVidName mKeepCurrV mCurRate mSumRUR mObName mAcct ~
mMarkModeV mFICategoryV mQualityCategory mFICodeName mQuotTypeName mVidObespech

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-term-obl-gar.viddogob$ tt-term-obl-gar.nomdogob$ tt-term-obl-gar.fop-date ~
tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.end-date tt-term-obl-gar.dopinfo$ ~
tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.mestonahowzdenie$ ~
tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.sop-date ~
tt-term-obl-gar.sop-offbal tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id ~
tt-term-obl-gar.opisanie$ tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$ tt-term-obl-gar.plavob$ ~
mKeepCurrV mCurRate mSumRUR mMarkModeV mFICategoryV mQualityCategory mVidObespech
&Scoped-define List-2 tt-term-obl-gar.viddogob$ tt-term-obl-gar.nomdogob$ tt-term-obl-gar.fop-date ~
tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.end-date tt-term-obl-gar.dopinfo$ ~
tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.mestonahowzdenie$ ~
tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.sop-date ~
tt-term-obl-gar.sop-offbal tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id ~
tt-term-obl-gar.opisanie$ tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$ tt-term-obl-gar.plavob$ ~
mKeepCurrV mCurRate mSumRUR mMarkModeV mFICategoryV mVidObespech
&Scoped-define List-3 tt-term-obl-gar.viddogob$ tt-term-obl-gar.nomdogob$ tt-term-obl-gar.fop-date ~
tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.end-date tt-term-obl-gar.dopinfo$ ~
tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.mestonahowzdenie$ ~
tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.sop-date ~
mAcct tt-term-obl-gar.sop-offbal tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id ~
tt-term-obl-gar.opisanie$ tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$ tt-term-obl-gar.plavob$ ~
mKeepCurrV mCurRate mSumRUR mMarkModeV mFICategoryV mVidObespech
&Scoped-define List-4 tt-term-obl-gar.symbol tt-term-obl-gar.amt-rub tt-term-obl-gar.currency ~
tt-term-obl-gar.fop tt-term-obl-gar.fop-date tt-term-obl-gar.sop-offbal tt-term-obl-gar.end-date ~
tt-term-obl-gar.sop-date tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$ tt-term-obl-gar.nomdogob$ ~
tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id tt-term-obl-gar.plavob$ ~
tt-term-obl-gar.opisanie$ tt-term-obl-gar.mestonahowzdenie$ tt-term-obl-gar.dopinfo$

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mVidObespech AS CHARACTER FORMAT "X(4)"
     LABEL "Вид обеспечения (РВПС)"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
     &ELSE SIZE 4 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR
      tt-term-obl-gar SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-term-obl-gar.viddogob$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 16 COLON-ALIGNED
          &ELSE AT ROW 1 COL 16 COLON-ALIGNED &ENDIF HELP
          "Вид договора обеспечения (F1 выбор)"
          LABEL "Вид договора   " FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     mVidName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 29 COLON-ALIGNED
          &ELSE AT ROW 1 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
          FORMAT "x(36)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 36 BY 1
          &ELSE SIZE 36 BY 1 &ENDIF
   tt-term-obl-gar.nomerpp$
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 74 COLON-ALIGNED
        &ELSE AT ROW 1 COL 74 COLON-ALIGNED &ENDIF HELP
        "Порядковый номер объекта обеспечения"
        LABEL "N п/п" FORMAT "999"
        VIEW-AS FILL-IN
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
        &ELSE SIZE 3 BY 1 &ENDIF
     tt-term-obl-gar.nomdogob$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер договора"
          LABEL "Номер договора " FORMAT "x(35)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 35 BY 1
          &ELSE SIZE 35 BY 1 &ENDIF
     tt-term-obl-gar.fop-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 67 COLON-ALIGNED
          &ELSE AT ROW 2 COL 67 COLON-ALIGNED &ENDIF HELP
          "Дата начисления обеспечения"
          LABEL "Заключен" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-term-obl-gar.symbol
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 4 COLON-ALIGNED
          &ELSE AT ROW 3 COL 4 COLON-ALIGNED &ENDIF HELP
          "Тип клиента"
          LABEL "Тип" FORMAT "x(1)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 1 BY 1
          &ELSE SIZE 1 BY 1 &ENDIF
     tt-term-obl-gar.fop
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 16 COLON-ALIGNED
          &ELSE AT ROW 3 COL 16 COLON-ALIGNED &ENDIF
          LABEL "Код" FORMAT "9999999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
     mCustName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 24 COLON-ALIGNED
          &ELSE AT ROW 3 COL 24 COLON-ALIGNED &ENDIF NO-LABEL
          FORMAT "x(30)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
          &ELSE SIZE 30 BY 1 &ENDIF
     tt-term-obl-gar.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 67 COLON-ALIGNED
          &ELSE AT ROW 3 COL 67 COLON-ALIGNED &ENDIF HELP
          "Плановая дата списания обеспечения"
          LABEL "Окончание" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-term-obl-gar.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 4 COLON-ALIGNED
          &ELSE AT ROW 5 COL 4 COLON-ALIGNED &ENDIF HELP
          "Валюта обеспечения"
          LABEL "Вал" FORMAT "x(3)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-term-obl-gar.amt-rub
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 16 COLON-ALIGNED
          &ELSE AT ROW 5 COL 16 COLON-ALIGNED &ENDIF HELP
          "Сумма обеспечения"
          LABEL "Сумма" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     mKeepCurrV
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 45 COLON-ALIGNED
          &ELSE AT ROW 5 COL 45 COLON-ALIGNED &ENDIF HELP
          "Валюта учета"
          LABEL "ВалУчета" FORMAT "x(8)"
          VIEW-AS COMBO-BOX INNER-LINES 2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mCurRate
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 66 COLON-ALIGNED
          &ELSE AT ROW 5 COL 66 COLON-ALIGNED &ENDIF HELP
          "Курс"
          LABEL "Курс" FORMAT ">,>>9.99999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     mSumRUR
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF HELP
          "Сумма в национальной валюте"
          LABEL "НацВал" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-term-obl-gar.vidob$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 16 COLON-ALIGNED
          &ELSE AT ROW 7 COL 16 COLON-ALIGNED &ENDIF HELP
          "Вид обеспечения"
          LABEL "Вид обеспечения" FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     mObName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 29 COLON-ALIGNED
          &ELSE AT ROW 7 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
          FORMAT "x(20)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
     tt-term-obl-gar.datapost$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 67 COLON-ALIGNED
          &ELSE AT ROW 7 COL 67 COLON-ALIGNED &ENDIF HELP
          "Дата поступления"
          LABEL "Поступило" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mAcct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF HELP
          "Лицевой счет"
          LABEL "Лицевой счет   " FORMAT "x(24)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
     tt-term-obl-gar.sop-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 67 COLON-ALIGNED
          &ELSE AT ROW 8 COL 67 COLON-ALIGNED &ENDIF HELP
          "Фактическая дата списания обеспечения"
          LABEL "Выбыло" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mMarkModeV
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 16 COLON-ALIGNED
          &ELSE AT ROW 10 COL 16 COLON-ALIGNED &ENDIF HELP
          "Способ оценки обеспечения"
          LABEL "Способ оценки  " FORMAT "X(30)"
          VIEW-AS COMBO-BOX INNER-LINES 3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 34 BY 1
          &ELSE SIZE 34 BY 1 &ENDIF
     tt-term-obl-gar.sop-offbal
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 72 COLON-ALIGNED
          &ELSE AT ROW 10 COL 72 COLON-ALIGNED &ENDIF HELP
          "Количество объектов обеспечения"
          LABEL "Количество" FORMAT "999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     mFICategoryV
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "Категория финансового инструмента"
          LABEL "Категория Ф.И. " FORMAT "X(25)"
          VIEW-AS COMBO-BOX INNER-LINES 3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 29 BY 1
          &ELSE SIZE 29 BY 1 &ENDIF
     mQualityCategory
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 72 COLON-ALIGNED
          &ELSE AT ROW 11 COL 72 COLON-ALIGNED &ENDIF HELP
          "Категория качества (F1 выбор)"
          LABEL "Категория качества" FORMAT "X(2)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 2 BY 1
          &ELSE SIZE 2 BY 1 &ENDIF
     tt-term-obl-gar.fuser-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 16 COLON-ALIGNED
          &ELSE AT ROW 12 COL 16 COLON-ALIGNED &ENDIF HELP
          "Код финансового инструмента (F1 выбор)"
          LABEL "Код Ф.И.       " FORMAT "X(15)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     mFICodeName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 32 COLON-ALIGNED
          &ELSE AT ROW 12 COL 32 COLON-ALIGNED &ENDIF NO-LABEL
          FORMAT "X(15)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-term-obl-gar.plavob$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 72 COLON-ALIGNED
          &ELSE AT ROW 12 COL 72 COLON-ALIGNED &ENDIF HELP
          "Плавающее обеспечение (F1 выбор)"
          LABEL "Плавающее обеспечение" FORMAT "ДА/НЕТ"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-term-obl-gar.suser-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 16 COLON-ALIGNED
          &ELSE AT ROW 13 COL 16 COLON-ALIGNED &ENDIF HELP
          "Тип котировки (F1 выбор)"
          LABEL "Тип котировки  " FORMAT "X(15)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     mQuotTypeName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 32 COLON-ALIGNED
          &ELSE AT ROW 13 COL 32 COLON-ALIGNED &ENDIF NO-LABEL
          FORMAT "X(15)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     mVidObespech
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 72 COLON-ALIGNED
          &ELSE AT ROW 13 COL 72 COLON-ALIGNED &ENDIF HELP
          "Вид обеспечения по категории качества"
     tt-term-obl-gar.opisanie$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 16 COLON-ALIGNED
          &ELSE AT ROW 15 COL 16 COLON-ALIGNED &ENDIF HELP
          "Описание предмета обеспечения"
          LABEL "Описание       " FORMAT "x(4096)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-term-obl-gar.mestonahowzdenie$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 16 COLON-ALIGNED
          &ELSE AT ROW 16 COL 16 COLON-ALIGNED &ENDIF HELP
          "Местонахождение имущества"
          LABEL "Местонахождение" FORMAT "x(4096)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     tt-term-obl-gar.dopinfo$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 16 COLON-ALIGNED
          &ELSE AT ROW 17 COL 16 COLON-ALIGNED &ENDIF HELP
          "Дополнительная информация"
          LABEL "Дополнительно  " FORMAT "x(4096)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     "──────────────────────────────────────────────────────────────────────────────" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
          &ELSE SIZE 78 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 1
          &ELSE AT ROW 4 COL 1 &ENDIF
     "──────────────────────────────────────────────────────────────────────────────" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
          &ELSE SIZE 78 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 1
          &ELSE AT ROW 14 COL 1 &ENDIF
     "──────────────────────────────────────────────────────────────────────────────" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
          &ELSE SIZE 78 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 1
          &ELSE AT ROW 9 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 19
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-term-obl-gar T "?" NO-UNDO bisquit term-obl
      ADDITIONAL-FIELDS:
          FIELD valuwcetaobesp$ AS CHARACTER /* ВалУчетаОбесп */
          FIELD viddogob$ AS CHARACTER /* ВидДогОб */
          FIELD vidob$ AS CHARACTER /* ВидОб */
          FIELD datapost$ AS DATE /* ДатаПост */
          FIELD dopinfo$ AS CHARACTER /* ДопИнфо */
          FIELD kawcestvoobesp$ AS CHARACTER /* КачествоОбесп */
          FIELD kursvalobesp$ AS DECIMAL /* КурсВалОбесп */
          FIELD mestonahowzdenie$ AS CHARACTER /* Местонахождение */
          FIELD nomdogob$ AS CHARACTER /* НомДогОб */
          FIELD nomerpp$ AS INT64 /* НомерПП */
          FIELD opisanie$ AS CHARACTER /* Описание */
          FIELD summanacval$ AS DECIMAL /* СуммаНацВал */
          FIELD plavob$ AS LOGICAL /* Плавающее обеспечение */
          FIELD CustSurr AS CHARACTER /* CustSurr */
          FIELD sss AS DATE /* sss */
          FIELD term-obl-status AS CHARACTER /* term-obl-status */
          FIELD XSysPledgeID AS CHARACTER /* XSysPledgeID */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          FIELD fine            AS CHARACTER /* Штраф за невыполнение */
          FIELD minsummf        AS CHARACTER /* Минимальная сумма штрафа */
          /* Additional fields you should place here                      */

          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-term-obl-gar" "" }

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
/* SETTINGS FOR FILL-IN tt-term-obl-gar.amt-rub IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-term-obl-gar.symbol IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-term-obl-gar"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME


/* ************************  Control Triggers  ************************ */
   /* F1 ============ tt-term-obl-gar.viddogob$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.viddogob$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.viddogob$ TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.viddogob$ IN FRAME fMain
DO:
   DO TRANS:
      pick-value = ?.
      RUN pclass.p ("ВидДогОб",
                    "ВидДогОб",
                    "Виды договоров обеспечения",
                    iLevel + 1).
      IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
         AND  pick-value   NE ? THEN
      DO:
         ASSIGN
            mVidName:SCREEN-VALUE                  = GetCodeName ("ВидДогОб", pick-value)
            tt-term-obl-gar.viddogob$:SCREEN-VALUE = TRIM(pick-value)
         .
      END.
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

   /* LEAVE ============ tt-term-obl-gar.viddogob$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.viddogob$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.viddogob$ TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.viddogob$ IN FRAME fMain
DO:
   DEF VAR vNumPP  AS INT64  NO-UNDO.
   DEF VAR vNumber AS INT64  NO-UNDO.
   DEF VAR vSurr   AS CHAR NO-UNDO.
   DEF VAR vVidDog AS CHAR NO-UNDO.
   {&BEG_BT_LEAVE}
   IF NOT ({assigned tt-term-obl-gar.viddogob$:SCREEN-VALUE}
           AND       tt-term-obl-gar.viddogob$:SCREEN-VALUE NE "?") THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Заполните поле вид договора обеспечения").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   ELSE
   DO:
      mVidName:SCREEN-VALUE = GetCodeName ("ВидДогОб", tt-term-obl-gar.viddogob$:SCREEN-VALUE).
      IF    tt-term-obl-gar.viddogob$ NE tt-term-obl-gar.viddogob$:SCREEN-VALUE
         AND
            (  iMode EQ {&MOD_ADD}
            OR iMode EQ {&MOD_EDIT}
            ) THEN
      DO:
         vNumPP = 0.
         FOR EACH b-term-obl WHERE
                  b-term-obl.contract  EQ iContract
            AND   b-term-obl.cont-code EQ iContCode
            AND   b-term-obl.idnt      EQ 5
         NO-LOCK:
            ASSIGN
               vSurr = b-term-obl.contract         + "," +
                       b-term-obl.cont-code        + "," +
                       STRING(b-term-obl.idnt)     + "," +
                       STRING(b-term-obl.end-date) + "," +
                       STRING(b-term-obl.nn)
               vVidDog =     GetXAttrValueEx ("term-obl", vSurr, "ВидДогОб", "")
               vNumber = INT64(GetXAttrValueEx ("term-obl", vSurr, "НомерПП" , ""))
            .
            IF vVidDog NE tt-term-obl-gar.viddogob$:SCREEN-VALUE THEN
               NEXT.
            vNumPP = MAX(vNumPP, vNumber + 1).
         END.
         tt-term-obl-gar.nomerpp$:SCREEN-VALUE = STRING(vNumPP).
      END.
      tt-term-obl-gar.viddogob$ = tt-term-obl-gar.viddogob$:SCREEN-VALUE.
   END.

   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.nomerpp$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.nomerpp$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.nomerpp$ TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.nomerpp$ IN FRAME fMain
DO:
   DEF VAR vNumPP  AS INT64  NO-UNDO.
   DEF VAR vNumber AS INT64  NO-UNDO.
   DEF VAR vSurr   AS CHAR NO-UNDO.
   DEF VAR vVidDog AS CHAR NO-UNDO.

   {&BEG_BT_LEAVE}
   FOR EACH b-term-obl WHERE
            b-term-obl.contract  EQ iContract
      AND   b-term-obl.cont-code EQ iContCode
      AND   b-term-obl.idnt      EQ 5
   NO-LOCK:
      ASSIGN
         vSurr = b-term-obl.contract         + "," +
                 b-term-obl.cont-code        + "," +
                 STRING(b-term-obl.idnt)     + "," +
                 STRING(b-term-obl.end-date) + "," +
                 STRING(b-term-obl.nn)
         vVidDog =     GetXAttrValueEx ("term-obl", vSurr, "ВидДогОб", "")
         vNumber = INT64(GetXAttrValueEx ("term-obl", vSurr, "НомерПП" , ""))
      .
      IF vVidDog NE tt-term-obl-gar.viddogob$:SCREEN-VALUE THEN
         NEXT.
      IF    (iMode EQ {&MOD_ADD}  AND vNumber EQ INT64(tt-term-obl-gar.nomerpp$:SCREEN-VALUE))
         OR (iMode EQ {&MOD_EDIT} AND vNumber EQ INT64(tt-term-obl-gar.nomerpp$:SCREEN-VALUE) AND vNumber NE INT64(tt-term-obl-gar.nomerpp$))
      THEN DO:
         RUN Fill-SysMes IN h_tmess("", "", "",
                                    "Такой номер п/п уже есть").
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   RUN GetObespAcct IN h_lngar (tt-term-obl-gar.viddogob$:SCREEN-VALUE,
                                INT64 (tt-term-obl-gar.nomerpp$:SCREEN-VALUE),
                                b-loan.contract,
                                b-loan.cont-code,
                                MAX(b-loan.since, DATE (tt-term-obl-gar.fop-date:SCREEN-VALUE)),
                                tt-term-obl-gar.symbol:SCREEN-VALUE,
                                INT64 (tt-term-obl-gar.fop:SCREEN-VALUE),
                                BUFFER b-acct).
   mAcct:SCREEN-VALUE = IF AVAIL b-acct
                        THEN STRING (b-acct.acct, GetAcctFmt(b-acct.acct-cat))
                        ELSE "".
   {&BT_LEAVE}
END.


   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
   /* LEAVE ============ tt-term-obl-gar.plavob$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.plavob$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.plavob$ TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.plavob$ IN FRAME fMain
DO:
   DEF VAR vSurr    AS CHAR NO-UNDO.
   DEF BUFFER b-term-obl FOR term-obl.

   {&BEG_BT_LEAVE}
      /* Проверка на значение "ПлавОб" при установке значения "Да" -
      ** значение "Да" может иметь только одно обеспечение на договор. */
   IF tt-term-obl-gar.plavob$:SCREEN-VALUE EQ "ДА" THEN
   DO:
      FOR EACH b-term-obl WHERE
               b-term-obl.contract  EQ iContract
         AND   b-term-obl.cont-code EQ iContCode
         AND   b-term-obl.idnt      EQ 5
      NO-LOCK:
            /* Определим значение ДР "ПлавОб" */
         vSurr    = b-term-obl.contract        + "," +
                    b-term-obl.cont-code       + "," +
                    STRING(b-term-obl.idnt)    + "," +
                    STRING(b-term-obl.end-date)+ "," +
                    STRING(b-term-obl.nn).
         IF vSurr NE iSurrogate THEN
         DO:
            IF GetXattrValueEx ("term-obl", vSurr, "ПлавОб", "НЕТ") EQ "ДА" THEN
            DO:
               RUN Fill-SysMes IN h_tmess("", "", "",
                                          "Уже есть один плавающий договор обеспечения - <" +
                                          GetXattrValueEx ("term-obl", vSurr, "НомДогОб", "") + "> от " +
                                          STRING(b-term-obl.end-date)).
               {return_no_apply.i '{&RET-ERROR}'}
            END.
         END.
      END.
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ tt-term-obl-gar.fop ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.fop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.fop TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.fop IN FRAME fMain
DO:
   IF iMode EQ {&MOD_VIEW} THEN
   DO:
      CASE tt-term-obl-gar.symbol:SCREEN-VALUE:
         WHEN 'Ю' THEN
            RUN RunClassMethod IN h_xclass ("cust-corp",
                                            "Look",
                                            "","",
                                            "cust#", 
                                            STRING (tt-term-obl-gar.fop) + ",3").
         WHEN 'Б' THEN
            RUN RunClassMethod IN h_xclass ("banks",
                                            "Look",
                                            "","",
                                            "banks#", 
                                            STRING (tt-term-obl-gar.fop) + ",3").
         WHEN 'Ч' THEN
            RUN RunClassMethod IN h_xclass ("person",
                                            "Look",
                                            "","",
                                            "person#", 
                                            STRING (tt-term-obl-gar.fop) + ",3").
      END CASE.
   END.
   ELSE
   DO TRANS:
      pick-value = ?.
      CASE tt-term-obl-gar.symbol:SCREEN-VALUE:
         WHEN 'Ю' THEN
            IF FGetSetting("ФильтрКлОб","","ДА") EQ "ДА" THEN
               RUN browseld.p("cust-corp", "", "", ?, 4).
            ELSE
               RUN browseld.p("cust-corp", "crClass-Code", "*", ?, 4).
         WHEN 'Б' THEN
            IF FGetSetting("ФильтрКлОб","","ДА") EQ "ДА" THEN 
               RUN browseld.p("banks", "client", "yes", ?, 4).
            ELSE
               RUN browseld.p("banks",     "", "", ?, 4).
         WHEN 'Ч' THEN
            IF FGetSetting("ФильтрКлОб","","ДА") EQ "ДА" THEN
               RUN browseld.p("person",    "", "", ?, 4).
            ELSE
               RUN browseld.p("person", "crClass-Code", "*", ?, 4).
      END CASE.
         /* Если ввод подтвержден, то заполнем код клиента */
      IF     {&LAST_KEY}    EQ 10
         AND pick-value NE ? THEN
      DO:
         tt-term-obl-gar.fop:SCREEN-VALUE = pick-value.

         RUN RE_CLIENT (tt-term-obl-gar.symbol:SCREEN-VALUE,
                        tt-term-obl-gar.fop:SCREEN-VALUE,
                        INPUT-OUTPUT mCustName).
      END.

      mCustName:SCREEN-VALUE = mCustName.
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.fop ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.fop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.fop TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.fop IN FRAME fMain
DO:
   DEF VAR vCustName AS CHAR NO-UNDO INIT ?.
   {&BEG_BT_LEAVE}
   RUN RE_CLIENT (tt-term-obl-gar.symbol:SCREEN-VALUE,
                  tt-term-obl-gar.fop:SCREEN-VALUE,
                  INPUT-OUTPUT vCustName).
   IF NOT ({assigned vCustName}
      AND  vCustName NE "?")  THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Клиента с таким кодом не существует!").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   ELSE
   DO:
      mCustName:SCREEN-VALUE = vCustName.
      RUN GetObespAcct IN h_lngar (tt-term-obl-gar.viddogob$:SCREEN-VALUE,
                                   INT64 (tt-term-obl-gar.nomerpp$:SCREEN-VALUE),
                                   b-loan.contract,
                                   b-loan.cont-code,
                                   MAX(b-loan.since, DATE (tt-term-obl-gar.fop-date:SCREEN-VALUE)),
                                   tt-term-obl-gar.symbol:SCREEN-VALUE,
                                   INT64 (tt-term-obl-gar.fop:SCREEN-VALUE),
                                   BUFFER b-acct).
      mAcct:SCREEN-VALUE = IF AVAIL b-acct
                           THEN STRING (b-acct.acct, GetAcctFmt(b-acct.acct-cat))
                           ELSE "".
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.fop-date ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.fop-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.fop-date TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.fop-date IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF NOT {assigned tt-term-obl-gar.fop-date:SCREEN-VALUE}  THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Введите дату заключения договора").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   RUN GetObespAcct IN h_lngar (tt-term-obl-gar.viddogob$:SCREEN-VALUE,
                                INT64 (tt-term-obl-gar.nomerpp$:SCREEN-VALUE),
                                b-loan.contract,
                                b-loan.cont-code,
                                MAX(b-loan.since, DATE (tt-term-obl-gar.fop-date:SCREEN-VALUE)),
                                tt-term-obl-gar.symbol:SCREEN-VALUE,
                                INT64 (tt-term-obl-gar.fop:SCREEN-VALUE),
                                BUFFER b-acct).
   mAcct:SCREEN-VALUE = IF AVAIL b-acct
                        THEN STRING (b-acct.acct, GetAcctFmt(b-acct.acct-cat))
                        ELSE "".
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.end-date ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.end-date TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.end-date IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF DATE(tt-term-obl-gar.end-date:SCREEN-VALUE) EQ ? THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Установите дату окончания!").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   IF DATE(tt-term-obl-gar.end-date:SCREEN-VALUE) LT DATE(tt-term-obl-gar.fop-date:SCREEN-VALUE) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Значение даты окончания не может быть меньше даты заключения!").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ tt-term-obl-gar.currency ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.currency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.currency TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.currency IN FRAME fMain
DO:
   DO TRANS:
      pick-value = ?.
      RUN currency.p ('Учетный',
                      3).
      IF     {&LAST_KEY}    EQ 10
         AND pick-value NE ? THEN
         tt-term-obl-gar.currency:SCREEN-VALUE = pick-value.
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.amt-rub ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.amt-rub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.amt-rub TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.amt-rub IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF DEC(tt-term-obl-gar.amt-rub:SCREEN-VALUE) EQ 0 THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Сумма не может быть нулевой").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

         /* VALUE-CHANGED */
   /* LEAVE ============ mKeepCurrV ============ */
&Scoped-define SELF-NAME mKeepCurrV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKeepCurrV TERMINAL-SIMULATION
ON LEAVE  OF mKeepCurrV IN FRAME fMain
DO:
   IF mKeepCurrV:SCREEN-VALUE EQ "ДогВал" THEN
   DO:
      ASSIGN
         mCurRate:SENSITIVE    = NO
         mCurRate:SCREEN-VALUE = "0"
         mSumRUR:SCREEN-VALUE  = "0"
      .
      DISABLE mCurRate WITH FRAME {&MAIN-FRAME}.
   END.
   ELSE
   DO:
      IF tt-term-obl-gar.currency:SCREEN-VALUE EQ "" THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Договор открыт в нац. валюте, значение поля должно быть <ДогВал>").
         mKeepCurrV:SCREEN-VALUE = "ДогВал".
         {return_no_apply.i '{&RET-ERROR}'}
       END.
      ASSIGN
         mCurRate:SENSITIVE    = YES
      .
      ENABLE mCurRate WITH FRAME {&MAIN-FRAME}.
   END.
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ tt-term-obl-gar.vidob$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.vidob$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.vidob$ TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.vidob$ IN FRAME fMain
DO:
   DO TRANS:
      pick-value = ?.
      RUN pclass.p ("ВидОб",
                   "ВидОб",
                   "Виды параметров обеспечения",
                   iLevel + 1).
      IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
         AND  pick-value   NE ? THEN
      DO:
         ASSIGN
            tt-term-obl-gar.vidob$:SCREEN-VALUE = TRIM(pick-value)
            mObName:SCREEN-VALUE = GetCodeName ("ВидОб", pick-value)
        .
      END.
   END.

END.


   /* LEAVE ============ tt-term-obl-gar.vidob$ ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.vidob$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.vidob$ TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.vidob$ IN FRAME fMain
DO:
   DEF VAR vVidObespech AS CHAR NO-UNDO.
   {&BEG_BT_LEAVE}
   IF NOT ({assigned tt-term-obl-gar.vidob$:SCREEN-VALUE}
           AND       tt-term-obl-gar.vidob$:SCREEN-VALUE NE "?") THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Заполните вид обеспечения").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   vVidObespech = GetCode("ВидОб",
                          tt-term-obl-gar.vidob$:SCREEN-VALUE
                          ).
   IF {assigned vVidObespech} THEN
   DO:
      IF NUM-ENTRIES(vVidObespech) EQ 1 THEN
         mVidObespech:SCREEN-VALUE = vVidObespech.
      ELSE
         mVidObespech:SCREEN-VALUE = "".
   END.
   mObName:SCREEN-VALUE = GetCodeName ("ВидОб", 
                                       tt-term-obl-gar.vidob$:SCREEN-VALUE).
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ mMarkModeV ============ */
&Scoped-define SELF-NAME mMarkModeV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mMarkModeV TERMINAL-SIMULATION
ON LEAVE OF mMarkModeV IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF NOT {assigned mMarkModeV:SCREEN-VALUE} THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Не заполнено поле <Способ оценки>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   mSetFI = LOOKUP(mMarkModeV:SCREEN-VALUE, mTemp-String1) EQ 3.
   IF mSetFI THEN
      ASSIGN
         tt-term-obl-gar.fuser-id:SENSITIVE IN FRAME fMain = YES
         tt-term-obl-gar.suser-id:SENSITIVE IN FRAME fMain = YES
         mFICategoryV:SENSITIVE             IN FRAME fMain = YES
         tt-term-obl-gar.fuser-id:VISIBLE   IN FRAME fMain = YES
         tt-term-obl-gar.suser-id:VISIBLE   IN FRAME fMain = YES
         mFICategoryV:VISIBLE               IN FRAME fMain = YES
      .
   ELSE
      ASSIGN
         tt-term-obl-gar.fuser-id:SENSITIVE IN FRAME fMain = NO
         tt-term-obl-gar.suser-id:SENSITIVE IN FRAME fMain = NO
         mFICategoryV:SENSITIVE             IN FRAME fMain = NO
         tt-term-obl-gar.fuser-id:VISIBLE   IN FRAME fMain = NO
         tt-term-obl-gar.suser-id:VISIBLE   IN FRAME fMain = NO
         mFICategoryV:VISIBLE               IN FRAME fMain = NO
      .
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME mAcct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mAcct TERMINAL-SIMULATION
ON F1 OF mAcct IN FRAME fMain
DO:
   DEF VAR mHAcct AS CHAR NO-UNDO.
   mHAcct = REPLACE(SELF:SCREEN-VALUE,'-','').
   {find-act.i
      &acct    = mHAcct
      &curr    = tt-term-obl-gar.currency:SCREEN-VALUE
   }
   IF AVAIL acct THEN
      RUN formld.p(acct.class-code,
                   acct.acct + "," + acct.currency, "", "{&MOD_VIEW}",
                   iLevel + 1) NO-ERROR.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

   /* LEAVE ============ term-obl.sop-offbal ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.sop-offbal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.sop-offbal TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.sop-offbal IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF    (INT64(tt-term-obl-gar.sop-offbal:SCREEN-VALUE) EQ 0 OR INT64(tt-term-obl-gar.sop-offbal:SCREEN-VALUE) EQ ?)
      AND LOOKUP(mMarkModeV:SCREEN-VALUE, mTemp-String1) NE 1 THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Не заполнено поле <Количество>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ mQualityCategory ============ */
&Scoped-define SELF-NAME mQualityCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mQualityCategory TERMINAL-SIMULATION
ON F1 OF mQualityCategory IN FRAME fMain
DO:
   DEF VAR vNumPP AS CHAR NO-UNDO.
   DO TRANS:
      pick-value = ?.
      RUN pclass.p ("КачествоОбесп",
                    "КачествоОбесп",
                    "Категории качества",
                    iLevel + 1).
      IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
         AND  pick-value   NE ? THEN
      DO:
         ASSIGN
            mQualityCategory:SCREEN-VALUE = pick-value
         .
      END.
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ mQualityCategory ============ */
&Scoped-define SELF-NAME mQualityCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mQualityCategory TERMINAL-SIMULATION
ON LEAVE OF mQualityCategory IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF    mQualityCategory:SCREEN-VALUE NE ""
     AND GetCode("КачествоОбесп",mQualityCategory:SCREEN-VALUE) = ? THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Некорректно заполнено поле Категория качества. Воспользуйтесь F1").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ tt-term-obl-gar.fuser-id ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.fuser-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.fuser-id TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.fuser-id IN FRAME fMain
DO:
   DEF VAR vJ AS INT64 NO-UNDO.
   vJ = LOOKUP(mFICategoryV:SCREEN-VALUE, mTemp-String).
      /* По категории ФИ определяем имя процедуры просмотра для классификатора соответствующего ФИ */
   FIND FIRST code WHERE
              code.class EQ "instr-cat"
      AND     code.code  EQ ENTRY(vJ, mTemp-String2)
   NO-LOCK NO-ERROR.
   IF NOT AVAIL code THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Не удалось найти финансового инструмент <" + ENTRY(vJ, mTemp-String2) + "> " +
                                  "в классификаторе <instr-cat>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   ELSE
   DO:
      IF     SEARCH(code.misc[3] + ".p") EQ ?
         AND SEARCH(code.misc[3] + ".r") EQ ? THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Не найдена процедура просмотра финансового инструмента " + code.misc[3] + ".p").
         {return_no_apply.i '{&RET-ERROR}'}
      END.
      ELSE
      DO TRANS:
         pick-value = ?.
         RUN VALUE(code.misc[3] + ".p") (code.code,
                                         iLevel + 1).
         IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
            AND  pick-value   NE ? THEN
         DO:
            tt-term-obl-gar.fuser-id:SCREEN-VALUE = pick-value.
               /* Нужно найти в одной из таблиц currency или sec-code финансовый инструмент с кодом pick-value,
               ** дабы определить наименование этого инструмента ? Сейчас сделано криво !!! */
            IF ENTRY(vJ, mTemp-String2) EQ "currency" THEN
            DO:
               FIND FIRST currency WHERE
                          currency.currency EQ pick-value
               NO-LOCK NO-ERROR.
               IF NOT AVAIL currency THEN
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "",
                                              "Не найдена валюта с кодом <" + pick-value + ">").
                  {return_no_apply.i '{&RET-ERROR}'}
               END.
               ELSE
                  mFICodeName:SCREEN-VALUE = currency.name-currenc.
            END.
            IF ENTRY(vJ, mTemp-String2) EQ "sec-code" THEN
            DO:
               FIND FIRST sec-code WHERE
                          sec-code.sec-code EQ pick-value
               NO-LOCK NO-ERROR.
               IF NOT AVAIL sec-code THEN
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "",
                                              "Не найдена ценная бумага с кодом <" + pick-value + ">").
                  {return_no_apply.i '{&RET-ERROR}'}
               END.
               ELSE
                  mFICodeName:SCREEN-VALUE = sec-code.name.
            END.
         END.
      END.
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ tt-term-obl-gar.fuser-id ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.fuser-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.fuser-id TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.fuser-id IN FRAME fMain
DO:
   DEF VAR vJ AS INT64 NO-UNDO.
   {&BEG_BT_LEAVE}

      /* Нужно найти в одной из таблиц currency или sec-code финансовый инструмент
      ** с кодом pick-value, дабы определить наименование этого инструмента */
   vJ = LOOKUP(mFICategoryV:SCREEN-VALUE, mTemp-String).

   IF ENTRY(vJ, mTemp-String2) EQ "currency" THEN
   DO:
      FIND FIRST currency WHERE
                 currency.currency EQ tt-term-obl-gar.fuser-id:SCREEN-VALUE
      NO-LOCK NO-ERROR.
      IF NOT AVAIL currency THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Не найдена валюта с кодом <" + tt-term-obl-gar.fuser-id:SCREEN-VALUE + ">").
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   IF ENTRY(vJ, mTemp-String2) EQ "sec-code" THEN
   DO:
      FIND FIRST sec-code WHERE
                 sec-code.sec-code EQ tt-term-obl-gar.fuser-id:SCREEN-VALUE
      NO-LOCK NO-ERROR.
      IF NOT AVAIL sec-code THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Не найдена ценная бумага с кодом <" + tt-term-obl-gar.fuser-id:SCREEN-VALUE + ">").
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   IF     LOOKUP(mMarkModeV:SCREEN-VALUE, mTemp-String1) EQ 3
      AND tt-term-obl-gar.fuser-id:SCREEN-VALUE EQ "" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Необходимо заполнить поле <Код Ф.И.>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ tt-term-obl-gar.suser-id ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.suser-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.suser-id TERMINAL-SIMULATION
ON F1 OF tt-term-obl-gar.suser-id IN FRAME fMain
DO:
   DEF VAR vJ AS INT64 NO-UNDO.

   vJ = LOOKUP(mFICategoryV:SCREEN-VALUE, mTemp-String).
   FIND FIRST code WHERE
              code.code  EQ ENTRY(vJ, mTemp-String2)
      AND     code.class EQ "instr-cat"
   NO-LOCK NO-ERROR.
      /* класс instr-cat - финансовые инструменты
      ** code.code       - код ФИ, в данном случае это currency и sec-code,
      ** code.misc[1]    - искомое название классификатора */
   IF NOT AVAIL code THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Не удалось найти финансового инструмент <" + ENTRY(vJ, mTemp-String2) + "> " +
                                  "в классификаторе <instr-cat>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   ELSE
   DO TRANS:
      pick-value = ?.
      RUN pclass.p (code.misc[1],
                    code.misc[1],
                    "Тип котировки",
                    iLevel + 1).
      IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
         AND  pick-value   NE ? THEN
         ASSIGN
            tt-term-obl-gar.suser-id:SCREEN-VALUE = pick-value
            mQuotTypeName:SCREEN-VALUE            = GetCodeName(code.misc[1], pick-value)
         .
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

   /* LEAVE ============ tt-term-obl-gar.fuser-id ============ */
&Scoped-define SELF-NAME tt-term-obl-gar.suser-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-term-obl-gar.suser-id TERMINAL-SIMULATION
ON LEAVE OF tt-term-obl-gar.suser-id IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF     LOOKUP(mMarkModeV:SCREEN-VALUE, mTemp-String1) EQ 3
      AND tt-term-obl-gar.suser-id:SCREEN-VALUE EQ "" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "",
                                  "Необходимо заполнить поле <Тип котировки>").
      {return_no_apply.i '{&RET-ERROR}'}
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* VALUE-CHANGED ============ mFICategoryV ============ */
&Scoped-define SELF-NAME mFICategoryV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mFICategoryV TERMINAL-SIMULATION
ON VALUE-CHANGED OF mFICategoryV IN FRAME fMain
DO:
   ASSIGN
      tt-term-obl-gar.cor-acct              = mFICategoryV:SCREEN-VALUE
      tt-term-obl-gar.fuser-id:SCREEN-VALUE = ""
      tt-term-obl-gar.suser-id:SCREEN-VALUE = ""
      mFICodeName:SCREEN-VALUE              = ""
      mQuotTypeName:SCREEN-VALUE            = ""
   .
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* F1 ============ mVidObespech ============ */
&Scoped-define SELF-NAME mVidObespech
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mVidObespech TERMINAL-SIMULATION
ON F1 OF mVidObespech IN FRAME fMain
DO:
   DO TRANS:
      pick-value = ?.
      RUN pclass.p ("Ф117_ВидОб",
                    "Ф117_ВидОб",
                    "Вид обеспечения ( для формы 117)",
                    iLevel + 1).
      IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
         AND  pick-value   NE ? THEN
         ASSIGN
            mVidObespech:SCREEN-VALUE = pick-value
         .
   END.
   {return_no_apply.i}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ mSumRUR ============ */
&Scoped-define SELF-NAME mSumRUR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mSumRUR TERMINAL-SIMULATION
ON LEAVE OF mSumRUR IN FRAME fMain
DO:
   DEF VAR vmSumRUR AS DEC NO-UNDO.
   {&BEG_BT_LEAVE}
   IF     mKeepCurrV:SCREEN-VALUE   EQ "НацВал"
      AND DEC(mSumRUR:SCREEN-VALUE) NE 0
      AND {assigned tt-term-obl-gar.currency:SCREEN-VALUE} THEN
   DO:
      vmSumRUR = DEC(tt-term-obl-gar.amt-rub:SCREEN-VALUE) * DEC(mCurRate:SCREEN-VALUE).
      IF vmSumRUR <> DEC(mSumRUR:SCREEN-VALUE) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Сумма в нац.валюте не соответствует установленному курсу.").
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   {&BT_LEAVE}

END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


   /* LEAVE ============ mCurRate ============ */
&Scoped-define SELF-NAME mCurRate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mCurRate TERMINAL-SIMULATION
ON LEAVE OF mCurRate IN FRAME fMain
DO:
   {&BEG_BT_LEAVE}
   IF     mKeepCurrV:SCREEN-VALUE    EQ "НацВал"
      AND DEC(mCurRate:SCREEN-VALUE) NE 0
      AND {assigned tt-term-obl-gar.currency:SCREEN-VALUE} THEN
   DO:
      mSumRUR:SCREEN-VALUE = STRING(DEC(tt-term-obl-gar.amt-rub:SCREEN-VALUE) * DEC(mCurRate:SCREEN-VALUE)).
   END.
   {&BT_LEAVE}
END.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).
      /* Теперь тем чего на метасхеме нет */
   ASSIGN
      tt-term-obl-gar.symbol:LIST-ITEMS = "Ю,Б,Ч"
      mMarkModeV:LIST-ITEMS             = mTemp-String1
      mKeepCurrV:LIST-ITEMS             = mTemp-String3
      mFICategoryV:LIST-ITEMS           = mTemp-StringS
   .

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
  DISPLAY mVidName mCustName mKeepCurrV mCurRate mSumRUR  mObName mAcct mMarkModeV mFICategoryV
          mQualityCategory /* mFICodeName */ mQuotTypeName mVidObespech
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-term-obl-gar THEN
    DISPLAY tt-term-obl-gar.fop-date tt-term-obl-gar.symbol tt-term-obl-gar.fop
            tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.sop-date
            tt-term-obl-gar.sop-offbal tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$
            tt-term-obl-gar.nomdogob$ tt-term-obl-gar.end-date tt-term-obl-gar.vidob$
            tt-term-obl-gar.datapost$ tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id
            tt-term-obl-gar.opisanie$ tt-term-obl-gar.mestonahowzdenie$ tt-term-obl-gar.dopinfo$
            tt-term-obl-gar.plavob$
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-term-obl-gar.viddogob$ tt-term-obl-gar.nomdogob$ tt-term-obl-gar.fop-date
         tt-term-obl-gar.symbol tt-term-obl-gar.fop tt-term-obl-gar.end-date tt-term-obl-gar.dopinfo$
         tt-term-obl-gar.currency tt-term-obl-gar.amt-rub tt-term-obl-gar.mestonahowzdenie$
         tt-term-obl-gar.vidob$ tt-term-obl-gar.datapost$ tt-term-obl-gar.sop-date
         tt-term-obl-gar.sop-offbal tt-term-obl-gar.fuser-id tt-term-obl-gar.suser-id
         tt-term-obl-gar.opisanie$ tt-term-obl-gar.nomerpp$ tt-term-obl-gar.viddogob$
         tt-term-obl-gar.plavob$
         mQuotTypeName mVidObespech /* mFICodeName */ mAcct
         mCustName mVidName mObName mFICategoryV mQualityCategory
         mKeepCurrV mCurRate mSumRUR mMarkModeV
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
   RUN RE_CLIENT (tt-term-obl-gar.symbol,
                  tt-term-obl-gar.fop,
                  INPUT-OUTPUT mCustName).

   RUN GetObespAcct IN h_lngar (tt-term-obl-gar.viddogob$,
                                tt-term-obl-gar.nomerpp$,
                                b-loan.contract,
                                b-loan.cont-code,
                                MAX(b-loan.since, tt-term-obl-gar.fop-date),
                                tt-term-obl-gar.symbol,
                                tt-term-obl-gar.fop,
                                BUFFER b-acct).
   ASSIGN
      mKeepCurr        = IF GetXAttrValueEx ("term-obl", iSurrogate, "ВалУчетаОбесп", "") EQ ""
                           THEN "ДогВал"
                           ELSE "НацВал"
      mFICategoryV     = IF {assigned tt-term-obl-gar.cor-acct} AND LOOKUP(tt-term-obl-gar.cor-acct, mTemp-String2) GT 0
                           THEN ENTRY(LOOKUP(tt-term-obl-gar.cor-acct, mTemp-String2), mTemp-String)
                           ELSE ""
      mMarkModeV       = ENTRY(tt-term-obl-gar.fop-offbal + 1, mTemp-String1)
      mVidName         = GetCodeName("ВидДогОб", tt-term-obl-gar.viddogob$)
      mObName          = GetCodeName("ВидОб", tt-term-obl-gar.vidob$)
      mAcct            = IF AVAIL b-acct
                           THEN STRING (b-acct.acct, GetAcctFmt(b-acct.acct-cat))
                           ELSE ""
      mQualityCategory = Get_QualityGar ("comm-rate", iSurrogate, gend-date)
      mQualityCategory = IF {assigned mQualityCategory} AND mQualityCategory NE "?"
                           THEN mQualityCategory
                           ELSE ""
      mVidObespech     = Get_VidObespech (iSurrogate, gend-date)
      mVidObespech     = IF {assigned mVidObespech}
                           THEN mVidObespech
                           ELSE IF mQualityCategory NE ""
                                 THEN GetXAttrValueEx("loan", iContract + "," + iContCode, "Ф117_ВидОб", "")
                                 ELSE ""
      mSetFI           = LOOKUP(mMarkModeV, mTemp-String1) EQ 3
   .
   /* mFICodeName:VISIBLE IN FRAME fMain = NO. */
   IF NOT mSetFI THEN
      ASSIGN
         tt-term-obl-gar.fuser-id:SENSITIVE IN FRAME fMain = NO
         tt-term-obl-gar.suser-id:SENSITIVE IN FRAME fMain = NO
         mFICategoryV:SENSITIVE             IN FRAME fMain = NO
         tt-term-obl-gar.fuser-id:VISIBLE   IN FRAME fMain = NO
         tt-term-obl-gar.suser-id:VISIBLE   IN FRAME fMain = NO
         mFICategoryV:VISIBLE               IN FRAME fMain = NO
      .
   IF {assigned mFICategoryV} THEN
   DO:
         /* Категория ФИ */
      IF {assigned tt-term-obl-gar.fuser-id} THEN
      DO:
         IF ENTRY(LOOKUP(mFICategoryV, mTemp-String), mTemp-String2) EQ "currency" THEN
         DO:
            FIND FIRST currency WHERE
                       currency.currency EQ tt-term-obl-gar.fuser-id
            NO-LOCK NO-ERROR.
            IF NOT AVAIL currency THEN
               RUN Fill-SysMes IN h_tmess ("", "", "",
                                           "Не найдена валюта с кодом " + tt-term-obl-gar.fuser-id).
            ELSE
               mFICodeName = currency.name-currenc.
         END.
         ELSE
         DO:
            IF ENTRY(LOOKUP(mFICategoryV, mTemp-String), mTemp-String2) EQ "sec-code" THEN
            DO:
               FIND FIRST sec-code WHERE
                          sec-code.sec-code EQ tt-term-obl-gar.fuser-id
               NO-LOCK NO-ERROR.
               IF NOT AVAIL sec-code THEN
                  RUN Fill-SysMes IN h_tmess ("", "", "",
                                              "Не найдена ценная бумага с кодом " + tt-term-obl-gar.fuser-id).
               ELSE
                  mFICodeName = sec-code.name.
            END.
         END.
      END.
         /* Тип котировки */
      IF {assigned tt-term-obl-gar.suser-id} THEN
      DO:
         FIND FIRST code WHERE
                    code.class EQ "instr-cat"
            AND     code.code  EQ ENTRY(LOOKUP(mFICategoryV, mTemp-String), mTemp-String2)
         NO-LOCK NO-ERROR.
            /* класс instr-cat - финансовые инструменты
               code.code       - код ФИ, в данном случае это currency и sec-code,
               code.misc[1]    - искомое название классификатора ("Курсы" или "ТипКот") */
         IF AVAIL code THEN
            mQuotTypeName = GetCodeName(code.misc[1], tt-term-obl-gar.suser-id).
      END.
   END.
   ASSIGN
      mKeepCurrV:SCREEN-VALUE IN FRAME fMain       = mKeepCurr
      mMarkModeV:SCREEN-VALUE IN FRAME fMain       = mMarkModeV
      mFICategoryV:SCREEN-VALUE IN FRAME fMain     = mFICategoryV
      mVidName:SCREEN-VALUE IN FRAME fMain         = mVidName
      mCustName:SCREEN-VALUE IN FRAME fMain        = mCustName
      mObName:SCREEN-VALUE IN FRAME fMain          = mObName
      mAcct:SCREEN-VALUE IN FRAME fMain            = mAcct
      mQuotTypeName:SCREEN-VALUE IN FRAME fMain    = mQuotTypeName
      mFICodeName:SCREEN-VALUE IN FRAME fMain      = mFICodeName
      mQualityCategory:SCREEN-VALUE IN FRAME fMain = mQualityCategory
      mVidObespech:SCREEN-VALUE IN FRAME fMain     = mVidObespech
      mCurRate:SENSITIVE                           = mKeepCurr NE "ДогВал"
   .
      /* Предустановка полей */
   IF iMode EQ {&MOD_ADD} THEN
      ASSIGN
         tt-term-obl-gar.fuser-id                              = ""
         tt-term-obl-gar.suser-id                              = ""
         tt-term-obl-gar.fuser-id:SCREEN-VALUE IN FRAME fMain  = tt-term-obl-gar.fuser-id
         tt-term-obl-gar.suser-id:SCREEN-VALUE IN FRAME fMain  = tt-term-obl-gar.suser-id
         tt-term-obl-gar.symbol:SCREEN-VALUE IN FRAME fMain    = b-loan.cust-cat
         tt-term-obl-gar.fop:SCREEN-VALUE IN FRAME fMain       = STRING(b-loan.cust-id)
         tt-term-obl-gar.fop-date:SCREEN-VALUE IN FRAME fMain  = STRING(gend-date)
         tt-term-obl-gar.end-date:SCREEN-VALUE IN FRAME fMain  = STRING(b-loan.end-date)
         tt-term-obl-gar.datapost$:SCREEN-VALUE IN FRAME fMain = STRING(gend-date)
         mVidObespech:SCREEN-VALUE IN FRAME fMain              = ""
      .
   ELSE
      ASSIGN
         mPrevEndDate = tt-term-obl-gar.end-date
         mPrevNN      = tt-term-obl-gar.nn
      .

   IF    iMode EQ {&MOD_VIEW}
      OR iMode EQ {&MOD_EDIT} THEN
   DO:
      IF tt-term-obl-gar.opisanie$:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         tt-term-obl-gar.opisanie$:SCREEN-VALUE IN FRAME fMain = "".
      IF tt-term-obl-gar.mestonahowzdenie$:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         tt-term-obl-gar.mestonahowzdenie$:SCREEN-VALUE IN FRAME fMain = "".
      IF tt-term-obl-gar.dopinfo$:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         tt-term-obl-gar.dopinfo$:SCREEN-VALUE IN FRAME fMain = "".
      IF mVidObespech:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         mVidObespech:SCREEN-VALUE IN FRAME fMain = "".
      IF tt-term-obl-gar.plavob$:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         tt-term-obl-gar.plavob$:SCREEN-VALUE IN FRAME fMain = "НЕТ".
      IF tt-term-obl-gar.nomdogob$:SCREEN-VALUE IN FRAME fMain EQ "?" THEN
         tt-term-obl-gar.nomdogob$:SCREEN-VALUE IN FRAME fMain = "".
   END.

   RUN RE_CLIENT (tt-term-obl-gar.symbol:SCREEN-VALUE,
                  tt-term-obl-gar.fop:SCREEN-VALUE,
                  INPUT-OUTPUT mCustName).
   mCustName:SCREEN-VALUE = mCustName.

   tt-term-obl-gar.nomdogob$:SENSITIVE = NOT mAutoGenNum OR (GetXAttrInit("term-obl-gar","НомДогОбРед") EQ "Да").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_F9 TERMINAL-SIMULATION 
PROCEDURE Local_F9 :
                  /* Проверка прав на редактирование договора подчиненного */
   IF     USERID("bisquit") NE b-loan.user-id
      AND NOT GetSlavePermission(USERID("bisquit"),b-loan.user-id,"w")
   THEN RETURN ERROR.
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
   DEF BUFFER termo FOR term-obl.
      FIND FIRST termo WHERE ROWID(termo) = tt-term-obl-gar.local__rowid NO-LOCK NO-ERROR.

   IF AVAIL termo THEN
      sTermRecid = RECID(termo).

   IF iMode EQ {&MOD_ADD} THEN
   DO:
      IF NUM-ENTRIES(iInstanceList, CHR(3)) LT 2 THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "",
                                     "Неверное число параметров передано в процедуру.").
         RETURN ERROR.
      END.
      ASSIGN
         iContract = ENTRY(1, ENTRY(2, iInstanceList, CHR(3)))
         iContCode = ENTRY(2, ENTRY(2, iInstanceList, CHR(3)))
         tt-term-obl-gar.sop-offbal = 1.
      .
   END.
   ELSE
      ASSIGN
         iContract = ENTRY(1, iSurrogate)
         iContCode = ENTRY(2, iSurrogate)
      .
   FIND FIRST b-loan WHERE
              b-loan.contract  EQ iContract
      AND     b-loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF tt-term-obl-gar.nomerpp$ EQ ? THEN
      tt-term-obl-gar.nomerpp$ = 0.

   mAutoGenNum = GetXAttrInit(tt-term-obl-gar.Class-Code, "AutoGenNum") EQ "ДА".

   IF iMode EQ {&MOD_ADD} THEN
   DO:     
      IF mAutoGenNum THEN
      DO: 
         DO TRANS:
            pick-value = ?.
            RUN pclass.p ("ВидОб",
                          "ВидОб",
                          "Виды параметров обеспечения",
                          iLevel + 1).
            IF     ({&LAST_KEY} EQ 13 OR {&LAST_KEY} EQ 10)
               AND  pick-value   NE ? THEN
            DO: 
               ASSIGN
                  tt-term-obl-gar.vidob$ = TRIM(pick-value)
                  mObName = GetCodeName ("ВидОб", pick-value)
               .
            END.
         END.
         RUN GetNumLoan (tt-term-obl-gar.Class-Code,              /* Kласс             */
                         gend-date,                               /* Базовая дата      */
                         SUBSTITUTE("&1|&2|&3|&4|&5",
                                    b-loan.Branch-id,
                                    iContract,
                                    iContcode,
                                    tt-term-obl-gar.vidob$,
                                    ""),                          /* Код филиала и все необходимые теги  */
                         NO,                                      /* Обязательно генерировать номер ? */
                         OUTPUT mNum,                             /* Номер договора    */
                         OUTPUT mCounterValue).
      
         tt-term-obl-gar.nomdogob$ = mNum.
      END.
   END.

   /* сохраним начальные значения */
   IF iMode EQ {&MOD_EDIT} THEN
   DO:      
      BUFFER-COPY tt-term-obl-gar TO tt-term-obl-gar-old. 
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
   DEF VAR vSurr     AS CHAR NO-UNDO.
   DEF VAR vSurrOld  AS CHAR NO-UNDO.
   DEF VAR vMsg      AS CHAR NO-UNDO.
   
   DEF BUFFER bXlink   FOR xlink.
   DEF BUFFER bLink    FOR links.
   DEF BUFFER bNewLink FOR links.
   
   /* если были изменения, то нужно заполнить д.р. BKIStatus */
   IF iMode EQ {&MOD_EDIT} AND
   GetXAttrValueEx("loan",tt-term-obl-gar.contract
      + "," + tt-term-obl-gar.cont-code,"BKIStatus",?) NE ?
   THEN
   DO:
      IF fGetSetting("НБКИ","НБКИОбеспИзм", "") EQ "1" THEN
      DO:
         IF tt-term-obl-gar-old.amt-rub   NE tt-term-obl-gar.amt-rub
         OR tt-term-obl-gar-old.end-date  NE tt-term-obl-gar.end-date
         OR tt-term-obl-gar-old.fop-date  NE tt-term-obl-gar.fop-date       
         THEN
         DO:
            UpdateSigns("loan",
               tt-term-obl-gar.contract + "," + tt-term-obl-gar.cont-code,
               "BKIStatus",
               "ПОДГ",
               YES).
         END.      
      END.
   END. 
   
   vSurr     = tt-term-obl-gar.contract         + "," +
               tt-term-obl-gar.cont-code        + "," +
               STRING(tt-term-obl-gar.idnt)     + "," +
               STRING(tt-term-obl-gar.end-date) + "," +
               STRING(tt-term-obl-gar.nn).

   IF     iMode        EQ {&MOD_EDIT}
      AND mPrevEndDate NE tt-term-obl-gar.end-date THEN
   DO:
      vSurrOld  = tt-term-obl-gar.contract         + "," +
                  tt-term-obl-gar.cont-code        + "," +
                  STRING(tt-term-obl-gar.idnt)     + "," +
                  STRING(mPrevEndDate)             + "," +
                  STRING(mPrevNN).

      lLink:
      DO ON ERROR UNDO lLink, RETRY lLink:
         IF RETRY THEN
         DO:
            vMsg = "Ошибка при обновлении дополнительных связей обеспечения".
            UNDO lLink, RETURN ERROR vMsg.
         END.

         FOR EACH bXlink WHERE bXlink.class-code EQ "term-obl-gar"
                         NO-LOCK,
            EACH bLink WHERE bLink.link-id   EQ bXlink.link-id
                         AND bLink.source-id EQ vSurrOld
                        NO-LOCK
                        ON ERROR UNDO lLink, RETRY lLink:

            FIND FIRST bNewLink WHERE ROWID(bNewLink) EQ ROWID(bLink)
                                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF LOCKED(bNewLink) THEN
            DO:
               vMsg = SUBST('Дополнительаная связь "&1" редактируется другим пользователем',
                            bXlink.link-code).
               WhoLocks2(RECID(bLink), "links", INPUT-OUTPUT vMsg).
               UNDO lLink, RETURN ERROR vMsg.

            END. /* IF LOCKED(bNewLink) THEN */
            ELSE IF AVAIL(bNewLink) THEN
            DO:
               ASSIGN
                  bNewLink.source-id = vSurr
               .
               RELEASE bNewLink.
            END. /* ELSE IF AVAIL(bNewLink) THEN */
         END. /* FOR EACH bXlink WHERE bXlink.class-code EQ "term-obl-gar" */

         FOR EACH bXlink WHERE bXlink.class-code EQ "term-obl-gar"
                         NO-LOCK,
            EACH bLink WHERE bLink.link-id   EQ bXlink.link-id
                         AND bLink.target-id EQ vSurrOld
                        NO-LOCK
                        ON ERROR UNDO lLink, RETRY lLink:

            FIND FIRST bNewLink WHERE ROWID(bNewLink) EQ ROWID(bLink)
                                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF LOCKED(bNewLink) THEN
            DO:
               vMsg = SUBST('Дополнительаная связь "&1" редактируется другим пользователем',
                            bXlink.link-code).
               WhoLocks2(RECID(bLink), "links", INPUT-OUTPUT vMsg).
               UNDO lLink, RETURN ERROR vMsg.

            END. /* IF LOCKED(bNewLink) THEN */
            ELSE IF AVAIL(bNewLink) THEN
            DO:
               ASSIGN
                  bNewLink.target-id = vSurr
               .
               RELEASE bNewLink.
            END. /* ELSE IF AVAIL(bNewLink) THEN */
         END. /* FOR EACH bXlink WHERE bXlink.class-code EQ "term-obl-gar" */

      END. /* lLink: */

      tr:
      DO ON ERROR  UNDO, RETRY
      ON ENDKEY UNDO, LEAVE tr:
            /* Обновляем коэффициенты остаточной стоимости в comm-rate */
         FOR EACH comm-rate WHERE
                  comm-rate.kau        EQ vSurrOld
            AND   comm-rate.acct       EQ "0"
            AND   comm-rate.currency   EQ term-obl.currency
            AND   comm-rate.min-value  EQ 0.00
            AND   comm-rate.period     EQ 0
         NO-LOCK:
            FIND FIRST b-comm-rate WHERE
                 RECID(b-comm-rate) EQ RECID(comm-rate)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL b-comm-rate THEN
            DO:
               RUN wholocks2.p (RECID(comm-rate),"comm-rate" ,
                                program-name(1) +
                                '~n Ошибка при монопольном доступе к ' +
                                'коэффициенту остаточной стоимости ' + vSurr).
               RETURN {&RET-ERROR}.
            END.
            ELSE
               b-comm-rate.kau = vSurr.
         END. /* FOR EACH comm-rate */
         RELEASE comm-rate.
         RELEASE b-comm-rate.

            /* Обновляем индивидуальные стоимости в instr-rate */
         FOR EACH instr-rate WHERE
                  instr-rate.instr-cat  EQ "collateral_value"
            AND   instr-rate.rate-type  EQ "fair_value"
            AND   instr-rate.instr-code EQ vSurrOld
         NO-LOCK:
            FIND FIRST b-instr-rate WHERE
                 RECID(b-instr-rate) EQ RECID(instr-rate)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL b-instr-rate THEN
            DO:
               RUN wholocks2.p (RECID(instr-rate),"instr-rate" ,
                                program-name(1) +
                                '~n Ошибка при монопольном доступе к ' +
                                'индивидуальной стоимости обеспечения ' +
                                vSurr).
               RETURN {&RET-ERROR}.
            END.
            ELSE
               b-instr-rate.instr-code = vSurr.
         END. /* FOR EACH instr-rate */
         RELEASE instr-rate.
         RELEASE b-instr-rate.
      END.
   END.  /* mPrevEndDate NE tt-term-obl-gar.end-date */
   IF LOOKUP(mMarkModeV, mTemp-String1) EQ 2 THEN
   DO:

      FIND FIRST instr-rate WHERE
                 instr-rate.instr-cat  EQ "collateral_value"
         AND     instr-rate.rate-type  EQ "fair_value"
         AND     instr-rate.instr-code EQ vSurr
         AND     instr-rate.since      EQ ( IF tt-term-obl-gar.datapost$ NE ?
                                              THEN DATE(tt-term-obl-gar.datapost$)
                                              ELSE tt-term-obl-gar.fop-date)
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF NOT AVAIL instr-rate THEN
      DO:
         FIND FIRST instr-rate WHERE
                    instr-rate.instr-cat  EQ "collateral_value"
            AND     instr-rate.rate-type  EQ "fair_value"
            AND     instr-rate.instr-code EQ vSurr
            AND     instr-rate.since      EQ
                         ( IF tt-term-obl-gar.datapost$ NE ?
                          THEN DATE(tt-term-obl-gar.datapost$)
                          ELSE tt-term-obl-gar.fop-date)
         NO-LOCK NO-ERROR.
         IF AVAIL instr-rate THEN
         DO:
            RUN wholocks2.p (RECID(instr-rate),"instr-rate" ,program-name(1) +
                             '~n Ошибка при монопольном доступе к ' +
                             'индивидуальной стоимости обеспечения ' + vSurr).
            RETURN {&RET-ERROR}.
         END.
         ELSE
         DO:
            CREATE instr-rate.
            ASSIGN
               instr-rate.instr-cat  = "collateral_value"
               instr-rate.rate-type  = "fair_value"
               instr-rate.instr-code = vSurr
               instr-rate.since      = ( IF tt-term-obl-gar.datapost$ NE ?
                                        THEN DATE(tt-term-obl-gar.datapost$)
                                        ELSE tt-term-obl-gar.fop-date)
               instr-rate.rate-instr = tt-term-obl-gar.amt-rub     /* Стоимость */
               instr-rate.per        = tt-term-obl-gar.sop-offbal  /* "за" = "количество" */
            .
         END.
      END.
      ELSE
      DO:
         IF instr-rate.rate-instr NE tt-term-obl-gar.amt-rub THEN
         DO:
            DO ON ERROR UNDO, LEAVE:
               pick-value = ?.
               RUN FIll-sysmes IN h_tmess ("", "", "4",
                                           "Корректировать индивидуальную стоимость?").
               IF pick-value EQ "YES" THEN
                  ASSIGN
                     instr-rate.instr-code = vSurr
                     instr-rate.rate-instr = tt-term-obl-gar.amt-rub
                     instr-rate.per        = tt-term-obl-gar.sop-offbal
                  .
            END.
         END.
      END.
   END.
   IF iMode EQ {&MOD_ADD}
   THEN
      Set_QualityGar("comm-rate",
                     vSurr,
                     mQualityCategory).
      /* Ищем категорию качества */
   FOR EACH comm-rate WHERE
            comm-rate.commission EQ "КачОбеспеч"
      AND   comm-rate.acct       EQ "0"
      AND   comm-rate.currency   EQ tt-term-obl-gar.currency
      AND   comm-rate.kau        EQ vSurr
      AND   comm-rate.min-value  EQ 0
      AND   comm-rate.period     EQ 0
      AND   comm-rate.since      LE gend-date
   NO-LOCK BY comm-rate.since DESCENDING:
      LEAVE.
   END.
      /* Если он есть, то определяем значение вида обеспечения по соответствующему ДР */
   IF AVAIL comm-rate THEN
   DO:
      UpdateSigns ("comm-rate",
                   GetSurrogateBuffer("comm-rate",(BUFFER comm-rate:HANDLE)),
                   "Ф117_ВидОб",
                   mVidObespech,
                   NO).
   END.
   IF    iMode EQ {&MOD_EDIT}
      OR iMode EQ {&MOD_ADD} THEN
   DO:
      UpdateSigns("term-obl", vSurr, "ВалУчетаОбесп", IF mKeepCurrV EQ "ДогВал" THEN ? ELSE "810", ?).
      UpdateSigns("term-obl", vSurr,  "КурсВалОбесп", STRING(mCurRate), ?).
      UpdateSigns("term-obl", vSurr,   "СуммаНацВал", STRING(mSumRUR),  ?).
   END.
   
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
      /* Для того, чтобы сохранить из временной таблицы значения реквизитов,
      ** не объектами полями, нужно добавить их в список след. образом : */
   SetFormDefList(GetFormDefList() + 
                  ",tt-term-obl-gar.plavob$,tt-term-obl-gar.AgrCounter").

   IF iMode EQ {&MOD_ADD} THEN
      ASSIGN
         tt-term-obl-gar.class-code      = iClass
         tt-term-obl-gar.contract        = iContract
         tt-term-obl-gar.cont-code       = iContcode
         tt-term-obl-gar.idnt            = 5
      .
   IF    iMode EQ {&MOD_ADD}
      OR (iMode EQ {&MOD_EDIT}
        AND mPrevEndDate NE tt-term-obl-gar.end-date
         ) THEN
   DO:
      FOR LAST b-term-obl WHERE
               b-term-obl.contract  EQ iContract
         AND   b-term-obl.cont-code EQ iContcode
         AND   b-term-obl.idnt      EQ 5
         AND   b-term-obl.end-date  EQ tt-term-obl-gar.end-date
      NO-LOCK BY b-term-obl.nn:
         tt-term-obl-gar.nn = b-term-obl.nn + 1.
      END.
   END.
   IF    iMode EQ {&MOD_EDIT}
      OR iMode EQ {&MOD_ADD} THEN
   DO:
      tt-term-obl-gar.fop-offbal = LOOKUP(mMarkModeV:SCREEN-VALUE IN FRAME fMain, mTemp-String1) - 1.
      IF mSetFI THEN
         tt-term-obl-gar.cor-acct   = ENTRY(LOOKUP(mFICategoryV, mTemp-String), mTemp-String2).
      ELSE
            /* обнуляем значения ДР фин. инструментов */
      ASSIGN
            tt-term-obl-gar.cor-acct = ""
            tt-term-obl-gar.fuser-id = ""
            tt-term-obl-gar.suser-id = ""
      .
   END.
   IF mCounterValue GT 0 THEN
   DO:
     IF NOT UpdateSigns("term-obl",
                         SUBST("&1,&2,&3,&4,&5",
                               tt-term-obl-gar.contract,
                               tt-term-obl-gar.cont-code,
                               STRING(tt-term-obl-gar.idnt),
                               STRING(tt-term-obl-gar.end-date),
                               STRING(tt-term-obl-gar.nn)),
                         "AgrCounter",
                         STRING(mCounterValue),
                         NO) THEN
         RUN Fill-SysMes IN h_tmess ("","","",
                                     "Ошибка сохранения ДР AgrCounter").
      ELSE
         tt-term-obl-gar.AgrCounter = STRING(mCounterValue).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='24/03/2016 13:17:38.773+04:00' */
/* $LINTUSER='miam' */
/* $LINTMODE='1' */
/* $LINTFILE='f-l-gar.p' */
/*prosign/a2tl8Ai6fn+5mNbgb3Wgg*/
/* --- f-l-gar.p was humbly modified by (c)blodd converter v.1.11 on 6/27/2017 2:41pm --- */
