&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-acct NO-UNDO LIKE acct
       FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
       FIELD alw#tnomer$ AS CHARACTER /* АльтНомер */
       FIELD datasoobwtls$ AS CHARACTER /* ДатаСообщЛС */
       FIELD doverotkrswc$ AS CHARACTER /* ДоверОткрСч */
       FIELD dogotkrls$ AS CHARACTER /* ДогОткрЛС */
       FIELD dohkomissii$ AS CHARACTER /* ДохКомиссии */
       FIELD internet$ AS LOGICAL /* Интернет */
       FIELD istpostupl$ AS CHARACTER /* ИстПоступл */
       FIELD korpkl$ AS CHARACTER /* КорпКл */
       FIELD mbr$ AS LOGICAL /* МБР */
       FIELD mval$ AS DECIMAL /* МВал */
       FIELD minswcetwcik$ AS INT64 /* МинСчетчик */
       FIELD nezak$ AS LOGICAL /* НеЗак */
       FIELD nerek$ AS CHARACTER /* НеРек */
       FIELD novaciwa$ AS CHARACTER /* Новация */
       FIELD nomdog$ AS CHARACTER /* НомДог */
       FIELD odnins$ AS LOGICAL /* ОднИнс */
       FIELD operposwc$ AS CHARACTER /* ОперПоСч */
       FIELD pereocotr$ AS CHARACTER /* ПереоцОтр */
       FIELD pereocpol$ AS CHARACTER /* ПереоцПол */
       FIELD porvydvypis$ AS CHARACTER /* ПорВыдВыпис */
       FIELD primls$ AS CHARACTER /* ПримЛС */
       FIELD priwcinablok$ AS CHARACTER /* ПричинаБлок */
       FIELD priwcinazakrswc$ AS CHARACTER /* ПричинаЗакрСч */
       FIELD procprivl$ AS CHARACTER /* ПроцПривл */
       FIELD rezerv$ AS CHARACTER /* Резерв */
       FIELD rolw#swcpov$ AS CHARACTER /* РольСчПОВ */
       FIELD svodnswc$ AS CHARACTER /* СводнСч */
       FIELD svwaz_swc$ AS CHARACTER /* Связ_сч */
       FIELD sv_swc_881$ AS CHARACTER /* Св_сч_881 */
       FIELD skonsalw#do$ AS LOGICAL /* СКонСальдо */
       FIELD sotrotkrswc$ AS CHARACTER /* СотрОткрСч */
       FIELD sotrutvswc$ AS CHARACTER /* СотрУтвСч */
       FIELD srok$ AS CHARACTER /* Срок */
       FIELD swcetnu$ AS CHARACTER /* СчетНУ */
       FIELD swcetpk$ AS LOGICAL /* СчетПК */
       FIELD swc_proc$ AS CHARACTER /* Сч_проц */
       FIELD haroper$ AS CHARACTER /* ХарОпер */
       FIELD celw#kred$ AS CHARACTER /* ЦельКред */
       FIELD Acct-DBI AS CHARACTER /* Acct-DBI */
       FIELD acct-def AS LOGICAL /* acct-def */
       FIELD acct-exept AS LOGICAL /* acct-exept */
       FIELD bank-inn AS LOGICAL /* bank-inn */
       FIELD cls-op-templ AS CHARACTER /* cls-op-templ */
       FIELD confiden AS CHARACTER /* confiden */
       FIELD DocRefL AS CHARACTER /* DocRefL */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD form-type-code AS CHARACTER /* form-type-code */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD IDCust AS CHARACTER /* IDCust */
       FIELD Kodtip_sch AS CHARACTER /* Kodtip_sch */
       FIELD kraw AS CHARACTER /* kraw */
       FIELD name-acct AS CHARACTER /* name-acct */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD SourceOfReceipt AS CHARACTER /* SourceOfReceipt */
       FIELD sv_swcet_1571$ AS CHARACTER /* Св_счет_1571 */
       FIELD unkg$ AS INT64 /* УНКг */
       FIELD msfo-acct AS CHARACTER /* msfo-acct */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD msfo-rez AS CHARACTER /* msfo-rez */
       FIELD swcet_1571$ AS CHARACTER /* Счет_1571 */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       FIELD pribubyt446$ AS CHARACTER /* ПрибУбыт446 */

       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-acct" "" }
       .
DEFINE TEMP-TABLE tt-deputy NO-UNDO LIKE deputy
       FIELD datanazn$ AS DATE /* ДатаНазн */
       FIELD dataotm$ AS DATE /* ДатаОтм */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */

       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-deputy" "deputy" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-ACCT.P
      Comment: (0078824) Адаптирпован для работы в
               Биссмарт
   Parameters:
         Uses:
      Used by:
      Created: 15.10.2005 20:10 Om
     Modified: 15.10.2005 20:10 Om       
     Modified: 05.07.2007 15:25 KSV      (0078824) Адаптирпован для работы в
                                         Биссмарт
     Modified: 27.07.2007 14:53 ariz     (0078434) Ошибка: не подставлялся УНК
                                         при создании счета из под субъекта.
     Modified: 20.08.2007 21:00 KSV      (0081029) Локальный триггер на GO для
                                         ряда полей отключен для Биссмарт
     Modified: 30.10.2007 12:44 MUTA     (0082120) Реализация количественного учета
                                         ценных бумаг в Базовом модуле
     Modified: 08.04.2008 MUTA 0090931   Доработка инструмента acct-qty.
     Modified: 29.05.2008 15:31 KSV      (0085464) Биссмарт. Исправлено
                                         появление join-меню при покидании
                                         полей CURRENCY и ACCT
     Modified: 06.10.2009 14:31 ksv      (0118166) QBIS. незнач. исправление
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

{intrface.get separate}
{intrface.get acct}     /* Библиотека для работы со счетами. */
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */
{intrface.get kau}      /* Библиотека для работы с субсчетами. */
{intrface.get terr}
{intrface.get brnch}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get blkob}    /* Библиотека для работы с блокировками объектов. */
{intrface.get op}
{sh-defs.i}             /* Переменные для рассчета остатка по счету. */
{accttok.def}           /* Объявления констант используемых в маске. */

{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}


DEF VAR mFldh     AS HANDLE NO-UNDO.
DEF VAR mAcctMask AS CHAR   NO-UNDO. /* Маска счета. */
DEF VAR mTokAcct  AS CHAR   NO-UNDO. /* Токен счета. */
DEF VAR mTokIdx   AS CHAR   NO-UNDO.
DEF VAR mTokLen   AS CHAR   NO-UNDO.
DEF VAR mKauId    AS CHAR   NO-UNDO. /* Код шаблона КАУ. */
DEF VAR mFlagUnk  AS LOG    NO-UNDO. /* отображать ли УНК или код клиента (cust-id) */
DEF VAR mDateAcctQty AS DATE NO-UNDO.
DEF VAR mAccessMask  AS CHAR NO-UNDO. /*Маска счетов, для которых выводится доступный остаток */
DEF VAR mAccessContAcct AS CHAR NO-UNDO.
DEF VAR vEditSet  AS CHAR   NO-UNDO. 
DEF VAR vCustId-ScV   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate446 AS DATE   NO-UNDO.   
mDate446 = DATE(FGetSetting("ЦБ446П","Дата446П","01/01/2016")).

DEFINE TEMP-TABLE t-obj NO-UNDO
   FIELD rec AS RECID.

DEFINE TEMP-TABLE ttBTTUsed NO-UNDO LIKE bis-temp-table.

        /* Всегда отображать браузер ДР. */
&GLOBAL-DEFINE XATT-ED-ON
/* список полей для проверки предустановленного значения в режиме создания
** (для заблокированных полей формы) */
&GLOBAL-DEFINE INITIAL-VALID-LIST tt-acct.bal-acct
/* проверять права на группы реквизитов */
&GLOBAL-DEFINE CHECK-XATTR-GROUP-PERMISSION

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id tt-acct.rate-type
&Scoped-define ENABLED-TABLES tt-acct
&Scoped-define FIRST-ENABLED-TABLE tt-acct
&Scoped-Define ENABLED-OBJECTS unk$ mNrAcctCur vAcctCur mNrAcctPos vAcctPos ~
vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose mNrShAvCur ~
vAvCur mNrShAvPos vAvPos mNrShValNa sh-val-na mNrShBalNa sh-bal-na mChoice ~
sh-qty-close sh-qty-na
&Scoped-Define DISPLAYED-FIELDS tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id tt-acct.bal-acct tt-acct.currency tt-acct.acct ~
tt-acct.rate-type tt-acct.open-date tt-acct.close-date tt-acct.last-date ~
tt-acct.Details tt-acct.alw#tnomer$ tt-acct.contract tt-acct.contr-acct ~
tt-acct.user-id tt-acct.check-op
&Scoped-define DISPLAYED-TABLES tt-acct
&Scoped-define FIRST-DISPLAYED-TABLE tt-acct
&Scoped-Define DISPLAYED-OBJECTS vNameBranch vNameClient unk$ vSide ~
vNameUser vNameBlock vNameControl mKau-id vNameKau separator1 separator2 ~
separator3 separator4 vDateAcctPos mNrAcctCur vAcctCur mNrAcctPos vAcctPos ~
vDateDocAk vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose ~
mNrShAvCur vAvCur mNrShAvPos vAvPos vDateDocNAk mNrShValNa sh-val-na ~
mNrShBalNa sh-bal-na vDateDocAv mChoice sh-qty-close sh-qty-na

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id ~
unk$ tt-acct.bal-acct tt-acct.currency tt-acct.acct tt-acct.rate-type ~
tt-acct.open-date tt-acct.Details tt-acct.contract tt-acct.contr-acct ~
tt-acct.user-id tt-acct.check-op mKau-id tt-acct.pribubyt446$ 
&Scoped-define List-2 tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id ~
unk$ tt-acct.rate-type tt-acct.open-date tt-acct.close-date tt-acct.Details ~
tt-acct.contract tt-acct.contr-acct tt-acct.user-id tt-acct.check-op mKau-id ~
tt-acct.pribubyt446$ 
&Scoped-define List-3 fMain tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id unk$ tt-acct.bal-acct tt-acct.currency tt-acct.acct ~
tt-acct.open-date tt-acct.last-date tt-acct.Details tt-acct.contract ~
tt-acct.contr-acct tt-acct.user-id tt-acct.check-op mKau-id tt-acct.pribubyt446$ 
&Scoped-define List-4 tt-acct.acct

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE vSide AS CHARACTER FORMAT "X":U
     LABEL "Актив/пассив"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "А","П","-"
     DROP-DOWN-LIST
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameClient AS CHARACTER
     VIEW-AS EDITOR
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 3
     &ELSE SIZE 61 BY 3 &ENDIF NO-UNDO.

DEFINE VARIABLE mChoice AS CHARACTER FORMAT "x(10)" INITIAL "В инвалюте"
      VIEW-AS TEXT
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF.

DEFINE VARIABLE mKau-id AS CHARACTER FORMAT "x(12)" INITIAL ?
     LABEL "Код шабл.КАУ"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF.

DEFINE VARIABLE mNrAcctCur AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrAcctPos AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShAvCur AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShAvPos AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShBalClose AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShBalNa AS CHARACTER FORMAT "x(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShValClose AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShValNa AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
     &ELSE SIZE 3 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U
     LABEL "ОСТАТОК"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
     &ELSE SIZE 13 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator3 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator4 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-bal-close AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-bal-na AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-qty-close AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-qty-na AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-val-close AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-val-na AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE unk$ AS CHARACTER FORMAT "X(256)":U
     LABEL "УНК"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctCur AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctPos AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctQty AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAvCur AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAvPos AS DECIMAL FORMAT "Дбzz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateAcctPos AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocAk AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocAv AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocNAk AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameBlock AS LOGICAL FORMAT "Блокирован/":U INITIAL NO
     LABEL "Статус"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameBranch AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 49 BY 1
     &ELSE SIZE 49 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameControl AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 31 BY 1
     &ELSE SIZE 31 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameKau AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 48 BY 1
     &ELSE SIZE 48 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameUser AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 48 BY 1
     &ELSE SIZE 48 BY 1 &ENDIF NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-acct.branch-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 16 COLON-ALIGNED
          &ELSE AT ROW 1 COL 16 COLON-ALIGNED &ENDIF HELP
          "Код подразделения"
          LABEL "Подразделение" FORMAT "x(10)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vNameBranch
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 27 COLON-ALIGNED
          &ELSE AT ROW 1 COL 27 COLON-ALIGNED &ENDIF NO-LABEL
     tt-acct.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF FORMAT "X"
          VIEW-AS COMBO-BOX INNER-LINES 5
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-acct.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 40 COLON-ALIGNED
          &ELSE AT ROW 2 COL 40 COLON-ALIGNED &ENDIF HELP
          "Порядковый номер клиента (нажмите F1 для выбора)"
          LABEL "Клиент N" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vNameClient
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18
          &ELSE AT ROW 3 COL 18 &ENDIF NO-LABEL
     unk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 40 COLON-ALIGNED
          &ELSE AT ROW 2 COL 40 COLON-ALIGNED &ENDIF
     tt-acct.bal-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер балансового счета"
          LABEL "Счет 2 порядка" FORMAT "99999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     vSide
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 40 COLON-ALIGNED
          &ELSE AT ROW 6 COL 40 COLON-ALIGNED &ENDIF HELP
          "Активный (А), пассивный (П) или активно-пассивный (-) счет"
     tt-acct.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 65 COLON-ALIGNED
          &ELSE AT ROW 6 COL 65 COLON-ALIGNED &ENDIF HELP
          "Код валюты"
          LABEL "Валюта" FORMAT "xxx"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-acct.acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 16 COLON-ALIGNED
          &ELSE AT ROW 7 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер лицевого счета"
          LABEL "Счет" FORMAT "x(25)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF
     tt-acct.rate-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 65 COLON-ALIGNED
          &ELSE AT ROW 7 COL 65 COLON-ALIGNED &ENDIF HELP
          "Код типа курса переоценки счета"
          LABEL "    Тип курса" FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-acct.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF HELP
          "Дата открытия счета"
          LABEL "Открыт" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-acct.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 40 COLON-ALIGNED
          &ELSE AT ROW 8 COL 40 COLON-ALIGNED &ENDIF HELP
          "Дата закрытия счета"
          LABEL "Закрыт" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-acct.last-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 65 COLON-ALIGNED
          &ELSE AT ROW 8 COL 65 COLON-ALIGNED &ENDIF HELP
          "Дата последнего изменения"
          LABEL "Дата изм." FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-acct.Details
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 4
          &ELSE AT ROW 9 COL 4 &ENDIF HELP
          "Наименование счета"
          LABEL "Наименование"
          VIEW-AS EDITOR
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 2
          &ELSE SIZE 61 BY 2 &ENDIF
     tt-acct.alw#tnomer$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "Альтернативный номер"
          LABEL "Альт. номер" FORMAT "x(20)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 19 BY 1 &ENDIF
     tt-acct.contract
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 65 COLON-ALIGNED
          &ELSE AT ROW 11 COL 65 COLON-ALIGNED &ENDIF HELP
          "Код назначения счета"
          LABEL "Назначение" FORMAT "x(6)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
     tt-acct.pribubyt446$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 65 COLON-ALIGNED
          &ELSE AT ROW 12 COL 65 COLON-ALIGNED &ENDIF HELP
          "Символ дохода/расхода"
          LABEL "Символ" FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-acct.contr-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 16 COLON-ALIGNED
          &ELSE AT ROW 12 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер парного счета"
          LABEL "Парный счет" FORMAT "x(25)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
     tt-acct.user-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 16 COLON-ALIGNED
          &ELSE AT ROW 13 COL 16 COLON-ALIGNED &ENDIF HELP
          "Код ответственного исполнителя"
          LABEL "Ответственный" FORMAT "xxxx"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     vNameUser
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 29 COLON-ALIGNED
          &ELSE AT ROW 13 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     vNameBlock
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 16 COLON-ALIGNED
          &ELSE AT ROW 14 COL 16 COLON-ALIGNED &ENDIF
     tt-acct.check-op
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 37 COLON-ALIGNED
          &ELSE AT ROW 14 COL 37 COLON-ALIGNED &ENDIF HELP
          "Тип контроля операций по данному лицевому счету"
          LABEL "Контроль" FORMAT "x(8)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     vNameControl
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 46 COLON-ALIGNED
          &ELSE AT ROW 14 COL 46 COLON-ALIGNED &ENDIF NO-LABEL
     mKau-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 16 COLON-ALIGNED
          &ELSE AT ROW 15 COL 16 COLON-ALIGNED &ENDIF HELP
          "Номер шаблона КАУ"
     vNameKau
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 29 COLON-ALIGNED
          &ELSE AT ROW 15 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 1
          &ELSE AT ROW 16 COL 1 &ENDIF NO-LABEL
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 12 COLON-ALIGNED
          &ELSE AT ROW 16 COL 12 COLON-ALIGNED &ENDIF
     separator3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 37 COLON-ALIGNED
          &ELSE AT ROW 16 COL 37 COLON-ALIGNED &ENDIF NO-LABEL
     separator4
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 67 COLON-ALIGNED
          &ELSE AT ROW 16 COL 67 COLON-ALIGNED &ENDIF NO-LABEL
     vDateAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 17 COLON-ALIGNED
          &ELSE AT ROW 17 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mNrAcctCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 54 COLON-ALIGNED
          &ELSE AT ROW 17 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 17 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocAk
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 17 COLON-ALIGNED
          &ELSE AT ROW 18 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctQty
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-val-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShValClose
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-bal-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 18 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     mNrShBalClose
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 54 COLON-ALIGNED
          &ELSE AT ROW 18 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShAvCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 29 COLON-ALIGNED
          &ELSE AT ROW 20 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     vAvCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 29 COLON-ALIGNED
          &ELSE AT ROW 20 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShAvPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 54 COLON-ALIGNED
          &ELSE AT ROW 20 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     vAvPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 20 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocNAk
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 17 COLON-ALIGNED
          &ELSE AT ROW 19 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShValNa
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-val-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShBalNa
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 54 COLON-ALIGNED
          &ELSE AT ROW 19 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     sh-bal-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 19 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocAv
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 17 COLON-ALIGNED
          &ELSE AT ROW 20 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mChoice
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 26 COLON-ALIGNED
          &ELSE AT ROW 16 COL 26 COLON-ALIGNED &ENDIF NO-LABEL
     sh-qty-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-qty-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     "Клиент:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 10
          &ELSE AT ROW 3 COL 10 &ENDIF
     " На закр. день:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 1
          &ELSE AT ROW 17 COL 1 &ENDIF
     "В нац. валюте" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 55
          &ELSE AT ROW 16 COL 55 &ENDIF
     "+ неакц.  док.: П" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 1
          &ELSE AT ROW 19 COL 1 &ENDIF
     "+ акцепт. док.: √" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 1
          &ELSE AT ROW 18 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "     Доступный:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 1
          &ELSE AT ROW 20 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-acct T "?" NO-UNDO bisquit acct
      ADDITIONAL-FIELDS:
          FIELD akt_vzv$ AS CHARACTER /* Акт_взв */
          FIELD alw#tnomer$ AS CHARACTER /* АльтНомер */
          FIELD datasoobwtls$ AS CHARACTER /* ДатаСообщЛС */
          FIELD doverotkrswc$ AS CHARACTER /* ДоверОткрСч */
          FIELD dogotkrls$ AS CHARACTER /* ДогОткрЛС */
          FIELD dohkomissii$ AS CHARACTER /* ДохКомиссии */
          FIELD internet$ AS LOGICAL /* Интернет */
          FIELD istpostupl$ AS CHARACTER /* ИстПоступл */
          FIELD korpkl$ AS CHARACTER /* КорпКл */
          FIELD mbr$ AS LOGICAL /* МБР */
          FIELD mval$ AS DECIMAL /* МВал */
          FIELD minswcetwcik$ AS INT64 /* МинСчетчик */
          FIELD nezak$ AS LOGICAL /* НеЗак */
          FIELD nerek$ AS CHARACTER /* НеРек */
          FIELD novaciwa$ AS CHARACTER /* Новация */
          FIELD nomdog$ AS CHARACTER /* НомДог */
          FIELD odnins$ AS LOGICAL /* ОднИнс */
          FIELD operposwc$ AS CHARACTER /* ОперПоСч */
          FIELD pereocotr$ AS CHARACTER /* ПереоцОтр */
          FIELD pereocpol$ AS CHARACTER /* ПереоцПол */
          FIELD porvydvypis$ AS CHARACTER /* ПорВыдВыпис */
          FIELD primls$ AS CHARACTER /* ПримЛС */
          FIELD priwcinablok$ AS CHARACTER /* ПричинаБлок */
          FIELD priwcinazakrswc$ AS CHARACTER /* ПричинаЗакрСч */
          FIELD procprivl$ AS CHARACTER /* ПроцПривл */
          FIELD rezerv$ AS CHARACTER /* Резерв */
          FIELD rolw#swcpov$ AS CHARACTER /* РольСчПОВ */
          FIELD svodnswc$ AS CHARACTER /* СводнСч */
          FIELD svwaz_swc$ AS CHARACTER /* Связ_сч */
          FIELD sv_swc_881$ AS CHARACTER /* Св_сч_881 */
          FIELD skonsalw#do$ AS LOGICAL /* СКонСальдо */
          FIELD sotrotkrswc$ AS CHARACTER /* СотрОткрСч */
          FIELD sotrutvswc$ AS CHARACTER /* СотрУтвСч */
          FIELD srok$ AS CHARACTER /* Срок */
          FIELD swcetnu$ AS CHARACTER /* СчетНУ */
          FIELD swcetpk$ AS LOGICAL /* СчетПК */
          FIELD swc_proc$ AS CHARACTER /* Сч_проц */
          FIELD haroper$ AS CHARACTER /* ХарОпер */
          FIELD celw#kred$ AS CHARACTER /* ЦельКред */
          FIELD Acct-DBI AS CHARACTER /* Acct-DBI */
          FIELD acct-def AS LOGICAL /* acct-def */
          FIELD acct-exept AS LOGICAL /* acct-exept */
          FIELD bank-inn AS LOGICAL /* bank-inn */
          FIELD cls-op-templ AS CHARACTER /* cls-op-templ */
          FIELD confiden AS CHARACTER /* confiden */
          FIELD DocRefL AS CHARACTER /* DocRefL */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD form-type-code AS CHARACTER /* form-type-code */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD IDCust AS CHARACTER /* IDCust */
          FIELD Kodtip_sch AS CHARACTER /* Kodtip_sch */
          FIELD kraw AS CHARACTER /* kraw */
          FIELD name-acct AS CHARACTER /* name-acct */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD SourceOfReceipt AS CHARACTER /* SourceOfReceipt */
          FIELD sv_swcet_1571$ AS CHARACTER /* Св_счет_1571 */
          FIELD unkg$ AS INT64 /* УНКг */
          FIELD msfo-acct AS CHARACTER /* msfo-acct */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD msfo-rez AS CHARACTER /* msfo-rez */
          FIELD swcet_1571$ AS CHARACTER /* Счет_1571 */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          FIELD pribubyt446$ AS CHARACTER /* ПрибУбыт446 */

          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-acct" "" }

      END-FIELDS.
      TABLE: tt-deputy T "?" NO-UNDO bisquit deputy
      ADDITIONAL-FIELDS:
          FIELD datanazn$ AS DATE /* ДатаНазн */
          FIELD dataotm$ AS DATE /* ДатаОтм */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */

          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-deputy" "deputy" }

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
         HEIGHT             = 23.07
         WIDTH              = 79.86
         MAX-HEIGHT         = 23.07
         MAX-WIDTH          = 79.86
         VIRTUAL-HEIGHT     = 23.07
         VIRTUAL-WIDTH      = 79.86
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
   FRAME-NAME 3 Custom                                                  */
/* SETTINGS FOR FILL-IN tt-acct.acct IN FRAME fMain
   NO-ENABLE 1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.alw#tnomer$ IN FRAME fMain
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-acct.bal-acct IN FRAME fMain
   NO-ENABLE 1 3 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN tt-acct.branch-id IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-acct.check-op IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.close-date IN FRAME fMain
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN tt-acct.contr-acct IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.contract IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
ASSIGN
       tt-acct.contract:SELECTABLE IN FRAME fMain       = TRUE.

/* SETTINGS FOR FILL-IN tt-acct.pribubyt446$ IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.currency IN FRAME fMain
   NO-ENABLE 1 3 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR COMBO-BOX tt-acct.cust-cat IN FRAME fMain
   1 2 3 EXP-FORMAT                                                     */
/* SETTINGS FOR FILL-IN tt-acct.cust-id IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR EDITOR tt-acct.Details IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.last-date IN FRAME fMain
   NO-ENABLE 3 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN mKau-id IN FRAME fMain
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN tt-acct.open-date IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.rate-type IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN separator3 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN separator4 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sh-bal-close IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN sh-bal-na IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN unk$ IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-acct.user-id IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN vAcctPos IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN vAvPos IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN vDateAcctPos IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocAk IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocAv IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocNAk IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameBlock IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameBranch IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR vNameClient IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN
       vNameClient:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN vNameControl IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameKau IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameUser IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX vSide IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain TERMINAL-SIMULATION
ON ENTRY OF FRAME fMain
DO:
   DEFINE VAR vCustNameLine AS CHARACTER NO-UNDO.
   IF iMode EQ {&MOD_EDIT} THEN
   DO:
      RUN chk-blk.p (tt-acct.cust-cat:SCREEN-VALUE,tt-acct.cust-id:SCREEN-VALUE).
      IF RETURN-VALUE EQ "0" THEN DO:
         {getcustline.i &cust-cat = "tt-acct.cust-cat:SCREEN-VALUE" &cust-id = "tt-acct.cust-id:SCREEN-VALUE" &output-to = "vCustNameLine"}
         RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(tt-acct.number,GetAcctFmt(tt-acct.acct-cat))).
         APPLY "ESC".
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain TERMINAL-SIMULATION
ON F3 OF FRAME fMain ANYWHERE
DO:
IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code") THEN  DO:
   IF mChoice:SCREEN-VALUE EQ "Остаток ЦБ"
   THEN DO:
      ASSIGN
         vAcctQty:VISIBLE     = NO
         sh-qty-close:VISIBLE = NO
         sh-qty-na:VISIBLE    = NO
         vAcctCur:VISIBLE     = tt-acct.currency  NE ""
         sh-val-close:VISIBLE = tt-acct.currency  NE ""
         sh-val-na:VISIBLE    = tt-acct.currency  NE ""
         vAvCur:VISIBLE       =  tt-acct.currency  NE "" AND {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                         AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
         vAcctCur:SCREEN-VALUE     = string(vAcctCur)
         sh-val-close:SCREEN-VALUE = string(sh-val-close)
         sh-val-na:SCREEN-VALUE    = string(sh-val-na)
         vAvCur:SCREEN-VALUE       = STRING(vAvCur)
         mChoice:SCREEN-VALUE      = "В инвалюте"
      NO-ERROR.
                       /* Если отстаток на закрытый ОД равен нулю и
                        ** движений не было, то гасим поле. */
      IF       vAcctPos       EQ 0
         AND   vDateAcctPos   EQ ?
      THEN ASSIGN
         vAcctCur:VISIBLE  = NO
      .
                     /* Если акцептованный остаток равен нулю и
                     ** в интервале от последнего закрытого ОД до
                     ** текущей даты нет движения со статусом выше
                     ** или равного "крыж",
                     ** то гасим поля акцептованных остатков. */
      IF       sh-bal-close   EQ 0
         AND   NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, YES)
      THEN DO:
         ASSIGN
            sh-val-close:VISIBLE =  NO
         .
                     /* Если накцептованный остаток равен нулю и
                     ** в интервале от последнего закрытого ОД до
                     ** текущей даты нет движения со статусом ниже "крыж",
                     ** то гасим поля накцептованных остатков. */
         IF  NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, NO) THEN DO:

            IF sh-bal-na   EQ 0
            THEN ASSIGN
               sh-val-na:VISIBLE =  NO
            .
            IF vAvPos EQ 0
            THEN ASSIGN
               vAvCur:VISIBLE =  NO
            .

         END.
      END.
   END.
   ELSE DO:
      ASSIGN
           vAcctCur:VISIBLE     = NO
           sh-val-close:VISIBLE = NO
           sh-val-na:VISIBLE    = NO
           vAvCur:VISIBLE       = NO
           vAcctQty:VISIBLE     = YES
           sh-qty-close:VISIBLE = YES
           sh-qty-na:VISIBLE    = YES
           vAcctQty:SCREEN-VALUE = string(vAcctQty)
           sh-qty-close:SCREEN-VALUE = string(sh-qty-close)
           sh-qty-na:SCREEN-VALUE = string(sh-qty-na)
           mChoice:SCREEN-VALUE   = "Остаток ЦБ"
        NO-ERROR.

      IF       vAcctQty      EQ 0
         AND   mDateAcctQty   EQ ?
      THEN ASSIGN
         vAcctQty:VISIBLE  = NO
      .

      IF       sh-qty-close   EQ 0
         AND   NOT GetFirtsMoveDate (tt-acct.acct, mDateAcctQty, gEnd-date, YES)
      THEN DO:
         ASSIGN
            sh-qty-close:VISIBLE =  NO
         .

         IF       sh-qty-na   EQ 0
            AND   NOT GetFirtsMoveDate (tt-acct.acct, mDateAcctQty, gEnd-date, NO)
         THEN ASSIGN
            sh-qty-na:VISIBLE =  NO
         .
      END.

   END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   DEF VAR vTok      AS CHAR  NO-UNDO. /* Елемент маски. */
   DEF VAR vCnt      AS INT64       NO-UNDO. /* Счетчик. */
   DEF VAR vOFfset   AS INT64       NO-UNDO. /* Текущее положение курсора. */

   IF LAST-EVENT:FUNCTION EQ "?"
      THEN RETURN.

   vOFfset  = tt-acct.acct:CURSOR-OFFSET.
   DO vCnt = 1 TO tt-acct.acct:CURSOR-OFFSET:
      IF       SUBSTR (tt-acct.acct:FORMAT, vCnt, 1) NE "9"
         AND   SUBSTR (tt-acct.acct:FORMAT, vCnt, 1) NE "x"
      THEN vOFfset = vOFfset - 1.
   END.
   vTok = SUBSTR (mTokAcct, vOFfset, 1).
   IF LOOKUP (vTok, "с,z,п") EQ 0
      THEN RETURN NO-APPLY.
   tt-acct.contr-acct:SCREEN-VALUE ="".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON backspace OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON CLEAR OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON delete OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON F1 OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   RETURN NO-APPLY.
END.

&Scoped-define SELF-NAME tt-acct.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON F5 OF tt-acct.acct IN FRAME fMain /* счёт */
DO:
   RETURN NO-APPLY.
END.

&Scoped-define SELF-NAME tt-acct.contr-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.contr-acct TERMINAL-SIMULATION
ON F1 OF tt-acct.contr-acct IN FRAME fMain /* Парный счет */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      RUN acct-ctr.p (
         tt-acct.bal-acct,
         tt-acct.currency,
         iLevel + 1
      ).
      IF       LAST-EVENT:FUNCTION  NE "END-ERROR"
         AND   pick-value           NE ?
         THEN SELF:SCREEN-VALUE = ENTRY (1, pick-value).
   END.
   ELSE IF iMode EQ {&MOD_VIEW}
   THEN RUN formld.p (
      GetClassObject ("acct", tt-acct.contr-acct + "," + tt-acct.currency),
      tt-acct.contr-acct + "," + tt-acct.currency,
      "",
      {&MOD_VIEW},
      iLevel
   ).
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON LEAVE OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   {&BEG_BT_LEAVE}
   APPLY "RETURN" TO SELF.
   IF RETURN-VALUE EQ {&RET-ERROR}
      THEN RETURN NO-APPLY {&RET-ERROR}.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON RETURN OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   DEF VAR vOldNumber   AS CHAR   NO-UNDO. /* Номер счета до изменения. */
   DEF VAR vNumber      AS CHAR   NO-UNDO. /* Номер счета после изменения. */
   DEF VAR vOk          AS LOG    NO-UNDO. /* Результат работы с резервом счетов. */
   DEF VAR vTok         AS CHAR   NO-UNDO.
   DEF VAR vCnt         AS INT64  NO-UNDO. 
   DEF VAR i            AS INT64  NO-UNDO. 

   IF iMode NE {&MOD_ADD}
   THEN DO:
      APPLY "RETURN" TO FRAME {&MAIN-FRAME}.
      RETURN NO-APPLY.
   END.

   CHCK_BLCK:
   DO
   ON ERROR    UNDO CHCK_BLCK, LEAVE CHCK_BLCK
   ON ENDKEY   UNDO CHCK_BLCK, LEAVE CHCK_BLCK:
      ASSIGN
                        /* Сбрасываем флаг ошибки. */
         vOk         =  NO
                        /* Сохраняем старый номер счета. */
         vOldNumber  =  tt-acct.acct
      .
      /* 0185464 для QBIS проверяем на соответствие маске, т.к. ANY-PRINTABLE не срабатывает */
      &IF DEFINED(MANUAL-REMOTE) &THEN
         ASSIGN
            vNumber = tt-acct.acct:SCREEN-VALUE
            vCnt = 1
         .
         DO i = 1 TO LENGTH(vNumber):
            IF SUBSTRING(tt-acct.acct:FORMAT,i,1) NE "9" AND SUBSTRING(tt-acct.acct:FORMAT,i,1) NE "x"
               THEN NEXT.
            vTok = SUBSTR(mTokAcct,vCnt,1).
            IF LOOKUP (vTok, "с,z,п") EQ 0
               THEN OVERLAY(vNumber,i,1) = SUBSTRING(vOldNumber,vCnt,1). 
            vCnt = vCnt + 1.
            IF vCnt > LENGTH(mTokAcct) OR vCnt > LENGTH(vOldNumber) 
               THEN LEAVE.
         END.
         tt-acct.acct:SCREEN-VALUE = vNumber.
      &ENDIF

      ASSIGN tt-acct.acct.
                        /* Пересчет ключа. */
      RUN RecalcKey IN h_acct (
         tt-acct.class-code,
         tt-acct.bal-acct,
         tt-acct.acct,
         OUTPUT vNumber
      ) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN LEAVE CHCK_BLCK.
                        /* Если счет изменился, то... */
      IF AddFilToAcct(vNumber,shFilial) NE AddFilToAcct(vOldNumber, shFilial)
      THEN DO:
                        /* Проверка наличия счета в БД. */
         IF GetValueByQuery ("acct", "acct", "acct.acct  EQ '" + AddFilToAcct(vNumber,shFilial) + "' NO-LOCK") NE ?
         THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "acct03", "", "%s=" + vNumber).
            UNDO CHCK_BLCK, LEAVE CHCK_BLCK.
         END.
                        /* Проверяем наличие нового счета в классификаторе.
                        ** Если такой есть, то отменяем действие. */
         IF GetCode("СчетаРезерва", AddFilToAcct(vNumber,shFilial)) NE ?
         THEN DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "0",
               "Счет ~"" + vNumber + "~" зарезервирован в классификаторе ~"СчетаРезерва~"."
            ).
            UNDO CHCK_BLCK, LEAVE CHCK_BLCK.
         END.
         ELSE DO:
                        /* Может быть проблема одновременной блокировки,
                        ** но вероятность мала,
                        ** поэтому ошибку не обрабатываем. */
            RUN AcctKeep IN h_acct (AddFilToAcct(vNumber, shFilial),    OUTPUT vOk).
            RUN AcctFree IN h_acct (AddFilToAcct(vOldNumber, shFilial), OUTPUT vOk).
            vOldNumber = AddFilToAcct(vNumber, shFilial).
         END.
      END.
                        /* Проверки прошли успешно. */
      vOk   =  YES.
   END.
                        /* Проверка флага ошибки. */
   IF vOk
   THEN ASSIGN
      tt-acct.acct               = vNumber
      tt-acct.acct:SCREEN-VALUE  = tt-acct.acct
      tt-acct.number             = tt-acct.acct
   .
                        /* В случае ошибки возврат значений. */
   ELSE DO:
      ASSIGN
         tt-acct.number             = vOldNumber
         tt-acct.acct               = vOldNumber
         tt-acct.acct:SCREEN-VALUE  = STRING (REPLACE (vOldNumber," ","0"), tt-acct.acct:FORMAT)
      .
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-acct.open-date IN FRAME fMain 
DO:
   {&BEG_BT_LEAVE}
   IF YEAR(DATE(tt-acct.open-date:screen-value)) < 0  OR
      MONTH(DATE(tt-acct.open-date:screen-value)) < 0  OR
      DAY (DATE(tt-acct.open-date:screen-value)) < 0  
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1","Неверное значение реквизита 'Открыт'!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-acct.bal-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.bal-acct TERMINAL-SIMULATION
ON LEAVE OF tt-acct.bal-acct IN FRAME fMain /* Счет 2 порядка */
DO:
   DEF VAR mValid  AS INT64    NO-UNDO. /* Флаг ошибки. */
   DEF VAR mIntTmp AS INT64  NO-UNDO.
   
   /*ccc*/
   IF  iMode EQ {&MOD_ADD}
   AND LAST-EVENT:FUNCTION  NE "END-ERROR" 
   AND INT64(tt-acct.bal-acct:SCREEN-VALUE) EQ 91203
   AND (USERID("bisquit") EQ "O0400MGK" 
     OR USERID("bisquit") EQ "K0400MAA"
     OR USERID("bisquit") EQ "K0400SMV"
     OR USERID("bisquit") EQ "I0400STS"
     /*OR USERID("bisquit") EQ "RKO_MVY"*/)
   THEN
   DO:
      RUN Fill-SysMes ("", "", "-1","У Вас нет прав для открытия счетов ~nэтого счета второго порядка.").
      RETURN NO-APPLY.
   END.
   /*ccc*/
   
   {&BEG_BT_LEAVE}
   
   IF NOT imode EQ {&MOD_ADD}
      THEN RETURN.

   mIntTmp = INT64(tt-acct.bal-acct:SCREEN-VALUE) NO-ERROR.

   RUN Check-Acct-Bal-Acct IN h_acct (
      mIntTmp,
      tt-acct.acct-cat,
      tt-acct.cust-cat:SCREEN-VALUE,
      tt-acct.acct:SCREEN-VALUE,
      OUTPUT mValid
   ).
   IF mValid NE 0
      THEN RETURN NO-APPLY.
                        /* Установка свойств по связанным полям. */
   RUN ChkFldPropContr-Bal-Acct.
                        /* Установка свойств поля "парный счет". */
   RUN ChkFldPropContr-Acct.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.branch-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.branch-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.branch-id IN FRAME fMain /* Подразделение */
DO:
   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN DO:
                  /* МОжет ли пользователь создавать счета в данном подразделении. */
      RUN CheckBranch IN h_brnch (SELF:SCREEN-VALUE).
      IF RETURN-VALUE NE ""
         THEN RETURN NO-APPLY.

      ASSIGN
         vNameBranch = GetValueAttr ("branch", SELF:SCREEN-VALUE, "name")
         vNameBranch:SCREEN-VALUE = vNameBranch
      .
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.check-op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.check-op TERMINAL-SIMULATION
ON LEAVE OF tt-acct.check-op IN FRAME fMain /* Контроль */
DO:
   DEF VAR mValid AS INT64    NO-UNDO. /* Флаг ошибки. */

   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN DO:
      RUN Acct-Check-Side IN h_acct (
         tt-acct.check-op:SCREEN-VALUE,
         vSide:SCREEN-VALUE,
         OUTPUT mValid
      ).
      IF mValid NE 0
         THEN RETURN NO-APPLY.
      ASSIGN
         vNameControl = GetValueAttr ("code", "check-op," + SELF:SCREEN-VALUE, "name")
         vNameControl:SCREEN-VALUE = vNameControl
      .
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.close-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.close-date TERMINAL-SIMULATION
ON ENTRY OF tt-acct.close-date IN FRAME fMain /* Закрыт */
DO:
   IF iMode = {&MOD_EDIT} THEN.
 ELSE
 DO:
    {&BT_ENTRY}
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.close-date TERMINAL-SIMULATION
ON LEAVE OF tt-acct.close-date IN FRAME fMain /* Закрыт */
DO:
  IF iMode = {&MOD_EDIT} THEN.
  ELSE
  DO:
     {&BT_LEAVE}
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.contract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.contract TERMINAL-SIMULATION
ON F1 OF tt-acct.contract IN FRAME fMain /* Назначение */
DO:
   IF iMode EQ {&MOD_VIEW}
   THEN DO:
      RUN RunClassMethod (
         "contract",
         "look",
         "",
         "",
         ?,
         tt-acct.acct      + "," +
         tt-acct.currency  + "," +
         SELF:SCREEN-VALUE + "," +
         STRING (iLevel + 1)
      ).
      RETURN NO-APPLY.
   END.
   mCall_F1_IN_Frame = YES.
   APPLY "F1" TO FRAME fMain.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.currency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.currency TERMINAL-SIMULATION
ON LEAVE OF tt-acct.currency IN FRAME fMain /* Валюта */
DO:
   DEF VAR mForeign  AS CHAR   NO-UNDO. /* тип валюты счета 2-го порядка */

   /* получение реквизита foreign-curr (Валютный) счета 2-го порядка */
   mForeign = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "foreign-curr").
   {&BEG_BT_LEAVE}
   /* проверка валюты лицевого счета по счету 2-го порядка */
   IF     (mForeign                       EQ "NO"
      AND  tt-acct.currency:SCREEN-VALUE  NE "")
      OR  (mForeign                       EQ "YES"
      AND  tt-acct.currency:SCREEN-VALUE  EQ "")
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "acct37", "","").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.currency TERMINAL-SIMULATION
ON RETURN OF tt-acct.currency IN FRAME fMain /* Валюта */
DO:
   DEF VAR vValid       AS INT64    NO-UNDO. /* Флаг ошибки. */
   DEF VAR vProcName    AS CHAR   NO-UNDO. /* Имя процедуры метода. */
   DEF VAR vOk          AS LOG    NO-UNDO. /* Флаг сохранения номера счета в клас-ре. */
   DEF VAR vKodDoxRash  AS CHAR   NO-UNDO. /* Счет доходов и расходов. */
   DEF VAR vMakeAcct    AS LOG    NO-UNDO. /* Флаг успешного выполнения операции. */

   DEFINE BUFFER acct      FOR acct.
   DEFINE BUFFER xbal-acct FOR bal-acct.

   IF iMode NE {&MOD_ADD}
   THEN DO:
      APPLY "RETURN" TO FRAME {&MAIN-FRAME}.
      RETURN NO-APPLY.
   END.
                        /* Проверка правильности введенной валюты.
                        ** Проверка валюты счета по счету 2-го порядка. */
   APPLY "LEAVE" TO tt-acct.currency IN FRAME {&MAIN-FRAME}.
   IF RETURN-VALUE EQ {&RET-ERROR} THEN RETURN NO-APPLY.

   RUN Check-Acct-Currency IN h_acct (
      tt-acct.currency:SCREEN-VALUE,
      tt-acct.acct-cat,
      OUTPUT vValid
   ).
   IF vValid NE 0
      THEN RETURN NO-APPLY.

   vProcName = GET-CLASS-METHOD (tt-acct.class-code, "Create").
   IF vProcName NE ?
   THEN DO:
      IF SearchPfile (vProcName)
         THEN RUN VALUE (vProcName + ".p") (
            tt-acct.bal-acct:SCREEN-VALUE,
            tt-acct.currency:SCREEN-VALUE
         ).
         ELSE DO:
            RUN Fill-SysMes (
               "", "", "",
               "Не найдена процедура создания " + vProcName + ".p."
            ).
            RETURN NO-APPLY.
         END.
   END.
   ELSE
   TR:
   DO
   TRANSACTION
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
                        /* Если повторно создаем счет, то необходимо удалить
                        ** старый счет из классификатора "СчетаРезерва". */
      RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
      ASSIGN
         tt-acct.bal-acct
         tt-acct.currency
         tt-acct.cust-cat
         tt-acct.cust-id
         tt-acct.open-date
         tt-acct.details
         tt-acct.contract
         tt-acct.branch-id
      .
                        /* Получение маски счета. */
      RUN FindAcctMask IN h_acct (
         tt-acct.class-code,
         tt-acct.bal-acct,
         INPUT-OUTPUT mAcctMask,
         INPUT-OUTPUT vKodDoxRash
      ) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN DO:
         
         RUN Fill-SysMes IN h_tmess ("","","-1",RETURN-VALUE).
         RETURN NO-APPLY.
      END.
                        /* Если маска счета полностью состоит из символов "П",
                        ** то не запускаем процедуру генерации номера счета. */
      IF mAcctMask EQ FILL ({&TOK_ANY}, LENGTH (mAcctMask))
                        /* Заполняем пробелами для дальнейшего отображения. */
         THEN tt-acct.acct = FILL (" ", LENGTH (mAcctMask)).
      ELSE DO:
                        /* Отключаем создание истории по таблице "acct". */
         IF LOOKUP ("acct", FilesHist) GT 0
            THEN ENTRY (LOOKUP ("acct", FilesHist), FilesHist) =  "acct_closed".
                        /* Создаем счет. */

            /* Восстанавливаем ранее удаленный bis-temp-table */
            FOR EACH ttBTTUsed EXCLUSIVE-LOCK:
               IF NOT CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
                  CREATE bis-temp-table.
                  BUFFER-COPY ttBTTUsed TO bis-temp-table.
                  RELEASE bis-temp-table.
                  DELETE ttBTTUsed.
               END.
            END.
            
            /* Предугадываем какой bis-temp-table будет удален */
         FIND FIRST bis-temp-table WHERE bis-temp-table.surrogate-id BEGINS STRING(tt-acct.bal-acct) + "," + STRING(tt-acct.currency) + "," NO-LOCK NO-ERROR.
            IF AVAIL bis-temp-table THEN DO:
               IF NOT CAN-FIND(FIRST ttBTTUsed WHERE ttBTTUsed.surrogate-id = bis-temp-table.surrogate-id) THEN DO:
                  CREATE ttBTTUsed.
                  BUFFER-COPY bis-temp-table TO ttBTTUsed.
               END.
            END.

         IF     shFilial EQ "0500"
            AND SUBSTR(GetXAttrValueEx("_user",USERID("bisquit"),"office",""),1,4) = "0598" 
            AND CAN-DO("408*",STRING(tt-acct.bal-acct))  THEN 
            mAcctMask = replace(mAcctMask,"фффф","0598").

         RUN MakeAcct (
            tt-acct.class-code,  /* iClass                  */
            tt-acct.bal-acct,    /* iBal                    */
            tt-acct.currency,    /* iCurr                   */
            tt-acct.cust-cat,    /* iCustCat                */
            tt-acct.cust-id,     /* iCustID                 */
            tt-acct.open-date,   /* iOpenDate               */
            OUTPUT tt-acct.acct, /* oAcct                   */
            BUFFER acct,         /* BUFFER iacct FOR acct . */
            mAcctMask,           /* iAcctMask               */
            vKodDoxRash,         /* iKodDoxRash             */
            tt-acct.details,     /* iDetails                */
            ?,                   /* iKauId                  */
            tt-acct.contract,    /* iContract               */
            USERID ('bisquit'),  /* iUserId                 */
            tt-acct.branch-id,   /* iBranchId               */
            NO                   /* iCopyBalXattr           */
         ) NO-ERROR.

         IF ERROR-STATUS:ERROR THEN DO:
            DEFINE VAR vErrCnt AS INT64 NO-UNDO.
            vErrStr = "".
            IF {assigned RETURN-VALUE} THEN vErrStr = SUBSTR(RETURN-VALUE,(IF INDEX(RETURN-VALUE,"pp-acct.p<-") > 0 THEN INDEX(RETURN-VALUE,"pp-acct.p<-") + 11 ELSE 1)) + CHR(10).
            DO vErrCnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
                IF INDEX(vErrStr, ERROR-STATUS:GET-MESSAGE(vErrCnt)) = 0 THEN
                vErrStr = vErrStr + CHR(10) + ERROR-STATUS:GET-MESSAGE(vErrCnt).
                IF vErrCnt > 8 THEN LEAVE.
            END.
            IF NOT SESSION:REMOTE THEN
            RUN Fill-SysMes("","","-1",vErrStr).
            UNDO TR, LEAVE TR.
         END.
         
         IF LENGTH(vKodDoxRash) GT 0 AND
            gend-date   GE mDate446
         THEN DO:
            FIND LAST tmp-code WHERE 
                      tmp-code.class    EQ "ПрибУбытФ"   
               AND    tmp-code.code     EQ vKodDoxRash + "_"
               AND    tmp-code.beg-date LE gend-date
            NO-LOCK NO-ERROR.         
            IF AVAIL tmp-code THEN            
            ASSIGN
               tt-acct.pribubyt446$:SCREEN-VALUE = tmp-code.code
               tt-acct.pribubyt446$ = tmp-code.code
            .
         END.

            /* Удаляем догадки если не угадано */
            FOR EACH ttBTTUsed EXCLUSIVE-LOCK:
               IF CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
                  DELETE ttBTTUsed.
               END.
            END.

                                 /* Успешность обработки MakeAcct. */
         vMakeAcct   =  YES.
         IF tt-acct.acct EQ ?
            THEN UNDO TR, LEAVE TR.

         FOR FIRST acct WHERE
                  acct.acct      EQ tt-acct.acct
            AND   acct.currency  EQ tt-acct.currency
         EXCLUSIVE-LOCK:
            RUN DelSigns (
               "acct",
               tt-acct.acct + "," + tt-acct.currency
            ).
            DELETE acct.
                        /* Для счетов, у которых в маске счета нет символа "П".
                        ** Сохранение счет в классификаторе резерва. */
            IF INDEX (mAcctMask, {&TOK_ANY}) EQ 0
            THEN DO:
               RUN AcctKeep IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
               IF vOk NE YES
                  THEN UNDO TR, LEAVE TR.
            END.
         END.
      END.
   END.
                        /* Восстанавливаем ведение истории по таблице "acct". */
   IF LOOKUP ("acct_closed", FilesHist) GT 0
      THEN ENTRY (LOOKUP ("acct_closed", FilesHist), FilesHist)   =  "acct".
                        /* В случае ошибки откат триггера. */
   IF NOT vMakeAcct THEN
      RETURN NO-APPLY.
                        /* Установление типа курса. */
   IF tt-acct.acct-cat NE "d"
   THEN DO:
      FIND FIRST xbal-acct WHERE xbal-acct.bal-acct = tt-acct.bal-acct NO-LOCK NO-ERROR.
      ASSIGN
         tt-acct.rate-type:VISIBLE        =  tt-acct.currency NE ""
         tt-acct.rate-type                =  IF tt-acct.currency EQ ""
                                             THEN ""
                                             ELSE IF  AVAIL xbal-acct
                                                  AND {assigned xbal-acct.rate-type}
                                                  THEN xbal-acct.rate-type
                                                  ELSE "Учетный"
         tt-acct.rate-type:SCREEN-VALUE   =  tt-acct.rate-type
      .
   END.
   ELSE tt-acct.rate-type:VISIBLE = NO.
                        /* Получение токенов по маске. */
   RUN GetAcctMask IN h_acct (
      mAcctMask,
      OUTPUT mTokAcct,
      OUTPUT mTokIdx,
      OUTPUT mTokLen
   ).
                        /* Заменяем пробелы на символ 0.
                        ** И форматируем значение согласно категории. */
   ASSIGN
      tt-acct.contr-acct:SCREEN-VALUE  = ""
      tt-acct.acct      :SCREEN-VALUE  =  STRING (
                                             REPLACE (tt-acct.acct," ","0"),
                                             tt-acct.acct:FORMAT
                                          )
   .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.cust-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-cat TERMINAL-SIMULATION
ON LEAVE OF tt-acct.cust-cat IN FRAME fMain /* Тип клиента */
DO:
   DEF VAR vValid  AS INT64 NO-UNDO. /* Флаг ошибки. */
   DEF VAR vError  AS LOG NO-UNDO. /* Флаг ошибки. */

   {&BEG_BT_LEAVE}
                        /* Проверка соответствия категории и типа клиента. */
   RUN Check-Acct-Cust-Cat IN h_acct (
      tt-acct.bal-acct:SCREEN-VALUE,
      tt-acct.cust-cat:SCREEN-VALUE,
      tt-acct.acct:SCREEN-VALUE,
      OUTPUT vValid
   ).
   IF vValid NE 0
      THEN RETURN NO-APPLY {&RET-ERROR}.
                        /* Проверка для кредитов и депозитов. */
   IF       iMode             EQ {&MOD_edit}
      AND   SELF:SCREEN-VALUE NE tt-acct.cust-cat
      AND   tt-acct.cust-cat  NE "В" 
   THEN DO:

      vError = TRUE.
      RUN LoanChk (tt-acct.contract,
                   tt-acct.acct,
                   tt-acct.currency,
                   SELF:SCREEN-VALUE,
                   INT64 (tt-acct.cust-id:SCREEN-VALUE),
                   INPUT-OUTPUT vError).

      IF vError THEN
         RETURN NO-APPLY {&RET-ERROR}.

   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-cat TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-acct.cust-cat IN FRAME fMain /* Тип клиента */
DO:
   DEFINE VAR vClassCode AS CHAR NO-UNDO.
   vClassCode = GetCustClass(SELF:SCREEN-VALUE).
   /* определяем показывать ли УНК для данного типа клиентов или cust-id */
   {getflagunk.i &class-code="vClassCode" &flag-unk="mFlagUnk"}

   ASSIGN
      tt-acct.cust-id:VISIBLE = NO
      unk$           :VISIBLE = NO.

   IF SELF:SCREEN-VALUE EQ "В"
   THEN ASSIGN
      tt-acct.cust-id               = ?
      tt-acct.cust-id:SCREEN-VALUE  = ?
      vNameClient    :SCREEN-VALUE  = ""
      unk$                          = ?
      unk$           :SCREEN-VALUE  = ?
   .
   ELSE
      IF mFlagUnk THEN
         ASSIGN tt-acct.cust-id:VISIBLE = NO
                unk$           :VISIBLE = YES.
      ELSE
         ASSIGN unk$           :VISIBLE = NO
                tt-acct.cust-id:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.cust-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.cust-id IN FRAME fMain /* Клиент N */
DO:
   DEF VAR vCustId AS INT64 NO-UNDO.
   DEF VAR vError  AS LOG NO-UNDO. /* Флаг ошибки. */

   {&BEG_BT_LEAVE}
                        /* Данная обработка сделана по причине наличия в БД
                        ** клиента с нулевым номером. */
   IF SELF:SCREEN-VALUE EQ "?"
   THEN DO:
      RUN Fill-SysMes (
         "", "", "0",
         "Реквизит ~"Клиент N~" обязательный."
      ).
      RETURN NO-APPLY {&RET-ERROR}.
   END.
                        /* Проверка для кредитов и депозитов. */
   vCustId = INT64 (SELF:SCREEN-VALUE).
   IF       iMode                         EQ {&MOD_edit}
      AND   vCustId                       NE tt-acct.cust-id
   THEN DO:
      vError = TRUE.
      RUN LoanChk (tt-acct.contract,
                   tt-acct.acct,
                   tt-acct.currency,
                   tt-acct.cust-cat:SCREEN-VALUE,
                   vCustId,
                   INPUT-OUTPUT vError).

      IF vError THEN
         RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.Details TERMINAL-SIMULATION
ON RETURN OF tt-acct.Details IN FRAME fMain
DO:
   DEF VAR vH AS HANDLE NO-UNDO. /* Указатель на предыдущее поле. */
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN APPLY "TAB" TO SELF.
   ELSE DO:
      vh = SELF:PREV-TAB-ITEM.
      APPLY "RETURN" TO vh.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mKau-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKau-id TERMINAL-SIMULATION
ON F1 OF mKau-id IN FRAME fMain /* Код шабл.КАУ */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN
   DO TRANSACTION:

      RUN browseld.p (
         "ШаблКау",
                        /* Поля для предустановки. */
         "class"   + CHR(1) + "parent"  + CHR(1) + "code",
         "ШаблКау" + CHR(1) + "ШаблКау" + CHR(1) + GetXattrValueEx("bal-acct",string(tt-acct.bal-acct),"lst-kau-id","*") ,
         "",
         4
      ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
         THEN SELF:SCREEN-VALUE = pick-value.
   END.
   ELSE APPLY "F1" TO FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKau-id TERMINAL-SIMULATION
ON LEAVE OF mKau-id IN FRAME fMain /* Код шабл.КАУ */
DO:
   DEFINE VARIABLE mKauLst AS CHARACTER   NO-UNDO.

   IF {assigned mKau-id:SCREEN-VALUE}
     AND (imode EQ {&MOD_EDIT}
       OR imode EQ {&MOD_ADD}) THEN DO:

     mKauLst = GetXattrValueEx("bal-acct",string(tt-acct.bal-acct),"lst-kau-id","*").

     IF NOT CAN-DO(mKauLst,mKau-id:SCREEN-VALUE) THEN DO:

         RUN Fill-SysMes ("", "", "",
                          "Допустимые шаблоны КАУ для счета 2-го порядка " + STRING(tt-acct.bal-acct) + " - " + mKauLst).
         RETURN NO-APPLY {&RET-ERROR}.

      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unk$ TERMINAL-SIMULATION
ON LEAVE OF unk$ IN FRAME fMain /* УНК */
DO:
   DEF VAR d1 AS CHAR   NO-UNDO.
   DEFINE VARIABLE vCustId    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustClass AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUnkValue  AS CHARACTER NO-UNDO.
   {&BEG_BT_LEAVE}
   IF iMode NE {&MOD_VIEW} THEN
   DO:
      IF    NOT {assigned unk$:SCREEN-VALUE}
         OR unk$:SCREEN-VALUE EQ "?"
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Реквизит ~"УНК~" обязательный!").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      vCustId = LEFT-TRIM(tt-acct.cust-id:SCREEN-VALUE,"0").
      IF NOT {assigned vCustID} OR
         (LEFT-TRIM(SELF:SCREEN-VALUE,"0") EQ "" AND
         vCustID EQ LEFT-TRIM(string(vCustId-ScV),"0"))  THEN
         vCustId = "?".
      ASSIGN
        vCustClass = GetCustClass(tt-acct.cust-cat:SCREEN-VALUE)
        vUnkValue  = LEFT-TRIM(SELF:SCREEN-VALUE,"0").
      /* поиск клиента по УНК и коду клиента */
      FIND FIRST signs WHERE signs.file-name    EQ vCustClass
                         AND signs.code         EQ "УНК"
                         AND (LEFT-TRIM(signs.code-value,"0")  EQ vUnkValue
                           OR LEFT-TRIM(signs.xattr-value,"0") EQ vUnkValue)
                         AND signs.surrogate    EQ vCustId
      NO-LOCK NO-ERROR.
      /* если клиент не найден - ищем только по УНК */
      IF NOT AVAILABLE signs THEN
      FIND FIRST signs WHERE signs.file-name    EQ vCustClass
                         AND signs.code         EQ "УНК"
                         AND (LEFT-TRIM(signs.code-value,"0")  EQ vUnkValue
                           OR LEFT-TRIM(signs.xattr-value,"0") EQ vUnkValue)
      NO-LOCK NO-ERROR.
      /* если клиент не найден - ошибка */
      IF NOT AVAILABLE signs THEN
      DO:
         ASSIGN
            tt-acct.cust-id:SCREEN-VALUE = "?"
            vNameClient    :SCREEN-VALUE = "".
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Клиент не найден.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      /* записываем значение cust-id */
      tt-acct.cust-id:SCREEN-VALUE = signs.surrogate.
      vCustId-ScV = tt-acct.cust-id:SCREEN-VALUE.
      /* определяем наименование клиента */
      vNameClient:SCREEN-VALUE = GetCliName(tt-acct.cust-cat:SCREEN-VALUE,
                                            tt-acct.cust-id:SCREEN-VALUE,
                                            OUTPUT d1,OUTPUT d1,OUTPUT d1,
                                            INPUT-OUTPUT d1,OUTPUT d1,OUTPUT d1).
      /* проверка клиента */
      APPLY "LEAVE" TO tt-acct.cust-id.
      IF RETURN-VALUE EQ {&RET-ERROR} THEN RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.user-id TERMINAL-SIMULATION
ON F1 OF tt-acct.user-id IN FRAME fMain /* Ответственный */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN
   DO TRANSACTION:
      RUN browseld.p (
         "_user",
                        /* Поля для предустановки. */
         "_userid"      + CHR(1) +
         "SetFirstFrm"  + CHR(1) +
         "oth3"         + CHR(1) +
         "sv-3",
                        /* Список значений полей. */
         (IF getThisUserXAttrValue("ПросмотрСотр") EQ "Да"
            THEN (USERID ("bisquit") + "," + GetSlaves ())
            ELSE "*")                              + CHR(1) +
         "4"                                       + CHR(1) +
         "NO"                                      + CHR(1) +
         "NO",
         "",
         4
      ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
         THEN SELF:SCREEN-VALUE = pick-value.
   END.
   ELSE APPLY "F1" TO FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.user-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.user-id IN FRAME fMain /* Ответственный */
DO:
   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN ASSIGN
      vNameUser = GetValueAttr ("_user", SELF:SCREEN-VALUE, "_user-name")
      vNameUser:SCREEN-VALUE  = vNameUser
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
   DEF VAR vOk AS LOG    NO-UNDO. /* Флаг удаления номера счета в клас-ре. */
   mRetVal = IF mOnlyForm
               THEN {&RET-ERROR}
               ELSE  "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
                        /* Удаляем счет из классификатора "СчетаРезерва". */
   IF iMode EQ {&MOD_ADD}
      THEN RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
   RETURN NO-APPLY.
END.

&IF DEFINED(MANUAL-REMOTE) &THEN
ON LEAVE OF tt-acct.currency IN FRAME fMain /* Валюта */
DO:
   IF NOT mRunGoOfFrame AND iMode <> {&MOD_VIEW} THEN APPLY "RETURN" TO SELF.
END.

ON LEAVE OF tt-acct.acct IN FRAME fMain /* Счет */
DO:
   IF iMode <> {&MOD_VIEW} THEN APPLY "RETURN" TO SELF.
END.
&ELSE
ON GO OF tt-acct.cust-cat IN FRAME fMain /* Тип клиента */
, tt-acct.bal-acct, tt-acct.currency, tt-acct.cust-id
DO:
   RETURN NO-APPLY.
END.
&ENDIF

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

SUBSCRIBE "ACCT-FORM-SET-FOCUS-TO-ACCT" ANYWHERE RUN-PROCEDURE "SubscribeToAcctFocus".

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").
   
   /* Получение значения НП КонтрольРедактСч */
   vEditSet = fGetSetting("КонтрольРедактСч", ?, "НЕТ").     

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
   RUN enable_UI.

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

   /* Commented by KSV: Открываем те поля, которые разрешено изменять
   ** в зависимости от режима открытия */
   RUN EnableDisable.
   IF vNameBlock THEN DO:
      /* Установка цвета для поля acct-status. */
      mFldH          =  GetWidgetHandle (FRAME fMain:HANDLE, "vNameBlock").
      mFldH:DCOLOR   =  GetDCOLOR ("bright-red").
   END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.

END.

/* Восстанавливаем ранее удаленный bis-temp-table */
   IF AVAIL tt-acct THEN DO:
      {find-act.i
         &acct   = tt-acct.acct
         &curr   = tt-acct.currency
         &lockac = NO-LOCK
      }
   IF AVAIL acct THEN DO:
      FOR EACH ttBTTUsed WHERE ttBTTUsed.surrogate-id BEGINS STRING(acct.bal-acct) + "," + STRING(acct.currency) + "," EXCLUSIVE-LOCK:
         DELETE ttBTTUsed.
   END.
   END.
END.
FOR EACH ttBTTUsed EXCLUSIVE-LOCK TRANSACTION:
      IF NOT CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
         CREATE bis-temp-table.
         BUFFER-COPY ttBTTUsed TO bis-temp-table.
         RELEASE bis-temp-table.
         DELETE ttBTTUsed.
      END.
   END.

UNSUBSCRIBE "ACCT-FORM-SET-FOCUS-TO-ACCT".

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

RUN disable_ui.

RUN EndBisTTY.

/* Commented by KSV: Выгружаем библиотеки */
{intrface.del}

/* Commented by KSV: Возвращаем значение вызывающей процедуре */
RETURN mRetVal.


PROCEDURE BeforePrepareInstance:
   DEFINE INPUT-OUTPUT PARAMETER pAggrInstances AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE vExcAggCl  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vParents   AS CHARACTER NO-UNDO.
   vParents = iClass + "," + GetXclassAllParents(iClass).
   
   vExcAggCl  = FGetSetting("ExcAggCl",?,"").
   IF CAN-DO (vExcAggCl, vParents) THEN
   DO:
      ENTRY(LOOKUP("deputy",pAggrInstances),pAggrInstances) = "".
      pAggrInstances = REPLACE(pAggrInstances,",,",",").
      pAggrInstances = TRIM(pAggrInstances,",").      
   END.
   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChkFldPropContr-Acct TERMINAL-SIMULATION
PROCEDURE ChkFldPropContr-Acct :
/* Устанавливает свойства поля в зависимости от состояния объекта. */
   DO WITH FRAME fMain:
                        /* Может ли у счета быть парный счет? */
  IF GetValueByQuery (
      "code",
      "code",
      "     code.class  EQ 'Dual-bal-acct' " +
      "AND (code.code   EQ '" + tt-acct.bal-acct:SCREEN-VALUE + "'  " +
      "  OR code.val    EQ '" + tt-acct.bal-acct:SCREEN-VALUE + "') " +
      "NO-LOCK"
   ) NE ?
   THEN tt-acct.contr-acct:SENSITIVE   = YES.
   ELSE ASSIGN
      tt-acct.contr-acct:SCREEN-VALUE  = ""  WHEN  iMode EQ {&MOD_ADD}
                                                OR iMode EQ {&MOD_EDIT}
      tt-acct.contr-acct:SENSITIVE     = NO
   .
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChkFldPropContr-Bal-Acct TERMINAL-SIMULATION
PROCEDURE ChkFldPropContr-Bal-Acct :
DEF VAR mSide     AS CHAR   NO-UNDO. /* Характеристика счета. */
   DEF VAR vCustCat  AS CHAR   NO-UNDO. /* Тип клиента. */

   DO WITH FRAME fMain:
      mSide = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "side").
                        /* Если на счете 2-го порядка четко задана характеристика счета,
                        ** то в режиме создания наследуем эту характеристику для Л/С.
                        ** Для режима редактирования проверяем соответсвие
                        ** характеристик Л/С и счета 2-го порядка.
                        ** Если не совпадает, то открываем поле на редактирование. */
      IF       (LENGTH (mSide) LE 1
         AND  (vSide   EQ mSide
            OR iMode   EQ {&MOD_ADD}))
            OR mSide   EQ "АП"
      THEN ASSIGN
         vSide               = IF mSide NE "АП" THEN mSide ELSE "-"
         vSide:SCREEN-VALUE  = vSide
         vSide:SENSITIVE     = NO
      .

                        /* Иначе поле следует изменить. */
      ELSE vSide:SENSITIVE   = YES.
                        /* Действия для режима создания. */
      IF iMode EQ {&MOD_ADD}
      THEN DO:
                        /* В какой валюте может быть открыт счет. */
         IF GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "foreign-curr")   EQ "NO"
            THEN tt-acct.currency:SCREEN-VALUE = "".
                        /* Инициализация поля "Назначение". */
         IF tt-acct.contract:SCREEN-VALUE EQ ""
            THEN tt-acct.contract:SCREEN-VALUE  = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "contract").
                        /* Инициализация поля KAU-ID. */
         IF mKau-id:SCREEN-VALUE   EQ ""
         THEN ASSIGN
            mKau-id           :SCREEN-VALUE  = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "kau-id")
            vNameKau          :SCREEN-VALUE  = IF {assigned mKau-id:SCREEN-VALUE}
                                                   THEN IF mKau-id:SCREEN-VALUE EQ "__Multi"
                                                      THEN "Множественная субаналитика"
                                                      ELSE GetCodeName  ("ШаблКау",  mKau-id:SCREEN-VALUE)
                                                   ELSE ""
         .
                        /* Инициализация поля Cust-Cat */
         vCustCat = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "cust-cat").
         IF     INDEX ("ЮЧБВ", vCustCat)   NE 0
                /* не открывать поле cust-cat, если его значение передано в форму извне */
            AND NOT (NUM-ENTRIES (iInstanceList, CHR (3)) GE 3
                     AND   ENTRY (3, iInstanceList, CHR (3)) NE "?")
         THEN DO:
            ASSIGN
               tt-acct.cust-cat  :SCREEN-VALUE  = vCustCat
               tt-acct.cust-cat  :SENSITIVE     = tt-acct.bal-acct:SENSITIVE
            .
            APPLY "VALUE-CHANGED" TO tt-acct.cust-cat.
         END.
      END.
   END.
   RETURN.
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
  DISPLAY vNameBranch vNameClient unk$ vSide vNameUser vNameBlock vNameControl
          mKau-id vNameKau separator1 separator2 separator3 separator4
          vDateAcctPos mNrAcctCur vAcctCur mNrAcctPos vAcctPos vDateDocAk
          vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose
          mNrShAvCur vAvCur mNrShAvPos vAvPos vDateDocNAk mNrShValNa sh-val-na
          mNrShBalNa sh-bal-na vDateDocAv mChoice sh-qty-close sh-qty-na
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-acct THEN
    DISPLAY tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id tt-acct.bal-acct
          tt-acct.currency tt-acct.acct tt-acct.rate-type tt-acct.open-date
          tt-acct.close-date tt-acct.last-date tt-acct.Details
          tt-acct.alw#tnomer$ tt-acct.contract tt-acct.pribubyt446$ 
          tt-acct.contr-acct tt-acct.user-id tt-acct.check-op
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id unk$
         tt-acct.rate-type mNrAcctCur vAcctCur mNrAcctPos vAcctPos vAcctQty
         sh-val-close mNrShValClose sh-bal-close mNrShBalClose mNrShAvCur
         vAvCur mNrShAvPos vAvPos mNrShValNa sh-val-na mNrShBalNa sh-bal-na
         mChoice sh-qty-close sh-qty-na
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION
PROCEDURE LocalEnableDisable :
DEF VAR vCustCat  AS CHAR   NO-UNDO. /* Тип клиента со счета 2-го порядка. */
   DEF VAR vXattrFmt AS CHAR   NO-UNDO. /* Формат реквизита */ 
DEF VAR vFSens AS LOG NO-UNDO.     

                        /* Установка свойств поля "парный счет". */
   RUN ChkFldPropContr-Acct.
                        /* Блок объявлен для указания формы. */
   DO WITH FRAME fMain:
      IF vEditSet EQ "ДА" THEN
      DO:  
         FIND FIRST op-date WHERE op-date.op-date EQ tt-acct.open-date NO-LOCK NO-ERROR.
         IF NOT AVAIL(op-date) THEN DO:           
            FIND FIRST op-date WHERE op-date.op-date GE tt-acct.open-date NO-LOCK NO-ERROR.
            IF AVAIL(op-date) THEN
               IF Chk_Date_Cat(op-date.op-date, "b") THEN
                  ASSIGN
                     tt-acct.open-date  :SENSITIVE  = NO.
         END.
         IF Chk_Date_Cat(op-date.op-date, "b") THEN
            ASSIGN
               tt-acct.open-date  :SENSITIVE  = NO.
         IF tt-acct.close-date NE ?  THEN
         DO:
            FIND FIRST op-date WHERE op-date.op-date EQ tt-acct.close-date NO-LOCK NO-ERROR.
            IF NOT AVAIL(op-date) THEN DO:           
               FIND FIRST op-date WHERE op-date.op-date GE tt-acct.close-date NO-LOCK NO-ERROR.
               IF AVAIL(op-date) THEN
                  IF  Chk_Date_Cat(op-date.op-date, "b") THEN
                     ASSIGN
                        tt-acct.close-date :SENSITIVE  = NO
                        vFSens = YES
                     .
            END.
            IF  Chk_Date_Cat(op-date.op-date, "b") THEN
               ASSIGN
                  tt-acct.close-date :SENSITIVE  = NO
                  vFSens = YES
               . 
         END.
      END.       
      IF    iMode EQ {&Mod_Edit}
         OR iMode EQ {&Mod_ADD}
      THEN DO:
         IF       NUM-ENTRIES (iInstanceList, CHR (3))   GE 2
            AND   ENTRY (2, iInstanceList, CHR (3))      NE "?"
            AND   ENTRY (2, iInstanceList, CHR (3))      NE "0"
            THEN tt-acct.bal-acct:SENSITIVE = NO.
                                 /* Установка свойств по связанным полям. */
         RUN ChkFldPropContr-Bal-Acct.

         IF       NUM-ENTRIES (iInstanceList, CHR (3))   GE 3
            AND   ENTRY (3, iInstanceList, CHR (3))      NE "?"
         THEN ASSIGN
            tt-acct.cust-cat  :SENSITIVE  = NO
            tt-acct.cust-id   :SENSITIVE  = NO
            unk$              :SENSITIVE  = NO
            vNameClient       :SENSITIVE  = NO
         .
         vXattrFmt = GetXAttrEx("_user","ИзмКодОтдел","Data-Format").
         /* Запрещать или разрешать пользователю изменять код подразделения на счете */
         tt-acct.branch-id:SENSITIVE = LOGICAL(GetXAttrValueEx("_user",USERID("bisquit"),"ИзмКодОтдел",STRING(NO,vXattrFmt)),vXattrFmt).
      END.
      IF iMode EQ {&Mod_Edit}
      THEN DO:
         vCustCat = GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "cust-cat").
         ASSIGN
                        /* Поле тип клиента открываем, если:
                        ** - на счете 2-го порядка не указан тип клиента,
                        ** - тип клиента счета не совпадает с типом,
                        ** указанным на счете 2-го порядка. */
            tt-acct.cust-cat  :SENSITIVE  =     vCustCat EQ ""
                                             OR vCustCat NE tt-acct.cust-cat
            tt-acct.rate-type :SENSITIVE  = tt-acct.currency   NE ""
            mKau-id:SENSITIVE             = NOT CAN-FIND(FIRST kau WHERE kau.acct EQ tt-acct.acct AND kau.currency EQ tt-acct.currency)
         .
         IF NOT vFSens THEN 
            tt-acct.close-date:SENSITIVE  = tt-acct.close-date NE ?.
      END.

   /* Отображение поля KAU-ID */
      mKau-id:SCREEN-VALUE = mKauId.
                        /* В режиме создания гасим поля сумм безусловно. */
      IF iMode EQ {&MOD_ADD}
      THEN DO:
         ASSIGN
            vAcctCur     :VISIBLE   = NO
            vAcctPos     :VISIBLE   = NO
            sh-val-close :VISIBLE   = NO
            sh-bal-close :VISIBLE   = NO
            sh-val-na    :VISIBLE   = NO
            sh-bal-na    :VISIBLE   = NO
            vAvPos       :VISIBLE   = NO
            vAvCur       :VISIBLE   = NO
         .
         APPLY "VALUE-CHANGED" TO tt-acct.cust-cat.
      END.
      ELSE DO:

         ASSIGN
                        /* Валютные остатки только для валютных счетов. */
            vAcctCur     :VISIBLE   =  tt-acct.currency  NE ""
            sh-val-close :VISIBLE   =  tt-acct.currency  NE ""
            sh-val-na    :VISIBLE   =  tt-acct.currency  NE ""
            vAvCur       :VISIBLE   =  tt-acct.currency  NE "" AND {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                               AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
            vAvPos       :VISIBLE   =  {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                              AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
            tt-acct.cust-id :VISIBLE =  tt-acct.cust-cat  NE "В"
         .
                        /* Если отстаток на закрытый ОД равен нулю и
                        ** движений не было, то гасим поле. */
         IF       vAcctPos       EQ 0
            AND   vDateAcctPos   EQ ?
         THEN ASSIGN
            vAcctPos:VISIBLE  = NO
            vAcctCur:VISIBLE  = NO
         .
                        /* Если акцептованный остаток равен нулю и
                        ** в интервале от последнего закрытого ОД до
                        ** текущей даты нет движения со статусом выше
                        ** или равного "крыж",
                        ** то гасим поля акцептованных остатков. */
         IF       sh-bal-close   EQ 0
            AND   NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, YES)
         THEN DO:
            ASSIGN
               sh-val-close:VISIBLE =  NO
               sh-bal-close:VISIBLE =  NO
            .
                        /* Если накцептованный остаток равен нулю и
                        ** в интервале от последнего закрытого ОД до
                        ** текущей даты нет движения со статусом ниже "крыж",
                        ** то гасим поля накцептованных остатков. */
            IF NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, NO) THEN DO:
               IF       sh-bal-na   EQ 0
               THEN ASSIGN
                  sh-val-na:VISIBLE =  NO
                  sh-bal-na:VISIBLE =  NO
               .

               IF vAvPos:VISIBLE AND vAvPos EQ 0
               THEN ASSIGN
                  vAvPos:VISIBLE =  NO
                  vAvCur:VISIBLE =  NO
               .
            END.
         END.
      END.
      ASSIGN
         vAcctQty     :VISIBLE   = NO
         sh-qty-close :VISIBLE   = NO
         sh-qty-na    :VISIBLE   = NO.

      /* скрываем УНК или cust-id в зависимости от наличия реквизита УНК на классе */
      IF mFlagUnk
         THEN tt-acct.cust-id:HIDDEN IN FRAME fMain = YES.
         ELSE unk$           :HIDDEN IN FRAME fMain = YES.

      /* Поле для ПрибУбыт446 доступно только для счетов класса acctb-pl */
      ASSIGN
         tt-acct.pribubyt446$:HIDDEN IN FRAME fMain = NOT tt-acct.class-code matches "acctb-pl*"
         tt-acct.pribubyt446$:SENSITIVE = NO
      .
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_CTRL-G TERMINAL-SIMULATION
PROCEDURE Local_CTRL-G :
IF iMode EQ {&MOD_VIEW}
   THEN RUN printfrm.p (FRAME fMain:HANDLE).
   RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_F9 TERMINAL-SIMULATION
PROCEDURE Local_F9 :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE vBlock AS CHARACTER NO-UNDO.

vBlock = "".

IF iMode EQ {&MOD_VIEW} THEN
DO:

    IF tt-acct.cust-cat EQ "Ю" THEN
        vBlock = GetXAttrValueEx("cust-corp", STRING(tt-acct.cust-id),"Блок", "").

    IF tt-acct.cust-cat EQ "Б" THEN
        vBlock = GetXAttrValueEx("banks", STRING(tt-acct.cust-id),"Блок", "").

    IF tt-acct.cust-cat EQ "Ч" THEN
        vBlock = GetXAttrValueEx("person", STRING(tt-acct.cust-id),"Блок", "").

    IF vBlock EQ "Да" OR vBlock EQ "Yes" THEN
    DO:
        RUN Fill-SysMes IN h_tmess ("", "", 1, "Клиент заблокирован! Изменение реквизитов невозможно!").
        RETURN ERROR.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_GO TERMINAL-SIMULATION
PROCEDURE Local_GO :
DEFINE VAR vCustNameLine AS CHARACTER NO-UNDO.
   IF iMode eq {&MOD_ADD} THEN 
   DO:
      IF tt-acct.cust-cat EQ "Ч" AND 
         AVAIL tt-deputy
      THEN 
      ASSIGN
         tt-deputy.acct       = tt-acct.acct
         tt-deputy.currency   = tt-acct.currency
         tt-deputy.person-id  = tt-acct.cust-id
         tt-deputy.right-priv = NO
      .
                        /* Заполняем номер счета.
                        ** Данная мера необходима для снятия блокировки.
                        ** Причина блокировки - заполнение альтернативного ключа
                        ** одинаковыми значениями до срабатывания тригера.*/
      ASSIGN
         tt-acct.number = TRIM (ENTRY (1, tt-acct.acct, "@"))
         tt-acct.side   = IF vSide EQ "-" THEN  "АП" ELSE vSide
         tt-acct.kau-id = mKau-id:SCREEN-VALUE IN FRAME fMain
      .

      IF NOT CAN-DO(FGetSetting("КлБлокОткрСч","",""),tt-acct.acct) THEN DO:
         RUN chk-blk.p (tt-acct.cust-cat,tt-acct.cust-id).
         IF RETURN-VALUE EQ "0" THEN DO:
            {getcustline.i &cust-cat = "tt-acct.cust-cat" &cust-id = "tt-acct.cust-id" &output-to = "vCustNameLine"}
            RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(tt-acct.number,GetAcctFmt(tt-acct.acct-cat))).
            RETURN ERROR {&RET-ERROR}.
         END.
      END.
   END.
   IF iMode EQ {&MOD_ADD} THEN DO:
      IF mKau-id:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST code WHERE code.class EQ "ШаблКау" AND code.code EQ (mKau-id:SCREEN-VALUE IN FRAME fMain))
      THEN DO:
          RUN Fill-SysMes IN h_tmess ("","","-1","Не найден код КАУ " + mKau-id:SCREEN-VALUE IN FRAME fMain).
          APPLY "ENTRY" TO mKau-id IN FRAME fmain.
          RETURN ERROR {&RET-ERROR}.
      END.
   END.   
   IF iMode eq {&MOD_EDIT} THEN DO:

      IF mKau-id:SCREEN-VALUE IN FRAME fMain NE mKauID THEN
         tt-acct.kau-id = mKau-id:SCREEN-VALUE IN FRAME fMain.

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_PostJoin TERMINAL-SIMULATION
PROCEDURE Local_PostJoin :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vFldH  AS HANDLE NO-UNDO.
    /* Расшифровка кода блокировки счета. */
    vNameBlock  =  BlockAcct (tt-acct.acct + ',' + tt-acct.currency,
                              DATETIME(gend-date,(IF gend-date = TODAY THEN MTIME ELSE 86399000))
                              ) GT "".
    vFldH          =  GetWidgetHandle (FRAME fmain:HANDLE, "vNameBlock").
    IF vNameBlock THEN
    vFldH:DCOLOR   =  GetDCOLOR ("bright-red").
    DISP vNameBlock
        @ vNameBlock
        WITH FRAME fmain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION
PROCEDURE PostGetObject :
DEF VAR vFrmh        AS HANDLE NO-UNDO.          /* Указатель на фрейм. */
   DEF VAR vFldH        AS HANDLE NO-UNDO.          /* Указатель на поле формы. */
   DEF VAR vName        AS CHAR   NO-UNDO EXTENT 2. /* Наименование счета. */
   DEF VAR vCnt         AS INT64    NO-UNDO.          /* Счетчик. */
   DEF VAR vNrLst       AS CHAR   NO-UNDO           /* Список полей для установки. */
                        INIT "mNrAcctCur,mNrShValClose,mNrShValNa,mNrShAvCur,mNrAcctPos,mNrShBalClose,mNrShBalNa,mNrShAvPos".
   DEF VAR vPosLst      AS CHAR   NO-UNDO           /* Список полей хранящих остатки. */
                        INIT "".
   DEF VAR vSResult     AS CHAR   NO-UNDO.          /* Результат поиска. */
   DEF VAR vDateAcctCur AS DEC    NO-UNDO.          /* Дата валютного остатка. */
   DEF VAR vClassCode   AS CHAR   NO-UNDO.

         /* Проверка наличия права просмотра информации о клиенте у текущего пользователя */
   IF    (iMode EQ {&MOD_EDIT}
      OR  iMode EQ {&MOD_VIEW})
      AND tt-acct.cust-cat EQ "Ч"
      AND NOT GetPersonPermission(tt-acct.cust-id)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(tt-acct.cust-id)).
      RETURN ERROR.
   END.

                        /* Предустановка значений (входных параметров). */
   IF iMode EQ {&MOD_ADD}
   THEN DO:
      ASSIGN
         tt-acct.bal-acct  =  INT64 (ENTRY (2, iInstanceList, CHR (3)))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "?"
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "0"
                        /* Если указан клиент. */
         tt-acct.cust-cat  =  ENTRY (3, iInstanceList, CHR (3))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 3
                                    AND   ENTRY (3, iInstanceList, CHR (3)) NE "?"
         tt-acct.cust-id   =  INT64 (ENTRY (4, iInstanceList, CHR (3)))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 4
                                    AND   ENTRY (4, iInstanceList, CHR (3)) NE "?"
         tt-acct.branch-id =  ENTRY (5, iInstanceList, CHR (3))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 5
                                    AND   ENTRY (5, iInstanceList, CHR (3)) NE "?"
      .
      ASSIGN
         tt-acct.contract  = GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "contract")
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "?".
   END.

   ASSIGN
      vFrmh          =  FRAME fMain:HANDLE
                        /* Формирование названия подразделения. */
      vNameBranch    =  GetValueAttr ("branch", tt-acct.branch-id, "name")
                        /* Формирование поля "ответственный". */
      vNameUser      =  GetValueAttr ("_user", tt-acct.user-id, "_user-name")
                        /* Расшифровка кода блокировки счета. */
      vNameBlock     =  BlockAcct (tt-acct.acct + ',' + tt-acct.currency,
                                   DATETIME(gend-date,(IF gend-date = TODAY THEN MTIME ELSE 86399000))
                                  ) GT ""
                        /* Расшифровка кода блокировки счета. */
      vNameControl   =  GetCodeName ("check-op", tt-acct.check-op)
      vDateDocAk     =  gEnd-Date
      vDateDocNAk    =  gEnd-Date
      vDateDocAv     =  gEnd-Date
                        /* Устанавливаем формат отображения счета. */
      tt-acct.acct:FORMAT  =  GetAcctFmt (tt-acct.acct-cat)
      /* acct - ? ставим в том случае, когда режим - создание и НЕ mOnlyForm, т.е.
                                простой показ формы без сохранения объекта */
      tt-acct.acct         =  ?
                              WHEN iMode EQ {&Mod_ADD} AND NOT mOnlyForm
   .
                        /* Получение кода шаблона КАУ. */
   ASSIGN
      mKauId   =  IF {assigned tt-acct.kau-id}
                     THEN tt-acct.kau-id
                     ELSE IF tt-acct.bal-acct GT 0
                        THEN GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "kau-id")
                        ELSE ""
      vNameKau =  IF mKauId NE ""
                     THEN IF mKauId EQ "__Multi"
                        THEN "Множественная субаналитика"
                        ELSE GetCodeName ("ШаблКау", mKauId)
                     ELSE ""
      vSide    = IF tt-acct.side EQ "АП" THEN "-" ELSE tt-acct.side
   .

   /* Определяем что отображать. (УНК или cust-id) */
   vClassCode = GetCustClass(tt-acct.cust-cat).
   {getflagunk.i &class-code="vClassCode" &flag-unk="mFlagUnk"}

   /* если на классе есть ДР "УНК", т.е. нужно отображать поле "УНК" */
   IF mFlagUnk
      /* и определены тип и идентификатор клиента */
      AND {assigned tt-acct.cust-cat}
      AND {assigned STRING(tt-acct.cust-id)}
        /* читаем допреквизит УНК у клиента для отображения его в поле формы */
   THEN unk$              = GetXattrValueEx(GetCustClass(tt-acct.cust-cat), LEFT-TRIM(STRING(tt-acct.cust-id),"0"), "УНК", ?).
   ELSE unk$:SCREEN-VALUE = ?.

                        /* Данную информацию можно получить только в случае
                        ** создания или редактирования счета. */
   IF NOT (    iMode EQ {&Mod_View}
            OR iMode EQ {&Mod_Edit})
      THEN RETURN.
                        /* Получение наименования клиента. */
   RUN GetCustName IN h_base (
      tt-acct.cust-cat,
      tt-acct.cust-id,
      tt-acct.acct,
      OUTPUT vName [1],
      OUTPUT vName [2],
      INPUT-OUTPUT vInn
   ).
   vNameClient = TRIM (vName [1] + " " + vName [2]).
   vCustId-ScV = STRING(tt-acct.cust-id).
                        /* Определение прав доступа к просмотру остатка. */
   IF AcctLookBuffer ((BUFFER tt-acct:HANDLE))
   THEN DO:
                        /* Поиск остатка в национальной валюте. */
      RUN GetAcctPos IN h_acct (
         (BUFFER tt-acct:HANDLE),
         gEnd-Date,
         OUTPUT vAcctPos,
         OUTPUT vDateAcctPos
      ).
      IF vAcctPos EQ ?
         THEN vAcctPos = 0.
                        /* Поиск остатка в инвалюте валюте. */
      IF tt-acct.acct NE ""
      THEN DO:
         RUN GetAcctCur IN h_acct (
            (BUFFER tt-acct:HANDLE),
            gEnd-Date,
            OUTPUT vAcctCur,
            OUTPUT vDateAcctCur
         ).
         IF vAcctCur EQ ?
            THEN vAcctCur = 0.
      END.
                         /*Поиск доступного остатка */
      mAccessMask      = FGetSetting("СтандТр", "AccessAcct", "").
      mAccessContAcct  = FGetSetting("СтандТр", "AccessContAcct", "").

      IF {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
         AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
      THEN
         RUN CalcAvailPos(tt-Acct.acct, tt-Acct.currency, gend-date, gend-date,"П","П","cli-pos", YES, "*", YES, OUTPUT vAvPos, OUTPUT vAvCur).
      IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code")
         THEN DO:

         RUN GetAcctQty IN h_acct (
            (BUFFER tt-acct:HANDLE),
            gEnd-Date,
            OUTPUT vAcctQty,
            OUTPUT mDateAcctQty
         ).
         IF vAcctQty EQ ?
            THEN vAcctQty = 0.

         RUN acct-qty IN h_base (
            tt-acct.acct,
            tt-acct.currency,
            gend-date,
            gend-date, "√"
         ).
         ASSIGN
            sh-qty-close = sh-qty
         .
         RUN acct-qty IN h_base (
            tt-acct.acct,
            tt-acct.currency,
            gend-date,
            gend-date,
            "П"
         ).
         ASSIGN
            sh-qty-na = sh-qty.
      END.
                        /* Определение остатка на счете. */
      RUN acct-pos IN h_base (
         tt-acct.acct,
         tt-acct.currency,
         gend-date,
         gend-date, "√"
      ).
      ASSIGN
         sh-val-close = sh-val
         sh-bal-close = sh-bal
      .
      RUN acct-pos IN h_base (
         tt-acct.acct,
         tt-acct.currency,
         gend-date,
         gend-date,
         "П"
      ).
   END.
   ELSE DO:
      ASSIGN
                        /* Обнуление остатков. */
         sh-bal         =  0
         sh-val         =  0
      .
      DO vCnt = 1 TO NUM-ENTRIES (vNrLst):
         ASSIGN
            vFldH                =  GetWidgetHandle (vFrmh, ENTRY (vCnt, vNrLst))
            vFldH:DCOLOR         =  GetDCOLOR ("bright-yellow")
         .
      END.
      ASSIGN
         mNrAcctCur     = "            НЕТ ДОПУСКА"
         mNrShValClose  = "            НЕТ ДОПУСКА"
         mNrShValNa     = "            НЕТ ДОПУСКА"
         mNrAcctPos     = "            НЕТ ДОПУСКА"
         mNrShBalClose  = "            НЕТ ДОПУСКА"
         mNrShBalNa     = "            НЕТ ДОПУСКА"
         mNrShAvPos     = "            НЕТ ДОПУСКА"
         mNrShAvCur     = "            НЕТ ДОПУСКА"
      .
   END.
   ASSIGN
      vFldH          =  GetWidgetHandle (vFrmh, "vDateAcctPos")
      vFldH:DCOLOR   =  GetDCOLOR ("green")
      vFldH          =  GetWidgetHandle (vFrmh, "close-date")
      vFldH:DCOLOR   =  GetDCOLOR ("bright-yellow")

                        /* Формирование цвета, формат для отображения остатка. */
      vFldH          =  GetWidgetHandle (vFrmh, "vAcctCur")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAcctCur))
      vFldH:FORMAT   =  IF vAcctCur GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      vAcctCur       =  ABSOLUTE (vAcctCur)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-val-close")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-val-close))
      vFldH:FORMAT   =  IF sh-val-close GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      sh-val-close   =  ABSOLUTE (sh-val-close)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-val-na")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-val))
      vFldH:FORMAT   =  IF sh-val GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      sh-val-na      =  ABSOLUTE (sh-val)

      vFldH          =  GetWidgetHandle (vFrmh, "vAcctPos")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAcctPos))
      vFldH:FORMAT   =  IF vAcctPos GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      vAcctPos       =  ABSOLUTE (vAcctPos)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-bal-close")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-bal-close))
      vFldH:FORMAT   =  IF sh-bal-close GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      sh-bal-close   =  ABSOLUTE (sh-bal-close)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-bal-na")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-bal))
      vFldH:FORMAT   =  IF sh-bal GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      sh-bal-na      =  ABSOLUTE (sh-bal)

      vFldH          =  GetWidgetHandle (vFrmh, "vAvCur")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAvCur))
      vFldH:FORMAT   =  IF vAvCur GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      vAvCur         =  ABSOLUTE (vAvCur)

      vFldH          =  GetWidgetHandle (vFrmh, "vAvPos")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAvPos))
      vFldH:FORMAT   =  IF vAvPos GE 0
                           THEN "Дбzz,zzz,zzz,zzz,zz9.99"
                           ELSE "Крzz,zzz,zzz,zzz,zz9.99"
      vAvPos         =  ABSOLUTE (vAvPos)
      .

   IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code") THEN
      SetHelpStrAdd("F3 - В инвалюте/Остаток ЦБ").

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION
PROCEDURE PostSetObject :
DEF VAR vOk          AS LOG    NO-UNDO. /* Флаг ошибки. */
   DEF VAR vError       AS LOG    NO-UNDO. /* Флаг ошибки. */
   DEF VAR vField       AS CHAR   NO-UNDO. /* Код поля ошибки. */
   DEF VAR vProcName    AS CHAR   NO-UNDO. /* Процедура метода. */
   DEF VAR vUpdValid    AS INT64    NO-UNDO. /* Код ошибки. */

   DEFINE VARIABLE vClient      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClDate      AS DATE        NO-UNDO.
   DEFINE VARIABLE vDate-In     AS DATE        NO-UNDO.
   DEFINE VARIABLE vIsStopList  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vStr         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStatIP      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDocID       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDoc         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMaskAcct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFlChk       AS LOGICAL     NO-UNDO.
   DEFINE BUFFER   xop-date1 FOR op-date.
   DEFINE BUFFER   xop-date2 FOR op-date.   
   DEFINE BUFFER   person    FOR person.
   DEFINE BUFFER   cust-corp FOR cust-corp. 
   
                        /* Поиск сохраненного счета. */
                        
                        
   {find-act.i
      &acct    = tt-acct.acct
      &curr    = tt-acct.currency
   }
   IF NOT AVAIL acct
   THEN DO:
      RUN Fill-SysMes IN h_tmess (
         "", "18l", "",
         "%S=" + tt-acct.acct + "/" + tt-acct.currency
      ).
      RETURN ERROR.
   END.

   IF (iMode EQ {&MOD_ADD} OR (iMode EQ {&MOD_EDIT}  AND tt-acct.open-date:SENSITIVE IN FRAME fMain)) AND vEditSet EQ "ДА"
   THEN DO:
 
       vClDate = Get_CloseDate_Cat("b").
       FIND FIRST xop-date1 WHERE xop-date1.op-date EQ acct.open-date NO-LOCK NO-ERROR.
       IF NOT AVAIL(xop-date1) AND acct.open-date  NE ? THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","На дату открытия отсутствует операционный день").
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.  
       END.
       FIND FIRST xop-date2 WHERE xop-date2.op-date EQ acct.close-date NO-LOCK NO-ERROR.
       IF NOT AVAIL(xop-date2) AND acct.close-date NE ? THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","На дату закрытия отсутствует операционный день").
          APPLY "ENTRY" TO tt-acct.close-date IN FRAME fmain.
          RETURN ERROR.  
       END.       
      IF acct.open-date LE vClDate OR
         acct.close-date LE vClDate THEN
      DO:
          RUN Fill-SysMes (
               "", "", "-1",
               (IF acct.open-date LE vClDate THEN
                   "Операционный день на дату открытия закрыт"
                ELSE
                   "Операционный день на дату закрытия закрыт")               
          ).       
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.        
      END.
      IF acct.open-date LE vClDate OR
         acct.close-date LE vClDate THEN
      DO:
          RUN Fill-SysMes (
               "", "", "-1",
               (IF acct.open-date LE vClDate THEN
                   "Операционный день на дату открытия закрыт"
                ELSE
                   "Операционный день на дату закрытия закрыт")               
          ).       
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.        
      END.
   END.

   IF (iMode EQ {&MOD_ADD}   OR iMode EQ {&MOD_EDIT} ) AND
      LOOKUP(acct.cust-cat,"Ю,Б,Ч") > 0
   THEN DO:
      /* проверка даты заведения клиента */
      vDate-In = DATE(getValueAttr(getCustClass(acct.cust-cat),
                      STRING(acct.cust-id),
                     "date-in")) NO-ERROR.
       IF vDate-In > acct.open-date THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","ВНИМАНИЕ! Дата открытия счета не может быть меньше даты регистрации клиента!").
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.  
       END.
   END.
                        /* Инициализация доп.реквизитов со счета 2-го порядка
                        ** и из классификатора "МаскиНаслед". */
   RUN BalToAcct_Xattr ( RECID (acct), "*", YES, YES).

   RUN Check-Acct IN h_acct (
      BUFFER acct,
      OUTPUT vField,
      OUTPUT vUpdValid
   ).
   IF vUpdValid NE 0
      THEN RETURN ERROR.

   IF iMode EQ {&MOD_ADD}
   THEN DO:
                        /* Удаляем счет из классификатора "СчетаРезерва". */
      RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
      ASSIGN
         vProcName = GET-CLASS-METHOD (tt-acct.class-code, "U4").
      IF vProcName NE ?
      THEN DO:
         IF SearchPFile (vProcName)
            THEN RUN VALUE (vProcName + ".p") ( RECID (acct)).
         ELSE DO:
            RUN Fill-SysMes (
               "", "", "0",
               "Не найдена процедура удаления счетчика " + vProcName + ".p."
            ).
            RETURN ERROR.
         END.
      END.
   END.

   vClient = vNameClient:SCREEN-VALUE IN FRAME {&MAIN-FRAME}.
   {lg7001cl.i
      &in-class  = 'acct'
      &surrogate = "tt-acct.acct + ',' + tt-acct.currency"
      &cl_name1  = "vClient"
      &nodefpesr = YES
   }

   vError = FALSE.
   RUN LoanChk (tt-acct.contract,
                tt-acct.acct,
                tt-acct.currency,
                tt-acct.cust-cat,
                tt-acct.cust-id,
                INPUT-OUTPUT vError).

   /*Проверка по стоп-листам*/
   IF iMode EQ {&MOD_ADD}   OR iMode EQ {&MOD_EDIT} 
   THEN DO:
      RUN ChkByStopList (tt-acct.cust-cat,tt-acct.cust-id,"КонтрВводРед_Сч","CLNT",OUTPUT vIsStopList).
      IF vIsStopList THEN
      DO:
         pick-value = "NO".   
         RUN Fill-SysMes IN h_tmess (  "", "", "4", "Клиент явлется фигурантом справочника Стоп-листы! Продолжить ввод счета?").
      END.
      IF pick-value = "NO" THEN
      DO:
         {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
         RUN PrintClientSLRep.
         {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
         RETURN ERROR.
      END.
   END.  

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   vStr = FGetSetting("ОбменФМС","ФМССчета_Ред","").
   IF (iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT}) AND
      (FGetSetting("ОбменФМС","ФМСРед_Сч","") EQ "Да") AND
      CAN-DO(vStr,STRING(tt-acct.bal-acct))
   THEN DO:
      vFlChk = no.
      IF tt-acct.cust-cat EQ "Ю" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ tt-acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN DO:
            ASSIGN
               vStr    = GetXattrValueEX("cust-corp", STRING(tt-acct.cust-id), "Предпр", "")
               vStatIP = FGetSetting("СтандТр","СтатусФЛЧП","")
            .
            IF FGetSetting("ОбменФМС","ФМСРед_КлЮ","") EQ "Да" AND
               (vStr NE "" OR CAN-DO(vStatIP,cust-corp.cust-stat))
            THEN 
               ASSIGN
                  vStr    = "cust-corp"
                  vDocID  = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document-id","")
                  vDoc    = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document","")
                  vClient = cust-corp.name-corp
                  vFlChk  = yes
               .
         END.
      END.   
      ELSE
         IF tt-acct.cust-cat EQ "Ч" AND 
            FGetSetting("ОбменФМС","ФМСРед_Кл","") EQ "Да"
         THEN DO:
            FIND FIRST person WHERE person.person-id EQ tt-acct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL person THEN 
               ASSIGN
                  vStr    = "person"
                  vDocID  = person.document-id
                  vDoc    = person.document
                  vClient = person.name-last + " " + person.first-names
                  vFlChk  = yes
               .
         END.
      IF vFlChk THEN DO:
         {cl-fmschk.i
            "'acct'"
            "tt-acct.acct + ',' + tt-acct.currency"
            "vDocID"
            "vDoc"
            "GetXattrValueEX(vStr, STRING(tt-acct.cust-id), 'Document4Date_vid', '')"
            "vClient"
         }
      END.
   END. 

   IF (iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT} ) AND
      LOOKUP(acct.cust-cat,"Ю,Б,Ч") > 0
   THEN DO:
      vMaskAcct = FGetSetting("СтандТр","КонтрСчетДок","").
      IF CAN-DO(vMaskAcct,acct.acct) THEN
      DO:
         RUN SetSysConf IN h_base ("PROCESS_OP-EDIT","Да").
         RUN chkpersdoc.p ("",acct.cust-cat,acct.cust-id,gend-date,"3").
         RUN SetSysConf IN h_base ("PROCESS_OP-EDIT",?).
         IF RETURN-VALUE EQ "YES" THEN
            RETURN ERROR.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoanChk TERMINAL-SIMULATION
/* перенесено из acct-kl(.p */
PROCEDURE LoanChk :
   DEF INPUT        PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT        PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCustCat  AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCustId   AS INT64  NO-UNDO.
   DEF INPUT-OUTPUT PARAM ioError   AS LOG  NO-UNDO.

   DEF VAR vError AS LOG NO-UNDO INITIAL FALSE.

   DEF BUFFER cacct      FOR acct.
   DEF BUFFER loan-acct  FOR loan-acct.
   DEF BUFFER xloan-acct FOR loan-acct.
   DEF BUFFER bloan      FOR loan.

   IF     iContract EQ "Кредит"
      OR  iContract EQ "Депоз"
      AND iCustCat  NE "В" THEN
   DO:
      FOR EACH loan-acct WHERE
               loan-acct.contract  EQ iContract
           AND loan-acct.acct      EQ iAcct
           AND loan-acct.currency  EQ iCurrency
           AND loan-acct.acct-type EQ loan-acct.contract NO-LOCK,
          EACH loan WHERE
               loan.contract       EQ loan-acct.contract
           AND loan.cont-code      EQ loan-acct.cont-code
         NO-LOCK
         BREAK BY loan.cont-code :
         IF LAST-OF(loan.cont-code) THEN
         DO:
            vError = FALSE.
            LOOP:
            FOR EACH xloan-acct OF loan WHERE
                     xloan-acct.acct <> iAcct
               NO-LOCK:

               IF GetCodeMisc("ТипСчДог",xloan-acct.acct-type,1) EQ "Нет" THEN
                  NEXT.

               FIND FIRST cacct WHERE
                          cacct.acct     EQ xloan-acct.acct
                      AND cacct.currency EQ xloan-acct.currency NO-LOCK NO-ERROR.

               IF cacct.cust-cat NE "В"
                  AND (   cacct.cust-cat NE iCustCat
                       OR cacct.cust-id  NE iCustId) THEN
               DO:
                  IF ioError THEN
                  DO:
                     RUN Fill-SysMes("","","-1",
                            "Счет по договору " + loan.cont-code  + "~n" +
                            "У договора есть счета с другими клиентами").
                     RETURN.
                  END.
                  vError = TRUE.
                  LEAVE LOOP.
               END.
            END.
            IF NOT ioError AND NOT vError THEN DO:
              fixloan:
              DO TRANSACTION
                 ON ERROR  UNDO fixloan, LEAVE fixloan
                 ON ENDKEY UNDO fixloan, LEAVE fixloan:
                 FIND FIRST bloan WHERE
                            ROWID(bloan) = ROWID(loan) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                  IF AVAILABLE bloan THEN
                  ASSIGN
                    bloan.cust-cat = iCustCat
                    bloan.cust-id  = iCustId
                    .
                 RELEASE bloan.
              END.
            END.

         END.
      END.
   END.
   ioError = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='29/02/2016 14:42:53.296+04:00' */
/* $LINTUSER='miam' */
/* $LINTMODE='1' */
/* $LINTFILE='f-acct.p' */
/*prosignOz9Hvcq92wLb0UiL/NnjPQ*/