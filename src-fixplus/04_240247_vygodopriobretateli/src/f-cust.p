&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-corp NO-UNDO LIKE cust-corp
       FIELD regorgan$ AS CHARACTER /* РегОрган */
       FIELD reorginwaz$ AS CHARACTER /* РеоргИнЯз */
       FIELD reorgogrn$ AS CHARACTER /* РеоргОГРН */
       FIELD reorginn$ AS CHARACTER /* РеоргИНН */
       FIELD BQMail AS CHARACTER /* BQMail */
       FIELD BQSms AS CHARACTER /* BQSms */
       FIELD abwawtik$ AS CHARACTER /* АбЯщик */
       FIELD adresp$ AS CHARACTER /* АдресП */
       FIELD bankwemitent$ AS CHARACTER /* БанкЭмитент */
       FIELD bki_naimrf$ AS CHARACTER /* БКИ_НаимРФ */
       FIELD bki_naimwazyk$ AS CHARACTER /* БКИ_НаимЯзык */
       FIELD blok$ AS LOGICAL /* Блок */
       FIELD viddewat$ AS CHARACTER /* ВидДеят */
       FIELD vidkli$ AS CHARACTER /* ВидКли */
       FIELD gvk$ AS CHARACTER /* ГВК */
       FIELD grawzd$ AS CHARACTER /* Гражд */
       FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
       FIELD dataogrn$ AS DATE /* ДатаОГРН */
       FIELD datareg$ AS DATE /* ДатаРег */
       FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
       FIELD dko$ AS DECIMAL /* ДкО */
       FIELD dkowe$ AS DECIMAL /* ДкОЭ */
       FIELD dolruk$ AS CHARACTER /* ДолРук */
       FIELD iin$ AS CHARACTER /* ИИН */
       FIELD istoriwakl$ AS CHARACTER /* ИсторияКл */
       FIELD kategklient$ AS CHARACTER /* КатегКлиент */
       FIELD klient$ AS CHARACTER /* Клиент */
       FIELD klientuf$ AS LOGICAL /* КлиентУФ */
       FIELD koddokum$ AS CHARACTER /* КодДокум */
       FIELD kodklienta$ AS CHARACTER /* КодКлиента */
       FIELD kodpriwcposuwcet$ AS CHARACTER /* КодПричПосУчет */
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodsubki$ AS CHARACTER /* КодСубКИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD koldir$ AS INT64 /* КолДир */
       FIELD kolrab$ AS INT64 /* КолРаб */
       FIELD kop$ AS INT64 /* КОП */
       FIELD kopf$ AS INT64 /* КОПФ */
       FIELD korpkl$ AS CHARACTER /* КорпКл */
       FIELD kpp$ AS CHARACTER /* КПП */
       FIELD licvydnaim$ AS CHARACTER /* ЛицВыдНаим */
       FIELD licdatan$ AS CHARACTER /* ЛицДатаН */
       FIELD licdatao$ AS CHARACTER /* ЛицДатаО */
       FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
       FIELD lictip$ AS CHARACTER /* ЛицТип */
       FIELD materinkomp$ AS CHARACTER /* МАТЕРИНкомп */
       FIELD migrkart$ AS CHARACTER /* МигрКарт */
       FIELD migrpravprebpo$ AS DATE /* МигрПравПребПо */
       FIELD migrpravprebs$ AS DATE /* МигрПравПребС */
       FIELD migrprebyvpo$ AS DATE /* МигрПребывПо */
       FIELD migrprebyvs$ AS DATE /* МигрПребывС */
       FIELD migrcelw#vizita$ AS CHARACTER /* МигрЦельВизита */
       FIELD nbki_godob$ AS CHARACTER /* НБКИ_ГодОб */
       FIELD nbki_stpowct$ AS CHARACTER /* НБКИ_СтПочт */
       FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
       FIELD nomdop$ AS CHARACTER /* НомДоп */
       FIELD nomerpf$ AS CHARACTER /* НомерПФ */
       FIELD obosobpodr$ AS CHARACTER /* ОбособПодр */
       FIELD ogrn$ AS CHARACTER /* ОГРН */
       FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
       FIELD okvwed$ AS CHARACTER /* ОКВЭД */
       FIELD okogu$ AS INT64 /* ОКОГУ */
       FIELD okopf$ AS CHARACTER /* ОКОПФ */
       FIELD orguprav$ AS CHARACTER /* ОргУправ */
       FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
       FIELD osnova$ AS CHARACTER /* основа */
       FIELD ofwsor$ AS CHARACTER /* Офшор */
       FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
       FIELD pokrytie$ AS LOGICAL /* Покрытие */
       FIELD postkontrag$ AS CHARACTER /* ПостКонтраг */
       FIELD predpr$ AS LOGICAL /* Предпр */
       FIELD prim$ AS CHARACTER /* Прим */
       FIELD prim1$ AS CHARACTER /* Прим1 */
       FIELD prim2$ AS CHARACTER /* Прим2 */
       FIELD prim3$ AS CHARACTER /* Прим3 */
       FIELD prim4$ AS CHARACTER /* Прим4 */
       FIELD prim5$ AS CHARACTER /* Прим5 */
       FIELD prim6$ AS CHARACTER /* Прим6 */
       FIELD prisutorguprav$ AS CHARACTER /* ПрисутОргУправ */
       FIELD priwcvnes$ AS CHARACTER /* ПричВнес */
       FIELD reorgdata$ AS DATE /* РеоргДата */
       FIELD reorgegrn$ AS CHARACTER /* РеоргЕГРН */
       FIELD reorgkpp$ AS CHARACTER /* РеоргКПП */
       FIELD reorgokato$ AS CHARACTER /* РеоргОКАТО */
       FIELD reorgokpo$ AS CHARACTER /* РеоргОКПО */
       FIELD reorgpolnoe$ AS CHARACTER /* РеоргПолное */
       FIELD reorgsokr$ AS CHARACTER /* РеоргСокр */
       FIELD reorgfirm$ AS CHARACTER /* РеоргФирм */
       FIELD reorgwazykrf$ AS CHARACTER /* РеоргЯзыкРФ */
       FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
       FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
       FIELD statusdata$ AS DATE /* СтатусДата */
       FIELD statuspredpr$ AS CHARACTER /* СтатусПредпр */
       FIELD struktorg$ AS CHARACTER /* СтруктОрг */
       FIELD subw%ekt$ AS CHARACTER /* Субъект */
       FIELD tipkl$ AS CHARACTER /* ТипКл */
       FIELD unikkodadresa$ AS CHARACTER /* УникКодАдреса */
       FIELD unk$ AS DECIMAL /* УНК */
       FIELD unkg$ AS INT64 /* УНКг */
       FIELD ustavkap$ AS CHARACTER /* УставКап */
       FIELD uwcdok$ AS CHARACTER /* УчДок */
       FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
       FIELD uwcdokdata$ AS DATE /* УчДокДата */


       FIELD uwcredorg$ AS CHARACTER /* УчредОрг */
       FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
       FIELD fioruk$ AS CHARACTER /* ФИОрук */
       FIELD formsobs$ AS CHARACTER /* ФормСобс */
       FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
       FIELD wekonsekt$ AS CHARACTER /* ЭконСект */
       FIELD Address1Indeks AS INT64 /* Address1Indeks */
       FIELD BirthDay AS DATE /* BirthDay */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD branch-id AS CHARACTER /* branch-id */
       FIELD branch-list AS CHARACTER /* branch-list */
       FIELD brand-name AS CHARACTER /* brand-name */
       FIELD CMSCUR AS DECIMAL /* CMSCUR */
       FIELD contr_group AS CHARACTER /* contr_group */
       FIELD cont_type AS CHARACTER /* cont_type */
       FIELD country-id2 AS CHARACTER /* country-id2 */
       FIELD country-id3 AS CHARACTER /* country-id3 */
       FIELD CountryCode AS CHARACTER /* CountryCode */
       FIELD CRSCM AS CHARACTER /* CRSCM */
       FIELD date-export AS CHARACTER /* date-export */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD document AS CHARACTER /* document */
       FIELD document-id AS CHARACTER /* document-id */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD e-mail AS CHARACTER /* e-mail */
       FIELD engl-name AS CHARACTER /* engl-name */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD holding-id AS CHARACTER /* holding-id */
       FIELD iey AS CHARACTER /* iey */
       FIELD IndCode AS CHARACTER /* IndCode */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD issue AS CHARACTER /* issue */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD lic-sec AS CHARACTER /* lic-sec */
       FIELD LocCustType AS CHARACTER /* LocCustType */
       FIELD mess AS CHARACTER /* mess */
       FIELD NACE AS CHARACTER /* NACE */
       FIELD Netting AS LOGICAL /* Netting */
       FIELD NoExport AS LOGICAL /* NoExport */
       FIELD num_contr AS INT64 /* num_contr */
       FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
       FIELD Prim-ID AS CHARACTER /* Prim-ID */
       FIELD RegDate AS CHARACTER /* RegDate */
       FIELD RegNum AS CHARACTER /* RegNum */
       FIELD RegPlace AS CHARACTER /* RegPlace */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD Soato AS CHARACTER /* Soato */
       FIELD SphereID AS CHARACTER /* SphereID */
       FIELD tel AS CHARACTER /* tel */
       FIELD Telex AS CHARACTER /* Telex */
       FIELD Visa AS CHARACTER /* Visa */
       FIELD VisaNum AS CHARACTER /* VisaNum */
       FIELD VisaType AS CHARACTER /* VisaType */
       FIELD lat_name AS CHARACTER /* lat_name */
       FIELD wembnazv$ AS CHARACTER /* ЭмбНазв */
       FIELD country-id4 AS CHARACTER /* country-id4 */
       FIELD website AS CHARACTER /* website */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-cust-corp" "" "NO-INDEX"}
       .
DEFINE TEMP-TABLE tt-p-cust-adr NO-UNDO LIKE cust-ident
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
       {ln-tthdl.i "tt-p-cust-adr" "p-cust-adr" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-CUST.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 05.04.2006 12:50 ILVI (40632)
     Modified: 29.11.2010 18:46 Kraa (0115851) Убрана возможность очистки поля Региона ГНИ по f8
     Modified: 08/12/2010 kraa (0111435) доработана процедура postSetObject.
     Modified: 15/11/2012 ccc  (0133400) доработана обработка ИНН
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

&GLOBAL-DEFINE MAIN-FRAME fMain
/* триггеры для работы с ГНИ */

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

{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get cust}     /* Библиотека для работы с клиентами. */
{intrface.get op}       /* Библиотека для работы с документами. */
{intrface.get exch}      /*  */
{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}

DEFINE VARIABLE mType        AS CHARACTER NO-UNDO.  /* тип идентиифкатора банка */
DEFINE VARIABLE mTempVal     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlagUnk     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mTmpUnk      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctKey     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdrLogic    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFrmRole     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClient      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mScreenValue AS CHARACTER NO-UNDO. /* Значение на экране. */
DEFINE VARIABLE vAdrCntry    AS CHARACTER NO-UNDO. /* адрес страны */
DEFINE VARIABLE mAdrType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdrCntXattr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUniqCodAdr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLastOkOGRN  AS CHARACTER NO-UNDO.

DEFINE BUFFER bcident FOR cust-ident. /* Локализация буфера. */

{cust-adr.obj
   &def-vars-gni = YES
}

&GLOBAL-DEFINE FlowFields  tt-cust-corp.unk$
/* проверять права на группы реквизитов */
&GLOBAL-DEFINE CHECK-XATTR-GROUP-PERMISSION

RUN GetTypeMainAdr IN h_cust('Ю',OUTPUT mAdrType,OUTPUT mAdrCntXattr).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cust-corp

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-cust-corp.unk$ ~
tt-cust-corp.cust-id tt-cust-corp.date-in tt-cust-corp.date-out ~
tt-cust-corp.cust-stat tt-cust-corp.name-corp tt-cust-corp.name-short ~
tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.kodreg$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel ~
tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-corp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-corp
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-corp SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-corp SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-corp
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-corp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-TABLES tt-cust-corp
&Scoped-define FIRST-ENABLED-TABLE tt-cust-corp
&Scoped-Define ENABLED-OBJECTS mBankClient separator1 vOblChar vGorChar ~
vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt ~
separator2 mFormSobs mWebsite 
&Scoped-Define DISPLAYED-FIELDS tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define DISPLAYED-TABLES tt-cust-corp
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-corp
&Scoped-Define DISPLAYED-OBJECTS mBankClient separator1 vOblChar vGorChar ~
vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt ~
separator2 mFormSobs mWebsite 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in ~
tt-cust-corp.cust-stat tt-cust-corp.name-corp tt-cust-corp.name-short ~
tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.benacct 
&Scoped-define List-2 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in ~
tt-cust-corp.date-out tt-cust-corp.cust-stat tt-cust-corp.name-corp ~
tt-cust-corp.name-short tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] ~
tt-cust-corp.kodreggni$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel ~
tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.benacct ~
tt-cust-corp.website
&Scoped-define List-3 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ mFormSobs tt-cust-corp.subw%ekt$ ~
tt-cust-corp.bank-code-type tt-cust-corp.bank-code tt-cust-corp.corr-acct ~
tt-cust-corp.benacct tt-cust-corp.website
&Scoped-define List-4 tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mBankClient AS LOGICAL FORMAT "клиент/нет":U INITIAL NO 
     LABEL "Отношение к банку" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mFormSobs AS CHARACTER FORMAT "X(256)":U 
     LABEL "Форма собственности" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
     &ELSE SIZE 27 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mWebsite AS CHARACTER FORMAT "X(256)":U 
     LABEL "Сайт компании" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
     &ELSE SIZE 18 BY 1.01 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAdrIndInt AS INT64 FORMAT "999999":U INITIAL ? 
     LABEL "Индекс" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDomChar AS CHARACTER FORMAT "X(8)":U 
     LABEL "Дом" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vGorChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Город" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKorpChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "Корпус" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKvChar AS CHARACTER FORMAT "X(30)":U 
     LABEL "Офис" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vOblChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Район" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vPunktChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Нас.пункт" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vStrChar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Стр." 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vUlChar AS CHARACTER FORMAT "X(61)":U 
     LABEL "Улица" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 62 BY 1
     &ELSE SIZE 62 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-corp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mBankClient
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 20 COLON-ALIGNED
          &ELSE AT ROW 1 COL 20 COLON-ALIGNED &ENDIF HELP
          "Является ли субъект клиентом банка."
     tt-cust-corp.unk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 49 COLON-ALIGNED
          &ELSE AT ROW 1 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 49 COLON-ALIGNED
          &ELSE AT ROW 1 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-cust-corp.date-in
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 15 COLON-ALIGNED
          &ELSE AT ROW 2 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.date-out
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 49 COLON-ALIGNED
          &ELSE AT ROW 2 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.cust-stat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 15 COLON-ALIGNED
          &ELSE AT ROW 3 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 37 BY 1
          &ELSE SIZE 37 BY 1 &ENDIF
     tt-cust-corp.name-corp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 15 COLON-ALIGNED
          &ELSE AT ROW 4 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     tt-cust-corp.name-short
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 15 COLON-ALIGNED
          &ELSE AT ROW 5 COL 15 COLON-ALIGNED &ENDIF
          LABEL "Краткое наимен"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     tt-cust-corp.country-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 15 COLON-ALIGNED
          &ELSE AT ROW 6 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     tt-cust-corp.addr-of-low[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 8 COLON-ALIGNED
          &ELSE AT ROW 8 COL 8 COLON-ALIGNED &ENDIF HELP
          "Индекс,Район,Город,Нас.пункт,Ул.,Дом,Корп.,Кв."
          LABEL "Адрес" FORMAT "x(300)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 69 BY 1
          &ELSE SIZE 69 BY 1 &ENDIF
     tt-cust-corp.kodreggni$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 11 COLON-ALIGNED
          &ELSE AT ROW 8 COL 11 COLON-ALIGNED &ENDIF HELP
          "Код региона ГНИ"
          LABEL "Регион ГНИ" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.kodreg$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 27 COLON-ALIGNED
          &ELSE AT ROW 8 COL 27 COLON-ALIGNED &ENDIF
          LABEL "Регион"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vOblChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 49 COLON-ALIGNED
          &ELSE AT ROW 8 COL 49 COLON-ALIGNED &ENDIF HELP
          "Район"
     tt-cust-corp.addr-of-low[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 8 COLON-ALIGNED
          &ELSE AT ROW 9 COL 8 COLON-ALIGNED &ENDIF HELP
          "Индекс,Район,Город,Нас.пункт,Ул.,Дом,Корп.,Кв."
          LABEL ""
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     vGorChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 11 COLON-ALIGNED
          &ELSE AT ROW 9 COL 11 COLON-ALIGNED &ENDIF HELP
          "Город"
     vPunktChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 49 COLON-ALIGNED
          &ELSE AT ROW 9 COL 49 COLON-ALIGNED &ENDIF HELP
          "Населенный пункт"
     vUlChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 11 COLON-ALIGNED
          &ELSE AT ROW 10 COL 11 COLON-ALIGNED &ENDIF HELP
          "Улица"
     vDomChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 11 COLON-ALIGNED
          &ELSE AT ROW 11 COL 11 COLON-ALIGNED &ENDIF HELP
          "Дом"
     vStrChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 23 COLON-ALIGNED
          &ELSE AT ROW 11 COL 23 COLON-ALIGNED &ENDIF
     vKorpChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 37 COLON-ALIGNED
          &ELSE AT ROW 11 COL 37 COLON-ALIGNED &ENDIF HELP
          "Корпус"
     vKvChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 49 COLON-ALIGNED
          &ELSE AT ROW 11 COL 49 COLON-ALIGNED &ENDIF HELP
          "Офис(квартира)"
     vAdrIndInt
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 67 COLON-ALIGNED
          &ELSE AT ROW 11 COL 67 COLON-ALIGNED &ENDIF HELP
          "NORMAL"
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 1
          &ELSE AT ROW 12 COL 1 &ENDIF NO-LABEL
     tt-cust-corp.tel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 15 COLON-ALIGNED
          &ELSE AT ROW 13 COL 15 COLON-ALIGNED &ENDIF
          LABEL "Телефон"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
     tt-cust-corp.fax
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 49 COLON-ALIGNED
          &ELSE AT ROW 13 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
   tt-cust-corp.tax-insp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 15 COLON-ALIGNED
          &ELSE AT ROW 14 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.inn
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 49 COLON-ALIGNED
          &ELSE AT ROW 14 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     tt-cust-corp.ogrn$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 15 COLON-ALIGNED
          &ELSE AT ROW 15 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-cust-corp.okvwed$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 49 COLON-ALIGNED
          &ELSE AT ROW 15 COL 49 COLON-ALIGNED &ENDIF
          LABEL "ОКВЭД"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.okpo
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 15 COLON-ALIGNED
          &ELSE AT ROW 16 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     /*tt-cust-corp.okonx
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 49 COLON-ALIGNED
          &ELSE AT ROW 16 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
         &ELSE SIZE 16 BY 1 &ENDIF*/
     tt-cust-corp.kpp$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 15 COLON-ALIGNED
          &ELSE AT ROW 17 COL 15 COLON-ALIGNED &ENDIF
          LABEL "КПП"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.formsobs$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 49 COLON-ALIGNED
          &ELSE AT ROW 17 COL 49 COLON-ALIGNED &ENDIF
          LABEL "Код ФормСобс"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mFormSobs
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 49 COLON-ALIGNED
          &ELSE AT ROW 17 COL 49 COLON-ALIGNED &ENDIF
     tt-cust-corp.subw%ekt$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 15 COLON-ALIGNED
          &ELSE AT ROW 18 COL 15 COLON-ALIGNED &ENDIF
          LABEL "Субъект" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.bank-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 49 COLON-ALIGNED
          &ELSE AT ROW 18 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-cust-corp.bank-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 15 COLON-ALIGNED
          &ELSE AT ROW 19 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.corr-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 49 COLON-ALIGNED
          &ELSE AT ROW 19 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     tt-cust-corp.website
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 15 COLON-ALIGNED
          &ELSE AT ROW 20 COL 15 COLON-ALIGNED &ENDIF
          LABEL "Web-сайт"
          FORMAT "X(256)":U
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     tt-cust-corp.benacct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 49 COLON-ALIGNED
          &ELSE AT ROW 20 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     mWebsite
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 15 COLON-ALIGNED
          &ELSE AT ROW 21 COL 15 COLON-ALIGNED &ENDIF
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
      TABLE: tt-cust-corp T "?" NO-UNDO bisquit cust-corp
      ADDITIONAL-FIELDS:
          FIELD regorgan$ AS CHARACTER /* РегОрган */
          FIELD reorginwaz$ AS CHARACTER /* РеоргИнЯз */
          FIELD reorgogrn$ AS CHARACTER /* РеоргОГРН */
          FIELD reorginn$ AS CHARACTER /* РеоргИНН */
          FIELD BQMail AS CHARACTER /* BQMail */
          FIELD BQSms AS CHARACTER /* BQSms */
          FIELD abwawtik$ AS CHARACTER /* АбЯщик */
          FIELD adresp$ AS CHARACTER /* АдресП */
          FIELD bankwemitent$ AS CHARACTER /* БанкЭмитент */
          FIELD bki_naimrf$ AS CHARACTER /* БКИ_НаимРФ */
          FIELD bki_naimwazyk$ AS CHARACTER /* БКИ_НаимЯзык */
          FIELD blok$ AS LOGICAL /* Блок */
          FIELD viddewat$ AS CHARACTER /* ВидДеят */
          FIELD vidkli$ AS CHARACTER /* ВидКли */
          FIELD gvk$ AS CHARACTER /* ГВК */
          FIELD grawzd$ AS CHARACTER /* Гражд */
          FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
          FIELD dataogrn$ AS DATE /* ДатаОГРН */
          FIELD datareg$ AS DATE /* ДатаРег */
          FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
          FIELD dko$ AS DECIMAL /* ДкО */
          FIELD dkowe$ AS DECIMAL /* ДкОЭ */
          FIELD dolruk$ AS CHARACTER /* ДолРук */
          FIELD iin$ AS CHARACTER /* ИИН */
          FIELD istoriwakl$ AS CHARACTER /* ИсторияКл */
          FIELD kategklient$ AS CHARACTER /* КатегКлиент */
          FIELD klient$ AS CHARACTER /* Клиент */
          FIELD klientuf$ AS LOGICAL /* КлиентУФ */
          FIELD koddokum$ AS CHARACTER /* КодДокум */
          FIELD kodklienta$ AS CHARACTER /* КодКлиента */
          FIELD kodpriwcposuwcet$ AS CHARACTER /* КодПричПосУчет */
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodsubki$ AS CHARACTER /* КодСубКИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD koldir$ AS INT64 /* КолДир */
          FIELD kolrab$ AS INT64 /* КолРаб */
          FIELD kop$ AS INT64 /* КОП */
          FIELD kopf$ AS INT64 /* КОПФ */
          FIELD korpkl$ AS CHARACTER /* КорпКл */
          FIELD kpp$ AS CHARACTER /* КПП */
          FIELD licvydnaim$ AS CHARACTER /* ЛицВыдНаим */
          FIELD licdatan$ AS CHARACTER /* ЛицДатаН */
          FIELD licdatao$ AS CHARACTER /* ЛицДатаО */
          FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
          FIELD lictip$ AS CHARACTER /* ЛицТип */
          FIELD materinkomp$ AS CHARACTER /* МАТЕРИНкомп */
          FIELD migrkart$ AS CHARACTER /* МигрКарт */
          FIELD migrpravprebpo$ AS DATE /* МигрПравПребПо */
          FIELD migrpravprebs$ AS DATE /* МигрПравПребС */
          FIELD migrprebyvpo$ AS DATE /* МигрПребывПо */
          FIELD migrprebyvs$ AS DATE /* МигрПребывС */
          FIELD migrcelw#vizita$ AS CHARACTER /* МигрЦельВизита */
          FIELD nbki_godob$ AS CHARACTER /* НБКИ_ГодОб */
          FIELD nbki_stpowct$ AS CHARACTER /* НБКИ_СтПочт */
          FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
          FIELD nomdop$ AS CHARACTER /* НомДоп */
          FIELD nomerpf$ AS CHARACTER /* НомерПФ */
          FIELD obosobpodr$ AS CHARACTER /* ОбособПодр */
          FIELD ogrn$ AS CHARACTER /* ОГРН */
          FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
          FIELD okvwed$ AS CHARACTER /* ОКВЭД */
          FIELD okogu$ AS INT64 /* ОКОГУ */
          FIELD okopf$ AS CHARACTER /* ОКОПФ */
          FIELD orguprav$ AS CHARACTER /* ОргУправ */
          FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
          FIELD osnova$ AS CHARACTER /* основа */
          FIELD ofwsor$ AS CHARACTER /* Офшор */
          FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
          FIELD pokrytie$ AS LOGICAL /* Покрытие */
          FIELD postkontrag$ AS CHARACTER /* ПостКонтраг */
          FIELD predpr$ AS LOGICAL /* Предпр */
          FIELD prim$ AS CHARACTER /* Прим */
          FIELD prim1$ AS CHARACTER /* Прим1 */
          FIELD prim2$ AS CHARACTER /* Прим2 */
          FIELD prim3$ AS CHARACTER /* Прим3 */
          FIELD prim4$ AS CHARACTER /* Прим4 */
          FIELD prim5$ AS CHARACTER /* Прим5 */
          FIELD prim6$ AS CHARACTER /* Прим6 */
          FIELD prisutorguprav$ AS CHARACTER /* ПрисутОргУправ */
          FIELD priwcvnes$ AS CHARACTER /* ПричВнес */
          FIELD reorgdata$ AS DATE /* РеоргДата */
          FIELD reorgegrn$ AS CHARACTER /* РеоргЕГРН */
          FIELD reorgkpp$ AS CHARACTER /* РеоргКПП */
          FIELD reorgokato$ AS CHARACTER /* РеоргОКАТО */
          FIELD reorgokpo$ AS CHARACTER /* РеоргОКПО */
          FIELD reorgpolnoe$ AS CHARACTER /* РеоргПолное */
          FIELD reorgsokr$ AS CHARACTER /* РеоргСокр */
          FIELD reorgfirm$ AS CHARACTER /* РеоргФирм */
          FIELD reorgwazykrf$ AS CHARACTER /* РеоргЯзыкРФ */
          FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
          FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
          FIELD statusdata$ AS DATE /* СтатусДата */
          FIELD statuspredpr$ AS CHARACTER /* СтатусПредпр */
          FIELD struktorg$ AS CHARACTER /* СтруктОрг */
          FIELD subw%ekt$ AS CHARACTER /* Субъект */
          FIELD tipkl$ AS CHARACTER /* ТипКл */
          FIELD unikkodadresa$ AS CHARACTER /* УникКодАдреса */
          FIELD unk$ AS DECIMAL /* УНК */
          FIELD unkg$ AS INT64 /* УНКг */
          FIELD ustavkap$ AS CHARACTER /* УставКап */
          FIELD uwcdok$ AS CHARACTER /* УчДок */
          FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
          FIELD uwcdokdata$ AS DATE /* УчДокДата */
          FIELD uwcredorg$ AS CHARACTER /* УчредОрг */
          FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
          FIELD fioruk$ AS CHARACTER /* ФИОрук */
          FIELD formsobs$ AS CHARACTER /* ФормСобс */
          FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
          FIELD wekonsekt$ AS CHARACTER /* ЭконСект */
          FIELD Address1Indeks AS INT64 /* Address1Indeks */
          FIELD BirthDay AS DATE /* BirthDay */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD branch-id AS CHARACTER /* branch-id */
          FIELD branch-list AS CHARACTER /* branch-list */
          FIELD brand-name AS CHARACTER /* brand-name */
          FIELD CMSCUR AS DECIMAL /* CMSCUR */
          FIELD contr_group AS CHARACTER /* contr_group */
          FIELD cont_type AS CHARACTER /* cont_type */
          FIELD country-id2 AS CHARACTER /* country-id2 */
          FIELD country-id3 AS CHARACTER /* country-id3 */
          FIELD CountryCode AS CHARACTER /* CountryCode */
          FIELD CRSCM AS CHARACTER /* CRSCM */
          FIELD date-export AS CHARACTER /* date-export */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD document AS CHARACTER /* document */
          FIELD document-id AS CHARACTER /* document-id */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD e-mail AS CHARACTER /* e-mail */
          FIELD engl-name AS CHARACTER /* engl-name */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD holding-id AS CHARACTER /* holding-id */
          FIELD iey AS CHARACTER /* iey */
          FIELD IndCode AS CHARACTER /* IndCode */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD issue AS CHARACTER /* issue */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD lic-sec AS CHARACTER /* lic-sec */
          FIELD LocCustType AS CHARACTER /* LocCustType */
          FIELD mess AS CHARACTER /* mess */
          FIELD NACE AS CHARACTER /* NACE */
          FIELD Netting AS LOGICAL /* Netting */
          FIELD NoExport AS LOGICAL /* NoExport */
          FIELD num_contr AS INT64 /* num_contr */
          FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
          FIELD Prim-ID AS CHARACTER /* Prim-ID */
          FIELD RegDate AS CHARACTER /* RegDate */
          FIELD RegNum AS CHARACTER /* RegNum */
          FIELD RegPlace AS CHARACTER /* RegPlace */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD Soato AS CHARACTER /* Soato */
          FIELD SphereID AS CHARACTER /* SphereID */
          FIELD tel AS CHARACTER /* tel */
          FIELD Telex AS CHARACTER /* Telex */
          FIELD Visa AS CHARACTER /* Visa */
          FIELD VisaNum AS CHARACTER /* VisaNum */
          FIELD VisaType AS CHARACTER /* VisaType */
          FIELD lat_name AS CHARACTER /* lat_name */
          FIELD wembnazv$ AS CHARACTER /* ЭмбНазв */
          FIELD country-id4 AS CHARACTER /* country-id4 */
          FIELD website AS CHARACTER /* website */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-cust-corp" "" }
          
      END-FIELDS.
      TABLE: tt-p-cust-adr T "?" NO-UNDO bisquit cust-ident
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
          {ln-tthdl.i "tt-p-cust-adr" "p-cust-adr" }
          
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
         HEIGHT             = 25.93
         WIDTH              = 103
         MAX-HEIGHT         = 25.93
         MAX-WIDTH          = 103
         VIRTUAL-HEIGHT     = 25.93
         VIRTUAL-WIDTH      = 103
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
/* SETTINGS FOR FILL-IN tt-cust-corp.addr-of-low[1] IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.addr-of-low[2] IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-HELP                                           */
/* SETTINGS FOR FILL-IN tt-cust-corp.bank-code IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.bank-code-type IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.benacct IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.corr-acct IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.country-id IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.cust-id IN FRAME fMain
   3                                                                    */
ASSIGN 
       tt-cust-corp.cust-id:READ-ONLY IN FRAME fMain        = TRUE.
/* SETTINGS FOR FILL-IN tt-cust-corp.cust-stat IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.date-in IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.date-out IN FRAME fMain
   2 3                                                                  */
/* SETTINGS FOR FILL-IN tt-cust-corp.fax IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.formsobs$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.inn IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.kodreg$ IN FRAME fMain
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-cust-corp.kodreggni$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-corp.kpp$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN mBankClient IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mFormSobs IN FRAME fMain
   3                                                                    */
/* SETTINGS FOR FILL-IN tt-cust-corp.name-corp IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.name-short IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.ogrn$ IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okonx IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okpo IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okvwed$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN tt-cust-corp.subw%ekt$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT                                           */
/* SETTINGS FOR FILL-IN tt-cust-corp.tax-insp IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.tel IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.unk$ IN FRAME fMain
   1 2 3                                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-cust-corp"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-cust-corp.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.bank-code IN FRAME fMain /* Идентиф. банка */
DO:
   IF iMode EQ {&MOD_VIEW} THEN DO:
     IF SELF:SCREEN-VALUE NE "" THEN DO:
        mTempVal = GetNeedBankCode(tt-cust-corp.bank-code-type,
                                   SELF:SCREEN-VALUE,
                                   "bank-id").
        RUN banks#.p (mTempVal,iLevel).
     END.
   END.
   ELSE DO:
      IF tt-cust-corp.bank-code-type:SCREEN-VALUE NE "" AND
         tt-cust-corp.bank-code-type:SCREEN-VALUE NE "ИНН"
      THEN DO:
         ASSIGN 
            tt-cust-corp.bank-code-type.
         DO TRANSACTION:
            RUN bankscod.p (INPUT-OUTPUT tt-cust-corp.bank-code-type, iLevel + 1).
         END.
      END.
      ELSE
      DO TRANSACTION:
         RUN banks.p (iLevel + 1).
      END.

      IF     LASTKEY EQ 10 
         AND pick-value NE ? 
      THEN DO:
         DISPLAY 
            ENTRY(2,pick-value) WHEN NUM-ENTRIES(pick-value) GT 1 @ tt-cust-corp.bank-code
            ENTRY(1,pick-value) @ tt-cust-corp.bank-code-type
         WITH FRAME {&FRAME-NAME}.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.bank-code IN FRAME fMain /* Идентиф. банка */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE flag-error AS INT64 NO-UNDO.

   IF     tt-cust-corp.bank-code-type:SCREEN-VALUE NE ""
      AND tt-cust-corp.bank-code:SCREEN-VALUE NE "" THEN
   DO:
      IF tt-cust-corp.bank-code:SCREEN-VALUE NE "ИНН" THEN
      DO:
         {getbank.i "banks" "INPUT tt-cust-corp.bank-code" "INPUT tt-cust-corp.bank-code-type"}
      END.
      ELSE
      DO:
         FIND FIRST cust-ident WHERE cust-ident.cust-cat       EQ "Б"
                               AND   cust-ident.cust-code-type EQ "ИНН"
                               AND   cust-ident.cust-code      EQ tt-cust-corp.bank-code:SCREEN-VALUE
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
            FIND FIRST banks WHERE banks.bank-id EQ cust-ident.cust-id
               NO-LOCK NO-ERROR.
      END.

      IF NOT AVAILABLE(banks) THEN
      DO:
         MESSAGE COLOR ERROR
            "Банк с кодом идентификатора ~"" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "~"" SKIP
            "и идентификатором ~"" + tt-cust-corp.bank-code:SCREEN-VALUE + "~" не существует " SKIP
         VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
                                                 /* проверить возможность работы с банком        */
      RUN Check-Bank IN h_op (BUFFER banks, YES, OUTPUT flag-error).
      IF NOT flag-error EQ 0 THEN
         RETURN NO-APPLY {&RET-ERROR}.

      FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK NO-ERROR.
      IF AVAILABLE banks-corr THEN
         tt-cust-corp.corr-acct = banks-corr.corr-acct.

      DISPLAY 
         tt-cust-corp.corr-acct 
      WITH FRAME {&FRAME-NAME}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.unk$ IN FRAME fMain /* УНК */
DO:

   DEF VAR vOk AS LOGICAL  NO-UNDO. /* Результат переопределения объекта */

   IF iMode EQ {&MOD_VIEW} THEN 
      RUN xview.p    ("cust-corp", tt-cust-corp.cust-id).  
   ELSE IF  iMode EQ {&MOD_EDIT}
         OR iMode EQ {&MOD_ADD} THEN 
   DO:
      DO TRANSACTION:
         RUN browseld.p ("cust-corp", "", "", "", iLevel + 1).
      END.
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
      THEN DO:
                        /* Переключаем режим с ввода на редактирование. */
         ASSIGN
            iMode       = {&MOD_EDIT}
            iSurrogate  = pick-value
         .

         /* Переполучение объекта для случая вызова из транзакции */
         REGET:
         DO ON ERROR  UNDO, LEAVE
            ON ENDKEY UNDO, LEAVE: 

            IF mOnlyForm THEN
            DO:                     
               RUN UnbindInstance   IN h_data (mInstance, 
                                               iMode).

               RUN GetBaseAttrs     IN h_data ("cust-corp", 
                                               iSurrogate, 
                                               mInstance, 
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN GetExtAttrs      IN h_data ("cust-corp", 
                                               iSurrogate, 
                                               mInstance, 
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN GetAggrInstances IN h_data (mInstance,
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN SetInstanceProp IN h_data (mInstance,
                                              "__template",
                                              "NO",
                                              OUTPUT vOk).
               IF NOT vOk THEN
                  RUN DelEnptyInstance IN h_data (mInstance).
   
               RUN PrepareInstanceEx IN h_data (?, ?, ?, ?, ?).
            END.
         END.

         RUN MainAction.
      END.
   END.

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.bank-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.bank-code-type IN FRAME fMain /* Код */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN 
      RUN shifr.p("КодБанка",SELF:SCREEN-VALUE,iLevel + 1).
   ELSE DO:
      DO TRANSACTION:
         RUN pclass.p ("КодБанка", "КодБанка", "КОДЫ БАНКОВ", iLevel + 1).
      END.
      IF     LASTKEY EQ 10 
         AND pick-value NE ? THEN 
         DISPLAY 
            pick-value @ tt-cust-corp.bank-code-type 
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.benacct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.benacct TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.benacct IN FRAME fMain /* Расчетный счет */
DO:
   {&BEG_BT_LEAVE}
   IF     tt-cust-corp.bank-code-type:SCREEN-VALUE NE ""
      AND tt-cust-corp.bank-code:SCREEN-VALUE NE "" THEN
   DO:
      RUN key-tst.p(tt-cust-corp.benacct:SCREEN-VALUE,
                    tt-cust-corp.bank-code:SCREEN-VALUE,
                    OUTPUT mAcctKey).
      IF     mAcctKey NE ? 
         AND mAcctKey NE SUBSTR(tt-cust-corp.benacct:SCREEN-VALUE, 9, 1) THEN
      DO:
         MESSAGE "Внимание: Неверный ключ!" 
            VIEW-AS ALERT-BOX ERROR.
/*          RETURN NO-APPLY {&RET-ERROR}. */
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.corr-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.corr-acct TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.corr-acct IN FRAME fMain /* Корсчет в РКЦ */
DO:
   IF iMode NE {&MOD_VIEW} THEN DO:
      IF     tt-cust-corp.bank-code-type:SCREEN-VALUE NE ""
         AND tt-cust-corp.bank-code:SCREEN-VALUE      NE "" 
      THEN DO:
         IF tt-cust-corp.bank-code-type:SCREEN-VALUE NE "ИНН" THEN
            mTempVal = GetValueByQuery("banks-code",
                                       "bank-id",
                                       "     banks-code.bank-code-type  EQ '" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "'" +
                                       " AND banks-code.bank-code       EQ '" + tt-cust-corp.bank-code:SCREEN-VALUE       + "'"
                                       ).
         ELSE
            mTempVal = GetValueByQuery("cust-ident",
                                       "cust-id",
                                       "     cust-ident.cust-cat        EQ 'Б' " +
                                       " AND cust-ident.cust-code-type  EQ '" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "'" +
                                       " AND cust-ident.cust-code       EQ '" + tt-cust-corp.bank-code:SCREEN-VALUE       + "'"
                                       ).
         IF mTempVal NE ? THEN
            mTempVal = GetValueByQuery("banks",
                                       "bank-id",
                                       "banks.bank-id  EQ " + mTempVal
                                       ).
         IF mTempVal NE ? THEN
            RUN  banksch2.p (INT64(mTempVal), iLevel + 1).
         IF     LASTKEY EQ 10 
            AND pick-value NE ? THEN
            DISPLAY 
               pick-value @ tt-cust-corp.corr-acct
            WITH FRAME {&FRAME-NAME}.
      END.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.cust-stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.cust-stat TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.cust-stat IN FRAME fMain /* Статус */
DO:
   IF iMode EQ {&Mod_View} THEN DO:
      IF {assigned SELF:SCREEN-VALUE} THEN 
         RUN shifr.p("КодПредп",GetCodeVal("КодПредп",SELF:SCREEN-VALUE),iLevel + 1).
   END.
   ELSE DO:
      DO TRANSACTION:
         RUN codelay.p ("КодПредп", "КодПредп", "ВИДЫ ПРЕДПРИЯТИЙ", iLevel + 1).
      END.
      IF     LASTKEY EQ 10 
         AND pick-value NE ? THEN 
         DISPLAY 
            GetCode("КодПредп",pick-value) @ tt-cust-corp.cust-stat 
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.cust-stat TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.cust-stat IN FRAME fMain /* Статус */
DO:
   {&BEG_BT_LEAVE}
   IF SELF:SCREEN-VALUE NE "" THEN
   DO:
      IF GetCodeVal("КодПредп", SELF:SCREEN-VALUE) EQ ? THEN DO:
         RUN Fill-SysMes IN h_tmess ("","comm01","", "%s=КодПредп" + "%s=" + SELF:SCREEN-VALUE). 
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.date-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.date-in TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.date-in IN FRAME fMain /* Дата заведения */
,tt-cust-corp.date-out
DO:
   {&BEG_BT_LEAVE}
   /* проверка вводимой даты, чтобы не возникало прогресовой ошибки */
   DEFINE VARIABLE vdtTmpDt AS DATE NO-UNDO.
   vdtTmpDt = DATE(SELF:SCREEN-VALUE) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) 
         VIEW-AS ALERT-BOX.
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   /* проверка соответствия дат открытия/закрытия */
   IF INPUT tt-cust-corp.date-out LT INPUT tt-cust-corp.date-in THEN
   DO:
      MESSAGE "Дата заведения клиента должна быть меньше даты закрытия"
         VIEW-AS ALERT-BOX.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.inn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.inn TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.inn IN FRAME fMain /* ИНН */
DO:
   DEFINE VAR i       AS INTEGER NO-UNDO.
   DEFINE VAR vLen    AS INTEGER NO-UNDO.
   DEFINE VAR isAlpha AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vFl AS LOGICAL NO-UNDO.
   vFl = YES.
   {&BEG_BT_LEAVE}
   IF SELF:SCREEN-VALUE NE "" THEN
   DO:
     vLen = LENGTH(SELF:SCREEN-VALUE).
     DO i = 1 TO vLen:
      IF ASC(SUBSTRING(SELF:SCREEN-VALUE,i,1)) LT 48 OR 
         ASC(SUBSTRING(SELF:SCREEN-VALUE,i,1)) GT 57 THEN isAlpha = YES.
      ELSE isAlpha = NO.
      IF isAlpha THEN LEAVE.
     END.
     IF NOT isAlpha THEN
     DO:
        IF INT64(SELF:SCREEN-VALUE) EQ 0 THEN
        DO:
          MESSAGE 'В ИНН запрещены только нули.' VIEW-AS ALERT-BOX.
          RETURN NO-APPLY {&RET-ERROR}.
        END.
     END.
     IF (vLen NE 5) AND (vLen NE 10) AND (vLen NE 12) THEN
     DO:
       MESSAGE 'В ИНН должно быть 5, 10 или 12 цифр, введено ' + TRIM(STRING(vLen)) + '.' 
         VIEW-AS ALERT-BOX TITLE " Внимание ".
       RETURN NO-APPLY {&RET-ERROR}.
     END.
     IF vLen NE 5 AND
       NOT fValidInnSignature(SELF:SCREEN-VALUE, mTempVal) THEN DO:
       IF vLen = 10 THEN 
           MESSAGE ("Последняя цифра ИНН (ключ) должна быть: ~"" + mTempVal + "~"")
             SKIP "OK - Исправить, Cancel - Пропустить"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " Внимание " UPDATE vfl.
       IF vLen = 12 THEN 
           MESSAGE ("Последние 2 цифры ИНН (ключ) должны быть: ~"" + mTempVal + "~"")
             SKIP "OK - Исправить, Cancel - Пропустить"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " Внимание " UPDATE vfl.
       IF vfl OR vfl EQ ? THEN 
              RETURN NO-APPLY {&RET-ERROR}.
     END.
     mTempVal = GetValueByQuery("cust-corp",
                    "name-corp",
                    "     cust-corp.inn        eq '" + SELF:SCREEN-VALUE + "'" +
                    " AND cust-corp.country-id eq '" + tt-cust-corp.country-id:SCREEN-VALUE + "'" +
                    " AND RECID(cust-corp)  NE " + (IF tt-cust-corp.local__rowid NE ? 
                    THEN STRING(Rowid2Recid('cust-corp',tt-cust-corp.local__rowid)) 
                    ELSE "0")
                  ).
     IF mTempVal NE ? THEN DO:
       MESSAGE "Запись с такими реквизитами уже существует в таблице:" 
                SUBSTR(mTempVal, 1, 80) "!" 
          SKIP "OK - Исправить, Cancel - Пропустить"
       VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " Внимание " UPDATE vfl.
       IF vfl OR vfl EQ ? THEN 
            RETURN NO-APPLY {&RET-ERROR}.
     END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.kodreggni$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
,tt-cust-corp.addr-of-low[1]
DO:
   RETURN NO-APPLY {&RET-ERROR}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON ENTRY OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   &SCOPED-DEFINE KodRegGniHelpStr "F1 Основной адрес"
   IF NUM-ENTRIES(mHelpStrAdd,{&KodRegGniHelpStr}) = 0 THEN DO:
      SetHelpStrAdd(TRIM(mHelpStrAdd + "│" + {&KodRegGniHelpStr},"│")).
      RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
,tt-cust-corp.addr-of-low[1]
DO:
   DEF VAR vHInst    AS HANDLE NO-UNDO. /* Указатель на ID пользователя. */
   DEF VAR vTemplate AS LOG    NO-UNDO. /* Значение __template для cust-ident.*/
   DEF VAR vOk       AS LOG    NO-UNDO. /* Флаг ошибки. */

   IF iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      FIND FIRST bcident WHERE bcident.cust-cat       = "Ю"
                           AND bcident.cust-id        = tt-cust-corp.cust-id
                           AND bcident.cust-code      = mUniqCodAdr
                           AND bcident.cust-code-type = mAdrType
      NO-LOCK NO-ERROR.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat~001cust-id~001cust-code-type",
                     "Ю" + CHR(1) + STRING(tt-cust-corp.cust-id) + CHR(1) + mAdrType,
                     "cust-cat~001cust-id~001cust-code-type",
                     iLevel + 1).
      IF LASTKEY EQ 10
      THEN DO:
         FIND FIRST bcident WHERE
                    bcident.cust-cat       EQ "Ю"
                AND bcident.cust-id        EQ tt-cust-corp.cust-id
                AND bcident.cust-code-type EQ ENTRY(1,pick-value)
                AND bcident.cust-code      EQ ENTRY(2,pick-value)
         NO-LOCK NO-ERROR.
      END.
      IF AVAIL bcident THEN DO:
         {cust-adr.obj 
            &addr-to-vars     = YES
            &addr-to-vars-gni = YES
            &tablefield       = "TRIM(bcident.issue)" 
            &fieldgni         = "GetXattrValue('cust-ident',
                                                bcident.cust-code-type + ',' + 
                                                bcident.cust-code      + ',' + 
                                                STRING(bcident.cust-type-num),
                                                'КодыАдреса'
                                                )"
         }
         ASSIGN
            vAdrIndInt                 :SCREEN-VALUE = STRING(vAdrIndInt,"999999")           
            vOblChar                   :SCREEN-VALUE = vOblChar    
            vGorChar                   :SCREEN-VALUE = vGorChar  
            vPunktChar                 :SCREEN-VALUE = vPunktChar
            vUlChar                    :SCREEN-VALUE = vUlChar   
            vDomChar                   :SCREEN-VALUE = vDomChar  
            vKorpChar                  :SCREEN-VALUE = vKorpChar 
            vKvChar                    :SCREEN-VALUE = vKvChar
            vStrChar                   :SCREEN-VALUE = vStrChar
            tt-cust-corp.addr-of-low[1]:SCREEN-VALUE = bcident.issue
            tt-cust-corp.kodreggni$    :SCREEN-VALUE = GetXattrValue('cust-ident',
                                                                     bcident.cust-code-type + ',' + 
                                                                     bcident.cust-code      + ',' + 
                                                                     STRING(bcident.cust-type-num),
                                                                     'КодРегГНИ'
                                                                     )
            vAdrCntry                                = GetXattrValue('cust-ident',
                                                                     bcident.cust-code-type + ',' + 
                                                                     bcident.cust-code      + ',' + 
                                                                     STRING(bcident.cust-type-num),
                                                                     'country-id'
                                                                     ) 
            tt-cust-corp.kodreg$       :SCREEN-VALUE = GetXattrValue('cust-ident',
                                                                      bcident.cust-code-type + ',' + 
                                                                      bcident.cust-code      + ',' + 
                                                                      STRING(bcident.cust-type-num),
                                                                      'КодРег'
                                                                      )  
            mUniqCodAdr                               = bcident.cust-code                                                                                                                                           
         .
      END.
   END.
   ELSE IF iMode EQ {&MOD_ADD}
   THEN DO:
      /* Получение указателя на датасет. */
      ASSIGN
         vHInst      = WIDGET-HANDLE (GetInstanceProp2 (mInstance,"p-cust-adr"))
         vTemplate   = LOGICAL (GetInstanceProp2 (vHInst,"__template"))
      .
      RUN SetInstanceProp (vHInst,"cust-cat"      ,"Ю"     ,OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHInst,"cust-code-type",mAdrType,OUTPUT vOk) NO-ERROR.
      RUN formld.p (STRING (vHInst) + "~003p-cust-adr",
                    "",
                    "Ю~003~003" + mAdrType,
                    {&MOD_ADD},
                    iLevel + 6
                  ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
      THEN DO:
         TEMP-TABLE tt-p-cust-adr:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (vHInst:DEFAULT-BUFFER-HANDLE).
         {cust-adr.obj 
            &addr-to-vars     = YES
            &addr-to-vars-gni = YES
            &tablefield       = "TRIM(tt-p-cust-adr.issue)" 
            &fieldgni         = "tt-p-cust-adr.kodyadresa$"
         }
         ASSIGN
            vAdrIndInt                 :SCREEN-VALUE = STRING(vAdrIndInt,"999999")           
            vOblChar                   :SCREEN-VALUE = vOblChar    
            vGorChar                   :SCREEN-VALUE = vGorChar  
            vPunktChar                 :SCREEN-VALUE = vPunktChar
            vUlChar                    :SCREEN-VALUE = vUlChar   
            vDomChar                   :SCREEN-VALUE = vDomChar  
            vKorpChar                  :SCREEN-VALUE = vKorpChar 
            vKvChar                    :SCREEN-VALUE = vKvChar
            vStrChar                   :SCREEN-VALUE = vStrChar
            tt-cust-corp.addr-of-low[1]:SCREEN-VALUE = tt-p-cust-adr.issue
            tt-cust-corp.kodreggni$    :SCREEN-VALUE = tt-p-cust-adr.kodreggni$
            vAdrCntry                                = tt-p-cust-adr.country-id
            tt-cust-corp.kodreg$       :SCREEN-VALUE = tt-p-cust-adr.kodreg$
            mUniqCodAdr                              = tt-p-cust-adr.cust-code                                                                                                                                           
         .
      END.
      ELSE RUN SetInstanceProp (vHInst, "__template", STRING (vTemplate), OUTPUT vOk) NO-ERROR.
   END.
   ELSE IF iMode = {&MOD_VIEW} THEN DO:
      FOR FIRST bcident WHERE bcident.cust-cat       = "Ю"
                          AND bcident.cust-id        = tt-cust-corp.cust-id
                          AND bcident.cust-code      = mUniqCodAdr
                          AND bcident.cust-code-type = mAdrType
      NO-LOCK:
         RUN formld.p ("p-cust-adr",
                       GetSurrogateBuffer("cust-ident",(BUFFER bcident:HANDLE)),
                       "",
                       {&MOD_VIEW},
                       iLevel + 1
                      ).
      END.
   END.

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON CLEAR OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON DELETE-CHARACTER OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON BACKSPACE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   SetHelpStrAdd(TRIM(REPLACE(mHelpStrAdd,{&KodRegGniHelpStr},""),"│")).
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
END.
&ANALYZE-RESUME
/* _UIB-CODE-BLOCK-END */

&Scoped-define SELF-NAME mBankClient
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBankClient TERMINAL-SIMULATION
ON F1 OF mBankClient IN FRAME fMain /* Отношение к банку */
DO:
   IF     iMode EQ {&MOD_ADD} 
      OR  iMode EQ {&MOD_EDIT}
    THEN SELF:SCREEN-VALUE = IF SELF:SCREEN-VALUE EQ ENTRY (1, SELF:FORMAT, "/")
                               THEN ENTRY (2, SELF:FORMAT, "/")
                               ELSE ENTRY (1, SELF:FORMAT, "/").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBankClient TERMINAL-SIMULATION
ON LEAVE OF mBankClient IN FRAME fMain /* Отношение к банку */
DO:
   {&BEG_BT_LEAVE}
   IF iMode EQ {&MOD_EDIT}
      AND SELF:SCREEN-VALUE NE ENTRY (1, SELF:FORMAT, "/")
   THEN DO:
      IF GetBufferValue (
         "acct",
         "WHERE " +
         "     acct.cust-cat  EQ 'ю' " +
         "AND  acct.cust-id   EQ '" + STRING (tt-cust-corp.cust-id) + "'",
         "acct") NE ?
      THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "0",
            "У субъекта есть счета.~nНельзя снять признак ""клиент""."
         ).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.name-short
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.name-short TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.name-short IN FRAME fMain /* Краткое наимен */
DO:
   IF iMode EQ {&MOD_VIEW}  THEN
      RUN xview.p("cust-corp",tt-cust-corp.cust-id).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.ogrn$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.ogrn$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.ogrn$ IN FRAME fMain /* ogrn$ */
DO:
   DEFINE VARIABLE mOGRN2   AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE mMessage AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE  mYesNo  AS LOGICAL   INITIAL YES NO-UNDO.
   DEFINE VARIABLE mNumb    AS INT64               NO-UNDO.
   DEFINE VARIABLE mSurr    AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE mIsReady AS LOGICAL               NO-UNDO.
   DEFINE VARIABLE mPredPr  AS CHARACTER             NO-UNDO.

   DEFINE VARIABLE mItem AS INT64 NO-UNDO.
   {&BEG_BT_LEAVE}

   IF     tt-cust-corp.ogrn$:SCREEN-VALUE NE "" 
      AND tt-cust-corp.ogrn$:SCREEN-VALUE NE "?" 
      THEN
   DO:
      mIsReady = NO.

      IF iMode EQ {&MOD_EDIT} THEN
      DO:
         mPredPr = GetXAttrValueEx(iClass, STRING(tt-cust-corp.cust-id), "Предпр", "").
      END.

      IF iMode   NE {&MOD_EDIT} 
      OR mPredPr NE "Предпр"
      THEN
      DO:
         RUN chk-ogrn.p(tt-cust-corp.ogrn$:SCREEN-VALUE, YES, OUTPUT mOGRN2) .

         IF tt-cust-corp.ogrn$:SCREEN-VALUE EQ mOGRN2 THEN
            ASSIGN
               mMessage = "Прошло ключевание ОГРН для юридического лица"
               mIsReady = YES
            .
      END.

      IF  NOT mIsReady
      AND NOT (    iMode NE {&MOD_EDIT}
               AND {assigned mOGRN2}
               AND TRIM(mOGRN2,"1234567890") = "")
      AND (   iMode   NE {&MOD_EDIT}
           OR mPredPr EQ "Предпр")
      THEN
      DO:
         RUN chk-ogrn.p(tt-cust-corp.ogrn$:SCREEN-VALUE, NO, OUTPUT mOGRN2) .

         IF tt-cust-corp.ogrn$:SCREEN-VALUE EQ mOGRN2 THEN
            ASSIGN
               mMessage = "Прошло ключевание ОГРН для предпринимателя"
               mIsReady = YES
            .
      END.

      IF NOT mIsReady THEN
      DO:

         IF mOGRN2 EQ "** не прошло **" OR (iMode NE {&MOD_EDIT} AND mOGRN2 BEGINS "** ") THEN
            mMessage = "Ошибка в ключевании ОГРН~n   ОГРН = " + tt-cust-corp.ogrn$:SCREEN-VALUE.
         ELSE IF mOGRN2 BEGINS "** " THEN
            mMessage = "Ошибка в ключевании ОГРН~n"  + SUBSTRING(mOGRN2,4).
         ELSE
            mMessage = "Ошибка в ключевании ОГРН~n Должно быть " + mOGRN2 + " вместо " + tt-cust-corp.ogrn$:SCREEN-VALUE.

         MESSAGE 
            mMessage + "~nБудете исправлять?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.

         IF mYesNo NE NO THEN
            RETURN NO-APPLY {&RET-ERROR}.
      END.
      ELSE
         IF iMode NE {&MOD_EDIT} THEN DO:
            IF mLastOkOGRN <> tt-cust-corp.ogrn$:SCREEN-VALUE
            THEN MESSAGE mMessage VIEW-AS ALERT-BOX.
            mLastOkOGRN = tt-cust-corp.ogrn$:SCREEN-VALUE.
         END.
 
      IS_EXIST:
      DO mItem = 1 TO 2:

         FOR FIRST signs NO-LOCK WHERE signs.file-name EQ ENTRY(mItem, "banks,cust-corp")
                           AND signs.code      EQ "ОГРН"
                           AND signs.code-val  EQ tt-cust-corp.ogrn$:SCREEN-VALUE
                           AND (   signs.file-name NE "cust-corp"
                                OR iMode           EQ {&MOD_ADD}
                                OR (    iMode           EQ {&MOD_EDIT}
                                    AND signs.surrogate NE STRING(tt-cust-corp.cust-id))
                               ):
      
            MESSAGE 
               "Запись ОГРН=" + tt-cust-corp.ogrn$:SCREEN-VALUE + " уже существует~nБудете исправлять?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.

            IF mYesNo NE NO THEN
               RETURN NO-APPLY {&RET-ERROR}.
            ELSE
               LEAVE IS_EXIST.
         END.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.unk$ IN FRAME fMain /* УНК */
DO:
{&BEG_BT_LEAVE}
      RUN ChkUpdUnk$ IN h_cust("cust-corp",
                               STRING(tt-cust-corp.cust-id),
                               tt-cust-corp.unk$:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "0",
            RETURN-VALUE
         ).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
{&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/*&Scoped-define SELF-NAME tt-cust-corp.okonx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.okonx TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.okonx IN FRAME fMain /* ОКОНХ */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN 
      RUN shifr.p("ОКОНХ",SELF:SCREEN-VALUE,iLevel + 1).
   ELSE DO:
      DO TRANSACTION:
         RUN pclass.p ("ОКОНХ", "ОКОНХ", "КОДЫ ОКОНХ", iLevel + 1).
      END.
      IF     LASTKEY EQ 10 
         AND pick-value NE ? THEN 
         DISPLAY 
            pick-value @ tt-cust-corp.okonx
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.okvwed$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.okvwed$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.okvwed$ IN FRAME fMain /* ОКВЭД */
DO:
   DEF VAR mI      AS INT64  NO-UNDO.
   DEF VAR vOldVal AS CHAR NO-UNDO.
   DEF VAR vOldFmt AS CHAR NO-UNDO.
   
   IF iMode EQ {&MOD_VIEW} THEN DO:
      mTempVal = "".
      DO mI = 1 TO NUM-ENTRIES(SELF:SCREEN-VALUE):
         mTempVal = TRIM(mTempVal + 
                    STRING(STRING(ENTRY(mI,SELF:SCREEN-VALUE),"X(12)") +  
                           "- "                                        + 
                           GetCodeName(GetXAttrEx('cust-corp',
                                                  'ОКВЭД',
                                                  'Domain-Code'), 
                                       ENTRY(mI,SELF:SCREEN-VALUE)
                                       ),"X(400)"
                          )) + "~n".
      END.

      vOldVal = tt-cust-corp.okvwed$:SCREEN-VALUE.
      vOldFmt = tt-cust-corp.okvwed$:FORMAT.
      _view_okved:
      DO ON ERROR  UNDO _view_okved, RETRY _view_okved
         ON ENDKEY UNDO _view_okved, RETRY _view_okved:
         IF RETRY THEN DO:
            tt-cust-corp.okvwed$:FORMAT = vOldFmt.
            tt-cust-corp.okvwed$:SCREEN-VALUE = vOldVal.
            LEAVE _view_okved.
         END.
         tt-cust-corp.okvwed$:FORMAT = "x(32000)".
         tt-cust-corp.okvwed$:SCREEN-VALUE = mTempVal.
         RUN extedit (tt-cust-corp.okvwed$:HANDLE, ?).
         UNDO _view_okved, RETRY _view_okved.
      END.

   END.
   ELSE DO:
      RUN LookupXattr IN h_xclass ("cust-corp",
                                   "ОКВЭД",iLevel + 1).
      IF     LASTKEY EQ 10 
         AND pick-value NE ? 
      THEN
         SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = pick-value.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON ENTRY OF tt-cust-corp.unk$ IN FRAME fMain /* unk$ */
DO:
   IF iMode              EQ {&MOD_EDIT}
   AND tt-cust-corp.unk$ EQ ? 
   AND mFlagUnk
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "4", "Присвоить новое значение УНК?").
      IF pick-value EQ "YES" THEN
         tt-cust-corp.unk$:SCREEN-VALUE = STRING(NewUnk("cust-corp")).
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

ON F6 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DEFINE VAR vMandAddrs AS CHAR NO-UNDO.
   DEFINE BUFFER xxcode FOR code.
   IF iMode = {&MOD_EDIT}
   OR iMode = {&MOD_VIEW} THEN DO:
      FOR EACH xxcode WHERE xxcode.class = "КодАдр"
                        AND CAN-DO(xxcode.misc[2],"Ю") NO-LOCK:
         {additem.i vMandAddrs xxcode.code}
      END.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat~001cust-id~001cust-code-type",
                     "Ю" + CHR(1) + STRING(tt-cust-corp.cust-id) + CHR(1) + vMandAddrs,
                     "cust-cat~001cust-id~001cust-code-type",
                     iLevel + 1).
   END.
   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   {getflagunk.i &class-code="'cust-corp'" &flag-unk="mFlagUnk"}
   IF mFlagUnk THEN DO:
      mTempVal = GetXAttrEx("cust-corp","УНК","data-format").
      IF mTempVal NE FILL("9", LENGTH(mTempVal)) THEN DO:
         MESSAGE "Формат УНК, заданный в метасхеме д.б. ~"999...~"!"
            VIEW-AS ALERT-BOX ERROR.
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
   END.

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").

   /* Commented by KSV: Корректируем вертикальную позицию фрейма */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.
   
   RUN MainAction NO-ERROR.
   IF ERROR-STATUS:ERROR
      THEN LEAVE MAIN-BLOCK.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END. /* MAIN-BLOCK */

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
  DISPLAY mBankClient separator1 vOblChar vGorChar vPunktChar vUlChar vDomChar 
          vStrChar vKorpChar vKvChar vAdrIndInt separator2 mFormSobs 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-corp THEN 
    DISPLAY tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in 
          tt-cust-corp.date-out tt-cust-corp.cust-stat tt-cust-corp.name-corp 
          tt-cust-corp.name-short tt-cust-corp.country-id 
          tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ 
          tt-cust-corp.kodreg$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel 
          tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn 
          tt-cust-corp.ogrn$ tt-cust-corp.okvwed$ tt-cust-corp.okpo 
          /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ tt-cust-corp.formsobs$ 
          tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type 
          tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website 
          tt-cust-corp.benacct 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id 
         tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat 
         tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id 
         separator1 tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ 
         tt-cust-corp.kodreg$ vOblChar tt-cust-corp.addr-of-low[2] vGorChar 
         vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt 
         separator2 tt-cust-corp.tel tt-cust-corp.fax tt-cust-corp.tax-insp 
         tt-cust-corp.inn tt-cust-corp.ogrn$ tt-cust-corp.okvwed$ 
         tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ 
         tt-cust-corp.formsobs$ mFormSobs tt-cust-corp.subw%ekt$ 
         tt-cust-corp.bank-code-type tt-cust-corp.bank-code 
         tt-cust-corp.corr-acct tt-cust-corp.website tt-cust-corp.benacct 
         mWebsite 
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
   DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN
         tt-cust-corp.unk$     :HIDDEN = NOT mFlagUnk
         tt-cust-corp.cust-id  :HIDDEN = mFlagUnk
         mFormSobs             :HIDDEN = iMode NE {&MOD_View}
         tt-cust-corp.formsobs$:HIDDEN = iMode EQ {&MOD_View}
         &IF DEFINED( MANUAL-REMOTE ) &THEN
            tt-cust-corp.Website  :HIDDEN = iMode EQ {&MOD_View}
            mWebsite              :HIDDEN = iMode NE {&MOD_View}
            mWebsite              :SENSITIVE = NO
         &ENDIF
         
         
      .
      CASE mFrmRole:
         WHEN "addr_struct" THEN
            ASSIGN
               tt-cust-corp.addr-of-low[1]:HIDDEN = YES
               tt-cust-corp.addr-of-low[2]:HIDDEN = YES
               tt-cust-corp.kodreggni$    :HIDDEN = NO
               vAdrIndInt                 :HIDDEN = NO
               vOblChar                   :HIDDEN = NO   
               vGorChar                   :HIDDEN = NO
               vPunktChar                 :HIDDEN = NO
               vUlChar                    :HIDDEN = NO
               vDomChar                   :HIDDEN = NO
               vKorpChar                  :HIDDEN = NO
               vKvChar                    :HIDDEN = NO
               vStrChar                   :HIDDEN = NO
            .
         WHEN "addr_nostruct" THEN
            ASSIGN
               tt-cust-corp.addr-of-low[1]:HIDDEN    = NO
               tt-cust-corp.addr-of-low[2]:HIDDEN    = NO
               tt-cust-corp.kodreggni$    :HIDDEN    = YES
               vAdrIndInt                 :HIDDEN    = YES
               vOblChar                   :HIDDEN    = YES   
               vGorChar                   :HIDDEN    = YES
               vPunktChar                 :HIDDEN    = YES
               vUlChar                    :HIDDEN    = YES
               vDomChar                   :HIDDEN    = YES
               vKorpChar                  :HIDDEN    = YES
               vKvChar                    :HIDDEN    = YES
               vStrChar                   :HIDDEN    = YES
            .

      END CASE.
      IF iMode EQ {&MOD_ADD} OR
        (iMode EQ {&MOD_EDIT} AND
         NOT CAN-FIND(FIRST cust-role WHERE
                           cust-role.cust-cat   EQ "Ю"
                       AND cust-role.cust-id    EQ STRING(tt-cust-corp.cust-id)
                       AND cust-role.Class-Code EQ "ImaginClient"
                       AND cust-role.file-name  EQ "branch"                                  
                       AND cust-role.surrogate  NE shFilial
                           USE-INDEX cust-id 
                           NO-LOCK)) THEN DO:
          mBankClient:SENSITIVE = YES.

      END.
      ELSE mBankClient:SENSITIVE = NO.

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
   /* записываем значения переменных адреса в поле адреса */
   {cust-adr.obj 
      &vars-to-addr = YES
      &tablefield   = "tt-cust-corp.addr-of-low[1]"
      &AddCond      = "tt-cust-corp.addr-of-low[2] = '__FORM~001'"
   }
   
   tt-cust-corp.kodyadresa$ = fChkDopGni(tt-cust-corp.kodreggni$:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                         vCodeOblChar,
                                         vCodeGorChar,
                                         vCodePunktChar,
                                         vCodeUlChar).

   
   tt-cust-corp.unikkodadresa$ = mUniqCodAdr.

   ASSIGN
      tt-cust-corp.kodreg$.

   SetFormDefList('tt-cust-corp.kodyadresa$,tt-cust-corp.unikkodadresa$,tt-cust-corp.kodreg$').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainAction TERMINAL-SIMULATION 
PROCEDURE MainAction PRIVATE :
   
   IF     NUM-ENTRIES(iInstanceList,CHR(3)) GT 1
      AND ENTRY(2, iInstanceList,CHR(3)) GT ""
   THEN mFrmRole = ENTRY(2, iInstanceList,CHR(3)).
   ELSE mFrmRole = "addr_struct".

   IF GetCode("КодАдр", mAdrType) EQ "0"  THEN
      mFrmRole = "addr_nostruct".

   IF NUM-ENTRIES(iInstanceList,CHR(3)) GT 2 
   THEN mClient = NOT (ENTRY(3, iInstanceList,CHR(3)) EQ 'no').
   ELSE mClient = YES.
   /* Commented by KSV: Читаем данные */
   RUN GetObject IN THIS-PROCEDURE NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN ERROR "".

   &IF DEFINED( MANUAL-REMOTE ) &THEN
      mWebsite :SCREEN-VALUE IN FRAME {&FRAME-NAME} = GetXAttrValue("cust-corp",STRING(tt-cust-corp.cust-id),"website").
   &ENDIF
   IF iMode = {&MOD_EDIT} OR iMode = {&MOD_VIEW}
   THEN SetHelpStrAdd("F6 Обязательные адреса").
        
   IF mFlagUnk
   THEN tt-cust-corp.unk$:FORMAT IN FRAME {&FRAME-NAME} = mTempVal.
   ELSE tt-cust-corp.unk$:FORMAT IN FRAME {&FRAME-NAME} = FILL("9",20).

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

   RETURN.
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
   DEF BUFFER cust-ident FOR cust-ident.

   IF iMode NE {&MOD_VIEW} THEN DO:
      IF iMode EQ {&MOD_ADD} THEN
         ASSIGN
            mBankClient = mClient
            tt-cust-corp.date-in = TODAY
         .
      
      tt-cust-corp.last-date = TODAY.
      
      IF  tt-cust-corp.unk$       EQ ? 
      AND mFlagUnk
      THEN DO:
         pick-value = "YES".
         IF iMode EQ {&MOD_EDIT} THEN
            RUN Fill-SysMes IN h_tmess ("", "", "4", "Присвоить новое значение УНК?").
         IF pick-value EQ "YES" THEN
         DO:
            IF (FGetSetting("ГенУнк", "", "НЕТ") EQ "ДА" AND mBankClient) 
               OR FGetSetting("ГенУнк", "", "НЕТ") EQ "НЕТ" THEN
               tt-cust-corp.unk$ = NewUnk("cust-corp").
            IF  iMode EQ {&MOD_EDIT} THEN
               RUN Fill-SysMes IN h_tmess ("", "", "1", "Клиенту присвоено новое значение УНК - " + STRING(tt-cust-corp.unk$,mTempVal)).
         END.
      END.
   END.

            /* Для режима просмотра состояния на дату ищем актуальный
            ** на эту дату адрес прописки */
   IF mMOD_VIEW_DATE
   THEN
      FIND LAST cust-ident WHERE cust-ident.class-code      EQ "p-cust-adr"
                             AND cust-ident.cust-code-type  EQ "АдрЮр"
                             AND cust-ident.cust-cat        EQ "Ю"
                             AND cust-ident.cust-id         EQ tt-cust-corp.cust-id
                             AND cust-ident.open-date       LE gend-date
      NO-LOCK NO-ERROR.
            /* Если актуальный адрес прописки найден, используем его
            ** в форме для отображения состояния карточки на дату */
   IF AVAIL cust-ident THEN
   DO:
      /* Разбор значений адреса по экрану. */
      {cust-adr.obj 
         &addr-to-vars     = YES
         &addr-to-vars-gni = YES
         &tablefield       = "TRIM(cust-ident.issue)"
         &fieldgni         = "GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'КодыАдреса')"    
         
      }
      tt-cust-corp.kodreg$    = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'КодРег').
      tt-cust-corp.kodreggni$ = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'КодРегГНИ').
   END.
   ELSE DO:
      /* считываем поле адреса в переменные */
      {cust-adr.obj 
         &addr-to-vars     = YES
         &addr-to-vars-gni = YES
         &fieldgni         = "tt-cust-corp.kodyadresa$"    
         &tablefield       = "(IF ~{assigned TRIM(tt-cust-corp.addr-of-low[1])~} THEN TRIM(tt-cust-corp.addr-of-low[1]) ELSE TRIM(tt-cust-corp.addr-of-low[2]))"
      }
   END.

   IF iMode NE {&MOD_ADD} THEN 
      ASSIGN 
         vAdrCntry   = GetXattrValue("cust-corp",
                                     STRING(tt-cust-corp.cust-id),
                                     mAdrCntXattr)
         mUniqCodAdr = tt-cust-corp.unikkodadresa$
      .
   
   IF    iMode EQ {&MOD_VIEW}
      OR iMode EQ {&MOD_EDIT}
      THEN mBankClient = GetValueByQuery (
         "cust-role",
         "class-code",
         "        cust-role.cust-cat   EQ 'Ю'" + 
         "  AND   cust-role.cust-id    EQ '" + STRING (tt-cust-corp.cust-id) + "'" +
         "  AND   cust-role.class-code EQ 'ImaginClient'"
      ) NE ?.
            
   mTmp = GetXattrEx("cust-corp","ФормСобс","Domain-Code").
   IF mTmp EQ "" THEN
      mTmp = "ФормСобс".  
   mFormSobs = IF availCode(mTmp,tt-cust-corp.formsobs$) 
               THEN GetCodeName(mTmp,tt-cust-corp.formsobs$)
               ELSE "".    

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
   DEFINE VAR vCustId       LIKE cust-corp.cust-id NO-UNDO.
   DEFINE VAR vFNName       AS CHARACTER NO-UNDO.
   DEFINE BUFFER xcust-corp FOR cust-corp.
   DEFINE VAR vRowId        AS ROWID                  NO-UNDO.
   DEFINE VAR vXattrCode    LIKE xattr.xattr-code     NO-UNDO.
   DEFINE VAR vXattrName    LIKE xattr.name           NO-UNDO.
   DEFINE VAR vIsProgrField LIKE xattr.Progress-Field NO-UNDO.
   DEFINE VAR vMode         AS CHARACTER              NO-UNDO.
   DEFINE VAR vTmp          AS CHARACTER              NO-UNDO.
   DEFINE VAR vStatIP       AS CHARACTER              NO-UNDO.
   DEFINE BUFFER xcust    FOR cust-corp.
   
   IF LOGICAL(fGetSetting("CLI-EXPORT", "Cust-Corp", "NO")) EQ YES THEN
      RUN createxmlorg.p(tt-cust-corp.cust-id).

   vCustId = INT64(GetSurrogate("cust-corp", TO-ROWID(GetInstanceProp2(mInstance,"__rowid")))).

   /* (поднято из Дойче) простановка начального статуса */
   UpdateSigns("cust-corp",
               STRING(vCustId),
               "status",
               FGetSetting("ПовтВводКл", "НачСтатусЮ", ?),
               ?).

   UpdateSigns("cust-corp",
               STRING(vCustId),
               mAdrCntXattr,
               vAdrCntry,
               ?).

   IF CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   EQ "Ю"
                 AND cust-role.cust-id    EQ STRING(tt-cust-corp.cust-id)
                 AND cust-role.Class-Code EQ "ImaginClient"
                 AND cust-role.file-name  EQ "branch"                                  
                 AND cust-role.surrogate  NE shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) AND
   NOT CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   EQ "Ю"
                 AND cust-role.cust-id    EQ STRING(tt-cust-corp.cust-id)
                 AND cust-role.Class-Code EQ "ImaginClient"
                 AND cust-role.file-name  EQ "branch"                                  
                 AND cust-role.surrogate  EQ shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) THEN DO:

      pick-value = "no".
      RUN Fill-SysMes ("", "", "4", "Создать роль ImaginClient в текущем филиале?").
      IF pick-value EQ "yes" THEN
      RUN SetClientRole IN h_cust (
         mInstance:DEFAULT-BUFFER-HANDLE, "Ю", mBankClient)NO-ERROR.
   END.
   ELSE  /* Устанавливаем роль клиента. */
   RUN SetClientRole IN h_cust (mInstance:DEFAULT-BUFFER-HANDLE, 
                                "Ю", 
                                mBankClient) NO-ERROR.               
               
   vRowId = TO-ROWID(GetInstanceProp(mInstance,"__rowid")).
   FIND FIRST xcust WHERE ROWID(xcust) = vRowId NO-LOCK NO-ERROR.
   IF AVAIL xcust THEN DO:   
      /* Проверка основных реквизитов */ 
      RUN GetFirstUnassignedFieldManByRole IN h_cust ("cust-corp",
                                                      mInstance:DEFAULT-BUFFER-HANDLE,
                                                      "main",
                                                      OUTPUT vXattrCode,
                                                      OUTPUT vXattrName).
      IF     {assigned vXattrCode} THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0",
                  "Не заполнен основной обязательный реквизит """ + vXattrCode +
                  """ (" + vXattrName + ")."
                  ).
         RETURN ERROR.
      END.
      
   /* Проверка дополнительных реквизитов */ 
      RUN GetFirstUnassignedFieldManByRole IN h_cust ("cust-corp",
                                                      mInstance:DEFAULT-BUFFER-HANDLE,
                                                      "dp",
                                                      OUTPUT vXattrCode,
                                                      OUTPUT vXattrName).   
       IF     {assigned vXattrCode} THEN DO:                                                    
          mRetValFlag = '1'.
       END.      
   END.            

   {chk_frm_mand_adr.i
      &cust-type = "'Ю'"
      &cust-id   = "vCustId"
   }

   RUN GetCustNameFormatted IN h_cust ("Ю",STRING(tt-cust-corp.cust-id),OUTPUT vFNName).
   IF LENGTH(vFNName) > 160 THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","1","ВНИМАНИЕ! У данного клиента полное наименование может превышать 160 символов. Установите правильный алгоритм формирования наименования при помощи дополнительного реквизита ""ФорматНаим""!").
   END.

   /*проверка по стоп листам*/
   IF ChkClientByStopList ("cust-corp",
                           STRING(vCustId),
                           TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-corp),
                           TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-short),
                           tt-cust-corp.inn,
                           "",
                           "КонтрВводРед_Кл",
                           "CLNT")
   THEN DO: 
      
      {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
      RUN PrintClientSLRep. 
      {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
      vMode =   IF iMode EQ {&MOD_ADD} THEN "create" ELSE "edit".      
      IF NOT StopListAction("cust-corp",STRING(vCustId), vMode) THEN
         RETURN ERROR. 
   END.   

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   ASSIGN
      vTmp    = GetXattrValueEX("cust-corp", STRING(vCustId), "Предпр", "")
      vStatIP = FGetSetting("СтандТр","СтатусФЛЧП","")
   .
   IF FGetSetting("ОбменФМС","ФМСРед_КлЮ","") EQ "Да" AND
      (vTmp NE "" OR CAN-DO(vStatIP,tt-cust-corp.cust-stat)) 
   THEN DO:
      {cl-fmschk.i
         "'cust-corp'"
         "tt-cust-corp.cust-id"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'document-id', '')"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'document', '')"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'Document4Date_vid', '')"
         "tt-cust-corp.name-corp"
      }
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateObjectLocal TERMINAL-SIMULATION 
PROCEDURE ValidateObjectLocal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR vRecId AS RECID NO-UNDO. /* Для вызова метода CHKUPD. */
   DEF VAR vRoleKontr AS LOG  NO-UNDO. /* Нужно контролировать роли субъекта */

   vRecId = Rowid2Recid ("cust-corp",
                        TO-ROWID(GetInstanceProp(mInstance,"__rowid"))).            

   vRoleKontr = fGetSetting("КонтрКлВвод", "КонтрРоли", "") EQ "Да".
   IF     vRoleKontr 
      AND iMode EQ {&MOD_ADD}
   THEN
   DO:
      RUN chk-role.p("cust-corp", vRecId).
      IF RETURN-VALUE NE "" THEN
         RETURN ERROR RETURN-VALUE.

   END. /* IF vRoleKontr THEN */

   /* Запуск метода chkupd. */
   RUN RunClassMethod IN h_xclass (
      "cust-corp",
      "chkupd",
      "",
      "",
      "cust-req",
      STRING (vRecId)
   ) NO-ERROR.
   IF    ERROR-STATUS:ERROR
      OR RETURN-VALUE NE ""
      THEN RETURN ERROR.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='26/02/2015 09:39:09.285+04:00' */
/* $LINTUSER='KOMI' */
/* $LINTMODE='1' */
/* $LINTFILE='f-cust.p' */
/*prosignKHEHXzqFpew13KIvWSnS1g*/
