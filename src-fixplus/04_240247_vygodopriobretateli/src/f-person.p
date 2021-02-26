&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-p-cust-adr-reg NO-UNDO LIKE cust-ident
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD country-id AS CHARACTER /* country-id */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-p-cust-adr-reg" "p-cust-adr-reg" }
       .
DEFINE TEMP-TABLE tt-p-cust-adr-stay NO-UNDO LIKE cust-ident
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD country-id AS CHARACTER /* country-id */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-p-cust-adr-stay" "p-cust-adr-stay" }
       .
DEFINE TEMP-TABLE tt-p-cust-ident NO-UNDO LIKE cust-ident
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
       {ln-tthdl.i "tt-p-cust-ident" "p-cust-ident" }
       .
DEFINE TEMP-TABLE tt-person NO-UNDO LIKE person
       FIELD sotoper$ AS CHARACTER /* СотОпер */
       FIELD dolruk$ AS CHARACTER /* ДолРук */
       FIELD dom$ AS CHARACTER /* ДОМ */
       FIELD unikkodadresa$ AS CHARACTER /* УникКодАдреса */
       FIELD a-test$ AS LOGICAL /* А-тест */
       FIELD arhiv$ AS LOGICAL /* Архив */
       FIELD izobr$ AS CHARACTER /* Изобр */
       FIELD indgr$ AS CHARACTER /* ИндГр */
       FIELD iobss$ AS CHARACTER /* ИОБСС */
       FIELD iogbh$ AS CHARACTER /* ИОГБХ */
       FIELD NumberPFR AS CHARACTER /* NumberPFR */
       FIELD unstruc_regadd1 AS CHARACTER /* unstruc_regadd1 */
       FIELD abwawtik$ AS CHARACTER /* АбЯщик */
       FIELD blok$ AS LOGICAL /* Блок */
       FIELD brawcndogovor$ AS CHARACTER /* БрачнДоговор */
       FIELD viddewat$ AS CHARACTER /* ВидДеят */
       FIELD vidwzilw#wa$ AS CHARACTER /* ВидЖилья */
       FIELD vidreg$ AS CHARACTER /* ВидРег */
       FIELD vidsotr$ AS CHARACTER /* ВидСотр */
       FIELD gvk$ AS CHARACTER /* ГВК */
       FIELD datavpred$ AS CHARACTER /* ДатаВПред */
       FIELD dataokonprop$ AS DATE /* ДатаОконПроп */
       FIELD dataokpred$ AS CHARACTER /* ДатаОкПред */
       FIELD dataprop$ AS DATE /* ДатаПроп */
       FIELD datapropiski$ AS DATE /* ДатаПрописки */
       FIELD dko$ AS DECIMAL /* ДкО */
       FIELD dkowe$ AS DECIMAL /* ДкОЭ */
       FIELD dolwz$ AS CHARACTER /* Долж */
       FIELD izmdokdata$ AS DATE /* ИзмДокДата */
       FIELD izmdokprednomer$ AS CHARACTER /* ИзмДокПредНомер */
       FIELD izmfidata$ AS DATE /* ИзмФИДата */
       FIELD izmfipredimwa$ AS CHARACTER /* ИзмФИПредИмя */
       FIELD izmfipredfam$ AS CHARACTER /* ИзмФИПредФам */
       FIELD iin$ AS CHARACTER /* ИИН */
       FIELD kategfiz$ AS CHARACTER /* КатегФиз */
       FIELD klient$ AS CHARACTER /* Клиент */
       FIELD klientuf$ AS LOGICAL /* КлиентУФ */
       FIELD klprowziv$ AS CHARACTER /* КлПрожив */
       FIELD klreestr$ AS CHARACTER /* КлРеестр */
       FIELD koddokum$ AS CHARACTER /* КодДокум */
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodsubckki$ AS CHARACTER /* КодСубЦККИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD koliwzd$ AS INT64 /* КолИжд */
       FIELD korpkl$ AS CHARACTER /* КорпКл */
       FIELD kpp$ AS CHARACTER /* КПП */
       FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
       FIELD mestsvedpred$ AS CHARACTER /* МестСведПред */
       FIELD migrkart$ AS CHARACTER /* МигрКарт */
       FIELD migrpravprebpo$ AS DATE /* МигрПравПребПо */
       FIELD migrpravprebs$ AS DATE /* МигрПравПребС */
       FIELD migrprebyvpo$ AS DATE /* МигрПребывПо */
       FIELD migrprebyvs$ AS DATE /* МигрПребывС */
       FIELD migrcelw#vizita$ AS CHARACTER /* МигрЦельВизита */
       FIELD nalrez$ AS LOGICAL /* НалРез */
       FIELD nbki_stprowz$ AS CHARACTER /* НБКИ_СтПрож */
       FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
       FIELD nomerregbrak$ AS CHARACTER /* НомерРегБрак */
       FIELD nommarwsrut$ AS CHARACTER /* НомМаршрут */
       FIELD notarsogl$ AS LOGICAL /* НотарСогл */
       FIELD ogrn$ AS CHARACTER /* ОГРН */
       FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
       FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
       FIELD okvwed$ AS CHARACTER /* ОКВЭД */
       FIELD opekasogl$ AS LOGICAL /* ОпекаСогл */
       FIELD orgsvedpred$ AS CHARACTER /* ОргСведПред */
       FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
       FIELD ofwsor$ AS CHARACTER /* Офшор */
       FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
       FIELD padewzdat$ AS CHARACTER /* ПадежДат */
       FIELD padewzrod$ AS CHARACTER /* ПадежРод */
       FIELD padewztvor$ AS CHARACTER /* ПадежТвор */
       FIELD predpr$ AS LOGICAL /* Предпр */
       FIELD prim$ AS CHARACTER /* Прим */
       FIELD prim1$ AS CHARACTER /* Прим1 */
       FIELD prim2$ AS CHARACTER /* Прим2 */
       FIELD prim3$ AS CHARACTER /* Прим3 */
       FIELD prim4$ AS CHARACTER /* Прим4 */
       FIELD prim5$ AS CHARACTER /* Прим5 */
       FIELD prim6$ AS CHARACTER /* Прим6 */
       FIELD rabota$ AS CHARACTER /* Работа */
       FIELD rabotaadr$ AS CHARACTER /* РаботаАдр */
       FIELD rabotanawim$ AS DATE /* РаботаНайм */
       FIELD rabotanawimkon$ AS DATE /* РаботаНаймКон */
       FIELD rabota_viddewat$ AS CHARACTER /* Работа_ВидДеят */
       FIELD rabota_zanwatostw#$ AS CHARACTER /* Работа_Занятость */
       FIELD rabota_polowzenie$ AS CHARACTER /* Работа_Положение */
       FIELD rabota_sferadewat$ AS CHARACTER /* Работа_СфераДеят */
       FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
       FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
       FIELD svedregpred$ AS CHARACTER /* СведРегПред */
       FIELD semewinpolowz$ AS CHARACTER /* СемейнПолож */
       FIELD sempol$ AS CHARACTER /* СемПол */
       FIELD stepenw#rod$ AS CHARACTER /* СтепеньРод */
       FIELD strahnomer$ AS CHARACTER /* СтрахНомер */
       FIELD subw%ekt$ AS CHARACTER /* Субъект */
       FIELD telefon3$ AS CHARACTER /* Телефон3 */
       FIELD umer$ AS LOGICAL /* Умер */
       FIELD unkg$ AS INT64 /* УНКг */
       FIELD uwcdok$ AS CHARACTER /* УчДок */
       FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
       FIELD uwcdokdata$ AS DATE /* УчДокДата */
       FIELD formsobs$ AS CHARACTER /* ФормСобс */
       FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
       FIELD address-home AS CHARACTER /* address-home */
       FIELD Address4Dom AS CHARACTER /* Address4Dom */
       FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
       FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
       FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD BQMail AS CHARACTER /* BQMail */
       FIELD BQSms AS CHARACTER /* BQSms */
       FIELD branch-id AS CHARACTER /* branch-id */
       FIELD branch-list AS CHARACTER /* branch-list */
       FIELD contr_group AS CHARACTER /* contr_group */
       FIELD contr_type AS CHARACTER /* contr_type */
       FIELD country-id2 AS CHARACTER /* country-id2 */
       FIELD country-id3 AS CHARACTER /* country-id3 */
       FIELD date-export AS CHARACTER /* date-export */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
       FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
       FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD e-mail AS CHARACTER /* e-mail */
       FIELD engl-name AS CHARACTER /* engl-name */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD holding-id AS CHARACTER /* holding-id */
       FIELD incass AS LOGICAL /* incass */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD lat_card AS CHARACTER /* lat_card */
       FIELD lat_dop AS CHARACTER /* lat_dop */
       FIELD lat_fam AS CHARACTER /* lat_fam */
       FIELD lat_f_n AS CHARACTER /* lat_f_n */
       FIELD lat_name AS CHARACTER /* lat_name */
       FIELD lat_otch AS CHARACTER /* lat_otch */
       FIELD lat_titul AS CHARACTER /* lat_titul */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD lic-sec AS CHARACTER /* lic-sec */
       FIELD Lic_num AS CHARACTER /* Lic_num */
       FIELD mess AS CHARACTER /* mess */
       FIELD my_pict AS CHARACTER /* my_pict */
       FIELD my_sign AS CHARACTER /* my_sign */
       FIELD Netting AS LOGICAL /* Netting */
       FIELD NoExport AS LOGICAL /* NoExport */
       FIELD num_contr AS INT64 /* num_contr */
       FIELD old-person-id AS CHARACTER /* old-person-id */
       FIELD passw_card AS CHARACTER /* passw_card */
       FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
       FIELD RegNum AS CHARACTER /* RegNum */
       FIELD RetailID AS CHARACTER /* RetailID */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD Soato AS CHARACTER /* Soato */
       FIELD Svid_num AS CHARACTER /* Svid_num */
       FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
       FIELD Visa AS CHARACTER /* Visa */
       FIELD VisaNum AS CHARACTER /* VisaNum */
       FIELD VisaType AS CHARACTER /* VisaType */
       FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
       FIELD xview-photo AS CHARACTER /* xview-photo */
       FIELD xview-sign AS CHARACTER /* xview-sign */
       FIELD vidrestrukt$ AS CHARACTER /* ВидРеструкт */
       FIELD country-id4 AS CHARACTER /* country-id4 */
       FIELD kodklienta$ AS CHARACTER /* КодКлиента */
       FIELD kop$ AS INT64 /* КОП */
       FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
       FIELD otnokruwz_ipdl$ AS CHARACTER /* ОтнОкруж_ИПДЛ */
       FIELD status_ipdl$ AS CHARACTER /* Статус_ИПДЛ */
       FIELD steprodst_ipdl$ AS CHARACTER /* СтепРодст_ИПДЛ */
       FIELD SphereID AS CHARACTER /* SphereID */
       FIELD okpo AS CHARACTER /* okpo */
       FIELD unk$ AS DECIMAL /* УНК */
       FIELD kopf$ AS INT64 /* КОПФ */
       FIELD obrbs$ AS CHARACTER /* ОбрБС */
       FIELD obrgb$ AS CHARACTER /* ОбрГБ */
       FIELD okato$ AS CHARACTER /* ОКАТО */
       FIELD osnova$ AS CHARACTER /* основа */
       FIELD ulica$ AS CHARACTER /* Улица */
       FIELD fiobs$ AS CHARACTER /* ФИОБС */
       FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
       FIELD vidkli$ AS CHARACTER /* ВидКли */
       FIELD grawzd$ AS CHARACTER /* Гражд */
       FIELD fiogb$ AS CHARACTER /* ФИОГБ */
       FIELD fioruk$ AS CHARACTER /* ФИОрук */
       FIELD wekonsekt$ AS CHARACTER /* ЭконСект */
       FIELD wurstat$ AS CHARACTER /* ЮрСтат */
       FIELD wa$ AS CHARACTER /* Я */
       FIELD a-mypic AS CHARACTER /* a-mypic */
       FIELD aaaa AS CHARACTER /* aaaa */
       FIELD Address1Indeks AS INT64 /* Address1Indeks */
       FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
       FIELD Address3Street AS CHARACTER /* Address3Street */
       FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
       FIELD CountryCode AS CHARACTER /* CountryCode */
       FIELD Exam AS CHARACTER /* Exam */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD IndCode AS CHARACTER /* IndCode */
       FIELD LocCustType AS CHARACTER /* LocCustType */
       FIELD my_pic AS CHARACTER /* my_pic */
       FIELD NACE AS CHARACTER /* NACE */
       FIELD New$$ AS CHARACTER /* New */
       FIELD New1 AS CHARACTER /* New1 */
       FIELD Prim-ID AS CHARACTER /* Prim-ID */
       FIELD RegDate AS CHARACTER /* RegDate */
       FIELD vygoda$ AS CHARACTER /* Выгода */
       FIELD striska$ AS CHARACTER /* СтРиска */
       FIELD dataankety$ AS DATE /* ДатаАнкеты */
       FIELD fiosotrudnika$ AS CHARACTER /* ФИОсотрудника */
       FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
       FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
       FIELD local__id       AS INT64   /* Идентификатор записи     */
       FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
       FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
       /* Additional fields you should place here                      */
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-person" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: F-PERSON.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: om    
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
{intrface.get exch}       /* Инструменты для получения банковских реквизитов. */
{intrface.get cust}       /* Библиотека для работы с клиентами.  */
{intrface.get strng}      /* Инструменты для работы со строками  */
{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}

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
*/
&GLOBAL-DEFINE XATT-ED-ON

DEFINE VARIABLE mFlagUnk        AS LOG       NO-UNDO. /* Флаг наличия УНК. */
DEFINE VARIABLE mScreenValue    AS CHARACTER NO-UNDO. /* Значение на экране. */
DEFINE VARIABLE mIsContChecking AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vAdrCntry       AS CHARACTER NO-UNDO. /* адрес страны */
DEFINE VARIABLE mAdrType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdrCntXattr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUniqCodAdr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vHInst          AS HANDLE    NO-UNDO. /* Указатель на ID пользователя. */
DEFINE VARIABLE vTemplate       AS LOGICAL   NO-UNDO. /* Значение __template для cust-ident.*/
DEFINE VARIABLE vOk             AS LOGICAL   NO-UNDO. /* Флаг ошибки. */
DEFINE VARIABLE mCrCustRole     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNomMarsh       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTempVal        AS CHARACTER NO-UNDO.

DEF BUFFER bcident FOR cust-ident. /* Локализация буфера. */

{cust-adr.obj
   &def-vars-gni = YES
}

RUN GetTypeMainAdr IN h_cust('Ч',OUTPUT mAdrType,OUTPUT mAdrCntXattr).

/* Список реквизитов, которые могут отсутствовать в БД. */
&GLOBAL-DEFINE FlowFields  tt-person.unk$
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-person

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.document tt-person.Document4Date_vid tt-person.kodreggni$ ~
tt-person.kodreg$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.corr-acct tt-person.benacct tt-person.date-in tt-person.date-out ~
tt-person.subw%ekt$ tt-person.passw_card 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-person.person-id ~
tt-person.unk$ tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.document tt-person.Document4Date_vid tt-person.kodreggni$ ~
tt-person.kodreg$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.corr-acct tt-person.benacct tt-person.date-in tt-person.date-out ~
tt-person.subw%ekt$ tt-person.passw_card 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-person
&Scoped-define QUERY-STRING-fMain FOR EACH tt-person SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-person SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-person


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.document tt-person.Document4Date_vid tt-person.kodreggni$ ~
tt-person.kodreg$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.corr-acct tt-person.benacct tt-person.date-in tt-person.date-out ~
tt-person.subw%ekt$ tt-person.passw_card 
&Scoped-define ENABLED-TABLES tt-person
&Scoped-define FIRST-ENABLED-TABLE tt-person
&Scoped-Define ENABLED-OBJECTS mBankClient separator4 mIssue mKP separator1 ~
vOblChar vGorChar vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar ~
vAdrIndInt separator2 separator3 
&Scoped-Define DISPLAYED-FIELDS tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.document tt-person.Document4Date_vid tt-person.kodreggni$ ~
tt-person.kodreg$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.corr-acct tt-person.benacct tt-person.date-in tt-person.date-out ~
tt-person.subw%ekt$ tt-person.passw_card 
&Scoped-define DISPLAYED-TABLES tt-person
&Scoped-define FIRST-DISPLAYED-TABLE tt-person
&Scoped-Define DISPLAYED-OBJECTS mBankClient separator4 mIssue mKP ~
separator1 vOblChar vGorChar vPunktChar vUlChar vDomChar vStrChar vKorpChar ~
vKvChar vAdrIndInt separator2 separator3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 mBankClient tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.kodreggni$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.benacct tt-person.date-in tt-person.subw%ekt$ 
&Scoped-define List-2 mBankClient tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.kodreggni$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.benacct tt-person.date-in tt-person.date-out tt-person.subw%ekt$ 
&Scoped-define List-3 mBankClient tt-person.person-id tt-person.unk$ ~
tt-person.vidsotr$ tt-person.name-last tt-person.first-names ~
tt-person.birthday tt-person.gender tt-person.BirthPlace ~
tt-person.country-id tt-person.inn tt-person.tax-insp tt-person.document-id ~
tt-person.document tt-person.Document4Date_vid mIssue mKP ~
tt-person.kodreggni$ tt-person.bank-code-type tt-person.bank-code ~
tt-person.benacct tt-person.date-in tt-person.date-out tt-person.subw%ekt$ ~
tt-person.passw_card 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mBankClient AS LOGICAL FORMAT "клиент/нет" INITIAL NO 
     LABEL "Отношение к банку" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF.

DEFINE VARIABLE mIssue AS CHARACTER FORMAT "X(300)":U 
     LABEL "Выдан" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 1
     &ELSE SIZE 50 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mKP AS CHARACTER FORMAT "X(100)":U 
     LABEL "К/П" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAdrIndInt AS INT64 FORMAT "999999":U INITIAL 0 
     LABEL "Индекс" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDomChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "Дом" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vGorChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Город" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
     &ELSE SIZE 26 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKorpChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "Корп." 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKvChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "Кв." 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
     &ELSE SIZE 8 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vOblChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Район" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
     &ELSE SIZE 17 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vPunktChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "Нас.пункт" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
     &ELSE SIZE 30 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vStrChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "Стр." 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vUlChar AS CHARACTER FORMAT "X(68)":U 
     LABEL "Улица" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
     &ELSE SIZE 26 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-person SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mBankClient
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 20 COLON-ALIGNED
          &ELSE AT ROW 1 COL 20 COLON-ALIGNED &ENDIF HELP
          "Является ли субъект клиентом банка."
     tt-person.person-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 35 COLON-ALIGNED
          &ELSE AT ROW 1 COL 35 COLON-ALIGNED &ENDIF
          LABEL "Номер"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-person.unk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 33
          &ELSE AT ROW 1 COL 33 &ENDIF HELP
          ""
          LABEL "УНК" FORMAT "999999999999999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-person.vidsotr$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 63 COLON-ALIGNED
          &ELSE AT ROW 1 COL 63 COLON-ALIGNED &ENDIF HELP
          "Виды клиентов - частных лиц"
          LABEL "Тип клиента" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-person.name-last
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 18 COLON-ALIGNED
          &ELSE AT ROW 2 COL 18 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 59 BY 1
          &ELSE SIZE 59 BY 1 &ENDIF
     tt-person.first-names
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18 COLON-ALIGNED
          &ELSE AT ROW 3 COL 18 COLON-ALIGNED &ENDIF
          LABEL "Имя, отчество"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 59 BY 1
          &ELSE SIZE 59 BY 1 &ENDIF
     tt-person.birthday
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 18 COLON-ALIGNED
          &ELSE AT ROW 4 COL 18 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-person.gender
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 35 COLON-ALIGNED
          &ELSE AT ROW 4 COL 35 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-person.BirthPlace
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 63 COLON-ALIGNED
          &ELSE AT ROW 4 COL 63 COLON-ALIGNED &ENDIF HELP
          "Место рождения"
          LABEL "Место рождения" FORMAT "x(300)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-person.country-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 18 COLON-ALIGNED
          &ELSE AT ROW 5 COL 18 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
     tt-person.inn
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 35 COLON-ALIGNED
          &ELSE AT ROW 5 COL 35 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-person.tax-insp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 63 COLON-ALIGNED
          &ELSE AT ROW 5 COL 63 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
     separator4
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 1
          &ELSE AT ROW 6 COL 1 &ENDIF NO-LABEL
     tt-person.document-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 10 COLON-ALIGNED
          &ELSE AT ROW 7 COL 10 COLON-ALIGNED &ENDIF HELP
          "Код документа"
          LABEL "Документ" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 21 BY 1
          &ELSE SIZE 21 BY 1 &ENDIF
     tt-person.document
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 33 COLON-ALIGNED
          &ELSE AT ROW 7 COL 33 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "N" FORMAT "x(35)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
     tt-person.Document4Date_vid
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 56
          &ELSE AT ROW 7 COL 56 &ENDIF HELP
          "для отчета в Питерскую налоговую инспекцию"
          LABEL "Дата выдачи" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mIssue
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 9 COLON-ALIGNED
          &ELSE AT ROW 8 COL 9 COLON-ALIGNED &ENDIF HELP
          "Кем выдан"
     mKP
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 64 COLON-ALIGNED
          &ELSE AT ROW 8 COL 64 COLON-ALIGNED &ENDIF HELP
          "Код подразделения"
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 1
          &ELSE AT ROW 9 COL 1 &ENDIF NO-LABEL
     tt-person.kodreggni$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 12 COLON-ALIGNED
          &ELSE AT ROW 10 COL 12 COLON-ALIGNED &ENDIF HELP
          "Код региона ГНИ"
          LABEL "Регион ГНИ" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     vOblChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 24 COLON-ALIGNED
          &ELSE AT ROW 10 COL 24 COLON-ALIGNED &ENDIF HELP
          "Район"
     vGorChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 50 COLON-ALIGNED
          &ELSE AT ROW 10 COL 50 COLON-ALIGNED &ENDIF HELP
          "Город"
     vPunktChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 11 COLON-ALIGNED
          &ELSE AT ROW 11 COL 11 COLON-ALIGNED &ENDIF HELP
          "Населенный пункт"
     vUlChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 50 COLON-ALIGNED
          &ELSE AT ROW 11 COL 50 COLON-ALIGNED &ENDIF HELP
          "Улица"
     vDomChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 11 COLON-ALIGNED
          &ELSE AT ROW 12 COL 11 COLON-ALIGNED &ENDIF HELP
          "Дом"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     vStrChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 24 COLON-ALIGNED
          &ELSE AT ROW 12 COL 24 COLON-ALIGNED &ENDIF HELP
          "Строение"
     vKorpChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 38 COLON-ALIGNED
          &ELSE AT ROW 12 COL 38 COLON-ALIGNED &ENDIF HELP
          "Корпус"
     vKvChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 50 COLON-ALIGNED
          &ELSE AT ROW 12 COL 50 COLON-ALIGNED &ENDIF HELP
          "Квартира"
     tt-person.kodreg$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 67 COLON-ALIGNED
          &ELSE AT ROW 12 COL 67 COLON-ALIGNED &ENDIF HELP
          "Коды регионов"
          LABEL "Регион" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vAdrIndInt
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 11 COLON-ALIGNED
          &ELSE AT ROW 13 COL 11 COLON-ALIGNED &ENDIF HELP
          "Индекс"
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 1
          &ELSE AT ROW 14 COL 1 &ENDIF NO-LABEL
     tt-person.bank-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 11 COLON-ALIGNED
          &ELSE AT ROW 15 COL 11 COLON-ALIGNED &ENDIF HELP
          "Код идентификатора банка"
          LABEL "Код" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
     tt-person.bank-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 49 COLON-ALIGNED
          &ELSE AT ROW 15 COL 49 COLON-ALIGNED &ENDIF HELP
          "Значение иденификатора банка"
          LABEL "Идентиф. банка" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
          &ELSE SIZE 28 BY 1 &ENDIF
     tt-person.corr-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 11 COLON-ALIGNED
          &ELSE AT ROW 16 COL 11 COLON-ALIGNED &ENDIF HELP
          "Номер корреспондентского счета банка в РКЦ"
          LABEL "К/С" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-person.benacct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 49 COLON-ALIGNED
          &ELSE AT ROW 16 COL 49 COLON-ALIGNED &ENDIF
          LABEL "Счет клиента"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
          &ELSE SIZE 28 BY 1 &ENDIF
     separator3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 1
          &ELSE AT ROW 17 COL 1 &ENDIF NO-LABEL
     tt-person.date-in
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 7 COLON-ALIGNED
          &ELSE AT ROW 18 COL 7 COLON-ALIGNED &ENDIF HELP
          "Дата заведения клиента"
          LABEL "приход" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-person.date-out
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 7 COLON-ALIGNED
          &ELSE AT ROW 19 COL 7 COLON-ALIGNED &ENDIF HELP
          "Дата закрытия клиента"
          LABEL "уход" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-person.subw%ekt$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 27 COLON-ALIGNED
          &ELSE AT ROW 19 COL 27 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Субъект" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-person.passw_card
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 55 COLON-ALIGNED
          &ELSE AT ROW 19 COL 55 COLON-ALIGNED &ENDIF HELP
          "Кодовое слово для голосовой авторизации и службы поддержки"
          LABEL "Код. слово" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
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
      TABLE: tt-p-cust-adr-reg T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD country-id AS CHARACTER /* country-id */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-p-cust-adr-reg" "p-cust-adr-reg" }
          
      END-FIELDS.
      TABLE: tt-p-cust-adr-stay T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD country-id AS CHARACTER /* country-id */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-p-cust-adr-stay" "p-cust-adr-stay" }
          
      END-FIELDS.
      TABLE: tt-p-cust-ident T "?" NO-UNDO bisquit cust-ident
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
          {ln-tthdl.i "tt-p-cust-ident" "p-cust-ident" }
          
      END-FIELDS.
      TABLE: tt-person T "?" NO-UNDO bisquit person
      ADDITIONAL-FIELDS:
          FIELD sotoper$ AS CHARACTER /* СотОпер */
          FIELD dolruk$ AS CHARACTER /* ДолРук */
          FIELD dom$ AS CHARACTER /* ДОМ */
          FIELD unikkodadresa$ AS CHARACTER /* УникКодАдреса */
          FIELD a-test$ AS LOGICAL /* А-тест */
          FIELD arhiv$ AS LOGICAL /* Архив */
          FIELD izobr$ AS CHARACTER /* Изобр */
          FIELD indgr$ AS CHARACTER /* ИндГр */
          FIELD iobss$ AS CHARACTER /* ИОБСС */
          FIELD iogbh$ AS CHARACTER /* ИОГБХ */
          FIELD NumberPFR AS CHARACTER /* NumberPFR */
          FIELD unstruc_regadd1 AS CHARACTER /* unstruc_regadd1 */
          FIELD abwawtik$ AS CHARACTER /* АбЯщик */
          FIELD blok$ AS LOGICAL /* Блок */
          FIELD brawcndogovor$ AS CHARACTER /* БрачнДоговор */
          FIELD viddewat$ AS CHARACTER /* ВидДеят */
          FIELD vidwzilw#wa$ AS CHARACTER /* ВидЖилья */
          FIELD vidreg$ AS CHARACTER /* ВидРег */
          FIELD vidsotr$ AS CHARACTER /* ВидСотр */
          FIELD gvk$ AS CHARACTER /* ГВК */
          FIELD datavpred$ AS CHARACTER /* ДатаВПред */
          FIELD dataokonprop$ AS DATE /* ДатаОконПроп */
          FIELD dataokpred$ AS CHARACTER /* ДатаОкПред */
          FIELD dataprop$ AS DATE /* ДатаПроп */
          FIELD datapropiski$ AS DATE /* ДатаПрописки */
          FIELD dko$ AS DECIMAL /* ДкО */
          FIELD dkowe$ AS DECIMAL /* ДкОЭ */
          FIELD dolwz$ AS CHARACTER /* Долж */
          FIELD izmdokdata$ AS DATE /* ИзмДокДата */
          FIELD izmdokprednomer$ AS CHARACTER /* ИзмДокПредНомер */
          FIELD izmfidata$ AS DATE /* ИзмФИДата */
          FIELD izmfipredimwa$ AS CHARACTER /* ИзмФИПредИмя */
          FIELD izmfipredfam$ AS CHARACTER /* ИзмФИПредФам */
          FIELD iin$ AS CHARACTER /* ИИН */
          FIELD kategfiz$ AS CHARACTER /* КатегФиз */
          FIELD klient$ AS CHARACTER /* Клиент */
          FIELD klientuf$ AS LOGICAL /* КлиентУФ */
          FIELD klprowziv$ AS CHARACTER /* КлПрожив */
          FIELD klreestr$ AS CHARACTER /* КлРеестр */
          FIELD koddokum$ AS CHARACTER /* КодДокум */
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodsubckki$ AS CHARACTER /* КодСубЦККИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD koliwzd$ AS INT64 /* КолИжд */
          FIELD korpkl$ AS CHARACTER /* КорпКл */
          FIELD kpp$ AS CHARACTER /* КПП */
          FIELD licenzorg$ AS CHARACTER /* ЛицензОрг */
          FIELD mestsvedpred$ AS CHARACTER /* МестСведПред */
          FIELD migrkart$ AS CHARACTER /* МигрКарт */
          FIELD migrpravprebpo$ AS DATE /* МигрПравПребПо */
          FIELD migrpravprebs$ AS DATE /* МигрПравПребС */
          FIELD migrprebyvpo$ AS DATE /* МигрПребывПо */
          FIELD migrprebyvs$ AS DATE /* МигрПребывС */
          FIELD migrcelw#vizita$ AS CHARACTER /* МигрЦельВизита */
          FIELD nalrez$ AS LOGICAL /* НалРез */
          FIELD nbki_stprowz$ AS CHARACTER /* НБКИ_СтПрож */
          FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
          FIELD nomerregbrak$ AS CHARACTER /* НомерРегБрак */
          FIELD nommarwsrut$ AS CHARACTER /* НомМаршрут */
          FIELD notarsogl$ AS LOGICAL /* НотарСогл */
          FIELD ogrn$ AS CHARACTER /* ОГРН */
          FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
          FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
          FIELD okvwed$ AS CHARACTER /* ОКВЭД */
          FIELD opekasogl$ AS LOGICAL /* ОпекаСогл */
          FIELD orgsvedpred$ AS CHARACTER /* ОргСведПред */
          FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
          FIELD ofwsor$ AS CHARACTER /* Офшор */
          FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
          FIELD padewzdat$ AS CHARACTER /* ПадежДат */
          FIELD padewzrod$ AS CHARACTER /* ПадежРод */
          FIELD padewztvor$ AS CHARACTER /* ПадежТвор */
          FIELD predpr$ AS LOGICAL /* Предпр */
          FIELD prim$ AS CHARACTER /* Прим */
          FIELD prim1$ AS CHARACTER /* Прим1 */
          FIELD prim2$ AS CHARACTER /* Прим2 */
          FIELD prim3$ AS CHARACTER /* Прим3 */
          FIELD prim4$ AS CHARACTER /* Прим4 */
          FIELD prim5$ AS CHARACTER /* Прим5 */
          FIELD prim6$ AS CHARACTER /* Прим6 */
          FIELD rabota$ AS CHARACTER /* Работа */
          FIELD rabotaadr$ AS CHARACTER /* РаботаАдр */
          FIELD rabotanawim$ AS DATE /* РаботаНайм */
          FIELD rabotanawimkon$ AS DATE /* РаботаНаймКон */
          FIELD rabota_viddewat$ AS CHARACTER /* Работа_ВидДеят */
          FIELD rabota_zanwatostw#$ AS CHARACTER /* Работа_Занятость */
          FIELD rabota_polowzenie$ AS CHARACTER /* Работа_Положение */
          FIELD rabota_sferadewat$ AS CHARACTER /* Работа_СфераДеят */
          FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
          FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
          FIELD svedregpred$ AS CHARACTER /* СведРегПред */
          FIELD semewinpolowz$ AS CHARACTER /* СемейнПолож */
          FIELD sempol$ AS CHARACTER /* СемПол */
          FIELD stepenw#rod$ AS CHARACTER /* СтепеньРод */
          FIELD strahnomer$ AS CHARACTER /* СтрахНомер */
          FIELD subw%ekt$ AS CHARACTER /* Субъект */
          FIELD telefon3$ AS CHARACTER /* Телефон3 */
          FIELD umer$ AS LOGICAL /* Умер */
          FIELD unkg$ AS INT64 /* УНКг */
          FIELD uwcdok$ AS CHARACTER /* УчДок */
          FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
          FIELD uwcdokdata$ AS DATE /* УчДокДата */
          FIELD formsobs$ AS CHARACTER /* ФормСобс */
          FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
          FIELD address-home AS CHARACTER /* address-home */
          FIELD Address4Dom AS CHARACTER /* Address4Dom */
          FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
          FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
          FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD BQMail AS CHARACTER /* BQMail */
          FIELD BQSms AS CHARACTER /* BQSms */
          FIELD branch-id AS CHARACTER /* branch-id */
          FIELD branch-list AS CHARACTER /* branch-list */
          FIELD contr_group AS CHARACTER /* contr_group */
          FIELD contr_type AS CHARACTER /* contr_type */
          FIELD country-id2 AS CHARACTER /* country-id2 */
          FIELD country-id3 AS CHARACTER /* country-id3 */
          FIELD date-export AS CHARACTER /* date-export */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
          FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
          FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD e-mail AS CHARACTER /* e-mail */
          FIELD engl-name AS CHARACTER /* engl-name */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD holding-id AS CHARACTER /* holding-id */
          FIELD incass AS LOGICAL /* incass */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD lat_card AS CHARACTER /* lat_card */
          FIELD lat_dop AS CHARACTER /* lat_dop */
          FIELD lat_fam AS CHARACTER /* lat_fam */
          FIELD lat_f_n AS CHARACTER /* lat_f_n */
          FIELD lat_name AS CHARACTER /* lat_name */
          FIELD lat_otch AS CHARACTER /* lat_otch */
          FIELD lat_titul AS CHARACTER /* lat_titul */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD lic-sec AS CHARACTER /* lic-sec */
          FIELD Lic_num AS CHARACTER /* Lic_num */
          FIELD mess AS CHARACTER /* mess */
          FIELD my_pict AS CHARACTER /* my_pict */
          FIELD my_sign AS CHARACTER /* my_sign */
          FIELD Netting AS LOGICAL /* Netting */
          FIELD NoExport AS LOGICAL /* NoExport */
          FIELD num_contr AS INT64 /* num_contr */
          FIELD old-person-id AS CHARACTER /* old-person-id */
          FIELD passw_card AS CHARACTER /* passw_card */
          FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
          FIELD RegNum AS CHARACTER /* RegNum */
          FIELD RetailID AS CHARACTER /* RetailID */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD Soato AS CHARACTER /* Soato */
          FIELD Svid_num AS CHARACTER /* Svid_num */
          FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
          FIELD Visa AS CHARACTER /* Visa */
          FIELD VisaNum AS CHARACTER /* VisaNum */
          FIELD VisaType AS CHARACTER /* VisaType */
          FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
          FIELD xview-photo AS CHARACTER /* xview-photo */
          FIELD xview-sign AS CHARACTER /* xview-sign */
          FIELD vidrestrukt$ AS CHARACTER /* ВидРеструкт */
          FIELD country-id4 AS CHARACTER /* country-id4 */
          FIELD kodklienta$ AS CHARACTER /* КодКлиента */
          FIELD kop$ AS INT64 /* КОП */
          FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
          FIELD otnokruwz_ipdl$ AS CHARACTER /* ОтнОкруж_ИПДЛ */
          FIELD status_ipdl$ AS CHARACTER /* Статус_ИПДЛ */
          FIELD steprodst_ipdl$ AS CHARACTER /* СтепРодст_ИПДЛ */
          FIELD SphereID AS CHARACTER /* SphereID */
          FIELD okpo AS CHARACTER /* okpo */
          FIELD unk$ AS DECIMAL /* УНК */
          FIELD kopf$ AS INT64 /* КОПФ */
          FIELD obrbs$ AS CHARACTER /* ОбрБС */
          FIELD obrgb$ AS CHARACTER /* ОбрГБ */
          FIELD okato$ AS CHARACTER /* ОКАТО */
          FIELD osnova$ AS CHARACTER /* основа */
          FIELD ulica$ AS CHARACTER /* Улица */
          FIELD fiobs$ AS CHARACTER /* ФИОБС */
          FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
          FIELD vidkli$ AS CHARACTER /* ВидКли */
          FIELD grawzd$ AS CHARACTER /* Гражд */
          FIELD fiogb$ AS CHARACTER /* ФИОГБ */
          FIELD fioruk$ AS CHARACTER /* ФИОрук */
          FIELD wekonsekt$ AS CHARACTER /* ЭконСект */
          FIELD wurstat$ AS CHARACTER /* ЮрСтат */
          FIELD wa$ AS CHARACTER /* Я */
          FIELD a-mypic AS CHARACTER /* a-mypic */
          FIELD aaaa AS CHARACTER /* aaaa */
          FIELD Address1Indeks AS INT64 /* Address1Indeks */
          FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
          FIELD Address3Street AS CHARACTER /* Address3Street */
          FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
          FIELD CountryCode AS CHARACTER /* CountryCode */
          FIELD Exam AS CHARACTER /* Exam */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD IndCode AS CHARACTER /* IndCode */
          FIELD LocCustType AS CHARACTER /* LocCustType */
          FIELD my_pic AS CHARACTER /* my_pic */
          FIELD NACE AS CHARACTER /* NACE */
          FIELD New$$ AS CHARACTER /* New */
          FIELD New1 AS CHARACTER /* New1 */
          FIELD Prim-ID AS CHARACTER /* Prim-ID */
          FIELD RegDate AS CHARACTER /* RegDate */
          FIELD vygoda$ AS CHARACTER /* Выгода */
          FIELD striska$ AS CHARACTER /* СтРиска */
          FIELD dataankety$ AS DATE /* ДатаАнкеты */
          FIELD fiosotrudnika$ AS CHARACTER /* ФИОсотрудника */
          FIELD local__template AS LOGICAL   /* Признак шаблон/не шаблон */
          FIELD local__rowid    AS ROWID     /* ROWID записи в БД        */
          FIELD local__id       AS INT64   /* Идентификатор записи     */
          FIELD local__upid     AS INT64   /* Ссылка на запись в аггрегирующей таблице */
          FIELD user__mode      AS INT64   /* Флаг управления записью в БД */
          /* Additional fields you should place here                      */
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-person" "" }
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = " <insert window title>"
         HEIGHT             = 25.29
         WIDTH              = 83
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 83
         VIRTUAL-HEIGHT     = 25.29
         VIRTUAL-WIDTH      = 83
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
/* SETTINGS FOR FILL-IN tt-person.bank-code IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.bank-code-type IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.benacct IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-person.birthday IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.BirthPlace IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.corr-acct IN FRAME fMain
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN tt-person.country-id IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.date-in IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.date-out IN FRAME fMain
   2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-person.document IN FRAME fMain
   3 EXP-LABEL EXP-FORMAT EXP-HELP                                      */
/* SETTINGS FOR FILL-IN tt-person.document-id IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.Document4Date_vid IN FRAME fMain
   ALIGN-L 3 EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-person.first-names IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-person.gender IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.inn IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.kodreg$ IN FRAME fMain
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN tt-person.kodreggni$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN mBankClient IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mIssue IN FRAME fMain
   3                                                                    */
/* SETTINGS FOR FILL-IN mKP IN FRAME fMain
   3                                                                    */
/* SETTINGS FOR FILL-IN tt-person.name-last IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.passw_card IN FRAME fMain
   3 EXP-LABEL EXP-FORMAT EXP-HELP                                      */
/* SETTINGS FOR FILL-IN tt-person.person-id IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
ASSIGN 
       tt-person.person-id:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator3 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator4 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN tt-person.subw%ekt$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-person.tax-insp IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.unk$ IN FRAME fMain
   ALIGN-L 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN tt-person.vidsotr$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-person"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-person.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.bank-code TERMINAL-SIMULATION
ON F1 OF tt-person.bank-code IN FRAME fMain /* Идентиф. банка */
DO:
   DEF VAR vBankCodeType AS CHAR   NO-UNDO.  /* Код идентификатора банка. */

   IF    iMode EQ {&MOD_EDIT}
      OR iMode EQ {&MOD_ADD}
   THEN DO:
      vBankCodeType = tt-person.bank-code-type:SCREEN-VALUE.
      DO TRANSACTION:
         IF vBankCodeType NE "" AND
            vBankCodeType NE "ИНН"
            THEN DO:
               RUN bankscod.p (INPUT-OUTPUT vBankCodeType, iLevel + 1).
               tt-person.bank-code-type:SCREEN-VALUE = vBankCodeType.
            END.
            ELSE RUN banks.p (iLevel + 1).
      END.
      IF LASTKEY EQ 10
      THEN DO:
         ASSIGN
            tt-person.bank-code-type:SCREEN-VALUE  = ENTRY (1, pick-value)
            tt-person.bank-code     :SCREEN-VALUE  = ENTRY (2, pick-value)
         .
         APPLY "LEAVE" TO SELF.
         {&END_BT_F1}
         RETURN NO-APPLY.
      END.
   END.
   ELSE IF iMode EQ {&MOD_VIEW}
      THEN RUN formld.p ("banks", STRING (SELF:SCREEN-VALUE), "", 
                         {&MOD_VIEW}, iLevel + 1).

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.bank-code TERMINAL-SIMULATION
ON LEAVE OF tt-person.bank-code IN FRAME fMain /* Идентиф. банка */
DO:
   DEFINE VARIABLE vBankId  AS CHARACTER INIT ? NO-UNDO.  /* Идентификатор банка. */
   DEFINE VARIABLE vCustCat AS CHARACTER        NO-UNDO.
   DEFINE VARIABLE vCustId  AS CHARACTER        NO-UNDO.

   {&BEG_BT_LEAVE}
   IF       LENGTH (tt-person.bank-code-type:SCREEN-VALUE) GT 0
      AND   LENGTH (tt-person.bank-code     :SCREEN-VALUE) GT 0  
   THEN DO:

      IF tt-person.bank-code-type:SCREEN-VALUE EQ "МФО-9" THEN /* добить слева нулями */
         tt-person.bank-code:SCREEN-VALUE = fStrPadC (tt-person.bank-code:SCREEN-VALUE, 
                                                      9, NO, "0").

      IF tt-person.bank-code-type:SCREEN-VALUE EQ "ИНН" THEN
      DO:
         RUN GetCustomerByIdnt IN h_cust ("ИНН",
                                          tt-person.bank-code:SCREEN-VALUE,
                                          0, ?, ?,
                                          OUTPUT vCustCat,
                                          OUTPUT vCustId).
         IF LOOKUP ("Б", vCustCat) > 0  THEN
            vBankId = ENTRY (LOOKUP ("Б", vCustCat), vCustId).
      END.
      ELSE
      DO:         
         RUN GetBank IN h_base (BUFFER banks,
                                BUFFER banks-code,
                                INPUT  tt-person.bank-code     :SCREEN-VALUE,
                                INPUT  tt-person.bank-code-type:SCREEN-VALUE,
                                INPUT  YES).
         IF AVAIL banks THEN
            vBankId = STRING (banks.bank-id).
      END.
                           
      IF vBankId EQ ? THEN    
      DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","Не найден банк по идентификатору " + 
                                                 tt-person.bank-code:SCREEN-VALUE + "!").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      ELSE                    /* Поиск К/С банка. */
         vBankId = GetBufferValue ("banks-corr",
                                   "WHERE banks-corr.bank-corr EQ " + vBankId,
                                   "corr-acct").
                           /* Если нашли, то отображаем. */
      IF vBankId NE ?
      THEN ASSIGN
         tt-person.corr-acct:SCREEN-VALUE = vBankId
         tt-person.corr-acct              = vBankId.
      ELSE
         ASSIGN
            tt-person.corr-acct:SCREEN-VALUE = ""
            tt-person.corr-acct              = ""
         .
   END.
   ELSE
      ASSIGN
         tt-person.corr-acct:SCREEN-VALUE = ""
         tt-person.corr-acct              = ""
      .
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.corr-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.corr-acct TERMINAL-SIMULATION
ON F1 OF tt-person.corr-acct IN FRAME fMain /* К/С */
DO:
   DEF VAR vBankId AS INT64    NO-UNDO. /* Идентификатор банка. */

   IF (        iMode EQ {&MOD_EDIT}
          OR   iMode EQ {&MOD_ADD}
      )
      AND tt-person.bank-code-type:SCREEN-VALUE NE ""
   THEN DO TRANSACTION:
                        /* Получение идентификатора банка. */
      vBankId = INT64 (GetNeedBankCode (
                  tt-person.bank-code-type:SCREEN-VALUE,
                  tt-person.bank-code     :SCREEN-VALUE,
                  "bank-id"
      )).
      RUN banksch2.p (vBankId, iLevel + 1).
      IF LASTKEY EQ 10
         THEN ASSIGN
            tt-person.corr-acct:SCREEN-VALUE = pick-value
            tt-person.corr-acct              = pick-value.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.country-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.country-id TERMINAL-SIMULATION
ON U1 OF tt-person.country-id IN FRAME fMain /* Код страны */
DO:
                        /* Если кол-во возвращаемых элементов больш 2-х,
                        ** то берем только первый. */
   pick-value = ENTRY (1, pick-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.date-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.date-in TERMINAL-SIMULATION
ON LEAVE OF tt-person.date-in IN FRAME fMain /* приход */
DO:
   {&BEG_BT_LEAVE}
   IF (  iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT})
      AND (DATE (tt-person.date-out:SCREEN-VALUE) LT DATE (tt-person.date-in:SCREEN-VALUE))
   THEN DO:
      RUN Fill-SysMes IN h_tmess (
         "", "", "0",
         "Дата заведения клиента должна быть меньше даты ухода."
      ).
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.date-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.date-out TERMINAL-SIMULATION
ON LEAVE OF tt-person.date-out IN FRAME fMain /* уход */
DO:
   {&BEG_BT_LEAVE}
   IF (  iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT})
      AND (DATE (tt-person.date-out:SCREEN-VALUE) LT DATE (tt-person.date-in:SCREEN-VALUE))
   THEN DO:
      RUN Fill-SysMes IN h_tmess (
         "", "", "0",
         "Дата заведения клиента должна быть меньше даты ухода.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.document-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.document-id TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-person.document-id IN FRAME fMain /* Документ */
DO:
  RETURN NO-APPLY {&RET-ERROR}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.document-id TERMINAL-SIMULATION
ON F1 OF tt-person.document-id IN FRAME fMain /* Документ */
DO:
   DEFINE VARIABLE vCountry AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCitizen AS CHARACTER NO-UNDO.
   DEFINE BUFFER xcust-ident FOR cust-ident.

   ASSIGN 
      tt-person.country-id.
   vCountry = GetXattrValueEx("person",STRING(tt-person.person-id),"country-id2",tt-person.country-id).
   IF NOT {assigned vCountry} THEN
      vCitizen  = "Stateless".
   ELSE
      vCitizen = IF vCountry EQ "rus" THEN "CitizenRF" ELSE "Foreigner".
   RUN SetSysConf IN h_base ("FPersonIsRezident",vCitizen).

   IF iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      FIND FIRST xcust-ident WHERE ROWID(xcust-ident) = tt-p-cust-ident.local__rowid NO-LOCK NO-ERROR.
      RUN browseld.p("p-cust-ident", 
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "close-date1"     + CHR(1) + "close-date2" + CHR(1) + "ActionLock",
                     "Ч"        + CHR(1) + STRING(tt-person.person-id) + CHR(1) + STRING(gend-date) + CHR(1) + "" + CHR(1) + "F5",
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "close-date1"     + CHR(1) + "close-date2",
                     iLevel + 1).
      IF LASTKEY EQ 10
      THEN DO:
         FIND FIRST xcust-ident WHERE xcust-ident.cust-code-type = ENTRY(1, pick-value) 
                                  AND xcust-ident.cust-code      = ENTRY(2, pick-value)
         NO-LOCK NO-ERROR.
      END.
      IF AVAIL xcust-ident THEN DO:
         ASSIGN
            tt-person.document-id       = xcust-ident.cust-code-type
            tt-person.document          = xcust-ident.cust-code
            tt-person.Document4Date_vid = xcust-ident.open-date
            mIssue                      = xcust-ident.issue
         .
         mKP = GetXattrValue("cust-ident",
                             GetSurrogateBuffer("cust-ident",(BUFFER xcust-ident:HANDLE)),
                             "Подразд").
         DISPLAY tt-person.document-id tt-person.document mIssue mKP tt-person.Document4Date_vid WITH FRAME {&MAIN-FRAME}.
      END.
   END.
   ELSE IF iMode EQ {&MOD_ADD}
   THEN DO:
      /* Получение указателя на датасет. */
      ASSIGN
         vHInst      = WIDGET-HANDLE (GetInstanceProp2 (mInstance,"p-cust-ident"))
         vTemplate   = LOGICAL (GetInstanceProp2 (vHInst,"__template"))
      .
      RUN SetInstanceProp (vHInst, "cust-cat", "Ч", OUTPUT vOk) NO-ERROR.
      RUN formld.p (
         STRING (vHInst) + "~003p-cust-ident",
         "",
         "",
         {&MOD_ADD},
         iLevel + 6
      ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
      THEN DO:
         /* Синхронизируем со статическими таблицами. */
         RUN fill-local-tt (mInstance, YES, NO).
         ASSIGN
            tt-person.document-id      :SCREEN-VALUE  = tt-p-cust-ident.cust-code-type
            tt-person.document         :SCREEN-VALUE  = tt-p-cust-ident.cust-code
            mIssue                     :SCREEN-VALUE  = tt-p-cust-ident.issue
            mKP                        :SCREEN-VALUE  = tt-p-cust-ident.podrazd$
            tt-person.Document4Date_vid:SCREEN-VALUE  = STRING (tt-p-cust-ident.open-date)
         .
         RUN SetInstanceProp (vHInst, "__template",'no', OUTPUT vOk) NO-ERROR.
      END.
      ELSE RUN SetInstanceProp (vHInst, "__template", STRING (vTemplate), OUTPUT vOk) NO-ERROR.
   END.
   RUN DeleteOldDataProtocol IN h_base  ("FPersonIsRezident").

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.document-id TERMINAL-SIMULATION
ON LEAVE OF tt-person.document-id IN FRAME fMain /* Документ */
DO:
   {&BEG_BT_LEAVE}
   DEF VAR vHBuffer AS HANDLE NO-UNDO. /* Указатель на буффер. */

   IF iMode NE {&MOD_ADD}
   THEN DO:
      CREATE BUFFER vHBuffer FOR TABLE "cust-ident".
      vHBuffer:FIND-FIRST ("WHERE
               cust-ident.cust-code-type EQ '" + SELF:SCREEN-VALUE + "'
         AND   cust-ident.cust-cat       EQ 'Ч'
         AND   cust-ident.cust-id        EQ '" + STRING (tt-person.person-id) + "'
         AND   cust-ident.close-date     EQ ?",
      NO-LOCK) NO-ERROR.
      IF vHBuffer:AVAIL
      THEN DO:
         ASSIGN
            tt-person.document-id      :SCREEN-VALUE  = vHBuffer:BUFFER-FIELD ("cust-code-type"):BUFFER-VALUE
            tt-person.document         :SCREEN-VALUE  = vHBuffer:BUFFER-FIELD ("cust-code")     :BUFFER-VALUE
            mIssue                                    = vHBuffer:BUFFER-FIELD ("issue")         :BUFFER-VALUE
            mIssue                     :SCREEN-VALUE  = mIssue
            tt-person.Document4Date_vid:SCREEN-VALUE  = vHBuffer:BUFFER-FIELD ("open-date")     :BUFFER-VALUE
         .
         mKP = GetXattrValue("cust-ident",
                             GetSurrogateBuffer("cust-ident",vHBuffer),
                             "Подразд").
         mKP:SCREEN-VALUE = mKP.
      END.
      ELSE DO:

         ASSIGN
            tt-person.document-id      :SCREEN-VALUE  = ""
            tt-person.document         :SCREEN-VALUE  = ""
            mIssue                     :SCREEN-VALUE  = ""
            mKP                        :SCREEN-VALUE  = ""
            tt-person.Document4Date_vid:SCREEN-VALUE  = ""
         .   
         RUN Fill-SysMes IN h_tmess ("", "", "0","У клиента нет ни одного действующего удостоверения личности.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      DELETE OBJECT vHBuffer NO-ERROR.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.inn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.inn TERMINAL-SIMULATION
ON LEAVE OF tt-person.inn IN FRAME fMain /* ИНН */
DO:
   DEFINE VAR i       AS INTEGER NO-UNDO.
   DEFINE VAR vLen    AS INTEGER NO-UNDO.
   DEFINE VAR isAlpha AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vFl AS LOGICAL INIT YES NO-UNDO.
   DEFINE VAR vTempVal  AS CHAR    NO-UNDO.
   DEFINE VAR vValidInn AS LOGICAL NO-UNDO.
   DEFINE BUFFER xperson FOR person.
   {&BEG_BT_LEAVE}
   ASSIGN
      tt-person.inn
      tt-person.country-id
   .
   IF {assigned tt-person.inn} THEN DO:
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
      vTempVal = GetValueByQuery("person",
                                 "person-id",
                                 "     person.inn        = '" + tt-person.inn + "'" +
                                 " AND person.country-id = '" + tt-person.country-id + "'" +
                                 (IF tt-person.local__rowid <> ?
                                  THEN " AND ROWID(person) <> TO-ROWID('" + STRING(tt-person.local__rowid) + "')"
                                  ELSE "") +
                                 " NO-LOCK"
                                ).
      IF vTempVal NE ? THEN DO:
         FIND FIRST xperson WHERE xperson.person-id = INT64(vTempVal) NO-LOCK NO-ERROR.
         IF AVAIL xperson
         THEN DO:
           MESSAGE "Запись с такими реквизитами уже существует в таблице:" 
                SUBSTR(xperson.name-last + " " + xperson.first-names, 1, 80) "!" 
           SKIP "OK - Исправить, Cancel - Пропустить"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " Внимание " UPDATE vfl.
           IF vfl OR vfl EQ ? THEN 
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      IF vLen NE 5 AND NOT fValidInnSignature(SELF:SCREEN-VALUE, mTempVal) THEN DO:
        MESSAGE ("Последние 2 цифры ИНН (ключ) должны быть: ~"" + mTempVal + "~"")
              SKIP "OK - Исправить, Cancel - Пропустить"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " Внимание " UPDATE vfl.
        IF vfl OR vfl EQ ? THEN 
               RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   IF tt-person.inn = ? THEN DO:
      tt-person.inn = "".
      tt-person.inn:SCREEN-VALUE = "".
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.kodreggni$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.kodreggni$ TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   RETURN NO-APPLY {&RET-ERROR}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.kodreggni$ TERMINAL-SIMULATION
ON ENTRY OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   &SCOPED-DEFINE KodRegGniHelpStr "F1 Осн. адрес"
   SetHelpStrAdd(TRIM(mHelpStrAdd + "│" + {&KodRegGniHelpStr},"│")).
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.kodreggni$ TERMINAL-SIMULATION
ON F1 OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   IF iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      FIND FIRST bcident WHERE bcident.cust-cat       = "Ч"
                           AND bcident.cust-id        = tt-person.person-id
                           AND bcident.cust-code      = mUniqCodAdr
                           AND bcident.cust-code-type = mAdrType
      NO-LOCK NO-ERROR.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
                     "Ч"        + CHR(1) + STRING(tt-person.person-id) + CHR(1) + mAdrType,
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
                     iLevel + 1).
      IF LASTKEY EQ 10
      THEN DO:
         FIND FIRST bcident WHERE
                    bcident.cust-cat       EQ "Ч"
                AND bcident.cust-id        EQ tt-person.person-id
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
            tt-person.kodreggni$       :SCREEN-VALUE = GetXattrValue('cust-ident',
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
            tt-person.kodreg$          :SCREEN-VALUE = GetXattrValue('cust-ident',
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
         vHInst      = WIDGET-HANDLE (GetInstanceProp2 (mInstance,"p-cust-adr-reg"))
         vTemplate   = LOGICAL (GetInstanceProp2 (vHInst,"__template"))
      .
      
      RUN SetInstanceProp (vHInst,"cust-cat"      ,"Ч"     ,OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHInst,"cust-code-type",mAdrType,OUTPUT vOk) NO-ERROR.
      RUN formld.p (STRING (vHInst) + "~003p-cust-adr",
                    "",
                    "Ч~003~003" + mAdrType,
                    {&MOD_ADD},
                    iLevel + 6
                   ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
      THEN DO:
         TEMP-TABLE tt-p-cust-adr-reg:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (vHInst:DEFAULT-BUFFER-HANDLE).
         {cust-adr.obj 
            &addr-to-vars     = YES
            &addr-to-vars-gni = YES
            &tablefield       = "TRIM(tt-p-cust-adr-reg.issue)" 
            &fieldgni         = "tt-p-cust-adr-reg.kodyadresa$"
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
            tt-person.kodreggni$       :SCREEN-VALUE = tt-p-cust-adr-reg.kodreggni$
            vAdrCntry                                = tt-p-cust-adr-reg.country-id
            tt-person.kodreg$          :SCREEN-VALUE = tt-p-cust-adr-reg.kodreg$
            mUniqCodAdr                              = tt-p-cust-adr-reg.cust-code                                                                                                                                           
         NO-ERROR.
      END.
      ELSE RUN SetInstanceProp (vHInst, "__template", STRING (vTemplate), OUTPUT vOk) NO-ERROR.
   END.
   ELSE IF iMode = {&MOD_VIEW} THEN DO:
      FOR FIRST bcident WHERE bcident.cust-cat       = "Ч"
                          AND bcident.cust-id        = tt-person.person-id
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.kodreggni$ TERMINAL-SIMULATION
ON LEAVE OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
   SetHelpStrAdd(TRIM(REPLACE(mHelpStrAdd,{&KodRegGniHelpStr},""),"│")).
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON CLEAR OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON BACKSPACE OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON DELETE-CHARACTER OF tt-person.kodreggni$ IN FRAME fMain /* Регион ГНИ */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

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
      AND  SELF:SCREEN-VALUE NE ENTRY (1, SELF:FORMAT, "/") 
   THEN DO:
      IF GetBufferValue (
         "acct",
         "WHERE " +
         "     acct.cust-cat  EQ 'ч' " +
         "AND  acct.cust-id   EQ '" + STRING (tt-person.person-id) + "'",
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

&Scoped-define SELF-NAME tt-person.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.unk$ TERMINAL-SIMULATION
ON LEAVE OF tt-person.unk$ IN FRAME fMain /* УНК */
DO:
{&BEG_BT_LEAVE}
      RUN ChkUpdUnk$ IN h_cust("person",
                               STRING(tt-person.person-id),
                               tt-person.unk$:SCREEN-VALUE) NO-ERROR.
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

&Scoped-define SELF-NAME tt-person.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.unk$ TERMINAL-SIMULATION
ON ENTRY OF tt-person.unk$ IN FRAME fMain /* unk$ */
DO:
   IF iMode EQ {&MOD_EDIT}
   AND tt-person.unk$ EQ ? 
   AND mFlagUnk
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "4", "Присвоить новое значение УНК?").
      IF pick-value EQ "YES" THEN
         tt-person.unk$:SCREEN-VALUE = STRING(NewUnk("person")).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-person.person-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.person-id TERMINAL-SIMULATION
ON F1 OF tt-person.person-id IN FRAME fMain /* Номер */
DO:
   APPLY "F1" TO tt-person.unk$.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-person.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-person.unk$ TERMINAL-SIMULATION
ON F1 OF tt-person.unk$ IN FRAME fMain /* УНК */
DO:

   DEF VAR vOk AS LOGICAL  NO-UNDO. /* Результат переопределения объекта */

   IF iMode EQ {&MOD_VIEW} THEN 
      RUN xview.p    ("person", tt-person.person-id).  
   ELSE IF  iMode EQ {&MOD_EDIT}
         OR iMode EQ {&MOD_ADD} THEN 
   DO:
      DO TRANSACTION:
         RUN browseld.p ("person", "", "", "", iLevel + 1).
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

               RUN GetBaseAttrs     IN h_data ("person", 
                                               iSurrogate, 
                                               mInstance, 
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN GetExtAttrs      IN h_data ("person", 
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

ON F2 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DEFINE VAR vHCustIdent     AS HANDLE  NO-UNDO.
   DEFINE VAR vHCustAdr       AS HANDLE  NO-UNDO.
   DEFINE VAR vHCustAdr2      AS HANDLE  NO-UNDO.
   DEFINE VAR vCustIdentTempl AS LOGICAL NO-UNDO.
   DEFINE VAR vCustAdrTempl   AS LOGICAL NO-UNDO.
   DEFINE VAR vCustAdr2Templ  AS LOGICAL NO-UNDO.
   DEFINE VAR vOk             AS LOGICAL NO-UNDO.

   ASSIGN
      vHCustIdent     = WIDGET-HANDLE(GetInstanceProp2(mInstance,"p-cust-ident"))
      vHCustAdr       = WIDGET-HANDLE(GetInstanceProp2(mInstance,"p-cust-adr-reg"))
      vHCustAdr2      = WIDGET-HANDLE(GetInstanceProp2(mInstance,"p-cust-adr-stay"))
      vCustIdentTempl = LOGICAL(GetInstanceProp2(vHCustIdent,"__template"))
      vCustAdrTempl   = LOGICAL(GetInstanceProp2(vHCustAdr  ,"__template"))
      vCustAdr2Templ  = LOGICAL(GetInstanceProp2(vHCustAdr2 ,"__template"))
   .
   RUN fill-container-tt (mInstance, "", "", NO) NO-ERROR.
   RUN f-pers-cont.p (STRING(mInstance) + {&PARAM_DELIM} + STRING(mIsContChecking), mInstance, "person",
                      STRING(tt-person.person-id), iMode, iLevel + 1).
   IF LASTKEY = 10
   OR LASTKEY = 13 THEN DO:
      mIsContChecking = NO.
      RUN fill-local-tt (mInstance, YES, NO).
      RUN SetInstanceProp (vHCustIdent, "__template", STRING(vCustIdentTempl), OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHCustAdr, "__template", STRING(vCustAdrTempl), OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHCustAdr2, "__template", STRING(vCustAdr2Templ), OUTPUT vOk) NO-ERROR.
   END.
   RETURN NO-APPLY.
END.

ON F5 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
DEFINE VARIABLE vHInst          AS HANDLE    NO-UNDO. /* Указатель на ID пользователя. */
DEFINE VARIABLE vTemplate       AS LOGICAL   NO-UNDO. /* Значение __template для cust-ident.*/
   IF iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      FIND FIRST bcident WHERE bcident.cust-cat       = "Ч"
                           AND bcident.cust-id        = tt-person.person-id
                           AND bcident.cust-code-type = "АдрФакт"
      NO-LOCK NO-ERROR.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
                     "Ч"        + CHR(1) + STRING(tt-person.person-id) + CHR(1) + "АдрФакт",
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
                     iLevel + 1).
      IF LASTKEY EQ 10
      THEN DO:
         FIND FIRST bcident WHERE
                    bcident.cust-cat       EQ "Ч"
                AND bcident.cust-id        EQ tt-person.person-id
                AND bcident.cust-code-type EQ ENTRY(1,pick-value)
                AND bcident.cust-code      EQ ENTRY(2,pick-value)
         NO-LOCK NO-ERROR.
      END.
      IF AVAIL bcident THEN DO:
         tt-person.PlaceOfStay = bcident.issue.
      END.
   END.
   ELSE IF iMode EQ {&MOD_ADD}
   THEN DO:
      /* Получение указателя на датасет. */
      ASSIGN
         vHInst      = WIDGET-HANDLE (GetInstanceProp2 (mInstance,"p-cust-adr-stay"))
         vTemplate   = LOGICAL (GetInstanceProp2 (vHInst,"__template"))
      .
      
      RUN SetInstanceProp (vHInst,"cust-cat"      ,"Ч"     ,OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHInst,"cust-code-type","АдрФакт",OUTPUT vOk) NO-ERROR.
      RUN formld.p (STRING (vHInst) + "~003p-cust-adr",
                    "",
                    "Ч~003~003АдрФакт",
                    {&MOD_ADD},
                    iLevel + 6
                   ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
      THEN DO:
         TEMP-TABLE tt-p-cust-adr-stay:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (vHInst:DEFAULT-BUFFER-HANDLE).
         tt-person.PlaceOfStay = tt-p-cust-adr-stay.issue.
      END.
      ELSE RUN SetInstanceProp (vHInst, "__template", STRING (vTemplate), OUTPUT vOk) NO-ERROR.
   END.
   ELSE IF iMode = {&MOD_VIEW} THEN DO:
      FOR LAST bcident WHERE bcident.cust-cat       = "Ч"
                         AND bcident.cust-id        = tt-person.person-id
                         AND bcident.cust-code-type = "АдрФакт"
      NO-LOCK BY bcident.open-date:
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

ON F6 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DEFINE VAR vMandAddrs AS CHAR NO-UNDO.
   DEFINE BUFFER xxcode FOR code.
   IF iMode = {&MOD_EDIT}
   OR iMode = {&MOD_VIEW} THEN DO:
      FOR EACH xxcode WHERE xxcode.class = "КодАдр"
                        AND CAN-DO(xxcode.misc[2],"Ч") NO-LOCK:
         {additem.i vMandAddrs xxcode.code}
      END.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
                     "Ч"        + CHR(1) + STRING(tt-person.person-id) + CHR(1) + vMandAddrs,
                     "cust-cat" + CHR(1) + "cust-id"                   + CHR(1) + "cust-code-type",
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

   /* Вх. параметры */
   {get-form-params.i
      mBankClient "LOGICAL"
      mCrCustRole "CHARACTER"
   }

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").

   RUN MainAction NO-ERROR.
   IF ERROR-STATUS:ERROR
      THEN LEAVE MAIN-BLOCK.

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
  DISPLAY mBankClient separator4 mIssue mKP separator1 vOblChar vGorChar 
          vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt 
          separator2 separator3 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-person THEN 
    DISPLAY tt-person.person-id tt-person.unk$ tt-person.vidsotr$ 
          tt-person.name-last tt-person.first-names tt-person.birthday 
          tt-person.gender tt-person.BirthPlace tt-person.country-id 
          tt-person.inn tt-person.tax-insp tt-person.document-id 
          tt-person.document tt-person.Document4Date_vid tt-person.kodreggni$ 
          tt-person.kodreg$ tt-person.bank-code-type tt-person.bank-code 
          tt-person.corr-acct tt-person.benacct tt-person.date-in 
          tt-person.date-out tt-person.subw%ekt$ tt-person.passw_card 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mBankClient tt-person.person-id tt-person.unk$ tt-person.vidsotr$ 
         tt-person.name-last tt-person.first-names tt-person.birthday 
         tt-person.gender tt-person.BirthPlace tt-person.country-id 
         tt-person.inn tt-person.tax-insp separator4 tt-person.document-id 
         tt-person.document tt-person.Document4Date_vid mIssue mKP separator1 
         tt-person.kodreggni$ vOblChar vGorChar vPunktChar vUlChar vDomChar 
         vStrChar vKorpChar vKvChar tt-person.kodreg$ vAdrIndInt separator2 
         tt-person.bank-code-type tt-person.bank-code tt-person.corr-acct 
         tt-person.benacct separator3 tt-person.date-in tt-person.date-out 
         tt-person.subw%ekt$ tt-person.passw_card 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitEnableDisable TERMINAL-SIMULATION 
PROCEDURE InitEnableDisable :
DEF VAR vXattrFormat AS CHAR   NO-UNDO. /* ФОрмат ДР УНК. */
   
   IF  mFlagUnk
   AND iMode EQ {&MOD_EDIT}
   THEN DO:
      vXattrFormat = GetXAttrEx (iClass, "УНК", "data-format").
      IF vXattrFormat NE FILL("9", LENGTH(vXattrFormat))
      THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "0",
            "Неверно задан формат реквизита ~"УНК~" для класса ~"" + iClass + "~"." +
            "~nФормат УНК д.б. ~"999...~".").
         RETURN ERROR.
      END.
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION 
PROCEDURE LocalEnableDisable :
IF mFlagUnk
      THEN tt-person.person-id:HIDDEN IN FRAME fMain = YES.
      ELSE tt-person.unk$     :HIDDEN IN FRAME fMain = YES.
                        /* Позиционирование курсера. */
   IF       iMode                                  EQ {&MOD_ADD}
      AND   NUM-ENTRIES (iInstanceList, CHR (3))   GE 2
      THEN  mFirstTabItemAll  =  IF mFlagUnk
                                    THEN tt-person.unk$:HANDLE
                                    ELSE tt-person.person-id:HANDLE.

   IF iMode EQ {&MOD_ADD} OR
     (iMode EQ {&MOD_EDIT} AND
      NOT CAN-FIND(FIRST cust-role WHERE
                        cust-role.cust-cat   EQ "Ч"
                    AND cust-role.cust-id    EQ STRING(tt-person.person-id)
                    AND cust-role.Class-Code EQ "ImaginClient"
                    AND cust-role.file-name  EQ "branch"                                  
                    AND cust-role.surrogate  NE shFilial
                        USE-INDEX cust-id 
                        NO-LOCK)) THEN DO:
       mBankClient:SENSITIVE = YES.
   
   END.
   ELSE mBankClient:SENSITIVE = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
   DEFINE VAR vEndDate      AS DATE      NO-UNDO.
   DEFINE VAR vOldEndDate   AS DATE      NO-UNDO.   
   DEFINE VAR vAvtoSrok     AS CHARACTER NO-UNDO.
   IF mOnlyForm AND iMode EQ {&MOD_ADD} THEN
   DO:
      /* проверяем документы */
      FIND FIRST code WHERE code.class  EQ "КодДокум"
                        AND code.parent EQ "КодДокум"
                        AND code.code   EQ tt-person.document-id NO-LOCK NO-ERROR.
      IF AVAIL code THEN
      DO:
         vAvtoSrok = GetXattrValueEx("code",code.class + "," + code.code,"АвтоСрок","").
   
         vEndDate = ?.
         vOldEndDate = ?.
         IF {assigned vAvtoSrok} THEN DO:
            IF tt-person.birthday NE ? THEN DO:
               vEndDate = CalcSrokForDocument(vAvtoSrok,tt-p-cust-ident.open-date,tt-person.birthday).
               vOldEndDate = tt-p-cust-ident.end-date.
   
               IF vEndDate NE ? AND vEndDate NE vOldEndDate THEN DO:
                  RUN Fill-SysMes IN h_tmess ("","","3",
                                              "Для документа [" + tt-p-cust-ident.cust-code-type + "]~n" +
                                              "скорректировать дату окончания на " + STRING(vEndDate,"99/99/9999") + "?" + "|Да,Нет").
                  IF pick-value EQ "1" THEN
                     tt-p-cust-ident.end-date = vEndDate.
                  vOldEndDate = vEndDate.
               END.
               
               IF vOldEndDate LT TODAY AND FGetSetting("СтандТр","СрокДокум","Нет") NE "Нет" THEN DO:
                  RUN Fill-SysMes IN h_tmess ("","comm15",IF FGetSetting("СтандТр","СрокДокум","Нет") = "Да" THEN "-1" ELSE "",
                                              "%s=" + tt-person.name-last + " " + tt-person.first-names +
                                              "%s=" + tt-p-cust-ident.cust-code-type + "%s=" + STRING(vOldEndDate) + "|Продолжить,Отменить").
                  IF pick-value EQ "yes" OR pick-value EQ "2" THEN
                  RETURN ERROR "Документ не введен".            
               END.
            END.
         END.
      END.
   END.
   
tt-person.document-id = "__FORM~001" + tt-person.document-id.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local_GO TERMINAL-SIMULATION 
PROCEDURE local_GO :
tt-person.last-date = TODAY.
   
   DO WITH FRAME fMain:
      ASSIGN
                        /* СОхраняем адрес. */
         vAdrIndInt  = INT64 (vAdrIndInt:SCREEN-VALUE)
         vOblChar    = vOblChar  :SCREEN-VALUE
         vGorChar    = vGorChar  :SCREEN-VALUE
         vPunktChar  = vPunktChar:SCREEN-VALUE
         vUlChar     = vUlChar   :SCREEN-VALUE
         vDomChar    = vDomChar  :SCREEN-VALUE
         vKorpChar   = vKorpChar :SCREEN-VALUE
         vKvChar     = vKvChar   :SCREEN-VALUE
         vStrChar    = vStrChar  :SCREEN-VALUE
                        /* Сохраняем коды адреса. */
         tt-person.kodyadresa$ = fChkDopGni(tt-person.kodreggni$:SCREEN-VALUE,vCodeOblChar,
                                            vCodeGorChar,vCodePunktChar,vCodeUlChar)
                        /* Сохраняем документ. */
         tt-person.document-id         = tt-person.document-id      :SCREEN-VALUE
         tt-person.document            = tt-person.document         :SCREEN-VALUE
         mIssue                        = mIssue                     :SCREEN-VALUE
         mKP                           = mKP                        :SCREEN-VALUE
         tt-person.Document4Date_vid   = DATE (tt-person.Document4Date_vid:SCREEN-VALUE)
      NO-ERROR.
      RUN MakeIssue IN h_cust (mIssue, mKP, OUTPUT tt-person.issue).

      mIsContChecking = YES.
      APPLY "F2" TO FRAME {&MAIN-FRAME}.
      IF mIsContChecking THEN DO:
         mIsContChecking = NO.
         RETURN ERROR.
      END.

   END.
   /* Сохраняем адрес из экрана в буффер. */
   {cust-adr.obj 
      &vars-to-addr = YES
      &tablefield   = "tt-person.address[1]"
      &AddCond      = "tt-person.address[2] = '__FORM~001'"
   }
   tt-person.unikkodadresa$ = mUniqCodAdr.

   ASSIGN
      tt-person.kodreg$
      tt-person.nommarwsrut$ = mNomMarsh
   .
   IF mOnlyForm AND iMode EQ {&MOD_ADD} AND mBankClient:SCREEN-VALUE EQ "клиент" THEN
   DO:   
      /* записываем УНКг */
      RUN GetNewUnkgNumber("person", YES, OUTPUT tt-person.unkg$).
      SetFormDefList('tt-person.kodyadresa$,tt-person.unikkodadresa$,tt-person.nommarwsrut$,tt-person.unkg$').
   END.
   ELSE
   SetFormDefList('tt-person.kodyadresa$,tt-person.unikkodadresa$,tt-person.nommarwsrut$').

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainAction TERMINAL-SIMULATION 
PROCEDURE MainAction PRIVATE :
/* Commented by KSV: Корректируем вертикальную позицию фрейма */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.

   /* Commented by KSV: Читаем данные */
   RUN GetObject NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN RETURN ERROR.

   IF {assigned tt-person.nommarwsrut$} THEN
      mNomMarsh = tt-person.nommarwsrut$.

   SetHelpStrAdd("F2 Контакты│F5 Место преб. в РФ" + (IF iMode = {&MOD_EDIT} OR iMode = {&MOD_VIEW} THEN "│F6 Обяз. адреса" ELSE "")).

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
   RETURN ERROR.
&ENDIF

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").
     
   IF iMode <> {&MOD_ADD} THEN DO WITH FRAME {&MAIN-FRAME}:
      DISPLAY tt-person.document-id tt-person.document.
      APPLY "LEAVE" TO tt-person.document-id.
   END.
       
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
DEF BUFFER cust-ident FOR cust-ident.

         /* Проверка наличия права просмотра информации о клиенте у текущего пользователя */
   IF   (iMode EQ {&MOD_EDIT}
      OR iMode EQ {&MOD_VIEW})
      AND NOT GetPersonPermission(tt-person.person-id)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(tt-person.person-id)).
      RETURN ERROR.
   END.
            /* Для режима просмотра состояния на дату ищем актуальный
            ** на эту дату адрес прописки */
   IF mMOD_VIEW_DATE
   THEN
      FIND LAST cust-ident WHERE cust-ident.class-code      EQ "p-cust-adr"
                             AND cust-ident.cust-code-type  EQ "АдрПроп"
                             AND cust-ident.cust-cat        EQ "Ч"
                             AND cust-ident.cust-id         EQ tt-person.person-id
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
      tt-person.kodreg$    = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'КодРег').
      tt-person.kodreggni$ = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'КодРегГНИ').
   END.
   ELSE DO:
      /* Разбор значений адреса по экрану. */
      {cust-adr.obj 
         &addr-to-vars     = YES
         &addr-to-vars-gni = YES
         &tablefield       = "TRIM(tt-person.address[1] + ' ' + tt-person.address[2])"
         &fieldgni         = "tt-person.kodyadresa$"    
      }
   END.
   IF iMode NE {&MOD_ADD} THEN 
      ASSIGN 
         vAdrCntry   = GetXattrValue("person",
                                     STRING(tt-person.person-id),
                                     mAdrCntXattr)
         mUniqCodAdr = tt-person.unikkodadresa$
      .


   /* Определяем что отображать. */
   {getflagunk.i &class-code="'person'" &flag-unk="mFlagUnk"}

   IF iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT}
   THEN DO:             
      IF mFlagUnk AND tt-person.unk$ EQ ?
      THEN DO:
         pick-value = "YES".
         IF iMode EQ {&MOD_EDIT} THEN
            RUN Fill-SysMes IN h_tmess ("", "", "4", "Присвоить новое значение УНК?").
         IF pick-value EQ "YES" THEN DO:
            IF (FGetSetting("ГенУНК", "", "НЕТ") EQ "ДА" AND mBankClient) 
               OR FGetSetting("ГенУНК", "", "НЕТ") EQ "НЕТ" THEN

               tt-person.unk$       = NewUnk (iClass).
         END.
      END.
   END.

   IF    iMode EQ {&MOD_VIEW}
      OR iMode EQ {&MOD_EDIT}
      THEN mBankClient = GetValueByQuery (
         "cust-role",
         "class-code",
         "        cust-role.cust-cat   EQ 'Ч'" + 
         "  AND   cust-role.cust-id    EQ '" + (IF {assigned iSurrogate} THEN iSurrogate 
                                                                         ELSE STRING(tt-person.person-id)) + "'" +
         "  AND   cust-role.class-code EQ 'ImaginClient'"
      ) NE ?.
                        /* Поиск документа для режима создания. */
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_VIEW} THEN
   DO:
                        /* Если указан тип документа,
                        ** то ищем последний действующий документ
                        ** и перебрасываем данные для отображения. */
      IF LENGTH (tt-person.document-id) GT 0 THEN
      DO:
         LASTDOC:
         FOR EACH tt-p-cust-ident WHERE            
            tt-p-cust-ident.cust-code-type   EQ tt-person.document-id  AND
            tt-p-cust-ident.close-date       EQ ?
         BY tt-p-cust-ident.issue DESC:
            LEAVE LASTDOC.
         END.
         IF AVAIL tt-p-cust-ident   THEN
            ASSIGN
               tt-person.document-id         =  tt-p-cust-ident.cust-code-type
               tt-person.document            =  tt-p-cust-ident.cust-code
               mIssue                        =  tt-p-cust-ident.issue
               mKP                           =  tt-p-cust-ident.podrazd$         
               tt-person.Document4Date_vid   =  tt-p-cust-ident.open-date
            .
         IF iMode EQ {&MOD_VIEW} AND
            mKP EQ ? THEN
            mKP = "".
      END.                              
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
&GLOBAL-DEFINE CustIdntSurr    cust-ident.cust-code-type + ',' + ~
                               cust-ident.cust-code      + ',' + ~
                               STRING(cust-ident.cust-type-num)

PROCEDURE PostSetObject :
   DEFINE VAR vRowId        AS ROWID                  NO-UNDO.
   DEFINE VAR vXattrCode    LIKE xattr.xattr-code     NO-UNDO.
   DEFINE VAR vXattrName    LIKE xattr.name           NO-UNDO.
   DEFINE VAR vIsProgrField LIKE xattr.Progress-Field NO-UNDO.
   DEFINE VAR vInt          AS INT64                  NO-UNDO.
   DEFINE VAR vRole         AS CHAR                   NO-UNDO.
   DEFINE VAR vEndDate      AS DATE      NO-UNDO.
   DEFINE VAR vOldEndDate   AS DATE      NO-UNDO.   
   DEFINE VAR vAvtoSrok     AS CHARACTER NO-UNDO.
   DEFINE VAR vYesNo        AS LOGICAL   NO-UNDO.
   DEFINE VAR vMode         AS CHARACTER NO-UNDO.
   DEFINE BUFFER xperson    FOR person.

/* Переписываем из датасета значение person-id в таблицу tt-person. */
   tt-person.person-id = mInstance:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD ("person-id"):BUFFER-VALUE.
   IF work-module EQ 'vok'
   THEN DO:
      FIND FIRST cust-role WHERE cust-role.Class-Code = "КлиентВОК"
                             AND cust-role.cust-cat   = "Ч"
                             AND cust-role.cust-id    = STRING(tt-person.person-id)
                             AND cust-role.file-name  = (IF shMode THEN "branch" ELSE "person")
                             AND cust-role.surrogate  = (IF shMode THEN shFilial ELSE STRING(tt-person.person-id))
      NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-role
      THEN DO:
         CREATE cust-role.
         ASSIGN
            cust-role.Class-Code = "КлиентВОК"
            cust-role.cust-cat   = "Ч"
            cust-role.cust-id    = STRING(tt-person.person-id)
            cust-role.file-name  = (IF shMode THEN "branch" ELSE "person")
            cust-role.surrogate  = (IF shMode THEN shFilial ELSE STRING(tt-person.person-id))
         .
      END.
   END.

   /* Дополнительные роли */
   IF  {assigned mCrCustRole}
   AND mCrCustRole <> "*"
   THEN DO vInt = 1 TO NUM-ENTRIES(mCrCustRole):
      vRole = ENTRY(vInt,mCrCustRole).
      FIND FIRST cust-role WHERE cust-role.Class-Code = vRole
                             AND cust-role.cust-cat   = "Ч"
                             AND cust-role.cust-id    = STRING(tt-person.person-id)
                             AND cust-role.file-name  = (IF shMode THEN "branch" ELSE "person")
                             AND cust-role.surrogate  = (IF shMode THEN shFilial ELSE STRING(tt-person.person-id))
      NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-role THEN DO:
         CREATE cust-role.
         ASSIGN
            cust-role.Class-Code = vRole
            cust-role.cust-cat   = "Ч"
            cust-role.cust-id    = STRING(tt-person.person-id)
            cust-role.file-name  = (IF shMode THEN "branch" ELSE "person")
            cust-role.surrogate  = (IF shMode THEN shFilial ELSE STRING(tt-person.person-id))
         .
         RELEASE cust-role.
      END.
   END.
                        
   /* (поднято из Дойче) простановка начального статуса */
   UpdateSigns("person",
               STRING (tt-person.person-id),
               "status",
               FGetSetting("ПовтВводКл", "НачСтатусЧ", ?),
               ?).

   UpdateSigns("person",
               STRING (tt-person.person-id),
               mAdrCntXattr,
               vAdrCntry,
               ?).

   /* Если мы вводим инкассатора, то поднимаем флаг. */
   IF GetSysConf("DRIncass") EQ "YES"
   THEN DO:
      UpdateSigns("person", 
                  STRING (tt-person.person-id), 
                  "incass", 
                  "Да",
                  ?).
   END.

   IF CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   EQ "Ч"
                 AND cust-role.cust-id    EQ STRING(tt-person.person-id)
                 AND cust-role.Class-Code EQ "ImaginClient"
                 AND cust-role.file-name  EQ "branch"                                  
                 AND cust-role.surrogate  NE shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) AND
   NOT CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   EQ "Ч"
                 AND cust-role.cust-id    EQ STRING(tt-person.person-id)
                 AND cust-role.Class-Code EQ "ImaginClient"
                 AND cust-role.file-name  EQ "branch"                                  
                 AND cust-role.surrogate  EQ shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) THEN DO:

      pick-value = "no".
      RUN Fill-SysMes ("", "", "4", "Создать роль ImaginClient в текущем филиале?").
      IF pick-value EQ "yes" THEN
      RUN SetClientRole IN h_cust (
         mInstance:DEFAULT-BUFFER-HANDLE, "Ч", mBankClient)NO-ERROR.
   END.
   ELSE  /* Устанавливаем роль клиента. */
   RUN SetClientRole IN h_cust (
      mInstance:DEFAULT-BUFFER-HANDLE, "Ч", mBankClient)NO-ERROR.

   vRowId = TO-ROWID(GetInstanceProp(mInstance,"__rowid")).
   FIND FIRST xperson WHERE ROWID(xperson) = vRowId NO-LOCK NO-ERROR.
   IF AVAIL xperson THEN DO:
       
      /* Проверка основных реквизитов */
      RUN GetFirstUnassignedFieldManByRole IN h_cust ("person",
                                                      (BUFFER xperson:HANDLE),
                                                      "main",
                                                      OUTPUT vXattrCode,
                                                      OUTPUT vXattrName).
      IF {assigned vXattrCode} THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "0",
               "Не заполнен основной обязательный реквизит """ + vXattrCode +
               """ (" + vXattrName + ")."
               ).
            RETURN ERROR.
      END.
  
   END.
   
   {chk_frm_mand_adr.i
      &cust-type = "'Ч'"
      &cust-id   = "tt-person.person-id"
   }

   FOR LAST bcident WHERE bcident.cust-cat       = "Ч"
                      AND bcident.cust-id        = tt-person.person-id
                      AND bcident.cust-code-type = "АдрФакт"
   NO-LOCK BY bcident.open-date:
      UpdateSigns("person",
                  STRING(tt-person.person-id),
                  "PlaceOfStay",
                  bcident.issue,
                  ?).
   END.
   FOR LAST bcident WHERE bcident.cust-cat       = "Ч"
                      AND bcident.cust-id        = tt-person.person-id
                      AND bcident.cust-code-type = "АдрФакт"
   NO-LOCK BY bcident.open-date:
      UpdateSigns("person",
                  STRING(tt-person.person-id),
                  "PlaceOfStay",
                  bcident.issue,
                  ?).
   END.

   FOR EACH code WHERE code.class  EQ "КодДокум"
                   AND code.parent EQ "КодДокум"
   NO-LOCK:
      vAvtoSrok = GetXattrValueEx("code",code.class + "," + code.code,"АвтоСрок","").

      FOR EACH cust-ident USE-INDEX cust
                       WHERE cust-ident.cust-cat        EQ "Ч"
                         AND cust-ident.cust-id         EQ tt-person.person-id
                         AND cust-ident.class-code      EQ "p-cust-ident"
                         AND cust-ident.cust-code-type  EQ code.code
                         AND cust-ident.close-date      EQ ?
      NO-LOCK BY cust-ident.open-date DESCENDING:
         LEAVE.
      END.   
      IF NOT AVAIL cust-ident THEN NEXT.
      vEndDate = ?.
      vOldEndDate = ?.
      IF {assigned vAvtoSrok} THEN DO:
         IF tt-person.birthday NE ? THEN DO:
            vEndDate = CalcSrokForDocument(vAvtoSrok,cust-ident.open-date,tt-person.birthday).
            vOldEndDate = DATE(GetXattrValueEx("cust-ident",{&CustIdntSurr},"end-date","")).

            IF vEndDate NE ? AND vEndDate NE vOldEndDate THEN DO:
               RUN Fill-SysMes IN h_tmess ("","","3",
                                           "Для документа [" + cust-ident.cust-code-type + "]~n" +
                                           "скорректировать дату окончания на " + STRING(vEndDate,"99/99/9999") + "?" + "|Да,Нет").
               IF pick-value EQ "1" THEN
               UpdateSigns("p-cust-ident", 
                           {&CustIdntSurr},
                           "end-date",
                           STRING(vEndDate),
                           ?).               
               vOldEndDate = vEndDate.
            END.
            
            IF vOldEndDate LT TODAY AND FGetSetting("СтандТр","СрокДокум","Нет") NE "Нет" THEN DO:
               RUN Fill-SysMes IN h_tmess ("","comm15",IF FGetSetting("СтандТр","СрокДокум","Нет") = "Да" THEN "-1" ELSE "",
                                           "%s=" + tt-person.name-last + " " + tt-person.first-names +
                                           "%s=" + cust-ident.cust-code-type + "%s=" + STRING(vOldEndDate) + "|Продолжить,Отменить").
               IF pick-value EQ "yes" OR pick-value EQ "2" THEN
               RETURN ERROR "Документ не введен".            
            END.
            
         END.
      END.
   END.

   IF LOGICAL(fGetSetting("CLI-EXPORT", "Person", "NO")) EQ YES THEN
      RUN createxmlpers.p(tt-person.person-id) .

   /* проверка по стоп листам */
   IF  ChkClientByStopList ('person' ,
                            STRING(tt-person.person-id),
                            tt-person.name-last + ' ' + tt-person.first-name,
                            "",
                            tt-person.inn,
                            tt-person.document,
                            "КонтрВводРед_Кл",
                            "CLNT")
   THEN DO:
      {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
      RUN PrintClientSLRep.
      {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"} 
      vMode =   IF iMode EQ {&MOD_ADD} THEN "create" ELSE "edit".      
      IF NOT StopListAction("person",STRING(tt-person.person-id), vMode) THEN
         RETURN ERROR.   
   END.

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   IF FGetSetting("ОбменФМС","ФМСРед_Кл","") EQ "Да"
   THEN DO:
      {cl-fmschk.i
         "'person'"
         "tt-person.person-id"
         "REPLACE(tt-person.document-id,'__FORM~001','')"
         "tt-person.document"
         "GetXattrValueEX('person', STRING(tt-person.person-id), 'Document4Date_vid', '')"
         "tt-person.name-last + ' ' + tt-person.first-names"
      }
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateObjectLocal TERMINAL-SIMULATION
PROCEDURE ValidateObjectLocal :
   DEFINE VAR vRowId        AS ROWID                  NO-UNDO. 
   DEFINE VAR vRecId        AS RECID                  NO-UNDO. /* Для вызова метода CHKUPD. */
   DEF    VAR vRoleKontr    AS LOG                    NO-UNDO. /* Нужно контролировать роли субъекта */
   
   vRowId = TO-ROWID(GetInstanceProp(mInstance,"__rowid")).
   /* Запуск метода chkupd. */
   vRecId = Rowid2Recid ("person", vRowId).

   vRoleKontr = fGetSetting("КонтрКлВвод", "КонтрРоли", "") EQ "Да".
   IF     vRoleKontr 
      AND iMode EQ {&MOD_ADD}
   THEN
   DO:
      RUN chk-role.p("person", vRecId).
      IF RETURN-VALUE NE "" THEN
         RETURN ERROR RETURN-VALUE.

   END. /* IF vRoleKontr THEN */

   RUN RunClassMethod IN h_xclass (
      "person",
      "chkupd",
      "",
      "",
      "pers-req",
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
/* $LINTDATE='09/02/2015 17:36:25.215+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='f-person.p' */
/*prosigneiKLr2YcK6IHN7THvxzJmQ*/