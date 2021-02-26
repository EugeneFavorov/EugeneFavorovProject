&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-person NO-UNDO LIKE person
       FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
       FIELD strahnomer$ AS CHARACTER /* СтрахНомер */
       FIELD koliwzd$ AS INT64 /* КолИжд */
       FIELD sempol$ AS CHARACTER /* СемПол */
       FIELD kodsubckki$ AS CHARACTER /* КодСубЦККИ */
       FIELD rabota_viddewat$ AS CHARACTER /* Работа_ВидДеят */
       FIELD rabotanawim$ AS DATE /* РаботаНайм */
       FIELD rabotanawimkon$ AS DATE /* РаботаНаймКон */
       FIELD rabota_polowzenie$ AS CHARACTER /* Работа_Положение */
       FIELD rabota_zanwatostw#$ AS CHARACTER /* Работа_Занятость */
       FIELD umer$ AS LOGICAL /* Умер */
       FIELD izmfipredfam$ AS CHARACTER /* ИзмФИПредФам */
       FIELD izmfipredimwa$ AS CHARACTER /* ИзмФИПредИмя */
       FIELD izmfidata$ AS DATE /* ИзмФИДата */
       FIELD datapropiski$ AS DATE /* ДатаПрописки */
       FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
       FIELD nbki_stprowz$ AS CHARACTER /* НБКИ_СтПрож */
       FIELD izmdokprednomer$ AS CHARACTER /* ИзмДокПредНомер */
       FIELD izmdokdata$ AS DATE /* ИзмДокДата */
       FIELD rabota_sferadewat$ AS CHARACTER /* Работа_СфераДеят */
       FIELD abwawtik$ AS CHARACTER /* АбЯщик */
       FIELD arhiv$ AS LOGICAL /* Архив */
       FIELD blok$ AS LOGICAL /* Блок */
       FIELD viddewat$ AS CHARACTER /* ВидДеят */
       FIELD vidkli$ AS CHARACTER /* ВидКли */
       FIELD vidsotr$ AS CHARACTER /* ВидСотр */
       FIELD gvk$ AS CHARACTER /* ГВК */
       FIELD grawzd$ AS CHARACTER /* Гражд */
       FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
       FIELD datavpred$ AS CHARACTER /* ДатаВПред */
       FIELD dataokpred$ AS CHARACTER /* ДатаОкПред */
       FIELD dko$ AS DECIMAL /* ДкО */
       FIELD dkowe$ AS DECIMAL /* ДкОЭ */
       FIELD dolwz$ AS CHARACTER /* Долж */
       FIELD dom$ AS CHARACTER /* ДОМ */
       FIELD iin$ AS CHARACTER /* ИИН */
       FIELD indgr$ AS CHARACTER /* ИндГр */
       FIELD iobss$ AS CHARACTER /* ИОБСС */
       FIELD iogbh$ AS CHARACTER /* ИОГБХ */
       FIELD kategfiz$ AS CHARACTER /* КатегФиз */
       FIELD klient$ AS CHARACTER /* Клиент */
       FIELD klientuf$ AS LOGICAL /* КлиентУФ */
       FIELD klreestr$ AS CHARACTER /* КлРеестр */
       FIELD koddokum$ AS CHARACTER /* КодДокум */
       FIELD kodklienta$ AS CHARACTER /* КодКлиента */
       FIELD kodreg$ AS CHARACTER /* КодРег */
       FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
       FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
       FIELD kop$ AS INT64 /* КОП */
       FIELD kopf$ AS INT64 /* КОПФ */
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
       FIELD obrbs$ AS CHARACTER /* ОбрБС */
       FIELD obrgb$ AS CHARACTER /* ОбрГБ */
       FIELD ogrn$ AS CHARACTER /* ОГРН */
       FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
       FIELD okvwed$ AS CHARACTER /* ОКВЭД */
       FIELD orgsvedpred$ AS CHARACTER /* ОргСведПред */
       FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
       FIELD ofwsor$ AS CHARACTER /* Офшор */
       FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
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
       FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
       FIELD sotoper$ AS CHARACTER /* СотОпер */
       FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
       FIELD svedregpred$ AS CHARACTER /* СведРегПред */
       FIELD subw%ekt$ AS CHARACTER /* Субъект */
       FIELD telefon3$ AS CHARACTER /* Телефон3 */
       FIELD ulica$ AS CHARACTER /* Улица */
       FIELD unk$ AS DECIMAL /* УНК */
       FIELD unkg$ AS INT64 /* УНКг */
       FIELD uwcdok$ AS CHARACTER /* УчДок */
       FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
       FIELD uwcdokdata$ AS DATE /* УчДокДата */
       FIELD fiobs$ AS CHARACTER /* ФИОБС */
       FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
       FIELD fiogb$ AS CHARACTER /* ФИОГБ */
       FIELD fioruk$ AS CHARACTER /* ФИОрук */
       FIELD formsobs$ AS CHARACTER /* ФормСобс */
       FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
       FIELD wa$ AS CHARACTER /* Я */
       FIELD Address1Indeks AS INT64 /* Address1Indeks */
       FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
       FIELD Address3Street AS CHARACTER /* Address3Street */
       FIELD Address4Dom AS CHARACTER /* Address4Dom */
       FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
       FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
       FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD branch-id AS CHARACTER /* branch-id */
       FIELD branch-list AS CHARACTER /* branch-list */
       FIELD contr_group AS CHARACTER /* contr_group */
       FIELD contr_type AS CHARACTER /* contr_type */
       FIELD country-id2 AS CHARACTER /* country-id2 */
       FIELD country-id3 AS CHARACTER /* country-id3 */
       FIELD CountryCode AS CHARACTER /* CountryCode */
       FIELD date-export AS CHARACTER /* date-export */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
       FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
       FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD e-mail AS CHARACTER /* e-mail */
       FIELD engl-name AS CHARACTER /* engl-name */
       FIELD Exam AS CHARACTER /* Exam */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD holding-id AS CHARACTER /* holding-id */
       FIELD incass AS LOGICAL /* incass */
       FIELD IndCode AS CHARACTER /* IndCode */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD lat_dop AS CHARACTER /* lat_dop */
       FIELD lat_fam AS CHARACTER /* lat_fam */
       FIELD lat_f_n AS CHARACTER /* lat_f_n */
       FIELD lat_name AS CHARACTER /* lat_name */
       FIELD lat_otch AS CHARACTER /* lat_otch */
       FIELD lat_titul AS CHARACTER /* lat_titul */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD lic-sec AS CHARACTER /* lic-sec */
       FIELD Lic_num AS CHARACTER /* Lic_num */
       FIELD LocCustType AS CHARACTER /* LocCustType */
       FIELD mess AS CHARACTER /* mess */
       FIELD NACE AS CHARACTER /* NACE */
       FIELD Netting AS LOGICAL /* Netting */
       FIELD New$$ AS CHARACTER /* New */
       FIELD New1 AS CHARACTER /* New1 */
       FIELD NoExport AS LOGICAL /* NoExport */
       FIELD num_contr AS INT64 /* num_contr */
       FIELD old-person-id AS CHARACTER /* old-person-id */
       FIELD passw_card AS CHARACTER /* passw_card */
       FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
       FIELD Prim-ID AS CHARACTER /* Prim-ID */
       FIELD RegNum AS CHARACTER /* RegNum */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD Soato AS CHARACTER /* Soato */
       FIELD Svid_num AS CHARACTER /* Svid_num */
       FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
       FIELD Visa AS CHARACTER /* Visa */
       FIELD VisaNum AS CHARACTER /* VisaNum */
       FIELD VisaType AS CHARACTER /* VisaType */
       FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
       FIELD xview-photo AS CHARACTER /* xview-photo */
       FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
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
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: F-PERS-CONT.P
      Comment: <comment>
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
{globals.i}
{intrface.get exch}       /* Инструменты для получения банковских реквизитов. */
{intrface.get cust}       /* Библиотека для работы с клиентами.  */
{intrface.get strng}      /* Инструменты для работы со строками  */

&IF DEFINED(SESSION-REMOTE) =  0 &THEN
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

DEFINE VAR mIsChecking AS LOGICAL NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-fMain tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-person.sotoper$ ~
tt-person.fax tt-person.e-mail 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-person
&Scoped-define QUERY-STRING-fMain FOR EACH tt-person SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-person SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-person


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define ENABLED-TABLES tt-person
&Scoped-define FIRST-ENABLED-TABLE tt-person
&Scoped-Define ENABLED-OBJECTS mPhoneHome1 mPhoneHome2 mPhoneWork mPhoneMob 
&Scoped-Define DISPLAYED-FIELDS tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define DISPLAYED-TABLES tt-person
&Scoped-define FIRST-DISPLAYED-TABLE tt-person
&Scoped-Define DISPLAYED-OBJECTS mPhoneHome1 mPhoneHome2 mPhoneWork ~
mPhoneMob 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 
&Scoped-define List-2 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 
&Scoped-define List-3 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mPhoneHome1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Домашний телефон" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneHome2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Домашний телефон факт" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneMob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Мобильный телефон" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneWork AS CHARACTER FORMAT "X(256)":U 
     LABEL "Рабочий телефон" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-person SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mPhoneHome1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 23 COLON-ALIGNED
          &ELSE AT ROW 1 COL 23 COLON-ALIGNED &ENDIF
     mPhoneHome2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 23 COLON-ALIGNED
          &ELSE AT ROW 2 COL 23 COLON-ALIGNED &ENDIF
     mPhoneWork
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 23 COLON-ALIGNED
          &ELSE AT ROW 3 COL 23 COLON-ALIGNED &ENDIF
     tt-person.sotoper$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 23 COLON-ALIGNED
          &ELSE AT ROW 4 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
     mPhoneMob
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 23 COLON-ALIGNED
          &ELSE AT ROW 5 COL 23 COLON-ALIGNED &ENDIF
     tt-person.fax
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 23 COLON-ALIGNED
          &ELSE AT ROW 6 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
     tt-person.e-mail
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 23 COLON-ALIGNED
          &ELSE AT ROW 7 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 1
         SIZE 49 BY 9
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-person T "?" NO-UNDO bisquit person
      ADDITIONAL-FIELDS:
          FIELD okato_302$ AS CHARACTER /* ОКАТО_302 */
          FIELD strahnomer$ AS CHARACTER /* СтрахНомер */
          FIELD koliwzd$ AS INT64 /* КолИжд */
          FIELD sempol$ AS CHARACTER /* СемПол */
          FIELD kodsubckki$ AS CHARACTER /* КодСубЦККИ */
          FIELD rabota_viddewat$ AS CHARACTER /* Работа_ВидДеят */
          FIELD rabotanawim$ AS DATE /* РаботаНайм */
          FIELD rabotanawimkon$ AS DATE /* РаботаНаймКон */
          FIELD rabota_polowzenie$ AS CHARACTER /* Работа_Положение */
          FIELD rabota_zanwatostw#$ AS CHARACTER /* Работа_Занятость */
          FIELD umer$ AS LOGICAL /* Умер */
          FIELD izmfipredfam$ AS CHARACTER /* ИзмФИПредФам */
          FIELD izmfipredimwa$ AS CHARACTER /* ИзмФИПредИмя */
          FIELD izmfidata$ AS DATE /* ИзмФИДата */
          FIELD datapropiski$ AS DATE /* ДатаПрописки */
          FIELD nbki_streg$ AS CHARACTER /* НБКИ_СтРег */
          FIELD nbki_stprowz$ AS CHARACTER /* НБКИ_СтПрож */
          FIELD izmdokprednomer$ AS CHARACTER /* ИзмДокПредНомер */
          FIELD izmdokdata$ AS DATE /* ИзмДокДата */
          FIELD rabota_sferadewat$ AS CHARACTER /* Работа_СфераДеят */
          FIELD abwawtik$ AS CHARACTER /* АбЯщик */
          FIELD arhiv$ AS LOGICAL /* Архив */
          FIELD blok$ AS LOGICAL /* Блок */
          FIELD viddewat$ AS CHARACTER /* ВидДеят */
          FIELD vidkli$ AS CHARACTER /* ВидКли */
          FIELD vidsotr$ AS CHARACTER /* ВидСотр */
          FIELD gvk$ AS CHARACTER /* ГВК */
          FIELD grawzd$ AS CHARACTER /* Гражд */
          FIELD gruppakl$ AS CHARACTER /* ГруппаКл */
          FIELD datavpred$ AS CHARACTER /* ДатаВПред */
          FIELD dataokpred$ AS CHARACTER /* ДатаОкПред */
          FIELD dko$ AS DECIMAL /* ДкО */
          FIELD dkowe$ AS DECIMAL /* ДкОЭ */
          FIELD dolwz$ AS CHARACTER /* Долж */
          FIELD dom$ AS CHARACTER /* ДОМ */
          FIELD iin$ AS CHARACTER /* ИИН */
          FIELD indgr$ AS CHARACTER /* ИндГр */
          FIELD iobss$ AS CHARACTER /* ИОБСС */
          FIELD iogbh$ AS CHARACTER /* ИОГБХ */
          FIELD kategfiz$ AS CHARACTER /* КатегФиз */
          FIELD klient$ AS CHARACTER /* Клиент */
          FIELD klientuf$ AS LOGICAL /* КлиентУФ */
          FIELD klreestr$ AS CHARACTER /* КлРеестр */
          FIELD koddokum$ AS CHARACTER /* КодДокум */
          FIELD kodklienta$ AS CHARACTER /* КодКлиента */
          FIELD kodreg$ AS CHARACTER /* КодРег */
          FIELD kodreggni$ AS CHARACTER /* КодРегГНИ */
          FIELD kodyadresa$ AS CHARACTER /* КодыАдреса */
          FIELD kop$ AS INT64 /* КОП */
          FIELD kopf$ AS INT64 /* КОПФ */
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
          FIELD obrbs$ AS CHARACTER /* ОбрБС */
          FIELD obrgb$ AS CHARACTER /* ОбрГБ */
          FIELD ogrn$ AS CHARACTER /* ОГРН */
          FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
          FIELD okvwed$ AS CHARACTER /* ОКВЭД */
          FIELD orgsvedpred$ AS CHARACTER /* ОргСведПред */
          FIELD osnvidydewat$ AS CHARACTER /* ОснВидыДеят */
          FIELD ofwsor$ AS CHARACTER /* Офшор */
          FIELD ocenkariska$ AS CHARACTER /* ОценкаРиска */
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
          FIELD riskotmyv$ AS CHARACTER /* РискОтмыв */
          FIELD sotoper$ AS CHARACTER /* СотОпер */
          FIELD svedvygdrlica$ AS CHARACTER /* СведВыгДрЛица */
          FIELD svedregpred$ AS CHARACTER /* СведРегПред */
          FIELD subw%ekt$ AS CHARACTER /* Субъект */
          FIELD telefon3$ AS CHARACTER /* Телефон3 */
          FIELD ulica$ AS CHARACTER /* Улица */
          FIELD unk$ AS DECIMAL /* УНК */
          FIELD unkg$ AS INT64 /* УНКг */
          FIELD uwcdok$ AS CHARACTER /* УчДок */
          FIELD uwcdokgr$ AS CHARACTER /* УчДокГр */
          FIELD uwcdokdata$ AS DATE /* УчДокДата */
          FIELD fiobs$ AS CHARACTER /* ФИОБС */
          FIELD fiobuhg$ AS CHARACTER /* ФИОбухг */
          FIELD fiogb$ AS CHARACTER /* ФИОГБ */
          FIELD fioruk$ AS CHARACTER /* ФИОрук */
          FIELD formsobs$ AS CHARACTER /* ФормСобс */
          FIELD formsobs_118$ AS CHARACTER /* ФормСобс_118 */
          FIELD wa$ AS CHARACTER /* Я */
          FIELD Address1Indeks AS INT64 /* Address1Indeks */
          FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
          FIELD Address3Street AS CHARACTER /* Address3Street */
          FIELD Address4Dom AS CHARACTER /* Address4Dom */
          FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
          FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
          FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD branch-id AS CHARACTER /* branch-id */
          FIELD branch-list AS CHARACTER /* branch-list */
          FIELD contr_group AS CHARACTER /* contr_group */
          FIELD contr_type AS CHARACTER /* contr_type */
          FIELD country-id2 AS CHARACTER /* country-id2 */
          FIELD country-id3 AS CHARACTER /* country-id3 */
          FIELD CountryCode AS CHARACTER /* CountryCode */
          FIELD date-export AS CHARACTER /* date-export */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
          FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
          FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD e-mail AS CHARACTER /* e-mail */
          FIELD engl-name AS CHARACTER /* engl-name */
          FIELD Exam AS CHARACTER /* Exam */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD holding-id AS CHARACTER /* holding-id */
          FIELD incass AS LOGICAL /* incass */
          FIELD IndCode AS CHARACTER /* IndCode */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD lat_dop AS CHARACTER /* lat_dop */
          FIELD lat_fam AS CHARACTER /* lat_fam */
          FIELD lat_f_n AS CHARACTER /* lat_f_n */
          FIELD lat_name AS CHARACTER /* lat_name */
          FIELD lat_otch AS CHARACTER /* lat_otch */
          FIELD lat_titul AS CHARACTER /* lat_titul */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD lic-sec AS CHARACTER /* lic-sec */
          FIELD Lic_num AS CHARACTER /* Lic_num */
          FIELD LocCustType AS CHARACTER /* LocCustType */
          FIELD mess AS CHARACTER /* mess */
          FIELD NACE AS CHARACTER /* NACE */
          FIELD Netting AS LOGICAL /* Netting */
          FIELD New$$ AS CHARACTER /* New */
          FIELD New1 AS CHARACTER /* New1 */
          FIELD NoExport AS LOGICAL /* NoExport */
          FIELD num_contr AS INT64 /* num_contr */
          FIELD old-person-id AS CHARACTER /* old-person-id */
          FIELD passw_card AS CHARACTER /* passw_card */
          FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
          FIELD Prim-ID AS CHARACTER /* Prim-ID */
          FIELD RegNum AS CHARACTER /* RegNum */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD Soato AS CHARACTER /* Soato */
          FIELD Svid_num AS CHARACTER /* Svid_num */
          FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
          FIELD Visa AS CHARACTER /* Visa */
          FIELD VisaNum AS CHARACTER /* VisaNum */
          FIELD VisaType AS CHARACTER /* VisaType */
          FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
          FIELD xview-photo AS CHARACTER /* xview-photo */
          FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
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
/* SETTINGS FOR FILL-IN tt-person.e-mail IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.fax IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneHome1 IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneHome2 IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneMob IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneWork IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.sotoper$ IN FRAME fMain
   1 2 3                                                                */
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(SESSION-REMOTE) =  0 &THEN
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

   /* Вх. параметры */
   IF NUM-ENTRIES(iInstanceList,{&PARAM_DELIM}) > 1
      THEN mIsChecking = LOGICAL(ENTRY(2, iInstanceList,{&PARAM_DELIM})).

   /* Commented by KSV: Инициализация системных сообщений */
   RUN Init-SysMes("","","").

   RUN MainAction NO-ERROR.
   IF ERROR-STATUS:ERROR
      THEN LEAVE MAIN-BLOCK.

   IF mIsChecking THEN APPLY "GO" TO FRAME {&MAIN-FRAME}.
   IF ERROR-STATUS:ERROR
      THEN mIsChecking = NO.
   IF NOT THIS-PROCEDURE:PERSISTENT AND NOT mIsChecking THEN
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
  DISPLAY mPhoneHome1 mPhoneHome2 mPhoneWork mPhoneMob 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-person THEN 
    DISPLAY tt-person.sotoper$ tt-person.fax tt-person.e-mail 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ mPhoneMob 
         tt-person.fax tt-person.e-mail 
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
   mSwitchF9 = NO.
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalPutTitle TERMINAL-SIMULATION 
PROCEDURE LocalPutTitle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
      mBT_Title         = "[ КОНТАКТЫ ФЛ - " + LC(ENTRY(iMode,{&MOD_LIST})) + " ]"
      FRAME fMain:TITLE = mBT_Title
   .
   RETURN ERROR.
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
   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-person.phone[1] = "" WHEN tt-person.phone[1] = ?
         tt-person.phone[2] = "" WHEN tt-person.phone[2] = ?
         tt-person.phone[1] = SetEntries(1,tt-person.phone[1],",",TRIM(REPLACE(mPhoneHome1:SCREEN-VALUE,","," ")))
         tt-person.phone[1] = SetEntries(2,tt-person.phone[1],",",TRIM(REPLACE(mPhoneHome2:SCREEN-VALUE,","," ")))
         tt-person.phone[2] = SetEntries(1,tt-person.phone[2],",",TRIM(REPLACE(mPhoneWork:SCREEN-VALUE,","," ")))
         tt-person.phone[2] = SetEntries(2,tt-person.phone[2],",",TRIM(REPLACE(mPhoneMob:SCREEN-VALUE,","," ")))
      .
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_GO TERMINAL-SIMULATION 
PROCEDURE Local_GO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR vTestPhone AS CHAR          NO-UNDO.
   DEFINE VAR vPrefixes  AS CHARACTER     NO-UNDO.
   DO WITH FRAME {&MAIN-FRAME}:
      IF TRIM(mPhoneHome1:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "Номер телефона может содержать только цифры и символы: - ( )").
         APPLY "ENTRY" TO mPhoneHome1.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneHome2:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "Номер телефона может содержать только цифры и символы: - ( )").
         APPLY "ENTRY" TO mPhoneHome2.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneWork:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "Номер телефона может содержать только цифры и символы: - ( )").
         APPLY "ENTRY" TO mPhoneWork.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneMob:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "Номер телефона может содержать только цифры и символы: - ( )").
         APPLY "ENTRY" TO mPhoneMob.
         RETURN ERROR.
      END.
      vTestPhone = mPhoneHome1:SCREEN-VALUE + mPhoneHome2:SCREEN-VALUE + mPhoneWork:SCREEN-VALUE + mPhoneMob:SCREEN-VALUE.
      RUN CheckFullFieldValue (iClass, "phone", iSurrogate, TRIM(vTestPhone)) NO-ERROR.
      IF {assigned RETURN-VALUE} THEN DO:
         IF INDEX(RETURN-VALUE,"обязат") > 0
            THEN RUN Fill-SysMes ("", "", "0","Необходимо заполнить один из телефонов (домашний, рабочий или мобильный)").
         ELSE RUN Fill-SysMes ("", "", "0", RETURN-VALUE).
         RETURN ERROR.
      END.

      IF FGetSetting("Проверки","ПровТелефон","") = "Да" THEN
      IF vTestPhone = "" THEN
         RUN Fill-SysMes ("", "", "0","Телефон клиента не указан").


      IF     {assigned "mPhoneMob:SCREEN-VALUE"} 
         AND {assigned "tt-person.sotoper$:SCREEN-VALUE"}
         AND tt-person.sotoper$:SCREEN-VALUE <> "?"
         AND SUBSTRING(mPhoneMob:SCREEN-VALUE, 1, 1) <> "7" THEN
      DO:      
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "0",
                                     'Номер мобильного телефона должен начинаться с цифры "7".').
         RETURN ERROR.
      END.
      IF {assigned "tt-person.sotoper$:SCREEN-VALUE"} AND
         tt-person.sotoper$:SCREEN-VALUE <> "?" THEN
      DO:
         FOR EACH code WHERE CODE.CLASS   =  "СотОпер"
                         AND CODE.PARENT  =  tt-person.sotoper$:SCREEN-VALUE
            NO-LOCK:
            {additem.i vPrefixes CODE.CODE}
         END.
         
         IF NOT CAN-DO(vPrefixes, SUBSTRING(mPhoneMob:SCREEN-VALUE, 2, 3)) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("",
                                        "",
                                        "0",
                                        'Код мобильного оператора <' 
                                           + SUBSTRING(mPhoneMob:SCREEN-VALUE, 2, 3) 
                                           + '> отсутствует в списке разрешенных для оператора <' 
                                           + tt-person.sotoper$:SCREEN-VALUE + '>.').
            RETURN ERROR.
         END.
      END.
   END.
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
   IF tt-person.sotoper$ = ? THEN 
      tt-person.sotoper$ = "".
   IF tt-person.fax = ? THEN 
      tt-person.fax = "".
   IF tt-person.e-mail = ? THEN 
      tt-person.e-mail = "".

   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Подсветка полей из LIST-5 (настроить для себя )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

   /* Commented by KSV: Показываем экранную форму */
   STATUS DEFAULT "".
&IF DEFINED(SESSION-REMOTE) =  0 &THEN
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
   IF AVAIL tt-person THEN DO:
      mPhoneHome1 = GetEntries(1, tt-person.Phone[1], ",", "").
      mPhoneHome2 = GetEntries(2, tt-person.Phone[1], ",", "").
      mPhoneWork  = GetEntries(1, tt-person.Phone[2], ",", "").
      mPhoneMob   = GetEntries(2, tt-person.Phone[2], ",", "").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

