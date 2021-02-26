/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: XATTR-ED.P
      Comment: Ввод расширенных реквизитов
   Parameters: in-class-code  - класс объекта
               in-surrogate   - суррогат объекта
               in-title       - заголовок формы
               in-create      - режим работы
               level          - Y-координата формы
      Created: 11/09/97 Serge
-------------------------------------------------------------------------------
     Modified: 11/09/1997 Serge  -
     Modified: 01/10/1997 eagle  -
     Modified: 23/02/1998 Olenka - проверка class.progress-table
     Modified: 13/05/1998 mkv    - если объявлен домен, являющийся классификатором
                                   и поле code.misc[3] не пусто, то процедура берется
                                   из этого поля, иначе pclass
     Modified: 19/01/1999 Serge  - сделал возможность передачи in-surrogate в
                                   процедуру валидации через параметр "id"
                                   сортировка по наименованию
                                   temp-table - shared
     Modified: 12/03/1999 Dima   - XAttrList теперь содержит список Xattr'ов,
                                   которые могут меняться самим _User'ом.
     Modified: 09/04/1999 eagle  - вывод Xattr-code,
     Modified: 12/07/2001 yakv   - global shared XATTR_Params = информация о
                                   редактируемой записи (class,surrogate)
-----------------------------------------------------------------------------
     Modified: 05.06.02 LER 5766 - вызов броузеров _CODE_ {call-brc.i}
     Modified: 24.07.02 GunK     - xattr-value.mandatory вместо xattr.mandatory
     Modified: 24.10.02 ler 8029 - унификакция выбора и валидации значений доп.реквизита,
                                   выбор и валидация начального значения доп.реквизита (xattr.initial).
     Modified: 18.11.02 LER 11551 Унификация обработки реквизитов для основной и альтернативной форм.
                                  В альтернативных формах просмотра теперь выполняется проверка обязательности
                                  доп. реквизитов (для обязательных доп. реквизитов поле не может оставаться пустым).
                                  Значение рекв. по классиф в сотв. с domain-code не проверяется при пустом знач.
     Modified: 06.04.04 ligp    -   у класса _user появился подкласс, которому тоже нельзя менять 
                                   самому себе доп. реквизиты (заявка 24593)
     Modified: 23/05/2004 Om  Доработка:
                                 1. При входе в режиме "создание" отражать ДР:
                                 - присвоенные,
                                 - обязательные,
                                 - все остальные, у которых начальное значение
                                   отлично от пустой строки.
                                 2. Перечитывать ДР, только после редактирования.
     Modified: 23/05/2004 Om  Ошибка:
                                 Устранение ошибки "не найден запись xattr-value",
                                 при входе в пустой брауз (режим редактирования).
    Modified: 19/11/2004 Om  Доработка:
                              1. При входе в режиме "редактирование",
                                 неверно отображались названия клавишь.
                              2. Все сообщения реализованы через службу сообщений.
                              3. При отмене редактирования не производилось
                                 обновление экрана.
    Modified: 10/02/2005 Om  Доработка:
                              Пользователю потребовалось ввести реквизит
                              форматом x(3000). Ограничение поля было 655.
   Modified: 09/03/2005 Om  Доработка.
                        Подключен механиз инициализации ДР.
   Modified: 01/04/2005 Xaro Доработка.
                        Подключен вызов метода проверки обьекта целиком
   Modified: 10/08/2005 Om  Доработка.
                        Отключен механиз проверки объекта целиком до реализации валидатора.
   Modified: 23/09/2005 Om  Доработка.
                        Изменение названия зкладки фильтра.
   Modified: 04/05/2006 Shib  Заявка 0058996.
                        Добавлена проверка на наличие права редактирования доп.реквизитов.    
   Modified: 31/10/2006 Ariz Доработка (0055955)
                        Добавлена обработка метода GetXValDesc для получения связанной
                        информации по значению допреквизита.
                        Возможность указать на допреквизите:
                           метод GetXValDesc - собственный метод получения описания значения ДР,
                        выводимого в полях "Наим.парам"/"Знач.парам".
                        Процедура, заданная в этом методе, должна возвращать в pick-value
                        "Наим.парам"/"Знач.парам" с разделителем CHR(2) ("Наим.парам" + CHR(2) + "Знач.парам")
                        или просто pick-value = "Наим.парам".
     Modified: 17.07.2007 20:43 KSV      (0078824) Адаптирован для Биссмарт
     Modified: 30.08.2007 16:39 Daru 
     Modified: 10.09.2007 10:56 KSV      (0080717) Исправлена ошибка сохранения
                                         доп. реков пользователя
     Modified: 12.02.2009 17:16 KSV      (0106088) незнач. исправление
     Modified: 03.03.2009 12:55 ariz     Темпорированные реквизиты подсвечиваются
                                         цветом заданным НП TempXAttrColor,
                                         если он пуст, не подсвечиваются.
     Modified: 15.04.2009 20:03 KSV      (0108589) добавлена поддержка темпор.
                                         допреков в QBIS
     Modified: 07.07.2009 19:59 ariz     0128939: В стандарт 0128212
     Modified: 11.09.2009 18:27:46 ksv   (0111396) Поддержка группировки в QBIS
     Modified: 25.03.2010 16:53 ksv      (0110628) + сброс семафора CloseOnGo     
     Modified: 01.04.2010 16:12 solm     (0125834) + раскраска темпорированных
                                          доп. реквизитов для QBIS          
*/
/******************************************************************************/

DEF INPUT PARAM in-class-code LIKE class.class-code NO-UNDO.
DEF INPUT PARAM in-surrogate  LIKE signs.surrogate  NO-UNDO.
DEF INPUT PARAM in-title      AS CHAR NO-UNDO.
DEF INPUT PARAM in-create     AS CHAR NO-UNDO.
DEF INPUT PARAM level         AS INT64  NO-UNDO.

{globals.i}             /* Глобальные переменные сессии. */
{form.def}              /* Константы пользовательского интерфейса*/
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{usform.i}              /* Процедуры для выбора значений при работе в extedit.p */

                        /* Инициализация процесса протоколирования. */
RUN Init-SysMes ("", "", "").
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */
{intrface.get data}     /* Библиотека функций, работающих с отвязанным набором данных
                           (Instance) на основе информации, хранящейся в метасхеме. */
{intrface.get hist} 
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */
{xattr-ed.def NEW}      /* Таблица для отображения записей. */
{userconf.def NEW}      /* Ключ для поиска настройки фильтра. */
                        /* Формирование ключа для загрузки фильтров. */
&GLOBAL-DEFINE no_userconf_get   YES

DEFINE VARIABLE params_   AS CHARACTER NO-UNDO.
DEF VAR proc-name         AS CHAR  NO-UNDO.    /* Имя процедуры. */
DEF VAR vProcNameLook     AS CHAR NO-UNDO.     /*процедура просмотра*/
DEF VAR vParamLook        AS CHAR NO-UNDO.     /*параметры процедуры просмотра*/
DEFINE VARIABLE vNoCheck  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE vTempControl  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vChoice   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vPrevVal  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mStatusMandatFld AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mStatusEditFld AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vStatus     AS CHARACTER NO-UNDO.

DEF VAR vSignsDt    AS DATE   NO-UNDO. /* Дата, на которую определяем значение ДР. */
   vSignsDt = IF gend-date NE ? THEN gend-date ELSE gend-hdate.
   
/* Объявление подписки на методы */
SUBSCRIBE TO "GetTTXattrValue" ANYWHERE.

SUBSCRIBE TO "SetTTXattrValue" ANYWHERE.

/* Устанавливаем семафор CloseOnGo в значение YES, которое позволит всем браузерам
** вызывающимся отсюда, закрываться по событию GO, при условии, что это конечно
** в них предусмотрено изначально */
RUN SetCloseOnGoSemaphore IN h_base ( THIS-PROCEDURE, YES ).
 
/* Доп. настройка фильтра код метода browse класса объекта */
RUN GetClassMethod IN h_xclass (
    in-class-code, /* Класс объекта. */               
    "browse",    /* Код метода. */                  
    "","",
    OUTPUT proc-name,
    OUTPUT params_). 
/* Обработка ошибки. */
IF    proc-name EQ ? 
   OR proc-name EQ '""' 
   OR proc-name EQ "''" 
   THEN 
      user-config-info = "XATTR-ED.P," + in-class-code + ",?,". 
   ELSE 
      user-config-info = "XATTR-ED.P," + proc-name + ",?,".

{flt-file.i NEW}        /* Определение структуры динамического фильтра. */


DEF VAR list-class     AS CHAR NO-UNDO. /* Необходим flt-file.end. */
DEF VAR num-class      AS INT64  NO-UNDO. /* Счетчик. */
                        /* Определяем поля для фильтрации. */
{flt-file.add
   &cat     = 1
   &labelt  = "('Дополнительные реквизиты')"
   &tablef  = in-class-code
   &include = "'1,IsEndDte,2,sc-1,Name'"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'IsEndDte'"
   &a-code        = "'IsEndDte'"
   &a-datatype    = "'LOGICAL'"
   &a-label       = "'Показать:'"
   &a-help        = "'F1 - выберите режим отображения'"
   &a-format      = "'все реквизиты/присвоенные реквизиты'"
   &a-initial     = "'NO'"
   &a-HotKey      = "KEY-CODE ('Ctrl-F3')"
   &a-HotSet      = "YES"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'sc-1'"
   &a-code        = "'sc-1'"
   &a-multi       = "YES"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'Name'"
   &a-code        = "'Name'"
   &a-datatype    = "'CHARACTER'"
   &a-label       = "'Название реквизита:'"
   &a-help        = "'Название реквизита.'"
   &a-procename   = "'no-proc'"
   &a-param       = "'1'"
}
{emptyfld.atr 1}
{emptyfld.atr 2}
{flt-file.end}          /* Окончание формирования структуры фильтра. */

&GLOBAL-DEFINE XattrWhere                                            ~
   WHERE                                                             ~
           (IF GetFltVal ("sc-1")   EQ "*"                           ~
               THEN YES                                              ~
               ELSE CAN-DO (GetFltVal ("sc-1"), xattr-value.code))   ~
      AND  (IF GetFltVal ("name")   EQ "*"                           ~
               THEN YES                                              ~
               ELSE CAN-DO (GetFltVal ("name"), xattr-value.name))   ~
      AND  (IF vall                                                  ~
               THEN YES                                              ~
               ELSE (   xattr-value.code-value GT ''                 ~
                     OR xattr-value.since NE ?                       ~
                     OR xattr-value.mandatory                        ~
                     OR xattr-value.lastattr                         ~
                     OR (mByGroup                                    ~
                         AND xattr-value.code-value  EQ ""           ~
                         AND xattr-value.CLASS       EQ ""           ~
                         AND xattr-value.xattr-group NE "") ))       ~
      AND  (IF mGroupList EQ "*"                                     ~
               THEN YES                                              ~
               ELSE xattr-value.xattr-group EQ mGroupList)

&GLOBAL-DEFINE ByGroupSort BY xattr-value.xattr-group BY xattr-value.NAME

&GLOBAL-DEFINE BTNF9    "F9-редактировать"
&GLOBAL-DEFINE BTNESC   "ESC - закончить ред-ние"

DEF NEW GLOBAL SHARED VAR clip-buffer AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE browse-height 9
&GLOBAL-DEFINE mainmess "F6-фильтр"

DEF VAR mWhereSurr    AS CHAR  NO-UNDO.
DEF VAR mWhereSurrOld AS CHAR  NO-UNDO.
DEF VAR mTMP          AS CHAR  NO-UNDO.

mTMP = GetNewSurrogate(GetXclassProgress(in-class-code),
                       in-surrogate,
                       OUTPUT mWhereSurr,
                       OUTPUT mWhereSurrOld).
IF {assigned mTMP} THEN
   in-surrogate = mTMP.

level = 4.

DEF NEW GLOBAL SHARED VAR XATTR_Params AS CHAR INIT "" NO-UNDO.
ASSIGN XATTR_Params = in-class-code + "," + in-surrogate.

/* переменные  */    /* DEF VAR tmp_log AS log NO-UNDO. */
DEF VAR any-entered AS LOG    NO-UNDO. /* Изменено ли что-нибудь. */
DEF VAR vparam      AS CHAR   NO-UNDO.
DEF VAR vall        AS LOG    NO-UNDO.
DEF VAR num-row     AS INT64  NO-UNDO.
DEF VAR vclass      LIKE class.class-code NO-UNDO.
DEF VAR cn          AS CHAR   NO-UNDO. /* Название класса. */
DEF VAR mXattrCode  AS CHAR   NO-UNDO. /* Для позиционирования на запись. */

DEF VAR vsep        AS CHAR   NO-UNDO FORMAT "x".
DEF VAR vDN         LIKE xattr-value.displ-name. /* AS CHAR FORM "x" NO-UNDO. */
DEF VAR vl          AS LOG    NO-UNDO.
DEF VAR edit-mode   AS log    NO-UNDO.
DEF VAR cclass      LIKE class.class-code NO-UNDO.
DEF VAR vdcolor     AS INT64  NO-UNDO.
DEF VAR mStartMode  AS LOG    NO-UNDO. /* Режим с которым открывается брауз. */
DEF VAR mTempColor  AS CHAR   NO-UNDO. /* Цвет отображения темпорированных ДР */
/* переменные для поиска в редакторе ДР */
DEF VAR vCodeFnd    AS CHAR   NO-UNDO. /* поле Код ДР */
DEF VAR vNameFnd    AS CHAR   NO-UNDO. /* поле Наим. ДР */
DEF VAR vCurrROWID  AS ROWID  NO-UNDO. /* ROWID текущей записи */
DEF VAR mGroupList  AS CHAR   NO-UNDO. /* Список групп реквизитов на этом классе */
DEF VAR mi          AS INT64  NO-UNDO.
DEF VAR mTmpStr     AS CHAR   NO-UNDO.
DEF VAR mByGroup    AS LOG    NO-UNDO. /* отображение по группам/без групп */
DEF VAR vIsCode     AS LOG    NO-UNDO.  
DEF VAR vTable      AS CHAR   NO-UNDO.
DEF VAR vHBL        AS HANDLE NO-UNDO. /* Хэндл на буфер для блокировки редактирования */
DEF VAR vDBufferMutex AS HANDLE NO-UNDO.
/* буфера */
DEF BUFFER bclass  FOR class.
DEF BUFFER ccclass FOR class.

DEFINE TEMP-TABLE ttProc NO-UNDO
   FIELD fCode     AS CHARACTER
   FIELD fNameProc AS CHARACTER
   FIELD fHProc    AS HANDLE
   .
/* Временная таблица для сохранения и восстановления введенных значений допреквизитов */
DEF TEMP-TABLE xattr-value-copy NO-UNDO LIKE xattr-value.

/* много ли значений у темпарированого ДР */
FUNCTION CheckMultiTempXAttr RETURN LOG (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR
):
   DEF VAR vIsMulti   AS LOG INIT FALSE  NO-UNDO. 

   DEF BUFFER tmpsigns FOR tmpsigns.

   FOR EACH tmpsigns WHERE tmpsigns.file-name  EQ in-FileName
                           AND tmpsigns.code       EQ in-Code
                           AND tmpsigns.surrogate  EQ in-Surr
   NO-LOCK: 
      IF  vIsMulti THEN
         RETURN vIsMulti.
      vIsMulti = TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION GetTempXAttrDate RETURN DATETIME (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR,
   INPUT in-Date     AS DATE
):
   DEF VAR vDate     AS DATETIME   NO-UNDO.
   DEF VAR vValue    AS CHAR       NO-UNDO.
   DEF VAR vResult   AS DATETIME   NO-UNDO. /* дата ДР. */

   DEF BUFFER tmpsigns FOR tmpsigns.

   FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ in-FileName
                           AND tmpsigns.code       EQ in-Code
                           AND tmpsigns.surrogate  EQ in-Surr
                           AND tmpsigns.since      LE in-Date
      NO-LOCK NO-ERROR.
      IF AVAIL tmpsigns THEN 
      DO:
         vValue = IF tmpsigns.code-value NE "" THEN tmpsigns.code-value ELSE tmpsigns.xattr-value.
         vDate = tmpsigns.since.
      END.

      vResult  =  IF     AVAIL tmpsigns
                     AND {assigned vValue}
                     THEN vDate
                     ELSE ?.
   
   RETURN vResult.
END.


/* 8-рочная дребедень */
DEFINE QUERY q1 FOR xattr-value.
DEFINE BROWSE b1 QUERY q1
   DISPLAY

&IF DEFINED( MANUAL-REMOTE ) &THEN
   /* В QBIS это поле нужно для группировки */
   xattr-value.xattr-group FORMAT "x(100)" WIDTH 1
   /* В QBIS это поле нужно для раскраски темпорированных доп. реквизитов */
   xattr-value.name-dcolor WIDTH 1
&ENDIF   
   xattr-value.displ-name /* @ vDN */

   /* В QBIS это поле без : */   
   &IF DEFINED( MANUAL-REMOTE ) = 0 &THEN   
      string( xattr-value.temp-mul , "*/:") @ vsep WIDTH 1 
   &ELSE
      string( xattr-value.temp-mul , "*/ ") @ vsep WIDTH 1 
   &ENDIF 
   xattr-value.code-value
      FORMAT "x(20000)"
&IF DEFINED( MANUAL-REMOTE ) &THEN   
      WIDTH 27
&ELSE
      WIDTH 31
&ENDIF  
   ENABLE xattr-value.code-value
   WITH {&browse-height} DOWN no-box /*    separators */
        NO-ROW-MARKERS NO-LABELS.
&IF DEFINED( MANUAL-REMOTE ) &THEN
   RUN BloddSetFormHelpLabel("F6 Фильтр^find").
&ENDIF 
DEFINE BUTTON bAllSome  LABEL "^F3-все/прис".
DEFINE BUTTON bF3  LABEL "F3-Код".
DEFINE BUTTON bByGroup  LABEL "^F4-по группам".
DEFINE BUTTON bflt      LABEL {&mainmess}.
DEFINE BUTTON bedd      LABEL "".
IF screen-lines - level < {&browse-height} + 10 THEN
   level = screen-lines - {&browse-height} - 10.

DEFINE FRAME f-xattr
   mGroupList
      VIEW-AS COMBO-BOX SIZE 59 BY 1
      AT 1
      FORMAT "x(60)"
      LABEL "Группа реквизитов"
   "------------------------------------------------------------------------------"
   b1
   "------------------------------------------------------------------------------"
   xattr-value.since
      AT 1
      LABEL "Дата дейст"  
   code.name
      VIEW-AS FILL-IN SIZE 32 BY 1 
      AT 34
      LABEL "Наим.парам"
   SKIP
   xattr-value.class
      FORMAT "x(20)"
      LABEL "Класс рекв"
   code.val
      VIEW-AS FILL-IN SIZE 32 BY 1 
      AT 34
      LABEL "Знач.парам" 
   SKIP
   vDN
      VIEW-AS FILL-IN SIZE 60 BY 1
      AT 3
      FORMAT "x(80)"
      LABEL "Код рекв"
   xattr-value.description
      VIEW-AS EDITOR INNER-LINES 3 INNER-CHARS 65
      AT 3
      LABEL "Описание"
   bF3
   bAllSome
   bByGroup
   bflt
   bedd
WITH ROW 2 CENTERED OVERLAY SIDE-LABELS TITLE DCOLOR 9 "".
/* **************** initial *************** */

RUN SetTitle.           /* Установка заголовка формы. */

num-row = 0.

CASE in-create:
                        /* Режим создания.
                        ** Отображаются все ДР объекта
                        ** и ДР, имеющие начальное значение в метасхеме. */   
   WHEN ?      THEN edit-mode = YES.

                        /* Режим просмотра.
                        ** Отображаются все ДР объекта. */
   WHEN "NO"   THEN edit-mode = NO.

                        /* Режим редактирования.
                        ** Отображаются все ДР объекта
                        ** и ОБЯЗАТЕЛЬНЫЕ ДР, имеющие начальное значение в метасхеме. */
   WHEN "YES"  THEN edit-mode = YES.
   OTHERWISE DO:        /* Данный режим предназначен для выдачи сообщения и
                        ** работе в режиме просмотра. */
      RUN Fill-SysMes ("", "", "1", in-create).

      ASSIGN            /* Сбрасываем режим, для запрета режима редактирования. */
         edit-mode   = ?
                        /* Собираем только существующие значения ДР. */
         in-create   = "NO"
      .
   END.
END CASE.

IF edit-mode EQ YES THEN
   edit-mode = GetPermission(in-class-code,in-surrogate,"w").

ASSIGN 
   mStartMode     = edit-mode    /* Запоминаем режим с которым открывается брауз. */
   vdcolor        = xattr-value.code-value:column-dcolor in browse b1
   bflt:label     = {&mainmess}
   bflt:VISIBLE   = NO
                     /* Чтение НП, определяющего цвет отображения наименования темпорированных ДР */
   mTempColor     = FGetSetting("Temporal","TempXAttrColor","")
   vTempControl   = FGetSetting("Temporal","TemXAttrContr","") EQ "Да".
.


IF NOT edit-mode
THEN bedd:LABEL = {&BTNF9}.
ELSE DO:
   bedd:VISIBLE = FALSE.
   IF edit-mode
      THEN xattr-value.code-value:column-dcolor in browse b1 = 1.
   bedd:LABEL = {&BTNESC}.
END.

FIND FIRST class WHERE
   class.class EQ in-class-code
NO-LOCK NO-ERROR.
IF NOT AVAIL class
THEN DO:

   RUN Fill-SysMes ("", "", "0",
                    "Эта запись имеет ссылку на отсутствующий класс в метасхеме." +
                    "~nКод класса '"+ (IF in-class-code EQ ?
                                       THEN "?"
                                       ELSE in-class-code) +
                    "'.~nОбратитесь к администратору системы.").

   RUN End-SysMes.         /* Завершение процесса протоколирования. */
   {intrface.del}          /* Выгрузка инструментария. */
   RETURN.
END.

/* прочитаем всякую хрень для проверки прав редактирования в статусе */
RUN InitStatusCheckData.

/* Собираем список групп реквизитов для combo-box'а */
mTmpStr = GetXAttrGroupList(in-class-code).
mGroupList = "Все,*,Вне групп,-".
DO mi = 1 TO NUM-ENTRIES(mTmpStr):
            /* Добавляем только те группы, на которые у пользователя есть право чтения */
   IF GetXAttrGroupPermission(in-class-code,ENTRY(mi,mTmpStr),in-surrogate,"r") THEN
      mGroupList = mGroupList + "," + GetXAttrEx(in-class-code,ENTRY(mi,mTmpStr),"name") + "," + ENTRY(mi,mTmpStr).
END.
mGroupList:LIST-ITEM-PAIRS = mGroupList.
mGroupList = "*".

ASSIGN
   cn       = class.NAME
   cclass   = in-class-code
.
RUN ffparent IN h_xclass (input-output cclass,buffer class).

/* Включить контроль проверки удостоверений для физических лиц */

IF cclass EQ "op" THEN
   RUN SetSysConf IN h_base ("PROCESS_OP-EDIT","Да").

IF cclass <> ? THEN
   FIND FIRST ccclass WHERE ccclass.class = cclass NO-LOCK NO-ERROR.

IF GetSysConf("param-list") NE ? THEN
   vall = YES.
/* Формирование полного списка реквизитов. */
RUN CreateXattrValue IN h_xclass (
   OUTPUT table xattr-value,
   vAll,
   in-create,
   in-class-code,
   cclass,
   in-surrogate,
   OUTPUT num-row,
   OUTPUT any-entered).

/******************************************************************************/
&IF DEFINED(SESSION-REMOTE) &THEN
ON CHOOSE OF bAllSome DO:
   RUN SetFltFieldList("IsEndDte",string(NOT LOGICAL(GetFltVal ("IsEndDte")))).
   APPLY "choose" TO bflt IN FRAME f-xattr.
END.
   
bAllSome:SENSITIVE = TRUE.
   /* Кнопка <^F4> доступна только, когда в комбо-боксе "Группа реквизитов" выбрано значение "Все" */
bByGroup:SENSITIVE = mGroupList:SCREEN-VALUE EQ "*".
&ENDIF

/* Событие при нажатии кнопки <^F4-по группам>/<^F4-без групп> */
ON CHOOSE OF bByGroup IN FRAME f-xattr
DO:
   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE). /* blodd.p */
   &ENDIF
         /* Если в комбо-боксе "Группа реквизитов" выбрано значение отличное от значения "Все" */
   IF mGroupList:SCREEN-VALUE NE "*" THEN RETURN NO-APPLY.
   mByGroup = NOT mByGroup.
   bByGroup:LABEL = IF mByGroup THEN "^F4-без групп" ELSE "^F4-по группам".
   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      mByGroup,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).
   CLOSE query q1.
   IF mByGroup
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
END.

ON CHOOSE OF bflt DO:

   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE).
   &ENDIF
                        /* Запускаем отображение фильтра. */
   RUN flt-file.p (LASTKEY).  
                        /* Установка заголовка формы. */
   RUN SetTitle.

   vall =   IF GetFltVal ("IsEndDte") EQ "YES"
               THEN YES
               ELSE NO.

   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      /* режим "по группам" применяем, только если выбрано значение "Все" в комбо-боксе "Группа реквизитов" */
      IF mGroupList EQ "*" THEN mByGroup ELSE NO,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).

   CLOSE query q1.
            /* Если включен режим "по группам", */
   IF     mByGroup
      AND mGroupList EQ "*"
               /* применяем сортировку по группе, затем по наименованию, */
   THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
               /* иначе только по наименованию */
   ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
END.

ON F5 OF xattr-value.code-value IN BROWSE b1
DO:
   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

                        /* В режиме просмотра запрещаем редактирование. */
   IF NOT edit-mode
      THEN SELF:READ-ONLY = YES.
                        /* Просмотр / редактирование в отдельном окне. */

   RUN extedit (SELF, THIS-PROCEDURE).
                        /* Восстанавливаем атрибут. */
   SELF:READ-ONLY = NO.
   ASSIGN
      xattr-value.code-value  = SELF:INPUT-VALUE
      any-entered             = yes
   .
   /* Установка значения.
   ** Также устанавливает значение в EXTEDIT.P). */
   RUN UsSetFldVal (SELF, SELF:INPUT-VALUE, 1).

                        /* Если произвели модификацию значения,
                        ** то устанавливаем признак. */
   IF GetTegDAta (SELF:PRIVATE-DATA, "MODIFIED") EQ "YES"
      THEN any-entered = YES.
END.

ON F5 OF xattr-value.description IN FRAME f-xattr
DO:
   RUN extedit.p (SELF).
   RETURN NO-APPLY.
END.


ON "F6" , "CTRL-F6", "Ctrl-F3" OF FRAME f-xattr ANYWHERE
   APPLY "CHOOSE" TO bflt IN FRAME f-xattr.

ON CHOOSE OF bedd DO:
                        /* Запрещаем редактирование в режиме "только чтение" модуля "АДМИНИСТРИРОВАНИЕ" */
   IF     work-module EQ "admin"
      AND NOT IsUserAdm (USERID ('bisquit'))
   THEN RETURN NO-APPLY.

   IF GetPermission(in-class-code,in-surrogate,"w") NE YES THEN
      RETURN NO-APPLY.
                        /* Проверка прав на группу реквизитов в QBIS через ANY-PRINTABLE не отраатывает корректно!, поэтому добавим проверку здесь */
   IF {assigned xattr-value.xattr-group}
      AND NOT GetXAttrGroupPermissionEx (xattr-value.class, GetXAttrEx (xattr-value.class, xattr-value.code, "xattr-group"), in-surrogate, "w")
   THEN RETURN NO-APPLY.

   RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* Класс объекта                 */
                                   INPUT "CheckEdtSign",    /* id метода                     */
                                   INPUT "",                /* id доп реквизита              */
                                   INPUT "",                /* код Метода                    */
                                   INPUT ?,                 /* Процедура по умолчанию        */
                                   INPUT ""                 /* Список входных параметров     */
   ).   
   IF RETURN-VALUE NE "" AND RETURN-VALUE NE "no-method" THEN 
   DO:
      RUN Fill-SysMes ("", "", "0", RETURN-VALUE). 
      ASSIGN
         edit-mode   = NO
         bedd:LABEL  = {&BTNF9}
      .
      xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 0.
      RETURN NO-APPLY.
   END.

   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      OR edit-mode        EQ ? THEN 
      RETURN.

   &IF DEFINED(SESSION-REMOTE) &THEN
   IF AVAIL xattr-value THEN
   DO:
      DEFINE VARIABLE vCode AS CHARACTER      NO-UNDO.
      vCode = xattr-value.code.

      RUN LockRecord (cclass,in-surrogate) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN NO-APPLY.

      edit-mode   = YES.
      RUN f-signs.p(INPUT BUFFER xattr-value:HANDLE,
                    INPUT xattr-value.code-value:HANDLE IN BROWSE b1,
                    THIS-PROCEDURE,
                    OUTPUT any-entered).

      IF any-entered THEN
      DO:
         /* сбрасываем дату в qbis для запроса далее */
         xattr-value.since = ? .
         /* Commented by KSV: Отключаем проверку значений ВСЕХ доп. реквизитов,
         ** т.к. менялся только один */
         vNoCheck = YES.
         RUN SaveXattrValues.
         vNoCheck = NO.
      END.
      
      RUN CreateXattrValuePrivate (
            OUTPUT TABLE xattr-value,
            vAll,
            edit-mode,
            /* режим "по группам" применяем, только если выбрано значение "Все" в комбо-боксе "Группа реквизитов" */
            IF mGroupList EQ "*" THEN mByGroup ELSE NO,
            in-class-code,
            cclass,
            in-surrogate,
            OUTPUT num-row,
            OUTPUT any-entered).

      
      DEFINE VARIABLE vRowid AS ROWID      NO-UNDO.

      CLOSE QUERY q1.
               /* Если включен режим "по группам", */
      IF     mByGroup
         AND mGroupList EQ "*"
                  /* применяем сортировку по группе, затем по наименованию, */
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                  /* иначе только по наименованию */
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

      FIND FIRST xattr-value WHERE xattr-value.code = vCode NO-ERROR.
      REPOSITION q1 TO ROWID ROWID(xattr-value) NO-ERROR.
      
      APPLY "ENTRY" TO b1.
      edit-mode   = NO.
   END.
   RUN UpdateBrowser(b1:HANDLE).
   RUN UnLockRecord (cclass, in-surrogate).
   RETURN .
   &ENDIF

   IF NOT mStartMode THEN 
   DO:
      IF NOT edit-mode THEN 
      DO: /* редактируем */
         IF cclass = "" THEN 
         DO:
            RUN Fill-SysMes ("", "", "0", "Нет класса прародителя.").
            RETURN NO-APPLY.
         END.
         IF  class.progress-table AND (NOT tst-rght-TBL(cclass, 'w') OR
            (cclass EQ "_user" AND NOT tst-rght-TBL(cclass, 'c'))) THEN 
            RETURN NO-APPLY.

         RUN LockRecord (cclass,in-surrogate) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            RETURN NO-APPLY.

         ASSIGN
            edit-mode   = YES
            mXattrCode  = xattr-value.code
         .

         RUN CreateXattrValuePrivate (
               OUTPUT TABLE xattr-value,
               vAll,
               edit-mode,
               /* режим "по группам" применяем, только если выбрано значение "Все" в комбо-боксе "Группа реквизитов" */
               IF mGroupList EQ "*" THEN mByGroup ELSE NO,
               in-class-code,
               cclass,
               in-surrogate,
               OUTPUT num-row,
               OUTPUT any-entered).
         
         ASSIGN
            bedd:LABEL  = {&BTNESC}
            xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 1
         .
                           /* Переоткрываем запрос. */
         CLOSE QUERY q1.
                  /* Если включен режим "по группам", */
         IF     mByGroup
            AND mGroupList EQ "*"
                     /* применяем сортировку по группе, затем по наименованию, */
         THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                     /* иначе только по наименованию */
         ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
                           /* Позиционируемся на запись. */
         FIND FIRST xattr-value WHERE
            xattr-value.CODE EQ mXattrCode
         NO-ERROR.
         IF AVAIL xattr-value
            THEN REPOSITION q1 TO RECID RECID (xattr-value).
      END.
      ELSE 
      DO: /* проверяем откат */
         ASSIGN INPUT BROWSE b1 xattr-value.code-value.
         IF any-entered THEN 
         DO:
            pick-value = "NO".
            RUN Fill-SysMes ("", "", "4",
                             "Сохранить введенные значения реквизитов?").
            IF pick-value EQ "YES" THEN 
            DO:
               RUN SaveXattrValues.
               IF RETURN-VALUE EQ "NO-APPLY" THEN 
               DO:
                  APPLY "entry" TO b1.
                  RETURN NO-APPLY.
               END.
            END.
            ELSE
               any-entered = NO.
            RUN RestoreInitXattrValues.

            /* Формирование полного списка реквизитов. */
            RUN CreateXattrValuePrivate (
               OUTPUT table xattr-value,
               YES,                       /* Создаем все записи. */
               in-create,
               /* режим "по группам" применяем, только если выбрано значение "Все" в комбо-боксе "Группа реквизитов" */
               IF mGroupList EQ "*" THEN mByGroup ELSE NO,
               in-class-code,
               cclass,
               in-surrogate,
               OUTPUT num-row,
               OUTPUT any-entered).

            CLOSE QUERY q1.
                     /* Если включен режим "по группам", */
            IF     mByGroup
               AND mGroupList EQ "*"
                        /* применяем сортировку по группе, затем по наименованию, */
            THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                        /* иначе только по наименованию */
            ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
            APPLY "ENTRY" TO b1.
            any-entered = NO.
         END.
         ASSIGN
            edit-mode   = NO
            bedd:LABEL  = {&BTNF9}
         .
         xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 0.

         RUN UnLockRecord (cclass, in-surrogate).
      END.
   END.
END.
/*----------------------------------------------------------------------------*/
ON F9 OF FRAME f-xattr ANYWHERE APPLY "CHOOSE" TO bedd IN FRAME f-xattr.

/* Инициализация реквизита по требованию пользователя. */
ON F3 OF xattr-value.code-value IN BROWSE b1
DO:
   IF edit-mode NE YES THEN 
   DO: 
      APPLY "CHOOSE" TO bF3 IN FRAME f-xattr.
      RETURN NO-APPLY.
   END.
   ASSIGN
      xattr-value.code-value = GetXattrInit (xattr-value.class, xattr-value.code)
      xattr-value.code-value:SCREEN-VALUE in browse b1 = xattr-value.code-value
      any-entered = yes
   .
   RETURN NO-APPLY.
END.

ON F3 OF FRAME f-xattr ANYWHERE DO:
   APPLY "CHOOSE" TO bF3 IN FRAME f-xattr.
   RETURN NO-APPLY.
END.

ON CHOOSE OF bF3 DO:

   &IF DEFINED(SESSION-REMOTE) &THEN
      RUN UpdateBrowser(b1:HANDLE).
   &ENDIF

   IF AVAIL xattr-value THEN
      vCodeFnd = xattr-value.CODE. 
   vIsCode = NOT vIsCode.

   IF vIsCode THEN 
      ASSIGN
         vDN:LABEL = "Имя рекв"
         bF3:LABEL = "F3-Имя"
      .
   ELSE
      ASSIGN
         vDN:LABEL = "Код рекв"
         bF3:LABEL = "F3-Код"
      .
   
   RUN CreateXattrValuePrivate (
         OUTPUT table xattr-value,
         vAll,
         edit-mode,
         mByGroup,
         in-class-code,
         cclass,
         in-surrogate,
         OUTPUT num-row,
         OUTPUT any-entered).
   CLOSE query q1.
   IF mByGroup
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

   FIND FIRST xattr-value WHERE xattr-value.CODE = vCodeFnd NO-LOCK NO-ERROR.

   vCurrROWID = ROWID(xattr-value).       /* текущая запись */

   REPOSITION q1 TO ROWID vCurrROWID NO-ERROR.  /* возвращаемся к записи, до переключения */
   
   APPLY "ENTRY" TO FRAME f-xattr.

   APPLY "ROW-ENTRY" TO BROWSE b1. /* IN FRAME f-xattr. */

END.


/* Отображение значений темпоральных ДР. */
ON SHIFT-F3 OF xattr-value.code-value IN BROWSE b1
DO:
   FIND FIRST xattr where
            xattr.xattr-code = xattr-value.code
      AND   xattr.class-code = xattr-value.class
   NO-LOCK NO-ERROR.
   IF       AVAIL Xattr
      AND   xattr.temporal
   THEN DO:
      RUN browseld.p (
         "tmpsigns",
         "file-name"       + "~001" +
         "code"            + "~001" +
         "surrogate",
         cclass            + "~001" +
         xattr-value.CODE  + "~001" +
         in-surrogate,
         "file-name~001code~001surrogate",
         4
      ) NO-ERROR.
      
      xattr-value.temp-mul = CheckMultiTempXAttr(cclass,in-surrogate,xattr-value.code). 
      APPLY "ROW-ENTRY" TO BROWSE b1. 

      /* вынес из блока проверки edit-mode потому что браузер значений ДР позволяет добовлять и удалять в режиме просмотра */
      xattr-value.code-value = GetTempXAttrValueEx2(
                                    cclass,in-surrogate,xattr-value.code,
                                    IF gend-date EQ ?
                                    THEN gend-hdate
                                    ELSE gend-date,
                                    "",YES).
         
      xattr-value.since      = GetTempXAttrDate(cclass,in-surrogate,xattr-value.code,
                                                      IF gend-date EQ ? THEN gend-hdate ELSE gend-date).

      APPLY "ROW-ENTRY" TO BROWSE b1.

      IF       edit-mode
      THEN DO:
                           /* Установка значения.
                           ** Также устанавливает значение в EXTEDIT.P). */
         RUN UsSetFldVal (SELF, xattr-value.code-value, 1).
      END.
      /* Commented by KSV: Обновляем браузер в QBIS */
      &IF DEFINED(SESSION-REMOTE) &THEN
      RUN UpdateBrowser(b1:HANDLE IN FRAME f-xattr).
      &ENDIF /* DEFINED(SESSION-REMOTE) */
   END.
APPLY "ENTRY" TO BROWSE b1. 
   RETURN NO-APPLY.
END.

&IF DEFINED(SESSION-REMOTE) &THEN
ON F1 OF b1 ANYWHERE
DO:
   APPLY "F1" TO xattr-value.code-value in BROWSE b1.
   RETURN NO-APPLY.
END.

ON SHIFT-F3 OF b1 ANYWHERE
DO:
   APPLY "SHIFT-F3" TO xattr-value.code-value in BROWSE b1.
   RETURN NO-APPLY.
END.
&ENDIF

PROCEDURE CheckForEdit:
   DEFINE PARAMETER BUFFER xattr-value FOR xattr-value.
   DEFINE OUTPUT PARAMETER oOk AS LOGICAL INIT NO.
   
   IF  xattr-value.code  <> ?
   AND xattr-value.CLASS <> "" THEN DO:
      FIND FIRST xattr WHERE xattr.xattr-code = xattr-value.code
                         AND xattr.class-code = xattr-value.class
      NO-LOCK NO-ERROR.
      IF AVAIL xattr THEN DO:
         IF edit-mode THEN DO:
            IF xattr-value.constant
            THEN RUN Fill-SysMes ("", "", "0", "Этот реквизит постоянный.~nЕго нельзя изменять.").
            ELSE IF     {assigned xattr-value.xattr-group}
                    AND NOT GetXAttrGroupPermissionEx (xattr-value.class, GetXAttrEx (xattr-value.class, xattr-value.code, "xattr-group"), in-surrogate, "w")
            THEN .
            ELSE oOk = YES.

               /* проверка прав по статусу */
            IF mStatusEditFld NE "" THEN
               IF NOT CAN-DO(mStatusEditFld, xattr-value.code) THEN
               DO:
                   RUN Fill-SysMes IN h_tmess ("", "", "0", "Вы не имеете права изменять реквизит " + xattr-value.CODE + " в статусе " + vStatus).
                   oOk = NO.
               END.

            /* Вызов собственной процедуры проверки */            
            RUN RunClassMethod IN h_xclass 
                                 (xattr-value.class,
                                  "ChkEdtXattr",
                                   xattr-value.code,
                                   "",?,
                                   xattr-value.code)    NO-ERROR.
            IF RETURN-VALUE NE "" AND RETURN-VALUE NE "no-method" THEN 
               oOk = NO.
         END.
         ELSE oOk = YES.
      END.
      ELSE DO:
         RUN Fill-SysMes ("", "", "0",
            "Нет такого реквизита (" +
            (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
            (IF xattr-value.CODE  EQ ? THEN "?" ELSE xattr-value.CODE ) + ")."
         ).
      END.
   END.

   RETURN.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
ON F1 OF xattr-value.code-value in browse b1 DO:

   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

   pick-value = ?.

   IF edit-mode THEN DO:

      RUN SetSysConf in h_base ("XAttrF1IsOK","NO").

      IF xattr.data-type EQ "logical"
      THEN DO:
         any-entered = YES.
         RUN UsSetFldVal (
            SELF,
            STRING (NOT (UsGetFldVal (SELF) EQ SUBSTR (xattr.data-format, 1, INDEX (xattr.data-format,"/") - 1)),
            xattr.data-format),
            1
         ).
      END.
      ELSE IF xattr.data-type = "date" THEN RUN calend.

      ELSE IF xattr.data-type = "decimal" THEN RUN calc.

      ELSE DO TRANSACTION:
        /*  Выбор знач с помощью проц просмотра (по F1) */
        RUN LookupField IN h_xclass (BUFFER xattr, 
                                     UsGetFldVal (SELF), 
                                     YES,
                                     level).
      END.
      IF (   LASTKEY EQ 10
          OR (LASTKEY             EQ 13
              AND xattr.data-type EQ "decimal")
          OR GetSysConf("XAttrF1IsOK") = "YES")
      AND pick-value NE ?
      THEN DO:
         ASSIGN
            xattr-value.code-value:SCREEN-VALUE in browse b1 = pick-value
            any-entered             = yes
         .
                        /* Установка значения.
                        ** Также устанавливает значение в EXTEDIT.P). */
         RUN UsSetFldVal (SELF, pick-value, 1).
      END.
   END. /* edit-mode */
   ELSE DO: /* look */

      /* вызов собственной проц просмотра */
      {xattr-ed.nav &val    = xattr-value.code-value
                    &pXattr = Yes
                    &pSurr  = in-surrogate
      }

   END.
   RETURN NO-APPLY.
END.

/*------------------------------------------------------------------------------------------*/
ON ANY-PRINTABLE, CLEAR, DELETE-CHARACTER, CTRL-V, BACKSPACE,
   CTRL-SHIFT-Z OF xattr-value.code-value IN BROWSE b1
DO:
   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

   IF xattr-value.code = ? THEN RETURN NO-APPLY.
   IF edit-mode THEN DO:
      any-entered = YES.
      IF KEYLABEL(LASTKEY) = "ctrl-v" THEN DO:
         xattr-value.code-value:SCREEN-VALUE IN BROWSE b1 = xattr-value.code-value:SCREEN-VALUE + clip-buffer.
      END.
      IF LASTKEY EQ KEYCODE ("Ctrl-Shift-z") THEN
      DO:
         IF VALID-HANDLE(FOCUS) AND CAN-DO("EDITOR,FILL-IN",FOCUS:TYPE) THEN
            clip-buffer = FOCUS:SCREEN-VALUE.
         ELSE
            clip-buffer = FRAME-VALUE.
         RUN chk-terr.p (clip-buffer).
      END.
   END.
   ELSE RETURN NO-APPLY.
END.

ON ROW-ENTRY OF b1 IN FRAME f-xattr DO:
   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      THEN RETURN NO-APPLY.

   vPrevVal =    xattr-value.code-value.
   IF xattr-value.CODE EQ "МастерКлиент" THEN
      RUN SetSysConf IN h_base ("MC_BEFORE_VAL",vPrevVal).
   IF xattr-value.CODE EQ "ДубльГруппа" THEN
      RUN SetSysConf IN h_base ("GR_BEFORE_VAL",vPrevVal).

   DEF VAR dt LIKE xattr-value.since NO-UNDO.
   dt = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
   IF xattr-value.since GT dt THEN xattr-value.since = ?.

   DISPLAY
      xattr-value.description
      xattr-value.class 

      xattr-value.NAME WHEN     vIsCode @ vDN 
      xattr-value.CODE WHEN NOT vIsCode @ vDN 

            xattr-value.since
      ""
         @ code.val
      ""
         @ code.name
   WITH FRAME f-xattr.

   DISPLAY
      xattr-value.displ-name /* @ vDN */

      &IF DEFINED( MANUAL-REMOTE ) = 0 &THEN   
         string( xattr-value.temp-mul , "*/:") @ vsep 
      &ELSE
         string( xattr-value.temp-mul , "*/ ") @ vsep 
      &ENDIF   

      xattr-value.code-value
   WITH BROWSE b1.
   
   APPLY "ENTRY" TO xattr-value.code-value IN BROWSE b1.

   FIND FIRST xattr WHERE
            xattr.xattr-code EQ xattr-value.code
      AND   xattr.class-code EQ xattr-value.class
   NO-LOCK NO-ERROR.

   IF AVAIL xattr THEN
   DO:
      /* попытаться выполнить процедуру метода получения связанной информации */
      pick-value = ?.
      RUN RunClassMethod IN h_xclass 
                         (xattr-value.class,
                         "GetXValDesc",
                         xattr-value.code,
                         "",?,
                         CHR(1) + REPLACE(STRING(xattr-value.code-value),",",";"))
      NO-ERROR.
      IF pick-value NE ? THEN
         DISPLAY 
            ENTRY(1,pick-value,CHR(2)) @ code.name
            IF NUM-ENTRIES(pick-value,CHR(2)) GT 1 THEN ENTRY(2,pick-value,CHR(2)) ELSE "" @ code.val
         WITH FRAME f-xattr.
      IF     RETURN-VALUE        EQ "no-method"
         AND xattr.domain-code   NE ""
      THEN DO:
         /* ищем запись в tmp-code для того, чтобы определить
         ** темпорированный это классификатор или обычный */
         FIND LAST tmp-code WHERE tmp-code.class      EQ xattr.domain-code
                              AND tmp-code.code       EQ xattr-value.code-value
                              AND tmp-code.beg-date   LE gend-date
         NO-LOCK NO-ERROR.
         /* Если классификатор темпорированный, собираем соотв. суррогат */
         IF AVAIL tmp-code
         THEN DISPLAY tmp-code.val  @ code.val
                      tmp-code.name @ code.name
         WITH FRAME f-xattr.
         /* Иначе пытаемся найти обычное значение в классификаторе */
         ELSE DO:
            FIND FIRST CODE WHERE
                     code.class EQ xattr.domain-code
               AND   code.code  EQ xattr-value.code-value
            NO-LOCK NO-ERROR.

            IF AVAIL code
               THEN DISPLAY code.val code.name with frame f-xattr.
         END.
      END.
   END.
END.
    
ON ROW-LEAVE OF b1 IN FRAME f-xattr DO:
   &IF DEFINED(SESSION-REMOTE) &THEN
   RETURN .
   &ENDIF

   vPrevVal =    xattr-value.code-value.
               /* пропускаем группы */
   IF xattr-value.CLASS EQ "" THEN RETURN. 
   IF KEYFUNC(LASTKEY) = "end-error" THEN RETURN /* NO-APPLY*/ .
   IF xattr-value.code = ? or edit-mode NE YES THEN RETURN.
   ASSIGN INPUT browse b1 xattr-value.code-value.
   RUN CheckXattrValue.
   IF RETURN-VALUE = "NO-APPLY" THEN RETURN NO-APPLY. /* undo, RETURN. */
   DISPLAY xattr-value.code-value with browse b1.

   IF vTempControl AND IsTemporal(cclass,xattr-value.CODE) THEN DO:

      IF xattr-value.code-value  NE GetXattrValue (cclass, in-surrogate, xattr-value.CODE) AND xattr-value.code-value NE vPrevVal THEN DO:

         {getdate.i
           &DispBeforeDate = "xattr-value.code"
           &DateLabel      = "Дата начала"
           &DateHelp       = "Дата начала действия реквизита (F1 - Календарь)"}

         vTable = GetXclassFile (in-class-code).
         IF end-date NE ? AND 
            CAN-FIND (FIRST tmpsigns WHERE
                            tmpsigns.file-name EQ vTable
                        AND tmpsigns.CODE      EQ xattr-value.code
                        AND tmpsigns.surrogate EQ in-surrogate
                        AND tmpsigns.since > end-date) THEN DO:

            RUN Fill-SysMes ("", "", "4", "Обнаружены значения с более поздней датой. Для просмотра нажмите Shift+F3. Сохранить введенное значение?").           
            vChoice = (pick-value = "YES").
            IF vChoice NE YES THEN DO:
               xattr-value.code-value:SCREEN-VALUE = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
               RETURN NO-APPLY.
            END.

         END.
         IF end-date NE ? THEN
            xattr-value.since = end-date.
      END.
   END.
   IF xattr-value.CODE EQ "МастерКлиент" THEN
      RUN SetSysConf IN h_base ("MC_AFTER_VAL",xattr-value.code-value).
   IF xattr-value.CODE EQ "ДубльГруппа" THEN
      RUN SetSysConf IN h_base ("GR_AFTER_VAL",xattr-value.code-value).
   IF xattr-value.lastattr and KEYFUNC(LASTKEY) = "return" THEN DO:
      APPLY "GO" TO SELF.
      RETURN NO-APPLY.
   END.
END.

ON GO OF FRAME f-xattr DO:
   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      THEN RETURN.

   IF LASTKEY = 10 THEN
      ASSIGN INPUT BROWSE b1 xattr-value.code-value.
   RUN SaveXattrValues.
   IF RETURN-VALUE = "NO-APPLY" then
   DO:
      APPLY "entry":U TO b1.
      RETURN NO-APPLY.
   END.
   RUN UnLockRecord (cclass, in-surrogate).
END.

ON END-ERROR, ENDKEY OF FRAME f-xattr  
&IF DEFINED(SESSION-REMOTE) &THEN 
   anywhere  
&ENDIF
   DO:
&IF DEFINED(SESSION-REMOTE) &THEN 
   RUN UnLockRecord (cclass, in-surrogate).
&ELSE   
   IF NOT AVAIL xattr-value
   OR xattr-value.code = ? THEN RETURN.

   IF mStartMode THEN RETURN.

   IF edit-mode THEN DO:
      APPLY "CHOOSE" TO bedd IN FRAME f-xattr.
      RETURN NO-APPLY.
   END.
&ENDIF
END.

/* поиск в редакторе ДР */
ON F7 OF BROWSE b1 ANYWHERE
DO:
   /* отображение формы поиска */
   DO WITH FRAME fnd OVERLAY 1 COL CENTERED ROW 8
   COLOR messages SIDE-LABELS TITLE "[ ПОИСК ]"
   ON ENDKEY UNDO, LEAVE:
      PAUSE 0.
      UPDATE
         vCodeFnd  FORMAT "X(100)" LABEL "Код"
                   VIEW-AS FILL-IN SIZE 26 BY 1
         vNameFnd  FORMAT "X(100)" LABEL "Наименование"
                   VIEW-AS FILL-IN SIZE 26 BY 1.
   END.
   HIDE FRAME fnd.

   IF    LAST-EVENT:FUNCTION EQ "END-ERROR"  /* Esc */
      OR (vCodeFnd            EQ ""          /* ни одно поле не заполнено */
      AND vNameFnd            EQ "")
   THEN RETURN NO-APPLY.
   
   vCurrROWID = ROWID(xattr-value).       /* текущая запись */
   ASSIGN INPUT BROWSE b1 xattr-value.code-value .
   RUN SaveTmpXattrValues.
   QUERY q1:GET-FIRST().
   REPEAT:                                /* перебор записей запроса */
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF  xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
      AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
      QUERY q1:GET-NEXT().
   END.
   
   IF AVAILABLE xattr-value
   THEN DO:                               /* запись найдена */
      /* позиционируем курсор на найденную запись */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* обновить форму */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* запись не найдена - сообщение, выход */
      BELL.
      MESSAGE "Запись не найдена" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* возвращаемся к записи, до поиска */
      RETURN NO-APPLY.
   END.
   RUN RestoreTmpXattrValues.
   RETURN NO-APPLY.
END.

/* найти следующую запись, удовлетворяющую запросу поиска */
ON SHIFT-F7 OF  BROWSE b1  ANYWHERE
DO:
   vCurrROWID = ROWID(xattr-value).       /* текущая запись */
   REPEAT:                                /* перебор записей запроса */
      QUERY q1:GET-NEXT().
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF     xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
         AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
   END.
   IF AVAILABLE xattr-value
   THEN DO:                               /* запись найдена */
      /* позиционируем курсор на найденную запись */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* обновить форму */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* запись не найдена - сообщение, выход */
      BELL.
      MESSAGE "Запись не найдена" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* возвращаемся к записи, до нажатия Shift-F7 */
      RETURN NO-APPLY.
   END.
   RETURN NO-APPLY.
END.

/* найти предыдущую запись, удовлетворяющую запросу поиска */
ON CTRL-F7 OF  BROWSE b1 ANYWHERE
DO:
   vCurrROWID = ROWID(xattr-value).       /* текущая запись */
   REPEAT:                                /* перебор записей запроса */
      QUERY q1:GET-PREV().
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF     xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
         AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
   END.
   IF AVAILABLE xattr-value
   THEN DO:                               /* запись найдена */
      /* позиционируем курсор на найденную запись */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* обновить форму */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* запись не найдена - сообщение, выход */
      BELL.
      MESSAGE "Запись не найдена" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* возвращаемся к записи, до нажатия Ctrl-F7 */
      RETURN NO-APPLY.
   END.
   RETURN NO-APPLY.
END.

ON CTRL-G GO.
ON CTRL-G OF FRAME f-xattr ANYWHERE DO:
   DEF VAR rid AS RECID NO-UNDO.
   IF AVAIL xattr-value THEN rid = RECID(xattr-value).
   ELSE rid = ?.
   FORM HEADER
      "             ДОП. РЕКВИЗИТЫ " + in-title + " на " + STRING(vSignsDt) FORMAT "x(100)" SKIP
      "             класс: " + in-class-code + " (" + cn + ") (""" + ENTRY(3, user-config-info) + """)"
      FORMAT "x(100)"
   WITH FRAME a NO-BOX WIDTH 200.
   {setdest.i &nodef="/*"}
   GET FIRST q1.
   DO WHILE AVAIL xattr-value WITH FRAME a:
      DISP
         xattr-value.code LABEL "КОД РЕКВИЗИТА" FORMAT "x(20)"
         xattr-value.since
         string( xattr-value.temp-mul , "*/ ") @ vsep   LABEL "Е" FORMAT "x(1)"
         xattr-value.name  LABEL "НАИМЕНОВАНИЕ"       FORMAT "x(40)"
         xattr-value.class
         xattr-value.code-value  FORMAT "x(60)"
      WITH FRAME a DOWN.
      DOWN.
      GET NEXT q1.
   END.
   &SCOP def_telefon YES
   {signatur.i &user-only=1}
   {preview.i}.
   FIND xattr-value WHERE RECID(xattr-value) = rid NO-LOCK NO-ERROR.
   RETURN NO-APPLY.
END.

ON ROW-DISPLAY OF b1 IN FRAME f-xattr
DO:
   DEFINE VARIABLE vTmpFld AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vColor  AS INT64    NO-UNDO.

            /* Если цвет темпорированных ДР определен, раскрашиваем их в этот цвет */
   IF {assigned mTempColor}
   THEN DO:
      COLOR DISPLAY VALUE(mTempColor) vTmpFld FORMAT "x(1)" WITH FRAME tmp-frm.
      vColor = vTmpFld:DCOLOR. 
      HIDE FRAME tmp-frm.
               /* Если ДР - темпорированный, раскрашиваем */
      IF IsTemporal(GetXclassProgress(xattr-value.class), xattr-value.code)
      THEN DO:
         DEF VAR dt LIKE xattr-value.since NO-UNDO.
         dt = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
         IF xattr-value.since GT dt THEN xattr-value.since = ?.

         xattr-value.displ-name:DCOLOR  IN BROWSE b1 = vColor.
         &IF DEFINED( MANUAL-REMOTE ) &THEN
            xattr-value.name-dcolor = vColor.
         &ENDIF
      END.
   END.

            /* Ракрашивание лэйблов групп */
   IF     xattr-value.CLASS EQ ""
      AND xattr-value.xattr-group NE ""
   THEN DO:
      COLOR DISPLAY "bright-white" vTmpFld FORMAT "x(1)" WITH FRAME tmp-frm.
      vColor = vTmpFld:DCOLOR. 
      HIDE FRAME tmp-frm.
      xattr-value.displ-name:DCOLOR IN BROWSE b1 = vColor.
   END.

   RETURN.
END.

ON VALUE-CHANGED OF mGroupList IN FRAME f-xattr
DO:
   ASSIGN mGroupList.
   /* В таблице xattr-value реквизиты вне групп имеют реквизит
   ** xattr-group равный неопределенному значению. Так сделать для
   ** сортировки - чтобы реквизиты вне групп попадали в конец
   ** списка в режиме "по группам". Поэтому осуществляется подмена
   ** значения признака группы, по которой производится фильтрация,
   ** с пустого (т.е. когда выбрано "Вне групп") на неопределенное */
   IF mGroupList EQ "-" THEN mGroupList = ?.

   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      /* режим "по группам" применяем, только если выбрано значение "Все" в комбо-боксе "Группа реквизитов" */
      IF mGroupList EQ "*" THEN mByGroup ELSE NO,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).
   CLOSE query q1.

   CLOSE QUERY q1.
            /* Если включен режим "по группам", */
   IF     mByGroup
      AND mGroupList EQ "*"
               /* применяем сортировку по группе, затем по наименованию, */
   THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
               /* иначе только по наименованию */
   ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
   /* Commented by KSV: Обновляем браузер в QBIS */
   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE IN FRAME f-xattr). /* blodd.p */
   &ENDIF /* DEFINED(SESSION-REMOTE) */
   RETURN.
END.

ON CTRL-F4 OF FRAME f-xattr ANYWHERE
DO:
   APPLY "choose" TO bByGroup IN FRAME f-xattr.
   RETURN.
END.
/*------------------------------------------------------------------------------------------*/
/********   основной блок   **********/
IF GetSysConf("param-list") NE ? THEN DO:
   RUN SetFltFieldLIST ("sc-1",GetSysConf("param-list")).
   RUN SetFltFieldLIST ("IsEndDte","YES").
END.
OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

PAUSE 0.
FRAME f-xattr:VISIBLE = YES.

XATTRBLK:
DO
ON ERROR  UNDO XATTRBLK, LEAVE XATTRBLK
ON ENDKEY UNDO XATTRBLK, LEAVE XATTRBLK
WITH FRAME f-xattr:

   PAUSE 0.

   mGroupList:SCREEN-VALUE = "*".
   ENABLE
      mGroupList
      b1
      bflt
      bedd when mStartMode
   WITH FRAME f-xattr.

   ASSIGN
      xattr-value.description:read-only   = YES
      xattr-value.description:pfcolor     = 0
      xattr-value.description:SENSITIVE   = YES
   .

&IF DEFINED(SESSION-REMOTE) &THEN
   /* Commented by KSV: APPLY необходим для Биссмарт */
   APPLY "ENTRY" TO b1.
&ENDIF

   /* Commented by KSV: Не допускаем перегонку лишних данных в браузере */
   &IF DEFINED(SESSION-REMOTE) &THEN
   b1:PRIVATE-DATA = {&BLODD_BROWSE_NO_REFRESH}.
   &ENDIF
   

   WAIT-FOR GO, END-ERROR OF FRAME f-xattr FOCUS b1.
END.
ASSIGN FRAME f-xattr:visible = NO.

RUN DeleteOldDataProtocol IN h_base ("AllowChkDialog").

RUN SetSysConf IN h_base ("MC_BEFORE_VAL",?).
RUN SetSysConf IN h_base ("GR_BEFORE_VAL",?).
RUN SetSysConf IN h_base ("MC_AFTER_VAL",?).
RUN SetSysConf IN h_base ("GR_AFTER_VAL",?).
RUN SetSysConf IN h_base ("PROCESS_OP-EDIT",?).
RUN End-SysMes.         /* Завершение процесса протоколирования. */
{intrface.del}          /* Выгрузка инструментария. */
RETURN.

/********  конец основного блока  **********/
PROCEDURE CheckXattrValue.             /* по ON F1 */

   DEF VAR ferr    AS LOG  NO-UNDO.
   DEF VAR err-msg AS CHAR NO-UNDO.

   DEF BUFFER xattr FOR xattr. /* локализация буффера */
   /* Полная провера значнеи реквизита. */
   RUN CheckFullFieldValue IN h_xclass (
      xattr-value.class,      /* Код класса. */
      xattr-value.code,       /* Код реквизита. */
      in-surrogate,           /* Идентификатор объекта. */
      xattr-value.code-value  /* Значение реквизита. */
   ).
   IF RETURN-VALUE NE ""
   THEN DO:
      RUN Fill-SysMes ("", "", "0", RETURN-VALUE).
      RETURN "NO-APPLY".
   END.

   /* Данная проверка необходима для ДР, у которых на метасхеме
   ** атрибут "Mandatory" установлен в нет, но процедура валидации одного из
   ** введенных ДР установила во временной таблице атрибу "Mandatory" в ДА.
   ** Такая потребность есть при создании пары ДР, которые не могут
   ** существовать один без другово.*/
   IF       (xattr-value.MANDATORY OR CAN-DO(mStatusMandatFld,xattr-value.code))
      AND   xattr-value.code-value EQ ""
   THEN DO:
      RUN Fill-SysMes (
         "", "", "0",
         "Вы обязаны ввести значение реквизита (" +
         (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
         (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ").").
      RETURN "NO-APPLY".
   END.
   
   IF {assigned xattr-value.code-value}
   THEN DO:

      RUN GetXattr IN h_xclass (
            xattr-value.class,
            xattr-value.code,
            BUFFER xattr
         ).

      IF NOT AVAIL xattr
      THEN DO:
         RUN Fill-SysMes (
            "", "", "0",
            "Нет такого реквизита (" +
            (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
            (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ")."
         ).
         RETURN "NO-APPLY".
      END.

      err-msg = "реквизита " + STRING(xattr.xattr-code).
                        /* Приведение к формату не запускаем для допреквизитов, у которых установлен домен */
      IF NOT {assigned xattr.Domain-Code} THEN
         RUN SetValue IN h_base(
               INPUT-OUTPUT xattr-value.code-value,
               INPUT xattr.data-type,
               INPUT xattr.data-format,
               INPUT-OUTPUT err-msg,
               OUTPUT ferr
            ).
      xattr-value.code-value = TRIM(xattr-value.code-value).
   END.
END.

/* Сохранение значений ДР. */
PROCEDURE SaveXattrValues.

   DEFINE VARIABLE mInstance     AS HANDLE     NO-UNDO.
   DEFINE VARIABLE mOk           AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE mRETURN-VALUE AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vXattrVal     AS CHARACTER  NO-UNDO. /* Значение ДР. */
   DEFINE VARIABLE XattrList     AS CHARACTER  NO-UNDO
                                 /* Здесь уазываются коды ДР, отвечающие за права доступа,
                                 ** которые пользователь может изменить сам себе. */
                                 INITIAL 'Принтер,Телефон'.
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .
   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.
   vListCS = GetHistoryFieldsCS ( cclass ). 

   /* Проверка значений ДР на корректность введенных значений. */
   OPEN QUERY q1 FOR EACH xattr-value.
   GET FIRST q1.
   /* Commented by KSV: Для Биссмарта данная процедура сохраняет значение доп.
   ** река, поэтому необходимости в проверке значений всех реквизитов нет */
   IF vNoCheck <> YES THEN
      REPEAT WHILE AVAIL xattr-value:
         IF xattr-value.class EQ "" THEN
         DO:
            GET NEXT q1.
            NEXT.
         END.

         RUN CheckXattrValue.
   
         IF RETURN-VALUE EQ "NO-APPLY"
         THEN DO:
            REPOSITION q1 TO RECID RECID (xattr-value).
            RETURN "NO-APPLY".
         END.
         GET NEXT q1.
      END.

   /* Если что-то редактировалось... */
   IF any-entered THEN
   XattrTRANS:
   DO TRANSACTION:

      NextXattr:
      FOR EACH xattr-value,
      FIRST xattr WHERE
               xattr.class-code EQ xattr-value.class
         AND   xattr.xattr-code EQ xattr-value.code
      NO-LOCK:

         /* Получаем значение ДР. */
         vXattrVal = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
         vStrCS = vXattrVal. 

         /* Если что-то меняли, то заносим изменения в базу. */
         IF    (     vXattrVal               EQ ?
               AND   xattr-value.code-value  NE "")
            OR (     vXattrVal               NE ?
               AND   (  xattr-value.code-value  NE (IF CAN-DO(vListCS,xattr-value.CODE) THEN vStrCS ELSE vXattrVal)
                  OR    xattr.temporal))
         THEN DO:

            /* Проверяются права на редактирование своих собственных ДР.
            ** Разрешается изменять только ДР. указанные в XAttrList. */
            IF     CAN-DO(GetXclassAllChildsEx('_user'),in-class-code)     
               AND userid('bisquit') EQ in-surrogate
               AND NOT CAN-DO (XAttrList, xattr-value.code)
            THEN DO:
               RUN Fill-SysMes (
                  "", "", "0",
                  "Нельзя изменять реквизит (" +
                  (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
                  (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ") " +
                  "самому себе."
               ).
               UNDO NextXattr, NEXT NextXattr.
            END.

            IF     CAN-DO(GetXclassAllChildsEx('ХранРаб'),in-class-code)
               AND CAN-FIND(FIRST cust-role WHERE 
                                  cust-role.cust-role-id EQ INT64(in-surrogate) 
                              AND cust-role.surrogate    EQ USERID('bisquit')) THEN
            DO:
               RUN Fill-SysMes ("", "", "0",
                                "Нельзя изменять реквизиты своих ролей класса ХранРаб"
                               ).
               UNDO NextXattr, NEXT NextXattr.
            END.

            IF vTempControl AND xattr.temporal AND xattr-value.code-value  NE vXattrVal AND xattr-value.since EQ ? THEN DO:

               {getdate.i
                 &DispBeforeDate = "xattr-value.code"
                 &DateLabel      = "Дата начала"
                 &DateHelp       = "Дата начала действия реквизита (F1 - Календарь)"}
                 
                vTable = GetXclassFile (in-class-code).
                 
                IF end-date NE ? AND 
                   CAN-FIND (FIRST tmpsigns WHERE
                                   tmpsigns.file-name EQ vTable
                               AND tmpsigns.CODE      EQ xattr-value.code
                               AND tmpsigns.surrogate EQ in-surrogate
                               AND tmpsigns.since > end-date) THEN DO:
              
                   RUN Fill-SysMes ("", "", "4", "Обнаружены значения с более поздней датой. Для просмотра нажмите Shift+F3. Сохранить введенное значение?").           
                   vChoice = (pick-value = "YES").
                   IF vChoice NE YES THEN DO:
                      xattr-value.code-value = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
                      RETURN NO-APPLY.
                   END.
              
                END.
                IF end-date NE ? THEN
                   xattr-value.since = end-date.
            END.  
            IF vTempControl AND xattr.Temporal AND xattr-value.since NE ? THEN DO:
               UpdateTempSignsEx(in-class-code,
                                 in-surrogate,
                                 xattr-value.code,
                                 xattr-value.since,
                                 xattr-value.code-value,
                                 ?).
            END.
            ELSE 
            /* Сохраняем занчение в БД, без проверки на возможную ошибку. */
            UpdateSigns (
               in-class-code,          /* Класс объекта. */
               in-surrogate,           /* Идентификатор объекта. */
               xattr-value.code,       /* Код ДР. */
               xattr-value.code-value, /* Значение ДР. */
               ?).                     /* Индексируемость. */              
         END.
      END.
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* Класс объекта                 */
                                      INPUT "xattrFmsChk",     /* id метода                     */
                                      INPUT "",                /* id доп реквизита              */
                                      INPUT "",                /* код Метода                    */
                                      INPUT ?,                 /* Процедура по умолчанию        */
                                      INPUT REPLACE(in-surrogate,",",";") 
      ) NO-ERROR.

/* Проверка обьекта целиком пока закрыта,
** т.к. при редактировании объекта осущетсвляется двойной запуск проверок.

      /* Отключаем все агрегации */
      RUN PrepareInstance ("").

      /* Cоздание обьекта и заполнение данными из БД */
      RUN GetInstance IN h_data (INPUT  in-class-code, /* Класс объекта            */
                                 INPUT  in-surrogate,  /* Идентификатор объекта    */
                                 OUTPUT mInstance,     /* Хэндл созданного обьекта */
                                 OUTPUT mOK).          /* Флаг результата          */
      IF mOK <> YES                  OR
         NOT VALID-HANDLE(mInstance) THEN
         LEAVE XattrTRANS.

      /* Выполнение метода "validate" */
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* Класс объекта                 */
                                      INPUT "validate",        /* id метода                     */
                                      INPUT "",                /* id доп реквизита              */
                                      INPUT "",                /* код Метода                    */
                                      INPUT ?,                 /* Процедура по умолчанию        */
                                      INPUT in-class-code +    /* Список входных параметров     */
                                            CHR (1)       +
                                            STRING (mInstance)).

      /* Обработка ошибки метода "validate"  */
      IF RETURN-VALUE = {&RET-ERROR} THEN DO:
         RUN Fill-SysMes ("", "", "0", "Невозможно сохранить изменения дополнительных реквизитов." +
                                       "Причина: Ошибка при проверке "                            +
                                       in-title + " (" + in-class-code + ").").
         ASSIGN mRETURN-VALUE = "NO-APPLY".
         UNDO XattrTRANS, LEAVE XattrTRANS.
      END.
*/
   END. 

   /* Удаление обьекта 
   IF VALID-HANDLE (mInstance) THEN
      RUN DelEmptyInstance (mInstance). 
   */

   /* При ошибке позиционирование на первую запись*/
   IF mRETURN-VALUE = "NO-APPLY" THEN DO:
      OPEN QUERY q1 FOR EACH xattr-value.
      GET FIRST q1.
      IF AVAILABLE xattr-value THEN
         REPOSITION q1 TO RECID RECID (xattr-value).
   END.
   ELSE
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* Класс объекта                 */
                                      INPUT "copyAttr",        /* id метода                     */
                                      INPUT "",                /* id доп реквизита              */
                                      INPUT "",                /* код Метода                    */
                                      INPUT ?,                 /* Процедура по умолчанию        */
                                      INPUT REPLACE(in-surrogate,",",";")       /* Список входных параметров     */
      ) NO-ERROR.

   RETURN mRETURN-VALUE.
END.

/* Установка заголовка формы. */
PROCEDURE SetTitle.
   
   frame f-xattr:TITLE =   "[Д.Р. " + in-title + "(" +
                           in-class-code + ") (~"" + ENTRY(3, user-config-info) +
                           "~") на " + STRING(vSignsDt) + "]".
END PROCEDURE.

/* Модификация CreateXattrValue из pp-xclass для группировки реквизитов */
PROCEDURE CreateXattrValuePrivate PRIVATE.
   DEF OUTPUT PARAM TABLE        FOR xattr-value.     /* временная таблица реквизитов объекта */
   DEF INPUT  PARAM iAll         AS LOG    NO-UNDO.   /* все реквизиты/только присвоенные */
   DEF INPUT  PARAM iCreate      AS CHAR   NO-UNDO.   /* предустанавливать ли ДР начальным значением */
   DEF INPUT  PARAM iGroup       AS LOG    NO-UNDO.   /* Включать ли группы */
   DEF INPUT  PARAM iClass       AS CHAR   NO-UNDO.   /* класс объекта */
   DEF INPUT  PARAM iFile        AS CHAR   NO-UNDO.   /* таблица */
   DEF INPUT  PARAM iSurrogate   AS CHAR   NO-UNDO.   /* суррогат объекта */
   DEF OUTPUT PARAM oNumRow      AS INT64    NO-UNDO.
   DEF OUTPUT PARAM oAnyEntered  AS LOG    NO-UNDO.

   DEFINE VARIABLE mProcName AS CHARACTER  NO-UNDO.

                        /* Сохраняем текущее состояние значений допреквизитов во врем.таблице */
   ASSIGN INPUT BROWSE b1 xattr-value.code-value NO-ERROR.
   RUN SaveTmpXattrValues.

   RUN CreateXattrValueEx in h_xclass (
      OUTPUT table xattr-value,
      vIsCode,
      iAll,
      iCreate,
      iClass,
      iFile,
      iSurrogate,
      OUTPUT oNumRow,
      OUTPUT oAnyEntered).
   oAnyEntered = oAnyEntered OR any-entered.
         /* если включен режим "по группам" */
   IF iGroup
   THEN DO:
            /* Добавляем группы, описанные на классе */
      CR_GR:
      FOR EACH xattr WHERE xattr.Class-Code EQ iClass
                       AND xattr.DATA-TYPE  EQ "group"
      NO-LOCK:
                  /* Добавляем только те группы, на которые у пользователя есть право чтения */
         IF NOT GetXAttrGroupPermission(xattr.Class-Code,xattr.Xattr-Code,iSurrogate,"r") THEN NEXT CR_GR.

         CREATE xattr-value.
         ASSIGN xattr-value.CLASS         = ""
                xattr-value.CODE          = xattr.Xattr-Code
                xattr-value.xattr-group   = xattr.Xattr-Code
                xattr-value.NAME          = CAPS(" " + xattr.NAME)
                xattr-value.displ-name    = IF vIsCode THEN CAPS(" " + xattr.xattr-code) ELSE CAPS(" " + xattr.NAME)
                xattr-value.DESCRIPTION   = "Группа реквизитов " + xattr.NAME + " (" + xattr.Xattr-Code + ")"
                xattr-value.MANDATORY     = NO
                xattr-value.constant      = YES
                xattr-value.code-value    = "       "
         .
      END.
            /* Добавляем псевдогруппу "Вне групп" */
      CREATE xattr-value.
      ASSIGN xattr-value.CLASS         = ""
             xattr-value.CODE          = "Вне групп"
             xattr-value.xattr-group   = ?
             xattr-value.NAME          = CAPS(" Вне групп")
             xattr-value.displ-name    = CAPS(" Вне групп")
             xattr-value.DESCRIPTION   = "Вне групп"
             xattr-value.MANDATORY     = NO
             xattr-value.constant      = YES
             xattr-value.code-value    = "       "
      .
   END.

   FOR EACH xattr-value WHERE xattr-value.xattr-group NE "":
      xattr-value.xattr-group = ENTRY (1, xattr-value.xattr-group).
   END.

                        /* Восстанавливаем значения допреквизитов */
   RUN RestoreTmpXattrValues.
  
/* CHR(8) - backspace
   CHR(16) -
   CHR(21) - 
     */
   RETURN.
END PROCEDURE.



PROCEDURE InitStatusCheckData PRIVATE.
   DEFINE VARIABLE vFieldStat  AS CHARACTER NO-UNDO.
/*   MESSAGE "1 mStatusMandatFld=" mStatusMandatFld SKIP "mStatusEditFld=" mStatusEditFld
      SKIP GetXattrEx(in-class-code,"StatModelOn","Initial")
      SKIP ENTRY(1,GetXattrEx(in-class-code,"StatModelOn","Data-Format"),"~/")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
   mStatusEditFld = "".
   mStatusMandatFld = "".
      /* определяем включена ли модель состояний на классе объекта */
   IF GetXattrEx(in-class-code,"StatModelOn","Initial") NE ENTRY(1,GetXattrEx(in-class-code,"StatModelOn","Data-Format"),"~/")
   THEN  RETURN.
      /* определяем код поля, в котором хранится статус */
   vFieldStat = DYNAMIC-FUNCTION ("GetFieldStat" IN h_stdtrg,GetXclassProgress(in-class-code)).
   IF NOT {assigned vFieldStat} THEN   RETURN.
      /* определяем статус */
   vStatus = GetValueAttr(GetXclassProgress(in-class-code),in-surrogate,vFieldStat).
   IF vStatus = "" THEN   RETURN.
      /* ищем описание статуса на классе */
   FIND FIRST xstatus 
      WHERE xstatus.class-code  EQ in-class-code
        AND xstatus.status-code EQ vStatus
   NO-LOCK NO-ERROR.
   IF NOT AVAIL xstatus THEN  RETURN.
      /* сохраняем список обязательных для статуса полей */
   mStatusMandatFld = xstatus.mandatory-fields. 
      /* нет ограничений на редактирование полей */
   IF xstatus.edit-fields EQ "*" THEN  RETURN.
   mStatusEditFld = xstatus.edit-fields .
END PROCEDURE.


/* Позиционирование браузера на указанную запись реквизита.
** Можно использовать из контекста вызванных процедур (например, метода chkupd реквизита)
** Можно передавать в качестве указателя на запись для позиционирования как ROWID
** записи, так и код реквизита */
PROCEDURE RepositionCursor.
   DEF INPUT  PARAM iRowID     AS ROWID  NO-UNDO.
   DEF INPUT  PARAM iXAttrCode AS CHAR   NO-UNDO.

                        /* Если не передан ROWID, ищем запись по коду реквизита */
   IF iRowID EQ ? THEN
   DO:
      FIND FIRST xattr-value WHERE xattr-value.code EQ iXAttrCode NO-LOCK NO-ERROR.
      IF AVAIL xattr-value THEN
         iRowID = ROWID (xattr-value).
   END.
                        
   IF iRowID NE ? THEN
   DO:
      CLOSE QUERY q1.
               /* Если включен режим "по группам", */
      IF     mByGroup
         AND mGroupList EQ "*"
                  /* применяем сортировку по группе, затем по наименованию, */
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                  /* иначе только по наименованию */
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

      /* позиционируем курсор */
      REPOSITION q1 TO ROWID iRowID NO-ERROR.

      /* обновить форму */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.

   RETURN.
END PROCEDURE.

/* Методы по подписке */
PROCEDURE GetTTXattrValue.
   DEF OUTPUT PARAM TABLE FOR xattr-value.
   DEF OUTPUT PARAM oXAEdHdl AS HANDLE NO-UNDO.

   oXAEdHdl = THIS-PROCEDURE.

   RETURN.
END PROCEDURE.

PROCEDURE SetTTXattrValue.
   DEF INPUT PARAM TABLE FOR xattr-value.
   RETURN.
END PROCEDURE.

/* сохраняем введенные, но не сохраненные значения допреквизитов */
PROCEDURE SaveTmpXattrValues:
DEF BUFFER xattr-value FOR xattr-value.
   EMPTY TEMP-TABLE xattr-value-copy.
   FOR EACH xattr-value:
      CREATE xattr-value-copy.
      BUFFER-COPY xattr-value TO xattr-value-copy.
   END.
END PROCEDURE.

/* восстанавливаем введенные, но не сохраненные значения допреквизитов */
PROCEDURE RestoreTmpXattrValues:
   DEF BUFFER xattr-value FOR xattr-value.
   IF any-entered THEN
   FOR EACH xattr-value-copy:
      FIND FIRST xattr-value WHERE xattr-value.class EQ xattr-value-copy.class
                               AND xattr-value.code  EQ xattr-value-copy.code
      NO-ERROR.
      IF NOT AVAIL xattr-value THEN
      DO:
         CREATE xattr-value.
         BUFFER-COPY xattr-value-copy TO xattr-value.
      END.
      ELSE
         IF xattr-value.code-value NE xattr-value-copy.code-value THEN
            xattr-value.code-value = xattr-value-copy.code-value.
   END.
END PROCEDURE.

/* восстанавливаем начальные значения допреквизитов */
PROCEDURE RestoreInitXattrValues:
DEF BUFFER xattr-value FOR xattr-value.
   FOR EACH xattr-value:
      xattr-value.code-value = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, "").
   END.
END PROCEDURE.

/* Блокирование записи для невозможности редактирования ДР одновременно несколькими пользователями */
PROCEDURE LockRecord PRIVATE.
   DEF INPUT  PARAM iFileName  AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSurrogate AS CHAR   NO-UNDO.

   DEF VAR vOK AS LOG    NO-UNDO.

   FIND FIRST signs WHERE signs.file-name EQ iFileName
                      AND signs.surrogate EQ iSurrogate
   NO-LOCK NO-ERROR.
   IF NOT AVAIL signs THEN
   DO:
      FIND FIRST tmpsigns WHERE tmpsigns.FILE-NAME EQ iFileName
                            AND tmpsigns.surrogate EQ iSurrogate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL tmpsigns THEN 
      DO:
                        /* Если еще нет ни одного значения ДР на объекте,
                        ** создадим служебное значение для того, чтобы было что заблокировать */
                        /* Отключение триггеров, чтобы не писалась история по служебному ДР */
         ON CREATE OF signs OVERRIDE DO: END.
         ON WRITE  OF signs OVERRIDE DO: END.
         DO TRANS:
            CREATE signs.
            ASSIGN signs.FILE-NAME  = iFileName
                   signs.surrogate  = iSurrogate
                   signs.code       = "$lock"
                   signs.code-value = "$lock"
            .
         END.
                        /* Восстанавливаем триггеры */
         ON CREATE OF signs REVERT.
         ON WRITE  OF signs REVERT.
      END.
   END.

   DO TRANS:
      CREATE BUFFER vHBL FOR TABLE (IF AVAIL signs THEN "signs" ELSE  "tmpsigns").

                        /* Попытка блокировки записи */
      &IF DEFINED(ORACLE) &THEN
         CREATE BUFFER vDBufferMutex FOR TABLE "mutex"
            &IF DEFINED(SESSION-REMOTE) = 0 &THEN 
            IN WIDGET-POOL "DATA"
            &ENDIF 
         .
         vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns),NO-LOCK,NO-WAIT) NO-ERROR.
         {mutex-lock-dyn.i
            &RES-FIND   = vOK
            &BUFFER     = vDBufferMutex
            &filename   = (vHBL:TABLE) 
            &recid      = (vHBL:RECID)
            &for-lock   = "EXCLUSIVE"
         }
         IF vOK THEN
            DELETE OBJECT vHBL NO-ERROR.
      &ELSE
         vOK = vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns), EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
      &ENDIF
                        /* Если попытка не удалась - значит доп.реквизиты уже кто-то редактирует */
      IF NOT vOK THEN
      DO:
                        /* Вывод сообщения */
         RUN Fill-SysMes IN h_tmess ("", "", "", 
                            "Ошибка блокировки при изменении дополнительных реквизитов объекта '" + 
                            iSurrogate + "' " + ". Попробуйте позже!").
         &IF DEFINED (ORACLE) &THEN
            vDBufferMutex:FIND-FIRST("where rec-id EQ " + STRING (vHBL:RECID) +
                   "  and filename EQ ~"" +  vHBL:TABLE + "~"",NO-LOCK) NO-ERROR . 
         IF vDBufferMutex:AVAIL AND vDBufferMutex:RECID > 0 THEN
         RUN wholocks2.p (vDBufferMutex:RECID, "mutex", "").
         DELETE OBJECT vHBL NO-ERROR.
         &ELSE
         vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns), NO-LOCK) NO-ERROR.
         IF vHBL:AVAIL THEN
            RUN wholocks2.p (vHBL:RECID, IF AVAIL signs THEN "signs" ELSE "tmpsigns", "").
         &ENDIF
                        /* Выход с ошибкой */
         RETURN ERROR.
      END.
   END.
   RETURN.
END PROCEDURE.

/* Разблокирование записи по окончании редактирования */
PROCEDURE UnLockRecord PRIVATE.
   DEF INPUT  PARAM iFileName  AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSurrogate AS CHAR   NO-UNDO.
   DEF BUFFER signs FOR signs.

   IF VALID-HANDLE (vHBL) THEN
      DELETE OBJECT vHBL NO-ERROR.

   &IF DEFINED(ORACLE) &THEN
   IF VALID-HANDLE (vDBufferMutex) THEN
      DELETE OBJECT vDBufferMutex NO-ERROR.
   &ENDIF
                        /* Отключение триггеров, чтобы не писалась история по служебному ДР */
   ON DELETE OF signs OVERRIDE DO: END.
   ON WRITE  OF signs OVERRIDE DO: END.
   DO TRANS:
                        /* Поиск служебной записи для блокировки */
      FIND FIRST signs WHERE signs.file-name  EQ iFileName
                         AND signs.surrogate  EQ iSurrogate
                         AND signs.code       EQ "$lock"
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                        /* Если нашли такую, удаляем */
      IF AVAIL signs THEN DELETE signs.
   END.
                        /* Восстанавливаем триггеры */
   ON DELETE OF signs REVERT.
   ON WRITE  OF signs REVERT.
   RETURN.
END PROCEDURE.
/* $LINTUSER='SHOI' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='28/01/2015 22:37:42.311+04:00' */
/* $LINTFILE='xattr-ed.p' */
/*prosignzxalI+jeNlsjPag40awFJQ*/