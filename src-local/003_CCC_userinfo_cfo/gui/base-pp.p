{globals.i}

/* +++ base-pp.p was humbly modified by (c)blodd converter v.1.09 on 8/9/2016 9:09am +++ */

/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: base-pp.p
      Comment: персистентная процедура, содержащая основные системные функции
   Parameters: нет
         Uses:
      Used by: инициализация в bislogin.i
      Created: 09/11/99 serge
     Modified: 09/06/2001 shin - добавил обнуление для групповой печати
     Modified: 27/04/2002 NIK  - добавлены функции FGetSetting*
     Modified: 09/03/2003 AVAL - добавлена функция SetValue
     Modified: 11.02.2003 17:14 DEMA     (0012943) Функции работы с паролями
                                         перенесены в pp-right.p
     Modified: 22.04.2004 08:59 KSV      (ош.компиляции) Исправлена
                                         FChkClsDate.
     Modified: 11.08.2004 14:40 KSV      (0034252) Рефакторинг процедуры
                                         SetValue.
     Modified: 07.10.2004 10:15 Om  Ошибка.
                  SetValue неверно обрабатывала значение для логического типа.
     Modified: 14.07.2005 16:09 KSV      (0047805) Добавлена процедура
                                         ResetReturnValue, выполняющая сброс
                                         атрибута RETURN-VALUE.
     Modified: 27.07.2005 14:29 Om  Доработка.
                        Добавлена процедура вывода названия продукта.
     Modified: 17.08.2005 18:23 KSV      (0050131) Подключен системный кэш
     Modified: 07.08.2005       TSL    Многофилиальность в FChkClsDate и FGetLastClsDate
     Modified: 28.05.2006 13:14 KSV      (0042082) Доработана WhoLocks для
                                         проверки локировки через мютекс для
                                         ORACLE
     Modified: 15.09.2006 15:39 Vasov    (0064604) Добавлена функция GetBankInn
     Modified: 24.10.2007 18:06 MUTA     0082119 Реализация количественного учета ценных бумаг в Базовом модуле
     Modified: 25.07.2008 12:29 KSV      (0095920) исправлена RemHeaderMark   
     Modified: 19.11.2008 13:02 KSV      (0100636) изменена точка включения
                                         sysconf, т.к. под QBIS он требует
                                         загруженный pp-tmess
     Modified: 11.03.2009 20:15 ariz     Изменения GetTempXAttrValueEx в связи
                                         с переносом хранения значений темпорированных
                                         реквизитов в таблицу tmpsigns.
     Modified: 25.03.2010 16:26 ksv      (0110628) + методы управления семафором
                                         CloseOnGo                                        
     Modified: 11.06.2010 15:42 ksv      (0129286) исправлен WhoLocks
     Modified: 24.12.2010 11:31 krok     (0133053) Удалена GetCBDocType.
                                         Используйте GetDocTypeDigitalEx из pp-op.p
     Modified: 23.01.2011 13:03 ksv      (0140791) Оптимизация WhoLocks                                         
  процедуры/функции:
     CheckHandle

     Acct-Pos - получение остатков и оборотов по лс
     Cli-Pos  - получение остатков и оборотов по лс клиентов
     Acct-Qty - получение количества ценных бумаг по счету

     CurrCode

     term2str - возвращает строчное наименование периода
     per2str  - возвращает строчное наименование типа периода

     SetModule - устанавливает текущий модуль
     GetModule

     GetCust
     GetUserBranchId - поиск кода подразделения по пользователю

     CheckOpKNF      - проверка документа по инструкции 93-И
     CheckKNF        - проверка реквизитов по инструкции 93-И

     GetXAttrValue   - возврат значения допреквизита
     GetXAttrValueEx - возврат значения допреквизита
     
     GetTempXAttrValueEx - возвращет значение ТЕМПОРИРОВАННОГО допреквизита 
                           на ПРОИЗВОЛЬНУЮ дату с учетом значения по умолчанию
     GetTempXAttrValueEx - возвращет значение ТЕМПОРИРОВАННОГО допреквизита
                           на ГЛОБАЛЬНУЮ дату. Если не найден, то возвращает пустое значение
     
     GetXAttrSurr    - Возвращает список сурогатов по значению

     FGetSetting     - Возврат значения настройки
     FGetSettingEx   - Возврат значения настройки с учетом message mode.

     SetValue        - приведение значения к формату;
                       при ошибке - формирование сообщения и выставление признака ошибки
     SearchPfile     - проверка наличия r-кода или p-кода по Propath
     Holiday         - проверка является ли день нерабочим

     GetBank         - поиск банка по идентификатору и его значению
     GetBankInn      - получение ИНН банка
     GetTrueCodeType - возвращает код родительского классификатора для bank-code-type
     WhoLocks        - возвращает да, если удалось определить кто держит запись
                       выходным параметром является информационная строка
     GetAttrValueOnDate   - Определение значения реквизита объекта на заданный
                            момент времени по журналу изменений
     GetAttrValueOnDateEx - Альтернативный вариант вызова GetAttrValueOnDate
  Инструменты для проверок на открытый-закрытый день и возможности совершения операций :

     CheckOpRight      - Проверка прав на совершение определенной операции с документом
                         в опердне
     CheckOpEx         - Проверка определенных прав на документ
     CheckOpDateEx     - Проверка определенных прав на опердень

     Переведены на использование CheckOpEx,CheckOpDateEx:
        CheckDate         - Проверка по дню и неопределенной категории с сообщением (chkdate.i chkdate1.i)
        CheckDateNoMsg    - Проверка по дню и неопределенной категории без сообщения
        CheckCatDateNoMsg - Проверка по дню и определенной категории
        CheckOpDate       - Проверка по документу (chkdate2.i)


     FChkClsDate     - Контроль закрытости дня (возвращает YES, NO)
     FGetLastClsDate - Возвращает дату последнего закрытого дня, на переданную дату
*/
{globals.i}

h_base = THIS-PROCEDURE.

{sh-defs.i}
{zo-defs.i}
{sh-temp.i}

{intrface.get cache}
{getsett.fun}
{gstag.i}               /* Работа со строкой в теговом формате. */
{intrface.get xclass}
{intrface.get rights}
{intrface.get separate} /* Инструменты для проверки возможности совершения операции */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get osyst}    /* Инструменты для работы с Oracle (системные) */
{intrface.get isrv}     /* Инструменты для работы с внутренним счетчиком(только для oracle) */
{intrface.get db2l}
{intrface.get hist}
{calendar.def}

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "BASE-PP"
   &LIBNAME       = "Библиотека основных функций"
   &DESCRIPTION   = "Библиотека основных функций"
   }

/* Commented by KSV: Под QBIS требует загруженного pp-tmess.p */
{sysconf.i} /*Библиотека процедур для работы с внутренней таблицей SysConf*/

def var codenatcurr  like  currency.currency no-undo.
def var balschinn    as    char              no-undo.
def var bank-inn     as    char              no-undo.

ASSIGN
   codenatcurr = FGetSetting("КодНацВал",?,"")
   balschinn   = FGetSetting("БалСчИНН",?,?)
   bank-inn    = FGetSetting("ИНН",?,?)
.

{userfunc.i} /* Функции для работы с доп. реквизитами класса _User */
{getcust.pro}
{getbank.pro}
{filial.pro}            /* Инструменты для работы с многофилиальной БД. */
{mf-loan.i}             /* Функции addFilToLoan и delFilFromLoan преобразований
                        ** полей loan.doc-ref в loan.cont-code и наоборот. */
{xattrtmp.def}          /* Таблица содержит перечень темпоральных ДР. */

&GLOBAL-DEFINE def-stream-log YES /* временно */
DEFINE STREAM lock-log.

/* Загружаем текущий модуль */
def var work-module as char init "0" no-undo.
Procedure SetModule.
    def input param in-module as char no-undo.
    work-module = in-module.
end.

Procedure GetModule.
    def output param in-module as char no-undo.
    in-module = work-module.
end.

procedure SetUser.
   DEF VAR vBranchId AS CHAR  NO-UNDO. /* Код подразделения. */
   DEF VAR vTimeZone AS CHAR  NO-UNDO. /* Часовой пояс для филиала (ДР timezone) */
   DEF VAR vTZInt    AS INT64 NO-UNDO. /* Сдвиг времени часового пояса в минутах */

   {otdel2.i}

   assign
     corp-ok       = GetTablePermission("cust-corp", "r")
     priv-ok       = GetTablePermission("person", "r")
     int-ok        = Yes
     bank-ok       = GetTablePermission("banks", "r")
     access        = ""
     usr-printer   = getThisUserXAttrValue('Принтер')
     acct-look     = getThisUserXAttrValue('Л~/С')
     ClassAcctPosView = getThisUserXAttrValue('ClassAcctPosView')
     rightview     = getThisUserXAttrValue('СтатусПросм')
     otdel-lst     = getThisUserXAttrValue('ОтделенияТр')
     userids       = getSlaves()
     balschinn     = FGetSetting("БалСчИНН",?,?)
     bank-inn      = FGetSetting("ИНН",?,?)     
   .

   IF NOT CAN-DO(userids,userid('bisquit')) THEN DO:
       {additem.i userids USERID('bisquit')}
   END.

                        /* Устанавливаем часовой пояс для филиала, если он задан в ДР timezone */
   SESSION:TIMEZONE = ?.
   /* Commented by SOAV: приоритет ДР timezone у branch (0187977) */
   vBranchId = getThisUserXAttrValue ("Отделение").
   vTimeZone = GetXAttrValue ("branch", vBranchId, "timezone").
   IF NOT {assigned vTimeZone} THEN
   vTimeZone = GetXAttrValue ("branch", shFilial, "timezone").
   IF {assigned vTimeZone} THEN
   DO:
      vTZInt = ( IF vTimeZone BEGINS "-" THEN -1 ELSE 1) * 
               (INT64 (ENTRY (1, TRIM (vTimeZone, "+-"), ":")) * 60 + 
               INT64 (ENTRY (2, vTimeZone, ":"))) NO-ERROR.
      IF     NOT ERROR-STATUS:ERROR
         AND vTZInt NE ?
      THEN
         SESSION:TIMEZONE = vTZInt.
   END.

   run SetUserRights in h_rights.
                        /* Если сессия не запущена в режиме READ-ONLY. */
   IF DBRESTRICTIONS ("bisquit") NE "READ-ONLY" THEN
   DO:
                       /* установка режима частных буферов, при наличии у пользователя
                       ** доперквизита PrivateBuffers. */
      {longrpt.i &after}
   END.   

   {userinfo.i}

end procedure.

procedure CheckHandle:
  /* does nothing */
end procedure.

/* проверка наличия r-кода или p-кода */
function SearchPfile returns log
         (input filename as char ):
  IF filename = "" OR filename = ? THEN
     RETURN NO.
  IF SEARCH(filename + ".r") <> ? or
     SEARCH(filename + ".p") <> ? then
     return yes.
  else
    return no.
end.

/* Функция проверяет, является ли день нерабочим */
FUNCTION Holiday RETURNS LOGICAL (INPUT iDate AS DATE).
   RETURN HolidayExt(iDate, "", "").
END FUNCTION.

{pfuncdef 
  &DefProc="HolidayExt"
  &Description="Функция проверяет, является ли день нерабочим в разрезе типа и среза календаря"
  &Parameters=" "
  &Result=" "
  &Sample=" "}
FUNCTION HolidayExt RETURNS LOGICAL (INPUT iDate AS DATE,
                                     INPUT iType AS CHAR,
                                     INPUT iBranch AS CHAR).
   DEFINE VARIABLE vFlag  AS LOGICAL INIT NO NO-UNDO.
   DEFINE VARIABLE vProg  AS CHAR INIT "" NO-UNDO.
   DEFINE VARIABLE vHolid AS LOG  INIT NO NO-UNDO.

   DEFINE BUFFER   bHoliday FOR holiday.
   DEFINE BUFFER   bCode    FOR code.

   IF iType   EQ ? THEN iType   = "".
   IF iBranch EQ ? THEN iBranch = "".

              /* используем проверенный функционал */
   if (WEEKDAY(iDate) MODULO 6) EQ 1 THEN
      vFlag = YES. /* выходной */

              /* поиск процедуры календаря */
   FOR FIRST bCode WHERE bCode.class EQ {&CALND-CODE}
                     AND bCode.code  EQ iType
                         NO-LOCK :
      vProg = bCode.misc[2].
   END.                  
              /* указана прoцедура - динамический календарь */
   IF {assigned vProg} AND SearchPFile (vProg) THEN
   DO:
      RUN VALUE(vProg) (INPUT iDate, INPUT iType, INPUT iBranch, OUTPUT vHolid) NO-ERROR.
      vFlag = vHolid.
   END.
              /* поиск в разрезе типов и секций календаря, индекс holidate */
   FOR FIRST bHoliday WHERE bHoliday.op-date EQ iDate 
                        AND bHoliday.code    EQ iType
                        AND bHoliday.branch-id EQ iBranch
                            NO-LOCK :
      vFlag = NOT vFlag.
   END.
   RETURN vFlag.
END FUNCTION.

function term2str returns char (input in-beg-date as date, in-end-date as date):
   DEFINE VARIABLE iBegDate AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iEndDate AS CHARACTER NO-UNDO.

   IF    (     YEAR (in-beg-date) GE  SESSION:YEAR-OFFSET
          AND  YEAR (in-beg-date) LT (SESSION:YEAR-OFFSET + 100))
     AND (     YEAR (in-end-date) GE  SESSION:YEAR-OFFSET
          AND  YEAR (in-end-date) LT (SESSION:YEAR-OFFSET + 100))
   THEN
      ASSIGN
         iBegDate = STRING (in-beg-date,"99/99/99")
         iEndDate = STRING (in-end-date,"99/99/99")
      .
   ELSE
      ASSIGN
         iBegDate = STRING (in-beg-date,"99/99/9999")
         iEndDate = STRING (in-end-date,"99/99/9999")
      .

   return
      if in-beg-date eq in-end-date 
      then 
          string(day(in-beg-date),"99") + " " + 
          entry(month(in-beg-date), "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") + " " + 
          string(year(in-beg-date)) + " г."
      else if day(in-beg-date) = 1 and day(in-end-date + 1) = 1 and year(in-end-date) = year(in-beg-date) 
           then if month(in-end-date) = month(in-beg-date) 
                then entry(month(in-beg-date), "январь,февраль,март,апрель,май,июнь,июль,август,сентябрь,октябрь,ноябрь,декабрь") + " " + string(year(in-beg-date)) + " г."
                else if month(in-end-date) - month(in-beg-date) = 2 and month(in-beg-date) modulo 3 = 1 
                     then string(INT64((month(in-beg-date) / 3) + 1)) + " квартал " + string(year(in-beg-date)) + " г."
                     else if in-end-date = date(12,31,year(in-end-date)) and in-beg-date = date(1,1,year(in-end-date)) 
                          then string(year(in-beg-date)) + " год"
                          else if month(in-beg-date) = 1 and (month(in-end-date) - month(in-beg-date)) modulo 3 = 2 
                               then "1-" + string(INT64((month(in-end-date) / 3))) + " кварталы " + string(year(in-beg-date)) + " г."
                               else if month(in-end-date) - month(in-beg-date) = 5 and month(in-beg-date) modulo 6 = 1 
                                    then string(INT64(month(in-end-date) / 6)) + '-е полугодие ' + string(year(in-beg-date)) + " г."
                                    else iBegDate + "-" + iEndDate
           else ( IF in-beg-date = ? then "  /  /  " else iBegDate) + "-" + ( IF in-end-date = ? then "  /  /  " else iEndDate)

   .
end.

FUNCTION term2strEng RETURNS CHAR (INPUT in-beg-date AS DATE, in-end-date AS DATE):
   DEFINE VARIABLE iBegDate AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iEndDate AS CHARACTER NO-UNDO.

   IF    (     YEAR (in-beg-date) GE  SESSION:YEAR-OFFSET
          AND  YEAR (in-beg-date) LT (SESSION:YEAR-OFFSET + 100))
     AND (     YEAR (in-end-date) GE  SESSION:YEAR-OFFSET
          AND  YEAR (in-end-date) LT (SESSION:YEAR-OFFSET + 100))
   THEN
      ASSIGN
         iBegDate = STRING (in-beg-date,"99/99/99")
         iEndDate = STRING (in-end-date,"99/99/99")
      .
   ELSE
      ASSIGN
         iBegDate = STRING (in-beg-date,"99/99/9999")
         iEndDate = STRING (in-end-date,"99/99/9999")
      .

   RETURN
      IF in-beg-date EQ in-end-date
         THEN STRING(DAY(in-beg-date),"99") + " " + ENTRY(MONTH(in-beg-date), "january,february,march,april,may,june,july,august,september,october,november,december") + " " + STRING(YEAR(in-beg-date))
         ELSE IF DAY(in-beg-date) = 1 AND DAY(in-end-date + 1) = 1 AND YEAR(in-end-date) = YEAR(in-beg-date)
              THEN IF MONTH(in-end-date) = MONTH(in-beg-date) 
                   THEN ENTRY(MONTH(in-beg-date), "january,february,march,april,may,june,july,august,september,october,november,december") + " " + STRING(YEAR(in-beg-date))
                   ELSE IF MONTH(in-end-date) - MONTH(in-beg-date) = 2 AND MONTH(in-beg-date) MODULO 3 = 1 
                        THEN STRING(INT64((MONTH(in-beg-date) / 3) + 1)) + " quarter " + STRING(YEAR(in-beg-date))
                        ELSE IF in-end-date = date(12,31,YEAR(in-end-date)) AND in-beg-date = date(1,1,YEAR(in-end-date)) 
                             THEN STRING(YEAR(in-beg-date))
                             ELSE IF MONTH(in-beg-date) = 1 AND (MONTH(in-end-date) - MONTH(in-beg-date)) MODULO 3 = 2 
                                  THEN "1-" + STRING(INT64((MONTH(in-end-date) / 3))) + " quarters " + STRING(YEAR(in-beg-date))
                                  ELSE IF MONTH(in-end-date) - MONTH(in-beg-date) = 5 AND MONTH(in-beg-date) MODULO 6 = 1 
                                       THEN STRING(INT64(MONTH(in-end-date) / 6)) + 'half-year ' + STRING(YEAR(in-beg-date))
                                       ELSE iBegDate + "-" + iEndDate
      ELSE ( IF in-beg-date = ? THEN "  /  /  " ELSE iBegDate) + "-" + ( IF in-end-date = ? THEN "  /  /  " ELSE iEndDate)
   .
END.

function per2str returns char (input in-beg-date as date, in-end-date as date):
    DEFINE VARIABLE vstr AS CHARACTER   NO-UNDO.
    vstr = {sv-tstr in-beg-date in-end-date} .
   RETURN vstr.
end.

procedure CurrCode:
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: CurrCode.P
      Comment: Возвращает код валюты, задаваемой input параметром in-code,
      Comment: в output параметре out-code, в альтернативной классификации,
      Comment: заданной в настроечных параметрах параметром "КодыВал".
      Comment: Если параметр "КодыВал" не задан, возвращает in-code.
   Parameters: input : in-code = currency.currency
   Parameters: output: out-code - код валюты в альтернативной классификации
         Uses: globals.i
      Used by:
      Created: 17/07/1997 Dima
     Modified:
*/

   def input param in-code   like currency.currency no-undo.
   def output param out-code like currency.currency no-undo.

   DEF VAR AltCurr AS CHAR   NO-UNDO.

   AltCurr = FGetSetting("КодыВал",?,"").
   if in-code = "" then
      out-code = codenatcurr.
   else if AltCurr = "" then
      out-code = in-code.
   else do:
      {profind.i &FWAY=FIRST &FBUF=code &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE code.class = AltCurr and
                       code.code  = in-code"}
      if avail code then
         out-code = code.val.
      else
         RUN Fill-AlertSysMes IN h_tmess("","",-1,"Нет валюты с кодом " + STRING(in-code) + " в классификаторе " + STRING(AltCurr) + "!").

   end.
end procedure.


/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename: acct-pos
      Comment: универсальная процедура расчета остатка и оборотов по счету
   Parameters: many
         Uses: acct-pos.i
     Modified: 24/08/1998 Olenka - все переделано
*/

/* устанавливается в bqglset */
def new global shared var GCheck-Op as logical no-undo.

{acct-pos.i &Type = "acct-pos" &defproc=yes}
{acct-pos.i &Type = "cli-pos"  &defproc=yes}
{acct-pos.i &Type = "acct-qty" &defproc=yes}
{acct-pos-pure.pro}
{acct-pos-pure.pro &sh-temp = "-sh"}


/* Проверка по дню и неопределенной категории с сообщением */
PROCEDURE CheckDate.
   DEFINE INPUT  PARAMETER iOpDate  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vMsg    AS CHARACTER NO-UNDO.

   RUN CheckOpDateEx(iOpDate,
                     NO,
                     iStatus NE "статус",
                     "",
                     OUTPUT vMsg).

   IF vMsg NE "" THEN DO:
      RUN Fill-AlertSysMes IN h_tmess("","",1,STRING(vMsg)).

      RETURN.
   END.
   ELSE RETURN "ok".
END PROCEDURE.

/* Проверка по дню и неопределенной категории без сообщения */
PROCEDURE CheckDateNoMsg.
   DEFINE INPUT  PARAMETER iOpDate  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oMsg     AS CHARACTER NO-UNDO.

   RUN CheckOpDateEx(iOpDate,
                     NO,
                     iStatus NE "статус",
                     "",
                     OUTPUT oMsg).

END PROCEDURE.

/* Проверка по дню и определенной категории */
PROCEDURE CheckCatDateNoMsg.
   DEFINE INPUT  PARAMETER iOpDate  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oMsg     AS CHARACTER NO-UNDO.

   RUN CheckOpDateEx(iOpDate,
                     NO,
                     iStatus NE "статус",
                     iAcctCat,
                     OUTPUT oMsg).

END PROCEDURE.

/* Проверка по документу */
PROCEDURE CheckOpDate.
   DEFINE INPUT  PARAMETER iOp     AS INT64   NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oMsg    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iStatOp AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vOpRec AS RECID      NO-UNDO.
   DEFINE VARIABLE vRight AS CHARACTER  NO-UNDO.

   {profind.i &FWAY=FIRST &FBUF=op &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE op.op EQ iOp"}
   IF NOT AVAIL op THEN DO:
       oMsg = "Документ удален.".
       RETURN.
   END.

   vRight = getThisUserXAttrValue('СтатусРед').
   RUN CheckOpEx(RECID(op),
                 NO,
                 YES,
                 vRight,
                 NO,
                 iStatOp EQ "ПРИЗМДОК",
                 OUTPUT oMsg).
   IF oMsg > "" THEN
      RETURN.

   {profind.i &FWAY=FIRST &FBUF=op &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE op.op EQ iOp"}
   IF NOT AVAIL op THEN DO:
       oMsg = "Документ удален.".
       RETURN.
   END.
   RUN CheckOpDateEx(op.op-date,
                     NO,
                     iStatus NE "статус",
                     op.acct-cat,
                     OUTPUT oMsg).

END PROCEDURE.

/*  Универсальная проверка наличия и прав на совершение действий с документом в опердне  */
/*  Возможные коды операций над документами:                                             */
/*  VIEW   - Просмотр документа                                                          */
/*  Ins    - Создание документа                                                          */
/*  Upd    - Редактирование документа                                                    */
/*  Del    - Удаление документа                                                          */
/*  Signs  - Редактирование допреквизитов документа                                      */
/*  Copy   - Копирование документа                                                       */
/*  ChgSts - Изменение статуса документа                                                 */
/*  ChgDt  - Изменение даты документа                                                    */
/*  Undo   - Откат документа                                                             */
/*  Ann    - Аннуляция документа                                                         */

PROCEDURE CheckOpRight.
   DEFINE INPUT PARAMETER iOpRec   AS RECID     NO-UNDO. /* Номер записи документа*/
   DEFINE INPUT PARAMETER iDate    AS DATE      NO-UNDO. /* Дата опердня*/
   DEFINE INPUT PARAMETER iCodOper AS CHARACTER NO-UNDO. /* Код операции над документом*/

   DEFINE VARIABLE vMess  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRight AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vFlag  AS LOGICAL   NO-UNDO.

   FIND FIRST op WHERE RECID(op) EQ iOpRec NO-LOCK NO-ERROR.

                  /* Проверка прав удаления объектов подчиненных */
   IF iCodOper EQ "Del" 
   THEN DO:
      IF     USERID("bisquit") NE op.user-id
         AND NOT GetSlavePermissionBasic (USERID("bisquit"),op.user-id,"d") 
      THEN DO:
         vMess = "Вы не имеете права удалять объекты пользователя '" + op.user-id + "'.".
         RETURN ERROR vMess.
      END.
   END.
                  /* Проверка прав редактирования объектов подчиненных */
   ELSE IF iCodOper NE "View"
   THEN DO:
      IF     USERID("bisquit") NE op.user-id
         AND NOT GetSlavePermissionBasic (USERID("bisquit"),op.user-id,"w") 
      THEN DO:
         vMess = "Вы не имеете права редактировать объекты пользователя '" + op.user-id + "'.".
         RETURN ERROR vMess.
      END.
   END.

   CASE iCodOper:
      WHEN "View" THEN DO:
         RUN CheckOpEx(iOpRec,YES,NO,?,NO,NO,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      WHEN "Ins" THEN DO:
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(iDate,NO,YES,"",OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      WHEN "Copy" OR
      WHEN "ChgSts" THEN DO:
         vRight = IF iCodOper EQ "Copy"
                  THEN ?
                  ELSE getThisUserXAttrValue('СтатусИзм')
         .
         RUN CheckOpEx(iOpRec,NO,NO,vRight,NO,NO,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           NO,
                           IF iCodOper EQ "Copy"
                           THEN YES
                           ELSE NO,
                           op.acct-cat,
                           OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      WHEN "Upd" OR
      WHEN "Del" THEN DO:
         vRight = getThisUserXAttrValue('СтатусРед').
         RUN CheckOpEx(iOpRec,NO,YES,vRight,NO,YES,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           NO,
                           YES,
                           op.acct-cat,
                           OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      WHEN "Signs" THEN DO:
         vRight = getThisUserXAttrValue('СтатусРед').
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         vDate = IF iDate EQ ?
                 THEN op.op-date
                 ELSE iDate.
         vFlag =     getThisUserXattrValue("ИзмДопРекв") = "имеет"
                 AND (   vDate        LE Get_Date_Cat("b",?)
                      OR op.op-status GE FGetSetting("ПРИЗМДОК",?,CHR(251) + CHR(251))
                     ).                

         RUN CheckOpEx(iOpRec,
                       NO,
                       YES,
                       IF vFlag
                       THEN ?
                       ELSE vRight,
                       NO,
                       IF  vFlag
                       THEN NO
                       ELSE YES,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" 
            AND getThisUserXattrValue("ИзмДопРекв") NE "имеет" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           NO,
                           YES,
                           op.acct-cat,
                           OUTPUT vMess).
         IF     vMess > ""
            AND getThisUserXattrValue("ИзмДопРекв") NE "имеет" THEN
            RETURN ERROR vMess.
      END.
      WHEN "UpdDetails" THEN DO:
         vRight = getThisUserXAttrValue('СтатусРед').
         RUN CheckOpEx(iOpRec,NO,YES,vRight,NO,NO,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           NO,
                           YES,
                           op.acct-cat,
                           OUTPUT vMess).
         IF     vMess > ""
            AND getThisUserXattrValue("ИзмЗакОД") EQ "не имеет" THEN
            RETURN ERROR vMess.
      END.
      WHEN "Undo" OR
      WHEN "Ann" THEN DO:
         vRight = IF iCodOper EQ "Undo"
                  THEN getThisUserXAttrValue('СтатусОткат')
                  ELSE getThisUserXAttrValue('СтатусАннул')
         .
         RUN CheckOpEx(iOpRec,NO,YES,vRight,NO,NO,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           NO,
                           IF iCodOper EQ "Undo"
                           THEN NO
                           ELSE YES,
                           op.acct-cat,
                           OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      WHEN "ChgDt" THEN DO:
         vRight = getThisUserXAttrValue('СтатусРед').
         RUN CheckOpEx(iOpRec,NO,YES,vRight,YES,YES,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         FIND FIRST op WHERE
              RECID(op) EQ iOpRec NO-LOCK NO-ERROR.
         RUN CheckSessions (iOpRec,OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
         RUN CheckOpDateEx(IF iDate EQ ?
                           THEN op.op-date
                           ELSE iDate,
                           YES,
                           YES,
                           op.acct-cat,
                           OUTPUT vMess).
         IF vMess > "" THEN
            RETURN ERROR vMess.
      END.
      OTHERWISE
         RETURN ERROR "Недопустимая операция над документом!".
   END CASE.
END PROCEDURE.


/* Универсальная проверка наличия и прав на документ */
PROCEDURE CheckOpEx.
   DEFINE INPUT PARAMETER iOpRec     AS RECID     NO-UNDO. /* Номер записи документа*/
   DEFINE INPUT PARAMETER iAcctRight AS LOGICAL   NO-UNDO. /* Если нет доступа к документу проверять ли
                                                              на наличие доступа к счетам проводок*/
   DEFINE INPUT PARAMETER iFlSal     AS LOGICAL   NO-UNDO. /* Проверять на принадлежность мод Зарплата*/
   DEFINE INPUT PARAMETER iStat      AS CHARACTER NO-UNDO. /* Статусы для проверки откат/анн/присв/изм даты */
   DEFINE INPUT PARAMETER iFlDate    AS LOGICAL   NO-UNDO. /* Проверять на возможность переноса */
   DEFINE INPUT PARAMETER iChkMaxSts AS LOGICAL   NO-UNDO. /* Проверка на максимальный статус, с кот можно изменять док-т*/

   DEFINE OUTPUT PARAMETER oMsg       AS CHARACTER NO-UNDO. /*Сообщение об ошибке*/

   DEFINE BUFFER acct     FOR acct.
   DEFINE BUFFER op-entry FOR op-entry.

   DEFINE VARIABLE vPrIzmDoc    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIzmDate     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLockStr     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDostDocAcct AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vUserids     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRightView   AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE
        RECID(op) EQ iOpRec EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAILABLE op THEN DO:
      IF    LOCKED(op) THEN DO:
         IF WhoLocks2(INPUT        iOpRec, 
                     "op",
                     INPUT-OUTPUT vLockStr) 
         THEN oMsg = vLockStr.
         ELSE oMsg = "Документ редактируется другим пользователем.".
      END.
      ELSE
         oMsg = "Документ  удален другим пользователем.".
      RETURN.
   END.
   FIND CURRENT op NO-LOCK NO-ERROR.

   ASSIGN
      vRightView     = getThisUserXAttrValue('СтатусПросм')
      vUserids       = getSlaves()
   .

   IF NOT CAN-DO(vUserids,userid('bisquit')) THEN
      {additem.i vUserids USERID('bisquit')}

   vDostDocAcct = getThisUserXAttrValue('ДостДокПоСчет') EQ 'Да'.

   IF NOT CAN-DO(vRightView,op.op-status) THEN 
   bl:
   DO:
      /* если установлен ДР ДостДокПоСчет в значение Да */
      /* проверяем подчиненных и счета проводок */
      IF vDostDocAcct THEN DO:
         IF NOT CAN-DO(vUserids,op.user-id) THEN DO:
            IF iAcctRight THEN DO:
               FOR EACH op-entry OF op NO-LOCK:
                  {find-act.i
                     &acct = op-entry.acct-db
                     &curr = op-entry.currency
                  }
                  IF     AVAIL acct 
                     AND AcctView(BUFFER acct) 
                  THEN LEAVE bl.

                  {find-act.i
                     &acct = op-entry.acct-cr
                     &curr = op-entry.currency
                  }
                  IF     AVAIL acct 
                     AND AcctView(BUFFER acct) 
                  THEN LEAVE bl.
               END.
            END.
         END.
         ELSE
            LEAVE bl.
      END.
      oMsg = "Вы не имеете права работать с этим документом".
      RETURN.
   END.

   IF iFlDate THEN DO:
      vIzmDate = FGetSetting("ИзмДтДок",?,"").
      IF vIzmDate > "" AND CAN-DO(vIzmDate,op.op-status) THEN DO:
         oMsg = 'Документ со статусом "' + op.op-status + '" нельзя переносить в другой день!'.
         RETURN.
      END.
   END.

   IF iChkMaxSts THEN DO:
       vPrIzmDoc = FGetSetting("ПРИЗМДОК",?,"").
       IF     vPrIzmDoc GT ""
          AND GetCode("Статус",vPrIzmDoc) NE ?
          AND op.op-status GE vPrIzmDoc THEN DO:

          oMsg = "С документом, имеющим статус " + op.op-status +
                 " уже ничего нельзя делать!~nОбратитесь к администратору.".
          RETURN.
       END.
   END.
   IF     iStat NE ?
      AND NOT CAN-DO(iStat,op.op-status) THEN DO:
      oMsg = "У вас нет прав работать с документами такого статуса.".
      RETURN.
   END.
   RELEASE op.
END PROCEDURE.

/* Универсальная проверка наличия, прав на опердень */
PROCEDURE CheckOpDateEx.
   DEFINE INPUT  PARAMETER iDate   AS DATE      NO-UNDO. /*Дата опердня*/
   DEFINE INPUT  PARAMETER iNoDay  AS LOGICAL   NO-UNDO. /*Проверять наличие опердня*/
   DEFINE INPUT  PARAMETER iChkCls AS LOGICAL   NO-UNDO. /*Проверять закрыт ли день*/
   DEFINE INPUT  PARAMETER iCat    AS CHARACTER NO-UNDO. /*По какой категории проверять закр опердня*/
   DEFINE OUTPUT PARAMETER oMsg    AS CHARACTER NO-UNDO. /*Сообщение об ошибке*/

   DEF VAR vOpDateAv AS LOG NO-UNDO INIT YES. /* Признак, есть ли такой ОД в системе. */

   IF iDate EQ ? THEN
      RETURN.
                        /* Захват и проверка ОД. */
   UNDONOOPDATE:
   DO ON ERROR UNDO UNDONOOPDATE, LEAVE UNDONOOPDATE:
      {daylock.i
         &in-op-date = iDate
         &lock-type  = SHARE
         &cats       = iCat
         &return-mes = oMsg
         &undo-act-l = "RETURN."
         &undo-act-a = "IF iNoDay
                        THEN ASSIGN
                           oMsg        = ''
                           vOpDateAv   = NO
                        .
                        UNDO UNDONOOPDATE, LEAVE UNDONOOPDATE."
      }
   END.
                        /* Снимаем блокировку если существует ОД. */
   IF vOpDateAv
   THEN DO:
      {rel-date.i &in-op-date = iDate &cats = iCat}
   END.
   ELSE DO:
                        /* Если ОД не найден и требуется такая проверка,
                        ** то формируем сообщение об ошибке. */
      IF NOT iNoDay
      THEN DO:
         oMsg = "ОПЕР.день " + STRING(iDate, "99/99/9999") + " не найден".
         RETURN.
      END.
                        /* Если ОД не найден и проверки на отсутствие ОД
                        ** не требуется, то проверяем попадает ли данная дата в 
                        ** интервал закрытых ОД. Если да то формируем сообщение
                        ** об ошибке. */
      ELSE IF iDate LE FGetLastClsDate(?, "*")
      THEN DO:
         oMsg =   "Дата " + STRING(iDate, "99/99/9999") +
                  " лежит в области закрытых операционных дней.".
         RETURN.
      END.
   END.
                        /* Проверка на закрытые дни, если это требуется
                        ** и существует ОД. */
   IF       iChkCls 
      AND   vOpDateAv
   THEN DO:
      IF GetCode("acct-cat",iCat) EQ ? THEN DO: /* неизвестная категория */
         IF Chk_Date_AllCat (iDate) THEN DO:
            oMsg = "Этот опеpационный день уже закpыт по всем категориям - нельзя вносить испpавления".
            RETURN.
         END.
      END.
      ELSE DO:
         IF Chk_Date_Cat (iDate,iCat) THEN DO:
            oMsg = "День " + STRING(iDate,"99/99/9999") + ' уже закрыт по категории "' + iCat + '" - ' + Cat_Name(iCat) + " - нельзя вносить испpавления".
            RETURN.
         END.
      END.
   END.

   IF work-module NE "nalog" THEN DO: /* отключаем все проверки, если налоговый учет */

      {chkdmnmx.i
          &in-op-date = iDate
          &action     = "oMsg = ""Вы не имеете права работать в этом операционном дне!"".~
                         RETURN."
      }

      {chkblock.i
        &surr   = STRING(iDate)
        &action = "oMsg = ""Вы не имеете права работать в заблокированном операционном дне!""."
      }
   END.
END PROCEDURE.

/* Проверка отношения документа к закрытой/выверенной смене ВОК, если смена не находится в 
статусе 'ОТКРЫТА' будем ругаться. */
PROCEDURE CheckSessions:
   DEFINE INPUT   PARAMETER iOpRec  AS RECID                   NO-UNDO. /* Номер записи документа*/
   DEFINE OUTPUT  PARAMETER oMsg    AS CHARACTER   INITIAL ""  NO-UNDO. /*Сообщение об ошибке*/

   DEFINE VARIABLE vLockStr      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIntDprId     AS INT64     NO-UNDO.
   DEFINE VARIABLE vLogCloseSes  AS LOGICAL     NO-UNDO.

   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER sessions  FOR sessions.

   FIND FIRST op WHERE
        RECID(op) EQ iOpRec EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAILABLE op THEN DO:
      IF    LOCKED(op) THEN DO:
         IF WhoLocks2(INPUT        iOpRec, 
                       "op",
                     INPUT-OUTPUT vLockStr) 
         THEN oMsg = vLockStr.
         ELSE oMsg = "Документ редактируется другим пользователем.".
      END.
      ELSE
         oMsg = "Документ  удален другим пользователем.".
      RETURN.
   END.
   FIND CURRENT op NO-LOCK NO-ERROR.

   vIntDprId = INT64(GetXAttrValueEx("op",STRING(op.op),"Dpr-Id","_ERROR_")) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      {profind.i &FWAY=FIRST &FBUF=sessions &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE sessions.dpr-id EQ vIntDprId"}
      vLogCloseSes = AVAILABLE sessions AND sessions.dpr-status NE "ОТКРЫТА".
   END.
   IF vLogCloseSes AND getThisUserXattrValue("ИзмДокум") NE "Имеет" THEN
   DO:
      oMsg = "Эта смена уже закpыта - нельзя вносить испpавления.".
      RETURN.
   END.
END PROCEDURE.

/* возвращет значение ТЕМПОРИРОВАННОГО допреквизита на ПРОИЗВОЛЬНУЮ дату
** с учетом значения по умолчанию */
FUNCTION GetTempXAttrValueEx2 RETURN CHAR (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR,
   INPUT in-Date     AS DATE,
   INPUT in-nofound  AS CHAR,
   INPUT iRetUnknown AS LOG
):
   DEF VAR vValue    AS CHAR   NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO. /* Значение ДР. */

   /* старый суррогат "commission,acct,currency,kau,min-value,period,since" меняем на новый "comm-rate-id" */
   DEF BUFFER comm-rate FOR comm-rate.
   IF in-FileName EQ "comm-rate" AND NUM-ENTRIES(in-surr) > 3 THEN
   DO:
      IF ENTRY(4,in-surr) <> "" THEN
         {profind.i &FWAY=FIRST &FBUF=comm-rate &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE 
                    comm-rate.commission = ENTRY(1,in-surr)        AND
                    comm-rate.acct       = ENTRY(2,in-surr)        AND
                    comm-rate.currency   = ENTRY(3,in-surr)        AND
                    comm-rate.kau        = ENTRY(4,in-surr)        AND
                    comm-rate.min-value  = DEC(ENTRY(5,in-surr))   AND
                    comm-rate.period     = INT64(ENTRY(6,in-surr)) AND
                    comm-rate.since      = DATE(ENTRY(7,in-surr))  "} 
      ELSE
         {profind.i &FWAY=FIRST &FBUF=comm-rate &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE
                    comm-rate.filial-id  = shfilial                AND
                    comm-rate.branch-id  = ''                      AND
                    comm-rate.commission = ENTRY(1,in-surr)        AND
                    comm-rate.acct       = ENTRY(2,in-surr)        AND
                    comm-rate.currency   = ENTRY(3,in-surr)        AND
                    comm-rate.kau        = ENTRY(4,in-surr)        AND
                    comm-rate.min-value  = DEC(ENTRY(5,in-surr))   AND
                    comm-rate.period     = INT64(ENTRY(6,in-surr)) AND
                    comm-rate.since      = DATE(ENTRY(7,in-surr))  "} 
      IF AVAIL comm-rate THEN
         in-surr = STRING(comm-rate.comm-rate-id).
   END.
                        /* Проверка, является ли реквизит темпоральным. */
   IF CAN-FIND (FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ in-FileName
                                    AND ttXattrTemp.fXattr   EQ in-Code)
   THEN DO:
      {profind.i &FWAY=LAST &FBUF=tmpsigns &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE tmpsigns.file-name  EQ in-FileName
                   AND tmpsigns.code       EQ in-Code
                   AND tmpsigns.surrogate  EQ in-Surr
                   AND tmpsigns.since      LE in-Date"}
      IF AVAIL tmpsigns THEN vValue = IF tmpsigns.code-value NE "" THEN tmpsigns.code-value ELSE tmpsigns.xattr-value.
      vResult  =  IF     AVAIL tmpsigns
                           /* Для темпорированных ДР */
                    AND  ({assigned vValue}
                      OR  (vValue EQ ?
                     AND  iRetUnknown))
                     THEN vValue
                     ELSE in-nofound.
   END.
   ELSE vResult = in-nofound.
   RETURN vResult.
END FUNCTION.

/* возвращет значение ТЕМПОРИРОВАННОГО допреквизита на ПРОИЗВОЛЬНУЮ дату
** с учетом значения по умолчанию */
FUNCTION GetTempXAttrValueEx RETURN CHAR (
   INPUT iFileName AS CHAR,
   INPUT iSurr     AS CHAR,
   INPUT iCode     AS CHAR,
   INPUT iDate     AS DATE,
   INPUT iNofound  AS CHAR
):
   RETURN GetTempXAttrValueEx2 (iFileName,iSurr,iCode,iDate,iNofound,NO).
END FUNCTION.

/* возвращет значение ТЕМПОРИРОВАННОГО допреквизита на ГЛОБАЛЬНУЮ дату.
** Если не найден, то возвращает пустое значение */
FUNCTION GetTempXAttrValue RETURN CHAR (
   INPUT iFileName AS CHAR,
   INPUT iSurr     AS CHAR,
   INPUT iCode     AS CHAR
):
   RETURN GetTempXAttrValueEx2 (iFileName,iSurr,iCode,gend-date,"",NO).
END FUNCTION.

/* возвращет значение допреквизита с учетом значения по умолчанию. Если значение
** темпорированного ДР = ?, то возвращает именно неопределенное значение,
** а не значение по умолчанию */
FUNCTION GetXAttrValueEx2 RETURN CHAR (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR,
   INPUT in-nofound  AS CHAR
):
   {getxattrval.i &retunknown=YES}
END FUNCTION.

/* возвращет значение допреквизита с учетом значения по умолчанию */
FUNCTION GetXAttrValueEx RETURN CHAR (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR,
   INPUT in-nofound  AS CHAR
):
   {getxattrval.i}
END FUNCTION.

FUNCTION GetXAttrValue RETURN CHAR (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR
):
   DEF VAR in-nofound AS CHAR   NO-UNDO.  /* Значение по-умолчанию = пустое значение */

   {getxattrval.i}
END FUNCTION.

/* Возвращает список сурогатов по значению */
FUNCTION GetXAttrSurr RETURNS CHARACTER 
         (INPUT in-FileName AS CHARACTER,
          INPUT in-Code     AS CHARACTER,
          INPUT in-Value    AS CHARACTER):

   DEFINE VARIABLE mRet AS CHARACTER INITIAL "" NO-UNDO.
   
   IF NOT IsXAttrIndexed(in-FileName, in-Code) THEN
      FOR EACH signs WHERE signs.file-name   EQ in-FileName
                       AND signs.code        EQ in-Code
                       AND signs.xattr-value EQ in-Value
         NO-LOCK:
         {additem.i mRet signs.surrogate}
      END.
   ELSE
      FOR EACH signs WHERE signs.file-name  EQ in-FileName
                       AND signs.code       EQ in-Code
                       AND signs.code-value EQ in-Value
         NO-LOCK:
         {additem.i mRet signs.surrogate}
      END.
   RETURN mRet.
END.


/* Проверка кода KNF для Инструкции 93-И */
{knf.pro}

/* поиск кода подразделения по пользователю */
function GetUserBranchId returns char (input in-user as char):

   def var out-branch as char no-undo.

   out-branch = GetXAttrValue("_user", in-user, "Отделение").
   IF out-branch = "" then
      out-branch = substr(in-user,2,4).
   if not can-find(first branch where branch.branch-id eq out-branch) then
      out-branch = dept.branch.
   return out-branch.
End.
                        /* Необходима для процедуры SetValue,
                        ** т.к. при динамическом создании TT и
                        ** возникновении ошибки, получаем 49 ошибки при работе
                        ** со статическими расшаренными таблицами. */
DEF TEMP-TABLE ttSetValue NO-UNDO
   FIELD fInteger    AS INT64
   FIELD fDecimal    AS DEC
   FIELD fCharacter  AS CHAR
   FIELD fLogical    AS LOG
   FIELD fDate       AS DATE
   FIELD fDateTime   AS DATETIME
   FIELD fDateTime-TZ AS DATETIME-TZ
.
                        /* Создаем буффер для статической ttSetValue. */
CREATE ttSetValue.
/*------------------------------------------------------------------------------
  Purpose:     Приводит значение в соответствии с форматом и типом 
  Parameters:  pValue   - значение
               iType    - тип значения
               iFormat  - формат
               pMsg     - сообщение
               oErr     - флаг ошибки
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE SetValue:
   DEFINE INPUT-OUTPUT PARAMETER pValue   AS CHARACTER            NO-UNDO.
   DEFINE INPUT  PARAMETER       iType    AS CHARACTER            NO-UNDO.
   DEFINE INPUT  PARAMETER       iFormat  AS CHARACTER            NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pMsg     AS CHARACTER            NO-UNDO.
   DEFINE OUTPUT PARAMETER       oErr     AS LOGICAL    INIT YES  NO-UNDO.

   DEFINE VARIABLE vHTable    AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vHBuffer   AS HANDLE     NO-UNDO.

   /* Commented by KSV: Проверка не осуществляется для типа class и значения
   ** "?" */
   IF iType = "class" OR pValue = "?" THEN 
   DO:
      oErr = NO.
      RETURN.
   END.

   /* Приводим значение логического выражения в соответствии
   ** с форматом, в принципе некорректное действие, исключительно ради 
   ** совместимости со старым кодом.
   ** Проверку формата осуществляем как для положительного значения,
   ** так и для отрицательного. */
   IF iType EQ "LOGICAL"
      THEN pValue =  IF ENTRY (1, iFormat, "~/") EQ pValue
                        THEN  "YES"
                        ELSE  IF ENTRY (2, iFormat, "~/") EQ pValue
                                 THEN "NO"
                                 ELSE pValue.
   
   /* Commented by KSV: Для пустого значения и цифрового типа обрабатываем
   ** значение, исключительно ради совместимости со старым кодом */
   IF CAN-DO("integer,decimal",iType) AND pValue = "" THEN pValue = "0".
                        /* Блок проверки значения согласно описанию. */
   CHK:
   DO:
                        /* Получение указателя на буффер. */
      vHBuffer = BUFFER ttSetValue:HANDLE.
                        /* Установление необходимых значений. */
      vHBuffer:BUFFER-FIELD ("f" + iType):FORMAT = iFormat NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN LEAVE CHK.
                        /* Сохранение значения. */
      vHBuffer:BUFFER-FIELD ("f" + iType):BUFFER-VALUE = pValue NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN LEAVE CHK.
                        /* Проверка значения на соответствие типу данных.  */
      IF vHBuffer:BUFFER-FIELD ("f" + iType):BUFFER-VALUE EQ ?
      THEN DO:
         pValue = ?.
         LEAVE CHK.
      END.
                        /* Проверка значения на соответствие формата данных. */
      pValue = STRING(vHBuffer:BUFFER-FIELD ("f" + iType):BUFFER-VALUE,iFormat) NO-ERROR.
      IF ERROR-STATUS:ERROR
         OR ERROR-STATUS:NUM-MESSAGES GT 0
         THEN LEAVE CHK.
                        /* Установление значений. */
      ASSIGN
         oErr     = NO
      .
   END.
   IF oErr = YES OR pValue = ? THEN 
   DO:
       pMsg = "Значение " + pMsg + " не соответствует типу ~"" +
              ( IF iType = ? THEN "?" ELSE iType)   + "~"" +
              " или формату ~"" +
              ( IF iFormat = ? THEN "?" else iFormat) + "~" !".

      IF pValue = ? then pValue = "".
      oErr = YES.
  END.

  pValue = RIGHT-TRIM (pValue).
  
  RETURN.

END PROCEDURE.

/******************************************************************************/
/* Контроль закрытости дня по катгориям учета
   возвращает yes (закрыт), если опредень закрыт хотя бы по одной категории
   из переданного списка */
FUNCTION FChkClsDateByFil RETURNS LOGICAL
   (INPUT iDate AS DATE,
    INPUT iCats AS CHARACTER,
    INPUT iFil  AS CHARACTER
   ):
   
   DEFINE VARIABLE vI AS INT64    NO-UNDO.
   DEFINE BUFFER acct-pos FOR acct-pos.

   IF iCats = "*" THEN
   DO:
      {profind.i &FWAY=LAST &FBUF=acct-pos &FLOCK=NO-LOCK &FNOER=YES
            &FCOND="WHERE acct-pos.filial-id =  iFil 
                      AND acct-pos.since     >= iDate USE-INDEX apos-date"}
      RETURN AVAILABLE acct-pos.
   END.
   ELSE
   DO vI = 1 TO NUM-ENTRIES(iCats):
      IF Chk_Date_CatByFil(iDate,ENTRY(vI,iCats),iFil) THEN
         RETURN YES.
   END.
   RETURN NO.

END.

/******************************************************************************/
/* Контроль закрытости дня по катгориям учета
   возвращает yes (закрыт), если опредень закрыт хотя бы по одной категории
   из переданного списка */
FUNCTION FChkClsDate RETURNS LOGICAL
   (INPUT iDate AS DATE,
    INPUT iCats AS CHARACTER):
   
   RETURN FChkClsDateByFil(iDate,iCats,shFilial).

END.

/******************************************************************************/
/* Возвращает дату последнего закрытого дня, на переданную дату 
   по категориям учета и филиалу*/
FUNCTION FGetLastClsDateByFil RETURNS DATE (
   INPUT iDate AS DATE,
   INPUT iCats AS CHARACTER,
   INPUT iFil  AS CHARACTER
):
   DEFINE VARIABLE vI         AS INT64  NO-UNDO.
   DEFINE VARIABLE vSinceDate AS DATE     NO-UNDO.
   DEFINE VARIABLE vDateTmp   AS DATE     NO-UNDO.
   DEFINE BUFFER acct-pos FOR acct-pos.
   
   IF iCats = "*" THEN
   DO:
      IF iDate = ? THEN
         {profind.i &FWAY=LAST &FBUF=acct-pos &FLOCK=NO-LOCK &FNOER=YES
            &FCOND="WHERE acct-pos.filial-id = iFil USE-INDEX apos-date"}
      ELSE
         {profind.i &FWAY=LAST &FBUF=acct-pos &FLOCK=NO-LOCK &FNOER=YES
            &FCOND="WHERE acct-pos.filial-id =  iFil
                      AND acct-pos.since     <= iDate USE-INDEX apos-date"}
      IF AVAILABLE acct-pos THEN
         vSinceDate = acct-pos.since.
   END.
   ELSE
   DO vI = 1 TO NUM-ENTRIES(iCats):   
      vDateTmp = Get_Date_Cat(ENTRY(vI,iCats),iDate).
      IF vDateTmp <> ? THEN
         vSinceDate =   IF vSinceDate EQ ?
                           THEN vDateTmp
                           ELSE MIN (vSinceDate, vDateTmp).
   END.
   IF       iDate       NE ?
      AND   vSinceDate  EQ ?
   THEN DO:
      RUN Fill-SysMes IN h_tmess (
         "", "core49", "",
         "%s=" + STRING (iDate,"99/99/9999")
      ).
      {profind.i &FWAY=FIRST &FBUF=acct-pos &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE acct-pos.filial-id = iFil
                  AND CAN-DO (iCats,acct-pos.acct-cat) USE-INDEX apos-date"}
      IF AVAILABLE acct-pos
         THEN vSinceDate = acct-pos.since.
      RUN Fill-SysMes IN h_tmess (
         "", "core50", "",
         "%s=" + ( IF vSinceDate EQ ?
            THEN "не существует"
            ELSE STRING (vSinceDate,"99/99/9999"))
      ).
   END.
   RETURN vSinceDate.

END.

/******************************************************************************/
/* Возвращает дату последнего закрытого дня, на переданную дату 
   по категориям учета */
FUNCTION FGetLastClsDate RETURNS DATE 
   (INPUT iDate AS DATE,
    INPUT iCats AS CHARACTER):

   RETURN FGetLastClsDateByFil(iDate,iCats,shFilial).

END.




/******************************************************************************/
FUNCTION FGetCats RETURN CHARACTER 
   (INPUT iCatSourse AS CHARACTER,
    INPUT iRecid     AS RECID):

DEFINE VARIABLE vCats      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vOpClasses AS CHARACTER  NO-UNDO.

   DEF BUFFER op-template FOR op-template.
   IF iCatSourse = "op-kind" THEN
   DO:
      FIND FIRST op-kind WHERE RECID(op-kind) = iRecid
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE op-kind THEN
         RETURN ?.

      vOpClasses = Ls-Class("op").
      {profind.i &FWAY=FIRST &FBUF=op-template &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="OF op-kind"} 
      IF AVAILABLE op-template THEN
         /* старые транзакции... */
         FOR EACH op-template OF op-kind WHERE
                  ( IF op-template.cr-class-code EQ "" 
                      THEN TRUE
                      ELSE CAN-DO (vOpClasses, op-template.cr-class-code))
            NO-LOCK:
            vCats = JoinStrings(vCats,op-templ.acct-cat).
         END.
      ELSE
         /* новые транзакции... */
         FOR EACH op-kind-tmpl OF op-kind WHERE
                  CAN-DO(vOpClasses,op-kind-tmpl.work-class-code)
            NO-LOCK:
            vCats = JoinStrings(vCats,GetXattrInit(op-kind-tmpl.work-class-code,
                                                   "acct-cat")).
         END.
         IF NOT {assigned vCats} THEN 
            vCats = GetXAttrValue("op-kind",op-kind.op-kind,"AcctCatList").
   END.
   ELSE IF iCatSourse = "op" THEN
   DO:
      FIND FIRST op WHERE RECID(op) = iRecid
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE op THEN
         RETURN ?.
      FOR EACH op-entry OF op 
         NO-LOCK:
         IF LOOKUP(op-entry.acct-cat,vCats) = 0 THEN
            {additem.i vCats op-entry.acct-cat}
      END.
   END.
   RETURN vCats.
END.

/*------------------------------------------------------------------------------
  Purpose:     Определяет кто удерживает запись по ее rec-id
  Parameters:  iRecId   - RECID записи
               pString  - сообщение, в которое надо добавить информацию о
                          пользователе, удерживающем запись
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(ORACLE) &THEN
   DEFINE TEMP-TABLE locking_data
      FIELD xidusn            AS INTEGER
      FIELD XIDSLOT           AS INTEGER            
      FIELD XIDSQN            AS INTEGER            
      FIELD OBJECT_ID         AS INTEGER            
      FIELD SESSION_ID        AS INTEGER            
      FIELD ORACLE_USERNAME   AS CHARACTER          
      FIELD OS_USER_NAME      AS CHARACTER          
      FIELD fPROCESS           AS CHARACTER          
      FIELD LOCKED_MODE       AS INTEGER            
   .
&ENDIF

/* !!! Процедуру WhoLocks использовать запрещенно!  
   вместо нее следует использовать WhoLocks2, в которой дополнительно определен параметр iTableName 
*/
FUNCTION WhoLocks RETURN LOGICAL (INPUT         iRecId      AS RECID ,
                                  INPUT-OUTPUT  pString     AS CHARACTER).

   RETURN WhoLocks2( iRecId,"", pString).
END FUNCTION.


FUNCTION WhoLocks2 RETURN LOGICAL (INPUT         iRecId      AS RECID ,
                                    INPUT         iTableName  AS CHARACTER ,
                                    INPUT-OUTPUT  pString     AS CHARACTER).

   DEFINE BUFFER xconnect FOR bisquit._connect.
   DEFINE BUFFER xuser    FOR bisquit._user.
   DEFINE BUFFER xlock    FOR bisquit._lock.

   DEFINE VARIABLE vUsers     AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE vNumUsers  AS INTEGER INIT 0 NO-UNDO.
   DEFINE VARIABLE vNumTable  AS INTEGER NO-UNDO.

   /* Commented by Mike: На всякий случай удаляем RELEASE bisquit._connect. */
   
   IF pString EQ "" THEN
     pString = 'Запись изменяет другой пользователь,~nпопробуйте позже!'.
  
   /* Commented by KSV: Если информация о сессии залочившей запись, все еще
   ** не найдена, то ищем ее стандартным образом, через таблицу _lock.  */
   &IF DEFINED(ORACLE) &THEN
       /* мы получаем recid прикладной таблицы, а локируется спец таблица mutex
          корректируем..   */
       FIND FIRST mutex WHERE 
          mutex.filename EQ iTableName AND  
          mutex.rec-id   EQ iRecId NO-LOCK NO-ERROR.
       IF AVAILABLE mutex THEN
          ASSIGN
             iRecId = RECID(mutex) 
             iTableName = "mutex"
          .
   &ENDIF
   
      IF NOT AVAILABLE xconnect THEN 
      DO:
        /* Таблица _LOCK не имеет индексов и имеет стековую структуру, поэтому 
        ** последовательно перебираем все записи пока не встретим нужную, либо до 
        ** первой пустой  */
        DEFINE VARIABLE vStart AS INTEGER     NO-UNDO.
        vStart = TIME.

        find first _file where _file._File-Name eq iTableName no-lock no-error.
        vNumTable = _file._File-Number.
        
        FOR EACH xlock NO-LOCK WHILE xLock._Lock-Table <> ?: 
           IF xlock._Lock-Table EQ vNumTable AND
              xlock._lock-recid EQ INT64(iRecId) 
           THEN 
              LEAVE.     
           /* Если для поиска локировки требуется больше 15 секунд, прекращаем
           ** это бесперспективное занятие */
           IF TIME - vStart > 15 THEN LEAVE. 
        END. /* FOR EACH: */
        IF AVAILABLE xlock AND xlock._lock-recid = INT64(iRecId) THEN 
        DO:
                        /* whole-index _connect, т.к. нет индексов кроме
                        ** одного по полю connect-id */
           FIND FIRST xconnect WHERE 
              xconnect._connect-usr = xlock._lock-usr NO-LOCK NO-ERROR.
           IF AVAIL xconnect THEN 
              FIND FIRST xuser WHERE 
                 xuser._userid = CODEPAGE-CONVERT(xconnect._connect-name, DBCODEPAGE("bisquit"), SESSION:CPTERM) NO-LOCK NO-ERROR.
        END.
      END.

   
   /* в случаее оракла не всегда можно найти залокировавшего через _lock */
&IF DEFINED(ORACLE) &THEN
   DEFINE VARIABLE vConnectID AS INT64    NO-UNDO.
   IF NOT AVAILABLE xconnect THEN  
   DO:
      DEFINE VAR handle1 AS INTEGER.
      DEFINE VAR i AS INTEGER.
      DEFINE VARIABLE vLockingData AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vErrMsg AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vTableName AS CHARACTER.
      vTableName = CAPS(iTableName).
      vTableName = REPLACE(vTableName,"-","_").
      
      DEFINE VARIABLE vSqlCmd AS CHARACTER   NO-UNDO.
      vSqlCmd = 
        " select lo.process "+
        " from   v$locked_object lo, dba_objects o "+
        " where  o.object_id = lo.object_id                        "+
        "        and lo.xidusn != 0                                "+
        "        and o.owner = user                                "+
        "        and o.object_name = '" + (vTableName) + "'".
      
      RUN STORED-PROC send-sql-statement handle1 = PROC-HANDLE (vSqlCmd). 
   
      FOR EACH proc-text-buffer WHERE PROC-HANDLE = handle1:
                        /* whole-index _connect, т.к. нет индексов кроме
                        ** одного по полю connect-id */
           FIND FIRST xconnect WHERE xconnect._connect-Pid = int64(proc-text) NO-LOCK NO-ERROR.
           IF AVAIL xconnect THEN 
           DO:
              vNumUsers = vNumUsers + 1. 
              FIND FIRST xuser WHERE 
                 xuser._userid = CODEPAGE-CONVERT(xconnect._connect-name, DBCODEPAGE("bisquit"), SESSION:CPTERM) 
              NO-LOCK NO-ERROR.
              vUsers = vUsers +
                       " Пользователь: "   + STRING(xuser._User-name, "x(35)") +
                       " Код: " + STRING(xuser._userid, "x(15)") + "~n" +
                       "   Устройство: " + STRING(xconnect._connect-device,"x(35)") +
                       " PID: " + STRING(STRING(xconnect._connect-Pid),"x(15)") + "~n" +
                       "      Телефон: " + STRING(getUserXattrValue(xuser._userid, "Телефон"),"x(25)") + "~n ".
                
           END.   
      END.   
      CLOSE STORED-PROC send-sql-statement WHERE PROC-HANDLE = handle1.
   END.
&ENDIF

   /* Commented by KSV: Информация о сессии и пользователе найдена, добавляем
   ** ее к строке сообщения */
   IF AVAILABLE xconnect AND 
      AVAILABLE xuser AND   
      vNumUsers < 2
      THEN
   DO:     

      vUsers =    "     Пользователь: "   + STRING(xuser._User-name, "x(35)") +
                  "~n              Код: " + STRING(xuser._userid, "x(35)") +
                  "~n       Устройство: " + STRING(xconnect._connect-device,"x(35)") +
                  "~n              PID: " + STRING(STRING(xconnect._connect-Pid),"x(35)") +
                  "~n          Телефон: " + STRING(getUserXattrValue(xuser._userid, "Телефон"),"x(35)").
          
   END.
      
   pString = pString + "~n~n" + vUsers.

   RETURN YES.
END FUNCTION.

/*Форматирование номера документа до нужного кол-ва символов */
FUNCTION FormatDocNum RETURN CHARACTER (INPUT iDocNum AS CHARACTER,
                                        INPUT iLength AS INT64):
   RETURN
   STRING(INT64(
      SUBSTRING(
                STRING(iDocNum),
                ( IF (LENGTH(STRING(iDocNum)) - iLength) LE 0 
                   THEN 1 
                   ELSE LENGTH(STRING(iDocNum)) - (iLength - 1) 
                ),
                iLength
               )
   )).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Сбрысывает значение атрибута RETURN-VALUE в пустую строку
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE ResetReturnValue:
   RETURN .
END PROCEDURE.

/* Отображает название продукта в правом верхнем углу экрана. */
PROCEDURE RemHeaderMark:
   IF NOT AVAILABLE dept THEN 
      FIND FIRST dept WHERE NO-ERROR.
   PUT SCREEN COLOR BLACK/GRAY COL 77 ROW 2 dept.logo [2].
END.

/*Добавляет код филиала во внутренний номер счета*/
FUNCTION AddFilToAcct RETURNS CHARACTER (iAcct      AS CHARACTER,
                                         iFilial-Id AS CHARACTER):
   IF NOT GetDBMode()      OR
      INDEX(iAcct,"@") > 0 OR
      NOT {assigned iAcct} THEN RETURN iAcct.
   ASSIGN
      SUBSTRING(iAcct,26,1) = "@"
      ENTRY(2,iAcct,"@")    = iFilial-Id
   .  
   RETURN iAcct.
END FUNCTION.

/*Убирает код филиала из внутреннего номера счета*/
FUNCTION DelFilFromAcct RETURNS CHARACTER (iAcct AS CHARACTER):
   RETURN TRIM (ENTRY (1, iAcct, "@")).
END FUNCTION.

/* Трансформирует маску, добавляя туда суффикс филиала
** для полей acct.acct, loan.cont-code
** Используется для фильтров и парсерных функций. */
FUNCTION FmtMskAddSuffix RETURNS CHARACTER (
   iMask       AS CHARACTER,
   iAttrCode   AS CHARACTER
):
   DEF VAR vValSrc      AS CHAR   NO-UNDO. /* Исходное значение поля. */
   DEF VAR vVarI        AS CHAR   NO-UNDO. /* I-ый элемент значения. */
   DEF VAR vValRes      AS CHAR   NO-UNDO. /* Скорректированное значение. */
   DEF VAR vCnt         AS INT64    NO-UNDO. /* Счетчик. */
   DEF VAR vFormat      AS INT64    NO-UNDO. /* Ширина поля. */
   DEF VAR vDelta       AS INT64    NO-UNDO. /* Кол-во пробелов до разделителя. */
   DEF VAR vLen         AS INT64    NO-UNDO. /*Длина строки*/
   DEF VAR vAddEndStar  AS LOG    NO-UNDO. /*vAddEndStar - указывает на Необходимость добавления "*"
                                           ** в конец маски для аттрибута cont-code. */
   IF NOT shMode
      THEN RETURN iMask.
   vValSrc = iMask.
   
   CASE iAttrCode:
   WHEN  "cont-code" THEN
   DO:
      /*mitr: Для номера договора кредитов отдельные правила.
      Если последний символ в маске есть звездочка "*", то добавляем в конец 
      звездочку.
      
      Структура cont-code следующая :
      1) Номер охватывающего договора
      2) @
      3) Номер филиала
      
         Дополнительно для траншей добавляем 
      4) пробел
      5) номер течения
      
      Пример: если мы полчаем строку типа 222333222*, то 
      нам необходимо вернуть такую маску 222333222*@002*
      
      */
      vValRes = "".
      
      IF vValSrc EQ "*" THEN
         /*Если номер договора не указан, то не надо добавлять код филиала.*/                  
         vValRes = vValSrc.
      ELSE
      DO vCnt = 1 TO NUM-ENTRIES (vValSrc):
         vVarI    =  ENTRY (vCnt, vValSrc).
      
         vLen = LENGTH(vVarI).
         IF vLen > 0 THEN
            vAddEndStar = (SUBSTR(vVarI, vLen, 1) EQ "*").
   
         /*Если последный символ в маске = "*", то после номера филиала также
         добавляем "*"*/
         IF INDEX(vVarI,"@") EQ 0 
            AND  vVarI <> "!" THEN
            vVarI = addFilToLoan(vVarI, shFilial) + IF vAddEndStar THEN "*" ELSE "".
         {additem.i vValRes vVarI}
      END.
   END.
   OTHERWISE
   DO vCnt = 1 TO NUM-ENTRIES (vValSrc):
      ASSIGN
         vVarI    =  ENTRY (vCnt, vValSrc)
         vFormat  =  25 + IF INDEX (vVarI, "!") EQ 0 THEN 0 ELSE 1
         vDelta   =  5
      .
      IF       INDEX (vVarI, "@")   EQ 0
         AND   vVarI                NE "*"
         AND   vVarI                NE "!" 
      THEN DO:
         vVarI =  IF INDEX (vVarI, "*")   EQ 0
                     THEN  (vVarI + FILL (" ", vFormat - LENGTH (vVarI)) + "@" + IF shModeMulty 
                                                                                 THEN "*" 
                                                                                 ELSE shFilial)
                     ELSE  IF SUBSTR (vVarI, LENGTH (vVarI)) EQ "*"
                        THEN  (vVarI /* +                                  "@" + shFilial*/)
                        ELSE  (vVarI + FILL (" ", vDelta) +                "@" + IF shModeMulty 
                                                                                 THEN "*" 
                                                                                 ELSE shFilial).
      END.
      {additem.i vValRes vVarI}

   END.
   END CASE.

   IF vValRes NE ""
      THEN RETURN vValRes.
      ELSE RETURN "".
END FUNCTION.

/*----------------------------------------------------------------------------*/
FUNCTION GetBankINN RETURN CHAR (INPUT iBankCodeType  AS CHAR,
                                 INPUT iBankCode      AS CHAR):

   DEF BUFFER banks        FOR banks.      
   DEF BUFFER banks-code   FOR banks-code. 

   DEFINE VARIABLE vBank-id AS INT64     NO-UNDO.
   DEFINE VARIABLE vBegDate AS DATE        NO-UNDO.
   DEFINE VARIABLE vEnDate  AS DATE        NO-UNDO.
   DEFINE VARIABLE vInn     AS CHARACTER   NO-UNDO.

   IF iBankCodeType EQ "bank-id" THEN   
      vBank-id = INT64 (iBankCode).
   ELSE
   DO:
      RUN GetBank IN h_base (BUFFER banks,
                             BUFFER banks-code,
                             INPUT  iBankCode,
                             INPUT  iBankCodeType,
                             INPUT  NO).
      IF AVAIL banks THEN
         vBank-id = banks.bank-id.
   END.

   ASSIGN 
      vBegDate = ?
      vEnDate  = ?
   .
   
   {getcustident.i
      &BegDate  = vBegDate
      &EndDate  = vEnDate
      &CustCat  = 'Б'  
      &CustId   = vBank-id
      &CustType = 'ИНН'
      &RetValue = vInn
   }

   RETURN vInn.

END FUNCTION.

/* Получает перечень теморальных ДР. */
PROCEDURE SetXattrTmp.
   DEF INPUT PARAM TABLE FOR ttXattrTemp.
   RETURN.
END PROCEDURE.

PROCEDURE DestroyInterface.
   {intrface.del}          /* Выгрузка инструментария. */ 
   RETURN.
END PROCEDURE.

FUNCTION GetFmtQty RETURNS CHARACTER (INPUT iObject      AS CHARACTER,
                                      INPUT iAcctCat     AS CHARACTER,
                                      INPUT iNumChar     AS INT64,
                                      INPUT iDefAccuracy AS INT64):

   DEFINE VARIABLE mSett   AS CHARACTER   NO-UNDO. /* значение настроечного параметра Тчнсть_БалСч/Тчнсть_ЛСч */
   DEFINE VARIABLE mLngBs  AS INT64     NO-UNDO. /* длина основания */
   DEFINE VARIABLE mLngFr  AS INT64     NO-UNDO. /* длина дробной части */
   DEFINE VARIABLE mResult AS CHARACTER   NO-UNDO.

   IF iAcctCat EQ "bal-acct" THEN
      mSett = FGetSetting("Тчнсть_БалСч", "", "").
   ELSE IF iAcctCat EQ "acct" THEN
      mSett = FGetSetting("Тчнсть_ЛСч", "", "").

   mLngFr = IF {assigned mSett} THEN INT64(mSett)
                                ELSE iDefAccuracy.
   mLngBs = iNumChar - mLngFr - 3.

   mResult = "-" + FILL(">", mLngBs) + "9." + FILL("9", mLngFr).

   RETURN mResult.

END FUNCTION.

/* Глобальный семафор, показывающий должен ли следующий браузер закрываться 
** на GO, по умолчанию NO */
DEFINE VARIABLE mCloseOnGo AS LOGICAL NO-UNDO.
/* Хэндл процедуры, первой поменявшей mCloseOnGo в значение YES */
DEFINE VARIABLE mCogProcHandle AS HANDLE NO-UNDO.
/* Идентификатор процедуры, первой поменявшей mCloseOnGo в значение YES */
DEFINE VARIABLE mCogProcID     AS INT64 NO-UNDO.

/*------------------------------------------------------------------------------
  Purpose:     Проверяет является ли хэндл валидным с доп.контролем по значению
               UNIQUE-ID.
  Parameters:  iObjectHandle - хэндл
               iObjectID     - значение UNIQUE-ID, если не задано, то работает
                               как обычный VALID-HANDLE
  Notes:       Значения хэндлов переиспользуются циклически, поэтому 
               VALID-HANDLE может вернуть TRUE, хотя объект, на который он 
               ссылался изначально, уже не существует, а значение его 
               хэндла было присвоено новому объекту
------------------------------------------------------------------------------*/
FUNCTION ValidObjectHandle RETURNS LOGICAL ( iObjectHandle AS HANDLE,
                                             iObjectID     AS INT64 ):
   IF VALID-HANDLE( iObjectHandle ) <> YES THEN RETURN NO.
   /* Если iObjectID не задан, то валидности хэндла будет достаточно */
   IF iObjectID = ? THEN RETURN YES.
   
   /* Если iObjectID указан и совпадает, то хэндл валиден */
   IF CAN-QUERY( iObjectHandle, "UNIQUE-ID" ) AND 
      iObjectHandle:UNIQUE-ID = iObjectID THEN RETURN YES.
   
   /* Во всех остальных случаях, он не валиден */   
   RETURN NO.                                                 
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает значение семафора CloseOnGo
  Parameters:  oCloseOnGo - значение семафора
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE GetCloseOnGoSemaphore:
   DEFINE OUTPUT PARAMETER oCloseOnGo     AS LOGICAL NO-UNDO.
   
   /* Если процедура, его изначально изменившая, уже выгружена, значение
   ** сбрасывается в NO */
   IF NOT ValidObjectHandle( mCogProcHandle, mCogProcID ) THEN 
      mCloseOnGo = NO.
   
   oCloseOnGo = mCloseOnGo.   
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Устанавливает значение семафора CloseOnGo
  Parameters:  iCallerHandle - хэндл процедуры, выполняющий изменение
               iCloseOnGo    - новое значение семафора
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE SetCloseOnGoSemaphore:
   DEFINE INPUT  PARAMETER iCallerHandle AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER iCloseOnGo    AS LOGICAL NO-UNDO.
   
   /* Запоминаем хэндл процедуры, выполнившей изменение семафора со значения по
   ** умолчанию  */
   IF iCloseOnGo AND NOT mCloseOnGo AND 
      NOT ValidObjectHandle( mCogProcHandle, mCogProcID ) THEN
   DO:
      ASSIGN 
         mCogProcHandle = iCallerHandle
         mCogProcID     = iCallerHandle:UNIQUE-ID.            
   END.   
   
   mCloseOnGo = iCloseOnGo.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Определение значения реквизита объекта на заданный момент времени по       */
/* журналу изменений.                                                         */
/* Параметры:                                                                 */
/*   iHandle   - указатель на буфер объекта                                   */
/*   iAttrName - наименование основного или расширенного реквизита объекта,   */
/*               значение которого требуется определить.                      */
/*   iDate     - дата, на которую требуется определить значение реквизита.    */
/*   iTime     - время для даты iDate, на которое требуется определить        */
/*               значение реквизита. Указывается в секундах, прошедших с      */
/*               начала суток, соответствующих дате iDate. Неопределённое     */
/*               значение воспринимается как 0.                               */
/*   oResult   - возвращаемый результат, преобразованный к строковому типу.   */
/* Устанавливает ERROR-STATUS:ERROR = YES в следующих случаях:                */
/*   - в iHandle передан пустой указатель;                                    */
/*   - указанный момент времени ещё не наступил;                              */
/*   - объект не существовал в указанный момент времени.                      */
/*----------------------------------------------------------------------------*/
PROCEDURE GetAttrValueOnDate.
    DEFINE INPUT  PARAMETER iObject   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iDate     AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER iTime     AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oResult   AS CHARACTER NO-UNDO.

    IF VALID-HANDLE(iObject) THEN DO:
        RUN GetAttrValueOnDateEx(iObject:TABLE,
                                 Surrogate(iObject),
                                 iAttrName,
                                 iDate,
                                 iTime,
                                 OUTPUT oResult)
        NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
            RETURN.
    END.
    RETURN ERROR.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Альтернативный вариант вызова GetAttrValueOnDate. Объект передаётся не     */
/* в виде указателя на буфер, а в виде пары из имени таблицы и суррогата      */
/*----------------------------------------------------------------------------*/
PROCEDURE GetAttrValueOnDateEx.
    DEFINE INPUT  PARAMETER iTable     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iSurrogate AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iDate      AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER iTime      AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oResult    AS CHARACTER NO-UNDO.

    DEFINE BUFFER history FOR history.

    DEFINE VARIABLE vModStr LIKE history.field-value            NO-UNDO.
    DEFINE VARIABLE vI      AS   INT64                          NO-UNDO.
    DEFINE VARIABLE vS      AS   CHARACTER                      NO-UNDO.
    DEFINE VARIABLE vFound  AS   LOGICAL             INITIAL NO NO-UNDO.

    IF iTime = ? THEN
        iTime = 0.
    IF iDate > TODAY OR (iDate = TODAY AND iTime > TIME) THEN
        RETURN ERROR.
    FOR EACH history WHERE
        history.file-name = iTable     AND
        history.field-ref = iSurrogate AND
        (history.modif-date < iDate OR
         history.modif-date = iDate AND
         history.modif-time < iTime)
    NO-LOCK
    BY history.history-id DESCENDING:
        LEAVE.
    END.
    IF AVAILABLE history AND history.modify = {&HIST_DELETE} THEN
        RETURN ERROR.
    FOR EACH history WHERE
        history.file-name   = iTable     AND
        history.field-ref   = iSurrogate AND
        history.field-name  =  ""        AND
        &IF DEFINED(ORACLE) = 0 &THEN
        history.field-value <> ""        AND
        &ENDIF
        (history.modif-date > iDate OR
         history.modif-date = iDate AND
         history.modif-time > iTime)
    NO-LOCK
    BY history.history-id:
        vModStr = history.field-value.
        DO vI = 1 TO NUM-ENTRIES(vModStr) BY 2:
            vS = ENTRY(vI, vModStr).
            IF vS = "" THEN
                LEAVE.
            IF vS = iAttrName OR vS = "*" + iAttrName THEN
                ASSIGN
                    oResult = ENTRY(vI + 1, vModStr)
                    vFound  = YES
                .
        END.
        IF vFound THEN DO:
            IF oResult = "{&UNKVAL}" THEN
                oResult = ?.
            RETURN.
        END.
    END.
    oResult = DYNAMIC-FUNCTION(IF IsValidField(iTable, iAttrName) THEN
                                   "GetValueAttr"
                               ELSE
                                   "GetXAttrValue",
                               iTable,
                               iSurrogate,
                               iAttrName).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает список подчиненных пользователя, на объекты которых
               пользователь имеет право доступа на чтение.
  Parameters:  iUserId - код пользователя, список подчиненных которого необходим
  Notes:
------------------------------------------------------------------------------*/
FUNCTION getUserSlaves RETURNS CHAR
          (INPUT iUserId AS CHAR): /* Код пользователя */

  RETURN GetSlavesListMethod (iUserId, "r").
END FUNCTION. /* getUserSlaves */

/*------------------------------------------------------------------------------
  Purpose:     Возвращает список подчиненных текущего пользователя, на объекты
               которых пользователь имеет право доступа на чтение.
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
FUNCTION getSlaves RETURNS CHAR:
  RETURN getUserSlaves (USERID("bisquit")).
END FUNCTION. /* getSlaves */
/* $LINTFILE='base-pp.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='ariz' */
/* $LINTDATE='20/04/2016 16:02:08.999+04:00' */
/*prosignCmTZKb/RR/QZfSPOfJ7xJw*/
/* --- base-pp.p was humbly modified by (c)blodd converter v.1.09 on 8/9/2016 9:09am --- */
