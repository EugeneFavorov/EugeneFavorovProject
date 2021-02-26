{globals.i}
{intrface.get tmess}

/* +++ pp-swi.p was humbly modified by (c)blodd converter v.1.11 on 2/28/2017 7:55am +++ */

/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: PP-SWI.P 
      Comment: стандартные SWIFT функции
               sw-trans (транслируемая строка, YES - на латынь

                                               NO - на кирилицу )
   Parameters: нет
         Uses:
      Used by:
      Created: 25.01.2000 mkv
     Modified: 28.02.00 mkv английский адрес клиента, 
                            ошибка клиринговой системы - всегда подставл "MF"
     Modified: 29.02.00 mkv английский адрес и наименование нашего банка
     Modified: 29.04.2003 mkv 0012948
     Modified: 20.01.2004 mkv 0023614  добавлена фукция GetRecOpByRef

FUNCTION'S

 GetOpByRef      - возвращает код документа по референсу и дате

 GetBankCodeType - уникален ли банковский реквизит
 GetBankCodeFormat - код банка в формате экспорта
 GetStWarning    - возвращает статус предупреждения
 xover           - переворачивает строку сзаду наперед
 convxdate       - возвращает дату по указанному формату, если не соответсвует
                   то ?
 convxinteger    - возвращает целое число по указанному формату, если не
                   соответсвует то ?
 convxdecimal    - возвращает дробное число по указанному формату, если не
                   соответсвует то ?
 comp-curr       - сравнивает валюту счета с валютой проводки, если одиноковы
                   YES
 word-wrap       - делит строку на сумму подстрок с произвольным кол-вом
                   символов в каждой
                   например word-wrap("1234567","3,4") вернет "1234567"
 we-have-bic     - имеем ли мы бик
 acctb-have-bic  - с биком ли филиал
 sw-trans        - транслитерация и детранслитерация
 sw-code         - значение классификатора по коду и классу
 sw-code2        - описание классификатора по коду и классу
 sw-val          - код классификатора по значение и классу
 sw-val-beg      - код клиринговой системы по коду идентификатора банка
                   (неуникальные тоже обрабатываются)
 sw-desc         - код классификатора по описанию и классу
 sw-descm        - misc классификатора по описанию и классу
 sw-buf          - убивает 1 и 3 символы по asc и разбивает текст
                   по 79 символов с рзделителем ~n
 sw-e72          - обработка др swift-det-cod и swift-det-inf
                   для экспорт 72 тега
 sw-icn          - поиск счета c-nostro для импорта
 GetCNostroAcct  - поиск счета c-nostro для экспорта
 is-cyril        - наличие кирилицы в строке
 send-rec        - определяет тип доумента rec или send
 get-op-bank     - возвращает recid банковского реквизита
                   с заданной ролью (с учетом пустой роли)
 pos-acct-no-bic - возвращает банковский счет "по умолчанию"
                   по маске подразделения для банков без бика
 save-reflect    - при импорте для сохранения информации в формате из
                   файла по 35 символов с добавлением пробелов
 num-link        - колличество связанных документов
 CliDbEqCliCr    - Дебет и кредит принадлежат одному клиенту
 StrTimeToInt    - преобразование времени в INT64 возможны форматы hh:mm:ss, hh:mm, hhmmss

PROCEDURE's
 testxfigure     - 
 e-sw50          - формирование 50 тега
 e-sw59          - формирование 59 тега
 e-sw50-103      - формирование 50 тега вариант 2
 e-sw59-103      - формирование 59 тега вариант 2
 get-engl-name   - возвращает ангийское наименование и адресс клиента по счету
 e-sw52          - формирование 52 тега
 e-sw57          - формирование 57 тега
 e-sw58          - формирование 58 тега
 op-vdate        - проверка опердня на открытие,неоткрытие,блокирование и
                   выходной
 op-vdate950     - проверка опердня на открытие,неоткрытие,блокирование и
                   выходной для выписок
 e-sw5-tp        - формирование технологии платежа для 72 тега
 e-rel-ref       - связанный референс для 21 тега
 get-engl-bank   - английское наименование и адресс банка
 get-afromn      - выделяет счет из наименования банка выступающего как клиент
 i-sw25          - обработка счета из 25 тега
 i-sw_posit_on_tran-acct - позиционирование на транзитный счет
                   (др acct-curr-trans) и обработка др транзакции
                   ЗапрНазнСч и РазрНазнСч
 e-sw56-910      - формирование 56 тега для авизо
 frm-det-inf     - бъет строки по кол-ву символов 
 catal-protoc    - поиск и коректировка каталога протоколов возвращает путь к
                   каталогу и удаляет файл протокола
                   если каталог не создан, то протоколы создаются в рабочий каталог (0228144)
 add-protoc     - соединение сводного протокола и реестра импорта
 reestr-protoc  - формирование протокола квитовки 
 GetBanks       - универсальный поиск банка
 DecodeVerForm  - расшифровка версии формата
 DelInDet       - обработка делителя, его вхождение заменяется на перевод строки
 GetAcctById    - поиск счета
 BankNoBic      - филиал без бика
 GetFlagGo      - Определение типа документа: внутренний, начальный, ответный или транзитный
 OurBank        - наш ли банк
 UpdDocType     - изменение кода документа для проверки 
 CrDocType      - создание кода документа
 RmBufCopyDoc   - Обрезание текста сообщения
*/

{globals.i}
{chkacces.i}
{bank-id.i}
{intrface.get "xclass"} /* Подключение инструментария работы с метасхемой     */
{intrface.get "strng"}  /* Подключение инструментария работы со строкой       */
{intrface.get "op"}     /* Подключение инструментария работы с документом     */
{intrface.get "import"} /* Подключение инструментария работы с имп/экспортом  */
{intrface.get filex}

{debug.equ}

{mutex-buff.def} 

{pfuncdef
 &DefLib="swi" 
 &Description="стандартные SWIFT функции"}

&GLOBAL-DEFINE def-stream-log YES  /* временно                                */
DEFINE STREAM lock-log.

DEF VAR mKodFil   AS CHAR NO-UNDO. /* "КодФил"                                */
DEF VAR mOurAdrr  AS CHAR NO-UNDO. /* "БанкГород"                             */
DEF VAR mOurName  AS CHAR NO-UNDO. /* "БанкС"                                 */
DEF VAR mBalINN   AS CHAR NO-UNDO. /* "БалСчИНН"                              */
DEF VAR mOurINN   AS CHAR NO-UNDO. /* "ИНН"                                   */
DEF VAR mSpaceInn AS CHAR NO-UNDO. /* Наличие пробела после инн и кпп         */
                                   /* в тегах 50-59                           */
DEF TEMP-TABLE tt-det-cod NO-UNDO  /* список кодов use IN "frm-det-inf"       */
         FIELD id         AS CHAR  /* код                                     */
         FIELD val        AS CHAR. /* значение                                */

ASSIGN mKodFil   = FGetSetting("КодФил",     ?,"")
       mOurAdrr  = FGetSetting("БанкГород",  ?,"")
       mBalINN   = FGetSetting("БалСчИНН",   ?,"")
       mOurINN   = FGetSetting("ИНН",        ?,"")
       mOurName  = FGetSetting("БанкС",      ?,"").

DEF BUFFER sign FOR signs.
 
/*----------------------------------------------------------------------------*/
FUNCTION Chk2863U RETURNS LOGICAL (INPUT iDetails AS CHARACTER,
                                    INPUT iName    AS CHARACTER).
 
    DEFINE VARIABLE vRes AS LOGICAL     NO-UNDO.

    vRes = INDEX(iDetails, "2863-У") GT 0 AND
          INDEX(iName, "Бюджетный") GT 0.

    RETURN vRes.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* возвращаетъ инн из строк следующих форматов:                               */
/*                                                                            */
/*    INN xxxxxxxxxx.KPPyyyyyyyy                                              */
/*    INNxxxxxxxxxx.KPPyyyyyyyy                                               */
/*    INN xxxxxxxxxx                                                          */
/*    INNxxxxxxxxxx                                                           */
/*    ИНН xxxxxxxxxx.KPPyyyyyyyy                                              */
/*    ИННxxxxxxxxxx.KPPyyyyyyyy                                               */
/*    ИНН xxxxxxxxxx                                                          */
/*    ИННxxxxxxxxxx                                                           */
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetInnFromString returns CHARACTER (vB AS CHAR):
   DEFINE VARIABLE vD AS INT64 NO-UNDO.
   DEFINE VARIABLE vR AS CHAR    NO-UNDO.

   IF TRIM(vB) BEGINS "INN" OR  
      TRIM(vB) BEGINS "ИНН" THEN 
   DO:
      vD = INDEX(vB,".").

      IF vD > 0 THEN
         vB = SUBSTRING(vB,1,vD - 1).

      vR = TRIM(SUBSTRING(vB,4)). 
   END.
   RETURN vR.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/* преобразование времени в INT64 возможны форматы hh:mm:ss, hh:mm, hhmmss     */
/*---------------------------------------------------------------------------*/
FUNCTION StrTimeToInt RETURN INT64 (ipStrTime AS CHAR, ipDel AS CHAR):

   RETURN IF {assigned ipDel} THEN
             (INT64(GetEntries(1,ipStrTime,ipDel,"0")) * 3600 +
              INT64(GetEntries(2,ipStrTime,ipDel,"0")) * 60 +
              INT64(GetEntries(3,ipStrTime,ipDel,"0")))
             
          ELSE 
             (INT64(SUBSTR(ipStrTime,1,2)) * 3600 +
              INT64(SUBSTR(ipStrTime,3,2)) * 60 +
              INT64(SUBSTR(ipStrTime,5)))
             .

END FUNCTION.

/*---------------------------------------------------------------------------*/
/*Дебет и кредит принадлежат одному клиенту*/
/*---------------------------------------------------------------------------*/
FUNCTION CliDbEqCliCr RETURN LOG (ipRecOpEntry  AS RECID,
                                  ipMaskCustCat AS CHAR):
   DEF BUFFER op-entry FOR op-entry.
   DEF BUFFER acct-db  FOR acct.
   DEF BUFFER acct-cr  FOR acct.
   
   FIND FIRST op-entry WHERE RECID(op-entry) EQ ipRecOpEntry NO-LOCK NO-ERROR.
   FIND FIRST acct-db  WHERE acct-db.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
   FIND FIRST acct-cr  WHERE acct-cr.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
   
   RETURN AVAIL op-entry AND
          AVAIL acct-db  AND
          AVAIL acct-cr  AND
          acct-db.cust-cat EQ acct-cr.cust-cat AND
          acct-db.cust-id  EQ acct-cr.cust-id AND
          CAN-DO(ipMaskCustCat,acct-db.cust-cat).

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*поиск счета c-nostro для экспорта*/
/*---------------------------------------------------------------------------*/
FUNCTION GetCNostroAcct RETURN CHAR (ipAcct AS CHAR, ipCurr AS CHAR):

    DEF BUFFER c-nostro   FOR c-nostro.

    FIND FIRST c-nostro WHERE
       c-nostro.acct EQ ipAcct AND
       c-nostro.curr EQ ipCurr AND 
       {assigned c-nostro.corr-acct} NO-LOCK NO-ERROR.
    RETURN IF AVAIL c-nostro THEN
              c-nostro.corr-acct
           ELSE "".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*поиск документа по референсу и дате*/
/*---------------------------------------------------------------------------*/
FUNCTION GetOpByRef RETURN CHAR (ipOpRef AS CHAR, ipOpDate AS DATE):

   DEF BUFFER op-impexp  FOR op-impexp. 
   DEF BUFFER op         FOR op.

   FOR EACH op-impexp WHERE
            op-impexp.op-reference EQ ipOpRef NO-LOCK,
      FIRST op WHERE
            op.op EQ op-impexp.op AND
            op.op-date EQ ipOpDate NO-LOCK:

      RETURN STRING(op.op).
   END.
   RETURN ?.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*поиск банковского реквизита  и проверка на уникальность*/
/*---------------------------------------------------------------------------*/
FUNCTION GetBankCodeType RETURNS LOGICAL (INPUT iBankCodeType AS CHAR):
   DEF BUFFER code         FOR code.      

   FIND FIRST code WHERE code.class EQ "КодБанка" AND
                         code.code EQ iBankCodeType NO-LOCK NO-ERROR.
   RETURN code.class EQ code.parent AND code.misc[5] EQ "YES".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*код банка в формате экспорта*/
/*---------------------------------------------------------------------------*/
FUNCTION GetBankCodeFormat RETURN CHAR (INPUT ipBankCodeType AS CHAR,
                                        INPUT ipBankCode     AS CHAR):
   RETURN 
      IF ipBankCodeType EQ "BIC" AND
         LENGTH(ipBankCode) > 8  AND
         SUBSTR(ipBankCode,9) EQ FILL("X", LENGTH(ipBankCode) - 8) THEN
         SUBSTR(ipBankCode,1,8)
      ELSE ipBankCode.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*возвращает статус предупреждения*/
/*---------------------------------------------------------------------------*/
FUNCTION GetStWarning RETURN CHAR (INPUT ipOpKind   AS CHAR,
                                   INPUT ipOpStatus AS CHAR):
  DEF VAR vStWarning AS CHAR NO-UNDO.
  vStWarning = FGetSetting("swift", "st-warning", "").
  RETURN IF vStWarning NE "" AND
            GetXattrValue("op-kind", ipOpKind, "st-warning") EQ "Да" THEN
            vStWarning
         ELSE ipOpStatus.
END.
/*---------------------------------------------------------------------------*/
/* переворачивает строку сзаду наперед*/
/*---------------------------------------------------------------------------*/
FUNCTION xover RETURNS CHARACTER (INPUT xxstr AS CHARACTER) :
    DEFINE VARIABLE i AS INT64   NO-UNDO. /* счетчик */
    DEFINE VARIABLE xxwork_str AS CHARACTER NO-UNDO. /* перевернуто */
    DO i = LENGTH(xxstr) TO 1 BY -1:
         xxwork_str = xxwork_str + SUBSTR(xxstr,i,1).
    END.
    RETURN xxwork_str.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*возвращает дату по указанному формату, если не соответсвует то ?*/
/*---------------------------------------------------------------------------*/
FUNCTION convxdate RETURNS DATE (INPUT xsource AS CHARACTER, 
                                 INPUT xformat AS CHARACTER) :
    DEFINE VARIABLE xlenxsource AS INT64   NO-UNDO. /* длина строки */
    DEFINE VARIABLE xlenxformat AS INT64   NO-UNDO. /* длина формата */
    DEFINE VARIABLE xflagxerror AS LOGICAL   NO-UNDO. /* делитель */

    DEFINE VARIABLE xyear       AS CHARACTER NO-UNDO. /* строка года */
    DEFINE VARIABLE xday        AS CHARACTER NO-UNDO. /* строка дня */
    DEFINE VARIABLE xmounth     AS CHARACTER NO-UNDO. /* строка месяца */
    DEFINE VARIABLE xtemplate AS CHARACTER NO-UNDO. /* строка шаблона */

    ASSIGN
        xsource = TRIM(xsource)
        xlenxsource = LENGTH(xsource)
        xlenxformat = LENGTH(xformat).

    IF xlenxsource <> xlenxformat THEN DO:  RETURN ?. END.

    IF INDEX(xformat,'yyyy') > 0 THEN DO:
        xyear = SUBSTR( xsource,INDEX(xformat,'yyyy'),4 ).
        RUN testxfigure(INPUT xyear, INPUT xtemplate, OUTPUT xflagxerror).
        IF xflagxerror = YES THEN DO:   RETURN ?. END.
        IF INT64(xyear) < (year(today) - 120) OR 
           INT64(xyear) > (year(today) + 10) THEN DO:   RETURN ?. END.
    END.
    ELSE IF INDEX(xformat,'yy') > 0 THEN DO:
        xyear = SUBSTR( xsource,INDEX(xformat,'yy'),2 ).
        RUN testxfigure(INPUT xyear, INPUT xtemplate, OUTPUT xflagxerror).
        IF xflagxerror = YES THEN DO: RETURN ?. END.
    END.
    ELSE IF INDEX(xformat,'y') > 0 THEN DO:   RETURN ?. END.

    IF INDEX(xformat,'dd') > 0 THEN DO:
        xday = SUBSTR( xsource,INDEX(xformat,'dd'),2 ).
        RUN testxfigure(INPUT xday, INPUT xtemplate, OUTPUT xflagxerror).
        IF xflagxerror = YES THEN DO:   RETURN ?. END.
        IF INT64(xday) < 1 OR INT64(xday) > 31 THEN DO:   RETURN ?. END.
    END.
    ELSE IF INDEX(xformat,'d') > 0 THEN DO: RETURN ?. END.

    IF INDEX(xformat,'mm') > 0 THEN DO:
        xmounth = SUBSTR( xsource,INDEX(xformat,'mm'),2 ).
        RUN testxfigure(INPUT xmounth, INPUT xtemplate, OUTPUT xflagxerror).
        IF xflagxerror = YES THEN DO:   RETURN ?. END.
        IF INT64(xmounth) < 1 OR INT64(xmounth) > 12 THEN DO:   RETURN ?. END.
    END.
    ELSE IF INDEX(xformat,'m') > 0 THEN DO:   RETURN ?. END.
    RETURN DATE(xday + "/" + xmounth + "/" + xyear).

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*возвращает целое число по указанному формату, если не соответсвует то ?*/
/*---------------------------------------------------------------------------*/
FUNCTION convxinteger RETURNS INT64 (INPUT xsource AS CHARACTER, 
                                       INPUT xformat AS CHARACTER) :
    DEFINE VARIABLE xlenxsource AS INT64   NO-UNDO. /* длина строки */
    DEFINE VARIABLE xlenxformat AS INT64   NO-UNDO. /* длина формата */
    DEFINE VARIABLE xflagxerror AS LOGICAL   NO-UNDO. /* делитель */
    DEFINE VARIABLE xtemplate AS CHARACTER NO-UNDO. /* строка шаблона */

    IF xformat <> "" THEN DO:
        ASSIGN
          xlenxsource = LENGTH(xsource)
          xlenxformat = LENGTH(xformat).

        IF xlenxsource > xlenxformat THEN RETURN ?.
    END. 
    RUN testxfigure(INPUT xsource, INPUT xtemplate, OUTPUT xflagxerror).
    IF xflagxerror = YES THEN RETURN ?.
    RETURN INT64(xsource).

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*возвращает дробное число по указанному формату, если не соответсвует то ?*/
/*---------------------------------------------------------------------------*/
FUNCTION convxdecimal RETURNS DECIMAL (INPUT xsource AS CHARACTER, 
                                       INPUT xformat AS CHARACTER) :
    DEFINE VARIABLE xlenxsource AS INT64   NO-UNDO. /* длина строки */
    DEFINE VARIABLE xlenxformat AS INT64   NO-UNDO. /* длина формата */
    DEFINE VARIABLE xflagxerror AS LOGICAL   NO-UNDO. /* делитель */
    DEFINE VARIABLE xtemplate AS CHARACTER NO-UNDO. /* строка шаблона */

    IF xformat <> "" THEN DO:
        ASSIGN
          xlenxsource = LENGTH(xsource)
          xlenxformat = LENGTH(xformat).

        IF xlenxsource > xlenxformat THEN RETURN ?.
    END. 
    IF NUM-ENTRIES(xsource,".") > 2 THEN RETURN ?.
    RUN testxfigure(INPUT xsource, INPUT xformat, OUTPUT xflagxerror).
    IF xflagxerror = YES THEN
       RETURN ?.
    RETURN DECIMAL(xsource).

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE testxfigure:
    DEFINE INPUT  PARAMETER xstr          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER xstrxtemplate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER xnotxfigure   AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE xtemplate AS CHARACTER NO-UNDO. /* строка шаблона */
    DEFINE VARIABLE j         AS INT64   NO-UNDO.

    DO j = 1 TO LENGTH(xstr):
      IF xstrxtemplate = "" AND 
         ( ASC(SUBSTR(xstr,j,1)) < 48 OR ASC(SUBSTR(xstr,j,1)) > 57 )
      THEN DO:  xnotxfigure = YES. RETURN.  END.
      ELSE IF xstrxtemplate <> "" AND 
         ((ASC(SUBSTR(xover(xstr),j,1)) < 48 OR ASC(SUBSTR(xover(xstr),j,1)) > 57) AND
           SUBSTR(xover(xstr),j,1) <> SUBSTR(xover(xstrxtemplate),j,1))
      THEN DO:
      xnotxfigure = YES. RETURN.  END.
    END.
    xnotxfigure = NO.
    RETURN.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*сравнивает валюту счета с валютой проводки, если одиноковы yes*/
/*---------------------------------------------------------------------------*/
FUNCTION comp-curr RETURN LOGICAL(INPUT curr1 AS CHAR, INPUT curr2 AS CHAR):
   IF (curr1 NE "{&in-NC-Code}" AND curr1 EQ curr2) 
    OR (curr1 EQ "{&in-NC-Code}" AND curr2 EQ "") 
   THEN RETURN YES.
   ELSE RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/* делит строку на сумму подстрок с произвольным кол-вом символов в каждой   */
/* например word-wrap("1234567","3,4") вернет "1234567"                     */
/*---------------------------------------------------------------------------*/
FUNCTION word-wrap RETURN CHAR (INPUT in-str AS CHAR,
                                INPUT in-n AS CHAR,
                                INPUT in-flag-end AS LOGICAL):

   DEF VAR wstr AS CHAR NO-UNDO.
   DEF VAR i AS INT64 NO-UNDO.
   DEF VAR j AS INT64 INITIAL 1 NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(in-n):
      ASSIGN
       wstr = wstr + ( IF i EQ 1 /*or substr(in-str,j) EQ ""*/
                      THEN ""
                      ELSE chr(1)
                     )
                   + substr(in-str,j,INT64(ENTRY(i,in-n)) )
       j = j + INT64(ENTRY(i,in-n))
      .
   END.
   RETURN wstr.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/* возвращает имеет филиал БИК или нет */
/*---------------------------------------------------------------------------*/
FUNCTION we-have-bic RETURN LOGICAL:

   DEF BUFFER banks-code FOR banks-code. 
   DEF BUFFER branch     FOR branch. 

   RELEASE banks-code.

   FOR FIRST branch WHERE branch.branch-id EQ mKodFil NO-LOCK:
      FIND FIRST banks-code 
           WHERE banks-code.bank-id        EQ branch.bank-id 
             AND banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
   END.
   RETURN AVAIL banks-code.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*с биком ли филиал*/
/*---------------------------------------------------------------------------*/
FUNCTION acctb-have-bic RETURN LOGICAL (INPUT in-acct AS CHAR):
   DEF BUFFER branch     FOR branch.     
   DEF BUFFER acct       FOR acct.     
   DEF BUFFER banks-code FOR banks-code.     

   RELEASE acct.
   RELEASE branch.
   RELEASE banks-code.
   
   FIND FIRST acct WHERE acct.acct EQ in-acct NO-LOCK NO-ERROR.
   IF AVAIL acct AND acct.cust-cat EQ "Б" THEN
      FIND FIRST branch WHERE branch.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
   IF AVAIL branch THEN
      FIND FIRST  banks-code WHERE 
                  banks-code.bank-id EQ acct.cust-id AND
                  banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
   IF AVAIL branch AND NOT AVAIL banks-code 
      THEN RETURN NO.
   RETURN YES.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*транслитерация и детранслитерация*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-trans RETURNS CHAR (INPUT trans-str AS CHAR, INPUT translation AS LOGICAL,INPUT VerForm AS CHAR).

/* ТРАНСЛИТЕРАЦИЯ */

/* кирилица */
     DEFINE VARIABLE cyr-sym AS CHAR INITIAL
       "А~001Б~001В~001Г~001Д~001Е~001Ё~001Ж~001З~001И~001Й~001К~001Л~001М~001Н~001О~001П~001Р~001С~001Т~001У~001Ф~001Х~001Ц~001Ч~001Ш~001Щ~001Ъ~001Ы~001Ь~001Э~001Ю~001Я~001/~001-~001:~001?~001,~001(~001)~001+"
       NO-UNDO.
/* латинский эквивалент */
     DEFINE VARIABLE lat-sym AS CHAR INITIAL
       "A~001B~001V~001G~001D~001E~001E~001J~001Z~001I~001)~001K~001L~001M~001N~001O~001P~001R~001S~001T~001U~001F~001H~001+~001(~001,~001?~001'~001Y~001X~001W~001Q~001C~001WX~001QX~001CX~001AX~001EX~001IX~001OX~001UX"
       NO-UNDO.
/* не транслировать участок заключонный в эти символы */
     DEFINE VARIABLE lat-limit AS CHAR INITIAL "YX" NO-UNDO.
/* не транслировать эти символы */
     DEFINE VARIABLE not-detrans AS CHAR INITIAL "'" NO-UNDO.
/* при обнаружении этих символов дублировать их */
     DEFINE VARIABLE double-sym AS CHAR INITIAL "ЭЬ~001ЮЬ~001ЯЬ~001АЬ~001ЕЬ~001ИЬ~001ОЬ~001УЬ~001ЫЬ" NO-UNDO.
/* при обнаружении этих символов раздублировать их */
     DEFINE VARIABLE quoter-sym AS CHAR INITIAL "AXAX~001WXWX~001QXQX~001CXCX~001EXEX~001IXIX~001OXOX~001UXUX" NO-UNDO.
 /* RETURN */
     DEFINE VARIABLE work-str AS CHAR INITIAL "" NO-UNDO.
/* длина входной строки */
     DEFINE VARIABLE len-str AS INT64 INITIAL 0 NO-UNDO.
/* счетчик */
     DEFINE VARIABLE j AS INT64 INITIAL 0 NO-UNDO.
/* счетчик */
     DEFINE VARIABLE k AS INT64 INITIAL 2 NO-UNDO.
/* флаг попуска */
     DEFINE VARIABLE space-flag AS LOGICAL INITIAL NO NO-UNDO.
/* флаг попуска */
     DEFINE VARIABLE end-limit-flag AS LOGICAL INITIAL NO NO-UNDO.
/* флаг попуска */
     DEFINE VARIABLE tree-space-flag AS INT64 INITIAL 3 NO-UNDO.
/* строка включся в YX */
     DEFINE VARIABLE in-yx-str AS CHAR INITIAL ".~001 ~001-~001:" NO-UNDO.
/* старая транслитерация */
     DEFINE VARIABLE vOldTranslit AS LOG  NO-UNDO.
     DEFINE VARIABLE vBegStr      AS CHAR NO-UNDO.

vOldTranslit = FGetSetting("SWIFT","OldTranslit","Нет") EQ "Да".

trans-str = TRIM(trans-str).
IF NOT CAN-DO("RUR5,RUR6",VerForm) THEN DO:
ASSIGN  
        trans-str = CAPS(trans-str)
        len-str   = LENGTH(trans-str).
 

DO j = 1 TO len-str:
    IF SUBSTRING(trans-str,j,1) = '"' THEN DO:
         OVERLAY(trans-str,j) = " ".
    END.
END.


IF translation AND
   NOT vOldTranslit AND
   trans-str BEGINS "~{VO" AND
   INDEX(trans-str,"~}") > 0 THEN
   ASSIGN 
      vBegStr   = "YX(" + SUBSTR(trans-str, 2, INDEX(trans-str,"~}") - 2) + ")YX"
      trans-str = SUBSTR(trans-str, INDEX(trans-str,"~}") + 1)
   .
ELSE IF NOT vOldTranslit AND
   trans-str BEGINS "YX(VO" AND
   INDEX(trans-str,")YX") > 0 THEN
   ASSIGN 
      vBegStr   = "~{" + SUBSTR(trans-str, 4, INDEX(trans-str,")YX") - 4) + "~}"
      trans-str = SUBSTR(trans-str, INDEX(trans-str,")YX") + 3)
   .

DO j = 1 TO len-str:
  IF translation THEN DO: /* translation */
    IF space-flag = YES THEN space-flag = NO.
    ELSE IF SUBSTRING(trans-str,j,1) = '"' THEN DO:
         work-str = work-str + "'".
    END.
    ELSE IF j <> len-str AND LOOKUP( SUBSTRING(trans-str,j,2),double-sym,"~001" ) > 0 THEN DO:
        ASSIGN 
         work-str = work-str + 
           ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),cyr-sym,"~001" ),lat-sym,"~001" ) +
           ENTRY( LOOKUP( SUBSTRING(trans-str,j + 1,1),cyr-sym,"~001" ),lat-sym,"~001" ) +
           ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),cyr-sym,"~001" ),lat-sym,"~001" ) +
           ENTRY( LOOKUP( SUBSTRING(trans-str,j + 1,1),cyr-sym,"~001" ),lat-sym,"~001" )
         space-flag = YES
        .
    END.
    ELSE IF (ASC(SUBSTRING(trans-str,j,1)) >= 65 AND ASC(SUBSTRING(trans-str,j,1)) <= 90)
         OR end-limit-flag THEN DO:
        IF end-limit-flag = NO THEN DO:
          IF SUBSTRING(trans-str,j,2) = lat-limit THEN
          ASSIGN
            end-limit-flag = IF (ASC(SUBSTR(trans-str,j + 2,1)) < 65 OR ASC(SUBSTR(trans-str,j + 2,1)) > 90)
                           AND LOOKUP(SUBSTR(trans-str,j + 2,1),in-yx-str,"~001") EQ 0 THEN NO
                          ELSE YES
            work-str = work-str + lat-limit + lat-limit + lat-limit +
                       IF (ASC(SUBSTR(trans-str,j + 2,1)) < 65 OR ASC(SUBSTR(trans-str,j + 2,1)) > 90)
                        AND LOOKUP(SUBSTR(trans-str,j + 2,1),in-yx-str,"~001") EQ 0 THEN lat-limit
                       ELSE ""
             space-flag = YES.
          ELSE IF (ASC(SUBSTR(trans-str,j + 1,1)) < 65 OR ASC(SUBSTR(trans-str,j + 1,1)) > 90)
               AND LOOKUP(SUBSTR(trans-str,j + 1,1),in-yx-str,"~001") EQ 0 THEN
           ASSIGN
/*            end-limit-flag = YES*/
            work-str = work-str + lat-limit + SUBSTR(trans-str,j,1) + lat-limit.
          ELSE 
           ASSIGN
            end-limit-flag = IF (ASC(SUBSTR(trans-str,j + 1,1)) < 65 OR ASC(SUBSTR(trans-str,j + 1,1)) > 90)
               AND LOOKUP(SUBSTR(trans-str,j + 1,1),in-yx-str,"~001") EQ 0 THEN NO
                             ELSE YES
            work-str = work-str + lat-limit + SUBSTR(trans-str,j,1).
        END.
        ELSE DO:
          IF SUBSTRING(trans-str,j,2) = lat-limit THEN
          ASSIGN
            work-str = work-str + lat-limit + lat-limit + IF (ASC(SUBSTR(trans-str,j + 2,1)) < 65 OR ASC(SUBSTR(trans-str,j + 2,1)) > 90)
                           AND LOOKUP(SUBSTR(trans-str,j + 2,1),in-yx-str,"~001") EQ 0 THEN lat-limit
                          ELSE ""
            end-limit-flag = IF (ASC(SUBSTR(trans-str,j + 2,1)) < 65 OR ASC(SUBSTR(trans-str,j + 2,1)) > 90)
                           AND LOOKUP(SUBSTR(trans-str,j + 2,1),in-yx-str,"~001") EQ 0 THEN NO
                          ELSE YES
             space-flag = YES.
          ELSE IF (ASC(SUBSTR(trans-str,j + 1,1)) < 65 OR ASC(SUBSTR(trans-str,j + 1,1)) > 90)
               AND LOOKUP(SUBSTR(trans-str,j + 1,1),in-yx-str,"~001") EQ 0 THEN
           ASSIGN
            work-str = work-str + SUBSTR(trans-str,j,1) + lat-limit
            end-limit-flag = NO.
          ELSE 
           ASSIGN
            work-str = work-str + SUBSTR(trans-str,j,1).

        END.
    END.
    ELSE IF LOOKUP( SUBSTRING(trans-str,j,1),cyr-sym,"~001" ) > 0 THEN DO:
         work-str = work-str + 
           ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),cyr-sym,"~001" ),lat-sym,"~001" ).
    END.
    ELSE DO:
         work-str = work-str + SUBSTRING(trans-str,j,1).
    END.
  END.
  ELSE DO: /* detranslation */
    IF tree-space-flag < 3 THEN tree-space-flag = tree-space-flag + 1.
    ELSE IF 3 <= len-str - j AND LOOKUP( SUBSTRING(trans-str,j,4),quoter-sym,"~001" ) > 0 THEN DO:
        ASSIGN 
         work-str = work-str + 
           ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),lat-sym,"~001" ),cyr-sym,"~001" ) +
           ENTRY( LOOKUP( SUBSTRING(trans-str,j + 1,1),lat-sym,"~001" ),cyr-sym,"~001" )
         tree-space-flag = 0
        .
    END.
    ELSE IF ( 3 <= len-str - j AND SUBSTRING(trans-str,j,2) = lat-limit AND NUM-ENTRIES(trans-str,lat-limit) >= 3) OR end-limit-flag = YES THEN DO:
         /* существуют только внешние апострофы а всё остальное включение */
         /* sssxyrrrxyxyrrrxyxyrrrxysss */

        IF end-limit-flag = NO THEN
         ASSIGN
          end-limit-flag = YES  
          /*work-str = work-str + SUBSTRING(trans-str,j,2) !!!*/ 
          tree-space-flag = 2
         .
        ELSE IF SUBSTRING(trans-str,j,4) = (lat-limit + lat-limit) THEN 
         ASSIGN
          work-str = work-str + SUBSTRING(trans-str,j,2)
          tree-space-flag = 0
         .
        ELSE IF SUBSTRING(trans-str,j,2) = lat-limit THEN 
         ASSIGN /*work-str = work-str + SUBSTRING(trans-str,j,2) !!!*/
                tree-space-flag = 2
                end-limit-flag = NO
         .
        ELSE 
         ASSIGN work-str = work-str + SUBSTRING(trans-str,j,1)
         .
        
    END.
    ELSE IF LOOKUP( SUBSTRING(trans-str,j,2),lat-sym,"~001" ) > 0 THEN DO:
       ASSIGN
         tree-space-flag = 2
         work-str = work-str + IF LOOKUP(SUBSTRING(trans-str,j,2),not-detrans,"~001") > 0 THEN SUBSTRING(trans-str,j,2) /*!!!*/
           ELSE ENTRY( LOOKUP( SUBSTRING(trans-str,j,2),lat-sym,"~001" ),cyr-sym,"~001" ).
    END.
    ELSE IF LOOKUP( SUBSTRING(trans-str,j,1),lat-sym,"~001" ) > 0 THEN DO:
         work-str = work-str + IF LOOKUP(SUBSTRING(trans-str,j,1),not-detrans,"~001") > 0 THEN SUBSTRING(trans-str,j,1) /*!!!*/
           ELSE ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),lat-sym,"~001" ),cyr-sym,"~001" ).
    END.
    ELSE DO:
         work-str = work-str + SUBSTRING(trans-str,j,1).
    END.
  END.
END.
END.
ELSE IF VerForm EQ "RUR5" THEN DO:

/* ТРАНСЛИТЕРАЦИЯ SWIFT-RUR 5*/
/* l-sym */
     DEFINE VARIABLE l-sym AS CHAR INITIAL "o~001i~001c~001q~001x~001e~001u~001a".

ASSIGN
/* кирилица */
     cyr-sym =
       "А~001Б~001В~001Г~001Д~001Е~001Ё~001Ж~001З~001И~001Й~001К~001Л~001М~001Н~001О~001П~001Р~001С~001Т~001У~001Ф~001Х~001Ц~001Ч~001Ш~001Щ~001Ъ~001Ы~001Ь~001Э~001Ю~001Я~0010~0011~0012~0013~0014~0015~0016~0017~0018~0019~001~{~001~}~001(~001)~001?~001+~001,~001/~001-~001.~001:~001 "
/* латинский эквивалент */
     lat-sym =
       "A~001B~001V~001G~001D~001E~001o~001J~001Z~001I~001i~001K~001L~001M~001N~001O~001P~001R~001S~001T~001U~001F~001H~001C~001c~001Q~001q~001x~001Y~001X~001e~001u~001a~0010~0011~0012~0013~0014~0015~0016~0017~0018~0019~001((~001))~001(~001)~001?~001+~001,~001/~001-~001.~001:~001 "
/* l-sym */
     l-sym = "o~001i~001c~001q~001x~001e~001u~001a"
/* не транслировать участок заключонный в эти символы */
     lat-limit = "'"
 /* RETURN */
     work-str = ""
/* длина входной строки */
     len-str = 0
/* счетчик */
     j = 0
/* флаг попуска */
     space-flag = NO
/* флаг попуска */
     end-limit-flag = NO
     len-str   = LENGTH(trans-str)
     vBegStr   = "".
     IF translation THEN
     DO:
        ASSIGN
           not-detrans = "~{,~}"
           trans-str   = REPLACE(trans-str,"'"," ")
           trans-str   = REPLACE(trans-str,'"',' ')
        .
        IF NOT vOldTranslit AND
           trans-str BEGINS "~{VO" AND
           INDEX(trans-str,"~}") > 0 THEN
           ASSIGN 
              vBegStr   = "'(" + SUBSTR(trans-str, 2, INDEX(trans-str,"~}") - 2) + ")'"
              trans-str = SUBSTR(trans-str, INDEX(trans-str,"~}") + 1)
           .
     END.
     ELSE DO:
        not-detrans = "((,))".
        IF NOT vOldTranslit AND
           trans-str BEGINS "'(VO" AND
           INDEX(trans-str,")'") > 0 THEN
           ASSIGN 
              vBegStr   = "~{" + SUBSTR(trans-str, 3, INDEX(trans-str,")'") - 3) + "~}"
              trans-str = SUBSTR(trans-str, INDEX(trans-str,")'") + 2)
           .
     END.

&SCOPED-DEFINE GetTrans   ENTRY(LOOKUP(SUBSTR(trans-str,j,1),cyr-sym,'~001'), ~
                                lat-sym,'~001')
&SCOPED-DEFINE GetDeTrans ENTRY(LOOKUP(SUBSTR(trans-str,j,2),lat-sym,"~001"), ~
                                cyr-sym,"~001")
DO j = 1 TO len-str:
  IF translation THEN DO: /* translation */

    IF SUBSTRING(trans-str,j,1) = "'" THEN
       end-limit-flag = NOT end-limit-flag.

    ELSE IF (SUBSTRING(trans-str,j,1) >= "A" AND 
             SUBSTRING(trans-str,j,1) <= "Z") THEN
    ASSIGN
      work-str = work-str + ( IF NOT end-limit-flag THEN "'" ELSE "")
      end-limit-flag = YES.

    ELSE
    ASSIGN
      work-str = work-str + ( IF end-limit-flag THEN "'" ELSE "")
      end-limit-flag = NO.



    IF end-limit-flag THEN
    work-str = work-str + ( IF CAN-DO(not-detrans,SUBSTRING(trans-str,j,1)) THEN
                              {&GetTrans}
                           ELSE SUBSTRING(trans-str,j,1)) + 
               ( IF j EQ len-str THEN "'" ELSE "").

    ELSE IF INDEX(cyr-sym,SUBSTRING(trans-str,j,1)) NE 0 THEN
       work-str = work-str + {&GetTrans}.
  END.
  ELSE DO: /* detranslation */

    IF space-flag THEN space-flag = NO.
    ELSE IF SUBSTRING(trans-str,j,1) = "'" THEN
    end-limit-flag = NOT end-limit-flag.

    ELSE IF end-limit-flag THEN
    DO:
       work-str = work-str + ( IF CAN-DO(not-detrans,SUBSTR(trans-str,j,2)) THEN
                                 {&GetDeTrans}
                              ELSE SUBSTRING(trans-str,j,1)).
       IF CAN-DO(not-detrans,SUBSTR(trans-str,j,2)) THEN
          space-flag = YES.

    END.
    ELSE IF INDEX(l-sym,SUBSTRING(trans-str,j,1)) NE 0 AND
            ASC(ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),lat-sym,"~001"),
                       lat-sym,"~001")
               ) NE ASC(SUBSTRING(trans-str,j,1)) THEN

    work-str = work-str + 
        ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),
                       SUBSTR(lat-sym,INDEX(lat-sym,SUBSTR(trans-str,j,1)) + 1),
                       "~001"
                     ),
               SUBSTR(cyr-sym,INDEX(lat-sym,SUBSTR(trans-str,j,1)) + 1),
               "~001"
             ).
    ELSE IF INDEX(lat-sym,SUBSTRING(trans-str,j,2)) NE 0 THEN
    ASSIGN
    space-flag = YES
    work-str = work-str + {&GetDeTrans}.

    ELSE IF INDEX(lat-sym,SUBSTRING(trans-str,j,1)) NE 0 THEN
    ASSIGN
    work-str = work-str + 
        ENTRY( LOOKUP( SUBSTRING(trans-str,j,1),lat-sym,"~001"),
               cyr-sym,"~001"
             ).
  END.
END.

END.
ELSE RUN SWTransRUR6(INPUT translation,
                     INPUT  trans-str,
                     OUTPUT work-str).


RETURN vBegStr + work-str.

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*значение классификатора по коду и классу*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-code RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-code AS CHAR).

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ in-class
   AND code.code EQ in-code NO-LOCK NO-ERROR.
   IF AVAIL code AND {assigned code.val}
      THEN RETURN code.val.
      ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*описание классификатора по коду и классу*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-code2 RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-code AS CHAR).
   DEF BUFFER code FOR code.

    FIND FIRST code WHERE code.class EQ in-class
    AND code.code EQ in-code NO-LOCK NO-ERROR.
    IF AVAIL code  AND {assigned code.description[1]}
       THEN RETURN code.description[1].
       ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*код классификатора по значение и классу*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-val RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-val AS CHAR).
   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ in-class
   AND code.val EQ in-val NO-LOCK NO-ERROR.
   IF AVAIL code THEN RETURN code.code.
   ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*код клиринговой системы по коду идентификатора банка
  (неуникальные тоже обрабатываются)*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-val-beg RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-val AS CHAR).
   DEF BUFFER code FOR code.
   DEF BUFFER xycode FOR code.
   RELEASE code.
   RELEASE xycode.
  
   FIND FIRST xycode WHERE xycode.class EQ "КодБанка" AND
                           xycode.code EQ in-val NO-LOCK NO-ERROR.
   IF AVAIL xycode THEN
   DO:
      IF xycode.class EQ xycode.parent THEN
      FIND FIRST code WHERE code.class EQ in-class AND
                            code.val EQ in-val NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST code WHERE code.class EQ in-class AND
                            code.val EQ xycode.parent NO-LOCK NO-ERROR.
   END.
   IF AVAIL code THEN RETURN code.code.
   ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*код классификатора по описанию и классу*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-desc RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-val AS CHAR).
   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ in-class
   AND code.description[1] EQ in-val NO-LOCK NO-ERROR.
   IF AVAIL code THEN RETURN code.code.
      ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*misc классификатора по описанию и классу*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-descm RETURNS CHAR (INPUT in-class AS CHAR, INPUT in-val AS CHAR).
   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ in-class
   AND code.description[1] EQ in-val NO-LOCK NO-ERROR.
   IF AVAIL code THEN RETURN code.misc[1] + "," + code.code.
   ELSE RETURN "?".
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*убивает 1 и 3 символы и разбивает текст по 79 символов с рзделителем ~n*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-buf RETURNS CHAR (INPUT in-buf AS CHAR).
 DEFINE VARIABLE lb AS INT64 NO-UNDO.
 DEFINE VARIABLE zj AS INT64 NO-UNDO.

 IF length(in-buf) - length(TRIM(in-buf)) > 1 AND length(in-buf) > 80 THEN in-buf = TRIM(in-buf).

 IF INDEX(in-buf,chr(1)) NE 0 THEN
 in-buf = substr(in-buf,1,INDEX(in-buf,chr(1)) - 1) + substr(in-buf,INDEX(in-buf,chr(1)) + 1).

 IF INDEX(in-buf,chr(3)) NE 0 THEN
 in-buf = substr(in-buf,1,INDEX(in-buf,chr(3)) - 1) + substr(in-buf,INDEX(in-buf,chr(3)) + 1).

 lb = length(in-buf).
 IF lb > 79 THEN DO zj = 2 TO trunc(lb / 79,0) + 1:
  substr(in-buf,zj * 80 - 80 ,0) = "~n".
 END.

 RETURN in-buf.

END FUNCTION.
/*---------------------------------------------------------------------------*/
/* обработка др swift-det-cod и swift-det-inf для экспорт 72 тега*/
/*---------------------------------------------------------------------------*/
FUNCTION sw-e72 RETURNS CHAR (INPUT in-recop  AS RECID,
                              INPUT in-headr  AS LOGICAL, 
                              INPUT tran      AS LOGICAL, 
                              INPUT VerForm   AS CHAR,
                              INPUT iFormISO  AS LOGICAL).

   DEFINE BUFFER signs2 FOR signs.
   DEFINE VARIABLE str72      AS CHAR     NO-UNDO. /* вых. стр.         */
   DEFINE VARIABLE wstr72     AS CHAR     NO-UNDO. /* вых. стр.         */
   DEFINE VARIABLE ww         AS CHAR     NO-UNDO. /* вых. стр.         */
   DEFINE VARIABLE wwl        AS LOGICAL  NO-UNDO. /* вых. стр.         */
   DEFINE VARIABLE i72        AS INT64      NO-UNDO. /* счетчик.          */
   DEFINE VARIABLE l72        AS INT64      NO-UNDO. /* length счетчик.   */
   DEFINE VARIABLE VDetCod    AS CHAR     NO-UNDO. /* список кодов др frm-det-cod      */
   DEFINE VARIABLE VDetInf    AS CHAR     NO-UNDO. /* список значений др frm-det-inf   */
   
   DEFINE VARIABLE vIndNzp     AS INT64   INIT 0   NO-UNDO.
   DEFINE VARIABLE vCode       AS CHARACTER INIT ""  NO-UNDO.
   DEFINE VARIABLE vVal        AS CHARACTER INIT ""  NO-UNDO.
   DEFINE VARIABLE vValTrans   AS CHARACTER INIT ""  NO-UNDO.
   DEFINE VARIABLE vIsNzp      AS LOGICAL   INIT NO  NO-UNDO.

   DEF BUFFER op FOR op.

   FIND FIRST op WHERE RECID(op) EQ in-recop NO-LOCK NO-ERROR.
   IF AVAIL op THEN DO:
     VDetCod = GetXattrValueEx("op",STRING(op.op),"swift-det-cod",""). 
     VDetInf = GetXattrValueEx("op",STRING(op.op),"swift-det-inf","").
  
     IF VDetCod NE "" AND
        VDetInf NE "" AND
        NUM-ENTRIES(VDetCod,"|") EQ NUM-ENTRIES(VDetInf,"|") AND
        ENTRY(1,VDetCod,"|") NE "" THEN
     DO:
        l72 = NUM-ENTRIES(VDetCod,"|").
        
        DO i72 = 1 TO l72:

            vCode       = ENTRY(i72,VDetCod,"|").
            vVal        = ENTRY(i72,VDetInf,"|").
            vValTrans   = IF tran THEN sw-trans(vVal,YES,VerForm) ELSE "".

            IF {assigned vCode} THEN ASSIGN vIsNzp = (vCode EQ "NZP").
            IF vIsNzp THEN vIndNzp = vIndNzp + 1.        /* счетчик строк NZP */

            IF      vCode NE "" THEN ASSIGN wstr72 =(IF in-headr THEN ":72:" ELSE "") + "/" + vCode + "/".
            ELSE IF vVal  NE "" THEN ASSIGN wstr72 = "//".
            
            IF vCode NE "" OR 
               vVal  NE ""  
            THEN DO:
              IF tran THEN DO:
                 IF LENGTH(vValTrans) > 28 THEN DO:
                    ASSIGN 
                       wstr72  = wstr72 + SUBSTR(vValTrans,1,28) + "~n" + "//" + SUBSTR(vValTrans,29,28)
                       vIndNzp = vIndNzp + 1 WHEN vIsNzp /* счетчик строк NZP */
                    .
                 END.
                 ELSE
                    wstr72 = wstr72 + vValTrans.
              END.
              ELSE
                 wstr72 = wstr72 + vVal.
            
              in-headr = NO.
              IF NOT (vIndNzp GT 2 AND vIsNzp AND iFormISO)  THEN
                 str72 = ( IF str72 NE "" THEN str72 + "~n" ELSE "") + wstr72.
            END.
         END.
      END.
   END.

   ASSIGN
      wstr72   = ""
      wwl      = NO
      ww       = "".

  IF INDEX(str72,"/BNF/") NE 0 THEN DO:
  DO i72 = 1 TO NUM-ENTRIES(str72,"~n"):
     IF ENTRY(i72,str72,"~n") BEGINS "/BNF/" OR ENTRY(i72,str72,"~n") BEGINS ":72:/BNF/" OR wwl THEN DO:
       wwl = YES.
       IF ENTRY(i72,str72,"~n") BEGINS "/BNF/" OR ENTRY(i72,str72,"~n") BEGINS ":72:/BNF/"  OR ENTRY(i72,str72,"~n") BEGINS "//" THEN DO:
          ww = ww + ( IF ww EQ "" OR ww EQ ":72:" THEN "" ELSE "~n") + ENTRY(i72,str72,"~n").
       END.
       ELSE IF ENTRY(i72,str72,"~n") BEGINS "/" THEN DO:
           wwl = NO.
           wstr72 = wstr72 + ( IF wstr72 EQ "" THEN "" ELSE "~n") + ENTRY(i72,str72,"~n").
       END.
     END.
     ELSE DO:
         IF ENTRY(i72,str72,"~n") BEGINS ":72:" THEN
         ASSIGN
         ww = ":72:"
         wstr72 = wstr72 + ( IF wstr72 EQ "" THEN "" ELSE "~n") + substr(ENTRY(i72,str72,"~n"),5).
         ELSE 
         wstr72 = wstr72 + ( IF wstr72 EQ "" THEN "" ELSE "~n") + ENTRY(i72,str72,"~n").
     END.
  END.
  str72 = ww + "~n" + wstr72.
  END.
  RETURN str72.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*  поиск счета c-nostro */
/*---------------------------------------------------------------------------*/
FUNCTION sw-icn RETURNS CHAR (INPUT in-acct AS CHAR ,INPUT in-curr AS CHAR).
   DEF BUFFER acct     FOR acct.
   DEF BUFFER c-nostro FOR c-nostro.

     FIND FIRST acct WHERE acct.acct = in-acct AND acct.curr = in-curr NO-LOCK NO-ERROR.
     IF NOT AVAIL acct THEN DO:
       FIND FIRST c-nostro WHERE c-nostro.corr-acct EQ in-acct AND
         CAN-FIND(FIRST acct WHERE acct.acct EQ c-nostro.acct AND
                                   acct.curr EQ c-nostro.curr AND
                                   acct.close-date EQ ?) NO-LOCK NO-ERROR.
       IF AVAIL c-nostro THEN DO:
           FIND FIRST acct OF c-nostro NO-LOCK NO-ERROR.
           IF AVAIL acct THEN RETURN acct.acct.
       END.
     END.
     RETURN in-acct.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*  определяет есть ли кирилица в строке */
/*---------------------------------------------------------------------------*/
FUNCTION is-cyril RETURNS LOGICAL (INPUT in-buf AS CHAR).
 DEFINE VARIABLE i AS INT64 NO-UNDO.
 DO i = 1 TO length(in-buf):
   IF asc(SUBSTRING(in-buf,i,1)) >= 128 THEN RETURN YES.
 END.
RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*колличество связанных документов*/
/*---------------------------------------------------------------------------*/
FUNCTION num-link RETURNS INT64
   (INPUT ipFileName AS CHAR,
    INPUT ipCode     AS CHAR,
    INPUT ipCodeVal  AS CHAR):

   DEF BUFFER signs FOR signs.

   FOR EACH signs WHERE signs.file-name EQ ipFileName AND
                        signs.code      EQ ipCode     AND
                        signs.code-val  EQ ipCodeVal
   NO-LOCK BY signs.surr:
      ACCUM signs.surr (COUNT).
   END.
   RETURN ACCUM COUNT (signs.surr).
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*определяет тип доумента rec или send*/
/*---------------------------------------------------------------------------*/
FUNCTION send-rec RETURNS CHAR (INPUT op-rec AS recid).

    DEF BUFFER cracct   FOR acct.
    DEF BUFFER acct     FOR acct.
    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER op-bank  FOR op-bank.

    DEF VAR Vacct-rec  AS CHAR NO-UNDO. /* значений др acct-rec*/
    DEF VAR Vacct-send AS CHAR NO-UNDO. /* значений др acct-send*/
    DEF VAR Vinn-rec   AS CHAR NO-UNDO. /* значений др inn-rec*/
    DEF VAR Vinn-send  AS CHAR NO-UNDO. /* значений др inn-send*/
    DEF VAR Vname-rec  AS CHAR NO-UNDO. /* значений др name-rec*/
    DEF VAR Vname-send AS CHAR NO-UNDO. /* значений др name-send*/
  
    FIND FIRST op WHERE recid(op) EQ op-rec NO-LOCK NO-ERROR.
    IF AVAIL op THEN DO:

       Vinn-send  = GetXattrValueEx("op",STRING(op.op),"inn-send","").
       Vacct-send = GetXattrValueEx("op",STRING(op.op),"acct-send","").
       Vname-send = GetXattrValueEx("op",STRING(op.op),"name-send","").
       Vinn-rec   = GetXattrValueEx("op",STRING(op.op),"inn-rec",""). 
       Vacct-rec  = GetXattrValueEx("op",STRING(op.op),"acct-rec","").
       Vname-rec  = GetXattrValueEx("op",STRING(op.op),"name-rec","").

        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-db EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ? NO-LOCK NO-ERROR.
        FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
        FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.
        FIND FIRST cracct WHERE cracct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.

        IF op.doc-kind EQ "rec" OR op.doc-kind EQ "send" 
         THEN RETURN op.doc-kind.
        ELSE IF CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "") AND
                CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "rec")
         THEN RETURN "send".
        ELSE IF CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "") AND
                CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "send")
         THEN RETURN "rec".
        ELSE IF Vinn-send NE "" OR
           Vacct-send NE "" OR
           Vname-send NE ""
        THEN RETURN "rec".
        ELSE IF Vinn-rec NE "" OR
           Vacct-rec NE "" OR
           Vname-rec NE ""
        THEN RETURN "send".
        ELSE IF CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "")
           AND AVAIL acct
           AND CAN-DO(FGetSetting("НазнСчМБР",?,?),acct.contract)
        THEN RETURN "send".
        ELSE IF CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "")
           AND AVAIL cracct
           AND CAN-DO(FGetSetting("НазнСчМБР",?,?),cracct.contract)
        THEN RETURN "rec".
        ELSE RETURN "".
    END.

END FUNCTION.
/*---------------------------------------------------------------------------*/
/*возвращает recid банковского реквизита с заданной ролью (с учетом пустой роли)*/
/*---------------------------------------------------------------------------*/
FUNCTION get-op-bank RETURNS RECID (INPUT op-rec AS recid, INPUT in-op-bank-type AS CHAR).
    DEF BUFFER cracct FOR acct.
    DEF BUFFER acct     FOR acct.
    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER op-bank  FOR op-bank.

    DEF VAR signs-send AS LOGICAL NO-UNDO.
    DEF VAR signs-rec AS LOGICAL NO-UNDO.
    signs-send = NO.
    signs-rec = NO.
    DEFINE VARIABLE Vinn-send AS CHAR NO-UNDO. /* значений др inn-send*/
    DEFINE VARIABLE Vacct-send AS CHAR NO-UNDO. /* значений др acct-send*/
    DEFINE VARIABLE Vname-send AS CHAR NO-UNDO. /* значений др name-send*/
    DEFINE VARIABLE Vinn-rec AS CHAR NO-UNDO. /* значений др inn-rec*/
    DEFINE VARIABLE Vacct-rec AS CHAR NO-UNDO. /* значений др acct-rec*/
    DEFINE VARIABLE Vname-rec AS CHAR NO-UNDO. /* значений др name-rec*/
  

    FIND FIRST op WHERE recid(op) EQ op-rec NO-LOCK NO-ERROR.
    IF AVAIL op THEN DO:

       Vinn-send  = GetXattrValueEx("op",STRING(op.op),"inn-send","").
       Vacct-send = GetXattrValueEx("op",STRING(op.op),"acct-send","").
       Vname-send = GetXattrValueEx("op",STRING(op.op),"name-send","").
       Vinn-rec   = GetXattrValueEx("op",STRING(op.op),"inn-rec",""). 
       Vacct-rec  = GetXattrValueEx("op",STRING(op.op),"acct-rec","").
       Vname-rec  = GetXattrValueEx("op",STRING(op.op),"name-rec","").

        IF Vinn-send NE "" OR
           Vacct-send NE "" OR
           Vname-send NE ""
        THEN signs-send = YES.
        IF Vinn-rec NE "" OR
           Vacct-rec NE "" OR
           Vname-rec NE ""
        THEN signs-rec = YES.
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-db EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ? NO-LOCK NO-ERROR.
        FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
        FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.
        FIND FIRST cracct WHERE cracct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
        FIND FIRST op-bank OF op WHERE op-bank.op-bank-type EQ in-op-bank-type NO-LOCK NO-ERROR.
        IF AVAIL op-bank THEN RETURN recid(op-bank).
        ELSE DO:
            FIND FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "" NO-LOCK NO-ERROR.
            IF AVAIL op-bank THEN DO:
                 IF in-op-bank-type EQ "send" THEN DO:
                    IF (op.doc-kind EQ "send" OR (op.doc-kind NE "rec" AND signs-rec)) OR
                    (CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "") AND
                     CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "rec")) OR
                    (
                    AVAIL acct AND
                    CAN-DO(FGetSetting("НазнСчМБР",?,?),acct.contract) AND
                    AVAIL cracct AND
                    NOT CAN-DO(FGetSetting("НазнСчМБР",?,?),cracct.contract) AND
                    op.doc-kind NE "rec"
                    )
                    THEN RETURN recid(op-bank).
                 END.
                 ELSE IF in-op-bank-type EQ "rec" THEN DO:

                    IF (op.doc-kind EQ "send" OR (op.doc-kind NE "rec" AND signs-rec)) OR
                    (CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "") AND
                     CAN-FIND(FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "rec")) OR
                    (
                    AVAIL acct AND
                    CAN-DO(FGetSetting("НазнСчМБР",?,?),acct.contract) AND
                    AVAIL cracct AND
                    NOT CAN-DO(FGetSetting("НазнСчМБР",?,?),cracct.contract) AND
                    op.doc-kind NE "rec"
                    )
                    THEN.
                     ELSE RETURN recid(op-bank).
                 END.
            END.
        END.
    END.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Возвращает Возвращает дату поступления                                     */
/*----------------------------------------------------------------------------*/
FUNCTION ChangeInsDate RETURNS DATE (INPUT iOpDate  AS DATE,
                                     INPUT iInsDate AS DATE).
   DEF VAR vInsDate    AS DATE NO-UNDO.
   DEF VAR vI          AS CHAR NO-UNDO.

   vInsDate = iInsDate.

   vI = FGetSetting("OpDateInit", "ChangeInsDateI", "").
   CASE vI:
      WHEN "0" THEN
         ASSIGN 
            vInsDate = iOpDate
               WHEN FGetSetting("OpDateInit", "ChangeInsDate", "")   EQ "Да".
      WHEN "1" THEN 
         vInsDate = iOpDate.
      WHEN "2" THEN DO:
         vInsDate = IF iInsDate EQ ? THEN iOpDate
                                     ELSE iInsDate.
      END.
      WHEN "3" THEN 
         vInsDate = IF iInsDate EQ ? THEN TODAY
                                     ELSE iInsDate.
   END CASE.
   RETURN vInsDate.
END FUNCTION.

/*---------------------------------------------------------------------------*/
/*формирование 50 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw50 :

    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM rec-open  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.


    DEF VAR sym-skip   AS CHAR INITIAL "~n" NO-UNDO. 
    DEF VAR tmp-send   AS LOGICAL NO-UNDO. 
    DEF VAR tmp-acct   AS CHAR NO-UNDO. 
    DEF VAR v-acct-db  AS CHAR NO-UNDO.  /* в send-rec теряется указатель на op-entry */
    DEF VAR tmp-inn    AS CHAR NO-UNDO. 
    DEF VAR tmp-name   AS CHAR NO-UNDO  EXTENT 5. 
    DEF VAR db-inn     AS CHAR NO-UNDO. 
    DEF VAR db-name    AS CHAR NO-UNDO EXTENT 2. 
    DEF VAR tmp-i      AS INT64 NO-UNDO. 
    DEF VAR tmp-j      AS INT64 NO-UNDO. 
    DEF VAR ch-ret     AS CHAR NO-UNDO. 
    DEF VAR buf-set    AS CHAR NO-UNDO.
    DEF VAR VNotISO    AS LOG NO-UNDO.  /*усли нет то наименования не обрезается*/
    DEF VAR Vinn-send  AS CHAR NO-UNDO. /* значений др inn-send*/
    DEF VAR Vacct-send AS CHAR NO-UNDO. /* значений др acct-send*/
    DEF VAR Vname-send AS CHAR NO-UNDO. /* значений др name-send*/
    DEF VAR VOpKind    AS CHAR NO-UNDO. /* код транзакции */
    DEF VAR VOptempl   AS CHAR NO-UNDO. /* код шаблона */
    DEF VAR VFormat    AS CHAR NO-UNDO. /* формат */

    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER acct     FOR acct.
    DEF BUFFER signs    FOR signs.

    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".
    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO
                     ).

    {wordwrap.def}
    ASSIGN
        tmp-send   = NO
        tmp-acct   = ""
        v-acct-db  = ""
        tmp-inn    = ""
        tmp-i      = 0
        tmp-name[1] = ""
        tmp-name[2] = ""
        tmp-name[3] = ""
        tmp-name[4] = ""
        tmp-name[5] = ""
        db-inn     = ""
        db-name[1] = ""
        db-name[2] = ""
        ch-ret     = ""
        buf-set     = ""
        err-op     = "".

        RELEASE acct.
        RELEASE op.
        RELEASE op-entry.
        RELEASE signs.
    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
        Vinn-send  = GetXattrValueEx("op",STRING(op.op),"inn-send","").
        Vacct-send = GetXattrValueEx("op",STRING(op.op),"acct-send","").
        Vname-send = GetXattrValueEx("op",STRING(op.op),"name-send","").
        FIND FIRST op-entry WHERE recid(op-entry) EQ rec-open NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-db EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ? NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:
            FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
            IF NOT AVAIL acct THEN err-op = "not-avail-acct".
            ELSE DO:
              {getcust.i &name=db-name &INN=db-inn &OFFinn="/*" &OFFsigns="/*"}
            END.
        END.
        
        v-acct-db = op-entry.acct-db.

        IF Vinn-send  NE "" OR
           Vacct-send NE "" OR
           Vname-send NE "" THEN
        DO:
           IF NOT foreign THEN DO:
              {exp-read.sgn "acct-send" tmp-acct}
           END.
           {exp-read.sgn "inn-send" tmp-inn}
           {exp-read.sgn "name-send" tmp-name[1]}
       
        END.
        ELSE DO:

            IF send-rec(recid(op)) EQ "send" THEN DO:
                ASSIGN
                tmp-inn = op.inn
                tmp-name[1] = op.name-ben
                tmp-acct = ( IF NOT foreign THEN op.ben-acct ELSE "").
            END.
            IF send-rec(recid(op)) NE "send" OR 
               (tmp-acct EQ "" AND (tmp-inn EQ "" OR tmp-inn EQ ?) AND 
                tmp-name[1] EQ "") THEN DO:

              buf-set = FGetSetting("БалСчИНН",?,?).

              ASSIGN
                tmp-inn = db-inn
                tmp-name[1] = db-name[1]
                tmp-name[2] = db-name[2]
                tmp-acct = "".

              IF NOT foreign THEN DO:
                 tmp-acct = v-acct-db.

                 FIND FIRST acct WHERE acct.acct EQ tmp-acct NO-LOCK NO-ERROR.

                 IF AVAIL acct AND acct.contract EQ "Касса" AND
                    {assigned op.name-ben} THEN
                    ASSIGN
                       tmp-name[1] = op.name-ben
                       tmp-inn     = "".
                 ELSE DO:
                    IF AVAIL acct AND acct.cust-cat NE "В" AND
                     NOT foreign AND NOT we-have-bic() AND
                     (buf-set EQ "" OR (buf-set NE "" AND 
                                        NOT CAN-DO(buf-set,substr(tmp-acct,1,5))
                                       )
                     ) THEN DO:
   
                      tmp-name[2] = tmp-name[2] + ( IF tmp-name[2] NE "" THEN " " 
                                                   ELSE "") + "в " +
                                     mOurName.
                      tmp-name[2] = tmp-name[2] + " " + mOurAdrr.
   
                    END.
                    ELSE IF AVAIL acct AND acct.cust-cat EQ "В" OR
                      CAN-DO(buf-set,substr(tmp-acct,1,5)) THEN DO:
                          tmp-name[1] = mOurName.
                          tmp-name[2] = mOurAdrr.
                          tmp-inn = mOurINN.
                    END.
                 END.
              END.

              ELSE DO:
                  buf-set = mBalINN.
  
                  FIND FIRST acct WHERE acct.acct EQ v-acct-db NO-LOCK NO-ERROR.
                  IF AVAIL acct AND (acct.cust-cat EQ "В" OR
                    CAN-DO(buf-set,substr(acct.acct,1,5)) ) THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
                  ELSE IF AVAIL acct THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
                 
                  ASSIGN
                   tmp-name[1] = ( IF tmp-inn NE "" AND tmp-inn NE ? 
                                   THEN ("INN" + mSpaceInn + tmp-inn + " ") 
                                  ELSE "") + 
                                 ( IF AVAIL acct AND TRIM(ch-ret) NE "" 
                                   THEN ch-ret 
                                  ELSE (tmp-name[1] + " " + tmp-name[2]))
                   tmp-name[2] = ""
                   tmp-inn = "".
              END.
            END.
        END. 

    END. 
   IF num-entries(tmp-acct, "@") > 1 then
    tmp-acct = DelFilFromAcct(tmp-acct).

    IF tmp-acct EQ ? THEN tmp-acct = "".
    ASSIGN
        tmp-name[1] = tmp-name[1] + " " + tmp-name[2] + " " +
                     ( IF tmp-name[1] NE "" AND
                         vFormat BEGINS "TELEX" THEN
                         Get106N(rec-op)
                      ELSE "")
        tmp-name[1]  = IF f-trans THEN sw-trans(TRIM(tmp-name[1]),YES,VerForm) 
                       ELSE TRIM(tmp-name[1])
        tmp-j = IF tmp-inn NE "" AND tmp-acct NE "" THEN 2
                ELSE IF tmp-inn NE "" OR tmp-acct NE "" THEN 3 
                ELSE 4
        tmp-j = IF VNotISO THEN 10 ELSE tmp-j
        .
  IF (tmp-acct NE "" AND tmp-acct NE ?) OR (tmp-inn NE "" AND tmp-inn NE ?) OR tmp-name[1] NE "" THEN DO:
    tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

    copy-op = copy-op + sym-skip + ":50:" + 
          ( IF tmp-acct NE "" AND tmp-acct NE ? THEN ("/" + tmp-acct + sym-skip) ELSE "") +
          ( IF tmp-inn NE "" AND tmp-inn NE ? THEN (( IF f-trans OR foreign THEN "INN" ELSE "ИНН") + mSpaceInn + tmp-inn + sym-skip) ELSE "").

   RUN CutTag1(tmp-j,tmp-name[1],tmp-acct,tmp-inn,sym-skip,INPUT-OUTPUT copy-op).

  END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 59 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw59:


    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM rec-open  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.


    DEF VAR sym-skip AS CHAR INITIAL "~n" NO-UNDO. 
    DEF VAR tmp-send AS LOGICAL NO-UNDO. 
    DEF VAR tmp-acct AS CHAR NO-UNDO. 
    DEF VAR tmp-inn  AS CHAR NO-UNDO. 
    DEF VAR tmp-name AS CHAR NO-UNDO  EXTENT 5. 
    DEF VAR db-inn   AS CHAR NO-UNDO. 
    DEF VAR db-name  AS CHAR NO-UNDO EXTENT 2. 
    DEF VAR tmp-i    AS INT64 NO-UNDO. 
    DEF VAR tmp-j    AS INT64 NO-UNDO. 
    DEF VAR ch-ret   AS CHAR NO-UNDO. 
    DEF VAR buf-set  AS CHAR NO-UNDO.
    DEF VAR v-acct-db AS CHAR NO-UNDO.  /* в send-rec теряется указатель на op-entry */
    DEF VAR VNotISO    AS LOGICAL NO-UNDO.
    DEF VAR VOpKind    AS CHAR NO-UNDO. /* код транзакции */
    DEF VAR VOptempl   AS CHAR NO-UNDO. /* код шаблона */
    DEF VAR VFormat    AS CHAR NO-UNDO. /* формат */

    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER acct     FOR acct.
    DEF BUFFER signs    FOR signs.

    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".
    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO
                     ).

    {wordwrap.def}
    ASSIGN
        tmp-send   = NO
        tmp-acct   = ""
        v-acct-db  = ""
        tmp-inn    = ""
        tmp-i    = 0
        tmp-name[1] = ""
        tmp-name[2] = ""
        tmp-name[3] = ""
        tmp-name[4] = ""
        tmp-name[5] = ""
        db-inn      = ""
        db-name[1]  = ""
        db-name[2]  = ""
        ch-ret      = ""
        buf-set     = ""
        err-op      = "".

    RELEASE acct.
    RELEASE op.
    RELEASE op-entry.
    RELEASE signs.

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN 
       err-op = "not-avail-op".
    ELSE DO:
        FIND FIRST op-entry WHERE recid(op-entry) EQ rec-open NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-cr EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:
            FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
            IF NOT AVAIL acct THEN err-op = "not-avail-acct".
            ELSE DO:
              {getcust.i &name=db-name &INN=db-inn &OFFinn="/*" &OFFsigns="/*"}
            END.
        END.
        
        v-acct-db = op-entry.acct-cr.
        buf-set = FGetSetting("БалСчИНН",?,buf-set).

        IF GetXattrValueEx("op",STRING(op.op),"name-rec",?) NE ? OR
           GetXattrValueEx("op",STRING(op.op),"acct-rec",?) NE ? OR
           GetXattrValueEx("op",STRING(op.op),"inn-rec",?)  NE ? THEN
        DO:
           {exp-read.sgn "acct-rec" tmp-acct}
           {exp-read.sgn "inn-rec"  tmp-inn}
           {exp-read.sgn "name-rec" tmp-name[1]}
        END.
        ELSE DO:
            IF send-rec(recid(op)) NE "send" THEN DO:
                ASSIGN  tmp-inn     = op.inn
                        tmp-name[1] = op.name-ben
                        tmp-acct    = op.ben-acct
                .

               FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
               IF AVAIL acct AND acct.contract EQ "Касса" AND
                  {assigned op.name-ben} THEN
                  ASSIGN

                     tmp-acct      = v-acct-db
                     tmp-inn       = db-inn
                     tmp-name[1]   = db-name[1]
                     tmp-name[2]   = db-name[2].

               FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
               IF AVAIL acct AND acct.contract EQ "Касса" AND
                  {assigned op.name-ben} THEN
                  ASSIGN
                     tmp-inn     = op.inn
                     tmp-name[1] = op.name-ben
                     tmp-acct    = op.ben-acct WHEN op.ben-acct NE ""
                     tmp-acct    = v-acct-db   WHEN op.ben-acct EQ ""
                  .

            END.
            IF send-rec(recid(op)) EQ "send"                                              OR 
               (tmp-acct EQ "" AND (tmp-inn EQ "" OR tmp-inn EQ ?) AND tmp-name[1] EQ "") THEN 
            DO:
               ASSIGN tmp-inn       = db-inn
                      tmp-name[1]   = db-name[1]
                      tmp-name[2]   = db-name[2]
                      tmp-acct      = v-acct-db.

               IF NOT foreign THEN 
               DO:
                  tmp-acct = v-acct-db.

                  FIND FIRST acct WHERE acct.acct EQ tmp-acct NO-LOCK NO-ERROR.
                  IF AVAIL acct AND acct.contract EQ "Касса" AND
                     {assigned op.name-ben} THEN
                     ASSIGN
                        tmp-name[1] = op.name-ben
                        tmp-inn     = op.inn.
                  ELSE DO:
                     IF AVAIL acct AND acct.cust-cat EQ "В" OR
                        CAN-DO(buf-set,substr(tmp-acct,1,5)) 
                     THEN 
                        ASSIGN tmp-name[1] = mOurName
                               tmp-name[2] = mOurAdrr
                               tmp-inn     = mOurINN
                     .
                  END.
               END.
               ELSE DO:
                  FIND FIRST acct WHERE acct.acct EQ v-acct-db NO-LOCK NO-ERROR.
                  IF AVAIL acct AND (acct.cust-cat EQ "В" OR
                    CAN-DO(buf-set,substr(acct.acct,1,5)) ) THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
                  ELSE IF AVAIL acct THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
                 
                  ASSIGN
                   tmp-name[1] = ( IF tmp-inn NE "" AND tmp-inn NE ? 
                                   THEN ("INN" + mSpaceInn + tmp-inn + " ") 
                                  ELSE "") + 
                                 ( IF AVAIL acct AND TRIM(ch-ret) NE "" 
                                   THEN ch-ret 
                                  ELSE (tmp-name[1] + " " + tmp-name[2]))
                   tmp-name[2] = ""
                   tmp-inn = "".
               END.
            END.
        END. 
    END.

    IF num-entries(tmp-acct, "@") > 1 then       
    tmp-acct = DelFilFromAcct(tmp-acct).

    IF tmp-acct EQ ? THEN tmp-acct = "".

    ASSIGN
        tmp-name[1] = tmp-name[1] + " " + tmp-name[2]
        tmp-name[1]  = IF f-trans THEN sw-trans(TRIM(tmp-name[1]),YES,VerForm) 
                       ELSE TRIM(tmp-name[1])
        tmp-j = IF tmp-inn NE "" AND tmp-acct NE "" THEN 3    
                ELSE IF tmp-inn NE "" OR tmp-acct NE "" THEN 4
                ELSE 5
        tmp-j = IF VNotISO THEN 10 ELSE tmp-j.

  IF (tmp-acct NE "" AND tmp-acct NE ?) OR (tmp-inn NE "" AND tmp-inn NE ?) OR tmp-name[1] NE "" THEN DO:
/*    {wordwrap.i &s=tmp-name &n=tmp-j &l=35}*/
    tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

    copy-op = copy-op + sym-skip + ":59:" + 
          ( IF tmp-acct NE "" THEN ("/" + tmp-acct + sym-skip) ELSE "") +
          ( IF tmp-inn NE "" AND tmp-inn NE ? THEN (( IF f-trans OR foreign THEN "INN" ELSE "ИНН") + mSpaceInn + tmp-inn + sym-skip) ELSE "").

    RUN CutTag1(tmp-j,tmp-name[1],tmp-acct,tmp-inn,sym-skip,INPUT-OUTPUT copy-op).

  END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* формирование 50 тега вариант 2                                            */
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw50-103 :

   DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
   DEF INPUT            PARAM rec-open  AS recid NO-UNDO.
   DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
   DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
   DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
   DEF INPUT            PARAM type-mes AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
   DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.
   
   DEF VAR sym-skip    AS CHAR INITIAL "~n"  NO-UNDO. 
   DEF VAR tmp-send    AS LOGICAL INITIAL NO NO-UNDO. 
   DEF VAR tmp-acct    AS CHAR INITIAL ""    NO-UNDO. 
   DEF VAR tmp-opc     AS CHAR INITIAL ""    NO-UNDO.
   DEF VAR tmp-bic     AS CHAR INITIAL ""    NO-UNDO.
   DEF VAR v-acct-db   AS CHAR INITIAL ""    NO-UNDO.  
   DEF VAR tmp-inn     AS CHAR INITIAL ""    NO-UNDO. 
   DEF VAR tmp-kpp     AS CHAR INITIAL ""    NO-UNDO.
   DEF VAR tmp-name    AS CHAR               NO-UNDO  EXTENT 5. 
   DEF VAR db-inn      AS CHAR INITIAL ""    NO-UNDO.  
   DEF VAR db-name     AS CHAR               NO-UNDO EXTENT 2. 
   DEF VAR tmp-i       AS INT64  INITIAL 0     NO-UNDO . 
   DEF VAR tmp-j       AS INT64                NO-UNDO. 
   DEF VAR ch-ret      AS CHAR INITIAL ""    NO-UNDO. 
   DEF VAR buf-set     AS CHAR INITIAL ""    NO-UNDO.
   DEF VAR VNotISO     AS LOGICAL            NO-UNDO.
   DEF VAR VOpKind     AS CHAR               NO-UNDO. /* код транзакции */
   DEF VAR VOptempl    AS CHAR               NO-UNDO. /* код шаблона    */
   DEF VAR VFormat     AS CHAR               NO-UNDO. /* формат         */

   DEF BUFFER op       FOR op.
   DEF BUFFER op-entry FOR op-entry.
   DEF BUFFER acct     FOR acct.
   DEF BUFFER signs    FOR signs.

   {wordwrap.def}

   mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".
   RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                     INPUT-OUTPUT VFormat,
                     INPUT-OUTPUT VOpKind,
                     INPUT-OUTPUT VOptempl,
                     INPUT-OUTPUT VNotISO).
   
   ASSIGN
     tmp-name[1] = ""
     tmp-name[2] = ""
     tmp-name[3] = ""
     tmp-name[4] = ""
     tmp-name[5] = ""
     db-name[1] = ""
     db-name[2] = ""
     err-op     = "".
   
   RELEASE acct.
   RELEASE op.
   RELEASE op-entry.
   RELEASE signs.
   
   FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
      IF NOT AVAIL op THEN DO:
      err-op = "not-avail-op".
      RETURN.
   END.
   /*------------*/
   /*  Тэг 50 F  */
   /*------------*/
   IF GetXattrValueEx("op",STRING(op.op),"Swift-50-opt","") EQ "F" THEN DO:
   
      DEF VAR vSw50id      AS CHAR NO-UNDO.
      DEF VAR vSw50inf     AS CHAR NO-UNDO.
      DEF VAR tmp-inf      AS CHAR NO-UNDO.
      DEF VAR vNameSend    AS CHAR NO-UNDO.
      DEF VAR vInnSend     AS CHAR NO-UNDO.
      DEF VAR vKppSend     AS CHAR NO-UNDO.
      
      DEF VAR vInnTag      AS CHAR NO-UNDO.
      DEF VAR vKppTag      AS CHAR NO-UNDO.
      
      DEF VAR vNumString   AS INT64  NO-UNDO INIT 0.
      DEF VAR vCurString   AS INT64  NO-UNDO INIT 1.
      DEF VAR vIndex       AS INT64  NO-UNDO INIT 2.
      
      DEF VAR vSw50fx      AS CHAR NO-UNDO.
      DEF VAR vSw50f8      AS CHAR NO-UNDO.
      DEF VAR vAcctTmp     AS CHAR NO-UNDO.

      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
      vAcctTmp = IF op.doc-kind EQ "send"
                    THEN op.ben-acct
                    ELSE op-entry.acct-db.

      tmp-inf  = GetXAttrValueEx("op",STRING(op.op), "acct-send", vAcctTmp).

      IF NOT {assigned tmp-inf} THEN DO:
         ASSIGN   vSw50id  = GetXAttrValueEx("op",STRING(op.op),"Swift-50f-id","")
                  vSw50inf = GetXAttrValueEx("op",STRING(op.op),"Swift-50f-inf","").
         IF NOT {assigned vSw50id}  OR
            NOT {assigned vSw50inf} THEN 
         DO:
            err-op = "not-avail-format-f". 
            RETURN.
         END.
         
         if f-trans                          and
            CAN-DO("CUST,DRLC,EMPL",vSw50id) then 
         do:
            if NUM-ENTRIES(vSw50inf,"/") ge 2    
            then 
               ENTRY(2,vSw50inf,"/") = sw-trans(ENTRY(2,vSw50inf,"/"),yes,VerForm).
            else
               vSw50inf = sw-trans(vSw50inf,yes,VerForm).
         end.
            
         tmp-inf = vSw50id + "/" + vSw50inf.
      END.
      ELSE
         tmp-inf = "/" + tmp-inf.
      
      IF LENGTH(tmp-inf) GT 35 THEN do:
         assign   tmp-name[4] =  "8/" 
                              +  if    f-trans 
                                 then  sw-trans(SUBSTRING(tmp-inf,36,33),yes,VerForm)
                                 else  SUBSTRING(tmp-inf,36,33)
                  tmp-inf     =  SUBSTRING(tmp-inf,1,35)
                  vNumString  =  vNumString + 1.
      end.

      vNameSend   = GetXAttrValueEx("op",STRING(op.op),"name-send",op.name-ben).

      if not foreign then do:
         ASSIGN
            vInnSend    = GetXAttrValueEx("op",STRING(op.op),"inn-send",op.inn)
            vKppSend    = GetXAttrValueEx("op",STRING(op.op),"kpp-send","").
                  
         IF {assigned vInnSend} or {assigned vKppSend} THEN DO:
         
            tmp-name[vCurString] = "1/".
            
            IF {assigned vInnSend} THEN DO:
               vInnTag = if   length(vInnSend) eq 5 
                         then ( IF f-trans then "KIO" else "КИО") 
                         else ( IF f-trans then "INN" else "ИНН").
               tmp-name[vCurString] = tmp-name[vCurString] + vInnTag + vInnSend.
            END.
            IF {assigned vKppSend} THEN DO:
               vInnTag = if f-trans then "KPP" else "КПП".
               tmp-name[vCurString] = tmp-name[vCurString] + "." + vInnTag + vKppSend.
            END.
            
            assign   tmp-name[vCurString] = TRIM(tmp-name[vCurString],".")
                     vNumString           = vNumString + 1
                     vCurString           = vCurString + 1.
         end.
         vNameSend   = sw-trans(vNameSend,yes,VerForm).
      end.
      
      IF GetXAttrValueEx("op",STRING(op.op),"Swift-50f-1","") EQ "Да" AND 
         {assigned vNameSend}                                         THEN 
      DO:
         IF LENGTH(vNameSend) GT 33 THEN
            ASSIGN   tmp-name[vCurString]       = "1/" + SUBSTRING(vNameSend,1,33)
                     tmp-name[vCurString + 1]   = "1/" + SUBSTRING(vNameSend,34)
                     vNumString                 = vNumString + 2
                     vCurString                 = vCurString + 2.
         ELSE 
            ASSIGN   tmp-name[vCurString]       = "1/" + vNameSend
                     vNumString                 = vNumString + 1
                     vCurString                 = vCurString + 1.
      END.
      
      DO WHILE (vNumString LT 4 AND 
                vIndex     LE 7 AND 
                vCurString LE 4)
      :
         vSw50fx = GetXAttrValueEx("op",STRING(op.op),"Swift-50f-" + string(vIndex),"").
         IF {assigned vSw50fx} THEN DO:
         
            if f-trans THEN 
            do:
               if vIndex eq 3 or 
                  vIndex eq 5 or
                  vIndex eq 6 
               then do:
                  if (num-entries(vSw50fx,"/") ge 2) then
                     entry(2,vSw50fx,"/") = sw-trans(entry(2,vSw50fx,"/"),yes,VerForm).
                  ELSE
                     vSw50fx = sw-trans(vSw50fx,yes,VerForm).
               end.
               else if vIndex eq 2 then do:
                  vSw50fx = sw-trans(vSw50fx,yes,VerForm).
               end.
            end.
         
            IF LENGTH(vSw50fx) GT 33 THEN DO:
            
               ASSIGN tmp-name[vCurString] = string(vIndex) + "/" + SUBSTRING(vSw50fx,1,33)
                      vNumString  = vNumString + 1
                      vCurString  = vCurString + 1.
               
               IF (vNumString LT 4) THEN
                  ASSIGN tmp-name[vCurString] = string(vIndex) + "/" + SUBSTRING(vSw50fx,34)
                         vNumString  = vNumString + 1
                         vCurString  = vCurString + 1.
            END.
            ELSE 
               ASSIGN   tmp-name[vCurString] = string(vIndex) + "/" + vSw50fx
                        vNumString  = vNumString + 1
                        vCurString  = vCurString + 1.
         END.
         vIndex = vIndex + 1.
      END.
      
      copy-op = copy-op                            +
                sym-skip      + ":50F:" + tmp-inf  +  
                sym-skip      + tmp-name[1]        +        
                sym-skip      + tmp-name[2]        +        
                sym-skip      + tmp-name[3]        +        
                sym-skip      + tmp-name[4].
   END.
   ELSE DO:
   /*----------------*/
   /*  Тэг 50 A,K,"" */
   /*----------------*/
     IF GetXattrValueEx("op",STRING(op.op),"BIC-SEND",?) NE ? THEN
     DO:
        ASSIGN
          tmp-bic = GetXattrValue("op",STRING(op.op),"BIC-SEND").
          tmp-opc = "A".
     END.
     ELSE tmp-opc = "K".
     tmp-kpp = GetXAttrValueEx("op",STRING(op.op),"kpp-send",?).
     RELEASE signs.


     FIND FIRST op-entry WHERE recid(op-entry) EQ rec-open NO-LOCK NO-ERROR.
     IF NOT AVAIL op-entry THEN
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
     IF op-entry.acct-db EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ?
           NO-LOCK NO-ERROR.
     IF NOT AVAIL op-entry
        THEN err-op = "not-avail-op-entry".
     ELSE DO:
        FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
        IF NOT AVAIL acct THEN
           err-op = "not-avail-acct".
        ELSE DO:
          {getcust.i &name=db-name &INN=db-inn &OFFinn="/*" &OFFsigns="/*"}
        END.
     END.
     
     v-acct-db = op-entry.acct-db.

     IF GetXAttrValueEx("op",STRING(op.op),"name-send",?) NE ? OR
        GetXAttrValueEx("op",STRING(op.op),"acct-send",?) NE ? OR
        GetXAttrValueEx("op",STRING(op.op),"inn-send",?) NE ? THEN
     DO:
         IF NOT foreign OR
            (CAN-DO("K,A",tmp-opc) AND
             CAN-DO("910,103",type-mes)
            ) THEN
         DO:
            {exp-read.sgn "acct-send" tmp-acct}
         END.
         {exp-read.sgn "inn-send" tmp-inn}
         {exp-read.sgn "name-send" tmp-name[1]}
     
     END.
     ELSE DO:

         IF send-rec(recid(op)) EQ "send" THEN
            ASSIGN
               tmp-inn = op.inn
               tmp-name[1] = op.name-ben
               tmp-acct = op.ben-acct.
         leave910:
         DO:
         IF send-rec(recid(op)) NE "send" OR 
            (tmp-acct EQ "" AND
             (tmp-inn EQ "" OR
              tmp-inn EQ ?
             ) AND 
             tmp-name[1] EQ ""
            ) THEN
         DO:
           IF type-mes EQ "910" AND
              NOT CAN-FIND(FIRST acct WHERE acct.acct EQ v-acct-db AND
                                            acct.cust-cat EQ "Ю")
           THEN LEAVE leave910.
           buf-set = FGetSetting("БалСчИНН",?,buf-set).
           
           ASSIGN
             tmp-inn     = db-inn
             tmp-name[1] = db-name[1]
             tmp-name[2] = db-name[2].

           IF NOT foreign OR
              (CAN-DO("K,A",tmp-opc) AND
               CAN-DO("910,103",type-mes)
              ) THEN tmp-acct = v-acct-db.
           IF NOT foreign THEN
           DO:

              FIND FIRST acct WHERE acct.acct EQ tmp-acct NO-LOCK NO-ERROR.
              IF AVAIL acct AND
                 acct.cust-cat NE "В" AND
                 NOT foreign AND
                 NOT we-have-bic() AND
                 (buf-set EQ "" OR
                    (buf-set NE "" AND 
                     NOT CAN-DO(buf-set,substr(tmp-acct,1,5))
                    )
                 ) THEN
              DO:

                tmp-name[2] = tmp-name[2] +
                              ( IF tmp-name[2] NE "" THEN " " 
                               ELSE "") + "в " +
                              FGetSetting("БанкС",?,"").
                tmp-name[2] = tmp-name[2] +
                              " " +
                              FGetSetting("БанкГород",?,"").

              END.
              ELSE IF AVAIL acct AND
                 acct.cust-cat EQ "В" OR
                 CAN-DO(buf-set,substr(tmp-acct,1,5)) THEN
              DO:
                 tmp-name[1] = FGetSetting("БанкС",    ?,"").
                 tmp-name[2] = FGetSetting("БанкГород",?,"").
                 tmp-inn     = FGetSetting("ИНН",      ?,"").
              END.
           END.

           ELSE DO:
               buf-set = FGetSetting("БалСчИНН",?,buf-set).

               FIND FIRST acct WHERE acct.acct EQ v-acct-db NO-LOCK NO-ERROR.
               IF AVAIL acct AND
                  (acct.cust-cat EQ "В" OR
                   CAN-DO(buf-set,substr(acct.acct,1,5))
                  ) THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
               ELSE IF AVAIL acct
                  THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
              
               ASSIGN
                vInnTag    = if length(tmp-inn) eq 5 then "KIO" else "INN"
                tmp-name[1] = ( IF tmp-inn NE "" AND
                                  tmp-inn NE ?  AND
                                  tmp-opc NE "K"
                                THEN (vInnTag + mSpaceInn + tmp-inn + " ") 
                               ELSE "") + 
                              ( IF AVAIL acct AND TRIM(ch-ret) NE "" 
                                THEN ch-ret 
                               ELSE (tmp-name[1] + " " + tmp-name[2]))
                tmp-name[2] = ""
                tmp-inn     = "".
         END.
         END.
         END. /* leave910 */
     END. 
    
    
    IF tmp-opc EQ "A" AND type-mes EQ "103" AND
       (GetXAttrValueEx("op",STRING(op.op),"name-send",?) NE ? OR
        GetXAttrValueEx("op",STRING(op.op),"acct-send",?) NE ? OR
        GetXAttrValueEx("op",STRING(op.op),"inn-send",?) NE ? OR
        GetXAttrValueEx("op",STRING(op.op),"bic-send",?) NE ? 
       ) AND
       GetXAttrValueEx("op",STRING(op.op),"acct-send",?) EQ ?
    THEN tmp-acct = "".

    IF tmp-acct EQ ? THEN tmp-acct = "".
    ASSIGN
       tmp-name[1] = tmp-name[1] + " " + tmp-name[2] + " " + ( IF tmp-name[1] NE ""      AND
                                                                 vFormat BEGINS "TELEX" THEN Get106N(rec-op)
                                                                                        ELSE "")
       tmp-name[1] = IF f-trans
                     THEN sw-trans(TRIM(tmp-name[1]),YES,VerForm) 
                     ELSE TRIM(tmp-name[1])
       tmp-j       = IF      tmp-inn  NE "" AND 
                             tmp-acct NE "" THEN 3
                     ELSE IF tmp-inn  NE "" OR  
                             tmp-acct NE "" THEN 4 
                     ELSE 4.

    IF VNotIso THEN tmp-j = 10.

    IF (tmp-acct NE "" AND
        tmp-acct NE ?
       ) OR
       (tmp-inn NE "" AND
        tmp-inn NE ?
       ) OR
       tmp-name[1] NE "" OR
       tmp-bic NE "" THEN
    DO:
    
       IF f-trans OR foreign THEN
         vInnTag = if length(tmp-inn) eq 5 then "KIO" else "INN".    
       else       
         vInnTag = if length(tmp-inn) eq 5 then "КИО" else "ИНН".    
    
       tmp-name[1] = word-wrap(INPUT tmp-name[1],
                               INPUT RIGHT-TRIM(FILL("35,",tmp-j),","),
                               INPUT YES).
    IF num-entries(tmp-acct, "@") > 1 then
    tmp-acct = DelFilFromAcct(tmp-acct).
    tmp-acct = TRIM(ENTRY(1,tmp-acct,"@")).
       copy-op =
          copy-op +
          sym-skip +
          ":50" +
          tmp-opc +
          ":" +
          ( IF tmp-acct NE "" AND
              tmp-acct NE ?
              THEN ("/" + DelFilFromAcct(tmp-acct) + sym-skip)
           ELSE ""
          ) +
          ( IF tmp-bic NE "" AND
              tmp-opc EQ "A"
              THEN (tmp-bic + sym-skip)
           ELSE ""
          ) +
          ( IF tmp-inn NE "" AND
              tmp-inn NE ? AND
              NOT CAN-DO("A", tmp-opc) THEN
              (vInnTag + mSpaceInn + tmp-inn +
               ( IF tmp-kpp NE ? AND
                  tmp-opc NE "A" AND
                  VerForm EQ "RUR6" AND
                  type-mes EQ "103"
                  THEN (
                        ( IF f-trans OR
                            foreign
                            THEN ".KPP"
                         ELSE ".КПП"
                        ) + mSpaceInn +
                        tmp-kpp
                       )
                ELSE ""
               ) + sym-skip
              )
           ELSE IF tmp-kpp NE ? AND
              tmp-opc NE "A" AND
              VerForm EQ "RUR6" AND
              type-mes EQ "103"
              THEN (
                    ( IF f-trans OR
                       foreign
                       THEN "KPP"
                     ELSE "КПП"
                    ) + mSpaceInn +
                    tmp-kpp +
                    sym-skip
                   )
           ELSE ""
          ).
  
       IF tmp-opc NE "A" THEN
          RUN CutTag1(tmp-j,tmp-name[1],tmp-acct,tmp-inn,sym-skip,INPUT-OUTPUT copy-op).
    END.
   END.
    
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 59 тега вариант 2*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw59-103:

    DEF INPUT            PARAM rec-op     AS RECID    NO-UNDO.
    DEF INPUT            PARAM foreign    AS LOGICAL  NO-UNDO. 
    DEF INPUT            PARAM f-trans    AS LOGICAL  NO-UNDO. 
    DEF INPUT            PARAM VerForm    AS CHAR     NO-UNDO. 
    DEF INPUT            PARAM type-mes   AS CHAR     NO-UNDO.
    DEF INPUT-OUTPUT     PARAM copy-op    AS CHAR     NO-UNDO. 
    DEF INPUT-OUTPUT     PARAM err-op     AS CHAR     NO-UNDO.

    DEF VAR tmp-name    AS CHAR     NO-UNDO EXTENT 5. 
    DEF VAR db-name     AS CHAR     NO-UNDO EXTENT 2. 
    DEF VAR sym-skip    AS CHAR     NO-UNDO INIT "~n" . 
    DEF VAR tmp-send    AS LOGICAL  NO-UNDO INIT NO. 
    DEF VAR tmp-acct    AS CHAR     NO-UNDO INIT "". 
    DEF VAR v-acct-db   AS CHAR     NO-UNDO INIT "".  /* в send-rec теряется указатель на op-entry */
    DEF VAR tmp-kpp     AS CHAR     NO-UNDO.
    DEF VAR tmp-inn     AS CHAR     NO-UNDO INIT "". 
    DEF VAR tmp-opc     AS CHAR     NO-UNDO INIT "".
    DEF VAR tmp-bic     AS CHAR     NO-UNDO INIT "".
    DEF VAR db-inn      AS CHAR     NO-UNDO INIT "". 
    DEF VAR tmp-i       AS INT64      NO-UNDO INIT 0. 
    DEF VAR tmp-j       AS INT64      NO-UNDO. 
    DEF VAR ch-ret      AS CHAR     NO-UNDO INIT "". 
    DEF VAR buf-set     AS CHAR     NO-UNDO INIT "".
    DEF VAR VNotISO     AS LOGICAL  NO-UNDO.
    DEF VAR VOpKind     AS CHAR     NO-UNDO. /* код транзакции    */
    DEF VAR VOptempl    AS CHAR     NO-UNDO. /* код шаблона       */
    DEF VAR VFormat     AS CHAR     NO-UNDO. /* формат            */
    DEF VAR vlSkip      AS LOG      NO-UNDO INIT NO.
    DEF VAR vlNotSigns  AS LOG      NO-UNDO INIT NO.  
    DEF VAR vXNameRec   AS CHAR     NO-UNDO.
    DEF VAR vXAcctRec   AS CHAR     NO-UNDO.
    DEF VAR vXInnRec    AS CHAR     NO-UNDO.

    DEFINE BUFFER acct      FOR acct.
    DEFINE BUFFER op        FOR op.
    DEFINE BUFFER op-entry  FOR op-entry.
    DEFINE BUFFER signs     FOR signs.

    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".

    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO).
    {wordwrap.def}

    ASSIGN
        tmp-name[1]  = ""
        tmp-name[2]  = ""
        tmp-name[3]  = ""
        tmp-name[4]  = ""
        tmp-name[5]  = ""
        db-name[1]   = ""
        db-name[2]   = ""
        err-op       = "".

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN DO:
       err-op = "NOT-AVAIL-OP".
       RETURN.
    END.

    ASSIGN  tmp-bic   = GetXattrValueEx("op",STRING(op.op),"bic-rec",   "")
            vXNameRec = GetXattrValueEx("op",STRING(op.op),"name-rec",  "")
            vXAcctRec = GetXattrValueEx("op",STRING(op.op),"acct-rec",  "")
            vXInnRec  = GetXattrValueEx("op",STRING(op.op),"inn-rec",   "")
            tmp-kpp   = GetXattrValueEx("op",STRING(op.op),"kpp-rec",   "").

    tmp-opc = IF {assigned tmp-bic} THEN "A" ELSE "".

    FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
    IF op-entry.acct-cr EQ ? THEN
       FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.

    IF NOT AVAIL op-entry THEN DO:
       err-op = "NOT-AVAIL-OP-ENTRY".
       RETURN.
    END.

    FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
    IF NOT AVAIL acct THEN 
       err-op = "NOT-AVAIL-ACCT".
    ELSE DO:
      {getcust.i &name=db-name &INN=db-inn &OFFinn="/*" &OFFsigns="/*"}
    END.
    
    v-acct-db = op-entry.acct-cr.
    buf-set   = FGetSetting("БалСчИНН",?,buf-set).

    IF {assigned vXNameRec} OR
       {assigned vXAcctRec} OR
       {assigned vXInnRec}  THEN
    DO:
       ASSIGN tmp-acct    = vXAcctRec
              tmp-inn     = vXInnRec
              tmp-name[1] = vXNameRec.
    END.
    ELSE DO:
        IF send-rec(recid(op)) EQ "rec" THEN 
           ASSIGN   tmp-inn     = op.inn
                    tmp-name[1] = op.name-ben
                    tmp-acct    = op.ben-acct
           .

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("e-sw59-103 1)","tmp-acct: "    + string(tmp-acct,   'X(25)') + " " + 
                                         "tmp-inn: "     + string(tmp-inn,    'X(25)') + " " + 
                                         "tmp-name[1]: " + string(tmp-name[1],'X(25)')).
         &ENDIF

         vlNotSigns = NOT {assigned tmp-acct}    AND 
                      NOT {assigned tmp-inn}     AND 
                      NOT {assigned tmp-name[1]}.  

        IF send-rec(recid(op)) NE "rec" OR vlNotSigns THEN 
        DO:
          ASSIGN tmp-inn       = db-inn
                 tmp-name[1]   = db-name[1]
                 tmp-name[2]   = db-name[2]
                 tmp-acct      = v-acct-db
          .
          IF NOT foreign THEN DO:
             tmp-acct = v-acct-db.
             FOR FIRST acct WHERE acct.acct EQ tmp-acct NO-LOCK:
                IF acct.cust-cat EQ "В" OR
                   CAN-DO(buf-set,substr(tmp-acct,1,5)) 
                THEN ASSIGN tmp-name[1]  = mOurName
                            tmp-name[2]  = mOurAdrr
                            tmp-inn      = mOurINN.
             END.
          END.
          ELSE DO:
              FOR FIRST acct WHERE acct.acct EQ v-acct-db NO-LOCK:
                 RUN get-engl-name (INPUT recid(acct), 
                                    INPUT acct.cust-cat EQ "В" OR CAN-DO(buf-set,substr(acct.acct,1,5)), 
                                    OUTPUT ch-ret).
              END.
             
              ASSIGN
               tmp-name[1] = ( IF tmp-inn NE "" AND tmp-inn NE ? THEN ("INN" + mSpaceInn + tmp-inn + " ") ELSE "") + 
                             ( IF AVAIL acct AND TRIM(ch-ret) NE "" THEN ch-ret ELSE (tmp-name[1] + " " + tmp-name[2]))
               tmp-name[2] = ""
               tmp-inn = "".
          END.
        END.
        IF num-entries(tmp-acct, "@") > 1 then
        tmp-acct = DelFilFromAcct(tmp-acct).
    END.  

    /* это важно !!!! */  
    IF tmp-acct      EQ ? THEN tmp-acct      = "".
    IF tmp-inn       EQ ? THEN tmp-inn       = "".
    IF tmp-name[1]   EQ ? THEN tmp-name[1]   = "".
    IF tmp-bic       EQ ? THEN tmp-bic       = "".

    ASSIGN
       tmp-name[1] = tmp-name[1] + " " + tmp-name[2]
       tmp-name[1] = IF f-trans THEN sw-trans(INPUT TRIM(tmp-name[1]),
                                              INPUT YES,
                                              INPUT VerForm) 
                                ELSE TRIM(tmp-name[1])
       tmp-j       = IF       VNotISO        THEN 10
                     ELSE IF  tmp-inn  NE "" AND 
                              tmp-acct NE "" THEN 3    
                     ELSE IF  tmp-inn  NE "" OR 
                              tmp-acct NE "" THEN 4
                     ELSE 5.                                       

   IF tmp-acct     NE ""   OR
      tmp-inn      NE ""   OR
      tmp-name[1]  NE ""   OR
      tmp-bic      NE ""     
   THEN DO:
      tmp-name[1] = word-wrap(tmp-name[1],RIGHT-TRIM(FILL("35,",tmp-j),","),YES).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("e-sw59-103 2)","tmp-acct: "     + string(tmp-acct)).
      RUN dbgprint.p ("e-sw59-103 3)","v-acct-db: "    + string(v-acct-db)).
      RUN dbgprint.p ("e-sw59-103 4)","vlNotSigns: "   + string(vlNotSigns)).
      RUN dbgprint.p ("e-sw59-103 5)","tmp-bic: "      + string(tmp-bic)).
      &ENDIF
                                       /* ФОРМИРОВАНИЯ СТРОКИ ТЭГА */
      copy-op = copy-op + sym-skip + ":59" + tmp-opc + ":".
                                       /* ACCT                     */
      IF {assigned tmp-acct} AND
         NOT ({assigned v-acct-db}  AND 
              vlNotSigns            AND 
              {assigned tmp-bic})
      THEN
         copy-op = copy-op + "/" + tmp-acct + sym-skip.
                                       /* BIC                      */
      IF tmp-bic NE "" AND
         tmp-opc EQ "A"
      THEN   
         copy-op = copy-op + tmp-bic + sym-skip.
                                       /* INN                      */
      IF {assigned tmp-inn} AND
         tmp-opc NE "A"
      THEN
         ASSIGN copy-op = copy-op + ( IF f-trans OR foreign THEN "INN" ELSE "ИНН") + mSpaceInn + tmp-inn
                vlSkip  = YES.
                                       /* KPP                      */
      IF {assigned tmp-kpp} AND
         VerForm EQ "RUR6"  AND
         tmp-opc NE "A"     AND
         type-mes EQ "103"  
      THEN 
         ASSIGN copy-op = copy-op + ( IF {assigned tmp-inn} then "." else "")
                copy-op = copy-op + ( IF f-trans OR foreign THEN "KPP" ELSE "КПП") + mSpaceInn + tmp-kpp
                vlSkip  = YES.
 
      IF vlSkip THEN
         copy-op = copy-op + sym-skip.

                                       /* ОБРЕЗАНИЕ СТРОК          */
      IF tmp-opc NE "A" THEN
         RUN CutTag1(tmp-j,tmp-name[1],tmp-acct,tmp-inn,sym-skip,INPUT-OUTPUT copy-op).

   END.

END PROCEDURE.  
/*---------------------------------------------------------------------------*/
/*возвращает ангийское наименование и адресс клиента по счету*/
/*---------------------------------------------------------------------------*/
PROCEDURE get-engl-name.
    DEF INPUT  PARAM rec-acct AS recid.
    DEF INPUT  PARAM f-in-acct AS LOGICAL.
    DEF OUTPUT PARAM ch-ret AS CHAR NO-UNDO.
    DEF VAR table-name AS CHAR INIT "" NO-UNDO.

    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER acct     FOR acct.
    DEF BUFFER banks-code    FOR banks-code.
    
    FIND FIRST acct WHERE recid(acct) EQ rec-acct NO-LOCK NO-ERROR.
    IF AVAIL acct AND NOT f-in-acct THEN DO:
       CASE acct.cust-cat:
        WHEN "Ч" THEN table-name = "person".
        WHEN "Ю" THEN table-name = "cust-corp".
        WHEN "Б" THEN table-name = "banks".
       END CASE.
    
       ch-ret =
          GetXattrValueEx(table-name,
                          string(acct.cust-id),
                          ( IF acct.cust-cat EQ "Б" THEN "swift-name"
                           ELSE "engl-name"
                          ),
                          ch-ret).   
    
       IF GetXattrValueEx(table-name,
                          string(acct.cust-id),
                          ( IF acct.cust-cat EQ "Б" THEN "swift-address"
                           ELSE "engl-address"
                          ),?
                          ) NE ? THEN
       ch-ret = TRIM(ch-ret + "~n" + 
                     GetXattrValue(table-name,
                                   string(acct.cust-id),
                                   ( IF acct.cust-cat EQ "Б" THEN "swift-address"
                                    ELSE "engl-address"
                                   )
                                  )
                    ).
    END. 
    ELSE IF AVAIL acct THEN DO:

       FIND FIRST banks-code WHERE banks-code.bank-code EQ bank-mfo-9 AND
                  banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
       IF AVAIL banks-code THEN
       DO:
          ch-ret = 
          GetXattrValueEx("banks",
                          string(banks-code.bank-id),
                          "swift-name",
                          ch-ret).
          IF GetXattrValueEx("banks",
                             string(banks-code.bank-id),
                             "swift-address",
                             ?
                            ) NE ? THEN
          ch-ret = TRIM(ch-ret + "~n" + 
                        GetXattrValue("banks",
                                      string(banks-code.bank-id),
                                      "swift-address"
                                     )
                       ).
       END.
    END. 
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 52 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw52:

    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
    DEF INPUT            PARAM TypeMes AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF VAR var-frm AS CHAR NO-UNDO. /* вариант формата */
    DEF VAR sym-skip AS CHAR INITIAL "~n" NO-UNDO. 
    DEF VAR tmp-send AS LOGICAL NO-UNDO. 
    DEF VAR tmp-bic  AS CHAR NO-UNDO. 
    DEF VAR tmp-type-bank  AS CHAR NO-UNDO. 
    DEF VAR tmp-acct AS CHAR NO-UNDO. 
    DEF VAR tmp-inn AS CHAR NO-UNDO. 
    DEF VAR tmp-acct2 AS CHAR NO-UNDO. 
    DEF VAR tmp-name AS CHAR NO-UNDO  EXTENT 5. 
    DEF VAR tmp-i    AS INT64 NO-UNDO. 
    DEF VAR tmp-j    AS INT64 NO-UNDO. 
    DEF VAR ch-ret   AS CHAR NO-UNDO. 
    DEF VAR set1   AS CHAR NO-UNDO. 
    DEF VAR set2   AS CHAR NO-UNDO. 
    DEF VAR vRecOpBank AS recid NO-UNDO.
    DEF VAR vTmpStrCode AS CHAR NO-UNDO.
    DEF VAR vAcctDb AS CHARACTER   NO-UNDO INIT ?.

    DEF VAR vNotISO       AS LOG   NO-UNDO.
    DEF VAR vOpKind       AS CHAR  NO-UNDO. /* код транзакции */
    DEF VAR vOptempl      AS CHAR  NO-UNDO. /* код шаблона */
    DEF VAR vFormat       AS CHAR  NO-UNDO. /* формат */ 

    DEFINE VARIABLE vInn AS CHARACTER   NO-UNDO.

    DEF BUFFER op         FOR op.
    DEF BUFFER op-entry   FOR op-entry.
    DEF BUFFER acct       FOR acct.
    DEF BUFFER signs      FOR signs.      
    DEF BUFFER banks      FOR banks.      
    DEF BUFFER banks-code FOR banks-code. 
    DEF BUFFER banks-corr FOR banks-corr. 
    DEF BUFFER op-bank    FOR op-bank. 

    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO
                     ).
    {wordwrap.def}
    ASSIGN
     var-frm  = ""
     tmp-send = NO
     tmp-bic  = ""
     tmp-type-bank = ""
     tmp-acct = ""
     tmp-inn = ""
     set1 = ""
     set2 = ""
     tmp-acct2 = ""
     tmp-name[1] = ""
     tmp-name[2] = ""
     tmp-name[3] = ""
     tmp-name[4] = ""
     tmp-name[5] = ""
     tmp-i    = 0
     tmp-j    = 0
     ch-ret   = "".

    RELEASE acct.
    RELEASE op.
    RELEASE op-entry.
    RELEASE signs.
    RELEASE banks.
    RELEASE banks-code.
    RELEASE banks-corr.
    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-db EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ? NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:

            set1 = FGetSetting("НазнСчМБР",?,"").
            set2 = mBalINN.

            vRecOpBank = get-op-bank(recid(op),"send").
            FIND FIRST op-bank WHERE recid(op-bank) EQ vRecOpBank NO-LOCK NO-ERROR.
            IF AVAIL op-bank OR
               (TypeMes EQ "910" AND
                vFormat EQ "SWIFT" AND
                CAN-FIND(FIRST acct WHERE
                               acct.acct EQ op-entry.acct-db
                           AND acct.cust-cat EQ "В")
              ) THEN
            DO:

                tmp-inn = "".
                IF TypeMes EQ "202" THEN
                   tmp-inn =
                   GetXattrValueEx("op-bank",
                                   string(op-bank.op) + ',' + op-bank.op-bank-type,
                                   "inn",
                                   tmp-inn).
                ASSIGN
                  tmp-type-bank = IF AVAIL op-bank THEN op-bank.bank-code-type
                                  ELSE "МФО-9"
                  tmp-bic       = IF AVAIL op-bank THEN
                                     GetBankCodeFormat
                                        (op-bank.bank-code-type,
                                         op-bank.bank-code)
                                  ELSE bank-mfo-9
                  tmp-acct      = IF AVAIL op-bank THEN op-bank.corr-acct
                                  ELSE ""
                  tmp-name[1]   = IF AVAIL op-bank THEN op-bank.bank-name
                                  ELSE ""
                  var-frm       = IF tmp-type-bank EQ "BIC" AND
                                     tmp-bic NE "" THEN "A"
                                  ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                  ELSE "D"
                .
                IF AVAIL op-bank THEN
                   FIND FIRST banks-code WHERE
                      banks-code.bank-code-type EQ op-bank.bank-code-type AND
                      banks-code.bank-code      EQ op-bank.bank-code
                   NO-LOCK NO-ERROR.
                ELSE 
                   FIND FIRST banks-code WHERE
                      banks-code.bank-code-type EQ "МФО-9" AND
                      banks-code.bank-code      EQ bank-mfo-9
                   NO-LOCK NO-ERROR.
                IF AVAIL banks-code THEN DO:
                    FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.
                    FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                       IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                       THEN DO: tmp-acct2 = banks-corr.corr-acct. LEAVE. END.
                    END.

                    IF foreign AND tmp-type-bank EQ "МФО-9" THEN DO:
                      FIND FIRST banks-code OF banks WHERE 
                       banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

                      ASSIGN
                        tmp-type-bank = IF AVAIL banks-code 
                                         THEN banks-code.bank-code-type
                                        ELSE "BIC"
                        tmp-bic       = IF AVAIL banks-code AND tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                         substr(banks-code.bank-code,9) EQ 
                                         FILL("X", length(banks-code.bank-code) - 8) 
                                        THEN substr(banks-code.bank-code,1,8) 
                                        ELSE IF AVAIL banks-code
                                        THEN banks-code.bank-code
                                        ELSE ""
                        tmp-acct      = ""
                        tmp-name[1]   = IF tmp-name[1] EQ "" AND AVAIL banks 
                                         THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                        ELSE tmp-name[1]
                        var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                        ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                        ELSE "D"
                      .
                    END.
                    ELSE DO:

                      ASSIGN
                        tmp-type-bank = IF AVAIL banks-code 
                                         THEN banks-code.bank-code-type
                                        ELSE "BIC"
                        tmp-bic       = IF AVAIL banks-code AND tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                         substr(banks-code.bank-code,9) EQ 
                                         FILL("X", length(banks-code.bank-code) - 8) 
                                        THEN substr(banks-code.bank-code,1,8) 
                                        ELSE IF AVAIL banks-code
                                        THEN banks-code.bank-code
                                        ELSE ""
                        tmp-acct      = IF tmp-acct EQ "" 
                                         THEN tmp-acct2
                                        ELSE tmp-acct
                        tmp-name[1]   = IF tmp-name[1] EQ "" AND AVAIL banks 
                                         THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                        ELSE tmp-name[1]
                        var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                        ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                        ELSE "D"
                      .

                    END.
                END.

            END.
            ELSE DO:
                IF TypeMes EQ "103" OR 
                   TypeMes EQ "100" 
                THEN
                  vAcctDb = GetXattrValueEx("op",string(op.op),"acct-db",?).

                IF NOT {assigned vAcctDb} THEN
                   vAcctDb = op-entry.acct-db.

                FIND FIRST acct WHERE acct.acct EQ vAcctDb NO-LOCK NO-ERROR.
                IF AVAIL acct AND acct.cust-cat EQ "Б" AND CAN-DO(set1,acct.contract) AND 
                 ((NOT CAN-DO(set2,substr(acct.acct,1,5)) AND (TypeMes EQ "202" OR TypeMes EQ "910"))
                  OR
                  (TypeMes NE "202" AND 
                   GetXattrValueEx("op",string(op.op),"name-send",?) NE ? OR
                   GetXattrValueEx("op",string(op.op),"acct-send",?) NE ? OR
                   GetXattrValueEx("op",string(op.op),"inn-send",?) NE ? OR
                   (op.doc-kind EQ "send" AND (op.ben-acct NE "" OR TRIM(op.name-ben) NE "" OR (op.inn NE ? AND op.inn NE "")))
                  ) 
                 ) THEN 
                DO:
                    FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                    IF AVAIL banks THEN DO:
                        vInn = GetBankInn ("bank-id", STRING (banks.bank-id)).
                        IF {assigned vInn} AND TypeMes EQ "202" THEN tmp-inn = vInn.

                        IF NOT foreign THEN
                        FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                           IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                           THEN tmp-acct2 = banks-corr.corr-acct.
                        END.

                        FIND FIRST banks-code OF banks WHERE 
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code OR foreign OR op-entry.curr NE "" THEN 
                        FIND FIRST banks-code OF banks WHERE 
                           banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

                        IF foreign THEN 
                        RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
                        ASSIGN
                          tmp-type-bank = IF AVAIL banks-code THEN banks-code.bank-code-type ELSE tmp-type-bank
                          tmp-bic       = IF AVAIL banks-code AND 
                                           tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                           substr(banks-code.bank-code,9) EQ 
                                           FILL("X", length(banks-code.bank-code) - 8) 
                                          THEN substr(banks-code.bank-code,1,8) 
                                          ELSE IF AVAIL banks-code THEN banks-code.bank-code
                                          ELSE tmp-bic
                          tmp-acct      = tmp-acct2
                          tmp-name[1]   = ch-ret
                          tmp-name[1]   = IF (tmp-name[1] EQ "" AND foreign) OR NOT foreign 
                                          THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                          ELSE tmp-name[1]
                          var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                          ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                          ELSE "D"
                        .
                    END.
                END.
                ELSE IF AVAIL acct AND
                 ((TypeMes EQ "202" AND
                   CAN-DO(set1,acct.contract)
                  ) OR
                  (TypeMes EQ "910" AND
                   vFormat EQ "SWIFT"
                  )
                 ) AND 
                 (acct.cust-cat EQ "В" OR CAN-DO(set2,substr(acct.acct,1,5))) AND
                 ((op.doc-kind EQ "rec" AND
                   GetXattrValueEx("op",string(op.op),"name-send",?) EQ ? AND
                   GetXattrValueEx("op",string(op.op),"acct-send",?) EQ ? AND
                   GetXattrValueEx("op",string(op.op),"inn-send",?) EQ ?
                  ) OR
                  (op.doc-kind EQ "send" AND (op.ben-acct EQ "" AND TRIM(op.name-ben) EQ "" AND (op.inn EQ ? OR op.inn EQ "")))
                 ) 
                 THEN DO:

                    FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                    IF AVAIL banks AND
                       acct.cust-cat EQ "Б" AND
                       TypeMes EQ "202" THEN
                    DO:
                        vInn = GetBankInn ("bank-id", STRING (banks.bank-id)).
                        IF {assigned vInn} AND TypeMes EQ "202" THEN tmp-inn = vInn.

                        IF NOT foreign THEN
                        FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                           IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                           THEN tmp-acct2 = banks-corr.corr-acct.
                        END.

                        FIND FIRST banks-code OF banks WHERE 
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code OR foreign OR op-entry.curr NE "" THEN 
                        FIND FIRST banks-code OF banks WHERE 
                           banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

                        IF foreign THEN 
                        RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
                        ASSIGN
                          tmp-type-bank = IF AVAIL banks-code THEN banks-code.bank-code-type ELSE tmp-type-bank
                          tmp-bic       = IF AVAIL banks-code AND 
                                           tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                           substr(banks-code.bank-code,9) EQ 
                                           FILL("X", length(banks-code.bank-code) - 8) 
                                          THEN substr(banks-code.bank-code,1,8) 
                                          ELSE IF AVAIL banks-code THEN banks-code.bank-code
                                          ELSE tmp-bic
                          tmp-acct      = tmp-acct2
                          tmp-name[1]   = ch-ret
                          tmp-name[1]   = IF (tmp-name[1] EQ "" AND foreign) OR NOT foreign 
                                          THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                          ELSE tmp-name[1]
                          var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                          ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                          ELSE "D"
                        .
                    END.
                    ELSE DO:

                        IF foreign THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
                        tmp-name[1] = mOurName.
                        tmp-name[2] = mOurAdrr.
                        tmp-inn = IF TypeMes EQ "202" THEN mOurINN ELSE "".

                        ASSIGN
                          tmp-type-bank = IF foreign THEN "BIC" ELSE "МФО-9"
                          tmp-bic       = IF foreign THEN bank-bic ELSE bank-mfo-9
                          tmp-acct      = IF foreign THEN "" ELSE bank-acct
                          tmp-name[1]   = IF foreign THEN ch-ret ELSE (tmp-name[1] + "~n" + tmp-name[2])
                          tmp-name[2]   = ""
                          var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                          ELSE IF tmp-type-bank EQ "BIC" THEN "D2"
                                          ELSE "D" 
                        .

                    END.
                END.

            END.
        END.

    END.
    IF num-entries(tmp-acct, "@") > 1 then
    tmp-acct = DelFilFromAcct(tmp-acct).
    IF tmp-acct EQ ? THEN tmp-acct = "".
    IF (tmp-acct NE "" AND tmp-acct NE ?) OR tmp-bic NE "" OR tmp-name[1] NE "" THEN DO:

        tmp-name[1] = IF f-trans THEN sw-trans(tmp-name[1],YES,VerForm) ELSE tmp-name[1].
        tmp-j = IF var-frm EQ "B" AND tmp-acct NE "" AND tmp-acct NE ? THEN 2 
                ELSE IF var-frm EQ "B" AND tmp-acct EQ "" THEN 3
                ELSE IF tmp-acct NE "" THEN 4
                ELSE 5
         .
        tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

        vTmpStrCode = sw-val-beg("SWIFT_CLI",tmp-type-bank).
        copy-op = copy-op + sym-skip + ":52" + substr(var-frm,1,1) + ":" +
           ( IF (var-frm EQ "D2" OR
                var-frm EQ "A")      AND
               tmp-acct NE ""        AND
               tmp-acct NE ?
               THEN ("/" + tmp-acct + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "A" AND
               tmp-bic NE ""
               THEN (tmp-bic + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "D" AND
               tmp-bic NE ""
               THEN ("//" +
                     ( IF vTmpStrCode EQ "MF" AND
                         CAN-DO("RUR5,RUR6",VerForm)
                         THEN "RU"
                         ELSE vTmpStrCode
                     ) +
                     tmp-bic +
                     ( IF vTmpStrCode NE "MF" AND
                         vTmpStrCode NE "RU"
                         THEN sym-skip
                         ELSE ""
                     )
                    )
               ELSE ""
           ) +
           ( IF var-frm EQ "D" AND
               tmp-acct NE ""
               THEN (( IF vTmpStrCode EQ "MF" OR
                         vTmpStrCode EQ "RU"
                         THEN "."
                         ELSE ""
                     ) + tmp-acct + sym-skip
                    )
               ELSE ""
           ) +
           ( IF var-frm BEGINS "D" AND
               tmp-inn NE "" AND
               tmp-inn NE ?
               THEN (( IF tmp-acct EQ "" AND
                         (vTmpStrCode EQ "MF" OR
                          vTmpStrCode EQ "RU"
                         )
                         THEN sym-skip
                         ELSE ""
                     ) +
                     ( IF f-trans OR
                         foreign
                         THEN "INN"
                         ELSE "ИНН"
                     ) + mSpaceInn + tmp-inn + sym-skip
                    )
                    ELSE ""
           )
        .
        IF var-frm NE "A" THEN 
           RUN CutTag2(tmp-j,tmp-name[1],tmp-acct,tmp-bic,sym-skip,INPUT-OUTPUT copy-op).

    END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 57 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw57:
    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
    DEF INPUT            PARAM TypeMes AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF BUFFER xacct FOR acct. 
    DEF BUFFER xbanks-code FOR banks-code. 
    DEF BUFFER bBanksCode  FOR banks-code. 
    DEF BUFFER bBanks      FOR banks. 

    DEF VAR var-frm       AS CHAR  NO-UNDO. /* вариант формата */
    DEF VAR sym-skip      AS CHAR  NO-UNDO INIT "~n".
    DEF VAR tmp-send      AS LOG   NO-UNDO.
    DEF VAR tmp-bic       AS CHAR  NO-UNDO.
    DEF VAR tmp-type-bank AS CHAR  NO-UNDO. 
    DEF VAR tmp-acct      AS CHAR  NO-UNDO. 
    DEF VAR tmp-acct2     AS CHAR  NO-UNDO. 
    DEF VAR tmp-inn       AS CHAR  NO-UNDO.
    DEF VAR tmp-name      AS CHAR  NO-UNDO  EXTENT 5.
    DEF VAR tmp-i         AS INT64   NO-UNDO.
    DEF VAR tmp-j         AS INT64   NO-UNDO.
    DEF VAR ch-ret        AS CHAR  NO-UNDO.
    DEF VAR set1          AS CHAR  NO-UNDO.
    DEF VAR set2          AS CHAR  NO-UNDO.
    DEF VAR vRecOpBank    AS recid NO-UNDO.
    DEF VAR vBicCode      AS CHAR  NO-UNDO.
    DEF VAR VTmpStrCode   AS CHAR  NO-UNDO.
    
    DEF VAR vUpdBRecHdr   AS LOG   NO-UNDO. /*включить механизм замены шапки*/
    DEF VAR vNotISO       AS LOG   NO-UNDO.
    DEF VAR vOpKind       AS CHAR  NO-UNDO. /* код транзакции */
    DEF VAR vOptempl      AS CHAR  NO-UNDO. /* код шаблона */
    DEF VAR vFormat       AS CHAR  NO-UNDO. /* формат */

    DEFINE VARIABLE vInn AS CHARACTER   NO-UNDO.

    DEF BUFFER op         FOR op.
    DEF BUFFER op-entry   FOR op-entry.
    DEF BUFFER acct       FOR acct.
    DEF BUFFER signs      FOR signs.      
    DEF BUFFER op-bank    FOR op-bank.      
    DEF BUFFER banks      FOR banks.      
    DEF BUFFER banks-code FOR banks-code. 
    DEF BUFFER banks-corr FOR banks-corr. 

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57","INIT TypeMes=" + TypeMes).
&ENDIF

    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO
                     ).

    {wordwrap.def}
    ASSIGN
     var-frm  = ""
     tmp-send = NO
     tmp-bic  = ""
     tmp-type-bank = ""
     tmp-acct = ""
     tmp-acct2 = ""
     tmp-name[1] = ""
     tmp-name[2] = ""
     tmp-name[3] = ""
     tmp-name[4] = ""
     tmp-name[5] = ""
     tmp-i    = 0
     tmp-j    = 0
     set1    = ""
     set2    = ""
     tmp-inn = ""
     ch-ret  = "".

    RELEASE acct.
    RELEASE xacct.
    RELEASE op.
    RELEASE op-entry.
    RELEASE signs.
    RELEASE banks.
    RELEASE banks-code.
    RELEASE xbanks-code.
    RELEASE banks-corr.

    set1 = FGetSetting("НазнСчМБР",?,"").
    set2 = mBalINN.
    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-cr EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:
            vRecOpBank = get-op-bank(recid(op),"rec").
            FIND FIRST op-bank WHERE recid(op-bank) EQ vRecOpBank NO-LOCK NO-ERROR.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57"," AVAIL op-bank:" + STRING(AVAIL op-bank)).
&ENDIF
            IF NOT AVAIL op-bank AND TypeMes NE "202" AND
             ((op.doc-kind EQ "send" AND
               GetXattrValueEx("op",string(op.op),"name-rec",?) EQ ? AND
               GetXattrValueEx("op",string(op.op),"acct-rec",?) EQ ? AND
               GetXattrValueEx("op",string(op.op),"inn-rec",?) EQ ?
              ) OR
              (op.doc-kind EQ "rec" AND 
               (op.ben-acct EQ "" AND TRIM(op.name-ben) EQ "" AND 
                (op.inn EQ ? OR op.inn EQ "")
               )
              )
             ) 
            THEN DO:
               vRecOpBank = get-op-bank(recid(op),"ben").
               FIND FIRST op-bank WHERE recid(op-bank) EQ vRecOpBank NO-LOCK NO-ERROR.
            END.

            IF AVAIL op-bank THEN DO:

                tmp-inn = "".
                IF TypeMes EQ "202" THEN
                tmp-inn = GetXattrValueEx("op-bank",
                                          string(op-bank.op) + ',' + op-bank.op-bank-type,
                                          "inn",tmp-inn).
                FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57"," op-entry.acct-cr:" + acct.acct).
&ENDIF

                IF TypeMes NE "200" THEN DO:
                   IF op-bank.corr-acct NE "" OR op-bank.corr-acct NE ? AND TypeMes NE "200" THEN DO:
                       FIND FIRST xacct WHERE xacct.acct EQ op-bank.corr-acct NO-LOCK NO-ERROR.
                       IF AVAIL xacct AND xacct.cust-id EQ acct.cust-id 
                       THEN DO:
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57"," op-entry.acct-cr:" + GetNullStr(acct.acct)    +
                         " xacct.acct:"       + GetNullStr(xacct.acct)   +
                         " acct.cust-id:"     + GetNullInt(acct.cust-id) +
                         " xacct.cust-id:"    + GetNullInt(acct.cust-id)).
&ENDIF
                          RETURN.
                       END.
                   END.
                   IF op-bank.bank-code NE "" OR op-bank.bank-code NE ? AND TypeMes NE "200" THEN DO:                      
                       FIND FIRST xbanks-code WHERE xbanks-code.bank-code      = op-bank.bank-code AND
                                                    xbanks-code.bank-code-type = op-bank.bank-code-type 
                          NO-LOCK NO-ERROR.
                       IF AVAIL xbanks-code AND xbanks-code.bank-id EQ acct.cust-id 
                       THEN DO:
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57"," acct.acct:" + GetNullStr(acct.acct) +
                      "~n acct.cust-id:" + GetNullInt(acct.cust-id) + "~n" +
"set1" + set1  + "~n" +
"acct.contract" + acct.contract).

&ENDIF                    
                          IF NOT (acct.cust-cat EQ "Б" AND acct.cust-id EQ 1052 ) THEN 
                          IF NOT CAN-DO (set1, acct.contract) OR
                            (GetXAttrValueEx ("acct", acct.acct + "," + acct.currency, "acct-def", "")    NE "Да" AND
                             GetXAttrValueEx ("acct", acct.acct + "," + acct.currency, "acct-def-cr", "") NE "Да") THEN
                             RETURN.
                       END.
                   END.
                END.

                ASSIGN
                  tmp-type-bank = op-bank.bank-code-type
                  tmp-bic       = IF tmp-type-bank EQ "BIC" AND length(op-bank.bank-code) > 8 AND 
                                    substr(op-bank.bank-code,9) EQ 
                                    FILL("X", length(op-bank.bank-code) - 8) 
                                  THEN substr(op-bank.bank-code,1,8) 
                                  ELSE op-bank.bank-code
                  tmp-acct      = op-bank.corr-acct
                  tmp-name[1]   = op-bank.bank-name
                  var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                  ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                  ELSE "D"
                .
                FIND FIRST banks-code WHERE banks-code.bank-code-type EQ op-bank.bank-code-type AND
                                            banks-code.bank-code EQ op-bank.bank-code NO-LOCK NO-ERROR.
                IF AVAIL banks-code THEN DO:
                    FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.

                    IF foreign THEN DO:
                      
                      RUN get-engl-bank (banks-code.bank-id, OUTPUT ch-ret).
                      IF tmp-type-bank EQ "МФО-9" THEN
                        FIND FIRST banks-code OF banks WHERE 
                         banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

                    END.
                    ELSE DO:

                      IF banks-code.bank-code-type EQ "МФО-9" THEN
                      FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                         IF substr(banks-corr.corr-acct,6,3) EQ substr(op-entry.acct-cr,6,3)
                         THEN DO: tmp-acct2 = banks-corr.corr-acct. LEAVE. END.
                      END.
                      IF tmp-type-bank EQ "BIC" AND op-entry.curr EQ "" THEN DO:
                        FIND FIRST banks-code OF banks WHERE
                         banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF AVAIL banks-code AND banks-code.bank-code-type EQ "МФО-9" THEN
                        FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                           IF substr(banks-corr.corr-acct,6,3) EQ substr(op-entry.acct-cr,6,3)
                           THEN DO: tmp-acct2 = banks-corr.corr-acct. LEAVE. END.
                        END.
                      END.
                    END.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57"," AVAIL banks-code:" + STRING(AVAIL banks-code)).
&ENDIF

                    IF NOT AVAIL banks-code THEN
                       FIND FIRST banks-code WHERE banks-code.bank-code-type EQ op-bank.bank-code-type AND
                                                   banks-code.bank-code      EQ op-bank.bank-code 
                           NO-LOCK NO-ERROR.

                    ASSIGN
                        tmp-type-bank = IF AVAIL banks-code 
                                        THEN banks-code.bank-code-type
                                        ELSE "BIC"
                        tmp-bic       = IF AVAIL banks-code AND tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                           substr(banks-code.bank-code,9) EQ 
                                           FILL("X", length(banks-code.bank-code) - 8) 
                                         THEN substr(banks-code.bank-code,1,8) 
                                        ELSE IF AVAIL banks-code 
                                         THEN banks-code.bank-code
                                        ELSE ""
                        tmp-acct      =  IF NOT foreign AND tmp-acct EQ "" THEN tmp-acct2
                                         ELSE tmp-acct
                        tmp-name[1]   = IF foreign AND ch-ret NE "" THEN ch-ret
                                        ELSE IF tmp-name[1] EQ "" AND AVAIL banks 
                                         THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                        ELSE tmp-name[1]
                        var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                    ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                    ELSE "D"
                      .
                END.
            END.
            ELSE IF TypeMes EQ "200" THEN DO:

                FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
                IF AVAIL acct AND acct.cust-cat EQ "Б" AND 
                   NOT CAN-DO(set2,substr(acct.acct,1,5)) THEN DO:

                    FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                    IF AVAIL banks THEN DO:
                        vInn = GetBankInn ("bank-id", STRING (banks.bank-id)).
                        IF {assigned vInn} AND TypeMes EQ "202" THEN tmp-inn = vInn.

                        IF NOT foreign THEN
                        FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                           IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                           THEN tmp-acct2 = banks-corr.corr-acct.
                        END.
                        IF AVAIL acct AND
                          (acct.cust-cat EQ "В" OR
                           CAN-DO(set2,substr(acct.acct,1,5)) )
                         THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
                        ELSE IF AVAIL acct THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).

                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code OR foreign OR op-entry.curr NE "" THEN
                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code THEN
                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.

                        ASSIGN
                          tmp-type-bank = IF AVAIL banks-code THEN banks-code.bank-code-type ELSE "BIC"
                          tmp-bic       = IF AVAIL banks-code AND
                                           tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND
                                           substr(banks-code.bank-code,9) EQ
                                           FILL("X", length(banks-code.bank-code) - 8)
                                          THEN substr(banks-code.bank-code,1,8)
                                          ELSE IF AVAIL banks-code THEN banks-code.bank-code
                                          ELSE ""
                          tmp-acct      = acct.acct
                          tmp-name[1]   = ch-ret
                          tmp-name[1]   = IF (tmp-name[1] EQ "" AND foreign) OR NOT foreign
                                          THEN ({banknm.lf banks} + " " + {bankct.lf banks})
                                          ELSE ""
                          var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                          ELSE IF tmp-type-bank EQ "BIC" THEN "D2"
                                          ELSE "D"
                        .
                    END.

                END.

                IF AVAIL acct AND
                 tmp-acct EQ "" AND (tmp-inn EQ "" OR tmp-inn EQ ?) AND tmp-name[1] EQ "" THEN DO:


                        IF foreign THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
                        tmp-name[1] = mOurName.
                        tmp-name[2] = mOurAdrr.
                        tmp-inn = IF TypeMes EQ "202" THEN mOurINN ELSE "".

                        ASSIGN
                          tmp-type-bank = IF foreign THEN "BIC" ELSE "МФО-9"
                          tmp-bic       = ""
                          tmp-acct      = acct.acct
                          tmp-name[1]   = IF foreign THEN ch-ret ELSE (tmp-name[1] + "~n" + tmp-name[2])
                          tmp-name[2]   = ""
                          var-frm       = "D2" 
                        .
                END.

            END.
        END.

    END.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57","1tmp-acct:"     + GetNullStr(tmp-acct) +
                         " tmp-bic:"      + GetNullStr(tmp-bic)  +
                         " tmp-name[1]:"  + GetNullStr(tmp-name[1])).
&ENDIF
  IF num-entries(tmp-acct, "@") > 1 then
  tmp-acct = DelFilFromAcct(tmp-acct).
  IF tmp-acct EQ ? THEN 
     tmp-acct = "".

  IF tmp-acct     NE ""  OR 
     tmp-bic      NE ""  OR 
     tmp-name[1]  NE ""  OR 
     tmp-inn      NE ""  THEN 
  DO:
        ASSIGN
           set1         = FGetSetting("SWIFT","BicRecIsHdr","")
           set2         = FGetSetting("SWIFT","BicRecNoHdr","")
           vBicCode     = FGetSetting("SWIFT","HeadChg"    ,"")
           vUpdBRecHdr  = NO.
           
        IF VFormat       EQ "swift"    AND
           CAN-DO("100,103", TypeMes)  AND
           op-entry.curr EQ ""         AND 
           set1          NE ""         AND
           set2          NE ""         THEN
        DO:
           FIND FIRST bBanksCode 
                WHERE bBanksCode.bank-code      EQ tmp-bic 
                  AND bBanksCode.bank-code-type EQ tmp-type-bank 
                      NO-LOCK NO-ERROR.
           IF AVAIL bBanksCode THEN 
           DO:
              FIND FIRST bBanks OF banks-code NO-LOCK NO-ERROR.
              IF AVAIL   bBanks THEN 
              DO:
                 FIND FIRST bBanksCode WHERE
                            bBanksCode.bank-id        EQ bBanks.bank-id
                        AND bBanksCode.bank-code-type EQ "REGN" NO-LOCK NO-ERROR.
                 IF AVAIL   bBanksCode   AND 
                    CAN-DO(vBicCode,bBanksCode.bank-code)
                 THEN
                    ASSIGN OVERLAY(copy-op,INDEX(copy-op,"~{2:I1") + 7, 12) = set1.
                 ELSE 
                    ASSIGN OVERLAY(copy-op,INDEX(copy-op,"~{2:I1") + 7, 12) = set2 
                           vUpdBRecHdr                                      = YES.
              END.
           END. 
        END.
        
        IF vUpdBRecHdr            AND
           NOT var-frm BEGINS "D" THEN
        DO:
           var-frm = "D".
           IF AVAIL banks THEN
              FIND FIRST banks-code OF banks WHERE
                 banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
              IF AVAIL banks-code THEN
                 ASSIGN
                    tmp-bic       = banks-code.bank-code
                    tmp-type-bank = "МФО-9"
                    tmp-name[1]   = {banknm.lf banks} + " " + {bankct.lf banks}.
        END.
        
        tmp-name[1] = IF f-trans THEN sw-trans(tmp-name[1],YES,VerForm) ELSE tmp-name[1].
        tmp-j = IF var-frm EQ "B" AND tmp-acct NE "" THEN 2 
                ELSE IF var-frm EQ "B" AND tmp-acct EQ "" THEN 3
                ELSE IF tmp-acct NE "" THEN 4
                ELSE 5
         .
        tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-sw57","2tmp-acct:"     + GetNullStr(tmp-acct) +
                         " tmp-bic:"      + GetNullStr(tmp-bic)  +
                         " tmp-name[1]:"  + GetNullStr(tmp-name[1])).
&ENDIF

        vTmpStrCode = sw-val-beg("SWIFT_CLI",tmp-type-bank).
        copy-op = copy-op + sym-skip + ":57" + substr(var-frm,1,1) + ":" +
           ( IF ((var-frm EQ "A" AND
                 CAN-DO("100,103,200,202", TypeMes)
                ) OR
                var-frm EQ "D2"
               ) AND
               tmp-acct NE "" 
               THEN ("/" + tmp-acct + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "A" AND
               tmp-bic NE "" 
               THEN (tmp-bic + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "D" AND
               tmp-bic NE "" 
               THEN ("//" +
                     ( IF vTmpStrCode EQ "MF" AND 
                         CAN-DO("RUR5,RUR6",VerForm)
                         THEN "RU" 
                         ELSE vTmpStrCode
                     ) + 
                     tmp-bic +
                     ( IF vTmpStrCode NE "MF" AND 
                         vTmpStrCode NE "RU" 
                         THEN sym-skip
                          ELSE ""
                     )
                    )
                ELSE ""
           ) + 
           ( IF var-frm EQ "D" AND
               tmp-acct NE "" 
               THEN (( IF vTmpStrCode EQ "MF" OR
                         vTmpStrCode EQ "RU"
                         THEN "."
                         ELSE ""
                     ) +
                     tmp-acct +
                     sym-skip
                    )
               ELSE ""
           ) +
           ( IF var-frm BEGINS "D" AND
               tmp-inn NE "" AND
               tmp-inn NE ?
               THEN (( IF tmp-acct EQ "" AND
                         CAN-DO("MF,RU",vTmpStrCode)
                         THEN sym-skip
                         ELSE ""
                      ) +
                      ( IF f-trans OR
                          foreign
                          THEN "INN"
                          ELSE "ИНН"
                      ) + mSpaceInn + tmp-inn + sym-skip
                     )
               ELSE ""
           )
        .
        IF var-frm NE "A" THEN 
           RUN CutTag2(tmp-j,tmp-name[1],tmp-acct,tmp-bic,sym-skip,INPUT-OUTPUT copy-op).

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("e-sw57","3tmp-acct:"     + GetNullStr(tmp-acct) +
                                  " tmp-bic:"      + GetNullStr(tmp-bic)  +
                                  " tmp-name[1]:"  + GetNullStr(tmp-name[1])).
         &ENDIF
    END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 58 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw58:
    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO. 
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF BUFFER xacct FOR acct. 
    DEF BUFFER xbanks-code FOR banks-code. 

    DEF VAR var-frm AS CHAR NO-UNDO. /* вариант формата */
    DEF VAR vFormat AS CHAR NO-UNDO. /*  формат */
    DEF VAR sym-skip AS CHAR INITIAL "~n" NO-UNDO. 
    DEF VAR tmp-send AS LOGICAL NO-UNDO. 
    DEF VAR tmp-bic  AS CHAR NO-UNDO. 
    DEF VAR tmp-type-bank  AS CHAR NO-UNDO. 
    DEF VAR tmp-acct AS CHAR NO-UNDO. 
    DEF VAR tmp-inn AS CHAR NO-UNDO. 
    DEF VAR tmp-acct2 AS CHAR NO-UNDO. 
    DEF VAR set1 AS CHAR NO-UNDO.
    DEF VAR set2 AS CHAR NO-UNDO.
    DEF VAR tmp-name AS CHAR NO-UNDO  EXTENT 5. 
    DEF VAR tmp-i    AS INT64 NO-UNDO. 
    DEF VAR tmp-j    AS INT64 NO-UNDO. 
    DEF VAR ch-ret   AS CHAR NO-UNDO. 
    DEF VAR vRecOpBank AS recid NO-UNDO.
    DEF VAR vTmpStrCode AS CHAR NO-UNDO.

    DEFINE VARIABLE vInn AS CHARACTER   NO-UNDO.
    
    DEF BUFFER op         FOR op.
    DEF BUFFER op-entry   FOR op-entry.
    DEF BUFFER acct       FOR acct.
    DEF BUFFER signs      FOR signs.      
    DEF BUFFER banks      FOR banks.      
    DEF BUFFER banks-code FOR banks-code. 
    DEF BUFFER banks-corr FOR banks-corr. 
    DEF BUFFER op-bank    FOR op-bank. 

    {wordwrap.def}
    ASSIGN
     var-frm  = ""
     tmp-send = NO
     tmp-bic  = ""
     tmp-type-bank = ""
     tmp-acct = ""
     tmp-inn = ""
     tmp-acct2 = ""
     set1 = ""
     set2 = ""
     tmp-name[1] = ""
     tmp-name[2] = ""
     tmp-name[3] = ""
     tmp-name[4] = ""
     tmp-name[5] = ""
     tmp-i    = 0
     tmp-j    = 0
     ch-ret   = ""
     vFormat  = IF INDEX(VerForm,",") > 0 THEN
                   ENTRY(2,VerForm)
                ELSE ""
     VerForm  = ENTRY(1,VerForm)
   .

    RELEASE acct.
    RELEASE xacct.
    RELEASE op.
    RELEASE op-entry.
    RELEASE signs.
    RELEASE banks.
    RELEASE banks-code.
    RELEASE xbanks-code.
    RELEASE banks-corr.

    set1 = FGetSetting("НазнСчМБР",?,"").
    set2 = mBalINN.
    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF op-entry.acct-cr EQ ? THEN
        FIND FIRST op-entry OF op WHERE op-entry.acct-cr NE ? NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:
            vRecOpBank = get-op-bank(recid(op),"ben").
            FIND FIRST op-bank WHERE recid(op-bank) EQ vRecOpBank NO-LOCK NO-ERROR.
            IF AVAIL op-bank THEN DO:

                tmp-inn = GetXattrValueEx("op-bank",
                                          string(op-bank.op) + ',' + op-bank.op-bank-type,
                                          "inn","").

                ASSIGN
                  tmp-type-bank = op-bank.bank-code-type
                  tmp-bic       = IF tmp-type-bank EQ "BIC" AND length(op-bank.bank-code) > 8 AND 
                                    substr(op-bank.bank-code,9) EQ 
                                    FILL("X", length(op-bank.bank-code) - 8) 
                                  THEN substr(op-bank.bank-code,1,8) 
                                  ELSE op-bank.bank-code
                  tmp-acct      = op-bank.corr-acct
                  tmp-name[1]   = op-bank.bank-name
                  var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                  ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                  ELSE "D"
                .
                FIND FIRST banks-code WHERE banks-code.bank-code-type EQ op-bank.bank-code-type AND
                                            banks-code.bank-code EQ op-bank.bank-code NO-LOCK NO-ERROR.
                IF AVAIL banks-code THEN DO:
                    FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.
                    IF NOT foreign THEN
                    FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                       IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                       THEN DO: tmp-acct2 = banks-corr.corr-acct. LEAVE. END.
                    END.

                    IF foreign AND tmp-type-bank EQ "МФО-9" THEN 
                     FIND FIRST banks-code OF banks WHERE 
                      banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

                    ASSIGN
                      tmp-type-bank = IF AVAIL banks-code 
                                      THEN banks-code.bank-code-type
                                      ELSE "BIC"
                      tmp-bic       = IF AVAIL banks-code AND tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND 
                                         substr(banks-code.bank-code,9) EQ 
                                         FILL("X", length(banks-code.bank-code) - 8) 
                                       THEN substr(banks-code.bank-code,1,8) 
                                      ELSE IF AVAIL banks-code 
                                       THEN banks-code.bank-code
                                      ELSE ""
                      tmp-acct      = IF tmp-acct EQ "" THEN tmp-acct2
                                      ELSE tmp-acct
                      tmp-name[1]   = IF tmp-name[1] EQ "" AND AVAIL banks 
                                       THEN ({banknm.lf banks} + " " + {bankct.lf banks}) 
                                      ELSE tmp-name[1]
                      var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                  ELSE IF tmp-type-bank EQ "BIC" THEN "D2" 
                                  ELSE "D"
                    .
                END.

            END.
            ELSE IF CliDbEqCliCr(RECID(op-entry),"Б") AND
               vFormat EQ "SWIFT" THEN
            DO:
               FIND FIRST acct WHERE
                  acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
               IF AVAIL acct THEN
               DO:
                  FIND FIRST banks WHERE
                     banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                  IF AVAIL banks THEN
                  DO:
                     FOR EACH banks-corr WHERE
                        banks-corr.bank-corr EQ banks.bank-id
                     NO-LOCK:
                        IF comp-curr(substr(banks-corr.corr-acct,6,3),
                                     op-entry.curr) THEN
                           tmp-acct2 = banks-corr.corr-acct.
                     END.
                     FIND FIRST banks-code OF banks WHERE
                        banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.
                     IF NOT AVAIL banks-code THEN
                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                  END.
                  ASSIGN
                     tmp-type-bank = IF AVAIL banks-code THEN
                                        banks-code.bank-code-type
                                     ELSE "BIC"
                     tmp-bic       = IF AVAIL banks-code THEN
                                        GetBankCodeFormat
                                           (banks-code.bank-code-type,
                                            banks-code.bank-code)
                                     ELSE ""
                     tmp-acct      = GetCNostroAcct(acct.acct, acct.curr)
                     var-frm       = IF tmp-type-bank EQ "BIC" AND
                                        tmp-bic NE "" THEN
                                        "A"
                                     ELSE IF tmp-type-bank EQ "BIC" OR
                                        tmp-acct NE "" THEN
                                        "D2"
                                     ELSE "D"
                     tmp-name[1]   = IF AVAIL banks THEN
                                        ({banknm.lf banks} + " " +
                                         {bankct.lf banks})
                                     ELSE ""
                     tmp-acct      = IF tmp-acct  EQ ""  AND
                                        var-frm   EQ "D" AND
                                        tmp-acct2 NE "" THEN
                                        tmp-acct2
                                     ELSE tmp-acct
                   .
               END.
            END.
            ELSE IF
             ((op.doc-kind EQ "send" AND
               GetXattrValueEx("op",string(op.op),"name-rec",?) EQ ? AND
               GetXattrValueEx("op",string(op.op),"acct-rec",?) EQ ? AND
               GetXattrValueEx("op",string(op.op),"inn-rec",?) EQ ?
              ) OR
              (op.doc-kind EQ "rec" AND
               (op.ben-acct EQ "" AND TRIM(op.name-ben) EQ "" AND
                (op.inn EQ ? OR op.inn EQ "")
               )
              )
             )
            THEN DO:

                FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
                IF AVAIL acct AND acct.cust-cat EQ "Б" AND 
                   NOT CAN-DO(set2,substr(acct.acct,1,5)) THEN DO:

                    FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                    IF AVAIL banks THEN DO:
                        vInn = GetBankInn ("bank-id", STRING(banks.bank-id)).
                        IF {assigned vInn} THEN tmp-inn = vInn.

                        IF NOT foreign THEN
                        FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
                           IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
                           THEN tmp-acct2 = banks-corr.corr-acct.
                        END.
                        IF AVAIL acct AND
                          (acct.cust-cat EQ "В" OR
                           CAN-DO(set2,substr(acct.acct,1,5)) )
                         THEN RUN get-engl-name (recid(acct), YES, OUTPUT ch-ret).
                        ELSE IF AVAIL acct THEN RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).

                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code OR foreign THEN
                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.
                        IF NOT AVAIL banks-code THEN
                        FIND FIRST banks-code OF banks WHERE
                           banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.

                        ASSIGN
                          tmp-type-bank = IF AVAIL banks-code THEN banks-code.bank-code-type ELSE "BIC"
                          tmp-bic       = IF AVAIL banks-code AND
                                           tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND
                                           substr(banks-code.bank-code,9) EQ
                                           FILL("X", length(banks-code.bank-code) - 8)
                                          THEN substr(banks-code.bank-code,1,8)
                                          ELSE IF AVAIL banks-code THEN banks-code.bank-code
                                          ELSE ""
                          tmp-acct      = acct.acct
                          tmp-name[1]   = ch-ret
                          tmp-name[1]   = IF (tmp-name[1] EQ "" AND foreign) OR NOT foreign
                                          THEN ({banknm.lf banks} + " " + {bankct.lf banks})
                                          ELSE ""
                          var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                                          ELSE IF tmp-type-bank EQ "BIC" THEN "D2"
                                          ELSE "D"
                        .
                    END.

                END.

            END.
        END.

    END.

    IF num-entries(tmp-acct, "@") > 1 then
    tmp-acct = DelFilFromAcct(tmp-acct).
    IF tmp-acct NE "" OR tmp-bic NE "" OR tmp-name[1] NE "" THEN DO:

        tmp-name[1] = IF f-trans THEN sw-trans(tmp-name[1],YES,VerForm) ELSE tmp-name[1].
        tmp-j = IF var-frm EQ "B" AND tmp-acct NE "" THEN 2 
                ELSE IF var-frm EQ "B" AND tmp-acct EQ "" THEN 3
                ELSE IF tmp-acct NE "" THEN 4
                ELSE 5
         .
        tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

        vTmpStrCode = sw-val-beg("SWIFT_CLI",tmp-type-bank).
        copy-op = copy-op + sym-skip + ":58" + substr(var-frm,1,1) + ":" +
           ( IF (var-frm EQ "A" OR
                var-frm EQ "D2"
               ) AND
               tmp-acct NE "" 
               THEN ("/" + tmp-acct + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "A" AND
               tmp-bic NE "" 
               THEN (tmp-bic + sym-skip)
               ELSE ""
           ) +
           ( IF var-frm EQ "D" AND
               tmp-bic NE "" 
               THEN ("//" + ( IF vTmpStrCode EQ "MF" AND 
                                CAN-DO("RUR5,RUR6",VerForm)
                                THEN "RU" 
                             ELSE vTmpStrCode
                            ) + 
                     tmp-bic + ( IF vTmpStrCode NE "MF" AND 
                                   vTmpStrCode NE "RU" 
                                   THEN sym-skip
                                   ELSE ""
                               )
                    )
               ELSE ""
           ) + 
           ( IF var-frm EQ "D" AND
               tmp-acct NE "" 
               THEN (( IF vTmpStrCode EQ "MF" OR
                         vTmpStrCode EQ "RU"
                         THEN "."
                         ELSE ""
                     ) + tmp-acct + sym-skip
                    )
               ELSE ""
           ) +
           ( IF var-frm BEGINS "D" AND
               tmp-inn NE "" AND
               tmp-inn NE ?
               THEN (( IF tmp-acct EQ "" AND
                         (vTmpStrCode EQ "MF" OR
                          vTmpStrCode EQ "RU"
                         )
                         THEN sym-skip
                         ELSE ""
                     ) + ( IF f-trans OR foreign THEN "INN" ELSE "ИНН") +
                     mSpaceInn + tmp-inn + sym-skip
                    )
               ELSE ""
           )
         .
 
       IF var-frm NE "A" THEN 
          RUN CutTag2(tmp-j,tmp-name[1],tmp-acct,tmp-bic,sym-skip,INPUT-OUTPUT copy-op).
    END.


END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*возвращает банковский счет "по умолчанию" по маске подразделения для банков без бика*/
/*---------------------------------------------------------------------------*/
FUNCTION pos-acct-no-bic RETURN CHAR (INPUT in-acct AS CHAR, INPUT in-sr AS CHAR):

   DEF BUFFER acct       FOR acct.
   DEF BUFFER branch     FOR branch.
   DEF BUFFER signs      FOR signs.

&IF DEFINED (IS-DEBUG) &THEN
RUN dbgprint.p ("pos-acct-no-bic","in-acct:" + in-acct). 
&ENDIF

  DEF VAR vAcct53 AS CHAR NO-UNDO.
  IF length(in-acct) NE 20 THEN RETURN in-acct.

  RUN BankNoBic(in-acct, BUFFER branch).
  IF AVAIL branch THEN
  DO:
     FIND FIRST acct WHERE acct.cust-cat EQ "Б" AND
                acct.cust-id = branch.bank-id AND
                substr(acct.acct,6,3) = substr(in-acct,6,3) AND
                acct.close-date = ? AND
                acct.filial-id  EQ ShFilial                       AND
        CAN-FIND(FIRST signs WHERE
          signs.file = "acct" AND
          signs.surrogate EQ acct.acct + "," + acct.currency AND
          signs.code = ("acct-def-" + in-sr) AND signs.code-val = "Да") NO-LOCK NO-ERROR.
     IF NOT AVAIL acct THEN 
     FIND FIRST acct WHERE acct.cust-cat EQ "Б" AND
                acct.cust-id = branch.bank-id AND 
                substr(acct.acct,6,3) = substr(in-acct,6,3) AND 
                acct.close-date = ? AND
                acct.filial-id  EQ ShFilial                       AND
        CAN-FIND(FIRST signs WHERE 
          signs.file = "acct" AND 
          signs.surrogate EQ acct.acct + "," + acct.currency AND
          signs.code = "acct-def" AND signs.xattr-val = "Да") NO-LOCK NO-ERROR.

&IF DEFINED (IS-DEBUG) &THEN
RUN dbgprint.p ("pos-acct-no-bic","in-sr:"         + in-sr +
                                  " acct.acct:"    + acct.acct +
                                  " acct.cust-id:" + STRING(acct.cust-id)).
&ENDIF
    IF AVAIL acct THEN in-acct = acct.acct.                              

    IF in-sr EQ "cr"              AND
       AVAIL acct                 AND
       (acct.acct BEGINS "30301"   OR
        acct.acct BEGINS "30302") 
    THEN  DO:
       vAcct53 = GetSysConf("AcctTag53").

&IF DEFINED (IS-DEBUG) &THEN
RUN dbgprint.p ("pos-acct-no-bic","vAcct53:"  + vAcct53).
&ENDIF
       IF  vAcct53 BEGINS "30305" OR
           vAcct53 BEGINS "30306"
       THEN DO:

&IF DEFINED (IS-DEBUG) &THEN
RUN dbgprint.p ("pos-acct-no-bic","НазнСч30305:"  + FGetSetting("SWIFT","НазнСч30305","")).
&ENDIF
          RUN GetAcctById 
              ("Б",acct.cust-id,acct.currency,FGetSetting("SWIFT","НазнСч30305",""), "acct-def", BUFFER acct).
          IF AVAIL acct THEN
             in-acct = acct.acct.
       END.
    END.
    ELSE IF AVAIL acct THEN 
       in-acct = acct.acct.
  END.
  RETURN in-acct.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*проверка опердня на открытие,неоткрытие,блокирование и выходной*/
/*---------------------------------------------------------------------------*/
PROCEDURE op-vdate-ext :

   DEF INPUT-OUTPUT PARAM in-vdate     AS DATE    NO-UNDO.
   DEF INPUT        PARAM iOpKind      AS RECID   NO-UNDO.
   DEF INPUT        PARAM no-fnd       AS LOGICAL NO-UNDO. 
   DEF INPUT        PARAM no-open      AS LOGICAL NO-UNDO. 
   DEF INPUT-OUTPUT PARAM out-err      AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM op-ch-reason AS CHAR    NO-UNDO.

   DEFINE VARIABLE OK AS LOGICAL    NO-UNDO.
   DEF VAR err-class AS CHAR INITIAL "mess-error" NO-UNDO.

   DEFINE BUFFER op-date  FOR op-date.
   DEFINE BUFFER bop-date FOR op-date.
   DEFINE BUFFER op-kind  FOR op-kind.

   FIND FIRST op-date WHERE op-date EQ in-vdate NO-LOCK NO-ERROR.
   
   IF NOT AVAIL op-date AND no-fnd THEN 
   DO:
      REPEAT:
         IF NOT {holiday.i in-vdate} THEN DO:
            {additem.i "out-err" "err-class + ':swift-32A-08'"}
            {additem.i "op-ch-reason" "'  - Операционный день ' + STRING(in-vdate) + ' не доступен. Поиск доступного.'"}
            LEAVE.
         END.
         IF NOT CAN-FIND(FIRST bop-date WHERE bop-date.op-date > in-vdate) AND no-open THEN DO:
            in-vdate = ?.
            LEAVE.
         END.
         in-vdate = in-vdate + 1.
      END.
      RETURN.
   END.

   FIND FIRST op-kind WHERE RECID(op-kind) = iOpKind
      NO-LOCK NO-ERROR.
   {gdateimp.i  
       &v-op-date        = in-vdate
       &no-chk-defopdate = YES
       &add-error        = YES
       &op-error         = out-err
       &error-code-cls   = "err-class + ':swift-32A-07'"
       &error-code-blk   = "err-class + ':swift-32A-09'"
       &no-errmessage    = YES 
       &op-ch-reason     = op-ch-reason
   }
   IF NOT OK THEN 
      in-vdate = ?.
   ELSE
   DO:
      {rel-date.i &in-op-date = in-vdate}
   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE op-vdate :

   DEF INPUT-OUTPUT PARAM in-vdate     AS DATE    NO-UNDO.
   DEF INPUT        PARAM iOpKind      AS RECID   NO-UNDO.
   DEF INPUT        PARAM no-fnd       AS LOGICAL NO-UNDO. 
   DEF INPUT-OUTPUT PARAM out-err      AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM op-ch-reason AS CHAR    NO-UNDO.

   RUN op-vdate-ext(INPUT-OUTPUT in-vdate,
                    INPUT iOpKind,
                    INPUT no-fnd, 
                    INPUT YES, 
                    INPUT-OUTPUT out-err,
                    INPUT-OUTPUT op-ch-reason).

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*проверка опердня на открытие,неоткрытие,блокирование и выходной для выписок*/
/*---------------------------------------------------------------------------*/
PROCEDURE op-vdate950:

  DEF INPUT-OUTPUT PARAM in-vdate  AS DATE    NO-UNDO.
  DEF INPUT        PARAM iOpKind   AS RECID   NO-UNDO.
  DEF INPUT        PARAM no-fnd    AS LOGICAL NO-UNDO. 
  DEF INPUT-OUTPUT PARAM out-err   AS CHAR    NO-UNDO.

  DEF VAR err-class AS CHAR INITIAL "mess-error" NO-UNDO.

  DEFINE VARIABLE OK AS LOGICAL    NO-UNDO.
  DEFINE BUFFER lop-date FOR op-date. /* TSL - для daylock.i*/ 
  DEFINE BUFFER lop-date-cat-lock FOR op-date-cat-lock.
  DEFINE VARIABLE mCats  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vIcats AS INT64    NO-UNDO.

  DEFINE BUFFER op-kind FOR op-kind.

  FIND FIRST op-kind WHERE RECID(op-kind) = iOpKind
     NO-LOCK NO-ERROR.
  {gdateimp.i  
     &v-op-date         = in-vdate
     &no-chk-defopdate  = YES
     &add-error         = YES
     &op-error          = out-err
     &error-code-cls    = "err-class + ':swift-32A-07'"
     &error-code-blk    = "err-class + ':swift-32A-09'"
     &no-chk-prevopdate = YES
     &no-errmessage     = YES 
  }
  IF NOT OK THEN 
     in-vdate = ?.
  ELSE
  DO:
     {rel-date.i &in-op-date = in-vdate}
  END.


END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование технологии платежа для 72 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw5-tp:
    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF BUFFER op         FOR op.
    DEF BUFFER op-entry   FOR op-entry.
    DEF BUFFER setting    FOR setting.      

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
        FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
        IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
        ELSE DO:
          FIND FIRST setting WHERE 
                     setting.code EQ "ТехПлат" AND 
                     setting.sub-code NE "" AND
                     CAN-DO(setting.val,op-entry.type) NO-LOCK NO-ERROR.
          IF AVAIL setting AND setting.code NE "" THEN
          copy-op = copy-op + "." + 
           ( IF      setting.sub-code EQ "ТППочта" THEN "POST"
            ELSE IF setting.sub-code EQ "ТПТелеграф" THEN "TELG" 
            ELSE IF setting.sub-code EQ "ТПЭлектронный" THEN "ELEK"
            ELSE setting.sub-code).
        END.
    END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* связанный референс для 21 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-rel-ref:

    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO. 
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF BUFFER op         FOR op.

    DEF VAR tmp-str AS CHAR NO-UNDO.

    FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN err-op = "not-avail-op".
    ELSE DO:
      {exp-read.sgn "Rel-Ref" tmp-str}
      copy-op = copy-op + "~n" + ":21:" + 
        IF TRIM(tmp-str) NE "" THEN tmp-str ELSE "NONREF".
    END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*при импорте для сохранения информации в формате из файла по 35 символов с добавлением пробелов*/
/*---------------------------------------------------------------------------*/
FUNCTION save-reflect RETURN CHAR (INPUT InStr AS CHAR,INPUT InStrSR AS CHAR,INPUT InNum AS INT64):
DEF VAR TmpStr AS CHAR NO-UNDO.
DEF VAR TmpCount AS INT64 NO-UNDO.
  DO TmpCount = 1 TO NUM-ENTRIES(InStr,chr(1)):

   TmpStr = TmpStr +
     IF TRIM(ENTRY(TmpCount,InStr,chr(1))) EQ "" THEN ""
     ELSE IF length(ENTRY(TmpCount,InStr,chr(1))) < InNum THEN
        (ENTRY(TmpCount,InStr,chr(1)) + 
         ( IF InStrSR EQ "Да" THEN
             FILL(" ", InNum - length(ENTRY(TmpCount,InStr,chr(1))))
          ELSE " "
         )
        )
     ELSE ENTRY(TmpCount,InStr,chr(1)).
  END.
  IF InStrSR EQ "Да" THEN RETURN TmpStr.
  ELSE RETURN TRIM(TmpStr).
END FUNCTION.
/*---------------------------------------------------------------------------*/
/* английское наименование и адресс банка*/
/*---------------------------------------------------------------------------*/
PROCEDURE get-engl-bank.
    DEF INPUT  PARAM in-id-bank LIKE banks.bank-id.
    DEF OUTPUT PARAM ch-ret AS CHAR NO-UNDO.

    DEF BUFFER banks FOR banks.
    
    ch-ret = "".
    ch-ret = GetXattrValueEx("banks",string(in-id-bank),"swift-name",ch-ret).
    
    IF GetXattrValueEx("banks",string(in-id-bank),"swift-address",?) NE ? THEN
    ch-ret = TRIM(ch-ret + "~n" +
                  GetXattrValue("banks",string(in-id-bank),"swift-address")
                 ).   
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* выделяет счет из наименования банка выступающего как клиент*/
/*---------------------------------------------------------------------------*/
PROCEDURE get-afromn:

DEF INPUT-OUTPUT  PARAM io-name AS CHAR NO-UNDO.
DEF INPUT-OUTPUT  PARAM io-wacct AS CHAR NO-UNDO.
DEF INPUT-OUTPUT  PARAM io-acct AS CHAR NO-UNDO.

DEF VAR TmpCount AS INT64 NO-UNDO.
DEF VAR TmpCountAcct AS INT64 NO-UNDO.
DEF VAR TmpStr AS CHAR NO-UNDO.
DEF VAR vFlagSymb1 AS LOG NO-UNDO.

    DEF BUFFER op         FOR op.
    DEF BUFFER op-entry   FOR op-entry.
    DEF BUFFER acct       FOR acct.
    DEF BUFFER c-nostro   FOR c-nostro.

FIND FIRST acct WHERE acct.acct EQ io-acct NO-LOCK NO-ERROR.

IF NOT AVAIL acct THEN DO:
   FIND FIRST c-nostro WHERE c-nostro.corr-acct EQ io-acct AND
     CAN-FIND(FIRST acct WHERE acct.acct EQ c-nostro.acct AND
                               acct.curr EQ c-nostro.curr AND
                               acct.close-date EQ ?) NO-LOCK NO-ERROR.
  IF AVAIL c-nostro THEN FIND FIRST acct OF c-nostro NO-LOCK NO-ERROR.
END.

IF AVAIL acct AND
   CAN-DO(FGetSetting("НазнСчМБР",?,?),acct.contract) AND
   we-have-bic() THEN
DO TmpCount = length(Io-name) TO 1 BY -1:

    IF substr(Io-name,TmpCount,1) >= "0" AND
       substr(Io-name,TmpCount,1) <= "9" THEN
       TmpCountAcct = TmpCountAcct + 1.
    ELSE IF asc(substr(Io-name,TmpCount,1)) EQ 1 AND
       TmpCountAcct NE 0 THEN
       ASSIGN
          vFlagSymb1   = YES
          TmpCountAcct = TmpCountAcct + 1.
    ELSE ASSIGN
       vFlagSymb1   = NO
       TmpCountAcct = 0.

    IF (TmpCountAcct EQ 20 AND vFlagSymb1 EQ NO) OR
       (TmpCountAcct EQ 21 AND vFlagSymb1 EQ YES) THEN
    DO:
      ASSIGN
       Io-acct = REPLACE(substr(Io-name,TmpCount ,TmpCountAcct),CHR(1),"")
       Io-wacct = Io-acct
       Io-name = TRIM(substr(Io-name, 1, TmpCount - 1)).

      DO TmpCount = 1 TO NUM-ENTRIES(Io-name," "):
         IF TmpCount NE NUM-ENTRIES(Io-name," ") THEN
         TmpStr = TmpStr + ( IF TmpStr EQ "" THEN "" ELSE " ") + ENTRY(TmpCount, Io-name, " ").
      END.
      Io-name = TmpStr.
      LEAVE.
    END.

END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* обработка счета из 25 тега*/
/*---------------------------------------------------------------------------*/
PROCEDURE i-sw25:

DEF INPUT-OUTPUT PARAM in-acct AS CHAR NO-UNDO.
DEF INPUT PARAM in-curr AS CHAR NO-UNDO.
DEF INPUT PARAM AcBnkSend AS LOGICAL NO-UNDO.
DEF INPUT PARAM bic-headr AS CHAR NO-UNDO.

    DEF BUFFER currency   FOR currency.
    DEF BUFFER acct       FOR acct.
    DEF BUFFER c-nostro   FOR c-nostro.
    DEF BUFFER banks-code FOR banks-code.

   FIND FIRST currency
        WHERE currency.i-currency EQ in-curr NO-LOCK NO-ERROR.
   IF NOT AVAIL currency THEN
   FIND FIRST currency
        WHERE currency.currency EQ in-curr NO-LOCK NO-ERROR.
   IF NOT AVAIL currency THEN in-curr = "".
   ELSE in-curr = currency.currency.
   FIND FIRST acct WHERE acct.acct EQ in-acct AND
                         acct.close-date EQ ? NO-LOCK NO-ERROR.
   in-acct = DelFilFromAcct(in-acct).
   IF NOT AVAIL acct THEN DO:
      FIND FIRST c-nostro WHERE c-nostro.corr-acct EQ in-acct   AND
           CAN-FIND(FIRST acct WHERE acct.acct EQ c-nostro.acct AND
                                     acct.curr EQ c-nostro.curr AND
                            acct.filial-id  EQ shFilial AND
                                     acct.close-date EQ ?) NO-LOCK NO-ERROR.
      IF AVAIL c-nostro THEN DO:
         FIND FIRST acct OF c-nostro NO-LOCK NO-ERROR.
      END.
   END.
   IF AVAIL acct THEN in-acct = acct.acct.
   ELSE IF bic-headr NE ? AND bic-headr NE "" AND bic-headr NE "?" THEN DO:
      FIND FIRST banks-code WHERE banks-code.bank-code      EQ bic-headr
                              AND banks-code.bank-code-type EQ "BANK_ID" NO-LOCK NO-ERROR.
      IF NOT AVAIL banks-code THEN
      FIND FIRST banks-code WHERE banks-code.bank-code EQ bic-headr
                              AND banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.
      FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.
      IF AVAIL banks THEN DO:
         FOR EACH acct WHERE acct.cust-cat EQ "Б"
              AND acct.cust-id EQ banks.bank-id
              AND acct.curr EQ in-curr
           AND acct.filial-id  EQ shFilial
              AND acct.close-date EQ ? NO-LOCK,
              FIRST c-nostro OF acct WHERE c-nostro.corr-acct BEGINS in-acct AND TRIM(c-nostro.corr-acct) NE "" NO-LOCK:
                in-acct = acct.acct.
              RETURN.
         END.
         FOR EACH acct WHERE acct.cust-cat EQ "Б"
              AND acct.cust-id EQ banks.bank-id
              AND acct.curr EQ in-curr
              AND acct.filial-id  EQ shFilial
              AND acct.close-date EQ ? NO-LOCK,
              FIRST c-nostro OF acct WHERE in-acct BEGINS c-nostro.corr-acct AND TRIM(c-nostro.corr-acct) NE "" NO-LOCK:
              in-acct = acct.acct.
              RETURN.
         END.
         IF AcBnkSend THEN DO:
            FIND FIRST acct WHERE acct.cust-cat EQ "Б"
                   AND acct.cust-id EQ banks.bank-id 
                   AND acct.curr EQ in-curr
               AND acct.filial-id  EQ shFilial
                   AND acct.close-date EQ ? NO-LOCK NO-ERROR.
            IF AVAIL acct THEN in-acct = acct.acct.
         END.
      END.
   END.
in-acct = AddFilToAcct(in-acct,shFilial).
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*позиционирование на транзитный счет (др acct-curr-trans) и
обработка др транзакции ЗапрНазнСч и РазрНазнСч*/
/*---------------------------------------------------------------------------*/
PROCEDURE i-sw_posit_on_tran-acct:

   DEF INPUT-OUTPUT PARAM in-acct    AS CHAR NO-UNDO.
   DEF INPUT        PARAM in-op-kind AS CHAR NO-UNDO.
   
   DEF VAR i        AS INT64  NO-UNDO.
   DEF VAR v-tabu   AS CHAR NO-UNDO.
   DEF VAR v-permit AS CHAR NO-UNDO.
   DEF VAR v-curr   AS CHAR NO-UNDO.
   DEF VAR vTmpStr  AS CHAR NO-UNDO. /*для времнного хранения*/

   DEF BUFFER acct FOR acct.
   DEF BUFFER xacct FOR acct.

   v-tabu = GetXattrValueEx("op-kind",in-op-kind,"ЗапрНазнСч",?).
   IF v-tabu EQ ? THEN RETURN.
   v-permit = GetXattrValueEx("op-kind",in-op-kind,"РазрНазнСч","!*").

   IF length(in-acct) < 20 OR
      substr(in-acct,6,3) EQ "{&in-NC-Code}"
    THEN RETURN.

   v-curr = substr(in-acct,6,3).

   FIND FIRST acct WHERE acct.acct EQ in-acct   AND
                         acct.filial-id  EQ ShFilial 
                    AND  acct.curr EQ substr(in-acct,6,3) NO-LOCK NO-ERROR.

   IF NOT AVAIL acct OR (AVAIL acct AND NOT CAN-DO(v-tabu, acct.contract))
    THEN RETURN.
   ELSE
    DO i = 1 TO NUM-ENTRIES(v-permit):
      IF i EQ 1 THEN DO:
        IF GetXattrValueEx("acct",acct.acct + ',' + acct.currency,
                                  "acct-curr-trans",?) NE ? THEN
        DO:
           in-acct = GetXattrValue("acct",acct.acct + ',' + acct.currency,
                                   "acct-curr-trans").
           LEAVE.
        END.
      END.
      FIND FIRST xacct WHERE xacct.cust-cat EQ acct.cust-cat
                        AND  xacct.cust-id  EQ acct.cust-id
                        AND  xacct.curr EQ v-curr
                        AND  xacct.filial-id EQ ShFilial
                        AND  CAN-DO(ENTRY(i,v-permit),xacct.contract) NO-LOCK NO-ERROR.
      IF AVAIL xacct THEN DO:
        in-acct = xacct.acct.
        LEAVE.
      END.
      ELSE IF i EQ NUM-ENTRIES(v-permit) THEN
         RUN nacct IN h_import
            (in-op-kind, v-curr, OUTPUT vTmpStr, OUTPUT in-acct).
    END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*формирование 56 тега для авизо*/
/*---------------------------------------------------------------------------*/
PROCEDURE e-sw56-910:

    DEF INPUT            PARAM rec-op  AS recid NO-UNDO.
    DEF INPUT            PARAM foreign AS LOGICAL NO-UNDO.
    DEF INPUT            PARAM f-trans AS LOGICAL NO-UNDO.
    DEF INPUT            PARAM VerForm AS CHAR NO-UNDO.
    DEF INPUT            PARAM TypeMes AS CHAR NO-UNDO.
    DEF INPUT-OUTPUT PARAM copy-op AS CHAR NO-UNDO.
    DEF INPUT-OUTPUT PARAM err-op  AS CHAR NO-UNDO.

    DEF VAR var-frm AS CHAR NO-UNDO. /* вариант формата */
    DEF VAR sym-skip AS CHAR INITIAL "~n" NO-UNDO.
    DEF VAR tmp-send AS LOGICAL NO-UNDO.
    DEF VAR tmp-bic  AS CHAR NO-UNDO.
    DEF VAR tmp-type-bank  AS CHAR NO-UNDO.
    DEF VAR tmp-acct AS CHAR NO-UNDO.
    DEF VAR tmp-inn AS CHAR NO-UNDO.
    DEF VAR tmp-acct2 AS CHAR NO-UNDO.
    DEF VAR tmp-name AS CHAR NO-UNDO  EXTENT 5.
    DEF VAR tmp-i    AS INT64 NO-UNDO.
    DEF VAR tmp-j    AS INT64 NO-UNDO.
    DEF VAR ch-ret   AS CHAR NO-UNDO.
    DEF VAR set1   AS CHAR NO-UNDO.
    DEF VAR set2   AS CHAR NO-UNDO.
    DEF VAR flag-opbank AS LOGICAL NO-UNDO.
    DEF VAR vRecOpBank AS recid NO-UNDO.
    DEF VAR vTmpStrCode AS CHAR NO-UNDO.
    DEF VAR VNotISO    AS LOG NO-UNDO.  /*усли нет то наименования не обрезается*/
    DEF VAR VOpKind    AS CHAR NO-UNDO. /* код транзакции */
    DEF VAR VOptempl   AS CHAR NO-UNDO. /* код шаблона */
    DEF VAR VFormat    AS CHAR NO-UNDO. /* формат */

    DEF BUFFER op       FOR op.
    DEF BUFFER op-entry FOR op-entry.
    DEF BUFFER op-bank  FOR op-bank.
    DEF BUFFER banks    FOR banks.
    DEF BUFFER banks-code FOR banks-code.
    DEF BUFFER acct     FOR acct.

    RUN DecodeVerForm(INPUT-OUTPUT VerForm,
                      INPUT-OUTPUT VFormat,
                      INPUT-OUTPUT VOpKind,
                      INPUT-OUTPUT VOptempl,
                      INPUT-OUTPUT VNotISO
                     ).

    {wordwrap.def}
    mSpaceInn = IF FGetSetting("SWIFT","SpaceInn","Да") EQ "Да" THEN " " ELSE "".
    RELEASE banks.
    ASSIGN
        var-frm         = ""
        tmp-send        = NO
        tmp-bic         = ""
        tmp-type-bank   = ""
        tmp-acct        = ""
        tmp-inn         = ""
        set1            = ""
        set2            = ""
        tmp-acct2       = ""
        tmp-name[1]     = ""
        tmp-name[2]     = ""
        tmp-name[3]     = ""
        tmp-name[4]     = ""
        tmp-name[5]     = ""
        tmp-i           = 0
        tmp-j           = 0
        ch-ret          = ""
    .
FIND FIRST op WHERE recid(op) EQ rec-op NO-LOCK NO-ERROR.
IF NOT AVAIL op THEN err-op = "not-avail-op".
ELSE DO:
 FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
 IF op-entry.acct-db EQ ? THEN
 FIND FIRST op-entry OF op WHERE op-entry.acct-db NE ? NO-LOCK NO-ERROR.
 IF NOT AVAIL op-entry THEN err-op = "not-avail-op-entry".
 ELSE DO:

  vRecOpBank = get-op-bank(recid(op),"send").
  FIND op-bank WHERE recid(op-bank) EQ vRecOpBank NO-LOCK NO-ERROR.
  IF AVAIL op-bank AND
  ((CAN-FIND(FIRST acct WHERE
                acct.acct     EQ op-entry.acct-db AND
                acct.cust-cat EQ "Б") AND
    NOT CAN-FIND(op-bank OF op WHERE op-bank.op-bank-type EQ "intermed")
   ) OR
   (INDEX(copy-op,"~n:52") EQ 0 AND
    NOT foreign AND
    VFormat EQ "SWIFT")
  )
  THEN DO:
    FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
    FIND FIRST banks-code WHERE banks-code.bank-code EQ op-bank.bank-code AND
               banks-code.bank-code-type EQ op-bank.bank-code-type NO-LOCK NO-ERROR.

    IF (AVAIL banks-code AND
        (acct.cust-id NE banks-code.bank-id OR
         (INDEX(copy-op,"~n:52") EQ 0 AND
          NOT foreign AND
          VFormat EQ "SWIFT"
         )
        )
       ) OR
       NOT AVAIL banks-code THEN DO:

       RELEASE banks.
       IF INDEX(copy-op,"~n:52") NE 0 OR
          foreign OR
          VFormat NE "SWIFT" THEN
          FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
       ELSE IF AVAIL banks-code THEN
          FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.
       IF AVAIL banks THEN DO:

           IF NOT foreign THEN
           FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
              IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
              THEN tmp-acct2 = banks-corr.corr-acct.
           END.

           FIND FIRST banks-code OF banks WHERE
              banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
           IF NOT AVAIL banks-code OR foreign THEN
           FIND FIRST banks-code OF banks WHERE
              banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

           IF foreign THEN
           RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
           ASSIGN
             tmp-type-bank = IF AVAIL banks-code THEN banks-code.bank-code-type ELSE tmp-type-bank
             tmp-bic       = IF AVAIL banks-code AND
                              tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND
                              substr(banks-code.bank-code,9) EQ
                              FILL("X", length(banks-code.bank-code) - 8)
                             THEN substr(banks-code.bank-code,1,8)
                             ELSE IF AVAIL banks-code THEN banks-code.bank-code
                             ELSE tmp-bic
             tmp-acct      = tmp-acct2
             tmp-name[1]   = ch-ret
             tmp-name[1]   = IF (tmp-name[1] EQ "" AND foreign) OR NOT foreign
                             THEN ({banknm.lf banks} + " " + {bankct.lf banks})
                             ELSE tmp-name[1]
             var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                             ELSE IF tmp-type-bank EQ "BIC" THEN "D2"
                             ELSE "D"
           .
       END.
    END.
  END.
  ELSE IF CAN-FIND(op-bank OF op WHERE op-bank.op-bank-type EQ "intermed") THEN DO:

    RELEASE banks.
    RELEASE banks-code.
    FIND FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "intermed" NO-LOCK NO-ERROR.

    FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
    FIND FIRST banks-code WHERE banks-code.bank-code EQ op-bank.bank-code AND
               banks-code.bank-code-type EQ op-bank.bank-code-type NO-LOCK NO-ERROR.

    flag-opbank =
      IF (AVAIL banks-code AND acct.cust-id NE banks-code.bank-id AND
          acct.cust-cat EQ "Б"
         ) OR (NOT AVAIL banks-code AND acct.cust-cat EQ "Б") THEN NO ELSE YES.

    IF NOT flag-opbank THEN DO:
        FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
        IF AVAIL banks THEN DO:

           IF NOT foreign THEN
           FOR EACH banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id NO-LOCK:
              IF comp-curr(substr(banks-corr.corr-acct,6,3),op-entry.curr)
              THEN tmp-acct2 = banks-corr.corr-acct.
           END.

           FIND FIRST banks-code OF banks WHERE
              banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
           IF NOT AVAIL banks-code OR foreign THEN
           FIND FIRST banks-code OF banks WHERE
              banks-code.bank-code-type EQ "BIC" NO-LOCK NO-ERROR.

           IF foreign THEN
           RUN get-engl-name (recid(acct), NO, OUTPUT ch-ret).
        END.
    END.
    ASSIGN
             tmp-type-bank = IF flag-opbank THEN op-bank.bank-code-type
                             ELSE IF AVAIL banks-code THEN banks-code.bank-code-type
                             ELSE tmp-type-bank
             tmp-bic       = IF flag-opbank THEN op-bank.bank-code
                             ELSE IF AVAIL banks-code AND
                              tmp-type-bank EQ "BIC" AND length(banks-code.bank-code) > 8 AND
                              substr(banks-code.bank-code,9) EQ
                              FILL("X", length(banks-code.bank-code) - 8)
                             THEN substr(banks-code.bank-code,1,8)
                             ELSE IF AVAIL banks-code THEN banks-code.bank-code
                             ELSE tmp-bic
             tmp-acct      = IF flag-opbank THEN op-bank.corr-acct ELSE tmp-acct2
             tmp-name[1]   = IF flag-opbank THEN op-bank.bank-name ELSE ch-ret
             tmp-name[1]   = IF NOT flag-opbank AND
                             ((tmp-name[1] EQ "" AND foreign) OR NOT foreign)
                             THEN ({banknm.lf banks} + " " + {bankct.lf banks})
                             ELSE tmp-name[1]
             var-frm       = IF tmp-type-bank EQ "BIC" AND tmp-bic NE "" THEN "A"
                             ELSE IF tmp-type-bank EQ "BIC" THEN "D2"
                             ELSE "D"
           .
  END.

 tmp-acct = DelFilFromAcct(tmp-acct).
  IF tmp-acct EQ ? THEN tmp-acct = "".
  IF (tmp-acct NE "" AND tmp-acct NE ?) OR tmp-bic NE "" OR tmp-name[1] NE "" THEN DO:

      tmp-name[1] = IF f-trans THEN sw-trans(tmp-name[1],YES,VerForm) ELSE tmp-name[1].
      tmp-j = IF var-frm EQ "B" AND tmp-acct NE "" AND tmp-acct NE ? THEN 2
              ELSE IF var-frm EQ "B" AND tmp-acct EQ "" THEN 3
              ELSE IF tmp-acct NE "" THEN 4
              ELSE 5
       .
      tmp-name[1] = word-wrap(tmp-name[1], substr(FILL("35,",tmp-j),1,length(FILL("35,",tmp-j)) - 1) ,  YES).

      vTmpStrCode = sw-val-beg("SWIFT_CLI",tmp-type-bank).
      copy-op = copy-op + sym-skip + ":56" + substr(var-frm,1,1) + ":" +
         ( IF var-frm EQ "D2" AND
             tmp-acct NE "" AND
             tmp-acct NE ?
             THEN ("/" + tmp-acct + sym-skip)
             ELSE ""
         ) +
         ( IF var-frm EQ "A" AND
             tmp-bic NE ""
             THEN (tmp-bic + sym-skip)
             ELSE ""
         ) +
         ( IF var-frm EQ "D" AND
             tmp-bic NE ""
             THEN ("//" + ( IF vTmpStrCode EQ "MF" AND
                              CAN-DO("RUR5,RUR6",VerForm)
                              THEN "RU"
                              ELSE vTmpStrCode
                          ) +
                   tmp-bic + ( IF vTmpStrCode NE "MF" AND
                                 vTmpStrCode NE "RU"
                                 THEN sym-skip
                                 ELSE ""
                             )
                  )
             ELSE ""
         ) +
         ( IF var-frm EQ "D" AND
             tmp-acct NE ""
          THEN (( IF vTmpStrCode EQ "MF" OR
                    vTmpStrCode EQ "RU"
                    THEN "."
                    ELSE ""
                ) + tmp-acct + sym-skip
               )
          ELSE ""
         ) +
         ( IF var-frm BEGINS "D" AND
             tmp-inn NE "" AND
             tmp-inn NE ?
             THEN (( IF tmp-acct EQ "" AND
                       (vTmpStrCode EQ "MF" OR
                        vTmpStrCode EQ "RU"
                       )
                       THEN sym-skip
                       ELSE ""
                   ) + ( IF f-trans OR foreign THEN "INN" ELSE "ИНН") +
                   mSpaceInn + tmp-inn + sym-skip
                  )
             ELSE ""
         )
      .
      IF var-frm NE "A" THEN 
         RUN CutTag2(tmp-j,tmp-name[1],tmp-acct,tmp-bic,sym-skip,INPUT-OUTPUT copy-op).

  END.
 END.
END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* бъет строки по кол-ву символов */
/*---------------------------------------------------------------------------*/
PROCEDURE frm-det-inf:

   DEF INPUT PARAM ipLen AS INT64 NO-UNDO.
   /*кол-во символов в подстроке внутри |*/
   DEF INPUT-OUTPUT PARAM ioDetCod AS CHAR NO-UNDO. /*значение др frm-det-cod*/
   DEF INPUT-OUTPUT PARAM ioDetInf AS CHAR NO-UNDO. /*значение др frm-det-inf*/
   /*возвращает преобразованные строки*/

   
   DEF VAR vI AS INT64 NO-UNDO. /* счетчик */
   DEF VAR vTmpInt AS INT64 NO-UNDO. /* временное число */
   DEF VAR vTmpStr AS CHAR NO-UNDO. /* временная строка */
   DEF VAR vTmpCode AS CHAR NO-UNDO. /* временная строка для кода реквизита */
   ipLen = ipLen - 2.
   FOR EACH tt-det-cod:
      DELETE tt-det-cod.
   END.
   /* формирование TEMP-TABLE */
   DO vI = 1 TO NUM-ENTRIES(ioDetCod,"|"):
      IF ENTRY(vI,ioDetCod,"|") NE "" THEN DO:
         CREATE tt-det-cod.
         ASSIGN tt-det-cod.id  = ENTRY(vI,ioDetCod,"|")
                tt-det-cod.val = FILL(" ",LENGTH(tt-det-cod.id)) +
                                 ENTRY(vI,ioDetInf,"|").
      END.
      ELSE tt-det-cod.val = tt-det-cod.val + ENTRY(vI,ioDetInf,"|").
   END.

   /*разбиение*/
   ASSIGN
    ioDetCod = ""
    ioDetInf = "".
   FOR EACH tt-det-cod NO-LOCK:
      ASSIGN
       vTmpInt = IF LENGTH(tt-det-cod.val) MODULO ipLen EQ 0 
        THEN LENGTH(tt-det-cod.val) / ipLen
        ELSE TRUNCATE(LENGTH(tt-det-cod.val) / ipLen,0) + 1.
       vTmpStr = FILL(STRING(ipLen) + ",", vTmpInt).

      tt-det-cod.val =
      word-wrap(tt-det-cod.val,SUBSTR(vTmpStr,1,LENGTH(vTmpStr) - 1),YES).

      tt-det-cod.val = REPLACE(tt-det-cod.val,CHR(1),"|").

      ASSIGN
       ioDetCod =
        ioDetCod + ( IF ioDetCod EQ "" THEN "" ELSE "|") +
        tt-det-cod.id + FILL("|",NUM-ENTRIES(tt-det-cod.val,"|") - 1)
       ioDetInf =
       ioDetInf + ( IF ioDetInf EQ "" THEN "" ELSE "|") + tt-det-cod.Val
      .
   END.
   ioDetInf = LEFT-TRIM(ioDetInf).
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* поиск и коректировка каталога протоколов возвращает путь к каталогу       */
/*   и удаляет файл протокола                                                */
/*---------------------------------------------------------------------------*/
PROCEDURE catal-protoc:

   DEF INPUT-OUTPUT PARAM ioCatalProt AS CHAR NO-UNDO. /* путь к каталогу */
   DEF INPUT PARAM ipDelFileProt AS LOGICAL NO-UNDO. /* удалять протокол */

   DEF VAR vFldNotExist AS CHAR NO-UNDO. /*сообщение, если каталог не создан*/
   DEF VAR vSlash AS CHAR INITIAL "~\" NO-UNDO. /* слэш в зависиости от ОС */

   IF OPSYS EQ "unix" THEN vSlash = "/".

   ioCatalProt = FGetSetting("НСИ","КатПрот","").
   IF ioCatalProt > "" THEN
      ioCatalProt =
         IF SUBSTR(ioCatalProt,length(ioCatalProt),1) NE vSlash
         THEN ioCatalProt + vSlash
         ELSE ioCatalProt.
   ELSE ioCatalProt = "." + vSlash.
   IF NOT ExistFolder(ioCatalProt) THEN
   DO:
      OS-CREATE-DIR VALUE ( {&RELATIVE_2_ABSOLUTE}( ioCatalProt ) ).
      IF OS-ERROR NE 0 THEN
      DO:
         vFldNotExist = "Каталог протокола " + ioCatalProt + " (НП КатПрот) не создан.~n" + 
                        "Код ошибки OS-ERROR = " + STRING(OS-ERROR) + "~n" + 
                        "Протоколы будут созданы в каталоге ".
        ioCatalProt = "." + vSlash.
        vFldNotExist = vFldNotExist + ioCatalProt.
         IF auto THEN
            RUN Fill-AlertSysMes IN h_tmess("","",1,STRING(vFldNotExist)).

         ELSE
            RUN Fill-AlertSysMes IN h_tmess("","",1,STRING(vFldNotExist)).

      END.
   END.
   IF ipDelFileProt AND SEARCH(ioCatalProt + "protocol.swi") NE ?
   THEN OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( ioCatalProt + "protocol.swi" ) ).

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* соединение сводного протокола и реестра импорта */
/*---------------------------------------------------------------------------*/
PROCEDURE add-protoc:

   DEF INPUT PARAM ipCatalProt AS CHAR NO-UNDO. /* путь к каталогу */

   IF SEARCH(ipCatalProt + "protocol.swi") EQ ? THEN RETURN.
   IF OPSYS EQ "unix" THEN DO:
      IF SEARCH("_spool.tmp") NE ? THEN DO:
         IF FGetSetting("SWIFT", "DelProt", "") EQ "Да" THEN
         UNIX SILENT cat VALUE(ipCatalProt + "protocol.swi") _spool.tmp >
         VALUE(ipCatalProt + "protocol.log").
         ELSE UNIX SILENT cat VALUE(ipCatalProt + "protocol.swi") _spool.tmp >>
         VALUE(ipCatalProt + "protocol.log").
      END.
      ELSE
      DO:
         IF FGetSetting("SWIFT", "DelProt", "") EQ "Да" THEN
         UNIX SILENT cat VALUE(ipCatalProt + "protocol.swi") >
         VALUE(ipCatalProt + "protocol.log").
         ELSE UNIX SILENT cat VALUE(ipCatalProt + "protocol.swi") >>
         VALUE(ipCatalProt + "protocol.log").
      END.
   END.
   ELSE DOS SILENT copy VALUE(ipCatalProt + "protocol.swi") +
   _spool.tmp VALUE(ipCatalProt + "protocol.swi").

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* формирование протокола квитовки */
/*---------------------------------------------------------------------------*/
PROCEDURE reestr-protoc:
   DEF INPUT PARAM ipCatalProt AS CHAR NO-UNDO. /* путь к каталогу */
   DEF INPUT PARAM ipOutFile AS CHAR NO-UNDO. /* имя файла квитовки */

   IF FGetSetting("SWIFT", "reestr-cvi", "") EQ "Да" AND
   SEARCH(ipCatalProt + ipOutFile + ".cvi") NE ? THEN DO:
      IF OPSYS EQ "unix" AND SEARCH("_spool.tmp") NE ? THEN DO:
         OS-COPY VALUE ( {&RELATIVE_2_ABSOLUTE}( "_spool.tmp" ) ) VALUE ( {&RELATIVE_2_ABSOLUTE}( "1.tmp" ) ).
         OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( "_spool.tmp" ) ).
         UNIX SILENT cat 1.tmp VALUE(ipCatalProt + ipOutFile + ".cvi") >>
         _spool.tmp.
         OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( "1.tmp" ) ).
      END.
      ELSE DOS SILENT copy "_spool.tmp" +
      VALUE(ipCatalProt + ipOutFile + ".cvi") "_spool.tmp".
   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*универсальный поиск банка*/
/*---------------------------------------------------------------------------*/
PROCEDURE GetBanks:
   DEF INPUT PARAM iBankCode     AS CHAR NO-UNDO.     
   DEF INPUT PARAM iBankCodeType AS CHAR NO-UNDO.
   DEF PARAM BUFFER x-banks-code FOR banks-code.
   DEF PARAM BUFFER x-banks      FOR banks.

   DEF BUFFER banks-code   FOR banks-code.
   DEF BUFFER banks        FOR banks.

   RELEASE banks.
   RELEASE banks-code.
   RELEASE x-banks.
   RELEASE x-banks-code.

   FIND FIRST x-banks-code WHERE
      x-banks-code.bank-code EQ iBankCode AND
      x-banks-code.bank-code-type EQ iBankCodeType
   NO-LOCK NO-ERROR.

   IF AVAIL x-banks-code THEN
   FIND FIRST x-banks OF x-banks-code NO-LOCK NO-ERROR.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* расшифровка версии формата */
/*---------------------------------------------------------------------------*/
PROCEDURE DecodeVerForm:
  
   DEF INPUT-OUTPUT PARAM VerForm  AS CHAR NO-UNDO. /* версия формата*/
   DEF INPUT-OUTPUT PARAM VFormat  AS CHAR NO-UNDO. /* формат */
   DEF INPUT-OUTPUT PARAM VOpKind  AS CHAR NO-UNDO. /* код транзакции */
   DEF INPUT-OUTPUT PARAM VOptempl AS CHAR NO-UNDO. /* код шаблона */
   DEF INPUT-OUTPUT PARAM VNotISO  AS LOG NO-UNDO.  /*если нет то наименования
                                                      не обрезается*/
   IF NUM-ENTRIES(VerForm) > 1 THEN
      ASSIGN
         VFormat  = GetEntries(2,VerForm,",","")
         VOpKind  = GetEntries(3,VerForm,",","")
         VOptempl = GetEntries(4,VerForm,",","")
         VNotISO  = IF NUM-ENTRIES(VerForm) > 2 AND
                       GetXattrValue("op-kind",VOpKind,"Format-ISO") EQ "Нет"
                       THEN YES
                       ELSE NO
         VerForm  = ENTRY(1,VerForm).
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* обработка делителя, его вхождение заменяется на перевод строки*/
/*---------------------------------------------------------------------------*/
PROCEDURE DelInDet:
   DEF INPUT PARAM        iOpTempl  AS CHAR NO-UNDO. /*код шаблона*/
   DEF INPUT-OUTPUT PARAM ioDet  AS CHAR NO-UNDO.    /*содержание операции*/

   DEF VAR vDelInDet             AS CHAR NO-UNDO. /*др DelInDet*/
  
   ASSIGN
      vDelInDet = GetXattrValueEx("op-template",iOpTempl,"DelInDet",?)
      ioDet     = IF {assigned ioDet} AND
                     vDelInDet NE ?
                  THEN REPLACE(ioDet,vDelInDet,"~n")
                  ELSE ioDet.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* обработка делителя, его вхождение заменяется на перевод строки*/
/*---------------------------------------------------------------------------*/
PROCEDURE DelNoPrtDet:
   DEF INPUT-OUTPUT PARAM ioDet  AS CHAR NO-UNDO.    /*содержание операции*/

   DEF VAR vCount AS INT64 NO-UNDO.

   DO vCount = 1 TO 31:
      ioDet = IF {assigned ioDet} AND
                 NOT CAN-DO("10,13", STRING(vCount)) THEN REPLACE(ioDet,CHR(vCount)," ")
              ELSE ioDet.
   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* поиск счета */
/*---------------------------------------------------------------------------*/
PROCEDURE GetAcctById:
   DEF INPUT PARAM ipCustCat AS CHAR  NO-UNDO. /*тип клиента*/
   DEF INPUT PARAM ipCustId  AS INT64 NO-UNDO. /*код клиента*/
   DEF INPUT PARAM ipCurr    AS CHAR  NO-UNDO. /*валюта*/
   DEF INPUT PARAM ipContr   AS CHAR  NO-UNDO. /*назначение или пусто*/
   DEF INPUT PARAM ipSigns   AS CHAR  NO-UNDO. /*код др, или пусто*/
   DEF PARAM BUFFER x-acct FOR acct.
   DEF VAR vRecidAcct        AS RECID NO-UNDO.

   IF ipSigns EQ "" THEN 
      FIND FIRST x-acct    WHERE
         x-acct.cust-cat   EQ ipCustCat   AND
         x-acct.cust-id    EQ ipCustId    AND
         x-acct.currency   EQ ipCurr      AND
         x-acct.close-date EQ ?           AND
         x-acct.contract   BEGINS ipContr AND
         x-acct.filial-id  EQ shFilial    
      NO-LOCK NO-ERROR.
   ELSE DO:

&IF DEFINED (IS-DEBUG) &THEN
RUN dbgprint.p ("pp-swi.p/GetAcctById","ipCustCat: " + ipCustCat +
                                       " ipCustId:"  + STRING(ipCustId) +
                                       " ipCurr:"    + ipCurr +
                                       " ipContr:"   + ipContr +
                                       " ipSigns:"   + ipSigns ).
&ENDIF

      FOR EACH  x-acct                WHERE
                x-acct.cust-cat   EQ ipCustCat   AND
                x-acct.cust-id    EQ ipCustId    AND
                x-acct.currency   EQ ipCurr      AND
                x-acct.close-date EQ ?           AND
                x-acct.contract   BEGINS ipContr AND
                x-acct.filial-id  EQ shFilial    
          NO-LOCK,
          FIRST signs WHERE
                signs.file       EQ "acct"                          AND
                signs.surr       EQ x-acct.acct + "," + x-acct.curr AND
                signs.code       EQ ipSigns                         AND
                (signs.code-val  EQ "Да"                            OR
                 signs.xattr-val EQ "Да")
          NO-LOCK:

   &IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-swi.p/GetAcctById","!").
   &ENDIF
          vRecidAcct = RECID(x-acct).
          LEAVE.
      END.
      FIND FIRST x-acct WHERE RECID(x-acct) EQ vRecidAcct NO-LOCK NO-ERROR.
   END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* изменение кода документа для проверки */
/*---------------------------------------------------------------------------*/
PROCEDURE UpdDocType:
   DEF PARAM BUFFER op-template FOR op-template.

   DEF INPUT  PARAM ipDocType AS CHAR NO-UNDO. /*кода документа до обработки*/
   DEF INPUT  PARAM ipOpKind  AS CHAR NO-UNDO. /*код транзакции*/
   DEF OUTPUT PARAM opDocType AS CHAR NO-UNDO. /*кода документа после*/

   DEF VAR          vDocType  AS CHAR NO-UNDO. /*временно*/

   vDocType = GetXAttrValue("op-kind", ipOpKind, "ImportDocType").

   IF AVAILABLE op-template AND {assigned op-template.doc-type} THEN DO:
       IF vDocType = "DOC-TYPE" THEN
           opDocType = op-template.doc-type.
       ELSE
           RUN GetDocTypeDigital IN h_op (op-template.doc-type,
                                          ?,
                                          OUTPUT opDocType).
   END.
   ELSE DO:
       IF vDocType = "" THEN
           RUN GetDocTypeDigital IN h_op (ipDocType,
                                          ?,
                                          OUTPUT opDocType).
       ELSE
           opDocType = ipDocType.
   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* создание кода документа */
/*---------------------------------------------------------------------------*/
PROCEDURE CrDocType:
    DEFINE PARAMETER BUFFER op-template FOR  op-template.
    /* Код документа до обработки */
    DEFINE INPUT  PARAMETER iDocType    LIKE doc-type.doc-type NO-UNDO.
    /* Код транзакции */
    DEFINE INPUT  PARAMETER iOpKind     LIKE op-kind.op-kind   NO-UNDO.
    /* Код документа после обработки */
    DEFINE OUTPUT PARAMETER oDocType    AS   CHARACTER         NO-UNDO.

    DEFINE VARIABLE vDocType LIKE doc-type.doc-type NO-UNDO.
    DEFINE VARIABLE vAvail   AS   LOGICAL           NO-UNDO.

    IF AVAILABLE op-template AND {assigned op-template.doc-type} THEN
        oDocType = op-template.doc-type.
    ELSE DO:
        oDocType = "".
        RUN AvailDocType IN h_op (iDocType, ?, OUTPUT vAvail).
        RUN GetDocType IN h_op (iDocType, ?, OUTPUT vDocType).
        CASE GetXAttrValue("op-kind", iOpKind, "ImportDocType"):
            WHEN "DIGITAL" THEN
                oDocType = vDocType.
            WHEN "DOC-TYPE" THEN
                IF vAvail THEN
                    oDocType = iDocType.
            OTHERWISE DO:
                IF vAvail THEN
                    oDocType = iDocType.
                ELSE IF {assigned iDocType} THEN DO:
                    RUN AvailDocTypeDigital IN h_op (iDocType,
                                                     ?,
                                                     OUTPUT vAvail).
                    IF vAvail THEN
                        oDocType = vDocType.
                END.
            END.
        END.
    END.
    IF oDocType = ? THEN
        oDocType = "".
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* Определение типа документа: внутренний,начальный,ответный или транзитный  */
/*---------------------------------------------------------------------------*/
/*из i-swopcr.i перенести i-swo100.p pos-acct-no-bic*/ /*mkv*/
PROCEDURE GetFlagGo:
   DEF INPUT PARAM  ipAcctDb       AS CHAR NO-UNDO. /* счет дебета                    */
   DEF INPUT PARAM  ipBankTypeRec  AS CHAR NO-UNDO. /* тип кода банка получателя      */
   DEF INPUT PARAM  ipBankRec      AS CHAR NO-UNDO. /* банк получателя                */
   DEF INPUT PARAM  ipAvailBankRec AS LOG  NO-UNDO. /* указан банк получателя         */
   DEF OUTPUT PARAM opFlagGo       AS INT64  NO-UNDO. /* тип документа                  */
                                                    /*  0  - внутр.                   */
                                                    /*  1  - нач.                     */
                                                    /*  2  - отв.                     */
                                                    /*  3  - транз.                   */
   DEF VAR vIdOurBank              AS INT64  NO-UNDO. /*  код нашего банка              */
   DEF VAR vIsOurBank              AS LOG  NO-UNDO. /*  мы банк получатль             */
   DEF VAR vAcctDbNoBank           AS LOG  NO-UNDO. /*  счет дебета - не нашего банка */

   RUN OurBank(INPUT  ipBankTypeRec,          /*тип кода*/              
               INPUT  ipBankRec,              /*код банка*/             
               INPUT  ipAvailBankRec,         /*указан банк получателя*/
               OUTPUT vIsOurBank,             /*наш банк или нет*/      
               OUTPUT vIdOurBank).            /*код нашего банка*/      

   FIND FIRST acct WHERE
              acct.acct EQ ipAcctDb NO-LOCK NO-ERROR.
   ASSIGN
      vAcctDbNoBank =    ( AVAIL acct                 AND 
                           acct.cust-cat NE "Б"
                          ) 
                      OR ( AVAIL acct                 AND 
                           acct.cust-cat EQ "Б"       AND 
                           acct.cust-id  EQ vIdOurBank
                          )
                                /*  vAcctDbNoBank - отправитель */
                                /*  vIsOurBank    - получатель  */      

      opFlagGo      = IF            vAcctDbNoBank AND    /* Наш банк или наш клиент              */      
                                    vIsOurBank           /* Наш банк или наш клиент              */      
                      THEN 0
                      ELSE   IF     vAcctDbNoBank AND    /* Наш банк или наш клиент              */      
                                NOT vIsOurBank           /* Сторонний банк или стороннний клиент */
                      THEN 1
                      ELSE   IF NOT vAcctDbNoBank AND    /* Сторонний банк или стороннний клиент */  
                                    vIsOurBank           /* Наш банк или наш клиент              */      
                      THEN 2
                      /*        NOT vAcctDbNoBank AND */ /* Сторонний банк или стороннний клиент */
                      /*        NOT vIsOurBank        */ /* Сторонний банк или стороннний клиент */
                      ELSE 3
   .
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* филиал без бика                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE BankNoBic:
   DEF INPUT  PARAM ipAcct      AS CHAR NO-UNDO. /*счет филиала*/
   DEF PARAM BUFFER x-branch FOR branch.

   DEF BUFFER acct       FOR acct.
   DEF BUFFER branch     FOR branch.

   RELEASE x-branch.
   IF LENGTH(ipAcct) NE 20
      THEN RETURN.

   FIND FIRST acct 
        WHERE acct.acct EQ ipAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL acct THEN DO:
      {getbracc.i &pre="x-" &acct="ipAcct"}
   END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* наш ли банк                                                               */
/*---------------------------------------------------------------------------*/
PROCEDURE OurBank:
   DEF INPUT  PARAM ipBankCodeType AS CHAR NO-UNDO. /* тип кода              */
   DEF INPUT  PARAM ipBankCode     AS CHAR NO-UNDO. /* код банка             */
   DEF INPUT  PARAM ipAvailBankRec AS LOG  NO-UNDO. /* указан банк получателя*/
   DEF OUTPUT PARAM opIsOurBank    AS LOG  NO-UNDO. /* наш банк или нет      */
   DEF OUTPUT PARAM opIdOurBank    AS INT64  NO-UNDO. /* код нашего банка      */

   DEF          VAR vBranchId      AS CHAR NO-UNDO.
   DEF          VAR mCount         AS INT64  NO-UNDO.

   DEF BUFFER banks-code FOR banks-code.
   DEF BUFFER branch     FOR branch.

   FOR FIRST branch WHERE branch.branch-id EQ mKodFil NO-LOCK:
      ASSIGN 
         opIdOurBank   = branch.bank-id
         vBranchId     = IF   we-have-bic() 
                         THEN branch.branch-id
                         ELSE branch.parent-id.
   END.
FIND-BANKS:
   DO mCount = 1 TO NUM-ENTRIES(vBranchId,";"): 
      RELEASE branch.
      RELEASE banks-code.

      FOR FIRST branch WHERE 
                branch.branch-id EQ ENTRY(mCount,vBranchId,";") NO-LOCK:
         FIND FIRST banks-code WHERE 
                    banks-code.bank-id        EQ branch.bank-id AND
                    banks-code.bank-code-type EQ ipBankCodeType AND
                    banks-code.bank-code      EQ ipBankCode     NO-LOCK NO-ERROR.
         IF AVAIL   banks-code THEN
            LEAVE FIND-BANKS.
      END.
   END.

   opIsOurBank = (AVAIL banks-code) OR (NOT ipAvailBankRec).

END PROCEDURE.
/*---------------------------------------------------------------------------*/
/* Обрезание текста сообщения                                                */
/*---------------------------------------------------------------------------*/
PROCEDURE RmBufCopyDoc:
   DEF INPUT        PARAM ipBufCopyDoc  AS CHAR NO-UNDO. /*техт собщения*/
   DEF INPUT-OUTPUT PARAM iopBuf        AS CHAR NO-UNDO. /*Текущая строка*/
   iopBuf = IF LENGTH(ipBufCopyDoc) >= 31500 THEN ""
            ELSE iopBuf.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Определение способа получения документа                                    */
/*----------------------------------------------------------------------------*/
PROCEDURE SetGettingWay:
   DEFINE INPUT PARAMETER  iOp      LIKE  op.op                   NO-UNDO.
   DEFINE INPUT PARAMETER  iOpKind  LIKE  op-kind.op-kind         NO-UNDO.
   DEFINE INPUT PARAMETER  iMailNum LIKE  mail-user.mail-user-num NO-UNDO.
   
   DEFINE VARIABLE vNameRecv AS CHARACTER  INIT "СпособПолуч" NO-UNDO.
   DEFINE VARIABLE vGetWay   AS CHARACTER                     NO-UNDO.

   vGetWay = GetXAttrValueEx("op", STRING(iOp), vNameRecv, "").
   IF vGetWay EQ "" THEN DO:
      vGetWay = GetXAttrValueEx("mail-user",STRING(iMailNum),vNameRecv,"").
      IF NOT {assigned vGetWay} THEN
         vGetWay = GetXAttrValueEx("op-kind",iOpKind,vNameRecv,"").
      IF {assigned vGetWay} THEN
         UpdateSigns("op",STRING(iOp),vNameRecv,vGetWay,YES).
   END.

END PROCEDURE.
/*---------------------------------------------------------------------------*/
PROCEDURE ReleaseOpDate:
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEFINE VARIABLE mCats  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vIcats AS INT64    NO-UNDO.
   {rel-date.i &in-op-date = iDate}
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE CutTag1:
   DEF INPUT        PARAM iMax          AS INT64  NO-UNDO.
   DEF INPUT        PARAM iTmpName      AS CHAR NO-UNDO.
   DEF INPUT        PARAM iTmpAcct      AS CHAR NO-UNDO.
   DEF INPUT        PARAM iTmpInn       AS CHAR NO-UNDO.
   DEF INPUT        PARAM iSymSkip      AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM oCopyOp       AS CHAR NO-UNDO.

   DEF VAR i AS INT64 NO-UNDO.

   DO i = 1 TO iMax:
      IF ENTRY(i,iTmpName,chr(1)) NE "" THEN 
         oCopyOp = oCopyOp + ( IF i EQ 1 AND (iTmpAcct EQ "" AND (iTmpInn EQ "" OR iTmpInn EQ ?)) THEN "" ELSE iSymSkip) + ENTRY(i,iTmpName,chr(1)). 
    END.
END.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE CutTag2:
   DEF INPUT        PARAM iMax          AS INT64  NO-UNDO.
   DEF INPUT        PARAM iTmpName      AS CHAR NO-UNDO.
   DEF INPUT        PARAM iTmpAcct      AS CHAR NO-UNDO.
   DEF INPUT        PARAM iTmpBic       AS CHAR NO-UNDO.
   DEF INPUT        PARAM iSymSkip      AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM oCopyOp       AS CHAR NO-UNDO.

   DEF VAR i AS INT64 NO-UNDO.

   DO i = 1 TO iMax:
      IF ENTRY(i,iTmpName,chr(1)) NE "" THEN
         oCopyOp = oCopyOp + ( IF iTmpAcct EQ "" AND iTmpBic EQ "" THEN "" ELSE iSymSkip) + ENTRY(i,iTmpName,chr(1)) + iSymSkip.
   END.
END.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Chk-a RETURN LOG (iArg AS CHAR):
   DEF VAR i AS INT64 NO-UNDO.
   DEF VAR j AS INT64 NO-UNDO.
   DO i = 1 TO LENGTH(iArg):
      j = asc(SUBSTRING(iArg,i,1)).
      IF ((j GE 65  AND j LE 90 )  OR
          (j GE 128 AND j LE 159)) THEN NEXT.
      ELSE
         RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Chk-c RETURN LOG (iArg AS CHAR):
   DEF VAR i AS INT64 NO-UNDO.
   DEF VAR j AS INT64 NO-UNDO.
   DO i = 1 TO LENGTH(iArg):
      j = asc(SUBSTRING(iArg,i,1)).
      IF ((j GE 65  AND j LE 90 )  OR
          (j GE 128 AND j LE 159)  OR
          (j GE 48  AND j LE 57 )) THEN NEXT.
      ELSE
         RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Chk-n RETURN LOG (iArg AS CHAR):
   DEF VAR i AS INT64 NO-UNDO.
   DEF VAR j AS INT64 NO-UNDO.
   DO i = 1 TO LENGTH(iArg):
      j = asc(SUBSTRING(iArg,i,1)).
      IF (j GE 48  AND j LE 57 ) THEN NEXT.
      ELSE
         RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-XXXX RETURN LOG (iArg AS CHAR,iTag AS CHAR, iIsMess AS log):

   IF (NUM-ENTRIES(iArg,"/") LT 2) THEN DO:
      IF iIsMess THEN MESSAGE "Тэг " + iTag + " не содержит разделитель /" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   IF length(ENTRY(1,iArg,"/")) NE 2 THEN DO:
      IF iIsMess THEN MESSAGE "Тэг " + iTag + ": ошибка формата 2!a - кол-во символов не равно 2" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   
   IF Chk-a(ENTRY(1,iArg,"/")) THEN DO:
      IF iIsMess THEN MESSAGE "Тэг " + iTag + ": ошибка формата 2!a - символы только прописные" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   
   IF length(ENTRY(2,iArg,"/")) GT 27 THEN DO:
      IF iIsMess THEN MESSAGE "Тэг " + iTag + ": ошибка формата 27x - кол-во символов не больше 2" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-IBEI RETURN LOG (iArg AS CHAR, iIsMess AS log, iForeing AS log):

   DEF VAR s1 AS CHAR NO-UNDO.
   DEF VAR s2 AS CHAR NO-UNDO.
   
   if iForeing then do:
      IF (length(iArg) LT 8) OR (length(iArg) GT 11) THEN DO:
         IF iIsMess THEN MESSAGE "Тэг IBEI: ошибка длины тэга" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
         RETURN YES.
      END.
      
      s1 = SUBSTRING(iArg,1,8).
      s2 = SUBSTRING(iArg,9).
      
      IF Chk-a(s1) THEN DO:
         IF iIsMess THEN MESSAGE "Тэг IBEI: ошибка формата 4!a2!a2!a - символы только прописные" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
         RETURN YES.
      END.
      IF Chk-a(s2) THEN DO:
         IF iIsMess THEN MESSAGE "Тэг IBEI: ошибка формата 2!c[3!c] - символы только прописные и цифры" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
         RETURN YES.
      END.
   end.
   else do:
      if NUM-ENTRIES(iArg,"/") ne 2 then do:
         IF iIsMess THEN MESSAGE "Тэг IBEI: ошибка формата /30x - тэг не содержит ""/"" " VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
         RETURN YES.
      end.
   end.
   
   
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-INF RETURN LOG (iArg AS CHAR,iTag AS CHAR, iIsMess AS log, iForeing AS log):

   DEFINE VAR oOk   AS LOGICAL  NO-UNDO INIT NO.

   CASE iTag:
      WHEN "IBEI"  THEN oOk = Check50F-IBEI(iArg,iIsMess,iForeing).
      WHEN "ARNU"  OR
      WHEN "CCPT"  OR
      WHEN "CUST"  OR
      WHEN "DRLC"  OR
      WHEN "EMPL"  OR
      WHEN "NIDN"  OR
      WHEN "SOSE"  OR
      WHEN "TXID"  THEN oOk = Check50F-XXXX (iArg,iTag,iIsMess).
   END CASE.

   RETURN oOk.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-x RETURN LOG (iArg AS CHAR,iNum AS INT64, iIsMess AS log):
   DEF VAR vTag AS CHAR NO-UNDO.
   IF GetEntries(2,iArg,CHR(1),"") NE "" THEN
      ASSIGN
         vTag = ENTRY(1,iArg,CHR(1))
         iArg = ENTRY(2,iArg,CHR(1))
      .

   IF (NUM-ENTRIES(iArg,"/") NE 2) THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка " + string(iNum) + ": не содержит разделитель /" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   IF length(ENTRY(1,iArg,"/")) NE 2 THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка " + string(iNum) + ": ошибка формата 2!a - кол-во символов не равно 2" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   IF Chk-a(ENTRY(1,iArg,"/")) THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка " + string(iNum) + ": ошибка формата 2!a - символы только прописные" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   IF length(ENTRY(2,iArg,"/")) GT 30 THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка " + string(iNum) + ": ошибка формата 27x - кол-во символов не больше 30" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-4 RETURN LOG (iArg AS CHAR, iIsMess AS log):
   DEF VAR vTag AS CHAR NO-UNDO.
   IF GetEntries(2,iArg,CHR(1),"") NE "" THEN
      ASSIGN
         vTag = ENTRY(1,iArg,CHR(1))
         iArg = ENTRY(2,iArg,CHR(1))
      .
   IF length(iArg) NE 8 THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка 4: ошибка формата 4!n2!n2!n - кол-во символов не равно 8" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   IF Chk-n(iArg) THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка 4: ошибка формата 4!n2!n2!n - символы только цифры" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F-1 RETURN LOG (iArg AS CHAR,iNum AS INT64,iIsMess AS log):
   DEF VAR vTag AS CHAR NO-UNDO.
   IF GetEntries(2,iArg,CHR(1),"") NE "" THEN
      ASSIGN
         vTag = ENTRY(1,iArg,CHR(1))
         iArg = ENTRY(2,iArg,CHR(1))
      .
   IF length(iArg) GT 33 THEN DO:
      IF iIsMess THEN MESSAGE ( IF vTag NE "" THEN ("Тег " + vTag + " ") ELSE "") +
                              "Строка " + string(iNum) + ": ошибка формата 33x - кол-во символов ,больше 33" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.         
      RETURN YES.
   END.
   RETURN NO.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION Check50F RETURN LOG (iArg AS CHAR,iNum AS INT64, iIsMess AS log):
   DEFINE VAR oOk   AS LOGICAL  NO-UNDO INIT NO.
   CASE iNum:
      WHEN 1  OR
      WHEN 2  OR
      WHEN 8  THEN oOk = Check50F-1(iArg,iNum,iIsMess).
      WHEN 4  THEN oOk = Check50F-4(iArg,iIsMess).
      WHEN 3  OR
      WHEN 5  OR
      WHEN 6  OR
      WHEN 7  THEN oOk = Check50F-x(iArg,iNum,iIsMess).
   END CASE.
   RETURN oOk.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION FillNAcctCr RETURN CHAR (iOpKind      AS CHAR,
                                  iFormat      AS CHAR,
                                  iTypeMsg     AS CHAR,
                                  iAcct        AS CHAR,
                                  iBankCode    AS CHAR,
                                  iOurBankCode AS CHAR,
                                  iINN         AS CHAR,
                                  iOurINN      AS CHAR,
                                  iBankName    AS CHAR,
                                  iOurBankName AS CHAR):

   DEFINE VAR oAcct   AS CHAR NO-UNDO.
   DEFINE VAR vTmpStr AS CHAR NO-UNDO.

   oAcct = iAcct.
   IF     iFormat      EQ "TELEX-BSS"
      AND CAN-DO("100,103",iTypeMsg)
      AND NOT {assigned iAcct}
      AND iBankCode    EQ iOurBankCode
      AND iINN         EQ iOurINN
      AND iBankName    EQ iOurBankName THEN DO:

       RUN nacct IN h_import
          (iOpKind, "", OUTPUT vTmpStr, OUTPUT oAcct).

   END.
   RETURN oAcct.
END FUNCTION.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
FUNCTION trans50f RETURN char (iTxt as char,iTrans as log,iVerForm as char):
   iTxt = SUBSTRING(iTxt,3).
   if iTrans THEN do:
      if NUM-ENTRIES(iTxt,"/") ge 2 then 
         ENTRY(2,iTxt,"/") = sw-trans(ENTRY(2,iTxt,"/"),no,iVerForm).
      ELSE
         iTxt = sw-trans(iTxt,no,iVerForm).
   end.
   RETURN iTxt.
END FUNCTION.
/****************************************************************************/

/*---------------------------------------------------------------------------*/
/*  Преобразование содержания операции для исключения недопустимых символов  */
/*---------------------------------------------------------------------------*/
PROCEDURE WrapSwiftDetails.
   DEFINE INPUT        PARAMETER iWrapCodes AS CHARACTER   NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioDetails  AS CHARACTER   NO-UNDO.
   DEFINE INPUT        PARAMETER iTransform AS LOGICAL     NO-UNDO.
   DEFINE OUTPUT       PARAMETER oMsg AS CHARACTER         NO-UNDO.

   DEFINE VARIABLE vTmpCodes AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTmpDet   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFlag     AS INT64       NO-UNDO.
   DEFINE VARIABLE vI        AS INT64       NO-UNDO.
   DEFINE VARIABLE vJ        AS INT64       NO-UNDO.

   vTmpCodes = iWrapCodes.
   oMsg      = "".
   REPEAT:
      vTmpDet = ioDetails.
      vTmpDet = word-wrap(vTmpDet,vTmpCodes,YES).
      vFlag = 0.
      DO vI = 2 TO NUM-ENTRIES(vTmpDet, CHR(1)) :
         IF ENTRY(vI, vTmpDet, CHR(1)) BEGINS "-" OR 
            ENTRY(vI, vTmpDet, CHR(1)) BEGINS ":" THEN DO:
            IF iTransform THEN DO:
               ENTRY(vI - 1, vTmpCodes) = STRING(INTEGER(ENTRY(vI - 1, vTmpCodes)) - 1).
               vFlag = vI.
               DO vJ = vI TO NUM-ENTRIES(iWrapCodes) :
                  ENTRY(vJ, vTmpCodes) = ENTRY(vJ, iWrapCodes).
               END.
               LEAVE.
            END.
            ELSE oMsg = oMsg + " Недопустимый символ " + 
                        "<" + SUBSTR(ENTRY(vI,vTmpDet,CHR(1)),1,1) + "> " +
                        "в начале строки: " + ENTRY(vI,vTmpDet,CHR(1)).
         END.
      END. 
      IF vFlag EQ 0 THEN LEAVE.
   END.
   ioDetails = vTmpDet.

END PROCEDURE.

PROCEDURE SWTransRUR6.
   DEFINE INPUT  PARAMETER iTransl AS LOGICAL                       NO-UNDO.
   DEFINE INPUT  PARAMETER iStr    AS CHAR                          NO-UNDO.
   DEFINE OUTPUT PARAMETER oStr    AS CHAR                          NO-UNDO.

   DEFINE VARIABLE vLat           AS CHARACTER INITIAL "A,B,V,G,D,E,o,J,Z,I,i,K,L,M,N,O,P,R,S,T,U,F,H,C,c,Q,q,x,Y,X,e,u,a,j,n,n,p,d,b,s,v,z,r,m, f, ), (,~", j,~/,~/,~/,),(,(,),f,f,f, f"   CASE-SENSITIVE NO-UNDO.
   DEFINE VARIABLE vKir           AS CHARACTER INITIAL "А,Б,В,Г,Д,Е,Ё,Ж,З,И,Й,К,Л,М,Н,О,П,Р,С,Т,У,Ф,Х,Ц,Ч,Ш,Щ,Ъ,Ы,Ь,Э,Ю,Я,',№,#,%,&,!,$,;,_,=,~",*,~},~{,m,~',~/,~\,|,>,<,[,],*,@,^,~~"     CASE-SENSITIVE NO-UNDO.
   DEFINE VARIABLE vLatExept      AS CHARACTER INITIAL "(,)"                                                                                                CASE-SENSITIVE NO-UNDO.
   DEFINE VARIABLE vItm           AS CHARACTER INITIAL ""           NO-UNDO.
   DEFINE VARIABLE vItm2          AS CHARACTER INITIAL ""           NO-UNDO.
   DEFINE VARIABLE vI             AS INT64                          NO-UNDO.
   DEFINE VARIABLE vJ             AS INT64                          NO-UNDO.
   DEFINE VARIABLE vFlagTranclit  AS LOGICAL        INIT YES        NO-UNDO.
   DEFINE VARIABLE vFlagLat       AS LOGICAL        INIT NO         NO-UNDO.
   DEFINE VARIABLE vFlagFig       AS LOGICAL        INIT NO         NO-UNDO.

   IF iTransl THEN iStr = CAPS(iStr).

   DO vI   = 1 TO LENGTH(iStr):
      vItm  = SUBSTRING(iStr, vI, 1).
     vItm2 = SUBSTRING(iStr, vI + 1, 1).
     IF vItm  EQ "'" AND 
        vItm2 EQ "(" THEN DO:
        oStr = oStr + "~{".
        vI = vI + 1.
        vFlagTranclit = NOT vFlagTranclit.
        NEXT.
     END.

     IF vItm  EQ ")" AND 
        vItm2 EQ "'" THEN DO:
        oStr = oStr + "~}".
        vI = vI + 1.
        vFlagTranclit = NOT vFlagTranclit.
        NEXT.
     END.

     IF NOT iTransl THEN DO:
        IF vItm EQ "'"   THEN DO:
           vFlagTranclit = NOT vFlagTranclit.
           NEXT.
        END.
     END.
     IF iTransl THEN DO:
        IF vItm EQ "~{" AND vItm2 EQ "V"  THEN DO:
           vFlagTranclit = NOT vFlagTranclit.
           oStr = oStr + "'(" .
           vFlagFig = YES.
           NEXT.
        END.
     END.

     IF iTransl AND vFlagFig THEN DO:
        IF vItm EQ "~}"   THEN DO:
           vFlagTranclit = NOT vFlagTranclit.
           oStr = oStr + ")'" .
           vFlagFig = NO.
           NEXT.
        END.
     END.
     IF iTransl  AND
        vItm  EQ "(" AND 
        vItm2 EQ "V" THEN DO:
        oStr = oStr + "'" + vItm + vItm2.
        vI = vI + 1.
        vFlagTranclit = NOT vFlagTranclit.
        vFlagFig = YES.
        NEXT.
     END.
     IF iTransl AND vFlagFig AND
        vItm  EQ ")" THEN DO:
        oStr = oStr + ")'".
        vI = vI + 1.
        vFlagTranclit = NOT vFlagTranclit.
        vFlagFig = NO.
        NEXT.
     END.

     IF NOT iTransl THEN DO:
        IF vItm EQ "(" OR vItm EQ ")"  THEN DO:
           oStr = oStr + vItm .
           NEXT.
        END.
     END.

     IF vFlagTranclit THEN DO:
        IF iTransl THEN DO:
           IF vItm GE "A"  AND
              vItm LE "Z"  AND
              NOT vFlagLat THEN DO:
              vFlagLat = NOT vFlagLat.
              oStr = oStr + "'" + vItm.

              IF vI GE LENGTH(iStr) THEN 
                 oStr = oStr  + "'".
              NEXT.
           END.

           IF vFlagLat AND
              vI GE LENGTH(iStr) THEN DO:
              oStr = oStr + vItm + "'".
              NEXT.
           END.

           IF NOT (vItm GE "A"  AND
                   vItm LE "Z") AND
              vFlagLat THEN DO:
              vFlagLat = NOT vFlagLat.
              oStr = oStr + "'".
           END.
           IF vFlagLat AND
              vI EQ  LENGTH(iStr) THEN DO:
              oStr = oStr + vItm + "'".
              NEXT.
           END.
        END.
     END.

      vJ    = LOOKUP(vItm,IF iTransl THEN vKir
                                    ELSE vLat).
      IF vJ NE 0 AND vFlagTranclit THEN 
         vItm = ENTRY(vJ, IF iTransl THEN vLat
                                    ELSE vKir).
       oStr = oStr + vItm.
   END.
END PROCEDURE.

PROCEDURE PosAcctOnFil.
   DEFINE INPUT         PARAMETER iBankCode AS CHARACTER   NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER iAcctCr   AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT        PARAMETER oOK       AS LOGICAL     NO-UNDO.
   DEFINE OUTPUT        PARAMETER oError    AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcctCr  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOurBank AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBankID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vFilCr   AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bAcctCr FOR acct.
   DEFINE BUFFER acct    FOR acct.
   DEFINE BUFFER branch  FOR branch.

   vOurBank =  IF iBankCode EQ bank-mfo-9 OR
                  iBankCode EQ bank-bic   OR
                  iBankCode EQ ? 
               THEN YES
               ELSE NO.

   IF NOT vOurBank THEN
      RETURN.
   IF NOT shMode THEN
      RETURN.
   IF fGetSetting ("SWIFT", "СчКрМФ", "Нет") NE "Да" THEN
      RETURN.

   vAcctCr = iAcctCr.
   {find-act.i &acct = vAcctCr &bact = bAcctCr}
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("PosAcctOnFil","1 AVAIL bAcctCr:" + STRING(AVAIL bAcctCr)).    
   &ENDIF
   IF AVAIL bAcctCr THEN
      RETURN.

   FIND FIRST bAcctCr WHERE bAcctCr.number EQ vAcctCr NO-LOCK NO-ERROR.
   IF AVAIL bAcctCr AND NOT AMBIGUOUS bAcctCr THEN DO:
      vFilCr = bAcctCr.filial-id.
      FIND FIRST branch WHERE branch.Branch-Id EQ vFilCr NO-LOCK NO-ERROR.
      IF AVAIL branch THEN DO:
         vBankID = branch.Bank-Id.
         &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p ("PosAcctOnFil","vBankID:" + GetNullInt(vBankID)).    
         &ENDIF
         FIND FIRST acct WHERE acct.cust-cat  EQ "Б"
                           AND acct.cust-id   EQ vBankID
                           AND acct.filial-id EQ shFilial
                           AND acct.bal-acct  EQ 30301
                           AND acct.currency  EQ bAcctCr.currency
            NO-LOCK NO-ERROR.
         IF AVAIL acct THEN 
            ASSIGN
               iAcctCr = acct.acct
               oOK     = YES.
         ELSE
            oError = "swift-59-03".
      END.
   END.

END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/10/2014 12:31:51.362+04:00' */
/* $LINTFILE='pp-swi.p' */
/*prosignxlmj7+AjNY2Q0NFpA0V1Sg*/
/* --- pp-swi.p was humbly modified by (c)blodd converter v.1.11 on 2/28/2017 7:55am --- */
