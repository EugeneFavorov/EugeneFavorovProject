/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ Герман
Что делает:     Создает отчет исправлений на счетах дат открытия и закрытия
Как работает:   
Место запуска:  Планировщик утром.
Создан:         25.05.2017 Борисов А.В.
*/

DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.   /* days=5;mail=<список> */

RUN pb_tstwork.p.
IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.

{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
/*
{intrface.get netw}     /** Отправка в bispc */
*/
{intrface.get date}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{parsin.def}

IF HolidayRu(TODAY) THEN RETURN.

DEFINE VARIABLE iMail       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iDays       AS INTEGER      NO-UNDO.
iMail  = GetParamByNameAsChar(iParam, "mail", "").          /* Список п/я для рассылки отчета */
iDays  = INT(GetParamByNameAsChar(iParam, "days", "5")).

end-date = TODAY - 1.
beg-date = rAfterWorkDays(TODAY, - iDays).

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dNewDate    AS DATE         NO-UNDO.
DEFINE VARIABLE d2NiDate    AS DATE         NO-UNDO.
DEFINE VARIABLE d2NiSrok    AS DATE         NO-UNDO.
DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cAcctL      AS CHARACTER    NO-UNDO INIT
    "30109*,30111*,405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*".
DEFINE TEMP-TABLE ttIzm     NO-UNDO
    FIELD cFil      AS CHARACTER
    FIELD cAcct     AS CHARACTER
    FIELD cSurr     AS CHARACTER
    FIELD dOpen     AS DATE
    FIELD dClose    AS DATE
    FIELD d2NI      AS DATE
    FIELD dIspr     AS DATETIME
    FIELD cFild     AS CHARACTER
    FIELD dOld      AS DATE
    FIELD dNew      AS DATE
    FIELD cUser     AS CHARACTER
    FIELD cComm     AS CHARACTER
    .
DEFINE BUFFER   hist        FOR history.

/* * Дата, сообщенная в НИ **************************************************** */
FUNCTION Date2NI    RETURNS DATE
   (INPUT  iTxt     AS CHARACTER ) FORWARD.

/* Открытие счета ************************************************************* */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'C')
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "Транз")
    NO-LOCK
    BY history.modif-date:

    /* Если счет был позже переоткрыт, то пропускаем */
    FIND FIRST hist
        WHERE (hist.file-name   EQ 'acct')
          AND (hist.field-ref   EQ history.field-ref)
          AND (hist.modify      EQ 'C')
          AND (hist.modif-date  GE history.modif-date)
          AND (hist.modif-date  GT history.modif-date
            OR hist.modif-time  GT history.modif-time)
        NO-LOCK NO-ERROR.
    IF (AVAIL hist) THEN NEXT.

    /* Если в налоговую отправлена дата из карточки счета, то пропускаем */
    IF (Date2NI("ДатаОткрСч") EQ acct.open-date) THEN NEXT.
    /* Если в НИ еще не отправили, но 3 дня еще не прошло, то пропускаем */
    IF (d2NiDate EQ ?) AND (d2NiSrok GT TODAY) THEN NEXT.

    RUN NextChange("*open-date*", acct.open-date).
    /* Если при создании счета была указана текущая дата создания, то пропускаем * /
    IF (dNewDate EQ history.modif-date) THEN NEXT. */
    /* Если в НИ еще не отправили, но дата открытия = сегодня, то пропускаем * /
    IF (d2NiDate EQ ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */

    RUN CreateTT("open-date").
END.

/* Исправление даты открытия счета ******************************************** */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'W')
      AND CAN-DO("*open-date*", history.field-value)
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "Транз")
    NO-LOCK
    BY history.modif-date:

    /* Если в налоговую отправлена дата из карточки счета, то пропускаем */
    IF (Date2NI("ДатаОткрСч") EQ acct.open-date) THEN NEXT.

    RUN NextChange("*open-date*", acct.open-date).
    RUN CreateTT("open-date").
END.

/* Исправление даты закрытия счета ******************************************** */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'W')
      AND CAN-DO("*close-date*", history.field-value)
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "Транз")
    NO-LOCK
    BY history.modif-date:

    /* Если в налоговую отправлена дата из карточки счета, то пропускаем */
    IF (Date2NI("ДатаЗакрСч") EQ acct.close-date) THEN NEXT.
    /* Если в НИ еще не отправили, но 3 дня еще не прошло, то пропускаем */
    IF (d2NiDate EQ ?) AND (d2NiSrok GT TODAY) THEN NEXT.

    RUN NextChange("*close-date*", acct.close-date).
    /* Если счет был закрыт текущей датой создания, то пропускаем * /
    IF (dNewDate NE ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */
    /* Если в НИ еще не отправили, но дата закрытия = сегодня, то пропускаем * /
    IF (d2NiDate EQ ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */

    RUN CreateTT("close-date").
END.

IF (iNum EQ 0)
THEN DO:
    RUN pb_mail.p (iMail, "Open / Close-date - OK !", "", "").
    {intrface.del}
    RETURN.
END.

cFl = STRING(YEAR(beg-date)) + STRING(MONTH(beg-date), "99") + STRING(DAY(beg-date), "99") + "-"
    + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "/tmp/acctoc-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/* Отчет ********************************************************************** */
PUT UNFORMATTED XLHead("open", "CCDDDDCDDCC", "59,150,118,118,115,124,131,71,71,170,190").
cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Счет",0,0,0)
    + XLCellHead("Дата открытия в карточке счета",0,0,0)
    + XLCellHead("Дата закрытия в карточке счета",0,0,0)
    + XLCellHead("Дата отправки сообщения в НИ",0,0,0)
    + XLCellHead("Дата открытия, сообщенная в НИ",0,0,0)
    + XLCellHead("Дата и время исправления",0,0,0)
    + XLCellHead("Было",0,0,0)
    + XLCellHead("Стало",0,0,0)
    + XLCellHead("Исправил сотрудник",0,0,0)
    + XLCellHead("Комментарий",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH ttIzm
    WHERE (ttIzm.cFild EQ "open-date")
    NO-LOCK
    BREAK BY ttIzm.cFil
          BY ttIzm.cAcct
          BY ttIzm.dIspr:

    cXL = XLCell(ttIzm.cFil)
        + XLCell(ttIzm.cAcct)
        + XLDateCell(ttIzm.dOpen)
        + XLDateCell(ttIzm.dClose)
        + XLCell(GetXAttrValue("acct", ttIzm.cSurr, "ДатаСообщЛС"))
        + XLDateCell(ttIzm.d2NI)
        + XLCell(STRING(ttIzm.dIspr, "99.99.9999  HH:MM:SS"))
        + XLDateCell(ttIzm.dOld)
        + XLDateCell(ttIzm.dNew)
        + XLCell(ttIzm.cUser)
        + XLCell(ttIzm.cComm)
        .
    PUT UNFORMATTED XLRow(IF FIRST-OF(ttIzm.cFil) THEN 1 ELSE 0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLNextList("close", "CCDDDDCDDCC", "59,150,118,118,115,124,131,71,71,170,190").
cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Счет",0,0,0)
    + XLCellHead("Дата открытия в карточке счета",0,0,0)
    + XLCellHead("Дата закрытия в карточке счета",0,0,0)
    + XLCellHead("Дата отправки сообщения в НИ",0,0,0)
    + XLCellHead("Дата закрытия, сообщенная в НИ",0,0,0)
    + XLCellHead("Дата и время исправления",0,0,0)
    + XLCellHead("Было",0,0,0)
    + XLCellHead("Стало",0,0,0)
    + XLCellHead("Исправил сотрудник",0,0,0)
    + XLCellHead("Комментарий",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH ttIzm
    WHERE (ttIzm.cFild EQ "close-date")
    NO-LOCK
    BREAK BY ttIzm.cFil
          BY ttIzm.cAcct
          BY ttIzm.dIspr:

    cXL = XLCell(ttIzm.cFil)
        + XLCell(ttIzm.cAcct)
        + XLDateCell(ttIzm.dOpen)
        + XLDateCell(ttIzm.dClose)
        + XLCell(GetXAttrValue("acct", ttIzm.cSurr, "ДатаСообщЗак"))
        + XLDateCell(ttIzm.d2NI)
        + XLCell(STRING(ttIzm.dIspr, "99.99.9999  HH:MM:SS"))
        + XLDateCell(ttIzm.dOld)
        + XLDateCell(ttIzm.dNew)
        + XLCell(ttIzm.cUser)
        + XLCell(ttIzm.cComm)
        .
    PUT UNFORMATTED XLRow(IF FIRST-OF(ttIzm.cFil) THEN 1 ELSE 0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой отчета проверим, запущен ли bispc * /
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/ * Отправляем протокол * /
RUN sndbispc.p ("file=" + cFl + ";class=bq").
*/

RUN pb_mail.p (iMail, "Otchet open/close-date", "", cFl).
OS-DELETE VALUE(cFl).
{intrface.del}

/* **************************************************************************** */
/* Ищем дату, на которую было заменено старое значение ************************ */
PROCEDURE NextChange:
    DEFINE INPUT  PARAMETER iMask   AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER iDate   AS DATE         NO-UNDO.

    /* Ищем ближайшее исправление */
    FIND FIRST hist
        WHERE (hist.file-name   EQ 'acct')
          AND (hist.field-ref   EQ history.field-ref)
          AND (hist.modify      EQ 'W')
          AND CAN-DO(iMask, hist.field-value)
          AND (hist.modif-date  GE history.modif-date)
          AND (hist.modif-date  GT history.modif-date
            OR hist.modif-time  GT history.modif-time)
        NO-LOCK NO-ERROR.
    IF (AVAIL hist)
    THEN DO: 
        cTmp     = ENTRY(LOOKUP(REPLACE(iMask, "*", ""), hist.field-value) + 1, hist.field-value).
        dNewDate = IF (cTmp EQ "" OR cTmp EQ "~003~004~005") THEN ? ELSE DATE(cTmp).
    END.
    ELSE dNewDate = iDate.  /* Если еще одной правки не было, то дата со счета */
END PROCEDURE.

/* ************************************** */
PROCEDURE CreateTT:
    DEFINE INPUT  PARAMETER iDat    AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE dOldDate        AS DATE         NO-UNDO.

    IF (LOOKUP(iDat, history.field-value) EQ 0)
    THEN dOldDate   = ?.
    ELSE DO:
        cTmp        = ENTRY(LOOKUP(iDat, history.field-value) + 1, history.field-value).
        dOldDate    = IF (cTmp EQ "" OR cTmp EQ "~003~004~005") THEN ? ELSE DATE(cTmp).
    END.

    IF (dNewDate EQ ? AND dOldDate EQ ?) THEN RETURN.

    FIND FIRST _user
        WHERE (_user._userid    EQ history.user-id)
        NO-LOCK NO-ERROR.
    CREATE ttIzm.
    ASSIGN
        ttIzm.cFil   = acct.filial-id
        ttIzm.cAcct  = acct.number
        ttIzm.cSurr  = acct.acct + "," + acct.currency
        ttIzm.dOpen  = acct.open-date
        ttIzm.dClose = acct.close-date
        ttIzm.d2NI   = d2NiDate
        ttIzm.dIspr  = DATETIME(history.modif-date, history.modif-time * 1000)
        ttIzm.cFild  = iDat
        ttIzm.dNew   = dNewDate
        ttIzm.dOld   = dOldDate
        ttIzm.cUser  = IF (AVAIL _user) THEN _user._user-name ELSE history.user-id
        ttIzm.cComm  = IF (d2NiDate NE ?) THEN "" ELSE (
                       IF (d2NiSrok EQ TODAY) THEN "Сегодня надо отправить в НИ" ELSE (
                       IF (d2NiSrok LT TODAY) THEN "Пропущена отправка в НИ"     ELSE ""))
        iNum         = iNum + 1.
        .
END PROCEDURE.

/* ************************************** */
FUNCTION Date2NI    RETURNS DATE
   (INPUT  iTxt     AS CHARACTER ).

    DEFINE VARIABLE I       AS INTEGER  NO-UNDO.

    d2NiDate = ?.
    FOR EACH PackObject
        WHERE (PackObject.file-name EQ 'acct')
          AND (PackObject.Surrogate EQ acct.acct + "," + acct.currency)
        NO-LOCK,
    FIRST Packet
        WHERE (Packet.PacketID      EQ PackObject.PacketID)
          AND (Packet.Class-Code    EQ "PTaxX")
          AND (Packet.mail-format   EQ IF (iTxt EQ "ДатаОткрСч") THEN "XFNSAcctOpen" ELSE "XFNSAcctClose")
        NO-LOCK,
    FIRST PacketText
        WHERE (PacketText.PacketID  EQ PackObject.PacketID)
        NO-LOCK:

        I = INDEX(PacketText.Contents, CODEPAGE-CONVERT(iTxt, "1251")).
        IF (I NE 0)
        THEN 
            d2NiDate = DATE(SUBSTRING(PacketText.Contents, I + 12, 10)) NO-ERROR.
    END.

    d2NiSrok = rAfterWorkDays(IF (iTxt EQ "ДатаОткрСч") THEN acct.open-date ELSE acct.close-date, 3).

    RETURN d2NiDate.
END FUNCTION.
