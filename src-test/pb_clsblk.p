/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.1281
Что делает:     Отчет по блокировкам, закрытым в заданный период
Как работает:   
Параметры:      mode=auto|hand;block=Банкрот,БлокДб,БлокСумм,БлокТамож,КонкПр
Место запуска:  Планировщик 21:00, ОТЧЕТЫ ПО КАРТОТЕКАМ
Создан:         27.06.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get netw}     /** Отправка в bispc */
{sh-defs.i}
{parsin.def}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iMode       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iMail       AS CHARACTER    NO-UNDO.
iMode = GetParamByNameAsChar(iParam,"mode","auto").
iMail = GetParamByNameAsChar(iParam,"mail","a.borisov").

DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cName1      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cName2      AS CHARACTER    NO-UNDO.

IF (iMode = "auto")
THEN ASSIGN
    beg-date = TODAY
    end-date = TODAY.
ELSE DO:
    {getdates.i}
END.
cFl = STRING(YEAR(beg-date)) + STRING(MONTH(beg-date), "99") + STRING(DAY(beg-date), "99") + "-"
    + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./clsblk-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
RUN XLAddStyle('<Style ss:ID="s81red"><Font ss:Color="#FF0000"/><Alignment ss:Horizontal="Right"/></Style>').
PUT UNFORMATTED XLHead("ul", "ICCCCCNCCCDC", "40,59,54,300,150,106,85,123,147,127,133,167").

cXL = XLCellHat("Отчет по закрытым блокировкам, установленным на счетах клиентов банка за "
                + IF (beg-date = end-date) THEN STRING(beg-date, "99.99.9999")
                  ELSE ("период с " + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999")),11).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("N п/п",0,0,0)
    + XLCellHead("Филиал",0,0,0)
    + XLCellHead("Номер группы",0,0,0)
    + XLCellHead("Наименование клиента",0,0,0)
    + XLCellHead("Номер счета",0,0,0)
    + XLCellHead("Наименование блокировки",0,0,0)
    + XLCellHead("Сумма блокировки",0,0,0)
    + XLCellHead("Начало действия блокировки",0,0,0)
    + XLCellHead("Окончание действия блокировки",0,0,0)
    + XLCellHead("Дата закрытия блокировки",0,0,0)
    + XLCellHead("Дата последней операции по счету",0,0,0)
    + XLCellHead("ФИО сотрудника, закрывшего блокировку",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE CAN-DO("405*,406*,407*,40802*,40807*,40817*,40820*,423*,426*", acct.acct)
      AND (acct.close-date          = ?
        OR acct.close-date          > beg-date)
    NO-LOCK,
EACH blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND (blockobject.surrogate    = acct.acct + "," + acct.currency)
      AND (blockobject.end-datetime <> ?)
      AND CAN-DO("Банкрот,БлокДб,БлокСумм,БлокТамож,КонкПр", blockobject.block-type)
    NO-LOCK
    BY acct.filial-id
    BY blockobject.beg-datetime:

    FOR EACH history
        WHERE (history.file-name    = 'blockobject')
          AND (history.field-ref    = STRING(blockobject.blockobjectid))
          AND (history.modify       = 'W')
          AND CAN-DO("*end-datetime*", history.field-value)
          AND (history.modif-date   >= beg-date)
          AND (history.modif-date   <= end-date)
        NO-LOCK
        BY history.modif-date DESC
        BY history.modif-time DESC:

        RUN GetCustName IN h_base(acct.cust-cat, acct.cust-id, ?, OUTPUT cName1, OUTPUT cName2, INPUT-OUTPUT cXL).
        RUN acct-pos    IN h_base(acct.acct, acct.currency, beg-date, end-date, "√").
        FIND FIRST _user
            WHERE (_user._userid    = history.user-id)
            NO-LOCK NO-ERROR.
        I   = I + 1.
        cXL = XLNumCell(I)
            + XLCellWrapR(acct.filial-id)
            + XLCellWrapR(IF (acct.filial-id = "0500") THEN GetXAttrValue("acct", acct.acct + "," + acct.currency, "groupOABS") ELSE "")
            + XLCellWrap(cName1 + " " + cName2)
            + XLCell(acct.number)
            + XLCell(blockobject.block-type)
            + XLNumCell(- blockobject.val[3])
            + XLCellWrapR(STRING(blockobject.beg-datetime, "99.99.9999 HH:MM"))
            + (IF (DATE(blockobject.end-datetime) <> history.modif-date)                   THEN
              XLCellStyle("s81red", STRING(blockobject.end-datetime, "99.99.9999 HH:MM")) ELSE
              XLCellWrapR(STRING(blockobject.end-datetime, "99.99.9999 HH:MM")))
            + XLCellWrapR(STRING(DATETIME(history.modif-date, history.modif-time * 1000), "99.99.9999 HH:MM:SS"))
            + XLDateCell(IF (acct.currency = "") THEN lastmove ELSE lastcurr)
            + XLCell(IF (AVAIL _user) THEN _user._user-name ELSE history.user-id)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        LEAVE.
    END.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

IF (iMode = "auto")
THEN 
    IF (I = 0)
    THEN RUN pb_mail.p (iMail, "Segodnja net zakrytyh blokirovok", "", "").
    ELSE RUN pb_mail.p (iMail, "Otchet o zakrytyh blokirovkah", "", cFl).
ELSE DO:
    /* Перед отправкой отчета проверим, запущен ли bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF (mRet EQ "")
        THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
    END.
    /* Отправляем протокол */
    RUN sndbispc.p ("file=" + cFl + ";class=bq").
END.
{intrface.del}
