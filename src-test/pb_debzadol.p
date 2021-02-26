/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Отчет по списанию безнадёжной комиссии
Параметры:      Запрашивает даты отчета
Место запуска:  ОТЧЕТЫ ПО КАРТОТЕКАМ
Создан:         01.01.2018 Борисов А.В.
*/
{globals.i}             /** Глобальные определения */
{intrface.get netw}     /** Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{client.i}

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2   AS CHARACTER NO-UNDO.
{getdates.i}

cFl = "-" + STRING(YEAR(beg-date)) + STRING(MONTH(beg-date), "99") + STRING(DAY(beg-date), "99")
    + "-" + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./debzadol" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("0000", "CDCCNC", "300,71,150,150,70,770").
RUN OneFil("0000").
PUT UNFORMATTED XLNextList("0300", "CDCCNC", "300,71,150,150,70,770").
RUN OneFil("0300").
PUT UNFORMATTED XLNextList("0500", "CDCCNC", "300,71,150,150,70,770").
RUN OneFil("0500").
PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой отчета проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}

/******************************************* Процедуры */
PROCEDURE OneFil:
    DEFINE INPUT  PARAMETER iFil    AS CHARACTER    NO-UNDO.

    cXL = XLCellHead("Наименование",0,0,0)
        + XLCellHead("Дата",0,0,0)
        + XLCellHead("47425*",0,0,0)
        + XLCellHead("47423*",0,0,0)
        + XLCellHead("Сумма",0,0,0)
        + XLCellHead("Назначение",0,0,0)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

    FOR EACH op
        WHERE op.op-date    >= beg-date
          AND op.op-date    <= end-date
          AND op.op-kind    = "0723"
          AND op.doc-type   = "09"
          AND op.op-status  >= "√"
          AND op.filial-id  = iFil
        NO-LOCK,
    FIRST op-entry OF op
        NO-LOCK,
    FIRST acct
        WHERE acct.acct     = op-entry.acct-db
        NO-LOCK:

        RUN GetCustName IN h_base (acct.cust-cat, acct.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cXL).
        cXL = cN1 + " " + cN2.

        cXL = XLCellWrap(cXL)
            + XLDateCell(op.op-date)
            + XLCell(DelFilFromAcct(op-entry.acct-db))
            + XLCell(DelFilFromAcct(op-entry.acct-cr))
            + XLNumCell(op-entry.amt-rub)
            + XLCell(op.details)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
END PROCEDURE.
