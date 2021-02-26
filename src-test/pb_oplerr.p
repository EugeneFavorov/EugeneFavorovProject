{globals.i}             /** Глобальные определения */
{intrface.get netw}     /* Отправка в bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCrd  AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDtk  AS DATETIME  NO-UNDO.
DEFINE VARIABLE dD1   AS DATE      NO-UNDO.
DEFINE VARIABLE dD2   AS DATE      NO-UNDO.
DEFINE VARIABLE dDt1  AS DATETIME  NO-UNDO.
DEFINE VARIABLE dDt2  AS DATETIME  NO-UNDO.
DEFINE VARIABLE nSum  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lNoOp AS LOGICAL   NO-UNDO INIT YES.    /* Не найдено ошибочных документов */
{pb_oplerr.fun}
{getdates.i}

/******************************************* Реализация */
FOR EACH op
    WHERE (op.op-date           GE beg-date)
      AND (op.op-date           LE end-date)
      AND (op.op-kind       BEGINS "16m_")
      AND (op.filial-id         EQ shFilial)
    NO-LOCK,
FIRST op-entry OF op
    NO-LOCK,
LAST history
    WHERE (history.file-name    EQ 'op')
      AND (history.field-ref    EQ STRING(op.op))
      AND (history.modify       EQ 'C')
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ op-entry.acct-db)
      AND (acct.currency        EQ op-entry.currency)
    NO-LOCK
    BY op-entry.acct-db
    BY op.op-date:

    put screen col 1 row 24 "Обрабатывается " + op-entry.acct-db + " - " + STRING(op.op-date, "99.99.9999").
    dDtk = DATETIME(history.modif-date, history.modif-time * 1000).
    IF IsCart(op-entry.acct-db + "," + op-entry.currency, dDtk)
    THEN DO:
        IF lNoOp
        THEN DO:
            cFl = "/home2/bis/quit41d/log/oplerr/oplerr-" + shFilial
                + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
                + "-" + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".xml".
            OUTPUT TO VALUE(cFl).

            PUT UNFORMATTED XLHead("op", "DСCCNCDCNDC", "71,127,213,213,80,40,119,127,97,71,127").
            cXL = XLCellHead("Комиссия",0,0,4)
                + XLCellHead("Картотека",0,0,5)
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
            cXL = XLCellHead("Дата",0,0,0)
                + XLCellHead("Создан",0,0,0)
                + XLCellHead("Счет",0,0,0)
                + XLCellHead("Счет Кр",0,0,0)
                + XLCellHead("Сумма",0,0,0)
                + XLCellHead("Тип",0,0,0)
                + XLCellHead("Дата постановки",0,0,0)
                + XLCellHead("История",0,0,0)
                + XLCellHead("Сумма",0,0,0)
                + XLCellHead("Списано",0,0,0)
                + XLCellHead("История",0,0,0)
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.

        cXL = XLDateCell(op.op-date)
            + XLCell(STRING(dDtk, "99.99.9999 HH:MM:SS"))
            + XLCell(op-entry.acct-db)
            + XLCell(op-entry.acct-cr)
            + XLNumCell(op-entry.amt-rub)
            + XLCell(cCrd)
            + XLDateCell(dD1)
            + XLCell(STRING(dDt1, "99.99.9999 HH:MM:SS"))
            + XLNumCell(nSum)
            + XLDateCell(dD2)
            + XLCell(IF (dDt2 EQ ?) THEN "" ELSE STRING(dDt2, "99.99.9999 HH:MM:SS"))
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        lNoOp = NO.
    END.
END.

put screen col 1 row 24 color normal STRING(" ","X(80)").

/* Перед отправкой протокола проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
IF lNoOp
THEN MESSAGE "Нет списаний комиссий при наличии картотек" VIEW-AS ALERT-BOX.
ELSE DO:
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.

    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF (mRet EQ "")
        THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
    END.
    /* Отправляем протокол */
    RUN sndbispc.p ("file=" + cFl + ";class=bq").
END.
