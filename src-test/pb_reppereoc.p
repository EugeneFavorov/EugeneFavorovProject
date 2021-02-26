{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get instrum}  /** Функции для работы с курсами */
{intrface.get netw}     /** Отправка в bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCur  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nKur  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nDif  AS DECIMAL   NO-UNDO.

{getdate.i}
cFl = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./pereoc-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("acct", "CCCNNKNN", "60,150,57,115,115,60,100,124").

cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Счет",0,0,0)
    + XLCellHead("Валюта",0,0,0)
    + XLCellHead("Сумма в валюте",0,0,0)
    + XLCellHead("Сумма в рублях",0,0,0)
    + XLCellHead("Курс ЦБ",0,0,0)
    + XLCellHead("Разница",0,0,0)
    + XLCellHead("Сумма расчетная",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.close-date  = ?)
      AND (acct.filial-id   <> "0400")
      AND (acct.currency    <> "")
      AND CAN-DO("!60313*,!60314*,!7*,!.....810*,*", acct.acct)
    NO-LOCK
    BREAK BY acct.filial-id
          BY acct.currency
          BY acct.acct:

    put screen col 1 row 24 "Обрабатывается " + acct.acct.
    IF FIRST-OF(acct.currency)
    THEN DO:
        FIND FIRST currency
            WHERE (currency.currency    = acct.currency)
            NO-LOCK NO-ERROR.
        IF (AVAIL currency)
        THEN cCur = currency.i-currency.
        ELSE cCur = acct.currency.
        nKur = FindRate("УЧЕТНЫЙ", acct.currency, end-date).
    END.

    RUN acct-pos IN h_base(acct.acct, acct.currency, end-date, end-date, "√").
    nDif = ROUND(sh-val * nKur, 2) - sh-bal.

    IF (nDif <> 0.0)
    THEN DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLCell(cCur)
            + XLNumCell(sh-val)
            + XLNumCell(sh-bal)
            + XLNumCell(nKur)
            + XLNumCell(nDif)
            + XLNumCell(ROUND(sh-val * nKur, 2))
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

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
