/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.247 Автоматизация 91803* в БИСе
Что делает:     Отчет по счетам 47423 с закрытыми р/с
Место запуска:  
Создан:         23.05.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{sh-defs.i}
{intrface.get xclass}
{intrface.get netw}     /* Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cINN    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNpp    AS INTEGER   NO-UNDO.
DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRsrv   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRS     AS CHARACTER NO-UNDO.
DEFINE VARIABLE c23     AS CHARACTER NO-UNDO.
DEFINE VARIABLE d23     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE d25     AS DECIMAL   NO-UNDO.
DEFINE BUFFER  ac47423  FOR acct.
DEFINE BUFFER  ac47425  FOR acct.

cFl = STRING(YEAR(gend-date)) + STRING(MONTH(gend-date), "99") + STRING(DAY(gend-date), "99") + "-" + shFilial.
cFl = "acc47423-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("acct", "ICCCCCNNC", "35,88,88,350,161,150,110,110,244").

cXL = XLCellHead("№пп",0,0,0)
    + XLCellHead("Филиал",0,0,0)
    + XLCellHead("Код клиента",0,0,0)
    + XLCellHead("Наименование",0,0,0)
    + XLCellHead("Номер р/счета",0,0,0)
    + XLCellHead("Номер счета 47423",0,0,0)
    + XLCellHead("Сумма задолженности",0,0,0)
    + XLCellHead("Начисленные резервы",0,0,0)
    + XLCellHead("Комментарии",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

iNpp = 1.
/* По всем счетам 47423 "за РКО" с остатком */
FOR EACH ac47423
    WHERE (ac47423.bal-acct     EQ 47423)
      AND (ac47423.close-date   EQ ?)
      AND (ac47423.contract     EQ "")
      AND (ac47423.kau-id       EQ ""
        OR ac47423.kau-id       EQ ?)
      AND (ac47423.filial-id    EQ shFilial)
    NO-LOCK
    BREAK BY ac47423.filial-id
          BY ac47423.cust-cat
          BY ac47423.cust-id
          BY ac47423.acct:

    /* Ищем р/с по связи acct47423 */
    cRS     = GetLinks("acctb", ac47423.acct + "," + ac47423.currency, ?, "acct47423", "|", ?).
    IF (cRS EQ "") THEN NEXT.

    RUN acct-pos IN h_base(ac47423.acct, ac47423.currency, gend-date, gend-date, "√").
/*  IF (sh-bal EQ 0) THEN NEXT.
*/  d23 = sh-bal.

    DO I = 1 TO NUM-ENTRIES(cRS, "|"):
        IF (ENTRY(2, ENTRY(I, cRS, "|")) EQ "")
        THEN DO:
            cRS = ENTRY(I, cRS, "|").
            LEAVE.
        END.
    END.

    /* Закрыт ли р/с ? */
    FIND FIRST acct
        WHERE (acct.acct        EQ ENTRY(1, cRS))
          AND (acct.currency    EQ ENTRY(2, cRS))
          AND CAN-DO("405*,406*,407*,40802*", acct.acct)
/*        AND (acct.contract    EQ "Расчет")
*/        AND (acct.cust-cat    EQ ac47423.cust-cat)
          AND (acct.cust-id     EQ ac47423.cust-id)
          AND (acct.close-date  NE ?)
          AND (acct.filial-id   EQ ac47423.filial-id)
        NO-LOCK NO-ERROR.
    IF (NOT AVAIL acct) THEN NEXT.

    /* Ищем 47425 */
    cRsrv   = GetLinks("acct", ac47423.acct + "," + ac47423.currency, ?, "acct-reserve", "", ?).
    IF (NUM-ENTRIES(cRsrv) GE 2)
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cRsrv), ENTRY(2, cRsrv), gend-date, gend-date, "√").
        d25 = - sh-bal.
    END.
    ELSE d25 = 0.

    RUN GetCustName IN h_base (ac47423.cust-cat, ac47423.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).

    cXL = XLNumCell(iNpp)
        + XLCell(ac47423.filial-id)
        + XLCell(ac47423.cust-cat + "_" + STRING(ac47423.cust-id))
        + XLCellWrap(TRIM(cN1 + " " + cN2))
        + XLCell(acct.number)
        + XLCell(ac47423.number)
        + XLNumCell(d23)
        + XLNumCell(d25)
        + XLCellWrap(IF (d23 EQ d25) THEN "" ELSE "Несовпадение остатков 47423 и 47425")
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    iNpp = iNpp + 1.
END.

FOR EACH ac47423
    WHERE (ac47423.bal-acct     EQ 47423)
      AND (ac47423.close-date   NE ?)
      AND (ac47423.contract     EQ "")
      AND (ac47423.kau-id       EQ ""
        OR ac47423.kau-id       EQ ?)
      AND (ac47423.filial-id    EQ shFilial)
    NO-LOCK,
FIRST xlink
    WHERE (xlink.class-code     EQ 'acctb')
      AND (xlink.link-code      EQ 'acct-reserve')
      AND (xlink.link-direction BEGINS "s")
    NO-LOCK,
LAST links
    WHERE (links.link-id        EQ xlink.link-id)
      AND (links.source-id      EQ ac47423.acct + ',' + ac47423.currency)
    NO-LOCK,
FIRST ac47425
    WHERE (ac47425.acct         EQ ENTRY(1, links.target-id))
      AND (ac47425.currency     EQ ENTRY(2, links.target-id))
      AND (ac47425.close-date   EQ ?)
    NO-LOCK
    I = 1 TO 5:

    cRS = GetLinks("acctb", links.source-id, ?, "acct47423", "|", ?).
    RUN GetCustName IN h_base (ac47423.cust-cat, ac47423.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    RUN acct-pos IN h_base(ac47425.acct, ac47425.currency, gend-date, gend-date, "√").
    d25 = - sh-bal.

    cXL = XLNumCell(iNpp)
        + XLCell(ac47423.filial-id)
        + XLCell(ac47423.cust-cat + "_" + STRING(ac47423.cust-id))
        + XLCellWrap(TRIM(cN1 + " " + cN2))
        + XLCell(SUBSTRING(cRS, 1, 20))
        + XLCell(ac47423.number)
        + XLNumCell(0.0)
        + XLNumCell(d25)
        + XLCellWrap("47423 закрыт, а 47425 нет")
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    iNpp = iNpp + 1.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой протокола проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF mRet EQ "" THEN
       MESSAGE 
          "Запустите программу bispc и нажмите ОК"
       VIEW-AS ALERT-BOX.
END.

/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").

{intrface.del}
