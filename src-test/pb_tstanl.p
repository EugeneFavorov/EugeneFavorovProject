/**
Авторские права принадлежат: ПАО Плюс Банк
Что делает:     Проверка соответствия аналитики и остатка на Картотеке
Место запуска:  
Создан:         24.06.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{sh-defs.i}             /* остаток на счете */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{intrface.get netw}     /* Отправка в bispc */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE nSumKau     AS DECIMAL      NO-UNDO.
DEFINE VARIABLE nTmp        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cPr         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO INIT 0.

cFl = "./tstanl.xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("Картотеки", "CCNNC", "59,150,83,83,450").

cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Счет картотеки",0,0,0)
    + XLCellHead("Остаток",0,0,0)
    + XLCellHead("Аналитика",0,0,0)
    + XLCellHead("Суммы в бал.документах",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.acct    BEGINS "9090")
      AND CAN-DO("КартБл,Карт2", acct.contract)
      AND (acct.close-date  EQ ?)
      AND (acct.filial-id   NE "0400")
    NO-LOCK
    BY acct.filial-id
    BY acct.acct:

    put screen col 1 row 24 "Обрабатывается: " + acct.filial-id + " - " + acct.number.
    nSumKau = 0.
    cPr     = "".
    FOR EACH kau
        WHERE (kau.acct     EQ acct.acct)
          AND (kau.currency EQ acct.currency)
          AND (kau.zero-bal EQ no)
        NO-LOCK:

        nSumKau = nSumKau + kau.balance.
        cDR = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").
        FIND FIRST op-entry
            WHERE (op-entry.op  = INT64(cDR))
            NO-LOCK NO-ERROR.
        cDR  = GetXAttrValue("op", cDR, "amt-rub").
        nTmp = DEC(cDR) NO-ERROR.
        IF (AVAIL op-entry) AND (op-entry.amt-rub <> DEC(TRIM(ENTRY(3, kau.sort))))
        THEN cPr = cPr + (IF (cPr = "") THEN "" ELSE "~n")
                 + "В аналитике " + ENTRY(2, kau.sort) + "," + TRIM(ENTRY(3, kau.sort)) + " сумма бал.проводки = " + STRING(op-entry.amt-rub).
        IF (nTmp <> DEC(TRIM(ENTRY(3, kau.sort))))
        THEN cPr = cPr + (IF (cPr = "") THEN "" ELSE "~n")
                 + "В аналитике " + ENTRY(2, kau.sort) + "," + TRIM(ENTRY(3, kau.sort)) + " доп.рек. amt-rub = " + cDR.
    END.
    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "√").

    IF (sh-bal NE nSumKau) OR (cPr NE "")
    THEN DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLNumCell(sh-bal)
            + XLNumCell(nSumKau)
            + XLCellWrap(cPr)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        iNum = iNum + 1.
    END.
/*  ELSE DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLNumCell(sh-bal)
            + XLNumCell(nSumKau)
            + XLCell("OK")
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END. */
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

IF (iNum EQ 0)
THEN MESSAGE "Ошибок в картотеках не обнаружено."
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
ELSE DO:
    /* Перед отправкой протокола проверим, запущен ли bispc */
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
