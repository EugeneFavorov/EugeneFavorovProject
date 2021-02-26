{globals.i}             /** Глобальные определения */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{intrface.get netw}     /* Отправка в bispc */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cOpBal      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDat        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iTmp        AS INTEGER      NO-UNDO.
DEFINE BUFFER   opb         FOR op.

cFl = "./tstkauop.xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("Картотеки", "CCDNC", "59,150,72,72,420").

cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Счет картотеки",0,0,0)
    + XLCellHead("Дата",0,0,0)
    + XLCellHead("Сумма",0,0,0)
    + XLCellHead("Ошибка",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.acct    BEGINS "9090")
      AND CAN-DO("КартБл,Карт2", acct.contract)
      AND (acct.close-date  EQ ?)
/*    AND (acct.filial-id   EQ shFilial)
*/    AND (acct.filial-id   NE "0400")
    NO-LOCK
    BY acct.filial-id
    BY acct.acct:

    put screen col 1 row 24 "Обрабатывается: " + acct.filial-id + " - " + acct.number.
    FOR EACH kau
        WHERE (kau.acct     EQ acct.acct)
          AND (kau.currency EQ acct.currency)
          AND (kau.zero-bal EQ no)
        NO-LOCK,
    FIRST op
        WHERE (op.op        EQ INT64(ENTRY(1, kau.kau)))
        NO-LOCK:

        cDat    = ENTRY(2, kau.sort).
        cDat    = ENTRY(3, cDat, ".") + "." + ENTRY(2, cDat, ".") + "." + ENTRY(1, cDat, ".").
        cOpBal  = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").

        FIND FIRST opb
            WHERE (opb.op   EQ INT64(cOpBal))
            NO-LOCK NO-ERROR.
        IF (cOpBal EQ "") OR (NOT AVAIL opb)
        THEN DO:
            cXL = XLCell(acct.filial-id)
                + XLCell(acct.number)
                + XLDateCell(DATE(cDat))
                + XLNumCell(DEC(ENTRY(3, kau.sort)))
                + XLCell("Отсутствует балансовый документ")
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
            iNum = iNum + 1.
        END.
        ELSE DO:
            iTmp = INDEX(op.details, "документа").
            IF (iTmp NE 0)
            THEN DO:
                cTmp = TRIM(TRIM(SUBSTRING(op.details, iTmp + 10), "."), "~n").
                IF (cTmp NE opb.doc-num)
                THEN DO:
                    cXL = XLCell(acct.filial-id)
                        + XLCell(acct.number)
                        + XLDateCell(DATE(cDat))
                        + XLNumCell(DEC(ENTRY(3, kau.sort)))
                        + XLCell("Ошибка: " + opb.doc-num + "//" + op.details)
                        .
                    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
                    iNum = iNum + 1.
                END.
            END.
        END.
    END.
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
