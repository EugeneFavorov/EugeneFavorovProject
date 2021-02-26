/**
Авторские права принадлежат: ПАО Плюс банк
Базируется:     comgroup.p
Основание:      СЗ ОСРКО.511
Что делает:     Начисляет комиссию в конечном статусе на ПП
                с очередностью 1,2,3 и по счетам с блокировками Банкрот и КонкПр
Как работает:   
Параметры:      
Место запуска:  Пост-обработка при смене статуса
Создан:         07.04.2017 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get blkob}
{intrface.get tmess}
{topkind.def}

DEFINE INPUT  PARAMETER iop     AS INT64        NO-UNDO.    /* Документ */
DEFINE INPUT  PARAMETER iParam  AS CHARACTER    NO-UNDO.    /* <Комиссионная транзакция>;<маска Дб>;<маска Кр> */

DEFINE VARIABLE cBlock      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTranz      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lOk         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER    NO-UNDO.
DEFINE BUFFER   oe      FOR op-entry.

FOR FIRST op
    WHERE (op.op    EQ iop)
    NO-LOCK,
FIRST oe OF op
    WHERE CAN-DO(ENTRY(2, iParam, ";"), oe.acct-db)
      AND CAN-DO(ENTRY(3, iParam, ";"), oe.acct-cr)
    NO-LOCK,
FIRST history
    WHERE (history.file-name    EQ "op")
      AND (history.field-ref    EQ STRING(iop))
      AND (history.modify       EQ 'C')
    NO-LOCK:

    /* Если уже есть комиссия - пропускаем */
    IF (GetLinks(op.class-code, STRING(iop), ?, "КомГр", ",", op.op-date) NE "") THEN RETURN.

    cBlock = BlockAcct(oe.acct-db + "," + oe.currency, DATETIME(op.op-date, IF (op.op-date EQ TODAY) THEN TIME ELSE 0)).

    IF      CAN-DO("1,2,3", op.order-pay)
        OR  CAN-DO(cBlock, "Банкрот")
        OR  CAN-DO(cBlock, "КонкПр")
    THEN DO:
        cErrMsg = "Документ переведен в статус " + op.op-status + "~nНачисляем комиссию.".
        RUN Fill-SysMes IN h_tmess("", "", "1", cErrMsg).

        /* Запуск транзакции  УТ */
        {empty tOpKindParams}     /* очистить таблицу параметров */
        ASSIGN
            lOk =   TDAddParam("_OpMain", STRING(iop))
                AND TDAddParam("_OpDate", STRING(history.modif-date, "99.99.99"))
                AND TDAddParam("_OpTime", STRING(history.modif-time, "HH:MM:SS"))
            NO-ERROR.
        RUN ex-trans.p (ENTRY(1, iParam, ";"), op.op-date, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

        IF NOT lOk
        THEN DO:
            cErrMsg = "Ошибка: комиссия не начислена - " + cErrMsg.
            RUN Fill-SysMes IN h_tmess("", "", "-1", cErrMsg).
        END.
    END.
END.
{intrface.del}
