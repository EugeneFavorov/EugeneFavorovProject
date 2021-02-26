/**
Базируется:     cr-t-hist.p
Что делает:     Копирование текущих графиков в историю.
Как работает:   Сохраняет графики для всех действующих договоров на дату последнего условия,
                если такого графика нет в истории. Номер графика (ДР NumGr) вычисляется
                по порядковому номеру условия договора
Место запуска:  Планировщик
Создан:         27.03.2017 Борисов А.В.
*/

DEFINE INPUT  PARAMETER iAuto   AS CHARACTER    NO-UNDO.    /* Планировщик (AUTO) или вручную */

{globals.i}
{pb_logit.i}
{intrface.get xclass}

DEFINE VARIABLE cLog        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dLcDate     AS DATE      NO-UNDO.
DEFINE VARIABLE iLcNum      AS INT64     NO-UNDO.
DEFINE VARIABLE mTypeIdnt   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI          AS INT64     NO-UNDO.
DEFINE VARIABLE vIdnt       AS INT64     NO-UNDO.

IF FGetSetting ("ГрафИст", "СохрГрафИзм",?) NE "ДА"
THEN RETURN .

end-date  = TODAY - (IF (iAuto EQ "AUTO") THEN 1 ELSE 0).   /* Планировщик запускает утром следующего дня */
mTypeIdnt = FGetSetting ("ГрафИст", "ГрафТип","1,2,3").
cLog      = "/home2/bis/quit41d/log/to-hist/"
          + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99") + ".log".
RUN LogIt("Пуск", cLog).

FOR EACH loan
    WHERE (loan.contract    EQ "Кредит")
      AND (loan.open-date   LE end-date)
      AND (loan.close-date  EQ ?)
      AND (loan.loan-status NE "СОЗД")
      AND (loan.filial-id   NE "0400")
    NO-LOCK
    BY loan.filial-id
    BY loan.cont-code
    :

    iLcNum = 0.
    FOR EACH loan-cond
        WHERE (loan-cond.contract   EQ loan.contract)
          AND (loan-cond.cont-code  EQ loan.cont-code)
        NO-LOCK
        BY loan-cond.since:

        iLcNum  = iLcNum + 1.
        dLcDate = loan-cond.since.
    END.

    DO vi = 1 TO num-entries (mTypeIdnt):
        vIdnt = INT64(ENTRY (vi,mTypeIdnt)).
        RUN CreateHistHeader(vIdnt, dLcDate, iLcNum) NO-ERROR.
        IF ERROR-STATUS:ERROR
        THEN RUN LogIt("  ??? Ошибка: " + ERROR-STATUS:GET-MESSAGE(1), cLog).
    END.
END.

IF (iAuto NE "AUTO") THEN
put screen col 1 row 24 color normal STRING(" ","X(80)").
RUN LogIt("Стоп", cLog).
{intrface.del}
RETURN.

/* Сохранение одного графика ************************************************** */
PROCEDURE CreateHistHeader:
    DEFINE INPUT PARAMETER  iIdnt   AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER  iSince  AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER  iNumGr  AS INT64 NO-UNDO.

    DEFINE VARIABLE vnn     AS INT64 NO-UNDO INITIAL 0.

    FIND FIRST term-obl-hist
        WHERE term-obl-hist.contract    EQ loan.contract
          AND term-obl-hist.cont-code   EQ loan.cont-code
          AND term-obl-hist.idnt        EQ iIdnt
          AND term-obl-hist.since       EQ iSince
        NO-LOCK NO-ERROR .
    IF NOT AVAILABLE term-obl-hist
    THEN DO:
        RUN LogIt(STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + " график N " + STRING(iNumGr)
                + " idnt=" + STRING(iIdnt), cLog).
        IF (iAuto NE "AUTO") THEN
        put screen col 1 row 24 "Обрабатывается " + STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + STRING(" ","X(10)").

        CREATE term-obl-hist.
        ASSIGN
            term-obl-hist.contract      = loan.contract
            term-obl-hist.cont-code     = loan.cont-code
            term-obl-hist.idnt          = iIdnt
            term-obl-hist.olap          = "1"
            term-obl-hist.since         = iSince
            term-obl-hist.chtype        = '1'
            term-obl-hist.description   = "Автосохранение нового условия " + STRING(iSince)
            term-obl-hist.user-id       = USERID("bisquit")
            .
        UpdateSigns ("term-obl-hist", STRING(term-obl-hist.tobl-id), "DateSave", STRING(NOW), YES).
        UpdateSigns ("term-obl-hist", STRING(term-obl-hist.tobl-id), "NumGr", STRING(iNumGr), YES).

        /* Прочистка на всякий случай */
        FOR EACH tobl-hist-amt
            WHERE (tobl-hist-amt.tobl-id    EQ term-obl-hist.tobl-id)
            EXCLUSIVE-LOCK:

            DELETE tobl-hist-amt.
        END.

        /* копирование графика */
        FOR EACH term-obl
            WHERE term-obl.contract     EQ loan.contract
              AND term-obl.cont-code    EQ loan.cont-code
              AND term-obl.idnt         EQ iIdnt
            NO-LOCK:

            CREATE tobl-hist-amt.
            ASSIGN
                tobl-hist-amt.tobl-id      = term-obl-hist.tobl-id
                tobl-hist-amt.class-code   = "term-obl-hist"
                tobl-hist-amt.end-date     = term-obl.end-date
                tobl-hist-amt.dsc-beg-date = term-obl.dsc-beg-date
                tobl-hist-amt.amt-rub      = term-obl.amt-rub
                tobl-hist-amt.currency     = loan.currency
                vnn                        = vnn + 1
                tobl-hist-amt.nn           = vnn
                .
        END.

        RUN LogIt(STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + " график N " + STRING(iNumGr)
                + " - сохранено idnt=" + STRING(iIdnt) + " : " + STRING(vnn, ">>>9") + " строк", cLog).
    END.
END PROCEDURE.  /* CreateHistHeader */
