
/* +++ e-ac950-tst.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:29am +++ */

/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Проверяет, нужноли проводку печатать в выписке
Как работает:   Сравнивает статус op с НП Выписки / ВнешРасход - ОстПриход
Параметры:      op-entry, op, acct - определены во внешнем модуле
                &corr  - db/cr
                &label - внешний цикл по op-entry
Используется в: 
Создан:         20.10.2016 Борисов А.В.
*/

{&corr}StatusTst:
DO:
    FOR EACH half-entry OF op
        NO-LOCK:

        IF (half-entry.acct-{&corr} BEGINS "3")   /* Внешний */
        THEN
            IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "П" &ELSE "А" &ENDIF )
                                 THEN FGetSetting("Выписки", "ВнешРасход", "")
                                 ELSE FGetSetting("Выписки", "ВнешПриход", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.

        IF (half-entry.acct-{&corr} BEGINS "2")   /* Кассовый */
        THEN
            IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "П" &ELSE "А" &ENDIF )
                                 THEN FGetSetting("Выписки", "КассРасход", "")
                                 ELSE FGetSetting("Выписки", "КассПриход", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.
    END.

    /* Остальные */
    IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "П" &ELSE "А" &ENDIF )
                         THEN FGetSetting("Выписки", "ОстРасход", "")
                         ELSE FGetSetting("Выписки", "ОстПриход", "")))
    THEN NEXT {&label}.
END.

/* --- e-ac950-tst.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:29am --- */
