/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Проверяет, нужноли проводку печатать в выписке
Как работает:   Сравнивает статус op с НП Выписки / ВнешРасход - ОстПриход
Параметры:      op-entry, op, acct - определены во внешнем модуле
                &corr  - db/cr
                &label - внешний цикл по op-entry
Используется в: 
Создан:         01.01.2012 Борисов А.В.
*/

{&corr}StatusTst:
DO:
    FOR EACH half-entry OF op
        NO-LOCK:

        IF (half-entry.acct-{&corr} BEGINS "3")   /* Внешний */
        THEN
            IF (op.op-status LT (IF (((acct.side EQ "П") AND ("{&corr}" EQ "cr"))
                                  OR ((acct.side EQ "А") AND ("{&corr}" EQ "db"))) THEN FGetSetting("Выписки", "ВнешРасход", "")
                                                                                   ELSE FGetSetting("Выписки", "ВнешПриход", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.

        IF (half-entry.acct-{&corr} BEGINS "2")   /* Кассовый */
        THEN
            IF (op.op-status LT (IF (((acct.side EQ "П") AND ("{&corr}" EQ "cr"))
                                  OR ((acct.side EQ "А") AND ("{&corr}" EQ "db"))) THEN FGetSetting("Выписки", "КассРасход", "")
                                                                                   ELSE FGetSetting("Выписки", "КассПриход", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.
    END.

    /* Остальные */
    IF (op.op-status LT (IF (((acct.side EQ "П") AND ("{&corr}" EQ "cr"))
                          OR ((acct.side EQ "А") AND ("{&corr}" EQ "db"))) THEN FGetSetting("Выписки", "ОстРасход", "")
                                                                           ELSE FGetSetting("Выписки", "ОстПриход", "")))
    THEN NEXT {&label}.
END.
