/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Подсчет остатков для выписок с учетом таблицы статусов
Как работает:   Подставляет доп.проверку op-entry-check в программу acct-pos.i (в модуле acct-p0.i)
Параметры:      определены во внешнем модуле :
                op-entry   - проверяемая проводка,
                in-acct    - р/с, для которого считается остаток,
                half-entry - буфер для полупроводок,
                lDoNext    - признак пропуска проводки.
Место запуска:  
Создан:         27.10.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{sh-defs.i}
{zo-defs.i}

&SCOPED-DEFINE op-entry-check  /* Доп.проверка в цикле по проводкам для вычисления остатка */ ~
    StatusTst: ~
    DO: ~
        lDoNext = FALSE. ~
 ~
        FIND FIRST op OF op-entry ~
            NO-LOCK NO-ERROR. ~
        FIND FIRST acct ~
            WHERE (acct.acct    EQ in-acct) ~
            NO-LOCK NO-ERROR. ~
 ~
        IF (op-entry.acct-db EQ in-acct) ~
        THEN DO:                                    /* Дебетовая проводка */ ~
            FOR EACH half-entry OF op ~
                NO-LOCK: ~
 ~
                IF (half-entry.acct-cr BEGINS "3")  /* Внешний */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "П") THEN FGetSetting("Выписки", "ВнешРасход", "") ~
                                                               ELSE FGetSetting("Выписки", "ВнешПриход", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
 ~
                IF (half-entry.acct-cr BEGINS "2")  /* Кассовый */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "П") THEN FGetSetting("Выписки", "КассРасход", "") ~
                                                               ELSE FGetSetting("Выписки", "КассПриход", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
            END. ~
 ~
            /* Остальные */ ~
            IF (op.op-status LT (IF (acct.side EQ "П") THEN FGetSetting("Выписки", "ОстРасход", "") ~
                                                       ELSE FGetSetting("Выписки", "ОстПриход", ""))) ~
            THEN lDoNext = TRUE. ~
        END. ~
        ELSE DO:                                    /* Кредитовая проводка */ ~
            FOR EACH half-entry OF op ~
                NO-LOCK: ~
 ~
                IF (half-entry.acct-db BEGINS "3")  /* Внешний */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "А") THEN FGetSetting("Выписки", "ВнешРасход", "") ~
                                                               ELSE FGetSetting("Выписки", "ВнешПриход", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
 ~
                IF (half-entry.acct-db BEGINS "2")  /* Кассовый */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "А") THEN FGetSetting("Выписки", "КассРасход", "") ~
                                                               ELSE FGetSetting("Выписки", "КассПриход", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
            END. ~
 ~
            /* Остальные */ ~
            IF (op.op-status LT (IF (acct.side EQ "А") THEN FGetSetting("Выписки", "ОстРасход", "") ~
                                                       ELSE FGetSetting("Выписки", "ОстПриход", ""))) ~
            THEN lDoNext = TRUE. ~
        END. ~
    END. ~
    IF lDoNext THEN NEXT.

DEFINE VARIABLE lDoNext     AS LOGICAL  NO-UNDO.
DEFINE BUFFER   half-entry  FOR op-entry.

{acct-pos.i &Type = "acct-pos"}
