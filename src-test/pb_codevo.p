/**
Авторские права принадлежат: 
Базируется:     pr_chk_val.p
Основание:      СЗ ОВКиДО.3805, .3806, .3825
Что делает:     По классификатору КодыВОавт ищет код ВО, соответствующий документу
Место запуска:  Процедура контроля в классификаторе ПроцедурыКонтр
Создан:         31.08.2016 Борисов А.В.
*/

DEFINE PARAMETER BUFFER iop FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.   /* где происходит проверка: hand / <пусто> при смене статуса */
DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INIT YES.  /* Если ни одно правило не сработало, то смена статуса */

{globals.i}
{pb_logit.i}
{intrface.get xclass}

DEFINE VARIABLE iLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMess       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDetl       AS CHARACTER    NO-UNDO.
DEFINE BUFFER   acdb        FOR acct.
DEFINE BUFFER   accr        FOR acct.
/*
DEFINE BUFFER   oedb        FOR op-entry.
DEFINE BUFFER   oecr        FOR op-entry.
*/
iLog = "/home2/bis/quit41d/log/code_vo/" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + ".log".

FOR EACH op-entry OF iop
NO-LOCK:
    IF CAN-DO("30111*,40820*,426*", iop.ben-acct)
    THEN DO:
        FOR EACH code
            WHERE (code.class   = 'КодыВОавт')
              AND (code.parent  = 'КодыВОавт')
              AND (code.code    = "99")    /* Внешние платежи */
              AND (iParam       = ""
                OR code.val     <> "")
              AND CAN-DO(code.description[1], op-entry.acct-db)
              AND CAN-DO(code.description[2], op-entry.acct-cr)
            NO-LOCK:

            RUN ChangeVO.
            RETURN.
        END.
    END.

    FOR EACH code
        WHERE (code.class   = 'КодыВОавт')
          AND (code.parent  = 'КодыВОавт')
          AND (code.code    <> "99")    /* Внешние платежи - отдельно */
          AND (iParam       = ""
            OR code.val     <> "")      /* В ручном режиме по непустым кодам VO */
        NO-LOCK
        BY code.code:

        cDetl = IF (NUM-ENTRIES(code.name, "|") > 1) THEN ENTRY(2, code.name, "|") ELSE "*".
        IF      CAN-DO(ENTRY(1, code.description[1], "|"), op-entry.acct-db)
            AND CAN-DO(ENTRY(1, code.description[2], "|"), op-entry.acct-cr)
            AND CAN-DO(ENTRY(1, code.description[3], "|"), op-entry.currency)
            AND CAN-DO(cDetl, iop.details)
        THEN DO:

            IF (NUM-ENTRIES(code.description[1], "|") = 2)     /* Если в Дб указано Назначение счета */
            THEN DO:
                FIND FIRST acdb
                    WHERE (acdb.acct    = op-entry.acct-db)
                    NO-LOCK NO-ERROR.
                IF NOT CAN-DO(ENTRY(2, code.description[1], "|"), acdb.contract)
                THEN NEXT.
            END.

            IF (NUM-ENTRIES(code.description[2], "|") = 2)     /* Если в Кр указано Назначение счета */
            THEN DO:
                FIND FIRST accr
                    WHERE (accr.acct    = op-entry.acct-cr)
                    NO-LOCK NO-ERROR.
                IF NOT CAN-DO(ENTRY(2, code.description[2], "|"), accr.contract)
                THEN NEXT.
            END.

            IF      (NUM-ENTRIES(code.description[3], "|") = 2)
                AND (ENTRY(2, code.description[3], "|")    = "чужие")
            THEN DO:
                FIND FIRST acdb
                    WHERE (acdb.acct    = op-entry.acct-db)
                    NO-LOCK NO-ERROR.
                FIND FIRST accr
                    WHERE (accr.acct    = op-entry.acct-cr)
                    NO-LOCK NO-ERROR.
                IF      (acdb.cust-cat = accr.cust-cat)
                    AND (acdb.cust-id  = accr.cust-id)
                THEN NEXT.
            END.

            RUN ChangeVO.
            RETURN.
        END.
    END.
END.

{intrface.del}

/* Правка ВО */
PROCEDURE ChangeVO:
    DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cAuto       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE ibeg        AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iend        AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cVOn        AS CHARACTER    NO-UNDO.    /* код ВО для Назначения платежа */
    DEFINE VARIABLE cVOd        AS CHARACTER    NO-UNDO.    /* код ВО для ДР КодОпВал117 */
    DEFINE BUFFER   bop         FOR op.

    oResult = NO.   /* В случае ошибки - на валютный контроль */
    cAuto = ENTRY(1, code.name, "|").
    cMess = shFilial + " " + STRING(USERID("bisquit"),"x(9)") + " Док." + STRING(iop.op,">>>>>>>>9") + " от " + STRING(iop.op-date, "99.99.9999")
          + " (" + DelFilFromAcct(op-entry.acct-db) + " - " + DelFilFromAcct(op-entry.acct-cr)
          + ") : правило " + code.code + "-" + STRING(cAuto,"x(6)") + " : ".
    cDet  = TRIM(iop.details).
    ibeg  = INDEX (cDet, "~{").
    iend  = INDEX (cDet, "}").
    cVOn  = ENTRY(1, code.val, "|").
    cVOd  = IF (NUM-ENTRIES(code.val, "|") = 2) THEN ENTRY(2, code.val, "|") ELSE "".

    IF (cVOn = "*")
    THEN cMess = cMess + "Оставляем "
               + (IF ((ibeg = 0) OR (ibeg GE iend))
                  THEN "без кода VO"
                  ELSE ("код ~{" + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1)) + "}").
    ELSE DO:
        IF (cVOn NE "")
        THEN DO:
            /* ставим код ВО */
            IF (ibeg = 0) OR (ibeg GE iend)
            THEN DO:
                cMess = cMess + "Ставим код VO" + cVOn.
                cDet  = "~{VO" + cVOn + "} " + cDet.
            END.
            ELSE DO:
                IF (("~{VO" + cVOn) NE SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1))
                THEN DO:
                    cMess = cMess + "Меняем код " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1) + " на VO" + cVOn.
                    SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1) = "VO" + cVOn.
                END.
                ELSE cMess = cMess + "Оставляем код " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1).
            END.
        END.
        ELSE DO:
            /* Убираем код ВО */
            IF (ibeg NE 0) AND (ibeg LT iend)
            THEN DO:
                cMess = cMess + "Убираем код " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1).
                SUBSTRING(cDet, ibeg, iend - ibeg + 1) = "".
                cDet = TRIM(cDet).
            END.
            ELSE cMess = cMess + "Кода VO не было и не надо".
        END.
    END.

    IF (cDet NE iop.details)
    THEN DO:
        FOR FIRST bop 
            WHERE (bop.op   = iop.op)
            EXCLUSIVE-LOCK:

            bop.details = cDet.
        END.
    END.

    IF (cVOd NE "")     /* Надо проставить ДР КодОпВал117 */
    THEN DO:
        cDet = STRING(IF (op-entry.currency = "") THEN op-entry.amt-rub ELSE op-entry.amt-cur).
        cVOd = cVOd + ",," + cDet + ",0," + op-entry.currency + "," + cDet + ",1,1,Д,".
        UpdateSigns ("op", STRING(iop.op), "КодОпВал117", cVOd, NO).
        cMess = cMess + " | " + "Ставим ДР КодОпВал117 = " + cVOd.
    END.

    IF (code.code = "99")
    THEN cMess = cMess + " |" + iop.ben-acct + "|".

    cDet = SEARCH(iLog).
    RUN LogIt(cMess, iLog).
    IF (cDet = ?) THEN OS-COMMAND SILENT VALUE("chmod 666 " + iLog).   /* Если лог только что создан, разрешаем запись всем */

    oResult = (cAuto = "авто").
END PROCEDURE.