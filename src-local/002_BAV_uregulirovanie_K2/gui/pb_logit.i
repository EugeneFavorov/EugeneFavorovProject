
/* +++ pb_logit.i was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:26am +++ */

/**
Авторские права принадлежат: ПАО "Плюс Банк"
Что делает:     Процедуры записи сообщений в протокол (как есть) и в лог (с пометкой даты и времени)
Создан:         25.03.2016 Борисов А.В.
*/

DEFINE STREAM log.
DEFINE STREAM prt.

/* Запись в лог-файл */
PROCEDURE Logit:
    DEFINE INPUT PARAMETER msg  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cLog AS CHARACTER NO-UNDO.

    IF (cLog NE "")
    THEN DO:
        OUTPUT STREAM log TO VALUE(cLog) APPEND.
        IF (msg EQ "")
        THEN PUT STREAM log UNFORMATTED " " SKIP.
        ELSE 
            PUT STREAM log UNFORMATTED STRING(TODAY, "99.99.9999") " " STRING(TIME, "HH:MM:SS") " - " msg SKIP.    
        OUTPUT STREAM log CLOSE.
    END.
END PROCEDURE.

/* Запись в протокол */
PROCEDURE ProtIt:
    DEFINE INPUT PARAMETER msg  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cPrt AS CHARACTER NO-UNDO.

    IF (cPrt NE "")
    THEN DO:
        OUTPUT STREAM prt TO VALUE(cPrt) APPEND.
        PUT STREAM prt UNFORMATTED msg.    
        OUTPUT STREAM prt CLOSE.
    END.
END PROCEDURE.

/* --- pb_logit.i was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:26am --- */
