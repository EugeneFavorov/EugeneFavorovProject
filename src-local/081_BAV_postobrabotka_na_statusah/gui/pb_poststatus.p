{globals.i}
{intrface.get tmess}

/* +++ pb_poststatus.p was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am +++ */

/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ ОСРКО.511
Что делает:     Запускает процедуры пост обработки при смене статуса
Как работает:   
Параметры:      
Место запуска:  chst-opa.p
Создан:         06.04.2017 Борисов А.В.
*/

{intrface.get xclass}

DEFINE PARAMETER BUFFER iop     FOR op.

DEFINE VARIABLE cProc       AS CHARACTER    NO-UNDO.    /* Процедура пост обработки */
DEFINE BUFFER   grCode      FOR code. 

cProc = GetCodeDesc("Статус", iop.op-status, 2, "").

FOR FIRST code
    WHERE (code.class   EQ "ПроцедурыКонтр")
      AND (code.code    EQ cProc)
    NO-LOCK:

    IF {assigned code.val}
    THEN DO:    /* Процедура */
        RUN VALUE(code.val + ".p") (iop.op, code.description[1]).
    END.
    ELSE DO:    /* Группа процедур */
        FOR EACH grCode
            WHERE (grCode.class     EQ "ПроцедурыКонтр")
              AND (grCode.parent    EQ cProc)
              AND (grCode.val       NE "")
            NO-LOCK:

            RUN VALUE(grCode.val + ".p") (iop.op, grCode.description[1]).
        END.
    END.      
END.
{intrface.del}

/* --- pb_poststatus.p was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am --- */
