/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      Письмо Русиновой Л.Г. и Половинкиной Е.В. от 25.06.2018
Что делает:     Проставляет ДР ppdoc = "2" при наличии кодов ВО
Место запуска:  ПроцедурыКонтр - ПостОбраб√
Создан:         25.06.2018 Борисов А.В.
*/

{intrface.get xclass}

DEFINE INPUT  PARAMETER iop     AS INT64    NO-UNDO.    /* Документ */
DEFINE INPUT  PARAMETER iParam  AS CHAR     NO-UNDO.
iParam = "~{VO" + REPLACE(iParam, ",", "}*,~{VO") + "}*".

FOR EACH op
    WHERE (op.op = iop)
      AND CAN-DO(iParam, op.details)
    NO-LOCK:

    UpdateSigns("op", STRING(iop), "ppdoc", "2", YES).
END.

{intrface.del}
