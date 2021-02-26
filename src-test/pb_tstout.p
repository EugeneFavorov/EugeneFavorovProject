/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ 
Что делает:     Контроль внешних документов: дата док.и счет клиента
Место запуска:  Процедура контроля на входе ФБН
Создан:         16.08.2017 Борисов А.В.
*/

DEFINE PARAMETER BUFFER Op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INIT YES.  /* По умолчанию - смена статуса */

{globals.i}
{intrface.get tmess}

FOR FIRST op-entry OF op
    WHERE (op-entry.acct-cr BEGINS "30102")
    NO-LOCK:

    IF (op.doc-date > op.op-date)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "Дата документа больше даты опердня!").
        oResult = ?.
    END.
    ELSE IF (LENGTH(op.name-ben) > 160)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "Поле КЛИЕНТ > 160 символов!").
        oResult = ?.
    END.
    ELSE IF (op.ben-acct = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "3", "Счет клиента пустой.|Продолжить,Отменить").
        IF pick-value <> "1" THEN oResult = ?.
    END.
END.
{intrface.del}
RETURN IF (oResult = ?) THEN "Откат" ELSE "".
