/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Исправляет удаленные документы ВОК, не попадающие в реестр
Как работает:   Устанавливает ДР "ИД смены", kau на проводке и создает Субаналитические проводки
Параметры:      Обрабатывает отобранные/помеченные документы, запрашивает N сметы
Место запуска:  ВОК - Документы - Ctrl-G - Исправление удаленных документов
Создан:         11.10.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get xclass}
{tmprecid.def}          /** Используем информацию из броузера */

DEFINE VARIABLE cSmena      AS CHARACTER    NO-UNDO.

RUN g-prompt.p ("integer", "Смена", ">>>>>>>9", ",",
                "Введите номер смены", 30, ",","",?,?,OUTPUT cSmena).
IF (cSmena EQ ?) THEN RETURN.

FOR EACH tmprecid 
    NO-LOCK,
FIRST op
    WHERE (RECID(op)        EQ tmprecid.id)
      AND (op.op-status     EQ "АС")
    NO-LOCK,
FIRST op-entry OF op
    WHERE (op-entry.acct-db BEGINS "20202")
      AND (op-entry.acct-cr BEGINS "20202")
    EXCLUSIVE-LOCK:

    cSmena = ENTRY(1, cSmena).
    op-entry.kau-db = cSmena.
    op-entry.kau-cr = cSmena.
    UpdateSigns ("op", STRING(op.op), "dpr-id", cSmena, YES).

    CREATE kau-entry.
    ASSIGN
        kau-entry.op        = op.op
        kau-entry.op-entry  = op-entry.op-entry
        kau-entry.kau-entry = 1
        kau-entry.kau       = cSmena
        kau-entry.acct      = op-entry.acct-db
        kau-entry.currency  = ""
        kau-entry.qty       = 0
        kau-entry.amt-cur   = 0
        kau-entry.amt-rub   = op-entry.amt-rub
        kau-entry.op-code   = "000000"
        kau-entry.contract-date = op.contract-date
        kau-entry.op-status = op.op-status
        kau-entry.kau-id    = "КодСменыВОК".
        kau-entry.value-date = op-entry.value-date.
        kau-entry.user-id   = op.user-id.
        .
    CREATE kau-entry.
    ASSIGN
        kau-entry.op        = op.op
        kau-entry.op-entry  = op-entry.op-entry
        kau-entry.kau-entry = 2
        kau-entry.kau       = cSmena
        kau-entry.acct      = op-entry.acct-cr
        kau-entry.currency  = op-entry.currency
        kau-entry.qty       = 0
        kau-entry.amt-cur   = op-entry.amt-cur
        kau-entry.amt-rub   = op-entry.amt-rub
        kau-entry.op-code   = "000000"
        kau-entry.contract-date = op.contract-date
        kau-entry.op-status = op.op-status
        kau-entry.kau-id    = "КодСменыВОК".
        kau-entry.value-date = op-entry.value-date.
        kau-entry.user-id   = op.user-id.
        .
END.
{intrface.del}          /* Выгрузка инструментария. */
