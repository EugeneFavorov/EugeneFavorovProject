/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Откладывает перевод SWIFT на "завтра", подменяя счет Кр на 30220
Как работает:   по выделенным документам
Место запуска:  меню Ctrl-G
Создан:         16.02.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get tmess}

DEFINE VARIABLE iSw         AS INTEGER      NO-UNDO INIT 0.
DEFINE BUFFER   opsw    FOR op.
DEFINE BUFFER   oesw    FOR op-entry.

FOR EACH tmprecid 
    NO-LOCK,
FIRST opsw
    WHERE (RECID(opsw) EQ tmprecid.id)
      AND (opsw.op-status   = "ФСВО")
    EXCLUSIVE-LOCK,
FIRST oesw OF opsw
    WHERE (oesw.acct-cr     BEGINS "3011")
    EXCLUSIVE-LOCK:

    FIND FIRST acct
        WHERE (acct.bal-acct    = 30220)
          AND (acct.currency    = oesw.currency)
          AND (acct.filial-id   = opsw.filial-id)
        NO-LOCK NO-ERROR.
    oesw.acct-cr    = acct.acct.
    opsw.op-status  = "√".
    iSw = iSw + 1.
END.

RUN Fill-SysMes IN h_tmess ("","","1","Обработано " + STRING(iSw) + " документов.").
{intrface.del}
