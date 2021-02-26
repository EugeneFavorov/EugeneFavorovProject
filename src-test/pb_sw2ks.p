/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Создает перевод SWIFT на к/с на основании перевода на 30220
Как работает:   по выделенным документам
Место запуска:  меню Ctrl-G
Создан:         16.02.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get instrum}  /** Функции для работы с курсами */
{intrface.get tmess}

DEFINE VARIABLE iOp         AS INT64        NO-UNDO.
DEFINE VARIABLE iSw         AS INTEGER      NO-UNDO INIT 0.
DEFINE BUFFER   opsw    FOR op.
DEFINE BUFFER   oesw    FOR op-entry.
DEFINE BUFFER   obsw    FOR op-bank.

FOR EACH tmprecid 
    NO-LOCK,
FIRST opsw
    WHERE (RECID(opsw) EQ tmprecid.id)
      AND (opsw.op-status   = "√")
    NO-LOCK,
FIRST oesw OF opsw
    WHERE (oesw.acct-cr     BEGINS "30220")
    NO-LOCK:

    CREATE op.
    BUFFER-COPY opsw EXCEPT op TO op
        ASSIGN
            op.op-status        = "ФСВО"
            op.op-transaction   = NEXT-VALUE(op-transaction-id)
            op.op-date          = TODAY
            op.doc-date         = TODAY
            op.ins-date         = TODAY
            op.op-value-date    = TODAY
            op.due-date         = TODAY
            op.contract-date    = TODAY
            op.user-id          = USERID('bisquit')
        NO-ERROR.
    iOp = op.op.
    RUN CopySigns IN h_xclass (opsw.class-code, STRING(opsw.op), opsw.class-code, STRING(iOp)) NO-ERROR.

    CREATE op-entry.
    BUFFER-COPY oesw TO op-entry
        ASSIGN
            op-entry.op             = iOp
            op-entry.op-entry       = 1
            op-entry.op-transaction = op.op-transaction
            op-entry.op-status      = op.op-status
            op-entry.op-date        = TODAY
            op-entry.value-date     = TODAY
            op-entry.amt-rub        = CurToBase("УЧЕТНЫЙ", op-entry.currency, TODAY, op-entry.amt-cur)
            op-entry.acct-db        = oesw.acct-cr
            op-entry.acct-cr        = AddFilToAcct(GetXAttrValue("op", STRING(iOp), "acctcorr"), shFilial)
        NO-ERROR.

    FOR EACH obsw OF opsw
        NO-LOCK:

        CREATE op-bank.
        BUFFER-COPY obsw EXCEPT op TO op-bank
            ASSIGN
                op-bank.op      = iOp
            NO-ERROR.
        UpdateSigns("op-bank", STRING(op-bank.op) + "," + op-bank.op-bank-type, "ver-form", "A", NO).
        UpdateSigns("op-bank", STRING(op-bank.op) + "," + op-bank.op-bank-type, "Cod-BIC", GetXAttrValue("op-bank", STRING(obsw.op) + "," + obsw.op-bank-type, "Cod-BIC"), NO).
    END.

    iSw = iSw + 1.
END.

RUN Fill-SysMes IN h_tmess ("","","1","Обработано " + STRING(iSw) + " документов.").
{intrface.del}

RUN CopySigns IN h_xclass ("op", STRING(opsw.op), "op", STRING(op.op)) NO-ERROR.
