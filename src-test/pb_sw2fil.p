/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Создает проводку по межфилу для перевода SWIFT
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
DEFINE VARIABLE cCorr       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAc01       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAc02       AS CHARACTER    NO-UNDO.
DEFINE BUFFER   opsw    FOR op.
DEFINE BUFFER   op2     FOR op.
DEFINE BUFFER   oesw    FOR op-entry.
DEFINE BUFFER   obsw    FOR op-bank.
DEFINE BUFFER   accr    FOR acct.
DEFINE BUFFER   c-nos2  FOR c-nostro.
DEFINE BUFFER   b-code  FOR banks-code.
DEFINE BUFFER   br2     FOR branch.

FOR EACH tmprecid 
    NO-LOCK,
FIRST opsw
    WHERE (RECID(opsw) EQ tmprecid.id)
      AND (opsw.op-status   = "ФСВО")
    EXCLUSIVE-LOCK,
FIRST oesw OF opsw
    WHERE (oesw.acct-cr     BEGINS "3030")
    NO-LOCK,
FIRST c-nostro
    WHERE (c-nostro.acct    = oesw.acct-cr)
    NO-LOCK,
FIRST banks-code
    WHERE (banks-code.bank-code      = c-nostro.nmfor)
      AND (banks-code.bank-code-type = c-nostro.code-nmfor)
    NO-LOCK,
FIRST branch
    WHERE (branch.bank-id   = banks-code.bank-id)
    NO-LOCK:

    cCorr = GetXAttrValue("op", STRING(opsw.op), "acctcorr").
    FIND FIRST accr
        WHERE (accr.acct   BEGINS cCorr)
          AND (accr.close-date  = ?)
        NO-LOCK NO-ERROR.
    IF (accr.filial-id = branch.branch-id)
    THEN DO:
        CREATE op.
        BUFFER-COPY opsw EXCEPT op TO op
            ASSIGN
                op.op-status        = "ФСВО"
                op.filial-id        = branch.branch-id
                op.branch-id        = branch.branch-id
                op.user-id          = USERID('bisquit')
                op.op-transaction   = NEXT-VALUE(op-transaction-id)
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
                op-entry.filial-id      = branch.branch-id
                op-entry.acct-db        = AddFilToAcct(c-nostro.corr-acct, branch.branch-id)
                op-entry.acct-cr        = accr.acct
                op-entry.user-id        = op.user-id
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
        RUN CreateLinks("opbct", "op-mf", STRING(opsw.op), STRING(iOp), TODAY, ?, "").
    END.
    ELSE DO:    /* В 2 приема через ГО */
        FOR EACH acct
            WHERE (acct.acct   BEGINS SUBSTRING(oesw.acct-cr,1,5))  /* 30301 или 30305 */
              AND (acct.currency    = oesw.currency)
              AND (acct.filial-id   = branch.branch-id)
            NO-LOCK,
        FIRST c-nos2
            WHERE (c-nos2.acct      = acct.acct)
            NO-LOCK,
        FIRST b-code
            WHERE (b-code.bank-code      = c-nos2.nmfor)
              AND (b-code.bank-code-type = c-nos2.code-nmfor)
            NO-LOCK,
        FIRST br2
            WHERE (br2.bank-id      = b-code.bank-id)
              AND (br2.branch-id    = accr.filial-id)
            NO-LOCK:

            cAc01 = acct.acct.
            cAc02 = AddFilToAcct(c-nos2.corr-acct, br2.branch-id).
        END.

        /* 1 */
        CREATE op.
        BUFFER-COPY opsw EXCEPT op TO op
            ASSIGN
                op.op-status        = "√"
                op.filial-id        = branch.branch-id
                op.branch-id        = branch.branch-id
                op.user-id          = USERID('bisquit')
                op.op-transaction   = NEXT-VALUE(op-transaction-id)
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
                op-entry.filial-id      = branch.branch-id
                op-entry.acct-db        = AddFilToAcct(c-nostro.corr-acct, branch.branch-id)
                op-entry.acct-cr        = cAc01
                op-entry.user-id        = op.user-id
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
        RUN CreateLinks("opbct", "op-mf", STRING(opsw.op), STRING(iOp), TODAY, ?, "").

        /* 2 */
        CREATE op.
        BUFFER-COPY opsw EXCEPT op TO op
            ASSIGN
                op.op-status        = "ФСВО"
                op.filial-id        = accr.filial-id
                op.branch-id        = accr.filial-id
                op.user-id          = USERID('bisquit')
                op.op-transaction   = NEXT-VALUE(op-transaction-id)
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
                op-entry.filial-id      = accr.filial-id
                op-entry.acct-db        = cAc02
                op-entry.acct-cr        = accr.acct
                op-entry.user-id        = op.user-id
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
        RUN CreateLinks("opbct", "op-mf", STRING(opsw.op), STRING(iOp), TODAY, ?, "").
    END.

    opsw.op-status = "√√".
    iSw = iSw + 1.
END.

RUN Fill-SysMes IN h_tmess ("","","1","Обработано " + STRING(iSw) + " документов.").
{intrface.del}
