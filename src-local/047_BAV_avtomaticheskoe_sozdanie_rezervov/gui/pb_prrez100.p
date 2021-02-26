{globals.i}
{intrface.get tmess}

/* +++ pb_prrez100.p was humbly modified by (c)blodd converter v.1.11 on 5/4/2017 6:18am +++ */

/**
Авторские права принадлежат: ПАО Плюс банк
Основание:
Что делает:     Корректирует %Рез на счетах 47423
Как работает:
Параметры:      Дата опердня
Место запуска:  До тр-ции rsrv_pr51  (ДР nprrsrv)
Создан:         21.13.2016 Борисов А.В.
*/

{globals.i}
{pb_logit.i}
{intrface.get xclass}

DEF INPUT PARAM iBegDate AS DATE NO-UNDO.

DEFINE VARIABLE irez        AS INT64        NO-UNDO.
DEFINE VARIABLE nRate       AS DECIMAL      NO-UNDO.
DEFINE VARIABLE dRate       AS DATE         NO-UNDO.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.

cLog = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + "-" + STRING(TIME, "99999").
cLog = "/home2/bis/quit41d/log/rsrv/prrez-" + cLog + "-" + shFilial + ".log".
RUN LogIt("Запуск " + STRING(TODAY, "99.99.9999") + " на дату " + STRING(TODAY, "99.99.9999") + ", USER: " + USERID("bisquit"), cLog).

FOR EACH acct
    WHERE (acct.bal-acct    EQ 47423)
      AND CAN-DO("!КомВыд,*", acct.contract)
      AND (acct.close-date  EQ ?)
      AND (acct.filial-id   EQ shFilial)
    NO-LOCK:

    IF      (GetLinks("acctb", acct.acct + "," + acct.currency, "", "acct47423", "!", iBegDate) EQ "")    /* нет связи с р/с */
        AND (GetXAttrValue("acct", acct.acct + "," + acct.currency, "groupOABS") NE "591")
    THEN NEXT.

    FIND FIRST loan
        WHERE (loan.contract    EQ 'кредит')
          AND (loan.cust-cat    EQ acct.cust-cat)
          AND (loan.cust-id     EQ acct.cust-id)
          AND (loan.close-date  EQ ?)
        NO-LOCK NO-ERROR.
    IF (AVAIL loan) THEN NEXT.      /* Клиентов с кред.договорами пропускаем */

    irez  = 0.
    nRate = 0.
    FOR EACH comm-rate
        WHERE (comm-rate.acct       EQ acct.acct)
          AND (comm-rate.currency   EQ acct.currency)
          AND (comm-rate.commission EQ "%Рез")
          AND (comm-rate.since      LE iBegDate)
          AND (comm-rate.filial-id  EQ shFilial)
        NO-LOCK
        BREAK BY comm-rate.since DESC:

        irez = irez + 1.
        IF FIRST(comm-rate.since)
        THEN DO:
            nRate = comm-rate.rate-comm.
            dRate = comm-rate.since.
        END.
    END.

    RUN LogIt(acct.acct + ", %Рез = " + STRING(nRate, ">>9.99") + ", кол-во = " + STRING(irez), cLog).
    IF (nRate NE 0) THEN NEXT.

    IF (irez EQ 0) /* OR (dRate NE iBegDate) */
    THEN DO:
        CREATE comm-rate.
        ASSIGN
            comm-rate.commission    = "%Рез"
            comm-rate.rate-fixed    = NO
            comm-rate.acct          = acct.acct
            comm-rate.currency      = acct.currency
            comm-rate.since         = IF (irez EQ 0) THEN acct.open-date ELSE iBegDate
            comm-rate.rate-comm     = 100.0
            comm-rate.kau           = ""
            comm-rate.min-value     = 0.0
            comm-rate.period        = 0
            comm-rate.filial-id     = shFilial
            .
        RUN LogIt(STRING("", "x(30)") + ", Создан %Рез = 100.00 на " + STRING(comm-rate.since, "99.99.9999"), cLog).
/*      RUN LogIt(STRING("", "x(30)") + ", Создан %Рез = 100.00 на " + STRING(IF (irez EQ 0) THEN acct.open-date ELSE iBegDate, "99.99.9999"), cLog).
*/  END.
    ELSE DO:
        FOR LAST comm-rate
            WHERE (comm-rate.acct       EQ acct.acct)
              AND (comm-rate.currency   EQ acct.currency)
              AND (comm-rate.commission EQ "%Рез")
              AND (comm-rate.since      EQ dRate)
              AND (comm-rate.rate-comm  EQ 0)
              AND (comm-rate.filial-id  EQ shFilial)
            EXCLUSIVE-LOCK:

            comm-rate.rate-comm     = 100.0.
        END.
        RUN LogIt(STRING("", "x(30)") + ", Исправлен %Рез на " + STRING(dRate, "99.99.9999"), cLog).
    END.
END.
{intrface.del}
RETURN "yes".

/* --- pb_prrez100.p was humbly modified by (c)blodd converter v.1.11 on 5/4/2017 6:18am --- */
