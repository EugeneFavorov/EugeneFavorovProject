/**
Авторские права принадлежат: ОАО "Плюс Банк"
Базируется:     fill-graphp.p
Основание:      
Что делает:     Готовит на печать график платежей по кредиту Авто+ с учетом ЧДГ
Место запуска:  print-graphs.p 
Создан:         28.11.2017 Борисов А.В.
*/

{globals.i}
{svarloan.def new}

{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get instrum}  /* Библиотека для работы с фин. инструментами. */
{intrface.get loan}     /* Инструменты для работы с loan. */
{intrface.get loanc}
{intrface.get corr}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get date}
{intrface.get pogcr}    /* Библиотека инструментов для работы с графиками погашения в КиД. */

{fill-graphs.def}       /* Объявление вр.таблицы ttReport */
{pp-corr.p}

DEF INPUT PARAM iContract   AS CHAR     NO-UNDO.    /* Назначение договора */
DEF INPUT PARAM iContCode   AS CHAR     NO-UNDO.    /* Номер договора */
DEF INPUT PARAM iStart      AS DATE     NO-UNDO.    /* Дата начала графика */
DEF INPUT PARAM iDate       AS DATE     NO-UNDO.    /* Дата расчета договора */
DEF OUTPUT PARAM TABLE FOR ttReport.           /* Временная таблица */

/* Объявление переменных */
DEFINE VARIABLE cLoanSurr   AS CHAR     NO-UNDO.            /* Суррогат договора */
DEFINE VARIABLE cPayType    AS CHAR     NO-UNDO.            /* Тип условия */
DEF VAR mID                 AS INT64    NO-UNDO.            /* Порядковый номер строки (для расчета формул) */
DEFINE VARIABLE iKod        AS INT64    NO-UNDO.            /* Код (номер) операции */
DEFINE VARIABLE dDateVyd    AS DATE     NO-UNDO.            /* Дата выдачи кредита */
DEFINE VARIABLE dFirstPlan  AS DATE     NO-UNDO.            /* Дата первой строки из планового графика */
DEFINE VARIABLE nLoanSumm   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE nLoanRest   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE cOpLNPr     AS CHAR     NO-UNDO INIT "10,46,362,378".           /* Список операций оплаты непросроченных процентов, для подсчета корректировки */

DEFINE VARIABLE myLastDate  AS DATE     NO-UNDO.            /* Последняя дата графика */
DEFINE VARIABLE nProc       AS DECIMAL  NO-UNDO INIT 0.     /* Сумма фактически уплаченных %% */
DEFINE VARIABLE nFirstProc  AS DECIMAL  NO-UNDO INIT 0.     /* Платеж %% за Первый процентный период */
DEFINE VARIABLE nRKOcomm    AS DECIMAL  NO-UNDO INIT 0.     /* Платеж за РКО */
DEFINE VARIABLE dFirstPay   AS DATE     NO-UNDO.            /* Дата первого планового платежа (первая выплата процентов) */
DEFINE VARIABLE cTmp        AS CHAR     NO-UNDO.
DEFINE VARIABLE dT1         AS DECIMAL      NO-UNDO.
DEFINE VARIABLE dT2         AS DECIMAL      NO-UNDO.

DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE VARIABLE cSumOPR     AS CHARACTER    NO-UNDO EXTENT 8    /* Платежи по договору - сумма операций */
    INIT [ "5", "50,578", "46,100,720,409", "362,580,600,582", "478,361,378", "57,516", "526", "974,976,978"].
DEFINE VARIABLE dSumPlt     AS DECIMAL      NO-UNDO EXTENT 9.   /* Платежи по договору */
DEFINE VARIABLE cOpLAll     AS CHARACTER    NO-UNDO.            /* Все операции */
cOpLAll = cSumOPR[1] + "," + cSumOPR[2] + "," + cSumOPR[3] + "," + cSumOPR[4] + ","
        + cSumOPR[5] + "," + cSumOPR[6] + "," + cSumOPR[7] + "," + cSumOPR[8].

   /* Локализация буферов */
DEFINE BUFFER b-loan        FOR loan.
DEFINE BUFFER ins-loan      FOR loan.        /* договор страхования */
DEFINE BUFFER b-loan-cond   FOR loan-cond.
DEFINE BUFFER b-term-obl    FOR term-obl.
DEFINE BUFFER x-term-obl    FOR term-obl.
DEFINE BUFFER b-loan-acct   FOR loan-acct.
DEFINE BUFFER b-RepTable    FOR ttReport.
DEFINE BUFFER li100         FOR loan-int.
DEFINE BUFFER ch100         FOR chowhe.

/* Объявляем дополнительные временные таблицы */
DEFINE TEMP-TABLE tt-term-obl  LIKE term-obl.  /* Для idnt = 3 основной долг и idnt = 1 %% */
{fill-graphs.i}         /* Инструменты для рассчета ЭПС */

mFormGrKom = FGetSetting("ГрафКомН","ФормГрКом",?).

/* Очищаем вр.таблицу */
{empty tt-term-obl}
{empty ttReport}
mID = 1.

/* ****************************************************************************************************************************** */
FOR loan 
    WHERE (loan.contract    EQ iContract)
      AND (loan.cont-code   EQ iContCode)
    NO-LOCK:

    cLoanSurr   = iContract + "," + iContCode.
    myLastDate  = loan.open-date.

    /* Дата выдачи и сумма кредита ********************************************************************************************** */
    IF (loan.open-date = iStart)
    THEN DO:
        FOR EACH loan-int OF loan           /* По фактическим платежам */
            NO-LOCK,
        FIRST chowhe
            WHERE (chowhe.id-d  EQ loan-int.id-d)
              AND (chowhe.id-k  EQ loan-int.id-k)
              AND (chowhe.id-op EQ 4)
            NO-LOCK
            BY loan-int.mdate:

            nLoanRest   = loan-int.amt-rub.
            nLoanSumm   = nLoanRest.
            dDateVyd    = loan-int.mdate.
            LEAVE.
        END.
    END.
    ELSE DO:
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 0,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 2,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 13,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 7,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        nLoanRest   = nLoanSumm.
        dDateVyd    = iStart.
    END.

    RUN SetSysConf IN h_base ("СУММА_ДОГОВОРА", STRING(nLoanSumm)).
    RUN SetSysConf IN h_base ("ДАТА_ВЫДАЧИ",    STRING(dDateVyd, "99.99.9999")).

    /* Заносим в таблицу все реальные платежи ******************************************************************************* */
    FOR EACH loan-int OF loan
        WHERE (loan-int.mdate   LT iDate) /* До даты отчета */
          AND (loan-int.mdate   GE iStart)
        NO-LOCK,
    FIRST chowhe
        WHERE (chowhe.id-d      EQ loan-int.id-d)
          AND (chowhe.id-k      EQ loan-int.id-k)
          AND CAN-DO(cOpLAll, STRING(chowhe.id-op))
        NO-LOCK
        BREAK BY loan-int.mdate:

        IF FIRST-OF(loan-int.mdate) THEN dSumPlt = 0.0.

        DO I = 1 TO 8:
            IF CAN-DO(cSumOPR[I], STRING(chowhe.id-op))
            THEN DO:
                IF (chowhe.id-op = 100)
                THEN DO:    /* есть операция 10 на ту же сумму */
                    FOR EACH li100 OF loan
                        WHERE (li100.mdate      EQ loan-int.mdate)
                          AND (li100.amt-rub    EQ loan-int.amt-rub)
                        NO-LOCK,
                    FIRST ch100
                        WHERE (ch100.id-d       EQ li100.id-d)
                          AND (ch100.id-k       EQ li100.id-k)
                          AND (ch100.id-op      EQ 10)
                        NO-LOCK:
                    
                        dSumPlt[I] = dSumPlt[I] + loan-int.amt-rub.
                        LEAVE.
                    END.
                END.
                ELSE dSumPlt[I] = dSumPlt[I] + loan-int.amt-rub.
            END.
        END.

        IF CAN-DO(cOpLNPr, STRING(chowhe.id-op))
        THEN nProc = nProc + loan-int.amt-rub.

        /* Заполняем график */
        IF LAST-OF(loan-int.mdate)
        THEN DO:
            CREATE ttReport.
            ASSIGN
                ttReport.tf_id             = mID
                mID                        = mID + 1
                ttReport.tf_payment-date   = loan-int.mdate
                ttReport.tf_basic-sum-loan = dSumPlt[1]
                ttReport.tf_prosr-osn      = dSumPlt[2]
                ttReport.tf_sum-percent    = dSumPlt[3]
                ttReport.tf_prosr-proc     = dSumPlt[4]
                ttReport.tf_proc-pr-osn    = dSumPlt[5]
                ttReport.tf_peni           = dSumPlt[6]
                ttReport.tf_peni-pts       = dSumPlt[7]
                ttReport.tf_comm           = dSumPlt[8]
                ttReport.tf_sum-payment    = dSumPlt[1] + dSumPlt[2] + dSumPlt[3] + dSumPlt[4] + dSumPlt[5] + dSumPlt[6] + dSumPlt[7] + dSumPlt[8]
                nLoanRest                  = nLoanRest  - dSumPlt[1] - dSumPlt[2].
                ttReport.tf_rest-debts     = nLoanRest
                .
        END.
    END.

    /* Заносим в таблицу график плановых платежей начиная с сегодняшней даты ************************************************ */
    /* Копируем обязательства "как есть" */
    DO I = 1 TO 3:
        /* Плановый график начинаем с даты справки */
        RUN CopyTTData(loan.contract, loan.cont-code, I, iDate, loan.end-date).
    END.

    /* Добавляем данные в таблицу отчета ttReport из tt-term-obl  */
    dFirstPlan = ?.
    FOR EACH tt-term-obl
        WHERE tt-term-obl.idnt GE 1
          AND tt-term-obl.idnt LE 3
        BY tt-term-obl.end-date DESC:

        RUN CrtRepTbl(tt-term-obl.idnt,
                      tt-term-obl.end-date,
                      tt-term-obl.amt-rub).
        IF (tt-term-obl.idnt NE 2)      /* Остаток м.б.на дату условия */
        THEN dFirstPlan = tt-term-obl.end-date.
    END.

    /* Нумеруем строки и считаем суммы ежедневных платежей ****************************************************************** */
    FOR EACH ttReport
        WHERE (ttReport.tf_id = 0)
        NO-LOCK
        BY ttReport.tf_payment-date:

        ttReport.tf_sum-payment = ttReport.tf_sum-percent + ttReport.tf_basic-sum-loan.

        IF (ttReport.tf_sum-payment EQ 0)
        THEN DELETE ttReport.                  /* Убираем из графика пустые строки */
        ELSE ASSIGN
                ttReport.tf_id = mID
                mID                 = mID + 1.
    END.

    /* Корректировка %% - переписка с Коротаевой **************************************************************************** */
    IF (dFirstPlan NE ?)    /* Если есть что-то из планового графика */
    THEN DO:
        FOR EACH term-obl OF loan
            WHERE CAN-DO("1,3", STRING(term-obl.idnt))
              AND (term-obl.end-date    LT dFirstPlan)
            NO-LOCK
            BY term-obl.end-date DESC:  /* Предыдущая дата планового графика */

            FIND FIRST b-term-obl OF loan
                WHERE (b-term-obl.idnt      EQ 1)
                  AND (b-term-obl.end-date  EQ term-obl.end-date)
                NO-LOCK NO-ERROR.

            /* Если на предыдущую дату не планировались проценты, а мы их наверняка начислили за часть срока */
            IF (NOT AVAIL b-term-obl) OR (term-obl.amt-rub EQ 0)
            THEN DO:
                /* Считаем все проценты по плановому графику до предыдущей даты */
                
                FOR EACH x-term-obl OF loan
                    WHERE (x-term-obl.idnt      EQ 1)
                      AND (x-term-obl.end-date  LT dFirstPlan)
                    NO-LOCK:

                    nProc = nProc - x-term-obl.amt-rub. /* Сумма корректировки */
                END.

                FOR FIRST ttReport
                    WHERE (ttReport.tf_payment-date    EQ dFirstPlan)
                    NO-LOCK:

                    /* Корректировка */
                    ttReport.tf_sum-percent = ttReport.tf_sum-percent - nProc.
                    ttReport.tf_sum-payment = ttReport.tf_sum-payment - nProc.
                END.
            END.

            LEAVE.
        END.
    END.
END. /* FOR loan */

{intrface.del}
