/**
Авторские права принадлежат: ОАО "Плюс Банк"
Базируется:     fill-graphp.p
Основание:      ОПОм.1211
Что делает:     Готовит на печать график платежей по кредиту Авто+ с учетом ЧДГ
Место запуска:  print-graphn.p 
Создан:         06.07.2016 Борисов А.В.
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

{fill-graphn.def}       /* Объявление вр.таблицы ttReportTable */
{pp-corr.p}

DEF INPUT PARAM iContract   AS CHAR     NO-UNDO.    /* Назначение договора */
DEF INPUT PARAM iContCode   AS CHAR     NO-UNDO.    /* Номер договора */
DEF INPUT PARAM iDate       AS DATE     NO-UNDO.    /* Дата расчета договора */
/*
DEF INPUT PARAM fullKasko   AS DECIMAL  NO-UNDO.
*/
DEF INPUT PARAM typeTable   AS CHAR     NO-UNDO.    /* 1- Авто, 2- Авто+Каско, 3 - МБ+, 4 - Авто+Каско(09.14) */
DEF INPUT PARAM ipayments   AS CHAR     NO-UNDO.    /* ALL - все, в тч и будущие,  доЧДГ - до даты ЧДГ или сегодняшней */
DEF OUTPUT PARAM TABLE FOR ttReportTable.           /* Временная таблица */

/* Объявление переменных */
DEFINE VARIABLE cLoanSurr   AS CHAR     NO-UNDO.            /* Суррогат договора */
DEFINE VARIABLE cPayType    AS CHAR     NO-UNDO.            /* Тип условия */
DEF VAR mID                 AS INT64    NO-UNDO INIT 1.     /* Порядковый номер строки (для расчета формул) */
DEF VAR mI                  AS INT64    NO-UNDO.
/*
DEF VAR rko11_price         AS INT64    NO-UNDO.
*/
DEFINE VARIABLE cOp         AS CHAR     NO-UNDO.            /* Вид операции по договору из chowhe */
DEFINE VARIABLE iKod        AS INT64    NO-UNDO.            /* Код (номер) операции */
DEFINE VARIABLE dDateVyd    AS DATE     NO-UNDO.            /* Дата выдачи кредита */
DEFINE VARIABLE dReal2Plan  AS DATE     NO-UNDO.            /* Дата перехода графика с реальных платежей на плановые */
DEFINE VARIABLE dFirstPlan  AS DATE     NO-UNDO.            /* Дата первой строки из планового графика */
DEFINE VARIABLE nLoanSumm   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE nLoanRest   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE cOpLOsn     AS CHAR     NO-UNDO INIT "5,50".                    /* Список операций оплаты осн.долга */
DEFINE VARIABLE cOpLPrc     AS CHAR     NO-UNDO INIT "10,46,361,362,378,478".   /* Список операций оплаты процентов */
DEFINE VARIABLE cOpLNPr     AS CHAR     NO-UNDO INIT "10,46,362,378".           /* Список операций оплаты непросроченных процентов, для подсчета корректировки */
DEFINE VARIABLE cOpLPr1     AS CHAR     NO-UNDO INIT "721". /* Список операций оплаты процентов за первый процентный период */
DEFINE VARIABLE cOpLDop     AS CHAR     NO-UNDO INIT "381,409,713,976,978".     /* Дополнительные платежи */
DEFINE VARIABLE cOpLAll     AS CHAR     NO-UNDO.            /* Все операции */
DEFINE VARIABLE cAllPrc     AS CHAR     NO-UNDO.            /* Все проценты */
cOpLAll = cOpLOsn + "," + cOpLPrc + "," + cOpLPr1 + "," + cOpLDop.
cAllPrc = cOpLPrc + "," + cOpLPr1.

DEFINE VARIABLE myLastDate  AS DATE     NO-UNDO.            /* Последняя дата графика */
DEFINE VARIABLE nProc       AS DECIMAL  NO-UNDO INIT 0.     /* Сумма фактически уплаченных %% */
DEFINE VARIABLE nFirstProc  AS DECIMAL  NO-UNDO INIT 0.     /* Платеж %% за Первый процентный период */
DEFINE VARIABLE nRKOcomm    AS DECIMAL  NO-UNDO INIT 0.     /* Платеж за РКО */
DEFINE VARIABLE dFirstPay   AS DATE     NO-UNDO.            /* Дата первого планового платежа (первая выплата процентов) */
DEFINE VARIABLE cTmp        AS CHAR     NO-UNDO.
DEFINE VARIABLE nTmp        AS DECIMAL  NO-UNDO.

   /* Локализация буферов */
DEF BUFFER b-loan       FOR loan.
DEF BUFFER ins-loan     FOR loan.        /* договор страхования */
DEF BUFFER b-loan-cond  FOR loan-cond.
DEF BUFFER b-term-obl   FOR term-obl.
DEF BUFFER x-term-obl   FOR term-obl.
DEF BUFFER b-loan-acct  FOR loan-acct.
DEF BUFFER b-RepTable   FOR ttReportTable.

/* Объявляем дополнительные временные таблицы */
DEF TEMP-TABLE tt-term-obl  LIKE term-obl.  /* Для idnt = 3 основной долг и idnt = 1 %% */

{fill-graphn.i}             /* Инструменты для рассчета ЭПС */

mFormGrKom = FGetSetting("ГрафКомН","ФормГрКом",?).

/* Очищаем вр.таблицу */
{empty tt-term-obl}
{empty ttReportTable}

/* ****************************************************************************************************************************** */
FOR loan 
    WHERE (loan.contract    EQ iContract)
      AND (loan.cont-code   EQ iContCode)
    NO-LOCK:

    cLoanSurr   = iContract + "," + iContCode.
    myLastDate  = loan.open-date.

    /* Дата выдачи и сумма кредита ********************************************************************************************** */
    dDateVyd    = loan.open-date.
    FOR EACH term-obl OF loan           /* По плановому графику */
        WHERE (term-obl.idnt  EQ 2)
        NO-LOCK
        BY term-obl.end-date:

        nLoanRest   = term-obl.amt-rub.
        nLoanSumm   = nLoanRest.
        dDateVyd    = term-obl.end-date.
        LEAVE.
    END.

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

    RUN SetSysConf IN h_base ("СУММА_ДОГОВОРА", STRING(nLoanSumm)).
    RUN SetSysConf IN h_base ("ДАТА_ВЫДАЧИ",    STRING(dDateVyd, "99.99.9999")).

    /* Первая плановая дата - первая уплата процентов *************************************************************************** */
    FOR EACH term-obl OF loan
        WHERE (term-obl.idnt    EQ 1)
          AND (term-obl.amt-rub NE 0)
        NO-LOCK
        BY term-obl.end-date:

        dFirstPay = term-obl.end-date.
        LEAVE.
    END.

    /* Поиск действующего условия (последнего, м.б.в будущем) ******************************************************************* */
    FOR EACH loan-cond
        WHERE loan-cond.contract    EQ loan.contract
          AND loan-cond.cont-code   EQ loan.cont-code
        NO-LOCK
        BY loan-cond.since DESCENDING:

        cPayType = GetXAttrValue("loan-cond", cLoanSurr + "," + STRING(loan-cond.since), "PayType").

        /* Заносим в таблицу все реальные платежи ******************************************************************************* */
        dReal2Plan = loan.open-date.    /* Дата перехода на плановые платежи - последний фактический платеж */
        FOR EACH loan-int OF loan
            WHERE IF (ipayments EQ "ALL") THEN TRUE ELSE
                  (loan-int.mdate   LE MINIMUM(loan-cond.since, TODAY)) /* До последнего условия или текущей даты */
            NO-LOCK,
        FIRST chowhe
            WHERE (chowhe.id-d      EQ loan-int.id-d)
              AND (chowhe.id-k      EQ loan-int.id-k)
              AND CAN-DO(cOpLAll, STRING(chowhe.id-op))
            NO-LOCK
            BREAK BY loan-int.mdate:

            cOp  = STRING(chowhe.id-op).
            iKod = IF CAN-DO(cOpLOsn, cOp) THEN   3 ELSE   (    /* Основной долг */
                   IF CAN-DO(cOpLPrc, cOp) THEN   1 ELSE  (     /* Проценты */
                   IF CAN-DO(cOpLPr1, cOp) THEN   4 ELSE (      /* Проценты за первый процентный период */
                   IF CAN-DO(cOpLDop, cOp) THEN 400 ELSE 0))).  /* Дополнительные платежи */
            IF (iKod NE 0)
            THEN DO:
                RUN CrtRepTbl(iKod,
                              loan-int.mdate,
                              loan-int.amt-rub).
                CASE iKod:
                    WHEN 4   THEN nFirstProc  = nFirstProc + loan-int.amt-rub.
                    WHEN 400 THEN nRKOcomm    = nRKOcomm   + loan-int.amt-rub.
                    WHEN 3   THEN nLoanRest   = nLoanRest  - loan-int.amt-rub.
                END CASE.
                IF CAN-DO(cOpLNPr, cOp)
                THEN nProc = nProc      + loan-int.amt-rub.
            END.

            /* Подсчитываем остаток ОД */
            IF LAST-OF(loan-int.mdate)
            THEN DO:
                RUN CrtRepTbl(2,
                              loan-int.mdate,
                              nLoanRest).
                dReal2Plan   = loan-int.mdate.
            END.
        END.

        /* Заносим в таблицу график плановых платежей начиная с сегодняшней даты ************************************************ */
        /* Копируем обязательства "как есть" */
        DO mI = 1 TO 3:
            /* Если сегодня были платежи, то плановый график начинаем с завтрашнего дня */
            RUN CopyTTData(loan.contract, loan.cont-code, mI, dReal2Plan + 1, loan.end-date).
        END.

        /* Добавляем данные в таблицу отчета ttReportTable из tt-term-obl  */
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

        /* Заносим в таблицу недоплаченные комиссии ***************************************************************************** */
        IF mFormGrKom EQ "Да"       /* Если график комиссий ведется на договоре */
        THEN DO:
            nTmp = 0.
            FOR EACH x-term-obl
                WHERE (x-term-obl.contract  EQ loan.contract)
                  AND (x-term-obl.cont-code EQ loan.cont-code)
                  AND (x-term-obl.idnt      EQ 10)
                  AND (x-term-obl.end-date  GE loan-cond.since)
                NO-LOCK:

                nTmp = nTmp + x-term-obl.amt-rub.
            END.

            /* Если недоплачено, добавляем в таблицу отчета */
            IF (nFirstProc LT nTmp)
            THEN DO:
                RUN CrtRepTbl (4,
                               dFirstPay,
                               nTmp - nFirstProc).
                nFirstProc = nTmp.
            END.
        END.

        /* Сумма комиссии за РКО ********************************************** */
        cTmp = GetXAttrValue("loan", cLoanSurr, "rko_comiss").

        IF (cTmp NE "")
        THEN nTmp = DEC(cTmp).
        ELSE DO:
            FOR EACH comm-rate
                WHERE comm-rate.commission EQ "%РКО"
                  AND CAN-DO(comm-rate.kau, loan.cont-code)
                  AND CAN-DO(comm-rate.kau, loan.contract)
                NO-LOCK
                BY comm-rate.since DESC:

                nTmp = comm-rate.rate-comm.
                LEAVE.  /* Комиссия за РКО есть в каждом условии. Берем последнюю */
            END.
        END.

        /* Если недоплачено, добавляем в таблицу отчета */
        IF (nRKOcomm LT nTmp)
        THEN DO:
            RUN CrtRepTbl (400,
                           dFirstPay,
                           nTmp - nRKOcomm).
            nRKOcomm = nTmp.
        END.

        /* Если даты dFirstPay не было в графике, нужно скорректировать остаток долга */
        FIND FIRST ttReportTable
            WHERE (ttReportTable.tf_payment-date    EQ dFirstPay)
            NO-LOCK NO-ERROR.
        IF (NOT AVAIL ttReportTable)
        THEN DO:
            /* Берем следующую дату графика - для страховок */
            FOR EACH b-RepTable
                WHERE (b-RepTable.tf_payment-date   GE dFirstPay)
                NO-LOCK
                BY b-RepTable.tf_payment-date:

                dFirstPay = b-RepTable.tf_payment-date.
                LEAVE.
            END.
        END.
        ELSE IF (ttReportTable.tf_rest-debts EQ 0)
        THEN DO:
            nTmp = nLoanSumm.
            FOR EACH b-RepTable
                WHERE (b-RepTable.tf_payment-date   LT dFirstPay)
                NO-LOCK
                BY b-RepTable.tf_payment-date DESC:

                nTmp = b-RepTable.tf_rest-debts.
                LEAVE.
            END.

            /* Берем предыдущую сумму из графика */
            RUN CrtRepTbl(2,
                          dFirstPay,
                          nTmp).
        END.

        /* Учет страховых платежей (обязательств) *********************************************************************************** */
        IF CAN-DO("1,2", typeTable)
        THEN DO:
            /* По всем договорам страхования */
            FOR EACH ins-loan
                WHERE  ins-loan.contract         EQ "Страх"
                  AND  ins-loan.parent-cont-code EQ iContCode
                  AND  ins-loan.parent-contract  EQ iContract
                  AND  ins-loan.open-date        LE iDate
                  AND (ins-loan.close-date       GE iDate
                    OR ins-loan.close-date       EQ ?)
                NO-LOCK,
            EACH term-obl OF ins-loan
                WHERE term-obl.idnt EQ 1
                NO-LOCK
                BREAK BY term-obl.end-date:

                /* Сохраняем сумму во временной таблице */
                RUN CrtRepTbl(5,
                              MAXIMUM(term-obl.end-date, dFirstPay),
                              term-obl.amt-rub).
            END. /* Учет страховых платежей */
        END.

        /* Нумеруем строки и считаем суммы ежедневных платежей ****************************************************************** */
        mID = 1.
        FOR EACH ttReportTable
            NO-LOCK
            BY ttReportTable.tf_payment-date:

            ttReportTable.tf_sum-payment =
                ttReportTable.tf_sum-percent
              + ttReportTable.tf_basic-sum-loan
              + (IF (typeTable EQ "3") THEN 0 ELSE (    /* У 3-его нет доп.платежей */
                ttReportTable.tf_additional-charge1
              + ttReportTable.tf_additional-charge2)).

            IF (ttReportTable.tf_sum-payment EQ 0)
            THEN DELETE ttReportTable.                  /* Убираем из графика пустые строки */
            ELSE ASSIGN
                    ttReportTable.tf_id = mID
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

                    FOR FIRST ttReportTable
                        WHERE (ttReportTable.tf_payment-date    EQ dFirstPlan)
                        NO-LOCK:

                        /* Корректировка */
                        ttReportTable.tf_sum-percent = ttReportTable.tf_sum-percent - nProc.
                        ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment - nProc.
                    END.
                END.

                LEAVE.
            END.

            IF (dFirstPlan GE TODAY)
            THEN dReal2Plan = TODAY.    /* Если весь плановый график в будущем, то дата создания графика - сегодня */
        END.
        ELSE dReal2Plan = TODAY.        /* Если весь график фактический, то КД закончен, и дата создания графика - сегодня */

        /* Завершение *********************************************************************************************************** */
        CASE typeTable:
            WHEN "1" THEN DO:
                FOR FIRST ttReportTable
                    WHERE (ttReportTable.tf_id EQ 1)
                    NO-LOCK:

                    ASSIGN
                        ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1  + ttReportTable.tf_sum-percent
                        ttReportTable.tf_sum-percent        = 0.
                        .
                END.
            END.
            WHEN "2" OR WHEN "4" THEN DO:
                /* Корректируем строки графика */
                FOR EACH ttReportTable
                    BY ttReportTable.tf_id:

                    /* "Первый %-ный период" переносим в колонку "Проценты" */
                    IF      (typeTable EQ "4")
                        AND (ttReportTable.tf_additional-charge1 NE 0)
                    THEN DO:
                        ASSIGN
                            ttReportTable.tf_sum-percent        = ttReportTable.tf_sum-percent + ttReportTable.tf_additional-charge1
                            ttReportTable.tf_additional-charge1 = 0
                            .
                    END.
                END.

                /* Формируем график процентов */
                cTmp  = "".
                nTmp = 0.
                FOR EACH comm-rate
                    WHERE (comm-rate.kau        EQ cLoanSurr)
                      AND (comm-rate.commission EQ "%Кред")
                    NO-LOCK
                    BY comm-rate.since:

                    IF (comm-rate.rate-comm NE nTmp)
                    THEN DO:
                        cTmp = cTmp + (IF (cTmp EQ "") THEN "с " ELSE "|с ")
                                    + STRING(comm-rate.since, "99.99.9999") + "г. - "
                                    + STRING(comm-rate.rate-comm, ">>9.99") + "%".
                        nTmp = comm-rate.rate-comm.
                    END.
                END.

                RUN SetSysConf IN h_base ("ГРАФИК_ПРОЦЕНТОВ", cTmp).
                RUN SetSysConf IN h_base ("ПОСЛЕДНЯЯ_ДАТА",   STRING(myLastDate, "99.99.9999")).
                RUN SetSysConf IN h_base ("ДАТА_ГРАФИКА",     STRING(dReal2Plan, "99.99.9999")).
            END.
        END CASE. /* CASE typeTable: */

        LEAVE.  /* Используем только последнее условие */
    END.  /* FOR EACH loan-cond */
END. /* FOR loan */

RUN DeleteOldDataProtocol IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
RUN DeleteOldDataProtocol IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").

{intrface.del}
