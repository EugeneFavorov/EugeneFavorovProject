/*                                               
        ОАО "Плюс банк"
      Comment: Расчет излишне начисленных процентов 
   Parameters:  Входящие: cContCode номер вклада; dateOp дата операционного дня; dateLastNach дата последнего начисления
               Исходящие: dPrev сумма излишне начисл за предыдущие годы; dCurr сумма излишне начисл за текущий год
         Uses:
      Used by: extrpars.fun
      Created: в году так 2013 KAM
     Modifier: 23/10/2014 KAU Рефакторинг кода 
                24/10/2014 KAU добавил функционал для изъятия процентов начиная со штрафного периода.
*/

    DEFINE INPUT PARAM cContCode    AS CHAR NO-UNDO.    /* номер вклада */
    DEFINE INPUT PARAM dateOp       AS DATE NO-UNDO.    /* дата операционного дня  */
    DEFINE INPUT PARAM dateLastNach AS DATE NO-UNDO.    /* дата последнего начисления  */
    DEFINE OUTPUT PARAM dPrev       AS DEC NO-UNDO.     /* сумма излишне начисл за предыдущие годы */
    DEFINE OUTPUT PARAM dCurr       AS DEC NO-UNDO.     /* сумма излишне начисл за текущий год */

    {globals.i}
    {intrface.get loan}
    {intrface.get i254}
    {intrface.get comm}
    {client.i}
    {tmprecid.def}
    {wordwrap.def}
    {navigate.def}
    {flt-file.i}
    {sh-defs.i}
    {ksh-defs.i new}
    {intrface.get dps}
    {dpsproc.def}
    {pp-corr.p}

    {svednew.i}

dPrev = 0.
dCurr = 0.

DEF VAR dPrevReal       AS DEC INIT 0 NO-UNDO.
DEF VAR dCurrReal       AS DEC INIT 0 NO-UNDO.

DEF VAR dateCurrYear    AS DATE     NO-UNDO.    /*дата начала года*/
DEF VAR ostatok         AS DEC      NO-UNDO.    /*Остаток срочного вклада*/
DEF VAR lastDate        AS DATE     NO-UNDO.    /*дата последней операции по счету*/
DEF VAR dateKap         AS DATE     NO-UNDO.    /*1) изначально смотрит просто дату операций. В дальнейшем считает ожидаемую дату капитализации*/
DEF VAR shtraf          AS INT64    NO-UNDO.    /*используется в case для опеределения распределения вклада по типу его штрафования*/ 
                                                /* 1 = Для вкладов, у которых при любом снятии будет штрафная ставка */
                                                /* 2 = Для вкладов, у которых при снятии до первой капитализации будет штрафная ставка */
                                                /* 3 = Для вкладов, у которых штрафная ставка зависит от даты снятия */
                                                /* 4 = Для типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1%, далее штрафная ставка зависит от даты снятия */
                                                /* 5 = Пенсионный */
DEF VAR snatie          AS LOG      NO-UNDO.    /*флаг последняя операция по вкладу довнесение или снятие*/
DEF VAR osnComm         AS CHAR     NO-UNDO.    /*код основной комиссии вклада*/
DEF VAR shtrComm        AS CHAR     NO-UNDO.    /*код штрафной комиссии вклада*/
DEF VAR summProc        AS DEC      NO-UNDO.
DEF VAR raschProc       AS DEC      NO-UNDO.
DEF VAR acct42          AS CHAR     NO-UNDO.    /*основной счет вклада*/
DEF VAR acct474         AS CHAR     NO-UNDO.    /*счет процентов привязанный к вкладу*/
DEF VAR acct706         AS CHAR     NO-UNDO.    /*счет расходов привязанный к вкладу*/
DEF VAR acctTr          AS CHAR     NO-UNDO.    /*счет для перечисления процентов*/
DEF VAR notIndivid      AS LOG      NO-UNDO.    /*флаг нулевой суммы документа.... а в другом месте флаг отсутствия индивидуального тарифа для счета вклада*/
DEF VAR pPeriod         AS DEC      NO-UNDO.
DEF VAR fullPeriod      AS DEC      NO-UNDO.
DEF VAR periodSnatie    AS DEC      NO-UNDO.
DEF VAR perShtraf       AS DEC      NO-UNDO.
DEF VAR stavkaShtraf    AS DEC      NO-UNDO.    /*процент штрафной ставки для вклада для периода в котором изымаем*/
DEF VAR stavkaOsnovn    AS DEC      NO-UNDO.
DEF VAR lastOstatok     AS DEC      NO-UNDO.
DEF VAR lastOstatokRasch AS DEC     NO-UNDO.    /*остаток после предыдущей операции без учета начислений*/
DEF VAR tempDate        AS DATE     NO-UNDO.    /*переменная в которую запихиваем всё подряд*/
DEF VAR tempDate2       AS DATE     NO-UNDO.    /*просто дата которую мы используем когда не можем использовать tempDate*/
DEF VAR daysInYear      AS INT64    NO-UNDO.    /*количество дней в году*/
DEF VAR myCur           LIKE loan.currency NO-UNDO.
DEF VAR firstOstatok    AS DEC      NO-UNDO.    /*остаток вклада в день открытия*/
DEF VAR prich474_42     AS LOG      NO-UNDO.    /*флаг наличие хотябы одного причисления на счет вклада*/
DEF VAR amt_rub_cur     AS DEC      NO-UNDO.    /*сумма именно этой проводки для внесения в таблицу tt-dviacct*/
DEF VAR procRaschAdd    AS LOG      NO-UNDO.    /*флаг участия документа в расчете процентов FALSE - уже участвует*/
DEF VAR dateFirstSnatie AS DATE     NO-UNDO.    /*Дата первого штрафного изъятия вклада*/
DEF VAR status_vkl      AS CHAR     NO-UNDO.    /*статус отобранного вклада*/
DEF VAR countdps_t      AS INT64    NO-UNDO.    /*количество основных счетов вклада у договора*/
DEF VAR isVost          AS LOG      NO-UNDO.    /*необходимо ли брать у данного вклада ставку ДВ*/
DEF VAR tmpOstatok      AS DEC      NO-UNDO.    /*сюда ложим остаток основного счета на дату каждой операции по нему*/
   
/* в tt-dviacct - разбитие периодов по отличающимся суммам */
DEF TEMP-TABLE tt-dviacct
    FIELD cont-code LIKE loan.cont-code /*Номер договора вклада*/
    FIELD cont-type LIKE loan.cont-type /*тип вклада*/
    FIELD ostatokAcct AS DEC INIT 0     /*Остаток счета исходящий*/
    FIELD ostatokAcct-Proc AS DEC INIT 0 /*Остаток счета минус проценты*/
    FIELD cur LIKE acct.currency        /*валюта вклада*/
    FIELD acct LIKE acct.acct           /*счет вклада, тут пока что только основной счет вклада бывает*/
    FIELD debet AS LOG INIT FALSE       /*здесь флаг этой операции. Снятие TRUE*/
    FIELD posDate AS DATE               /*дата операций по договору*/
    FIELD nachProc AS LOG INIT FALSE    /*флаг того что в этот день было начисление процентов*/
    FIELD amt_rub_cur AS DEC INIT 0     /*сумма операций по этому договору в этом дне*/
    .

/* в tt-stavka - разбитие периодов по отличающимся суммам и отличающимся ставкам */ 
DEF TEMP-TABLE tt-stavka
    FIELD cont-code LIKE loan.cont-code     /*номер договора вклада*/
    FIELD cont-type LIKE loan.cont-type     /*тип вклада*/
    FIELD ostatokAcct AS DEC                /*остаток счета на дату*/
    FIELD ostatokAcctRasch AS DEC           /*остаток который должен быть*/
    FIELD acct LIKE acct.acct               /*счет вклада*/
    FIELD startDate AS DATE                 /*дата предыдущего документа*/
    FIELD endDate AS DATE                   /*дата данного документа*/
    FIELD closeDate AS DATE                 /*дата закрытия вклада*/
    FIELD period AS DEC                     /*разница между датой предыдущего документа и датой нынешнего документа*/
    FIELD cur LIKE acct.currency            /*валюта вклада*/
    FIELD stavkaOsn LIKE comm-rate.rate-comm    /*% ставки основной*/
    FIELD stavkaShtr LIKE comm-rate.rate-comm   /*% ставки штрафной*/
    FIELD procRasch AS DEC                  /*расчитанные проценты за период*/
    FIELD shtraf AS INT64                   /*тип штрафного изъятия с вклада*/
    FIELD summProc AS DEC                   /*сумма начисленных процентов. Реальная за всё время*/
    FIELD daysInYear AS INT64               /*количество дней в году в котором движение по вкладу*/
    FIELD fullPeriod AS INT64               /*весь период по вкладу на день документа*/
    FIELD nachProc AS LOG INIT FALSE        /*было ли в этот день начисление процентов*/
    FIELD procRaschAdd AS LOG INIT FALSE        /*флаг участия документа в расчете процентов FALSE - уже участвует, TRUE в близжайшее начисление начнёт участвовать*/
    .

/* в tt-comm-rate заполняются ставки по данному вкладу по всем периодам по всем типам штрафов*/    
DEF TEMP-TABLE tt-comm-rate     
    FIELD rate-comm LIKE comm-rate.rate-comm    /*процент ставки*/
    FIELD period LIKE comm-rate.period          /*период*/
    FIELD shtraf AS INT64 INIT 0                /*тип штрафа 0 - оснавная*/
    .                                           /*1 - штрафная*/
                                                /*2 - вост*/
    
FIND FIRST loan WHERE loan.contract  EQ "dps"
                        AND loan.cont-code = cContCode NO-LOCK NO-ERROR.

IF AVAIL loan THEN DO:
    
    {empty tt-dviacct}
    {empty tt-stavka}
    {empty tt-comm-rate}

    dateCurrYear = DATE(1, 1, YEAR(dateOp)).
    tempDate = dateOp.
    dateOp = dateOp .
    IF LOOKUP(loan.cont-type,listContType) > 0  
        AND loan.open-date < dateOp THEN DO:
            /*считаем количество счетов если больше 1, необходим ручной контроль*/
        SELECT COUNT(*) INTO countdps_t FROM loan-acct WHERE loan-acct.cont-code = loan.cont-code AND loan-acct.acct-type = 'loan-dps-t'.
      IF countdps_t = 1 THEN DO: 
        status_vkl = loan-status.
        acct474 = ''.
        acct706 = ''.
        acctTr = ''.
        summProc = 0.
        lastOstatok = 0.
        lastOstatokRasch = 0.
        prich474_42 = FALSE.
        dateFirstSnatie = loan.open-date.
        shtraf = 0.
        RUN get-summ-beg-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT firstOstatok).
        
        /*   Счет пересчисления процентов */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-dps-tr"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acctTr = loan-acct.acct.
        END.
        
        /*   Счет % 474   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-dps-int"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acct474 = loan-acct.acct.
        END.
        
        /*   Счет % 706   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-expens"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acct706 = loan-acct.acct.
        END.
        
        /*   Счет депозита   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
            AND loan-acct.cont-code = loan.cont-code
            AND loan-acct.acct-type = "loan-dps-t"
            NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
            IF AVAIL acct THEN DO:
                acct42 = acct.acct.
                ostatok = 0.
                lastDate = loan.open-date.
                dateKap = lastDate.

                /*смотрим документы по основному счету вклада
                Опеределяем все операции по основному счету вклада и разбрасываем их по дням*/                                
                FOR EACH op-entry WHERE 
                        (op-entry.op-status = "√" OR op-entry.op-status = "√√")
                        AND (op-entry.acct-db = acct.acct OR op-entry.acct-cr = acct.acct),
                FIRST op OF op-entry WHERE op.contract-date >= lastDate 
                        AND op.contract-date <= dateOp
                NO-LOCK BY op.contract-date:
                    amt_rub_cur = 0.
                    dateKap = op.contract-date.
                    notIndivid = TRUE.
                    
                    IF (op-entry.acct-db = acct.acct) THEN DO: /*если изъятие со вклада*/
                         IF SUBSTRING(op-entry.acct-cr,1,3) <> '706' THEN  DO:  /* не считаем исправительные проводки возврат % */
                            IF TRIM(loan.currency) = '' THEN DO:
                                ostatok  = ostatok - ABS(op-entry.amt-rub). 
                                amt_rub_cur = ABS(op-entry.amt-rub).
                            END.
                            ELSE DO:
                                IF op-entry.amt-cur = 0 THEN notIndivid = FALSE.
                                ostatok  = ostatok - abs(op-entry.amt-cur).
                                amt_rub_cur = abs(op-entry.amt-cur).
                            END.
                            snatie = TRUE.
                        END.
                    END.
                    ELSE DO: /*если довнесение или начисление на вклад*/
                        IF (op-entry.acct-db = acct474) THEN DO:
                            prich474_42 = TRUE.
                            /*TODO для точности лучше заменить конец месяца - 4 дня*/
                            IF DAY(dateKap) > 24 THEN DO: /* если в конце месяца, то скорее всего начисл. */
                                dateKap = DATE_CORRECT(MONTH(dateKap),0,31,YEAR(dateKap)). /*возвращает последний день месяца*/
                                amt_rub_cur = 0.
                            END.
                        END.
                        ELSE DO:
                            IF loan.currency = '' THEN DO:
                                ostatok  = ostatok + abs(op-entry.amt-rub).
                                amt_rub_cur = abs(op-entry.amt-rub).
                            END.
                            ELSE DO:
                            IF op-entry.amt-cur = 0 THEN notIndivid = FALSE.
                            ostatok  = ostatok + ABS(op-entry.amt-cur).
                            amt_rub_cur = ABS(op-entry.amt-cur).
                            END.
                        END.
                        snatie = FALSE.
                    END.
                 
                    IF notIndivid THEN DO:  /* если сумма проводки не 0 */
                        FIND FIRST tt-dviacct WHERE tt-dviacct.cont-code = loan.cont-code
                            AND tt-dviacct.cont-type = loan.cont-type
                            AND tt-dviacct.posDate = dateKap
                            NO-ERROR.
                        IF AVAIL tt-dviacct THEN DO: /*если уже создавали записи в таблице по этому договору по данному опердню*/
                            IF snatie AND NOT tt-dviacct.debet THEN DO: 
                                amt_rub_cur = tt-dviacct.amt_rub_cur - amt_rub_cur.
                                IF amt_rub_cur < 0 THEN tt-dviacct.debet = TRUE. /*возможно меняем флаг изъятия*/
                                tt-dviacct.amt_rub_cur = abs(amt_rub_cur).
                            END.
                            ELSE DO:
                                IF (snatie EQ tt-dviacct.debet) THEN DO:
                                    tt-dviacct.amt_rub_cur = tt-dviacct.amt_rub_cur + amt_rub_cur.
                                END.
                                ELSE DO:
                                    IF NOT snatie AND tt-dviacct.debet THEN DO:
                                        amt_rub_cur = amt_rub_cur - tt-dviacct.amt_rub_cur.
                                        IF amt_rub_cur > 0 THEN tt-dviacct.debet = FALSE.
                                        tt-dviacct.amt_rub_cur = ABS(amt_rub_cur).
                                    END.
                                END.
                            END.
                            tt-dviacct.ostatokAcct-Proc = ostatok.
                            RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateKap,INPUT loan.currency,OUTPUT tmpOstatok).
                            tt-dviacct.ostatokAcct = tmpOstatok.
  
                        END.
                        ELSE DO:
                        RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateKap,INPUT loan.currency,OUTPUT lastOstatok).

                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct-Proc = ostatok
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.cur = loan.currency
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = dateKap
                            tt-dviacct.debet = snatie
                            tt-dviacct.amt_rub_cur = amt_rub_cur
                            .
                        END.
                    END.
                END. /*FOR EACH op-entry*/

                /*тут пока не понятно что происходит... зачем-то создаём операцию в сегодняшнем дне на один рубль
                может дальше это объясняется?*/
                IF dateKap < dateOp THEN DO:
                        RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateOp,INPUT loan.currency,OUTPUT lastOstatok).
 
                        CREATE tt-dviacct.
                        ASSIGN 
                        tt-dviacct.cont-code = loan.cont-code
                        tt-dviacct.cont-type = loan.cont-type
                        tt-dviacct.ostatokAcct-Proc = ostatok
                        tt-dviacct.ostatokAcct = lastOstatok
                        tt-dviacct.cur = loan.currency
                        tt-dviacct.acct = acct42
                        tt-dviacct.posDate = dateOp
                        tt-dviacct.debet = TRUE
                        tt-dviacct.amt_rub_cur = 1
                        .
                END.
                
                /* разбиваем на периоды начисления % */
                tempDate = DATE ( MONTH(loan.open-date) , 1 , YEAR(loan.open-date) ).
                DO WHILE tempDate <= dateOp:
                    dateKap = DATE_CORRECT(MONTH(tempDate),0,31,YEAR(tempDate)).
                    /*смещаем на месяц++*/
                    tempDate = DATE_CORRECT(MONTH(tempDate),1,1,YEAR(tempDate)).
                     
                    IF dateKap <= dateOp AND (dateKap <= loan.close-date OR loan.close-date = ?) THEN DO:
                        tempDate2 = loan.open-date - 1.

                        /*пробегаем по созданной таблице ищем день капитализации процентов соответствующий dateKap*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= dateKap BY tt-dviacct.posDate DESC:
                            IF tt-dviacct.posDate > tempDate2 THEN DO:
                                tempDate2 = tt-dviacct.posDate.
                                lastOstatok = tt-dviacct.ostatokAcct.
                                lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                                myCur = tt-dviacct.cur.
                                IF tempDate2 EQ dateKap THEN tt-dviacct.nachProc = TRUE.  
                                LEAVE.
                            END.
                        END.
                        /*если всё-таки нашли значение в таблице tt-dviacct но оно не оказалось начислением
                        Типо тут должно быть начисление... Но оно было на ноль рублей*/
                        IF tempDate2 >= (loan.open-date - 1) AND tempDate2 <> dateKap THEN DO:
                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.ostatokAcct-Proc = lastOstatokRasch
                            tt-dviacct.cur = myCur
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = dateKap
                            tt-dviacct.debet = FALSE
                            tt-dviacct.nachProc = TRUE
                            tt-dviacct.amt_rub_cur = 0
                            .
                        END.
                    END.
                END.

                dateKap = loan.open-date.
                /* Определение даты первой капитализации */
                IF acct474 <> '' THEN DO:
                    FIND FIRST op-entry WHERE (op-entry.op-status = "√" OR op-entry.op-status = "√√")
                            AND op-entry.acct-db = acct474
                            AND (op-entry.acct-cr EQ acctTr OR op-entry.acct-cr EQ acct42)
                /*FIX пока уберу это по-моему предыдущий вариант лучше AND (SUBSTRING(op-entry.acct-cr,1,2) = '40' OR SUBSTRING(op-entry.acct-cr,1,2) = '42')*/
                            AND op-entry.op-date > loan.open-date 
                            AND op-entry.op-date <= dateOp
                    NO-LOCK NO-ERROR.
                    IF AVAIL op-entry THEN DO:
                        dateKap = op-entry.op-date.
                    END.
                END.
                /*это видимо на всякий случай*/
                IF dateKap EQ loan.open-date THEN DO:
                    dateKap = DATE_CORRECT(MONTH(dateKap),1,31,YEAR(dateKap)).
                END.
                
                IF acct706 <> '' AND acct474 <> '' THEN DO:
                    /*пошли по документам начисления процентов*/
                    FOR EACH op-entry WHERE 
                        (op-entry.op-status = "√" OR op-entry.op-status = "√√")
                        AND op-entry.acct-cr = acct474,
                        FIRST op OF op-entry WHERE
                        op.contract-date >= loan.open-date AND op.contract-date <= dateOp
                        NO-LOCK:
                        IF loan.currency = '' THEN DO:
                            summProc = summProc + abs(op-entry.amt-rub).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal + abs(op-entry.amt-rub).
                            ELSE
                                dCurrReal = dCurrReal + abs(op-entry.amt-rub).
                        END.
                        ELSE DO:
                            summProc = summProc + abs(op-entry.amt-cur).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal + abs(op-entry.amt-cur).
                            ELSE
                                dCurrReal = dCurrReal + abs(op-entry.amt-cur).
                        END.
                    END. /*FOR EACH op-entry*/
                    
                    /* 09/09/2013 учитываем исправительные проводки излишне начисл */
                    FOR EACH op-entry WHERE 
                        (op-entry.op-status = "√" OR op-entry.op-status = "√√")
                        AND op-entry.acct-cr = acct706
                        AND (op-entry.acct-db = acct474 OR op-entry.acct-db = acct42),
                        FIRST op OF op-entry WHERE
                        op.contract-date >= loan.open-date AND op.contract-date <= dateOp
                        NO-LOCK:
                        IF loan.currency = '' THEN DO:
                            summProc = summProc - abs(op-entry.amt-rub).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal - abs(op-entry.amt-rub).
                            ELSE
                                dCurrReal = dCurrReal - abs(op-entry.amt-rub).
                        END.
                        ELSE DO:
                            summProc = summProc - abs(op-entry.amt-cur).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal - abs(op-entry.amt-cur).
                            ELSE
                                dCurrReal = dCurrReal - abs(op-entry.amt-cur).
                        END.
                    END. /*FOR EACH op-entry*/
                    
                END.
                
                /* Находим первое штрафное изъятие */
                IF IsShtrIzVkl(RECID(loan), dateOp, acct42) 
                THEN dateFirstSnatie =  dateOp. /*или вот это штрафное*/
                
                FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= dateOp 
                        AND ( tt-dviacct.posDate < loan.close-date  OR loan.close-date = ? )
                        AND tt-dviacct.debet 
                NO-LOCK BY tt-dviacct.posDate:
                        IF IsShtrIzVkl(RECID(loan), tt-dviacct.posDate, acct42) 
                        THEN DO:
                            dateFirstSnatie = tt-dviacct.posDate. /*или вот это штрафное*/
                            LEAVE.
                        END.
                END.

                periodSnatie = dateFirstSnatie - loan.open-date.
                
                /* Для вкладов, у которых при любом снятии будет штрафная ставка */
                IF LOOKUP(loan.cont-type,listContType_str) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    shtraf = 1.
                END.
                
                /* Для вкладов, у которых при снятии до первой капитализации будет штрафная ставка (Вост) */
                IF LOOKUP(loan.cont-type,listContType_str01) > 0  
                    AND dateFirstSnatie > loan.open-date 
                    AND dateFirstSnatie < dateKap THEN DO:
                    shtraf = 2.
                END.
                
                /* Для вкладов, у которых штрафная ставка зависит от даты снятия */
                IF LOOKUP(loan.cont-type,listContType_invest) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    shtraf = 3.
                END.

                /* Для типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1%, далее штрафная ставка зависит от даты снятия */
                IF LOOKUP(loan.cont-type,listContType_gold) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    IF dateFirstSnatie < dateKap THEN shtraf = 2.
                    ELSE shtraf = 4.
                END.    

                /*пенсионный */
                IF LOOKUP(loan.cont-type,listContType_hitr2) > 0
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    IF dateFirstSnatie < dateKap THEN shtraf = 2.
                    ELSE shtraf = 4.
                END.
                
                /* Определяем ставки */
                RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                loan.open-date,
                                loan.open-date,
                                "commission",
                                OUTPUT osnComm).  /* код основной коммисии */
                
                RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                loan.open-date,
                                loan.open-date,
                                "pen-commi",
                                OUTPUT shtrComm). /* код штрафной коммисии */

                {empty tt-comm-rate}
                /* основная */
                notIndivid = TRUE. /*а вот тут мы меняем смысл этого флага*/
                /*Ищем индивидуальную основную ставку для счета*/
                FOR EACH comm-rate WHERE comm-rate.commission EQ osnComm
                    AND comm-rate.since <= loan.open-date
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    AND comm-rate.min-value < firstOstatok
                    NO-LOCK
                    BREAK BY comm-rate.period BY comm-rate.min-value BY comm-rate.since DESC:
                    IF LAST-OF (comm-rate.since) AND LAST-OF (comm-rate.min-value) AND FIRST-OF(comm-rate.period) THEN DO:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.period
                        tt-comm-rate.shtraf = 0
                        .
                    END.
                END.
                IF notIndivid THEN DO:
                    /*ищем основную ставку для вклада*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ osnComm
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 0
                            .
                        END.
                    END.
                END.
                
                /* штрафная */
                notIndivid = TRUE.
                /*ищем штрафную индивидуальную ставку*/
                FOR EACH comm-rate WHERE comm-rate.commission EQ shtrComm
                    AND comm-rate.since <= loan.open-date
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    NO-LOCK
                    BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                    IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.period
                        tt-comm-rate.shtraf = 1
                        .
                    END.
                END.
                IF notIndivid THEN DO:
                    /*ищем штрафную ставку*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ shtrComm
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 1
                            .
                        END.
                    END.
                END.
                
                /* штрафная ВОСТ (0.1) */
                notIndivid = TRUE.
                /*ищем вост индивидуальную ставку*/
                FOR EACH comm-rate WHERE comm-rate.commission = "Вост"
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    NO-LOCK BY comm-rate.since:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.since - loan.open-date
                        tt-comm-rate.shtraf = 2
                        .
                END.
                IF notIndivid THEN DO:
                    /*ищем вост ставку*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ "Вост"
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 2
                            .
                        END.
                    END.
                END.
                
                /* дополнительно разбиваем, если изменяется ставка во время жизни вклада */
                FOR EACH tt-comm-rate NO-LOCK BY tt-comm-rate.period:
                    tempDate = loan.open-date + tt-comm-rate.period - 1.
                    IF tt-comm-rate.period > 0 AND tempDate <= dateOp 
                        AND (tempDate < loan.close-date OR loan.close-date EQ ?) 
                    THEN DO:
                        notIndivid = FALSE. /*Здесь опять используем для других целей этот флаг*/
                        /*находим последнюю операцию для всех ставок во всех периодах больше 0*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= tempDate 
                        NO-LOCK BY tt-dviacct.posDate DESC:
                            IF tt-dviacct.posDate < tempDate 
                            THEN notIndivid = TRUE.
                            ELSE notIndivid = FALSE.
                            lastOstatok = tt-dviacct.ostatokAcct.
                            lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                            myCur = tt-dviacct.cur.
                            LEAVE.
                        END.
                        IF notIndivid THEN DO: /*если операция не в день окончания периода*/
                        /*то создаём нулевую операцию для разделения между периодами датой окончания периода*/
                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.ostatokAcct-Proc = lastOstatokRasch
                            tt-dviacct.cur = myCur
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = tempDate
                            tt-dviacct.debet = FALSE
                            tt-dviacct.amt_rub_cur = 0
                            .
                        END.
                    END.
                END. /*FOR EACH tt-comm-rate*/
                
                stavkaShtraf = 0.   
                                
                CASE shtraf: 
                    /* Для вкладов, у которых при любом снятии будет штрафная ставка */
                    WHEN 1 THEN DO:
                        FIND FIRST tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL tt-comm-rate 
                        THEN DO:
                            perShtraf = 0.
                            stavkaShtraf = tt-comm-rate.rate-comm.
                        END.
                    END. 

                    /* Для вкладов, у которых при снятии до первой капитализации будет штрафная ставка (Вост) */
                    WHEN 2 THEN DO:
                        isVost = TRUE.
                    END.
                     /*TODO разобраться как должны изыматься вклады invest*/
                    /* Для вкладов, у которых штрафная ставка зависит от даты снятия */
                    WHEN 3 THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 
                                AND tt-comm-rate.period <= periodSnatie 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            stavkaShtraf = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                        /*это видимо защита минимального остатка вклада но почему-то 10 тысяч*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.cont-code EQ loan.cont-code 
                                AND tt-dviacct.debet
                                AND tt-dviacct.posDate > loan.open-date
                                AND (tt-dviacct.posDate < loan.close-date OR loan.close-date EQ ?) 
                        NO-LOCK BY tt-dviacct.posDate:
                            IF tt-dviacct.ostatokAcct < 10000 THEN DO:
                                isVost = TRUE.
                            END.
                        END.
                    END. 
                    
                    /* Для типов вкладов, у которых при снятии до 1 капитализации - ставка будет 0.1%, далее штрафная ставка зависит от даты снятия */
                    WHEN 4 THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 
                                AND tt-comm-rate.period <= periodSnatie 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            perShtraf = tt-comm-rate.period.
                            stavkaShtraf = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                    END. 
                END.

                /* заполняем временную табличку tt-stavka */
                pPeriod = 0.
                fullPeriod = 0.
                stavkaOsnovn = 0.
                lastOstatokRasch = 0.
                
                FOR EACH tt-dviacct WHERE tt-dviacct.cont-code EQ loan.cont-code 
                        AND (tt-dviacct.posDate <= loan.close-date OR loan.close-date EQ ?) NO-LOCK 
                BY tt-dviacct.posDate:
                    /*скорее всего только если первая операция по вкладу 
                    то есть именно первое внесение средств на вклад*/
                    IF fullPeriod EQ 0 THEN DO:
                        fullPeriod = 1.
                        lastDate = tt-dviacct.posDate.
                        tempDate = DATE_CORRECT(2,0,31,YEAR(tt-dviacct.posDate)). 
                        IF DAY(tempDate) > 28 THEN daysInYear = 36600.
                        ELSE daysInYear = 36500.
                        CREATE tt-stavka.
                            ASSIGN
                                tt-stavka.cont-code = loan.cont-code
                                tt-stavka.cont-type = loan.cont-type
                                tt-stavka.ostatokAcct = tt-dviacct.ostatokAcct
                                tt-stavka.ostatokAcctRasch = tt-dviacct.ostatokAcct-Proc
                                tt-stavka.acct = acct42
                                tt-stavka.startDate = lastDate
                                tt-stavka.endDate = tt-dviacct.posDate
                                tt-stavka.closeDate = loan.close-date
                                tt-stavka.period = pPeriod
                                tt-stavka.cur = loan.currency
                                tt-stavka.stavkaOsn = 0
                                tt-stavka.stavkaShtr = 0
                                tt-stavka.procRasch = 0
                                tt-stavka.shtraf = shtraf
                                tt-stavka.daysInYear = daysInYear
                                tt-stavka.fullPeriod = 0
                                tt-stavka.nachProc = tt-dviacct.nachProc
                                .
                            lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                    END.
                    ELSE DO:
                        fullPeriod = tt-dviacct.posDate - loan.open-date.
                        pPeriod = tt-dviacct.posDate - lastDate .
                        
                        /*находим процент основной ставки вклада*/
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf = 0 
                                AND tt-comm-rate.period <= fullPeriod 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            stavkaOsnovn = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                        /*если с вклада необходимо взять ставку ДВ. подменяем штрафную ставку*/
                        IF isVost THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf = 2 
                                AND tt-comm-rate.period <= fullPeriod 
                        NO-LOCK BY tt-comm-rate.period DESC:
                                stavkaShtraf = tt-comm-rate.rate-comm.
                                LEAVE.
                            END.
                        END.

                        tempDate = DATE_CORRECT(2,0,31,YEAR(tt-dviacct.posDate)). 
                        IF DAY(tempDate) > 28 THEN daysInYear = 36600.
                        ELSE daysInYear = 36500.
                        
                        procRaschAdd = TRUE.
                        
                        IF shtraf > 0 THEN DO:
                            IF shtraf EQ 4 THEN DO:
                                FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1
                                        AND tt-comm-rate.period <= fullperiod 
                                NO-LOCK BY tt-comm-rate.period DESC:
                                    stavkaShtraf = tt-comm-rate.rate-comm.
                                    LEAVE.
                                END.
                            END.
                            
                            IF fullPeriod >= perShtraf
                            THEN raschProc = (lastOstatokRasch * stavkaShtraf * pPeriod) / DEC(daysInYear).
                            ELSE raschProc = (lastOstatokRasch * stavkaOsnovn * pPeriod) / DEC(daysInYear).
                            
                            IF lastOstatokRasch < 0 THEN raschProc = 0.
                        
                            /*для следующего круга расчитываем остаток вклада*/    
                            IF tt-dviacct.debet 
                            THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur.
                            ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur.
                                
                            /*Если у вклада есть причисление и это дата начисления процентов. 
                            И уже после первой капитализации то мы прибавим расчитанные нами проценты*/
                            IF prich474_42 AND tt-dviacct.nachProc AND tt-dviacct.posDate >= dateKap  
                            THEN DO:
                                /*причислим расчитанные нами проценты за этот круг*/
                                lastOstatokRasch = lastOstatokRasch + raschProc.
                                /*пробегаем по созданным расчетам процентов которые ещё не учитывали в расчетах*/
                                FOR EACH tt-stavka WHERE tt-stavka.cont-code = loan.cont-code 
                                        AND tt-stavka.cont-type = loan.cont-type 
                                        AND tt-stavka.procRaschAdd 
                                NO-LOCK:
                                    lastOstatokRasch = lastOstatokRasch + tt-stavka.procRasch. 
                                    tt-stavka.procRaschAdd = FALSE. /*отмечаем что данный документ уже участвовал в расчете*/
                                END. 
                                procRaschAdd = FALSE. /*отмечаем что данный документ уже участвовал в расчете*/
                            END.
                        END. /*IF shtraf > 0*/
                        
                        ELSE DO: /*Тут расчет процентов если штрафного изъятия не было*/
                            raschProc = ROUND((lastOstatokRasch * stavkaOsnovn * pPeriod) / DEC(daysInYear),2).
                            IF lastOstatokRasch < 0 THEN raschProc = 0.
                            IF prich474_42 AND tt-dviacct.nachProc 
                                    AND NOT (
                                                YEAR(loan.open-date) EQ YEAR(tt-dviacct.posDate) 
                                            AND MONTH(loan.open-date) EQ MONTH(tt-dviacct.posDate)
                                            )
                            THEN DO:
                                IF tt-dviacct.debet 
                                THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur + raschProc.
                                ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur + raschProc.
                                
                                FOR EACH tt-stavka WHERE tt-stavka.cont-code = loan.cont-code 
                                        AND tt-stavka.cont-type = loan.cont-type 
                                        AND tt-stavka.procRaschAdd 
                                NO-LOCK:
                                    lastOstatokRasch = lastOstatokRasch + tt-stavka.procRasch.
                                    tt-stavka.procRaschAdd = FALSE.
                                END. 
                                procRaschAdd = FALSE.
                            END.
                            ELSE DO:
                                IF tt-dviacct.debet THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur.
                                    ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur.
                            END.
                        END.
                        
                        CREATE tt-stavka.
                        ASSIGN
                            tt-stavka.cont-code = loan.cont-code
                            tt-stavka.cont-type = loan.cont-type
                            tt-stavka.ostatokAcct = tt-dviacct.ostatokAcct
                            tt-stavka.ostatokAcctRasch = lastOstatokRasch
                            tt-stavka.acct = acct42
                            tt-stavka.startDate = lastDate
                            tt-stavka.endDate = tt-dviacct.posDate
                            tt-stavka.closeDate = loan.close-date
                            tt-stavka.period = pPeriod
                            tt-stavka.cur = loan.currency
                            tt-stavka.stavkaOsn = stavkaOsnovn
                            tt-stavka.stavkaShtr = stavkaShtraf
                            tt-stavka.procRasch = raschProc
                            tt-stavka.procRaschAdd  = procRaschAdd
                            tt-stavka.shtraf = shtraf
                            tt-stavka.summProc = summProc
                            tt-stavka.daysInYear = daysInYear
                            tt-stavka.fullPeriod = fullPeriod
                            .
                        lastDate = tt-dviacct.posDate.
                    END.
                END. /*FOR EACH tt-dviacct*/
            END. /* IF AVAIL acct THEN DO: */
            
            /*то что было в предыдущие годы*/ 
            FOR EACH tt-stavka WHERE tt-stavka.endDate < dateCurrYear AND tt-stavka.endDate <= dateLastNach NO-LOCK:
                dPrev = dPrev + tt-stavka.procRasch.
            END. /* FOR EACH tt-stavka */
            
            /*в этом году*/
            FOR EACH tt-stavka WHERE tt-stavka.endDate >= dateCurrYear AND tt-stavka.endDate <= dateLastNach NO-LOCK:
                dCurr = dCurr + tt-stavka.procRasch.
            END. /* FOR EACH tt-stavka */
            
        END. /* IF AVAIL loan-acct */
      END. /* IF countdps_t = 1 THEN DO: */
      ELSE DO:
        MESSAGE 'Более одного срочного счета, возможна ошибка в расчете процентов' VIEW-AS ALERT-BOX.
      END.
    END. /* IF LOOKUP */
    ELSE DO:
        MESSAGE 'Вклад устаревший требуется ручная проверка процентов ' + STRING(loan.cont-type) VIEW-AS ALERT-BOX.
    END.
END. /* IF AVAIL loan */

    {intrface.del}






