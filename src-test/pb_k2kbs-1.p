/**
Авторские права принадлежат: ПАО "Плюс Банк"
Основание:      
Что делает:     Процедура урегулирования К2 - КБС на одном л/с
Как работает:   
Параметры:      <л/с>
Место запуска:  
Создан:         25.03.2016 Борисов А.В.
*/

{globals.i}             /* Глобальные переменные сессии. */
{topkind.def}           /* Запуск УТ из процедуры */
{sh-defs.i}             /* остаток на счете */
{pb_logit.i}            /* Запись в лог */

DEFINE INPUT        PARAM iAcct    AS CHARACTER NO-UNDO.    /* Лицевой счет */
DEFINE INPUT        PARAM iCurr    AS CHARACTER NO-UNDO.    /* Валюта л/с   */
DEFINE INPUT        PARAM iPrtAll  AS CHARACTER NO-UNDO.    /* Печатать всю информацию - "ДА", или только изменения - "НЕТ" */
DEFINE INPUT        PARAM iUser    AS CHARACTER NO-UNDO.    /* Пользователь, от чьего имени создаются проводки */
                                                            /* Если указан => из Планировщика без оплаты       */
DEFINE INPUT        PARAM iLog     AS CHARACTER NO-UNDO.    /* Лог-файл     */
DEFINE INPUT        PARAM iPrt     AS CHARACTER NO-UNDO.    /* Протокол     */
DEFINE INPUT-OUTPUT PARAM lFirst   AS LOGICAL   NO-UNDO.    /* Первый счет? */

/* Проверки *************************************************************************************** */
DEFINE VARIABLE cMess       AS CHARACTER NO-UNDO.   /* Сообщение в лог */
IF (iCurr NE "") THEN RETURN.   /* <<< =============  Пока валютные счета пропускаем ================================================================ */

FIND FIRST acct
    WHERE (acct.acct        = iAcct)
      AND (acct.currency    = iCurr)
    NO-LOCK NO-ERROR.
IF (acct.cust-cat = "Ч") AND NOT CAN-DO("40802*,40821*", iAcct)
THEN DO:
    cMess = "???   Обнаружена картотека на счете ФЛ : " + iAcct.
    RUN LogIt(cMess, iLog).
    RUN pb_mail.p ("a.borisov,y.tsisnevich", "Картотека на счете ФЛ : " + SUBSTRING(iAcct,1,20), cMess, "").
    RETURN.
END.
RELEASE acct.

/* Переменные  ************************************************************************************ */
DEFINE VARIABLE cUser       AS CHARACTER NO-UNDO.   /* Пользователь */
DEFINE VARIABLE cBlOrdL     AS CHARACTER NO-UNDO.   /* Блокировки на л/с */
DEFINE VARIABLE lArest      AS LOGICAL   NO-UNDO.   /* Есть блокировка Арест */
DEFINE VARIABLE cAllOrd     AS CHARACTER NO-UNDO.   /* Список блокировок на л/с для протокола */
DEFINE VARIABLE cAcctK2     AS CHARACTER NO-UNDO.   /* Картотека K2 */
DEFINE VARIABLE cCurrK2     AS CHARACTER NO-UNDO.   /* Валюта K2 - всегда рубли */
DEFINE VARIABLE cAcctKbs    AS CHARACTER NO-UNDO.   /* Картотека КБС */
DEFINE VARIABLE cCurrKbs    AS CHARACTER NO-UNDO.   /* Валюта КБС - всегда рубли */
DEFINE VARIABLE nAcPos      AS DECIMAL   NO-UNDO.   /* Остаток на л/с (с учетом незавершенных списаний - П) */
DEFINE VARIABLE nAcPosTmp   AS DECIMAL   NO-UNDO.   /* Остаток при списании */
DEFINE VARIABLE nK2Pos      AS DECIMAL   NO-UNDO.   /* Остаток на К2 */
DEFINE VARIABLE nKbsPos     AS DECIMAL   NO-UNDO.   /* Остаток на КБС */
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO.   /* Запуск тр-ций: имя тр-ции */
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.   /* Запуск тр-ций: лог.параметр */
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.   /* Запуск тр-ций: ошибка */
DEFINE VARIABLE nPorog      AS DECIMAL   NO-UNDO.   /* Порог оплаты = 0 для К2, blocksum для КБС */
DEFINE VARIABLE cPartial    AS CHARACTER NO-UNDO.   /* Частичная оплата */
DEFINE VARIABLE cOp         AS CHARACTER NO-UNDO.   /* Документ оплаты  */

/* Временная таблица для хранения документов на картотеках */
DEFINE TEMP-TABLE ttKauCrt  NO-UNDO
    FIELD crt       AS CHARACTER        /* Тип картотеки: K2 или КБС */
    FIELD crtn      AS CHARACTER        /* Картотека после переноса  */
    FIELD kau       AS CHARACTER        /* Документ на картотеке <op>,<oe> */
    FIELD sort      AS CHARACTER        /* Сортировка в картотеке <приоритет>,<дата>,<сумма> = kau.sort */
    FIELD opdate    AS DATE             /* Дата постановки на картотеку */
    FIELD numop     AS INTEGER          /* = kau.numop */
    FIELD balance   AS DECIMAL          /* часть суммы документа, стоящая на картотек = kau.balance */
    FIELD firstsum  AS DECIMAL          /* Сумма постановки на картотеку */
    FIELD summa     AS DECIMAL          /* Сумма оплаты (м.б.частичной) */
    FIELD order-pay AS INTEGER          /* Очередность исх.док. для переноса(5(налог) = 3) */
    FIELD order-sym AS CHARACTER        /* Очередность исх.док. для протокола  */
    FIELD order-new AS INTEGER          /* Очередность исх.док. для оплаты: (4-5(налог),5-4,6-5) */
    FIELD details   AS CHARACTER        /* Назначение платежа исх.док. */
    FIELD action    AS CHARACTER        /* Действие: перемещение / оплата */
    FIELD eerror    AS CHARACTER        /* Ошибки */
    FIELD block     AS CHARACTER        /* Тип блокировки для документа из cBlOrdL */
    FIELD blocksum  AS DECIMAL          /* Сумма блокировки при БлокСумм */
    FIELD priost    AS LOGICAL          /* Документ приостановлен на К2 */
    FIELD oplata    AS CHARACTER        /* op документа оплаты */
    FIELD comissia  AS DECIMAL          /* комиссия, начисленная на документ оплаты */
    .

{pb_k2kbs.pro}

cMess = USERID("bisquit") + "  == Урегулирование К2 - КБС на счете " + iAcct.
RUN LogIt(cMess, iLog).
cUser = IF (iUser EQ "") THEN USERID('bisquit') ELSE iUser.

/* Остаток на л/с (с учетом незавершенных списаний) */
RUN acct-pos IN h_base(iAcct, iCurr, TODAY, TODAY, "√").
nAcPos    = IF (iCurr EQ "") THEN (sh-db - sh-bal) ELSE (sh-vdb - sh-val).
RUN acct-pos IN h_base(iAcct, iCurr, TODAY, TODAY, "П").
nAcPos    = nAcPos - (IF (iCurr EQ "") THEN sh-db ELSE sh-vdb).
nAcPos    = IF (nAcPos < 0.00) THEN 0.00 ELSE nAcPos.           /* Только для печати */
cMess = "  Правильный остаток на счете = " + STRING(nAcPos, "->>,>>>,>>>,>>9.99").
RUN LogIt(cMess, iLog).

nAcPosTmp    = - (IF (iCurr EQ "") THEN sh-bal ELSE sh-val).
IF (nAcPosTmp NE nAcPos)
THEN DO:
    cMess = "  Раньше использовали остаток = " + STRING(nAcPosTmp, "->>,>>>,>>>,>>9.99").
    RUN LogIt(cMess, iLog).
    cMess = "  Расхождение                 = " + STRING(nAcPosTmp - nAcPos, "->>,>>>,>>>,>>9.99").
    RUN LogIt(cMess, iLog).
END.
nAcPosTmp = nAcPos.

/* Блокировки на л/с ********************************************************** */
cBlOrdL = BlockArr(iAcct + "," + iCurr).
cAllOrd = ENTRY(2, cBlOrdL, ";").
cBlOrdL = ENTRY(1, cBlOrdL, ";").
lArest  = INDEX(cAllOrd, "Арест") NE 0.
cMess = "  - Блокировки на счете " + cBlOrdL.
RUN LogIt(cMess, iLog).

/* Картотеки */
nK2Pos      = 0.
cCurrK2     = "".
cAcctK2     = GetXAttrValue("acct", iAcct + "," + iCurr, "Карт2ВнСчет").
cMess = "  - К2  = " + cAcctK2.
RUN LogIt(cMess, iLog).
IF (cAcctK2 NE "")
THEN DO:
    ASSIGN
        cCurrK2     = ENTRY(2, cAcctK2).
        cAcctK2     = ENTRY(1, cAcctK2).
        .
    RUN acct-pos IN h_base(cAcctK2, cCurrK2, TODAY, TODAY, "Ф").
    nK2Pos  = (IF (cCurrK2 EQ "") THEN sh-bal ELSE sh-val).
END.

{empty ttKauCrt}

/* Сканируем К2 для переноса на КБС ******************************************* */
IF (nK2Pos GT 0)    /* Если есть документы нв К2 */
THEN DO:
    /* Для каждой аналитики К2 */
    FOR EACH kau
        WHERE (kau.acct         EQ cAcctK2)
          AND (kau.currency     EQ cCurrK2)
          AND (kau.zero-bal     EQ no)
        NO-LOCK,
    FIRST op
        WHERE (op.op            EQ INT64(ENTRY(1, kau.kau)))  /* пропускаем КАУ без документов */
        NO-LOCK,
    FIRST op-entry OF op
        NO-LOCK
        BY kau.sort
        BY op.op:

        RUN New_ttKauCrt("К2").
    END.

    FOR EACH ttKauCrt
        WHERE (ttKauCrt.crt EQ "К2")
        BY ttKauCrt.order-pay
        BY ttKauCrt.sort:

        IF       NOT (cBlOrdL BEGINS "КонкПр")                  /* При конкурсном производстве и банкротстве... */
            AND  NOT (cBlOrdL BEGINS "Банкрот")                 /*     все оставляем на своих местах            */
            AND (NOT ttKauCrt.priost AND (ttKauCrt.block NE "") /* Не приостановлен и есть блокировка */
                  OR ttKauCrt.priost AND lArest)                /* или приостановлен, но есть Арест   */
        THEN DO:

            cMess = "  - Перенос на КБС".
            RUN LogIt(cMess, iLog).
            /* Запуск транзакции  sbk_2kbs - SB: Перемещение К2 <--> КБС */
            cTranz = "sbk_2kbs".
            {empty tOpKindParams}     /* очистить таблицу параметров */
            ASSIGN
                lOk =   TDAddParam("iSrcCrt"   , cAcctK2)
                    AND TDAddParam("iCrtType"  , "КБС")
                    AND TDAddParam("iAcct"     , iAcct)
                    AND TDAddParam("iCurrency" , iCurr)
                    AND TDAddParam("iKau"      , ttKauCrt.kau)
                    AND TDAddParam("iDetails"  , ttKauCrt.details)
                    AND TDAddParam("iUser"     , cUser)
                    AND TDAddParam("iLog"      , iLog)
                NO-ERROR.
            IF NOT lOk
            THEN DO:
                cMess = "???   Ошибка передачи параметров в транзакцию " + cTranz.
                RUN LogIt(cMess, iLog).
                ttKauCrt.eerror = "Ошибка передачи параметров в транзакцию " + cTranz.
            END.
            ELSE DO:
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                    cMess = "???   Ошибка при К2 -> КБС в транзакции " + cTranz + " : " + cErrMsg.
                    RUN LogIt(cMess, iLog).
                    RUN pb_mail.p ("a.borisov", "Ошибка К2-КБС", cMess, SEARCH("sysmess.log")).
                    ttKauCrt.eerror = cErrMsg.
                    ttKauCrt.action = "Ошибка".
                END.
                ELSE DO:
                    ttKauCrt.action = "Перенесен на КБС".  /* На КБС его заново сканировать не будем */
                    ttKauCrt.crtn   = "КБС".
                    IF ttKauCrt.priost
                    THEN ttKauCrt.eerror = "Приостановлен".
                END.
            END.
        END. /* IF NOT (cBlOrdL BEGINS "КонкПр") */
        ELSE ttKauCrt.action = IF (iPrtAll EQ "ДА") THEN "Оставлен на К2" ELSE "".

    END. /* FOR EACH ttKauCrt */
END. /* IF (nK2Pos GT 0) */

/* Сканируем КБС для переноса на К2 ******************************************* */
cCurrKbs    = "".
cAcctKbs    = GetXAttrValue("acct", iAcct + "," + iCurr, "КартБВнСчет").    /* КБС могла появиться только что при К2 -> КБС */
cMess = "  - КБС = " + cAcctKbs.
RUN LogIt(cMess, iLog).

IF (cAcctKbs NE "")
THEN DO:
    ASSIGN
        cCurrKbs    = ENTRY(2, cAcctKbs)
        cAcctKbs    = ENTRY(1, cAcctKbs)
        .
    RUN acct-pos IN h_base(cAcctKbs, cCurrKbs, TODAY, TODAY, "Ф").
    nKbsPos     = (IF (cCurrKbs EQ "") THEN sh-bal ELSE sh-val).

    IF (nKbsPos GT 0)    /* Если есть документы нв КБС */
    THEN DO:
        /* Для каждой аналитики КБС */
        FOR EACH kau
            WHERE (kau.acct         EQ cAcctKbs)
              AND (kau.currency     EQ cCurrKbs)
              AND (kau.zero-bal     EQ no)
            NO-LOCK,
        FIRST op
            WHERE (op.op            EQ INT64(ENTRY(1, kau.kau)))  /* пропускаем КАУ без документов */
            NO-LOCK,
        FIRST op-entry OF op
            NO-LOCK
            BY kau.sort
            BY op.op:

            IF NOT CAN-FIND(FIRST ttKauCrt
                                WHERE (ttKauCrt.kau     EQ kau.kau)
                                  AND (ttKauCrt.action  EQ "Перенесен на КБС"))
            THEN RUN New_ttKauCrt("КБС").
        END.

        FOR EACH ttKauCrt
            WHERE (ttKauCrt.crt EQ "КБС")
            BY ttKauCrt.order-pay
            BY ttKauCrt.sort:

            IF       NOT (cBlOrdL BEGINS "КонкПр")                  /* При конкурсном производстве и банкротстве... */
                AND  NOT (cBlOrdL BEGINS "Банкрот")                 /*     все оставляем на своих местах            */
                AND (NOT ttKauCrt.priost AND (ttKauCrt.block EQ "") /* Не приостановлен и нет блокировок */
                  OR     ttKauCrt.priost AND NOT lArest)            /* или приостановлен и нет Ареста */
            THEN DO:

                cMess = "  - Перенос на К2".
                RUN LogIt(cMess, iLog).
                /* Запуск транзакции  sbk_2kbs - SB: Перемещение К2 <--> КБС */
                cTranz = "sbk_2kbs".
                {empty tOpKindParams}     /* очистить таблицу параметров */
                ASSIGN
                    lOk =   TDAddParam("iSrcCrt"   , cAcctKbs)
                        AND TDAddParam("iCrtType"  , "К2")
                        AND TDAddParam("iAcct"     , iAcct)
                        AND TDAddParam("iCurrency" , iCurr)
                        AND TDAddParam("iKau"      , ttKauCrt.kau)
                        AND TDAddParam("iDetails"  , ttKauCrt.details)
                        AND TDAddParam("iUser"     , cUser)
                        AND TDAddParam("iLog"      , iLog)
                    NO-ERROR.
                IF NOT lOk
                THEN DO:
                    cMess = "???   Ошибка передачи параметров в транзакцию " + cTranz.
                    RUN LogIt(cMess, iLog).
                    ttKauCrt.eerror = "Ошибка передачи параметров в транзакцию " + cTranz.
                END.
                ELSE DO:
                    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                    IF NOT lOk
                    THEN DO:
                        cMess = "???   Ошибка при КБС -> К2 в транзакции " + cTranz + " : " + cErrMsg.
                        RUN LogIt(cMess, iLog).
                        RUN pb_mail.p ("a.borisov", "Ошибка К2-КБС", cMess, SEARCH("sysmess.log")).
                        ttKauCrt.eerror = cErrMsg.
                        ttKauCrt.action = "Ошибка".
                    END.
                    ELSE DO:
                        ttKauCrt.action = "Перенесен на К2".
                        ttKauCrt.crtn   = "К2".
                        IF ttKauCrt.priost
                        THEN ttKauCrt.eerror = "Приостановлен".
                    END.
                END.
            END. /* IF NOT (cBlOrdL BEGINS "КонкПр") */
            ELSE ttKauCrt.action = IF (iPrtAll EQ "ДА") THEN "Оставлен на КБС" ELSE "".

        END. /* FOR EACH ttKauCrt */
    END. /* IF (nKbsPos GT 0) */
END. /* IF (cAcctKbs NE "") */

/* Проверяем аналитики на возможность оплаты ********************************** */
cCurrK2     = "".
cAcctK2     = GetXAttrValue("acct", iAcct + "," + iCurr, "Карт2ВнСчет").    /* К2 могла появиться только что при КБС -> К2 */
IF (cAcctK2 NE "")
THEN ASSIGN
        cCurrK2     = ENTRY(2, cAcctK2).
        cAcctK2     = ENTRY(1, cAcctK2).
        .

IF          (iUser EQ "")               /* Процедура запущена не из Планировщика */
    AND NOT (cBlOrdL BEGINS "КонкПр")   /* и нет конкурсного производства        */
    AND NOT (cBlOrdL BEGINS "Банкрот")  /* и нет банкротства => оплачивать можно */
THEN DO:
    FOR EACH ttKauCrt
/*      WHERE NOT ttKauCrt.priost   / * только неприостановленные */
        NO-LOCK
        BY ttKauCrt.crtn                            /* К2, затем КБС */
        BY ttKauCrt.order-new                       /* в порядке "новой" очередности  */
        BY ttKauCrt.sort                            /* по дате постановки в картотеку */
        BY (ttKauCrt.balance - ttKauCrt.firstsum):  /* частично оплаченные - первыми  */

        nPorog  = IF (ttKauCrt.crtn EQ "К2") THEN 0 ELSE ttKauCrt.blocksum.

        IF     (ttKauCrt.block NE "Дб")     /* Для К2 выполняется автоматически */
           AND (nAcPosTmp      GT nPorog)
        THEN DO:                            /* Можем что-то оплатить */
            IF     (iCurr NE cCurrK2)
                OR (iCurr NE "")
            THEN DO:                        /* Для вал счета руб картотека => ручная обработка */
                cMess = "  - Оплата вручную".
                RUN LogIt(cMess, iLog).
                ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|")
                                + "Оплата вручную".
            END.
            ELSE IF ttKauCrt.priost         /* Приостановленные не оплачиваем, но записываем в протокол */
            THEN DO:                        /* Для вал счета руб картотека => ручная обработка */
                cMess = "  - Приостановленный документ".
                RUN LogIt(cMess, iLog).
                ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "НЕ оплачен".
                ttKauCrt.eerror = ttKauCrt.eerror + (IF (ttKauCrt.eerror EQ "") THEN "" ELSE "|") + "Приостановлен".
            END.
            ELSE DO:                                            /* и р/с и картотека - рублевые */
                cPartial = "yes".

                IF (nAcPosTmp - nPorog GE ttKauCrt.balance)
                THEN DO:                                        /* Можем оплатить эту аналитику полностью */
                    IF (ttKauCrt.balance EQ ttKauCrt.firstsum)
                    THEN cPartial   = "no".                /* Ранее не было частичной оплаты */
                    ttKauCrt.summa  = ttKauCrt.balance.
                END.
                ELSE ttKauCrt.summa = nAcPosTmp - nPorog.

                cMess = "  - " + (IF (cPartial EQ "yes") THEN "Частичная оплата" ELSE "Оплата").
                RUN LogIt(cMess, iLog).
                /* Запуск транзакции  sbk_pay - SB: Создание документов оплаты */
                RUN SetSysConf IN h_base("ОплатаСКартотеки","").
                cTranz = "sbk_pay".
                {empty tOpKindParams}     /* очистить таблицу параметров */
                ASSIGN
                    lOk =   TDAddParam("iSrcCrt"   , IF (ttKauCrt.crtn EQ "К2") THEN cAcctK2 ELSE cAcctKbs)
                        AND TDAddParam("iCrtType"  , ttKauCrt.crtn)
                        AND TDAddParam("iAcct"     , iAcct)
                        AND TDAddParam("iCurrency" , iCurr)
                        AND TDAddParam("iKau"      , ttKauCrt.kau)
                        AND TDAddParam("iSumma"    , STRING(ttKauCrt.summa))
                        AND TDAddParam("partial"   , cPartial)
                        AND TDAddParam("inalog"    , (IF (INDEX(ttKauCrt.order-sym, "налог") NE 0) THEN "yes" ELSE "no"))
                        AND TDAddParam("iUser"     , cUser)
                        AND TDAddParam("iLog"      , iLog)
                    NO-ERROR.
                IF NOT lOk
                THEN DO:
                    cMess = "???   Ошибка передачи параметров в транзакцию " + cTranz.
                    RUN LogIt(cMess, iLog).
                    ttKauCrt.eerror = "Ошибка передачи параметров в транзакцию " + cTranz.
                END.
                ELSE DO:
                    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                    IF NOT lOk
                    THEN DO:
                        cMess = "???   Ошибка при оплате в транзакции " + cTranz + " : " + cErrMsg.
                        RUN LogIt(cMess, iLog).
                        RUN pb_mail.p ("a.borisov", "Ошибка К2-КБС", cMess, SEARCH("sysmess.log")).
                        ttKauCrt.eerror = ttKauCrt.eerror + (IF (ttKauCrt.eerror EQ "") THEN "" ELSE "|") + "Проверьте непроведенные документы".
                        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "Ошибка оплаты".
                    END.
                    ELSE DO:
                        nAcPosTmp       = nAcPosTmp - ttKauCrt.summa.
                        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|")
                                        + (IF (cPartial EQ "no") THEN "Оплата" ELSE "Частичная оплата").
                        ttKauCrt.oplata = GetSysConf("ОплатаСКартотеки").
                        RUN LogIt("    Документ оплаты - " + ttKauCrt.oplata, iLog).
                    END.
                END.
            END. /* IF (iCurr NE cCurrK2) */
        END. /* IF (ttKauCrt.block NE "Дб") */
    END. /* FOR EACH ttKauCrt */
END. /* IF (iUser EQ "") */

/* Начисляем комиссии на документы оплаты с картотеки */
RUN LogIt("Начисление комиссий :", iLog).
FOR EACH ttKauCrt
    WHERE (ttKauCrt.oplata  NE "")
    NO-LOCK:

    RUN LogIt("  - документ оплаты - " + ttKauCrt.oplata, iLog).
    cTranz = "CreateComPT".
    {empty tOpKindParams}     /* очистить таблицу параметров */
    lOk = TDAddParam("CurOp", ttKauCrt.oplata) NO-ERROR.
    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
    IF lOk
    THEN DO:
        ttKauCrt.comissia = DEC(GetSysConf("КомиссияСКартотеки")).
        RUN LogIt("    комиссия - " + STRING(ttKauCrt.comissia, ">>>,>>>,>>9.99"), iLog).
    END.
    ELSE DO:
        cMess = "???   Ошибка снятия комиссии : " + cErrMsg.
        RUN LogIt(cMess, iLog).
        RUN pb_mail.p ("a.borisov", "Ошибка К2-КБС", cMess, SEARCH("sysmess.log")).
        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "Ошибка комиссии".
    END.
END.

/* Печатаем протокол, если есть хоть один документ */
IF CAN-FIND(FIRST ttKauCrt
                WHERE (IF (iPrtAll EQ "ДА") THEN YES ELSE (ttKauCrt.action NE "")))
THEN DO:
    RUN XLProtokol(iPrt, lFirst).
    lFirst = NO.
END.
