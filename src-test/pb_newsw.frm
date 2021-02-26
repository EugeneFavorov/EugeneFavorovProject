/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОКМР.1578 - формирование валютных переводов в Бисквите
Что делает:     Форма ввода документа
Параметры:
Место запуска:
Создан:         26.05.2017 Борисов А.В.
*/

FORM
    "N:" cDocNum "    ДЕБЕТ:"   cAcctDb              "  КРЕДИТ:  " cAcctCr   SKIP
    "ПЕРЕВОД:    ВАЛЮТА" cValPere "  СУММА" dSumPere cCommT        cAcctComm SKIP
    "СПИСАНИЕ:   ВАЛЮТА" cValSpis "  СУММА" dSumSpis "  КУРС "  dKurs SKIP
    "ПЛАТЕЛЬЩИК:" cINNTxt       cSndINN     HELP "F5-просмотр 50K:" SKIP
    "            НАИМЕНОВАНИЕ"  cSndName    HELP "F5-просмотр 50K:, F11/F12-транслитерация/обратно"
                                iSndCnt1    SKIP
    "            АДРЕС       "  cSndAdr     HELP "F5-просмотр 50K:, F11/F12-транслитерация/обратно"
                                iSndCnt2    SKIP
    "          " cDrTxt         cDrTel      SKIP
    "БАНК-ПОСРЕДНИК (SWIFT)  "  cSwIntB     cIntBName cIntBCntr SKIP
    "БАНК БЕНЕФИЦИАРА (SWIFT)"  cSwBenB     HELP "F5-ввод корсчета"
                                cBenBName cBenBCntr SKIP
    "БЕНЕФИЦИАР: СЧЕТ        "  cBenAcct    HELP "F1-из списка" SKIP
    "            НАИМЕНОВАНИЕ"  cBenName    HELP "F1-из списка, F5-просмотр 59:, F11/F12-транслитерация/обратно"
                                iBenCnt1    SKIP
    "            АДРЕС       "  cBenAdr     HELP "F1-из списка, F5-просмотр 59:, F11/F12-транслитерация/обратно"
                                iBenCnt2    SKIP
    cBenCntrT cBenCntr cBenRFT  cBenRF      cBenULT  cBenUL SKIP
    "НАЗНАЧЕНИЕ "               cDetails    HELP "F5-просмотр 70:, F11/F12-транслитерация/обратно"
                                iDetCnt     SKIP
    "ДЕТАЛИ РАСХОДОВ         "  cCharge     SKIP
    "ИНФОРМАЦИЯ ОТПРАВИТЕЛЯ  "  cSndInfo
    "  (6 строк)" AT ROW 17 COL 1

    WITH FRAME swdoc
    KEEP-TAB-ORDER 1 DOWN OVERLAY CENTERED NO-LABEL ROW 3 WIDTH 80
    TITLE COLOR BRIGHT-WHITE "[ ПЕРЕВОД SWIFT ]".

/* ON ******************************************* */
/* Заглушки. Иначе форма закрывается */
ON F3, F4 ANYWHERE RETURN NO-APPLY.

/* ON ******************************************* */
ON LEAVE OF cDocNum IN FRAME swdoc
DO:
    ASSIGN cDocNum.
    IF (cDocNum EQ "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "1", "Введите номер документа!").
        RETURN NO-APPLY.
    END.
END.

/* ON ******************************************* */
ON F1 OF cValPere IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN currency.p("Учетный",3).
    END.
    IF    (LASTKEY EQ 10
        OR LASTKEY  EQ 13)
       AND pick-value NE ?
    THEN cValPere = pick-value.

    DISPLAY cValPere WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON LEAVE OF cValPere IN FRAME swdoc
DO:
    ASSIGN cValPere.
    dSumSpis = 0.0.
    dSumPere = 0.0.
/*
    IF CAN-DO(op-template.currency, cValPere)
    THEN DO:
*/
        IF (cValPere NE cValSpis)
        THEN DO:
            FIND FIRST currency
                WHERE (currency.currency    EQ cValPere)
                NO-LOCK NO-ERROR.
            IF (AVAIL currency)
            THEN DO:
                dKursU   = ROUND(CrossRate("учетный", cValSpis, cValPere, TODAY), 6).
                dKurs    = dKursU.
                ENABLE dSumSpis dKurs WITH FRAME swdoc.
            END.
            ELSE DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "Неизвестная валюта: " + cValPere).
                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            dKursU   = 1.0.
            dKurs    = 1.0.
            DISABLE dSumSpis dKurs WITH FRAME swdoc.
        END.

        cAcctCr = GetRefVal("SWkorr", in-op-date, IF CAN-DO(op-template.currency, cValPere) THEN cValPere ELSE cValSpis).
        DISPLAY cAcctCr dKurs dSumPere dSumSpis WITH FRAME swdoc.
/*
    END.
    ELSE DO:
        RUN Fill-SysMes IN h_tmess ("", "", "1", "Допустимая валюта: " + op-template.currency).
        RETURN NO-APPLY.
    END.
*/
END.

/* ON ******************************************* */
ON LEAVE OF dSumPere IN FRAME swdoc
DO:
    ASSIGN dSumPere.
    IF (dSumPere EQ 0)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "1", "Введите сумму перевода!").
        RETURN NO-APPLY.
    END.
    ELSE DO:
        dSumPereR = ROUND(CurToBase("УЧЕТНЫЙ", cValPere, in-op-date, dSumPere), 2).
        dSumSpis  = ROUND(dSumPere / dKurs, 2).
        dSumSpisR = ROUND(CurToBase("УЧЕТНЫЙ", cValSpis, in-op-date, dSumSpis), 2).
        DO WHILE dSumSpisR < dSumPereR :
            dSumSpis  = dSumSpis + 0.01.
            dSumSpisR = ROUND(CurToBase("УЧЕТНЫЙ", cValSpis, in-op-date, dSumSpis), 2).
        END.
        DISPLAY dSumSpis WITH FRAME swdoc.

        /* Вычислим сумму комиссии */
        IF (cAcctComm <> "")
        THEN DO:
            cValComm = cValPere.
            dSumComR = dSumPere.

            IF (acct.cust-cat EQ "Ю") OR (cAcctDb BEGINS "40802")
            THEN DO:    /* ЮЛ/ИП */
                IF CAN-DO("!840,!978,*", cValPere)
                THEN DO:
                    cValComm = "840".
                    dSumComR = CurToCur("УЧЕТНЫЙ", cValPere, cValComm, in-op-date, dSumPere).
                END.
                dSumComm = IF (cValPere = "840") THEN ROUND(MINIMUM(250.0, MAXIMUM( 25.0,((dSumComR * 0.1 ) / 100.0))), 2) ELSE (
                           IF (cValPere = "978") THEN ROUND(MINIMUM(250.0, MAXIMUM( 35.0,((dSumComR * 0.2 ) / 100.0))), 2)
                                                 ELSE ROUND(MINIMUM(250.0, MAXIMUM( 25.0,((dSumComR * 0.2 ) / 100.0))), 2)). /* MAX и MIN в USD */
            END.
            ELSE DO:    /* ФЛ */
                IF     (CAN-DO("840,978",     cValPere) AND (dSumPere > 50000.0))
                    OR (CAN-DO("!840,!978,*", cValPere) AND ((CurToCur("УЧЕТНЫЙ", cValPere, "840", in-op-date, dSumPere) > 50000.0)
                                                          OR (CurToCur("УЧЕТНЫЙ", cValPere, "978", in-op-date, dSumPere) > 50000.0)))
                THEN dSumComm = ROUND(dSumComR * 0.07, 2).   /* Заградительный тариф 7% */
                ELSE DO:
                    IF CAN-DO("!840,!978,!156,*", cValPere)
                    THEN DO:
                        cValComm = "840".
                        dSumComR = CurToCur("УЧЕТНЫЙ", cValPere, cValComm, in-op-date, dSumPere).
                    END.
                    dSumComm = IF (cValPere = "840") THEN ROUND(MINIMUM( 75.0, MAXIMUM( 20.0,((dSumComR * 0.15) / 100.0))), 2) ELSE (
                               IF (cValPere = "156") THEN ROUND(MINIMUM(500.0, MAXIMUM(170.0,((dSumComR * 0.15) / 100.0))), 2)
                                                     ELSE ROUND(MINIMUM(150.0, MAXIMUM( 30.0,((dSumComR * 0.3 ) / 100.0))), 2)).
                END.
                
                
            END.

            dSumComR = ROUND(CurToBase("УЧЕТНЫЙ", cValComm, in-op-date, dSumComm), 2).
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Сумма комиссии = " + TRIM(STRING(dSumComm, ">>>,>>>,>>9.99")) + " " + GetISOCode(cValComm)
                                                   + " (" + TRIM(STRING(dSumComR, ">>>,>>>,>>9.99")) + " руб)").
        END.

        RUN CheckPOS.
    END.
END.

/* ON ******************************************* */
ON F1 OF cAcctComm IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN browseld.p("acct",
            "acct-cat" + CHR(1) + "bal-acct"      + CHR(1) + "contract"               + CHR(1) + "cust-cat"    + CHR(1) + "cust-id",
            "b"        + CHR(1) + "40*,423*,426*" + CHR(1) + "Текущ,Расчет,Депоз,dps" + CHR(1) + acct.cust-cat + CHR(1) + STRING(acct.cust-id),
            "bal-acct", 5).

        IF (KEYFUNCTION(LASTKEY) EQ "GO")
        THEN DO:
            cAcctComm   = ENTRY(1, pick-value).
            cCurrComm   = ENTRY(2, pick-value).
            DISPLAY cAcctComm WITH FRAME swdoc.
        END.
    END.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cAcctComm IN FRAME swdoc
DO:
    RUN Fill-SysMes IN h_tmess ("", "", "1", "Используйте F1 для выбора счета!").
    DISPLAY cAcctComm WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON LEAVE OF cAcctComm IN FRAME swdoc
DO:
    RUN CheckPOS.
END.

/* ON ******************************************* */
ON LEAVE OF dSumSpis IN FRAME swdoc
DO:
    IF (dSumSpis NE DEC(dSumSpis:SCREEN-VALUE))
    THEN DO:
        IF (ROUND(dSumPere / dKursU, 2) > DEC(dSumSpis:SCREEN-VALUE))
        THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Введенной суммы недостаточно для перевода").
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN dSumSpis.
            dSumSpisR = ROUND(CurToBase("УЧЕТНЫЙ", cValSpis, in-op-date, dSumSpis), 2).
            IF (dSumPere NE 0) THEN dKurs = ROUND(dSumPere / dSumSpis, 6).
            DISPLAY dKurs WITH FRAME swdoc.
        END.
    END.

    RUN CheckPOS.
END.

/* ON ******************************************* */
ON LEAVE OF dKurs IN FRAME swdoc
DO:
    IF (dKurs NE DEC(dKurs:SCREEN-VALUE))
    THEN DO:
        IF (dKursU > DEC(dKurs:SCREEN-VALUE))
        THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Введенный курс превышает учетный = " + STRING(dKursU)).
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN dKurs.
            dSumSpis  = ROUND(dSumPere / dKurs, 2).
            dSumSpisR = ROUND(CurToBase("УЧЕТНЫЙ", cValSpis, in-op-date, dSumSpis), 2).
            DISPLAY dSumSpis WITH FRAME swdoc.
        END.
    END.

    RUN CheckPOS.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cSndName IN FRAME swdoc
DO:
    ASSIGN cSndName.
    iSndCnt1 = LENGTH(cSndName).
    DISPLAY iSndCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F11 OF cSndName IN FRAME swdoc
DO:
    ASSIGN cSndName.
    cSndName = sw-trans(cSndName, YES, "RUR5").
    iSndCnt1 = LENGTH(cSndName).
    DISPLAY cSndName iSndCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F12 OF cSndName IN FRAME swdoc
DO:
    ASSIGN cSndName.
    cSndName = sw-trans(cSndName, NO, "RUR5").
    iSndCnt1 = LENGTH(cSndName).
    DISPLAY cSndName iSndCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cSndAdr IN FRAME swdoc
DO:
    ASSIGN cSndAdr.
    iSndCnt2 = LENGTH(cSndAdr).
    DISPLAY iSndCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F11 OF cSndAdr IN FRAME swdoc
DO:
    ASSIGN cSndAdr.
    cSndAdr  = sw-trans(cSndAdr, YES, "RUR5").
    iSndCnt2 = LENGTH(cSndAdr).
    DISPLAY cSndAdr iSndCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F12 OF cSndAdr IN FRAME swdoc
DO:
    ASSIGN cSndAdr.
    cSndAdr  = sw-trans(cSndAdr, NO, "RUR5").
    iSndCnt2 = LENGTH(cSndAdr).
    DISPLAY cSndAdr iSndCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F5 OF cSndINN, cSndName, cSndAdr IN FRAME swdoc
DO:
    ASSIGN cSndINN cSndName cSndAdr cDrTel.
    cN1   = cSndAdr
          + IF (cDrTxt  = "ДЕНЬ РОЖДЕНЬЯ") THEN (", " + cDrTel) ELSE "".
/*  cN1   = ENTRY(1, cSndINN, "/").
    cN2   = IF (NUM-ENTRIES(cSndINN, "/") < 2) THEN "" ELSE ENTRY(2, cSndINN, "/").
*/  MESSAGE STRING(":50К:/" + SUBSTRING(cAcctDb, 1, 20), "x(35)") + "~n"
/*        + (IF (cN1 + cN2 = "") THEN "" ELSE
            (STRING("INN" + cN1 + IF (cN2 = "") THEN "" ELSE (", KPP" + cN2), "x(35)") + "~n"))
*/        + (IF (cSndINN = "") THEN "" ELSE (STRING("INN" + cSndINN, "x(35)") + "~n"))
          + STRING(TRIM(SUBSTRING(cSndName, 1, 35)), "x(35)") + "~n"
          + (IF (iSndCnt1 < 36) THEN "" ELSE (STRING(TRIM(SUBSTRING(cSndName, 36, 35)), "x(35)") + "~n"))
          + STRING(TRIM(SUBSTRING(cN1,  1, 35)), "x(35)")
          + (IF (LENGTH(cN1) < 36) THEN "" ELSE ("~n" + STRING(TRIM(SUBSTRING(cN1, 36, 35)), "x(35)")))
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.

/* ON ******************************************* */
ON F1 OF cSwIntB IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN browseld.p("banks",
            "bank-code-type" + CHR(1) + "SetFirstFrm",
            "BIC"            + CHR(1) + "4",
            "", 5).
        IF (KEYFUNCTION(LASTKEY) EQ "GO")
        THEN DO:
            FOR FIRST banks
                WHERE (banks.bank-id                EQ INT(pick-value))
                NO-LOCK,
            FIRST banks-code OF banks
                WHERE (banks-code.bank-code-type    EQ 'BIC')
                NO-LOCK:

                cSwIntB   = banks-code.bank-code.
                cIntBName = banks.name.
                cIntBCntr = "(" + banks.country-id + ")".
                DISPLAY cSwIntB cIntBName cIntBCntr WITH FRAME swdoc.
            END.
        END.
    END.
END.

/* ON ******************************************* */
ON LEAVE OF cSwIntB IN FRAME swdoc
DO:
    ASSIGN cSwIntB.

    IF (cSwIntB NE "")
    THEN DO:
        cIntBName = "".
        FOR FIRST banks-code
            WHERE (banks-code.bank-code         EQ cSwIntB)
              AND (banks-code.bank-code-type    EQ 'BIC')
            NO-LOCK,
        FIRST banks OF banks-code
            NO-LOCK:

            cIntBName = banks.name.
            cIntBCntr = "(" + banks.country-id + ")".
        END.

        IF (cIntBName NE "")
        THEN DISPLAY cIntBName cIntBCntr WITH FRAME swdoc.
        ELSE DO:
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Банк не найден!").
            RETURN NO-APPLY.
        END.
    END.
END.

/* ON ******************************************* */
ON F1 OF cSwBenB IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN browseld.p("banks",
            "bank-code-type" + CHR(1) + "SetFirstFrm",
            "BIC"            + CHR(1) + "4",
            "", 5).
        IF (KEYFUNCTION(LASTKEY) EQ "GO")
        THEN DO:
            FOR FIRST banks
                WHERE (banks.bank-id                EQ INT(pick-value))
                NO-LOCK,
            FIRST banks-code OF banks
                WHERE (banks-code.bank-code-type    EQ 'BIC')
                NO-LOCK:

                cSwBenB   = banks-code.bank-code.
                cBenBName = banks.name.
                cBenBCntr = "(" + banks.country-id + ")".
                DISPLAY cSwBenB cBenBName cBenBCntr WITH FRAME swdoc.
            END.
        END.
    END.
END.

/* ON ******************************************* */
ON F5 OF cSwBenB IN FRAME swdoc
DO:
    RUN g-prompt.p ("character", "Корсчет", "x(30)", "",
                    "Введите корсчет банка бенефициара",
                    ?,"","",?,?,OUTPUT cKsBenB).
END.

/* ON ******************************************* */
ON LEAVE OF cSwBenB IN FRAME swdoc
DO:
    ASSIGN cSwBenB.

    IF (cSwBenB NE "")
    THEN DO:
        cBenBName = "".
        FOR FIRST banks-code
            WHERE (banks-code.bank-code         EQ cSwBenB)
              AND (banks-code.bank-code-type    EQ 'BIC')
            NO-LOCK,
        FIRST banks OF banks-code
            NO-LOCK:

            cBenBName = banks.name.
            cBenBCntr = "(" + banks.country-id + ")".
        END.

        IF (cBenBName NE "")
        THEN DISPLAY cBenBName cBenBCntr WITH FRAME swdoc.
        ELSE DO:
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Банк не найден!").
            RETURN NO-APPLY.
        END.
    END.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cBenName IN FRAME swdoc
DO:
    ASSIGN cBenName.
    iBenCnt1 = LENGTH(cBenName).
    DISPLAY iBenCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F11 OF cBenName IN FRAME swdoc
DO:
    ASSIGN cBenName.
    cBenName = sw-trans(cBenName, YES, "RUR5").
    iBenCnt1 = LENGTH(cBenName).
    DISPLAY cBenName iBenCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F12 OF cBenName IN FRAME swdoc
DO:
    ASSIGN cBenName.
    cBenName = sw-trans(cBenName, NO, "RUR5").
    iBenCnt1 = LENGTH(cBenName).
    DISPLAY cBenName iBenCnt1 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cBenAdr IN FRAME swdoc
DO:
    ASSIGN cBenAdr.
    iBenCnt2 = LENGTH(cBenAdr).
    DISPLAY iBenCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F11 OF cBenAdr IN FRAME swdoc
DO:
    ASSIGN cBenAdr.
    cBenAdr  = sw-trans(cBenAdr, YES, "RUR5").
    iBenCnt2 = LENGTH(cBenAdr).
    DISPLAY cBenAdr iBenCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F12 OF cBenAdr IN FRAME swdoc
DO:
    ASSIGN cBenAdr.
    cBenAdr  = sw-trans(cBenAdr, NO, "RUR5").
    iBenCnt2 = LENGTH(cBenAdr).
    DISPLAY cBenAdr iBenCnt2 WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F1 OF cBenAcct, cBenName, cBenAdr IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN codelay.p ("Бенефициары", "Бенефициары", "Частые бенефициары", 3).
        cBenAcct = pick-value.
        cBenName = GetCodeName("Бенефициары", pick-value).
        cBenAdr  = GetCode("Бенефициары", pick-value).
        cBenCntr = GetCodeDesc("Бенефициары", pick-value, 1, "RUS").
        DISPLAY cBenAcct cBenName cBenAdr cBenCntr WITH FRAME swdoc.
    END.
END.

/* ON ******************************************* */
ON F5 OF cBenAcct, cBenName, cBenAdr IN FRAME swdoc
DO:
    ASSIGN cBenAcct cBenName cBenAdr.
    MESSAGE STRING(":59:/" + cBenAcct, "x(35)") + "~n"
          + STRING(TRIM(SUBSTRING(cBenName, 1, 35)), "x(35)") + "~n"
          + (IF (iBenCnt1 < 36) THEN "" ELSE (STRING(TRIM(SUBSTRING(cBenName, 36, 35)), "x(35)") + "~n"))
          + STRING(TRIM(SUBSTRING(cBenAdr,  1, 35)), "x(35)")
          + (IF (iBenCnt2  < 36) THEN "" ELSE ("~n" + STRING(TRIM(SUBSTRING(cBenAdr,  36, 35)), "x(35)")))
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.

/* ON ******************************************* */
ON F1 OF cBenCntr IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN count.p (3).
        IF (KEYFUNCTION(LASTKEY) EQ "GO")
            AND pick-value <> ?
        THEN DO:
            cBenCntr = pick-value.
            DISPLAY cBenCntr WITH FRAME swdoc.
        END.
    END.
END.

/* ON ******************************************* */
ON F1 OF cBenRF IN FRAME swdoc
DO:
    cBenRF = IF (cBenRF EQ ?) THEN YES ELSE NOT cBenRF.
    DISPLAY cBenRF WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F1 OF cBenUL IN FRAME swdoc
DO:
    cBenUL = IF (cBenUL EQ ?) THEN YES ELSE NOT cBenUL.
    DISPLAY cBenUL WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cDetails IN FRAME swdoc
DO:
    ASSIGN cDetails.
    iDetCnt = LENGTH(cDetails).
    DISPLAY iDetCnt WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F5 OF cDetails IN FRAME swdoc
DO:
    ASSIGN cDetails.
    MESSAGE STRING(":70:" +                               SUBSTRING(cDetails,   1, 35),  "x(39)") + "~n"
          + (IF (iDetCnt <  36) THEN "" ELSE (STRING(TRIM(SUBSTRING(cDetails,  36, 35)), "x(39)") + "~n"))
          + (IF (iDetCnt <  71) THEN "" ELSE (STRING(TRIM(SUBSTRING(cDetails,  71, 35)), "x(39)") + "~n"))
          + (IF (iDetCnt < 106) THEN "" ELSE (STRING(TRIM(SUBSTRING(cDetails, 106, 35)), "x(39)") + "~n"))
          + (IF (iDetCnt < 141) THEN "" ELSE (":72:" + Swift72("/NZP/" + SUBSTRING(cDetails, 141))))
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.

/* ON ******************************************* */
ON F11 OF cDetails IN FRAME swdoc
DO:
    ASSIGN cDetails.
    cDetails = sw-trans(cDetails, YES, "RUR5").
    iDetCnt = LENGTH(cDetails).
    DISPLAY cDetails iDetCnt WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON F12 OF cDetails IN FRAME swdoc
DO:
    ASSIGN cDetails.
    cDetails = sw-trans(cDetails, NO, "RUR5").
    iDetCnt = LENGTH(cDetails).
    DISPLAY cDetails iDetCnt WITH FRAME swdoc.
END.

/* ON ******************************************* */
ON LEAVE OF cDetails IN FRAME swdoc
DO:
    ASSIGN cDetails.
    IF (iDetCnt > 140) AND (INDEX(cSndInfo, "/NZP/") = 0)
    THEN DO:
        cSndInfo = cSndInfo + (IF (cSndInfo = "") THEN "" ELSE "~n")
                 + "/NZP/" + SUBSTRING(cDetails, 141).
        DISPLAY cSndInfo WITH FRAME swdoc.
    END.
END.

/* ON ******************************************* */
ON F1 OF cCharge IN FRAME swdoc
DO:
    DO TRANSACTION:
        RUN pclass.p ("SWIFT_PAY", "SWIFT_PAY", "РАСХОДЫ ОПЛАЧИВАЕТ", 6).
        IF      (KEYFUNCTION(LASTKEY) EQ "GO")
            AND (pick-value NE ?)
        THEN DO:
            cCharge = pick-value.
            DISPLAY cCharge WITH FRAME swdoc.
        END.
    END.
END.

/* ON ******************************************* */
ON INSERT OF cSndInfo IN FRAME swdoc
DO:
    ASSIGN cSndInfo.
    DO TRANSACTION:
        RUN pclass.p ("SWIFT_COD", "SWIFT_COD", "КОД СООБЩЕНИЯ", 6).
        IF      (KEYFUNCTION(LASTKEY) EQ "GO")
            AND (pick-value NE ?)
        THEN DO:
            cSndInfo = cSndInfo + "/" + pick-value + "/".
            DISPLAY cSndInfo WITH FRAME swdoc.
        END.
    END.
END.

/* ON ******************************************* */
ON F1 OF cSndInfo IN FRAME swdoc
DO:
    ASSIGN cSndInfo.
    IF (cSndInfo NE "")
    THEN MESSAGE ":72:" + Swift72(cSndInfo)
            VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.
