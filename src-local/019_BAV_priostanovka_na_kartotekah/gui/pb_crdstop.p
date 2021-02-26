{globals.i}
{intrface.get tmess}

/* +++ pb_crdstop.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:24am +++ */

/**
Авторские права принадлежат: ПАО Плюс банк
Базируется:     crdstopcop.p
Основание:      ОСРКО.184
Что делает:     Процедура приостановки списания с Картотек
Как работает:   Изменен браузер документов на картотеках
Место запуска:  
Создан:         05.07.2016 Борисов А.В.
*/

{globals.i}

{intrface.get xclass}
DEFINE TEMP-TABLE ttOpBal   NO-UNDO
    FIELD cart      AS CHARACTER
    FIELD sort      AS CHARACTER
    FIELD op-bal    AS CHARACTER
    FIELD op-date   AS DATE
    FIELD doc-num   AS CHARACTER
    FIELD doc-type  AS CHARACTER
    FIELD order-pay AS CHARACTER
    FIELD summ      AS DECIMAL
    FIELD details   AS CHARACTER
    FIELD vb-op     AS CHARACTER
    FIELD vb-num    AS CHARACTER
    FIELD priost    AS CHARACTER
    .

{ch_cart.i}

DEFINE INPUT PARAM iKauIdLst AS CHAR NO-UNDO.

DEFINE VARIABLE mAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDR      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRID     AS RECID     NO-UNDO.
DEFINE VARIABLE mIsAmbig AS LOGICAL   NO-UNDO.

DEFINE VARIABLE mDateB   AS DATE      NO-UNDO.
DEFINE VARIABLE mDateE   AS DATE      NO-UNDO.

DEFINE BUFFER oacct FOR acct.
DEFINE BUFFER opb   FOR op.
DEFINE BUFFER oe    FOR op-entry.

FORM
    SKIP
    mAcct LABEL "Лицевой счет" FORMAT "x(25)" HELP "Лицевой счет"
    SKIP
    WITH FRAME fAcct CENTERED ROW 10 OVERLAY SIDE-LABELS.

ON F1 OF mAcct IN FRAME fAcct
DO:
    DO TRANSACTION:
        RUN browseld.p("acctb", "cust-cat~001sc-1~001sc-2~001ORAND", "Ю,Ч,Б~001Карт2ВнСчет~001КартБВнСчет~0011", "", 4).

        IF ({&LAST_KEY} NE KEYCODE("ESC")) AND (pick-value NE ?)
        THEN mAcct:SCREEN-VALUE = pick-value.
    END.
END.

IF NOT {assigned iKauIdLst}
THEN iKauIdLst = "Карт-ка2".

PAUSE 0.
{update.i &THIS_FRAME = "fAcct" &EXFILE = "pb_crdstop.p.st1" {&*}} .
HIDE FRAME fAcct.

IF {&LAST_KEY} EQ KEYCODE("ESC") OR NOT {assigned mAcct}
THEN RETURN.

mAcct = REPLACE(mAcct, "-", "").
{find-act.i &acct = mAcct}

IF NOT AVAILABLE acct
THEN DO:
    RUN Fill-AlertSysMes IN h_tmess("","",-1,"Счет" + CHR(32) + STRING(mAcct) + CHR(32) + "не найден").

    RETURN.
END.

{empty ttOpBal}
RUN ScanCrt("К2").  /* Собираем документы из К2  */
RUN ScanCrt("КБС"). /* Собираем документы из КБС */

IF NOT CAN-FIND(FIRST ttOpBal)
THEN DO:
    RUN Fill-AlertSysMes IN h_tmess("","",-1,"Документы на картотеках не обнаружены").

    RETURN.
END.
/*
mTmpObjHand = TEMP-TABLE TmpObj:HANDLE.
RUN browseld.p ("op",
                "UseTmpObjInQuery~001op-date1~001op-date2~001FirstFrame~001ViewSC~001BrwRole~001Title",
                STRING(mTmpObjHand) + CHR(1) + STRING(mDateB) + CHR(1) + STRING(mDateE) + "~00116~001ПриостСпис~001StopWritting~001" + SUBSTRING(mAcct, 1, 20),
                "", 4).
*/

/* Визуализация списка документов ============================================= */
/* query для браузера загрузок */
DEFINE QUERY  qOpBal FOR ttOpBal SCROLLING.
DEFINE BROWSE bOpBal QUERY qOpBal NO-LOCK
    DISPLAY
        ttOpBal.cart      FORMAT 'x(3)'     COLUMN-LABEL 'КРТ'
        ttOpBal.order-pay FORMAT 'x(1)'     COLUMN-LABEL 'О'
        ttOpBal.op-date   FORMAT '99/99/99' COLUMN-LABEL '  ДАТА'
        ttOpBal.doc-type  FORMAT 'x(3)'     COLUMN-LABEL 'КОД'
        ttOpBal.doc-num   FORMAT 'x(10)'    COLUMN-LABEL 'НОМЕР ДОК'
        ttOpBal.summ      FORMAT '>,>>>,>>9.99' COLUMN-LABEL 'СУММА    '
        ttOpBal.details   FORMAT 'x(25)'    COLUMN-LABEL '      СОДЕРЖАНИЕ'
/*      ttOpBal.vb-num    FORMAT 'x(10)'    COLUMN-LABEL 'НОМЕР В/Б'
*/      ttOpBal.priost    FORMAT 'x(6)'     COLUMN-LABEL 'ПРИОСТ'
    WITH 14 DOWN WIDTH 78 NO-ASSIGN NO-BOX.
DEFINE FRAME fOpBal
    bOpBal
WITH OVERLAY CENTERED ROW 5 
TITLE COLOR BRIGHT 'ПРИОСТАНОВКА ДОКУМЕНТОВ НА КАРТОТЕКАХ'.

/* Заглушки. Иначе отчет закрывается */
ON F3, F4 OF bOpBal {return_no_apply.i}

ON F5, " " OF bOpBal DO:
    IF (AVAIL ttOpBal)
    THEN DO:
        ttOpBal.priost = IF (ttOpBal.priost EQ "") THEN "Да" ELSE "".
        UpdateSigns ("op", ttOpBal.vb-op, "ПриостСпис", ttOpBal.priost, YES).
        bOpBal:REFRESH().
    END.
END.

ON F1 OF bOpBal DO:
    IF (AVAIL ttOpBal)
    THEN DO: /* RUN formld.p ("opb", ttOpBal.op-bal, "", "{&MOD_VIEW}", 2) NO-ERROR. */
        FIND FIRST op
            WHERE (op.op    EQ INT64(ttOpBal.op-bal))
            NO-LOCK NO-ERROR.
        IF (AVAIL op)
        THEN DO:
            RUN RunClassMethod in h_xclass (op.class-code, "Look", "", "", "",
                                            "?," + STRING(op.user-id) + "," + ttOpBal.op-bal + ",2").
            RUN BRefresh.
        END.
    END.
END.

/* ============================================================================ */
/* обновление браузера документов */
PROCEDURE BRefresh:
    CLOSE QUERY qOpBal.
    OPEN  QUERY qOpBal
        FOR EACH ttOpBal
            BY ttOpBal.cart
            BY ttOpBal.order-pay
            BY ttOpBal.sort
            BY ttOpBal.vb-op.
    ENABLE ALL WITH FRAME fOpBal.
    PUT SCREEN
        ROW FRAME fOpBal:ROW + FRAME fOpBal:HEIGHT-CHARS - 1
        COL FRAME fOpBal:COL + 1
        'F1 Просмотр|F5, пробел - приостановить/отменить приостановку|ESC Выход      '
        COLOR MESSAGES.
    /* если есть хотя бы 1 запись - то рефрешим браузер */
    IF CAN-FIND(FIRST ttOpBal)
    THEN bOpBal:REFRESH().

    APPLY "HOME" TO BROWSE bOpBal.
END PROCEDURE.

/* Обработка отчета */
PAUSE 0.
RUN BRefresh.

{wait_for.i &THIS_FRAME = "fOpBal" &ENTRY_FOCUS = "bOpBal" &EXFILE = "pb_crdstop.p.wf1" {&*}} .
HIDE FRAME fOpBal.

{intrface.del}

/* Документы одной картотеки ************************************************** */
PROCEDURE ScanCrt:
    DEFINE INPUT  PARAMETER iCrt   AS CHARACTER    NO-UNDO. /* К2 или КБС */

    RELEASE oacct.
    cDR = GetXAttrValue("acct", acct.acct + "," + acct.currency, IF (iCrt EQ "К2") THEN "Карт2ВнСчет" ELSE "КартБВнСчет").
    IF (cDR NE "")
    THEN FIND oacct
            WHERE oacct.acct        EQ ENTRY(1, cDR)
              AND oacct.currency    EQ ENTRY(2, cDR) 
            NO-LOCK NO-ERROR.

    IF AVAILABLE oacct
    THEN
        FOR EACH kau
            WHERE kau.acct      EQ oacct.acct
              AND kau.currency  EQ oacct.currency
              AND kau.zero-bal  EQ NO  
            NO-LOCK,
        FIRST op
            WHERE op.op         EQ INT64(ENTRY(1, kau.kau))
            NO-LOCK,
        FIRST op-entry OF op
            NO-LOCK:

            cDR = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").
            FIND FIRST opb
                WHERE (opb.op   EQ INT64(cDR))
                NO-LOCK NO-ERROR.
            IF (AVAIL opb)
            THEN FIND FIRST oe OF opb
                    NO-LOCK NO-ERROR.

            CREATE ttOpBal.
            ASSIGN
                ttOpBal.cart        = iCrt
                ttOpBal.sort        = kau.sort
                ttOpBal.op-bal      = cDR
                ttOpBal.op-date     = IF ((AVAIL opb) AND (opb.doc-date NE ?)) THEN opb.doc-date ELSE op.op-date
                ttOpBal.doc-num     = IF (AVAIL opb) THEN opb.doc-num   ELSE "?"
                ttOpBal.doc-type    = IF (AVAIL opb) THEN opb.doc-type  ELSE GetXAttrValue("op", ENTRY(1, kau.kau), "ВидОп")
                ttOpBal.order-pay   = IF (AVAIL opb) THEN opb.order-pay ELSE op.order-pay
                cDR                 = GetXAttrValue("op", cDR, "amt-rub")
                ttOpBal.summ        = IF (cDR NE "") THEN DEC(cDR)      ELSE ( IF (AVAIL oe) THEN oe.amt-rub ELSE (
                                      IF (NUM-ENTRIES(kau.sort) GE 3) THEN DEC(TRIM(ENTRY(3, kau.sort))) ELSE kau.balance))
                ttOpBal.details     = IF (AVAIL opb) THEN opb.details   ELSE ""
                ttOpBal.vb-op       = ENTRY(1, kau.kau)
                ttOpBal.vb-num      = op.doc-num
                ttOpBal.priost      = GetXAttrValue("op", ENTRY(1, kau.kau), "ПриостСпис")
                .
            RELEASE oe.
        END.
END PROCEDURE.
/* --- pb_crdstop.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:24am --- */
