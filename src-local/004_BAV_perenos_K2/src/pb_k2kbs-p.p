/**
Авторские права принадлежат: ПАО "Плюс Банк"
Что делает:     Принудительный перенос документов К2 - КБС
Параметры:      
Место запуска:  Меню Ctrl-G  счетов
Создан:         12.04.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get tmess}
{tmprecid.def}
{sh-defs.i}             /* остаток на счете */
{topkind.def}
{pb_logit.i}            /* Запись в лог */
{pb_k2init.i}

DEFINE VARIABLE cAcct       AS CHARACTER    NO-UNDO INIT "".    /* р/с */
DEFINE VARIABLE cCurr       AS CHARACTER    NO-UNDO.            /* вал */
DEFINE VARIABLE cAcctK2     AS CHARACTER    NO-UNDO.            /* K2  */
DEFINE VARIABLE cAcctKbs    AS CHARACTER    NO-UNDO.            /* КБС */
DEFINE VARIABLE nK2Pos      AS DECIMAL      NO-UNDO INIT 0.     /* Остаток на К2 */
DEFINE VARIABLE nKbsPos     AS DECIMAL      NO-UNDO INIT 0.     /* Остаток на КБС */
DEFINE VARIABLE cKrcv       AS CHARACTER    NO-UNDO INIT "".    /* Картотека - получатель */
DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.            /* Назначение платежа исх.док. */
DEFINE VARIABLE cTranz      AS CHARACTER    NO-UNDO.            /* Запуск тр-ции sbk_2kbs */
DEFINE VARIABLE lOk         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMess       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO.            /* Счетчик перенесенных документов */
DEFINE BUFFER ac     FOR acct.

cMess = "====== Перенос документов К2 - КБС".
RUN LogIt(cMess, cLog).

FOR FIRST tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct) EQ tmprecid.id)
    NO-LOCK:

    cMess = "  Отмечен счет " + acct.acct.
    RUN LogIt(cMess, cLog).

    IF (acct.acct BEGINS "9090")
    THEN DO:    /* Выбран счет Картотеки */
        IF NOT CAN-DO("Карт2,КартБл", acct.contract)
        THEN DO:
            cMess = "Выбранный счет не является К2 или КБС !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.

        RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "Ф").
        IF (sh-bal EQ 0)
        THEN DO:
            cMess = "На выбранной картотеке нет документов !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.

        cKrcv = ENTRY(LOOKUP(acct.contract, "Карт2,КартБл"), "КБС,К2").

        FOR EACH signs
            WHERE (signs.file-name   EQ "acct")
              AND (signs.code        EQ ENTRY(LOOKUP(acct.contract, "Карт2,КартБл"), "Карт2ВнСчет,КартБВнСчет"))
              AND (IF (acct.contract EQ "Карт2") THEN
                  (signs.xattr-value EQ (acct.acct + "," + acct.currency)) ELSE
                  (signs.code-value  EQ (acct.acct + "," + acct.currency)))
            NO-LOCK,
        FIRST ac
            WHERE (ac.acct           EQ ENTRY(1, signs.surrogate))
              AND (ac.currency       EQ ENTRY(2, signs.surrogate))
              AND (ac.close-date     EQ ?)
            NO-LOCK:

            cAcct = ac.acct.
            cCurr = ac.currency.
        END.

        IF (cAcct EQ "")
        THEN DO:
            cMess = "Выбранная картотека не привязана ни к одному р/с !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.
    END.
    ELSE DO:    /* Выбран р/с */
        cAcct = acct.acct.
        cCurr = acct.currency.
    END.
END.

/* Ищем блокировки КонкПр или Банкрот ***************************************** */
FIND FIRST blockobject
    WHERE (blockobject.file-name     EQ 'acct')
      AND (blockobject.class-code    EQ 'BlockAcct')
      AND CAN-DO("КонкПр,Банкрот", blockobject.block-type)
      AND (blockobject.beg-datetime  LE DATETIME(TODAY, MTIME))
      AND ((blockobject.end-datetime EQ ?)
        OR (blockobject.end-datetime GE DATETIME(TODAY, MTIME)))
      AND (blockobject.surrogate     EQ cAcct + "," + cCurr)
    NO-LOCK NO-ERROR.
IF (NOT AVAIL blockobject)
THEN DO:
    cMess = "На р/с " + DelFilFromAcct(cAcct) + " отсутствуют блокировки КонкПр или Банкрот !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

cAcctK2  = ENTRY(1, GetXAttrValue("acct", cAcct + "," + cCurr, "Карт2ВнСчет")).
cAcctKbs = ENTRY(1, GetXAttrValue("acct", cAcct + "," + cCurr, "КартБВнСчет")).

IF (cAcctK2 EQ "")
THEN DO:
    cMess = "На р/с " + DelFilFromAcct(cAcct) + " отсутствует Картотека 2 !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

/* Если нет КБС, то перенос с К2 на КБС */
IF (cAcctKbs EQ "")
THEN cKrcv = "КБС".
ELSE DO:
    RUN acct-pos IN h_base(cAcctKbs, "", TODAY, TODAY, "Ф").
    nKbsPos = sh-bal.
END.

RUN acct-pos IN h_base(cAcctK2, "", TODAY, TODAY, "Ф").
nK2Pos = sh-bal.

IF (nK2Pos + nKbsPos EQ 0)
THEN DO:
    cMess = "На картотеках р/с " + DelFilFromAcct(cAcct) + " нет документов !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

IF (cKrcv EQ "")
THEN DO:
    IF (nKbsPos EQ 0)
    THEN cKrcv = "КБС".
    IF (nK2Pos  EQ 0)
    THEN cKrcv = "К2".

    IF (cKrcv EQ "")
    THEN DO:            /* Спрашиваем, откуда и куда переносить */
        RUN Fill-SysMes IN h_tmess ("", "", "3",
            "В каком направлении будем переносить документы?|С К2 на КБС,с КБС на К2").
        CASE pick-value:
            WHEN "0" THEN RETURN.
            WHEN "1" THEN cKrcv = "КБС".
            WHEN "2" THEN cKrcv = "К2".
        END CASE.
    END.
END.

/* Помечаем документы на исходной картотеке *********************************** */
cMess = "    Отображаем аналитику " + cKrcv.
RUN LogIt(cMess, cLog).
cAcctK2 = IF (cKrcv EQ "К2") THEN cAcctKbs ELSE cAcctK2.
run "crd.p" (cAcctK2, "", 4).
iNum = 0.

FOR EACH tmprecid 
    NO-LOCK,
FIRST kau
    WHERE (RECID(kau) EQ tmprecid.id)
    NO-LOCK:

    cDet = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").    /* Исходный документ, поставленный на картотеку */
    IF (cDet NE "")
    THEN DO:
        FIND FIRST op
            WHERE (op.op    EQ INT64(cDet))
            NO-LOCK NO-ERROR.

        IF (AVAIL op)
        THEN cDet = op.details.
        ELSE cDet = "".
    END.

    /* Запуск транзакции  sbk_2kbs - SB: Перемещение К2 <--> КБС */
    cTranz = "sbk_2kbs".
    {empty tOpKindParams}     /* очистить таблицу параметров */
    ASSIGN
        lOk =   TDAddParam("iSrcCrt"   , cAcctK2)
            AND TDAddParam("iCrtType"  , cKrcv)
            AND TDAddParam("iAcct"     , cAcct)
            AND TDAddParam("iCurrency" , cCurr)
            AND TDAddParam("iKau"      , kau.kau)
            AND TDAddParam("iDetails"  , cDet)
            AND TDAddParam("iUser"     , USERID('bisquit'))
            AND TDAddParam("iLog"      , cLog)
        NO-ERROR.
    IF NOT lOk
    THEN DO:
        cMess = "Ошибка передачи параметров в транзакцию " + cTranz.
        RUN LogIt("???  " + cMess, cLog).
        MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ELSE DO:
        RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
        IF NOT lOk
        THEN DO:
            cMess = cErrMsg.
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        END.
        ELSE DO:
            cMess = "    Документ перенесен на " + cKrcv.
            RUN LogIt(cMess, cLog).
            iNum  = iNum + 1.
        END.
    END.
END.

cMess = "Перенесено " + STRING(iNum) + " документов на " + cKrcv.
RUN LogIt(cMess, cLog).
MESSAGE cMess VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
