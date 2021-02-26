/*
процедура собирает временную таблицу для запуска 
процедуры создания уведомления в Нотариат (notar-cre.p)
Выборка делается либо по tmprecid.id кредитных договоров
либо из файла kd.txt
Параметры:
    iDate - Дата опердня, в котором будут выбираться проводки
*/
{globals.i}
{intrface.get xclass}
{sh-defs.i}

DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.

DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NFOLD FOR NOTIFICATION.

/* 'Сообщение в нотариат о появлении, изменении и прекращении залога' */
/* DEFINE BUFFER NF FOR PLEDGENOTIFICATION. */
/* Строка параметров для передачи в процедуру создания или изменения залога*/
DEFINE VARIABLE mParStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRet AS INT64 NO-UNDO.
DEFINE VARIABLE mreg-zalog-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNN AS INT64 NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidOb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst91312 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mNotifCreNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam AS CHARACTER EXTENT 2 NO-UNDO.

{tmprecid.def}

mParam[1] = ENTRY(1, iFile).
mParam[2] = IF NUM-ENTRIES(iFile) GT 1 THEN ENTRY(2, iFile) ELSE "req".

IF mParam[1] NE "tmp" THEN
DO:
    {empty tmprecid}
    INPUT FROM VALUE("./kd.txt") CONVERT TARGET "ibm866".
    REPEAT:
        IMPORT UNFORMATTED mStr.
        IF mStr NE "end" THEN
        DO:
            FIND FIRST loan
                WHERE loan.contract EQ "Кредит"
                AND loan.cont-code EQ mStr
                NO-LOCK NO-ERROR.
            IF AVAILABLE(loan) THEN
            DO:
                CREATE tmprecid.
                tmprecid.id = RECID(loan).
            END.
        END.
    END.
    INPUT CLOSE.
END.

DEFINE TEMP-TABLE ttNotif
    FIELD OpEntryDbOp AS INT64
    FIELD OpEntryCrOp AS INT64
    FIELD acct-db AS CHARACTER
    FIELD acct-cr AS CHARACTER
    FIELD contract AS CHARACTER
    FIELD cont-code AS CHARACTER
    FIELD RecLoan AS INT64
    FIELD RecTermObl AS INT64
    INDEX contract cont-code contract
    . 
    
/* 
Отработка сообщений
о возникновении залога
об изменении залога
*/
{setdest.i}
PUT UNFORMATTED "Обработанные договоры" SKIP.
FOR EACH tmprecid,
    FIRST loan
    WHERE RECID(loan) EQ tmprecid.id
    NO-LOCK:
    IF loan.close-date NE ? THEN /* Договор закрыт */
    DO:
        FIND FIRST NF 
            WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
            AND NF.NOTIFICATIONTYPE EQ 2
            AND NF.STATUS_ GE 6 
            AND NF.STATUS_ LE 7 
        NO-LOCK NO-ERROR.
        IF AVAILABLE(NF) THEN
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "уже есть уведомление о снятии в новой системе. Все равно создаем."
            SKIP.
/*        ELSE        */
        DO:
            FIND FIRST NFOLD
                WHERE NFOLD.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
                AND NFOLD.NOTIFICATIONTYPE EQ 2
                AND NFOLD.STATUS_ GE 4
                AND NFOLD.STATUS_ LE 5 
            NO-LOCK NO-ERROR.
            IF AVAILABLE(NFOLD) THEN
                PUT UNFORMATTED 
                ENTRY(1, loan.cont-code, "@") 
                " ; "
                "уже есть уведомление о снятии в старой системе. Все равно создаем."
                SKIP.
/*            ELSE */
            DO:
                FIND FIRST NF 
                    WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
                    AND NF.NOTIFICATIONTYPE EQ 1
                    AND NF.STATUS_ GE 6 
                    AND NF.STATUS_ LE 7 
                NO-LOCK NO-ERROR.
                FIND FIRST NFOLD
                    WHERE NFOLD.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
                    AND NFOLD.NOTIFICATIONTYPE EQ 1
                    AND NFOLD.STATUS_ GE 4
                    AND NFOLD.STATUS_ LE 5 
                NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(NFOLD) AND NOT AVAILABLE(NF) THEN
                        PUT UNFORMATTED 
                        ENTRY(1, loan.cont-code, "@") 
                        " ; "
                        "нет уведомления о постановке"
                        SKIP.
                ELSE
                DO:
                    FOR EACH term-obl
                        WHERE term-obl.contract EQ loan.contract
                        AND term-obl.cont-code EQ loan.cont-code
                        AND term-obl.idnt EQ 5
                        NO-LOCK BY term-obl.fop-date DESC:
                        LEAVE.
                    END.
                    IF AVAILABLE(term-obl) THEN
                    DO:
                        IF mParam[2] NE "req" THEN
                            PUT UNFORMATTED 
                            ENTRY(1, loan.cont-code, "@") 
                            " ; "
                            "Снятие"
                            SKIP.
                        ELSE
                        DO:
                            mParStr = loan.contract + "," + loan.cont-code + "," + TRIM(STRING(RECID(loan))) + "," + TRIM(STRING(RECID(term-obl))) + ",2".
                            RUN notar-cre.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
                            IF mNotifRet NE 0 THEN
                            DO:
                                PUT UNFORMATTED 
                                ENTRY(1, loan.cont-code, "@") 
                                " ; "
                                "Снятие"
                                SKIP.
                                UpdateSigns("term-obl-gar", 
                                term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                "NotifId",
                                STRING(mNotifRet), 
                                ?).
                            END.
                            ELSE
                                PUT UNFORMATTED 
                                ENTRY(1, loan.cont-code, "@") 
                                " ; "
                                "нет действий"
                                SKIP.
                        END.
                    END.
                END.
            END.
        END.
    END.    
    ELSE /* договор открыт */
    DO:
        FOR EACH loan-acct
            WHERE loan-acct.contract EQ loan.contract
            AND loan-acct.cont-code EQ loan.cont-code
            AND loan-acct.acct-type BEGINS "КредОб"
        NO-LOCK
        BY loan-acct.since DESC:
            mNN = IF loan-acct.acct-type EQ "КредОб" THEN 0 ELSE INT64(SUBSTRING(loan-acct.acct-type, 7)).
            FIND FIRST term-obl
                WHERE term-obl.contract EQ loan.contract
                AND term-obl.cont-code EQ loan.cont-code
                AND term-obl.idnt EQ 5
                AND term-obl.nn EQ mNN
                NO-LOCK NO-ERROR.
            IF AVAILABLE(term-obl) THEN
            DO:
                RUN acct-pos IN h_base (loan-acct.acct,
                                        loan.currency,
                                        TODAY,
                                        TODAY,
                                        "П").
                IF loan.currency EQ "" THEN
                    mOst91312 = sh-bal.
                ELSE
                    mOst91312 = sh-val.
                IF mOst91312 NE 0 THEN
                DO:
                        mNotifCreNumber = GetXAttrValueEx("term-obl", 
                                            term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                            "NotifRefNumber",
                                            ?).
                        IF mNotifCreNumber EQ ? THEN
                        DO:
                            mNotifCreNumber = GetXAttrValueEx("term-obl",
                                            term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                                "reg-zalog",
                                                ?).
                        END.
                        IF mNotifCreNumber EQ ? THEN
                        DO:
                            IF mParam[2] NE "req" THEN
                                PUT UNFORMATTED 
                                ENTRY(1, loan.cont-code, "@") 
                                " ; "
                                (IF term-obl.nn GT 0 THEN "Замена" ELSE "Постановка")                                    
                                SKIP.
                            ELSE
                            DO:
                                mParStr = loan.contract + "," + loan.cont-code + "," + TRIM(STRING(RECID(loan))) + "," + TRIM(STRING(RECID(term-obl))) + 
                                (IF term-obl.nn GT 0 THEN ",3" ELSE ",1").
                                RUN notar-cre.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
                                IF mNotifRet NE 0 THEN
                                DO:
                                    PUT UNFORMATTED 
                                    ENTRY(1, loan.cont-code, "@") 
                                    " ; "
                                    (IF term-obl.nn GT 0 THEN "Замена" ELSE "Постановка")                                    
                                    SKIP.
                                    UpdateSigns("term-obl-gar", 
                                    term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                    "NotifId",
                                    STRING(mNotifRet), 
                                    ?).
                                END.
                                ELSE
                                    PUT UNFORMATTED 
                                    ENTRY(1, loan.cont-code, "@") 
                                    " ; "
                                    "Постановка/замена не прошла"
                                    SKIP.
                            END.
                        END.
                        ELSE
                            PUT UNFORMATTED 
                            ENTRY(1, loan.cont-code, "@") 
                            " ; "
                            (IF term-obl.nn GT 0 THEN "Замена" ELSE "Постановка")
                            " уже есть."                                    
                            SKIP.
                END.
                ELSE
                DO:
                    IF mParam[2] NE "req" THEN
                        PUT UNFORMATTED 
                        ENTRY(1, loan.cont-code, "@") 
                        " ; "
                        "Остаток на 91312 = 0, возможно только снятие"
                        SKIP.
                    ELSE
                    DO:
                        mParStr = loan.contract + "," + loan.cont-code + "," + TRIM(STRING(RECID(loan))) + "," + TRIM(STRING(RECID(term-obl))) + ",2".
                        RUN notar-cre.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
                        
                        IF mNotifRet NE 0 THEN
                        DO:
                            PUT UNFORMATTED 
                            ENTRY(1, loan.cont-code, "@") 
                            " ; "
                            "снятие"
                            SKIP.
                            UpdateSigns("term-obl-gar", 
                            term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                            "NotifId",
                            STRING(mNotifRet), 
                            ?).
                            
                        END.
                        ELSE
                            PUT UNFORMATTED 
                            ENTRY(1, loan.cont-code, "@") 
                            " ; "
                            "Нет действий"
                            SKIP.
                    END.
                END.
            END.
        END.
    END.
END.

{intrface.del}
{preview.i}
