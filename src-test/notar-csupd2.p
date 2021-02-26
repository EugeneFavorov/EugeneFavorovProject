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
/*
DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
*/
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
/*DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NFOLD FOR NOTIFICATION.
*/
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

{tmprecid.def}

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
/*    IF loan.close-date NE ? THEN /* Договор закрыт */
        PUT UNFORMATTED 
        ENTRY(1, loan.cont-code, "@") 
        " ; "
        " договор закрыт, не создаем уведомление."
        SKIP.
    ELSE
*/
    DO:
/*        FIND FIRST NF 
            WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
            AND NF.NOTIFICATIONTYPE EQ 3
            AND NF.STATUS_ GE 6 
            AND NF.STATUS_ LE 7 
        NO-LOCK NO-ERROR.
        IF AVAILABLE(NF) THEN
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "уже есть уведомление о снятии в новой системе"
            SKIP.
        ELSE       */
        DO:
/*            FIND FIRST NF 
                WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
                AND NF.NOTIFICATIONTYPE EQ 1
                AND NF.STATUS_ GE 6 
                AND NF.STATUS_ LE 7
                AND NF.CREATIONREFERENCENUMBER NE ? 
            NO-LOCK NO-ERROR.
            FIND FIRST NFOLD
                WHERE NFOLD.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
                AND NFOLD.NOTIFICATIONTYPE EQ 1
                AND NFOLD.STATUS_ GE 4
                AND NFOLD.STATUS_ LE 5 
                AND NF.CREATIONREFERENCENUMBER NE ? 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(NFOLD) AND NOT AVAILABLE(NF) THEN
                    PUT UNFORMATTED 
                    ENTRY(1, loan.cont-code, "@") 
                    " ; "
                    "нет уведомления о постановке"
                    SKIP.
            ELSE */
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
/* entry в строке
5 - тип уведомления
6 - статус
7 - aplicatnid
*/
                    mParStr = loan.contract + "," + loan.cont-code + "," + TRIM(STRING(RECID(loan))) + "," + TRIM(STRING(RECID(term-obl))) + ",3,2,2".
                    RUN notar-cre2.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
                    IF mNotifRet NE 0 THEN
                    DO:
                        PUT UNFORMATTED 
                        ENTRY(1, loan.cont-code, "@") 
                        " ; "
                        "Замена"
                        SKIP.
                        UpdateSigns("term-obl-gar", 
                        term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                        "NotifId",
                        STRING(mNotifRet), 
                        ?).
                        FOR EACH loan-acct
                            WHERE loan-acct.contract EQ loan.contract
                            AND loan-acct.cont-code EQ loan.cont-code
                            AND loan-acct.acct-type EQ "КредОб" + IF term-obl.nn EQ 0 THEN "" ELSE TRIM(STRING(term-obl.nn))
                            NO-LOCK
                            BY loan-acct.since DESC:
                            LEAVE.
                        END.
                        IF AVAILABLE(loan-acct) THEN
                        DO:
                            FOR EACH op-entry
                                WHERE op-entry.filial-id EQ loan.filial-id
                                AND op-entry.acct-db EQ loan-acct.acct
                                AND op-entry.kau-db BEGINS loan-acct.contract + "," + loan-acct.cont-code + ","
                                NO-LOCK
                                BY op-entry.op-date DESC:
                                LEAVE.
                            END.
                            IF AVAILABLE(op-entry) THEN
                            DO:
                                UpdateSigns("opo", 
                                            STRING(op-entry.op), 
                                            "NotifId",
                                            STRING(mNotifRet), 
                                            ?).
                            END.
                        END.
                    END.
                    ELSE
                        PUT UNFORMATTED 
                        ENTRY(1, loan.cont-code, "@") 
                        " ; "
                        "не создано уведомление"
                        SKIP.
                END.
            END.
        END.
    END.    
END.

{intrface.del}
{preview.i}

