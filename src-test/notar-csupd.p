/*
процедура собирает временную таблицу для запуска 
процедуры создания уведомления в Нотариат (notar-cre.p)
Выборка делается по проводкам постановки/снятия залога конкретного опердня
Параметры:
    iDate - Дата опердня, в котором будут выбираться проводки
*/
{globals.i}
{intrface.get xclass}
{intrface.get lngar}

DEFINE INPUT  PARAMETER iDate AS DATE NO-UNDO.

DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan FOR loan.
/* DEFINE BUFFER term-obl FOR term-obl. */

/* 'Сообщение в нотариат о появлении, изменении и прекращении залога' */
/* DEFINE BUFFER NF FOR PLEDGENOTIFICATION. */
/* Строка параметров для передачи в процедуру создания или изменения залога*/
DEFINE VARIABLE mParStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRet AS INT64 NO-UNDO.
DEFINE VARIABLE mreg-zalog-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNN AS INT64 NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidObNotar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPriznak AS CHARACTER NO-UNDO.

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

FOR EACH op-entry
    WHERE op-entry.filial-id EQ shFilial
    AND op-entry.op-date EQ iDate
    AND op-entry.acct-cr BEGINS "91312"
    AND op-entry.op-status GE CHR(251) 
    NO-LOCK:
    mreg-zalog-no = GetXAttrValueEx("op",
                    STRING(op-entry.op),
                    "NotifId",
                    "0"
                    ).
    IF mreg-zalog-no EQ "0" THEN
    DO:
        FOR EACH loan-acct
            WHERE loan-acct.contract EQ ENTRY(1, op-entry.kau-cr)
            AND loan-acct.cont-code EQ ENTRY(2, op-entry.kau-cr)
            AND loan-acct.acct EQ op-entry.acct-cr
            AND loan-acct.acct-type BEGINS "КредОб"
            NO-LOCK
            BY loan-acct.since DESC:
            LEAVE.
        END.
        IF AVAILABLE(loan-acct)
            AND GetXAttrValueEx("loan",
                                loan-acct.contract + "," + loan-acct.cont-code,
                                "Priznak",
                                "0"
            ) EQ "0"
            THEN
        DO:
/*            mNN = IF loan-acct.acct-type EQ "КредОб" THEN 0 ELSE INT64(SUBSTRING(loan-acct.acct-type, 7)). */ 
         
           mTermSurr = "0".   
           RUN GetGarByAcct IN h_lngar (loan-acct.contract,
                                        loan-acct.cont-code,
                                        loan-acct.acct,
                                        IF SUBSTRING(loan-acct.acct, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(loan-acct.acct, 6, 3),
                                        iDate,
                                        OUTPUT mTermSurr).            
            IF mTermSurr NE "0" THEN
            DO:
                FOR EACH term-obl
                    WHERE term-obl.contract EQ ENTRY(1, mTermSurr)
                    AND term-obl.cont-code EQ ENTRY(2, mTermSurr)
                    AND term-obl.idnt EQ INT64(ENTRY(3, mTermSurr))
                    AND term-obl.end-date = DATE(ENTRY(4, mTermSurr))
                    AND term-obl.nn = INT64(ENTRY(5, mTermSurr))
                    NO-LOCK:
                    LEAVE.
                END.
/*                mTermSurr = term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn). */ 
                mVidObNotar = GetXAttrValueEx("term-obl", 
                                        mTermSurr, 
                                        "ВидОб",
                                        "0").
                IF mVidObNotar EQ "Автомобиль"
                THEN
                DO:
                    FOR EACH loan
                        WHERE loan.contract EQ loan-acct.contract 
                        AND loan.cont-code EQ loan-acct.cont-code
                        NO-LOCK:
                        LEAVE.
                    END.
/* Вставка для тестирования и демонстрации */   
/*IF CAN-DO("22-00-63141-АПН@0000,45-00-51773-ГАПН@0000,50-00-64567-ДПН@0000", loan.cont-code) THEN
DO: */                  
/* Вставка для тестирования и демонстрации конец*/                    
                    CREATE ttNotif.
                    ASSIGN
                    ttNotif.OpEntryCrOp = op-entry.op
                    ttNotif.contract = loan-acct.contract
                    ttNotif.cont-code = loan-acct.cont-code
                    ttNotif.RecLoan = RECID(loan)
                    ttNotif.RecTermObl = RECID(term-obl)
                    ttNotif.OpEntryDbOp = ?
                    .
                    FOR EACH op-entry1
                        WHERE op-entry1.filial-id EQ shFilial
                        AND op-entry1.op-date EQ iDate
                        AND op-entry1.acct-db BEGINS "91312"
                        AND op-entry1.acct-db NE op-entry.acct-cr
                        AND op-entry1.op-status GE CHR(251) 
                        AND op-entry1.kau-db BEGINS loan-acct.contract + "," + loan-acct.cont-code  
                        NO-LOCK:
                        LEAVE.
                    END.
                    IF AVAILABLE(op-entry1) THEN
                    DO:
                        ttNotif.OpEntryDbOp = op-entry1.op.
                    END.
/*                    
END.
*/                    
                END.
            END.
        END.
    END.
END.
/* ---- */
FOR EACH ttNotif BY ttNotif.cont-code:
    mParStr = ttNotif.contract + "," + ttNotif.cont-code + "," + TRIM(STRING(ttNotif.RecLoan)) + "," + TRIM(STRING(ttNotif.RecTermObl)) +
              (IF ttNotif.OpEntryDbOp NE ? THEN ",3" ELSE ",1").
    RUN notar-cre.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
    IF mNotifRet NE 0 THEN
    DO:
/*        FIND FIRST term-obl 
            WHERE RECID(term-obl) EQ ttNotif.RecTermObl
            NO-LOCK NO-ERROR.
        IF AVAILABLE(term-obl) THEN    
        DO: */
            UpdateSigns("term-obl-gar", 
            mTermSurr,
            "NotifId",
            STRING(mNotifRet), 
            ?).
/*        END. */    
        UpdateSigns("opo", 
                    STRING(ttNotif.OpEntryCrOp), 
                    "NotifId",
                    STRING(mNotifRet), 
                    ?).
        IF ttNotif.OpEntryDbOp NE ? THEN
        DO:
            UpdateSigns("opo",
                    STRING(ttNotif.OpEntryDbOp), 
                    "NotifId",
                    STRING(mNotifRet), 
                    ?).
        END.
    END.
END.    
/* 
Отработка сообщений
о прекращении залога
*/
{empty ttNotif}
FOR EACH op-entry
    WHERE op-entry.filial-id EQ shFilial
    AND op-entry.op-date EQ iDate
    AND op-entry.acct-db BEGINS "91312"
    AND op-entry.op-status GE CHR(251) 
    NO-LOCK:
    FIND FIRST op
        WHERE op.op EQ op-entry.op
    NO-LOCK NO-ERROR.
    IF AVAILABLE(op) 
	AND CAN-DO("!prod_2016,!prod_2016_Tum,!prod_2016RC,!prod_2016Al,!prod_2017Nrv,!prod_2016Ап,!prod_2016ап55,!prod_2016АпТм,!prod_2016*,*", op.op-kind) THEN
    DO:
        mreg-zalog-no = GetXAttrValueEx("op",
                        STRING(op-entry.op),
                        "NotifId",
                        "0"
                        ).
        IF mreg-zalog-no EQ "0" THEN
        DO:
            FOR EACH loan-acct
                WHERE loan-acct.contract EQ ENTRY(1, op-entry.kau-db)
                AND loan-acct.cont-code EQ ENTRY(2, op-entry.kau-db)
                AND loan-acct.acct EQ op-entry.acct-db
                AND loan-acct.acct-type BEGINS "КредОб"
                NO-LOCK
                BY loan-acct.since DESC:
                LEAVE.
            END.
            IF AVAILABLE(loan-acct) THEN
            DO:
                mPriznak = GetXAttrValueEx("loan",
                                loan-acct.contract + "," + loan-acct.cont-code,
                                "Priznak",
                                "0"
                            ).
                IF NOT CAN-DO("!Роскап*ОВ*,!Союз*ОВ*,!Норвик*ОВ*,!Алекс*ОВ*,СОЮЗ*,РОСКАП*,Норвик*,Коллектор*", mPriznak)
                    THEN
                DO:
/*                    mNN = IF loan-acct.acct-type EQ "КредОб" THEN 0 ELSE INT64(SUBSTRING(loan-acct.acct-type, 7)).
                    FOR EACH term-obl
                        WHERE term-obl.contract EQ loan-acct.contract
                        AND term-obl.cont-code EQ loan-acct.cont-code
                        AND term-obl.idnt EQ 5
                        AND term-obl.nn = mNN
                        NO-LOCK:
                        LEAVE.
                    END.
*/                    
                   mTermSurr = "0".   
                   RUN GetGarByAcct IN h_lngar (loan-acct.contract,
                                                loan-acct.cont-code,
                                                loan-acct.acct,
                                                IF SUBSTRING(loan-acct.acct, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(loan-acct.acct, 6, 3),
                                                iDate,
                                                OUTPUT mTermSurr).            
                    IF mTermSurr NE "0" THEN
                    DO:
                        FOR EACH term-obl
                            WHERE term-obl.contract EQ ENTRY(1, mTermSurr)
                            AND term-obl.cont-code EQ ENTRY(2, mTermSurr)
                            AND term-obl.idnt EQ INT64(ENTRY(3, mTermSurr))
                            AND term-obl.end-date = DATE(ENTRY(4, mTermSurr))
                            AND term-obl.nn = INT64(ENTRY(5, mTermSurr))
                            NO-LOCK:
                            LEAVE.
                        END.
/*                        mTermSurr = term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn). */ 
                        mVidObNotar = GetXAttrValueEx("term-obl", 
                                                mTermSurr, 
                                                "ВидОб",
                                                "0").
                        IF mVidObNotar EQ "Автомобиль"
                        THEN
                        DO:
                            FOR EACH loan
                                WHERE loan.contract EQ loan-acct.contract 
                                AND loan.cont-code EQ loan-acct.cont-code
                                NO-LOCK:
                                LEAVE.
                            END.
                            CREATE ttNotif.
                            ASSIGN
                            ttNotif.OpEntryDbOp = op-entry.op
                            ttNotif.contract = loan-acct.contract
                            ttNotif.cont-code = loan-acct.cont-code
                            ttNotif.RecLoan = RECID(loan)
                            ttNotif.RecTermObl = RECID(term-obl) 
                            .
                        END.
                    END.
                END.
            END.
        END.
    END.
END.
/* ---- */
FOR EACH ttNotif BY ttNotif.cont-code:
    mParStr = ttNotif.contract + "," + ttNotif.cont-code + "," + TRIM(STRING(ttNotif.RecLoan)) + "," + TRIM(STRING(ttNotif.RecTermObl)) + ",2,1".
    RUN notar-cre.p(INPUT mParStr, INPUT-OUTPUT mNotifRet).
    IF mNotifRet NE 0 THEN
    DO:
/*        FIND FIRST term-obl 
            WHERE RECID(term-obl) EQ ttNotif.RecTermObl
            NO-LOCK NO-ERROR.
        IF AVAILABLE(term-obl) THEN    
        DO: */
            UpdateSigns("term-obl-gar", 
            mTermSurr,
            "NotifId",
            STRING(mNotifRet), 
            ?).
/*        END. */    
        UpdateSigns("opo",
                STRING(ttNotif.OpEntryDbOp), 
                "NotifId",
                STRING(mNotifRet), 
                ?).
    END.
END.    

{intrface.del}

