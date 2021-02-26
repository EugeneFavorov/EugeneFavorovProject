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
/*
DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
*/
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.

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

    
/* 
Отработка сообщений
о возникновении залога
об изменении залога
*/
{setdest.i}
PUT UNFORMATTED "Обработанные договоры" SKIP.
FOR EACH tmprecid,
    FIRST term-obl
    WHERE RECID(term-obl) EQ tmprecid.id
    NO-LOCK:
    FIND FIRST loan
        WHERE loan.contract EQ term-obl.contract
        AND loan.cont-code EQ term-obl.cont-code
        NO-LOCK NO-ERROR.
    IF AVAILABLE(loan) THEN
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
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "нет регистрационного номера уведомления. Пропускаем."
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
                "Снятие."
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
                "не создано уведомление"
                SKIP.
        END.
    END.
END.

{intrface.del}
{preview.i}
