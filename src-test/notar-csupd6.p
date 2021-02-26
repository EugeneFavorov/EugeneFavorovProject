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
    IF loan.close-date NE ? THEN /* Договор закрыт */
        PUT UNFORMATTED 
        ENTRY(1, loan.cont-code, "@") 
        " ; "
        " договор закрыт, не создаем уведомление."
        SKIP.
    ELSE
    DO:
        FIND FIRST NF 
            WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
            AND NF.NOTIFICATIONTYPE EQ 3
            AND NF.STATUS_ GE 6 
            AND NF.STATUS_ LE 7 
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(NF) THEN
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "Нет уведомления о замене"
            SKIP.
    END.    
END.

{intrface.del}
{preview.i}

