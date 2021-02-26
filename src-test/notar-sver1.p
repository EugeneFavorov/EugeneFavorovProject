{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.

DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
/* 'Сообщение в нотариат о появлении, изменении и прекращении залога' */
DEFINE BUFFER NF FOR NOTIFICATIONS.
DEFINE VARIABLE mNotifId AS INT64 NO-UNDO.
DEFINE VARIABLE mNotifCreNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifCreNumberNotary AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tLoan 
    FIELD cont-code AS CHARACTER
    FIELD NotifId AS INT64
    FIELD NotifRefNumber AS CHARACTER
    FIELD DateOb AS DATE
    .

{tmprecid.def}

    FOR EACH tmprecid,
	FIRST loan
	WHERE RECID(loan) EQ tmprecid.id
	NO-LOCK:
        FOR EACH term-obl
            WHERE term-obl.contract EQ loan.contract
            AND term-obl.cont-code EQ loan.cont-code
            AND term-obl.idnt EQ 5
            AND term-obl.nn EQ 0
        NO-LOCK BY term-obl.since:
	LEAVE.
	END.
        IF AVAILABLE(term-obl) THEN
        DO:
            mNotifCreNumberNotary = "Нет".
            mNotifCreNumber = GetXAttrValueEx("term-obl", 
                                term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                "reg-zalog",
                                "Нет").
            mNotifId = INT64( GetXAttrValueEx("term-obl", 
                                term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                                "reg-zalog-no",
                                "0"))
            .
            IF mNotifCreNumber NE "Нет" THEN
            DO:
                FIND FIRST NF
                    WHERE NF.NOTIFICATIONID EQ mNotifId
                    AND NF.STATUS_ EQ 5
                    NO-LOCK NO-ERROR.
                IF AVAILABLE(NF) THEN
                DO:
                    mNotifCreNumberNotary = NF.NOTIFICATIONREFERENCENUMBER.
                END.
            END.
        END.
        CREATE tLoan.

        ASSIGN
            tLoan.cont-code = loan.cont-code
            tLoan.NotifId = mNotifId
            tLoan.NotifRefNumber = mNotifCreNumberNotary
            tLoan.DateOb = term-obl.fop-date
            .
    END.

{setdest.i}
FOR EACH tLoan:
    PUT UNFORMATTED
    tLoan.cont-code
    ";"
    tLoan.NotifRefNumber
    ";"
    tLoan.DateOb
    SKIP.
END.
{preview.i}



{intrface.del}

