/*
 Процедура сканирует таблицу 
NOTARY_ONLINE.PLEDGENOTIFICATION
и проставляет ДР 
NotifRefNumber       Регистрационный номер уведомления о возн
NotifRegDate         Дата регистрации уведомления
на term-obl-gar
*/
{globals.i}
{intrface.get xclass}
{intrface.get lngar}

DEFINE VARIABLE mNotifID AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRefNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.

DEFINE BUFFER signs FOR signs.
/* DEFINE BUFFER signs1 FOR signs. */
DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NF1 FOR PLEDGENOTIFICATION.
{setdest.i}
FOR EACH op-entry
    WHERE 
    op-entry.op-date GE TODAY - 25 
/*    op-entry.op-date GE DATE("01.09.2016")
    AND op-entry.op-date LE DATE("10.10.2016") */
    AND op-entry.acct-cr BEGINS "91312"
    AND op-entry.op-status GE CHR(251)
    NO-LOCK:
    mNotifID = GetXAttrValueEx("op",
        STRING(op-entry.op),
        "NotifID",
        "NO"
        ).
    IF mNotifID NE "NO" THEN
    DO:
        mTermSurr = "0".   
        RUN GetGarByAcct IN h_lngar (ENTRY(1, op-entry.kau-cr),
                                    ENTRY(2, op-entry.kau-cr),
                                    op-entry.acct-cr,
                                    IF SUBSTRING(op-entry.acct-cr, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(op-entry.acct-cr, 6, 3),
                                    op-entry.op-date,
                                    OUTPUT mTermSurr).
        
        FIND FIRST NF
            WHERE NF.PLEDGEID EQ INT64(mNotifID)
            AND NF.STATUS_ EQ 7
            NO-LOCK NO-ERROR.
        IF AVAILABLE(NF) 
/* AND (NF.NOTIFICATIONREFERENCENUMBER NE ? OR CREATIONREFERENCENUMBER NE ?) */
	THEN
        DO:
/*        PUT UNFORMATTED op-entry.op-date " - " mTermSurr SKIP. */
            UpdateSigns("term-obl-gar", 
                        mTermSurr, 
                        "NotifRefNumber",
                        IF NF.NOTIFICATIONTYPE EQ 1 THEN NF.NOTIFICATIONREFERENCENUMBER ELSE NF.CREATIONREFERENCENUMBER, 
                        ?).
            IF NF.REGISTRATIONTIME NE ? THEN
            DO:
                UpdateSigns("term-obl-gar", 
                            mTermSurr, 
                            "NotifRegDate",
                            STRING(NF.REGISTRATIONTIME), 
                            ?).
            END.
        END.
    END. 
END.
{preview.i}

{intrface.del}

