{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mDatereg AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE BUFFER acct FOR acct.

{tmprecid.def}

{setdest.i}
PUT UNFORMATTED
"BEGINS"
SKIP.
FOR EACH tmprecid,
    FIRST acct
    WHERE RECID(acct) EQ tmprecid.id
    NO-LOCK:
/*    mDatereg[1] = GetXAttrValueEx("acct",
        acct.acct + "," + acct.currency,
        "„ â ‘®®¡é‹‘",
        ""
        ). */
    IF acct.filial-id EQ "0000" AND 
    acct.acct EQ '40701810000000105076     @0000'
    /*acct.open-date LE DATE("30.04.2015") */
    THEN
    
    DO:
        FOR EACH PackObject
            WHERE PackObject.file-name EQ "acct"
            AND PackObject.surrogate EQ ENTRY(1, acct.acct, "@") + "@0400," + acct.currency
            NO-LOCK, 
            FIRST seance
            WHERE seance.op_kind  = 'e-opx502'
            AND seance.direct = 'íªá¯®àâ'
            AND seance.seanceid = packobject.seanceid

            BY PackObject.PacketId:
            LEAVE.
        END.
        IF NOT AVAILABLE(PackObject) THEN
            FOR EACH PackObject
                WHERE PackObject.file-name EQ "acct"
                AND PackObject.surrogate EQ acct.acct + "," + acct.currency
                NO-LOCK BY PackObject.PacketId:
                LEAVE.
            END.
    END.
    ELSE
        FOR EACH PackObject
            WHERE PackObject.file-name EQ "acct"
            AND PackObject.surrogate EQ acct.acct + "," + acct.currency
            NO-LOCK BY PackObject.PacketId:
            LEAVE.
        END.
    IF AVAILABLE(PackObject) THEN
    DO:
        FIND FIRST Packet
            WHERE Packet.PacketId EQ PackObject.PacketId
            NO-LOCK NO-ERROR.
        IF AVAILABLE(Packet) THEN
        DO: 
            IF
            UpdateSigns(acct.class-code,
                        acct.acct + "," + acct.currency,
            			"„ â ‘®®¡é‹‘",
            			STRING(Packet.PackDate, "99/99/9999"),
                		?) THEN
                PUT UNFORMATTED
                acct.acct
                SKIP.
        END. 
    END.
END.    


{preview.i}
{intrface.del}
