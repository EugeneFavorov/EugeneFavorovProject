/*
 Процедура сканирует таблицу 
NOTARY_ONLINE.PLEDGENOTIFICATION
и выставляет уведомлениям статус 2 
*/

{globals.i}


DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NF1 FOR PLEDGENOTIFICATION.
DEFINE BUFFER signs FOR signs.
DEFINE BUFFER op FOR op.

DEFINE VARIABLE mOpKindList AS CHARACTER NO-UNDO.

mOpKindList = "ВыдАвто+,ПДГ2,m92003otsN,K_ed_VID_AP,K_ed_PDG,K_ed_OTSTUP".



FOR EACH NF
    WHERE NF.STATUS_ EQ 1
    NO-LOCK:
    FIND FIRST signs
        WHERE signs.file-name EQ 'op'
        AND signs.code EQ 'NotifId'
        AND signs.dec-value EQ NF.PLEDGEID
        NO-LOCK NO-ERROR.
    IF AVAILABLE(signs) THEN
    DO:
        FIND FIRST op
            WHERE op.op EQ INT64(signs.surrogate)
            NO-LOCK NO-ERROR.
        IF CAN-DO(mOpKindList, op.op-kind) THEN
        DO:
            FIND FIRST NF1
                WHERE NF1.PLEDGEID EQ NF.PLEDGEID
                EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE(NF1) THEN
                DO:
                    NF1.STATUS_ = 2.
                    VALIDATE NF1.
                    FIND CURRENT NF1 NO-LOCK NO-ERROR. 
                END.
        END.
    END.
END.
