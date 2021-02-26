{globals.i}
{setdest.i}

{tmprecid.def}





FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    
    FIND LAST loan-acct OF loan
        WHERE loan-acct.acct-type = "Šà¥¤¨â" NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        FOR EACH op-entry WHERE
        (op-entry.op-status = "û" OR op-entry.op-status = "ûû") AND
        op-entry.acct-cr = loan-acct.acct AND
        substring(op-entry.acct-db,1,5) = '61214'
         NO-LOCK BY op-entry.op-date DESC:
            FIND first loan-int where loan-int.op = op-entry.op no-error.
            if avail loan-int then do:
                delete loan-int.
                /* loan-int.cont-code = loan-int.cont-code + 'del'. */
                put unformatted loan.cont-code + ' ' + loan-acct.acct skip.
            end.             
            LEAVE.
        END.
    END.
    FIND LAST loan-acct OF loan
        WHERE loan-acct.acct-type = "Šà¥¤’" NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        FOR EACH op-entry WHERE
        (op-entry.op-status = "û" OR op-entry.op-status = "ûû") AND
        op-entry.acct-cr = loan-acct.acct AND
        substring(op-entry.acct-db,1,5) = '61214'
         NO-LOCK BY op-entry.op-date DESC:
            FIND first loan-int where loan-int.op = op-entry.op no-error.
            if avail loan-int then do:
                delete loan-int.
                /* loan-int.cont-code = loan-int.cont-code + 'del'. */
                put unformatted loan.cont-code + ' ' + loan-acct.acct skip.
            end.             
            LEAVE.
        END.
    END.
    
    
END.
    
{preview.i}


