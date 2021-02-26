{globals.i}
{setdest.i}

{tmprecid.def}
{intrface.get xclass}

{getdates.i}

DEF VAR contcode AS CHAR NO-UNDO.
def var uved as char no-undo.

   FOR EACH op-entry WHERE
        op-entry.op-status BEGINS "√" AND
        op-entry.acct-db BEGINS '40817' AND
        op-entry.acct-cr BEGINS '47422' AND
        op-entry.op-date >= beg-date AND 
        op-entry.op-date <= end-date 
        NO-LOCK:
            
            contcode = GetXAttrValueEx("op",STRING(op-entry.op),"cont-code","").
            IF contcode <> "" THEN DO:
                FIND FIRST loan WHERE loan.contract = 'Кредит' AND
                    loan.cont-code = contcode
                    NO-LOCK NO-ERROR.
            	if avail loan then do:
	            uved = GetXAttrValueEx("loan",loan.contract + ',' + loan.cont-code,"Uvedom","").
                IF uved = 'Да' THEN DO: 
                    PUT UNFORMATTED loan.doc-ref + ' ' + STRING(op-entry.op-date) + '  ' + op-entry.acct-db SKIP.
                END.
                end.
            END.
    END.

{intrface.del}    
{preview.i}


