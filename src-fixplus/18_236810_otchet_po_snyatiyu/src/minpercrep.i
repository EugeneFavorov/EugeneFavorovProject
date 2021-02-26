/* ‘Ÿ’ˆ… */
FOR EACH op-entry WHERE op-entry.op-date GE beg-date
                    AND op-entry.op-date LE end-date
                    AND op-entry.acct-cr BEGINS "20202"
                    AND op-entry.acct-db EQ acct.acct
NO-LOCK:
    ASSIGN
       tt-rep.numct = tt-rep.numct + 1
       tt-rep.sumct = tt-rep.sumct + op-entry.amt-rub
    .
END. 
/* ‘’“‹…ˆ… */
FOR EACH op-entry WHERE op-entry.op-date GE beg-date
                    AND op-entry.op-date LE end-date
                    AND op-entry.acct-cr EQ acct.acct
NO-LOCK:
    ASSIGN
       tt-rep.numdb = tt-rep.numdb + 1
       tt-rep.sumdb = tt-rep.sumdb + op-entry.amt-rub
    .
END.
