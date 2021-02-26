
{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get dps}
{intrface.get dpspr}

{setdest.i}
{sh-defs.i}
{dpsproc.def}
DEF VAR mSumm AS DECIMAL NO-UNDO.
DEFINE TEMP-TABLE tt-1lacct
   FIELD acct AS CHARACTER
   FIELD cont-type AS CHARACTER
   FIELD open-date AS date
   FIELD acct-type AS CHARACTER
  FIELD cont-code AS CHARACTER
  INDEX cont-code cont-code.

DEFINE TEMP-TABLE tt-1lacct2
   FIELD acct      AS CHARACTER format "x(22)"
   FIELD cont-type AS CHARACTER format "x(20)"
   FIELD open-date AS date
   FIELD acct-type AS CHARACTER format "x(15)"
   FIELD cont-code AS CHARACTER format "x(23)"
   FIELD op-date   AS date
INDEX cont-code cont-code.

for each tmprecid,
    first loan where
        recid(loan) eq tmprecid.id
and loan.contract EQ 'dps'
/*AND loan.loan-status EQ '√'*/
    no-lock:
     FIND LAST loan-acct of loan where
     loan-acct.acct-type = 'loan-dps-t'
     AND loan-acct.since     LE TODAY
     no-lock NO-ERROR.
     IF AVAIL loan-acct THEN
     DO:
     CREATE tt-1lacct.
     ASSIGN
     tt-1lacct.acct = loan-acct.acct
     tt-1lacct.acct-type = loan-acct.acct-type
     tt-1lacct.cont-code = loan.cont-code
     tt-1lacct.cont-type = loan.cont-type
     tt-1lacct.open-date = loan.open-date
     
     .
     end.
     end.
    no-lock NO-ERROR.

/*FOR EACH tt-1lacct NO-LOCK:


PUT UNFORMATTED
    entry(1,tt-1lacct.acct,"@") format "x(22)"
	skip.
    
    END.*/    
    FOR EACH tt-1lacct NO-LOCK:
    /*PUT UNFORMATTED
                           entry(1,tt-1lacct.cont-code,"@") format "x(23)"
                           entry(1,tt-1lacct.acct,"@") format "x(22)"
                           skip.*/
    FIND FIRST op-entry WHERE
    op-entry.acct-cr EQ tt-1lacct.acct
    and (op-entry.acct-db BEGINS '42306'
    or op-entry.acct-db BEGINS '42302'
    or op-entry.acct-db BEGINS '42303'
    or op-entry.acct-db BEGINS '42304'
    or op-entry.acct-db BEGINS '42305'
    or op-entry.acct-db BEGINS '42307')
    no-lock NO-ERROR.
    IF AVAIL op-entry THEN DO:
    /*PUT UNFORMATTED 
    tt-1lacct.cont-code format "x(35)"
    op-entry.acct-cr format "x(35)"
    op-entry.acct-db format "x(35)"
    op-entry.op-date 
    skip.
    END.
    END.*/
     FIND FIRST op
       WHERE op.op EQ op-entry.op
         and (op.op-kind EQ '028cl1_4'
         or op.op-kind begins '012sz'
         or op.op-kind begins '012dv'
         or op.op-kind begins '028rs')
        and op.op-status EQ "√"
            NO-LOCK NO-ERROR.
            IF AVAIL op THEN
            DO: /* if op */
                CREATE tt-1lacct2.
                ASSIGN
                    tt-1lacct2.acct      = entry(1,tt-1lacct.acct,"@")  /* format "x(22)" */
                    tt-1lacct2.cont-type = STRING(tt-1lacct.cont-type, "x(20)") /* format "x(20)" */
                    tt-1lacct2.open-date = tt-1lacct.open-date
                    tt-1lacct2.acct-type = STRING(tt-1lacct.acct-type, "x(15)") /* format "x(15)" */
                    tt-1lacct2.cont-code = STRING(entry(1,tt-1lacct.cont-code,"@"), "x(23)") /* format "x(23)" */
                    tt-1lacct2.op-date   = op-entry.op-date
                .

            /* PUT UNFORMATTED
            entry(1,tt-1lacct.cont-code,"@") format "x(23)"
            entry(1,tt-1lacct.acct,"@") format "x(22)"
            tt-1lacct.acct-type format "x(15)"
            tt-1lacct.cont-type format "x(20)"
            tt-1lacct.open-date
            '  'op-entry.op-date
            skip. */
            END.
            end.
            end.
            /*PUT UNFORMATTED "123".*/
            /*PUT UNFORMATTED "1234" skip.*/

{setdest.i &file-name = "111.log"}
FOR EACH tt-1lacct2 NO-LOCK BY tt-1lacct2.op-date:
  PUT UNFORMATTED tt-1lacct2.acct tt-1lacct2.cont-type tt-1lacct2.open-date "  " tt-1lacct2.acct-type tt-1lacct2.cont-code tt-1lacct2.op-date SKIP.
END.
{preview.i &file-name = "111.log"}

/* {preview.i} */
{intrface.del}