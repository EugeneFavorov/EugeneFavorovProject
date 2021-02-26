
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
   FIELD acct-type AS CHARACTER
  FIELD cont-code AS CHARACTER
  INDEX cont-code cont-code.
  
  DEFINE TEMP-TABLE tt-1lacct2
  FIELD cont-code AS CHARACTER
  FIELD acct-cr AS CHARACTER
  FIELD acct-db AS CHARACTER
  FIELD op-date   AS date
  INDEX cont-code cont-code.





for each tmprecid,
    first loan where
        recid(loan) eq tmprecid.id
and loan.contract EQ 'dps'
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
     .
     end.
     end.
    no-lock NO-ERROR.


    FOR EACH tt-1lacct NO-LOCK:
    FIND FIRST op-entry WHERE
    op-entry.acct-db EQ tt-1lacct.acct
    and op-entry.acct-cr BEGINS '42306'
    no-lock NO-ERROR.
    IF AVAIL op-entry THEN DO:
    CREATE tt-1lacct2.
    ASSIGN
    tt-1lacct2.cont-code = STRING(entry(1,tt-1lacct.cont-code,"@"), "x(23)")
    tt-1lacct2.acct-cr      = entry(1,op-entry.acct-cr,"@")
    tt-1lacct2.acct-db      = entry(1,op-entry.acct-db,"@")
    tt-1lacct2.op-date      = op-entry.op-date.
/*    PUT UNFORMATTED 
    tt-1lacct.cont-code format "x(35)"
    op-entry.acct-cr format "x(35)"
    op-entry.acct-db format "x(35)"
    op-entry.op-date 
    skip.*/
    END.
    END.
     /*FIND FIRST op
       WHERE op.op EQ op-entry.op
         and (op.op-kind EQ '028cl1_4'
         or op.op-kind begins '012sz'
         or op.op-kind begins '012dv')
        and op.op-status EQ "√"
            NO-LOCK NO-ERROR.
            IF AVAIL op THEN
            DO: /* if op */
            PUT UNFORMATTED
            entry(1,tt-1lacct.cont-code,"@") format "x(23)"
            entry(1,tt-1lacct.acct,"@") format "x(22)"
            tt-1lacct.acct-type format "x(15)"
            skip.
            END.
            END.
            /*PUT UNFORMATTED "123".*/
            END.*/
/*PUT UNFORMATTED "1234" skip.*/
{setdest.i &file-name = "111.log"}
FOR EACH tt-1lacct2 NO-LOCK BY tt-1lacct2.op-date:
PUT UNFORMATTED tt-1lacct2.cont-code tt-1lacct2.acct-cr tt-1lacct2.acct-db tt-1lacct2.op-date SKIP.
END.
{preview.i &file-name = "111.log"}

{intrface.del}