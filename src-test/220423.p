
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
  FIELD open-date AS DATE
  INDEX cont-code cont-code.

for each tmprecid,
    first loan where
        recid(loan) eq tmprecid.id
and loan.contract EQ 'dps'
/*AND loan.loan-status EQ '√'*/
    no-lock:
     FIND LAST loan-acct of loan where
     loan-acct.acct-type = 'loan-dps-int'
     AND loan-acct.since     LE TODAY
     no-lock NO-ERROR.
     IF AVAIL loan-acct THEN
     DO:
     CREATE tt-1lacct.
     ASSIGN
     tt-1lacct.acct = loan-acct.acct
     tt-1lacct.acct-type = loan-acct.acct-type
     tt-1lacct.cont-code = loan.cont-code
     tt-1lacct.open-date = loan.open-date
     .
     end.
     end.
    no-lock NO-ERROR.



    FOR EACH tt-1lacct NO-LOCK:
    FOR EACH op-entry WHERE
    op-entry.acct-db EQ tt-1lacct.acct
    or op-entry.acct-cr EQ tt-1lacct.acct
    and op-entry.op-date <> tt-1lacct.open-date
    no-lock:
    IF AVAIL op-entry THEN DO:
     FIND FIRST op
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
            op-entry.op-date "  "
            tt-1lacct.open-date
            skip.
            END.
            END.
            end.
            /*PUT UNFORMATTED "123".*/
            END.
/*PUT UNFORMATTED "1234" skip.*/
{preview.i}
{intrface.del}