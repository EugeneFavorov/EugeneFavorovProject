
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

for each tmprecid,
    first loan where
        recid(loan) eq tmprecid.id
and loan.contract EQ 'dps'
AND loan.loan-status EQ '√'
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
    FIND LAST loan-acct of loan where
    loan-acct.acct-type = 'loan-dps-p'
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
     FIND LAST loan-acct of loan where
     loan-acct.acct-type = 'loan-dps-tr'
     AND loan-acct.since     LE TODAY
     and loan-acct.acct BEGINS '4'
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
     .
     end.
     end.
     /*or loan-acct.acct-type = 'loan-dps-t'
     or loan-acct.acct-type = 'loan-dps-int'
     or loan-acct.acct-type = 'loan-dps-tr'
     AND loan-acct.since     LE TODAY*/
    no-lock NO-ERROR.

FOR EACH tt-1lacct NO-LOCK:
FIND FIRST ACCT WHERE
acct.acct EQ tt-1lacct.acct AND
acct.close-date EQ ?
NO-LOCK NO-ERROR.
IF AVAIL acct THEN
DO:
 PUT UNFORMATTED
     entry(1,tt-1lacct.cont-code,"@") format "x(23)"
         entry(1,tt-1lacct.acct,"@") format "x(22)"
             tt-1lacct.acct-type format "x(15)"
                 acct.close-date
                     skip.
                      END.
/* PUT UNFORMATTED "123".*/
END.
/*PUT UNFORMATTED "1234" skip.*/
{preview.i}
{intrface.del}