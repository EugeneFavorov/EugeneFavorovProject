{globals.i}
{intrface.get pbase}
{sh-defs.i}

{tmprecid.def}

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL    NO-UNDO.

{setdest.i &filename = "'comp-cyber.log'"}

mCnt = 0.
FOR EACH bank.cyberplat WHERE TRUE 
   AND bank.cyberplat.idate GE TODAY
   AND bank.cyberplat.PROG  NE 'CASH_IN'                   /*Банкоматы*/
   AND bank.cyberplat.PROG  NE 'CARDDEBTPAY'               /*Комиссии зарплатников*/
   NO-LOCK QUERY-TUNING (NO-INDEX-HINT):
    
   mOk = TRUE.
   FOR EACH signs WHERE signs.file-name = 'op'
      AND signs.code = 'did-smartfl' 
      AND signs.dec-value = bank.cyberplat.did 
      NO-LOCK,
      EACH op WHERE 
      op.op = INT64(signs.surrogate) 
      NO-LOCK QUERY-TUNING (NO-INDEX-HINT):
      LEAVE.
   END.
   IF NOT AVAIL(op) THEN 
   DO:
      mCnt = mCnt + 1.
      PUT UNFORMATTED
         mCnt                   ";" 
         bank.cyberplat.idate   ";"
         bank.cyberplat.did     ";"
         bank.cyberplat.debit   ";"
         bank.cyberplat.credit  ";"
         bank.cyberplat.sum     ";"
         bank.cyberplat.details ";"
      SKIP.
   END.          
END.

IF mCnt EQ 0 THEN
DO:
   PUT UNFORMATTED
      "Все прогружено."
   SKIP.
END.

{preview.i &filename = "'comp-cyber.log'"}

{intrface.del}

RETURN.
