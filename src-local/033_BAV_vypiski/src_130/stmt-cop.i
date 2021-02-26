Op-Block:
FOR EACH op-entry WHERE
         op-entry.op-date   EQ {&dob}
     AND op-entry.op-status GE "Ф"
     AND (   ( ts.zo eq 1 AND {ifndef {&r}} NOT {endif} */ op-entry.prev-year)
          {ifdef {&r}}
          OR ( ts.zo eq 2 AND     op-entry.prev-year )
          {endif} */
          OR ( ts.zo eq 3 AND NOT op-entry.prev-year )
         )
     AND op-entry.acct-{&this} EQ acct.acct
     AND ( ts.rubentries OR ( op-entry.amt-cur NE 0 ) )
     AND op-entry.currency {&eq} acct.currency
         NO-LOCK,
   FIRST op OF op-entry
         NO-LOCK:

   {stmt-tst.i &label=Op-Block {&*}}

   FIND FIRST op-bank OF op NO-LOCK NO-ERROR.

IF ts.kurs THEN
   FOR EACH half-entry OF op WHERE
            (op-entry.acct-{&corr} NE ? AND ROWID(half-entry) = ROWID(op-entry))
         OR (op-entry.acct-{&corr} EQ ? AND half-entry.acct-{&this} EQ ?)
      NO-LOCK:

      /* Учитываем сумму с той стороны, где полупроводки две */
      vHalf = CAN-FIND (FIRST xOp-entry OF op WHERE
                              xOp-entry.op-entry     NE op-entry.op-entry
                          AND xOp-entry.acct-{&corr} EQ ?).
      CREATE stmt.
      ASSIGN stmt.acct          = acct.acct
             stmt.acct-{&corr}  = half-entry.acct-{&corr}

             stmt.amt-rub       = IF acct.acct-cat NE "d" THEN (IF vHalf THEN op-entry.amt-rub
                                                                         ELSE half-entry.amt-rub)
                                                          ELSE (IF vHalf THEN op-entry.qty
                                                                         ELSE half-entry.qty)
             /* Валютная сумма - только если это не проводка конвертации */
             stmt.amt-cur       = IF NOT CAN-DO (vKurs,half-entry.acct-{&corr})
                                     THEN op-entry.amt-cur
                                     ELSE half-entry.amt-cur

             stmt.corr-acct     = op-bank.corr-acct WHEN AVAIL op-bank
             stmt.bank-code     = op-bank.bank-code WHEN AVAIL op-bank
             /* Для ЗО дату проводки присваиваем = дате выписки */
             stmt.op-date       = op-entry.op-date
             stmt.op-value-date = op.op-value-date
             stmt.doc-num       = op.doc-num
             stmt.op            = op.op
             stmt.prev-year     = half-entry.prev-year
	          stmt.rwd           = ROWID ( half-entry )
             stmt.doc-type      = op.doc-type.
      RUN GetOpDetails(BUFFER op, OUTPUT stmt.details) NO-ERROR.
      RELEASE stmt.
   END.
ELSE
   FOR FIRST half-entry OF op WHERE
            ( op-entry.acct-{&corr} NE ? AND ROWID(half-entry) = ROWID(op-entry) )
         OR ( op-entry.acct-{&corr} EQ ? AND half-entry.acct-{&this} EQ ?)
      NO-LOCK:

      CREATE stmt.
      ASSIGN stmt.acct          = acct.acct
             stmt.acct-{&corr}  = half-entry.acct-{&corr}
             stmt.amt-rub       = IF acct.acct-cat NE "d" THEN op-entry.amt-rub
                                                          ELSE op-entry.qty
             stmt.amt-cur       = op-entry.amt-cur
             stmt.corr-acct     = op-bank.corr-acct WHEN AVAIL op-bank
             stmt.bank-code     = op-bank.bank-code WHEN AVAIL op-bank
             /* Для ЗО дату проводки присваиваем = дате выписки */
             stmt.op-date       = op-entry.op-date
             stmt.op-value-date = op.op-value-date
             stmt.doc-num       = op.doc-num
             stmt.op            = op.op
             stmt.prev-year     = half-entry.prev-year
	          stmt.rwd           = ROWID ( half-entry )
             stmt.doc-type      = op.doc-type.
      RUN GetOpDetails(BUFFER op, OUTPUT stmt.details) NO-ERROR.
   END.
END. /* for each op-entry */

