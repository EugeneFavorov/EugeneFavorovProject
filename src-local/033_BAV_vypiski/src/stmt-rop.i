&IF "{&THIS}" EQ "CR" &THEN
   &SCOPED-DEFINE SEND SEND
&ELSE
   &SCOPED-DEFINE SEND REC
&ENDIF

Op-Block:
FOR EACH op-entry WHERE 
         op-entry.op-date   EQ {&dob}
/* Замена Плюс банк
     AND op-entry.op-status GE gop-status
*/   AND op-entry.op-status GE "Ф"
/* Конец замены Плюс банк */
     AND (   ( ts.zo eq 1 AND {ifndef {&r}} NOT {endif} */ op-entry.prev-year)
          {ifdef {&r}}
          OR ( ts.zo eq 2 AND     op-entry.prev-year ) 
          {endif} */
          OR ( ts.zo eq 3 AND NOT op-entry.prev-year )
         )
     AND op-entry.currency {&eq} ""
     AND op-entry.acct-{&this} EQ acct.acct
         NO-LOCK,
   FIRST op OF op-entry 
         NO-LOCK
   BREAK BY op-entry.op:

/* Вставка Плюс банк */
   {stmt-tst.i &label=Op-Block {&*}}
/* Конец вставки Плюс банк */
   RELEASE op-bank.
&IF DEFINED (Plug) NE 0 &THEN
   IF NOT CAN-DO (stNaznShMBR, acct.contract) THEN
      FIND FIRST op-bank OF op WHERE op-bank.op-bank-type EQ "{&send}" NO-LOCK NO-ERROR.
   IF NOT AVAIL op-bank THEN
&ENDIF
      FIND FIRST op-bank OF op NO-LOCK NO-ERROR.

IF ts.kurs THEN
   /* Либо полупроводка, либо та-же самая проводка */
   FOR EACH half-entry OF op WHERE 
            ( op-entry.acct-{&corr} NE ? AND ROWID(half-entry) = ROWID(op-entry) )
         OR ( op-entry.acct-{&corr} EQ ? AND half-entry.acct-{&this} EQ ? )
      NO-LOCK:         
      
      /* Первая проводка по корресп. счету в документе */
      IF FIRST-OF (op-entry.op) THEN Symb = Op-entry.Symbol.   
      /* Символ не определен */
      IF Symb NE Op-entry.Symbol THEN Symb = "".
      vHalf = CAN-FIND (FIRST xOp-entry OF op WHERE 
                              xOp-entry.op-entry     NE op-entry.op-entry
                          AND xOp-entry.acct-{&corr} EQ ?).
      
      CREATE stmt.
      ASSIGN stmt.acct          = acct.acct
             stmt.acct-{&corr}  = half-entry.acct-{&corr}
             stmt.acct-{&this}  = ?
             stmt.amt-rub       = IF vHalf 
                                     THEN op-entry.amt-rub 
                                     ELSE half-entry.Amt-Rub
             stmt.corr-acct     = op-bank.corr-acct WHEN AVAIL op-bank
             stmt.bank-code     = op-bank.bank-code WHEN AVAIL op-bank
             stmt.op-date       = op-entry.op-date
             stmt.op-value-date = op.op-value-date
             stmt.doc-num       = op.doc-num
             stmt.doc-type      = op.doc-type
             stmt.prev-year     = half-entry.prev-year
             stmt.op            = op.op
             stmt.ben-acct      = op.ben-acct
             stmt.rwd           = ROWID ( half-entry )
             stmt.symbol        = IF Ts.Kc THEN half-entry.symbol ELSE symb
             stmt.inn           = op.inn
             stmt.name-ben      = op.name-ben
      .
      RUN GetOpDetails(BUFFER op, OUTPUT stmt.details) NO-ERROR.
   END.
ELSE 
   FOR FIRST half-entry OF op WHERE 
            ( op-entry.acct-{&corr} NE ? AND ROWID(half-entry) = ROWID(op-entry) )
         OR ( op-entry.acct-{&corr} EQ ? AND half-entry.acct-{&this} EQ ? )
      NO-LOCK:         
      
      /* Первая проводка по корресп. счету в документе */
      IF FIRST-OF (op-entry.op) THEN Symb = Op-entry.Symbol.   
      /* Символ не определен */
      IF Symb NE Op-entry.Symbol THEN Symb = "".
      /* Определяем источник суммы */
      vAmtRub = op-entry.amt-rub.
      /* Определяем разбивку */
      IF NOT Ts.Kc AND
         {assigned op-entry.symbol} AND        
         CAN-FIND(FIRST stmt WHERE stmt.op EQ op.op AND
                                   stmt.acct-{&corr} EQ op-entry.acct-{&corr} AND
                                   stmt.acct EQ acct.acct AND                                  
                                   stmt.doc-num EQ op.doc-num AND
                                   stmt.op-date EQ op.op-date) 
      THEN DO:
         FIND FIRST stmt WHERE     stmt.op EQ op.op AND
                                   stmt.acct-{&corr} EQ op-entry.acct-{&corr} AND
                                   stmt.acct EQ acct.acct AND
                                   stmt.doc-num EQ op.doc-num AND 
                                   stmt.op-date EQ op.op-date
         NO-LOCK NO-ERROR.
         stmt.amt-rub = stmt.amt-rub + vAmtRub.  
         stmt.symbol  = symb.
         NEXT op-block.   
      END.
      
      CREATE stmt.
      ASSIGN stmt.acct          = acct.acct
             stmt.acct-{&corr}  = half-entry.acct-{&corr}
             stmt.acct-{&this}  = ?
             stmt.amt-rub       = vAmtRub
             stmt.corr-acct     = op-bank.corr-acct WHEN AVAIL op-bank
             stmt.bank-code     = op-bank.bank-code WHEN AVAIL op-bank
             stmt.op-date       = op-entry.op-date
             stmt.op-value-date = op.op-value-date
             stmt.doc-num       = op.doc-num
             stmt.doc-type      = op.doc-type
             stmt.prev-year     = op-entry.prev-year
             stmt.op            = op.op
             stmt.ben-acct      = op.ben-acct
             stmt.rwd           = ROWID ( half-entry )
             stmt.symbol        = IF Ts.Kc THEN (if {assigned op-entry.symbol}
                                                 then op-entry.symbol
                                                 else half-entry.symbol)
                                           ELSE symb
             stmt.inn           = op.inn
             stmt.name-ben      = op.name-ben

     .
      RUN GetOpDetails(BUFFER op, OUTPUT stmt.details) NO-ERROR.
   END.
END. /* for each op-entry... */

