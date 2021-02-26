{globals.i}
Define Input Parameter inAcct As Char. /* ëó ÑÅ, ëó äê */
Define Output Parameter mSum As Decimal.
mSum = 0.
/*
MESSAGE inAcct VIEW-AS ALERT-BOX.
  */

FIND LAST op-entry WHERE 
       op-entry.acct-db   EQ entry(1,inAcct)
   AND op-entry.acct-cr   EQ entry(2,inAcct)
   AND op-entry.op-date   EQ date(entry(3,inAcct))
   AND op-entry.op-status GE "˚" no-lock no-error.
IF AVAIL(op-entry) THEN
DO:
   IF op-entry.currency EQ '' THEN
   DO:
      mSum = abs(op-entry.amt-rub).
   END.
   ELSE
   DO:
      mSum =  abs(op-entry.amt-cur).
   END.
END.
/* MESSAGE entry(3,inAcct) VIEW-AS ALERT-BOX.*/
/* try(3,inAcct) VIEW-AS ALERT-BOX.*/
