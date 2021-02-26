{globals.i}
Define Input Parameter contcode As CHAR. /* Номер договора */
Define Output Parameter mProl As INT.

mProl = 0.
FIND FIRST loan WHERE loan.contract = 'DPS' AND loan.cont-code = (contcode + '@' + ShFilial) NO-LOCk NO-ERROR.
IF NOT AVAIL loan THEN RETURN.
mProl = loan.prolong.





                                                   


