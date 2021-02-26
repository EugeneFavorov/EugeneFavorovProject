
/* kam */
/* считаем сумму комиссии(перв плат период) кредита на дату пересчета */

{globals.i}
{sh-defs.i}
{intrface.get loan}

Define Input Parameter contcode As CHAR. /* Номер договора */
Define Output Parameter mSumComm As DECIMAL.

DEFINE BUFFER bterm-obl for term-obl.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE VARIABLE mSumFullComm AS DECIMAL NO-UNDO.
DEFINE VARIABLE firstDate AS DATE NO-UNDO.
DEFINE VARIABLE not0 AS LOGICAL NO-UNDO.
DEFINE VARIABLE vydDate AS DATE NO-UNDO.

firstDate = ?.
mSumComm = 0.
mSumFullComm = 0.
not0 = TRUE.
contcode = AddFilToLoan(contcode, shFilial).
FIND FIRST loan WHERE loan.contract = 'Кредит' AND loan.cont-code = contcode NO-LOCk NO-ERROR.
IF NOT AVAIL loan THEN RETURN.

	vydDate = loan.open-date.
	FOR EACH loan-int OF loan WHERE loan-int.id-d = 0
		AND loan-int.id-k = 3 NO-LOCK
		BY loan-int.mdate:
		vydDate = loan-int.mdate.
		LEAVE.
    END.


  FIND FIRST term-obl OF loan WHERE term-obl.idnt EQ 10 AND term-obl.nn = 173 NO-LOCK NO-ERROR.              
  IF AVAIL term-obl THEN DO:
      mSumFullComm = term-obl.amt-rub.
/*
      RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract,
                                       loan.cont-code,
                                       3,
                                       loan.open-date,
                                       BUFFER term-obl).
*/
      RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract,
                                       loan.cont-code,
                                       1,
                                       loan.open-date,
                                       BUFFER bterm-obl).
      /*
      IF AVAIL term-obl AND
       term-obl.dsc-beg-date <= 
        (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
      THEN ASSIGN
        firstDate   = term-obl.dsc-beg-date.
   

      IF AVAIL bterm-obl AND
       bterm-obl.dsc-beg-date <= 
        (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
      THEN ASSIGN
        firstDate   = bterm-obl.dsc-beg-date.
    */
      IF AVAIL bterm-obl 
      THEN ASSIGN
        firstDate   = bterm-obl.dsc-beg-date.

      IF firstDate > loan.since OR firstDate = ? THEN DO:
          mSumComm = (mSumFullComm / (firstDate - vydDate)) * (loan.since - vydDate).
          FOR EACH loan-int OF loan WHERE loan-int.id-k = 377 AND loan-int.mdate <= loan.since NO-LOCK:
            mSumComm = mSumComm - loan-int.amt-rub.
          END.
          not0 = FALSE.
      END.
  END.

  FIND FIRST loan-acct OF loan WHERE loan-acct.acct-type = 'КредБудКом' NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN DO:
      RUN acct-pos IN h_base (loan-acct.acct,
                           loan-acct.currency,
                           loan.since,
                           loan.since,
                                    ?).
      IF not0 OR mSumComm > ABSOLUTE(sh-bal) THEN mSumComm = ABSOLUTE(sh-bal).
  END.


