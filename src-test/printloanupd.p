{globals.i}

run printloan.p.

DEFINE SHARED VARIABLE rid_loan  AS RECID.
{intrface.get xclass}

FIND FIRST loan WHERE
     RECID(loan) EQ rid_loan
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan THEN
   RETURN.

IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"date_zayper",?) <> ? THEN RETURN.
IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"date_trans",?) = ? THEN RETURN.
IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"acct_trans",?) = ? THEN RETURN.

UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code,'date_zayper', STRING(TODAY,"99.99.9999"), ?).  

