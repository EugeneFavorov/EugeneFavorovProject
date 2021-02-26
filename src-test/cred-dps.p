{globals.i}

DEFINE BUFFER bloan FOR loan.
DEFINE BUFFER bloan1 FOR loan.
DEFINE BUFFER bloan-acct FOR loan-acct.
DEFINE BUFFER bloan-acct1 FOR loan-acct.
DEFINE BUFFER bloan-acct2 FOR loan-acct.

{setdest.i}
PUT UNFORMATTED "       Кредиты со срочным вкладом в качестве обеспечения" SKIP.
PUT UNFORMATTED "       у которых открыт счет 423, но нет договора вклада." SKIP.
FOR EACH bloan-acct
  WHERE bloan-acct.contract EQ "Кредит"
	AND bloan-acct.acct BEGINS "4550" NO-LOCK,
  FIRST bloan 
    WHERE bloan.close-date EQ ?
    AND bloan.open-date LE DATE("20.08.2012")
    AND bloan.close-date EQ ?
  NO-LOCK:
/* PUT UNFORMATTED bloan-acct.acct " " bloan.cont-code SKIP. */
  FIND FIRST bloan-acct1
    WHERE bloan-acct1.contract EQ bloan.contract
    AND bloan-acct1.cont-code EQ bloan.cont-code
    AND bloan-acct1.acct BEGINS "423"
    NO-LOCK NO-ERROR.
  IF AVAILABLE(bloan-acct1) THEN  
  DO:
    FIND FIRST bloan-acct2
      WHERE bloan-acct2.contract EQ "dps"
      AND bloan-acct2.acct EQ bloan-acct1.acct
      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(bloan-acct2) THEN
    DO:
      FIND FIRST person
        WHERE person.person-id EQ bloan.cust-id
        NO-LOCK NO-ERROR.
        PUT UNFORMATTED
        person.name-last
        " "
        person.first-names
        " "
        bloan.cont-code
        SKIP.
    END.
    END.
  
  
END.

{preview.i}
