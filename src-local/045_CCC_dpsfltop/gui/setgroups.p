{globals.i}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get db2l}

{sh-defs.i}

DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mOk    AS LOGICAL   NO-UNDO.

DEFINE BUFFER b-signs FOR signs.

{setdest.i &filename = "'111.log'"}

mInt = 0.


/*SELECT ACCOUNTS.ACCOUNT,ACCOUNTS.GRP*/
/*FROM ACCOUNTS                       */
/*WHERE                               */
/*ACCOUNTS.GRP >= 500                 */
/*--AND ACCOUNTS.GRP <= 599           */
/*AND ACCOUNTS.CLOSTIME is null       */
/*AND ACCOUNTS.ACCOUNT  NOT LIKE '0%' */
/*AND ACCOUNTS.ACCOUNT  NOT LIKE 'U%' */
/*AND ACCOUNTS.ACCOUNT  NOT LIKE 'E%' */
/*AND ACCOUNTS.ACCOUNT  NOT LIKE 'T%' */

FOR EACH bank.ACCOUNTS WHERE
       ACCOUNTS.GRP GE 500
   /*AND ACCOUNTS.GRP EQ 599*/    /*указанная группа*/
   AND ACCOUNTS.CLOSTIME EQ ?
   AND NOT (ACCOUNTS.ACCOUNT BEGINS "0") 
   AND NOT (ACCOUNTS.ACCOUNT BEGINS "U")
   AND NOT (ACCOUNTS.ACCOUNT BEGINS "E")
   AND NOT (ACCOUNTS.ACCOUNT BEGINS "T")
   NO-LOCK,
   EACH qbis.acct WHERE
       qbis.acct.filial-id EQ "0500"
   AND qbis.acct.number    EQ ACCOUNTS.ACCOUNT
   NO-LOCK:
   
   /*значение в code-value*/   
   FOR EACH signs WHERE
       signs.file-name   EQ "acct"
   AND signs.code        EQ "groupOABS"
   AND signs.surrogate   EQ qbis.acct.acct + "," + qbis.acct.currency
   NO-LOCK:
      LEAVE.
   END.
   /**/
   IF NOT AVAIL(signs) THEN
   DO:
      mInt = mInt + 1.
      mOk = UpdateSigns("acctb",
                  qbis.acct.acct + ',' + qbis.acct.currency,
                  "groupOABS",
                  TRIM(STRING(bank.ACCOUNTS.GRP,">>>>>9")),
                  ?).
      PUT UNFORMATTED
         mInt                  ";"
         mOk                   ";"
         bank.ACCOUNTS.ACCOUNT ";"
         bank.ACCOUNTS.GRP     ";"
         qbis.acct.number
      SKIP.
   END.
END.

IF mInt EQ 0 THEN
   PUT UNFORMATTED "Acct not found." SKIP.

mInt = 0.
FOR EACH  signs WHERE
          signs.file-name   EQ "cust-corp"
      AND signs.code        EQ "CID"
/*      AND signs.xattr-value EQ "35327"*/
      NO-LOCK:

   FIND FIRST bank.GROUPS WHERE
      bank.GROUPS.CID EQ INT64(signs.xattr-value)
   NO-LOCK NO-ERROR.
   IF AVAIL(bank.GROUPS) THEN
   DO:
      FOR EACH b-signs WHERE
          b-signs.file-name   EQ "cust-corp"
      AND b-signs.code        EQ "groupOABS"
      AND b-signs.surrogate   EQ signs.surrogate 
      NO-LOCK:
         LEAVE.
      END.
      IF NOT AVAIL(b-signs) THEN
      DO:
         mInt = mInt + 1.
	      UpdateSigns("cust-corp",
	                  signs.surrogate,
	                  "groupOABS",
	                  TRIM(STRING(bank.GROUPS.GRP,">>>>>9"))
	                  ,?).
         PUT UNFORMATTED
            signs.code " = "
            signs.xattr-value ";"
            " cust-id = " signs.surrogate ";"
            bank.GROUPS.GRP
         SKIP.
      END.
   END.
END.

IF mInt EQ 0 THEN
   PUT UNFORMATTED "Cust-corp not found." SKIP.
   
mInt = 0.
FOR EACH  signs WHERE
       signs.file-name   EQ "person"
   AND signs.code        EQ "CIDIP"
/* AND signs.xattr-value EQ "35327"*/
   NO-LOCK,
   EACH acct WHERE
       acct.cust-cat   EQ "Ч"
   AND acct.cust-id    EQ INT64(signs.surrogate)
   AND acct.bal-acct   EQ 40802
/*   AND acct.close-date EQ ?*/
   NO-LOCK:
   
   FIND FIRST bank.GROUPS WHERE
      bank.GROUPS.CID EQ INT64(signs.xattr-value)
   NO-LOCK NO-ERROR.
   IF AVAIL(bank.GROUPS) 
   AND CAN-DO("581,582,583,584,585,586,516,517",TRIM(STRING(bank.GROUPS.GRP,">>>>>9"))) THEN
   DO:
      FOR EACH b-signs WHERE
          b-signs.file-name   EQ "person"
      AND b-signs.code        EQ "groupOABS"
      AND b-signs.surrogate   EQ signs.surrogate 
      NO-LOCK:
         LEAVE.
      END.
      IF NOT AVAIL(b-signs) THEN
      DO:
         mInt = mInt + 1.
	      UpdateSigns("person",
	                  signs.surrogate,
	                  "groupOABS",
	                  TRIM(STRING(bank.GROUPS.GRP,">>>>>9"))
	                  ,?).
         PUT UNFORMATTED
            "Нет "
            signs.code " = "
            signs.xattr-value ";"
            " person-id = " signs.surrogate ";"
            bank.GROUPS.GRP
         SKIP.
      END.
   END.
END.

IF mInt EQ 0 THEN
   PUT UNFORMATTED "Person not found." SKIP.

{preview.i &filename = "'111.log'"}

{intrface.del}

RETURN.
