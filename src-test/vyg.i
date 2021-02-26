
DEF VAR mPosCode     AS CHAR NO-UNDO. /* Код портфеля */
DEF VAR mPosName     AS CHAR NO-UNDO. /* Наименование портфеля */
DEF VAR mAcct47427   AS CHAR NO-UNDO. /* счет 47427 */
DEF VAR mAcct47423   AS CHAR NO-UNDO. /* счет 47423 */
DEF VAR mAcct47425_27   AS CHAR NO-UNDO. /* счет 47425 к 47427 */
DEF VAR mAcct47425_23   AS CHAR NO-UNDO. /* счет 47425 к 47423 */
DEF VAR mAcct459     AS CHAR NO-UNDO. /* счет 459 */
DEF VAR mAcct458     AS CHAR NO-UNDO. /* счет 459 */
DEF VAR mAcctRez   AS CHAR NO-UNDO. /* счет резерва */
DEF VAR mCur   like acct.currency no-undo. /* валюта счета */
DEF VAR cc   AS CHAR NO-UNDO. /* счет резерва */
DEF VAR mCred-period   AS CHAR NO-UNDO. /* счет резерва */
DEF VAR cc2  AS CHAR NO-UNDO. /* счет резерва */


DEF VAR mAcctSsud-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct47427-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct47423-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct47425_27-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct47425_23-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct459-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct458-pos   like op-entry.amt-rub no-undo. /*  */
DEF VAR mAcct458-date   like acct-pos.since no-undo. /*  */
DEF VAR mAcctRez-pos   like op-entry.amt-rub no-undo. /*  */

DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DEFINE VARIABLE mCID AS CHAR    NO-UNDO.
DEF VAR mPymentGE14   like op-entry.amt-rub no-undo. /*  */
DEFINE VARIABLE vKolPayment AS INTEGER    NO-UNDO.
def var fname as char init "crp.txt" no-undo.
def var cciidd as char no-undo.

{omsk.pro}
/*
 {setdest.i &cols=85}  
 for each rep-table no-lock:
 
 
 put unformatted rep-table.date-proc skip rep-table.summ-proc skip.
 end.
 {preview.i}
 
*/
{setdest.i
  &filename = fname
  &nodef    = "/*"
  &OPTION   = "convert target '1251'"
  &custom="printer.page-lines - "}
  
   /* == page-size(0) */

/* OUTPUT TO "./crp.txt" CONVERT TARGET "1251". */


PUT UNFORMATTED
	"Код Клиента"
	chr(9)
	"Наименование"
	chr(9)
	"Счет в нашей базе"
	chr(9)
	"Платеж"
	chr(9)
	"Дата выдачи"
	chr(9)
	"Дата гашен."
	chr(9)
	"Ставка"
	chr(9)
	"Контракт"
	chr(9)
	"Риск"
	chr(9)
	"Резерв"
	chr(9)
	"Портфель"
	chr(9)
	"47427xx"
	chr(9)
	"Остаток"
	chr(9)
	"Счет резерва по ссуде"
	chr(9)
	"Остаток"
	chr(9)
	"47423xx"
	chr(9)
	"Остаток"
	chr(9)
	"459xx"
	chr(9)
	"Остаток"
	chr(9)
	"47425xx к 47427xx"
	chr(9)
	"Остаток"
	chr(9)
	"47425xx к 47423xx"
	chr(9)
	"Остаток"
	chr(9)
	"ЭПС"
	chr(13)
	chr(10).

FOR EACH rep-table SHARE-LOCK BY rep-table.cont-code:

/* FOR EACH rep-table SHARE-LOCK, FIRST loan OF rep-table BY loan.class-code: */
   FIND FIRST loan 
       WHERE loan.contract EQ rep-table.contract 
       AND loan.cont-code EQ rep-table.cont-code 
       NO-LOCK NO-ERROR.
   IF loan.open-date LE end-date THEN
   DO:

   IF CAN-DO("loan-trans*", loan.class-code) THEN
       vKolPayment = 2.
   ELSE
       vKolPayment = 14.

   ASSIGN
      mPosCode = LnInBagOnDate (rep-table.contract,rep-table.cont-code,end-date)
      mPosName = fGetPosName (mPosCode)
   .
   IF mPosName NE ? THEN
   DO:
       cc = GetXAttrValueEx("loan","ПОС," + mPosCode,"ПОССрПрос","cc").
       mPosName = mPosName + " " + cc.
   END.
   ELSE
       mPosName = " ".

     
   IF rep-table.rate EQ 0 OR rep-table.rate EQ ? THEN
       IF INDEX(rep-table.cont-code," ") <> 0 THEN
       DO:
/*           MESSAGE ENTRY(1, rep-table.cont-code, " ")
               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
           rep-table.rate = GET_COMM_LOAN (rep-table.contract,ENTRY(1, rep-table.cont-code, " "), "%Кред",end-date).	
       END.
       ELSE
           rep-table.rate = GET_COMM_LOAN (rep-table.contract,rep-table.cont-code,"%Кред",end-date).	
   ii = 0.

/*   MESSAGE rep-table.acct
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   IF trim(rep-table.acct) EQ "" OR rep-table.acct EQ ? THEN
   DO:
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code NE rep-table.cont-code
             AND loan-acct.cont-code BEGINS rep-table.cont-code
              AND loan-acct.acct-type EQ "Кредит"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
   DO:
       ASSIGN
       rep-table.acct = loan-acct.acct.
       MESSAGE "Finds - " + loan-acct.acct
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   END.
*/

      FIND LAST loan-cond WHERE
          loan-cond.contract  EQ rep-table.Contract
          AND    loan-cond.cont-code EQ rep-table.Cont-Code
          AND    loan-cond.since     LE end-date
          NO-LOCK NO-ERROR.
      IF AVAIL loan-cond THEN
          mCred-period = loan-cond.cred-period.

/* поик ссудного счета и остатка если период погашения П или Кс */
        FIND LAST loan-acct 
            WHERE loan-acct.contract  EQ rep-table.contract
            AND loan-acct.cont-code EQ rep-table.cont-code
            AND loan-acct.acct-type EQ "Кредит"
            AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN
        DO:
            FIND FIRST acct 
            WHERE acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
            IF acct.cust-cat EQ "Ю" THEN
            DO:
                 mCID = GetXattrValueEx("cust-corp",
                                    STRING(acct.cust-id),
                                    "cid",
                                    "NO CID").
            END.
            ELSE
            DO:
		 IF SUBSTRING(acct.acct,1,3) = '454' THEN cciidd = 'cidip'.
			 else cciidd = 'cid'.
                 mCID = GetXattrValueEx("person",
                                    STRING(acct.cust-id),
                                    cciidd,
                                    "NO CID").
            END.

        IF /* mCred-period EQ "П" OR */ mCred-period EQ "Кс" THEN  
        DO:
            RUN acct-pos IN h_base (acct.acct,
                                    acct.currency,
                                    end-date,
                                    end-date,
                                    ?).
                IF acct.currency EQ "" THEN
                    mAcctSsud-pos = ABSOLUTE(sh-bal).
                ELSE
                    mAcctSsud-pos = ABSOLUTE(sh-val).
        END.
        ELSE
            mCred-period = " ".

    END.

/* поик счета и остатка 47427 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредТ"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct47427 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcct47427-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct47427-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcct47427 = ""
	mAcct47427-pos = 0
.

/* поик счета и остатка 47423 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредБудКом"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct47423 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcct47423-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct47423-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcct47423 = ""
	mAcct47423-pos = 0
.

/* поик счета и остатка 47425_27 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредРезП"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct47425_27 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcct47425_27-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct47425_27-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcct47425_27 = ""
	mAcct47425_27-pos = 0
.

/* поик счета и остатка 47425_23 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредРезКом"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct47425_23 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcct47425_23-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct47425_23-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcct47425_23 = ""
	mAcct47425_23-pos = 0
.

/* поик счета и остатка 459 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредПр%"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct459 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcct459-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct459-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcct459 = ""
	mAcct459-pos = 0
.

/* поик счета и остатка резерв */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредРез"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcctRez = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).
          IF acct.currency = "" THEN
		mAcctRez-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcctRez-pos = ABSOLUTE(sh-val).


	END.
   ELSE
	assign
	mAcctRez = ""
	mAcctRez-pos = 0
.

/* поик счета и остатка 458 */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ rep-table.contract
             AND loan-acct.cont-code EQ rep-table.cont-code
              AND loan-acct.acct-type EQ "КредПр"
              AND loan-acct.since     LE end-date
            NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
	DO: 
		mAcct458 = loan-acct.acct.
	find first acct where acct.acct EQ loan-acct.acct
            NO-LOCK NO-ERROR.
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 end-date,
                                 end-date,
                                 ?).

         mAcct458-date = lastmove.
          IF acct.currency = "" THEN
		mAcct458-pos = ABSOLUTE(sh-bal).
	  ELSE
		mAcct458-pos = ABSOLUTE(sh-val).

      FIND LAST acct-pos WHERE
          acct-pos.acct EQ loan-acct.acct
          AND acct-pos.balance EQ 0
           NO-LOCK NO-ERROR.
      IF AVAILABLE(acct-pos) THEN
      DO:
          FIND FIRST op-entry WHERE op-entry.acct-db EQ loan-acct.acct
              AND CAN-DO("45*", op-entry.acct-cr)
              AND op-entry.op-status BEGINS "√"
               NO-LOCK NO-ERROR.
          IF AVAILABLE(op-entry) THEN
              mAcct458-date = op-entry.op-date.
      END.


	END.
   ELSE
	assign
	mAcct458 = ""
	mAcct458-pos = 0
.
  mPymentGE14 = 0.0. 

  DO i = 1 TO NUM-ENTRIES(rep-table.date-proc, chr(179)):

      IF DATE(ENTRY(i, rep-table.date-proc, chr(179))) GE end-date THEN
      DO:
          ii = ii + 1.
		  
      IF ii LT vKolPayment THEN DO:

      IF DECIMAL(ENTRY(i, rep-table.summ-proc, chr(179))) GT 0 THEN 
      DO:
          PUT UNFORMATTED
              mCID
              chr(9)
              rep-table.cli-name
              chr(9)
/*              TRIM(rep-table.acct) + chr(9). */
              "'" + RetAcctOmsk(substring(trim(rep-table.acct), 1, 20)) + chr(9). 
    IF /* mCred-period EQ "П" OR */ mCred-period EQ "Кс" THEN
          PUT UNFORMATTED mAcctSsud-pos.
    ELSE 
          PUT UNFORMATTED
              ENTRY(i, rep-table.summ-proc, chr(179)).
          PUT UNFORMATTED
              chr(9)
              rep-table.beg-date
              chr(9).
              IF i <= NUM-ENTRIES(rep-table.date-proc, chr(179)) THEN
                  PUT UNFORMATTED REPLACE( ENTRY(i, rep-table.date-proc, chr(179)), '.', '/') chr(9).
              ELSE
                  PUT UNFORMATTED " " chr(9).

              PUT UNFORMATTED
                  "'" rep-table.rate
                  chr(9)
                  "'" + REPLACE(rep-table.cont-code, "@0400", "")
                  chr(9)
                  rep-table.gr-riska 
                  chr(9)
                  rep-table.res-fact-proc 
                  chr(9)
                  mPosName
                  chr(9).
              IF ii = 1 THEN 
                  PUT UNFORMATTED
                  "'" + RetAcctOmsk(substring(trim(mAcct47427), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL( mAcct47427-pos))
                  chr(9) 
                  "'" + RetAcctOmsk(substring(trim(mAcctRez), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL(mAcctRez-pos))
                  chr(9) 
                  "'" + RetAcctOmsk(substring(trim(mAcct47423), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL(mAcct47423-pos))
                  chr(9) 
                  "'" + RetAcctOmsk(substring(trim(mAcct459), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL(mAcct459-pos))
                  chr(9) 
                  "'" + RetAcctOmsk(substring(trim(mAcct47425_27), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL(mAcct47425_27-pos))
                  chr(9) 
                  "'" + RetAcctOmsk(substring(trim(mAcct47425_23), 1, 20))
                  chr(9)
                  ABSOLUTE(DECIMAL(mAcct47425_23-pos))
                  chr(9) 
      string(rep-table.eps)
      chr(9)
                  .
              ELSE 
                  PUT UNFORMATTED 
                      " " 
                      chr(9) 
                      " "
                      chr(9) 
                      " " 
                      chr(9) 
                      " "
                      chr(9) 
                      " " 
                      chr(9) 
                      " "
                      chr(9) 
                      " " 
                      chr(9) 
                      " "
                      chr(9) 
                      " " 
                      chr(9) 
                      " "
                      chr(9) 
                      " " 
                      chr(9) 
                      " " 
                      chr(9) 
      string(rep-table.eps)
      chr(9)
                      .
      PUT UNFORMATTED
          chr(13)
          chr(10).
      END.
      END.  /* lt 14 */
      
      ELSE  /* lt 14 */
          mPymentGE14 = mPymentGE14 + DECIMAL(ENTRY(i, rep-table.summ-proc, chr(179))).

      END.  /* DATE(ENTRY(i, rep-table.date-proc, chr(179))) GE end-date */
  END.  /* DO i = 1 */
  IF mPymentGE14 GT 0 THEN
  PUT UNFORMATTED
    mCID
    chr(9)
    rep-table.cli-name
    chr(9)
    "'" + RetAcctOmsk(substring(trim(rep-table.acct), 1, 20)) + chr(9)
/*    substring(trim(rep-table.acct), 1, 20) + "@" + chr(9) */
    mPymentGE14
    chr(9)
      rep-table.beg-date
/*    " " */
    chr(9)
    REPLACE( ENTRY(NUM-ENTRIES(rep-table.date-proc, chr(179)), rep-table.date-proc, chr(179)), '.', '/')
    chr(9)
      "'" rep-table.rate
      chr(9)
      "'" + REPLACE(rep-table.cont-code, "@0400", "")
      chr(9)
      rep-table.gr-riska 
      chr(9)
      rep-table.res-fact-proc 
      chr(9)
      mPosName
      chr(9)
      string(rep-table.eps)
      chr(9)
    chr(13)
    chr(10).

IF mAcct458-pos GT 0 THEN
    PUT UNFORMATTED
    mCID
    chr(9)
    rep-table.cli-name
    chr(9)
    "'" + RetAcctOmsk(substring(trim(mAcct458), 1, 20)) + chr(9)
    mAcct458-pos
    chr(9)
    rep-table.beg-date
/*    " " */
    chr(9)
    mAcct458-date
    chr(9)
    "'" rep-table.rate
    chr(9)
    "'" + REPLACE(rep-table.cont-code, "@0400", "")
    chr(9)
    rep-table.gr-riska 
    chr(9)
    rep-table.res-fact-proc 
    chr(9)
    mPosName
    chr(9)
      string(rep-table.eps)
      chr(9)
	chr(13)
	chr(10).
    END.
END.
/* OUTPUT CLOSE. 
MESSAGE "crp.txt export "
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/ 
{preview.i &filename = fname } 

RUN sndbispc ("file=" + fname + ";class=bq").
