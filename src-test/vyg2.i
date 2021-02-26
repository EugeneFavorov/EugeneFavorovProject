
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
def var fname as char init "crp.csv" no-undo.

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
	";"
	"Наименование"
	";"
	"Счет в нашей базе"
	";"
	"Платеж"
	";"
	"Дата выдачи"
	";"
	"Дата гашен."
	";"
	"Ставка"
	";"
	"Контракт"
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



      FIND LAST loan-cond WHERE
          loan-cond.contract  EQ rep-table.Contract
          AND    loan-cond.cont-code EQ rep-table.Cont-Code
          AND    loan-cond.since     LE end-date
          NO-LOCK NO-ERROR.
      IF AVAIL loan-cond THEN
          mCred-period = loan-cond.cred-period.

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
         mCID = GetXattrValueEx("person",
                            STRING(acct.cust-id),
                            "cid",
                            "NO CID").
    END.
end.
    
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
              ";"
              rep-table.cli-name
              ";"
/*              TRIM(rep-table.acct) + chr(9). */
              "'" + RetAcctOmsk(substring(trim(rep-table.acct), 1, 20)) + ';'. 
    IF /* mCred-period EQ "П" OR */ mCred-period EQ "Кс" THEN
          PUT UNFORMATTED mAcctSsud-pos.
    ELSE 
          PUT UNFORMATTED
              ENTRY(i, rep-table.summ-proc, chr(179)).
          PUT UNFORMATTED
              ";"
              rep-table.beg-date
              ";".
              IF i <= NUM-ENTRIES(rep-table.date-proc, chr(179)) THEN
                  PUT UNFORMATTED REPLACE( ENTRY(i, rep-table.date-proc, chr(179)), '.', '/') ';'.
              ELSE
                  PUT UNFORMATTED " " ";".

              PUT UNFORMATTED
                  "'" rep-table.rate
                  ";"
                  REPLACE(rep-table.cont-code, "@0400", "")
                  ";"
                  .


      PUT UNFORMATTED
          chr(13)
          chr(10).
      END.

  END.  /* DO i = 1 */
END.
END.
END.
END.

/* OUTPUT CLOSE. 
MESSAGE "crp.txt export "
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/ 
/*
{preview.i &filename = fname } 
*/
RUN sndbispc ("file=" + fname + ";class=bq").
