/*
	‚Š‹€„›
*/

{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
/*{intrface.get lngar}*/
/*{intrface.get chwch}*/
/*{intrface.get xobj}*/     /* ¨¡«¨®â¥ª  ¤«ï à ¡®âë á ®¡ê¥ªâ ¬¨. */
{client.i}
/*{lshpr.pro}*/
{tmprecid.def}
{wordwrap.def}
/*{svarloan.def}*/
{navigate.def}
/*{loan_par.def &new = new}*/
{flt-file.i}
{sh-defs.i}
{ksh-defs.i NEW}

{intrface.get dps}
{dpsproc.def}


DEF VAR nn          AS INT64     NO-UNDO.
def var eDate as date no-undo.
def var cDate as date no-undo.
def new shared stream vvs.
def var fname as char  init "./depfp.txt"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def var ni as int64.
DEF VAR vFlag     AS LOG  NO-UNDO. /*­ ç¨á«¥­¨ï */
def var dProl as date no-undo.

DEF BUFFER bloan     FOR loan.
DEF VAR mCurr AS CHARACTER NO-UNDO.
DEF VAR mSumm AS DECIMAL NO-UNDO.
DEF VAR mName AS CHARACTER NO-UNDO.
DEF VAR mAcctN AS CHARACTER NO-UNDO.
DEF VAR mAcctBal AS DECIMAL NO-UNDO.
DEF VAR mAcctCur AS CHARACTER NO-UNDO.
DEF VAR mAcctProc AS CHARACTER NO-UNDO.
DEF VAR mAcctProcCur AS CHARACTER NO-UNDO.
DEF VAR mAcctProcBal AS DECIMAL NO-UNDO.
DEF VAR mCity AS character NO-UNDO.
DEF VAR mMinOst AS decimal NO-UNDO.
DEF VAR mIntPeriod AS character NO-UNDO.
def var vBegDate as date no-undo.
def var vBegDate1 as date no-undo.
def var vEndDate as date no-undo.
DEF VAR vComm     AS char NO-UNDO. /*ª®¤ ª®¬¨á¨¨*/
DEF VAR vInter    AS CHAR NO-UNDO. /*áå¥¬  ­ ç¨á«¥­¨ï*/
def var vSubCod as char no-undo.
DEF VAR vResult   AS DEC  NO-UNDO.
DEF VAR vResult1  AS DEC  NO-UNDO.
DEF VAR vRateComm AS DEC  NO-UNDO. /*áâ ¢ª  ­  ¬®¬¥­â ®âªàëâ¨ï*/
DEF VAR mPenComm AS CHAR  NO-UNDO. 
def var rateclose as dec no-undo. /* áâ ¢ª  ¤®áà®çª¨ */
DEF VAR vValOst AS DEC  NO-UNDO.
DEF VAR vPeriod AS INT  NO-UNDO.
DEF VAR in-kau AS CHAR  NO-UNDO.

DEFINE TEMP-TABLE Loan-fp-m
    FIELD num as int64
    FIELD cont-code AS CHARACTER
    FIELD cont-type AS CHARACTER
    FIELD acctproc AS CHARACTER
    FIELD acctprocbal AS DECIMAL
    FIELD prol-date AS date
    .

{getdate.i}
{setdest.i &col=170 }

PUT UNFORMATTED
  "ÚÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿" SKIP
  "³  ü  ³        Œ… „ƒ‚€        ³  ’ˆ ‚Š‹€„€    ³    ‘—…’ „…‡ˆ’€    ³ ‘’. € „…. ‘— ³„€’€ ‹ ³ " SKIP
  "ÃÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´" SKIP.

ni = 0.

FOR EACH tmprecid,
   FIRST loan WHERE RECID(loan) EQ tmprecid.id no-lock,
   FIRST loan-cond where loan-cond.cont-code eq loan.cont-code and loan-cond.contract eq loan.contract
NO-LOCK:
/*   ‘ç¥â ¯à®æ¥­â®¢   */
      FIND FIRST loan-acct WHERE loan-acct.contract  EQ loan.contract
                         AND loan-acct.cont-code EQ loan.cont-code AND loan-acct.acct-type EQ "loan-dps-int"
     			 NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN DO:
	mAcctProc = loan-acct.acct.
	mAcctProcCur = loan-acct.currency.
      END. ELSE DO:
	mAcctProc = "".
	mAcctProcCur = "".
      END.

/*   áâ â®ª ­  áç¥â¥ ¯à®æ¥­â®¢   */
      if mAcctProc <> "" then do:
	      RUN acct-pos IN h_base (mAcctProc, mAcctProcCur, end-date, end-date, "").
	      mAcctProcBal = - sh-bal.
      end. ELSE DO:
       mAcctProcBal = 0.
      end.
      IF loan.prolong GE 1 THEN
           DO:
	 RUN get-beg-date-prol(RECID(loan),end-date,output cDate, output eDate).
        if cDate <> loan.open-date
        then dProl = cDate.
        else dProl = ?.
        end.
        ELSE DO:
        dProl = ?.
        END.
        
/*   ‡ ¯¨áì ¤ ­­ëå ¯® Š„   */
      CREATE loan-fp-m.
      ASSIGN
          loan-fp-m.num = ni
          loan-fp-m.cont-code = loan.cont-code
	  loan-fp-m.acctprocbal = mAcctProcBal
	  loan-fp-m.cont-type = loan.cont-type
	  loan-fp-m.acctproc = mAcctProc
	  loan-fp-m.prol-date = dProl
          .
 ni = ni + 1.

IF loan-fp-m.acctprocbal NE 0 then
 DO:
      PUT UNFORMATTED
	  "³"  STRING(loan-fp-m.num , ">>>>9")
	  "³"  STRING(loan-fp-m.cont-code, "x(30)")
	  "³"  STRING(loan-fp-m.cont-type, "x(16)")
	  "³"  STRING(loan-fp-m.acctproc, "x(21)")
	  "³"  STRING(loan-fp-m.acctprocbal, "->,>>>,>>>,>>9.99")
	  "³"  (if loan-fp-m.prol-date <> ? THEN STRING(loan-fp-m.prol-date, "99/99/9999") ELSE STRING("","x(10)"))
          "³"  skip.

END.
END.

PUT UNFORMATTED
  "ÀÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ" SKIP.

{preview.i &col=170}

/*
output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream vvs unformatted
    "cli-name" delim
    "sum_total" delim
    "curr" delim
    "sum_rez" delim
    "sum_pros" delim
    "p_4" eol.
for each LoanGroup no-lock:
  put stream vvs unformatted
    LoanGroup.cli-name delim
    LoanGroup.sum_total delim
    LoanGroup.curr delim
    LoanGroup.sum_rez delim
    LoanGroup.sum_pros delim
    LoanGroup.p_4 eol.
end.
output stream vvs close.
MESSAGE "„ ­­ë¥ ¢ë£àã¦¥­ë ¢ ä ©« " + fname + "." VIEW-AS ALERT-BOX.
*/
{intrface.del}
