/*
	ВКЛАДЫ
*/

{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
/*{intrface.get lngar}*/
/*{intrface.get chwch}*/
/*{intrface.get xobj}*/     /* Библиотека для работы с объектами. */
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


/*def new shared stream vvs.                           */
/*def var fname as char  init "./depfp.txt"  no-undo.  */
/*def var delim as char init ";" format "x(1)" no-undo.*/
/*def var eol as char format "x(2)" no-undo.           */
/*eol = chr(13) + chr(10).                             */

def var ni as int64.
DEF VAR vFlag     AS LOG  NO-UNDO. /*начисления */

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
def var dProl as date no-undo.
DEF VAR vComm     AS char NO-UNDO. /*код комисии*/
DEF VAR vInter    AS CHAR NO-UNDO. /*схема начисления*/
def var vSubCod as char no-undo.
DEF VAR vResult   AS DEC  NO-UNDO.
DEF VAR vResult1  AS DEC  NO-UNDO.
DEF VAR vRateComm AS DEC  NO-UNDO. /*ставка на момент открытия*/
DEF VAR mPenComm AS CHAR  NO-UNDO. 
def var rateclose as dec no-undo. /* ставка досрочки */
DEF VAR vValOst AS DEC  NO-UNDO.
DEF VAR vPeriod AS INT  NO-UNDO.
DEF VAR in-kau AS CHAR  NO-UNDO.

DEFINE TEMP-TABLE Loan-fp-m
    FIELD num as int64
    FIELD city as character
    FIELD cust-cat AS CHARACTER
    FIELD cust-id AS int64
    FIELD cli-name AS CHARACTER
    FIELD cont-code AS CHARACTER
    FIELD cont-type AS CHARACTER
    FIELD open-date AS date
    FIELD end-date AS date
    FIELD close-date AS date
    FIELD curr AS character
    FIELD period AS int64
    FIELD summ AS DECIMAL
    FIELD comm AS decimal
    FIELD rateclose AS DECIMAL
    FIELD intperiod AS CHARACTER
    FIELD minost AS DECIMAL
    FIELD acct AS CHARACTER
    FIELD acctbal AS DECIMAL
    FIELD acctproc AS CHARACTER
    FIELD acctprocbal AS DECIMAL
    FIELD dateprol AS DATE
    .

DEFINE VARIABLE fname as char   no-undo.

{getdate.i}

ni = 0.

FOR EACH tmprecid,
   FIRST loan WHERE RECID(loan) EQ tmprecid.id no-lock,
   FIRST loan-cond where loan-cond.cont-code eq loan.cont-code and loan-cond.contract eq loan.contract
NO-LOCK:


/*   Сумма вклада   */
      RUN get-summ-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mSumm).


/* Мин остаток на вкладе */
      RUN get_last_min_ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mMinOst).
      if mMinOst = ? then mMinOst=0.


/*   Наименование клиента   */
      find first cust-role
	        where (loan.cust-cat=cust-role.cust-cat) and (string(loan.cust-id) = cust-role.cust-id) no-lock no-error.
      if avail cust-role then do:
	        mName = cust-role.short-name.
	  end.

/*   Счет депозита   */
      FIND FIRST loan-acct WHERE loan-acct.contract  EQ loan.contract
      			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ (if loan.end-date <> ? then "loan-dps-t" else "loan-dps-p")
		        NO-LOCK NO-ERROR.

      mAcctN   = IF AVAIL loan-acct THEN loan-acct.acct ELSE "".
      mAcctCur = IF AVAIL loan-acct THEN loan-acct.currency ELSE "".

/*   Остаток на депоз. счете   */
      if mAcctN <> "" then do:
	      RUN acct-pos IN h_base (mAcctN, mAcctCur, end-date, end-date, "П").
	      mAcctBal = - sh-bal.
      end. ELSE mAcctBal = 0.

/*   Счет процентов   */
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

/*   Остаток на счете процентов   */
      if mAcctProc <> "" then do:
	      RUN acct-pos IN h_base (mAcctProc, mAcctProcCur, end-date, end-date, "П").
	      mAcctProcBal = - sh-bal.
      end. ELSE mAcctProcBal = 0.


/*   Город   */
     FIND FIRST branch WHERE branch.branch-id  EQ loan.branch-id NO-LOCK NO-ERROR.
     IF AVAIL branch THEN DO:
	mCity = branch.branch-id.
     END.


/*   Валюта   */
     If loan.curr = "" then mCurr = "810". else mCurr = loan.curr.

/*   Выплата процентов   */
     FIND FIRST code WHERE code.parent = "int-period" and code.code =  loan-cond.int-period NO-LOCK NO-ERROR.
     IF AVAIL code THEN DO:
	mIntPeriod = code.name.
     END.


/*   Нач последнего периода   */
     RUN get-beg-date-prol IN h_dpspc (RECID(loan), end-date, OUTPUT vBegDate, OUTPUT vEndDate).
     if vBegDate <> loan.open-date
	then dProl = vBegDate.
	else dProl = ?.

    ASSIGN
     vRateComm = ?
     mPenComm = ?
     vValOst =?
     vPeriod = ?
     rateclose = ?
     .
     
     IF mAcctN <> "" THEN DO:
        FIND FIRST acct WHERE acct.acct EQ mAcctN NO-LOCK NO-ERROR.        
        /*   процент досрочки   */
        RUN Get_Last_Pen-Commi IN h_dpspc (RECID(loan), end-date, end-date, OUTPUT mPenComm).
        /* если нет штрафной комиссии, то берем основную */
        IF NOT {assigned mPenComm} THEN
            RUN Get_Last_Commi in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT mPenComm).
        in-kau  = loan.contract + "," + 
             loan.cont-code + "," + 
             IF loan.end-date = ? 
               THEN "ОстВклВ" 
               ELSE "ОстВклС".
        /*реальный остаток на вкладе на день открытия */
        run kau-pos.p (acct.acct,
                   acct.currency,
                   vBegDate,
                   vBegDate,
                   gop-status,
                   in-kau).
         vValOst = ABS(if acct.currency eq '' then ksh-bal  else ksh-val) . 
	 vPeriod = end-date - vBegDate.

     IF {assigned mPenComm} THEN
       DO:
         {findcom1.i
          &dir       = LAST
          &comm-rate = comm-rate
          &rcom      = mPenComm
          &rsum      = vValOst
          &since1    = "LE vBegDate"
          &vPeriodInt = vPeriod
         }
         rateclose = IF AVAIL comm-rate THEN comm-rate.rate-comm ELSE 0.
       end.
       else rateclose = ?.	


	/*   Определение кода ставки   */
        RUN Get_Last_Commi in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT vComm).
        RUN Get_Last_Inter in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT vInter).
        IF vComm  = ? OR vComm  = "?" OR
           vInter = ? OR vInter = "?" THEN NEXT.
	vSubCod = IF loan.end-date NE ? THEN 'ОстВклС' ELSE 'ОстВклВ'.

        { findsch.i
             &dir    = last
             &sch    = vInter
             &since1 = " le end-date"
        }
        IF AVAIL interest-sch-line
        THEN DO:
          vBegDate1 = end-date - 1.
          RUN nachkin.p(RECID(interest-sch-line),
                        vComm,
                        RECID(acct),
                        end-date,
                        loan.contract + "," + loan.cont-code + "," + vSubCod ,
                        yes,
                        OUTPUT vResult,
                        OUTPUT vResult1,
                        INPUT-OUTPUT vBegDate1,
                        OUTPUT vFlag).
           vRateComm = DEC(ENTRY(4,RETURN-VALUE)) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN vRateComm=?.
        END.
        ELSE DO:
          RUN GetCommRate(BUFFER acct, vInter, vComm, OUTPUT vRateComm ).
        END.
     END.

     ni = ni + 1.

/*   Запись данных по КД   */
      CREATE loan-fp-m.
      ASSIGN
          loan-fp-m.num = ni
          loan-fp-m.city = mCity
          loan-fp-m.cust-cat = loan.cust-cat
          loan-fp-m.cust-id = loan.cust-id
          loan-fp-m.cli-name = mName
          loan-fp-m.cont-code = loan.cont-code
          loan-fp-m.open-date = loan.open-date
          loan-fp-m.end-date = loan.end-date
          loan-fp-m.intperiod = mIntPeriod
          loan-fp-m.summ = mSumm
          loan-fp-m.comm = vRateComm
          loan-fp-m.rateclose = rateclose
	  loan-fp-m.acct = mAcctN
	  loan-fp-m.acctbal = mAcctBal
	  loan-fp-m.acctproc = mAcctProc
	  loan-fp-m.acctprocbal = mAcctProcBal
	  loan-fp-m.cont-type = loan.cont-type
	  loan-fp-m.curr = mcurr
	  loan-fp-m.minost = mMinOst
	  loan-fp-m.dateprol = dProl
	  loan-fp-m.period = vEndDate - vBegDate
	  loan-fp-m.close-date = loan.close-date.
          .

      

END.

fname = "svod_" + STRING(end-date,"99/99/9999") + ".csv". 
fname = REPLACE(fname,"/",""). 

OUTPUT TO VALUE (fname)
    UNBUFFERED  CONVERT SOURCE "IBM866" TARGET "1251".
    
PUT UNFORMATTED
  ";  №  ;       Город        ;ТИП; КОД КЛ;     НАИМЕНОВАНИЕ КЛИЕНТА     ;        НОМЕР ДОГОВОРА        ;  ТИП ДЕПОЗИТА   ;ВАЛ; СРОК ; СУММА ДЕПОЗИТА  ;СТАВКА;СТ.РАС; ДАТА С   ; ДАТА ПО  ;    ВЫПЛАТА ПРОЦ.   ; НЕСНИЖАЕМЫЙ ОСТ.;   СЧЕТ ДЕПОЗИТА    ; ОСТ. НА ДЕП. СЧ.;   СЧЕТ ПРОЦЕНТОВ   ;ОСТ. НА СЧ. ПРОЦ.;Д.ПРОЛОНГ.;" 
SKIP.

FOR EACH loan-fp-m NO-LOCK:
     PUT UNFORMATTED
   	  ";"  STRING(loan-fp-m.num, ">>>>9")
   	  ";"  STRING(loan-fp-m.city, "x(20)")
   	  ";"  STRING(loan-fp-m.cust-cat, "x(3)")
   	  ";"  STRING(loan-fp-m.cust-id, ">>>>>>9")
   	  ";"  STRING(loan-fp-m.cli-name, "x(30)")
   	  ";"  STRING(loan-fp-m.cont-code, "x(30)")
   	  ";"  STRING(loan-fp-m.cont-type, "x(17)")
   	  ";"  STRING(loan-fp-m.curr, "x(3)")
   	  ";"  (if loan-fp-m.period <> ? THEN STRING(loan-fp-m.period, ">>>>>9") ELSE STRING("","x(6)"))
   	  ";"  STRING(loan-fp-m.summ, "->,>>>,>>>,>>9.99")
   	  ";"  STRING(loan-fp-m.comm, ">>9.99")
   	  ";"  STRING(loan-fp-m.rateclose,">>9.99")
   	  ";"  STRING(loan-fp-m.open-date, "99/99/9999")
   	  ";"  (if loan-fp-m.end-date <> ? THEN STRING(loan-fp-m.end-date, "99/99/9999") ELSE STRING("","x(10)"))
   	  ";"  STRING(loan-fp-m.intperiod,"x(20)")
   	  ";"  STRING(loan-fp-m.minost, "->,>>>,>>>,>>9.99")
   	  ";"  loan-fp-m.acct
   	  ";"  STRING(loan-fp-m.acctbal, "->,>>>,>>>,>>9.99")
   	  ";"  loan-fp-m.acctproc
   	  ";"  STRING(loan-fp-m.acctprocbal, "->,>>>,>>>,>>9.99")
   	  ";"  if loan-fp-m.dateprol <> ? then STRING(loan-fp-m.dateprol, "99/99/9999") else "  /  /    "
        ";"  (if loan-fp-m.close-date <> ? THEN STRING(loan-fp-m.close-date, "99/99/9999") ELSE STRING("","x(10)"))	  
        ";"  
     SKIP.
END.

OUTPUT CLOSE.

MESSAGE "Данные выгружены в файл " + fname + "." 
VIEW-AS ALERT-BOX.

{intrface.del}


