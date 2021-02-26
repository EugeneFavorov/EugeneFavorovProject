/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: 
      Comment: равняем остатки параметров и остатки на счетах

   Parameters:
      Used by:
      Created: kam 06/07/2016
     Modified: 
*/

{globals.i}
{sh-defs.i}
{intrface.get loan}
{intrface.get lv}
{tmprecid.def}

{getdate.i}

{setdest.i}

DEF VAR listAcctType AS CHAR NO-UNDO.
DEF VAR listLoanPar AS CHAR NO-UNDO.
DEF VAR i AS INT64 NO-UNDO.
DEF VAR iPar AS DECIMAL NO-UNDO.
DEF VAR ostAcct AS DECIMAL NO-UNDO.
DEF VAR ostPar AS DECIMAL NO-UNDO.
DEF VAR mSummaPar AS DECIMAL NO-UNDO.
DEF VAR idd AS INT64 NO-UNDO.
DEF VAR idk AS INT64 NO-UNDO.
DEF VAR mRow AS ROWID NO-UNDO.


listAcctType = "КредРезПОВ,КредРезПрОВ,КредРезПени,КредРезП,КредРезПр,КредРез,КредРез1".
listLoanPar =  "540,541,357,350,351,21,46".


FUNCTION loan-ost RETURNS DECIMAL (
    INPUT vContract AS CHAR,
    INPUT vContCode AS CHAR,
    INPUT vSince AS DATE,
    INPUT vAcctType AS CHAR):
    
    DEF VAR vRes AS DEC NO-UNDO.
    vRes = 0.
    FOR EACH loan-acct
        WHERE loan-acct.contract EQ vContract
          AND loan-acct.cont-code EQ vContCode
          AND loan-acct.acct-type EQ vAcctType
          AND loan-acct.since <= vSince
        NO-LOCK:
      
      FIND FIRST acct OF loan-acct NO-LOCK.
      IF acct.close-date = ? THEN do:      
          RUN acct-pos IN h_base (loan-acct.acct,
                           loan-acct.currency,
                           vSince,
                           vSince,
                           "√"            /* = статус "крыж" */
                           ).
      
          vRes = sh-bal.  /* (IF acct.side EQ "А" THEN sh-bal ELSE (- sh-bal)). */
          /* message vRes ' ' loan-acct.acct ' ' loan-acct.acct-type view-as alert-box. */
       END.
    END. 
    RETURN vRes.
END FUNCTION.

FUNCTION fu-par RETURNS DECIMAL (
    BUFFER iloan FOR loan,
    INPUT vSince AS DATE,
    INPUT vParm AS INT):

    DEF VAR vAmt AS DEC NO-UNDO.
    DEF VAR vAmt2 AS DEC NO-UNDO.
    DEF VAR vAmt3 AS DEC NO-UNDO.
    DEF VAR vDbSumDec AS DEC NO-UNDO.
    DEF VAR vCrSumDec AS DEC NO-UNDO.
    DEF VAR vTmpCurr        AS CHAR   NO-UNDO.

    RUN STNDRT_PARAM IN h_loan (
	iloan.contract, iloan.cont-code, vParm,
	IF iloan.since >= vSince THEN vSince ELSE iloan.since,
                              OUTPUT vAmt, OUTPUT vDbSumDec, OUTPUT vCrSumDec).
    RUN inter_current(BUFFER iloan, vParm, OUTPUT vAmt2).
    vAmt = vAmt + vAmt2.

    IF iloan.since < vSince THEN DO:
	/* надо добавить операции за будущие дни */
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-d = vParm NO-LOCK:
	      vAmt = vAmt + loan-int.amt-rub.
	  END.
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-k = vParm NO-LOCK:
	      vAmt = vAmt - loan-int.amt-rub.
	  END.
    END.
    RETURN vAmt.
END.




FOR EACH tmprecid,
    FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    
    DO i = 1 TO NUM-ENTRIES(listAcctType):
        ostAcct = loan-ost(loan.contract,loan.cont-code,end-date,ENTRY(i,listAcctType)).
        ostPar = fu-par(buffer loan,end-date,int64(ENTRY(i,listLoanPar))).
        mSummaPar = 0.
        IF ostAcct > ostPar THEN DO:
            mSummaPar = ostAcct - ostPar.
            idd = int64(ENTRY(i,listLoanPar)).
            idk = 22.
        END.
        IF ostAcct < ostPar THEN DO:
            mSummaPar = ostPar - ostAcct.
            idk = int64(ENTRY(i,listLoanPar)).
            idd = 22.
            
        END.
        IF mSummaPar <> 0 THEN DO:
        RUN Cr_LoanIntSimple IN h_lv (loan.contract,             /* Идентификатор   */
                                    loan.cont-code,             /* договора        */
                                    end-date,            /* Дата операции   */ 
                                    mSummaPar,             /* Сумма операции  */
                                    idd, /* п. Начислено    */
                                    idk,                     /* п. Списано      */
                                    NO,                   /* Авт. операция   */
                                    ?,                     /* handle проводки */
                                    OUTPUT mRow).
        END.
    END.
    PUT UNFORMATTED loan.cont-code SKIP.
    
END.
{preview.i}

{intrface.del}
