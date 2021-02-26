/*{form.def}
{globals.i}
{exchange.equ}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get pbase}
{intrface.get trans}
{intrface.get exch}
{intrface.get epack}
{intrface.get pack}


DEFINE INPUT  PARAM iRec AS RECID NO-UNDO.
DEF VAR mPrim AS CHARACTER NO-UNDO.
        mPrim = "00-000-000".
         DEF FRAME prim_frame
            mPrim VIEW-AS FILL-IN SIZE 35 BY 1
               NO-LABEL    
               HELP  "Промо-код"
               WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 DOWN WIDTH 37
               TITLE COLOR BRIGHT-WHITE "[ Промо-код ]".
         
         DISP mPrim WITH FRAME prim_frame.
         PAUSE 0.
         DO TRANSACTION ON ERROR  UNDO, LEAVE
                        ON ENDKEY UNDO, LEAVE WITH FRAME  prim_frame:
            UPDATE mPrim.
         END.
         HIDE FRAME prim_frame NO-PAUSE.
        /* UpdateSigns("op",STRING(op.op),"Примечание",mPrim, NO).*/
/*FIND FIRST loan WHERE RECID(loan) EQ iRec
   NO-LOCK NO-ERROR.
IF  AVAIL loan THEN 
DO:
        UpdateSigns('loan',loan.contract + "," + loan.cont-code,'promo',mPrim,no).
       
END.*/
MESSAGE iRec VIEW-AS ALERT-BOX.

 FIND FIRST loan WHERE RECID(loan) EQ iRec 
    NO-LOCK NO-ERROR.
 IF NOT AVAIL loan THEN 
 DO:
    MESSAGE "!!!!" VIEW-AS ALERT-BOX.

    RETURN.
 END.
{intrface.del}*/ 

/*FOR EACH loan-acct WHERE
loan-acct.contract  EQ bloan.contract 
loan.Cont-Type EQ 'ДВ'
and loan.contract EQ 'dps'
AND loan.loan-status EQ '√' 
    NO-LOCK,
first loan-acct


FOR EACH loan WHERE 
loan.Cont-Type EQ 'ДВ'
and loan.contract EQ 'dps'
AND loan.loan-status EQ '√' 
    NO-LOCK,
first loan-acct where
loan-acct.acct-type = 'loan-dps-t' */

{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get dps}
{intrface.get dpspr}

{setdest.i}
{sh-defs.i}
{dpsproc.def}
{getdates.i}
DEF BUFFER loan-acct1 for loan-acct.
DEF BUFFER loan1 for loan.
DEF VAR mSumm AS DECIMAL NO-UNDO.
DEF VAR mAcct AS CHARACTER NO-UNDO.
mAcct = '000'.
	for each tmprecid,
    				first loan where
        			recid(loan) eq tmprecid.id
    				no-lock,
  			each loan-acct of loan where
     			loan-acct.acct-type = 'loan-dps-p'
			or loan-acct.acct-type = 'loan-dps-t'
    			no-lock,
		each acct of loan-acct
		no-lock:
	
	for each loan-acct1 where loan-acct1.acct = acct.acct
	and loan-acct1.contract begins 'кред',
	first loan1 where loan1.cont-code = loan-acct1.cont-code
	no-lock:
	
	




 PUT UNFORMATTED loan.cont-code " , " string(loan.open-date) " , " string(loan.end-date) " , " string(loan.close-date) " , " loan1.cont-code " , " string(loan1.open-date) " , " string(loan1.end-date) " , " string(loan1.close-date) " , " loan1.loan-status " , "mAcct skip. 
END.
END.
PUT UNFORMATTED "1234" skip.
{preview.i}
{intrface.del}                 