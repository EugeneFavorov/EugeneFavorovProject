/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: 
      Comment: меняем роль счета

   Parameters:
      Used by:
      Created: kam 06/07/2016
     Modified: 
*/

{globals.i}
{tmprecid.def}

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
def buffer bloan-acct for loan-acct.




FOR EACH tmprecid,
    FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    find first loan-acct of loan where loan-acct.acct-type = "КредРезОВ" no-error.
    if avail loan-acct then do:
        find first bloan-acct where bloan-acct.contract = loan.contract 
            and bloan-acct.cont-code = loan.cont-code
            and bloan-acct.acct-type = "КредРез".
            if avail bloan-acct then do:
                find first acct where acct.acct = bloan-acct.acct no-lock no-error.
                if avail acct and acct.close-date <> ? then do:
                    delete bloan-acct.       
                    loan-acct.acct-type = "КредРез".
                end.
                else do:
                    put unformatted loan.cont-code + bloan-acct.acct + 'error' skip.
                end.
            end.
    end.    
    PUT UNFORMATTED loan.cont-code SKIP.
    
END.
{preview.i}

{intrface.del}
