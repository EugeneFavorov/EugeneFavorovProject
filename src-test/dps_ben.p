/*
    Copyright: 
     Filename: 
      Comment: Процедура "ВыпПосле".
   Parameters:
      Created: kam
     Modified:
*/

{globals.i}
{intrface.get xclass} 

{loan_sn.i} /* Процедуры разбора private-data по модулям:
            ** - "Кредиты и Депозиты",
            ** - "Частные вклады" */

{dpsproc.def} 
/* {pp-corr.p} */
{intrface.get tmess}
{get_date.i}


DEF INPUT  PARAMETER in-sss AS RECID   NO-UNDO.
DEF INPUT  PARAMETER in-rid  AS RECID   NO-UNDO.
DEF OUTPUT PARAMETER oResult AS INTEGER NO-UNDO.

def var h_templ     AS HANDLE  NO-UNDO.

def var strBic      AS CHAR no-undo.
def var strCorrAcct AS CHAR no-undo.
def var strBenKlient AS CHAR no-undo.
def var strBenAcct  AS CHAR no-undo.

main:
do:  
    find first op-entry where recid(op-entry) = in-rid no-lock no-error.
    if not avail op-entry then leave main.

    /* ищем вклад */
    RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
    IF NOT VALID-HANDLE(h_templ) THEN RETURN.
    FIND FIRST loan WHERE loan.contract  EQ "dps"
        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
    IF NOT AVAIL loan THEN RETURN.

    strBic      = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"vmfo","").
    strCorrAcct = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"corr-acct","").
    strBenKlient = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"ben-klient","").
    strBenAcct  = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"ben-acct","").
                               
    IF strCorrAcct <> '' AND strBic <> '' THEN DO:                      
       IF strBic EQ "045209783" THEN DO:
           strBic = "045209884".
           strCorrAcct = "30101810152090000884".
       END.                             
        find first op of op-entry exclusive-lock no-error.
        if avail op then do:
            create op-bank.
            assign
                op-bank.op             = op.op
                op-bank.op-bank-type   = ""
                op-bank.bank-code-type = "МФО-9"
                op-bank.bank-code      = strBic
                op-bank.corr-acct      = strCorrAcct
                op.ben-acct = strBenAcct  
                op.name-ben = strBenKlient.
        end.
    END.
    
end.  /* end main */
{intrface.del}      
