/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2020 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters: 
         Uses: -
      Used by: 
      Created: 
     
*/

{globals.i}
{intrface.get instrum} /* Инструменты для курсов и валют
{intrface.get xclass} */

{g-defs.i}
{crdps.def}
{chkacces.i}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get lnbh}
{intrface.get cust}
{intrface.get brnch}
{intrface.get date}
{intrface.get tmcod}
{intrface.get db2l}   
{intrface.get dps}
{intrface.get trust}
{intrface.get trans}
{intrface.get pbase}
{form.def}

DEFINE INPUT PARAMETER iTypeVal AS CHARACTER.
DEFINE INPUT PARAMETER iCur_db AS CHARACTER.
DEFINE INPUT PARAMETER iCur_cr AS CHARACTER.
DEFINE INPUT PARAMETER iBr AS CHARACTER.
DEFINE INPUT PARAMETER iNom AS CHARACTER.  /*номер процедуры*/
DEFINE INPUT PARAMETER iDate AS CHARACTER.  /*номер процедуры*/
DEFINE VARIABLE vVal       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRateType  AS CHARACTER NO-UNDO.    

DEFINE VARIABLE vCrossRate AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vRateDate  AS DATE      NO-UNDO.
DEFINE VARIABLE vRateTime  AS INT64     NO-UNDO.
DEFINE VARIABLE vPersID  AS INT64     NO-UNDO.
DEFINE VARIABLE vDateD AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNomD AS CHARACTER NO-UNDO.
      
DEFINE VARIABLE i  AS INT64     NO-UNDO.      
DEFINE VARIABLE vFilterFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFilterVal AS CHARACTER NO-UNDO.
 
CASE iNom:
    WHEN "1" THEN 
        DO:   
            RUN CrossRateTimeR(iTypeVal,
                iCur_cr, /* iInCurr */
                iCur_db, /* iOutCurr */
                iBr ,
                TODAY,
                TIME,
                DEC(0),
                OUTPUT vCrossRate,
                OUTPUT vRateDate,
                OUTPUT vRateTime).   
            vVal = STRING(vCrossRate).  
        END.
    WHEN "2"  THEN 
        DO:          
            DEF BUFFER b-loan FOR loan.         
                   
            FOR EACH loan-acct WHERE loan-acct.acct  EQ iTypeVal
                AND loan-acct.currency EQ iCur_db
                AND CAN-DO("loan-dps-p,loan-dps-t", loan-acct.acct-type)   NO-LOCK,
                EACH loan WHERE loan.contract  EQ loan-acct.contract
                AND loan.cont-code EQ loan-acct.cont-code
                AND loan.close-date EQ ? 
                AND CAN-DO("Ч", loan.cust-cat)              
                NO-LOCK:
                  IF (NOT AVAILABLE loan) THEN   
                    RETURN.   
                    
                      /*MESSAGE loan-acct.currency + '/' + iTypeVal + '|' + iCur_db + '|' + iCur_cr + '|' + iBr + '|'  + iNom + '|' iDate VIEW-AS ALERT-BOX.*/
                i = 0.
                FOR EACH  b-loan 
                    WHERE b-loan.cust-id EQ loan.cust-id
                    AND b-loan.class-code BEGINS 'proxy' 
                    AND b-loan.close-date EQ ?
                    AND b-loan.open-date LE date(iDate)  NO-LOCK :                          
                        vNomD = STRING(b-loan.doc-num).  
                        i  =  i + 1.    
                END. 
               If i eq 0 THEN  RETURN.   
            vFilterFld = "cust-id" + CHR(1)  + 'loan-allowed' + CHR(1) + 'close-date2'+ CHR(1) + 'open-date2'.
            vFilterVal = STRING(loan.cust-id) + CHR(1) + string(loan.cont-code) + CHR(1) +  "?"+ CHR(1) + iDate .                                 
            pick-value = "".
            RUN browseld.p ("proxy", vFilterFld,vFilterVal,?,7).
                   
            IF (LASTKEY = 10 OR LASTKEY = 13) AND pick-value <> ? THEN
            DO:
                IF i GT 1 THEN 
                    Do:
                        FIND FIRST b-loan 
                            WHERE b-loan.cont-code EQ ENTRY(2,pick-value ,",")
                                AND b-loan.class-code BEGINS 'proxy'       NO-LOCK NO-ERROR.  
                        vNomD = STRING(b-loan.doc-num).                  
                    END.
                vPersID = INT64(GetXAttrValue("loan", pick-value, "agent-id")).                                      
                vDateD = STRING(GetXAttrValue("loan", pick-value, "create-date")). 
                FIND FIRST person WHERE person.person-id = vPersID               NO-LOCK NO-ERROR.
                pick-value = ENTRY(2,pick-value ,",").
                IF NOT AVAILABLE person THEN 
                DO:
                    MESSAGE "В системе не найден клиент по доверенности." VIEW-AS ALERT-BOX.
                    RETURN NO-APPLY.
                END.
                vVal = STRING(person.person-id) + ',' +  pick-value + ',' + vNomD + ',' +  vDateD.                  
            END. 
            else  vVal = "-1". 
            
        END.  
END. 
    WHEN "3" THEN                   
                DO:                          
                    FIND LAST instr-rate
                    WHERE instr-rate.instr-cat = "currency"
                    AND instr-rate.rate-type = "Специальный"
                    AND instr-rate.instr-code = iCur_cr
                    AND instr-rate.since = today
                    NO-LOCK NO-ERROR.                        
                        IF AVAIL instr-rate THEN
                           
                                /* запомним в переменную */                              
                                vVal = STRING(instr-rate.rate-instr).
                                /* после чего удалим */
                                /*DELETE instr-rate.*/
                           
                         ELSE    
                            vVal = '0'.
                        /**/
                    /**/
                END.  
   WHEN "4" THEN                   
                DO:                          
                    FIND LAST instr-rate
                    WHERE instr-rate.instr-cat = "currency"
                    AND instr-rate.rate-type = "Специальный"
                    AND instr-rate.instr-code = iCur_cr
                    AND instr-rate.since = today
                    EXCLUSIVE-LOCK NO-ERROR.
           DELETE instr-rate.           
           vVal = 'Спецкурс удален'.
                END.    
                
                  
END CASE. 

RETURN vVal.