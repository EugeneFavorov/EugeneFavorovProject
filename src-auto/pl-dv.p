/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: pl-dv.p
      Comment: Новый метод контроля довложения.
   Parameters: id и дата
         Uses:
      Used BY:
      Created: 31.05.2016 Sami 0278895
     Modified:      
*/

{globals.i}
{dpsproc.def}
{intrface.get date}
 
DEFINE INPUT  PARAM iRec         AS RECID NO-UNDO.
DEFINE INPUT  PARAM in-op-date   AS DATE  NO-UNDO.
DEFINE INPUT  PARAM iDn-cont-date AS DATE  NO-UNDO.
DEFINE OUTPUT PARAM oMess        AS CHAR  NO-UNDO INIT "".

DEF VAR mLim-date AS DATE NO-UNDO.     /*дата последнего довнесения*/
DEF VAR mSumm AS DEC NO-UNDO. 
def var fl as INT64 no-undo .

DEF BUFFER op-entry FOR op-entry.
DEF BUFFER bop-entry FOR op-entry.
DEF BUFFER bloan-acct FOR loan-acct.
  
FIND FIRST loan WHERE RECID(loan) = iRec NO-LOCK NO-ERROR.
/*IF NOT AVAILABLE loan THEN DO:
   oMess = "Вклад не найден".
   RETURN.
END.*/  
  
RUN end_doloan_dps in h_dpspc (iRec,
                              iDn-cont-date,
                              OUTPUT mLim-date,
                              OUTPUT oMess).
                              
IF iDn-cont-date GE mLim-date THEN
DO:
 

   /* Ищем счет вклада */
   FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
          AND loan-acct.cont-code EQ loan.cont-code
          AND loan-acct.acct-type EQ "loan-dps-t"
          AND loan-acct.since     LE iDn-cont-date NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
   DO:

      /* Ищем нашу проводку */
      FIND LAST bop-entry WHERE bop-entry.filial-id EQ shfilial
         AND bop-entry.acct-cr EQ loan-acct.acct
         AND bop-entry.op-date EQ iDn-cont-date NO-LOCK NO-ERROR.
  
      FIND FIRST acct WHERE acct.acct EQ bop-entry.acct-db NO-LOCK NO-ERROR.  
      
      /*IF NOT CAN-DO("4*",bop-entry.acct-db) OR loan.cust-id NE acct.cust-id
      THEN DO:  
         oMess =  "C " + STRING(mLim-date) + " пополнение только в безналично в сумме, не более начисленных процентов за предыдущий месяц.".
         RETURN.  
      END.*/
      
      /*    Проверяются проводки в течение текущего месяца */
      FIND FIRST op-entry WHERE op-entry.filial-id EQ shfilial
            AND op-entry.acct-cr EQ loan-acct.acct
            AND op-entry.op-status GE gop-status
            AND op-entry.acct-db BEGINS("4")
            AND op-entry.op-date GE FirstMonDate(iDn-cont-date)
            AND op-entry.op-date LE LastMonDate(iDn-cont-date)
            AND op-entry.op NE bop-entry.op NO-LOCK NO-ERROR.      
      IF AVAIL op-entry THEN
      DO:
        /* oMess =  "В данном месяце уже были довложения на счет договора.".
         RETURN.*/   
      END.
      ELSE DO:
         /* с ролью loan-dps-int */
         FIND LAST bloan-acct WHERE bloan-acct.contract  EQ loan.contract
            AND bloan-acct.cont-code EQ loan.cont-code
            AND bloan-acct.acct-type EQ "loan-dps-int" NO-LOCK NO-ERROR.
         IF AVAIL bloan-acct THEN
         DO:
            /* ищем проводки или обороты за текущий месяц */
            FOR EACH op-entry WHERE op-entry.filial-id EQ shfilial
                  AND op-entry.acct-cr EQ bloan-acct.acct
                  AND op-entry.op-status GE gop-status
                  AND op-entry.acct-db BEGINS("70606")
                  AND op-entry.op-date GE 
                    (IF iDn-cont-date EQ LastMonDate(iDn-cont-date)
                        THEN FirstMonDate(iDn-cont-date)
                        ELSE FirstMonDate(GoMonth(iDn-cont-date,-1)))
                  AND op-entry.op-date LE 
                    (IF iDn-cont-date EQ LastMonDate(iDn-cont-date)
                        THEN LastMonDate(iDn-cont-date)
                        ELSE  LastMonDate(GoMonth(iDn-cont-date,-1)))
            NO-LOCK:     
               
               mSumm = mSumm + op-entry.amt-rub.
         
            END.
            IF bop-entry.amt-rub > mSumm THEN
            DO:
              /* oMess =  "Сумма проводки больше разрешенной " + STRING(mSumm).
               RETURN.*/       
            END.            
         END.
      END.          
   END. /*IF AVAIL loan-acct THEN*/
   ELSE DO:
     /* oMess =  "C " + STRING(mLim-date) + " пополнение только в безналично в сумме, не более начисленных процентов за предыдущий месяц.".
      RETURN.*/
   END.
   END.
  IF iDn-cont-date LE mLim-date THEN 
   DO:
   FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
             AND loan-acct.cont-code EQ loan.cont-code
                       AND loan-acct.acct-type EQ "loan-dps-t"
                                 AND loan-acct.since     LE iDn-cont-date NO-LOCK NO-ERROR.
                                    IF AVAIL loan-acct THEN
DO:
    FIND LAST bop-entry WHERE bop-entry.filial-id EQ shfilial
    AND bop-entry.acct-cr EQ loan-acct.acct
    AND bop-entry.op-date EQ iDn-cont-date NO-LOCK NO-ERROR.
     FIND FIRST acct WHERE acct.acct EQ bop-entry.acct-db NO-LOCK NO-ERROR.          
    IF loan.cust-id NE acct.cust-id THEN   
DO:
RUN Get_Min_Summ IN h_dpspc
(recid(loan),
in-op-date,
in-op-date,
output fl).
END.
END.
END.
/*IF fl > (if acct.currency > '' then bop-entry.amt-cur else bop-entry.amt-rub) THEN DO:
oMess =  "Превышена минимальная сумма".
RETURN.

END.*/



{intrface.del}
/* $LINTFILE='pl-dv.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='miam' */
/* $LINTDATE='28/07/2016 10:29:42.906+03:00' */
/*prosignhnsH6/wk5v9WDrJhJJi+Xw*/