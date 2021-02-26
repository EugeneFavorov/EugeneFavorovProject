/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: a-deldps.fun
      Comment: TT:0235439 Миграция. Отказ клиента от оформления вклада
   Parameters:
         Uses:
      Used by: a-deldps.p
      Created: 12/02/2015 14:33 KMBIS Миграция. Отказ клиента от оформления вклада
                                      Вспомогательные функции
     Modified: 
*/
/*================================================================================================*/
/*=== Поиск связанного вклада ДВ =================================================================*/
FUNCTION FndDV RETURNS CHAR (INPUT iContr AS CHAR,
                             INPUT iCode  AS CHAR):

DEF VAR vRes  AS  CHAR  NO-UNDO.

DEF BUFFER bLoanAcct FOR loan-acct.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bLAcct    FOR loan-acct.
DEF BUFFER bAcctDel  FOR acct.

   vRes = "".
   FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ iContr
                        AND bLoanAcct.cont-code EQ iCode 
                      NO-LOCK,
      FIRST bAcct WHERE bAcct.acct      EQ bLoanAcct.acct
                    AND bAcct.currency  EQ bLoanAcct.currency
                  NO-LOCK
                  BREAK BY bAcct.acct.

      IF LAST-OF(bAcct.acct) THEN
      DO:
         FOR EACH bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct    
                            AND bLAcct.currency  EQ bLoanAcct.currency
                            AND bLAcct.contract  EQ bLoanAcct.contract
                            AND bLAcct.cont-code NE bLoanAcct.cont-code
                            AND bLAcct.acct-type EQ "loan-dps-p"
                          NO-LOCK:

            /*Пока делаем так, в тз не расписано как искать связь с договором ДВ */
            {additem.i vRes bLAcct.cont-code}

         END. /* FOR FIRST bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct     */
      END. /* IF LAST-OF(bAcct.acct) THEN */
   END. /* FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract */

   RETURN vRes.

END FUNCTION. /* FndDV */

/*================================================================================================*/

/*================================================================================================*/
/*=== Поиск не аннулированных документов =========================================================*/
FUNCTION FndOp RETURNS LOG (INPUT iContr AS CHAR,
                            INPUT iCode  AS CHAR):

DEF VAR vFind  AS  LOG  NO-UNDO INIT NO.

DEF BUFFER bLoan      FOR loan.
DEF BUFFER bKauEntry  FOR kau-entry.
DEF BUFFER bLoanAcct  FOR loan-acct.
DEF BUFFER bLAcct     FOR loan-acct.
DEF BUFFER bAcct      FOR acct.
DEF BUFFER bOp        FOR op.
DEF BUFFER bOpEntry   FOR op-entry.

   /* Ищем по кау, если есть то не разбираемся больше  */
   FOR FIRST bKauEntry WHERE bKauEntry.kau BEGINS SUBST("&1,&2,", 
                                                        iContr,
                                                        iCode)
                       NO-LOCK:
      /* По вкладу имеется субаналитика */
      vFind = YES.
   END.

   IF vFind EQ NO THEN
      FOR FIRST bLoan WHERE bLoan.contract  EQ iContr
                        AND bLoan.cont-code EQ iCode 
                      NO-LOCK,
        EACH bLoanAcct WHERE bLoanAcct.contract  EQ bLoan.contract 
                         AND bLoanAcct.cont-code EQ bLoan.cont-code
                       NO-LOCK,
         FIRST bAcct WHERE bAcct.acct      EQ bLoanAcct.acct
                       AND bAcct.currency  EQ bLoanAcct.currency
                       AND bAcct.cust-cat  EQ bLoan.cust-cat
                       AND bAcct.cust-id   EQ bLoan.cust-id
                     NO-LOCK
                     BREAK BY bAcct.acct:
     
         IF FIRST-OF(bAcct.acct) THEN
         DO:
            FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ bAcct.acct
                                 OR bOpEntry.acct-cr EQ bAcct.acct
                              NO-LOCK,
               FIRST bOp WHERE bOp.op      EQ bOpEntry.op
                           AND bOp.op-date GE bLoan.open-date
                           AND bOp.op-date NE ?
                         NO-LOCK:
               /* Нашли не аннулированный документ */
           
               /* Ищем привязки счета к другому договору */
               FIND FIRST bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct 
                                   AND bLAcct.currency  EQ bLoanAcct.currency
                                   AND bLAcct.cont-code NE bLoan.cont-code
                                 NO-LOCK NO-ERROR.

               IF NOT AVAIL(bLAcct) THEN
               DO:
                  /* На дату документа, счет был привязан только к нашему договору */
                  vFind = YES.
               END. /* IF NOT AVAIL(bLAcct) THEN */

            END. /* FOR FIRST op-entry WHERE op-entry.acct-db EQ acct.acct */
         END. /* IF FIRST-OF(bAcct.acct) THEN */
      END. /* FOR EACH loan-acct WHERE loan-acct.contract  EQ loan.contract */

   RETURN vFind.

END FUNCTION. /* FndOp */

/*================================================================================================*/
