/* ayv
   Поиск счетов для ежемесячных комиссий
   Используется транзакциями _16m*
*/

{globals.i}
{intrface.get blkob}
{intrface.get instrum}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iAcct    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iOpDate  AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER iSumm    AS DECIMAL   NO-UNDO.

DEFINE SHARED VARIABLE pick-value AS CHARACTER NO-UNDO.

DEFINE VARIABLE newAcct       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE newAcct2      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE acctBal       AS DECIMAL      NO-UNDO INIT 0.
DEFINE VARIABLE acctTmp       AS CHARACTER    NO-UNDO INIT ''.
DEFINE VARIABLE acctCustId    AS INT          NO-UNDO INIT 0.
DEFINE VARIABLE acctCustCat   AS CHARACTER    NO-UNDO INIT ''.
DEFINE VARIABLE iAcctContract AS CHARACTER.
DEFINE VARIABLE iAcctCurrency AS CHARACTER.

DEFINE VARIABLE mBlSumm  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mListBlk AS CHARACTER NO-UNDO.

DEF BUFFER acct1 FOR acct.
DEF BUFFER acct2 FOR acct.

/*MESSAGE "INPUT" string(iAcct) view-as alert-box.*/

newAcct =  "0".
newAcct2 = "0".

FIND FIRST acct WHERE acct.acct EQ iAcct NO-LOCK NO-ERROR.

IF AVAIL (acct) AND acct.acct NE "40702978805170010001     @0500"   /* ООО "Азиатское Торговое Агентство" */
                AND acct.acct NE "40702840805170010003     @0500"   /* ООО "Азиатское Торговое Агентство" */
                AND acct.acct NE "40702840005170010007     @0500"   /* ООО "ИнтерТранс" */
THEN DO:
         /*MESSAGE "810 THEN" view-as alert-box.*/

         iAcctContract = acct.contract.
         iAcctCurrency = acct.currency.
         ASSIGN
                acctCustId  = acct.cust-id
                acctCustCat = acct.cust-cat
              .
                IF     acct.currency NE '' 
                   AND acct.contract EQ 'Текущ'
                THEN DO:
                         RUN acct-pos IN h_base (iAcct, acct.currency, iOpDate, iOpDate, ?).
                         /*acctBal  = ABS(sh-bal).*/
                         acctBal  = ABS(sh-val) * FindRate("УЧЕТНЫЙ", acct.currency, iOpDate).

                         mBlSumm  = GetBlockPosition(acct.acct, acct.currency, "*", iOpDate).
                         mBlSumm  = ABS(mBlSumm).
                         mListBlk = BlockAcct(acct.acct + "," + acct.currency, DATETIME(DATE(iOpDate) + 1) - 1).
      
                         IF     LOOKUP("Блок", mListBlk)   EQ 0
                            AND LOOKUP("БлокДб", mListBlk) EQ 0
                            AND ((acctBal - mBlSumm)       GE iSumm)
                            AND acct.filial-id             EQ shFilial
                         THEN newAcct = acct.acct.
                         ELSE DO:
                                  FOR EACH acct1 WHERE acct1.cust-cat   EQ acctCustCat
                                                   AND acct1.cust-id    EQ acctCustId
                                                   AND acct1.close-date EQ ?
                                                   AND acct1.currency   EQ ''
                                                   AND acct1.contract   EQ 'Расчет'
                                                   AND acct1.filial-id  EQ shFilial
                                      NO-LOCK:
                                      IF AVAIL (acct1) AND GetXAttrValue("acct", acct1.acct + "," + acct1.currency, "КорпКарт") <> 'Да'
                                      THEN newAcct = acct1.acct.
                                      ELSE NEXT.
                                  END.
                              END.
                     END.



      IF acct.currency EQ '' AND acct.contract EQ 'ДУ'
      THEN newAcct = acct.acct.



      IF acct.currency EQ '' AND CAN-DO('СпецПА,СпБрок,Кон138,КонЗдт,КонРез,Текущ,Расчет', acct.contract)
      THEN DO:
               FOR EACH acct WHERE acct.cust-cat   EQ acctCustCat
                               AND acct.cust-id    EQ acctCustId
                               AND acct.close-date EQ ?
                               AND acct.currency   EQ ''
                               AND acct.contract   EQ 'Расчет'
                               AND acct.filial-id  EQ shFilial
                   NO-LOCK:
                            IF AVAIL (acct) AND GetXAttrValue("acct", acct.acct + "," + acct.currency, "КорпКарт") <> 'Да'
                            THEN DO:
                                     IF (acct.contract EQ iAcctContract) AND (acct.currency EQ iAcctCurrency) AND (GetXAttrValue("acct", iAcct + "," + iAcctCurrency, "КорпКарт") <> 'Да')
                                     THEN NewAcct = iAcct.
                                     ELSE NewAcct = acct.acct.
                                 END.
                            ELSE NEXT.
               END.
           END.

     END.
ELSE DO:
         /*MESSAGE "!810 ELSE" view-as alert-box.*/

         iAcctContract = acct.contract.
         iAcctCurrency = acct.currency.
         acctCustId    = acct.cust-id.
         acctCustCat   = acct.cust-cat.

         FOR EACH acct WHERE acct.cust-cat   EQ acctCustCat
                         AND acct.cust-id    EQ acctCustId
                         AND acct.close-date EQ ?
                         AND acct.currency   EQ ''
                         AND acct.contract   EQ 'Расчет'
                         AND acct.filial-id  EQ shFilial
             NO-LOCK:

             IF AVAIL (acct) AND GetXAttrValue("acct", acct.acct + "," + acct.currency, "КорпКарт") <> 'Да'
             THEN DO:
                      /*MESSAGE "VAL -> RUB" string(acct.acct) view-as alert-box.*/

                      newAcct2 = acct.acct.
                      RUN acct-pos IN h_base (acct.acct, acct.currency, iOpDate, iOpDate, ?).
                      acctBal  = ABS(sh-bal).

                      mBlSumm  = GetBlockPosition(acct.acct, acct.currency, "*", iOpDate).
                      mBlSumm  = ABS(mBlSumm).
                      mListBlk = BlockAcct(acct.acct + "," + acct.currency, DATETIME(DATE(iOpDate) + 1) - 1).
      
                      IF     LOOKUP("Блок", mListBlk)   EQ 0
                         AND LOOKUP("БлокДб", mListBlk) EQ 0
                         AND ((acctBal - mBlSumm)       GE iSumm)
                         AND acct.filial-id             EQ shFilial
                      THEN DO:
                               newAcct = acct.acct.
                               /*MESSAGE "RUB OK" string(newAcct) view-as alert-box.*/
                           END.  
                      ELSE DO:
                               FIND FIRST acct2 WHERE acct2.acct EQ iAcct NO-LOCK NO-ERROR.

                               RUN acct-pos IN h_base (acct2.acct, acct2.currency, iOpDate, iOpDate, ?).
                               acctBal  = ABS(sh-val) * FindRate("УЧЕТНЫЙ", acct2.currency, iOpDate).

                               mBlSumm  = GetBlockPosition(acct2.acct, acct2.currency, "*", iOpDate).
                               mBlSumm  = ABS(mBlSumm).            	            
                               mListBlk = BlockAcct(acct2.acct + "," + acct2.currency, DATETIME(DATE(iOpDate) + 1) - 1).
      
                               IF     LOOKUP("Блок", mListBlk)   EQ 0
                                  AND LOOKUP("БлокДб", mListBlk) EQ 0
                                  AND ((acctBal - mBlSumm)       GE iSumm)
                                  AND acct2.filial-id            EQ shFilial
                               THEN DO:
                                        newAcct = acct2.acct.
                                        /*MESSAGE "VAL OK.   RUB -> VAL" string(newAcct) string(acctBal) view-as alert-box.*/
                                    END.  
                               ELSE DO:
                                        newAcct = newAcct2.
                                        /*MESSAGE "VAL BAD.   VAL -> RUB" string(newAcct) view-as alert-box.*/
                                    END. 
                           END.
                  END.
             ELSE NEXT.
         END.
     END.

pick-value = newAcct.

/*MESSAGE "OUTPUT" string(pick-value) view-as alert-box.*/

RETURN pick-value.