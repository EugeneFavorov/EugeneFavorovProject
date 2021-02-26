{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

{sh-defs.i}

DEFINE INPUT  PARAMETER iOpOp AS INT64   NO-UNDO.
DEFINE OUTPUT PARAMETER iOk   AS LOGICAL NO-UNDO.

DEFINE VARIABLE mCustID AS INT64   NO-UNDO.

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","Begin").

FOR EACH op WHERE TRUE
   AND op.op EQ iOpOp
   NO-LOCK,
   EACH op-entry OF op
   NO-LOCK:
   /**/
   FIND FIRST acct WHERE TRUE
      AND acct.acct     EQ op-entry.acct-db
      AND acct.bal-acct EQ 40817
      AND acct.cust-cat EQ "Ч"
   NO-LOCK NO-ERROR.
   IF AVAIL(acct) THEN 
   DO:
      mCustID = acct.cust-id.
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","acct-db mCustID = " + STRING(mCustID)).
      RUN ChkOver(mCustID).
   END.
   /**/
   FIND FIRST acct WHERE TRUE
      AND acct.acct     EQ op-entry.acct-cr
      AND acct.bal-acct EQ 40817
      AND acct.cust-cat EQ "Ч"
   NO-LOCK NO-ERROR.
   IF AVAIL(acct) THEN 
   DO:
      mCustID = acct.cust-id.
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","acct-cr mCustID = " + STRING(mCustID)).
      RUN ChkOver(mCustID).
   END.
END. 

iOk = YES.

{intrface.del}   

PROCEDURE ChkOver:
   DEFINE INPUT PARAMETER iCustID AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAnswer AS LOGICAL NO-UNDO.
      
   FOR EACH code WHERE TRUE
      AND code.class  EQ "Over-Mir"
      AND code.parent EQ "Over-Mir"
      AND code.val    EQ STRING(iCustID)
      AND code.description[2] NE ""
      AND code.description[3] NE ""
      NO-LOCK:
      LEAVE.
   END.
   
   IF AVAIL(code)
   AND code.misc[1] EQ "NO" THEN
   DO:
      MESSAGE
         "Клиенту: " + TRIM(code.name) + ", номер: " + TRIM(code.val) + " CID: " + TRIM(code.code) +
         "~nнеобходимо подписать распоряжение на списание поступлений" +
         "~n с карты Мир в счет гашения овердрафта, номер договора: " + code.description[1] + "." +
         "~nРаспечатайте Распоряжение в Payment 10 и подпишите у клиента."
         "~nПосле это запустите транзакцию r90909."
      VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.
