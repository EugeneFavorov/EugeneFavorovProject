/*
Банковская интегрированная система БИСквит
Copyright: (C) 1992-2017 ТОО "Банковские информационные системы"
*/

DEFINE PARAMETER BUFFER op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT YES.

/*DEFINE VARIABLE oResult AS LOGICAL NO-UNDO.*/

{globals.i}

DEFINE VARIABLE mNumber       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMess         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnswer       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mHandle       AS INT64     NO-UNDO.
DEFINE VARIABLE iStat         AS INT64     NO-UNDO.
DEFINE VARIABLE mPCOverAcct   AS LOGICAL   NO-UNDO.

oResult = YES.

FOR EACH op-entry OF op WHERE TRUE
   AND CAN-DO("40817....0599*,40820....0599*",op-entry.acct-cr)
   NO-LOCK:
      
/*FOR EACH op-entry OF op WHERE TRUE*/
/*   /*AND op-entry.op EQ 59363329*/*/
/*   NO-LOCK:                       */

   mNumber = op-entry.acct-cr.

   FIND FIRST acct WHERE acct.acct EQ mNumber NO-LOCK NO-ERROR.
   IF AVAIL(acct) THEN
      mPCOverAcct = GetXattrValue("acct",acct.acct + "," + acct.currency,"pc_over_acct") = "Да".   
   
   RUN STORED-PROCEDURE GET_OVERDRAFT_ID mHandle = PROC-HANDLE
      (
      INPUT  PARAM P_ACC            = mNumber,
      INPUT  PARAM P_TYPE_ACC       = 2,
      INPUT  PARAM P_DATE           = TODAY,
      OUTPUT PARAM GET_OVERDRAFT_ID = ?
      ).
   CLOSE STORED-PROC GET_OVERDRAFT_ID iStat = PROC-STATUS.
   
   IF iStat = 0 THEN
   DO:
      IF GET_OVERDRAFT_ID NE ? 
      THEN RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "mNumber = " + mNumber + " iStat = " + STRING(iStat) + " GET_OVERDRAFT_ID = " + STRING(GET_OVERDRAFT_ID)).
      ELSE RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
      "mNumber = " + mNumber + " iStat = " + STRING(iStat) + " GET_OVERDRAFT_ID = ?").
      IF GET_OVERDRAFT_ID GE 1 THEN
      DO:
         oResult = NO.   
      END.
   END.
   ELSE
   DO:
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
      "~niStat = " + STRING(iStat)).
      oResult = YES.
   END.

	IF oResult EQ NO THEN LEAVE.
END.

{intrface.del}

RETURN.
