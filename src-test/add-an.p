/*
Банковская интегрированная система БИСквит
Copyright: (C) 1992-2017 ТОО "Банковские информационные системы"
*/

DEFINE INPUT  PARAMETER iOpOp  AS INT64     NO-UNDO.
DEFINE OUTPUT PARAMETER oDprID AS CHARACTER NO-UNDO.

{globals.i}
{intrface.get xclass}
{intrface.get sessions}
{intrface.get tparam}   /* Библиотека передачи информации м/у программами. */
{intrface.get vok}      /* Инструменты для работы с обьектами ВОК pp-vok.p */

DEFINE VARIABLE mDprID        AS INT64     NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL   NO-UNDO.

DEFINE BUFFER op       FOR op.
DEFINE BUFFER op-entry FOR op-entry.

FIND FIRST op WHERE op.op = iOpOp NO-LOCK NO-ERROR.

FOR EACH op-entry OF op WHERE TRUE
   AND CAN-DO("40817....0599*,40820....0599*",op-entry.acct-cr)
   NO-LOCK:
      
   /*Установка dpr-id*/
   FIND FIRST sessions WHERE TRUE 
      AND sessions.op-date   EQ TODAY
      AND sessions.user-id   EQ op.user-inspector
      AND sessions.dpr-close EQ ?
   NO-LOCK NO-ERROR.
   IF AVAIL(sessions) THEN 
   DO:
      oDprID = STRING(sessions.dpr-id).
      mOk = UpdateSigns("op",STRING(op.op),"dpr-id",oDprID,YES).
      
      FIND FIRST kau-entry WHERE TRUE
         AND kau-entry.op       = op-entry.op
         AND kau-entry.op-entry = op-entry.op-entry
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(kau-entry) THEN
      DO: 
         CREATE kau-entry.
         ASSIGN
            kau-entry.acct-cat   = "b"
            kau-entry.op         = op-entry.op
            kau-entry.op-entry   = op-entry.op-entry
            kau-entry.kau-entry  = op-entry.op-entry
            kau-entry.op-date    = op-entry.op-date
            kau-entry.currency   = op-entry.currency
            kau-entry.amt-cur    = op-entry.amt-cur
            kau-entry.amt-rub    = op-entry.amt-rub
            kau-entry.user-id    = op-entry.user-id
            kau-entry.op-code    = op-entry.op-cod
            kau-entry.op-status  = op-entry.op-status
            kau-entry.value-date = op-entry.value-date
            kau-entry.debit      = YES
            kau-entry.qty        = 0
            kau-entry.acct       = op-entry.acct-db
            kau-entry.kau        = oDprID
            kau-entry.kau-id     = "КодСменыВОК".
         
         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
            " kau-entry.kau = " + kau-entry.kau).
      END.
   END.
END.

{intrface.del}

RETURN oDprID.
