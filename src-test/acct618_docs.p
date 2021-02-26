/* fev
   Поиск движений по счетам для ежемесячных комиссий
   Используется транзакциями _16m*
*/

{globals.i}
{intrface.get blkob}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER i__Acct      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iAcct706     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iOpDate      AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER iTransaction AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE pick-value   AS CHARACTER NO-UNDO.

DEFINE VARIABLE j AS INTEGER NO-UNDO.
      


j = 1.

FOR EACH op-entry WHERE op-entry.acct-db EQ i__Acct
                    AND op-entry.acct-cr EQ iAcct706
                    AND op-entry.op-date EQ iOpDate
    NO-LOCK:
             IF AVAIL op-entry
             THEN DO:
                      /*message i__Acct "AVAIL OP" view-as alert-box.*/
                      FIND FIRST op WHERE op.op        EQ op-entry.op
                                      AND op.doc-date  EQ op-entry.op-date
                                      AND op.op-kind   EQ iTransaction
                                      AND op.filial-id EQ shFilial
                                      AND op.op-status EQ chr(251)
                                      AND op.doc-type  EQ "017"
                           NO-LOCK NO-ERROR.
                           IF AVAIL op
                           THEN DO: 
                                    /*message i__Acct "AVAIL OP-ENTRY" view-as alert-box.*/ 
                                    j = 0.
                                END.
             END.
             ELSE j = 1.
END.

/*
FOR EACH op WHERE op.doc-date  EQ iOpDate
              AND op.op-kind   EQ iTransaction
              AND op.filial-id EQ shFilial
              AND op.op-status EQ chr(251)
              AND op.doc-type  EQ "017"
            /*AND CAN-DO("*" + string(substr(i__Acct, 1, 20)) + "*", op.details)*/
    NO-LOCK:
             IF AVAIL op
             THEN DO:
                      /*message i__Acct "AVAIL OP" view-as alert-box.*/
                      FIND FIRST op-entry WHERE op-entry.op EQ op.op
                                            AND op-entry.acct-db EQ i__Acct
                                            AND op-entry.acct-cr EQ iAcct706
                                            AND op-entry.op-date EQ op.op-date
                           NO-LOCK NO-ERROR.
                           IF AVAIL op-entry
                           THEN DO: 
                                    /*message i__Acct "AVAIL OP-ENTRY" view-as alert-box.*/ 
                                    j = 0.
                                END.
             END.
             ELSE j = 1.
END.
*/

/*message i__Acct iAcct706 iOpDate iTransaction j view-as alert-box.*/

IF j = 0 THEN pick-value = "0".

IF j = 1 THEN pick-value = "1".

RETURN pick-value.