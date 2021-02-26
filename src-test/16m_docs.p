/* fev
   Поиск движений по счетам для ежемесячных комиссий
   Используется транзакциями _16m*
*/

{globals.i}
{intrface.get blkob}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER i__Acct      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iKom-Acct    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iAcct474     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iAcct706     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iOpDate      AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER iTransaction AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE pick-value   AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
      


i = 0.
j = 0.

/*message i__Acct iKom-Acct iAcct474 iAcct706 iOpDate iTransaction view-as alert-box.*/

FOR EACH op WHERE op.doc-date  EQ iOpDate
              AND op.op-kind   EQ iTransaction
              AND op.filial-id EQ shFilial
              AND ((op.op-status NE "А") AND (op.op-status NE "АК"))
              /*AND op.doc-type  EQ "017"*/
              AND CAN-DO("*" + string(substr(i__Acct, 1, 20)) + "*", op.details)
    NO-LOCK:
             IF AVAIL op
             THEN DO:
                     IF op.doc-type EQ "09"
                     THEN DO:
                              FIND FIRST op-entry WHERE op-entry.op EQ op.op
                                                    AND op-entry.acct-db EQ iAcct474
                                                    AND op-entry.acct-cr EQ iAcct706
                                                    AND op-entry.op-date EQ op.op-date
                                   NO-LOCK NO-ERROR.
                                   IF AVAIL op-entry 
                                   THEN i = 1.
                                   ELSE i = 0.
                     END.
                     IF op.doc-type EQ "017"
                     THEN DO:
                              FIND FIRST op-entry WHERE op-entry.op EQ op.op
                                                    AND op-entry.acct-db EQ iKom-Acct
                                                    AND op-entry.acct-cr EQ iAcct474
                                                    AND op-entry.op-date EQ op.op-date
                                   NO-LOCK NO-ERROR.
                                   IF AVAIL op-entry 
                                   THEN j = 1.
                                   ELSE j = 0.
                     END.
             END.
             ELSE DO:
                      i = 0.
                      j = 0.
             END.
END.



IF i = 0 AND j = 0
THEN DO: 
         /*message "NOT AVAIL OP for" string(substr(i__Acct, 1, 20)) "(00)" view-as alert-box.*/
         pick-value = "00".
END.



IF i = 0 AND j = 1
THEN DO: 
         message "Для счёта" string(substr(i__Acct, 1, 20)) "отсутствует МО 47423 -> 70601!" view-as alert-box.
         pick-value = "11".
END.



IF i = 1 AND j = 0
THEN DO: 
         message "Для счёта" string(substr(i__Acct, 1, 20)) "отсутствует БО" string(substr(iKom-Acct, 1, 5)) "-> 47423!" view-as alert-box.
         pick-value = "11".
END.



IF i = 1 AND j = 1
THEN DO: 
         /*message "AVAIL OP for" string(substr(i__Acct, 1, 20)) "(11)" view-as alert-box.*/
         pick-value = "11".
END.



RETURN pick-value.