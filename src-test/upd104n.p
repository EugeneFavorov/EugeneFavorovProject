{globals.i}

DEFINE BUFFER op FOR op.
DEFINE BUFFER op1 FOR op.

FOR EACH op 
   WHERE 
/*   op.op EQ 26029061  */
   op.op-date GE TODAY - 10
   AND op.op-date LE TODAY 
   AND op.op-kind EQ "i-ed104n"
   AND TRIM(op.user-inspector) EQ ""
   NO-LOCK:
      FIND FIRST op1
         WHERE RECID(op1) EQ RECID(op)
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(op1) THEN
      DO:
         op1.user-inspector = "K0400MVS".
/*         PUT UNFORMATTED
         op1.op-date
         " "
         op1.doc-num
         SKIP. */
         FIND CURRENT op1 NO-LOCK NO-ERROR.
      END.
END.

