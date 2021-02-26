{globals.i}
Define Input Param rid As RecID No-Undo.
FIND FIRST op WHERE RECID(op) EQ RID NO-LOCK NO-ERROR.
IF AVAIL(op) THEN DO:
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF AVAIL(op-entry) THEN DO:
      IF CAN-DO(FGetSetting("Œ®‘çŒ áª","",""),op-entry.acct-cr) THEN DO:
         RUN memorn.p(INPUT rid).
      END.
      ELSE DO:
         IF op.op-date ge date("31.03.14") OR 
            (op.op-date EQ ? AND op.doc-date GE DATE("31.03.14")) THEN do: 
            {pp-uni.p &NEW_1256 = YES
		&LaserOff = YES
                &NoInpDefs = YES
                &FRM_PRN  = " pp-uni_1.frm " 
                &a107h    = YES}
         END.
         ELSE DO: 
            RUN pp-uni_old_1.p (INPUT Rid).
         END.
      END.
   END. 
END.

