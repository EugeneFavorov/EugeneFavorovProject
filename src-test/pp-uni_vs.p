{globals.i}
Define Input Param rid As RecID No-Undo.
FIND FIRST op WHERE RECID(op) EQ rid NO-LOCK NO-ERROR.
IF AVAIL(op) THEN DO:
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF AVAIL(op-entry) THEN DO:
      IF CAN-DO(FGetSetting("Œ®‘çŒ áª","",""),op-entry.acct-cr) THEN DO:
         RUN memorn.p(INPUT rid).
      END.
      ELSE 
        {pp-uni_v.p 
		&NEW_1256  = YES
		&LaserOff  = YES
		&FRM_PRN   = " pp-uni_v.frm " 
		&NoInpDefs = YES}
   END. 
END.

