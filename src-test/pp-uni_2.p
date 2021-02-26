{globals.i}
Define Input Param rid As RecID No-Undo.
FIND FIRST op WHERE RECID(op) EQ RID NO-LOCK NO-ERROR.
IF AVAIL(op) THEN DO:
   if op.op-date ge date("31.03.14") OR 
      (op.op-date EQ ? AND op.doc-date GE DATE("31.03.14")) then do:
      RUN pp-uni_new_2.p(rid).
   end.
   else do:
      RUN pp-uni_old_2.p(rid).
   end.
END.
