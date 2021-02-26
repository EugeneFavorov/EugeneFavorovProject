DEFINE VARIABLE mInfMess AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliMess AS CHARACTER NO-UNDO.
mInfMess = "".
FOR EACH DataBlock WHERE 
   DataBlock.DataClass-ID = "pcd"
   NO-LOCK,
   EACH DataLine WHERE TRUE
   AND DataLine.Data-ID EQ DataBlock.Data-ID
   AND DataLine.Sym1    EQ STRING(tt-person.person-id)
   NO-LOCK:
      mInfMess = mInfMess + "~n" + DataLine.Txt.
      mCliMess = TRIM(DataLine.Sym3).
END.

IF {assigned mInfMess} THEN
MESSAGE 
   "Информация о клиенте:" mCliMess SKIP 
   mInfMess
VIEW-AS ALERT-BOX.

