PROCEDURE Get-Xattr-Var:
   def var c_cond-pay as char no-undo. 
   c_Cond-Pay = GetXAttrValueEx("op", STRING(op.op), "УслОпл", ?).

   if c_Cond-Pay <> ? then do:
      Cond-Pay[1] = GetCodeName("УслОпл", c_Cond-Pay).
     
      if Cond-Pay[1] = ? then
	 Cond-Pay[1] = c_Cond-Pay.
   END.
   ELSE DO:
      assign
         Cond-Pay[1] = ""
	 c_Cond-Pay  = ""
      .
   END.
   IF   c_Cond-Pay BEGINS "С"
     OR c_Cond-Pay EQ "2"  THEN
      Num-Day = GetXAttrValueEx("op", STRING(op.op), "СркАкц", Num-Day).
   ELSE DO:
      Num-Day = "".
      Cond-Pay[1] = right-trim(Cond-Pay[1] /*+ " " + GetXAttrValueEx("op", STRING(op.op), "ОснСпБАк", "")*/).
      {wordwrap.i &s=Cond-Pay &n=3 &l=57 }
   END.

   mDataMarkDBank = STRING(Date(GetXAttrValueEx("op", STRING(op.op), "ДатаОтмБПОЛ", ""))       ,"99.99.9999").
   IF NOT {assigned mDataMarkDBank} THEN mDataMarkDBank = "".
   mDataCartIn    = STRING(Date(GetXAttrValueEx("op", STRING(op.op), "ДатаПомещенияВКарт", "")),"99.99.9999").
   IF NOT {assigned mDataCartIn} THEN mDataCartIn = "".

END PROCEDURE.
