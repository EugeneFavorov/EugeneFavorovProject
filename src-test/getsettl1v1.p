
   {globals.i}
   def input  param mStr  as char   no-undo.
   DEF VAR vRecLoan      AS RECID   NO-UNDO.
   def var dDateBl       as date    no-undo.
   DEF VAR tContract     AS char    NO-UNDO.
   def var tCont-Code    as char    no-undo.
   if num-entries(mStr,"|") <> 2 then return.
   tContract    = entry(1,mStr,"|").
   tCont-code   = entry(2,mStr,"|").
   find first loan where loan.contract = tContract
                     and loan.Cont-code = tCont-code
                     no-lock no-error.
  if avail loan then do:
     RUN getsettl1.p(RECID(loan),loan.open-date).
  end.
 