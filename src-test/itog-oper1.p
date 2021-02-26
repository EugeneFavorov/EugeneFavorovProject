/* DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.
*/

{globals.i}
{prn-doc.def &with_proc=YES}

def var beg-date  as date no-undo.
def var toper     as char no-undo.
def var mStrTable as char no-undo.
def var Str       as char no-undo.
def var top-kind  as char no-undo.

beg-date = today.

{getdates.i}



DEFINE TEMP-TABLE ttOper
   FIELD date-beg       AS DATE
   FIELD date-end       AS DATE
   FIELD filial-id      AS CHARACTER
   FIELD branch-id      AS CHARACTER
   FIELD oper           AS CHARACTER
   FIELD stat           AS CHARACTER
   FIELD op-kind        AS CHARACTER
   FIELD kol-vo         AS int64
   FIELD name           AS char
   FIELD FIO            AS char
   FIELD name-otd       AS char

   INDEX m1 filial-id  branch-id oper op-kind stat .


for each op where op.ins-date >= beg-date and op.ins-date <= end-date and op.op-status <> "А" no-lock.
   if op.user-inspector = "" then do:
      toper = op.user-id.
   end.
   else do:
      toper = op.user-inspector.
   end.
   find first _user where _user._userid = toper no-lock no-error.
   if avail _user then do:
      str = GetUserXAttrValue(_user._userid, "str-branch").
      if str = ? or str = "" then do:
         next. 
      end.
   end.
   else do:
      next.
   end.
   str = GetXattrValueEx("op",STRING(Op.op),"dpr-id",?).
   if str = ? or str = "" then do:
      next. 
   end.
   FIND FIRST code WHERE
   		   code.class = "ОперацииКассы" AND
   		   trim(code.code) = trim(op.op-kind)
   	NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      if code.val = "Исключить" then do:
         next.
      end.
      top-kind = code.parent.
   END.
   ELSE DO:
      top-kind = op.op-kind. 
   END.

   find first ttOper where ttOper.filial-id = op.filial-id
                       and ttOper.branch-id = op.branch-id
                       and ttOper.oper      = toper
                       and ttOper.op-kind   = top-kind
                       and ttOper.stat      = op.op-status 
                       no-error.
   if not avail ttOper then do:
      create ttOper.
      assign
         ttOper.date-beg  = beg-date    
         ttOper.date-end  = end-date    
         ttOper.filial-id = op.filial-id  
         ttOper.branch-id = op.branch-id  
         ttOper.oper      = toper         
         ttOper.stat      = op.op-status  
         ttOper.op-kind   = top-kind  
         ttoper.fio =  _user._User-Name
      .
   end.
   ttOper.kol-vo   =  ttOper.kol-vo + 1.     
end.
for each ttOper.
   find first op-kind where op-kind.op-kind = ttoper.op-kind no-lock no-error.
   if avail op-kind then do:
      ttoper.name =  op-kind.name-opkind.
   end.
   FIND FIRST code WHERE
   		   code.class = "ОперацииКассы" AND
   		   code.code = ttOper.op-kind
   	NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      ttoper.name =  code.name.
   END.
   find first branch where branch.branch-id = ttoper.branch-id no-lock no-error.
   if avail branch then do:
      ttoper.name-otd =  branch.Short-Name.
   end.
end.
/*
output to "123.txt".
for each ttOper.
   export ttOper.
end.
output close.
*/

RUN BeginCircle_TTName ("opop").

for each ttoper.
   mStrTable =
   TRIM(STRING(ttOper.FIO)) + "~n" +  
   TRIM(STRING(ttOper.name-otd)) + "~n" +  
   TRIM(STRING(ttOper.date-beg)) + "~n" +  
   TRIM(STRING(ttOper.date-end)) + "~n" +  
   TRIM(STRING(ttOper.name)) + "~n" +  
   TRIM(STRING(ttOper.kol-vo))  + "~n" +  
   TRIM(STRING(ttOper.stat))  + "~n" +  
   "`" + TRIM(STRING(ttOper.op-kind))   
   .
   RUN Insert_TTName("mStrTable[opop]",mStrTable).
   RUN NextCircle_TTName("opop").
end.

RUN NextCircle_TTName("opop").
RUN EndCircle_TTName ("opop").
RUN printvd.p ("kass-op1",INPUT TABLE ttnames).


