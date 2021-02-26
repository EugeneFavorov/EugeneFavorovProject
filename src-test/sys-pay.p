/*
*/


&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{lshpr.pro} 
{t-otch.i NEW}

{svarloan.def NEW}
{loan.pro}
{intrface.get comm}
{intrface.get instrum}
{intrface.get card}

{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
def var i as int64           NO-UNDO.
def var j as int64           NO-UNDO.
def var str01  as char       NO-UNDO.
def var Direct01 as char     NO-UNDO.
DEF VAR mStrTable AS CHAR    NO-UNDO.

DEFINE TEMP-TABLE ttOp-entry
   FIELD OpTran    AS int64
   FIELD Op        AS int64
   FIELD Op-entry  AS int64
   FIELD DateP     AS DATE
   FIELD curr      AS CHARACTER
   FIELD acct-db   AS CHARACTER
   FIELD acct-cr   AS CHARACTER
   FIELD amt-rub   AS decimal
   FIELD System    AS CHARACTER
   FIELD Direct    AS CHARACTER
   FIELD Stat      AS CHARACTER
   FIELD iUserId   AS CHARACTER
   FIELD iBranchId AS CHARACTER
   FIELD IDPlat    AS int64
   FIELD FIOPlat   AS CHARACTER
   FIELD SummaP    AS decimal
   FIELD SummaC    AS decimal
   FIELD SummaCB   AS decimal
   FIELD Sts       AS CHARACTER    /**/
   INDEX m1 Op-entry
   INDEX m2 System Direct 
   INDEX m3 OpTran
   .

DEFINE TEMP-TABLE ttOp-tran
   FIELD OpTran   AS int64        /**/
   FIELD DateP    AS DATE         /**/
   FIELD System   AS CHARACTER    /**/
   FIELD curr     AS CHARACTER
   FIELD Direct   AS CHARACTER    /**/
   FIELD Stat     AS CHARACTER    /**/
   FIELD IDPlat   AS int64        /**/
   FIELD FIOPlat  AS CHARACTER    /**/
   FIELD SummaP   AS decimal
   FIELD SummaC   AS decimal
   FIELD SummaCB  AS decimal
   FIELD Sts      AS CHARACTER    /**/
   FIELD iUserId   AS CHARACTER
   FIELD iBranchId AS CHARACTER
   INDEX m1 System Direct 
   INDEX m2 OpTran
   .




i = num-entries(iStr).
output to "stop.txt".
do j = 1 to i.
   str01 = entry(j,iStr).
   for each op-entry where op-entry.op-cod = str01
                       and op-entry.op-date = today
                       and op-entry.filial-id = shFilial /* нужно это доделать.*/
   .
      find first Op where Op.op = op-entry.op no-error.
      find first ttOp-entry where ttOp-entry.op = op-entry.op no-error.
      if not avail ttOp-entry then do:
         create ttOp-entry.
         Assign
            ttOp-entry.op           = op-entry.op
            ttOp-entry.op-entry     = op-entry.op-entry
            ttOp-entry.OpTran       = op-entry.op-transaction
            ttOp-entry.DateP        = op-entry.op-date
            ttOp-entry.amt-rub      = op-entry.amt-rub
            ttOp-entry.acct-db      = op-entry.acct-db
            ttOp-entry.acct-cr      = op-entry.acct-cr
            ttOp-entry.System       = op-entry.op-cod
            ttOp-entry.Stat         = op-entry.op-status
            ttOp-entry.curr         = op-entry.curr
            ttOp-entry.iUserId      = op-entry.User-Id
            ttOp-entry.iBranchID    = op.branch-id
         .
         if op-entry.curr <> "" then do:
            ttOp-entry.amt-rub = op-entry.amt-cur.
         end.
         else do:
            ttOp-entry.amt-rub = op-entry.amt-rub.
         end.
      end.
   end.
end.

for each ttOp-entry USE-INDEX m3.
   find first ttOp-tran where ttOp-tran.OpTran = ttOp-entry.OpTran no-error.
   if not avail ttOp-tran then do:
      create ttOp-tran.
      Assign
         ttOp-tran.OpTran       = ttOp-entry.OpTran
         ttOp-tran.DateP        = ttOp-entry.DateP
         ttOp-tran.iUserId      = ttOp-entry.iUserId
         ttOp-tran.iBranchID    = ttOp-entry.ibranchid
      .
   end.
   if ttOp-tran.System = "" then do:
      ttOp-tran.System  = ttOp-entry.System.
   end.
   if ttOp-tran.Curr    = "" then do:
      ttOp-tran.Curr    = ttOp-entry.Curr.
   end.
   if ttOp-tran.Direct  = "" and ttOp-entry.acct-db begins "202" then do:
      ttOp-tran.Direct  = "Отправка".
   end.
   if ttOp-tran.Direct  = "" and ttOp-entry.acct-cr begins "202" then do:
      ttOp-tran.Direct  = "Получение".
   end.
   if ttOp-tran.IDPlat  = 0 and ttOp-entry.acct-cr begins "40911" then do:
      find first signs where signs.file = "op"
                         and signs.code = "ФИО"
                         and signs.surr = string(ttOp-entry.op) no-lock no-error.
      if avail signs 
      then do:
          ttOp-tran.FIOPlat = signs.xattr-value .
          ttOp-tran.IDPlat = -1.
      end.
   end.
   if ttOp-tran.IDPlat  = 0 and  ttOp-entry.acct-cr begins "409" then do:
      find first acct where acct.acct = ttOp-entry.acct-cr no-lock no-error.
      if not avail acct then do:
         message "Ошибка поиска счёта " ttOp-entry.acct-cr view-as alert-box.
         next.
      end.
      if acct.cust-cat <> "Ч" then do:
         message "Неправильный клиент по счёту " acct.acct view-as alert-box.
         next.
      end.
      ttOp-tran.IDPlat = acct.cust-id.
      find first person where person.person-id = acct.cust-id no-lock no-error.
      if not avail person then do:
         next.
      end.
      ttOp-tran.FIOPlat = trim(name-last) + " " + trim(first-names) .
   end.
   if ttOp-tran.IDPlat  = 0 and  ttOp-entry.acct-db begins "409" then do:
      find first acct where acct.acct = ttOp-entry.acct-db no-lock no-error.
      if not avail acct then do:
         message "Ошибка поиска счёта " ttOp-entry.acct-db view-as alert-box.
         next.
      end.
      if acct.cust-cat <> "Ч" then do:
         message "Неправильный клиент по счёту " acct.acct view-as alert-box.
         next.
      end.
      ttOp-tran.IDPlat = acct.cust-id.
      find first person where person.person-id = acct.cust-id no-lock no-error.
      if not avail person then do:
         next.
      end.
      ttOp-tran.FIOPlat = trim(name-last) + " " + trim(first-names) .
   end.

end.
/* заполнение статуса платежа */
for each ttOp-entry USE-INDEX m3.
   find first ttOp-tran where ttOp-tran.OpTran = ttOp-entry.OpTran no-error.
   if not avail ttOp-tran then do:
      message "Ошибочный транзакционный код " ttOp-entry.OpTran view-as alert-box.
      next.
   end.
   if ttOp-tran.Direct  = "Отправка" and ttOp-entry.acct-cr begins "3" then do:
      ttOp-tran.Stat    = ttOp-entry.Stat.
   end.
   if ttOp-tran.Direct  = "Получение" and ttOp-entry.acct-cr begins "202" then do:
      ttOp-tran.Stat    = ttOp-entry.Stat.
   end.
end.

/* Заполнение суммы перевода и комиссии чужого банка (по документу перечисления комиссии) */

for each ttOp-entry USE-INDEX m3.
   find first ttOp-tran where ttOp-tran.OpTran = ttOp-entry.OpTran no-error.
   if not avail ttOp-tran then do:
      message "Ошибочный транзакционный код " ttOp-entry.OpTran view-as alert-box.
      next.
   end.
   if ttOp-tran.Direct  = "Получение" and ttOp-entry.acct-db begins "409" and ttOp-entry.acct-cr begins "202" then do:
      ttOp-tran.SummaP  = ttOp-tran.SummaP + ttOp-entry.amt-rub.
   end.
   if ttOp-tran.Direct  = "Отправка" and ttOp-entry.acct-db begins "409" and ttOp-entry.acct-cr begins "3" then do:
      ttOp-tran.SummaP  = ttOp-entry.amt-rub.
   end.
   if ttOp-tran.Direct  = "Отправка" and ttOp-entry.acct-db begins "706" and ttOp-entry.acct-cr begins "3" then do:
      ttOp-tran.SummaCB = ttOp-entry.amt-rub.
      ttOp-entry.Sts = "Обр".
   end.
end.

RUN Insert_TTName ("info", ""). 
FIND FIRST ttNames WHERE
           ttnames.tname EQ 'info'
NO-LOCK NO-ERROR.
for each ttOp-tran USE-INDEX m2.
   export ttOp-tran.
      mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                        ELSE "") +
      TRIM(STRING(ttOp-tran.Direct)) + "~n" +  
      TRIM(STRING(ttOp-tran.Stat)) + "~n" +  
      TRIM(STRING(ttOp-tran.System)) + "~n" +  
      TRIM(STRING(ttOp-tran.dateP,"99.99.9999")) + "~n" +  
      TRIM(STRING(ttOp-tran.IDPlat)) + "~n" +  
      TRIM(STRING(ttOp-tran.FIOPlat)) + "~n" +  
      TRIM(STRING(ttOp-tran.iBranchId)) + "~n" +  
      TRIM(STRING(ttOp-tran.iUserId)) + "~n" +  
      TRIM(STRING(ttOp-tran.SummaP)) + "~n" +  
      TRIM(STRING(ttOp-tran.curr)) + "~n" +  
      TRIM(STRING(ttOp-tran.SummaCB)) + "~n" +  
      TRIM(STRING(ttOp-tran.OpTran))   
      .

end.

RUN INSERT_TTNAME ("Info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'Info'
NO-LOCK NO-ERROR.
ttnames.tvalue = mStrTable.

RUN printvd.p ("sys-pay",INPUT TABLE ttnames).

output close.




