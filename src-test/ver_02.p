/*
DEFINE INPUT PARAMETER in-op-date   LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER in-rec-kind  AS   RECID      NO-UNDO.
*/

DEF VAR details_       AS CHAR.
def var buf-str as char.

def var date-cur as date.
def var op-op-date as date.
def var in-op-date as date.
def var cur-op-date as date.
def var ost-kau       as dec.
def var ob-kau-db     as char.
def var ob-kau-cr     as char.
def var ost-kau-pr    as dec.
def var ob-kau-db-pr  as dec.
def var ob-kau-cr-pr  as dec.
def var vost-kau      as dec.
def var vob-kau-db    as dec.
def var vob-kau-cr    as dec.
def var vost-kau-pr   as dec.
def var vob-kau-db-pr as dec.
def var vob-kau-cr-pr as dec.
def var summ_ as dec.
def var str_summ as char.

DEFINE BUFFER xloan-acct FOR loan-acct .
DEFINE BUFFER yloan-acct FOR loan-acct .
in-op-date = today.
op-op-date = in-op-date.
cur-op-date = in-op-date.

  {globals.i}
  {sh-defs.i new}
  {ksh-defs.i new}
  {defoptr.i new}


DEF STREAM s_imp.

{getfile.i &filename='"pv.csv"' &mode=must-exist}


INPUT STREAM s_imp FROM VALUE(fname).
REPEAT ON ERROR UNDO, RETRY:
   IMPORT STREAM s_imp UNFORMATTED buf-str   NO-ERROR.
   buf-str = CODEPAGE-CONVERT(buf-str,SESSION:CHARSET,"1251"). 
   str_summ = "1".
   summ_ = dec(str_summ) .
   ost-kau     = 0. 
   ob-kau-db   = "99999810400000000000". 
   if num-entries(buf-str) >= 2 then do:
      ob-kau-cr   = entry(1, buf-str,';') no-error. 
   end.
   else do:
       message buf-str skip " в файле нет счёта " view-as alert-box.
       next.
   end.
   find first acct where acct.acct begins ob-kau-cr
                     and acct.filial-id = shfilial
                     no-lock no-error.
   if avail acct then do:
      details_ = acct.details.
      details_ = "Выданы п" + substr(details_,index(details_,"Правоустанав") + 1).
   end.
   if num-entries(buf-str) >= 2 then do:
      details_    = details_ + " " + trim(entry(2, buf-str,';')). 
   end.
   ost-kau-pr  = 0. 
   ob-kau-db-pr = 0.
   ob-kau-cr-pr = 0.
   vost-kau     = 0.
   vob-kau-db   = 0.
   vob-kau-cr    = 0.
   vost-kau-pr   = 0.
   vob-kau-db-pr = 0.
   vob-kau-cr-pr = 0.
   CREATE op.
   {op(sess).cr &op-status='Ф'}
    assign op.doc-date = op-op-date
           op.op-date = op-op-date
           op.op-value-date = op-op-date
           op.due-date     =  op-op-date
           op.details     = details_
           op.acct-cat    = 'o'
           op.doc-type    =  'ВБО'
           op.doc-num =  string(int(substr(STRING(op.op),length(STRING(op.op)) - 3,4))) 
           op.user-id = userid("bisquit")
           op.op-kind = "МО внебаланс"
           op.order-pay = "5"
   .
   CREATE op-entry.
   ASSIGN
    op-entry.op         = op.op
    op-entry.op-entry   = 1
    op-entry.user-id    = op.user-id
    op-entry.op-status  = op.op-status
    op-entry.op-date    = op.op-date
    op-entry.acct-cat   = op.acct-cat
    op-entry.acct-db    = ob-kau-db
    op-entry.acct-cr    = ob-kau-cr
    op-entry.currency   = ""
    op-entry.amt-cur    = 0
    op-entry.amt-rub    = summ_
    op-entry.op-status  = op.op-status
    op-entry.value-date = op.op-date
    op-entry.type       = "ВН"
   .


end.


