DEFINE INPUT PARAMETER in-op-date   LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER in-rec-kind  AS   RECID      NO-UNDO.
DEF VAR details_       AS CHAR.
def var buf-str as char.

def var date-cur as date.
def var op-op-date as date.
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
   IMPORT STREAM s_imp UNFORMATTED buf-str NO-ERROR.
   str_summ = entry(3, buf-str,';') no-error.
   summ_ = dec(str_summ) .

   message str_summ. pause 0.
   ost-kau     = 0. 
   ob-kau-db   = entry(1, buf-str,';') no-error. 
   ob-kau-cr   = entry(2, buf-str,';') no-error. 
   details_    =  entry(4, buf-str,';') no-error. 
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
   {op(sess).cr &op-status=chr(251)}
    assign op.doc-date = op-op-date
           op.op-date = op-op-date
           op.op-value-date = op-op-date
           op.due-date     =  op-op-date
           op.details     = details_
           op.acct-cat    = 'b'
           op.doc-type    =  '09'
           op.doc-num = STRING(op.op)
           op.user-id = USERID("bisquit")
           op.op-kind = "000ivv"
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
   .


end.


