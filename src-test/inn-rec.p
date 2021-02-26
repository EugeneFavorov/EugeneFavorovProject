{globals.i}
{prn-doc.def &with_proc=YES}

DEF VAR mStrTable AS CHAR   NO-UNDO.
DEF VAR str02     AS CHAR   NO-UNDO.
def var inn_pol   as char   NO-UNDO.
def var inn_pl    as char   NO-UNDO.
def var name_pl   as char   NO-UNDO.
def var name_ofm  as char   NO-UNDO.
DEF VAR vOpTimeB  AS INT    NO-UNDO.

def temp-table acct_ no-undo
    field acct      as char
    field inn-pl    as char
    field name-pl   as char
    field OFM       as char
    index pl IS PRIMARY UNIQUE inn-pl
    .
def temp-table op_  no-undo
    field op        as int
    field inn-pl    as char
    field inn-pol   as char
    index pol inn-pol
    .

DEFINE BUFFER bacct_ FOR acct_. 

{getdates.i &beglabel="„ â  ­ ç « " &endlabel="„ â  ®ª®­ç ­¨ï"}
vOpTimeB = TIME.

for each acct
    where acct.acct begins "40"
/*      and acct.close-date = ? */
      and (acct.close-date = ? or acct.close-date >= beg-date)
    no-lock:

    name_pl = "".
    inn_pl  = "".
    if acct.cust-cat = "" then do:
       find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
       if not avail cust-corp then next.
       inn_pl  = cust-corp.inn.
       name_pl = cust-corp.name-short.
       name_ofm   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "€­ «¨§”Œ", "").
    end.
    if acct.cust-cat = "—" then do:
       find first person  where person.person-id = acct.cust-id no-lock no-error.
       if not avail person then next.
       inn_pl  = person.inn.
       name_pl = person.name-last + " " + person.first-names.
       name_ofm   = GetXattrValueEX("person", STRING(person.person-id), "€­ «¨§”Œ", "").
    end.
    if inn_pl = "" then  next.
    if inn_pl = ?  then  next.
    if inn_pl = "000000000000" then  next.
    if inn_pl = "0000000000"   then  next.
    find first acct_ where inn-pl = inn_pl no-lock no-error.
    if avail acct_ then next.
    if name_ofm = ? then name_ofm = "".

    create acct_.
    assign
       acct_.acct    = acct.acct
       acct_.inn-pl  = inn_pl
       acct_.name-pl = name_pl
       acct_.ofm = name_ofm
    .
end.



for each op-entry
    where op-entry.op-date >= beg-date
      and op-entry.op-date <= end-date
      and op-entry.acct-cr begins "301"
    no-lock,
first op of op-entry
    no-lock:

    find first acct where acct.acct = op-entry.acct-db no-lock no-error.
    if not avail acct then next.
    inn_pl = "".
    if acct.cust-cat = "" then do:
       find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
       if not avail cust-corp then next.
       inn_pl = cust-corp.inn.
    end.
    if acct.cust-cat = "—" then do:
       find first person  where person.person-id = acct.cust-id no-lock no-error.
       if not avail person then next.
       inn_pl = person.inn.
    end.
    inn_pol = op.inn.
    if inn_pl  = "" then  next.
    if inn_pl  = ?  then  next.
    if inn_pol = "" then  next.
    if inn_pol = ?  then  next.
    if inn_pol = "000000000000" then  next.
    if inn_pol = "0000000000"   then  next.
    if inn_pol = inn_pl         then  next.
    find first acct_ where acct_.inn-pl = inn_pl  no-lock no-error.
    if not avail acct_ then next.
    find first acct_ where acct_.inn-pl = inn_pol no-lock no-error.
    if not avail acct_ then next.

    create op_.
    assign
       op_.op      = op.op
       op_.inn-pl  = inn_pl
       op_.inn-pol = inn_pol
    .
end.

/* message (TIME - vOpTimeB) view-as alert-box. */
RUN BeginCircle_TTName ("opop").

output to "inn-rec.txt".

for each op_
    no-lock,
first op where op.op = op_.op no-lock,
first op-entry of op no-lock,
first op-bank  of op no-lock,
first acct_  where acct_.inn-pl = op_.inn-pl no-lock,
first bacct_ where bacct_.inn-pl = op_.inn-pol no-lock
    by op_.inn-pl
    by op.op-date
    by op.op:
    str02     = REPLACE(REPLACE(REPLACE(op.details,"~n",""),CHR(13),""),CHR(10),"").
    mStrTable =        STRING(op.op)
              + "~n'" + op_.inn-pl
              + "~n"  + TRIM(acct_.name-pl)
              + "~n"  + (if TRIM(acct_.ofm) = "" then " " else TRIM(acct_.ofm))
              + "~n'" + Op_.inn-pol
              + "~n"  + TRIM(op.name-ben)
              + "~n"  + (if TRIM(bacct_.ofm) = "" then " " else TRIM(bacct_.ofm))
              + "~n"  + TRIM(op-bank.bank-name)
              + "~n'" + op.ben-acct
              + "~n'" + STRING(op.op-date, "99.99.9999")
              + "~n"  + TRIM(STRING(op-entry.amt-rub))
              + "~n"  + TRIM(str02).
    RUN Insert_TTName("mStrTable[opop]",mStrTable).
    RUN NextCircle_TTName("opop").
end.
output close.

RUN EndCircle_TTName ("opop").
RUN printvd.p ("inn-rec",INPUT TABLE ttnames).
