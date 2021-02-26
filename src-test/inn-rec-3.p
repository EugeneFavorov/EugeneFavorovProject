
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

/*
{lshpr.pro} 
{t-otch.i NEW}
{dpsproc.def}


{svarloan.def NEW}
{loan.pro}
{intrface.get comm}
{intrface.get instrum}
{intrface.get card}

{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{ksh-defs.i NEW}
{intrface.get instrum}  /** Функции для работы с курсами */
*/

/* DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO. */
def var i as int64           NO-UNDO.
def var j as int64           NO-UNDO.
def var str01     as char    NO-UNDO.
def var Direct01  as char    NO-UNDO.
DEF VAR mStrTable AS CHAR    NO-UNDO.
DEF VAR mtoday    AS date    NO-UNDO.
DEF VAR User_     AS CHAR    NO-UNDO.
DEF VAR str02     AS CHAR    NO-UNDO.


def var inn_pol  as char.
def var inn_pl   as char.
def var name_pl  as char.
def var num      as char.
def var num3      as char.
DEFINE VARIABLE vOpTime   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vOpTimeB   AS INTEGER   NO-UNDO.

  def temp-table inn_3 no-undo
    field inn-pl    as char
    field cust-cat  as char
    field cust-id   as int
    index pl inn-pl
  .
  def temp-table inn_ no-undo
    field inn-pol    as char
    field inn3       as char
    index pol inn-pol inn3
  .

  def temp-table op_ no-undo
    field op  as int
    field inn-pl  as char
    field inn-pol as char
    field OFM     as char
    index pol inn-pol
  .


user_ =  USERID("bisquit").     



{getdates.i &beglabel="Дата начала" &endlabel="Дата окончания"}

vOpTimeB = TIME.

for each acct where acct.acct begins "40" 
                and acct.close-date = ?
                no-lock.
    name_pl = "".
    if acct.cust-cat = "Ю" then do:
       find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
       if not avail cust-corp then next.
       num = GetXAttrValue("cust-corp",string(cust-corp.cust-id),"АнализОФМ").
       if int(num) <> 3 then  next.
       inn_pl = cust-corp.inn.
       if inn_pl = "" then  next.
       create inn_3.
       assign
          inn_3.inn-pl   = inn_pl
          inn_3.cust-cat = "Ю"
          inn_3.cust-id  = cust-corp.cust-id.
       .
    end.
    if acct.cust-cat = "Ч" then do:
       find first person  where person.person-id = acct.cust-id no-lock no-error.
       if not avail person then next.
       num = getxattrvalue ("person",string(person.person-id), "АнализОФМ").
       if int(num) <> 3 then  next.
       inn_pl = person.inn.
       if inn_pl = "" then  next.
       create inn_3.
       assign
          inn_3.inn-pl   = inn_pl
          inn_3.cust-cat = "Ч"
          inn_3.cust-id  = person.person-id.
       .
    end.
end.
for each op-entry  use-index amt-rub where op-entry.op-date >= beg-date  
                                and  op-entry.op-date <= end-date
                                no-lock.
    find first op of op-entry no-error .
    if not avail op then next.
    if not op-entry.acct-cr  begins "301" then next.
    find first acct where acct.acct = op-entry.acct-db no-lock no-error.
    if not avail acct then next.
    if acct.cust-cat = "Ю" then do:
       find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
       if not avail cust-corp then next.
       inn_pl = cust-corp.inn.
    end.
    if acct.cust-cat = "Ч" then do:
       find first person  where person.person-id = acct.cust-id no-lock no-error.
       if not avail person then next.
       inn_pl = person.inn.
    end.
    find first inn_3 where inn_3.inn-pl = inn_pl no-lock no-error.
    if not avail inn_3 then next.
    inn_pol = op.inn.
    if inn_pl = "" then  next.
    if inn_pl = ? then  next.
    if inn_pol = "" then  next.
    if inn_pol = ? then  next.
    if inn_pol = "000000000000" then  next.
    if inn_pol = "0000000000" then  next.
    find first inn_ where inn_.inn-pol = inn_pol
                     no-lock no-error.
    if avail inn_ then next.
    create inn_.
    assign
       inn_.inn-pol = inn_pol
       inn_.inn3    = inn_pl  
    .
end.
for each op-entry  use-index amt-rub where op-entry.op-date >= beg-date  
                                and  op-entry.op-date <= end-date
                                no-lock.
    find first op of op-entry no-error .
    if not avail op then next.
    if not op-entry.acct-cr  begins "301" then next.
    inn_pol = op.inn.
    if inn_pol = "" then  next.
    if inn_pol = ? then  next.
    if inn_pol = "000000000000" then  next.
    if inn_pol = "0000000000" then  next.
    find first inn_ where inn_.inn-pol = inn_pol no-lock no-error.
    if not avail inn_ then next.
    find first acct where acct.acct = op-entry.acct-db no-lock no-error.
    if not avail acct then next.
    if acct.cust-cat = "Ю" then do:
       find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
       if not avail cust-corp then next.
       inn_pl = cust-corp.inn.
    end.
    if acct.cust-cat = "Ч" then do:
       find first person  where person.person-id = acct.cust-id no-lock no-error.
       if not avail person then next.
       inn_pl = person.inn.
    end.
    num3 = "0".
    find first inn_3 where inn_3.inn-pl = inn_pl no-lock no-error.
    if not avail inn_3 then num3 = "3".
    inn_pol = op.inn.
    create op_.
    assign
       op_.op      = op.op
       op_.inn-pl  = inn_pl
       op_.inn-pol = inn_pol
       op_.OFM     =  num3
    .
end.

vOpTime = TIME - vOpTimeB .
message vOpTime view-as alert-box.


output to "inn3.txt".
for each op_.
   export op_.
end.
output close.


/*
RUN BeginCircle_TTName ("opop").

for each inn_.
   find first acct_ where acct_.inn-pl = inn_.inn-pol no-lock no-error.
   if not avail acct_ then next.
   for each op_ where op_.inn-pol  = inn_.inn-pol
                  and op_.inn-pl  <> inn_.inn-pol
                  no-lock.
      find first acct_ where acct_.inn-pl = op_.inn-pl no-lock no-error.
      if not avail acct_ then next.
      find first op where op.op = op_.op no-lock no-error.
      find first op-entry of op no-lock no-error.
      find first op-bank  of op no-lock no-error.
      str02 = REPLACE(Op.details,"~n","").
      str02 = REPLACE(str02,CHR(13),"").
      str02 = REPLACE(str02,CHR(10),"").

      mStrTable =
      TRIM(STRING(Op.op)) + "~n" +  
      TRIM(STRING("'" + op_.inn-pl)) + "~n" +  
      TRIM(STRING(acct_.name-pl)) + "~n" +  
      TRIM(STRING(Op.name-ben)) + "~n" +  
      TRIM(STRING("'" + Op.Inn)) + "~n" +  
      TRIM(STRING(Op-bank.bank-name)) + "~n" +  
      TRIM(STRING("'" + Op.ben-acct)) + "~n" +  
      TRIM(STRING(Op.op-date)) + "~n" +  
      TRIM(STRING(Op-entry.amt-rub)) + "~n" +  
      TRIM(STRING(str02))  
      .
      RUN Insert_TTName("mStrTable[opop]",mStrTable).
      RUN NextCircle_TTName("opop").
   end.
end.

RUN NextCircle_TTName("opop").
RUN EndCircle_TTName ("opop").
RUN printvd.p ("inn-rec",INPUT TABLE ttnames).
*/
/*RUN printvd.p ("sys-pay02",INPUT TABLE ttnames).*/
/*
output to "stop.txt".
for each ttnames.
export ttnames.
end.
output close.
*/




