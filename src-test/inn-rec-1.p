
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
DEF VAR tcust-cat AS CHAR    NO-UNDO.
DEF VAR tcusr-id  AS int     NO-UNDO.

def var date_b   as date.
def var gbeg-date_save as date.
def var inn_pol  as char.
def var tinn     as char.
def var inn_pl   as char.
def var name_pl  as char.
def var name_pol as char.
def var num      as char.
def var num3      as char.
DEFINE VARIABLE vOpTime   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vOpTimeB   AS INTEGER   NO-UNDO.
DEF BUFFER bacct for acct.


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
    field name-pl as char
    index pol inn-pol
  .



user_ =  USERID("bisquit").     

tINN = "5504144522".

FORM
  " " skip
  tINN form "x(12)" LABEL " ИНН клиента   " skip
  " "
  WITH FRAME d1
       CENTERED SIDE-LABELS
       title " Введите ИНН клиента ".

 update tINN with frame d1 overlay row 4.
 hide frame d1.
 {on-esc LEAVE}
 inn_pl = "".
 name_pl = "".
 find first cust-corp  where cust-corp.inn = tINN no-lock no-error.
 if avail cust-corp then do:
    num = GetXAttrValue("cust-corp",string(cust-corp.cust-id),"АнализОФМ").
    if int(num) <> 3 then  do :
       message "У клиента с ИНН " tINN " не установлен АнализОФМ равный 3 " view-as alert-box.
       return.
    end.
    inn_pl = cust-corp.inn.
    create inn_3.
    assign
       inn_3.inn-pl   = inn_pl
       inn_3.cust-cat = "Ю"
       inn_3.cust-id  = cust-corp.cust-id.
   .
   name_pl = cust-corp.name-short .
   date_b = cust-corp.date-in.
 end.
 else do:
    find first person  where person.inn = tINN no-lock no-error.
    if avail person then do:
       num = getxattrvalue ("person",string(person.person-id), "АнализОФМ").
       if int(num) <> 3 then do:
          message "У клиента с ИНН " tINN " не установлен АнализОФМ равный 3 " view-as alert-box.
          return.
       end.
       inn_pl = person.inn.
       create inn_3.
       assign
          inn_3.inn-pl   = inn_pl
          inn_3.cust-cat = "Ч"
          inn_3.cust-id  = person.person-id.
       .
       name_pl = person.name-last + " " + person.first-names.
       date_b = person.date-in.
    end.
 end.
 if inn_pl = "" then do:
    message "Не найден клиент с ИНН " tINN view-as alert-box.
    return.
 end.
 else do:
    message name_pl date_b view-as alert-box.
 end. 

 DEF VAR tmp-date-1 AS DATE NO-UNDO.

 gbeg-date_save  = gbeg-date.
 gbeg-date = date_b.

 {getdates.i &beglabel="Дата начала" &endlabel="Дата окончания"}

 gbeg-date = gbeg-date_save.

vOpTimeB = TIME.

output to "inn3.txt".

for each inn_3.
   for each acct where acct.cust-cat = inn_3.cust-cat
                   and acct.cust-id  = inn_3.cust-id
                   and (acct.close-date = ? or acct.close-date >= beg-date)
                   and acct.acct begins "40"
                   no-lock.
      for each op-entry  use-index entry-db where op-entry.acct-db = acct.acct
                                              and  op-entry.op-date >= beg-date  
                                              and  op-entry.op-date <= end-date
                                              no-lock,
         first op of op-entry
         no-lock:
         if op-entry.acct-cr  begins "301" then do:
            inn_pol = op.inn.
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
         if op-entry.acct-cr  begins "40" then do:
            find first bacct where bacct.acct = op-entry.acct-cr no-lock no-error.
            if not avail bacct then next.
            if bacct.cust-cat = "Ю" then do:
               find first cust-corp  where cust-corp.cust-id = bacct.cust-id no-lock no-error.
               if not avail cust-corp then next.
               inn_pol = cust-corp.inn.
            end.
            if bacct.cust-cat = "Ч" then do:
               find first person  where person.person-id = bacct.cust-id no-lock no-error.
               if not avail person then next.
               inn_pol = person.inn.
            end.
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
      end.
   end.
end.

output to "inn_1.txt".
for each inn_.
   export inn_.
end.
output close.

for each op-entry  where  op-entry.acct-cat = "b"
                     and  op-entry.op-date >= beg-date  
                     and  op-entry.op-date <= end-date
                     and  op-entry.curr = ""
                     and  (op-entry.acct-cr  begins "301" or op-entry.acct-cr  begins "40")
                     and  op-entry.status >= chr(251)
                     no-lock,
    first op of op-entry
    no-lock:
    if  op-entry.acct-cr  begins "301" then do:
       inn_pol = op.inn.
       if inn_pol = "" then  next.
       if inn_pol = ? then  next.
       if inn_pol = "000000000000" then  next.
       if inn_pol = "0000000000" then  next.
       if inn_pol = "0" then  next.
       find first inn_ where inn_.inn-pol = inn_pol no-lock no-error.
       if not avail inn_ then next.
       find first acct where acct.acct = op-entry.acct-db no-lock no-error.
       if not avail acct then next.
       if acct.cust-cat = "Ю" then do:
          find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
          if not avail cust-corp then next.
          inn_pl = cust-corp.inn.
          name_pl = cust-corp.name-short.
       end.
       if acct.cust-cat = "Ч" then do:
          find first person  where person.person-id = acct.cust-id no-lock no-error.
          if not avail person then next.
          inn_pl = person.inn.
          name_pl = person.name-last +  person.first-name.
       end.
       if inn_pl = "" then  next.
       if inn_pl = ? then  next.
       num3 = "0".
       find first inn_3 where inn_3.inn-pl = inn_pl no-lock no-error.
       if not avail inn_3 then num3 = "0".
       inn_pol = op.inn.
       create op_.
       assign
          op_.op      = op.op
          op_.inn-pl  = inn_pl
          op_.inn-pol = inn_pol
          op_.OFM     =  num3
          op_.name-pl = name_pl
       .
    end.
    if  op-entry.acct-cr  begins "40" then do:
       find first bacct where bacct.acct = op-entry.acct-cr no-lock no-error.
       if not avail bacct then next.
       if bacct.cust-cat = "Ю" then do:
          find first cust-corp  where cust-corp.cust-id = bacct.cust-id no-lock no-error.
          if not avail cust-corp then next.
          inn_pol = cust-corp.inn.
       end.
       if bacct.cust-cat = "Ч" then do:
          find first person  where person.person-id = bacct.cust-id no-lock no-error.
          if not avail person then next.
          inn_pol = person.inn.
       end.
       if inn_pol = "" then  next.
       if inn_pol = ? then  next.
       if inn_pol = "000000000000" then  next.
       if inn_pol = "0000000000" then  next.
       if inn_pol = "0" then  next.
       find first inn_ where inn_.inn-pol = inn_pol no-lock no-error.
       if not avail inn_ then next.
       find first acct where acct.acct = op-entry.acct-db no-lock no-error.
       if not avail acct then next.
       if acct.cust-cat = "Ю" then do:
          find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
          if not avail cust-corp then next.
          inn_pl = cust-corp.inn.
          name_pl = cust-corp.name-short.
       end.
       if acct.cust-cat = "Ч" then do:
          find first person  where person.person-id = acct.cust-id no-lock no-error.
          if not avail person then next.
          inn_pl = person.inn.
          name_pl = person.name-last +  person.first-name.
       end.
       if inn_pl = "" then  next.
       if inn_pl = ? then  next.
       num3 = "0".
       find first inn_3 where inn_3.inn-pl = inn_pl no-lock no-error.
       if not avail inn_3 then num3 = "0".
       inn_pol = op.inn.
       create op_.
       assign
          op_.op      = op.op
          op_.inn-pl  = inn_pl
          op_.inn-pol = inn_pol
          op_.OFM     =  num3
          op_.name-pl = name_pl
       .
    end.
end.

output close.

vOpTime = TIME - vOpTimeB .
message vOpTime view-as alert-box.

RUN BeginCircle_TTName ("opop").

for each op_
    no-lock,
first op where op.op = op_.op no-lock,
first op-entry of op no-lock,
first op-bank  of op no-lock
   by op.op-date
   by op.op:

   str02 = REPLACE(Op.details,"~n","").
   str02 = REPLACE(str02,CHR(13),"").
   str02 = REPLACE(str02,CHR(10),"").
   if  op-entry.acct-cr  begins "301" then do:
   
      mStrTable =
      TRIM(STRING(Op.op)) + "~n" +  
      TRIM(STRING("'" + op_.inn-pl)) + "~n" +  
      TRIM(STRING(op_.name-pl)) + "~n" +  
      TRIM(STRING("'" + Op.Inn)) + "~n" +  
      TRIM(STRING(Op.name-ben)) + "~n" +  
      TRIM(STRING(Op-bank.bank-name)) + "~n" +  
      TRIM(STRING("'" + Op.ben-acct)) + "~n" +  
      TRIM(STRING(Op.op-date)) + "~n" +  
      TRIM(STRING(Op-entry.amt-rub)) + "~n" +  
      TRIM(STRING(str02))  
      .
      RUN Insert_TTName("mStrTable[opop]",mStrTable).
      RUN NextCircle_TTName("opop").
   end.
   if op-entry.acct-cr  begins "40" then do:
      find first acct where acct.acct = op-entry.acct-cr no-lock no-error.
      if not avail acct then next.
      if acct.cust-cat = "Ю" then do:
         find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
         if not avail cust-corp then next.
         inn_pol = cust-corp.inn.
         name_pol = cust-corp.name-short.
      end.
      if acct.cust-cat = "Ч" then do:
         find first person  where person.person-id = acct.cust-id no-lock no-error.
         if not avail person then next.
         inn_pol = person.inn.
         name_pol = person.name-last +  person.first-name.
      end.
  
      mStrTable =
      TRIM(STRING(Op.op)) + "~n" +  
      TRIM(STRING("'" + op_.inn-pl)) + "~n" +  
      TRIM(STRING(op_.name-pl)) + "~n" +  
      TRIM(STRING(name_pol)) + "~n" +  
      TRIM(STRING("'" + inn_pol)) + "~n" +  
      TRIM(STRING("'")) + "~n" +  
      TRIM(STRING("'" + Op.ben-acct)) + "~n" +  
      TRIM("'" + STRING(Op.op-date))  + "~n" +  
      TRIM(STRING(Op-entry.amt-rub))  + "~n" +  
      TRIM(STRING(str02))  
      .
      RUN Insert_TTName("mStrTable[opop]",mStrTable).
      RUN NextCircle_TTName("opop").
   end.
end.

RUN NextCircle_TTName("opop").
RUN EndCircle_TTName ("opop").
RUN printvd.p ("inn-rec-1",INPUT TABLE ttnames).






