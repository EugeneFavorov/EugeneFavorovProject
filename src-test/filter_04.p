/*
Фильтр № 4

Пояснение: платежи, которые не были учтены при разработке логики фильтров. 

Условие А:  поле <БИК банка получателя> не содержится в списке our_bic_list;
Условие B:  счет получателя НЕ соответствует ни одной маске счета списка cred_acc_f4_list;
Условие С: в период времени с [текущая дата минус количество дней, заданное параметром begin_date_delta_f4] 
           до [текущая дата минус количество дней, заданное параметром end_date_delta_f4] 
           не было ни одного исполненного платежа на этого получателя от этого отправителя;
Условие D:  у Клиента подключена услуга <SMS-подтверждение отправляемых платежей>;
Условие E: платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании

Критерий фильтра:
F = (A and B and C) and ((not D)) or (D and E))

*/

DEFINE INPUT  PARAMETER iParam-01       AS CHARACTER    NO-UNDO.
DEFINE output PARAMETER f01_Error       AS CHARACTER    NO-UNDO.
{globals.i}

f01_Error = "".

def var begn_dt_delta_f4  as int64 no-undo.
def var cred_acc_f4_list  as char no-undo.
def var currency_code     as char no-undo.
def var end_dt_delta_f4   as int64 no-undo.
def var exclude_bic_list  as char no-undo.
def var min_pay_sum_f4    as dec  no-undo.
def var our_bic_list      as char no-undo.
def var emon_serv_inn_ls  as char no-undo.
def var critA             as log  no-undo init no.
def var critB             as log  no-undo init no.
def var critC             as log  no-undo init yes.
def var critD             as log  no-undo init no.
def var critE             as log  no-undo init no.
def var critX             as log  no-undo init no.

def var date_b   as date no-undo.
def var date_e   as date no-undo.

DEF BUFFER bop for op.
DEF BUFFER bop-entry for op-entry.
DEF BUFFER bop-bank   for op-bank.


begn_dt_delta_f4 =  int(FGetSetting("КЛБДопПров","begn_dt_delta_f4",?)).
cred_acc_f4_list =  FGetSetting("КЛБДопПров","cred_acc_f4_list",?).
end_dt_delta_f4  =  int(FGetSetting("КЛБДопПров","end_dt_delta_f2",?)).
our_bic_list     =  FGetSetting("КЛБДопПров","our_bic_list",?).

find first op where op.op = int(iParam-01) no-lock no-error.
find FIRST op-bank  OF op NO-LOCK no-error.
find FIRST op-entry OF op NO-LOCK no-error.
if not avail op-bank then do:
   return.
end.
if not avail op-entry then do:
   return.
end.


/* Условие А:  поле <БИК банка получателя> не содержится в списке our_bic_list; */
if not can-do(replace(our_bic_list," ",""),op-bank.bank-code) then critA = Yes.

/* Условие B:  счет получателя НЕ соответствует ни одной маске счета списка cred_acc_f4_list; */
if not can-do(replace(cred_acc_f4_list," ",""),op.ben-acct) then critB = Yes.

/*
   Условие С: в период времени с [текущая дата минус количество дней, 
   заданное параметром begin_date_delta_f4] до [текущая дата минус количество дней, 
   заданное параметром end_date_delta_f4] не было ни одного исполненного платежа 
   на этого получателя от этого отправителя; 
   Считаю, что нужно проверку производить по связке (БИК + счет), так как платежи могут идти в адрес как юрлиц,
   так и физлиц. В платежках на физлицо в поле <ИНН получателя> указывается ИНН банка получателя, либо ИНН не указывается.
   Связка (БИК + счет) должна однозначно определять как юрлицо, так и физлицо.
   С уважением,
   начальник отдела информационной безопасности ПАО <Плюс Банк>
   Мамыкин Антон Юрьевич.
*/

date_b = op-entry.op-date - begn_dt_delta_f4.
date_e = op-entry.op-date - end_dt_delta_f4.

for each  bop-entry where bop-entry.acct-db = op-entry.acct-db
                      and bop-entry.op-date >= date_b
                      and bop-entry.op-date <= date_e
                      and bop-entry.op-status >= chr(251)
                      and bop-entry.curr = op-entry.curr
                      no-lock ,
         first bop of bop-entry where bop.ben-acct = op.ben-acct no-lock,
         first bop-bank of bop where bop-bank.bank-code = op-bank.bank-code
         no-lock :
         critC = no.

/*
         message bop-entry.op view-as alert-box.
*/
         LEAVE.
end.

/* Условие D:  у Клиента подключена услуга <SMS-подтверждение отправляемых платежей>; */
if GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMS") = "Да"  then critD = Yes.

/* Условие E: платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании */
if op-entry.amt-rub < dec(GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMSSumma"))  then critE = Yes.


/*
Критерий фильтра:
F = (A and B and C) and ((not D)) or (D and E))
*/

critX = critA and critB and critC and ( not critD or (critD and critE)).
/*
message 
"critA = " critA
"critB = " critB
"critC = " critC
"critD = " critD
"critE = " critE
"critX = " critX
view-as alert-box.
*/
if critX then do: 
   f01_Error = "OF4".
   if INDEX(op.op-error,f01_Error) = 0  then do:
      op.op-error = op.op-error + (IF op.op-error <> '' THEN ',' ELSE '') + "mess-error:" + f01_Error.
   end.
end.
return. 








