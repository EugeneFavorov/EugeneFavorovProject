/*
Фильтр № 2

Пояснение: платежи физическим лицам, юридическим лицам, индивидуальным предпринимателям, благотворительным организациям. 

Условие А: поле <БИК банка получателя> не содержится в списке our_bic_list;
Условие B: счет получателя соответствует маскам счетов, указанным в списке cred_acc_f2_list;
Условие С: в период времени с [текущая дата минус количество дней, заданное параметром begin_date_delta_f2]
           до [текущая дата минус количество дней, заданное параметром end_date_delta_f2] 
           не было ни одного исполненного платежа на этого получателя от этого отправителя;
Условие D : У Клиента подключена услуга <SMS-подтверждение отправляемых платежей>
Условие E : платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании;
Условие F: ИНН получателя отсутствует в списке emoney_services_inn_list;
Условие G: поле <БИК банка получателя> не удовлетворяет ни одной маске из списка exclude_bic_list
Условие H: Платеж на сумму больше или равную той, которая задана параметром min_pay_sum_f2
Условие J: Код валюты платежа присутствует в списке currency_code

Критерий фильтра:
F = A and B and C and F and G and H and J and ((not D) or (D and E))


Добрый день.
Прошу внести корректировки по фильтру 2 в БИС и в отчет, т.к. это сильно разгрузит операционистов и не будет раздражать клиентов :
"В случае если платеж совершает  клиент ЮЛ на свой счет открытый в другом Банке или в нашем, 
такой платеж не должен попадать в фильтр по сомнительным операциям! 
ИНН одинаковый у плательщика и получателя средств."
А. Мамыкин ушел в отпуск или на больничный и не успел написать вам, но обещал.
С уважением,
исполнительный директор ПАО "Плюс Банк"
Нестёркина Лариса Анатольевна
тел.: (3812) 39 30 15
внутр: 060


*/

DEFINE INPUT  PARAMETER iParam-01       AS CHARACTER    NO-UNDO.
DEFINE output PARAMETER f01_Error       AS CHARACTER    NO-UNDO.
{globals.i}

f01_Error = "".

def var begn_dt_delta_f2  as int64 no-undo.
def var cred_acc_f2_list  as char no-undo.
def var currency_code     as char no-undo.
def var end_dt_delta_f2   as int64 no-undo.
def var exclude_bic_list  as char no-undo.
def var min_pay_sum_f2    as dec  no-undo.
def var our_bic_list      as char no-undo.
def var emon_serv_inn_ls  as char no-undo.
def var critA             as log  no-undo init no.
def var critB             as log  no-undo init no.
def var critC             as log  no-undo init yes.
def var critD             as log  no-undo init no.
def var critE             as log  no-undo init no.
def var critF             as log  no-undo init no.
def var critH             as log  no-undo init no.
def var critG             as log  no-undo init no.
def var critJ             as log  no-undo init no.
def var critK             as log  no-undo init no.
def var critX             as log  no-undo init no.

def var date_b   as date no-undo.
def var date_e   as date no-undo.

DEF BUFFER bop for op.
DEF BUFFER bop-entry for op-entry.
DEF BUFFER bop-bank   for op-bank.


begn_dt_delta_f2 =  int(FGetSetting("КЛБДопПров","begn_dt_delta_f2",?)).
cred_acc_f2_list =  FGetSetting("КЛБДопПров","cred_acc_f2_list",?).
currency_code    =  FGetSetting("КЛБДопПров","currency_code",?).
end_dt_delta_f2  =  int(FGetSetting("КЛБДопПров","end_dt_delta_f2",?)).
exclude_bic_list =  FGetSetting("КЛБДопПров","exclude_bic_list",?).
min_pay_sum_f2   =  dec(FGetSetting("КЛБДопПров","min_pay_sum_f2",?)).
our_bic_list     =  FGetSetting("КЛБДопПров","our_bic_list",?).
emon_serv_inn_ls =  FGetSetting("КЛБДопПров","emon_serv_inn_ls",?).

find first op where op.op = int(iParam-01) no-lock no-error.
find FIRST op-bank  OF op NO-LOCK no-error.
find FIRST op-entry OF op NO-LOCK no-error.
if not avail op-bank then do:
   return.
end.
if not avail op-entry then do:
   return.
end.

if not op.op-kind MATCHES "*klb*" then do:
   return.
end.


/* Условие А:  поле <БИК банка получателя> НЕ содержится в списке our_bic_list; */
if not can-do(replace(our_bic_list," ",""),op-bank.bank-code) then critA = Yes.

/* Условие B: счет получателя соответствует маскам счетов, указанным в списке cred_acc_f2_list; */
if can-do(replace(cred_acc_f2_list," ",""),op.ben-acct) then critB = Yes.

/*
   Условие С: в период времени с [текущая дата минус количество дней, 
   заданное параметром begin_date_delta_f2] до [текущая дата минус количество дней, 
   заданное параметром end_date_delta_f2] не было ни одного исполненного платежа 
   на этого получателя от этого отправителя; 
   Считаю, что нужно проверку производить по связке (БИК + счет), так как платежи могут идти в адрес как юрлиц,
   так и физлиц. В платежках на физлицо в поле <ИНН получателя> указывается ИНН банка получателя, либо ИНН не указывается.
   Связка (БИК + счет) должна однозначно определять как юрлицо, так и физлицо.
   С уважением,
   начальник отдела информационной безопасности ПАО <Плюс Банк>
   Мамыкин Антон Юрьевич.
*/

date_b = op-entry.op-date - begn_dt_delta_f2.
date_e = op-entry.op-date - end_dt_delta_f2.

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

/* Условие D : У Клиента подключена услуга <SMS-подтверждение отправляемых платежей> */
if GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMS") = "Да"  then critD = Yes.

/* Условие E : платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании; */
if op-entry.amt-rub < dec(GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMSSumma"))  then critE = Yes.

/* Условие F: ИНН получателя отсутствует в списке emoney_services_inn_list; */
if not can-do(replace(emon_serv_inn_ls," ",""),op.inn) then critF = Yes.

/* Условие G: поле <БИК банка получателя> не удовлетворяет ни одной маске из списка exclude_bic_list */
if not can-do(replace(exclude_bic_list," ",""),op-bank.bank-code) then critG = Yes.

/* Условие H: Платеж на сумму больше или равную той, которая задана параметром min_pay_sum_f2 */
if op-entry.amt-rub >= min_pay_sum_f2 then critH = Yes.

/* Условие J: Код валюты платежа присутствует в списке currency_code */
if  can-do(replace(currency_code," ",""),(if op-entry.curr = "" then "810" else op-entry.curr)) then critJ = Yes.

/* Условие K : ИНН Плательщика и получателя разные */
find first acct where acct.acct = op-entry.acct-db.
if acct.cust-cat = "Ю" then do:
   find first cust-corp where cust-corp.cust-id = acct.cust-id no-lock no-error.
   if avail cust-corp then do:
      if op.inn <> cust-corp.inn then do:
         critK = yes.
      end.
   end.
end.
if acct.cust-cat = "Ч" then do:
   find first person where person.person-id = acct.cust-id no-lock no-error.
   if avail person then do:
      if op.inn <> person.inn then do:
         critK = yes.
      end.
   end.
end.

/*
F = A and B and C and F and G and H and J and ((not D) or (D and E)) and K
*/

critX = critA and critB and critC and critF and critG and critH and critJ and ( not critD or (critD and critE)) and critK
.

/*
message 
"critA = " critA
"critB = " critB
"critC = " critC
"critD = " critD
"critE = " critE
"critF = " critF
"critG = " critG
"critH = " critH
"critJ = " critJ
"critK = " critK
"critX = " critX
view-as alert-box.
*/


if critX then do: 
   f01_Error = "OF2".
   if INDEX(op.op-error,f01_Error) = 0  then do:
      op.op-error = op.op-error + (IF op.op-error <> '' THEN ',' ELSE '') + "mess-error:" + f01_Error.
   end.
end.
return. 








