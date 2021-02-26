/*
Фильтр № 3

Пояснение: платежи через специальные банковские счета, оплата мобильных телефонов, перевод денег на электронные кошельки. 

Условие А:  поле <БИК банка получателя> НЕ содержится в списке our_bic_list;
Условие B:  счет получателя соответствует маскам счетов, указанным в списке cred_acc_f3_list;
Условие C:  платеж на сумму больше или равную той, которая задана параметром min_pay_sum_f3;
Условие D:  у Клиента подключена услуга <SMS-подтверждение отправляемых платежей>
Условие E:  платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании
Условие F:  ИНН получателя есть в списке emoney_services_inn_list
Условие G: счет получателя НЕ соответствует ни одной маске счета списка exclude_cred_acc_f3_list
Условие J: Код валюты платежа присутствует в списке currency_code


Критерий фильтра:
F = ((A and B and C and (not D)) or (A and B and C and D and E) or F) and G and H

*/
DEFINE INPUT  PARAMETER iParam-01       AS CHARACTER    NO-UNDO.
DEFINE output PARAMETER f01_Error       AS CHARACTER    NO-UNDO.

{globals.i}

f01_Error = "".

def var cred_acc_f3_list  as char no-undo.
def var currency_code     as char no-undo.
def var emon_serv_inn_ls  as char no-undo.
def var exe_cr_acc_f3_ls  as char no-undo.
def var min_pay_sum_f3    as dec  no-undo.
def var our_bic_list      as char no-undo.
def var critA             as log  no-undo.
def var critB             as log  no-undo.
def var critC             as log  no-undo.
def var critD             as log  no-undo.
def var critE             as log  no-undo.
def var critF             as log  no-undo.
def var critG             as log  no-undo.
def var critJ             as log  no-undo.
def var critX             as log  no-undo.

cred_acc_f3_list =  FGetSetting("КЛБДопПров","cred_acc_f3_list",?).
currency_code    =  FGetSetting("КЛБДопПров","currency_code",?).
emon_serv_inn_ls =  FGetSetting("КЛБДопПров","emon_serv_inn_ls",?).
exe_cr_acc_f3_ls =  FGetSetting("КЛБДопПров","exe_cr_acc_f3_ls",?).
min_pay_sum_f3   =  dec(FGetSetting("КЛБДопПров","min_pay_sum_f3",?)).
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
/* Условие А:  поле <БИК банка получателя> НЕ содержится в списке our_bic_list; */
if not can-do(replace(our_bic_list," ",""),op-bank.bank-code) then critA = Yes.

/* Условие B:  счет получателя соответствует маскам счетов, указанным в списке cred_acc_f3_list; */
if can-do(replace(cred_acc_f3_list," ",""),op.ben-acct) then critB = Yes.

/* Условие C:  платеж на сумму больше или равную той, которая задана параметром min_pay_sum_f3; */
if op-entry.amt-rub >= min_pay_sum_f3 then critC = Yes.

/* Условие D:  у Клиента подключена услуга <SMS-подтверждение отправляемых платежей> */
   if GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMS") = "Да"  then critD = Yes.

/* Условие E:  платеж на сумму меньшую, чем минимальная пороговая сумма, которую клиент указал сам в договоре об SMS информировании */
   if op-entry.amt-rub < dec(GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMSSumma"))  then critE = Yes.

/* Условие F:  ИНН получателя есть в списке emoney_services_inn_list */
if can-do(replace(emon_serv_inn_ls," ",""),op.inn) then critF = Yes.

/* Условие G: счет получателя НЕ соответствует ни одной маске счета списка exclude_cred_acc_f3_list */
if not can-do(replace(exe_cr_acc_f3_ls," ",""),op.ben-acct) then critG = Yes.

/* Условие J: Код валюты платежа присутствует в списке currency_code */
if  can-do(replace(currency_code," ",""),(if op-entry.curr = "" then "810" else op-entry.curr)) then critJ = Yes.

/*
Критерий фильтра:
F = ((A and B and C and (not D)) or (A and B and C and D and E) or F) and G and H
*/
critX = ((critA and critB and critC and not critD) or (critA and critB and critC and critD and critE ) or critF ) and critG and critJ.

/*
message 
"critA = " critA
"critB = " critB
"critC = " critC
"critD = " critD
"critE = " critE
"critF = " critF
"critG = " critG
"critJ = " critJ
"critX = " critX
view-as alert-box.
*/
if critX then do :
   f01_Error = "OF3".
   if INDEX(op.op-error,f01_Error) = 0  then do:
      op.op-error = op.op-error + (IF op.op-error <> '' THEN ',' ELSE '') + "mess-error:" + f01_Error.
   end.
end.

return. 








