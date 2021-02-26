/*
������ � 3

���᭥���: ���⥦� �१ ᯥ樠��� ������᪨� ���, ����� �������� ⥫�䮭��, ��ॢ�� ����� �� ���஭�� ��襫쪨. 

�᫮��� �:  ���� <��� ����� �����⥫�> �� ᮤ�ন��� � ᯨ᪥ our_bic_list;
�᫮��� B:  ��� �����⥫� ᮮ⢥����� ��᪠� ��⮢, 㪠����� � ᯨ᪥ cred_acc_f3_list;
�᫮��� C:  ���⥦ �� �㬬� ����� ��� ࠢ��� ⮩, ����� ������ ��ࠬ��஬ min_pay_sum_f3;
�᫮��� D:  � ������ ������祭� ��㣠 <SMS-���⢥ত���� ��ࠢ�塞�� ���⥦��>
�᫮��� E:  ���⥦ �� �㬬� �������, 祬 �������쭠� ��ண���� �㬬�, ������ ������ 㪠��� ᠬ � ������� �� SMS ���ନ஢����
�᫮��� F:  ��� �����⥫� ���� � ᯨ᪥ emoney_services_inn_list
�᫮��� G: ��� �����⥫� �� ᮮ⢥����� �� ����� ��᪥ ��� ᯨ᪠ exclude_cred_acc_f3_list
�᫮��� J: ��� ������ ���⥦� ��������� � ᯨ᪥ currency_code


���਩ 䨫���:
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

cred_acc_f3_list =  FGetSetting("�������஢","cred_acc_f3_list",?).
currency_code    =  FGetSetting("�������஢","currency_code",?).
emon_serv_inn_ls =  FGetSetting("�������஢","emon_serv_inn_ls",?).
exe_cr_acc_f3_ls =  FGetSetting("�������஢","exe_cr_acc_f3_ls",?).
min_pay_sum_f3   =  dec(FGetSetting("�������஢","min_pay_sum_f3",?)).
our_bic_list     =  FGetSetting("�������஢","our_bic_list",?).

find first op where op.op = int(iParam-01) no-lock no-error.
find FIRST op-bank  OF op NO-LOCK no-error.
find FIRST op-entry OF op NO-LOCK no-error.
if not avail op-bank then do:
   return.
end.
if not avail op-entry then do:
   return.
end.
/* �᫮��� �:  ���� <��� ����� �����⥫�> �� ᮤ�ন��� � ᯨ᪥ our_bic_list; */
if not can-do(replace(our_bic_list," ",""),op-bank.bank-code) then critA = Yes.

/* �᫮��� B:  ��� �����⥫� ᮮ⢥����� ��᪠� ��⮢, 㪠����� � ᯨ᪥ cred_acc_f3_list; */
if can-do(replace(cred_acc_f3_list," ",""),op.ben-acct) then critB = Yes.

/* �᫮��� C:  ���⥦ �� �㬬� ����� ��� ࠢ��� ⮩, ����� ������ ��ࠬ��஬ min_pay_sum_f3; */
if op-entry.amt-rub >= min_pay_sum_f3 then critC = Yes.

/* �᫮��� D:  � ������ ������祭� ��㣠 <SMS-���⢥ত���� ��ࠢ�塞�� ���⥦��> */
   if GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMS") = "��"  then critD = Yes.

/* �᫮��� E:  ���⥦ �� �㬬� �������, 祬 �������쭠� ��ண���� �㬬�, ������ ������ 㪠��� ᠬ � ������� �� SMS ���ନ஢���� */
   if op-entry.amt-rub < dec(GetXAttrValue("acct",op-entry.acct-db + ',' + op-entry.curr,"SMSSumma"))  then critE = Yes.

/* �᫮��� F:  ��� �����⥫� ���� � ᯨ᪥ emoney_services_inn_list */
if can-do(replace(emon_serv_inn_ls," ",""),op.inn) then critF = Yes.

/* �᫮��� G: ��� �����⥫� �� ᮮ⢥����� �� ����� ��᪥ ��� ᯨ᪠ exclude_cred_acc_f3_list */
if not can-do(replace(exe_cr_acc_f3_ls," ",""),op.ben-acct) then critG = Yes.

/* �᫮��� J: ��� ������ ���⥦� ��������� � ᯨ᪥ currency_code */
if  can-do(replace(currency_code," ",""),(if op-entry.curr = "" then "810" else op-entry.curr)) then critJ = Yes.

/*
���਩ 䨫���:
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








