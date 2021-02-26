/*
Изначально известны следующие сведения:
 - Наименование получателя
 - БИК банка получателя	
 - Счет получателя
 - ИНН получателя (если получателем является юридическое лицо)

Условие А: поле "Получатель" равно наименованию заранее заданного получателя.
Условие B: поле "Счет получателя" рано номеру заранее заданного счета.
Условие C: поле "Назначение платежа" содержит номер заранее заданного счета.
Условие D: поле "ИНН получателя" равно заранее заданному ИНН
Условие E: поле "БИК банка получателя" равно заранее заданному БИКу.

Критерий фильтра:
F = D or (E and (A or B or C))

Фильтр сработает:
1) если ИНН получателя совпадет с заранее заданным значением
2) при одновременном выполнении условий A, E
3) при одновременном выполнении условий B, E
4) при одновременном выполнении условий C, E

*/
DEFINE INPUT  PARAMETER iParam-01       AS CHARACTER    NO-UNDO.
DEFINE output PARAMETER f01_Error       AS CHARACTER    NO-UNDO.
{globals.i}
{intrface.get terr}

define var f01_Inn   as char no-undo.
define var fio_fr    as char no-undo.
define var fio_pol   as char no-undo.
define var tData-Id  as int no-undo.
define var critX     as log  no-undo.
define var mPercLen  as int     no-undo.  
define var mPercWord as int     no-undo.  

mPercLen  = 90.
mPercWord = 80.
 
find first op where op.op = int(iParam-01) no-lock no-error.
if not avail op then do:
   return .
end.
find first DataClass WHERE DataClass.Parent-ID EQ 'DOP_KLB' 
                       and DataClass.DataClass-ID EQ "FRAUD"
                     USE-INDEX Parent  NO-LOCK no-error.
if not avail DataClass then do:
   return.
end.
Find first  DataBlock  WHERE DataBlock.DataClass-ID EQ 'FRAUD' NO-LOCK no-error.
if not avail DataBlock then do:
   return.
end.
tData-Id = DataBlock.Data-Id.
fio_pol = op.name-ben. 

find FIRST op-bank OF op NO-LOCK no-error.
if avail op-bank then do:
   for each DataLine WHERE DataLine.Data-ID EQ tData-Id no-lock:
      if    DataLine.sym1    eq op.inn
        or (DataLine.sym2    eq op-bank.bank-code
            and ( DataLine.sym3    eq op.ben-acct 
                 or  CompareNameFast (INPUT trim(DataLine.sym4),INPUT fio_pol,INPUT mPercLen,INPUT mPercWord)
                 or  index(op.details,DataLine.sym3) > 0  
                 )) then do:
         
         critX = Yes.
      end.
   end.
end.    


if critX then do :
   f01_Error = "OF1".
   if INDEX(op.op-error,f01_Error) = 0  then do:
      op.op-error = op.op-error + (IF op.op-error <> '' THEN ',' ELSE '') + "mess-error:" + f01_Error.
   end.
end.

/*
   for each DataLine WHERE DataLine.Data-ID EQ tData-Id 
                           and  (DataLine.sym1    eq op.inn 
                            or  (DataLine.sym2    eq op-bank.bank-code
                            and (DataLine.sym3    eq op.ben-acct 
                            or  DataLine.sym4     eq fio_pol
                            or  index(op.op-details,DataLine.sym3) > 0  
                          )))
          no-lock.

      if avail DataLine then do:
         output to "f1.txt".
         export DataLine.
         output close.
         critX = Yes.
      end.


*/

/*            vOK = CompareNameFast (INPUT mNameBase,INPUT mNameIn,INPUT mPercLen,INPUT mPercWord)
*/
