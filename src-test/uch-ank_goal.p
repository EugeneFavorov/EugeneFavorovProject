{globals.i}
{intrface.get re}
{intrface.get rights}
{intrface.get xclass}
{intrface.get tmess}
{xattrpar.def}

DEFINE INPUT PARAMETER iClass AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iId    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCode  AS CHARACTER NO-UNDO.

DEF VAR vResult AS CHAR NO-UNDO. /* Результат РВ. */
DEF VAR vErrMsg AS CHAR NO-UNDO. /* Результат РВ. */

IF NOT {assigned iValue} THEN RETURN.
/*GetXAttrValueEx ("cust-corp", in-cust-id, "ИнНекОрг", "")GetXAttrValueEx(iClass,iId,iCode,"") iClass iId iCode*/
If NOT CAN-DO("до 100 операций,от 100 до 500 месяц,более 500 операций", iValue) THEN  
RETURN ERROR "Введенное значение не пододит по допустимой маске. ~n Для выбора правильного значения нажмите F1".