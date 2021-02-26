{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

DEF VAR vResult AS CHAR NO-UNDO. /* Результат РВ. */
DEF VAR vErrMsg AS CHAR NO-UNDO. /* Результат РВ. */

IF NOT {assigned iValue} THEN RETURN.

If NOT CAN-DO("Менее 3 мес,От 3 до 6 мес,Более 6 мес", iValue) THEN  
RETURN ERROR "Введенное значение не пододит по допустимой маске. ~n Для выбора правильного значения нажмите F1".