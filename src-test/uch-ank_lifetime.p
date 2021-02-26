{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

DEF VAR vResult AS CHAR NO-UNDO. /* Результат РВ. */
DEF VAR vErrMsg AS CHAR NO-UNDO. /* Результат РВ. */

IF NOT {assigned iValue} THEN RETURN.

If NOT CAN-DO("Более  5 лет,От 1 до 5 лет,От 6 до 12 месяцев,От 3 до 6 месяцев,Менее 3 месяцев", iValue) THEN  
RETURN ERROR "Введенное значение не пододит по допустимой маске. ~n Для выбора правильного значения нажмите F1".