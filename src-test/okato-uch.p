{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

   DEF VAR vResult   AS CHAR  NO-UNDO. /* Результат РВ. */
   DEF VAR vErrMsg   AS CHAR  NO-UNDO. /* Результат РВ. */

IF NOT {assigned iValue} THEN RETURN.
 
ELSE IF LENGTH(TRIM(iValue)) NE 8 AND 
        LENGTH(TRIM(iValue)) NE 11 AND   
	      (  NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{9~}",
                           iValue,
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg)) 
             OR   NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{11~}",
                           substring(iValue,1,3),
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg))
                )             
            
     THEN RETURN ERROR "Неверное значение реквизита !~n"
     + "Поле Код ОКАТО заполняется 8 или 11 значным числом~n Для ввода Кода ОКТМО используйте соответствующий дополнительный реквизит (ОКАТО-НАЛОГ)" .
