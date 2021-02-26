{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

   DEF VAR vResult   AS CHAR  NO-UNDO. /* ������� ��. */
   DEF VAR vErrMsg   AS CHAR  NO-UNDO. /* ������� ��. */

IF NOT {assigned iValue} THEN RETURN.
 
ELSE IF LENGTH(iValue) NE 14    
             OR   NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{3~}",
                           substring(iValue,1,3),
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg))
             OR   NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{3~}",
                           substring(iValue,5,3),
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg))
             OR   NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{3~}",
                           substring(iValue,9,3),
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg))
             OR   ((INDEX(iValue,"-") NE 4)
             OR   (INDEX(substring(iValue,5),"-") NE 4)
             OR   (INDEX(substring(iValue,9),"-") NE 4))




     THEN RETURN ERROR "����୮� ���祭�� ४����� !~n"
     + "����� ���㬥�� ������ ���� ������~n"
     + "� �ଠ� 'xxx-xxx-xxx-xx'. ".
