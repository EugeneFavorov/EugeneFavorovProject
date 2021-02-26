{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

   DEF VAR vResult   AS CHAR  NO-UNDO. /* ������� ��. */
   DEF VAR vErrMsg   AS CHAR  NO-UNDO. /* ������� ��. */

IF NOT {assigned iValue} THEN RETURN.
 
ELSE IF LENGTH(TRIM(iValue)) NE 8 AND 
        LENGTH(TRIM(iValue)) NE 11 AND   
             (  NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{8~}",
                           iValue,
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg)) 
             OR   NOT (DYNAMIC-FUNC("ereg":U IN h_re,"^[0-9]~{11~}",
                           iValue,
                           OUTPUT vResult,
                           INPUT-OUTPUT vErrMsg))       
                )             
            
     THEN RETURN ERROR "����୮� ���祭�� ४����� !~n"
     + "���� ��� ����� ���������� 8 ��� 11 ����� ~n ��� ����� ���� ����� �ᯮ���� ᮮ⢥�����騩 �������⥫�� ४����� (�����)" .
