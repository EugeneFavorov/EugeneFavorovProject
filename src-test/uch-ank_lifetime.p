{globals.i}
{intrface.get re}

DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

DEF VAR vResult AS CHAR NO-UNDO. /* ������� ��. */
DEF VAR vErrMsg AS CHAR NO-UNDO. /* ������� ��. */

IF NOT {assigned iValue} THEN RETURN.

If NOT CAN-DO("�����  5 ���,�� 1 �� 5 ���,�� 6 �� 12 ����楢,�� 3 �� 6 ����楢,����� 3 ����楢", iValue) THEN  
RETURN ERROR "��������� ���祭�� �� ������� �� �����⨬�� ��᪥. ~n ��� �롮� �ࠢ��쭮�� ���祭�� ������ F1".