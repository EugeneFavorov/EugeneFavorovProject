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

DEF VAR vResult AS CHAR NO-UNDO. /* ������� ��. */
DEF VAR vErrMsg AS CHAR NO-UNDO. /* ������� ��. */

IF NOT {assigned iValue} THEN RETURN.
/*GetXAttrValueEx ("cust-corp", in-cust-id, "�������", "")GetXAttrValueEx(iClass,iId,iCode,"") iClass iId iCode*/
If NOT CAN-DO("�� 100 ����権,�� 100 �� 500 �����,����� 500 ����権", iValue) THEN  
RETURN ERROR "��������� ���祭�� �� ������� �� �����⨬�� ��᪥. ~n ��� �롮� �ࠢ��쭮�� ���祭�� ������ F1".