/*
               ��� "���� ����"
    Copyright: 
     Filename: report_mini.p
      Comment: ����� � ���ﭨ� �� �� 㪠������ ����
   Parameters: 
         Uses:
      Used by:
      Created: vvv
     Modified: 
*/
{globals.i}

/* ��ᨬ ���� */
{getdate.i}

IF end-date >= TODAY THEN
	DO:
		/**/
		MESSAGE "����� ������������ �������� �� ���� �� ������� " + STRING(TODAY, "99/99/9999") VIEW-AS ALERT-BOX TITLE " �訡�� ".
		UNDO, RETRY.	
		/**/
	END.

IF NOT CONNECTED("bank") THEN
	DO:
	{spinner.i "����������..."}
    CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf") NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "�� 㤠���� ᮥ�������� � ����� BANK" VIEW-AS ALERT-BOX.
        RETURN.
    END.
END.

	/* ��� �뤥������ ��  ��室�� ���� */
	RUN mini_com.p (end-date).


IF CONNECTED("bank") THEN
    DISCONNECT bank.
IF CONNECTED("bismfr") THEN
    DISCONNECT bismfr.

    
















