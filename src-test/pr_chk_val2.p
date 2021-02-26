/*
	Filename: pr_chk_val2.p
	Comment : ��楤�� ��� ��।������ ����室�����
			  ����⭮�� ����஫�. ������� �� ���
			  ������ �� 04.09.2015.
	Created : ayv
*/

DEFINE PARAMETER BUFFER Op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT YES.

DEFINE BUFFER db-acct FOR acct.
DEFINE BUFFER cr-acct FOR acct.

{globals.i}
{intrface.get db2l}
{pp-uni.var &FILE_sword_p=YES}
{pp-uni.prg &NoUsePTransDoc=yes} 

FOR EACH op-entry OF op NO-LOCK:

	IF op-entry.acct-db NE ? THEN DO:
		{find-act.i
		 &bact = db-acct
		 &acct = op-entry.acct-db
		}
	END.

	IF op-entry.acct-cr NE ? THEN DO:
		{find-act.i
		 &bact = cr-acct
		 &acct = op-entry.acct-cr
		}
	END.

	IF AVAIL(cr-acct) AND AVAIL(db-acct) THEN DO:

		/* � ��⮢ �� �������襭�� ���⥦� */
		IF db-acct.currency EQ '' AND 
		   db-acct.bal-acct EQ 40807 AND 
		   cr-acct.currency NE '' AND 
		   cr-acct.bal-acct EQ 30220 THEN
			oResult = NO.

		IF db-acct.currency NE '' AND 
		   CAN-DO('40817,423*,40820,426*,407*,40807',STRING(db-acct.bal-acct)) AND 
		   cr-acct.currency NE '' AND 
		   cr-acct.bal-acct EQ 30220 THEN
			oResult = NO.

		/* � �������襭��� ���⥦�� �� ���譨� ��� ��� ��� ��㣨� 䨫����� */
		IF db-acct.currency NE '' AND 
		   db-acct.bal-acct EQ 30236 AND 
		   CAN-DO('40820*,426*,40807*',op.ben-acct) THEN
			oResult = NO.

		IF db-acct.currency EQ '' AND 
		   db-acct.bal-acct EQ 30236 AND 
		   CAN-DO('40820*,40807*',op.ben-acct) THEN
			oResult = NO.

		/* � �������襭��� ���⥦�� �� �࠭���� ��� */
		IF db-acct.currency NE '' AND 
		   db-acct.bal-acct EQ 30236 AND 
		   cr-acct.currency NE '' AND 
		   CAN-DO('407*,40802',STRING(cr-acct.bal-acct)) AND 
		   cr-acct.contract EQ '�࠭�1' THEN
			oResult = NO.

	END.

	IF oResult EQ NO THEN LEAVE.

END.