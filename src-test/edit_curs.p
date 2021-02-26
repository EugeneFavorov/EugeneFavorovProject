{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{pick-val.i}
{refer.i}
{tmprecid.def}

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: 
      Comment: ��楤�� ।���஢���� ���ᮢ �����
   Parameters:
         Uses:
		 Date: 03/07/2014
      Used by:
      Created: vvv
*/

DEF VAR mAction 	AS CHAR 		INIT "��������"		NO-UNDO.	/* ����⢨� */
DEF VAR mDate 		AS DATE 		INIT TODAY 			NO-UNDO.  	/* ��� */
DEF VAR mTimes 		AS CHARACTER 						NO-UNDO.  	/* �६� */
DEF VAR mTimes2 	AS CHARACTER 						NO-UNDO.  	/* ����� �६� */
DEF VAR QP 			AS DATETIME 						NO-UNDO.	/* ⥪�饥 �६� */
DEF VAR nCNT		AS INTEGER 		INIT 0				NO-UNDO.	/* ���稪 ���������� ����ᥩ */

QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").
/* ���㣫�� �� �� */
mTimes2 = SUBSTRING( REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":",""), 1, 2) + "0000".


/* �ଠ �롮� */
DEFINE FRAME fChoose
	mAction		VIEW-AS COMBO-BOX LIST-ITEMS '��������','������஢����' FORMAT "x(14)"
				LABEL "����⢨�"	
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ �롮� ����⢨� ]" .	

/* �ଠ 㤠����� */
DEFINE FRAME fDelete
   mDate 			LABEL			"���       " 							SKIP
   mTimes			LABEL			"�६�      " 	FORMAT "99:99:99"
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ �������� ���ᮢ ]" .	

/* �ଠ ।���஢���� */
DEFINE FRAME fEdit
   mDate 			LABEL			"���       " 							SKIP
   mTimes			LABEL			"�६�      " 	FORMAT "99:99:99"		SKIP
   "---------------------" SKIP
   mTimes2			LABEL			"����� �६�" 	FORMAT "99:99:99"
	WITH OVERLAY SIDE-LABEL CENTERED ROW 8
        TITLE COLOR BRIGHT-WHITE "[ ������஢���� ]" .		
		
/**/
MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
		/**/
		PAUSE 0.
		/**/
		UPDATE
		mAction
		WITH FRAME fChoose.	
		/**/
	END.
	/**/
	HIDE FRAME fChoose NO-PAUSE.
	/**/
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN
		LEAVE MAIN_BLOCK.
	/**/
	/* 㤠���� ����� ����� */
	IF mAction = '��������' THEN
		DO:
			/**/
			IF LASTKEY EQ 27 THEN
				RETURN.
			/**/
			PAUSE 0.
			nCNT = 0.
			/**/
			UPDATE
			mDate
			mTimes
			WITH FRAME fDelete.	
			
			/**/
			HIDE FRAME fDelete NO-PAUSE.
			/**/
			IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
				DO:
					/* ��㧥� ���ࠧ������� */
					RUN browseld.p ("branch",
								   "",
								   "",
								   "branch-type",
								   2).
					/**/
					/* ���� �� �ᥬ �⬥祭�� ���ࠧ������� */
					FOR EACH tmprecid
					NO-LOCK,
						/* ��室�� ���ࠧ������� */
						FIRST branch
						WHERE RECID(branch) EQ  tmprecid.id
						NO-LOCK:
							
							/* ��室�� ����� ����� ��� ������� ���ࠧ������� */
							/* �� 㪠������ �६� */
							FOR EACH irate-time
							WHERE irate-time.branch-id  = branch.branch-id
							AND irate-time.instr-cat  = "currency"
							AND irate-time.iratedatetime  = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99"))
							EXCLUSIVE-LOCK:
							/**/
								/* 㤠�塞 ����� */
								DELETE irate-time NO-ERROR.
								/**/
								nCNT = nCNT + 1.
								/**/
							END.
							/**/
					END.		
					/**/
					MESSAGE "������� " + STRING(nCNT) + ( IF nCNT = 1 THEN " ������!" ELSE IF nCNT > 1 AND nCNT < 5 THEN " �����!" ELSE " ����ᥩ!" ) VIEW-AS ALERT-BOX.
					/**/
				END.
		END.
	/**/
	/* ��������� �६��� ���� ����� */
	IF mAction = '������஢����' THEN
		DO:
			/**/
			IF LASTKEY EQ 27 THEN
				RETURN.		
			/**/
			PAUSE 0.
			nCNT = 0.
			/**/
			UPDATE
			mDate
			mTimes
			mTimes2
			WITH FRAME fEdit.	
			/**/
			IF mTimes2 = "" OR mTimes2 = mTimes THEN
				DO:
					/**/
					MESSAGE "������ ����� �६�!" VIEW-AS ALERT-BOX TITLE "�訡��".
					UNDO, RETRY.
					/**/
				END.
			/**/
			HIDE FRAME fDelete NO-PAUSE.
			/**/
			IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
				DO:
					/* ��㧥� ���ࠧ������� */
					RUN browseld.p ("branch",
								   "",
								   "",
								   "branch-type",
								   2).
					/**/
					/* ���� �� �ᥬ �⬥祭�� ���ࠧ������� */
					FOR EACH tmprecid
					NO-LOCK,
						/* ��室�� ���ࠧ������� */
						FIRST branch
						WHERE RECID(branch) EQ  tmprecid.id
						NO-LOCK:
							/* ��室�� ����� ����� ��� ������� ���ࠧ������� */
							/* �� 㪠������ �६� */
							FOR EACH irate-time
							WHERE irate-time.branch-id  = branch.branch-id
							AND irate-time.instr-cat  = "currency"
							AND irate-time.iratedatetime  = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99"))
							EXCLUSIVE-LOCK:
							/**/
								/**/
								ASSIGN
									irate-time.iratedatetime = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes2, "99:99:99"))
								.
								/**/
								nCNT = nCNT + 1.
								/**/
							END.
							/**/		
					END.		
					/**/
					MESSAGE "�������� �६� � " + STRING(nCNT) + (IF nCNT = 1 THEN " �����!" ELSE " ����ᥩ!") VIEW-AS ALERT-BOX.
					/**/
				END.			
		END.	
/**/
END.




