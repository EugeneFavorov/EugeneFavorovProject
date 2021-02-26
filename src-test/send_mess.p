{globals.i}
{intrface.get bicq}


/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: 
      Comment: ��ࠢ�� ᮮ�饭�� ����ࠬ �� ��������� ���� �����
   Parameters: ��� � �६� ��������� ���� �����
         Uses:
      Used by:
      Created: vvv
*/

DEF VAR mDate 	AS DATE INIT TODAY NO-UNDO.  /* ��� */
DEF VAR mTimes 	AS CHARACTER NO-UNDO.  		 /* �६� */
DEF VAR QP 		AS DATETIME NO-UNDO.
DEF VAR SF		AS DATETIME NO-UNDO.
DEF VAR sell 	AS CHAR INIT "�த���" NO-UNDO.
DEF VAR buy 	AS CHAR INIT "���㯪�" NO-UNDO.
/**/
DEF VAR tmp_str	AS CHAR INIT "" NO-UNDO.
DEF VAR tmp_usr	AS CHAR INIT "" NO-UNDO.
/**/
DEFINE TEMP-TABLE frates
    FIELD branch 	AS CHAR		/* ���ࠧ������� */
	FIELD datetime	AS DATETIME /* ��� � �६� ����⢨�*/
	FIELD type		AS CHAR		/* ⨯ ���� */
	FIELD cur		AS CHAR		/* ����� */
	FIELD code		AS CHAR		/* ��� ������ */
	FIELD val		AS DECIMAL	/* ���祭�� */
	FIELD usr		AS CHAR		/* ���� ��ࠢ�塞 ᮮ�饭�� */
.

DEFINE TEMP-TABLE mess
	FIELD branch 	AS CHAR
	FIELD usr		AS CHAR
	FIELD MESS		AS CHAR
.

{empty frates}

/**/
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").

/* ����㥬 �ମ�� ��� ����� ������ */
DEFINE FRAME fGet  
   mDate 			LABEL			"   ���       " 							SKIP
   mTimes			LABEL			"   �६�      " 	FORMAT "99:99:99" 		SKIP

   WITH WIDTH 30 /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ ��������� ���� ����� ]" .

/* �६�*/
ON LEAVE OF mTimes IN FRAME fGet  
DO:	  

	 ERROR-STATUS:ERROR = NO.
	 
     SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + SELF:SCREEN-VALUE) NO-ERROR.
	 
		IF ERROR-STATUS:ERROR = YES THEN DO:
			MESSAGE "����୮ 㪠���� �६�!" VIEW-AS ALERT-BOX TITLE "�訡��".
			RETURN NO-APPLY.
		END.	
   mTimes = SELF:SCREEN-VALUE.
END.		
		
MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mDate
			mTimes
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.	
	
	/* ���஡㥬 ���� ��������� ���ᮢ �����, �ந��������� � 㪠������ �६� */	
	
	FOR EACH irate-time
		WHERE irate-time.iratedatetime = DATETIME( STRING(mDATE, "99-99-9999") + " " + STRING(mTimes,"99:99:99") )
		AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
		NO-LOCK BY irate-time.branch-id :
		
			CREATE frates.
			
			ASSIGN
				frates.branch = irate-time.branch-id
				frates.datetime = irate-time.iratedatetime
				frates.type = irate-time.rate-type
				frates.code = irate-time.instr-code
				frates.val = irate-time.rate-instr
			.
			/**/
			FIND FIRST currency 
				WHERE currency.currency = irate-time.instr-code
			NO-LOCK NO-ERROR.
			
			/**/
			IF AVAIL currency THEN
				ASSIGN 
					frates.cur = currency.name-currenc
				.
			/**/	
	END.
	

	/**/
	IF AVAIL frates THEN
		DO:
			/* ��㯯��㥬 ᮮ�饭�� �� ���ࠧ������� */
			FOR EACH frates
				BREAK BY frates.branch:
					/**/
					IF FIRST-OF(frates.branch) THEN 
						DO:
							/**/
							tmp_str = "".
							/**/
							CREATE mess.
							/**/
							ASSIGN
								mess.branch = frates.branch
							.
						END.				
					/**/	
					IF tmp_str = "" THEN
						tmp_str = "|" + STRING(frates.cur, "x(13)") + "|" + STRING(frates.type, "x(13)") + "|" + STRING(STRING(mTimes,"99:99:99"),"x(9)")+ "|" + STRING(STRING(frates.val,"-99.9999"), "x(12)") + "|" + "@".
					ELSE
						tmp_str = tmp_str + "|" + STRING(frates.cur, "x(13)") + "|" + STRING(frates.type, "x(13)") + "|" + STRING(STRING(mTimes,"99:99:99"),"x(9)")+ "|" + STRING(STRING(frates.val,"-99.9999"), "x(12)") + "|" + "@".
					/**/
					IF LAST-OF(frates.branch) THEN
						DO:
							ASSIGN
								mess.mess = tmp_str.
						END.
					/**/
			END.

			/* ���� �� ⠡��� */
			FOR EACH mess
				BY mess.branch:
				/**/
				tmp_usr = "".
				/* ��室�� ������ ᬥ�� ����஢ �⮣� ���ࠧ������� */
				FOR EACH sessions
					WHERE sessions.op-date = mDate
					AND sessions.branch-id = mess.branch
					AND sessions.dpr-status = "�������":
					/**/
					tmp_usr = tmp_usr + (IF tmp_usr = "" THEN "" ELSE ",") + sessions.user-id.
				END.
				/**/
				ASSIGN
					mess.usr = tmp_usr
				.
			END.
			/**/

			/* ��ࠢ�塞 ᮮ�饭�� */
			FOR EACH mess :
				RUN MyGroupSendMess in h_Bicq(mess.usr, "", mess.mess, "currency").
			END.
			/**/
			MESSAGE "����饭�� �ᯥ譮 ��ࠢ����!" VIEW-AS ALERT-BOX.
			/**/
		END.
	ELSE
		MESSAGE "�� 㪠������ ���� � �६� ��������� ���� �� ������� �� � ������ ���ࠧ�������!" VIEW-AS ALERT-BOX.
	
	/*
	RUN MyGroupSendMess in h_Bicq("0_TEST", "", "|   ������    | �த���/���㯪� | 35.1242/38.4120  |@|    ����     | �த���/���㯪� | 35.1242/38.4120  |", "currency").
	*/
	
END.


