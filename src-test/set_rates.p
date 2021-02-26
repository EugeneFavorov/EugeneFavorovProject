/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2007 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: importpol.p
      Comment: �஢�ઠ �।�⮢
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

/*
{globals.i}
{setdest.i}
{intrface.get count}
*/

{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{pick-val.i}
{refer.i}
{tmprecid.def}

    
DEFINE VARIABLE mBranch AS CHARACTER  NO-UNDO.  /* ���ࠧ������� */
DEFINE VARIABLE mCurr	AS CHARACTER  NO-UNDO.  /* ����� 1 */
DEFINE VARIABLE mCurr-2 AS CHARACTER  NO-UNDO.  /* ����� 2 */
DEFINE VARIABLE mCurrName AS CHARACTER NO-UNDO.  /* ������������ ������ 1 */
DEFINE VARIABLE mCurrName-2 AS CHARACTER NO-UNDO.  /* ������������ ������ 2 */
DEFINE VARIABLE mDate AS DATE INIT TODAY NO-UNDO.  /* ��� */
DEFINE VARIABLE mTimes AS CHARACTER NO-UNDO.  /* �६� */
DEFINE VARIABLE mType AS CHARACTER  NO-UNDO.  /* ⨯ ����樨 */
DEFINE VARIABLE mCrossRate AS LOGICAL INITIAL YES. /* ���� ���� */ 
DEFINE VARIABLE mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* ���祭�� */ 
DEFINE VARIABLE mZZ AS DECIMAL INITIAL 1.00 FORMAT ">>>999.99" NO-UNDO. /* �� ᪮�쪮 */ 

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEFINE BUFFER xxx FOR irate-time.
DEFINE BUFFER yyy FOR irate-time.
DEFINE BUFFER zzz FOR irate-time.

/**/

DEFINE BUTTON button1 LABEL "��������".
DEFINE BUTTON button2 LABEL "�⬥����".
DEFINE VAR mMESS AS CHARACTER VIEW-AS TEXT /*FORMAT "x(20)"*/ NO-UNDO.

/**/
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").


/* ��� �⮣� �� ࠡ�⠥� �롮� ������ */
RUN SetSysConf IN h_base("IdBranch2",shfilial).

/*---------------------------------*/
/* �ଠ ��⠭���� ������ */

DEFINE FRAME fGet  
   mCurr 			LABEL 			"�����     " 	FORMAT "X(3)"
   mCurrName		NO-LABEL						FORMAT "X(20)"		  	SKIP 
   mDate 			LABEL			"���       " 							SKIP
   mTimes			LABEL			"�६�      " 	FORMAT "99:99:99" 		SKIP
   mType			LABEL			"��� ����  " 	FORMAT "x(12)" 			SKIP
   mValue			LABEL			"���祭��   " 							SKIP
   mZZ				LABEL			"��         " 							SKIP

   WITH WIDTH 40 /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ ��⠭���� ���ᮢ ����� ]" .

/**/
DEFINE FRAME Qwy
	mMESS		NO-LABEL /*FORMAT "X(25)"*/ SKIP(3)
	button1 	SPACE(5)
	button2
WITH /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9 
TITLE COLOR BRIGHT-WHITE "[ ������஢���� ���ᮢ ����� ]".	
		
/* ����� 1 */

ON F1 OF mCurr IN FRAME fGet  
DO:
   pick-value = "".
   DO WITH FRAME fGet :
      DO TRANSACTION:
         RUN vokcurx.p (mType:SCREEN-VALUE,"",4).
         IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE "" THEN 
         DO:
            FIND FIRST currency WHERE
               currency.currency EQ pick-value
               NO-LOCK NO-ERROR.
            IF AVAILABLE currency THEN
            DO:
               SELF:SCREEN-VALUE      = currency.currency.
			   mCurrName:SCREEN-VALUE = currency.name-currenc.
            END.
         END.
      END.
   END.
   RETURN NO-APPLY.
END.

/* ����� 1 */
ON LEAVE OF mCurr IN FRAME fGet 
DO:
   IF LASTKEY EQ 27 THEN
      RETURN.
	
   FIND FIRST currency WHERE
      currency.currency EQ SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE currency OR SELF:SCREEN-VALUE = "" THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      -1,
                      "����� " + SELF:SCREEN-VALUE + " �� �������"). 
      RETURN NO-APPLY.
   END.
   mCurr = SELF:SCREEN-VALUE.
   mCurrName:SCREEN-VALUE = currency.name-currenc.
END.

/* ��� ���� */
ON F1 OF mType IN FRAME fGet 
DO:
   pick-value = "".
   FIND FIRST code WHERE 
          code.class EQ ""
      AND code.code EQ "����� ���" NO-LOCK NO-ERROR.
   IF AVAIL code THEN
   DO:
      DO TRANSACTION:
         RUN vokxtype.p (code.code,
                         code.code,
                         "�롥� ��� ���� �����",
                         4).
      END.
      IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE "" THEN 
      DO:
         SELF:SCREEN-VALUE = pick-value.
      END.
   END.
   RETURN NO-APPLY.
END.

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

/* ��� ����*/
ON LEAVE OF mType IN FRAME fGet  
DO:	  
		IF SELF:SCREEN-VALUE = "" THEN DO:
			MESSAGE "����室��� 㪠���� ⨯ ����!" VIEW-AS ALERT-BOX TITLE "�訡��".
			RETURN NO-APPLY.
		END.	
   mType = SELF:SCREEN-VALUE.
END.
/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mCurr
			mDate
			mTimes
			mType
			mValue
			mZZ
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* �㦭� �஢���� 㪠����� ���祭�� */
	/*------------------------------------*/
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59").
	/*---------*/
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + STRING(mTimes, "99:99:99")).
	/*---------*/	
	
	IF mType = "" THEN DO:
		MESSAGE "����室��� 㪠���� ⨯ ����!" VIEW-AS ALERT-BOX TITLE "�訡��".
		UNDO, RETRY.
	END.
	
	IF mValue = 0 THEN DO:
		MESSAGE "���祭�� ������ ���� �⫨筮 �� ���!" VIEW-AS ALERT-BOX TITLE "�訡��".
		UNDO, RETRY.
	END.
	
	IF mZZ <> 1 AND mZZ <> 100 THEN DO:
		MESSAGE "���祭�� ���-�� ������ ���� 1, ���� 100." VIEW-AS ALERT-BOX TITLE "�訡��".
		UNDO, RETRY.
	END.
	
	/* �᫨ ���� �����, � �㦭� �஢���� ������, �⮡� �� �뫨 ���������!! */
	IF mType <> "" THEN
		DO:
			FIND FIRST code
				WHERE code.class = "����� ���"
				AND	  code.code = mType
				NO-LOCK NO-ERROR.
			
			IF AVAIL code AND code.val = mCurr THEN DO:
				MESSAGE "����୮ ��࠭� ����� � ��� ����!" VIEW-AS ALERT-BOX TITLE "�訡��".
				UNDO, RETRY.
			END.
		END.
		
	/* plus.vvv 29/04/2014 */
	/* �஢�ઠ ���㯪�/�த��� � ���ᮬ �� */
	
	IF mType EQ "���㯪�" OR mType EQ "�த���" THEN 
		DO:
			/* ������ ���� �� �� ������ ���� �� ��।������� ����� */
			FIND LAST xxx
				WHERE xxx.instr-cat EQ 'currency'
				AND xxx.instr-code EQ mCurr
				AND xxx.rate-type EQ '����'
				AND xxx.iratedate > DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00")
				AND xxx.iratedate < DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59")
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					IF mType EQ "���㯪�" THEN 
						IF mValue >= xxx.rate-instr THEN
							DO:
								MESSAGE "���� ���㯪� " + STRING(mCurrName) + " �� ����� �ॢ���� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					.
					/**/
					IF mType EQ "�த���" THEN 
						IF mValue <= xxx.rate-instr THEN
							DO:
								MESSAGE "���� �த��� " + STRING(mCurrName) + " �� ����� ���� ����� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					.					
				END.
		END.
		
	/* �஢�ઠ �� ���� ���� */
	
	IF mDate < DATE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 1, 10)) THEN
		DO:
			MESSAGE "����� ��⠭�������� ����� ����� ��� ����� ����!!!" VIEW-AS ALERT-BOX TITLE "�訡��".
			UNDO, RETRY.			
		END.
	
	/*------------------------------------*/
	RUN browseld.p ("branch",
				   "",
				   "",
				   "branch-type",
				   4).
	IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
	DO:
		/* ���� �� �ᥬ �⬥祭�� ���ࠧ������� */
		FOR EACH tmprecid
			NO-LOCK:
		  FIND FIRST branch WHERE 
			RECID(branch) EQ  tmprecid.id
			NO-LOCK NO-ERROR.
			/**/
			IF AVAIL branch THEN
				DO:
					/* ��ᬮ�ਬ ��� �� �� �⮬ ��� ���ᮢ �����, ��⠭�������� ������� */
					FIND LAST yyy
						WHERE yyy.instr-code = mCurr
						AND yyy.rate-type = mType
						AND yyy.branch-id  = branch.branch-id
						AND yyy.instr-cat  = "currency"
						AND yyy.iratedatetime > SF
						AND yyy.iratedatetime <= CM
					NO-LOCK NO-ERROR.
					/**/
					IF AVAIL yyy THEN DO:
						MESSAGE "���� ����� �����, ��⠭������� �� ��� ����, �� ������� �� �६���!" VIEW-AS ALERT-BOX.
						RETURN.
					END.
					
					/* ��ᬮ�ਬ ��� �� 㦥 ⠪�� ����� */
					FIND FIRST zzz
						WHERE zzz.instr-code = mCurr
						AND zzz.rate-type = mType
						AND zzz.branch-id  = branch.branch-id
						AND zzz.instr-cat  = "currency"
						AND zzz.iratedatetime = SF
					EXCLUSIVE-LOCK NO-ERROR.
					
					/* �᫨ ���� - � ��ᨬ ��������� ����� ��� ���? */
					IF AVAIL zzz THEN
						DO:
							/*
							DO ON ENDKEY UNDO, LEAVE:
							  pick-value = ?.
							  */
							mMESS = "��� ���ࠧ������� " + STRING(zzz.branch-id) + " �� ������ �६� ��⠭����� ���� " + STRING(zzz.rate-instr) + " �� ����� " + STRING(mCurr) + ". ��������?". 
							 
							MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
							TITLE "[ ������஢���� ���ᮢ ����� ]" UPDATE choiceMess AS LOGICAL.
							  
							IF choiceMess = TRUE THEN
								DO:
									ASSIGN
									zzz.rate-instr = mValue.
									/**/
									VALIDATE zzz.
								END.  
							
							IF LASTKEY EQ 27 THEN
								RETURN.
								
						END.
					ELSE
						/* ���� ���� */
						DO:
							/* ᮧ���� ������ */
							CREATE irate-time NO-ERROR.
							/* ��ᢠ����� ���祭�� */
							ASSIGN
								irate-time.instr-code = mCurr
								irate-time.rate-type  = mType
								irate-time.branch-id  = branch.branch-id
								irate-time.rate-instr = mValue
								irate-time.instr-cat  = "currency"
								irate-time.per 		  = mZZ
								irate-time.bound-summ = 0
								irate-time.iratedatetime = SF
							.
							/* ��࠭塞 */
							VALIDATE irate-time.
						END.
					/**/	
				END.
		END.
	END.
	/**/	
	IF LASTKEY EQ 27 THEN
		RETURN.
	ELSE
		MESSAGE "��⠭���� ���ᮢ �஢����� �ᯥ譮!" VIEW-AS ALERT-BOX TITLE "���ଠ��".
END.

{intrface.del}          /* ���㧪� �����㬥����.  */    
