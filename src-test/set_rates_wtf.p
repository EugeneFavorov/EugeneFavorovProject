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

    
DEF VAR mBranch AS CHARACTER  NO-UNDO.  /* ���ࠧ������� */
DEF VAR mCurr	AS CHARACTER  NO-UNDO.  /* ����� 1 */
DEF VAR mCurrName AS CHARACTER NO-UNDO.  /* ������������ ������ 1 */
DEF VAR mDate AS DATE INIT TODAY NO-UNDO.  /* ��� */
DEF VAR mType AS CHAR INIT "���樠���" VIEW-AS TEXT NO-UNDO.  /* ⨯ ����樨 */
DEF VAR mCrossRate AS LOGICAL INITIAL YES. /* ���� ���� */ 
DEF VAR mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* ���祭�� */ 
DEF VAR mZZ AS DECIMAL INITIAL 1.00 FORMAT ">>>999.99" NO-UNDO. /* �� ᪮�쪮 */ 

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEF BUFFER zzz FOR instr-rate.
/**/

DEF VAR choiceMess AS LOGICAL INIT FALSE NO-UNDO.

DEF BUTTON button1 LABEL "��������".
DEF BUTTON button2 LABEL "�⬥����".
DEF VAR mMESS AS CHARACTER VIEW-AS TEXT /*FORMAT "x(20)"*/ NO-UNDO.

/**/
/* ��� �⮣� �� ࠡ�⠥� �롮� ������ */
RUN SetSysConf IN h_base("IdBranch2",shfilial).

/*---------------------------------*/
/* �ଠ ��⠭���� ������ */

DEF FRAME fGet  
   mCurr 			LABEL 			"�����     " 	FORMAT "X(3)"
   mCurrName		NO-LABEL						FORMAT "X(20)"		  	SKIP 
   mDate 			LABEL			"���       " 							SKIP
   mType			LABEL			"��� ����  " 	FORMAT "x(12)" 			SKIP
   mValue			LABEL			"���祭��   " 							SKIP
   mZZ				LABEL			"��         " 							SKIP

   WITH WIDTH 40 /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ ��⠭���� ���������� ���ᮢ ����� ]" .

/**/
DEF FRAME Qwy
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
	  /*
         RUN vokcurx.p (mType:SCREEN-VALUE,"",4).
	*/
		 RUN browseld.p("currency",
               "currency"         + CHR(1) +
               "instr-cat"        + CHR(1) +
               "rate-type"        + CHR(1) +
               "oth5",

               "!" + "810" + ',*' + CHR(1) +
               "currency"             + CHR(1) +
               "���樠���"              + CHR(1) +
               "no",

               "instr-cat" + CHR(1) +
               "rate-type",
               1).
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
/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mCurr
			mDate
			mType
			mValue
			mZZ
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* �㦭� �஢���� 㪠����� ���祭�� */
	/**/	
	IF mValue = 0 THEN DO:
		MESSAGE "���祭�� ������ ���� �⫨筮 �� ���!" VIEW-AS ALERT-BOX TITLE "�訡��".
		UNDO, RETRY.
	END.
	
	IF mZZ <> 1 AND mZZ <> 100 THEN DO:
		MESSAGE "���祭�� ���-�� ������ ���� 1, ���� 100." VIEW-AS ALERT-BOX TITLE "�訡��".
		UNDO, RETRY.
	END.
		
	/* �஢�ઠ �� ���� ���� */
	
	IF mDate < TODAY THEN
		DO:
			MESSAGE "����� ��⠭�������� ����� ����� ��� ����� ����!!!" VIEW-AS ALERT-BOX TITLE "�訡��".
			UNDO, RETRY.			
		END.
	
	/*------------------------------------*/
	IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
	DO:
		/**/
		RUN SetRatesWTF(INPUT mCurr,INPUT mType, INPUT mValue,INPUT mZZ).
		/**/
	END.
	/**/	
	IF LASTKEY EQ 27 THEN
		RETURN.
	ELSE
		MESSAGE "��⠭���� ���ᮢ �஢����� �ᯥ譮!" VIEW-AS ALERT-BOX TITLE "���ଠ��".
END.

/*---------------------------------------------*/
PROCEDURE SetRatesWTF:
	DEF INPUT PARAMETER val_code  AS CHARACTER.
    DEF INPUT PARAMETER rate_type AS CHARACTER.
	DEF INPUT PARAMETER set_value AS DECIMAL.
	DEF INPUT PARAMETER per_value AS DECIMAL.
	
		/**/
		IF set_value > 0 THEN 
			DO:
				/**/
				FIND FIRST zzz
					WHERE zzz.instr-code = val_code
					AND zzz.rate-type = rate_type
					AND zzz.instr-cat  = "currency"
					AND zzz.since = mDATE
				EXCLUSIVE-LOCK NO-ERROR.
				
				/* �᫨ ���� - � ��ᨬ ��������� ����� ��� ���? */
				IF AVAIL zzz THEN
					DO:
						mMESS = "�� ����� ���� ��⠭����� ���� " + STRING(zzz.rate-instr) + " �� ����� " + STRING(val_code) + ". ��������?". 
						 
						MESSAGE mMESS VIEW-AS ALERT-BOX BUTTONS YES-NO
						TITLE "[ ������஢���� ���ᮢ ����� ]" UPDATE choiceMess.
						  
						IF choiceMess = TRUE THEN
							DO:
								ASSIGN
								zzz.rate-instr = set_value.
								/**/
								VALIDATE zzz.
								/**/
							END.  
						
						IF LASTKEY EQ 27 THEN
							RETURN.
							
					END.
				ELSE
					/* ���� ���� */
					DO:
						/* ᮧ���� ������ */
						CREATE instr-rate NO-ERROR.
						/* ��ᢠ����� ���祭�� */
						ASSIGN
							instr-rate.instr-code = val_code
							instr-rate.rate-type  = rate_type
							instr-rate.rate-instr = set_value
							instr-rate.instr-cat  = "currency"
							instr-rate.per 		  = per_value
							instr-rate.since 	  = mDATE
						.
						/* ��࠭塞 */
						VALIDATE instr-rate.
						/**/
					END.
				/**/
			END.
		/**/
END.
{intrface.del}          /* ���㧪� �����㬥����.  */    
