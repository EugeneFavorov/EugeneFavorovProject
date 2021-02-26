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
DEF VAR mDate AS DATE INIT TODAY NO-UNDO.  /* ��� */
DEF VAR mCrossRate AS LOGICAL INITIAL YES. /* ���� ���� */ 
DEF VAR mValue AS DECIMAL INITIAL 0.00 FORMAT ">>>999.9999" NO-UNDO. /* ���祭�� */ 
/**/
DEF VAR mValName1 AS CHAR INIT "�����" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName2 AS CHAR INIT "������ ���" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName3 AS CHAR INIT "���" VIEW-AS TEXT NO-UNDO.
DEF VAR mValName4 AS CHAR INIT "����" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mVal1 AS CHAR INIT "398" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal2 AS CHAR INIT "840" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal3 AS CHAR INIT "978" VIEW-AS TEXT NO-UNDO.
DEF VAR mVal4 AS CHAR INIT "156" VIEW-AS TEXT NO-UNDO.
/**/
DEF VAR mValBuy1 AS DEC NO-UNDO.
DEF VAR mValBuy2 AS DEC NO-UNDO.
DEF VAR mValBuy3 AS DEC NO-UNDO.
DEF VAR mValBuy4 AS DEC NO-UNDO.
/**/
DEF VAR mValSell1 AS DEC NO-UNDO.
DEF VAR mValSell2 AS DEC NO-UNDO.
DEF VAR mValSell3 AS DEC NO-UNDO.
DEF VAR mValSell4 AS DEC NO-UNDO.
/**/
DEF VAR mValCnt1 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt2 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt3 AS DEC INIT 1 NO-UNDO.
DEF VAR mValCnt4 AS DEC INIT 1 NO-UNDO.
/**/

DEF VAR strBr AS CHARACTER INIT "" NO-UNDO.
DEF VAR SF	AS DATETIME NO-UNDO.
DEF VAR QP AS DATETIME NO-UNDO.
DEF VAR CM AS DATETIME NO-UNDO.

DEFINE BUFFER xxx FOR instr-rate.
DEFINE BUFFER yyy FOR instr-rate.
DEFINE BUFFER zzz FOR instr-rate.

/**/

DEF VAR choiceMess AS LOGICAL INIT FALSE NO-UNDO.

DEFINE BUTTON button1 LABEL "��������".
DEFINE BUTTON button2 LABEL "�⬥����".
DEFINE VAR mMESS AS CHARACTER VIEW-AS TEXT /*FORMAT "x(20)"*/ NO-UNDO.

/**/
/*---------------------------------*/
/* �ଠ ��⠭���� ������ */

DEFINE FRAME fGet  
	/* ������ ��ப� */
	SPACE(15) 
	mDate 			LABEL			"���  "  		SKIP
	/**/
	"-------------------------------------------------------"		SKIP
	"|      �����     | ��� |  ���㯪� |  �த��� |   ��  |"		SKIP
	"-------------------------------------------------------"		SKIP	
	"|" SPACE(1) mValName1 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal1 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy1 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell1	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt1	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP	
	/**/
	"|" SPACE(1) mValName2 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal2 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy2 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell2	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt2	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/
	"|" SPACE(1) mValName3 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal3 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy3 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell3	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt3	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/
	"|" SPACE(1) mValName4 	NO-LABEL	 	FORMAT "x(15)" 			SPACE(1) 
	"|" SPACE(1) mVal4 		NO-LABEL 		FORMAT "x(3)" 			SPACE(1) 
	"|" SPACE(1) mValBuy4 	NO-LABEL		FORMAT "999.9999" 		SPACE(1)
	"|" SPACE(1) mValSell4	NO-LABEL		FORMAT "999.9999"		SPACE(1)
	"|" SPACE(1) mValCnt4	NO-LABEL		FORMAT "999.9"			SPACE(1)
	"|" 															SKIP
	"-------------------------------------------------------"		SKIP
	/**/	
	WITH /*WIDTH 62 */ /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 6
        TITLE COLOR BRIGHT-WHITE "[ ��⠭���� ���������� ���ᮢ ����� ]" .

/**/
DEFINE FRAME Qwy
	mMESS		NO-LABEL /*FORMAT "X(25)"*/ SKIP(3)
	button1 	SPACE(5)
	button2
WITH /*2 COL 1 down*/ OVERLAY SIDE-LABEL CENTERED ROW 9 
TITLE COLOR BRIGHT-WHITE "[ ������஢���� ���ᮢ ����� ]".	
		
/*---------------------------------*/
ON F1 OF mDate IN FRAME fGet  
DO:
   pick-value = "".
   DO WITH FRAME fGet :
      DO TRANSACTION:
		/**/
        RUN calend.p.
		/**/
		IF (lastkey eq 13 or lastkey eq 10) and pick-value ne ? then
			SELF:SCREEN-VALUE = string(date(pick-value), "99/99/9999").		
		/**/
      END.
   END.
   RETURN NO-APPLY.
END.

/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  PAUSE 0.
		UPDATE
			mDate
			/**/
			mValName1
			mVal1
			mValBuy1
			mValSell1
			mValCnt1
			/**/
			mValName2
			mVal2
			mValBuy2
			mValSell2
			mValCnt2
			/**/
			mValName3
			mVal3
			mValBuy3
			mValSell3
			mValCnt3			
			/**/
			mValName4
			mVal4
			mValBuy4
			mValSell4
			mValCnt4			
			/**/			
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* �㦭� �஢���� 㪠����� ���祭�� */
	/*------------------------------------*/
	/* �஢�ઠ �� ���� ���� */
	IF mDate < TODAY THEN
		DO:
			MESSAGE "����� ��⠭�������� ��������� ����� ����� ��� ����� ����!!!" VIEW-AS ALERT-BOX TITLE "�訡��".
			UNDO, RETRY.			
		END.
		
	/* �� ���祭�� �㫥�� */	
	IF mValBuy1 = 0 AND mValBuy2 = 0 AND mValBuy3 = 0 AND mValBuy4 = 0
		AND mValSell1 = 0 AND mValSell2 = 0 AND mValSell3 = 0 AND mValSell4 = 0 THEN
		DO:
			MESSAGE "�� ��⠭����� �� ���� ���� �����!!!" VIEW-AS ALERT-BOX TITLE "�訡��".
			UNDO, RETRY.		
		END.

		
	/* plus.vvv 29/04/2014 */
	/* �஢�ઠ ���㯪�/�த��� � ���ᮬ �� */
	/*
	/* 1. ����� */
	/* ������ ���� �� �� ������ ���� �� ��।������� ����� */
	IF mValBuy1 > 0 OR mValSell1 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal1
				AND xxx.rate-type EQ "����"
				AND xxx.since EQ mDATE
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy1 >= xxx.rate-instr AND mValBuy1 > 0 THEN
							DO:
								MESSAGE "���� ���㯪� " + STRING(mValName1) + " �� ����� �ॢ���� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell1 <= xxx.rate-instr AND mValSell1 > 0 THEN
							DO:
								MESSAGE "���� �த��� " + STRING(mValName1) + " �� ����� ���� ����� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.			
				END.
		END.

	/* 2. ������ ��� */
	/* ������ ���� �� �� ������ ���� �� ��।������� ����� */
	IF mValBuy2 > 0 OR mValSell2 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal2
				AND xxx.rate-type EQ "����"
				AND xxx.since EQ mDATE
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy2 >= xxx.rate-instr AND mValBuy2 > 0 THEN
							DO:
								MESSAGE "���� ���㯪� " + STRING(mValName2) + " �� ����� �ॢ���� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell2 <= xxx.rate-instr AND mValSell2 > 0 THEN
							DO:
								MESSAGE "���� �த��� " + STRING(mValName2) + " �� ����� ���� ����� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.			
				END.
		END.

	/* 3. ��� */
	/* ������ ���� �� �� ������ ���� �� ��।������� ����� */
	IF mValBuy3 > 0 OR mValSell3 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal3
				AND xxx.rate-type EQ "����"
				AND xxx.since EQ mDATE
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy3 >= xxx.rate-instr AND mValBuy3 > 0 THEN
							DO:
								MESSAGE "���� ���㯪� " + STRING(mValName3) + " �� ����� �ॢ���� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell3 <= xxx.rate-instr AND mValSell3 > 0 THEN
							DO:
								MESSAGE "���� �த��� " + STRING(mValName3) + " �� ����� ���� ����� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.			
				END.	
		END.
		
	/* 4. ���� */
	/* ������ ���� �� �� ������ ���� �� ��।������� ����� */
	IF mValBuy4 > 0 OR mValSell4 > 0 THEN 
		DO:
			/**/
			FIND LAST xxx
				WHERE xxx.instr-cat EQ "currency"
				AND xxx.instr-code EQ mVal4
				AND xxx.rate-type EQ "����"
				AND xxx.since EQ mDATE
			NO-LOCK NO-ERROR.
			
			IF AVAIL xxx THEN 
				DO:
					/**/
						IF mValBuy4 >= xxx.rate-instr AND mValBuy4 > 0 THEN
							DO:
								MESSAGE "���� ���㯪� " + STRING(mValName4) + " �� ����� �ॢ���� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.
					/**/
						IF mValSell4 <= xxx.rate-instr AND mValSell4 > 0 THEN
							DO:
								MESSAGE "���� �த��� " + STRING(mValName4) + " �� ����� ���� ����� ���� �� " + STRING(xxx.rate-instr) VIEW-AS ALERT-BOX TITLE "�訡��".
								UNDO, RETRY.							
							END.			
				END.	
		END.	
	*/	
	/*------------------------------------*/
	/**/
	IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN 
	DO:
		/**/
		RUN SetRatesWTF(INPUT "840",INPUT "�������", INPUT mValBuy2,INPUT mValCnt2).
		RUN SetRatesWTF(INPUT "840",INPUT "������", INPUT mValSell2,INPUT mValCnt2).
		RUN SetRatesWTF(INPUT "978",INPUT "�������", INPUT mValBuy3,INPUT mValCnt3).
		RUN SetRatesWTF(INPUT "978",INPUT "������", INPUT mValSell3,INPUT mValCnt3).
		/**/
		RUN SetRatesWTF(INPUT "156",INPUT "�������", INPUT mValBuy4,INPUT mValCnt4).
		RUN SetRatesWTF(INPUT "156",INPUT "������", INPUT mValSell4,INPUT mValCnt4).
		RUN SetRatesWTF(INPUT "398",INPUT "�������", INPUT mValBuy1,INPUT mValCnt1).
		RUN SetRatesWTF(INPUT "398",INPUT "������", INPUT mValSell1,INPUT mValCnt1).
		/**/
	END.
	/**/	
	IF LASTKEY EQ 27 THEN
		RETURN.
	ELSE
		MESSAGE "��⠭���� ���ᮢ �஢����� �ᯥ譮!" VIEW-AS ALERT-BOX TITLE "���ଠ��".
END.

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
