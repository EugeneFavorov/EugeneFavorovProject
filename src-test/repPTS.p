{globals.i}
{intrface.get tmess}
{intrface.get xclass}

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: 
      Comment: ����� ���
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/
DEF VAR sumOPL AS DEC INIT 0 NO-UNDO.
/**/

def temp-table ZZ NO-UNDO
  field CONT_CODE	AS CHAR
  field FIO			AS CHAR
  field p519		AS DEC
  field rem40817	AS DEC
.
{empty ZZ}

DEF BUFFER LL FOR LOAN.
DEF BUFFER AP FOR ACCT-POS.
DEF BUFFER CV FOR LOAN-VAR.

def var fname as char  init "./avto_ru.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def new shared stream vvs.

/* ��室�� ������������ ������ */
FUNCTION GetName RETURNS CHAR
	(
	cat AS CHARACTER,
	id AS INT64
	):
	/**/
	DEF VAR sNAME AS CHAR NO-UNDO.
	/**/
	IF cat = "�" THEN
		DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL PERSON THEN
				/* ��� ������ */
				sNAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
	ELSE
		DO:
			FIND FIRST CUST-CORP 
			WHERE CUST-CORP.CUST-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL CUST-CORP THEN
				/* ������������ �࣠����樨 */
				sNAME = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		END.
	/**/
	RETURN sNAME.
	/**/
END.

{getdates.i}
{spinner.i "���� ��..."}
/* ���� �� 40817 */
FOR EACH LOAN-ACCT
	WHERE LOAN-ACCT.CONTRACT = '�।��'
	AND ( LOAN-ACCT.ACCT-TYPE = '�।����' OR LOAN-ACCT.ACCT-TYPE = '�।����1' )
	AND SUBSTRING(LOAN-ACCT.ACCT, 1, 5) = '40817'
	/* ��� �஢���� �� 㪠����� ��ਮ� */
	AND NOT CAN-FIND
		(
		FIRST OP-ENTRY
		WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
		AND OP-ENTRY.ACCT-CR MATCHES("70601*101101*")
		AND OP-ENTRY.OP-DATE >= beg-date
		AND OP-ENTRY.OP-DATE <= end-date
		)
	/* �� ������� �� */
	AND CAN-FIND
		(
		FIRST LOAN 
		WHERE LOAN.CONTRACT = LOAN-ACCT.CONTRACT
		AND LOAN.CONT-CODE = LOAN-ACCT.CONT-CODE
		AND LOAN.CLOSE-DATE = ?
		)
	/**/
	NO-LOCK,
			/* ������ ���祭�� 519 */
			FIRST SIGNS
			WHERE SIGNS.FILE-NAME = 'loan'
			AND SIGNS.SURROGATE = LOAN-ACCT.CONTRACT + "," + LOAN-ACCT.CONT-CODE
			AND SIGNS.CODE = 'interest[51]'
			AND SIGNS.DEC-VALUE <> 0
			:
				/**/
				sumOPL = 0.
				/* �����⠥� �㬬� ������ �� ��ਮ� */
				FOR EACH LOAN-INT
				WHERE LOAN-INT.CONTRACT = LOAN-ACCT.CONTRACT
				AND LOAN-INT.CONT-CODE = LOAN-ACCT.CONT-CODE
				AND (LOAN-INT.ID-D = 526 OR LOAN-INT.ID-D = 5 )
				AND LOAN-INT.ID-K = 519
				AND LOAN-INT.MDATE >= beg-date
				AND LOAN-INT.MDATE <= end-date
				NO-LOCK:
					/**/
					sumOPL = sumOPL + LOAN-INT.AMT-RUB. 
					/**/
				END.
				/**/
				/* ���⮪ �� 40817 */
				FIND LAST ACCT-POS
				WHERE ACCT-POS.ACCT = LOAN-ACCT.ACCT
				AND ACCT-POS.CURRENCY = LOAN-ACCT.CURRENCY
				AND ACCT-POS.SINCE <= end-date
				NO-LOCK NO-ERROR.
				/**/
					IF AVAIL ACCT-POS AND ACCT-POS.BALANCE < 0 AND SIGNS.DEC-VALUE - sumOPL <> 0 THEN
						DO:
							/**/
							CREATE ZZ.
							/**/
							ASSIGN
								ZZ.CONT_CODE = LOAN-ACCT.CONT-CODE
								ZZ.p519 = SIGNS.DEC-VALUE - sumOPL
							.
							/* ������������ ������ */
							FIND FIRST LL 
							WHERE LL.CONTRACT = LOAN-ACCT.CONTRACT
							AND LL.CONT-CODE = LOAN-ACCT.CONT-CODE
							NO-LOCK NO-ERROR.
								/**/
								IF AVAIL LL THEN
									DO:
										/**/
										ZZ.FIO = GetName(LL.CUST-CAT, LL.CUST-ID).
										/**/
									END.
							/* ���⮪ �� 40817 */
							FIND LAST AP
							WHERE AP.ACCT = LOAN-ACCT.ACCT
							AND AP.CURRENCY = LOAN-ACCT.CURRENCY
							AND AP.SINCE <= end-date
							AND AP.BALANCE < 0
							NO-LOCK NO-ERROR.
								/**/
								IF AVAIL AP THEN
									DO:
										/**/
										ZZ.rem40817 = ABS( AP.BALANCE ).
										/**/
									END.
							/**/	
						END.
END.

/* �뢮� */
IF AVAIL ZZ THEN 
	DO:
		/**/
		fname = "./pts_credits_"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
			/**/
			put stream vvs unformatted
			"���" delim
			"� ��" delim
			"��ࠬ��� 519" delim
			"���⮪ �� 40817"
			eol.
			/**/
			FOR EACH ZZ
			NO-LOCK:
						/**/
						put stream vvs unformatted
						ZZ.FIO delim
						ZZ.CONT_CODE delim
						ZZ.p519 delim
						ZZ.rem40817
						eol.
						/**/
			END.	

		output stream vvs close.

		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
	END.
ELSE
	MESSAGE "��祣� �� �������!" VIEW-AS ALERT-BOX.

