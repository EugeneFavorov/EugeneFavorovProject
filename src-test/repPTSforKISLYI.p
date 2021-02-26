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
DEF BUFFER FF FOR PL_INDOCSREG.
/**/
def temp-table ZZ NO-UNDO
  field CONT_CODE	AS CHAR		/* + � �।�⭮�� ������� */
  field FIO			AS CHAR		/* + ��� ����騪� */
  field OPEN_DATE	AS DATE		/* + ��� �।�⭮�� ������� */
  field SROK_PTS	AS DATE		/* + �ப ᤠ� ��� */
  field POST_BANK	AS DATE		/* + ��� ����㯫���� ��� � ���� */
  field U_POSTB		AS CHAR		/* + �� ᮧ��� ᮡ�⨥ <���⁠��> */
  field Z_POSTB		AS DATETIME	/* + ��� ᮧ����� ᮡ��� <���⁠��> */
  field SUM_PROV	AS DEC		/* + �㬬� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */
  field PROV_DATE	AS DATE  	/* + ��� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */
.
/**/
{empty ZZ}
/**/
DEF VAR TMP_SM 	AS DEC	INIT 0 NO-UNDO.
DEF VAR TMP_ST 	AS DEC	INIT 0 NO-UNDO.
DEF VAR EVENT1 	AS CHAR 	INIT '�ப����' 	NO-UNDO.
DEF VAR EVENT2 	AS CHAR 	INIT '���⁠��'	 	NO-UNDO.
DEF VAR TMP_DATE AS DATE  NO-UNDO.

def var fname as char  init "./ff.csv"  no-undo.
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

/* ��� ���짮��⥫� */
FUNCTION Get_UserName RETURN CHAR
	(
	USR AS CHAR
	):
	/**/
	FIND FIRST _USER
	WHERE _USER._USERID EQ USR
	NO-LOCK NO-ERROR.
		/**/
		IF AVAIL _USER THEN
			DO:
				/**/
				RETURN _USER._USER-NAME.
				/**/
			END.
		ELSE
			RETURN ?.
END.
	

{getdates.i}
{spinner.i "���� ��..."}
/* ���� �� 40817 */
FOR EACH LOAN-ACCT
	WHERE LOAN-ACCT.CONTRACT = '�।��'
	AND LOAN-ACCT.CONT-CODE MATCHES("*��*")
	AND ( LOAN-ACCT.ACCT-TYPE = '�।����' OR LOAN-ACCT.ACCT-TYPE = '�।����1' )
	AND SUBSTRING(LOAN-ACCT.ACCT, 1, 5) = '40817'
	/* ���� �஢���� ᯨᠭ�� ���� �� ��� */
	AND CAN-FIND
		(
		FIRST OP-ENTRY
		WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
		AND OP-ENTRY.ACCT-CR MATCHES("70601*7101101*")
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
	/* ���� ᮡ�⨥ 1 */
	AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		)
	/* ���� ᮡ�⨥ 2 */
	AND CAN-FIND
		(
		FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT2
		)
		NO-LOCK:
		
		/**/
		FIND FIRST PL_INDOCSREG
		WHERE PL_INDOCSREG.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
		AND PL_INDOCSREG.EVENT = EVENT1
		NO-LOCK NO-ERROR.
			/**/
			IF AVAIL PL_INDOCSREG THEN
				DO:
					/**/
					FIND FF
					WHERE FF.SURROGATE = LOAN-ACCT.CONT-CODE + " 0"
					AND FF.EVENT = EVENT2
					NO-LOCK NO-ERROR.					
					/**/
						/**/
						IF AVAIL FF THEN
							DO:
								/* �᫮��� ᮡ�⨩  = �᫨ �� ���६� ᤥ���, �� �뫨 �஢���� ᯨᠭ�� �� ���� */
								IF PL_INDOCSREG.DATE_VALUE >= FF.DATE_VALUE THEN
									DO:
										TMP_SM = 0.
										/* ������ �㬬� �஢���� */
										FOR EACH OP-ENTRY
										WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
										AND OP-ENTRY.ACCT-CR MATCHES("70601*7101101*")
										AND OP-ENTRY.OP-DATE >= beg-date
										AND OP-ENTRY.OP-DATE <= end-date
										NO-LOCK:
											/**/
											TMP_SM = TMP_SM + OP-ENTRY.AMT-RUB.
											TMP_DATE = OP-ENTRY.OP-DATE.
											/**/
										END.
										/**/
										TMP_ST = 0.
										/* � ⠪�� �㬬� ��୨����� �஢���� */
										FOR EACH OP-ENTRY
										WHERE OP-ENTRY.ACCT-DB MATCHES("70601*7101101*")
										AND OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
										AND OP-ENTRY.OP-DATE >= beg-date
										AND OP-ENTRY.OP-DATE <= end-date
										NO-LOCK:
											/**/
											TMP_ST = TMP_ST + OP-ENTRY.AMT-RUB.
											/**/
										END.
										/**/
										IF TMP_ST <> TMP_SM THEN
											DO:
												/**/
												CREATE ZZ.
												/**/
												ASSIGN
													ZZ.CONT_CODE = LOAN-ACCT.CONT-CODE  	/* + � �।�⭮�� ������� */
													ZZ.SUM_PROV = TMP_SM					/* + �㬬� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */
													ZZ.SROK_PTS = PL_INDOCSREG.DATE_VALUE	/* + �ப ᤠ� ��� */
													ZZ.POST_BANK = FF.DATE_VALUE			/* + ��� ����㯫���� ��� � ���� */
													ZZ.PROV_DATE = TMP_DATE					/* + ��� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */
													ZZ.U_POSTB = Get_UserName(FF.USER_ID)	/* + �� ᮧ��� ᮡ�⨥ <���⁠��> */
													ZZ.Z_POSTB = FF.CREATE_DATE				/* + ��� ᮧ����� ᮡ��� <���⁠��> */
												.
												/**/
												FIND LOAN
												WHERE LOAN.CONTRACT = LOAN-ACCT.CONTRACT
												AND LOAN.CONT-CODE = LOAN-ACCT.CONT-CODE
												NO-LOCK NO-ERROR.
													/**/
													IF AVAIL LOAN THEN
														DO:
															/**/
															ZZ.OPEN_DATE = LOAN.OPEN-DATE.					/* + ��� �।�⭮�� ������� */
															ZZ.FIO = GetName(LOAN.CUST-CAT, LOAN.CUST-ID).	/* + ��� ����騪� */
															/**/
														END.
													/**/
												/**/
											END.
										/**/
									END.
							END.
				END.
		/**/
END.

/*
run instview.p(TEMP-TABLE ZZ:HANDLE).
*/

/* �뢮� */
IF AVAIL ZZ THEN 
	DO:
		/**/
		fname = "./pts_peni_"  + REPLACE(string(today), "/","_") + ".csv".

		output stream vvs to value (fname)
		UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
			/**/
			put stream vvs unformatted
			"��� ����騪� " delim
			"� ��" delim
			"��� ��" delim
			"�ப ᤠ� ���" delim
			"��� ����㯫���� ��� � ����" delim
			"�� ᮧ��� ᮡ�⨥ <���⁠��" delim
			"��� ᮧ����� ᮡ��� <���⁠��>" delim
			"�㬬� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601)" delim
			"��� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601)" delim
			eol.
			/**/
			FOR EACH ZZ
			NO-LOCK:
						/**/
						put stream vvs unformatted
						ZZ.FIO delim
						ZZ.CONT_CODE delim
						STRING(ZZ.OPEN_DATE, "99.99.9999") delim
						STRING(ZZ.SROK_PTS, "99.99.9999") delim
						STRING(ZZ.POST_BANK, "99.99.9999") delim
						STRING(ZZ.U_POSTB) delim
						STRING(ZZ.Z_POSTB, "99.99.9999 HH:MM:SS") delim
						ZZ.SUM_PROV delim
						STRING(ZZ.PROV_DATE, "99.99.9999")
						eol.
						/**/
						/*
						  field CONT_CODE	AS CHAR		/* + � �।�⭮�� ������� */
						  field FIO			AS CHAR		/* + ��� ����騪� */
						  field OPEN_DATE	AS DATE		/* + ��� �।�⭮�� ������� */
						  field SROK_PTS	AS DATE		/* + �ப ᤠ� ��� */
						  field POST_BANK	AS DATE		/* + ��� ����㯫���� ��� � ���� */
						  field U_POSTB		AS CHAR		/* + �� ᮧ��� ᮡ�⨥ <���⁠��> */
						  field Z_POSTB		AS DATETIME	/* + ��� ᮧ����� ᮡ��� <���⁠��> */
						  field SUM_PROV	AS DEC		/* + �㬬� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */
						  field PROV_DATE	AS DATE  	/* + ��� �஢���� �� ᯨᠭ�� ���� (�� 40817 �� 70601) */						
						*/
			END.	

		output stream vvs close.

		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
	END.
ELSE
	MESSAGE "��祣� �� �������!" VIEW-AS ALERT-BOX.

