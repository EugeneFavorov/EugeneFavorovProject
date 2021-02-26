/*
 * ���������� ���㧪� ���ଠ樨 ��� ���ਠ�
 *
 */
ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{intrface.get count}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get pack}
{intrface.get cust}

DEF INPUT PARAMETER maskLoan AS CHAR.

{cust-adr.obj
  &def-vars     = YES
}

DEF VAR tI          AS INT64 INIT 0 NO-UNDO.
DEF VAR TMP_CHEL    AS CHAR NO-UNDO.
DEF VAR ZALOG_DERZH AS INT64 INIT 0 NO-UNDO.
DEF BUFFER CCODE FOR CODE.
DEF VAR TYPE_ACC AS CHAR NO-UNDO.
DEF VAR NUMB_ACC AS CHAR NO-UNDO.
DEF VAR nn       AS INT64 INIT 0 NO-UNDO.

/*
def new shared stream vvs.
def var fname as char  init "./ved_insur.csv"  no-undo.
*/
def var delim as char init ";" format "x(1)" no-undo.
def var eol   as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

DEF TEMP-TABLE HZ NO-UNDO
	/* ����� �� �� */
	field jNN			AS INT64	/* ���浪��� ����� */
	field jCONT_CODE	AS CHAR		/* ����� �� */
	field jFILIAL_ID	AS CHAR
	field jCOMMENT		AS CHAR		/* ���ᠭ�� */
	field jOPEN_DATE	AS DATE		/* ��� ������ �� */
	field jEND_DATE		AS DATE 	/* ��� ������� �� */
	/* ���ଠ�� � ��������⥫� */
	field iTYPE			AS CHAR		/* ⨯ ������ */
	field iFIRSTNAME 	AS CHAR		/* ������� */
	field iLASTNAME 	AS CHAR		/* ��� */
	field iMIDDLENAME 	AS CHAR		/* ����⢮ */ 
	field iBIRTHDATE 	AS DATE		/* ��� ஦����� */ 
	field iDOCUMENTCODE AS CHAR		/* ��� ���� ���㬥�� */
	field iDOCUMENTNAME AS CHAR		/* ��� ���㬥�� */ 
	field iDOCUMENTSERIESNUMBER AS CHAR	/* ���� � ����� ���㬥�� */ 
	/* ���� ��������⥫� */
	field aTYPE			AS INT64 	/* ⨯ ���� */
	field aREG_CODE		AS CHAR		/* ��� ॣ���� */
	field aREG_NAME		AS CHAR		/* ������������ ॣ���� */
	field aDISTRICT		AS CHAR		/* ����� */
	field aCITY			AS CHAR		/* ��த */
	field aLOCALITY		AS CHAR		/* ��ᥫ���� �㭪� */
	field aSTREET		AS CHAR		/* ���� */
	field aHOUSE		AS CHAR		/* ��� */
	field aBUILDING		AS CHAR		/* ����� */
	field aAPARTMENT	AS CHAR		/* ������ */
	/* ���ᠭ�� ������ */
	field wZALOG_TYPE	AS INT64	/* ��� ��⮬����� - 1, ��� ��㣮�� ⨯� ������ - 2 */
	field wZALOG_ID		AS CHAR		/* ��� ��⮬����� - VIN-���, ��� ��㣮�� ⨯� ������ - ����� �����䨪��� */
	field wZALOG_DESC	AS CHAR		/* ���ᠭ�� �।��� ������ */
	field wZALOG_SF		AS INT64	/* ������ � ��ઠ  ��� ��� ���᪠ ����� - ��� �� �㦭� */
	FIELD wZALOG_SURR   AS CHAR     /* ���ண�� ������� ������ ��� ��⠭���� �� */
	/* new */
	FIELD wNOTICE		AS INT64	/* �ਭ�⨥ - 0, ���ᠭ�� - 1 */
	FIELD term-obl-surrogate AS CHAR
	FIELD NOTIFICATIONREFERENCENUMBER AS CHAR
.
DEF VAR vFile AS CHAR NO-UNDO.

vFile = 'n' + string(year(today)) + string(month(today),"99") + string(day(today),"99")
	 + REPLACE( STRING( TIME,'HH:MM:SS'), ':', '') + '.log'.
{setdest.i &filename = vFile}

DEF TEMP-TABLE ttVIN LIKE signs
		INDEX sur surrogate
		INDEX xat xattr-value
	.

{empty ttVIN}
FOR EACH signs
     WHERE signs.file-name EQ 'term-obl'
	   AND signs.code EQ "TCVIN"
	 NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
	 	IF signs.surrogate MATCHES '*@' + shFilial + '*'
	 	 THEN DO:
	    	CREATE ttVIN.
	    	BUFFER-COPY signs TO ttVIN.
    	END.
END.

IF maskLoan EQ '*' THEN
/* ����㧪� ॣ. ����஢ 㢥�������� */
FOR EACH notifications
 WHERE notifications.notificationreferencenumber NE ?
   AND notifications.contractsourcesystem EQ 1
   /* AND notifications.notificationid EQ 28819 */
 NO-LOCK,
 EACH personalproperties
  WHERE personalproperties.pledgeid EQ notifications.notificationid
    AND personalproperties.id NE ? 
    AND personalproperties.id NE ''
 NO-LOCK QUERY-TUNING( NO-INDEX-HINT):

	/* PUT UNFORMATTED notifications.notificationid " " notifications.contractnumber " " personalproperties.id SKIP. */

 FOR EACH ttVIN
 	 WHERE ttVIN.file-name EQ 'term-obl'
	   AND ttVIN.code EQ "TCVIN"
	   AND ttVIN.xattr-value EQ personalproperties.id
	   AND ttVIN.surrogate BEGINS '�।��,' + notifications.contractnumber + '@' + shFilial /* + ',' */
	 NO-LOCK:
	/* PUT UNFORMATTED ttVIN.surrogate SKIP. */
	/*IF /* AVAIL ttVIN
		AND*/  ttVIN.surrogate BEGINS '�।��,97-00-29591'
	 THEN*/ DO:
		DEF VAR ss AS CHAR NO-UNDO.
		DEF VAR ss2 AS CHAR NO-UNDO.

		ss = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog-no", "").
		ss2 = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog", "").
		IF ss <> '' AND ss <> STRING(notifications.notificationid) THEN
			PUT UNFORMATTED notifications.contractnumber ": ᮮ�饭�� 㦥 ����� ��㣮� �����" SKIP.
		IF ss2 <> '' AND ss2 <> STRING(notifications.registrationtime) + ',' + notifications.notificationreferencenumber THEN
			PUT UNFORMATTED notifications.contractnumber ": ᮮ�饭�� 㦥 ����� ��㣮� ॣ �����" SKIP.
		IF ss EQ '' THEN DO:
			UpdateSigns("term-obl-gar",ttVIN.surrogate, "reg-zalog-no",
				STRING(notifications.notificationid), ?).
		END.
		IF ss2 EQ '' THEN DO:
			/* put unformatted "YES" SKIP. */
			UpdateSigns("term-obl-gar",ttVIN.surrogate, "reg-zalog",
				STRING(notifications.registrationtime) + ',' + notifications.notificationreferencenumber, ?).
		END.
	END.
END.
END.

/* {preview.i} */
/* return. */
&IF DEFINED( SESSION-REMOTE ) = 0 &THEN
    put screen col 1 row screen-lines + message-lines + 1
		color bright-blink-normal "ᡮ� ������ � �������...".
&ELSE
	mblodd_char_Tmp03 = ?.
	RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "ᡮ� ������ � �������...", ? ).
&ENDIF
{empty HZ}
/* ᡮ� ������ � ������� */
/* ���� �� ���ᯥ祭�� = ���������� */
FOR EACH signs
	WHERE signs.file-name = 'term-obl'
	AND signs.code = '�����'
	AND signs.xattr-value = '��⮬�����'
	NO-LOCK,
	/* ��室�� �� */
	FIRST LOAN
	WHERE LOAN.CONTRACT = ENTRY(1, signs.SURROGATE)
	AND LOAN.CONT-CODE = ENTRY(2, signs.SURROGATE)
	AND LOAN.OPEN-DATE >= DATE( 7, 15, 2014)
	AND LOAN.FILIAL-ID = /*ENTRY(2, signs.SURROGATE,'@') */ shFilial
	AND LOAN.LOAN-STATUS <> '����'
 /* AND loan.cont-code EQ "01-00-14537-���@0400XXX" */
	AND loan.cont-code MATCHES maskLoan
	NO-LOCK:
		/* �த���� �।��� �ய�᪠�� */
		IF GetXAttrValueEx ("loan", loan.contract + ',' + loan.cont-code, "���������", "") NE "" THEN NEXT.

IF LOAN.CONT-CODE BEGINS '45-00-29975-' OR
   LOAN.CONT-CODE BEGINS '45-00-29901-' OR
   LOAN.CONT-CODE BEGINS '45-00-30580-' OR
   LOAN.CONT-CODE BEGINS '45-00-30729-' THEN NEXT.

		tI = tI + 1.
		/* ����� �� �६����� ⠡���� */
		CREATE HZ.
		/*----------------- ����� � �� -----------------*/
		ASSIGN
			HZ.jNN = tI + 1
			HZ.jCONT_CODE = LOAN.DOC-REF
			HZ.jFILIAL_ID = loan.filial-id
			HZ.jCOMMENT = LOAN.CONT-TYPE
			HZ.jOPEN_DATE = LOAN.OPEN-DATE
			HZ.jEND_DATE  = LOAN.END-DATE
			HZ.wNOTICE = ?
			HZ.term-obl-surrogate = SIGNS.SURROGATE
			.
		/**/
		/*----------------- ����� � ��������⥫� -----------------*/
		TMP_CHEL = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "CustSurr", "").
		/* �� */
		IF ENTRY(1, TMP_CHEL) = "�" THEN
		DO:
			/**/
			FIND FIRST PERSON
			    WHERE PERSON.PERSON-ID = INT64( ENTRY(2, TMP_CHEL) )
			    NO-LOCK NO-ERROR.
			/**/
			IF AVAIL PERSON THEN
			DO:
				/* ��� */
				HZ.iTYPE = "�".
				HZ.iLASTNAME  = PERSON.NAME-LAST.			/* ������� */
				HZ.iFIRSTNAME =	ENTRY(1, PERSON.FIRST-NAMES, " ").	/* ��� */
				HZ.iMIDDLENAME = SUBSTRING(PERSON.FIRST-NAMES, INDEX(PERSON.FIRST-NAMES, " ") + 1).	/* ����⢮ */ 
				HZ.iBIRTHDATE =	PERSON.BIRTHDAY.		/* ��� ஦����� */ 
				/**/
				FIND FIRST CCODE
				    WHERE CCODE.CLASS = "�������"
				    AND CCODE.PARENT = "�������"
				    AND CCODE.CODE = PERSON.DOCUMENT-ID
				    NO-LOCK NO-ERROR.
				/**/
				IF AVAIL CCODE THEN
					HZ.iDOCUMENTCODE = CCODE.VAL.  /* ��� ���� ���㬥�� */
				/**/
				HZ.iDOCUMENTNAME = PERSON.DOCUMENT-ID. /* ��� ���㬥�� */ 

				/* �ଠ� ��� ����� ��� ��ᯮ�� */
				IF  HZ.iDOCUMENTCODE                    EQ '21' AND
				    NUM-ENTRIES( PERSON.DOCUMENT)       EQ 3 AND
				    LENGTH( ENTRY( 1, PERSON.DOCUMENT)) EQ 2 AND
				    LENGTH( ENTRY( 2, PERSON.DOCUMENT)) EQ 2 THEN
				    HZ.iDOCUMENTSERIESNUMBER = ENTRY( 1, PERSON.DOCUMENT) + ENTRY( 2, PERSON.DOCUMENT) + ' ' + ENTRY( 3, PERSON.DOCUMENT).
					/* HZ.iDOCUMENTSERIESNUMBER = SUBSTRING(REPLACE(PERSON.DOCUMENT, " ", ""), 1, 4) + 
									" " + SUBSTRING(REPLACE(PERSON.DOCUMENT, " ", ""), 5). /* ���� � ����� ���㬥�� */
				END.*/
				ELSE HZ.iDOCUMENTSERIESNUMBER = PERSON.DOCUMENT.
				/**/
				/* ���� */
				HZ.aTYPE = 0.
				HZ.aREG_CODE = GetXAttrValueEx ("person", STRING(PERSON.PERSON-ID), "���������", ""). /* ��� ॣ���� */
				/**/
				HZ.aREG_NAME = GetCodeName( "���������", HZ.aREG_CODE).
				{cust-adr.obj
				    &addr-to-vars = YES
				    &tablefield   = "TRIM(person.address[1] + ' ' + person.address[2])"
				}
				HZ.aDISTRICT  = IF TRIM(vOblChar)   <> "" THEN TRIM( vOblChar) ELSE ?.  /* ����� */
				HZ.aCITY      = IF TRIM(vGorChar)   <> "" THEN TRIM( vGorChar) ELSE ?.  /* ��த */
				HZ.aLOCALITY  = IF TRIM(vPunktChar) <> "" THEN TRIM( vPunktChar) ELSE ?. /* ��ᥫ���� �㭪� */
				HZ.aSTREET    = IF TRIM(vUlChar)    <> "" THEN TRIM( vUlChar) ELSE ?.   /* ���� */
				HZ.aHOUSE     = IF TRIM(vDomChar)   <> "" THEN TRIM( vDomChar) ELSE ?.  /* ��� */
				HZ.aBUILDING  = IF TRIM(vKorpChar)  <> "" THEN TRIM( vKorpChar) ELSE ?. /* ����� */
				HZ.aAPARTMENT = IF TRIM(vKvChar)    <> "" THEN TRIM( vKvChar) ELSE ?.   /* ������ */
				
				/*
				IF HZ.aREG_CODE <> ? THEN 
				DO:
					/**/
					FIND FIRST CODE
					    WHERE CODE.CLASS = "���������"
					      AND CODE.PARENT = "���������"
					      AND CODE.CODE = HZ.aREG_CODE
					    NO-LOCK NO-ERROR.
					/**/
					IF AVAIL CODE THEN
					DO:
						/**/
						HZ.aREG_NAME = CODE.NAME.	/* ������������ ॣ���� */
						/**/
					END.
					/**/
					HZ.aDISTRICT = ( IF TRIM( ENTRY(2, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(2, PERSON.ADDRESS[1])) ELSE ? ).		/* ����� */
					HZ.aCITY = ( IF TRIM( ENTRY(3, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(3, PERSON.ADDRESS[1])) ELSE ? ).		/* ��த */
					HZ.aLOCALITY = ( IF TRIM( ENTRY(4, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(4, PERSON.ADDRESS[1])) ELSE ? ).		/* ��ᥫ���� �㭪� */
					HZ.aSTREET = ( IF TRIM( ENTRY(5, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(5, PERSON.ADDRESS[1])) ELSE ? ).		/* ���� */
					HZ.aHOUSE = ( IF TRIM( ENTRY(6, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(6, PERSON.ADDRESS[1])) ELSE ? ).		/* ��� */
					HZ.aBUILDING = ( IF TRIM( ENTRY(7, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(7, PERSON.ADDRESS[1])) ELSE ? ).		/* ����� */
					HZ.aAPARTMENT = ( IF TRIM( ENTRY(8, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(8, PERSON.ADDRESS[1])) ELSE ? ). 	/* ������ */
					/**/
				END.
				*/
			END.
			/**/
		END.
		/**/	
		/* �� */
		IF ENTRY(1, TMP_CHEL) = "�" THEN
		DO:
			/**/
			FIND FIRST CUST-CORP
			    WHERE CUST-CORP.CUST-ID = INT64( ENTRY(2, TMP_CHEL) )
			    NO-LOCK NO-ERROR.
			/**/
		END.
		/**/
		/*----------------- ����� �� �।��� ������ -----------------*/
		IF SIGNS.XATTR-VALUE = '��⮬�����' THEN
		DO:
			/**/
			HZ.wZALOG_TYPE = 1.
			HZ.wZALOG_SURR = signs.surrogate.
			/* ������ TCVIN */
			HZ.wZALOG_ID = GetXAttrValueEx ("term-obl", signs.surrogate, "TCVIN", "").

			/* �����������  - �� �㦭� */
			IF INDEX( CAPS(HZ.wZALOG_ID), "����") > 0 OR
			   TRIM( HZ.wZALOG_ID ) = "-" THEN
			DO:
				/* �㦭� �뫮 ? */
				HZ.wZALOG_ID = ?. 
				/**/
			END.

			/* ������ � ��ઠ */
			HZ.wZALOG_DESC = "������ ��⮬�����: " + 
			    GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCbrand", "")
			    + " " +
			    GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCmodel", "")
			    + ", ��� ���᪠:"
			    + GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCyear", "").
			DEF VAR vTCbody AS CHAR NO-UNDO.
			DEF VAR vTCmotor AS CHAR NO-UNDO.
			DEF VAR vTCchassis AS CHAR NO-UNDO.
			vTCbody    = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCbody", "").
			vTCmotor   = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCmotor", "").
			vTCchassis = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCchassis", "").
			IF vTCbody    NE "" AND NOT vTCbody BEGINS "��������"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', � �㧮��: ' + vTCbody.
			IF vTCmotor   NE "" AND NOT vTCmotor BEGINS "��������"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', ������ � � �����⥫�: ' + vTCmotor.
			IF vTCchassis NE "" AND NOT vTCchassis BEGINS "��������"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', � ���: ' + vTCchassis.
			/**/
			IF TRIM( GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCbrand", "") + GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCmodel", "") ) = ""
			 /* OR TRIM( GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCyear", "") ) = "" */
			     THEN
			DO:
				/**/
				HZ.wZALOG_SF = 1.
				/**/
			END.
			ELSE
			DO:
				/**/
				HZ.wZALOG_SF = 0.
				/**/
			END.
			/* new */
			/* �㦭� 㧭��� �뫨 �� �஢���� �� �ਭ��� �⮣� ������ ��� �� ᯨᠭ�� ? */
			/* ��।����� ��� � ����⥪� �� ���ᯥ祭�� */
			/* ��।���� ⨯ � "�����" ��� */
			TYPE_ACC = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "��������", "").
			NUMB_ACC = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "�������", "0").
			/**/
			IF NUMB_ACC <> "0" THEN
				TYPE_ACC = TYPE_ACC + NUMB_ACC.
			/* ������ ��� �� ⨯� */
			IF TYPE_ACC EQ "" THEN RELEASE LOAN-ACCT. ELSE
			FIND FIRST LOAN-ACCT
			    WHERE LOAN-ACCT.CONTRACT = LOAN.CONTRACT
			      AND LOAN-ACCT.CONT-CODE = LOAN.CONT-CODE
			      AND LOAN-ACCT.ACCT-TYPE = TYPE_ACC
			    NO-LOCK NO-ERROR.
			/* �᫨ ��諨 ��� */
			IF AVAIL LOAN-ACCT THEN
			DO:
				/* ������ �஢���� �� �।��� = �������� */
				FIND FIRST OP-ENTRY
				    WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
				      AND OP-ENTRY.OP-DATE NE ? AND CAN-FIND(FIRST OP OF OP-ENTRY WHERE OP.OP-DATE NE ?)
					  AND op-entry.filial-id = loan.filial-id
				    NO-LOCK NO-ERROR.
				/**/
				IF AVAIL OP-ENTRY AND LOAN.LOAN-STATUS <> '����' THEN
					HZ.wNOTICE =  0.
				/* ��� �� ������ = �������� */
				FIND FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
					  AND OP-ENTRY.OP-DATE NE ? AND CAN-FIND(FIRST OP OF OP-ENTRY WHERE OP.OP-DATE NE ?)
					  AND op-entry.filial-id = loan.filial-id
					NO-LOCK NO-ERROR.
				/**/
				IF AVAIL OP-ENTRY THEN
					HZ.wNOTICE =  1.
			END.
		END.
		ELSE
		DO:
			/**/
			HZ.wZALOG_TYPE = 2.
			/**/
		END.

END.

/* �஢�ઠ ���㦥���� ������ 
FOR EACH NOTIFICATIONS
 WHERE NOTIFICATIONS.CONTRACTSOURCESYSTEM EQ 1
   AND NOTIFICATIONS.STATUS_ NE 6 /* 㤠����� */
   AND NOTIFICATIONS.STATUS_ EQ 4
   AND NOTIFICATIONS.CREATETIME >= DATE(3,6,2015)
   AND NOTIFICATIONS.CREATETIME < DATE(3,11,2015)
 NO-LOCK  BY NOTIFICATIONS.CREATETIME :
    FIND FIRST LOAN 
      WHERE LOAN.DOC-REF EQ NOTIFICATIONS.CONTRACTNUMBER
        AND LOAN.FILIAL-ID EQ !!!  shFilial
        AND LOAN.CONTRACT EQ "�।��"
       NO-LOCK NO-ERROR.
    IF NOT AVAIL LOAN OR (LOAN.LOAN-STATUS NE "����" /* AND LOAN.LOAN-STATUS NE "����"*/ ) THEN DO:
	PUT UNFORMATTED
	    NOTIFICATIONS.CREATETIME " "
	    NOTIFICATIONS.CONTRACTNUMBER " "
	    (IF NOT AVAIL LOAN THEN "-" ELSE LOAN.LOAN-STATUS) " "
	    /* (IF NOT AVAIL LOAN THEN "-" ELSE STRING(LOAN.CLOSE-DATE)) */
	     SKIP.
    END.
    RELEASE LOAN.
END.
{preview.i}
RETURN.
*/

/* �뢮� */
/*
run instview.p(TEMP-TABLE HZ:HANDLE). 
*/

/* �ᯮ�� ������ */

/* ᮧ����� ����� ��� ⠡��� */
/**/
/* 1. ���㧨�� ��������� ᮮ�饭�� - ���ଠ�� � ������� - � ⠡���� NOTIFICATIONS. */
/* new */
DEF BUFFER NF FOR NOTIFICATIONS.
/**/
/* 2.  ���㧨�� ���� ��� ��������⥫� � ��������ঠ⥫� � ⠡���� PledgeAddressRF / PledgeAddressForeign.*/
DEF BUFFER AR FOR PLEDGEADDRESSRF.
DEF BUFFER AF FOR PLEDGEADDRESSFOREIGN.
/* 3. ���㧨�� ��������⥫�(-��) � ��������ঠ⥫�(-��) � ⠡���� PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, �� ���㧪� �ਢ易�� �� �����䨪��ࠬ ࠭�� ���㦥��� ����.*/
DEF BUFFER RO FOR PLEDGERUSSIANORGANIZATION.
DEF BUFFER FO FOR PLEDGEFOREIGNORGANIZATION.
DEF BUFFER PP FOR PLEDGEPRIVATEPERSON.
/* 4. �ਢ易�� ��������⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgors � 㪠������ ⨯� ��������ঠ⥫�. */
DEF BUFFER PG FOR PLEDGEMESSAGEPLEDGORS.
/* 5. �ਢ易�� ��������ঠ⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgees � 㪠������ ⨯� ��������ঠ⥫�. */
DEF BUFFER MP FOR PLEDGEMESSAGEPLEDGEES.
/* 6. ���㧨�� ����� � ������ � ⠡���� PersonalProperties, ��� ��⮬������ 㪠�뢠�� PropertyType=1 � � ���� ID 㪠�뢠�� VIN-��� ��⮬�����.*/
DEF BUFFER PJ FOR PERSONALPROPERTIES.

/* ���稪� */
DEF VAR CNT_NF AS INTEGER INIT 1 NO-UNDO.
/**/
DEF VAR CNT_AR AS INTEGER INIT 1 NO-UNDO.
DEF VAR CNT_AF AS INTEGER INIT 1 NO-UNDO.
/**/
DEF VAR CNT_RO AS INTEGER INIT 1 NO-UNDO.
DEF VAR CNT_FO AS INTEGER INIT 1 NO-UNDO.
/* DEF VAR CNT_PM AS INTEGER INIT 1 NO-UNDO. */
DEF VAR CNT_PP AS INTEGER INIT 1 NO-UNDO.
DEF VAR CNT_MP AS INTEGER INIT 1 NO-UNDO.
DEF VAR CNT_PJ AS INTEGER INIT 1 NO-UNDO.

/* ��宦����� ���祭�� ���稪�� */

&IF DEFINED( SESSION-REMOTE ) = 0 &THEN
    put screen col 1 row screen-lines + message-lines + 1
		color bright-blink-normal "��।������ ���稪��...".
&ELSE
	mblodd_char_Tmp03 = ?.
	RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "��।������ ���稪��...", ? ).
&ENDIF
/* new */
/* NOTIFICATIONS 
SELECT MAX(NOTIFICATIONS.NOTIFICATIONID)
INTO CNT_NF
FROM NOTIFICATIONS.
IF CNT_NF < 22880 THEN CNT_NF = 22880. */

/* PLEDGEADDRESSRF 
SELECT MAX(PLEDGEADDRESSRF.ADDRESSID)
  INTO CNT_AR
  FROM PLEDGEADDRESSRF. */

/* PLEDGEADDRESSFOREIGN 
SELECT MAX(PLEDGEADDRESSFOREIGN.ADDRESSID)
  INTO CNT_AF
  FROM PLEDGEADDRESSFOREIGN.*/

/* PLEDGERUSSIANORGANIZATION */
SELECT MAX(PLEDGERUSSIANORGANIZATION.CLIENTID)
  INTO CNT_RO
  FROM PLEDGERUSSIANORGANIZATION.

/* PLEDGEFOREIGNORGANIZATION 
SELECT MAX(PLEDGEFOREIGNORGANIZATION.CLIENTID)
  INTO CNT_FO
  FROM PLEDGEFOREIGNORGANIZATION.*/

/* PLEDGEPRIVATEPERSON 
SELECT MAX(PLEDGEPRIVATEPERSON.CLIENTID)
  INTO CNT_PP
  FROM PLEDGEPRIVATEPERSON.*/

/* PLEDGE *
SELECT MAX(PLEDGEMESSAGEPLEDGORS.PLEDGEID)
  INTO CNT_PM
  FROM PLEDGEMESSAGEPLEDGORS.
*/
/* PLEDGEMESSAGEPLEDGEES 
SELECT MAX(PERSONALPROPERTIES.PERSONALPROPERTYID)
  INTO CNT_PJ
  FROM PERSONALPROPERTIES.

IF CNT_PJ < 22890 THEN CNT_PJ = 22890.*/

/**/
/*output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".*/

/*
FOR EACH HZ
	/* WHERE HZ.iBIRTHDATE <> ?
	AND ( (HZ.wZALOG_ID = ? AND HZ.jOPEN_DATE < DATE(3,17,2015)) OR HZ.wZALOG_SF = 1 
	)
	AND HZ.wNOTICE <> ?*/
	NO-LOCK:
	/**/
	    put unformatted
			HZ.jNN delim
			HZ.jCONT_CODE delim
			HZ.jCOMMENT delim
			HZ.jOPEN_DATE delim
			HZ.jEND_DATE delim
			HZ.iTYPE delim
			HZ.iFIRSTNAME delim
			HZ.iLASTNAME delim
			HZ.iMIDDLENAME delim
			HZ.iBIRTHDATE delim
			HZ.iDOCUMENTCODE delim
			HZ.iDOCUMENTNAME delim
			HZ.iDOCUMENTSERIESNUMBER delim
			HZ.aTYPE delim
			HZ.aREG_CODE delim
			HZ.aREG_NAME delim
			HZ.aDISTRICT delim
			HZ.aCITY delim
			HZ.aLOCALITY delim
			HZ.aSTREET delim
			HZ.aHOUSE delim
			HZ.aBUILDING delim
			HZ.aAPARTMENT delim
			HZ.wZALOG_TYPE delim
			HZ.wZALOG_ID delim
			HZ.wZALOG_DESC delim
			(IF HZ.wZALOG_ID = ? THEN "��������� TCVIN" ELSE "���������� ����� �� ��⮬�����") delim
	  eol.
	/**/
END.
*/
/*output stream vvs close.*/

/*
 MESSAGE "������ �ᯥ譮 �����襭!" VIEW-AS ALERT-BOX.
 RUN sndbispc ("file=" + fname + ";class=bq").
*/

/* ��⠢�� ���ଠ樨 � ����� = ��������ঠ⥫� */
	/* ���� ��������ঠ⥫� */
	DO WHILE True:
		CNT_AR = NEXT-VALUE( SEQ_ADDRESSID).
		FIND FIRST AR WHERE AR.ADDRESSID EQ CNT_AR NO-LOCK NO-ERROR.
		IF NOT AVAIL AR THEN LEAVE.
	END.
	RELEASE AR.
	CREATE AR.
	ASSIGN 
/*
		AR.ADDRESSID 	= CNT_AR
		AR.REGIONCODE 	= "55"				/* ��� ॣ���� */
		AR.REGION 		= "��᪠� ���"		/* ������������ ॣ���� */
		AR.DISTRICT 	= ?					/* ����� */
		AR.CITY 		= "��� �"			/* ��த */
		AR.LOCALITY 	= ?					/* ��ᥫ���� �㭪� */
		AR.STREET 		= "������ ���"	/* ���� */
		AR.HOUSE 		= "6"				/* ��� */
		*/
		AR.ADDRESSID 	= CNT_AR            /* SEQ_ ADDRESSID */
		AR.REGIONCODE 	= "77"				/* ��� ॣ���� */
		AR.REGION 		= "�. ��᪢�"		/* ������������ ॣ���� */
		AR.DISTRICT 	= ?					/* ����� */
		AR.CITY 		= ""				/* ��த */
		AR.LOCALITY 	= ?					/* ��ᥫ���� �㭪� */
		AR.STREET 		= "�����⪮�� ���"	/* ���� */
		AR.HOUSE 		= "7 ���.1"				/* ��� */
	.
	/**/
	VALIDATE AR.
	/**/		
	/* ���ଠ�� � ��������ঠ⥫� */
	CNT_RO = CNT_RO + 1.
	/**/
	CREATE RO.
	/**/
	ASSIGN
		RO.CLIENTID  = CNT_RO
		RO.NAMEFULL  = '�㡫�筮� ��樮��୮� ����⢮ "���� ����"'
		RO.OGRN 	 = "1025500000624"
		RO.INN 	 	 = "5503016736"
		RO.ADDRESSID = CNT_AR
	.
	/**/
	VALIDATE RO.
	/**/
	/* ���������� �����䨪��� ��������ঠ⥫� */
	ZALOG_DERZH = CNT_RO.
	/*!!!!!!!!!*/

{bar-beg2.i
&BarTotal     = tI
&BarMessage   = "'������ ��⮧������...'"}	

DO /* TRANSACTION */ ON ERROR UNDO, THROW:
    FIND FIRST seance WHERE
        seance.seancedate = today AND
        seance.direct = '��ᯮ��' AND
        seance.op-kind = 'e-notary'
        NO-LOCK NO-ERROR.
    IF NOT AVAIL seance THEN DO:
    CREATE Seance.
    ASSIGN
        seance.seancedate = today
        seance.direct = '��ᯮ��'
        seance.op-kind = 'e-notary'
        .
    VALIDATE seance.

    def var vPacketID as int64 no-undo.
    def var mKind as char no-undo init "Open".
    /*
    RUN PacketCreate (INPUT  seance.seanceid,
                        INPUT  -1,
                        INPUT  -1, /*mMailUserID,*/
                        INPUT  "ExpNotariat" /*iClass*/ ,
                        OUTPUT vPacketID).
    */
END.

FIND FIRST packet
  WHERE packet.seanceid = seance.seanceid NO-LOCK NO-ERROR.
IF NOT AVAIL packet THEN DO:
    MAIN:
    DO ON ERROR UNDO, THROW:
        IF RETRY THEN DO:
            PUT UNFORMATTED "�訡�� ᮧ����� �����" SKIP.
            UNDO, RETURN ERROR.
        END.
        {pack-crt.i
         &Packet      = Packet
         &PacketID    = vPacketID
         &SeanceID    = seance.seanceid
         &MailUserID  = 0
         &State       = "'����'"
         &AbonentID   = 0
         &Kind        = mKind
         &Format      = mKind
         &ClassCode   = "'Packet'"
         &ParentID    = 0
        }
    END.
END. ELSE vPacketID = packet.packetid.

/* ���� �� �������� ��⮧������ */
/* ��⠢�塞 ����� � ⠡���� */
FOR EACH HZ
	WHERE HZ.iBIRTHDATE <> ?
	/* ������ ��ઠ */
	AND HZ.wZALOG_SF = 0
	/* TCVIN */
	AND (HZ.wZALOG_ID <> ? OR HZ.jOPEN_DATE >= DATE(3,17,2015))
	/* �ਭ�⨥ ��� ᯨᠭ�� */
	/* AND HZ.wNOTICE <> ? */
	AND (
		/* ���� ��������� ������ � 㪠����� � �� */
		(HZ.wNOTICE = 0 AND
		 NOT CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  /*AND NOTIFICATIONS.NOTIFICATIONTYPE EQ HZ.wNOTICE + 1 �� ��ࠢ�� */
			  AND NOTIFICATIONS.STATUS_ NE 6 /* 㤠����� */
			  
			)
		)  OR
		/* ��� �� �ந��諠 ������ ᯨᠭ�� ������, �뫠 ���⠭���� � �� �뫮 ᯨᠭ��  */
		(HZ.wNOTICE = 1 AND
		 (NOT CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  AND NOTIFICATIONS.NOTIFICATIONTYPE EQ HZ.wNOTICE + 1
			  AND NOTIFICATIONS.STATUS_ NE 6 /* 㤠����� */
			)) AND
		 CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  AND NOTIFICATIONS.NOTIFICATIONTYPE EQ 1
			  AND NOTIFICATIONS.STATUS_ NE 6 /* 㤠����� */
			)
		)
	    )
	NO-LOCK ON ERROR UNDO, NEXT:

	{bar2.i &BarPointer = nn}
	nn = nn + 1.
/* put unformatted "1 " HZ.term-obl-surrogate SKIP. */
	/* �᫨ ᯨᠭ�� ���� ���� ॣ����樮��� ����� */
	IF HZ.wNOTICE = 1 THEN DO:
/* 
		ss = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog-no", "").
		ss2 = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog", "").

 */
 		DEF VAR ss3 AS CHAR NO-UNDO.
 		ss3 = GetXAttrValueEx ("term-obl", HZ.term-obl-surrogate, "reg-zalog", ?).
 		IF ss3 <> ? AND NUM-ENTRIES(ss3) > 1 
 			THEN HZ.NOTIFICATIONREFERENCENUMBER = ENTRY(2,ss3).
 			ELSE HZ.NOTIFICATIONREFERENCENUMBER = ?.
 		IF HZ.NOTIFICATIONREFERENCENUMBER EQ ? THEN DO:
 			/* ��� ॣ.������ ��� �⬥�� ������ */
 			PUT UNFORMATTED HZ.term-obl-surrogate ": ��� ॣ.������ ��� �⬥�� ������" SKIP.
 			NEXT.
 		END.
		/* ���ॡ���� ���� �।����� ������ *
		DEF BUFFER GH FOR NOTIFICATIONS.
		FIND FIRST GH
		WHERE GH.CONTRACTNUMBER = SUBSTRING(HZ.jCONT_CODE, 1, 60)
		AND GH.STATUS_ = 1
		NO-LOCK NO-ERROR.
		IF NOT AVAIL GH THEN NEXT. 
		HZ.NOTIFICATIONREFERENCENUMBER 	= GH.CREATIONREFERENCENUMBER. */
	END.
	IF LENGTH(HZ.NOTIFICATIONREFERENCENUMBER) > 23 THEN DO:
		PUT UNFORMATTED HZ.term-obl-surrogate " ����� ����� " HZ.NOTIFICATIONREFERENCENUMBER " ����� 23 ᨬ�����" SKIP.
		NEXT.
	END.

/* put unformatted "2" SKIP. */
	/* 1. ���㧨�� ��������� ᮮ�饭�� - ���ଠ�� � ������� - � ⠡���� NOTIFICATIONS */
	RUN PacketCreateLink(vPacketID,  
                           "loan", 
                           STRING( '�।��' + ',' + AddFilToLoan(HZ.jCONT_CODE, HZ.jFILIAL_ID)), 
                           (IF HZ.wNOTICE = 0 THEN "Open" ELSE (IF HZ.wNOTICE = 1 THEN "Close" ELSE "")) /* mKind */ ).

	/* ᫥���騩 �����䨪��� */
	DO WHILE True:
		CNT_NF = NEXT-VALUE(SEQ_PLEDGEID).
		FIND FIRST NF WHERE NF.NOTIFICATIONID EQ CNT_NF NO-LOCK NO-ERROR.
		IF NOT AVAIL NF THEN LEAVE.
	END.
	/**/	
	RELEASE NF.

	CREATE NF.
	ASSIGN
		/* �����䨪��� */
		NF.NOTIFICATIONID				= CNT_NF /* SEQ_PLEDGEID */
		NF.CREATETIME					= NOW
		NF.MODIFYTIME					= NOW
		NF.STATUS_					    = 1 /* 1-����� 㢥�������� ( IF HZ.wNOTICE = 0 THEN 1 ELSE 2 ) */
		NF.CONTRACTSOURCESYSTEM			= 1
		NF.CONTRACTNAME 				= HZ.jCOMMENT
		NF.CONTRACTDATE 				= HZ.jOPEN_DATE
		NF.CONTRACTNUMBER 				= SUBSTRING(HZ.jCONT_CODE, 1, 60)
		NF.CONTRACTTERMOF 		   		= HZ.jEND_DATE
		NF.NOTIFICATIONAPPLICANTID 		= 1
		NF.NOTIFICATIONREFERENCENUMBER 	= ?
		NF.REGISTRATIONTIME				= ?
		NF.CREATIONREFERENCENUMBER		= ?
		NF.NOTIFICATIONTYPE				= HZ.wNOTICE + 1
	.	
	IF HZ.wNOTICE = 1 THEN DO:
		NF.CREATIONREFERENCENUMBER     = HZ.NOTIFICATIONREFERENCENUMBER.
		NF.NOTIFICATIONREFERENCENUMBER = HZ.NOTIFICATIONREFERENCENUMBER.
	END.
	/**/
	VALIDATE NF.
/* put unformatted "3" SKIP. */
		
	/* �᫨ ���� ��⮧����, � ��⠢�塞 ����� */
	IF HZ.wNOTICE = 0 THEN
		DO:
			/* 2. ���㧨�� ���� ��� ��������⥫� � ��������ঠ⥫� � ⠡���� PledgeAddressRF / PledgeAddressForeign.*/
			/* ᫥���騩 �����䨪��� */
/* put unformatted "4" SKIP. */
			IF HZ.aTYPE = 0 THEN DO:
				/**/
				DO WHILE True:
					CNT_AR = NEXT-VALUE( SEQ_ADDRESSID).
					FIND FIRST AR WHERE AR.ADDRESSID EQ CNT_AR NO-LOCK NO-ERROR.
					IF NOT AVAIL AR THEN LEAVE.
				END.
				/**/	
				RELEASE AR.
				CNT_AR = CNT_AR + 1.
				/**/
				CREATE AR.
				/**/
				ASSIGN
					AR.ADDRESSID 	= CNT_AR /* SEQ_ ADDRESSID */
					AR.REGISTRATION = "Y"
					AR.REGIONCODE 	= SUBSTRING(HZ.aREG_CODE, 1, 2)		/* ��� ॣ���� */
					AR.REGION 		= SUBSTRING(HZ.aREG_NAME, 1, 60)	/* ������������ ॣ���� */
					AR.DISTRICT 	= SUBSTRING(HZ.aDISTRICT, 1, 60)	/* ����� */
					AR.CITY 		= SUBSTRING(HZ.aCITY, 1, 60)		/* ��த */
					AR.LOCALITY 	= SUBSTRING(HZ.aLOCALITY, 1, 60)	/* ��ᥫ���� �㭪� */
					AR.STREET 		= SUBSTRING(HZ.aSTREET, 1, 60)		/* ���� */
					AR.HOUSE 		= SUBSTRING(HZ.aHOUSE, 1, 8)		/* ��� */
					AR.BUILDING 	= SUBSTRING(HZ.aBUILDING, 1, 8)		/* ����� */
					AR.APARTMENT 	= SUBSTRING(HZ.aAPARTMENT, 1, 8)	/* ������ */
				.
				/**/
				VALIDATE AR.
				/**/
			END.
			/**/
			/* 3. ���㧨�� ��������⥫�(-��) � ��������ঠ⥫�(-��) � ⠡���� PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, �� ���㧪� �ਢ易�� �� �����䨪��ࠬ ࠭�� ���㦥��� ����.*/
			
/* put unformatted "5" SKIP. */
			DO WHILE True:
				CNT_PP = NEXT-VALUE(SEQ_CLIENTID).
				FIND FIRST PP WHERE PP.CLIENTID EQ CNT_PP NO-LOCK NO-ERROR.
				IF NOT AVAIL PP THEN LEAVE.
			END.
			RELEASE PP.
			
			/* CNT_PP = CNT_PP + 1. */
			/* ����� � ��������⥫� */
			CREATE PP.
			/**/
			ASSIGN
				PP.CLIENTID = CNT_PP /* SEQ_CLIENTID */
				PP.FIRSTNAME = SUBSTRING(HZ.iFIRSTNAME, 1, 60)
				PP.LASTNAME = SUBSTRING(HZ.iLASTNAME, 1, 60)
				PP.MIDDLENAME = SUBSTRING(HZ.iMIDDLENAME, 1, 60)
				PP.BIRTHDATE = HZ.iBIRTHDATE
				PP.DOCUMENTCODE = SUBSTRING(HZ.iDOCUMENTCODE, 1, 2)
				PP.DOCUMENTNAME = SUBSTRING(HZ.iDOCUMENTNAME, 1, 255)
				PP.DOCUMENTSERIESNUMBER = SUBSTRING(HZ.iDOCUMENTSERIESNUMBER, 1, 25)
				/* �����䨪��� ���� ��������⥫� */
				PP.ADDRESSRFID = CNT_AR
			.
			/**/
			VALIDATE PP.
			/**/
			/* 4. �ਢ易�� ��������⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgors � 㪠������ ⨯� ��������ঠ⥫�.*/
			/**/
			/*CNT_PM = CNT_PM + 1.*/
/* put unformatted "6" SKIP. */
			CREATE PG.
			/**/
			ASSIGN
				/* �����䨪��� ᮮ�饭�� */
				PG.PLEDGEID = NF.NOTIFICATIONID
				/* �����䨪��� ��������⥫� */
				PG.CLIENTID = PP.CLIENTID
			.
				/* ⨯ ������ */
				IF HZ.iTYPE = "�" THEN
					PG.CLIENTTYPE = "P".
				ELSE	
					PG.CLIENTTYPE = "L".
				/**/

			/**/
			VALIDATE PG.
			/**/
			/* 5. �ਢ易�� ��������ঠ⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgees � 㪠������ ⨯� ��������ঠ⥫�. */
			/**/
			CREATE MP.
			/**/
			ASSIGN
				/* �����䨪��� ᮮ�饭�� */
				MP.PLEDGEID = NF.NOTIFICATIONID
				/* �����䨪��� ��������ঠ⥫� */
				MP.CLIENTID = ZALOG_DERZH
				/**/
				MP.CLIENTTYPE = "L"
			.
			/**/
			VALIDATE MP.
			/**/
			/* 6. ���㧨�� ����� � ������ � ⠡���� PersonalProperties, ��� ��⮬������ 㪠�뢠�� PropertyType=1 � � ���� ID 㪠�뢠�� VIN-��� ��⮬�����.*/
			
			DO WHILE True:
				CNT_PJ = NEXT-VALUE(SEQ_PERSONALPROPERTIES).
				FIND FIRST PJ WHERE PJ.PERSONALPROPERTYID EQ CNT_PJ NO-LOCK NO-ERROR.
				IF NOT AVAIL PJ THEN LEAVE.
			END.
			RELEASE PJ.
			
			/* CNT_PJ = CNT_PJ + 1. */
			/**/
/* put unformatted "7" SKIP. */
			CREATE PJ.
			/**/
			ASSIGN
				PJ.PERSONALPROPERTYID = CNT_PJ /* SEQ_PERSONALPROPERTIES */
				PJ.PLEDGEID = NF.NOTIFICATIONID
				PJ.PROPERTYTYPE = HZ.wZALOG_TYPE
				PJ.ID = SUBSTRING(HZ.wZALOG_ID, 1, 25)
				PJ.DESCRIPTION = SUBSTRING(HZ.wZALOG_DESC, 1, 200)
			.
			VALIDATE PJ.

/* put unformatted "8" SKIP. */
			IF HZ.wNOTICE = 0 THEN
 				UpdateSigns("term-obl-gar", HZ.wZALOG_SURR, "reg-zalog-no",
					STRING( NF.NOTIFICATIONID), ?).

		END.
/* put unformatted "9" SKIP. */
		PUT UNFORMATTED
		 (IF HZ.wNOTICE EQ 0 THEN "�����" ELSE "���⨥") " �� �������� " + HZ.jCONT_CODE + " �ᯮ��஢�� (id=" + STRING(CNT_NF) + ')' SKIP.
	END.
CATCH eAnyError AS Progress.Lang.Error:
 PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
END CATCH.
END.

{del-bar.i}

/*MESSAGE "������ �ᯥ譮 �����襭!" VIEW-AS ALERT-BOX.*/
{preview.i &filename=vFile}

{intrface.del}
