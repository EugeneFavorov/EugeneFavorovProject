/*
 * ежедневная выгрузка информации для нотариата
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
	/* данные по КД */
	field jNN			AS INT64	/* порядковый номер */
	field jCONT_CODE	AS CHAR		/* номер КД */
	field jFILIAL_ID	AS CHAR
	field jCOMMENT		AS CHAR		/* описание */
	field jOPEN_DATE	AS DATE		/* дата открытия КД */
	field jEND_DATE		AS DATE 	/* дата закрытия КД */
	/* информация о залогодателе */
	field iTYPE			AS CHAR		/* тип клиента */
	field iFIRSTNAME 	AS CHAR		/* Фамилия */
	field iLASTNAME 	AS CHAR		/* Имя */
	field iMIDDLENAME 	AS CHAR		/* Отчество */ 
	field iBIRTHDATE 	AS DATE		/* Дата рождения */ 
	field iDOCUMENTCODE AS CHAR		/* Код вида документа */
	field iDOCUMENTNAME AS CHAR		/* Вид документа */ 
	field iDOCUMENTSERIESNUMBER AS CHAR	/* Серия и номер документа */ 
	/* адрес залогодателя */
	field aTYPE			AS INT64 	/* тип адреса */
	field aREG_CODE		AS CHAR		/* код региона */
	field aREG_NAME		AS CHAR		/* наименование региона */
	field aDISTRICT		AS CHAR		/* Район */
	field aCITY			AS CHAR		/* Город */
	field aLOCALITY		AS CHAR		/* Населенный пункт */
	field aSTREET		AS CHAR		/* Улица */
	field aHOUSE		AS CHAR		/* Дом */
	field aBUILDING		AS CHAR		/* Корпус */
	field aAPARTMENT	AS CHAR		/* Квартира */
	/* описание залога */
	field wZALOG_TYPE	AS INT64	/* Для автомобиля - 1, для другого типа залога - 2 */
	field wZALOG_ID		AS CHAR		/* Для автомобиля - VIN-код, для другого типа залога - некий идентификатор */
	field wZALOG_DESC	AS CHAR		/* Описание предмета залога */
	field wZALOG_SF		AS INT64	/* Модель и марка  или год выпуска пустые - нам не нужно */
	FIELD wZALOG_SURR   AS CHAR     /* суррогат договора залога для установки ДР */
	/* new */
	FIELD wNOTICE		AS INT64	/* Принятие - 0, Списание - 1 */
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
/* загрузка рег. номеров уведомлений */
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
	   AND ttVIN.surrogate BEGINS 'Кредит,' + notifications.contractnumber + '@' + shFilial /* + ',' */
	 NO-LOCK:
	/* PUT UNFORMATTED ttVIN.surrogate SKIP. */
	/*IF /* AVAIL ttVIN
		AND*/  ttVIN.surrogate BEGINS 'Кредит,97-00-29591'
	 THEN*/ DO:
		DEF VAR ss AS CHAR NO-UNDO.
		DEF VAR ss2 AS CHAR NO-UNDO.

		ss = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog-no", "").
		ss2 = GetXAttrValueEx ("term-obl", ttVIN.SURROGATE, "reg-zalog", "").
		IF ss <> '' AND ss <> STRING(notifications.notificationid) THEN
			PUT UNFORMATTED notifications.contractnumber ": сообщение уже имеет другой номер" SKIP.
		IF ss2 <> '' AND ss2 <> STRING(notifications.registrationtime) + ',' + notifications.notificationreferencenumber THEN
			PUT UNFORMATTED notifications.contractnumber ": сообщение уже имеет другой рег номер" SKIP.
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
		color bright-blink-normal "сбор данных о залогах...".
&ELSE
	mblodd_char_Tmp03 = ?.
	RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "сбор данных о залогах...", ? ).
&ENDIF
{empty HZ}
/* сбор данных о залогах */
/* идем по обеспечению = АВТОМОБИЛЬ */
FOR EACH signs
	WHERE signs.file-name = 'term-obl'
	AND signs.code = 'ВидОб'
	AND signs.xattr-value = 'Автомобиль'
	NO-LOCK,
	/* находим КД */
	FIRST LOAN
	WHERE LOAN.CONTRACT = ENTRY(1, signs.SURROGATE)
	AND LOAN.CONT-CODE = ENTRY(2, signs.SURROGATE)
	AND LOAN.OPEN-DATE >= DATE( 7, 15, 2014)
	AND LOAN.FILIAL-ID = /*ENTRY(2, signs.SURROGATE,'@') */ shFilial
	AND LOAN.LOAN-STATUS <> 'СОЗД'
 /* AND loan.cont-code EQ "01-00-14537-АПН@0400XXX" */
	AND loan.cont-code MATCHES maskLoan
	NO-LOCK:
		/* проданные кредиты пропускаем */
		IF GetXAttrValueEx ("loan", loan.contract + ',' + loan.cont-code, "НомДогЦес", "") NE "" THEN NEXT.

IF LOAN.CONT-CODE BEGINS '45-00-29975-' OR
   LOAN.CONT-CODE BEGINS '45-00-29901-' OR
   LOAN.CONT-CODE BEGINS '45-00-30580-' OR
   LOAN.CONT-CODE BEGINS '45-00-30729-' THEN NEXT.

		tI = tI + 1.
		/* ложим во временную таблицу */
		CREATE HZ.
		/*----------------- данные о КД -----------------*/
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
		/*----------------- данные о залогодателе -----------------*/
		TMP_CHEL = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "CustSurr", "").
		/* ФЛ */
		IF ENTRY(1, TMP_CHEL) = "Ч" THEN
		DO:
			/**/
			FIND FIRST PERSON
			    WHERE PERSON.PERSON-ID = INT64( ENTRY(2, TMP_CHEL) )
			    NO-LOCK NO-ERROR.
			/**/
			IF AVAIL PERSON THEN
			DO:
				/* инфо */
				HZ.iTYPE = "Ч".
				HZ.iLASTNAME  = PERSON.NAME-LAST.			/* Фамилия */
				HZ.iFIRSTNAME =	ENTRY(1, PERSON.FIRST-NAMES, " ").	/* Имя */
				HZ.iMIDDLENAME = SUBSTRING(PERSON.FIRST-NAMES, INDEX(PERSON.FIRST-NAMES, " ") + 1).	/* Отчество */ 
				HZ.iBIRTHDATE =	PERSON.BIRTHDAY.		/* Дата рождения */ 
				/**/
				FIND FIRST CCODE
				    WHERE CCODE.CLASS = "КодДокум"
				    AND CCODE.PARENT = "КодДокум"
				    AND CCODE.CODE = PERSON.DOCUMENT-ID
				    NO-LOCK NO-ERROR.
				/**/
				IF AVAIL CCODE THEN
					HZ.iDOCUMENTCODE = CCODE.VAL.  /* Код вида документа */
				/**/
				HZ.iDOCUMENTNAME = PERSON.DOCUMENT-ID. /* Вид документа */ 

				/* формат серия номер для паспорта */
				IF  HZ.iDOCUMENTCODE                    EQ '21' AND
				    NUM-ENTRIES( PERSON.DOCUMENT)       EQ 3 AND
				    LENGTH( ENTRY( 1, PERSON.DOCUMENT)) EQ 2 AND
				    LENGTH( ENTRY( 2, PERSON.DOCUMENT)) EQ 2 THEN
				    HZ.iDOCUMENTSERIESNUMBER = ENTRY( 1, PERSON.DOCUMENT) + ENTRY( 2, PERSON.DOCUMENT) + ' ' + ENTRY( 3, PERSON.DOCUMENT).
					/* HZ.iDOCUMENTSERIESNUMBER = SUBSTRING(REPLACE(PERSON.DOCUMENT, " ", ""), 1, 4) + 
									" " + SUBSTRING(REPLACE(PERSON.DOCUMENT, " ", ""), 5). /* Серия и номер документа */
				END.*/
				ELSE HZ.iDOCUMENTSERIESNUMBER = PERSON.DOCUMENT.
				/**/
				/* адрес */
				HZ.aTYPE = 0.
				HZ.aREG_CODE = GetXAttrValueEx ("person", STRING(PERSON.PERSON-ID), "КодРегГНИ", ""). /* код региона */
				/**/
				HZ.aREG_NAME = GetCodeName( "КодРегГНИ", HZ.aREG_CODE).
				{cust-adr.obj
				    &addr-to-vars = YES
				    &tablefield   = "TRIM(person.address[1] + ' ' + person.address[2])"
				}
				HZ.aDISTRICT  = IF TRIM(vOblChar)   <> "" THEN TRIM( vOblChar) ELSE ?.  /* Район */
				HZ.aCITY      = IF TRIM(vGorChar)   <> "" THEN TRIM( vGorChar) ELSE ?.  /* Город */
				HZ.aLOCALITY  = IF TRIM(vPunktChar) <> "" THEN TRIM( vPunktChar) ELSE ?. /* Населенный пункт */
				HZ.aSTREET    = IF TRIM(vUlChar)    <> "" THEN TRIM( vUlChar) ELSE ?.   /* Улица */
				HZ.aHOUSE     = IF TRIM(vDomChar)   <> "" THEN TRIM( vDomChar) ELSE ?.  /* Дом */
				HZ.aBUILDING  = IF TRIM(vKorpChar)  <> "" THEN TRIM( vKorpChar) ELSE ?. /* Корпус */
				HZ.aAPARTMENT = IF TRIM(vKvChar)    <> "" THEN TRIM( vKvChar) ELSE ?.   /* Квартира */
				
				/*
				IF HZ.aREG_CODE <> ? THEN 
				DO:
					/**/
					FIND FIRST CODE
					    WHERE CODE.CLASS = "КодРегГНИ"
					      AND CODE.PARENT = "КодРегГНИ"
					      AND CODE.CODE = HZ.aREG_CODE
					    NO-LOCK NO-ERROR.
					/**/
					IF AVAIL CODE THEN
					DO:
						/**/
						HZ.aREG_NAME = CODE.NAME.	/* наименование региона */
						/**/
					END.
					/**/
					HZ.aDISTRICT = ( IF TRIM( ENTRY(2, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(2, PERSON.ADDRESS[1])) ELSE ? ).		/* Район */
					HZ.aCITY = ( IF TRIM( ENTRY(3, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(3, PERSON.ADDRESS[1])) ELSE ? ).		/* Город */
					HZ.aLOCALITY = ( IF TRIM( ENTRY(4, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(4, PERSON.ADDRESS[1])) ELSE ? ).		/* Населенный пункт */
					HZ.aSTREET = ( IF TRIM( ENTRY(5, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(5, PERSON.ADDRESS[1])) ELSE ? ).		/* Улица */
					HZ.aHOUSE = ( IF TRIM( ENTRY(6, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(6, PERSON.ADDRESS[1])) ELSE ? ).		/* Дом */
					HZ.aBUILDING = ( IF TRIM( ENTRY(7, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(7, PERSON.ADDRESS[1])) ELSE ? ).		/* Корпус */
					HZ.aAPARTMENT = ( IF TRIM( ENTRY(8, PERSON.ADDRESS[1])) <> "" THEN TRIM( ENTRY(8, PERSON.ADDRESS[1])) ELSE ? ). 	/* Квартира */
					/**/
				END.
				*/
			END.
			/**/
		END.
		/**/	
		/* ЮЛ */
		IF ENTRY(1, TMP_CHEL) = "Ю" THEN
		DO:
			/**/
			FIND FIRST CUST-CORP
			    WHERE CUST-CORP.CUST-ID = INT64( ENTRY(2, TMP_CHEL) )
			    NO-LOCK NO-ERROR.
			/**/
		END.
		/**/
		/*----------------- данные об предмете залога -----------------*/
		IF SIGNS.XATTR-VALUE = 'Автомобиль' THEN
		DO:
			/**/
			HZ.wZALOG_TYPE = 1.
			HZ.wZALOG_SURR = signs.surrogate.
			/* найдем TCVIN */
			HZ.wZALOG_ID = GetXAttrValueEx ("term-obl", signs.surrogate, "TCVIN", "").

			/* ОТСУТСТВУЕТ  - не нужно */
			IF INDEX( CAPS(HZ.wZALOG_ID), "ОТСУ") > 0 OR
			   TRIM( HZ.wZALOG_ID ) = "-" THEN
			DO:
				/* нужно было ? */
				HZ.wZALOG_ID = ?. 
				/**/
			END.

			/* модель и марка */
			HZ.wZALOG_DESC = "Модель автомобиля: " + 
			    GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCbrand", "")
			    + " " +
			    GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCmodel", "")
			    + ", год выпуска:"
			    + GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCyear", "").
			DEF VAR vTCbody AS CHAR NO-UNDO.
			DEF VAR vTCmotor AS CHAR NO-UNDO.
			DEF VAR vTCchassis AS CHAR NO-UNDO.
			vTCbody    = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCbody", "").
			vTCmotor   = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCmotor", "").
			vTCchassis = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "TCchassis", "").
			IF vTCbody    NE "" AND NOT vTCbody BEGINS "ОТСУТСТВ"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', № кузова: ' + vTCbody.
			IF vTCmotor   NE "" AND NOT vTCmotor BEGINS "ОТСУТСТВ"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', модель и № двигателя: ' + vTCmotor.
			IF vTCchassis NE "" AND NOT vTCchassis BEGINS "ОТСУТСТВ"
			  THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', № шасси: ' + vTCchassis.
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
			/* нужно узнать были ли проводки по принятию этого залога или по списанию ? */
			/* определить счет в картотеке по обеспечению */
			/* определим тип и "номер" счета */
			TYPE_ACC = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "ВидДогОб", "").
			NUMB_ACC = GetXAttrValueEx ("term-obl", SIGNS.SURROGATE, "НомерПП", "0").
			/**/
			IF NUMB_ACC <> "0" THEN
				TYPE_ACC = TYPE_ACC + NUMB_ACC.
			/* найдем счет по типу */
			IF TYPE_ACC EQ "" THEN RELEASE LOAN-ACCT. ELSE
			FIND FIRST LOAN-ACCT
			    WHERE LOAN-ACCT.CONTRACT = LOAN.CONTRACT
			      AND LOAN-ACCT.CONT-CODE = LOAN.CONT-CODE
			      AND LOAN-ACCT.ACCT-TYPE = TYPE_ACC
			    NO-LOCK NO-ERROR.
			/* если нашли счет */
			IF AVAIL LOAN-ACCT THEN
			DO:
				/* найдем проводку по кредиту = ПРИНЯТИЕ */
				FIND FIRST OP-ENTRY
				    WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
				      AND OP-ENTRY.OP-DATE NE ? AND CAN-FIND(FIRST OP OF OP-ENTRY WHERE OP.OP-DATE NE ?)
					  AND op-entry.filial-id = loan.filial-id
				    NO-LOCK NO-ERROR.
				/**/
				IF AVAIL OP-ENTRY AND LOAN.LOAN-STATUS <> 'ЗАКР' THEN
					HZ.wNOTICE =  0.
				/* или по дебету = СПИСАНИЕ */
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

/* проверка выгруженных данных 
FOR EACH NOTIFICATIONS
 WHERE NOTIFICATIONS.CONTRACTSOURCESYSTEM EQ 1
   AND NOTIFICATIONS.STATUS_ NE 6 /* удаленные */
   AND NOTIFICATIONS.STATUS_ EQ 4
   AND NOTIFICATIONS.CREATETIME >= DATE(3,6,2015)
   AND NOTIFICATIONS.CREATETIME < DATE(3,11,2015)
 NO-LOCK  BY NOTIFICATIONS.CREATETIME :
    FIND FIRST LOAN 
      WHERE LOAN.DOC-REF EQ NOTIFICATIONS.CONTRACTNUMBER
        AND LOAN.FILIAL-ID EQ !!!  shFilial
        AND LOAN.CONTRACT EQ "Кредит"
       NO-LOCK NO-ERROR.
    IF NOT AVAIL LOAN OR (LOAN.LOAN-STATUS NE "ВВЕД" /* AND LOAN.LOAN-STATUS NE "ЗАКР"*/ ) THEN DO:
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

/* вывод */
/*
run instview.p(TEMP-TABLE HZ:HANDLE). 
*/

/* экспорт данных */

/* создадим буферы для таблиц */
/**/
/* 1. Выгрузить заголовок сообщения - информацию о договоре - в таблицу NOTIFICATIONS. */
/* new */
DEF BUFFER NF FOR NOTIFICATIONS.
/**/
/* 2.  Выгрузить адреса для залогодателя и залогодержателя в таблицы PledgeAddressRF / PledgeAddressForeign.*/
DEF BUFFER AR FOR PLEDGEADDRESSRF.
DEF BUFFER AF FOR PLEDGEADDRESSFOREIGN.
/* 3. Выгрузить залогодателя(-ей) и залогодержателя(-ей) в таблицы PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, при выгрузке привязать по идентификаторам ранее выгруженные адреса.*/
DEF BUFFER RO FOR PLEDGERUSSIANORGANIZATION.
DEF BUFFER FO FOR PLEDGEFOREIGNORGANIZATION.
DEF BUFFER PP FOR PLEDGEPRIVATEPERSON.
/* 4. Привязать залогодателя(-ей) к сообщению через таблицу PledgeMessagePledgors с указанием типа залогодержателя. */
DEF BUFFER PG FOR PLEDGEMESSAGEPLEDGORS.
/* 5. Привязать залогодержателя(-ей) к сообщению через таблицу PledgeMessagePledgees с указанием типа залогодержателя. */
DEF BUFFER MP FOR PLEDGEMESSAGEPLEDGEES.
/* 6. Выгрузить данные о залоге в таблицу PersonalProperties, для автомобилей указывать PropertyType=1 и в поле ID указывать VIN-код автомобиля.*/
DEF BUFFER PJ FOR PERSONALPROPERTIES.

/* счетчики */
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

/* нахождение значений счетчиков */

&IF DEFINED( SESSION-REMOTE ) = 0 &THEN
    put screen col 1 row screen-lines + message-lines + 1
		color bright-blink-normal "определение счетчиков...".
&ELSE
	mblodd_char_Tmp03 = ?.
	RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "определение счетчиков...", ? ).
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
			(IF HZ.wZALOG_ID = ? THEN "Отсутствует TCVIN" ELSE "Отсутствуют данные об автомобиле") delim
	  eol.
	/**/
END.
*/
/*output stream vvs close.*/

/*
 MESSAGE "Импорт успешно завершен!" VIEW-AS ALERT-BOX.
 RUN sndbispc ("file=" + fname + ";class=bq").
*/

/* вставка информации о банке = залогодержателе */
	/* адрес залогодержателя */
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
		AR.REGIONCODE 	= "55"				/* код региона */
		AR.REGION 		= "Омская обл"		/* наименование региона */
		AR.DISTRICT 	= ?					/* Район */
		AR.CITY 		= "Омск г"			/* Город */
		AR.LOCALITY 	= ?					/* Населенный пункт */
		AR.STREET 		= "Газетный пер"	/* Улица */
		AR.HOUSE 		= "6"				/* Дом */
		*/
		AR.ADDRESSID 	= CNT_AR            /* SEQ_ ADDRESSID */
		AR.REGIONCODE 	= "77"				/* код региона */
		AR.REGION 		= "г. Москва"		/* наименование региона */
		AR.DISTRICT 	= ?					/* Район */
		AR.CITY 		= ""				/* Город */
		AR.LOCALITY 	= ?					/* Населенный пункт */
		AR.STREET 		= "Известковый пер"	/* Улица */
		AR.HOUSE 		= "7 стр.1"				/* Дом */
	.
	/**/
	VALIDATE AR.
	/**/		
	/* информация о залогодержателе */
	CNT_RO = CNT_RO + 1.
	/**/
	CREATE RO.
	/**/
	ASSIGN
		RO.CLIENTID  = CNT_RO
		RO.NAMEFULL  = 'Публичное акционерное общество "Плюс Банк"'
		RO.OGRN 	 = "1025500000624"
		RO.INN 	 	 = "5503016736"
		RO.ADDRESSID = CNT_AR
	.
	/**/
	VALIDATE RO.
	/**/
	/* запоминаем идентификатор залогодержателя */
	ZALOG_DERZH = CNT_RO.
	/*!!!!!!!!!*/

{bar-beg2.i
&BarTotal     = tI
&BarMessage   = "'Импорт автозалогов...'"}	

DO /* TRANSACTION */ ON ERROR UNDO, THROW:
    FIND FIRST seance WHERE
        seance.seancedate = today AND
        seance.direct = 'Экспорт' AND
        seance.op-kind = 'e-notary'
        NO-LOCK NO-ERROR.
    IF NOT AVAIL seance THEN DO:
    CREATE Seance.
    ASSIGN
        seance.seancedate = today
        seance.direct = 'Экспорт'
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
            PUT UNFORMATTED "ошибка создания пакета" SKIP.
            UNDO, RETURN ERROR.
        END.
        {pack-crt.i
         &Packet      = Packet
         &PacketID    = vPacketID
         &SeanceID    = seance.seanceid
         &MailUserID  = 0
         &State       = "'СОЗД'"
         &AbonentID   = 0
         &Kind        = mKind
         &Format      = mKind
         &ClassCode   = "'Packet'"
         &ParentID    = 0
        }
    END.
END. ELSE vPacketID = packet.packetid.

/* идем по найденным автозалогам */
/* вставляем записи в таблицу */
FOR EACH HZ
	WHERE HZ.iBIRTHDATE <> ?
	/* модель марка */
	AND HZ.wZALOG_SF = 0
	/* TCVIN */
	AND (HZ.wZALOG_ID <> ? OR HZ.jOPEN_DATE >= DATE(3,17,2015))
	/* принятие или списание */
	/* AND HZ.wNOTICE <> ? */
	AND (
		/* либо отсутствует запись с указанным № КД */
		(HZ.wNOTICE = 0 AND
		 NOT CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  /*AND NOTIFICATIONS.NOTIFICATIONTYPE EQ HZ.wNOTICE + 1 любая отправка */
			  AND NOTIFICATIONS.STATUS_ NE 6 /* удаленные */
			  
			)
		)  OR
		/* или же произошла операция списания залога, была постановка и не было списания  */
		(HZ.wNOTICE = 1 AND
		 (NOT CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  AND NOTIFICATIONS.NOTIFICATIONTYPE EQ HZ.wNOTICE + 1
			  AND NOTIFICATIONS.STATUS_ NE 6 /* удаленные */
			)) AND
		 CAN-FIND
			(
			FIRST NOTIFICATIONS
			WHERE NOTIFICATIONS.CONTRACTNUMBER = HZ.jCONT_CODE
			  AND NOTIFICATIONS.NOTIFICATIONTYPE EQ 1
			  AND NOTIFICATIONS.STATUS_ NE 6 /* удаленные */
			)
		)
	    )
	NO-LOCK ON ERROR UNDO, NEXT:

	{bar2.i &BarPointer = nn}
	nn = nn + 1.
/* put unformatted "1 " HZ.term-obl-surrogate SKIP. */
	/* если списание надо найти регистрационные данные */
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
 			/* нет рег.данных для отмены залога */
 			PUT UNFORMATTED HZ.term-obl-surrogate ": нет рег.данных для отмены залога" SKIP.
 			NEXT.
 		END.
		/* потребуется найти предыдущую запись *
		DEF BUFFER GH FOR NOTIFICATIONS.
		FIND FIRST GH
		WHERE GH.CONTRACTNUMBER = SUBSTRING(HZ.jCONT_CODE, 1, 60)
		AND GH.STATUS_ = 1
		NO-LOCK NO-ERROR.
		IF NOT AVAIL GH THEN NEXT. 
		HZ.NOTIFICATIONREFERENCENUMBER 	= GH.CREATIONREFERENCENUMBER. */
	END.
	IF LENGTH(HZ.NOTIFICATIONREFERENCENUMBER) > 23 THEN DO:
		PUT UNFORMATTED HZ.term-obl-surrogate " длина номера " HZ.NOTIFICATIONREFERENCENUMBER " больше 23 символов" SKIP.
		NEXT.
	END.

/* put unformatted "2" SKIP. */
	/* 1. Выгрузить заголовок сообщения - информацию о договоре - в таблицу NOTIFICATIONS */
	RUN PacketCreateLink(vPacketID,  
                           "loan", 
                           STRING( 'Кредит' + ',' + AddFilToLoan(HZ.jCONT_CODE, HZ.jFILIAL_ID)), 
                           (IF HZ.wNOTICE = 0 THEN "Open" ELSE (IF HZ.wNOTICE = 1 THEN "Close" ELSE "")) /* mKind */ ).

	/* следующий идентификатор */
	DO WHILE True:
		CNT_NF = NEXT-VALUE(SEQ_PLEDGEID).
		FIND FIRST NF WHERE NF.NOTIFICATIONID EQ CNT_NF NO-LOCK NO-ERROR.
		IF NOT AVAIL NF THEN LEAVE.
	END.
	/**/	
	RELEASE NF.

	CREATE NF.
	ASSIGN
		/* идентификатор */
		NF.NOTIFICATIONID				= CNT_NF /* SEQ_PLEDGEID */
		NF.CREATETIME					= NOW
		NF.MODIFYTIME					= NOW
		NF.STATUS_					    = 1 /* 1-новое уведомление ( IF HZ.wNOTICE = 0 THEN 1 ELSE 2 ) */
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
		
	/* если новый автозалог, то вставляем данные */
	IF HZ.wNOTICE = 0 THEN
		DO:
			/* 2. Выгрузить адреса для залогодателя и залогодержателя в таблицы PledgeAddressRF / PledgeAddressForeign.*/
			/* следующий идентификатор */
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
					AR.REGIONCODE 	= SUBSTRING(HZ.aREG_CODE, 1, 2)		/* код региона */
					AR.REGION 		= SUBSTRING(HZ.aREG_NAME, 1, 60)	/* наименование региона */
					AR.DISTRICT 	= SUBSTRING(HZ.aDISTRICT, 1, 60)	/* Район */
					AR.CITY 		= SUBSTRING(HZ.aCITY, 1, 60)		/* Город */
					AR.LOCALITY 	= SUBSTRING(HZ.aLOCALITY, 1, 60)	/* Населенный пункт */
					AR.STREET 		= SUBSTRING(HZ.aSTREET, 1, 60)		/* Улица */
					AR.HOUSE 		= SUBSTRING(HZ.aHOUSE, 1, 8)		/* Дом */
					AR.BUILDING 	= SUBSTRING(HZ.aBUILDING, 1, 8)		/* Корпус */
					AR.APARTMENT 	= SUBSTRING(HZ.aAPARTMENT, 1, 8)	/* Квартира */
				.
				/**/
				VALIDATE AR.
				/**/
			END.
			/**/
			/* 3. Выгрузить залогодателя(-ей) и залогодержателя(-ей) в таблицы PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, при выгрузке привязать по идентификаторам ранее выгруженные адреса.*/
			
/* put unformatted "5" SKIP. */
			DO WHILE True:
				CNT_PP = NEXT-VALUE(SEQ_CLIENTID).
				FIND FIRST PP WHERE PP.CLIENTID EQ CNT_PP NO-LOCK NO-ERROR.
				IF NOT AVAIL PP THEN LEAVE.
			END.
			RELEASE PP.
			
			/* CNT_PP = CNT_PP + 1. */
			/* данные о залогодателе */
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
				/* идентификатор адреса залогодателя */
				PP.ADDRESSRFID = CNT_AR
			.
			/**/
			VALIDATE PP.
			/**/
			/* 4. Привязать залогодателя(-ей) к сообщению через таблицу PledgeMessagePledgors с указанием типа залогодержателя.*/
			/**/
			/*CNT_PM = CNT_PM + 1.*/
/* put unformatted "6" SKIP. */
			CREATE PG.
			/**/
			ASSIGN
				/* идентификатор сообщения */
				PG.PLEDGEID = NF.NOTIFICATIONID
				/* идентификатор залогодателя */
				PG.CLIENTID = PP.CLIENTID
			.
				/* тип клиента */
				IF HZ.iTYPE = "Ч" THEN
					PG.CLIENTTYPE = "P".
				ELSE	
					PG.CLIENTTYPE = "L".
				/**/

			/**/
			VALIDATE PG.
			/**/
			/* 5. Привязать залогодержателя(-ей) к сообщению через таблицу PledgeMessagePledgees с указанием типа залогодержателя. */
			/**/
			CREATE MP.
			/**/
			ASSIGN
				/* идентификатор сообщения */
				MP.PLEDGEID = NF.NOTIFICATIONID
				/* идентификатор залогодержателя */
				MP.CLIENTID = ZALOG_DERZH
				/**/
				MP.CLIENTTYPE = "L"
			.
			/**/
			VALIDATE MP.
			/**/
			/* 6. Выгрузить данные о залоге в таблицу PersonalProperties, для автомобилей указывать PropertyType=1 и в поле ID указывать VIN-код автомобиля.*/
			
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
		 (IF HZ.wNOTICE EQ 0 THEN "Залог" ELSE "Снятие") " по договору " + HZ.jCONT_CODE + " экспортирован (id=" + STRING(CNT_NF) + ')' SKIP.
	END.
CATCH eAnyError AS Progress.Lang.Error:
 PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
END CATCH.
END.

{del-bar.i}

/*MESSAGE "Импорт успешно завершен!" VIEW-AS ALERT-BOX.*/
{preview.i &filename=vFile}

{intrface.del}
