/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: Создаем документы перечисления по реестру "Союз"
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/



/* Пробуем конвертнуть в дату */
/* Входная строка в формате 11.11.11  или 11.11.2011 */
FUNCTION ConvertDate RETURN DATE(INPUT iDate AS CHARACTER):
    DEFINE VAR returnDate AS DATE NO-UNDO INIT ?.
    DEFINE VAR mTmpStr AS CHAR NO-UNDO.
    DEFINE VAR mTmpDay AS INT64 NO-UNDO.
    DEFINE VAR mTmpMonth AS INT64 NO-UNDO.
    DEFINE VAR mTmpYear AS INT64 NO-UNDO.
    DEFINE VAR mTmpDate AS DATE NO-UNDO.

    ERROR-STATUS:ERROR = NO.
    mTmpStr = REPLACE(iDate,"/",".").
        IF LENGTH(mTmpStr) = 8 OR LENGTH(mTmpStr) = 10 THEN DO:
            mTmpDay = INT(SUBSTRING(mTmpStr,1,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpMonth = INT(SUBSTRING(mTmpStr,4,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpYear = INT(SUBSTRING(mTmpStr,7)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpDate = DATE (mTmpMonth,mTmpDay,mTmpYear) NO-ERROR.
            IF ERROR-STATUS:ERROR = NO THEN DO:
                returnDate = mTmpDate.
            END.
        END.
    RETURN returnDate.
END FUNCTION.

/* Проверяет, является ли строка - числом */
FUNCTION ConvertDecimal RETURN DECIMAL (iStr AS CHAR):
   iStr = replace(iStr,' ','').
   iStr = replace(iStr,',','.').
   DEF VAR vRes AS DECIMAL NO-UNDO.
   ERROR-STATUS:ERROR = NO.
   vRes = DECIMAL(iStr) NO-ERROR.
   IF ERROR-STATUS:ERROR = YES THEN vRes = -1.
   RETURN vRes.
END FUNCTION.

{sh-defs.i}
/* {ksh-defs.i NEW} */
{globals.i}  
 {topkind.def} 
/*
{intrface.get tmess}
{intrface.get pbase} */
{intrface.get xclass}

DEF VAR ffname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.

DEF VAR sCode AS CHAR NO-UNDO.
DEF VAR sName AS CHAR NO-UNDO.
DEF VAR sName1 AS CHAR NO-UNDO.
DEF VAR sDate AS DATE NO-UNDO.
DEF VAR sOpDate AS DATE FORMAT "99/99/9999" LABEL "Введите дату реестра" INIT TODAY NO-UNDO.
DEF VAR sSum AS DECIMAL NO-UNDO.
DEF VAR sSumQ AS DECIMAL NO-UNDO.
DEF VAR sSumS AS DECIMAL NO-UNDO.
DEF VAR sSumR AS DECIMAL NO-UNDO.
DEF VAR sSumT AS DECIMAL NO-UNDO.
DEF VAR sSumU AS DECIMAL NO-UNDO.
DEF VAR sSumAA AS DECIMAL NO-UNDO.
DEF VAR sNewSum AS DECIMAL NO-UNDO.
DEF VAR iInt AS INT64 NO-UNDO.
DEF VAR ost40817 AS DECIMAL NO-UNDO.
DEF VAR AcctCesRS AS CHAR NO-UNDO.
DEF VAR strDateProd AS CHAR NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.

DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttOp
	FIELD in-cont-code LIKE loan.cont-code
	FIELD in-doc-ref LIKE loan.cont-code
	FIELD in-open-date LIKE loan.open-date
	FIELD in-op-date AS DATE
	FIELD in-acctdb LIKE loan-acct.acct
	FIELD in-acctcr LIKE loan-acct.acct
	FIELD in-curr LIKE loan-acct.currency
	FIELD in-filial LIKE loan.filial-id
	FIELD in-sSum AS DECIMAL
	FIELD in-sNewSum AS DECIMAL
	FIELD in-nameCl AS CHAR
	FIELD in-acctext AS CHAR
	FIELD mess AS CHAR
	FIELD in-loanop AS CHAR
    FIELD in-DateProd AS CHAR
.

DEF TEMP-TABLE ttloan-acct
	field contract like loan-acct.contract
	field cont-code like loan-acct.cont-code
	field acct-type like loan-acct.acct-type
	field acct like loan-acct.acct
	field currency like loan-acct.currency
	field since like loan-acct.since	
.


/*
PAUSE 0.
UPDATE SKIP(1) sOpDate SKIP(1)
  WITH FRAME fMain OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE "Введите дату реестра".
HIDE FRAME fMain.
*/
{getdate.i}

sOpDate = end-date.
	
ffname = '/data/home2/bis/quit41d/imp-exp/alexbank/in/1.csv'.

iInt = 0.

{getfile.i &filename=ffname &mode=must-exist }

IF SEARCH (fname) = ? THEN DO:
	MESSAGE ("Файл не найден " + fname) VIEW-AS ALERT-BOX.
	RETURN.
END.

{empty ttOp}
{empty ttloan-acct}
INPUT FROM VALUE(fname) CONVERT TARGET "ibm866"  SOURCE "1251".

REPEAT ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED fstr.
	fstr = REPLACE(fstr,'"','').
	iInt = iInt + 1.
	ost40817 = 0.
		sCode = TRIM(ENTRY(3,fstr,';')).
		sName = ENTRY(2,fstr,';').
		sSumQ = ConvertDecimal(ENTRY(11,fstr,';')). /* ОД         = L */
		sSumR = ConvertDecimal(ENTRY(12,fstr,';')). /* Проср.ОД   = M */ 
		sSumS = ConvertDecimal(ENTRY(13,fstr,';')). /* Начисл%    = N */
		sSumT = ConvertDecimal(ENTRY(14,fstr,';')). /* Просроч%   = O */
		sSumU = ConvertDecimal(ENTRY(15,fstr,';')). /* Просроч% на просрОД = P */
		sSumAA = ConvertDecimal(ENTRY(19,fstr,';')). /* Сумма передаваемых прав требований = T */
        strDateProd = TRIM(ENTRY(5,fstr,';')). /* Дата продажи договора */
	
/*
 message 'first ' ssumaa view-as alert-box. 
*/
		sDate = sOpDate.
/*		sDate = TODAY - 1.
		sOpDate = TODAY - 1.  */
		IF sSumAA > 0 THEN DO:
			FIND FIRST loan WHERE loan.doc-ref = sCode 
				AND (loan.filial-id = '0000' OR loan.filial-id = '0300')
				NO-LOCK NO-ERROR.
			IF AVAIL loan THEN DO:
			RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT sName1).
/*                                           	CREATE ttOp. */

				FIND last loan-acct OF loan WHERE loan-acct.acct-type = 'Кредит'
				 NO-ERROR.
				IF AVAIL loan-acct THEN DO:
					IF sSumQ > 0 THEN DO:

						 CREATE ttOp.  
						ASSIGN
							ttOp.in-cont-code = loan.cont-code
							ttOp.in-doc-ref = loan.doc-ref
							ttOp.in-open-date = loan.open-date
							ttOp.in-acctdb = loan-acct.acct
							ttOp.in-curr = loan-acct.currency
							ttOp.in-filial = loan.filial-id
							ttOp.in-sSum = sSumQ
							ttOp.in-sNewSum = sSumQ
							ttOp.in-namecl = sName1
							ttOp.in-acctext ='срочный ОД'
							ttOp.in-op-date = sOpDate
							ttOp.in-acctcr = '47422810100000000191'
							ttOp.in-loanop = '4'
							ttOp.in-DateProd = strDateProd
						.
						RELEASE ttOp.
					END. /* IF sSum > 0 THEN DO: */
					ELSE DO:
/*						ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum). */
					END.
				
					create ttloan-acct.
					assign
						ttloan-acct.contract = loan-acct.contract
						ttloan-acct.cont-code = loan-acct.cont-code	
						ttloan-acct.acct-type = loan-acct.acct-type
						ttloan-acct.acct = loan-acct.acct
						ttloan-acct.currency = loan-acct.currency
						ttloan-acct.since = loan-acct.since
					.

					delete loan-acct.


				END.
                                                              

				FIND last loan-acct OF loan WHERE loan-acct.acct-type = 'КредТОВ'
				 NO-ERROR.
				IF AVAIL loan-acct THEN DO:
					IF sSumS > 0 THEN DO:

						 CREATE ttOp. 
						ASSIGN
							ttOp.in-cont-code = loan.cont-code
							ttOp.in-doc-ref = loan.doc-ref
							ttOp.in-open-date = loan.open-date
							ttOp.in-acctdb = loan-acct.acct
							ttOp.in-curr = loan-acct.currency
							ttOp.in-filial = loan.filial-id
							ttOp.in-sSum = sSumS
							ttOp.in-sNewSum = sSumS
							ttOp.in-namecl = sName1
							ttOp.in-acctext ='срочные %%'
							ttOp.in-op-date = sOpDate
							ttOp.in-acctcr = '47422810100000000191'
							ttOp.in-loanop = '65'
							ttOp.in-DateProd = strDateProd
						.
						RELEASE ttOp.
					END. /* IF sSum > 0 THEN DO: */
					ELSE DO:
/*						ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum). */
					END.
				
					create ttloan-acct.
					assign
						ttloan-acct.contract = loan-acct.contract
						ttloan-acct.cont-code = loan-acct.cont-code	
						ttloan-acct.acct-type = loan-acct.acct-type
						ttloan-acct.acct = loan-acct.acct
						ttloan-acct.currency = loan-acct.currency
						ttloan-acct.since = loan-acct.since
					.
				

					delete loan-acct.
				
				END.

				FIND last loan-acct OF loan WHERE loan-acct.acct-type = 'КредПр'
				 NO-ERROR.
				IF AVAIL loan-acct THEN DO:
					IF sSumR > 0 THEN DO:

						  CREATE ttOp. 
						ASSIGN
							ttOp.in-cont-code = loan.cont-code
							ttOp.in-doc-ref = loan.doc-ref
							ttOp.in-open-date = loan.open-date
							ttOp.in-acctdb = loan-acct.acct
							ttOp.in-curr = loan-acct.currency
							ttOp.in-filial = loan.filial-id
							ttOp.in-sSum = sSumR
							ttOp.in-sNewSum = sSumR
							ttOp.in-namecl = sName1
							ttOp.in-acctext ='просроченный ОД'
							ttOp.in-op-date = sOpDate
							ttOp.in-acctcr = '47422810100000000191'
							ttOp.in-loanop = '2'
							ttOp.in-DateProd = strDateProd
						.
						RELEASE ttOp.
					END. /* IF sSum > 0 THEN DO: */
					ELSE DO:
/*						ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum). */
					END.

					create ttloan-acct.
					assign
						ttloan-acct.contract = loan-acct.contract
						ttloan-acct.cont-code = loan-acct.cont-code	
						ttloan-acct.acct-type = loan-acct.acct-type
						ttloan-acct.acct = loan-acct.acct
						ttloan-acct.currency = loan-acct.currency
						ttloan-acct.since = loan-acct.since
					.

					delete loan-acct.
				END.

sSumU = sSumU + sSumT.
/* message 'sSumU ' sSumU view-as alert-box. */
				FIND last loan-acct OF loan WHERE loan-acct.acct-type = 'КредПр%ОВ'
				 NO-ERROR.
				IF AVAIL loan-acct THEN DO:

					IF sSumU > 0 THEN DO:

						  CREATE ttOp. 
						ASSIGN
							ttOp.in-cont-code = loan.cont-code
							ttOp.in-doc-ref = loan.doc-ref
							ttOp.in-open-date = loan.open-date
							ttOp.in-acctdb = loan-acct.acct
							ttOp.in-curr = loan-acct.currency
							ttOp.in-filial = loan.filial-id
							ttOp.in-sSum = sSumU
							ttOp.in-sNewSum = sSumU
							ttOp.in-namecl = sName1
							ttOp.in-acctext ='просроченные %%'
							ttOp.in-op-date = sOpDate
							ttOp.in-acctcr = '47422810100000000191'
							ttOp.in-loanop = '98'
							ttOp.in-DateProd = strDateProd
						.
						RELEASE ttOp.
					END. /* IF sSum > 0 THEN DO: */
					ELSE DO:
/*						ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum). */
					END.


   				
					create ttloan-acct.
					assign
						ttloan-acct.contract = loan-acct.contract
						ttloan-acct.cont-code = loan-acct.cont-code	
						ttloan-acct.acct-type = loan-acct.acct-type
						ttloan-acct.acct = loan-acct.acct
						ttloan-acct.currency = loan-acct.currency
						ttloan-acct.since = loan-acct.since
					.
				

					delete loan-acct.
				
				END.
/*
			ssumaa = ssumQ + ssumR + ssumT + ssumU.
*/
				FIND last loan-acct OF loan WHERE loan-acct.acct-type = 'Кред91418'
				 NO-ERROR.
				IF AVAIL loan-acct THEN DO:

					IF sSumAA > 0 THEN DO:

						  CREATE ttOp. 
						ASSIGN
							ttOp.in-cont-code = loan.cont-code
							ttOp.in-doc-ref = loan.doc-ref
							ttOp.in-open-date = loan.open-date
							ttOp.in-acctdb = loan-acct.acct
							ttOp.in-curr = loan-acct.currency
							ttOp.in-filial = loan.filial-id
							ttOp.in-sSum = sSumAA
							ttOp.in-sNewSum = sSumAA
							ttOp.in-namecl = sName1
							ttOp.in-acctext = 'номинальная стоимость приобретенных прав требования'
							ttOp.in-op-date = sOpDate
							ttOp.in-acctcr = '99999810400000000000'
							ttOp.in-DateProd = strDateProd
						.
						RELEASE ttOp.
					END. /* IF sSum > 0 THEN DO: */
					ELSE DO:
/*						ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum). */
					END.
				
				
					create ttloan-acct.
					assign
						ttloan-acct.contract = loan-acct.contract
						ttloan-acct.cont-code = loan-acct.cont-code	
						ttloan-acct.acct-type = loan-acct.acct-type
						ttloan-acct.acct = loan-acct.acct
						ttloan-acct.currency = loan-acct.currency
						ttloan-acct.since = loan-acct.since
					.

					delete loan-acct.
				
				END.













			END.
			ELSE DO:
			/* 	 ttOp.mess = ttOp.mess + 'Договор ' + sCode + ' не найден'. */ 
			END.

		END.
		ELSE DO:
		    /*
			IF sDate = ? THEN 
				ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' неверный формат даты платежа'.
			IF sSum = 0 THEN DO:
				ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' сумма в реестре = 0'.
			END.
			IF sSum < 0 THEN DO:
				ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' неверный формат суммы платежа'.
			END.
			*/
		END.
		
    fstr = ''.
 END. /* IMPORT UNFORMATTED fstr. */

FOR EACH ttOp WHERE ttOp.in-sNewSum > 0:
/* message 'for each trans' view-as alert-box. */
						{empty tOpKindParams}     /* очистить таблицу параметров */
						mRes = TDAddParam("in-cont-code",ttOp.in-cont-code).
						mRes = TDAddParam("in-doc-ref",ttOp.in-doc-ref).
						mRes = TDAddParam("in-open-date",STRING(ttOp.in-open-date,"99.99.9999")).
						IF ttOp.in-acctcr <> '99999810400000000000' THEN DO:
						mRes = TDAddParam("in-acctdb",ttOp.in-acctdb).
						mRes = TDAddParam("in-acctcr",ttOp.in-acctcr).
						END.
						else do:
						mRes = TDAddParam("in-acctdb",'').
						mRes = TDAddParam("in-acctcr",'').
						end.

						IF ttOp.in-acctcr = '99999810400000000000' THEN DO:
						mRes = TDAddParam("in-acctdb1",ttOp.in-acctdb).
						mRes = TDAddParam("in-acctcr1",ttOp.in-acctcr).
						END.
						else do:
						mRes = TDAddParam("in-acctdb1",'').
						mRes = TDAddParam("in-acctcr1",'').
						end.
						mRes = TDAddParam("in-curr",ttOp.in-curr).
						mRes = TDAddParam("in-filial",ttOp.in-filial).
						mRes = TDAddParam("in-sSum",STRING(ttOp.in-sNewSum)).
						mRes = TDAddParam("in-namecl",ttOp.in-nameCl).
						mRes = TDAddParam("in-acctext",ttOp.in-acctext).					
						mRes = TDAddParam("in-op-date",STRING(ttOp.in-op-date,"99.99.9999")).
		                mRes = TDAddParam("in-loanop",ttOp.in-loanop).
		                mRes = TDAddParam("in-DateProd",ttOp.in-DateProd).
/* message 'sumaa ' ssumaa ' ' mOk view-as alert-box. */

						RUN ex-trans.p(INPUT 'inalex', 
							INPUT date(sOpDate), 
							INPUT TABLE tOpKindParams, 
							OUTPUT mOK, 
							OUTPUT mMessage).
/* message mMessage view-as alert-box. */
						IF NOT mOK THEN
						DO:
							ttOp.mess = ttOp.mess + ' Договор ' + ttOp.in-cont-code + ', документ не создан, ' + mMessage.
						END.
						ELSE DO:
							ttOp.mess = ttOp.mess +  ' Договор ' + ttOp.in-cont-code + ', документ создан, сумма в реестре ' + STRING(ttOp.in-sSum) + ', сумма документа ' + STRING(ttOp.in-sNewSum).
						END.
END.



{intrface.del}


					FOR each ttloan-acct no-lock:
					        create loan-acct.
						assign
						loan-acct.contract = ttloan-acct.contract
						loan-acct.cont-code = ttloan-acct.cont-code	
						loan-acct.acct-type = ttloan-acct.acct-type
						loan-acct.acct = ttloan-acct.acct
						loan-acct.currency = ttloan-acct.currency
						loan-acct.since = ttloan-acct.since
						.
					end.							


{setdest.i}
	FOR EACH ttOp NO-LOCK:
		PUT UNFORMATTED ttOp.mess skip.
	END.
{preview.i}

RETURN.
 
PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END PROCEDURE.

 
 
