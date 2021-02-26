/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: Отступные
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

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
 {ksh-defs.i NEW} 
{globals.i}  
 {topkind.def} 

{intrface.get xclass}
{intrface.get loan}

DEF VAR listLoanAcct AS CHAR NO-UNDO.
DEF VAR listParam AS CHAR NO-UNDO.
DEF VAR sCode AS CHAR NO-UNDO.
DEF VAR sAcct AS CHAR NO-UNDO.
DEF VAR opDate AS DATE NO-UNDO.
DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.
DEF VAR sTmp AS CHAR NO-UNDO.
DEF VAR sObesp AS CHAR NO-UNDO. 
DEF VAR iCnt   AS INT64 NO-UNDO.
DEF VAR sSum AS DECIMAL NO-UNDO.
DEF VAR sSum2 AS DECIMAL NO-UNDO.
DEF VAR vDbSumDec AS DECIMAL NO-UNDO.
DEF VAR vCrSumDec AS DECIMAL NO-UNDO.
DEF VAR sName AS CHAR NO-UNDO.
DEF VAR pp-sum1 AS DECIMAL NO-UNDO EXTENT 2.



DEFINE TEMP-TABLE ttOp
	FIELD in-cont-code LIKE loan.cont-code
	FIELD in-doc-ref LIKE loan.cont-code
	FIELD in-open-date LIKE loan.open-date
	FIELD in-op-date AS DATE
	FIELD in-acctdb LIKE loan-acct.acct
	FIELD in-acctcl LIKE loan-acct.acct
	FIELD in-curr LIKE loan-acct.currency
	FIELD in-filial LIKE loan.filial-id
	FIELD in-sSum AS DECIMAL
	FIELD mess AS CHAR
    FIELD in-namecl AS CHAR
.

DEF INPUT PARAMETER inparam AS CHAR NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER acct FOR acct.

listLoanAcct = 'Кредит,КредБудКом,КредПр%,КредТ,КредПр,КредБудРКО'.
listParam = '8,9,12'.

sCode = ENTRY(1,inparam,',').
sAcct = ENTRY(2,inparam,',').
opDate = Date(ENTRY(3,inparam,',')).
sObesp  = ''.
FIND FIRST loan WHERE loan.contract = 'Кредит' AND loan.cont-code = sCode NO-LOCK NO-ERROR.
IF AVAIL loan THEN DO:

RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id, OUTPUT sName).

DO iCnt = 1 TO NUM-ENTRIES(listLoanAcct,','):
    
    FOR EACH loan-acct WHERE
        loan-acct.contract = loan.contract AND
        loan-acct.cont-code = loan.cont-code AND
        loan-acct.acct-type = ENTRY(iCnt,listLoanAcct, ",") AND 
        loan-acct.since <= opDate  
        NO-LOCK:

        RUN acct-pos IN h_base (loan-acct.acct,
                                 loan-acct.currency,
                                 opDate,
                                 opDate,
                                 ?).        
        
        IF loan-acct.currency = "" THEN
            sSum = ABS(sh-bal).
        ELSE sSum = ABS(sh-val).
            
        IF sSum > 0 THEN DO:
            CREATE ttOp.  
				ASSIGN
				    ttOp.in-cont-code = loan.cont-code
					ttOp.in-doc-ref = loan.doc-ref
					ttOp.in-open-date = loan.open-date
					ttOp.in-curr = loan-acct.currency
					ttOp.in-filial = loan.filial-id
					ttOp.in-sSum = sSum
					ttOp.in-op-date = opDate
					ttOp.in-acctdb = sAcct
					ttOp.in-acctcl = TRIM(ENTRY(1,loan-acct.acct,'@'))
					ttOp.in-namecl = sName
					.
					RELEASE ttOp.   
        END.						
     END.
END.    

    FIND LAST loan-acct WHERE
        loan-acct.contract = loan.contract AND
        loan-acct.cont-code = loan.cont-code AND
        loan-acct.acct-type = 'КредШт%' AND 
        loan-acct.since <= opDate  
        NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        
        DO iCnt = 1 TO NUM-ENTRIES(listParam,','):
            
        RUN STNDRT_PARAM IN h_loan (
            loan.contract, loan.cont-code, ENTRY(iCnt,listParam, ","),
            loan.since, OUTPUT sSum, OUTPUT vDbSumDec, OUTPUT vCrSumDec).
            RUN inter_current(BUFFER loan, ENTRY(iCnt,listParam, ","), OUTPUT sSum2).
            sSum = sSum + sSum2.
        IF sSum > 0 THEN DO:
        
            CREATE ttOp.  
		    ASSIGN
		        ttOp.in-cont-code = loan.cont-code
			    ttOp.in-doc-ref = loan.doc-ref
			    ttOp.in-open-date = loan.open-date
			    ttOp.in-curr = loan-acct.currency
			    ttOp.in-filial = loan.filial-id
			    ttOp.in-sSum = sSum
			    ttOp.in-op-date = opDate
			    ttOp.in-acctdb = sAcct
			    ttOp.in-acctcl = TRIM(ENTRY(1,loan-acct.acct,'@'))
			    ttOp.in-namecl = sName
			    .
			RELEASE ttOp.  
			
		    END.	
		END.
			
    END.
    

FOR EACH term-obl OF loan WHERE
        term-obl.idnt = 5 NO-LOCK:
        sTmp = TRIM(GetXattrValueEx("term-obl",
                loan.contract + "," + loan.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                "ВидОб","")).
        IF sTmp  <> 'Автомобиль' THEN NEXT.
        sObesp = sObesp + ' ' + TRIM(GetXattrValueEx("term-obl",
                loan.contract + "," + loan.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                "TCyear","")).
        sObesp = sObesp + ' ' + TRIM(GetXattrValueEx("term-obl",
                loan.contract + "," + loan.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                "TCmodel","")).
        sTmp = TRIM(GetXattrValueEx("term-obl",
                loan.contract + "," + loan.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                "TCVIN","")).
        IF sTmp = "" OR sTmp = 'ОТСУТСТВУЕТ' THEN DO:
            sTmp = TRIM(GetXattrValueEx("term-obl",
                loan.contract + "," + loan.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                "TCbody","")).
        END. 
        sObesp = sObesp + ' VIN ' + sTmp.
        LEAVE.
END.

FIND FIRST acct WHERE acct.acct = sAcct NO-ERROR.
IF AVAIL acct THEN DO:
    acct.details = 'Средства труда, полученные по договорам отступного, залога, назначение которых не определено, автомобиль'
    + sObesp.
    acct.cust-cat = 'В'.
    acct.branch-id = '0009'.
    RELEASE acct.
END.

END. /* if avail loan */

FOR EACH ttOp WHERE ttOp.in-sSum > 0:
						{empty tOpKindParams}     /* очистить таблицу параметров */
						mRes = TDAddParam("in-cont-code",ttOp.in-cont-code).
						mRes = TDAddParam("in-doc-ref",ttOp.in-doc-ref).
						mRes = TDAddParam("in-open-date",STRING(ttOp.in-open-date,"99.99.9999")).
						mRes = TDAddParam("in-acctdb",ttOp.in-acctdb).
						mRes = TDAddParam("in-acctcl",ttOp.in-acctcl).
						mRes = TDAddParam("in-curr",ttOp.in-curr).
						mRes = TDAddParam("in-filial",ttOp.in-filial).
						mRes = TDAddParam("in-sSum",STRING(ttOp.in-sSum)).
						mRes = TDAddParam("in-op-date",STRING(ttOp.in-op-date,"99.99.9999")).
                        mRes = TDAddParam("in-name",ttOp.in-namecl).
						RUN ex-trans.p(INPUT 'otst0009_l', 
							INPUT date(OpDate), 
							INPUT TABLE tOpKindParams, 
							OUTPUT mOK, 
							OUTPUT mMessage).

						IF NOT mOK THEN
						DO:
							ttOp.mess = ttOp.mess + ' Договор ' + ttOp.in-cont-code + ', документ не создан, ' + mMessage.
						END.
						ELSE DO:
							ttOp.mess = ttOp.mess +  ' Договор ' + ttOp.in-cont-code + ', документ создан, сумма ' + STRING(ttOp.in-sSum).
						END.
END.

{intrface.del}

{setdest.i}
	FOR EACH ttOp NO-LOCK:
		PUT UNFORMATTED ttOp.mess SKIP.
	END.
{preview.i}


PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER INIT ''.
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE IF cat = 'Ю' THEN
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END.

