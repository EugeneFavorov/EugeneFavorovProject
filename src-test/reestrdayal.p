/* реестр в Алекс */
DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{tmprecid.def}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR sName AS CHAR NO-UNDO.
DEFINE VAR sAddr AS CHAR NO-UNDO.
DEFINE VAR fName AS CHAR NO-UNDO.
DEFINE VAR ffName AS CHAR NO-UNDO.
DEFINE VAR sCommand AS CHAR NO-UNDO.
DEFINE VAR AcctCesRS AS CHAR NO-UNDO.
DEFINE VAR iCount AS INT64 NO-UNDO.
DEFINE VAR dSum AS DECIMAL NO-UNDO.
DEFINE VAR sContCOde AS CHAR NO-UNDO.
DEF VAR sIdLoan AS CHAR NO-UNDO.
end-date = today - 1.
{getdate.i
   &noinit = "/*"}

iCount = 0.
dSum = 0.
fName = STRING(YEAR(end-date),"9999") + STRING(MONTH(end-date),"99") + STRING(DAY(end-date),"99") + "_01_PLUS.txt".

if SEARCH(fName) NE ? THEN OS-DELETE VALUE(fName).

OUTPUT TO VALUE(fName) APPEND CONVERT TARGET "1251".

eachop:
DO TRANSACTION ON ERROR  UNDO eachop, LEAVE eachop:
    
    
FOR EACH op WHERE
    op.op-date = end-date 
    AND (op.op-kind = 'peralex' or op.op-kind begins 'perechAl' or op.op-kind = '04012')
	AND op.op-status BEGINS "√"   
	/* AND op.filial-id = '0300' */
    NO-LOCK,
    EACH op-entry OF op 
	WHERE 
/* op-entry.op-status BEGINS "√"  
	AND */
 op-entry.acct-db begins '40817'
	AND op-entry.acct-cr begins '47422'
	NO-LOCK:
	    
    IF GetXAttrValueEx("op",string(op.op),"rst-time-exp","") = "" THEN DO:
  	sContCode = GetXattrValueEx("op",
                string(op.op),
                "cont-code",
                 "").
/*                message sContCode view-as alert-box. */
        IF INDEX(sContCode,"@") = 0 AND sContCode <> '' THEN DO:
            sContCode = sContCode + '@' + op.filial-id.
        END.        
        sIdLoan = ''.    
        FIND FIRST loan where loan.contract = 'Кредит' AND loan.cont-code = sContCode no-lock no-error.
        IF AVAIL loan then do:
		FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
			AND loan-acct.acct-type = 'КредРасч'
			AND loan-acct.cont-code = sContCode
			AND loan-acct.acct = op-entry.acct-db NO-LOCK NO-ERROR.
		IF AVAIL loan-acct THEN DO:
			FIND FIRST loan WHERE loan.contract = loan-acct.contract 
				AND loan.cont-code = loan-acct.cont-code NO-LOCK NO-ERROR.
			AcctCesRS = GetXattrValueEx("loan",
                loan-acct.contract + "," + loan-acct.cont-code,
                "AcctCesRS",
                 "").
			RUN GetName( INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT sName).
			RUN GetAddr( INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT sAddr).
			iCount = iCount + 1.
			dSum = dSum + op-entry.amt-rub.
			RUN PrintLine (iCount,
			        STRING(recid(loan)),
					op.op-date,
					op-entry.amt-rub,
					loan-acct.cont-code,
					sName,
					sAddr,
					AcctCesRS,
					op.details).
            UpdateSigns("opb", string(op.op), "rst-time-exp", STRING( NOW, "99.99.9999 HH:MM:SS"), ?) NO-ERROR.
		END.
        END.
    END. /* IF GetXAttrValueEx("op-kind",op-kind.op-kind,"rst-time-exp","") <> "" THEN DO: */
END.

OUTPUT CLOSE.

sName = 'Реестр на сумму ' + STRING(dSum, ">>>>>>>>>>>9.99") + CHR(10) + ' Документов ' + STRING(iCount) + CHR(10) + 'Выгружаем в ALEX?'.
MESSAGE sName VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice2 AS LOGICAL.
IF  choice2 <> YES THEN DO:
	if SEARCH(fName) NE ? THEN OS-DELETE VALUE(fName).
	UNDO eachop, LEAVE eachop.
END.
ELSE DO:
	ffname = "/data/home2/bis/quit41d/imp-exp/alexbank/out/" + fname.
	/*sCommand = 'mv ' + fname + ' ' + ffname.*/
	OS-COPY VALUE(fName) VALUE(ffname). 
	/*OS-COMMAND SILENT VALUE(sCommand).*/
END.

END. /* eachop: DO TRANSACTION ON ERROR  UNDO eachop, LEAVE eachop: */
{intrface.del}
  
Procedure PrintLine.
DEFINE INPUT PARAMETER iNum AS INT NO-UNDO.
DEFINE INPUT PARAMETER iDocNum AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iSum AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iContCode AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iAddr AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iAcct AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iDetails AS CHAR NO-UNDO.

	PUT UNFORMATTED
/* Номер п/п  */
	STRING(iNum) 
	";"
/* Номер п/п  */
	iDocNum 
	";"
/* Номер договора */
	ENTRY(1,iContCode,'@')
	";"
/* Дата операции */
	STRING(DAY(iDate), "99") + "." + STRING(MONTH(iDate), "99") + "." + STRING(YEAR(iDate), "9999") 
	";"
/* Время операции */
	"00:00:00" 
	";"
/* ФИО заемщика  */
	iName
	";"
/* Сумма операции */
	STRING(iSum, ">>>>>>>>>>>9.99")
	";"
/* Назначение платежа */
	REPLACE(REPLACE(iDetails,CHR(10),''),CHR(13),'')
	CHR(13) + CHR(10)
	.
END PROCEDURE.

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

PROCEDURE GetAddr:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER saddr AS CHARACTER.
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE person.person = id
   NO-LOCK NO-ERROR.
    IF AVAIL person THEN
    /* Address клиента */
    saddr = TRIM(STRING(person.address[1])) + TRIM(STRING(person.address[2])).
 END.
 ELSE
  DO:
   FIND FIRST cust-corp 
   WHERE cust-corp.cust-id = id
   NO-LOCK NO-ERROR.
    IF AVAIL cust-corp THEN
    /* наименование организации */
    saddr = TRIM(STRING(cust-corp.addr-of-low[1])) + TRIM(STRING(cust-corp.addr-of-low[2])).
  END.
END PROCEDURE.




