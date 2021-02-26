
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
def var numr as INT64 no-undo init 1.


pause 0.
end-date = today .
define FRAME frame_date_codes 
     end-date label "Дата реестра"
	 numr label "номер за день"
	 with 1  COL 1 down
	 
width 40 CENTERED OVERLAY ROW 10 TITLE "Дата, номер реестра".

  do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	end-date
	numr

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   if LASTKEY EQ KEYCODE("ESC") THEN
	return.
   if LASTKEY EQ KEYCODE("F1")
		THEN do:
			CASE FRAME-FIELD:
			WHEN "end-date" THEN
			DO:
				run calend.p.
				if (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				then frame-value = string(date(pick-value), "99/99/9999").
			END.
			END CASE.
		end.
	ELSE APPLY LASTKEY.
   end. /* EDITING: */
  end.  /* do on */	



iCount = 0.
dSum = 0.
fName = STRING(YEAR(end-date),"9999") + STRING(MONTH(end-date),"99") + STRING(DAY(end-date),"99") + '_' + STRING(numr,"99") +  '_810_ROSCAP_PLB.CRP'.

if SEARCH(fName) NE ? THEN OS-DELETE VALUE(fName).

OUTPUT TO VALUE(fName) APPEND CONVERT TARGET "1251".


FOR EACH op WHERE
    op.op-date = end-date 
    AND (op.op-kind = 'perroscap' or op.op-kind begins 'perechRS' or op.op-kind = '04012')
	AND op.op-status BEGINS "√"   
	/* AND op.filial-id = '0300' */
    NO-LOCK,
    EACH op-entry OF op 
	WHERE 
/* op-entry.op-status BEGINS "√"  
	AND */
 op-entry.acct-db begins '40817'
	AND (op-entry.acct-cr = '47422810400000002705     @0000')
	NO-LOCK:

  	sContCode = GetXattrValueEx("op",
                string(op.op),
                "cont-code",
                 "").
/*                message sContCode view-as alert-box. */
        IF INDEX(sContCode,"@") = 0 AND sContCode <> '' THEN DO:
            sContCode = sContCode + '@' + op.filial-id.
        END.            

		FIND LAST loan WHERE loan.contract = 'Кредит'
			AND loan.cont-code = sContCode NO-LOCK NO-ERROR.
	    IF AVAIL loan THEN DO:
			iCount = iCount + 1.
			dSum = dSum + op-entry.amt-rub.
			RUN PrintLine (string(recid(loan)),
					op.op-date,
					op-entry.amt-rub).
		END.
END.

OUTPUT CLOSE.

sName = 'Реестр на сумму ' + STRING(dSum, ">>>>>>>>>>>9.99") + CHR(10) + ' Документов ' + STRING(iCount) + CHR(10) + 'Выгружаем в РосКап'.
MESSAGE sName VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice2 AS LOGICAL.
IF  choice2 <> YES THEN DO:
	if SEARCH(fName) NE ? THEN OS-DELETE VALUE(fName).
END.
ELSE DO:
	ffname = "/data/home2/bis/quit41d/imp-exp/roscap/out/" + fname.
	/*sCommand = 'mv ' + fname + ' ' + ffname.*/
	OS-COPY VALUE(fName) VALUE(ffname). 
	/*OS-COMMAND SILENT VALUE(sCommand).*/
END.

{intrface.del}
  
Procedure PrintLine.
DEFINE INPUT PARAMETER iIdDog AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iSum AS DECIMAL NO-UNDO.


	PUT UNFORMATTED
    'p'
/* Номер договора */
	iIdDog
	";"
/* Дата операции */
	STRING(YEAR(iDate), "9999") + STRING(MONTH(iDate), "99") + STRING(DAY(iDate), "99")   
	";"
/* Сумма операции */
	TRIM(STRING(iSum, ">>>>>>>>>>>9.99"))
	";"
	CHR(13)
	SKIP.
END PROCEDURE.




