

/* kam */

/* сумма проводки продажи */
FUNCTION GetSumProd RETURN DECIMAL (iContCode AS CHAR, iRole AS CHAR):
   DEF VAR vRes AS DECIMAL NO-UNDO INIT 0.
	FIND LAST loan-acct WHERE 
		loan-acct.contract = 'Кредит'
		AND loan-acct.cont-code = iContCode
		AND loan-acct.acct-type = iRole NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		FOR EACH op-entry WHERE op-entry.acct-cr = loan-acct.acct
			AND op-entry.acct-db BEGINS '61214' 
			AND op-entry.op-status BEGINS "√" NO-LOCK, 
			EACH op OF op-entry	WHERE 
				op.op-kind BEGINS 'prod_201' NO-LOCK:
			vRes = vRes + ABS(op-entry.amt-rub).
		END.
	END.
   RETURN vRes.
END FUNCTION.


DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{tmprecid.def}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR os_455 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_45815 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_47427 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_47427_p AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_45915 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_sr_91604 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_pr_91604 AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR os_pr_91604_p AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR penalty_os AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR penalty_prc AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR cost_zakl AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR cost_zakl1 AS DECIMAL NO-UNDO INIT 0.
DEF VAR datepereschet AS DATE NO-UNDO.


FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:

datepereschet = end-date.
/* DATE("30/08/2016"). */

	if loan.since <> datepereschet AND
	   AVAIL loan-int then do:
	    RUN l-calc2.p ("Кредит",       /* Назначение договора. */
	           loan.cont-code,      /* Номер договора. */
	           date(datepereschet),   /* Окончание договора + день для выполнения автом. */
	           FALSE,		/* включать/не включать пересчет течений договора */
	           TRUE).		/* выводить/ не выводить протокол на экран */
	end.


	os_455 = GetSumProd(loan.cont-code,'Кредит').
	os_45815 = GetSumProd(loan.cont-code,'КредПр').
	os_47427 = GetSumProd(loan.cont-code,'КредБудКом').
	os_47427 = os_47427 + GetSumProd(loan.cont-code,'КредБудРКО').
	os_47427 = os_47427 + GetSumProd(loan.cont-code,'КредТ').
	os_45915 = GetSumProd(loan.cont-code,'КредПр').
	os_sr_91604 = GetSumProd(loan.cont-code,'КредТВ').
	os_pr_91604 = GetSumProd(loan.cont-code,'КредПр%В').
	os_pr_91604_p = GetSumProd(loan.cont-code,'КредПр%В1').
	cost_zakl1 = os_455 + os_45815 + os_47427 + os_47427_p + os_45915 
				+ os_sr_91604 + os_pr_91604_p + penalty_os + penalty_prc .
	cost_zakl = os_455 + os_45815 + os_47427 + os_47427_p + os_45915 
				+ os_sr_91604 + os_pr_91604_p + penalty_os + penalty_prc + round(( os_455) * 0.035, 2 ).
	
	PUT UNFORMATTED
/* Идентификатор кредитного договора во внешней системе */
	iRecIDloan 
	"^"
	STRING(os_455, ">>>>>>>>>>>9.99")
	"^"
	STRING(os_45815, ">>>>>>>>>>>9.99")
	"^"
	STRING(os_47427, ">>>>>>>>>>>9.99")
	"^"
	STRING(os_47427_p, ">>>>>>>>>>>9.99")
	"^"
	STRING(os_45915, ">>>>>>>>>>>9.99")
	"^"
    STRING(os_sr_91604, ">>>>>>>>>>>9.99")
	"^"
    STRING(os_pr_91604, ">>>>>>>>>>>9.99")
	"^"
    STRING(os_pr_91604_p, ">>>>>>>>>>>9.99")
	"^"
    STRING(penalty_os, ">>>>>>>>>>>9.99")
	"^"
    STRING(penalty_prc, ">>>>>>>>>>>9.99")
	"^"
    STRING(cost_zakl, ">>>>>>>>>>>9.99")
	"^"
/* Система */
	'ПАО "ПЛЮС БАНК"'
	CHR(13) + CHR(10)
	.
END.

OUTPUT CLOSE.
{intrface.del}
