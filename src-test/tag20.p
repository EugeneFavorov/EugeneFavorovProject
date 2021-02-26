
DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{tmprecid.def}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER bloan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan-cond FOR loan-cond.
DEFINE BUFFER comm-rate FOR comm-rate.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE VAR sum_com AS DECIMAL NO-UNDO INIT 0.
DEFINE VAR date_str AS DATE NO-UNDO INIT ?. /* Дата оплаты страхового договора жизни и здоровья */
DEFINE VAR sum_str AS DECIMAL NO-UNDO INIT 0. /* Сумма страхового договора жизни и здоровья */
DEFINE VAR vidstr AS CHAR NO-UNDO INIT "".
DEFINE VAR mCont_Code AS CHAR NO-UNDO INIT "".
DEFINE VAR mAcct408 AS CHAR NO-UNDO INIT "".

FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FIND FIRST loan-cond WHERE 
		loan-cond.contract EQ loan.contract
		AND loan-cond.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.
	IF AVAIL loan-cond THEN DO:
		FIND FIRST comm-rate WHERE comm-rate.commission = '%Выд'
			AND comm-rate.kau = loan.contract + ',' + loan.cont-code
			and comm-rate.since = loan-cond.since NO-LOCK NO-ERROR.
		IF AVAIL comm-rate THEN sum_com = comm-rate.rate-comm.
	END.
findbloan:
	FOR EACH bloan WHERE CAN-DO("insurance", bloan.class-code)
		AND bloan.contract EQ 'СТРАХ'
		AND bloan.parent-cont-code EQ loan.cont-code
		AND bloan.parent-contract EQ 'Кредит'
		NO-LOCK BY bloan.open-date:
		vidstr = GetXAttrValueEx("loan",
		bloan.contract + "," + bloan.cont-code,
		"VidStr",
		"").
		IF vidstr begins 'Жизнь' THEN DO:
			FIND FIRST term-obl WHERE
				term-obl.idnt EQ 1
				AND term-obl.cont-code EQ bloan.cont-code
				AND term-obl.contract EQ 'СТРАХ' NO-LOCK NO-ERROR.
				IF AVAIL term-obl THEN sum_str = term-obl.amt-rub.
				mCont_Code = bloan.doc-ref.
			LEAVE findbloan.
		END.
	END.
	
	IF sum_str > 0 THEN DO:
		IF loan.open-date < DATE("30.04.2015") THEN
			FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract 
				AND loan-acct.cont-code = REPLACE(loan.cont-code,'@0000','@0400')
				AND loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
		ELSE FIND FIRST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
		IF AVAIL loan-acct THEN	DO:
			FOR EACH op-entry WHERE op-entry.acct-db = loan-acct.acct 
				AND op-entry.op-status begins "√" 
				AND op-entry.amt-rub > 0 NO-LOCK,
				FIRST op OF op-entry WHERE op.details MATCHES '*' + mCont_Code + '*' NO-LOCK BY op-entry.op-date:
				date_str = op-entry.op-date.
				LEAVE.
			END.
		
		
		END.
	END.
	
	PUT UNFORMATTED
/* Идентификатор кредитного договора во внешней системе */
	iRecIDloan 
	"^".
	IF sum_com > 0 THEN PUT UNFORMATTED	
		STRING(sum_com, ">>>>>>>>>>>9.99").
	PUT UNFORMATTED	"^".
	IF date_str <> ? THEN PUT UNFORMATTED
		STRING(YEAR(date_str), "9999") + STRING(MONTH(date_str), "99") + STRING(DAY(date_str), "99").
	PUT UNFORMATTED	"^".
	IF sum_str > 0 THEN PUT UNFORMATTED
		STRING(sum_str, ">>>>>>>>>>>9.99").
	PUT UNFORMATTED "^^".	
	IF sum_str > 0 THEN PUT UNFORMATTED "1".
	PUT UNFORMATTED "^".	
	PUT UNFORMATTED	
/* Система */
	'ПАО "ПЛЮС БАНК"'
	CHR(13) + CHR(10)
	.
END.
RETURN.



{intrface.del}
