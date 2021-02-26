
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
DEF VAR NumDog AS CHAR NO-UNDO.
/* Регистрационный номер свидетельства о регистрации в нотариате */
DEF VAR	regNum AS CHAR NO-UNDO.
/* Дата нотариального удостоверения */
DEF VAR	regDate AS CHAR NO-UNDO.
/* VIN */
DEF VAR vin AS CHAR NO-UNDO.
/* Марка Автомобиля */
DEF VAR	mark AS CHAR NO-UNDO.
/* Цвет Автомобиля */
DEF VAR colour AS CHAR NO-UNDO.
/* Год Выпуска */
DEF VAR	yearAuto AS CHAR NO-UNDO.
/* Двигатель№ */
DEF VAR	engine AS CHAR NO-UNDO.
/* Модель автомобиля */
DEF VAR	model AS CHAR NO-UNDO.
/* Шасси № */
DEF VAR shassis AS CHAr NO-UNDO.
/* ПТС Серия*/
DEF VAR	ptsSer AS CHAR NO-UNDO.
/* ПТС Номер */
DEF VAR	ptsNum AS CHAR NO-UNDO.
/* ПТС дата выдачи */
DEF VAR	ptsDate AS CHAR NO-UNDO.
/* ПТС кем выдан */
DEF VAR	ptsWho AS CHAR NO-UNDO.


FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FOR EACH term-obl WHERE
		term-obl.cont-code EQ loan.cont-code
		AND term-obl.contract EQ loan.contract
		AND term-obl.idnt EQ 5
		AND (term-obl.end-date = ? OR term-obl.end-date >= TODAY) 
		NO-LOCK BY term-obl.fop-date:
		
		FIND FIRST signs WHERE signs.file-name = 'term-obl'
			AND signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
			AND signs.code = 'reg-zalog' NO-LOCK NO-ERROR.
		IF AVAIL signs THEN DO:
			regNum = ENTRY(2,signs.xattr-value).
			regDate = STRING(YEAR(signs.date-value), "9999") + STRING(MONTH(signs.date-value), "99") + STRING(DAY(signs.date-value), "99").
		END.
		ELSE DO:
		    regNum = ''.
		    regDate = ''.
		    FIND FIRST signs WHERE signs.file-name = 'term-obl'
		    and signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
		    and signs.code = 'NotifRefNumber' no-lock no-error.
		    IF AVAIL signs THEN DO:
			    regNum = signs.xattr-value.
			END.
		END.
		NumDog = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"НомДогОб",
		"").
		vin = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCVIN",
		"").
		mark = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCbrand",
		"").
		colour = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCcolor",
		"").
		yearAuto = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCyear",
		"").		
		engine = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmotor",
		"").
		model = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmodel",
		"").
		shassis = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCchassis",
		"").
		ptsSer = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCSER",
		"").
		ptsNum = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCNUMB",
		"").
		FIND FIRST signs WHERE signs.file-name = 'term-obl'
			AND signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
			AND signs.code = 'TCDATE' NO-LOCK NO-ERROR.
		IF AVAIL signs THEN DO:
			ptsDate = STRING(YEAR(signs.date-value), "9999") + STRING(MONTH(signs.date-value), "99") + STRING(DAY(signs.date-value), "99").
		END.
		ELSE DO:
			ptsDate = ''.
		END.
		
	PUT UNFORMATTED
	/* Идентификатор залогодателя по договору обеспечения */
	loan.cust-id 
	"^"
	/* Идентификатор договора обеспечения во внешней системе */
	recid(term-obl)
	"^"
	/* Номер договора обеспечения */
	NumDog
	"^"
	/* Дата и время регистрации залога в нотариате */
	"^"
	/* Реестровый номер свидетельства о регистрации в нотариате */
	"^"
	/* Регистрационный номер свидетельства о регистрации в нотариате */
	regNum
	"^"
	/* Дата нотариального удостоверения */
	regDate
	"^"
	/* VIN */
	vin
	"^"
	/* Марка Автомобиля */
	mark
	"^"
	/* Цвет Автомобиля */
	colour
	"^"
	/* Год Выпуска */
	yearAuto
	"^"
	/* Двигатель№ */
	engine
	"^"
	/* Модель автомобиля */
	model
	"^"
	/* Шасси № */
	shassis
	"^"
	/* ПТС Серия*/
	ptsSer
	"^"
	/* ПТС Номер */
	ptsNum
	"^"
	/* ПТС дата выдачи */
	ptsDate
	"^"	
	/* ПТС кем выдан */
	ptsWho
	"^"
	/* Система */
	'ПАО "ПЛЮС БАНК"'
	CHR(13) + CHR(10)
	.
END.
END.
RETURN.

{intrface.del}
