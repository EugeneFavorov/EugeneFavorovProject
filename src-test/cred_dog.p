DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
DEFINE INPUT  PARAMETER iHeader AS INT64 NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan1 FOR loan.
DEFINE BUFFER loan-cond FOR loan-cond.
DEFINE BUFFER loan-cond1 FOR loan-cond.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan-var FOR loan-var.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER comm-rate1 FOR comm-rate.
DEFINE BUFFER comm-rate2 FOR comm-rate.
DEFINE BUFFER comm-rate3 FOR comm-rate.


DEFINE VARIABLE mOS_455 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mOS_47427 AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mOS_45815 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mOS_45915 AS DECIMAL NO-UNDO.
/*
DEFINE TEMP-TABLE ttLoan 
	FIELD DOG_ID
	FIELD URNO
	FIELD TARIFF
	FIELD CUR
	FIELD DOPEN
	FIELD DFINAL
	FIELD DOG_SUMMA
	FIELD DNBKI
	FIELD SUBJ_ID
	FIELD OS_455
	FIELD OS_47427
	FIELD OS_45815
	FIELD OS_45915
	FIELD DATE_PAY
	FIELD PR_ST
	FIELD SUMMA_PLAT
	FIELD PENALTY OS
	FIELD PENALTY PRC
	FIELD PRODUCT
	FIELD COST_BUY
	FIELD DOG_KIND
	FIELD PERIOD
	FIELD SHEDULE_TYPE
.
*/
IF iHeader EQ 1 THEN
DO:
{cred_dog.i}
END.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op-entry FOR op-entry.
/*
OUTPUT TO VALUE("tag1.txt") CONVERT TARGET "1251".
*/
FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	IF loan.open-date LT DATE("30.04.2015") THEN
		FIND FIRST loan1
			WHERE loan1.filial-id EQ "0400"
			AND loan1.contract EQ loan.contract
			AND loan1.cont-code EQ ENTRY(1, loan.cont-code, "@") + "@0400"
		NO-LOCK NO-ERROR.
	IF NOT AVAILABLE(loan1) THEN
		FIND FIRST loan1
		    WHERE RECID(loan1) EQ RECID(loan)
		    NO-LOCK NO-ERROR.

	FIND FIRST loan-cond
		WHERE loan-cond.contract EQ loan.contract
		AND loan-cond.cont-code EQ loan.cont-code
	    NO-LOCK NO-ERROR.

	FIND LAST loan-cond1
		WHERE loan-cond1.contract EQ loan.contract
		AND loan-cond1.cont-code EQ loan.cont-code
	    NO-LOCK NO-ERROR.

	FIND FIRST term-obl
		WHERE term-obl.contract EQ loan.contract
		AND term-obl.cont-code EQ loan.cont-code
		AND term-obl.idnt = 2
		AND term-obl.end-date EQ loan-cond.since
	    NO-LOCK NO-ERROR.
/* КРЕДИТ */
	FOR EACH loan-acct
		WHERE loan-acct.contract EQ loan.contract
		AND loan-acct.cont-code EQ loan.cont-code
		AND loan-acct.acct-type EQ "Кредит"
		AND loan-acct.since LT end-date
		BY loan-acct.since DESC:
		LEAVE.
	END.

    RUN acct-pos IN h_base (
        loan-acct.acct,
        loan-acct.currency,
        end-date + 1,
        end-date + 1, "√"
        ).

    mOS_455 = ABSOLUTE(sh-in-bal).

/* КРЕДТ */
	FOR EACH loan-acct
		WHERE loan-acct.contract EQ loan.contract
		AND loan-acct.cont-code EQ loan.cont-code
		AND loan-acct.acct-type EQ "КредТ"
		AND loan-acct.since LT end-date
		BY loan-acct.since DESC:
		LEAVE.
	END.

    RUN acct-pos IN h_base (
        loan-acct.acct,
        loan-acct.currency,
        end-date + 1,
        end-date + 1, "√"
        ).

    mOS_47427 = ABSOLUTE(sh-in-bal).

/* КредПр */
	FOR EACH loan-acct
		WHERE loan-acct.contract EQ loan.contract
		AND loan-acct.cont-code EQ loan.cont-code
		AND loan-acct.acct-type EQ "КредПр"
		AND loan-acct.since LT end-date
		BY loan-acct.since DESC:
		LEAVE.
	END.
	IF AVAILABLE(loan-acct) THEN
	DO:
	    RUN acct-pos IN h_base (
	        loan-acct.acct,
	        loan-acct.currency,
	        end-date + 1,
	        end-date + 1, "√"
	        ).

	    mOS_45815 = ABSOLUTE(sh-in-bal).
    END.
    ELSE
	    mOS_45815 = 0.

/* КредПр% */
	FOR EACH loan-acct
		WHERE loan-acct.contract EQ loan.contract
		AND loan-acct.cont-code EQ loan.cont-code
		AND loan-acct.acct-type EQ "КредПр%"
		AND loan-acct.since LT end-date
		BY loan-acct.since DESC:
		LEAVE.
	END.

	IF AVAILABLE(loan-acct) THEN
	DO:
	    RUN acct-pos IN h_base (
	        loan-acct.acct,
	        loan-acct.currency,
	        end-date + 1,
	        end-date + 1, "√"
	        ).

	    mOS_45915 = ABSOLUTE(sh-in-bal).
	END.
	ELSE
	    mOS_45915 = 0.

	FOR EACH comm-rate1
		WHERE comm-rate1.kau EQ loan.contract + "," + loan.cont-code
		AND comm-rate1.commission EQ "%Кред"
		AND comm-rate1.since LT end-date
		NO-LOCK
		BY comm-rate1.since DESC:
		LEAVE.
	END.

	FOR EACH comm-rate2
		WHERE comm-rate2.kau EQ loan.contract + "," + loan.cont-code
		AND comm-rate2.commission EQ "Пеня-К"
		AND comm-rate2.since LT end-date
		NO-LOCK
		BY comm-rate2.since DESC:
		LEAVE.
	END.

	FOR EACH comm-rate3
		WHERE comm-rate3.kau EQ loan.contract + "," + loan.cont-code
		AND comm-rate3.commission EQ "Пеня%К"
		AND comm-rate3.since LT end-date
		NO-LOCK
		BY comm-rate3.since DESC:
		LEAVE.
	END.



	PUT UNFORMATTED
/* Уникальный ID кредитного договора в вашей БД (если выгрузка происходит с нескольких локальных баз, например когда каждый филиал живет на своей базе - ID могут пересекаться.) */
	iRecIDloan 
	";"
/* Юридический номер кредитного договора */
	TRIM(ENTRY(1, loan.cont-code, "@"))
	";"
/* Тарифный план (заполняется сотрудником РК) */	
	"0"
	";"
/* Валюта */
	IF loan1.currency EQ "" THEN "810" ELSE loan1.currency
	";"
/* Дата валютирования */
	STRING(loan-cond.since, "99.99.9999")
	";"
/* Дата погашения  договора */
	STRING(loan.end-date, "99.99.9999")
	";"
/* Сумма */
	TRIM(STRING(term-obl.amt-rub, ">>>>>>>>>>>9.99"))
	";"
/* Дата договора  */
	STRING(loan.open-date, "99.99.9999")
	";"
/* Клиент по договору (идентификатор клиента во внешней системе) */
	TRIM(STRING(loan1.cust-id))
	";"
	" "
	";"
/* Остаток срочной ссудной задолженности (без разделителей группы разрядов, два десятичных знака) на дату сделки */
    TRIM(STRING(mOS_455, ">>>>>>>>>>>9.99"))
	";"
/* Остаток срочных процентов текущего расчетного периода, учтенных на балансе  (без разделителей группы разрядов, два десятичных знака) на дату сделки */
    TRIM(STRING(mOS_47427, ">>>>>>>>>>>9.99"))
	";"
/* Остаток просроченной ссудной задолженности (без разделителей группы разрядов, два десятичных знака) на дату сделки */
    TRIM(STRING(mOS_45815, ">>>>>>>>>>>9.99"))
	";"
/* Остаток просроченных процентов учтенных на балансе (без разделителей группы разрядов, два десятичных знака) на дату сделки */
    TRIM(STRING(mOS_45915, ">>>>>>>>>>>9.99"))
	";"
/* День платежа (два знака) на дату сделки */
	loan-cond1.cred-date
	";"
/* Процентная ставка по договору(без разделителей группы разрядов, два десятичных знака) */
	(IF AVAILABLE(comm-rate2) THEN (IF comm-rate1.rate-comm EQ 0 THEN "0" ELSE TRIM(STRING(comm-rate1.rate-comm, ">>>>9.99"))) ELSE "0")
	";"
/* Сумма платежа. Порядок ввода значения в данное поле: */
	REPLACE(GetXAttrValueEx("loan-cond",
		loan-cond1.contract + "," + loan-cond1.cont-code + "," + STRING(loan-cond1.since),
		"АннуитПлат",
		"0"
		), ",", "")
	";"
/* Процентная ставка неустойки на просроченный ОД, как правило ежедневная (Только для неустойки. 
	Если вы используете %% на просроченный основной долг, которые равны основной годовой ставке по кредиту, в этом поле ставьте ноль) */
	(IF AVAILABLE(comm-rate2) THEN (IF comm-rate2.rate-comm EQ 0 THEN "0" ELSE TRIM(STRING(comm-rate2.rate-comm, ">>>>9.999"))) ELSE "0")
	";"
/* Процентная ставка неустойки на просроченные проценты, как правило ежедневная (Только для неустойки. Если вы используете %% 
	на просроченный основной долг, которые равны основной годовой ставке по кредиту, в этом поле ставьте ноль)  */
	(IF AVAILABLE(comm-rate3) THEN (IF comm-rate3.rate-comm EQ 0 THEN "0" ELSE TRIM(STRING(comm-rate3.rate-comm, ">>>>9.999"))) ELSE "0")
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
/* Банковский продукт */
	IF loan.class-code EQ "loan_aig" THEN "Ипотека" ELSE "Автокредит"
	";"
/* Цена покупки кредита (без разделителей группы разрядов, два десятичных знака) */
	TRIM(STRING( (mOS_455 + mOS_45815 + mOS_47427 + mOS_45915) + ROUND((mOS_455 + mOS_45815) / 100 * 6, 2), ">>>>>>>>>>>9.99"))
	";"
	"2"
	";"
/* Срок (в днях) */
	TRIM(STRING(loan.end-date - loan.open-date + 1, ">>>>>>9"))
	";"
/* Алгоритм погашения основного долга */
	"1"
	";"
/* Тип процентной ставки 1 - % 2 - фиксированная*/
	(IF comm-rate2.rate-fixed THEN "3" ELSE "2")
	CHR(13) + CHR(10)
	.

END.
/*
OUTPUT CLOSE.
*/
{intrface.del}
