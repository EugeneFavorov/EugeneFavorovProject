{globals.i}
{sh-defs.i}
{intrface.get xclass }
{intrface.get tmess }
{intrface.get cust }

DEFINE BUFFER acct-beg FOR acct.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bacct-pos FOR acct-pos.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.

DEFINE VARIABLE mFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmount-beg AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmount-end AS DECIMAL NO-UNDO.
DEFINE VARIABLE mDolg AS DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE mBegDate AS DATE NO-UNDO.
DEFINE VARIABLE mEndDate AS DATE NO-UNDO.
DEFINE VARIABLE mNullDate AS DATE NO-UNDO.
DEFINE VARIABLE mAcctBeg AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOborDB AS DECIMAL NO-UNDO.
DEFINE VARIABLE mOborCr AS DECIMAL NO-UNDO.
DEFINE VARIABLE mOborCrAll AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctDetails AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.


DEFINE TEMP-TABLE tt-acct-pos LIKE acct-pos.


DEFINE TEMP-TABLE tt-nalog
	FIELD cust-cat AS CHARACTER
	FIELD cust-id AS INT64
	FIELD NameCli AS CHARACTER
	FIELD acct-beg AS CHARACTER
	FIELD acct AS CHARACTER
	FIELD filial-id AS CHARACTER
	FIELD nulldate AS DATE
	FIELD amt-beg AS DECIMAL
	FIELD amt-end AS DECIMAL
	FIELD amt-db AS DECIMAL
	FIELD amt-cr AS DECIMAL
	FIELD amt-2015 AS DECIMAL
	FIELD amt-dolg90 AS DECIMAL
	FIELD amt-dolg4590 AS DECIMAL
	FIELD amt-dolg45 AS DECIMAL
	FIELD amt-res AS DECIMAL
	FIELD date-otc AS DATE
INDEX cc cust-cat cust-id acct.

FUNCTION OborDb RETURN DECIMAL(
	INPUT iBegDate AS DATE,
	INPUT iEndDate AS DATE,
	INPUT iAcct AS CHARACTER
	) FORWARD.

FUNCTION OborCr RETURN DECIMAL(
	INPUT iBegDate AS DATE,
	INPUT iEndDate AS DATE,
	INPUT iAcct AS CHARACTER
	) FORWARD.

FUNCTION Obesp LOGICAL(
	INPUT iAcct AS CHARACTER,
	INPUT iEndDate AS DATE
	) FORWARD.
/*
FUNCTION RetDolg RETURN DECIMAL(
	INPUT iBegDate AS DATE,
	INPUT iEndDate AS DATE,
	INPUT iAcct AS CHARACTER,
	INPUT iAcctPos AS DECIMAL
	) FORWARD.
*/

{getdate.i}

OUTPUT to VALUE("nalog420fz.log") CONVERT TARGET "1251".

mBegDate = DATE("01.01." + STRING(YEAR(end-date), "9999")).
mEndDate = end-date.

IF mEndDate GT DATE("30.04.2015") THEN
	mFilial = "0000".
ELSE
	mFilial = "0400".

FOR EACH acct
	WHERE acct.filial-id EQ mFilial
	AND CAN-DO("!45918*,459*,91604*,91603*", acct.acct)  
/*	AND CAN-DO("91604810004050000095*,91604810704050000094*", acct.acct)  */
	AND acct.open-date LT mEndDate
	AND (
		acct.close-date EQ ?
		OR acct.close-date GT mEndDate
		)
	NO-LOCK:
	ASSIGN
	mAmount-beg = 0
	mAmount-end = 0.

	RUN acct-pos IN h_base (
		acct.acct,
        acct.currency,
        end-date,
        end-date, "√"
        ).

	mAmount-end = ABSOLUTE(sh-bal).
	IF mAmount-end GT 0 THEN
	IF Obesp(INPUT acct.acct,
		INPUT mEndDate
		) THEN
	DO:
	DO:
		mI = mI + 1.
		PUT UNFORMATTED mI SKIP.
		IF mFilial EQ "0000" 
			AND acct.open-date LT DATE("30.04.2015") 
			THEN
			mAcctBeg = REPLACE(acct.acct, "@0000", "@0400").
		ELSE
			mAcctBeg = acct.acct.
		mNullDate = mBegDate.
		FOR EACH bacct-pos 
			WHERE 
			bacct-pos.filial-id EQ "0000"
			AND bacct-pos.since GE DATE("30.04.2015")
			AND bacct-pos.since LE mEndDate
			AND bacct-pos.acct EQ acct.acct
			AND bacct-pos.balance EQ 0
			NO-LOCK
			BY bacct-pos.since DESC:
			LEAVE.
		END.
		IF AVAILABLE(bacct-pos) THEN
			mNullDate = bacct-pos.since + 1.
		ELSE
		DO:
			FOR EACH bacct-pos 
				WHERE 
				bacct-pos.filial-id EQ "0400"
				AND bacct-pos.since GE mBegDate
				AND bacct-pos.since LT DATE("30.04.2015")
				AND bacct-pos.acct EQ REPLACE(acct.acct, "@0000", "@0400")
				AND bacct-pos.balance EQ 0
				NO-LOCK
				BY bacct-pos.since DESC:
				LEAVE.
			END.
			IF AVAILABLE(bacct-pos) THEN
				mNullDate = bacct-pos.since + 1.
		END.

/*		IF mNullDate EQ mBegDate THEN
		DO: */
			RUN acct-pos IN h_base (
				mAcctBeg,
		        acct.currency,
		        mBegDate,
		        mBegDate, "√"
		        ).
		
			mAmount-beg = ABSOLUTE(sh-bal).
/*		END. */

        RUN GetCustName IN h_base (acct.cust-cat,
                                   acct.cust-id,
                                   ?,
                                   OUTPUT mAcctDetails[1],
                                   OUTPUT mAcctDetails[2],
                                   INPUT-OUTPUT mAcctDetails[3]).
        mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
        mCliName = mAcctDetails[1].

		mOborCrAll = OborCr(mBegDate,
			mEndDate,
			ENTRY(1, acct.acct, "@")
			).

			CREATE tt-nalog.
			ASSIGN
			tt-nalog.cust-cat = acct.cust-cat
			tt-nalog.cust-id = acct.cust-id
			tt-nalog.NameCli = mCliName
			tt-nalog.acct-beg = mAcctBeg
			tt-nalog.acct = acct.acct
			tt-nalog.filial-id = ENTRY(2, acct.acct, "@")
			tt-nalog.amt-beg = mAmount-beg 
			tt-nalog.amt-end = mAmount-end
			tt-nalog.amt-cr = mOborCrAll 
			tt-nalog.nulldate = mNullDate
			.
			IF mOborCrAll GT mAmount-beg THEN 
				tt-nalog.amt-2015 = mAmount-end.
			ELSE
				tt-nalog.amt-2015 = OborDb(tt-nalog.nulldate,
											mEndDate,
											ENTRY(1, acct.acct, "@")
											).

			RUN RetDolg(INPUT mNullDate,
				INPUT mEndDate,
				INPUT ENTRY(1, acct.acct, "@"),
				tt-nalog.amt-2015,
				INPUT-OUTPUT mDolg
				).

/*			mOborCr = 0.

			/* до 45 дней */
			RUN acct-pos IN h_base (
				acct.acct,
		        acct.currency,
		        (IF mNullDate LT end-date - 44 + 1 THEN end-date - 44 + 1 ELSE mNullDate),
		        end-date, "√"
		        ).

			tt-nalog.amt-dolg45 = ABSOLUTE(sh-db) - (IF ABSOLUTE(sh-db) GT ABSOLUTE(sh-bal) THEN ABSOLUTE(sh-cr) ELSE 0).

			/* от 45 до 90 дней */
			IF mNullDate LT end-date - 44 THEN
			DO:
				RUN acct-pos IN h_base (
					acct.acct,
			        acct.currency,
			        IF mNullDate LT end-date - 90 + 1 THEN end-date - 90 + 1 ELSE mNullDate,
			        end-date - 44, "√"
			        ).

				tt-nalog.amt-dolg4590 = ABSOLUTE(sh-db) - (IF ABSOLUTE(sh-db) GT ABSOLUTE(sh-bal) THEN ABSOLUTE(sh-cr) ELSE 0).
			END.
			ELSE
				tt-nalog.amt-dolg4590 = 0. 
*/
/*			mOborDb = OborDb(mEndDate - 44 + 1,
				mEndDate,
				ENTRY(1, tt-nalog.acct, "@")
				).
			IF mOborCrAll GT mAmount-beg THEN
			DO:
				mOborCr = OborCr(mEndDate - 44 + 1,
					mEndDate,
				ENTRY(1, tt-nalog.acct, "@")
					).
			END.
			
			tt-nalog.amt-dolg45 = mOborDb - mOborCr.
*/
			/* от 45 до 90 дней */
/*			mOborDb = OborDb(mEndDate - 90 + 1,
				mEndDate - 44,
				ENTRY(1, tt-nalog.acct, "@")
				).
			IF mOborCrAll GT mAmount-beg THEN
			DO:
				mOborCr = OborCr(mBegDate,
					mEndDate,
				ENTRY(1, tt-nalog.acct, "@")
					).
			END.
			tt-nalog.amt-dolg4590 = mOborDb - mOborCr.
*/			
			/* свыше 90 дней*/
			tt-nalog.amt-dolg45 = mDolg[1].
			tt-nalog.amt-dolg4590 = mDolg[2].
			tt-nalog.amt-dolg90 = mDolg[3].
			tt-nalog.amt-res = tt-nalog.amt-dolg90 + tt-nalog.amt-dolg4590 * 0.5.
	END.
	END.
END.
OUTPUT CLOSE.

OUTPUT to VALUE("nalog420fz.csv") CONVERT TARGET "1251".

PUT UNFORMATTED 
"Наименование заемщика"
";"
"Л/С (91604,91603,459)"
/* ";"
"mBegDate"
";"
"mEndDate"
";"
"Остаток на начало периода"
";"
"Обороты кредит" */
";"
"Остаток всего"
";"
"Остаток, возникший в 2015 году"
";"
"Задолженность со сроком возникновения свыше 90 календарных дней"
";"
"Задолженность со сроком возникновения от 45 до 90 календарных дней"
";"
"Задолженность со сроком возникновения менее 45 календарных дней"
";"
"Размер резерва"
SKIP
"1"
";"
"2"
/* ";"
"2a"
";"
"2b"
";"
"2c"
";"
"2d" */
";"
"3"
";"
"4"
";"
"5"
";"
"6"
";"
"7"
";"
"8"
SKIP.

FOR EACH tt-nalog:
	PUT UNFORMATTED 
	tt-nalog.NameCli
	";'"
	TRIM(ENTRY(1, tt-nalog.acct, "@"))
/*	";"
	tt-nalog.nulldate
	";"
	mEndDate
	";"
	tt-nalog.amt-beg
	";"
	tt-nalog.amt-cr */
	";"
	tt-nalog.amt-end
	";"
	tt-nalog.amt-2015
	";"
	tt-nalog.amt-dolg90
	";"
	tt-nalog.amt-dolg4590
	";"
	tt-nalog.amt-dolg45
	";"
	tt-nalog.amt-res
	SKIP
	.
END.
OUTPUT CLOSE.

RUN sndbispc ("file=" + "nalog420fz.csv" + ";class=bq").

{intrface.del}


/* functions */
FUNCTION OborDb RETURN DECIMAL(
	INPUT iBegDate AS DATE,
	INPUT iEndDate AS DATE,
	INPUT iAcct AS CHARACTER
	):
	DEFINE BUFFER op-entry FOR op-entry.
	DEFINE VARIABLE vAmt AS DECIMAL INIT 0 NO-UNDO.

	FOR EACH op-entry
		WHERE op-entry.filial-id EQ "0400"
		AND op-entry.op-date GE iBegDate
		AND op-entry.op-date LE iEndDate
		AND op-entry.acct-db BEGINS iAcct
		AND op-entry.user-id NE "IRBIS"
/*		AND NOT (op-entry.acct-cr BEGINS SUBSTRING(iAcct, 1, 3))  */
		NO-LOCK, op
		WHERE op.op EQ op-entry.op
		AND op.op-status GE CHR(251) 
		NO-LOCK:
		vAmt = vAmt + op-entry.amt-rub.
	END.

	FOR EACH op-entry
		WHERE op-entry.filial-id EQ "0000"
		AND op-entry.op-date GE iBegDate
		AND op-entry.op-date LE iEndDate
		AND op-entry.acct-db BEGINS iAcct
		AND op-entry.user-id NE "IRBIS"
/*		AND NOT (op-entry.acct-cr BEGINS SUBSTRING(iAcct, 1, 3))  */
		NO-LOCK, op
		WHERE op.op EQ op-entry.op
		AND op.op-status GE CHR(251) 
		NO-LOCK:
		vAmt = vAmt + op-entry.amt-rub.
	END.
	RETURN vAmt.
END FUNCTION.
/*  */
FUNCTION OborCr DECIMAL(
	INPUT iBegDate AS DATE,
	INPUT iEndDate AS DATE,
	INPUT iAcct AS CHARACTER

	):
	DEFINE BUFFER op-entry FOR op-entry.
	DEFINE VARIABLE vAmt AS DECIMAL INIT 0 NO-UNDO.

	FOR EACH op-entry
		WHERE op-entry.filial-id EQ "0400"
		AND op-entry.op-date GE iBegDate
		AND op-entry.op-date LE iEndDate
		AND op-entry.acct-cr BEGINS iAcct
		AND op-entry.user-id NE "IRBIS"
/*		AND NOT (op-entry.acct-db BEGINS SUBSTRING(iAcct, 1, 3))  */
		NO-LOCK, op
		WHERE op.op EQ op-entry.op
		AND op.op-status GE CHR(251) 
		NO-LOCK:
		vAmt = vAmt + op-entry.amt-rub.
	END.

	FOR EACH op-entry
		WHERE op-entry.filial-id EQ "0000"
		AND op-entry.op-date GE iBegDate
		AND op-entry.op-date LE iEndDate
		AND op-entry.acct-cr BEGINS iAcct
		AND op-entry.user-id NE "IRBIS"
/*		AND NOT (op-entry.acct-db BEGINS SUBSTRING(iAcct, 1, 3)) */
		NO-LOCK, op
		WHERE op.op EQ op-entry.op
		AND op.op-status GE CHR(251) 
		NO-LOCK:
		vAmt = vAmt + op-entry.amt-rub.
	END.
	RETURN vAmt.
END FUNCTION.
/**/
FUNCTION Obesp LOGICAL(
	INPUT iAcct AS CHARACTER,
	INPUT iEndDate AS DATE
	):
	DEFINE BUFFER loan-acct FOR loan-acct.
	DEFINE BUFFER term-obl FOR term-obl.
	DEFINE VARIABLE vRet AS LOGICAL INIT NO NO-UNDO.

	FOR EACH loan-acct 
		WHERE 
/*		loan-acct.filial-id EQ "0000"
		AND */
		loan-acct.contract EQ "Кредит"
		AND loan-acct.acct EQ iAcct
		AND loan-acct.since LE iEndDate
		NO-LOCK
		BY loan-acct.since DESC:
		LEAVE.
	END.
	IF AVAILABLE(loan-acct) THEN
	DO:
		FOR EACH term-obl
			WHERE term-obl.contract EQ loan-acct.contract
			AND term-obl.cont-code EQ loan-acct.cont-code
			AND term-obl.class-code EQ "term-obl-gar"
			AND term-obl.fop-date LE iEndDate
			AND term-obl.end-date LT iEndDate
			NO-LOCK
			BY term-obl.fop-date DESC:
			LEAVE.
		END.
	END.
	IF AVAILABLE(term-obl) THEN
		vRet = YES.

	RETURN vRet.

END FUNCTION.
/**/

/* RetDolg */
PROCEDURE RetDolg: 
	DEFINE INPUT PARAMETER iBegDate AS DATE NO-UNDO.
	DEFINE INPUT PARAMETER iEndDate AS DATE NO-UNDO.
	DEFINE INPUT PARAMETER iAcct AS CHARACTER NO-UNDO.
	DEFINE INPUT PARAMETER iAcctPos AS DECIMAL NO-UNDO.
	DEFINE INPUT-OUTPUT PARAMETER vRet AS DECIMAL EXTENT 3 NO-UNDO.
	

	DEFINE BUFFER op-entry FOR op-entry.
	DEFINE BUFFER acct-pos FOR acct-pos.
	DEFINE BUFFER acct-pos1 FOR acct-pos.

	DEFINE VARIABLE vAmtDb AS DECIMAL INIT 0 NO-UNDO.
	DEFINE VARIABLE vAmtCr AS DECIMAL INIT 0 NO-UNDO.
	DEFINE VARIABLE vAmt AS DECIMAL INIT 0 NO-UNDO.
/*	DEFINE VARIABLE vRet AS DECIMAL EXTENT 3 NO-UNDO. */

	{empty tt-acct-pos}
	
	FOR EACH acct-pos
		WHERE 
		acct-pos.filial-id = "0400"
		AND acct-pos.since GE iBegDate
		AND acct-pos.since LT DATE("30.04.2015") 
		AND acct-pos.acct EQ TRIM(iAcct) + "     @0400"
		NO-LOCK
		BY acct-pos.since
		QUERY-TUNING(NO-INDEX-HINT):
		CREATE tt-acct-pos.
		BUFFER-COPY acct-pos TO tt-acct-pos.
		tt-acct-pos.acct = iAcct.
	END.
	IF AVAILABLE(tt-acct-pos) THEN
		vAmt = tt-acct-pos.balance.

	FOR EACH acct-pos
		WHERE 
		acct-pos.filial-id = "0000"
		AND acct-pos.since GE DATE("30.04.2015") 
		AND acct-pos.since LE iEndDate 
		AND acct-pos.acct EQ TRIM(iAcct) + "     @0000"
		NO-LOCK
		BY acct-pos.since
		QUERY-TUNING(NO-INDEX-HINT):
		CREATE tt-acct-pos.
		BUFFER-COPY acct-pos TO tt-acct-pos.
		tt-acct-pos.acct = iAcct.
		IF acct-pos.since EQ DATE("30.04.2015") THEN
		DO:
			FIND FIRST acct-pos1
				WHERE
				acct-pos1.filial-id = "0400"
				AND acct-pos1.since EQ DATE("30.04.2015") 
				AND acct-pos1.acct EQ TRIM(iAcct) + "     @0400"
				NO-LOCK NO-ERROR.
			IF AVAILABLE(acct-pos1) THEN
				ASSIGN
				tt-acct-pos.debit = acct-pos1.debit
				tt-acct-pos.credit = acct-pos1.credit.
		END.
	END.

	IF AVAILABLE(tt-acct-pos) THEN
		vAmt = tt-acct-pos.balance.

	FOR EACH op-entry
		WHERE op-entry.filial-id EQ "0000"
		AND op-entry.op-date GT acct-pos.since
		AND op-entry.op-date LE iEndDate
		AND op-entry.op-status EQ CHR(251) 
		AND
		( 
		op-entry.acct-db BEGINS iAcct
		OR op-entry.acct-cr BEGINS iAcct
		)
		NO-LOCK
/*		, op
		WHERE op.op EQ op-entry.op
		AND op.op-status EQ CHR(251) 
		NO-LOCK */
		BREAK BY op-entry.op-date:
		IF FIRST-OF(op-entry.op-date) THEN
			ASSIGN
			vAmt = 0
			vAmtDb = 0
			vAmtCr = 0.

		vAmtDb = vAmtDb + op-entry.amt-rub.
		vAmtCr = vAmtCr + op-entry.amt-rub.

		IF LAST-OF(op-entry.op-date) THEN
		DO:
			CREATE tt-acct-pos.
			ASSIGN
			tt-acct-pos.filial-id = "0000"
			tt-acct-pos.acct = iAcct
			tt-acct-pos.currency = ""
			tt-acct-pos.since = op-entry.op-date
			tt-acct-pos.debit = vAmtDb
			tt-acct-pos.credit = vAmtCr
			tt-acct-pos.balance = vAmt + vAmtDb - vAmtCr
			.
			vAmt = tt-acct-pos.balance.
		END.
	END.

	vAmtDb = 0.
	vAmtCr = 0.

	vAmt = iAcctPos.

	FOR EACH tt-acct-pos 
	WHERE 
	tt-acct-pos.since GE (IF iBegDate LT iEndDate - 44 + 1 THEN iEndDate - 44 + 1 ELSE iBegDate)
	BY tt-acct-pos.since DESC:
/*		IF FIRST(tt-acct-pos.since) THEN
			vAmt = tt-acct-pos.balance.
*/		vAmtDb = vAmtDb + tt-acct-pos.debit.
		vAmtCr = vAmtCr + tt-acct-pos.credit.
	END.
	vRet[1] = vAmtDb - (IF vAmtDb GT vAmt THEN vAmtCr ELSE 0).

	vAmtDb = 0.
	vAmtCr = 0.
	vAmt = iAcctPos - vRet[1].
	vRet[2] = 0.
	vRet[3] = 0.

	IF iBegDate LT iEndDate - 44 THEN
	DO:
		FOR EACH tt-acct-pos 
		WHERE 
		tt-acct-pos.since GE (IF iBegDate LT iEndDate - 89 + 1 THEN iEndDate - 89 + 1 ELSE iBegDate)
		AND tt-acct-pos.since LE iEndDate - 44
		BY tt-acct-pos.since DESC:
/*			IF FIRST(tt-acct-pos.since) THEN
				vAmt = tt-acct-pos.balance.
*/			vAmtDb = vAmtDb + tt-acct-pos.debit.
			vAmtCr = vAmtCr + tt-acct-pos.credit.
		END.
		vRet[2] = vAmtDb - (IF vAmtDb GT vAmt THEN vAmtCr ELSE 0).
	END.
	vRet[3] = iAcctPos - vRet[1] - vRet[2].

END PROCEDURE.



