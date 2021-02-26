{globals.i}
{sh-defs.i}
{intrface.get xclass}


DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.
DEFINE BUFFER acct-pos FOR acct-pos.
DEFINE BUFFER branch FOR branch.
DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateRecord AS DATE NO-UNDO. /* Дата которую пишем*/
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mI1 AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtCust AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE mAmtSide AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE mAmtBal2 AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE mAmtBal1 AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE mAcctDetails AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mOstType AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-saldo
	FIELD filial-id AS CHARACTER
	FIELD side AS CHARACTER
	FIELD sidename AS CHARACTER
	FIELD bal-acct1 AS CHARACTER
	FIELD bal-acct AS CHARACTER
	FIELD cust-id AS INT64
	FIELD name-cli AS CHARACTER
	FIELD acct AS CHARACTER
	FIELD name AS CHARACTER
	FIELD since AS DATE
	FIELD currency AS CHARACTER
	FIELD amt-rub AS DECIMAL
	FIELD amt-cur AS DECIMAL
	INDEX pp filial-id bal-acct1 bal-acct cust-id.


/* Список филиалов, по которым готовим выписки */
ASSIGN
mFilialList = "0000,0300"
mNmFil = "gb,tf"
.
{getdate.i}
{setdirel.i}

mDateRecord = end-date.


/* Цикл формирования выписок */
DO mI = 1 TO NUM-ENTRIES(mFilialList):
OUTPUT TO VALUE("saldo_" + ENTRY(mI, mNmFil) + ".log").
	FIND FIRST branch
		WHERE branch.branch-id EQ ENTRY(mI, mFilialList)
	NO-LOCK NO-ERROR.
	 FOR EACH datablock
	 	WHERE datablock.branch-id EQ ENTRY(mI, mFilialList)
	 	AND CAN-DO("*cur", datablock.dataclass-id)
	 	AND datablock.beg-date EQ mDateRecord
	 	AND datablock.end-date EQ mDateRecord
	 	AND datablock.isfinal EQ YES
	 	NO-LOCK, 
	 	EACH dataline
	 		WHERE dataline.data-id EQ datablock.data-id
/*	 		AND DataLine.Sym1 BEGINS "980"  */
	 		NO-LOCK BY DataLine.Sym1:

	 		PUT UNFORMATTED 
	 		ENTRY(mI, mFilialList)
	 		" "
	 		DataLine.Sym1
	 		SKIP.

	 		FIND FIRST bal-acct
	 			WHERE bal-acct.bal-acct EQ INT64(DataLine.Sym1)
	 		NO-LOCK NO-ERROR.
				FOR EACH acct
					WHERE acct.filial-id EQ ENTRY(mI, mFilialList)
					AND acct.bal-acct EQ INT64(DataLine.Sym1)
					AND acct.currency EQ DataLine.Sym2 
					AND acct.open-date LE mDateRecord
					AND (acct.close-date EQ ? OR acct.close-date GE mDateRecord )
				    BY acct.cust-id:

				    mOstType = "POS".

				/* проверяем остаток на дату расчета */
					RUN acct-pos IN h_base (
						acct.acct,
				        acct.currency,
				        mDateRecord,
				        mDateRecord, 
				        "√"
				        ).
					IF sh-bal EQ 0 AND acct.acct-cat EQ "d" THEN
					DO:
						RUN acct-qty in h_base (acct.acct,
							acct.currency,
					        mDateRecord,
					        mDateRecord, 
							 "√"). 
						sh-bal = sh-qty.
					    mOstType = "QTY".
					END.

					PUT UNFORMATTED
					acct.acct
					"|"
					"sh-bal - " sh-bal
					"|"
					"sh-qty - " sh-qty
					SKIP.

					IF sh-bal NE 0 THEN
					DO:
						IF acct.cust-cat EQ "В" THEN
							mAcctDetails[1] = branch.name.
						ELSE
						DO:
				            RUN GetCustName IN h_base (acct.cust-cat,
				                                       acct.cust-id,
				                                       ?,
				                                       OUTPUT mAcctDetails[1],
				                                       OUTPUT mAcctDetails[2],
				                                       INPUT-OUTPUT mAcctDetails[3]).
				            mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
			            END.
			            IF mOstType EQ "POS" THEN
						    FOR EACH acct-pos
						    	WHERE acct-pos.filial-id EQ branch.branch-id
						    	AND acct-pos.acct EQ acct.acct
						    	AND acct-pos.currency EQ acct.currency
						    	AND acct-pos.since LE mDateRecord
						    	BY acct-pos.since DESC:
						    	LEAVE.
						    END.
						ELSE
						    FOR EACH acct-qty
						    	WHERE acct-qty.filial-id EQ branch.branch-id
						    	AND acct-qty.acct EQ acct.acct
						    	AND acct-qty.currency EQ acct.currency
						    	AND acct-qty.since LE mDateRecord
						    	BY acct-qty.since DESC:
						    	LEAVE.
						    END.

					    CREATE tt-saldo.
					    ASSIGN
					    	tt-saldo.filial-id = branch.branch-id
					    	tt-saldo.side = DataLine.Sym4
					    	tt-saldo.sidename = IF DataLine.Sym4 BEGINS "А" THEN "Активу" ELSE "Пассиву"
					    	tt-saldo.bal-acct1 = TRIM(STRING(bal-acct.bal-acct1))
					    	tt-saldo.bal-acct = TRIM(STRING(bal-acct.bal-acct))
					    	tt-saldo.cust-id = acct.cust-id
					    	tt-saldo.name-cli = IF TRIM(mAcctDetails[1]) NE "" THEN TRIM(mAcctDetails[1]) ELSE acct.details
					    	tt-saldo.acct = acct.acct
					    	tt-saldo.name = IF acct.details EQ ? OR TRIM(acct.details) EQ "" THEN mAcctDetails[1] ELSE acct.details
					    	tt-saldo.since = IF mOstType EQ "POS" THEN (IF AVAILABLE(acct-pos) THEN acct-pos.since ELSE mDateRecord) ELSE (IF AVAILABLE(acct-qty) THEN acct-qty.since ELSE mDateRecord)
					    	tt-saldo.currency = acct.currency
					    	tt-saldo.amt-rub = ABSOLUTE(sh-bal)
					    	tt-saldo.amt-cur = ABSOLUTE(sh-val)
					    	.
				    END.
				END.
	END.
OUTPUT CLOSE.
END.


FOR EACH tt-saldo
	BREAK BY tt-saldo.filial-id
	BY tt-saldo.sidename
	BY tt-saldo.bal-acct1
	BY tt-saldo.bal-acct
	BY tt-saldo.cust-id:
	IF FIRST-OF(tt-saldo.filial-id) THEN
	DO:
		DO mI1 = 1 TO NUM-ENTRIES(mFilialList):
			IF ENTRY(mI1, mFilialList) EQ tt-saldo.filial-id THEN
				mI = mI1.
		END.
		OUTPUT TO VALUE( mDir + mDirElec + "//saldo-" + ENTRY(mI, mNmFil) + "-" + STRING(mDateRecord, "99.99.9999") + ".txt").
		FIND FIRST branch
			WHERE branch.branch-id EQ tt-saldo.filial-id
		NO-LOCK NO-ERROR.
		{saldohed.i}
	END.
	IF FIRST-OF(tt-saldo.cust-id) THEN
		ASSIGN 
		mAmtCust[1] = 0
		mAmtCust[2] = 0
		.
	IF FIRST-OF(tt-saldo.bal-acct) THEN
		ASSIGN 
		mAmtBal2[1] = 0
		mAmtBal2[2] = 0
		.
	IF FIRST-OF(tt-saldo.bal-acct1) THEN
		ASSIGN 
		mAmtBal1[1] = 0
		mAmtBal1[2] = 0
		.
	IF FIRST-OF(tt-saldo.sidename) THEN
	DO:
		ASSIGN 
		mAmtSide[1] = 0
		mAmtSide[2] = 0
		.
		PUT UNFORMATTED
    	FILL(" ", 63)
    	SUBSTRING(tt-saldo.sidename, 1, LENGTH(TRIM(tt-saldo.sidename)) - 1)
    	SKIP.
	END.
	PUT UNFORMATTED
    	tt-saldo.bal-acct1 FORMAT "X(5)"
    	" "
    	tt-saldo.bal-acct FORMAT "X(7)"
    	" "
    	tt-saldo.acct FORMAT "X(20)"
    	" "
    	tt-saldo.name FORMAT "X(42)"
    	" "
    	STRING(tt-saldo.since, "99.99.9999")
    	"  "
    	tt-saldo.currency FORMAT "X(3)"
    	"  "
    	STRING(tt-saldo.amt-rub, ">>>>>>>>>>>>>>>9.99")
    	" "
    	STRING(tt-saldo.amt-cur, ">>>>>>>>>>>>>>>9.99")
    	SKIP.
	ASSIGN
		mAmtCust[1] = mAmtCust[1] + tt-saldo.amt-rub
		mAmtCust[2] = mAmtCust[2] + tt-saldo.amt-cur
		mAmtBal2[1] = mAmtBal2[1] + tt-saldo.amt-rub
		mAmtBal2[2] = mAmtBal2[2] + tt-saldo.amt-cur
		mAmtBal1[1] = mAmtBal1[1] + tt-saldo.amt-rub
		mAmtBal1[2] = mAmtBal1[2] + tt-saldo.amt-cur
		mAmtSide[1] = mAmtSide[1] + tt-saldo.amt-rub
		mAmtSide[2] = mAmtSide[2] + tt-saldo.amt-cur
		.
	IF LAST-OF(tt-saldo.cust-id) THEN
		PUT UNFORMATTED
    	"ИТОГО" FORMAT "X(5)"
    	" "
    	"по" FORMAT "X(2)"
    	" "
    	tt-saldo.name-cli FORMAT "X(85)"
    	" "
    	STRING(mAmtCust[1], ">>>>>>>>>>>>>>>9.99")
    	" "
    	STRING(mAmtCust[2], ">>>>>>>>>>>>>>>9.99")
    	SKIP.

	IF LAST-OF(tt-saldo.bal-acct) THEN
		PUT UNFORMATTED
    	"ИТОГО" FORMAT "X(5)"
    	" "
    	"по" FORMAT "X(2)"
    	" "
    	tt-saldo.bal-acct FORMAT "X(85)"
    	" "
    	STRING(mAmtBal2[1], ">>>>>>>>>>>>>>>9.99")
    	" "
    	STRING(mAmtBal2[2], ">>>>>>>>>>>>>>>9.99")
    	SKIP.

	IF LAST-OF(tt-saldo.bal-acct1) THEN
		PUT UNFORMATTED
    	"ИТОГО" FORMAT "X(5)"
    	" "
    	"по" FORMAT "X(2)"
    	" "
    	tt-saldo.bal-acct1 FORMAT "X(85)"
    	" "
    	STRING(mAmtBal1[1], ">>>>>>>>>>>>>>>9.99")
    	" "
    	STRING(mAmtBal1[2], ">>>>>>>>>>>>>>>9.99")
    	SKIP.

	IF LAST-OF(tt-saldo.sidename) THEN
		PUT UNFORMATTED
    	"ИТОГО" FORMAT "X(5)"
    	" "
    	"по" FORMAT "X(2)"
    	" "
    	tt-saldo.sidename FORMAT "X(85)"
    	" "
    	STRING(mAmtSide[1], ">>>>>>>>>>>>>>>9.99")
    	" "
    	STRING(mAmtSide[2], ">>>>>>>>>>>>>>>9.99")
    	SKIP.

	IF LAST-OF(tt-saldo.filial-id) THEN
		OUTPUT CLOSE.
END.
{chmoddir.i}
{intrface.del}
