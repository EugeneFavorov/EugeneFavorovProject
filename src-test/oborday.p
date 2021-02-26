{globals.i}
{sh-defs.i}
{intrface.get xclass}


DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.
DEFINE BUFFER acct-pos FOR acct-pos.
DEFINE BUFFER branch FOR branch.
DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.


DEFINE BUFFER code FOR code.
DEFINE BUFFER code1 FOR code.
DEFINE BUFFER tmp-code FOR tmp-code.
DEFINE BUFFER signs FOR signs.
DEFINE BUFFER signs1 FOR signs.


DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateRecord AS DATE NO-UNDO. /* Дата которую пишем*/
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE II AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtSide AS DECIMAL EXTENT 6 NO-UNDO.
DEFINE VARIABLE mAmtfilial AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE mAmtBal1 AS DECIMAL EXTENT 6 NO-UNDO.
DEFINE VARIABLE mAmtRazdel1 AS DECIMAL EXTENT 6 NO-UNDO.
DEFINE VARIABLE mAmtRazdel2 AS DECIMAL EXTENT 6 NO-UNDO.
DEFINE VARIABLE mAcctDetails AS CHARACTER EXTENT 3 NO-UNDO.


DEFINE TEMP-TABLE tt-obor
	FIELD num AS INT64
	FIELD filial-id AS CHARACTER
	FIELD filial-pref AS CHARACTER
	FIELD side AS CHARACTER
	FIELD sidename AS CHARACTER
	FIELD order AS INT64
	FIELD razdel1 AS CHARACTER
	FIELD razdname1 AS CHARACTER
	FIELD razdel2 AS CHARACTER
	FIELD razdname2 AS CHARACTER
	FIELD bal-acct1 AS CHARACTER
	FIELD bal-acct AS CHARACTER
	FIELD name AS CHARACTER
	FIELD AmtPosIn AS DECIMAL
	FIELD amt-rub-db AS DECIMAL
	FIELD amt-cur-db AS DECIMAL
	FIELD amt-rub-cr AS DECIMAL
	FIELD amt-cur-cr AS DECIMAL
	FIELD AmtPosOut AS DECIMAL
	INDEX pp filial-id side bal-acct1 bal-acct.


/* Список филиалов, по которым готовим выписки */
ASSIGN
mFilialList = "0000,0300"
mNmFil = "gb,tf"
.
{getdate.i}

{sectacct.i}

{setdirel.i}

mDateRecord = end-date.


OUTPUT TO VALUE("saldo.log").
/* Цикл формирования выписок */
DO mI = 1 TO NUM-ENTRIES(mFilialList):
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
	 		NO-LOCK BREAK BY DataLine.Sym1:
	 		IF FIRST-OF(DataLine.Sym1) THEN
	 		DO:
		 		FIND FIRST bal-acct
		 			WHERE bal-acct.bal-acct EQ INT64(DataLine.Sym1)
		 		NO-LOCK NO-ERROR.
		 		FIND FIRST ttSectAcct
		 			WHERE ttSectAcct.acct-cat EQ bal-acct.acct-cat
		 			AND ttSectAcct.bal-acct1 EQ SUBSTRING(STRING(bal-acct.bal-acct), 1, 3)
		 		NO-LOCK NO-ERROR.
		 		IF NOT AVAILABLE(ttSectAcct) THEN
		 			MESSAGE bal-acct.acct-cat SKIP bal-acct.bal-acct1 VIEW-AS ALERT-BOX.
			    CREATE tt-obor.
			    ASSIGN
			    	tt-obor.filial-id = branch.branch-id
			    	tt-obor.filial-pref = ENTRY(mI, mNmFil)
			    	tt-obor.side = DataLine.Sym4
			    	tt-obor.sidename = IF DataLine.Sym4 BEGINS "А" THEN "Актив" ELSE "Пассив"
			    	tt-obor.razdel1 = ttSectAcct.razdel1
			    	tt-obor.razdname1 = ttSectAcct.name1
			    	tt-obor.razdel2 = ttSectAcct.razdel2
			    	tt-obor.razdname2 = ttSectAcct.name2
			    	tt-obor.bal-acct1 = TRIM(STRING(bal-acct.bal-acct1))
			    	tt-obor.bal-acct = TRIM(STRING(bal-acct.bal-acct))
			    	tt-obor.name = bal-acct.name-bal-acc
			    	.
			    IF tt-obor.bal-acct1 NE SUBSTRING(tt-obor.bal-acct, 1, 3) THEN
			    	tt-obor.bal-acct1 = SUBSTRING(tt-obor.bal-acct, 1, 3).
			END.
			ASSIGN
				tt-obor.AmtPosOut = tt-obor.AmtPosOut + IF DataLine.Sym4 BEGINS "А" THEN DataLine.Val[6] ELSE DataLine.Val[8].
			IF TRIM(dataline.Sym2) EQ  "" THEN
				ASSIGN
		    	tt-obor.amt-rub-db = tt-obor.amt-rub-db + DataLine.Val[2]
		    	tt-obor.amt-rub-cr = tt-obor.amt-rub-cr + DataLine.Val[4]
		    	.
		    ELSE
				ASSIGN
		    	tt-obor.amt-cur-db = tt-obor.amt-cur-db + DataLine.Val[2]
		    	tt-obor.amt-cur-cr = tt-obor.amt-cur-cr + DataLine.Val[4]
		    	.
	 		IF LAST-OF(DataLine.Sym1) THEN
	 		DO: 
	 			tt-obor.AmtPosIn = (IF DataLine.Sym4 BEGINS "А" 
	 								THEN 
	 								tt-obor.AmtPosOut - tt-obor.amt-rub-db - tt-obor.amt-cur-db + tt-obor.amt-rub-cr + tt-obor.amt-cur-cr
	 								ELSE 
	 								tt-obor.AmtPosOut + tt-obor.amt-rub-db + tt-obor.amt-cur-db - tt-obor.amt-rub-cr - tt-obor.amt-cur-cr
	 								).
	 		END. 
	END.
END.

OUTPUT CLOSE.

ASSIGN 
	mAmtRazdel1[1] = 0
	mAmtRazdel1[2] = 0
	mAmtRazdel1[3] = 0
	mAmtRazdel1[4] = 0
	mAmtRazdel1[5] = 0
	mAmtRazdel1[6] = 0
	mAmtRazdel2[1] = 0
	mAmtRazdel2[2] = 0
	mAmtRazdel2[3] = 0
	mAmtRazdel2[4] = 0
	mAmtRazdel2[5] = 0
	mAmtRazdel2[6] = 0
	. 

FOR EACH tt-obor
/*	WHERE tt-obor.bal-acct1 EQ ttSectAcct.bal-acct1 */
	BREAK 
	BY tt-obor.filial-id
	BY tt-obor.sidename
	BY tt-obor.bal-acct1
	/*	BY tt-obor.bal-acct  */
/*		BY tt-obor.razdel1 */
/*		BY tt-obor.razdel2  */
		:
/*	FOR EACH ttSectAcct
		BREAK BY ttSectAcct.bal-acct1
		BY ttSectAcct.razdel1
		BY ttSectAcct.razdel2:
*/
		IF FIRST-OF(tt-obor.filial-id) THEN
		DO:
			OUTPUT TO VALUE( mDir + mDirElec + "//oborot-" + tt-obor.filial-pref + "-" + STRING(mDateRecord, "99.99.9999") + ".txt") CONVERT TARGET "1251".
			FIND FIRST branch
				WHERE branch.branch-id EQ tt-obor.filial-id
			NO-LOCK NO-ERROR.
			{oborhed.i} 
			ASSIGN 
			mAmtfilial[1] = 0
			mAmtfilial[2] = 0
			mAmtfilial[3] = 0
			mAmtfilial[4] = 0
			mRazdel[1] = tt-obor.razdel1
/*			mRazdel[2] = tt-obor.razdel2 */
			mRazdelName[1] = tt-obor.razdname1
/*			mRazdelName[2] = tt-obor.razdname2 */
			. 
			ASSIGN 
				mAmtRazdel1[1] = 0
				mAmtRazdel1[2] = 0
				mAmtRazdel1[3] = 0
				mAmtRazdel1[4] = 0
				mAmtRazdel1[5] = 0
				mAmtRazdel1[6] = 0
			.
		END.
		IF FIRST-OF(tt-obor.bal-acct1) THEN
			ASSIGN 
			mAmtBal1[1] = 0
			mAmtBal1[2] = 0
			mAmtBal1[3] = 0
			mAmtBal1[4] = 0
			mAmtBal1[5] = 0
			mAmtBal1[6] = 0
			.
/*		IF tt-obor.razdel1 NE mRazdel[1] THEN
			ASSIGN 
			mAmtRazdel1[1] = 0
			mAmtRazdel1[2] = 0
			mAmtRazdel1[3] = 0
			mAmtRazdel1[4] = 0
			mAmtRazdel1[5] = 0
			mAmtRazdel1[6] = 0
			.
		IF FIRST-OF(tt-obor.razdel2) AND tt-obor.razdel2 NE " " THEN
			ASSIGN 
			mAmtRazdel2[1] = 0
			mAmtRazdel2[2] = 0
			mAmtRazdel2[3] = 0
			mAmtRazdel2[4] = 0
			mAmtRazdel2[5] = 0
			mAmtRazdel2[6] = 0
			. */
		IF FIRST-OF(tt-obor.sidename) THEN
		DO:
			PUT UNFORMATTED
			FILL(" ", 40)
			tt-obor.sidename 
			SKIP.

			ASSIGN 
			mAmtSide[1] = 0
			mAmtSide[2] = 0
			mAmtSide[3] = 0
			mAmtSide[4] = 0
			mAmtSide[5] = 0
			mAmtSide[6] = 0
			.
		END.
/* MESSAGE 
 tt-obor.razdel1 SKIP mRazdel[1] SKIP
 tt-obor.razdel2 SKIP mRazdel[2] SKIP
 tt-obor.bal-acct1
 VIEW-AS ALERT-BOX. */
/*		IF tt-obor.razdel2 NE mRazdel[2] AND TRIM(mRazdel[2]) NE "" THEN
		DO:
			PUT UNFORMATTED
	    	"ИТОГО" FORMAT "X(7)"
	    	" "
	    	"по" FORMAT "X(2)"
	    	" "
	    	mRazdelName[2] FORMAT "X(35)"
	    	"|"
			STRING(mAmtRazdel2[5], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[2] + mAmtRazdel2[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[3] + mAmtRazdel2[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel2[6], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			SKIP
			.
	    	II = 36.
	    	DO WHILE II LT LENGTH(TRIM(mRazdelName[2])):
				PUT UNFORMATTED
		    	"     " FORMAT "X(7)"
		    	" "
		    	"  " FORMAT "X(2)"
		    	" "
		    	SUBSTRING(TRIM(mRazdelName[2]), II) FORMAT "X(35)"
				"|                   |                   |                   |                   |                   |                   |                   |                   |" 
				SKIP.
				II = II + 35.
			END.
			ASSIGN 
				mAmtRazdel2[1] = 0
				mAmtRazdel2[2] = 0
				mAmtRazdel2[3] = 0
				mAmtRazdel2[4] = 0
				mAmtRazdel2[5] = 0
				mAmtRazdel2[6] = 0
				mRazdel[2] = tt-obor.razdel2
				mRazdelName[2] = tt-obor.razdname2
				. 
		END.
*/
		IF tt-obor.razdel1 NE mRazdel[1] AND TRIM(mRazdel[1]) NE "" THEN
		DO:
			PUT UNFORMATTED
	    	"ИТОГО" FORMAT "X(7)"
	    	" "
	    	"по" FORMAT "X(2)"
	    	" "
	    	mRazdelName[1] FORMAT "X(35)"
	    	"|"
			STRING(mAmtRazdel1[5], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[2] + mAmtRazdel1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[3] + mAmtRazdel1[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[6], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			SKIP
			.
	    	II = 36.
	    	DO WHILE II LT LENGTH(TRIM(mRazdelName[1])):
				PUT UNFORMATTED
		    	"     " FORMAT "X(7)"
		    	" "
		    	"  " FORMAT "X(2)"
		    	" "
		    	SUBSTRING(TRIM(mRazdelName[1]), II) FORMAT "X(35)"
				"|                   |                   |                   |                   |                   |                   |                   |                   |" 
				SKIP.
				II = II + 35.
			END.
			ASSIGN 
				mAmtRazdel1[1] = 0
				mAmtRazdel1[2] = 0
				mAmtRazdel1[3] = 0
				mAmtRazdel1[4] = 0
				mAmtRazdel1[5] = 0
				mAmtRazdel1[6] = 0
				mRazdel[1] = tt-obor.razdel1
				mRazdelName[1] = tt-obor.razdname1
				mRazdel[2] = tt-obor.razdel2
				mRazdelName[2] = tt-obor.razdname2
				. 
		END.
		PUT UNFORMATTED
	    	tt-obor.bal-acct1 FORMAT "X(7)"
	    	"|"
	    	tt-obor.bal-acct FORMAT "X(7)"
	    	"|"
	    	tt-obor.name FORMAT "X(30)"
	    	"|"
	    	/* 4 */
	    	STRING(tt-obor.AmtPosIn, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 5 */
	    	STRING(tt-obor.amt-rub-db, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 6 */
	    	STRING(tt-obor.amt-cur-db, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 7 */
	    	STRING(tt-obor.amt-rub-db + tt-obor.amt-cur-db, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 8 */
	    	STRING(tt-obor.amt-rub-cr, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 9 */
	    	STRING(tt-obor.amt-cur-cr, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 10 */
	    	STRING(tt-obor.amt-rub-cr + tt-obor.amt-cur-cr, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	/* 11 */
	    	STRING(tt-obor.AmtPosOut, ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	SKIP.
	    	II = 31.
	    	DO WHILE II LT LENGTH(TRIM(tt-obor.name)):
				PUT UNFORMATTED
				"       |       |"
		    	SUBSTRING(TRIM(tt-obor.name), II) FORMAT "X(30)"
				"|                   |                   |                   |                   |                   |                   |                   |                   |" 
				SKIP.
				II = II + 30.
			END.

		ASSIGN
			mAmtSide[1] = mAmtSide[1] + tt-obor.amt-rub-db
			mAmtSide[2] = mAmtSide[2] + tt-obor.amt-cur-db
			mAmtSide[3] = mAmtSide[3] + tt-obor.amt-rub-cr
			mAmtSide[4] = mAmtSide[4] + tt-obor.amt-cur-cr
			mAmtSide[5] = mAmtSide[5] + tt-obor.AmtPosIn
			mAmtSide[6] = mAmtSide[6] + tt-obor.AmtPosOut

			mAmtRazdel1[1] = mAmtRazdel1[1] + tt-obor.amt-rub-db
			mAmtRazdel1[2] = mAmtRazdel1[2] + tt-obor.amt-cur-db
			mAmtRazdel1[3] = mAmtRazdel1[3] + tt-obor.amt-rub-cr
			mAmtRazdel1[4] = mAmtRazdel1[4] + tt-obor.amt-cur-cr
			mAmtRazdel1[5] = mAmtRazdel1[5] + tt-obor.AmtPosIn
			mAmtRazdel1[6] = mAmtRazdel1[6] + tt-obor.AmtPosOut

			mAmtBal1[1] = mAmtBal1[1] + tt-obor.amt-rub-db
			mAmtBal1[2] = mAmtBal1[2] + tt-obor.amt-cur-db
			mAmtBal1[3] = mAmtBal1[3] + tt-obor.amt-rub-cr
			mAmtBal1[4] = mAmtBal1[4] + tt-obor.amt-cur-cr
			mAmtBal1[5] = mAmtBal1[5] + tt-obor.AmtPosIn
			mAmtBal1[6] = mAmtBal1[6] + tt-obor.AmtPosOut
			.
/*
			IF tt-obor.Razdel2 NE " " THEN
			ASSIGN
				mAmtRazdel2[1] = mAmtRazdel2[1] + tt-obor.amt-rub-db
				mAmtRazdel2[2] = mAmtRazdel2[2] + tt-obor.amt-cur-db
				mAmtRazdel2[3] = mAmtRazdel2[3] + tt-obor.amt-rub-cr
				mAmtRazdel2[4] = mAmtRazdel2[4] + tt-obor.amt-cur-cr
				mAmtRazdel2[5] = mAmtRazdel2[5] + tt-obor.AmtPosIn
				mAmtRazdel2[6] = mAmtRazdel2[6] + tt-obor.AmtPosOut
			. 
*/
	/*		mAmtBal2[1] = mAmtBal2[1] + tt-obor.amt-rub-db
			mAmtBal2[2] = mAmtBal2[2] + tt-obor.amt-cur-db
			mAmtBal2[3] = mAmtBal2[4] + tt-obor.amt-rub-cr
			mAmtBal2[4] = mAmtBal2[4] + tt-obor.amt-cur-cr
	*/		
		IF LAST-OF(tt-obor.bal-acct1) THEN
			PUT UNFORMATTED
	    	"ИТОГО" FORMAT "X(7)"
	    	" "
	    	"по" FORMAT "X(2)"
	    	" "
	    	tt-obor.bal-acct1 FORMAT "X(4)"
	    	" "
	    	FILL(" ", 30)
	    	"|"
			STRING(mAmtBal1[5], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[2] + mAmtBal1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[3] + mAmtBal1[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtBal1[6], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			SKIP
			.

		
		IF LAST-OF(tt-obor.sidename) THEN
		DO:
/**/
			PUT UNFORMATTED
	    	"ИТОГО" FORMAT "X(7)"
	    	" "
	    	"по" FORMAT "X(2)"
	    	" "
	    	mRazdelName[1] FORMAT "X(35)"
	    	"|"
			STRING(mAmtRazdel1[5], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[2] + mAmtRazdel1[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[3] + mAmtRazdel1[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtRazdel1[6], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			SKIP
			.
	    	II = 36.
	    	DO WHILE II LT LENGTH(TRIM(mRazdelName[1])):
				PUT UNFORMATTED
		    	"     " FORMAT "X(7)"
		    	" "
		    	"  " FORMAT "X(2)"
		    	" "
		    	SUBSTRING(TRIM(mRazdelName[1]), II) FORMAT "X(35)"
				"|                   |                   |                   |                   |                   |                   |                   |                   |" 
				SKIP.
				II = II + 35.
			END.
			ASSIGN 
				mAmtRazdel1[1] = 0
				mAmtRazdel1[2] = 0
				mAmtRazdel1[3] = 0
				mAmtRazdel1[4] = 0
				mAmtRazdel1[5] = 0
				mAmtRazdel1[6] = 0
				mRazdel[1] = tt-obor.razdel1
				mRazdelName[1] = tt-obor.razdname1
				mRazdel[2] = tt-obor.razdel2
				mRazdelName[2] = tt-obor.razdname2
				. 

/**/
			PUT UNFORMATTED
	    	"ИТОГО" FORMAT "X(7)"
	    	" "
	    	"по" FORMAT "X(2)"
	    	" "
	    	(TRIM(tt-obor.sidename) + "у") FORMAT "X(7)"
	    	" "
	    	FILL(" ", 27)
	    	"|"
			STRING(mAmtSide[5], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[1] + mAmtSide[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[3] + mAmtSide[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtSide[6], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			SKIP
			.
			ASSIGN
				mAmtfilial[1] = mAmtfilial[1] + mAmtSide[1]
				mAmtfilial[2] = mAmtfilial[2] + mAmtSide[2]
				mAmtfilial[3] = mAmtfilial[3] + mAmtSide[3]
				mAmtfilial[4] = mAmtfilial[4] + mAmtSide[4]
				.
		END.

		IF LAST-OF(tt-obor.filial-id) THEN
		DO:
			PUT UNFORMATTED
	    	"ВСЕГО ОБОРОТОВ" 
	    	" "
	    	FILL(" ", 31)
	    	"|"
	    	FILL(" ", 19)
	    	"|"
			STRING(mAmtfilial[1], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtfilial[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtfilial[1] + mAmtfilial[2], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtfilial[3], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtfilial[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
			STRING(mAmtfilial[3] + mAmtfilial[4], ">>>>>>>>>>>>>>>9.99")
	    	"|"
	    	FILL(" ", 19)
	    	"|"
	    	SKIP
	"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- " SKIP
			SKIP(2)
			"    Руководитель                                          Главный бухгалтер"
			SKIP
			.
			OUTPUT CLOSE.
		END.
/*	END. */
END.
{chmoddir.i}

{intrface.del}
