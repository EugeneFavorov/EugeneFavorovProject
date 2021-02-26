{globals.i}

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.
DEFINE BUFFER tt-op FOR op.
DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.
DEFINE BUFFER op-entry FOR op-entry.

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateRecord AS DATE NO-UNDO. /* Дата которую пишем*/
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.

{tmprecid.def}

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
	 FOR EACH datablock
	 	WHERE datablock.branch-id EQ ENTRY(mI, mFilialList)
	 	AND CAN-DO("*cur", datablock.dataclass-id)
	 	AND datablock.beg-date EQ mDateRecord
	 	AND datablock.end-date EQ mDateRecord
	 	AND datablock.isfinal EQ YES
	 	NO-LOCK, 
	 	EACH dataline
	 		WHERE dataline.data-id EQ datablock.data-id
	 		AND NOT (dataline.val[2] = 0.0 AND dataline.val[4] = 0.0)
	 		NO-LOCK BREAK BY DataLine.Sym1:
/*	 		FIND FIRST op-entry
	 			WHERE op-entry.filial-id EQ ENTRY(mI, mFilialList)
	 			AND op-entry.op-date EQ end-date
	 			AND op-entry.op-status GE CHR(251)
	 			AND op-entry.acct-db BEGINS DataLine.Sym1
	 		NO-LOCK NO-ERROR.
	 		IF AVAILABLE(op-entry) THEN
	 		DO: */
	 		IF FIRST-OF(DataLine.Sym1) THEN
	 		DO:
				OUTPUT TO VALUE( mDir + mDirElec + "//extrall-" + ENTRY(mI, mNmFil) + "-" + DataLine.Sym1 + "-" + STRING(mDateRecord, "99.99.9999") + ".txt").
				FOR EACH acct
					WHERE acct.filial-id EQ ENTRY(mI, mFilialList)
					AND acct.bal-acct EQ INT64(DataLine.Sym1)
/*					AND acct.currency EQ DataLine.Sym2 */
					AND acct.open-date LE mDateRecord
					AND (acct.close-date EQ ? OR acct.close-date GE mDateRecord )
				    BY acct.currency + SUBSTRING(acct.acct, 14, 7):
						RUN vyp_all.p(RECID(acct)). 
				END.
				OUTPUT CLOSE.
			END.
/*			END. */
	END.
END.
{chmoddir.i}
