{globals.i}

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.
DEFINE BUFFER tt-op FOR op.
DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.
DEFINE BUFFER datablockel FOR datablock.
DEFINE BUFFER datalineel FOR dataline.

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateRecord AS DATE NO-UNDO. /* Дата которую пишем*/
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE isCash AS LOGICAL NO-UNDO.
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.

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
	FIND FIRST datablockel
	 	WHERE datablockel.branch-id EQ ENTRY(mI, mFilialList)
	 	AND datablockel.beg-date EQ end-date
	 	AND datablockel.end-date EQ end-date
	 	AND datablockel.dataclass-id EQ "safetyp"
	 	NO-LOCK NO-ERROR.
	 IF AVAILABLE(datablockel) THEN
	 DO:
		FOR EACH datalineel
			WHERE datalineel.data-id EQ datablockel.data-id
/*			AND datalineel.val[1] EQ DECIMAL(RECID(op-entry)) */
			AND datalineel.val[4] EQ 2.0
		NO-LOCK:
			FIND FIRST op-entry
				WHERE RECID(op-entry) EQ INT64(datalineel.val[1])
			NO-LOCK NO-ERROR.
			IF AVAILABLE(op-entry) THEN
			DO:
				FIND FIRST op
					WHERE op.op EQ op-entry.op
				NO-LOCK NO-ERROR.
				IF AVAILABLE(op) THEN
				DO:
					FIND FIRST tmprecid WHERE tmprecid.id EQ RECID(op) NO-ERROR.
					IF NOT AVAILABLE(tmprecid) THEN
					DO:
						CREATE tmprecid.
						ASSIGN tmprecid.id = RECID(op).
						RUN op-printel.p (RECID(op)) . 
					END.
				END. /* IF AVAILABLE(op) */
			END. /* AVAILABLE(op-entry) */
		END. /* FOR EACH datalineel */
	END. /* AVAILABLE(datablockel) */
END. /* DO mI */


