{globals.i}
DEFINE INPUT  PARAMETER iParam AS CHARACTER NO-UNDO. 

{ttretval.def}
DEFINE BUFFER op FOR op.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER op-entry FOR op-entry.

DEFINE VARIABLE mFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpDate1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpDate2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctDb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpKind AS CHARACTER NO-UNDO.
/*
MESSAGE NUM-ENTRIES(iParam) SKIP iParam
VIEW-AS ALERT-BOX.
*/
IF NUM-ENTRIES(iParam) GT 6 THEN
DO:
	MESSAGE "Неверное число параметров передано в процедуру." 
	VIEW-AS ALERT-BOX.
	pick-value = "0".
END.
ELSE
DO:
	ASSIGN
		mFilial = shFilial
		shFilial = ENTRY(1,iParam)
		mOpDate1 = ENTRY(2,iParam)
		mOpDate2 = ENTRY(3,iParam)
		mAcctDb = ENTRY(4,iParam)
		mAcctCr = ENTRY(5,iParam)
		mOpKind = ENTRY(6,iParam)
		.

	/*
	{tmpobj.def}

	FOR EACH op-entry
	    WHERE op-entry.filial-id EQ mFilial
	    AND op-entry.op-date EQ mOpDate
	    NO-LOCK,
	    FIRST op
	    WHERE op.op EQ op-entry.op
	    AND op.op-date EQ mOpDate
	    AND op.op-status GE CHR(251)
	    NO-LOCK:
	         CREATE TmpObj.
	         TmpObj.rid = RECID(op-entry).
	END.

	mTmpObjHand = TEMP-TABLE TmpObj:HANDLE.
	*/
	pick-value = "".
   {empty ttRetVal}
	DO TRANSACTION:
	/*
	RUN browseld.p ("op",
	"UseTmpObjInQuery" + CHR(1) + "title",
	STRING(mTmpObjHand) + CHR(1) + "Документы за ",
	?, /* Поля для блокировки. */
	3). /* Строка отображения фрейма. */
	*/
		RUN browseld.p ('op',
			"op-date1" + CHR(1) + "op-date2" + CHR(1) + "acct-db" + CHR(1) + "acct-cr" + CHR(1) + "op-kind" + CHR(1) + "retRcp" + CHR(1) + "retFld" + CHR(1) + "retType",
			 mOpDate1 + CHR(1) + mOpDate2 + CHR(1) + mAcctDb + CHR(1) + mAcctCr + CHR(1) + mOpKind + CHR(1) + 
                   STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1)
                 + "op"     + CHR(1)  
                 + "multi"
			 ,
			"",
			3).
	END.

   IF (LASTKEY = 13 OR LASTKEY = 10) THEN DO:
      FOR EACH ttRetVal:
         {additem.i pick-value ttRetVal.pickvalue}
      END.
   END.
/*
	IF NOT {assigned pick-value} THEN
		pick-value = "0".
	ELSE
		FOR EACH tmprecid BREAK BY tmprecid.id:
			IF FIRST(tmprecid.id) THEN
				pick-value = TRIM(STRING(tmprecid.id)).
			ELSE
				pick-value = pick-value + "," + TRIM(STRING(tmprecid.id)).
		END.
*/		
END.
	shFilial = mFilial.

/* RETURN pick-value. */

