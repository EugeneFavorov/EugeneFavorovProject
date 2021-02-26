{globals.i}
{intrface.get prnvd}

&SCOPED-DEFINE SAFETYPE YES

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.
DEFINE BUFFER tt-op FOR op.
DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.
DEFINE BUFFER dataline1 FOR dataline.
DEFINE BUFFER branch FOR branch.

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE isCash AS LOGICAL NO-UNDO.
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.
DEFINE VARIABLE mItog AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE mCurr AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttElecSafe
	FIELD filial-id AS CHARACTER
	FIELD filial-name AS CHARACTER
	FIELD dbcr AS CHARACTER
	FIELD bal-acct AS CHARACTER
	FIELD DocCount AS INT64
	FIELD amt-rub  AS DECIMAL
	FIELD amt-usd  AS DECIMAL
	FIELD amt-eur  AS DECIMAL
	FIELD amt-kaz  AS DECIMAL
	FIELD amt-other  AS DECIMAL
	FIELD amt-itog  AS DECIMAL
	FIELD dbcr-sort AS CHARACTER
	INDEX ff filial-id bal-acct dbcr-sort
	.

/*
{tmprecid.def}
*/
/* Список филиалов, по которым готовим выписки */
/*
ASSIGN
 mFilialList = "0000,0300" 
mNmFil = "gb,tf"
.
*/
mFilialList = shFilial.

MESSAGE "Филиал" 
/* VIEW-AS ALERT-BOX */
UPDATE mFilialList.

{getdate.i}
{setdirel.i}
{elsafe.def}

/*
{setdest.i}
FOR EACH ttSafeType BREAK BY STRING(ttSafeType.op-entry-id) + ttSafeType.bal-acct:
	PUT UNFORMATTED
	STRING(ttSafeType.op-entry-id)
	";"
	ttSafeType.bal-acct
	SKIP.
END.
{preview.i}
*/
mFilial = mFilialList.
{elsafe.cre}
FIND FIRST branch 
	WHERE branch.branch-id EQ mFilialList
	NO-LOCK NO-ERROR.
FOR EACH ttSafeType BREAK BY ttSafeType.bal-acct + ttSafeType.dbcr-sort:
	IF FIRST-OF(ttSafeType.bal-acct + ttSafeType.dbcr-sort) THEN
	DO:
		CREATE ttElecSafe.
		ASSIGN
			ttElecSafe.filial-id = mFilialList
			ttElecSafe.filial-name = branch.name
			ttElecSafe.bal-acct  = ttSafeType.bal-acct
			ttElecSafe.dbcr  = ttSafeType.dbcr
			ttElecSafe.dbcr-sort  = ttSafeType.dbcr-sort
			.
	END.
    FIND FIRST op-entry
    	WHERE RECID(op-entry) EQ ttSafeType.op-entry-id
        AND op-entry.acct-db NE ?
    NO-LOCK NO-ERROR.
    IF AVAILABLE(op-entry) THEN
    DO:
	    FIND FIRST op
	    	WHERE op.op EQ op-entry.op
	    	AND op.op-status BEGINS CHR(251)
	    NO-LOCK NO-ERROR.
	    IF AVAILABLE(op) THEN
	    DO:
	    	mCurr = IF op-entry.currency NE "" THEN op-entry.currency ELSE "810".
	    	IF op.doc-num BEGINS "П" THEN
				mCurr = "810".
/*				IF SUBSTRING(op-entry.acct-cr, 6, 3) EQ "810" THEN "810" ELSE SUBSTRING(op-entry.acct-db, 6, 3).  */
			CASE mCurr:  
				WHEN "810" THEN
					ttElecSafe.amt-rub = ttElecSafe.amt-rub + op-entry.amt-rub.
				WHEN "840" THEN      
					ttElecSafe.amt-usd = ttElecSafe.amt-usd + op-entry.amt-cur.
				WHEN "978" THEN      
					ttElecSafe.amt-eur = ttElecSafe.amt-eur + op-entry.amt-cur.
				WHEN "398" THEN
					ttElecSafe.amt-kaz = ttElecSafe.amt-kaz + op-entry.amt-cur.
				OTHERWISE
					ttElecSafe.amt-other = ttElecSafe.amt-other + op-entry.amt-cur.
			END CASE.
			ASSIGN
				ttElecSafe.DocCount = ttElecSafe.DocCount + 1
				ttElecSafe.amt-itog = ttElecSafe.amt-itog + op-entry.amt-rub
				.
		END.
	END.
END. /* for ttSafeType */

	ASSIGN
		mItog[1] = 0.0
		mItog[2] = 0.0
		.
	RUN Insert_TTName("filial",ttElecSafe.filial-name).
	RUN Insert_TTName("opdate",STRING(end-date, "99.99.9999")).
	RUN Insert_TTName("ex", "EX-" + STRING(YEAR(end-date), "9999") + "-" + STRING(MONTH(end-date), "99") + "-" + STRING(DAY(end-date), "99") + "-Б").
	RUN BeginCircle_TTName("b1").
	FOR EACH ttElecSafe
		WHERE ttElecSafe.filial-id EQ mFilialList
		BY ttElecSafe.bal-acct + ttElecSafe.dbcr-sort:
		RUN Insert_TTName("balstr[b1]", 
		/*1*/
		ttElecSafe.bal-acct + 
		"~n" +  
		/*2*/
		" " +  
		"~n" + 
		/*3*/
		ttElecSafe.dbcr +  
		"~n" + 
		/*4*/
		" " +  
		"~n" + 
		/*5*/
		STRING(ttElecSafe.DocCount, ">>>>>>>>9") + 
		"~n" + 
		/*6*/
		" " + 
		"~n" + 
		/*7*/
		STRING(ttElecSafe.amt-rub, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*8*/
		" " + 
		"~n" + 
		/*9*/
		STRING(ttElecSafe.amt-usd, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*10*/
		STRING(ttElecSafe.amt-eur, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*11*/
		STRING(ttElecSafe.amt-kaz, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*12*/
		STRING(ttElecSafe.amt-other, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*13*/
		STRING(ttElecSafe.amt-itog, ">>>>>>>>>>>9.99") + 
		"~n" + 
		/*14*/
		" " +
		"/n"  
		).  
		IF ttElecSafe.bal-acct BEGINS "9" THEN
		DO:
			IF ttElecSafe.bal-acct NE "99998"
				AND ttElecSafe.bal-acct NE "99999" THEN
				mItog[2] = mItog[2] + ttElecSafe.amt-itog.
		END.
		ELSE
			mItog[1] = mItog[1] + ttElecSafe.amt-itog.

		RUN NextCircle_TTName("b1").
END.
	RUN EndCircle_TTName("b1").
	RUN Insert_TTName("ItogBal", STRING(mItog[1], ">>>>>>>>>>>9.99")).
	RUN Insert_TTName("ItogVBal",STRING(mItog[2], ">>>>>>>>>>>9.99")).
	RUN printvd.p("elpril4", INPUT TABLE ttnames). 
	
