{globals.i}
{intrface.get cust}     
{intrface.get "comm"}
{sh-defs.i}
/*
{prn-doc.def &with_proc=YES}
*/
DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vClName AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mAmount AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI AS INT64 NO-UNDO.
DEFINE VARIABLE vLnRate AS DECIMAL NO-UNDO.
DEFINE VARIABLE mComm AS DECIMAL NO-UNDO.
DEFINE VARIABLE mCommDate AS DATE NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan-acct1 FOR loan-acct.
DEFINE BUFFER term-obl FOR term-obl.

/*
RUN BeginCircle_TTName("garan").
*/
{getdate.i}
/*
{setdest.i}
*/
OUTPUT TO VALUE("./garan.txt") CONVERT TARGET "1251".

{headgaran.i}

FOR EACH loan
	WHERE loan.filial-id EQ shFilial
	and loan.contract EQ "Кредит"
	and loan.cont-type EQ "Гарантии"
	and 
	(loan.close-date EQ ? OR loan.close-date GT end-date)
	AND
	loan.open-date LE end-date
	NO-LOCK:

	RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, "",
	                          OUTPUT vClName[1],
	                          OUTPUT vClName[2],
	                          INPUT-OUTPUT vClName[3] ).

	vClName[1] = vClName[1] + " " + vClName[2].

	sh-bal = 0.
	FOR EACH loan-acct1 
		WHERE loan-acct1.contract EQ loan.contract
		and loan-acct1.cont-code EQ loan.cont-code
		and loan-acct1.acct-type EQ "КредЛинВГ"
		NO-LOCK
		BY loan-acct1.since DESC:
		LEAVE.
	END.
	IF AVAILABLE(loan-acct1) THEN
	DO:
		RUN acct-pos IN h_base (loan-acct1.acct,
		                        loan-acct1.currency,
		                        end-date,
		                        end-date,
		                        CHR(251)).
	END.
	ELSE
		sh-bal = 0.

	IF sh-bal NE 0 THEN
		FIND FIRST loan-acct
			WHERE RECID(loan-acct) EQ RECID(loan-acct1)
			NO-LOCK NO-ERROR.
	ELSE
	DO:
		FOR EACH loan-acct 
			WHERE loan-acct.contract EQ loan.contract
			and loan-acct.cont-code EQ loan.cont-code
			and loan-acct.acct-type EQ "КредВГар"
			NO-LOCK
			BY loan-acct.since DESC:
			LEAVE.
		END.

		RUN acct-pos IN h_base (loan-acct.acct,
		                        loan-acct.currency,
		                        end-date,
		                        end-date,
		                        CHR(251)).
	END.

	mAmount = " ".

	FOR EACH term-obl
		WHERE term-obl.contract EQ loan.contract
		AND term-obl.cont-code EQ loan.cont-code
		AND term-obl.idnt EQ 5
		NO-LOCK
		BREAK BY term-obl.amt-rub:
		IF FIRST-OF(term-obl.amt-rub) THEN
			mAmount = STRING(term-obl.amt-rub, ">>>>>>>>>>>>9.99").
		ELSE
			mAmount = mAmount + "," + STRING(term-obl.amt-rub, ">>>>>>>>>>>>9.99").
	END.

	vLnRate = Get_Comm ("%Выд",          /* Код комиссии */
	                    ?,              /* Идентификатор счета */
	                    loan.currency,  /* Код приведенной валюты (? - пу) */
	                    loan.contract + "," +
	                    loan.cont-code, /* Код КАУ                ("" - пу) */
	                    0.00,           /* Минимальный остаток    (0 - пу) */
	                    0,              /* Период/срок            (0 - пу) */
	                    loan.open-date).         /* Дата, на которую требуется получить %%	*/
	mComm = vLnRate.
	mCommDate = loan.open-date.

	DO vI = 1 TO NUM-ENTRIES(mAmount):

		PUT UNFORMATTED
		IF vI EQ 1 THEN loan.cust-id ELSE " " /*1*/
		CHR(9)
		IF vI EQ 1 THEN vClName[1]  ELSE " " /*2*/
		CHR(9)
		" " /*3*/
		CHR(9)
		IF vI EQ 1 THEN ENTRY(1, loan.cont-code, "@") ELSE " " /*4*/
		CHR(9)
		IF loan.currency EQ "" THEN "810" ELSE loan.currency /*5*/
		CHR(9)
		IF vI EQ 1 THEN STRING(ABSOLUTE(sh-val))  ELSE " "   /*6*/
		CHR(9)
		IF vI EQ 1 THEN STRING(ABSOLUTE(sh-bal))  ELSE " " /*7*/
		CHR(9)
		IF vI EQ 1 THEN STRING(loan.open-date) ELSE " " /*8*/
		CHR(9)
		IF vI EQ 1 THEN STRING(loan.end-date) ELSE " " /*9*/
		CHR(9)
				IF loan.currency EQ "" THEN "810" ELSE loan.currency /*10*/
		CHR(9)
		ENTRY(vI, mAmount) /*11*/
		CHR(9)
		" " /*12*/
		CHR(9)
		" " /*13*/
		CHR(9)
		" " /*14*/
		CHR(9)
		" " /*15*/
		CHR(9)
		"'"
		IF vI EQ 1 THEN ENTRY(1, loan-acct.acct, "@") ELSE " " /*16*/
		CHR(9)
		IF vI EQ 1 THEN STRING(mComm) ELSE " " /*17*/
		CHR(9)
		IF vI EQ 1 THEN STRING(mCommDate) ELSE " " /*18*/
	
		SKIP. 

	END.

END.
/*
mStr = 
"1~n" +
"2~n" +
"3~n" +
"4~n" +
"5~n" +
"6~n" +
"7~n" +
"8~n" +
"9~n" +
"10~n" +
"11~n" +
"12~n" +
"13~n" +
"14~n" +
"15~n" +
"16~n" +
"17~n" +
"18~n". 

    RUN Insert_TTName ("garan",  mStr). 
    RUN NextCircle_TTName("garan").

RUN EndCircle_TTName("garan").   
RUN printvd.p("garan" , INPUT TABLE ttnames).
*/
/*
{preview.i}
*/
OUTPUT CLOSE.
RUN sndbispc ("file=" + "garan.txt" + ";class=xls").  

{intrface.del}

 