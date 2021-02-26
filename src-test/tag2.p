DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 

DEFINE VARIABLE mComm AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCommDias AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCommPeriod AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNum AS INT64 NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER comm-rate FOR comm-rate.
DEFINE BUFFER comm-rate1 FOR comm-rate.

ASSIGN
	mComm = "%Кред,%КрПр,Пеня-К,Пеня%К"
	mCommDias = "201,210,216,217"
	mCommPeriod = "1,1,2,2"
.
/*
OUTPUT TO VALUE("tag2.txt") CONVERT TARGET "1251".
*/
FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FOR EACH comm-rate
		WHERE comm-rate.kau EQ loan.contract + "," + loan.cont-code
		NO-LOCK
		BREAK BY comm-rate.commission:
		IF FIRST-OF(comm-rate.commission) THEN
			IF CAN-DO(mComm, comm-rate.commission) THEN
			DO:
				FOR EACH comm-rate1
					WHERE comm-rate1.kau EQ loan.contract + "," + loan.cont-code
					AND comm-rate1.commission EQ comm-rate.commission
					AND comm-rate1.since LE end-date
					NO-LOCK
					BY comm-rate1.since:
/*					LEAVE.
				END. */
				mNum = 0.
				DO mI = 1 TO NUM-ENTRIES(mComm):
					IF ENTRY(mI, mComm) EQ comm-rate.commission THEN
						ASSIGN
							mNum = mI
							mI = 5.
				END.
				IF mNum GT 0 and avail comm-rate1 THEN
					PUT UNFORMATTED
				/* Идентификатор договора во внешней системе */
					iRecIDloan 
					"^"
				/* Тип ставки  */
					ENTRY(mNum, mCommDias)
					"^"
				/* Дата ставки  */
					STRING(YEAR(comm-rate1.since), "9999") + STRING(MONTH(comm-rate1.since), "99") + STRING(DAY(comm-rate1.since), "99")
					"^"
				/* Значение процентной ставки */
					(IF comm-rate1.rate-comm EQ 0 THEN "0" ELSE TRIM(STRING(comm-rate1.rate-comm, ">>>>9.9999")))
					"^"
				/* Тип процентной ставки 1 - % 2 - фиксированная*/
					(IF comm-rate1.rate-fixed THEN "3" ELSE ENTRY(mNum, mCommPeriod))
					"^"
				/* Система */
					'ПАО "ПЛЮС БАНК"'
					"^"
					CHR(13) + CHR(10)
					.
			   end. /* FOR EACH comm-rate1 */
		END.
	END.
END.
/*
OUTPUT CLOSE.
*/
