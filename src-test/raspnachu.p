{globals.i}

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE vKindDate   AS DATE   NO-UNDO.
DEFINE VARIABLE vKindChar   AS CHAR   NO-UNDO.
DEFINE VARIABLE vTemplInt   AS INT64    NO-UNDO.
DEFINE VARIABLE vLongInt    AS INT64    NO-UNDO.
DEFINE VARIABLE vCommHandle AS HANDLE NO-UNDO. /*указатель на  библиотеку комиссий*/
DEF VAR vResult   AS DEC  NO-UNDO. /*переменные */
DEF VAR vResult1  AS DEC  NO-UNDO. /*для        */
DEF VAR vBegDate  AS DATE NO-UNDO. /*вызова     */
DEF VAR vComm     AS CHAR NO-UNDO. /*код комисии*/
DEF VAR vInter    AS CHAR NO-UNDO. /*схема начисления*/
DEF VAR vFlag     AS LOG  NO-UNDO. /*начисления */
DEF VAR fNum      AS INT64    NO-UNDO. /*номер строки для вывода*/
DEF VAR fLoan     AS CHAR   NO-UNDO. /*номер и дата договора*/
DEF VAR vSubCod   AS CHAR NO-UNDO. /*тип остатка*/
DEF VAR vRateComm AS DEC  NO-UNDO. /*ставка на момент открытия*/
DEF VAR vAcctCust AS CHAR NO-UNDO. /*наименование владельца счета*/
DEF VAR vAmount   AS DEC  NO-UNDO. /*ставка на момент открытия*/

DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op FOR op.
DEFINE BUFFER kau FOR kau.
DEFINE BUFFER kau-entry FOR kau-entry.
DEFINE BUFFER comm-rate FOR comm-rate.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.

DEF TEMP-TABLE Rep-tt NO-UNDO
  FIELD ContCode AS CHAR        /*номер вклада*/
  FIELD TypeOp   AS CHAR        /*тип - начисление/уплата*/
  FIELD OpenDate AS DATE        /*дата открытия*/
  FIELD StDate   AS DATE        /*дата привлечения средств*/
  FIELD Acct     AS CHAR        /*счет на дату*/
  FIELD AcctCust AS CHAR        /*владелец счета*/
  FIELD Rate     AS DEC         /*% ставка*/
  FIELD Amount   AS DEC         /*% сумма за день*/
  FIELD EndDate  AS DATE INIT ? /*дата возврата средств*/
INDEX main-ind ContCode OpenDate.
/*
{tmprecid.def}
*/
FORM
  fNum
     COLUMN-LABEL "N!п/п"
     FORMAT       ">>>>9"
  fLoan
     COLUMN-LABEL "Номер и дата договора"
     FORMAT       "x(31)"
  Rep-tt.AcctCust
     COLUMN-LABEL "Наименование!владельца!счета"
     FORMAT       "x(30)"
  Rep-tt.Acct
     COLUMN-LABEL "N!Cчета"
     FORMAT       "x(20)"
  Rep-tt.TypeOp
     COLUMN-LABEL "Содержание!операции"
     FORMAT       "x(12)"
  Rep-tt.StDate
     COLUMN-LABEL "Дата!привлечения"
     FORMAT       "99/99/9999"
  Rep-tt.EndDate
     COLUMN-LABEL "Дата!возврата"
     FORMAT       "99/99/9999"
  Rep-tt.Rate
     COLUMN-LABEL "Ставка %"
     FORMAT       ">>>>>>>9.99"
  Rep-tt.Amount
     COLUMN-LABEL "Сумма"
     FORMAT       ">>>>>>>9.99"
HEADER "Приложение 1 к Инструкции"                                AT 90 SKIP
       "'О порядке бухгалтерского учета операций начисления и"    AT 62 SKIP
       "уплаты / получения процентов по привлеченным и"           AT 69 SKIP
       "размещенным средствам'"                                   AT 93 SKIP
       "Руководителю ___________________________________________" AT 59 SKIP
       "(наименование подразделения)"                             AT 75 SKIP
       "РАСПОРЯЖЕНИЕ"                                             AT 50 SKIP
       "На бухгалтерское отражение операций начисления и уплаты процентов по" AT 20 SKIP
       "следующим договорам на привлечение средств"                          AT 30 SKIP
       "за " + STRING(end-date, "99.99.9999") + " г."   FORMAT "X(16)"  AT 50       SKIP(4)

WITH FRAME RepFrame DOWN WIDTH 150 TITLE "" .

{def_work.i new}
{dpsproc.def}

{f_for_t.i}

{intrface.get loan}
{intrface.get tmess}

RUN LOAD_NACHTOOL (YES, OUTPUT vCommHandle).

/* Список филиалов, по которым готовим выписки */
ASSIGN
mFilialList = "0000,0300"
mNmFil = "gb,tf"
.
{getdate.i}
{setdirel.i}

/* Цикл формирования выписок */
DO mI = 1 TO NUM-ENTRIES(mFilialList):
{empty Rep-tt}               /*чистим рабочую таблицу*/
    FOR EACH op-entry
	    WHERE op-entry.op-date EQ end-date
	    AND op-entry.filial-id EQ ENTRY(mI, mFilialList)
	    AND op-entry.acct-db BEGINS "70606"
	    AND op-entry.acct-cr BEGINS "47426"
		NO-LOCK, 
		FIRST op
	    WHERE op.op EQ op-entry.op
	    AND op.op-date EQ end-date
	    AND op.op-status BEGINS CHR(251)
	    NO-LOCK:
	    IF NUM-ENTRIES(op-entry.kau-cr) EQ 3 THEN
       DO:
		    FIND FIRST loan
		    	WHERE loan.contract EQ ENTRY(1, op-entry.kau-cr)
		    	AND loan.cont-code  EQ ENTRY(2, op-entry.kau-cr)
		    NO-LOCK NO-ERROR.
		    FOR EACH loan-acct
		    	WHERE loan-acct.contract EQ loan.contract
		    	AND loan-acct.cont-code  EQ loan.cont-code
		    	AND loan-acct.acct-type EQ "Депоз"
		    	AND loan-acct.since LE end-date
		    NO-LOCK 
		    BY loan-acct.since DESC:
		    	LEAVE.
		    END.
		    FIND FIRST acct
		    	WHERE acct.acct EQ loan-acct.acct
		    NO-LOCK NO-ERROR.

	        FIND LAST cust-corp WHERE
	                  cust-corp.cust-id = loan.cust-id
	        NO-LOCK NO-ERROR.

	        vAcctCust  = IF AVAIL cust-corp
	                     THEN cust-corp.name-short
	                     ELSE "?".
          FOR EACH comm-rate
            WHERE comm-rate.filial-id EQ ENTRY(mI, mFilialList) AND 
                  comm-rate.since LE gend-date AND
                  comm-rate.kau EQ ENTRY(1, op-entry.kau-cr) + "," + ENTRY(2, op-entry.kau-cr)
            NO-LOCK
            BY comm-rate.since DESC:
            LEAVE.
          END.

    	    vAmount = (IF op-entry.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
/*	        END. */

	      /*создаем рабочую таблицу*/
	      CREATE Rep-tt.
	      ASSIGN
	        Rep-tt.ContCode = ENTRY(1, loan.cont-code, "@")
	        Rep-tt.TypeOp   = "начисление%"
	        Rep-tt.OpenDate = loan.open-date
	        Rep-tt.Acct     = TRIM(ENTRY(1, loan-acct.acct, "@"))
	        Rep-tt.Rate     = comm-rate.rate-comm
	        Rep-tt.EndDate  = loan.end-date WHEN loan.end-date <> ?
	        Rep-tt.AcctCust = vAcctCust
	        Rep-tt.StDate   = loan.open-date
	        Rep-tt.Amount   = vAmount
	        .
	    END.
	END.
/*вывод данных*/
{setdest.i}

fNum = 1.
FOR EACH Rep-tt USE-INDEX main-ind WITH  FRAME RepFrame:
   DISPLAY
     fNum
     string(Rep-tt.ContCode,"x(20)") + " " + STRING(Rep-tt.OpenDate,"99/99/9999") @ fLoan
     Rep-tt.AcctCust
     Rep-tt.Acct
     Rep-tt.TypeOp
     Rep-tt.StDate
     Rep-tt.EndDate WHEN Rep-tt.EndDate <> ? @ Rep-tt.EndDate
     Rep-tt.Rate
     Rep-tt.Amount
     .
   DOWN.
   fNum = fNum + 1.
END.

DO WITH FRAME RepFrame :
  UNDERLINE
    fNum
    fLoan
    Rep-tt.AcctCust
    Rep-tt.Acct
    Rep-tt.TypeOp
    Rep-tt.StDate
    Rep-tt.EndDate
    Rep-tt.Rate
    Rep-tt.Amount
    .
END.

PUT UNFORMATTED
  SKIP(1)
  'Подпись уполномоченного лица __________________________(должность, Ф.И.О.) "__"_____________ 201 г'
  SKIP(1).

{preview.i}
 OS-COPY VALUE("./_spool.tmp") VALUE( mDir + mDirElec + "//raspnach-ul-" + ENTRY(mI, mNmFil) + "-" + STRING(end-date, "99.99.9999") + ".txt"). 

END.
{intrface.del}
