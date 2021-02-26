FUNCTION ConvertLoanInt CHAR(INPUT iId_K AS INT64, INPUT iId_D AS INT64):
    DEFINE VAR sRes AS CHAR NO-UNDO.
    sRes = ?.
    CASE iId_K:
        WHEN 377 THEN DO:
        	sRes = '13'. /*13*/
        END.
    END CASE.
    RETURN sRes.
END FUNCTION.   

/**/
DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
DEF VAR datepereschet AS DATE NO-UNDO.

{tmprecid.def} 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR strOP AS CHAR.

/*OUTPUT TO VALUE(tag5.txt) CONVERT TARGET "1251".*/

datepereschet = DATE("31/03/2016").

FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:

	if loan.since <> datepereschet AND
	   AVAIL loan-int then do:
	    RUN l-calc2.p ("Кредит",       /* Назначение договора. */
	           loan.cont-code,      /* Номер договора. */
	           date(datepereschet),   /* Окончание договора + день для выполнения автом. */
	           FALSE,		/* включать/не включать пересчет течений договора */
	           TRUE).		/* выводить/ не выводить протокол на экран */
	end.



  FOR EACH loan-int OF loan NO-LOCK:
  	strOP = ConvertLoanInt(loan-int.id-k,loan-int.id-d).
  	IF strOP <> ? THEN
	PUT UNFORMATTED
/* ╚д┼нтид╚к"╬а ─о├о┬ораа┬о ┬н┼и═е╔асибтем┼а*/
	iRecIDloan 
	"^"
/* ╥и╧а╬п┼а&╚и */
	strOP
	"^"
/* ─атаа╬п┼а&╚и */
	STRING(YEAR(loan-int.mdate), "9999") + STRING(MONTH(loan-int.mdate), "99") + STRING(DAY(loan-int.mdate), "99")
	"^"
/* ╤г╠м ╬п┼а&╚и */
	STRING(loan-int.amt-rub, ">>>>>>>>>>>9.99")
	"^"
/* Система */
	'ПАО "ПЛЮС БАНК"'
	CHR(13) + CHR(10)
	.
  END.
END.
OUTPUT CLOSE.

{intrface.del}
