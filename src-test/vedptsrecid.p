/*
    Copyright: 
     Filename: VEDPTS.P
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 13.02.2014 kam

*/

{globals.i}
{intrface.get i254}
{client.i}
{tmprecid.def}

DEFINE VARIABLE vCustName  AS CHAR NO-UNDO. 
DEFINE VARIABLE mDateOpen AS CHAR NO-UNDO.
DEFINE VARIABLE mDateClose AS CHAR NO-UNDO.  
DEFINE VARIABLE mDatePlanPTS AS CHAR NO-UNDO.
DEFINE VARIABLE mDatePTS AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mVidObespech AS CHARACTER FORMAT "X(25)".
/**/
DEFINE VARIABLE vModel  AS CHAR NO-UNDO.
DEFINE VARIABLE vVIN  AS CHAR NO-UNDO.
DEFINE VARIABLE vRelDate  AS CHAR NO-UNDO.
DEFINE VARIABLE vSerPTS  AS CHAR NO-UNDO.
DEFINE VARIABLE vNumPTS  AS CHAR NO-UNDO.
/**/
DEF VAR mSurr     AS CHAR NO-UNDO.

def new shared stream vvs.
def var fname as char  init "./vedpts.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

/* {getdates.i} */
{setdest.i &custom="printer.page-lines - "} /* == page-size(0) */
    /* OUTPUT TO "./_spool.txt". */
    PUT UNFORMATTED
        STRING("ФИО","x(40)")
        '|'
        STRING("Номер КД","x(25)")
        '|'
        STRING("Дата КД","x(10)")
        '|'
        "Дата зк КД"
        '|'
        "Плановая дата предоставления подлинника ПТС"
        '|'
        "Дата предоставления подлинника ПТС в банк"
		'|'
		"  Марка, модель ТС  "
		'|'
		"         VIN        "
		'|'
		"Год выпуска ТС"
		'|'
		"  Серия, номер ПТС  "
        SKIP
        .

output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
put stream vvs unformatted
    "ФИО" delim
    "Номер КД" delim
    "Дата КД" delim    
    "Дата зк КД" delim
    "Плановая дата предоставления подлинника ПТС" delim
    "Дата предоставления подлинника ПТС в банк" delim
	"Марка, модель ТС" delim
	"VIN" delim
	"Год выпуска ТС" delim
	"Серия, номер ПТС" delim
    eol.

    FOR EACH tmprecid,
		/**/
        FIRST loan WHERE recid(loan) EQ tmprecid.id
        /* AND loan.open-date >= beg-date */
        AND loan.cont-type BEGINS 'АВТО' 
        /* AND loan.open-date <= end-date */ 
        NO-LOCK, 
		/**/
		FIRST term-obl OF loan
        WHERE term-obl.cont-code = loan.cont-code
        AND term-obl.contract EQ loan.contract
        AND term-obl.idnt EQ 5
        AND term-obl.class-code EQ "term-obl-gar"
        NO-LOCK:
			/**/
            mDateOpen = STRING(loan.open-date,"99.99.9999").
			/**/
            IF loan.close-date <> ? THEN
				mDateClose = STRING(loan.close-date,"99.99.9999").
            ELSE 
				mDateClose = STRING("","x(10)").
			/**/	
            mSurr = term-obl.contract + "," +
                term-obl.cont-code        + "," +
                STRING(term-obl.idnt)     + "," +
                STRING(term-obl.end-date) + "," +
                STRING(term-obl.nn).
			/**/	
            mVidObespech =  GetXAttrValueEx ("term-obl", mSurr, "ВидОб", "111").
			/**/
               IF UPPER(TRIM(mVidObespech)) EQ "АВТОМОБИЛЬ" THEN
               DO:      /* avto */
                   RUN RE_CLIENT (loan.cust-cat, loan.cust-id, INPUT-OUTPUT vCustName).
				   /**/
                   mDatePTS = GetXAttrValueEx ("term-obl", mSurr, "ПриемПТС", "").
				   /* Марка, модель ТС */
				   vModel = GetXAttrValueEx ("term-obl", mSurr, "TCmodel", "").
				   /* VIN */
				   vVIN = GetXAttrValueEx ("term-obl", mSurr, "TCVIN", "ОТСУТСТВУЕТ").
				   /* Год выпуска ТС */
				   vRelDate = GetXAttrValueEx ("term-obl", mSurr, "TCyear", "").
				   /* Серия ПТС */
				   vSerPTS = GetXAttrValueEx ("term-obl", mSurr, "TCSER", "").
				   /* Номер ПТС */
				   vNumPTS = GetXAttrValueEx ("term-obl", mSurr, "TCNUMB", "").
				   /**/
                   mDatePlanPTS = STRING((loan.open-date + 10),"99.99.9999").
				   /**/
                   PUT UNFORMATTED
                       STRING(vCustName,"x(40)")
                       '|'
                       STRING(ENTRY(1, loan.cont-code, "@"),"x(25)")
                       '|'
                       mDateOpen
                       '|'
                       mDateClose
                       '|'
                       STRING(mDatePlanPTS,"x(43)")
                       '|'
                       STRING(mDatePTS, "x(41)")
					   '|'
					   STRING(vModel, "x(20)")
					   '|'
					   STRING(vVIN, "x(20)")
					   '|'
					   STRING(vRelDate, "x(14)")
					   '|'
					   STRING(vSerPTS + " " + vNumPTS, "x(20)")
                       SKIP
                       .
                       
        put stream vvs unformatted
            vCustName delim
            ENTRY(1, loan.cont-code, "@") delim
            mDateOpen delim
            mDateClose delim
            mDatePlanPTS delim
            mDatePTS delim
			vModel delim
			vVIN delim
			vRelDate delim
			vSerPTS + vNumPTS delim
            eol.
                       
               END.      /* avto */
      END.
      
      
output stream vvs close.
      
/* OUTPUT CLOSE.
     OS-COPY VALUE("./_spool.txt") VALUE("./_spool.tmp"). */
/*    OS-COMMAND VALUE("cp ./_spool.txt ./_spool.tmp"). */
{preview.i}

RUN sndbispc ("file=" + fname + ";class=bq").
 
