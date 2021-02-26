{globals.i}
{chkacces.i}


def temp-table tmprec NO-UNDO

  field fio as character  /*ФИО Автора*/
  field contcode as character      /*НОМЕР КД*/
  field opendate as date /*Дата начала действия условия*/
  field fak_date as date /*дата фактического внесения условия*/
  field acctdb as CHARACTER    /*счет по дб*/
  field enddate as date /*Срок договора*/
  field kol_vo_day as INT /*кол-во дней начало-факт.дат.*/
  field stat like Loan.loan-status 
.

def var fio as character.
def new shared stream gsv.
def var fname as char  init "./conditionality.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10). 
DEF VAR str  AS CHAR FORMAT "x(120)".
def var surr as character no-undo.


{getdates.i}
{get-fmt.i &obj='" + ""b-Acct-Fmt"" + "'}

{setdest.i &col=170 }

PUT UNFORMATTED
  "┌──────────────────────┬──────────┬──────────────────────┬───────────┬─────────────┬───────────────────┬─────────────┬────────┐" SKIP
  "│№ Договора            │Дата факт.│  № Счета             │Дата начала│Срок договора│  ФИО автора       │Проверка     │Статус  │" SKIP
  "│	                  │внес.усл. │  по дебету           │           │ действия усл│                   │             │договора│" SKIP 
  "├──────────────────────┼──────────┼──────────────────────┼───────────┼─────────────┼───────────────────┼─────────────┼────────┤" SKIP.


	 FOR each history WHERE history.file-name EQ 'loan-cond' 
			    AND history.modif-date >= beg-date 
			    AND history.modif-date <= end-date 
			    AND ENTRY(1, history.field-ref, ",") EQ "Кредит"
				NO-LOCK,
 
first loan-cond where  history.field-ref EQ (loan-cond.contract + ","
							 + loan-cond.cont-code + "," 
							+ STRING(loan-cond.since,"99/99/99"))
	no-lock:
DO:
Surr = loan-cond.contract         + "," +
               loan-cond.cont-code        + "," +
               STRING(loan-cond.since,"99/99/99")     .
      
Str =GetXAttrValueEx ("loan-cond",
                             Surr,
                    "CondEndDate",
                              "").
END.
FOR each loan-acct of loan-cond  where loan-acct.cont-code EQ loan-cond.cont-code
				/*AND CAN-DO("Кредит",/*"КредРасч",*/ACCT-TYPE)*/
				AND ACCT-TYPE EQ "Кредит"
				NO-LOCK:

find first  op-entry where        (op-entry.acct-db EQ loan-acct.acct) 
			OR (op-entry.acct-cr EQ loan-acct.acct) 
						
NO-LOCK no-error.
END.
FIND FIRST _User WHERE _User._Userid EQ history.user-id 
	NO-LOCK NO-ERROR.
	fio=_User._User-Name. 

FIND FIRST Loan WHERE loan.cont-code EQ loan-cond.cont-code   
	NO-LOCK NO-ERROR.


  
      create tmprec.
      assign
        tmprec.contcode = loan-cond.cont-code
        tmprec.fak_date = history.modif-date
	tmprec.acctdb = (if avail op-entry then op-entry.acct-db  ELSE "см. статус либо док. арх. датой")
  	tmprec.opendate = loan-cond.since
	tmprec.enddate =DATE(str)
	tmprec.fio =fio
        tmprec.kol_vo_day = (tmprec.opendate - tmprec.fak_date)
        tmprec.stat=loan.loan-status
     .

      put unformatted "│"
  	string (tmprec.contcode,"x(22)") "│"
	string (tmprec.fak_date,"99/99/9999") "│"        
	string (tmprec.acctdb, "x(22)") "│"
  	string (tmprec.opendate,"99/99/9999") "│"
 	string (tmprec.enddate,"99/99/9999") "│"
        string (tmprec.fio, "x(40)") "│"
	string (tmprec.kol_vo_day, "->>>9") "|"
	string (tmprec.stat, "x(6)") "│"
      skip. 
end.

PUT UNFORMATTED

  "└──────────────────────┴──────────┴──────────────────────┴───────────┴─────────────┴───────────────────┴─────────────┴────────┘" SKIP.

{preview.i &col=170}

output stream gsv to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream gsv unformatted
    "№ Договора" delim
    "Дата фактического внесения условия" delim
    "№ Счета по дебету" delim
    "Дата начала действия условия" delim
    "Срок договора" delim
    "ФИО автора" delim
    "Проверка" delim
    "Статус договора" eol.
    
  for each Tmprec no-lock:
    put stream gsv unformatted
      tmprec.contcode delim
      tmprec.fak_date  delim
      tmprec.acctdb delim      
      tmprec.opendate delim
      tmprec.enddate delim
      tmprec.fio    delim
      tmprec.kol_vo_day  delim
      tmprec.stat eol.
end.
output stream gsv close.

MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + fname + ";class=bq").


{intrface.del}

