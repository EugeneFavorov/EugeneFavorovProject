/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: dps_prezent_ved.p
      Comment: Ведомость по акции Золотая лихорадка
   Parameters: 
         Uses:
      Used by: 
      Created: 06/08/2008 feok
     Modified: 14/10/2009 Jadv (0108906)
		24/06/2014 KAU добавлял поля город, счет 91202. Переделал поле офис теперь там название подразделения.
				Изменил вывод ИНН для правильного отображения.
*/

{globals.i}
 
/* Строка параметров */
DEF INPUT PARAM iStr AS CHAR NO-UNDO.
DEF VAR iPersonId AS INT64 NO-UNDO.
DEF VAR pOk AS LOG NO-UNDO.
DEF VAR dt AS DATE NO-UNDO.
DEF VAR ost AS DEC NO-UNDO.
DEF VAR begActionDate AS DATE NO-UNDO.
DEF VAR endActionDate AS DATE NO-UNDO.
DEF VAR closed-date AS DATE NO-UNDO.
DEF VAR endVidDate AS DATE NO-UNDO.
DEF VAR acctdata AS CHAR NO-UNDO.
DEF VAR dtfrom AS DATE NO-UNDO.
DEF VAR dtfrom_g AS DATE NO-UNDO.
DEF VAR cnt AS INT NO-UNDO.
DEF buffer bsigns for signs.
DEF VAR t AS CHAR NO-UNDO.
def new shared stream vvs.
def var fname as char  init "./dps_prezent.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def var mAdr as char no-undo.

DEF TEMP-TABLE tmprec NO-UNDO
  field branch-id like branch.short-name
  field city as CHAR
  field acct as CHAR
  field cust-id as int64
  field fio AS CHAR
  field CID AS CHAR
  field docum AS CHAR
  field tel AS CHAR
  field adr AS CHAR
  field inn AS CHAR
  field op-date AS DATE
  field user-inspector LIKE op.user-inspector
  field rate-comm like comm-rate.rate-comm
.


begActionDate = DATE('04/04/2013').
endActionDate = DATE('31/12/2013').
endVidDate = DATE("28/02/2014").

{setdest.i}

FOR EACH signs
	WHERE signs.file-name = "acct" AND 
		  signs.code = "form-type-code" AND 
		  signs.code-value = "Слиток 585" NO-LOCK:
	FIND FIRST acct WHERE acct.acct EQ ENTRY(1,signs.surrogate) NO-LOCK.

	FOR EACH op-entry 
		WHERE op-entry.acct-cr = acct.acct AND op-entry.op-date <> ? NO-LOCK,
		FIRST op OF op-entry NO-LOCK:
		FIND FIRST bsigns 
			WHERE bsigns.file-name = "op" AND bsigns.code = "ФЛподарок" 
			    and bsigns.surrogate EQ STRING(op.op) NO-LOCK NO-ERROR.
		IF AVAIL bsigns THEN DO:
			FIND FIRST person 
				WHERE person.person-id = INTEGER(bsigns.code-value) NO-LOCK NO-ERROR.
			RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT mAdr).
	
		END. ELSE DO:
			RELEASE person.
			mAdr = "".
		END.
		/*{findcom1.i
			&dir=last
			&rsum=ost
			&comm-rate=comm-rate
			&rcom="'Слиток_585'"
			&since1=" le op.op-date "
		}*/
		FIND LAST comm-rate
				 WHERE comm-rate.filial-id = shfilial
				   AND comm-rate.commi eq 'Слиток_585'
				   NO-LOCK NO-ERROR.
		FIND FIRST branch where branch.branch-id EQ acct.branch-id NO-LOCK NO-ERROR.
		FIND FIRST code WHERE code.class EQ 'TitulBr' 
				and CAN-DO (code.val,acct.branch-id) NO-LOCK NO-ERROR.
		CREATE tmprec.
		ASSIGN
			tmprec.branch-id = branch.short-name
			tmprec.city = code.name
			tmprec.acct = substring(acct.number,1,5) + '-' + substring(acct.number,6,3) + '-' + substring(acct.number,9,1) + '-' +  substring(acct.number,10,4) + '-' +  substring(acct.number,14)
			tmprec.op-date = op.op-date
			tmprec.user-inspector = op.user-inspector
			tmprec.rate-comm = comm-rate.rate-comm WHEN AVAIL( comm-rate )
			tmprec.cust-id = person.person-id WHEN AVAIL( person )
			tmprec.fio = person.name-last + " " + person.first-names WHEN AVAIL( person )
			tmprec.cid = GetXattrValueEx("person", string(person.person-id), "CID", "") WHEN AVAIL ( person )
			tmprec.docum = person.document-id + " " + person.document + " выдан " + GetXattrValueEx("person", string(person.person-id), "Document4Date_vid", "") + " " + person.issue WHEN AVAIL( person )
			tmprec.adr = mAdr
	


			.
/*kau*/	   IF AVAIL person and person.inn NE ? and person.inn NE '' 
		THEN tmprec.inn = ("'" + person.inn + "'").
	   tmprec.docum = REPLACE (tmprec.docum,CHR(10),'').
	END.
END.

PUT UNFORMATTED "Нажмите ESC для выгрузки отчета в BisPC" skip(1).

{preview.i}

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream vvs unformatted
    "офис" delim
    "город" delim
    "счет" delim
    "Дата выдачи" delim
    "сумма" delim
    "ИНН" delim
    "ФИО клиента" delim
    "CID" delim
    "паспортные данные" delim
	"адрес клиента" delim
	"кассир"
    eol.

  for each tmprec no-lock 
	by tmprec.branch-id by tmprec.op-date:
    put stream vvs unformatted
		tmprec.branch-id delim
/*kau*/		tmprec.city delim
/*kau*/		tmprec.acct delim
	        tmprec.op-date delim
		string(tmprec.rate-comm) delim
	    tmprec.inn delim
	    tmprec.fio delim
/*kau*/	    tmprec.cid delim
	    tmprec.docum delim
	    tmprec.adr delim
        tmprec.user-inspector
      eol.
  end.
output stream vvs close.

RUN sndbispc ("file=" + fname + ";class=bq").