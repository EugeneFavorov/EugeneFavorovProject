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
DEF buffer bbsigns for signs.
DEF VAR t AS CHAR NO-UNDO.
def new shared stream vvs.
def var fname as char  init "./dps_prezent.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def var mAdr as char no-undo.
def var summ2014 as decimal no-undo init 0.
def var i2014 as int64 no-undo.
def var mCid AS char no-undo.
def var lastCid as char no-undo.
def var mFio as char no-undo.
def var listAccts as char no-undo.
def var numAcct as char no-undo.
def var iIndex as int64 no-undo.
def var strDps as char no-undo.

listAccts = '70606810101096404001,70606810304006412181,70606810703536412181,70606810901166412181,70606810505986412071,70606810901166412071'.
i2014 = 2014.

DEF TEMP-TABLE tmprec NO-UNDO
  field branch-id like acct.branch-id
  field cust-id as int64
  field cid as CHAR
  field fio AS CHAR
  field docum AS CHAR
  field tel AS CHAR
  field adr AS CHAR
  field inn AS CHAR
  field country AS CHAR
  field op-date AS DATE
  field birthday AS DATE
  field user-inspector LIKE op.user-inspector
  field rate-comm like comm-rate.rate-comm
.
{empty tmprec}
begActionDate = DATE('04/04/2013').
endActionDate = DATE('31/12/2013').
endVidDate = DATE("28/02/2014").

{setdest.i}

lastCid = ''.

FOR EACH signs
	WHERE signs.file-name = "acct" AND 
		  signs.code = "form-type-code" AND 
		  signs.code-value = "Слиток 585" NO-LOCK:
	FIND FIRST acct WHERE acct.acct EQ ENTRY(1,signs.surrogate) NO-LOCK.

	FOR EACH op-entry 
		WHERE op-entry.acct-cr = acct.acct AND op-entry.op-date <> ? AND YEAR(op-entry.op-date) = i2014 NO-LOCK,
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
       /*     message string(op.doc-num) + ' ' + string(op.op-date) view-as alert-box. */
        END.
		

		
		
		/*{findcom1.i			&dir=last
			&rsum=ost
			&comm-rate=comm-rate
			&rcom="'Слиток_585'"
			&since1=" le op.op-date "
		}*/
		FIND LAST comm-rate
				 WHERE comm-rate.filial-id = shfilial
				   AND comm-rate.commi eq 'Слиток_585'
				   NO-LOCK NO-ERROR.

		CREATE tmprec.
		ASSIGN
			tmprec.branch-id = acct.branch-id
			tmprec.op-date = op.op-date
			tmprec.user-inspector = op.user-inspector
			tmprec.rate-comm = comm-rate.rate-comm WHEN AVAIL( comm-rate )
			tmprec.cust-id = person.person-id WHEN AVAIL( person )
			tmprec.fio = person.name-last + " " + person.first-names WHEN AVAIL( person )
			tmprec.fio = op.details when not avail (person)
			tmprec.inn = person.inn WHEN AVAIL( person )
			tmprec.birthday = person.birthday WHEN AVAIL( person )
			tmprec.docum = person.document-id + " " + person.document + " выдан " + GetXattrValueEx("person", string(person.person-id), "Document4Date_vid", "") + " " + person.issue WHEN AVAIL( person )
			tmprec.adr = mAdr
			tmprec.cid = GetXattrValueEx("person", string(person.person-id), "CID", "") WHEN AVAIL ( person )
			tmprec.country = person.country-id WHEN AVAIL( person )
			.
	END.
END.

DO cnt = 1 TO NUM-ENTRIES(listAccts):
    numAcct = entry(cnt,listAccts,",").
    for each op-entry where
        /* substring(op-entry.acct-db,1,20) = numAcct */
        op-entry.acct-db begins numAcct
        and op-entry.op-status begins "√"
        and year(op-entry.op-date) = i2014 no-lock, first op of op-entry no-lock:
        CREATE tmprec.
        ASSIGN
            tmprec.op-date = op.op-date
            tmprec.user-inspector = op.user-inspector
            tmprec.rate-comm = op-entry.amt-rub
            tmprec.fio = op.details
            .
        iIndex = index(op.details,'договор').
        if iIndex > 0 then do:
            strDps = substring(op.details,iIndex).
            if num-entries(strDps,' ') > 1 then do:
                strDps = trim(replace(entry(2,strDps,' '),'№','')).
                find first loan where loan.doc-ref = strDps
                  and loan.contract = 'dps' 
                  and loan.cust-cat = 'Ч'
                  no-lock no-error.
                    if avail loan then do:
                        FIND FIRST person 
                            WHERE person.person-id = loan.cust-id NO-LOCK NO-ERROR.
                        RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT mAdr).
                        ASSIGN
                            tmprec.cust-id = person.person-id WHEN AVAIL( person )
                            tmprec.fio = person.name-last + " " + person.first-names WHEN AVAIL( person )
                            tmprec.inn = person.inn WHEN AVAIL( person )
                            tmprec.birthday = person.birthday WHEN AVAIL( person )
                            tmprec.docum = person.document-id + " " + person.document + " выдан " + GetXattrValueEx("person", string(person.person-id), "Document4Date_vid", "") + " " + person.issue WHEN AVAIL( person )
                            tmprec.adr = mAdr
                            tmprec.cid = GetXattrValueEx("person", string(person.person-id), "CID", "") WHEN AVAIL ( person )
                            tmprec.country = person.country-id WHEN AVAIL( person )
                        .
                    end.        
            end.
        end.
    end.
END.

PUT UNFORMATTED "Нажмите ESC для выгрузки отчета в BisPC" skip(1).

{preview.i}

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream vvs unformatted
    "код клиента" delim
    "фио клиента" delim
    "акция" delim
    "сумма подарка" delim
    "удержан ндфл" delim
    "сумма подарков с начала 2014" delim
	"дата выплаты" delim
	"дата рождения" delim
	"гражданство" delim
	"серия номер паспорта" delim
	"адрес" delim
    eol.

  for each tmprec no-lock 
	 break by tmprec.cust-id :
	     if first-of(tmprec.cust-id) then summ2014 = tmprec.rate-comm. 
	       else if tmprec.cust-id <> 0 then do:
	           summ2014 = summ2014 + tmprec.rate-comm.
	       end.
	       else
	           summ2014 = tmprec.rate-comm.
    put stream vvs unformatted
		tmprec.cust-id delim
		tmprec.fio delim
		'' delim
		string(tmprec.rate-comm) delim
		'' delim
		string(summ2014) delim
        tmprec.op-date delim
        .
        if summ2014 > 4000 then do:
            put stream vvs unformatted
                string(tmprec.birthday, "99.99.9999") delim
                tmprec.country delim
                tmprec.docum delim
                tmprec.adr delim
                .
        end.
     put stream vvs unformatted eol.
  end.
output stream vvs close.

RUN sndbispc ("file=" + fname + ";class=bq").

PROCEDURE RetAdr:
DEF INPUT  PARAM iAdr AS CHAR NO-UNDO.
DEF OUTPUT PARAM oAdr AS CHAR NO-UNDO.

DEF VAR vOldAdr   AS LOG   NO-UNDO.
DEF VAR vPrefAdr  AS CHAR  NO-UNDO.
DEF VAR vSepAdr   AS CHAR  NO-UNDO.
DEF VAR vFlDot    AS LOG   NO-UNDO INIT FALSE. /* Использовалась ли точка в адре
се */
DEF VAR vCnt      AS INT64 NO-UNDO.
vOldAdr = FGetSetting("ВарПечатАдр","",?) NE "Новый".
IF NOT vOldAdr THEN
DO:
   DO vCnt = 1 TO NUM-ENTRIES(iAdr):
      CASE vCnt:
         WHEN 6 THEN
            vPrefAdr = "д.".
/* sku поменял местами строение и квартиру */
         WHEN 9 THEN
            vPrefAdr = "стр.".
         WHEN 7 THEN
            vPrefAdr = "корп.".
         WHEN 8 THEN
             vPrefAdr = "кв.".
         OTHERWISE
            vPrefAdr = "".
      END CASE.
      /* sku убираем лишний префикс */
      IF LENGTH(vPrefAdr)>0 AND ENTRY(vCnt,iAdr,",") BEGINS vPrefAdr
        THEN vPrefAdr = "".
      IF     NOT vFlDot
         AND vCnt GE 5 THEN
         ASSIGN
            vSepAdr = ". "
            vFlDot  = TRUE.
      ELSE
         vSepAdr = ", ".
      IF ENTRY(vCnt,iAdr,",") NE "" THEN
      DO:
         IF vCnt EQ 1 THEN
            oAdr = ENTRY(vCnt,iAdr,",").
         ELSE
            oAdr = oAdr + vSepAdr + vPrefAdr + ENTRY(vCnt,iAdr,",").
      END.
   END.
END.
else
   oAdr = iAdr.
END PROCEDURE.
