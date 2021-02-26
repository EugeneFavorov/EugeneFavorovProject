/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: ved_insur.p
      Comment: Отчет по договорах страхования для Доценко
   Parameters:
         Uses:
      Used by:
      Created: 11/03/13
     Modified: 11/03/13 Serge
*/
{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{intrface.get lngar}
{intrface.get chwch}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get cust}       /* Библиотека для работы с клиентами.  */
{intrface.get strng}      /* Инструменты для работы со строками  */
{client.i}
{lshpr.pro}
{tmprecid.def}
{wordwrap.def}
{svarloan.def}
{navigate.def}
{loan_par.def &new = new} 
{flt-file.i}
{sh-defs.i}

def new shared stream vvs.
def var fname as char  init "./pros_kasko.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

DEF BUFFER bloan   FOR loan.
DEF BUFFER bbloan   FOR loan.
DEF BUFFER bcloan   FOR loan.
DEF BUFFER bperson FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF VAR cname AS CHAR NO-UNDO.
DEF VAR cdetails AS CHAR NO-UNDO.
DEF VAR cont-code AS CHAR NO-UNDO.
DEF VAR doc-ref AS CHAR NO-UNDO.
DEF VAR g AS LOGICAL NO-UNDO.


DEFINE VARIABLE mPhoneHome1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPhoneHome2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPhoneMob AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPhoneWork AS CHARACTER NO-UNDO.
DEFINE VARIABLE sName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPriznak     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mcommrate    AS DECIMAL NO-UNDO.
DEFINE VARIABLE mdatadog    AS DATE NO-UNDO.

                
DEFINE TEMP-TABLE Kasko
	FIELD cont-code AS CHARACTER
	FIELD doc-ref   AS CHARACTER
	FIELD cust-id   AS INTEGER
	FIELD open-date AS DATE
	FIELD close-date  AS DATE
	FIELD end-date  AS DATE
	FIELD cdetails  AS CHARACTER
	FIELD cname     AS CHARACTER
	FIELD cellphone AS CHARACTER
	FIELD zname     AS CHARACTER
	FIELD priznak   AS CHARACTER
	FIELD commrate  AS DECIMAL
	FIELD datasogl  AS DATE
.


{getdates.i}

{setdest.i &col=170 }
PUT UNFORMATTED "Нажмите ESC для выгрузки отчета в BisPC" skip(1).

FOR EACH bloan WHERE bloan.end-date GE end-date
AND (bloan.close-date GE end-date OR bloan.close-date = ?)
AND bloan.cont-code MATCHES '*ПК*'
AND bloan.filial-id eq shFilial
 NO-LOCK:
	{spinner.i "Секундочку ... " }	
	cont-code = bloan.cont-code.

    For each bbloan where bbloan.parent-cont-code eq cont-code and bbloan.parent-contract eq bloan.contract
	and bbloan.filial-id eq shFilial
	 break by bbloan.end-date DESCENDING:
		IF CAN-DO("*ПК*", bbloan.parent-cont-code) THEN
			IF bbloan.open-date LT DATE(02,20,2013) OR GetXAttrValueEx("loan", STRING("СТРАХ," + bbloan.cont-code), "VidStr", "") begins "КАСКО" THEN /* ориентировочно до 20.02.2013 г. все договора автострахования в банке были по умолчанию КАСКО, доп. реквизиты у них никто заполнять не собирается, поэтому так */
				IF bbloan.end-date GE beg-date
				AND bbloan.end-date LE end-date THEN DO:
					IF bbloan.end-date GE beg-date
					AND bbloan.end-date LE end-date THEN DO:

    RUN GetName(INPUT bloan.cust-cat,INPUT bloan.cust-id,OUTPUT sName).
    mPriznak = GetXattrValueEx("loan",bloan.contract + "," + bloan.cont-code,"priznak","").
    mcommrate = GET_COMM(
                          "%Кред",
                          ?,
                          bloan.currency,
                          bloan.contract + "," + bloan.cont-code,
                          0.00,
                          0,
                          end-date).
    mdatadog = DATE(GetXAttrValueEx("loan", 
                                      bloan.contract + "," + bloan.cont-code, 
                                      "ДатаСогл",
                                      "")).


						IF bbloan.cust-cat EQ 'Ч' THEN DO:
							FIND FIRST bperson WHERE bperson.person-id EQ bbloan.cust-id NO-LOCK.
							cname = bperson.name-last + ' ' + bperson.first-names.
							cdetails = 'Адрес: ' + bperson.address[1] + '; Телефон: ' + bperson.phone[1] + ' ' + bperson.phone[2].
						END.
						IF bbloan.cust-cat EQ 'Ю' THEN DO:
							FIND FIRST bcust-corp WHERE bcust-corp.cust-id EQ bbloan.cust-id NO-LOCK.
							cname = bcust-corp.cust-stat + ' ' + bcust-corp.name-corp.
							cdetails = 'Адрес: ' + bcust-corp.addr-of-low[1] + '; Факс: ' + bcust-corp.fax.
						END.

						CREATE Kasko.
						ASSIGN
							kasko.cont-code = bbloan.parent-cont-code
							kasko.cname = cname
							kasko.cdetails = cdetails
							kasko.doc-ref = bbloan.cont-code
							kasko.close-date  = bbloan.end-date
							kasko.open-date = bbloan.open-date
							kasko.zname = sName
							kasko.Priznak = mPriznak
							kasko.commrate = mcommrate
							kasko.cellphone = "'" + mPhoneMob
							kasko.datasogl = mdatadog
						.
						g = true.
						LEAVE.
					END.
				END.
    End.
END.

IF g THEN DO:
	PUT UNFORMATTED
	  "┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐" SKIP
	  "│                                                                                 Ведомость по договорам КАСКО                                                                                │" SKIP
	  "├──────────────────────┬─────────────────────────────────────────────┬──────────────────────┬──────────┬──────────┬───────────────────────────────────────────────────────────────────────────┤" SKIP
	  "│№ Кредитного договора │                 Заемщик                     │№ Договора страхования│Оконч. ДС │Открыт. ДС│                            Контакты заемщика                              │" SKIP
	  "├──────────────────────┼─────────────────────────────────────────────┼──────────────────────┼──────────┼──────────┼───────────────────────────────────────────────────────────────────────────┤" SKIP.
	output stream vvs to value (fname)
	UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
	put stream vvs unformatted
		"Ведомость по договорам КАСКО" eol.
	put stream vvs unformatted
		"№ КД" delim
		"Дата закл." delim
		"Заемщик" delim
		"Страховая" delim
		"№ ДС" delim	
		"Дата окончания ДС" delim
		"Дата открытия ДС" delim
		"Сотовый  заемщика" delim
		"Признак" delim
		"Ставка %" delim
		"Контакты заемщика" delim
		eol.

	for each Kasko no-lock by kasko.end-date:
		PUT UNFORMATTED
			"│"  STRING(kasko.cont-code, "x(22)")
			"│"  STRING(kasko.cname, "x(45)")
			"│"  STRING(kasko.doc-ref, "x(22)")
			"│"  STRING(kasko.close-date, "99/99/9999")
			"│"  STRING(kasko.open-date, "99/99/9999")
			"│"  STRING(kasko.cdetails, "x(75)") "│" skip.
		put stream vvs unformatted
			kasko.cont-code delim
                        kasko.datasogl  delim
			kasko.zname delim
			kasko.cname delim
			kasko.doc-ref delim
			kasko.close-date delim	
			kasko.open-date delim		
			kasko.cellphone  delim
			kasko.priznak delim
                        Replace(STRING(kasko.commrate),".",",") delim
			kasko.cdetails delim
			eol.
	end.
	output stream vvs close.
	PUT UNFORMATTED
	  "└──────────────────────┴─────────────────────────────────────────────┴──────────────────────┴──────────┴──────────┴───────────────────────────────────────────────────────────────────────────┘" SKIP.
	{preview.i &col=170}
	MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.
	RUN sndbispc ("file=" + fname + ";class=bq").
END.
ELSE MESSAGE "Данных не найдено" VIEW-AS ALERT-BOX.

{intrface.del}

PROCEDURE GetName:
   DEFINE INPUT PARAMETER cat AS CHARACTER.
   DEFINE INPUT PARAMETER id AS INT64.
   DEFINE OUTPUT PARAMETER sname AS CHARACTER.
 
   IF cat = "Ч" THEN
   DO:
      FIND FIRST PERSON 
         WHERE PERSON.PERSON-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE PERSON THEN
      DO:
         /* ФИО клиента */
         ASSIGN
         sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES
         mPhoneMob  = GetEntries(2, person.Phone[2], ",", "").
      END.
      ELSE 
        mPhoneMob  = "".
   END.
   ELSE
   DO:
      FIND FIRST CUST-CORP 
         WHERE CUST-CORP.CUST-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE CUST-CORP THEN
         /* наименование организации */
         sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
   END.
END PROCEDURE.

