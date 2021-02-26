/*
	Проверочный отчет
:

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{intrface.get comm}

DEFINE TEMP-TABLE otch
        FIELD fio          AS CHAR                 /* ФИО заемщика */
        FIELD tel          AS CHAR                 /* телефон заемщика */
        FIELD cont_code    AS CHAR                 /* номер КД */
        FIELD plan_date    AS DATE                 /* ближайшая плановая дата */
        FIELD rateperc     AS DECIMAL              /* текущая ставка %%*/
        FIELD ost40817     AS DECIMAL              /* остаток 40817 */
        FIELD zadolzh      AS DECIMAL              /* задолженность */
        FIELD F_G          AS DECIMAL              /* остаток 40817 - задолженность */
        FIELD dateI        AS DATE                 /* дату последней проводки Дт 90909*7*  Кт 99999 */ 
        FIELD dateJ        AS DATE                 /* dateI + 30*/
        INDEX cont_code cont_code       
    .


{empty otch}
{getdate.i}

DEF VAR nameCl AS CHAR NO-UNDO.
DEF VAR telCl AS CHAR NO-UNDO.
DEF VAR plan_date AS DATE NO-UNDO.
DEF VAR rateperc AS DECIMAL NO-UNDO.
DEF VAR ost40817 AS DECIMAL NO-UNDO.
DEF VAR zadolzh      AS DECIMAL   NO-UNDO.
DEF VAR F_G          AS DECIMAL   NO-UNDO.
DEF VAR dateI        AS DATE      NO-UNDO. 
DEF VAR dateJ        AS DATE      NO-UNDO.

DEF VAR ost455 AS DECIMAL NO-UNDO.
DEF VAR ost458 AS DECIMAL NO-UNDO.
DEF VAR ost459 AS DECIMAL NO-UNDO.
DEF VAR ost47427 AS DECIMAL NO-UNDO.
DEF VAR ost91604 AS DECIMAL NO-UNDO.
DEF VAR ost91604Pr AS DECIMAL NO-UNDO.
DEF VAR ostKredPrOv AS DECIMAL NO-UNDO.
DEF VAR ostKredTOv AS DECIMAL NO-UNDO.
DEF VAR ostKredShtOv AS DECIMAL NO-UNDO.
DEF VAR ostKredPeniSud AS DECIMAL NO-UNDO.
DEF VAR ostKredGPosh AS DECIMAL NO-UNDO.

DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.

/* 
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:
  */
    for each loan where loan.contract = 'Кредит' and loan.close-date = ? no-lock:
        
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредРасп2'
    NO-LOCK NO-ERROR.
  IF AVAIL loan-acct THEN DO:

    RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT nameCl).
	RUN GetTel(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT telCl).
    
    plan_date = DATE("01.01.1970"). 
    /* ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ */
    FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
            term-obl.contract = loan.contract
    	AND term-obl.cont-code = loan.cont-code
    	AND term-obl.idnt = 3
		AND term-obl.end-date <> ?
    	AND term-obl.end-date >= end-date  NO-LOCK
    	BY term-obl.end-date:
			plan_date = term-obl.end-date.
			LEAVE.
    END.
    /* ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ */
    FOR EACH term-obl WHERE /*term-obl.amt-rub <> 0 AND */
            term-obl.contract = 'Кредит'
        AND term-obl.cont-code = loan.cont-code
        AND term-obl.idnt = 1
	AND term-obl.end-date <> ?
    	AND term-obl.end-date >= end-date NO-LOCK
        BY term-obl.end-date:
           IF plan_date EQ DATE("01.01.1970") THEN
              plan_date = term-obl.end-date.
           ELSE
              plan_date = MIN(term-obl.end-date,plan_date).
          LEAVE.
    END.
    
    rateperc = GET_COMM(
                          "%Кред",
                          ?,
                          loan.currency,
                          loan.contract + "," + loan.cont-code,
                          0.00,
                          0,
                          end-date).

	ost40817 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредРасч'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost40817 = abs(sh-bal).
		ELSE ost40817 = abs(sh-val).
	END.

	ost455 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'Кредит'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost455 = abs(sh-bal).
		ELSE ost455 = abs(sh-val).
	END.
	
	ost458 = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредПр'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ost458 = abs(sh-bal).
        ELSE ost458 = abs(sh-val).
    END.
    
    ost459 = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредПр%'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ost459 = abs(sh-bal).
        ELSE ost459 = abs(sh-val).
    END.
    
    ost47427 = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредТ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ost47427 = abs(sh-bal).
        ELSE ost47427 = abs(sh-val).
    END.
    
    ost91604 = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредТВ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ost91604 = abs(sh-bal).
        ELSE ost91604 = abs(sh-val).
    END.
    
    ost91604Pr = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредПр%В'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ost91604Pr = abs(sh-bal).
        ELSE ost91604Pr = abs(sh-val).
    END.
    
    ostKredPrOv = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредПр%ОВ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ostKredPrOv = abs(sh-bal).
        ELSE ostKredPrOv = abs(sh-val).
    END.    
    
    ostKredTOv = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредТОВ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ostKredTOv = abs(sh-bal).
        ELSE ostKredTOv = abs(sh-val).
    END.
    
    ostKredShtOv = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредШт%ОВ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ostKredShtOv = abs(sh-bal).
        ELSE ostKredShtOv = abs(sh-val).
    END.    
    
    ostKredPeniSud = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредПениСуд'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ostKredPeniSud = abs(sh-bal).
        ELSE ostKredPeniSud = abs(sh-val).
    END. 
    
    ostKredGPosh = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредГПош'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            ostKredGPosh = abs(sh-bal).
        ELSE ostKredGPosh = abs(sh-val).
    END.
    
    zadolzh = ost455 + ost458 + ost459 + ost47427 + ost91604 + ost91604Pr + ostKredPrOv + ostKredTOv + ostKredShtOv + ostKredGPosh.
    
    F_G = ost40817 - zadolzh.
    
    dateI = ?.
    dateJ = ?.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредРасп2'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        FOR EACH op-entry WHERE
        (op-entry.op-status = "√" OR op-entry.op-status = "√√") AND
        /* op-entry.op-date >= end-date AND */
        op-entry.acct-db = loan-acct.acct AND
        SUBSTRING(op-entry.acct-cr,1,5) = '99999'
        NO-LOCK BY op-entry.op-date DESC:
        dateI = op-entry.op-date.
        LEAVE.
        END.
    END.   
    
    IF dateI <> ? THEN 
    dateJ = dateI + 30.
/*
	ostRasch1 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредРасч1'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ostRasch1 = abs(sh-bal).
		ELSE ostRasch1 = abs(sh-val).
	END.
*/



	CREATE otch.
	ASSIGN
		otch.fio 		= nameCl
		otch.tel		= telCl
		otch.cont_code  = TRIM(ENTRY(1,loan.cont-code,"@"))
		otch.plan_date 	= plan_date
        otch.rateperc   = IF rateperc EQ ? THEN 0 ELSE rateperc
		otch.ost40817	= ost40817
		otch.zadolzh	= zadolzh
		otch.F_G        = F_G
		otch.dateI	    = dateI
		otch.dateJ      = dateJ
		.
END.
END.
/* выводим в ексель */
    fname = "./otch"  + "_" + USERID('bisquit') + ".xml".
		    OUTPUT stream vvs to value (fname)
        UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

    PUT STREAM vvs UNFORMATTED
'
<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Created>2014-04-24T09:54:07Z</Created>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12075</WindowHeight>
  <WindowWidth>15315</WindowWidth>
  <WindowTopX>360</WindowTopX>
  <WindowTopY>90</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62" ss:Name="qwe">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s65">
    <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s66" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Standard"/>
  </Style>
  <Style ss:ID="s67" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s68">
    <Alignment ss:Horizontal="Right" ss:Vertical="Bottom" ss:WrapText="1"/>
    <NumberFormat ss:Format="0"/>
  </Style>

 </Styles>
 <Worksheet ss:Name="lll">
  <Table >
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
 
   
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО</Data></Cell>\
	<Cell ss:StyleID="s66"><Data ss:Type="String">Телефон</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Плановая дата</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Ставка%%</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 40817</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Задолженность</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">F-G</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Отражено (дата)</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Списать (дата)</Data></Cell>
   </Row>\n'.

FOR EACH otch NO-LOCK BY otch.cont_code:
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.fio + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.tel + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.cont_code + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.plan_date)) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.rateperc) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost40817) + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.zadolzh) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.F_G) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.dateI NE ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.dateI,"99.99.9999"). 
    PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.dateJ NE ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.dateJ,"99.99.9999"). 
    PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '</Row>\n'.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}

PROCEDURE GetTel:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER mSignsV AS CHARACTER.
 DEF VAR mSignVL AS CHAR NO-UNDO.
 DEF VAR mSignsF AS CHAR NO-UNDO.
	mSignVL = "".
	mSignsV = "".
	mSignsF = "".
	IF loan.cust-cat = 'Ч' THEN DO:
		FIND FIRST person WHERE person.person-id = loan.cust-id NO-LOCK NO-ERROR.
		mSignVL = GetXAttrValueEx("person",
                  STRING(person.person-id),
                  "cell-phone",
                  "").
		IF {assigned mSignVL} THEN
		mSignsV = mSignVL.
		ELSE IF  {assigned person.phone[1]}
		AND NUM-ENTRIES(person.phone[1]) GT 1
		AND {assigned ENTRY(2,person.phone[1])} THEN
		mSignsV = ENTRY(2, person.phone[1]).
		ELSE IF {assigned person.phone[1]} THEN
		mSignsV = ENTRY(1, STRING(person.phone[1])).
		IF {assigned person.phone[2]} THEN
		mSignsV = mSignsV + ' ' + ENTRY(1, STRING(person.phone[2])).
		mSignsF = GetXAttrValueEx("person",
                  STRING(person.person-id),
               "fax",
                 "").
		mSignsV = mSignsV + ' ' + mSignsF.  
		mSignsF = GetXAttrValueEx("person",
                  STRING(person.person-id),
               "РаботаТел",
                 "").
		mSignsV = mSignsV + ' ' + mSignsF.  
	END.
	ELSE IF loan.cust-cat = 'Ю' THEN DO:
		mSignsV = GetXAttrValueEx("cust-corp", 
			STRING(cust-corp.cust-id), 
            "tel", 
            "").
		mSignsF = GetXAttrValueEx("cust-corp", 
            STRING(cust-corp.cust-id), 
            "fax", 
            "").
		mSignsV = mSignsV + ' ' + mSignsF. 
	END.
END PROCEDURE.

PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END PROCEDURE.









