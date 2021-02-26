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
        FIELD priznak      AS CHAR
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
DEF VAR ostKredRasp AS DECIMAL NO-UNDO.
DEF VAR priznak as char no-undo.

DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.

/* 
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:
  */
    for each loan where loan.contract = 'Кредит' and loan.close-date = ? 
      and loan.user-id = 'SERVSOUZ'
    no-lock:
        
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредРасп'
    NO-LOCK NO-ERROR.
  IF AVAIL loan-acct THEN DO:

RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
    IF loan-acct.currency = '' THEN
      ostKredRasp = abs(sh-bal).
    ELSE ostKredRasp = abs(sh-val).

if ostKredRasp > 0 then do:

priznak = GetXAttrValue("loan",
              loan.contract + "," + loan.cont-code,
              "priznak").

    RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT nameCl).
	/* RUN GetTel(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT telCl). */
    
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
    
    dateI = ?.
    dateJ = ?.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = 'КредРасп'
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
    
    /* IF dateI <> ? THEN 
    dateJ = dateI + 30. */
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
		otch.dateI	    = dateI
		
    otch.priznak    = priznak
		.
END.
END.
end.
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>

 
   
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО</Data></Cell>\
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Плановая дата</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Допрек priznak</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 40817</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Отражено (дата)</Data></Cell>

   </Row>\n'.

FOR EACH otch NO-LOCK BY otch.cont_code:
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.fio + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.cont_code + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.plan_date)) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.priznak + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost40817) + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.dateI NE ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.dateI,"99.99.9999"). 
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









