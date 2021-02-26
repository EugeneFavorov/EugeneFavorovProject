/*
	Проверочный отчет
Данный отчет должен выгружаться по кредитам с доп. реквизитом Priznak - <союз> и содержать информацию:

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{intrface.get comm}

DEFINE TEMP-TABLE otch
        FIELD cont_code    AS CHAR                 /* номер КД */
        FIELD fio          AS CHAR                 /* ФИО заемщика */
        FIELD tel          AS CHAR                 /* телефон заемщика */
        FIELD plan_date    AS DATE                 /* ближайшая плановая дата */
        FIELD rateperc     AS DECIMAL              /* текущая ставка %%*/
        FIELD cred-date    AS CHAR                 /* день гашения од */ 
        FIELD annuit       AS DECIMAL              /* размер аннуитета */
        FIELD acct40817    AS CHAR                 /* номер счета 40817 */ 
        FIELD ost40817     AS DECIMAL              /* остаток 40817 */
        FIELD ostRasch1    AS DECIMAL              /* остаток */
        FIELD ost455       AS DECIMAL              /* остаток 455 */
	FIELD ost458       AS DECIMAL              /* остаток 458 */
	FIELD ost459       AS DECIMAL              /* остаток 458 */
	FIELD ost91604     AS DECIMAL              /* остаток 458 */
	FIELD ost91604Pr   AS DECIMAL              /* остаток 458 */
	FIELD ost91315     AS DECIMAL              /* остаток 91315 */
	FIELD ost91312     AS DECIMAL              /* остаток 91312 */
	FIELD ost913121    AS DECIMAL              /* остаток 91312 */
        FIELD ost47427     AS DECIMAL              /* остаток 47427 */
        FIELD ost90909     AS DECIMAL              /* остаток 90909 */
        FIELD acct47401    AS CHAR                 /* номер счета 47401 */  
        FIELD priznak      AS CHAR                 /* ДР priznak */  
        FIELD sud          AS CHAR                 /* ДР sud */  
        FIELD acct60323    AS CHAR                 /* номер счета 60323 */ 
        FIELD ost60323     AS DECIMAL              /* остаток 60323 */
        INDEX cont_code cont_code       
    .


{empty otch}

def var nameCl as char no-undo.
def var telCl as char no-undo.
def var plan_date as date no-undo.
def var annuit as decimal no-undo.
def var acct40817 as char no-undo.
def var ost40817 as decimal no-undo.
def var ostRasch1 as decimal no-undo.
def var ost455 as decimal no-undo.
def var ost458 as decimal no-undo.
def var ost459 as decimal no-undo.
def var ost91604 as decimal no-undo.
def var ost91604Pr as decimal no-undo.
def var ost91315 as decimal no-undo.
def var ost91312 as decimal no-undo.
def var ost913121 as decimal no-undo.
def var ost47427 as decimal no-undo.
def var ost90909 as decimal no-undo.
def var acct47401 as char no-undo.
def var mcommrate as decimal no-undo.
def var mcred-date as char no-undo.
def var ost60323 as decimal no-undo.
def var acct60323 as char no-undo.



DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 
 {getdate.i}

/* По отмеченным клиентам */
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT nameCl).
	RUN GetTel(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT telCl).
    mcred-date = "".
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

    annuit = 0.
    FIND LAST loan-cond WHERE loan-cond.contract = loan.contract
		AND loan-cond.cont-code = loan.cont-code
		AND loan-cond.since <= end-date NO-LOCK NO-ERROR.
		IF AVAIL loan-cond THEN DO:
			FIND FIRST signs WHERE signs.file-name = 'loan-cond'
				AND signs.code = 'аннуитплат'
				AND signs.surrogate = loan.contract + ',' + loan.cont-code + ',' + STRING(DAY(loan-cond.since),"99") + '/' + STRING(MONTH(loan-cond.since),"99") + '/' + SUBSTRING(STRING(YEAR(loan-cond.since),"9999"),3) 
				NO-LOCK NO-ERROR.
				IF AVAIL signs THEN annuit = signs.dec-value.
                   mcred-date = STRING(loan-cond.cred-date).
		END.

	ost40817 = 0.
	acct40817 = ''.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредРасч'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		acct40817 = loan-acct.acct.
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost40817 = abs(sh-db).
		ELSE ost40817 = abs(sh-db).
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
	
	ost91315 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредВГар'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost91315 = abs(sh-bal).
		ELSE ost91315 = abs(sh-val).
	END.
	
	ost91312 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредОб'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost91312 = abs(sh-bal).
		ELSE ost91312 = abs(sh-val).
	END.

	ost913121 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредОб1'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost913121 = abs(sh-bal).
		ELSE ost913121 = abs(sh-val).
	END.
	
	ost458 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредПр'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost458 = abs(sh-in-bal).
		ELSE ost458 = abs(sh-in-val).
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

	ost90909 = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредРасп'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost90909 = abs(sh-bal).
		ELSE ost90909 = abs(sh-val).
	END.	

	ost60323 = 0.
	acct60323 = ''.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредГПош'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		acct60323 = loan-acct.acct.
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			ost60323 = abs(sh-bal).
		ELSE ost60323 = abs(sh-val).
	END.


        mcommrate = GET_COMM(
                          "%Кред",
                          ?,
                          loan.currency,
                          loan.contract + "," + loan.cont-code,
                          0.00,
                          0,
                          end-date).
	
	CREATE otch.
	ASSIGN
		otch.cont_code 	= TRIM(ENTRY(1,loan.cont-code,"@"))
		otch.fio 		= nameCl
		otch.tel		= telCl
		otch.plan_date 	= plan_date
                otch.rateperc  =  IF mcommrate eq ? THEN 0 ELSE mcommrate
                otch.cred-date  =  mcred-date
		otch.annuit		= annuit
		otch.acct40817  = TRIM(ENTRY(1,acct40817," "))
		otch.ost40817	= ost40817
		otch.ostRasch1	= ostRasch1
		otch.ost455		= ost455
		otch.ost458		= ost458
		otch.ost459		= ost459
		otch.ost91604		= ost91604
		otch.ost91604Pr		= ost91604Pr
		otch.ost91315	= ost91315
		otch.ost91312	= ost91312
		otch.ost913121	= ost913121
		otch.ost47427	= ost47427
		otch.ost90909   = ost90909
		otch.acct47401	= TRIM(ENTRY(1,acct47401," "))
		otch.priznak    = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"priznak","")
		otch.sud        = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"sud","")
		otch.acct60323  = TRIM(ENTRY(1,acct60323," "))
		otch.ost60323	= ost60323
		.
END.

/* выводим в ексель */
    fname = "./otch"  + "_" + USERID('bisquit') + ".xml".
		    output stream vvs to value (fname)
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
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="40"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="40"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="201.25"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
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
    <Cell ss:StyleID="s66"><Data ss:Type="String">День гаш. ОД</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Размер аннуитета</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">счет 40817</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Списание 40817</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">КредРасч1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 455</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 458 утро!!!</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 91315</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 91312</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 91312(1)</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 47427</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 90909</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 459</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">КредПр%В 91604</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">КредТВ 91604</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Счет 47401 (из доп.реквизита)</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Признак ОВ</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Суд</Data></Cell>
	<Cell ss:StyleID="s66"><Data ss:Type="String">КредГПош</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток на 60323</Data></Cell>
   </Row>\n'.

FOR EACH otch NO-LOCK BY otch.cont_code BY cont_code:
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
    PUT STREAM vvs UNFORMATTED otch.cred-date + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.annuit) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct40817 + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost40817) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostRasch1) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost455) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost458) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91315) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91312) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost913121) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost47427) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost90909) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost459) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91604Pr) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91604) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct47401 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.priznak + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.sud + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct60323 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost60323) + '</Data></Cell>\n'.
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
 def var mSignVL as char no-undo.
 def var mSignsF as char no-undo.
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









