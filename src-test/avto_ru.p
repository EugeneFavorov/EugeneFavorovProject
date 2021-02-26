{tmprecid.def}
{globals.i}
{client.i}

def temp-table avtoz NO-UNDO
  field MODEL 	AS CHAR
  field VIN 	AS CHAR
  field fYEAR	AS CHAR
  field mCOLOR	AS CHAR
  field jBODY 	AS CHAR
  field DVIG	AS CHAR
  field dSTART	AS CHAR
  field dEND	AS CHAR
.

DEF VAR GNI	AS CHAR		NO-UNDO.
DEF VAR vDataCl	AS INT64	NO-UNDO.
DEF VAR str	AS CHAR		NO-UNDO.
DEF VAR nn	AS INT64	NO-UNDO.
DEF VAR iCount	AS INT64	NO-UNDO.
DEF VAR MM AS CHAR NO-UNDO.
DEF VAR VIN AS CHAR NO-UNDO.
DEF VAR BR AS CHAR INIT "" NO-UNDO.

def var fname as char  init "./avto_ru.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def new shared stream vvs.

{getdates.i}
{justasec}

/*-----------------------------------*/
SELECT COUNT(*)
INTO iCount 
FROM signs,
	 loan	
WHERE signs.file-name = "term-obl"
and signs.code = "ВидОб"
and signs.xattr-value = "Автомобиль"
and loan.contract = ENTRY(1, signs.surrogate)
and loan.cont-code = ENTRY(2, signs.surrogate)
and (loan.close-date is null or loan.close-date >= today)
and exists
	(
	select *
	from  term-obl
	where term-obl.contract = loan.contract
	and term-obl.cont-code = loan.cont-code
	and term-obl.idnt = 5
	and term-obl.class-code = "term-obl-gar"
	and term-obl.fop-date >= beg-date
	and term-obl.fop-date <= end-date 
	)
.

 {bar-beg2.i
  &BarTotal     = iCount
  &BarMessage   = "'Поиск залоговых автомобилей...'"
 }
/*-----------------------------------*/

/* идем по ДР договора обеспечения */
FOR EACH SIGNS
	WHERE SIGNS.FILE-NAME = "term-obl"
	AND SIGNS.CODE = "ВидОб"
	AND SIGNS.XATTR-VALUE = "Автомобиль"
	NO-LOCK,
		/* находим КД */
		FIRST LOAN
			WHERE LOAN.CONTRACT = ENTRY(1, SIGNS.SURROGATE)
			AND LOAN.CONT-CODE = ENTRY(2, SIGNS.SURROGATE)
			AND (LOAN.CLOSE-DATE EQ ? OR LOAN.CLOSE-DATE >= TODAY)
		NO-LOCK:

	/* находим договор обеспечения */
	FIND FIRST TERM-OBL
		WHERE TERM-OBL.CONTRACT = LOAN.CONTRACT
		AND TERM-OBL.CONT-CODE = LOAN.CONT-CODE
		AND TERM-OBL.IDNT = 5
		AND TERM-OBL.CLASS-CODE = "term-obl-gar"
		AND TERM-OBL.FOP-DATE >= beg-date
		AND TERM-OBL.FOP-DATE <= end-date 
	NO-LOCK NO-ERROR.
	/**/
		IF AVAIL TERM-OBL THEN
			DO:
				/**/
				nn = nn + 1.
				{bar2.i &BarPointer = nn}
					/**/
					MM = REPLACE(GetXattrValue("term-obl", SIGNS.SURROGATE,"TCmodel"), ",", " ").
					BR = GetXattrValue("term-obl", SIGNS.SURROGATE,"TCbrand").
					VIN = ( IF CAPS( GetXattrValue("term-obl", SIGNS.SURROGATE,"TCVIN")) MATCHES("*ОТСУТ*") THEN "" ELSE GetXattrValue("term-obl", SIGNS.SURROGATE,"TCVIN") ).
					/**/
					IF MM <> "" THEN
						DO:
							CREATE avtoz.
							/**/
							avtoz.VIN = VIN.
							avtoz.DVIG = ( IF CAPS(GetXattrValue("term-obl", surr,"TCmotor")) MATCHES("*ОТСУТ*") THEN "" ELSE GetXattrValue("term-obl", surr,"TCmotor") ).
							avtoz.fYEAR = GetXattrValue("term-obl", surr,"TCyear").
							avtoz.mCOLOR = GetXattrValue("term-obl", surr,"TCcolor").
							avtoz.jBODY = ( IF CAPS( GetXattrValue("term-obl", surr,"TCbody")) MATCHES("*ОТСУТ*") THEN "" ELSE GetXattrValue("term-obl", surr,"TCbody") ).
							avtoz.dSTART = STRING(term-obl.fop-date,"99.99.9999").
							/**/
							/*
							IF BR = "" THEN 
								DO:
									avtoz.MARKA = (IF SUBSTRING(MM, 1, INDEX(MM, " ") - 1) <> "" THEN SUBSTRING(MM, 1, INDEX(MM, " ") - 1) ELSE MM).
									avtoz.MODEL = (IF SUBSTRING(MM, INDEX(MM, " ") + 1) <> "" THEN SUBSTRING(MM, INDEX(MM, " ") + 1) ELSE MM).
								END.
							ELSE
								DO:
									avtoz.MARKA = BR.
									avtoz.MODEL = MM.
								END.
							*/
							IF BR = "" THEN
								avtoz.MODEL = MM.
							ELSE
								avtoz.MODEL = BR + " " + MM.
							
							/**/
							FIND LAST loan-acct
								WHERE loan-acct.contract = loan.contract
								AND loan-acct.cont-code = loan.cont-code
								AND loan-acct.acct-type BEGINS 'КредОб'
								AND loan-acct.acct begins '913'
							NO-LOCK NO-ERROR.
					
								IF AVAIL loan-acct THEN
									DO:
										/**/
										FIND LAST op-entry
											WHERE op-entry.acct-db = loan-acct.acct 
											AND (op-entry.op-status = '√' OR op-entry.op-status = '√√')
										NO-LOCK NO-ERROR.
											/**/
											IF AVAIL op-entry THEN
												avtoz.dEND = string(op-entry.op-date, "99.99.9999").
									END.			
						END.
			END.
END.

{del-bar.i}
   
fname = "./avtozalog_"  + REPLACE(string(today), "/","_") + ".xml".
   
output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
/**/
put stream vvs unformatted '
<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>10005</WindowHeight>
  <WindowWidth>10005</WindowWidth>
  <WindowTopX>120</WindowTopX>
  <WindowTopY>135</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s64">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s66">
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s67">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial" ss:Bold="1"/>
   <Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial" ss:Bold="1"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s72">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial" x:CharSet="204" x:Family="Swiss" ss:Color="#000000"
    ss:Bold="1"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s83">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial" x:CharSet="204" x:Family="Swiss" ss:Color="#000000"
    ss:Bold="1"/>
   <Interior ss:Color="#BFBFBF" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s84">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial" x:CharSet="204" x:Family="Swiss" ss:Color="#000000"
    ss:Bold="1"/>
   <Interior ss:Color="#BFBFBF" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s85">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial" x:CharSet="204" x:Family="Swiss" ss:Color="#000000"
    ss:Bold="1"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="journal">
  <Names>
   <NamedRange ss:Name="_FilterDatabase" ss:RefersTo="=journal!R1C1:R1C8"
    ss:Hidden="1"/>
  </Names>
  <Table x:FullColumns="1" x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="173.25"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="117"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="57"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="170.25"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="117"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="129.75"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="84.75"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="81.75"/>
   <Row ss:AutoFitHeight="0">
    <Cell ss:StyleID="s67"><Data ss:Type="String">model</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">vin</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">year</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">color</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">body</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">dvig</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">avtozalog s</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">avtozalog po</Data><NamedCell
      ss:Name="_FilterDatabase"/></Cell>
   </Row>'.

FOR EACH avtoz NO-LOCK:
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.MODEL) + '</Data></Cell>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.VIN) + '</Data></Cell>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.fYEAR) + '</Data></Cell>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.mCOLOR) + '</Data></Cell>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.jBODY) + '</Data></Cell>
			<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(avtoz.DVIG) + '</Data></Cell>
			<Cell ss:StyleID="s72"><Data ss:Type="String">' + STRING(avtoz.dSTART) + '</Data></Cell>
			<Cell ss:StyleID="s72"><Data ss:Type="String">' + STRING(avtoz.dEND) + '</Data></Cell>
			</Row>
			'.
END.

put stream vvs unformatted
'
</Table>\n
</Worksheet>\n
</Workbook>\n
'.

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").  


/*
output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

put stream vvs unformatted
	"VIN"  delim
	"Марка ТС"  delim
	"Модель ТС" delim
	"В залоге до"
eol.

FOR EACH avtoz
	NO-LOCK:
	
	put stream vvs unformatted
			avtoz.vin delim
			avtoz.MARKA delim	
			avtoz.MODEL delim
			avtoz.DEndZal
			eol.
	END.
	
output stream vvs close.

MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + fname + ";class=bq").
*/
{intrface.del}
