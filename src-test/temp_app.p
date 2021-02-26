{globals.i}
{intrface.get tmess}
{tmprecid.def}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: Шаблон АПП
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

DEF VAR client_name	AS CHAR NO-UNDO.
DEF VAR ii AS INTEGER INIT 0 NO-UNDO.
DEF VAR zz AS INTEGER INIT 1 NO-UNDO.
def new shared stream puk.
def var fname as char  init "./PTS.csv"  no-undo.
def var tmp_crd	as char no-undo.

DEFINE TEMP-TABLE app
    FIELD dnumb	 	AS CHAR			/* номер КД */
	FIELD dopen		AS DATE			/* дата открытия КД */
	FIELD fname		AS CHAR			/* ФИО заемщика */
	FIELD cnt		AS INTEGER		/* номер */
	FIELD akk		AS INTEGER 
	FIELD serdoc	AS CHAR			/* серия */
    FIELD numbdoc	AS CHAR			/* номер документа (из Доп. реквизитов) */
	FIELD serbdoc 	AS CHAR			/* серия документа (из Доп. реквизитов) */
	FIELD datedoc 	AS CHAR			/* дата документа */
    .

{empty app}

/* для каждого выделеннего КД */
FOR EACH tmprecid NO-LOCK,
	FIRST loan
	WHERE RECID(loan) EQ tmprecid.id NO-LOCK :	
		ii = 0.
		/* находим обеспечения */
		FOR EACH term-obl
			WHERE term-obl.contract = 'Кредит'
			AND term-obl.cont-code = loan.cont-code
			AND term-obl.idnt = 5
			NO-LOCK:
					
			/**/
			RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT  client_name).

			/* создаем запись */
			CREATE app.
			/**/
			ASSIGN 
				app.dnumb = loan.doc-ref
				app.dopen = loan.open-date
				app.fname = client_name
				app.cnt = ii + 1
				/**/
				app.akk = zz.

			/* номер документа */
			FIND FIRST signs
				WHERE signs.file-name = 'term-obl'
				AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(ii)
				AND signs.code = 'TCNUMB'
			NO-LOCK NO-ERROR.
				/**/
				IF AVAIL signs THEN
					ASSIGN 
						app.numbdoc = signs.xattr-value.
						
			/* серия документа */
			FIND FIRST signs
				WHERE signs.file-name = 'term-obl'
				AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(ii)
				AND signs.code = 'TCSER'
			NO-LOCK NO-ERROR.
				/**/
				IF AVAIL signs THEN
					ASSIGN 
						app.serdoc = signs.xattr-value.				
						
			/* дата документа */
			FIND FIRST signs
				WHERE signs.file-name = 'term-obl'
				AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(ii)
				AND signs.code = 'TCDATE'
			NO-LOCK NO-ERROR.
				/**/
				IF AVAIL signs THEN
					ASSIGN 
						app.datedoc = signs.xattr-value.			
			/**/
			ii = ii + 1.
			zz = zz + 1.
		END.
END.

/* вывод */	
/*
run instview.p(TEMP-TABLE app:HANDLE). 
*/

/* находим наименование клиента */
PROCEDURE GetName:
	DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
	DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
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
END.

fname = "./PTS_" + replace(string(TODAY,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

output stream puk to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
	
IF zz = 2 THEN DO:
	
put stream puk unformatted '<?xml version="1.0"?>
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
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>18195</WindowWidth>
  <WindowTopX>480</WindowTopX>
  <WindowTopY>30</WindowTopY>
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
  <Style ss:ID="m95216736">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="m24982500">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s62">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s64">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s66">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s67">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s69">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s71">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s73">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s81">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s84">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s86">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s87">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
  </Style>
  <Style ss:ID="s88">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
 </Styles>'.
 
 FOR EACH app
	NO-LOCK BY app.dnumb BY app.cnt: 
			/* потом новый лист */
			PUT STREAM puk UNFORMATTED
				' <Worksheet ss:Name="' + STRING(app.dnumb) + '">
				  <Table x:FullColumns="1"
				   x:FullRows="1">
				   <Column ss:AutoFitWidth="0" ss:Width="18.25"/>
				   <Column ss:AutoFitWidth="0" ss:Width="40.25"/>
				   <Column ss:AutoFitWidth="0" ss:Width="101.5"/>
				   <Column ss:AutoFitWidth="0" ss:Width="56.5"/>
				   <Column ss:AutoFitWidth="0" ss:Width="70"/>
				   <Column ss:AutoFitWidth="0" ss:Width="43.5"/>
				   <Column ss:AutoFitWidth="0" ss:Width="51"/>
				   <Column ss:AutoFitWidth="0" ss:Width="57.75"/>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:MergeAcross="4" ss:StyleID="s64"><Data ss:Type="String">Акт</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:MergeAcross="4" ss:StyleID="s66"><Data ss:Type="String">приема-передачи документов</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">по КД №</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s66"><Data ss:Type="String">' + STRING(app.dnumb) + '</Data></Cell>
					<Cell ss:StyleID="s66"><Data ss:Type="String">от</Data></Cell>
					<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(app.dopen, "99.99.9999") + " г." + '</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:MergeAcross="4" ss:StyleID="s66"><Data ss:Type="String">' + STRING(app.fname) + '</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:MergeAcross="4" ss:StyleID="s66"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row ss:AutoFitHeight="0" ss:Height="25.5">
					<Cell ss:StyleID="s69"><Data ss:Type="String">№</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="m24982500"><Data ss:Type="String">Документ</Data></Cell>
					<Cell ss:StyleID="s69"><Data ss:Type="String">Дата документа</Data></Cell>
					<Cell ss:StyleID="s69"><Data ss:Type="String">Номер документа</Data></Cell>
					<Cell ss:StyleID="s69"><Data ss:Type="String">Кол-во листов</Data></Cell>
					<Cell ss:StyleID="s69"><Data ss:Type="String">Оригинал/копия</Data></Cell>
					<Cell ss:StyleID="s69"><Data ss:Type="String">Примечание</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>'.

		PUT STREAM puk UNFORMATTED '<Row>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="Number">' + STRING(app.cnt) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="1" ss:StyleID="m95216736"><Data ss:Type="String">Паспорт транспортного средства</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s73"><Data ss:Type="String">' + STRING(REPLACE(app.datedoc,'/','.')) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + STRING(app.serdoc) + " " + STRING(app.numbdoc) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="Number">1</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">оригинал</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"/>'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.   

END. 

/* подвальчик */ 

	put stream puk unformatted
		   '      <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">Дата ПП:</Data></Cell>
					<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(TODAY, "99.99.9999") + '</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">Передал</Data></Cell>
					<Cell ss:StyleID="s84"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s86"><Data ss:Type="String">Принял</Data></Cell>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">(Специалист )</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:Index="5" ss:StyleID="s67"><Data ss:Type="String">(ОПКД)</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">Дата ПП:</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s67"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>				   
				   
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">Передал</Data></Cell>
					<Cell ss:StyleID="s84"/>
					<Cell ss:Index="5" ss:StyleID="s67"><Data ss:Type="String">Принял</Data></Cell>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s88"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>
				   <Row>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s67"><Data ss:Type="String">(ОПКД)</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:Index="5" ss:StyleID="s67"><Data ss:Type="String">(Архив)</Data></Cell>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
					<Cell ss:StyleID="s62"/>
				   </Row>'.
 
	put stream puk unformatted
'		</Table>
	</Worksheet>
 </Workbook>
'.	
END.
	ELSE
	/* несколько */
DO:

put stream puk unformatted '<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Author>enk</Author>
  <LastAuthor>enk</LastAuthor>
  <Created>2014-05-12T03:37:09Z</Created>
  <LastSaved>2014-05-16T10:08:42Z</LastSaved>
  <Version>11.9999</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>18195</WindowWidth>
  <WindowTopX>480</WindowTopX>
  <WindowTopY>30</WindowTopY>
  <Calculation>ManualCalculation</Calculation>
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
  <Style ss:ID="s21">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s22">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s23">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s24">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s25">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s27">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s28">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s29">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s30">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s31">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s32">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s33">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s34">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s35">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s36">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s37">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s38">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s39">
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
  <Style ss:ID="s41">
   <Alignment ss:Horizontal="Right" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
   <Interior/>
  </Style>
  <Style ss:ID="s42">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="01-00-14266-АП">
  <Table
   x:FullRows="1">
   <Column ss:AutoFitWidth="0" ss:Width="26.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="69"/>
   <Column ss:AutoFitWidth="0" ss:Width="96.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="54.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="78.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="56.25"/>
   <Column ss:Width="50.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="52.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="51"/>
   <Column ss:AutoFitWidth="0" ss:Width="66.75"/>
   <Row>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:Index="5" ss:StyleID="s22"><Data ss:Type="String">Акт</Data></Cell>
    <Cell ss:StyleID="s22"/>
    <Cell ss:StyleID="s22"/>
    <Cell ss:StyleID="s22"/>
    <Cell ss:Index="10" ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
   </Row>
   <Row>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:Index="5" ss:StyleID="s23"><Data ss:Type="String">приема-передачи документов</Data></Cell>
    <Cell ss:StyleID="s23"/>
    <Cell ss:StyleID="s23"><Data ss:Type="String">от</Data></Cell>
    <Cell ss:StyleID="s24"><Data ss:Type="String">' + STRING(today, "99.99.9999") + " г." + '</Data></Cell>
    <Cell ss:Index="10" ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
   </Row>
   <Row>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s25"/>
    <Cell ss:MergeAcross="1" ss:StyleID="s23"/>
    <Cell ss:Index="10" ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
   </Row>
   <Row>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:MergeAcross="4" ss:StyleID="s23"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
   </Row>
   <Row ss:AutoFitHeight="0" ss:Height="25.5">
    <Cell ss:StyleID="s27"><Data ss:Type="String">№</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">№ КД</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">ФИО</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Дата КД</Data></Cell>
    <Cell ss:StyleID="s28"><Data ss:Type="String">Документ</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Дата документа</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Номер документа</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Кол-во листов</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Оригинал/копия</Data></Cell>
    <Cell ss:StyleID="s27"><Data ss:Type="String">Примечание</Data></Cell>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
    <Cell ss:StyleID="s21"/>
   </Row>'.
 

 FOR EACH app
	NO-LOCK BY app.akk:

    /**/
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="38.25">'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="Number">' + STRING(app.akk) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="String">' + STRING(app.dnumb) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="String">' + STRING(app.fname) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="String">' + STRING(app.dopen, "99.99.9999") + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s31"><Data ss:Type="String">Паспорт транспортного средства</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="String">' + STRING(REPLACE(app.datedoc, '/', '.')) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="String">' + STRING(app.serdoc) + " " + STRING(app.numbdoc) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="Number">1</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"><Data ss:Type="String">оригинал</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s29"/>'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.   
		
END. 

/* подвальчик */ 
	put stream puk unformatted
	     '<Row>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s33"/>
			<Cell ss:StyleID="s34"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
			<Cell ss:StyleID="s32"/>
		   </Row>
		   <Row>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
		   </Row>
		   <Row>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s41"><Data ss:Type="String">Передал</Data></Cell>
			<Cell ss:StyleID="s42"/>
			<Cell ss:StyleID="s42"/>
			<Cell ss:Index="5" ss:StyleID="s25"/>
			<Cell ss:StyleID="s35"><Data ss:Type="String">Принял</Data></Cell>
			<Cell ss:StyleID="s36"/>
			<Cell ss:StyleID="s36"/>
			<Cell ss:StyleID="s36"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
		   </Row>
		   <Row>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s25"><Data ss:Type="String">(Специалист )</Data></Cell>
			<Cell ss:StyleID="s37"/>
			<Cell ss:Index="6" ss:StyleID="s25"><Data ss:Type="String">(ОПКД)</Data></Cell>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
		   </Row>
		   <Row>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s25"/>
			<Cell ss:StyleID="s25"/>
			<Cell ss:StyleID="s25"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
			<Cell ss:StyleID="s21"/>
		   </Row>'.

	put stream puk unformatted
'		</Table>
	</Worksheet>
 </Workbook>
'.	
END.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").



