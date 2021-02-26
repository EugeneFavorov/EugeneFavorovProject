/*
               ОАО "Плюс Банк"
    Copyright: 
     Filename: mini.com
      Comment: Данные о состоянии КД на указанную дату
   Parameters: 
         Uses:
      Used by:
      Created: vvv
     Modified: 
*/
{tmprecid.def}

DEF INPUT PARAM iDate AS DATE NO-UNDO.

DEF VAR vK AS INT64 INIT 0 NO-UNDO.

DEF VAR client_name AS CHAR NO-UNDO.
def new shared stream puk.
def var fname as char  init "./SELL_CRD.csv"  no-undo.

DEFINE TEMP-TABLE sell
    FIELD dnumb	 	AS CHAR			/* номер КД */
	FIELD fname		AS CHAR			/* ФИО заемщика */
	FIELD ddate		AS DATE			/* дата */
	FIELD p0		AS INTEGER
	FIELD p7		AS INTEGER
	FIELD p10		AS INTEGER
	FIELD p48		AS INTEGER
	FIELD p109		AS INTEGER
	FIELD p173		AS INTEGER
	FIELD p209		AS INTEGER
	FIELD p210		AS INTEGER
	FIELD p229		AS INTEGER
	FIELD p248		AS INTEGER
	FIELD p301		AS INTEGER
    FIELD sum		AS INTEGER
	.

{empty sell}

{spinner.i "Обработка..."}

/* для каждого выделеннего КД */
FOR EACH tmprecid NO-LOCK,
	FIRST loan
	WHERE RECID(loan) EQ tmprecid.id NO-LOCK :	
	
	/* наименование клиента */
	RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT  client_name).
	
	/* находим данные по состоянию КД */
	FIND FIRST DEBTS
		WHERE DEBTS.CONT_CODE = LOAN.CONT-CODE
		AND DEBTS.DEBT_DATE = iDate 
	NO-LOCK NO-ERROR.
	
	CREATE sell.
	
	ASSIGN
		sell.dnumb = LOAN.DOC-REF
		sell.fname = client_name
		sell.ddate = iDate.
	
	IF AVAIL DEBTS THEN 
		DO:
			/* положим во временную табличку */
			ASSIGN
				sell.p0   	= DEBTS.P0
				sell.p7   	= DEBTS.P7
				sell.p10 	= DEBTS.P10
				sell.p48 	= DEBTS.P48
				sell.p109 	= DEBTS.P109
				sell.p173 	= DEBTS.P173
				sell.p209 	= DEBTS.P209
				sell.p210 	= DEBTS.P210
				sell.p229 	= DEBTS.P229
				sell.p248	= DEBTS.P248
				sell.p301	= DEBTS.P301
				sell.sum 	= DEBTS.DEBT_SUM
				.
			/**/
			vK = vK + 1.
			/**/
		END.
END.

/* вывод */	
/*
run instview.p(TEMP-TABLE sell:HANDLE). 
*/

IF vK > 0 THEN 
	DO:
		/**/
		fname = "./SELL_" + replace(string(TODAY,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

		output stream puk to value (fname)
			UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

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
		  <WindowWidth>27795</WindowWidth>
		  <WindowTopX>480</WindowTopX>
		  <WindowTopY>120</WindowTopY>
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
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		  </Style>
		  <Style ss:ID="s63">
		   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		  </Style>
		  <Style ss:ID="s64">
		   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
		   <Borders>
			<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
		   </Borders>
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		  </Style>
		  <Style ss:ID="s65">
		   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
		   <Borders>
			<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
		   </Borders>
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		  </Style>
		  <Style ss:ID="s66">
		   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
		   <Borders>
			<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
		   </Borders>
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		  </Style>
		  <Style ss:ID="s68">
		   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
		   <Borders>
			<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
			<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
		   </Borders>
		   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
			ss:Color="#000000"/>
		   <NumberFormat ss:Format="Fixed"/>
		  </Style>
		 </Styles>
		 <Worksheet ss:Name="КД">
		  <Table x:FullColumns="1"
		   x:FullRows="1" ss:StyleID="s62">
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="119.25"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="162.75"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="69"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="62"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="72"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="74.25"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="76.5"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="78"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="61.5"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="62.25"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="72.75"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="58.5"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="66.75"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="68.25"/>
		   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="63"/>
		   <Row ss:AutoFitHeight="0" ss:Height="39" ss:StyleID="s63">
			<Cell ss:StyleID="s64"><Data ss:Type="String">№ КД</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Клиент</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Дата</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Сумма к оплате</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Остаток срочной задолженности</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Просроченные заемные средства</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Просроч. задолженность по процентам</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Просроченные %% на внебалансе</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Комиссия за РКО</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Комиссия за выдачу</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Просроченная кмс. за РКО</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Проср. %% на проср.о.д.</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">%% на в/б. за проср. о.д.</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Проср. %% на в/б за проср. о.д.</Data></Cell>
			<Cell ss:StyleID="s64"><Data ss:Type="String">Неупл.Кмс за РКО</Data></Cell>
		   </Row>'.
		/**/
		FOR EACH sell
			BY sell.dnumb:
			
		   put stream puk unformatted '<Row>'.
		   put stream puk unformatted '<Cell ss:StyleID="s65"><Data ss:Type="String">' + STRING(sell.dnumb) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s65"><Data ss:Type="String">' + STRING(sell.fname) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s66"><Data ss:Type="String">' + STRING(sell.ddate, '99.99.9999') + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.sum) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p0) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p7) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p10) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p48) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p109) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p173) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p209) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p210) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p229) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p248) + '</Data></Cell>'.
		   put stream puk unformatted '<Cell ss:StyleID="s68"><Data ss:Type="Number">' + STRING(sell.p301) + '</Data></Cell>'.
		   put stream puk unformatted '</Row>'.
		  
		END.
		/**/

		put stream puk unformatted
			'		</Table>
				</Worksheet>
			</Workbook>'.	



		output stream puk close.
		RUN sndbispc ("file=" + fname + ";class=bq").
		/**/
		END.
ELSE
		DO:
		/**/
			MESSAGE "На дату " + STRING(iDate, "99/99/9999") + " не осуществлен расчет данных!" VIEW-AS ALERT-BOX.
		/**/
		END.	
/* ------------ находим наименование клиента ---------------*/
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
/* ------------ находим наименование клиента ---------------*/
    
















