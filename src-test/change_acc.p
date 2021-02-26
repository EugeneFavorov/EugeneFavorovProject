{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get xclass}
{pick-val.i}
{refer.i}
/*
Создание отчета по открытым счетам
*/
def new shared stream vvs.
def var fname as char  init "./change_accts.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.

def var my_indx 	as INTEGER no-undo.
def var tmp_br 		as CHARACTER no-undo init ''.
def var iCount 		as INTEGER NO-UNDO.
def var nn			as INT64 INIT 0 NO-UNDO.

def var OPEN_ACC 	as logical view-as toggle-box no-undo.
def var CLOSE_ACC 	as logical view-as toggle-box no-undo.
def var	DEPTS		as char no-undo.
def var DATE_STR	as DATE INIT TODAY NO-UNDO.
def var DATE_END	as DATE INIT TODAY NO-UNDO.

OPEN_ACC = true.
CLOSE_ACC = true.

eol = chr(13) + chr(10).
DEFINE BUFFER b_signs FOR signs.

DEF TEMP-TABLE ACC NO-UNDO
	FIELD TYPE AS INTEGER				/* тип отбора счета: 0 - открыт, 1 - закрыт */
	FIELD ACCT AS CHARACTER				/* Номер счета */
	FIELD CUR AS CHARACTER				/* Валюта */
	FIELD OPEN_DATE AS CHAR				/* Дата открытия */
	FIELD CLOSE_DATE AS CHAR			/* Дата закрытия */
	FIELD GRP AS CHARACTER				/* Группа */
    FIELD CL_NAME AS CHARACTER 			/* ФИО/(Наименование для счетов с маской счета 421,422) */
	FIELD CL_INN AS CHARACTER			/* ИНН */
    FIELD CL_BIRTHDAY AS CHAR	    	/* Дата рождения */
    FIELD CL_BIRTHPLACE AS CHARACTER	/* Место рождения */
	FIELD CL_DOC	AS CHARACTER		/* Код документа, удостоверяющего личность */
	FIELD CL_DOCNUMB AS CHARACTER		/* Серия, № документа, удостоверяющего личность */
	FIELD CL_DOC_OUT AS DATE			/* Дата выдачи документа, удостоверяющего личность */
.	

PAUSE 0.
DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME FTUNE:
  UPDATE
	/*
	DEPTS		VIEW-AS COMBO-BOX LIST-ITEMS '0100','0300','0400'
				LABEL "Подразделение " 	SKIP	
	*/
	OPEN_ACC 	LABEL "Открытые счета" 	SKIP
	CLOSE_ACC 	LABEL "Закрытые счета" 	SKIP
	DATE_STR 	LABEL "Период с "	SKIP
	DATE_END	LABEL "Период по"
	WITH CENTERED ROW 8 OVERLAY SIDE-LABELS
  TITLE "[ Отбор счетов ]".
END.

HIDE FRAME FTUNE NO-PAUSE.

IF LASTKEY EQ KEYCODE("ESC") THEN
	RETURN.
	
IF 	DATE_STR > DATE_END THEN
	DO:
		MESSAGE "Неверный порядок дат!" VIEW-AS ALERT-BOX.
		UNDO, RETRY.	
	END.

{justasec}

/* найдем кол-во записей */	
SELECT COUNT(*)
INTO iCount
FROM ACCT
WHERE ACCT.CUST-CAT = "Ч"
		AND
		(
			( ACCT.OPEN-DATE >= DATE_STR AND ACCT.OPEN-DATE <= DATE_END AND OPEN_ACC )
			OR
			( ACCT.CLOSE-DATE >= DATE_STR AND ACCT.CLOSE-DATE <= DATE_END AND CLOSE_ACC )
		)
		AND (
			LOOKUP(SUBSTRING(ACCT.ACCT, 1, 5), '40817,40820') > 0
			OR 
			LOOKUP(SUBSTRING(ACCT.ACCT, 1, 3), '423,426,421,422') > 0
			)
		.
/**/
{bar-beg2.i
&BarTotal     = iCount
&BarMessage   = "'Поиск счетов...'"
}

/* идем по счетам */
FOR EACH ACCT 
	WHERE ACCT.CUST-CAT = "Ч"
		AND
		(
			( ACCT.OPEN-DATE >= DATE_STR AND ACCT.OPEN-DATE <= DATE_END AND OPEN_ACC )
			OR
			( ACCT.CLOSE-DATE >= DATE_STR AND ACCT.CLOSE-DATE <= DATE_END AND CLOSE_ACC )
		)
		AND (
			LOOKUP(SUBSTRING(ACCT.ACCT, 1, 5), '40817,40820') > 0
			OR 
			LOOKUP(SUBSTRING(ACCT.ACCT, 1, 3), '423,426,421,422') > 0
			)		
	NO-LOCK BY ACCT.ACCT:
	/**/
	CREATE ACC.
	/**/
	{bar2.i &BarPointer = nn}
	nn = nn + 1.
	/* тип отбора счета */
	IF ACCT.OPEN-DATE >= DATE_STR AND ACCT.OPEN-DATE <= DATE_END AND OPEN_ACC THEN
		DO:
			ACC.TYPE = 0.
		END.
	ELSE
		DO:
			ACC.TYPE = 1.
		END.
		
	/* Номер счета */
	ACC.ACCT = ACCT.NUMBER.
	
	/* Дата открытия */
	ACC.OPEN_DATE = STRING(ACCT.OPEN-DATE, '99.99.9999').
	
	/* Дата закрытия */
	ACC.CLOSE_DATE = (IF ACCT.CLOSE-DATE <> ? THEN STRING(ACCT.CLOSE-DATE,'99.99.9999') ELSE "").
	
	/* Группа */
	ACC.GRP = (IF ACCT.CONTRACT <> ? THEN ACCT.CONTRACT ELSE "").
	
	/* Валюта */
	FIND FIRST CURRENCY
		WHERE CURRENCY.CURRENCY = ACCT.CURRENCY
	NO-LOCK NO-ERROR.
		/**/
		IF AVAIL CURRENCY THEN
			ACC.CUR = CURRENCY.I-CURRENCY.
		/**/
	/**/
	FIND FIRST PERSON 
	WHERE PERSON.PERSON-ID = ACCT.CUST-ID
	NO-LOCK NO-ERROR.
		/**/
		IF AVAIL PERSON THEN
			DO:
				/**/
				IF LOOKUP(SUBSTRING(ACCT.ACCT, 1, 3), '421,422') > 0 THEN
					/* наименование счета */
					ACC.CL_NAME = (IF ACCT.DETAILS <> ? THEN ACCT.DETAILS ELSE "").
				ELSE
					/* ФИО клиента */
					ACC.CL_NAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
				/**/
				/* ИНН */
				ACC.CL_INN = (IF PERSON.INN <> ? THEN PERSON.INN ELSE "").
				/* Дата рождения */
				ACC.CL_BIRTHDAY = (IF PERSON.BIRTHDAY <> ? THEN STRING(PERSON.BIRTHDAY, "99.99.9999") ELSE "").
				/* Место рождения */
				ACC.CL_BIRTHPLACE = GetXAttrValueEx ("person", string(person.person-id), "BirthPlace", "").
				/* Код документа */
				ACC.CL_DOC = PERSON.DOCUMENT-ID.
				/* Серия, № документа, удостоверяющего личность */
				ACC.CL_DOCNUMB = PERSON.DOCUMENT.
				/* Дата выдачи документа, удостоверяющего личность */
				FIND FIRST CUST-IDENT
					WHERE CUST-IDENT.CUST-CODE-TYPE = PERSON.DOCUMENT-ID
					AND CUST-IDENT.CUST-CODE = PERSON.DOCUMENT
					AND CUST-IDENT.CUST-CAT = "Ч"
					AND CUST-IDENT.CUST-ID = PERSON.PERSON-ID
				NO-LOCK NO-ERROR.
					/**/
					IF AVAIL CUST-IDENT THEN
						ACC.CL_DOC_OUT = CUST-IDENT.OPEN-DATE.
			END.
END.
/**/
{del-bar.i}
/*
run instview.p(TEMP-TABLE ACC:HANDLE). 	
*/

fname = "./acct_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

/* если есть счета */
IF nn > 0 THEN
	DO:
	
	{init-bar.i "Выгрузка в MS Excel"}
	/* рисуем шапку */
	put stream vvs unformatted '<?xml version="1.0"?>
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
								  <Style ss:ID="s91">
								   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"/>
								  </Style>
								  <Style ss:ID="s110">
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
								  <Style ss:ID="s181">
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								  </Style>
								  <Style ss:ID="s182">
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								   <NumberFormat ss:Format="Short Date"/>
								  </Style>
								  <Style ss:ID="s183">
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								   <NumberFormat ss:Format="Short Date"/>
								  </Style>
								  <Style ss:ID="s184">
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								  </Style>
								  <Style ss:ID="s185">
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								  </Style>
								  <Style ss:ID="s186">
								   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								  </Style>
								  <Style ss:ID="s187">
								   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
								   <Borders>
									<Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
									<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
								   </Borders>
								   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
									ss:Color="#000000"/>
								  </Style>
								 </Styles>'.
						 
	IF CAN-FIND(FIRST ACC WHERE ACC.TYPE = 0) THEN
		DO:
		/* если есть открытые счета */
		put stream vvs unformatted '<Worksheet ss:Name="Открытые счета">
									  <Table x:FullColumns="1"
									   x:FullRows="1" ss:DefaultRowHeight="15">
									   <Column ss:AutoFitWidth="0" ss:Width="127.5"/>
									   <Column ss:AutoFitWidth="0" ss:Width="69.75"/>
									   <Column ss:AutoFitWidth="0" ss:Width="75.75"/>
									   <Column ss:AutoFitWidth="0" ss:Width="79.5"/>
									   <Column ss:AutoFitWidth="0" ss:Width="66.75"/>
									   <Column ss:AutoFitWidth="0" ss:Width="168.75"/>
									   <Column ss:AutoFitWidth="0" ss:Width="84.75"/>
									   <Column ss:AutoFitWidth="0" ss:Width="87"/>
									   <Column ss:AutoFitWidth="0" ss:Width="236.25"/>
									   <Column ss:AutoFitWidth="0" ss:Width="99"/>
									   <Column ss:AutoFitWidth="0" ss:Width="113.25" ss:Span="1"/>
									   <Row ss:AutoFitHeight="0" ss:Height="40.5">
										<Cell ss:StyleID="s91"><Data ss:Type="String">Номер счета</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Валюта счета</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Дата открытия</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Дата закрытия</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Группа</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">ФИО</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">ИНН</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Дата рождения</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Место рождения</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Код документа, удостоверяющего личность</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Серия, № документа, удостоверяющего личность</Data></Cell>
										<Cell ss:StyleID="s91"><Data ss:Type="String">Дата выдачи документа, удостоверяющего личность</Data></Cell>
									   </Row>'.
								
										FOR EACH ACC
											WHERE ACC.TYPE = 0 
											NO-LOCK:
		/*
			FIELD TYPE AS INTEGER				/* тип отбора счета: 0 - открыт, 1 - закрыт */
			FIELD ACCT AS CHARACTER				/* Номер счета */
			FIELD CUR AS CHARACTER				/* Валюта */
			FIELD OPEN_DATE AS DATE				/* Дата открытия */
			FIELD CLOSE_DATE AS DATE			/* Дата закрытия */
			FIELD GRP AS CHARACTER				/* Группа */
			FIELD CL_NAME AS CHARACTER 			/* ФИО/(Наименование для счетов с маской счета 421,422) */
			FIELD CL_INN AS CHARACTER			/* ИНН */
			FIELD CL_BIRTHDAY AS DATE	    	/* Дата рождения */
			FIELD CL_BIRTHPLACE AS CHARACTER	/* Место рождения */
			FIELD CL_DOC	AS CHARACTER		/* Код документа, удостоверяющего личность */
			FIELD CL_DOCNUMB AS CHARACTER		/* Серия, № документа, удостоверяющего личность */
			FIELD CL_DOC_OUT AS DATE			/* Дата выдачи документа, удостоверяющего личность */
		*/											
											put stream vvs unformatted '<Row ss:Height="12.75" ss:StyleID="s185">\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s110"><Data ss:Type="String">' + STRING(ACC.ACCT) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CUR) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.OPEN_DATE) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.CLOSE_DATE) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.GRP) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s186"><Data ss:Type="String">' + STRING(ACC.CL_NAME) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CL_INN) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s183"><Data ss:Type="String">' + STRING(ACC.CL_BIRTHDAY) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s187"><Data ss:Type="String">' + STRING(ACC.CL_BIRTHPLACE) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s184"><Data ss:Type="String">' + STRING(ACC.CL_DOC) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CL_DOCNUMB) + '</Data></Cell>\n'.
											put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.CL_DOC_OUT) + '</Data></Cell>\n'.
											put stream vvs unformatted '</Row>\n'.										
											/**/
										END.
		/* закроем страницу */
		put stream vvs unformatted '  </Table>  </Worksheet>'.

	END.
	
	/* если есть закрытые счета */
	IF CAN-FIND(FIRST ACC WHERE ACC.TYPE = 1) THEN
			DO:
			
			/* если есть открытые счета */
			put stream vvs unformatted '<Worksheet ss:Name="Закрытые счета">
										  <Table x:FullColumns="1"
										   x:FullRows="1" ss:DefaultRowHeight="15">
										   <Column ss:AutoFitWidth="0" ss:Width="127.5"/>
										   <Column ss:AutoFitWidth="0" ss:Width="69.75"/>
										   <Column ss:AutoFitWidth="0" ss:Width="75.75"/>
										   <Column ss:AutoFitWidth="0" ss:Width="79.5"/>
										   <Column ss:AutoFitWidth="0" ss:Width="66.75"/>
										   <Column ss:AutoFitWidth="0" ss:Width="168.75"/>
										   <Column ss:AutoFitWidth="0" ss:Width="84.75"/>
										   <Column ss:AutoFitWidth="0" ss:Width="87"/>
										   <Column ss:AutoFitWidth="0" ss:Width="236.25"/>
										   <Column ss:AutoFitWidth="0" ss:Width="99"/>
										   <Column ss:AutoFitWidth="0" ss:Width="113.25" ss:Span="1"/>
										   <Row ss:AutoFitHeight="0" ss:Height="40.5">
											<Cell ss:StyleID="s91"><Data ss:Type="String">Номер счета</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Валюта счета</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Дата открытия</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Дата закрытия</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Группа</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">ФИО</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">ИНН</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Дата рождения</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Место рождения</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Код документа, удостоверяющего личность</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Серия, № документа, удостоверяющего личность</Data></Cell>
											<Cell ss:StyleID="s91"><Data ss:Type="String">Дата выдачи документа, удостоверяющего личность</Data></Cell>
										   </Row>'.
											/**/
											FOR EACH ACC
												WHERE ACC.TYPE = 1 
												NO-LOCK BY ACC.ACCT:
												/**/
			/*
				FIELD TYPE AS INTEGER				/* тип отбора счета: 0 - открыт, 1 - закрыт */
				FIELD ACCT AS CHARACTER				/* Номер счета */
				FIELD CUR AS CHARACTER				/* Валюта */
				FIELD OPEN_DATE AS DATE				/* Дата открытия */
				FIELD CLOSE_DATE AS DATE			/* Дата закрытия */
				FIELD GRP AS CHARACTER				/* Группа */
				FIELD CL_NAME AS CHARACTER 			/* ФИО/(Наименование для счетов с маской счета 421,422) */
				FIELD CL_INN AS CHARACTER			/* ИНН */
				FIELD CL_BIRTHDAY AS DATE	    	/* Дата рождения */
				FIELD CL_BIRTHPLACE AS CHARACTER	/* Место рождения */
				FIELD CL_DOC	AS CHARACTER		/* Код документа, удостоверяющего личность */
				FIELD CL_DOCNUMB AS CHARACTER		/* Серия, № документа, удостоверяющего личность */
				FIELD CL_DOC_OUT AS DATE			/* Дата выдачи документа, удостоверяющего личность */
			*/											
												put stream vvs unformatted '<Row ss:Height="12.75" ss:StyleID="s185">\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s110"><Data ss:Type="String">' + STRING(ACC.ACCT) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CUR) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.OPEN_DATE) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.CLOSE_DATE) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.GRP) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s186"><Data ss:Type="String">' + STRING(ACC.CL_NAME) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CL_INN) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s183"><Data ss:Type="String">' + STRING(ACC.CL_BIRTHDAY) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s187"><Data ss:Type="String">' + STRING(ACC.CL_BIRTHPLACE) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s184"><Data ss:Type="String">' + STRING(ACC.CL_DOC) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s181"><Data ss:Type="String">' + STRING(ACC.CL_DOCNUMB) + '</Data></Cell>\n'.
												put stream vvs unformatted '<Cell ss:StyleID="s182"><Data ss:Type="String">' + STRING(ACC.CL_DOC_OUT) + '</Data></Cell>\n'.
												put stream vvs unformatted '</Row>\n'.											
												/**/
											END.
			/* закроем страницу */
			put stream vvs unformatted '
										</Table>
										</Worksheet>
										'.
			END.
	{del-bar.i}			

	put stream vvs unformatted ' </Workbook>'.
	END.

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").