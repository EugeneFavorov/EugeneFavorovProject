{globals.i}
{intrface.get tmess}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment:ведомость по задолженности (юрика,физ)
   Parameters:
         Uses:
      Used by:
      Created: kam + vvv
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря"

def new shared stream puk.
def var fname as char  init "./current_rates.csv"  no-undo.

def var sell 	as char init "Продажа" no-undo.
def var buy 	as char init "Покупка" no-undo.
DEF VAR mNumb 	AS CHARACTER NO-UNDO.  /* номер распоряжения */
DEF VAR mDate 	AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.  /* дата */
DEF VAR mTimes 	AS CHARACTER NO-UNDO.  /* время */
DEF VAR QP 		AS DATETIME NO-UNDO.
DEF VAR SF		AS DATETIME NO-UNDO.
DEF VAR CM		AS DATETIME NO-UNDO.
DEF VAR TMP_TIME	AS DATETIME NO-UNDO.
DEF VAR tmp_tt	AS DATETIME NO-UNDO.
DEF VAR TMP_BRANCH		AS CHAR NO-UNDO.
def var BranchAddress as char no-undo.
def var BranchName	 as char no-undo.
def var i as decimal init 1 no-undo.
DEF VAR ii AS decimal init 0 NO-UNDO.
def var fl as decimal init 0 no-undo.
/*
tmp_tt = DATETIME("01-01-2030" + " " + "23:59:59").
*/
/*
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").
*/

DEFINE TEMP-TABLE rates
    FIELD branch 	AS CHAR		/* подразделение */
	FIELD brname	AS CHAR		/* наименование подразделения */
    FIELD address	AS CHAR		/* адрес подразделения*/
	FIELD datet		AS DATETIME /* время установки */
	FIELD buy840	AS DECIMAL	/* покупка 840 */
	FIELD sell840	AS DECIMAL	/* продажа 840 */
    FIELD buy978 	AS DECIMAL	/* покупка 978 */
    FIELD sell978	AS DECIMAL	/* продажа 978*/
    FIELD buy398 	AS DECIMAL	/* покупка 398 */
    FIELD sell398	AS DECIMAL	/* продажа 398 */
    .

DEF BUFFER ttrates FOR rates.
DEF BUFFER ffrates FOR rates.

{empty rates}

/* форма отбора данных */

DEFINE FRAME fGet   
   mDate 			LABEL			"Дата       " 							SKIP
   WITH WIDTH 25 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Отбор данных ]" .

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mDate
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59"). 
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00"). 
	
	/* создадим буферную запись*/
	CREATE ttrates.
					
	/* отберем установленные курсы за весь день */
	FOR EACH irate-time
		WHERE irate-time.iratedatetime <= SF
		AND	irate-time.iratedatetime >= CM
		AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
		NO-LOCK BY irate-time.iratedatetime /*DESCENDING*/ BY irate-time.branch-id :
			
			ii = ii + 1.
			/*---------------------------------------------------------------*/
			/* если произошло изменение подразделения или времени */
			IF TMP_BRANCH <> irate-time.branch-id OR TMP_TIME <> irate-time.iratedatetime THEN
				DO:
					/* и есть что сохранять, т.е. подразделение не пустое*/
					IF TMP_BRANCH <> "" THEN
						DO:
							/* нужно скопировать текущую запись в нужную табличку */
							BUFFER-COPY ttrates TO rates.

							VALIDATE rates NO-ERROR.
							
							RELEASE rates.
						END.
						
					/* если это не первые курсы дня */
					IF (TMP_TIME <> irate-time.iratedatetime AND TMP_TIME <> ?) OR fl = 1 THEN 
						DO:
							fl = 1.
							
							/* нужно найти последние значения курсов ИМЕННО ПО ЭТОМУ ПОДРАЗДЕЛЕНИЮ и скопировать во временную запись*/
							/* тем самым сохраним старые значения по этому подразделению */
							FOR EACH ffrates
								WHERE ffrates.branch EQ irate-time.branch-id
							BY ffrates.datet DESCENDING:
								/**/
								BUFFER-COPY ffrates TO ttrates.
								/**/
								LEAVE.
							END.

						END.
				END.
			/*---------------------------------------------------------------*/
					/* определим наименование и адрес подразделения */

					RUN GetBranch(INPUT irate-time.branch-id,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).
					/**/
					ASSIGN
					ttrates.branch = irate-time.branch-id
					ttrates.brname = BranchName
					ttrates.address = BranchAddress
					ttrates.datet = irate-time.iratedatetime
					.
	
						/* Продажа */
						IF irate-time.rate-type = sell THEN
							DO:
								/* 840*/
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										ttrates.sell840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										ttrates.sell978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										ttrates.sell398 = irate-time.rate-instr.				
							END.
						/* Покупка */	
						IF irate-time.rate-type = buy THEN
							DO:
								/* 840*/
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										ttrates.buy840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										ttrates.buy978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										ttrates.buy398 = irate-time.rate-instr.					
							END.
					/**/

					/*запомним время */
					TMP_TIME = irate-time.iratedatetime.
					/* запомним подразделение */
					TMP_BRANCH = irate-time.branch-id.
	END.
END.


/* находим наименование и адрес подразделения */
PROCEDURE GetBranch:
	DEF INPUT 			PARAMETER 	br 		AS CHARACTER.
	DEF INPUT-OUTPUT 	PARAMETER 	sname 	AS CHARACTER.
	DEF INPUT-OUTPUT 	PARAMETER 	sadres 	AS CHARACTER.
	
	DEF VAR M_INDX1 AS INTEGER INIT 0 NO-UNDO.
	DEF VAR M_SUBSTR AS CHARACTER NO-UNDO.
	DEF VAR M_INDX2 AS INTEGER INIT 0 NO-UNDO.
	
	FIND FIRST branch
		WHERE branch.branch-id = br
	NO-LOCK NO-ERROR.
	
	IF AVAIL branch THEN
		DO:
			sname = branch.name.
			sadres = branch.address.
		END.
	/**/
	M_INDX1 = INDEX(sname, '"').
	M_SUBSTR = SUBSTRING(sname, M_INDX1 + 1).
	M_INDX2 = INDEX(M_SUBSTR, '"').
	
	IF M_INDX1 <> 0 AND M_INDX2 <> 0 THEN
		sname = SUBSTRING(sname, 1, M_INDX1 + M_INDX2).
	/* ДО */
	sname = REPLACE(sname, "Дополнительный офис", "ДО").
	/* Кредитно-кассовый офис */
	sname = REPLACE(sname, "Кредитно-кассовый офис", "ККО").
	
END.



/* вывод */	
/*
run instview.p(TEMP-TABLE rates:HANDLE). 
*/

IF LASTKEY EQ 27 THEN
	RETURN.
	
IF ii > 0 THEN DO:

fname = "./rates_" + replace(string(mDATE,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

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
  <Style ss:ID="m54376960">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m54376980">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s19">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s24">
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
  <Style ss:ID="s29">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s30">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
   <NumberFormat ss:Format="0.0000"/>
  </Style>
  <Style ss:ID="s34">
   <Alignment ss:Vertical="Center"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s36">
   <Alignment ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s37">
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s38">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s40">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s41">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
 </Styles>'.
 /**/
 /**/
 FOR EACH rates
 WHERE rates.branch <> ""
 NO-LOCK BY rates.datet BY rates.branch:
	
	 /* если новая группа - то создадим новый лист */
		IF tmp_tt <> rates.datet THEN DO:
			/* предварительно закроем предыдущий лист*/
			IF tmp_tt <> ? THEN
				DO:
				put stream puk unformatted
				'  <Row ss:Index="40" ss:Height="15">
					<Cell ss:MergeAcross="4" ss:StyleID="s34"><Data ss:Type="String">Начальник отдела операций на валютном и денежном рынках</Data></Cell>
					<Cell ss:StyleID="s29"/>
					<Cell ss:StyleID="s29"/>

				    <Cell ss:MergeAcross="1" ss:StyleID="s36"><Data ss:Type="String">Фукс С.А.</Data></Cell>
				   </Row>
				'.
				/**/
				put stream puk unformatted
				'</Table>
					</Worksheet>'.
					
				END.
			
				/* новый лист */
				mNumb = SUBSTRING(STRING( rates.datet, "99/99/9999 HH:MM:SS.SSS" ), 1, 10) + "/" + STRING(i).
				i = i + 1.
				
				put stream puk unformatted 
				 '<Worksheet ss:Name="Курсы валют ' + REPLACE(SUBSTRING(STRING( rates.datet, "99/99/9999 HH:MM:SS.SSS" ), 12, 8),":", "-") + ' ">
				  <Table ss:ExpandedColumnCount="14" ss:ExpandedRowCount="40" x:FullColumns="1"
				   x:FullRows="1" ss:StyleID="s19">
				   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="82.5"/>
				   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="47.25"/>
				   <Column ss:Index="9" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="60"/>
				   <Column ss:Index="11" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="54.75"/>
				   <Column ss:Index="13" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="55.5"/>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">Публичное акционерное общество &quot;Плюс Банк&quot; &#10;</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">КАЗНАЧЕЙСТВО                                            &#10;</Data></Cell>
				   </Row>
				   <Row ss:Index="4">
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">Р А С П О Р Я Ж Е Н И Е</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">об установлении курсов покупки и курсов продажи наличной иностранной валюты</Data></Cell>
				   </Row>
				   <Row ss:Index="7">
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">№ ' + STRING(mNumb) + ' от ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " года"'</Data></Cell>
				   </Row>
				   <Row ss:Index="9">
					<Cell ss:Index="2" ss:MergeAcross="12" ss:MergeDown="1" ss:StyleID="s37"><Data
					  ss:Type="String">Установить с ' + SUBSTRING(STRING(rates.datet, "99/99/9999 HH:MM:SS.SSS"), 12, 2) + ' часов ' + SUBSTRING(STRING(rates.datet, "99/99/9999 HH:MM:SS.SSS"), 15, 2)  + ' минут ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " года"'                &#10;в кассах банка следующие курсы покупки и продажи наличной иностранной валюты за Российский рубль: &#10;</Data></Cell>
				   </Row>
				   <Row ss:Index="12">
					<Cell ss:MergeAcross="7" ss:MergeDown="1" ss:StyleID="s38"><Data
					  ss:Type="String">Наименование подразделений ПАО &quot;Плюс Банк&quot;</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">Доллар США</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">ЕВРО</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">Казахстанский тенге</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:Index="9" ss:StyleID="s24"><Data ss:Type="String">Покупка</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">Продажа</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">Покупка</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">Продажа</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">Покупка</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">Продажа</Data></Cell>
				   </Row>
				   '.
			/* запомним время */
			tmp_tt = rates.datet.
		END.
		
		ii = 1.
		
		PUT STREAM puk UNFORMATTED '<Row ss:Height="15">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="3" ss:StyleID="m54376960"><Data ss:Type="String">' + STRING(rates.brname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="3" ss:StyleID="m54376980"><Data ss:Type="String">' + STRING(rates.address) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
 END.
	
put stream puk unformatted
'  <Row ss:Index="40" ss:Height="60">
    <Cell ss:MergeAcross="1" ss:StyleID="s34"><Data ss:Type="String">Начальник отдела операций на валютном и денежном рынках</Data></Cell>
    <Cell ss:StyleID="s29"/>
    <Cell ss:StyleID="s29"/>
    <Cell ss:MergeAcross="1" ss:StyleID="s36"><Data ss:Type="String">Фукс С.А.</Data></Cell>
   </Row>
'.


put stream puk unformatted
'		</Table>
	</Worksheet>
 </Workbook>
'.	

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

END.
ELSE
	MESSAGE "Данных за указанный день не найдено!" VIEW-AS ALERT-BOX.

/* конец вывода */		


