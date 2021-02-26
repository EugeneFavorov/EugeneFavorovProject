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
      Created: vvv
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря"

def new shared stream puk.
def var fname 			as char  init "./branch_rates.csv"  no-undo.
def var delim 			as char init ";" format "x(1)" no-undo.
def var eol 			as char format "x(2)" no-undo.

def var sell 			as char init "БНалПр" no-undo.
def var buy 			as char init "БНалПок" no-undo.
def var spec			as char init "Специальный" no-undo.
DEF VAR mDate 			AS DATE INIT TODAY  FORMAT "99/99/9999" NO-UNDO.  /* дата */


DEF VAR TMP_USER		AS CHAR NO-UNDO.
def var BranchAddress 	as char no-undo.
def var BranchName	 	as char no-undo.

def var i 				as decimal init 1 no-undo.
def var UserName		as char	no-undo.
def var ownr			as char no-undo.

RUN GetUserName(INPUT USERID("bisquit"),INPUT-OUTPUT ownr).

/* для созданных записей */
DEFINE TEMP-TABLE br_rates
    FIELD branch 	AS CHAR		/* подразделение */
	FIELD brname	AS CHAR		/* наименование подразделения */
    FIELD address	AS CHAR		/* адрес подразделения*/
	FIELD set_date	AS DATE		/* дата */
	FIELD who		AS CHAR		/* пользователь */
	/**/
	FIELD buy840	AS DECIMAL	/* покупка 840 */
	FIELD sell840	AS DECIMAL	/* продажа 840 */
	/**/
    FIELD buy978 	AS DECIMAL	/* покупка 978 */
    FIELD sell978	AS DECIMAL	/* продажа 978 */
	/**/
    FIELD buy398 	AS DECIMAL	/* покупка 398 */
    FIELD sell398	AS DECIMAL	/* продажа 398 */
	/**/
    FIELD buy156 	AS DECIMAL	/* покупка 156 */
    FIELD sell156	AS DECIMAL	/* продажа 156 */
	/**/
    .
	
DEF BUFFER ttrates FOR br_rates.
    
{empty br_rates}

/* форма отбора данных */

DEFINE FRAME fGet   
   mDate 			LABEL			"Дата       " 							SKIP
   WITH WIDTH 25 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Параметры Отчета ]" .


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
	
	IF LASTKEY EQ 27 THEN
		RETURN.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.

	/* plus.vvv */
	/* 30/05/2014 */
	/* найдем ДР на пользователе */
	FIND FIRST SIGNS
		WHERE SIGNS.FILE-NAME = "_user"
		AND SIGNS.SURROGATE = CAPS(userid("bisquit"))
		AND SIGNS.CODE = "Отделение"
	NO-LOCK NO-ERROR.
	
	/**/
	IF AVAIL SIGNS THEN
	
	DO:

		/* определим наименование и адрес подразделения */
		RUN GetBranch(INPUT SIGNS.CODE-VALUE,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).	
		/* создадим буферную запись*/
		CREATE ttrates.			
		/* отберем установленные курсы за указанный день */
		FOR EACH instr-rate
			WHERE instr-rate.instr-cat = "currency"
			AND ( instr-rate.rate-type = sell OR instr-rate.rate-type = buy OR instr-rate.rate-type = spec )			
			AND instr-rate.since EQ mDate
			NO-LOCK :
			
			/* если все курсы установлены в одно время */
			/* то все в одну запись */

			/**/
			ASSIGN
				ttrates.branch = SIGNS.CODE-VALUE
				ttrates.brname = BranchName
				ttrates.address = BranchAddress
				ttrates.set_date = mDate
				.	

				/* Продажа */
				IF instr-rate.rate-type = sell THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								ttrates.sell840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								ttrates.sell978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								ttrates.sell398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								ttrates.sell156 = instr-rate.rate-instr.
					END.
					
				/* Покупка */	
				IF instr-rate.rate-type = buy THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								ttrates.buy840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								ttrates.buy978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								ttrates.buy398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								ttrates.buy156 = instr-rate.rate-instr.	
					END.
				/*
				/* Специальный */	
				IF instr-rate.rate-type = spec THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								ttrates.spec840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								ttrates.spec978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								ttrates.spec398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								ttrates.spec156 = instr-rate.rate-instr.	
					END.
				*/	
				/* История CREATE */	
				FIND FIRST HISTORY 
					WHERE HISTORY.FILE-NAME = 'instr-rate'
					AND HISTORY.FIELD-REF = instr-rate.instr-cat + "," + instr-rate.rate-type + "," + instr-rate.instr-code + "," + STRING(instr-rate.since, "99/99/99")
					AND HISTORY.MODIFY = "C"
				NO-LOCK NO-ERROR.
				
				IF AVAIL HISTORY THEN
					DO:
						/* найдем имя пользователя */
						RUN GetUserName(INPUT HISTORY.USER-ID,INPUT-OUTPUT UserName).
						/* пользователь */
						IF ttrates.who <> "" AND INDEX(ttrates.who, UserName) = 0 THEN 
							DO:
								ASSIGN
								ttrates.who = ttrates.who + "," + UserName.
							END.
						ELSE
							DO:
								ASSIGN
								ttrates.who = UserName.
							END.
					END.
		END. /* FOR EACH instr-rate */		
	END. /* IF AVAIL branch */
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
END.

/**/
PROCEDURE GetUserName:

DEF INPUT 			PARAMETER 	USR		AS CHARACTER.
DEF INPUT-OUTPUT 	PARAMETER 	user_name 	AS CHARACTER.

	FIND FIRST _USER
		WHERE _USER._USERID EQ USR
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE _USER THEN
		user_name = STRING(_USER._USER-NAME).
END.


/* вывод */	
/*
run instview.p(TEMP-TABLE br_rates:HANDLE). 
*/

fname = "./branch_rates_" + replace(string(mDATE,"99.99.9999"),".","_") + "_" +  userid('bisquit') + ".xml".	

IF LASTKEY EQ 27 THEN
	RETURN.

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
  <Style ss:ID="m50741696">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="h:mm:ss"/>
  </Style>
  <Style ss:ID="m50741716">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741736">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741512">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741532">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741552">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741572">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741592">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741612">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741632">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m50741652">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s64">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s486">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s499">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s500">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s503">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s504">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="0.0000"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s486" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="89.25"/>
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="85.25"/>
   <Column ss:Index="4" ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="59.25"/>
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="59.25"/>
   <Row ss:AutoFitHeight="0"/>
   <Row ss:AutoFitHeight="0" ss:Height="15.75">
    <Cell ss:MergeAcross="14" ss:StyleID="s63"><Data ss:Type="String">' + STRING(BranchName) + '</Data></Cell>
   </Row>
   
   <Row ss:Index="5" ss:AutoFitHeight="0" ss:Height="15.75">
    <Cell ss:MergeAcross="14" ss:StyleID="s64"><Data ss:Type="String">' + STRING(BranchAddress) + '</Data></Cell>
   </Row>
   <Row ss:AutoFitHeight="0" ss:Height="15.75">
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
    <Cell ss:StyleID="s64"/>
   </Row>
   <Row ss:AutoFitHeight="0">
    <Cell ss:MergeDown="1" ss:StyleID="m50741512"><Data ss:Type="String">Дата</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m50741552"><Data ss:Type="String">№ распоряжения</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741572"><Data ss:Type="String">Доллар США</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741592"><Data ss:Type="String">ЕВРО</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741612"><Data ss:Type="String">Казахстанский тенге</Data></Cell>
	<Cell ss:MergeAcross="1" ss:StyleID="m50741612"><Data ss:Type="String">Китайский юань</Data></Cell>
    <Cell ss:MergeAcross="1" ss:MergeDown="1" ss:StyleID="m50741632"><Data
      ss:Type="String">Установил</Data></Cell>
   </Row>
   <Row ss:AutoFitHeight="0">
    <Cell ss:Index="3" ss:StyleID="s499"><Data ss:Type="String">Покупка</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Продажа</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Покупка</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Продажа</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Покупка</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Продажа</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Покупка</Data></Cell>
    <Cell ss:StyleID="s499"><Data ss:Type="String">Продажа</Data></Cell>	
   </Row>'.

	FOR EACH br_rates
	WHERE br_rates.branch <> ""
	NO-LOCK BY br_rates.set_date:

		PUT STREAM puk UNFORMATTED '<Row>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s500"><Data ss:Type="String">' + STRING(br_rates.set_date, "99.99.9999") + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s503"><Data ss:Type="String">' + STRING(br_rates.set_date, "99/99/9999") + "/" + STRING(i) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell978) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell398) + '</Data></Cell>\n'. 		
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy156) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell156) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="1" ss:StyleID="m50741716"><Data ss:Type="String">' + STRING(br_rates.who) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
		i = i + 1.
	END.

put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

/* конец вывода */		




