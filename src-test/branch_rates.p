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

def var sell 			as char init "Продажа" no-undo.
def var buy 			as char init "Покупка" no-undo.
DEF VAR mDate 			AS DATE INIT TODAY  FORMAT "99/99/9999" NO-UNDO.  /* дата */
DEF VAR SF				AS DATETIME NO-UNDO.
DEF VAR CM				AS DATETIME NO-UNDO.
DEF VAR TMP_TIME		AS DATETIME NO-UNDO.
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
	FIELD set_date	AS DATETIME /* дата */
	FIELD who		AS CHAR		/* пользователь */
	FIELD buy840	AS DECIMAL	/* покупка 840 */
	FIELD sell840	AS DECIMAL	/* продажа 840 */
    FIELD buy978 	AS DECIMAL	/* покупка 978 */
    FIELD sell978	AS DECIMAL	/* продажа 978*/
    FIELD buy398 	AS DECIMAL	/* покупка 398 */
    FIELD sell398	AS DECIMAL	/* продажа 398 */
    .
	
DEF BUFFER ttrates FOR br_rates.
    
{empty br_rates}

/* форма отбора данных */

DEFINE FRAME fGet   
   mDate 			LABEL			"Дата       " 							SKIP
   WITH WIDTH 25 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Параметры Отчета ]" .
/*
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
	/* время дня */
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59"). 
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00"). 

			/* найдем запись */
			FIND FIRST branch
				WHERE /*RECID(branch) EQ tmprecid.id*/ branch.branch-id EQ shfilial
			NO-LOCK NO-ERROR.
			
			/* определим наименование и адрес подразделения */
			RUN GetBranch(INPUT branch.branch-id,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).
			
			TMP_TIME = ?.
			
			IF AVAIL branch THEN DO:				
				/* отберем установленные курсы за указанный день в определенном подразделении */
				FOR EACH irate-time
					WHERE irate-time.branch-id = branch.branch-id
					AND irate-time.iratedatetime <= SF
					AND	irate-time.iratedatetime >= CM
					AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
					NO-LOCK BY irate-time.iratedatetime /*DESCENDING*/ :
					
					
					/* если все курсы установлены в одно время */
					/* то все в одну запись */
					IF TMP_TIME <> irate-time.iratedatetime THEN
						DO:
							IF TMP_TIME <> ? THEN
								/* сохраним если есть что */
								VALIDATE br_rates.
								/* и создадим новую запись */
								CREATE br_rates.
								/**/
								ASSIGN
								br_rates.branch = irate-time.branch-id
								br_rates.brname = BranchName
								br_rates.address = BranchAddress
								br_rates.set_date = irate-time.iratedatetime
								.	
								/*запомним время */
								TMP_TIME = irate-time.iratedatetime.
						END.

						/* Продажа */
						IF irate-time.rate-type = sell THEN
							DO:
								/* 840 */
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										br_rates.sell840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										br_rates.sell978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										br_rates.sell398 = irate-time.rate-instr.				
							END.
						/* Покупка */	
						IF irate-time.rate-type = buy THEN
							DO:
								/* 840 */
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										br_rates.buy840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										br_rates.buy978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										br_rates.buy398 = irate-time.rate-instr.					
							END.
						/* История CREATE */	
						FIND FIRST HISTORY 
							WHERE HISTORY.FILE-NAME = 'irate-time'
							AND HISTORY.FIELD-REF = STRING(irate-time.irate-id)
							AND HISTORY.MODIFY = "C"
						NO-LOCK NO-ERROR.
						
						IF AVAIL HISTORY THEN
							DO:
								/* найдем имя пользователя */
								RUN GetUserName(INPUT HISTORY.USER-ID,INPUT-OUTPUT UserName).
								/* пользователь */
								IF br_rates.who <> "" AND INDEX(br_rates.who, UserName) = 0 THEN 
									DO:
										ASSIGN
										br_rates.who = br_rates.who + "," + UserName.
									END.
								ELSE
									DO:
										ASSIGN
										br_rates.who = UserName.
									END.
								/* реальное время */
									ASSIGN
									br_rates.real_time = ADD-INTERVAL(DATETIME(STRING(HISTORY.MODIF-DATE, "99/99/9999") + " " + STRING(HISTORY.MODIF-TIME, "HH:MM:SS")), 3, "hours").
							END.
						/**/
						/**/										
				END. /* FOR EACH irate-time */
			END. /* IF AVAIL branch */
END.
*/

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
	/* время дня */
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59"). 
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00"). 

	/* найдем запись */
	/*
	FIND FIRST branch
		WHERE /*RECID(branch) EQ tmprecid.id*/ branch.branch-id EQ shfilial
	NO-LOCK NO-ERROR.
	*/
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
	
	TMP_TIME = ?.
	/*	
	IF AVAIL branch THEN
	*/
	
	DO:

		/* определим наименование и адрес подразделения */
		RUN GetBranch(INPUT SIGNS.CODE-VALUE,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).	
		/* создадим буферную запись*/
		CREATE ttrates.			
		/* отберем установленные курсы за указанный день в определенном подразделении */
		FOR EACH irate-time
			WHERE irate-time.branch-id = SIGNS.CODE-VALUE
			AND irate-time.iratedatetime <= SF
			AND	irate-time.iratedatetime >= CM
			AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
			NO-LOCK BY irate-time.iratedatetime /*DESCENDING*/ :
			
			/* если все курсы установлены в одно время */
			/* то все в одну запись */
			IF TMP_TIME <> irate-time.iratedatetime THEN
				DO:
					IF TMP_TIME <> ? THEN
					
						/* нужно скопировать текущую запись в нужную табличку */
						BUFFER-COPY ttrates TO br_rates.

						VALIDATE br_rates NO-ERROR.
						
						RELEASE br_rates.
					
						/**/
						ASSIGN
						ttrates.branch = irate-time.branch-id
						ttrates.brname = BranchName
						ttrates.address = BranchAddress
						ttrates.set_date = irate-time.iratedatetime
						.	
						/*запомним время */
						TMP_TIME = irate-time.iratedatetime.
				END.

				/* Продажа */
				IF irate-time.rate-type = sell THEN
					DO:
						/* 840 */
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
						/* 840 */
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
				/* История CREATE */	
				FIND FIRST HISTORY 
					WHERE HISTORY.FILE-NAME = 'irate-time'
					AND HISTORY.FIELD-REF = STRING(irate-time.irate-id)
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
		END. /* FOR EACH irate-time */		
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
  <Author>Васильев Виктор Владимирович</Author>
  <LastAuthor>Васильев Виктор Владимирович</LastAuthor>
  <Created>2014-04-28T08:09:09Z</Created>
  <LastSaved>2014-04-28T08:24:35Z</LastSaved>
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
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1" />
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
  <Table ss:ExpandedColumnCount="15" x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s486" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="69"/>
   <Column ss:Index="3" ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="75"/>
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="73.5"/>
   <Column ss:Index="9" ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="59.25"/>
   <Column ss:StyleID="s486" ss:AutoFitWidth="0" ss:Width="56.25"/>
   <Row ss:AutoFitHeight="0"/>
   <Row ss:AutoFitHeight="0" ss:Height="15.75">
    <Cell ss:MergeAcross="14" ss:StyleID="s63"><Data ss:Type="String">' + STRING(BranchName) + '</Data></Cell>
   </Row>
   <Row ss:Index="4" ss:AutoFitHeight="0" ss:Height="15.75">
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
    <Cell ss:MergeAcross="1" ss:MergeDown="1" ss:StyleID="m50741532"><Data
      ss:Type="String">Время начала действия ' + (IF CAN-DO('0516,0517', signs.code-value) THEN '(время Омское)' ELSE '') + '</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m50741552"><Data ss:Type="String">№ распоряжения</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741572"><Data ss:Type="String">Доллар США</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741592"><Data ss:Type="String">ЕВРО</Data></Cell>
    <Cell ss:MergeAcross="1" ss:StyleID="m50741612"><Data ss:Type="String">Казахстанский тенге</Data></Cell>
    <Cell ss:MergeAcross="1" ss:MergeDown="1" ss:StyleID="m50741632"><Data
      ss:Type="String">Установил</Data></Cell>
    <Cell ss:MergeAcross="1" ss:MergeDown="1" ss:StyleID="m50741652"><Data
      ss:Type="String">Кассир</Data></Cell>
   </Row>
   <Row ss:AutoFitHeight="0">
    <Cell ss:Index="5" ss:StyleID="s499"><Data ss:Type="String">Покупка</Data></Cell>
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
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s500"><Data ss:Type="String">' + SUBSTRING(STRING(br_rates.set_date, "99.99.9999" + "T" + "HH:MM:SS.SSS"), 1, 10) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="1" ss:StyleID="m50741696"><Data ss:Type="String">' + SUBSTRING(STRING(br_rates.set_date, "99-99-9999" + "T" + "HH:MM:SS.SSS"), 12,8) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s503"><Data ss:Type="String">' + SUBSTRING(STRING(br_rates.set_date, "99/99/9999" + "T" + "HH:MM:SS.SSS"),1,10) + "/" + STRING(i) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.buy398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s504"><Data ss:Type="Number">' + STRING(br_rates.sell398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="1" ss:StyleID="m50741716"><Data ss:Type="String">' + STRING(br_rates.who) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="1" ss:StyleID="m50741736"><Data ss:Type="String">' + STRING(ownr) + '</Data></Cell>\n'. 
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




