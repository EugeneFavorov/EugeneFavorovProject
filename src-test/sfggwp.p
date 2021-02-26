{globals.i}
/*
Для печати данных по страховым компаниям
*/
def new shared stream puk.
def var fname as char  init "./sfggwp.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
def var my_indx as INTEGER no-undo.
def var tmp_br as CHARACTER no-undo init ''.
def var OPEN_D as logical view-as toggle-box no-undo.
def var CLOSE_D as logical view-as toggle-box no-undo.

OPEN_D = true.
CLOSE_D = true.

eol = chr(13) + chr(10).
DEFINE BUFFER b_signs FOR signs.

def temp-table str NO-UNDO
	FIELD TYPE AS INTEGER				/* Тип доверенности 0 - простая, 1 - доверенность РКО */
	FIELD BRANCH AS CHARACTER			/* Подразделение */
	FIELD PR_BEGIN AS CHARACTER			/* Начало действия */
	FIELD PR_END AS CHARACTER			/* Окончание действия */
	FIELD FIO_PRINCIPAL AS CHARACTER	/* ФИО доверителя */
	FIELD FIO_AGENT	AS CHARACTER		/* ФИО представителя */
    FIELD DATE_OPEN AS CHARACTER 		/* Дата открытия */
	FIELD FIO_OPEN AS CHARACTER			/* ФИО сотрудника, который открыл */
    FIELD DATE_CLOSE AS CHARACTER	    /* Дата закрытия */
    FIELD FIO_CLOSE AS CHARACTER		/* ФИО сотрудника, который закрыл */
.	

pause 0.
Do on error undo, leave on endkey undo, leave with frame ftune:
  Update
	OPEN_D label "Открытые доверенности" 
	CLOSE_D label "Закрытые доверенности"
  with centered row 10 overlay side-labels 1 col
  title "[ Параметры Отчета ]".
End.
Hide frame ftune no-pause.

if LASTKEY EQ KEYCODE("ESC") THEN
	return.
	
/* идем по ДОВЕРЕННОСТЯМ */
FOR EACH LOAN 
WHERE LOAN.CONTRACT EQ "proxy" NO-LOCK,
	FIRST SIGNS 
	WHERE LOAN.CONTRACT + "," + LOAN.CONT-CODE EQ SIGNS.SURROGATE
	AND SIGNS.FILE-NAME EQ "loan"
	AND SIGNS.CODE EQ "drower-id"
	/*AND SIGNS.DEC-VALUE EQ 8128*/
NO-LOCK BY LOAN.BRANCH-ID:

IF AVAIL LOAN THEN DO:	
	/* создадим строку */
	CREATE str.
	/* ТипиТип */
	str.TYPE = 0.
	/* Код филиала*/
	str.BRANCH = STRING( LOAN.BRANCH-ID ).
	/* Начало действия */
	str.PR_BEGIN = STRING( LOAN.OPEN-DATE, "99.99.9999" ).
	/* Окончание действия */
	IF LOAN.LOAN-STATUS = "ЗАКР" THEN
		str.PR_END = STRING( LOAN.CLOSE-DATE, "99.99.9999").
	ELSE
		str.PR_END = STRING( LOAN.END-DATE, "99.99.9999" ).
	/* -- */
	IF str.PR_END EQ ? THEN
		str.PR_END = STRING( LOAN.END-DATE, "99.99.9999" ).
	/* -- */	
	IF LOAN.CUST-CAT = "Ч" THEN
		/* ФИО доверителя */
		FIND FIRST PERSON 
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		NO-LOCK NO-ERROR.
			IF AVAIL PERSON THEN
			/* заполним ФИО доверителя */
			str.FIO_PRINCIPAL = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
	ELSE
		/* ФИО доверителя */
		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = LOAN.CUST-ID
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
			/* заполним ФИО доверителя */
			str.FIO_PRINCIPAL = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		
	/* ФИО представителя */
	/* из Доп реков LOAN */
	FIND FIRST b_signs
	WHERE b_signs.CODE EQ "agent-id"
	AND b_signs.FILE-NAME EQ "loan"
	AND b_signs.SURROGATE EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
	NO-LOCK NO-ERROR.
		IF AVAIL b_signs THEN DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = b_signs.DEC-VALUE
			NO-LOCK NO-ERROR.
			/* заполним ФИО представителя */
			IF AVAIL PERSON THEN 
				str.FIO_AGENT = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
			
	/* пойдем в историю */	
	/* кто и когда открыл */	
	FOR EACH HISTORY
	WHERE HISTORY.FILE-NAME EQ "LOAN"
	AND	HISTORY.FIELD-REF EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
	/*AND	HISTORY.MODIFY EQ "С"*/
	NO-LOCK BY HISTORY.MODIF-DATE:
		/* не нужно ничего парсить!!!!
		/* надо как-то распарсить дату начала действия доверенности */
		my_indx = INDEX(HISTORY.FIELD-VALUE, "create-date").
		IF my_indx > 0 THEN DO:
			str.DATE_OPEN = ENTRY( 2, SUBSTRING( HISTORY.FIELD-VALUE, my_indx )).
			LEAVE.
		END.
		*/
		str.DATE_OPEN = STRING( HISTORY.MODIF-DATE, "99.99.9999").
		LEAVE.
	END.
	/* найти ФИО пользователя, который открыл */
	
	FIND FIRST _USER
	WHERE _USER._USERID EQ HISTORY.USER-ID
	NO-LOCK NO-ERROR.
	IF AVAILABLE _USER THEN
		str.FIO_OPEN = STRING(_USER._USER-NAME).
	/* если договор закрыт - то ищем дату и кто закрыл */
	IF LOAN.LOAN-STATUS = "ЗАКР" THEN DO:
	/* смотрим последнее изменение статуса */
	
		FOR EACH HISTORY
		WHERE HISTORY.FILE-NAME EQ "LOAN"
		AND	HISTORY.FIELD-REF EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
		AND	HISTORY.MODIFY EQ "W"
		NO-LOCK BY HISTORY.MODIF-DATE DESCENDING:
			/*
			MESSAGE HISTORY.USER-ID VIEW-AS ALERT-BOX.
			*/
			IF INDEX(HISTORY.FIELD-VALUE, "loan-status") > 0 THEN DO:
				str.DATE_CLOSE = STRING( HISTORY.MODIF-DATE, "99.99.9999").
				LEAVE.
			END.
		END.	

		/* найти ФИО пользователя, который закрыл */
		
		FIND FIRST _USER
		WHERE _USER._USERID EQ HISTORY.USER-ID
		NO-LOCK NO-ERROR.
		IF AVAILABLE _USER THEN
			str.FIO_CLOSE = _USER._USER-NAME.
	END.		
END.	
END.

/* нужно добавить ДОВЕРЕННОСТИ РКО */
FOR EACH LOAN
	WHERE LOAN.CONTRACT EQ "proxy"
	AND	LOAN.CLASS-CODE EQ "proxy-base"
	NO-LOCK BY LOAN.OPEN-DATE:
IF AVAIL LOAN THEN DO:	
	/* создадим строку */
	CREATE str.
	/* ТипиТип */
	str.TYPE = 1.
	/* Код филиала*/
	str.BRANCH = STRING( LOAN.BRANCH-ID ).
	/* Начало действия */
	str.PR_BEGIN = STRING( LOAN.OPEN-DATE, "99.99.9999" ).
	/* Окончание действия */
	IF LOAN.LOAN-STATUS = "ЗАКР" THEN
		str.PR_END = STRING( LOAN.CLOSE-DATE, "99.99.9999").
	ELSE
		str.PR_END = STRING( LOAN.END-DATE, "99.99.9999" ).
	/* -- */
	IF str.PR_END EQ ? THEN
		str.PR_END = STRING( LOAN.END-DATE, "99.99.9999" ).
	/* -- */	
	IF LOAN.CUST-CAT = "Ч" THEN
		/* ФИО доверителя */
		FIND FIRST PERSON 
		WHERE PERSON.PERSON-ID = LOAN.CUST-ID
		NO-LOCK NO-ERROR.
			IF AVAIL PERSON THEN
			/* заполним ФИО доверителя */
			str.FIO_PRINCIPAL = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
	ELSE
		/* ФИО доверителя */
		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = LOAN.CUST-ID
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
			/* заполним ФИО доверителя */
			str.FIO_PRINCIPAL = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.		
	.
		
	/* ФИО представителя */
	/* из Доп реков LOAN */
	FIND FIRST b_signs
	WHERE b_signs.CODE EQ "agent-id"
	AND b_signs.FILE-NAME EQ "loan"
	AND b_signs.SURROGATE EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
	NO-LOCK NO-ERROR.
		IF AVAIL b_signs THEN DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = b_signs.DEC-VALUE
			NO-LOCK NO-ERROR.
			/* заполним ФИО представителя */
			IF AVAIL PERSON THEN 
				str.FIO_AGENT = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
			
	/* пойдем в историю */	
	/* кто и когда открыл */	
	FOR EACH HISTORY
	WHERE HISTORY.FILE-NAME EQ "LOAN"
	AND	HISTORY.FIELD-REF EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
	NO-LOCK BY HISTORY.MODIF-DATE:

		str.DATE_OPEN = STRING( HISTORY.MODIF-DATE, "99.99.9999").
		LEAVE.
	END.
	/* найти ФИО пользователя, который открыл */
	
	FIND FIRST _USER
	WHERE _USER._USERID EQ HISTORY.USER-ID
	NO-LOCK NO-ERROR.
	IF AVAILABLE _USER THEN
		str.FIO_OPEN = STRING(_USER._USER-NAME).
	/* если договор закрыт - то ищем дату и кто закрыл */
	IF LOAN.LOAN-STATUS = "ЗАКР" THEN DO:
	/* смотрим последнее изменение статуса */
	
		FOR EACH HISTORY
		WHERE HISTORY.FILE-NAME EQ "LOAN"
		AND	HISTORY.FIELD-REF EQ LOAN.CONTRACT + "," + LOAN.CONT-CODE
		AND	HISTORY.MODIFY EQ "W"
		NO-LOCK BY HISTORY.MODIF-DATE DESCENDING:
			/*
			MESSAGE HISTORY.USER-ID VIEW-AS ALERT-BOX.
			*/
			IF INDEX(HISTORY.FIELD-VALUE, "loan-status") > 0 THEN DO:
				str.DATE_CLOSE = STRING( HISTORY.MODIF-DATE, "99.99.9999").
				LEAVE.
			END.
		END.	

		/* найти ФИО пользователя, который закрыл */
		
		FIND FIRST _USER
		WHERE _USER._USERID EQ HISTORY.USER-ID
		NO-LOCK NO-ERROR.
		IF AVAILABLE _USER THEN
			str.FIO_CLOSE = _USER._USER-NAME.
	END.		
END.	
END.	

fname = "./principal_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

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
  <Style ss:ID="m46038528">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>
  <Style ss:ID="m46038548">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>
  <Style ss:ID="m46038568">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>
  <Style ss:ID="s69">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
  </Style>
  <Style ss:ID="s70">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
  </Style>
  <Style ss:ID="s71">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
  </Style>
 </Styles>
 '.
/* идем по каждому подразделению*/ 
FOR EACH str
	WHERE str.TYPE = 0
	AND ( 
	( CLOSE_D = true AND str.DATE_CLOSE <> '')
	OR
	( OPEN_D = true AND str.DATE_CLOSE = '' )
	OR
	( OPEN_D = false AND CLOSE_D = false )
	)
	NO-LOCK BY str.BRANCH BY str.FIO_PRINCIPAL :

/* если новая группа - то создадим новый лист */
	IF tmp_br <> str.BRANCH THEN DO:
		/* предварительно закроем предыдущий лист*/
		
		IF tmp_br <> '' THEN
		    put stream puk unformatted
			'</Table> </Worksheet>
			'.
		.
	/* новый лист */
	put stream puk unformatted
	'<Worksheet ss:Name= "' + STRING( str.BRANCH ) + '">
	  <Table ss:ExpandedColumnCount="11" x:FullColumns="1"
	   x:FullRows="1" ss:DefaultRowHeight="15">
	   <Column ss:AutoFitWidth="0" ss:Width="50.25"/>
	   <Column ss:AutoFitWidth="0" ss:Width="60.75" ss:Span="1"/>
	   <Column ss:Index="4" ss:AutoFitWidth="0" ss:Width="193.5"/>
	   <Column ss:AutoFitWidth="0" ss:Width="177.75"/>
	   <Column ss:AutoFitWidth="0" ss:Width="59.25"/>
	   <Column ss:AutoFitWidth="0" ss:Width="222"/>
	   <Column ss:AutoFitWidth="0" ss:Width="60.75"/>
	   <Column ss:AutoFitWidth="0" ss:Width="186"/>
	   <Column ss:AutoFitWidth="0" ss:Width="114"/>
	   <Column ss:AutoFitWidth="0" ss:Width="108"/>
	   <Row ss:AutoFitHeight="0" ss:Height="18.75">
		<Cell ss:MergeAcross="4" ss:StyleID="m46038528"><Data ss:Type="String">Доверенность</Data></Cell>
		<Cell ss:MergeAcross="1" ss:StyleID="m46038548"><Data ss:Type="String">Открытие</Data></Cell>
		<Cell ss:MergeAcross="1" ss:StyleID="m46038568"><Data ss:Type="String">Закрытие</Data></Cell>
	   </Row>
	   <Row ss:AutoFitHeight="0" ss:Height="27.75">
		<Cell ss:StyleID="s69"><Data ss:Type="String">Код филиала</Data></Cell>
		<Cell ss:StyleID="s69"><Data ss:Type="String">Начало</Data></Cell>
		<Cell ss:StyleID="s69"><Data ss:Type="String">Окончание</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">ФИО доверителя</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">ФИО представителя</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Дата</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Сотрудник</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Дата</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Сотрудник</Data></Cell>
	   </Row>
	'. 
	/* запомним подразделение */
	tmp_br = str.BRANCH.
	END.
  
	/*FOR EACH str NO-LOCK BY str.BRANCH BY str.FIO_PRINCIPAL:*/

		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.BRANCH + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.PR_BEGIN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.PR_END + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_PRINCIPAL + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_AGENT + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.DATE_OPEN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_OPEN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.DATE_CLOSE + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.FIO_CLOSE + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.

	/*END.*/

END.

		put stream puk unformatted
		'</Table> </Worksheet>
		'.

/********************* Доверенностями РКО **********************/
	/* новый лист */
	put stream puk unformatted
	'<Worksheet ss:Name= "' + 'Доверенности РКО' + '">
	  <Table ss:ExpandedColumnCount="11" x:FullColumns="1"
	   x:FullRows="1" ss:DefaultRowHeight="15">
	   <Column ss:AutoFitWidth="0" ss:Width="50.25"/>
	   <Column ss:AutoFitWidth="0" ss:Width="60.75" ss:Span="1"/>
	   <Column ss:Index="4" ss:AutoFitWidth="0" ss:Width="193.5"/>
	   <Column ss:AutoFitWidth="0" ss:Width="177.75"/>
	   <Column ss:AutoFitWidth="0" ss:Width="59.25"/>
	   <Column ss:AutoFitWidth="0" ss:Width="222"/>
	   <Column ss:AutoFitWidth="0" ss:Width="60.75"/>
	   <Column ss:AutoFitWidth="0" ss:Width="186"/>
	   <Column ss:AutoFitWidth="0" ss:Width="114"/>
	   <Column ss:AutoFitWidth="0" ss:Width="108"/>
	   <Row ss:AutoFitHeight="0" ss:Height="18.75">
		<Cell ss:MergeAcross="4" ss:StyleID="m46038528"><Data ss:Type="String">Доверенность</Data></Cell>
		<Cell ss:MergeAcross="1" ss:StyleID="m46038548"><Data ss:Type="String">Открытие</Data></Cell>
		<Cell ss:MergeAcross="1" ss:StyleID="m46038568"><Data ss:Type="String">Закрытие</Data></Cell>
	   </Row>
	   <Row ss:AutoFitHeight="0" ss:Height="27.75">
		<Cell ss:StyleID="s69"><Data ss:Type="String">Код филиала</Data></Cell>
		<Cell ss:StyleID="s69"><Data ss:Type="String">Начало</Data></Cell>
		<Cell ss:StyleID="s69"><Data ss:Type="String">Окончание</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">ФИО доверителя</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">ФИО представителя</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Дата</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Сотрудник</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Дата</Data></Cell>
		<Cell ss:StyleID="s70"><Data ss:Type="String">Сотрудник</Data></Cell>
	   </Row>
	'. 
	FOR EACH str
		WHERE str.TYPE = 1
		AND ( 
		( CLOSE_D = true AND str.DATE_CLOSE <> '')
		OR
		( OPEN_D = true AND str.DATE_CLOSE = '' )
		OR
		( OPEN_D = false AND CLOSE_D = false )
		)
		NO-LOCK BY str.BRANCH BY str.FIO_PRINCIPAL :

		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.BRANCH + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.PR_BEGIN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + str.PR_END + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_PRINCIPAL + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_AGENT + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.DATE_OPEN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s71"><Data ss:Type="String">' + str.FIO_OPEN + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.DATE_CLOSE + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + str.FIO_CLOSE + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.

	END.
/********************* Доверенностями РКО **********************/
put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
/*---------------------------*/