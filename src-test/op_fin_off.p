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
      Created: vvv      наименование плательщика, номер счета, дата и номер платежного поручения, сумма, наименование получателя
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря"

def new shared stream puk.
def var fname 			as char  init "./branch_rates.csv"  no-undo.
def var delim 			as char init ";" format "x(1)" no-undo.
def var eol 			as char format "x(2)" no-undo.

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
    FIELD nom-doc 	 AS CHAR		
    FIELD doc-type   AS CHAR     
	FIELD code-name	 AS CHAR		
    FIELD status-doc AS CHAR
    FIELD branch-id	 AS CHAR	
	FIELD details	 AS CHAR
	FIELD doc-date   AS CHAR
	FIELD name-send  AS CHAR
	FIELD summ       AS DECIMAL
	FIELD acct       AS CHAR
	FIELD name-rec   AS CHAR
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
	
	/*IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	/* время дня */
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59"). 
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00"). 

	 найдем запись */
	/*
	FIND FIRST branch
		WHERE /*RECID(branch) EQ tmprecid.id*/ branch.branch-id EQ shfilial
	NO-LOCK NO-ERROR.
	*/
	/* plus.vvv */
	/* 30/05/2014 */
	/* найдем ДР на пользователе 
	FIND FIRST SIGNS
		WHERE SIGNS.FILE-NAME = "_user"
		AND SIGNS.SURROGATE = CAPS(userid("bisquit"))
		AND SIGNS.CODE = "Отделение"
	NO-LOCK NO-ERROR.
	
	/**/
	IF AVAIL SIGNS THEN
	

	TMP_TIME = ?.*/

 FIND FIRST code where code.class eq 'mess-error'
 and code.code  eq 'm300'  no-lock no-error.
 /*MESSAGE code.name VIEW-AS ALERT-BOX.          */          
                     
   FOR EACH op WHERE op.op-status EQ 'А'
                     and  op.doc-date    EQ mDate 
                     and   INDEX(op.op-error,"m300" ) > 0 NO-LOCK,
   EACH op-entry WHERE op-entry.op EQ op.op 
                                         
      NO-LOCK:
           CREATE br_rates.   
                    ASSIGN 
                        br_rates.nom-doc    = op.doc-num 
                        br_rates.doc-type   = op.doc-type
                        br_rates.code-name  =  if avail code then code.name else "Не определено"  
                        br_rates.status-doc = op.op-status
                        br_rates.branch-id  = op.branch-id   
                        br_rates.details    = op.details 
                        br_rates.doc-date   = REPLACE(STRING(op.ins-date, "99/99/9999"), "/", ".")
                        br_rates.name-send  = GetXAttrValueEx("op", STRING(op.op), "name-send", "").
                        br_rates.summ       = op-entry.amt-rub. 
                        br_rates.name-rec   = op.name-ben.
                        br_rates.acct       = substr(string(op-entry.acct-db),1,20).                
                       .
end.
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
	
put stream puk unformatted 
'<?xml version="1.0"?>
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
   <NumberFormat ss:Format="@"/>
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
  <Style ss:ID="s68" >
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" />   
   <NumberFormat ss:Format="Short Date"/>
  </Style> 
  <Style ss:ID="s69">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" />
   <NumberFormat ss:Format="0.00"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="lll">
  <Table >
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="35"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="35"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="60"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="150"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Код </Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Сообщение</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Подразделение</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Содержание</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата документа</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Плательщик</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Сумма</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Получатель</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Счет</Data></Cell>
   </Row>
'.

   FOR EACH br_rates
    WHERE br_rates.status-doc <> ""
    NO-LOCK 
    BY br_rates.branch-id:
        PUT STREAM puk UNFORMATTED '<Row>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.nom-doc  + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.doc-type  + '</Data></Cell>'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.code-name + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.branch-id + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.details + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.doc-date + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.name-send + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + string(br_rates.summ) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.name-rec + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + br_rates.acct + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '</Row>\n'.  
        i = i + 1.
    END.
         
put stream puk unformatted
'  </Table>
 </Worksheet>
</Workbook>
' .

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

/* конец вывода */		




