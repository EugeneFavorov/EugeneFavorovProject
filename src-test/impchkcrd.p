/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: importpol.p
      Comment: Проверка кредитов
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

/*
{globals.i}
{setdest.i}
{intrface.get count}
*/

{globals.i}
{intrface.get refer}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{pick-val.i}
{refer.i}
/**/
FUNCTION myreplace RETURNS CHARACTER (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vStrTMP  AS CHARACTER NO-UNDO.
   vStrTMP = REPLACE(REPLACE(iStr,'╓','"'),'·','"').
   vStrTMP = REPLACE(TRIM(TRIM(vStrTMP),'"'),'""','"').
   RETURN vStrTMP.
END FUNCTION.   
/**/
/* находим наименование клиента */
PROCEDURE GetName:
	DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
	DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
	IF cat = "Ч" THEN

		FIND FIRST PERSON 
		WHERE PERSON.PERSON-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL PERSON THEN
			/* ФИО клиента */
			sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
	ELSE

		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
			/* наименование организации */
			sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
END.	
/**/

DEFINE TEMP-TABLE crd
	FIELD svin		AS CHAR 	/* идентификационный номер (VIN) */
    FIELD custId 	AS CHAR	/* идентификатор клиента */
	FIELD cname		AS CHAR		/* наименование клиента */
	FIELD cat		AS CHAR		/* тип клиента */
    FIELD code		AS CHAR		/* номер КД */
    .
DEF VAR t_svin AS CHAR NO-UNDO.
DEF VAR t_custId AS CHAR NO-UNDO.
DEF VAR t_cname AS CHAR NO-UNDO.
DEF VAR t_cat AS CHAR NO-UNDO.
DEF VAR t_code AS CHAR NO-UNDO.
    
{empty crd}

DEFINE VAR fstr AS CHAR INIT '' NO-UNDO.
DEFINE VAR custN AS CHAR NO-UNDO.
DEFINE VAR sVIN AS CHAR NO-UNDO.

DEFINE VARIABLE mTemp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mFileName AS CHARACTER  NO-UNDO.  /* имя файла */


def new shared stream puk.
def var fname as char  init "./find_credits.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
/*---------------------------------*/
/* форма запроса данных */
FORM
   mFileName
      VIEW-AS FILL-IN SIZE 60 BY 1
      FORMAT "X(1024)"
      LABEL  "Файл"
      HELP "Файл для импорта (F1 - выбрать файл)"

WITH FRAME fGet OVERLAY 1 COL CENTERED ROW 10
COLOR messages SIDE-LABELS.

ON F1 OF mFileName IN FRAME fGet
DO:
   pick-value = "".
   RUN ch-file.p (INPUT-OUTPUT mTemp).
   IF LAST-EVENT:FUNCTION NE "END-ERROR"
   THEN SELF:SCREEN-VALUE = mTemp.
   RETURN NO-APPLY.
END.
/*---------------------------------*/

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
	  UPDATE
		 mFileName
	  WITH FRAME fGet.
	END.
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	
	/* открываем файл */
	INPUT FROM VALUE (mFileName) CONVERT TARGET "IBM866" SOURCE "1251".

	REPEAT ON ENDKEY UNDO,LEAVE:
		IMPORT UNFORMATTED fstr NO-ERROR.
		/* считали идентификационный номер */	
		sVIN = myreplace(ENTRY(1,fstr,';')).
		
		t_svin = sVIN.
		t_custId = ''.
		t_cname = ''.
		t_cat = ''.
		t_code = ''.
		/* по нему найдем данные */
		FIND FIRST SIGNS
			WHERE SIGNS.FILE-NAME = "term-obl"
			AND SIGNS.CODE = "TCVIN"
			AND SIGNS.XATTR-VALUE = TRIM(sVIN)
		NO-LOCK NO-ERROR.
			/* КД */
			IF AVAIL SIGNS THEN DO:
				FIND FIRST LOAN
					WHERE LOAN.CONTRACT = ENTRY(1,SIGNS.SURROGATE,',')
					AND	LOAN.CONT-CODE = ENTRY(2,SIGNS.SURROGATE,',')
				NO-LOCK NO-ERROR.
				
				IF AVAIL LOAN THEN DO:
				
					/* найдем наименование клиента */
					RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT custN).
						/* запомним данные */
						t_custId = STRING(LOAN.CUST-ID).
						t_cname = custN.
						t_cat = LOAN.CUST-CAT. 
						t_code = LOAN.CONT-CODE.
				END.
			END.
			
			
			/* вставляем строку */
				CREATE crd.
					ASSIGN
					crd.svin = t_svin
					crd.custId = STRING(t_custId)
					crd.cat = t_cat
					crd.cname = t_cname
					crd.code = t_code
				. 
			/**/
    END.
END.

IF AVAIL crd THEN
DO:
/* все считали - теперь выводим в EXCEL*/
fname = "./sf_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

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
  <WindowHeight>7740</WindowHeight>
  <WindowWidth>13395</WindowWidth>
  <WindowTopX>120</WindowTopX>
  <WindowTopY>45</WindowTopY>
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
  <Style ss:ID="s79">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s82">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s130">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table ss:ExpandedColumnCount="5" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:AutoFitWidth="0" ss:Width="164.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="68.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="198.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="132.75"/>
   <Row ss:AutoFitHeight="0" ss:Height="19.5">
    <Cell ss:StyleID="s79"><Data ss:Type="String">VIN</Data></Cell>
    <Cell ss:StyleID="s79"><Data ss:Type="String">ИД клиента</Data></Cell>
    <Cell ss:StyleID="s79"><Data ss:Type="String">Тип клиента</Data></Cell>
    <Cell ss:StyleID="s79"><Data ss:Type="String">Наименование клиента</Data></Cell>
    <Cell ss:StyleID="s79"><Data ss:Type="String">КД</Data></Cell>
   </Row>
'.

	FOR EACH crd
		NO-LOCK:
			PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
			PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s82"><Data ss:Type="String">' + crd.svin + '</Data></Cell>\n'.
			PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s82"><Data ss:Type="String">' + crd.custId + '</Data></Cell>\n'.
			PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s79"><Data ss:Type="String">' + crd.cat + '</Data></Cell>\n'.
			PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s130"><Data ss:Type="String">' + crd.cname + '</Data></Cell>\n'. 
			PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s130"><Data ss:Type="String">' + STRING(crd.code) + '</Data></Cell>\n'. 
			PUT STREAM puk UNFORMATTED '</Row>\n'.	
			
    END.
	
put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
/**/
END.

/* конец вывода */	
	
{intrface.del}          /* Выгрузка инструментария.  */    
RETURN "".