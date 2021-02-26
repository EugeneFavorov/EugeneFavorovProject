/*
	Проверочный отчет
Вам направлена копия Служебной записки № ОПХД.525 "Создание отчета в БИС" от 24.05.2017.
Автор: Родионова Наталья Викторовна. 


*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{intrface.get comm}

DEFINE TEMP-TABLE otch
        FIELD cont_code    AS CHAR                 /* номер КД */
        FIELD fio          AS CHAR                 /* ФИО заемщика */
        FIELD open_date    AS DATE                 /* дата выдачи */
        FIELD open_date1   AS date                 /* дата выдачи залога 1 */
        FIELD open_date2   AS date                 /* дата выдачи залога 2 */
        FIELD marc1        AS char                
        FIELD marc2        AS char                 
        FIELD year1        AS CHAR
        FIELD year2        AS CHAR
        FIELD vin1        AS CHAR
        FIELD vin2        AS CHAR
        FIELD summ1       AS DECIMAL
        FIELD summ2       AS DECIMAL
        INDEX cont_code cont_code       
    .


{empty otch}

def var cont_code as char no-undo.
def var fio as char no-undo.
def var open_date as date no-undo.
def var open_date1 as date no-undo.
def var open_date2 as date no-undo.
def var marc1 as char no-undo.
def var marc2 as char no-undo.
def var year1 as char no-undo.
def var year2 as char no-undo.
def var vin1 as char no-undo.
def var vin2 as char no-undo.
def var summ1 as decimal no-undo.
def var summ2 as decimal no-undo.

DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 
def var countob as int no-undo.
def buffer bsigns for signs.

 /* {getdate.i} */

/* По отмеченным клиентам */
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT fio).
    open_date = loan.open-date.
    cont_code = loan.doc-ref.
    find first loan-int where loan-int.cont-code = loan.cont-code 
            and loan-int.contract = 'Кредит'
            and loan-int.id-k = 3
            and loan-int.id-d = 0
            and loan-int.mdate >= loan.open-date
            and loan-int.amt-rub <> 0
            no-lock no-error.
    if avail loan-int then open_date = loan-int.mdate.

    countob = 0.
    
    FOR EACH term-obl WHERE
    term-obl.cont-code = loan.cont-code
    AND term-obl.contract = 'Кредит'
    AND term-obl.idnt = 5 NO-LOCK,
    FIRST signs
            WHERE signs.file-name = 'term-obl'
            AND signs.code = 'ВидОб'
            AND signs.xattr-value = 'Автомобиль'
            AND signs.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
    NO-LOCK:
        if countob = 0 then do:
            summ1 = term-obl.amt-rub.
            open_date1 = term-obl.fop-date.
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCbrand'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then marc1 = bsigns.xattr-value.
            else marc1 = ''.            
 
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCmodel'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then marc1 = TRIM(marc1 + ' ' + bsigns.xattr-value).
            
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCyear'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then year1 = bsigns.code-value.
            else year1 = ''. 
            
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCvin'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then vin1 = bsigns.xattr-value.
            else vin1 = ''.
        end.
        
        if countob = 1 then do:
            summ2 = term-obl.amt-rub.
            open_date2 = term-obl.fop-date.
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCbrand'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then marc2 = bsigns.xattr-value.
            else marc2 = ''.            
 
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCmodel'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then marc2 = TRIM(marc2 + ' ' + bsigns.xattr-value).
            
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCyear'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then year2 = bsigns.code-value.
            else year2 = ''. 
            
            find FIRST bsigns
            WHERE bsigns.file-name = 'term-obl'
            AND bsigns.code = 'TCvin'
            AND bsigns.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK no-error.
            if avail bsigns then vin2 = bsigns.xattr-value.
            else vin2 = ''.
        end.
        
        countob = countob + 1.
    
    end.
    
    IF countob > 1 then do:
        
	  CREATE otch.
	  ASSIGN
		otch.cont_code 	= cont_code
		otch.fio 		= fio
        otch.open_date = open_date
        otch.open_date1 = open_date1
        otch.open_date2 = open_date2
        otch.marc1 = marc1
        otch.marc2 = marc2
        otch.year1 = year1
        otch.year2 = year2
        otch.vin1 = vin1
        otch.vin2 = vin2
        otch.summ1 = summ1
        otch.summ2 = summ2
		.
	end.	
		
		
END.

/* выводим в ексель */
    fname = "./otch"  + "_" + USERID('bisquit') + ".xml".
		    output stream vvs to value (fname)
        UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

    PUT STREAM vvs UNFORMATTED
'
<?xml version="1.0"?>
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
   <NumberFormat ss:Format="Standard"/>
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
  <Style ss:ID="s68">
    <Alignment ss:Horizontal="Right" ss:Vertical="Bottom" ss:WrapText="1"/>
    <NumberFormat ss:Format="0"/>
  </Style>

 </Styles>
 <Worksheet ss:Name="lll">
  <Table >
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80.00"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>      
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО</Data></Cell>\
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата выдачи</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата заключения договора обесп1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Марка ТС обесп1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Год выпуска обесп1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">VIN обесп1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма обесп1</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата заключения договора обесп2</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Марка ТС обесп2</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Год выпуска обесп2</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">VIN обесп2</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма обесп2</Data></Cell>
   </Row>\n'.

      
FOR EACH otch NO-LOCK BY otch.cont_code BY cont_code:
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.fio + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.cont_code + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.open_date)) + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.open_date1)) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.marc1 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.year1 + '</Data></Cell>\n'.  
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.vin1 + '</Data></Cell>\n'.  
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.summ1) + '</Data></Cell>\n'.
            
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.open_date2)) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.marc2 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.year2 + '</Data></Cell>\n'.  
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.vin2 + '</Data></Cell>\n'.  
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.summ2) + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED '</Row>\n'.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}



PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
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
END PROCEDURE.









