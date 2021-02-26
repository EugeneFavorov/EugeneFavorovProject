/*
	Проверочный отчет
Данный отчет должен выгружаться по кредитам с доп. реквизитом Priznak - <союз> и содержать информацию:
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}

DEFINE TEMP-TABLE otch
   FIELD cont_code    AS CHAR               /* номер КД */
   FIELD fio          AS CHAR               /* ФИО заемщика */
   FIELD addr         AS CHAR               /* адрес заемщика */
	FIELD ind          AS CHAR               /* индекс заемщика */
   FIELD open_date    AS DATE               /* дата открытия договора */
   FIELD city         AS CHAR               /* регион для отправки уведомлений */
   
   INDEX cont_code cont_code       
.
{empty otch}

DEF VAR nameCl     AS CHAR         NO-UNDO.
DEF VAR addrCl     AS CHAR         NO-UNDO.
DEF VAR indaddrCl  AS CHAR         NO-UNDO.
DEF VAR indCl      AS CHAR         NO-UNDO.
DEF VAR open_date  AS CHAR         NO-UNDO.
DEF VAR mI         AS INT64        NO-UNDO. /* перебор индекса */
DEF VAR mK         AS INT64 INIT 0 NO-UNDO. /* счетчик договоров */
DEF VAR mN         AS INT64 INIT 0 NO-UNDO. /* счетчик количества почтовых реестров */
DEF VAR mCode-type AS CHAR         NO-UNDO.
DEF VAR mCity      AS CHAR         NO-UNDO.
DEF VAR mDecimal   AS DECIMAL      NO-UNDO.
       
DEF VAR fname      AS CHAR         NO-UNDO.
DEF VAR fstr       AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.

/* По отмеченным клиентам */
FOR EACH tmprecid, /* NO-LOCK, */
	FIRST loan WHERE 
   RECID(loan) = tmprecid.id 
NO-LOCK:

   RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT nameCl).
  
   RUN RetAdrOutloan.p(INPUT INT64(loan.cust-id), 
                       INPUT loan.cust-cat, 'АдрФакт', 
                       INPUT loan.open-date, 
                       OUTPUT indaddrCl, 
                       OUTPUT mCity).
   indaddrCl = TRIM(TRIM(indaddrCl,',')).
   
   IF indaddrCl = '' THEN 
      RUN RetAdrOutloan.p(INPUT INT64(loan.cust-id), 
                          INPUT loan.cust-cat, 'АдрПроп', 
                          INPUT loan.open-date, 
                          OUTPUT indaddrCl, 
                          OUTPUT mCity).
   indaddrCl = TRIM(TRIM(indaddrCl,',')).
   
   RUN RetAdrOutloan.p(INPUT INT64(loan.cust-id), 
                       INPUT loan.cust-cat, 'АдрФакт', 
                       TODAY, 
                       OUTPUT indaddrCl, 
                       OUTPUT mCity).
   indaddrCl = TRIM(TRIM(indaddrCl,',')).
   
   IF indaddrCl = '' THEN 
      RUN RetAdrOutloan.p(INPUT INT64(loan.cust-id), 
                          INPUT loan.cust-cat, 'АдрПроп', 
                          TODAY, 
                          OUTPUT indaddrCl, 
                          OUTPUT mCity).

   IF NUM-ENTRIES(indaddrCl, ",") > 1 THEN 
   DO:
      mDecimal = DECIMAL(ENTRY(1, indaddrCl)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         indCl = "".
      ELSE 
         indCl  = ENTRY(1, indaddrCl).
      addrCl = TRIM(LEFT-TRIM(indaddrCl, "," + indCl)).
   END.

   CREATE otch. 
   ASSIGN
      otch.cont_code  = loan.cont-code
      otch.fio        = nameCl
      otch.addr       = addrCl
      otch.ind        = indCl    
      otch.open_date  = loan.open-date
      otch.city       = mCity
   .
END.

/* выводим в ексель */
fname = "./otch"  + "_" + USERID('bisquit') + ".xml".
OUTPUT STREAM vvs TO VALUE (fname)
UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

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
 </Styles>
 <Worksheet ss:Name="lll">
  <Table>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="25"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="70"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="40"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="140"/>
   <Row ss:Index="1" ss:AutoFitHeight="0" ss:Height="20">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер списка</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">№ п/п</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
	  <Cell ss:StyleID="s66"><Data ss:Type="String">Индекс_Адрес</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата открытия</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Индекс</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и1</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и2</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и3</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и4</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и5</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">и6</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Адрес</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО_Индекс_Адрес</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Город</Data></Cell>
   </Row>
'.

FOR EACH otch NO-LOCK BY otch.fio:
   mK = mK + 1.
   IF (mK + 34) MODULO 35 EQ 0 THEN
      mN = mN + 1.
   PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(mN) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Index="2"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(mK) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.fio + '</Data></Cell>\n'.
	   PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED DelFilFromLoan(otch.cont_code) + '</Data></Cell>\n'.
	   PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.ind + ', ' + otch.addr + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(otch.open_date,"99.99.9999") + ' г.</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.ind + '</Data></Cell>\n'.
      IF otch.ind NE "" THEN 
      DO mI = 1 TO LENGTH(otch.ind):
         PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
         PUT STREAM vvs UNFORMATTED SUBSTRING(otch.ind, mI, 1) + '</Data></Cell>\n'.
      END.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Index="14"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(otch.addr) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Formula="=RC[-12]&amp;CHAR(10)&amp;RC[-10]"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(otch.city) + '</Data></Cell>\n'.
   PUT STREAM vvs UNFORMATTED '</Row>'.
END.    

PUT STREAM vvs UNFORMATTED 
'  
  </Table>
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