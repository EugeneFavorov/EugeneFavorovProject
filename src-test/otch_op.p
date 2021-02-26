/*
	Проверочный отчет
zss
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}

DEFINE TEMP-TABLE otch
   FIELD docNumber AS CHAR
   FIELD acctdb    AS CHAR                 /* */
   FIELD acctcr    AS CHAR 
   FIELD amt-rub   AS DECIMAL               /* */
   FIELD opdetails AS CHAR                 /* */
   FIELD LoanCode  AS CHAR
.
{empty otch}

DEF VAR mK        AS INT64 INIT 0 NO-UNDO.
DEF VAR fname     AS CHAR  NO-UNDO.
DEF VAR fstr      AS CHAR  INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.

/* По отмеченным */
FOR EACH tmprecid NO-LOCK,
	FIRST op WHERE RECID(op) EQ tmprecid.id NO-LOCK:

   FOR EACH op-entry 
      WHERE op-entry.op EQ op.op 
   NO-LOCK:

      CREATE otch. 
	   ASSIGN
       otch.docNumber = op.doc-num
	 	   otch.acctdb    = op-entry.acct-db
	 	   otch.acctcr    = op-entry.acct-cr
       otch.amt-rub   = op-entry.amt-rub
	 	   otch.opdetails = op.details
      .
    FIND LAST loan-acct WHERE loan-acct.acct EQ op-entry.acct-db  NO-LOCK NO-ERROR. 
      IF avail loan-acct then otch.LoanCode = Entry(1,loan-acct.cont-code,'@').

  END.
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="55"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="130"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="130"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="700"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="130"/>
   <Row ss:Index="1" ss:AutoFitHeight="0" ss:Height="17.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">№</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">№док</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Дебет</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Кредит</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Назначение</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер КД</Data></Cell>
   </Row>
'.

FOR EACH otch NO-LOCK:
   mK = mK + 1.
   PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(mK) + '</Data></Cell>\n'.
     
     PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.docNumber + '</Data></Cell>\n'.

      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.

      PUT STREAM vvs UNFORMATTED DelFilFromLoan(otch.acctdb) + '</Data></Cell>\n'.
	   PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED DelFilFromLoan(otch.acctcr) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(otch.amt-rub) + '</Data></Cell>\n'.
	   PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.opdetails + '</Data></Cell>\n'.
   PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.LoanCode + '</Data></Cell>\n'.
   PUT STREAM vvs UNFORMATTED '</Row>'.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}