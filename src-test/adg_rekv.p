/* Задвоение операций 408 & 973*/

{globals.i}

{tmprecid.def}

{intrface.get xclass}

DEFINE VARIABLE fname AS CHARACTER NO-UNDO.
DEFINE NEW SHARED STREAM vvs.

/* выводим в ексель */
fname = "./otchdg"  + "_" + USERID('bisquit') + ".xml".
OUTPUT STREAM vvs TO VALUE (fname)
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
  <WindowHeight>11445</WindowHeight>
  <WindowWidth>15015</WindowWidth>
  <WindowTopX>510</WindowTopX>
  <WindowTopY>570</WindowTopY>
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
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s64">
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
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s68" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s69" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Standard"/>
  </Style>
  <Style ss:ID="s70">
   <NumberFormat ss:Format="Standard"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="lll">
  <Table ss:ExpandedColumnCount="9" ss:ExpandedRowCount="29031978" x:FullColumns="1"
   x:FullRows="1">
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="159.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s68"><Data ss:Type="String">Договор</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">ДГ- Дата подачи заявления:</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">ДГ- Дата досрочного гашения:</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">ДГ- Сумма досрочного гашения:</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">ДГ- Тип досрочного гашения:</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">ДГ- После ДГ уменьшить:</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">ДГ- Перечисление:</Data></Cell>        
   </Row>
   '.
   
for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
        
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED loan.doc-ref + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    IF GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_Date_pod_zay","") <> "" THEN       
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_Date_pod_zay",""))).
    PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="DateTime">'.
    IF GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_Date_dosr_g","") <> "" THEN  
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_Date_dosr_g",""))).
    PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_summ_dosr_g","")) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED string(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_type_dosr_g","")) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED string(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_type_dosr_g2","")) + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED string(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dg_perechisl","")) + '</Data></Cell>\n'.      


     PUT STREAM vvs UNFORMATTED '</Row>\n'.
end.
 
PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .
OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}

   
