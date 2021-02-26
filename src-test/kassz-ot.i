    def var fname as char no-undo.
    def new shared stream vvs.

    
    fname = "./kasszot"  + "_" + userid('bisquit') + ".xml".
   
   output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  put stream vvs unformatted '
 <?xml version="1.0"?>
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
  <Style ss:ID="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s66">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s67">
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s68">
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s70" ss:Parent="s62">
   <Font ss:Bold="1"/>
  </Style>
  <Style ss:ID="s71">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   </Style>
  <Style ss:ID="s78">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s79">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s80">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s81">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s82">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="journal">
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="10000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="115"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="180"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="90"/>
   <Column ss:StyleID="s64" ss:Width="100"/>
   <Column ss:StyleID="s65" ss:Width="190"/>
   <Column ss:StyleID="s63" ss:Width="100"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
'.
/*   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>*/
   put stream vvs unformatted '<Row>
                <Cell><Data ss:Type="String">Дата</Data></Cell>
                <Cell><Data ss:Type="String">Отделение</Data></Cell>
                <Cell><Data ss:Type="String">Наименование</Data></Cell>
                <Cell><Data ss:Type="String">Счет</Data></Cell>
                <Cell><Data ss:Type="String">Сумма</Data></Cell>
                <Cell><Data ss:Type="String">Кассовый символ</Data></Cell>
                <Cell><Data ss:Type="String">Исполнитель</Data></Cell>
                <Cell><Data ss:Type="String">Отделение исполнителя</Data></Cell>
                </Row>
                '.
DEF VAR kKZ AS INT64 NO-UNDO.
FOR EACH tt-kassop NO-LOCK
    BREAK BY tt-kassop.zdate:
   put stream vvs unformatted '<Row>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.zdate) + '</Data></Cell>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.branch) + '</Data></Cell>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.name-corp) + '</Data></Cell>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.acct) + '</Data></Cell>
                <Cell><Data ss:Type="Number">' + STRING(tt-kassop.summ) + '</Data></Cell>
                <Cell><Data ss:Type="Number">' + STRING(tt-kassop.ks) + '</Data></Cell>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.user-name) + '</Data></Cell>
                <Cell><Data ss:Type="String">' + STRING(tt-kassop.branchuser) + '</Data></Cell>
                </Row>
                '.  
    kKZ = kKZ + 1.
    
                  
END.
kKZ = kKZ.
  put stream vvs unformatted '<Row>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell ss:Formula="=SUM(R[-' + STRING(kKZ) + ']C:R[-1]C)" ><Data ss:Type="Number">' + STRING('1') + '</Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                <Cell><Data ss:Type="String"> </Data></Cell>
                </Row>
                '.      

 put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").  