/* Отчет по процедуре перечисления со счета*/

/* имя создаваемого файла */
fname = "./REPORT" + "_" + STRING(end-date,'99-99-9999') + ".xml".

/* записываем в файл */   
OUTPUT STREAM vvs TO VALUE (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
/* стили */  
PUT STREAM vvs UNFORMATTED 
'<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">\n
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">\n
  <Version>14.00</Version>\n
 </DocumentProperties>\n
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">\n
  <AllowPNG/>\n
 </OfficeDocumentSettings>\n
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">\n
  <ProtectStructure>False</ProtectStructure>\n
  <ProtectWindows>False</ProtectWindows>\n
 </ExcelWorkbook>\n
 <Styles>\n
  <Style ss:ID="Default" ss:Name="Normal">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="up_b">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>\n
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="2"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="2"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="2"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="2"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="left">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="left_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="right">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="right_b">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
 </Styles>\n
 '.

PUT STREAM vvs UNFORMATTED
'<Worksheet ss:Name="Отчет">\n
  <Table ss:ExpandedColumnCount="9" ss:ExpandedRowCount="500" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:AutoFitWidth="0" ss:Width="170" ss:Span="1"/>
   <Column ss:Index="6" ss:AutoFitWidth="0" ss:Width="122.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="105"/>
   <Column ss:AutoFitWidth="0" ss:Width="82"/>
<Row>\n
   <Cell ss:StyleID="Default" ss:MergeAcross="1"><Data ss:Type="String">Перечисление со счета после закрытия вклада</Data></Cell>\n
</Row>\n   
<Row>\n
   <Cell ss:StyleID="Default"><Data ss:Type="String">' + strdate + '</Data></Cell>\n
</Row>\n
<Row>\n
</Row>\n
<Row ss:Height="45">\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Дата перечисления</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Номер договора</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Тип вклада</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Счет с которого перечисляем</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Счет на который перечисляем</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Сумма перечисления</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Остаток на счете с которого перечисляем</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Дата закрытия счета с которого перечисляем</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Код клиента</Data></Cell>\n
</Row>\n
'.

FOR EACH tt-loan
  BREAK BY tt-loan.date-per:

       PUT STREAM vvs UNFORMATTED 
      '<Row ss:AutoFitHeight="0">\n
       <Cell ss:StyleID="left_b"><Data ss:Type="String">' + STRING(tt-loan.date-per,"99.99.9999") + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-loan.cont-code) + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-loan.cont-type) + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-loan.acct-from) + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-loan.acct-to) + '</Data></Cell>\n
       <Cell ss:StyleID="right"><Data ss:Type="String">' + STRING(tt-loan.amt-per) + '</Data></Cell>\n
       <Cell ss:StyleID="right"><Data ss:Type="String">' + STRING(tt-loan.acct-bal) + '</Data></Cell>\n
       <Cell ss:StyleID="right"><Data ss:Type="String">' + STRING(tt-loan.close-date) + '</Data></Cell>\n
       <Cell ss:StyleID="right_b"><Data ss:Type="String">' + STRING(tt-loan.cust-id) + '</Data></Cell>\n
       </Row>\n
      '.
END.

PUT STREAM vvs UNFORMATTED
'</Table>\n
  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">\n
   <Unsynced/>
   <FitToPage/>
   <Print>
    <FitHeight>0</FitHeight>
   </Print>
   <Selected/>
   <ProtectObjects>False</ProtectObjects>
   <ProtectScenarios>False</ProtectScenarios>
  </WorksheetOptions>\n*/
 </Worksheet>\n
</Workbook>\n
'.

OUTPUT STREAM vvs CLOSE.

/*вывод через bispc*/
RUN sndbispc ("file=" + fname + ";class=bq").