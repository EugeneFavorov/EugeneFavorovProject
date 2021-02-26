/* Для отчета "Контроль ручных проводок" handdoc.p*/

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
  <Table ss:ExpandedColumnCount="7" ss:ExpandedRowCount="500" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:AutoFitWidth="0" ss:Width="130"/>
   <Column ss:AutoFitWidth="0" ss:Width="130"/>
   <Column ss:AutoFitWidth="0" ss:Width="90"/>
   <Column ss:AutoFitWidth="0" ss:Width="350"/>
   <Column ss:AutoFitWidth="0" ss:Width="100"/>
<Row>\n
   <Cell ss:StyleID="Default" ss:MergeAcross="1"><Data ss:Type="String">Контроль ручных проводок</Data></Cell>\n
</Row>\n   
<Row>\n
   <Cell ss:StyleID="Default"><Data ss:Type="String">' + strdate + '</Data></Cell>\n
</Row>\n
<Row>\n
</Row>\n
<Row ss:Height="45">\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Дата операции</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Номер документа</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Счет дебета</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Счет кредита</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Сумма</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Назначение платежа</Data></Cell>\n
   <Cell ss:StyleID="up_b"><Data ss:Type="String">Имя пользователя</Data></Cell>\n
</Row>\n
'.

FOR EACH tt-op
  BREAK BY tt-op.date-op:
       PUT STREAM vvs UNFORMATTED 
      '<Row ss:AutoFitHeight="0">\n
       <Cell ss:StyleID="left_b"><Data ss:Type="String">' + STRING(tt-op.date-op,"99.99.9999") + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-op.doc-num) + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-op.acct-db) + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-op.acct-cr) + '</Data></Cell>\n
       <Cell ss:StyleID="right"><Data ss:Type="String">' + STRING(tt-op.amt,"->,>>>,>>>,>>>,>>9.99") + '</Data></Cell>\n
       <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(tt-op.details) + '</Data></Cell>\n
       <Cell ss:StyleID="right"><Data ss:Type="String">' + STRING(tt-op.user-name) + '</Data></Cell>\n
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