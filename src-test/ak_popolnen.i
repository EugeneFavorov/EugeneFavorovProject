def var fname as char no-undo.
   def new shared stream vvs.

    
    fname = "./Aktsiya"  + "_" + userid('bisquit') + ".xml".
   
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
   <Interior ss:Color="#963634" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="0"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="journal">
  <Table ss:ExpandedColumnCount="30" ss:ExpandedRowCount="10000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="50"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="50"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="50"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="50"/>
   <Column ss:StyleID="s63" ss:Width="140"/>
   <Column ss:StyleID="s63" ss:Width="70"/>
   <Column ss:StyleID="s63" ss:Width="100"/>
   <Column ss:StyleID="s63" ss:Width="50"/>
   <Column ss:StyleID="s63" ss:Width="50"/>
   <Column ss:StyleID="s63" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="70"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="200"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100"/>'.


PUT STREAM VVS UNFORMATTED '<Row>
		<Cell><Data ss:Type="String">Дата отчета</Data></Cell>
		<Cell><Data ss:Type="String">Дата Отк счета</Data></Cell>
		<Cell><Data ss:Type="String">Дата Зак счета</Data></Cell>
		<Cell><Data ss:Type="String">Код клиента</Data></Cell>
		<Cell><Data ss:Type="String">Имя клиента</Data></Cell>
		<Cell><Data ss:Type="String">Остаток на момент акции</Data></Cell>
		<Cell><Data ss:Type="String">Остаток на дату</Data></Cell>
		<Cell><Data ss:Type="String">Номер документа</Data></Cell>
		<Cell><Data ss:Type="String">Дата документа</Data></Cell>
		<Cell><Data ss:Type="String">Дебет</Data></Cell>
		<Cell><Data ss:Type="String">Кредит</Data></Cell>
		<Cell><Data ss:Type="String">Сумма в рублях</Data></Cell>
		<Cell><Data ss:Type="String">Назначение</Data></Cell>
		<Cell><Data ss:Type="String">Тип вклада</Data></Cell>
		<Cell><Data ss:Type="String">Номер вклада</Data></Cell>
		<Cell><Data ss:Type="String">Подразделение</Data></Cell>
		<Cell><Data ss:Type="String">Источник привлечения</Data></Cell>
		<Cell><Data ss:Type="String">Номер телефона</Data></Cell>

		</Row>
		'.

FOR EACH tt-a NO-LOCK:

PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(today) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.OtkAc) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.ZakAc) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.CustId) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.CustName) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.OstPod) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.OstTek) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.docnum) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.dDoc) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.acct-db) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.acct-cr) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.amt) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.det) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.ContType) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.ContCode) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.kko) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.IstPriv) + '</Data></Cell>
		<Cell ' + tt-a.bol + '><Data ss:Type="String">' + string(tt-a.Phone) + '</Data></Cell>

		</Row>
		'.

END.
 put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").  