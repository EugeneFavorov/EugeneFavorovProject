
   def new shared stream vvs.

    
    fname = "./VedProv"  + "_" + userid('bisquit') + ".xml".
   
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
   <NumberFormat ss:Format="@"/>
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
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="10000000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="58"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="83"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="92"/>
   <Column ss:StyleID="s66" ss:Width="82"/>
   <Column ss:StyleID="s65" ss:Width="190"/>
   <Column ss:StyleID="s63" ss:Width="100"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>'.

   put stream vvs unformatted '<Row>
				<Cell ss:Index="5"><Data ss:Type="String">' + STRING('Приложение 1') + '</Data></Cell>
				</Row>
				' +
				'<Row>
				<Cell ss:Index="5"><Data ss:Type="String">' + STRING('К приказу № 333 от 25.12.2013') + '</Data></Cell>
				</Row>
				' +
	'<Row ss:AutoFitHeight="0">
	<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Ведомость проводок пользователя</Data></Cell>
	</Row>
	'.

PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING('За') + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(end-date) + '</Data></Cell>
			</Row>
			'.
PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING('ФИО') + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(zUserName) + '</Data></Cell>
			</Row>
			'.
PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING('Должность') + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(zUserDol) + '</Data></Cell>
			</Row>
			'.
PUT STREAM VVS UNFORMATTED '<Row>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Дебет') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Кредит') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Сумма') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Дата создания') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Дата опердня') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Номер документа') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Код транзакции') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Индивидуальный номер документа') + '</Data></Cell>
			</Row>
			'.

PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Хранятся в бумажном виде</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Балансовые</Data></Cell>
		</Row>
		'.
FOR EACH tt-op-day NO-LOCK
	WHERE tt-op-day.acct-cat EQ "b"
        AND tt-op-day.save-type EQ "p"
	AND tt-op-day.user-id eq zUsId
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-db) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-cr) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.amt-rub) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(end-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op-kind) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op) + '</Data></Cell>
			</Row>
			'.
	mItog = mItog + tt-op-day.amt-rub.
END.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="1" ss:StyleID="s70"><Data ss:Type="String">Итог</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(mItog) + '</Data></Cell>
		</Row>
		'.
mItog = 0.



PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Внебалансовые</Data></Cell>
		</Row>
		'.
       FOR EACH tt-op-day NO-LOCK
            WHERE tt-op-day.acct-cat EQ "o"
            AND tt-op-day.acct-cat EQ "b"
            AND tt-op-day.save-type EQ "p"
	AND tt-op-day.user-id EQ zUsId
            BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
            :
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-db) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-cr) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.amt-rub) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(end-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op-kind) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op) + '</Data></Cell>
			</Row>
			'.
	mItog = mItog + tt-op-day.amt-rub.
END.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="1" ss:StyleID="s70"><Data ss:Type="String">Итог</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(mItog) + '</Data></Cell>
		</Row>
		'.
mItog = 0.



PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Хранятся в электронном документе</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Балансовые</Data></Cell>
		</Row>
		'.
    FOR EACH tt-op-day 
        WHERE tt-op-day.acct-cat EQ "b"
        AND tt-op-day.save-type EQ "e"
	AND tt-op-day.user-id EQ zUsId
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-db) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-cr) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.amt-rub) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(end-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op-kind) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op) + '</Data></Cell>
			</Row>
			'.
	mItog = mItog + tt-op-day.amt-rub.
END.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="1" ss:StyleID="s70"><Data ss:Type="String">Итог</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(mItog) + '</Data></Cell>
		</Row>
		'.
mItog = 0.



PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="2" ss:StyleID="s70"><Data ss:Type="String">Внебалансовые</Data></Cell>
		</Row>
		'.
    FOR EACH tt-op-day 
        WHERE tt-op-day.acct-cat EQ "o"
        AND tt-op-day.save-type EQ "e"
	AND tt-op-day.user-id EQ zUsId
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-db) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.acct-cr) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.amt-rub) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(end-date) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op-kind) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(tt-op-day.op) + '</Data></Cell>
			</Row>
			'.
	mItog = mItog + tt-op-day.amt-rub.
END.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:MergeAcross="1" ss:StyleID="s70"><Data ss:Type="String">Итог</Data></Cell>
		</Row>
		'.
PUT STREAM VVS UNFORMATTED '<Row>
		<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(mItog) + '</Data></Cell>
		</Row>
		'.

PUT STREAM VVS UNFORMATTED '<Row></Row>
			<Row>
			<Cell><Data ss:Type="String">' + STRING(zUserName) + '</Data></Cell>
			<Cell><Data ss:Type="String">__________</Data></Cell>
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



























