/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  closaccyr.p
      Comment:  Ведмость счетов, не работающих 1 и 2 года.
         Uses:
      Used by:  -
      Created:  ayv
*/
{globals.i}

{getdates.i}
/*{setdest.i}*/


def new shared stream vvs.
def var strdate as char no-undo.
def var fname as char no-undo.
DEF VAR mTmpStr AS CHAR NO-UNDO.
DEF VAR ownr AS CHAR NO-UNDO.
DEF VAR datact AS DATE NO-UNDO.
DEF VAR dwnmonth AS DEC NO-UNDO.
DEF VAR strbeg AS CHAR NO-UNDO.
DEF VAR strend AS CHAR NO-UNDO.
DEF VAR cnt AS DEC NO-UNDO.


DEF TEMP-TABLE op-tmp
	FIELD Details AS CHAR
	FIELD amt-rub AS DEC
	FIELD acctcr AS CHAR.
  
  strbeg = string(beg-date,"99.99.9999").
  strend = string(end-date,"99.99.9999"). 
	if beg-date = end-date THEN
  strdate = strbeg.
  ELSE 
  strdate = "период с " + strbeg + " по " + strend.
FOR EACH op WHERE
op.op-kind EQ "VOK008"
 and op.op-date ge beg-date and op.op-date le end-date
NO-LOCK:
	
	FIND FIRST op-entry WHERE op-entry.op = op.op
NO-LOCK NO-ERROR.
IF AVAIL (op-entry) THEN DO:

	CREATE op-tmp.
				ASSIGN
					 op-tmp.Details = op.Details
					 op-tmp.amt-rub = op-entry.amt-rub
					 op-tmp.acctcr = op-entry.acct-cr.
 END.
 END.
 fname = "./reestr_t_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".

 output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
put stream vvs unformatted 
'<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"\n
 xmlns:o="urn:schemas-microsoft-com:office:office"\n
 xmlns:x="urn:schemas-microsoft-com:office:excel"\n
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"\n
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
   <Alignment ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="m76396836">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="s63">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="s66">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Center"/>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="s69">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="s70">\n
   <Alignment ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="s73">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="s80">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="s81">\n
   <Alignment ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="s84">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
  </Style>\n  
  <Style ss:ID="s85">\n
   <Alignment ss:Vertical="Center"/>\n
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Size="8"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
 </Styles>\n
 <Worksheet ss:Name="ДС 7">\n
  <Table ss:ExpandedColumnCount="26" ss:ExpandedRowCount="50" x:FullColumns="1"\n
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:Index="2" ss:AutoFitWidth="0" ss:Width="69"/>\n
   <Column ss:Width="12"/>\n
   <Column ss:Width="9.75" ss:Span="8"/>\n
   <Column ss:Index="13" ss:AutoFitWidth="0" ss:Width="33.75"/>\n\n
   <Column ss:Hidden="1" ss:AutoFitWidth="0" ss:Span="1"/>\n
   <Column ss:Index="16" ss:Width="9.75" ss:Span="8"/>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s63"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><B><Font html:Color="#000000">Реестр принятых платежей по </Font><Font
        html:Size="11" html:Color="#000000">МАДОУ "Детский сад № 7"</Font></B></ss:Data></Cell>\n

   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
   <Cell ss:MergeAcross="25" ss:StyleID="s63"><Data ss:Type="String">за ' + strdate + '</Data></Cell>\n
    
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">Наименование получателя платежа</Font><B><Font
        html:Color="#000000">:  МАДОУ "Детский сад № 7 присмотра и оздоровления г.Тюмени" </Font></B></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="19" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">р/с </Font><B><Font
        html:Color="#000000">№</Font></B><Font html:Color="#000000"> </Font><B><Font
        html:Color="#000000">40703810200020000267 </Font></B><Font
       html:Color="#000000"> в &quot;Запсибкомбанк&quot; ПАО г.Тюмень</Font></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">БИК 047102613</Data></Cell>\n
    <Cell ss:StyleID="s70"><Data ss:Type="String">к/с 30101810271020000613</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s73"><Data ss:Type="String">Администратор дохода: ИНН </Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="String">&#160;7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">8</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="m76396836"><Data ss:Type="String">КПП</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">3</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">Наименование платежа:</Data></Cell>\n
    <Cell ss:StyleID="s81"><Data ss:Type="String">Дополнительные услуги</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s84"/>\n
    <Cell ss:StyleID="s85"><Data ss:Type="String">Родительская плата</Data></Cell>\n
   </Row>\n
   <Row>\n
    <Cell/>\n
    <Cell/>\n
	</Row>\n'.
  
  cnt = 0.
FOR EACH op-tmp WHERE op-tmp.acctcr EQ '40911810105300010003     @0300' NO-LOCK:
cnt = cnt + 1.

  
  PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
  <Cell ss:StyleID="s66"><Data ss:Type="String">' + string(cnt) + '</Data></Cell>\n
  <Cell ss:StyleID="s66"><Data ss:Type="Number">' + string(op-tmp.amt-rub) + '</Data></Cell>\n
  <Cell ss:StyleID="s66" ss:MergeAcross="22"><Data ss:Type="String">' + op-tmp.Details + '</Data></Cell>\n

  </Row>\n
  '.
END.
PUT STREAM vvs UNFORMATTED
'<Row>
  <Cell ss:StyleID="s66"><Data ss:Type="String">Итого</Data></Cell>
  <Cell ss:Index="2" ss:Formula="=SUM(R[-7]C:R[-1]C)"><Data ss:Type="Number"></Data></Cell>
  </Row>'.

/*вторая страница*/
PUT STREAM vvs UNFORMATTED
'</Table>\n
</Worksheet>\n
<Worksheet ss:Name="ДС 73 ДО">\n
  <Table ss:ExpandedColumnCount="26" ss:ExpandedRowCount="50" x:FullColumns="1"\n
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:Index="2" ss:AutoFitWidth="0" ss:Width="69"/>\n
   <Column ss:Width="12"/>\n
   <Column ss:Width="9.75" ss:Span="8"/>\n
   <Column ss:Index="13" ss:AutoFitWidth="0" ss:Width="33.75"/>\n\n
   <Column ss:Hidden="1" ss:AutoFitWidth="0" ss:Span="1"/>\n
   <Column ss:Index="16" ss:Width="9.75" ss:Span="8"/>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s63"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><B><Font html:Color="#000000">Реестр принятых платежей по </Font><Font
        html:Size="11" html:Color="#000000">МАДОУ "Детский сад № 73"</Font></B></ss:Data></Cell>\n
  
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
   <Cell ss:MergeAcross="25" ss:StyleID="s63"><Data ss:Type="String">за ' + strdate + '</Data></Cell>\n
    
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">Наименование получателя платежа</Font><B><Font
        html:Color="#000000">:  МАДОУ "Детский сад № 73" </Font></B></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="19" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">р/с </Font><B><Font
        html:Color="#000000">№</Font></B><Font html:Color="#000000"> </Font><B><Font
        html:Color="#000000">40703810602994001136 </Font></B><Font
       html:Color="#000000"> в &quot;Запсибкомбанк&quot; ПАО г.Тюмень</Font></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">БИК 047102613</Data></Cell>\n
    <Cell ss:StyleID="s70"><Data ss:Type="String">к/с 30101810271020000613</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s73"><Data ss:Type="String">Администратор дохода: ИНН </Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="String">&#160;7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">8</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">5</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="m76396836"><Data ss:Type="String">КПП</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">Наименование платежа:</Data></Cell>\n
    <Cell ss:StyleID="s81"><Data ss:Type="String">Дополнительные услуги</Data></Cell>\n
   </Row>\n
   
   <Row>\n
    <Cell/>\n
    <Cell/>\n

  </Row>\n
  '.
  cnt = 0.
  FOR EACH op-tmp WHERE op-tmp.acctcr EQ '40911810805300010002     @0300' NO-LOCK:
  cnt = cnt + 1.
  
  PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
  <Cell ss:StyleID="s66"><Data ss:Type="String">' + string(cnt) + '</Data></Cell>\n
  <Cell ss:StyleID="s66"><Data ss:Type="Number">' + string(op-tmp.amt-rub) + '</Data></Cell>\n
  <Cell ss:StyleID="s66" ss:MergeAcross="23"><Data ss:Type="String">' + op-tmp.Details + '</Data></Cell>\n
  </Row>\n'.
END.
PUT STREAM vvs UNFORMATTED
'<Row>
  <Cell ss:StyleID="s66"><Data ss:Type="String">Итого</Data></Cell>
  <Cell ss:Index="2" ss:Formula="=SUM(R[-7]C:R[-1]C)"><Data ss:Type="Number"></Data></Cell>
  </Row>'.
/*Третья страница*/
PUT STREAM vvs UNFORMATTED
'</Table>\n
</Worksheet>\n
<Worksheet ss:Name="ДС 73 РП">\n
  <Table ss:ExpandedColumnCount="26" ss:ExpandedRowCount="50" x:FullColumns="1"\n
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:Index="2" ss:AutoFitWidth="0" ss:Width="69"/>\n
   <Column ss:Width="12"/>\n
   <Column ss:Width="9.75" ss:Span="8"/>\n
   <Column ss:Index="13" ss:AutoFitWidth="0" ss:Width="33.75"/>\n\n
   <Column ss:Hidden="1" ss:AutoFitWidth="0" ss:Span="1"/>\n
   <Column ss:Index="16" ss:Width="9.75" ss:Span="8"/>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s63"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><B><Font html:Color="#000000">Реестр принятых платежей по </Font><Font
        html:Size="11" html:Color="#000000">МАДОУ "Детский сад № 73"</Font></B></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
   <Cell ss:MergeAcross="25" ss:StyleID="s63"><Data ss:Type="String">за ' + strdate + '</Data></Cell>\n
    
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="25" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">Наименование получателя платежа</Font><B><Font
        html:Color="#000000">:  МАДОУ "Детский сад № 73" </Font></B></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="19" ss:StyleID="s66"><ss:Data ss:Type="String"\n
      xmlns="http://www.w3.org/TR/REC-html40"><Font html:Color="#000000">р/с </Font><B><Font
        html:Color="#000000">№</Font></B><Font html:Color="#000000"> </Font><B><Font
        html:Color="#000000">40703810002994001135 </Font></B><Font
       html:Color="#000000"> в &quot;Запсибкомбанк&quot; ПАО г.Тюмень</Font></ss:Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">БИК 047102613</Data></Cell>\n
    <Cell ss:StyleID="s70"><Data ss:Type="String">к/с 30101810271020000613</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s73"><Data ss:Type="String">Администратор дохода: ИНН </Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="String">&#160;7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">8</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">5</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="m76396836"><Data ss:Type="String">КПП</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">7</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">2</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">0</Data></Cell>\n
    <Cell ss:StyleID="s80"><Data ss:Type="Number">1</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="s69"><Data ss:Type="String">Наименование платежа:</Data></Cell>\n
    <Cell ss:MergeAcross="1" ss:StyleID="s84"/>\n
    <Cell ss:StyleID="s85"><Data ss:Type="String">Родительская плата</Data></Cell>\n
   </Row>\n
   <Row>\n
    <Cell/>\n
    <Cell/>\n
  </Row>\n
  '.
  cnt = 0.
  FOR EACH op-tmp WHERE op-tmp.acctcr EQ '40911810505300010001     @0300' NO-LOCK:
  cnt = cnt + 1.
  
  PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
  <Cell ss:StyleID="s66"><Data ss:Type="String">' + string(cnt) + '</Data></Cell>\n
  <Cell ss:StyleID="s66"><Data ss:Type="Number">' + string(op-tmp.amt-rub) + '</Data></Cell>\n
  <Cell ss:StyleID="s66" ss:MergeAcross="23"><Data ss:Type="String">' + op-tmp.Details + '</Data></Cell>\n
  </Row>\n'.
END.
PUT STREAM vvs UNFORMATTED
'<Row>
  <Cell ss:StyleID="s66"><Data ss:Type="String">Итого</Data></Cell>
  <Cell ss:Index="2" ss:Formula="=SUM(R[-7]C:R[-1]C)"><Data ss:Type="Number"></Data></Cell>
  </Row>'.

put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
  
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").