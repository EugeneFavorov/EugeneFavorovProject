{globals.i}
/*
��� ���� ������ �� ���客� ��������
*/
def new shared stream puk.
def var fname as char  init "./ololo.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
DEF BUFFER m_signs FOR signs.

def temp-table fear NO-UNDO
	FIELD type AS CHARACTER 		/* ��� ���客���� */
    FIELD name AS CHARACTER 		/* ������������ ���客�� �������� */
    FIELD inn AS CHARACTER			/* ��� */
	FIELD kpp AS CHARACTER			/* ��� */
    FIELD phone AS CHARACTER		/* ����䮭 */
    FIELD fax AS CHARACTER			/* ���� */
    FIELD curacc as CHARACTER		/* ����� ���⭮�� ��� ���客�� �������� */
    FIELD namebank as CHARACTER		/* ������������ ����� ���客�� �������� */
	FIELD citybank as CHARACTER		/* ��த ����� ���客�� �������� */
    FIELD corraccbank AS CHARACTER	/* ���. ��� ����� ���客�� �������� */
	FIELD bankbic AS CHARACTER 		/* ��� ����� ���客�� �������� */
.	
	
/* ��� ��� ��. ���, � ������ ���⠢��� ���.४. ���� ���客����*/
FOR EACH cust-corp NO-LOCK, FIRST signs 
    WHERE STRING(cust-corp.cust-id) = signs.surrogate
    AND signs.file-name EQ "cust-corp"
    AND signs.code EQ "insurTypes"
    NO-LOCK BREAK BY signs.code-value:
	
	/* ᮧ����� ��ப� */
	CREATE fear.
	
	fear.type = signs.code-value.
	fear.name = cust-corp.name-short.
	fear.inn = cust-corp.inn.
/* ��� */
FIND LAST m_signs
	WHERE m_signs.code EQ "���"
	AND m_signs.file-name EQ "cust-corp"
	AND STRING(cust-corp.cust-id) = m_signs.surrogate
NO-LOCK NO-ERROR.
	IF AVAIL m_signs THEN DO:
		fear.kpp = m_signs.xattr-value.
	END.
/* ⥫�䮭 */	
FIND LAST tmpsigns
	WHERE tmpsigns.code EQ "tel"
	AND   tmpsigns.file-name EQ "cust-corp"
	AND   STRING(cust-corp.cust-id) = tmpsigns.surrogate
NO-LOCK NO-ERROR.
	
	fear.phone = tmpsigns.xattr-value.
	fear.fax = cust-corp.fax.
	fear.curacc = cust-corp.benacct.

/* ������������ ����� */
FIND FIRST banks-code
WHERE banks-code.bank-code = cust-corp.bank-code
AND banks-code.bank-code-type = "���-9"
NO-LOCK NO-ERROR.
    IF AVAIL banks-code THEN DO:
          FIND FIRST banks
		  WHERE banks.bank-id = banks-code.bank-id
		  NO-LOCK NO-ERROR.
          IF AVAIL banks THEN
			fear.namebank = banks.name.
			fear.citybank = banks.town-type + ". " + banks.town.
	END.		
	fear.corraccbank = cust-corp.corr-acct.
	fear.bankbic = cust-corp.bank-code.
	
END.

fname = "./insurance_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

output stream puk to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
put stream puk unformatted '<?xml version="1.0"?>
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
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
  </Style>
 </Styles>
 <Worksheet ss:Name="����1">
  <Table ss:ExpandedColumnCount="11" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:AutoFitWidth="0" ss:Width="63"/>
   <Column ss:AutoFitWidth="0" ss:Width="140.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="67.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="67.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="93"/>
   <Column ss:AutoFitWidth="0" ss:Width="73.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="147.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="114"/>
   <Column ss:AutoFitWidth="0" ss:Width="114"/>
   <Column ss:AutoFitWidth="0" ss:Width="108"/>
   <Row ss:AutoFitHeight="0" ss:Height="30.75">
    <Cell ss:StyleID="s65"><Data ss:Type="String">��� ���客����</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">������������ ���客�� ��������</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">���</Data></Cell>
	<Cell ss:StyleID="s65"><Data ss:Type="String">���</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">����䮭</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">����</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">����� ���⭮�� ��� ���客�� ��������</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">������������ ����� ���客�� ��������</Data></Cell>
	<Cell ss:StyleID="s65"><Data ss:Type="String">��த ����� ���客�� ��������</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">���. ��� ����� ���客�� ��������</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">��� ����� ���客�� ��������</Data></Cell>
   </Row>
'.   
FOR EACH fear NO-LOCK BY fear.type BY fear.name:

	PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="45">\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.type + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.name + '</Data></Cell>\n'. 
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.inn + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.kpp + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.phone + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.fax + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.curacc + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.namebank + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.citybank + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.corraccbank + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + fear.bankbic + '</Data></Cell>\n'.
	PUT STREAM puk UNFORMATTED '</Row>\n'.

END.

put stream puk unformatted
'
  </Table>
 </Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
/*---------------------------*/