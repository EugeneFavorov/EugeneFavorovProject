/* ��������� �� ��࠭�� */
/* kam */

{globals.i}
{tmprecid.def}
{intrface.get xclass}

def var nameCl as char no-undo.
def var sCont-code as char no-undo.
def var summg as decimal no-undo.
def var summ409 as decimal no-undo.
def var date409 as date no-undo.
def var nameAg as char no-undo.

{getdates.i}

DEFINE TEMP-TABLE otchgar
        FIELD cont_code    AS CHAR                 /* ����� �� */
        FIELD open_date    AS DATE                 /* ��� �� */
        FIELD name_cl      AS CHAR                 /* ��� ����騪� */
        FIELD summ_g       AS DECIMAL              /* �㬬� ��࠭⨨ */
        FIELD summ_409     AS DECIMAL              /* �㬬� 409 */
        FIELD date_409     AS DATE                 /* ��� 409 */
        FIELD name_ag      AS CHAR 
        INDEX cont_code cont_code       
    .

{empty otchgar}


  def new shared stream vvs.
  def var fname as char no-undo.
    
    fname = "./otchgar"  + "_" + userid('bisquit') + ".xml".


/* �� �⮡࠭�� �� */
for each loan where loan.open-date >= beg-date and loan.open-date <= end-date
    and loan.class-code = 'loan-guarantee'
	no-lock:
	    
            /* ����砥� ��� ������ */
            RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT nameCl).
            
            FIND FIRST loan-int WHERE 
                loan-int.contract = loan.contract
                AND loan-int.cont-code = loan.cont-code
                AND loan-int.id-d = 5
                AND loan-int.id-k = 174
                NO-LOCK NO-ERROR.
            IF AVAIL loan-int THEN DO:
                summ409 = loan-int.amt-rub.
                date409 = loan-int.mdate.
            END.
            ELSE DO:
                summ409 = 0.
                date409 = ?.
            END.
            FIND FIRST loan-int WHERE 
                loan-int.contract = loan.contract
                AND loan-int.cont-code = loan.cont-code
                AND loan-int.id-d = 47
                AND loan-int.id-k = ?
                NO-LOCK NO-ERROR.
            IF AVAIL loan-int THEN DO:
                summg = loan-int.amt-rub.
            END.
            ELSE DO:
                summg = 0.
            END.
            
            nameAg = GetXAttrValueEx("loan",loan.contract + ',' + loan.cont-code,"agent","").
            CREATE otchgar.            
            assign
                otchgar.open_date = loan.open-date
                otchgar.cont_code = loan.doc-ref
                otchgar.name_cl = nameCl
                otchgar.summ_g = summg                
                otchgar.summ_409 = summ409
                otchgar.date_409 = date409
                otchgar.name_ag = nameAg
                .
            release otchgar. 
            
end.

output stream vvs to value (fname)
	UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
put stream vvs unformatted 
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
  <Table >
  
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="200.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>         
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>      


   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
   
    <Cell ss:StyleID="s66"><Data ss:Type="String">������������ �࣠����樨 (�ਭ樯��)</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">� ��࠭⨨</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">��� �뤠�</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">�㬬� ��࠭⨨</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">�㬬� �����ᨨ, ��.</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">��� ������ �����ᨨ</Data></Cell>    
    <Cell ss:StyleID="s66"><Data ss:Type="String">������������ �����</Data></Cell>

   </Row>

'.

    FOR EACH otchgar by otchgar.open_date:
    put stream vvs unformatted
    '
    <Row ss:AutoFitHeight="0">
        <Cell><Data ss:Type="String">'.  
        put stream vvs unformatted otchgar.name_cl + '</Data></Cell>'.
        put stream vvs unformatted '<Cell><Data ss:Type="String">'.  
        put stream vvs unformatted otchgar.cont_code + '</Data></Cell>'.
		
		/**/
        IF otchgar.open_date <> ? THEN
			DO:   
				put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchgar.open_date),"9999") + '-' + string(month(otchgar.open_date),"99") + '-' + string(day(otchgar.open_date),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.    
        ELSE
		put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
		IF otchgar.summ_g <> ? AND otchgar.summ_g > 0 THEN DO:
        	put stream vvs unformatted string(otchgar.summ_g,"->>>>>>>>>9.99").
		END.
		put stream vvs unformatted '</Data></Cell>'. 
		/**/
		/**/
		put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
		IF otchgar.summ_409 <> ? AND otchgar.summ_409 > 0 THEN DO:
        	put stream vvs unformatted string(otchgar.summ_409,"->>>>>>>>>9.99").
		END.
		put stream vvs unformatted '</Data></Cell>'. 
		/**/
        IF otchgar.date_409 <> ? THEN
			DO:   
				put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchgar.date_409),"9999") + '-' + string(month(otchgar.date_409),"99") + '-' + string(day(otchgar.date_409),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.    
        ELSE
		    put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchgar.name_ag + '</Data></Cell>'.
		put stream vvs unformatted '</Row>'.
    END.

put stream vvs unformatted 
'  </Table>
 </Worksheet>
</Workbook>
' .

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").

/**/
PROCEDURE GetName:
DEF INPUT PARAMETER cat AS CHARACTER.
DEF INPUT PARAMETER id AS INT64.
DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "�" THEN
	DO:
		FIND FIRST PERSON 
		WHERE PERSON.PERSON-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL PERSON THEN
				DO:
					/* ��� ������ */
					sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
				END.
	END.
 ELSE
	DO:
		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
				DO:
					/* ������������ �࣠����樨 */
					sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
				END.
	END.
END.
  
  
  