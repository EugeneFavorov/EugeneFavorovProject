
/* отчет по блокировкам 40817 */
/* zss */


def var sName     as char no-undo.
def var sContCode as char no-undo.
def var dDateLoan as date no-undo.
def var myStartDate as date no-undo.
def var myEndDate as date no-undo.
def var strTrans as char no-undo format "x(30)".
def var strTrans1 as char no-undo.
def var strTrans2 as char no-undo.
def var dOst as decimal no-undo. 
def var UserFIO as decimal no-undo. 

{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE TEMP-TABLE tt-otchdg
        FIELD sFio        AS CHAR
        FIELD sAcct       AS CHAR        
        FIELD sContCode   AS CHAR                 /* номер договора */
        FIELD sOst        AS DECIMAL
        FIELD sBlock      AS DECIMAL
        FIELD UserFIO     AS CHAR  
        FIELD dateBlock   AS DATETIME
        INDEX sContCode sContCode       
        
    .

{getdate.i}

{empty tt-otchdg}

FOR EACH blockobject WHERE
   (blockobject.end-datetime EQ ? OR blockobject.end-datetime >= datetime(end-date))
   AND blockobject.class-code EQ 'BlockAcct'
   AND blockobject.block-type EQ 'БлокСумм'
   AND blockobject.file-name EQ 'acct'
   AND blockobject.surrogate BEGINS '40817'
   NO-LOCK:
       
   FIND FIRST acct WHERE
        acct.acct = ENTRY(1,blockobject.surrogate) NO-lOCK NO-ERROR.
   IF AVAIL acct AND (acct.close-date = ? or acct.close-date >= end-date) THEN do:     
    sName = ''.
    dOst = 0.
    sContCode = ''.
    	RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT sName).
			RUN acct-pos IN h_base (acct.acct, ENTRY(2,blockobject.surrogate), end-date, end-date, ?).
			    dOst = abs(sh-bal).
		
        FIND LAST loan-acct
		        WHERE loan-acct.acct BEGINS acct.acct 
		        AND loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
		    IF AVAIL loan-acct THEN sContCode = ENTRY(1,loan-acct.cont-code,'@').
		          
	       create tt-otchdg.
   

	     	assign
    			tt-otchdg.sFio      = sName
    			tt-otchdg.sAcct     = acct.acct
    			tt-otchdg.sContCode = sContCode
    			tt-otchdg.sOst      = dOst
    			tt-otchdg.sBlock    = abs(blockobject.val[3])
    			tt-otchdg.dateBlock = blockobject.beg-datetime
    		  tt-otchdg.UserFIO   = GetXattrValueEx("_user",blockobject.user-id,"ФИОП",'')
        .
    END.   
   
END.       
 
def var fname as char no-undo.
DEF NEW SHARED STREAM vvs.

/* выводим в ексель */
    fname = "./otchblock"  + "_" + USERID('bisquit') + ".xml".
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
  <Table >
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Клиент</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер счета</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">БлокСумм</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата установления блокировки</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Пользователь уст. блокировку</Data></Cell>
   </Row>
'.

FOR EACH tt-otchdg NO-LOCK:
  IF(tt-otchdg.sContCode NE '') THEN do:    
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED tt-otchdg.sFio + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED tt-otchdg.sAcct + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED tt-otchdg.sContCode + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(tt-otchdg.sOst,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(tt-otchdg.sBlock,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED STRING(YEAR(tt-otchdg.dateBlock),"9999") + '-' + STRING(MONTH(tt-otchdg.dateBlock),"99") + '-' + STRING(DAY(tt-otchdg.dateBlock),"99") + 'T00:00:00.000' + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED tt-otchdg.UserFIO + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED '</Row>'.
  end.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").

PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END PROCEDURE.

 
 
