/* ведомость поступлений за период по проданным кредитам */
/* kam */

{globals.i}
{tmprecid.def}

def var nameCl as char no-undo.
def var numAcct as char no-undo.
def var contCode as char no-undo.
define var summPost as decimal no-undo.
define var summPer as decimal no-undo.

{getdates.i}

  def new shared stream vvs.
  def var fname as char no-undo.
    
    fname = "./postper"  + "_" + userid('bisquit') + ".xml".
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
  <Created>2014-05-15T03:37:36Z</Created>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>4935</WindowHeight>
  <WindowWidth>18195</WindowWidth>
  <WindowTopX>0</WindowTopX>
  <WindowTopY>75</WindowTopY>
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
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="Fixed"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="wer">
  <Table x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="60.75"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="112.5"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="102"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="105.75"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="105"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="102"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="111"/>
   <Row ss:AutoFitHeight="0">
    <Cell><Data ss:Type="String">Клиент</Data></Cell>
    <Cell><Data ss:Type="String">Номер счета</Data></Cell>
    <Cell><Data ss:Type="String">Номер КД</Data></Cell>
    <Cell><Data ss:Type="String">Сумма поступлений</Data></Cell>
    <Cell><Data ss:Type="String">Сумма перечисления</Data></Cell>
    <Cell><Data ss:Type="String">C Даты</Data></Cell>
    <Cell><Data ss:Type="String">По Дату</Data></Cell>
   </Row>

'.

for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
	
	   find first loan-acct where loan-acct.contract = 'Кредит'
	       and loan-acct.cont-code = loan.cont-code
	       and loan-acct.acct-type = 'КредРасч'  no-lock no-error.
	   if avail loan-acct then do:   
	       summPost = 0.
	       summPer = 0.
	       RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT nameCl).
	       numAcct = loan-acct.acct.
	       contCode = loan.cont-code.
	       for each op-entry where
	           op-entry.op-status begins "√" and
               op-entry.op-date >= beg-date and
               op-entry.op-date <= end-date and
               op-entry.acct-cr = loan-acct.acct no-lock:
	           summPost = summPost + abs(op-entry.amt-rub).
           end.
           for each op-entry where
               op-entry.op-status begins "√" and
               op-entry.op-date >= beg-date and
               op-entry.op-date <= end-date and
               op-entry.acct-db = loan-acct.acct no-lock,
               first op of op-entry where op.op-kind = 'prkrperdo' no-lock:
               summPer = summPer + abs(op-entry.amt-rub).
           end.
          
      /*  put unformatted loan.cont-code ',' string(summPost) ',' string(summPer) skip. */
    
    put stream vvs unformatted
    '
    <Row ss:AutoFitHeight="0">
            <Cell><Data ss:Type="String">'.  
        put stream vvs unformatted nameCl + '</Data></Cell>
        <Cell><Data ss:Type="String">'.
        put stream vvs unformatted loan-acct.acct + '</Data></Cell>
        <Cell><Data ss:Type="String">'.
        put stream vvs unformatted contCode + '</Data></Cell>
        <Cell><Data ss:Type="Number">'.
        put stream vvs unformatted string(summPost) + '</Data></Cell>
        <Cell><Data ss:Type="Number">'.
        put stream vvs unformatted string(summPer) + '</Data></Cell>
        <Cell><Data ss:Type="DateTime">'.
        put stream vvs unformatted string(year(beg-date),"9999") + '-' + string(month(beg-date),"99") + '-' + string(day(beg-date),"99") + 'T00:00:00.000</Data></Cell>
        <Cell><Data ss:Type="DateTime">'.
        put stream vvs unformatted string(year(end-date),"9999") + '-' + string(month(end-date),"99") + '-' + string(day(end-date),"99") + 'T00:00:00.000</Data></Cell>
    </Row>
    '.
    end.
    
end.

put stream vvs unformatted 
'  </Table>
 </Worksheet>
</Workbook>
' .

output stream vvs close.
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
END.
  
  
  