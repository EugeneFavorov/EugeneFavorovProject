/* реестр в первое коллекторское */
DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{tmprecid.def}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR sName AS CHAR NO-UNDO.
DEFINE VAR sAddr AS CHAR NO-UNDO.
DEFINE VAR fName AS CHAR NO-UNDO.
DEFINE VAR ffName AS CHAR NO-UNDO.
DEFINE VAR sCommand AS CHAR NO-UNDO.
DEFINE VAR AcctCesRS AS CHAR NO-UNDO.
DEFINE VAR iCount AS INT64 NO-UNDO.
DEFINE VAR dSum AS DECIMAL NO-UNDO.
DEFINE VAR sContCOde AS CHAR NO-UNDO.
DEF VAR sIdLoan AS CHAR NO-UNDO.
DEF NEW SHARED STREAM vvs.

end-date = today - 1.
{getdate.i
   &noinit = "/*"}

iCount = 0.
dSum = 0.

DEF NEW SHARED STREAM vvs.

/* выводим в ексель */
    fname = "./reestrday1k"  + "_" + USERID('bisquit') + ".xml".
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

   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">№ п/п</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО заемщика</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер КД</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма к перечислению</Data></Cell>
   </Row>
'.


FOR EACH op WHERE
    op.op-date = end-date 
    AND (op.op-kind = 'perpkb')
    AND op.op-status BEGINS "√"   
    NO-LOCK,
    EACH op-entry OF op 
    WHERE 

 op-entry.acct-db begins '40817'
    AND op-entry.acct-cr begins '47422'
    NO-LOCK:
        
    sContCode = GetXattrValueEx("op",
                string(op.op),
                "cont-code",
                 "").

        IF INDEX(sContCode,"@") = 0 AND sContCode <> '' THEN DO:
            sContCode = sContCode + '@' + op.filial-id.
        END.        
        sIdLoan = ''.    
        FIND FIRST loan where loan.contract = 'Кредит' AND loan.cont-code = sContCode no-lock no-error.
        IF AVAIL loan then do:
        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
            AND loan-acct.acct-type = 'КредРасч'
            AND loan-acct.cont-code = sContCode
            AND loan-acct.acct = op-entry.acct-db NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            FIND FIRST loan WHERE loan.contract = loan-acct.contract 
                AND loan.cont-code = loan-acct.cont-code NO-LOCK NO-ERROR.

            RUN GetName( INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT sName).

            iCount = iCount + 1.
            dSum = dSum + op-entry.amt-rub.

            PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED string(iCount) + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED sName + '</Data></Cell>\n'.    
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED loan.doc-ref + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED STRING(op-entry.amt-rub,"->>>>>>>>>9.99") + '</Data></Cell>\n'.

            PUT STREAM vvs UNFORMATTED '</Row>'.
    
        END.
    END. 
END. 
            PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED '' + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED '' + '</Data></Cell>\n'.    
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED 'Итого' + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED STRING(dSum,"->>>>>>>>>9.99") + '</Data></Cell>\n'.

            PUT STREAM vvs UNFORMATTED '</Row>'.

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").


{intrface.del}

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





