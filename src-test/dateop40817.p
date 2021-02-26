{globals.i}
    /* {setdest.i} */

{tmprecid.def}



DEF VAR tmpdate AS DATE NO-UNDO.
def new shared stream puk.
def var fname1 as char no-undo.
 fname1 = "./40817_" + ".xml". /*  + string(DATE(today)) + */ 


      output stream puk to value (fname1)
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
        <WindowHeight>7740</WindowHeight>
        <WindowWidth>13395</WindowWidth>
        <WindowTopX>120</WindowTopX>
        <WindowTopY>45</WindowTopY>
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
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
        </Style>
        <Style ss:ID="s63">
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
         <NumberFormat ss:Format="Fixed"/>
        </Style>
        <Style ss:ID="s64">
         <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
        </Style>
        <Style ss:ID="s65">
         <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
        </Style>
        <Style ss:ID="s66">
         <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
         <NumberFormat ss:Format="Fixed"/>
        </Style>
        <Style ss:ID="s67">
         <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
        </Style>
        <Style ss:ID="s68">
         <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
        </Style>
        <Style ss:ID="s69">
         <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
         <Borders>
          <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
          <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
         </Borders>
         <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
          ss:Size="11" ss:Color="#000000"/>
         <NumberFormat ss:Format="Fixed"/>
        </Style>
       </Styles>
       <Worksheet ss:Name="post">
        <Table x:FullColumns="1"
         x:FullRows="1" ss:StyleID="s62" ss:DefaultRowHeight="15">
         <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="150.25"/>
         <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="150.25"/>
         <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.25"/>
         <Row ss:AutoFitHeight="0" ss:Height="31.5">
         
          <Cell ss:StyleID="s66"><Data ss:Type="String">Номер КД</Data></Cell>
          <Cell ss:StyleID="s66"><Data ss:Type="String">Счет</Data></Cell>
          <Cell ss:StyleID="s66"><Data ss:Type="String">Дата</Data></Cell>
         </Row>
         '.

FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:
    FIND LAST loan-acct OF loan
        WHERE loan-acct.acct-type = "КредРасч" NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        tmpdate = DATE("01/01/1900").
        FOR EACH op-entry WHERE
        (op-entry.op-status = "√" OR op-entry.op-status = "√√") AND
        (op-entry.acct-db = loan-acct.acct OR op-entry.acct-cr = loan-acct.acct) NO-LOCK BY op-entry.op-date DESC:
            tmpdate = op-entry.op-date.
            LEAVE.
        END.
      

PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + loan.doc-ref + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + TRIM(ENTRY(1,loan-acct.acct,'@')) + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + STRING(tmpdate) + '</Data></Cell>\n'.

                                        PUT STREAM puk UNFORMATTED '</Row>\n'.  

        PUT UNFORMATTED loan.doc-ref + '     ' + TRIM(ENTRY(1,loan-acct.acct,'@')) + '     ' + STRING(tmpdate) SKIP.
    END.
END.
    
    /* {preview.i}
 */

put stream puk unformatted
        '
                  </Table>
                </Worksheet>
         </Workbook>
        '.
output stream puk close.

RUN sndbispc ("file=" + fname1 + ";class=bq").