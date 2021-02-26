
DEFINE INPUT  PARAMETER iParams AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mDet AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumA  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumP  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumAVal  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumPVal  AS DECIMAL     NO-UNDO.
     
DEFINE BUFFER bacct FOR acct.

def new shared stream puk.
DEFINE VARIABLE fname as char   no-undo.

{getdate.i &DateLabel       = "Дата"
              &DateHelp        = "Дата (F1 - Календарь)"
              &return         = "return 'Пользователь прервал ввод'"
              }
              
   {justasec} 

fname = "./Acct_kass" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml". 

output stream puk to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
    
  /*  UNBUFFERED CONVERT TARGET "1251".*/
put stream puk unformatted '<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <LastSaved>2014-04-14T03:05:08Z</LastSaved>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>27795</WindowWidth>
  <WindowTopX>675</WindowTopX>
  <WindowTopY>3300</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Swiss" ss:Size="16"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="res">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s61">
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
  <Style ss:ID="s62">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
    <NumberFormat ss:Format="0.00"/>    
  </Style>    
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table ss:ExpandedColumnCount="15" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
       
   <Column ss:AutoFitWidth="0" ss:Width="50"/>
   <Column ss:AutoFitWidth="0" ss:Width="210.0"/>     
   <Column ss:AutoFitWidth="0" ss:Width="75"/>
   <Column ss:AutoFitWidth="0" ss:Width="500"/>
   <Column ss:AutoFitWidth="0" ss:Width="65"/> 
   <Column ss:AutoFitWidth="0" ss:Width="150.75"/> 
   <Column ss:AutoFitWidth="0" ss:Width="150.75"/> 
   <Row ss:AutoFitHeight="0" ss:Height="30">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">Остатки денежных средств в кассе Банка на дату</Data></Cell> 
   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="29.25">
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ п/п</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Счет</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Подразделение</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наименование</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Валюта</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Сумма в валюте</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Сумма в рублях</Data></Cell>     
   </Row>
   '.
  /* */ 
      
   FOR EACH bacct
       WHERE      
       bacct.contract eq "Касса"
       and bacct.acct-cat EQ 'b'
       and (bacct.close-date EQ ? OR bacct.close-date ge end-date) 
       NO-LOCK 
       BREAK BY bacct.bal-acct 
             BY bacct.branch-id : 
        
       mI  = mI  + 1.        
       mAcct    = TRIM(ENTRY(1, bacct.acct, "@")).

      RUN acct-pos IN h_base (
           bacct.acct,
           bacct.currency,
           end-date,
           end-date, "√"
           ).
          
           
       if bacct.side eq "А" then
        do: 
            mSumA = mSumA + ABSOLUTE(sh-bal).
            mSumAVal = mSumAVal + ABSOLUTE(sh-val).
        end.
       if bacct.side eq "П" then
        do: 
            mSumP = mSumP + ABSOLUTE(sh-bal).
            mSumPVal = mSumPVal + ABSOLUTE(sh-val).
        end.   
         
           
    mDet = if NOT {assigned bacct.details} then "" else bacct.details. 
    
    
        PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + string(mI) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + mAcct + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + STRING(bacct.branch-id) + '</Data></Cell>\n'.        
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s62"><Data ss:Type="String">' + mDet + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + STRING(bacct.currency) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(ABSOLUTE(sh-val)) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(ABSOLUTE(sh-bal)) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '</Row>\n'.         
   end.
   
 /*put stream puk unformatted 
 '  <Row ss:AutoFitHeight="0" ss:Height="20.25">  
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>  
    <Cell ss:StyleID="res"><Data ss:Type="String">Валюта </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">Рубли  </Data></Cell> 
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25"> 
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Сумма Активов : </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  trim(STRING(mSumAVal,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  trim(STRING(mSumA,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell>   
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25">    
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Сумма Пассивов : </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  trim(STRING(mSumPVal,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  trim(STRING(mSumP,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell>   
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25">         
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Разница между Активами и Пассивами: </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +   trim(STRING(mSumAVal - mSumPVal,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +   trim(STRING(mSumA - mSumP,">>>>>>>>>>>>>>9,99")) + ' </Data></Cell>      
    </Row>   
 '. 
        */  
   put stream puk unformatted
'
      </Table>
    </Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}
