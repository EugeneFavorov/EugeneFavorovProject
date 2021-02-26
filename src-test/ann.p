{globals.i} 
{tmprecid.def} 

DEFINE VARIABLE mDog    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClient AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaz    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGNI_o  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGNI_z  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate_c AS CHARACTER NO-UNDO.
DEFINE VARIABLE name1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE name2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInn AS CHARACTER NO-UNDO.

/*
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumA  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumP  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumAVal  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumPVal  AS DECIMAL     NO-UNDO.*/
     

DEFINE BUFFER bacct FOR acct.

def new shared stream puk.
DEFINE VARIABLE fname as char   no-undo.

fname = "./Acct_" /*+ replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit')*/ + ".xml". 

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
       
   <Column ss:AutoFitWidth="0" ss:Width="70"/>
   <Column ss:AutoFitWidth="0" ss:Width="90"/>   
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="500"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Row ss:AutoFitHeight="0" ss:Height="20">    
   <Cell ss:StyleID="s62"><Data ss:Type="String">' + dept.name-bank + '</Data></Cell> 
   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="20">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">Выписка из Книги регистрации открытых счетов</Data></Cell> 
   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="45">
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата открытия счета</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ и дата договора</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ лицевого счета</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наименование счета (клиента)</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Цель счета</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Порядок выдачи выписки</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата обращения в ГНИ (откр)</Data></Cell>     
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата закрытия счета</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Примечание</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата обращения в ГНИ (закр)</Data></Cell>
   </Row>
   

   <Row ss:AutoFitHeight="0" ss:Height="20">
   <Cell ss:StyleID="s61"><Data ss:Type="String">1</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">2</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">3</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">4</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">5</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">6</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">7</Data></Cell>     
   <Cell ss:StyleID="s61"><Data ss:Type="String">8</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">9</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">10</Data></Cell>
   </Row>
   '.

FOR EACH tmprecid NO-LOCK,                       
    FIRST  bacct WHERE RECID(bacct) EQ tmprecid.id
    NO-LOCK 
    BREAK BY bacct.open-date: 
    
    RUN GetCustName IN h_base (bacct.cust-cat,bacct.cust-id, bacct.acct,
				OUTPUT name1,
				OUTPUT name2,
				INPUT-OUTPUT mINN).
    ASSIGN
    name1 = name1 + " " + name2
    name2 = "".
    .	
            
    mDog = GetXAttrValueEx("acct",STRING(bacct.acct) + ',' + STRING(bacct.currency), "ДогОткрЛС","").
    mDog = If {assigned mDog} then ENTRY(2,mDog,",") + ' от ' + REPLACE(ENTRY(1,mDog,","), "/", ".") else "".  
    
    mGNI_o = GetXAttrValueEx("acct",STRING(bacct.acct) + ',' + STRING(bacct.currency), "ДатаСообщЛС","").
    mGNI_o = If {assigned mGNI_o} then REPLACE(mGNI_o, "/", ".") else "". 
    
    mGNI_z = GetXAttrValueEx("acct",STRING(bacct.acct) + ',' + STRING(bacct.currency), "ДатаСообщЗак","").
    mGNI_z = If {assigned mGNI_z} then REPLACE(mGNI_z, "/", ".") else "".     
    
    mDate_c = string(bacct.close-date,"99/99/9999").     
    mDate_c = If  mDate_c eq ? then   "" else REPLACE(mDate_c, "/", ".").  
   
    FIND FIRST contract WHERE contract.contract EQ bacct.contract NO-LOCK NO-ERROR.
    IF(avail contract)THEN     
        mNaz = contract.name-contract. 
    else 
        mNaz = bacct.contract.
    
        PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="35">\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + REPLACE(string(bacct.open-date,"99/99/9999"), "/", ".") + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  mDog   + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + subSTRING(bacct.acct,1,20) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + name1 + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  mNaz    + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + '?' + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  mGNI_o  + '</Data></Cell>\n'.        
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  mDate_c + '</Data></Cell>\n'.        
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + '?' + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  mGNI_z  + '</Data></Cell>\n'.
      /*  PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(ABSOLUTE(sh-bal)) + '</Data></Cell>\n'.*/
        PUT STREAM puk UNFORMATTED '</Row>\n'.         
   end.
 
          /*  mSumP = abs(mSumP). 
            mSumPVal  =  abs(mSumPVal).     
 put stream puk unformatted 
 '  <Row ss:AutoFitHeight="0" ss:Height="20.25">  
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>  
    <Cell ss:StyleID="res"><Data ss:Type="String">Валюта </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">Рубли  </Data></Cell> 
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25"> 
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Сумма Активов : </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  STRING(mSumAVal) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  STRING(mSumA) + ' </Data></Cell>   
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25">    
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Сумма Пассивов : </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  STRING(mSumPVal) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +  STRING(mSumP) + ' </Data></Cell>   
    </Row>
    <Row ss:AutoFitHeight="0" ss:Height="20.25">         
    <Cell ss:StyleID="res"><Data ss:Type="String">  </Data></Cell>    
    <Cell ss:StyleID="res"><Data ss:Type="String"> Разница между Активами и Пассивами: </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +   STRING(mSumAVal - mSumPVal) + ' </Data></Cell> 
    <Cell ss:StyleID="res"><Data ss:Type="String">' +   STRING(mSumA - mSumP) + ' </Data></Cell>      
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
