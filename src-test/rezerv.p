
DEFINE INPUT  PARAMETER iParams AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{intrface.get xclass}

DEFINE VARIABLE mVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mAcctA AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctR AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSum  AS DECIMAL     NO-UNDO.
     

DEFINE BUFFER bloan-acct FOR loan-acct.
DEFINE BUFFER bloan-acct2 FOR loan-acct.

def new shared stream puk.
DEFINE VARIABLE fname as char   no-undo.
/*
{getdates.i &DateLabel       = "Дата"
              &DateHelp        = "Дата (F1 - Календарь)"
              &DispAfterDate   = "mVal 
                                  label ""Валюта""
                                  HELP ""Код валюты""                                 
                                  "
              &UpdAfterDate    = "mVal VIEW-AS FILL-IN SIZE 10 BY 1
                                  FORMAT '999'
                                  LABEL 'Валюта'"

              &noinit          = YES
               &return         = "return 'Пользователь прервал ввод'"
              }*/
              
 {getdate.i &DateLabel       = "Дата"
              &DateHelp        = "Дата (F1 - Календарь)"
              &return         = "return 'Пользователь прервал ввод'"
              }             
              
       
              
if NOT {assigned mVal} Then mVal = "".
if mVal ne "" and (NOT can-do("810,840,978,398,''",mVal)) then
 Do:
 MESSAGE "Нет такой валюты" VIEW-AS ALERT-BOX.
 RETURN {&RET-ERROR}.
end.

   {justasec} 

fname = "./Rezerv_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml". 

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
   <Column ss:AutoFitWidth="0" ss:Width="150"/>   
   <Column ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:AutoFitWidth="0" ss:Width="150"/>
  
   <Row ss:AutoFitHeight="0" ss:Height="30">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">Резервы, созданные по Активам в иностранной валюте на  ' + string(end-date) +  'г. </Data></Cell> 
   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="29.25">
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ п/п</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Счет Актива</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Счет резерва</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Сумма</Data></Cell>   
   </Row>
   '.
   
    FOR EACH bloan-acct WHERE /*bloan-acct.contract eq "Кредит" and*/
         bloan-acct.currency NE ""
        AND bloan-acct.acct-type = "КредПр"                
        NO-LOCK 
        BREAK by bloan-acct.since: 
                                 
        FIND LAST bloan-acct2 WHERE /*bloan-acct.contract eq "Кредит" and */
             bloan-acct.cont-code eq bloan-acct2.cont-code           
            AND bloan-acct2.acct-type = "КредРез1"        NO-LOCK NO-ERROR.
                  
                         
    /*   mAcctA  = GetInfoPros(RECID(loan),end-date,"КредПр").*/    
       mAcctA  = TRIM(ENTRY(1, bloan-acct.acct, "@")).
               
           IF AVAIL bloan-acct2 THEN 
              mAcctR  = TRIM(ENTRY(1, bloan-acct2.acct, "@")).
            else next . 
            
       RUN acct-pos IN h_base (
           bloan-acct2.acct,
           "",
           end-date,
           end-date, "√"
           ).
               
         mI  = mI  + 1. 
         msum = abs(sh-bal).              
    
        PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + string(mI) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + mAcctA + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + mAcctR + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + string(msum) + '</Data></Cell>\n'.  
   
        PUT STREAM puk UNFORMATTED '</Row>\n'.         
   end.
   
IF  mI = 0 THEN
 Do: MESSAGE "Нет данных для отчета" VIEW-AS ALERT-BOX.    
      return  .
 
 end. 

   put stream puk unformatted
'
      </Table>
    </Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}
