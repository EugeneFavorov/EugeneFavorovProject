
DEFINE INPUT  PARAMETER iParams AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}
{intrface.get xclass}

DEFINE VARIABLE mVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mDetDb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetCr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctDb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumA  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumP  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumAVal  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mSumPVal  AS DECIMAL     NO-UNDO.
     

DEFINE BUFFER bacct FOR acct.
DEFINE BUFFER bop-entry FOR op-entry.

def new shared stream puk.
DEFINE VARIABLE fname as char   no-undo.

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
              }
              
       
              
if NOT {assigned mVal} Then mVal = "".
if mVal ne "" and (NOT can-do("810,840,978,398,''",mVal)) then
 Do:
 MESSAGE "Нет такой валюты" VIEW-AS ALERT-BOX.
 RETURN {&RET-ERROR}.
end.

   {justasec} 

fname = "./Oborot_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml". 

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
   <Column ss:AutoFitWidth="0" ss:Width="500"/>
   <Column ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:AutoFitWidth="0" ss:Width="500"/> 
   <Column ss:AutoFitWidth="0" ss:Width="100"/> 
   <Column ss:AutoFitWidth="0" ss:Width="150.75"/> 
   <Column ss:AutoFitWidth="0" ss:Width="150.75"/> 
   <Row ss:AutoFitHeight="0" ss:Height="30">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">Обороты по указанной валюте за указанную дату</Data></Cell> 
   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="29.25">
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ п/п</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Счет по Дт</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наименование</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Счет по Кр</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наименование</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Сумма в валюте</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Сумма в рублях</Data></Cell>     
   </Row>
   '.
         
   FOR EACH  bop-entry WHERE
    bop-entry.op-date >= beg-date and
    bop-entry.op-date <= end-date and
    bop-entry.currency eq mVal
     NO-LOCK 
       BREAK BY bop-entry.acct-cr 
             BY bop-entry.acct-db: 
        
       mI  = mI  + 1.        
       mAcctCr    = TRIM(ENTRY(1,bop-entry.acct-cr, "@")).
       mAcctDb    = TRIM(ENTRY(1,bop-entry.acct-db, "@")).
          
   find first bacct where bacct.acct eq bop-entry.acct-cr no-lock no-error.  
       if avail bacct then              
           mDetCr = if NOT {assigned bacct.details} then "" else bacct.details.
   find first bacct where bacct.acct eq bop-entry.acct-db no-lock no-error. 
       if avail bacct then
           mDetDb = if NOT {assigned bacct.details} then "" else bacct.details.
    
        PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + string(mI) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + mAcctCr + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s62"><Data ss:Type="String">' + mDetCr + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + mAcctDb + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s62"><Data ss:Type="String">' + mDetDb + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + STRING(bop-entry.op-date) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(bop-entry.amt-cur) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(bop-entry.amt-rub) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '</Row>\n'.         
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
