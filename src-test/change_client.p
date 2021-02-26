/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment:
   Parameters:
         Uses:
      Used by:
      Created:
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

DEFINE INPUT PARAM my_CustCat AS CHAR NO-UNDO. /* CUST_CAT Ю(Ч) */

DEFINE BUFFER 	bbloan 		FOR loan.
DEFINE BUFFER 	bbloan2		FOR loan.

def new shared stream puk.
def var fname as char  init "./credits.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.

DEFINE VARIABLE mDocSerial     AS CHARACTER FORMAT "x(4)" NO-UNDO.
DEFINE VARIABLE tClient        AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE vPureClient    AS Int64 NO-UNDO.   
DEFINE VARIABLE custN	 	   AS CHARACTER NO-UNDO.
DEFINE VARIABLE custDC     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE i          AS INTEGER NO-UNDO.   
DEFINE VARIABLE sumStr     AS INTEGER NO-UNDO.      
DEFINE VARIABLE chINN       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chKPP       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chName      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE chOgrn      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE chName1      AS CHARACTER NO-UNDO.    
DEFINE VARIABLE chName2      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE chAdr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE flag          AS LOGICAL  init false NO-UNDO. 
   
 DEFINE TEMP-TABLE tt-cli NO-UNDO   
    FIELD cat          AS CHAR
    FIELD mName        AS CHAR
    FIELD mAcct        AS CHAR
    FIELD mKpp         AS CHAR
    FIELD mInn         AS CHAR
    FIELD mOgrn        AS CHAR
    FIELD mNameCh      AS CHAR
    FIELD mAdrCh       AS CHAR
    FIELD mOpenDAcct   AS DATE
    FIELD mCloseDAcct  AS DATE
    FIELD PassportDeal AS CHAR   
    . 

DEF BUFFER tmpsigns_inn     FOR tmpsigns.

{empty tt-cli}
end-date = TODAY.

mDocSerial = GetXAttrValue("_user", USERID("bisquit"), "Отделение").
tClient = "Ю".
{getdates.i 
   &dispBeforeDate="mDocSerial HELP 'Подразделение' LABEL 'Подразделение'  SKIP
                     tClient HELP 'Ю или ИП' LABEL 'Тип клиента Ю/ИП'  SKIP  "
   
   
   &UpdBeforeDate="mDocSerial HELP 'Подразделение' LABEL 'Подразделение'  SKIP 
                   tClient HELP 'Ю или ИП' LABEL 'Тип клиента Ю/ИП'  SKIP"
   
   
   &BegHelp="Введите дату начала периода (F1 - календарь)"
   &EndHelp="Введите дату окончания периода (F1 - календарь)"
   &UpdBeforeDate="mDocSerial,tClient" 
  &return         = "return 'Пользователь прервал ввод'"
  
}
IF (mDocSerial = 'СВОД') OR (mDocSerial = '') OR (mDocSerial = '*') THEN flag = TRUE.
if  substring(mDocSerial,3,2) eq "00" Then mDocSerial = substring(mDocSerial,1,2). 
/* идем по Юридическим лицам*/
IF tClient = 'Ю' THEN
DO:
    FOR EACH signs WHERE
        signs.file-name  EQ "cust-corp"
        AND signs.code       EQ "PasspTransaction"
        AND signs.code-value EQ "Да" 
         NO-LOCK,
        EACH cust-corp WHERE cust-corp.cust-id EQ INT64(signs.surrogate)      
        NO-LOCK,                      
        EACH acct WHERE acct.cust-cat EQ "Ю"
        AND acct.cust-id  EQ cust-corp.cust-id 
        AND acct.contract EQ 'Расчет' 
        AND (beg-date <= acct.open-date) AND (end-date >= acct.open-date) 
        AND ((acct.branch-id begins mDocSerial) or flag)
      NO-LOCK:   
            
            /* MESSAGE "888" /*mDocSerial "2" GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id","")*/ VIEW-AS ALERT-BOX.*/           
      /* IF  (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id",""))    THEN*/ 
        Do:                   
            sumStr = 0.   
            RUN GetStr(INPUT "cust-corp",INPUT "name-corp",INPUT string(cust-corp.cust-id),INPUT-OUTPUT chName). 
            RUN GetStr(INPUT "cust-corp",INPUT "inn",INPUT string(cust-corp.cust-id),INPUT-OUTPUT chINN). 
            RUN GetStr(INPUT "cust-corp",INPUT "kpp",INPUT string(cust-corp.cust-id),INPUT-OUTPUT chKPP).        
            chOgrn = GetXAttrValueEx("cust-corp",  STRING(cust-corp.cust-id), "ОГРН",  "").
            
            RUN GetAdr(INPUT "АдрЮр",INPUT cust-corp.cust-id,"Ю",INPUT-OUTPUT chAdr).  
                        
            CREATE tt-cli.
            ASSIGN
                vPureClient         = vPureClient + 1      
                tt-cli.cat          = acct.cust-cat             
                tt-cli.mName        = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP
                tt-cli.mAcct        = SUBSTR(acct.acct,1,20)               
                tt-cli.mOpenDAcct   = acct.open-date 
                tt-cli.mCloseDAcct  = acct.close-date 
                tt-cli.PassportDeal = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "PasspTransaction","")   
                tt-cli.mNameCh      = chName 
                tt-cli.mInn         = chINN
                tt-cli.mKpp         = chKPP        
                tt-cli.mOgrn        = chOgrn      
                tt-cli.mAdrCh       = chAdr   
                .
                        
            chINN  = "".  
            chName = "".
            chAdr  = "".
        end.
    END.
END.

/* идем по отмеченным Физическим лицам */
IF tClient = 'ИП' THEN
DO:
    FOR EACH signs WHERE
        signs.file-name  EQ "person"
        AND signs.code       EQ "PasspTransaction"
        AND signs.code-value EQ "ДА"  NO-LOCK,
        EACH person WHERE person.person-id EQ INT64(signs.surrogate)  
        NO-LOCK,                      
        EACH acct WHERE acct.cust-cat EQ "Ч"
        AND acct.cust-id  EQ person.person-id 
        AND acct.contract EQ 'Расчет'
        and (beg-date <= acct.open-date) AND (end-date >= acct.open-date)
          AND ((acct.branch-id begins mDocSerial) or flag)          
                               
        NO-LOCK:      
            
       /* if  substring(mDocSerial,3,2) eq "00" Then mDocSerial = substring(mDocSerial,1,2).          
   
        IF  ((GetXAttrValueEx("person",STRING(person.person-id), "branch-id","") eq mDocSerial) or flag) THEN*/
        Do:     
            sumStr = 0.             
             
            /*RUN GetStr(INPUT "name-last",INPUT string(person.person-id),INPUT-OUTPUT chName). 
            
            RUN GetStr(INPUT "kpp",INPUT string(person.person-id),INPUT-OUTPUT chKPP).*/ 
          /*  RUN GetStr(INPUT "inn",INPUT string(person.person-id),INPUT-OUTPUT chINN).*/
            RUN GetStr(INPUT "person",INPUT "first-names",INPUT string(person.person-id),INPUT-OUTPUT chName1). 
            RUN GetStr(INPUT "person",INPUT "name-last",INPUT string(person.person-id),INPUT-OUTPUT chName2).
            if chName1 eq chName2 then chName = chName1.
             else chName = chName1 + " " + chName2.
            RUN GetAdr(INPUT "АдрПроп",INPUT person.person-id,"Ч",INPUT-OUTPUT chAdr).          
           
            CREATE tt-cli.
            ASSIGN
                vPureClient         = vPureClient + 1
                tt-cli.cat          = acct.cust-cat               
                tt-cli.mName        = person.name-last + " " + person.first-names
                tt-cli.mAcct        = SUBSTR(acct.acct,1,20)               
                tt-cli.mOpenDAcct   = acct.open-date 
                tt-cli.mCloseDAcct  = acct.close-date 
                tt-cli.PassportDeal = GetXAttrValueEx("person",STRING(person.person-id), "PasspTransaction","")   
                tt-cli.mNameCh      = chName                 
                tt-cli.mInn         = "Не предусмотрено"
                tt-cli.mKpp         = "Нет"       
                tt-cli.mOgrn        = "Нет"   
                tt-cli.mAdrCh       = chAdr 
                  
                .      
                        
            chINN  = "".  
            chName = "".
            chAdr  = "".
        end.
    END.
END.

if not AVAILABLE  tt-cli then
do:
MESSAGE  "Данных не найдено"         VIEW-AS ALERT-BOX .    
RETURN.
end.

fname = "./change_client_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

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
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
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
  <Style ss:ID="s63">
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
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s69">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
   <Style ss:ID="s70">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table ss:ExpandedColumnCount="10" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">   
   <Column ss:AutoFitWidth="0" ss:Width="250.0"/>
   <Column ss:AutoFitWidth="0" ss:Width="120.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="114.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="109.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="180.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Row ss:AutoFitHeight="0" ss:Height="29.25">
    <Cell ss:StyleID="s62"><Data ss:Type="String">Наименование клиента</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Счет</Data></Cell>
    <Cell ss:StyleID="s64"><Data ss:Type="String">Дата открытия</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">Дата закрытия</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">ОГРН</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Изменение юридического адреса</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">Изменение ИНН</Data></Cell>    
    <Cell ss:StyleID="s68"><Data ss:Type="String">Изменение КПП</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">Изменение наименования</Data></Cell>
    <Cell ss:StyleID="s70"><Data ss:Type="String">Паспорт сделки</Data></Cell>
   </Row>
   '.

FOR EACH tt-cli
NO-LOCK 
    BREAK BY tt-cli.mName:                  
		custDC =if tt-cli.mCloseDAcct = ? then "" else STRING(tt-cli.mCloseDAcct, "99.99.9999").	  
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
	    PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s62"><Data ss:Type="String">' + STRING(tt-cli.mName) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="String">' + STRING(tt-cli.mAcct) + '</Data></Cell>\n'.
	    PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s64"><Data ss:Type="String">' + STRING(tt-cli.mOpenDAcct, "99.99.9999") + '</Data></Cell>\n'.		
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="String">' + custDC + '</Data></Cell>\n'.
	    PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(tt-cli.mOgrn) + '</Data></Cell>\n'.	
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s66"><Data ss:Type="String">' + STRING(tt-cli.mAdrCh) + '</Data></Cell>\n'.	
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(tt-cli.mINN) + '</Data></Cell>\n'.     
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(tt-cli.mKPP) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' + STRING(tt-cli.mNameCh) + '</Data></Cell>\n'.
        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(tt-cli.PassportDeal) + '</Data></Cell>\n'. 
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

/* конец вывода */		
 
/* находим наименование клиента */

/*GetStr(INPUT "name-last",INPUT string(person.person-id),INPUT-OUTPUT chName).,
RUN GetStr(INPUT "cust-corp",INPUT "name-corp",INPUT string(cust-corp.cust-id),INPUT-OUTPUT chName). 
*/
PROCEDURE GetStr:
    DEF INPUT PARAMETER fName AS CHARACTER.
	DEF INPUT PARAMETER fCode AS CHARACTER.	
    DEF INPUT PARAMETER id AS CHARACTER.    
	DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
	
FOR EACH  tmpsigns_inn WHERE tmpsigns_inn.file-name = fName
        and tmpsigns_inn.code = fCode 
        and tmpsigns_inn.surrogate  = id 
        and tmpsigns_inn.xattr-value NE ""
        and (beg-date <= tmpsigns_inn.since) 
        AND (end-date >= tmpsigns_inn.since)        
        NO-LOCK:      
                sname         =  sname +  STRING(tmpsigns_inn.since,"99/99/9999") + " " +  tmpsigns_inn.xattr-value  + ";".     
         END. 		
	If index(sname,";") EQ Length(sname) THEN  sname = "Не измененялось".		
END.			

/* находим юр.адрес */
PROCEDURE GetAdr:
    DEF INPUT PARAMETER fName AS CHARACTER.
    DEF INPUT PARAMETER id AS INTEGER.
    DEF INPUT PARAMETER vType AS CHARACTER.
    DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.       
    
DEFINE VARIABLE vStr     AS CHARACTER INIT '' NO-UNDO.

FOR EACH  cust-ident WHERE cust-ident.cust-cat eq vType
        and cust-ident.cust-id  eq id 
        and cust-ident.cust-code-type eq fName
        and (beg-date <= cust-ident.open-date) AND (end-date >= cust-ident.open-date)          
        NO-LOCK:      
            
           if string(cust-ident.close-date) NE "" THEN vStr = "Дата закрытия:" + string(cust-ident.close-date).  
            
                sname         =  sname + "Дата ввода:" + string(cust-ident.open-date) + " " + string(cust-ident.issue)  + " " +  ";".     
END.   
    
    If index(sname,";") EQ Length(sname) THEN  sname = "Не измененялось".
END.