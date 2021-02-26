/*
               Банковская интегрированная система БИСквит
    Copyright: ПАО "ПЛЮС БАНК"
     Filename: cheta47422.p
      Comment: Отчет в АБС БИСКВИТ для мониторинга контрагентов               
      Comment: Вывод xml(открытие Excel)
   Parameters: 
         Uses: ZSS
      Used by:
      Created: 31/05/2018 zss
     Modified: 
*/
{globals.i}                            
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}

{clnt.fun}

/* {intrface.get base} */
{flt-val.i}
/* {details.i} */
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt1   AS INT64     NO-UNDO.

/* 
def var SummSh-in-bal as DECIMAL NO-UNDO.
def var SummSh-db as DECIMAL NO-UNDO.
def var SummSh-cr as DECIMAL NO-UNDO. */
def var SummSh-bal as DECIMAL NO-UNDO.
def var fname as char no-undo.
def var GetNameRezult AS CHAR NO-UNDO.
def var GetNameRezult1 AS CHAR NO-UNDO.
def new shared stream puk.


DEFINE TEMP-TABLE ttChet

        FIELD Acct              AS CHAR /*счет*/
        FIELD AcctNumber        AS CHAR
        FIELD AcctContragent    AS CHAR /*Чей счет*/
        FIELD ContragentINN     AS CHAR /*ИНН контр агента*/
        FIELD DateOper          AS DATE /*Дата последней операции*/
        FIELD AcctDetails       AS CHAR /*Наименование */
/*      FIELD AcctSh-cr         AS DECIMAL
        FIELD AcctSh-in-bal     AS DECIMAL 
        FIELD AcctSh-db         AS DECIMAL*/
        FIELD AcctSh-bal        AS DECIMAL
        
.

{empty ttChet}
ASSIGN
/* SummSh-in-bal = 0
SummSh-db = 0
SummSh-cr = 0 */
SummSh-bal = 0
.

/* PAUSE 0. */

{getdate.i
   &DateLabel="Задайте дату"
   &DateHelp="Введите дату"
   &AddPostUpd=" 
      
               "
}

/* PAUSE 0.
 */
for EACH acct WHERE ((CAN-DO('60312*,60314*',acct.acct)) or 
                    (acct.acct MATCHES '47423*' and acct.details MATCHES '*агентским*') or 
                    (acct.acct MATCHES '60323*' and  CAN-DO('*аренд*, *залог*',acct.details))) 
                    and (acct.filial-id <> '0400' and  acct.close-date EQ ?) 
                    
                 
                    NO-LOCK:

 {spinner.i "Секундочку ... " }

 RUN acct-pos-exc-cr IN h_base (acct.acct, acct.currency, end-date, CHR(251)).

        IF(lastmove NE ?) THEN DO:

        
         CREATE ttChet.                               
   
         ttChet.AcctNumber = acct.number.
     
         RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT GetNameRezult,OUTPUT GetNameRezult1).
         ttChet.AcctContragent = GetNameRezult. /*имя агента*/
         ttChet.ContragentINN  = GetNameRezult1. /*ИНН*/

         IF(acct.details EQ '') THEN ttChet.AcctDetails = ttChet.AcctContragent. /*если наименование пустое то брать контр. агента*/
         ELSE ttChet.AcctDetails = acct.details. /*наименование счета*/
         ttChet.DateOper = lastmove. /*дата последней операции*/
         ttChet.AcctSh-bal = sh-bal. /*исходящий остаток в рублях*/

          mInt = mInt + 1.

         SummSh-bal = SummSh-bal + sh-bal.
     
        END.
end.
 RELEASE ttChet.
 fname = "./oth_" + USERID ("bisquit") + ".xml".

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
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="350"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="65"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="150"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="500"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="70"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="140"/>
         <Row ss:AutoFitHeight="0" ss:Height="31.5">
          <Cell ss:StyleID="s64"><Data ss:Type="String">Наименование контрагента</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">ИНН</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Лицевой счет</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Наименование счета</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Остаток</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Дата последней операции  </Data></Cell>
      

         </Row>
         '.


      FOR EACH ttChet NO-LOCK:
         {spinner.i "Секундочку ... " }

         PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
                                      
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">'  REPLACE(ttChet.AcctContragent,",",' ')  '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' ttChet.ContragentINN  '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' ttChet.AcctNumber  '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' AcctDetails  '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-bal '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.DateOper '</Data></Cell>\n'.
                                      
                                        PUT STREAM puk UNFORMATTED '</Row>\n'.



     
      END.



/* PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' 'ИТОГО' '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' SummSh-bal '</Data></Cell>\n'.

PUT STREAM puk UNFORMATTED '</Row>\n'. */

put stream puk unformatted
        '
                  </Table>
                </Worksheet>
         </Workbook>
        '.
output stream puk close.

RUN sndbispc ("file=" + fname + ";class=bq").

/*получение имени агента и его ИНН*/
PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 DEF OUTPUT PARAMETER sname1 AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN DO:
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
    
    IF PERSON.INN EQ ? THEN sname1 =  ''.
        ELSE sname1 = PERSON.INN.

    END.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN DO:
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
      IF CUST-CORP.INN EQ  ? THEN sname1 =  ''.
        ELSE sname1 = CUST-CORP.INN.
    END. 
  END.
END.
{intrface.del}