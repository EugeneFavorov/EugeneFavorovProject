/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2018 "Банковские информационные системы"
     Filename: cheta47422.p
      Comment: Выписка по операциям на счете организации
               (Открытая система печати)
      Comment: Вывод xml(открытие Excel)
   Parameters: 
         Uses: ZSS
      Used by:
      Created: 18/03/2016 zss
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
def var eol as char format "x(2)" no-undo.
def var m1 as DECIMAL NO-UNDO.
def var delim as char init "|" format "x(1)" no-undo.
def var SummSh-in-bal as DECIMAL NO-UNDO.
def var SummSh-db as DECIMAL NO-UNDO.
def var SummSh-cr as DECIMAL NO-UNDO.
def var SummSh-bal as DECIMAL NO-UNDO.
def var fname as char no-undo.
def new shared stream puk.

DEFINE VARIABLE myStartDate AS DATE NO-UNDO.
DEFINE VARIABLE myEndDate AS DATE NO-UNDO.

def var sP AS CHAR NO-UNDO. /*Для условий в PUT пустая переменая -затычка*/

DEFINE TEMP-TABLE ttChet

        FIELD Acct              AS CHAR
        FIELD AcctNumber        AS CHAR
        FIELD AcctBranch        AS CHAR
        FIELD AcctDetails       AS CHAR
        FIELD NumDogovor        AS CHAR
        FIELD AcctSh-cr         AS DECIMAL
        FIELD AcctSh-bal        AS DECIMAL
        FIELD AcctSh-in-bal     AS DECIMAL
        FIELD AcctSh-db         AS DECIMAL
        
.


{empty ttChet}
ASSIGN
SummSh-in-bal = 0
SummSh-db = 0
SummSh-cr = 0
SummSh-bal = 0
myStartDate = TODAY
myEndDate = TODAY
.



/* {getdates.i
    &BegLabel = Начальная_дата
    &EndLabel = Конечная_дата
    &TitleLabel=Выберите дату для отчета
    
}
 */
PAUSE 0.
/*Выбор параметров отчета*/
DEFINE FRAME frame_date_codes 
   myStartDate LABEL "С даты"
   myEndDate LABEL "По дату"
   WITH 1 COL 1 DOWN
   
   WIDTH 50 CENTERED OVERLAY TITLE "Отчет по учет ком. по аренде сейфовой ячейки".



ON 'F1' OF myStartDate
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN ASSIGN myStartDate:SCREEN-VALUE = pick-value.
   END.

ON 'F1' OF myEndDate
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN ASSIGN myEndDate:SCREEN-VALUE = pick-value.
   END.
/* ON 'F1' OF strTrans
   DO:
     /*  IF FRAME-VALUE = strTrans THEN 
      DO: */ 
         strTrans:SCREEN-VALUE = string(strTrans).
         strTrans = strTrans
         .
      /* END. */        
     /*  ELSE 
      DO:
         strTrans:SCREEN-VALUE = string(strTrans).
         strTrans = strTrans1.
      END. */
   END. */

PAUSE 0.
UPDATE
   myStartDate
   myEndDate 
   /* strTrans */
   WITH FRAME frame_date_codes.


/* message fname view-as alert-box. */
/* message myStartDate ',' myEndDate view-as alert-box. */
for EACH acct WHERE /* acct.details MATCHES '*сейф*' and */ acct.acct MATCHES '47422*' NO-LOCK:

 {spinner.i "Секундочку ... " }
 FIND  FIRST loan-acct where loan-acct.acct EQ  acct.acct and loan-acct.acct-type = "АрдБудДох" no-lock no-error.
  IF AVAIL(loan-acct) THEN do:  
   RUN acct-pos IN h_base (acct.acct, '',  myStartDate, myEndDate, CHR(251)).


  

      IF NOT (Sh-in-bal  = 0  and sh-db = 0 and sh-cr = 0 and sh-bal = 0) /* and AVAIL(loan-acct) */ THEN DO:
        CREATE ttChet.                               
   

          ttChet.Acct = acct.acct. /*счет через @ */
          ttChet.AcctNumber = acct.number. /*счет -номе*/
          ttChet.AcctBranch = acct.branch-id. /*код отдела*/
          ttChet.AcctDetails = acct.details. /*наименование счета*/
    
         /* определение номера договора*/     
        /*  FIND  FIRST loan-acct where loan-acct.acct EQ  acct.acct and loan-acct.acct-type = "АрдБудДох" no-lock no-error.
          IF AVAIL(loan-acct) THEN DO:  */
          ttChet.NumDogovor = GetEntries(1,loan-acct.cont-code,'@',loan-acct.cont-code).      
       /*    END.  */ 


         /*  ttChet.NumDogovor = loan-acct.cont-code. /*Номер договора*/ */
          ttChet.AcctSh-in-bal = Sh-in-bal. /*входящий остаток в рублях*/
          ttChet.AcctSh-db = sh-db. /*дебетовые обороты в рублях*/
          ttChet.AcctSh-cr = sh-cr. /*обороты по кредиту в рублях*/
          ttChet.AcctSh-bal = sh-bal. /*исходящий остаток в рублях*/


          mInt = mInt + 1.

        SummSh-in-bal = SummSh-in-bal + Sh-in-bal.
        SummSh-db = SummSh-db + sh-db.
        SummSh-cr = SummSh-cr + sh-cr.
        SummSh-bal = SummSh-bal + sh-bal.


      
      END.  
  END.

end.
 RELEASE ttChet.
def var NomerDogovora AS CHAR NO-UNDO.
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
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="150"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="450"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="250"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="95"/>
         <Row ss:AutoFitHeight="0" ss:Height="31.5">
          <Cell ss:StyleID="s64"><Data ss:Type="String">Лицевой счет</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Код подразделения</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Наименование счета</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Номер договора на аренду сейфовой ячейки</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Входящий остаток</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Оборот по дебету</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Оборот по кредиту</Data></Cell>
          <Cell ss:StyleID="s64"><Data ss:Type="String">Исходящий остаток</Data></Cell>
         </Row>
         '.


      for EACH ttChet NO-LOCK:





         PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' ttChet.AcctNumber  '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctBranch '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctDetails '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.NumDogovor '</Data></Cell>\n'.




                             IF(String(ttChet.AcctSh-in-bal) <> '0') THEN DO: PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-in-bal '</Data></Cell>\n'. END.
                                                               ELSE PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
                                                                    
                             IF(String(ttChet.AcctSh-db ) <> '0') THEN DO: PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-db '</Data></Cell>\n'. END.
                                                               ELSE PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
                                                                      
                             IF(String(ttChet.AcctSh-cr) <> '0') THEN DO: PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-cr '</Data></Cell>\n'. END.
                                                               ELSE PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
                                                                
                             IF(String(ttChet.AcctSh-bal) <> '0') THEN DO: PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-bal '</Data></Cell>\n'. END.
                                                               ELSE PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' '</Data></Cell>\n'.




   
 
                                 /*   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-in-bal '</Data></Cell>\n'. 
                                   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-db '</Data></Cell>\n'. 
                                   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-cr '</Data></Cell>\n'.
                                   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' ttChet.AcctSh-bal '</Data></Cell>\n'. 
                                   */

                                        PUT STREAM puk UNFORMATTED '</Row>\n'.



     
      END.



PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' 'ИТОГО' '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">'  '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' SummSh-in-bal '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' SummSh-db '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' SummSh-cr '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="String">' SummSh-bal '</Data></Cell>\n'.
PUT STREAM puk UNFORMATTED '</Row>\n'.

put stream puk unformatted
        '
                  </Table>
                </Worksheet>
         </Workbook>
        '.
output stream puk close.

RUN sndbispc ("file=" + fname + ";class=bq").
/* {setdest.i &col=170 }

      for EACH ttChet NO-LOCK:
      mInt1 = mInt1 + 1.
      message ttChet.AcctSh-cr view-as alert-box. 
      PUT UNFORMATTED
      ttChet.AcctNumber delim ttChet.AcctBranch delim ttChet.AcctDetails delim ttChet.AcctSh-in-bal delim  ttChet.AcctSh-db delim 
      ttChet.AcctSh-cr delim ttChet.AcctSh-bal eol 

       skip.

       END.


    PUT UNFORMATTED 'Итого' SummSh-in-bal delim SummSh-db delim SummSh-cr delim SummSh-bal eol
    skip.
      

PUT UNFORMATTED "ВСЕГО:" STRING(mInt1) eol 
       skip.
{preview.i &col=170} */