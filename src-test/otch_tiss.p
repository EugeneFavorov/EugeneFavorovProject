/* Отчет по перечислению  */
/* kam */



{globals.i}
{pp-corr.p}
{prn-doc.def &with_proc=YES}
{date.fun}

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.
def var commiss as decimal no-undo format '99.99' init 5.0.
def var summAgent as decimal no-undo.
def var summPrem as decimal no-undo.
def var AmtStringA as char no-undo.
def var DecStrA as decimal no-undo.
def var AmtString2A as char no-undo.
def buffer aop for op.
def buffer aop-entry for op-entry.
DEFINE VARIABLE AmtString  AS CHARACTER NO-UNDO.
DEFINE VARIABLE DecStr     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE AmtString2 AS CHARACTER NO-UNDO.

  
DEFINE TEMP-TABLE otchakc
        FIELD date_dog     AS CHAR                 /* дата договора */
        FIELD num_dog      AS CHAR                 /* номер договора */
        FIELD name_cl      AS CHAR                 /* наименование заемщика */
        FIELD summBrok     AS DECIMAL              /* Вознаграждение Брокера по договору оказания услуг */
        FIELD summAgent     AS DECIMAL              /* сумма страх премии */
        INDEX num_dog num_dog       
    .

{empty otchakc}

tmpdate = date(month(today),1,year(today)).
beg-date = tmpdate.
end-date = date_correct(month(today),0,31,year(today)).


pause 0.

define FRAME frame_date_codes 
         beg-date label "Дата С:"
	 end-date label "Дата По:"	
	 commiss label "Коэфф.вознагр"
	 with 1  COL 1 down
	 
width 50 CENTERED OVERLAY ROW 10 TITLE "Данные для отчета ".

  do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	beg-date
	end-date
	commiss

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   if LASTKEY EQ KEYCODE("ESC") THEN
	return.
   if LASTKEY EQ KEYCODE("F1")
		THEN do:
			CASE FRAME-FIELD:
			WHEN "beg-date" THEN
				DO:
				run calend.p.
				if (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				then frame-value = string(date(pick-value), "99/99/9999").
			END.
			WHEN "end-date" THEN
				DO:
				run calend.p.
				if (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				then frame-value = string(date(pick-value), "99/99/9999").
			end.
			END CASE.

		end.
		ELSE APPLY LASTKEY.


   end. /* EDITING: */
  end.  /* do on */	



  for each op where 
        op.op-date >= beg-date 
        and op.op-date <= end-date 
        and op.ben-acct = '40701810900000008072'
        and op.op-status begins "√" no-lock,
        each op-entry of op where substring(op-entry.acct-db,1,5) = '40817' no-lock:
        summPrem = abs(op-entry.amt-rub).
        summAgent = summPrem * commiss / 100.

        nameCl = ''.
        find first acct where acct.acct = op-entry.acct-db no-lock no-error.
        if avail acct then RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).
        

            create otchakc.            
            assign
                otchakc.date_dog = substring(op.DETAILS,94,10)
                otchakc.num_dog = substring(op.DETAILS,73,17)
                otchakc.name_cl = nameCl                
                otchakc.summBrok = summPrem
                otchakc.summAgent = summAgent
                .
            release otchakc.
end.

DEF VAR fname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF NEW SHARED STREAM vvs.
/* выводим в ексель */
    fname = "./otchtiss"  + "_" + USERID('bisquit') + ".xml".
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="50.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер ПП</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Дата заключения договора оказания услуг</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">N договора оказания услуги </Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО Клиента</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вознаграждение Брокера по договору оказания услуг</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вознаграждение Агента, %</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вознаграждение Агента, руб.</Data></Cell>
   </Row>
'.

        /*
        FIELD date_dog     AS CHAR                 /* дата договора */
        FIELD num_dog      AS CHAR                 /* номер договора */
        FIELD name_cl      AS CHAR                 /* наименование заемщика */
        FIELD summBrok     AS DECIMAL              /* Вознаграждение Брокера по договору оказания услуг */
        FIELD summAgent     AS DECIMAL              /* сумма страх премии */      
    
    */

summPrem = 0.
summAgent = 0.
    
DEF VAR ii as INT NO-UNDO.          
FOR EACH otchakc NO-LOCK BY otchakc.name_cl :
	ii = ii + 1.
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(ii) + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.date_dog + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.num_dog + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.name_cl + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otchakc.summBrok,"->>>>>>>>>9.99") + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(commiss,"->>>9.99") + '</Data></Cell>\n'.
        
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otchakc.summAgent,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '</Row>'.
    
    summPrem = summPrem + otchakc.summBrok.
    summAgent = summAgent + otchakc.summAgent.
END.  
PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.  
PUT STREAM vvs UNFORMATTED '</Row>'.

run amtstr.p (summPrem, yes, output AmtStringA, output DecStrA).
AmtString2A = StRing(DecStrA,"99") + ' коп.'.
AmtStringA = AmtStringA + ' ' + AmtString2A.

PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED 'Итого Вознаграждение Брокера по договору оказания услуг' + STRING(summPrem,"->>>>>>>>>9.99") + '(' + AmtStringA + ')</Data></Cell>\n'.
PUT STREAM vvs UNFORMATTED '</Row>'.

run amtstr.p (summAgent, yes, output AmtStringA, output DecStrA).
AmtString2A = StRing(DecStr,"99") + ' коп.'.
AmtStringA = AmtStringA + ' ' + AmtString2A.

PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED 'Итого размер АВ' + STRING(summAgent,"->>>>>>>>>9.99") + '(' + AmtStringA + ')</Data></Cell>\n'.
PUT STREAM vvs UNFORMATTED '</Row>'.

summPrem = summAgent * 18 / 118.
run amtstr.p (summPrem, yes, output AmtStringA, output DecStrA).
AmtString2A = StRing(DecStrA,"99") + ' коп.'.
AmtStringA = AmtStringA + ' ' + AmtString2A.

PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED 'Включая НДС 18%' + STRING(summPrem,"->>>>>>>>>9.99") + '(' + AmtStringA + ')</Data></Cell>\n'.
    
PUT STREAM vvs UNFORMATTED '</Row>'.

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
RETURN.
  

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
  
  
