/* Отчет по перечислению  */
/* kam */



{globals.i}
{pp-corr.p}
{prn-doc.def &with_proc=YES}
{date.fun}

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.
def var commiss as decimal no-undo init 35.
def var kasko as logical no-undo view-as toggle-box init true.
def var summVozv as decimal no-undo.
def var summPrem as decimal no-undo.
def var AmtString as char no-undo.
def var DecStr as decimal no-undo.
def var AmtString2 as char no-undo.
def buffer aop for op.
def buffer bloan for loan.
def buffer aop-entry for op-entry.
def var opdate as date no-undo.
def var numdoc as char no-undo.
def var vidstr as char no-undo.
def var details as char no-undo.
  
DEFINE TEMP-TABLE otchakc
        FIELD cont_code    AS CHAR                 /* номер договора */
        FIELD name_cl      AS CHAR                 /* наименование заемщика */
        FIELD open_date    AS DATE                 /* дата договора */
        FIELD op_date   AS DATE
        FIELD summPrem     AS DECIMAL              /* сумма страх премии */
        FIELD summVozv     AS DECIMAL              /* сумма возврата */
        FIELD numdoc     AS CHAR
        FIELD vidstr	AS CHAR
        FIELD details	AS CHAR
        INDEX cont_code cont_code       
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
	 kasko label "КС/С1(C2)" at row 4 col 19
	 with 1  COL 1 down
	 
width 50 CENTERED OVERLAY ROW 10 TITLE "Данные для отчета ".

  do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	beg-date
	end-date
	commiss
	kasko

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


for each loan where loan.open-date >= beg-date
    and loan.open-date <= end-date
    and loan.class-code = 'insurance'
    and loan.contract = 'СТРАХ'
    and loan.parent-contract = 'Кредит'
    and loan.cust-cat = 'Ю'
    and loan.cust-id = 15340
    no-lock:
       if (kasko = true and length(loan.doc-ref) > 2 and substring(loan.doc-ref,(length(loan.doc-ref) - 1)) = 'КС')
          or (kasko = false and length(loan.doc-ref) > 2 and (substring(loan.doc-ref,(length(loan.doc-ref) - 1)) = 'С1' or substring(loan.doc-ref,(length(loan.doc-ref) - 1)) = 'С2')) 

 THEN DO:
       find first bloan where bloan.contract = loan.parent-contract 
       and bloan.cont-code = loan.parent-cont-code
       and bloan.loan-status <> 'СОЗД' no-lock no-error.
       if not avail bloan then next.
       summVozv = 0.
       summPrem = 0.
       nameCl = ''.
       numdoc = ''.
       details = ''.
       opdate = today.
       find last loan-acct where loan-acct.contract = loan.parent-contract
           and loan-acct.cont-code = loan.parent-cont-code 
           and loan-acct.acct-type = 'КредРасч'
           no-lock no-error.
           if avail loan-acct then do:
               find first acct of loan-acct no-lock no-error.
               if avail acct then do:
                   RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).
               end.
               for each op where 
             /*  op.op-date >= beg-date 
               and op.op-date <= end-date 
               and */
               op.ben-acct = '40701810000000007546'
               and op.inn = '7744000253'
               and op.op-status begins "√" no-lock,
               each op-entry of op where op-entry.acct-db = loan-acct.acct no-lock:
                   summPrem = abs(op-entry.amt-rub).
                   opdate = op.op-date.
                   numdoc = op.doc-num.
                   summVozv = abs(op-entry.amt-rub) * commiss / 100.
		details = trim(details + ' ' + op.details).
               end.
       end.
   vidstr = GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,
                             "vidstr",
                             "").


       create otchakc.            
            assign
                otchakc.cont_code = loan.doc-ref
                otchakc.name_cl = nameCl
                otchakc.open_date = loan.open-date
                otchakc.op_date = opdate
                otchakc.summPrem = summPrem
                otchakc.summVozv = summVozv
                otchakc.numdoc = numdoc
                otchakc.vidstr = vidstr
		otchakc.details = details
                .
            release otchakc.
    END.
end.


DEF VAR fname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF NEW SHARED STREAM vvs.
/* выводим в ексель */
    fname = "./otchakc1"  + "_" + USERID('bisquit') + ".xml".
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
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>  
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>   
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер ПП</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата заключения договора</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>    
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО Страхователя</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Назначение платежа</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вид страхования</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Страховая премия</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вознаграждение, %</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Вознаграждение, руб</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер документа</Data></Cell>      
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата проводки</Data></Cell>
   </Row>
'.
                
                
DEF VAR ii as INT NO-UNDO init 0.          
FOR EACH otchakc NO-LOCK BY otchakc.name_cl BY otchakc.open_date:
	ii = ii + 1.
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(ii) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED STRING(YEAR(otchakc.open_date),"9999") + '-' + STRING(MONTH(otchakc.open_date),"99") + '-' + STRING(DAY(otchakc.open_date),"99") + 'T00:00:00.000' + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.cont_code + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.name_cl + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.details + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otchakc.vidstr + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otchakc.summPrem,"->>>>>>>>>9.99") + '</Data></Cell>\n'.  
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(commiss,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otchakc.summVozv,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otchakc.numdoc) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED STRING(YEAR(otchakc.op_date),"9999") + '-' + STRING(MONTH(otchakc.op_date),"99") + '-' + STRING(DAY(otchakc.op_date),"99") + 'T00:00:00.000' + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '</Row>'.
END.    


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
  
  
