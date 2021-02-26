/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  closaccyr.p
      Comment:  Ведмость счетов, не работающих 1 и 2 года.
         Uses:
      Used by:  -
      Created:  ayv
*/
{globals.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get tmess}
{tmprecid.def}
{getdate.i}

def new shared stream vvs.
def var fname as char no-undo.
DEF VAR mTmpStr AS CHAR NO-UNDO.
DEF VAR ownr AS CHAR NO-UNDO.
DEF VAR datact AS DATE NO-UNDO.
DEF VAR dwnmonth AS DEC NO-UNDO.
DEF VAR chkdate1 AS DATE NO-UNDO.
DEF VAR chkdate2 AS DATE NO-UNDO.
DEF VAR tempDate AS DATE NO-UNDO.


DEF TEMP-TABLE acct-yr
	FIELD branch-id AS CHAR
	FIELD number AS CHAR
	FIELD Details AS CHAR
	FIELD open-date AS DATE
	FIELD balance AS CHAR
	FIELD acti-date AS DATE
	FIELD dwntime AS DEC
	INDEX branch-id IS PRIMARY branch-id ASCENDING
	INDEX number number ASCENDING.
	
	
chkdate1 = DATE(MONTH(end-date),DAY(end-date),YEAR(end-date) - 1).
chkdate2 = DATE(MONTH(end-date),DAY(end-date),YEAR(end-date) - 2).

/*---Поиск записей----*/
FOR EACH tmprecid,
   FIRST acct WHERE
      RECID(acct) EQ tmprecid.id
NO-LOCK:
	
	
	/*---------Проверка на активность в течении последнего года-----------*/
	FIND FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
							  AND chkdate1 <= op-entry.op-date 
							  USE-INDEX entry-cr NO-LOCK NO-ERROR.							  
	IF NOT AVAIL op-entry THEN DO:
	
		FIND FIRST op-entry WHERE op-entry.acct-db EQ acct.acct
							  AND chkdate1 <= op-entry.op-date 
							  USE-INDEX entry-db NO-LOCK NO-ERROR.			
		/*----------Проверка на активность от года и больше-----------------*/
		IF NOT AVAIL op-entry THEN DO:
		
			FIND FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
							  AND chkdate1 > op-entry.op-date 
							  USE-INDEX entry-cr NO-LOCK NO-ERROR.
			IF AVAIL op-entry THEN DO:
			
				IF acct.cust-cat EQ "Ю" 
				THEN DO :
					
						FIND FIRST cust-corp WHERE acct.cust-id EQ cust-corp.cust-id NO-LOCK NO-ERROR.
						ownr = cust-corp.name-short.
						
				END.
				ELSE IF acct.cust-cat EQ "Ч" THEN DO:
				
					FIND FIRST person WHERE acct.cust-id EQ person.person-id NO-LOCK NO-ERROR.
					IF getxattrvalue("person",string(person.person-id),"Субъект") EQ "ФЛП" THEN 
					ownr = "ИП " + person.name-last + " " + person.first-name.
					ELSE ownr = person.name-last + " " + person.first-name.
					
				END.
				datact = op-entry.op-date.
				tempDate = datact.
				FOR EACH op-entry WHERE op-entry.op-date > datact
									AND (op-entry.acct-cr EQ acct.acct OR op-entry.acct-db EQ acct.acct) NO-LOCK
									BY op-entry.op-date:
									
					IF op-entry.op-date > tempDate THEN tempDate = op-entry.op-date.
					
				END.
				datact = tempDate.
				dwnmonth = TRUNCATE (((end-date - tempDate) / 30), 0).
				RUN acct-pos IN h_base (acct.acct,acct.currency,end-date,end-date,gop-status).
				mTmpStr = STRING (ABSOLUTE(IF acct.currency EQ "" THEN sh-bal ELSE sh-val)).
			
				CREATE acct-yr.
				ASSIGN
					acct-yr.branch-id = acct.branch-id
					acct-yr.number = acct.number
					acct-yr.Details = ownr
					acct-yr.open-date = acct.open-date
					acct-yr.balance = mTmpStr
					acct-yr.acti-date = datact
					acct-yr.dwntime = dwnmonth.
					
			END.
			/*----------Счет не активен, проверяем дату открытия-----------------*/	
			ELSE DO:
			
				FIND FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
							  AND chkdate1 > op-entry.op-date 
							  USE-INDEX entry-cr NO-LOCK NO-ERROR.
				IF AVAIL op-entry THEN DO:
				
					IF acct.cust-cat EQ "Ю" 
						THEN DO:
					
							FIND FIRST cust-corp WHERE acct.cust-id EQ cust-corp.cust-id NO-LOCK NO-ERROR.
							ownr = cust-corp.name-short.
						
						END.
						ELSE IF acct.cust-cat EQ "Ч" THEN DO:
				
							FIND FIRST person WHERE acct.cust-id EQ person.person-id NO-LOCK NO-ERROR.
							IF getxattrvalue("person",string(person.person-id),"Субъект") EQ "ФЛП" THEN 
							ownr = "ИП " + person.name-last + " " + person.first-name.
							ELSE ownr = person.name-last + " " + person.first-name.
					
						END.
				
						datact = op-entry.op-date.
						tempDate = datact.
						FOR EACH op-entry WHERE op-entry.op-date > datact
									AND (op-entry.acct-cr EQ acct.acct OR op-entry.acct-db EQ acct.acct) NO-LOCK
									BY op-entry.op-date:
									
							IF op-entry.op-date > tempDate THEN tempDate = op-entry.op-date.
							
						END.
						datact = tempDate.
						dwnmonth = TRUNCATE (((end-date - tempDate) / 30), 0).
						RUN acct-pos IN h_base (acct.acct,acct.currency,end-date,end-date,?).
						mTmpStr = STRING(ABSOLUTE(IF acct.currency EQ "" THEN sh-bal ELSE sh-val)).
			
						CREATE acct-yr.
						ASSIGN
							acct-yr.branch-id = acct.branch-id
							acct-yr.number = acct.number
							acct-yr.Details = ownr
							acct-yr.open-date = acct.open-date
							acct-yr.balance = mTmpStr
							acct-yr.acti-date = datact
							acct-yr.dwntime = dwnmonth.
						
				END.
				/*----------Счет не активен, проверяем дату открытия-----------------*/
				ELSE DO:
				
					IF chkdate1 > acct.open-date THEN DO:
					
						dwnmonth = TRUNCATE (((end-date - acct.open-date) / 30), 0).
						RUN acct-pos IN h_base (acct.acct,acct.currency,end-date,end-date,gop-status).
						mTmpStr = STRING(ABSOLUTE(IF acct.currency EQ "" THEN sh-bal ELSE sh-val)).
			
						CREATE acct-yr.
						ASSIGN
							acct-yr.branch-id = acct.branch-id
							acct-yr.number = acct.number
							acct-yr.Details = ownr
							acct-yr.open-date = acct.open-date
							acct-yr.balance = mTmpStr
							acct-yr.acti-date = acct.open-date
							acct-yr.dwntime = dwnmonth.
							
					END.
					
				END.
				
			END.
			
		END.
		
	END.
	
END.

fname = "./closaccyr_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
put stream vvs unformatted '<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"\n
 xmlns:o="urn:schemas-microsoft-com:office:office"\n
 xmlns:x="urn:schemas-microsoft-com:office:excel"\n
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"\n
 xmlns:html="http://www.w3.org/TR/REC-html40">\n
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">\n
  <Version>14.00</Version>\n
 </DocumentProperties>\n
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">\n
  <AllowPNG/>\n
 </OfficeDocumentSettings>\n
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">\n
  <ProtectStructure>False</ProtectStructure>\n
  <ProtectWindows>False</ProtectWindows>\n
 </ExcelWorkbook>\n
 <Styles>\n
  <Style ss:ID="Default" ss:Name="Normal">\n
   <Alignment ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="s65">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="s79">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
   <Interior ss:Color="#E26B0A" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="s22">\n
   <NumberFormat ss:Format="dd.mm.yyyy"/>\n
  </Style>\n
  <Style ss:ID="s82">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="2"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="2"/>\n
   </Borders>\n
  </Style>\n
 </Styles>\n
 <Worksheet ss:Name="1 год">\n
  <Table ss:ExpandedColumnCount="7" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
    <Column ss:AutoFitWidth="0" ss:Width="27.75"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="110.25"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="190.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="76.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="66"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="81.75"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="77.25"/>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="6" ss:StyleID="s79"><Data ss:Type="String">ВЕДОМОСТЬ НЕИСПОЛЬЗУЮЩИХСЯ СЧЕТОВ.(1 ГОД)</Data></Cell>\n
   </Row>\n
   <Row ss:Height="15.75">\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Офис</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Номер счета</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Наименование счета</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Дата открытия</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Остаток</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Дата активности</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Простой (мес.)</Data></Cell>\n
   </Row>\n
'.

FOR EACH acct-yr WHERE acct-yr.acti-date > chkdate2 NO-LOCK BY acct-yr.branch-id BY acct-yr.number :

	PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.branch-id + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.number + '</Data></Cell>\n'. 
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.Details + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s22"><Data ss:Type="DateTime">' + string(year(acct-yr.open-date),"9999") + '-' + string(month(acct-yr.open-date),"99") + '-' + string(day(acct-yr.open-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.balance + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s22"><Data ss:Type="DateTime">' + string(year(acct-yr.acti-date),"9999") + '-' + string(month(acct-yr.acti-date),"99") + '-' + string(day(acct-yr.acti-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">' + trim(string(acct-yr.dwntime, ">99")) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '</Row>\n'.
END.

PUT STREAM vvs UNFORMATTED 
'</Table>\n
 </Worksheet>\n
 <Worksheet ss:Name="2 года">\n
  <Table ss:ExpandedColumnCount="7" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
    <Column ss:AutoFitWidth="0" ss:Width="27.75"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="110.25"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="190.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="76.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="66"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="81.75"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="77.25"/>\n
   <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
    <Cell ss:MergeAcross="6" ss:StyleID="s65"><Data ss:Type="String">ВЕДОМОСТЬ НЕИСПОЛЬЗУЮЩИХСЯ СЧЕТОВ.(2 ГОДА)</Data></Cell>\n
   </Row>\n
   <Row ss:Height="15.75">\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Офис</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Номер счета</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Наименование счета</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Дата открытия</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Остаток</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Дата активности</Data></Cell>\n
    <Cell ss:StyleID="s82"><Data ss:Type="String">Простой (мес.)</Data></Cell>\n
   </Row>\n
'.

FOR EACH acct-yr WHERE acct-yr.acti-date < chkdate2 NO-LOCK BY acct-yr.branch-id BY acct-yr.number:

	PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.branch-id + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.number + '</Data></Cell>\n'. 
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">' + acct-yr.Details + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s22"><Data ss:Type="DateTime">' + string(year(acct-yr.open-date),"9999") + '-' + string(month(acct-yr.open-date),"99") + '-' + string(day(acct-yr.open-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell ><Data ss:Type="String">' + acct-yr.balance + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s22"><Data ss:Type="DateTime">' + string(year(acct-yr.acti-date),"9999") + '-' + string(month(acct-yr.acti-date),"99") + '-' + string(day(acct-yr.acti-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">' + trim(string(acct-yr.dwntime, ">99")) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '</Row>\n'.

END.

put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
  
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").
/*---------------------------*/