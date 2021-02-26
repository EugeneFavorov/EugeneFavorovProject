/* ведомость по птс */
/* kam + vvv */

FUNCTION GetFIO RETURN CHAR(INPUT iUser AS CHAR):
    DEFINE VARIABLE FIO AS CHAR NO-UNDO INIT "".
    FIND FIRST _user WHERE _user._userid = iUser NO-LOCK NO-ERROR.
    IF AVAILABLE _user THEN FIO = _user._User-Name.
    ELSE DO:
        FIND FIRST code WHERE code.class EQ 'strahpol'
            AND code.parent EQ 'strahpol'
            AND code.code = iUser NO-LOCK NO-ERROR.
        IF AVAILABLE code THEN FIO = code.description[1].
    END.
    IF iUser <> ? THEN FIO = FIO + '(' + iUser + ')'.
    RETURN FIO.
END FUNCTION.

{globals.i}
{tmprecid.def}

def var nameCl as char no-undo.
def var numAcct as char no-undo.
def var contCode as char no-undo.
define var summPost as decimal no-undo.
define var summPer as decimal no-undo.
define var srPost1 as recid no-undo.
define var dtbank1 as recid no-undo.
define var dtarch1 as recid no-undo.
define var dtizat1 as recid no-undo.
define var srPost2 as recid no-undo.
define var dtbank2 as recid no-undo.
define var dtarch2 as recid no-undo.
def var tmp_type as char no-undo.
def var tmp_role as char no-undo.
def var fl as integer init 1 no-undo.
define var ii as int no-undo.

DEFINE TEMP-TABLE otchpts
        FIELD cont_code    AS CHAR                 /* номер КД */
        FIELD open_date    AS DATE                 /* дата КД */
        FIELD name_cl      AS CHAR                 /* ФИО заемщика */
        FIELD sernom       AS CHAR                 /* серия */
        FIELD srokpost1    AS DATE                 /* срок предоставления ПТС в Банк */
        FIELD datebank1    AS DATE                 /* дата поступления ПТС в Банк */ 
		FIELD whoin		   AS CHAR				   /* кто внес дату ПостБанк */		
        FIELD datearch1    AS DATE                 /* дата поступления ПТС в Архив */
        FIELD comment1     AS CHAR                 /* комментарий */         
        FIELD whoprin1     AS CHAR                 /* кто принял */         
        FIELD dateizat1    AS DATE                 /* дата изьятия */
        FIELD osnizat1     AS CHAR                 /* основание изьятия */ 
        FIELD whovyd1      AS CHAR                 /* кто выдал */  
        FIELD whovyd2      AS CHAR                 /* кто принял */                   
        FIELD srokpost2    AS DATE                 /* срок предоставления ПТС в Банк */
        FIELD datebank2    AS DATE                 /* дата поступления ПТС в Банк */        
        FIELD datearch2    AS DATE                 /* дата поступления ПТС в Архив */
        FIELD whoprin2     AS CHAR                 /* кто принял */  
        INDEX cont_code cont_code       
    .

{empty otchpts}


  def new shared stream vvs.
  def var fname as char no-undo.
    
    fname = "./otchpts"  + "_" + userid('bisquit') + ".xml".


/* по отобранным КД */
for each tmprecid,
    first loan
	where recid(loan) eq tmprecid.id
	no-lock:
        ii = 0.
		/* идем по договорам обеспечения */
        FOR EACH term-obl
            WHERE term-obl.contract = 'Кредит'
            AND term-obl.cont-code = loan.cont-code
            AND term-obl.idnt = 5
            NO-LOCK:
            
            /* получаем имя клиента */
            RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT nameCl).
			/* plus.vvv */
			/* действующий ли договор обеспечения - не было ли списания? */
			/* из картотеки счетов КД нужно по роли найти нужный счет */
			/* посмотреть не было ли проводки */
			tmp_type = ?.
			tmp_role = ?.
			FL = 1.
			/* найдем виддогоб */
            FIND FIRST signs
                WHERE signs.file-name = 'term-obl'
                AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
                AND signs.code = 'виддогоб'
            NO-LOCK NO-ERROR.
			/**/
				IF AVAIL signs THEN
					tmp_type = signs.xattr-value.
			/**/
			
			/* найдем роль */
			IF term-obl.nn = 0 THEN
				tmp_role = tmp_type.
			ELSE
				tmp_role = tmp_type + string(term-obl.nn).
			
			/* найдем счет по роли */
			FIND FIRST LOAN-ACCT OF LOAN
				WHERE LOAN-ACCT.ACCT-TYPE = tmp_role
			NO-LOCK NO-ERROR.
			/**/
/*				IF AVAIL LOAN-ACCT THEN
					DO:
/* message loan-acct.acct view-as alert-box. */
						/* посмотрим есть ли проводка ПРИХОД */
						FIND FIRST OP-ENTRY
						       WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
						       AND OP-ENTRY.OP-STATUS >= "√"
						NO-LOCK NO-ERROR.
						/**/
							IF NOT AVAIL OP-ENTRY THEN
								DO:
									FL = 0.
								END.						
						/**/					
						/* посмотрим есть ли проводка СПИСАНИЯ */
						FIND FIRST OP-ENTRY
							WHERE OP-ENTRY.ACCT-DB = LOAN-ACCT.ACCT
							AND OP-ENTRY.OP-STATUS >= "√"
						NO-LOCK NO-ERROR.
						/**/
							IF AVAIL OP-ENTRY THEN
								DO:
									FL = 0.
								END.						
						/**/
					END.
				ELSE
					FL = 0.
			/**/	
			
*/			IF FL = 1 THEN
				DO:				
				/* plus.vvv */
					
					/* создаем запись */
					CREATE otchpts.
					/**/
					ASSIGN 
						otchpts.cont_code = loan.doc-ref
						otchpts.open_date = loan.open-date
						otchpts.name_cl = nameCl
				  /*       otchpts.  */
						.

					/* номер документа */
					FIND FIRST signs
						WHERE signs.file-name = 'term-obl'
						AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
						AND signs.code = 'TCNUMB'
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL signs THEN
							otchpts.sernom = signs.xattr-value.
								
					/* серия документа */
					FIND FIRST signs
						WHERE signs.file-name = 'term-obl'
						AND signs.surrogate = 'Кредит,'+ STRING(loan.cont-code) + ",5," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
						AND signs.code = 'TCSER'
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL signs THEN 
							otchpts.sernom =  TRIM(otchpts.sernom + ' ' + signs.xattr-value).             
						   
					ContCode = term-obl.cont-code + ' ' + string(term-obl.nn).
					
					srPost1 = ?.
					dtbank1 = ?.
					dtarch1 = ?.
					dtizat1 = ?.
					srPost2 = ?.
					dtbank2 = ?.
					dtarch2 = ?.
					/**/
					FOR EACH pl_indocsreg
						WHERE pl_indocsreg.doc_type eq "ПТС"
						AND pl_indocsreg.file_name eq "term-obl"
						AND pl_indocsreg.surrogate eq ContCode
						NO-LOCK BY RECID(pl_indocsreg):
						/**/
						CASE pl_indocsreg.event:
							WHEN 'СрокСдачи' THEN
								DO:
									IF srPost1 = ? THEN
										DO:
											otchpts.srokpost1 = pl_indocsreg.date_value.
											srPost1 = recid(pl_indocsreg).
										END.
								END.
							WHEN 'ПостБанк' THEN
								DO:
									IF dtbank1 = ? THEN
										DO:
											otchpts.datebank1 = pl_indocsreg.date_value.
											dtbank1 = recid(pl_indocsreg).
											/**/
											otchpts.whoin = GetFIO(pl_indocsreg.user_id). 
											/**/
										END.
								END.            
							WHEN 'ПостАрх' THEN
								DO:
									IF dtarch1 = ? THEN
										DO:
											otchpts.datearch1 = pl_indocsreg.date_value.
											otchpts.comment1 = pl_indocsreg.details.
											otchpts.whoprin1 = GetFIO(pl_indocsreg.user_id). 
											dtarch1 = recid(pl_indocsreg).
										END.
								END.             
						END CASE.
					END.
					
					FOR EACH pl_indocsreg
						WHERE pl_indocsreg.doc_type eq "ПТС"
						AND pl_indocsreg.file_name eq "term-obl"
						AND pl_indocsreg.surrogate eq ContCode
						NO-LOCK BY RECID(pl_indocsreg) DESCENDING:
						/**/
						CASE pl_indocsreg.event:
							WHEN 'ПостАрх' THEN DO:
								IF dtarch1 <> recid(pl_indocsreg) AND dtarch2 = ? AND dtbank2 = ? AND srPost2 = ? AND dtizat1 = ? THEN DO:
									otchpts.datearch2 = pl_indocsreg.date_value.
									otchpts.whoprin2 = GetFIO(pl_indocsreg.user_id). 
									dtarch2 = recid(pl_indocsreg).
								END.
							END.
							WHEN 'ПостБанк' THEN DO:
								IF dtbank1 <> recid(pl_indocsreg) AND dtbank2 = ? AND srPost2 = ? AND dtizat1 = ? THEN DO:
									otchpts.datebank2 = pl_indocsreg.date_value.
									dtbank2 = recid(pl_indocsreg).
								END.
							END. 
							WHEN 'СрокСдачи' THEN DO:
								IF srPost1 <> recid(pl_indocsreg) AND srPost2 = ? AND dtizat1 = ? THEN DO:
									otchpts.srokpost2 = pl_indocsreg.date_value.
									srPost2 = recid(pl_indocsreg).
								END.
							END.                    
							WHEN 'ИзъятАрх' THEN DO:
								IF dtizat1 = ? THEN DO:
									otchpts.dateizat1 = pl_indocsreg.date_value.
									otchpts.osnizat1 = pl_indocsreg.details.
									otchpts.whovyd1 = GetFIO(pl_indocsreg.user_id).
									otchpts.whovyd2 = GetFIO(pl_indocsreg.descriptions). 
									dtizat1 = recid(pl_indocsreg).
								END.
							END.  
						END CASE.
					END.
					
				RELEASE otchpts.            
				ii = ii + 1.
				END.
        END.
           
end.


output stream vvs to value (fname)
	UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
put stream vvs unformatted 
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="100.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="75.75" ss:Span="1"/>
   <Column ss:Index="5" ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="101.25"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="111.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="99.75"/>
   
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="99.75"/>
   
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="89.25"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="96.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="83.25"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="93"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="108"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="105"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="108.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80.25"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Кредитный договор</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата договора</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО заемщика</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">серия и № ПТС</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">срок предоставления ПТС в Банк</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">дата поступления ПТС в Банк</Data></Cell>
	
	<Cell ss:StyleID="s67"><Data ss:Type="String">кто внес</Data></Cell>
	
    <Cell ss:StyleID="s67"><Data ss:Type="String">дата поступления ПТС в Архив</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">комментарии к ПТС</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">кто принял</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">дата изъятия ПТС из Архива</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">основание изъятия</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">кто выдал</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">кому выдано</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">срок возврата ПТС в Банк</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">дата возврата ПТС в Банк</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">дата возврата ПТС в Архив</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">кто принял </Data></Cell>
   </Row>

'.

    FOR EACH otchpts:
        /*
        FIELD cont_code    AS CHAR                 /* номер КД */
        FIELD name_cl      AS CHAR                 /* ФИО заемщика */
        FIELD sernom       AS CHAR                 /* серия */
        FIELD srokpost1    AS DATE                 /* срок предоставления ПТС в Банк */
        FIELD datebank1    AS DATE                 /* дата поступления ПТС в Банк */   
		FIELD whoin		   AS CHAR				   /* кто внес дату ПостБанк */			
        FIELD datearch1    AS DATE                 /* дата поступления ПТС в Архив */
        FIELD comment1     AS CHAR                 /* комментарий */         
        FIELD whoprin1     AS CHAR                 /* кто принял */         
        FIELD dateizat1    AS DATE                 /* дата изьятия */
        FIELD osnizat1     AS CHAR                 /* основание изьятия */                 
        FIELD whovyd1      AS CHAR                 /* кто выдал */  
        FIELD whovyd2      AS CHAR                 /* кто принял */                   
        FIELD srokpost2    AS DATE                 /* срок предоставления ПТС в Банк */
        FIELD datebank2    AS DATE                 /* дата поступления ПТС в Банк */        
        FIELD datearch2    AS DATE                 /* дата поступления ПТС в Архив */
        FIELD whoprin2     AS CHAR                 /* кто принял */ 
        */
        
    put stream vvs unformatted
    '
    <Row ss:AutoFitHeight="0">
        <Cell><Data ss:Type="String">'.  
        put stream vvs unformatted otchpts.cont_code + '</Data></Cell>'.
		/**/
        IF otchpts.open_date <> ? THEN
			DO:   
				put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.open_date),"9999") + '-' + string(month(otchpts.open_date),"99") + '-' + string(day(otchpts.open_date),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.    
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.name_cl + '</Data></Cell><Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.sernom + '</Data></Cell>'.
		/**/
        IF otchpts.srokpost1 <> ? THEN
			DO:   
				put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.srokpost1),"9999") + '-' + string(month(otchpts.srokpost1),"99") + '-' + string(day(otchpts.srokpost1),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.    
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/*ayv добавлена проверка на дату*/
		IF otchpts.srokpost1 < 01/01/2000 THEN 
				MESSAGE "Неверный срок предоставления ПТС в банк на договоре " + otchpts.cont_code VIEW-AS ALERT-BOX.
		/**/
        IF otchpts.datebank1 <> ? THEN
			DO:   
				put stream vvs unformatted '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.datebank1),"9999") + '-' + string(month(otchpts.datebank1),"99") + '-' + string(day(otchpts.datebank1),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.
        ELSE 
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/*ayv добавлена проверка на дату*/
		IF otchpts.datebank1 < 01/01/2000 THEN 
				MESSAGE "Неверная дата поступления ПТС в банк на договоре " + otchpts.cont_code VIEW-AS ALERT-BOX.
		/**/
		IF otchpts.whoin <> ? THEN
			DO:
				put stream vvs unformatted '<Cell><Data ss:Type="String">' + string(otchpts.whoin) + '</Data></Cell>'.
			END.
		ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
        IF otchpts.datearch1 <> ? THEN
			DO:
				put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.datearch1),"9999") + '-' + string(month(otchpts.datearch1),"99") + '-' + string(day(otchpts.datearch1),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.     
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.comment1 + '</Data></Cell>'.
		put stream vvs unformatted '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.whoprin1 + '</Data></Cell>'.
		/**/
        IF otchpts.dateizat1 <> ? THEN
			DO:
				put stream vvs unformatted '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.dateizat1),"9999") + '-' + string(month(otchpts.dateizat1),"99") + '-' + string(day(otchpts.dateizat1),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.        
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.osnizat1 + '</Data></Cell>        
		<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.whovyd1 + '</Data></Cell>        
		<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.whovyd2 + '</Data></Cell>'.
		/**/
        IF otchpts.srokpost2 <> ? THEN
			DO:       
				put stream vvs unformatted '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.srokpost2),"9999") + '-' + string(month(otchpts.srokpost2),"99") + '-' + string(day(otchpts.srokpost2),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/	
        IF otchpts.datebank2 <> ? THEN
			DO:                      
				put stream vvs unformatted '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.datebank2),"9999") + '-' + string(month(otchpts.datebank2),"99") + '-' + string(day(otchpts.datebank2),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
        IF otchpts.datearch2 <> ? THEN
			DO:  
				put stream vvs unformatted '<Cell><Data ss:Type="DateTime">'.
				put stream vvs unformatted string(year(otchpts.datearch2),"9999") + '-' + string(month(otchpts.datearch2),"99") + '-' + string(day(otchpts.datearch2),"99") + 'T00:00:00.000' + '</Data></Cell>'.
			END.
        ELSE
			put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>'.
		/**/
		put stream vvs unformatted '<Cell><Data ss:Type="String">'.
		put stream vvs unformatted otchpts.whoprin2 + '</Data></Cell>'.
		put stream vvs unformatted '</Row>'.
    
    END.

put stream vvs unformatted 
'  </Table>
 </Worksheet>
</Workbook>
' .

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").

/**/
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
				DO:
					/* ФИО клиента */
					sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
				END.
	END.
 ELSE
	DO:
		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
				DO:
					/* наименование организации */
					sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
				END.
	END.
END.
  
  
  