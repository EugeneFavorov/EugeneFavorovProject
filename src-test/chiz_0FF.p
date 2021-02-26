{globals.i}
{intrface.get tmess}
{intrface.get xclass}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: данные по документам и изменении статуса документов
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

DEF NEW SHARED STREAM puk.
DEF VAR fname 		AS CHAR  INIT "./DOCUMENTS.csv"  NO-UNDO.

/**/
DEFINE TEMP-TABLE doc
    FIELD zid	 	AS INTEGER		/* идентификатор */
	FIELD znumb		AS CHAR			/* номер документа */
	FIELD zdb		AS CHAR			/* дебет */
	FIELD zcr		AS CHAR			/* кредит */
	FIELD zopdate	AS DATE			/* дата платежа */
	FIELD zopsum	AS DEC			/* сумма платежа */
	FIELD zcr_doc	AS DATETIME		/* дата-время постановки */
	FIELD zFBN		AS DATETIME		/* дата-время присвоения статуса ФБН */
	FIELD zWHO_FBN	AS CHAR			/* сотрудник присвоил ФБН */
	FIELD zFBO		AS DATETIME		/* дата-время присвоения статуса ФБО */
	FIELD zV		AS DATETIME 	/* дата-время присвоения галочки */
	.

{empty doc}

DEFINE TEMP-TABLE doc_hist
	FIELD fprn		AS INTEGER		/* идентификатор документа */
	FIELD fstatus	AS CHAR			/* значение статуса */
	FIELD fdate		AS DATETIME		/* когда внесли изменения */
	FIELD fwho		AS CHAR			/* кто внес изменения */
.

/* спросим даты */
{getdates.i}

FOR EACH op-entry
	WHERE (
			( ( op-entry.acct-db BEGINS "407" OR op-entry.acct-db BEGINS "40802" ) AND op-entry.acct-cr BEGINS "30102" )
			OR
			( op-entry.acct-db BEGINS "30102" AND ( op-entry.acct-cr BEGINS "407" OR op-entry.acct-cr BEGINS "40802" ) )
		)
	AND op-entry.op-date <= beg-date
	AND op-entry.op-date >= end-date
	/*
	AND op-entry.op = 4032889
	*/
	NO-LOCK,
		/**/
		FIRST op OF op-entry
		WHERE op.op-status BEGINS chr(251)
		NO-LOCK:
		
			/**/
			CREATE doc.
			/**/
			ASSIGN
				doc.zid = op.op
				doc.znumb = op.doc-num
				doc.zdb = substring(op-entry.acct-db, 1, 20)
				doc.zcr = substring(op-entry.acct-cr, 1, 20)
				doc.zopdate = op-entry.op-date
				doc.zopsum = op-entry.amt-rub
			.
			
			/* идем по истории */
			FOR EACH HISTORY 
			WHERE HISTORY.FILE-NAME = "op"
			AND HISTORY.FIELD-REF = STRING(op.op)
			NO-LOCK
			BY RECID(history):
			
				/* дата постановки */
				IF HISTORY.MODIFY = "C" THEN
					DO:
						/* дата постановки */
						doc.zcr_doc = DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000).
						/* нужно создать запись истории */
						/**/
						CREATE doc_hist.
						/**/
						ASSIGN
							doc_hist.fprn = op.op
							doc_hist.fdate = DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000)
							doc_hist.fwho = HISTORY.USER-ID
						.
						/**/
					END.
					
				/*  */
				IF INDEX(HISTORY.FIELD-VALUE, "op-status") > 0 THEN
					DO:
						/* вставим значение старого статуса */
						doc_hist.fstatus = SUBSTRING(HISTORY.FIELD-VALUE,
													INDEX(HISTORY.FIELD-VALUE, ",", INDEX(HISTORY.FIELD-VALUE, "op-status")) + 1,
													INDEX(HISTORY.FIELD-VALUE, ",", INDEX(HISTORY.FIELD-VALUE, ",", INDEX(HISTORY.FIELD-VALUE, "op-status")) + 1)  -  INDEX(HISTORY.FIELD-VALUE, ",", INDEX(HISTORY.FIELD-VALUE, "op-status")) - 1).
						/**/
						/* и вновь создадим шаблон */
						CREATE doc_hist.
						/**/
						ASSIGN
							doc_hist.fprn = op.op
							doc_hist.fdate = DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000)
							doc_hist.fwho = HISTORY.USER-ID
						.						
						/**/
					END.
				/* */
				
			END.
			
			/* когда ливнули из истории - нужно поставить действующий статус */
			doc_hist.fstatus = op.op-status.
			/**/
END.

/*
/* вывод */
run instview.p(TEMP-TABLE doc_hist:HANDLE). 
/**/
*/


fname = "./DOCUMENTS_" + replace(string(beg-date,"99.99.9999"),".","_") + "_" + replace(string(end-date,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

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
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>27795</WindowWidth>
  <WindowTopX>480</WindowTopX>
  <WindowTopY>120</WindowTopY>
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
  <Style ss:ID="s17">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s18">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s19">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s20">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s21">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s22">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s23">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s24">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s25">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="General Date"/>
  </Style>
  <Style ss:ID="s26">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Документы">
  <Table x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s17" ss:DefaultRowHeight="15.75">
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="64.5"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="132.75"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="132.75"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="79.5"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="102.75"/>
   <Column ss:StyleID="s17" ss:Width="89.25"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="93"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="159.75"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="87.75"/>
   <Column ss:StyleID="s17" ss:AutoFitWidth="0" ss:Width="92.25"/>
   <Row>
    <Cell ss:MergeAcross="9" ss:StyleID="s26"><Data ss:Type="String">Данные о документах за период с ' + STRING(beg-date, "99.99.9999") + ' по ' + STRING(end-date, "99.99.9999") + '</Data></Cell>
   </Row>
   <Row ss:Index="3" ss:AutoFitHeight="0" ss:Height="38.25" ss:StyleID="s19">
    <Cell ss:StyleID="s18"><Data ss:Type="String">№ документа</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Дебет</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Кредит</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Дата платежа</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Сумма платежа</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Время постановки</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Время ФБН</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Сотрудник ФБН</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Время ФБО</Data></Cell>
    <Cell ss:StyleID="s18"><Data ss:Type="String">Время V</Data></Cell>
   </Row>'.

   /* построчно выводим данные о документах */
	FOR EACH doc
		BY doc.znumb:
		/**/
		put stream puk unformatted '<Row>'.
		put stream puk unformatted '<Cell ss:StyleID="s20"><Data ss:Type="String">' + STRING(doc.znumb) + '</Data></Cell>'.
		put stream puk unformatted '<Cell ss:StyleID="s22"><Data ss:Type="String">' + STRING(doc.zdb) + '</Data></Cell>'.
		put stream puk unformatted '<Cell ss:StyleID="s22"><Data ss:Type="String">' + STRING(doc.zcr) + '</Data></Cell>'.
		put stream puk unformatted '<Cell ss:StyleID="s23"><Data ss:Type="String">' + STRING(doc.zopdate, "99.99.9999") + '</Data></Cell>'.
		put stream puk unformatted '<Cell ss:StyleID="s24"><Data ss:Type="Number">' + STRING(doc.zopsum) + '</Data></Cell>'.
		put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + STRING(doc.zcr_doc, "99.99.99 HH:MM:SS") + '</Data></Cell>'.
		
		/* время ФБН */
		FIND FIRST doc_hist
		WHERE doc_hist.fprn = doc.zid
		AND doc_hist.fstatus = "ФБН"
		NO-LOCK NO-ERROR.
			/**/
			IF AVAIL doc_hist THEN
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + STRING(doc_hist.fdate, "99.99.99 HH:MM:SS") + '</Data></Cell>'.
					/**/
					FIND FIRST _USER
					WHERE _USER._USERID EQ doc_hist.fwho
					NO-LOCK NO-ERROR.
						/**/
						IF AVAILABLE _USER THEN
							DO:
								put stream puk unformatted '<Cell ss:StyleID="s20"><Data ss:Type="String">' + STRING(_USER._USER-NAME) + '</Data></Cell>'.
							END.
				END.
			ELSE
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + " " + '</Data></Cell>'.
					put stream puk unformatted '<Cell ss:StyleID="s20"><Data ss:Type="String">' + " " + '</Data></Cell>'.
				END.
			/**/
		
		/* время ФБО */
		FIND FIRST doc_hist
		WHERE doc_hist.fprn = doc.zid
		AND doc_hist.fstatus = "ФБО"
		NO-LOCK NO-ERROR.	
			/**/
			IF AVAIL doc_hist THEN
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + STRING(doc_hist.fdate, "99.99.99 HH:MM:SS") + '</Data></Cell>'.
				END.
			ELSE
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + " " + '</Data></Cell>'.
				END.
			/**/
			
		/* время V */
		FIND FIRST doc_hist
		WHERE doc_hist.fprn = doc.zid
		AND doc_hist.fstatus = chr(251)
		NO-LOCK NO-ERROR.	
			/**/
			IF AVAIL doc_hist THEN
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + STRING(doc_hist.fdate, "99.99.99 HH:MM:SS") + '</Data></Cell>'.
				END.
			ELSE
				DO:
					put stream puk unformatted '<Cell ss:StyleID="s25"><Data ss:Type="String">' + " " + '</Data></Cell>'.
				END.
			/**/
		put stream puk unformatted '</Row>'.
		/**/
	END.

	put stream puk unformatted
		'		</Table>
			</Worksheet>
		</Workbook>'.		

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
