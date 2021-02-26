{tmprecid.def}
{globals.i}
{sh-defs.i}
{client.i}

def temp-table avtoz NO-UNDO
  field NN		AS INT64
  field VIN		AS CHAR
  field MODEL 	AS CHAR
  field MARKA	AS CHAR
  field DEndZal	AS CHAR
.

DEF VAR GNI	AS CHAR		NO-UNDO.
DEF VAR vDataCl	AS INT64	NO-UNDO.
DEF VAR str	AS CHAR		NO-UNDO.
DEF VAR nn	AS INT64	NO-UNDO.
DEF VAR iCount	AS INT64	NO-UNDO.
DEF VAR MM AS CHAR NO-UNDO.
DEF VAR VIN AS CHAR NO-UNDO.
DEF VAR BR AS CHAR INIT "" NO-UNDO.

def var fname as char  init "./avto_ru.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).
def new shared stream vvs.

{justasec}
/**/
FUNCTION CheckRus RETURNS CHARACTER
        (
        sVIN AS CHARACTER
        ):
        /**/
        DEF VAR out_VIN AS CHAR NO-UNDO.
		DEF VAR cnt AS INT64 INIT 1 NO-UNDO.
		DEF VAR pos AS INT64 INIT 1 NO-UNDO.
		DEF VAR rus AS CHAR INIT " АВСЕНКМОРТХасеоху" NO-UNDO.
		DEF VAR eng AS CHAR INIT " ABCEHKMOPTXaceoxy" NO-UNDO.
        /**/
		out_VIN = sVIN.
		/**/
		DO WHILE cnt < LENGTH(out_VIN):
			/**/
			IF ASC(SUBSTRING(sVIN, cnt, 1)) >= 128 THEN
				DO:
					/* тогда заменим */
					/* найдем позицию в русской строке */
					pos = INDEX(rus, SUBSTRING(out_VIN, cnt, 1)).
					/**/
					IF pos = 0 THEN 
						pos = 1.
					/*
					MESSAGE "STRING = " + out_VIN + ", pos = " + STRING(pos) + ", letter = " + SUBSTRING(out_VIN, cnt, 1) VIEW-AS ALERT-BOX.
					*/
					
					/* заменям на соответствующую букву латиницы */
					out_VIN = REPLACE( out_VIN, SUBSTRING(out_VIN, cnt, 1), SUBSTRING(eng, pos, 1) ).
					/**/
				END.
			cnt = cnt + 1.
			/**/
		END.
	/**/
	RETURN out_VIN.
	/**/
END.
/**/
/*-----------------------------------*/
SELECT COUNT(*)
INTO iCount 
FROM signs,
	 loan	
WHERE signs.file-name = "term-obl"
and signs.code = "ВидОб"
and signs.xattr-value = "Автомобиль"
and loan.contract = ENTRY(1, signs.surrogate)
and loan.cont-code = ENTRY(2, signs.surrogate)
and loan.loan-status = "ВВЕД"
and (loan.close-date is null or loan.close-date >= today)
and exists
	(
	select *
	from  term-obl
	where term-obl.contract = loan.contract
	and term-obl.cont-code = loan.cont-code
	and term-obl.idnt = 5
	and term-obl.class-code = "term-obl-gar"
	and term-obl.fop-date <= today
	)
.

 {bar-beg2.i
  &BarTotal     = iCount
  &BarMessage   = "'Поиск залоговых автомобилей...'"
 }
/*-----------------------------------*/

/* идем по ДР договорам обеспечения */
FOR EACH SIGNS
	WHERE SIGNS.FILE-NAME = "term-obl"
	AND SIGNS.CODE = "ВидОб"
	AND SIGNS.XATTR-VALUE = "Автомобиль"
	NO-LOCK,
		/* находим КД */
		FIRST LOAN
			WHERE LOAN.CONTRACT = ENTRY(1, SIGNS.SURROGATE)
			AND LOAN.CONT-CODE = ENTRY(2, SIGNS.SURROGATE)
			AND LOAN.LOAN-STATUS = "ВВЕД"
			AND (LOAN.CLOSE-DATE EQ ? OR LOAN.CLOSE-DATE >= TODAY)
		NO-LOCK:

	/* находим договор обеспечения */
	FIND FIRST TERM-OBL
		WHERE TERM-OBL.CONTRACT = LOAN.CONTRACT
		AND TERM-OBL.CONT-CODE = LOAN.CONT-CODE
		AND TERM-OBL.IDNT = 5
		AND TERM-OBL.CLASS-CODE = "term-obl-gar"
		AND TERM-OBL.FOP-DATE <= today
	NO-LOCK NO-ERROR.
	/**/
		IF AVAIL TERM-OBL THEN
			DO:
				/**/
				nn = nn + 1.
				{bar2.i &BarPointer = nn}


				FIND LAST loan-acct
					WHERE loan-acct.contract = loan.contract
					AND loan-acct.cont-code = loan.cont-code
					AND loan-acct.acct-type EQ ('КредОб' + (if term-obl.nn eq 0 then '' else string(term-obl.nn)))
					/*AND loan-acct.acct begins '913'*/
					NO-LOCK NO-ERROR.
					
				IF AVAIL loan-acct THEN
				DO:
					RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, today, today, ?).
					IF sh-bal EQ 0 THEN NEXT.
					/* if loan-acct.cont-code eq '45-00-2656-АП@0400' THEN 
					message loan-acct.acct view-as alert-box. */
				END. ELSE NEXT.
										/*
										FIND LAST op-entry
											WHERE op-entry.acct-db = loan-acct.acct 
											AND (op-entry.op-status = '√' OR op-entry.op-status = '√√')
										NO-LOCK NO-ERROR.
											
											IF AVAIL op-entry THEN
												avtoz.DEndZal = string(op-entry.op-date).
												
									END.
							*/

					/**/
					MM = REPLACE(GetXattrValue("term-obl", SIGNS.SURROGATE,"TCmodel"), ",", " ").
					BR = GetXattrValue("term-obl", SIGNS.SURROGATE,"TCbrand").
					VIN = GetXattrValue("term-obl", SIGNS.SURROGATE,"TCVIN").
					/**/
					/* ОТСУТСТВУЕТ и НЕ УСТАНОВЛЕН */
					IF MM <> "" AND VIN <> ""  AND INDEX(CAPS(VIN), "УСТАН") = 0 AND INDEX(CAPS(VIN), "ОТСУ") = 0
					AND VIN <> "-" THEN 
						DO:
							CREATE avtoz.
							/**/
							avtoz.VIN = CheckRus(VIN).
							/**/
							IF BR = "" THEN 
								DO:
									/**/
									avtoz.MARKA = (IF SUBSTRING(MM, 1, INDEX(MM, " ") - 1) <> "" THEN SUBSTRING(MM, 1, INDEX(MM, " ") - 1) ELSE MM).
									avtoz.MODEL = (IF SUBSTRING(MM, INDEX(MM, " ") + 1) <> "" THEN SUBSTRING(MM, INDEX(MM, " ") + 1) ELSE MM).
									/**/
								END.
							ELSE
								DO:
									avtoz.MARKA = BR.
									avtoz.MODEL = MM.
								END.
							/**/
							avtoz.DEndZal =  STRING(
							    IF TERM-OBL.END-DATE < TODAY THEN TODAY ELSE TERM-OBL.END-DATE).
							/**/
							
							
						END.
			END.
END.

/**/
DEF VAR TMP_VIN AS CHAR NO-UNDO.

FOR EACH avtoz
EXCLUSIVE-LOCK
BREAK BY avtoz.VIN :
	/**/
	IF TMP_VIN = avtoz.VIN THEN
		DO:
			/**/
			TMP_VIN = avtoz.VIN.
			/**/
			DELETE avtoz.
			/**/
		END.
	ELSE
		/**/
		TMP_VIN = avtoz.VIN.
		/**/
END.
/**/	

{del-bar.i}
   
fname = "./avto_ru_"  + REPLACE(string(today), "/","_") + ".csv".

/*   
output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
/**/
put stream vvs unformatted '
 <?xml version="1.0"?>
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
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s66">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s67">
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s68">
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s70" ss:Parent="s62">
   <Font ss:Bold="1"/>
  </Style>
  <Style ss:ID="s71">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   </Style>
  <Style ss:ID="s78">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s79">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s80">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s81">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s82">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="@"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="journal">
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="90000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="58"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="83"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="92"/>
   <Column ss:StyleID="s66" ss:Width="82"/>
   <Column ss:StyleID="s65" ss:Width="190"/>
   <Column ss:StyleID="s63" ss:Width="100"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s67" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s68" ss:AutoFitWidth="0" ss:Width="80"/>'.

PUT STREAM VVS UNFORMATTED '<Row>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('№ п/п') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Модель,марка') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('VIN-код') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Год') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Цвет') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Номер шасси') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Номер двигателя') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Страна') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Город') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Авто в залоге с') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Авто в залоге по') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Залогодержатель') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('Номер кредита') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('CID') + '</Data></Cell>
			</Row>
			'.

FOR EACH avtoz NO-LOCK:
	PUT STREAM VVS UNFORMATTED '<Row>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.nn) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.mm) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.vin) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.year) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.vcolor) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.Nhassi) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.Ndvig) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.strana) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.city) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.Dzalog) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.DEndZal) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING('ОАО ''Плюсбанк''') + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.NDog) + '</Data></Cell>
			<Cell ss:StyleID="s70"><Data ss:Type="String">' + STRING(avtoz.CID) + '</Data></Cell>
			</Row>
			'.
END.

put stream vvs unformatted
'
</Table>\n
</Worksheet>\n
</Workbook>\n
'.
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").  
*/

output stream vvs to value (fname)
UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

put stream vvs unformatted
	"VIN"  delim
	"Марка ТС"  delim
	"Модель ТС" delim
	"В залоге до"
eol.

FOR EACH avtoz
	NO-LOCK:
	
	put stream vvs unformatted
			avtoz.vin delim
			avtoz.MARKA delim	
			avtoz.MODEL delim
			avtoz.DEndZal
			eol.
	END.
	
output stream vvs close.

MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}

