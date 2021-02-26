{globals.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get tmess}

DEF NEW SHARED STREAM vvs.
DEF VAR fname AS CHAR NO-UNDO.

DEF VAR vKod  AS CHAR NO-UNDO.
DEF VAR vOfis AS CHAR NO-UNDO.
DEF VAR vSotr AS CHAR NO-UNDO.
DEF VAR mTmp  AS CHAR NO-UNDO.
DEF VAR mDate AS DATE NO-UNDO.

DEF TEMP-TABLE sotr 
	FIELD ofis AS CHAR
	FIELD kod  AS CHAR
	FIELD one  AS INT64
	FIELD sotr AS CHAR.
	
mDate = DATE(12/12/2013).	
	
FOR EACH loan WHERE
		 loan.contract EQ 'dps' AND
		 loan.open-date LT mDate AND
		 loan.end-date NE ? AND
		 loan.loan-status EQ "Ф"
	NO-LOCK:
	
	IF GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "Акция201403","") NE "" THEN DO:
		mTmp = GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "Акция201403","").
		vKod = TRIM(ENTRY(2,mTmp)).
		FIND _User WHERE 
		     _User._Userid = vKod
		NO-LOCK NO-ERROR.
			vOfis = STRING(GetXAttrValue("_user",vKod,"Отделение")).
			vSotr = _User._User-Name.
		
		CREATE sotr.
		ASSIGN
			sotr.kod  = vKod
			sotr.ofis = vOfis
			sotr.sotr = vSotr
			sotr.one  = 1.
	END.
	
END.

fname = "./ak201403_sotrud_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".

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
   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="s62">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Bold="1"/>\n
   <Interior ss:Color="#F79646" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="s64">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="s65">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
  </Style>\n
  <Style ss:ID="s67">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Interior ss:Color="#9BBB59" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="s68">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
 </Styles>\n
<Worksheet ss:Name="Отчет">\n
<Table ss:ExpandedColumnCount="2" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="401.5"/>\n'.

FOR EACH sotr NO-LOCK BREAK BY sotr.ofis BY sotr.kod:

	ACCUMULATE sotr.one (TOTAL BY sotr.kod BY sotr.ofis).
	
	IF FIRST-OF(sotr.ofis) THEN DO:
		FIND branch WHERE branch.branch-id = sotr.ofis NO-LOCK NO-ERROR.
		PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
	<Cell ss:StyleID="s62"><Data ss:Type="String">' + TRIM(branch.name) + '</Data></Cell>\n
</Row>\n'.
	END.
	
	IF LAST-OF(sotr.kod) THEN DO:
		PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
    <Cell ss:StyleID="s68"><Data ss:Type="String">' + TRIM(sotr.sotr) + '</Data></Cell>\n
    <Cell ss:StyleID="s64"><Data ss:Type="Number">' + STRING(ACCUM TOTAL BY sotr.kod sotr.one)  + '</Data></Cell>\n
   </Row>\n'.
	END.

	IF LAST-OF(sotr.ofis) THEN DO:
		PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
    <Cell ss:StyleID="s65"><Data ss:Type="String">Всего по отделению:</Data></Cell>\n
    <Cell ss:StyleID="s64"><Data ss:Type="Number">' + STRING(ACCUM TOTAL BY sotr.ofis sotr.one) + '</Data></Cell>\n
   </Row>\n'.
	END.

END.

PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">\n
    <Cell ss:StyleID="s65"><Data ss:Type="String">Всего:</Data></Cell>\n
    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + STRING(ACCUM TOTAL sotr.one) + '</Data></Cell>\n
   </Row>\n
   </Table>\n
 </Worksheet>\n
</Workbook>\n'.

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").