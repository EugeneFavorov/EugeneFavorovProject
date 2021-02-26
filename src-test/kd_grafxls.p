/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 2000-2012 ЗАО "Банковские информационные системы"
     Filename: schofpmt.p
   Parameters: 0 - Оба графика, 1 - ОД, 2 - %%
      Comment: График платежей с учетом погашений.
      Created: 04.03.2012 15:11 MIOA    
*/

{globals.i}
{tmprecid.def}
{client.i}

DEF VAR mI               AS INT64 NO-UNDO.
DEF VAR mJ               AS INT64 NO-UNDO.
DEF VAR mId              AS INT64 NO-UNDO.
DEF VAR mCrLineFlg       AS LOG   NO-UNDO.
DEF VAR mCrNextDog       AS LOG   NO-UNDO.
DEF VAR mAmnt            AS DEC   NO-UNDO.
DEF VAR mSumm-t          AS DEC   NO-UNDO EXTENT 3.
DEF VAR mTotals          AS DEC   NO-UNDO EXTENT 3.

DEF TEMP-TABLE tt-graf
   FIELD id           AS INT
   FIELD id2          AS INT
   FIELD end-date     AS DATE
   FIELD dsc-beg-date AS DATE
   FIELD sop-date     AS DATE
   FIELD amt-rub-prc  AS DEC
   FIELD amt-rub-dolg AS DEC
   FIELD summ-t       AS DEC
   FIELD idnt         AS INT
   FIELD op-date      AS DATE 
   FIELD op-amnt      AS DEC
   FIELD cont-code    LIKE loan.cont-code
   FIELD kr-acct      LIKE loan-acct.acct
   FIELD name         AS CHAR
   FIELD cid          AS CHAR
INDEX id IS PRIMARY idnt id id2
INDEX idnt idnt end-date 
.

/* {getdate.i} */
{setdest.i}
{justamin}

{empty tt-graf}
BLK-LOAN:
FOR EACH tmprecid,
   FIRST loan WHERE
   RECID(loan) EQ tmprecid.id
NO-LOCK:
   mSumm-t = 0.

/*   IF NUM-ENTRIES(loan.cont-code, " ") GT 1 THEN
      NEXT BLK-LOAN.*/
   
   mCrLineFlg = loan.cont-type EQ "Течение".

   FIND FIRST loan-acct WHERE
               loan-acct.contract  EQ loan.contract  
         AND   loan-acct.cont-code EQ loan.cont-code 
         AND   loan-acct.acct-type EQ "Кредит"
   NO-LOCK.
   
   BLK-T-OBL:
   FOR EACH term-obl WHERE 
            term-obl.contract  EQ loan.contract  
      AND   term-obl.cont-code BEGINS loan.cont-code 
      AND   term-obl.end-date  LE end-date
      AND  (term-obl.idnt      EQ 1
        OR  term-obl.idnt      EQ 3)
   NO-LOCK 
   BY term-obl.idnt 
   BY term-obl.end-date:

      IF     mCrLineFlg 
         AND term-obl.idnt      EQ 3 
         AND term-obl.cont-code EQ loan.cont-code THEN
         NEXT BLK-T-OBL.

      mI = mI + 1.
      IF term-obl.amt-rub GT 0 THEN
      DO:
        FIND FIRST tt-graf WHERE tt-graf.cont-code EQ loan.cont-code and tt-graf.end-date EQ term-obl.dsc-beg-date NO-ERROR.
        IF NOT AVAIL tt-graf THEN DO:
         CREATE tt-graf.
         ASSIGN
            tt-graf.id           = mI
            tt-graf.cont-code    = loan.cont-code
            tt-graf.kr-acct      = loan-acct.acct
            tt-graf.end-date     = term-obl.dsc-beg-date
            tt-graf.cid          = GetXattrValueEx(
        	(IF loan.cust-cat EQ "Ч" THEN "person" ELSE (IF loan.cust-cat EQ "Ю" THEN "cust-corp" ELSE "---")),
        	STRING(loan.cust-id),"CID","")
	    .
	RUN RE_CLIENT (loan.cust-cat,       /* тип клиента */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT tt-graf.name). /* наименование клиента */
	
       END.
       ASSIGN
        tt-graf.amt-rub-dolg      = term-obl.amt-rub WHEN term-obl.idnt EQ 3
        tt-graf.amt-rub-prc       = term-obl.amt-rub WHEN term-obl.idnt EQ 1
        .
      END.
   END.

END.

DEF VAR fname as char.
def new shared stream vvs.


fname = "./graf"  + "_" + userid('bisquit') + ".xml".

output stream vvs to value (fname)

UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

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
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="6"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s65">
   <Font ss:Size="9" ss:Bold="1"/>
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
   <NumberFormat ss:Format="0"/>
  </Style>

  <Style ss:ID="s67">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>

  <Style ss:ID="s68">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="8" 
    ss:Color="#000000" ss:Bold="1"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="journal">
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="150000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="12">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="140"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>\n'.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(12) + '" >\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">CID</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">наименование</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">счет</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">платеж од</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">платеж %%</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">дата гашения</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">номер кредитного договора</Data></Cell>\n
		</Row>\n
		'.

FOR EACH tt-graf
 BY tt-graf.cont-code BY tt-graf.end-date:
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(12) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + tt-graf.cid + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + tt-graf.name + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + DelFilFromAcct( tt-graf.kr-acct) + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="Number">' + string(tt-graf.amt-rub-dolg) + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="Number">' + string( tt-graf.amt-rub-prc) + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + string(tt-graf.end-date) + '</Data></Cell>\n
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + string(tt-graf.cont-code) + '</Data></Cell>\n
		</Row>\n
		'.
END.

 put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
output stream vvs close.
put unformatted "нажмите ESC для формирования XLS файла." SKIP.
{preview.i}
RUN sndbispc ("file=" + fname + ";class=bq").
