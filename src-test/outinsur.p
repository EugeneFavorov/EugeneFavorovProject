/*
	Проверочный отчет
Данный отчет должен выгружаться по кредитам с доп. реквизитом Priznak - <союз> и содержать информацию:

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan1 FOR loan.

DEFINE TEMP-TABLE otch
   FIELD cont_code  AS CHAR    /* номер КД */
   FIELD fio        AS CHAR    /* ФИО заемщика */
	 FIELD open_date  AS DATE    /* дата открытия договора */
   FIELD strahbik   AS CHAR    /*  */
	 FIELD strah      AS CHAR    /*  */
   FIELD ind        AS CHAR    /* индекс заемщика */
   FIELD adr_strah  AS CHAR    /* адрес страховой организации */
   FIELD dogstrah   AS CHAR    /* номер договора страхования */
   INDEX cont_code cont_code   
.

{empty otch}

def var mDogStrah as char no-undo.
def var nameCl    as char no-undo.
def var telCl     as char no-undo.
def var mStrah    as char no-undo.
def var addrStrah as char no-undo.
def var adr_strah as char no-undo.
DEF VAR indStrah  AS CHAR NO-UNDO.
def var open_date as date no-undo.
DEF VAR mK        AS INT64 INIT 0 NO-UNDO. /* счетчик договоров */
DEF VAR mI        AS INT64        NO-UNDO. /* перебор индекса */
DEF VAR mDecimal  AS DECIMAL      NO-UNDO.
/* def var plan_date as date no-undo.
def var annuit as decimal no-undo.
def var acct40817 as char no-undo.
def var ost40817 as decimal no-undo.
def var ost455 as decimal no-undo.
def var ost47427 as decimal no-undo.
def var ost90909 as decimal no-undo.
def var acct47401 as char no-undo. */

DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr  AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 
 {getdate.i}

/* По отмеченным клиентам */
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    RUN GetName(INPUT loan.cust-cat, INPUT loan.cust-id, OUTPUT nameCl).

   FOR EACH loan1 WHERE
	 	loan1.contract EQ "СТРАХ"
	 	AND loan1.parent-cont-code EQ loan.cont-code
	 	AND (loan1.end-date = ? OR loan1.end-date >= end-date)
	 	NO-LOCK:
	 	RUN GetName(INPUT loan1.cust-cat, INPUT loan1.cust-id, OUTPUT mStrah).
	 
   mDogStrah = loan1.doc-ref.

   RUN RetAdr.p(INPUT INT64(loan1.cust-id), INPUT loan1.cust-cat, 'АдрЮр', INPUT loan1.open-date, OUTPUT adr_strah).
   adr_strah = TRIM(TRIM(adr_strah,',')).
   IF adr_strah = '' THEN RUN RetAdr.p(INPUT INT64(loan1.cust-id), INPUT loan1.cust-cat, 'АдрФакт', INPUT loan1.open-date, OUTPUT adr_strah).
   adr_strah = TRIM(TRIM(adr_strah,',')).
   IF adr_strah = '' THEN DO:
      RUN RetAdr.p(INPUT INT64(loan1.cust-id), INPUT loan1.cust-cat, 'АдрЮр', TODAY, OUTPUT adr_strah).
      adr_strah = TRIM(TRIM(adr_strah,',')).
      IF adr_strah = '' THEN RUN RetAdr.p(INPUT INT64(loan1.cust-id), INPUT loan1.cust-cat, 'АдрФакт',TODAY, OUTPUT adr_strah).
   END.
 
   IF NUM-ENTRIES(adr_strah, ",") > 1 THEN 
   DO:
      mDecimal = DECIMAL(ENTRY(1, adr_strah)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         indStrah = "".
      ELSE 
         indStrah = ENTRY(1, adr_strah).
      addrStrah = TRIM(LEFT-TRIM(adr_strah, "," + indStrah)).
   END.

   FIND FIRST cust-corp 
        WHERE cust-corp.cust-id EQ loan1.cust-id
   NO-LOCK NO-ERROR.

    CREATE otch.
	 	ASSIGN
	 	   otch.cont_code = loan.cont-code
	 	   otch.fio 		  = nameCl
       otch.strahbik  = cust-corp.bank-code
	 	   otch.strah		  = mStrah
	 	   otch.open_date = loan.open-date
       otch.adr_strah = addrStrah
       otch.ind       = indStrah  
       otch.dogstrah  = mDogStrah
	 	.
	END.	
END.

/* выводим в ексель */
   fname = "./otch"  + "_" + USERID('bisquit') + ".xml".
	output stream vvs to value (fname)
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="50.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="40"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="15"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Row ss:Index="1" ss:AutoFitHeight="0" ss:Height="17.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">номер</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">f1</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">f2</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ФИО</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Реквизиты</Data></Cell>
	  <Cell ss:StyleID="s66"><Data ss:Type="String">БИК</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Strah</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Индекс</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I1</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I2</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I3</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I4</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I5</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">I6</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Adr_Strah</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">индекс_адрес</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Имя_Адрес</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">dogstrah</Data></Cell>.    
   </Row>
'.

FOR EACH otch NO-LOCK 
   BY otch.strah 
   BY otch.cont_code :

   mK = mK + 1.
   PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED STRING(mK) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Index="4"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.fio + '</Data></Cell>\n'.
	    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED DelFilFromLoan(otch.cont_code) + ' от ' + STRING(otch.open_date,"99.99.9999") + ' г.' + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED TRIM(otch.strahbik) + '</Data></Cell>\n'.
	    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED TRIM(otch.strah) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.ind + '</Data></Cell>\n'.
      IF otch.ind NE "" THEN 
      DO mI = 1 TO LENGTH(otch.ind):
         PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
         PUT STREAM vvs UNFORMATTED SUBSTRING(otch.ind, mI, 1) + '</Data></Cell>\n'.
      END.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Index="15"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.adr_strah + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.ind  + ', ' + otch.adr_strah '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Formula="=RC[-10]&amp;CHAR(10)&amp;RC[-1]"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:Index="18"><Data ss:Type="String">'.
      PUT STREAM vvs UNFORMATTED otch.dogstrah + '</Data></Cell>\n'.
   PUT STREAM vvs UNFORMATTED '</Row>'.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}

PROCEDURE GetAddr:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER mSignsV AS CHARACTER.
 def var mSignVL as char no-undo.
IF loan.cust-cat = 'Ч' THEN DO:
	FIND FIRST person WHERE person.person-id = loan.cust-id NO-LOCK NO-ERROR.
	mSignsV =  TRIM(STRING(person.address[1])) + TRIM(STRING(person.address[2])).
END.
END PROCEDURE.

PROCEDURE GetTel:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER mSignsV AS CHARACTER.
 def var mSignVL as char no-undo.
 def var mSignsF as char no-undo.
	mSignVL = "".
	mSignsV = "".
	mSignsF = "".
	IF loan.cust-cat = 'Ч' THEN DO:
		FIND FIRST person WHERE person.person-id = loan.cust-id NO-LOCK NO-ERROR.
		mSignVL = GetXAttrValueEx("person",
                  STRING(person.person-id),
                  "cell-phone",
                  "").
		IF {assigned mSignVL} THEN
		mSignsV = mSignVL.
		ELSE IF  {assigned person.phone[1]}
		AND NUM-ENTRIES(person.phone[1]) GT 1
		AND {assigned ENTRY(2,person.phone[1])} THEN
		mSignsV = ENTRY(2, person.phone[1]).
		ELSE IF {assigned person.phone[1]} THEN
		mSignsV = ENTRY(1, STRING(person.phone[1])).
		IF {assigned person.phone[2]} THEN
		mSignsV = mSignsV + ' ' + ENTRY(1, STRING(person.phone[2])).
		mSignsF = GetXAttrValueEx("person",
                  STRING(person.person-id),
               "fax",
                 "").
		mSignsV = mSignsV + ' ' + mSignsF.  
		mSignsF = GetXAttrValueEx("person",
                  STRING(person.person-id),
               "РаботаТел",
                 "").
		mSignsV = mSignsV + ' ' + mSignsF.  
	END.
	ELSE IF loan.cust-cat = 'Ю' THEN DO:
		mSignsV = GetXAttrValueEx("cust-corp", 
			STRING(cust-corp.cust-id), 
            "tel", 
            "").
		mSignsF = GetXAttrValueEx("cust-corp", 
            STRING(cust-corp.cust-id), 
            "fax", 
            "").
		mSignsV = mSignsV + ' ' + mSignsF. 
	END.
END PROCEDURE.

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
END PROCEDURE.









