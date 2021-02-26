/*
kam

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{pp-corr.p}
def var tmpdate as date no-undo.
def var tmpdate1 as date no-undo.

tmpdate1 = TODAY.
/* tmpdate1 = date("31.12.2016"). */
tmpdate = date(IF month(tmpdate1) EQ 1 THEN 12 ELSE (month(tmpdate1) - 1),1,IF month(tmpdate1) EQ 1 THEN year(tmpdate1) - 1 ELSE year(tmpdate1)).
beg-date = tmpdate.
end-date = date_correct(IF month(tmpdate1) EQ 1 THEN 12 ELSE (month(tmpdate1) - 1),0,31,IF month(tmpdate1) EQ 1 THEN year(tmpdate1) - 1 ELSE year(tmpdate1)).

{getdates.i
&NoInit    = "YES"
}

DEFINE TEMP-TABLE otch1
        FIELD cont_code    AS CHAR                 /* номер КД */
		FIELD periodod     AS CHAR                  /* Периодичность погашения основного долга */
		FIELD summodpl     AS DECIMAL              /* сумма од плановая */
		FIELD summodup     AS DECIMAL              /* сумма од уплаченная */
		FIELD periodpr     AS CHAR                  /* Периодичность погашения % */
		FIELD summprpl     AS DECIMAL              /* сумма % плановая */
		FIELD summprup     AS DECIMAL              /* сумма % уплаченная */		
		FIELD summcomm     AS DECIMAL              /* сумма комиссий */		 
        INDEX cont_code cont_code       
    .


{empty otch1}

def var cont_code as char no-undo.
def var periodod as char no-undo.
def var summodpl as decimal no-undo.
def var summodup as decimal no-undo.
def var periodpr as char no-undo.
def var summprpl as decimal no-undo.
def var summprup as decimal no-undo.
def var summcomm as decimal no-undo.

DEF VAR fname AS CHAR NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 

/* По отмеченным клиентам */
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    FIND LAST loan-cond WHERE loan-cond.contract = loan.contract
		AND loan-cond.cont-code = loan.cont-code
		AND loan-cond.since <= end-date NO-LOCK NO-ERROR.
		IF AVAIL loan-cond THEN DO:
			periodod = loan-cond.cred-period.
			periodpr = loan-cond.int-period.
		END.

    summodpl = 0.
    /* ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ */
    FOR EACH term-obl WHERE term-obl.amt-rub <> 0
    	AND term-obl.contract = loan.contract
    	AND term-obl.cont-code = loan.cont-code
    	AND term-obl.idnt = 3
		AND term-obl.dsc-beg-date <> ?
		AND term-obl.dsc-beg-date <= end-date
    	AND term-obl.dsc-beg-date >= beg-date  NO-LOCK
    	:
    	    summodpl = summodpl + ABS(term-obl.amt-rub).
    END.
    summprpl = 0.
    /* ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ */
    FOR EACH term-obl WHERE term-obl.amt-rub <> 0
    	AND term-obl.contract = loan.contract
    	AND term-obl.cont-code = loan.cont-code
    	AND term-obl.idnt = 1
		AND term-obl.dsc-beg-date <> ?
		AND term-obl.dsc-beg-date <= end-date
    	AND term-obl.dsc-beg-date >= beg-date NO-LOCK
    	:
			summprpl = summprpl + ABS(term-obl.amt-rub). 
    END.
    
    summodup = 0.
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 2
        AND loan-int.id-d = 1
        NO-LOCK:    
        summodup = summodup + abs(loan-int.amt-rub).
    END.
    
    summprup = 0.
    
    /* 100 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 29
        AND loan-int.id-d = 30
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END.
    /* 237 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 248
        AND loan-int.id-d = 8
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END.
    /* 309 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 48
        AND loan-int.id-d = 30
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END. 
    /* 480 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 6
        AND loan-int.id-d = 352
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END.
    /* 46 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 35
        AND loan-int.id-d = 5
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END. 
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 6
        AND loan-int.id-d = 352
        NO-LOCK:    
        summprup = summprup + abs(loan-int.amt-rub).
    END.     
    
    summcomm = 0.
    /* 127 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 9
        AND loan-int.id-d = 26
        NO-LOCK:    
        summcomm = summcomm + abs(loan-int.amt-rub).
    END.    
    /* 129 */
    FOR EACH loan-int WHERE loan-int.contract = loan.contract
        AND loan-int.cont-code = loan.cont-code
        AND loan-int.mdate >= beg-date
        AND loan-int.mdate <= end-date
        AND loan-int.id-k = 12
        AND loan-int.id-d = 26
        NO-LOCK:    
        summcomm = summcomm + abs(loan-int.amt-rub).
    END.    
	
	CREATE otch1.
	ASSIGN
		otch1.cont_code 	= loan.cont-code
		otch1.periodod 	= periodod
		otch1.summodpl 	= summodpl
		otch1.summodup 	= summodup
		otch1.periodpr 	= periodpr
		otch1.summprpl 	= summprpl
		otch1.summprup 	= summprup        
        otch1.summcomm 	= summcomm        
		.
    RELEASE otch1.
END.

/* выводим в ексель */
    fname = "./otch1"  + "_" + USERID('bisquit') + ".xml".
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
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="120.00"/>
   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Периодичность погашения од</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма од плановая</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма од уплаченная</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Периодичность погашения %</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма % плановая</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма % уплаченная</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">сумма комиссий</Data></Cell>
   </Row>
'.

		
FOR EACH otch1 NO-LOCK BY otch1.cont_code:
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED DelFilFromLoan(otch1.cont_code) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otch1.periodod) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED TRIM(STRING(otch1.summodpl,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED TRIM(STRING(otch1.summodup,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(otch1.periodpr) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED TRIM(STRING(otch1.summprpl,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED TRIM(STRING(otch1.summprup,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.	
	PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED TRIM(STRING(otch1.summcomm,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.	
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









