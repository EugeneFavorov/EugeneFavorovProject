{globals.i}
{intrface.get tmess}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: данные по VIP клиентам
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/
DEF VAR TOTAL 	AS DECIMAL INIT 0 NO-UNDO.
DEF VAR izVIP	AS CHAR NO-UNDO.
DEF VAR DEPOSIT	AS DECIMAL INIT 3000000 NO-UNDO. /* 3000000 */
DEF VAR CREDIT	AS DECIMAL INIT 10000000 NO-UNDO. /* 10000000 */
DEF VAR TURN	AS DECIMAL INIT 15000000 NO-UNDO. /* 15000000 */

/**/
def new shared stream puk.
def var zname as char  init "./vips.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
/**/
def var cnt_month as integer no-undo.
def var TMP_SUM as dec no-undo.
/**/

DEFINE TEMP-TABLE vitz
	FIELD cat		AS CHAR		/* тип клиента */
	FIELD did	 	AS INT64	/* идентификатор клиента */
	FIELD sname		AS CHAR		/* ФИО клиента */
	FIELD gtype 	AS DECIMAL	/* тип VIP'a: */
	FIELD tname		AS CHAR		/* наименование типа VIP'a */
	FIELD itsvip	AS CHAR		/* значение Доп. реквизита "видсотр" */
    FIELD sum		AS DECIMAL	/* сумма у клиента */
    .

{empty vitz}

cnt_month = MONTH(TODAY) - MONTH(DATE(1, 1, YEAR(TODAY))).


/* находим наименование клиента */
FUNCTION GetName RETURNS CHAR
	(
	cat AS CHARACTER,
	id AS INT64
	):
	/**/
	DEF VAR sNAME AS CHAR NO-UNDO.
	/**/
	IF cat = "Ч" THEN
		DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL PERSON THEN
				/* ФИО клиента */
				sNAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
	ELSE
		DO:
			FIND FIRST CUST-CORP 
			WHERE CUST-CORP.CUST-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL CUST-CORP THEN
				/* наименование организации */
				sNAME = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		END.
	/**/
	RETURN sNAME.
	/**/
END.

/*------------------------------------------------ Ф И З И К И ------------------------------------------------*/
{spinner.i "1..."}
/*<---------------- по вкладам ---------------->*/
FOR EACH LOAN
WHERE LOAN.CONTRACT = 'dps'
AND (LOAN.CLOSE-DATE = ? OR LOAN.CLOSE-DATE >= DATE(1, 1, YEAR(TODAY)))
AND LOAN.END-DATE = ?
AND LOAN.CUST-CAT = "Ч"
NO-LOCK,
		EACH LOAN-ACCT OF LOAN
		WHERE ( LOAN-ACCT.ACCT-TYPE = "loan-dps-p" OR LOAN-ACCT.ACCT-TYPE = "loan-dps-t" )
		NO-LOCK,
				LAST ACCT-POS
				WHERE ACCT-POS.ACCT = LOAN-ACCT.ACCT
				AND ACCT-POS.CURRENCY = LOAN-ACCT.CURRENCY
BREAK BY LOAN.CUST-ID:
	/**/
	IF  FIRST-OF(LOAN.CUST-ID) THEN
		DO:
			TOTAL = 0.
		END.
	/**/
	/* если рубли */
	IF LOAN-ACCT.CURRENCY = " " THEN
		TOTAL = TOTAL + ABS(ACCT-POS.BALANCE).
	/* если валюта */
	ELSE
		DO:
			/* найдем учетный курс */
			FIND FIRST INSTR-RATE
				WHERE INSTR-RATE.INSTR-CAT = "currency"
				AND INSTR-RATE.RATE-TYPE = "Учетный"
				AND INSTR-RATE.INSTR-CODE = LOAN-ACCT.CURRENCY
				AND INSTR-RATE.SINCE = ACCT-POS.SINCE
			NO-LOCK NO-ERROR.
			/**/	
			IF AVAIL INSTR-RATE THEN
				TOTAL = TOTAL + ABS(ACCT-POS.BALANCE) * INSTR-RATE.RATE-INSTR.
			ELSE
				TOTAL = TOTAL + ABS(ACCT-POS.BALANCE).
		END.
		
	/**/ 
	IF LAST-OF(LOAN.CUST-ID) AND TOTAL > DEPOSIT THEN
		DO:	
			/**/
			CREATE vitz.
			/**/
			ASSIGN 
				vitz.cat = "Ч"
				vitz.did = LOAN.CUST-ID
				vitz.gtype = 1
				vitz.tname = "Вклады физ.лиц"
				vitz.sum = TOTAL
				vitz.sname = GetName("Ч",LOAN.CUST-ID)
				vitz.itsvip = GetXAttrValueEx ("person", STRING(LOAN.CUST-ID), "видсотр", "")
			.
			/**/
		END.				
END.

/*<---------------- по счетам ---------------->*/
{spinner.i "2..."}
FOR EACH ACCT
	WHERE ( ACCT.CONTRACT = "Расчет" OR ACCT.CONTRACT = "Текущ" )
	AND (ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1, 1, YEAR(TODAY)))
	AND ACCT.CUST-CAT = "Ч"
	AND NOT CAN-FIND
		(
		FIRST vitz
		WHERE vitz.cat = "Ч"
		AND vitz.did = ACCT.CUST-ID
		)
	NO-LOCK,
		LAST ACCT-POS
		WHERE ACCT-POS.ACCT = ACCT.ACCT
		AND ACCT-POS.CURRENCY = ACCT.CURRENCY
BREAK BY ACCT.CUST-ID:
		
	/**/
	IF  FIRST-OF(ACCT.CUST-ID) THEN
		DO:
			TOTAL = 0.
		END.
	/**/
	/* если рубли */
	IF ACCT.CURRENCY = " " THEN
		TOTAL = TOTAL + ABS(ACCT-POS.BALANCE).
	/* если валюта */
	ELSE
		DO:
			/* найдем учетный курс */
			FIND FIRST INSTR-RATE
				WHERE INSTR-RATE.INSTR-CAT = "currency"
				AND INSTR-RATE.RATE-TYPE = "Учетный"
				AND INSTR-RATE.INSTR-CODE = ACCT.CURRENCY
				AND INSTR-RATE.SINCE = ACCT-POS.SINCE
			NO-LOCK NO-ERROR.
			/**/	
			IF AVAIL INSTR-RATE THEN
				TOTAL = TOTAL + ROUND(ABS(ACCT-POS.BALANCE) * INSTR-RATE.RATE-INSTR, 2).
			ELSE
				TOTAL = TOTAL + ABS(ACCT-POS.BALANCE).
		END.
		
	/**/ 
	IF LAST-OF(ACCT.CUST-ID) AND TOTAL > DEPOSIT THEN
		DO:
			/**/
			CREATE vitz.
			/**/
			ASSIGN 
				vitz.cat = "Ч"
				vitz.did = ACCT.CUST-ID
				vitz.gtype = 2
				vitz.tname = "Счета физ.лиц"
				vitz.sum = TOTAL
				vitz.sname = GetName(ACCT.CUST-CAT,ACCT.CUST-ID)
				vitz.itsvip = GetXAttrValueEx ("person", STRING(ACCT.CUST-ID), "видсотр", "")				
			.				
		END.
	
END.

/*------------------------------------------- Ф И З И К И + Ю Р И К И --------------------------------------------*/
{spinner.i "3..."}
/*<---------------- по кредитам ---------------->*/
FOR EACH LOAN
WHERE LOAN.CONTRACT = 'Кредит'
AND (LOAN.CLOSE-DATE = ? OR LOAN.CLOSE-DATE >= DATE(1, 1, YEAR(TODAY)))
AND (LOAN.CUST-CAT = "Ч" OR LOAN.CUST-CAT = "Ю")
NO-LOCK,
		FIRST TERM-OBL OF LOAN
		WHERE TERM-OBL.IDNT = 2
BREAK BY LOAN.CUST-ID:

	/**/
	IF  FIRST-OF(LOAN.CUST-ID) THEN
		DO:
			TOTAL = 0.
		END.
	/**/

	TOTAL = TOTAL + TERM-OBL.AMT-RUB.
		
	/**/ 
	IF LAST-OF(LOAN.CUST-ID) AND TOTAL > CREDIT THEN
		DO:
			/**/
			CREATE vitz.
			/**/
			ASSIGN 
				vitz.cat = LOAN.CUST-CAT
				vitz.did = LOAN.CUST-ID
				vitz.gtype = 3
				vitz.tname = "Кредиты"
				vitz.sum = TOTAL
				vitz.sname = GetName(LOAN.CUST-CAT,LOAN.CUST-ID)
				vitz.itsvip = GetXAttrValueEx ("person", STRING(LOAN.CUST-ID), "видсотр", "")				
			.
			/**/
		END.
END.

/*--------------------------------------- по среднемесячным оборотам -----------------------------------------*/
{spinner.i "4..."}
FOR EACH ACCT
	WHERE (ACCT.CONTRACT = "Расчет" OR ACCT.CONTRACT = "Текущ" )
	AND (ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1, 1, YEAR(TODAY)))
	NO-LOCK:
	/**/
		/**/
		TMP_SUM = 0.
		
		/* обороты по дебету */
		FOR EACH OP-ENTRY
		WHERE OP-ENTRY.ACCT-DB = ACCT.ACCT
		AND OP-ENTRY.OP-DATE >= DATE(1, 1, YEAR(TODAY))
		AND OP-ENTRY.OP-DATE <= DATE(MONTH(TODAY), 1, YEAR(TODAY))
		NO-LOCK:
			/**/
			TMP_SUM = TMP_SUM + OP-ENTRY.AMT-RUB.
			/**/
		END.
		
		/* обороты по кредиту */
		FOR EACH OP-ENTRY
		WHERE OP-ENTRY.ACCT-CR = ACCT.ACCT
		AND OP-ENTRY.OP-DATE >= DATE(1, 1, YEAR(TODAY))
		AND OP-ENTRY.OP-DATE <= DATE(MONTH(TODAY), 1, YEAR(TODAY))
		NO-LOCK:
			/**/
			TMP_SUM = TMP_SUM + OP-ENTRY.AMT-RUB.
			/**/
		END.	
		/**/
		/**/
		IF TMP_SUM / cnt_month > TURN THEN
			DO:
				/**/
				IF NOT CAN-FIND( FIRST vitz WHERE vitz.cat = ACCT.CUST-CAT AND  vitz.did = ACCT.CUST-ID) THEN
					DO:
						/**/
						CREATE vitz.
						/**/
						ASSIGN
							vitz.cat = ACCT.CUST-CAT
							vitz.did = ACCT.CUST-ID
							vitz.gtype = 4
							vitz.tname = "Обороты"
							vitz.sum = TMP_SUM / cnt_month
							vitz.sname = GetName(ACCT.CUST-CAT,ACCT.CUST-ID)
							vitz.itsvip = GetXAttrValueEx ((IF ACCT.CUST-CAT = "Ч" THEN "person" ELSE "cust-corp"), STRING(ACCT.CUST-ID), "видсотр", "")	
						.
						/**/
					END.
				ELSE
					DO:
						/**/
						FIND FIRST vitz
						WHERE vitz.cat = ACCT.CUST-CAT
						AND  vitz.did = ACCT.CUST-ID
						NO-LOCK NO-ERROR.
							/**/
							IF AVAIL vitz THEN 
								DO:
									/**/
									IF INDEX(vitz.tname, "Обороты") = 0 THEN
										vitz.tname = vitz.tname + "," + "Обороты".
									/**/
								END.
							/**/
						/**/
					END.
					/**/
			END.
	/**/
END.

/*--------------------------------------- по среднемесячным остаткам -----------------------------------------*/
{spinner.i "4..."}
FOR EACH ACCT
	WHERE (ACCT.CONTRACT = "Расчет" OR ACCT.CONTRACT = "Текущ" )
	AND (ACCT.CLOSE-DATE = ? OR ACCT.CLOSE-DATE >= DATE(1, 1, YEAR(TODAY)))
	AND CAN-FIND
		(
		FIRST ACCT-POS
		WHERE ACCT-POS.ACCT = ACCT.ACCT
		AND ACCT-POS.CURRENCY = ACCT.CURRENCY
		AND ACCT-POS.SINCE >= DATE(1, 1, YEAR(TODAY))
		AND ABS(ACCT-POS.BALANCE) > CREDIT
		)	
	NO-LOCK:
		/**/
		IF NOT CAN-FIND( FIRST vitz WHERE vitz.cat = ACCT.CUST-CAT AND  vitz.did = ACCT.CUST-ID) THEN
			DO:
				/**/
				CREATE vitz.
				/**/
				ASSIGN
					vitz.cat = ACCT.CUST-CAT
					vitz.did = ACCT.CUST-ID
					vitz.gtype = 4
					vitz.tname = "Остатки"
					vitz.sname = GetName(ACCT.CUST-CAT,ACCT.CUST-ID)
					vitz.itsvip = GetXAttrValueEx ((IF ACCT.CUST-CAT = "Ч" THEN "person" ELSE "cust-corp"), STRING(ACCT.CUST-ID), "видсотр", "")	
				.
				/**/
				FIND FIRST ACCT-POS
				WHERE ACCT-POS.ACCT = ACCT.ACCT
				AND ACCT-POS.CURRENCY = ACCT.CURRENCY
				AND ACCT-POS.SINCE >= DATE(1, 1, YEAR(TODAY))
				AND ABS(ACCT-POS.BALANCE) > CREDIT
				NO-LOCK NO-ERROR.
					/**/
					IF AVAIL ACCT-POS THEN
						DO:
							/**/
							vitz.sum = ABS(ACCT-POS.BALANCE).
							/**/
						END.
					/**/
				/**/
			END.
		ELSE
			DO:
				/**/
				FIND FIRST vitz
				WHERE vitz.cat = ACCT.CUST-CAT
				AND  vitz.did = ACCT.CUST-ID
				NO-LOCK NO-ERROR.
					/**/
					IF AVAIL vitz THEN 
						DO:
							/**/
							IF INDEX(vitz.tname, "Остатки") = 0 THEN
								vitz.tname = vitz.tname + "," + "Остатки".
							/**/
						END.
					/**/
				/**/
			END.
		/**/
END.


/*----------------------------------------- V I P  -  Ф И З И К И --------------------------------------------*/
{spinner.i "6..."}
FOR EACH SIGNS
	WHERE SIGNS.FILE-NAME = "person"
	AND SIGNS.CODE = "видсотр"
	AND SIGNS.CODE-VALUE = "VIP"
	NO-LOCK,
		FIRST PERSON
		WHERE PERSON.PERSON-ID = INT64(SIGNS.SURROGATE)
		/* не попавшие в список */
		AND NOT CAN-FIND
			(
			FIRST vitz
			WHERE vitz.cat = "Ч"
			AND vitz.did = PERSON.PERSON-ID
			):
				/**/
				CREATE vitz.
				/**/
				ASSIGN 
					vitz.cat = "Ч"
					vitz.did = PERSON.PERSON-ID
					vitz.gtype = 6
					vitz.tname = "ДР = VIP"
					vitz.sum = 0
					vitz.itsvip = "VIP"
					vitz.sname = GetName("Ч",PERSON.PERSON-ID)
				.					
END.


/*------------------------------------------------ Ю Р И К И ------------------------------------------------*/

/**/

/*
/* вывод */	
run instview.p(TEMP-TABLE vitz:HANDLE). 
*/



zname = "./vips_" + userid('bisquit') + ".xml".	

IF LASTKEY EQ 27 THEN
	RETURN.
	
output stream puk to value (zname)
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
  <Style ss:ID="s64">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="VIP-Клиенты">
   <Table x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Row ss:AutoFitHeight="0">
    <Cell><Data ss:Type="String">Тип клиента</Data></Cell>
    <Cell><Data ss:Type="String">Идентификатор</Data></Cell>
    <Cell><Data ss:Type="String">Наименование клиента</Data></Cell>
    <Cell><Data ss:Type="String">Тип VIPa</Data></Cell>
    <Cell><Data ss:Type="String">Значение ДР "видсотр"</Data></Cell>
    <Cell><Data ss:Type="String">Сумма(руб.)</Data></Cell>
   </Row>'.
   
   /* 1. выполнили норматив + VIP */
   PUT STREAM puk UNFORMATTED
   '<Row ss:AutoFitHeight="0">
    <Cell ss:MergeAcross="5" ss:StyleID="s64"><Data ss:Type="String">Клиенты выполнили норматив и ДР = VIP</Data></Cell>
   </Row>\n'.
   /**/
 	FOR EACH vitz
		WHERE vitz.gtype <> 6
		AND vitz.itsvip = 'VIP'
	NO-LOCK:
		/**/
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.cat) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.did) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.sname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.tname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.itsvip) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="Number">' + STRING(vitz.sum) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
		/**/
	END.

   /* 2. выполнили норматив + <> VIP */
   PUT STREAM puk UNFORMATTED
   '<Row ss:AutoFitHeight="0">
    <Cell ss:MergeAcross="5" ss:StyleID="s64"><Data ss:Type="String">Клиенты выполнили норматив и ДР <> VIP</Data></Cell>
   </Row>\n'.
   /**/
 	FOR EACH vitz
		WHERE vitz.gtype <> 6
		AND vitz.itsvip <> 'VIP'
	NO-LOCK:
		/**/
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.cat) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.did) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.sname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.tname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.itsvip) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="Number">' + STRING(vitz.sum) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
		/**/
	END.

   /* 3. невыполнили норматив + = VIP */
   PUT STREAM puk UNFORMATTED
   '<Row ss:AutoFitHeight="0">
    <Cell ss:MergeAcross="5" ss:StyleID="s64"><Data ss:Type="String">Клиенты не выполнили норматив и ДР = VIP</Data></Cell>
   </Row>\n'.
   /**/
 	FOR EACH vitz
		WHERE vitz.gtype = 6
	NO-LOCK:
		/**/
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.cat) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.did) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.sname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.tname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(vitz.itsvip) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="Number">' + STRING(vitz.sum) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
		/**/
	END.	
	
put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + zname + ";class=bq").
/* конец вывода */	
