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
DEF VAR TOTAL AS DECIMAL INIT 0 NO-UNDO.
DEF VAR TMP_NAME AS CHAR NO-UNDO.
DEF BUFFER ploan FOR LOAN.
/**/
DEF VAR DATE1 AS DATE NO-UNDO.
DEF VAR DATE2 AS DATE NO-UNDO.
DEF VAR COEF_REZ AS DECIMAL NO-UNDO.

def new shared stream puk.
def var fname 			as char  init "./branch_rates.csv"  no-undo.
def var delim 			as char init ";" format "x(1)" no-undo.
def var eol 			as char format "x(2)" no-undo.

DEFINE TEMP-TABLE avt
	FIELD indx		 AS INTEGER	/* нумерация */
   FIELD tnum	 	 AS CHAR		/* № КД */
	FIELD tcid		 AS INT64	/* ID клиента */
	FIELD tfname    AS CHAR		/* ФИО клиента */
	FIELD tschet    AS CHAR    /* счёт 40817 */
	FIELD tbal      AS DECIMAL /* остаток на счёте 40817 */
	FIELD op4       AS DECIMAL /* сумма 4 операции (выдача) (kam) */
	FIELD tdate		 AS DATE		/* дата открытия КД */
	FIELD tkk		 AS DECIMAL	/* категория качества */
   FIELD tportf    AS CHAR 	/* портфель */
	FIELD fpr91312	 AS CHAR		/* проводка 91312 кредит за то же число */
	FIELD mdate		 AS DATE		/* дата, > end-date and <= today */
	FIELD mFL		 AS CHAR		/* Есть ли проводка в последующие дни */
   FIELD op4Date   AS DATE
   FIELD sumforvyd AS DECIMAL
   FIELD ppdate    as date
   FIELD ost40817  AS DECIMAL
.

/*pda*/
DEF VAR vSumForVyd LIKE avt.sumforvyd INIT 0.00 NO-UNDO. /*Итог */ 
DEF VAR vOp4       LIKE avt.Op4       INIT 0.00 NO-UNDO. /*Итог сумма 4 операции (выдача)*/
DEF VAR vTbal      LIKE avt.Tbal      INIT 0.00 NO-UNDO. /*Итог остаток на счёте 40817*/
DEF VAR vOst40817  LIKE avt.ost40817  INIT 0.00 NO-UNDO. /*Итог */

{empty avt}
{sh-defs.i}
{getdates.i}

FOR EACH LOAN
	WHERE LOAN.CONTRACT = "Кредит"
        AND LOAN.OPEN-DATE >= beg-date
        and LOAN.OPEN-DATE <= end-date
	AND LOAN.CONT-TYPE = "АвтоПлюс"
	NO-LOCK:
		/**/
		CREATE avt.
		/**/
		ASSIGN
			avt.tnum = LOAN.DOC-REF
			avt.tcid = LOAN.CUST-ID
			avt.tdate = LOAN.OPEN-DATE
		.
		
		find first term-obl where term-obl.cont-code = loan.cont-code
		and term-obl.contract = loan.contract
		and term-obl.idnt = 2
		no-lock no-error.
		if avail term-obl then avt.sumforvyd = term-obl.amt-rub.
		  else avt.sumforvyd = 0.
		
		find first loan-int where loan-int.cont-code = loan.cont-code
		  and loan-int.contract = loan.contract
		  and loan-int.id-d = 0
		  and loan-int.id-k = 3
		  and loan-int.amt-rub <> 0 no-lock no-error.
        if avail loan-int then do:
          avt.op4 = abs(loan-int.amt-rub).
          avt.op4Date = loan-int.mdate.
        end.
		else do:
		  avt.op4 = 0.
		  avt.op4Date = ?.

		end.  
		  
		/**/
		DATE1 = ?.
		DATE2 = ?.
			/**/
			RUN GetName(INPUT LOAN.CUST-CAT,INPUT LOAN.CUST-ID, OUTPUT TMP_NAME).
			/**/
			avt.tfname = TMP_NAME.
            avt.ppdate = ?.

			FIND FIRST ACCT 
			WHERE ACCT.BAL-ACCT = 40817
			  AND ACCT.CUST-CAT = "Ч"
                          AND ACCT.CUST-ID = LOAN.CUST-ID
			NO-LOCK NO-ERROR.
				IF AVAIL ACCT THEN
				  DO:
   					/* счёт 40817 клиента */
					avt.tschet = ACCT.NUMBER.
                	                RUN acct-pos IN h_base (ACCT.ACCT, ACCT.CURRENCY, today, today, 'Ф').
                        	        /* остаток на счёте 40817 клиента */
                                	avt.tbal = - sh-bal.
                                	
                                	RUN acct-pos IN h_base (ACCT.ACCT, ACCT.CURRENCY, today, today, 'ФБН').
                                    /* остаток на счёте 40817 клиента */
                                    avt.ost40817 = - sh-bal.
          
                                    find first op-entry where op-entry.acct-db = acct.acct
                                        and substring(op-entry.acct-cr,1,2) = '30'
                                        and op-entry.op-date >= loan.open-date 
                                        no-lock no-error.
                                    
                                    if avail op-entry then do:
                                        avt.ppdate = op-entry.op-date.
                                    end.
                  END.
	                        ELSE 
	                          DO:
					avt.tschet = "НЕТ СЧЁТА".
                                	avt.tbal = 0.
                                	avt.ost40817 = 0.
	                          END.



			/* найдем портфель */
			FIND FIRST term-obl OF loan
				WHERE term-obl.idnt = 128
			NO-LOCK NO-ERROR.
			/**/
			IF AVAILABLE term-obl THEN
				DO:
					/**/
					DATE1 = term-obl.end-date.
					/**/
					FIND FIRST ploan
						WHERE ploan.contract = term-obl.lnk-contract 
						AND ploan.cont-code = term-obl.lnk-cont-code
					NO-LOCK NO-ERROR.
					/**/
					IF AVAIL ploan THEN
						DO:
							/**/
							avt.tportf = ploan.DOC-REF.
							avt.tkk = ploan.risk.
							/**/
						END.
				END.
			/* найдем кэф резерва */
			FIND FIRST comm-rate 
				WHERE comm-rate.kau = loan.contract + "," + loan.cont-code
				AND comm-rate.commission = "%Рез"
			NO-LOCK NO-ERROR.
			/**/
				IF AVAIL comm-rate THEN
					DO:
						/**/
						DATE2 = comm-rate.since.
						COEF_REZ = comm-rate.rate-comm.
						/**/
					END.
			/* определимся со значениями */
			IF ( DATE1 > END-DATE OR DATE1 = ? ) AND DATE2 <= END-DATE AND DATE2 <> ? THEN
				DO:
					/**/
					avt.tkk = COEF_REZ.
					avt.tportf = "".
					/**/
				END.
			/**/

			/* найдем проводку по 91312 */
			FIND FIRST LOAN-ACCT
				WHERE LOAN-ACCT.CONTRACT = LOAN.CONTRACT
				AND LOAN-ACCT.CONT-CODE = LOAN.CONT-CODE
				AND SUBSTRING(LOAN-ACCT.ACCT, 1, 5) = "91312"
			NO-LOCK NO-ERROR.
				/**/
				IF AVAIL LOAN-ACCT THEN
					DO:
						/**/
						FIND FIRST OP-ENTRY
							WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
							AND OP-ENTRY.OP-DATE = end-date
						NO-LOCK NO-ERROR.
						/**/
							IF AVAIL OP-ENTRY THEN
								DO:
									avt.fpr91312 = "ЕСТЬ".
								END.
							ELSE
								DO:
									avt.fpr91312 = "НЕТ".
								END.
					END.
				/**/
		/* если проводку за указанный день не нашли */
		IF avt.fpr91312 = "НЕТ" THEN
			DO:
				/**/
				FIND FIRST OP-ENTRY
					WHERE OP-ENTRY.ACCT-CR = LOAN-ACCT.ACCT
					AND OP-ENTRY.OP-DATE > end-date
					AND OP-ENTRY.OP-DATE <= TODAY
				NO-LOCK NO-ERROR.
				/**/
					IF AVAIL OP-ENTRY THEN
						DO:
							avt.mdate = OP-ENTRY.OP-DATE.
							avt.mFL = "ЕСТЬ".
							/**/
						END.
				/**/
			END.
		/**/
		VALIDATE avt.
END.
/*
run instview.p(TEMP-TABLE avt:HANDLE). 
*/

/**/
fname = "./avtoplus_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  userid('bisquit') + ".xml".	

IF LASTKEY EQ 27 THEN
	RETURN.

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
  <Created>2014-06-11T08:54:02Z</Created>
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
  <Style ss:ID="Default_b" ss:Name="Default_b">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:Bold="1"/>
  </Style>
  <Style ss:ID="s65">
    <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="NumFormat" ss:Name="NumFormat_b">
    <NumberFormat ss:Format="### ### ##0.00" />
  </Style>
  <Style ss:ID="NumFormat_b" ss:Name="NumFormat">
    <NumberFormat ss:Format="### ### ##0.00" />
    <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table>
   <Column ss:AutoFitWidth="0" ss:Width="80.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="80.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="120.5" ss:Span="1"/>
   <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="140"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Row ss:AutoFitHeight="0">
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Дата С</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Дата выдачи</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">№ КД</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">ФИО</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Сумма на выдачу</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Счёт клиента</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Выдача факт</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Наличие ПП</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Дата ПП</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Остаток 40817</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">КК</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Портфель</Data></Cell>
    <Cell ss:StyleID="Default_b"><Data ss:Type="String">Проводка по 91312 за ' + STRING(end-date, "99.99.9999") + '</Data></Cell>
	 <Cell ss:StyleID="Default_b"><Data ss:Type="String">Дата</Data></Cell>
	 <Cell ss:StyleID="Default_b"><Data ss:Type="String">Проводка по 91312 позже</Data></Cell>
   </Row>'.

	FOR EACH avt
	NO-LOCK:
	   ASSIGN
         vSumForVyd = vSumForVyd + avt.SumForVyd
         vOp4       = vOp4       + avt.Op4      
         vTbal      = vTbal      + avt.Tbal     
         vOst40817  = vOst40817  + avt.Ost40817 
      .
   	/**/
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.tDate) + '</Data></Cell>\n'.
		      IF avt.op4Date <> ? THEN DO:
            PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.op4Date) + '</Data></Cell>\n'.
        END.
        ELSE DO:
            PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String"></Data></Cell>\n'.
        END.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.tnum) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.tfname) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat"><Data ss:Type="Number">' + STRING(avt.sumforvyd) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.tschet) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat"><Data ss:Type="Number">' + STRING(avt.op4) + '</Data></Cell>\n'.	
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat"><Data ss:Type="Number">' + STRING(avt.tbal) + '</Data></Cell>\n'.
        IF avt.ppDate <> ? THEN DO:
            PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.ppDate) + '</Data></Cell>\n'.
        END.
        ELSE DO:
            PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String"></Data></Cell>\n'.
        END.	
		/* PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="Number">' + STRING(avt.tbal,"->>>,>>>,>>>,>>9.99") + '</Data></Cell>\n'. */
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat"><Data ss:Type="Number">' + STRING(avt.ost40817) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat"><Data ss:Type="Number">' + STRING(avt.tkk) + '</Data></Cell>\n'.  
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.tportf) + '</Data></Cell>\n'. 
		
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.fpr91312) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.mDate) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell><Data ss:Type="String">' + STRING(avt.mFL) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.	   
		/**/
	END.
	   /*pda вывод итогов*/
      PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="Default_b"><Data ss:Type="String">ИТОГО</Data></Cell>\n'.
         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat_b" ss:Index="5" ><Data ss:Type="Number">' + STRING(vSumForVyd) + '</Data></Cell>\n'. 
         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat_b" ss:Index="7" ><Data ss:Type="Number">' + STRING(vOp4)       + '</Data></Cell>\n'. 
         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat_b" ss:Index="8" ><Data ss:Type="Number">' + STRING(vTbal)      + '</Data></Cell>\n'. 
         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="NumFormat_b" ss:Index="10"><Data ss:Type="Number">' + STRING(vOst40817)  + '</Data></Cell>\n'. 
      PUT STREAM puk UNFORMATTED '</Row>\n'. 

		put stream puk unformatted
		'
			  </Table>
			</Worksheet>
		 </Workbook>
		'.


output stream puk close.

RUN sndbispc ("file=" + fname + ";class=bq").
/* конец вывода */			
   

/* находим наименование клиента */
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




