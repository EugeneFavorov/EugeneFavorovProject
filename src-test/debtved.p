/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment:ведомость по задолженности (юрика,физ)
   Parameters:
         Uses:
      Used by:
      Created: kam + vvv
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

DEFINE INPUT PARAM my_CustCat AS CHAR NO-UNDO. /* CUST_CAT Ю(Ч) */

DEFINE BUFFER 	bbloan 		FOR loan.
DEFINE BUFFER 	bbloan2		FOR loan.
DEFINE VAR		custN	 	AS CHAR NO-UNDO.
DEFINE VAR		tmp_type 	AS CHAR INIT '' NO-UNDO.

def new shared stream puk.
def var fname as char  init "./credits.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.

/* TEMPORARY */
def var FprosrOd 	AS DECIMAL INIT 0 NO-UNDO.	/* просроченный ОД */
def var FprosrProc 	AS DECIMAL INIT 0 NO-UNDO.	/* просроченные %% */
def var Fpeny 		AS DECIMAL INIT 0 NO-UNDO.	/* пени */
def var Fod 		AS DECIMAL INIT 0 NO-UNDO.	/* остаток ОД */
def var Fcnt		AS DECIMAL INIT 0 NO-UNDO.

/* TOTAL */
def var Tcnt		AS DECIMAL INIT 0 NO-UNDO.


DEFINE TEMP-TABLE tt-debt
    FIELD custId 	AS INT64	/* идентификатор клиента */
	FIELD custName	AS CHAR		/* наименование клиента */
    FIELD code		AS CHAR		/* номер КД */
	FIELD date		AS DATE		/* дата выдачи */
	FIELD type 		AS DECIMAL	/* тип записи: 1 - обычный КД, 2 - КЛ, 3 - транш по КЛ */
    FIELD prosrOd 	AS DECIMAL	/* просроченный ОД */
    FIELD prosrProc AS DECIMAL	/* просроченные %% */
    FIELD peny 		AS DECIMAL	/* пени */
    FIELD od 		AS DECIMAL	/* остаток ОД */
    .
    
{empty tt-debt}


end-date = TODAY.
{getdate.i}


/* идем по отмеченным Юридическим лицам */
IF my_CustCat = 'Ю' THEN
	DO:
		FOR EACH tmprecid NO-LOCK,
			FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK:
			/* у которых есть незакрытые кредиты на текущую дату */
			FOR EACH loan WHERE loan.contract = 'Кредит'
				AND loan.cust-cat = my_CustCat
				AND loan.cust-id = cust-corp.cust-id
				AND loan.open-date <= end-date
				AND (loan.close-date = ? OR loan.close-date >= end-date)
				/* чтобы отсечь проход по каждой КЛ */
				AND NOT loan.class-code BEGINS 'loan-tran-lin'
				NO-LOCK BY loan.cont-code:
				/* найдем наименование */
				RUN GetName(INPUT my_CustCat,INPUT cust-corp.cust-id,INPUT-OUTPUT custN).
							
				IF loan.cont-type = 'Течение' THEN
				
					/* вставки Кредитной линии */
					DO:						
						RUN CalcLine(INPUT loan.contract,INPUT loan.cont-code,INPUT cust-corp.cust-id,INPUT custN).
					END.
				
				ELSE 
					/* вставка одного договора */
					DO:
						RUN CalcOne(INPUT loan.contract,INPUT loan.cont-code,INPUT cust-corp.cust-id,INPUT custN,INPUT 1).
					END.
				
				 
			END.
		END.    
	END.

/* идем по отмеченным Физическим лицам */
IF my_CustCat = 'Ч' THEN
	DO:
		FOR EACH tmprecid NO-LOCK,
			FIRST person WHERE RECID(person) EQ tmprecid.id NO-LOCK:
			/* у которых есть незакрытые кредиты на текущую дату */
			FOR EACH loan WHERE loan.contract = 'Кредит'
				AND loan.cust-cat = my_CustCat
				AND loan.cust-id = person.person-id
				AND loan.open-date <= end-date
				AND (loan.close-date = ? OR loan.close-date >= end-date)
				/* чтобы отсечь проход по каждой КЛ */
				AND NOT loan.class-code BEGINS 'loan-tran-lin'
				NO-LOCK BY loan.cont-code:
						
				/* найдем наименование */
				RUN GetName(INPUT my_CustCat,INPUT person.person-id,INPUT-OUTPUT custN).
				
				IF loan.cont-type = 'Течение' THEN
					/* вставки Кредитной линии */
					DO: 
						RUN CalcLine(INPUT loan.contract,INPUT loan.cont-code,INPUT person.person-id,INPUT custN).
					END.
				
				ELSE 
					/* вставка одного договора */
					DO:
						RUN CalcOne(INPUT loan.contract,INPUT loan.cont-code,INPUT person.person-id,INPUT custN,INPUT 1).
					END.
				
				 
			END.
		END.    
	END.

/* идем по отмеченным Кредитам */	
IF my_CustCat = 'К' THEN
	DO:
		FOR EACH tmprecid NO-LOCK,
			FIRST loan WHERE RECID(loan) EQ tmprecid.id
			AND loan.contract = 'Кредит'
			AND loan.open-date <= end-date
			AND (loan.close-date = ? OR loan.close-date >= end-date)
			NO-LOCK BY loan.cont-code:
							
				/* найдем наименование */
				RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT custN).

				IF loan.cont-type = 'Течение' THEN
					DO:
						/* вставка основного договора КЛ */
						RUN CalcOne(INPUT loan.contract,INPUT loan.cont-code,INPUT loan.cust-id,INPUT custN,INPUT 2).
					END.
				
				ELSE 
					DO:
						IF INDEX(loan.cont-code, " ") > 0 THEN
							DO:
								/* вставка транша */
								RUN CalcOne(INPUT loan.contract,INPUT loan.cont-code,INPUT loan.cust-id,INPUT custN,INPUT 3).
							END.
						ELSE
							DO:
								/* обычный договор */
								RUN CalcOne(INPUT loan.contract,INPUT loan.cont-code,INPUT loan.cust-id,INPUT custN,INPUT 1).
							END.
					END.
		END.    	
	END.

/* вывод */	
/*
run instview.p(TEMP-TABLE tt-debt:HANDLE). 
*/

fname = "./credits_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	


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
  <LastSaved>2014-04-14T03:05:08Z</LastSaved>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>27795</WindowWidth>
  <WindowTopX>675</WindowTopX>
  <WindowTopY>3300</WindowTopY>
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
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s64">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s65">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table ss:ExpandedColumnCount="6" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:AutoFitWidth="0" ss:Width="149.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="88.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="114.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="109.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="120.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="128.25"/>
   <Row ss:AutoFitHeight="0" ss:Height="29.25">
    <Cell ss:StyleID="s62"><Data ss:Type="String">Кредит</Data></Cell>
    <Cell ss:StyleID="s62"><Data ss:Type="String">Дата выдачи</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Просроченный ОД</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Просроченные %%</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Штрафы/пени</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Остаток ОД</Data></Cell>
   </Row>
   '.
/*  */ 
FOR EACH tt-debt
NO-LOCK BY tt-debt.custId BY tt-debt.code BY tt-debt.type:

	/* если КД относится к КЛ */
	IF tt-debt.type > 1 AND (tmp_type = '' OR tmp_type = tt-debt.code OR tmp_type = SUBSTRING(tt-debt.code,1,INDEX(tt-debt.code, " "))) THEN 
		/* запомним суммы */
		DO:
			FprosrOd = FprosrOd + tt-debt.prosrOd.
			Fpeny = Fpeny + tt-debt.peny.
			FprosrProc = FprosrProc + tt-debt.prosrProc.
			Fod = Fod + tt-debt.od.
			Fcnt = Fcnt + 1.
			/**/

			/**/
			IF tmp_type = '' THEN 
				DO:
					IF INDEX(tt-debt.code, " ") > 0 THEN
						tmp_type = SUBSTRING(tt-debt.code,1,INDEX(tt-debt.code, " ")).
					ELSE
						tmp_type = tt-debt.code.
				END.
			/**/
		END.
	ELSE
		DO:	
			/* если есть, то*/
			/* нужно вывести подытоговую строку по КЛ */
			IF Fcnt > 1 THEN
				DO:
					PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s64"><Data ss:Type="String">' + 'Итого задолженность:' + '</Data></Cell>\n'.
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + ' ' + '</Data></Cell>\n'.
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(FprosrOd) + '</Data></Cell>\n'.
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(Fpeny) + '</Data></Cell>\n'.
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(FprosrProc) + '</Data></Cell>\n'. 
					PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(Fod) + '</Data></Cell>\n'. 
					PUT STREAM puk UNFORMATTED '</Row>\n'.	
					/* обнулить текущие суммы */
					FprosrOd = 0.
					Fpeny = 0.
					FprosrProc = 0.
					Fod = 0.
					Fcnt = 0.
					tmp_type = ''.
				END.
			
			IF 	tt-debt.type > 1 THEN 
				DO:
					FprosrOd = tt-debt.prosrOd.
					Fpeny = tt-debt.peny.
					FprosrProc = tt-debt.prosrProc.
					Fod = tt-debt.od.
					Fcnt = 1.
					/**/
					IF INDEX(tt-debt.code, " ") > 0 THEN
						tmp_type = SUBSTRING(tt-debt.code,1,INDEX(tt-debt.code, " ")).
					ELSE
						tmp_type = tt-debt.code.
					/**/
				END.
			/*
			ELSE
				DO:
				
				END.
			*/
		END.
		
		
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s64"><Data ss:Type="String">' + tt-debt.code + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(tt-debt.date, "99.99.9999") + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(tt-debt.prosrOd) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(tt-debt.peny) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(tt-debt.prosrProc) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(tt-debt.od) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '</Row>\n'.
		
END.

/* если есть, то*/
/* нужно вывести подытоговую строку по КЛ */

IF Fcnt > 1 THEN
	DO:
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s64"><Data ss:Type="String">' + 'Итого задолженность:' + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + ' ' + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(FprosrOd) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(Fpeny) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(FprosrProc) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s65"><Data ss:Type="Number">' + STRING(Fod) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
	END.

put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

/* конец вывода */		
 
 
/* расчет линии договора */
PROCEDURE CalcLine:
	DEF INPUT PARAMETER contract AS CHARACTER.
    DEF INPUT PARAMETER cont-code AS CHARACTER.
    DEF INPUT PARAMETER custId AS INT64. 
	DEF INPUT PARAMETER custName AS CHARACTER.
	
	/* для основного договора */
	FIND FIRST bbloan
	WHERE bbloan.contract = contract
	AND bbloan.cont-code = cont-code
	NO-LOCK NO-ERROR.
	
	IF AVAIL bbloan THEN 
		DO:
			/* добавляем строку */
			RUN CalcOne(INPUT bbloan.contract,INPUT bbloan.cont-code,INPUT custId,INPUT custName,INPUT 2).
		END.
	
	/* для каждого транша */
	FOR EACH bbloan2
	WHERE bbloan2.contract = contract
	AND bbloan2.cont-code MATCHES(cont-code + " *")
	AND bbloan2.class-code BEGINS 'loan-tran-lin'
	NO-LOCK:
		/* добавляем по строке */
		RUN CalcOne(INPUT bbloan2.contract,INPUT bbloan2.cont-code,INPUT custId,INPUT custName,INPUT 3).
	END.
END.

/* находим наименование клиента */
PROCEDURE GetName:
	DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
	DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
	IF cat = "Ч" THEN

		FIND FIRST PERSON 
		WHERE PERSON.PERSON-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL PERSON THEN
			/* ФИО клиента */
			sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
	ELSE

		FIND FIRST CUST-CORP 
		WHERE CUST-CORP.CUST-ID = id
		NO-LOCK NO-ERROR.
			IF AVAIL CUST-CORP THEN
			/* наименование организации */
			sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
END.			

/* расчет одного договора */
PROCEDURE CalcOne:
	DEF INPUT PARAMETER contract AS CHARACTER.
    DEF INPUT PARAMETER cont-code AS CHARACTER.
    DEF INPUT PARAMETER custId AS INT64.  
	DEF INPUT PARAMETER custName AS CHARACTER.
	DEF INPUT PARAMETER type AS DECIMAL.
    
    DEFINE VARIABLE param0   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param7   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param13  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param8   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param9   AS DECIMAL NO-UNDO.         
    DEFINE VARIABLE param11  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param12  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param14  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param15  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE param18  AS DECIMAL NO-UNDO.                
    DEFINE VARIABLE param82  AS DECIMAL NO-UNDO.                 
    DEFINE VARIABLE param10  AS DECIMAL NO-UNDO.       
    DEFINE VARIABLE param16  AS DECIMAL NO-UNDO.       
    DEFINE VARIABLE param48  AS DECIMAL NO-UNDO.       
    DEFINE VARIABLE param248 AS DECIMAL NO-UNDO.       
    DEFINE VARIABLE param229 AS DECIMAL NO-UNDO.
		
    FIND FIRST bbloan
		WHERE bbloan.contract = contract
		AND bbloan.cont-code = cont-code
		NO-LOCK NO-ERROR.
		
    IF AVAIL bbloan THEN DO:
	
        /* Остаток срочной задолженности */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        0,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param0).    			/* Значение параметра */    
		/* Просроченные заемные средства */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        7,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param7).    			/* Значение параметра */   
        /* Просроч. заемные средства до выясн. */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        13,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param13).    		/* Значение параметра */   
        /* просроченный ОД */
        param7 = param7 + param13.
        /* Штр. %% за просроч. заемные средства */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        8,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param8).    			/* Значение параметра */ 
        /* Пеня за просроч. заемные средства */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        9,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param9).    			/* Значение параметра */ 
        /* Штр. %% за просроченные %% */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        11,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param11).    		/* Значение параметра */ 
		/* Пеня за просроченные %% */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        12,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param12).    		/* Значение параметра */ 
		/* Штр. %% за пр. до выясн. заемные ср-ва */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        14,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param14).    		/* Значение параметра */
		/* Пеня за просроч. заемные ср-ва до выясн. */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        15,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param15).    		/* Значение параметра */
		/* Пеня за просроч. %% до выяснения */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        18,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param18).    		/* Значение параметра */
		/* Пеня за пр.%% на внебалансе */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        82,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param82).    		/* Значение параметра */
        /* пени */
        param8 = param8 + param9 + param11 + param12 + param14 + param15 + param18 + param82.
        /* Просроч. задолженность по процентам */   
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        10,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param10).    		/* Значение параметра */           
		/* Просроченные %% до выяснения */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        16,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param16).    		/* Значение параметра */  
        /* Просроченные %% на внебалансе */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        48,        					/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param48).    		/* Значение параметра */  
		/* Проср. %% на в/б за проср. о.д. */
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        248,        				/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param248).    		/* Значение параметра */  
        /* %% на в/б. за проср. о.д. */                                                        
        RUN PRM(
        bbloan.Contract,            /* Назначение договора */
        bbloan.Cont-Code,           /* Номер договора */
        229,        				/* Код параметра  */
        bbloan.since,               /* Значение параметра на дату состояния договора */
        TRUE,                   	/* считать % */
        OUTPUT param229).    		/* Значение параметра */ 
        /* просроченные %% */
        param10 = param10 + param16 + param48 + param248 + param229.
		/* TOTAL */	
		Tcnt = Tcnt + 1.
		
		/*  */
		/*  */
        CREATE tt-debt.
        ASSIGN
            tt-debt.custId = custId
			tt-debt.custName = custName
            tt-debt.code = cont-code
			tt-debt.date = bbloan.open-date
			tt-debt.type = type
            tt-debt.prosrOd = param7 
            tt-debt.peny = param8 
            tt-debt.prosrProc = param10 
            tt-debt.od = param0      
        .  
    END.

END.












