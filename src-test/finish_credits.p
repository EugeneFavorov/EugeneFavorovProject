/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment:отчет по заканчивающимся кредитам
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

{globals.i}
{tmprecid.def}
{param-dog.p}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get loan}

DEFINE VAR              custN           AS CHAR NO-UNDO.
DEFINE VAR              sizeN           AS DECIMAL INIT 0 NO-UNDO.
DEFINE BUFFER tt FOR TERM-OBL.
DEFINE BUFFER ff FOR TERM-OBL.

def new shared stream puk.
def var fname as char  init "./finish_credits.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
/* есть ли ЧДГ */
def var CHDG as logical view-as toggle-box no-undo.
def var beg-date as date view-as fill-in no-undo.
def var end-date as date view-as fill-in no-undo.
def var dates as date no-undo.
def var sum408 as decimal no-undo.
def var sum423 as decimal no-undo.
def var sum458 as decimal no-undo.
def var sumKredPr as decimal no-undo.
def var sumKredT as decimal no-undo.
def var par_9 as decimal no-undo.
def var par_519 as decimal no-undo.
def var sumPar9 as decimal no-undo.
def var sumPar519 as decimal no-undo.

def var acct91315 as char no-undo. /*КредВГар*/
def var sum91315 as decimal no-undo. /*КредВГар*/
  
 DEF VAR a1       AS DECIMAL NO-UNDO.
 DEF VAR a2       AS DECIMAL NO-UNDO.


DEFINE TEMP-TABLE fin
        FIELD code              AS CHAR         /* номер КД */
        FIELD dates             AS DATE      /* С */
        FIELD name              AS CHAR         /* наименование клиента */
    FIELD date          AS DATE         /* дата последнего платежа */
    FIELD size          AS DECIMAL      /* сумма последнего платежа */
    FIELD sum408        AS DECIMAL
    FIELD sum423        AS DECIMAL
    FIELD sum458        AS DECIMAL
    FIELD sum91315      AS DECIMAL
    FIELD acct91315     AS CHAR
    FIELD sumKredPr     AS DECIMAL
    FIELD sumKredT      AS DECIMAL
    FIELD sumPar9       AS DECIMAL
    FIELD sumPar519     AS DECIMAL
    .
 
{empty fin}
/**/
beg-date = TODAY.
end-date = TODAY.
CHDG = TRUE.
/**/
PAUSE 0.
DO on ERROR UNDO, LEAVE on ENDKEY UNDO, LEAVE WITH FRAME ftune:
  Update
        CHDG LABEL "Частичное досрочное гашение" 
        beg-date LABEL "С какого дня"
        end-date LABEL "По какой день"
  WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
  TITLE "[ Параметры Отчета ]".
END.
HIDE FRAME ftune NO-PAUSE.

IF LASTKEY EQ KEYCODE("ESC") THEN
        RETURN.

/* идем по всем КД, у которых последний платеж входит в указанный период*/
FOR EACH loan
        WHERE loan.contract = 'Кредит'
        AND loan.open-date <= end-date
        AND loan.close-date = ?
        AND loan.filial-id <> '0400'
        NO-LOCK,
                FIRST term-obl
                WHERE term-obl.contract = loan.contract 
                AND   term-obl.cont-code = loan.cont-code
                AND   term-obl.idnt = 2
                AND   term-obl.amt-rub = 0
                AND       term-obl.end-date >= beg-date
                AND       term-obl.end-date <= end-date
                NO-LOCK:                

                        FIND LAST ff
                        WHERE ff.CONTRACT = loan.contract
                        AND ff.CONT-CODE = loan.cont-code
                        AND ff.IDNT = 2
                        AND ff.END-DATE < beg-date
                        NO-LOCK NO-ERROR.
                        
                        IF AVAIL ff AND ff.AMT-RUB > 0 THEN
                                DO:
                                        /* найдем наименование клиента */
                                        RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT custN).
                                        
                                        /* найдем дату и сумму последнего платежа */
                                        FIND LAST tt
                                        WHERE tt.CONTRACT = TERM-OBL.CONTRACT
                                        AND       tt.CONT-CODE = TERM-OBL.CONT-CODE
                                        AND       tt.IDNT = 2
                                        AND   tt.AMT-RUB > 0 
                                        AND       tt.END-DATE < TERM-OBL.END-DATE
                                        NO-LOCK NO-ERROR.
                                                /* запомним размер последнего платежа */
                                                IF AVAIL tt THEN
                                                        DO:
                                                                sizeN = tt.AMT-RUB.
                                                        END.
                                                
                                        
                                        /* если установлено условие наличия ЧДГ*/
                                        IF CHDG = TRUE THEN
                                                DO:
                                                        sum408 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum408 = abs(sh-bal).
                                                        END.
                                                        sum423 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредРасч1' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum423 = abs(sh-bal).
                                                        END.
                                                        sum458 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредПр' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum458 = abs(sh-bal).
                                                        END.
                                                        sum91315 = 0.
                                                        acct91315 = ''.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредВГар' NO-LOCK NO-ERROR.
							if avail loan-acct then do:	
   	  						  	find last acct of loan-acct where acct.close-date = ? no-lock no-error.
	                                                        IF AVAIL acct THEN DO:
        	                                                        acct91315 = substring(loan-acct.acct,1,20).
                	                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                        	                                        sum91315 = abs(sh-bal).
                                	                        END.
							end.

                                                        sumKredPr = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредПр%' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sumKredPr = abs(sh-bal).
                                                        END.
							
                                                        sumKredT = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредТ' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sumKredT = abs(sh-bal).
                                                        END.
                                                        



                                                        dates = date("01/01/1900").
                                                        FOR EACH LOAN-COND
                                                        WHERE LOAN-COND.CONTRACT = LOAN.CONTRACT
                                                        AND     LOAN-COND.CONT-CODE = LOAN.CONT-CODE
                                                        NO-LOCK BY loan-cond.since:
                                                            dates = loan-cond.since.
                                                            leave.
                                                        end.

                                                        FOR EACH LOAN-COND
                                                        WHERE LOAN-COND.CONTRACT = LOAN.CONTRACT
                                                        AND     LOAN-COND.CONT-CODE = LOAN.CONT-CODE
                                                        NO-LOCK,
                                                                FIRST SIGNS 
                                                                WHERE SIGNS.FILE-NAME = "loan-cond"
                                                                AND SIGNS.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
                                                                AND SIGNS.CODE = 'PayType'
                                                                AND SIGNS.XATTR-VALUE = 'ДосрПогаш'
                                                                NO-LOCK:
                                                                        run all_param IN h_Loan(loan.contract,

                                                                         loan.cont-code,   /* Номер договора */
                                                                            9,                /* Код параметра */
                                                                            today,
                                                                            OUTPUT par_9,     /* Сумма параметра */
                                                                                OUTPUT a1,        /* Валюта параметра */
                                                                                         OUTPUT a2 /* Сумма параметра в рублях */
                                                                                        ).     

                                                                        run all_param IN h_Loan(loan.contract,

                                                                         loan.cont-code,   /* Номер договора */
                                                                            519,                /* Код параметра */
                                                                            today, 
                                                                            OUTPUT par_519,     /* Сумма параметра */
                                                                                OUTPUT a1,        /* Валюта параметра */
                                                                                         OUTPUT a2 /* Сумма параметра в рублях */
                                                                                        ).        
                                                                                        

                                                                        CREATE fin.
                                                                        ASSIGN
                                                                                fin.code = loan.cont-code
                                                                                fin.dates = dates
                                                                                fin.name = custN
                                                                                fin.date = TERM-OBL.END-DATE
                                                                                fin.size = sizeN
                                                                                fin.sum408 = sum408
                                                                                fin.sum423 = sum423
                                                                                fin.sum458 = sum458
                                                                                fin.sum91315 = sum91315
	                                                                        fin.acct91315 = acct91315
        			                                                fin.sumKredPr = sumKredPr
									                               	fin.sumKredT = sumKredT

                                                                   fin.sumPar9 = par_9         
                                                                   fin.sumPar519 = par_519
                                                                    
                                                                                .
                                                                                /* message par_9 view-as alert-box.   */
                                                                        LEAVE.  
                                                                END.
                                                END.
                                        
                                        /* иначе просто добавляем запись */
                                        ELSE
                                                DO:
                                                        sum408 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum408 = abs(sh-bal).
                                                        END.
                                                        sum423 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредРасч1' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum423 = abs(sh-bal).
                                                        END.
                                                        sum458 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредПр' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum458 = abs(sh-bal).
                                                        END.
                                                        sum91315 = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредВГар' NO-LOCK NO-ERROR.
                                                    if avail loan-acct then do: 
                                                      find last acct of loan-acct where acct.close-date = ? no-lock no-error.
                                                        IF AVAIL acct THEN DO:
                                                                acct91315 = substring(loan-acct.acct,1,20).
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sum91315 = abs(sh-bal).
                                                        END.

                                                    end.
                                                        else do:
                                                                acct91315 = ''.
                                                        end.

                                                        sumKredPr = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредПр%' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sumKredPr = abs(sh-bal).
                                                        END.
							
                                                        sumKredT = 0.
                                                        FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                                                                AND loan-acct.cont-code = loan.cont-code
                                                                AND loan-acct.acct-type = 'КредТ' NO-LOCK NO-ERROR.

                                                        IF AVAIL loan-acct THEN DO:
                                                                RUN acct-pos IN h_base (loan-acct.acct, '', end-date, end-date, ?).
                                                                sumKredT = abs(sh-bal).
                                                        END.

                                                        dates = date("01/01/1900").
                                                        FOR EACH LOAN-COND
                                                        WHERE LOAN-COND.CONTRACT = LOAN.CONTRACT
                                                        AND     LOAN-COND.CONT-CODE = LOAN.CONT-CODE
                                                        NO-LOCK BY loan-cond.since:
                                                            dates = loan-cond.since.
                                                            leave.
                                                        end.

                                                              run all_param IN h_Loan(loan.contract,

                                                                         loan.cont-code,   /* Номер договора */
                                                                            9,                /* Код параметра */
                                                                            today,
                                                                            OUTPUT par_9,     /* Сумма параметра */
                                                                                OUTPUT a1,        /* Валюта параметра */
                                                                                         OUTPUT a2 /* Сумма параметра в рублях */
                                                                                        ).     

                                                             run all_param IN h_Loan(loan.contract,

                                                                         loan.cont-code,   /* Номер договора */
                                                                            519,                /* Код параметра */
                                                                            today, 
                                                                            OUTPUT par_519,     /* Сумма параметра */
                                                                                OUTPUT a1,        /* Валюта параметра */
                                                                                         OUTPUT a2 /* Сумма параметра в рублях */
                                                                                        ).                          
                                                CREATE fin.
                                                ASSIGN
                                                        fin.code = loan.cont-code
                                                        fin.dates = dates
                                                        fin.name = custN
                                                        fin.date = TERM-OBL.END-DATE
                                                        fin.size = sizeN
                                                        fin.sum408 = sum408
                                                        fin.sum423 = sum423
                                                        fin.sum458 = sum458
                                                        fin.sum91315 = sum91315
                                                        fin.acct91315 = acct91315
                                                        fin.sumKredPr = sumKredPr
							fin.sumKredT = sumKredT

                                                        fin.sumPar9 = par_9         
                                                        fin.sumPar519 = par_519
                                                        .  
                                                
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
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>7740</WindowHeight>
  <WindowWidth>13395</WindowWidth>
  <WindowTopX>120</WindowTopX>
  <WindowTopY>45</WindowTopY>
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
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s64">
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
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
4    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s66">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
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
  <Style ss:ID="s67">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s69">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
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
 </Styles>
 <Worksheet ss:Name="Кредиты">
  <Table x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s62" ss:DefaultRowHeight="15">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="80"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="156.75"/>
      <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="105"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="213"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="105"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="126.75"/>
   <Row ss:AutoFitHeight="0" ss:Height="31.5">
    <Cell ss:StyleID="s64"><Data ss:Type="String">Филиал</Data></Cell>
    <Cell ss:StyleID="s64"><Data ss:Type="String">Кредит</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">Дата C</Data></Cell>
    <Cell ss:StyleID="s64"><Data ss:Type="String">Клиент</Data></Cell>
    <Cell ss:StyleID="s65"><Data ss:Type="String">Дата последнего платежа</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма платежа</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток 40817</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток 42301</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток 45815</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток 91315</Data></Cell>
    <Cell ss:StyleID="s64"><Data ss:Type="String">Счет КредВГар</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток КредПр%</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Остаток КредТ</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Пени</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Пени птс</Data></Cell>

   </Row>
   '.
        /*  */ 
        FOR EACH fin
        NO-LOCK BY fin.date BY fin.code:

                                        PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + ENTRY(1,ENTRY(2,fin.code,"@")," ") + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + delFilFromLoan(fin.code) + '</Data></Cell>\n'.
                                         PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(fin.dates, "99.99.9999") + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + fin.name + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s68"><Data ss:Type="String">' + STRING(fin.date, "99.99.9999") + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.size, '->>>>>>>9.99')) + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sum408, '->>>>>>>9.99')) + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sum423, '->>>>>>>9.99')) + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sum458, '->>>>>>>9.99')) + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sum91315, '->>>>>>>9.99')) + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + fin.acct91315 + '</Data></Cell>\n'. 
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sumKredPr, '->>>>>>>9.99')) + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sumKredT, '->>>>>>>9.99')) + '</Data></Cell>\n'.


                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sumPar9, '->>>>>>>9.99')) + '</Data></Cell>\n'.
                                        PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s69"><Data ss:Type="Number">' + TRIM(STRING( fin.sumPar519, '->>>>>>>9.99')) + '</Data></Cell>\n'.

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
 

/* находим наименование клиента */
PROCEDURE GetName:
        DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
        DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
        
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
