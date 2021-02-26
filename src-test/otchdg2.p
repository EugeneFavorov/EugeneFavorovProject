/* отчет по проводкам чдг, пдг -- с 40817 */
/* kam */

DEFINE VARIABLE sName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE sContCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskRS   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDateLoan AS DATE NO-UNDO.
DEFINE VARIABLE myStartDate AS DATE NO-UNDO.
DEFINE VARIABLE myEndDate AS DATE NO-UNDO.
DEFINE VARIABLE strTrans AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE strTrans1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE strTrans2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst AS DECIMAL NO-UNDO.
DEFINE VARIABLE countloan AS INT NO-UNDO.
DEFINE VARIABLE strloan   AS CHARACTER NO-UNDO.

{globals.i}
{sh-defs.i}

DEFINE BUFFER   bloan-acct  FOR loan-acct.
DEFINE BUFFER   bloan       FOR loan.
DEFINE BUFFER   bop-entry   FOR op-entry.

DEFINE TEMP-TABLE tt-otchdg
   FIELD dateop    AS DATE          /* дата  операции */
   FIELD dateplop  AS DATE        /* плановая дата  операции */
   FIELD sContCode AS CHARACTER     /* номер договора */
   FIELD dateloan  AS DATE          /* дата  договора */
   FIELD sName	   AS CHARACTER		   /* ФИО */
   FIELD summop	   AS DECIMAL		   /* сумма проводки */
   FIELD acct	   AS CHARACTER		   /* кредрасч 40817 */
   FIELD ost	   AS DECIMAL		   /* сумма кредрасч */
   FIELD sumafter  AS DECIMAL          /* зачислено после чдг */
   FIELD dateclose AS DATE            /* дата  закрытия 40817 */
   FIELD loan40817 AS CHARACTER            /* Счет связан с КД */
   FIELD ostKredRasp AS DECIMAL
   INDEX sContCode sContCode       
   .

PAUSE 0.

ASSIGN
myStartDate = TODAY
myEndDate = TODAY
strTrans = 'ПДГ2'
strTrans1 = 'ПДГ2'
strTrans2 = 'chdg_new,ChDG,093008++'
mMaskRS = "40817*,47422810*170*".

PAUSE 0.

DEFINE FRAME frame_date_codes 
   myStartDate LABEL "С даты"
   myEndDate LABEL "По дату"
   strTrans LABEL "Код Транзакции" HELP "F1 - выбор транзакций ПДГ/ЧДГ"
   WITH 1 COL 1 DOWN
	 
   WIDTH 50 CENTERED OVERLAY TITLE "Отчет по ЧДГ, ПДГ".



ON 'F1' OF myStartDate
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN ASSIGN myStartDate:SCREEN-VALUE = pick-value.
   END.

ON 'F1' OF myEndDate
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN ASSIGN myEndDate:SCREEN-VALUE = pick-value.
   END.
ON 'F1' OF strTrans
   DO:
      IF FRAME-VALUE = strTrans1 THEN 
      DO: 
         strTrans:SCREEN-VALUE = STRING(strTrans2).
         strTrans = strTrans2.
      END.		    
      ELSE 
      DO:
         strTrans:SCREEN-VALUE = STRING(strTrans1).
         strTrans = strTrans1.
      END.
   END.

PAUSE 0.
UPDATE
   myStartDate
   myEndDate 
   strTrans
   WITH FRAME frame_date_codes.


{empty tt-otchdg}

mainblock:
FOR EACH op WHERE 
   op.op-date >= myStartDate
   AND op.op-date <= myEndDate
   AND op.op-status BEGINS "√" 
   AND CAN-DO (strTrans, op.op-kind)

   NO-LOCK,
   EACH op-entry OF op NO-LOCK:
   ASSIGN
      sName = ''
      sContCode = ''
      dDateLoan = DATE("01/01/1900")
      most = 0.
   IF YES THEN 
   DO:
      FIND FIRST acct WHERE acct.acct = op-entry.acct-db NO-LOCK NO-ERROR.
      IF NOT AVAILABLE acct THEN 
         NEXT mainblock.

      FOR EACH loan-int OF op-entry NO-LOCK:
         sContCode = loan-int.cont-code.   
         FIND FIRST loan WHERE loan.contract = loan-int.contract 
            AND loan.cont-code = loan-int.cont-code
            NO-LOCK NO-ERROR.      
         IF AVAILABLE loan THEN dDateLoan = loan.open-date.
         LEAVE.
      END.

      IF sContCode EQ "" THEN  sContCode = acct.acct.

      FIND FIRST tt-otchdg WHERE 
                 tt-otchdg.dateop    EQ op.op-date AND
                 tt-otchdg.sContCode EQ sContCode
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL tt-otchdg AND NOT CAN-DO(mMaskRS,op-entry.acct-db) THEN
         NEXT mainblock.         

      IF op-entry.acct-db BEGINS "40817" THEN 
      DO:
         RUN acct-pos IN h_base (
            acct.acct,
            acct.currency,
            op.op-date,
            op.op-date, 
            ?).
         most = ABSOLUTE(IF acct.currency = "" THEN sh-bal ELSE sh-val ) .
      END.

      /* поиск КД с общим счетом 40817 КредРасч */
      IF AVAIL loan THEN
      FIND FIRST loan-acct WHERE 
                 loan-acct.contract  EQ loan.contract
             AND loan-acct.cont-code EQ loan.cont-code
             AND loan-acct.acct-type EQ 'КредРасч'
             AND SUBSTRING(loan-acct.acct,1,5) EQ '40817'
      NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN
      DO:
         strloan = ''.
         countloan = 1.
         FOR EACH bloan-acct WHERE
                  bloan-acct.contract  EQ 'Кредит'
              AND bloan-acct.acct-type EQ 'КредРасч'
              AND bloan-acct.cont-code NE loan-acct.cont-code
              AND bloan-acct.acct      EQ loan-acct.acct
            NO-LOCK, 
            FIRST bloan WHERE
                  bloan.contract   EQ bloan-acct.contract 
              AND bloan.cont-code  EQ bloan-acct.cont-code 
              AND bloan.filial-id  NE '0400' 
              AND bloan.close-date EQ ?
         NO-LOCK:
            countloan = countloan + 1.
            strloan   = strloan + ENTRY(1,bloan.cont-code,'@') + ','.
         END.  
      END.
      
      IF NOT AVAIL tt-otchdg THEN
      DO:
         CREATE tt-otchdg. 
         ASSIGN 
            tt-otchdg.dateplop  = op.contract-date
            tt-otchdg.dateop    = op.op-date
            tt-otchdg.sContCode = sContCode
            tt-otchdg.sumafter = 0.
      END.

      IF op-entry.acct-db BEGINS '408' AND op-entry.acct-cr BEGINS '45' THEN DO:
          FIND FIRST bop-entry WHERE  
            bop-entry.op-date = op-entry.op-date
            AND bop-entry.op-status BEGINS "√"
            AND bop-entry.acct-cr = op-entry.acct-db
            AND RECID(bop-entry) > RECID(op-entry)
            NO-LOCK NO-ERROR.
            IF AVAIL bop-entry THEN tt-otchdg.sumafter = tt-otchdg.sumafter + abs(bop-entry.amt-rub).
      END. 
      
      ASSIGN
         tt-otchdg.dateloan  = dDateLoan
         tt-otchdg.acct      = IF CAN-DO(mMaskRS,op-entry.acct-db) THEN op-entry.acct-db ELSE ""
         tt-otchdg.dateclose = IF CAN-DO(mMaskRS,op-entry.acct-db) THEN acct.close-date ELSE ?
         tt-otchdg.summop    = tt-otchdg.summop + IF CAN-DO(mMaskRS,op-entry.acct-db) THEN abs(op-entry.amt-rub) ELSE 0
         tt-otchdg.ost       = abs(most)
         tt-otchdg.loan40817 = strloan
         .
      IF tt-otchdg.sName EQ "" THEN
      DO:
          sName = "".
          IF AVAILABLE loan THEN 
             RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT sName).
          ELSE  IF acct.cust-cat EQ "Ч" THEN
             RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT sName).
          tt-otchdg.sName     = sName.
      END.
      tt-otchdg.ostKredRasp = 0.
      FIND FIRST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасп' NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, '', op.op-date, op.op-date, ?).
        tt-otchdg.ostKredRasp = abs(sh-bal).
      END.
      
   END.
END.  


/*
run instview.p(TEMP-TABLE tt-otchdg:HANDLE).				 
  */
DEFINE VARIABLE fname AS CHARACTER NO-UNDO.
DEFINE NEW SHARED STREAM vvs.

/* выводим в ексель */
fname = "./otchchdg"  + "_" + USERID('bisquit') + ".xml".
OUTPUT STREAM vvs TO VALUE (fname)
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
  <WindowHeight>11445</WindowHeight>
  <WindowWidth>15015</WindowWidth>
  <WindowTopX>510</WindowTopX>
  <WindowTopY>570</WindowTopY>
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
   <NumberFormat ss:Format="Short Date"/>
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom" ss:WrapText="1"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="left">
   <NumberFormat ss:Format="@"/>
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom" ss:WrapText="1"/>
  </Style>
  <Style ss:ID="s67" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s68" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s69" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Standard"/>
  </Style>
  <Style ss:ID="s70">
   <NumberFormat ss:Format="Standard"/>
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom" ss:WrapText="1"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="lll">
  <Table ss:ExpandedColumnCount="12" ss:ExpandedRowCount="29031978" x:FullColumns="1"
   x:FullRows="1">
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="159.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="159.75"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="142.5"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s70" ss:AutoFitWidth="0" ss:Width="111"/>
   <Row ss:Index="1" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата операции</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">Номер договора</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата выдачи</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">ФИО</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">Сумма</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">КредРасч 40817</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">Остаток на 40817 на дату операции</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">Зачислено после ЧДГ</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Дата закрытия 40817</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Счет связан с КД</Data></Cell>
    <Cell ss:StyleID="s69"><Data ss:Type="String">Остаток на КредРасп на дату операции</Data></Cell>
   </Row>
   '.

DEFINE VARIABLE tmpSum AS DECIMAL NO-UNDO.
FOR EACH tt-otchdg NO-LOCK BREAK BY tt-otchdg.dateop BY tt-otchdg.sContCode:
   IF FIRST-OF (tt-otchdg.sContCode) THEN tmpSum = 0.
   tmpSum = tmpSum + tt-otchdg.summop.
   IF LAST-OF (tt-otchdg.sContCode) THEN 
   DO:

   PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
     PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="DateTime">'.
     PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(tt-otchdg.dateplop)) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="DateTime">'.
     PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(tt-otchdg.dateop)) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED TRIM(ENTRY(1,tt-otchdg.sContCode,"@")) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
     PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(tt-otchdg.dateloan)) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED tt-otchdg.sname + '</Data></Cell>\n'.                              /*5*/
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED STRING(tmpSum,"->,>>>,>>>,>>9.99") + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED TRIM(ENTRY(1,tt-otchdg.acct,"@")) + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED STRING(tt-otchdg.ost,"->,>>>,>>>,>>9.99") + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED STRING(tt-otchdg.sumafter,"->,>>>,>>>,>>9.99") + '</Data></Cell>\n'.     
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED (IF tt-otchdg.dateclose NE ? THEN STRING(tt-otchdg.dateclose,"99.99.9999") ELSE "") + '</Data></Cell>\n'.
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED TRIM(tt-otchdg.loan40817," ,") + '</Data></Cell>\n'. 
     PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
     PUT STREAM vvs UNFORMATTED STRING(tt-otchdg.ostKredRasp,"->,>>>,>>>,>>9.99") + '</Data></Cell>\n'. 
   PUT STREAM vvs UNFORMATTED '</Row>\n'.


   /* PUT STREAM vvs UNFORMATTED SUBSTITUTE('
   <Row ss:AutoFitHeight="0">
    <Cell><Data ss:Type="DateTime">&1</Data></Cell>
    <Cell><Data ss:Type="DateTime">&2</Data></Cell>
    <Cell><Data ss:Type="String">&3</Data></Cell>
    <Cell><Data ss:Type="DateTime">&4</Data></Cell>
    <Cell><Data ss:Type="String">&5</Data></Cell>
    <Cell><Data ss:Type="Number">&6</Data></Cell>
    <Cell><Data ss:Type="String">&7</Data></Cell>
    <Cell><Data ss:Type="Number">&8</Data></Cell>' +
    (
    IF tt-otchdg.dateclose EQ ? THEN
    '<Cell><Data ss:Type="String">&9</Data></Cell>'
    ELSE
    '<Cell><Data ss:Type="DateTime">&9</Data></Cell>') +
    '</Row>',
         ISO-DATE(DATETIME(tt-otchdg.dateplop)),
         ISO-DATE(DATETIME(tt-otchdg.dateop)),
         TRIM(ENTRY(1,tt-otchdg.sContCode,"@")),
         ISO-DATE(DATETIME(tt-otchdg.dateloan)),
         tt-otchdg.sname,
         tmpSum,
         TRIM(ENTRY(1,tt-otchdg.acct,"@")),
         tt-otchdg.ost,
         (IF tt-otchdg.dateclose NE ? THEN ISO-DATE(DATETIME(tt-otchdg.dateclose)) ELSE "")
         ). */
   END.
END.

PUT STREAM vvs UNFORMATTED 
   '  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").

PROCEDURE GetName:
   DEFINE INPUT PARAMETER cat AS CHARACTER.
   DEFINE INPUT PARAMETER id AS INT64.
   DEFINE OUTPUT PARAMETER sname AS CHARACTER.
 
   IF cat = "Ч" THEN
   DO:
      FIND FIRST PERSON 
         WHERE PERSON.PERSON-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE PERSON THEN
         /* ФИО клиента */
         sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
   END.
   ELSE
   DO:
      FIND FIRST CUST-CORP 
         WHERE CUST-CORP.CUST-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE CUST-CORP THEN
         /* наименование организации */
         sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
   END.
END PROCEDURE.

 
 
