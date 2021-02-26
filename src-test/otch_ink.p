/* ведомость по инкассации */
/* kam */

{globals.i}
{pp-corr.p}

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.

DEFINE VAR fname AS CHAR NO-UNDO.
DEFINE VAR fstr AS CHAR INIT '' NO-UNDO.
DEFINE VAR bFind AS LOGICAL NO-UNDO.
DEFINE VAR sum_T AS DEC NO-UNDO.

DEF VAR tot-summ         AS DECIMAL.              /* общая сумма инкасации */
DEF VAR tot-summ_kom     AS DECIMAL.              /* общая сумма комиссии за инкасацию */
DEF VAR tot-summ_nds     AS DECIMAL.              /* общая ндс за комиссию*/
DEF VAR tot-summ_all     AS DECIMAL.              /* общая сумма коммиссии и НДС */

DEF INPUT PARAM iForm AS CHAR.

def new shared stream vvs.
  
DEFINE TEMP-TABLE otchink
        FIELD open_date    AS DATE                 /* дата проводки */
        FIELD acct_cl      AS CHAR                 /* счет заемщика */
        FIELD name_cl      AS CHAR                 /* наименование заемщика */
        FIELD summ         AS DECIMAL              /* сумма инкасации */
        FIELD summ_kom     AS DECIMAL              /* сумма комиссии за инкасацию */
        FIELD summ_nds     AS DECIMAL              /* ндс за комиссию*/
        FIELD summ_all     AS DECIMAL              /* сумма коммиссии и НДС */
        FIELD tarif        AS DECIMAL              /* тариф */
        FIELD count_op     AS INT64                /* количество проводок(выездов) */
        INDEX acct_cl acct_cl       
    .

IF iForm NE 'NO' THEN DO:

  {empty otchink}
      
      fname = "./otchink"  + "_" + userid('bisquit') + ".xml".

  tmpdate  = date(month(today),1,year(today)).
  beg-date = tmpdate.
  end-date = date_correct(month(today),0,31,year(today)).
   
  {getdates.i
  &NoInit    = "YES"
  }
    
    for each op where 
          op.op-date >= beg-date and
          op.op-date <= end-date and
          op.doc-type = '037п' and
          op.op-status begins "√" 
           no-lock,
          each op-entry of op no-lock:
              
               
          find first otchink where otchink.acct_cl = op-entry.acct-cr no-error.
          if avail otchink then 
          do: 
              otchink.summ = otchink.summ + abs(op-entry.amt-rub).
              otchink.count_op = otchink.count_op + 1.
          END.
          else do:
              nameCl = ''.
              find first acct where acct.acct = op-entry.acct-cr no-lock no-error.
              if avail acct then RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).
              create otchink.            
              assign
                  otchink.open_date = beg-date
                  otchink.acct_cl = op-entry.acct-cr
                  otchink.name_cl = nameCl
                  otchink.summ = abs(op-entry.amt-rub)
                  otchink.count_op = 1
                  otchink.tarif = 0
                  otchink.summ_kom = 0
                  .
              release otchink.
          end.
  end.

  FOR EACH otchink:
      bFind = FALSE.
      FOR EACH comm-rate WHERE comm-rate.commission = '%Инкас' 
          AND comm-rate.since <= otchink.open_date 
          AND comm-rate.acct = otchink.acct_cl
          AND comm-rate.min-value <= otchink.summ
          NO-LOCK BREAK BY comm-rate.min-value DESCENDING BY comm-rate.since DESCENDING:
            IF LAST-OF(comm-rate.min-value) AND LAST-OF(comm-rate.since) THEN DO: 
              bFind = TRUE. 
              IF comm-rate.rate-fixed = TRUE THEN DO:
                  otchink.tarif = comm-rate.rate-com.
                  sum_T         = comm-rate.rate-com * otchink.count_op.
                  otchink.summ_kom = sum_T * 100 / 118.
                  otchink.summ_nds = sum_T * 18  / 118.
                  otchink.summ_all = sum_T.
              END.
              ELSE DO:
                  otchink.tarif = comm-rate.rate-com.
                  sum_T         = comm-rate.rate-com / 100 * otchink.summ.
                  otchink.summ_kom = sum_T * 100 / 118.
                  otchink.summ_nds = sum_T * 18  / 118.
                  otchink.summ_all = sum_T.
              END.
              LEAVE.
            END.   
      END.
      IF bFind = FALSE THEN DO:
          FOR EACH comm-rate WHERE comm-rate.commission = '%Инкас' 
              AND comm-rate.since <= otchink.open_date 
              AND comm-rate.acct = '0'
              AND comm-rate.min-value <= otchink.summ
            NO-LOCK BREAK BY comm-rate.min-value DESCENDING BY comm-rate.since DESCENDING:  
            IF LAST-OF(comm-rate.min-value) AND LAST-OF(comm-rate.since) THEN DO:
              IF comm-rate.rate-fixed = TRUE THEN DO:
                  otchink.tarif = comm-rate.rate-com.
                  sum_T         = comm-rate.rate-com * otchink.count_op.
                  otchink.summ_kom = sum_T * 100 / 118.
                  otchink.summ_nds = sum_T * 18  / 118.
                  otchink.summ_all = sum_T.
              END.
              ELSE DO:
                  otchink.tarif = comm-rate.rate-com.
                  sum_T         = comm-rate.rate-com / 100 * otchink.summ.
                  otchink.summ_kom = sum_T * 100 / 118.
                  otchink.summ_nds = sum_T * 18  / 118.
                  otchink.summ_all = sum_T.
              END.
              LEAVE.
            END.    
          END.
       
      END.
  END.

  ASSIGN
    tot-summ     = 0
    tot-summ_all = 0
    tot-summ_nds = 0
    tot-summ_kom = 0
    .

  output stream vvs to value (fname)
          UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  put stream vvs unformatted 
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
     <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="85.5"/>
     <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
     <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00" ss:Span="1"/>
     <Column ss:Index="5" ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
     <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="101.25"/>   
     <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="101.25"/>
     <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="101.25"/>
     <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111.75"/>
     <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
      <Cell ss:StyleID="s67"><Data ss:Type="String">Дата</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Счет клиента</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Наименование клиента</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма инкассации</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма комиссии за инкасацию, согласно тарифам банка</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">НДС</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Сумма, взятая с клиента</Data></Cell>
      <Cell ss:StyleID="s66"><Data ss:Type="String">Тариф по договору</Data></Cell>
     </Row>
  '.



      FOR EACH otchink NO-LOCK:
          /*
          FIELD open_date    AS DATE                 /* дата проводки */
          FIELD acct_cl      AS CHAR                 /* счет заемщика */
          FIELD name_cl      AS CHAR                 /* наименование заемщика */
          FIELD summ         AS DECIMAL              /* сумма инкасации */
          FIELD summ_kom     AS DECIMAL              /* сумма комиссии за инкасацию */
          FIELD tarif        AS DECIMAL              /* тариф */
          */
          
         /* otchink.tarif = 0.1.
          otchink.summ_kom = otchink.summ * otchink.tarif. */ 
      put stream vvs unformatted
      '<Row ss:AutoFitHeight="0">'.
          IF otchink.open_date <> ? THEN DO:   
              put stream vvs unformatted  '<Cell><Data ss:Type="DateTime">'.
              put stream vvs unformatted string(year(otchink.open_date),"9999") + '-' + string(month(otchink.open_date),"99") + '-' + string(day(otchink.open_date),"99") + 'T00:00:00.000' + '</Data></Cell>\n'.
          END.    
          ELSE put stream vvs unformatted '<Cell><Data ss:Type="String"></Data></Cell>\n'.
          put stream vvs unformatted '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted otchink.acct_cl + '</Data></Cell>\n'.
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted otchink.name_cl + '</Data></Cell>\n'.
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchink.summ,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchink.summ_kom,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchink.summ_nds,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.        
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchink.summ_all,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.
          put stream vvs unformatted  '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchink.tarif,"->>>>>>>>>9.99")) + '</Data></Cell>\n'.         
          .
          put stream vvs unformatted '</Row>'.
          ASSIGN
            tot-summ     = tot-summ     + otchink.summ
            tot-summ_all = tot-summ_all + otchink.summ_all
            tot-summ_nds = tot-summ_nds + otchink.summ_nds
            tot-summ_kom = tot-summ_kom + otchink.summ_kom
            .
      END.

  put stream vvs unformatted
      '<Row ss:AutoFitHeight="0">\n
       <Cell></Cell>\n
       <Cell></Cell>\n
       <Cell ss:StyleID="s66"><Data ss:Type="String">Итого:</Data></Cell>\n
       <Cell><Data ss:Type="String">' + trim(string(tot-summ,"->>>>>>>>>9.99")) + '</Data></Cell>\n
       <Cell><Data ss:Type="String">' + trim(string(tot-summ_kom,"->>>>>>>>>9.99")) + '</Data></Cell>\n
       <Cell><Data ss:Type="String">' + trim(string(tot-summ_nds,"->>>>>>>>>9.99")) + '</Data></Cell>\n
       <Cell><Data ss:Type="String">' + trim(string(tot-summ_all,"->>>>>>>>>9.99")) + '</Data></Cell>\n
       </Row>'.      

  put stream vvs unformatted 
  '  </Table>
   </Worksheet>
  </Workbook>
  ' .

  output stream vvs close.
  RUN sndbispc ("file=" + fname + ";class=bq").

END.

IF iForm = 'YES' THEN DO: 
  
  /*объявляем handle для передачи temp-table в УТ*/
  DEF NEW SHARED VAR hO-tt AS HANDLE NO-UNDO.
  DEF VAR hO-b    AS HANDLE NO-UNDO.
  DEF VAR hO-btt  AS HANDLE NO-UNDO.

  /*заполняем таблицу для УТ*/
  hO-b = BUFFER otchink:HANDLE.

  CREATE TEMP-TABLE hO-tt.
  hO-tt:CREATE-LIKE("otchink").
  hO-tt:TEMP-TABLE-PREPARE("xotchink").
  
  hO-btt = hO-tt:DEFAULT-BUFFER-HANDLE.

  FOR EACH otchink:
    hO-btt:BUFFER-CREATE.
    hO-btt:BUFFER-COPY(hO-b).
  END.

    RETURN STRING(hO-tt).

END.

/*удаляем шаред*/
IF iForm = 'NO' THEN  
    DELETE OBJECT hO-tt.

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
    sname = "ИП " + PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
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