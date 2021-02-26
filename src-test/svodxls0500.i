/* Ведомость проводок пользователя/эл.хранения */

/* имя создаваемого файла */
fname = "./SVOD" + "_" + STRING(end-date,'99-99-9999') + "_" + tUserID + "_" + USERID('bisquit') + ".xml".

/* записываем в файл */
OUTPUT STREAM vvs TO VALUE (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
/* стили */  
PUT STREAM vvs UNFORMATTED 
'<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
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
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"\n
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="tab">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="head">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="13"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="botbor">\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="left">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
  </Style>\n
  <Style ss:ID="center">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
  </Style>\n
  <Style ss:ID="tab_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="tab_c">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="tab_l">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
 </Styles>\n
'.

/* создаем ведомость */
FOR EACH tt-op-day WHERE tt-op-day.hozdoc NE '-'
                   AND   CAN-DO(tUserID,tt-op-day.user-id)
                   AND   tt-op-day.razdel NE 'k'
    BREAK BY tt-op-day.hozdoc
          BY tt-op-day.branch-id
          BY tt-op-day.save-type
          BY tt-op-day.acct-cat
          BY tt-op-day.currency
          BY tt-op-day.acct-dbf
          BY tt-op-day.acct-dbl
          BY tt-op-day.doc-numt:

  tNull = FALSE.

  IF FIRST-OF(tt-op-day.hozdoc) THEN 
  DO:
    /* заполняем тип сшива */
    tStr = tt-op-day.hozdoc.

    CASE tStr:
      WHEN 'Докдн' THEN
        tShiv = 'Документы дня'.
      WHEN 'Агент' THEN
        tShiv = 'Агентские вознаграждения'.
      WHEN 'Хоздок' THEN
        tShiv = 'Хозяйственные операции'.
      WHEN 'Подотчет' THEN
        tShiv = 'Подотчетные суммы'.
    END CASE. 

    PUT STREAM vvs UNFORMATTED
      '   <Worksheet ss:Name="' + tShiv + '">\n
          <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="1500000" x:FullColumns="1"
           x:FullRows="1" ss:DefaultRowHeight="15">\n
           <Column ss:AutoFitWidth="0" ss:Width="108.85" ss:Span="1"/>\n
           <Column ss:Index="3" ss:AutoFitWidth="0" ss:Width="83.25" ss:Span="1"/>\n
           <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="54"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="55.5"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="54.75"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="59.25"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="57.25"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="63.75"/>\n
      '.
  END.

  IF FIRST-OF(tt-op-day.branch-id) THEN 
  DO:
    IF shFilial EQ '0500' THEN 
    DO:
      FIND FIRST code WHERE code.class EQ 'ТитПодразд'
                      AND   code.code  EQ  tt-op-day.branch-id 
      NO-LOCK NO-ERROR.
        IF AVAIL(code) THEN 
        DO:
          IF code.misc[3] EQ shFilial THEN
            vUserOtd = code.name.
          ELSE 
          DO:
            FIND FIRST b-code WHERE b-code.class EQ 'ТитПодразд'
                 AND   b-code.code EQ shFilial.
            IF AVAIL(b-code) THEN
              vUserOtd = b-code.name.
          END.
        END.
    END.

    /* шапка ведомости */
    PUT STREAM vvs UNFORMATTED
      ' <Row ss:Height="17.25">\n
         <Cell ss:MergeAcross="9" ss:StyleID="head"><Data ss:Type="String">' + (IF NOT iUserInp EQ 'serv' 
                                              THEN 'Ведомость проводок пользователя' 
                                              ELSE 'Ведомость документов по автоматическим транзакциям, электронное хранение') + '</Data></Cell>\n
        </Row>\n
        <Row>\n
         <Cell ss:StyleID="left"><Data ss:Type="String">за</Data></Cell>\n
         <Cell ss:StyleID="botbor"><Data ss:Type="String">' + STRING(end-date,'99.99.9999') + ' г.' + '</Data></Cell>\n
        </Row>\n
      '.

    IF NOT tUserID MATCHES 'SERV' THEN
      PUT STREAM vvs UNFORMATTED
        '   <Row>\n
              <Cell ss:StyleID="left"><Data ss:Type="String">ФИО</Data></Cell>\n
              <Cell ss:StyleID="botbor"><Data ss:Type="String">' + vUserName + '</Data></Cell>\n
             </Row>\n
        '.

    PUT STREAM vvs UNFORMATTED
      '   <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">наименование подразделения</Data></Cell>\n
            <Cell ss:Index="3" ss:MergeAcross="5" ss:StyleID="botbor"><Data ss:Type="String">' + vUserOtd + '</Data></Cell>\n
           </Row>\n
      '.

    PUT STREAM vvs UNFORMATTED
      '   <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_b"><Data ss:Type="String">' + tShiv + '</Data></Cell>\n
           </Row>\n
           <Row>\n
           </Row>\n
      '.

  END.

    /* бумажные\электронные */
    IF FIRST-OF(tt-op-day.save-type) AND NOT tUserID MATCHES 'SERV' THEN
    	PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="9" ss:StyleID="tab_b"><Data ss:Type="String">Хранятся в ' + (IF tt-op-day.save-type EQ '1' THEN 'бумажном' ELSE 'электронном') + ' виде:</Data></Cell>\n
           </Row>\n
        '.
	
  /* оформляем документы в бумажном виде */
  IF tt-op-day.save-type EQ '1' THEN DO:

  	/* шапка таблицы */
      IF FIRST-OF(tt-op-day.save-type) THEN
      	PUT STREAM vvs UNFORMATTED
          '  <Row ss:Height="25">\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Дебет</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Кредит</Data></Cell>\n
              <Cell ss:MergeAcross="1" ss:StyleID="tab"><Data ss:Type="String">Сумма</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Дата проводки</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Документ</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Номер</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Дата документа</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Код транз.</Data></Cell>\n
              <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Транз.</Data></Cell>\n
             </Row>\n
             <Row>\n
              <Cell ss:Index="3" ss:StyleID="tab"><Data ss:Type="String">рубли</Data></Cell>\n
              <Cell ss:StyleID="tab"><Data ss:Type="String">ин.валюта</Data></Cell>\n
             </Row>\n
          '.

  	/* балансовые\внебалансовые */
  	IF FIRST-OF(tt-op-day.acct-cat) THEN
  		PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="9" ss:StyleID="tab_b"><Data ss:Type="String">' + (IF tt-op-day.acct-cat = 'b' THEN 'балансовые' ELSE 'внебалансовые') + '</Data></Cell>\n
           </Row>\n
        '.	
  	
  	/* документ пользователя */
  	PUT STREAM vvs UNFORMATTED
      '  <Row>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + ENTRY(1,tt-op-day.acct-db,' ') + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + ENTRY(1,tt-op-day.acct-cr,' ') + '</Data></Cell>\n
          <Cell ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(tt-op-day.amt-rub,'>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
          <Cell ss:StyleID="tab_c"><Data ss:Type="String">' + (IF tt-op-day.currency EQ '' THEN '' ELSE (TRIM(STRING(tt-op-day.amt-cur,'>>>,>>>,>>>,>>9.99')))) + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + STRING(tt-op-day.op-date,"99.99.99") + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + tt-op-day.doc-type + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + tt-op-day.doc-num + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF tt-op-day.doc-date NE ? THEN STRING(tt-op-day.doc-date,"99.99.99") ELSE '') + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + tt-op-day.op-kind + '</Data></Cell>\n
          <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + STRING(tt-op-day.op-trans) + '</Data></Cell>\n
         </Row>\n
      '.
  	
  	/* считаем итоги */
  	ACCUMULATE tt-op-day.amt-rub (TOTAL BY tt-op-day.currency).
  	ACCUMULATE tt-op-day.amt-cur (TOTAL BY tt-op-day.currency).
  	ACCUMULATE tt-op-day.amt-rub (TOTAL BY tt-op-day.acct-cat).

  	/* итог по одной из валют */
  	IF LAST-OF(tt-op-day.currency) THEN
  		PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_l"><Data ss:Type="String">Итого</Data></Cell>\n
            <Cell ss:StyleID="tab_c"><Data ss:Type="String">' + (TRIM(STRING(ACCUM TOTAL BY tt-op-day.currency tt-op-day.amt-rub,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
            <Cell ss:StyleID="tab_c"><Data ss:Type="String">' + (IF tt-op-day.currency EQ '' THEN '' ELSE (TRIM(STRING(ACCUM TOTAL BY tt-op-day.currency tt-op-day.amt-cur,'>>>,>>>,>>>,>>9.99')))) + '</Data></Cell>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
           </Row>\n
        '.

  	/*общий итог*/
  	IF LAST-OF(tt-op-day.acct-cat) THEN
  		PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_l"><Data ss:Type="String">Итог общий</Data></Cell>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_c"><Data ss:Type="String">' + (TRIM(STRING(ACCUM TOTAL BY tt-op-day.acct-cat tt-op-day.amt-rub,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
            <Cell ss:StyleID="tab_c"/>\n
           </Row>\n
        '.

  END. /* IF tt-op-day.save-type EQ '1' */

  /* электронный вид */
  ELSE DO:

    IF FIRST-OF(tt-op-day.save-type) THEN DO:
      PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="3" ss:StyleID="tab_l"/>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Балансовые</Data></Cell>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Внебалансовые</Data></Cell>\n
           </Row>\n
        '.  
      ASSIGN
        vSum-b     = 0.00
        vSum-o     = 0.00
        vSum-b-val = 0.00
        vSum-o-val = 0.00.

    END.

    /* рассчитываем итоги для эл.хранения */
    IF tt-op-day.acct-cat EQ 'b' THEN DO:
      vSum-b = vSum-b + tt-op-day.amt-rub.
      IF tt-op-day.currency NE '' THEN
        vSum-b-val = vSum-b-val + tt-op-day.amt-rub.
    END.
    ELSE DO:
      vSum-o = vSum-o + tt-op-day.amt-rub.
      IF tt-op-day.currency NE '' THEN
        vSum-o-val = vSum-o-val + tt-op-day.amt-rub.
    END. 

    /*заполняем итоги для эл.хранения*/
    IF LAST-OF(tt-op-day.save-type) THEN
      PUT STREAM vvs UNFORMATTED
        '  <Row>\n
            <Cell ss:MergeAcross="3" ss:StyleID="tab_l"><Data ss:Type="String">хранятся в электронном виде</Data></Cell>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(vSum-b,'>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(vSum-o,'>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
           </Row>\n
           <Row>\n
            <Cell ss:MergeAcross="3" ss:StyleID="tab_l"><Data ss:Type="String">в т.ч. по операциям с иностранной валютой</Data></Cell>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(vSum-b-val,'>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(vSum-o-val,'>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
           </Row>\n
        '.  

  END. 

	/* разделитель*/
	IF LAST-OF(tt-op-day.save-type) THEN
		PUT STREAM vvs UNFORMATTED
      '  <Row>\n
         </Row>\n
      '.

  IF LAST-OF(tt-op-day.hozdoc) THEN DO:

    /*подпись исполнителя и контроллера*/
    PUT STREAM vvs UNFORMATTED
      '  <Row>\n
            <Cell ss:MergeAcross="1"><Data ss:Type="String">Исполнитель:</Data></Cell>\n
           </Row>\n
            <Row>\n
            <Cell ss:StyleID="botbor" ss:MergeAcross="1"><Data ss:Type="String">' + vUserName + '</Data></Cell>\n
            <Cell/>
            <Cell ss:StyleID="botbor"/>
           </Row>\n
           <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">ФИО</Data></Cell>\n
            <Cell ss:Index="4" ss:StyleID="center"><Data ss:Type="String">подпись</Data></Cell>\n
           </Row>\n
           <Row>\n
           </Row>\n
             <Row>\n
            <Cell ss:MergeAcross="1"><Data ss:Type="String">Контролер:</Data></Cell>\n
           </Row>\n
           <Row>\n
            <Cell ss:StyleID="botbor" ss:MergeAcross="1"/>\n
            <Cell/>
            <Cell ss:StyleID="botbor"/>
           </Row>\n
           <Row>\n
            <Cell ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">ФИО</Data></Cell>\n
            <Cell ss:Index="4" ss:StyleID="center"><Data ss:Type="String">подпись</Data></Cell>\n
           </Row>\n
          </Table>\n
          <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">\n
           <FitToPage/>\n
           <Print>\n
            <FitHeight>0</FitHeight>\n
           </Print>\n
          </WorksheetOptions>\n
         </Worksheet>\n
      '.

  END.

END. /* for each tt-op-day */

/* при отсутствии проведенных проводок у пользователя */
IF tNull THEN
    PUT STREAM vvs UNFORMATTED
      '   <Worksheet ss:Name="Проводки отсутствуют">\n
          <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="150000" x:FullColumns="1"
           x:FullRows="1" ss:DefaultRowHeight="15">\n
           <Column ss:AutoFitWidth="0" ss:Width="108.85" ss:Span="1"/>\n
           <Column ss:Index="3" ss:AutoFitWidth="0" ss:Width="83.25" ss:Span="1"/>\n
           <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="54"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="55.5"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="54.75"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="59.25"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="57.25"/>\n
           <Column ss:AutoFitWidth="0" ss:Width="63.75"/>\n
           <Row ss:Height="17.25">\n
            <Cell ss:MergeAcross="9" ss:StyleID="head"><Data ss:Type="String">Проведенных проводок не обнаружено</Data></Cell>\n
           </Row>\n
           </Table>\n
           </Worksheet>\n
      '.

PUT STREAM vvs UNFORMATTED
  '
  </Workbook>\n
  '.

OUTPUT STREAM vvs CLOSE.

/*вывод через bispc*/
RUN sndbispc ("file=" + fname + ";class=bq").