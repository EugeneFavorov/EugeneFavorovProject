/* Выписка с разбивкой по итогам дня */
/* pda */

DEFINE INPUT  PARAMETER iParams AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}
{tmprecid.def}
DEFINE VARIABLE fname AS CHARACTER NO-UNDO.         /* имя создаваемого файла */
DEF NEW SHARED STREAM vvs.

DEFINE VARIABLE mI           AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mAcctDetails AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE mCust        AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mBegDate     AS DATE NO-UNDO.
DEFINE VARIABLE mEndDate     AS DATE NO-UNDO.
DEFINE VARIABLE mToday       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankMFO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrINN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrKPP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpDetails   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDb       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmtCr       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mKPP         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDbAll    AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mAmtCrAll    AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mBalBeg      AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mBalEnd      AS DECIMAL INIT 0.0 NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bacct FOR acct.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
DEFINE BUFFER op FOR op.
DEFINE BUFFER bop FOR op.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER banks FOR banks.
DEFINE BUFFER banks-code FOR banks-code.

{getdates.i}

/* создаем ведомость */
FOR EACH tmprecid,
   FIRST acct
   WHERE
      RECID(acct) EQ tmprecid.id
   NO-LOCK:

fname = "./" + delfilfromacct(acct.acct) + "_" + STRING(beg-date, '99999999') + "_" +  STRING(end-date, '99999999') + ".xml".

/* записываем в файл */
OUTPUT STREAM vvs TO VALUE (fname)
   UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

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
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>\n
   <Borders/>\n
   <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="head">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
   <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="12"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="botbor">\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="center">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
  </Style>\n
  <Style ss:ID="center_b"> 
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="tab">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Top" ss:WrapText="1"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="tab_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="tab_c">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="tab_r">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Top"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="tab_l">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
 </Styles>\n
'.

IF NOT AVAILABLE(Acct) THEN
   MESSAGE "Не найден счет." VIEW-AS ALERT-BOX.
DO: 
   ASSIGN
      mAcct    = delFilFromAcct(acct.acct)
      mBegDate = beg-date
      mEndDate = end-date
      mToday   = {strdate.i TODAY}
   .
   /* IF mBegDate EQ mEndDate THEN
      mBegDate = acct.open-date.
   ELSE */
   IF mBegDate LT acct.open-date THEN
      mBegDate = acct.open-date.

   RUN acct-pos IN h_base (acct.acct,
                           acct.currency,
                           beg-date,
                           end-date, 
                           gop-status
                           ).
   ASSIGN
      mBalBeg   = ABSOLUTE(sh-in-bal)    
      mBalEnd   = ABSOLUTE(sh-bal) 
      mAmtDbAll = ABSOLUTE(sh-db)
      mAmtCrAll = ABSOLUTE(sh-cr)
   .
       
/* Заголовок таблицы */
   PUT STREAM vvs UNFORMATTED
      ' <Worksheet ss:Name="' + mAcct + '">\n
        <Table>\n
         <Column ss:AutoFitWidth="0" ss:Width="36"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="87"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="46"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="48"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="84"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="144"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="144"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="72"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="144"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="87"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="72"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="144"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="108" ss:Span="1"/>\n
         <Column ss:Index="15" ss:AutoFitWidth="0" ss:Width="182"/>\n
         <Row>\n
         </Row>\n
         <Row ss:Height="17.25">\n
          <Cell ss:MergeAcross="14" ss:StyleID="head"><Data ss:Type="String">' + 'Выписка по счету '  
                                                                               + STRING(mAcct) 
                                                                               + ' за период с ' 
                                                                               + STRING(mBegDate,'99.99.9999') + ' г. по '
                                                                               + STRING(mEndDate,'99.99.9999') + ' г.' + '</Data></Cell>\n
         </Row>\n
         <Row>\n
         </Row>\n
         <Row>\n
          <Cell ss:MergeAcross="1" ss:StyleID="Default"><Data ss:Type="String">Таблица 1</Data></Cell>\n
         </Row>\n
         <Row ss:Height="80">\n
          <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">N п/п</Data></Cell>\n
          <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Дата совершения операции (дд.мм.гггг)</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Реквизиты документа, на основании которого была совершена операция по счету (специальному банковскому счету)</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Реквизиты банка плательщика/получателя денежных средств</Data></Cell>\n
          <Cell ss:MergeAcross="3" ss:StyleID="tab"><Data ss:Type="String">Реквизиты плательщика/получателя денежных средств</Data></Cell>\n
          <Cell ss:MergeAcross="1" ss:StyleID="tab"><Data ss:Type="String">Сумма операции по счету (спецальному банковскому счету)</Data></Cell>\n
          <Cell ss:MergeDown="1" ss:StyleID="tab"><Data ss:Type="String">Назначение платежа</Data></Cell>\n
         </Row>\n
         <Row ss:Height="48">\n
          <Cell ss:Index="3" ss:StyleID="tab"><Data ss:Type="String">вид (шифр)</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">номер</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">дата</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">корреспондентский счет</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">наименование</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">БИК</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">наименование/Ф.И.О.</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">ИНН/КИО</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">КПП</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">номер счета(специального банковского счета)</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">по дебету</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">по кредиту</Data></Cell>\n
         </Row>\n
         <Row>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">1</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">2</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">3</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">4</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">5</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">6</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">7</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">8</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">9</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">10</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">11</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">12</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">13</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">14</Data></Cell>\n
          <Cell ss:StyleID="tab"><Data ss:Type="String">15</Data></Cell>\n
         </Row>\n
      '.
   FOR EACH op-entry
    WHERE op-entry.op-date GE mBegDate 
      AND op-entry.op-date LE mEndDate  
      AND (op-entry.acct-db EQ acct.acct
         OR op-entry.acct-cr EQ acct.acct)
   NO-LOCK, 
    FIRST op
       WHERE op.op EQ op-entry.op
         AND op.op-date GE mBegDate 
         AND op.op-date LE mEndDate  
       AND op.op-status GE chr(251)
    NO-LOCK
    BREAK by op.op-date:
      IF op-entry.acct-db EQ acct.acct THEN
      DO:
         mCorrAcct = op-entry.acct-cr. 
         IF mCorrAcct EQ ? THEN
         DO:
         FOR EACH bop-entry
            WHERE bop-entry.op EQ op-entry.op
            AND bop-entry.acct-cr NE ?
            NO-LOCK:
         LEAVE.
         END.
         IF AVAILABLE(bop-entry) THEN
            mCorrAcct = bop-entry.acct-cr. 
         END.
         FIND FIRST bacct 
            WHERE bacct.acct EQ mCorrAcct
         NO-LOCK NO-ERROR.
         ASSIGN 
            mAmtDb = op-entry.amt-rub
            mAmtCr = 0.0
         .
      END.   
      ELSE
      DO:
         mCorrAcct = op-entry.acct-db. 
         IF mCorrAcct EQ ? THEN
         DO:
         FOR EACH bop-entry
            WHERE bop-entry.op EQ op-entry.op
            AND bop-entry.acct-db NE ?
            NO-LOCK:
         LEAVE.
         END.
         IF AVAILABLE(bop-entry) THEN
            mCorrAcct = bop-entry.acct-db.
         END.     
         FIND FIRST bacct 
            WHERE bacct.acct EQ mCorrAcct
         NO-LOCK NO-ERROR.
         ASSIGN 
            mAmtDb = 0.0
            mAmtCr = op-entry.amt-rub
         .
      END.
        
    FIND FIRST op-bank
       WHERE op-bank.op EQ op.op
    NO-LOCK NO-ERROR.
    IF AVAILABLE(op-bank) THEN
    DO:
       mKPP = IF CAN-DO("30*", op-entry.acct-cr) THEN "Kpp-rec" ELSE "Kpp-send".   
       ASSIGN
          mBankMFO  = op-bank.bank-code
          mBankName = op-bank.bank-name
          mBankAcct = op-bank.corr-acct
          mCorrName = REPLACE(REPLACE(REPLACE(op.name-ben, ";", ":"), ";", ":"), chr(10), "")
          mCorrINN  = op.inn
          mCorrKPP  = GetXAttrValueEx("op",
                                      STRING(op.op),
                                      mKPP,
                                      " ")
          mCorrAcct = op.ben-acct
       .
    END.    
    ELSE
    DO:
      ASSIGN
         mBankName = FGetSetting ("Банк", ?, " ")
         mBankMFO  = FGetSetting ("БанкМФО", ?, "0")
         mBankAcct = FGetSetting ("КорСч", ?, "0")
      .
      RUN GetCustName IN h_base (bacct.cust-cat,
                                 bacct.cust-id,
                                 ?,
                                 OUTPUT mAcctDetails[1],
                                 OUTPUT mAcctDetails[2],
                                 INPUT-OUTPUT mAcctDetails[3]).
      mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
      IF bacct.cust-cat NE "В" THEN
      DO:
         ASSIGN
            mCorrName = mAcctDetails[1]
            mCorrINN  = mAcctDetails[3]
            mCorrAcct = DelFilFromAcct(bacct.acct)
         .
      END.
      ELSE    
         ASSIGN
            mCorrName = FGetSetting ("Банк", ?, " ")
            mCorrINN  = FGetSetting ("ИНН", ?, " ")
            mCorrAcct = DelFilFromAcct(bacct.acct)
         .
   END.

      ASSIGN          
         mI = mI + 1
         mOpDetails = REPLACE(REPLACE(REPLACE(op.details, CHR(10), ""), CHR(60), ""), CHR(62), "")
      .

      PUT STREAM vvs UNFORMATTED
         ' <Row ss:AutoFitHeight="1">\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + STRING(mI) + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF op.op-date  NE ? THEN STRING(op.op-date,"99.99.9999") ELSE '------------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF op.doc-type NE ? THEN op.doc-type ELSE '------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF op.doc-num  NE ? THEN op.doc-num  ELSE '------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF op.doc-date NE ? THEN STRING(op.doc-date,"99.99.9999") ELSE '------------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mBankAcct   NE ? THEN mBankAcct ELSE '--------------------')+ '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mBankName   NE ? THEN mBankName ELSE '--------------------')+ '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mBankMFO    NE ? THEN mBankMFO  ELSE '----------')+ '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mCorrName   NE ? THEN mCorrName ELSE '--------------------')+ '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mCorrINN    NE ? THEN mCorrINN  ELSE '------------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mCorrKPP    NE ? THEN mCorrKPP  ELSE '---------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mCorrAcct   NE ? THEN mCorrAcct ELSE ' ')+ '</Data></Cell>\n
            <Cell ss:StyleID="tab_r"><Data ss:Type="String">' + (IF mAmtDb NE 0.0 THEN STRING(mAmtDb,'>>>,>>>,>>>,>>9.99') ELSE '---------------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_r"><Data ss:Type="String">' + (IF mAmtCr NE 0.0 THEN STRING(mAmtCr,'>>>,>>>,>>>,>>9.99') ELSE '---------------') + '</Data></Cell>\n
            <Cell ss:StyleID="tab_l"><Data ss:Type="String">' + (IF mOpDetails  NE ? THEN mOpDetails ELSE '--------------------------')+ '</Data></Cell>\n
           </Row>\n
         '. 
      /* итоги по каждому дню */  
      IF LAST-OF(op.op-date) THEN
      DO:
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 op.op-date,
                                 op.op-date, "√").
      PUT STREAM vvs UNFORMATTED
         ' <Row>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_b"><Data ss:Type="String">Входящий остаток пассив</Data></Cell>\n
            <Cell ss:MergeAcross="5" ss:StyleID="tab_r"><Data ss:Type="String">' + STRING(ABSOLUTE(sh-in-bal), '>>>,>>>,>>>,>>9.99') + '</Data></Cell>\n
           </Row>\n
           <Row>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_b"><Data ss:Type="String">Обороты</Data></Cell>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_r"><Data ss:Type="String">Дебет:</Data></Cell>\n
            <Cell ss:StyleID="tab_r"><Data ss:Type="String">' + STRING(ABSOLUTE(sh-db), '>>>,>>>,>>>,>>9.99') + '</Data></Cell>\n
            <Cell ss:MergeAcross="1" ss:StyleID="tab_r"><Data ss:Type="String">Кредит:</Data></Cell>\n
            <Cell ss:StyleID="tab_r"><Data ss:Type="String">' + STRING(ABSOLUTE(sh-cr), '>>>,>>>,>>>,>>9.99') + '</Data></Cell>\n
           </Row>\n
           <Row>\n
            <Cell ss:MergeAcross="2" ss:StyleID="tab_b"><Data ss:Type="String">Исходящий остаток пассив</Data></Cell>\n
            <Cell ss:MergeAcross="5" ss:StyleID="tab_r"><Data ss:Type="String">' + STRING(ABSOLUTE(sh-bal), '>>>,>>>,>>>,>>9.99') + '</Data></Cell>\n
           </Row>\n
         '.
      END.
   END. /* for each op-entry */
END. /* do */

/* таблица с итогами */
PUT STREAM vvs UNFORMATTED
   ' <Row>\n
     </Row>\n
     <Row>\n
     </Row>\n
     <Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="Default"><Data ss:Type="String">Таблица 2</Data></Cell>\n
     </Row>\n
     <Row ss:Height="33">\n
      <Cell ss:MergeAcross="4" ss:StyleID="tab"><Data ss:Type="String">Остаток по стчету (специальному банковскому счету) на начало периода</Data></Cell>\n
      <Cell ss:MergeAcross="1" ss:StyleID="tab"><Data ss:Type="String">Сумма по дебету счета (специального банковского счета) за период</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Сумма по кредиту счета (специального банковского счета) за период</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">Остаток по счету (специальному банковскому счету) за период</Data></Cell>\n
     </Row>\n 
     <Row>\n
      <Cell ss:MergeAcross="4" ss:StyleID="tab"><Data ss:Type="String">1</Data></Cell>\n
      <Cell ss:MergeAcross="1" ss:StyleID="tab"><Data ss:Type="String">2</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">3</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab"><Data ss:Type="String">4</Data></Cell>\n
     </Row>\n
     <Row>\n
      <Cell ss:MergeAcross="4" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(mBalBeg, '>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
      <Cell ss:MergeAcross="1" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(mAmtDbAll, '>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(mAmtCrAll, '>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="tab_c"><Data ss:Type="String">' + TRIM(STRING(mBalEnd, '>>>,>>>,>>>,>>9.99')) + '</Data></Cell>\n
     </Row>\n
   '.
/* подпись */
PUT STREAM vvs UNFORMATTED
   ' <Row>\n
     </Row>\n
     <Row>\n
     </Row>\n
     <Row ss:AutoFitHeight="1">\n
      <Cell ss:Index="2" ss:MergeAcross="1" ss:MergeDown="1" ss:StyleID="Default"><Data ss:Type="String">Представитель банка</Data></Cell>\n
     </Row>\n
     <Row>\n
     </Row>\n
     <Row>\n
      <Cell ss:Index="4" ss:MergeAcross="2" ss:StyleID="center_b"><Data ss:Type="String">Начальник Отдела сопровождения РКО</Data></Cell>\n
      <Cell ss:Index="8" ss:MergeAcross="1" ss:StyleID="center_b"><Data ss:Type="String">Герман Юлия Сергеевна</Data></Cell>\n
      <Cell ss:Index="11" ss:MergeAcross="1" ss:StyleID="center_b"><Data ss:Type="String"></Data></Cell>\n
      <Cell ss:Index="14" ss:MergeAcross="1" ss:StyleID="center_b"><Data ss:Type="String">' + STRING(mToday) + '</Data></Cell>\n
     </Row>\n
     <Row>\n
      <Cell ss:Index="4" ss:MergeAcross="2" ss:StyleID="center"><Data ss:Type="String">(должность)</Data></Cell>\n
      <Cell ss:Index="8" ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">(Ф.И.О.)</Data></Cell>\n
      <Cell ss:Index="11" ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">(подпись)</Data></Cell>\n
      <Cell ss:Index="14" ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">(дата)</Data></Cell>\n
     </Row>\n
     <Row>\n
      <Cell ss:Index="11" ss:StyleID="Default"><Data ss:Type="String">М.П.</Data></Cell>\n
     </Row>\n
   '.

PUT STREAM vvs UNFORMATTED
   ' </Table>\n
        <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">\n
         <FitToPage/>\n
         <Print>\n
          <FitHeight>0</FitHeight>\n
         </Print>\n
        </WorksheetOptions>\n
       </Worksheet>\n
      </Workbook>\n
   '.

OUTPUT STREAM vvs CLOSE.    

/* RUN sndbispc ("file=" + fname + ";class=bq"). */

END.

MESSAGE "Выписки сформированы!" SKIP "Расположены в homes (192.168.171.155)" VIEW-AS ALERT-BOX.
{intrface.del}