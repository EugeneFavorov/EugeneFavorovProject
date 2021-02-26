/*  Copyright: (C) 
     Filename: 
      Comment: процедура поиска счетов с неверно проставленными налоговыми ДР
   Parameters: iPar маски счетов второго порядка
      Created: pda */
 
{globals.i}
{setdest.i}
{tmprecid.def}

DEF NEW SHARED STREAM vvs.

DEFINE INPUT PARAMETER iPar AS CHARACTER.

DEFINE VARIABLE fname     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
DEFINE VARIABLE mRekvList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRekv     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRekvAcct AS CHARACTER NO-UNDO.
/* DEF VAR mDateUL   AS DATE INIT '08/29/2014' NO-UNDO. */
/* DEF VAR mDateFL   AS DATE INIT '07/04/2014' NO-UNDO. */
 
DEFINE TEMP-TABLE tt-hist-ac
   FIELD  modif-acct LIKE acct.acct
   FIELD  user-id    LIKE history.user-id    /*userid сотрудника который внес последнее изменение*/
   FIELD  user-name  LIKE _user._user-name   /*имя сотр.*/
   FIELD  modif-time LIKE history.modif-time 
   FIELD  modif-date-ac AS CHARACTER         /*дата изменения*/
   FIELD  val-now       AS CHARACTER         /*текущее значение ДР*/
   FIELD  val-before    AS CHARACTER         /*предыдущее значение*/
   FIELD  name-rekv     AS CHARACTER         /*имя измененного реквизита*/
   FIELD  open-date-ac  AS CHARACTER         /*дата откр. счета*/
   FIELD  field-val     AS CHARACTER 
.

DEF NEW SHARED STREAM vvs.         

{getdates.i}

mBalList  = ENTRY(1,iPar,';').
mRekvList = 'ДатаСообщЛС'. 

i = 1.
mRekv = ENTRY(i,mRekvList). /*искомый ДР*/

/* FOR EACH tmprecid NO-LOCK, */
/* FIRST acct  */
FOR EACH acct 
   WHERE CAN-DO(mBalList,STRING(acct.bal-acct))
     AND acct.open-date GE beg-date
     AND acct.open-date LE end-date
     /* AND acct.filial-id EQ '0300'  */
NO-LOCK BREAK BY bal-acct:
mRekvAcct = TRIM(GetXattrValueEx('acct',acct.acct + ',' + acct.currency,mRekv,'')).
IF CAN-DO('Не треб*',mRekvAcct) OR mRekvAcct = '' THEN
   FOR EACH  history
       WHERE history.modify EQ 'W'
         AND history.file-name EQ 'acct'
         AND CAN-DO('*' + mRekv + '*',history.field-value)
         AND history.field-ref EQ TRIM(acct.acct + ',' + acct.currency)
   NO-LOCK BREAK BY field-ref
                 BY modif-date 
                 BY modif-time:
      FIND FIRST _user 
          WHERE _user._userid EQ history.user-id
      NO-LOCK NO-ERROR.
      
      DO:
         CREATE tt-hist-ac.
         ASSIGN
            tt-hist-ac.modif-acct    = TRIM(acct.acct + ',' + acct.currency)
            tt-hist-ac.open-date-ac  = STRING(acct.open-date,    '99.99.9999')
            tt-hist-ac.modif-date-ac = STRING(history.modif-date,'99.99.9999')
            tt-hist-ac.field-val     = STRING(REPLACE(history.field-value,'*',''))
            tt-hist-ac.user-id       = history.user-id
            tt-hist-ac.user-name     = IF AVAIL _user THEN _user._user-name ELSE ''
         .
      END.
   END.
END. /* for each acct */

/* {setdest.i &file-name = '111.log'} */
 /*
FOR EACH tt-hist-ac 
NO-LOCK BREAK  BY open-date-ac
               BY modif-acct:
   mRekv = ENTRY(i,mRekvList). /*искомый ДР*/
   IF LOOKUP(mRekv,tt-hist-ac.field-val) NE 0 THEN
   DO:
      ASSIGN
         tt-hist-ac.name-rekv  = mRekv
         tt-hist-ac.val-now    = GetXattrValueEx('acct',
                                                 tt-hist-ac.modif-acct,
                                                 mRekv,'')
         tt-hist-ac.val-before = ENTRY(LOOKUP(mRekv,tt-hist-ac.field-val) + 1,tt-hist-ac.field-val)
      .
      
      PUT UNFORMATTED tt-hist-ac.open-date-ac ';' 
                      tt-hist-ac.modif-acct   ';' 
                      tt-hist-ac.name-rekv    ';' 
                      tt-hist-ac.val-before   ';' 
                      tt-hist-ac.val-now      ';'  
                      tt-hist-ac.user-id      ';' 
                      tt-hist-ac.user-name    ';'
                      tt-hist-ac.modif-date SKIP.
   END. /*do*/
END. /*for each tt-hist-ac*/
*/
/* {preview.i &file-name = '111.log'} */

fname = "./report_DR_acct_" + userid('bisquit') + ".xml".

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
   <Alignment ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="left">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="left_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="right">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="right_b">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
 </Styles>\n
<Worksheet ss:Name="LIST_NAME">\n
  <Table ss:ExpandedColumnCount="9" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="80"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="160"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="80"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="95"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="95"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="75"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Row>\n
    <Cell ss:MergeAcross="4" ss:StyleID="left_b"><Data ss:Type="String">ВЫГРУЗКА ПО СЧЕТАМ: ' + mBalList + '</Data></Cell>\n
   </Row>\n
   <Row>\n
    <Cell ss:MergeAcross="4" ss:StyleID="left"><Data ss:Type="String">' + 'открытых за период с ' + STRING(beg-date,"99.99.9999") + ' по ' + STRING(end-date,"99.99.9999") + '</Data></Cell>\n
   </Row>\n
   <Row>\n
   </Row>\n
   <Row>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">Дата открытия</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">Номер счета</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">ДР</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">Значение до</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">Текущее значение</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">USERID</Data></Cell>\n    
    <Cell ss:StyleID="left_b"><Data ss:Type="String">ФИО</Data></Cell>\n
    <Cell ss:StyleID="left_b"><Data ss:Type="String">Дата изменения</Data></Cell>\n
   </Row>\n
   <Row>\n
'.

FOR EACH tt-hist-ac 
NO-LOCK BREAK  BY open-date-ac
               BY modif-acct
               BY modif-date-ac:
   IF LAST-OF(tt-hist-ac.modif-acct) THEN 
   DO:
      ASSIGN
         tt-hist-ac.name-rekv  = mRekv
         tt-hist-ac.val-before = ENTRY(LOOKUP(mRekv,tt-hist-ac.field-val) + 1,tt-hist-ac.field-val)
         tt-hist-ac.val-now    = GetXattrValueEx('acct',
                                                 tt-hist-ac.modif-acct,
                                                 mRekv,'')
      .
      
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.open-date-ac  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + ENTRY(1,tt-hist-ac.modif-acct) + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.name-rekv     + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.val-before    + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.val-now       + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.user-id       + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.user-name     + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tt-hist-ac.modif-date-ac + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '</Row>\n'.
      PUT STREAM vvs UNFORMATTED '<Row>\n'.
   END.
END. /*for each tt-hist-ac*/
PUT STREAM vvs UNFORMATTED
'</Row>\n
 </Table>\n
 </Worksheet>\n
 </Workbook>\n
'.

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
