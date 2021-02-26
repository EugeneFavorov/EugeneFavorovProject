/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:  
      Created: fev
*/

{globals.i}
{setdest.i}
{tmprecid.def}
{chkacces.i}
{sh-defs.i}
{card.i}
{clg-cr.err}          /* Ошибки при открытии договоров ПК */
{intrface.get count}  /* Чтобы работал GetCounterNextValue */
{intrface.get xclass} /* Чтобы работало получение начального значения реквизита */
{intrface.get corr}
{intrface.get date}
{intrface.get jloan}
{intrface.get instrum}
{intrface.get db2l}
{intrface.get tmess}
{intrface.get dpspc}
{intrface.get acct}
{intrface.get trnsl}
{intrface.get loan}
{intrface.get refer}    /* Библиотека службы справочников. */
{intrface.get tmcod}
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

DEF NEW SHARED STREAM vvs.
DEF VAR fname  AS CHAR NO-UNDO.
DEF VAR i AS   DECIMAL NO-UNDO.
DEF VAR str1   AS CHAR NO-UNDO.
DEF VAR str2   AS CHAR NO-UNDO.
DEF VAR inn    AS CHAR NO-UNDO.
DEF VAR date1  AS DATE FORMAT 99/99/9999 NO-UNDO.

DEFINE TEMP-TABLE tempt
 FIELD ts1  AS CHARACTER
 FIELD ts2  AS CHARACTER
 FIELD ts3  AS CHARACTER
 FIELD ts4  AS CHARACTER
 FIELD ts5  AS CHARACTER
 FIELD ts6  AS CHARACTER
 FIELD ts7  AS CHARACTER
 FIELD ts8  AS CHARACTER
 FIELD ts9  AS CHARACTER
 FIELD ts10 AS CHARACTER
 FIELD ts11 AS CHARACTER
 FIELD ts12 AS CHARACTER
 FIELD ts13 AS CHARACTER
.         

DEF BUFFER btempt FOR tempt.
DEF BUFFER bacct1 FOR acct.
DEF BUFFER bacct2 FOR acct.

PAUSE 0.

FORM
date1 label "Введите Дату"

with frame www overlay side-labels 1 col centered row 6 title
color bright-white
"[ " + "ОТЧЁТ DETAILS " + "]" width 34.

DO on endkey undo, return on error undo, retry with frame www:
DISPLAY date1.
SET 
 date1
editing:
readkey.
apply lastkey.
END.
END.                                                                                        
DO on endkey undo, leave on error undo, leave with frame prn:



i = 1.

for each op where op.op-date = date1
              and can-do   ("*klb*,*bum*", op.op-kind)
              and can-do ("!401*,!403*,*", op.ben-acct)
              and can-do    ("01КЛ,01БУМ", op.doc-type)
              and can-do         ("!А*,*", op.op-status)
              and op.order-pay           = "5"
              /*and op.filial-id = "0300"*/
              /*and op.doc-num = "381"*/
              /*and op.op = 120061583*/
    no-lock:
             find first op-entry where op-entry.op = op.op no-lock no-error.
             find first bacct1 where bacct1.acct = op-entry.acct-db no-lock no-error.
             inn = "".
             if bacct1.cust-cat = "Ч" then do: find first person    where person.person-id  = bacct1.cust-id no-lock no-error. inn = person.inn. end.
             if bacct1.cust-cat = "Ю" then do: find first cust-corp where cust-corp.cust-id = bacct1.cust-id no-lock no-error. inn = cust-corp.inn. end.
             find first bacct2 where bacct2.acct = op-entry.acct-cr no-lock no-error.
             find first history where history.field-ref     = string(op.op)
                                  and history.file-name     = "op"
                                  and history.modif-date    >= op.doc-date
                                  and history.modif-date    <= today
                                  and history.modify        EQ "W"
                                  and can-do("!SERV*,*",    history.user-id)
                                  and can-do("*КЛБ*,*БУМ*", history.field-value)
                                  /*and history.modif-time =  23396*/
                  no-lock no-error.
             find first _user where _user._userid = history.user-id no-lock no-error.
    if avail op-entry
         and bacct1.cust-id <> bacct2.cust-id
         and inn <> op.inn
    THEN
      DO:
        CREATE tempt.
          ASSIGN
            tempt.ts1  = if op.op-date = ? then "" else string(op.doc-date, "99/99/9999")
            tempt.ts2  = if op.branch-id = "" then "" else string(op.branch-id)
            tempt.ts3  = if string(GetXattrValueEx("acct", string(bacct1.acct) + "," + string(bacct1.currency), "groupOABS", "")) = "" then "" else string(GetXattrValueEx("acct", string(bacct1.acct) + "," + string(bacct1.currency), "groupOABS", ""))
            tempt.ts4  = if op.doc-num = "" then "" else string(op.doc-num)
            tempt.ts5  = if op.op-status = "" then "" else string(op.op-status)
            tempt.ts6  = if op.doc-type = "" then "" else string(op.doc-type)
            tempt.ts7  = if op-entry.acct-db = "" then "" else string(substr(op-entry.acct-db, 1, 20))
            tempt.ts8  = if op-entry.acct-cr = "" then "" else string(substr(op-entry.acct-cr, 1, 20))
            tempt.ts9  = if string(op-entry.amt-rub) = "" then "" else string(op-entry.amt-rub, ">>>>>>>>9.99")
            tempt.ts10 = if op.order-pay = "" then "" else string(op.order-pay)
            tempt.ts11 = if op.details = "" then "" else string(op.details)
            tempt.ts12 = if op.ben-acct = "" then "" else string(op.ben-acct)
            tempt.ts13 = if not avail history then "" else string(history.user-id) + " (" + string(_user._user-name) + ")"
          .
      END.
  i = i + 1.
END.

i = 1.

fname = "./otchet_details_" + userid('bisquit') + ".xml".

OUTPUT STREAM vvs TO VALUE (fname)

UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

PUT STREAM vvs UNFORMATTED
'
<?xml version="1.0"?>\n
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
  <Style ss:ID="center">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="center_b">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
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
  <Table ss:ExpandedColumnCount="13" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="120"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="120"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="70"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="400"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="120"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="140"/>\n
   <Row>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">DOC-DATE</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">BRANCH-ID</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">GROUPS</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">DOC-NUM</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">OP-STATUS</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">DOC-TYPE</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">ACCT-DB</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">ACCT-CR</Data></Cell>\n    
    <Cell ss:StyleID="left"><Data ss:Type="String">AMT-RUB</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">ORDER-PAY</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">DETAILS</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">BEN-ACCT</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">HISTORY-USER</Data></Cell>\n
   </Row>\n
   <Row>\n
'.


FOR EACH tempt NO-LOCK:
tempt.ts11 = replace(replace(tempt.ts11,'>',''),'<','').
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts1  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts2  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts3  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts4  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts5  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts6  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts7  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts8  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts9  + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts10 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + replace(replace(tempt.ts11,chr(13),''),chr(10),'') + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts12 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts13 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '</Row>\n'.
      PUT STREAM vvs UNFORMATTED '<Row>\n'.
END.



PUT STREAM vvs UNFORMATTED
'</Row>\n
 </Table>\n
 </Worksheet>\n
 </Workbook>\n
'.



OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").

END.