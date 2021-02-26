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
DEF VAR fname AS CHAR NO-UNDO.
DEF VAR i AS DECIMAL NO-UNDO.
DEF VAR flag1 AS DECIMAL NO-UNDO.
DEF VAR flag2 AS DECIMAL NO-UNDO.
DEF VAR str1 AS CHAR NO-UNDO.
DEF VAR str2 AS CHAR NO-UNDO.
DEF VAR client_name AS CHAR NO-UNDO.
DEF VAR client_id AS CHAR NO-UNDO.
DEF VAR client_sub AS CHAR NO-UNDO.
DEF VAR client_acct AS CHAR NO-UNDO.
DEF VAR beg-date AS DATE NO-UNDO.
DEF VAR end-date AS DATE NO-UNDO.

DEF BUFFER bacct FOR acct.

DEFINE TEMP-TABLE tempt
 FIELD ts1 AS CHARACTER
 FIELD ts2 AS CHARACTER
 FIELD ts3 AS CHARACTER
 FIELD ts4 AS CHARACTER
 FIELD ts5 AS CHARACTER
 FIELD ts6 AS CHARACTER
 FIELD ts7 AS CHARACTER
 FIELD ts8 AS CHARACTER
 FIELD ts9 AS CHARACTER
 FIELD ts0 AS CHARACTER
.         

DEF BUFFER btempt FOR tempt.



i = 0.

beg-date = today.
end-date = today.   

{getdates.i
&NoInit    = "YES"
}

FOR EACH op-entry WHERE op-entry.acct-cr BEGINS "70601"
                    /*AND op-entry.op EQ 50216265*/
                    AND CAN-DO("!2720213,27202..,27902..,27102..,27402..,27802..,27203..,27903..,27103..,27403..,27803..", SUBSTR(op-entry.acct-cr, 14, 7))
                    AND op-entry.op-date >= beg-date
                    AND op-entry.op-date <= end-date:
    IF AVAILABLE op-entry
       THEN DO:
                client_name = "".
                client_id   = "".
                client_sub  = "".
                client_acct = "".
                FIND FIRST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
                FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
                IF acct.cust-cat EQ "Ч" THEN DO: 
                                                 FIND FIRST person WHERE acct.cust-id EQ person.person-id NO-LOCK NO-ERROR. 
                                                            client_name = string(person.name-last) + " " + string(person.first-names). 
                                                            client_id   = string(person.person-id). 
                                                            client_sub  = string(GetXAttrValueEx("person", string(person.person-id), "Субъект", "")).
                                             END.
                IF acct.cust-cat EQ "Ю" THEN DO: 
                                                 FIND FIRST cust-corp WHERE acct.cust-id EQ cust-corp.cust-id NO-LOCK NO-ERROR. 
                                                            client_name = string(cust-corp.cust-stat) + (IF cust-corp.cust-stat EQ "" THEN "" ELSE " ") + string(cust-corp.name-corp). 
                                                            client_id   = string(cust-corp.cust-id). 
                                                            client_sub  = string(GetXAttrValueEx("cust-corp", string(cust-corp.cust-id), "Субъект", "")). 
                                             END.
                IF acct.cust-cat EQ "В" THEN DO: 
                                                 client_name = string('ПАО "Плюс Банк"'). 
                                                 client_id   = string("0"). 
                                                 client_sub  = string("В"). 
                                             END.

                            FIND FIRST bacct WHERE bacct.cust-cat EQ acct.cust-cat
                                             AND bacct.cust-cat NE "В"
                                             AND bacct.cust-id EQ acct.cust-id
                                             AND ((bacct.close-date = ?) OR (bacct.close-date >= beg-date))
                                             /*AND bacct.currency = acct.currency*/
                                             AND ((bacct.contract = "Расчет") OR (bacct.contract = "ДУ"))
                                             AND bacct.filial-id = acct.filial-id
                                             AND CAN-DO("406..,407..,40802,40807", string(bacct.bal-acct))
                                             NO-LOCK NO-ERROR.
                                                     IF ((AVAIL bacct) AND (GetXAttrValue("acct", bacct.acct + "," + bacct.currency, "КорпКарт") EQ "Да")) THEN NEXT.
                                                     IF ((AVAIL bacct) AND (GetXAttrValue("acct", bacct.acct + "," + bacct.currency, "КорпКарт") NE "Да") AND (bacct.close-date EQ ?)) THEN client_acct = SUBSTR(bacct.acct, 1, 20).
                                                     IF ((AVAIL bacct) AND (GetXAttrValue("acct", bacct.acct + "," + bacct.currency, "КорпКарт") NE "Да") AND (bacct.close-date NE ?)) THEN client_acct = SUBSTR(bacct.acct, 1, 20) + " закрыт " + string(bacct.close-date).
                                                     IF NOT AVAIL bacct THEN client_acct = "нет расчётного счёта".

                flag1 = 0.
                flag2 = 0.
 
                IF     CAN-DO("ФЛП", string(client_sub))
                   AND CAN-DO("40802*,нет расчётного счёта", client_acct)
                   AND CAN-DO("70601", SUBSTR(op-entry.acct-cr, 1, 5))
                   AND CAN-DO("27203,27903,27103,27403,27803", SUBSTR(op-entry.acct-cr, 14, 5))
                   THEN flag1 = 1.
                   ELSE flag1 = 0.

                IF     CAN-DO("!ФЛП,!ФЛ,!ФЛН,*", string(client_sub))
                   AND CAN-DO("406*,407*,40807*,нет расчётного счёта", client_acct)
                   AND CAN-DO("70601", SUBSTR(op-entry.acct-cr, 1, 5))
                   AND CAN-DO("!2720213,27202..,27902..,27102..,27402..,27802..", SUBSTR(op-entry.acct-cr, 14, 7))
                   THEN flag2 = 1.
                   ELSE flag2 = 0.

                IF flag1 = 0 AND flag2 = 0 AND client_sub <> "В"
                   THEN DO:
                            i = i + 1.
                            CREATE tempt.
                            ASSIGN
                                   tempt.ts1 = string(i)
                                   tempt.ts2 = string(op-entry.op-date, "99/99/9999")
                                   tempt.ts3 = string(op.branch-id)
                                   tempt.ts4 = string(client_name)
                                   tempt.ts5 = string(client_sub) + " / " + string(client_id)
                                   tempt.ts6 = string(client_acct)
                                   tempt.ts7 = string(SUBSTR(op-entry.acct-cr, 1, 20))
                                   tempt.ts8 = GetXattrValueEx("_user", string(op.user-id), "ФИОП", string(op.user-id))
                                   tempt.ts9 = string(op.details)
                                   tempt.ts0 = "op = " + string(op.op)

                            .
                        END.
            END.
END.

i = 1.

fname = "./otchet_70601_" + userid('bisquit') + "_" + string(beg-date, "99.99.9999") + "-" + string(end-date, "99.99.9999") + ".xml".

OUTPUT STREAM vvs TO VALUE (fname)

UNBUFFERED  CONVERT  TARGET "UTF-8" SOURCE "IBM866".

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
  <Table ss:ExpandedColumnCount="10" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="30"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="130"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="250"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="200"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="120"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="180"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="410"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Row>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">#</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">ДАТА</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">КОД ПОДРАЗДЕЛЕНИЯ</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">НАИМЕНОВАНИЕ КЛИЕНТА</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">КОД КЛИЕНТА</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">Р/С КЛИЕНТА</Data></Cell>\n    
    <Cell ss:StyleID="left"><Data ss:Type="String">СЧЁТ ДОХОДОВ</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">ПОЛЬЗОВАТЕЛЬ</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">НАИМЕНОВАНИЕ ОПЕРАЦИИ</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">РАЗНОЕ</Data></Cell>\n
   </Row>\n
   <Row>\n
'.



FOR EACH tempt NO-LOCK:
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts1 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts2 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts3 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts4 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts5 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts6 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts7 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts8 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts9 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">' + tempt.ts0 + '</Data></Cell>\n'.
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
