/* 
   fev
   Поиск оборотов по счету без комиссий для ежемесячных комиссий
   Используется транзакциями _16m*
*/

{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{intrface.get tmess}
{tmprecid.def}

DEF VAR iAcct AS CHARACTER NO-UNDO.
DEF VAR date1 AS Date NO-UNDO format "99/99/9999".
DEF VAR date2 AS Date NO-UNDO format "99/99/9999".
DEF VAR date3 AS Date NO-UNDO format "99/99/9999".

DEF VAR name   AS CHARACTER NO-UNDO.
DEF VAR fname  AS CHAR NO-UNDO.
DEF VAR summa1 AS DEC NO-UNDO.
DEF VAR summa2 AS DEC NO-UNDO.
DEF VAR summa3 AS DEC NO-UNDO.

DEF new shared stream vvs.

fname = "./_last_comission_" + userid('bisquit') + ".xml".
date1 = date(month(today),1,year(today)).
date2 = date(today - 1).
date3 = date(today).

PAUSE 0.

FORM
date1 label "Начало" 
date2 label "Конец"
date3 label "Сегодня"

with frame www overlay side-labels 3 col centered row 10 title
color bright-white
"[ " + "Введите даты " + "]" width 34.

DO on endkey undo, return on error undo, retry with frame www:
DISPLAY date1.
DISPLAY date2.
DISPLAY date3.
SET 
 date1
 date2
 date3
editing:
readkey.
apply lastkey.
END.
END.                                                                                        
DO on endkey undo, leave on error undo, leave with frame prn:

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
  <Author>Фаворов Евгений Владимирович</Author>
  <LastAuthor>Фаворов Евгений Владимирович</LastAuthor>
  <Created>2016-10-30T10:43:16Z</Created>
  <LastSaved>2016-10-21T05:58:29Z</LastSaved>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
  <Colors>
   <Color>
    <Index>14</Index>
    <RGB>#333399</RGB>
   </Color>
   <Color>
    <Index>15</Index>
    <RGB>#333333</RGB>
   </Color>
   <Color>
    <Index>52</Index>
    <RGB>#A9A9A9</RGB>
   </Color>
   <Color>
    <Index>53</Index>
    <RGB>#201F35</RGB>
   </Color>
   <Color>
    <Index>54</Index>
    <RGB>#C0C0C0</RGB>
   </Color>
   <Color>
    <Index>55</Index>
    <RGB>#808080</RGB>
   </Color>
  </Colors>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>9855</WindowHeight>
  <WindowWidth>17100</WindowWidth>
  <WindowTopX>480</WindowTopX>
  <WindowTopY>105</WindowTopY>
  <RefModeR1C1/>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Arial"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center"/>
  </Style>
   <Style ss:ID="s64">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
  </Style>
   <Style ss:ID="s69">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Tahoma" x:CharSet="204" x:Family="Swiss" ss:Size="12"
    ss:Bold="1"/>
   <Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="@"/>
   <Protection/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Sheet">
  <Table ss:ExpandedColumnCount="3" ss:ExpandedRowCount="5000" x:FullColumns="1"
   x:FullRows="1">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="81"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="450"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="180"/>
   <Row ss:AutoFitHeight="0" ss:Height="24">
    <Cell ss:StyleID="s63"><Data ss:Type="String">ID Клиента</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Наименование Клиента</Data></Cell>
    <Cell ss:StyleID="s63"><Data ss:Type="String">Счёт Клиента</Data></Cell>
   </Row>
'.



for each acct where acct.filial-id eq shFilial
                and ((acct.bal-acct EQ 40701) OR (acct.bal-acct EQ 40702) OR (acct.bal-acct EQ 40703) OR (acct.bal-acct EQ 40802) OR (acct.bal-acct EQ 40807))
              no-lock:



summa1 = 0.
for each op-entry where /*op-entry.filial-id eq shFilial
                    and*/ can-do('√*', op-entry.op-status)
		    and op-entry.op-date ge date1
		    and op-entry.op-date le date2
		    and (op-entry.acct-db eq acct.acct or op-entry.acct-cr eq acct.acct)
	            and can-do('!70601*,!60322*,!47423*,*', op-entry.acct-cr)
		  no-lock,
		first op of op-entry where can-do('!А*,*', op.op-status)
		no-lock:
		summa1 = summa1 + op-entry.amt-rub.   /* сумма проводок за месяц */
end.



if summa1 = 0
then do:
        summa2 = 0.

        /*message string(summa1) " " string(acct.acct) view-as alert-box.*/
        for each op-entry where /*op-entry.filial-id eq shFilial
                            and*/ can-do('*', op-entry.op-status)
         		    and op-entry.op-date eq date3
	                    and (op-entry.acct-db eq acct.acct or op-entry.acct-cr eq acct.acct)
         	            and can-do('!70601*,!60322*,!47423*,*', op-entry.acct-cr)
			  no-lock,
		first op of op-entry where can-do('!А*,*', op.op-status)
		no-lock:
		summa2 = summa2 + op-entry.amt-rub.   /* сумма проводок за последний день */
        end.



        RUN GetName (INPUT acct.cust-cat, INPUT acct.cust-id, OUTPUT name).



        summa3 = 0.

        for each op-entry where /*op-entry.filial-id eq shFilial
                            and*/ can-do('*', op-entry.op-status)
         		    and op-entry.op-date eq date3
	                    and op-entry.acct-db eq acct.acct
         	            and can-do('47423*', op-entry.acct-cr)
			  no-lock,
		first op of op-entry where can-do('!А*,*', op.op-status)
                                       and op.doc-type = "017"
                                       and can-do("16m_*", op.op-kind)
		no-lock:
		summa3 = summa3 + op-entry.amt-rub.   /* сумма комиссионных проводок */
        end.



        if summa2 > 0
           and summa3 = 0
           and acct.cust-cat = 'Ч'
           and GetXAttrValue("person", string(acct.cust-id), "Пакет") EQ ""
        then do:
           /*message string(name) " " string(summa1) " " string(summa2) view-as alert-box.*/
           put stream vvs unformatted '<Row>'.
           put stream vvs unformatted '<Cell ss:StyleID="s69"><Data ss:Type="String">' + string(acct.cust-id) + '</Data></Cell>\n'.
           put stream vvs unformatted '<Cell><Data ss:Type="String">' + string(name) + '</Data></Cell>\n'.
           put stream vvs unformatted '<Cell ss:StyleID="s64"><Data ss:Type="String">' + string(acct.acct) + '</Data></Cell>\n'.
           put stream vvs unformatted '</Row>'.
        end.



        if summa2 > 0 
           and summa3 = 0
           and acct.cust-cat = 'Ю'
           and GetXAttrValue("cust-corp", string(acct.cust-id), "Пакет") EQ ""
        then do:
           /*message string(name) " " string(summa1) " " string(summa2) view-as alert-box.*/
           put stream vvs unformatted '<Row>'.
           put stream vvs unformatted '<Cell ss:StyleID="s69"><Data ss:Type="String">' + string(acct.cust-id) + '</Data></Cell>\n'.
           put stream vvs unformatted '<Cell><Data ss:Type="String">' + string(name) + '</Data></Cell>\n'.
           put stream vvs unformatted '<Cell ss:StyleID="s64"><Data ss:Type="String">' + string(acct.acct) + '</Data></Cell>\n'.
           put stream vvs unformatted '</Row>'.
        end.
end.
end.



put stream vvs unformatted 
'
  </Table>
  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">
   <Selected/>
   <ProtectObjects>False</ProtectObjects>
   <ProtectScenarios>False</ProtectScenarios>
  </WorksheetOptions>
 </Worksheet>
</Workbook>
'.



output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").



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
END.