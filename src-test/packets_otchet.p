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
{card_acct.i}
{card_open.i}
{card_fost.i}
{card_amt.i}
{card_del.i}
{card_mfr.i}
{card_ui.i}

DEF NEW SHARED STREAM vvs.
DEF VAR fname AS CHAR NO-UNDO.
DEF VAR i AS DECIMAL NO-UNDO.
DEF VAR CorpCard AS CHAR NO-UNDO.
DEF VAR DataProv AS CHAR NO-UNDO.
DEF VAR DataBegin AS DATE NO-UNDO.
                                 
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

IF month(today) EQ 1 OR month(today) EQ 2 OR month(today) EQ 3
THEN DataBegin = date(month(add-interval(today, -3, "month")), 1, year(add-interval(today, -1, "year"))).
ELSE DataBegin = date(month(add-interval(today, -3, "month")), 1, year(today)).

CREATE tempt.
ASSIGN
       tempt.ts1 = "ID ЮрЛица"
       tempt.ts2 = "Наименование ЮрЛица"
       tempt.ts3 = "Проводки c " + string(DataBegin, "99/99/99")
       tempt.ts4 = "Пакет ЮрЛица"
       tempt.ts5 = "Счёт Дебета Проводки"
       tempt.ts6 = "Филиал"
       tempt.ts7 = "Статус"
       tempt.ts8 = "Сумма"
       tempt.ts9 = ""
.



/*i = 1.*/

FOR EACH cust-corp WHERE cust-corp.date-out <= date(today) OR cust-corp.date-out EQ ?
                   /*AND cust-corp.cust-id EQ 14836*/
                   /*AND cust-corp.date-in >= date("01/10/2016")*/
NO-LOCK BY cust-corp.cust-id:

	FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
		             AND tmpsigns.code EQ 'Пакет'
			     AND tmpsigns.surrogate EQ string(cust-corp.cust-id)
			     AND tmpsigns.since <= date(today)
	NO-LOCK NO-ERROR.

	IF NOT AVAIL tmpsigns OR tmpsigns.xattr-value EQ ''
	THEN NEXT.

        CorpCard = "".
        DataProv = "".

	FOR EACH acct WHERE acct.cust-cat EQ 'Ю'
	                AND acct.cust-id EQ cust-corp.cust-id
	                AND acct.close-date EQ ?
	                AND acct.filial-id NE "0400"
	                AND can-do("Расчет", acct.contract)
	NO-LOCK:
 	        IF AVAIL acct THEN DO:

                                       IF GetXattrValueEx("acct", acct.acct + "," + acct.currency, "КорпКарт", "") EQ "Да"
                                       THEN CorpCard = "Да".
                                       ELSE CorpCard = "Нет".

                                       FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                                                           AND can-do("47423*", op-entry.acct-cr)
                                                           AND can-do("АК", op-entry.op-status)
                                                           AND CorpCard EQ "Нет"
                                                           AND op-entry.value-date >= DataBegin
                                       NO-LOCK BY op-entry.op-transaction:
                                       
                                       IF AVAIL op-entry THEN DO:
                                                                  IF op-entry.op-date EQ ?
                                                                  THEN DataProv = string(op-entry.value-date).
                                                                  ELSE DataProv = string(op-entry.op-date).

                                                                  FIND FIRST op WHERE op.op EQ op-entry.op 
                                                                                  AND op.op-kind EQ "1617"
                                                                  NO-LOCK NO-ERROR.
                                                                  IF AVAIL op THEN DO:
                                                                                       CREATE tempt.
                                                                                       ASSIGN
                                                                                       tempt.ts1 = string(cust-corp.cust-id)
                                                                                       tempt.ts2 = string(cust-corp.cust-stat) + " " + string(cust-corp.name-corp)
                                                                                       tempt.ts3 = string(DataProv)
                                                                                       tempt.ts4 = string(tmpsigns.xattr-value)
                                                                                       tempt.ts5 = string(op-entry.acct-db, "x(20)")
                                                                                       tempt.ts6 = string(acct.filial-id)
                                                                                       tempt.ts7 = string(op-entry.op-status)
                                                                                       tempt.ts8 = string(op-entry.amt-rub)
                                                                                       tempt.ts9 = ""
                                                                                       .
                                                                                   END.
                                                              END. 
                                       END.
                                   END.
        END.

/*  i = i + 1.*/
END.



CREATE tempt.
ASSIGN
       tempt.ts1 = ""
       tempt.ts2 = ""
       tempt.ts3 = ""
       tempt.ts4 = ""
       tempt.ts5 = ""
       tempt.ts6 = ""
       tempt.ts7 = ""
       tempt.ts8 = ""
       tempt.ts9 = ""
.



CREATE tempt.
ASSIGN
       tempt.ts1 = "ID ФизЛица"
       tempt.ts2 = "Наименование ФизЛица"
       tempt.ts3 = "Проводки c " + string(DataBegin, "99/99/99")
       tempt.ts4 = "Пакет ФизЛица"
       tempt.ts5 = "Счёт Дебета Проводки"
       tempt.ts6 = "Филиал"
       tempt.ts7 = "Статус"
       tempt.ts8 = "Сумма"
       tempt.ts9 = ""
.



FOR EACH person WHERE person.date-out <= date(today) OR person.date-out EQ ?
                /*AND person.date-in >= date("01/11/2016")*/
NO-LOCK BY person.person-id:

	FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                             AND tmpsigns.code EQ 'Пакет'
			     AND tmpsigns.surrogate EQ string(person.person-id)
			     AND tmpsigns.since <= date(today)
	NO-LOCK NO-ERROR.

	IF NOT AVAIL tmpsigns OR tmpsigns.xattr-value EQ ''
	THEN NEXT.

        CorpCard = "".
        DataProv = "".

	FOR EACH acct WHERE acct.cust-cat EQ 'Ч'
	                AND acct.cust-id EQ person.person-id
                        AND acct.close-date EQ ?
	                AND acct.filial-id NE "0400"
	                AND can-do("Расчет", acct.contract)
	NO-LOCK:
	        IF AVAIL acct THEN DO:
                                       IF GetXattrValueEx("acct", acct.acct + "," + acct.currency, "КорпКарт", "") EQ "Да"
                                       THEN CorpCard = "Да".
                                       ELSE CorpCard = "Нет".

                                       FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                                                           AND can-do("47423*", op-entry.acct-cr)
                                                           AND can-do("АК", op-entry.op-status)
                                                           AND CorpCard EQ "Нет"
                                                           AND op-entry.value-date >= DataBegin

                                       NO-LOCK BY op-entry.op-transaction DESC:
                                       
                                       IF AVAIL op-entry THEN DO:
                                                                  IF op-entry.op-date EQ ?
                                                                  THEN DataProv = string(op-entry.value-date).
                                                                  ELSE DataProv = string(op-entry.op-date).

                                                                  FIND FIRST op WHERE op.op EQ op-entry.op 
                                                                                  AND op.op-kind EQ "1617"
                                                                  NO-LOCK NO-ERROR.
                                                                  IF AVAIL op THEN DO:
                                                                                       CREATE tempt.
                                                                                       ASSIGN
                                                                                       tempt.ts1 = string(person.person-id)
                                                                                       tempt.ts2 = string(person.name-last) + " " + string(person.first-names)
                                                                                       tempt.ts3 = string(DataProv)
                                                                                       tempt.ts4 = string(tmpsigns.xattr-value)
                                                                                       tempt.ts5 = string(op-entry.acct-db, "x(20)")
                                                                                       tempt.ts6 = string(acct.filial-id)
                                                                                       tempt.ts7 = string(op-entry.op-status)
                                                                                       tempt.ts8 = string(op-entry.amt-rub)
                                                                                       tempt.ts9 = ""
                                                                                       .
                                                                                   END.
                                                              END. 
                                       LEAVE.
                                       END.
                                   END.
        END.

/*  i = i + 1.*/
END.



fname = "./packets_otchet_" + userid('bisquit') + ".xml".

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
  <Style ss:ID="center_b">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>\n
  <Style ss:ID="center">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
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
   <Column ss:AutoFitWidth="0" ss:Width="250"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="120"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="130"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="60"/>\n
   <Row>\n
'.



FOR EACH tempt NO-LOCK:
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts1 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="left"><Data ss:Type="String">'   + tempt.ts2 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts3 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts4 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts5 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts6 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts7 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts8 + '</Data></Cell>\n'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="center"><Data ss:Type="String">' + tempt.ts9 + '</Data></Cell>\n'.
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
