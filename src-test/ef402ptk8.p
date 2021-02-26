/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ef402ptk8.p
      Comment: Выгрузка данных по форме 402 в Excel
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
{card_comm.i}
{card_amt.i}
{card_del.i}
{card_mfr.i}
{card_ui.i}

DEF NEW SHARED STREAM vvs.
DEF VAR fname AS CHAR NO-UNDO.
DEF VAR i AS DECIMAL NO-UNDO.
DEF VAR str1 AS CHAR NO-UNDO.
DEF VAR str2 AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER iDataID AS INT64 NO-UNDO.

DEFINE TEMP-TABLE tempt
 FIELD ts00 AS CHARACTER
 FIELD ts0 AS CHARACTER
 FIELD ts1 AS CHARACTER
 FIELD ts2 AS CHARACTER
 FIELD ts3 AS CHARACTER
 FIELD ts4 AS CHARACTER
 FIELD ts5 AS CHARACTER
 FIELD ts6 AS CHARACTER
 FIELD ts7 AS CHARACTER
 FIELD ts8 AS CHARACTER
 FIELD ts9 AS CHARACTER
 FIELD ts10 AS CHARACTER
 FIELD ts11 AS CHARACTER
.         

DEF BUFFER btempt FOR tempt.



i = 1.

FOR EACH dataline WHERE dataline.data-id = iDataID NO-LOCK by dataline.sym1:
  IF AVAILABLE dataline
    THEN
      DO:
        CREATE tempt.
          ASSIGN
            tempt.ts0 = string(i)
            tempt.ts1 = string(substring(dataline.sym1,7,2)) + "." + string(substring(dataline.sym1,5,2)) + "." + string(substring(dataline.sym1,1,4))
            tempt.ts2 = string(dataline.sym2)
            tempt.ts3 = string(dataline.val[1])
            tempt.ts4 = GETENTRIES(5, dataline.txt, "~n", "")
            tempt.ts5 = GETENTRIES(1, dataline.txt, "~n", "")
            tempt.ts6 = GETENTRIES(2, dataline.txt, "~n", "")
            tempt.ts7 = string(GETENTRIES(10, dataline.txt, "~n", ""))
            tempt.ts8 = GETENTRIES(3, dataline.txt, "~n", "")
            tempt.ts9 = GETENTRIES(4, dataline.txt, "~n", "")
            tempt.ts10 = GETENTRIES(1, REPLACE(REPLACE(GETENTRIES(2, dataline.txt, "]", ""),"]",""),"[",""), "/", "")
            tempt.ts11 = GETENTRIES(2, REPLACE(REPLACE(GETENTRIES(2, dataline.txt, "]", ""),"]",""),"[",""), "/", "")
          .
      END.
  i = i + 1.
END.

i = 1.

FOR EACH tempt NO-LOCK by tempt.ts4 by tempt.ts10:
  tempt.ts00 = string(i).
  i = i + 1.
END.



/* fname = "./ef402ptk8_" + replace(string(end-date,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml". */
fname = "./ef402ptk8_" + userid('bisquit') + ".xml".

OUTPUT STREAM vvs TO VALUE (fname)

UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".

PUT STREAM vvs UNFORMATTED '<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Author>Фаворов Евгений Владимирович</Author>
  <LastAuthor>Фаворов Евгений Владимирович</LastAuthor>
  <LastPrinted>2013-09-24T07:30:19Z</LastPrinted>
  <Created>2013-06-21T08:05:27Z</Created>
  <LastSaved>2014-02-06T09:45:26Z</LastSaved>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>4350</WindowHeight>
  <WindowWidth>15330</WindowWidth>
  <WindowTopX>-15</WindowTopX>
  <WindowTopY>4305</WindowTopY>
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
  <Style ss:ID="m31173920">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173940">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173696">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173716">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173736">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173756">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173776">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173796">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173024">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173044">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173064">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173084">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173104">
   <Alignment ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="m31173124">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s62">
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s64">
   <Alignment ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s65">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s66">
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="9"/>
   <Interior/>
  </Style>
  <Style ss:ID="s67">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s84">
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s85">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="9"/>
   <Interior/>
  </Style>
  <Style ss:ID="s86">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s87">
   <Alignment ss:Horizontal="Right" ss:Vertical="Center"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="8"/>
   <Interior/>
  </Style>
  <Style ss:ID="s88">
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204" ss:Size="9"/>
   <Interior/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Москва">
  <Table ss:ExpandedColumnCount="21" x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s62" ss:DefaultRowHeight="12">
   <Column ss:StyleID="s63" ss:Width="46.5"/>
   <Column ss:StyleID="s63" ss:Width="53.25"/>
   <Column ss:StyleID="s63" ss:Width="36.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="60"/>
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="40.5"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="51.75"/>
   <Column ss:StyleID="s65" ss:Width="36.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="34.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="45.75"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="68.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="81"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="60.75"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="75"/>
   <Column ss:StyleID="s62" ss:Width="151.5"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="251.25"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="115.5"/>
   <Column ss:StyleID="s66" ss:AutoFitWidth="0" ss:Width="81"/>
   <Column ss:StyleID="s62" ss:Width="198"/>
   <Column ss:StyleID="s62" ss:Width="180"/>
   <Column ss:StyleID="s62" ss:Width="78"/>
   <Column ss:StyleID="s62" ss:Width="69"/>
   <Row ss:AutoFitHeight="0" ss:Height="38.25">
    <Cell ss:MergeDown="1" ss:StyleID="m31173024"><Data ss:Type="String">Номер&#10;раздела</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173044"><Data ss:Type="String">Код&#10;строки</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173064"><Data ss:Type="String">Номер КО</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173084"><Data ss:Type="String">Дата&#10;операции</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173104"><Data ss:Type="String">Код&#10;валюты&#10;платежа</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173124"><Data ss:Type="String">Сумма&#10;платежа, &#10;в&#160;тыс. ед.&#10;валюты</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173696"><Data ss:Type="String">Код услуги</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173716"><Data ss:Type="String">Код&#10;направления&#10;платежа</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173736"><Data ss:Type="String">Код страны нерезидента</Data></Cell>
    <Cell ss:MergeAcross="2" ss:StyleID="m31173756"><Data ss:Type="String">БИК (СВИФТ) банка, обслуживающего нерезидента</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173776"><Data ss:Type="String">ИНН резидента&#10;(только для ЮЛ)</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173796"><Data ss:Type="String">Клиент</Data></Cell>
    <Cell ss:MergeDown="1" ss:StyleID="m31173920"><Data ss:Type="String">Наименование нерезидента</Data></Cell>
    <Cell ss:MergeAcross="3" ss:StyleID="m31173940"><Data ss:Type="String">Примечание</Data></Cell>
    <Cell ss:StyleID="s84"/>
   </Row>
   <Row ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:Index="10" ss:StyleID="s67"><Data ss:Type="String">БИК</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">СВИФТ</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Код страны банка, обслуживающего нерезидента</Data></Cell>
    <Cell ss:Index="16" ss:StyleID="s67"><Data ss:Type="String">Код страны банка нерезидента</Data></Cell>
    <Cell ss:StyleID="s85"><Data ss:Type="String">Краткое содержание назначения платежа</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Наименование иностранного банка</Data></Cell>
    <Cell ss:StyleID="s67"><Data ss:Type="String">Наименование иностранного банка, обслуживающего контрагента-нерезидента</Data></Cell>
   </Row>
'.



FOR EACH tempt NO-LOCK by tempt.ts4 by tempt.ts10:
  FOR EACH btempt NO-LOCK:  
    IF integer(btempt.ts00) = integer(tempt.ts00) + 1
      THEN DO:
        IF (btempt.ts4 = tempt.ts4) AND (btempt.ts10 = tempt.ts10) 
          THEN DO: 
            tempt.ts0 = "*".
            btempt.ts0 = "*".
            tempt.ts3 = "777" + tempt.ts3.
            btempt.ts3 = "777" + btempt.ts3.
          END.
      END.
  END.
END.



i = 0.
str1 = "".
str2 = "".

FOR EACH tempt NO-LOCK by tempt.ts4 by tempt.ts10:

  IF ((tempt.ts0 = "*") AND (str1 <> "777") AND (str2 <> tempt.ts10) AND (substring(tempt.ts3,1,3) = "777")) OR
     ((tempt.ts0 = "*") AND (str1 = "777") AND (str2 <> tempt.ts10) AND (substring(tempt.ts3,1,3) = "777"))
    THEN DO:
      i = 0.
      i = i + decimal(substring(tempt.ts3,4,(length(tempt.ts3) - 3))).
/*      tempt.ts0 = string(i). */
    END.

  IF ((tempt.ts0 = "*") AND (str1 = "777") AND (str2 = tempt.ts10) AND (substring(tempt.ts3,1,6) = "777777")) OR
     ((tempt.ts0 = "*") AND (str1 = "777777") AND (str2 = tempt.ts10) AND (substring(tempt.ts3,1,6) = "777777"))
    THEN DO:
      i = i + decimal(substring(tempt.ts3,7,(length(tempt.ts3) - 6))).
      tempt.ts0 = string(i).
    END.

  IF (tempt.ts0 = "*") AND (str1 = "777777") AND (str2 = tempt.ts10) AND (substring(tempt.ts3,1,3) = "777")
    THEN DO:
      i = i + decimal(substring(tempt.ts3,4,(length(tempt.ts3) - 3))).
      tempt.ts0 = string(i).
      tempt.ts3 = string(i).
      i = 0.
    END.

  IF (substring(tempt.ts3,1,3) = "777")
    THEN DO:
      str1 = "777".
      IF (substring(tempt.ts3,1,6) = "777777")
        THEN str1 = "777777".
    END.
    ELSE str1 = "".

  str2 = tempt.ts10.

END.


i = 0.

FOR EACH tempt NO-LOCK:
  IF ((tempt.ts0 = "*") AND (substring(tempt.ts3,1,3) = "777")) OR
     (substring(tempt.ts3,1,6) = "777777")
    THEN i = i + 1.
    ELSE DO:
      PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + "1189/4" + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts1 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts2 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">' + tempt.ts3 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts4 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts5 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts6 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts7 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts8 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts9 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts10 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String"></Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s86"><Data ss:Type="String">' + tempt.ts11 + '</Data></Cell>'.
      PUT STREAM vvs UNFORMATTED '</Row>'.
    END.
END.



PUT STREAM vvs UNFORMATTED
   '
  </Table>
 </Worksheet>
</Workbook>
'.

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
