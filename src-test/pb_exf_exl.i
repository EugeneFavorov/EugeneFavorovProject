/* ------------------------------------------------------
File          : pb_exf_exl.i
Назначение    : Библиотека п/п для выгрузки из Бисквита в файлы XL
---------------------------------------------------------
                Список функций:
                ===============
- начало XL-файла
   XLHead (cFN, cColl, lColl) => char
      cFN   - Имя листа XL-файла,
      cColl - Формат столбцов: D - дата, I - целое, N - числа, K - числа (курс), C - символьн,
      lColl - ширина столбцов.
- следующий лист XL-файла
   XLNextList (cFN, cColl, lColl) => char
      cFN   - Имя листа XL-файла,
      cColl - Формат столбцов: D - дата, I - целое, N - числа, K - числа (курс), C - символьн,
      lColl - ширина столбцов.
- окончание XL-файла
   XLEnd () => char
- начало строки таблицы
   XLRow (iBord) => char
      iBord - Тип верхней границы: 1 - одинарная черта, 2 - двойная.
- окончание строки таблицы
   XLRowEnd () => char
- ячейка символьного формата
   XLCell (cVal) => char
      cVal  - содержимое ячейки
- ячейка формата даты
   XLDateCell (dVal) => char
      daVal - содержимое ячейки. Если неопр.значение, то ячейка пустая.
              Если год < 1800 (бредовый), выдается STRING(dVal).
              Формат в XL "ДД.ММ.ГГГГ"
- ячейка числового формата
   XLCell (dVal) => char
      dVal  - содержимое ячейки. Если неопр.значение, то ячейка пустая.
              Формат в XL "# ##0.00"
- пустая ячейка
   XLEmptyCell () => char

                Использование:
                ==============
/* Вставляем инклюдник */
   {pb_exf_exl.i}
/* Открываем поток для вывода в файл, или используем             */
/* {exp-path.i &stream="STREAM xl"} - кладет в .../imp-exp/doc/  */
   OUTPUT STREAM xl THROUGH unix-dos > VALUE(FullFileName).
/* Формируем файл: */
   PUT STREAM xl UNFORMATTED
      XLHead("имя листа", "CDIN", "50,20.25,10,40.5")
         XLRow(2) XLCell("Имя") XLCell("Дата")    XLCell("Сумма") XLRowEnd() /* заголовки */
         XLRow(0) XLCell(cName) XLDateCell(TODAY) XLNumCell(summ) XLRowEnd() /* данные    */
      XLEnd().
/* Закрываем поток */
     OUTPUT STREAM xl CLOSE.

------------------------------------------------------ */

/*********** Добавление стилей ячеек **************/
/* ******* Новые стили начинать с s80 *********** */
DEFINE VARIABLE cXLAddStyles    AS CHARACTER    NO-UNDO INIT "".
PROCEDURE XLAddStyle:
    DEFINE INPUT  PARAMETER iNewStyle   AS CHARACTER    NO-UNDO.

    cXLAddStyles = cXLAddStyles + "~n" + iNewStyle.
END PROCEDURE.

/*********** Вывод заголовока файла xls  **********/
FUNCTION XLHead RETURNS CHAR 
   (INPUT cFN   AS CHARACTER,  /* Имя листа XL-файла */
    INPUT cColl AS CHARACTER,  /* Формат столбцов: D - дата, N - числа, C - символьн. */
    INPUT lColl AS CHARACTER   /* Ширина столбцов */
   ):

    DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

    cRetStr =    "<?xml version=""1.0"" encoding=""cp866"" standalone=""yes""?>"
        + "~n<?mso-application progid=""Excel.Sheet""?>"
        + "~n<Workbook xmlns=""urn:schemas-microsoft-com:office:spreadsheet"""
        + "~n xmlns:o=""urn:schemas-microsoft-com:office:office"""
        + "~n xmlns:x=""urn:schemas-microsoft-com:office:excel"""
        + "~n xmlns:ss=""urn:schemas-microsoft-com:office:spreadsheet"""
        + "~n xmlns:html=""http://www.w3.org/TR/REC-html40"">"
        + "~n <DocumentProperties xmlns=""urn:schemas-microsoft-com:office:office"">"
        + "~n  <Version>11.9999</Version>"
        + "~n </DocumentProperties>"
        + "~n <ExcelWorkbook xmlns=""urn:schemas-microsoft-com:office:excel"">"
        + "~n </ExcelWorkbook>"
        + "~n <Styles>"
        + "~n  <Style ss:ID=""Default"" ss:Name=""Normal"">"
        + "~n   <Alignment ss:Vertical=""Top""/>"
        + "~n   <Borders/>"
        + "~n   <Font ss:FontName=""Arial Cyr"" x:CharSet=""204""/>"
        + "~n   <Interior/>"
        + "~n   <NumberFormat/>"
        + "~n   <Protection/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s21"">"
        + "~n   <NumberFormat ss:Format=""@""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s22"">"
        + "~n   <NumberFormat ss:Format=""Standard""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s23"">"
        + "~n   <NumberFormat ss:Format=""Short Date""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s24"">"
        + "~n   <NumberFormat ss:Format=""#,##0""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s25"">"
        + "~n   <NumberFormat ss:Format=""#,##0.0000""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s26"">"
        + "~n   <NumberFormat ss:Format=""\+* #,##0.00_p;\-* #,##0.00_p;* &quot;-&quot;??_p;@_p""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s31"">"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s32"">"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Double"" ss:Weight=""3""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s41"">"
        + "~n   <Alignment ss:Horizontal=""Center"" ss:Vertical=""Center"" ss:WrapText=""1""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s42"">"
        + "~n   <Alignment ss:Horizontal=""Center"" ss:Vertical=""Center"" ss:WrapText=""1""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Double"" ss:Weight=""3""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s43"">"
        + "~n   <Alignment ss:Horizontal=""Center"" ss:Vertical=""Center"" ss:WrapText=""1""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s44"">"
        + "~n   <Alignment ss:Horizontal=""Center"" ss:Vertical=""Center"" ss:WrapText=""1""/><Font ss:Bold=""1""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Left"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Right"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s45"">"
        + "~n   <Alignment ss:Horizontal=""Left"" ss:Vertical=""Top"" ss:WrapText=""1""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s46"">"
        + "~n   <Alignment ss:Horizontal=""Right"" ss:Vertical=""Top"" ss:WrapText=""1""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s47"">"
        + "~n   <Alignment ss:Horizontal=""Center"" ss:Vertical=""Center"" ss:WrapText=""1""/><Borders/>"
        + "~n   <Font ss:Size=""12"" ss:Bold=""1""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s48"">"
        + "~n   <Alignment ss:Vertical=""Top"" ss:WrapText=""1""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Left"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Right"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s49"">"
        + "~n   <Alignment ss:Vertical=""Top"" ss:WrapText=""1""/>"
        + "~n   <NumberFormat ss:Format=""Standard""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Left"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Right"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s63"">"
        + "~n   <Alignment ss:Horizontal=""Right""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s64"">"
        + "~n   <Alignment ss:Horizontal=""Center""/>"
        + "~n   <Font ss:Bold=""1""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s65"">"
        + "~n   <Font ss:Size=""8""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s66"">"
        + "~n   <Alignment ss:Horizontal=""Center""/>"
        + "~n   <Font ss:Size=""8""/>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s71"">"
        + "~n   <Alignment ss:Horizontal=""Center""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s72"">"
        + "~n   <Alignment ss:Horizontal=""Left""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s73"">"
        + "~n   <Alignment ss:Horizontal=""Right"" ss:Vertical=""Top""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + "~n  <Style ss:ID=""s74"">"
        + "~n   <Alignment ss:Horizontal=""Center""/>"
        + "~n   <Borders>"
        + "~n    <Border ss:Position=""Bottom"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Left"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Right"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n    <Border ss:Position=""Top"" ss:LineStyle=""Continuous"" ss:Weight=""1""/>"
        + "~n   </Borders>"
        + "~n  </Style>"
        + cXLAddStyles
        + "~n </Styles>"
        + "~n <Worksheet ss:Name=""" + cFN + """>"
        + "~n  <Table>"
        .

   i = LENGTH(cColl) - NUM-ENTRIES(lColl).
   IF i > 0 THEN lColl = lColl + FILL(",", i).

   DO i = 1 TO LENGTH(cColl) :
      CASE SUBSTR(cColl, i, 1):
         WHEN "D" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s23""".
         WHEN "N" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s22""".
         WHEN "K" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s25""".
         WHEN "I" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s24""".
         WHEN "+" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s26""".
         OTHERWISE
            cRetStr = cRetStr + "   <Column ss:StyleID=""s21""".
      END CASE.

      IF ENTRY(i, lColl) = ""
         THEN cRetStr = cRetStr + " ss:AutoFitWidth=""0""".
         ELSE cRetStr = cRetStr + " ss:Width=""" + TRIM(STRING(INTEGER(ENTRY(i, lColl)) * 3 / 4, ">>>9.99")) + """".

      cRetStr = cRetStr + "/>~n".
   END.
   
   RETURN cRetStr.
          /* */
END FUNCTION.

/*********** Вывод заголовока след.листа xls  **********/
FUNCTION XLNextList RETURNS CHAR 
   (INPUT cFN   AS CHARACTER,  /* Имя листа XL-файла */
    INPUT cColl AS CHARACTER,  /* Формат столбцов: D - дата, N - числа, C - символьн. */
    INPUT lColl AS CHARACTER   /* Ширина столбцов */
   ):

   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

   cRetStr = "  </Table>~n"
           + "  <WorksheetOptions xmlns=""urn:schemas-microsoft-com:office:excel"">~n"
           + "  </WorksheetOptions>~n"
           + " </Worksheet>~n"
           + " <Worksheet ss:Name=""" + cFN + """>~n"
           + "  <Table>~n"
           .

   i = LENGTH(cColl) - NUM-ENTRIES(lColl).
   IF i > 0 THEN lColl = lColl + FILL(",", i).

   DO i = 1 TO LENGTH(cColl) :
      CASE SUBSTR(cColl, i, 1):
         WHEN "D" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s23""".
         WHEN "N" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s22""".
         WHEN "K" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s25""".
         WHEN "I" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s24""".
         WHEN "+" THEN
            cRetStr = cRetStr + "   <Column ss:StyleID=""s26""".
         OTHERWISE
            cRetStr = cRetStr + "   <Column ss:StyleID=""s21""".
      END CASE.

      IF ENTRY(i, lColl) = ""
         THEN cRetStr = cRetStr + " ss:AutoFitWidth=""0""".
         ELSE cRetStr = cRetStr + " ss:Width=""" + TRIM(STRING(INTEGER(ENTRY(i, lColl)) * 3 / 4, ">>>9.99")) + """".

      cRetStr = cRetStr + "/>~n".
   END.
   
   RETURN cRetStr.
          /* */
END FUNCTION.

/********* Вывод окончания файла xls  **********/
FUNCTION XLEnd RETURNS CHAR: 
   RETURN "  </Table>~n" +
          "  <WorksheetOptions xmlns=""urn:schemas-microsoft-com:office:excel"">~n" +
          "  </WorksheetOptions>~n" +
          " </Worksheet>~n" +
          "</Workbook>".
END FUNCTION.

/****** Добавление строки ***********/
FUNCTION XLRow RETURNS CHAR 
   (INPUT iBord AS INTEGER  /* Тип верхней границы: 1 - одинарная черта, 2 - двойная */
   ) :

   CASE iBord:
      WHEN 2 THEN
         RETURN "   <Row ss:StyleID=""s32"">~n".
      WHEN 1 THEN
         RETURN "   <Row ss:StyleID=""s31"">~n".
      WHEN -3 THEN
         RETURN "   <Row ss:StyleID=""s43"">~n".
      WHEN -2 THEN
         RETURN "   <Row ss:StyleID=""s42"">~n".
      WHEN -1 THEN
         RETURN "   <Row ss:StyleID=""s41"">~n".
      OTHERWISE
         RETURN "   <Row>~n".
   END CASE.
END FUNCTION.

/****** Добавление строки заданной высоты ***********/
FUNCTION XLRowH RETURNS CHAR 
   (INPUT iBord   AS INTEGER,  /* Тип верхней границы: 1 - одинарная черта, 2 - двойная */
    INPUT iHeight AS INTEGER
   ) :

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
    cTmp = "   <Row ss:Height=""" + TRIM(STRING(iHeight * 3 / 4, ">>>9.99")) + """".
    CASE iBord:
        WHEN 2 THEN
            RETURN cTmp + " ss:StyleID=""s32"">~n".
        WHEN 1 THEN
            RETURN cTmp + " ss:StyleID=""s31"">~n".
        WHEN -3 THEN
            RETURN cTmp + " ss:StyleID=""s43"">~n".
        WHEN -2 THEN
            RETURN cTmp + " ss:StyleID=""s42"">~n".
        WHEN -1 THEN
            RETURN cTmp + " ss:StyleID=""s41"">~n".
        OTHERWISE
            RETURN cTmp + ">~n".
    END CASE.
END FUNCTION.

/****** Конец строки ***********/
FUNCTION XLRowEnd RETURNS CHAR :
    RETURN "   </Row>~n".
END FUNCTION.

/****** Вставка произвольного текста  ****************/
FUNCTION XLText RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

    RETURN cVal.
END FUNCTION.

/****** Замена символов ***********/
FUNCTION XLReplace RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

    cVal = REPLACE(cVal, "№", "N").
    cVal = REPLACE(cVal, "<", "&lt;").
    cVal = REPLACE(cVal, ">", "&gt;").
    cVal = REPLACE(cVal, "&lt;B&gt;", "<B>").
    cVal = REPLACE(cVal, "&lt;/B&gt;", "</B>").
    cVal = REPLACE(cVal, '"', "&quot;").
    cVal = REPLACE(cVal, CHR(10), "&#10;").
    RETURN cVal.
END FUNCTION.

/****** Добавление новой символьной ячейки ***********/
FUNCTION XLCell RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** Добавление новой символьной ячейки ***********/
FUNCTION XLCellStyle RETURNS CHAR 
   (INPUT cStl AS CHAR,  /* стиль */
    INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL '    <Cell ss:StyleID="XXX"><ss:Data ss:Type="String" xmlns="http://www.w3.org/TR/REC-html40">' NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   cBCell = REPLACE(cBCell, "XXX", cStl).
   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** Добавление новой символьной ячейки в рамке ***********/
FUNCTION XLCellBox RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s48""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** Добавление новой символьной ячейки в шапку (в центре, с переносом, жирным шрифтом 12) ***********/
FUNCTION XLCellHat RETURNS CHAR 
   (INPUT cVal        AS CHAR,    /* содержимое ячейки */
    INPUT iMergeRight AS INTEGER  /* сколько ячеек добавить вправо */
   ) :

    DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "ss:StyleID=""s47""><ss:Data ss:Type=""String"">" NO-UNDO.
    DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
    DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

    cBCell = "    <Cell "
           + (IF (iMergeRight EQ 0) THEN "" ELSE ("ss:MergeAcross=""" + STRING(iMergeRight) + """ "))
           + cBCell.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

    RETURN cRetStr.
END FUNCTION.

/****** Добавление новой символьной ячейки в заголовок (в центре, с переносом, жирным) ***********/
FUNCTION XLCellHead RETURNS CHAR 
   (INPUT cVal        AS CHAR,    /* содержимое ячейки */
    INPUT iIndex      AS INTEGER, /* номер столбца (для пропуска объединений вниз) */
    INPUT iMergeDown  AS INTEGER, /* сколько ячеек добавить вниз   */
    INPUT iMergeRight AS INTEGER  /* сколько ячеек добавить вправо */
   ) :

    DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "ss:StyleID=""s44""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
    DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
    DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

    cBCell = "    <Cell "
           + (IF (iIndex      EQ 0) THEN "" ELSE ("ss:Index="""       + STRING(iIndex)      + """ "))
           + (IF (iMergeDown  EQ 0) THEN "" ELSE ("ss:MergeDown="""   + STRING(iMergeDown)  + """ "))
           + (IF (iMergeRight EQ 0) THEN "" ELSE ("ss:MergeAcross=""" + STRING(iMergeRight) + """ "))
           + cBCell.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

    RETURN cRetStr.
END FUNCTION.

/****** Добавление новой символьной ячейки с переносом ***********/
FUNCTION XLCellWrap RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s45""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** Добавление новой символьной ячейки с переносом и сдвигом вправо ***********/
FUNCTION XLCellWrapR RETURNS CHAR 
   (INPUT cVal AS CHAR   /* содержимое ячейки */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s46""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/НЕ определено/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** Добавление новой ячейки с датой ***********/
FUNCTION XLDateCell RETURNS CHAR 
   (INPUT daVal AS DATE   /* содержимое ячейки */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell><Data ss:Type=""DateTime"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "T00:00:00.000</Data></Cell>" NO-UNDO.

   IF daVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE IF YEAR(daVal) < 1800 THEN
      RETURN "    <Cell><Data ss:Type=""String"">" + 
                      STRING(daVal) + cECell + "~n".
   ELSE
      RETURN cBCell + STRING(YEAR(daVal), "9999") + "-" +
                      STRING(MONTH(daVal),  "99") + "-" +
                      STRING(DAY(daVal),    "99") + cECell + "~n".
END FUNCTION.

/****** Добавление новой числовой ячейки ***********/
FUNCTION XLNumCell RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* содержимое ячейки */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** Добавление новой числовой ячейки ***********/
FUNCTION XLNumCellStyle RETURNS CHAR 
   (INPUT cStl AS CHAR,   /* стиль */
    INPUT dVal AS DECIMAL /* содержимое ячейки */
   ) :

   DEF VAR cBCell AS CHAR INITIAL '    <Cell ss:StyleID="XXX"><Data ss:Type="Number">' NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   cBCell = REPLACE(cBCell, "XXX", cStl).
   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** Добавление новой числовой ячейки с рамкой *********/
FUNCTION XLNumCellBox RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* содержимое ячейки */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell ss:StyleID=""s49""><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** Добавление новой числовой ячейки. Вместо 0 - пусто **/
FUNCTION XLNumECell RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* содержимое ячейки */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF (dVAL = ?) OR (dVAL = 0) THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** Добавление новой ячейки с формулой ***********/
FUNCTION XLFormulaCell RETURNS CHAR 
   (INPUT cVal AS CHAR   /* формула */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell ss:Formula=""" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL """></Cell>" NO-UNDO.

   RETURN cBCell + cVal + cECell + "~n".
END FUNCTION.

/****** Добавление пустой ячейки ***********/
FUNCTION XLEmptyCell RETURNS CHAR :
    RETURN "    <Cell/>~n".
END FUNCTION.

FUNCTION XLEmptyCells RETURNS CHAR
   (INPUT iNum AS INTEGER /* количество ячеек */
   ) :
   IF (iNum GT 1)
   THEN RETURN XLEmptyCell() + XLEmptyCells(iNum - 1).
   ELSE RETURN XLEmptyCell().
END FUNCTION.

/****** Замена группы значений в тексте  *************/
PROCEDURE XLSwap:
    DEFINE INPUT-OUTPUT PARAMETER cText   AS CHARACTER    NO-UNDO.
    DEFINE INPUT        PARAMETER cBox    AS CHARACTER    NO-UNDO.
    DEFINE INPUT        PARAMETER iBoxN   AS INTEGER      NO-UNDO. /* Кол-во полей cBox */
    DEFINE INPUT        PARAMETER cVal    AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iValN       AS INTEGER      NO-UNDO. /* Кол-во цифр в iVal. Остальные поля cBox = '' */

    iValN  = LENGTH(cVal).

    DO I = iBoxN TO iValN + 1 BY -1:
        cText = REPLACE(cText, cBox + STRING(I), " ").
    END.
    DO I = iValN TO 1 BY -1:
        cText = REPLACE(cText, cBox + STRING(I), SUBSTRING(cVal, I, 1)).
    END.
END PROCEDURE.
