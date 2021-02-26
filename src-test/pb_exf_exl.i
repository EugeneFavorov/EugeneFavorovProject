/* ------------------------------------------------------
File          : pb_exf_exl.i
�����祭��    : ������⥪� �/� ��� ���㧪� �� ��᪢�� � 䠩�� XL
---------------------------------------------------------
                ���᮪ �㭪権:
                ===============
- ��砫� XL-䠩��
   XLHead (cFN, cColl, lColl) => char
      cFN   - ��� ���� XL-䠩��,
      cColl - ��ଠ� �⮫�殢: D - ���, I - 楫��, N - �᫠, K - �᫠ (����), C - ᨬ����,
      lColl - �ਭ� �⮫�殢.
- ᫥���騩 ���� XL-䠩��
   XLNextList (cFN, cColl, lColl) => char
      cFN   - ��� ���� XL-䠩��,
      cColl - ��ଠ� �⮫�殢: D - ���, I - 楫��, N - �᫠, K - �᫠ (����), C - ᨬ����,
      lColl - �ਭ� �⮫�殢.
- ����砭�� XL-䠩��
   XLEnd () => char
- ��砫� ��ப� ⠡����
   XLRow (iBord) => char
      iBord - ��� ���孥� �࠭���: 1 - �����ୠ� ���, 2 - �������.
- ����砭�� ��ப� ⠡����
   XLRowEnd () => char
- �祩�� ᨬ���쭮�� �ଠ�
   XLCell (cVal) => char
      cVal  - ᮤ�ন��� �祩��
- �祩�� �ଠ� ����
   XLDateCell (dVal) => char
      daVal - ᮤ�ন��� �祩��. �᫨ �����.���祭��, � �祩�� �����.
              �᫨ ��� < 1800 (�।���), �뤠���� STRING(dVal).
              ��ଠ� � XL "��.��.����"
- �祩�� �᫮���� �ଠ�
   XLCell (dVal) => char
      dVal  - ᮤ�ন��� �祩��. �᫨ �����.���祭��, � �祩�� �����.
              ��ଠ� � XL "# ##0.00"
- ����� �祩��
   XLEmptyCell () => char

                �ᯮ�짮�����:
                ==============
/* ��⠢�塞 ������ */
   {pb_exf_exl.i}
/* ���뢠�� ��⮪ ��� �뢮�� � 䠩�, ��� �ᯮ��㥬             */
/* {exp-path.i &stream="STREAM xl"} - ������ � .../imp-exp/doc/  */
   OUTPUT STREAM xl THROUGH unix-dos > VALUE(FullFileName).
/* ��ନ�㥬 䠩�: */
   PUT STREAM xl UNFORMATTED
      XLHead("��� ����", "CDIN", "50,20.25,10,40.5")
         XLRow(2) XLCell("���") XLCell("���")    XLCell("�㬬�") XLRowEnd() /* ��������� */
         XLRow(0) XLCell(cName) XLDateCell(TODAY) XLNumCell(summ) XLRowEnd() /* �����    */
      XLEnd().
/* ����뢠�� ��⮪ */
     OUTPUT STREAM xl CLOSE.

------------------------------------------------------ */

/*********** ���������� �⨫�� �祥� **************/
/* ******* ���� �⨫� ��稭��� � s80 *********** */
DEFINE VARIABLE cXLAddStyles    AS CHARACTER    NO-UNDO INIT "".
PROCEDURE XLAddStyle:
    DEFINE INPUT  PARAMETER iNewStyle   AS CHARACTER    NO-UNDO.

    cXLAddStyles = cXLAddStyles + "~n" + iNewStyle.
END PROCEDURE.

/*********** �뢮� ���������� 䠩�� xls  **********/
FUNCTION XLHead RETURNS CHAR 
   (INPUT cFN   AS CHARACTER,  /* ��� ���� XL-䠩�� */
    INPUT cColl AS CHARACTER,  /* ��ଠ� �⮫�殢: D - ���, N - �᫠, C - ᨬ����. */
    INPUT lColl AS CHARACTER   /* ��ਭ� �⮫�殢 */
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

/*********** �뢮� ���������� ᫥�.���� xls  **********/
FUNCTION XLNextList RETURNS CHAR 
   (INPUT cFN   AS CHARACTER,  /* ��� ���� XL-䠩�� */
    INPUT cColl AS CHARACTER,  /* ��ଠ� �⮫�殢: D - ���, N - �᫠, C - ᨬ����. */
    INPUT lColl AS CHARACTER   /* ��ਭ� �⮫�殢 */
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

/********* �뢮� ����砭�� 䠩�� xls  **********/
FUNCTION XLEnd RETURNS CHAR: 
   RETURN "  </Table>~n" +
          "  <WorksheetOptions xmlns=""urn:schemas-microsoft-com:office:excel"">~n" +
          "  </WorksheetOptions>~n" +
          " </Worksheet>~n" +
          "</Workbook>".
END FUNCTION.

/****** ���������� ��ப� ***********/
FUNCTION XLRow RETURNS CHAR 
   (INPUT iBord AS INTEGER  /* ��� ���孥� �࠭���: 1 - �����ୠ� ���, 2 - ������� */
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

/****** ���������� ��ப� �������� ����� ***********/
FUNCTION XLRowH RETURNS CHAR 
   (INPUT iBord   AS INTEGER,  /* ��� ���孥� �࠭���: 1 - �����ୠ� ���, 2 - ������� */
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

/****** ����� ��ப� ***********/
FUNCTION XLRowEnd RETURNS CHAR :
    RETURN "   </Row>~n".
END FUNCTION.

/****** ��⠢�� �ந����쭮�� ⥪��  ****************/
FUNCTION XLText RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

    RETURN cVal.
END FUNCTION.

/****** ������ ᨬ����� ***********/
FUNCTION XLReplace RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

    cVal = REPLACE(cVal, "�", "N").
    cVal = REPLACE(cVal, "<", "&lt;").
    cVal = REPLACE(cVal, ">", "&gt;").
    cVal = REPLACE(cVal, "&lt;B&gt;", "<B>").
    cVal = REPLACE(cVal, "&lt;/B&gt;", "</B>").
    cVal = REPLACE(cVal, '"', "&quot;").
    cVal = REPLACE(cVal, CHR(10), "&#10;").
    RETURN cVal.
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� ***********/
FUNCTION XLCell RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� ***********/
FUNCTION XLCellStyle RETURNS CHAR 
   (INPUT cStl AS CHAR,  /* �⨫� */
    INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL '    <Cell ss:StyleID="XXX"><ss:Data ss:Type="String" xmlns="http://www.w3.org/TR/REC-html40">' NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   cBCell = REPLACE(cBCell, "XXX", cStl).
   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� � ࠬ�� ***********/
FUNCTION XLCellBox RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s48""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� � 蠯�� (� 業��, � ��७�ᮬ, ���� ���⮬ 12) ***********/
FUNCTION XLCellHat RETURNS CHAR 
   (INPUT cVal        AS CHAR,    /* ᮤ�ন��� �祩�� */
    INPUT iMergeRight AS INTEGER  /* ᪮�쪮 �祥� �������� ��ࠢ� */
   ) :

    DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "ss:StyleID=""s47""><ss:Data ss:Type=""String"">" NO-UNDO.
    DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
    DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

    cBCell = "    <Cell "
           + (IF (iMergeRight EQ 0) THEN "" ELSE ("ss:MergeAcross=""" + STRING(iMergeRight) + """ "))
           + cBCell.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

    RETURN cRetStr.
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� � ��������� (� 業��, � ��७�ᮬ, ����) ***********/
FUNCTION XLCellHead RETURNS CHAR 
   (INPUT cVal        AS CHAR,    /* ᮤ�ন��� �祩�� */
    INPUT iIndex      AS INTEGER, /* ����� �⮫�� (��� �ய�᪠ ��ꥤ������ ����) */
    INPUT iMergeDown  AS INTEGER, /* ᪮�쪮 �祥� �������� ����   */
    INPUT iMergeRight AS INTEGER  /* ᪮�쪮 �祥� �������� ��ࠢ� */
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
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

    RETURN cRetStr.
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� � ��७�ᮬ ***********/
FUNCTION XLCellWrap RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s45""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** ���������� ����� ᨬ���쭮� �祩�� � ��७�ᮬ � ᤢ���� ��ࠢ� ***********/
FUNCTION XLCellWrapR RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ᮤ�ন��� �祩�� */
   ) :

   DEFINE VARIABLE cBCell  AS CHARACTER INITIAL "    <Cell ss:StyleID=""s46""><ss:Data ss:Type=""String"" xmlns=""http://www.w3.org/TR/REC-html40"">" NO-UNDO.
   DEFINE VARIABLE cECell  AS CHARACTER INITIAL "</ss:Data></Cell>" NO-UNDO.
   DEFINE VARIABLE cRetStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nF      AS INTEGER   NO-UNDO.

   IF cVAL = ?
   THEN cRetStr = cBCell + "/�� ��।�����/" + cECell + "~n".
   ELSE cRetStr = cBCell + XLReplace(cVal)   + cECell + "~n".

   RETURN cRetStr.
          /* */
END FUNCTION.

/****** ���������� ����� �祩�� � ��⮩ ***********/
FUNCTION XLDateCell RETURNS CHAR 
   (INPUT daVal AS DATE   /* ᮤ�ন��� �祩�� */
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

/****** ���������� ����� �᫮��� �祩�� ***********/
FUNCTION XLNumCell RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* ᮤ�ন��� �祩�� */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** ���������� ����� �᫮��� �祩�� ***********/
FUNCTION XLNumCellStyle RETURNS CHAR 
   (INPUT cStl AS CHAR,   /* �⨫� */
    INPUT dVal AS DECIMAL /* ᮤ�ন��� �祩�� */
   ) :

   DEF VAR cBCell AS CHAR INITIAL '    <Cell ss:StyleID="XXX"><Data ss:Type="Number">' NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   cBCell = REPLACE(cBCell, "XXX", cStl).
   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** ���������� ����� �᫮��� �祩�� � ࠬ��� *********/
FUNCTION XLNumCellBox RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* ᮤ�ন��� �祩�� */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell ss:StyleID=""s49""><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF dVAL = ? THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** ���������� ����� �᫮��� �祩��. ����� 0 - ���� **/
FUNCTION XLNumECell RETURNS CHAR 
   (INPUT dVal AS DECIMAL /* ᮤ�ন��� �祩�� */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell><Data ss:Type=""Number"">" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL "</Data></Cell>" NO-UNDO.

   IF (dVAL = ?) OR (dVAL = 0) THEN
      RETURN "    <Cell/>~n".
   ELSE
      RETURN cBCell + STRING(dVal) + cECell + "~n".
END FUNCTION.

/****** ���������� ����� �祩�� � ��㫮� ***********/
FUNCTION XLFormulaCell RETURNS CHAR 
   (INPUT cVal AS CHAR   /* ��㫠 */
   ) :

   DEF VAR cBCell AS CHAR INITIAL "    <Cell ss:Formula=""" NO-UNDO.
   DEF VAR cECell AS CHAR INITIAL """></Cell>" NO-UNDO.

   RETURN cBCell + cVal + cECell + "~n".
END FUNCTION.

/****** ���������� ���⮩ �祩�� ***********/
FUNCTION XLEmptyCell RETURNS CHAR :
    RETURN "    <Cell/>~n".
END FUNCTION.

FUNCTION XLEmptyCells RETURNS CHAR
   (INPUT iNum AS INTEGER /* ������⢮ �祥� */
   ) :
   IF (iNum GT 1)
   THEN RETURN XLEmptyCell() + XLEmptyCells(iNum - 1).
   ELSE RETURN XLEmptyCell().
END FUNCTION.

/****** ������ ��㯯� ���祭�� � ⥪��  *************/
PROCEDURE XLSwap:
    DEFINE INPUT-OUTPUT PARAMETER cText   AS CHARACTER    NO-UNDO.
    DEFINE INPUT        PARAMETER cBox    AS CHARACTER    NO-UNDO.
    DEFINE INPUT        PARAMETER iBoxN   AS INTEGER      NO-UNDO. /* ���-�� ����� cBox */
    DEFINE INPUT        PARAMETER cVal    AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iValN       AS INTEGER      NO-UNDO. /* ���-�� ��� � iVal. ��⠫�� ���� cBox = '' */

    iValN  = LENGTH(cVal).

    DO I = iBoxN TO iValN + 1 BY -1:
        cText = REPLACE(cText, cBox + STRING(I), " ").
    END.
    DO I = iValN TO 1 BY -1:
        cText = REPLACE(cText, cBox + STRING(I), SUBSTRING(cVal, I, 1)).
    END.
END PROCEDURE.
