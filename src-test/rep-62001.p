{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{parsin.def}
{sh-defs.i}

{tmprecid.def}

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt1  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt4  AS INT64     NO-UNDO.

DEFINE VARIABLE mContCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAutoName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAutoVIN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAutoYear       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAutoVidO       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRowStyle   AS CHARACTER NO-UNDO.

DEFINE BUFFER signs  FOR signs.
DEFINE BUFFER signs2 FOR signs.

/*{setdest.i &filename = "'rep-62001.log'"}*/

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="rep-62001.xml" }

mInt  = 0.
mInt1 = 0.
mInt2 = 0.
mInt3 = 0.
mInt4 = 0.

{rep-62001.hdr}

mRowStyle  = "s77".

FOR EACH acct WHERE TRUE
   AND acct.acct-cat  EQ "b"
   AND acct.acct      BEGINS "62"
   /*AND acct.acct BEGINS "62001810300090001199"*/
   AND acct.filial-id EQ "0000"
   NO-LOCK,
   EACH signs WHERE TRUE
   AND signs.code      EQ "NomKD"
   AND signs.file-name EQ "acct"
   AND signs.surrogate EQ acct.acct + "," + acct.currency
   NO-LOCK:

   mInt1 = mInt1 + 1.
   
   /*Ищем договора*/
   ASSIGN
      mContCode = ""
      mAutoName = ""
      mAutoVIN  = ""
      mAutoYear = "".
   FOR EACH loan WHERE TRUE
      AND loan.contract  EQ "Кредит"
      AND loan.cont-code BEGINS TRIM(signs.code-value)
      NO-LOCK:
      IF loan.filial-id NE "0000" THEN NEXT.
      mContCode = loan.cont-code.
   END.
   IF NOT {assigned mContCode} THEN
   FOR EACH loan WHERE TRUE
      AND loan.contract  EQ "Кредит"
      AND loan.cont-code BEGINS TRIM(signs.code-value)
      NO-LOCK:
      IF loan.filial-id NE "0400" THEN NEXT.         
      mContCode = loan.cont-code.
   END.
   IF NOT {assigned mContCode} THEN
   FOR EACH loan WHERE TRUE
      AND loan.contract  EQ "Кредит"
      AND loan.cont-code BEGINS TRIM(signs.code-value)
      NO-LOCK:
      IF loan.filial-id NE "0300" THEN NEXT.         
      mContCode = loan.cont-code.
   END.
   IF NOT {assigned mContCode} THEN
   FOR EACH loan WHERE TRUE
      AND loan.contract  EQ "Кредит"
      AND loan.cont-code BEGINS TRIM(signs.code-value)
      NO-LOCK:
      IF loan.filial-id NE "0500" THEN NEXT.         
      mContCode = loan.cont-code.
   END.
   /*Проверка обеспечения*/
   mCnt = 0.
   FOR EACH loan WHERE TRUE
      AND loan.contract    EQ "Кредит"
      AND loan.cont-code   EQ mContCode
      NO-LOCK:
      FOR EACH /*FIRST*/ /*LAST*/ /*EACH*/ term-obl WHERE TRUE
         AND term-obl.cont-code EQ loan.cont-code
         AND term-obl.contract  EQ loan.contract
         AND term-obl.idnt      EQ 5
         NO-LOCK:
         FOR EACH signs2 WHERE TRUE
            AND signs2.file-name EQ "term-obl"
            AND signs2.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn)
            NO-LOCK:
            /*IF signs.code EQ "TCbrand" THEN mAutoName  = signs.xattr-value.*/
            IF signs2.code EQ "TCmodel"  THEN mAutoName = signs2.xattr-value.
            IF signs2.code EQ "TCVIN"    THEN mAutoVIN  = signs2.xattr-value.
            IF signs2.code EQ "TCyear"   THEN mAutoYear = signs2.code-value.
            IF signs2.code EQ "ВидОб"    THEN mAutoVidO = signs2.xattr-value.
         END.
         IF mAutoVidO EQ "Автомобиль" THEN mCnt = mCnt + 1.
      END.
   END.
   /*Если есть договор и обеспечение*/
   IF {assigned mContCode} 
   AND mCnt GT 0 THEN
   FOR EACH loan WHERE TRUE
      AND loan.contract    EQ "Кредит"
      AND loan.cont-code   EQ mContCode
      /*AND loan.cont-code   EQ  "03-00-2061-МБА@0400"*/
      NO-LOCK:
      
      PUT UNFORMATTED
         '   <Row ss:AutoFitHeight="1">~n' +
         '    <Cell ss:Index="2" ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + acct.acct + '</Data></Cell>~n' +
         '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mContCode + '</Data></Cell>~n'.
      
      mInt2 = mInt2 + 1.
      mCnt2 = 0.
      
      FOR EACH /*FIRST*/ /*LAST*/ /*EACH*/ term-obl WHERE TRUE
         AND term-obl.cont-code EQ loan.cont-code
         AND term-obl.contract  EQ loan.contract
         AND term-obl.idnt      EQ 5
         NO-LOCK:
   
         ASSIGN
            mAutoName = ""
            mAutoVIN  = ""
            mAutoYear = ""
            mAutoVidO = "".   
         
         FOR EACH signs2 WHERE TRUE
            AND signs2.file-name EQ "term-obl"
            AND signs2.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn)
            NO-LOCK:
            IF signs2.code EQ "TCmodel"  THEN mAutoName = signs2.xattr-value.
            IF signs2.code EQ "TCVIN"    THEN mAutoVIN  = signs2.xattr-value.
            IF signs2.code EQ "TCyear"   THEN mAutoYear = signs2.code-value.
            IF signs2.code EQ "ВидОб"    THEN mAutoVidO = signs2.xattr-value.
         END.
         
         IF mAutoVidO EQ "Автомобиль" THEN
         DO:
            mCnt2 = mCnt2 + 1.
            IF mCnt2 EQ 1 THEN
            PUT UNFORMATTED
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoName + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoVIN  + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoYear + '</Data></Cell>~n' +
               '   </Row>'
            SKIP.
            ELSE
            PUT UNFORMATTED
               '   <Row ss:AutoFitHeight="1">~n' +
               '    <Cell ss:Index="2" ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">Обеспечение ' + TRIM(STRING(mCnt2)) + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String"></Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoName + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoVIN  + '</Data></Cell>~n' +
               '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoYear + '</Data></Cell>~n' +
               '   </Row>'
            SKIP.
         END.
      END.
/*      IF mCnt2 EQ 0 THEN PUT UNFORMATTED ";;;;;" SKIP.*/
   END.
   /*Нет договора или договор есть, нет обеспечения*/
   ELSE IF NOT {assigned mContCode} OR mCnt EQ 0 THEN
   DO:
      mInt = 0.
      FOR EACH code WHERE TRUE
         AND code.class  EQ "Auto62001"
         AND code.parent EQ "Auto62001"
         AND code.code   EQ signs.code-value
         NO-LOCK:
         
         mInt = mInt + 1.   
         mInt3 = mInt3 + 1.
         
         ASSIGN
            mContCode = signs.code-value
            mAutoName = code.name
            mAutoVIN  = code.val
            mAutoYear = code.description[1].
            
         PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
            '    <Cell ss:Index="2" ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + acct.acct + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mContCode + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoName + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoVIN  + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoYear + '</Data></Cell>~n' +
            '   </Row>'
         SKIP.
      END.
      IF mInt EQ 0 THEN
      DO:
         mInt3 = mInt3 + 1.
         PUT UNFORMATTED
            '   <Row ss:AutoFitHeight="1">~n' +
            '    <Cell ss:Index="2" ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + acct.acct + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mContCode + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + 'Не найден договор или обеспечение' + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoVIN  + '</Data></Cell>~n' +
            '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mAutoYear + '</Data></Cell>~n' +
            '   </Row>'
         SKIP.
      END.
   END.
END.

/*MESSAGE mInt1 ";" mInt2 ";" mInt3 ";" mInt2 + mInt3 ";"*/
/*VIEW-AS ALERT-BOX.                                     */

PUT UNFORMATTED
   '  </Table>~n' + 
   '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">~n' +
   '   <PageSetup><Layout x:Orientation="Landscape"/></PageSetup>~n' +
   '   <Unsynced/>~n' +
   '   <FitToPage/>~n' +
   '   <Selected/>~n' +
   '  </WorksheetOptions>~n' +
   ' </Worksheet>~n' +
   '</Workbook>'
SKIP.

/*{preview2.i &filename="rep-62001.xml" }*/

RUN sndbispc ("file=" + "rep-62001.xml" + ";class=bq").

{intrface.del}

RETURN.
