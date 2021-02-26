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

DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mCondDate   AS DATE      NO-UNDO.
DEFINE VARIABLE mPrice11    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrice1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPolRekv    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDatSogl    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPerson     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmount     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mRowStyle   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mStatus           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateOpen         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateClose        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsNew            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNomDog           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTCbrand          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTCmodel          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTCVIN            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTCyear           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTCisNew          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryDate1Post  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumb1Post  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryId1Post    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryDate2Out   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumb2Out   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryId2Out     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRefNumber   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRegZalog         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRegDate     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mExist    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mBarTotal AS INT64     NO-UNDO.
DEFINE VARIABLE mBarCurnt AS INT64     NO-UNDO.

DEFINE BUFFER term-obl  FOR term-obl.
DEFINE BUFFER term-obl2 FOR term-obl.

DEFINE BUFFER loan      FOR loan.
DEFINE BUFFER loan2     FOR loan.

{getdates.i}

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="rep-cd.xml" }

/*mBegDate = DATE("01/01/2017").*/
/*mEndDate = DATE("13/04/2017").*/

mBegDate = beg-date. 
mEndDate = end-date.

mBarTotal = 0.

FOR EACH loan WHERE TRUE
/*   AND loan.close-date EQ ?*/
   AND loan.contract    EQ "Кредит"
   AND loan.cont-type   BEGINS "Авт"
   AND loan.loan-status NE "СОЗД"
/*   AND loan.filial-id  NE "0400"*/
   AND loan.open-date   GE mBegDate
   AND loan.open-date   LE mEndDate
/*   AND loan.cont-code  BEGINS "20-00-108699-ГАПАБ"*/
   NO-LOCK,
   LAST /*EACH*/ term-obl WHERE TRUE
   AND term-obl.cont-code EQ loan.cont-code
   AND term-obl.contract  EQ loan.contract
   AND term-obl.idnt      EQ 5
   NO-LOCK:
   mBarTotal = mBarTotal + 1.
END.

{bar-beg2.i
   &BarTotal     = mBarTotal
   &BarMessage   = "'Формирование отчета'"}

{rep-cd.hdr}

mBarCurnt = 0.
mInt = 0.
FOR EACH loan WHERE TRUE
/*   AND loan.close-date EQ ?*/
   AND loan.contract    EQ "Кредит"
   AND loan.cont-type   BEGINS "Авт"
   AND loan.loan-status NE "СОЗД"
/*   AND loan.filial-id  NE "0400"*/
   AND loan.open-date   GE mBegDate
   AND loan.open-date   LE mEndDate
/*   AND loan.cont-code   BEGINS "20-00-108699-ГАПАБ"*/
   NO-LOCK,
   LAST /*EACH*/ term-obl WHERE TRUE
   AND term-obl.cont-code EQ loan.cont-code
   AND term-obl.contract  EQ loan.contract
   AND term-obl.idnt      EQ 5
   NO-LOCK:
      
   mBarCurnt = mBarCurnt + 1.
   {bar2.i
     &BarPointer = mBarCurnt}

   mExist = NO.
   FOR EACH loan2 WHERE TRUE
      AND loan2.contract   EQ "Кредит"
      AND loan2.cont-code  BEGINS ENTRY(1,loan.cont-code,"@")
      AND loan.filial-id   EQ "0400"
      AND loan2.filial-id  NE loan.filial-id
      NO-LOCK:
      mExist = YES.  
   END. 
   IF mExist THEN NEXT.

   FOR EACH loan-cond WHERE TRUE
      AND loan-cond.contract  EQ loan.contract
      AND loan-cond.cont-code EQ loan.cont-code
      NO-LOCK BY loan-cond.since DESC:
      mCondDate = loan-cond.since.
   END.
   FIND FIRST term-obl2 WHERE TRUE
      AND term-obl2.cont-code EQ loan.cont-code
      AND term-obl2.contract  EQ loan.contract
      AND term-obl2.end-date  EQ mCondDate
      AND term-obl2.idnt      EQ 2
   NO-LOCK NO-ERROR .
   mAmount = IF AVAIL(term-obl2) THEN term-obl2.amt-rub ELSE 0.

/*   MESSAGE mCondDate ";" mAmount ";" loan.cust-cat*/
/*   VIEW-AS ALERT-BOX.                             */

   IF mAmount       LE 0   THEN NEXT.
   IF loan.cust-cat EQ "Б" THEN NEXT.
   
   mPerson = "".
   IF loan.cust-cat EQ "Ч" THEN       
   FOR EACH person WHERE
      person.person-id EQ loan.cust-id
      NO-LOCK:
      mPerson = person.name-last + " " + person.first-names.
   END.
   
   mStatus    = IF loan.loan-status EQ "ЗАКР" THEN "Закрыт" ELSE "Открыт".
   mDateOpen  = IF loan.open-date  NE ? THEN STRING(loan.open-date, "99/99/9999") ELSE "".
   mDateClose = IF loan.close-date NE ? THEN STRING(loan.close-date,"99/99/9999") ELSE "".
   mStatus    = IF loan.loan-status EQ "ЗАКР" THEN "Закрыт" ELSE "Открыт".
   mPrice11   = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko11_price","").
   mPrice1    = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_price","").
   mPolRekv   = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучательРекв","").
   mDatSogl   = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ДатаСогл","").
   mPrice11   = TRIM(STRING(DECIMAL(mPrice11),">>>,>>>,>>>,>>>,>>9.99")).
   mPrice1    = TRIM(STRING(DECIMAL(mPrice1 ),">>>,>>>,>>>,>>>,>>9.99")).
   ASSIGN
      mNomDog          = ""
      mTCbrand         = ""
      mTCmodel         = ""
      mTCyear          = ""
      mTCisNew         = ""
      mRegZalog        = ""
      mNotaryDate1Post = ""
      mNotaryNumb1Post = ""
      mNotaryId1Post   = ""
      mNotaryDate2Out  = ""
      mNotaryNumb2Out  = ""
      mNotaryId2Out    = ""
      mNotifRefNumber  = ""
      mNotifRegDate    = "".
   FOR EACH signs WHERE TRUE
      AND signs.file-name EQ "term-obl"
      AND signs.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn)
      NO-LOCK:
      IF signs.code EQ "НомДогОб"        THEN mNomDog          = signs.xattr-value.
      IF signs.code EQ "TCbrand"         THEN mTCbrand         = signs.xattr-value.
      IF signs.code EQ "TCmodel"         THEN mTCmodel         = signs.xattr-value.
      IF signs.code EQ "TCVIN"           THEN mTCVIN           = signs.xattr-value.
      IF signs.code EQ "TCyear"          THEN mTCyear          = signs.code-value.
      IF signs.code EQ "TCis-new"        THEN mTCisNew         = signs.code-value.
      IF signs.code EQ "NotaryDate1Post" THEN mNotaryDate1Post = signs.xattr-value.
      IF signs.code EQ "NotaryNumb1Post" THEN mNotaryNumb1Post = signs.xattr-value.
      IF signs.code EQ "NotaryId1Post"   THEN mNotaryId1Post   = signs.code-value.
      IF signs.code EQ "NotaryDate2Out"  THEN mNotaryDate2Out  = signs.xattr-value.
      IF signs.code EQ "NotaryNumb2Out"  THEN mNotaryNumb2Out  = signs.xattr-value.
      IF signs.code EQ "NotaryId2Out"    THEN mNotaryId2Out    = signs.code-value.
      IF signs.code EQ "NotifRefNumber"  THEN mNotifRefNumber  = signs.xattr-value.

/*      IF signs.code EQ "reg-zalog"      THEN mRegZalog       = signs.xattr-value.*/
/*      IF signs.code EQ "NotifRegDate"   THEN mNotifRegDate   = signs.code-value.*/

   END.
   
   mRowStyle  = "s77".
/*   IF mTCisNew EQ "" OR mTCisNew EQ "Нет" THEN mRowStyle  = "s77yellow".*/
   IF mTCisNew EQ "Да"THEN mRowStyle  = "s77green".
   mDatSogl = STRING(loan.open-date,"99/99/9999").
   
   mInt = mInt + 1.
   
   PUT UNFORMATTED
      '   <Row ss:AutoFitHeight="1">~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mDatSogl + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mStatus + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + loan.cont-code + '</Data></Cell>~n' +
      /*'    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + DelFilFromLoan(loan.cont-code) + '</Data></Cell>~n' +*/
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mDateOpen  + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mDateClose + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNomDog + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mPerson + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + TRIM(STRING(mAmount,">>>,>>>,>>>,>>9.99")) + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + REPLACE(REPLACE(mTCbrand,"<",""),">","") + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + REPLACE(REPLACE(mTCmodel,"<",""),">","") + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + REPLACE(REPLACE(mTCVIN,"<",""),">","") + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mTCyear + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mTCisNew + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryDate1Post + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryNumb1Post + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryId1Post + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryDate2Out + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryNumb2Out + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotaryId2Out + '</Data></Cell>~n' +            
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotifRefNumber + '</Data></Cell>~n' +
      /*'    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mNotifRegDate + '</Data></Cell>~n' +*/
      /*'    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mRegZalog + '</Data></Cell>~n' +      */
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mPrice11 + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + mPrice1 + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle  + '"><Data ss:Type="String">' + REPLACE(REPLACE(mPolRekv,"<",""),">","") + '</Data></Cell>~n' +
      '   </Row>'
   SKIP.
END.

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

/*{preview2.i &filename="rep-cd.xml" }*/

RUN sndbispc ("file=" + "rep-cd.xml" + ";class=bq").

{intrface.del}          /* Выгрузка инструментария. */ 

RETURN.
