{globals.i}
{intrface.get pbase}
{sh-defs.i}

DEFINE VARIABLE mInt        AS INT64     NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mOst        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mNomerCZ    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRowStyle1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRowStyle2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRowStyle3  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustName   AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mTmp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetails    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mExist    AS LOGICAL   NO-UNDO.

{getdate.i}

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="rep-nomercz.xml" }

{rep-nomercz.hdr}

mInt = 0.

FOR EACH acct WHERE TRUE
   AND acct.acct-cat   EQ "b"
   AND acct.close-date EQ ?  
   AND CAN-DO("60323*,47423*",acct.acct)
   NO-LOCK:
      
   RUN acct-pos-pure IN h_base (acct.acct,
                                acct.currency,
                                end-date,
                                end-date,
                                CHR(251)
                                ).
   ASSIGN
      mOst = ABS(sh-bal).
   IF mOst EQ 0 THEN NEXT.
   
   mNomerCZ = GetXattrValueEx("acct",acct.acct + "," + acct.currency,"НомерСЗ","").
         
/*      vInBal = sh-in-bal*/
/*      vInVal = sh-in-val*/
/*      vVal   = sh-val   */
/*      vBal   = sh-bal   */
/*      vDb    = sh-db    */
/*      vCr    = sh-cr    */
/*      vVdb   = sh-vdb   */
/*      vVcr   = sh-vcr   */
   
   mRowStyle1 = "s73".
   mRowStyle2 = "s74".
   mRowStyle3 = "s76".
   
   IF TRIM(acct.details) EQ "" THEN
   DO:
      RUN GetCustName IN h_base (acct.cust-cat,
                           acct.cust-id,
                           ?,
                           OUTPUT mCustName[1],
                           OUTPUT mCustName[2],
                           INPUT-OUTPUT mTmp).
      mDetails = TRIM(mCustName[1] + " " + mCustName[2]).
   END.
   ELSE mDetails = TRIM(acct.details).
   
   PUT UNFORMATTED
      '   <Row ss:AutoFitHeight="1">~n' +
      '    <Cell ss:StyleID="' + mRowStyle1  + '"><Data ss:Type="String">' + DelFilFromAcct(acct.acct) + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle2  + '"><Data ss:Type="String">' + mDetails + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle3  + '"><Data ss:Type="Number">' + TRIM(STRING(mOst,"->>>,>>>,>>>,>>9.99")) + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + mRowStyle1  + '"><Data ss:Type="String">' + TRIM(mNomerCZ) + '</Data></Cell>~n' +
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

{preview2.i &filename="rep-nomercz.xml" }

RUN sndbispc ("file=" + "rep-nomercz.xml" + ";class=bq").

{intrface.del}          /* Выгрузка инструментария. */ 

RETURN.
