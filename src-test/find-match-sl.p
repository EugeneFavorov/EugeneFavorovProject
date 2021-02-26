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
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mNom   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymb  AS CHARACTER NO-UNDO.
DEFINE BUFFER b-loan FOR loan.
DEFINE VARIABLE mFIO  AS CHARACTER NO-UNDO.

ETIME(YES).

DEFINE VARIABLE mString     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPasp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOGRN       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtsenka    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskIP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskUL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameUL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINNFL      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINNUL      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSkip       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE  tt-cli
   FIELD cust-cat  AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD inn       AS CHARACTER.

DEFINE VARIABLE mTxt        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDataID     AS INT64      NO-UNDO.

mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="find-sl.xml" }

{empty tt-cli}

FOR EACH DataLine WHERE TRUE
   AND DataLine.Data-ID EQ mDataID
   AND DataLine.Txt     EQ "PNU"
   NO-LOCK:

   /*cust-corp*/
   FIND FIRST cust-corp WHERE TRUE
      AND CAN-DO("*",cust-corp.country-id)
      AND cust-corp.inn EQ DataLine.Sym3
   NO-LOCK NO-ERROR.

   IF AVAIL(cust-corp) THEN
   DO:
      FIND FIRST tt-cli WHERE TRUE
         AND tt-cli.cust-id  EQ cust-corp.cust-id
         AND tt-cli.cust-cat EQ "Ю"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-cli) THEN
      DO:
         CREATE tt-cli.
         ASSIGN
            tt-cli.cust-id  = cust-corp.cust-id
            tt-cli.cust-cat = "Ю"
            tt-cli.inn      = DataLine.Sym3.
      END.
   END.
   /*person*/   
   FIND FIRST person WHERE TRUE
      AND CAN-DO("*",person.country-id)
      AND person.inn EQ DataLine.Sym3
   NO-LOCK NO-ERROR.
   
   IF AVAIL(person) THEN
   DO:
      FIND FIRST tt-cli WHERE TRUE
         AND tt-cli.cust-id  EQ cust-corp.cust-id
         AND tt-cli.cust-cat EQ "Ч"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-cli) THEN
      DO:
         CREATE tt-cli.
         ASSIGN
            tt-cli.cust-id  = person.person-id
            tt-cli.cust-cat = "Ч"
            tt-cli.inn      = DataLine.Sym3.
      END.
   END.
END.

mInt = 0.
FOR EACH tt-cli 
   NO-LOCK:
      
   FOR EACH DataLine WHERE TRUE
      AND DataLine.Data-ID EQ mDataID
      AND DataLine.Sym3    EQ tt-cli.inn
      NO-LOCK:
      LEAVE.
   END.
/*   PUT UNFORMATTED       */
/*      tt-cli.cust-cat ";"*/
/*      tt-cli.cust-id  ";"*/
/*      tt-cli.inn      ";"*/
/*      AVAIL(DataLine)    */
/*   SKIP.                 */
END.

{find-match-sl.hdr}

mInt = 0.
FOR EACH tt-cli 
   NO-LOCK:
      
/*   RELEASE acct.                            */
/*   FOR EACH acct WHERE TRUE                 */
/*      AND acct.cust-cat   EQ tt-cli.cust-cat*/
/*      AND acct.cust-id    EQ tt-cli.cust-id */
/*      AND acct.acct-cat   EQ "b"            */
/*      AND acct.close-date EQ ?              */
/*      NO-LOCK:                              */
/*      LEAVE.                                */
/*   END.                                     */
/*   IF AVAIL(acct) THEN                      */
/*   DO:                                      */
      mOtsenka = "".
      mRiskIP  = "".
      mRiskFL  = "".
      mRiskUL  = "".
      mNameFL  = "".
      mNameUL  = "".
      mINNFL   = "".
      mINNUL   = "".
      mSkip    = NO.
      IF tt-cli.cust-cat EQ "Ч" THEN
      DO:
         FOR EACH tmpsigns WHERE TRUE
      	   AND tmpsigns.file-name  EQ "person"
      	   AND tmpsigns.surrogate  EQ STRING(tt-cli.cust-id)
      	   NO-LOCK BY tmpsigns.since:
            IF tmpsigns.code EQ "ОценкаРиска" THEN mOtsenka = tmpsigns.xattr-value.
            IF tmpsigns.code EQ "РискОтмывИП" THEN mRiskIP  = tmpsigns.code-value.
            IF tmpsigns.code EQ "РискОтмыв"   THEN mRiskFL  = tmpsigns.code-value.
      	END.
         IF INDEX(mOtsenka,"550-П") NE 0
   	      AND mRiskFL EQ "высокий" THEN mSkip = YES.
   	   FIND FIRST person WHERE TRUE
            AND person.person-id EQ tt-cli.cust-id
         NO-LOCK NO-ERROR.
         IF AVAIL(person) THEN
         ASSIGN 
   	      mNameFL = person.name-last + " " + person.first-names
   	      mINNFL  = person.inn.
   	END.
      IF tt-cli.cust-cat EQ "Ю" THEN
      DO:
         FOR EACH tmpsigns WHERE TRUE
      	   AND tmpsigns.file-name  EQ "cust-corp"
      	   AND tmpsigns.surrogate  EQ STRING(tt-cli.cust-id)
      	   NO-LOCK BY tmpsigns.since:
            IF tmpsigns.code EQ "ОценкаРиска" THEN mOtsenka = tmpsigns.xattr-value.
            IF tmpsigns.code EQ "РискОтмыв"   THEN mRiskUL  = tmpsigns.code-value.
      	END.
      	IF INDEX(mOtsenka,"550-П") NE 0
         	AND mRiskUL EQ "высокий" THEN mSkip = YES.
         FIND FIRST cust-corp WHERE TRUE
            AND cust-corp.cust-id EQ tt-cli.cust-id
         NO-LOCK NO-ERROR.
          
         IF AVAIL(cust-corp) THEN
         ASSIGN 
   	      mNameUL = cust-corp.cust-stat + " " + cust-corp.name-corp
   	      mINNUL  = cust-corp.inn.
   	   
      END.
      mCustID = "".
      mINN    = "".
   	IF mSkip EQ NO THEN
      DO:
         /*№	Наименование ЮЛ /ФИО ИП,ФЛ	ИНН	Код клиента в БИС */
         mInt    = mInt + 1.
         mName   = IF tt-cli.cust-cat EQ "Ю" THEN mNameUL ELSE mNameFL.
         mCustID = STRING(tt-cli.cust-id).
         mINN    = IF tt-cli.cust-cat EQ "Ю" THEN mINNUL  ELSE mINNFL.
         PUT UNFORMATTED
            '   <Row>~n' +
            '    <Cell><Data ss:Type="Number">' + STRING(mInt)  + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mName)   + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mINN)    + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mCustID) + '</Data></Cell>~n' +
            '   </Row>~n'.
      END.
/*   END.*/
END.

{find-match-sl.ftr}

/*{preview2.i &filename="find-sl.xml" }*/

OUTPUT CLOSE.

RUN sndbispc ("file=" + "find-sl.xml" + ";class=bq").

{intrface.del}

RETURN.
