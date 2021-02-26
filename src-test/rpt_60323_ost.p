{globals.i}
{client.i}
{sh-defs.i}
DEFINE VARIABLE mDateS    AS DATE INITIAL TODAY NO-UNDO.
DEFINE VARIABLE mFilial   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKD       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFname    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst60323    AS DECIMAL NO-UNDO.
DEFINE VARIABLE mOst40817    AS DECIMAL NO-UNDO.

DEFINE BUFFER bloan-acct FOR loan-acct.
DEFINE STREAM kag.

mFname = "./rpt_60323_ost_" + replace(string(TODAY,"99.99.9999"),".","_") + "_" +  "_" + USERID('bisquit') + ".xml".   

OUTPUT STREAM kag TO VALUE (mFname)
   UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".


DEFINE TEMP-TABLE ttRep NO-UNDO 
   FIELD name AS CHARACTER 
   FORMAT "x(40)" 
   LABEL "Š«¨¥­â"
   FIELD kd AS CHARACTER 
   FORMAT "x(25)" 
   LABEL "®¬¥à ¤®£®¢®à "
   FIELD sum60323  AS DECIMAL
   FORMAT "->>>,>>>,>>>,>>9.99"
   LABEL "‘ã¬¬  60323"
   FIELD sum40817  AS DECIMAL
   FORMAT "->>>,>>>,>>>,>>9.99"
   LABEL "‘ã¬¬  ­  à/á".


ASSIGN
   mDateS  = gend-date
   mFilial = shFilial.

DEFINE FRAME iParam 
   mDateS    LABEL "‡  ¤ âã"  FORMAT "99.99.9999"
   mFilial   LABEL "”¨«¨ «"
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ ‡€„€‰’… €€Œ…’› ]".

ON F1 OF mDateS
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN mDateS:SCREEN-VALUE = pick-value.
   END.

ON F1 OF mFilial
   DO:
      DO TRANSACTION:
         RUN browseld.p("branch",
            "branch-type" ,
            "10,11",
            "",
            4).
         mFilial:SCREEN-VALUE = pick-value.
      END. 
   END.

PAUSE 0.
UPDATE 
   mDateS
   mFilial
   WITH FRAME iParam. 

IF LAST-EVENT:FUNCTION  EQ "END-ERROR"
   THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.

{setdest.i}

FOR EACH acct WHERE 
   acct.acct-cat EQ "b"
   AND acct.bal-acct EQ 60323
   AND acct.close-date EQ ? 
   AND acct.filial-id = mFilial
   NO-LOCK,
   FIRST loan-acct WHERE loan-acct.acct EQ acct.acct
   AND loan-acct.currency EQ acct.currency
   AND loan-acct.contract EQ "Šà¥¤¨â"

   NO-LOCK :


   RUN acct-pos IN h_base (
      acct.acct,
      acct.currency,
      mDateS,
      mDateS, 
      ?).

   most60323 = (IF loan-acct.currency = "" THEN sh-bal ELSE sh-val ) .
   /* IF most60323 EQ 0 THEN NEXT. */

   FIND LAST bloan-acct WHERE loan-acct.contract EQ loan-acct.contract
      AND bloan-acct.cont-code EQ loan-acct.cont-code
      AND bloan-acct.acct-type EQ "Šà¥¤ áç"
      NO-LOCK NO-ERROR.
   IF AVAILABLE bloan-acct THEN
      RUN acct-pos IN h_base (
         bloan-acct.acct,
         bloan-acct.currency,
         mDateS,
         mDateS,
         ?).
   ELSE 
      NEXT.

   most40817 = (IF loan-acct.currency = "" THEN sh-bal ELSE sh-val ) .

   /*IF most40817 EQ 0 THEN NEXT.*/
   RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT mCustName).

   mKd = ENTRY(1,loan-acct.cont-code,"@").
   
/*   PUT UNFORMATTED mCustName FORMAT "x(40)" 
      ENTRY(1,loan-acct.cont-code,"@") FORMAT "x(25)"  
      most60323 FORMAT "->>>,>>>,>>>,>>9.99"
      most40817 FORMAT "->>>,>>>,>>>,>>9.99"
      SKIP.
*/
   CREATE ttRep.
   ASSIGN 
      ttRep.name = mCustName
      ttRep.kd = mKd
      ttRep.sum60323 = most60323
      ttRep.sum40817 = most40817.

END.
/*{preview.i}*/

PUT STREAM kag UNFORMATTED {rpt_60323_ost.i1} SKIP.
FOR EACH ttrep:
   PUT STREAM kag UNFORMATTED
      SUBSTITUTE('
   <Row>
    <Cell><Data ss:Type="String">&1</Data></Cell>
    <Cell><Data ss:Type="String">&2</Data></Cell>
    <Cell><Data ss:Type="Number">&3</Data></Cell>
    <Cell><Data ss:Type="Number">&4</Data></Cell>
   </Row>',
      ttRep.name ,
      ttRep.kd,
      ttRep.sum60323,
      ttRep.sum40817) SKIP.
END.
PUT STREAM kag UNFORMATTED {rpt_60323_ost.i2} SKIP.

RUN sndbispc ("file=" + mfname + ";class=bq").


