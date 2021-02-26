/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: rep-kasko.p
      Comment: Отчет по договорах страхования для Доценко
   Parameters:
         Uses:
      Used by:
      Created: 11/03/13
     Modified: 11/03/13 Serge
*/

{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{intrface.get lngar}
{intrface.get chwch}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{client.i}
{lshpr.pro}
{tmprecid.def}
{wordwrap.def}
{svarloan.def}
{navigate.def}

{loan_par.def &new = new} 
{flt-file.i}
{sh-defs.i}


def new shared stream vvs.
def var fname as char init "./rep-kasko.p.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10).

DEF BUFFER b-loan      FOR loan.
DEF BUFFER s-loan      FOR loan.
DEF BUFFER loan-acct   FOR loan-acct.
DEF BUFFER bperson     FOR person.
DEF BUFFER bcust-corp  FOR cust-corp.

DEF VAR cname AS CHAR NO-UNDO.
DEF VAR cdetails AS CHAR NO-UNDO.
DEF VAR cont-code AS CHAR NO-UNDO.
DEF VAR doc-ref AS CHAR NO-UNDO.
DEF VAR g AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE Kasko
	FIELD cont-code AS CHARACTER
	FIELD doc-ref   AS CHARACTER
	FIELD cust-id   AS INTEGER
	FIELD open-date AS DATE
	FIELD close-date  AS DATE
	FIELD end-date  AS DATE
	FIELD cdetails  AS CHARACTER
	FIELD cname     AS CHARACTER
.

DEFINE VARIABLE mCred  AS LOGICAL NO-UNDO.

{getdates.i}

/*beg-date = DATE("01/09/2016").*/
/*end-date = DATE("15/09/2016").*/

OUTPUT TO VALUE (fname) UNBUFFERED CONVERT TARGET "1251".

FOR EACH b-loan WHERE 
        b-loan.contract   EQ "Кредит"
   AND CAN-DO("*ПК*",b-loan.cont-code)
/*   AND CAN-DO("45-00-10062-АПК*",b-loan.cont-code)*/
   AND  b-loan.end-date   GE end-date
   AND (b-loan.close-date GE end-date OR b-loan.close-date EQ ?)
   AND  b-loan.filial-id EQ shFilial
   NO-LOCK,
   EACH s-loan WHERE
        s-loan.parent-contract  EQ b-loan.contract
   AND  s-loan.parent-cont-code EQ b-loan.cont-code
   AND  s-loan.class-code       EQ "insurance"
   AND  s-loan.contract         EQ "СТРАХ"
   AND  s-loan.filial-id        EQ shFilial
   AND  s-loan.end-date         GE DATE("01/07/2014")
   AND  DAY(s-loan.end-date)    GE DAY(beg-date)
   AND  MONTH(s-loan.end-date)  GE MONTH(beg-date)
   AND  DAY(s-loan.end-date)    LE DAY(end-date)
   AND  MONTH(s-loan.end-date)  LE MONTH(end-date)
   NO-LOCK BREAK BY b-loan.cont-code:
   
   mCred = NO.   
   
   IF  FIRST-OF(b-loan.cont-code) THEN
   DO:
/*      IF (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2014) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2014))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2015) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2015))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2016) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2016))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2017) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2017))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2018) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2018))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2019) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2019))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2020) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2020))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2021) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2021)) THEN*/
/*      DO:                                                                                                                                  */
         mCred = YES.
         PUT UNFORMATTED
            "Кредитный договор;"
            b-loan.cont-code ";"
            b-loan.open-date ";"
            b-loan.end-date ";"
            b-loan.close-date ";"
         SKIP.
/*      END.*/
   END.
      
   IF 1 EQ 1 THEN
   DO:
/*      IF (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2014) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2014))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2015) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2015))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2016) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2016))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2017) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2017))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2018) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2018))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2019) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2019))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2020) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2020))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2021) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2021)) THEN*/
/*      DO:                                                                                                                                  */
         IF s-loan.open-date LT DATE(02,20,2013) OR
            GetXAttrValueEx("loan",STRING("СТРАХ," + s-loan.cont-code), "VidStr", "") begins "КАСКО" THEN
         DO:
            PUT UNFORMATTED
               "Договор страхования " (IF s-loan.end-date LT TODAY THEN "законч." ELSE "действ.") ";"
               s-loan.cont-code ";"
               s-loan.open-date ";"
               s-loan.end-date ";"
               s-loan.close-date ";"
               GetXAttrValueEx("loan",STRING("СТРАХ," + s-loan.cont-code),"VidStr","")
            SKIP.
         END.
/*      END.*/
   END.
   
   IF LAST-OF(b-loan.cont-code) THEN
   DO:
/*      IF (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2014) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2014))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2015) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2015))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2016) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2016))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2017) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2017))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2018) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2018))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2019) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2019))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2020) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2020))     */
/*      OR (s-loan.end-date GE DATE(MONTH(beg-date),DAY(beg-date),2021) AND s-loan.end-date LE DATE(MONTH(end-date),DAY(end-date),2021)) THEN*/
/*      DO:                                                                                                                                  */
         FIND LAST loan-acct WHERE
              loan-acct.contract  EQ b-loan.contract
         AND  loan-acct.cont-code EQ b-loan.cont-code
         AND  loan-acct.since     LE b-loan.since
         AND  loan-acct.acct-type EQ "КредРасч"
         NO-LOCK NO-ERROR.
         IF AVAIL(loan-acct) THEN
         DO:
            FOR EACH op-entry WHERE
                op-entry.acct-db EQ loan-acct.acct
            AND op-entry.kau-cr  EQ b-loan.contract + "," + b-loan.cont-code + "," + "516"
            NO-LOCK:
               PUT UNFORMATTED
                  ";Документ;"
                  op-entry.op-date ";"
                  op-entry.amt-rub ";"
               SKIP.
            END.
         END.
/*      END.*/
   END.
END.

OUTPUT CLOSE.

/*{preview.i &filename = "'rep-kasko.p.csv'"}*/

{intrface.del}
