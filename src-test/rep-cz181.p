/**/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

{intrface.get xclass}

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE iParam1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iParam2 AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mSumm       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mOst        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mPost       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mPostStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt1       AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt2       AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt3       AS INT64     NO-UNDO.
DEFINE VARIABLE mName       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItogo      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mDate       AS DATE      NO-UNDO.
DEFINE VARIABLE mAD         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mLinkId     AS INT64     NO-UNDO.
DEFINE VARIABLE mRAcct      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRCurr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalSumm    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalSummDb  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBalSummCr  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBalExist   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE mBalSummDbAll  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBalSummCrAll  AS DECIMAL   NO-UNDO.

DEFINE VARIABLE mBalList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalSide    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCol01      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol02      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol22      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol03      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol04      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol05      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol06      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol07      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol08      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol09      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCol10      AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCol5       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCol7       AS DECIMAL   NO-UNDO.

DEFINE VARIABLE mLastMove   AS DATE      NO-UNDO.

DEFINE STREAM out-stream.

DEFINE TEMP-TABLE tt-list
   FIELD acct         AS CHAR 
   FIELD currency     AS CHAR
   FIELD details      AS CHAR
   FIELD amt          AS DECIMAL
   FIELD in-ost       AS DECIMAL
   FIELD db-obor      AS DECIMAL
   FIELD cr-obor      AS DECIMAL
   FIELD out-ost      AS DECIMAL
   FIELD lastmove     AS DATE
   FIELD racct        AS CHAR 
   FIELD rcurrency    AS CHAR
   FIELD rout-ost     AS DECIMAL
   FIELD acct62001    AS CHAR 
   FIELD curr62001    AS CHAR
   FIELD ost62001     AS DECIMAL.

DEFINE TEMP-TABLE tt-otch
   FIELD acct         AS CHAR 
   FIELD currency     AS CHAR
   FIELD details      AS CHAR
   FIELD amt          AS DECIMAL
   FIELD in-ost       AS DECIMAL
   FIELD db-obor      AS DECIMAL
   FIELD cr-obor      AS DECIMAL
   FIELD out-ost      AS DECIMAL
   FIELD lastmove     AS DATE
   FIELD racct        AS CHAR 
   FIELD rcurrency    AS CHAR
   FIELD rout-ost     AS DECIMAL
   FIELD acct62001    AS CHAR 
   FIELD curr62001    AS CHAR
   FIELD ost62001     AS DECIMAL
   FIELD ballist      AS CHAR
   FIELD balside      AS CHAR
   FIELD balsumm      AS CHAR
   FIELD needrow      AS LOGICAL.
   
DEFINE TEMP-TABLE tt-entry
   FIELD db-cr        AS CHAR
   FIELD acct         AS CHAR 
   FIELD corr         AS CHAR
   FIELD bal-acct     AS INT64
   FIELD amt-rub      AS DECIMAL
   FIELD op-date      AS DATE.

DEFINE BUFFER tt-entry2 FOR tt-entry.

{empty tt-otch}

{getdates.i}

mBegDate = beg-date.
mEndDate = end-date.

mBegDate = DATE("01/01/2018").
mEndDate = DATE("31/03/2018").

/*mFileName = "./" +                                                             */
/*   STRING(YEAR(TODAY),"9999") + "-" +                                          */
/*   STRING(MONTH(TODAY),"99") + "-" +                                           */
/*   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-cz181.xml".*/
/*OUTPUT STREAM out-stream TO VALUE(mFileName)                                   */
/*       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".                      */

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-cz181.csv".
OUTPUT STREAM out-stream TO VALUE(mFileName) 
   UNBUFFERED CONVERT TARGET "1251" SOURCE "IBM866".       

MESSAGE "Формирование отчета..".

/*iParam1 = "612*".         */
/*iParam2 = "62001*,60312*".*/

iParam1 = TRIM(ENTRY(1,iParam,";")).
iParam2 = TRIM(ENTRY(2,iParam,";")).

/*Ищем счета для счета 2 порядка из параметра*/
mCnt1 = 0.
FOR EACH acct WHERE TRUE
   /*AND acct.filial-id   EQ shFilial*/
   AND acct.acct-cat    EQ "b"
   AND CAN-DO(iParam1,acct.acct)
   /*AND (acct.acct EQ "61212810605960000003     @0500" OR acct.acct EQ "61212810401400000000     @0000")*/
   /*AND acct.acct EQ "61209810300090001138     @0000"*/
   /*AND acct.acct EQ "61209810000090001153     @0000"*/
   /*AND acct.acct EQ "61209810900090001224     @0000"*/
   /*AND acct.acct EQ "61209810000090001166     @0000"*/
   NO-LOCK:
   mCnt1 = mCnt1 + 1.

   CREATE tt-otch.
   ASSIGN
      tt-otch.acct     = acct.acct
      tt-otch.currency = acct.currency
      tt-otch.details  = acct.details.
END.

/*Заполняем дебетовые проводки*/
mCnt2 = 0.
FOR EACH tt-otch WHERE
   NO-LOCK:
   FOR EACH op-entry WHERE
       op-entry.acct-db EQ tt-otch.acct
   AND op-entry.op-date NE ?
   AND op-entry.op-date GE mBegDate
   AND op-entry.op-date LE mEndDate
   NO-LOCK:
   mCnt2 = mCnt2 + 1.
   
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~ndb    = " + 
         "~nop-entry.acct-db = " + STRING(op-entry.acct-db) + 
         "~nop-entry.acct-cr = " + STRING(op-entry.acct-cr) +
         "~nop-entry.op-date = " + STRING(op-entry.op-date,"99/99/9999") +
         "~nop-entry.amt-rub = " + TRIM(STRING(op-entry.amt-rub,"->>>,>>>,>>>,>>9.99"))).
   
   CREATE tt-entry.
   ASSIGN
      tt-entry.db-cr    = "db"
      tt-entry.acct     = op-entry.acct-db
      tt-entry.corr     = op-entry.acct-cr
      tt-entry.bal-acct = INT64(SUBSTRING(op-entry.acct-cr,1,5))
      tt-entry.amt-rub  = op-entry.amt-rub
      tt-entry.op-date  = op-entry.op-date.
   END.
END.

/*Заполняем кредитовые проводки*/
mCnt3 = 0.
FOR EACH tt-otch WHERE
   NO-LOCK,
   EACH op-entry WHERE
       op-entry.acct-cr EQ tt-otch.acct
   AND op-entry.op-date NE ?
   AND op-entry.op-date GE mBegDate
   AND op-entry.op-date LE mEndDate
   NO-LOCK:
   
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~ncr    = " + 
         "~nop-entry.acct-db = " + STRING(op-entry.acct-db) + 
         "~nop-entry.acct-cr = " + STRING(op-entry.acct-cr) +
         "~nop-entry.op-date = " + STRING(op-entry.op-date,"99/99/9999") +
         "~nop-entry.amt-rub = " + TRIM(STRING(op-entry.amt-rub,"->>>,>>>,>>>,>>9.99"))).
      
   mCnt3 = mCnt3 + 1.
   CREATE tt-entry.
   ASSIGN
      tt-entry.db-cr    = "cr"
      tt-entry.acct     = op-entry.acct-cr
      tt-entry.corr     = op-entry.acct-db
      tt-entry.bal-acct = INT64(SUBSTRING(op-entry.acct-db,1,5))
      tt-entry.amt-rub  = op-entry.amt-rub
      tt-entry.op-date  = op-entry.op-date.
END.

/*Предварительный вывод*/

/*{rep-62101-h.i}*/

/*Вычисление списка балансовых счетов с оборотами*/
ASSIGN
   mBalList = ""
   mBalSide = ""
   mBalSumm = "".
   
FOR EACH tt-entry WHERE
   tt-entry.amt-rub NE 0
   NO-LOCK 
   BREAK BY tt-entry.db-cr DESC BY tt-entry.bal-acct:

   IF FIRST-OF(tt-entry.bal-acct) THEN
   DO:
      mSumm = 0.
      FOR EACH tt-entry2 WHERE
             tt-entry2.bal-acct EQ tt-entry.bal-acct
         AND CAN-DO(iParam2,TRIM(STRING(tt-entry.bal-acct)))
         AND tt-entry2.db-cr    EQ tt-entry.db-cr
         AND tt-entry2.amt-rub  NE 0
         NO-LOCK:
         mSumm = mSumm + tt-entry2.amt-rub.
      END.

      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~ntt-entry.db-cr    = " + tt-entry.db-cr + 
         "~ntt-entry.bal-acct = " + STRING(tt-entry.bal-acct) + 
         "~nmSumm             = " + TRIM(STRING(mSumm,"->>>,>>>,>>>,>>9.99"))).

      IF mSumm NE 0 
      AND CAN-DO(iParam2,TRIM(STRING(tt-entry.bal-acct))) THEN
      DO:
         ASSIGN
            mBalList = mBalList + "," + TRIM(STRING(tt-entry.bal-acct))
            mBalSide = mBalSide + "," + tt-entry.db-cr.
      END.
   END.
END.

mBalList = TRIM(mBalList,",").
mBalSide = TRIM(mBalSide,",").

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
   "~nmBalList    = " + TRIM(mBalList,",") + 
   "~nmBalSide    = " + TRIM(mBalSide,",") + 
   "~nСчетов      = " + TRIM(STRING(mCnt1)) +
   "~nдб проводок = " + TRIM(STRING(mCnt2)) +
   "~nкр проводок = " + TRIM(STRING(mCnt3))).

DO mCnt = 1 TO NUM-ENTRIES(mBalList):
   FOR EACH tt-entry WHERE TRUE
      AND TRIM(STRING(tt-entry.bal-acct)) EQ ENTRY(mCnt,mBalList)
      AND tt-entry.db-cr                  EQ ENTRY(mCnt,mBalSide)
      NO-LOCK,
      FIRST tt-otch WHERE
      tt-otch.acct EQ tt-entry.acct
      EXCLUSIVE-LOCK:
      tt-otch.needrow = YES.
   END.
END.

/*FOR EACH tt-otch                    */
/*   NO-LOCK BREAK BY tt-otch.acct:   */
/*                                    */
/*   IF FIRST-OF(tt-otch.acct) THEN   */
/*   PUT STREAM out-stream UNFORMATTED*/
/*      tt-otch.acct      ";"         */
/*      tt-otch.needrow   ";"         */
/*   SKIP.                            */
/*END.                                */

/*
Лицевой счет	
Наименование счета	
Дата	
Обороты по дебету 62101
ВСЕГО			
Обороты по кредиту 60311	
Обороты по кредиту 70606
ВСЕГО		
*/

IF NOT {assigned mBalList} THEN
DO:
   MESSAGE "Нет данных"
   VIEW-AS ALERT-BOX.
   OUTPUT STREAM out-stream CLOSE.
   MESSAGE "".
   RETURN.
END.

/*Шапка*/
PUT STREAM out-stream UNFORMATTED "Лицевой счет;Наименование счета;".
DO mCnt = 1 TO NUM-ENTRIES(mBalList):
   IF ENTRY(mCnt,mBalSide) EQ "db" THEN 
   DO:
      PUT STREAM out-stream UNFORMATTED "Обороты по дебету "  + ENTRY(mCnt,mBalList) ";".
      
   END.
   IF ENTRY(mCnt,mBalSide) EQ "cr" THEN PUT STREAM out-stream UNFORMATTED "Обороты по кредиту " + ENTRY(mCnt,mBalList) ";".
END.
PUT STREAM out-stream UNFORMATTED "Всего по дебету;Всего по кредиту;".
PUT STREAM out-stream UNFORMATTED SKIP.

/*Строки отчета*/
FOR EACH tt-otch WHERE
   tt-otch.needrow EQ YES
   NO-LOCK BREAK BY tt-otch.acct:

   IF FIRST-OF(tt-otch.acct) THEN
   DO:
      mBalSummDbAll = 0.
      mBalSummCrAll = 0.

      PUT STREAM out-stream UNFORMATTED 
         tt-otch.acct ";"
         REPLACE(REPLACE(tt-otch.details,CHR(10),""),CHR(13),"") ";" 
      SKIP.
   
      FOR EACH tt-entry WHERE TRUE
         AND tt-entry.acct     EQ tt-otch.acct
         NO-LOCK BREAK BY tt-entry.op-date:
         
         IF FIRST-OF(tt-entry.op-date) THEN
         DO:
            PUT STREAM out-stream UNFORMATTED ";"tt-entry.op-date ";".
            mBalSummDbAll = 0.
            mBalSummCrAll = 0.
            DO mCnt = 1 TO NUM-ENTRIES(mBalList):
               mBalSummDb = 0.
               mBalSummCr = 0.
               FOR EACH tt-entry2 WHERE TRUE
                  AND tt-entry2.bal-acct EQ INT64(ENTRY(mCnt,mBalList)) 
                  AND tt-entry2.op-date  EQ tt-entry.op-date
                  AND tt-entry2.db-cr EQ ENTRY(mCnt,mBalSide)
                  NO-LOCK:
                  IF ENTRY(mCnt,mBalSide) EQ "db" THEN mBalSummDb = mBalSummDb + tt-entry2.amt-rub.
                  IF ENTRY(mCnt,mBalSide) EQ "cr" THEN mBalSummCr = mBalSummCr + tt-entry2.amt-rub.
                  IF ENTRY(mCnt,mBalSide) EQ "db" THEN mBalSummDbAll = mBalSummDbAll + tt-entry2.amt-rub.
                  IF ENTRY(mCnt,mBalSide) EQ "cr" THEN mBalSummCrAll = mBalSummCrAll + tt-entry2.amt-rub.
               END.
            	IF ENTRY(mCnt,mBalSide) EQ "db" THEN PUT STREAM out-stream UNFORMATTED TRIM(STRING(mBalSummDb,">>>,>>>,>>>,>>9.99")) ";".
            	IF ENTRY(mCnt,mBalSide) EQ "cr" THEN PUT STREAM out-stream UNFORMATTED TRIM(STRING(mBalSummCr,">>>,>>>,>>>,>>9.99")) ";".
            END.
            PUT STREAM out-stream UNFORMATTED TRIM(STRING(mBalSummDbAll,">>>,>>>,>>>,>>9.99")) ";".
            PUT STREAM out-stream UNFORMATTED TRIM(STRING(mBalSummCrAll,">>>,>>>,>>>,>>9.99")) ";".
         END.
         IF LAST-OF(tt-entry.op-date) THEN PUT STREAM out-stream UNFORMATTED SKIP.
      END.
   END.
END.

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

MESSAGE "".

RETURN.
