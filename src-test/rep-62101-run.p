/*

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

{intrface.get xclass}

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mSumm       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mPost       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mPostStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mName       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItogo      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mDate       AS DATE      NO-UNDO.
DEFINE VARIABLE mAD         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mLinkId     AS INT64     NO-UNDO.
DEFINE VARIABLE mRAcct      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRCurr      AS CHARACTER NO-UNDO.

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
   FIELD ost62001     AS DECIMAL.
   
DEFINE TEMP-TABLE tt-entry
   FIELD db-cr        AS CHAR
   FIELD acct         AS CHAR 
   FIELD amt-rub      AS DECIMAL
   FIELD op-date      AS DATE.

{empty tt-otch}

{getdates.i}

mBegDate = beg-date.
mEndDate = end-date.

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-62101.xml".
OUTPUT STREAM out-stream TO VALUE(mFileName) 
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

MESSAGE "Формирование отчета..".

/*Ищем счета 61011 и остатки по ним*/
IF CAN-DO(iParam,"61011") THEN
FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND acct.bal-acct    EQ 61011
/*   AND acct.acct        EQ "61011810000090000012     @0000"*/
/*   AND acct.acct        EQ "61011810100900000112     @0000"*/
   NO-LOCK:

   mLastMove = ?.
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mEndDate,CHR(251)).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.acct     = acct.acct
      tt-otch.currency = acct.currency
      tt-otch.details  = acct.details
      tt-otch.in-ost   = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal.

   IF sh-in-bal NE 0 THEN
   DO:
      RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mBegDate,CHR(251)).
   
      IF lastmove EQ DATE("16/11/2014") 
      OR lastmove EQ ? THEN
      FOR LAST bank.post-mfr WHERE
             bank.post-mfr.debacc   EQ acct.number
         AND bank.post-mfr.postdate LT DATE("01/01/2015")
         NO-LOCK:
         mLastMove = bank.post-mfr.postdate.
         tt-otch.lastmove = mLastMove.
      END.
      ELSE
         mLastMove = lastmove.
         tt-otch.lastmove = mLastMove.
   END.
   ELSE tt-otch.lastmove = ?.
   
   FIND FIRST op-entry WHERE
       op-entry.op-date   EQ DATE("01/01/2016")
   AND op-entry.filial-id EQ "0000" 
   AND op-entry.acct-cr   EQ acct.acct
   NO-LOCK NO-ERROR.
   IF AVAIL(op-entry) THEN
      tt-otch.acct62001 = op-entry.acct-db.
END.
/*Остатки на 620101 после переноса*/
FOR EACH tt-otch NO-LOCK:
   {find-act.i
      &acct = tt-otch.acct62001
   }
   
   IF AVAIL(acct) THEN
   DO:
      RUN acct-pos IN h_base(acct.acct,acct.currency,mEndDate,mEndDate,CHR(251)).
      
      ASSIGN 
         tt-otch.ost62001  = sh-bal.
   END.  
END.

/*Ищем счета 62101 и остатки по ним*/
IF CAN-DO(iParam,"62101") THEN
FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND acct.bal-acct    EQ 62101
/*   AND acct.acct        EQ "62101810900090001075     @0000"*/
   NO-LOCK:
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mEndDate,CHR(251)).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.acct     = acct.acct
      tt-otch.currency = acct.currency
      tt-otch.details  = acct.details
      tt-otch.in-ost   = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal.
END.

/*
/*Ищем счета 62001 и остатки по ним*/
IF CAN-DO(iParam,"62001") THEN
FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND acct.bal-acct    EQ 62001
   /*AND acct.acct        EQ "62001810400090001316     @0000"*/
   NO-LOCK:
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mEndDate,CHR(251)).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.acct     = acct.acct
      tt-otch.currency = acct.currency
      tt-otch.details  = acct.details
      tt-otch.in-ost   = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal.
END.

/*Заполняем дебетовые проводки для 62001*/
FOR EACH tt-otch WHERE
      tt-otch.acct BEGINS "62001"
   NO-LOCK:
   FOR EACH op-entry WHERE
          op-entry.acct-db EQ tt-otch.acct
      AND op-entry.op-date NE ?
      AND op-entry.op-date EQ DATE("22/01/2016")
      NO-LOCK:
      CREATE tt-entry.
      ASSIGN
         tt-entry.db-cr   = "db"
         tt-entry.acct    = op-entry.acct-db
         tt-entry.amt-rub = op-entry.amt-rub
         tt-entry.op-date = op-entry.op-date.
   END.
   FOR EACH op-entry WHERE
          op-entry.acct-db EQ tt-otch.acct
      AND op-entry.op-date NE ?
      AND op-entry.op-date EQ DATE("29/02/2016")
      NO-LOCK:
      CREATE tt-entry.
      ASSIGN
         tt-entry.db-cr   = "db"
         tt-entry.acct    = op-entry.acct-db
         tt-entry.amt-rub = op-entry.amt-rub
         tt-entry.op-date = op-entry.op-date.
   END.
   FOR EACH op-entry WHERE
          op-entry.acct-db EQ tt-otch.acct
      AND op-entry.op-date NE ?
      AND op-entry.op-date EQ DATE("31/03/2016")
      NO-LOCK:
      CREATE tt-entry.
      ASSIGN
         tt-entry.db-cr   = "db"
         tt-entry.acct    = op-entry.acct-db
         tt-entry.amt-rub = op-entry.amt-rub
         tt-entry.op-date = op-entry.op-date.
   END.
END.
*/

/*Заполняем дебетовые проводки*/
FOR EACH tt-otch WHERE
/*      tt-otch.acct BEGINS "61011"*/
/*   OR tt-otch.acct BEGINS "62101"*/
   NO-LOCK:
   FOR EACH op-entry WHERE
          op-entry.acct-db EQ tt-otch.acct
      AND op-entry.op-date NE ?
      AND op-entry.op-date GE mBegDate
      AND op-entry.op-date LE mEndDate
      NO-LOCK:
      CREATE tt-entry.
      ASSIGN
         tt-entry.db-cr   = "db"
         tt-entry.acct    = op-entry.acct-db
         tt-entry.amt-rub = op-entry.amt-rub
         tt-entry.op-date = op-entry.op-date.
   END.
END.

/*Заполняем кредитовые проводки*/
FOR EACH tt-otch WHERE
/*      tt-otch.acct BEGINS "61011"*/
/*   OR tt-otch.acct BEGINS "62101"*/
   NO-LOCK:
   FOR EACH op-entry WHERE
          op-entry.acct-cr EQ tt-otch.acct
      AND op-entry.op-date NE ?
      AND op-entry.op-date GE mBegDate
      AND op-entry.op-date LE mEndDate
      NO-LOCK:
         
      IF op-entry.acct-cr BEGINS "61011"
      AND op-entry.op-date EQ DATE("01/01/2016")
      THEN NEXT.
      
      CREATE tt-entry.
      ASSIGN
         tt-entry.db-cr   = "cr"
         tt-entry.acct    = op-entry.acct-cr
         tt-entry.amt-rub = op-entry.amt-rub
         tt-entry.op-date = op-entry.op-date.
   END.
END.

/*Ищем счет резерва по link и его остаток*/
FOR EACH tt-otch NO-LOCK:
      
   mLinkId = GetXLinkID("acct","acct-reserve").
         
   FIND FIRST links WHERE
       links.link-id   EQ mLinkId
   AND links.source-id EQ tt-otch.acct + ',' + tt-otch.currency
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF AVAIL(links) THEN
   DO:
      mRAcct = ENTRY(1,links.target-id).
      mRCurr = ENTRY(2,links.target-id).
      
      {find-act.i
         &acct = mRAcct
         &currency = mRCurr  
      }
      
      IF AVAIL(acct) THEN
      DO:
         RUN acct-pos IN h_base(acct.acct,acct.currency,mEndDate,mEndDate,CHR(251)).
         
         ASSIGN 
            tt-otch.racct     = acct.acct
            tt-otch.rcurrency = acct.currency
            tt-otch.rout-ost  = sh-bal.
      END.  
   END.   
END.

/*Предварительный вывод*/

{rep-62101-h.i}

FOR EACH tt-otch NO-LOCK:

   ASSIGN
      mCol01 = tt-otch.details
      mCol02 = DelFilFromAcct(tt-otch.acct)
      mCol22 = DelFilFromAcct(tt-otch.acct62001)
      mCol03 = TRIM(STRING(ABS(tt-otch.in-ost),"->>>>>>>>>>>>>>>>>>>9.99"))
      mCol04 = IF tt-otch.lastmove NE ? THEN STRING(tt-otch.lastmove,"99/99/9999") ELSE ""
      mCol5   = 0
      mCol05  = ""
      mCol06  = ""
      mCol7   = 0
      mCol07  = ""
      mCol08  = "".

   IF tt-otch.acct      BEGINS("61011")
   AND tt-otch.out-ost  EQ 0
   AND tt-otch.ost62001 NE 0
   THEN mCol09 = TRIM(STRING(ABS(tt-otch.ost62001),"->>>>>>>>>>>>>>>>>>>9.99")).
   ELSE mCol09 = TRIM(STRING(ABS(tt-otch.out-ost), "->>>>>>>>>>>>>>>>>>>9.99")).

   mCol10 = TRIM(STRING(ABS(tt-otch.rout-ost),"->>>>>>>>>>>>>>>>>>>9.99")).
         
   FOR EACH tt-entry WHERE
       tt-entry.acct EQ tt-otch.acct  
   NO-LOCK BREAK BY tt-entry.acct BY tt-entry.op-date:
   
      IF  FIRST-OF(tt-entry.acct)
      AND FIRST-OF(tt-entry.op-date) THEN
      DO:
         mCol5   = 0.
         mCol05  = "".
         mCol06  = "".
         mCol7   = 0.
         mCol07  = "".
         mCol08  = "".
      END.
      
      IF tt-entry.db-cr EQ "db" THEN
      ASSIGN
         mCol5  = mCol5 + tt-entry.amt-rub
         mCol06 = STRING(tt-entry.op-date,"99/99/9999").  
      IF tt-entry.db-cr EQ "cr" THEN
      ASSIGN
         mCol7  = mCol7 + tt-entry.amt-rub
         mCol08 = STRING(tt-entry.op-date,"99/99/9999").
      
      IF  LAST-OF(tt-entry.acct)
      AND LAST-OF(tt-entry.op-date) THEN
      DO:
         mCol05 = TRIM(STRING(mCol5,"->>>>>>>>>>>>>>>>>>>9.99")).
         mCol07 = TRIM(STRING(mCol7,"->>>>>>>>>>>>>>>>>>>9.99")).
      END. 
   END.
   
   /*Расчет реализации*/
   mCol7  = DECIMAL(mCol09) - DECIMAL(mCol03) - DECIMAL(mCol05). 
   mCol07 = TRIM(STRING(ABS(mCol7),"->>>>>>>>>>>>>>>>>>>9.99")).
   
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mCol01 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mCol02 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mCol22 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + mCol03 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mCol04 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + mCol05 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mCol06 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + mCol07 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mCol08 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + mCol09 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + mCol10 + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
      
END.

{rep-62101-f.i}

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
