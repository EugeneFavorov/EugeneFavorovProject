/**/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

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
DEFINE VARIABLE mLastMove   AS DATE      NO-UNDO.
DEFINE VARIABLE mDateStr    AS CHARACTER NO-UNDO.

DEFINE STREAM out-stream.

DEFINE TEMP-TABLE tt-otch
   FIELD acct         AS CHAR 
   FIELD details      AS CHAR
   FIELD amt          AS DECIMAL
   FIELD in-ost       AS DECIMAL
   FIELD db-obor      AS DECIMAL
   FIELD cr-obor      AS DECIMAL
   FIELD out-ost      AS DECIMAL
   FIELD lastmove     AS DATE.

{empty tt-otch}

{getdate.i}

mBegDate = beg-date.
mEndDate = end-date.

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-61011.xml".

OUTPUT STREAM out-stream TO VALUE(mFileName) 
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

MESSAGE "Формирование отчета..".

FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND acct.bal-acct    EQ 61011
   NO-LOCK:
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mEndDate,mEndDate,CHR(251)).
   
   IF lastmove EQ DATE("16/11/2014") 
   OR lastmove EQ ? THEN
   FOR LAST bank.post-mfr WHERE
          bank.post-mfr.debacc   EQ acct.number
      AND bank.post-mfr.postdate LT DATE("01/01/2015")
      NO-LOCK:
      mLastMove = bank.post-mfr.postdate.
   END.
   ELSE
      mLastMove = lastmove.

   CREATE tt-otch.
   ASSIGN
      tt-otch.acct     = acct.acct
      tt-otch.details  = acct.details
      tt-otch.in-ost   = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal
      tt-otch.lastmove = mLastMove.
END.

{rep-61011-h.i}

ASSIGN
   mCnt   = 0.

FOR EACH tt-otch WHERE
   NO-LOCK:
      
   IF  tt-otch.in-ost  EQ 0
   AND tt-otch.db-obor EQ 0
   AND tt-otch.cr-obor EQ 0
   AND tt-otch.out-ost EQ 0 THEN NEXT.

   mDateStr = IF tt-otch.lastmove EQ ? THEN 'Не определена' ELSE STRING(tt-otch.lastmove,"99/99/9999").
      
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-otch.details + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + DelFilFromAcct(tt-otch.acct) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="' + (IF tt-otch.out-ost GE 0 THEN 's73' ELSE 's75') + '"><Data ss:Type="Number">' + TRIM(STRING(ABS(tt-otch.out-ost),"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + mDateStr + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

{rep-61011-f.i}

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
