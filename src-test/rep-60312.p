/**/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iAgent AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE mOpOp       AS INT64     NO-UNDO.
DEFINE VARIABLE mUserID     AS CHARACTER NO-UNDO.

DEFINE STREAM out-stream.

DEFINE TEMP-TABLE tt-otch
   FIELD acct         AS CHAR 
   FIELD details      AS CHAR
   FIELD amt          AS DECIMAL
   FIELD in-ost       AS DECIMAL
   FIELD db-obor      AS DECIMAL
   FIELD cr-obor      AS DECIMAL
   FIELD out-ost      AS DECIMAL
   FIELD usr-id       AS CHAR.

{empty tt-otch}

{getdates.i}

mBegDate = beg-date.
mEndDate = end-date.

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-60312.xml".
OUTPUT STREAM out-stream TO VALUE(mFileName) 
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

MESSAGE "Формирование отчета..".

IF iAgent NE "АД" THEN
FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND acct.bal-acct    EQ 60312
   NO-LOCK:
      
   mAD = GetXattrValueEx("acct",acct.acct + "," + acct.curr,"agent-dog","Нет") EQ "Да".

   IF mAD THEN NEXT.   
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mEndDate,CHR(251)).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.acct    = acct.acct
      tt-otch.details = acct.details
      tt-otch.in-ost  = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal.
   
   mOpOp = 0.
   mUserID = "".
   FOR EACH op-entry WHERE TRUE
      AND op-entry.op-date GE mBegDate
      AND op-entry.op-date LE mEndDate
      AND (op-entry.acct-db EQ acct.acct OR op-entry.acct-cr EQ acct.acct)
      NO-LOCK BY op-entry.op-date:
         
      IF op-entry.op GT mOpOp THEN
      ASSIGN
         mOpOp    = op-entry.op
         mUserID = op-entry.user-id.
   END.
   FIND FIRST _user WHERE
      _user._Userid EQ mUserID
   NO-LOCK NO-ERROR.
   IF AVAIL(_user)
         THEN mUserID = STRING(_user._User-Name).
   ASSIGN
      tt-otch.usr-id = mUserID.
END.
ELSE IF iAgent EQ "АД" THEN
FOR EACH acct WHERE
       acct.filial-id   EQ "0000"
   AND acct.acct-cat    EQ "b"
   AND (acct.bal-acct   EQ 60305
    OR  acct.bal-acct   EQ 60312)
   NO-LOCK:
      
   mAD = GetXattrValueEx("acct",acct.acct + "," + acct.curr,"agent-dog","Нет") EQ "Да".

   IF NOT mAD THEN NEXT.   
      
   RUN acct-pos IN h_base(acct.acct,acct.currency,mBegDate,mEndDate,CHR(251)).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.acct    = acct.acct
      tt-otch.details = acct.details
      tt-otch.in-ost  = sh-in-bal
      tt-otch.db-obor  = sh-db
      tt-otch.cr-obor  = sh-cr
      tt-otch.out-ost  = sh-bal.
      
   mOpOp = 0.
   mUserID = "".
   FOR EACH op-entry WHERE TRUE
      AND op-entry.op-date GE mBegDate
      AND op-entry.op-date LE mEndDate
      AND (op-entry.acct-db EQ acct.acct OR op-entry.acct-cr EQ acct.acct)
      NO-LOCK BY op-entry.op-date:
         
      IF op-entry.op GT mOpOp THEN
      ASSIGN
         mOpOp    = op-entry.op
         mUserID = op-entry.user-id.
   END.
   FIND FIRST _user WHERE
      _user._Userid EQ mUserID
   NO-LOCK NO-ERROR.
   IF AVAIL(_user)
         THEN mUserID = STRING(_user._User-Name).
   ASSIGN
      tt-otch.usr-id = mUserID.      
END.

{rep-60312-h.i}

ASSIGN
   mCnt   = 0.

FOR EACH tt-otch WHERE
   NO-LOCK:
      
   IF  tt-otch.in-ost  EQ 0
   AND tt-otch.db-obor EQ 0
   AND tt-otch.cr-obor EQ 0
   AND tt-otch.out-ost EQ 0 THEN NEXT.
      
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-otch.details + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + DelFilFromAcct(tt-otch.acct) + '</Data></Cell>' SKIP.
   IF tt-otch.acct BEGINS "60312" THEN
   DO:
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="' + (IF tt-otch.in-ost  GE 0 THEN 's73' ELSE 's75') + '"><Data ss:Type="Number">' + TRIM(STRING(ABS(tt-otch.in-ost),"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + TRIM(STRING(tt-otch.db-obor,"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + TRIM(STRING(tt-otch.cr-obor,"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="' + (IF tt-otch.out-ost GE 0 THEN 's73' ELSE 's75') + '"><Data ss:Type="Number">' + TRIM(STRING(ABS(tt-otch.out-ost),"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-otch.usr-id + '</Data></Cell>' SKIP.
   END.
   ELSE IF tt-otch.acct BEGINS "60305" THEN
   DO:
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="' + (IF tt-otch.in-ost  GT 0 THEN 's75' ELSE 's73') + '"><Data ss:Type="Number">' + TRIM(STRING(ABS(tt-otch.in-ost),"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + TRIM(STRING(tt-otch.db-obor,"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + TRIM(STRING(tt-otch.cr-obor,"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="' + (IF tt-otch.out-ost GT 0 THEN 's75' ELSE 's73') + '"><Data ss:Type="Number">' + TRIM(STRING(ABS(tt-otch.out-ost),"->>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-otch.usr-id + '</Data></Cell>' SKIP.
   END.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

{rep-60312-f.i}

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
