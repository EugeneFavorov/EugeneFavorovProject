/*
acct618f.p
*/

DEF INPUT PARAM iClass    AS CHARACTER NO-UNDO. /* Класс для отбора */
DEF INPUT PARAM iInstance AS HANDLE    NO-UNDO. /* Таблица экземпляра класса */

{globals.i}
{intrface.get trans}
{intrface.get pbase}
{intrface.get date}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

{sh-defs.i}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра  */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы              */

DEFINE VARIABLE mCnt      AS INT64       NO-UNDO.
DEFINE VARIABLE mOk       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mGrp      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mInt         AS INT64       NO-UNDO.
DEFINE VARIABLE mExist       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mBlockList   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCart        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mBegDate     AS DATE        NO-UNDO.
DEFINE VARIABLE mEndDate     AS DATE        NO-UNDO.
DEFINE VARIABLE mSum         AS DECIMAL     NO-UNDO.

DEFINE VARIABLE iStat        AS INT64       NO-UNDO.
DEFINE VARIABLE mHandle      AS INT64       NO-UNDO.
DEFINE VARIABLE mLastDate    AS DATE        NO-UNDO.

DEFINE TEMP-TABLE ttAcct
   FIELD acct-rid  AS RECID
   FIELD acct      AS CHARACTER
   FIELD curr      AS CHARACTER
   FIELD ogrp      AS CHARACTER
   FIELD ost       AS DECIMAL
   FIELD ldate     AS DATE.

DEFINE BUFFER acct   FOR acct.
DEFINE BUFFER b-acct FOR acct.
DEFINE BUFFER o-acct FOR acct.

ETIME(YES).

{setdest.i &filename = "'acctf.log'"}

mBegDate = DATE("01/01/2017").
mEndDate = DATE("31/12/2017").

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
   mInt = 0.
   FOR EACH acct WHERE
          acct.acct-cat   EQ "b"
      AND acct.filial-id  EQ shFilial
/*      AND acct.acct EQ "42301810201600015909     @0500"*/
/*      AND acct.acct EQ "40817810501600013427     @0500"*/
/*      AND acct.acct EQ "40817810000000040342     @0000"*/
/*      AND acct.acct EQ "40817810201600021489     @0500"*/
      AND CAN-DO("40817810*,40820810*,42301810*,42601810*",acct.acct)
      AND acct.open-date  LE mBegDate
      AND acct.close-date EQ ?
      NO-LOCK: 
      
      ASSIGN
         mInt   = mInt + 1.

      RUN acct-pos IN h_base(acct.acct,acct.currency,TODAY,TODAY,CHR(251)).

      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nacct.acct = " + acct.acct + 
         "~nsh-bal = " + STRING(sh-bal)).
      
      IF sh-bal EQ 0 THEN NEXT.

      FOR EACH loan-acct WHERE
             loan-acct.acct     EQ acct.acct
         AND loan-acct.currency EQ acct.currency
         NO-LOCK,
         EACH loan WHERE
             loan.contract  EQ loan-acct.contract
         AND loan.cont-code EQ loan-acct.cont-code
         AND loan.cont-type NE "ДВ"
         NO-LOCK:
         LEAVE.
      END.

      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nAVAIL(loan) = " + STRING(AVAIL(loan))).
      
      FIND FIRST ttAcct WHERE
         ttAcct.acct-rid = RECID(acct) NO-LOCK NO-ERROR.
      IF NOT AVAIL(ttAcct) THEN
      DO:
         mExist = NO.
         FOR EACH op-entry WHERE
                op-entry.op-date   GE mBegDate
            AND op-entry.op-date   LE mEndDate
            AND op-entry.filial-id EQ shFilial
            AND op-entry.acct-db   EQ acct.acct
            AND CAN-DO("!70601*,*",op-entry.acct-cr)
            NO-LOCK:
            
            mExist = YES.
            
/*            PUT UNFORMATTED          */
/*               "db;"                 */
/*               op-entry.acct-db   ";"*/
/*               op-entry.acct-cr ";"  */
/*               op-entry.amt-rub ";"  */
/*               mExist ";"            */
/*            SKIP.                    */

         END.
         
         FOR EACH op-entry WHERE
                op-entry.op-date   GE mBegDate
            AND op-entry.op-date   LE mEndDate
            AND op-entry.filial-id EQ shFilial
            AND CAN-DO("!70606*,*",op-entry.acct-db)
            AND op-entry.acct-cr   EQ acct.acct
            NO-LOCK:
            
            IF op-entry.acct-db BEGINS "47411"
            AND NOT AVAIL(loan) THEN NEXT.
            
            IF  op-entry.acct-db BEGINS "47422" 
            AND op-entry.acct-db NE "47422810901600000017     @0500" THEN NEXT.
            
/*            PUT UNFORMATTED          */
/*               "cr;"                 */
/*               AVAIL(loan) ";"       */
/*               op-entry.acct-db   ";"*/
/*               op-entry.acct-cr ";"  */
/*               op-entry.amt-rub ";"  */
/*               mExist ";"            */
/*            SKIP.                    */
            
            mExist = YES.
            LEAVE.
         END.
      END.
             
      RUN STORED-PROCEDURE GET_LAST_DATEPAY mHandle = PROC-HANDLE
         (
         INPUT  PARAM P_ACC             = acct.number,
         INPUT  PARAM P_DATELESS        = TODAY,
         OUTPUT PARAM GET_LAST_DATEPAY  = ?
         ).
      CLOSE STORED-PROC GET_LAST_DATEPAY iStat = PROC-STATUS.
      IF iStat = 0 THEN
      DO:
         mLastDate = GET_LAST_DATEPAY.
      END.
      ELSE mLastDate = ?.
      
      
      
      mGrp = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").
      
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nmExist = " + STRING(mExist) + 
         "~nmGrp   = " + mGrp).
      
      IF NOT mExist
/*         AND mGrp NE "598"*/
         AND mGrp NE "599" THEN
      DO:
         CREATE ttAcct.
         ASSIGN
            ttAcct.acct-rid = RECID(acct)
            ttAcct.acct     = acct.acct
            ttAcct.curr     = acct.currency
            ttAcct.ogrp     = mGrp
            ttAcct.ost      = sh-bal
            ttAcct.ldate    = mLastDate.
      END.
   END.

   mInt = 0.
   mSum = 0.
   FOR EACH ttAcct NO-LOCK:
      mInt = mInt + 1.
      mSum = mSum + IF ABS(ttAcct.ost) GT 240 THEN 240 ELSE ABS(ttAcct.ost).
      PUT UNFORMATTED
         mInt ";"
         /*ttAcct.acct-rid ";"*/
         ttAcct.acct ";"
         ttAcct.ogrp ";"
         /*ttAcct.curr ";"*/
         ABS(ttAcct.ost)  ";"
         ttAcct.ldate     ";"
         (IF ABS(ttAcct.ost) GT 240 THEN 240 ELSE ABS(ttAcct.ost))
      SKIP.
   END.

   PUT UNFORMATTED FILL("=",80) SKIP.
   PUT UNFORMATTED mInt ";;;;;" TRIM(STRING(mSum,">>>,>>>,>>>,>>9.99")) SKIP.
   
   {preview.i &filename = "'acctf.log'"}

   pick-value = "1". 
   RUN messmenu.p(10 ,
      "[ ВОПРОС ]",
      "Запустить создание документов?",
      "Запустить,Отменить").
   IF pick-value NE "1" THEN RETURN.

   IF mInt GT 0 THEN
   DO:
      /* Создание динамической таблицы */
      RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

      mFilterTable:ADD-NEW-FIELD ("acct","character").
      mFilterTable:ADD-NEW-FIELD ("currency","character").
      mFilterTable:ADD-NEW-INDEX("acct").
      mFilterTable:ADD-INDEX-FIELD("acct","acct").

      /* Создание таблицы */
      mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

      RUN Fill-ProgressErr IN h_tmess ("").
      IF    ERROR-STATUS:ERROR
         OR NOT mFilterTable:PREPARED THEN
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

      mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
      mBuffer:FIND-FIRST() NO-ERROR.

      mCnt = 0.
      FOR EACH ttAcct NO-LOCK:
         mBuffer:BUFFER-CREATE().
         ASSIGN
            mCnt                                              = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("acct")       :BUFFER-VALUE = ttAcct.acct
            mBuffer:BUFFER-FIELD ("currency")   :BUFFER-VALUE = ttAcct.curr.
      END.
      IF mCnt NE 0 THEN
      RUN g-fltr.p (iInstance,
                    mFilterTable,
                    "",
                    OUTPUT mOk).
   END.
END.

{intrface.del}   

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.

