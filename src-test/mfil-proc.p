/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: mfil-proc.p 
      Comment: Выбор документов
   Parameters: iClass iInstance
         Uses:
      Used by:
      Created: 
*/

DEF INPUT PARAM iClass    AS CHARACTER NO-UNDO. /* Класс для отбора */
DEF INPUT PARAM iInstance AS HANDLE    NO-UNDO. /* Таблица экземпляра класса */

{globals.i}
{intrface.get tmess}
{intrface.get trans}
{intrface.get pbase}
{intrface.get xclass}
{intrface.get date}

{tmpobj.def}
{ttretval.def}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра  */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы              */

DEFINE VARIABLE mCnt          AS INT64       NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mInt          AS INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS INT64       NO-UNDO.
DEFINE VARIABLE mAcct         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mStr          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFIO          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mBegDate      AS DATE        NO-UNDO.
DEFINE VARIABLE mFilial       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct-Ben     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mLinkId       AS INT64       NO-UNDO.

{setdest.i &filename = "'mfil.log'"}

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("mOpOp",    "character").
   mFilterTable:ADD-NEW-FIELD ("mDate",    "character").
   mFilterTable:ADD-NEW-FIELD ("mDocNum",  "character").
   mFilterTable:ADD-NEW-FIELD ("mDocType", "character").
   mFilterTable:ADD-NEW-FIELD ("mOrdPay",  "character").
   mFilterTable:ADD-NEW-FIELD ("mTrans",   "character").
   mFilterTable:ADD-NEW-FIELD ("mAcctDb",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAcctCr",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAmtRub",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAmtCur",  "character").
   mFilterTable:ADD-NEW-FIELD ("mCurrency","character").
   mFilterTable:ADD-NEW-FIELD ("mFilial",  "character").
   mFilterTable:ADD-NEW-FIELD ("mDetails", "character").
   
   mFilterTable:ADD-NEW-INDEX  ("macct").
   mFilterTable:ADD-INDEX-FIELD("macct","mOpOp").

   /* Создание таблицы */
   mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

   RUN Fill-ProgressErr IN h_tmess ("").
   IF    ERROR-STATUS:ERROR
      OR NOT mFilterTable:PREPARED THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
   
   mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
   mBuffer:FIND-FIRST() NO-ERROR.

   mCnt = 0.

/*   ASSIGN                        */
/*      mBegDate = GetBaseOpDate() */
/*      mFilial  = shFilial        */
/*      mInt     = 0.              */
/*                                 */
/*   mBegDate = DATE("25/01/2017").*/
   
   FOR EACH op-entry WHERE
          op-entry.filial-id EQ "0000"
      AND op-entry.op-date  GE TODAY - 7 
/*        OR op-entry.op-date  EQ TODAY - 4)*/
      AND op-entry.acct-db   BEGINS "30306"
      AND (op-entry.acct-cr  BEGINS "30305"
           OR op-entry.acct-cr  BEGINS "20202"
        OR op-entry.acct-cr  BEGINS "47423")
      NO-LOCK,
      FIRST op OF op-entry WHERE
          op.op-status EQ CHR(251)
      AND (IF op-entry.acct-cr  BEGINS "30305" THEN op.doc-kind EQ "send" OR op.doc-kind EQ "rec" ELSE op.doc-kind EQ "send") 
      NO-LOCK,
      FIRST op-bank WHERE
          op-bank.op             EQ op.op
      AND op-bank.bank-code-type EQ "МФО-9"
      AND op-bank.bank-code      EQ "045209884"
      NO-LOCK,
      FIRST c-nostro WHERE
          c-nostro.nmfor EQ op-bank.bank-code
      AND c-nostro.acct  EQ op-entry.acct-db
      NO-LOCK:
      
      mLinkId = GetXLinkID("opb","op-mfil").      
      FOR EACH links WHERE
             links.link-id   EQ mLinkId
         AND links.source-id EQ STRING(op.op)
         NO-LOCK:
         LEAVE.
      END.
      
      IF NOT AVAIL(links) AND LOOKUP(TRIM(STRING(op.op)),"44316153,44325782,44325784,44399270") = 0 THEN
      DO:
         mAcct-Ben = AddFilToAcct(IF op-entry.acct-cr BEGINS "30305" AND op.doc-kind EQ "rec" THEN GetXAttrValueEx("op",STRING(op.op),"acct-send","") ELSE op.ben-acct,"0500").
         PUT UNFORMATTED
            STRING(op.op)              ";"
            STRING(op-entry.op-date,"99/99/9999") ";"
            op.doc-num     ";"
            mAcct-Ben      ";"
            AddFilToAcct(c-nostro.corr-acct,"0500")   ";"
            STRING(op-entry.amt-rub)      ";"
            STRING(op-entry.amt-cur) ";"
            op-entry.currency ";"
            "0500;"
            REPLACE(REPLACE(op.details,CHR(10),""),CHR(13),"") ";" 
      	SKIP.
      
         mBuffer:BUFFER-CREATE().
         ASSIGN 
            mCnt                                             = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid"):BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("mOpOp")     :BUFFER-VALUE = STRING(op.op)
            mBuffer:BUFFER-FIELD ("mDate")     :BUFFER-VALUE = STRING(op-entry.op-date,"99/99/9999")
            mBuffer:BUFFER-FIELD ("mDocNum")   :BUFFER-VALUE = op.doc-num
            mBuffer:BUFFER-FIELD ("mDocType")  :BUFFER-VALUE = IF op.doc-type EQ "032" THEN "019" ELSE op.doc-type 
            mBuffer:BUFFER-FIELD ("mOrdPay")   :BUFFER-VALUE = op.order-pay
            mBuffer:BUFFER-FIELD ("mTrans")    :BUFFER-VALUE = TRIM(STRING(op.op-transaction))
            mBuffer:BUFFER-FIELD ("mAcctDb")   :BUFFER-VALUE = mAcct-Ben
            mBuffer:BUFFER-FIELD ("mAcctCr")   :BUFFER-VALUE = AddFilToAcct(c-nostro.corr-acct,"0500")
            mBuffer:BUFFER-FIELD ("mAmtRub")   :BUFFER-VALUE = STRING(op-entry.amt-rub)
            mBuffer:BUFFER-FIELD ("mAmtCur")   :BUFFER-VALUE = STRING(op-entry.amt-cur)
            mBuffer:BUFFER-FIELD ("mCurrency") :BUFFER-VALUE = op-entry.currency
            mBuffer:BUFFER-FIELD ("mFilial")   :BUFFER-VALUE = "0500"
            mBuffer:BUFFER-FIELD ("mDetails")  :BUFFER-VALUE = op.details.
      END.
      ELSE
         PUT UNFORMATTED "Уже есть связанный документ для " + STRING(op.op) + "    " + op.doc-num + "." SKIP.
   END.
   
   /*транзакция */   
   IF mCnt NE 0 THEN
   RUN g-fltr.p (iInstance,
                 mFilterTable,
                 "",
                 OUTPUT mOk).
END.
 
{preview.i &filename = "'mfil.log'"}

{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.
