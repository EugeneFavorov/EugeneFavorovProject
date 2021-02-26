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
DEFINE VARIABLE mLinkId       AS INT64       NO-UNDO.

/*{setdest.i &filename = "'mfil-obr-2.log'"}*/

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
   mFilterTable:ADD-NEW-FIELD ("mBenAcct", "character").
   mFilterTable:ADD-NEW-FIELD ("mNameBen", "character").
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
          op-entry.filial-id EQ "0300"
      AND op-entry.op-date   /*GE TODAY - 5*/  EQ DATE("07/08/2017") 
      AND op-entry.acct-db   BEGINS "30306"
      AND (op-entry.acct-cr  BEGINS "30233"
        OR op-entry.acct-cr  BEGINS "47423")
      NO-LOCK,
      FIRST op OF op-entry WHERE TRUE
      AND op.op-status    EQ CHR(251)
      AND (NOT op.op-kind BEGINS 'rout')
      AND op.doc-kind     EQ "send"
      AND op.details      BEGINS 'Отражение требований по карте'
      AND CAN-DO('30233*,47423*',op.ben-acct)      
      NO-LOCK,
      FIRST c-nostro WHERE
      c-nostro.nmfor EQ "044525129"
      AND c-nostro.acct  EQ op-entry.acct-db
      NO-LOCK QUERY-TUNING(NO-INDEX-HINT):
      
      mLinkId = GetXLinkID("opbct","op-mf").      
      FOR EACH links WHERE
             links.link-id   EQ mLinkId
         AND links.source-id EQ STRING(op.op)
         NO-LOCK:
         LEAVE.
      END.
      
      IF NOT AVAIL(links) THEN
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
            " op.op = " + STRING(op.op) + 
            " op-date = " + STRING(op-entry.op-date,"99/99/9999") + 
            " op.doc-num = " + op.doc-num + 
            " op.doc-num = " + op.doc-num +
            " op.ben-acct = " + AddFilToAcct(op.ben-acct,"0000") + 
            " op.name-ben = " + op.name-ben +
            " c-nostro.corr-acct = " + AddFilToAcct(c-nostro.corr-acct,"0000") +
            " op-entry.amt-rub = " + STRING(op-entry.amt-rub) +
            " op-entry.amt-cur = " + STRING(op-entry.amt-cur) +
            " op-entry.currency = " + op-entry.currency).
      
         mBuffer:BUFFER-CREATE().
         ASSIGN
            mCnt                                             = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid"):BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("mOpOp")     :BUFFER-VALUE = STRING(op.op)
            mBuffer:BUFFER-FIELD ("mDate")     :BUFFER-VALUE = STRING(op-entry.op-date,"99/99/9999")
            mBuffer:BUFFER-FIELD ("mDocNum")   :BUFFER-VALUE = op.doc-num
            mBuffer:BUFFER-FIELD ("mDocType")  :BUFFER-VALUE = op.doc-type
            mBuffer:BUFFER-FIELD ("mOrdPay")   :BUFFER-VALUE = op.order-pay
            mBuffer:BUFFER-FIELD ("mBenAcct")  :BUFFER-VALUE = op.ben-acct
            mBuffer:BUFFER-FIELD ("mNameBen")  :BUFFER-VALUE = op.name-ben
            mBuffer:BUFFER-FIELD ("mTrans")    :BUFFER-VALUE = TRIM(STRING(op.op-transaction))
            mBuffer:BUFFER-FIELD ("mAcctDb")   :BUFFER-VALUE = AddFilToAcct(op.ben-acct,"0000")
            mBuffer:BUFFER-FIELD ("mAcctCr")   :BUFFER-VALUE = AddFilToAcct(c-nostro.corr-acct,"0000")
            mBuffer:BUFFER-FIELD ("mAmtRub")   :BUFFER-VALUE = STRING(op-entry.amt-rub)
            mBuffer:BUFFER-FIELD ("mAmtCur")   :BUFFER-VALUE = STRING(op-entry.amt-cur)
            mBuffer:BUFFER-FIELD ("mCurrency") :BUFFER-VALUE = op-entry.currency
            mBuffer:BUFFER-FIELD ("mFilial")   :BUFFER-VALUE = "0000"
            mBuffer:BUFFER-FIELD ("mDetails")  :BUFFER-VALUE = op.details.
      END.
      ELSE
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
            "Уже есть связанный документ для " + STRING(op.op)).
      END.
   END.
   
   /*транзакция */   
   IF mCnt NE 0 THEN
   RUN g-fltr.p (iInstance,
                 mFilterTable,
                 "",
                 OUTPUT mOk).
END.
 
/*{preview.i &filename = "'mfil-obr-2.log'"}*/

{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.
