/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: kassz-proc.p 
      Comment: Выбор счетов для транзакции ssp-in
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

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("mAcctDb",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAcctCr",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAmtRub",  "character").
   mFilterTable:ADD-NEW-FIELD ("mAmtCur",  "character").
   mFilterTable:ADD-NEW-FIELD ("mCurrency","character").
   mFilterTable:ADD-NEW-FIELD ("mSymbol",  "character").
   mFilterTable:ADD-NEW-FIELD ("mFilial",  "character").
   mFilterTable:ADD-NEW-FIELD ("mDID",     "character").
   mFilterTable:ADD-NEW-FIELD ("mFIO",     "character").
   mFilterTable:ADD-NEW-FIELD ("mDate",    "character").
   mFilterTable:ADD-NEW-FIELD ("mCommen",  "character").
   mFilterTable:ADD-NEW-FIELD ("mDetails", "character").
   
   mFilterTable:ADD-NEW-INDEX  ("macct").
   mFilterTable:ADD-INDEX-FIELD("macct","mAcctDb").

   /* Создание таблицы */
   mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

   RUN Fill-ProgressErr IN h_tmess ("").
   IF    ERROR-STATUS:ERROR
      OR NOT mFilterTable:PREPARED THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
   
   mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
   mBuffer:FIND-FIRST() NO-ERROR.

   IF NOT CONNECTED("bank") THEN 
   DO:
      MESSAGE "Нет соединения c bank.".
   END.

   mCnt = 0.
   FOR EACH bank.query_cash_nal WHERE 
      bank.query_cash_nal.idate GE DATE("12/01/2017")
      NO-LOCK QUERY-TUNING(NO-INDEX-HINT):

      FOR EACH signs WHERE
             signs.file-name EQ "op"
         AND signs.code      EQ "did-smartfl"
         AND signs.dec-value EQ bank.query_cash_nal.DID NO-LOCK,
         FIRST op WHERE 
         op.op EQ INT64(signs.surrogate)
         NO-LOCK:
         LEAVE.
      END.

      IF AVAIL(op) THEN NEXT.
      
      mAcct = AddFilToAcct(bank.query_cash_nal.ACC,"0500").
      
      {find-act.i
         &acct = mAcct
      }
      
      IF AVAIL(acct) THEN
      DO:
         IF acct.cust-cat EQ "Ю" THEN
         DO:
            FIND FIRST cust-corp WHERE
               cust-corp.cust-id EQ acct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL(cust-corp) THEN
            ASSIGN mFIO = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"ФИОрук","ФИО не заполнено").
         END.
         ELSE IF acct.cust-cat EQ "Ч" THEN
         DO:
            FIND FIRST person WHERE
               person.person-id EQ acct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL(person) THEN
            ASSIGN mFIO = GetXattrValueEx("person",STRING(person.person-id),"ФИОрук","ФИО не заполнено").
         END.
      END.

      mBuffer:BUFFER-CREATE().
      ASSIGN 
         mCnt                                             = mCnt + 1
         mBuffer:BUFFER-FIELD ("__filterid"):BUFFER-VALUE = mCnt
         mBuffer:BUFFER-FIELD ("mAcctDb")   :BUFFER-VALUE = mAcct 
         mBuffer:BUFFER-FIELD ("mAcctCr")   :BUFFER-VALUE = "20202810505001000000     @0500"
         mBuffer:BUFFER-FIELD ("mAmtRub")   :BUFFER-VALUE = STRING(bank.query_cash_nal.AMOUNT)
         mBuffer:BUFFER-FIELD ("mAmtCur")   :BUFFER-VALUE = STRING(0)
         mBuffer:BUFFER-FIELD ("mCurrency") :BUFFER-VALUE = ""
         mBuffer:BUFFER-FIELD ("mSymbol")   :BUFFER-VALUE = bank.query_cash_nal.SYMBOL
         mBuffer:BUFFER-FIELD ("mFilial")   :BUFFER-VALUE = "0500"
         mBuffer:BUFFER-FIELD ("mDID")      :BUFFER-VALUE = STRING(bank.query_cash_nal.DID)
         mBuffer:BUFFER-FIELD ("mFIO")      :BUFFER-VALUE = mFIO
         mBuffer:BUFFER-FIELD ("mDate")     :BUFFER-VALUE = STRING(bank.query_cash_nal.IDATE,"99/99/9999")
         mBuffer:BUFFER-FIELD ("mCommen")   :BUFFER-VALUE = STRING(bank.query_cash_nal.GETIME,"99/99/9999") + " " + 
                                                            STRING(bank.query_cash_nal.GETIME-1,"hh:mm:ss") + " " +
                                                            (IF {assigned bank.query_cash_nal.COMMEN} THEN "|" + bank.query_cash_nal.COMMEN ELSE "")
         mBuffer:BUFFER-FIELD ("mDetails")  :BUFFER-VALUE = "Выдача денежных средств с расчетного счета юридического лица по чеку.".
   END.
   
   /*транзакция */   
   IF mCnt NE 0 THEN   
   RUN g-fltr.p (iInstance,
                 mFilterTable,
                 "",
                 OUTPUT mOk).
END.
 

{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.
