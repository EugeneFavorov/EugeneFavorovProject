/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: sspin-proc.p 
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

{sh-defs.i}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра  */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы              */

DEFINE VARIABLE mCnt          AS INT64       NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mInt          AS INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS INT64       NO-UNDO.

DEFINE VARIABLE mStr          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFilial       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mBalAcct      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCustCat      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCustID       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOst          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDog          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct60312    AS CHARACTER   NO-UNDO.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("mfilial", "character").
   mFilterTable:ADD-NEW-FIELD ("mbalacct", "int64").
   mFilterTable:ADD-NEW-FIELD ("macct",  "character").
   mFilterTable:ADD-NEW-FIELD ("mdetails","character").
   mFilterTable:ADD-NEW-FIELD ("mcontract","character").
   mFilterTable:ADD-NEW-FIELD ("mcustcat","character").
   mFilterTable:ADD-NEW-FIELD ("mcustid","character").
   mFilterTable:ADD-NEW-FIELD ("most","character").
   mFilterTable:ADD-NEW-FIELD ("mdog","character").
   mFilterTable:ADD-NEW-FIELD ("m60312","character").
   mFilterTable:ADD-NEW-INDEX ("macct").
   
   mFilterTable:ADD-INDEX-FIELD("macct","macct").

   /* Создание таблицы */
   mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

   RUN Fill-ProgressErr IN h_tmess ("").
   IF    ERROR-STATUS:ERROR
      OR NOT mFilterTable:PREPARED THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
   
   mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
   mBuffer:FIND-FIRST() NO-ERROR.

   /*!*/
   mCnt    = 0.
   mStr    = "".
   mFilial = "0000".
   INPUT FROM VALUE("a-sales.txt").
   REPEAT:
      IMPORT UNFORMATTED mStr.
      IF mStr NE "end" THEN
      DO:
         IF ENTRY(1,mStr,";") EQ "Договор" THEN
         DO:
            mDog = TRIM(ENTRY(2,mStr,";")).
            NEXT.
         END.
         IF ENTRY(1,mStr,";") EQ "Счет 60312" THEN
         DO:
            mAcct60312 = TRIM(ENTRY(2,mStr,";")) + "     @" + mFilial.
            NEXT.
         END.
         FOR EACH acct WHERE TRUE
            AND acct.acct EQ ENTRY(1,mStr,";") + "     @" + mFilial
            NO-LOCK:
            mAcct    = acct.acct.
            mDetails = SUBSTRING(acct.details,INDEX(acct.details,"автомобиль")).
            RUN acct-pos IN h_base(acct.acct,acct.currency,TODAY,TODAY,CHR(251)).
            mOst = STRING(sh-bal).
         END.
         
         mBuffer:BUFFER-CREATE().
         ASSIGN 
            mCnt                                              = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("mfilial")    :BUFFER-VALUE = mFilial
            mBuffer:BUFFER-FIELD ("macct")      :BUFFER-VALUE = mAcct
            mBuffer:BUFFER-FIELD ("most")       :BUFFER-VALUE = mOst
            mBuffer:BUFFER-FIELD ("mdog")       :BUFFER-VALUE = mDog
            mBuffer:BUFFER-FIELD ("m60312")     :BUFFER-VALUE = mAcct60312
            mBuffer:BUFFER-FIELD ("mdetails")   :BUFFER-VALUE = mDetails.
      END.
   END.
   INPUT CLOSE.
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
