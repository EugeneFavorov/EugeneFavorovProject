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

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра  */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы              */

DEFINE VARIABLE mCnt          AS INT64       NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mInt          AS INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS INT64       NO-UNDO.

DEFINE VARIABLE mStr          AS CHARACTER   NO-UNDO.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("mfilial-id", "character").
   mFilterTable:ADD-NEW-FIELD ("mbalacct", "int64").
   mFilterTable:ADD-NEW-FIELD ("macct",  "character").
   mFilterTable:ADD-NEW-FIELD ("mdetails","character").
   mFilterTable:ADD-NEW-FIELD ("mcontract","character").
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
   mCnt = 0.
   mStr = "".
   INPUT FROM VALUE("cracct.txt").
   REPEAT:
      IMPORT UNFORMATTED mStr.
      IF mStr NE "end" THEN
      DO:
         /*MESSAGE mStr
         VIEW-AS ALERT-BOX.*/
         mBuffer:BUFFER-CREATE().
         ASSIGN 
            mCnt                                              = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("mfilial-id") :BUFFER-VALUE = "0000"
            mBuffer:BUFFER-FIELD ("mbalacct")   :BUFFER-VALUE = SUBSTRING(ENTRY(1,mStr,"|"),1,5)
            mBuffer:BUFFER-FIELD ("macct")      :BUFFER-VALUE = ENTRY(1,mStr,"|")
            mBuffer:BUFFER-FIELD ("mdetails")   :BUFFER-VALUE = ENTRY(2,mStr,"|")
            mBuffer:BUFFER-FIELD ("mcontract")  :BUFFER-VALUE = "".
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
