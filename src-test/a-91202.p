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
/*DEFINE VARIABLE mChoice       AS LOGICAL     NO-UNDO.*/
DEFINE VARIABLE mInt          AS INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS INT64       NO-UNDO.

DEFINE VARIABLE mStr          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct62101    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcct91202    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails3     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails4     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails5     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDetails6     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAuto         AS CHARACTER   NO-UNDO.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("macct",  "character").
   mFilterTable:ADD-NEW-FIELD ("mdetails1","character").
   mFilterTable:ADD-NEW-FIELD ("mdetails2","character").
   mFilterTable:ADD-NEW-FIELD ("mdetails3","character").
   mFilterTable:ADD-NEW-FIELD ("mdetails4","character").
   mFilterTable:ADD-NEW-FIELD ("mdetails5","character").
   mFilterTable:ADD-NEW-FIELD ("mdetails6","character").
   mFilterTable:ADD-NEW-FIELD ("mloadtype","character").
   mFilterTable:ADD-NEW-FIELD ("mauto","character").
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

   MESSAGE "Первичное оприходование с открытием счета (Yes)~nПовторное без открытия счета (No)"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET mChoice AS LOGICAL.

   IF mChoice EQ ? THEN RETURN {&RET-ERROR}.

   IF mChoice EQ YES THEN
   DO:
      INPUT FROM VALUE("a-91202.csv") CONVERT TARGET "ibm866"  SOURCE "1251".
      REPEAT:
         IMPORT UNFORMATTED mStr.
         IF mStr NE "end" THEN
         DO:
            mAcct62101 = TRIM(ENTRY(1,mStr,";")).
            mDetails1  = TRIM(ENTRY(2,mStr,";")).
            mDetails2  = TRIM(ENTRY(3,mStr,";")).
            mDetails3  = TRIM(ENTRY(4,mStr,";")).
            mDetails4  = TRIM(ENTRY(5,mStr,";")).
            mDetails5  = TRIM(ENTRY(6,mStr,";")).
            mDetails6  = TRIM(ENTRY(7,mStr,";")).
            
            IF mAcct62101 BEGINS "6" THEN
            DO:
               mBuffer:BUFFER-CREATE().
               ASSIGN
                  mCnt                                              = mCnt + 1
                  mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
                  mBuffer:BUFFER-FIELD ("macct")      :BUFFER-VALUE = mAcct62101
                  mBuffer:BUFFER-FIELD ("mdetails1")  :BUFFER-VALUE = mDetails1
                  mBuffer:BUFFER-FIELD ("mdetails2")  :BUFFER-VALUE = mDetails2
                  mBuffer:BUFFER-FIELD ("mdetails3")  :BUFFER-VALUE = mDetails3
                  mBuffer:BUFFER-FIELD ("mdetails4")  :BUFFER-VALUE = mDetails4
                  mBuffer:BUFFER-FIELD ("mdetails5")  :BUFFER-VALUE = mDetails5
                  mBuffer:BUFFER-FIELD ("mdetails6")  :BUFFER-VALUE = mDetails6
                  mBuffer:BUFFER-FIELD ("mloadtype")  :BUFFER-VALUE = "1".
            END.
         END.
      END.
      INPUT CLOSE.
   END.
   ELSE
   DO:
      INPUT FROM VALUE("a-91202-2.csv") CONVERT TARGET "ibm866"  SOURCE "1251".
      REPEAT:
         IMPORT UNFORMATTED mStr.
         IF mStr NE "end" THEN
         DO:
            mAcct91202 = TRIM(ENTRY(1,mStr,";")).
            mDetails1  = TRIM(ENTRY(2,mStr,";")).
            mDetails2  = TRIM(ENTRY(3,mStr,";")).
            mDetails3  = TRIM(ENTRY(4,mStr,";")).
            mDetails4  = TRIM(ENTRY(5,mStr,";")).
            mDetails5  = TRIM(ENTRY(6,mStr,";")).
            mDetails6  = TRIM(ENTRY(7,mStr,";")).
            
            IF mAcct91202 BEGINS "91202" THEN
            DO:
               mBuffer:BUFFER-CREATE().
               ASSIGN
                  mCnt                                              = mCnt + 1
                  mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
                  mBuffer:BUFFER-FIELD ("macct")      :BUFFER-VALUE = mAcct91202
                  mBuffer:BUFFER-FIELD ("mdetails1")  :BUFFER-VALUE = mDetails1
                  mBuffer:BUFFER-FIELD ("mdetails2")  :BUFFER-VALUE = mDetails2
                  mBuffer:BUFFER-FIELD ("mdetails3")  :BUFFER-VALUE = mDetails3
                  mBuffer:BUFFER-FIELD ("mdetails4")  :BUFFER-VALUE = mDetails4
                  mBuffer:BUFFER-FIELD ("mdetails5")  :BUFFER-VALUE = mDetails5
                  mBuffer:BUFFER-FIELD ("mdetails6")  :BUFFER-VALUE = mDetails6
                  mBuffer:BUFFER-FIELD ("mloadtype")  :BUFFER-VALUE = "2".
            END.
         END.
      END.
      INPUT CLOSE.
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
