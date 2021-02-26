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

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы             */

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

   mFilterTable:ADD-NEW-FIELD ("acct",     "character").
   mFilterTable:ADD-NEW-FIELD ("number",   "character").
   mFilterTable:ADD-NEW-FIELD ("currency", "character").
   mFilterTable:ADD-NEW-FIELD ("cust-cat", "character").
   mFilterTable:ADD-NEW-FIELD ("cust-id",  "int64").
   mFilterTable:ADD-NEW-INDEX ("number").
   
   mFilterTable:ADD-INDEX-FIELD("number","number").

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

   

/* =====================================================
"FOR EACH ACCT WHERE ACCT.FILIAL-ID = '0500'
AND ACCT.CLOSE-DATE EQ ?
AND ACCT.CURRENCY EQ '',
FIRST SIGNS WHERE SIGNS.FILE-NAME EQ 'ACCT'
AND SIGNS.SURROGATE EQ (ACCT.ACCT + ',' + ACCT.CURRENCY)
AND SIGNS.CODE EQ 'КорпКарт'
AND SIGNS.CODE-VALUE EQ 'Да' NO-LOCK:"
===================================================== */



/*{setdest.i &filename = "'report.log'"}*/



   FOR EACH signs WHERE signs.file-name  EQ "acct"
                    AND signs.code       EQ "КорпКарт"
                    AND signs.code-value EQ "Да"
                  /*AND (((signs.code    EQ "КорпКарт") AND (signs.code-value EQ "Да")) OR ((signs.code EQ "groupOABS") AND (signs.code-value EQ "599")))*/

            NO-LOCK,
                     EACH acct WHERE acct.filial-id   EQ "0500"
                                 AND acct.close-date  EQ ?
                                 AND acct.acct        EQ ENTRY(1,signs.surrogate)
                                 AND acct.currency    EQ ENTRY(2,signs.surrogate)
                               /*AND acct.acct        EQ "40702810100000000009     @0500"*/
                          NO-LOCK: 
                                   /*MESSAGE acct.acct VIEW-AS ALERT-BOX.*/     
                      
                                   mBuffer:BUFFER-CREATE().
                                   ASSIGN                                
                                          mCnt                                             = mCnt + 1
                                          mBuffer:BUFFER-FIELD ("__filterid"):BUFFER-VALUE = mCnt
                                          mBuffer:BUFFER-FIELD ("acct")      :BUFFER-VALUE = acct.acct
                                          mBuffer:BUFFER-FIELD ("number")    :BUFFER-VALUE = acct.number
                                          mBuffer:BUFFER-FIELD ("currency")  :BUFFER-VALUE = acct.currency
                                          mBuffer:BUFFER-FIELD ("cust-cat")  :BUFFER-VALUE = acct.cust-cat
                                          mBuffer:BUFFER-FIELD ("cust-id")   :BUFFER-VALUE = acct.cust-id
                                   .
   /*PUT UNFORMATTED acct.acct skip.*/
   END.



/*{preview.i &filename = "'report.log'"}*/



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
