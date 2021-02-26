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
{pp-corr.p}
{sh-defs.i}
{ksh-defs.i NEW}
{tmpobj.def}
{ttretval.def}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы             */

DEFINE VARIABLE mCnt          AS  INT64       NO-UNDO.
DEFINE VARIABLE mOk           AS  LOGICAL     NO-UNDO.
DEFINE VARIABLE mInt          AS  INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS  INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS  INT64       NO-UNDO.

DEFINE VARIABLE i             AS  INT64       NO-UNDO.
DEFINE VARIABLE j             AS  INT64       NO-UNDO.
DEFINE VARIABLE mStr1         AS  CHARACTER   NO-UNDO.
DEFINE VARIABLE mStr2         AS  CHARACTER   NO-UNDO.
DEFINE VARIABLE mStr3         AS  CHARACTER   NO-UNDO.
DEFINE VARIABLE Date1         AS  DATE FORMAT "99/99/9999".

DEFINE BUFFER   btmpsigns     FOR tmpsigns.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Создание динамической таблицы */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("acct",      "character").
   mFilterTable:ADD-NEW-FIELD ("number",    "character").
   mFilterTable:ADD-NEW-FIELD ("currency",  "character").
   mFilterTable:ADD-NEW-FIELD ("cust-cat",  "character").
   mFilterTable:ADD-NEW-FIELD ("cust-id",   "int64").
   mFilterTable:ADD-NEW-FIELD ("usluga",    "character").
   mFilterTable:ADD-NEW-FIELD ("comiss",    "character").
   mFilterTable:ADD-NEW-INDEX ("number").
   
   mFilterTable:ADD-INDEX-FIELD("number","number").

   /* Создание таблицы */
   mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

   RUN Fill-ProgressErr IN h_tmess ("").
   IF ERROR-STATUS:ERROR
      OR NOT mFilterTable:PREPARED THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
   
   mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
   mBuffer:FIND-FIRST() NO-ERROR.

   /*!*/
   i     = 0.
   j     = 0.
   mCnt  = 0.
   mStr1 = "".
   mStr2 = "".
   mStr3 = "".



   FOR EACH tmpsigns WHERE tmpsigns.file-name = "cust-corp"
                       AND tmpsigns.code      = "ЦФК-Онлайн"
            NO-LOCK:

            IF NOT CAN-DO(mStr1, tmpsigns.surrogate)
            THEN DO:
                     mStr1 = mStr1 + tmpsigns.surrogate + ",".
                     j = j + 1.
                 END.
   END.

   /*MESSAGE mStr1 string(j) string(ENTRY(1, mStr1, ',')) string(ENTRY(2, mStr1, ',')) view-as alert-box.*/

   DO i = 1 TO j:
   /*MESSAGE string(ENTRY(INT(i), mStr1, ',')) view-as alert-box.*/
   FIND LAST tmpsigns WHERE tmpsigns.file-name   = "cust-corp"
                        AND tmpsigns.code        = "ЦФК-Онлайн"
                        AND tmpsigns.surrogate   = string(ENTRY(INT(i), mStr1, ','))
             NO-LOCK.
             FOR EACH acct WHERE acct.filial-id       EQ shFilial
                             /*AND acct.acct          EQ ENTRY(1,signs.surrogate)*/
                             /*AND acct.currency      EQ ENTRY(2,signs.surrogate)*/
                             AND string(acct.cust-id) EQ tmpsigns.surrogate
                             AND acct.close-date      EQ ?
                             AND acct.cust-cat        EQ "Ю"
                             AND can-do("40602*,40701*,40702*,40703*,40807*,40821*", acct.acct)
                             AND (((acct.currency EQ "") AND (can-do("СпецПА,СпБрок,ДУ,Кон138,КонЗдт,КонРез,Текущ,Расчет", acct.contract)))
                                 OR
                                 ((acct.currency NE "") AND (acct.contract EQ "Текущ")))
                             /*AND acct.acct          NE "40702810500400010846     @0500"*/
                             /*AND acct.acct          EQ "40702978805170010001     @0500"*/
                      NO-LOCK:
             
                      IF GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "КорпКарт", "") EQ "Да"
                      THEN NEXT.

                      Date1 = Today.

                      mStr2 = GetTempXAttrValueEx("cust-corp", string(acct.cust-id), "ЦФК-Онлайн", Date1, "").
                      IF mStr2 = ""
                      THEN DO:
                               Date1 = tmpsigns.since - 1.
                               mStr2 = GetTempXAttrValueEx("cust-corp", string(acct.cust-id), "ЦФК-Онлайн", Date1, "").
                           END.

                      /*Date1 = Add-Interval(today, -1, "month").*/
                      /*Date1 = Date_Correct(month(Date1), 0, 31, year(Date1)).*/
                      /*mStr2 = GetTempXAttrValueEx("cust-corp", string(acct.cust-id), "ЦФК-Онлайн", Date1, "").*/

                      /*MESSAGE string(tmpsigns.surrogate) string(acct.acct) "mStr2 =" mStr2 "Date1 =" string(Date1) view-as alert-box.*/

                      mBuffer:BUFFER-CREATE().
                      ASSIGN                                
                             mCnt                                              = mCnt + 1
                             mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
                             mBuffer:BUFFER-FIELD ("acct")       :BUFFER-VALUE = acct.acct
                             mBuffer:BUFFER-FIELD ("number")     :BUFFER-VALUE = acct.number
                             mBuffer:BUFFER-FIELD ("currency")   :BUFFER-VALUE = acct.currency
                             mBuffer:BUFFER-FIELD ("cust-cat")   :BUFFER-VALUE = acct.cust-cat
                             mBuffer:BUFFER-FIELD ("cust-id")    :BUFFER-VALUE = acct.cust-id
                             mBuffer:BUFFER-FIELD ("usluga")     :BUFFER-VALUE = tmpsigns.code
                             mBuffer:BUFFER-FIELD ("comiss")     :BUFFER-VALUE = mStr2
                      .
             END.
   END.



   /*транзакция */   
   IF mCnt NE 0 THEN          
   RUN g-fltr.p (iInstance, mFilterTable, "", OUTPUT mOk).
END.



{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.
