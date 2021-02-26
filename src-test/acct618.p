/*
acct618.p
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

DEFINE TEMP-TABLE ttAcct
   FIELD acct-rid  AS RECID
   FIELD acct      AS CHARACTER
   FIELD curr      AS CHARACTER.

DEFINE BUFFER acct   FOR acct.
DEFINE BUFFER b-acct FOR acct.
DEFINE BUFFER o-acct FOR acct.

ETIME(YES).

/*{setdest.i &file-name = "111.log"}*/

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
   mInt = 0.
   FOR EACH acct WHERE
          acct.acct-cat   EQ "b"
      AND acct.filial-id  EQ shFilial
    /*AND acct.acct EQ "40701810204000011487     @0000"*/
      AND CAN-DO("!40821*,406*,407*,40802*",acct.acct)
      AND CAN-DO("!Текущ,!Накоп,!СпецПА,!Транз1,!СпБрок,*",acct.contract) 
      AND acct.open-date  LE DATE(TODAY - 365)
      AND acct.close-date EQ ?
      NO-LOCK: 
      
      ASSIGN
         mInt   = mInt + 1.
      FIND FIRST ttAcct WHERE
         ttAcct.acct-rid = RECID(acct) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttAcct THEN
      DO:
         mExist = NO.
         FOR EACH op-entry WHERE
                op-entry.op-date   GE DATE(TODAY - 365)
            AND op-entry.filial-id EQ shFilial
            AND op-entry.acct-db   EQ acct.acct
            AND NOT CAN-DO("47423*,706*",op-entry.acct-cr)
            NO-LOCK:
            mExist = YES.   
         END.
         FOR EACH op-entry WHERE
                op-entry.op-date   GE DATE(TODAY - 365)
            AND op-entry.filial-id EQ shFilial
            AND op-entry.acct-cr   EQ acct.acct
            NO-LOCK:
            mExist = YES.   
         END.
      END.
      ASSIGN
         mBlockList = ""
         mCart      = NO.
      mBlockList = BlockAcct(acct.acct + "," + acct.currency,
                             DATETIME(TODAY,MTIME)).
      FOR EACH o-acct WHERE
             o-acct.acct-cat   EQ "o"
         AND o-acct.filial-id  EQ shFilial
         AND o-acct.cust-cat EQ acct.cust-cat
         AND o-acct.cust-id  EQ acct.cust-id
         NO-LOCK:
         
         IF NOT CAN-DO("90901*,90902*",o-acct.acct) THEN NEXT. 
          
         RUN acct-pos IN h_base (o-acct.acct,
                                 o-acct.currency,
                                 TODAY,
                                 TODAY,
                                 CHR(251)).
         IF sh-bal NE 0 THEN mCart = YES.
      END.
             
      mGrp = GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "groupOABS", "").
      
      IF NOT mExist 
         AND NOT {assigned mBlockList} 
         AND NOT mCart 
         AND mGrp NE "598"
         AND mGrp NE "599" THEN
      DO:
         CREATE ttAcct.
         ASSIGN
            ttAcct.acct-rid = RECID(acct)
            ttAcct.acct     = acct.acct
            ttAcct.curr     = acct.currency.
      END.
   END.
   mInt = 0.
   FOR EACH ttAcct NO-LOCK:
      mInt = mInt + 1.
/*      PUT UNFORMATTED       */
/*         mInt ";"           */
/*         ttAcct.acct-rid ";"*/
/*         ttAcct.acct ";"    */
/*         ttAcct.curr ";"    */
/*      SKIP.                 */
   END.

/*   {preview.i &file-name = "111.log"}*/

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

