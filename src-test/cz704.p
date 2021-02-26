/**/

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
{tmpobj.def}
{tmprecid.def}
{ttretval.def}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* Буфер таблицы для фильтра  */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* Буфер таблицы              */

DEFINE VARIABLE mCnt      AS INT64       NO-UNDO.
DEFINE VARIABLE mOk       AS LOGICAL     NO-UNDO.

DEFINE VARIABLE mBAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBCurr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOCurr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetails  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mInt      AS INT64     NO-UNDO.

DEFINE BUFFER b-acct FOR acct.
DEFINE BUFFER o-acct FOR acct.

FORM
   mBAcct   LABEL  "  Расчетный счет" 
            FORMAT "x(20)"
            HELP   "Выбор расчетного счета "
   mDetails LABEL  "  Причина отзыва" 
            FORMAT "x(500)"
            VIEW-AS FILL-IN SIZE 50 BY 1
            HELP   "Причина отзыва докмента"

WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE " Выбор счета клиента ".

ON 'F1':U OF mBAcct IN FRAME frAcct
DO:
   {empty ttRetVal}
   DO TRANSACTION:
      RUN browseld.p("acct",
         "RetRcp" + CHR(1) + 
         "RetFld" + CHR(1) + 
         "RetType",
         STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1) + 
         "acct,currency" + CHR(1) + 
         "singl",   /*"multi",*/
         "",
         4).
   END. 
   
   IF KEYFUNCTION (LASTKEY) NE "END-ERROR" THEN
   DO:
      FOR EACH ttRetVal NO-LOCK:
         ASSIGN 
            mBAcct              = ENTRY(1,ttRetVal.PickValue)
            mBCurr              = ENTRY(2,ttRetVal.PickValue)
            mBAcct:SCREEN-VALUE = ENTRY(1,ttRetVal.PickValue).
      END.
      APPLY "ENTRY" TO mBAcct.
      RETURN NO-APPLY.
   END.
END.

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frAcct.
      RETURN ERROR.
   END.

   UPDATE
      mBAcct
      mDetails
   WITH FRAME frAcct.

/*mBAcct = "
40702810104000001062 
40702810904000005080 90901810804001011115 90902810604001015514
40702810704000012095 90901810200000000077 90902810400000002492
".*/
/*mBCurr = "".                    */
      
   {find-act.i
      &bact = b-acct
      &acct = mBAcct
      @curr = mBCurr 
   }

   IF AVAIL(b-acct) THEN
   DO:
      {empty ttRetVal}
         RUN browseld.p (
            "accto",
            "cust-cat" + CHR(1) + 
            "cust-id" + CHR(1) + 
            "RetRcp" + CHR(1) + 
            "RetFld" + CHR(1) + 
            "RetType",
            b-acct.cust-cat + CHR(1) +
            STRING(b-acct.cust-id)  + CHR(1) +
            STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1) + 
            "acct,currency" + CHR(1) + 
            "singl",   /*"multi",*/
            "",
            4).
   
      FOR EACH ttRetVal NO-LOCK:
         ASSIGN 
            mOAcct = ENTRY(1,ttRetVal.PickValue)
            mOCurr = ENTRY(2,ttRetVal.PickValue).
         LEAVE.
      END.
   
      {find-act.i
	      &bact = o-acct
	      &acct = mOAcct
	      @curr = mOCurr 
	   }

      IF AVAIL(o-acct) THEN
      DO:
         RUN kauacctv.p(o-acct.acct,o-acct.currency,4).
         
         IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
         DO:
            pick-value = "".
            FOR EACH tmprecid NO-LOCK: 
               FIND FIRST kau WHERE 
                  RECID(kau) EQ tmprecid.id 
               NO-LOCK NO-ERROR.
               IF NOT AVAIL kau THEN NEXT.
               FIND FIRST op WHERE
                  op.op EQ INT64(ENTRY(1,kau.kau))
               NO-LOCK NO-ERROR.
               FIND FIRST op-entry OF op
               NO-LOCK NO-ERROR.
               IF AVAIL(op) 
                  AND AVAIL(op-entry) THEN
               DO:
                  pick-value = IF pick-value EQ ""
                               THEN STRING(op.op) + "," + 
                                    STRING(op-entry.op-entry) + "," + 
                                    STRING(RECID(kau)) + "," + 
                                    STRING(kau.balance) + "," +
                                    kau.acct
                               ELSE pick-value + CHR(1) + 
                                    STRING(op.op) + "," + 
                                    STRING(op-entry.op-entry) + "," + 
                                    STRING(RECID(kau)) + "," + 
                                    STRING(kau.balance) + "," +
                                    kau.acct.
/*                  MESSAGE AVAIL(kau) ";" kau.kau ";" kau.kau-id ";" kau.acct ";" kau.balance ";" kau.zero-bal*/
/*                  VIEW-AS ALERT-BOX.                                                                         */
/*                  MESSAGE pick-value                                                                         */
/*                  VIEW-AS ALERT-BOX.                                                                         */
/*               pick-value = kau.kau + "," + kau.kau-id + "," + kau.acct + "," + STRING(kau.balance) + "," + STRING(kau.zero-bal).*/
               END.
            END.
         END.         
      END.
   END.
   ELSE
      pick-value = "".
END.
HIDE FRAME frAcct.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   IF NUM-ENTRIES(pick-value) GT 0 THEN
   DO:
      /* Создание динамической таблицы */
      RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).
   
      mFilterTable:ADD-NEW-FIELD ("op","int64").
      mFilterTable:ADD-NEW-FIELD ("op-entry","int64").
      mFilterTable:ADD-NEW-FIELD ("kau-recid","int64").
      mFilterTable:ADD-NEW-FIELD ("kau-balance","decimal").
      mFilterTable:ADD-NEW-FIELD ("kau-acct","character").
      mFilterTable:ADD-NEW-FIELD ("details","character").
      mFilterTable:ADD-NEW-INDEX("op").
      mFilterTable:ADD-INDEX-FIELD("op","op").
   
      /* Создание таблицы */
      mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.
   
      RUN Fill-ProgressErr IN h_tmess ("").
      IF    ERROR-STATUS:ERROR
         OR NOT mFilterTable:PREPARED THEN
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      
      mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
      mBuffer:FIND-FIRST() NO-ERROR.
      
      mCnt = 0.
      DO mInt = 1 TO NUM-ENTRIES(pick-value,CHR(1)):
         mBuffer:BUFFER-CREATE().
         ASSIGN 
            mCnt                                              = mCnt + 1
            mBuffer:BUFFER-FIELD ("__filterid") :BUFFER-VALUE = mCnt
            mBuffer:BUFFER-FIELD ("op")         :BUFFER-VALUE = INT64(ENTRY(1,ENTRY(mInt,pick-value,CHR(1))))
            mBuffer:BUFFER-FIELD ("op-entry")   :BUFFER-VALUE = INT64(ENTRY(2,ENTRY(mInt,pick-value,CHR(1))))
            mBuffer:BUFFER-FIELD ("kau-recid")  :BUFFER-VALUE = INT64(ENTRY(3,ENTRY(mInt,pick-value,CHR(1))))
            mBuffer:BUFFER-FIELD ("kau-balance"):BUFFER-VALUE = DECIMAL(ENTRY(4,ENTRY(mInt,pick-value,CHR(1))))
            mBuffer:BUFFER-FIELD ("kau-acct")   :BUFFER-VALUE = ENTRY(5,ENTRY(mInt,pick-value,CHR(1)))
            mBuffer:BUFFER-FIELD ("details")    :BUFFER-VALUE = ", " + TRIM(mDetails,".") + ".".
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

/*
REPEAT:
  SET os-file WITH FRAME osfile-info.
  FILE-INFO:FILE-NAME = os-file.
  DISPLAY FILE-INFO:FULL-PATHNAME FORMAT "x(60)" LABEL "Full Path"
    FILE-INFO:PATHNAME FORMAT "x(60)" LABEL "Path"
    FILE-INFO:FILE-TYPE LABEL "Type"
    WITH FRAME osfile-info SIDE-LABELS TITLE "OS File Info".
END.


FILE-INFO:FILE-MOD-TIME
*/ 



