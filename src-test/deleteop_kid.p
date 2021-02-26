/* Глухов. Можно удалять докуметы по кредитам и ЧВ созданные транзакциями в нескольких потоках */
{globals.i}
{intrface.get xclass}
{intrface.get op}
{intrface.get kau}
{ttretval.def}

DEFINE TEMP-TABLE tt-op NO-UNDO
   FIELD op AS INT64.

DEFINE VARIABLE mint AS INT64 NO-UNDO.
DEFINE VARIABLE mint1 AS INT64 NO-UNDO.
DEFINE VARIABLE mk   AS INT64  NO-UNDO.
DEFINE VARIABLE mdate AS DATE  NO-UNDO.
DEFINE VARIABLE mc AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE mtranz AS CHARACTER  NO-UNDO.
DEFINE VARIABLE muser  AS CHARACTER NO-UNDO.
DEFINE VARIABLE most   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mpotok  AS CHARACTER INIT "*" NO-UNDO.
DEFINE VARIABLE mMaxTime   AS DATETIME-TZ NO-UNDO. 
DEFINE VARIABLE mMaxRID    AS RECID NO-UNDO. 
DEFINE VARIABLE mFilial    AS CHARACTER NO-UNDO.


DEFINE BUFFER bcode FOR code.
DEfine stream log-stream.

FOR EACH   bcode WHERE 
           bcode.class  EQ "ПотокиЗапуск" 
       AND bcode.parent EQ "ПотокиЗапуск" 
       AND bcode.code   BEGINS STRING(TODAY)  + "," 
NO-LOCK:

IF mMaxTime EQ ? 
   OR
   mMaxTime LT DATETIME-TZ(bcode.misc[1])
THEN 
   ASSIGN
      mMaxTime = DATETIME-TZ(bcode.misc[1])
      mMaxRID  = RECID(bcode).
END.

IF mMaxTime EQ ? THEN

FIND LAST  bcode WHERE 
           bcode.class  EQ "ПотокиЗапуск" 
       AND bcode.parent EQ "ПотокиЗапуск" 
NO-LOCK NO-ERROR.

IF AVAIL bcode THEN mMaxRID = RECID(bcode).

FIND FIRST code WHERE RECID(code) EQ mMaxRID
NO-LOCK NO-ERROR.                                                                                                                          	

IF AVAILABLE code THEN 
   ASSIGN
      mUser   = code.val
      mDate  =  DATE(code.misc[3])
      mFilial = code.misc[4]
      mtranz = REPLACE(code.misc[5],";",",")
      .


DEFINE FRAME iParam 
   mFilial LABEL "Филиал        " FORMAT "X(4)"
   mtranz  LABEL "Транзакции    " FORMAT "x(150)" VIEW-AS fill-in size 50 by 1
   mUser   LABEL "Пользователь  " FORMAT "x(12)"
   mpotok  LABEL "Поток         " HELP "* - все потоки,  -1 - без потоков" FORMAT "x(40)"
   mDate   LABEL "Опер. день    " FORMAT "99.99.99"
   most    LABEL "Осталось удал." VIEW-AS TEXT
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ ЗАДАЙТЕ ПАРАМЕТРЫ УДАЛЕНИЯ]".


ON 'F1' OF mTranz
DO:
   DO TRANSACTION:
      {empty ttretval}
      RUN browseld.p("user-proc",
                     "procedure~001RetRcp~001RetFld~001ActionLock",
                     "run14~001" + STRING (TEMP-TABLE ttRetVal:HANDLE) + "~001params~001F1,F2,INS,F9,DEL,ENTER", 
                     "",
                     4).
      IF     (LASTKEY EQ 13 OR
             LASTKEY EQ 10) 
         AND CAN-FIND (FIRST ttRetVal)
         THEN 
         FOR EACH ttretval:
             mTranz:SCREEN-VALUE = REPLACE(
             IF NUM-ENTRIES(ttretval.pickvalue," ") GE 2 THEN 
                ENTRY(2,ttretval.pickvalue," ") 
             ELSE "",";",",").
         end.
   END.
END.

ON 'F1' OF mUser
   DO:
      pick-value = "".
      DO TRANSACTION:
         RUN browseld.p("_user","","",?,4).
      END.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN mUser:SCREEN-VALUE = pick-value.

   END.


ON F1 OF mFilial
   DO:
      DO TRANSACTION:
         RUN browseld.p("branch",
            "branch-type" ,
            "10,11",
            "",
            4).
         IF (LASTKEY EQ 13 OR
            LASTKEY EQ 10) AND
            pick-value NE ?
            THEN mFilial:SCREEN-VALUE = pick-value.
      END. 
   END.

ON F1 OF mDate
DO:
   DO TRANS:
      pick-value = ?.
      RUN calend.p.
      IF  ( LASTKEY EQ KEYCODE("ENTER")
           OR LASTKEY EQ 10 )
           AND pick-value NE ?
      THEN ASSIGN mDate:SCREEN-VALUE = pick-value.
   END.
END.


PAUSE 0.
UPDATE 
   mFilial
   mTranz
   mUser
   mpotok
   mDate
   most
   WITH FRAME iParam. 
IF LAST-EVENT:FUNCTION  EQ "END-ERROR" THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.

ASSIGN
   mint = 0.
   

IF mPotok EQ "-1" THEN
DO mk = 1 TO NUM-ENTRIES(mtranz):
   FOR EACH op WHERE 
            op.filial-id EQ mFilial
        AND op.op-kind   EQ ENTRY(mk,mtranz)
        AND op.op-date   EQ mDate
        AND op.user-id   EQ muser       
      NO-LOCK:
         CREATE tt-op.
         tt-op.op = op.op.
         mInt = mInt + 1.
   END.
END.
ELSE
DO mk = 1 TO NUM-ENTRIES(mtranz):
   FOR EACH op WHERE 
            op.filial-id EQ mFilial
        AND op.op-kind   EQ ENTRY(mk,mtranz)
        AND op.op-date   EQ mDate
        AND op.user-id   EQ "SERVCRED"       
      NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "op" AND
      signs.surrogate EQ string(op.op) AND
      signs.code EQ "СоздалПоток" AND 
      signs.code-val BEGINS muser + "|"
      NO-LOCK:
      IF CAN-DO(mpotok,ENTRY(2,signs.code-val,"|")) THEN
      DO:
         CREATE tt-op.
         tt-op.op = op.op.
         mInt = mInt + 1.
      END.
   END.
END.

MESSAGE "Будет удалено " mInt " документов за " mdate VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO TITLE "ПОДТВЕРЖДЕНИЕ" UPDATE mc.

IF NOT mc THEN RETURN.


{disable.hst}
{setdest.i &stream="stream log-stream" &filename='delkid.log'}
ASSIGN
   mint1 = mint
   mint = 0.
MC1:
FOR EACH tt-op NO-LOCK:    
   ASSIGN
      mInt = mInt + 1
      mint1 = mint1 - 1
      most = string(mint1)
      most:SCREEN-VALUE = string(mint1)
   .

   FOR EACH loan-int WHERE
      loan-int.op = tt-op.op AND loan-int.contract EQ "Кредит" NO-LOCK,
      EACH loan OF loan-int  NO-LOCK:
      IF loan.since NE mDate THEN 
      DO:
      ASSIGN
      mint = mint - 1
      mint1 = mint1 + 1.
      put stream log-stream unformatted loan.cont-code "               не пересчитан на дату операции" SKIP.
      NEXT MC1.
      END.
   END.


   FOR EACH loan-int WHERE
      loan-int.op = tt-op.op
      EXCLUSIVE-LOCK:
      DELETE loan-int.
   END.

   FOR EACH op-entry WHERE
      op-entry.op EQ tt-op.op
      EXCLUSIVE-LOCK:
      DELETE op-entry.
   END.


   FOR EACH op WHERE
      op.op EQ tt-op.op
      EXCLUSIVE-LOCK:

      action:
      FOR EACH kau-entry OF op EXCLUSIVE-LOCK
         ON ERROR UNDO Action, NEXT Action ON ENDKEY UNDO Action, LEAVE Action:    
         {op-kau.del}
         DELETE kau-entry.
      END.

      RUN DelSigns IN h_xclass("op",string(op.op)).

      DELETE op.
   END.
END.    
{enable.hst}
IF DEC(most) NE 0 THEN
DO:
   MESSAGE "Не удалено  " mOst VIEW-AS ALERT-BOX.
   {preview.i &stream="stream log-stream" &filename='delkid.log'}
END.
ELSE
DO:
   OUTPUT STREAM log-stream CLOSE.
   MESSAGE "Удалено   "  mInt VIEW-AS ALERT-BOX.
END.

