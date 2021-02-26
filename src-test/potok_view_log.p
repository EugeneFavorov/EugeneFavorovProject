/*
 ПАО ПлюсБанк
 А. Глухов
*/

&scop logpath "/home2/bis/quit41d/log/autocred/"

{globals.i}
{intrface.get tmess}
{ttretval.def}
DEFINE INPUT PARAMETER iparam AS CHARACTER NO-UNDO.

DEFINE VARIABLE mDateS     AS DATE      NO-UNDO.
DEFINE VARIABLE mFilial    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUser      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpkind    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFname     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMenuId    AS INT64     NO-UNDO.
DEFINE VARIABLE mPotok     AS INT64     NO-UNDO.
DEFINE VARIABLE mNpp       AS INT64     NO-UNDO.
DEFINE VARIABLE mallFinish AS CHAR      NO-UNDO.
DEFINE VARIABLE mallPotok  AS INT64     NO-UNDO.
DEFINE VARIABLE mallOpkind AS INT64     NO-UNDO.
DEFINE VARIABLE mGoodPotok AS INT64     NO-UNDO.
DEFINE VARIABLE mMaxTime   AS DATETIME-TZ NO-UNDO. 
DEFINE VARIABLE mMaxRID    AS RECID NO-UNDO. 

DEFINE BUFFER bcode FOR code.

DEFINE STREAM sIn.

DEFINE TEMP-TABLE tlog NO-UNDO
   FIELD potok      AS INT64 FORMAT ">9" COLUMN-LABEL "Поток"
   FIELD opkind     AS CHARACTER FORMAT "x(15)" COLUMN-LABEL  "Транзакция"
   FIELD start      AS CHAR COLUMN-LABEL "Запущено"
   FIELD finish     AS CHAR COLUMN-LABEL "Завершено"
   FIELD allfinish  AS CHAR COLUMN-LABEL "Поток завершен"
   FIELD iserror    AS LOG
   INDEX idx IS PRIMARY potok opkind
   INDEX idx1 potok start.


DEFINE TEMP-TABLE tlogmess NO-UNDO
   FIELD npp        AS INT64
   FIELD potok      AS INT64
   FIELD opkind     AS CHARACTER
   FIELD mess       AS CHARACTER
   INDEX idx IS PRIMARY potok opkind npp.


ASSIGN
   mDateS  = TODAY 
   mFilial = shFilial.

IF iparam EQ "100" THEN
   ASSIGN
      mUser   = "SERVCRED"
      mMenuId = 0
      .
ELSE 
DO:


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

   FIND FIRST code WHERE RECID(code) EQ mMaxRID
   NO-LOCK NO-ERROR.

   IF AVAILABLE code THEN 
      ASSIGN
         mUser   = code.val
         mMenuId = INT64(ENTRY(2,code.code))
         mDateS  = DATE(ENTRY(1,code.code))
         .
END.

DEFINE FRAME iParam 
   mFilial   LABEL "Филиал           " FORMAT "X(4)"
   mMenuId   LABEL "Номер пункта меню"  FORMAT ">9"
   mUser     LABEL "Пользователь     "  FORMAT "x(12)"
   mDates    LABEL "Дата запуска     "  FORMAT "99.99.99"

   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ ЗАДАЙТЕ ПАРАМЕТРЫ ]".


ON 'F1' OF mMenuId
DO:
   DO TRANSACTION:
      {empty ttretval}
      RUN browseld.p("user-proc",
                     "procedure~001RetRcp~001RetFld~001ActionLock",
                     "run14~001" + STRING (TEMP-TABLE ttRetVal:HANDLE) + "~001name-proc~001F1,F2,INS,F9,DEL,ENTER", 
                     "",
                     4).

      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN 
         FOR EACH ttretval:
             mMenuId:SCREEN-VALUE = STRING(INT64(ENTRY(1,ttretval.pickvalue," "))).
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

ON F1 OF mDates
DO:
   DO TRANS:
      pick-value = ?.
      RUN calend.p.
      IF  ( LASTKEY EQ KEYCODE("ENTER")
           OR LASTKEY EQ 10 )
           AND pick-value NE ?
      THEN ASSIGN mDates:SCREEN-VALUE = pick-value.
   END.
END.


PAUSE 0.
UPDATE 
   mFilial
   mMenuId
   mUser
   mDates
   WITH FRAME iParam. 
IF LAST-EVENT:FUNCTION  EQ "END-ERROR" THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.

RUN LoadPotokLog IN THIS-PROCEDURE.
{setdest.i &filename="'potokk14.log'"}.
RUN ViewPotokLog IN THIS-PROCEDURE.
{preview.i &filename="'potokk14.log'"}.



PROCEDURE ViewPotokLog.
   FOR EACH tlog BREAK BY tlog.potok:
      IF FIRST-OF(tlog.potok) THEN
         mallPotok = mallPotok + 1.
      IF LAST-OF(tlog.potok) THEN
         IF tlog.allfinish NE "" THEN
            mGoodPotok = mGoodPotok + 1.
END.


PUT UNFORMATTED "Результаты запуска п." mMenuId " пользователем " mUser " за  " mDates SKIP.
PUT UNFORMATTED "-----------------------------------------------------------" SKIP.
PUT UNFORMATTED "Всего потоков " mallPotok " из них завершено " mGoodPotok SKIP.

FOR EACH tlog BREAK BY tlog.potok BY tlog.start:
      DISP tlog.potok
           tlog.opkind
           start
           (IF tlog.iserror THEN  "Ошибка" ELSE finish) @ finish
           allfinish.
END.
PUT UNFORMATTED "-----------------------------------------------------------" SKIP " " SKIP " " SKIP.

FOR EACH tlogmess BREAK BY tlogmess.opkind BY tlogmess.potok BY tlogmess.npp:
   IF FIRST-OF (tlogmess.opkind) THEN
   DO:
     PUT UNFORMATTED " "  SKIP " "  SKIP.      
     PUT UNFORMATTED "Сообщения транзакции " tlogmess.opkind SKIP.      
     PUT UNFORMATTED "----------------------" SKIP .
   END.
   IF FIRST-OF (tlogmess.potok) THEN
      PUT UNFORMATTED "####" tlogmess.potok "####" SKIP.      
   PUT UNFORMATTED tlogmess.mess SKIP.      
END.


END PROCEDURE.

PROCEDURE LoadPotokLog.
   DO mPotok = 0 TO 99:
      mFname = {&logpath} + TRIM(STRING(mMenuId)) + "_" + mUser + "_" + 
         string(year(mDateS)) + 
         string(month(mDateS),"99") + 
         string(day(mDateS),"99") + "_" + 
         mFilial + "_" + 
         TRIM(STRING(mPotok)) + ".log".

      ASSIGN
         mFname = SEARCH(mFname)
         mAllFinish = ""
         .

      IF mFname NE ? THEN 
      DO:
         INPUT STREAM sIn FROM VALUE(mFname).
         mnpp = 0.
         REPEAT:

            IMPORT STREAM sIn UNFORMATTED mstr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
               RUN Fill-SysMes IN h_tmess ("","","1","Ошибка загрузки лога: " + ERROR-STATUS:GET-MESSAGE(1)).

            CASE ENTRY(1,mstr,"="):
               WHEN  "<#ТранзакцияЗапуск#>" THEN 
                  DO:
                     FIND FIRST tlog WHERE tlog.potok  EQ mPotok 
                                       AND tlog.opkind EQ ENTRY(2,mstr,"=")
                     NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE tlog THEN
                     DO: 
                        CREATE tlog.
                        ASSIGN
                           mOpkind     = ENTRY(2,mstr,"=")
                           mnpp        = 1
                           tlog.potok  = mPotok 
                           tlog.opkind = mOpkind
                           tlog.start  = ENTRY(2,STRING(DATETIME(ENTRY(3,mstr,"=")))," ").
                     END.
                  END.
               WHEN  "<#ТранзакцияВыполнена#>" THEN 
                  DO:
                     FIND FIRST tlog WHERE tlog.potok  EQ mPotok 
                                       AND tlog.opkind EQ mOpkind
                        EXCLUSIVE-LOCK NO-ERROR.
                     ASSIGN
                        tlog.finish  = ENTRY(2,STRING(DATETIME(ENTRY(3,mstr,"=")))," ").
                  END.
               WHEN  "<#Ошибка выполнения транзакции#>" THEN 
                  DO:
                     FIND FIRST tlog WHERE tlog.potok  EQ mPotok 
                                       AND tlog.opkind EQ mOpkind
                        EXCLUSIVE-LOCK NO-ERROR.
                     ASSIGN
                        tlog.iserror  = Yes.
                  END.
               WHEN  "<#Закончено#>" THEN 
                  DO:
                     ASSIGN
                        mAllFinish  = ENTRY(2,STRING(DATETIME(ENTRY(2,mstr,"=")))," ").
                  END.
               OTHERWISE
               DO:   
                  CREATE tlogmess.
                  ASSIGN
                     tlogmess.npp    = mnpp 
                     tlogmess.potok  = mPotok
                     tlogmess.opkind = mopkind
                     tlogmess.mess   = TRIM(mstr)
                     mnpp            = mnpp  + 1
                     .
               END.
            END CASE.
         END.

         INPUT STREAM sIn CLOSE.

         FOR EACH tlog WHERE tlog.potok EQ mPotok EXCLUSIVE-LOCK:
            tlog.allfinish = mAllFinish.
         END.

      END.

   END.
END.