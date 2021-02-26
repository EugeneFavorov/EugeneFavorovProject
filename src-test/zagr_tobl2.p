/*
  Глухов
*/
{globals.i}
{intrface.get tmess}   

DEFINE VARIABLE mFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mreadyFName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpStr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mZagol        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFirst        AS LOG       NO-UNDO.
DEFINE VARIABLE mLineN        AS INT64     NO-UNDO.
DEFINE VARIABLE mI            AS INT64     NO-UNDO.
DEFINE VARIABLE mSpisok       AS CHARACTER NO-UNDO.
DEF VAR ffname AS CHAR VIEW-AS FILL-IN NO-UNDO.

DEFINE VARIABLE mDate            AS DATE     NO-UNDO.
DEFINE VARIABLE mOst             AS DECIMAL     NO-UNDO.


FUNCTION transh1 RETURNS CHARACTER (INPUT iCC AS CHARACTER):
DEFINE BUFFER loan FOR loan.
DEFINE VARIABLE vTransh AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTrNMax  AS INT64 NO-UNDO.
   vTrNMax = 0.
   FOR EACH loan WHERE loan.contract EQ "Кредит" AND loan.cont-code BEGINS iCC + " "
      AND loan.close-date EQ ? NO-LOCK:
      IF INT64(ENTRY(2,loan.cont-code," ")) GE vTrNMax THEN 
      ASSIGN
         vTrNMax = INT64(ENTRY(2,loan.cont-code," "))
         vTransh = loan.cont-code.
   END.
   RETURN vTransh.
END FUNCTION.

DEFINE TEMP-TABLE tt-tobl NO-UNDO
   FIELD cont-code AS CHARACTER
   FIELD data      AS DATE
   FIELD Summa     AS DECIMAL
   FIELD ostatok   AS DECIMAL
   INDEX i1 IS UNIQUE PRIMARY cont-code data.


ASSIGN
   mDate = DATE("16/09/2016")
   mSpisok = "047-КЛ;048-КЛ;051-КЛ;052-КЛ;053-КЛ;055-КЛ"
  .


ffname = './graf.csv'.

{getfile.i &filename=ffname &mode=must-exist }

mFileName = SEARCH(pick-value).


IF  mFileName EQ ? THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("","","1","Файл не выбран! Работа завершена!").
   RETURN "".
END.

DEFINE FRAME iParam 
   mDate    LABEL "За дату"  FORMAT "99.99.9999"
   mSpisok   LABEL "Список линий" FORMAT "x(60)"
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ Параметры ]".

ON F1 OF mDate
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN mDate:SCREEN-VALUE = pick-value.
   END.

PAUSE 0.
UPDATE 
   mDate
   mSpisok
   WITH FRAME iParam. 

IF LAST-EVENT:FUNCTION  EQ "END-ERROR"
   THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.



mLineN = 0.

INPUT FROM VALUE(mFileName).
MC1:
REPEAT:
   IMPORT UNFORMATTED mTmpStr. 
   mLineN = mLineN + 1.
   IF mLineN = 1 THEN
   DO:
      mZagol = mTmpStr.
      DO mI = 2 TO NUM-ENTRIES(mZagol,";"):
         IF LOOKUP(TRIM(ENTRY(mI,mZagol,";")),mSpisok,";") > 0 THEN
            ENTRY(mI,mZagol,";") = transh1(TRIM(ENTRY(mi,mZagol,";")) + "@0000").
         ELSE
            NEXT MC1.
      END.
   END.
   ELSE
   DO:
      DO mI = 2 TO NUM-ENTRIES(mTmpStr,";"):
         CREATE tt-tobl.
         ASSIGN
            tt-tobl.cont-code = ENTRY(mi,mZagol,";")
            tt-tobl.summa     = DEC(ENTRY(mI,mtmpstr,";"))
            tt-tobl.data      = DATE(ENTRY(1,mtmpstr,";")).
      END.
   END.
END.
INPUT CLOSE.

{setdest.i}

FOR EACH tt-tobl EXCLUSIVE-LOCK BREAK BY tt-tobl.cont-code BY tt-tobl.data : 
   IF FIRST-OF(tt-tobl.cont-code) THEN
   DO:
      mOst = 0.
      FIND FIRST term-obl WHERE term-obl.contract EQ "кредит"
         AND term-obl.cont-code EQ tt-tobl.cont-code
         AND term-obl.end-date EQ mDate
         AND term-obl.idnt EQ 2 NO-LOCK NO-ERROR.
      IF AVAILABLE term-obl THEN mOst = term-obl.amt-rub.
      ELSE MESSAGE tt-tobl.cont-code + " нет первого остатка".
   END.
   IF mOst GT 0 THEN
      ASSIGN
         mOst = mOst - tt-tobl.summa
         tt-tobl.ostatok = mOst.

END.


DO mI = 2 TO NUM-ENTRIES(mZagol,";"):
   FOR EACH term-obl WHERE term-obl.contract EQ "кредит"
      AND term-obl.cont-code EQ ENTRY(mi,mZagol,";")
      AND term-obl.end-date GT mDate
      AND term-obl.idnt EQ 2 EXCLUSIVE-LOCK:

      DELETE term-obl.

   END.
   FOR EACH term-obl WHERE term-obl.contract EQ "кредит"
      AND term-obl.cont-code EQ ENTRY(mi,mZagol,";")
      AND term-obl.end-date GT mDate
      AND term-obl.idnt EQ 3 EXCLUSIVE-LOCK:

      DELETE term-obl.

   END.
     FOR EACH tt-tobl where  tt-tobl.cont-code EQ ENTRY(mi,mZagol,";") NO-LOCK:
               create term-obl.
               assign
                  term-obl.contract   = "кредит"
                  term-obl.cont-code  = tt-tobl.cont-code
                  term-obl.end-date   = tt-tobl.data
                  term-obl.nn         = 1
                  term-obl.idnt       = 2
                  term-obl.acct       = ''
                  term-obl.currency   = ""
                  term-obl.amt-rub    = tt-tobl.ostatok
                  term-obl.fuser-id   = userid('bisquit').
               create term-obl.
               assign
                  term-obl.contract   = "кредит"
                  term-obl.cont-code  = tt-tobl.cont-code
                  term-obl.end-date   = tt-tobl.data
                  term-obl.dsc-beg-date   = tt-tobl.data
                  term-obl.fop-date   = mDate
                  term-obl.nn         = 1
                  term-obl.idnt       = 3
                  term-obl.acct       = ''
                  term-obl.currency   = ""
                  term-obl.amt-rub    = tt-tobl.summa
                  term-obl.fuser-id   = userid('bisquit').
     end.

END.


{preview.i}
/*RUN instview.p(TEMP-TABLE tt-tobl:HANDLE).*/










