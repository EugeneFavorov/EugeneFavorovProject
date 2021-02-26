/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: ved-acct.p
      Comment: Ведомости расчетных счетов (открытых, не работающих более года,
               счетов, по кот. осуществл. только безнал. операции
   Parameters: iParam - если "РасчСчета", то ведомость расчетных счетов
                      - если "НеРабСчета", то счета, не работающие более года
                      - если "РСчетаБезнал", то открытые счета только с безналич.
                        операциями
         Uses:
      Used by:
      Created: 17/04/1003 kolal (из pos-nal2.p)
     Modified: 17/04/2003 kolal Заявка 13134.
     Modified: 16/07/2003 kolal Исправлено простановка поля "телефон". Заявка 13134.
     Modified: 24/05/2004 ABKO 28659 - изменена сортировка для "НеРабСчета","РСчетаБезнал"
     Modified: 12/01/2006 kraw (0056474) "НеРабСчета" теперь названия печатаются правильно
     Modified: 27.04.2006 TSL  Корректный доступ к данным фильтра счетов
*/
&SCOP DEFAULT_MASK "401*,402*,403*,404*,405*,406*,407*"
DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO. /* Строка входных параметров */

{globals.i}             /* Глобальные переменные сессии. */
{flt-val.i}    
{tmprecid.def}          /* Таблица отметок. */

DEFINE VARIABLE mCustName AS CHARACTER /* Наименование клиента */
   FORMAT "x(40)"
   EXTENT 10
   NO-UNDO.
DEFINE VARIABLE mIndex        AS INT64 NO-UNDO. /* Счетчик для вывода полного
                                                 имени клиента */
DEFINE VARIABLE mBegDate      AS DATE    NO-UNDO. /* Начальная дата для остатков */
DEFINE VARIABLE mLastMove     AS DATE    NO-UNDO. /* Дата последнего движения */
DEFINE VARIABLE mKassAcct     AS CHARACTER INIT "202*,40906*" NO-UNDO.
                                              /* Маска счета для проверки
                                                 наличных операций */
DEFINE VARIABLE mBalAcctMask  AS CHARACTER NO-UNDO. /**/
DEFINE VARIABLE mTelephone    AS CHARACTER NO-UNDO. /* телефон клиента */
DEFINE VARIABLE vINN          AS CHARACTER NO-UNDO. /* ИНН */
DEFINE VARIABLE mStrLastMove  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCurrClId         AS INT64 NO-UNDO. /* Текущее значение для fClId */
DEFINE VARIABLE mCurrBalAcct      AS INT64 NO-UNDO. /* Текущее значение для fbal-acct */
DEFINE VARIABLE mCountBalAcct AS INT64 NO-UNDO. /* Счетчик счетов а группе */
DEFINE VARIABLE mCountAll     AS INT64 NO-UNDO. /* Счетчик счетов */

DEFINE TEMP-TABLE tmprwd
   FIELD fBal-acct AS INT64 /* Счет 1-го порядка */
   FIELD fClId     AS INT64 
   FIELD fRwd      AS ROWID   /* Для выбора по фильтру */
   FIELD fName     AS CHAR    /* Для сортировки */
  INDEX idxB fBal-acct.

{getdate.i}
{wordwrap.def}
{sh-defs.i}

/* Редактируем маску */
IF iParam = "РСчетаБезнал" THEN
DO:
   PAUSE 0.
   DO
      ON ERROR UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE
      WITH FRAME KassAcctFrame:

      UPDATE
         mKassAcct FORMAT "x(100)"
            LABEL "Счета наличных операций"
            HELP  "Введите маску для счетов наличных операций"
            VIEW-AS FILL-IN SIZE 20 BY 1
      WITH CENTERED ROW 10 OVERLAY SIDE-LABELS
         COLOR messages TITLE "[ ЗАДАЙТЕ МАСКУ СЧЕТОВ ]"
      EDITING:
         READKEY.
         APPLY LASTKEY.
      END.
   END.

   HIDE FRAME KassAcctFrame NO-PAUSE.

   IF KEYFUNC(LASTKEY) = "end-error" THEN
      RETURN.
END.

/* Выборка в списке счетов может быть открыта
   не с той сортировкой, или по ДР.
   Поэтому перекинем все в ТТ                   */
mBalAcctMask = IF GetFltVal("bal-acct") NE "*"
               THEN GetFltVal("bal-acct")
               ELSE {&DEFAULT_MASK}.

                        /* Формируем счета для отчета. */
mIndex = 0.
FOR EACH TmpRecId,
FIRST acct WHERE
         RECID (acct) EQ TmpRecId.id
   AND   CAN-DO (mBalAcctMask, STRING(acct.bal-acct))
NO-LOCK:
   mIndex = mIndex + 1.
   CREATE tmprwd.
   ASSIGN
      tmprwd.fRwd       = ROWID (acct)
      tmprwd.fClId      = acct.cust-id
      tmprwd.fBal-acct  = INT64 (SUBSTRING (STRING (acct.bal-acct), 1, 3))
   .
END.

mIndex = 0.

DEFINE NEW GLOBAL SHARED VARIABLE usr-printer LIKE PRINTER.PRINTER NO-UNDO.
/* Из setdest.i для избежания повторного определения */
IF iParam = "РасчСчета" THEN
DO:
   {setdest.i &nodef="/*" &cols=120}
END.
ELSE
DO:
   {setdest.i &nodef="/*" &cols=82}
END.

PUT UNFORMATTED name-bank SKIP(1).
CASE iParam:
   WHEN "РасчСчета" THEN
      PUT UNFORMATTED
         "Ведомость расчетных счетов на " + STRING(end-date,"99/99/9999") SKIP.
   WHEN "НеРабСчета" THEN
      PUT UNFORMATTED
         "Ведомость счетов, не работающих более года на " + STRING(end-date,"99/99/9999") SKIP.
   WHEN "РабСчета" THEN
      PUT UNFORMATTED
         "Ведомость счетов, работающих в течении года на " + STRING(end-date,"99/99/9999") SKIP.         
   WHEN "РСчетаБезнал" THEN
      PUT UNFORMATTED
         "Ведомость счетов предприятий, по счетам которых осуществляются операции" SKIP
         "в безналичном порядке и не осуществляются операции с наличными деньгами" SKIP
         "на " + STRING(end-date,"99/99/9999") SKIP.
END CASE.

IF iParam = "РасчСчета" THEN
   PUT UNFORMATTED "╒═════════════════════════╤════════════════════════════════════════╤════════╤═══════════════╤════════╤════════╤════════╕" SKIP
                   "│     Расчетный счет      │        Наименование клиента            │ Открыт │    Телефон    │        │        │        │" SKIP
                   "╞═════════════════════════╪════════════════════════════════════════╪════════╪═══════════════╪════════╪════════╪════════╡" SKIP.
ELSE
   PUT UNFORMATTED "╒═════════════════════════╤════════════════════════════════════════╤════════════╕" SKIP
                   "│     Расчетный счет      │        Наименование клиента            │    ДПД     │" SKIP
                   "╞═════════════════════════╪════════════════════════════════════════╪════════════╡" SKIP.

IF iParam = "РасчСчета" THEN
DO:
FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?)
      NO-LOCK
   BREAK BY tmprwd.fClId:

   /* Доп. условия отбора */

   IF mCurrClId <> tmprwd.fClId THEN
   DO:
      IF mCountBalAcct NE 0 THEN
      DO:
         PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────┼───────────────┼────────┼────────┼────────┤" SKIP.
         PUT UNFORMATTED "│" +
                         FILL(" ", 25) +
                         "│    Итого по счетам " +
                         STRING(mCurrBalAcct, "999") +
                         ": " +
                         STRING(mCountBalAcct, ">>>>9") +
                         " счетов   " +
                         "│" +
                         FILL(" ", 8) +
                         "│" +
                         FILL(" ", 15) +
                         "│        │        │        │"
                         SKIP.
         PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────┼───────────────┼────────┼────────┼────────┤" SKIP.
      END.
      ASSIGN
         mCurrclid = tmprwd.fClId
         mCurrBalAcct = tmprwd.fBal-acct
         mCountBalAcct = 0
      .
   END.

   ASSIGN
      mCountBalAcct = mCountBalAcct + 1
      mCountAll = mCountAll + 1
   .

   {getcust.i &name="mCustName" &OFFinn = "/*"}

   mCustName[1] = mCustName[1] + " " + mCustName[2].
   {wordwrap.i &s=mCustName &n=10 &l=40}

   IF acct.cust-cat = "Б" THEN
   DO:
      mTelephone = GetXAttrValue("banks", STRING(acct.cust-id), "fax").
      IF mTelephone = "" THEN
         mTelephone = GetXAttrValue("banks", STRING(acct.cust-id), "tel").
   END.
   ELSE
      IF acct.cust-cat = "Ю" THEN
      DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id
            NO-LOCK
            NO-ERROR.
         IF AVAIL cust-corp THEN
            mTelephone = cust-corp.fax.
         IF    mTelephone = ?
            OR mTelephone = "" THEN
            mTelephone = GetXAttrValue("cust-corp", STRING(acct.cust-id), "tel").
      END.
      ELSE
      DO:
         FIND person WHERE person.person-id = acct.cust-id
            NO-LOCK
            NO-ERROR.
         IF AVAIL person THEN
         DO:
            mTelephone = person.phone[1].
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = person.phone[2].
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = GetXattrValue("person",STRING(person.person-id),"cell-phone").
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = person.fax.
         END.
         IF    mTelephone = ?
            OR mTelephone = "" THEN
            mTelephone = "".
      END.

   PUT UNFORMATTED "│" +
                   STRING(acct.acct, "x(25)") +
                   "│" +
                   STRING(mCustName[1], "x(40)") +
                   "│" +
                   STRING(acct.open-date, "99/99/99") +
                   "│" +
                   STRING(mTelephone, "x(15)") +
                   "│        │        │        │"
                   SKIP.

   mIndex = 2.
   DO WHILE mCustName[mIndex] NE ""
      AND mIndex LE 10 :
      PUT UNFORMATTED "│" +
                      FILL(" ", 25) +
                      "│" +
                      STRING(mCustName[mIndex], "x(40)") +
                      "│" +
                      FILL(" ", 8) +
                      "│" +
                      FILL(" ", 15) +
                      "│        │        │        │"
                      SKIP.
      mIndex = mIndex + 1.
   END.

END.
END.
ELSE DO:
   FOR EACH tmprwd,
      FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                   AND (acct.close-date GT end-date
                   OR   acct.close-date EQ ?):
      {getcust.i &name="mCustName" &OFFinn = "/*"}
      tmprwd.fName = mCustName[1] + " " + mCustName[2].
   END.

   FOR EACH tmprwd,
      FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                   AND (acct.close-date GT end-date
                   OR   acct.close-date EQ ?)
         NO-LOCK
      BREAK BY tmprwd.fClId
      BY tmprwd.fBal-acct:
          
      IF FIRST-OF(tmprwd.fClId) THEN
      DO:
         IF mCountBalAcct GT 1 THEN
         DO:
            PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────────┤" SKIP.
            PUT UNFORMATTED "│" +
                            FILL(" ", 25) +
                            "│    Итого счетов : " +
                            STRING(mCountBalAcct, ">>>>9") +
                            FILL(" ", 16) +
                            "│" SKIP.
            PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────────┤" SKIP.
         END.
         ELSE IF mCountBalAcct EQ 1 THEN
            PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────────┤" SKIP.
         ASSIGN
            mCountBalAcct = 0
            mCustName[1] = tmprwd.fName
         .
         {wordwrap.i &s=mCustName &n=10 &l=40}
      END.

      /* Доп. условия отбора */
      IF iParam = "НеРабСчета" THEN DO:
         mBegDate = DATE(MONTH(end-date),
                         DAY(end-date),
                        YEAR(end-date) - 1) NO-ERROR.
         /* А вдруг ввели 29.02.2004 ? */
         IF ERROR-STATUS:ERROR THEN
            mBegDate = DATE(MONTH(end-date),
                            DAY(end-date) - 1,
                            YEAR(end-date) - 1).

         RUN acct-pos IN h_base (acct.acct,
                                 acct.curr,
                                 mBegDate,
                                 end-date,
                                 ?).
         mLastMove = IF acct.curr = "" THEN
                        LastMove
                     ELSE
                        LastCurr.
                        
         /* пропускаем те, у которых было движения за последний год или открыты меньше года */
         IF mLastMove NE ? AND mLastMove GE mBegDate OR acct.open-date GE mBegDate THEN
            NEXT.
      END.
      /* Доп. условия отбора */
      IF iParam = "РабСчета" THEN DO:
         mBegDate = DATE(MONTH(end-date),
                         DAY(end-date),
                        YEAR(end-date) - 1) NO-ERROR.
         /* А вдруг ввели 29.02.2004 ? */
         IF ERROR-STATUS:ERROR THEN
            mBegDate = DATE(MONTH(end-date),
                            DAY(end-date) - 1,
                            YEAR(end-date) - 1).

         RUN acct-pos IN h_base (acct.acct,
                                 acct.curr,
                                 mBegDate,
                                 end-date,
                                 ?).
         mLastMove = IF acct.curr = "" THEN
                        LastMove
                     ELSE
                        LastCurr.
         /* пропускаем те, у которых не было движения за последний год*/
                        
         mStrLastMove = IF mLastMove NE ? THEN STRING(mLastMove,"99/99/9999") ELSE "".         
         IF mLastMove LE mBegDate OR mLastMove EQ ? THEN
            NEXT.
      END.
      IF iParam = "РСчетаБезнал" THEN
      DO:
         FIND FIRST op-entry WHERE (op-entry.acct-db = acct.acct
                                AND CAN-DO(mKassAcct, op-entry.acct-cr))
                                AND op-entry.op-date NE ?
            NO-LOCK NO-ERROR.
         IF AVAIL op-entry THEN
            NEXT.

         FIND FIRST op-entry WHERE (op-entry.acct-cr = acct.acct
                                AND CAN-DO(mKassAcct, op-entry.acct-db))
                                AND op-entry.op-date NE ?
            NO-LOCK NO-ERROR.
         IF AVAIL op-entry THEN
            NEXT.
      END.



      ASSIGN
         mCountBalAcct = mCountBalAcct + 1
         mCountAll = mCountAll + 1
      .


      PUT UNFORMATTED "│" +
                      STRING(acct.acct, "x(25)") +
                      "│" +
                      STRING(mCustName[1], "x(40)") +
                      "│ " +
                      mStrLastMove +  
                      " │" SKIP.

      mIndex = 2.
      DO WHILE mCustName[mIndex] NE ""
         AND mIndex LE 10:
         PUT UNFORMATTED "│" +
                         FILL(" ", 25) +
                         "│" +
                         STRING(mCustName[mIndex], "x(40)") +
                         "│            │" SKIP.
         mIndex = mIndex + 1.
      END.
   END.
END.

IF mCountBalAcct GT (IF iParam EQ "РасчСчета" THEN 0 ELSE 1) THEN
DO:
   PUT UNFORMATTED (IF iParam EQ "РасчСчета" THEN
"├─────────────────────────┼────────────────────────────────────────┼────────┼───────────────┼────────┼────────┼────────┤"
                    ELSE
"├─────────────────────────┼────────────────────────────────────────┼────────────┤") SKIP.
   PUT UNFORMATTED (IF iParam = "РасчСчета" THEN
                       "│" +
                       FILL(" ", 25) +
                       "│    Итого по счетам " +
                       STRING(mCurrBalAcct, "999") +
                       ": " +
                       STRING(mCountBalAcct, ">>>>9") +
                       " счетов   " +
                       "│" +
                       FILL(" ", 8) +
                       "│" +
                       FILL(" ", 15) +
                       "│        │        │        │"
                    ELSE "│" +
                         FILL(" ", 25) +
                         "│    Итого счетов : " +
                         STRING(mCountBalAcct, ">>>>9") +
                         FILL(" ", 16) +
                         "│"
                   ) SKIP.
   PUT UNFORMATTED (IF iParam = "РасчСчета" THEN
"├─────────────────────────┼────────────────────────────────────────┼────────┼───────────────┼────────┼────────┼────────┤"
                       ELSE
"├─────────────────────────┼────────────────────────────────────────┼────────────┤") SKIP.
END.
ELSE IF    mCountBalAcct EQ 1
       AND iParam NE "РасчСчета" THEN
            PUT UNFORMATTED
"├─────────────────────────┼────────────────────────────────────────┼────────────┤" SKIP.

IF mCountAll NE 0 THEN
DO:
   PUT UNFORMATTED (IF iParam = "РасчСчета" THEN
                       "│" +
                       FILL(" ", 25) +
                       "│    Итого " +
                       STRING(mCountAll, ">>>>9") +
                       " счетов" +
                       FILL(" ", 18) +
                       "│" +
                       FILL(" ", 8) +
                       "│" +
                       FILL(" ", 15) +
                       "│        │        │        │"
                    ELSE
                       "│" +
                       FILL(" ", 25) +
                       "│    Итого " +
                       STRING(mCountAll, ">>>>9") +
                       " счетов" +
                       FILL(" ", 18) +
                       "│            │") SKIP.
   PUT UNFORMATTED (IF iParam = "РасчСчета" THEN
"└─────────────────────────┴────────────────────────────────────────┴────────┴───────────────┴────────┴────────┴────────┘"
                    ELSE
"└─────────────────────────┴────────────────────────────────────────┴────────────┘") SKIP.
END.

{signatur.i}
{preview.i}


OUTPUT TO VALUE("ved-acct2.csv") CONVERT TARGET "1251".

FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?):
   {getcust.i &name="mCustName" &OFFinn = "/*"}
   tmprwd.fName = mCustName[1] + " " + mCustName[2].
END.

FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?)
      NO-LOCK:

   mBegDate = DATE(MONTH(end-date),
                   DAY(end-date),
                   YEAR(end-date) - 1) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      mBegDate = DATE(MONTH(end-date),
                      DAY(end-date) - 1,
                      YEAR(end-date) - 1).

   RUN acct-pos IN h_base (acct.acct,
                           acct.curr,
                           mBegDate,
                           end-date,
                           ?).
   mLastMove = IF acct.curr = "" THEN
                  LastMove
               ELSE
                  LastCurr.
   /* пропускаем те, у которых не было движения за последний год*/
   mStrLastMove = IF mLastMove NE ? THEN STRING(mLastMove,"99/99/9999") ELSE "".         
   IF mLastMove LE mBegDate OR mLastMove EQ ? THEN
      NEXT.

   PUT UNFORMATTED
      "'" DelFilFromAcct(acct.acct) ";"
      tmprwd.fName ";"
      acct.open-date ";"
      mStrLastMove
   SKIP.
END.

OUTPUT CLOSE.

MESSAGE 
"Информация выгружена в файл ved-acct2.csv." 
VIEW-AS ALERT-BOX.

RETURN.
