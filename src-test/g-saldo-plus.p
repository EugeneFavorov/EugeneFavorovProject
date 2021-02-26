/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  g-saldo.p
      Comment:  Сальдирование остатков

      Created:  15/10/1997 peter
     Modified:  21/04/2000 eagle добавлена маска номера счета в выборке по счетам
     Modified: 09.09.2002 15:08 SEMA     по заявке 0009554 подъем функционала (выборка счетов по маске) из версии Гута
     Modified: 11.04.2003       NIK      Возврат ошибки при отказе от обработки
     Modified: 12.05.2003       NIK      Добавлен параметр:
                                         iRate - выполнять перенос по курсу ЦБ
     Modified: 19/10/2005 kraw (0052878) совместимость с Oracle
     Modified: 21.11.2005 TSL    Многофилиальность
     Modified: 19/07/2007 kraw (0073795) Вопросы при запуске
*/

/*
  ERROR codes:
    0 - NO-ERROR
    1 - bad currency
    2 - missing acct
    3 - bad contr-acct
    4 - bad side
    5 - contr-acct closed
*/

DEFINE INPUT PARAMETER in-op-date LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER oprid      AS recid        NO-UNDO.
DEFINE INPUT PARAMETER iRate      AS LOGICAL      NO-UNDO.

DEF VAR a-sh-bal LIKE acct-pos.balance NO-UNDO.
DEF VAR a-sh-val LIKE acct-cur.balance NO-UNDO.
DEF VAR a-amt    LIKE op-entry.amt-rub NO-UNDO.
DEF VAR a-cur    LIKE op-entry.amt-cur NO-UNDO.
DEF VAR rate-amt AS   DECIMAL          NO-UNDO.

DEF VAR fmt       AS CHAR             NO-UNDO.
DEF VAR mListAcct AS CHAR INITIAL "*" NO-UNDO.
DEF VAR c-entry   AS INT64.

DEFINE VARIABLE mStart     AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE mIsMesShow AS LOGICAL NO-UNDO.
DEFINE VARIABLE mAlong     AS LOGICAL NO-UNDO.

DEFINE VARIABLE mTextMess AS CHARACTER NO-UNDO.

{def-wf.i NEW}
{g-docnum.def}
{intrface.get instrum}
{intrface.get xclass}
DEF TEMP-TABLE acct-pair NO-UNDO
    FIELD acct-cat   LIKE acct.acct-cat
    FIELD acct       LIKE acct.acct       INIT ""
    FIELD side       LIKE acct.side
    FIELD currency   LIKE acct.currency   INIT ""
    FIELD contr-acct LIKE acct.contr-acct INIT ""
    FIELD ERROR      AS INT64           INIT 0 COLUMN-LABEL "КОД ОШИБКИ"
    INDEX acct-cur is unique primary acct-cat acct currency
    INDEX err-code ERROR
    INDEX error-acct-side ERROR acct currency side.

DEF VAR errors AS CHAR FORMAT "x(30)" EXTENT 5
    INIT ['Парный счет не открыт в нужной валюте',
          'Не открыт парный счет',
          'Несоответствие парных счетов',
          'Несоответствие признака ""Актив/Пассив""',
          'Парный счет закрыт'] NO-UNDO.

DEF BUFFER b2-acct FOR acct.
{g-defs.i}
{checkstart.i &set-code="'СальдоЗапуск'" }

/*============================================================================*/
PAUSE 0.
pick-value = 'no'.

FIND FIRST op-kind WHERE recid(op-kind) EQ oprid NO-LOCK.

gen:
DO TRANSACTION ON ERROR UNDO, LEAVE:

   {op(ok-plus.del
       &op-kind=op-kind.op-kind
       &date=in-op-date
       &stat=yes
      &set-code="'СальдоУдал'"
      &mes1="В этом дне уже проводилось сальдирование"
      &mes2="Удалить его ?"
      &mes3="Удаление документов сальдирования..."
   }

   PAUSE 0.

   mAlong = GetXAttrValueEx ("op-kind",op-kind.op-kind,"along","") = "много документов".

   FOR EACH op-templ OF op-kind NO-LOCK:
      mListAcct = GetXAttrValueEx ("op-template",
                                   op-kind.op-kind + "," + string(op-templ.op-templ),
                                   "lstacct",
                                   "*").

      FOR EACH acct WHERE acct.filial-id   EQ shFilial
                      AND acct.acct-cat    EQ op-templ.acct-cat
                      AND CAN-DO(mListAcct,acct.acct)
                      AND acct.contr-acct  NE ""
                      AND (acct.close-date EQ ? OR 
                           acct.close-DATE GE in-op-date
                          ) NO-LOCK:
         
         /*Пропускаем счета по агентским вознаграждениям*/
         IF  acct.bal-acct  EQ 47422
         AND acct.branch-id EQ "0009" THEN NEXT.
         IF  acct.bal-acct  EQ 47423
         AND acct.branch-id EQ "0009" THEN NEXT.
          
         CREATE acct-pair.
         ASSIGN
            acct-pair.acct-cat   = acct.acct-cat
            acct-pair.acct       = acct.acct
            acct-pair.currency   = acct.currency
            acct-pair.contr-acct = acct.contr-acct
            acct-pair.side       = acct.side
         .
         FIND FIRST xacct WHERE xacct.acct-cat EQ acct.acct-cat
                            AND xacct.acct     EQ acct.contr-acct
                            AND xacct.currency EQ acct.currency
                                NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(xacct) THEN DO:
            IF CAN-FIND(xacct WHERE xacct.acct-cat EQ acct.acct-cat
                                AND xacct.acct     EQ acct.contr-acct)
               THEN acct-pair.ERROR = 1.
               ELSE acct-pair.ERROR = 2.
         END.
         ELSE IF xacct.contr-acct NE acct.acct THEN acct-pair.ERROR = 3.
         ELSE IF xacct.side       EQ acct.side THEN acct-pair.ERROR = 4.
         ELSE IF xacct.close-date NE ?         THEN DO:
            IF xacct.close-date GE in-op-date THEN DO:
               RUN acct-pos IN h_base (acct-pair.acct,
                                       acct-pair.currency,
                                       in-op-date,
                                       in-op-date,
                                       "П").
               a-sh-bal = sh-bal.
               RUN acct-pos IN h_base (acct-pair.contr-acct,
                                       acct-pair.currency,
                                       in-op-date,
                                       in-op-date,
                                       "П").
               
               acct-pair.ERROR = IF ABS(a-sh-bal) + ABS(sh-bal) = 0
                                 THEN 0
                                 ELSE 5.
            END.
            ELSE
               acct-pair.ERROR = 5.
         END.
      END.                             /* FOR EACH acct WHERE acct.acct-cat   */
   END.                                /* FOR EACH op-templ OF op-kind        */

   FORM  acct-pair.acct
         acct-pair.currency
         acct-pair.contr-acct
         errors[1]            COLUMN-LABEL "ПРОБЛЕМА"
   WITH FRAME fr-error DOWN WIDTH 88.

   FIND FIRST acct-pair WHERE acct-pair.ERROR NE 0 NO-LOCK NO-ERROR.
   IF AVAIL acct-pair THEN DO:
      pick-value = 'no'.

      mIsMesShow = NO.

      FOR EACH acct-pair WHERE acct-pair.ERROR NE 0
         NO-LOCK:

         mTextMess = 
           STRING(IF acct-pair.acct = ? THEN "?" ELSE acct-pair.acct) + " " +
           STRING(IF acct-pair.currency = ? THEN "?" ELSE acct-pair.currency) + " " +
           STRING(IF acct-pair.contr-acct = ? THEN "?" ELSE acct-pair.contr-acct) + " " +
           STRING(errors[1]).
         
         RUN dbgprint.p ("g-saldo-plus.p",mTextMess).              
         PAUSE 0.
         
         IF acct-pair.ERROR EQ 5 THEN
         DO:
            mTextMess = "По счету " + acct-pair.acct + " парный счет закрыт. Нельзя провести сальдирование".
            RUN dbgprint.p ("g-saldo-plus.p",mTextMess).
            PAUSE 0.
            NEXT.
         END.
      END.                                       /* FOR EACH acct-pair        */
   END.                                          /* IF AVAIL acct-pair THEN   */
   ELSE
      pick-value = "yes".

   pick-value = "yes".

   RELEASE acct.
   RELEASE xacct.
   RELEASE op-templ.
   RELEASE acct-pair.

   cur-op-date = in-op-date.
   FOR EACH op-templ OF op-kind NO-LOCK
      ON ENDKEY UNDO, LEAVE
      ON ERROR  UNDO, LEAVE:

      IF NOT mAlong THEN DO:
         CREATE op.
         {op(sess).cr}
         {g-op.ass}
         VALIDATE op.
        
         FIND FIRST doc-type OF op NO-LOCK NO-ERROR.
         c-entry = 1.
      END.

      FOR EACH acct-pair WHERE acct-pair.acct-cat EQ op-templ.acct-cat
                           AND acct-pair.side     EQ "А"
                           AND acct-pair.ERROR    EQ 0 NO-LOCK:

         IF acct-pair.currency NE "" THEN DO:
            FIND FIRST acct WHERE acct.acct     EQ acct-pair.acct
                              AND acct.currency EQ acct-pair.currency
                                  NO-LOCK NO-ERROR.
            IF iRate AND acct.rate-type EQ "УЧЕТНЫЙ"
               THEN rate-amt = FindRateWork(acct.rate-type,
                                            acct-pair.currency,
                                            in-op-date).
         END.

         RUN acct-pos IN h_base (acct-pair.acct,
                                 acct-pair.currency,
                                 in-op-date,
                                 in-op-date,
                                 "П").
         ASSIGN
            a-sh-bal = sh-bal
            a-sh-val = sh-val
         .
         RUN acct-pos IN h_base (acct-pair.contr-acct,
                                 acct-pair.currency,
                                 in-op-date,
                                 in-op-date,
                                 "П").
         ASSIGN
            a-amt = max(a-sh-bal + sh-bal, 0)
            a-cur = max(a-sh-val + sh-val, 0)
         .

         IF (acct-pair.currency EQ "" AND a-amt EQ a-sh-bal) OR
            (acct-pair.currency NE "" AND a-cur EQ a-sh-val) THEN NEXT.

         IF mAlong THEN DO:

            CREATE op.
            {op(sess).cr}
            {g-op.ass}
            VALIDATE op.
           
            FIND FIRST doc-type OF op NO-LOCK NO-ERROR.         
            c-entry = 1.

         END.

         CREATE op-entry.
         {g-en.ass &ind=c-entry}
         ASSIGN
            op-entry.value-date = in-op-date
            op-entry.acct-db    = (IF (acct-pair.currency EQ "" AND a-amt GT a-sh-bal) OR
                                      (acct-pair.currency NE "" AND a-cur GT a-sh-val)
                                      THEN acct-pair.acct
                                      ELSE acct-pair.contr-acct)
            op-entry.acct-cr    = (IF op-entry.acct-db EQ acct-pair.acct
                                      THEN acct-pair.contr-acct
                                      ELSE acct-pair.acct)
            op-entry.currency   = acct-pair.currency
            op-entry.amt-cur    = (IF acct-pair.currency NE ""
                                      THEN (IF op-entry.acct-db EQ acct-pair.acct
                                               THEN (a-cur    - a-sh-val)
                                               ELSE (a-sh-val - a-cur))
                                      ELSE 0)
            op-entry.amt-rub    = (IF acct-pair.currency NE ""        AND
                                      iRate                           AND
                                      acct.rate-type     EQ "УЧЕТНЫЙ"
                                      THEN op-entry.amt-cur * rate-amt
                                      ELSE (IF op-entry.acct-db EQ acct-pair.acct
                                               THEN (a-amt    - a-sh-bal)
                                               ELSE (a-sh-bal - a-amt)))
            c-entry             = c-entry + 1
         .

         {g-psigns.i}

         RELEASE op-entry.

         IF mAlong THEN RUN PutDocNum.

      END.                                       /* FOR EACH acct-pair...     */

      IF NOT mAlong THEN RUN PutDocNum.
   END.                                          /* FOR EACH op-templ...      */

   pick-value = 'yes'.
END.

HIDE MESSAGE NO-PAUSE.

{intrface.del instrum}

IF pick-value NE "yes" THEN
   RETURN ERROR "error".
/******************************************************************************/

PROCEDURE PutDocNum:

   IF NOT CAN-FIND (FIRST op-entry OF op NO-LOCK) THEN
      DELETE op.
   ELSE
      IF NOT autonumdoc THEN DO:              /* Автонумерация             */
         ASSIGN
            docnum-op   = string (recid (op))
            docnum-tmpl = GetXattrValue("op-template",
                                        op.op-kind + "," +
                                        string(op.op-template),
                                        "ДокНомер")
         .
         IF {assigned docnum-tmpl} THEN DO:
            {g-docnum.i}
         END.
      END.
   
   RELEASE op.

END PROCEDURE.
