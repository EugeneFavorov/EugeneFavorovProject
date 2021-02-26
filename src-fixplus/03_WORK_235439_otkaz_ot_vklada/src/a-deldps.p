/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: a-deldps.p
      Comment: TT:0235439 Миграция. Отказ клиента от оформления вклада
   Parameters:
         Uses:
      Used by:
      Created: 12/02/2015 14:33 KMBIS Миграция. Отказ клиента от оформления вклада
     Modified: 
*/
DEF INPUT PARAM iOpDate  AS  DATE  NO-UNDO.

{globals.i}
{intrface.get tmess}

DEF VAR mOk      AS  LOG   NO-UNDO. /* Договор успешно удален              */
DEF VAR mLogFile AS  CHAR  NO-UNDO. /* Путь к файлу логов                  */
DEF VAR mMsg     AS  CHAR  NO-UNDO. 

DEF BUFFER bLAcct  FOR loan-acct.

DEF STREAM sExp.

{a-deldps.fun}
{a-deldps.pro}

{setdest.i} 

mLogFile = fGetSetting("ОткОтОф", "Лог", "").
IF NOT {assigned mLogFile} THEN
DO:
   RUN Fill-SysMes IN h_tmess("","", "-1", "Не задан путь к файлу лога.").

END.

DO ON ERROR UNDO, RETRY:
IF RETRY THEN
DO:
   RUN Fill-SysMes IN h_tmess("","", "-1", "Ошибка записи в файл лога.").
   RETURN.
END.

   OUTPUT STREAM sExp TO VALUE(mLogFile) APPEND KEEP-MESSAGES.
END.

mMsg = SUBST("Процесс начат: &1 &2.",
             STRING(TODAY, "99/99/9999"),
             STRING(TIME, "HH:MM:SS")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

mMsg = SUBST("Обрабатываемая дата: &1.",
             STRING(iOpDate, "99/99/9999")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).
RUN SetMsg IN THIS-PROCEDURE(0, "====================").

lMainDps:
FOR EACH loan WHERE loan.contract  EQ "dps"
                AND loan.open-date EQ iOpDate
                AND loan.end-date  NE ?
                AND loan.cust-cat  EQ "Ч"
              NO-LOCK,
   FIRST person WHERE person.person-id EQ loan.cust-id
                NO-LOCK:

   /* Есть ли роли счетов, привязанные не в день открытия договора */
   FIND FIRST loan-acct WHERE loan-acct.contract  EQ loan.contract
                          AND loan-acct.cont-code EQ loan.cont-code
                          AND loan-acct.since     NE loan.open-date
                        NO-LOCK NO-ERROR.

   IF AVAIL(loan-acct) THEN /* Есть роли привязанные датой <> дате открытия */
      NEXT lMainDps.

   IF FndOp(loan.contract, loan.cont-code) EQ NO THEN
   DO:
      RUN DelDps IN THIS-PROCEDURE(loan.contract,
                                   loan.cont-code,
                                   NO,
                                   OUTPUT mOk).
      IF mOk THEN
      DO:
         mMsg = SUBST("Договор удален успешно.").
         RUN SetMsg IN THIS-PROCEDURE(0, mMsg).
      END.
      ELSE 
      DO:
         mMsg = SUBST("ОШИБКА! Договор &1 не удален!",
                      loan.cont-code).
         RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

      END. /* IF mOk THEN ... ELSE */

      RUN SetMsg IN THIS-PROCEDURE(0, "").

   END. /* IF mFindOp EQ NO THEN */

END. /* lMainDps: */

mMsg = SUBST("Процесс закончен: &1 &2",
             STRING(TODAY, "99/99/9999"),
             STRING(TIME, "HH:MM:SS")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

PUT STREAM sExp UNFORM SKIP(1).
OUTPUT STREAM sExp CLOSE.
