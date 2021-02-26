
/* +++ a-deldps.pro was humbly modified by (c)blodd converter v.1.09 on 7/27/2015 8:10am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: a-deldps.pro
      Comment: TT:0235439 Миграция. Отказ клиента от оформления вклада
   Parameters:
         Uses:
      Used by: a-deldps.p
      Created: 12/02/2015 14:33 KMBIS Миграция. Отказ клиента от оформления вклада
                                      Процедуры удаления
     Modified: 
*/
/*================================================================================================*/
/*=== Основная процедура удаления вклада =========================================================*/
PROCEDURE DelDps:
  DEF INPUT  PARAM iContr AS  CHAR  NO-UNDO.
  DEF INPUT  PARAM iCode  AS  CHAR  NO-UNDO.
  DEF INPUT  PARAM iIsDV  AS  LOG   NO-UNDO. /* Вклад До востребования */
  DEF OUTPUT PARAM oOk    AS  LOG   NO-UNDO.

DEF VAR vMsg     AS  CHAR  NO-UNDO. /* Сообщение в лог */
DEF VAR vLoanDV  AS  CHAR  NO-UNDO. /* Код вклада ДВ   */
DEF VAR vOk      AS  LOG   NO-UNDO. /* Код вклада ДВ   */


DEF BUFFER bDpsLoan  FOR loan.
DEF BUFFER bDVLoan   FOR loan.
DEF BUFFER bLoanAcct FOR loan-acct.
DEF BUFFER bLAcct    FOR loan-acct.
DEF BUFFER bAcct     FOR acct.

oOk = NO.

lMainDel:
DO TRANSACTION ON ERROR UNDO, THROW:

   vMsg = SUBST("Договор: &1",
                iCode).
   RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

   FIND FIRST bDpsLoan WHERE bDpsLoan.contract  EQ iContr
                         AND bDpsLoan.cont-code EQ iCode 
                       EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF LOCKED(bDpsLoan) THEN
   DO:
      vMsg = SUBST("Договор &1 заблокирован другим пользователем, пропущен.",
                   iContr).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO lMainDel, LEAVE lMainDel.

   END. /* IF LOCKED(bDpsLoan) THEN */
   ELSE IF AVAIL(bDpsLoan) THEN
   DO:
      IF iIsDV EQ NO THEN
      DO:
         vMsg = SUBST("Владелец договора (person-id): &1",
                      STRING(bDpsLoan.cust-id)).
         RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

         /*=== Найдем связанный с вкладом договор ДВ ===*/
         vLoanDV = FndDV(bDpsLoan.contract, bDpsLoan.cont-code).
         IF NUM-ENTRIES(vLoanDV) GT 1 THEN
         DO:
            vMsg = SUBST("&1 &2",
                         "Вклад связан с несколькими договорами до востребования",
                         "или несколькими счетами с ролью 'loan-dps-p'").
            RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            UNDO lMainDel, LEAVE lMainDel.
        
         END. /* IF NUM-ENTRIES(vLoanDV) GT 1 THEN */
      END. /* IF iIsDV EQ NO THEN */

      lAcctLoop:
      FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract
                           AND bLoanAcct.cont-code EQ bDpsLoan.cont-code
                         NO-LOCK,
         FIRST bAcct WHERE bAcct.acct     EQ bLoanAcct.acct
                       AND bAcct.currency EQ bLoanAcct.currency
                     NO-LOCK
                     BREAK BY bLoanAcct.acct
                           BY bLoanAcct.acct-type
                     ON ERROR UNDO, THROW:

         IF FIRST-OF(bLoanAcct.acct) THEN
         DO:
            vMsg = SUBST("Счет &1:", bAcct.acct).
            RUN SetMsg IN THIS-PROCEDURE(0, vMsg).
         END.

         IF LAST-OF(bLoanAcct.acct-type) THEN
         DO:
            vMsg = bLoanAcct.acct-type.
            /*=== Удаляем роль ===*/
            RUN DelLoanAcct(ROWID(bLoanAcct), OUTPUT vOk).
            IF vOk THEN
            DO:
               vMsg = SUBST("- Удалена роль: &1.",
                            vMsg).
               RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

            END. /* IF vOk THEN */
            ELSE
            DO:
               vMsg = SUBST("Ошибка: не удалось удалить роль &1.",
                            vMsg).
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
               UNDO lMainDel, LEAVE lMainDel.

            END. /* IF vOk THEN ... ELSE */

            FIND FIRST bLAcct WHERE bLAcct.acct      EQ bAcct.acct    
                                AND bLAcct.currency  EQ bAcct.currency
                                AND (bLAcct.contract  NE bDpsLoan.contract  OR
                                     bLAcct.cont-code NE bDpsLoan.cont-code)
                              NO-LOCK NO-ERROR.

            IF    bAcct.open-date NE bDpsLoan.open-date /* Счет открыт не днем вклада, оставляем */
               OR bAcct.cust-cat  NE bDpsLoan.cust-cat  /* Счет не открыт на владельца вклада    */
               OR bAcct.cust-id   NE bDpsLoan.cust-id
               OR AVAIL(bLAcct)                         /* Счет принадлежит другому договору     */
            THEN
               NEXT lAcctLoop. 

            IF LAST-OF(bLoanAcct.acct) THEN
            DO:
               /*=== Удаляем все документы по счету ===*/
               RUN DelAllOpEntry(bAcct.acct, OUTPUT vOk).
               IF vOk NE YES THEN
                  UNDO lMainDel, LEAVE lMainDel.

               vMsg = bAcct.acct.

               /*=== Удаляем счет ===*/
               RUN DelAcct(ROWID(bAcct), OUTPUT vOk).
               IF vOk THEN
               DO:
                  vMsg = SUBST("- Удален счет &1.", vMsg).
                  RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

               END. /* IF vOk THEN */
               ELSE
               DO:
                  vMsg = SUBST("Ошибка: не удалось удалить счет &1.", vMsg).
                  RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
                  UNDO lMainDel, LEAVE lMainDel.
               
               END. /* IF vOk THEN ... ELSE */

            END. /* IF LAST-OF(bLoanAcct.acct) THEN */
         END. /* IF LAST-OF(bLoanAcct.acct-type) THEN */
      END. /* FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract */

      /*=== Удаляем связаные субъекты ===*/
      RUN DelCustRole(bDpsLoan.contract, bDpsLoan.cont-code, OUTPUT vOk).
      IF vOk NE YES THEN
         UNDO lMainDel, LEAVE lMainDel.

      IF {assigned vLoanDV} THEN
      DO:
         FOR FIRST bDVLoan WHERE bDVLoan.contract  EQ bDpsLoan.contract
                             AND bDVLoan.cont-code EQ vLoanDV
                             AND bDVLoan.open-date EQ bDpsLoan.open-date
                             AND bDVLoan.end-date  EQ ?
                           NO-LOCK:
            /* Вклад ДВ открыт в тот же день, что и срочный */

            FIND FIRST bLoanAcct WHERE bLoanAcct.contract  EQ bDVLoan.contract
                                   AND bLoanAcct.cont-code EQ bDVLoan.cont-code
                                   AND bLoanAcct.since     NE bDVLoan.open-date
                                 NO-LOCK NO-ERROR.
           
            IF AVAIL(bLoanAcct) THEN
            DO:
               /* Есть ли роли счетов, привязанные не в день открытия договора */
               vMsg = SUBST("Договор ДВ &1 пропущен, &2.",
                            bDVLoan.cont-code,
                            "есть роли с датой привязки отличной от даты отрытия договора").
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            END. /* IF AVAIL(bLoanAcct) THEN */

            IF FndOp(bDVLoan.contract, bDVLoan.cont-code) EQ NO THEN
            DO:
               vMsg = SUBST("Обрабатываем связанный Договор ДВ &1.",
                            bDVLoan.cont-code).
               RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

               RUN DelDps IN THIS-PROCEDURE(bDVLoan.contract,
                                            bDVLoan.cont-code,
                                            YES,
                                            OUTPUT mOk).
               IF mOk NE YES THEN
               DO:
                  vMsg = SUBST("Ошибка: связанный договор ДВ &1 не удален.",
                               bDVLoan.cont-code).
                  RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
                  UNDO lMainDel, LEAVE lMainDel.

               END. /* IF mOk NE YES THEN */

            END. /* IF FndOp(bDVLoan.contract, bDVLoan.cont-code) EQ NO THEN */
            ELSE 
            DO:
               /* Есть проводки по вкладу ДВ */
               vMsg = SUBST("Договор ДВ &1 пропущен, &2.",
                            bDVLoan.cont-code,
                            "есть не аннулированные проводки").
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).

            END. /* IF FndOp(...) EQ NO THEN ... ELSE */
         END. /* FOR FIRST bDVLoan WHERE bDVLoan.contract  EQ bDpsLoan.contract */
      END. /* IF {assigned vLoanDV} THEN */

      /* Удаляем договор */
      DELETE bDpsLoan.
      ASSIGN 
         oOk  = YES
         vMsg = SUBST("Договор &1 удален.",
                      iCode)
      .
      RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

   END. /* ELSE IF AVAIL(bDpsLoan) THEN */

END. /* lMainDel: DO TRANSACTION ON ERROR UNDO, THROW: */
             
/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "Ошибка: не возможно удалить договор вклада.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelDps */

/*================================================================================================*/

/*================================================================================================*/
/*=== Вывод сообщения ============================================================================*/
PROCEDURE SetMsg PRIVATE:
   DEF INPUT PARAM iType  AS INT64 NO-UNDO.
   DEF INPUT PARAM iTxt   AS CHAR  NO-UNDO.

   PUT STREAM sExp UNFORM SUBST("&1: &2", STRING(iType, "->9"), iTxt) SKIP.

   IF auto EQ NO THEN
   DO:
      /* Запуск выполнен пользователем */
      RUN Fill-SysMes IN h_tmess("","",STRING(iType), iTxt).

   END.
END PROCEDURE. /* SetMsg */

/*================================================================================================*/

/*================================================================================================*/
/*=== Удаляем роль по счету ======================================================================*/
PROCEDURE DelLoanAcct:
   DEF INPUT  PARAM iRowId AS ROWID NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bLoanAcct  FOR  loan-acct.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FIND FIRST bLoanAcct WHERE ROWID(bLoanAcct) EQ iRowId
                        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF LOCKED(bLoanAcct) THEN
   DO:
      vMsg = SUBST("Роль &1 счета &2 заблокирована.",
                   bLoanAcct.acct-type,
                   bLoanAcct.acct).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   
   END. /* IF LOCKED(bLoanAcct) THEN */
   ELSE IF AVAIL(bLoanAcct) THEN
   DO:
      /* Удаляем роль */
      DELETE bLoanAcct.
      oOk = YES.

   END. /* ELSE IF AVAIL(bLoanAcct) THEN */

   IF oOk = NO THEN
      UNDO, LEAVE.

END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "Ошибка: не возможно удалить роль счета.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelLoanAcct */

/*================================================================================================*/

/*================================================================================================*/
/*=== Удаляем документ ===========================================================================*/
PROCEDURE DelOp:
   DEF INPUT  PARAM iOp  AS INT64 NO-UNDO.
   DEF OUTPUT PARAM oOk  AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bOp     FOR op.
DEF BUFFER bOpEn   FOR op-entry.
DEF BUFFER bSign   FOR signs.
DEF BUFFER bOpBank FOR op-bank.
DEF BUFFER bOpImp  FOR op-impexp.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FIND FIRST bOp WHERE bOp.op EQ iOp 
                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
           
   IF LOCKED(bOp) THEN
   DO:
      vMsg = SUBST("Документ &1 (op.op) заблокирован.",
                   STRING(bOp.op)).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO, LEAVE.
   
   END. /* IF LOCKED(bOpDel) THEN */
   ELSE IF AVAIL(bOp) THEN
   DO:
      FOR EACH bOpEn OF bOp EXCLUSIVE-LOCK
                            ON ERROR UNDO, THROW:
         DELETE bOpEn.
      END.

      FOR EACH bOpBank OF bOp EXCLUSIVE-LOCK
                              ON ERROR UNDO, THROW:
         DELETE bOpBank.
      END.

      FOR EACH bOpImp OF bOp EXCLUSIVE-LOCK
                             ON ERROR UNDO, THROW:
         DELETE bOpImp.
      END.

      FOR EACH bSign WHERE bSign.file-name EQ "op"
                       AND bSign.surrogate EQ STRING(iOp)
                     EXCLUSIVE-LOCK
                     ON ERROR UNDO, THROW:
         DELETE bSign.
      END.

      DELETE bOp.
      oOk = YES.
   END. /* ELSE IF AVAIL(bOp) THEN */
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "Ошибка: не возможно удалить документа по счету.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelOp */

/*================================================================================================*/

/*================================================================================================*/
/*=== Удаляем все документы по счету =============================================================*/
PROCEDURE DelAllOpEntry:
   DEF INPUT  PARAM iAcct  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.
DEF VAR vOk   AS  LOG   NO-UNDO.

DEF BUFFER bOp       FOR op.
DEF BUFFER bOpEntry  FOR op-entry.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ iAcct
                        OR bOpEntry.acct-cr EQ iAcct
                     NO-LOCK,
      FIRST bOp WHERE bOp.op EQ bOpEntry.op
                     BREAK BY bOpEntry.op
                     ON ERROR UNDO, THROW:

      IF bOp.op-date NE ? THEN
      DO:
         /* Найден не аннулированный документ */
         vMsg = SUBST("По счету &1 найден не аннулированный документ от &2 номер &3(op.op) в статусе '&4'.",
                      DelFilFromAcct(iAcct),
                      STRING(bOp.op-date, "99/99/9999"),
                      STRING(bOp.op),
                      bOp.op-status).
         RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
         UNDO, LEAVE.
      END. /* bOp.op-date */

      IF LAST-OF(bOpEntry.op) THEN
      DO:

         /*=== Удаляем документ ===*/
         vMsg = SUBST("Ошибка: не удалось удалить документ &1 (op.op).",
                       STRING(bOpEntry.op)).
         RUN DelOp(bOpEntry.op, OUTPUT vOk).

         IF vOk NE YES THEN
         DO:
            RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            UNDO, LEAVE.

         END. /*IF vOk NE YES THEN*/
      END. /* IF LAST-OF(bOpEntry.op) THEN */
   END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ bAcct.acct */

   oOk = YES.

END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = SUBST("Ошибка: не возможно удалить проводки по счету &1.",
                   DelFilFromAcct(iAcct)).

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelOp */

/*================================================================================================*/

/*================================================================================================*/
/*=== Удаляем счет ===============================================================================*/
PROCEDURE DelAcct:
   DEF INPUT  PARAM iRowId AS ROWID NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bAcct   FOR acct.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   /* Ищем клиентский счет */
   FIND FIRST bAcct WHERE ROWID(bAcct) EQ iRowId
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   
   IF LOCKED(bAcct) THEN
   DO:
      vMsg = SUBST("Ошибка: счет заблокирован другим пользователем.").
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO, LEAVE.
   END.
   ELSE IF AVAIL(bAcct) THEN
   DO:
      RUN acct-del.p (RECID(bAcct)).

      IF {&RETURN_VALUE} EQ "блокирован" THEN
         UNDO, LEAVE.
      ELSE
      DO:
         /* Удаляем счет */
         DELETE bAcct.
         oOk = YES.
      END. /* IF RETURN-VALUE EQ "блокирован" THEN ... ELSE */
   END. /* ELSE IF AVAIL(bAcct) THEN */
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "Ошибка: не возможно удалить счет.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelAcct */

/*================================================================================================*/

/*================================================================================================*/
/*=== Удаляем связанных субъектов ================================================================*/
PROCEDURE DelCustRole:
   DEF INPUT  PARAM iContr AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCode  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bRole   FOR cust-role.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FOR EACH bRole WHERE bRole.file-name EQ "loan"
                    AND bRole.surrogate EQ SUBST("&1,&2", iContr, iCode)
                  EXCLUSIVE-LOCK:

      vMsg = SUBST("Удален &1: &2.",
                   bRole.class-code,
                   bRole.cust-name).

      DELETE bRole.
      RUN SetMsg IN THIS-PROCEDURE(0, vMsg).
   END. /* ELSE IF AVAIL(bAcct) THEN */
   oOk = YES.
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== Перехватываем ошибки ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "Ошибка: не возможно удалить связанный субъект.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelAcct */

/*================================================================================================*/

/* --- a-deldps.pro was humbly modified by (c)blodd converter v.1.09 on 7/27/2015 8:10am --- */
