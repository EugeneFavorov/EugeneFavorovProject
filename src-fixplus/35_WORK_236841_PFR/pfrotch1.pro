/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pfrotch1.pro
      Comment: 0236841 Миграция. Загрузка реестра зачисления пенсий из ПФ + отчет по пенсионерам
   Parameters: нет
         Uses:
      Used by: pfrotch1.p
      Created: 05/11/2015 KMBIS 0236841 Миграция. Загрузка реестра зачисления пенсий из ПФ
                                        Отчет по пенсионерам
     Modified: 
                                    
*/

/*================================================================================================*/
/*=== Создание общего списка пенсионеров =========================================================*/
PROCEDURE InitPensTT:
   DEF INPUT PARAM iAcct  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iBegD  AS DATE  NO-UNDO.
   DEF INPUT PARAM iEndD  AS DATE  NO-UNDO.

DEF VAR vPFRName  AS  CHAR  NO-UNDO.

DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bOp       FOR op.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPensAcct FOR ttPensAcct.

  lLastPfrOp:
  FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct
                      AND bOpEntry.op-date   GE iBegD
                      AND bOpEntry.op-date   LE iEndD
                      AND bOpEntry.op-status BEGINS CHR(251)
                    NO-LOCK,
     FIRST bAcct WHERE bAcct.acct     EQ bOpEntry.acct-cr 
                   AND bAcct.cust-cat EQ "Ч"
                 NO-LOCK
                 BREAK BY bAcct.cust-id
                       BY bOpEntry.op-date:

     IF LAST-OF(bAcct.cust-id) THEN
     DO:
        FIND FIRST bPensAcct WHERE bPensAcct.pers-id EQ bAcct.cust-id
                             EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL(bPensAcct) THEN
        DO:
           /* По данному пенсионеру, уже найдено счет с зачислением пенсии */
           /* Проверим, что это последнее зачисление                       */
           /* Такая ситация возможно как из-за ошибок алгоритма, так и в   */
           /* случае нескольких счетов ПФР (параметры процедуры)           */
           FIND FIRST bOp WHERE bOp.op      EQ bPensAcct.op-id
                            AND bOp.op-date GT bOpEntry.op-date
                          NO-LOCK NO-ERROR.
           /* Существующая запись последняя, найденная же проводка сделана раньше */
           IF AVAIL(bOp) THEN
              NEXT lLastPfrOp.

        END. /* IF AVAIL(bPensAcct) THEN */
        ELSE 
        DO:
           CREATE bPensAcct.
           ASSIGN
              bPensAcct.proxy    = NO
              bPensAcct.print    = NO
              bPensAcct.pers-id  = bAcct.cust-id
           .
        END. /* IF AVAIL(bPensAcct) THEN ... ELSE */

        ASSIGN
           bPensAcct.acct-pfr = bOpEntry.acct-cr
           bPensAcct.op-id    = bOpEntry.op
        .
        RELEASE bPensAcct.

     END. /* IF LAST-OF(bOpEntry.op-date) THEN */
  END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct */

END PROCEDURE. /* InitPensTT */

/*================================================================================================*/

/*================================================================================================*/
/*=== Помечаем счета пенсионеров подлежащих выводу в отчет =======================================*/
PROCEDURE FiltPensTT:
   DEF INPUT PARAM iRID   AS ROWID NO-UNDO.
   DEF INPUT PARAM iBegD  AS DATE  NO-UNDO.
   DEF INPUT PARAM iEndD  AS DATE  NO-UNDO.

DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bPensAcct FOR ttPensAcct.
DEF BUFFER bSign     FOR signs.

   FIND FIRST bPensAcct WHERE ROWID(bPensAcct) EQ iRID 
                        EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL(bPensAcct) THEN
      RETURN.

   FIND FIRST bOpEntry WHERE bOpEntry.acct-db   EQ bPensAcct.acct-pfr
                         AND bOpEntry.op-date   GE iBegD
                         AND bOpEntry.op-date   LE iEndD
                         AND bOpEntry.op-status BEGINS CHR(251)
                       NO-LOCK NO-ERROR.
   IF AVAIL(bOpEntry) THEN
   DO:
      /* По счету имеются списания, нужно проверить наличие списаний по доверенности */
      lOpFnd:
      FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ bPensAcct.acct-pfr
                          AND bOpEntry.op-date   GE iBegD
                          AND bOpEntry.op-date   LE iEndD
                          AND bOpEntry.op-status BEGINS CHR(251)
                        NO-LOCK,
         FIRST bSign WHERE bSign.file-name EQ "op"
                       AND bSign.surrogate EQ STRING(bOpEntry.op)
                       AND bSign.code      EQ "proxy-code"
                     NO-LOCK:
         /* Проводка сделана по доверенности, включаем счет в отчет */
         ASSIGN
            bPensAcct.proxy    = YES 
            bPensAcct.print    = YES
         .
         LEAVE lOpFnd.
      END. /* FOR EACH op-entry WHERE op-entry.acct-db   EQ acct.acct */
   END. /* IF AVAIL(op-entry) THEN */
   ELSE
   DO:
      /* Списаний по счету нет, значит счет однозначно подлежит выводу в отчет */
      ttPensAcct.print = YES.
   END. /* IF AVAIL(bOpEntry) THEN ... ELSE */

END PROCEDURE. /* FiltPensTT */

/*================================================================================================*/
