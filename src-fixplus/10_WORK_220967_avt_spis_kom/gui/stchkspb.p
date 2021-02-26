{globals.i}

/* +++ stchkspb.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: stchkspb.p
      Comment: метод контроля при изменении
   Parameters: Recid документа, Тип изменений
         Uses:
      Used by:
      Created: 04/02/2015 11:04 KMBIS Автоматическое списание комиссий за РКО
                                      Контроль изменений документа
     Modified: 
*/
DEF INPUT PARAM iRecid  AS  RECID  NO-UNDO.
DEF INPUT PARAM iParam  AS  CHAR   NO-UNDO.

{globals.i}
{intrface.get tmess}    /* Инструменты обработки сообщений. */

DEF VAR mMsg      AS  CHAR  NO-UNDO. /* Сообщение об ошибке               */
DEF VAR mOpInfo   AS  CHAR  NO-UNDO. /* Данные документа                  */
DEF VAR mResult   AS  LOG   NO-UNDO. /* Флаг успешной обработки           */
                      
DEF BUFFER bComOp  FOR op.
DEF BUFFER bxlink  FOR xlink.
DEF BUFFER blinks  FOR links.


ASSIGN
   PICK-VALUE = "YES"
   mResult    = YES
.

FOR FIRST op WHERE RECID(op) EQ iRecid
             NO-LOCK:

   IF     iParam       EQ     "status" 
      AND op.op-status BEGINS "А"
   THEN
   DO:
      mOpInfo = SUBST("номер '&1' от &2",
                op.doc-num,
                STRING(op.op-date,"99/99/9999")).

      FIND FIRST bxlink WHERE bxlink.class-code EQ op.class-code
                          AND bxlink.link-code  EQ "КомГр"
                         NO-LOCK NO-ERROR.
      RELEASE blinks.

      IF AVAIL(bxlink) THEN
         FIND FIRST blinks WHERE blinks.link-id   EQ bxlink.link-id
                             AND blinks.source-id EQ STRING(op.op)
                           EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF     NOT AVAIL(blinks) 
         AND LOCKED(blinks)
      THEN
      DO:
     
         ASSIGN
            mMsg    = SUBST("Связь с комиссионным документом заблокирована (документ &2)",
                            mOpInfo)
            mResult = NO
         .
         RUN Fill-SysMes IN h_tmess("", "", "-1", mMsg).
     
      END. /* IF     NOT AVAIL(blinks)  */
      ELSE IF AVAIL(blinks) THEN
      DO:

         /* Проверяем связанный комиссионный документ */
         FIND FIRST bComOp WHERE bComOp.op EQ INT64(blinks.target-id)
                           EXCLUSIVE-LOCk NO-WAIT NO-ERROR.
         
         IF     NOT AVAIL(bComOp) 
            AND LOCKED(bComOp)
         THEN
         DO:
            /* Комиссионный документ заблокирован */
            ASSIGN
               mMsg    = SUBST("Комиссионный документ заблокирован " +
                               "(для документа &1)",
                               mOpInfo)
               mResult = NO
            .
            RUN Fill-SysMes IN h_tmess("", "", "-1", mMsg).
         
         END. /* IF     NOT AVAIL(bComOp)  // AND LOCKED(bComOp) */
         ELSE IF AVAIL(bComOp) THEN
         DO:
            ASSIGN
               bComOp.op-status = op.op-status
               bComOp.op-date   = ?
            .
            RELEASE bComOp.
         END. /* ELSE IF AVAIL(bComOp) */

         IF mResult THEN
         DO:
            /* Удаляем связь */
            DELETE  blinks.
            RELEASE blinks.

         END. /* IF mResult THEN */
      END. /* ELSE IF AVAIL(blinks) THEN */

   END. /*   IF     iParam       EQ     "status" */
END. /* FOR FIRST op WHERE RECID(op) EQ a1  */

PICK-VALUE = STRING(mResult).

{intrface.del}

/* --- stchkspb.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am --- */
