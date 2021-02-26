{globals.i}

/* +++ comgroup.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: comgroup.p
      Comment: TT:0220967 БМ. Автоматическое списание комиссий за РКО
   Parameters: BUFFER op, iParam, oResult
         Uses:
      Used by:
      Created: 24/10/2014 11:43 KMBIS Автоматическое списание комиссий за РКО
     Modified: 
*/
DEF        PARAM BUFFER op      FOR op.
DEF INPUT  PARAM        iParam  AS  CHAR NO-UNDO.
DEF OUTPUT PARAM        oResult AS  LOG  NO-UNDO INIT YES.

{globals.i}
{g-trans.equ}

{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get pbase}

DEF VAR mOpKind   AS  CHAR  NO-UNDO. /* Транзакция создания ком.документа */
DEF VAR mRunKind  AS  CHAR  NO-UNDO. /* Промверка на запуск транзакции    */
DEF VAR mTmpStr   AS  CHAR  NO-UNDO. 
DEF VAR mMaskDb   AS  CHAR  NO-UNDO. /* Маска счетов до дебету            */
DEF VAR mMaskCr   AS  CHAR  NO-UNDO. /* Маска счетов по кредиту           */
DEF VAR mOpInfo   AS  CHAR  NO-UNDO. /* Данные документа                  */
DEF VAR mAmtInfo  AS  CHAR  NO-UNDO. /* Данные документа                  */
DEF VAR mResult   AS  CHAR  NO-UNDO. /* Результат работы ком.транзакции   */

DEF BUFFER bAcctDb FOR acct.
DEF BUFFER bAcctCr FOR acct.
DEF BUFFER bxlink  FOR xlink.
DEF BUFFER blinks  FOR links.
DEF BUFFER bop     FOR op.
DEF BUFFER bHist   FOR history.

ASSIGN
   mMaskDb = GetEntries(2, iParam, ";", "")
   mMaskCr = GetEntries(3, iParam, ";", "")
   mOpKind = GetXattrValueEx("op-kind", 
                             op.op-kind, 
                             "ComOpKind", 
                             GetEntries(1, iParam, ";", ""))
NO-ERROR.

/*=== Проверяем транзакцию порождения комиссии ===*/
IF NOT {assigned mOpKind} THEN
DO:
   ASSIGN 
      oResult = NO
      mTmpStr = "Не задана транзакция начисления комиссии"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mOpKind} THEN */
ELSE
DO:
   FIND FIRST op-kind WHERE op-kind.op-kind EQ mOpKind
                      NO-LOCK NO-ERROR NO-WAIT.

   IF NOT AVAIL(op-kind) THEN 
   DO:
      ASSIGN
         oResult = NO
         mTmpStr = "не найдена в базе"
         mTmpStr = "заблокирована другим пользователем" WHEN LOCKED(op-kind)
         mTmpStr = SUBST("Транзакция начисления &1", mTmpStr)
      .

      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
  
   END. /* IF NOT AVAIL(op-kind) THEN  */

END. /* IF NOT {assigned mOpKind} THEN ... ELSE */

/*=== Маска счетов дебета ===*/
IF NOT {assigned mMaskDb} THEN
DO:

   ASSIGN
      oResult = NO
      mTmpStr = "Не задана маска счетов до дебету"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mMaskDb} THEN */

/*=== Маска счетов кредита ===*/
IF NOT {assigned mMaskCr} THEN
DO:

   ASSIGN
      oResult = NO
      mTmpStr = "Не задана маска счетов до кредиту"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mMaskCr} THEN */


lMain:
DO TRANSACTION ON ERROR UNDO lMain, LEAVE lMain
               ON QUIT  UNDO lMain, LEAVE lMain:

   IF oResult EQ NO THEN 
      LEAVE lMain.

   /*=== Блок проверок проводок ===*/
   FIND op-entry OF op NO-LOCK NO-ERROR.

   /* Многопроводочный документ */
   IF AMBIGUOUS op-entry THEN  
      LEAVE lMain.

   /* Нет проводок в документе */
   IF NOT AVAIL(op-entry) THEN 
      LEAVE lMain.


   /* Остатки от полупроводок */
   IF    NOT {assigned op-entry.acct-db} 
      OR NOT {assigned op-entry.acct-cr} 
   THEN
      LEAVE lMain.

   {find-act.i &acct = op-entry.acct-db 
               &bact = bAcctDb}

   {find-act.i &acct = op-entry.acct-cr 
               &bact = bAcctCr}

   /* Не нашли счет в базе */
   IF    NOT AVAIL(bAcctCr) 
      OR NOT AVAIL(bAcctDb)
   THEN
   DO:
      oResult = NO.

      IF NOT AVAIL(bAcctDb) THEN
      DO:
         mTmpStr = SUBST("Счет '&1' по дебету не найден в базе",
                         op-entry.acct-db).
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
      END.

      IF NOT AVAIL(bAcctCr) THEN
      DO:
         mTmpStr = SUBST("Счет '&1' по кредиту не найден в базе",
                         op-entry.acct-cr).
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
      END.

      LEAVE lMain.

   END. /* IF    NOT AVAIL(bAcctDb)  */

   /* Счета не подошли под маску */
   IF    NOT CAN-DO(mMaskDb, op-entry.acct-db) 
      OR NOT CAN-DO(mMaskCr, op-entry.acct-cr) 
   THEN
      LEAVE lMain.

   /* Счета в разных валютах */
   IF bAcctDb.currency NE bAcctCr.currency THEN
      LEAVE lMain.

   IF {assigned op-entry.currency} THEN 
      mAmtInfo = SUBST("&1 (&2)",
                       STRING(op-entry.amt-cur),
                       op-entry.currency).
   ELSE
      mAmtInfo = SUBST("&1 (&2)",
                       STRING(op-entry.amt-rub),
                       FGetSetting("КодНацВал", "", "")).

   mOpInfo = SUBST("номер '&1' от &2 сумма &3",
                   op.doc-num,
                   STRING(op.op-date,"99/99/9999"),
                   mAmtInfo).


   /* Проверяем связь с комиссионным документом */
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
         mTmpStr = SUBST("Связь с комиссионным документом заблокирована (документ &2)",
                         mOpInfo)
         oResult = NO
      .
      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

   END.
   ELSE IF AVAIL(blinks) THEN
   DO:

      /* Проверяем связанный комиссионный документ */
      FIND FIRST bop WHERE bop.op EQ INT64(blinks.target-id)
                     EXCLUSIVE-LOCk NO-WAIT NO-ERROR.

      IF     NOT AVAIL(bop) 
         AND LOCKED(bop)  
      THEN
      DO:
         /* Комиссионный документ заблокирован */
         ASSIGN
            mTmpStr = SUBST("Комиссионный документ заблокирован " +
                            "(для документа &1)",
                            mOpInfo)
            oResult = NO
         .
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

      END.
      ELSE    IF NOT AVAIL(bop) 
           OR bop.op-date EQ ? 
      THEN
      DO:
         /* Связь есть, но документа нет */
         /* Комиссионный аннулирован     */
         DELETE blinks.
         RELEASE blinks.

      END.
      ELSE 
      DO:
         /* Найдена действующая связь с комиссионным документом */
         mTmpStr = SUBST("%s=&1%s=&2%s=&3",
                         op.doc-num,
                         STRING(op.op-date,"99/99/9999"),
                         mAmtInfo).
         PICK-VALUE = "NO".
         RUN Fill-SysMes IN h_tmess ("", "commgr01", "4", mTmpStr).

         /*=== Анализ выбора ===*/
         CASE PICK-VALUE:
        
            WHEN "YES" THEN /*=== Пересоздаем связь ======*/ 
            DO:
               
               RUN dpsopb.p(RECID(bop), 2, NO) NO-ERROR.

               IF {&RETURN_VALUE} GT "" OR 
                  {&RETURN_VALUE} EQ ? 
               THEN 
               DO:
                   /* Не смогли аннулировать */
                  oResult = NO.
                  LEAVE lMain.
               END.

               DELETE  blinks.
               RELEASE blinks.
               RELEASE bop.
            END.
        
            WHEN "NO" THEN /*=== Решили оставить связь ===*/
            DO:
               
               oResult = YES.
               LEAVE lMain.

            END.
        
            OTHERWISE      /*=== Нажали ESC ==============*/
            DO:
               
               oResult = NO.
               LEAVE lMain.

            END.
        
         END CASE. /* STRING(PICK-VALUE, "YES/NO"): */

      END.

   END. /* ELSE IF AVAIL(blinks) THEN */

   ASSIGN
      oResult  = NO
      mRunKind = GetBaseOpKind()
   .
   IF NOT {assigned mRunKind} THEN
      RUN InitBaseLibrary IN h_pbase (?, gend-date, ?).

   /* Передаем op.op основного документа */
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "_OpMain", STRING(op.op)).

   FOR FIRST bHist WHERE bHist.file-name  EQ "op"
                     AND bHist.field-ref  EQ STRING(op.op)
                     AND bHist.modify     NE "RUN"
                     AND bHist.modify     NE "PRINT"
                     AND bHist.modify     NE "SAVE"
                   NO-LOCK:

      /* Время создания документа */
      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                                      "_OpTime", 
                                      STRING(bHist.modif-time, "HH:MM:SS")).

      /* Дата создания документа */
      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                                      "_OpDate", 
                                      STRING(bHist.modif-date, "99.99.99")).

   END. /* FOR LAST bHist WHERE bHist.file-name  EQ "op" */
 
   /* Вызываем создание комиссионного документа */
   RUN RunTransaction IN h_pbase (mOpKind) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
   DO:
      /* Произошла ошибка выполняния транзакции начисления комиссии */
      ASSIGN
         mTmpStr = SUBST("Ошибка при выполнении транзакции: &1",
                         mOpKind)
      .
      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
   END.
   ELSE 
      oResult = YES.

   IF oResult EQ YES THEN
   DO:

      /* Возможно понадобится анализ возвращаемого значение */
      ASSIGN
         mResult = GetSysConf("_ResultCom")
         oResult = {assigned mResult }
      .

   END.

END. /* lMain: */

{intrface.del}

RETURN.

/*=== Блок финализации =======================================================*/

FINALLY:

   /* Чистим за собой мусор */
   RUN DelTransAttr IN h_trans ("", 0, "_OpMain")     NO-ERROR.
   RUN DeleteOldDataProtocol IN h_base ("_ResultCom") NO-ERROR.

   RELEASE bop NO-ERROR.

END FINALLY.

/* --- comgroup.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am --- */
