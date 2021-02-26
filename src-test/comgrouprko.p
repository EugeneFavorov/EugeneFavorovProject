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
/* Вставка BAV - ОСРКО.511 */
{intrface.get blkob}
/* Конец вставки BAV - ОСРКО.511 */



IF CAN-DO("ВКЛБ,ВФМ", op.op-status)
THEN DO:
         oResult = YES.
         {intrface.del}
         RETURN.
     END.



DEF VAR mOpKind        AS CHAR NO-UNDO. /* Транзакция создания ком.документа */
DEF VAR mRunKind       AS CHAR NO-UNDO. /* Промверка на запуск транзакции    */
DEF VAR mTmpStr        AS CHAR NO-UNDO. 
DEF VAR mMaskDb        AS CHAR NO-UNDO. /* Маска счетов до дебету            */
DEF VAR mMaskCr        AS CHAR NO-UNDO. /* Маска счетов по кредиту           */
DEF VAR mOpInfo        AS CHAR NO-UNDO. /* Данные документа                  */
DEF VAR mAmtInfo       AS CHAR NO-UNDO. /* Данные документа                  */
DEF VAR mResult        AS CHAR NO-UNDO. /* Результат работы ком.транзакции   */
DEF VAR mFlag          AS INT  NO-UNDO.
DEF VAR mSum           AS DEC  NO-UNDO INIT 0.
DEF VAR mSum1          AS DEC  NO-UNDO INIT 0.
DEF VAR mSum2          AS DEC  NO-UNDO INIT 0.
DEF VAR mKom1          AS CHAR NO-UNDO INIT "Нулевая1".
DEF VAR mKom2          AS CHAR NO-UNDO INIT "Нулевая1".

DEF BUFFER bAcctDb     FOR acct.
DEF BUFFER bAcctCr     FOR acct.
DEF BUFFER bxlink      FOR xlink.
DEF BUFFER blinks      FOR links.
DEF BUFFER bop         FOR op.
DEF BUFFER bHist       FOR history.
DEF BUFFER bop2        FOR op.
DEF BUFFER bop-entry2  FOR op-entry.

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
               ON STOP  UNDO lMain, LEAVE lMain:

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
                       AND bxlink.link-code  EQ "КомГрРКО"
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

               IF RETURN-VALUE GT "" OR 
                  RETURN-VALUE EQ ? 
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





/*====================================================================================================================================================================================================================*/
   /*MESSAGE "comgrouprko.p" view-as alert-box.*/

   /*IF acct.acct = "40802810505850010087     @0500" THEN LEAVE lMain.*/

   IF can-do('01КЛ,01БУМ', op.doc-type)
      AND op-entry.currency EQ ""
      AND can-do('406*,407*,40807*,40802*', op-entry.acct-db)
      AND can-do('30102*,30110*,30223*,30301*', op-entry.acct-cr)
      AND int(op.order-pay) > 4
      AND (
           (can-do('!40101*,!40102*,!40201*,!40204*,!40402*,40817*,40820*,423*,426*', op.ben-acct))
           OR
           (can-do('47422*', op.ben-acct) AND can-do('*40817*,*408.17*,*40820*,*408.20*,*42301*,*423.01*,*42307*,*423.07*,*42601*,*426.01*', op.details))
           OR
           (can-do('30232*,47422*', op.ben-acct) AND can-do('*перечислен*карт*,*перечислен*ПК*,*перевод*карт*,*перевод*ПК*,*зачислен*карт*,*зачислен*ПК*,*пополнен*карт*,*пополнен*ПК*', op.details))
          )
   THEN DO:
            /*MESSAGE "OK op AND op-entry" view-as alert-box.*/
            mFlag = 1.
        END.
   ELSE DO:
            /*MESSAGE "NO op OR op-entry" view-as alert-box.*/
            LEAVE lMain.
        END.



   IF mFlag = 1
   THEN DO:
            FIND FIRST op-bank WHERE op-bank.bank-code-type EQ 'МФО-9'
                                 AND op-bank.op             EQ op.op
                                 AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)
                                 NO-LOCK NO-ERROR.
                 IF AVAIL op-bank
                 THEN DO: 
                          /*MESSAGE "OK op-bank" view-as alert-box.*/
                          mFlag = 1.
                      END.  
                 ELSE DO:
                          /*MESSAGE "NO op-bank" view-as alert-box.*/
                          LEAVE lMain.
                      END.
        END.



   /*MESSAGE "mFlag =" string(mFlag) view-as alert-box.*/



   FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
   /*MESSAGE string(acct.acct) string(acct.cust-cat) string(acct.bal-acct) view-as alert-box.*/

   IF can-do("СпБрок,СпецПА,Кон138,КонЗдт,КонРез", acct.contract) THEN LEAVE lMain.

   IF acct.cust-cat EQ 'Ч' AND acct.bal-acct EQ 40802 THEN FIND FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.



/*====== ИП платит на свои ФЛ счета ==================================================================================================================================================================================*/
   IF acct.cust-cat EQ 'Ч' 
      AND acct.bal-acct EQ 40802
      AND op.name-ben EQ string(person.name-last + ' ' + person.first-names)
   THEN DO:
            /*MESSAGE "AVAIL ACCT ИП платит на свои ФЛ счета" view-as alert-box.*/

            FOR EACH bop-entry2 WHERE bop-entry2.filial-id EQ shFilial
                                  AND bop-entry2.acct-db   EQ acct.acct
                                  AND can-do('√,√√,ФБК,ФБН,ФБО,ФБП,ФДД', bop-entry2.op-status)
                                  AND bop-entry2.op-date   >= date('01' + substring(string(today),3)) 
                                  AND bop-entry2.op-date   <= date(today)
                                  AND can-do('30102*,30110*,30223*,30301*', bop-entry2.acct-cr)
                NO-LOCK:
                         /*MESSAGE "AVAIL BOP-ENTRY2" view-as alert-box.*/

                         FIND FIRST bop2 WHERE bop2.op EQ bop-entry2.op NO-LOCK NO-ERROR.
                         IF AVAIL bop2 
                              AND can-do('01КЛ,01БУМ', bop2.doc-type)
                              AND (
                                   (can-do('!40101*,!40102*,!40201*,!40204*,!40402*,40817*,40820*,423*,426*', bop2.ben-acct))
                                   OR
                                   (can-do('47422*', bop2.ben-acct) AND can-do('*40817*,*408.17*,*40820*,*408.20*,*42301*,*423.01*,*42307*,*423.07*,*42601*,*426.01*', bop2.details))
                                   OR
                                   (can-do('30232*,47422*', bop2.ben-acct) AND can-do('*перечислен*карт*,*перечислен*ПК*,*перевод*карт*,*перевод*ПК*,*зачислен*карт*,*зачислен*ПК*,*пополнен*карт*,*пополнен*ПК*', bop2.details))
                                  )
                              AND int(bop2.order-pay) > 4
                              AND bop2.name-ben EQ string(person.name-last + ' ' + person.first-names)
                         THEN DO:
                                  /*MESSAGE "AVAIL BOP2" view-as alert-box.*/

                                  FIND FIRST op-bank WHERE op-bank.op EQ bop-entry2.op 
                                                       AND op-bank.bank-code-type EQ 'МФО-9'
                                                       AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)                        
                                       NO-LOCK NO-ERROR.
                                  IF AVAIL op-bank THEN DO:
                                                            /*MESSAGE "AVAIL OP-BANK" view-as alert-box.*/

	     	                                            IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency EQ '' 
                                                               AND bop-entry2.amt-rub > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-rub.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.
                          	     	                    
                                                            /*IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency NE ''
                                                               AND bop-entry2.amt-cur > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-cur.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.*/
                                                        END.
                              END.
                         ELSE NEXT.
            END.

            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency EQ '' 
               AND op-entry.amt-rub > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-rub.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/
                          	     	                    
            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency NE ''
               AND op-entry.amt-cur > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-cur.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/

            IF mSum + op-entry.amt-rub <= 350000 THEN DO: 
                                                          mKom1 = "RkoIpSmall".
                                                          mSum1 = op-entry.amt-rub.
                                                          mKom2 = "Нулевая1".
                                                          mSum2 = 0.
                                                      END.
            IF ((mSum < 350000) AND (mSum + op-entry.amt-rub > 350000)) THEN DO: 
                                                                                 mKom1 = "RkoIpSmall".
                                                                                 mSum1 = 350000 - mSum.
                                                                                 mKom2 = "RkoIpMiddle".
                                                                                 mSum2 = op-entry.amt-rub - mSum1.
                                                                             END.
            IF ((mSum >= 350000) OR (op-entry.amt-rub > 350000)) THEN DO: 
                                                                          mKom1 = "Нулевая1".
                                                                          mSum1 = 0.
                                                                          mKom2 = "RkoIpMiddle".
                                                                          mSum2 = op-entry.amt-rub.
                                                                      END.
        END.



/*====== ИП платит на чужие ФЛ счета =================================================================================================================================================================================*/
   IF acct.cust-cat EQ 'Ч' 
      AND acct.bal-acct EQ 40802
      AND op.name-ben NE string(person.name-last + ' ' + person.first-names)
   THEN DO:
            /*MESSAGE "AVAIL ACCT ИП платит на чужие ФЛ счета" view-as alert-box.*/

            /*FOR EACH bop-entry2 WHERE bop-entry2.filial-id EQ shFilial
                                  AND bop-entry2.acct-db   EQ acct.acct
                                  AND can-do('√,√√,ФБК,ФБН,ФБО,ФБП,ФДД', bop-entry2.op-status)
                                  AND bop-entry2.op-date   >= date('01' + substring(string(today),3)) 
                                  AND bop-entry2.op-date   <= date(today)
                                  AND can-do('30102*,30110*,30223*,30301*', bop-entry2.acct-cr)
                NO-LOCK:
                         /*MESSAGE "AVAIL BOP-ENTRY2" view-as alert-box.*/

                         FIND FIRST bop2 WHERE bop2.op EQ bop-entry2.op NO-LOCK NO-ERROR.
                         IF AVAIL bop2 
                              AND can-do('01КЛ,01БУМ', bop2.doc-type)
                              AND can-do('!40101*,!40102*,!40201*,!40204*,!40402*,40817*,40820*,423*,426*', bop2.ben-acct)
                              AND int(bop2.order-pay) > 4
                              AND bop2.name-ben NE string(person.name-last + ' ' + person.first-names)
                         THEN DO:
                                  /*MESSAGE "AVAIL BOP2" view-as alert-box.*/

                                  FIND FIRST op-bank WHERE op-bank.op EQ bop-entry2.op 
                                                       AND op-bank.bank-code-type EQ 'МФО-9'
                                                       AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)                        
                                       NO-LOCK NO-ERROR.
                                  IF AVAIL op-bank THEN DO:
                                                            /*MESSAGE "AVAIL OP-BANK" view-as alert-box.*/

	     	                                            IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency EQ '' 
                                                               AND bop-entry2.amt-rub > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-rub.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.
                          	     	                    
                                                            /*IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency NE ''
                                                               AND bop-entry2.amt-cur > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-cur.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.*/
                                                        END.
                              END.
                         ELSE NEXT.
            END.*/

            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency EQ '' 
               AND op-entry.amt-rub > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-rub.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/
                          	     	                    
            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency NE ''
               AND op-entry.amt-cur > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-cur.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/

            mKom1 = "RkoIpBig".
            mSum1 = op-entry.amt-rub.
            mKom2 = "Нулевая1".
            mSum2 = 0.
        END.

 

/*====== ЮЛ платит на счета ФЛ =======================================================================================================================================================================================*/
   IF acct.cust-cat EQ 'Ю' 
      AND can-do('406*,407*,40807*', acct.acct)
      AND can-do('!*дивиденд*,!*дивид-*,*', op.details)

   THEN DO:
            /*MESSAGE "AVAIL ACCT ЮЛ платит на счета ФЛ" view-as alert-box.*/

            FOR EACH bop-entry2 WHERE bop-entry2.filial-id EQ shFilial
                                  AND bop-entry2.acct-db   EQ acct.acct
                                  AND can-do('√,√√,ФБК,ФБН,ФБО,ФБП,ФДД', bop-entry2.op-status)
                                  AND bop-entry2.op-date   >= date('01' + substring(string(today),3)) 
                                  AND bop-entry2.op-date   <= date(today)
                                  AND can-do('30102*,30110*,30223*,30301*', bop-entry2.acct-cr)
                NO-LOCK:
                         /*MESSAGE "AVAIL BOP-ENTRY2" string(bop-entry2.op) view-as alert-box.*/

                         FIND FIRST bop2 WHERE bop2.op EQ bop-entry2.op NO-LOCK NO-ERROR.
                         IF AVAIL bop2 
                              AND can-do('01КЛ,01БУМ', bop2.doc-type)
                              AND (
                                   (can-do('!40101*,!40102*,!40201*,!40204*,!40402*,40817*,40820*,423*,426*', bop2.ben-acct))
                                   OR
                                   (can-do('47422*', bop2.ben-acct) AND can-do('*40817*,*408.17*,*40820*,*408.20*,*42301*,*423.01*,*42307*,*423.07*,*42601*,*426.01*', bop2.details))
                                   OR
                                   (can-do('30232*,47422*', bop2.ben-acct) AND can-do('*перечислен*карт*,*перечислен*ПК*,*перевод*карт*,*перевод*ПК*,*зачислен*карт*,*зачислен*ПК*,*пополнен*карт*,*пополнен*ПК*', bop2.details))
                                  )
                              AND can-do('!*дивиденд*,!*дивид-*,*', bop2.details)
                              AND int(bop2.order-pay) > 4
                         THEN DO:
                                  /*MESSAGE "AVAIL BOP2" view-as alert-box.*/

                                  FIND FIRST op-bank WHERE op-bank.op EQ bop-entry2.op 
                                                       AND op-bank.bank-code-type EQ 'МФО-9'
                                                       AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)                        
                                       NO-LOCK NO-ERROR.
                                  IF AVAIL op-bank THEN DO:
                                                            /*MESSAGE "AVAIL OP-BANK" view-as alert-box.*/

	     	                                            IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency EQ '' 
                                                               AND bop-entry2.amt-rub > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-rub.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.
                          	     	                    
                                                            /*IF bop-entry2.currency EQ acct.currency
                                                               AND bop-entry2.currency NE ''
                                                               AND bop-entry2.amt-cur > 0
                                                            THEN DO:
                                                                     mSum = mSum + bop-entry2.amt-cur.
                                                                     /*MESSAGE string(mSum) view-as alert-box.*/
                                                                 END.*/
                                                        END.
                              END.
                         ELSE NEXT.
            END.

            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency EQ '' 
               AND op-entry.amt-rub > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-rub.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/
                          	     	                    
            /*IF op-entry.currency EQ acct.currency
               AND op-entry.currency NE ''
               AND op-entry.amt-cur > 0
            THEN DO:
                     mSum = mSum + op-entry.amt-cur.
                     /*MESSAGE string(mSum) view-as alert-box.*/
                 END.*/
            
            IF mSum + op-entry.amt-rub <= 150000 THEN DO: 
                                                          mKom1 = "RkoYlSmall".
                                                          mSum1 = op-entry.amt-rub.
                                                          mKom2 = "Нулевая1".
                                                          mSum2 = 0.
                                                      END.
            IF ((mSum < 150000) AND (mSum + op-entry.amt-rub > 150000)) THEN DO: 
                                                                                 mKom1 = "RkoYlSmall".
                                                                                 mSum1 = 150000 - mSum.
                                                                                 mKom2 = "RkoYlBig".
                                                                                 mSum2 = op-entry.amt-rub - mSum1.
                                                                             END.
            IF ((mSum >= 150000) OR (op-entry.amt-rub > 150000)) THEN DO: 
                                                                         mKom1 = "Нулевая1".
                                                                         mSum1 = 0.
                                                                         mKom2 = "RkoYlBig".
                                                                         mSum2 = op-entry.amt-rub.
                                                                     END.
        END.



   IF     mKom1 = "Нулевая1"
      AND mKom2 = "Нулевая1" 
      AND mSum1 = 0 
      AND mSum2 = 0
   THEN DO:
         oResult = YES.
         {intrface.del}
         RETURN.
        END.



   /*MESSAGE string(op.op) string(op.op-transaction) chr(10) string(substr(op-entry.acct-db, 1, 20)) chr(10) string(mSum) chr(10) string(mSum1) string(mKom1) chr(10) string(mSum2) string(mKom2) view-as alert-box.*/
   /*MESSAGE string(substr(op-entry.acct-db, 1, 20)) chr(10) string(mSum) chr(10) string(mSum1) string(mKom1) chr(10) string(mSum2) string(mKom2) view-as alert-box.*/
   RUN SetSysConf in h_base ("mOp",            op.op).
   RUN SetSysConf in h_base ("mOpTransaction", op.op-transaction).
   RUN SetSysConf in h_base ("mAcctCl",        op-entry.acct-db).
   RUN SetSysConf in h_base ("mDetails",       op.details).
   RUN SetSysConf in h_base ("mSum1",          mSum1).
   RUN SetSysConf in h_base ("mSum2",          mSum2).
   RUN SetSysConf in h_base ("mKom1",          mKom1).
   RUN SetSysConf in h_base ("mKom2",          mKom2).
/*====================================================================================================================================================================================================================*/





 
   /* Вызываем создание комиссионного документа */
   RUN RunTransaction IN h_pbase (mOpKind) NO-ERROR.

   IF ERROR-STATUS:ERROR
   THEN DO:
            /* Произошла ошибка выполняния транзакции начисления комиссии */
            ASSIGN
             mTmpStr = SUBST("Ошибка при выполнении транзакции: &1", mOpKind)
            .
            RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
        END.
   ELSE oResult = YES.

   IF oResult EQ YES
   THEN DO:
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

   oResult = YES.

   RELEASE bop NO-ERROR.
END FINALLY.
