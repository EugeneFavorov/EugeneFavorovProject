/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: CHKTAKE_METH.I
      Comment: Метод контроля за изъятием
   Parameters:
         Uses:
      Used by:
      Created: 12.05.2011 15:53 DEAS    
     Modified: 12.05.2011 15:53 DEAS    
*/
{intrface.get tmess}

PROCEDURE meth_solid.
   DEFINE INPUT  PARAMETER iRec    AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm   AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate   AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrIz      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vShtrPeresch AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vin-kau      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMinOst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOst         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vDateShtrIz  AS DATE        NO-UNDO.
   DEFINE VARIABLE vStartDate   AS DATE        NO-UNDO.
   DEFINE VARIABLE vEnd-date    AS DATE        NO-UNDO.

   DEFINE BUFFER b-loan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.
      
      vShtrIz = GetXattrValueEx ("loan",
                                 b-loan.contract + "," + b-loan.cont-code,
                                 "ШтрафИзъят",
                                 "").
      vDateShtrIz = DATE (vShtrIz) NO-ERROR.
      vShtrPeresch = GetXattrValueEx ("loan",
                                      b-loan.contract + "," + b-loan.cont-code,
                                      "ШтрафПересчет",
                                      ?).
      RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                        iDate,
                                        OUTPUT vStartDate,
                                        OUTPUT vEnd-date).

      IF vDateShtrIz LT vStartDate
         OR (vDateShtrIz GE vStartDate 
             AND NOT {assigned vShtrPeresch}) THEN /* В пределах вирт. жизни вклада еще нет нарушений.*/
      DO:                               /*   нужно проверить */
         /* Мин остаток на вкладе */
         RUN get_last_min_ost IN h_dpspc (RECID (b-loan),
                                          iDate,
                                          iDate,
                                          OUTPUT vMinOst).
         /*Текущий остаток на вкладе*/
         RUN get-summ-ost IN h_dpspc (RECID (b-loan),
                                      iDate,
                                      iDate,
                                      OUTPUT vOst).
         
         vOst = ABS(IF b-loan.currency EQ '' THEN ksh-bal  ELSE ksh-val).
         /* Проверка нарушения МинОст */
         IF vOst - iSumm LT DEC (vMinOst) THEN
         DO:
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "ШтрафИзъят",
                         STRING(iDate) ,
                         NO).
         END.
      END.
   END. /* Main */
END PROCEDURE.

PROCEDURE meth_udob.
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrIz      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mShtrPeresch AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vin-kau      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOst         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vStartDate   AS DATE        NO-UNDO.
   DEFINE VARIABLE vEnd-date    AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateShtrIz  AS DATE        NO-UNDO.
   DEFINE BUFFER b-loan FOR loan.


   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.
      
      vShtrIz = GetXattrValueEx ("loan",
                                 b-loan.contract + "," + b-loan.cont-code,
                                 "ШтрафИзъят",
                                 "").
      vDateShtrIz = DATE (vShtrIz) NO-ERROR.
      mShtrPeresch = GetXattrValueEx ("loan",
                                      b-loan.contract + "," + b-loan.cont-code,
                                      "ШтрафПересчет",
                                      ?).
      RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                  iDate,
                                  OUTPUT vStartDate,
                                  OUTPUT vEnd-date).

      IF vDateShtrIz LT vStartDate /* В пределах вирт. жизни вклада еще нет нарушений.*/
         OR (vDateShtrIz GE vStartDate 
             AND NOT {assigned mShtrPeresch}) THEN 
      DO:                               
         FIND LAST loan-acct WHERE loan-acct.contract  =  b-loan.contract
                               AND loan-acct.cont-code =  b-loan.cont-code
                               AND loan-acct.since     <= iDate
                               AND loan-acct.acct-type = IF vEnd-date = ? THEN "loan-dps-p" ELSE "loan-dps-t"
                               NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  loan-acct THEN 
         DO:
            oMess = "Внимание. Ошибка определения счета вклада".
            RUN Fill-SysMes IN h_tmess ("", 
                                        "", 
                                        "-1", 
                                        oMess).
            UNDO MAIN, LEAVE MAIN.
         END.

         /* Сумма причисленных процентов */
         vin-kau  = b-loan.contract + "," + b-loan.cont-code + ",НачПрС1".
         RUN  kau-pos.p(loan-acct.acct,
                        b-loan.currency,
                        iDate,
                        iDate,
                        gop-status,
                        vin-kau).
         vOst = ABS(IF b-loan.currency EQ '' THEN ksh-bal  ELSE ksh-val).
      
         IF iSumm GT vOst THEN
         DO:
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "ШтрафИзъят",
                         STRING(iDate),
                         NO).
         END.
      END.
   END. /* Main */
END PROCEDURE.

PROCEDURE meth_invst.
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrPereschDop  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vShtrPeresch     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMinOst          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStr-Acct        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTekAcct         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-inter        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-kau          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStartDate       AS DATE        NO-UNDO. /*вирт. открытие вклада*/
   DEFINE VARIABLE vEnd-date        AS DATE        NO-UNDO. /*вирт. закрытие вклада*/
   DEFINE VARIABLE vBegDate         AS DATE        NO-UNDO. /* дата нач. действия вкл. счета */
   DEFINE VARIABLE vEndDate         AS DATE        NO-UNDO. /* дата оконч. действия вкл. счета */
   DEFINE VARIABLE vSummAcct        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vMinOstDec       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vAmtPrich        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mAmtSummIz       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vi               AS INT64       NO-UNDO.
   DEFINE VARIABLE vFlErr           AS INT64       NO-UNDO.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-acct      FOR acct.
   DEFINE BUFFER b-kau-entry FOR kau-entry.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.

      
      vShtrPereschDop = GetXattrValueEx ("loan",
                                         b-loan.contract + "," + b-loan.cont-code,
                                         "ШтрафПересчетДоп",
                                         "").
      IF NOT {assigned vShtrPereschDop} THEN
      DO:
         RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                           iDate,
                                           OUTPUT vStartDate,
                                           OUTPUT vEnd-date).
         /* Основной счет вклада */
         FIND LAST loan-acct WHERE loan-acct.contract  =  b-loan.contract
                               AND loan-acct.cont-code =  b-loan.cont-code
                               AND loan-acct.since     <= iDate
                               AND loan-acct.acct-type = IF vEnd-date = ? THEN "loan-dps-p" ELSE "loan-dps-t"
                               NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  loan-acct THEN 
         DO:
            oMess = "Внимание. Ошибка определения счета вклада".
            UNDO MAIN, LEAVE MAIN.
         END.
         RUN acct-pos IN h_base (loan-acct.acct,
                                 loan-acct.currency,
                                 iDate,
                                 iDate,
                                 gop-status).
         IF loan-acct.currency EQ "" THEN
            vSummAcct = ABS (sh-bal).
         ELSE
            vSummAcct = ABS (sh-val). /* Сумма на осн. счете вклада в день изъятия */

         /* Мин остаток на вкладе */
         RUN get_last_min_ost IN h_dpspc (RECID (b-loan),
                                          iDate,
                                          iDate,
                                          OUTPUT vMinOst).
         vMinOstDec = DEC(vMinOst) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            oMess = "Внимание. Ошибка определения МинОст вклада".
            RUN Fill-SysMes IN h_tmess ("", 
                                        "", 
                                        "-1", 
                                        oMess).
            UNDO MAIN, LEAVE MAIN.
         END.
         IF vSummAcct - iSumm LT vMinOstDec  THEN
         DO:  /* Нарушения МинОст */
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "ШтрафИзъятДоп",
                         STRING(iDate),
                         NO).
         END.
         ELSE
         DO: /* Наруш МинОст нет, проверим нарушение по причисленным процентам */
            vShtrPeresch = GetXattrValueEx ("loan",
                                            b-loan.contract + "," + b-loan.cont-code,
                                            "ШтрафПересчет",
                                           "").
            IF NOT {assigned vShtrPeresch} THEN
            DO:
               /* Определим сумму НачПРс1 за период */
               RUN Get_Interest_KauEntry IN h_dpspr (BUFFER b-loan,
                                                     loan-acct.acct,
                                                     loan-acct.currency,
                                                     b-loan.contract + "," + b-loan.cont-code + ",НачПрс1",
                                                     b-loan.open-date,
                                                     iDate,
                                                     OUTPUT vAmtPrich,
                                                     OUTPUT vFlErr).
/*               IF iSumm GT vAmtPrich THEN*/
               DO:
                  UpdateSigns ("loan",
                               b-loan.contract + "," + b-loan.cont-code,
                               "ШтрафИзъят",
                               STRING(iDate),
                               NO).
               END.
            END.
         END. /* ELSE */
      END. /* IF NOT {assigned vShtrIz} THEN */
   END. /* Main */
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose: Альтернативный метод для определения ограничения на максимальную сумму вклада.
  Указывается в МетодМаксОст. Сумма первого взноса * Значение реквизита МаксОстИнт.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE MaxDepAmount:
   DEFINE INPUT  PARAMETER iRid      AS RECID      NO-UNDO.
   DEFINE INPUT  PARAMETER ibeg      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iend      AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oIn-inter AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess     AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vMinOst       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vClassMaksOst AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMaxOst       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   define variable vAcctRole     as character  no-undo.
   define variable vKodOst       as character  no-undo.
   define variable vKauSurr      as character  no-undo.
   define variable vAcct         as character  no-undo.
   define variable vAmount       as integer    no-undo.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRid
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "Ошибка при определении вклада.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iend,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).

         /*Получение роли счета на период прол*/ 
         RUN GetbaseAcctRole IN h_dps (INPUT RECID(loan),   
                                       INPUT vBegDate,
                                       OUTPUT vAcctRole).

         /*Получение основного счета*/
         RUN GetBaseAcct IN h_dps (loan.contract,
                                   loan.cont-code,
                                   iend,
                                   OUTPUT vAcct).
         {find-act.i
            &acct = "ENTRY(1, vAcct) "
            &curr = "ENTRY(2, vAcct) "
         }
         IF NOT AVAIL acct THEN
            UNDO MAIN, LEAVE MAIN.

         /*Получение кода остатка основного счета*/
         RUN GetBaseKodOst IN h_dps (vAcctRole,
                                    OUTPUT vKodOst).  
         vKauSurr = loan.contract + "," + loan.cont-code + "," + vKodOst.

         /* сумма первоначального взноса */
         IF vBegDate EQ loan.open-date THEN
         DO:
            FIND FIRST kau-entry WHERE kau-entry.acct     EQ acct.acct 
                                   AND kau-entry.currency EQ acct.currency 
                                   AND kau-entry.kau      EQ vKauSurr
                                   AND NOT kau-entry.debit 
                                   AND kau-entry.op-date  GE vBegDate 
               NO-LOCK NO-ERROR.
            IF AVAIL kau-entry THEN
               oIn-inter = IF acct.currency EQ "" THEN kau-entry.amt-rub ELSE kau-entry.amt-cur. 
              ELSE RUN Get_Last_Param IN h_dpspc (iRid,
                                                  iBeg,
                                                  iEnd,
                                                  'МаксОст',
                                                  OUTPUT vMaxOst).
         END.
         ELSE
         DO:
            vBegDate = vBegDate - 1. 
            RUN kau-pos.p( acct.acct,
                           acct.currency,
                           vBegDate,
                           vBegDate,
                           "П",
                           vKauSurr ).
            oIn-inter = ABSOLUTE( IF acct.currency EQ "" THEN ksh-bal ELSE ksh-val).
         END.   
         RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                        ibeg,
                                        iend,
                                        "МаксОстИнт",
                                        OUTPUT vAmount).
         
         IF vAmount = ? THEN 
            DO:
               oMess =  "Не могу определить МаксОстИнт для метода МетодМаксОст=MaxDepAmount.".
               UNDO Main, LEAVE Main.   
            END.
         ELSE
         oIn-inter = IF oIn-inter NE 0 THEN oIn-inter * vAmount ELSE ?.                
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.


/*------------------------------------------------------------------------------
  Метод контроля ИЗЪЯТИЙ со вклада по ОБЩЕЙ СУММЕ ИЗЪЯТИЙ и КОЛИЧЕСТВУ изъятий за определенный ПЕРИОД времени.
  Указывается в МетЛимитИзъят на классе вклада или транзакции открытия.
  Использует реквизиты:
  ИзъятЛимит (в абсолютном выражении начинается с "=" или в % от первоначального взноса начинается с "%")
  ИзъятПериод (формат Г=,М=,Д=)
  ИзъятКолДоп
  не больше ИзъятКолДоп изъятий за период ИзъятПериод, причем в общей сумме не свыше ИзъятЛимит
------------------------------------------------------------------------------*/
PROCEDURE lim_cnt_per:
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmt  AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcctLst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vMaxCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vPeriod       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKauSurr      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAmount       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vFAmount      AS DECIMAL    NO-UNDO.  /* максимальная общая сумма изъятий */
   DEFINE VARIABLE vMaxAmount    AS DECIMAL    NO-UNDO.  /* максимальная общая сумма изъятий */
   DEFINE VARIABLE vMaxAmtChr    AS CHARACTER  NO-UNDO.  /* максимальная общая сумма изъятий */
   DEFINE VARIABLE vListDate     AS CHARACTER  NO-UNDO.  /* список дат изъятий */
   DEFINE VARIABLE tmp-date AS DATE NO-UNDO.
   DEFINE VARIABLE end-dat1 AS DATE NO-UNDO.
   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DEFINE BUFFER bkau-entry FOR kau-entry.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRec
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "Ошибка при определении вклада.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iDate,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).

         RUN get_acct IN h_dpspc (RECID(loan),vBegDate,iDate,OUTPUT vAcctLst).
         vKauSurr  = loan.contract + "," + loan.cont-code + "," + IF vEndDate = ? THEN "ОстВклВ" ELSE "ОстВклС".
         /* по всем вкладным счетам производим подсчет кол-ва и суммы изъятий с составлением списка дат */
         tmp-date = beg-date.
         vListDate = "".
         DO vI = 1 TO NUM-ENTRIES(vAcctLst) :
      
         FIND FIRST acct WHERE acct.acct = ENTRY(vI,vAcctLst) AND acct.currency = loan.currency  NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN NEXT.
         tmp-date = IF vI > 1 THEN DATE(ENTRY(vI - 1,vAcctLst)) ELSE vBegDate.
         end-dat1  = IF NUM-ENTRIES(vAcctLst) >= vI + 3 THEN DATE(ENTRY(vI + 3,vAcctLst)) ELSE vEndDate.
         FOR EACH kau-entry WHERE kau-entry.acct = acct.acct AND 
                               kau-entry.currency = acct.currency AND 
                               kau-entry.kau = vKauSurr AND
                               kau-entry.debit AND 
                               kau-entry.op-date >= tmp-date AND
                               kau-entry.op-date <= end-dat1  NO-LOCK,
                               EACH op-entry OF kau-entry 
                               WHERE NOT CAN-FIND(FIRST bkau-entry OF op-entry WHERE bkau-entry.kau = vKauSurr AND NOT bkau-entry.debit):
            vAmount = vAmount + (IF acct.currency = "" THEN op-entry.amt-rub ELSE op-entry.amt-cur).
            {additem.i vListDate STRING(op-entry.op-date)}
         END.

         END.   /*по всем vAcctLst*/

         /*Получим максимальную общую сумму изъятий*/
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятЛимит',
                                           OUTPUT vMaxAmtChr).
         IF {assigned vMaxAmtChr} THEN DO: 
            IF vMaxAmtChr BEGINS "=" THEN vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
               oMess =  "Ошибка при определении возможной суммы изъятия ИзъятЛимит " + vMaxAmtChr.
               UNDO Main, LEAVE Main.
            END.
            IF vMaxAmtChr BEGINS "%" THEN DO:
               IF vBegDate = loan.open-date THEN
                     vFAmount = DEC(GetXAttrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "СуммЗачислКорСчета",
                                    "0")).
               IF vBegDate > loan.open-date OR vFAmount = 0 THEN DO:
                  /*Получение основного счета*/
                  tmp-date = IF vBegDate > loan.open-date THEN vBegDate - 1 ELSE vBegDate.
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE tmp-date NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "Ошибка при определении основного счета вклада.".
                     UNDO Main, LEAVE Main.   
                  END.
                  RUN  kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     tmp-date,
                     tmp-date,
                     gop-status,
                     vKauSurr).
               vFAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
               END.
               vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                     oMess =  "Ошибка при определении возможной суммы изъятия ИзъятЛимит " + vMaxAmtChr.
                     UNDO Main, LEAVE Main.
                  END.
               vMaxAmount = vMaxAmount * vFAmount / 100.
            END.
         END.
         ELSE DO:
                     oMess =  "Ошибка при определении возможной суммы изъятия ИзъятЛимит".
                     UNDO Main, LEAVE Main.
         END.
         /*Итак, в vMaxAmount максимально возможная сумма изъятия*/
         IF vAmount + iAmt > vMaxAmount THEN DO:
            oMess = "Превышение допустимой суммы изъятия (допустимо " + STRING(vMaxAmount - vAmount) + ")!".
                     UNDO Main, LEAVE Main.
         END.
         /* Проверка на максимальную сумму прошла успешно */
         /*Получим максимальное количество изъятий за период */
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятКолДоп',
                                           OUTPUT vMaxCnt).
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятПериод',
                                           OUTPUT vPeriod).
         IF vMaxCnt = ? THEN DO:
                     oMess =  "Ошибка при определении количества допустимых изъятий ИзъятКолДоп.".
                     UNDO Main, LEAVE Main.
         END.
         IF NUM-ENTRIES(vListDate) >= vMaxCnt THEN 
         DO:
           tmp-date = DATE(ENTRY(NUM-ENTRIES(vListDate) - vMaxCnt + 1,vListDate)).
           end-dat1 = Get-end-date(tmp-date,vPeriod).
           IF end-dat1 = ? THEN DO:
                     oMess =  "Ошибка при определении периода допустимых изъятий ИзъятПериод.".
                     UNDO Main, LEAVE Main.
           END.
           IF end-dat1 >= iDate THEN DO:
                     oMess =  "Исчерпано допустимое количество изъятий до " + STRING(end-dat1) + " включительно!".
                     UNDO Main, LEAVE Main.
           END.
         END.
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.



/*------------------------------------------------------------------------------
  Метод контроля ИЗЪЯТИЙ со вклада ДО НЕСНИЖАЕМОГО ОСТАТКА 
  и [КОЛИЧЕСТВУ изъятий за определенный ПЕРИОД времени](необязательные).
  Указывается в МетЛимитИзъят на классе вклада или транзакции открытия.
  Использует реквизиты:
  ИзъятЛимит - неснижаемый остаток 
  (в абсолютном выражении начинается с "=" или в % от первоначального взноса начинается с "%")
  ИзъятПериод (формат Г=,М=,Д=) 
  ИзъятКолДоп
  не больше ИзъятКолДоп изъятий за период ИзъятПериод, причем остаток на вкладе не меньше ИзъятЛимит
  - если задано только ИзъятКолДоп,то считаем за весь срок вклада
  - если задан только ИзъятПериод, то считаем 1 изъятие за период
   иначе количество не контролируется
------------------------------------------------------------------------------*/
PROCEDURE min_cnt_per:
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmt  AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcctLst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vMaxCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vPeriod       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKauSurr      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAmount       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vFAmount      AS DECIMAL    NO-UNDO.  /* сумма первоначального взноса */
   DEFINE VARIABLE vMaxAmount    AS DECIMAL    NO-UNDO.  /* минимально допустимый остаток */
   DEFINE VARIABLE vMaxAmtChr    AS CHARACTER  NO-UNDO.  /* минимально допустимый остаток в параметре ИзъятЛимит */
   DEFINE VARIABLE vListDate     AS CHARACTER  NO-UNDO.  /* список дат изъятий */
   DEFINE VARIABLE tmp-date AS DATE NO-UNDO.
   DEFINE VARIABLE end-dat1 AS DATE NO-UNDO.
   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DEFINE BUFFER bkau-entry FOR kau-entry.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRec
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "Ошибка при определении вклада.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iDate,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).                                           

         vKauSurr  = loan.contract + "," + loan.cont-code + "," + IF vEndDate = ? THEN "ОстВклВ" ELSE "ОстВклС".

         /*Получим минимальный остаток вклада*/
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятЛимит',
                                           OUTPUT vMaxAmtChr).
         IF {assigned vMaxAmtChr} THEN DO: 
            IF vMaxAmtChr BEGINS "=" THEN vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
               oMess =  "Ошибка при определении неснижаемого остатка вклада ИзъятЛимит " + vMaxAmtChr.
               UNDO Main, LEAVE Main.
            END.
            IF vMaxAmtChr BEGINS "%" THEN DO:
               IF vBegDate = loan.open-date THEN
                     vFAmount = DEC(GetXAttrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "СуммЗачислКорСчета",
                                    "0")).
               IF vBegDate > loan.open-date OR vFAmount = 0 THEN DO:
                  /*Получение основного счета*/
                  tmp-date = IF vBegDate > loan.open-date THEN vBegDate - 1 ELSE vBegDate.
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE tmp-date NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "Ошибка при определении основного счета вклада.".
                     UNDO Main, LEAVE Main.   
                  END.

                  RUN  kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     tmp-date,
                     tmp-date,
                     gop-status,
                     vKauSurr).
               vFAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
               END.
               vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                     oMess =  "Ошибка при определении неснижаемого остатка вклада ИзъятЛимит " + vMaxAmtChr.
                     UNDO Main, LEAVE Main.
                  END.
               vMaxAmount = vMaxAmount * vFAmount / 100.
            END.
         END.
         ELSE DO:
                     oMess =  "Ошибка при определении неснижаемого остатка ИзъятЛимит".
                     UNDO Main, LEAVE Main.
         END.
         /*Итак, в vMaxAmount минимальный остаток вклада*/
                  /*Получение основного счета*/
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE iDate NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "Ошибка при определении основного счета вклада.".
                     UNDO Main, LEAVE Main.   
                  END.
                  RUN  kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     iDate,
                     iDate,
                     gop-status,
                     vKauSurr).
               vAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
         IF vAmount - iAmt < vMaxAmount THEN DO:
            oMess = "Превышение допустимой суммы изъятия (допустимо " + STRING(vAmount - vMaxAmount)  + ")!".
                     UNDO Main, LEAVE Main.
         END.

         /*Получим максимальное количество изъятий за период */
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятКолДоп',
                                           OUTPUT vMaxCnt).
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           'ИзъятПериод',
                                           OUTPUT vPeriod).
         IF vMaxCnt = ? AND NOT {assigned vPeriod} THEN DO:
                     oMess =  "".
                     UNDO Main, LEAVE Main.
         END.
         IF vMaxCnt = ? AND {assigned vPeriod} THEN vMaxCnt = 1.
         RUN get_acct IN h_dpspc (RECID(loan),vBegDate,iDate,OUTPUT vAcctLst).
         /* по всем вкладным счетам производим подсчет кол-ва изъятий с составлением списка дат */
         tmp-date = vBegDate.
         vListDate = "".
         DO vI = 1 TO NUM-ENTRIES(vAcctLst) :
      
         FIND FIRST acct WHERE acct.acct = ENTRY(vI,vAcctLst) AND acct.currency = loan.currency  NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN NEXT.
         tmp-date = IF vI > 1 THEN DATE(ENTRY(vI - 1,vAcctLst)) ELSE vBegDate.
         end-dat1  = IF NUM-ENTRIES(vAcctLst) >= vI + 3 THEN DATE(ENTRY(vI + 3,vAcctLst)) ELSE vEndDate.
         FOR EACH kau-entry WHERE kau-entry.acct = acct.acct AND 
                               kau-entry.currency = acct.currency AND 
                               kau-entry.kau = vKauSurr AND
                               kau-entry.debit AND 
                               kau-entry.op-date >= tmp-date AND
                               kau-entry.op-date <= end-dat1  NO-LOCK,
                               EACH op-entry OF kau-entry 
                               WHERE NOT CAN-FIND(FIRST bkau-entry OF op-entry WHERE bkau-entry.kau = vKauSurr AND NOT bkau-entry.debit):
            {additem.i vListDate STRING(op-entry.op-date)}
         END.

         END.   /*по всем vAcctLst*/

         IF NUM-ENTRIES(vListDate) >= vMaxCnt THEN 
         DO:
           tmp-date = DATE(ENTRY(NUM-ENTRIES(vListDate) - vMaxCnt + 1,vListDate)).
           end-dat1 = IF  {assigned vPeriod} THEN Get-end-date(tmp-date,vPeriod) ELSE vEndDate.
           IF end-dat1 = ? THEN DO:
                     oMess =  "".
                     UNDO Main, LEAVE Main.
           END.
           IF end-dat1 >= iDate THEN DO:
                     oMess =  "Исчерпано допустимое количество изъятий до " + STRING(end-dat1) + " включительно!".
                     UNDO Main, LEAVE Main.
           END.
         END.
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.

