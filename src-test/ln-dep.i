/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: LN-DEP.I
      Comment: Формирование базы начисления.
               Берем из доп. реквизита договора и если нет, то из начального значения
               реквизита.  значения(коды параметров) хранятся через запятую,допускается
               указание  нескольких параметров через "+"
               Можно указывать несколько групп параметров разделенных "&" при этом рез-тат
               начисления будет сумма начислений по каждой группе параметров по ставке из
               реквизита БазаСтав, где указывается ставка для каждой группы через "&.
               Пока максимально 10 групп.
            
               БазаНач - 0+7,....,0&7+13
               БазаСтав = %Кред,..,%КрКом&%КрПр
   Parameters:
         Uses:
      Used by:
      Created: 07.08.2008 15:13 Fepa 93525
*/

PROCEDURE chk-credit.

   DEF INPUT  PARAM iContract        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate            AS DATE NO-UNDO.
   DEF INPUT  PARAM iAcct            AS CHAR NO-UNDO.
   DEF INPUT  PARAM iamt-in-cur-acct AS DEC  NO-UNDO.
   DEF INPUT  PARAM iCorrAcct        AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oResult          AS CHAR NO-UNDO.

   DEF VAR vStopDopVRazresh AS CHAR NO-UNDO. /* НП СтопДопВРазреш */
   DEF VAR vCorrCurr        AS CHAR NO-UNDO. /* валюта корреспондирующего счета */

   DEF BUFFER bloan      FOR loan.      /* Локализация буффера */
   DEF BUFFER bloan-acct FOR loan-acct. /* Локализация буффера */
   DEF BUFFER loan-acct  FOR loan-acct. /* Локализация буффера */

   mb:
   DO ON ERROR UNDO, LEAVE:
      
      /* Находим договор */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
      DO:
         oResult = "Не найден договор с назначением " + iContract + " номером " + iContCode.
         LEAVE mb.
      END.

      /* Находим - привязан ли счет к депозитному договору с ролью "Депоз" */
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ iContract
                             AND bloan-acct.cont-code EQ iContCode
                             AND bloan-acct.acct      EQ iAcct
                             AND bloan-acct.acct-type EQ "Депоз"
                             AND bloan-acct.since     LE iDate
      NO-LOCK NO-ERROR.

      /* Если не привязан счет, то пропускаем и не ругаемся */
      IF NOT AVAIL bloan-acct THEN
         LEAVE mb.

      /* Ставку МинДопВ проверяем только в случае кредитования счета с ролью "Депоз" 
      ** в bloan-acct у нкас действующий счет с ролью "Депоз", и передается кредитуемый счет
      ** если они не равны, значит это другой счет (с ролью, отличной от Депоз) */
      IF bloan-acct.acct EQ iAcct THEN
      DO:
         /* Находим ставку МинДопВ */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "МинДопВ"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
   
         /* Если ставка заведена, то сравниваем сумму довложения со ставкой,
         ** если сумма довл. меньше ставки то ругаемся, иначе пропускаем и не ругаемся */
         IF    AVAIL comm-rate 
           AND iamt-in-cur-acct < comm-rate.rate-comm 
           and bloan.open-date < iDate
         THEN DO:
            IF bloan.currency EQ "" THEN
               oResult = "Ошибка: сумма взноса должна быть не менее " + STRING(comm-rate.rate-comm).
            ELSE
               oResult = "Ошибка: сумма взноса должна быть не менее " + STRING(comm-rate.rate-comm) + "(" + bloan.currency + ")".
            LEAVE mb.
         END.
            /* проверяем, нужно ли производить проверку) */
         ASSIGN 
            vStopDopVRazresh = FGetSetting("СтопДопВРазреш", "", "")
            vCorrCurr = SUBSTRING(iCorrAcct, 6 ,3)
         .
         ASSIGN
            vCorrCurr = "" WHEN vCorrCurr EQ "810".
         FIND FIRST loan-acct WHERE 
                    loan-acct.contract  EQ iContract
                AND loan-acct.cont-code EQ iContCode
                AND loan-acct.acct      EQ iCorrAcct
                AND loan-acct.currency  EQ vCorrCurr
                AND CAN-DO (ENTRY(1, vStopDopVRazresh, ";"), loan-acct.acct-type)
                AND loan-acct.since     LE iDate
         NO-LOCK NO-ERROR.
         IF AVAIL loan-acct THEN
         DO:
            FIND FIRST bloan-acct WHERE 
                       bloan-acct.contract  EQ iContract
                   AND bloan-acct.cont-code EQ iContCode
                   AND bloan-acct.acct-type EQ loan-acct.acct-type
                   AND bloan-acct.since     GT loan-acct.since
                   AND bloan-acct.since     LE iDate
            NO-LOCK NO-ERROR.
            IF NOT AVAIL bloan-acct THEN
               LEAVE mb.
         END.
         IF     NUM-ENTRIES (vStopDopVRazresh, ";") GT 1 
            AND CAN-DO (ENTRY(2, vStopDopVRazresh, ";"), iCorrAcct) THEN
            LEAVE mb.
          /* Теперь проверим СтопДопВ */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "СтопДопВ"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
    
         /* Если нету, то пропускаем, не ругаемся */
         IF NOT AVAIL comm-rate THEN
            LEAVE mb.
          
         IF iDate > bloan.end-date - comm-rate.rate-comm THEN
         DO:
            oResult = "Ошибка: дополнительные взносы по договору разрешены до " + STRING(bloan.end-date - comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.
   END. /* mb: */
END PROCEDURE. /*chk-credit*/

PROCEDURE chk-debet.

   DEF INPUT  PARAM iContract        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate            AS DATE NO-UNDO.
   DEF INPUT  PARAM iAcct            AS CHAR NO-UNDO.
   DEF INPUT  PARAM iamt-in-cur      AS DEC  NO-UNDO.
   DEF INPUT  PARAM iRidEntry        AS RECID NO-UNDO. /* Идентификатор проводки */
   DEF OUTPUT PARAM oResult          AS CHAR NO-UNDO.

   DEF VAR vAcctOst AS DEC NO-UNDO. /* Остаток на счете */
   
   DEF BUFFER bloan FOR loan.           /* Локализация буффера */
   DEF BUFFER bloan-acct FOR loan-acct. /* Локализация буффера */
   DEF BUFFER bop-entry FOR op-entry. /* Локализация буффера */

   mb:
   DO ON ERROR UNDO, LEAVE:
      /* Находим договор */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
      DO:
         oResult = "Не найден договор с назначением " + iContract + " номером " + iContCode.
         LEAVE mb.
      END.

      /* Проверяем дату окончания договора. Если дата проводки больше или равна даты окончания, 
      ** то не выполняем проверку */
      IF iDate >= bloan.end-date THEN
         LEAVE mb.

      /* Находим - привязан ли счет к депозитному договору с ролью "Депоз" */
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ iContract
                             AND bloan-acct.cont-code EQ iContCode
                             AND bloan-acct.acct      EQ iAcct
                             AND bloan-acct.acct-type EQ "Депоз"
                             AND bloan-acct.since     LE iDate
      NO-LOCK NO-ERROR.

      IF NOT AVAIL bloan-acct THEN
         LEAVE mb.

      /* проверка даты частичного изъятия   ivv */

      FIND LAST comm-rate WHERE comm-rate.commission EQ "СтопЧастИ"
                            AND comm-rate.acct       EQ "0"
                            AND comm-rate.currency   EQ bloan.currency
                            AND comm-rate.kau        EQ iContract + "," + iContCode
                            AND comm-rate.min-value  EQ 0
                            AND comm-rate.period     EQ 0
                            AND comm-rate.since      LE iDate
      NO-LOCK NO-ERROR.
 
      IF AVAIL comm-rate THEN DO:
 
        IF iDate < bloan.open-date + comm-rate.rate-comm THEN
         DO:
            oResult = "Ошибка: частичное изъятие допускается, начиная с " + STRING(bloan.open-date + comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.

      /* проверка минимальной суммы частичного изъятия ivv */

      FIND LAST comm-rate WHERE comm-rate.commission EQ "МинСнят"
                            AND comm-rate.acct       EQ "0"
                            AND comm-rate.currency   EQ bloan.currency
                            AND comm-rate.kau        EQ iContract + "," + iContCode
                            AND comm-rate.min-value  EQ 0
                            AND comm-rate.period     EQ 0
                            AND comm-rate.since      LE iDate
      NO-LOCK NO-ERROR.
 
      /* Если нету, то пропускаем, не ругаемся */
      IF AVAIL comm-rate THEN DO:
        IF iamt-in-cur <= comm-rate.rate-comm THEN
         DO:
            oResult = "Ошибка: минимальная сумма снятия " + STRING(comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.




      /* Ставку НеснОст проверяем только в случае дебетования счета с ролью "Депоз" 
      ** в bloan-acct у нас действующий счет с ролью "Депоз", и передается дебетуемый счет
      ** если они не равны, значит это другой счет (с ролью, отличной от Депоз) */
      IF bloan-acct.acct EQ iAcct THEN
      DO:
         /* Находим ставку НеснОст */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "НеснОст"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
   
         /* Если нету, то пропускаем, не ругаемся */
         IF NOT AVAIL comm-rate THEN
            LEAVE mb.
   
         RUN acct-pos IN h_base (iAcct, bloan.currency,
                                 iDate, iDate, CHR(251)).
         vAcctOst = (IF bloan.currency = "" THEN sh-bal ELSE sh-val ).
/*Проверка для проводки статуса крыж на сумму не нужна она учтется в sh-bal*/
         FIND FIRST bop-entry WHERE recid(bop-entry) EQ iRidEntry NO-LOCK NO-ERROR.
         IF AVAIL bop-entry THEN
            IF bop-entry.op-status GE CHR(251) THEN
               iamt-in-cur = 0.
   
         IF ABS(vAcctost) - iamt-in-cur < comm-rate.rate-comm THEN
         DO:
            IF bloan.currency EQ "" THEN
               oResult = "Ошибка: неснижаемый остаток по договору " + STRING(comm-rate.rate-comm).
            ELSE
               oResult = "Ошибка: неснижаемый остаток по договору " + STRING(comm-rate.rate-comm) + "(" + bloan.currency + ")".
            LEAVE mb.
         END.
      END.
   END. /* mb: */

END PROCEDURE. /*chk-debet*/

FUNCTION is-depmax RETURNS CHARACTER ( iContract AS CHAR,
                                       iContCode AS CHAR,
                                       iDate     AS DATE ):

   DEF VAR oResult AS INT64 NO-UNDO.

   DEF BUFFER bloan FOR loan. /* Локализация буффера */

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* Находим договор */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         LEAVE mb.

      /* Проверяем, заведена ли ставка %ДепМакс */
      FIND FIRST comm-rate WHERE comm-rate.commission EQ "%ДепМакс"
                             AND comm-rate.acct       EQ "0"
                             AND comm-rate.currency   EQ bloan.currency
                             AND comm-rate.kau        EQ iContract + "," + iContCode
                             AND comm-rate.min-value  EQ 0
                             AND comm-rate.period     EQ 0
      NO-LOCK NO-ERROR.

      IF AVAIL comm-rate THEN
         oResult = 1.
   END. /* mb: */

   RETURN STRING(oResult).

END FUNCTION. /*is-depmax*/
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='*' */
/* $LINTDATE='09/09/2014 15:54:22.639+04:00' */
/* $LINTFILE='ln-dep.i' */
/*prosignTz5oLJ3PU4G51A1hzaifoA*/