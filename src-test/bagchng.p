/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: BAGCHNG.P
      Comment: Привязка договоров к ПОСу.
   Parameters: tmprecid,режим
         Uses:
      Used by:
      Created: 30.06.2007 12:13 Om      
     Modified: 24.03.2009 20:15 KSV      (0108286) Исправлена ошибка с выбором
                                         по F1 в QBIS
     Modified: 30.03.2009 17:34 feok     <comment>
     Modified: 23.01.2011 15:44 Om        (0139888) Оптимизация кода.  
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get bag}      /* Библиотека для работ с ПОС. */
{intrface.get i254}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
                        /* Локальная таблица отметок. */
{tmprecid.def &NGSH =  "LOCAL"}

DEF INPUT   PARAM TABLE FOR tmprecid BIND.   /* Получаем таблицу по указателю. */
DEF INPUT   PARAM iMode AS CHAR NO-UNDO.     /* Режим работы (POSAdd, POSRemove). */

DEF VAR mPos         AS CHAR     NO-UNDO.
DEF VAR mBegDate     AS DATE     NO-UNDO.
DEF VAR mResult      AS LOG      NO-UNDO. /* Рузельтат создания POSa. */
DEF VAR mFl          AS LOG      NO-UNDO.

DEFINE BUFFER bBag   FOR loan.
DEFINE BUFFER tloan  FOR loan.

pick-value = ?.
CNHBG:
DO
ON ERROR    UNDO CNHBG, LEAVE CNHBG
ON ENDKEY   UNDO CNHBG, LEAVE CNHBG:
   {getdate.i
      &DispAfterDate =  "mPos FORMAT 'X(30)' LABEL 'ПОС' HELP 'Выберите ПОС'"
      &UpdAfterDate  =  "mPos WHEN  iMode EQ 'POSAdd'"
      &AddLookUp     =  "IF {&LAST_KEY} EQ KEY-CODE ('F1') AND {&FRAME_FIELD} EQ 'mPos'
                         THEN DO TRANSACTION:
                           RUN browseld.p ('UniformBag', 'open-date2', end-date, '', 4).
                           IF LASTKEY EQ 10
                              THEN DISPLAY pick-value @ mPos.
                         END.
                         ELSE "
      &AddPostUpd    =  "IF mPos NE '' THEN DO:
                           IF NOT CAN-FIND (FIRST loan WHERE
                                                   loan.contract  EQ 'ПОС'
                                             AND   loan.cont-code EQ mPos)
                           THEN DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '1',
                                  'Указан неверный код портфеля.'
                              ).
                              UNDO, RETRY.
                           END.
                           IF CAN-FIND (FIRST loan WHERE
                                              loan.parent-contract  EQ 'ПОС'
                                          AND loan.parent-cont-code EQ mPos)
                           THEN DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '1',
                                  'Нельзя привязывать к ПОС-ам верхнего уровня.'
                              ).
                              UNDO, RETRY.
                           END.
                         END.
                         ELSE DO:
                           IF iMode EQ 'POSAdd' THEN
                           DO:
                              RUN Fill-SysMes IN h_tmess (
                                 '', '', '1',
                                 'Необходимо укзать ПОС.'
                              ).                           
                              UNDO, RETRY.
                           END.
                           ELSE DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '4',
                                  'Вывести ссуды из портфелей?'
                              ).
                              IF pick-value NE 'YES'
                                 THEN UNDO CNHBG, LEAVE CNHBG.
                           END.
                         END.
                        "
      &DateLabel     =  "Дата привязки"
      &DateHelp      =  "Введите дату привязки к ПОСу (F1 - календарь)"
      &return        =  "UNDO CNHBG, LEAVE CNHBG"
   }

   mBegDate = end-date.
   IF mPos <> "" THEN
   FIND FIRST bBag WHERE bBag.contract  = "ПОС"
                     AND bBag.cont-code = mPos
   NO-LOCK NO-ERROR.

   IF    AVAIL bBag 
     AND bBag.close-date   <= end-date THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Выбран ПОС, закрытый на дату " + QUOTER (bBag.close-date) + ".").
      UNDO CNHBG, LEAVE CNHBG.
   END.
   
                        /* Инициализация процесса протоколирования. */
   RUN Init-SysMes IN h_tmess (
      "op-kind,КонвНаст",
      (IF   iMode =  "POSAdd"
       THEN  SUBSTITUTE("Привязка договоров к ПОС &1 на дату &2.",mPos,mBegDate)
       ELSE "Вывод договоров из ПОС."
      ), "").

   N_LOAN:
   FOR EACH tmprecid  NO-LOCK,
      FIRST loan WHERE RECID(loan) =  tmprecid.id
   NO-LOCK:
      IF loan.open-date >  mBegDate THEN
      DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС: "         +
            "дата привязки к портфелю не может выходить за рамки действия договора "   +
            QUOTER (STRING (loan.open-date, "99/99/9999")) + "." 
         ).
         NEXT N_LOAN.
     END.
     IF CAN-FIND(FIRST term-obl WHERE
                       term-obl.contract     =  loan.contract
                   AND term-obl.cont-code    =  loan.cont-code
                   AND term-obl.idnt         =  128
                   AND term-obl.end-date     =  mBegDate
                   AND term-obl.lnk-contract =  "ПОС" 
                 NO-LOCK) 
     THEN DO:
        RUN Fill-SysMes IN h_tmess ('', '', '1','Нельзя вывести договор из ПОСа в день включения. Удалите запись вручную!').
        NEXT N_LOAN.
     END.

                        /* Если ДР "ПОСИскл" установлен а "ДА" - всегда исключать договор из ПОСа.
                        ** и договор в ключен в ПОС. */
      IF       GetXAttrValue("loan",loan.contract + "," + loan.cont-code, "ПОСИскл")   =  "Да"                         
         AND   LnInBagOnDate(loan.contract, loan.cont-code, mBegDate)                  <> ?     THEN
      DO:
                        /* Исключаем договор из ПОС. */
         RUN SetLinkPos(loan.contract, loan.cont-code, "", mBegDate, ?, YES, OUTPUT mFl).         
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "ИСКЛЮЧЕН договор " + QUOTER (loan.cont-code) + " из ПОС. " +            
            " ДР ~"ПОСИскл~"(Исключать из ПОС)~ имеет значение ~"ДА~"."
         ).
         NEXT N_LOAN.
         END.
      IF LENGTH (mPos) >  0   THEN
         DO:
/*                
           /* проверка на наличие операций по урегулированию резерва по договору */
         IF VerifyPOSDate(loan.contract, loan.cont-code, mBegDate)   <> NO THEN
            DO:
            RUN Fill-SysMes IN h_tmess (
               "","","-1",
               "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС: "   +
               "не прошла проверка на наличие операций по урегулированию резерва."
            ).
            NEXT N_LOAN.
            END.
*/
                           /* Проверка возможности включения в ПОС транша */
         IF       NUM-ENTRIES(loan.cont-code, " ")                            =  2 
            AND   CanTranshToPOS ((BUFFER loan:HANDLE),  mPos, mBegDate, YES) <> YES   THEN
               DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС: " +
               "не прошла проверка возможности включения в ПОС транша."               
            ). 
            NEXT N_LOAN.
               END.
                           /* Проверка включения ссуды в ПОС по признакам однородности. */
            RUN ChkLoanUniformity IN h_bag (
               (BUFFER loan:HANDLE),
               (BUFFER bBag:HANDLE),
               mBegDate,
            YES,
               OUTPUT mResult
            ).
            IF NOT mResult THEN
            DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС: "    +
               "не прошла проверка включения ссуды в ПОС по признакам однородности."
            ). 
            NEXT N_LOAN.
            END.
                           /* Проверка соответствия по срокам */
            RUN ChkDelayConformity IN h_bag (
               (BUFFER loan:HANDLE),
               (BUFFER bBag:HANDLE),
               mBegDate,
            YES,
               OUTPUT mResult
            ).
            IF NOT mResult THEN 
            DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС: "   +
               "не прошла проверка соответствия по срокам."
            ). 
            NEXT N_LOAN.
            END.
         END.
                           /* Формирвоание связи. */ 
         RUN SetLinkPos IN h_bag (loan.contract, 
                                  loan.cont-code, 
                                  mPos, 
                                  mBegDate, 
                                  ?,
                                  YES, 
                                  OUTPUT mResult).
         IF NOT mResult THEN
         DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "ОШИБКА привязки договора " + QUOTER (loan.cont-code) + " к ПОС:  "  +
            "не удалось сформировать связь."            
         ).
         NEXT N_LOAN.
         END.
         
      IF loan.cont-type =  "Течение" THEN 
         FOR EACH tloan WHERE tloan.contract  =  loan.contract 
                          AND tloan.cont-code =  loan.cont-code
                          AND tloan.cont-code BEGINS loan.cont-code + " "
                          AND NUM-ENTRIES(tloan.cont-code," ") =  2
                          AND tloan.open-date >= mBegDate
                          AND tloan.close-date =  ?
         NO-LOCK:
            RUN UpdateUniformBag IN h_bag (BUFFER tloan).
         END.
             
      RUN Fill-SysMes IN h_tmess (
         "", "", "1",         
         IF LENGTH (mPos) > 0
            THEN ("Договор " + QUOTER (loan.cont-code) + 
                  " успешно привязан к ПОСу " + QUOTER (mPos) + ".")
            ELSE ("Договор " + QUOTER (loan.cont-code) 
                             + " успешно выведен из ПОС.")
      ).
      IF LENGTH (mPos) <= 0
      THEN DO:
         IF    pick-value <> "3"
           AND pick-value <> "4" 
         THEN
            RUN messmenu.p (10,
               "[Исключить из ПОС насовсем]",
               " Установить признак постоянного исключения " +
               "из ПОС для договора " + loan.cont-code + "?",
               "1. Да,"                +
               "2. Нет,"               +
               "3. Да для всех,"       +
               "4. Нет для всех"     
               ).
         IF NOT KEYFUNCTION(LASTKEY) =  "end-error"
         THEN DO:
            IF    pick-value =  "1"
               OR pick-value =  "3" 
            THEN DO:
               IF NOT UpdateSignsEx (loan.class-code,
                                     loan.contract + "," + loan.cont-code,
                                     "ПОСИскл",
                                     "Да") 
               THEN
                  RUN Fill-sysmes IN h_tmess ("", "", "1",
                        "Не могу установить ПОСИскл = Да для договора " + loan.cont-code).
            END.
         END.
      END.
   END.
END.
                        /* Завершение процесса протоколирования. */
RUN End-SysMes IN h_tmess.
{intrface.del}          /* Выгрузка инструментария. */ 

RETURN.
/* $LINTFILE='bagchng.p' */
/* $LINTMODE='1,4,6,3' */
/* $LINTENV ='2st' */
/* $LINTVSS ='*' */
/* $LINTUSER='pase' */
/* $LINTDATE='17/03/2017 16:04:13.005+04:00' */
/*prosignxyy7Tes3AtYY1rwqgapdmQ*/