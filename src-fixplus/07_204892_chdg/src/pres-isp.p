/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: PRES-ISP.P
      Comment: Операция исполнения заявки на досрочное погашение.
   Parameters: нет
         Uses:
      Used by:
      Created: 31.06.2007 13:24 Fepa 75124
     Modified: 06.10.2009 16:25 ksv      (0118160) группировка параметров для
                                         компиляции в спец.версии       
*/

DEF INPUT PARAM vStatusTO AS CHAR NO-UNDO.
DEF INPUT PARAM vPreschTo AS CHAR NO-UNDO.
DEF PARAM BUFFER term-obl FOR term-obl.
DEF OUTPUT PARAM vRefresh AS LOGICAL NO-UNDO.

{globals.i}
{intrface.get xclass} /* Загрузка инструментария метасхемы  */
{intrface.get tmess}
{intrface.get loan}
{loan.pro}

DEF VAR mSummToPay      AS DEC   NO-UNDO.
DEF VAR mOldAnSumm      AS DEC   NO-UNDO.
DEF VAR mCredOffset     AS CHAR  NO-UNDO.
DEF VAR mOffset         AS CHAR  NO-UNDO INIT ",->,<-".
DEF VAR mDelayOffset    AS CHAR  NO-UNDO.  /* сдвиг даты окончания плат.периода (осн.долг) */
DEF VAR mDelayOffsetInt AS CHAR  NO-UNDO.  /* сдвиг даты окончания плат.периода (проценты) */
DEF VAR vLCRecid        AS RECID NO-UNDO.
DEF VAR vCrOffset       AS CHAR  NO-UNDO.
DEF VAR mNewAnSumm      AS DEC   NO-UNDO.
DEF VAR vbcondSur       AS CHAR  NO-UNDO.
DEF VAR vicondSur       AS CHAR  NO-UNDO.
DEF VAR vcondSur        AS CHAR  NO-UNDO.
DEF VAR vDblAnnSince    AS DATE  NO-UNDO.
DEF VAR vRecalcAnn      AS LOG   NO-UNDO.
DEF VAR vLCRecNew       AS RECID NO-UNDO.
DEF VAR vLCSummNew      AS DEC   NO-UNDO.
DEF VAR vLoanCondSince  AS DATE  NO-UNDO.
DEF VAR mPayScheme      AS CHAR  NO-UNDO.
DEF VAR mPerSum         AS CHAR  NO-UNDO.
DEF VAR mOffSet2        AS CHAR  NO-UNDO.

DEFINE BUFFER bloan-cond FOR loan-cond.
DEFINE BUFFER iloan-cond FOR loan-cond.
DEFINE BUFFER xterm-obl  FOR term-obl.
DEFINE BUFFER yterm-obl  FOR term-obl.
DEFINE BUFFER bterm-obl  FOR term-obl.
DEFINE BUFFER xcomm-rate FOR comm-rate.
DEFINE BUFFER xloan-cond FOR loan-cond.
DEFINE BUFFER dloan-cond FOR loan-cond.

MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   IF vStatusTO NE "ВВЕД" THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","Операция исполнения доступна только для записей со статусом ВВЕД.").
      LEAVE.
   END.
   FIND FIRST loan WHERE loan.contract  EQ term-obl.contract
                     AND loan.cont-code EQ term-obl.cont-code
      NO-LOCK NO-ERROR.

   IF term-obl.end-date LE loan.since THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","Дата пересчета договора должна быть меньше даты начала действия условия досрочного погашения. Пересчитайте договор.").
      UNDO MAIN, LEAVE MAIN.
   END.

   FIND LAST bloan-cond WHERE bloan-cond.contract  EQ term-obl.contract
                          AND bloan-cond.cont-code EQ term-obl.cont-code
                          AND bloan-cond.since     LT term-obl.end-date NO-LOCK NO-ERROR.
   IF NOT AVAIL bloan-cond THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","По договору нет условий.").
      UNDO MAIN, LEAVE MAIN.
   END.

   mPayScheme = GetXAttrInit(bloan-cond.class-code, "СхемаПлат").

   /* удаляем автоматическое условие */
   IF loan.class-code = "loan_dbl_ann" THEN
   DO:
        blc:
        FOR EACH xloan-cond WHERE
                 xloan-cond.contract  EQ loan.contract
             AND xloan-cond.cont-code EQ loan.cont-code
             AND xloan-cond.since     GT term-obl.end-date
        NO-LOCK :
            IF GetXattrValue("loan-cond",
                             xloan-cond.contract + ","
                             + xloan-cond.cont-code + ","
                             + STRING(xloan-cond.since),
                             "AutoCond")  EQ "Да"
            THEN DO:
               FIND FIRST dloan-cond WHERE RECID(dloan-cond) = RECID(xloan-cond)
                   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
               IF AVAIL  dloan-cond THEN DO:    
                      /* то что делаеться при ручном удалении условия */
                   RUN lc-dell.p(dloan-cond.contract,
                                 dloan-cond.cont-code ,
                                 dloan-cond.since
                                 ).
                   DELETE dloan-cond.      
               END.
               LEAVE blc.
            END.
        END.
   END.

   /* Определяем плановый остаток, действующий на дату исполнения Доср. погаш. */
   FIND LAST bterm-obl WHERE bterm-obl.contract  EQ bloan-cond.contract
                         AND bterm-obl.cont-code EQ bloan-cond.cont-code
                         AND bterm-obl.idnt      EQ 2
                         AND bterm-obl.end-date  LE term-obl.end-date
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   /* Вычисляем сумму, которую осталось погасить, после досрочного погашения */
   mSummToPay = bterm-obl.amt-rub - term-obl.amt-rub.


   /* Странная мысль была... это видимо не надо...
     ASSIGN
        bterm-obl.amt-rub = mSummToPay.*/

   mOldAnSumm = DEC(GetXAttrValueEx("loan-cond",
                                    bloan-cond.contract  + "," +
                                    bloan-cond.cont-code + "," +
                                    STRING(bloan-cond.since),
                                    "АннуитПлат",
                                    "0")).

   FIND FIRST iloan-cond WHERE iloan-cond.contract  EQ term-obl.contract
                           AND iloan-cond.cont-code EQ term-obl.cont-code
                           AND iloan-cond.since     EQ term-obl.end-date 
   NO-LOCK NO-ERROR.

   IF AVAIL iloan-cond THEN
   DO:
      vicondSur = GetSurrogateBuffer("loan-cond",(BUFFER iloan-cond:HANDLE)).
      
      /* Если на эту дату уже существует условие на ту же сумму и тоже ДосрПогаш,
      ** то просто ставим заявке Исп, иначе ошибка */
      IF     GetXAttrValue("loan-cond",vicondSur,"PaySum")  EQ STRING(mSummToPay)
         AND GetXAttrValue("loan-cond",vicondSur,"PayType") EQ "ДосрПогаш" THEN
      DO:
         UpdateSigns(term-obl.class-code,
                     term-obl.contract          + "," +
                     term-obl.cont-code         + "," +
                     STRING(term-obl.idnt)      + "," +
                     STRING(term-obl.end-date)  + "," +
                     STRING(term-obl.nn),
                     "term-obl-status",
                     "ИСП",
                     ?).
      END.
      ELSE
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","Существует условие на дату начала действия заявки на досрочное погашение.").
         UNDO MAIN, LEAVE MAIN.
      END.
   END. 
   
   /* Вызываем процедуру создания условий на дату */
   RUN CrCond IN h_loan (term-obl.contract,
                          term-obl.cont-code,
                         term-obl.end-date,
                         "%Рез,%ОРез,ШтрНеДП"). 

   FIND FIRST loan-cond WHERE 
              loan-cond.contract  EQ term-obl.contract
          AND loan-cond.cont-code EQ term-obl.cont-code
          AND loan-cond.since     EQ term-obl.end-date
   NO-LOCK NO-ERROR.

              
  /* Обновляем ДР PayType для создания условий с типом платежа ДосрПогаш */        
   UpdateSigns(loan-cond.class-code,
               loan-cond.contract + "," +
              loan-cond.cont-code + "," +
              STRING(loan-cond.since),
              "PayType",
               "ДосрПогаш",
               ?).                        
 /* Обновляем ДР PaySum для вывода корректной суммы платежа */   
   UpdateSigns(loan-cond.class-code,
               loan-cond.contract + "," +
              loan-cond.cont-code + "," +
              STRING(loan-cond.since),
               "PaySum",
               TRIM(STRING(term-obl.amt-rub,">>>,>>>,>>>,>>>,>>9.99")),
              ?). 
 
   /* Условие создано, меняем статус на ИСП */
   UpdateSigns(term-obl.class-code,
               term-obl.contract          + "," +
               term-obl.cont-code         + "," +
               STRING(term-obl.idnt)      + "," +
               STRING(term-obl.end-date)  + "," +
               STRING(term-obl.nn),
               "term-obl-status",
               "ИСП",
               ?) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","Ошибка смены статуса заявки на досрочное погашение!").
      UNDO MAIN, LEAVE MAIN.
   END.

   /* Переформируем графики с учетом нового условия */
   ASSIGN
      mCredOffset          = GetXAttrValueEx("loan-cond",vcondSur,"cred-offset","")
      mDelayOffset         = GetXAttrValueEx("loan-cond",vcondSur,"delay-offset","")
      mDelayOffsetInt      = GetXAttrValueEx("loan-cond",vcondSur,"delay-offset-int","")
   .


   RUN SetSysConf IN h_base("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ",
                            STRING(LOOKUP(mCredOffset,mOffset))).
   RUN SetSysConf IN h_base("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ",
                            STRING(LOOKUP(mCredOffset,mOffset))).

   vLCRecid = RECID(loan-cond).

   /*заполнение доп. реков для дифференцированных*/
   IF mPayScheme EQ "Дифференцированная" THEN 
   DO:
      /* Ищем предыдущие условия  */
      FIND LAST bloan-cond WHERE bloan-cond.contract  EQ term-obl.contract
                       AND bloan-cond.cont-code EQ term-obl.cont-code
                       AND bloan-cond.since     LT term-obl.end-date 
      NO-LOCK NO-ERROR.
      IF AVAIL bloan-cond THEN 
      DO:
         mPerSum  = GetXAttrValueEx("loan-cond", bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since), "КредПлат",""). 
         mOffSet2 = GetXAttrValueEx("loan-cond", bloan-cond.contract + "," +
                     bloan-cond.cont-code + "," + STRING(bloan-cond.since), "int-offset",""). 
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "cred-offset",
                     mOffset2,
                     ?).

      END. 

      IF term-obl.symbol EQ "С" THEN
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "КредПлат",
                     "",
                     ?).
      IF term-obl.symbol EQ "Д" THEN
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "КредПлат",
                     mPerSum,
                     ?).
   END. /*IF mPayScheme EQ "Дифференцированная"*/
           
   /* Если на эту дату уже существует запись в графике Пл.Ост. то изменяем сумму */
   FIND FIRST xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract
                          AND xterm-obl.cont-code EQ term-obl.cont-code
                          AND xterm-obl.idnt      EQ 2
                          AND xterm-obl.end-date  EQ term-obl.end-date
                          AND xterm-obl.nn        EQ bterm-obl.nn
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.                   

   IF NOT AVAIL xterm-obl THEN
      CREATE xterm-obl.

   ASSIGN
      xterm-obl.contract     = term-obl.contract
      xterm-obl.cont-code    = term-obl.cont-code
      xterm-obl.end-date     = term-obl.end-date
      xterm-obl.idnt         = 2                  
      xterm-obl.nn           = bterm-obl.nn
      xterm-obl.amt          = mSummToPay
      xterm-obl.dsc-beg-date = term-obl.dsc-beg-date
   .
   
   RELEASE xterm-obl NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","Ошибка создания планового платежа!").
      UNDO MAIN, LEAVE MAIN.
   END.

   /* Пересчитывать дату. Т.е. в результате досрочного 
   ** погашения сокращается срок действия кредита */
   IF vPreschTo EQ "ДАТУ" THEN
   DO:
      /* Сохраняем старую сумму платежа сумму аннуитетного платежа 
      ** на новом условии */
      UpdateSigns(loan-cond.class-code,
                  loan-cond.contract  + "," +
                  loan-cond.cont-code + "," +
                  STRING(loan-cond.since),
                  "АннуитПлат",
                  IF mPayScheme EQ "Дифференцированная" THEN "" ELSE STRING(mOldAnSumm),
                  ?).
      RELEASE loan-cond NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","Ошибка создания условия!").
         UNDO MAIN, LEAVE MAIN.
      END.

      RUN i-avterm.p (RECID(loan),vLCRecid,mSummToPay).

   END.

   /* Пересчитывать сумму. Т.е. в результате досрочного
   ** погашения, срок остается прежний, но сокращается сумма аннуитетных выплат */
   ELSE 
   DO:
      FIND LAST comm-rate WHERE comm-rate.kau        EQ loan.contract + "," + loan.cont-code
                            AND comm-rate.commission EQ "%Кред" 
                            AND comm-rate.since      LT term-obl.end-date NO-LOCK NO-ERROR.

      vCrOffset = GetXAttrValue("loan-cond",
                                vcondSur,
                                "cred-offset").

      FIND FIRST xterm-obl WHERE
                 xterm-obl.contract  EQ loan.contract 
             AND xterm-obl.cont-code EQ loan.cont-code
             AND xterm-obl.idnt      EQ 1
             AND xterm-obl.end-date  EQ loan-cond.since
      NO-LOCK NO-ERROR.

      IF NOT AVAIL xterm-obl THEN
      DO:
         FIND FIRST xterm-obl WHERE
                    xterm-obl.contract  EQ loan.contract 
                AND xterm-obl.cont-code EQ loan.cont-code
                AND xterm-obl.idnt      EQ 1
                AND xterm-obl.end-date  GT loan-cond.since
         NO-LOCK NO-ERROR.
         IF AVAIL xterm-obl THEN
            vLoanCondSince = xterm-obl.end-date.
         ELSE
            vLoanCondSince = loan-cond.since.

      END.
      ELSE
         vLoanCondSince = loan-cond.since.

      /* Рассчитаем сумму аннуитетного платежа с учетом суммы досрочного погашения */
      RUN CalcAnnuitet (loan.contract,
                        loan.cont-code,
                        vLoanCondSince,
                        loan.end-date,
                        mSummToPay,
                        comm-rate.rate-comm,
                        loan-cond.cred-date,
                        loan-cond.cred-period,
                        "1",
                        0,
                        LOOKUP(vCrOffset,"--,->,<-"),
                        INT64(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "АннуитКорр",
                                            ?)
                            ),
                        DEC(GetXAttrValueEx("loan", 
                                            loan.contract + "," + loan.cont-code, 
                                            "Sum-depos",
                                            "0")
                            ),
                        INT64(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "FirstPeriod",
                                            "0")
                            ),
                        DEC(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "PartAmount",
                                            "0")
                            ),
                        OUTPUT mNewAnSumm).

      /* Сохраняем новую сумму аннуитетного платежа */
      UpdateSigns(loan-cond.class-code,
                  loan-cond.contract  + "," +
                  loan-cond.cont-code + "," +
                  STRING(loan-cond.since),
                  "АннуитПлат",
                  IF mPayScheme EQ "Дифференцированная" THEN "" ELSE STRING(mNewAnSumm),
                  ?).
      
      RELEASE loan-cond NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","Ошибка создания условия!").
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN i-avterm.p (RECID(loan),vLCRecid,mSummToPay).
   END.

   /* Проставим ОД и Проц дату dsc-beg-date */
   FIND FIRST yterm-obl WHERE yterm-obl.contract  EQ term-obl.contract
                          AND yterm-obl.cont-code EQ term-obl.cont-code
                          AND yterm-obl.idnt      EQ 1
                          AND yterm-obl.end-date  EQ term-obl.end-date
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL yterm-obl THEN
      ASSIGN
         yterm-obl.dsc-beg-date = term-obl.dsc-beg-date.

   FIND FIRST yterm-obl WHERE yterm-obl.contract  EQ term-obl.contract
                          AND yterm-obl.cont-code EQ term-obl.cont-code
                          AND yterm-obl.idnt      EQ 3
                          AND yterm-obl.end-date  EQ term-obl.end-date
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL yterm-obl THEN
      ASSIGN
         yterm-obl.dsc-beg-date = term-obl.dsc-beg-date.

   /* определяем дату начала второго условия */
   IF loan.class-code EQ "loan_dbl_ann" THEN
   DO:
      FIND FIRST loan-cond WHERE RECID(loan-cond) = vLCRecid 
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-cond THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","Ошибка создания условия!").
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN GetDateDblAnn(loan.contract, loan.cont-code, loan-cond.since, OUTPUT vDblAnnSince).
      vRecalcAnn = IF vPreschTo EQ "ДАТУ" THEN NO ELSE YES.
      /* Создаем, если нужно, автоматическое условие  */
      IF loan-cond.since < vDblAnnSince THEN DO:            

         RUN Cr_Cond_DblAnn IN h_Loan (loan.contract, 
                                       loan.cont-code, 
                                       loan-cond.since, 
                                       NO, 
                                       vRecalcAnn,
                                       vDblAnnSince,
                                       OUTPUT vLCRecNew,
                                       OUTPUT vLCSummNew) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            RUN fill-sysmes IN h_tmess ("","","0","Ошибка создания условия периода увеличения!").
            UNDO MAIN, LEAVE MAIN.
         END.
         /* что бы два раза не выводить графики */
         RUN SetSysConf IN h_Base("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН","ДА").
         RUN i-avterm.p (RECID(loan),vLCRecNew,vLCSummNew).
         RUN DeleteOldDataProtocol IN h_Base("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН").
      END.
   END.
   
   RUN DeleteOldDataProtocol IN h_base("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
   RUN DeleteOldDataProtocol IN h_base("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").

END.
vRefresh = YES.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:49:52.833+04:00' */
/* $LINTFILE='pres-isp.p' */
/*prosign0NeA1NBQmoHfS5AHZno9Aw*/