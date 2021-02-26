/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: DPSPR_TAX.I
      Comment: Блок процедур библиотеки dpspr для начисления налогов.
   Parameters:
         Uses:
      Used by: 
      Created: 11/04/2005 MIOA
     Modified: 
  Last change:
*/
&GLOB Xvvv-dbg 
DEF STREAM vvv-dbg.

/******************************************************************************/
/*                         НАЧИСЛЕНИЕ НАЛОГОВ                                 */
/******************************************************************************/
DEF VAR  Xresult AS DECIMAL NO-UNDO.
DEFINE VARIABLE mTemp-op-id AS CHARACTER   NO-UNDO.


{calc_rate.def}
{calc_rate.i}
/* Функция определения необходимости взятия налога при начислении %% */
FUNCTION fNeedNalog RETURNS LOGICAL (INPUT iRowID AS ROWID,
                                     INPUT iDate  AS DATE):

   DEFINE VARIABLE vCommRate      AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vCommRateStart AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vCommRateCB-5  AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vCBRefComm     AS CHARACTER   NO-UNDO.   
   DEFINE VARIABLE vPonigRate     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInterest      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegAcctOp     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegAcct       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vProcInclRate  AS CHARACTER   NO-UNDO.   

   DEFINE VARIABLE vDateStart AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateEnd   AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateStartOpen AS DATE        NO-UNDO.
   DEFINE VARIABLE vOldWork   AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vRetVal    AS LOGICAL INIT YES NO-UNDO.
   DEFINE VARIABLE vFirst     AS LOGICAL     NO-UNDO.

   DEFINE BUFFER   b-comm-rate  FOR comm-rate.
   DEFINE BUFFER   c-comm-rate  FOR comm-rate.
   DEFINE BUFFER   cc-comm-rate FOR comm-rate.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER b1-change  FOR tt-change.
   DEF BUFFER b2-change  FOR tt-change.

/*
Налоговый кодекс, статья 217:
Доходы, не подлежащие налогообложению (освобождаемые от налогообложения)
- доходы в виде процентов, получаемые налогоплательщиками по вкладам в банках,
  находящихся на территории Российской Федерации, если:

  - проценты по рублевым вкладам выплачиваются в пределах сумм, рассчитанных 
    исходя из действующей ставки рефинансирования ЦБ РФ, в течение периода,
    за который начислены указанные проценты;

  - установленная ставка не превышает 9 процентов годовых по вкладам в 
    иностранной валюте;

  - проценты по рублевым вкладам, которые на дату заключения договора либо 
    продления договора были установлены в размере, не превышающем действующую 
    ставку рефинансирования ЦБ РФ, при условии, что в течение периода 
    начисления процентов размер процентов по вкладу не повышался и с момента,
    когда процентная ставка по рублевому вкладу превысила ставку 
    рефинансирования ЦБ РФ, прошло не более трех лет;
*/
   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* Поиск договора */
      FIND FIRST loan WHERE ROWID(loan) EQ iRowID
         NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         UNDO MAIN, LEAVE MAIN.
         
      /*comment by deas. Столь сложный и ненадежный механизм вызыван необходимостью 
      максимально возможно уменьшить запуск nachkin.p (в CALC_Change_Rate)
      Более подробную информацию можно получить в КД 117979.*/

      /*Получение значения настроечного параметра "ПроцИскПонижСтав"*/
      vProcInclRate = fGetSetting("ПроцВклПонижСтав","", "").
      RUN Get_last_param IN h_dpspc (RECID(loan),
                               iDate,
                               iDate,
                               "ПонижОснСтавки",
                               OUTPUT vPonigRate).

      RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                        iDate,
                                        OUTPUT vDateStart,
                                        OUTPUT vDateEnd).

      /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
      &IF DEFINED(vvv-dbg) &THEN                                                                      
      OUTPUT STREAM vvv-dbg TO "vvv-dbg.tmp" APPEND.                                                 
      PUT STREAM vvv-dbg UNFORM "==========================" skip.                                    
      PUT STREAM vvv-dbg UNFORM loan.cont-code " " iDate " " vDateStart " " vDateEnd SKIP.            
      &ENDIF.                                                                                         
      /********************************************************************************************/

      /*1.Определение предельной ставки рефинансирования на дату операции*/                                  
      RUN GetBaseAcct IN h_dps (loan.contract,
                                loan.cont-code,
                                iDate,
                                OUTPUT vBegAcct).
      {find-act.i
         &acct = "ENTRY(1, vBegAcct) "
         &curr = "ENTRY(2, vBegAcct) "
      }
      IF NOT AVAIL acct THEN
         UNDO MAIN, LEAVE MAIN.
         
      /*Получение значения кода настроечного параметра "ЦбРефПред" далее - поиск с помощью findcom.i*/
      vCBRefComm = fGetSetting("ЦбРефПред", ?, "%ЦбРеф").
      {findcom1.i
         &comm-rate  = comm-rate
         &dir        = LAST
         &rsum       = 0
         &rcom       = vCBRefComm
         &since1     = "LE iDate "
         }
      IF NOT AVAIL comm-rate THEN                                   
         UNDO MAIN, LEAVE MAIN.                                     
      /*2.Определение ставки по вкладу на дату операции*/
      RUN  Calc_Date_CommRate (RECID(loan),
                               iDate,
                               OUTPUT vCommRate).
      /*Определение кода схемы начисления по вкладу на дату операции*/
      RUN Get_Last_Inter IN h_dpspc (RECID(loan),
                                     iDate,
                                     iDate,
                                     OUTPUT vInterest).
      {findsch.i
          &dir    = LAST
          &sch    = vInterest
          &since1 = " LE vDateEnd "
      }
      
      /*Определение предельной ставки рефинансирования и ставки по вкладу на дату открытия вклада*/
      RUN GetBaseAcct IN h_dps (loan.contract,
                                   loan.cont-code,
                                vDateStart,
                                OUTPUT vBegAcctOp).
      {find-act.i
         &acct = "ENTRY(1, vBegAcct) "
         &curr = "ENTRY(2, vBegAcct) "
      }
      IF NOT AVAIL acct THEN
         UNDO MAIN, LEAVE MAIN.

      {findcom1.i
         &comm-rate  = b-comm-rate
         &dir        = LAST                                      
         &rsum       = 0                                         
         &rcom       = vCBRefComm                                /*ставка ЦБ на дату операции         comm-rate.rate-comm*/ 
         &since1     = "LE vDateStart "                          /*ставка по вкладу на дату операции vCommRate*/            
         }
      IF NOT AVAIL b-comm-rate THEN                              /*ставка ЦБ на дату открытия вклада      b-comm-rate.rate-comm*/
         UNDO MAIN, LEAVE MAIN.                                  /*ставка по вкладу на дату открытия вкла vCommRateStart*/



      IF vDateStart EQ loan.open-date THEN
         vDateStartOpen = loan.open-date + 1. 
      ELSE
         vDateStartOpen = vDateStart.

      RUN  Calc_Date_CommRate (RECID(loan),
                               vDateStartOpen,
                               OUTPUT vCommRateStart).

      IF loan.open-date GE DATE("15.12.2014") AND loan.open-date LE DATE("31.12.2015") THEN
         vCommRateCB-5 = b-comm-rate.rate-comm - 5.
      ELSE
         vCommRateCB-5 = b-comm-rate.rate-comm.

      /*Ставка по вкладу превышает ставку рефинансирования на дату операции?*/
      IF vCommRate GT comm-rate.rate-comm THEN
      DO:
         /*Ставка по вкладу МЕНЬШШЕ ставки рефинансирования? (на дату открытия)*/
         IF NOT (vCommRateStart GT vCommRateCB-5)
            AND vCommRate LE vCommRateStart /*Ставка по вкладу НЕ повысилась?*/ THEN
         DO:
            /*Процедуры нет в списке НП и реквизит не равен "Да"*/
            IF NOT CAN-DO (vProcInclRate,interest-sch-line.proc-name)
               AND vPonigRate NE "Да"  THEN
            DO:
               /*Найти дату, когда ставка рефинансирования стала меньше ставки по вкладу.*/
               {findcom1.i
                  &comm-rate  = c-comm-rate
                  &dir        = FIRST
                  &rsum       = 0
                  &rcom       = vCBRefComm
                  &since1     = "GE vDateStart "     
                  &vRate-Comm = "LT vCommRate "                      
                  }
               IF AVAIL c-comm-rate
                  AND c-comm-rate.since LE iDate /*иначе не имеет смысла */ THEN
               DO:
                  IF (MonInPer(c-comm-rate.since,iDate) / 12) <= 3 /*Период Меньше трёх лет?*/ THEN
                  DO:
                     vRetVal = NO.
                     /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
                     &IF DEFINED(vvv-dbg) &THEN                                                                      
                     PUT STREAM vvv-dbg UNFORM
                       " 1)Ставка по вкладу превышает ставку рефинансирования на дату операции " vCommRate " " comm-rate.rate-comm SKIP
                       " Ставка по вкладу МЕНЬШШЕ ставки рефинансирования? (на дату открытия)  " vCommRateStart " " vCommRateCB-5 SKIP
                       " Ставка по вкладу НЕ повысилась? " vCommRate " " vCommRateStart
                       " Процедура начисления " interest-sch-line.proc-name " отсутствует в списке " vProcInclRate SKIP     
                       " Реквизит на транзакции открытия ПонижОснСтав " vPonigRate SKIP
                          " Прошло меньше трех лет c " c-comm-rate.since " по " iDate " " (MonInPer(vDateStart, c-comm-rate.since) / 12) <= 3 SKIP.
                     PUT STREAM vvv-dbg UNFORM "fNeedNalog " vRetVal SKIP.           
                     &ENDIF. 
                     /********************************************************************************************/
                  END.
               END.
            END.
            ELSE
               vOldWork = YES.
         END.
      END. /*Ставка по вкладу превышает ставку рефинансирования на дату операции?*/
      ELSE
      DO:
         /*Процедура начисления отсутствует в списке? */
         IF NOT CAN-DO (vProcInclRate,interest-sch-line.proc-name) THEN
         DO:
            IF vPonigRate EQ "Да" THEN
               vOldWork = YES.
            ELSE
            DO:
               /*Ставка по вкладу НЕ больше рефинансирования? (на дату открытия) */
               IF vCommRateCB-5 GE vCommRateStart
                  AND vCommRate LE vCommRateStart THEN 
               DO:
                  /*Возможен вариант, когда ставка по вкладу была постоянной на всем протяжении,
                  а ставка рефинансирования падала меньше ставки по вкладу, затем снова поднималась над ставкой вклада*/
                  {findcom1.i
                     &comm-rate  = cc-comm-rate
                     &dir        = FIRST
                     &rsum       = 0
                     &rcom       = vCBRefComm
                     &since1     = "GE vDateStart "     
                     &vRate-Comm = "LT vCommRate "                      
                     
                  }
                  IF AVAIL cc-comm-rate
                     AND cc-comm-rate.since LE iDate /*иначе не имеет смысла */ THEN
                  DO:
                     /*Проверим на 3 года*/
                     IF (MonInPer(cc-comm-rate.since,iDate) / 12) <= 3 THEN
                     DO:
                        vRetVal = NO.
                        /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
                        &IF DEFINED(vvv-dbg) &THEN                                                                      
                        PUT STREAM vvv-dbg UNFORM
                          " 1)Ставка по вкладу НЕ превышает ставку рефинансирования на дату операции " vCommRate " " comm-rate.rate-comm SKIP
                          "  Процедура начисления " interest-sch-line.proc-name " отсутствует в списке " vProcInclRate SKIP     
                          "  Реквизит на транзакции открытия ПонижОснСтав " vPonigRate SKIP
                          "  Ставка по вкладу НЕ больше рефинансирования? (на дату открытия)" vCommRateCB-5 " " vCommRateStart SKIP 
                          "  Ставка по вкладу не увеличилась " vCommRateStart " " vCommRate  SKIP
                          "  Нашлась ставка рефинансирования, которая понижалась меньше ставки вклада, а затем повыш." STRING(AVAIL cc-comm-rate)  SKIP
                          "  Ставка нашлась, проверка на 3 года" STRING ((MonInPer(/*vDateStart*/c-comm-rate.since,iDate) / 12))  SKIP.
                        PUT STREAM vvv-dbg UNFORM "fNeedNalog " vRetVal SKIP.           
                        &ENDIF. 
                        /********************************************************************************************/
                     END.
                  END.
                  ELSE
                  DO:
                     vRetVal = NO.
                     /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
                     &IF DEFINED(vvv-dbg) &THEN                                                                      
                     PUT STREAM vvv-dbg UNFORM
                       " 1)Ставка по вкладу НЕ превышает ставку рефинансирования на дату операции " vCommRate " " comm-rate.rate-comm SKIP
                       "  Процедура начисления " interest-sch-line.proc-name " отсутствует в списке " vProcInclRate SKIP     
                       "  Реквизит на транзакции открытия ПонижОснСтав " vPonigRate SKIP
                       "  Ставка по вкладу НЕ больше рефинансирования? (на дату открытия)" vCommRateCB-5 " " vCommRateStart SKIP 
                       "  Ставка по вкладу не увеличилась " vCommRateStart " " vCommRate  SKIP.
                     PUT STREAM vvv-dbg UNFORM "fNeedNalog " vRetVal SKIP.           
                     &ENDIF. 
                     /********************************************************************************************/
                  END.
               END.
            END.
         END.
         ELSE
            vOldWork = YES.
      END.
      IF vOldWork THEN
      DO: /*Запустим медленный, но верный механизм*/
         mTemp-op-id = GetSysConf("op-id").
         RUN DeleteOldDataProtocol IN h_base ("op-id").
         RUN  CreateCommBySince(iDate,
                                vDateStart,
                                RECID(loan)).
         RUN SetSysConf IN h_Base ("op-id", mTemp-op-id).
         
         /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
         &IF DEFINED(vvv-dbg) &THEN                                                                      
         vFirst = YES.                                                                                   
         FOR EACH tt-change:                                                                             
            IF tt-change.begdate >= vDateStart AND vFirst THEN                                           
            DO:                                                                                          
               vFirst = NO.                                                                              
               PUT STREAM vvv-dbg UNFORM "== виртуальное открытие вклада " vDateStart " ====" skip.      
            END.                                                                                         
            PUT STREAM vvv-dbg UNFORM tt-change.begdate " " tt-change.com " " tt-change.com-cb skip.     
         END.                                                                                            
         &ENDIF                                                                                          
         /**********************************************************************************************/

         FOR EACH tt-change WHERE tt-change.begdate < vDateStart:
             DELETE tt-change.
         END.
         FIND FIRST b-change USE-INDEX begdate NO-LOCK NO-ERROR.
         FIND LAST b1-change WHERE b1-change.com <=  b1-change.com-cb USE-INDEX begdate NO-LOCK NO-ERROR.      
         FIND FIRST tt-change WHERE tt-change.com >  tt-change.com-cb USE-INDEX begdate NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-change 
            OR
            (
               b-change.com <=  b-change.com-cb AND /* на дату заключения не превышали ставку ЦБ */
               NOT CAN-FIND(FIRST tt-change WHERE tt-change.com > b-change.com USE-INDEX begdate) AND /* проценты по вкладу не повышались */
               (MonInPer(tt-change.begdate, iDate) / 12) <= 3 /* прошло меньше трех лет */
            ) THEN
               vRetVal = NO.
         /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
         &IF DEFINED(vvv-dbg) &THEN
         IF AVAIL tt-change THEN
         DO:
            PUT STREAM vvv-dbg UNFORM "на дату заключения не превышали ставку ЦБ " b-change.com <=  b-change.com-cb SKIP.
            PUT STREAM vvv-dbg UNFORM "проценты по вкладу не повышались " NOT CAN-FIND(FIRST tt-change WHERE tt-change.com > b-change.com USE-INDEX begdate) SKIP.
            PUT STREAM vvv-dbg UNFORM "прошло меньше трех лет c " tt-change.begdate " по " iDate " " (MonInPer(tt-change.begdate, iDate) / 12) <= 3 SKIP.
         END.
         PUT STREAM vvv-dbg UNFORM "fNeedNalog " vRetVal skip.
         &ENDIF
         /**********************************************************************************************/
      END.  /* IF vOldWork THEN */
      /***************************ИНФОРМАЦИЯ ДЛЯ ОТЛАДКИ*********************************************/
      &IF DEFINED(vvv-dbg) &THEN
      OUTPUT STREAM vvv-dbg CLOSE.
      &ENDIF
      /**********************************************************************************************/
   END. /*MAIN*/

   RETURN vRetVal.
END FUNCTION.


/* Расчет налогооблагаемой базы для перенесенных процентов 
   Процедура определяет интервал расчета для перенесенных процентов и 
   запускает процедуру основного цикла расчета.

   ПАРАМЕТРЫ:
      INPUT  iLoanRI        - RECID обрабатываемого договора
      INPUT  iOpDate        - Дата опердня
      INPUT  iContDate      - Плановая дата
      INPUT  iOpTransaction - Код транзакции, если расчет запускается из 
                              парсерных функций. Если расчет для ведомости и/или
                              код транзакции определить невозможно - ?.
      INPUT  iLatestMove    - Искать САМУЮ последнюю проводку переноса (TRUE)
                              или последнюю относительно дат iOpDate iContDate (FALSE)
                              TRUE следует использовать при реальном начислении,
                              FALSE - при расчете налоговых ведомостей.
      INPUT  iNeg           - Учитывать отрицательный итог
      INPUT  iFlPrint       - Печатать ведомость
      
      OUTPUT oResult        - Сумма налогооблагаемой базы
      OUTPUT oFlEr          - Код ошибки. 0, если ошибок не было.
      
   ПРИМЕЧАНИЯ:
      Всегда используются основные схема и ставка, т.к. перенесенные проценты
      есть проценты зарезервированные, а резерв начисляется по основной схеме
*/

PROCEDURE Calc_TaxBase_Moved:
   DEFINE INPUT  PARAMETER iLoanRI        AS RECID   NO-UNDO.
   DEFINE INPUT  PARAMETER iOpDate        AS DATE    NO-UNDO.
   DEFINE INPUT  PARAMETER iContDate      AS DATE    NO-UNDO.
   DEFINE INPUT  PARAMETER iOpTransaction AS INT64 NO-UNDO.
   DEFINE INPUT  PARAMETER iLatestMove    AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER iNeg           AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER iFlPrint       AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult        AS DECIMAL NO-UNDO INIT 0.
   DEFINE OUTPUT PARAMETER oFlEr          AS INT64 NO-UNDO INIT -1.
   
   DEFINE VAR vCodOst   AS CHAR    NO-UNDO.
   DEFINE VAR vBegDate  AS DATE    NO-UNDO.
   DEFINE VAR vEndDate  AS DATE    NO-UNDO.
   DEFINE VAR vDate1    AS DATE    NO-UNDO.
   DEFINE VAR vDate2    AS DATE    NO-UNDO.   
   DEFINE var ldate    as date no-undo.
   DEFINE var end-dat1  as date no-undo.
   DEFINE VAR oResult1  AS DECIMAL NO-UNDO INIT 0.
   DEFINE VAR oResult2  AS DECIMAL NO-UNDO INIT 0.
   DEFINE var nach_end  as logical no-undo.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      
      /* 1. Поиск записи вклада */
      FIND FIRST loan WHERE RECID(loan) = iLoanRI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan THEN 
         UNDO MAIN, LEAVE MAIN.
   
      /* 2. Проверка осмысленности расчета перенесенных процентов
            (если даже резервного счета нет - 
             какие, пардон, могут быть перенесенные проценты?) */
      FIND LAST loan-acct OF loan 
                          WHERE loan-acct.acct-type =  'loan-dps-int'
                            AND loan-acct.since     <= iOpDate
                          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan-acct THEN 
         UNDO MAIN, LEAVE MAIN.
      nach_end = FGetSetting("NachEndLoan",?,'Да') eq 'Да'.
      /* 3. Определение интервала начисления */
      /* 3.1 Определяем даты последнего резервирования и последнего переноса */
      vCodOst = loan.contract + "," + loan.cont-code + ",НачПр".      
   
      IF iOpDate > iContDate THEN 
      DO:
         RUN get_beg_date_per IN h_dpspc (iLoanRI, 
                                          iOpDate, 
                                          OUTPUT vEndDate).
         RUN Get-Beg-Obl-T    IN h_dpspc (vCodOst,
                                          IF iLatestMove THEN {&VERY_FAR_FUTURE_DATE} 
                                                         ELSE (iContDate - 1), 
                                          iOpTransaction, 
                                          OUTPUT vBegDate).
      END.
      ELSE 
      DO:
         RUN get_beg_date_per IN h_dpspc (iLoanRI, 
                                          iContDate, 
                                          OUTPUT vEndDate).
         RUN Get-Beg-Obl-T    IN h_dpspc (vCodOst,
                                          IF iLatestMove THEN {&VERY_FAR_FUTURE_DATE} 
                                                         ELSE (iOpDate - 1),
                                          iOpTransaction, 
                                          OUTPUT vBegDate).
      END.
      /* 3.1.1 Проверка, а необходимо ли вообще начислять налог */
      IF fNeedNalog(ROWID(loan),
                    vEndDate) THEN
      DO:      
         /* 3.2 Корректировка даты начала расчета, если после пролонгации списания не было
                ВНИМАНИЕ!!!! Здесь - дата корректируется правильно. 
                То, что было в стандартной версии в процедуре БазаНалог (через new_dps_prol) - неверно. */
         RUN get-beg-date-prol   IN h_dpspc (iLoanRI, 
                                             iContDate, 
                                             OUTPUT vDate1, 
                                             OUTPUT vDate2).
         IF     vBegDate EQ vDate1        
            AND vDate1   NE loan.open-date THEN
            RUN correct_date IN h_dpspc (iLoanRI, 
                                         INPUT-OUTPUT vBegDate).
         /* 3.3 Проверка полученных дат */
         IF    vBegDate GE iContDate 
            OR vBegDate GE vEndDate THEN 
            UNDO MAIN, LEAVE MAIN.
         
         /* 3.4 Поиск капитализации, проведенной после последнего переноса, 
                но до последнего резервирования - такая капитализация тоже
                влияет на дату начала интервала расчета */
         RUN get_beg_kper IN h_dpspc (iLoanRI, 
                                      vEndDate, 
                                      iOpTransaction,
                                      INPUT-OUTPUT vBegDate).         
         IF vBegDate EQ ? THEN 
            UNDO MAIN, LEAVE MAIN.
      
         ldate = loan.end-date.
         if ldate <> ? and ldate <= vEndDate and vBegDate lt ldate then end-dat1 = ldate.
            else if (ldate <> ?  and ldate > vEndDate) or ldate = ? then end-dat1 = vEndDate.
            else end-dat1 = if not nach_end then  vBegDate else iContDate.
        RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, 
                                                 vBegDate, 
                                                 end-dat1,
                                                 FALSE, /* Не проверять даты */
                                                 TRUE,  /* Использовать основную ставку */
                                                 TRUE,  /* Использовать основную схему  */
                                                 TRUE,  /* Текущие ставки */
                                                 iNeg, iFlPrint,
                                                 loan.contract + "," + loan.cont-code + 
                                                       IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                                 OUTPUT oResult1, 
                                                 OUTPUT oFlEr).
        if end-dat1 lt vEndDate  then DO:
        RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, 
                                                 end-dat1, 
                                                 vEndDate,
                                                 TRUE, /* Не проверять даты */
                                                 TRUE,  /* Использовать основную ставку */
                                                 TRUE,  /* Использовать основную схему  */
                                                 TRUE,  /* Текущие ставки */
                                                 iNeg, iFlPrint,
                                                 loan.contract + "," + loan.cont-code + 
                                                       IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                                 OUTPUT oResult2, 
                                                 OUTPUT oFlEr).
        END.
      oResult =  oResult1 + oResult2.
      END.
   END.        

END PROCEDURE. /* END OF PROCEDURE Calc_TaxBase_Moved */

PROCEDURE Calc_TaxBase_NachNch:
   DEFINE INPUT  PARAMETER iLoanRI        AS RECID   NO-UNDO.
   DEFINE INPUT  PARAMETER iOpDate        AS DATE    NO-UNDO.
   DEFINE INPUT  PARAMETER iContDate      AS DATE    NO-UNDO.
   DEFINE INPUT  PARAMETER iOpTransaction AS INT64 NO-UNDO.
   DEFINE INPUT  PARAMETER iLatestMove    AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER iNeg           AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER iFlPrint       AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult        AS DECIMAL NO-UNDO INIT 0.
   DEFINE OUTPUT PARAMETER oFlEr          AS INT64 NO-UNDO INIT -1.
   
   DEFINE VAR vDate1    AS DATE    NO-UNDO.
   DEFINE VAR vDate2    AS DATE    NO-UNDO.   
   DEFINE var ldate    as date no-undo.
   DEFINE VAR oResult1  AS DECIMAL NO-UNDO INIT 0.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      
      /* 1. Поиск записи вклада */
      FIND FIRST loan WHERE RECID(loan) = iLoanRI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan THEN 
         UNDO MAIN, LEAVE MAIN.
   
      /* 2. Проверка осмысленности расчета перенесенных процентов
            (если даже резервного счета нет - 
             какие, пардон, могут быть перенесенные проценты?) */
      FIND LAST loan-acct OF loan WHERE loan-acct.acct-type =  'loan-dps-int'
                            AND loan-acct.since     <= iOpDate
                          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan-acct THEN 
         UNDO MAIN, LEAVE MAIN.

      /* 3.1.1 Проверка, а необходимо ли вообще начислять налог */
      IF fNeedNalog(ROWID(loan),
                    iContDate) THEN
      DO:      
         /* 3.2 Корректировка даты начала расчета, если после пролонгации списания не было
                ВНИМАНИЕ!!!! Здесь - дата корректируется правильно. 
                То, что было в стандартной версии в процедуре БазаНалог (через new_dps_prol) - неверно. */
         RUN get-beg-date-prol IN h_dpspc (iLoanRI, 
                                           iContDate, 
                                           OUTPUT vDate1, 
                                           OUTPUT vDate2).
         IF vDate1 NE loan.open-date THEN
            RUN correct_date IN h_dpspc (iLoanRI, 
                                         INPUT-OUTPUT vDate1).

         RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, 
                                                 vDate1, 
                                                 vDate2,
                                                 FALSE, /* Не проверять даты */
                                                 TRUE,  /* Использовать основную ставку */
                                                 TRUE,  /* Использовать основную схему  */
                                                 TRUE,  /* Текущие ставки */
                                                 iNeg, iFlPrint,
                                                 loan.contract + "," + loan.cont-code + 
                                                       IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                                 OUTPUT oResult1, 
                                                 OUTPUT oFlEr).
         oResult =  oResult1.
      END.
   END.        

END PROCEDURE. /* END OF PROCEDURE Calc_TaxBase_NachNch */


/******************************************************************************/
/* Расчет налогооблагаемой базы для доначисленных процентов
   Процедура определяет интервал расчета для доначисленных процентов и
   запускает процедуру основного цикла расчета.
   
   Иногда интервал может делиться на 2 части.

   ПАРАМЕТРЫ:
      INPUT  iLoanRI        - RECID обрабатываемого договора                    
      INPUT  iOpDate        - Дата опердня                                      
      INPUT  iContDate      - Плановая дата                                     
      INPUT  iOpTransaction - Код транзакции, если расчет запускается из        
                              парсерных функций. Если расчет для ведомости и/или
                              код транзакции определить невозможно - ?.
      INPUT  iFlClose       - Флаг "Операция проводится при закрытии вклада"
                              Если транзакция, которая вызывает процедуту, является
                              транзакцией закрытия, либо каким-то образом определяется, 
                              что это расчет налога при закрытии вклада - 
                              флаг устанавилвается в TRUE. 
      INPUT  iPen           - По какой ставке расчитывать проценты за время,
                              прошедшее после даты окончания вклада 
                              (используется, если вклад закрывается или пролонгируется 
                              позже даты окончания):
                              1      - Штрафные ставка и схема
                              2      - Основные ставка и схема
                              другое - Основные ставка и схема, но если 
                                       расчет идет при пролонгации - 
                                       использовать ставку и схему, которые начинаяют 
                                       действовать после пролонгации.
      INPUT  iNeg           - Учитывать отрицательный итог
      INPUT  iFlPrint       - Печатать ведомость          
      INPUT  iIgnorePastNch - Игнорировать наличие капитализации или начисления после даты расчета.
                              Если флаг установлен, 
                                  то ДАЖЕ при наличии начислений или капитализаций ПОСЛЕ 
                                  iContDate начало интервала начисления будет считаться 
                                  от последнего начисления ПЕРЕД iContDate до iContDate.
                              Если флаг не установлен - процедура будет искать начисления ПОСЛЕ iContDate
                              и не позволять расчет при их наличии.
                              
                              Параметр введен, чтобы процедурой можно было пользоваться как 
                              при начислении налога, так и для расчета ведомостей задним числом.
                              
                              Если расчитывается ведомость задним числом - следует установить флаг.
                              Если расчитываются проценты для взятия налога - флаг снимается.
      
      OUTPUT oResult        - Сумма налогооблагаемой базы        
      OUTPUT oFlEr          - Код ошибки. 0, если ошибок не было.
   
   ПРИМЕЧАНИЯ:
      
*/
PROCEDURE Calc_TaxBase_Added:
DEFINE INPUT  PARAMETER iLoanRI        AS RECID   NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate        AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iContDate      AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iOpTransaction AS INT64 NO-UNDO.
DEFINE INPUT  PARAMETER iFlClose       AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iPen           AS INT64 NO-UNDO.
DEFINE INPUT  PARAMETER iNeg           AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iFlPrint       AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iIgnorePastNch AS LOGICAL NO-UNDO. /* Игнорировать наличие капитализации после даты расчета */
DEFINE OUTPUT PARAMETER oResult        AS DECIMAL NO-UNDO INIT 0.
DEFINE OUTPUT PARAMETER oFlEr          AS INT64 NO-UNDO INIT 0.

/* Настройки */
DEFINE VAR vNachEndLoan AS LOGICAL NO-UNDO.
/* Даты */
/* Интервал начисления по основной ставке */
DEFINE VAR vBegDate  AS DATE    NO-UNDO.
DEFINE VAR vEndDate  AS DATE    NO-UNDO.
/* Интервал начисления по выборной ставке */
DEFINE VAR vBegDate2 AS DATE    NO-UNDO.
DEFINE VAR vEndDate2 AS DATE    NO-UNDO.
/* Даты интервала пролонгации */
DEFINE VAR vDate1    AS DATE    NO-UNDO.
DEFINE VAR vDate2    AS DATE    NO-UNDO.
/* Способ определения комиссии */
DEFINE VAR vFlComm    AS LOGICAL NO-UNDO. /* Да  - комиссия на текущую дату начисления, 
                                             Нет - комиссия до интервала начисления */
DEFINE VAR vBaseComm  AS LOGICAL NO-UNDO.
DEFINE VAR vBaseInter AS LOGICAL NO-UNDO.
/* Другое */
DEFINE VAR vFlProl    AS LOGICAL NO-UNDO.
DEFINE VAR vDelay     AS INT64 NO-UNDO.
DEFINE VAR vResult    AS DECIMAL NO-UNDO.

   /* 1. Поиск записи вклада */
   FIND FIRST loan WHERE RECID(loan) = iLoanRI NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan THEN RETURN.

   /* 2. Читаем настройки */
   vNachEndLoan = (FGetSetting("NachEndLoan",?,"Да") = "Да").

   /* 3. Определение полного интервала начисления */
   /* 3.1 Дата начала интервала - дата последнего начисления процентов 
          (вообще последнего или последнего перед датой iContDate - в зависимости от параметра) */
   IF iIgnorePastNch THEN DO:
     /* Делаем вручную работу функции get-date-nach, 
        НО! не относительно бесконечно далекой даты, а относительно iContDate */
     /* Поиск последней даты резерв. процентов относительно iContDate */
     RUN get_beg_date_per in h_dpspc(iLoanRI, iContDate, OUTPUT vBegDate).

     /* Поиск последней даты капитализации процентов */
     RUN get_beg_kper in h_dpspc(iLoanRI, iContDate, iOpTransaction, INPUT-OUTPUT vBegDate).
     RUN Get-Beg-Signs-T in h_dpspc(loan.contract + ',' + loan.cont-code,
                                            vBegDate,
                                            iContDate, 
                                            iOpTransaction, 
                                            OUTPUT vBegDate).
   END. 
   ELSE DO:
     RUN get-date-nach in h_dpspc (iLoanRI,
                                           iContDate,
                                           iOpTransaction,
                                           OUTPUT vBegDate).
   END. 

   /* 3.2 Проверка даты начала интервала */
   IF vBegDate = ? OR vBegDate >= iContDate THEN RETURN.

   /* 3.3 Дата окончания периода начисления */
   vEndDate = iContDate. /* для начала берем плановую дату */
   /*     коррекция по параметру "delay", если у нас идет закрытие вклада */
   IF iFlClose THEN DO:
     RUN get_last_delay in h_dpspc (iLoanRI,vBegDate,vEndDate, OUTPUT vDelay).
     IF vDelay < 0 THEN vEndDate = vEndDate + vDelay.
   END.
   /*     коррекция по параметру NachEndLoan - 
          если после окончания вклада начислять нельзя - обрежем интервал до окончания вклада */
   RUN get-beg-date-prol in h_dpspc (iLoanRI, iOpDate, OUTPUT vDate1, OUTPUT vDate2).
   IF vDate2 <> ? AND NOT vNachEndLoan THEN vEndDate = vDate2.
   
   /* 4. Определение полупериодов начисления */
   IF vDate2 < vBegDate THEN ASSIGN
     /* весь период - укладывается в выборный */
     vBegDate2 = vBegDate
     vEndDate2 = vEndDate
     vBegDate  = vDate2
     vEndDate  = vDate2
   .
   ELSE IF vBegDate <= vDate2 AND vDate2 <= vEndDate THEN ASSIGN
     /* период делится на основной и выборный  датой окончания вклада */
     vBegDate2 = vDate2
     vEndDate2 = vEndDate
     vEndDate  = vDate2
   .
   ELSE /* vDate2 > vEndDate */ ASSIGN
     /* весь период - укладывается в основной */
     vBegDate2 = vDate2
     vEndDate2 = vDate2
   .
   /* 4.1 Проверка, а необходимо ли вообще начислять налог */
   IF fNeedNalog(ROWID(loan),
                 vEndDate) THEN
   DO:    
      
      /* Начислить по основной ставке */
      RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, vBegDate, vEndDate,
                                              TRUE, /* Проверять даты */
                                              TRUE, /* Использовать основную ставку */
                                              TRUE, /* Использовать основную схему  */
                                              TRUE, /* Текущие ставки */
                                              iNeg, iFlPrint,
                                              loan.contract + "," + loan.cont-code + 
                                                    IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                              OUTPUT oResult, OUTPUT oFlEr).
      CASE iPen:
      WHEN 1 THEN ASSIGN vBaseComm = FALSE vBaseInter = FALSE vFlComm = FALSE. /* Ставки до интервала */
      WHEN 2 THEN ASSIGN vBaseComm = TRUE  vBaseInter = TRUE  vFlComm = FALSE. /* Ставки до интервала */
      OTHERWISE   DO:
                  ASSIGN vBaseComm = TRUE     vBaseInter = TRUE     vFlComm = TRUE.  /* Текущие ставки */
      
                  RUN Chk_Limit_Per in h_dpspc (iOpDate, iLoanRI, loan.prolong + 1, OUTPUT vFlProl).
                  IF NOT vFlProl THEN ASSIGN /* исчерпаны пролонгации */
                    vBaseComm  = ?
                    vBaseInter = ?
                  .
      END.
      END CASE.
      /* Начислить по выборной ставке */
      RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, vBegDate2, vEndDate2,
                                              TRUE,       /* Проверять даты */
                                              vBaseComm,  /* Использовать основную ставку */
                                              vBaseInter, /* Использовать основную схему  */
                                              vFlComm,
                                              iNeg, iFlPrint,
                                              loan.contract + "," + loan.cont-code + 
                                                    IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                              OUTPUT vResult, OUTPUT oFlEr).
      oResult = oResult + vResult.
   END.

   RETURN.
END PROCEDURE. /* END OF PROCEDURE Calc_TaxBase_Added */

/******************************************************************************/
/* Расчет налогооблагаемой базы весь период жизни вклада
   (от даты открытия/последней пролонгации до даты iOpDate iContDate).
   Процедура определяет полный интервал расчета и
   запускает процедуру основного цикла расчета.

   ПАРАМЕТРЫ:
      INPUT  iLoanRI        - RECID обрабатываемого договора
      INPUT  iOpDate        - Дата опердня
      INPUT  iContDate      - Плановая дата
      INPUT  iOpTransaction - Код транзакции, если расчет запускается из        
                              парсерных функций. Если расчет для ведомости и/или
                              код транзакции определить невозможно - ?.
      INPUT  iOpKind        - Код БИСквит-транзакции.
      INPUT  iBase          - Использовать базовые схему и ставку?
      INPUT  iNeg           - Учитывать отрицательный итог
      INPUT  iKap           - Обрабатывать параметр ОснКап 
                              (перерасчитывать налогооблагаемую базу от 
                               даты последней капитализации)
      INPUT  iFlPrint       - Печатать ведомость
      
      OUTPUT oResult        - Сумма налогооблагаемой базы
      OUTPUT oFlEr          - Код ошибки. 0, если ошибок не было.
*/
PROCEDURE Calc_TaxBase_Full:
DEFINE INPUT  PARAMETER iLoanRI        AS RECID   NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate        AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iContDate      AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iOpTransaction AS INT64 NO-UNDO.
DEFINE INPUT  PARAMETER iOpKind        AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER iBase          AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iKap           AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iNeg           AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iFlPrint       AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oResult        AS DECIMAL NO-UNDO INIT 0.
DEFINE OUTPUT PARAMETER oFlEr          AS INT64 NO-UNDO INIT 0.

/* Даты  */
DEFINE VAR vBegDate  AS DATE    NO-UNDO.
DEFINE VAR vEndDate  AS DATE    NO-UNDO.
DEFINE VAR vDate1    AS DATE    NO-UNDO.
DEFINE VAR vDate2    AS DATE    NO-UNDO.
/* Промежуточные параметры */
DEFINE VAR vDelay      AS INT64 NO-UNDO.            /* Сдвиг начисления в конце срока  */
DEFINE VAR vCloseLoan  AS LOGICAL NO-UNDO INIT FALSE. /* Наше начисление - в конце срока?*/
DEFINE VAR vFlComm     AS CHAR    NO-UNDO.            /* Флаг для определения ставки     */

   /* 1. Поиск записи вклада */
   FIND FIRST loan WHERE RECID(loan) = iLoanRI NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan THEN RETURN.
   
   /* 2. Определение интервала начисления
         2.1 Определяем интервал переоформления вклада */
   RUN get-beg-date-prol in h_dpspc (iLoanRI, iContDate, OUTPUT vDate1, OUTPUT vDate2).
   /*    2.2 Определяем даты начала и окончания расчета */
   vBegDate  = vDate1.
   vEndDate  = iContDate.
   /*    2.3 Корректировка даты начала расчета, 
             если дата переоформления включается в новый период начисления процентов */
   RUN correct_date   in h_dpspc (iLoanRI, INPUT-OUTPUT vBegDate).
   IF iKap THEN DO:
       RUN get_beg_kper in h_dpspc (iLoanRI,iContDate,iOpTransaction,
                                            INPUT-OUTPUT vBegDate).
   END. 
   /*    2.4 Корректировка даты окончания расчета с учетом сдвига начисления в конце срока */
   RUN get_last_delay in h_dpspc (iLoanRI, iContDate, iContDate, OUTPUT vDelay).
   IF iOpKind <> ? THEN
      RUN chk_close   in h_dpspc (iOpKind, loan.class-code, loan.loan-status, OUTPUT vCloseLoan).
   IF vCloseLoan AND vDelay <> ? THEN vEndDate = vEndDate + vDelay.

   /* 2.5 Проверка, а необходимо ли вообще начислять налог */
   IF fNeedNalog(ROWID(loan),
                 vEndDate) THEN
   DO:
      /*  3. Заодно определим, по какой ставке надо начислять проценты, если 
             вклад закрывается досрочно и среди параметров нет признака 'Осн' */
      RUN get_srok_vklad in h_dpspc (iLoanRI, vEndDate, OUTPUT vFlComm).
   
      RUN Calc_TaxBase_Any IN THIS-PROCEDURE (iLoanRI, vBegDate, vEndDate,
                                              FALSE,                      /* Не проверять даты */
                                              (iBase OR vFlComm  = "-1"), /* Использовать основную ставку */
                                              iBase,                      /* Использовать основную схему  */
                                              TRUE,                       /* Текущие ставки */
                                              iNeg, iFlPrint,
                                              loan.contract + "," + loan.cont-code + 
                                                    IF vDate2 = ? THEN ",ОстВклВ" ELSE ",ОстВклС",
                                              OUTPUT oResult, OUTPUT oFlEr).
   END.

   RETURN.
END PROCEDURE. /* END OF PROCEDURE Calc_TaxBase_Full */

/******************************************************************************/
/* Основной Цикл расчета налогооблагаемой базы 
   (служебная функция, применяемая в остальных расчетах налогов)
   
   ПАРАМЕТРЫ:
      INPUT  iLoanRI    - RECID обрабатываемого договора
      INPUT  iBegDate   - Начало интервала расчета
      INPUT  iEndDate   - Конец интервала расчета
      INPUT  iFlEnd     - Коррекция даты окончания расчета
                          (TRUE - только при расчете налога
                           на доначисленные проценты - БазаНалогД)
      INPUT  iBaseComm  - Использовать основную ставку (? означает искать ставку по pen-op-kind)
      INPUT  iBaseInter - Использовать основную схему  (? означает искать схему  по pen-op-kind)
      INPUT  iFlDate    - Дата определения ставки и схемы 
                          (TRUE  - дата в интервале расчета, 
                           FALSE - дата до начала интервала)
      INPUT  iNeg       - Учитывать отрицательный итог
      INPUT  iFlPrint   - Печатать ведомость
      INPUT  iKau       - Код основного субостатка вклада

      OUTPUT oResult    - Сумма налогооблагаемой базы
      OUTPUT oFlEr      - Kод ошибки. 0, если ошибок не было.
*/
PROCEDURE Calc_TaxBase_Any:
DEFINE INPUT  PARAMETER iLoanRI    AS RECID   NO-UNDO.
DEFINE INPUT  PARAMETER iBegDate   AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iEndDate   AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iFlEnd     AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iBaseComm  AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iBaseInter AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iFlDate    AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iNeg       AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iFlPrint   AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER iKau       AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER oResult    AS DECIMAL NO-UNDO INIT 0.
DEFINE OUTPUT PARAMETER oFlEr      AS INT64 NO-UNDO INIT 0.
DEFINE var vXstr       AS CHAR    NO-UNDO.

/* Счета */
DEFINE VAR vAcctList   AS CHAR    NO-UNDO.
DEFINE VAR vi          AS INT64 NO-UNDO.
/* Сроки */
DEFINE VAR vBegDateA   AS DATE    NO-UNDO.
DEFINE VAR vBegDateA1   AS DATE    NO-UNDO.
DEFINE VAR vBegDateA2   AS DATE    NO-UNDO.
DEFINE VAR vEndDateA   AS DATE    NO-UNDO.
DEFINE VAR vSubEndDate AS DATE    NO-UNDO.
DEFINE VAR vSubEndDate1 AS DATE    NO-UNDO.
DEFINE VAR vSubEndDate2 AS DATE    NO-UNDO.
DEFINE VAR vTmpDate    AS DATE    NO-UNDO.
/* Комиссии */
DEFINE VAR vComm       AS CHAR    NO-UNDO.
DEFINE VAR vInter      AS CHAR    NO-UNDO.
DEFINE VAR vCIDate1    AS DATE    NO-UNDO. /* дата определения комиссии и ставки */
DEFINE VAR vCIDate2    AS DATE    NO-UNDO. /* дата определения комиссии и ставки */
DEFINE VAR loanend     AS DATE    NO-UNDO.
/* Переменые для промежуточных результатов начисления */
DEFINE VAR vRes        AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VAR vRes1       AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VAR vFlLog      AS LOGICAL NO-UNDO.
def var i as int64 no-undo.
   vCIDate1 = iBegDate - 1.
   vCIDate2 = iBegDate - 1.
i = 1.
   /* 1. Определяем список счетов для начисления процентов */
   RUN get_acct in h_dpspc (iLoanRI, iBegDate, iEndDate, OUTPUT vAcctList).
   IF vAcctList = "" THEN RETURN.

   /* 2. Обработка списка счетов
         Список счетов - это список типа Счет1,Дата1,Счет2,Дата2,...СчетN,ДатаN,
         которые означают "Счет1 действует начиная с Даты1,
                           Счет2 действует начиная с Даты2,..."
         при этом, действие Счета1 заканчивается в Дата2
         (то есть, пары (Счет,Дата) отсортированы по датам в возрастающем порядке) */
   DO vi = 1 TO NUM-ENTRIES(vAcctList) BY 2:
      /* 2.1 Поиск счета */
      FIND FIRST acct WHERE acct.acct = ENTRY(vi,vAcctList) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE acct THEN NEXT.
   
      /* 2.2 Ищем интервал начисления для этого счета
             2.2.1 Начало интервала (т.к. начало интервала все время  сдвигается в большую 
                   сторону - начало очередного интервала запомним в vBegDate) */
      vTmpDate = DATE(ENTRY(vi + 1,vAcctList)).
      IF vi > 1 AND iBegDate < vTmpDate THEN iBegDate = vTmpDate.
      /*     2.2.2 Конец интервала (по умолчанию это конец всего интервала начисления,
                   но если счет, который мы обрабатываем заканчивает действие раньше -
                   то интервал для этого счета тоже закончится раньше) */
      vEndDateA = iEndDate.
      IF NUM-ENTRIES(vAcctList) >= vi + 3 THEN DO:
         vTmpDate = DATE(ENTRY(vi + 3,vAcctList)).
         IF vTmpDate < vEndDateA THEN vEndDateA = vTmpDate.
      END.

      /* 2.2A Если нужно - корректируем дату по настройке (Доработки по заявке 0009712) */
      IF iFlEnd THEN DO:
        RUN chkspprc.p (iEndDate, iLoanRI, OUTPUT vFlLog).
        IF vFlLog = ? THEN vFlLog = NO.
        IF vFlLog     THEN vEndDateA = iEndDate.
      END.
      
      /* 2.3 Интервал для счета нашли - (vBegDate,vEndDateA).
             Посмотрим, какая ставка действует в нашем интервале */
      IF iFlDate THEN ASSIGN vCIDate1 = iBegDate vCIDate2 = vEndDateA.
      loanend = loan.end-date.
      IF loanend <> ? AND loanend LT vCIDate2 THEN DO:
          IF NOT iBaseComm  THEN RUN Get_Last_Pen-Commi in h_dpspc(iLoanRI, loanend, loanend, OUTPUT vComm).
          ELSE                   RUN Get_Last_Commi     in h_dpspc(iLoanRI, loanend, loanend, OUTPUT vComm).
          IF NOT iBaseInter THEN RUN Get_Last_Pen_Inter in h_dpspc(iLoanRI, loanend, loanend, OUTPUT vInter).
          ELSE                   RUN Get_Last_Inter     in h_dpspc(iLoanRI, loanend, loanend, OUTPUT vInter).
      END.
      ELSE DO:
          IF NOT iBaseComm  THEN RUN Get_Last_Pen-Commi in h_dpspc(iLoanRI, vCIDate1, vCIDate2, OUTPUT vComm).
          ELSE                   RUN Get_Last_Commi     in h_dpspc(iLoanRI, vCIDate1, vCIDate2, OUTPUT vComm).
          IF NOT iBaseInter THEN RUN Get_Last_Pen_Inter in h_dpspc(iLoanRI, vCIDate1, vCIDate2, OUTPUT vInter).
          ELSE                   RUN Get_Last_Inter     in h_dpspc(iLoanRI, vCIDate1, vCIDate2, OUTPUT vInter).
      END.

      IF iBaseComm = ? OR iBaseInter = ? THEN 
         RUN Get_PenOpkind_Inter_Commi 
          in h_dpspc               (iLoanRI,INPUT-OUTPUT vComm,INPUT-OUTPUT vInter).

      IF vComm = ? OR vComm = '?' OR vInter = ? OR vInter = '?' THEN NEXT.

      /* 2.4 Начисляем налоги на счет acct за интервал (vBegDate,vEndDateA)
             
             Этот интервал ТОЖЕ разбивается на несколько:
             либо в зависимости от изменений ставки рефинансирования (здесь, чуть ниже),
             либо в зависимости от изменений основной ставки (в схеме начисления, т.е. в nachkin'е)
             
             Поэтому, в каждой итерации будем вычислять начало и конец подинтервала.
             Переменные такие:
             Дата начала подинтервала - vBegDate
             Дата конца  подинтервала - vSubEndDate
             Копия vBegDate (когда vBegDate еще пригодится, а передать 
                             OUTPUT-параметр надо) - vTmpDate
             Для правильного разбиения по изменениям ставки рефинансирования
             также зарезервируем переменную vBegDateA */


      vBegDateA1 =     iBegDate.
      {findsch.i &dir=last &sch=vInter 
                 &since1=" < vBegDateA1"}
      IF AVAILABLE interest-sch-line THEN 
      DO:
         RUN SetSysConf IN h_Base ("base-nalog", "yes").
         /* Запускаем Начкин для определения дат изменения ставки по вкладу */
         RUN nachkin.p(RECID(interest-sch-line), vComm, RECID(acct),
                       vEndDateA, iKau, yes, OUTPUT vRes[2], OUTPUT vRes1[2],
                       INPUT-OUTPUT vBegDateA1, OUTPUT oFler).
         RUN DeleteOldDataProtocol IN h_base ("base-nalog").
      END.


      vBegDateA = iBegDate - 1. /* это какая-то фигня для правильного пересчета %ЦБРеф */
      DO WHILE iBegDate < vEndDateA:
         /* 2.4.1 Дла вызова Nachkin'а не хватает буфера схемы начисления.
                  Ищем буфер (заодно корректируем дату конца подинтервала) */
         {findsch.i &dir=FIRST &sch=vInter 
                    &since1=" > iBegDate AND interest-sch-line.since <= vEndDateA"}


         IF AVAILABLE interest-sch-line THEN vSubEndDate = interest-sch-line.since.
         ELSE                                vSubEndDate = vEndDateA.

         /* Поиск первой даты изменения ставки %Реф и %ЦбРеф и выбор той, что была раньше */

         RUN Get-First-Comm-Rate in h_dpspc 
                  (RECID(acct), 
                   vEndDateA, 
                   INPUT-OUTPUT vBegDateA, 
                   INPUT-OUTPUT vSubEndDate).

         /* Сравниваем даты измененя ставки по вкладу и ставки ЦБРеф */
         FIND LAST Nachkin-tt WHERE Nachkin-tt.EndDate < vSubEndDate AND Nachkin-tt.EndDate > iBegDate USE-INDEX edate NO-ERROR.
         IF AVAIL Nachkin-tt THEN    
            ASSIGN
               vBegDateA   = Nachkin-tt.ENdDate
               vSubEndDate = Nachkin-tt.EndDate. 


         {findsch.i &dir=LAST &sch=vInter 
                    &since1 =" < vSubEndDate"}
  
         /*  2.4.2 Расчет сумм процентов по '%ЦбРеф' и ставке вклада
                   для получения налогооблагаемой суммы (ну, наконец-то!) */

         IF AVAILABLE interest-sch-line THEN 
         DO:
            /* Для первого вызова nachkin'а пользуемся копией vBegDate */
            vTmpDate = iBegDate.
            IF NOT vReserv THEN DO:

               RUN nachkin.p(RECID(interest-sch-line), fGetSetting("ЦбРефПред", ?, "%ЦбРеф"), RECID(acct),
                             vSubEndDate, iKau, iFlPrint, OUTPUT vRes[1], OUTPUT vRes1[1],
                             INPUT-OUTPUT vTmpDate, OUTPUT oFler).
               RUN nachkin.p(RECID(interest-sch-line), vComm, RECID(acct),
                             vSubEndDate, iKau, iFlPrint, OUTPUT vRes[2], OUTPUT vRes1[2],
                             INPUT-OUTPUT iBegDate, OUTPUT oFler).
            END.           
            ELSE DO:
               RUN nachkin IN h_nachd (RECID(interest-sch-line), fGetSetting("ЦбРефПред", ?, "%ЦбРеф"), RECID(acct),
                                       vSubEndDate, iKau, iFlPrint, OUTPUT vRes[1], OUTPUT vRes1[1],
                                       INPUT-OUTPUT vTmpDate, OUTPUT oFler).
               RUN nachkin IN h_nachd (RECID(interest-sch-line), vComm, RECID(acct),
                                       vSubEndDate, iKau, iFlPrint, OUTPUT vRes[2], OUTPUT vRes1[2],
                                       INPUT-OUTPUT iBegDate, OUTPUT oFler).
            END.

            /* Либо результат положительный, либо vNeg РАЗРЕШАЕТ отрицательный результат */
            IF vRes[2] > vRes[1] OR iNeg THEN vRes[1] = vRes[2] - vRes[1].
            ELSE                              vRes[1] = 0.
         END.


         /* 2.4.3 Проверка на корректность результата */
         IF oFler <> 0 THEN DO:
           oResult = 0.
           RETURN.
         END.
         
         /* 2.4.4 Готовимся к следущей итерации... */
         iBegDate = vSubEndDate.
         oResult  = oResult + vRes[1].
      END. /* END OF 2.4 */
   END. /* END OF 2. */
   RETURN.
END PROCEDURE. /* END OF PROCEDURE Calc_TaxBase_Any */

/******************************************************************************/
/* Функции, необходимые для работы НЧПрогресс и НачислПр */
/******************************************************************************/

/*Случай, когда резервирование прошло так:
  реальная дата операции = дата окончания вклада
  плановая дата операции = дата начала нового срока.
  Тогда такое резервирование должно попасть в результат НчПрогресс, 
  но не попадет потому что фактически проводка осталась в прошлой жизни вклада*/

FUNCTION necessary_credit RETURN DECIMAL (in-acct AS CHAR,
                                          beg-date AS DATE,
                                          in-kau AS CHAR,
                                          in-cur AS CHAR):
  DEF VAR tmp_dec AS DECIMAL.
  FOR EACH kau-entry  WHERE  kau-entry.acct      =     in-Acct
                          AND kau-entry.currency =     in-Cur
                          AND kau-entry.op-status >=   gop-status
                          AND NOT kau-entry.debit
                          AND kau-entry.op-date  >=     Beg-Date - 10  /*так же как и пересчет по плановым датам*/
                          AND kau-entry.op-date  <      Beg-Date
                          AND kau-entry.contract-date >= Beg-Date
                          AND kau-entry.kau      = in-kau NO-LOCK:
        /*MESSAGE op.op amt-rub op.details VIEW-AS ALERT-BOX.*/
        tmp_dec = tmp_dec + IF in-cur = "" THEN  amt-rub ELSE amt-cur.
  END.
  RETURN tmp_dec.
END.

/*Возвращает "лишние" кредитовые обороты по субаналитическому счету,
 если плановая дата документа не совпадает с датой операции и не попадает в рассматриваемый период жизни вклада
 (чтобы обороты, накрученные на счете при пролонгации не выползли при расчете %% при закрытии вклада ) */
FUNCTION unneces_credit RETURN DECIMAL (in-acct AS CHAR,
                                     beg-date AS DATE,
                                     end-date AS DATE,
                                     in-kau AS CHAR,
                                     in-cur AS CHAR):
  DEF VAR tmp_dec AS DECIMAL.
  FOR EACH kau-entry  WHERE  kau-entry.acct      =     in-Acct
                          AND kau-entry.currency =     in-Cur
                          AND kau-entry.op-status >=   gop-status
                          AND NOT kau-entry.debit
                          AND kau-entry.op-date  >=     Beg-Date
                          AND kau-entry.op-date  <=     End-Date
                          AND kau-entry.kau      = in-kau NO-LOCK:
      /* MESSAGE  op.details op.contract-date op.doc-date VIEW-AS ALERT-BOX.*/
       IF kau-entry.contract-date < beg-date OR kau-entry.contract-date > end-date THEN DO:
        /*MESSAGE op.op amt-rub op.details VIEW-AS ALERT-BOX.*/
        tmp_dec = tmp_dec + IF in-cur = "" THEN  amt-rub ELSE amt-cur.
       END.
  END.
  RETURN tmp_dec.
END.

/*вычисляет сумму проводки при смене счета*/
FUNCTION unneces_turnover RETURN DECIMAL (
   iAcct_db  AS CHAR, /* Счет дебета */
   iAcct_cr  AS CHAR, /* Счет кредита */
   iBegDate  AS DATE, /* Начало периода расчета */
   iEndDate  AS DATE, /* Конец периода расчета */
   iKau      AS CHAR, /* КАУ */
   iCurrency AS CHAR  /* Валюта */
):

   DEF VAR vTmpDec AS DECIMAL INIT 0 NO-UNDO.

   FIND FIRST op-entry WHERE op-entry.acct-db   EQ iAcct_db 
                         AND op-entry.acct-cr   EQ iAcct_cr 
                         AND op-entry.op-date   GE iBegDate 
                         AND op-entry.op-date   LE iEndDate 
                         AND op-entry.currency  EQ iCurrency   
                         AND CAN-FIND(FIRST kau-entry OF op-entry WHERE kau-entry.kau     EQ iKau 
                                                                    AND kau-entry.debit   EQ NO)
   NO-LOCK NO-ERROR.
   FIND FIRST kau-entry OF op-entry WHERE kau-entry.kau     EQ iKau 
                                      AND kau-entry.debit   EQ NO
   NO-LOCK NO-ERROR.

   IF AVAIL kau-entry THEN 
      vTmpDec = IF iCurrency EQ "" THEN kau-entry.amt-rub 
                                   ELSE kau-entry.amt-cur.
   RETURN vTmpDec.
END FUNCTION.

/*  Что делает: Расчет суммы налогов по проводкам за период со дня виртуального
**              открытия вклада до даты последней капитализации процентов.
**              Отличия от НалПоНч: учитывает пролонгацию; капитализация
**              определяется независимо от наличия у договора счета 47411
**   Замечание: учитывает и капитализацию в "своей" тр-ции, поэтому в тр-ции
**              возврат %% должен стоять до капитализации*/
FUNCTION GetReturnTax RETURN DECIMAL (
   iRecOp        AS RECID, 
   iBegDate   AS DATE, /* Дата начала периода начисления */
   iEndDate   AS DATE, /* Дата окончания периода начисления */
   iContract  AS CHARACTER,
   iContCode  AS CHARACTER
   
):
   
   DEFINE VAR vResult     AS DECIMAL   NO-UNDO.
   DEFINE VAR vEndDate1   AS DATE      NO-UNDO.
   

   DEFINE BUFFER xloan-acct FOR loan-acct.
   DEFINE BUFFER loan       FOR loan. /* Локализация буфера. */

   FIND FIRST loan WHERE loan.contract  EQ iContract
                     AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
   IF AVAIL loan 
   THEN DO:
      FOR EACH loan-acct OF loan  WHERE
               loan-acct.acct-type EQ "loan-nal"
          AND  loan-acct.since     GE loan.open-date  
          AND  loan-acct.since     LE iEndDate
      NO-LOCK:
   
         /* Ищем по индексу acct-type для корректной сортировки по since */
         FIND FIRST xloan-acct WHERE xloan-acct.contract  EQ loan-acct.contract  
                                 AND xloan-acct.cont-code EQ loan-acct.cont-code 
                                 AND xloan-acct.acct-type EQ loan-acct.acct-type 
                                 AND xloan-acct.acct      EQ loan-acct.acct  
                                 AND xloan-acct.since     GT loan-acct.since 
                                 AND xloan-acct.since     LE iEndDate
         NO-LOCK NO-ERROR.
                          
         vEndDate1 =  IF AVAIL xloan-acct THEN xloan-acct.since - 1
                                          ELSE iEndDate.
         FOR EACH op-entry WHERE op-entry.acct-cr  EQ     loan-acct.acct     
                             AND op-entry.op-date  GE     loan-acct.since    
                             AND op-entry.op-date  GT     iBegDate           
                             AND op-entry.op-date  LE     vEndDate1          
                             AND op-entry.currency BEGINS loan-acct.currency 
                             AND op-entry.kau-db   BEGINS loan.contract + "," + loan.cont-code + ","
         NO-LOCK:
         
            FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
            IF AVAILABLE op
                 AND op.contract-date GT iBegDate
                 AND op.contract-date LE iEndDate
                 AND RECID(op)        NE iRecOp 
            THEN DO:                               /* и плановая дата попадает в период */
                  
               vResult = vResult + IF loan-acct.currency EQ "" THEN op-entry.amt-rub
                                                               ELSE op-entry.amt-cur.
            END. 
         END.
      END. 
   END.
   RETURN vResult.
END FUNCTION.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='05/10/2015 17:33:00.035+04:00' */
/* $LINTUSER='sados' */
/* $LINTMODE='1' */
/* $LINTFILE='dpspr_tax.i' */
/*prosigngH8meXg4CyO3u4ewyR56Cw*/