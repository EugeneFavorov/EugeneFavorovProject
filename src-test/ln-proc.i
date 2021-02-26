/* Формирование базы начисления.
   Берем из доп. реквизита договора и если нет, то из начального значения
   реквизита.  значения(коды параметров) хранятся через запятую,допускается
   указание  нескольких параметров через "+"
   Можно указывать несколько групп параметров разделенных "&" при этом рез-тат
   начисления будет сумма начислений по каждой группе параметров по ставке из
   реквизита БазаСтав, где указывается ставка для каждой группы через "&.
   Пока максимально 10 групп.

   БазаНач - 0+7,....,0&7+13
   БазаСтав = %Кред,..,%КрКом&%КрПр
*/

DEF BUFFER loan      FOR loan.
DEF BUFFER curloan   FOR loan.

DEF TEMP-TABLE ttsh NO-UNDO
   FIELD since    AS DATE
   FIELD Summa    AS DEC
INDEX since since.

PROCEDURE GetCalcCharNew.

   DEF INPUT  PARAM iContractChar AS CHAR NO-UNDO. /* Назначенеи договора */
   DEF INPUT  PARAM iContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iCodPar       AS INT64  NO-UNDO. /* Код параметра */
   DEF OUTPUT PARAM oCurrCalcChar AS CHAR NO-UNDO. /* База начисления */
   DEF OUTPUT PARAM oNumGrups     AS INT64  NO-UNDO.

   DEF VAR vCalcString   AS CHAR NO-UNDO. /* строка для определения базы
                                          начисления хранится в доп. реквизите
                                          БазаНач или в начальном значении
                                          для класса в метасхеме */

   vCalcString = GetXattrValueEx("loan",
                                 iContractChar + "," + iContCodeChar,
                                 "БазаНач","").
   IF vCalcString = "" THEN
      vCalcString = GetXattrInit(loan.class-code,"БазаНач").

   vCalcString = ENTRY(iCodPar,vCalcString) NO-ERROR.

   IF NOT ERROR-STATUS:ERROR THEN
      vCalcString = REPLACE(vCalcString,"+",",") NO-ERROR.

   IF NOT ERROR-STATUS:ERROR AND
      vCalcString <> ""      AND
      vCalcString <> ?       THEN
      oCurrCalcChar = vCalcString.
   ELSE
      oCurrCalcChar = IF iCodPar EQ 1 THEN
                         "0,7,13"
                      ELSE
                         STRING(most[iCodPar]).

   oNumGrups = NUM-ENTRIES(oCurrCalcChar,"&").
END PROCEDURE. /*GetCalcChar*/

/* Возвращает код процентной ставки.
** Данная связь, должна прошиваться в настройках БД.
** Для работы данной процедуры необходим инклюдник svarloan.def */
PROCEDURE GetCodeRateNew.

   DEF INPUT  PARAM iPrmInt   AS INT64  NO-UNDO. /* Порядковый номер %%, пени
                                                loan.interest[cod-par] */
   DEF INPUT  PARAM iNumGrups AS INT64  NO-UNDO. /* Число групп в базе начисл */
   DEF OUTPUT PARAM oRateChar AS CHAR NO-UNDO. /* Код тарифа/комиссии */

   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vCommChar    AS CHAR
                          INIT "1,2,3,4,5,2,3,4,5,6,4,5,7,8"
                          NO-UNDO. /* Массив связи процентов и комиссий */
   DEF VAR vRateString  AS CHAR NO-UNDO.
   DEF VAR vCounter     AS INT64  NO-UNDO.

   /*берем строку процентных ставок из доп. реквизита договора
    и если нет, то из начального значения реквизита.*/

   ASSIGN
      vRateString = GetXattrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "БазаСтав","")
      oRateChar = lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1]
      .

   IF vRateString = "" THEN
      vRateString = GetXattrInit(loan.class-code,"БазаСтав").

   /*если строка есть и возможно взять из этой строки элемент
    с номером cod-par, то это и будет процентной ставкой...*/
   IF vRateString <> ?  AND
      vRateString <> "" THEN
   DO:
      oRateChar = ENTRY(iPrmInt,vRateString) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oRateChar = lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1].
   END.

   IF NUM-ENTRIES(oRateChar,"&") < iNumGrups THEN
   DO vCounter = NUM-ENTRIES(oRateChar,"&") + 1 TO iNumGrups:
      oRateChar = oRateChar + "&"
                  + lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1].
   END.

END PROCEDURE.
/* хотелось бы использовать одну компоненту для получения кода коммисси
   по налогу и %5 ставки , но не судьба - явно указанкод параметра*/
PROCEDURE GetCodeTax.

   DEF INPUT  PARAM iPrmInt   AS INT64  NO-UNDO. /* Порядковый номер %%, пени
                                                loan.interest[cod-par] */
   DEF INPUT  PARAM iNumGrups AS INT64  NO-UNDO. /* Число групп в базе начисл */
   DEF OUTPUT PARAM oRateChar AS CHAR NO-UNDO. /* Код тарифа/комиссии */

   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vRateString  AS CHAR NO-UNDO.
   DEF VAR vCounter     AS INT64  NO-UNDO.

   /*берем строку процентных ставок из доп. реквизита договора
    и если нет, то из начального значения реквизита.*/

   ASSIGN
      vRateString = GetXattrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "БазаНалог","")
       .

    IF vRateString = "" THEN
       vRateString = GetXattrInit(loan.class-code,"БазаНалог").

   /*если строка есть и возможно взять из этой строки элемент
    с номером cod-par, то это и будет процентной ставкой...*/
   IF vRateString <> ?  AND
      vRateString <> "" THEN
   DO:
      oRateChar = ENTRY(iPrmInt,vRateString) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oRateChar = ?.
   END.
   ELSE oRateChar = ? . /* Нет никаких налогов */
END PROCEDURE .

/* Получение динамики остатка по параметрам договора.
** Частный случай - получение значения по одному параметру. */
PROCEDURE GET_REM_BY_PRM_NEW.

   DEF INPUT PARAM iContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM iContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM iPrmChar      AS CHAR NO-UNDO. /* Параметры для расчета
                                                                остатка. */
   DEF INPUT PARAM iBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate      AS DATE NO-UNDO. /* "По" окончание интервала*/
   DEF INPUT PARAM iCodPar       AS INT64  NO-UNDO. /* код параметра*/
   DEF INPUT PARAM iCurrency     AS CHAR NO-UNDO. /* Валюта*/
   DEF INPUT PARAM iSince        AS DATE NO-UNDO. /* дата состояния догоовра */
   DEF INPUT PARAM iRecalcLoan   AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP     AS LOG  NO-UNDO.
   DEF INPUT PARAM iForTransh    AS LOG  NO-UNDO.

   DEF VAR vChgDate     AS DATE NO-UNDO. /* Дата измененения парметра */
   DEF VAR vChgDate1    AS DATE NO-UNDO. 
   DEF VAR vChgDate2    AS DATE NO-UNDO. 
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-ый параметр */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* Сумма парметров на дату */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* Сумма i-ого параметра */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* Сумма дебетовых оборотов */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* Сумма кредитовых оборотов */
   DEF VAR vPrmCharBase AS CHAR NO-UNDO. /* Строка параметров "," вместо "&" */
   DEF VAR vPrmChar     AS CHAR NO-UNDO. /* Параметры в текущей группе */
   DEF VAR vGrupCounter AS INT64  NO-UNDO. /* Счетчик групп */
   DEF VAR vOsnDolg     AS LOG  NO-UNDO. /* Обработка символа "П" */
   DEF VAR vCurrentEndDate AS DATE NO-UNDO.
   
   DEF BUFFER xloan FOR loan. /* Локализация буффера */
   
   vPrmCharBase = REPLACE(iPrmChar,"&",",").
   
   FIND FIRST term-obl WHERE 
          term-obl.cont-code EQ iContCodeChar 
      AND term-obl.contract  EQ iContractChar 
      AND term-obl.idnt      EQ 2 
   NO-LOCK NO-ERROR.
  
  /* Обработка по ролям поддерживает только одну роль в списке
     пока не обрабатываем + и & */  
   IF iPrmChar MATCHES "*Роль*" THEN
   DO:
      RUN GET-OST-ACCT (iPrmChar,
                        RECID(curloan),
                        iBegDate,
                        iEndDate,
                        1).
   END.
   ELSE DO :
   /* Зажимаемся в указанный интервал дат */
   DO WHILE iBegDate LE iEndDate:
/* вывести по отдельной настройке, 
   например "Гр" - аналог "П", только по текущ.
   FIND LAST term-obl WHERE 
          term-obl.cont-code EQ iContCodeChar 
      AND term-obl.contract  EQ iContractChar 
      AND term-obl.idnt      EQ 2 
      AND term-obl.end-date  LE iBegDate
   NO-LOCK NO-ERROR.
*/
      /* Получение следующей даты изменения параметра */
      RUN GetChgDateParam (iContractChar,
                        iContCodeChar,
                        vPrmCharBase,      /* Список интересующих параметров */
                        iBegDate - IF vIshOst THEN 0 ELSE 1, /* От какой даты смотреть */
                        NOT iRecalcPP,
                        OUTPUT vChgDate1).
      IF iContractChar EQ "Депоз" 
      THEN
         vChgDate2 = GET_NEXT_COMM_DATE_LOAN (iContractChar ,
                                              iContCodeChar ,
                                              "МаксДеп",
                                              iBegDate,
                                              iEndDate).
      vChgDate = IF vChgDate2 EQ ? 
                 THEN vChgDate1 
                 ELSE IF vChgDate1 EQ ?
                 THEN vChgDate2 
                 ELSE MIN(vChgDate1,vChgDate2).
 
      IF iCodPar = 1                   
         AND LOOKUP(STRING(CodOstPar),vPrmCharBase) <> 0 
         AND NOT iRecalcLoan               
         AND NOT iRecalcPP THEN
            RUN CORR_NEXT_DATE (iBegDate - IF vIshOst THEN 0 ELSE 1 ,
                   INPUT-OUTPUT vChgDate).
      IF vChgDate EQ ? THEN vChgDate = iEndDate + 1.
      /* Корректировка даты текущего интервала */
      vCurrentEndDate = IF vChgDate GT iEndDate THEN
                           iEndDate + 1
                        ELSE
                           vChgDate + IF vIshOst THEN 0 ELSE 1. /* Исх/Вх остаток */
      DO vGrupCounter = 1 TO NUM-ENTRIES (iPrmChar,"&"):
         ASSIGN
            vPrmChar     = ENTRY(vGrupCounter,iPrmChar,"&")
            vTotalPrmDec = 0
         .
         
         DO vCurrPrmInt = 1 TO NUM-ENTRIES (vPrmChar):
            IF ENTRY(vCurrPrmInt, vPrmChar) MATCHES "*П*"  THEN
               ASSIGN
                  vPrmChar = REPLACE(vPrmChar,"П","")
                  vOsnDolg = YES
               .
  
            IF INT64 (ENTRY(vCurrPrmInt, vPrmChar)) = CodOstPar 
                   AND iCodPar = 1                                    
                   AND NOT iRecalcLoan                                
                   AND NOT iRecalcPP THEN
            DO:
               IF INT64(ENTRY(vCurrPrmInt, vPrmChar)) = 0  THEN
               DO:
                  RUN PARAM_0_NEW IN h_Loan
                         (iContractChar,
                         iContCodeChar,    /* Номер договора */
                         INT64 (ENTRY(vCurrPrmInt, vPrmChar)), /* Код параметра */
                         iBegDate - IF vIshOst THEN 0 ELSE 1,  /* Дата расчета параметра */
                         OUTPUT vPrmValDec).     /* Значение параметра на дату пересчета договора */
                               
               END.
               ELSE
               DO:
                  RUN RE_PARAM IN h_Loan
                                 (INT64 (ENTRY(vCurrPrmInt, vPrmChar)),
                                 iBegDate - IF vIshOst THEN 0 ELSE 1, /* Вх/Исх остаток */
                                 iContractChar,
                                 iContCodeChar,
                                 OUTPUT vPrmValDec,
                                 OUTPUT vDbDec,
                                 OUTPUT vCrDec).
             
               END.

               FIND LAST ttCorr WHERE 
                           ttCorr.end-date <= iBegDate - 1 
               NO-LOCK NO-ERROR.
               vPrmValDec = IF AVAIL ttCorr THEN 
                               vPrmValDec - ttCorr.Corr 
                            ELSE 
                               vPrmValDec.
            END.
            ELSE
            DO: 
               IF INT64(ENTRY(vCurrPrmInt, vPrmChar)) = CodOstPar 
                       AND iCodPar = 1                           
                       AND iRecalcPP THEN
                  RUN GetFilOstSumm(iBegDate - IF vIshOst THEN 0 ELSE 1,
                                    CodOstPar,
                             OUTPUT vPrmValDec).
               ELSE
               DO:
                  IF NOT vOsnDolg THEN
                     RUN STNDRT_PARAM IN h_Loan
                                     (iContractChar,
                                      iContCodeChar,
                                      INT64 (ENTRY(vCurrPrmInt, vPrmChar)),
                                      iBegDate - IF vIshOst THEN 0 ELSE 1, /* Вх/исх остаток */
                                      OUTPUT vPrmValDec,
                                      OUTPUT vDbDec,
                                      OUTPUT vCrDec).
                  ELSE
                  DO:
                     vTotalPrmDec = IF AVAIL term-obl THEN 
                                       term-obl.amt-rub 
                                     ELSE 0.
                  END.
               END.
            END.
            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
            vOsnDolg = NO.
         END.

         IF iContractChar EQ "Депоз" THEN
         DO:
            RUN RE_B_LOAN IN h_Loan (iContractChar,
                                     iContCodeChar,
                                     BUFFER xloan).

            RUN base-sum-correct (iContractChar,
                                  iContCodeChar,
                                  xloan.currency,
                                  iBegDate,
                                  ENTRY(vGrupCounter,GetXAttrInit(xloan.class-code,"БазаСтав"),"&"),
                                  INPUT-OUTPUT vTotalPrmDec).
         END.
         /* Корректировка остатка */ 
         IF iForTransh THEN
            RUN SetTranshSum (iContCodeChar,iBegDate,vTotalPrmDec,vGrupCounter).
         ELSE
            RUN SetLnShPrm(iBegDate,?,?,iCurrency,?,vTotalPrmDec,?,?,?,?,vGrupCounter,?,?).
      END. 
      iBegDate = vCurrentEndDate.
   END. /* даты      */
   END. /* параметры */
   RETURN.
END PROCEDURE.

/* суммируем фактические остатки по траншам */
PROCEDURE GET_REM_BY_PRM_TR.
   DEF INPUT PARAM iContractChar AS CHAR NO-UNDO. /* Назначение охв. договора */
   DEF INPUT PARAM iContCodeChar AS CHAR NO-UNDO. /* Номер охв. договора */
   DEF INPUT PARAM iPrmChar      AS CHAR NO-UNDO. /* Параметры для расчета
                                                                остатка. */
   DEF INPUT PARAM iBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate      AS DATE NO-UNDO. /* "По" окончание интервала*/
   DEF INPUT PARAM iCodPar       AS INT64  NO-UNDO. /* код параметра*/
   DEF INPUT PARAM iCurrency     AS CHAR NO-UNDO. /* Валюта*/
   DEF INPUT PARAM iSince        AS DATE NO-UNDO. /* дата состояния догоовра */
   DEF INPUT PARAM iRecalcLoan   AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP     AS LOG  NO-UNDO.
   
   FOR EACH curloan WHERE curloan.contract  EQ     iContractChar
                      AND curloan.cont-code BEGINS iContCodeChar + " " 
                      AND NUM-ENTRIES(curloan.cont-code, " ") EQ 2 
                      AND curloan.open-date LE iEndDate
                      AND (   curloan.close-date EQ ?
                           OR curloan.close-date GE iBegDate)
       NO-LOCK:

      RUN GET_REM_BY_PRM_NEW (curloan.contract,
                              curloan.cont-code,
                              iPrmChar,  /* База начисления */
                              iBegDate,
                              iEndDate,
                              iCodPar,
                              iCurrency,
                              iSince,
                              iRecalcLoan,
                              iRecalcPP,
                              YES).
   END.
   RETURN.
END PROCEDURE.

/* суммируем плановые остатки по траншам */
PROCEDURE GET_REM_BY_TERM_TR.
   DEF INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* Назначение охв. договора */
   DEF INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* Номер охв. договора */
   DEF INPUT PARAM iCodGraf      AS INT64 NO-UNDO. /* Код. Графика */
   DEF INPUT PARAM iBegDate      AS DATE  NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate      AS DATE  NO-UNDO. /* "По" окончание интервала*/
   DEF INPUT PARAM iCurrency     AS CHAR  NO-UNDO. /* Валюта*/
   
   FOR EACH curloan WHERE curloan.contract  EQ     iContractChar
                      AND curloan.cont-code BEGINS iContCodeChar + " " 
                      AND NUM-ENTRIES(curloan.cont-code, " ") EQ 2 
                      AND curloan.open-date LE iEndDate
                      AND (   curloan.close-date EQ ?
                           OR curloan.close-date GE iBegDate)
       NO-LOCK:
         RUN GET_REM_BY_TERM (curloan.contract,
                              curloan.cont-code,
                              iCodGraf,    /* Код графика "плановых сумм" */
                              iBegDate,
                              iEndDate,
                              iCurrency,
                              YES).

   END.
   RETURN.
END PROCEDURE.

/* Формирование комиссии по остатку */
PROCEDURE GET_COMM_BY_REM_NEW.

   DEF INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* Номер договора */
   DEF INPUT PARAM iCommChar AS CHAR NO-UNDO. /* Код комиссии */
   DEF INPUT PARAM iKauChar  AS CHAR NO-UNDO. /* Код КАУ */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* "По" окончание интервала */
   DEF INPUT PARAM iCodPar   AS INT64  NO-UNDO. /* код параметра*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* Идентификатор счета */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* Текущ. знач %% ставки/тарифа */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* Ставка/тариф */
   DEF VAR vGrupCounter  AS INT64   NO-UNDO. /* Счетчик групп*/
   DEF VAR vCommChar     AS CHAR  NO-UNDO. /* Комиссия в рамках группы*/
   DEF VAR vBegDate      AS DATE  NO-UNDO. /* Переменные для определения мин*/
   DEF VAR vBegDateT     AS DATE  NO-UNDO. /* даты изменения комиссии во всей
                                              группе */
   DEF VAR vMinRate      AS DEC   NO-UNDO. /* минимальное значение */
   DEF VAR vMaxRate      AS DEC   NO-UNDO. /* максимальное значение */

   DEF VAR vCommCur      AS CHAR  NO-UNDO. /* валюта */

   DEFINE BUFFER bLnShPrm FOR LnShPrm.
   DEFINE BUFFER b-comm-rate FOR comm-rate.
   DEFINE BUFFER bc-rate     FOR comm-rate.

   /* Формирование комиссии в интервале дат */
   DO WHILE iBegDate <= iEndDate:

      /* Поиск параметров на дату */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* Если параметры определены,
      ** то попытка поиска значения комиссии/тарифа */
      IF AVAIL LnShPrm THEN
      DO:

         ASSIGN
            vAcctRecid = ?
            vBegDateT  = iEndDate  + 1
            vCommCur   = LnShPrm.Currency
            .

         DO vGrupCounter = 1 TO NUM-ENTRIES(iCommChar,"&"):

            ASSIGN
               vCommChar = ENTRY(vGrupCounter,iCommChar,"&").
            FIND LAST bc-rate WHERE bc-rate.kau        EQ iKauChar
                                AND bc-rate.commission EQ vCommChar
                                AND bc-rate.since      LE iBegDate
               NO-LOCK NO-ERROR.
               /* Получение комиссии/тарифа на дату */
               vRateDec  = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                     ELSE 0.
               /* Получение типа: "%,=,пеня" */
               vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", iCommChar)
                               THEN
                                  {&GCodePeny}
                               ELSE
                               IF GET_COMM_TYPE (
                                   vCommChar,  /*Код комиссии */
                                   vAcctRecid, /*Идентификатор счета */
                                   vCommCur,   /*Код приведенной валюты */
                                   iKauChar,   /*Код КАУ ("" - по умолчанию) */
                                   LnShPrm.balance[vGrupCounter], /*Минимальный
                                                   остаток (0 - поумолчанию) */
                                   0,          /*Период/срок (0-поумолчанию)*/
                                   iBegDate) THEN
                                  "="
                               ELSE
                                  "%"
               .


            /* буфер для заполнения мин и макс знач. */
            RUN GET_COMM_LOAN_BUF IN h_Loan (
                                   iContractChar,       /* Назначение договора */
                                   iContCodeChar,       /* Номер договора */
                                   iCommChar,           /* Код комиссии */
                                   iBegDate,            /* На дату */
                                   BUFFER b-comm-rate). /* Буфер */


            /* минимальное значение */
            vMinRate = DEC (GetXAttrValueEx("comm-rate",
                                  GetSurrogateBuffer("comm-rate",
                                                     BUFFER b-comm-rate:HANDLE
                                                     ),
                                  "МинЗнач",
                                  "0"
                                  )
                               ) NO-ERROR.
            /* максимальное значение */
            vMaxRate = DEC (GetXAttrValueEx("comm-rate",
                                  GetSurrogateBuffer("comm-rate",
                                                     BUFFER b-comm-rate:HANDLE
                                                     ),
                                  "МаксЗнач",
                                  "0"
                                  )
                               ) NO-ERROR.
          /* Уточним, не я вляется ли пени фиксированным */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
               vCommChar,  /*Код комиссии */
               vAcctRecid, /*Идентификатор счета */
               vCommCur,   /*Код приведенной валюты */
               iKauChar,   /*Код КАУ ("" - по умолчанию) */
               LnShPrm.balance[vGrupCounter], /*Минимальный
                              остаток (0 - поумолчанию) */
               0,             /*Период/срок (0-поумолчанию)*/
               iBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.
            /* Корректировка/создание %% ставки */
            RUN SetLnShPrm (iBegDate,?,?,?,?,?,?,vRateDec,
                            vCommTypeChar,?,vGrupCounter, vMinRate , vMaxRate ).

            /* Узнаем дату изменения СТАВКИ / ТАРИФА */
            vBegDate = GET_NEXT_COMM_DATE (
                         vCommChar,       /* Код комиссии */
                         vAcctRecid,      /* Идентификатор счета */
                         vCommCur,        /* Код приведенной валюты */
                         iKauChar,        /* Код КАУ ("" - по умолчанию) */
                         LnShPrm.balance[vGrupCounter], /* Минимальный остаток
                                                          (0 - поумолчанию) */
                         0,               /* Период/срок (0 - поумолчанию) */
                         iBegDate,
                         iEndDate).

            IF vBegDate < vBegDateT AND vBegDate <> ? THEN vBegDateT = vBegDate.

         END.
         iBegDate = vBegDateT.
      END.

      /* Если параметров для поиска комиссий не найдено,
      ** то ищем их впереди */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since > iBegDate
         NO-ERROR.

         /* А если их нет,
         ** то и не надо искать комиссию/тариф */
         iBegDate = IF AVAIL LnShPrm THEN
                       LnShPrm.since
                    ELSE
                       iEndDate + 1.
      END.
   END.
   RETURN.

END PROCEDURE.

/* Формирование налога по остатку */
PROCEDURE GET_COMM_BY_TAX.

   DEF INPUT PARAM iCommChar AS CHAR NO-UNDO. /* Код комиссии */
   DEF INPUT PARAM iKauChar  AS CHAR NO-UNDO. /* Код КАУ */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* "По" окончание интервала */
   DEF INPUT PARAM iCodPar   AS INT64  NO-UNDO. /* код параметра*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* Идентификатор счета */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* Текущ. знач %% ставки/тарифа */
   DEF VAR vGrupCounter  AS INT64   NO-UNDO. /* Счетчик групп*/
   DEF VAR vCommChar     AS CHAR  NO-UNDO. /* Комиссия в рамках группы*/
   DEF VAR vBegDate      AS DATE  NO-UNDO. /* Переменные для определения мин*/
   DEF VAR vBegDateT     AS DATE  NO-UNDO. /* даты изменения комиссии во всей
                                              группе */
   DEF VAR vCommCur      AS CHAR  NO-UNDO. /* валюта */

   DEFINE BUFFER bLnShPrm FOR LnShPrm.
   DEFINE BUFFER comm-rate FOR comm-rate .

   /* Формирование комиссии в интервале дат */
   DO WHILE iBegDate <= iEndDate:

      /* Поиск параметров на дату */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* Если параметры определены,
      ** то попытка поиска значения комиссии/тарифа */
      IF AVAIL LnShPrm THEN
      DO:

         ASSIGN
            vAcctRecid = ?
            vBegDateT  = iEndDate  + 1
            vCommCur   = LnShPrm.Currency
            .

         DO vGrupCounter = 1 TO NUM-ENTRIES(iCommChar,"&"):

            ASSIGN
               vCommChar = ENTRY(vGrupCounter,iCommChar,"&") .
               /* Получение налога на дату . ищем индивидуальную ставку
                  налога для договора - если нет - общую. 
               */
               RUN  GET_COMM_BUF (
                            vCommChar,      /* Код комиссии */
                            vAcctRecid,     /* Идентификатор счета */
                            vCommCur,       /* Код приведенной валюты */
                            iKauChar,       /* Код КАУ ("" - по умолчанию) */
                            LnShPrm.balance[vGrupCounter], /*Миним. остаток
                                                           (0 - поумолчанию) */
                            0,              /* Период/срок (0 - поумолчанию) */
                            iBegDate,
                            BUFFER comm-rate
                       )
               .
              /* если найдена индивидуальная ставка - то договор всегда
         должен жить с индивидуальной - нет труда при необходимости ввести
         такую же. как стандартная */
            IF NOT AVAIL comm-rate
             THEN  DO :
               RUN  GET_COMM_BUF (
                            vCommChar,      /* Код комиссии */
                            vAcctRecid,     /* Идентификатор счета */
                            vCommCur,       /* Код приведенной валюты */
                            "",       /* Код КАУ ("" - по умолчанию) */
                            LnShPrm.balance[vGrupCounter], /*Миним. остаток
                                                           (0 - поумолчанию) */
                            0,              /* Период/срок (0 - поумолчанию) */
                            iBegDate,
                            BUFFER comm-rate
                       ).
                   IF AVAIL comm-rate
                   THEN  /*  договор обслуживается по стандартному налогу */
                   ASSIGN
                      vRateDec = comm-rate.rate-comm
                      iKauChar = "".
                   ELSE  vRateDec = 0.0 .
            END.           
            ELSE
             vRateDec = comm-rate.rate-comm .
            /* Корректировка/создание %% ставки */
            RUN SetLnTax (iBegDate,vRateDec,vGrupCounter).

            /* Узнаем дату изменения СТАВКИ / ТАРИФА */
            vBegDate = GET_NEXT_COMM_DATE (
                         vCommChar,       /* Код комиссии */
                         vAcctRecid,      /* Идентификатор счета */
                         vCommCur,        /* Код приведенной валюты */
                         iKauChar,        /* Код КАУ ("" - по умолчанию) */
                         LnShPrm.balance[vGrupCounter], /* Минимальный остаток
                                                          (0 - поумолчанию) */
                         0,               /* Период/срок (0 - поумолчанию) */
                         iBegDate,
                         iEndDate).

            IF vBegDate < vBegDateT AND vBegDate <> ? THEN vBegDateT = vBegDate.

         END.
         iBegDate = vBegDateT.
      END.

      /* Если параметров для поиска комиссий не найдено,
      ** то ищем их впереди */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since > iBegDate
         NO-ERROR.

         /* А если их нет,
         ** то и не надо искать комиссию/тариф */
         iBegDate = IF AVAIL LnShPrm THEN
                       LnShPrm.since
                    ELSE
                       iEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* Начисление процентов и формироание отчета */
PROCEDURE GET_NAC_REP_NEW.

   DEF INPUT PARAM iSchmRecid AS RECID NO-UNDO. /* Код схемы начисления */
   DEF INPUT PARAM iContCode  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iContract  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iBegDate   AS DATE  NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM iEndDate   AS DATE  NO-UNDO. /* "По" окончание интервала */

   DEF VAR vEndDate   AS DATE NO-UNDO. /* Окончание периода начисления прцентов*/
   DEF VAR vDaysInt   AS INT64  NO-UNDO. /* Количество дней в периоде */
   DEF VAR vCounter   AS INT64  NO-UNDO. /* Счетчик групп*/
   DEF VAR vTermCheck AS LOG  NO-UNDO.
   DEF VAR vNextDate1 AS DATE NO-UNDO.
   DEF VAR vNextDate2 AS DATE NO-UNDO.
   DEF VAR vIsEqRat   AS LOG  NO-UNDO.
   DEF VAR vtax       AS DECIMAL INIT 1 NO-UNDO .
   DEF VAR mFormRasch AS CHAR NO-UNDO. /* Формула расчета процентов с ДР ВыбФормРасчПроц */

   DEF BUFFER buf-lnFost        FOR LnShPrm.
   DEF BUFFER interest-sch-line FOR interest-sch-line.

   /* Поиск комиссии */
   RUN get_sch_line_by_rid IN h_schem
       (iSchmRecid,
        BUFFER interest-sch-line).

   /* Поиск параметров начисления на дату */
   FIND LAST LnShPrm WHERE
             LnShPrm.since <= iBegDate
   NO-ERROR.

   /* Поиск параметров начисления вперед */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since > iBegDate
   NO-ERROR.

   /* Определение даты начала начисления,
   ** исходя из наличия параметров начисления */
   iBegDate = IF AVAIL LnShPrm THEN
                 iBegDate
              ELSE
              IF AVAIL buf-lnFost THEN
                 buf-lnFost.since
              ELSE
                 iEndDate.

   FIND FIRST loan WHERE 
              loan.cont-code EQ iContCode 
          AND loan.contract  EQ iContract 
   NO-LOCK NO-ERROR.
   FIND FIRST loan-cond WHERE 
              loan-cond.cont-code EQ iContCode 
          AND loan-cond.contract  EQ iContract 
   NO-LOCK NO-ERROR.

   /* Получаем формулу расчета процентов с договора или с класса */
   mFormRasch = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ВыбФормРасчПроц",GetXAttrInit(loan.class-code,"ВыбФормРасчПроц")).
   IF mFormRasch EQ ? 
      THEN mFormRasch = fGetSetting("ВыбФормРасчПроц","","1").
   /* Зажимаемся в установленный (скорректированный)
   ** интервал дат */
   DO WHILE iBegDate <= iEndDate:
      /* Находим параметры расчета */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* Поиск следующих параметров */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since > iBegDate
      NO-ERROR.

      /* Получение даты окончания текущего периода */
      vEndDate = IF AVAIL buf-lnFost THEN
                    buf-lnFost.since - 1
                 ELSE
                    iEndDate.

      /* Определение кол-ва дней в интервале */
      vDaysInt = cDay(interest-sch-line.interest-month,
                      iBegDate,
                      vEndDate + 1).

     

      FIND FIRST term-obl WHERE term-obl.contract  EQ iContract
                            AND term-obl.cont-code EQ iContCode
                            AND term-obl.idnt EQ 1
                            AND term-obl.end-date GE iBegDate
                            AND term-obl.end-date LE vEndDate
      NO-LOCK NO-ERROR.

      vNextDate1 = FRST_DATE(iBegDate,
                    vEndDate,
                    loan-cond.int-date,
                    loan-cond.int-period,
                    loan-cond.since).
      vNextDate2 = FRST_DATE(iBegDate,
                    vEndDate + 1,
                    loan-cond.int-date,
                    loan-cond.int-period,
                    loan-cond.since).

      IF AVAIL term-obl THEN 
      DO:
         IF term-obl.end-date EQ loan.end-date THEN 
         DO:
            IF vNextDate1 EQ vNextDate2 AND vEndDate <= loan.end-date THEN 
            ASSIGN
               vTermCheck = YES.
         END.
         ELSE
            ASSIGN
               vTermCheck = YES.
      END.

      /* otch1 - расшарена. В ней и создаем отчет */
      IF AVAIL LnShPrm THEN
      DO vCounter = 1 TO EXTENT(LnShPrm.balance):
         
         IF LnShPrm.balance[vCounter] = ? OR LnShPrm.rate[vCounter] = ? THEN 
            NEXT.
         vIsEqRat = IF LnShPrm.CommType[vCounter] EQ "=" THEN 
                       YES 
                    ELSE NO.
        /* определяем поправлчный коэффициент, если надо учитывать НДС */
        IF  LnShPrm.tax[vCounter] <> ?
        THEN ASSIGN  vTax = 100 / (100 +  LnShPrm.tax[vCounter]).
      	ELSE VTAX = 1.  
        CREATE otch1.
        IF  LnShPrm.tax[vCounter] <> ?
        THEN ASSIGN  vTax = 100 / (100 +  LnShPrm.tax[vCounter])
                     otch1.summ_opl =  LnShPrm.tax[vCounter].
    
         ASSIGN
            otch1.bal-summ = IF vIsEqRat EQ NO THEN LnShPrm.balance[vCounter] ELSE ?
            otch1.beg-date = iBegDate
            otch1.end-date = vEndDate
            otch1.ndays    = IF vIsEqRat EQ NO THEN vDaysInt ELSE ?
            otch1.rat1     = IF vIsEqRat EQ NO THEN LnShPrm.rate[vCounter] ELSE ?
         /* Расчет процентной ставки в зависимости от типа */

           otch1.summ_pr = 
            /* Расчет значения по с типом "ставка". В зависимости от формулы */
            IF    LnShPrm.CommType[vCounter] EQ "%" 
              AND mFormRasch EQ "1" THEN
                              /*Дн в пер*//*    Сумма             */  /*      Ставка      */    /*       Дней в году        */
                ROUND ((vTax * vDaysInt * (LnShPrm.balance[vCounter] * LnShPrm.rate[vCounter] / (interest-sch-line.basis-time * 100))), 2)
            ELSE
            IF    LnShPrm.CommType[vCounter] EQ "%" 
              AND mFormRasch EQ "2" THEN
                      /*          Сумма              */   /*      Ставка      */ /*100*/ /*Дн в пер*/ /*       Дней в году       */ 
               ROUND(((vTax * LnShPrm.balance[vCounter] * LnShPrm.rate[vCounter] / 100 ) * vDaysInt / interest-sch-line.basis-time ),2)

            /* Расчет значения с типом "тариф". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ "=" AND vTermCheck THEN
               ROUND(vTax * LnShPrm.rate[vCounter], 2)

            /* Расчет значения с типом "пеня". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ {&GCodePeny} THEN
               ROUND ((vTax * vDaysInt * (LnShPrm.balance[vCounter] *
                                   LnShPrm.rate[vCounter] / 100)), 2)
             /* Расчет значения с типом "пеня = фиксированная". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ {&GCodePenyFix} THEN
                IF LnShPrm.balance[vCounter] > 0
                  THEN
                        ROUND ((vTax * vDaysInt * LnShPrm.rate[vCounter]), 2)
                  ELSE 0

            ELSE
               ?

            otch1.summ_pr  = IF otch1.summ_pr EQ ? THEN
                                0
                             ELSE
                                otch1.summ_pr
         .
         /* проверка на минимальное значение */
         IF     LnShPrm.balance[vCounter] GT 0 
            AND LnShPrm.MinRate[vCounter] NE 0 
            AND LnShPrm.MinRate[vCounter] GT otch1.summ_pr  THEN 
            otch1.summ_pr = LnShPrm.MinRate[vCounter].
         /* проверка на максимальное значение */
         IF     LnShPrm.balance[vCounter] GT 0 
            AND LnShPrm.MaxRate[vCounter] NE 0 
            AND LnShPrm.MaxRate[vCounter] LT otch1.summ_pr THEN 
            otch1.summ_pr = LnShPrm.MaxRate[vCounter].
      END.
      iBegDate = vEndDate + 1.
   END.
   RETURN.
END PROCEDURE.

PROCEDURE GET-OST-ACCT.
   DEF INPUT PARAM iStr         AS CHAR  NO-UNDO.
   DEF INPUT PARAM iRid         AS RECID NO-UNDO .
   DEF INPUT PARAM iBegDate     AS DATE  NO-UNDO .
   DEF INPUT PARAM iEndDate     AS DATE  NO-UNDO .
   DEF INPUT PARAM iGrupCounter AS INT64   NO-UNDO .
  
   DEF VAR vAcctType AS CHAR   NO-UNDO.
   DEF VAR vFunc     AS CHAR   NO-UNDO.
   DEF VAR vList     AS CHAR   NO-UNDO.
   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR vI        AS INT64    NO-UNDO. /* Счетчик */
   DEF VAR vCurrency AS CHAR   NO-UNDO. /* тк счета в одной валюте */
   DEF BUFFER acct FOR acct .

   {empty ttsh}
   DO vI = 1 TO NUM-ENTRIES(iStr):
      ASSIGN
         iStr      = REPLACE(iStr, " ", "")
         vFunc     = ENTRY(vi,iStr)
         vAcctType = SUBSTR(SUBSTR(vFunc,INDEX(vFunc, "(") + 1), 1,
                            LENGTH(SUBSTR(vFunc,INDEX(vFunc, "(") + 1)) - 1)
      .
      RUN listacct.p(vAcctType,iRid,iBegDate,iEndDate,OUTPUT vList) .
      DO i = 1 TO 	NUM-ENTRIES(vList) :
         FIND FIRST acct  WHERE acct.acct = ENTRY(i,vList) NO-LOCK NO-ERROR .
         IF NOT AVAIL acct
         THEN NEXT .
         RUN apos-sh.p(acct.acct,acct.currency,iBegDate,iEndDate,gop-status).
         FOR EACH sh NO-LOCK:
            /* Корректировка остатка */
            FIND FIRST ttsh WHERE ttsh.since EQ sh.since NO-LOCK NO-ERROR.
            IF NOT AVAIL ttsh 
            THEN DO:
               CREATE ttsh.
               ASSIGN 
                  ttsh.Summa = 0
                  ttsh.since = sh.since
                  vCurrency  = acct.currency
               .
            END.
            ttsh.Summa = ttsh.Summa + IF acct.currency = '' THEN sh.bal ELSE sh.val.
            
         END.
      END.
   END.
   FOR EACH ttsh NO-LOCK:
      RUN SetLnShPrm (ttsh.since + IF vIshOst THEN 0 ELSE 1,
                      ?,
                      ?,
                      vCurrency,
                      ?,
                      ttsh.Summa,
                      ?,
                      ?,
                      ?,
                      ?,
                      iGrupCounter,
                      ?,
                      ?).
   END.
END PROCEDURE .

FUNCTION is-need-correct RETURNS LOGICAL ( iContract AS CHAR,
                                           iContCode AS CHAR,
                                           iDate     AS DATE ):

   RETURN IF is-depmax (iContract,iContCode,iDate) EQ "1" THEN YES
                                                          ELSE NO.

END FUNCTION.

PROCEDURE base-sum-correct.

   DEF INPUT        PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT        PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT        PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT        PARAM iRateCode AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM ioBaseSum AS DEC  NO-UNDO.
   
   DEF VAR vDepMax AS DEC NO-UNDO.
   DEF BUFFER bc-rate  FOR comm-rate.

   mb:
   DO ON ERROR UNDO, LEAVE:

      IF NOT CAN-DO("%Деп,%ДепМакс",iRateCode) THEN
         LEAVE mb.
      
      FIND LAST bc-rate WHERE bc-rate.kau        EQ iContract + "," + iContCode
                          AND bc-rate.commission EQ "МаксДеп"
                          AND bc-rate.since      LE iDate
         NO-LOCK NO-ERROR.
      /* Получение комиссии/тарифа на дату */
      vDepMax = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                 ELSE ?.
      
      IF    iRateCode EQ "%Деп" 
        AND ioBaseSum > vDepMax THEN
        ioBaseSum = vDepMax.

      IF iRateCode EQ "%ДепМакс" THEN
      DO:
        IF ioBaseSum > vDepMax THEN
           ioBaseSum = ioBaseSum - vDepMax.
        ELSE
        IF ioBaseSum <= vDepMax  THEN
           ioBaseSum = 0.
      END.

   END. /* mb: */

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:20:59.602+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='ln-proc.i' */
/*prosignO7aoTRNnCFzWlMV+jTIX5Q*/