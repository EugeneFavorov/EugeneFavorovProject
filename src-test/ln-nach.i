DEF NEW GLOBAL SHARED TEMP-TABLE filost NO-UNDO
      FIELD type    AS INT64
      FIELD balance LIKE loan-var.balance
      FIELD since   LIKE term-obl.end-date
INDEX since IS UNIQUE type since ASCENDING
INDEX s since .


DEF TEMP-TABLE ttCorr NO-UNDO
   FIELD end-date AS DATE
   FIELD Summa    AS DEC
   FIELD Corr     AS DEC
INDEX end-date end-date.
.
DEF TEMP-TABLE tt-trans-sum NO-UNDO
   FIELD contcode AS CHAR
   FIELD since    AS DATE
   FIELD Summa    AS DEC EXTENT 10 INIT ?
INDEX s since contcode
INDEX c contcode
.

&GLOB GCodePeny "пеня"  /* Код ставки "пеня" */
&GLOB GCodePenyFix "пеня="  /* Код ставки "пеня в " */
&Glob CodOstPar
DEF VAR CodOstPar AS INT64 NO-UNDO .
DEF VAR vIshOst   AS LOG  NO-UNDO.

FUNCTION SetCodPar RETURNS LOG(INPUT iClass AS CHAR):
 CodOstpar = GetParCode(iClass,'КодОснДолг') .
END.

{w-ost.i}

/* Возвращает код процентной ставки.
** Данная связь, должна прошиваться в настройках БД.
** Для работы данной процедуры необходим инклюдник svarloan.def
*/
PROCEDURE GetCodeRate.

   DEF INPUT  PARAM ipPrmInt   AS INT64  NO-UNDO. /* Порядковый номер %%, пени
                                                   loan.interest[cod-par] */
   DEF OUTPUT PARAM opRateChar AS CHAR NO-UNDO. /* Код тарифа/комиссии */
   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vCommChar AS CHAR
                        INIT "1,2,3,4,5,2,3,4,5,6,4,5,7,8"
                        NO-UNDO. /* Массив связи процентов и комиссий */

   opRateChar = lrate [lr-st + INT64(ENTRY(ipPrmInt, vCommChar)) - 1].

END PROCEDURE.

/* Динамика изменений условий в интервале дат
** Формирует на дату условия код схемы начисления процентов */
PROCEDURE GetCondDate.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* Дата начала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* Дата окончания */

   DEF VAR vActualSchChar AS CHAR NO-UNDO. /* Код схемы начисления процентов */
   DEF VAR vCurrEndDate   AS DATE NO-UNDO. /* Текущая дата окончания
                                              интервала */

   DEF BUFFER next-loan-cond FOR loan-cond.

   /* Двигаемся в интервале дат */
   DO WHILE ipBegDate LE ipEndDate:

      /* Поиск актуального условия */
      RUN RE_L_COND IN h_Loan
                    (ipContractChar,
                     ipContCodeChar,
                     ipBegDate,
                     BUFFER loan-cond).

      /* Поиск следующего условия */
      RUN RE_L_COND_FRST IN h_Loan
                         (ipContractChar,
                          ipContCodeChar,
                          ipBegDate + 1,
                          BUFFER next-loan-cond).

      /* Определение даты окончания интервала начисления */

      vCurrEndDate   = IF AVAIL next-loan-cond
                       THEN next-loan-cond.since - 1
                       ELSE ipEndDate.

      /* Получение кода схемы начисления процентов
      ** После доработки сохранения кода схемы,
      ** скорректировать текущий код */

      IF NOT AVAILABLE loan-cond THEN
      DO:
         ipBegDate = vCurrEndDate + 1.
         NEXT.
      END.

      vActualSchChar = &IF DEFINED(prefix) = 0
                         &THEN
                         "КД_"
                         &ELSE
                         "{&prefix}"
                         &ENDIF   + string(loan-cond.disch-type).

      /* Создание/изменение таблицы LnShPrm */
      RUN SetLnShPrm (ipBegDate, vActualSchChar, ?, ?, ?, ?, ?, ?, ?, ?,1,?,?).

      ipBegDate      = vCurrEndDate + 1.

   END.

   RETURN.

END PROCEDURE. /*PROCEDURE GetCondDate.*/

/* Разбиение периода на интервалы постоянства счета по роли.
** уже не требуется т.к мы отвязали ставку от счета
*/
PROCEDURE GetLnAcct.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipAcctTypeChar AS CHAR NO-UNDO. /* Роль счета */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "По" окончание
                                                       интервала */

   DEF BUFFER loan-acct FOR loan-acct.

   /* Определяем действующие счета в интервале дат */
   DO WHILE ipBegDate LE ipEndDate:

      /* Поиск счета на дату ipBegDate */
      run RE_L_ACCT IN h_Loan
                    (ipContractChar,
                     ipContCodeChar,
                     ipAcctTypeChar,
                     ipBegDate,
                     BUFFER loan-acct).

      IF AVAILABLE  loan-acct THEN
         /* Создание/изменение таблицы LnShPrm */
         RUN SetLnShPrm (ipBegDate, ?, loan-acct.acct,
                         loan-acct.currency, ?, ?, ?, ?, ?, ?,1,?,?).

      /* Поиск следующей привязки */
      RUN RE_L_ACCT_FRST IN h_Loan
                        (ipContractChar,
                         ipContCodeChar,
                         ipAcctTypeChar,
                         ipBegDate + 1,
                         BUFFER loan-acct).

      ipBegDate = IF AVAIL loan-acct
                  THEN loan-acct.since
                  ELSE ipEndDate + 1.
   END.

END PROCEDURE. /*PROCEDURE GetLnAcct.*/

/* Формирование динамики схем начисления */
PROCEDURE GetLnScheam.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "По" окончание
                                                       интервала */

   DEF VAR vSchRecid   AS RECID NO-UNDO. /* Идентификатор сх начисления */
   DEF VAR vEndSchDate AS DATE  NO-UNDO. /* Дата окончания текущей
                                            сх. начисления */

   DEF BUFFER xLnShPrm FOR LnShPrm.

   /* "Зажимаем" определение схем в интервал дат */
   DO WHILE ipBegDate LE ipEndDate:

      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.


      IF AVAIL LnShPrm THEN
      DO:
         /* Беру актуальную схему на дату */
         RUN get_current_loan_scheme IN h_Loan
             (LnShPrm.IntSch, /*  Код схемы начисления %% */
              /*LnShPrm.since,*/
              ipBegDate, /*by Илюха - выяснить: когда схема заведена после
                           начала договора происходит зацикливание*/
              OUTPUT vSchRecid).

         /* Создание/изменение таблицы LnShPrm */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, vSchRecid, ?, ?, ?, ?, ?,1,?,?).

         /* Узнаю дату окончания действия текущей схемы начисления */
         RUN get_next_date_loan_scheme  IN h_Loan
             (LnShPrm.IntSch, /*  Код схемы начисления %% */
              /*LnShPrm.since,*/
              ipBegDate, /*by Илюха - выяснить: когда схема заведена после
                           начала договора происходит зацикливание*/
              OUTPUT vEndSchDate).

         /*а если действие текущей схемы заканчивается позже, чем
           начинается действие другой*/
         FIND FIRST xLnShPrm WHERE
                    xLnShPrm.since  GT ipBegDate
                AND xLnShPrm.IntSch NE LnShPrm.IntSch
            NO-LOCK NO-ERROR.

         IF AVAILABLE xLnShPrm THEN
            vEndSchDate  = IF vEndSchDate <> ? THEN
                              MIN(vEndSchDate,xLnShPrm.since)
                           ELSE
                              xLnShPrm.since.

         /* Корректировка даты начала следующей итерации */
         /*  */
         ipBegDate = IF  vEndSchDate NE ? AND
                         vEndSchDate LE ipEndDate
                     THEN vEndSchDate
                     ELSE ipEndDate + 1.
      END.
      ELSE
      DO:

         /* Поик первой попавшейся записи с даты начала интервала */
         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
            NO-ERROR.

         /* Корректировка даты начала следующей итерации */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

END PROCEDURE.

/* Получение динамики остатка по параметрам договора.
** Частный случай - получение значения по одному параметру. */
PROCEDURE GET_REM_BY_PRM.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipPrmChar      AS CHAR NO-UNDO. /* Параметры для расчета
                                                      остатка. */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "По" окончание
                                                      интервала */
   DEF INPUT PARAM iCodPar        AS INT64  NO-UNDO. /* */
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* Валюта*/
   DEF INPUT PARAM iSince         AS DATE NO-UNDO. /* дата состояния догоовра*/
   DEF INPUT PARAM iRecalcLoan    AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP      AS LOG  NO-UNDO.


   DEF VAR vChgDate     AS DATE NO-UNDO. /* Дата измененения парметра */
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-ый параметр */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* Сумма парметров на дату */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* Сумма i-ого параметра */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* Сумма дебетовых оборотов */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* Сумма кредитовых оборотов */
   /* Зажимаемся в указанный интервал дат */
   DO WHILE ipBegDate LE ipEndDate:
      vTotalPrmDec = 0.
      DO vCurrPrmInt = 1 TO NUM-ENTRIES (ipPrmChar):
         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar
            AND iCodPar = 1
            AND NOT iRecalcLoan
            AND NOT iRecalcPP THEN
         DO:
            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1, /* Дата расчета параметра */
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar
                    AND iCodPar = 1
                    AND iRecalcPP THEN
               RUN GetFilOstSumm(ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                 CodOstPar,
                          OUTPUT vPrmValDec).
            ELSE
         /* пока оставил  этот кусок
         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
            iCodPar = 1
         THEN
         DO:

            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1, /* Дата расчета параметра */
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                               INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE
         */
            /* Получение значения параметра */
               RUN STNDRT_PARAM IN h_Loan
                             (ipContractChar,
                              ipContCodeChar,
                              INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                              ipBegDate - IF vIshOst THEN 0 ELSE 1, /* Дата расчета параметра */
                              OUTPUT vPrmValDec,
                              OUTPUT vDbDec,
                              OUTPUT vCrDec).

            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
         END.

         /* Корректировка остатка */
         RUN SetLnShPrm (ipBegDate, ?, ?, iCurrency,?,vTotalPrmDec, ?, ?, ?, ?,1,?,?).

        /* Получение даты изменения параметра */
        RUN GetChgDateParam (ipContractChar,
                             ipContCodeChar,
                             ipPrmChar,          /* Список интересующих параметров */
                             ipBegDate - IF vIshOst THEN 0 ELSE 1, /* От какой даты смотреть */
                             NOT iRecalcPP,
                             OUTPUT vChgDate).

        IF iCodPar = 1
           AND LOOKUP(string(CodOstPar),ipPrmChar) <> 0
           AND NOT iRecalcLoan
           AND NOT iRecalcPP THEN
            RUN CORR_NEXT_DATE (ipBegDate - IF vIshOst THEN 0 ELSE 1 ,
                   INPUT-OUTPUT vChgDate).

        /* Корректировка даты текущего интервала */
        ipBegDate = IF vChgDate GT ipEndDate THEN
                       ipEndDate + 1
                    ELSE
                       vChgDate + IF vIshOst THEN 0 ELSE 1.  /* Исх/Вх остаток */
     END.
  RETURN.
END PROCEDURE.

/* Формирование планового ВХОДЯЩЕГО остатка по плановым суммам */
PROCEDURE GET_REM_BY_TERM.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipIdntInt      AS INT64  NO-UNDO. /* Идентификатор плановой
                                                      сущности */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "По" окончание
                                                      интервала */
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* Валюта*/
   DEF INPUT PARAM iForTransh     AS LOG  NO-UNDO.

   DEF BUFFER term-obl   FOR term-obl.

      /* Поиск остатка на дату начала i-го периода */
   FIND LAST term-obl WHERE
             term-obl.contract  EQ ipContractChar
         AND term-obl.cont-code EQ ipContCodeChar
         AND term-obl.idnt      EQ ipIdntInt
         AND term-obl.end-date  LT ipBegDate + (IF vIshOst THEN 1 ELSE 0)
   NO-LOCK NO-ERROR.

   /* Формирование остатка в интервале дат */
   DO WHILE ipBegDate LE ipEndDate:
      /* Формирование/корректировка остатка */
      IF AVAIL term-obl THEN
      DO:
         IF iForTransh THEN
            RUN SetTranshSum (ipContCodeChar,ipBegDate,term-obl.amt-rub,1).
         ELSE
            RUN SetLnShPrm (ipBegDate,?,?,iCurrency,?,term-obl.amt-rub,?,?,?,?,1,?,?).
      END.
      /* Определение даты следующего планового платежа */
      FIND FIRST term-obl WHERE
                 term-obl.contract  EQ ipContractChar
             AND term-obl.cont-code EQ ipContCodeChar
             AND term-obl.idnt      EQ ipIdntInt
             AND term-obl.end-date  GE ipBegDate + (IF vIshOst THEN 1 ELSE 0)
         NO-LOCK NO-ERROR.

      ipBegDate = IF AVAIL term-obl
                  THEN term-obl.end-date + 1
                  ELSE ipEndDate + 1.

   END.

   RETURN.

END PROCEDURE.

/* Формирование комиссии по остатку */
PROCEDURE GET_COMM_BY_REM.

   DEF INPUT PARAM ipCommChar AS CHAR NO-UNDO. /* Код комиссии */
   DEF INPUT PARAM ipKauChar  AS CHAR NO-UNDO. /* Код КАУ */
   DEF INPUT PARAM ipBegDate  AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate  AS DATE NO-UNDO. /* "По" окончание интервала */

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* Идентификатор счета */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* Текущее значение %% ставки/тарифа */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* Ставка/тариф */
   DEF VAR vCommCur      AS CHAR  NO-UNDO.

   DEF BUFFER bLnShPrm FOR LnShPrm.
   DEF BUFFER bc-rate  FOR comm-rate.

   /* Формирование комиссии в интервале дат */
   DO WHILE ipBegDate LE ipEndDate:

      /* Поиск параметров на дату */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* Если параметры определены,
      ** то попытка поиска значения комиссии/тарифа */
      IF AVAIL LnShPrm THEN
      DO:
         FIND LAST bc-rate WHERE bc-rate.kau        EQ ipKauChar
                             AND bc-rate.commission EQ ipCommChar
                             AND bc-rate.since      LE ipBegDate
            NO-LOCK NO-ERROR.
         /* Получение комиссии/тарифа на дату */
         vRateDec = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                     ELSE 0.
         ASSIGN
             vAcctRecid = ?
             vCommCur   = LnShprm.Currency

             /* Получение типа: "%,=,пеня" */
             vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", ipCommChar)
                             THEN
                                {&GCodePeny}
                             ELSE
                             IF GET_COMM_TYPE (
                                  ipCommChar,         /* Код комиссии */
                                  vAcctRecid,         /* Идентификатор счета */
                                  vCommCur,           /* Код валюты */
                                  ipKauChar,          /* Код КАУ  */
                                  LnShPrm.balance[1], /* Мин остаток */
                                  0,                  /* Период/срок */
                                  ipBegDate)
                             THEN "="
                             ELSE "%"
         .
          /* Уточним, не я вляется ли пени фиксированным */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
                  ipCommChar,         /* Код комиссии */
                  vAcctRecid,         /* Идентификатор счета */
                  vCommCur,           /* Код валюты */
                  ipKauChar,          /* Код КАУ  */
                  LnShPrm.balance[1], /* Мин остаток */
                  0,                  /* Период/срок */
                  ipBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.

         /* Корректировка/создание %% ставки */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?,
                         vRateDec, vCommTypeChar, ?,1,?,?).


         /* Узнаем дату изменения СТАВКИ / ТАРИФА */
         ipBegDate = GET_NEXT_COMM_DATE (
                      ipCommChar,          /* Код комиссии */
                      vAcctRecid,          /* Идентификатор счета */
                      vCommCur,            /* Код приведенной валюты */
                      ipKauChar,           /* Код КАУ ("" - по умолчанию) */
                      LnShPrm.balance[1],  /* Мин остаток (0 - поумолчанию) */
                      0,                   /* Период/срок (0 - поумолчанию) */
                      ipBegDate,
                      ipEndDate).

      END.
      /* Если параметров для поиска комиссий не найдено,
      ** то ищем их впереди */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
         NO-ERROR.

         /* А если их нет,
         ** то и не надо искать комиссию/тариф */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* Начисление процентов и формироание отчета */
PROCEDURE GET_NAC_REP.

   DEF INPUT PARAM ipSchmRecid AS RECID NO-UNDO. /* Код схемы начисления */
   DEF INPUT PARAM ipBegDate   AS DATE  NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate   AS DATE  NO-UNDO. /* "По" окончание интервала */
   DEF INPUT PARAM ipFormRasch AS CHAR  NO-UNDO. /* Формула для расчета процентов с ДР ВыбФормРасчПроц */

   DEF VAR vEndDate AS DATE NO-UNDO. /* Окончание периода начисления прцентов*/
   DEF VAR vDaysInt AS INT64  NO-UNDO. /* Количество дней в периоде */

   DEF BUFFER buf-lnFost        FOR LnShPrm.
   DEF BUFFER interest-sch-line FOR interest-sch-line.

   /* Поиск комиссии */
   RUN get_sch_line_by_rid IN h_schem
       (ipSchmRecid,
        BUFFER interest-sch-line).

   /* Поиск параметров начисления на дату */
   FIND LAST LnShPrm WHERE
             LnShPrm.since LE ipBegDate
      NO-ERROR.

   /* Поиск параметров начисления вперед */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since GT ipBegDate
      NO-ERROR.

   /* Определение даты начала начисления,
   ** исходя из наличия параметров начисления */
   ipBegDate = IF AVAIL LnShPrm
               THEN ipBegDate
               ELSE IF AVAIL buf-lnFost
                    THEN buf-lnFost.since
                    ELSE ipEndDate.

   /* Зажимаемся в установленный (скорректированный)
   ** интервал дат */
   DO WHILE ipBegDate LE ipEndDate:

      /* Находим параметры расчета */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* Поиск следующих параметров */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since GT ipBegDate
         NO-ERROR.

      /* Получение даты окончания текущего периода */
      vEndDate = IF AVAIL buf-lnFost
                 THEN buf-lnFost.since - 1
                 ELSE ipEndDate.

      /* Определение кол-ва дней в интервале */
      vDaysInt = cDay(interest-sch-line.interest-month,
                      ipBegDate,
                      vEndDate + 1).

      /* otch1 - расшарена. В ней и создаем отчет */
      IF AVAIL LnShPrm THEN
      DO:

         CREATE otch1.
         ASSIGN
             otch1.bal-summ = LnShPrm.balance[1]
             otch1.beg-date = ipBegDate
             otch1.end-date = vEndDate
             otch1.ndays    = vDaysInt
             otch1.rat1     = LnShPrm.rate[1]

             /* Расчет процентной ставки в зависимости от типа */
             otch1.summ_pr =

             /* Расчет значения по с типом "ставка". */
             IF    LnShPrm.CommType[1] EQ "%"
               AND ipFormRasch EQ "1" THEN
                       /*Дн в пер*//*     Сумма     */   /* Ставка    */    /*       Дней в году      */
                ROUND ((vDaysInt * (LnShPrm.balance[1] * LnShPrm.rate[1] / (interest-sch-line.basis-time * 100))), 2)
             ELSE
             IF    LnShPrm.CommType[1] EQ "%"
              AND ipFormRasch EQ "2" THEN
                      /*      Сумма     */  /*  Ставка   */ /*100*/ /*Дн в пер*/ /*       Дней в году      */
               ROUND(((LnShPrm.balance[1] * LnShPrm.rate[1] / 100 ) * vDaysInt / interest-sch-line.basis-time),2)

             /* Расчет значения с типом "тариф". */
             ELSE IF LnShPrm.CommType[1] EQ "="
             THEN ROUND(LnShPrm.rate[1], 2)

             /* Расчет значения с типом "пеня". */
             ELSE IF LnShPrm.CommType[1] EQ {&GCodePeny} THEN
                ROUND ((vDaysInt * (LnShPrm.balance[1] *
                                    LnShPrm.rate[1] / 100)), 2)

             /* Расчет значения с типом "пеня = фиксированная". */
             ELSE IF LnShPrm.CommType[1] EQ {&GCodePenyFix} THEN
                IF LnShPrm.balance[1] > 0
                  THEN ROUND ((vDaysInt *  LnShPrm.rate[1]) , 2)
                  ELSE 0
             ELSE ?

             otch1.summ_pr  = IF otch1.summ_pr EQ ?
                              THEN 0
                              ELSE otch1.summ_pr

         .

      END.

      ipBegDate = vEndDate + 1.

   END.

   RETURN.

END PROCEDURE.

/* Вызов схем начисление процентов по договорам
** по сформированной временной таблице */
PROCEDURE RunLnScheam.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* Дата начала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* Дата окончания */
   DEF INPUT PARAM dat-per        AS DATE NO-UNDO. /* Дата перехода на 39П */
   DEF INPUT PARAM cod-par        AS INT64  NO-UNDO. /* Код параметра не исп. */
   DEF INPUT PARAM fl-type-ost    AS INT64  NO-UNDO. /* Всегда передается
                                                      1 не исп. */

   DEF VAR vEndDate    AS DATE        NO-UNDO. /* Дата окончания текущего интервала */
   DEF VAR vLMDateTmp  AS DATE        NO-UNDO.
   DEF VAR vIsPrSd     AS CHAR        NO-UNDO. /* ДР на условии учитывать ли сдвиг процентов */
   DEF VAR vBegDateDay AS INT64       NO-UNDO.
   DEF VAR vTmp        AS CHAR        NO-UNDO. /* ДР на условии учитывать ли сдвиг процентов */
   DEF VAR vI          AS INT64       NO-UNDO.
   DEF VAR vFlag       AS LOGICAL     NO-UNDO.
   DEF VAR vDate       AS DATE        NO-UNDO.

   DEF BUFFER buf-lnFost FOR LnShPrm.
   DEF BUFFER buf-lnTmp  FOR LnShPrm.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bloan      FOR loan.
   DEF BUFFER bpobl      FOR term-obl.

   /* Поиск схемы ранее даты начала интервала */
   FIND LAST LnShPrm WHERE
             LnShPrm.since LE ipBegDate
      NO-ERROR.

   /* Поиск схемы позже даты начала интервала */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since GT ipBegDate
      NO-ERROR.

   /* Определение даты начала начисления,
   ** исходя из наличия схем начисления */
   ipBegDate = IF AVAIL LnShPrm
               THEN ipBegDate
               ELSE IF AVAIL buf-lnFost
                    THEN buf-lnFost.since
                    ELSE ipEndDate.

      IF AVAILABLE LnShPrm THEN DO:
         IF CAN-DO("КД_1,КД_17",LnShPrm.IntSch)
         THEN DO:
            FIND LAST bloan-cond WHERE bloan-cond.contract  EQ ipContractChar
                                 AND bloan-cond.cont-code EQ ipContCodeChar
                                 AND bloan-cond.since     LE ipBegDate
            NO-LOCK NO-ERROR.

            vIsPrSd    = GetXAttrValue("loan-cond",
                                       bloan-cond.contract + "," +  bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                       "ПрУчСд").
            IF vIsPrSd NE "нет"
            THEN
               vIsPrSd = GetXattrInit(bloan-cond.class-code,"ПрУчСд").
         END.

         /* Специальные условия начисления процентов */
         IF    (   (    LnShPrm.IntSch EQ "КД_17")
               OR (    LnShPrm.IntSch EQ "КД_1"
                  AND vIsPrSd        EQ "нет")
               )
            AND (GetSysConf("calc-s1") NE "yes")
            AND cod-par EQ 1

         THEN DO:

            FIND FIRST bloan WHERE bloan.contract  EQ ipContractChar
                              AND bloan.cont-code EQ ipContCodeChar
            NO-LOCK NO-ERROR.

            IF GetSysConf("calc-loan-state") NE "yes"
            THEN DO:
               IF    vIsPrSd        EQ "нет"
                  OR LnShPrm.IntSch EQ "КД_17"
               THEN DO:
                  vBegDateDay = IF bloan-cond.cred-date EQ 31
                              THEN 1
                              ELSE (bloan-cond.cred-date + 1).
                  vLMDateTmp  = Date(MONTH(ipBegDate), 1, YEAR(ipBegDate)) - 1.
                  FND_BegDate:
                  DO WHILE     DAY(ipBegDate) NE vBegDateDay
                           AND ipBegDate      GT (bloan.open-date + 1)
                  :
                     ipBegDate = ipBegDate - 1.
                     IF     ipBegDate       EQ vLMDateTmp
                        AND DAY(vLMDateTmp) LE vBegDateDay
                     THEN
                        LEAVE FND_BegDate.
                  END.

                  LnShPrm.since = ipBegDate.
                  vLMDateTmp    = Date(MONTH(ipEndDate), 1, YEAR(ipEndDate)) - 1.

                  FND_EndDate:
                  DO WHILE     DAY(ipEndDate) GT bloan-cond.cred-date
                           AND ipEndDate ne bloan.end-date
                  :
                     ipEndDate = ipEndDate - 1.
                     IF     ipEndDate EQ vLMDateTmp
                        AND DAY(vLMDateTmp) LE bloan-cond.cred-date
                     THEN
                        LEAVE FND_EndDate.
                  END.
                  IF LnShPrm.IntSch EQ "КД_17"
                  THEN DO:
                     IF     ipBegDate        LE (bloan.open-date + 1)
                        AND DAY(ipBegDate)   LE bloan-cond.cred-date
                        AND MONTH(ipEndDate) EQ MONTH(ipBegDate)
                        AND YEAR(ipEndDate)  EQ YEAR(ipBegDate)
                     THEN DO:
                        RETURN. /* Не начисляем проценты (нет предидущего месяца) */
                     END.
                     
                     vDate = DATE(MONTH(ipBegDate),bloan-cond.cred-date,YEAR(ipBegDate)) NO-ERROR.
                     /*  ошибка может быть только если за пределы месяца выскочим,
                     ** тогда берем последний день */
                     IF ERROR-STATUS:ERROR THEN
                        vDate = LastMonDate(ipBegDate).

                     BLOCK-DATE:
                     DO WHILE vDate LE ipEndDate:
                        IF vDate GE ipBegDate
                        THEN DO:
                           vFlag = TRUE.
                           LEAVE BLOCK-DATE.

                        END.
                        vDate = GoMonth(vDate,1).
                     END.

                     IF NOT vFlag 
                     THEN
                        RETURN. /* Не начисляем проценты (даты начисления нет в периоде) */

                     IF ipBegDate NE (bloan.open-date + 1)
                     THEN DO:
                        ipBegDate = max(bloan.open-date + 1, FirstMonDate(GoMonth(ipEndDate, -1))).
                     END.

                     IF ipEndDate EQ bloan.end-date
                     THEN DO:
                        IF DAY(ipEndDate) LT bloan-cond.cred-date
                        THEN
                           ipEndDate = LastMonDate(ipBegDate).
                        ELSE IF DAY(ipEndDate) GT bloan-cond.cred-date
                        THEN DO:
                           ipBegDate = FirstMonDate(ipEndDate).
                        END.

                     END.
                     ELSE
                     DO:
                        ipEndDate = LastMonDate(LnShPrm.since).
                     END.
                     LnShPrm.since = ipBegDate.
                  END.
               END.
            END.
            ELSE
            DO:
               IF ipEndDate EQ LastMonDate(ipEndDate) THEN
                  ipBegDate = max(FirstMonDate(ipEndDate), (bloan.open-date)).
               ELSE
                  ipBegDate = max(FirstMonDate(ipEndDate), (bloan.open-date), ipBegDate).
              
               FIND LAST buf-lnTmp WHERE buf-lnTmp.since EQ ipBegDate NO-LOCK NO-ERROR.

                  IF NOT AVAIL buf-lnTmp
                  THEN
                     LnShPrm.since = ipBegDate.
            END.
         END.
   END.
   /* **************************************** */
   /* Вызов схем начисление процентов по договорам */
   RUN_SCH:
   DO WHILE ipBegDate LE ipEndDate:

      /* Находим параметры расчета */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* Поиск следующих параметров */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since GT ipBegDate
         NO-ERROR.

      /* Получение даты окончания текущего периода */
      vEndDate = IF AVAIL buf-lnFost
                 THEN buf-lnFost.since - 1
                 ELSE ipEndDate.

      /* По каким-то причинам нет схемы начисления */
      IF NOT AVAIL LnShPrm OR LnShPrm.SchRecid EQ ? THEN
      DO:
        ipBegDate = vEndDate + 1.
        NEXT RUN_SCH.
      END.

      /* Получение наименование процедуры */
      RUN GET_SCH_LINE_BY_RID IN h_schem
            (LnShPrm.SchRecid,
             BUFFER interest-sch-line).

      /* Запуск схемы начисления */
      RUN VALUE (interest-sch-line.proc + ".p")
            (ipContractChar,        /* Назначение договора */
             ipContCodeChar,        /* Номер договора */
             LnShPrm.SchRecid,      /* Идентификатор схемы */
             ipBegDate,             /* Дата начала расчета */
             vEndDate,              /* Дата окончания расчета */
             dat-per,               /* Дата перехода на 39П */
             cod-par,               /* Код параметра не исп. */
             fl-type-ost)           /* Всегда передается 1 не исп. */
      NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         ipBegDate = vEndDate + 1.
         NEXT RUN_SCH.
      END.

      /* Корректировка даты начла текущего интервала. */
      ipBegDate = vEndDate + 1.
   END.

   RETURN.

END PROCEDURE.

/* Создание/изменение таблицы LnShPrn */
PROCEDURE SetLnShPrm.

   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* Дата изменения */
   DEF INPUT PARAM ipIntSchChar   AS CHAR  NO-UNDO.  /* Код схемы */
   DEF INPUT PARAM ipAcctChar     AS CHAR  NO-UNDO.  /* счет */
   DEF INPUT PARAM ipCurrencyChar AS CHAR  NO-UNDO.  /* Валюта  */
   DEF INPUT PARAM ipSchRecid     AS RECID NO-UNDO.  /* Идентификатор сх.
                                                        начисления */
   DEF INPUT PARAM ipBalanceDec   AS DEC   NO-UNDO.  /* остаток на дату в
                                                       исчисляемой валюте */
   DEF INPUT PARAM ipAcctRemDec   AS DEC   NO-UNDO.  /* остаток в валюте счета
                                                        (параметра) */
   DEF INPUT PARAM ipRateDec      AS DEC   NO-UNDO.  /* Значение
                                                        ставки/тариф  */
   DEF INPUT PARAM ipCommTypeChar AS CHAR  NO-UNDO.  /* Фиксированная ставка
                                                        / %% - ставка  */
   DEF INPUT PARAM ipOtherComm    AS LOG   NO-UNDO.  /*для процедуры lnremva1.p
                                                       в некоторых случаях надо
                                                       брать другую комиссию*/
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* порядковый номер
                                                        группы */
   DEF INPUT PARAM iMinRate       AS DEC   NO-UNDO.  /* мин. знач ком. */
   DEF INPUT PARAM iMaxRate       AS DEC   NO-UNDO.  /* макс. знач ком. */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* результат сравнения буферов */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* счетчик номера группы */

   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* Чтобы не отрубался доступ к
                                          таблице после процедуры */

   /* Поиск записи на входящую дату */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   IF NOT AVAIL LnShPrm THEN
   DO:

      /* создаем запись на дату */
      CREATE LnShPrm.

      LnShPrm.since = ipDate.
   END.

   ASSIGN
      LnShPrm.IntSch   = ipIntSchChar     WHEN ipIntSchChar   NE ?
      LnShPrm.acct     = ipAcctChar       WHEN ipAcctChar     NE ?
      LnShPrm.currency = ipCurrencyChar   WHEN ipCurrencyChar NE ?
      LnShPrm.SchRecid = ipSchRecid       WHEN ipSchRecid     NE ?
      LnShPrm.acct_rem = ipAcctRemDec     WHEN ipAcctRemDec   NE ?
      LnShPrm.OtherCom = ipOtherComm      WHEN ipOtherComm    NE ?
      LnShPrm.balance[iNumGrup]  = ipBalanceDec     WHEN ipBalanceDec   NE ?
      LnShPrm.rate[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
      LnShPrm.CommType[iNumGrup] = ipCommTypeChar   WHEN ipCommTypeChar NE ?
      LnShPrm.MinRate[iNumGrup]  = iMinRate         WHEN iMinRate       NE ?
      LnShPrm.MaxRate[iNumGrup]  = iMaxRate         WHEN iMaxRate       NE ?
      .

   RELEASE LnShPrm.

   /* Корректировка записей с i-ой даты */
   FOR EACH LnShPrm WHERE
            LnShPrm.since GE ipDate:

      ASSIGN
         LnShPrm.IntSch   = ipIntSchChar     WHEN ipIntSchChar   NE ?
         LnShPrm.acct     = ipAcctChar       WHEN ipAcctChar     NE ?
         LnShPrm.currency = ipCurrencyChar   WHEN ipCurrencyChar NE ?
         LnShPrm.SchRecid = ipSchRecid       WHEN ipSchRecid     NE ?
         LnShPrm.acct_rem = ipAcctRemDec     WHEN ipAcctRemDec   NE ?
         /*LnShPrm.OtherCom = ipOtherComm      WHEN ipOtherComm    NE ?*/
         LnShPrm.balance[iNumGrup]  = ipBalanceDec     WHEN ipBalanceDec   NE ?
         LnShPrm.rate[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
         LnShPrm.CommType[iNumGrup] = ipCommTypeChar   WHEN ipCommTypeChar NE ?
         LnShPrm.MinRate[iNumGrup]  = iMinRate         WHEN iMinRate       NE ?
         LnShPrm.MaxRate[iNumGrup]  = iMaxRate         WHEN iMaxRate       NE ?
         .

      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
      NO-ERROR.

      IF AVAIL PrevLnShPrm THEN
      DO:
         ASSIGN
            LnShPrm.IntSch   = PrevLnShPrm.IntSch   WHEN LnShPrm.IntSch   EQ ""
            LnShPrm.acct     = PrevLnShPrm.acct     WHEN LnShPrm.acct     EQ ""
            LnShPrm.currency = PrevLnShPrm.currency WHEN LnShPrm.currency EQ ""
            LnShPrm.SchRecid = PrevLnShPrm.SchRecid WHEN LnShPrm.SchRecid EQ ?
            LnShPrm.acct_rem = PrevLnShPrm.acct_rem WHEN LnShPrm.acct_rem EQ ?
            .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                                           WHEN LnShPrm.balance[vNumGrup]  EQ ?
               LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                                           WHEN LnShPrm.rate[vNumGrup]     EQ ?
               LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                                           WHEN LnShPrm.CommType[vNumGrup] EQ ?
               LnShPrm.MinRate[vNumGrup] = PrevLnShPrm.MinRate[vNumGrup]
                                           WHEN LnShPrm.MinRate[vNumGrup] EQ ?
               LnShPrm.MaxRate[vNumGrup] = PrevLnShPrm.MaxRate[vNumGrup]
                                           WHEN LnShPrm.MaxRate[vNumGrup] EQ ?
               .
         END.
      END.
   END.

   /* Поиск записи на дату */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   /* Поиск предыдущей записи */
   FIND LAST PrevLnShPrm WHERE
             PrevLnShPrm.since LT ipDate
      NO-ERROR.

   /* Проверка на эквивалентность записей(предыдущей и текущей) */
   IF AVAIL PrevLnShPrm THEN
   DO:

      /* Из проверки исключаем дату на которую получаем остаток. */
      BUFFER-COMPARE
         PrevLnShPrm EXCEPT since
         TO LnShPrm
         SAVE result IN vCompareLog
      NO-ERROR.

      /*если ошибки нетЮ то удаляем созданную запись*/
      IF vCompareLog THEN
         DELETE LnShPrm.
   END.

   RETURN.

END PROCEDURE.

/* Создание/изменение таблицы LnShPrn */
PROCEDURE SetLnShPrmAdd.
   DEF INPUT PARAM ipCurrencyChar AS CHAR  NO-UNDO.  /* Валюта  */

   DEF VAR vContCode   AS CHAR  NO-UNDO. /* Код транша                  */
   DEF VAR vCompareLog AS LOG   NO-UNDO. /* результат сравнения буферов */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* счетчик номера группы       */
   DEF VAR vPrevGrup   AS INT64 NO-UNDO. /* номер группы                */
   DEF VAR vPrevDate   AS DATE  NO-UNDO. /* Дата от                     */
   DEF VAR vPrevSumm   AS DEC   EXTENT 10 NO-UNDO. /* Сумма от          */

   DEF BUFFER Prev-trans  FOR tt-trans-sum.
   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* Чтобы не отрубался доступ к
                                          таблице после процедуры */
   FOR EACH tt-trans-sum BREAK BY tt-trans-sum.contcode:

       /* Поиск записи на входящую дату */
      FIND FIRST LnShPrm WHERE
                 LnShPrm.since EQ tt-trans-sum.since
         NO-ERROR.
      IF FIRST(tt-trans-sum.contcode) THEN
         vContCode = tt-trans-sum.contcode.
      IF vContCode EQ tt-trans-sum.contcode THEN
      DO:
         IF NOT AVAIL LnShPrm THEN
         DO:
            /* создаем запись на дату */
            CREATE LnShPrm.
            ASSIGN
               LnShPrm.since = tt-trans-sum.since
            .
            FIND LAST PrevLnShPrm WHERE PrevLnShPrm.since LT LnShPrm.since NO-ERROR.
            IF AVAIL PrevLnShPrm THEN
            DO:
               ASSIGN
                  LnShPrm.IntSch   = PrevLnShPrm.IntSch
                  LnShPrm.acct     = PrevLnShPrm.acct
                  LnShPrm.currency = PrevLnShPrm.currency
                  LnShPrm.SchRecid = PrevLnShPrm.SchRecid
                  LnShPrm.acct_rem = PrevLnShPrm.acct_rem
               .
               DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
                  ASSIGN
                     LnShPrm.rate[vNumGrup]    = PrevLnShPrm.rate[vNumGrup]
                     LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                  .
               END.
            END.
         END.
         ASSIGN
            LnShPrm.currency = ipCurrencyChar WHEN ipCurrencyChar NE ?
         .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup] = tt-trans-sum.Summa[vNumGrup]
                                           WHEN tt-trans-sum.Summa[vNumGrup] NE ?
            .
         END.

         RELEASE LnShPrm.
      END.
      ELSE DO:
         IF NOT AVAIL LnShPrm THEN
         DO:
            /* создаем запись на дату */
            CREATE LnShPrm.
            ASSIGN
               LnShPrm.since = tt-trans-sum.since
            .
            FIND LAST PrevLnShPrm WHERE PrevLnShPrm.since LT LnShPrm.since NO-ERROR.
            IF AVAIL PrevLnShPrm THEN
            DO:
               ASSIGN
                  LnShPrm.IntSch   = PrevLnShPrm.IntSch
                  LnShPrm.acct     = PrevLnShPrm.acct
                  LnShPrm.currency = PrevLnShPrm.currency
                  LnShPrm.SchRecid = PrevLnShPrm.SchRecid
                  LnShPrm.acct_rem = PrevLnShPrm.acct_rem
               .
               DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
                  ASSIGN
                     LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                     LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                     LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                  .
               END.
            END.
         END.
         ASSIGN
            LnShPrm.currency = ipCurrencyChar WHEN ipCurrencyChar NE ?
         .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup] = LnShPrm.balance[vNumGrup]
                                         + tt-trans-sum.Summa[vNumGrup]
                                           WHEN tt-trans-sum.Summa[vNumGrup] NE ?
            .
         END.
         RELEASE LnShPrm.
         IF vPrevDate NE ? THEN
         FOR EACH LnShPrm WHERE LnShPrm.since GT vPrevDate
                            AND LnShPrm.since LT tt-trans-sum.since:

            DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
               LnShPrm.balance[vNumGrup] = LnShPrm.balance[vNumGrup]
                                         + vPrevSumm[vNumGrup].
            END.
         END.
         vPrevDate = tt-trans-sum.since.
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            vPrevSumm[vNumGrup] = tt-trans-sum.Summa[vNumGrup].
         END.

      END.
   END.
/*
   /* Поиск записи на дату */
   FOR EACH LnShPrm :

      /* Поиск предыдущей записи */
      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
         NO-ERROR.

      /* Проверка на эквивалентность записей(предыдущей и текущей) */
      IF AVAIL PrevLnShPrm THEN
      DO:
         /* Из проверки исключаем дату на которую получаем остаток. */
         BUFFER-COMPARE
            PrevLnShPrm EXCEPT since
            TO LnShPrm
            SAVE result IN vCompareLog
         NO-ERROR.

         /*если ошибки нет, то удаляем созданную запись*/
         IF vCompareLog THEN
            DELETE LnShPrm.
      END.
   END.
*/

   RETURN.

END PROCEDURE.

/* Создание/изменение таблицы tt-trans-sum */
PROCEDURE SetTranshSum.
   DEF INPUT PARAM ipContCode     AS CHAR  NO-UNDO.  /* Код договора   */
   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* Дата изменения */
   DEF INPUT PARAM ipBalanceDec   AS DEC   NO-UNDO.  /* остаток на дату в
                                                       исчисляемой валюте */
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* порядковый номер
                                                        группы */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* результат сравнения буферов */

   DEF BUFFER Prev-trans  FOR tt-trans-sum.

   /* Поиск записи на входящую дату */
   FIND FIRST tt-trans-sum WHERE tt-trans-sum.since    EQ ipDate
                             AND tt-trans-sum.contcode EQ ipContCode
      NO-ERROR.

   IF NOT AVAIL tt-trans-sum THEN
   DO:

      /* создаем запись на дату */
      CREATE tt-trans-sum.
      ASSIGN
         tt-trans-sum.since           = ipDate
         tt-trans-sum.Summa[iNumGrup] = 0
         tt-trans-sum.contcode        = ipContCode
      .
   END.

   tt-trans-sum.Summa[iNumGrup]  = ipBalanceDec.

   RELEASE tt-trans-sum.

   /* Поиск предыдущей записи */
   FIND LAST Prev-trans WHERE Prev-trans.since LT ipDate
                          AND Prev-trans.contcode EQ ipContCode
      NO-ERROR.

   /* Проверка на эквивалентность записей(предыдущей и текущей) */
   IF AVAIL Prev-trans THEN
   DO:

      /* Поиск записи на дату */
      FIND FIRST tt-trans-sum WHERE tt-trans-sum.since EQ ipDate
                                AND tt-trans-sum.contcode EQ ipContCode
         NO-ERROR.

      /* Из проверки исключаем дату на которую получаем остаток. */
      BUFFER-COMPARE
         Prev-trans EXCEPT since
         TO tt-trans-sum
         SAVE result IN vCompareLog
      NO-ERROR.

      /*если ошибки нет, то удаляем созданную запись*/
      IF vCompareLog THEN
         DELETE tt-trans-sum.
   END.

   RETURN.

END PROCEDURE.

/* Создание/изменение таблицы LnShPrm */
PROCEDURE SetLnTax.

   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* Дата изменения */
   DEF INPUT PARAM ipRateDec      AS DEC   NO-UNDO.  /* Значение
                                                       брать другую комиссию*/
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* порядковый номер
                                                        группы */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* Результат сравнения буферов */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* Счетчик номера группы */

   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* Чтобы не отрубался доступ к
                                           таблице после процедуры*/

   /* Поиск записи на входящую дату */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   IF NOT AVAIL LnShPrm THEN
   DO:

      /* Создаем запись на дату */
      CREATE LnShPrm.

      LnShPrm.since = ipDate.
   END.

   ASSIGN
      LnShPrm.tax[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
      .

   RELEASE LnShPrm.

   /* Корректировка записей с i-ой даты. */
   FOR EACH LnShPrm WHERE
            LnShPrm.since GE ipDate:

      ASSIGN
         LnShPrm.tax[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
         .

      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
      NO-ERROR.

      IF AVAIL PrevLnShPrm THEN
      DO:
         ASSIGN
            LnShPrm.IntSch   = PrevLnShPrm.IntSch   WHEN LnShPrm.IntSch   EQ ""
            LnShPrm.acct     = PrevLnShPrm.acct     WHEN LnShPrm.acct     EQ ""
            LnShPrm.currency = PrevLnShPrm.currency WHEN LnShPrm.currency EQ ""
            LnShPrm.SchRecid = PrevLnShPrm.SchRecid WHEN LnShPrm.SchRecid EQ ?
            LnShPrm.acct_rem = PrevLnShPrm.acct_rem WHEN LnShPrm.acct_rem EQ ?
            .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                                           WHEN LnShPrm.balance[vNumGrup]  EQ ?
               LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                                           WHEN LnShPrm.rate[vNumGrup]     EQ ?
               LnShPrm.tax[vNumGrup]     = PrevLnShPrm.tax[vNumGrup]
                                           WHEN LnShPrm.tax[vNumGrup]     EQ ?
               LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                                           WHEN LnShPrm.CommType[vNumGrup] EQ ?
               .
         END.
      END.
   END.

   /* Поиск записи на дату */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   /* Поиск предыдущей записи */
   FIND LAST PrevLnShPrm WHERE
             PrevLnShPrm.since LT ipDate
      NO-ERROR.

   /* Проверка на эквивалентность записей (предыдущей и текущей) */
   IF AVAIL PrevLnShPrm THEN
   DO:

      /* Из проверки исключаем дату на которую получаем остаток. */
      BUFFER-COMPARE
         PrevLnShPrm EXCEPT since
         TO LnShPrm
         SAVE result IN vCompareLog
      NO-ERROR.

      /*Если ошибки нет, то удаляем созданную запись.*/
      IF vCompareLog THEN
         DELETE LnShPrm.
   END.

   RETURN.

END PROCEDURE.

/* Формирование базы начисления. */
PROCEDURE GetCalcChar:

   DEF INPUT  PARAM iContractChar AS CHAR NO-UNDO. /* Назначенеи договора */
   DEF INPUT  PARAM iContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iCodPar       AS INT64  NO-UNDO. /* Код параметра */
   DEF OUTPUT PARAM oCurrCalcChar AS CHAR NO-UNDO. /* База начисления */

   oCurrCalcChar = IF iCodPar EQ 1 THEN
                       "0,7,13"
                   ELSE
                       STRING(most[iCodPar]).

END. /*PROCEDURE GetCalcChar*/


/* Получение динамики остатка по параметрам договора.
** Частный случай - получение значения по одному параметру. */
PROCEDURE GET_REM_BY_PRM_VTB.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM ipPrmChar      AS CHAR NO-UNDO. /* Параметры для расчета
                                                      остатка. */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "По" окончание
                                                      интервала */
   DEF INPUT PARAM ipCodPar       AS INT64  NO-UNDO. /* код параметра*/
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* Валюта*/
   DEF INPUT PARAM iSince         AS DATE NO-UNDO. /* дата состояния договора*/
   DEF INPUT PARAM iRecalcLoan    AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP      AS LOG  NO-UNDO.

   DEF VAR vChgDate     AS DATE NO-UNDO. /* Дата измененения парметра */
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-ый параметр */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* Сумма парметров на дату */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* Сумма i-ого параметра */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* Сумма дебетовых оборотов */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* Сумма кредитовых оборотов */
   DEF VAR vPrmValDec10 AS DEC  NO-UNDO. /* Сумма 10-ого параметра */
   DEF VAR vPrmValDec48 AS DEC  NO-UNDO. /* Сумма 48-ого параметра */

   IF ipCodPar = 1 AND iRecalcPP THEN ipPrmChar = STRING(CodOstPar).
   /* Зажимаемся в указанный интервал дат */
   DO WHILE ipBegDate LE ipEndDate:

      vTotalPrmDec = 0.

      DO vCurrPrmInt = 1 TO NUM-ENTRIES (ipPrmChar):


         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
            ipCodPar = 1                                    AND
            NOT iRecalcLoan                                 AND
            NOT iRecalcPP
         THEN
         DO:
            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1,
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
                 ipCodPar = 1                           AND
                 iRecalcPP
         THEN
            RUN GetFilOstSumm(ipBegDate - IF vIshOst THEN 0 ELSE 1,
                              CodOstPar,
                       OUTPUT vPrmValDec).
         ELSE

            /* Получение значения параметра */
            RUN STNDRT_PARAM IN h_Loan
                             (ipContractChar,
                              ipContCodeChar,
                              INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                              ipBegDate - IF vIshOst THEN 0 ELSE 1,
                              OUTPUT vPrmValDec,
                              OUTPUT vDbDec,
                              OUTPUT vCrDec).

         /*если ненулевое значение параметра 10 или 48, то сохраняем значение*/
         IF ipCodPar = 1 THEN
         DO:
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 10 THEN
               vPrmValDec10 = vPrmValDec.
            ELSE
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 48 THEN
               vPrmValDec48 = vPrmValDec.

            vTotalPrmDec = vTotalPrmDec +
                             IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 10 OR
                                INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 48
                             THEN
                                0
                             ELSE
                                vPrmValDec.
         END.
         ELSE
            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
      END.

      /* Корректировка остатка */
      RUN SetLnShPrm (ipBegDate, ?, ?, iCurrency, ?, vTotalPrmDec, ?, ?, ?,
                      (IF (vPrmValDec10 <> 0   OR
                           vPrmValDec48 <> 0 ) AND
                          ipCodPar = 1
                       THEN
                         YES
                       ELSE
                         ?),
                      1,
                      ?,
                      ?).

      /* Получение даты изменения параметра */
      RUN GetChgDateParam (ipContractChar,
                           ipContCodeChar,
                           ipPrmChar,          /* Список интересующих параметров */
                           ipBegDate - IF vIshOst THEN 0 ELSE 1, /* От какой даты смотреть */
                           NOT iRecalcPP,
                           OUTPUT vChgDate).

      IF ipCodPar = 1               AND
         LOOKUP(STRING(CodOstPar),ipPrmChar) <> 0 AND
         NOT iRecalcLoan            AND
         NOT iRecalcPP
      THEN
         RUN CORR_NEXT_DATE (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                INPUT-OUTPUT vChgDate).

      /* Корректировка даты текущего интервала */
      ipBegDate = IF vChgDate GT ipEndDate
                  THEN ipEndDate + 1
                  ELSE vChgDate + IF vIshOst THEN 0 ELSE 1.
   END.

   RETURN.

END PROCEDURE.

/* Формирование комиссии по остатку */
PROCEDURE GET_COMM_BY_REM_VTB.

   DEF INPUT PARAM ipCommChar AS CHAR NO-UNDO. /* Код комиссии */
   DEF INPUT PARAM ipKauChar  AS CHAR NO-UNDO. /* Код КАУ */
   DEF INPUT PARAM ipBegDate  AS DATE NO-UNDO. /* "С" начало интервала */
   DEF INPUT PARAM ipEndDate  AS DATE NO-UNDO. /* "По" окончание интервала */
   DEF INPUT PARAM ipCodPar   AS INT64  NO-UNDO. /* код параметра*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* Идентификатор счета */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* Текущее значение %%
                                              ставки/тарифа */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* Ставка/тариф */
   DEF VAR  BegDate1     AS DATE  NO-UNDO.
   DEF VAR  BegDate2     AS DATE  NO-UNDO.
   DEF VAR vCommCur      AS CHAR  NO-UNDO.

   DEF VAR vLog  AS LOG  NO-UNDO.
   DEF VAR vSumm AS DEC  NO-UNDO.
   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER bLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm  FOR LnShPrm.

   /* Формирование комиссии в интервале дат */
   DO WHILE ipBegDate LE ipEndDate:

      /* Поиск параметров на дату */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* Если параметры определены,
      ** то попытка поиска значения комиссии/тарифа */
      IF AVAIL LnShPrm  THEN
      DO:

         ASSIGN
             vAcctRecid = ?
             vCommCur   = LnShPrm.Currency
             .
             /* Получение комиссии/тарифа на дату */
             /*
             vRateDec = GET_COMM (
                   ipCommChar,         /* Код комиссии */
                   vAcctRecid,         /* Идентификатор счета */
                   vCommCur,           /* Код приведенной валюты */
                   ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                   LnShPrm.balance[1], /* Мин остаток (0 - поумолчанию) */
                   0,                  /* Период/срок (0 - поумолчанию) */
                   ipBegDate).*/
             /*vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).*/
         /*для lnremva1.p
         если основные проценты и расчет по повышенной ставке,
         то определяем ее,  если ее нет, то работаем по старой */
         IF ipCodPar         = 1    AND
            LnShPrm.OtherCom = YES THEN
         DO:
            /*vRateDec   = GET_COMM(
                "%КрПов",           /* Код комиссии */
                vAcctRecid,         /* Идентификатор счета */
                vCommCur,           /* Код приведенной валюты */
                ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                LnShPrm.balance[1], /* Минимальный остаток (0 - поумолчанию) */
                0,                  /* Период/срок (0 - поумолчанию) */
                ipBegDate).*/
            vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   "%КрПов",
                   ipBegDate).

            IF vRateDec = ?  THEN
               vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).
               /*
               vRateDec = GET_COMM(
                   ipCommChar,         /* Код комиссии */
                   vAcctRecid,         /* Идентификатор счета */
                   vCommCur,           /* Код приведенной валюты */
                   ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                   LnShPrm.balance[1], /* Миним  остаток (0 - поумолчанию) */
                   0,                  /* Период/срок (0 - поумолчанию) */
                   ipBegDate).*/

         END.
         ELSE
           vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).
         /* Получение типа: "%,=,пеня" */
         vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", ipCommChar)
                         THEN {&GCodePeny}
                         ELSE IF GET_COMM_TYPE (
                                  ipCommChar,         /* Код комиссии */
                                  vAcctRecid,         /* Идентификатор счета */
                                  vCommCur,           /* Код валюты */
                                  ipKauChar,          /* Код КАУ  */
                                  LnShPrm.balance[1], /* Минимальный остаток */
                                  0,                  /* Период/срок */
                                  ipBegDate)
                              THEN "="
                              ELSE "%".
          /* Уточним, не я вляется ли пени фиксированным */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
                  ipCommChar,         /* Код комиссии */
                  vAcctRecid,         /* Идентификатор счета */
                  vCommCur,           /* Код валюты */
                  ipKauChar,          /* Код КАУ  */
                  LnShPrm.balance[1], /* Мин остаток */
                  0,                  /* Период/срок */
                  ipBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.


         ASSIGN
            vDate = LnShPrm.Since
            vLog  = LnShPrm.OtherCom
            vSumm = LnShprm.Balance[1]
            .

         /* Корректировка/создание %% ставки */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?,
                         vRateDec,
                         vCommTypeChar,
                         LnShPrm.OtherCom,
                         1,
                         ?,
                         ?).

         /* Узнаем дату изменения СТАВКИ / ТАРИФА */
         IF ipCodPar = 1 THEN
         DO:
            FIND FIRST bLnShPrm WHERE
                       bLnShPrm.since GT ipBegDate
            NO-ERROR.

            ASSIGN
               BegDate1 = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        "%Кред",
                        ipBegDate,
                        ipEndDate)
                      /*GET_NEXT_COMM_DATE (
                          "%Кред",            /* Код комиссии */
                          vAcctRecid,         /* Идентификатор счета */
                          vCommCur,           /* Код приведенной валюты */
                          ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                          vSumm,              /* Мин остаток */
                          0,                  /* Период/срок */
                          ipBegDate,
                          ipEndDate)*/

               BegDate2 = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        "%КрПов",
                        ipBegDate,
                        ipEndDate)
               .
                          /*GET_NEXT_COMM_DATE (
                          "%КрПов",           /* Код комиссии */
                          vAcctRecid,         /* Идентификатор счета */
                          vCommCur,           /* Код приведенной валюты */
                          ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                          vSumm,              /* Мин остаток */
                          0,                  /* Период/срок */
                          ipBegDate,
                          ipEndDate)
               .          */

            IF BegDate1 <> ? AND
               BegDate2 <> ? THEN
               ipBegDate = MIN(BegDate1,BegDate2).
            ELSE
            IF BegDate1 <> ? THEN
               ipBegDate = BegDate1.
            ELSE
            IF BegDate2 <> ? THEN
               ipBegDate = BegDate2.
            ELSE
              ipBegDate = ipEndDate + 1.

            IF ipBegDate < ipEndDate + 1 THEN DO:
               RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?, ?, ?,vLog,1,?,?).
             END.

            IF AVAIL bLnShPrm             AND
               bLnShPrm.since < ipBegDate THEN
               ipBegDate = bLnShPrm.since.
         END.
         ELSE
           ipBegDate = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        ipCommChar,
                        ipBegDate,
                        ipEndDate).

           /*GET_NEXT_COMM_DATE (
                        ipCommChar,         /* Код комиссии */
                        vAcctRecid,         /* Идентификатор счета */
                        vCommCur,           /* Код приведенной валюты */
                        ipKauChar,          /* Код КАУ ("" - по умолчанию) */
                        vSumm,              /* Мин остаток (0 - поумолчанию) */
                        0,                  /* Период/срок (0 - поумолчанию) */
                        ipBegDate,
                        ipEndDate).*/

         IF ipCodPar <> 1 THEN
            /* Корректировка даты начала периода */
            ipBegDate = IF ipBegDate EQ ?
                        THEN ipEndDate + 1
                        ELSE ipBegDate.

      END.
      /* Если параметров для поиска комиссий не найдено,
      ** то ищем их впереди */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
            NO-ERROR.

         /* А если их нет,
         ** то и не надо искать комиссию/тариф */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* Получение даты изменения параметра */
PROCEDURE GetChgDateParam:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* договора */
   DEF INPUT  PARAM iPrmChar  AS CHAR NO-UNDO. /* Список параметров*/
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* От какой даты смотреть */
   DEF INPUT  PARAM iLog      AS LOG  NO-UNDO. /* */
   DEF OUTPUT PARAM oChgDate  AS DATE NO-UNDO. /* Ближайшая первая дата движения */

   DEF VAR vDate    AS DATE NO-UNDO.
   DEF VAR vChgDate AS DATE NO-UNDO.

   DEF BUFFER xloan-int FOR loan-int.
   DEF BUFFER xl        FOR loan-int.

   RUN GetChgDate IN h_Loan
                  (iContract,
                   iContCode,
                   iPrmChar,        /* Список интересующих параметров */
                   iBegDate,        /* От какой даты смотреть */
                   OUTPUT vChgDate).

   IF LOOKUP(STRING(CodOstPar),iPrmChar) > 0 AND iLog THEN
   DO:
      FIND FIRST xloan-int WHERE
                 xloan-int.contract  = iContract
             AND xloan-int.cont-code = iContCode
             AND xloan-int.id-d      = 1
             AND xloan-int.id-k      = 2
             AND xloan-int.mdate     > iBegDate
         NO-LOCK NO-ERROR.

      RELEASE xl .

      IF AVAIL xloan-int THEN
         FIND FIRST xl WHERE
                    xl.contract  = iContract
                AND xl.cont-code = iContCode
                AND xl.id-d      = 2
                AND xl.mdate     = xloan-int.mdate
                AND xl.avt
            NO-LOCK NO-ERROR.
      IF     AVAIL xloan-int AND
         NOT AVAIL xl        THEN
         vChgDate = IF vChgDate <> ? THEN
                          MIN(xloan-int.mdate,vChgDate)
                       ELSE
                          xloan-int.mdate.
   END.
   IF LOOKUP(STRING(CodOstPar),iPrmChar) > 0 AND NOT iLog THEN
   DO:
      RUN GetFilOstDate(iBegDate,CodOstPar,OUTPUT vDate).

      vChgDate = IF vDate     <> ? AND
                    vChgDate  <> ?
                 THEN
                    MIN(vDate,vChgDate)
                 ELSE
                 IF vDate  <> ?
                 THEN
                    vDate
                 ELSE
                    vChgDate
                 .

   END.
   oChgDate = vChgDate.

END PROCEDURE.

/* Получение даты последнего изменения параметра */
PROCEDURE GetLastDateParam:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* договора */
   DEF INPUT  PARAM iPrmChar  AS CHAR NO-UNDO. /* Список параметров*/
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* От какой даты смотреть */
   DEF OUTPUT PARAM oChgDate  AS DATE NO-UNDO. /* Последняя дата движения */

   DEF VAR vI       AS INT64 NO-UNDO.
   DEF VAR vParCode AS INT64 NO-UNDO.
   DEF VAR vDate    AS DATE  NO-UNDO.

   DEF BUFFER xloan-int FOR loan-int.

/*   vDate = iBegDate. */
   /* Поиск последней дебетовой операции по движению одного из параметров */
   DO vI = 1 TO NUM-ENTRIES(iPrmChar):
      ASSIGN
         vParCode = INT64(ENTRY(vI, iPrmChar)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.
   
      FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract 
                            AND xloan-int.cont-code EQ iContCode
                            AND xloan-int.id-d      EQ vParCode
                            AND xloan-int.mdate     GT iBegDate
           NO-LOCK NO-ERROR.
      IF AVAIL xloan-int THEN
         ASSIGN
            vDate    = xloan-int.mdate
            iBegDate = xloan-int.mdate
         .
      FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract
                            AND xloan-int.cont-code EQ iContCode
                            AND xloan-int.id-k      EQ vParCode
                            AND xloan-int.mdate     GT iBegDate
           NO-LOCK NO-ERROR.
      IF AVAIL xloan-int THEN
         ASSIGN
            vDate    = xloan-int.mdate
            iBegDate = xloan-int.mdate
         .
      IF CodOstPar EQ vParCode THEN
      DO:
         FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract
                               AND xloan-int.cont-code EQ iContCode
                               AND xloan-int.id-d      EQ 1
                               AND xloan-int.id-k      EQ 2
                               AND xloan-int.mdate     GT iBegDate
              NO-LOCK NO-ERROR.
         IF AVAIL xloan-int THEN
            vDate = xloan-int.mdate.
      END.
   END.

   oChgDate = vDate.

END PROCEDURE.

PROCEDURE GET_DOLG_CORR:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iSince    AS DATE NO-UNDO.
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO.
   DEF INPUT PARAM iPlan     AS LOG  NO-UNDO.

   DEF VAR vRem       AS DEC  NO-UNDO.
   DEF VAR vCorr      AS DEC  NO-UNDO.
   DEF VAR vParam     AS DEC  NO-UNDO.
   DEF VAR vDb        AS DEC  NO-UNDO.
   DEF VAR vCr        AS DEC  NO-UNDO.
   DEF VAR vTotalCorr AS DEC  NO-UNDO.
   DEF VAR vLastCorr  AS DEC  NO-UNDO.

   DEF VAR  procn    AS CHAR NO-UNDO. /*Процедура расчета параметра*/
   DEF VAR s_params  AS CHAR  INIT "0,13,33,34" NO-UNDO. /* Коды параметров,
                                                        обрабатываемые спец. функциями */


   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER bterm-obl FOR term-obl.
   
   {empty ttCorr}

   FOR EACH term-obl WHERE
            term-obl.contract   = iContract
        AND term-obl.cont-code  = iContCode
        AND term-obl.idnt       = 3
        AND term-obl.end-date  >= iSince
        AND term-obl.end-date  <= iEndDate
   NO-LOCK:
   
   IF CodOstPar = 0  THEN
      RUN PARAM_0_NEW
         (iContract,       /* Тип договора */
         iContCode,        /* Номер договора */
         CodOstPar,        /* Код параметра */
         term-obl.end-date,/* Дата пересчета договора */
         OUTPUT vParam).   /* Значение параметра на дату пересчета договора */
             
   ELSE
   DO:
      procn =   IF LOOKUP (STRING(CodOstPar), s_params) NE 0 THEN 
                 ("param_" + ENTRY(LOOKUP (STRING(CodOstPar), s_params), s_params))
                ELSE 
                 "stndrt_param".

      RUN VALUE (procn)
         (iContract,         /* Тип договора */
          iContCode,         /* Номер договора */
          CodOstPar,         /* Код параметра */
          term-obl.end-date, /* Дата пересчета договора */
          OUTPUT vParam,     /* Значение параметра на дату пересчета договора */
          OUTPUT vDb,        /* Дебетовы оборот по операциям */
          OUTPUT vCr).       /* Кредитовый оборот по операциям */
   END.
  
   vRem = vParam - vLastCorr.

   FIND LAST bterm-obl WHERE
          bterm-obl.contract  EQ icontract 
      AND bterm-obl.cont-code EQ iContCode 
      AND bterm-obl.idnt      EQ 2 
      AND bterm-obl.end-date  LE term-obl.end-date
   NO-LOCK NO-ERROR.
    
   IF AVAIL bterm-obl THEN
    vCorr = vCorr + IF vRem - bterm-obl.amt-rub > 0
                         THEN vRem - bterm-obl.amt-rub
                         ELSE 0.

   CREATE ttCorr.
   ASSIGN
         ttCorr.end-date = term-obl.end-date
         ttCorr.Summa    = vParam - vCorr
         ttCorr.Corr     = vCorr
         vLastCorr       = vCorr
   .
   RELEASE ttCorr.
   END.

END PROCEDURE.

PROCEDURE CORR_DOLG_SUMM:

   DEF INPUT        PARAM iDate  AS DATE NO-UNDO.
   DEF INPUT-OUTPUT PARAM pSumma AS DEC  NO-UNDO.

   FIND LAST ttCorr WHERE ttCorr.end-date <= iDate NO-LOCK NO-ERROR.

   pSumma = IF AVAIL ttCorr
            THEN pSumma - ttCorr.Corr
            ELSE pSumma.
   RETURN.

END PROCEDURE.

PROCEDURE CORR_NEXT_DATE:

   DEF INPUT        PARAM iBegDate AS DATE NO-UNDO.
   DEF INPUT-OUTPUT PARAM pChgDate AS DATE NO-UNDO.

   FIND FIRST ttCorr WHERE ttCorr.end-date > iBegDate NO-LOCK NO-ERROR.

   IF pChgDate = ? AND AVAIL ttCorr THEN
      pChgDate = ttCorr.end-date.

   pChgDate = IF AVAIL ttCorr AND ttCorr.end-date < pChgDate
              THEN ttCorr.end-date
              ELSE pChgDate.
   RETURN.

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:20:58.768+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='ln-nach.i' */
/*prosign4Jx8ab2MPTsxFPJkKcs+oQ*/