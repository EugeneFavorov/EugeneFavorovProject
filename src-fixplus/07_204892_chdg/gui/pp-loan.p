{globals.i}

/* +++ pp-loan.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: PP-LOAN.P
      Comment: Инструменты для работы с табличкой loan и
               некоторые вещи по кредитным договорам.
   Parameters: нет
         Uses:
      Used by:
      Created: 01/11/01 Om
     Modified: 04.05.2004 16:48 KSV      (0029735) Добавлена процедура
                                         MakePureLoanInt.
     Modified: 05.05.2004 19:46 KSV      (0029735) Исправлена ошибка вызова
                                         Fill-SysMes.
     Modified: 14.07.2004 17:28 KSV      (0032906) В процедуру NextContCode
                                         добавлено сохранение текущего значения
                                         счетчика deal-id для его возможного
                                         восстановления.
     Modified: 12.10.2007 16:36 koch     <comment>
*/

FORM "~n@(#) pp-loan.p 1.0 Om 01/11/01"
WITH frame sccs-id stream-io width 250.

DEF NEW SHARED VAR mask AS CHAR NO-UNDO INIT ?.

&GLOB  CodPar 'КодОснДолг,КодОснПр'
{globals.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get lv}
{intrface.get count}
{intrface.get refer}
{intrface.get i254}
{brwdpdsr.def} /* инструменты для работы с ДР ДепДоср */
{pick-var.i}
{ln-dep.i}
{t-otch.i NEW} /* Объявление таблички otch1 */
{par_mass.i} /* Массив для расчета параметров договора */
{savepars.i}
{tt-transh.def NO-UNDO} /* таблица для разноски по траншам */

DEFINE TEMP-TABLE ttIndicate1 LIKE ttIndicate.
DEFINE TEMP-TABLE ttIndicate2 LIKE ttIndicate.

{pfuncdef 
   &NAME="Loan"
   &LIBDEF=TRUE
   &LibName="Библиотека ф-ций работы с договорами"
}


/*------------------------------------------------------------------------------
  Purpose:     Возвращает уникальный идентификатор сделки как номер ген.
               соглашения + порядковый номер.
  Parameters:  iContCode   - номер ген. соглашения (некий префикс с
                             которым формируется номер)
               iLenght     - длина номера
  Notes:
------------------------------------------------------------------------------*/
FUNCTION NextContCode RETURNS CHARACTER (iContCode AS CHAR,
                                         iLength   AS INT64 ):

   DEF VAR vCode AS INT64 NO-UNDO.

   DEF BUFFER signs FOR signs.

   vCode = NEXT-VALUE(deal-id).

   /* Commented by KSV: Сохраняем последнее значение счетчика в специальном
   ** доп. реквизите. Это необходимо на случай, если текущее значение счетчика
   ** будет потеряно и его необходимо будет восстановить. Других механизмов
   ** его восстановления, к сожалению, нет. Механизм допускает некоторую
   ** погрешность, но ей можно пренебречь */
   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON QUIT UNDO TR,LEAVE TR:
      FIND FIRST signs WHERE
         signs.file-name   = "sequence" AND
         signs.surrogate   = "deal-id"  AND
         signs.code        = "deal-id"  EXCLUSIVE NO-WAIT NO-ERROR.

      IF LOCKED signs THEN UNDO TR,LEAVE TR.

      IF NOT AVAILABLE signs THEN
      DO:
         CREATE signs.
         ASSIGN
            signs.file-name   = "sequence"
            signs.surrogate   = "deal-id"
            signs.code        = "deal-id" .
      END.
      signs.code-value  = STRING(vCode).
   END.  /* End of TR BLOCK */

   RETURN iContCode +
          IF iLength = 0 THEN
             STRING(vCode)
          ELSE
             FILL("0",iLength - LENGTH(STRING(vCode))) + STRING(vCode).

END FUNCTION.

{form.def}
{lshpr.pro}
{loan.pro}
{loanint.pro}
{loan-srk.pro}
{jloan_counters.i}
{clcprmdog.i} /* определение функции CalcPrmValue для ALL_PARAM */
{dtterm.i}

FUNCTION GetContCode RETURNS CHARACTER (iContract AS CHAR,iContCode AS CHAR):
   DEFINE VARIABLE vRet AS CHARACTER NO-UNDO INIT ?.
   {&GET-LOAN}
   vRet = IF {assigned loan.doc-num} THEN loan.doc-num ELSE loan.doc-ref.
   RETURN vRet.
END FUNCTION.
/* возвращает код параметра основного долга, функция необходимо исполь-
зовать в методах расчета %% по параметрам, так как они
все заточены под кредиты*/


FUNCTION GetParCode RETURNS INT64 (iClass AS CHAR,iCod AS CHAR):
  DEF VAR vCod AS INT64 NO-UNDO .
  vCod =  INT64(GetXattrInit(iClass,iCod)) NO-ERROR.
  IF ERROR-STATUS:ERROR OR vCod = ?
  THEN IF iCod = ENTRY(1,{&CodPar})
       THEN vCod = 0.
       ELSE vCod = 4.
  RETURN vCod .
END FUNCTION.

FUNCTION GetFstWrkDay RETURNS DATE (
   INPUT iClass-Code AS CHAR,     /* Класс объекта */
   INPUT iUser-id    AS CHAR,     /* Сотрудник */
   INPUT in-date     AS DATE,     /* Дата начала поиска */
   INPUT iDelay      AS INT64,  /* Количество дней интервала расчета */
   INPUT iDirect     AS INT64   /* Направление: < 0 - поиск назад от заданной даты
                                   **             > 0 - поиск р.д. вперед */
):

   DEF VAR vBranch   AS CHAR        NO-UNDO.   /* подразделение из (по приоритету) договора/осн.счета/пользователя */
   DEF VAR vLoanGr   AS CHAR        NO-UNDO.   /* график работы с данным классом договоров */
   DEF VAR vi        AS INT64         NO-UNDO.   /* счетчик цикла */
   DEF VAR vResDate  AS DATE        NO-UNDO.   /* возвращаемое значение */

   /* Найдем подразделение пользователя вводившего договор */
   vBranch = GetXAttrValue("_user", iUser-id, "Отделение").
   /* Определим график работы с данным классом договоров */
   vLoanGr = GetXAttrInit(iClass-Code, "WorkGraf").

   CYCLE:
   DO vi = 0 TO iDelay:
      vResDate = in-date + vi * iDirect.
      IF        {holiday.i vResDate}                /* выходн. по базовому */
         OR NOT IsWorkDayBranch(vResDate, vBranch)  /* или NO(выходн.) по граф. подразд.*/
         OR NOT IsWorkDayGraf(vResDate, vLoanGr)    /* или NO(выходн.) по графику договора*/
      THEN DO:
         vResDate = ?.
         NEXT CYCLE.     /* переходим к следующей дате */
      END.
      ELSE LEAVE CYCLE.
   END.  /* of CYCLE block */

   /* Если попытка поиска не принесла результата то возвращаем нач. дату */
   vResDate = IF vResDate EQ ? THEN in-date ELSE vResDate.
   RETURN vResDate.
END FUNCTION.

/*!!! при подъеме в стандарт перенести в pp-loan !!! (перенесли!) */
FUNCTION GetPercentRate RETURNS DECIMAL
   (iDscBegDate AS DATE,     /* Начало периода дисконтирования */
    iDscEndDate AS DATE,     /* Окончание периода дисконтирования */
    iDscRate    AS DECIMAL): /* Ставка дисконтирования */

   DEFINE VARIABLE vDiscBeg AS DATE    NO-UNDO.
   DEFINE VARIABLE vDiscEnd AS DATE    NO-UNDO.
   DEFINE VARIABLE vDiscTmp AS DATE    NO-UNDO.
   DEFINE VARIABLE vDays    AS INT64 NO-UNDO.
   DEFINE VARIABLE vYear    AS INT64 NO-UNDO.
   DEFINE VARIABLE vPerCent AS DECIMAL NO-UNDO.

   ASSIGN
      vDiscBeg = iDscBegDate + 1
      vDiscEnd = iDscEndDate
      .
   DO WHILE vDiscBeg LE vDiscEnd:
      ASSIGN
         vYear    = YEAR(vDiscBeg)
         vDiscTmp = DATE(12,31,vYear)
         vDays    = vDiscTmp - DATE(1,1,vYear) + 1
         .
      IF vDiscTmp GT vDiscEnd THEN vDiscTmp = vDiscEnd.
      vPerCent = vPerCent + iDscRate * (vDiscTmp - vDiscBeg + 1) / vDays.
      IF vDiscBeg EQ vDiscEnd THEN LEAVE.
      vDiscBeg = vDiscTmp + 1.
   END.

   RETURN vPerCent.

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Создает внутреннюю операции на договоре без привязке к бухг.
               проводке
  Parameters:  iContract   - назначение договора
               iContCode   - идентификатор договра
               iCodOp      - код операции
               iDate       - дата операции
               iAmount     - сумма операции
               iAvt        - признак авт. операции
               oOk         - флаг завершения
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE MakePureLoanInt:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCodOp    AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmount   AS DECIMAL    NO-UNDO.
   DEFINE INPUT  PARAMETER iAvt      AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk       AS LOGICAL    .

   DEFINE BUFFER loan-int FOR loan-int.

   DEFINE VARIABLE incontr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cur-date   AS DATE       NO-UNDO.
   DEFINE VARIABLE vOperMask  AS CHARACTER  NO-UNDO.

   ASSIGN
      incontr  = iContract
      cur-date = iDate.

   {&GET-LOAN}

   IF loan.open-date > iDate THEN
   DO:
      RUN Fill-SysMes("","16L","","%s=" + iContract      +
                                  "%s=" + iContCode).
      RETURN.
   END.

   vOperMask = GetXattrInit(loan.class-code,"СписОпер").

   IF NOT {assigned vOperMask} THEN vOperMask = "*".
   IF NOT CAN-DO(vOperMask,STRING(iCodOp)) THEN
   DO:
      RUN Fill-SysMes("","13L","","%s=" + STRING(iCodOp) +
                                  "%s=" + iContract      +
                                  "%s=" + iContCode).
      RETURN.
   END.


   FIND FIRST chowhe WHERE
      chowhe.id-op = iCodOp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE chowhe THEN
   DO:
      RUN Fill-SysMes("","14L","","%s=" + STRING(iCodOp)).
      RETURN.
   END.

   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON QUIT UNDO TR,LEAVE TR:
      {cr-lint.i
         &df    = "/*"
         &id-d  = "chowhe.id-d"
         &id-k  = "chowhe.id-k"
         &s     = "iAmount"
         &avt   = "iAvt"
         &ruch  = "/*"
         &off   = "/*"
       }
       ASSIGN
          loan-int.user-id = USERID("bisquit").
      oOk = YES.
   END.  /* End of TR BLOCK */

   IF oOk <> YES THEN
      RUN Fill-SysMes("","15L","","%s=" + STRING(iCodOp) +
                                  "%s=" + iContract      +
                                  "%s=" + iContCode).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:    По Балансовому счету ищет все договора,
              в которых счет основного долга принадлежит этому балансовому,
              после чего для каждого договора ищется счет с указанной
              ролью, из поученных найденных счетов составляется список
              балансовых счетов, который возвращается.

              Рассматриваются договора в зависимости от назначения.

  Parameters:  iOrigBalAcct - балансовый счет основного долга
               iLoanPurpose - назначение раззматриваемых договоров
               iAcctRole    - искомая роль
               oBalAcctList - полученный список балансовых счетов

  Notes:
------------------------------------------------------------------------------*/
PROCEDURE GetBalAcctsOfRole:
DEFINE INPUT  PARAMETER iOrigBalAcct  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iLoanContract AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iAcctRole     AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate       AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oBalAcctList  AS CHAR NO-UNDO INIT "".

DEFINE BUFFER b-acct       FOR acct.
DEFINE BUFFER bb-acct      FOR acct.
DEFINE BUFFER b-loan-acct  FOR loan-acct.
DEFINE BUFFER bb-loan-acct FOR loan-acct.
DEFINE BUFFER b-loan       FOR loan.

DEFINE VAR vRoleList AS CHAR NO-UNDO INIT "".

   IF iLoanContract = "dps" THEN
     vRoleList = "loan-dps-p,loan-dps-t,loan-dps-ts,loan-dps-tsk".

   FOR EACH b-acct      WHERE b-acct.bal-acct      =  INT64(iOrigBalAcct) NO-LOCK,
       EACH b-loan-acct WHERE b-loan-acct.acct     =  b-acct.acct
                          AND b-loan-acct.currency =  b-acct.currency
                          AND b-loan-acct.contract =  iLoanContract         NO-LOCK:
       /* сразу отсекаем ненужные счета (с неподходящей ролью) */
       IF iLoanContract = "dps" THEN DO:
          IF NOT CAN-DO(vRoleList,b-loan-acct.acct-type) THEN NEXT.
       END.
       ELSE DO:
          IF NOT CAN-DO(GetMainAcctRole(b-loan-acct.contract, b-loan-acct.cont-code),
                        b-loan-acct.acct-type) THEN NEXT.
       END.

       FIND FIRST b-loan OF b-loan-acct
                         /* Незакрытый договор */
                         WHERE b-loan.open-date  <= iOpDate
                           AND (b-loan.close-date = ? OR b-loan.close-date >= iOpDate)
                         NO-LOCK NO-ERROR.
       IF NOT AVAILABLE b-loan THEN NEXT.

       /* Теперь договор нам точно подходит -
          ищем счет с ролью iAcctRole и его балансовый счет  */
       FIND LAST bb-loan-acct OF    b-loan
                              WHERE bb-loan-acct.acct-type =  iAcctRole
                                AND bb-loan-acct.since     <= iOpDate
                              NO-LOCK NO-ERROR.
       IF NOT AVAILABLE bb-loan-acct THEN NEXT.
       FIND FIRST bb-acct OF bb-loan-acct NO-LOCK NO-ERROR.
       IF NOT AVAILABLE bb-acct THEN NEXT.

       IF LOOKUP(STRING(bb-acct.bal-acct,'99999'),oBalAcctList) <> 0 THEN NEXT.

       {additem.i oBalAcctList STRING(bb-acct.bal-acct,'99999')}
       oBalAcctList = oBalAcctList + "," + b-loan.cont-code.

   END. /* for each b-acct, each b-loan-acct */

END PROCEDURE. /* END OF PROCEDURE GetBalAcctOfRole */

/*------------------------------------------------------------------------------
  Purpose:     Флаг необходимости округления платежей
  Parameters:  iContract - Идентификатор
               iContCode - договора
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetRoundFlag RETURNS LOG
   (iContract AS CHAR,
    iContCode AS CHAR):

   DEF VAR vStr AS CHAR NO-UNDO.

   {&GET-LOAN}

   vStr = GetXAttrValueEx("loan",iContract + "," + iContCode,"round",?).
   IF vStr <> ? THEN
      RETURN (vStr = "Да").
   ELSE
   DO:
      vStr = FGetSetting("Округ",?,?).
      IF vStr <> ?
      THEN
         RETURN ( IF loan.currency EQ ""
                 THEN INDEX(vStr,"НацВал")      <> 0
                 ELSE INDEX(vStr,loan.currency) <> 0).
      ELSE
         RETURN(NO).
   END.

END FUNCTION.

/* Поиск действующей схемы по договору на дату */
PROCEDURE GET_CURRENT_LOAN_SCHEME.

   DEF INPUT  PARAM iScheme AS CHAR          NO-UNDO. /* код схемы начисления */
   DEF INPUT  PARAM iDate   AS DATE          NO-UNDO. /* Дата */
   DEF OUTPUT PARAM oSchRid AS RECID INIT ?  NO-UNDO. /* RecId схемы */

   DEF BUFFER shc_line FOR interest-sch-line.

   FOR EACH shc_line WHERE
            shc_line.interest-sch EQ iScheme
        AND shc_line.since        LE iDate
   NO-LOCK BY shc_line.since  DESC :  /* СОРТИРОВКА ВАЖНА , find last брал по ключю а там еще номер счета  учитывался !!! */
      oSchRid = RECID(shc_line).
      LEAVE.
   END.
END PROCEDURE.

/* Поиск даты окончания текущей схемы (поиск следующей схемы) */
PROCEDURE GET_NEXT_DATE_LOAN_SCHEME.
   DEF INPUT  PARAM iScheme AS CHAR        NO-UNDO. /* код схемы начисления */
   DEF INPUT  PARAM iDate   AS DATE        NO-UNDO. /* Дата */
   DEF OUTPUT PARAM oDate   AS DATE INIT ? NO-UNDO. /* RecId схемы */

   DEF BUFFER shc_line FOR interest-sch-line.
   /* пока не выяснил что быстрее
   FOR EACH  shc_line where
             shc_line.interest-sch = iScheme
         AND shc_line.since        > iDate
   NO-LOCK BY shc_line.since:
      oDate = shc_line.since.
      LEAVE.
   END.
   */
   FOR FIRST shc_line WHERE
             shc_line.interest-sch = iScheme
         AND shc_line.since        > iDate
   NO-LOCK:
      oDate = shc_line.since.
   END.
END PROCEDURE.

PROCEDURE GET_COMM_LOAN_BUF:

   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iComm      AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.
   DEF PARAM BUFFER comm-rate FOR comm-rate.

   RELEASE comm-rate.

   FOR LAST comm-rate WHERE
            comm-rate.commission = iComm
        AND comm-rate.acct       = "0"
        AND comm-rate.kau        = iContract + "," + iContCode
        AND comm-rate.since     <= iDate
      USE-INDEX kau NO-LOCK:
      RETURN.
   END.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Поиск действующей комиссии по Кредитному договору
  Parameters:  iContract - Идентификатор
               iContCode - договора
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GET_COMM_LOAN RETURNS DEC
  (INPUT iContract  AS CHAR,   /* Идентификатор */
   INPUT iContCode  AS CHAR,   /* договора      */
   INPUT iComm      AS CHAR,   /* Код комиссии. */
   INPUT iDate      AS DATE):  /* Дата поиска.  */

   DEF BUFFER comm-rate FOR comm-rate.

   RUN GET_COMM_LOAN_BUF(iContract,iContCode,iComm,iDate,BUFFER comm-rate).

   RETURN ( IF AVAIL  comm-rate
           THEN  comm-rate.rate-comm
           ELSE ?).

END FUNCTION.
/*------------------------------------------------------------------------------
  Purpose:     Поиск действующей комиссии по Кредитному договору
  Parameters:  iContract - Идентификатор
               iContCode - договора
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GET_NEXT_COMM_DATE_LOAN RETURNS DATE
  (INPUT iContract  AS CHAR,   /* Идентификатор */
   INPUT iContCode  AS CHAR,   /* договора      */
   INPUT iComm      AS CHAR,   /* Код комиссии. */
   INPUT iBegDate   AS DATE,   /*  Интервал    */
   INPUT iEndDate   AS DATE):  /*  поиска.     */

   DEF BUFFER comm-rate FOR comm-rate.

   FOR FIRST comm-rate WHERE
             comm-rate.commission = iComm
         AND comm-rate.acct       = "0"
         AND comm-rate.kau        = iContract + "," + iContCode
         AND comm-rate.since      > iBegDate
         AND comm-rate.since     <= iEndDate
   USE-INDEX kau NO-LOCK:
      RETURN comm-rate.since.
   END.
   RETURN ?.

END FUNCTION.

FUNCTION GET_MAIN_COMM_RATE RETURNS DEC
  (INPUT iContract  AS CHAR,   /* Идентификатор */
   INPUT iContCode  AS CHAR,   /* договора      */
   INPUT iComm      AS CHAR,   /* Код комиссии. */
   INPUT iDate      AS DATE):  /* Дата поиска.  */

   DEF VAR vRec AS RECID NO-UNDO .
   DEF BUFFER comm-rate FOR comm-rate.
   DEF BUFFER loan FOR loan.

   vRec = GetLoanRecid(iContract,iContCode).
   RUN GetMainLnId(INPUT vRec, OUTPUT vRec) .
   FIND loan WHERE RECID(loan) = vRec NO-LOCK NO-ERROR.
   IF AVAIL loan
   THEN
   RUN GET_COMM_LOAN_BUF(loan.contract,loan.cont-code,iComm,iDate,BUFFER comm-rate).

   RETURN ( IF AVAIL  comm-rate
           THEN  comm-rate.rate-comm
           ELSE ?).

END FUNCTION.

FUNCTION getNextCommDateTransh RETURNS DATE
     (INPUT iContract  AS CHAR,
      INPUT iContCode  AS CHAR,
      INPUT iComm      AS CHAR,
      INPUT iBegDate   AS DATE,
      INPUT iEndDate   AS DATE):

   DEFINE VARIABLE vRetVal AS DATE       NO-UNDO.

   vRetVal = GET_NEXT_COMM_DATE_LOAN (iContract,
                                      iContCode,
                                      iComm,
                                      iBegDate,
                                      iEndDate).

   IF vRetVal EQ ? AND NUM-ENTRIES (iContCode," ") GT 1 THEN
      vRetVal = GET_NEXT_COMM_DATE_LOAN (iContract,
                                         ENTRY (1, iContCode, " "),
                                         iComm,
                                         iBegDate,
                                         iEndDate).
   RETURN vRetVal.
END FUNCTION.

FUNCTION getCommRateTransh RETURNS DECIMAL
   (INPUT iContract AS CHAR,
    INPUT iContCode AS CHAR,
    INPUT iComm     AS CHAR,
    INPUT iSince    AS DATE):

   DEFINE VARIABLE vRetVal AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vRecid  AS RECID      NO-UNDO.
   DEFINE VARIABLE vSurr   AS CHARACTER  NO-UNDO.
   /* Определяет комиссию на текущем договоре */
   vRetVal = get_Comm_Loan (iContract,
                            iContCode,
                            iComm,
                            iSince).
   /* Комиссия на охватывающем договоре */
   IF vRetVal EQ ?
   THEN vRetVal = get_Main_Comm_Rate (iContract,
                                      iContCode,
                                      iComm,
                                      iSince).
   RETURN vRetVal.
END FUNCTION.

/* функция определяет переход на расчет процентов  с компенсцией
округления */
FUNCTION MovRound RETURN DATE:
 DEF VAR vDate AS DATE NO-UNDO.
 vDate = DATE(FGetSetting("MoveRound",?,"01/01/1970")) NO-ERROR.
 IF vDate = ?
 THEN RETURN 01/01/1970.
 ELSE RETURN vDate .
END FUNCTION .

PROCEDURE MovRoundLoan.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iClassCode AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oMoveRound AS DATE NO-UNDO.
   DEF OUTPUT PARAM oKompPogr  AS CHAR NO-UNDO.

   DEF VAR vMoveRound AS CHAR NO-UNDO.

   vMoveRound = GetXattrValueEx("loan",
                                iContract + "," + iContCode,
                                "MoveRound",
                                "").
   IF vMoveRound EQ "" THEN
      vMoveRound = GetXattrInit(iClassCode,
                                "MoveRound").
   IF vMoveRound EQ "" THEN
   DO:
      oMoveRound = MovRound().
   END.
   ELSE
   DO:
      oMoveRound = DATE(vMoveRound) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oMoveRound = 01/01/1970.
   END.
   oKompPogr = GetXattrValueEx("loan",
                                iContract + "," + iContCode,
                                "КомпПогр",
                                "").
   IF oKompPogr EQ "" THEN
   DO:
      oKompPogr = GetXattrInit(iClassCode,
                               "КомпПогр").
      IF oKompPogr EQ "" THEN
         oKompPogr = FGetSetting("КомпПогр",?,"").
   END.
END PROCEDURE.

/* Метод создает привязку проводки к текущему договору, если это возможно */
PROCEDURE LinkOpEntryMeth.
   DEF INPUT PARAM iOpEntry         AS RECID       NO-UNDO.
   DEF INPUT PARAM iContract        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iContCode        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCodeInt         AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iSide            AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCurrentLinkType AS CHARACTER   NO-UNDO.

   DEF BUFFER op        FOR op.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER op-entry  FOR op-entry.

   DEF VAR vContractDate  AS DATE NO-UNDO.   /* Плановая дата документа. */

   FIND FIRST op-entry WHERE RECID(op-entry) EQ INT64(iOpEntry) NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN
      FIND FIRST loan WHERE
                           loan.contract  EQ iContract
                     AND   loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
      FIND LAST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
   
   IF AVAIL op THEN
   IF     IsBindEarlier(op.contract-date, loan.since)
      AND IsDateBanSince(loan.contract, loan.cont-code , op.contract-date)
   THEN
   DO TRANSACTION
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* Получение плановой даты операций. */
      RUN UpdContract (iContract,               /* Назначение договора. */
                       iContCode,               /* Номер договора. */
                       op.contract-date,        /* Плановая дата документа. */
                       iCodeInt,                /* Операция */
                       OUTPUT vContractDate).   /* Верная дата документа. */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN UNDO, LEAVE.

      /* Подтверждение ввода документа. */
      RUN ConfirmAction (
         RECID (op),                   /* Идентификатор договора. */
         INPUT-OUTPUT vContractDate).  /* Плановая дата документа. */

      IF {&RETURN_VALUE} NE ""
      THEN UNDO, LEAVE.

      /* Корректировка плановой даты операции. */
      RUN SetLIntDate (vContractDate).

      /* Пересчет договора на подтвержденную плановую дату. */
      IF vContractDate NE loan.since
      THEN RUN LoanCalc (iContract,       /* Назначение договора. */
                         iContCode,       /* Номер договора. */
                         vContractDate).  /* Пдановая дата докеумента. */

      IF vContractDate <> DATE(GetBufferValue("loan","WHERE loan.contract = '" + iContract + "'
                                                        AND loan.cont-code = '" + iContCode + "'",
                                              "since")) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", "Дата проводки не равна " +
            "дате пересчета состояния договора. При привязке возможны ошибки. " +
            "Пересчитайте договор.").
         UNDO, RETRY.
      END.

      /* Вызов метода проверки операции из метасхемы.
      ** Делаем непосредственно перед созданием операции,
      ** что-бы проверки были после подтверждения суммы и плановой даты */
      RUN RunChkMethod (iContract,        /* Назначение договора. */
                        iContCode,        /* Номер договора. */
                        RECID(op-entry),  /* Идентификатор проводки. */
                        iCodeInt).        /* Код операции внесистемного учета. */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /* Формирование перечня операци для создания.
      ** Определение суммы и валюты операций. */
      RUN GetOper (RECID (op-entry),   /* Идентификатор проводки. */
                  iContract,           /* Назначение договора. */
                  iContCode,           /* Номер договора. */
                  vContractDate,       /* Плановая дата док-а. */
                  iCodeInt,            /* Код вида операции. */
                  YES).                /* Ищем парную операцию */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /*Если операции подобраны, но все нулевые, то сообщение пользователю и аналитику не создаем*/
      FOR EACH lInt BY lInt.Amt DESC:
         LEAVE.
      END.
      IF (AVAIL lInt) AND (lInt.Amt LE 0) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0",
             "Привязка не может быть выполнена,т.к параметр нулевой. Проверьте предыдущие операции.").
         UNDO, LEAVE.
      END.

      /* Создание операции. */
      RUN CreLInt (iContract,       /* Назначение договора. */
                   iContCode).      /* Номер договора. */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO,  RETRY.
      END.

      /* Создание субаналитики по проводке. */
      RUN CreEntryKau(RECID(op-entry), /* Идентификатор проводки. */
                     iContract,        /* Назначение договора. */
                     iContCode,        /* Номер договора. */
                     iCodeInt,         /* Код вида операции. */
                     LOGICAL(iSide),   /* Сторона счета Yes - кредит. */
                     vContractDate).   /* Верная плановая дата документа. */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         IF {&RETURN_VALUE} BEGINS "lock"
         THEN RUN wholock(IF {&RETURN_VALUE} EQ "lock_op"
                          THEN RECID (op)
                          ELSE RECID(op-entry), "").

         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
            UNDO, RETRY.
         END.
      END.
   END.

END PROCEDURE.

/* Метод удаляет привязку проводки к текущему договору */
PROCEDURE UnLinkOpEntryMeth.
   DEF INPUT PARAM iOpEntry         AS RECID       NO-UNDO.
   DEF INPUT PARAM iContract        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iContCode        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCodeInt         AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iSide            AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCurrentLinkType AS CHARACTER   NO-UNDO.

   DEF BUFFER op        FOR op.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER op-entry  FOR op-entry.

   FIND FIRST op-entry WHERE RECID(op-entry) EQ INT64(iOpEntry) NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN
      FIND FIRST loan WHERE
                           loan.contract  EQ iContract
                     AND   loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      
   IF AVAIL loan THEN
      FIND LAST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
      
   /* Проводка привязана, желаем отвязать. */
   IF AVAIL op THEN    
   IF     IsBindEarlier(op.contract-date, loan.since)
        AND IsDateBanSince(loan.contract, loan.cont-code , op.contract-date)
   THEN
   DO TRANSACTION
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* Пересчет договора на плановую дату документа. */
      IF op.contract-date LT loan.since
      THEN RUN LoanCalc (iContract,          /* Назначение договора. */
                         iContCode,          /* Номер договора. */
                         op.contract-date).  /* Пдановая дата докеумента. */

      /* Удаление операции. */
      RUN DelLInt (op-entry.op,
                  op-entry.op-entry,
                  iContract,
                  iContCode,
                  iCodeInt).

      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /* Удаление субаналитики по проводке. */
      RUN DelEntryKau(RECID(op-entry), /* Идентификатор проводки. */
                     iContract,        /* Назначение договора. */
                     iContCode,        /* Номер договора. */
                     iCodeint,         /* Код вида операции. */
                     LOGICAL(iSide)).  /* Сторона счета Yes - кредит. */

      /* Обработка ошибки. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         IF {&RETURN_VALUE} BEGINS "lock"
         THEN RUN wholock(IF {&RETURN_VALUE} EQ "lock_op"
                          THEN RECID (op)
                          ELSE RECID(op-entry), "").

         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
            UNDO, LEAVE.
         END.
      END.
   END.

END PROCEDURE.


   /* Метод автоматической генерации номера кредитного договора */
PROCEDURE GetNumLoan.
   DEF INPUT  PARAM iClassCode    AS CHAR NO-UNDO. /* Класс договора    */
   DEF INPUT  PARAM iDate         AS DATE NO-UNDO. /* Базовая дата      */
   DEF INPUT  PARAM iBranch-id    AS CHAR NO-UNDO. /* Код филиала и все необходимые теги  */
   DEF INPUT  PARAM iGenNum       AS LOG  NO-UNDO. /* Обязательно генерировать номер ? */
   DEF OUTPUT PARAM oNum          AS CHAR NO-UNDO. /* Номер договора    */
   DEF OUTPUT PARAM oCounterValue AS INT64  NO-UNDO. /* Значение счетчика */

   DEF VAR vAutoGenNum AS CHAR NO-UNDO. /* Знач-е статич.рекв. "AutoGenNum" */
   DEF VAR vTemplate   AS CHAR NO-UNDO. /* Маска номера */
   DEF VAR vCounter    AS CHAR NO-UNDO. /* Код счетчика */
   DEF VAR oNumCD      AS CHAR NO-UNDO . /* используемая часть номера кредитного договора */
   DEF VAR vNumRegion  AS CHAR NO-UNDO . /* Код региона из ОКАТО - 2 первых символа */
   DEF VAR vNumOffis   AS CHAR NO-UNDO . /* Номер Офиса банка - 2 последних символа */
   DEF VAR vTmpStr     AS CHAR NO-UNDO.
   DEF VAR vTmpStr2    AS CHAR NO-UNDO.
   DEF VAR vTmpStr3    AS CHAR NO-UNDO.
   DEF VAR vTmpStrT    AS CHAR NO-UNDO.
   DEF VAR vNumDogOb   AS CHAR NO-UNDO.
   DEF VAR vSurr       AS CHAR NO-UNDO.
   DEF VAR vList       AS CHAR NO-UNDO.
   DEF VAR vTmpInt     AS INT64 NO-UNDO.
   DEF VAR vTmpInt2    AS INT64 NO-UNDO.
   DEF VAR vI          AS INT64 NO-UNDO.

   /*Переменные извлекаемые из iBranch-id, по мимо самого Branch-id                 */
   DEF VAR vCountract  AS CHAR NO-UNDO INIT "" . /* 2 -  Кредитный договор          */
   DEF VAR vCounCode   AS CHAR NO-UNDO INIT "" . /* 3 -  Кредитный договор          */
   DEF VAR vObType     AS CHAR NO-UNDO INIT "" . /* 4 -  Тип договора обеспечения   */
   DEF VAR vLoanType   AS CHAR NO-UNDO INIT "" . /* 5 -  Тип договора loan          */
   /*-------------------------------------------------------------------------------*/
   DEF VAR proc-name   AS CHAR NO-UNDO .   
   DEF VAR params      AS CHAR NO-UNDO .   
   DEF BUFFER bloan FOR loan.

      /* Получаем значение статического реквизита "AutoGenNum" */
   vAutoGenNum = GetXAttrInit (iClassCode, "AutoGenNum").
   IF    vAutoGenNum EQ "ДА"
      OR iGenNum THEN
   DO:

      /* Подготовка переменных  к использованию */

      IF NUM-ENTRIES(iBranch-id,"|") = 5
      THEN
         ASSIGN
         vCountract = ENTRY(2,iBranch-id,"|" )
         vCounCode  = ENTRY(3,iBranch-id,"|" )
         vObType    = ENTRY(4,iBranch-id,"|" )
         vLoanType  = ENTRY(5,iBranch-id,"|" )
         .
      iBranch-id = ENTRY(1,iBranch-id,"|" ) .

      vObType = GetCodeEx ('СуффДогОб',vObType,"") .
      vLoanType = GetCodeEx ('СуффДог',vLoanType,"") .
      RUN GetClassMethod IN h_xclass (iClassCode, 
                                      "GetLoanNumSb", 
                                      "", 
                                      "",
                                      OUTPUT proc-name,
                                      OUTPUT params).                                                                           
      IF {assigned proc-name} THEN 
         RUN RunClassMethod IN h_xclass (iClassCode, 
                                         "GetLoanNumSb", 
                                         "", 
                                         "", 
                                         ?,
                                         vCountract + "," + vCounCode + CHR(1)).
      IF {&RETURN_VALUE} NE "" THEN                                                                     
         oNumCD = {&RETURN_VALUE}.                                                                       
      vNumRegion = SUBSTRING( GetXAttrValue("branch", iBranch-id, "БанкОКАТО") ,1 ,2 ).
      vNumOffis  = SUBSTRING( iBranch-id , length(iBranch-id) - 1 , 2) .

         /* Определяем маску номера и код счетчика */
      RUN GetClassTemplatePars (iClassCode,
                                OUTPUT vTemplate,
                                OUTPUT vCounter).
                                                                 
      vList = CHR(92) + "," + CHR(47) + "," + CHR(45) + "," + CHR(95).
      CYCLE:                                                          
      DO vI = 1 TO NUM-ENTRIES(vList):
         oNumCD = Replace(oNumCD,ENTRY(vI,vList),"").
      END.
      ASSIGN
         iBranch-id    = IF iBranch-id EQ ? THEN "" ELSE iBranch-id         /* Проверим на "пустое" значение кода филиала */
         oNum          = ReplaceBasicTags (vTemplate, vCounter, iDate)      /* Подставляем в шаблон тэги номера и даты */
         oCounterValue = GetCounterCurrentValue (vCounter, iDate)      /* Получаем текущее значение счетчика,
                                                                       ** не менять местами строки т.к. ReplaceBasicTags
                                                                       ** в предыдущей строке двигает тот же счетчик на 1 .*/
         oNum          = ReplaceTag (oNum, "н", STRING(oCounterValue), NO)    /* Подставляем в шаблон номер договора  русская буква*/
         oNum          = ReplaceTag (oNum, "ф", iBranch-id, YES)              /* Подставляем в шаблон код подразделения русская буква*/
         oNum          = ReplaceTag (oNum, "l", oNumCD, NO)                   /* Подставляем в шаблон код из кредитного договора         латиница*/
         oNum          = ReplaceTag (oNum, "р", vNumRegion,NO)                /* Подставляем в шаблон код региона  русская буква */
         oNum          = ReplaceTag (oNum, "о", vNumOffis,YES)                 /* Подставляем в шаблон код офиса    русская буква*/
      .
      
      IF vObType NE "" THEN DO:    
         oNum = ReplaceTag (oNum, "v", vObType, NO).                   /* Подставляем в шаблон код из типа договора обеспечения латиница*/
         IF INDEX(oNum,"с") NE 0 THEN 
         DO:                              /* тэг "с" порядковый номер */
            vTmpInt = oCounterValue.
            vTmpStrT = SUBSTRING (oNum, INDEX(oNum, "[с") + 1, INDEX(oNum, "с]") - INDEX(oNum, "[с")).
            IF LENGTH(STRING(vTmpInt + 1)) GE LENGTH(vTmpStrT) THEN
            oNum = ReplaceTag (oNum, "с", STRING(vTmpInt + 1), NO). 
            ELSE 
            DO:
               vTmpStr = SUBSTRING (oNum, 1, INDEX(oNum, "[с") - 1).
               vTmpStr2 = SUBSTRING (oNum, INDEX(oNum, "с]") + 2).
               oNum = vTmpStr + FILL("0", (LENGTH(vTmpStrT) - LENGTH(STRING(vTmpInt + 1)))) + STRING(vTmpInt + 1) + vTmpStr2.
            END.
         END.
      END.
      ELSE
         SUBSTRING (oNum, INDEX(oNum, "-[v"), INDEX(oNum, "v]") - INDEX(oNum, "-[v") + 2) = "" NO-ERROR.         
      IF vLoanType NE "" THEN     
         oNum = ReplaceTag (oNum, "t", vLoanType, NO).                   /* Подставляем в шаблон код из типа договора латиница*/
      ELSE
         SUBSTRING (oNum, INDEX(oNum, "-[t"), INDEX(oNum, "t]") - INDEX(oNum, "-[t") + 2) = "" NO-ERROR.       
   END.
END PROCEDURE.

/* Определение ставки НДФЛ и статуса клиента (резидент/нерезидент) по счету */
PROCEDURE GetStaffByAcct.
   DEF INPUT  PARAM iAcct AS CHAR   NO-UNDO. /* счет */
   DEF INPUT  PARAM iCurr AS CHAR   NO-UNDO. /* валюта */
   DEF INPUT  PARAM iDate AS DATE   NO-UNDO. /* дата для определения ставки */
   DEF OUTPUT PARAM oRate AS DEC    NO-UNDO. /* ставка НДФЛ */
   DEF OUTPUT PARAM oStat AS CHAR   NO-UNDO. /* статус клиента */

   DEF VAR vTax AS CHAR   NO-UNDO.
   DEF BUFFER acct FOR acct.

   MAIN_BLOCK:
   DO:
      oStat = ?.
      /* поиск счета */
      FIND FIRST acct WHERE acct.acct EQ iAcct
                        AND acct.curr EQ iCurr
      NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         /* определение статуса по номеру счета 1-го порядка */
         oStat = GetCode("СтатусПоБалСчт",SUBSTRING(STRING(acct.bal-acct),1,3) + "*").
      /* если не нашли соответсвие в классификаторе, считаем что это резидент */
      IF oStat EQ ? THEN oStat = "резидент".
      /* определение кода ставки по статусу */
      vTax = GetCode("НалогПоСтатус",oStat).
      /* определение значения ставки по коду */
      /*oRate = GET_COMM(vTax,
                       RECID(acct),
                       iCurr,
                       "",0,0,
                       iDate).*/
      FIND LAST comm-rate WHERE comm-rate.since       LE iDate
                            AND comm-rate.filial-id = shfilial
                            AND comm-rate.branch-id = ""
                            AND comm-rate.acct        EQ '0'
                            AND comm-rate.commission  EQ vTax
                            AND comm-rate.currency    EQ ''
                            AND comm-rate.min-value   EQ 0
                            AND comm-rate.period      EQ 0
      NO-LOCK NO-ERROR.
      IF AVAIL comm-rate THEN oRate = comm-rate.rate-comm.
   END.

   RETURN.
END PROCEDURE.

/* Процедура расчета параметра на дату пересчета договора методом,
** применяемым для браузера brwl-par.p  */
PROCEDURE ALL_PARAM.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAmtID    AS INT64    NO-UNDO. /* Код параметра */
   DEF OUTPUT PARAM oAmt      AS DEC    NO-UNDO. /* Сумма параметра */
   DEF OUTPUT PARAM oCurr     AS CHAR   NO-UNDO. /* Валюта параметра */
   DEF OUTPUT PARAM oAmtRub   AS DEC    NO-UNDO. /* Сумма параметра в рублях */

   DEF VAR vDbSumDec  AS DEC  NO-UNDO.
   DEF VAR vCrSumDec  AS DEC  NO-UNDO.
   DEF VAR vCodOstpar AS INT64  NO-UNDO.
   DEF VAR vParamStr  AS CHAR NO-UNDO.
   DEF VAR vCodeStr   AS CHAR NO-UNDO.
   DEF VAR vCode      AS INT64  NO-UNDO.
   DEF VAR vAmtDiff   AS DEC  NO-UNDO. /* Сумма корректировки параметра */

   DEF BUFFER bloan     FOR loan.
   DEF BUFFER bterm-obl FOR term-obl.

   MAIN:
   DO:
      FIND FIRST bloan WHERE
                 bloan.contract  EQ iContract
             AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.

      IF AVAILABLE(bloan) THEN
      DO:
         ASSIGN
            vCodOstpar = GetParCode(bloan.class-code, "КодОснДолг").
            vParamStr  = FGetSetting("ШтрНастр","ШтрПарам",?).
            vCodeStr   = FGetSetting("ШтрНастр","ШтрКод",?)
            .

          /* Расчета параметра без учета процентов */
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           iAmtID,
                           ?,                    /* Расчет на дату loan.since */
                           OUTPUT oAmt,
                           OUTPUT vDbSumDec,
                           OUTPUT vCrSumDec).

         RUN inter_current  (BUFFER bloan, (iAmtID - vCodOstPar), OUTPUT vAmtDiff) .
         oAmt = oAmt + vAmtDiff.
         IF LOOKUP(STRING(iAmtID), vParamStr) NE 0 THEN
         DO:
            vCode = INT64(ENTRY(LOOKUP(STRING(iAmtID), vParamStr), vCodeStr)) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR
               AND vCode NE 0
               AND vCode NE ? THEN
            DO:
               FIND FIRST bterm-obl WHERE
                          bterm-obl.contract  EQ iContract
                      AND bterm-obl.cont-code EQ iContCode
                      AND bterm-obl.idnt      EQ 6
                      AND bterm-obl.end-date  EQ bloan.since
                      AND bterm-obl.nn        EQ vCode
               NO-LOCK NO-ERROR.
               IF AVAIL bterm-obl THEN
                  oAmt = oAmt + term-obl.amt-rub.
            END.
         END.

         IF CAN-DO("9,15,12,18,82",STRING(iAmtID)) THEN
         DO:
            vAmtDiff = 0.
            RUN CalcPrmValue (iContract,
                              iContCode,
                              iAmtID,
                              oAmt,
                              oCurr, /* в браузере brwl-par.p здесь всегда пустышка */
                              OUTPUT vAmtDiff).

            IF vAmtDiff NE ? THEN
            ASSIGN
               oAmt = vAmtDiff.
         END.

         RUN GetParP (RECID(bloan), iAmtID, oAmt, OUTPUT oAmtRub, OUTPUT oCurr).

      END.
   END.

   RETURN.
END PROCEDURE.

/* Инструмент для определения смещения даты начала и даты окончания расчета
** процентов по договору и дате перехода на 39П */
PROCEDURE GetOffset.
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDatePer     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oBeg-Offset  AS INT64  NO-UNDO.

   DEF VAR vStr AS CHAR NO-UNDO.

   DEF BUFFER loan FOR loan.

   MAIN_BLOCK:
   DO:
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
             AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF AVAIL loan THEN
      DO:
         vStr = GetXAttrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "beg-offset",
                                ?).
         IF vStr EQ ? THEN
            vStr = GetXattrInit(loan.class-code,
                                "beg-offset").
         oBeg-offset = INT64(vStr) NO-ERROR.
         IF    ERROR-STATUS:ERROR
            OR oBeg-offset EQ ? THEN
               oBeg-offset = 0.
         IF loan.open-date < iDatePer THEN
         /* Начисление происходит на исходящий остаток, и нужно учитывать день выдачи */
            IF oBeg-Offset GE 1 THEN
               oBeg-Offset = oBeg-Offset - 1.
      END.
   END.
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Пролонгирует депозитный договор
  Parameters:  iContract   - Назначение договора
               iContCode   - Идентификатор договра
               iDate       - дата операции
               iShiftType  - Тип сдвига 1 - Нет, 2 - Вперед, 3 - Назад
               oResult     - Результат обработки
               oOk         - Флаг завершения
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE DepDogProl:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEF INPUT  PARAM iShiftType AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oResult    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOk        AS LOG  NO-UNDO.

   DEF VAR mProlCount         AS INT64  NO-UNDO INIT 1.  /* Кол-во пролонгаций договора с учетом текущей */
   DEF VAR mFirstProlDate     AS DATE NO-UNDO.         /* Конечная дата первоначального срока договора */
   DEF VAR mSurr              AS CHAR NO-UNDO.         /* Суррогат */
   DEF VAR mSurC              AS CHAR NO-UNDO.         /* Суррогат копируемого условия */
   DEF VAR mDays              AS INT64  NO-UNDO.         /* Новый срок договора в днях */
   DEF VAR mAmount            AS DEC  NO-UNDO.         /* Cумма ОД (сумма параметров 0+2). */
   DEF VAR mAmtParam          AS DEC  NO-UNDO.         /* Сумма на параметре */
   DEF VAR mDbSumDec          AS DEC  NO-UNDO.
   DEF VAR mCrSumDec          AS DEC  NO-UNDO.
   DEF VAR mMove              AS INT64  NO-UNDO.         /* Направение сдвига */
   DEF VAR mOldEndDate        AS DATE NO-UNDO.         /* Первоначальная конечная дата договора */
   DEF VAR mNDays             AS INT64  NO-UNDO.
   DEF VAR mNMonthes          AS INT64  NO-UNDO.
   DEF VAR mNYears            AS INT64  NO-UNDO.
   DEF VAR mProcs             AS CHAR NO-UNDO.         /* Список %% ставок для копирования */
   DEF VAR mCntr              AS INT64  NO-UNDO.         /* Счетчик */
   DEF VAR mDopRecList        AS CHAR NO-UNDO.         /* Список ДР для копирования */
   DEF VAR mRateD             AS DEC  NO-UNDO.
   DEF VAR mRates             AS CHAR NO-UNDO.
   DEF VAR mTarif             AS CHAR NO-UNDO.         /* Тариф по старому условию */
   DEF VAR mTarifNew          AS CHAR NO-UNDO.         /* Тариф на текущую дату */
   DEF VAR mTarifErr          AS CHAR NO-UNDO.
   DEF VAR vCondCount         AS INT64  NO-UNDO.

   DEF BUFFER bloan      FOR loan.
   DEF BUFFER term-obl   FOR term-obl.
   DEF BUFFER bterm-obl  FOR term-obl.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER xloan-cond FOR loan-cond.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER bcomm-rate FOR comm-rate.
   DEF BUFFER tloan-cond FOR loan-cond. /* Локализация буфера. */

 MAIN:
   DO:
      FIND FIRST bloan WHERE
                 bloan.contract  EQ iContract
             AND bloan.cont-code EQ iContCode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF NOT AVAILABLE bloan THEN
      DO: 
         IF LOCKED bloan THEN 
              RUN Fill-SysMes IN h_tmess("","","-1","Договор заблокирован другим пользователем").
         ELSE RUN Fill-SysMes IN h_tmess("","","-1","Договор не найден").
         RETURN.
      END.

      IF AVAIL bloan THEN
      DO:
            /* Создаем список действующих ставок */
         FOR EACH comm-rate WHERE
                  comm-rate.kau        EQ bloan.contract + "," + bloan.cont-code
             AND  comm-rate.commission NE "%Рез"
             AND  comm-rate.commission NE "%ОРез"
         USE-INDEX kau NO-LOCK:
            IF LOOKUP (comm-rate.commission, mProcs) EQ 0 THEN
               mProcs = mProcs + ( IF mProcs NE "" THEN "," ELSE "") + comm-rate.commission.
         END.
            /* Подсчитаем число пролонгаций */
         FOR EACH bloan-cond WHERE
                  bloan-cond.contract  EQ bloan.contract
            AND   bloan-cond.cont-code EQ bloan.cont-code
         NO-LOCK:
            mSurr = bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since).
            IF GetXAttrValueEx("loan-cond", mSurr, "PayType", "") EQ "Пролонгац" THEN
               ASSIGN
                  mProlCount     = mProlCount + 1
                  mFirstProlDate = IF mFirstProlDate EQ ?
                                      THEN bloan-cond.since
                                      ELSE mFirstProlDate
               .
         END.
            /* Если пролонгаций еще не было, то берем дату окончания договора */
         IF mFirstProlDate EQ ? THEN
            mFirstProlDate = bloan.end-date.

         ASSIGN
               /* Новый срок договора = (n + 1) * Первоначальный срок договора, где n ╞ номер пролонгации с учетом проводимой. */
            mDays             = (mProlCount + 1) * (mFirstProlDate - bloan.open-date)
            mOldEndDate       = bloan.end-date
               /* Новая дата окончания договора = дата открытия договора + новый срок договора. */
            bloan.end-date    = bloan.open-date + mDays
               /* Договору устанавливаем статус ╓ПРОЛ· */
            bloan.loan-status = "ПРОЛ"
               /* Переопределяем тип сдвига для использования функцией GetFstWrkDay */
            mMove             = IF iShiftType = "3" THEN -1 ELSE IF iShiftType = "2" THEN 1 ELSE 0
         .
            /* Сдвинем дату, если необходимо */
         IF mMove NE 0 THEN
            bloan.end-date = GetFstWrkDay (bloan.Class-Code,
                                           bloan.user-id,
                                           bloan.end-date,
                                           9,
                                           mMove).
            /* Перерасчет параметров договора на дату пролонгации. */
         IF bloan.since NE iDate THEN
            RUN l-calc2.p (iContract,
                           iContCode,
                           iDate,
                           YES,
                           YES).
            /* Найдем сумму параметров 0 + 2 */
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           0,
                           iDate,
                           OUTPUT mAmtParam,
                           OUTPUT mDbSumDec,
                           OUTPUT mCrSumDec).
         mAmount = mAmtParam.
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           2,
                           iDate,
                           OUTPUT mAmtParam,
                           OUTPUT mDbSumDec,
                           OUTPUT mCrSumDec).
         mAmount = mAmount + mAmtParam.
            /* Найдем условие для копирования */
         FIND LAST bloan-cond WHERE
                   bloan-cond.contract  EQ bloan.contract
            AND    bloan-cond.cont-code EQ bloan.cont-code
            AND    bloan-cond.since     LE bloan.end-date /* Новая дата договора */
         NO-LOCK NO-ERROR.
         IF AVAIL bloan-cond THEN
         DO:
               /* Делаем пересчет на один день назад */
            RUN l-calc2.p (iContract,
                           iContCode,
                           iDate - 1,
                           YES,
                           YES).
               /* Создаем новое условие на договоре с типом "пролонгация"
               ** Смещение даты окончания, которая приходится на выходной день,
               ** осуществляем в соответствии с настройками транзакции (параметр iShiftType) */
            CREATE xloan-cond.
            BUFFER-COPY bloan-cond EXCEPT since TO xloan-cond
               ASSIGN
                  xloan-cond.since = iDate + 1 /* Плановая дата + 1 */
            .

            ASSIGN
                  /* Суррогат копируемого условия */
               mSurC       = bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since)
                  /* Суррогат нового условия */
               mSurr       = xloan-cond.contract + "," + xloan-cond.cont-code + "," + STRING(xloan-cond.since)
                  /* Список копируемых ДР */
               mDopRecList = "cred-work-calend,int-work-calend,cred-mode,int-mode,СхемаПлат,КолЛьгтПер,DateDelay," +
                             "DateDelayInt,КолЛьгтПерПрц,PayType,cred-curr-next,NDays,NYears,NMonthes,ПродТрф"
            .
               /* Копируем ДР с найденнного условия */
            DO mCntr = 1 TO NUM-ENTRIES(mDopRecList):
               UpdateSignsEx (xloan-cond.class-code,
                              mSurr,
                              ENTRY(mCntr, mDopRecList),
                              GetXAttrValue("loan-cond", mSurC, ENTRY(mCntr, mDopRecList))).
            END.
               /* Сумму остатка по депозиту */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "PaySum",
                           TRIM(STRING(mAmount, ">>>,>>>,>>>,>>>,>>9.99"))).
               /* Тип платежа - Пролонгация */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "PayType",
                           "Пролонгац").
               /* Дата окончания договора */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "CondEndDate",
                           STRING(bloan.end-date)).
               /* Копируем ставки на новое условие */
            DO mCntr = 1 TO NUM-ENTRIES(mProcs):
               FIND LAST comm-rate WHERE
                         comm-rate.kau        EQ iContract + "," + iContCode
                  AND    comm-rate.since      LE xloan-cond.since /* Дата пролонгации */
                  AND    comm-rate.commission EQ ENTRY(mCntr, mProcs)
               USE-INDEX kau NO-LOCK NO-ERROR.
               IF AVAIL comm-rate THEN
               DO:
                  CREATE bcomm-rate.
                  BUFFER-COPY comm-rate EXCEPT since comm-rate-id TO bcomm-rate
                     ASSIGN
                        bcomm-rate.since = xloan-cond.since
                  .
                     /* Сохраним значение ставки "%Деп" для отчета */
                  IF comm-rate.commission EQ "%Деп" THEN
                     mRates = TRIM(STRING(comm-rate.rate-comm, ">>>9.99999")).
               END.
            END.
               /* Если на эту дату уже существует запись в графике
               ** плановых остатков то изменяем сумму */
            FIND LAST term-obl WHERE
                      term-obl.contract  EQ iContract
               AND    term-obl.cont-code EQ iContCode
               AND    term-obl.idnt      EQ 2
               AND    term-obl.end-date  EQ iDate
               AND    term-obl.amt-rub   EQ 0
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF NOT AVAILABLE term-obl THEN
            DO:
               IF LOCKED term-obl THEN
                    RUN Fill-SysMes IN h_tmess("","","-1","Запись с плановым остатком заблокирована другим пользователем").
               ELSE RUN Fill-SysMes IN h_tmess("","","-1","Запись с плановым остатком не найдена").
               RETURN.
            END.

            IF AVAIL term-obl THEN
               ASSIGN
                  term-obl.amt-rub  = mAmount
                  term-obl.end-date = iDate + 1
               .
               /* Или создаем обязательство на эту дату */
            ELSE
            DO:
               FIND LAST term-obl WHERE
                         term-obl.contract  EQ iContract
                  AND    term-obl.cont-code EQ iContCode
                  AND    term-obl.idnt      EQ 2
               NO-LOCK NO-ERROR.
               IF AVAIL term-obl THEN
               DO:
                  CREATE bterm-obl.
                  ASSIGN
                     bterm-obl.contract     = term-obl.contract
                     bterm-obl.cont-code    = term-obl.cont-code
                     bterm-obl.end-date     = iDate
                     bterm-obl.idnt         = 2
                     bterm-obl.nn           = term-obl.nn + 1
                     bterm-obl.amt          = mAmount
                     bterm-obl.dsc-beg-date = term-obl.dsc-beg-date
                  .
                  RELEASE bterm-obl NO-ERROR.
               END.
            END.
               /* Создаем последнее нулевое обязательство на новую дату окончания депозита */
            FIND LAST term-obl WHERE
                      term-obl.contract  EQ iContract
               AND    term-obl.cont-code EQ iContCode
               AND    term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            CREATE bterm-obl.
            ASSIGN
               bterm-obl.contract     = iContract
               bterm-obl.cont-code    = iContCode
               bterm-obl.end-date     = bloan.end-date
               bterm-obl.idnt         = 2
               bterm-obl.nn           = ( IF AVAIL term-obl THEN term-obl.nn ELSE 0) + 1
               bterm-obl.amt          = 0
            .
            RELEASE bterm-obl NO-ERROR.
               /* Для верного формирования графика ОД удалим старый график */
            FOR EACH term-obl WHERE
                     term-obl.contract  EQ iContract
               AND   term-obl.cont-code EQ iContCode
               AND   term-obl.idnt      EQ 3
            EXCLUSIVE-LOCK:
               DELETE term-obl.
            END.
            RELEASE term-obl NO-ERROR.
               /* Проверяем соответствие тарифов */
            ASSIGN
                  /* Суррогат нового условия */
               mSurr = xloan-cond.contract + "," + xloan-cond.cont-code + "," + STRING(xloan-cond.since)
               mTarif = GetXAttrValueEx("loan-cond", mSurr, "ПродТрф", "")
            .
            RUN GetLoanTarif (iContract,
                              iContCode,
                              iDate + 1, /* Определяем на дату нового условия, для учета капитализации */
                              FALSE,
                              OUTPUT mTarifNew).
            IF mTarifNew EQ "" THEN
               mTarifErr = "ПРЕДУПРЕЖДЕНИЕ. Не найден подходящий тариф, договор пролонгирован со ставками по предыдущему условию!!!".
            ELSE IF mTarifNew NE mTarif THEN
            DO:
                  /* Если новое условие попадает под другой тариф, то обновим тариф */
               UpdateSignsEx (xloan-cond.class-code, mSurr, "ПродТрф", mTarifNew).
               mTarif = mTarifNew.
            END.
            ELSE
               mTarifNew = "".   /* Для отчета */
               /* Копируем ставки на новое условие */
            DO mCntr = 1 TO NUM-ENTRIES(mProcs):
                  /* Получим ставку из тарифа по коду комиссии */
               RUN GetTarifRate (mTarif,                /* Код тарифа */
                                 ENTRY(mCntr, mProcs),  /* Код комиссии */
                                 iDate,                 /* На дату */
                                 OUTPUT mRateD).        /* Ставка */
                  /* Если ставка найдена, то обновим ее на условии */
               IF mRateD GT 0 THEN
               DO:
                  FIND LAST comm-rate WHERE
                            comm-rate.kau        EQ iContract + "," + iContCode
                     AND    comm-rate.since      LE xloan-cond.since /* Дата пролонгации */
                     AND    comm-rate.commission EQ ENTRY(mCntr, mProcs)
                  USE-INDEX kau EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

                  IF NOT AVAILABLE comm-rate THEN
                  DO:
                     IF LOCKED comm-rate THEN 
                          RUN Fill-SysMes IN h_tmess("","","-1","Комиссия заблокирована другим пользователем").
                     ELSE RUN Fill-SysMes IN h_tmess("","","-1","Комиссия не найдена").
                     RETURN.
                  END.

                  IF AVAIL comm-rate THEN
                  DO:
                     comm-rate.rate-comm = mRateD.
                        /* Сохраним новое значение ставки "%Деп" для отчета */
                     IF comm-rate.commission EQ "%Деп" THEN
                        mRates = TRIM(STRING(comm-rate.rate-comm, ">>>9.99999")).
                  END.
               END.
            END.
               /* Пересчитываем график платежей по процентам. Сдвиг платежей, даты которых приходятся на выходные дни,
               ** проводится в соответствии с настройкой транзакции. (параметр iShiftType) */
            RUN SetSysConf IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ", iShiftType).
            RUN SetSysConf IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ",  iShiftType).
            RUN SetSysConf IN h_base ("ОБЯЗ. ПО ВОЗВРАТУ СДВИГ ОКОН.СРОКА", "1").
            RUN SetSysConf IN h_base ("ПЛАТ. ПО ПРОЦ. СДВИГ ОКОН.СРОКА", "1").
               /* Давим вывод на экран графиков для корректировки  */
            RUN SetSysConf IN h_base ("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН", "ДА").

            FOR EACH xloan-cond WHERE xloan-cond.contract  EQ loan.contract
                               AND xloan-cond.cont-code EQ loan.cont-code
            NO-LOCK:
               vCondCount = vCondCount + 1.
            END.
            RUN mm-to.p (RECID(bloan),
                         RECID(xloan-cond),
                         mAmount,
                         {&MOD_ADD},
                         YES,
                         YES,
                         YES,
                         YES,
                         bloan.risk,
                         vCondCount) NO-ERROR.
            oResult = {&RETURN_VALUE}.
               /* Чистим все... */
            RUN DeleteOldDataProtocol IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
            RUN DeleteOldDataProtocol IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").
            RUN DeleteOldDataProtocol IN h_base ("ОБЯЗ. ПО ВОЗВРАТУ СДВИГ ОКОН.СРОКА").
            RUN DeleteOldDataProtocol IN h_base ("ПЛАТ. ПО ПРОЦ. СДВИГ ОКОН.СРОКА").
            RUN DeleteOldDataProtocol IN h_base ("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН").

            IF oResult NE {&RET-ERROR} THEN
               ASSIGN
                  oOk     = TRUE
                  oResult = ( IF mTarifErr NE "" THEN mTarifErr + " " ELSE "") +
                            ( IF mTarifNew NE "" THEN "Новый тариф: " + mTarifNew + " " ELSE "") +
                            "Первонач.срок = " + STRING(xloan-cond.since) +
                            " Дата пролонгации = " + STRING(xloan-cond.since) +
                            " Новая дата окончания = " + STRING(bloan.end-date) +
                            " Ставка = " + mRates +
                            " Пролонгирована сумма = " +
                            TRIM(STRING(mAmount, ">>>,>>>,>>>,>>>,>>9.99"))
               .
            ELSE
               oResult = "Ошибка пролонгации договора".
         END.
      END.
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     Процедура возвращает значения продукта договора и флаг возможности пролонгации
  Parameters:  iContract   - Назначение договора
               iContCode   - Идентификатор договра
               iDate       - Дата пролонгации
               oResult     - Результат обработки
               oOk         - Указывает на возможность пролонгации (при инициализации - ложно)
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE DepPogProlChance:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEF OUTPUT PARAM oResult    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOk        AS LOG  NO-UNDO.

   DEF VAR vTarif        AS CHAR NO-UNDO.
   DEF VAR vProduct      AS CHAR NO-UNDO.

   DEF BUFFER loan-cond  FOR loan-cond.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER bcomm-rate FOR comm-rate.

      /* НП "КонтПроцСтав" - контроль процентной ставки при пролонгации = НЕТ,
      ** можно пролонгировать безконтрольно */
   IF FGetSetting("КонтПроцСтав", "", "ДА") EQ "НЕТ" THEN
      oOk = TRUE.
   ELSE
   DO:
         /* Определяем тариф по договору */
      FIND LAST loan-cond WHERE
                loan-cond.contract  EQ iContract
         AND    loan-cond.cont-code EQ iContCode
         AND    loan-cond.since     LE iDate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-cond THEN
      DO:
         ASSIGN
            vProduct = GetXAttrValueEx ("loan",
                                        iContract + "," + iContCode,
                                        "ПродКод",
                                        "?")
            vTarif   = GetXAttrValueEx ("loan-cond",
                                        loan-cond.contract + "," +
                                        loan-cond.cont-code + "," +
                                        STRING(loan-cond.since),
                                        "ПродТрф",
                                        "?")
         .
            /* Проверка существования тарифа */
         FIND FIRST code WHERE
                    code.class EQ "ПродТарифы"
            AND     code.code  EQ vTarif
         NO-LOCK NO-ERROR.
            /* Проверка на срок действия продукта */
         FIND LAST tmp-code WHERE
                   tmp-code.class    EQ "ПродЛин"
            AND    tmp-code.code     EQ vProduct
            AND    tmp-code.beg-date LE iDate
         NO-LOCK NO-ERROR.
         IF vProduct EQ "?" THEN
            oResult = "На договоре не установлен продукт.".
         ELSE IF NOT AVAIL tmp-code THEN
            oResult = "Установленный на договоре продукт <" + vProduct + "> не найден.".
         ELSE IF     tmp-code.end-date NE ?
                 AND tmp-code.end-date LT iDate THEN
            oResult = "Срок действия продукта <" + vProduct + "> окончен.".
         ELSE  IF vTarif EQ "?" THEN
            oResult = "На условии договора не установлен тариф.".
         ELSE IF NOT AVAIL code THEN
            oResult = "Установленный на действующем условии тариф <" + vTarif + "> не существует.".
         ELSE
         DO:
               /* Определим %% ставку %Деп по тарифу и сравним ее со ставкой договора */
            FIND LAST comm-rate WHERE
                      comm-rate.kau        EQ iContract + "," + iContCode
               AND    comm-rate.commission EQ "%Деп"
               AND    comm-rate.since      LE iDate
            USE-INDEX kau NO-LOCK NO-ERROR.
            FIND LAST bcomm-rate WHERE
                      bcomm-rate.kau        EQ "ПродТрф," + vTarif
               AND    bcomm-rate.commission EQ "%Деп"
               AND    bcomm-rate.since      LE iDate
            USE-INDEX kau NO-LOCK NO-ERROR.
               /* Если ставка не менялась, то пролонгируем */
            IF     AVAIL comm-rate
               AND AVAIL bcomm-rate
               AND comm-rate.rate-comm EQ bcomm-rate.rate-comm THEN
               oOk = TRUE.
               /* Для протокола */
            oResult = oResult + "Действующий тариф: " + vTarif + ".".
            IF NOT AVAIL bcomm-rate THEN
               oResult = oResult + " Не найдена действующая %% ставка тарифа.".
            IF NOT AVAIL comm-rate THEN
               oResult = oResult + " Не найдена действующая %% ставка договора.".
            IF     AVAIL comm-rate
               AND AVAIL bcomm-rate
               AND comm-rate.rate-comm NE bcomm-rate.rate-comm THEN
               oResult = oResult + " Действующие %% ставки договора и тарифа не равны.".
          END.
      END.
   END.

END PROCEDURE.


   /* ===============================-=--=-= */
   /* Инструмент для получения ставки тарифа */
PROCEDURE GetTarifRate.
   DEF INPUT  PARAM iTarif      AS CHAR NO-UNDO.   /* Код тарифа */
   DEF INPUT  PARAM iCommission AS CHAR NO-UNDO.   /* Код ставки */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.   /* На дату */
   DEF OUTPUT PARAM oRate       AS DEC  NO-UNDO.   /* Ставка */
      /* Локализуем буфер */
   DEF BUFFER comm-rate  FOR comm-rate.
      /* Если тариф задан */
   IF {assigned iTarif} THEN
   DO:
      FIND LAST comm-rate WHERE
                comm-rate.kau        EQ "ПродТрф," + iTarif
         AND    comm-rate.commission EQ iCommission
         AND    comm-rate.since      LE iDate
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF AVAIL comm-rate THEN
         oRate = comm-rate.rate-comm.
   END.
END PROCEDURE.

   /* Получить параметры тарифа */
PROCEDURE GetParamTarif.
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* Идентификатор   */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* договора        */
   DEF INPUT  PARAM iDate         AS DATE NO-UNDO. /* Дата            */
   DEF OUTPUT PARAM oStrParam     AS CHAR NO-UNDO. /* Флаг успешности */

      /* Переменные в которых хранятся параметры кредитования */
   DEF VAR vCurrency   AS CHAR NO-UNDO.
   DEF VAR vContType   AS CHAR NO-UNDO.
   DEF VAR vProduct    AS CHAR NO-UNDO.
   DEF VAR vRate       AS CHAR NO-UNDO.
   DEF VAR vKolDok     AS CHAR NO-UNDO.
   DEF VAR vTarif      AS CHAR NO-UNDO.
   DEF VAR vPoruchit   AS CHAR NO-UNDO.
   DEF VAR vVznos      AS CHAR NO-UNDO.
   DEF VAR vVznosProc  AS CHAR NO-UNDO.
   DEF VAR vStrah      AS CHAR NO-UNDO.
   DEF VAR vIndeks     AS CHAR NO-UNDO.
   DEF VAR vTypeClient AS CHAR NO-UNDO.
   DEF VAR vSumma      AS CHAR NO-UNDO.
   DEF VAR vSrok       AS CHAR NO-UNDO.
   DEF VAR vSurr       AS CHAR NO-UNDO.   /* Суррогаты */

   DEF BUFFER loan FOR loan. /* Локализация буфера */

   mb:
   DO ON ERROR UNDO, LEAVE:

         /* Ищем договор */
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
         AND     loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE mb.

         /* Получаем валюту, тип кредита с договора и ДР */
      ASSIGN
         vSurr       = loan.contract + "," + loan.cont-code
         vCurrency   = loan.currency
         vContType   = loan.cont-type
         vProduct    = GetXAttrValueEx ("loan", vSurr, "ПродКод"    , "")
         vKolDok     = GetXAttrValueEx ("loan", vSurr, "ПродКолДок" , "")
         vTarif      = GetXAttrValueEx ("loan", vSurr, "ПродТрф"    , "")
         vPoruchit   = GetXAttrValueEx ("loan", vSurr, "ПродПоруч"  , "")
         vVznos      = GetXAttrValueEx ("loan", vSurr, "ПродВзнос"  , "")
         vStrah      = GetXAttrValueEx ("loan", vSurr, "ПродСтрах"  , "")
         vIndeks     = GetXAttrValueEx ("loan", vSurr, "ПродИндекс" , "")
         vTypeClient = GetXAttrValueEx ("loan", vSurr, "ПродТипКл"  , "")
         vRate       = GetXAttrValueEx ("loan", vSurr, "ПродРейтинг", "")
         vRate       = IF NUM-ENTRIES(vRate, "R") GE 2
                          THEN ENTRY(2, vRate, "R")
                          ELSE vRate
      .
         /* Определим сумму */
      FIND LAST term-obl WHERE
                term-obl.contract  EQ iContract
         AND    term-obl.cont-code EQ iContCode
         AND    term-obl.idnt      EQ 2
         AND    term-obl.end-date  LE iDate
      NO-LOCK NO-ERROR.
         /* Если нашли, то сохраняем */
      IF AVAIL term-obl THEN
         vSumma = TRIM( STRING(term-obl.amt-rub, ">>>>>>>9.99") ).
         /* Ищем условие договора для получения Срока и Кода тарифа */
      FIND FIRST loan-cond WHERE
                 loan-cond.contract  EQ iContract
         AND     loan-cond.cont-code EQ iContCode
         AND     loan-cond.since     EQ loan.open-date
      NO-LOCK NO-ERROR.
         /* Если нашли, то получаем ДР */
      IF AVAIL loan-cond THEN
         ASSIGN
            vSurr  = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
            vSrok  = GetXAttrValue("loan-cond", vSurr, "NMonthes")
            vTarif = GetXAttrValue("loan-cond", vSurr, "ПродТрф")
         .
         /* Формируем возвращаемую строку */
      oStrParam = vProduct       + ";" +
                  STRING(vSumma) + ";" +
                  vCurrency      + ";" +
                  vSrok          + ";" +
                  vRate          + ";" +
                  vKolDok        + ";" +
                  vTarif         + ";" +
                  vContType      + ";" +
                  vPoruchit      + ";" +
                  vVznos         + ";" +
                  vVznosProc     + ";" +
                  vStrah         + ";" +
                  vIndeks        + ";" +
                  vTypeClient.
   END. /* mb: */
END PROCEDURE.


   /* ==========================================-=--=-= */
   /* Инструмент для вычисления кода тарифа по договору */
PROCEDURE GetLoanTarif.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* Назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* № договора */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* Дата */
   DEF INPUT  PARAM iCalcTrf   AS LOG  NO-UNDO.        /* Искать ли тариф на ДР "ПродТрф" */
   DEF OUTPUT PARAM oTarif     AS CHAR NO-UNDO INIT ?. /* Тариф */

   DEF VAR vStrParam   AS CHAR NO-UNDO. /* Параметры продукта с договора */
   DEF VAR vProdTmpC   AS CHAR NO-UNDO. /* Тип продукта */
   DEF VAR vIndCount   AS INT64  NO-UNDO. /* Количество продуктов в диапазоне */
   DEF VAR vSummMin    AS CHAR NO-UNDO. /* Минимальная сумма */
   DEF VAR vSummMax    AS CHAR NO-UNDO. /* Максимальная сумма */
   DEF VAR vSrokMin    AS CHAR NO-UNDO. /* Минимальный срок */
   DEF VAR vSrokMax    AS CHAR NO-UNDO. /* Максимальный срок */
   DEF VAR vVznosMin   AS CHAR NO-UNDO. /* Минимальный взнос */
   DEF VAR vVznosMax   AS CHAR NO-UNDO. /* Максимальный взнос */
   DEF VAR vSurr       AS CHAR NO-UNDO. /* Фильтр - зависит от типа продукта */
   DEF VAR vSurrTmp    AS CHAR NO-UNDO.
   DEF VAR vTmp        AS CHAR NO-UNDO.
   DEF VAR vI          AS INT64  NO-UNDO.
      /* Параметры продукта */
   DEF VAR vProduct    AS CHAR NO-UNDO. /* Код */
   DEF VAR vLoanSumm   AS CHAR NO-UNDO. /* Сумма договора */
   DEF VAR vCurrency   AS CHAR NO-UNDO. /* Валюта */
   DEF VAR vSrok       AS CHAR NO-UNDO. /* Срок */
   DEF VAR vRate       AS CHAR NO-UNDO. /* Рейтинг */
   DEF VAR vKolDok     AS CHAR NO-UNDO. /* Кол-во документов */
   DEF VAR vTarif      AS CHAR NO-UNDO. /* Тариф */
   DEF VAR vContType   AS CHAR NO-UNDO. /* Тип договора */
   DEF VAR vPoruchit   AS CHAR NO-UNDO. /* Получатель */
   DEF VAR vVznos      AS CHAR NO-UNDO. /* Взнос */
   DEF VAR vVznosProc  AS CHAR NO-UNDO. /* %% взноса ? */
   DEF VAR vStrah      AS CHAR NO-UNDO. /* Страховка */
   DEF VAR vIndeks     AS CHAR NO-UNDO. /* Индекс */
   DEF VAR vTypeClient AS CHAR NO-UNDO. /* Тип клиента */

      /* Получим параметры договора */
   RUN GetParamTarif (iContract,
                      iContCode,
                      iDate,
                      OUTPUT vStrParam).
      /* Если есть список, то двигаемся дальше */
   IF NUM-ENTRIES(vStrParam, ";") NE 0 THEN
   DO:
      ASSIGN
         vProduct    = ENTRY( 1, vStrParam, ";")
         vLoanSumm   = ENTRY( 2, vStrParam, ";")
         vCurrency   = ENTRY( 3, vStrParam, ";")
         vSrok       = ENTRY( 4, vStrParam, ";")
         vRate       = ENTRY( 5, vStrParam, ";")
         vKolDok     = ENTRY( 6, vStrParam, ";")
         vTarif      = IF iCalcTrf THEN ENTRY( 7, vStrParam, ";") ELSE ""
         vContType   = ENTRY( 8, vStrParam, ";")
         vPoruchit   = ENTRY( 9, vStrParam, ";")
         vVznos      = ENTRY(10, vStrParam, ";")
         vVznosProc  = ENTRY(11, vStrParam, ";")
         vStrah      = ENTRY(12, vStrParam, ";")
         vIndeks     = ENTRY(13, vStrParam, ";")
         vTypeClient = ENTRY(14, vStrParam, ";")
         vPoruchit   = IF vPoruchit EQ "" THEN "НЕТ" ELSE vPoruchit
         vStrah      = IF vStrah    EQ "" THEN "НЕТ" ELSE vStrah
         vSummMin    = "0.00"
         vSummMax    = "0.00"
         vSrokMin    = "0"
         vSrokMax    = "0"
         vVznosMin   = "0.00"
         vVznosMax   = "0.00"
      .
         /* Если тариф определен на ДР договора, то возвращаем его */
      IF vTarif NE "" THEN
         oTarif = vTarif.
         /* Иначе ищем на indicate */
      ELSE
      DO:
            /* Ищем на "ПродЛин"-е тип продукта */
         FIND LAST tmp-code WHERE
                   tmp-code.class    EQ "ПродЛин"
            AND    tmp-code.code     EQ vProduct
            AND    tmp-code.beg-date LE iDate
            AND   (tmp-code.end-date GE iDate
                OR tmp-code.end-date EQ ?)
         NO-LOCK NO-ERROR.
         IF AVAIL tmp-code THEN
            vProdTmpC = tmp-code.val.
            /* Определим диапазон сумм */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("АвтоПрод,ПотребПрод,ДепВСС", vProdTmpC)
                       THEN vProduct + "," + vCurrency
                       ELSE vProduct
         .
            /* Создаем массив нижних границ сумм (vTmp) */
         vIndCount = GetRefCrVal (vProdTmpC,
                                   "Сумма с",
                                   iDate,
                                   vSurr,
                                  (TEMP-TABLE ttIndicate:HANDLE)).
         FOR EACH ttIndicate WHERE
                  ttIndicate.fdec LE DEC(vLoanSumm):
            vTmp = vTmp + TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99")) + ",".
         END.
            /* В цикле по найденным нижним границам сумм определяем верхнюю границу */
         BLK1:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                      "Сумма по",
                                      iDate,
                                      vSurrTmp,
                                     (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fdec GE DEC(vLoanSumm):
               ASSIGN
                  vSummMin = ENTRY(vI, vTmp)
                  vSummMax = TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99"))
               .
               LEAVE BLK1.
            END.
         END. /* BLK1 */
            /* Определим диапазон сроков */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("АвтоПрод,ПотребПрод,ДепВСС", vProdTmpC)
                       THEN vSurr + "," + vSummMin + "," + vSummMax
                       ELSE vSurr + "," + vSummMin + "," + vSummMax + "," + vCurrency
         .
         vIndCount = GetRefCrVal (vProdTmpC,
                                  "Срок с",
                                  iDate,
                                  vSurr,
                                 (TEMP-TABLE ttIndicate:HANDLE)).
         FOR FIRST ttIndicate WHERE
                   ttIndicate.fint LE INT64(vSrok):
            vTmp = vTmp + TRIM(STRING(INT64(ttIndicate.fint), ">>>>>>>9")) + ",".
         END.
         BLK2:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                     "Срок по",
                                     iDate,
                                     vSurrTmp,
                                    (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fint GE INT64(vSrok):
               ASSIGN
                  vSrokMin = ENTRY(vI, vTmp)
                  vSrokMax = TRIM(STRING(INT64(ttIndicate.fint), ">>>>>>>9"))
               .
               LEAVE BLK2.
            END.
         END. /* BLK2 */
            /* Определим диапазон взносов */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("АвтоПрод,ПотребПрод", vProdTmpC)
                       THEN vSurr + "," + vSrokMin + "," + vSrokMax
                       ELSE vSurr + "," + vSrok
            vSurr = IF CAN-DO("АвтоПрод,КредПрод", vProdTmpC)
                       THEN vSurr + "," + vPoruchit
                       ELSE vSurr
         .
         vIndCount = GetRefCrVal (vProdTmpC,
                                  "Взнос с",
                                  iDate,
                                  vSurr,
                                 (TEMP-TABLE ttIndicate:HANDLE)).
         FOR FIRST ttIndicate WHERE
                   ttIndicate.fdec LE DEC(vVznos):
            vTmp = vTmp + TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99")) + ",".
         END.
         BLK3:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                     "Взнос по",
                                     iDate,
                                     vSurrTmp,
                                    (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fdec GE DEC(vVznos):
               ASSIGN
                  vVznosMin = ENTRY(vI, vTmp)
                  vVznosMax = TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99"))
               .
               LEAVE BLK3.
            END.
         END. /* BLK3 */

            /* Определение тарифа зависит от типа продукта */
         CASE vProdTmpC:
            WHEN "КредПрод" THEN
                  /* Продукт; Сумма С; Сумма ПО; ВАЛ; Срок; Пор; Документы; Рейтинг; Тариф */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vCurrency + "," +
                                   vSrok     + "," +
                                   vPoruchit + "," +
                                   vKolDok   + "," +
                                   vRate).
            WHEN "АвтоПрод" THEN
                  /* Продукт; Вал; Сумма с; Сумма по; Ср.с; Ср.п; Пор; Вз.с; Вз.по; Док; Стр; Рейт; Тариф */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax  + "," +
                                   vPoruchit + "," +
                                   vVznosMin + "," +
                                   vVznosMax + "," +
                                   vKolDok   + "," +
                                   vStrah    + "," +
                                   vRate).
            WHEN "ПотребПрод" THEN
                  /* Продукт; Вал; Сумма с; Сумма по; Ср.с; Ср.п; Вз.с; Вз.по; Док; Рейт; ИР; Тариф */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax  + "," +
                                   vVznosMin + "," +
                                   vVznosMax + "," +
                                   vKolDok   + "," +
                                   vRate     + "," +
                                   vIndeks).
            WHEN "АкцПрод" THEN
                  /* Продукт; Сумма С; Сумма ПО; ВАЛ; Срок; Вз с; Вз по; Тариф */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vCurrency + "," +
                                   vSrok     + "," +
                                   vVznosMin + "," +
                                   vVznosMax).
            WHEN "ДепВСС" THEN
                  /* Продукт; Вал; Сумма с; Сумма по; Ср.с; Ср.п; Тариф */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax).
         END CASE.
      END.
   END.

      /* Проверим, а существует ли такой тариф в классификаторе? */
   IF oTarif NE "" THEN
   DO:
      FIND FIRST code WHERE
                 code.class  EQ 'ПродТарифы'
         AND     code.parent EQ vProdTmpC
         AND     code.code   EQ oTarif
      NO-LOCK NO-ERROR.
      IF NOT AVAIL code THEN
         oTarif = "".
   END.
END PROCEDURE.

   /* ====================================================-=--=-= */
   /* Процедура для создания комиссии %Рез (комиссия применяется
   ** для определения доли резервирования в зависимости от риска) */
PROCEDURE CrResRate.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* Назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* № договора */
   DEF INPUT  PARAM iRate      AS DEC  NO-UNDO.        /* Ставка */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* Дата   */
      /* Локализация буфера */
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-rate FOR comm-rate.
      /* Поиск договора */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* Ищем комиссию */
      FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ "%Рез"
         AND     comm-rate.kau        EQ loan.contract + "," + loan.cont-code
         AND     comm-rate.currency   EQ loan.currency
         AND     comm-rate.acct       EQ "0"
         AND     comm-rate.min-value  EQ 0.00
         AND     comm-rate.period     EQ 0
         AND     comm-rate.since      LE iDate
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF NOT AVAIL comm-rate THEN
      DO:
         CREATE comm-rate.
         ASSIGN
            comm-rate.commission = "%Рез"
            comm-rate.kau        = loan.contract + "," + loan.cont-code
            comm-rate.currency   = loan.currency
            comm-rate.rate-comm  = iRate
            comm-rate.acct       = "0"
            comm-rate.min-value  = 0.00
            comm-rate.period     = 0
            comm-rate.since      = iDate
         .
      END.
   END.
END PROCEDURE.

   /* ====================================================-=--=-= */
   /* Процедура для сохранения комиссии %Рез (комиссия применяется
   ** для определения доли резервирования в зависимости от риска)
      В отличие от CrResRate изменяет значение, если комиссия на дату уже есть,
      а не только создает новую */
PROCEDURE SaveResRate.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* Назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* № договора */
   DEF INPUT  PARAM iRate      AS DEC  NO-UNDO.        /* Ставка */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* Дата   */
      /* Локализация буфера */
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-rate FOR comm-rate.
      /* Поиск договора */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* Ищем комиссию */
      DO WHILE TRUE:
         FIND FIRST comm-rate WHERE
                    comm-rate.commission EQ "%Рез"
            AND     comm-rate.kau        EQ loan.contract + "," + loan.cont-code
            AND     comm-rate.currency   EQ loan.currency
            AND     comm-rate.acct       EQ "0"
            AND     comm-rate.min-value  EQ 0.00
            AND     comm-rate.period     EQ 0
            AND     comm-rate.since      LE iDate
         USE-INDEX kau EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED comm-rate THEN
         DO:
            pick-value = "yes".
            RUN Fill-SysMes IN h_tmess ("", "", "4",
                "Не удалось сохранить коэффициент резервирования.~n" +
                "Запись заблокирована другим пользователем.~n" +
                "Повторить попытку (Да) / Отменить (Нет)?").
            IF pick-value EQ "no" THEN
               RETURN ERROR "Не удалось сохранить коэффициент резервирования".
         END.
         ELSE
            LEAVE.
      END.

      IF NOT AVAIL comm-rate THEN
      DO:
         CREATE comm-rate.
         ASSIGN
            comm-rate.commission = "%Рез"
            comm-rate.kau        = loan.contract + "," + loan.cont-code
            comm-rate.currency   = loan.currency
            comm-rate.acct       = "0"
            comm-rate.min-value  = 0.00
            comm-rate.period     = 0
            comm-rate.since      = iDate
         .
      END.
      comm-rate.rate-comm = iRate.
      RELEASE comm-rate.
   END.
END PROCEDURE.

/* Вычисление единовременных комиссий */
PROCEDURE CalcCommStart.
   DEF INPUT  PARAM iComm         AS CHAR NO-UNDO. /* Код комиссии          */
   DEF INPUT  PARAM iCalcBase     AS DEC  NO-UNDO. /* База начисл. комиссии */
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* Идентификатор         */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* договора              */
   DEF INPUT  PARAM iCurrency     AS CHAR NO-UNDO. /* Валюта                */
   DEF INPUT  PARAM iAvailComm    AS CHAR NO-UNDO. /* Какие ком.начислять   */
   DEF OUTPUT PARAM oCommVal      AS DEC  NO-UNDO. /* Сумма комиссии        */
   DEF OUTPUT PARAM oOk           AS INT64  NO-UNDO. /* Флаг успешности       */

   DEF VAR vProdCode   AS CHAR NO-UNDO.
   DEF VAR vCommToNach AS CHAR NO-UNDO.

   DEF BUFFER loan FOR loan. /* Локализация буффера */

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* Найдем договор, и код продукта */
      FIND FIRST loan WHERE loan.contract  EQ iContract
                     AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      vProdCode = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПродКод","").
      IF vProdCode EQ "" THEN
         LEAVE mb.

      vCommToNach = GetCode ("КомКасМ",vProdCode).

      /* Если iAvailComm=Да, то начисляем, то что хранится в КомКасМ,
      ** Если iAvailComm=Нет, то начисляем недостающую(ие) из списка
      ** %Откр,%Выд */
      IF    iAvailComm EQ "Нет"
        AND CAN-DO(vCommToNach,iComm) THEN
         LEAVE mb.

      IF    iAvailComm EQ "Да"
        AND NOT CAN-DO(vCommToNach,iComm) THEN
         LEAVE mb.

      /* Проверяем есть ли такая комиссия вообще
      ** есди нет, то ругаемся и выдаем ошибку */
      FIND FIRST commission WHERE commission.commission EQ iComm
                              AND commission.currency   EQ iCurrency
                              AND commission.min-value  EQ 0
                              AND commission.period     EQ 0
      NO-LOCK NO-ERROR.
      IF NOT AVAIL commission THEN
      DO:
         IF iAvailComm NE "?" THEN
            RUN Fill-SysMes IN h_tmess ("","","0","Не найдена комиссия " + iComm + " в валюте '" + iCurrency + "' в справочнике 'Комисси и тарифы'").
         LEAVE mb.
      END.

      FIND FIRST comm-rate WHERE comm-rate.commission EQ iComm
                             AND comm-rate.acct       EQ "0"
                             AND comm-rate.currency   EQ iCurrency
                             AND comm-rate.kau        EQ iContract + "," + iContCode
                             AND comm-rate.min-value  EQ 0
                             AND comm-rate.period     EQ 0
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF NOT AVAIL comm-rate THEN
      DO:
         IF iAvailComm NE "?" THEN
            RUN Fill-SysMes IN h_tmess ("","","0","Не найдена комиссия " + iComm + " в валюте '" + iCurrency + "' по договору " + iContCode ).
         LEAVE mb.
      END.

      /* Если тип комиссии "=", то эту сумму и берем,
      ** иначе считаем */
      IF comm-rate.rate-fixed THEN
         oCommVal = comm-rate.rate-comm.
      ELSE
         oCommVal = iCalcBase * comm-rate.rate-comm / 100.

      oOk = 1.
   END. /* mb: */
END PROCEDURE.


   /* =======================================================-===--==-= */
   /* Определение кода операции по счету                                */
PROCEDURE GetOpCodeFromAcct.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* Назначение договора */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* Номер договора */
   DEF INPUT  PARAM iAcct       AS CHAR NO-UNDO.  /* Счет */
   DEF INPUT  PARAM iCurrency   AS CHAR NO-UNDO.  /* Валюта */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.  /* Дата */
   DEF INPUT  PARAM iSide       AS CHAR NO-UNDO.  /* "Дб" или "Кр" */
   DEF OUTPUT PARAM oOpCode     AS CHAR NO-UNDO.  /* Код операции */
      /* Локализуем буферы */
   DEF BUFFER loan-acct FOR loan-acct.
   DEF BUFFER code      FOR code.
      /* Если код операции не опереден */
   oOpCode = ?.
      /* Определим роль, с которой данный счет связан с договором */
   FIND FIRST loan-acct WHERE
              loan-acct.contract  EQ iContract
      AND     loan-acct.cont-code EQ iContCode
      AND     loan-acct.acct      EQ iAcct
      AND     loan-acct.currency  EQ iCurrency
      AND     loan-acct.since     LE iDate
   NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
   DO:
         /* Ищем в "ТипСчДог" операцию для роли loan-acct.acct-type
         ** по Дебету/Кредиту (в зависимости от входного параметра) */
      FIND FIRST code WHERE
                 code.class  EQ     "ТипСчДог"
         AND     code.parent EQ     loan-acct.acct-type
         AND     code.code   BEGINS iSide
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         oOpCode = REPLACE(code.code, iSide, "").
   END.

END PROCEDURE.

   /* =======================---=-=-=-=-=-=-=-=- */
   /* Поиск/выбор счета по роли, аналог PlAcct  */
PROCEDURE ReqAcctByRole.
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.  /* Назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.  /* Номер договора */
   DEF INPUT  PARAM iAcctRole AS CHAR NO-UNDO.  /* Роль счета */
   DEF INPUT  PARAM iAcctCat  AS CHAR NO-UNDO.  /* Тип счета */
   DEF INPUT  PARAM iPLAcct   AS CHAR NO-UNDO.  /* Классификатор PLAcct */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.  /* Дата */
   DEF INPUT  PARAM iMess     AS LOG  NO-UNDO.  /* Выводить сообщение о необходимости выбора счета */
   DEF OUTPUT PARAM oAcct     AS CHAR NO-UNDO.  /* Счет */

   DEF VAR vTemp        AS CHAR NO-UNDO.
   DEF VAR DTType       AS CHAR NO-UNDO.
   DEF VAR DTKind       AS CHAR NO-UNDO.
   DEF VAR DTCust       AS CHAR NO-UNDO.
   DEF VAR vBal2        AS CHAR NO-UNDO.
   DEF VAR vFlt         AS CHAR NO-UNDO EXTENT 2.

   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-acct FOR loan-acct.

      /* Найдем договор */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* Ищем счет по роли и дате в картотеке счетов */
      FIND LAST loan-acct WHERE
                loan-acct.contract  EQ iContract
         AND    loan-acct.cont-code EQ iContCode
         AND    loan-acct.acct-type EQ iAcctRole
         AND    loan-acct.since     LE iDate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN
      DO:
         oAcct = loan-acct.acct.
      END.
         /* Если не найден, то выбираем из браузера счетов */
      ELSE
      DO:
         RUN DTCust (loan.cust-cat,
                     loan.cust-id,
                     ?,
                     OUTPUT DTCust).

         ASSIGN
            vTemp      = FGetSetting("К302П", "PLAcct", "")
            iPLAcct    = IF vTemp NE ""
                            THEN vTemp
                            ELSE iPLAcct
            DTType     = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "DTType",
                                         GetXAttrInit(loan.class-code, "DTType"))
            DTType     = IF DTType = ? OR DTType = ""
                            THEN "*"
                            ELSE DTType
            DTKind     = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "DTKind",
                                         GetXAttrInit(loan.class-code, "DTKind"))
            DTKind     = IF DTKind = ? OR DTKind = ""
                            THEN "*"
                            ELSE DTKind
            mask       = iAcctRole + CHR(1) + DTType + CHR(1) + DTCust + CHR(1) +
                            DTKind + CHR(1) + ( IF loan.currency = "" THEN "Руб" ELSE "Вал")
            pick-value = ?
         .
            /* Получим маску счета из классификатора (PLAcct) */
         RUN placct.p (iPLAcct,
                       iPLAcct,
                       "PlAcct",
                       0).

         IF     pick-value NE ?
            AND pick-value NE "" THEN
         DO:
            ASSIGN
               vBal2      = ENTRY(1, ENTRY(1, pick-value, "."), "*")
               vBal2      = IF LENGTH(vBal2) GE 5 THEN SUBSTR(vBal2, 1, 5) ELSE ""
               vFlt[1]    = "MustOffLdFlt|acct-cat"  + CHR(1) + "bal-acct"           + CHR(1) +
                            "acct"                   + CHR(1) + "currency"           + CHR(1) + "close-date1"
               vFlt[2]    = "YES|" + ( IF iAcctCat      EQ ? THEN "*" ELSE iAcctCat)      + CHR(1) +
                                     ( IF vBal2         EQ ? THEN "*" ELSE vBal2)         + CHR(1) +
                                     ( IF pick-value    EQ ? THEN "*" ELSE pick-value)    + CHR(1) +
                                                                 "*"                     + CHR(1) + "?"
               pick-value = ?
            .
            IF iMess THEN
               RUN Fill-SysMes IN h_tmess ("", "", "",
                                           "Выберите счет с ролью <" + iAcctRole + ">").

               /* Выбираем счет */
            DO TRANS:
               RUN browseld.p ("acct",
                               vFlt[1],
                               vFlt[2],
                               "",
                               4).
            END.
            IF pick-value NE ? THEN
               oAcct = ENTRY(1, pick-value).
         END.
      END.
   END.
END.

   /* ================---=-=-=-=-=-=-=-=- */
 /* Прослойка для вызова CRCONDEX */
PROCEDURE CRCOND:
   DEF INPUT PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.
   DEF INPUT PARAM iLIstOutStav AS CHAR NO-UNDO. /* пропускать ставки , котрые не нужно копировать со старого условия*/

   RUN CRCONDEX(iContract,
                iContCode,
                iDate,
                iLIstOutStav,
                ?,
                ?).

END PROCEDURE.

/* =============================================================== */
   /* Процедура создания условий на дату */
PROCEDURE CRCONDEX:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.
   DEF INPUT PARAM iLIstOutStav AS CHAR NO-UNDO. /* пропускать ставки , котрые не нужно копировать со старого условия*/
   DEF INPUT PARAM iPaySum      AS DEC  NO-UNDO.
   DEF INPUT PARAM iPayType     AS CHAR NO-UNDO.

   DEF VAR vSurr      AS CHAR NO-UNDO.
   DEF VAR vNDays     AS INT64  NO-UNDO.
   DEF VAR vNMonthes  AS INT64  NO-UNDO.
   DEF VAR vNYears    AS INT64  NO-UNDO.
   DEF VAR vPov       AS DEC  NO-UNDO.
   DEF VAR vProcs     AS CHAR NO-UNDO.
   DEF VAR vCntr      AS INT64  NO-UNDO.
   DEF VAR vlgot      AS INT64   NO-UNDO.
   DEF VAR vlgotpr    AS INT64   NO-UNDO.
   DEF VAR vErrMsg    AS CHAR   NO-UNDO.

    DEF VAR vSumOD     AS DECIMAL NO-UNDO .

   DEF BUFFER b-loan       FOR loan.      /* Сделка */
   DEF BUFFER loan-cond    FOR loan-cond.
   DEF BUFFER b-loan-cond  FOR loan-cond. /* Условия сделки */
   DEF BUFFER comm-rate    FOR comm-rate.
   DEF BUFFER b-comm-rate  FOR comm-rate.
   DEF BUFFER c-comm-rate  FOR comm-rate.
   DEF BUFFER term-obl     FOR term-obl.
   DEF BUFFER bto-copy     FOR term-obl.
    DEF BUFFER fterm-obl    FOR term-obl.

   IF iPayType EQ ? THEN iPayType = "Остаток".
   IF iLIstOutStav EQ ? THEN iLIstOutStav = "".

   MAIN-BLOCK:
   DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

         /* Если есть условие на указанную дату ... */
      FIND FIRST b-loan-cond WHERE
                 b-loan-cond.contract  EQ iContract
         AND     b-loan-cond.cont-code EQ iContCode
         AND     b-loan-cond.since     EQ iDate
      NO-LOCK NO-ERROR.
      IF AVAIL (b-loan-cond) THEN
         LEAVE MAIN-BLOCK. /* ... то выходим */

      FIND FIRST b-loan WHERE
                 b-loan.Contract  EQ iContract
         AND     b-loan.Cont-Code EQ iContCode
      NO-LOCK NO-ERROR.
         /* Ищем предыдущие условия  */
      FIND LAST loan-cond WHERE
                loan-cond.contract  EQ iContract
            AND loan-cond.cont-code EQ iContCode
            AND loan-cond.since     LT iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-cond THEN
         LEAVE MAIN-BLOCK.

         /* Создаем список действующих ставок */
      FOR EACH comm-rate WHERE
               comm-rate.kau        EQ b-loan.contract + "," + b-loan.cont-code
          AND  NOT CAN-DO(iLIstOutStav,comm-rate.commission) NO-LOCK:
         IF LOOKUP (comm-rate.commission, vProcs) EQ 0 THEN
            vProcs = vProcs + comm-rate.commission + ",".
      END.
         /* Создаем условие */
      CREATE b-loan-cond.

      IF GetXAttrValueEx("loan-cond", loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since), "СхемаПлат","") EQ "Дифференцированная" THEN
         BUFFER-COPY loan-cond EXCEPT since int-period int-date TO b-loan-cond.
      ELSE
      BUFFER-COPY loan-cond EXCEPT since TO b-loan-cond.
      ASSIGN
         b-loan-cond.since = iDate
         vSurr             = b-loan-cond.Contract + "," +
                             b-loan-cond.Cont-Code + "," +
                             STRING(b-loan-cond.since)
      .

      /* если дата окончания договора меньше или равна дате,
      ** на которую создается условие, создать плановую сумму (idnt=2)
      ** на дату условия на сумму ноль */
      IF b-loan.end-date LE iDate THEN
      DO:
         CREATE term-obl.
         ASSIGN
            term-obl.contract    = b-loan.contract
            term-obl.cont-code   = b-loan.cont-code
            term-obl.nn          = 1
            term-obl.end-date    = iDate
            term-obl.idnt        = 2
            term-obl.fop-date    = iDate
            term-obl.currency    = b-loan.currency
            term-obl.acct        = "0"
            term-obl.cont-type   = b-loan.cont-type
            term-obl.amt-rub     = 0
         .
      END.

      IF iPaySum = ? THEN DO:
      /* Плановый остаток */
      FIND LAST  fterm-obl  WHERE
                 fterm-obl.contract  EQ b-loan.contract
            AND  fterm-obl.cont-code EQ b-loan.cont-code
            AND  fterm-obl.idnt      EQ 2
            AND  fterm-obl.end-date  LE iDate
            NO-LOCK NO-ERROR .
         IF AVAILABLE fterm-obl THEN  vSumOD = fterm-obl.amt-rub.
      END.
      ELSE
         vSumOD = iPaySum.
            
         /* Копируем и создаем ДР */
      RUN CopySigns IN h_xclass (loan-cond.Class-Code,      /* Источник */
                                 loan-cond.Contract + "," + loan-cond.Cont-Code + "," + STRING(loan-cond.since),
                                 b-loan-cond.Class-Code,    /* Приемник */
                                 vSurr)
                                 NO-ERROR.

      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "PayType",
                     iPayType).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "PaySum",
                     STRING(vSumOD)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "ВидРеструкт",
                     "").
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "CondEndDate",
                     "").
      RUN DMY_In_Per (iDate,
                      b-loan.end-date,
                      OUTPUT vNDays,
                      OUTPUT vNMonthes,
                      OUTPUT vNYears).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "NDays",
                     STRING(vNDays)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr, "NMonthes",
                     STRING(vNMonthes)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "NYears",
                     STRING(vNYears)).

/* корректируем количество льготных периодов */
      vlgot  = INT64(GetXAttrValueEx("loan-cond",
                                     vSurr,
                                     "КолЛьгтПер","0")).
      vlgotpr = INT64(GetXAttrValueEx("loan-cond",
                                      vSurr,
                                      "КолЛьгтПерПрц","0")).
      IF vlgot GT 0 THEN
      FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                          AND term-obl.cont-code EQ iContCode
                          AND term-obl.idnt      EQ 3
                          AND term-obl.end-date  GT loan-cond.since
                          AND term-obl.end-date  LE b-loan-cond.since NO-LOCK:
         vlgot = vlgot - 1.
         IF vlgot LE 0 THEN LEAVE.
      END.
      IF vlgotpr GT 0 THEN
      FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                          AND term-obl.cont-code EQ iContCode
                          AND term-obl.idnt      EQ 1
                          AND term-obl.end-date  GT loan-cond.since
                          AND term-obl.end-date  LE b-loan-cond.since NO-LOCK:
         vlgotpr = vlgotpr - 1.
         IF vlgotpr LE 0 THEN LEAVE.
      END.
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "КолЛьгтПер",
                     STRING(vlgot)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "КолЛьгтПерПрц",
                     STRING(vlgotpr)).

         /* Kопируем ставки на условие */
      DO vCntr = 1 TO NUM-ENTRIES(vProcs):
         IF ENTRY(vCntr, vProcs) NE "" THEN
         DO:
            FIND LAST comm-rate WHERE
                      comm-rate.kau        EQ iContract + "," + iContCode
               AND    comm-rate.commission EQ ENTRY(vCntr, vProcs)
               AND    comm-rate.since      LT iDate
               AND    comm-rate.acct       EQ "0"
               NO-LOCK NO-ERROR.
            IF     AVAIL comm-rate
               AND comm-rate.commission NE "%Рез" THEN
            DO:
               IF CAN-FIND (FIRST b-comm-rate WHERE
                                  b-comm-rate.kau        EQ comm-rate.kau
                              AND b-comm-rate.since      EQ iDate
                              AND b-comm-rate.commission EQ comm-rate.commission
                              AND b-comm-rate.acct       EQ comm-rate.acct) THEN
               DO:
                  vErrMsg = "Для договора ~"" + iContract + "," + iContCode + 
                            "~" уже есть ставка ~"" + comm-rate.commission +
                            "~" на " + STRING(iDate) + ".".
                  UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
               END.
               CREATE b-comm-rate.
               BUFFER-COPY comm-rate EXCEPT since comm-rate-id TO b-comm-rate NO-ERROR.
               b-comm-rate.since = iDate.
            END.
         END.
      END.
      RELEASE b-comm-rate.
   END.
   IF {assigned vErrMsg} THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1",
                                  vErrMsg).
      RETURN ERROR vErrMsg.
   END.
END PROCEDURE.

   /* ================================== */
   /* Процедура изменения существующей ставки договора на дату */
PROCEDURE UpdComm:
   DEF INPUT PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.
   DEF INPUT PARAM iCommision   AS CHAR NO-UNDO. /* Ставкa */ 
   DEF INPUT PARAM iStav        AS DEC  NO-UNDO. /* Значение */

   DEF BUFFER comm-rate    FOR comm-rate.
   MAIN-BLOCK:
   DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      IF iStav <> 0 THEN DO:
               /* Заполняем ставку iCommision нужным значением */
         FIND FIRST comm-rate WHERE
                    comm-rate.kau        EQ iContract + "," + iContCode
                AND comm-rate.commission EQ iCommision
                AND comm-rate.since      EQ iDate
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:
            IF LOCKED comm-rate THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","Комиссия заблокирована другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Комиссия не найдена").
            RETURN.
         END.

         IF AVAIL comm-rate THEN
            ASSIGN
               comm-rate.rate-comm = iStav NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
         IF AVAIL comm-rate THEN
            RELEASE comm-rate NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
   END.

END PROCEDURE.

   /* ================================== */
/* Возвращает сумму процентов, к начислению за период по ставке */
PROCEDURE NachProcRate.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* Назначение договора */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* Номер договора */
   DEF INPUT  PARAM iDateBeg    AS DATE NO-UNDO.  /* Дата начала расчета */
   DEF INPUT  PARAM iDateEnd    AS DATE NO-UNDO.  /* Дата окончания расчета */
   DEF INPUT  PARAM iCommission AS CHAR NO-UNDO.  /* Код ставки */
   DEF INPUT  PARAM iBasePar    AS CHAR NO-UNDO.  /* Параметры для рассчета базы */
   DEF OUTPUT PARAM oResult     AS DEC  NO-UNDO.

   DEF VAR vAmnt       AS DEC  NO-UNDO.   /* Сумма */
   DEF VAR vCommission AS CHAR NO-UNDO.   /* Базовая ставка */
   DEF VAR vBaseComm   AS DEC  NO-UNDO.   /* Базовая ставка %% */
   DEF VAR vPenyComm   AS DEC  NO-UNDO.   /* Штрафная ставка %% */

   DEF VAR vSurr     AS CHAR NO-UNDO.
   DEF VAR vBaseNach AS CHAR NO-UNDO.
   DEF VAR vBaseStav AS CHAR NO-UNDO.
   DEF VAR vN        AS INT64  NO-UNDO.

   DEF VAR dat-per   AS DATE NO-UNDO.

      /* Если параметры для рассчета базы не переданы - берем стандартно 0+7 */
   IF iBasePar EQ ? THEN
      iBasePar = "0+7".

   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      oResult = ?.
   ELSE
   DO:
         /* Получение даты перехода на инструкцию 39П. */
      {ch_dat_p.i}
         /* Получаем значение ДР "БазаНач" и "БазаСтав" */
      ASSIGN
         vSurr     = loan.contract + "," + loan.cont-code
         vBaseNach = GetXattrValueEx("loan", vSurr, "БазаНач",  "")
         vBaseStav = GetXattrValueEx("loan", vSurr, "БазаСтав", "")
         vN        = NUM-ENTRIES(vBaseStav)
      .
         /* Добавим в ДР комиссию и Параметры для расчета базы */
      UpdateSignsEx(loan.Class-Code, vSurr, "БазаНач",  vBaseNach + ( IF vN EQ 0 THEN "" ELSE ",") + iBasePar).
      UpdateSignsEx(loan.Class-Code, vSurr, "БазаСтав", vBaseStav + ( IF vN EQ 0 THEN "" ELSE ",") + iCommission).

         /* Обнуляем табличку */
     {empty otch1}
         /* Запускаем начисление только по переданной комиссии */
      RUN lnscheme.p (iContract,    /* Назначение договора */
                      iContCode,    /* Номер договора */
                      iDateBeg,     /* Дата начала расчета */
                      iDateEnd,     /* Дата окончания расчета */
                      dat-per,      /* Дата перехода на 39П */
                      vN + 1,       /* Индекс ставки в ДР "БазаНач" */
                      1).           /* Всегда передается 1 не исп. */
         /* "Собираем" суммы к начислению */
      FOR EACH otch1:
         vAmnt = vAmnt + otch1.summ_pr.
      END.
         /* Возвращаем назад значения ДР */
      UpdateSignsEx(loan.Class-Code, vSurr, "БазаНач",  vBaseNach).
      UpdateSignsEx(loan.Class-Code, vSurr, "БазаСтав", vBaseStav).

      oResult = vAmnt.
   END.

   RETURN.
END PROCEDURE.

/* Создание нового условия по овердрафту */
PROCEDURE CrNewTermObl.
   DEFINE INPUT  PARAMETER iContract AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSince    AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iSumma    AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrMes   AS CHARACTER   NO-UNDO.

   DEF BUFFER bLoanCond  FOR loan-cond.
   DEF BUFFER bLoanCond2 FOR loan-cond.
   DEF BUFFER bSigns     FOR signs.
   DEF BUFFER bSigns2    FOR signs.
   DEF BUFFER bTermObl   FOR term-obl.
   DEF BUFFER bTermObl2  FOR term-obl.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      /* Поиск условия */
      RUN RE_L_COND (iContract,
                     iContCode,
                     iSince,
                     BUFFER bLoanCond).

      CREATE bLoanCond2.
      BUFFER-COPY bLoanCond EXCEPT since TO bLoanCond2.
      bLoanCond2.since = iSince.

      FOR EACH bSigns WHERE bSigns.file-name EQ "loan-cond"
                        AND bSigns.surrogate EQ bLoanCond2.contract + "," + bLoanCond2.cont-code
         NO-LOCK:

         CREATE bSigns2.
         BUFFER-COPY bSigns EXCEPT surrogate TO bSigns2.
         bSigns2.surrogate = bLoanCond2.contract + "," + bLoanCond2.cont-code.
      END.

      /* Поиск планового остатка */
      RUN RE_TERM_OBL (iContract,
                       iContCode,
                       2,
                       bLoanCond.since,
                       BUFFER bTermObl).

      CREATE bTermObl2.
      BUFFER-COPY bTermObl EXCEPT end-date amt-rub TO bTermObl2.
      ASSIGN
         bTermObl2.amt-rub   = iSumma
         bTermObl2.end-date  = iSince
         .
   END.
END PROCEDURE.

   /* поиск индивидуальной ставки по досрочному закрытию депозита */
PROCEDURE pGetBefDepRate.
   DEFINE INPUT  PARAMETER iContract AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRate     AS DEC  NO-UNDO.

   DEF VAR vNumDays  AS INT64  NO-UNDO.
   DEF VAR vProdCode AS CHAR NO-UNDO.

   DEF BUFFER loan     FOR loan.
   DEF BUFFER pro-obl  FOR pro-obl.
   DEF BUFFER ttRate   FOR ttRate.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
             AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE MAIN.
      RUN GetDepDosr (BUFFER loan) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         LEAVE MAIN.
         /* если реквизита нет, то заполняем с indicate */
      IF NOT CAN-FIND(FIRST ttRate) THEN
         RUN FillIntoIndicate (BUFFER loan).
      FOR EACH pro-obl WHERE
               pro-obl.contract  EQ loan.contract
           AND pro-obl.cont-code EQ loan.cont-code
           AND pro-obl.idnt      EQ 3
      NO-LOCK BY pro-obl.pr-date DESC:
         LEAVE.
      END.
      IF AVAIL pro-obl THEN
         vNumDays = iDate - pro-obl.pr-date.
      ELSE
         vNumDays = iDate - loan.open-date.
      FIND FIRST ttRate WHERE
              ttRate.firstday LE vNumDays
          AND ttRate.lastday  GE vNumDays
      NO-LOCK NO-ERROR.
      IF AVAIL ttRate THEN
         oRate = ttRate.rate.
   END.
END PROCEDURE.

/* Анализ параметров договора на пустоту для закрытия договора,   */
/* ilist-ost - параметры которые не нужно анализировать */
/* oIsValidClose - флаг возможности закрытия */
/* oSumm - сумма не нулевого параметра */
/* oParam - номер не нулевого параметра */
PROCEDURE ValidCloseLoan.
   DEFINE INPUT   PARAMETER iContract     AS CHAR NO-UNDO.
   DEFINE INPUT   PARAMETER iContCode     AS CHAR NO-UNDO.
   DEFINE INPUT   PARAMETER ilist-ost     AS CHAR NO-UNDO.   
   DEFINE OUTPUT  PARAMETER oIsValidClose AS LOGICAL NO-UNDO.
   DEFINE OUTPUT  PARAMETER oSumm         AS DECIMAL NO-UNDO.
   DEFINE OUTPUT  PARAMETER oParam        AS INT64   NO-UNDO.
   
   
   DEFINE VAR list-ost  AS CHAR NO-UNDO.
   DEFINE VAR vPrmCurr  AS CHAR NO-UNDO.
   DEFINE VAR vNachSumm AS DEC  NO-UNDO.
   
   DEFINE VAR summ   LIKE loan-var.balance.
   
   FIND FIRST loan WHERE 
              loan.contract EQ iContract
          AND loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   
   oIsValidClose = YES.
   
   FOR EACH loan-par WHERE loan-par.amt-id < 1000 NO-LOCK:

      IF loan-par.amt-id = 1 OR
         loan-par.amt-id = 3 OR
         loan-par.amt-id = 5
      THEN
         NEXT.

       summ = 0.

      /* Необрабатываемые параметры */
      IF CAN-DO(ilist-ost, STRING(loan-par.amt-id)) THEN 
      DO:
         NEXT.
      END.

      IF mass[loan-par.amt-id + 1] NE 0 THEN
         summ = loan.interest[mass[loan-par.amt-id + 1]].

      

      FIND LAST loan-var
         {wh-t &f=loan-var &i=loan-par.amt-id}
      NO-LOCK NO-ERROR.

      IF     AVAIL loan-var
         AND loan-var.balance NE 0
      THEN
         summ = summ + loan-var.balance.
   
      
      IF AVAIL loan-var THEN
         vPrmCurr = loan-var.currency.
      ELSE
         vPrmCurr = "".

      vNachSumm = 0.
      
      RUN CalcPrmValue (loan.contract,
                        loan.cont-code,
                        loan-par.amt-id,
                        summ,
                        vPrmCurr,
                        OUTPUT vNachSumm).

      IF vNachSumm NE ? THEN
          ASSIGN
             summ = vNachSumm 
          .
      
      

      
      IF summ NE 0 THEN
      DO:
         oIsValidClose = NO.
         oSumm = summ.
         oParam = loan-par.amt-id.
         
         RETURN.
      END.      
   END.
END.



PROCEDURE CloseLoan.
   DEFINE INPUT  PARAMETER iContract AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER iWhat     AS INT64  NO-UNDO.

   DEFINE VAR mess      AS CHAR    FORMAT "x(64)".
   DEFINE VAR mess1     AS CHAR    FORMAT "x(20)".
   DEFINE VAR fl-o      AS INT64.
   DEFINE VAR fl-er     AS LOG  INIT YES.
   DEFINE VAR i         AS INT64.
   DEFINE VAR mPayDate  AS DATE NO-UNDO.
   DEFINE VAR vDateN    AS DATE NO-UNDO.
   DEFINE VAR kk        AS INT64  INIT 0.
   DEFINE VAR list-ost  AS CHAR NO-UNDO.
   DEFINE VAR vPrmCurr  AS CHAR NO-UNDO.
   DEFINE VAR vNachSumm AS DEC  NO-UNDO.
   DEFINE VAR j         AS INT64     INIT 2.
   DEFINE VAR i1        AS INT64.
   DEFINE VAR j1        AS INT64.
   DEFINE VAR fl-undo   AS LOG  INIT NO.    /* yes - Обнаружена ошибка, откат  */
   DEFINE VAR fl1       AS LOG.
   DEFINE VAR fl-er1    AS LOG  INIT YES.
   DEFINE VAR vDbSumDec AS DEC  NO-UNDO.
   DEFINE VAR vCrSumDec AS DEC  NO-UNDO.
   DEFINE VAR vPar      AS CHAR NO-UNDO INIT '33,29,10'.
   DEFINE VAR vSumm     AS DEC  NO-UNDO.
   DEFINE VAR list_type    AS CHAR NO-UNDO. /* список ролей счетов для автоматич.
                                         ** закрытия при закрытии договора */
   DEFINE VAR was_noerr    AS LOG  NO-UNDO. /*yes - успешное закрытии счетов */
   DEFINE VAR isValidCloseLoan AS LOG  NO-UNDO. /* Пусты ли параметры */
   DEFINE VAR vamt-id AS INT64 NO-UNDO.

   DEFINE VAR summ   LIKE loan-var.balance.
   DEFINE VAR e1     LIKE loan-var.balance.
   DEFINE VAR e2     LIKE loan-var.balance.
   DEFINE VAR e3     LIKE loan-var.balance.
   DEFINE VAR summ-t LIKE term-obl.amt     COLUMN-LABEL "ФАКТИЧЕСКИЙ ОСТАТОК".

   DEFINE VARIABLE vPutStrem  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vDispMess  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vDispMess2 AS LOGICAL     NO-UNDO.

   DEFINE BUFFER xerm-obl FOR term-obl.
   DEFINE BUFFER loan     FOR loan. /* Локализация буфера. */

   &GLOB iskldbparam "95"

   FIND FIRST loan WHERE loan.contract  EQ iContract
                     AND loan.cont-code EQ iContCode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF NOT AVAILABLE loan THEN
   DO:
      IF LOCKED loan THEN
           RUN Fill-SysMes IN h_tmess("","","-1","Договор заблокирован другим пользователем").
      ELSE RUN Fill-SysMes IN h_tmess("","","-1","Договор не найден").
      RETURN.
   END.

   IF AVAIL loan
   THEN DO:

      list-ost  = FGetSetting("ИсклПарЗакр",?,"") + ",19,36".
      list_type = GetXAttrInit(loan.class-code,"list_type").
      ASSIGN
         vPutStrem  = LOOKUP("PutStrem" ,SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
         vDispMess  = LOOKUP("DispMess" ,SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
         vDispMess2 = LOOKUP("DispMess2",SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
      .

      IF vPutStrem
      THEN
         RUN PutStrem IN SOURCE-PROCEDURE
            (FILL (" ",29) + "~n~nЗАКРЫТИЕ ДОГОВОРА  " + STRING(LOAN.CONT-CODE) + "~n~n" +
             FILL (" ",29) + "Сообщения об ошибках:" + "~n") NO-ERROR.

      LOANCLOSE:
      DO
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

         IF loan.end-date GT iDate THEN
         DO:
            {f-error.i &CLASS   = КодОш
                       &par     = 1l
                       &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.
         IF     iWhat NE 1
            AND DATE(iDate + 1) LT loan.since THEN /* договор может быть пересчитан на 1 день вперед */
         DO:
            {f-error.i &class   = КодОш
                       &par     = 2l
                       &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.


         IF loan.since LE iDate THEN
         DO:

            {loancalc.i
               &incontr = "loan.contract"
               &incontc = "loan.cont-code"
               &dr      = "iDate + 1"
            }

            IF loan.since <> iDate + 1 THEN
            DO:
               IF vPutStrem
               THEN
                  RUN PutStrem IN SOURCE-PROCEDURE
                     ("Ошибки при попытке пересчитать договор.").
               RETURN.
            END.
         END.


                  /* Поиск последней операции по договору */
         FIND LAST loan-int USE-INDEX prim
              {wh-t &f=loan-int &c="/*"}
         NO-LOCK NO-ERROR.

         IF     AVAIL loan-int
            AND loan-int.mdate GT loan.since
         THEN DO:
            {f-error.i
               &class   = КодОш
               &par     = 3l
               &params  = "(loan.contract,loan.cont-code,loan-int.mdate + 1)"
               &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.

         i = 0.
                     /* Определяем даты пересчета и начального решения по Договорам */
         ASSIGN
            mPayDate = loan.since
            vDateN   = DATE(FGetSettingEx("ДатаНачКред", ?, "", NO))
            vDateN   = IF vDateN EQ ? THEN DATE(1,1,1900) ELSE vDateN
         .

         RUN ValidCloseLoan(loan.contract,
                            loan.cont-code,
                            FGetSetting("ИсклПарЗакр",?,"") + ",19,36",
                            OUTPUT isValidCloseLoan,
                            OUTPUT summ,
                            OUTPUT vamt-id  ).

         IF NOT isValidCloseLoan THEN
         DO:
               IF fl-er THEN
               DO:
                  mess = "ОШИБКА: Ненулевые значения параметров договоров".
                  IF vDispMess
                  THEN
                     RUN DispMess IN SOURCE-PROCEDURE (mess).

                  mess = "ПАРАМЕТРЫ ДОГОВОРА.".

                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",29) + mess +  "~n"
                        + FILL(" ",3)  + "Код  Наименование параметра"
                        + FILL(" ",19) + "Остаток" + "~n").
                  fl-er = NO.
               END.

               {f-error.i &class   = КодОш
                          &par     = 4l
                          &params  = "(vamt-id,summ)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }
         END.
         

         fl-er = YES.

         FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                             AND term-obl.cont-code EQ loan.cont-code
                             AND term-obl.idnt      EQ 3
                             AND term-obl.sop-date  EQ ?
         EXCLUSIVE-LOCK:

            {summ-t.i}

            IF fl-er THEN
            DO:
               mess = "Предупреждение: Есть незакрытые срочные обязательства".
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess) .
               fl-er = NO.
            END.

            IF summ-t EQ 0 THEN
            DO:
               ASSIGN
                  term-obl.sop-date = iDate
                  mess = "Закрыто срочное обязательство с плановой датой : " + string(term-obl.end-date).
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess).
            END.
            ELSE
            DO:
               fl-Undo = YES. /* Флаг для отката необходимо добавить */
               IF fl-er1 THEN
               DO:
                  mess = "Ошибка: Есть срочные обязательства с ненулевым остатком".
                  IF vDispMess
                  THEN
                     RUN DispMess IN SOURCE-PROCEDURE (mess).

                  mess = "СРОЧНЫЕ ОБЯЗАТЕЛЬСТВА С НЕНУЛЕВЫМ ОСТАТКОМ " + ":".
                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",19) + mess + "~n"
                        +                " Плановая дата"
                        + FILL(" ",11) + "Сумма"
                        + FILL(" ",9)  + "Фактический остаток").
                  fl-er1 = NO.
               END.

               {f-error.i &class   = КодОш
                          &par     = 5l
                          &params  = "(recid(term-obl),summ-t)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }
            END.
         END.

         ASSIGN
            fl-er  = YES
            fl-er1 = YES
         .

         FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                             AND term-obl.cont-code EQ loan.cont-code
                             AND term-obl.idnt      EQ 1
                             AND term-obl.sop-date  EQ ?
         EXCLUSIVE-LOCK:

            {summ-t1.i}

            IF fl-er THEN
            DO:
               mess = "ПРЕДУПРЕЖДЕНИЕ: Есть незакрытые обязательства по погашению процентов".
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess) .
               fl-er = NO.
            END.
            IF summ-t EQ 0 THEN
            DO:
               ASSIGN
                  term-obl.sop-date  = iDate
                  mess = "Закрыто обязательство с плановой датой : " + string(term-obl.end-date).

               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess).
            END.
            ELSE
            DO:
               fl-undo = YES.  /* Флаг для отката необходимо добавить */
               IF fl-er1 THEN
               DO:
                  ASSIGN
                     mess = "Ошибка: Есть обязательства по погашению процентов "
                     mess1 = "с ненулевым остатком".

                  IF vDispMess2
                  THEN
                     RUN DispMess2 IN SOURCE-PROCEDURE (mess,mess1) .

                  mess = "ОБЯЗАТЕЛЬСТВА ПО ПОГАШЕНИЮ ПРОЦЕНТОВ С НЕНУЛЕВЫМ ОСТАТКОМ  " + ":".
                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",14) + mess + "~n"
                        +                " Плановая дата"
                        + FILL(" ",11) + "Сумма"
                        + FILL(" ",9)  + "Фактический остаток") .

                  fl-er1 = NO.
               END.

               {f-error.i &class   = КодОш
                          &par     = 5l
                          &params  = "(recid(term-obl),summ-t)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }

            END.
         END.

         /* закрытие счетов (при условии, что успешно прошло остальное) */
         IF fl-o = 0 THEN
         DO:
            RUN close_acct.p(RECID(loan),
                             iDate,
                             OUTPUT was_noerr,
                             OUTPUT mess).
            IF vPutStrem
            THEN
               RUN PutStrem IN SOURCE-PROCEDURE (mess) .
            IF NOT was_noerr THEN
            DO:

               fl-o = fl-o + 1.
               UNDO LOANCLOSE, LEAVE LOANCLOSE.
            END.
         END.

         IF     NOT  fl-undo
            AND     fl-o = 0 THEN
            ASSIGN
               mess             = "~nДОГОВОР ЗАКРЫТ "
               loan.close-date  = iDate
               loan.loan-status = "ЗАКР".
         ELSE
         DO:
            ASSIGN
               mess            = "~nДОГОВОР НЕ ЗАКРЫТ"
               mess1           = "откат всех изменений"
            .
            IF vPutStrem
            THEN
               RUN PutStrem IN SOURCE-PROCEDURE (mess + "~n" + mess1).
            UNDO LOANCLOSE, LEAVE LOANCLOSE.
         END.
      END.
      IF vPutStrem
      THEN
         RUN PutStrem IN SOURCE-PROCEDURE (mess).


   END.

END PROCEDURE.

/* Функция возвращает истину, если переданная дата входит в льготный период договора (без пробега) */
FUNCTION fLgPeriod RETURN LOGICAL (
   iContract AS CHARACTER,
   iContCode AS CHARACTER,
   iDate     AS DATE
):
   DEF VAR oResult   AS LOG    NO-UNDO.
   DEF VAR vLgCnt    AS INT64    NO-UNDO.   /* Кол-во льготных периодов */
   DEF VAR vi        AS INT64    NO-UNDO.

   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bterm-obl  FOR term-obl.

   MAIN_BLOCK:
   DO:
      RUN RE_L_COND (iContract,iContCode,iDate,BUFFER bloan-cond).
      IF NOT AVAIL bloan-cond THEN LEAVE MAIN_BLOCK.

            /* Получить количество льготных периодов по %%, заданных на текущем условии */
      vLgCnt = INT64(GetXAttrValueEx("loan-cond",
                                   bloan-cond.contract    + "," +
                                   bloan-cond.cont-code   + "," +
                                   STRING(bloan-cond.since),
                                   "КолЛьгтПерПрц",
                                   "0")).

            /*Поиск первого не льготного платежа*/
      FOR EACH bterm-obl WHERE bterm-obl.contract  EQ bloan-cond.contract
                           AND bterm-obl.cont-code EQ bloan-cond.cont-code
                           AND bterm-obl.idnt      EQ 1
                           AND bterm-obl.nn        EQ 1
      NO-LOCK
      vi = 1 TO vLgCnt:
      END.
      IF NOT AVAIL bterm-obl THEN LEAVE MAIN_BLOCK.

            /* Сравниваем дату платежа (term-obl.end-date) с переданной датой */
      IF iDate LT bterm-obl.end-date THEN oResult = YES. /* переданная дата находится внутри льготного периода по договору */
   END.
   RETURN oResult.
END FUNCTION.
/* Процедура расчета курсовых разниц              */
FUNCTION fNVPIKurs RETURN DECIMAL (
   iContract AS CHARACTER,     /* тип договора */
   iContCode AS CHARACTER,     /* код договора */
   iDate     AS DATE,          /* Дата операции */
   iCodPar   AS CHARACTER,     /* Параметр договора */
   iSumm     AS DECIMAL,       /* сумма погашения   */
   iSign     AS CHARACTER      /* ПОЛ или ОТР какую курсовую разницу считаем*/
   ) :

DEF VAR oResult   AS DECIMAL    NO-UNDO.
DEF VAR vSumProv  AS DECIMAL NO-UNDO .

DEF BUFFER b-loan   FOR loan.

      IF iSumm EQ 0 THEN DO:
         oResult = 0 .
         RETURN oResult.
      END.
      FIND FIRST b-loan WHERE b-loan.contract   =  iContract
                          AND b-loan.cont-code  =  iContCode
                          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE b-loan
      THEN DO:
         oResult = 0 .
         RETURN oResult.
      END.
      IF b-loan.currency = ""   /* С рублевыми комиссиями курсовых разниц не возникает */
      THEN DO:
         oResult = 0 .
          RETURN oResult.
      END.

   /* Расчет курсовой разницы */
      RUN PROC_NVPIKurs IN THIS-PROCEDURE (
         INPUT  iContract ,
         INPUT  iContCode ,
         INPUT  iDate     ,
         INPUT  iCodPar   ,
         INPUT  iSumm     ,
         INPUT  b-loan.currency,
         INPUT  iSign     ,
         OUTPUT vSumProv  ).

       oResult = 0.
       IF iSign EQ "ПОЛ"  AND
          vSumProv > 0
       THEN
         ASSIGN
            oResult = vSumProv
         .

       IF iSign NE "ПОЛ"  AND
          vSumProv < 0
       THEN
         ASSIGN
            oResult = ABS(vSumProv)
         .

   RETURN oResult.
END FUNCTION.

/* Вспомогательные таблицы для процедуры PROC_NVPIKurs*/
DEFINE TEMP-TABLE tt-loan-int NO-UNDO LIKE loan-int
FIELD Kurs AS DECIMAL
FIELD Id AS INTEGER
.
DEFINE TEMP-TABLE tt-nach NO-UNDO LIKE loan-int
FIELD Kurs AS DECIMAL
FIELD Id AS INTEGER
.

/* Процедура расчета курсовых разниц   */
PROCEDURE PROC_NVPIKurs :
DEFINE INPUT PARAMETER    iContract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iDate     AS DATE NO-UNDO      .
DEFINE INPUT PARAMETER    iCodPar   AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iSumm     AS DECIMAL  NO-UNDO  .
DEFINE INPUT PARAMETER    iCurrency AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iSign     AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER   oSumProv  AS DECIMAL NO-UNDO   .

DEF VAR loan_ost   AS DECIMAL NO-UNDO .
DEF VAR loan_cr    AS DECIMAL NO-UNDO .
DEF VAR loan_db    AS DECIMAL NO-UNDO .
DEF VAR vFirst     AS LOGICAL NO-UNDO .
DEF VAR vFlagDel   AS LOGICAL NO-UNDO .
DEF VAR vSummDelta AS DECIMAL NO-UNDO .
DEF VAR vNN        AS INT64 NO-UNDO .

DEF BUFFER loan-int FOR loan-int.

EMPTY TEMP-TABLE tt-loan-int.
EMPTY TEMP-TABLE tt-nach.

   /* Найдем список операций с первой непогашенной */
   vFirst = FALSE .
   cycle_loan_int:
   FOR EACH loan-int WHERE
            loan-int.contract    = iContract
         AND loan-int.cont-code  = iContCode
         AND loan-int.mdate     <= iDate
         AND ( loan-int.id-d     = INT64(iCodPar)
         OR    loan-int.id-k     = INT64(iCodPar))
         NO-LOCK
            BY loan-int.mdate DESC
            BY loan-int.nn    DESC:
      /* определим остаток на праметре исходящий по операции */
      RUN STNDRT_PARAM (icontract,
                        icontcode,
                        INT64(iCodPar),
                        loan-int.mdate,
                        OUTPUT loan_ost,
                        OUTPUT loan_cr ,
                        OUTPUT loan_db
                        ).
      IF loan-int.id-d = INT64(iCodPar) THEN DO:
         IF loan_ost  - loan-int.amt-rub  > 0  /* входящий остаток на параметре по операции */
         THEN DO:
            CREATE tt-loan-int.
            BUFFER-COPY loan-int TO tt-loan-int
            ASSIGN
               tt-loan-int.kurs = FindRateWork ("УЧЕТНЫЙ",iCurrency,loan-int.mdate)
            .
         END.
         ELSE DO:
            IF vFirst = FALSE THEN DO:
               CREATE tt-loan-int.
               BUFFER-COPY loan-int TO tt-loan-int
               ASSIGN
                  tt-loan-int.kurs = FindRateWork ("УЧЕТНЫЙ",iCurrency,loan-int.mdate)
               .
               vFirst = TRUE .
            END.
            LEAVE cycle_loan_int.
         END.
      END.
      ELSE DO:
         /* уплаты */
         CREATE tt-loan-int.
         BUFFER-COPY loan-int TO tt-loan-int
         ASSIGN
            tt-loan-int.kurs = FindRateWork ("УЧЕТНЫЙ",iCurrency,loan-int.mdate)
         .
      END.
   END.

   /* перенумеруем по порядку и отсечем то, что не погаситься */
   vNN = 0 .
   vFlagDel = FALSE.



M1:
   FOR EACH tt-loan-int :
     IF vFlagDel = TRUE
     THEN DO:
       DELETE tt-loan-int.
     END.
     ELSE DO:
         vNN = vNN + 1.
         tt-loan-int.id = vNN.
         /* погашения за опердень должно быть на переданную  сумму iSumm*/
         IF tt-loan-int.id-k   =  INT64(iCodPar) AND
            tt-loan-int.mdate  =  iDate
         THEN DO:
            iSumm = iSumm - tt-loan-int.amt-rub.

            IF iSumm = 0
            THEN DO:
               ASSIGN
                  vFlagDel = TRUE
               .
               NEXT M1.
            END.
            IF iSumm < 0
            THEN DO:
               ASSIGN
                  vFlagDel = TRUE
                  iSumm  = 0
                  tt-loan-int.amt-rub = ABS(iSumm)
                  tt-loan-int.mdate   = iDate
                  .
               NEXT M1.
            END.
         END.
     END.
   END. /* FOR EACH m1 */

   IF iSumm > 0
   THEN DO:
      vNN = vNN + 1.
      CREATE tt-loan-int.
      ASSIGN
         tt-loan-int.Contract  = iContract
         tt-loan-int.Cont-code = iContCode
         tt-loan-int.id-d    = ?
         tt-loan-int.id-k    = INT64(iCodPar)
         tt-loan-int.amt-rub = iSumm
         tt-loan-int.mdate   = iDate
         tt-loan-int.id      = vNN
         tt-loan-int.nn      = vNN
          .
         tt-loan-int.Kurs    = FindRateWork ("УЧЕТНЫЙ",iCurrency,iDate)
      .
   END.


   oSumProv = 0.
   FOR EACH tt-loan-int
         BY tt-loan-int.id
          :
       IF tt-loan-int.id-d   = INT64(iCodPar) /* Начисление */
       THEN DO:
         CREATE tt-nach.
         BUFFER-COPY tt-loan-int TO tt-nach.
       END.
       ELSE DO: /* Уплата */
         FOR EACH  tt-nach WHERE
                   tt-nach.amt-rub > 0 AND
                   tt-nach.id < tt-loan-int.id
                   :
            IF TT-nach.amt-rub <= TT-loan-int.amt-rub
            THEN
               ASSIGN
                   vSummDelta = TT-nach.amt-rub
               .
            ELSE
               ASSIGN
                  vSummDelta = TT-loan-int.amt-rub
               .

            /* Если считаем положительную КР: */
            IF iSign EQ "ПОЛ"  AND
             ( TT-loan-int.kurs - tt-nach.kurs ) > 0
            THEN
               oSumProv = oSumProv + ( vSummDelta * (TT-loan-int.kurs - tt-nach.kurs)) .

            /* Если считаем отрицательную КР: */
            IF iSign EQ "ОТР"  AND
             ( TT-loan-int.kurs - tt-nach.kurs ) < 0
            THEN
               oSumProv = oSumProv + ( vSummDelta * (TT-loan-int.kurs - tt-nach.kurs)) .


            IF TT-nach.amt-rub <= TT-loan-int.amt-rub
            THEN
               ASSIGN
                  TT-loan-int.amt-rub  = TT-loan-int.amt-rub - vSummDelta
                  TT-nach.amt-rub = 0
               .
            ELSE
               ASSIGN
                  TT-nach.amt-rub     = TT-nach.amt-rub     - vSummDelta
                  TT-loan-int.amt-rub = TT-loan-int.amt-rub - vSummDelta
               .
         END. /* FOR EACH  по начислениям */
       END.
   END.  /* FOR EACH tt-loan-int по операциям */
END PROCEDURE.

PROCEDURE pTranshFill:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* Код документа */
   DEF INPUT  PARAM iOpEntry     AS INT64 NO-UNDO. /* Код проводки */
   DEF INPUT  PARAM iSide        AS LOG   NO-UNDO. /* Дт/Кр */
   DEF INPUT  PARAM iTranshOnly  AS LOG   NO-UNDO. /* Только для траншей */
   DEF INPUT  PARAM iRole        AS CHAR  NO-UNDO. /* Роль счета */
   DEF INPUT  PARAM iContract    AS CHAR  NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode    AS CHAR  NO-UNDO. /* Код договора */

   DEF VAR vAcct  AS CHAR  NO-UNDO.
   DEF VAR vCurr  AS CHAR  NO-UNDO.
   DEF VAR vDate  AS DATE  NO-UNDO.

   DEF BUFFER op-entry     FOR op-entry.
   DEF BUFFER loan-acct    FOR loan-acct.
   DEF BUFFER bloan-acct   FOR loan-acct.
   DEF BUFFER tt-Transh    FOR tt-Transh.

   MAIN:
   DO:
      FIND FIRST op-entry WHERE
                 op-entry.op       EQ iOp
             AND op-entry.op-entry EQ iOpEntry
      NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         LEAVE MAIN.
      RUN pTranshClear (iOp, iOpEntry).
      ASSIGN
         vAcct       = IF iSide THEN op-entry.acct-db ELSE op-entry.acct-cr 
         vCurr       = op-entry.currency
         vDate       = op-entry.op-date
         iTranshOnly = IF iTranshOnly EQ ? THEN YES ELSE iTranshOnly                
         iRole       = IF NOT {assigned iRole} THEN "*" ELSE iRole                
         iContract   = IF iContract EQ ? THEN "" ELSE iContract
         iContCode   = IF iContCode EQ ? THEN "" ELSE iContCode
      .
      FOR EACH loan-acct WHERE
         (
           (   iContract           EQ ""
           AND iContCode           EQ ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           )
         OR
           (   iContract           NE ""
           AND iContCode           EQ ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           AND loan-acct.contract  EQ iContract
           )
         OR
           (   iContract           NE ""
           AND iContCode           NE ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           AND loan-acct.contract  EQ iContract
           AND (   loan-acct.cont-code BEGINS iContCode + " "
               OR (NOT iTranshOnly
                   AND loan-acct.cont-code EQ iContCode
                   )
               )
           )
         )
         AND CAN-DO (iRole, loan-acct.acct-type)
         AND loan-acct.since LE vDate
         AND ( NOT iTranshOnly
            OR NUM-ENTRIES (loan-acct.cont-code, " ") GT 1
             )
         AND NOT CAN-FIND(FIRST bloan-acct WHERE
                                bloan-acct.contract  EQ loan-acct.contract
                            AND bloan-acct.cont-code EQ loan-acct.cont-code
                            AND bloan-acct.acct-type EQ loan-acct.acct-type
                            AND bloan-acct.since     GT loan-acct.since
                            AND bloan-acct.since     LE vDate
                          )
      NO-LOCK,
         FIRST loan WHERE
               loan.contract   EQ loan-acct.contract
           AND loan.cont-code  EQ loan-acct.cont-code
           AND loan.close-date EQ ?
           AND loan.open-date  LE vDate
      NO-LOCK:
          CREATE tt-Transh.
         ASSIGN
            tt-Transh.op        = op-entry.op
            tt-Transh.op-entry  = op-entry.op-entry
            tt-Transh.contract  = loan-acct.contract
            tt-Transh.cont-code = loan-acct.cont-code
            tt-Transh.acct-type = loan-acct.acct-type
            tt-Transh.currency  = loan-acct.currency
            tt-Transh.since     = loan-acct.since
            tt-Transh.open-date = loan.open-date
            tt-Transh.end-date  = loan.end-date
            tt-Transh.Side      = iSide
         .
      END.
   END.
END PROCEDURE.

PROCEDURE pTranshCheck:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* Код документа */
   DEF INPUT  PARAM iOpEntry     AS INT64 NO-UNDO. /* Код проводки */
   DEF INPUT  PARAM iCheckLoan   AS LOG   NO-UNDO. /* Проверка на уникальность договора в выборке */
   DEF INPUT  PARAM iCheckRole   AS LOG   NO-UNDO. /* Проверка на уникальность рли счета в выборке */
   DEF OUTPUT PARAM oResult      AS LOG   NO-UNDO.

   DEF BUFFER tt-Transh    FOR tt-Transh.
   DEF BUFFER btt-Transh   FOR tt-Transh.
   DEF BUFFER loan         FOR loan.
   DEF BUFFER bloan        FOR loan.

   oResult = YES.
   IF iCheckLoan AND oResult THEN
         /* проверка наличия в выборке траншей разных договоров */
      FOR FIRST tt-Transh WHERE
                tt-Transh.op       EQ iOp
            AND tt-Transh.op-entry EQ iOpEntry,
          FIRST loan WHERE
                loan.contract  EQ tt-Transh.contract
            AND loan.cont-code EQ ENTRY (1, tt-Transh.cont-code, " ")
          NO-LOCK,
          FIRST btt-Transh WHERE
                btt-Transh.op       EQ iOp
            AND btt-Transh.op-entry EQ iOpEntry
            AND CAN-FIND (FIRST bloan WHERE
                                bloan.contract  EQ btt-Transh.contract
                            AND bloan.cont-code EQ ENTRY (1, btt-Transh.cont-code, " ")
                            AND bloan.cont-code NE loan.cont-code
                          NO-LOCK
                         )
      :
         oResult = NO.
      END.
   IF iCheckRole AND oResult THEN
         /* проверка наличия в выборке привязок с разными ролями */
      FOR FIRST tt-Transh WHERE
                tt-Transh.op       EQ iOp
            AND tt-Transh.op-entry EQ iOpEntry
            AND CAN-FIND (FIRST btt-Transh WHERE
                                btt-Transh.op        EQ iOp
                            AND btt-Transh.op-entry  EQ iOpEntry
                            AND btt-Transh.acct-type NE tt-Transh.acct-type
                         )
      :
         oResult = NO.
      END.
END PROCEDURE.

PROCEDURE pTranshPosting:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* Код документа */
   DEF INPUT  PARAM iOpEnrty     AS INT64 NO-UNDO. /* Код проводки */

   DEF VAR vDate      AS DATE NO-UNDO.
   DEF VAR vSummOst   AS DEC  NO-UNDO. /* сумма проводки минус сумма разнесенных операций */
   DEF VAR vSummTr    AS DEC  NO-UNDO. /* сумма по траншу */
   DEF VAR vSummOb    AS DEC  NO-UNDO. /* сумма по обязательству */
   DEF VAR vSummTek   AS DEC  NO-UNDO. /* сумма остатка */
   DEF VAR vDb        AS DEC  NO-UNDO. /* для запуска RE_PARAM */
   DEF VAR vCr        AS DEC  NO-UNDO. /* для запуска RE_PARAM */
   DEF VAR vSumm      AS DEC  NO-UNDO. /* сумма проводки */

   DEF BUFFER op           FOR op.
   DEF BUFFER op-entry     FOR op-entry.
   DEF BUFFER tt-Transh    FOR tt-Transh.
   DEF BUFFER loan-cond    FOR loan-cond.
   DEF BUFFER term-obl     FOR term-obl.
   DEF BUFFER loan-int     FOR loan-int. 
   MAIN:
   DO:
      FIND FIRST op-entry WHERE
              op-entry.op       EQ iOp
          AND op-entry.op-entry EQ iOpEnrty
      NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         LEAVE MAIN.
      FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR. 
      IF NOT AVAIL op THEN LEAVE MAIN. 

      ASSIGN
         vDate    = ( IF op.contract-date  EQ ?  THEN op-entry.op-date ELSE op.contract-date)
         vSumm    = ( IF op-entry.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur)  
         vSummTek = vSumm
      .
         /* вычитаем уже разнесенную сумму */
      FOR EACH loan-int WHERE
               loan-int.op       EQ op-entry.op
           AND loan-int.op-entry EQ op-entry.op-entry
      NO-LOCK:
         vSummTek = vSummTek - loan-int.amt-rub.
      END.
      /* для всех записей по проводке */
      TRANSH:
      FOR EACH tt-Transh WHERE
               tt-Transh.op       EQ op-entry.op
           AND tt-Transh.op-entry EQ op-entry.op-entry
      BY tt-Transh.open-date:
         /* находим текущее условие */
         FIND LAST loan-cond WHERE
                   loan-cond.contract  EQ tt-Transh.contract
               AND loan-cond.cont-code EQ tt-Transh.cont-code
               AND loan-cond.since     LE vDate
         NO-LOCK NO-ERROR.
         IF NOT AVAIL loan-cond THEN
            NEXT TRANSH.
         IF loan-cond.cred-period EQ "П" THEN
            vDate = tt-Transh.end-date.
         /* находим существующую операцию */
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         NO-LOCK NO-ERROR.
         IF AVAIL loan-int THEN
            ASSIGN
               tt-Transh.amt      = loan-int.amt-rub
               tt-Transh.loan-int = YES
         .
         CASE tt-Transh.acct-type:
            WHEN "Кредит" THEN
            DO:
                  /* вычисляем сумму к погашению без учета операций 
                  ** на текущую дату */
               FIND LAST term-obl WHERE
                        term-obl.contract  EQ tt-Transh.contract
                    AND term-obl.cont-code EQ tt-Transh.cont-code
                    AND term-obl.idnt      EQ 3
                    AND term-obl.end-date  LE vDate
               NO-LOCK NO-ERROR.
               IF AVAIL term-obl THEN DO:
                  RUN summ-t.p (OUTPUT tt-Transh.amt-max,
                                tt-Transh.contract,
                                tt-Transh.cont-code,
                                RECID(term-obl),
                                vDate - 1
                                ).
               END.
               FOR EACH term-obl WHERE
                        term-obl.contract  EQ tt-Transh.contract
                    AND term-obl.cont-code EQ tt-Transh.cont-code
                    AND term-obl.idnt      EQ 3
                    AND term-obl.end-date  GT vDate
               NO-LOCK:

                  RUN summ-t.p (OUTPUT vSummOb,
                                tt-Transh.contract,
                                tt-Transh.cont-code,
                                RECID(term-obl),
                                vDate ).
                  ASSIGN
                     tt-Transh.amt-dos = tt-Transh.amt-dos + ( IF vSummTek GE vSummOb THEN vSummOb ELSE
                                                             ( IF vSummTek GT 0 THEN vSummTek ELSE 0))
                  .
                  vSummTek = vSummTek - vSummOb.
               END.
            END.
            WHEN "КредПр" THEN
            DO:
               /* Сумма к погашению просрочки */ 
               RUN RE_PARAM (7,
                             vDate,
                             tt-Transh.contract,
                             tt-Transh.cont-code,
                             OUTPUT tt-Transh.amt-max,
                             OUTPUT vDb,
                             OUTPUT vCr).
               IF tt-Transh.amt EQ 0 THEN
                  ASSIGN
                     tt-Transh.amt = MIN(vSummTek, tt-Transh.amt-max)
                  .
            END.
         END CASE.
      END. 
         /* Проверка, может быть сумма уже разнесена  */ 
      FIND FIRST tt-Transh WHERE tt-Transh.loan-int NO-LOCK NO-ERROR. 
      IF NOT AVAIL tt-Transh THEN
         /* После заполнения tt-Transh - разбиваем сумму по траншам  */ 
      FOR EACH tt-Transh NO-LOCK
      BY tt-Transh.end-date
      BY tt-Transh.open-date: 
         IF vSumm LE 0 THEN LEAVE. 
         ASSIGN
            tt-Transh.amt = ( IF vSumm > tt-Transh.amt-max THEN tt-Transh.amt-max ELSE vSumm)
            vSumm         = vSumm - tt-Transh.amt
         .
      END.
   END.
END PROCEDURE.

PROCEDURE pTranshCreateInt:
   DEF INPUT  PARAM iRole   AS CHAR NO-UNDO. /* Список ролей */
   DEF INPUT  PARAM iOper   AS CHAR NO-UNDO. /* Список операций */
   DEF OUTPUT PARAM oResult AS CHAR NO-UNDO. 

   DEF VAR vCounter  AS INT64     NO-UNDO.
   DEF VAR vKau      AS CHARACTER NO-UNDO. 
   DEF VAR vRow      AS ROWID     NO-UNDO.

   DEF BUFFER tt-Transh FOR tt-Transh.
   DEF BUFFER chowhe    FOR chowhe.
   DEF BUFFER op-entry  FOR op-entry.
   DEF BUFFER loan-int  FOR loan-int. 

   TRANSH:
   DO TRANS ON ERROR UNDO, RETRY:
      IF RETRY THEN
      DO:
         /* возврат ошибки */
         ASSIGN 
            oResult = "ERROR:" + ERROR-STATUS:GET-MESSAGE(1) + " " + {&RETURN_VALUE}
            .  
         UNDO TRANSH, LEAVE TRANSH.
      END.
      FOR EACH tt-Transh WHERE tt-Transh.loan-int ON ERROR UNDO TRANSH, RETRY TRANSH:
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE loan-int THEN
         DO:
            IF LOCKED loan-int THEN
                 RUN Fill-SysMes IN h_tmess("","","-1","Операция заблокирована другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Операция не найдена").
            RETURN.
         END.

         IF AVAIL loan-int THEN
            DELETE loan-int.
         ELSE
            UNDO TRANSH, RETRY TRANSH.
      END.
      FOR EACH tt-Transh WHERE (tt-Transh.amt     GT 0
                             OR tt-Transh.amt-dos GT 0 )
      ON ERROR UNDO TRANSH, RETRY TRANSH:
         FIND FIRST op-entry WHERE
                    op-entry.op       EQ tt-Transh.op
                AND op-entry.op-entry EQ tt-Transh.op-entry
         NO-LOCK NO-ERROR.
         IF NOT AVAIL op-entry THEN
            UNDO TRANSH, RETRY TRANSH.
         vCounter = LOOKUP(tt-Transh.acct-type, iRole).
         IF vCounter EQ 0 OR vCounter GT NUM-ENTRIES(iOper) THEN
            UNDO TRANSH, RETRY TRANSH.
         FIND FIRST chowhe WHERE chowhe.id-op EQ INT64(ENTRY(vCounter, iOper)) NO-LOCK NO-ERROR.
         IF NOT AVAIL chowhe THEN
            UNDO TRANSH, RETRY TRANSH.
         IF tt-Transh.amt GT 0 THEN
            RUN Cr_LoanIntSimple IN h_lv (tt-Transh.contract,
                                    tt-Transh.cont-code,
                                    op-entry.op-date,
                                    tt-Transh.amt,
                                    chowhe.id-d,
                                    chowhe.id-k,
                                    NO,
                                    (BUFFER op-entry:HANDLE),
                                    OUTPUT vRow).
         /* досрочное погашение */
         IF tt-Transh.amt-dos GT 0 THEN
             RUN Cr_LoanIntSimple IN h_lv (tt-Transh.contract,
                                     tt-Transh.cont-code,
                                     op-entry.op-date,
                                     tt-Transh.amt-dos,
                                     chowhe.id-d,
                                     chowhe.id-k,
                                     NO,
                                     (BUFFER op-entry:HANDLE),
                                     OUTPUT vRow).
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         NO-LOCK NO-ERROR.
         IF NOT AVAIL loan-int THEN
            UNDO TRANSH, RETRY TRANSH.
         /* Создание субаналитики на проводке */ 
         ASSIGN 
            vKau = tt-Transh.Contract + "," + 
                   tt-Transh.cont-code + "," + 
                   STRING(chowhe.id-op)       
            .
         IF    (tt-Transh.Side AND NOT {assigned Op-entry.Kau-db}) 
            OR (NOT tt-Transh.Side AND NOT {assigned Op-entry.Kau-cr}) 
            THEN DO:
            FIND CURRENT op-entry EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF NOT AVAILABLE op-entry THEN
            DO:
               IF LOCKED op-entry THEN 
                    RUN Fill-SysMes IN h_tmess("","","-1","Проводка заблокирована другим пользователем").
               ELSE RUN Fill-SysMes IN h_tmess("","","-1","Проводка не найдена").
               RETURN.
            END.

            IF NOT AVAIL op-entry THEN
               UNDO TRANSH, RETRY TRANSH.
            IF tt-Transh.Side THEN 
               ASSIGN 
                  op-entry.kau-db = vKau NO-ERROR. 
            ELSE 
               ASSIGN 
                  op-entry.kau-cr = vKau NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN
               UNDO TRANSH, RETRY TRANSH.
            RELEASE op-entry NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN
               UNDO TRANSH, RETRY TRANSH.
         END.
         /* Loan-int создан успешно  */
         ASSIGN 
            tt-Transh.IsCrLoanInt = TRUE. 
      
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
    Печать разноски траншей 
------------------------------------------------------------------------------*/
PROCEDURE pTT-TranshToFile:
   DEF INPUT  PARAM iFileName     AS CHAR    NO-UNDO. 
   DEF INPUT  PARAM iIsCrLoanInt  AS LOGICAL NO-UNDO. 
   
   DEF VAR vI    AS INT64 NO-UNDO. 
   DEF VAR vSumm AS DEC   NO-UNDO.

   OUTPUT TO VALUE(iFileName).
   PUT UNFORMATTED 
      SPACE(15) 
      "ПРОТОКОЛ РАЗНЕСЕНИЯ ПРОВОДКИ ПО ТРАНШАМ" SKIP(1)
      .
   FORM 
      vI                   COLUMN-LABEL "N п/п"
                           FORMAT ">>>>9"
      tt-Transh.Cont-code  COLUMN-LABEL "Номер договора"
                           FORMAT "x(20)"
      tt-Transh.end-date   COLUMN-LABEL "Дата погашения"
                           FORMAT "99/99/9999"
      tt-Transh.Amt-max    COLUMN-LABEL "Сумма к погашению"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      tt-Transh.Amt        COLUMN-LABEL "Сумма операции"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      tt-Transh.amt-dos    COLUMN-LABEL "Сумма доср.гашения"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      WITH FRAME list WIDTH 104 DOWN NO-BOX.
   FOR EACH tt-Transh WHERE
      ( IF iIsCrLoanInt = ? THEN TRUE ELSE tt-Transh.IsCrLoanInt = iIsCrLoanInt)
      NO-LOCK:
      ASSIGN 
         vI    = vI + 1
         vSumm = vSumm + tt-Transh.Amt + tt-Transh.amt-dos
         .
      DISPLAY 
         vI
         tt-Transh.Cont-code
         tt-Transh.end-date
         tt-Transh.Amt-max
         tt-Transh.Amt
         tt-Transh.amt-dos
         WITH FRAME list.
      DOWN WITH FRAME List. 
   END.
   DISPLAY 
      "Итого" @ tt-Transh.Cont-code 
      vSumm   @ tt-Transh.Amt
      WITH FRAME List. 
   DOWN WITH FRAME list.
   OUTPUT CLOSE.
END PROCEDURE. 

PROCEDURE pTranshGet:
   DEF OUTPUT PARAM TABLE FOR tt-Transh.
END PROCEDURE.

PROCEDURE pTranshPut:
   DEF INPUT PARAM TABLE FOR tt-Transh.
END PROCEDURE.

PROCEDURE pTranshClear:
   DEF INPUT  PARAM iOp       AS INT64 NO-UNDO. /* Код документа */
   DEF INPUT  PARAM iOpEntry  AS INT64 NO-UNDO. /* Код проводки */

   FOR EACH tt-Transh WHERE
         (  iOp                  EQ ?
        AND iOpEntry             EQ ?
         )
      OR (  iOp                  NE ?
        AND iOpEntry             EQ ?
        AND tt-Transh.op         EQ iOp
         )
      OR (  iOp                  NE ?
        AND iOpEntry             NE ?
        AND tt-Transh.op         EQ iOp
        AND tt-Transh.op-entry   EQ iOpEntry
         )
   :
      DELETE tt-Transh.
   END.
END PROCEDURE.

/* Процедура для формирования номера договора обеспечения */
/* на основе номера кредитного договора                   */

PROCEDURE GETLoanNumO :
DEFINE INPUT PARAMETER iCountract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER iCounCode  AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER oNumCD    AS CHARACTER NO-UNDO .

DEF BUFFER bloan FOR loan.
DEFINE VARIABLE vSimbol AS CHARACTER NO-UNDO .
DEFINE VARIABLE vForm  AS CHARACTER NO-UNDO .
vSimbol = "-" .
vForm   = "&1-&2-&3" .

   FIND FIRST bloan WHERE
            bloan.contract  = iCountract AND
            bloan.cont-code = iCouncode
            NO-LOCK NO-ERROR .
   IF AVAILABLE bloan THEN DO:
      IF NUM-ENTRIES(bloan.doc-ref ,vSimbol) > 3
      THEN
         oNumCD   = substitute( vForm ,
                  ENTRY(1,bloan.doc-ref,vSimbol )
                  , ENTRY(2,bloan.doc-ref,vSimbol )
                  , ENTRY(3,bloan.doc-ref,vSimbol )
                  ) .
      ELSE
         oNumCD   = ENTRY(1,bloan.doc-ref,"@") .
   END.
END PROCEDURE. /* GETLoanNumO */


/* Для формирования номера документа по шаблону . Проверяет наличие тега в шаблоне */
PROCEDURE AutoCodeNeed :
DEFINE INPUT  PARAMETER iClass AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER iTeg AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER oRes AS LOGICAL NO-UNDO .

DEFINE VARIABLE vTemplate AS CHARACTER NO-UNDO .
DEFINE VARIABLE vCounter AS CHARACTER NO-UNDO .

   oRes = FALSE .

   IF    GetXAttrInit (iClass, "AutoGenNum") EQ "ДА" THEN
   DO:
      RUN GetClassTemplatePars (iClass,
                                 OUTPUT vTemplate,
                                 OUTPUT vCounter).
      vTemplate = FGetSetting("ШаблонСчетч",vTemplate,"")  .
      IF INDEX(vTemplate,iTeg) > 0
      THEN
         oRes = TRUE .
   END.

END PROCEDURE. /* AutoCodeNeed */

/* По кредитной линии определяет срок окончания линии */
PROCEDURE EndDateKreditLine :
DEFINE INPUT  PARAMETER  iContract AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iAcct     AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iCurr     AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iDateEnd  AS DATE      NO-UNDO .
DEFINE OUTPUT PARAMETER  oDateEnd  AS DATE      NO-UNDO .

DEF BUFFER tloan     FOR loan.
DEF BUFFER loan      FOR loan.
DEF BUFFER loan-acct FOR loan-acct.
DEF BUFFER limits    FOR limits.
DEF BUFFER acct      FOR acct.

DEFINE VARIABLE vAccType  AS CHARACTER NO-UNDO .
DEFINE VARIABLE vContcode AS CHARACTER NO-UNDO .
DEFINE VARIABLE vTYpeLim  AS CHARACTER NO-UNDO .

   ASSIGN
      vAccType  = ""
      vContcode = ""
      vTypeLim  = ""
      .

   /* Охватывающий договор */
   FIND FIRST  tloan WHERE
               tloan.contract  EQ iContract
         AND   tloan.cont-code EQ ENTRY(1,iContCode," ")
         NO-LOCK NO-ERROR .
   IF NOT AVAILABLE tloan THEN RETURN ERROR "Не найден договор " + iContract + " " + ENTRY(1,iContCode," ")  .

   IF iDateEnd EQ ? THEN iDateEnd = tloan.end-date.

   FOR EACH    loan WHERE
         &IF DEFINED(oracle) &THEN
                 loan.contract  EQ iContract
            AND  loan.cont-code MATCHES(ENTRY(1,  tloan.cont-code, " ") + "*")
            AND  NUM-ENTRIES( loan.cont-code, " ") LE 2
            USE-INDEX PRIMARY       
         &ELSE
              (loan.contract  EQ iContract
         AND   loan.cont-code EQ tloan.cont-code)
         OR
              (loan.contract  EQ iContract
         AND   loan.cont-code BEGINS tloan.cont-code + " " )
         &ENDIF
         NO-LOCK ,
         FIRST   loan-acct WHERE
                 loan-acct.acct      EQ iAcct
            AND  loan-acct.currency  EQ iCurr
            AND  loan-acct.contract  EQ loan.contract
            AND  loan-acct.cont-code EQ loan.cont-code
            NO-LOCK :
            vAccType  = loan-acct.acct-type.
            vContCode = loan-acct.cont-code.
            LEAVE.
   END.

   CASE vAccType:
      WHEN "КредЛин"
         THEN DO:
            vTypeLim  = "limit-l-distr".
         END.
      WHEN "КредН"
         THEN DO:
            vTypeLim  = "limit-l-debts".
         END.
   END CASE.

   /* Лимиты ведутся по охватывающему договору */
   FOR EACH limits WHERE
            limits.file-name  EQ "loan"
        AND limits.surrogate  EQ iContract + "," + ENTRY(1,iContCode, " ")
        AND limits.Class-Code EQ vTypeLim
        AND limits.open-date  LT iDateEnd
        AND limits.quantity   EQ 0
   NO-LOCK BY limits.open-date DESCENDING:
      oDateEnd = limits.open-date.
      LEAVE.
   END.

   IF oDateEnd EQ ? THEN
      oDateEnd = DATE(GetTempXAttrValue("acct",iAcct + "," + iCurr,"dealenddate")) NO-ERROR.
   IF oDateEnd EQ ? THEN
      oDateEnd = iDateEnd.

END PROCEDURE. /* EndDateKreditLine */

PROCEDURE pGetDatePay:
DEFINE INPUT PARAMETER  iContract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iTypeDate AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iDate     AS DATE      NO-UNDO .
DEFINE INPUT PARAMETER  iSign     AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iTypeGraf AS INT64     NO-UNDO .
DEFINE OUTPUT PARAMETER oDate     AS DATE      NO-UNDO .

DEFINE VARIABLE         mFlPer    AS LOG       NO-UNDO .

   IF iTypeDate EQ "ПЕРИОД" THEN
      mFlPer = TRUE.
   ELSE
      mFlPer = FALSE.
   CASE iSign:
      WHEN "=" OR
      WHEN "EQ" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date EQ iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date EQ iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "LE" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date LE iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date LE iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "LT" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date LT iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date LT iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "GT" THEN
         FIND FIRST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date GT iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date GT iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "GE" THEN
         FIND FIRST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date GE iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date GE iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
   END CASE.

   IF AVAIL term-obl THEN
   DO:
      IF iTypeDate EQ "ПЛАН" THEN
         oDate = term-obl.end-date.
      ELSE IF iTypeDate EQ "ПЕРИОД" THEN
         oDate = term-obl.dsc-beg-date.
      ELSE
         oDate = ?.
   END.
   ELSE
      oDate = ?.

END PROCEDURE. /* pGetDatePay *//* Процедура сохранения графика */

/* процедура сохранения графика  */
PROCEDURE SetTermOblHist :
DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iSince    AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER iIdnt     AS INT64      NO-UNDO.
DEFINE INPUT  PARAMETER ichType   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iDescription   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iOlap          AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER iFlagMess      AS LOGICAL NO-UNDO .
DEFINE INPUT  PARAMETER iFlagGT        AS LOGICAL NO-UNDO . /* ДА - Удалять всю историю с этой даты  */
DEFINE OUTPUT PARAMETER oErrorStat     AS LOGICAL NO-UNDO . /* ДА - есть ошибка и график не сохранен */

   RUN SetTermOblHistEx(iContract
                      , iContCode
                      , iSince
                      , iIdnt
                      , 0
                      , ichType
                      , iDescription
                      , NO
                      , ?
                      , iOlap
                      , OUTPUT oErrorStat).

END PROCEDURE. /* SetTermOblHist */

/* процедура сохранения графика  */
PROCEDURE SetTermOblHistEx:
DEFINE INPUT  PARAMETER iContract    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iContCode    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iSince       AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER iIdnt        AS INT64     NO-UNDO.
DEFINE INPUT  PARAMETER iNGr         AS INT64     NO-UNDO.
DEFINE INPUT  PARAMETER ichType      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iDescription AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iSigned      AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iSignDate    AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER iOlap        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oErrorStat   AS LOGICAL   NO-UNDO. /* да - есть ошибка и график не сохранен */

DEFINE VARIABLE vUserId    AS CHARACTER NO-UNDO .

DEF BUFFER term-obl-hist FOR term-obl-hist.
DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.

IF FGetSetting ("ГрафИст", "СохрГрафИзм",?) NE "Да"  THEN RETURN.
IF LOOKUP ( STRING(iIdnt), FGetSetting ("ГрафИст", "ГрафТип",?)) = 0 THEN RETURN. 

   vUserId = USERID("bisquit").
   oErrorStat = TRUE  .

TR:
DO TRANSACTION 
   ON ERROR UNDO TR,LEAVE TR
   ON QUIT UNDO TR,LEAVE TR:

   /* создание шапки истории  */
   CREATE term-obl-hist .
      ASSIGN
         term-obl-hist.contract  = iContract
         term-obl-hist.cont-code = iContCode
         term-obl-hist.idnt      = iIdnt
         term-obl-hist.olap      = iOlap
         term-obl-hist.since     = iSince
         term-obl-hist.chtype    = ichtype
         term-obl-hist.description = idescription
         term-obl-hist.User-Id     = vUserId
   .

   VALIDATE term-obl-hist.
   
   /* прочистка  */
     FOR EACH tobl-hist-amt WHERE
            tobl-hist-amt.tobl-id EQ term-obl-hist.tobl-id
   EXCLUSIVE-LOCK:
      DELETE tobl-hist-amt.
     END.
   /* копирование графика  */
     FOR EACH term-obl  WHERE
            term-obl.contract  EQ iContract
        AND term-obl.cont-code EQ iContCode
        AND term-obl.idnt      EQ iIdnt
          NO-LOCK :
             CREATE tobl-hist-amt.
      BUFFER-COPY term-obl TO tobl-hist-amt.
             ASSIGN
               tobl-hist-amt.tobl-id = term-obl-hist.tobl-id
               .
      VALIDATE tobl-hist-amt.
     END.
   /* запись значений в ДР */
   UpdateSigns("term-obl-hist",STRING(term-obl-hist.tobl-id),"DateSave",STRING(NOW),YES).
   UpdateSigns("term-obl-hist",STRING(term-obl-hist.tobl-id),"NumGr"   ,STRING(iNGr),YES).

     oErrorStat = FALSE .
END.  /* TR */

END PROCEDURE. /* SetTermOblHistEx */


/* процедура удаления графика  */
PROCEDURE DelTermOblHist :
DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.   /* договор назначение */
DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.   /* договор номер */
DEFINE INPUT  PARAMETER iSince    AS DATE       NO-UNDO.   /* дата сохранения графика  */
DEFINE INPUT  PARAMETER iIdnt     AS INT64      NO-UNDO.   /* тип графика 1,3 и еще что... */
DEFINE INPUT  PARAMETER iFlagGT   AS LOGICAL    NO-UNDO.   /* да - удалять всю историю с этой даты */
/* удаление tobl-hist-amt в триггере на удаление term-obl-hist */

DEF BUFFER bterm-obl-hist FOR term-obl-hist.
DEF BUFFER bsigns FOR signs.

   IF iFlagGT THEN DO:
      FOR EACH  bterm-obl-hist WHERE
                bterm-obl-hist.contract  EQ iContract
            AND bterm-obl-hist.cont-code EQ iContcode
            AND bterm-obl-hist.idnt      EQ iIdnt
            AND bterm-obl-hist.since     GE iSince
            EXCLUSIVE-LOCK :
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(term-obl-hist.tobl-id)
            AND bsigns.code      EQ "Signed" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","Реквизит заблокирован другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Реквизит не найден").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "SignDate" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
           IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","Реквизит заблокирован другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Реквизит не найден").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         DELETE bterm-obl-hist.
      END.
   END.
   ELSE DO:
      FIND FIRST bterm-obl-hist WHERE
                bterm-obl-hist.contract  EQ iContract
            AND bterm-obl-hist.cont-code EQ iContcode
            AND bterm-obl-hist.idnt      EQ iIdnt
            AND bterm-obl-hist.since     EQ iSince
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR .

      IF NOT AVAILABLE bterm-obl-hist THEN
      DO:
        IF LOCKED bterm-obl-hist THEN 
              RUN Fill-SysMes IN h_tmess("","","-1","История заблокирована другим пользователем").
         ELSE RUN Fill-SysMes IN h_tmess("","","-1","История не найдена").
         RETURN.
      END.

      IF AVAILABLE bterm-obl-hist THEN
      DO:
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "Signed" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","Реквизит заблокирован другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Реквизит не найден").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "SignDate" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","Реквизит заблокирован другим пользователем").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","Реквизит не найден").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         DELETE bterm-obl-hist.
      END.
   END.
END PROCEDURE. /* DelTermOblHist */

/* Процедура получение ставки в зависимости от границ базового параметра  */
PROCEDURE GetFloatRate:
   DEF INPUT  PARAM iCommission AS CHAR   NO-UNDO. /* Код комиссии */
   DEF INPUT  PARAM iDate       AS DATE   NO-UNDO. /* Дата */
   DEF INPUT  PARAM iBaseAmt    AS DEC    NO-UNDO. /* величина базы для расчета по ставке */
   DEF OUTPUT PARAM oRate       AS DEC    NO-UNDO INIT ?. /* величина ставки */
   MAIN:
   DO:
      /* Сначала по коду ставки */
      GetRefCrVal("init-float",
                  "commission",
                  iDate,
                  ?,
                  (TEMP-TABLE ttIndicate:HANDLE)).
      FOR EACH ttIndicate WHERE ttIndicate.fChar EQ iCommission:
          /* далее по границе "ОТ" */
          GetRefCrVal("init-float",
                      "amt-min",
                      iDate,
                      ttIndicate.fChar,
                      (TEMP-TABLE ttIndicate1:HANDLE)).
          FOR EACH ttIndicate1 WHERE ttIndicate1.fDec LE iBaseAmt:
              /* далее по границе "ДО" */
              GetRefCrVal("init-float",
                          "amt-max",
                          iDate,
                          ttIndicate.fChar + CHR(44) + STRING(ttIndicate1.fDec),

                          (TEMP-TABLE ttIndicate2:HANDLE)).
              FOR FIRST ttIndicate2 WHERE ttIndicate2.fDec GT iBaseAmt
                                       OR ttIndicate2.fDec EQ 0: /* или нет верхней границы */
                  oRate = DECIMAL(GetRefVal("init-float",
                                            iDate,
                                            ttIndicate.fChar + "," + STRING(ttIndicate1.fDec) + "," + STRING(ttIndicate2.fDec))).
                  LEAVE MAIN.
              END.
          END.
      END.
   END.

END PROCEDURE.

/* Создание второго условия для двойной аннуитетной суммы */
PROCEDURE Cr_Cond_DblAnn.
  DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER iRecalcGraf  AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER iRecalcAnn   AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER iNewSince    AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opRcd       AS RECID     NO-UNDO.
  DEFINE OUTPUT PARAMETER opSumm      AS DECIMAL   NO-UNDO.

  DEF VAR vFirstPeriod  AS INTEGER   NO-UNDO.
  DEF VAR vPartAmount   AS DECIMAL   NO-UNDO.
  DEF VAR vKolLgtPer    AS INTEGER   NO-UNDO.  
  DEF VAR vCredOffSet   AS CHARACTER NO-UNDO.    
  DEF VAR vAnnuitKorr   AS INTEGER   NO-UNDO. 
  DEF VAR vSumDepos     AS DECIMAL   NO-UNDO.
  DEF VAR vSince        AS DATE      NO-UNDO. /* дата, на которую создаем условие */
  DEF VAR vNumPers      AS INTEGER   NO-UNDO.
  DEF VAR vAmount       AS DECIMAL   NO-UNDO.
  DEF VAR vCommRate     AS DECIMAL   NO-UNDO.
  DEF VAR vAnnuit2      AS DECIMAL   NO-UNDO.
  DEF VAR vCondCount    AS INTEGER   NO-UNDO.
  DEF VAR vWorkGraf       AS CHAR    NO-UNDO.   /* График работы */
  DEF VAR vBranch         AS CHAR    NO-UNDO.   /* Подразделение */
  DEF VAR vTmpDate        AS DATE    NO-UNDO.
  DEF VAR vPrimPartAmount AS DECIMAL NO-UNDO.
  DEF VAR vDateCond       AS DATE    NO-UNDO.
  
  DEF BUFFER loan        FOR loan.
  DEF BUFFER loan-cond   FOR loan-cond.
  DEF BUFFER bloan-cond  FOR loan-cond.
  DEF BUFFER bbloan-cond FOR loan-cond.
  DEF BUFFER term-obl    FOR term-obl.

  opRcd = ?.

  FIND FIRST loan WHERE loan.contract  EQ iContract
                    AND loan.cont-code EQ iContCode
          NO-LOCK NO-ERROR.
  IF NOT AVAIL loan THEN RETURN "ошибка поиска договора".

  FIND LAST bloan-cond WHERE bloan-cond.contract  EQ iContract
                         AND bloan-cond.cont-code EQ iContCode
                         AND bloan-cond.since     LE iSince
      NO-LOCK NO-ERROR.
  IF NOT AVAIL bloan-cond THEN RETURN ERROR "Ошибка поиска условия" .

  ASSIGN
    vKolLgtPer  = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "КолЛьгтПер",
                                      "0"))
    vCredOffSet = GetXattrValueEx("loan-cond",
                                   bloan-cond.contract + "," 
                                   + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                   "cred-offset",
                                   "--")
    vAnnuitKorr = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "АннуитКорр",
                                      "0"))
    vSumDepos   = DEC(GetXattrValueEx("loan",
                                      loan.contract + "," + loan.cont-code,
                                      "sum-depos",
                                      "0"))
    vAnnuit2   = DEC(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "AnnuitPlat2",
                                      "0"))
           
      .

  RUN GetFirstPerPartAmt(bloan-cond.contract, 
                         bloan-cond.cont-code,
                         bloan-cond.since,
                         OUTPUT vFirstPeriod,
                         OUTPUT vPartAmount).
  
  IF vPartAmount = 0 OR vFirstPeriod = 0 THEN RETURN ERROR "Ошибка задания реквизитов".

  /* Ищем последнее условие, на котором задана вторая аннуитетная сумма */
  IF iRecalcAnn <> YES THEN DO:
      RUN GetAnnuitPlat2(iContract,iContCode,bloan-cond.since,OUTPUT vAnnuit2).
      /* если не удалось определить сумму, то придется ее пересчитать */
      IF vAnnuit2 = 0 THEN iRecalcAnn = YES.
  END.
  
  IF iNewSince = ? THEN DO:  
      ASSIGN
         vBranch   = GetBranchForLoan((BUFFER loan:HANDLE))
         vWorkGraf = GetWorkGraf(iContract + "," + iContCode, 
                                 loan.Class-Code)
         vTmpDate  = loan.open-date
         vNumPers  = - 1
         .
    
      dt:
      DO WHILE vTmpDate < loan.end-date:
          
          vNumPers = vNumPers + 1.
    
          IF vNumPers = vFirstPeriod THEN DO:
             vSince = vTmpDate.
              LEAVE dt.
          END.
    
          /* Определение даты платежа с учетом периодичности. */
          vTmpDate = RE_MOVE_DATE (vTmpDate,
                                   loan.end-date + 366,
                                   INT(bloan-cond.cred-date),
                                   STRING(bloan-cond.cred-period) + ":" + STRING(bloan-cond.cred-month),
                                   LOOKUP(vCredOffset,"--,->,<-"),
                                   loan.open-date,
                                   vBranch, 
                                   vWorkGraf).
          
      END. /* DO WHILE */
  END.
  ELSE vSince = iNewSince.

  IF vSince = ? THEN RETURN "Не определена дата начала периода увеличения".

  /* если есть текущий плановый остаток, то это и есть 
  ** сумма всего кредита минус выплаченная в первом периоде */
  FIND FIRST term-obl WHERE term-obl.contract  EQ iContract        
                        AND term-obl.cont-code EQ iContCode        
                        AND term-obl.end-date  EQ vSince 
                        AND term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
  IF AVAIL term-obl THEN
      vAmount = term-obl.amt-rub.
  /* если плановых остатков почему-то нет, то определяем сколько выдали 
  * за период уменьшения и сколько из этой суммы должны были погасить по первоначальному условию */
  ELSE DO:
     RUN GetCurAmt(iContract,
                   iContCode,
                   vSince,
                   OUTPUT vAmount).
     FIND FIRST bbloan-cond WHERE  bbloan-cond.contract  EQ iContract
                               AND bbloan-cond.cont-code EQ iContCode
                               AND bbloan-cond.since     EQ loan.open-date
      NO-LOCK NO-ERROR.
     IF NOT AVAIL bbloan-cond THEN  RETURN ERROR  "Не найдено первое условие договора".

     vPrimPartAmount = DEC(GetXattrValueEx("loan-cond",
                                           bbloan-cond.contract + "," 
                                           + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                           "PartAmount",
                                           "0")).

     FIND FIRST term-obl WHERE term-obl.contract  EQ iContract        
                           AND term-obl.cont-code EQ iContCode        
                           AND term-obl.end-date  EQ loan.open-date 
                           AND term-obl.idnt      EQ 2
        NO-LOCK NO-ERROR.
     IF NOT AVAIL term-obl THEN  RETURN ERROR  "Не определена первоначальная сумма договора".
     
     vAmount = vAmount - ((term-obl.amt-rub * vPrimPartAmount) / 100).
  END.

  opRcd = ?.

  TR:
  DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
    ON QUIT UNDO TR,LEAVE TR:

          RUN CrCond(iContract, 
                     iContCode, 
                     vSince,
                     "%Рез,%ОРез")
          NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
               UNDO TR,LEAVE TR.
          END.

          FIND FIRST loan-cond WHERE
                     loan-cond.contract  EQ iContract
                 AND loan-cond.cont-code EQ iContCode
                 AND loan-cond.since     EQ vSince
             NO-LOCK NO-ERROR.
          IF NOT AVAIL loan-cond THEN UNDO TR,LEAVE TR.

          UpdateSignsEx(loan-cond.class-code,
                        loan-cond.Contract + "," + loan-cond.Cont-code + "," + STRING(loan-cond.since),
                        "AutoCond",
                        "Да").
          UpdateSignsEx(loan-cond.class-code,
                        loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                        "КолЛьгтПер",
                        "0").
          IF iRecalcAnn THEN DO:
          
              vCommRate = GET_COMM_LOAN(loan.contract,
                                        loan.cont-code,
                                        "%Кред",
                                        vSince).
    
              RUN CalcAnnuitet2(loan.contract,
                                loan.cont-code,
                                vSince,
                                loan.end-date,
                                vAmount,
                                vCommRate,
                                loan-cond.cred-date,
                                loan-cond.cred-period,
                                loan-cond.cred-month,
                                vKolLgtPer,
                                LOOKUP(vCredOffset,"--,->,<-"),
                                vAnnuitKorr,
                                vSumDepos,
                                vFirstPeriod,
                                vPartAmount,
                                2,
                                OUTPUT vAnnuit2).              
           END.
           
           UpdateSigns(loan-cond.class-code,
                       loan-cond.Contract + "," + loan-cond.Cont-code + "," + STRING(loan-cond.since),
                       "АннуитПлат",
                       STRING(vAnnuit2),
                       ?).       
           /* Сохраняем на текущем условии сумму аннуитета периода увеличения */
           UpdateSigns(bloan-cond.class-code,
                       bloan-cond.Contract + "," + bloan-cond.Cont-code + "," + STRING(bloan-cond.since),
                       "AnnuitPlat2",
                       STRING(vAnnuit2),
                       ?).
            
           opRcd = RECID(loan-cond).
  END.  /* End of TR BLOCK */

  IF opRcd = ? THEN
     RETURN ERROR "Ошибка создания автоматического условия договора № " + Loan.cont-code.

  FIND FIRST term-obl WHERE term-obl.contract  = iContract
                        AND term-obl.cont-code = iContCode
                        AND term-obl.idnt = 2
                        AND term-obl.end-date = vSince
      NO-LOCK NO-ERROR.
  IF AVAIL term-obl THEN opSumm = term-obl.amt-rub.
  ELSE RETURN ERROR "Ошибка определения суммы при создании автоматического условия договора № " + Loan.cont-code.    

  IF iRecalcGraf THEN DO:
      FOR EACH loan-cond WHERE
               loan-cond.contract  = loan.contract
           AND loan-cond.cont-code = loan.cont-code
      NO-LOCK:
         vCondCount = vCondCount + 1.
      END.
      
      /* пересчет графиков */
      RUN SetSysConf IN h_Base("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН","ДА").              
      RUN mm-to.p(RECID(loan),
                  opRcd,
                   opSumm,
                   1,
                   YES,
                   YES,
                   YES,
                   YES,
                   ?,
                   vCondCount) NO-ERROR.
      RUN DeleteOldDataProtocol IN h_Base("НЕ ВЫВОДИТЬ ГРАФИКИ НА ЭКРАН").
      IF ERROR-STATUS:ERROR THEN RETURN ERROR {&RETURN_VALUE}.
   END.
END PROCEDURE.

/* Определение даты второго условия для двойной аннуитетной суммы */
PROCEDURE GetDateDblAnn.
  DEFINE INPUT  PARAMETER iContract    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iContCode    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iSince       AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oSince       AS DATE      NO-UNDO.

  DEF VAR vFirstPeriod  AS INTEGER   NO-UNDO.
  DEF VAR vPartAmount   AS DECIMAL   NO-UNDO.
  DEF VAR vKolLgtPer    AS INTEGER   NO-UNDO.  
  DEF VAR vCredOffSet   AS CHARACTER NO-UNDO.    
  DEF VAR vSince        AS DATE      NO-UNDO. /* дата, на которую создаем условие */
  DEF VAR vNumPers      AS INTEGER   NO-UNDO.
  DEF VAR vWorkGraf       AS CHAR    NO-UNDO.   /* График работы */
  DEF VAR vBranch         AS CHAR    NO-UNDO.   /* Подразделение */
  DEF VAR vTmpDate        AS DATE    NO-UNDO.

  DEF BUFFER loan        FOR loan.
  DEF BUFFER loan-cond   FOR loan-cond.
  DEF BUFFER bloan-cond  FOR loan-cond.
  DEF BUFFER bbloan-cond FOR loan-cond.

  FIND FIRST loan WHERE loan.contract  EQ iContract
                    AND loan.cont-code EQ iContCode
          NO-LOCK NO-ERROR.
  IF NOT AVAIL loan THEN RETURN "ошибка поиска договора".

  FIND LAST bloan-cond WHERE bloan-cond.contract  EQ iContract
                         AND bloan-cond.cont-code EQ iContCode
                         AND bloan-cond.since     LE iSince
      NO-LOCK NO-ERROR.
  IF NOT AVAIL bloan-cond THEN RETURN ERROR "Ошибка поиска условия" .

  ASSIGN
    vKolLgtPer  = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "КолЛьгтПер",
                                      "0"))
    vCredOffSet = GetXattrValueEx("loan-cond",
                                   bloan-cond.contract + "," 
                                   + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                   "cred-offset",
                                   "--")        
      .
 
  RUN GetFirstPerPartAmt(bloan-cond.contract, 
                         bloan-cond.cont-code,
                         bloan-cond.since,
                         OUTPUT vFirstPeriod,
                         OUTPUT vPartAmount).

  IF vFirstPeriod = 0 THEN RETURN ERROR "Ошибка задания реквизита FirstPeriod".

  ASSIGN
     vBranch   = GetBranchForLoan((BUFFER loan:HANDLE))
     vWorkGraf = GetWorkGraf(iContract + "," + iContCode, 
                             loan.Class-Code)
     vTmpDate  = loan.open-date
     vNumPers  = - 1
     .

  dt:
  DO WHILE vTmpDate < loan.end-date:
      
      vNumPers = vNumPers + 1.

      IF vNumPers = vFirstPeriod THEN DO:
         vSince = vTmpDate.
          LEAVE dt.
      END.

      /* Определение даты платежа с учетом периодичности. */
      vTmpDate = RE_MOVE_DATE (vTmpDate,
                               loan.end-date + 366,
                               INT(bloan-cond.cred-date),
                               STRING(bloan-cond.cred-period) + ":" + STRING(bloan-cond.cred-month),
                               LOOKUP(vCredOffset,"--,->,<-"),
                               loan.open-date,
                               vBranch, 
                               vWorkGraf).
      IF vTmpDate = ? THEN LEAVE dt.
      
  END. /* DO WHILE */

  oSince = vSince.

END PROCEDURE.

/* Определение доли и продолжительности периода уменьшения */
PROCEDURE GetFirstPerPartAmt.
    DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.

    DEFINE OUTPUT PARAMETER  oFirstPeriod     AS INT   NO-UNDO.
    DEFINE OUTPUT PARAMETER  oPartAmount      AS DEC   NO-UNDO.

    DEF BUFFER bbloan-cond FOR loan-cond.

    DEF VAR vDateCond       AS DATE NO-UNDO.

    ASSIGN
        oPartAmount  = DEC(GetXattrValueEx("loan-cond",
                                           iContract + "," + iContCode + "," + STRING(iSince),
                                           "PartAmount",
                                           "0"))
        oFirstPeriod = INT(GetXattrValueEx("loan-cond",
                                           iContract + "," + iContCode + "," + STRING(iSince),
                                           "FirstPeriod",
                                           "0"))
          .
    /* Ищем последнее условие, на котором заданы параметры 
    ** для расчета второй аннуитетной суммы */
    vDateCond = iSince.
    fcnd:
    DO WHILE  oPartAmount = 0 
         AND oFirstPeriod = 0:
       FIND LAST bbloan-cond WHERE bbloan-cond.contract  EQ iContract
                               AND bbloan-cond.cont-code EQ iContCode
                               AND bbloan-cond.since     LT vDateCond
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bbloan-cond THEN LEAVE fcnd.
      ASSIGN
          vDateCond    = bbloan-cond.since
          oPartAmount  = DEC(GetXattrValueEx("loan-cond",
                                            bbloan-cond.contract + "," 
                                            + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                            "PartAmount",
                                            "0"))
          oFirstPeriod = INT(GetXattrValueEx("loan-cond",
                                             bbloan-cond.contract + "," 
                                             + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                             "FirstPeriod",
                                             "0"))
          .
    END.
      
END PROCEDURE.

/* Определение сохраненной суммы аннуитетного платежа периода увеличения */
PROCEDURE GetAnnuitPlat2:
    DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.
   
    DEFINE OUTPUT PARAMETER  oAnnuit2      AS DEC   NO-UNDO.
   
    DEF BUFFER bbloan-cond FOR loan-cond.
   
    DEF VAR vDateCond AS DATE NO-UNDO.
   
    ASSIGN
       vDateCond = iSince
       oAnnuit2 = 0.

    fcnd1:
    DO WHILE oAnnuit2 = 0:
        FIND LAST bbloan-cond WHERE bbloan-cond.contract  EQ iContract
                                AND bbloan-cond.cont-code EQ iContCode
                                AND bbloan-cond.since     LT vDateCond
       NO-LOCK NO-ERROR.
       IF NOT AVAIL bbloan-cond THEN LEAVE fcnd1.
       ASSIGN
           vDateCond  = bbloan-cond.since
           oAnnuit2   = DEC(GetXattrValueEx("loan-cond",
                                             bbloan-cond.contract + "," 
                                             + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                             "AnnuitPlat2",
                                             "0")).
    END.

END PROCEDURE.

/* Изменение даты открытия кредитного договора */
PROCEDURE ChangeLoanDate.
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* Идентификатор     */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* договора          */
   DEF INPUT  PARAM iNewDate     AS DATE NO-UNDO. /* Новая дата        */
   DEF INPUT  PARAM iChange      AS CHAR NO-UNDO. /* Да - Менять дату начала и строить графики
                                                      Нет - Только построение графиков */
   DEF INPUT  PARAM iChEndDate   AS CHAR NO-UNDO. /* НЕ сдвигать дату окончания */
   DEF INPUT  PARAM iChkSrok     AS CHAR NO-UNDO. /* Проверять срочночность ссудного счета */
   DEF INPUT  PARAM iChLacct     AS CHAR NO-UNDO. /* Сдвигать дату привязки счетов */
   DEF OUTPUT PARAM oOk          AS LOG  NO-UNDO. /* Флаг успешности   */

   DEF BUFFER loan       FOR loan.      /* Локализация буффера */
   DEF BUFFER loan-cond  FOR loan-cond. /* Локализация буффера */
   DEF BUFFER signs      FOR signs.     /* Локализация буффера */
   DEF BUFFER bsigns     FOR signs.     /* Локализация буффера */
   DEF BUFFER bloan-cond FOR loan-cond. /* Локализация буффера */ 
   DEF BUFFER ins-loan   FOR loan.      /* Локализация буффера */
   DEF BUFFER vcomm-rate FOR comm-rate. /* Локализация буффера */ 
   DEF BUFFER loan-int   FOR loan-int.  /* Локализация буффера */
   DEF BUFFER bloan      FOR loan.      /* Локализация буффера */
   DEF BUFFER loan-acct  FOR loan-acct. /* Локализация буффера */
   DEF BUFFER acct       FOR acct.      /* Локализация буффера */
   DEF BUFFER bcomm-rate FOR comm-rate. /* Локализация буффера */
   DEF BUFFER bterm-obl  FOR term-obl . /* Локализация буффера */

   DEF VAR vNDays       AS INT   NO-UNDO. /* Количество дней действия дог-ра */
   DEF VAR vNMonthes    AS INT   NO-UNDO. /* Количество мусяцев действия дог-ра */
   DEF VAR vNYears      AS INT   NO-UNDO. /* Количество лет действия дог-ра */
   DEF VAR vNEnd-date   AS DATE  NO-UNDO. /* Дата, сдвинутая в соотв. с раб/нераб днями */
   DEF VAR vMove        AS INT   NO-UNDO. /* Направление сдвига даты */
   DEF VAR vCounter     AS INT   NO-UNDO. /* Счетчик для удаления всех графиков */
   DEF VAR vLrecid      AS RECID NO-UNDO. /* RECID loan */
   DEF VAR vLCrecid     AS RECID NO-UNDO. /* RECID loan-cond */
   DEF VAR vLCSurr      AS CHAR  NO-UNDO. /* Суррогат loan-cond */
   DEF VAR vTOSurr      AS CHAR  NO-UNDO. /* Суррогат term-obl */
   DEF VAR vTOSurrNew   AS CHAR  NO-UNDO. /* Суррогат нового term-obl */
   DEF VAR vCredSumm    AS DEC   NO-UNDO. /* Сумма кредитного договора */
   DEF VAR vCredRisk    AS DEC   NO-UNDO. /* Коэф.риска кредитного договора */
   DEF VAR vCredOffs    AS CHAR  NO-UNDO. /* Значение доп.рекв cred-offset */
   DEF VAR vIntOffs     AS CHAR  NO-UNDO. /* Значение доп.реквint-offset */
   DEF VAR vOldDate     AS DATE  NO-UNDO. /* Старая дата открытия договора и условия */
   DEF VAR vOCRSurr     AS CHAR  NO-UNDO. /* Суррогат старого comm-rate'a */
   DEF VAR vNCRSurr     AS CHAR  NO-UNDO. /* Суррогат нового comm-rate'a */
   DEF VAR vQualGar     AS LOG   NO-UNDO. /* Успешность установки категории качества */
   DEF VAR vSurrIns     AS CHAR  NO-UNDO. /* Суррогат договора страхования*/
   DEF VAR vAnnSumm     AS DEC   NO-UNDO. /* Новая сумма аннуитета */
   DEF VAR vRateCred    AS DEC   NO-UNDO. /* Ставка %Кред */
   DEF VAR vCredDate    AS INT   NO-UNDO. /* cred-date с условия */
   DEF VAR vCredPer     AS CHAR  NO-UNDO. /* cred-period с условия */
   DEF VAR vCredMon     AS CHAR  NO-UNDO. /* cred-month с условия */
   DEF VAR vKolLgtPer   AS INT   NO-UNDO. /* КолЛьгтПер с уловия */
   DEF VAR vCondClass   AS CHAR  NO-UNDO. /* Класс с уловия */
   DEF VAR vBal2Acct    AS CHAR  NO-UNDO. /* Балансовый счет 2-го порядка */
   DEF VAR vListType    AS CHAR  NO-UNDO. /* ДР list_type */
   DEF VAR vAnnuitCorr  AS INT   NO-UNDO. /* ДР АннуитКорр */
   DEF VAR vSummDepos   AS DEC   NO-UNDO. /* ДР sum-depos */
   DEF VAR vFirstPeriod AS INT   NO-UNDO. /* ДР FirstPeriod */
   DEF VAR vPartAmount  AS DEC   NO-UNDO. /* ДР PartAmount */
   DEF VAR vComm        AS CHAR  NO-UNDO.

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* Блокируем договор */
      {profind.i &fnoer=TRUE &fnowh=TRUE
          &fway=FIRST &fbuf=loan &flock=EXCLUSIVE-LOCK &ffor=TRUE
          &fcond="WHERE loan.contract  EQ iContract ~
                    AND loan.cont-code EQ iContCode"}
      IF NOT AVAILABLE loan THEN DO:
         IF LOCKED loan THEN 
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("Договор &1 заблокирован другим пользователем",
                                                  iContCode)
                                      ).
         ELSE                             
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("Договор не найден &1", iContCode)
                                      ).
         LEAVE mb .
      END.   

      /* Блокируем условие договора */
      {profind.i &fnoer=TRUE &fnowh=TRUE
          &fway=FIRST &fbuf=loan-cond &flock=EXCLUSIVE-LOCK &ffor=TRUE
          &fcond="WHERE loan-cond.contract  EQ iContract ~
                    AND loan-cond.cont-code EQ iContCode"}
      IF NOT AVAILABLE loan-cond THEN DO:
         IF LOCKED loan-cond THEN 
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("Условие договора &1 заблокирован другим пользователем",
                                                  iContCode)
                                      ).
         ELSE
            RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("Условие договора &1 не найдено", iContCode)
                                   ).
         LEAVE mb .
      END.   

      ASSIGN
         /* поиск договора страхования по кредитному договору  */
         vSurrIns = ENTRY(1,
                          GetLinks("Link-Insur",               /* ID класса                            */
                                   iContract + "," + iContCode,/* ID(cуррогат) объекта                 */
                                   ?,                          /* Направление связи: s | t | ?         */
                                   "*",                        /* Список кодов линков в CAN-DO формате */
                                   CHR(1),                     /* Разделитель результирующего списка   */
                                   ?                           /* Дата, на которую осуществляется поиск связей */
                           ),
                           CHR(1)
                           )
      .
      FIND FIRST ins-loan WHERE ins-loan.contract  EQ ENTRY(1,vSurrIns)
                            AND ins-loan.cont-code EQ ENTRY(2,vSurrIns)
      NO-LOCK NO-ERROR.
      
      /* Инициализируем необходимые переменные */
      ASSIGN
         vLrecid          = RECID(loan)
         vLCrecid         = RECID(loan-cond)
         vCredRisk        = loan.risk
         vCredOffs        = GetXAttrValueEx("loan-cond",
                                            loan-cond.contract + "," 
                                          + loan-cond.cont-code + "," 
                                          + STRING(loan-cond.since),
                                            "cred-offset",
                                            "--")
         vIntOffs         = GetXAttrValueEx("loan-cond",
                                            loan-cond.contract + "," 
                                          + loan-cond.cont-code + "," 
                                          + STRING(loan-cond.since),
                                            "int-offset",
                                            "--")
      .

      /* Найдем плановый остаток и сохраним его сумму, для расчета графиков с нуля */
      FIND FIRST term-obl WHERE term-obl.contract  EQ iContract
                            AND term-obl.cont-code EQ iContCode
                            AND term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
      vCredSumm = term-obl.amt-rub.

      /* Если необходима смена даты открытия договора и условия */
      IF iChange EQ "Да" THEN
      CHANGE:
      DO:
         IF    CAN-FIND(FIRST loan-int WHERE 
                              loan-int.contract  EQ loan.contract
                          AND loan-int.cont-code EQ loan.cont-code
                        NO-LOCK)
            OR CAN-FIND(FIRST bloan WHERE 
                              bloan.contract  EQ     loan.contract
                          AND bloan.cont-code BEGINS loan.cont-code + " "
                          AND NUM-ENTRIES (bloan.cont-code, " ") GT 1
                        NO-LOCK) THEN
         DO:
            RUN Fill-SysMes("",
                            "",
                            0,
                            "Для договора существуют операции, или течения, "
                          + "~nизменение даты невозможно."
                            ).
            LEAVE CHANGE.
         END.
         IF iChkSrok EQ "ДА" THEN
         DO:
            FIND LAST loan-acct WHERE 
                      loan-acct.contract  EQ loan.contract
                  AND loan-acct.cont-code EQ loan.cont-code
                  AND loan-acct.acct-type EQ "Кредит"
            NO-LOCK NO-ERROR.
            IF AVAIL loan-acct THEN
            DO:
               FIND FIRST acct WHERE 
                          acct.acct     EQ loan-acct.acct
                      AND acct.currency EQ loan-acct.currency
               NO-LOCK NO-ERROR.
               IF AVAIL acct THEN
               DO:
                  RUN GetBalAcctFromLoan (loan.contract,
                                          loan.cont-code,
                                          "Кредит"
                                          ).
                  vBal2Acct = GetSysConf("БАЛАНСОВЫЙ СЧЕТ").
                  IF vBal2Acct NE STRING(acct.bal-acct) THEN
                  DO:
                     pick-value = ?.
                     
                     RUN Fill-SysMes("",
                                     "",
                                     4,
                                     "Существующий счет "
                                   + STRING(acct.bal-acct)
                                   + " не подходит по сроку к данному кредиту.~nДолжен быть счет "
                                   + vBal2Acct
                                   + ".~nПродолжить смену даты ?~n(смена счета должна быть выполнена вручную)"
                                     ).
                                     
                     IF pick-value EQ "NO" THEN
                        LEAVE CHANGE.
                  END. /* IF vBal2Acct NE acct.acct-bal THEN */
               END. /* IF AVAIL acct THEN */
            END. /* IF AVAIL loan-acct THEN */
         END. /* IF iChkSrok THEN */


         /* Если вызвана в эту же дату, в которую и была заведена заявка
         ** на кредит, то не создаем новое условие.
         ** !!!!! ПОТОМ эту проверку вынести выше, т.к. не все из этого блока тогда имеет смысл !!!! */
         IF iNewDate NE loan-cond.since THEN
         DO:
            CREATE bloan-cond.
            BUFFER-COPY loan-cond EXCEPT since TO bloan-cond.
            ASSIGN
               bloan-cond.since = iNewDate
               vLCrecid         = RECID(bloan-cond)
            .
            vLCSurr = GetSurrogateBuffer("loan-cond",(BUFFER bloan-cond:HANDLE)).
         END.
         ELSE
            vLCSurr = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).

         /* Изменяем дату открытия договора и дату пересчета договора,
         ** а также дату начала условия */
         ASSIGN
            vOldDate            = loan.open-date
            loan.open-date      = iNewDate
            loan.since          = iNewDate
            ins-loan.open-date  = iNewDate WHEN AVAIL ins-loan
            ins-loan.since      = iNewDate WHEN AVAIL ins-loan
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.

         /* Обновляем доп.реквизит ДатаСогл на договоре */
         UpdateSigns(loan.class-code,
                     iContract + "," + iContCode,
                     "ДатаСогл",
                     STRING(iNewDate),
                     ?).
   

         /* Теперь изменяем дату окончания договора, проверив на вых. день */
         ASSIGN
            vCondClass   = IF iNewDate NE loan-cond.since THEN bloan-cond.class-code
                                                          ELSE loan-cond.class-code
            vCredDate    = IF iNewDate NE loan-cond.since THEN bloan-cond.cred-date
                                                          ELSE loan-cond.cred-date
            vCredPer     = IF iNewDate NE loan-cond.since THEN bloan-cond.cred-period
                                                          ELSE loan-cond.cred-period
            vCredMon     = IF iNewDate NE loan-cond.since THEN STRING(bloan-cond.cred-month)
                                                          ELSE STRING(loan-cond.cred-month)
            vKolLgtPer   = INT(GetXAttrValue("loan-cond",vLCSurr,"КолЛьгтПер"))
            vAnnuitCorr  = INT(GetXAttrValueEx("loan-cond",vLCSurr,"АннуитКорр",?))
            vSummDepos   = DEC(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"Sum-depos","0"))
            vFirstPeriod = INT(GetXattrValueEx("loan-cond",vLCSurr,"FirstPeriod","0"))
            vPartAmount  = DEC(GetXattrValueEx("loan-cond",vLCSurr,"PartAmount","0"))
         .
         
         IF iChEndDate NE "ДА" THEN
         DO:
            ASSIGN
               vNDays     = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NDays","0"))
               vNMonthes  = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NMonthes","0"))
               vNYears    = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NYears","0"))
               vNEnd-date = GoMonth(iNewDate,vNYears * 12 + vNMonthes) + vNDays
            .
            /* Сравниваем получившуюся дату с датой первого рабочего дня
            ** если они отличаются - значит получилась дата выходного и надо двигать */
            IF vNEnd-date NE GetFirstDayForLoan((BUFFER loan:HANDLE),
                                                vNEnd-date,
                                               -1) THEN
            DO:
               RUN Fill-SysMes in h_tmess("","",3,"Дата окончания договора попала на выходной день" + "~n" + "Сдвинуть на ближайший рабочий ?|Сдвигать вперед,Сдвигать назад,Не Сдвигать").
               /* Т.к. для работы функции GetFirstDayForLoan надо передавать 
               ** количество дней в каком диапазоне искать рабочий день
               ** что бы не зашиваться на конкретные цифры - поиск первого рабочего 
               ** дня ограничим длительностью кредита */
               CASE pick-value:
                  WHEN "2" THEN
                     vMove = loan.open-date - loan.end-date.
                  WHEN "1" THEN
                     vMove = - (loan.open-date - loan.end-date).
                  OTHERWISE
                     vMove = 0.
               END CASE.
               vNEnd-date = GetFirstDayForLoan((BUFFER loan:HANDLE),vNEnd-date,vMove).
            END.

            ASSIGN
               loan.end-date     = vNEnd-date
               ins-loan.end-date = vNEnd-date WHEN AVAIL ins-loan
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.



         /* Теперь, после сдвига, снова перевычислим срок дней/месяцев/лет
         ** т.к. срок мог измениться, а также если дата начала остается той же
         ** то тоже надо перевычилслить срок, т.к. это может быть Изменение 
         ** условий, или если не изменяли дату окончания договора */
         IF   vMove      NE 0 
           OR iNewDate   EQ vOldDate
           OR iChEndDate EQ "ДА" THEN
         DO:
            RUN DMY_In_Per(loan.open-date, 
                           loan.end-date,
                           OUTPUT vNDays,
                           OUTPUT vNMonthes,
                           OUTPUT vNYears).
                     
         END.
         IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ФиксМес",GetXAttrInit(loan.class-code,"ФиксМес")) EQ "Да" THEN
         DO:
            UpdateSigns(loan-cond.class-code,vLCSurr,"NDays",STRING(vNDays),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NMonthes",STRING(vNMonthes + vNYears * 12),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NYears","0",?).
         END.
         ELSE
         DO:
            UpdateSigns(loan-cond.class-code,vLCSurr,"NDays",STRING(vNDays),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NMonthes",STRING(vNMonthes),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NYears",STRING(vNYears),?).
         END.
         UpdateSigns(loan-cond.class-code,vLCSurr,"CondEndDate",STRING(vNEnd-date),?).
         
         /* Обновляем суррогаты доп.реков т.к. в суррогат входит loan-cond.since */
         IF iNewDate NE vOldDate THEN
         DO:
            FOR EACH bsigns WHERE bsigns.file-name EQ "loan-cond"
                              AND bsigns.surrogate EQ loan.contract + "," + loan.cont-code + "," + STRING(vOldDate)
            NO-LOCK:
               {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=signs &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(signs) EQ RECID(bsigns) "}
               IF NOT AVAILABLE signs THEN DO:
                  UNDO mb, LEAVE mb.
               END.   
            
               IF CAN-DO("NDays,NMonthes,NYears,CondEndDate",signs.code) THEN
               DO:
                  DELETE signs.
                  NEXT.
               END.
               ASSIGN
                   signs.surrogate = loan.contract + "," + loan.cont-code + "," + STRING(iNewDate)
               NO-ERROR.

               IF ERROR-STATUS:ERROR THEN
                  UNDO mb, LEAVE mb.
            END.
         END.
         
         /* Меняем дату у ставок */
         FIND FIRST bcomm-rate WHERE
                    bcomm-rate.kau   EQ loan-cond.contract + "," + loan-cond.cont-code
         NO-LOCK NO-ERROR.
         DO WHILE AVAIL bcomm-rate:
            vComm = bcomm-rate.commission.
            {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=comm-rate &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(comm-rate) EQ RECID(bcomm-rate) "}
            IF NOT AVAILABLE comm-rate THEN DO:
               IF LOCKED comm-rate THEN 
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("Ставка &1 заблокирована другим пользователем",
                                               comm-rate.kau )
                                   ).
               ELSE
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("Ставка &1 не найдена", comm-rate.kau )
                                   ).
               UNDO mb, LEAVE mb.
            END. 
            comm-rate.since = iNewDate .
            
            RELEASE comm-rate .   
            RELEASE bcomm-rate .   
            FIND FIRST bcomm-rate WHERE
                       bcomm-rate.kau        EQ loan-cond.contract + "," + loan-cond.cont-code
                   AND bcomm-rate.commission GT vComm
                   AND bcomm-rate.since      EQ vOldDate
            NO-LOCK NO-ERROR.
         END.  
                                       
         /* Теперь изменяем даты по созданным обеспечениям, если они были */
         FOR EACH bterm-obl WHERE bterm-obl.contract  EQ loan.contract
                              AND bterm-obl.cont-code EQ loan.cont-code
                              AND bterm-obl.idnt      EQ 5
         NO-LOCK:

            /* Блокируем обеспечение */
            {profind.i &fnoer=TRUE &fnowh=TRUE
               &fway=FIRST &fbuf=term-obl &flock=EXCLUSIVE-LOCK &ffor=TRUE
               &fcond="WHERE RECID(term-obl) EQ RECID(bterm-obl) "}
            IF NOT AVAILABLE term-obl THEN DO:
               IF LOCKED term-obl THEN 
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    "Обеспечение заблокировано другим пользователем"                                               
                                   ).
               ELSE
                  RUN Fill-SysMes IN h_tmess("","","-1", "Обеспечение не найдено" ).
               UNDO mb, LEAVE mb.
            END.   
          
            ASSIGN
               term-obl.end-date = vNEnd-date
               term-obl.fop-date = iNewDate
            .

            
            /* ДР и катогория качества term-obl изменяются по тригеру term-obl */
         END.
         
         /* После смены даты, удаляем старое условие */
         IF iNewDate NE loan-cond.since THEN
         DO:
            RELEASE bloan-cond NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
            DELETE loan-cond NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.

         /* Теперь надо пересчитать сумму аннуитетного платежа, если схему аннуитетная */
         IF GetXAttrValue("loan-cond",vLCSurr,"СхемаПлат") EQ "Аннуитетная" THEN
         DO:
            FIND FIRST vcomm-rate WHERE vcomm-rate.kau        EQ loan.contract + "," + loan.cont-code
                                    AND vcomm-rate.commission EQ "%Кред"
                                    AND vcomm-rate.since      EQ iNewDate
            NO-LOCK NO-ERROR.
            
            IF AVAIL vcomm-rate THEN
               vRateCred = vcomm-rate.rate-comm.

            RUN CalcAnnuitet (loan.contract,
                              loan.cont-code,
                              loan.open-date,
                              loan.end-date,
                              vCredSumm,
                              vRateCred,
                              vCredDate, 
                              vCredPer,  
                              vCredMon,
                              vKolLgtPer,
                              STRING(LOOKUP(vCredOffs,"--,->,<-")),
                              vAnnuitCorr, 
                              vSummDepos,  
                              vFirstPeriod,
                              vPartAmount, 
                              OUTPUT vAnnSumm).
            UpdateSigns(vCondClass,vLCSurr,"АннуитПлат",STRING(vAnnSumm),?).
         END.

         IF iChLAcct EQ "ДА" THEN
         DO:
            vListType = GetXAttrInit(loan.class-code, "list_type").
            REPEAT vCounter = 1 TO NUM-ENTRIES(vListType):

               {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=LAST &fbuf=loan-acct &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE loan-acct.contract  EQ loan.contract ~
                            AND loan-acct.cont-code EQ loan.cont-code ~
                            AND loan-acct.acct-type EQ ENTRY(vCounter, vListType) "}
               IF LOCKED loan-acct THEN DO:
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    "Счет заблокирован другим пользователем" 
                                   ).
                  UNDO mb, LEAVE mb.
               END.   

               IF AVAIL loan-acct THEN
               DO:
                  loan-acct.since = iNewDate.

                  {profind.i &fnoer=TRUE &fnowh=TRUE
                     &fway=FIRST &fbuf=acct &flock=EXCLUSIVE-LOCK &ffor=TRUE
                     &fcond="WHERE acct.acct     EQ loan-acct.acct ~
                               AND acct.currency EQ loan-acct.currency "}
                  IF NOT AVAILABLE acct THEN DO:
                     IF LOCKED acct THEN 
                        RUN Fill-SysMes IN h_tmess("","","-1",
                                    "Счет заблокирован другим пользователем" 
                                   ).
                     ELSE
                        RUN Fill-SysMes IN h_tmess("","","-1", "Счет не найден" ).
                     
                     UNDO mb, LEAVE mb.
                  END.   

                  IF AVAIL acct THEN
                     acct.open-date = iNewDate.
               END.
            END.
         END.

         /* Отпускаем (разлочиваем) loan и loan-cond */
         RELEASE loan NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE loan-acct NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.            
         RELEASE acct NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE loan-cond NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE comm-rate NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
      END.
      
      /* Теперь пересчитываем графики 
      ** Сначала чистим их все ( с idnt = 1,2,3 ) */
      DO vCounter = 1 TO 3:
         FOR EACH bterm-obl WHERE bterm-obl.contract  EQ iContract
                              AND bterm-obl.cont-code EQ iContCode
                              AND bterm-obl.idnt      EQ vCounter
         NO-LOCK:
            {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=term-obl &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(term-obl) EQ RECID(bterm-obl) "}

            DELETE term-obl NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.
      END.

      RUN SetSysConf IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ",STRING(LOOKUP(vCredOffs,"--,->,<-"))).
      RUN SetSysConf IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ",STRING(LOOKUP(vIntOffs,"--,->,<-"))).

      /* Запускаем создание графиков */
      RUN mm-to.p(vLrecid,
                  vLCrecid,
                  vCredSumm,
                  1,         /* Ввод новой записи */
                  YES,
                  YES,
                  YES,
                  YES,
                  vCredRisk,
                  0) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         UNDO mb, LEAVE mb.
      
      RUN DeleteOldDataProtocol IN h_base("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
      RUN DeleteOldDataProtocol IN h_base("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").
      
      oOk = YES.
   END. /* mb: */
END PROCEDURE.

/*-------------------------------------------------------------------------
  Процедура для определения балансового счета 2-го порядка для ссудного счета.
  Номер балансового счета определяется на основании данных договора.
  -------------------------------------------------------------------------*/
PROCEDURE GetBalAcctFromLoan:
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. 
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. 
   DEF INPUT  PARAM iType         AS CHAR NO-UNDO. /* Роль счета */

   DEF VAR vTerm         AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTType        AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTKind        AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTTerm        AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR DTCust        AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR mask-internal AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR s             AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR yy            AS INT     NO-UNDO.
   DEF VAR dd            AS INT     NO-UNDO.
   DEF VAR vBal2Acct     AS CHAR    NO-UNDO. /* Балансовый счет 2-го порядка */

   DEF BUFFER loan FOR loan.

   RUN RE_B_LOAN (iContract, iContCode, BUFFER loan). 

    pick-value = ?.
   vBal2Acct  = ?.
   RUN GetDBITerm(loan.open-date,
                  loan.end-date,
                    loan.contract,
                    loan.cust-cat,
                        OUTPUT vTerm).

    RUN DTCust(loan.cust-cat, loan.cust-id, ?, OUTPUT DtCust).

    ASSIGN 
       DTType = GetXAttrInit(loan.class, "DTType")
       DTKind = GetXAttrInit(loan.class, "DTKind")
       DTTerm = Entry(3, vTerm,"/").

    IF DTType = ? OR DTType = "" THEN DTType = "*".
    IF DTKind = ? OR DTKind = "" THEN DTKind = "*".
    IF DTTerm = ? OR DTTerm = "" THEN DTTerm = "*".

   FIND FIRST code WHERE code.class = "ТипСчДог"
                     AND code.code  = iType
      NO-LOCK NO-ERROR.
   IF NOT AVAIL code THEN
      RUN Fill-SysMes("",
                      "",
                      0,
                      "Не найден код счета " + code.code
                     ).

    ASSIGN
        mask-internal = code.code + CHR(1) +   
                        DTType + CHR(1) +
                        DTCust + CHR(1) +
                        DTKind + CHR(1) +
                        DTTerm
        s = "".

    FOR EACH code WHERE code.class = "DTTerm" AND code.parent = "DTTerm"
      NO-LOCK:
       IF IS-Term(loan.open-date,
                  ( IF loan.end-date = ? THEN
                      12/31/9999
                   ELSE
                      loan.end-date),
                   code.code,
                   NO,
                   0,
                   OUTPUT yy,
                   OUTPUT dd)
       THEN
          {additem.i s code.code}

    END. /*FOR*/

    ASSIGN
       ENTRY(5,mask-internal,CHR(1)) = s
       mask = mask-internal
      .

   RUN cbracct.p("DecisionTable", "DecisionTable", "DecisionTable", -1).

   IF pick-value <> ? AND pick-value <> "" AND
      CAN-FIND( bal-acct WHERE bal-acct.bal-acct = INT(TRIM(pick-value)) )
   THEN
   DO:
      vBal2Acct = TRIM(pick-value).
      RUN SetSysConf in h_base ("БАЛАНСОВЫЙ СЧЕТ", vBal2Acct).
   END.

END PROCEDURE.

/*-------------------------------------------------------------------------
  Процедура возвращает сумму обеспечения по договору
  -------------------------------------------------------------------------*/
PROCEDURE SummObesp:
   DEFINE INPUT   PARAM iContract  AS CHARACTER    NO-UNDO. /* договор */
   DEFINE INPUT   PARAM iContCode  AS CHARACTER    NO-UNDO. /* договор */
   DEFINE INPUT   PARAM iTypeObesp AS CHARACTER    NO-UNDO. /* тип обеспечения */
   DEFINE INPUT   PARAM iDate      AS DATE         NO-UNDO. /* Дата расчёта */
   DEFINE OUTPUT  PARAM oSumm      AS INT64 INIT 0 NO-UNDO. /* Сумма обеспечения */
   
   DEFINE BUFFER gar-loan FOR loan.
   DEFINE BUFFER term-obl FOR term-obl.

   DEFINE VARIABLE vTypeShDog        AS CHARACTER   NO-UNDO. /* строка параметров */
   DEFINE VARIABLE vParam        AS CHARACTER   NO-UNDO. /* строка параметров */
   DEFINE VARIABLE vParamCode    AS CHARACTER   NO-UNDO. /* строка параметров */
   DEFINE VARIABLE vSummTmp      AS DEC         NO-UNDO.
   DEFINE VARIABLE vnn           AS CHAR        NO-UNDO.
   DEFINE VARIABLE vTmp          AS INT64       NO-UNDO.
   DEFINE VARIABLE vTmp1         AS DEC         NO-UNDO.
   DEFINE VARIABLE vTmp2         AS DEC         NO-UNDO.
   DEFINE VARIABLE vDogObesp     AS LOG INIT NO NO-UNDO.
   DEFINE VARIABLE vterm-obl-sur AS CHARACTER   NO-UNDO. /* строка параметров */

   vParam = GetCode("ПарамОбесп",iTypeObesp).
   
/* Находим обеспечение */
   FOR EACH term-obl WHERE 
            term-obl.contract   EQ iContract
        AND term-obl.cont-code  EQ iContCode
        AND term-obl.idnt       EQ 5  
        AND ((term-obl.sop-date NE ? 
          AND term-obl.sop-date GE iDate) 
          OR (term-obl.sop-date EQ ? 
          AND term-obl.end-date GE iDate))
        AND term-obl.cont-type  EQ iTypeObesp
   NO-LOCK:
        
      IF iDate GT DATE(GetXattrValueEx("term-obl", 
                                       iContract + "," + iContCode + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                                       "ДатаПост",
                                       "" )) THEN
      DO:
         /* Ищем действующий договор обеспечения */
         FIND FIRST gar-loan WHERE 
                    gar-loan.contract  EQ "КредГа" 
                AND gar-loan.cont-code EQ term-obl.lnk-cont-code
         NO-LOCK NO-ERROR.
         IF NOT AVAIL gar-loan THEN 
             FIND FIRST gar-loan WHERE 
                        gar-loan.contract  EQ "ДепГа" 
                    AND gar-loan.cont-code EQ term-obl.lnk-cont-code
             NO-LOCK NO-ERROR.

         IF AVAILABLE gar-loan THEN
         DO:
            vDogObesp = YES.
            RUN RE_PARAM (INT64(ENTRY(1,vParam)), /* код параметра договора */
                          iDate,                  /* дата расчета договора */
                          gar-loan.contract,      /* назначение договора */
                          gar-loan.cont-code,     /* номер договора */
                          OUTPUT vSummTmp,
                          OUTPUT vTmp1,
                          OUTPUT vTmp2).
            oSumm = oSumm + CurToBase ("УЧЕТНЫЙ",term-obl.currency,iDate,vSummTmp).  
         END.
      END.
   END.
   /* Если договор обеспечения не найден */
   IF NOT vDogObesp THEN
      FOR EACH term-obl WHERE 
               term-obl.contract   EQ iContract
           AND term-obl.cont-code  EQ iContCode
           AND term-obl.idnt       EQ 5  
           AND ((term-obl.sop-date NE ? 
           AND term-obl.sop-date GE iDate) 
           OR (term-obl.sop-date EQ ? 
           AND term-obl.end-date GE iDate))
      NO-LOCK:
         vterm-obl-sur = iContract + "," + iContCode + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn).
         vnn = GetXAttrValueEx ("term-obl", vterm-obl-sur, "НомерПП", "").
         vTypeShDog = GetXAttrValueEx ("term-obl", vterm-obl-sur, "ВидДогОб", "") + IF  INT(vnn) EQ 0 THEN "" ELSE vnn.
         IF vTypeShDog BEGINS iTypeObesp 
         THEN DO:
            FOR FIRST code WHERE code.parent EQ vTypeShDog
                             AND code.class  EQ "ТипСчДог"
            NO-LOCK:
                         
               vParamCode = code.code.            
                 /*первые два символа не дб, или кр - не нужны*/
               SUBSTRING(vParamCode,1,2) = "".
               FOR FIRST chowhe WHERE chowhe.id-op EQ INT(vParamCode)
               NO-LOCK:
                   vTmp = IF chowhe.id-k EQ ? THEN chowhe.id-d ELSE chowhe.id-k.
                   IF LOOKUP(STRING(vTmp),vParam) NE 0
                   THEN DO:
                      
                      RUN RE_PARAM (vTmp, /* код параметра договора */
                                    iDate,                                                        /* дата расчета договора */
                                    iContract,                                                    /* назначение договора */
                                    iContCode,                                                    /* номер договора */
                                    OUTPUT vSummTmp,
                                    OUTPUT vTmp1,
                                    OUTPUT vTmp2).
                      oSumm = oSumm + CurToBase ("УЧЕТНЫЙ",term-obl.currency,iDate,vSummTmp).  
                   END.
                END.
            END.
         END.
      END.
END PROCEDURE /* SummObesp */.

/* получение настроек индивидуальных комиссий */
procedure GetIndComm:
    def input  param iContract as char no-undo.
    def input  param iContCode as char no-undo.
    DEF output param oIdk      AS CHAR NO-UNDO.     /* Параметры списания процентов */
    DEF output param oIddk     AS CHAR NO-UNDO.     /* Параметры начисления процентов */
    DEF output param oIdDop    AS CHAR NO-UNDO.     /* Дополнительный (3-й) параметр */
    DEF output param oCommSpec AS CHAR NO-UNDO.     /* Код комиссии */
    DEF output param oCalcBack AS CHAR NO-UNDO.     /* Параметр пропуска при пересчете назад */
    DEF output param oSchemCls AS CHAR NO-UNDO.     /* Класс для "СхемНачКом" */
        
    DEF VAR vTmp      AS CHAR NO-UNDO.
    
    def buffer loan  for loan.
    def buffer code  for code.
    def buffer bcode for code.
    
    FIND FIRST loan WHERE 
               loan.contract  EQ iContract 
       AND     loan.cont-code EQ iContCode 
    NO-LOCK NO-ERROR.
    IF NOT AVAIL loan THEN
       RETURN.
    
       /* Определим по какому классу наcтройка в "СхемНачКом" */
   {shemnach.i &mSchemCls = oSchemCls &loan = loan}
    
       /* Заполнение переменных из классификатора НачКом */
    FOR EACH bcode WHERE bcode.class  EQ "СхемНачКом" 
                     AND bcode.parent EQ oSchemCls + ":СхемНачКом" 
    NO-LOCK,
       EACH code WHERE 
             code.class EQ "НачКом"
         AND code.code  EQ ENTRY(1,bcode.code,":")
    NO-LOCK:
       {additem.i oCommSpec code.code}  /* Код комиссии */
       vTmp = ENTRY(1, code.val).       
       {additem.i oIddk vTmp}           /* 1-й параметр */ 
       vTmp = ENTRY(2, code.val).       
       {additem.i oIdk vTmp}            /* 2-й параметр */
       vTmp = IF NUM-ENTRIES(code.val) GE 3 THEN ENTRY(3, code.val) ELSE "-1".
       {additem.i oIdDop vTmp}          /* 3-й параметр */
       vTmp = IF GetCodeMiscEx("СхемНачКом", code.code + ":" + oSchemCls, 3, "Нет") EQ "Да" 
                 THEN "Да" 
                 ELSE "Нет".
       {additem.i oCalcBack vTmp}       /* Список флажков расчета назад */
    END.
       /* По-умолчанию: */
    IF oCommSpec EQ "" THEN
       ASSIGN
          oCommSpec = "ЕдШтПр,%Выд,%ПрСтрах,%Мес"
          oIddk     = "171,173,175,177"
          oIdk      = "172,174,176,178"
          oIdDop    = "-1,-1,-1,-1"
          oCalcBack = "Нет,Нет,Нет,Нет".
   /* eof Заполнение переменных из классификатора НачКом */
end procedure.

PROCEDURE ExtendAcctFromLoan :
DEFINE INPUT PARAMETER  iContract as char no-undo.
DEFINE INPUT PARAMETER  iContCode as char no-undo.
DEFINE INPUT PARAMETER  idate     AS DATE NO-UNDO .
DEFINE INPUT PARAMETER  iOrder    AS INT64 NO-UNDO .
DEFINE OUTPUT PARAMETER oAcct AS CHARACTER NO-UNDO .

   DEF VAR vStr AS CHAR NO-UNDO.
   DEF VAR vNameDR AS CHAR NO-UNDO.

   {&GET-LOAN}

   vStr = GetXattrInit(loan.class-code,"СписДопСчет").
   IF Num-entries(vstr) LT iOrder THEN DO:
     oAcct = "" .
     RETURN.
   END.
   vNameDR = ENTRY(iOrder,vstr).
   oAcct = GetTempXAttrValueEx("loan",iContract + "," + iContCode,vNameDR,idate,"").  /* Берем значение счета из темпарированного ДР */
   IF oAcct EQ "" THEN
      oAcct = GetXAttrValueEx("loan",iContract + "," + iContCode,vNameDR,"").         /* Возможно был задан простой ДР , проверим и его */

END PROCEDURE. /* ExtendAcctFromLoan */

/* Процедура получает максимальный порядковый номер графика в истории 
   графиков (используя допрек NumGr на term-obl-hist) */

PROCEDURE GetMaxNumGraphHist:
   DEF INPUT  PARAM  iContract   AS CHAR NO-UNDO. 
   DEF INPUT  PARAM  iCont-code  AS CHAR NO-UNDO. 
   DEF OUTPUT PARAM  oMaxNGr     AS INT64 NO-UNDO.

   DEF VAR vMaxNGr AS INT64 NO-UNDO.
   DEF VAR vNGr    AS INT64 NO-UNDO.
                     
   vMaxNGr = 0.
   FOR EACH term-obl-hist WHERE
            term-obl-hist.contract  EQ iContract
        AND term-obl-hist.cont-code EQ iCont-code
   NO-LOCK:
      vNGr = INT64(GetXattrValueEx("term-obl-hist",
                                    STRING(term-obl-hist.tobl-id),
                                    "NumGr",
                                    "")).
      IF vNGr GT vMaxNGr THEN vMaxNGr = vNGr.
   END.

   oMaxNGr = vMaxNGr.

END PROCEDURE.

   /* Определение суммы первоначального взноса */
PROCEDURE GetFirstInvest.
   DEF INPUT  PARAM iAcctCred  AS CHAR NO-UNDO.    /* Счет депозита  */
   DEF INPUT  PARAM iCurrency  AS CHAR NO-UNDO.    /* Валюта операции */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* Дата */
   DEF INPUT  PARAM iOpStatus  AS CHAR NO-UNDO.    /* Статус операции */
   DEF OUTPUT PARAM oAmnt      AS DEC  NO-UNDO.    /* Сумма взноса */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO.    /* Дата первоначального взноса */

   DEF BUFFER op-entry FOR op-entry.
   
   FOR EACH op-entry WHERE 
            op-entry.acct-cr   EQ iAcctCred
      AND   op-entry.op-date   LE iDate
      AND   op-entry.op-status GE iOpStatus
      AND   op-entry.currency  EQ iCurrency  
      AND   op-entry.op        NE ?  
   NO-LOCK
   BY op-entry.op-date:
      IF oDate EQ ? THEN
         oDate = op-entry.op-date.
      IF oDate EQ op-entry.op-date THEN
         oAmnt = oAmnt + ( IF iCurrency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
      ELSE
         LEAVE.
   END.
END PROCEDURE.

   /* Определение дату поступления всей суммы. */
PROCEDURE GetDateAllFirstInvest.
   DEF INPUT  PARAM iAcctCred  AS CHAR NO-UNDO.    /* Счет депозита  */
   DEF INPUT  PARAM iCurrency  AS CHAR NO-UNDO.    /* Валюта операции */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* Дата */
   DEF INPUT  PARAM iOpStatus  AS CHAR NO-UNDO.    /* Статус операции */
   DEF INPUT  PARAM iSumm      AS DEC  NO-UNDO.    /* Дата */
   DEF OUTPUT PARAM oAmnt      AS DEC  NO-UNDO.    /* Сумма взноса */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO.    /* Дата первоначального взноса */

   DEF BUFFER op-entry FOR op-entry.

   FOR EACH op-entry WHERE
            op-entry.acct-cr   EQ iAcctCred
      AND   op-entry.op-date   LE iDate
      AND   op-entry.op-status GE iOpStatus
      AND   op-entry.currency  EQ iCurrency
      AND   op-entry.op        NE ?
   NO-LOCK
   BY op-entry.op-date:
      oAmnt = oAmnt + ( IF iCurrency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
      IF iSumm <= oAmnt 
      THEN DO:
         oDate = op-entry.op-date.
         LEAVE.
      END.
   END.
END PROCEDURE.

/* Дата начального решения по кредитам */
PROCEDURE GetDateNRKred.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO. /* Дата по умолчанию, если не заполнен НП ДатаНачКред */
   DEF OUTPUT PARAM oDate AS DATE NO-UNDO. /* Дата начального решения по кредитам */

   oDate = DATE(FGetSettingEx("ДатаНачКред",?,?,NO)).
   IF oDate = ? THEN 
      oDate = iDate.
END PROCEDURE.

/* Процедура сохраняет текущий график погашения по договору в term-obl-hist */
PROCEDURE SaveGraphToHistory:
   DEF INPUT  PARAM iContract    AS CHAR    NO-UNDO. 
   DEF INPUT  PARAM iCont-code   AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iSince       AS DATE    NO-UNDO. 
   DEF INPUT  PARAM iChType      AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iDescription AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iSigned      AS LOGICAL NO-UNDO.
   DEF INPUT  PARAM iSignDate    AS DATE    NO-UNDO.
   DEF INPUT  PARAM iOlap        AS LOGICAL NO-UNDO.

   DEF OUTPUT PARAM oError       AS LOGICAL NO-UNDO.

   DEF VAR vMaxNGr   AS INT64 NO-UNDO.
   DEF VAR vI        AS INT64 NO-UNDO.
   DEF VAR vIdnt     AS INT64 NO-UNDO.
   DEF VAR vTypeIdnt AS CHAR  NO-UNDO.

   DEF BUFFER loan FOR loan.
   
   /* если явно указано что не нужно сохранять график, или в настроечнике указано что для данного случая ненужно сохранять график */
   IF  GetSysConf("НеСохранятьГрафикПогашенияВИсторию") EQ "ДА" 
    OR FGetSetting ("ПрмСорхГр", iChType,"НЕТ") NE "ДА" THEN 
      /* то выходим, ничего не далая */
      RETURN.
   
   oError = FALSE.

   FIND FIRST loan WHERE
              loan.contract  EQ iContract
          AND loan.cont-code EQ iCont-code
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:
      vTypeIdnt = FGetSetting ("ГрафИст", "ГрафТип","1,2,3").

      /* Определим максимальный порядковый номер графика среди уже сохраненных 
         графиков в переменную vMaxNGr. Текущий график уже будем сохранять с 
         порядковым номером (vMaxNGr + 1) */
   
      RUN GetMaxNumGraphHist (loan.contract,
                              loan.cont-code,
                              OUTPUT vMaxNGr).
   
      DO vI = 1 TO NUM-ENTRIES(vTypeIdnt):
         vIdnt = INT64(ENTRY(vI,vTypeIdnt)) .
         RUN SetTermOblHistEx (
                             iContract
                            ,iCont-Code
                            ,iSince
                            ,vIdnt
                            ,vMaxNGr + 1
                            ,iChType 
                            ,iDescription
                            ,iSigned
                            ,iSignDate
                            , ( IF iOlap THEN "1" ELSE "" )
                            , OUTPUT oError ).

      END.
   END.
   ELSE
      oError = TRUE.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:09:34.512+04:00' */
/* $LINTFILE='pp-loan.p' */
/*prosigne+Gc6KAp1Es5RNxsZRerOQ*/
/* --- pp-loan.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am --- */
