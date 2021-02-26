/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: MYCOUNT2.P
      Comment: пересчет состояния договора вперед
   Parameters: тип договора, код договора, дата пересчета
         Uses:
      Used by:
      Created: ??/??/???? ???
     Modified: Илюха 25.07.2002
     Modified: Илюха 08.10.2002
     Modified: Илюха 31.10.2002 теперь 13 параметров
     Modified: Илюха 03.12.2002 перенос из VTB
     Modified: Илюха 17.12.2003 23075
     Modified: Fepa  16.03.2004 26698
     Modified: Илюха 09.10.2004 - Полная переделка (сравнивать с предыдущими
                                версиями бесполезно, а rat-dov.i тем более)
                                - Выкинул все лишнее
     Modified: jadv  07.11.2007 (0061310) - Подъем из спецверсии
*/

/*&GLOB PRINT_LOG YES*/

{globals.i}
{done}
{intrface.get rights} /* Загрузка инструментария по правам         */
{intrface.get xclass} /* Инструменты для работы с матасхемой       */
{intrface.get loan}   /* Инструменты по кредитам                   */
{intrface.get lv}     /* Инструменты для loan-ind & loan-var       */
{intrface.get date}   /* Инструменты для работы с датами           */
{intrface.get db2l}   /* Инструменты для  динамической работы с БД */
{intrface.get pogcr}  /* Инструменты для работы с граф. пог. в КиД */
{intrface.get tmess}  /* Инструмент обработки сообщений. */

{svarloan.def}
{t-otch.i NEW}
{loan-ost-p.i}        /* внутренние процедуры с созданием операций */

DEF INPUT PARAM iContract AS CHAR NO-UNDO.
DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
DEF INPUT PARAM iDob      AS DATE NO-UNDO.

DEF VAR mCurDate     AS DATE  NO-UNDO.
DEF VAR mErrors      AS INT64   NO-UNDO.
DEF VAR mCurErr      AS INT64   NO-UNDO.
DEF VAR mS           AS DEC   NO-UNDO.
DEF VAR mSumma24     AS DEC   NO-UNDO.
DEF VAR mSumma29     AS DEC   NO-UNDO.
DEF VAR mSumma48     AS DEC   NO-UNDO.
DEF VAR mSumma23     AS DEC   NO-UNDO.
DEF VAR mSumPr1      AS DEC   NO-UNDO.
DEF VAR mCondAvail   AS LOG   NO-UNDO.
DEF VAR mCondRec     AS RECID NO-UNDO.
DEF VAR mDelay       AS INT64   NO-UNDO.
DEF VAR mDelay1      AS INT64   NO-UNDO.
DEF VAR mDischType   AS INT64   NO-UNDO.
DEF VAR mDatPer      AS DATE  NO-UNDO.
DEF VAR mProcName    AS CHAR  NO-UNDO.
DEF VAR mRound       AS LOG   NO-UNDO.
DEF VAR mClassCode   AS CHAR  NO-UNDO.
DEF VAR mSince       AS DATE  NO-UNDO.
DEF VAR mAvtProc     AS CHAR  NO-UNDO. /*значение параметра "Авт%%"*/
DEF VAR mAvtProcPen  AS CHAR  NO-UNDO. /*значение параметра "Авт%%шт"*/
DEF VAR mExt         AS CHAR  NO-UNDO. /*значение параметра "НачШтр"*/
DEF VAR mSumStr      AS CHAR  NO-UNDO.
DEF VAR mSumStr1     AS CHAR  NO-UNDO.
DEF VAR mTypeCond    AS LOG   NO-UNDO.
DEF VAR mSurr        AS CHAR  NO-UNDO.
DEF VAR mNextPlan    AS LOG   NO-UNDO.
DEF VAR vSummaPogPr  AS DEC   NO-UNDO.
DEF VAR mIsOk        AS LOG   NO-UNDO. /* Признак успешного постр. графика комиссий */
DEF VAR vAmt1        AS DEC   NO-UNDO.
DEF VAR mKompPogr    AS CHAR  NO-UNDO.
DEF VAR mMoveRound   AS DATE  NO-UNDO.
DEF VAR vdateRasch   AS DATE  NO-UNDO.
DEF VAR mAmtId       AS INT64 NO-UNDO.
DEF VAR mContType    AS CHAR  NO-UNDO.
DEF VAR mDateN       AS DATE  NO-UNDO.
DEF VAR mDateM       AS DATE  NO-UNDO.
DEF VAR mFlRecalc    AS CHAR  NO-UNDO.

DEF BUFFER yloan-cond FOR loan-cond.
DEF BUFFER bloan FOR loan.

DEF TEMP-TABLE tt-move NO-UNDO
  FIELD mdate   AS DATE
  FIELD is-proc AS LOG
  FIELD is-pd   AS LOG
INDEX mdate mdate
.

DEF SHARED STREAM err .

{mycount.i}

FIND FIRST loan WHERE
           loan.contract  EQ iContract
       AND loan.cont-code EQ iContCode
NO-LOCK NO-ERROR.

&IF DEFINED(PRINT_LOG) &THEN

  DEF VAR mTimeVariableBeg  AS INT64 NO-UNDO.
  DEF VAR mTimeVariableDay  AS INT64 NO-UNDO.
  DEF VAR mTimeVariableEnd  AS INT64 NO-UNDO.
  DEF VAR mTimeVariable1    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable2    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable3    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable4    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable    AS INT64 NO-UNDO.

  mTimeVariable = TIME.

&ENDIF

{ch_dat_proc.i}

ASSIGN
   mCurDate   = loan.since
   mClassCode = loan.class-code
   mSince     = loan.since
   mDatPer    = GetDatPer(iContract,iContCode)
   mProcName  = GetNachProc(iContract,iContCode)
   mRound     = GetRoundFlag(iContract,iContCode)
   mFormGrKom = FGetSetting("ГрафКомН","ФормГрКом",?)
  .

/* Определить дату переключения на округления и режим учета погрешности */
RUN MovRoundLoan IN h_loan (loan.contract,
                            loan.cont-code,
                            loan.class-code,
                            OUTPUT mMoveRound,
                            OUTPUT mKompPogr).

PUT STREAM err UNFORMATTED
   " "                                   SKIP
   "ПЕРЕСЧЕТ  ДОГОВОРА " +
    GetContCode(iContract,iContCode) + " на " +
    STRING(iDob)                   AT 13 SKIP
  "Сообщения программы пересчета:" AT 13 SKIP.

IF AVAIL loan
   AND loan.close-date NE ?
   AND loan.close-date NE (idob - 1)
   AND loan.since      NE idob
THEN DO:
   PUT STREAM err UNFORMATTED
      "Договор " + GetContCode(loan.contract,loan.cont-code) + " закрыт" AT 13 SKIP.
   RETURN.
END.

IF NOT CAN-DO (GetRightClasses (mClassCode, "W"), mClassCode)
THEN DO:
   PUT STREAM err UNFORMATTED
      "Вы не имеете права изменять объекты данного класса '" +
      mClassCode + "'" AT 13 SKIP.
   {intrface.del}
   RETURN.
END.

/*считываем настроечные параметры*/
ASSIGN
   mErrors       = INT64(FGetSetting("КолОш",?,"0"))
   mAvtProc      = FGetSetting("Авт%%",?,"да")
   mAvtProcPen   = FGetSetting("Авт%%шт",?,"да")
   mExt          = FGetSetting("НачШтр",?,"")
   mDateN        = DATE(FGetSettingEx("ДатаНачКред", ?, "", NO))
   mDateN        = IF mDateN = ? THEN {&BQ-MIN-DATE} ELSE mDateN 
   .

RUN DeleteOldDataProtocol IN h_base ("СводныйГрафикОстатков").
RUN DeleteOldDataProtocol IN h_base ("СводныйГрафикКомиссий").
RUN DeleteOldDataProtocol IN h_base ("ДатаОстатка").
RUN DeleteOldDataProtocol IN h_base ("СуммаОстатка").
RUN DeleteOldDataProtocol IN h_base ("ПересчетППАннуитета").
RUN DeleteOldDataProtocol IN h_base ("ПерваяИтерацияПересчета").
RUN SetSysConf            IN h_Base ("ПересчетДоговора","Да").
RUN SetSysConf            IN h_base ("ДатаПересчетаДоговора",STRING(iDob)).


RUN RE_L_COND IN h_Loan(iContract,
                        iContCode,
                        mCurDate,
                        BUFFER loan-cond).
IF AVAIL loan-cond THEN
   ASSIGN
      mDischType = loan-cond.disch-type
      .

/*
  Определяем все возможные даты движения по договору в указанном интервале
*/

IF mCurDate <> iDob THEN
   RUN PrepareLoanMove(iContract,
                       iContCode,
                       mCurDate,
                       iDob,
                       mClassCode).

CH:
DO WHILE mCurDate < iDob ON ERROR  UNDO, RETURN
                         ON ENDKEY UNDO, RETURN :
   RELEASE tt-move.

   FIND FIRST tt-move  WHERE tt-move.mdate = mCurDate NO-ERROR.
   IF NOT AVAIL tt-move
   THEN DO:
      mCurDate = mCurDate + 1.
      NEXT CH.
   END.

   RELEASE loan-cond.

   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).
   IF AVAIL loan-cond THEN
      ASSIGN
         mCondAvail = YES
         mDelay     = loan-cond.delay
         mDelay1    = loan-cond.delay1
         mDischType = loan-cond.disch-type
         mCondRec   = RECID(loan-cond)
         .
   ELSE
      ASSIGN
         mCondAvail = NO
         mDelay     = 0
         mDelay1    = 0
         mDischType = 1
         .

   IF AVAILABLE loan-cond THEN
   DO:
      /*- ПЕРЕСЧЕТ ПС ДЛЯ АННУИТЕТНОЙ  СХЕМЫ !!!(НАДО ОПТИМИЗИРОВАТЬ) -*/
      ASSIGN
         mSurr     = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE))
         mTypeCond = GetXAttrValue("loan-cond",mSurr,"СхемаПлат") = "Аннуитетная"
         .

      mNextPlan = ProbegNextPlan(loan-cond.class-code, mSurr).
  /* здесь считать не будем */
      IF mTypeCond THEN
      DO:

         FIND LAST term-obl WHERE
                   term-obl.contract  = iContract
               AND term-obl.cont-code = iContCode
               AND term-obl.idnt      = 1
               AND term-obl.end-date  = mCurDate NO-LOCK NO-ERROR.
         IF AVAIL term-obl THEN
         DO:
            RELEASE term-obl.
     /* если не дай бог попадаем на дату планового платежа и у нас
      аннуитетная схема - проценты считаем три раза - это первый - чтобы корректно
      определить  все плановые суммы, причем проценты считаются
      неправильно с дополнительными тосками разбиения по операциям начисления
      процентов. что приводит к ошибкам округления Пока оставляем - блокируем
      в Nachislenieandoplata*/
             /* Получаем дату начала ведения договоров в системе */
             mDateM = DATE(FGetSettingMF("НРДатаСтарта",?,?,loan.filial-id, no)).
             /* Если договор мигрирированный, проверяем НП */
             IF mDateM GE loan.open-date THEN
             DO:
                mFlRecalc = FGetSettingMF("ПересчМигрАн",?,?,loan.filial-id, no).
                /* Если не пересчитывать, то проверяем, есть ли условие после даты миграц */
                IF mFlRecalc EQ "Нет" THEN
                DO:
                   FIND FIRST yloan-cond WHERE yloan-cond.contract  EQ loan.contract
                                           AND yloan-cond.cont-code EQ loan.cont-code
                                           AND yloan-cond.since     GT mDateM
                   NO-LOCK NO-ERROR.
                   /* Пересчитываем с даты условия */
                   IF AVAIL yloan-cond THEN
                   DO:
                      IF yloan-cond.since GT loan-cond.since THEN
                         RUN ps_rec.p(MAX(yloan-cond.since,mCurDate),
                                      1, 
                                      BUFFER loan, 
                                      BUFFER yloan-cond).
                      ELSE
                         RUN ps_rec.p(MAX(yloan-cond.since,mCurDate),
                                      1, 
                                      BUFFER loan, 
                                      BUFFER loan-cond).
                   END.
                END.
                ELSE
                   RUN ps_rec.p(mCurDate,1, BUFFER loan, BUFFER loan-cond).
             END.
             ELSE
                RUN ps_rec.p(mCurDate,1, BUFFER loan, BUFFER loan-cond).
         END.

      END.
   END.

   /*===== Основной блок пересчета договора ================*/
   /* пересчет проводок */
   FOR EACH loan-int WHERE
            loan-int.contract  = iContract
        AND loan-int.cont-code = iContCode
        AND loan-int.mdate     = mCurDate
   NO-LOCK:

      IF (loan-int.id-d = 30 AND loan-int.id-k = 29) OR
         (loan-int.id-d = 30 AND loan-int.id-k = 48) OR
         (loan-int.id-d = 29 AND loan-int.id-k = 31) OR
         (loan-int.id-d = 10 AND loan-int.id-k = 24) OR
         (loan-int.id-d = 33 AND loan-int.id-k = 24) OR
         (loan-int.id-d = 704 AND loan-int.id-k = 5) OR
         (loan-int.id-d = 200 AND loan-int.id-k = 5)
      THEN NEXT.

      RUN Cr_LoanVar IN h_lv (iContract,iContCode,mCurDate,ROWID(loan-int)).
   END.

      /* Срочные обязательства */
   RUN loan-ost-p IN this-procedure
        (iContract,
                  iContCode,
                  mCurDate,
                  mCondRec).
   /*
   Расчитываем текущие проценты 1 раз,в остальных процедурах просто
   выцепляем нужную сумму и сбрасываем ее в списке mSumStr
   */

   mSumStr = FILL(CHR(1),EXTENT(pick-var) - 1).
  /* Для аннуитетной схемы - это вторая точка расчета процентов */
   IF tt-move.is-proc THEN
   DO:
      /* для корректного расчета состояния договоров со спец. схемой начисления (КД_1,КД_17) */
      RUN SetSysConf IN h_base ("calc-loan-state","yes").

      vdateRasch = GetDateClc (tt-move.is-pd, mDischType,  mCurDate  ).
      RUN nach-pr_.p
         (iContract,        /* Идентификатор                         */
          iContCode,        /* договора                              */
          vdateRasch,       /* Дата расчета процентов                */
          ?,                /* Все параметры                         */
          "",               /* ..даже что не выносим на требования   */
          mDatPer,          /* дата перехода на 39П                  */
          mProcName,        /* процедура начисления                  */
          NO,               /* не сохранять loan.interest            */
          OUTPUT mS,        /* сумма по последнему параметру         */
          OUTPUT mSumStr).  /* строка текущих процентов через chr(1) */

      /* В свзяи с тем, что при расчете pint.p для 11/36 формы расчета она совершенно странно разбивает
      ** на периоды постоянства, возникает неверная сумма для корректировки. Будет разбиратсья
      ** с этим отдельно. Пока всё таки отключим компенсацию */
      IF AVAIL loan-cond
         AND NOT (loan-cond.disch-type EQ 11 OR loan-cond.disch-type EQ 36)
         AND (mMoveRound LE vdateRasch)
         AND (mKompPogr EQ "Да"
                      OR CAN-FIND(FIRST term-obl WHERE term-obl.contract  EQ iContract
                                       AND term-obl.cont-code EQ iContCode
                                       AND term-obl.end-date  EQ mCurDate
                                       AND term-obl.idnt      EQ 1)
              ) THEN
      DO:
         RUN mycount2-corr.p (
            iContract,
            iContCode,
            vdateRasch,
            OUTPUT vAmt1).
         ENTRY(1, mSumStr, CHR(1)) = STRING(vAmt1).
      END.

      /*}}}*/

   END.
   /* Запоминаем %% */
   mSumStr1 = mSumStr.

   /*
   Cначала обрабатываем зачисление на внебаланс 29 > 24 - означает, что
   кроме списанияотраженных %% списываются начисленные, но неотраженные
   */
   RUN bal-o-p (iContract,             /* Идентификатор       */
               iContCode,             /* договора            */
               mCurDate,              /* Дата                */
               mExt,                  /* Не требования       */
               mRound,                /* Округлять           */
               INPUT-OUTPUT mSumStr). /* Строка с текущими % */
   /*
   Теперь создаем остатки по начислению % на внебалансе
   */
   {crvint.i &cur-date = mCurDate &id-d = 29 &id-k = 31}
   /*
   ...и начисленные проводкой по внебалансу выравниваем остатки 29+48 и 24
   */
   RUN bal-o1-p (iContract,  /* Идентификатор */
                iContCode,  /* договора      */
                mCurDate).  /* Дата          */
   /*
   Разноска средств по начислению процентов на внебалансе
   */

   RUN nach-pro-p (iContract,              /* Идентификатор       */
                  iContCode,              /* договора            */
                  mCurDate,               /* Дата                */
                  mExt,                   /* Не требования       */
                  mRound,                 /* Округлять           */
                  INPUT-OUTPUT mSumStr).  /* Строка с текущими % */
   /*
   Начислено процентов по балансу.
   Если есть операция Начисляем и сумму операции разносим
   */
   RUN nach-prb-p (iContract,              /* Идентификатор       */
                  iContCode,              /* договора            */
                  mCurDate,               /* Дата                */
                  mExt,                   /* Не требования       */
                  mRound,                 /* Округлять           */
                  INPUT-OUTPUT mSumStr).  /* Строка с текущими % */

   /* Учитываем списание средств с внебаланса */
   {crvint.i &cur-date = mCurDate &id-d = 30 &id-k = 29}
   /*оплата вынесенных на просрочку процентов*/
   {crvint.i &cur-date = mCurDate &id-d = 30 &id-k = 48}

   {crvint.i &cur-date = mCurDate &id-d = 10 &id-k = 24}
   {crvint.i &cur-date = mCurDate &id-d = 33 &id-k = 24}

   /* Делаем разноску 4007 и 4020 операции */
   RUN opl_buypr-p (iContract,iContCode,mCurDate,mRound,mAvtProc) .

   /* Создаем просроченные проценты до выяснения ,
      если остаток по параметру 24 > 29  + 48 */
   ASSIGN
      mSumma24 = LastVarSumm(iContract,iContCode,24,mCurDate)
      mSumma29 = LastVarSumm(iContract,iContCode,29,mCurDate)
      mSumma48 = LastVarSumm(iContract,iContCode,48,mCurDate)
      mSumma23 = LastVarSumm(iContract,iContCode,23,mCurDate)
      .
   IF mSumma24 <> ? THEN
   DO:
      mSumma24 = mSumma24 - (IF mSumma29 <> ? THEN mSumma29 ELSE 0)
                          - (IF mSumma48 <> ? THEN mSumma48 ELSE 0)
                          - (IF mSumma23 <> ? THEN mSumma23 ELSE 0).
      IF mSumma24 > 0 THEN
         RUN Cr_LoanInt IN h_lv
           (iContract,      /* Идентификатор   */
            iContCode,      /* договора        */
            mCurDate,       /* Дата операции   */
            mSumma24,       /* Счумма операции */
            16,             /* п. Начислено    */
            24,             /* п. Списано      */
            YES,            /* Авт. операция   */
            ?).             /*                 */
   END.

   IF NOT AVAIL(loan-cond) THEN
   DO:
      mCurDate = mCurDate + 1.
      NEXT CH.
   END.

   /* Если есть остаток по оплате начисленных процентов */
      RUN OplataTrebovaniy(iContract,iContCode,mCurDate).

     /*
   Если оплачено > 0 создаем loan-int оплачено по тем %% кот. > 0 ???
   */
   RUN opl-pr-p(iContract,
                iContCode,
                mCurDate,
                mRound,
                mAvtProc,
                mAvtProcPen,
                mSumStr1).
   /*
   Если оплачено > 0 и есть текущие проценты (то что еще не начислено),
   то создаем операции начислено
   */
   /* а вот и третья тоска расчета процентов для аннуитетной схемы - здесь
      пересчитаем плановый платеж процентов */

   RUN NachislenieAndOplata(iContract,
                            iContCode,
                            mCurDate,
                            mSumStr,
                            mDischType,
                            mRound,
                            AVAIL loan-cond AND loan-cond.since = mCurDate).

      RUN DeleteOldDataProtocol IN h_base("calc-loan-state").
   /*
   отнесение процентов на довыяснение
   */
   {rat-dov.i}
   FIND FIRST loan-var WHERE
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode        
   NO-LOCK NO-ERROR .
   DO WHILE AVAIL loan-var :
      mAmtId = loan-var.amt-id .
      FOR EACH loan-var WHERE 
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode
        AND loan-var.amt-id    = mAmtId
        AND loan-var.since     = mCurDate NO-LOCK :
         RUN "error(lp.p"(RECID(loan-var),mCurDate,INPUT-OUTPUT mCurErr).
         IF mCurErr > mErrors THEN
         DO:
            PUT STREAM err UNFORMATTED
                "Количество ошибок превысило допустимое - " +
                STRING(mErrors)  +  " . Пересчет закончен" AT 4 SKIP.
            UNDO ch , LEAVE ch .
         END.
      END.   
      FIND FIRST loan-var WHERE
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode
        AND loan-var.amt-id    > mAmtId        
      NO-LOCK NO-ERROR .

   END.

   RUN res-o-p (iContract,
               iContCode,
               mCurDate).

   mCurDate = mCurDate + 1.
END.  /* DO WHILE */

/* для корректного расчета состояния договоров со спец. схемой начисления (КД_1,КД_17) */
RUN SetSysConf IN h_base ("calc-loan-state","yes").
/*Ищем движение по процентам на дату расчета*/
FIND FIRST tt-move  WHERE tt-move.mdate = mCurDate AND tt-move.is-proc = YES NO-ERROR.

IF NOT AVAIL loan-cond THEN

   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).
   IF AVAIL loan-cond THEN
      ASSIGN
         mDischType = loan-cond.disch-type
         .
   ELSE
      ASSIGN
         mDischType = 1
         .

IF AVAIL tt-move THEN
    vdateRasch = GetDateClc (tt-move.is-pd, mDischType, mCurDate).
ELSE
   vdateRasch = mCurDate.

RUN nach-pr_.p
      (iContract,        /* Идентификатор                         */
       iContCode,        /* договора                              */
       vdateRasch,       /* Дата расчета процентов                */
       ?,                /* Все параметры                         */
       "",               /* ..даже что не выносим на требования   */
       mDatPer,          /* дата перехода на 39П                  */
       mProcName,        /* процедура начисления                  */
       YES,              /* сохранять loan.interest               */
       OUTPUT mS,        /* сумма по последнему параметру         */
       OUTPUT mSumStr).  /* строка текущих процентов через chr(1) */

IF NOT AVAIL loan-cond  THEN
   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).

mDischType = IF AVAIL loan-cond THEN loan-cond.disch-type ELSE mDischType.

/* В свзяи с тем, что при расчете pint.p для 11/36 формы расчета она совершенно странно разбивает
** на периоды постоянства, возникает неверная сумма для корректировки. Будет разбиратсья
** с этим отдельно. Пока всё таки отключим компенсацию */
IF AVAIL loan-cond
   AND NOT (   loan-cond.disch-type EQ 11
            OR loan-cond.disch-type EQ 17
            OR loan-cond.disch-type EQ 36)
   AND (mMoveRound LE vdateRasch)
   AND (  mKompPogr EQ "Да"
               OR CAN-FIND(FIRST term-obl WHERE term-obl.contract  EQ iContract
                                            AND term-obl.cont-code EQ iContCode
                                            AND term-obl.end-date  EQ mCurDate
                                            AND term-obl.idnt      EQ 1)
      ) THEN
   RUN mycount2-corr.p (
      iContract,
      iContCode,
      vdateRasch,
      OUTPUT loan.interest[1]).

/*Корректировка 9 и 12 параметров 0168696*/
RUN corr-par.p ( iContract,
                  iContCode,
                  vDateRasch,
                  mSumStr,
                  9,
                  OUTPUT loan.interest[3]).

RUN corr-par.p ( iContract,
                  iContCode,
                  vDateRasch,
                  mSumStr,
                  12,
                  OUTPUT loan.interest[5]).

/* Сальдирование %% в конце месяца */
RUN AddAmtProc IN THIS-PROCEDURE(iContract,iContCode,mCurDate,loan.Class-Code).

RUN DeleteOldDataProtocol IN h_base("calc-loan-state").
IF GetXattrInit(loan.Class-Code,"СхемНачКом") NE "" THEN
DO:
   /* Расчет индивидуальных комиссий */
   RUN nach-ind.p (iContract,              /* Идентификатор  */
                   iContCode,              /* договора       */
                   loan.since,             /* Дата начала    */
                   iDob,                   /* Дата окончания */
                   mRound,                 /* Округлять      */
                   FALSE).                 /* не пересчет назад */

   /* расчет комиссий вынесенных на просрочку  */
   RUN nachpros.p(iContract,
                  iContCode,
                  iDob).

END.

/* Расчет доп. параметров сделки как начислений, так и выносов на просрочку */
IF loan.contract EQ "КредРО" OR CAN-DO (Ls-Class("loan_ces"),loan.class-code) THEN
   RUN calcaijk.p (loan.contract,
                   loan.cont-code,
                   iDob).

/* Пересчитаем график комиссий  */
IF mFormGrKom EQ "Да" THEN
   RUN CALC_PLAN_COMM (loan.contract,loan.cont-code,loan.open-date,loan.end-date,OUTPUT mIsOk).

RUN DeleteOldDataProtocol IN h_base("СводныйГрафикОстатков").
RUN DeleteOldDataProtocol IN h_base("СводныйГрафикКомиссий").
RUN DeleteOldDataProtocol IN h_base("ДатаОстатка").
RUN DeleteOldDataProtocol IN h_base("СуммаОстатка").
RUN DeleteOldDataProtocol IN h_base("ПересчетППАннуитета").
RUN DeleteOldDataProtocol IN h_base("ПерваяИтерацияПересчета").
RUN DeleteOldDataProtocol IN h_Base("ПересчетДоговора").
RUN DeleteOldDataProtocol IN h_Base("ДатаПересчетаДоговора").

&IF DEFINED(PRINT_LOG) &THEN
  IF  (TIME - mTimeVariable) >= 2 THEN
  DO:
     OUTPUT TO "mycount_.log" APPEND.
     PUT UNFORMATTED
        "Пересчет договора - " iContCode " c " mSince " на " iDob
     " - " STRING(TIME - mTimeVariable,"hh:mm:ss")  " " TODAY SKIP.
     OUTPUT CLOSE.
  END.
&ENDIF

FIND FIRST bloan WHERE ROWID(bloan) EQ ROWID(loan) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL bloan THEN
DO:
   RUN wholocks2.p ( RECID(loan),"loan" ,
                     program-name(1) +
                     '~n Ошибка при монопольном доступе к договору ' +
                     ENTRY(1,loan.cont-code," ")).
   UNDo, RETURN.
END.
ASSIGN bloan.l-int-date = bloan.since. /* необходимо сохранить предыдущую дату пересчета */

IF mCurErr <> 0
THEN DO:
   PUT STREAM err UNFORMATTED
       "Обнаружено - " + STRING(mCurErr) + " ошибок"  AT 13 SKIP
       "Договор пересчитан на " + STRING(mCurDate)    AT 13 SKIP.
   bloan.since = mCurDate.
END.
ELSE DO:
   PUT STREAM err UNFORMATTED
      "Ошибок не обнаружено"                  AT 13 SKIP
      "Договор пересчитан на " + STRING(iDob) AT 13 SKIP.
   bloan.since = iDob.
END.
/* Сообщение про пересчет графика комиссий */
IF NOT mIsOk THEN
   PUT STREAM err UNFORMATTED "График комиссий не пересчитан." AT 13 SKIP.
{intrface.del}
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='14/05/2015 15:58:51.658+04:00' */
/* $LINTUSER='priv' */
/* $LINTMODE='1' */
/* $LINTFILE='mycount2.p' */
/*prosignxtILBNfjv5RvM63Y7wQGVw*/