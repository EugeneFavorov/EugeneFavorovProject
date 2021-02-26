
/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: pp-pqres.p
     Comment: Расчет плановых списаний резерва для отчетности
              !!! Договор должен быть пересчитат на дату большую
                  либо равную дате отчета
  Parameters: no paramaters
         Uses:
      Used by:
     Created: Илюха
    Modified: Илюха
    Modified: опять Илюха
    Modified: 27-apr-2005 mitr (0045006)
    Modified: jadv 09.11.2007 (0034598)
*/
 
/*


F/P Name                Comment
─── ──────────────────  ────────────────────────────────────────────────────

  P CLC_LOAN_PLAN_RES - ОБЩАЯ ПРОЦЕДУРА ЗАПУСКА РАСЧЕТА ПЛАНОВОГО
                        СПИСАНИЯ РЕЗЕРВА ПО ДОГОВОРУ

  P PREPARE_SPIS_DOLG и  PREPARE_SPIS_DOLG2
                      - ПОСТРОЕНИЕ ПЛАНОВЫХ СПИСАНИЙ
                        ОСНОВНОГО ДОЛГА ПО ДОГОВОРУ

  P CALC_RESERV       - ПОСТРОЕНИЕ ПЛАНОВЫХ СПИСАНИЙ СФОРМИРОВАННОГО
                        РЕЗЕРВА ПО ДОГОВОРУ
  P SUMM_SPIS_RES     - ПОЛУЧЕНИЕ СУММЫ СПИСАНИЯ РЕЗЕРВА ЗА ПЕРИОД ДЛЯ
                        РАСПРЕДЕЛЕНИЯ РЕЗЕРВА ПО СРОКАМ В ОТЧЕТНОСТИ
  P DELETE_RES_SPIS   - УДАЛЕНИЕ ПЛАНОВЫХ СПИСАНИЙ РЕЗЕРВА ПО ДОГОВОРУ
  P SUMM_SPIS_DOLG    - СУММА СПИСАНИЯ ОСНОВНОГО ДОЛГА ЗА ПЕРИОД
  P GET_LOAN_PROCENT  - Сумма начисленных за период %%
  F LastNachDate      - Дата последнего начисления % на балансе
  P GET_SUMMA_POG     - Сумма погашения кредита за период.
  P GET_SUMMA_DOLG_RUB - Cумма задолженности и оплаты в рублях
*/

/* pp-pqres.p */
{pfuncdef
   &DefLib="pqres" 
   &Description="Расчет плановых списаний резерва для отчётности"
}
 
{pqres.def}

{globals.i}
{intrface.get xclass}
{intrface.get loan}
{intrface.get instrum}
{intrface.get i254}    /* Библиотека вычисления показателей, связанных с расчетом резерва по ссудам (254-П). */
{intrface.get rsrv}    /* Библиотека для работы с параметрами резервирования. */
{intrface.get cdrep}
{intrface.get strng}
{intrface.get loanx}
{intrface.get comm}
{intrface.get acct}

/*{loan.pro} он уже есть в {intrface.get loan} */
{lshpr.pro}

{svarloan.def NEW GLOBAL}
{t-otch.i NEW}
{sh-defs.i new}
{tt_getinfdogob.def}

&GLOB RES_PARAM 21
&GLOB SOGL_TYPE "Течение"
&GLOB CONTRACT  "Кредит"
&GLOB RES_IDNT  4
&GLOB iskldbparam "95"
/*&GLOB PqresLog YES*/

/* Скорректированные даты плановых выплат */
DEF TEMP-TABLE ttPlan NO-UNDO
   FIELD CONT-CODE AS CHAR
   FIELD end-date AS DATE
   FIELD summa    AS DEC
   FIELD old-date AS DATE
INDEX end-date end-date
INDEX CONT-CODE END-DATE.


DEF TEMP-TABLE ttRes NO-UNDO
   FIELD end-date AS DATE
   FIELD summa    AS DEC
INDEX end-date end-date.

/* обязательства, которые нужно учитывать */
DEF TEMP-TABLE ttTerm NO-UNDO
   FIELD end-date AS DATE
INDEX end-date end-date.

DEF TEMP-TABLE ttLoanInt NO-UNDO
   FIELD cont-code AS CHAR
   FIELD mdate     AS DATE
INDEX indx1 mdate.

/* счета по которомы считать остоток, которые нужно учитывать */
DEF TEMP-TABLE ttAcct NO-UNDO
   FIELD acct AS CHARACTER
INDEX acct acct.
/* */
DEF TEMP-TABLE tt-SumDolg NO-UNDO
   FIELD contract    AS CHARACTER
   FIELD cont-code   AS CHARACTER
   FIELD AcctType    AS CHARACTER
   FIELD since       AS DATE
   FIELD AllBasaOst  AS DECIMAL
   FIELD TranshCount AS INT64
INDEX idx1 contract cont-code AcctType since.

/* таблица для поиска по классификатору РезервДог */
DEF TEMP-TABLE tt-ReservDog NO-UNDO
   FIELD res_par   AS CHAR /* параметр резерва */
   FIELD res_sch   AS CHAR /* счет резерва */
   FIELD plus_par  AS CHAR /* параметры требований */
   FIELD minus_par AS CHAR /* параметры требований, минусуются */
INDEX idx1 res_par.

/* таблица для кеширования распределения резерва в между траншами */
DEF TEMP-TABLE tt-ResLoan NO-UNDO
   FIELD res_par   AS CHAR /* параметр резерва */
   FIELD cont-code AS CHAR /* номер договора */
   FIELD sum_tr    AS DEC  /* счет резерва */
   FIELD sum_res   AS DEC  /* параметры требований */
INDEX prim res_par cont-code.

DEF TEMP-TABLE tt-LoanTmp NO-UNDO
   FIELD id AS RECID                   /* Идентификатор записи      */
   FIELD contract  AS CHARACTER
   FIELD cont-code AS CHARACTER
INDEX id id
INDEX cont contract cont-code.

/* таблица для кеширования распределения расчётного резерва в между траншами */
DEF TEMP-TABLE tt-ResRasch NO-UNDO
   FIELD cont-code AS CHAR
   FIELD end-date  AS DATE
   FIELD summa     AS DEC
INDEX prim cont-code end-date.

/* таблица для распределения обеспечений по траншам */
DEF TEMP-TABLE tt-GarRasch NO-UNDO
   FIELD cont-code AS CHAR
   FIELD end-date  AS DATE
   FIELD summa     AS DEC
   FIELD dolg      AS DEC
   FIELD vidob     AS CHAR
   FIELD KK        AS CHAR
   FIELD Qsumma    AS DEC
INDEX prim cont-code end-date KK.

DEFINE STREAM sOut.

DEFINE VARIABLE mFOiAKredRole AS CHARACTER NO-UNDO.  /* значение НП ФОиАКредРоль */
DEFINE VARIABLE mParz1        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz2        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz3        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz4        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp1        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp2        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp3        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp4        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mAlgFIFO      AS LOG       NO-UNDO .
DEFINE VARIABLE mProcPar      AS CHARACTER NO-UNDO. /* Параметры, по которым определяется наличие просрочки */
DEFINE VARIABLE mProcPar139   AS CHARACTER NO-UNDO. /* Параметры, по которым определяется наличие просрочки */
DEFINE VARIABLE mDelay        AS LOGICAL   NO-UNDO. /* Параметр, который определяет учитывать или нет пробег оплаты ссуды */
DEFINE VARIABLE mCurrDayStr   AS CHARACTER NO-UNDO . /* Учитывать просрочку в текущем дне */
DEFINE VARIABLE mCurrDay      AS LOGICAL   NO-UNDO . /* Учитывать просрочку в текущем дне */
DEFINE VARIABLE mDateNR       AS DATE      NO-UNDO.  /* Дата НР из НП ДатаНачКред */
DEFINE VARIABLE mPutProt      AS LOG       NO-UNDO. /* Выводить ли протокол расчета в файл calcrsrv.log */
DEFINE VARIABLE mNPRazdYchet      AS CHARACTER NO-UNDO. /* НП РаздУчет */
DEFINE VARIABLE mNPOverClassTranz AS CHARACTER NO-UNDO. /* НП ОверКлассТранз */
DEFINE VARIABLE mNPNachStraph     AS CHARACTER NO-UNDO. /* НП НачШтр */
DEFINE VARIABLE mI139_Int         AS CHARACTER NO-UNDO. /* Начисленные проценты для 139И - НП И139_8861НачПроц */
DEFINE VARIABLE mI139_PastDueInt  AS CHARACTER NO-UNDO. /* Просроченные проценты для 139И - НП И139_8861ПрПроц */
DEFINE VARIABLE mRoleLst          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mModLoan          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mChildsListLoan   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSprReservCCode   AS CHARACTER NO-UNDO.

{pp-acctrisk.pro}
{pqres-buff.pro} /*процедуры в которые вместо iContract iContCode передаем buffer loan*/

PROCEDURE StartInterface.
   ASSIGN
      mFOiAKredRole  = FGetSetting("ФОиАКредРоль", "",               "")
      mParz1         = FGetSetting("Форма115П",    "СуммаЗадСсуда",  "")
      mParz2         = FGetSetting("Форма115П",    "СуммаЗадКомисс", "")
      mParz3         = FGetSetting("Форма115П",    "СуммаЗадПенни",  "")
      mParz4         = FGetSetting("Форма115П",    "СуммаЗадТреб",   "")
      mParp1         = FGetSetting("Форма115П",    "СуммаПросрСсуда","")
      mParp2         = FGetSetting("Форма115П",    "СуммаПросрКомисс","")
      mParp3         = FGetSetting("Форма115П",    "СуммаПросрПенни","")
      mParp4         = FGetSetting("Форма115П",    "СуммаПросрТреб", "")
      mProcPar       = FGetSetting("Форма115П",    "ЕстьПросрочка",  "")
      mDelay         = FGetSetting("ПробегОплаты", "ПробегОплСсуды", "Нет")   =  "Да"      
      mAlgFIFO       = FGetSetting("Форма115П",    "УчПог115",       "ДА")    =  "Да"
      mDateNR        = DATE(FGetSetting("ДатаНачКред",  "",          ""))
      mCurrDayStr    = fGetSetting("Форма115П", 
                                   "УчТекД"  , 
                                   "")
      mCurrDay       = (if {assigned mCurrDayStr} then
                          fGetSetting("Форма115П", 
                                      "УчТекД", 
                                      "НЕТ")
                       else
                          fGetSetting("ПОС","ПОСПрТекД",  "НЕТ")) =  "ДА"
      mPutProt          = FGetSetting("rsvrStream","", "") =  "Да"
      mNPRazdYchet      = FGetSetting("РаздУчет",?,"Нет")
      mNPOverClassTranz = FGetSetting("ОверКлассТранз","ИсклКлассКомпл","")
      mNPNachStraph     = FGetSetting("НачШтр",?,"")
      mI139_Int         = fGetSetting("110И","И139_8861НачПроц", "")
      mI139_PastDueInt  = fGetSetting("110И","И139_8861ПрПроц", "")
      mProcPar139       = FGetSetting("110И","ПарПроср","")
      mRoleLst          = FGetSetting("АвтСчКР",?,"")
      mModLoan          = fGetSetting ("Модули","Mod_Loan","Нет") =  "ДА"
      mSprReservCCode   = FGetSetting("СпрРезервДог",?,"РезервДог") 
   .
   IF mProcPar139 =  "" THEN mProcPar139 = FGetSetting("ПОС", "ПОСКонтрПар", "").
      /* алгоритм не нужен (может взятся из ПОСКонтрПар) */
   mProcPar139 = IF NUM-ENTRIES(mProcPar139,":") =  2 THEN ENTRY(2,mProcPar139,":")           
                                                      ELSE mProcPar139.
   RETURN.
END PROCEDURE.

PROCEDURE DestroyInterface:
   {intrface.del}
END PROCEDURE.
/*** ПРОЦЕДУРКИ ДЛЯ РЕЗЕРВА **************************************************/

/*===========================================================================*/
/*= ОБЩАЯ ПРОЦЕДУРА ЗАПУСКА РАСЧЕТА ПЛАНОВОГО СПИСАНИЯ РЕЗЕРВА ПО ДОГОВОРУ  =*/
/*===========================================================================*/
PROCEDURE CLC_LOAN_PLAN_RES:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate     AS DATE NO-UNDO.

   DEF VAR vLoanClass   AS CHAR NO-UNDO.
   DEF VAR vContType    AS CHAR NO-UNDO.
   DEF VAR vParamReserv AS DEC NO-UNDO.
   DEF VAR vDb          AS DEC NO-UNDO.
   DEF VAR vCr          AS DEC NO-UNDO.
   DEF VAR vOstatok     AS DEC NO-UNDO.
   DEF VAR vRemLoan     AS DEC NO-UNDO.
   DEF VAR vEndDate     AS DATE NO-UNDO.

   DEF BUFFER bloan         FOR loan.
   DEF BUFFER loan          FOR loan.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF NOT AVAIL loan THEN
      RETURN.
      
   IF IsLoanUnComLines (iContract, iContCode) THEN
      vEndDate = DATE(GetXAttrValue("loan",
                                    iContract + "," + iContCode,
                                    "FinRevDate")).
   ELSE 
      vEndDate = loan.end-date.
   
   IF    vEndDate =  ? 
      OR vEndDate <  iDate THEN 
      RETURN.

   ASSIGN
      vContType  = loan.cont-type
      vLoanClass = loan.class-code
      .

   IF NOT CAN-DO( GetXAttrInit(vLoanClass,"rel_type"), "КредРез")
   THEN
      RETURN.

   RUN RE_PARAM IN h_loan ({&RES_PARAM},          /* Код параметра   */
                           iDate,                 /* Дата расчета    */
                           iContract,             /* Назначение      */
                           iContCode,             /* Номер договора  */
                           OUTPUT vParamReserv,   /* Сумма параметра */
                           OUTPUT vDb,            /* Оборот ДБ */
                           OUTPUT vCr).           /* Оборот Кр */

   IF vParamReserv >= 0 THEN RETURN.

      RUN PREPARE_SPIS_DOLG (iContract,iContCode,iDate,
                             FGetSetting("ПробегОплаты",
                                         "ПробегОплСсуды",
                                         "Нет") = "Да",YES,YES,OUTPUT vOstatok).

  /* Проверка, что кредитная линия */

      FOR EACH bloan WHERE
               bloan.contract = iContract
           AND bloan.cont-code BEGINS iContCode + " "
      NO-LOCK:
       IF RECID(bloan) <> RECID(loan)
       THEN LEAVE.
      END.
      IF AVAIL bloan THEN DO : /* кредитная линия */
      vOstatok = 0 .

         FOR EACH bloan WHERE
               bloan.contract = iContract
           AND bloan.cont-code BEGINS iContCode + " "
          NO-LOCK:
             IF RECID(bloan) = RECID(loan)
             THEN NEXT.
             RUN PREPARE_SPIS_DOLG (iContract,
                                bLoan.Cont-Code,
                                iDate,
                                FGetSetting("ПробегОплаты",
                                           "ПробегОплСсуды",
                                           "Нет") = "Да",
                                YES,
                                YES,
                                OUTPUT vRemLoan).


         vOstatok = vOstatok + vRemLoan.
         END.
      END.
   RUN CALC_RESERV(iContract,iContCode,iDate,ABS(vParamReserv),vOstatok).

END PROCEDURE.
/* ========================================================================= */
/* ==== ПОСТРОЕНИЕ ПЛАНОВЫХ СПИСАНИЙ ОСНОВНОГО ДОЛГА ПО ДОГОВОРУ =========== */
/* ========================================================================= */
PROCEDURE PREPARE_SPIS_DOLG_COMMON:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Иднентификатор        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* Учитывать ли пробег   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* Учитывать пролонгацию */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEFINE INPUT PARAMETER iDirectly AS LOGICAL NO-UNDO. /*направление
                                                       перебора ttPLan*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* Остаток по договору   */

   DEF VAR vOstParam AS DEC NO-UNDO.
   DEF VAR vDovParam AS DEC NO-UNDO.
   DEF VAR vDb       AS DEC NO-UNDO.
   DEF VAR vCr       AS DEC NO-UNDO.
   DEF VAR vOstatok  AS DEC NO-UNDO.
   DEF VAR vSumma    AS DEC NO-UNDO.

   DEF BUFFER loan FOR loan.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF (loan.close-date <> ? AND loan.close-date < iDate) OR
       loan.open-date  > iDate
   THEN
      RETURN.

   IF loan.since >= iDate THEN
   DO:

      RUN RE_PARAM_EX IN h_loan (0,                     /* Код параметра   */
                                 iDate,                 /* Дата расчета    */
                                 loan.since,
                                 iContract,             /* Назначение      */
                                 iContCode,             /* Номер договора  */
                                 OUTPUT vOstParam,      /* Сумма параметра */
                                 OUTPUT vDb,            /* Оборот ДБ */
                                 OUTPUT vCr).           /* Оборот Кр */

      RUN RE_PARAM_EX IN h_loan (13,                    /* Код параметра   */
                                 iDate,                 /* Дата расчета    */
                                 loan.since,
                                 iContract,             /* Назначение      */
                                 iContCode,             /* Номер договора  */
                                 OUTPUT vDovParam,      /* Сумма параметра */
                                 OUTPUT vDb,            /* Оборот ДБ */
                                 OUTPUT vCr).           /* Оборот Кр */

      /* Ругаемся */
      IF vOstParam = 0 AND
         vDovParam = 0 AND
         iRet
      THEN DO:
         RETURN /*"ERROR"*/ .
      END.

   END.
   ELSE
      RUN CLC_LOAN_OST_ON_DATE(iContract,
                               iContCode,
                               iDate,
                               loan.since,
                               loan.since,
                               OUTPUT vOstParam,
                               OUTPUT vDovParam).
   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "Номер договора " iContCode
      " Дата отчета " iDate FORMAT "99/99/9999"               SKIP
      "Параметр  0: " STRING(vOstParam,"->>>,>>>,>>>,>>9.99") SKIP
      "Параметр 13: " STRING(vDovParam,"->>>,>>>,>>>,>>9.99") SKIP.
   OUTPUT CLOSE.
   &ENDIF
   /* !!!! ПРОВЕРИТЬ КОРРЕКТНОСТЬ !!!! */
   ASSIGN
      oRem     = vOstParam + vDovParam
      vOstatok = vOstParam
      .

   IF IsLoanUnComLines (iContract,iContCode) THEN
      RUN PREPARE_REAL_OPV(iContract,iContCode,iDelay,iProl,iDate,DATE(GetXAttrValue("loan",
                                                                                     iContract + "," + iContCode,
                                                                                     "FinRevDate"))).
   ELSE
      RUN PREPARE_REAL_OPV(iContract,iContCode,iDelay,iProl,iDate,loan.end-date).

   IF iDirectly =  FALSE THEN
   DO:
      FOR EACH ttPlan WHERE ttPlan.cont-code = icontcode NO-LOCK
         BY ttPlan.end-date DESC:

         IF ttPlan.end-date < iDate THEN LEAVE.
         ASSIGN
            vSumma   = MIN(vOstatok,ttPlan.Summa)
            vOstatok = vOstatok - vSumma
            .

         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vSumma
            .

         IF vOstatok <= 0 THEN LEAVE.
      END.
   END.
   ELSE
   DO:
      /*mitr: здесь изменен порядок обхода ttPLan*/
      FOR EACH ttPlan WHERE ttplan.cont-code = icontcode
         NO-LOCK BY ttPlan.end-date :

         IF ttPlan.end-date < iDate THEN LEAVE.

         ASSIGN
            vSumma   = MIN(vOstatok,ttPlan.Summa)
            vOstatok = vOstatok - vSumma
            .

         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vSumma
            .
         IF vOstatok <= 0 THEN LEAVE.
      END.
   END.

   /* !!! Не знаю куда относить либо на 1 день либо к ближайшей ПД */
   IF vOstatok > 0 THEN
   DO:
      FOR EACH ttPlan WHERE ttPlan.cont-code = icontcode
         NO-LOCK BY ttPlan.end-date:
         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vOstatok
            .
         LEAVE.
      END.
   END.

   IF vDovParam > 0 THEN
   DO:

      CREATE ttSpis.

      ASSIGN
         ttSpis.end-date = iDate
         ttSpis.summa    = vDovParam
         .
   END.

   RELEASE ttSpis.


END PROCEDURE.
/* ========================================================================= */
/* === ПОСТРОЕНИЕ РЕАЛЬНЫХ ПЛАНОВЫХ ДАТ ПОГАШЕНИЯ ОСНОВНОГО ДОЛГА КОТОРЫЕ == */
/* === БЫЛИ ДО ПРОЛОНГАЦИИ И С УЧЕТОМ ПРОБЕГА ============================== */
/* ========================================================================= */
PROCEDURE PREPARE_REAL_OPV:
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* Иднентификатор        */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* Договора              */
   DEF INPUT  PARAM iDelay       AS LOG  NO-UNDO. /* Учитывать ли пробег   */
   DEF INPUT  PARAM iProl        AS LOG  NO-UNDO. /* Учитывать пролонгацию */
   DEF INPUT  PARAM iDate        AS DATE NO-UNDO. /* Дата расчета          */
   DEF INPUT  PARAM iLoanEndDate AS DATE NO-UNDO. /* loan.end-date */

   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER loan-cond FOR loan-cond.
   DEF BUFFER pro-obl   FOR pro-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 3
      NO-LOCK BY term-obl.end-date:

      vDate = iLoanEndDate. /* Дата окончания договора */
      /* Для бессрочных */
      IF vDate = ? THEN 
         vDate = term-obl.end-date.
      
      IF iProl THEN DO:
        FOR EACH pro-obl WHERE
                  pro-obl.contract   = iContract
              AND pro-obl.cont-code  = iContCode
              AND pro-obl.idnt       = term-obl.idnt
              AND pro-obl.new-nn     = term-obl.nn
              AND pro-obl.n-end-date = term-obl.end-date
              AND pro-obl.pr-date   > iDate
         NO-LOCK BY pro-obl.pr-date /*DESC*/ BY pro-obl.nn /*DESC*/ :
            vDate = pro-obl.end-date. /* Дата окончания договора  с учетом пролонгации */
            LEAVE.
         END.
      END.

            /* Если учитывать пробег (при НП ПробегОплСсуды = Да), то: */
      IF iDelay THEN
      DO:
         /* Дата term-obl.dsc-beg-date должно быть меньше либо равна дате окончания (пролонгации) договора.*/
         IF    term-obl.dsc-beg-date <= vDate 
            OR term-obl.dsc-beg-date =  ? THEN
            vDate = IF term-obl.dsc-beg-date =  ? THEN term-obl.end-date ELSE term-obl.dsc-beg-date.
      END.
      ELSE  /* Иначе (ПробегОплСсуды=Нет)                              */
      DO:
         /* Проверяется строго поле term-obl.end-date. */
         IF term-obl.end-date <= vDate THEN
            vDate = term-obl.end-date.
      END.

      /* Проверка на вхождение в период срока */
      IF vDate >= iDate THEN
      DO:
         CREATE ttPlan.
         ASSIGN
            ttPlan.end-date = vDate
            ttPlan.summa    = term-obl.amt-rub
            ttPlan.old-date = term-obl.end-date
            ttPlan.cont-code  = icontcode
            .
      END.
   END.
   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
     " Договор " iContCode " Плановые списания основного долга" SKIP.

   FOR EACH ttPlan:
       PUT UNFORMATTED
          ttPlan.end-date FORMAT "99/99/9999"
          ttPlan.summa   FORMAT  "->>>,>>>,>>>,>>9.99" SKIP.
   END.

   OUTPUT CLOSE.
   &ENDIF

END PROCEDURE.
/* ========================================================================= */
/* === ПОСТРОЕНИЕ ПЛАНОВЫХ СПИСАНИЙ СФОРМИРОВАННОГО РЕЗЕРВА ПО ДОГОВОРУ ==== */
/* ========================================================================= */
PROCEDURE CALC_RESERV:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /* Договора */
   DEF INPUT PARAM iDate     AS DATE NO-UNDO. /* Дата расчета */
   DEF INPUT PARAM iReserv   AS DEC  NO-UNDO. /* Остаток 21 параметра */
   DEF INPUT PARAM iFullOst  AS DEC  NO-UNDO. /* 0 + 13 параметр */


   DEF VAR vPlanRes  AS DEC NO-UNDO. /* Сформированный резерв (21 параметр)*/
   DEF VAR vSpisRes  AS DEC NO-UNDO. /* */
   DEF VAR vPlanSpis AS DEC NO-UNDO. /* Плановое списание резерва*/

   ASSIGN
      vSpisRes = 0       /* То что уже списано */
      vPlanRes = iReserv /* Сформированный резерв*/
      .

   FOR EACH ttSpis BREAK BY ttSpis.end-date:
      ACCUM ttSpis.summa (TOTAL BY ttSpis.end-date).

      IF LAST (ttSpis.end-date) THEN
      DO:

         vPlanSpis = vPlanRes - vSpisRes.
         CREATE ttRes.
         ASSIGN
            ttRes.end-date = ttSpis.end-date
            ttRes.summa    = vPlanSpis
            .
      END.
      ELSE
      IF LAST-OF(ttSpis.end-date) THEN
      DO:
        (ACCUM TOTAL BY ttSpis.end-date ttSpis.summa) .

          ASSIGN
            vPlanSpis = vPlanRes * (ACCUM TOTAL BY ttSpis.end-date ttSpis.summa) / iFullOst
            vSpisRes  = vSpisRes + vPlanSpis
            .
         CREATE ttRes.
         ASSIGN
            ttRes.end-date = ttSpis.end-date
            ttRes.summa    = vPlanSpis
            .
      END.

   END.

END PROCEDURE.
/* ========================================================================= */
/* === ПОЛУЧЕНИЕ СУММЫ СПИСАНИЯ РЕЗЕРВА ЗА ПЕРИОД ДЛЯ РАСПРЕДЕЛЕНИЯ ======== */
/* === РЕЗЕРВА ПО СРОКАМ В ОТЧЕТНОСТИ  ===================================== */
/* ========================================================================= */
PROCEDURE SUMM_SPIS_RES:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* Начало периода */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* Окончание периода */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* Сумма списания резерва */

   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.

   RUN EMPTY_TABLES.

   oSumma = 0.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF     IsLoanUnComLines (iContract, iContCode) 
      AND DATE(GetXAttrValue("loan",
                             iContract + "," + iContCode,
                             "FinRevDate")) =  ? THEN RETURN.
   
   ELSE IF loan.end-date = ? THEN RETURN.

   IF LOOKUP("КредРез",GetXAttrEx(loan.class-code,"rel_type","initial")) = 0
   THEN
      RETURN.
   RUN CLC_LOAN_PLAN_RES (iContract,iContCode,iDate).

   FOR EACH ttRes WHERE
            ttRes.end-date >= iBegDate
        AND ttRes.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttRes.summa.
   END.
   oSumma = ROUND(oSumma,2).

END PROCEDURE.
/* ========================================================================= */
/*обход  ttSpis====== */
/* ==!!! C ============================================================ */
PROCEDURE GET_SPIS:

   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* Начало периода */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* Окончание периода */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* Сумма списания резерва */

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iBegDate
        AND ttSpis.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.
END PROCEDURE.
/* ========================================================================= */
/* === ПОЛУЧЕНИЕ СУММЫ СПИСАНИЯ ОСНОВНОГО ДОЛГА ЗА ПЕРИОД ========= ======== */
/* === ДЛЯ РАСПРЕДЕЛЕНИЯ ОСТАТКА ЗАДОЛЖЕННОСТИ ПО СРОКАМ В ОТЧЕТНОСТИ ====== */
/* ==!!! C ============================================================ */
PROCEDURE SUMM_SPIS_DOLG:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* Начало периода */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* Окончание периода */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* Учет пробега */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* Учет пролонгации */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* Сумма списания резерва */

   DEF VAR vOstatok AS DEC NO-UNDO.

   RUN EMPTY_TABLES.

   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "Номер договора " iContCode " Дата отчета " iDate FORMAT "99/99/9999"  SKIP
      "Срок с " iBegDate FORMAT "99/99/9999"
      " по "
       iEndDate  FORMAT "99/99/9999" SKIP.
   OUTPUT CLOSE.
   &ENDIF

   RUN PREPARE_SPIS_DOLG (iContract,iContCode,iDate,iDelay,iProl,YES,OUTPUT vOstatok).
   RUN  GET_SPIS(iBegDate, iEndDate, OUTPUT oSumma).

END PROCEDURE.


/* ========================================================================= */
/* === УДАЛЕНИЕ ПЛАНОВЫХ СПИСАНИЙ РЕЗЕРВА ПО ДОГОВОРУ ====================== */
/* ========================================================================= */
PROCEDURE DELETE_RES_SPIS:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора*/

   DEF BUFFER term-obl FOR term-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = {&RES_IDNT}
      EXCLUSIVE-LOCK:
      DELETE term-obl.
   END.

END PROCEDURE.
/* ========================================================================= */
/* === Расчет процентов по одному договору. ================================ */
/* ========================================================================= */
PROCEDURE GET_LOAN_PROCENT:

   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* договора */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* !!! начало периода - 1 */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* окончание периода */
   DEF INPUT  PARAM iParam   AS INT64  NO-UNDO. /* код параметра %% */
   DEF INPUT  PARAM iTypeOst   AS INT64  NO-UNDO. /* код параметра %% */
   DEF OUTPUT PARAM oSummaProc AS DEC  NO-UNDO. /* Сумма %% */

   RUN pint.p (iContract, iContCode, iBegDate, iEndDate, STRING(iParam)).
   FOR EACH otch1:
      ACCUM otch1.summ_pr (TOTAL) .
   END.

   oSummaProc = ACCUM TOTAL  otch1.summ_pr.

END PROCEDURE.
/* ========================================================================= */
/* == Дата последнего начисления %% на балансе ============================= */
/* ========================================================================= */
FUNCTION LastNachDate RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,
    iLastDate AS DATE,
    iDate     AS DATE):

   DEFINE BUFFER loan-int FOR loan-int.
    /* Определяем дату последнего начисления на 4ый параметр
       до даты enddate включительно */
    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.id-d     = 4
         AND loan-int.mdate   <= iDate
    NO-LOCK BY loan-int.mdate DESC:
      IF iLastDate < loan-int.mdate THEN  
         iLastDate = loan-int.mdate.
      LEAVE.
    END.

    /* Возвращаем дату последнего начисления %% на балансе */
    RETURN iLastDate.

END FUNCTION.

/* ========================================================================= */
/* == Сумма начисленных процентов + просроченных ( 16 параметр ) =========== */
/* ========================================================================= */
PROCEDURE GetProcentUnAccount :

   DEF INPUT  PARAM vContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM vContCode AS CHAR NO-UNDO. /* договора      */
   DEF INPUT  PARAM vEndDate  AS DATE NO-UNDO. /* Дата последнего начисления */
   DEF OUTPUT PARAM oAmtProc  AS DEC  NO-UNDO. /* Начисленные проценты по кредитной ведомости + просроченные проценты */

   DEF VAR vBegDate   AS DATE NO-UNDO. /* Дата последнего начисления процентов */
   DEF VAR vPint      AS DEC  NO-UNDO. /* Сумма начисленных процентов по ведомости */
   DEF VAR PastDueInt AS DEC  NO-UNDO. /* Сумма просроченных процентов, минус оплаченные и
                                          минус средства по начислению %% ( 31 и 32 парам )*/
   DEF VAR vListParam AS CHAR NO-UNDO. /* Список вычисляемых параметров */
   DEF VAR vLDopParam AS CHAR NO-UNDO. /* Дополнительный список вычисляемых параметров */
   DEF VAR vI         AS INT64  NO-UNDO. /* Счетчик */
   DEF VAR vPastDI    AS DEC  NO-UNDO. /* Значения параметров из mListParam */
   DEF VAR vCrPastDI  AS DEC  NO-UNDO.
   DEF VAR vDbPastDI  AS DEC  NO-UNDO.
   DEF VAR vParam     AS DEC  NO-UNDO.
   DEF VAR vParamDate AS DATE NO-UNDO.

   DEF BUFFER loan FOR loan.
   FIND FIRST loan where
         loan.contract  = vContract AND
         loan.cont-code = vContCode NO-LOCK NO-ERROR .

   IF loan.class-code =  "own-bill-liability"  OR
      loan.class-code  =  "dsc-bill-asset"
     THEN DO:
       RUN bprocper.p (vContract,
                       vContCode,
                       loan.open-date,
                       vEndDate,
                       "",
                       OUTPUT oAmtProc).
     END.
     ELSE DO:
      /* vListParam = "16,29,6,31,32". */
      vListParam = "16,6,31,32".
      vLDopParam = GetSysConf("AddParams").
      IF {assigned vLDopParam} THEN
         vListParam = vListParam + "," + vLDopParam.

   /* Вычисляем дату последнего начисления процентов */
   vBegDate = lastNachDate (vContract,
                            vContCode,
                            loan.open-date,
                            vEndDate ).

   /* Ищем сумму начисленных за период %% */
      IF vBegDate <> vEndDate  THEN
   RUN get_loan_procent (vContract,
                         vContCode,
                         IF vBegDate <  vEndDate THEN vBegDate + 1 ELSE vBegDate,
                         vEndDate,
                         4,
                         1,
                         OUTPUT vPint ).

   /* Вычисляем значение - сумму параметров 16,29,6,32 и 31 параметров.
      По сути это сумма просроченных процентов, минус оплаченные и
      минус средства по начислению %% ( 31 и 32 парам )*/
   DO vI = 1 TO NUM-ENTRIES(vListParam):
      RUN STNDRT_PARAM IN h_loan(vContract,
                                 vContCode,
                                 INT64(ENTRY(vI,vListParam)),
                                 vEndDate,
                                 OUTPUT vPastDI,
                                 OUTPUT vCrPastDI,
                                 OUTPUT vDbPastDI).

      PastDueInt = PastDueInt + vPastDI .
   END.

   RUN GET_PARAM IN h_loan (vContract,
                            vContCode,
                            4,
                            vEndDate,
                            OUTPUT vParam,
                            OUTPUT vParamDate
                            ).

   /* Складываем начисленные проценты по ведомости и просроченные проценты и
      вычитаем оплаченные + средства по начислению %% */
      IF vPint       = ? then vPint = 0.
      IF PastDueInt  = ? then PastDueInt = 0.
      IF vParam      = ? then vParam     = 0.

   oAmtProc = vPint + PastDueInt + vParam .
   END.

   IF oAmtProc < 0 THEN oAmtProc = 0.
END PROCEDURE.

/* ========================================================================= */
/* === Чистка временных табличек =========================================== */
/* ========================================================================= */
PROCEDURE EMPTY_TABLES:

   EMPTY TEMP-TABLE ttSpis.
   EMPTY TEMP-TABLE ttPlan.
   EMPTY TEMP-TABLE ttRes.
   EMPTY TEMP-TABLE ttTerm.

END PROCEDURE.
/* ========================================================================= */
/*  РАСЧЕТ БУДУЩИХ ЗНАЧЕНИЙ ПАРАМЕТРОВ 0 & 13 НА ДАТУ БОЛЬШУЮ ДАТЫ ПЕРЕСЧЕТА */
/*  ДОГОВОРА С УЧЕТОМ ПЛАНОВЫХ ГАШЕНИЙ - ИСПОЛЬЗУЕТСЯ ДЛЯ ОПРЕДЕЛЕНИЯ        */
/*  ОСТАТКА ДЛЯ СПИСАНИЯ ПО СРОКАМ, ХОТЯ РЕАЛЬНО ДОГОВОР ВСЕГДА ДОЛЖЕН БЫТЬ  */
/*  ПЕРЕСЧИТАН НА ДАТУ БОЛЬШУЮ ИЛИ РАВНУЮ ДАТЕ ОТЧЕТА                        */
/* ========================================================================= */
PROCEDURE CLC_LOAN_OST_ON_DATE:

   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* Договора*/
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO. /* Дата отчета */
   DEF INPUT  PARAM iSince     AS DATE NO-UNDO. /* дата пересчета договора*/
   DEF INPUT  PARAM iLoanSince AS DATE NO-UNDO. /* loan.since */
   DEF OUTPUT PARAM oSumma0    AS DEC  NO-UNDO. /* Сумма долга */
   DEF OUTPUT PARAM oSumma13   AS DEC  NO-UNDO. /* Сумма до выяснения */

   DEF VAR vBegDate AS DATE NO-UNDO.
   DEF VAR vEndDate AS DATE NO-UNDO.
   DEF VAR vPar0    AS DEC  NO-UNDO.
   DEF VAR vPar13   AS DEC  NO-UNDO.
   DEF VAR vDb      AS DEC  NO-UNDO.
   DEF VAR vCr      AS DEC  NO-UNDO.
   DEF VAR vFirst   AS LOG  NO-UNDO.
   DEF VAR vFind    AS LOG  NO-UNDO.
   DEF VAR vDelta   AS DEC  NO-UNDO.

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER bterm-obl FOR term-obl.

   IF iSince = ? THEN
      iSince = iLoanSince.

   IF iDate <= iSince THEN RETURN.

   ASSIGN
      vFirst   = YES
      vFind    = NO
      vBegDate = iSince + 1
      .

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 3
        AND term-obl.end-date >= iSince
        AND term-obl.end-date <  iDate
   NO-LOCK:

      vEndDate = term-obl.end-date.

      IF vFirst THEN
      DO: /* На первом шаге расчитываем параметры с разноской*/
         RUN RE_PARAM_EX IN h_loan (0,               /* Код параметра   */
                                    vEndDate,        /* Дата расчета    */
                                    iLoanSince,
                                    iContract,       /* Назначение      */
                                    iContCode,       /* Номер договора  */
                                    OUTPUT oSumma0,  /* Сумма параметра */
                                    OUTPUT vDb,      /* Оборот ДБ */
                                    OUTPUT vCr).     /* Оборот Кр */

         RUN RE_PARAM_EX IN h_loan (13,              /* Код параметра   */
                                    vEndDate,        /* Дата расчета    */
                                    iLoanSince,
                                    iContract,       /* Назначение      */
                                    iContCode,       /* Номер договора  */
                                    OUTPUT oSumma13, /* Сумма параметра */
                                    OUTPUT vDb,      /* Оборот ДБ */
                                    OUTPUT vCr).     /* Оборот Кр */
      END.
      ELSE
      DO:
         RUN CORR_DOLG_LOAN_INT (iContract,
                                 iContCode,
                                 vBegDate,
                                 vEndDate,
                                 INPUT-OUTPUT oSumma0,
                                 INPUT-OUTPUT oSumma13).
      END.
      /* Теперь корректируем значение с учетом ПС */
      FIND FIRST bterm-obl WHERE
                 bterm-obl.contract  = iContract
             AND bterm-obl.cont-code = iContCode
             AND bterm-obl.idnt      = 2
             AND bterm-obl.end-date = term-obl.end-date
      NO-LOCK NO-ERROR.
      IF AVAIL bterm-obl THEN
      DO:
         vDelta = oSumma0 - bterm-obl.amt-rub.
         IF vDelta > 0
         THEN
            ASSIGN
               oSumma0  = oSumma0  - vDelta
               oSumma13 = oSumma13 + vDelta
               .
      END.

      ASSIGN
         vBegDate = term-obl.end-date + 1
         vFind    = YES
         vFirst   = NO
         .

   END.
   /* Если были какие-то ОпВ ...*/
   IF vFirst = NO THEN
      RUN CORR_DOLG_LOAN_INT (iContract,
                              iContCode,
                              vBegDate,
                              iDate,
                              INPUT-OUTPUT oSumma0,
                              INPUT-OUTPUT oSumma13).
   ELSE DO: /* Иначе просто шарашим параметры */
      RUN RE_PARAM_EX IN h_loan (0,               /* Код параметра   */
                                 iDate,           /* Дата расчета    */
                                 iLoanSince,      /* loan.since */
                                 iContract,       /* Назначение      */
                                 iContCode,       /* Номер договора  */
                                 OUTPUT oSumma0,  /* Сумма параметра */
                                 OUTPUT vDb,      /* Оборот ДБ */
                                 OUTPUT vCr).     /* Оборот Кр */

      RUN RE_PARAM_EX IN h_loan (13,              /* Код параметра   */
                                 iDate,           /* Дата расчета    */
                                 iLoanSince,      /* loan.since */
                                 iContract,       /* Назначение      */
                                 iContCode,       /* Номер договора  */
                                 OUTPUT oSumma13, /* Сумма параметра */
                                 OUTPUT vDb,      /* Оборот ДБ */
                                 OUTPUT vCr).     /* Оборот Кр */
   END.

END PROCEDURE.
/* ========================================================================= */
/* = КОРРЕКТИРОВКА СУММ ПАРАМЕТРОВ 0 И 13 С УЧЕТОМ ОПЕРАЦИЙ МЕЖДУ ОпВ ====== */
/* ========================================================================= */
PROCEDURE CORR_DOLG_LOAN_INT:

   DEF INPUT        PARAM iContract AS CHAR NO-UNDO. /*Идентификатор*/
   DEF INPUT        PARAM iContCode AS CHAR NO-UNDO. /*договора*/
   DEF INPUT        PARAM iBegDate  AS DATE NO-UNDO. /*дата пред ОпВ + 1*/
   DEF INPUT        PARAM iEndDate  AS DATE NO-UNDO. /*дата след ОпВ*/
   DEF INPUT-OUTPUT PARAM pSumma0   AS DEC  NO-UNDO. /* Сумма 0*/
   DEF INPUT-OUTPUT PARAM pSumma13  AS DEC  NO-UNDO. /* Сумма 13*/

   DEF VAR vBegDate AS DATE NO-UNDO.
   DEF VAR vEndDate AS DATE NO-UNDO.
   DEF VAR vFind    AS LOG  NO-UNDO.
   DEF VAR vDelta   AS DEC  NO-UNDO.

   DEF BUFFER loan-int  FOR loan-int.
   DEF BUFFER bloan-int FOR loan-int.

   vBegDate = iBegDate.
   /* делаем разбивку по операции погашения ссуды  */
   FOR EACH loan-int WHERE
            loan-int.contract  = iContract
        AND loan-int.cont-code = iContCode
        AND loan-int.id-d      = 1
        AND loan-int.id-k      = 2
        AND loan-int.mdate    >= iBegDate
        AND loan-int.mdate    <= iEndDate
   NO-LOCK:

      vEndDate = loan-int.mdate.
      /* все операции которые увеличивают/уменьшают параметры */
      FOR EACH bloan-int WHERE
               bloan-int.contract  = iContract
           AND bloan-int.cont-code = iContCode
           AND ( bloan-int.id-d      = 0 OR
                 bloan-int.id-k      = 0)
           AND bloan-int.mdate    >= vBegDate
           AND bloan-int.mdate    <= vEndDate
      NO-LOCK:
         pSumma0 = pSumma0
                   + (IF bloan-int.id-d = 0
                      THEN bloan-int.amt-rub
                      ELSE 0)
                   - (IF bloan-int.id-k = 0
                      THEN bloan-int.amt-rub
                      ELSE 0).
      END.

      FOR EACH bloan-int WHERE
               bloan-int.contract  = iContract
           AND bloan-int.cont-code = iContCode
           AND ( bloan-int.id-d      = 13 OR
                 bloan-int.id-k      = 13)
           AND bloan-int.mdate    >= vBegDate
           AND bloan-int.mdate    <= vEndDate
      NO-LOCK:
         pSumma13 = pSumma13
                   + (IF bloan-int.id-d = 13
                      THEN bloan-int.amt-rub
                      ELSE 0)
                   - (IF bloan-int.id-k = 13
                      THEN bloan-int.amt-rub
                      ELSE 0).
      END.
      /* А теперь разноска суммы погашения */
      ASSIGN
         vDelta   = loan-int.amt-rub - pSumma13
         vBegDate = loan-int.mdate + 1
         vFind    = YES
         .
      IF vDelta  > 0
      THEN
         ASSIGN
            pSumma0  = pSumma0 - vDelta
            pSumma13 = 0
            .
      ELSE
         ASSIGN
            pSumma13 = pSumma13 - loan-int.amt-rub.
   END.
   /* все операции которые увеличивают/уменьшают параметры
      все что осталось после последнего погашения */
   vEndDate = iEndDate.

   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND ( bloan-int.id-d      = 0 OR
              bloan-int.id-k      = 0)
        AND bloan-int.mdate    >= vBegDate
        AND bloan-int.mdate    <= vEndDate
   NO-LOCK:
      pSumma0 = pSumma0
                + (IF bloan-int.id-d = 0
                   THEN bloan-int.amt-rub
                   ELSE 0)
                - (IF bloan-int.id-k = 0
                   THEN bloan-int.amt-rub
                   ELSE 0).
   END.

   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND ( bloan-int.id-d      = 13 OR
              bloan-int.id-k      = 13)
        AND bloan-int.mdate    >= vBegDate
        AND bloan-int.mdate    <= vEndDate
   NO-LOCK:
      pSumma13 = pSumma13
                + (IF bloan-int.id-d = 13
                   THEN bloan-int.amt-rub
                   ELSE 0)
                - (IF bloan-int.id-k = 13
                   THEN bloan-int.amt-rub
                   ELSE 0).
   END.

END PROCEDURE.
/* ========================================================================= */
/* = ПОСТРОЕНИЕ СКОРРЕКТИРОВАННОГО ГРАФИКА ПОГАШЕНИЯ С УЧЕТОМ ПРОЛОНГАЦИИ == */
/* = И ПРОБЕГА ============================================================= */
PROCEDURE PREPARE_PLAN_LINE:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT  PARAM iDelay    AS DATE NO-UNDO.
   DEF INPUT  PARAM iProl     AS DATE NO-UNDO.
   DEF INPUT  PARAM iType     AS INT64  NO-UNDO.

   DEF VAR vSumma AS DEC NO-UNDO.

   DEF BUFFER loan FOR loan.

   RUN EMPTY_TABLES.

   CASE iType:
      WHEN 1 THEN
         RUN PREPARE_SPIS_DOLG (iContract,
                                iContCode,
                                iDate,
                                iDelay,
                                iProl,
                                NO,
                                OUTPUT vSumma).
      WHEN 2 THEN
      DO:
         RUN PREPARE_SPIS_DOLG (iContract,
                                iContCode,
                                iDate,
                                iDelay,
                                iProl,
                                NO,
                                OUTPUT vSumma).

         FOR EACH loan WHERE
                  loan.contract = iContract
              AND loan.cont-code BEGINS iContCode + " "
              AND loan.cont-code <> iContCode
         NO-LOCK:
            RUN PREPARE_SPIS_DOLG (iContract,
                                   loan.Cont-Code,
                                   iDate,
                                   iDelay,
                                   iProl,
                                   NO,
                                   OUTPUT vSumma).
         END.
      END.
   END CASE.

END PROCEDURE.


PROCEDURE GET_PLAN_DATE:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT  PARAM iLScope   AS DATE NO-UNDO.
   DEF INPUT  PARAM iRScope   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oExist    AS LOG  NO-UNDO.
   DEF OUTPUT PARAM oDate     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oPDate    AS DATE NO-UNDO.

   ASSIGN
      oExist = NO
      oDate  = ?
      oPDate = ?
      .

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iLScope
        AND ttSpis.end-date <= iRScope
   NO-LOCK BY ttSpis.end-date DESC:
      ASSIGN
         oExist = YES
         oDate  = ttSpis.end-date
         oPDate = ttSpis.old-date
         .
      RETURN.
   END.

END PROCEDURE.

PROCEDURE GET_SUMMA_POG:
   DEF INPUT  PARAM iLScope   AS DATE NO-UNDO.
   DEF INPUT  PARAM iRScope   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oSumma    AS DECIMAL NO-UNDO.

   FOR EACH ttSpis WHERE
      (iLScope =  ? AND ttSpis.end-date <= iRScope) OR
      (iRScope =  ? AND ttSpis.end-date >= iLScope) OR
      (iRScope <> ? AND iLScope <> ? AND
       ttSpis.end-date >= iLScope AND ttSpis.end-date <= iRScope)
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.
END PROCEDURE.

{pfuncdef 
   &DefProc="SUMM_PROC"
   &Description="процедура возвращает непогашенный остаток по всем предыдущим 
               обязательствам включая переданное"}
PROCEDURE SUMM_PROC:
    
    DEF PARAM  BUFFER loan FOR loan.
    DEF INPUT  PARAM  iDatePlat AS DATE NO-UNDO. /* дата учета оплаты */
    DEF INPUT  PARAM  iBegDate  AS DATE NO-UNDO. /* дата начала подсчета непогашенных обязательств, если ?, то подсчитываются все обязательства */
    DEF INPUT  PARAM  iEndDate  AS DATE NO-UNDO. /* дата окончания подсчета непогашенных обязательств */
    DEF OUTPUT PARAM  oSumm     AS DEC NO-UNDO. /* сумма непогашенных обязательств */
    
    DEF BUFFER term-obl FOR term-obl.
    DEF BUFFER loan-int FOR loan-int.
    DEF BUFFER chowhe FOR chowhe.
    DEF BUFFER term-obl-hist FOR term-obl-hist.
    DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.
    
   {summ-t1.p &SUMM_PROC=YES &PERC=YES}
    
END PROCEDURE.

{pfuncdef 
   &DefProc="SUMM_PROC2"
   &Description="В отличие от summ-proc.p в этом случае будем бежать по истории графиков"}
PROCEDURE SUMM_PROC2:
    
    DEF PARAM  BUFFER loan FOR loan.
    DEF INPUT  PARAM  iDatePlat AS DATE NO-UNDO. /* дата учета оплаты */
    DEF INPUT  PARAM  iBegDate  AS DATE NO-UNDO. /* дата начала подсчета непогашенных обязательств, если ?, то подсчитываются все обязательства */
    DEF INPUT  PARAM  iEndDate  AS DATE NO-UNDO. /* дата окончания подсчета непогашенных обязательств */
    DEF OUTPUT PARAM  oSumm     AS DEC NO-UNDO. /* сумма непогашенных обязательств */
    
    DEF BUFFER term-obl FOR term-obl.
    DEF BUFFER loan-int FOR loan-int.
    DEF BUFFER chowhe FOR chowhe.
    DEF BUFFER term-obl-hist FOR term-obl-hist.
    DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.
    
   {summ-t1.p &SUMM_PROC=YES &PERC=YES &SUMM_PROC_HIST=YES}
    
END PROCEDURE.

PROCEDURE PREPARE_SPIS_PROC:
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Иднентификатор        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета          */
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* Остаток по договору   */

   DEF VAR summ-t           AS DEC   NO-UNDO.
   DEF VAR fl1              AS LOG INIT NO.
   DEF VAR e1 AS DEC   NO-UNDO INIT 0.
   DEF VAR e2 AS DEC   NO-UNDO INIT 0.
   DEF VAR e3 AS DEC   NO-UNDO INIT 0.

   DEF VAR i         AS INT64  NO-UNDO.
   DEF VAR vSumm     AS DEC  NO-UNDO.
   DEF VAR vDbSumDec AS DEC  NO-UNDO.
   DEF VAR vCrSumDec AS DEC  NO-UNDO.
   DEF VAR vDateN    AS DATE NO-UNDO.
   DEF VAR mPayDate  AS DATE NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER xerm-obl       FOR term-obl.
   DEF BUFFER loan FOR loan.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
      AND     loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* Определяем даты пересчета и начального решения по Договорам */
      ASSIGN
         mPayDate = loan.since
         vDateN   = DATE(FGetSettingEx("ДатаНачКред", ?, "", NO))
         vDateN   = IF vDateN =  ? THEN DATE(1,1,1900) ELSE vDateN
      .

      FOR EACH term-obl USE-INDEX end-date WHERE
               term-obl.contract  = iContract
           AND term-obl.cont-code = iContCode
           AND term-obl.idnt      = 1
           AND term-obl.end-date >= iDate
      NO-LOCK:

         {summ-t1.i}

         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = term-obl.end-date
            ttSpis.summa    = summ-t
            .
      END.
   END.
END PROCEDURE.


/*mitr:
досрочноое погашение ссуды учитывается в конце срока

0045006: Кредиты. Необходим инструмент
Комментарий, созданный при переходе 'На разработку'  Савинова Т.В. (22/3/2005 11:52:16)
Класс данных i1p1_2 -Ф.135(110-И).Расшифровки 2
НЕобходимо сделать отдельную функцию для расчета данной расшифровки:
- на отчетную дату брать остаток на счете и от него (от более раннего к более позднему)рассматривать дальнейшие плановые погашения

Например:
задолжность                                 на 21/04     7000
плановые выплаты                                        1000
фактически  было выплачено   на 21.05   4500
на отчетную дату  факт.остаток на 31.05 2500
тогда должны рассматривать
на 21.06 1000
на 21.07  1000
на 21.08    500

*/
/*мутация от PREPARE_SPIS_DOLG*/
/* ========================================================================= */
/* ==== ПОСТРОЕНИЕ ПЛАНОВЫХ СПИСАНИЙ ОСНОВНОГО ДОЛГА ПО ДОГОВОРУ =========== */
/* ========================================================================= */
PROCEDURE PREPARE_SPIS_DOLG:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Иднентификатор        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* Учитывать ли пробег   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* Учитывать пролонгацию */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* Остаток по договору   */

   RUN PREPARE_SPIS_DOLG_common IN THIS-PROCEDURE (
                           iContract,
                           iContCode,
                           iDate,
                           iDelay,
                           iProl,
                           iRet,
                           FALSE,   /* обратное направл перебора значений в ttPlan*/
                           OUTPUT oRem
                           ).


END PROCEDURE.

/*вторая версия построения списания  */
PROCEDURE PREPARE_SPIS_DOLG2:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Иднентификатор        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* Учитывать ли пробег   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* Учитывать пролонгацию */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* Остаток по договору   */

   RUN PREPARE_SPIS_DOLG_common IN THIS-PROCEDURE (
                           iContract,
                           iContCode,
                           iDate,
                           iDelay,
                           iProl,
                           iRet,
                           TRUE,    /* прямое направл перебора значений в ttPlan*/
                           OUTPUT oRem
                           ).


END PROCEDURE.

PROCEDURE SUMM_SPIS_DOLG2:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* Идентификатор */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* Договора*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* Дата расчета */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* Начало периода */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* Окончание периода */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* Учет пробега */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* Учет пролонгации */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* Сумма списания резерва */

   DEF VAR vOstatok AS DEC NO-UNDO.

   RUN EMPTY_TABLES.

   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "Номер договора " iContCode " Дата отчета " iDate FORMAT "99/99/9999"  SKIP
      "Срок с " iBegDate FORMAT "99/99/9999"
      " по "
       iEndDate  FORMAT "99/99/9999" SKIP.
   OUTPUT CLOSE.
   &ENDIF

   RUN PREPARE_SPIS_DOLG2 (iContract,iContCode,iDate,iDelay,iProl,YES,OUTPUT vOstatok).

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iBegDate
        AND ttSpis.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.

END PROCEDURE.

PROCEDURE GET_SUMMA_DOLG :
   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /*  Номер Договора */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /*ДАТА НАЧАЛА */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* Дата конца */
   DEF INPUT PARAM iDelayFl  AS LOG  NO-UNDO. /* Учитывать ли пробег погашения
           ** основного долга. Т.е. если окончание платежного периода попадает
           ** в период расчета, то платеж учитывается в расчете. Параметр
           ** имеет приоритет над НП ПробегОплаты.ПробегОплСсуды. Если значение
           ** входного параметра равно ? (неопр.), то используется НП. */
   DEF OUTPUT PARAM oSummaDolg  AS  DECIMAL NO-UNDO.
   /* плановое погашение за период */
   DEF OUTPUT PARAM oSummaPr   AS  DECIMAL NO-UNDO.
   /* непогашеннй остаток за период */

   DEF VAR vDelay    AS LOGICAL NO-UNDO .
   DEF VAR vProl     AS LOGICAL NO-UNDO .
   DEF VAR vDelayl   AS INT64   NO-UNDO .
   DEF VAR vBegDate  AS DATE    NO-UNDO. /*ДАТА НАЧАЛА */
   DEF VAR vEndDate  AS DATE    NO-UNDO . /* Дата конца */
   DEF VAR oSummaPrL AS DECIMAL NO-UNDO.

   DEF BUFFER term-obl FOR term-obl .
   DEF BUFFER pro-obl  FOR pro-obl .

   RUN EMPTY_TABLES.

   /* Учитывать пробег погашения */
   ASSIGN
      vDelay = IF iDelayFl <> ? THEN iDelayFl ELSE (FGetSetting("ПробегОплаты","ПробегОплСсуды","Нет") = "Да")
      vProl  = FGetSetting("УчитПрол",?,"Нет") = "Да"
      .

   FIND LAST loan  WHERE loan.contract = iContract
      AND  loan.cont-code = iContCode
      NO-LOCK NO-ERROR .
   IF loan.cont-type = {&Sogl_Type}
      THEN RETURN .

   IF vDelay THEN 
   DO :
      FIND LAST loan-cond  WHERE loan-cond.contract = iContract
         AND  loan-cond.cont-code = iContCode
         AND  loan-cond.since <= iBegDate NO-LOCK NO-ERROR.

      IF AVAIL loan-cond THEN 
      DO :
         /* сдвигаем правую границу, чтоты учесть  в выборке обязательства,
            попадающие
           в заданный диапазон с учетом пробега, но сдвиг не должен превышать
           дату условий  Не очень правильно - до этих условий могли быть
           другие условия с большим пробегом */
         vBegDate = IF iBegDate - loan-cond.delay1 < loan-cond.since
            THEN loan-cond.since
            ELSE iBegDate - loan-cond.delay1 .
      END.
      ELSE vBegDate = iBegDate .
      /* запоминаем точку с первым значением пробега */
      CREATE ttSpis.
      ASSIGN 
         ttSpis.end-date = vBegDate
         ttSpis.summa    = IF AVAIL loan-cond
                                         THEN loan-cond.delay1
                                         ELSE 0 .
      vDelayl = ttSpis.summa .
      FOR EACH loan-cond WHERE loan-cond.contract = iContract
         AND   loan-cond.cont-code = iContCode
         AND   loan-cond.since >  iBegDate
         AND   loan-cond.since <= iEndDate  NO-LOCK :
         IF loan-cond.delay1  <> vDelayl
            THEN 
         DO:
            /* запоминаеи точку со следующим значением пробега */
            CREATE ttSpis.
            ASSIGN 
               ttSpis.end-date = loan-cond.since
               ttSpis.summa    = loan-cond.delay1
               vDelayl = ttSpis.summa
               .
         END.
      END.
   END.
   ELSE 
   DO :
      CREATE ttSpis.
      ASSIGN 
         ttSpis.end-date = iBegDate
         ttSpis.summa    = 0 .

   END.
   FIND FIRST  ttSpis NO-ERROR.
   DO WHILE AVAIL ttSpis :

      vDelayl  = ttSpis.summa.
      vBegDate = ttSpis.end-date.

      FIND NEXT ttSpis NO-ERROR .
      IF AVAIL ttSpis
         THEN ASSIGN vEndDate = ttSpis.end-date - 1. /* старый пробег
                         действует до даты нового -1 день*/
      ELSE vEndDate = iEndDate.
      cicl:
      FOR EACH term-obl WHERE
         term-obl.contract      = iContract
         AND term-obl.cont-code = iContCode
         AND term-obl.idnt      = 3
         AND term-obl.end-date  >= vBegDate
         AND term-obl.end-date  <= vEndDate
         NO-LOCK :

         IF term-obl.end-date + vDelayl < iBegDate
            OR
            (IF iEndDate <  loan.open-date
            THEN term-obl.end-date + vDelayl > iEndDate
         ELSE FALSE)
            THEN NEXT cicl .
         /* исключаем при необходимости обязательства
           пролонгированные на данный период их
           более ранних периодов */
         IF NOT vProl
            THEN 
         DO:
            FIND FIRST pro-obl WHERE
               pro-obl.contract   = iContract
               AND pro-obl.cont-code  = iContCode
               AND pro-obl.idnt       = term-obl.idnt
               AND pro-obl.new-nn     = term-obl.nn
               AND pro-obl.n-end-date = term-obl.end-date
               AND pro-obl.pr-date   <  term-obl.end-date
               NO-LOCK NO-ERROR .
            IF AVAIL pro-obl AND pro-obl.end-date < iBegDate
               THEN  NEXT cicl.
         END.
         RUN summ-t.p(OUTPUT oSummaPrL,iContract,iContCode,RECID(term-obl),iEndDate).
         ASSIGN
            oSummaDolg = oSummaDolg + term-obl.amt-rub
            oSummaPr   = oSummaPr + oSummaPrL
            .
      END.
      IF vEndDate  = iEndDate
         THEN LEAVE .
   END.
END PROCEDURE .

{pfuncdef 
&DefProc="GET_SUMMA_DOLG_RUB"
&Description="Сумма задолженности и оплаты в рублях"}
 
 /* Сумма задолженности и оплаты в рублях */
PROCEDURE GET_SUMMA_DOLG_RUB :
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* Дата начала */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* Дата окончания */
   DEF INPUT  PARAM iDelayFl     AS LOG  NO-UNDO. /* Учитывать ли пробег погашения
          ** основного долга. Т.е. если окончание платежного периода попадает
          ** в период расчета, то платеж учитывается в расчете. Параметр
          ** имеет приоритет над НП ПробегОплаты.ПробегОплСсуды. Если значение
          ** входного параметра равно ? (неопр.), то используется НП. */
   DEF OUTPUT PARAM oSummaDolg   AS DEC  NO-UNDO. /* Плановое погашение за период */
                                                  /* по графику (РЭ на дату платежа по графику) */
   DEF OUTPUT PARAM oSummaPr     AS DEC  NO-UNDO. /* Непогашеннй остаток за период */
                                                  /* (в валюте договора) */
   DEF OUTPUT PARAM oSummaOplRub AS DEC  NO-UNDO. /* Сумма погашения в нац.вал. */
                                                  /* (РЭ на дату платежа) */

   DEF VAR vDelay    AS LOG  NO-UNDO.  /* Учитывать пробег погашения ОД */
   DEF VAR vProl     AS LOG  NO-UNDO.  /* Учитывать пролонгацию */
   DEF VAR vDelayl   AS INT64  NO-UNDO.  /* Пробег */
   DEF VAR vBegDate  AS DATE NO-UNDO.  /* Дата начала */
   DEF VAR vEndDate  AS DATE NO-UNDO.  /* Дата окончания */
   DEF VAR vSummaDolgN AS DEC  NO-UNDO.   /* Непог.ост. платежа на начало периода*/
   DEF VAR vSummaPrL AS DEC  NO-UNDO.  /* Непог.ост. платежа на конец периода */
   DEF VAR vSmDolgR  AS DEC  NO-UNDO.  /* Сумма планового погаш. в руб. */
   DEF VAR vSummaOpl AS DEC NO-UNDO. /* сумма оплаты в валюте договора */
   DEF VAR vIdDList  AS CHAR NO-UNDO INIT "1,5".
   DEF VAR vIdKList  AS CHAR NO-UNDO INIT "2,7".
   DEF VAR vCount    AS INT64  NO-UNDO.  /* Счетчик */

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER pro-obl   FOR pro-obl.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-cond FOR loan-cond.
   DEF BUFFER loan-int  FOR loan-int.

   RUN EMPTY_TABLES IN THIS-PROCEDURE.

   FIND LAST loan WHERE
             loan.contract  =  iContract
      AND    loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
      /* Если тип договора не "Течение" */
   IF     AVAIL loan
      AND loan.cont-type <> {&SOGL_TYPE} THEN
   DO:
         /* Учитывать пробег погашения, пролонгацию */
      ASSIGN
         vDelay   = IF iDelayFl <> ? THEN iDelayFl ELSE (FGetSetting("ПробегОплаты", 
                                                                     "ПробегОплСсуды", 
                                                                     "Нет") =  "Да")
         vProl    = FGetSetting("УчитПрол", ?, "Нет") =  "Да"
         vBegDate = iBegDate
      .
         /* Если учитываем пробег погашения ОД */
      IF vDelay THEN
      DO:
            /* сдвигаем правую границу, чтобы учесть в выборке обязательства, попадающие в
            ** заданный диапазон с учетом пробега, но сдвиг не должен превышать дату условий.
            ** Не очень правильно - до этих условий могли быть другие условия с большим пробегом */
         FIND LAST loan-cond  WHERE
                   loan-cond.contract  =  iContract
            AND    loan-cond.cont-code =  iContCode
            AND    loan-cond.since     <= iBegDate
         NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            vBegDate = IF (iBegDate - loan-cond.delay1) <  loan-cond.since
                          THEN loan-cond.since
                          ELSE iBegDate - loan-cond.delay1.
            /* Запоминаем точку с первым значением пробега */
         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = vBegDate
            ttSpis.summa    = IF AVAIL loan-cond THEN loan-cond.delay1 ELSE 0
            vDelayl         = IF AVAIL loan-cond THEN loan-cond.delay1 ELSE 0
         .
            /* Запоминаем точки со следующими отличными значениеми пробега */
         FOR EACH loan-cond WHERE
                  loan-cond.contract  =  iContract
            AND   loan-cond.cont-code =  iContCode
            AND   loan-cond.since     >  iBegDate
            AND   loan-cond.since     <= iEndDate
         NO-LOCK:
            IF loan-cond.delay1 <> vDelayl THEN
            DO:
               CREATE ttSpis.
               ASSIGN
                  ttSpis.end-date = loan-cond.since
                  ttSpis.summa    = loan-cond.delay1
                  vDelayl         = loan-cond.delay1
               .
            END.
         END.
      END.
         /* Без учета пробега погашения ОД */
      ELSE
      DO:
         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = iBegDate
            ttSpis.summa    = 0
         .
      END.

         /* Идем по точкам... */
      FIND FIRST ttSpis NO-ERROR.
      BLK:
      DO WHILE AVAIL ttSpis:
            /* Определим период и пробег */
         ASSIGN
            vDelayl  = ttSpis.summa
            vBegDate = ttSpis.end-date
            vEndDate = iEndDate
         .
            /* Старый пробег действует до даты нового -1 день*/
         FIND NEXT ttSpis NO-ERROR.
         IF AVAIL ttSpis THEN
            vEndDate = ttSpis.end-date - 1.

            /* Теперь идем по обязательствам, попадающим в период */
         CICL:
         FOR EACH term-obl WHERE
                  term-obl.contract  =  iContract
            AND   term-obl.cont-code =  iContCode
            AND   term-obl.idnt      =  3
            AND   term-obl.end-date  >= vBegDate
            AND   term-obl.end-date  <= vEndDate
         NO-LOCK:
               /* Исключаем обязательства меньшие даты начала */
            IF (term-obl.end-date + vDelayl) <  iBegDate THEN
               NEXT CICL.
               /* Это как понимать?  */
            IF      iEndDate <  loan.open-date
               OR (term-obl.end-date + vDelayl) >  iEndDate THEN
               NEXT CICL.

               /* Исключаем при необходимости обязательства пролонгированные
               ** на данный период из более ранних периодов */
            IF NOT vProl THEN
            DO:
               FIND FIRST pro-obl WHERE
                          pro-obl.contract   =  iContract
                  AND     pro-obl.cont-code  =  iContCode
                  AND     pro-obl.idnt       =  term-obl.idnt
                  AND     pro-obl.new-nn     =  term-obl.nn
                  AND     pro-obl.n-end-date =  term-obl.end-date
                  AND     pro-obl.pr-date    <  term-obl.end-date
               NO-LOCK NO-ERROR.
               IF     AVAIL pro-obl
                  AND pro-obl.end-date <  iBegDate THEN
                  NEXT CICL.
            END.
            /* Проверяем был ли погашен платеж строго до отчетного периода */
            RUN SetSysConf IN h_base ("РежимОтсрПлатежа","да").
            RUN summ-t.p (OUTPUT vSummaDolgN,
                          iContract,
                          iContCode,
                          RECID(term-obl),
                          MAX(iBegDate - 1,loan.open-date)).
            RUN DeleteOldDataProtocol IN h_base ("РежимОтсрПлатежа").
            /* Если платеж досрочно погашен до отчетного периода, то пропускаем его */
            IF vSummaDolgN =  0 THEN NEXT CICL.
            /* Считаем сколько осталось непогашенным на конец периода */
            RUN SetSysConf IN h_base ("РежимОтсрПлатежа","да").
            RUN summ-t.p (OUTPUT vSummaPrL,
                          iContract,
                          iContCode,
                          RECID(term-obl),
                          iEndDate).
            RUN DeleteOldDataProtocol IN h_base ("РежимОтсрПлатежа").
            ASSIGN
               oSummaDolg = oSummaDolg + term-obl.amt-rub
               oSummaPr   = oSummaPr   + vSummaPrL    /* Непогашеннй остаток за период */
               vSmDolgR   = vSmDolgR   + CurToBase("Учетный",
                                                   term-obl.currency,
                                                   term-obl.end-date,
                                                   vSummaDolgN)
            .
               /* Зачем ? Это где-то используется ? */
            CREATE ttTerm.
            ttTerm.end-date = term-obl.end-date.
         END.
            /* Можно выходить */
         IF vEndDate =  iEndDate THEN
            LEAVE BLK.
      END.
         /* Определим сумму погашения в рублях в отчетном периоде*/
      DO vCount = 1 TO NUM-ENTRIES(vIdDList):
          /* Перебор всех операций (5 или 50), в отчетный период. */
         FOR EACH loan-int OF loan WHERE
                  loan-int.id-d  =  INT64(ENTRY(vCount, vIdDList))
            AND   loan-int.id-k  =  INT64(ENTRY(vCount, vIdKList))
            AND   loan-int.mdate >= iBegDate
            AND   loan-int.mdate <= iEndDate
         NO-LOCK:
            ASSIGN
               vSummaOpl    = vSummaOpl + loan-int.amt-rub
               oSummaOplRub = oSummaOplRub + CurToBase("Учетный",
                                                       loan.currency,
                                                       loan-int.mdate,
                                                       loan-int.amt-rub).
         END.
      END.
         /* Если реальное погашение больше суммы плановых гашений,
         ** считаем, что они относятся к другому периоду (0103005) */
      oSummaOplRub = IF oSummaOplRub >  vSmDolgR
                        THEN vSmDolgR
                        ELSE oSummaOplRub.
      /* если вся сумма не погашена, то РЭ на дату окончания периода */
      IF loan.currency <> "" THEN   
         IF oSummaPr =  oSummaDolg THEN
            oSummaDolg = CurToBase("Учетный",
                                   loan.currency,
                                   iEndDate,
                                   oSummaDolg). 
         ELSE
         DO:
            oSummaDolg = oSummaOplRub + CurToBase("Учетный",
                                                  loan.currency,
                                                  iEndDate,
                                                  oSummaDolg - vSummaOpl).
         END.
   END.
END PROCEDURE .


{pfuncdef 
&DefProc="GET_SUMMA_DOLG_RUB_NEW"
&Description="Сумма задолженности и оплаты в рублях"}

   /* Сумма задолженности и оплаты в рублях */
PROCEDURE GET_SUMMA_DOLG_RUB_NEW:
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* Дата начала */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* Дата окончания */
   DEF OUTPUT PARAM oSummaDolg   AS DEC  NO-UNDO. /* Плановое погашение за период по графику */ 
                                                  /* (РЭ на дату платежа по графику) */
   DEF OUTPUT PARAM oSummaPr     AS DEC  NO-UNDO. /* Непогашеннй остаток за период */
                                                  /* (в валюте договора) */

   DEF VAR vIdDList     AS CHAR  NO-UNDO INIT "1,5". /* Список параметров по дебету */
   DEF VAR vIdKList     AS CHAR  NO-UNDO INIT "2,7". /* Список параметров по кредиту */
   DEF VAR vCount       AS INT64 NO-UNDO.  /* Счетчик */
   DEF VAR vListParam   AS CHAR  NO-UNDO INIT "0,7,13,47". /* Список параметров остатка */
   DEF VAR vOst         AS DEC   NO-UNDO.  /* Остатки по параметрам 0,7,13,47 */
   DEF VAR vRazn        AS DEC   NO-UNDO.  /* Разнесенная сумма*/
   DEF VAR vNeUch       AS DEC   NO-UNDO.  /* Сумма неучтенных платежей */
   DEF VAR vNeUchR      AS DEC   NO-UNDO.  /* Сумма неучтенных платежей в руб.*/
   DEF VAR vSumm        AS DEC   NO-UNDO.  /* переплата/задолженность (переплата +, задолж. -) */
   DEF VAR vDate        AS DATE  NO-UNDO.
   DEF VAR vDb          AS DEC   NO-UNDO.
   DEF VAR vCr          AS DEC   NO-UNDO.
   DEF VAR vBegDate     AS DATE  NO-UNDO. /*Скорректированная дата периода*/
   DEF VAR vRealDolg    AS DEC   NO-UNDO. /*Фактическая сумму задолженности*/

   DEF BUFFER loan           FOR loan.
   DEF BUFFER term-obl       FOR term-obl.
   DEF BUFFER loan-int       FOR loan-int.

   MAIN:
   DO:
      FIND LAST loan WHERE
                loan.contract  =  iContract
         AND    loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.

         /* Если тип договора не "Течение" */
      IF AVAIL loan AND 
               loan.cont-type <> {&SOGL_TYPE} THEN
      DO:
         /* Определение переплаты/задолженности на начало периода */
         FIND LAST term-obl  WHERE
                   term-obl.contract  =  loan.Contract
               AND term-obl.cont-code =  loan.Cont-Code
               AND term-obl.idnt      =  2
               AND term-obl.end-date  <= iBegDate - 1
         NO-LOCK NO-ERROR.

         IF AVAIL term-obl THEN
         DO:         
            vSumm = term-obl.amt-rub.
         END. 

         DO vCount = 1 TO NUM-ENTRIES(vListParam):
            RUN RE_PARAM_EX IN h_Loan (INT64(ENTRY(vCount,vListParam)),
                                       iBegDate - 1,
                                       loan.since,
                                       loan.contract,
                                       loan.cont-code,
                                       OUTPUT vOst,
                                       OUTPUT vDb,
                                       OUTPUT vCr ).
            ASSIGN
               vSumm     = vSumm - vOst
               vRealDolg = vRealDolg + vOst
            .
            
         END.

         /*Последнее обязательство до начала периода*/
         FIND LAST term-obl WHERE
                   term-obl.contract  =  iContract
             AND   term-obl.cont-code =  iContCode
             AND   term-obl.idnt      =  3
             AND   term-obl.end-date  <  iBegDate
         NO-LOCK NO-ERROR.

         IF AVAIL term-obl THEN
            vBegDate = term-obl.end-date.
         ELSE 
            vBegDate = iBegDate.

        /* Теперь идем по обязательствам, попадающим в период */
         CICL:
         FOR EACH term-obl WHERE
                  term-obl.contract  =  iContract
            AND   term-obl.cont-code =  iContCode
            AND   term-obl.idnt      =  3
            AND   term-obl.end-date  >= vBegDate
         NO-LOCK:

            IF term-obl.dsc-beg-date >  iEndDate THEN
               LEAVE CICL.

            IF term-obl.dsc-beg-date <  iBegDate THEN
               NEXT CICL.

            IF term-obl.sop-date <> ? THEN
               IF (term-obl.sop-date <= iBegDate) THEN 
                  NEXT CICL.
            
            IF vSumm >= term-obl.amt-rub THEN
            DO:
               vSumm = vSumm - term-obl.amt-rub.
               NEXT CICL.   
            END.  

               vNeUch  = vNeUch  + term-obl.amt-rub.
         END.
 
         vNeUch = MIN(vNeUch, vRealDolg).
      
          /* Если обязательств нет - выходим */
         IF vNeUch <= 0 THEN
            LEAVE MAIN. 

         /* Учет переплаты/задолженности */
         ASSIGN
            vDate      = iBegDate
            vRazn      = min(max(vSumm, 0), vNeUch)
            vNeUch     = vNeUch - vRazn
            vSumm      = vSumm - vRazn
         .

         IF FGetSetting("Форма115П","УчПред","") =  "ПОЛН" THEN
            oSummaDolg = oSummaDolg + CurToBase("Учетный",
                                                loan.currency,
                                                iBegDate, 
                                                vRazn).
         IF vNeUch <= 0 THEN
            LEAVE MAIN. 

         DO WHILE vDate <= iEndDate:
             /* Определим сумму погашения в рублях в отчетном периоде */
            DO vCount = 1 TO NUM-ENTRIES(vIdDList):
                /* Перебор всех операций (5 или 50), в отчетный период. */
               Opl:
               FOR EACH loan-int OF loan WHERE
                        loan-int.id-d  =  INT64(ENTRY(vCount, vIdDList))
                    AND loan-int.id-k  =  INT64(ENTRY(vCount, vIdKList))
                    AND loan-int.mdate =  vDate
               NO-LOCK:
                  ASSIGN
                     vSumm      = vSumm + loan-int.amt-rub
                     vRazn      = min(max(vSumm, 0), vNeUch)
                     vNeUch     = vNeUch - vRazn
                     vSumm      = vSumm - vRazn
                     oSummaDolg = oSummaDolg + CurToBase("Учетный",
                                                         loan.currency,
                                                         loan-int.mdate, 
                                                         vRazn)
                  .

                  IF vNeUch <= 0 THEN
                     LEAVE MAIN. 

               END.
            END.
            vDate = vDate + 1.
         END.

         ASSIGN
            vNeUchR = CurToBase("Учетный",
                               loan.currency,
                               iEndDate, 
                               vNeUch) 
            oSummaDolg = oSummaDolg + vNeUchR
            oSummaPr   = vNeUchR
         .

      END.
   END. /* MAIN */
END PROCEDURE.

/* Отбирает по счету договоры, к которым привязан счет на дату,
** и складывает во временную таблицу contract,cont-code договоров
** и роль, с которой привязанн счет */
PROCEDURE LoanFromAcct.
   DEF INPUT  PARAM iAcct  AS CHAR   NO-UNDO.   /* номер счета */
   DEF INPUT  PARAM iCurr  AS CHAR   NO-UNDO.   /* валюта счета */
   DEF INPUT  PARAM iSince AS DATE   NO-UNDO.   /* дата */
   DEF OUTPUT PARAM oAvail AS LOG    NO-UNDO.   /* признак наличия хоть одной записи */
   DEFINE OUTPUT PARAMETER TABLE FOR tt-oLoanAcct.

   DEF BUFFER b1-lacct FOR loan-acct. /* Локализация буфера. */

   {empty tt-oLoanAcct}

   FOR EACH loan-acct WHERE 
            loan-acct.acct  =  iAcct
        AND loan-acct.curr  =  iCurr
        AND loan-acct.since <= iSince 
        AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.acct       =  loan-acct.acct
                                           AND b1-lacct.curr       =  loan-acct.curr
                                           AND b1-lacct.contract   =  loan-acct.contract
                                           AND b1-lacct.cont-code  =  loan-acct.cont-code
                                           AND b1-lacct.acct-type  =  loan-acct.acct-type
                                           AND b1-lacct.since      <= iSince
                                           AND b1-lacct.since      >  loan-acct.since)
      NO-LOCK:
      CREATE tt-oLoanAcct.
      ASSIGN
         tt-oLoanAcct.contract  = loan-acct.contract
         tt-oLoanAcct.cont-code = loan-acct.cont-code
         tt-oLoanAcct.acct-type = loan-acct.acct-type
         tt-oLoanAcct.since     = loan-acct.since
      .
   END.
   oAvail = AVAILABLE tt-oLoanAcct.

   RETURN.
END PROCEDURE.

/* процедура, определяющая задолженность по параметрам в зависимости от роли счета */
PROCEDURE GetSummDolg.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM oSummDolg AS DEC    NO-UNDO. /* Сумма задолженности с разноской по срокам */
   DEF OUTPUT PARAM oAllDolg  AS DEC    NO-UNDO. /* общая сумма задолженности без разноски по срокам */
   DEF OUTPUT PARAM oByTerm   AS LOG    NO-UNDO. /* Расчет с учетом разноски по срокам или без */

   DEF VAR vTmpSumm   AS DEC    NO-UNDO.
   DEF VAR vTmpCurr   AS CHAR   NO-UNDO.
   DEF VAR vPrmList   AS CHAR   NO-UNDO INIT "Кредит,КредПр,КредН,КредЛин,КредТ,КредТПр,КредПр%,КредПр%1,КредБудКом,КредБудПени,КредДискДн,КредДискПр,КредРез,КредРез1,КредРезВб,КредРезП,КредРезПр,КредРезКом,КредРезПени,КредРезД,КредРезДПр,КредПроц,КредВГар". 
   DEF VAR vPrmListP  AS CHAR   NO-UNDO.
   DEF VAR vPrmListM  AS CHAR   NO-UNDO.
   DEF VAR vPrm       AS CHAR   NO-UNDO.
   DEF VAR vi         AS INT64  NO-UNDO.
   DEF VAR vSumm      AS DEC    NO-UNDO.
   DEF VAR vDb        AS DEC    NO-UNDO.
   DEF VAR vCr        AS DEC    NO-UNDO.
   DEF VAR vCodOstpar AS INT64  NO-UNDO.
   DEF VAR vCodeName  AS CHAR   NO-UNDO.

   DEF BUFFER code     FOR code.
   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.

   MAIN_BLOCK:
   DO:
      IF NOT CAN-DO (vPrmList, iAcctType) THEN
         LEAVE MAIN_BLOCK.
      vCodeName  = FGetSetting("СпрРезервДог",?,"РезервДог") .
      /* определение списка параметров по роли счета */
      IF GetCodeBuff(vCodeName, iAcctType, BUFFER code) THEN
         ASSIGN
            vPrmListP = code.misc[1]
            vPrmListM = code.misc[2]
            vPrmList  = vPrmListP + "," + vPrmListM
         .
      ELSE
      DO:
         FIND FIRST code WHERE code.class =  vCodeName
                           AND code.name  =  iAcctType
         NO-LOCK NO-ERROR.
         IF AVAILABLE (code) THEN
            vPrmList = code.misc[3].
         ELSE
         DO:
            &IF DEFINED(PqresLog) <> 0 &THEN
            OUTPUT TO "pqres.log" APPEND.
            PUT UNFORMATTED
               "PROCEDURE GetSummDolg: " SKIP
               "Роль " iAcctType " не найдена в классификаторе " vCodeName SKIP.
            OUTPUT CLOSE.
            &ENDIF
            LEAVE MAIN_BLOCK.
         END.
      END.
      RUN RE_B_LOAN(iContract,iContCode,BUFFER loan).
      vCodOstpar = GetParCode(loan.class-code,'КодОснДолг').
           /* вычисляем общую сумму задолженности */
      DO vi = 1 TO NUM-ENTRIES (vPrmList):
         vPrm = ENTRY(vi,vPrmList).
            /* исключим пустышки */
         IF vPrm =  "" THEN
            NEXT.
         RUN Get_Param_Cur IN h_loan(INT64(vPrm),
                                     vCodOstpar,
                                     iContract,
                                     iContCode,
                                     iSince,
                                     "",
                                     OUTPUT vTmpCurr,
                                     OUTPUT vTmpSumm,        /* сумма в валюте */
                                     OUTPUT vSumm).          /* сумма в рублях */
            /* параметры, которые нужно вычитать возвращаются уже отрицательными
            ** поэтому всё складываем */
         oAllDolg = oAllDolg + vSumm.      /* рубли */
      END.
      /* перевод из рублей в указанную валюту (если валюта - не рубли) */
      IF iCurrency <> "" THEN
         oAllDolg = CurToCurWork ("УЧЕТНЫЙ","",iCurrency,iSince,oAllDolg).

               /* Нужна ли разноска по срокам */
      IF     iBegDate <> ?
         AND iEndDate <> ?
      THEN DO:
         IF NOT AVAIL loan THEN LEAVE MAIN_BLOCK.
                  /* Срочная задолженность (валюта договора) */
         IF iAcctType =  "Кредит"
         THEN DO:
            RUN SUMM_SPIS_DOLG(iContract,
                               iContCode,
                               iSince,
                               iBegDate,
                               iEndDate,
                               mDelay,
                               YES,
                               OUTPUT oSummDolg).
            /* перевод из валюты договора в указанную валюту (если они не совпадают) */
            IF iCurrency <> loan.currency THEN
               oSummDolg = CurToCurWork ("УЧЕТНЫЙ",loan.currency,iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
                  /* Задолженность по процентам (валюта договора) */
         ELSE IF iAcctType =  "КредТ" OR iAcctType =  "КредТПр"
         THEN DO:
            RUN sumsrok-pr-loan (RECID(loan),
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 YES,
                                 1,
                                 OUTPUT vTmpSumm,
                                 OUTPUT oSummDolg).
            /* перевод из валюты договора в указанную валюту (если они не совпадают) */
            IF iCurrency <> loan.currency THEN
               oSummDolg = CurToCurWork ("УЧЕТНЫЙ",loan.currency,iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
                  /* Для просроченных задолженностей (по ссуде, процентам и т.п.) */
         ELSE IF CAN-DO ("КредПр,КредПр%,КредБудКом,КредБудПени,КредДискПр,КредПр%1",iAcctType) THEN
         DO:
            DO vi = 1 TO NUM-ENTRIES (vPrmList):
               RUN SUMM_BY_PRS_CUR (ENTRY(vi,vPrmList),
                                    iSince,
                                    iContract,
                                    iContCode,
                                    iBegDate,
                                    iEndDate,
                                    NO,
                                    OUTPUT vTmpCurr,
                                    OUTPUT vTmpSumm,  /* сумма в валюте параметра */
                                    OUTPUT vSumm).    /* сумма в рублях */
               oSummDolg = oSummDolg + vSumm.      /* рубли */
            END.
            /* перевод из рублей в указанную валюту (если валюта - не рубли) */
            IF iCurrency <> "" THEN
               oSummDolg = CurToCurWork ("УЧЕТНЫЙ","",iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
         /* Учет выданных гарантий */
         ELSE IF iAcctType =  "КредВГар"
         THEN DO:
            IF loan.cont-type = {&Sogl_Type}  THEN
               oSummDolg = 0.           
            ELSE
            DO:
               FOR EACH term-obl WHERE term-obl.contract  =  iContract
                                   AND term-obl.cont-code =  iContCode 
                                   AND term-obl.end-date  >= iBegDate
                                   AND term-obl.end-date  <= iEndDate
                                   AND term-obl.idnt      =  3 
               NO-LOCK:
                  oSummDolg = oSummDolg + term-obl.amt-rub.
               END.
               /* перевод из валюты договора в указанную валюту (если они не совпадают) */
               IF iCurrency <> loan.currency THEN
                  oSummDolg = CurToCurWork ("УЧЕТНЫЙ",loan.currency,iCurrency,iSince,oSummDolg).

               oByTerm = YES.
            END.
         END.
         ELSE oSummDolg = oAllDolg. 
      END.
      ELSE oSummDolg = oAllDolg.
        
      IF     iAcctType       =  "КредВГар"
         AND loan.class-code =  "loan-guarantee" 
         AND NOT oByTerm 
         THEN oByTerm = NO.      
      ELSE
      IF iBegDate <> ?
         AND iEndDate <> ? THEN
         oByTerm = YES.
   END.

   RETURN.
END PROCEDURE.


/* Сформированный резерв по роли счета с учетом сроков */
PROCEDURE SUMM_SPIS_RES_TYPE.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iFromTr   AS LOG    NO-UNDO.   /*НЕ ИСПОЛЬЗУЕТСЯ. Ранее было: для траншей: брать резерв с транша (YES) или охват. дог. (NO) */
   DEF OUTPUT PARAM oSummRsrv AS DEC    NO-UNDO.

   DEF VAR vPrmList       AS CHAR   NO-UNDO.
   DEF VAR vTrRoleCorr    AS CHAR   NO-UNDO INIT "КредРез;Кредит;КредРез1;КредПр;
                                                  КредРезВб;КредН,КредЛин,КредВГар;
                                                  КредРезП;КредТ;КредТПр;КредРезПр;
                                                  КредПр%;КредПр%1;КредРезКом;
                                                  КредБудКом;кредРезПени;КредБудПени;
                                                  КредРезД;КредДискДН;КредРезДПр;
                                                  КредДискПр;КредАккрВ;КредАккрГВ".
   DEF VAR vi             AS INT64  NO-UNDO.
   DEF VAR vSumm          AS DEC    NO-UNDO.
   DEF VAR vTmpSumm       AS DEC    NO-UNDO.
   DEF VAR vCurr          AS CHAR   NO-UNDO.
   DEF VAR vOsnRoleList   AS CHAR   NO-UNDO.   /* роли счетов для расчета основного долга */
   DEF VAR vBasaOst       AS DEC    NO-UNDO.   /* база для расчета резерва по данному траншу */
   DEF VAR vOstParamRes   AS DEC    NO-UNDO.   /* начисленный резерв на охват. договоре */
   DEF VAR vAllBasaOst    AS DEC    NO-UNDO.   /* база для расчета резерва по всем траншам */
   DEF VAR vSummDolg      AS DEC    NO-UNDO.   /* сумма задолженности */
   DEF VAR vAllSummDolg   AS DEC    NO-UNDO.   /* сумма задолженности без разнесения по срокам */
   DEF VAR vTranshCount   AS INT64  NO-UNDO.
   DEF VAR vRate          AS DEC    NO-UNDO.
   DEF VAR vByTerm        AS LOG    NO-UNDO.
   DEF VAR vCalcThisLoan  AS LOG    NO-UNDO. /*Получаем резерв по переданному договору*/
   DEF VAR vCalcMainLoan  AS LOG    NO-UNDO. /*Получить резерв по охватывающему договору*/
   DEF VAR vTypeAcctRes   AS CHAR   NO-UNDO.    /* роль счета резерва */
   DEF VAR vSummRsrvOst   AS DEC    NO-UNDO.
   DEF VAR vj             AS INT64  NO-UNDO.
   DEF VAR vTranshCls     AS CHAR   NO-UNDO.  /* классы траншей */
   DEF VAR vOpList        AS CHAR   NO-UNDO.  /* операции из СписОпер */
   DEF VAR vOk            AS LOG    NO-UNDO INIT FALSE.
   DEF VAR vSummRsrvRub   AS DEC    NO-UNDO.
   DEF VAR vCodeName      AS CHAR   NO-UNDO.

   DEF BUFFER loan  FOR loan. /* Локализация буфера. */
   DEF BUFFER bloan FOR loan. /* Локализация буфера. */
   DEF BUFFER code  FOR code. /* Локализация буфера. */
   DEF BUFFER loan-acct FOR loan-acct .

   MAIN_BLOCK:
   DO:
      vCodeName  = FGetSetting("СпрРезервДог",?,"РезервДог") .
            /* определение списка параметров по роли счета */
      IF GetCodeBuff("РезервДог", iAcctType, BUFFER code) THEN
         ASSIGN
            vPrmList = code.misc[3]
            vTypeAcctRes = code.name.
      ELSE
      DO:
         FIND FIRST code WHERE code.class =  vCodeName
                           AND code.name  =  iAcctType
         NO-LOCK NO-ERROR.
         IF AVAILABLE (code) THEN
            ASSIGN
               vPrmList = code.misc[3]
               vTypeAcctRes = code.name.
         ELSE
            LEAVE MAIN_BLOCK.
      END.

      IF mPutProt THEN
      DO:
         OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
         PUT STREAM sOut UNFORMATTED
            'ИНФОРМАЦИЯ: Используемые параметры из классификатора' vCodeName
            ' для расчета резерва' SKIP 
            '   Договор: '            TRIM(iContCode) SKIP
            '   Список параметров: '  TRIM(vPrmList) SKIP
            '   Роль счета резерва: ' TRIM(vTypeAcctRes)
         SKIP.
         OUTPUT STREAM sOut CLOSE.
      END.

      RUN RE_B_LOAN(iContract,iContCode,BUFFER loan).

      /* Выделяем договоры по которым некорректен более глубокий анализ  */
      /* СписОпер, ТипСчДог, rel_type                                    */
      /* в случае: alt-contract BEGINS "mm" - договоры ден. рынка,       */
      /* операции (резерв) ведутся на част.суммах                        */
      /* Ниже соотвествующая ветка в расчете vCalcThisLoan,vCalcMainLoan */

      RUN CHECK_CHOWHE_TRANSH (iContract,
                               iContCode,
                               iAcctType,
                               OUTPUT vOk).

      IF    (NUM-ENTRIES(iContCode," ") >  1 AND NOT vOk)
         OR (loan.cont-type = {&Sogl_Type} AND vOk) THEN 
      DO: 
         oSummRsrv = 0.
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               'ОШИБКА: ' + (IF (loan.cont-type = {&Sogl_Type} AND vOk) THEN
                                'Охватывающий договор. Резерв ведется на траншах.'
                             ELSE
                                'Траншевый договор. Резерв ведется на охватывающем договоре.')  SKIP
               '   Договор: '       TRIM(iContCode) SKIP
               '   Роль: '          TRIM(iAcctType)
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
         RETURN.
      END.

       /* Если договор соглашение и резерв ведется на траншах - возвращаем ноль.
          Если договор транш и резерв ведется на соглашении, то сумму сформ. резерва берем с соглашения
       */

      /* Если договор является траншем */
      IF NUM-ENTRIES(iContCode," ") =  2
          AND CAN-FIND (FIRST bloan WHERE bloan.contract  =  iContract
                                AND bloan.cont-code =  ENTRY(1,iContCode," ")
                       NO-LOCK) THEN
         IF LnRsrvCheckType (iContract,iContCode,vTypeAcctRes) THEN /* и резерв ведется на этом транше */
         DO: 
            vCalcThisLoan = YES. /*будем считать сформ. резерв по траншу (т.е. по переданному договору)*/
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Договор является траншем и резерв ведется на транше. Расчет резерва по траншу' SKIP 
                  '   Договор: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
         ELSE
         DO:
            vCalcMainLoan = YES. /*иначе будем считать резерв по соглашению (охватывающему договору)*/
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Договор является траншем, а резерв ведется на охватывающем договоре. Расчет резерва по охватывающему договору' SKIP  
                  '   Договор: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
      ELSE /*переданный договор является обычным или соглашением*/
         IF loan.cont-type = {&Sogl_Type} THEN 
         DO:
            IF LnRsrvCheckType (iContract,iContCode,vTypeAcctRes) THEN  /*резерв ведется на нем, а не на его траншах*/
            DO:
               vCalcThisLoan = YES. /*будем считать резерв по переданному договору*/
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     'ИНФОРМАЦИЯ: Договор является охватывающим и резерв ведется на охватывающем договоре. Расчет резерва по охватывающему договору' SKIP  
                     '   Договор: '  TRIM(iContCode)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
            END.
            ELSE
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     'ИНФОРМАЦИЯ: Договор является охватывающим, а резерв ведется на транщах. Расчет резерва по траншам' SKIP  
                     '   Договор: '  TRIM(iContCode)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
         END.
         ELSE
         DO:
            vCalcThisLoan = YES.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Расчет резерва по переданному договору' SKIP  
                  '   Договор: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
      
      /*Расчет по охватыващему договору*/
      IF vCalcMainLoan
      THEN DO:
         vi = LOOKUP(iAcctType,vTrRoleCorr,";").
         IF     iAcctType  BEGINS "КредРез"
            AND vi         >  0
         THEN vOsnRoleList = ENTRY(vi + 1,vTrRoleCorr,";").
         ELSE vOsnRoleList = iAcctType.
               /* Вычисляем базу для расчета резерва по данному траншу (сумма параметров на данном договоре-транше) */
         DO vi = 1 TO NUM-ENTRIES(vOsnRoleList):
            RUN GetSummDolg(iContract,
                            iContCode,
                            ENTRY(vi,vOsnRoleList),
                            iSince,
                            iBegDate,
                            iEndDate,
                            iCurrency,
                            OUTPUT vSummDolg,
                            OUTPUT vAllSummDolg,
                            OUTPUT vByTerm).
            vBasaOst = vBasaOst + vSummDolg.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Суммы по ролям для расчета резерва' SKIP  
                  '   Договор: '   TRIM(iContCode) SKIP
                  '   Роль: '      TRIM(ENTRY(vi,vOsnRoleList)) SKIP
                  '   Сумма: '     TRIM(STRING(vSummDolg, "->>>>>>>>>>>>>>9.99"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               'ИНФОРМАЦИЯ: База для расчета резерва' SKIP  
               '   Договор: '       TRIM(iContCode) SKIP
               '   Сумма: '         TRIM(STRING(vBasaOst, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
         IF vBasaOst =  0 THEN oSummRsrv = 0.
         ELSE DO:
            /* Определяем сумму начисл. резерва на охват. договоре (остаток по соотв. параметру)
            ** Т.к. резерв ведется на охватывающем договоре, то этот начисленный резерв -
            ** резерв по всем траншам. И начисл. резерв по данному траншу должен быть
            ** рассчитан как доля общего резерва, соответствующая доле обязательств по договору */
            DO vi = 1 TO NUM-ENTRIES(vPrmList):
               /*Учитываем разбивку по срокам*/
               IF iBegDate <> ? AND iEndDate <> ?
                   AND ENTRY(vi,vPrmList) =  "21"
               THEN
                  RUN SUMM_SPIS_RES(iContract,
                                 ENTRY(1,iContCode," "),
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 OUTPUT vSumm).    /* Резерв ведется в рублях*/
               ELSE /*Без разбивки по срокам*/
                  RUN SUMM_BY_PARAM_CUR(ENTRY(vi,vPrmList),
                                     iSince,
                                     iContract,
                                     ENTRY(1,iContCode," "),
                                     OUTPUT vCurr,
                                     OUTPUT vTmpSumm,        /* сумма в валюте */
                                     OUTPUT vSumm).          /* сумма в рублях */

               /* перевод из рублей в указанную валюту (если валюта - не рубли) */
               IF iCurrency <> "" THEN
                  vSumm = CurToCurWork ("УЧЕТНЫЙ","",iCurrency,iSince,vSumm).
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     'ИНФОРМАЦИЯ: Расчет начисленного резерва по параметрам' SKIP  
                     '   Договор: '  TRIM(iContCode) SKIP
                     '   Параметр: ' TRIM(ENTRY(vi,vPrmList)) SKIP
                     '   Сумма: '    TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   Валюта: '   TRIM(iCurrency)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
               /* вычитаем, чтобы сумма начисленного резерва была положительной
               для корректного договора */              
               vOstParamRes = vOstParamRes - vSumm.
            END.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Начисленный резерв ' SKIP  
                  '   Договор: '  TRIM(iContCode) SKIP
                  '   Сумма: '    TRIM(STRING(vOstParamRes, "->>>>>>>>>>>>>>9.99"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
            vTranshCount = 0.
            FIND FIRST tt-SumDolg WHERE tt-SumDolg.contract  =  iContract
                                    AND tt-SumDolg.cont-code =  ENTRY(1,iContCode," ")
                                    AND tt-SumDolg.AcctType  =  vOsnRoleList
                                    AND tt-SumDolg.since     =  iSince
               NO-LOCK NO-ERROR.
            IF AVAILABLE tt-SumDolg THEN
            DO: 
               ASSIGN
                  vAllBasaOst  = tt-SumDolg.AllBasaOst
                  vTranshCount = tt-SumDolg.TranshCount 
               .
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     'ИНФОРМАЦИЯ: База для расчета резерва по всем траншам ' SKIP  
                     '   Договор: '  TRIM(iContCode) SKIP
                     '   Сумма: '   TRIM(STRING(vAllBasaOst, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   Кол-во траншей: '   TRIM(STRING(vTranshCount))
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
            END.
            ELSE DO:
               /* определяем сумму по параметрам для всех траншей охват. договора данного транша */
               FOR EACH bloan WHERE bloan.contract  =  iContract
                                AND bloan.cont-code BEGINS ENTRY(1,iContCode," ") + " "
               NO-LOCK:
                  DO vi = 1 TO NUM-ENTRIES(vOsnRoleList):
                     RUN GetSummDolg(bloan.contract,
                                     bloan.cont-code,
                                     ENTRY(vi,vOsnRoleList),
                                     iSince,
                                     iBegDate,
                                     iEndDate,
                                     iCurrency,
                                     OUTPUT vSummDolg,
                                     OUTPUT vAllSummDolg,
                                     OUTPUT vByTerm).
                     IF mPutProt THEN
                     DO:
                        OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                        PUT STREAM sOut UNFORMATTED
                           'ИНФОРМАЦИЯ: База для расчета резерва по конкретному траншу ' SKIP  
                           '   Договор: ' TRIM(bloan.cont-code) SKIP
                           '   Роль: '    TRIM(ENTRY(vi,vOsnRoleList)) SKIP
                           '   Сумма: '   TRIM(STRING(vSummDolg, "->>>>>>>>>>>>>>9.99"))
                        SKIP.
                        OUTPUT STREAM sOut CLOSE.
                     END.
                     vAllBasaOst = vAllBasaOst + vSummDolg.
                  END.
                  vTranshCount = vTranshCount + 1.
               END.
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     'ИНФОРМАЦИЯ: База для расчета резерва по всем траншам ' SKIP  
                     '   Договор: '  TRIM(iContCode) SKIP
                     '   Сумма: '   TRIM(STRING(vAllBasaOst, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   Кол-во траншей: '   TRIM(STRING(vTranshCount))
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
               CREATE tt-SumDolg.
               ASSIGN
                  tt-SumDolg.contract    = iContract
                  tt-SumDolg.cont-code   = ENTRY(1,iContCode," ")
                  tt-SumDolg.AcctType    = vOsnRoleList
                  tt-SumDolg.since       = iSince
                  tt-SumDolg.AllBasaOst  = vAllBasaOst
                  tt-SumDolg.TranshCount = vTranshCount
                .
            END.
            /* Расчитываем сумму резерва */
            ASSIGN
               vRate = IF vAllBasaOst =  0 THEN 0 ELSE (vBasaOst / vAllBasaOst)
               oSummRsrv = ROUND(vRate * vOstParamRes,2)
            .
                       
            /* скорректируем округление на последнем транше */
            IF    oSummRsrv <> 0 
              AND vRate     <> 1 THEN 
            DO:
               ASSIGN
                  vSummRsrvOst = 0 
                  vI           = 0 
               .

               FOR EACH tt-DataLoan WHERE
                            tt-DataLoan.contract  =  iContract  
                        AND tt-DataLoan.cont-code BEGINS ENTRY(1,iContCode," ") + " " 
                        AND tt-DataLoan.since     =  iSince 
               NO-LOCK :
                  ASSIGN
                     vSummRsrvOst = vSummRsrvOst + tt-DataLoan.rsrv-summ 
                     vI = vI + 1 
                  .                 
               END.           
               IF vI =  vTranshCount - 1 THEN 
                  oSummRsrv = vOstParamRes - vSummRsrvOst .
                      
            END.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Расчитанный резерв ' SKIP  
                  '   Договор: '  TRIM(iContCode) SKIP
                  '   Сумма: '    TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99")) SKIP
                  '   %Рез: '     TRIM(STRING(vRate, "->>>>>>>>>>>>>>9.99999"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
         END.
      END.     /* расчет по охватывающему договору */

      ELSE
      /*Расчет по переданному договору*/
      IF vCalcThisLoan THEN
      DO:
         DO vi = 1 TO NUM-ENTRIES(vPrmList):
                     /* Умеем разносить по срокам только резерв по ссудной задолженности */
            IF     iBegDate <> ?
               AND iEndDate <> ?
               AND ENTRY(vi,vPrmList) =  "21"
            THEN DO:
               RUN SUMM_SPIS_RES(iContract,
                                 iContCode,
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 OUTPUT vSumm).    /* неизвестная валюта */
               /* определяем валюту параметра */
               RUN GetParP IN h_loan (RECID(loan),
                                      INT64(ENTRY(vi,vPrmList)),
                                      vSumm,
                                      OUTPUT vTmpSumm,  /* сумма в рублях */
                                      OUTPUT vCurr).
               /* перевод из валюты параметра в указанную валюту */
               IF iCurrency <> vCurr THEN
                  vSumm = CurToCurWork ("УЧЕТНЫЙ",vCurr,iCurrency,iSince,vSumm).
            END.
                     /* без разноски по срокам */
            ELSE DO:

               RUN SUMM_BY_PARAM_CUR(ENTRY(vi,vPrmList),
                                     iSince,
                                     iContract,
                                     iContCode,
                                     OUTPUT vCurr,
                                     OUTPUT vTmpSumm,        /* сумма в валюте */
                                     OUTPUT vSumm).          /* сумма в рублях */
               /* перевод из рублей в указанную валюту (если валюта - не рубли) */
               IF iCurrency <> "" THEN
                  vSumm = CurToCurWork ("УЧЕТНЫЙ","",iCurrency,iSince,vSumm).
            END.
            
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Расчет начисленного резерва ' SKIP  
                  '   Договор: '  TRIM(iContCode) SKIP
                  '   Параметр: ' TRIM(ENTRY(vi,vPrmList)) SKIP
                  '   Сумма: '    TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                  '   Валюта: '   TRIM(iCurrency)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
            /* вычитаем, чтобы сумма начисленного резерва была положительной
               для корректного договора */
            oSummRsrv = oSummRsrv - vSumm.
         END. /* расчет по переданному договору*/
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               'ИНФОРМАЦИЯ: Начисленный резерв ' SKIP  
               '   Договор: '  TRIM(iContCode) SKIP
               '   Сумма: '    TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
      END.
   END. /*MAIN_BLOCK*/
   

   /* возможно требуется разбиение резерва */
 
     /* ищем  счет */
      FIND LAST loan-acct WHERE loan-acct.contract  =  iContract
                            AND loan-acct.cont-code =  iContCode
                            AND loan-acct.acct-type =  iAcctType
                            AND loan-acct.since     <= iSince
           NO-LOCK NO-ERROR.


      vPrmList = GetCodeMisc(vCodeName, iAcctType, 3) .
      FIND FIRST code WHERE code.class   =  vCodeName
                        AND code.parent  =  vCodeName
                        AND code.code    <>  iAcctType
                        AND code.misc[3] =  vPrmList
            NO-LOCK NO-ERROR .

      IF     oSummRsrv <> 0
         AND AVAIL loan-acct         
         AND AVAIl code      THEN
      DO:

         RUN GetPartReservRole  IN h_rsrv ( loan-acct.acct,
                                 iCurrency,
                                 iSince,
                                 iAcctType,
                    INPUT-OUTPUT oSummRsrv,
                    INPUT-OUTPUT vSummRsrvRub ) .
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               'ИНФОРМАЦИЯ: Если требуется разбиение резерва ' SKIP  
               '   Счет: '        TRIM(loan-acct.acct) SKIP
               '   Роль: '        TRIM(iAcctType) SKIP
               '   Валюта: '      TRIM(iCurrency) SKIP
               '   Сумма: '       TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99")) SKIP
               '   Сумма в РУБ: ' TRIM(STRING(vSummRsrvRub, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
      END.

   RETURN.
END PROCEDURE.


/* возвращает расчетный резерв и расчетный коэффициент риска по роли счета */
PROCEDURE LnRsrvByType.
   DEF INPUT  PARAM iAcct     AS CHAR   NO-UNDO. /* номер счета */
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO. /* роль счета */
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO. /* валюта, в которой надо получить данные */
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO. /* дата, на которую проводить расчет */
   DEF OUTPUT PARAM oRsrvProc AS DEC    NO-UNDO.
   DEF OUTPUT PARAM oRsrvRate AS DEC    NO-UNDO.
   
   /* Анализируем тип роли счета */
            /* Вексель */
   IF      iAcct BEGINS "51"
      AND (   iAcctType =  "КредТ"
           OR iAcctType =  "КредДискДН"
           OR iAcctType =  "КредПр%"
           OR iAcctType =  "КредДискПр")
   THEN
       RUN LnFormRsrvProc (iContract,
                          iContCode,
                          iDate,
                          iAcctType,
                          iCurrency,
                          NO,
                          OUTPUT oRsrvProc,
                          OUTPUT oRsrvRate).
            /* Требование по процентам */
   ELSE IF iAcctType =  "КредТ" OR iAcctType =  "КредТПр" THEN
   DO:
      ASSIGN
         oRsrvProc = LnFormRsrvRoleSpis (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType) 
         oRsrvRate = LnFormRsrvCalcRole (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
      . 
   END.
            /* Просроченные проценты */
   ELSE IF iAcctType =  "КредПр%" OR iAcctType =  "КредПр%1" THEN
   DO:
      ASSIGN
         oRsrvProc = LnFormRsrvRoleSpis (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
         oRsrvRate = LnFormRsrvCalcRole (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
      . 
   END.
            /* Комиссии, штрафы, пени */
   ELSE IF CAN-DO("КредБудКом,КредБудПени",iAcctType) THEN
   DO:
      oRsrvProc = LnCalcRsrvProc (iContract,
                                  iContCode,
                                  iDate,
                                  iCurrency).
      oRsrvRate = LnFormRsrvCom (iContract,
                                 iContCode,
                                 iDate,
                                 iCurrency,
                                 iAcctType).
   END.
   ELSE IF iAcctType =  "КредПр" THEN
   DO:
       ASSIGN
         oRsrvProc =  LnRsrvRate(iContract,
                                 iContCode,
                                 iDate)
         oRsrvRate = LnFormRsrvBadDebtTransh (iContract,
                                              iContCode,
                                              iDate,
                                              iCurrency)
      .
   END.
   /* Если это другая роль счета, то считаем как для "Кредит", т.к.
   ** коэф. резервирования + гр.риска берется с договора (oRsrvProc - коэф. рез) */
   ELSE ASSIGN
        oRsrvProc =  LnRsrvRate(iContract,
                                iContCode,
                                iDate)
        oRsrvRate = LnFormRsrvGoodDebtTransh(iContract,
                                                  iContCode,
                                                  iDate,
                                                  iCurrency).

   RETURN.
END PROCEDURE.

/* Процедура поиска счета резерва договора */
PROCEDURE GetLoanAcctRsrv.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.   /* назаначение */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.   /* номер договора */
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.   /* роль счета, по которой определяется тип резерва */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.   /* дата, на которую ищется счет резерва */
   DEF OUTPUT PARAM oAcct     AS CHAR   NO-UNDO.   /* номер счета резерва */
   DEF OUTPUT PARAM oCurrency AS CHAR   NO-UNDO.   /* валюта счета резерва */
   DEF OUTPUT PARAM oRsrvRole AS CHAR   NO-UNDO.   /* роль счета резерва */
   
   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-acct FOR loan-acct.

   DEF VAR vCodeName AS CHAR NO-UNDO.

   MAIN_BLOCK:
   DO:
      ASSIGN
         vCodeName  = FGetSetting("СпрРезервДог",?,"РезервДог")
         oRsrvRole  = GetCodeName(vCodeName,iAcctType).
      IF NOT {assigned oRsrvRole}
         THEN LEAVE MAIN_BLOCK.

      RUN RE_L_ACCT IN h_loan (iContract,
                               iContCode,
                               oRsrvRole,
                               iSince,
                               BUFFER loan-acct).
      IF AVAIL loan-acct THEN
         ASSIGN oAcct      = loan-acct.acct
                oCurrency  = loan-acct.currency
         .

      IF NOT {assigned oAcct}
         AND NUM-ENTRIES(iContCode, " ") = 2 THEN
      DO:
         FIND FIRST loan WHERE
                    loan.contract  = iContract
                AND loan.cont-code = ENTRY(1, iContCode, " ")
         NO-LOCK NO-ERROR.
         
         IF     AVAIL loan 
            AND CAN-DO(GetXAttrInit(loan.class-code,"Rel_Type"), oRsrvRole) THEN
         DO:
            RUN RE_L_ACCT IN h_loan (loan.contract,
                                     loan.cont-code,
                                     oRsrvRole,
                                     iSince,
                                     BUFFER loan-acct).

            IF AVAIL loan-acct THEN
               ASSIGN 
                  oAcct      = loan-acct.acct
                  oCurrency  = loan-acct.currency
               .
         END.
      END.
   END.
   RETURN.
END PROCEDURE.

/* возвращает сформированный резерв по списку параметров с разбивкой по траншам
   похожа на PROCEDURE SUMM_SPIS_RES_TYPE, только для витрины, проще и быстрее */
{pfuncdef 
 &DefFunc = "LnRsrvByPar"
 &Description = "Возвращает сформированный резерв ~
 по списку параметров с разбивкой по траншам"
 &Parameters = "Назначение договора,Номер договора,~
 Список параметров,Дата расчета,Сумма резерва"
 &Result= "Значения во временной таблице tt-DataLoanInstr139"
 &Sample="RUN LnRsrvByPar IN h_pqres ( ~
 'Кредит', ~
   'br_705@002', ~
   '21,46', ~
   TODAY, ~
   OUTPUT vAmt)."}
PROCEDURE LnRsrvByPar:
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iSpisPar  AS CHAR   NO-UNDO. /* список параметров */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO. /* дата, на которую проводить расчет */
   DEF OUTPUT PARAM oRsrvSum  AS DEC    NO-UNDO. /* сумма резерва */

   DEF VAR vIsThis   AS LOG   NO-UNDO.
   DEF VAR vPar      AS CHAR  NO-UNDO.
   DEF VAR vParInt   AS INT64 NO-UNDO.
   DEF VAR vPar2     AS CHAR  NO-UNDO.
   DEF VAR vPar2Int  AS INT64 NO-UNDO.
   DEF VAR vCounter  AS INT64 NO-UNDO.
   DEF VAR vCounter2 AS INT64 NO-UNDO.
   DEF VAR vAmtLn    AS DEC   NO-UNDO.
   DEF VAR vAmtTr    AS DEC   NO-UNDO.
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vMaxSum   AS DEC   NO-UNDO.
   DEF VAR vMaxRow   AS ROWID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vCodeName AS CHAR  NO-UNDO.

   DEF BUFFER loan  FOR loan.
   DEF BUFFER bloan FOR loan.
   DEF BUFFER bcode FOR code.

   MAIN:
   DO:
      vCodeName  = FGetSetting("СпрРезервДог",?,"РезервДог").
      FIND FIRST loan WHERE 
                 loan.contract =  iContract 
             AND loan.cont-code =  iContCode 
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE MAIN.
      LOOP_PRM:
      DO vCounter = 1 TO NUM-ENTRIES (iSpisPar):
         ASSIGN
            vPar = ENTRY(vCounter, iSpisPar)
            /* если криво переводится, то следующий параметр */
            vParInt = INT64(vPar) 
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            LEAVE LOOP_PRM.
         vIsThis = YES.
            /* определяем где ведется резарв в случае траншевого договора */
         IF NUM-ENTRIES (loan.cont-code, " ") >  1 
            OR loan.cont-type =  "Течение" THEN
         DO:
               /* находим роль счета резерва по параметру */
            FIND FIRST tt-ReservDog WHERE
                       tt-ReservDog.res_par =  vPar
            NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-ReservDog THEN
            DO:
                  /* если есть записи, то уже заполняли ранее и скорее всего кривой параметр */
               IF CAN-FIND (FIRST tt-ReservDog NO-LOCK) THEN
                  LEAVE MAIN.
                  /* если не нашли, то заполняем временную табличку из классификатора */
               FOR EACH bcode WHERE
                        bcode.class  =  vCodeName
                    AND bcode.parent =  vCodeName
               NO-LOCK:
                  FIND FIRST tt-ReservDog WHERE
                             tt-ReservDog.res_par =  bcode.misc[3]
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL tt-ReservDog THEN
                  DO:
                     CREATE tt-ReservDog.
                     ASSIGN
                        tt-ReservDog.res_par = bcode.misc[3]
                        tt-ReservDog.res_sch = bcode.name
                     .
                  END.
                     IF {assigned bcode.misc[1]} THEN
                        {additem.i tt-ReservDog.plus_par bcode.misc[1]}
                     IF {assigned bcode.misc[2]} THEN
                        {additem.i tt-ReservDog.minus_par bcode.misc[2]}
               END.
                  /* переискиваем */
               FIND FIRST tt-ReservDog WHERE
                          tt-ReservDog.res_par =  vPar
               NO-LOCK NO-ERROR.
                  /* если не нашли, то параметр отсутствует в классификаторе */
               IF NOT AVAIL tt-ReservDog THEN
               DO:
                  LEAVE MAIN.
               END.
            END.
            vIsThis = LnRsrvCheckType (loan.contract,
                                       loan.cont-code,
                                       tt-ReservDog.res_sch).
         END. /* IF NUM-ENTRIES (loan.cont-code, " ") GT 1 
                              OR loan.cont-type EQ "Течение" THEN */
            /* для обычных договоров и охватывающих, 
               а так же для траншей с ведением резерва на них */
         IF vIsThis THEN
         DO:
               /* для охватывающего договора проверяем наличие параметров требований,
                  если их нет, то значит резерв будет распределен между траншами */
            IF loan.cont-type =  "Течение" THEN
            CHECK_KL:
            DO:
               LOOP_PRM2:
               DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.plus_par):
                  ASSIGN
                     vPar2 = ENTRY(vCounter2, tt-ReservDog.plus_par)
                        /* если криво переводится, то следующий параметр */
                     vPar2Int = INT64(vPar2) 
                  NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN
                     LEAVE LOOP_PRM2.
                  RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                             iDate,
                                             loan.since,
                                             loan.contract,
                                             loan.cont-code,
                                             OUTPUT vTmpDec[1], /* значение */
                                             OUTPUT vTmpDec[2],
                                             OUTPUT vTmpDec[3]).
                     /* если параметр есть, то считаем резерв как у обычного договора */
                  IF vTmpDec[1] >  0 THEN
                     LEAVE CHECK_KL.
               END.
                  /* если параметра требований нет, 
                     то переходим к следующему параметру резерва */
               NEXT LOOP_PRM.
            END.
            RUN RE_PARAM_EX IN h_Loan (vParInt,
                                       iDate,
                                       loan.since,
                                       loan.contract,
                                       loan.cont-code,
                                       OUTPUT vTmpDec[1], /* значение */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            oRsrvSum = oRsrvSum - vTmpDec[1]. /* параметр резерва отрицательный */
         END.
            /* для траншей в случае ведения резерва на охватывающем договоре, 
               а требований на отдельных траншах определяем часть относящуюся к траншу */
         ELSE
         DO:
               /* поищем договор во временной табличке */
            FIND FIRST tt-ResLoan WHERE
                       tt-ResLoan.res_par   =  vPar
                   AND tt-ResLoan.cont-code =  loan.cont-code
            NO-LOCK NO-ERROR.
               /* если не нашли то заполняем по всему траншевому договору */
            IF NOT AVAIL tt-ResLoan THEN
            DO:
               vAmtLn = 0.
               FOR EACH bloan WHERE 
                        bloan.contract  =  loan.contract 
                    AND bloan.cont-code BEGINS ENTRY(1, loan.cont-code, " ") + " "
                    AND NUM-ENTRIES (bloan.cont-code, " ") >  1 NO-LOCK:
                     /* суммируем параметры требований */
                  vAmtTr = 0.
                     /* плюсуем */
                  LOOP_PRM2:
                  DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.plus_par):
                     ASSIGN
                        vPar2 = ENTRY(vCounter2, tt-ReservDog.plus_par)
                           /* если криво переводится, то следующий параметр */
                        vPar2Int = INT64(vPar2) 
                     NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN
                        LEAVE LOOP_PRM2.
                     RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                                iDate,
                                                bloan.since,
                                                bloan.contract,
                                                bloan.cont-code,
                                                OUTPUT vTmpDec[1], /* значение */
                                                OUTPUT vTmpDec[2],
                                                OUTPUT vTmpDec[3]).
                     vAmtTr = vAmtTr + vTmpDec[1].
                  END.
                     /* минусуем */
                  LOOP_PRM2:
                  DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.minus_par):
                     ASSIGN
                        vPar2 = ENTRY(vCounter2, tt-ReservDog.minus_par)
                           /* если криво переводится, то следующий параметр */
                        vPar2Int = INT64(vPar2) 
                     NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN
                        LEAVE LOOP_PRM2.
                     RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                                iDate,
                                                bloan.since,
                                                bloan.contract,
                                                bloan.cont-code,
                                                OUTPUT vTmpDec[1], /* значение */
                                                OUTPUT vTmpDec[2],
                                                OUTPUT vTmpDec[3]).
                     vAmtTr = vAmtTr - vTmpDec[1].
                  END.
                  CREATE tt-ResLoan.
                  ASSIGN 
                     tt-ResLoan.res_par   = vPar
                     tt-ResLoan.cont-code = bloan.cont-code
                     tt-ResLoan.sum_tr    = vAmtTr
                  .
                  vAmtLn = vAmtLn + vAmtTr.
               END. /* FOR EACH bloan WHERE */
                  /* вычисляем сумму резерва, он на охватывающем договоре */
               FIND FIRST bloan WHERE
                          bloan.contract  =  loan.contract
                      AND bloan.cont-code =  ENTRY(1, loan.cont-code, " ")
               NO-LOCK NO-ERROR.
               IF NOT AVAIL bloan THEN
                  NEXT LOOP_PRM.
               RUN RE_PARAM_EX IN h_Loan (vParInt,
                                          iDate,
                                          bloan.since,
                                          bloan.contract,
                                          bloan.cont-code,
                                          OUTPUT vTmpDec[1], /* значение */
                                          OUTPUT vTmpDec[2],
                                          OUTPUT vTmpDec[3]).
                   /* если есть на что распределять */
               IF vAmtLn <> 0 THEN
               DO:
                  /* распределяем резерв пропорционально */
                  ASSIGN
                     vDif    = 0 - vTmpDec[1]
                     vMaxSum = 0
                     vMaxRow = ?
                  .
                  FOR EACH tt-ResLoan WHERE
                           tt-ResLoan.res_par   =  vPar
                       AND tt-ResLoan.cont-code BEGINS bloan.cont-code + " "
                       AND NUM-ENTRIES(tt-ResLoan.cont-code, " ") >  1
                  EXCLUSIVE-LOCK:
                     ASSIGN
                        tt-ResLoan.sum_res = 0 - ROUND (vTmpDec[1] / vAmtLn * tt-ResLoan.sum_tr, 2)
                        vDif = vDif - tt-ResLoan.sum_res
                     .
                        /* запоминаем строку с максимальной суммой */
                     IF vMaxSum <  tt-ResLoan.sum_res THEN
                        ASSIGN
                           vMaxSum = tt-ResLoan.sum_res
                           vMaxRow = ROWID (tt-ResLoan)
                        .
                  END.
                     /* компенсируем погрешность */
                  IF vDif <> 0 THEN
                  DO:
                     FIND FIRST tt-ResLoan WHERE 
                                ROWID (tt-ResLoan) =  vMaxRow 
                     NO-ERROR.
                     IF AVAIL tt-ResLoan THEN
                        tt-ResLoan.sum_res = tt-ResLoan.sum_res + vDif.
                  END.
               END.
                  /* всё заполнили, теперь переищем */
               FIND FIRST tt-ResLoan WHERE
                          tt-ResLoan.res_par   =  vPar
                      AND tt-ResLoan.cont-code =  loan.cont-code
               NO-LOCK NO-ERROR.
                  /* если вдруг не найден, то это загадачная ситуация... */
               IF NOT AVAIL tt-ResLoan THEN
                  NEXT LOOP_PRM.
            END. /* IF NOT AVAIL tt-ResLoan THEN */
            oRsrvSum = oRsrvSum + tt-ResLoan.sum_res.
         END. /* IF vIsThis THEN */
      END. /* DO vCounter = 1 TO NUM-ENTRIES (iSpisPar): */
   END. /* MAIN: */
END PROCEDURE.

/* Получение данных из договора КиД по счету: ПОС, сумма задолженности
** в соответствии с ролью счета по договору, значение % резервирования,
** группы риска, значение сформированного резерва, значение расчетного
** резерва и расчетного коэф. риска.
** Данные складываются во временную таблицу. Поля таблицы см. pqres.def
** Формат последнего параметра (таблица) при вызове:
**     вариант 1: OUTPUT TABLE tt-LoanAcct  - в этом случае все записи,
**         существовавшие до вызова, удаляются
**     вариант 2: OUTPUT TABLE tt-DataLoan APPEND - в этом случае записи,
**         полученные из процедуры, добавляются к записям уже существовавшим
** Параметр iCurrency используется для поиска счета в loan-acct.
** Все суммы приводятся к валюте договора 
**
** Доработано по 0195050: Обрабатываются также договоры аренды сейфовых ячеек
** (contract = АРЕНДА)
*/
PROCEDURE GetAllFromLoan.
   DEF INPUT  PARAM iAcct     AS CHAR   NO-UNDO.   /* номер счета */
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.   /* валюта счета */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.   /* дата, на которую собираются данные */
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.   /* период для расчета */
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.   /* данных по периодам */
   DEF INPUT  PARAM iInclClsd AS LOG    NO-UNDO.   /* Включать ли в расчет закрытые договоры */
   DEFINE OUTPUT PARAMETER TABLE FOR tt-DataLoan.  /* таблица в которую складываются данные */

   DEF VAR vBag      AS CHAR   NO-UNDO.      /* ПОС (если договор входит в ПОС) */
   DEF VAR vSummDolg AS DEC    NO-UNDO.      /* сумма задолженности в соответствии с ролью счета по договору  (с учетом сроков) */
   DEF VAR vRsrvRate AS DEC    NO-UNDO.      /* коэффициент резервирования */
   DEF VAR vGRsrv    AS INT64    NO-UNDO.      /* группы риска (категория качества) */
   DEF VAR vRsrvSumm AS DEC    NO-UNDO.      /* значение сформированного резерва (с учетом сроков) */
   DEF VAR vRSummClc AS DEC    NO-UNDO.      /* значение расчетного резерва */
   DEF VAR vRRsrvClc AS DEC    NO-UNDO.      /* значение расчетного коэф. риска */
   DEF VAR vRsrvAcct AS CHAR   NO-UNDO.      /* номер счета резерва */
   DEF VAR vRsrvCurr AS CHAR   NO-UNDO.      /* валюта счета резерва */
   DEF VAR vRsrvRole AS CHAR   NO-UNDO.      /* роль счета резерва */
   DEF VAR vBasaOst        AS DEC   NO-UNDO.
   DEF VAR vAllBasaOst     AS DEC   NO-UNDO.
   DEF VAR vAllSummDolg    AS DEC   NO-UNDO.
   DEF VAR vGar            AS DEC   NO-UNDO.
   DEF VAR vLoanRiskRate   AS DEC   NO-UNDO.
   DEF VAR vRiskAmount     AS DEC   NO-UNDO.
   DEF VAR vBasaDone       AS LOG   NO-UNDO.
   DEF VAR vByTerm         AS LOG   NO-UNDO.
   DEF VAR vTempByTerm     AS LOG   NO-UNDO.
   DEF VAR vSummDolgRub    AS DEC   NO-UNDO.   /* сумма задолженности в рублях */
   DEF VAR vEndDate        AS DATE  NO-UNDO.

   DEF VAR vTrRoleCorr    AS CHAR   NO-UNDO INIT "КредТ,КредТПр,КредПр%,
                                                  КредПр%1,КредБудКом,
                                                  КредБудПени,КредДискДН,
                                                  КредДискПр".
   DEF BUFFER bloan         FOR loan.      /* Локализация буфера. */
   DEF BUFFER bloan-acct    FOR loan-acct. /* Локализация буфера. */
   DEF BUFFER b1-lacct      FOR loan-acct.   /* Локализация буфера. */
   DEF BUFFER bloan-cond    FOR loan-cond.

   /* По-умолчанию включаем все договоры (и открытые, и закрытые на дату iSince) */
   IF iInclClsd =  ? THEN iInclClsd = YES.

   MAIN_BLOCK:
   DO:
      {empty tt-DataLoan}
      {empty tt-SumDolg}
      {empty tt-LoanAcct}

      FOR EACH bloan-acct WHERE 
               bloan-acct.acct     =  iAcct
           AND bloan-acct.curr     =  iCurrency
           AND bloan-acct.since    <= iSince 
           AND bloan-acct.contract =  "Кредит" 
                      /* В НП ФОиАКредРоль задаются роли счетов, которые должны
                      ** обрабатываться этим инструментом. */
           AND CAN-DO(mFOiAKredRole,bloan-acct.acct-type)
           AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.contract   =  bloan-acct.contract
                                              AND b1-lacct.cont-code  =  bloan-acct.cont-code
                                              AND b1-lacct.acct-type  =  bloan-acct.acct-type
                                              AND b1-lacct.since      <= iSince
                                              AND b1-lacct.since      >  bloan-acct.since)
         NO-LOCK:
         
         CREATE tt-LoanAcct.
         ASSIGN
            tt-LoanAcct.contract  = bloan-acct.contract
            tt-LoanAcct.cont-code = bloan-acct.cont-code
            tt-LoanAcct.acct-type = bloan-acct.acct-type
            tt-LoanAcct.since     = bloan-acct.since
         .
      END.

      FOR EACH bloan-acct WHERE 
               bloan-acct.acct     =  iAcct
           AND bloan-acct.curr     =  iCurrency
           AND bloan-acct.since    <= iSince 
           AND bloan-acct.contract =  "АРЕНДА"
           AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.contract   =  bloan-acct.contract
                                              AND b1-lacct.cont-code  =  bloan-acct.cont-code
                                              AND b1-lacct.acct-type  =  bloan-acct.acct-type
                                              AND b1-lacct.since      <= iSince
                                              AND b1-lacct.since      >  bloan-acct.since)
         NO-LOCK:
         
         CREATE tt-LoanAcct.
         ASSIGN
            tt-LoanAcct.contract  = bloan-acct.contract
            tt-LoanAcct.cont-code = bloan-acct.cont-code
            tt-LoanAcct.acct-type = bloan-acct.acct-type
            tt-LoanAcct.since     = bloan-acct.since
         .
      END.

      IF NOT AVAILABLE tt-LoanAcct THEN LEAVE MAIN_BLOCK.

      /* перебор всех найденных договоров */
      CYCLE:
      FOR EACH tt-LoanAcct,
         FIRST bloan WHERE bloan.contract   =  tt-LoanAcct.contract
                       AND bloan.cont-code  =  tt-LoanAcct.cont-code
                       AND (   bloan.close-date =  ?
                            OR bloan.close-date >  iSince
                            OR iInclClsd)
      NO-LOCK:
         
         IF bloan.contract =  "Кредит" THEN
         DO:
            IF mPutProt THEN
            DO:
                  /* поиск счета резерва */
               RUN GetLoanAcctRsrv IN THIS-PROCEDURE (
                                   tt-LoanAcct.contract,
                                   tt-LoanAcct.cont-code,
                                   tt-LoanAcct.acct-type,
                                   iSince,
                                   OUTPUT vRsrvAcct,
                                   OUTPUT vRsrvCurr,
                                   OUTPUT vRsrvRole).
             
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  'ИНФОРМАЦИЯ: Расчет резерв для счета ' TRIM(iAcct) SKIP
                  '   Договор: ' TRIM(tt-LoanAcct.cont-code) SKIP
                  '   Счет резерва: ' TRIM(vRsrvAcct)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
            
            /* ПОС (если договор входит в ПОС) */
             vBag = LnInBagOnDate (tt-LoanAcct.contract, tt-LoanAcct.cont-code, iSince).
            /* сумма задолженности в соответствии с ролью счета по договору  (с учетом сроков) */
            RUN GetSummDolgBuff IN THIS-PROCEDURE (BUFFER bloan,
                                                   tt-LoanAcct.acct-type,
                                                   iSince,
                                                   iBegDate,
                                                   iEndDate,
                                                   bloan.currency,
                                                   OUTPUT vSummDolg,
                                                   OUTPUT vAllSummDolg,
                                                   OUTPUT vByTerm).
            
                     /* значение % резервирования (коэффициент резервирования) */
            vRsrvRate = LnRsrvRate (tt-LoanAcct.contract, tt-LoanAcct.cont-code, iSince).
                     /* группы риска (категория качества) */
            vGRsrv = re_history_risk(tt-LoanAcct.contract, tt-LoanAcct.cont-code,iSince,?).

           /* значение сформированного резерва (с учетом сроков) */
            RUN SUMM_SPIS_RES_TYPE_BUFF IN THIS-PROCEDURE (BUFFER bloan,
                                                           tt-LoanAcct.acct-type,
                                                           iSince,
                                                           iBegDate,
                                                           iEndDate,
                                                           "",
                                                           ?,
                                                           OUTPUT vRsrvSumm).
                     /* значение расчетного резерва и коэф. риска */
            RUN LnRsrvByType (iAcct,
                              tt-LoanAcct.acct-type,
                              "",
                              tt-LoanAcct.contract,
                              tt-LoanAcct.cont-code,
                              iSince,
                              OUTPUT vRRsrvClc,
                              OUTPUT vRSummClc).

            /* Если нужна разноска по срокам */
            IF iBegDate <> ? AND
              iEndDate <> ? AND
              vSummDolg > 0 AND
              vAllSummDolg > 0 AND
              vSummDolg <> vAllSummDolg
              THEN ASSIGN
                     vRSummClc = (vRSummClc * vSummDolg) / vAllSummDolg NO-ERROR.

             /* поиск счета резерва */
            RUN GetLoanAcctRsrv(tt-LoanAcct.contract,
                                tt-LoanAcct.cont-code,
                                tt-LoanAcct.acct-type,
                                iSince,
                                OUTPUT vRsrvAcct,
                                OUTPUT vRsrvCurr,
                                OUTPUT vRsrvRole).
             
                      /* запись данных по договору в таблицу */
            DO:
               vSummDolgRub = CurToCurWork ("УЧЕТНЫЙ",iCurrency,"",iSince,vSummDolg).
               CREATE tt-DataLoan.
               ASSIGN tt-DataLoan.rid                 = RECID(bloan)
                      tt-DataLoan.acct                = iAcct
                      tt-DataLoan.currency            = iCurrency
                      tt-DataLoan.acct-type           = tt-LoanAcct.acct-type
                      tt-DataLoan.class-code          = bloan.class-code 
                      tt-DataLoan.contract            = tt-LoanAcct.contract
                      tt-DataLoan.cont-code           = tt-LoanAcct.cont-code
                      tt-DataLoan.since               = iSince
                      tt-DataLoan.open-date           = bloan.open-date
                      tt-DataLoan.end-date            = bloan.end-date
                      tt-DataLoan.close-date          = bloan.close-date
                      tt-DataLoan.bag                 = vBag
                      tt-DataLoan.dolg-summ           = IF vSummDolg = ? THEN 0
                                                                         ELSE vSummDolg
                      tt-DataLoan.grsrv               = IF vGRsrv    = ? THEN 1
                                                                         ELSE vGRsrv
                      tt-DataLoan.rsrv-rate           = IF vRsrvRate = ? THEN 0
                                                                         ELSE vRsrvRate
                      tt-DataLoan.rsrv-summ           = IF vRsrvSumm = ? THEN 0
                                                                         ELSE vRsrvSumm
                      tt-DataLoan.rsrv-rate-clc       = IF vRRsrvClc = ? THEN 0
                                                                         ELSE vRRsrvClc
                      tt-DataLoan.rsrv-summ-clc       = IF vRSummClc = ? THEN 0
                                                                         ELSE vRSummClc
                      tt-DataLoan.rsrv-summ-clc-gar   = 
                         IF CAN-DO(vTrRoleCorr, tt-LoanAcct.acct-type) THEN
                            (vSummDolgRub * vRsrvRate / 100)
                         ELSE
                            (vSummDolgRub * tt-DataLoan.rsrv-rate-clc / 100)
                      tt-DataLoan.acct-rsrv           = vRsrvAcct
                      tt-DataLoan.curr-rsrv           = vRsrvCurr
                      tt-DataLoan.role-rsrv           = vRsrvRole
                      tt-DataLoan.ByTerm              = vByTerm
                      tt-DataLoan.rsrv-summ           = ROUND(tt-DataLoan.rsrv-summ,2)
                      tt-DataLoan.rsrv-summ-clc       = ROUND(tt-DataLoan.rsrv-summ-clc,2)
                      tt-DataLoan.rsrv-summ-clc-gar   = ROUND(tt-DataLoan.rsrv-summ-clc-gar,2)
               .
            END.
         END. /* if bloan.contract eq "Кредит" */
         ELSE
         IF bloan.contract =  "АРЕНДА" THEN
         DO:
            /* обработка договора аренды сейфовой ячейки */
            RUN Acct-Pos-Pure IN h_base (iAcct, iCurrency, iSince, iSince, gOp-status).
            IF iCurrency =  "" THEN
               vSummDolg = ABS(sh-bal).
            ELSE
               vSummDolg = ABS(sh-val).

            IF     iBegDate <> ?
               AND iEndDate <> ? THEN
            DO:
               vByTerm = YES.
               /* ищем первое условие с датой > расчетной и ДР Prolong = Да
                  если нашлось - договор пролонгирован и при сравнении со сроками
                  будет браться дата пролонгации (= дата условия) */
               FOR EACH bloan-cond WHERE
                      bloan-cond.contract  =  bloan.contract
                  AND bloan-cond.cont-code =  bloan.cont-code
                  AND bloan-cond.since     >  iSince
               NO-LOCK
               BY bloan-cond.since:
                  IF GetXAttrValueEx("loan-cond",
                                     bloan-cond.contract + ","
                                     + bloan-cond.cont-code + ","
                                     + STRING(bloan-cond.since),
                                     "Prolong", "Нет") =  "Да" THEN
                     LEAVE.
               END.
               IF AVAILABLE bloan-cond THEN
                  vEndDate = bloan-cond.since.
               ELSE
                  vEndDate = bloan.end-date.

               /* если дата окончания не попала в нужный период, возвращаем 0 */
               IF vEndDate <> ?
                  AND (   vEndDate <  iBegDate
                       OR vEndDate >  iEndDate) THEN
                  vSummDolg = 0.
            END.
            ELSE
            DO:
               /* сроки не заданы, так что не проверяем их */
               vByTerm = NO.
            END.

            CREATE tt-DataLoan.
            ASSIGN tt-DataLoan.rid                 = RECID(bloan)
                   tt-DataLoan.acct                = iAcct
                   tt-DataLoan.currency            = iCurrency
                   tt-DataLoan.acct-type           = tt-LoanAcct.acct-type
                   tt-DataLoan.class-code          = bloan.Class-Code
                   tt-DataLoan.contract            = tt-LoanAcct.contract
                   tt-DataLoan.cont-code           = tt-LoanAcct.cont-code
                   tt-DataLoan.since               = iSince
                   tt-DataLoan.open-date           = bloan.open-date
                   tt-DataLoan.end-date            = bloan.end-date
                   tt-DataLoan.close-date          = bloan.close-date
                   tt-DataLoan.bag                 = ""
                   tt-DataLoan.dolg-summ           = vSummDolg
                   tt-DataLoan.grsrv               = 1
                   tt-DataLoan.rsrv-rate           = 0
                   tt-DataLoan.rsrv-summ           = 0
                   tt-DataLoan.rsrv-rate-clc       = 0
                   tt-DataLoan.rsrv-summ-clc       = 0
                   tt-DataLoan.rsrv-summ-clc-gar   = 0
                   tt-DataLoan.acct-rsrv           = ""
                   tt-DataLoan.curr-rsrv           = ""
                   tt-DataLoan.role-rsrv           = ""
                   tt-DataLoan.ByTerm              = vByTerm
            .
         END. /* if bloan.contract eq "АРЕНДА" */
      END. /* of FOR EACH tt-LoanAcct */
   END. /* of MAIN_BLOCK */

   RETURN.
END PROCEDURE.

{pfuncdef 
 &DefFunc="GetValueForInstr139"
 &Description="Возвращает временную табличку tt-DataLoanInstr139 с резервом и процентами"
 &Parameters="Назначение договора,Номер договора,Дата расчета,Временная таблица"
 &Result="Значения во временной таблице tt-DataLoanInstr139"
 &Sample="RUN GetValueForInstr139 IN h_pqres (~
 'Кредит', ~
   'br_705@002', ~
   TODAY, ~
   TABLE tt-DataLoanInstr139 BY-REFERENCE)."}
PROCEDURE GetValueForInstr139:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* назначение */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT PARAM iDate     AS DATE NO-UNDO. /* дата расчета */
   DEF INPUT PARAM TABLE FOR tt-DataLoanInstr139. /* Временная таблица */
   
   DEF VAR vRsrvSumm        AS DEC   NO-UNDO.
   DEF VAR vRsrvSummProsr   AS DEC   NO-UNDO.
   DEF VAR vInterest        AS DEC   NO-UNDO. /* Начисленные проценты */
   DEF VAR vPastDueInterest AS DEC   NO-UNDO. /* Просроченные проценты */
   DEF VAR vTmpDec          AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vI               AS INT64 NO-UNDO.
   
   DEF BUFFER loan FOR loan.
   
   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF NOT AVAIL loan THEN
      RETURN.

   {empty tt-DataLoan}
   {empty tt-DataLoanInstr139}
   {empty tt-SumDolg}

   ASSIGN
      vRsrvSumm        = 0
      vRsrvSummProsr   = 0
      vInterest        = 0
      vPastDueInterest = 0
   .
      /* значение сформированного резерва */
   RUN SUMM_SPIS_RES_TYPE IN THIS-PROCEDURE (
                           loan.contract,
                           loan.cont-code,
                           "Кредит",
                           iDate,
                           ?,
                           ?,
                           "",
                           ?,
                           OUTPUT vRsrvSumm).
   
      /* значение сформированного резерва по просроченному кредиту */
   RUN SUMM_SPIS_RES_TYPE IN THIS-PROCEDURE (
                           loan.contract,
                           loan.cont-code,
                           "КредПр",
                           iDate,
                           ?,
                           ?,
                           "",
                           ?,
                           OUTPUT vRsrvSummProsr).
   
      /* Считаем начисленные проценты */                   
   DO vI = 1 TO NUM-ENTRIES(mI139_Int):
      RUN RE_PARAM IN h_Loan (INT64(ENTRY(vI, mI139_Int)),
                              iDate,
                              loan.contract,
                              loan.cont-code,
                              OUTPUT vTmpDec[1],
                              OUTPUT vTmpDec[2],
                              OUTPUT vTmpDec[3]).
                                    
      vInterest = vInterest + vTmpDec[1].
   END.
            
      /* Считаем просроченные проценты */
   DO vI = 1 TO NUM-ENTRIES(mI139_PastDueInt):
      RUN RE_PARAM IN h_Loan (INT64(ENTRY(vI, mI139_PastDueInt)),
                              iDate,
                              loan.contract,
                              loan.cont-code,
                              OUTPUT vTmpDec[1],
                              OUTPUT vTmpDec[2],
                              OUTPUT vTmpDec[3]).
                                    
      vPastDueInterest = vPastDueInterest + vTmpDec[1].
   END.
   
   CREATE tt-DataLoanInstr139.
   ASSIGN
      tt-DataLoanInstr139.contract        = loan.contract
      tt-DataLoanInstr139.cont-code       = loan.cont-code
      tt-DataLoanInstr139.rsrv-summ       = vRsrvSumm
      tt-DataLoanInstr139.rsrv-summ-prosr = vRsrvSummProsr
      tt-DataLoanInstr139.Interest        = vInterest
      tt-DataLoanInstr139.PastDueInterest = vPastDueInterest
      tt-DataLoanInstr139.Loan-Currency   = loan.currency
   .   
   
END PROCEDURE.

   /* Возвращает дату погашения по первоначальному договору и дату погашения по
   ** последнему дополнению к договору, не позднее iEndDate
   ** входящий параметр iBegDate не используется */
PROCEDURE GetLoanEndDate.
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iBegDate      AS DATE NO-UNDO. /* начало периода отбора */
   DEF INPUT  PARAM iEndDate      AS DATE NO-UNDO. /* конец периода отбора */
   DEF OUTPUT PARAM oEndDateFirst AS DATE NO-UNDO. /* Даты погашения по первоначальному договору */
   DEF OUTPUT PARAM oEndDateLast  AS DATE NO-UNDO. /* Даты погашения по последнему дополнению к договору */

   DEF VAR vSurr       AS CHAR NO-UNDO. /* Суррогат условия договора */
   DEF VAR vDateString AS CHAR NO-UNDO. /* Дата с ДР CondEndDate условия */

   DEF BUFFER loan         FOR loan.     /* Локализация буффера. */
   DEF BUFFER term-obl     FOR term-obl. /* Локализация буффера. */
   DEF BUFFER t-pro-obl    FOR pro-obl.  /* Локализация буффера. */
   DEF BUFFER b-pro-obl    FOR pro-obl.  /* Локализация буффера. */
   DEF BUFFER bterm-obl    FOR term-obl. /* Локализация буфера. */

   &SCOPED-DEFINE NO-BASE-PROC YES

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* Ищем договор */
      FIND FIRST loan WHERE loan.contract  =  iContract
                        AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE mb.

      /* Первоначальная дата погашения */
      IF IsLoanUnComLines (iContract, iContCode) THEN
         oEndDateFirst = DATE(GetXAttrValue("loan",
                                            iContract + "," + iContCode,
                                            "FinRevDate")).
      ELSE
         oEndDateFirst = loan.end-date.

      FOR EACH term-obl OF loan WHERE
               term-obl.idnt =  3
      NO-LOCK BY term-obl.end-date DESC:
         /* Инициализируем первоначальную дату. */
         oEndDateFirst = term-obl.end-date.

         pb:
         FOR EACH t-pro-obl OF loan WHERE t-pro-obl.idnt       =  3
                                      AND t-pro-obl.n-end-date =  term-obl.end-date
         NO-LOCK BY t-pro-obl.end-date:
            /* Первонач. дата погашения */
            oEndDateFirst = t-pro-obl.end-date.
            LEAVE pb.
         END.

         FIND LAST pro-obl WHERE
                   pro-obl.contract   =  iContract
            AND    pro-obl.cont-code  =  iContCode
            AND    pro-obl.pr-date    >= loan.open-date
            AND    pro-obl.pr-date    <= iEndDate
         NO-LOCK NO-ERROR.

         IF     AVAIL pro-obl
            AND CAN-FIND (FIRST bterm-obl WHERE bterm-obl.contract  =  pro-obl.contract
                                            AND bterm-obl.cont-code =  pro-obl.cont-code
                                            AND bterm-obl.idnt      =  pro-obl.idnt
                                            AND bterm-obl.end-date  =  pro-obl.n-end-date
                                            AND bterm-obl.nn        =  pro-obl.new-nn)
         THEN
         DO:

            oEndDateLast = IF pro-obl.n-end-date < iEndDate THEN pro-obl.n-end-date
                                                            ELSE term-obl.end-date.

            FIND FIRST b-pro-obl WHERE
                       b-pro-obl.contract   =  iContract
               AND     b-pro-obl.cont-code  =  iContCode
               AND     b-pro-obl.pr-date    >  iEndDate
            NO-LOCK NO-ERROR.
            IF AVAIL b-pro-obl THEN
               oEndDateLast = b-pro-obl.end-date.
         END.
         /* Если не нашли записей о пролонгации, то поищем по ДР условий */
         IF oEndDateLast =  ? THEN
         DO:
            BLK:
            FOR EACH loan-cond WHERE
                     loan-cond.contract  =  iContract
               AND   loan-cond.cont-code =  iContCode
               AND   loan-cond.since     >= loan.open-date
               AND   loan-cond.since     <= iEndDate
            NO-LOCK BY loan-cond.since DESCENDING:
               vSurr = loan-cond.contract  + "," + loan-cond.cont-code + "," + STRING(loan-cond.since).
               IF GetXAttrValue("loan-cond", vSurr, "ВидРеструкт") =  "ПролОбСрДог" THEN
               DO:
                  vDateString = GetXAttrValueEx("loan-cond", vSurr, "CondEndDate", "").
                  IF vDateString <> "" THEN
                     oEndDateLast = DATE(vDateString).
                  LEAVE BLK.
               END.
            END. /* BLK: */
         END.
         /* Нас инетересует только одна конкретная запись, поэтому выходим */
         LEAVE mb.
      END.
   END.

   &UNDEFINE NO-BASE-PROC
   RETURN.
END PROCEDURE.


/* Раччитывает число дней просрочки по основному долгу относительно
** передаваемой даты */
PROCEDURE GetLoanPrQDay.
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* дата отсчета */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* дата отсчета */
   DEF OUTPUT PARAM oQDay     AS INT64  NO-UNDO. /* Число дней просрочки по основному долгу */

   DEF VAR vResult      AS DEC   NO-UNDO.
   DEF VAR vDbOper      AS DEC   NO-UNDO.
   DEF VAR vCrOper      AS DEC   NO-UNDO.

   DEF VAR vResult1     AS DEC   NO-UNDO.   /*Непогашеный остаток просрочки*/
   DEF VAR vResult2     AS DEC   NO-UNDO.   /**/

   DEF VAR dd           AS DATE  NO-UNDO.
   DEF VAR dd2          AS DATE  NO-UNDO.
   DEF VAR vNumDays     AS INT64 NO-UNDO.   /*Количество дней просрочки*/
   DEF VAR vDate        AS DATE  NO-UNDO.
   DEF VAR vDateBegPr   AS DATE  NO-UNDO.  /* дата начала периода просрочки */

   DEF BUFFER loan-int FOR loan-int.
   DEF BUFFER loan     FOR loan.

   MAIN:
   DO ON ERROR UNDO, RETRY:

      FIND FIRST loan WHERE 
                 loan.contract  =  iContract
             AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.

      /* Если дата открытия договора меньше даты НР, тогда в качестве начала просрочки по ОД
         *** берется дата из ДР over-date_loan */
      IF AVAIL loan AND loan.open-date <  mDateNR THEN
         dd = DATE(GetXAttrValue("loan", iContract + "," + iContCode, "over-date_loan")).

      RUN STNDRT_PARAM (iContract,
                       iContCode,
                       7,
                       iEndDate,
                       OUTPUT vResult,
                       OUTPUT vDbOper,
                       OUTPUT vCrOper
                       ).

      IF vResult >  0
      THEN DO:
      /* Если дата начала просрочки еще не определена, тогда определяем её */
         IF dd =  ? THEN
         DO:
            vResult1 = 0.
            /*Считам сумму просрочки на начало периода расчета*/
            RUN STNDRT_PARAM (iContract,
                              iContCode,
                              7,
                              iBegDate,
                              OUTPUT vResult1,
                              OUTPUT vDbOper,
                              OUTPUT vCrOper
                              ).

            /*Если просрочка уже была*/
            IF vResult1 > 0 THEN
            DO:
               /* Находим дату возникновения просрочки,
               ** просто найти операцию выноса на 7-ку не достаточно, т.к. могли довыносить*/
               LOOP:
               FOR EACH loan-int WHERE
                        loan-int.contract  =  iContract
                    AND loan-int.cont-code =  iContCode
                    AND loan-int.id-d      =  7
                    AND loan-int.mdate     <= iBegDate
               NO-LOCK BY loan-int.mdate DESC:
                  /*Считам сумму просрочки на предыдущий день*/
                  RUN STNDRT_PARAM (iContract,
                                    iContCode,
                                    7,
                                    loan-int.mdate - 1,
                                    OUTPUT vResult2,
                                    OUTPUT vDbOper,
                                    OUTPUT vCrOper
                                    ).
                  IF vResult2 = 0 THEN /*Значит нашли*/
                  DO:
                     dd = loan-int.mdate.
                     LEAVE LOOP.
                  END.
               END. /*LOOP*/
            END. /*Если просрочка уже была vResult1 > 0 */
            ELSE /*Нет просрочки на начало периода, значит ищем внутри - она есть так как на конец периодо есть остатоток на 7-ке*/
            DO:
               /*Ищем первую просрочку - вынос на просрочку в заданном периоде*/
               FIND FIRST loan-int WHERE
                      loan-int.contract  =  iContract
                  AND loan-int.cont-code =  iContCode
                  AND loan-int.id-d      =  7
                  AND loan-int.mdate     >= iBegDate
                  AND loan-int.mdate     <= iEndDate
               NO-LOCK NO-ERROR.

               /*Если просрочка не найденна или на начало расчета просрочка уже была*/
               dd = IF NOT AVAIL loan-int THEN iBegDate ELSE loan-int.mdate.

            END. /*Не было просрочки на начало периода*/
         END.
         vDate = dd. /*Присваиваем начальное значение дате возникновения просрочки*/
         vDateBegPr = dd.   /* Запоминам начало периода просрочки */

         /*Зажимаемся в интервал, ищем максимальное количество дней просрочки*/
         DO WHILE dd < iEndDate :
            /*Ищем следущую операцию начисления просрочки в текущем периоде*/
            FIND FIRST loan-int WHERE
                       loan-int.contract  =  iContract
                   AND loan-int.cont-code =  iContCode
                   AND loan-int.id-d      =  7
                   AND loan-int.mdate     > dd
                   AND loan-int.mdate     <= iEndDate
            NO-LOCK NO-ERROR.

            /*Есть следующая просрочка*/
            IF AVAIL loan-int THEN
               dd2 = loan-int.mdate.
            ELSE
               /*Нет следующего выноса, значит считаем до конца периода расчета*/
               dd2 = iEndDate.

            /*Рассчитаем остаток на 7-ом на начало проверяемого периода*/

            vNumDays = 0. /*Сбросим текущее количество количество дней просрочки*/

            RUN STNDRT_PARAM (iContract,
                              iContCode,
                              7,
                              dd,
                              OUTPUT vResult1, /*Сумма, которую нужно погасить*/
                              OUTPUT vDbOper,
                              OUTPUT vCrOper
                              ).
            /*Если есть задолженность, ведь может быть операции выноса на 7-ой параметр, но оплаченная вперед... */
            IF vResult1 > 0  THEN
            DO:
               /*Идем по операциям гашения просрочки*/
               LOOP:
               FOR EACH loan-int WHERE
                        loan-int.contract  =  iContract
                    AND loan-int.cont-code =  iContCode
                    AND loan-int.id-k      =  7
                    AND loan-int.mdate     > dd
                    AND loan-int.mdate     <= dd2
               NO-LOCK:
                  /*Уменьшаем сумму просрочки на суммы операции гашения*/
                  vResult1 = vResult1 - loan-int.amt-rub.

                  /*Если удалось погасить задолженность, значит нужно посчитать сколько длилась просрочка*/
                  IF vResult1 <= 0 THEN
                  DO:
                     /*Получаем количество дней погашенной просрочки */
                     vNumDays =  loan-int.mdate - vDate .
                     LEAVE loop.
                  END. /*IF*/
               END. /*LOOP*/
            END. /*vResult1 > 0 */

            /*Если рассчитали количество дней просрочки сравниваем с предыдущими значениями для получения максимума*/
            IF vNumDays > 0 THEN
            DO:
               oQDay = IF oQDay < vNumDays THEN vNumDays ELSE oQDay.
               vDate = dd2. /*Сбрасываем дату возникновения просрочки на дату следующей операции выноса*/
            END.
            dd = dd2.
         END. /*WHILE*/

         /*Если просрочка была, а мы так и не обнаружили ни одного гашения, значит весь период длится просрочка*/
         IF oQDay = 0 THEN
            oQDay = iEndDate - vDateBegPr + 1.

      END. /*Сумма просрочки на конец периода больше нуля*/
   END. /* MAIN: DO: */
   RETURN.
END PROCEDURE.

PROCEDURE GetQRestrLoan PRIVATE.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* вид реструктуризации */
   DEF INPUT  PARAM iRestrQuan AS CHAR NO-UNDO. /* кол-во реструктуризации */
   DEF INPUT  PARAM iIndex     AS INT64 NO-UNDO. /* */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* начало периода отбора */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* конец периода отбора */
   DEF INPUT-OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* Число дней просрочки по основному долгу */

   DEF BUFFER pro-obl FOR pro-obl.

   &SCOPED-DEFINE NO-BASE-PROC YES

      /* для ПролОбСрДог сначала ищем по количеству записей pro-obl */
   IF iTypeRestr =  "ПролОбСрДог" THEN
   DO:
      FOR EACH pro-obl WHERE
               pro-obl.contract   =  iContract
         AND   pro-obl.cont-code  =  iContCode
         AND   pro-obl.pr-date    >= iBegDate
         AND   pro-obl.pr-date    <= iEndDate
      NO-LOCK:
         oQuant = oQuant + 1.
      END.
   END.
      /* Теперь берем кол-во с ДР "КолРеструкт" */
   IF     oQuant =  0
      AND iIndex <= NUM-ENTRIES(iRestrQuan) THEN
         /* Индекс количества в ДР "КолРеструкт" должен соответствовать
         ** индексу типа рестр. в ДР "ВидРеструкт" */
      ASSIGN
         oQuant = INT64(ENTRY(iIndex ,iRestrQuan))
      NO-ERROR.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

PROCEDURE GetQRestrLoanCond PRIVATE.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* вид реструктуризации */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* начало периода отбора */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* конец периода отбора */
   DEF INPUT-OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* Число дней просрочки по основному долгу */

   DEF VAR vRestrCur AS CHAR NO-UNDO. /* Тип реструкторизации на ДР условия */

   DEF BUFFER loan-cond FOR loan-cond.

   &SCOPED-DEFINE NO-BASE-PROC YES

   MAIN:
   FOR EACH loan-cond WHERE
            loan-cond.contract  =  iContract
        AND loan-cond.cont-code =  iContCode
        AND loan-cond.since     >= iBegDate
        AND loan-cond.since     <= iEndDate
   NO-LOCK ON ERROR UNDO, RETRY:
      {do-retry.i MAIN}
      vRestrCur = GetXAttrValue("loan-cond",
                                iContract + "," + 
                                iContCode + "," + 
                                STRING (loan-cond.since),
                                "ВидРеструкт").
      IF {assigned vRestrCur} THEN
         IF SortDelimList(vRestrCur, ",", NO) = SortDelimList(iTypeRestr, ",", NO) THEN
            oQuant = oQuant + 1.
   END.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

/* определение количества реструктуризаций по данному кредитному договору
**  за отчетный период. */
PROCEDURE GetQRestr.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* номер договора */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* вид реструктуризации */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* начало периода отбора */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* конец периода отбора */
   DEF OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* Число дней просрочки по основному долгу */

   DEF VAR vRestrCur    AS CHAR NO-UNDO. /* Тип реструкторизации на ДР */
   DEF VAR vRestrQuan   AS CHAR NO-UNDO. /* Список кол-в реструктуризации на ДР "КолРеструкт" */
   DEF VAR vIndex       AS INT64  NO-UNDO. /* Индекс реструктуризации в списке ДР "ВидРеструкт" на дог.*/
   DEF VAR vRestrDoc     AS CHAR NO-UNDO. /* НП РестрДог - Учитывать количество реструктуризаций на договоре и условии */

   DEF BUFFER loan      FOR loan.

   &SCOPED-DEFINE NO-BASE-PROC YES

   oQuant = 0.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
      AND     loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* Если НП РестрДог = Да, то для учета реструкторизации и количества  
            сначала анализируется ДР ВидРеструкт и ДР КолРеструкт на договоре, если они пустые, то на  условии,
            если НП РестрДог = Нет или пусто, то смотрим по-старому */
      VRestrDoc = fGetSetting ("Форма117", "РестрДог", "").

         ASSIGN
            vRestrCur  = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "ВидРеструкт",
                                         "")
            vRestrQuan = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "КолРеструкт",
                                         "")
            vIndex     = LOOKUP(iTypeRestr, vRestrCur)
         vRestrQuan = IF NOT {assigned vRestrQuan} THEN "0" ELSE vRestrQuan
         .
         /* Если НП РестрДог = ДА, то высчитываем число дней просрочки
            сначало с договора, а потом с условия */
      IF vRestrDoc =  "Да" THEN
      DO:
            /* Если на договоре ДР ВидРеструкт и КолРеструкт не пустые,
               то подсчитываем, иначе даже не заходим в процедуру */
         IF     vRestrCur  <> ""
            AND vRestrQuan <> "" THEN
            RUN GetQRestrLoan (iContract,
                               iContCode,
                               iTypeRestr,
                               vRestrQuan,
                               vIndex,
                               loan.open-date,
                               iEndDate,
                               INPUT-OUTPUT oQuant).
            IF    iTypeRestr <> "ПролОбСрДог"
               OR oQuant     =  0 THEN
               RUN GetQRestrLoanCond (iContract,
                                      iContCode,
                                      iTypeRestr,
                                      loan.open-date,
                                      iEndDate,
                                      INPUT-OUTPUT oQuant).
      END.
         /* Если НП РестрДог = НЕТ или ПУСТО, 
            то высчитываем число дней просрочки по старому алгоритму */
      ELSE
      DO:
         RUN GetQRestrLoanCond (iContract,
                                iContCode,
                                iTypeRestr,
                                loan.open-date,
                                iEndDate,
                                INPUT-OUTPUT oQuant).
         IF oQuant =  0 THEN
            RUN GetQRestrLoan (iContract,
                               iContCode,
                               iTypeRestr,
                               vRestrQuan,
                               vIndex,
                               loan.open-date,
                               iEndDate,
                               INPUT-OUTPUT oQuant).
      END.
   END.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

/* Выполняет расчет суммы процентов, предназначеных к выплате в указаном периоде
** значение возвразается в валюте договора */
PROCEDURE GetSumPlanProc.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* Дата начала периода */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* Дата окончания периода */
   DEF INPUT  PARAM iBegF125   AS DATE NO-UNDO. /* Дата начала периода расчета ф.125 */
   DEF INPUT  PARAM iEndF125   AS DATE NO-UNDO. /* Дата окончания периода расчета ф.125*/
   DEF OUTPUT PARAM oSumProc   AS DEC  NO-UNDO. /* Сумма плановых платежей */
   
   DEF VARIABLE vUseHist AS LOGICAL INIT NO NO-UNDO.

   DEF BUFFER term-obl FOR term-obl. /* Локализация буфера. */
   DEF BUFFER loan     FOR loan.     /* Локализация буфера. */
   DEF BUFFER term-obl-hist FOR term-obl-hist.     /* Локализация буфера. */

   FIND FIRST loan WHERE 
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   
   IF AVAIL loan THEN 
   DO:
      IF NOT (loan.class-code  =  "own-bill-liability"
           OR loan.class-code  =  "dsc-bill-asset") THEN 
      DO:
         IF FGetSetting("Форма125", "ПроцУчИстГраф", "ДА")   =  "ДА" THEN
            vUseHist = CAN-FIND(
               FIRST term-obl-hist WHERE
                     term-obl-hist.contract  =  loan.contract
                 AND term-obl-hist.cont-code =  loan.cont-code
                 AND term-obl-hist.idnt      =  1
                 AND term-obl-hist.since     >= iEndF125).
         
         IF vUseHist THEN
            RUN SUMM_PROC2 IN THIS-PROCEDURE
                              (BUFFER loan,
                              iEndF125,
                              iBegDate,
                              iEndDate,
                              OUTPUT oSumProc).
         ELSE
            RUN SUMM_PROC IN THIS-PROCEDURE 
                            (BUFFER loan,
                             iEndF125,
                             iBegDate,
                             iEndDate,
                             OUTPUT oSumProc).
      END.
      ELSE /* векселя */
         RUN bprocper.p (iContract,
                         iContCode,
                         iBegF125,
                         iEndF125,
                         "Ext",
                         OUTPUT oSumProc).
   END.
   
   IF oSumProc <  0 OR oSumProc =  ? THEN oSumProc = 0.
   
END PROCEDURE.

/* Новая функция для расчета ставки по договору. Если нет дополнительных выплат
** возвращает значение ставки %Кред, иначе значение ЭПС по договору.
** Дата последнего начисления %% на балансе */
FUNCTION fCalcEpsLoan RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vList-Comm AS CHAR NO-UNDO.
   DEF VAR vCounter   AS INT64  NO-UNDO.
   DEF VAR vComm      AS CHAR NO-UNDO.
   DEF VAR vCardOver  AS CHAR NO-UNDO.
   DEF VAR vCalcEPS   AS LOG  NO-UNDO.
   DEF VAR vEPS       AS DEC  NO-UNDO.
   DEF VAR vAddPay    AS LOG  NO-UNDO.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER loan       FOR loan.
   DEF BUFFER loan-acct  FOR loan-acct.
   DEF BUFFER bloan-acct FOR loan-acct.
   DEFINE BUFFER term-obl FOR term-obl.

   MAIN:
   DO:
      vCalcEPS = FALSE.
      CHK:
      DO:
         vList-Comm =  FGetSetting("ЭПС", "ЭПСКмс" ,"").  /* получаем значение НП */
         REPEAT vCounter = 1 TO NUM-ENTRIES(vList-Comm):
            vComm = ENTRY(1, ENTRY(vCounter, vList-Comm), "=").
            FIND FIRST comm-rate WHERE
                       comm-rate.acct       =  "0"
                   AND comm-rate.commission =  vComm
                   AND comm-rate.kau        =  iContract + "," + iContCode
                   AND comm-rate.since      <= iDate
            NO-LOCK NO-ERROR.
            IF AVAILABLE comm-rate THEN
            DO:
               vCalcEPS = TRUE.
               LEAVE CHK.
            END.
         END.
         FOR EACH loan WHERE
                  loan.contract         =  "Страх"
            AND   loan.parent-cont-code =  iContCode
            AND   loan.parent-contract  =  iContract
            AND   loan.open-date        <= iDate
            AND  (loan.close-date       >= iDate
               OR loan.close-date       =  ?)
         NO-LOCK,
             EACH term-obl OF loan WHERE
                  term-obl.idnt =  1
         NO-LOCK:
            vCalcEPS = TRUE.
            LEAVE CHK.
         END.
         vCardOver = GetXattrValueEx("loan",
                                     iContract + "," + iContCode,
                                     "КартОвер",
                                     "").
         IF vCardOver =  "Разреш" THEN
         DO:
            FIND LAST loan-acct WHERE
                      loan-acct.acct-type =  "КредРасч"
                  AND loan-acct.cont-code =  iContCode
                  AND loan-acct.contract  =  iContract
                  AND loan-acct.since     <= iDate
            NO-LOCK NO-ERROR.
            IF AVAILABLE loan-acct THEN
            DO:
               vCalcEPS = CAN-FIND (LAST bloan-acct WHERE
                                         bloan-acct.acct      =  loan-acct.acct
                                     AND bloan-acct.acct-type BEGINS "SCS@"
                                     AND bloan-acct.since     <= iDate
                                    NO-LOCK).
            END.
         END. /* IF vCard-Over EQ "Разреш" THEN */
      END. /* CHK: DO: */
      IF vCalcEPS THEN
      DO:
         RUN pGetEpsLoan IN h_loan (iContract,
                                    iContCode,
                                    iDate,
                                    OUTPUT vEPS,
                                    OUTPUT vAddPay).
         IF vAddPay THEN
         DO:
            vEPS = vEPS * 100.
            LEAVE MAIN.
         END.
      END.
      vEPS = GET_COMM_LOAN (iContract,iContCode,"%Кред",iDate).
   END. /* MAIN: DO: */
   RETURN vEPS.
END FUNCTION.

   /* ЭПС рассчитывается по следующей формуле %Кред + %ОткрЛим + 12*КомСч */
FUNCTION fCalcEpsSpec RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):
   DEF VAR vCredRate AS DEC  NO-UNDO. /* ставка "%Кред" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* ставка "%ОткрЛим" */
   DEF VAR vComAcct  AS DEC  NO-UNDO. /* ставка "КомСч" */
   DEF VAR vEPS      AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%Кред"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%ОткрЛим"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "КомСч"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComAcct = comm-rate.rate-comm.
   vEPS = vCredRate + vOpenLim + 12 * vComAcct.
   RETURN vEPS.
END FUNCTION.

{pfuncdef 
   &DefFunc = "RecalcFixedToProc"
   &Description = "Пересчитывает фиксированную комиссию в % от суммы договора"
   &Parameters = "Назначение договора,Номер договора,Сумма комисии"
   &Result= "Значение в %"
   &Sample="RecalcFixedToProc(Кредит,123,10000)"}
   
FUNCTION RecalcFixedToProc RETURNS DECIMAL PRIVATE 
   (INPUT iContract AS CHARACTER,
    INPUT iContCode AS CHARACTER,
    INPUT iSummComm AS DECIMAL):

   DEFINE BUFFER term-obl FOR term-obl.
   
   FIND FIRST term-obl WHERE term-obl.contract  =  iContract
                         AND term-obl.cont-code =  iContCode
                         AND term-obl.idnt      =  2
   NO-LOCK NO-ERROR.

   RETURN IF AVAILABLE term-obl 
          THEN iSummComm / term-obl.amt-rub * 100
          ELSE 0.       
END FUNCTION.

{pfuncdef 
   &DefFunc = "fCalcEpsSpec1"
   &Description = "Ф117ПроцЭПС  ЭПС рассчитывается по следующей формуле %Кред + %Выд  Ф117ПроцЭПС"
   &Parameters = "Назначение договора,Номер договора,Дата"
   &Result= "Значение ЭПС"
   &Sample="fCalcEpsSpec1(Кредит,123,01/01/16)"}
   
FUNCTION fCalcEpsSpec1 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vCredRate AS DEC  NO-UNDO. /* ставка "%Кред" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* ставка "%Выд" */
   DEF VAR oSumEPS   AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%Кред"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%Выд"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = IF comm-rate.rate-fixed 
                 THEN RecalcFixedToProc(iContract,iContCode,comm-rate.rate-comm)
                 ELSE comm-rate.rate-comm.
   oSumEPS = vCredRate + vOpenLim .
   RETURN oSumEPS.
END FUNCTION.

{pfuncdef 
   &DefFunc = "fCalcEpsSpec2"
   &Description = "Ф117ПроцЭПС ЭПС рассчитывается по следующей формуле %Кред + %Выд*365/колДнейДоговора"
   &Parameters = "Назначение договора,Номер договора,Дата"
   &Result= "Значение ЭПС"
   &Sample="fCalcEpsSpec2(Кредит,123,01/01/16)"}

FUNCTION fCalcEpsSpec2 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vCredRate AS DEC  NO-UNDO. /* ставка "%Кред" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* ставка "%Выд" */
   DEF VAR vLoanDays AS INT64 NO-UNDO .
   DEF VAR oSumEPS      AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER loan       FOR loan.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%Кред"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%Выд"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = IF comm-rate.rate-fixed 
                 THEN RecalcFixedToProc(iContract,iContCode,comm-rate.rate-comm)
                 ELSE comm-rate.rate-comm.
   FIND FIRST loan WHERE
              loan.contract  =   iContract
          AND loan.cont-code =   iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
      vLoanDays = loan.end-date - loan.open-date.
   
   IF vLoanDays = 0
   THEN
      oSumEPS = 0 .
   ELSE
      oSumEPS = vCredRate + vOpenLim * 365 / vLoanDays .
   RETURN oSumEPS.
END FUNCTION.

/* Процентная ставка по просроченной части ссуды рассчитывается по следующей формуле 
  %КрПр + %КомЛим + 12*КомСч */
FUNCTION fCalcPrpsSpec RETURNS DEC
   (iContract AS CHAR,
   iContCode AS CHAR,
   iDate     AS DATE):
   DEF VAR vCrPrRate AS DEC NO-UNDO. /* ставка "%КрПр" */
   DEF VAR vComLim   AS DEC NO-UNDO. /* ставка "%КомЛим" */
   DEF VAR vComAcct  AS DEC NO-UNDO. /* ставка "КомСч" */
   DEF VAR vPenyK    AS DEC NO-UNDO. /* ставка "Пеня-К" */
   DEF VAR vPenyAcct AS DEC NO-UNDO. /* ставка "ПеняКомСч" */
   DEF VAR vPrPS     AS DEC NO-UNDO.

   DEF BUFFER comm-rate FOR comm-rate.

   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "%КрПр"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCrPrRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "%КомЛим"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComLim = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "КомСч"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComAcct = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "Пеня-К"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vPenyK = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "ПеняКомСч"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vPenyAcct = comm-rate.rate-comm.

   vPrPS = IF vCrPrRate <> ? THEN
               vCrPrRate + vComLim + 12 * vComAcct
           ELSE
               (vPenyK + vPenyAcct) * (DATE(1, 1, YEAR(iDate) + 1) - DATE(1, 1, YEAR(iDate))).

   RETURN vPrPS.
END FUNCTION.

/* Процедура по классу договора определяет какую функцию для расчета ЭПС
** необходимо запустить, запускает ее, возвращает рассчитанную ставку ЭПС */
PROCEDURE pCalcEps117.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iClassCode AS CHAR NO-UNDO. /* Класс договора */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO. /* Дата рассчета */
   DEF OUTPUT PARAM oSum       AS DEC  NO-UNDO. /* Сумма */

   DEF VAR vProcName AS CHAR NO-UNDO. /* имя вызываемой процедуры */

   DEF BUFFER term-obl FOR term-obl. /* Локализация буфера. */

   vProcName = GetXAttrInit(iClassCode, "Ф117ПроцЭПС").
   IF NOT {assigned vProcName} THEN
      vProcName = fGetSetting("Ф117ПроцЭПС","","fCalcEpsLoan").
   IF NOT {assigned vProcName} THEN
      vProcName = "fCalcEpsLoan".
/* Вставка Плюс банк */
   FIND FIRST loan
      WHERE  loan.contract  = iContract
        AND  loan.cont-code = iContCode
        AND (loan.cont-type = 'Потреб'
          OR loan.cont-type = 'КЗ'
          OR loan.cont-type = ?)
      NO-LOCK NO-ERROR.
   IF AVAIL loan THEN vProcName = "PscMortgage".
/* Конец вставки Плюс банк */
   oSum = DYNAMIC-FUNCTION(vProcName, iContract, iContCode, iDate) NO-ERROR.
END PROCEDURE.

/*  */

{pfuncdef 
    &DefProc="fCalcMarga"
    &Description="Получение вариационной маржи по счету скорее здесь - Получение вариационной маржи по клиенту"}
FUNCTION fCalcMarga RETURNS DEC
   (iAcct AS CHAR,
    iCurrency AS CHAR,
    iDateBeg  AS DATE,
    iDateEnd AS DATE):

   DEF VAR vRetVal AS DEC NO-UNDO.
   DEF VAR vVM     AS DEC NO-UNDO.

   DEF BUFFER acct FOR acct. /* Локализация буфера. */
   DEF BUFFER loan FOR loan. /* Локализация буфера. */

   MAIN:
   DO:
      {find-act.i
         &bact = acct
         &acct = iAcct
         &curr = iCurrency
      }
      IF NOT AVAIL acct THEN
         LEAVE main.
      FOR EACH loan WHERE
               loan.cust-cat =  acct.cust-cat
           AND loan.cust-id  =  acct.cust-id
           AND loan.contract =  "Позиция"
      NO-LOCK:
         RUN pCalcMarga IN h_loanx (loan.contract,
                                    loan.cont-code,
                                    iDateBeg,
                                    iDateEnd,
                                    NO,          /* не пересчитывать ВМ */
                                    OUTPUT vVM).
         vRetVal = vRetVal + vVM.
      END.
   END.
   RETURN vRetVal.
END FUNCTION.

/* Определяет:
   - признак наличия просрочки по договору (любой даже по внебалансу)+ максимальный срок просрочки
   - если есть сумму просрочки по ссуде
   - сумму ссудной задолжности
old version <= D66
*/
PROCEDURE CalcDebt115.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.   /* Назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.   /* Номер договора */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO.   /* Дата расчета */
   DEF OUTPUT PARAM oProcFl   AS LOG    NO-UNDO.   /* признак наличия просрочки по договору (любой даже по внебалансу) */
   DEF OUTPUT PARAM oQDaysPr  AS INT64    NO-UNDO.   /* максимальный срок просрочки (количество дней просрочки) */
   DEF OUTPUT PARAM oCredSum  AS DEC    NO-UNDO.   /* сумма ссудной задолженности */
   DEF OUTPUT PARAM oCrPrSum  AS DEC    NO-UNDO.   /* сумма просрочки по ссуде */

   DEF VAR vDate    AS DATE   NO-UNDO. /* Промежуточная дата */
   DEF VAR vProcPar AS CHAR   NO-UNDO INIT "7,10,48,210,248". /* Параметры, по которым определяется наличие просрочки */
   DEF VAR vCredPar AS CHAR   NO-UNDO INIT "0,13".            /* Параметры, по которым определяется сумма ссудной задолженности */

   DEF VAR vParSum  AS DEC   NO-UNDO. /* сумма параметра */
   DEF VAR vDb      AS DEC   NO-UNDO.
   DEF VAR vCr      AS DEC   NO-UNDO.
   DEF VAR vi       AS INT64 NO-UNDO.
   DEF VAR vPar     AS INT64 NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* Локализация буфера. */

   MAIN_BLOCK:
   DO:
            /* Сумма судной задолженности */
      DO vi = 1 TO NUM-ENTRIES(vCredPar):
         vPar = INT64(ENTRY(vi,vCredPar)).
         RUN RE_PARAM IN h_Loan (
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
         oCredSum = oCredSum + vParSum.
      END.

               /* признак наличия просрочки по договору (любой даже по внебалансу) */
      oProcFl = LN_GetParamsInteres (iContract,
                                     iContCode,
                                     vProcPar,
                                     iDate) >  0.
      IF NOT oProcFl THEN LEAVE MAIN_BLOCK.

               /* Определяем дату начала просрочки */
      BLCK:
      FOR EACH loan-int WHERE
               loan-int.contract    =  iContract
         AND   loan-int.cont-code   =  iContCode
         AND   loan-int.mdate       <= iDate
         AND  (CAN-DO(vProcPar, STRING (loan-int.id-d))
            OR CAN-DO(vProcPar, STRING (loan-int.id-k)))
      NO-LOCK
      BY loan-int.mdate DESCENDING:
        vDate = loan-int.mdate.
        IF LN_GetParamsInteres (iContract,
                                iContCode,
                                vProcPar,
                                loan-int.mdate - 1) <= 0
        THEN LEAVE BLCK.
      END.
            /* количество дней просрочки */
      oQDaysPr = iDate - vDate.
   END.

   RETURN.
END PROCEDURE.


/* Определяет:
   - признак наличия просрочки по договору (любой даже по внебалансу)+ максимальный срок просрочки
   - если есть сумму просрочки по ссуде
   - сумму ссудной задолжности
New version > D66
re_proc_name+NO  считает сумму по параметру без разноски.

*/
PROCEDURE CalcDebt115a.
   DEF INPUT  PARAMETER iContract AS CHAR   NO-UNDO.   /* Назначение договора */
   DEF INPUT  PARAMETER iContCode AS CHAR   NO-UNDO.   /* Номер договора */
   DEF INPUT  PARAMETER iDate     AS DATE   NO-UNDO.   /* Дата расчета */
   DEF INPUT-OUTPUT PARAMETER ioProcFl   AS LOG    NO-UNDO. /* признак наличия просрочки по договору (любой даже по внебалансу) */
   DEF INPUT-OUTPUT PARAMETER ioQDaysPr  AS INT64  NO-UNDO. /* максимальный срок просрочки (количество дней просрочки)          */
   DEF OUTPUT PARAMETER oSumZ1 AS DECIMAL NO-UNDO .    /* сумма ссудной задолжности    */
   DEF OUTPUT PARAMETER oSumZ2 AS DECIMAL NO-UNDO .    /* сумма задолжности коммисии   */
   DEF OUTPUT PARAMETER oSumZ3 AS DECIMAL NO-UNDO .    /* сумма задолжности пенни      */
   DEF OUTPUT PARAMETER oSumZ4 AS DECIMAL NO-UNDO .    /* сумма задолжности требования */
   DEF OUTPUT PARAMETER oSumP1 AS DECIMAL NO-UNDO .    /* сумма просроченной ссудной задолжности    */
   DEF OUTPUT PARAMETER oSumP2 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности коммисии   */
   DEF OUTPUT PARAMETER oSumP3 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности пенни      */
   DEF OUTPUT PARAMETER oSumP4 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности требования */

   DEF VAR vDate          AS DATE  NO-UNDO. /* Промежуточная дата */
   DEF VAR vParSum        AS DEC   NO-UNDO. /* сумма параметра */
   DEF VAR vDb            AS DEC   NO-UNDO.
   DEF VAR vCr            AS DEC   NO-UNDO.
   DEF VAR vi             AS INT64 NO-UNDO.
   DEF VAR vPar           AS INT64 NO-UNDO.
   DEF VAR vCrcDate       AS DATE  NO-UNDO.
   DEF VAR vPrsProcDate   AS DATE  NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* Локализация буфера. */
   DEF BUFFER loan     FOR loan.
   DEF BUFFER code     FOR code.

   DEFINE VARIABLE oSumm     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE oSummRub  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE oCurr     AS CHARACTER NO-UNDO.

   ASSIGN
      oSumZ1   = 0
      oSumZ2   = 0
      oSumZ3   = 0
      oSumZ4   = 0
      oSumP1   = 0
      oSumP2   = 0
      oSumP3   = 0
      oSumP4   = 0
      vCrcDate = LastWorkDay(iDate)
   .

   MAIN_BLOCK:
   DO:
   FIND FIRST loan NO-LOCK where
              loan.contract = iContract AND
              loan.cont-code = iContCode  NO-ERROR .


    /* Сумма  задолженности */
      DO vi = 1 TO NUM-ENTRIES(mParz1):
         vPar = INT64(ENTRY(vi,mParz1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */

              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ1 = oSumZ1 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParz2):
         vPar = INT64(ENTRY(vi,mParz2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ2 = oSumZ2 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParz3):
         vPar = INT64(ENTRY(vi,mParz3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ3 = oSumZ3 + vParSum.
      END.
      DO vi = 1 TO NUM-ENTRIES(mParz4):
         vPar = INT64(ENTRY(vi,mParz4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ4 = oSumZ4 + vParSum.
      END.

      IF ioProcFl <> YES THEN 
      DO:   
            /* Определение наличие просрочки - ОДИН раз для кредитной линии */
         RUN LN_GetPrsDate(iContract,
                           ENTRY (1, iContCode, " "),
                           iDate,
                           mProcPar,
                           mAlgFIFO,
                           mCurrDay,
                           OUTPUT vPrsProcDate,
                           OUTPUT ioProcFl).

         ioQDaysPr = iDate - vPrsProcDate.
      END.

         /* Даже если просрочки нет на всякий случай чесно посчитаем параметры  */
         /* Сумма просроченной  задолженности */
      DO vi = 1 TO NUM-ENTRIES(mParp1):
         vPar = INT64(ENTRY(vi,mParp1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump1 = oSump1 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParp2):
         vPar = INT64(ENTRY(vi,mParp2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump2 = oSump2 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParp3):
         vPar = INT64(ENTRY(vi,mParp3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump3 = oSump3 + vParSum.
      END.
      DO vi = 1 TO NUM-ENTRIES(mParp4):
         vPar = INT64(ENTRY(vi,mParp4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).


         oSump4 = oSump4 + vParSum.
      END.
   END.

   RETURN.
END PROCEDURE.

/* аналог CalcDebt115а с возможностью переопределения параметров */
PROCEDURE CalcDebt115b.
   DEF INPUT  PARAMETER iContract AS CHAR   NO-UNDO.   /* Назначение договора */
   DEF INPUT  PARAMETER iContCode AS CHAR   NO-UNDO.   /* Номер договора */
   DEF INPUT  PARAMETER iDate     AS DATE   NO-UNDO.   /* Дата расчета */
   DEF INPUT-OUTPUT PARAMETER ioProcFl   AS LOG    NO-UNDO. /* признак наличия просрочки по договору (любой даже по внебалансу) */
   DEF INPUT-OUTPUT PARAMETER ioQDaysPr  AS INT64  NO-UNDO. /* максимальный срок просрочки (количество дней просрочки)          */
   DEF OUTPUT PARAMETER oSumZ1 AS DECIMAL NO-UNDO .    /* сумма ссудной задолжности    */
   DEF OUTPUT PARAMETER oSumZ2 AS DECIMAL NO-UNDO .    /* сумма задолжности коммисии   */
   DEF OUTPUT PARAMETER oSumZ3 AS DECIMAL NO-UNDO .    /* сумма задолжности пенни      */
   DEF OUTPUT PARAMETER oSumZ4 AS DECIMAL NO-UNDO .    /* сумма задолжности требования */
   DEF OUTPUT PARAMETER oSumP1 AS DECIMAL NO-UNDO .    /* сумма просроченной ссудной задолжности    */
   DEF OUTPUT PARAMETER oSumP2 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности коммисии   */
   DEF OUTPUT PARAMETER oSumP3 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности пенни      */
   DEF OUTPUT PARAMETER oSumP4 AS DECIMAL NO-UNDO .    /* сумма просроченной задолжности требования */
   DEF INPUT  PARAMETER iParz1 AS CHAR NO-UNDO .    /* список параметров : сумма ссудной задолжности    */
   DEF INPUT  PARAMETER iParz2 AS CHAR NO-UNDO .    /* список параметров : сумма задолжности коммисии   */
   DEF INPUT  PARAMETER iParz3 AS CHAR NO-UNDO .    /* список параметров : сумма задолжности пенни      */
   DEF INPUT  PARAMETER iParz4 AS CHAR NO-UNDO .    /* список параметров : сумма задолжности требования */
   DEF INPUT  PARAMETER iParp1 AS CHAR NO-UNDO .    /* список параметров : сумма просроченной ссудной задолжности    */
   DEF INPUT  PARAMETER iParp2 AS CHAR NO-UNDO .    /* список параметров : сумма просроченной задолжности коммисии   */
   DEF INPUT  PARAMETER iParp3 AS CHAR NO-UNDO .    /* список параметров : сумма просроченной задолжности пенни      */
   DEF INPUT  PARAMETER iParp4 AS CHAR NO-UNDO .    /* список параметров : сумма просроченной задолжности требования */
   DEF INPUT  PARAMETER iProcPar  AS CHAR NO-UNDO .  /* Параметры, по которым определяется наличие просрочки */
   DEF INPUT  PARAMETER iLineLoan AS LOG NO-UNDO .   /* максимальная просрочка на : договоре - NO, кредитной линии - YES */
   DEF OUTPUT PARAMETER oMsgChar  AS CHARACTER NO-UNDO . /* Протокол */

   DEFINE VARIABLE mTmpParz1        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz2        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz3        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz4        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp1        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp2        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp3        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp4        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpProcPar      AS CHARACTER NO-UNDO .
   DEFINE VARIABLE vTmpProcPar1     AS CHARACTER NO-UNDO .

   DEF VAR vDate     AS DATE   NO-UNDO. /* Промежуточная дата */
   DEF VAR vParSum   AS DEC    NO-UNDO. /* сумма параметра */
   DEF VAR vDb       AS DEC    NO-UNDO.
   DEF VAR vCr       AS DEC    NO-UNDO.
   DEF VAR vi        AS INT64  NO-UNDO.
   DEF VAR vPar      AS INT64  NO-UNDO.
   DEF VAR vCrcDate  AS DATE   NO-UNDO.
   DEF VAR vTmpDec   AS DEC    NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* Локализация буфера. */
   DEF BUFFER loan     FOR loan.

   DEFINE VARIABLE oSumm     AS DECIMAL NO-UNDO .
   DEFINE VARIABLE oSummRub  AS DECIMAL NO-UNDO .
   DEFINE VARIABLE oCurr     AS CHARACTER NO-UNDO .


   DEF BUFFER bbloan FOR loan.
   DEF BUFFER bbbloan FOR loan.

   ASSIGN
      mTmpParz1 = (IF iParz1 >  "" THEN iParz1 ELSE mParz1)
      mTmpParz2 = (IF iParz2 >  "" THEN iParz2 ELSE mParz2)
      mTmpParz3 = (IF iParz3 >  "" THEN iParz3 ELSE mParz3)
      mTmpParz4 = (IF iParz4 >  "" THEN iParz4 ELSE mParz4)
      mTmpParp1 = (IF iParp1 >  "" THEN iParp1 ELSE mParp1)
      mTmpParp2 = (IF iParp2 >  "" THEN iParp2 ELSE mParp2)
      mTmpParp3 = (IF iParp3 >  "" THEN iParp3 ELSE mParp3)
      mTmpParp4 = (IF iParp4 >  "" THEN iParp4 ELSE mParp4)
      mTmpProcPar = (IF iProcPar >  "" THEN iProcPar ELSE mProcPar)
      vTmpProcPar1 = REPLACE(mTmpProcPar, ";", ",")
      oSumZ1   = 0
      oSumZ2   = 0
      oSumZ3   = 0
      oSumZ4   = 0
      oSumP1   = 0
      oSumP2   = 0
      oSumP3   = 0
      oSumP4   = 0
      vCrcDate = LastWorkDay(iDate)
   .

   MAIN_BLOCK:
   DO:
   FIND FIRST loan NO-LOCK where
              loan.contract = iContract AND
              loan.cont-code = iContCode  NO-ERROR .


    /* Сумма  задолженности */
      IF NOT (mTmpParz1 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz1):
         vPar = INT64(ENTRY(vi,mTmpParz1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */

              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ1 = oSumZ1 + vParSum.
      END.

      IF NOT (mTmpParz2 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz2):
         vPar = INT64(ENTRY(vi,mTmpParz2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ2 = oSumZ2 + vParSum.
      END.

      IF NOT (mTmpParz3 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz3):
         vPar = INT64(ENTRY(vi,mTmpParz3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ3 = oSumZ3 + vParSum.
      END.

      IF NOT (mTmpParz4 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz4):
         vPar = INT64(ENTRY(vi,mTmpParz4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSumZ4 = oSumZ4 + vParSum.
      END.


      IF ioProcFl =  ? THEN /* Определение наличие просрочки - ОДИН раз для кредитной линии */
      DO:         
          
          
         ioProcFl = FALSE .
         IF iLineLoan THEN /* наличия просрочки на кредитной линии */
         DO:
            /* признак наличия просрочки по договору (любой даже по внебалансу) */
            FOR FIRST bbloan where bbloan.contract  =  iContract
                               and bbloan.cont-code =  entry(1,iContCode ," " ) NO-LOCK :
               IF  LN_GetParamsInteres (iContract,
                                        bbloan.Cont-Code,
                                        vTmpProcPar1,
                                        iDate) >  0
               THEN
               DO:  /* Есть просрочка по охватывающему */
                 ioProcFl = true .
                 LEAVE.
               END.
               ELSE
               DO: /* Может есть просрочка по траншам */
                  FOR EACH bbbloan where bbbloan.contract = iContract
                                     and bbbloan.cont-code BEGINS  entry(1,iContCode ," " ) + " "
                     NO-LOCK:
                     IF  LN_GetParamsInteres (iContract,
                                              bbbloan.Cont-Code,
                                              vTmpProcPar1,
                                              iDate) >  0
                     THEN DO:
                        ioProcFl = true .
                        LEAVE.
                     END.
                  END.
               END.
            END.
         END.
         ELSE /* наличия просрочки на договоре */
         DO:
            FOR FIRST bbloan where bbloan.contract  =  iContract
                               and bbloan.cont-code =  iContCode NO-LOCK :
               IF  LN_GetParamsInteres (iContract,
                                        bbloan.Cont-Code,
                                        vTmpProcPar1,
                                        iDate) >  0
               THEN
               DO:  /* Есть просрочка по договору */
                 ioProcFl = true .
               END.
            END.
         END.

         /* Определяем дату начала просрочки */
         ioQDaysPr = 0.
         IF ioProcFl THEN
         DO:
                      /* Определение наличие просрочки - ОДИН раз для кредитной линии */
            
            RUN LN_GetPrsDate(iContract,
                              ENTRY (1, iContCode, " "),
                              iDate,
                              mTmpProcPar,
                              mAlgFIFO,
                              mCurrDay,
                              OUTPUT vDate,
                              OUTPUT ioProcFl).
                                        
            ioQDaysPr = iDate - vDate. 
             
            {empty ttLoanInt}
            IF iLineLoan THEN
            DO:
               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code =  entry(1,iContCode ," ")
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.

               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code BEGINS entry(1,iContCode ," " ) + " "
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.
            END.
            ELSE
            DO:
               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code =  iContCode
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.
            END.

            FOR EACH ttLoanInt BY ttloanInt.mdate DESCENDING:
               vDate   = ttLoanInt.mdate.
               vTmpDec = LN_GetParamsInteres (iContract,
                                       ttLoanInt.cont-code,
                                       vTmpProcPar1,
                                       ttLoanInt.mdate - 1).

               oMsgChar = oMsgChar + ttLoanInt.cont-code + " " + STRING(vDate,"99/99/9999") + " (" + STRING(vDate - 1,"99/99/9999") + " : " + STRING(vTmpDec) + ")~n".
               IF vTmpDec <= 0
               THEN
               DO:
                  /* количество дней просрочки */                  
                  oMsgChar  = oMsgChar + ttLoanInt.cont-code + " " + STRING(iDate,"99/99/9999") + " - " + STRING(vDate,"99/99/9999") + " = " + STRING(ioQDaysPr) + "~n".
                  LEAVE.
               END.
            END.
         END.
      END. /* IF ioProcFl EQ ? THEN */

     /* Даже если просрочки нет на всякий случай чесно посчитаем параметры  */

    /* Сумма просроченной  задолженности */
      IF NOT (mTmpParp1 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp1):
         vPar = INT64(ENTRY(vi,mTmpParp1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump1 = oSump1 + vParSum.
      END.

      IF NOT (mTmpParp2 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp2):
         vPar = INT64(ENTRY(vi,mTmpParp2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump2 = oSump2 + vParSum.
      END.

      IF NOT (mTmpParp3 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp3):
         vPar = INT64(ENTRY(vi,mTmpParp3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).

         oSump3 = oSump3 + vParSum.
      END.

      IF NOT (mTmpParp4 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp4):
         vPar = INT64(ENTRY(vi,mTmpParp4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* Тип договора */
              iContCode,            /* Номер договора */
              vPar,                 /* Код параметра */
              iDate,                /* Дата расчета */
              OUTPUT vParSum,       /* Сумма параметра */
              OUTPUT vDb,           /* Дебет обороты */
              OUTPUT vCr).          /* Кредитовые обороты */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* Если для расчета задана валюта отличная от национальной,
                переведем сумму в заданную валюту */
            IF oCurr <> "" THEN
               vParSum = CurToBase("Учетный",oCurr,vCrcDate,vParSum).


         oSump4 = oSump4 + vParSum.
      END.
   END.

   RETURN.
END PROCEDURE.

/* Возвращает сумму по видам операций и при наличии значения ДР на договоре.
   Сумма возвращается только в том случае, если есть какое-либо значение на ДР,
   в противном случае в качестве результата будет возвращен ? */
FUNCTION GetPercntLoanDR RETURNS DECIMAL
   (iContract  AS CHARACTER,  /* Назначение договора. */
   iContCode  AS CHARACTER,  /* Номер договора. */
   iListIdOp  AS CHARACTER,  /* Виды операций. */
   iDR        AS CHARACTER,  /* Код дополнительного реквезита. */
   iDate      AS DATE): /* Дата окончания расчета, на которую необходимо расчитать сумму по переданным операциям. */

   DEFINE VARIABLE vSumm     AS DECIMAL   INIT 0 NO-UNDO. /* Сумма по операциям */
   DEFINE VARIABLE vI        AS INT64     NO-UNDO. 
   DEFINE VARIABLE vDR       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTemporal AS LOG       NO-UNDO. /* Признак темпорированности ДР */

   DEFINE BUFFER loan FOR loan.
   DEFINE BUFFER bclass FOR class.
   DEFINE BUFFER xattr FOR xattr.

   /* Ищем договор */
   FIND FIRST loan WHERE
      loan.contract  =  iContract
      AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.
   /* Если договор не найден, возвращаем ? */
   IF NOT AVAILABLE loan THEN
      RETURN ?.

   /* Проверяем признак темпорированности переданного ДР */
   BLK:
   FOR EACH bclass WHERE 
      bclass.PROGRESS-CODE =  loan.class-code 
      NO-LOCK,
      FIRST xattr WHERE 
         xattr.Class-Code =  bclass.CLASS-CODE
         AND xattr.Xattr-Code =  iDR
         NO-LOCK:
      vTemporal = xattr.temporal. 
      LEAVE BLK.
   END.
   /* Если ДР темпорированный, то берем значения ДР на указанную дату,
      если нет, то просто берем его значение */
   IF vTemporal THEN
      vDR = GetTempXAttrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         iDR,
         iDate,
         ?).
   ELSE
      vDR = GetXAttrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         iDR,
         ?).
   /* Если ДР не заполнен, возвращаем в качестве результата ? */
   IF vDR =  ? THEN
      RETURN ?.

   /* Считаем сумму по переданным видам операций */
   DO vI = 1 TO NUM-ENTRIES(iListIdOp):      
      RUN pint.p (iContract, iContCode, loan.open-date, iDate, ENTRY(vI, iListIdOp)).
      FOR EACH otch1:
         vSumm = vSumm + otch1.summ_pr .
      END.
   END.

   RETURN vSumm.

END FUNCTION.

/* Расчет задолженностей по траншам, если %% ведутся на охватывающем договоре и для траншей разные %% ставки */
PROCEDURE GET_PROP_PROC_TR:
    DEF INPUT PARAM iContract   AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode   AS CHAR NO-UNDO.
    DEF INPUT PARAM iEndDate    AS DATE NO-UNDO.
    DEF OUTPUT PARAM table FOR tres.
    
    DEF VAR vOstProc AS DEC NO-UNDO.
    
    DEF BUFFER loan FOR loan.
    
    {empty tres}
    /* остаток задолженности по %% на охватывающем договоре */
    RUN SetSysConf IN h_base ("AddParams","10,29,34,48").
    RUN GetProcentUnAccount (iContract,
                             iContcode,
                             iEndDate,
                             OUTPUT vOstProc).
    RUN DeleteOldDataProtocol IN h_base("AddParams").
    /* получаем полную сумму начисленных %% по каждому траншу */
    RUN SetSysConf IN h_Base("СводныйГрафикПроцентов","Да").
    FOR EACH loan WHERE loan.contract  =  iContract 
                    AND loan.cont-code BEGINS iContcode + " "
                    AND NUM-ENTRIES(loan.cont-code," ") >  1
        NO-LOCK BY loan.open-date DESC:
        CREATE tres.
        ASSIGN
           tres.cont-code = loan.cont-code
           tres.od        = ROUND(LnPrincipal (loan.contract, loan.cont-code,iEndDate,""),2)
           tres.open-date = loan.open-date. 
        RUN pint.p (loan.contract, loan.cont-code, loan.open-date + 1, iEndDate, "4").
        FOR EACH otch1:
            ACCUM otch1.summ_pr (TOTAL) .
        END.
        ASSIGN
           tres.proc = MIN(ACCUM TOTAL  otch1.summ_pr, vOstProc)
           vOstProc  = vOstProc - tres.proc.    
    END.
    RUN DeleteOldDataProtocol IN h_base("СводныйГрафикПроцентов").
    
END PROCEDURE. 
    
/*
Ищем счёта по коду счёта второго порядка, если находит - то возвращает сумму остатков найденых счетов на указанную дату в рублях
*/
PROCEDURE GET_SUMM_BS2:
   DEF INPUT  PARAM iContract         AS CHAR     NO-UNDO. 
   DEF INPUT  PARAM iContCode         AS CHAR     NO-UNDO.
   DEF INPUT  PARAM iDate             AS DATE     NO-UNDO. /*Дата по которой брать остаток*/
   DEF INPUT  PARAM iAcct             AS INT64    NO-UNDO. /*Счёт второго порядка*/
   DEF INPUT  PARAM iFindTransh       AS LOGICAL  NO-UNDO. /*Искать только на охватывающем договоре(нет) или ещё и на траншах(да)*/
   DEF OUTPUT PARAM oIsAvailability   AS LOGICAL  NO-UNDO. /*Наличие(да) \ отсутствие(нет) счёта*/
   DEF OUTPUT PARAM oBalance          AS DEC      NO-UNDO. /*Остотко на дату iDate на найденом счете в рублях*/
   
   DEF BUFFER bloan-acct FOR loan-acct.
   
   /*Чистим темповую табличку со счетами*/
   {empty ttAcct}
   
   oIsAvailability = NO.
   oBalance = 0.
   /*Ищем договор,*/
   FOR EACH loan WHERE
            loan.contract  =  iContract
        AND loan.cont-code =  iContCode 
   NO-LOCK,
       /*Находим действующий счет договора с указаным БС2*/    
       EACH loan-acct WHERE
            loan-acct.contract  =  loan.contract
        AND loan-acct.cont-code =  loan.cont-code
        AND loan-acct.currency  =  loan.currency
        AND loan-acct.since     <= iDate
        /*Проверяем нет ли счёта с таокй же ролью на более позднюю дату*/
        AND NOT CAN-FIND(LAST bloan-acct WHERE
                              bloan-acct.contract  =  loan.contract
                          AND bloan-acct.cont-code =  loan.cont-code
                          AND bloan-acct.acct-type =  loan-acct.acct-type
                          AND bloan-acct.since     >  loan-acct.since
                          AND bloan-acct.since     <= iDate 
                         NO-LOCK)
   NO-LOCK,
       FIRST acct WHERE
             acct.acct     =  loan-acct.acct
         AND acct.currency =  loan.currency
         AND acct.bal-acct =  iAcct
         /*Проверяем не обрабатывалили мы этот счёт ранее*/
         AND NOT CAN-FIND(FIRST ttAcct WHERE
                                ttAcct.acct =  acct.acct 
                          NO-LOCK)
   NO-LOCK BY loan-acct.since:
      /*Запоминаем счёт*/
      CREATE ttAcct.
      ttAcct.acct = acct.acct.
      /*Счёт нашёлся !*/
      oIsAvailability = YES.
      /*Вычисляем остаток на счёте*/      
      RUN Acct-Pos-Pure IN h_base (acct.acct, 
                              loan.currency, 
                              iDate, 
                              iDate, 
                              CHR(251)).
      /*Счетов может быть несколько, поэтому суммируем результат(в рублях)*/
      oBalance  = oBalance + IF loan.currency =  "" THEN sh-bal
                                                    ELSE CurToBase("Учетный",loan.currency,iDate,sh-val).
   END.
   /*Если искать также и на траншах*/
   IF iFindTransh THEN
      /*то ищем на траншах*/
      FOR EACH loan WHERE
               loan.contract  =                 iContract
           AND loan.cont-code BEGINS            iContCode + " "
           AND NUM-ENTRIES(loan.cont-code, " ") =  2
      NO-LOCK,
      /*Находим действующий счет договора с указаным БС2*/    
          EACH loan-acct WHERE
               loan-acct.contract  =  loan.contract
           AND loan-acct.cont-code =  loan.cont-code
           AND loan-acct.currency  =  loan.currency
           AND loan-acct.since     <= iDate
           /*Проверяем нет ли счёта с таокй же ролью на более позднюю дату*/
           AND NOT CAN-FIND(LAST bloan-acct WHERE
                                 bloan-acct.contract  =  loan.contract
                             AND bloan-acct.cont-code =  loan.cont-code
                             AND bloan-acct.acct-type =  loan-acct.acct-type
                             AND bloan-acct.since     >  loan-acct.since 
                             AND bloan-acct.since     <= iDate
                            NO-LOCK)
      NO-LOCK,
          FIRST acct WHERE
                acct.acct     =  loan-acct.acct
            AND acct.currency =  loan.currency
            AND acct.bal-acct =  iAcct
            /*Проверяем не обрабатывалили мы этот счёт ранее*/
            AND NOT CAN-FIND(FIRST ttAcct WHERE
                                   ttAcct.acct =  acct.acct 
                             NO-LOCK)
      NO-LOCK BY loan-acct.since:
         /*Запоминаем счёт*/
         CREATE ttAcct.
         ttAcct.acct = acct.acct.
         /*Счёт нашёлся !*/
         oIsAvailability = YES.
         /*Вычисляем остаток на счёте*/      
         RUN Acct-Pos-Pure IN h_base (acct.acct, 
                                 loan.currency, 
                                 iDate, 
                                 iDate, 
                                 CHR(251)).
         /*Счетов может быть несколько, поэтому суммируем результат(в рублях)*/
         oBalance  = oBalance + IF loan.currency =  "" THEN sh-bal
                                                       ELSE CurToBase("Учетный",loan.currency,iDate,sh-val).
      END.  
END PROCEDURE.

/* Информация обо всех действующих на заданную дату договорах обеспечения.
   Данные помещаются в tt_getinfdogob.def */
PROCEDURE GetInfDogOb:

   DEFINE INPUT PARAMETER iContract AS  CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS  CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDate     AS  DATE NO-UNDO.
   DEFINE INPUT PARAMETER TABLE     FOR tt-term-obl-gar BIND.

   DEFINE VARIABLE vSurr AS CHARACTER NO-UNDO.

   FOR EACH term-obl WHERE term-obl.contract  =  iContract
      AND term-obl.cont-code =  iContCode
      AND term-obl.idnt      =  5
      AND term-obl.end-date  >= iDate
      AND term-obl.fop-date  <= iDate
      NO-LOCK:

      vSurr = term-obl.contract + "," + term-obl.cont-code + "," + STRING(term-obl.idnt) + "," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn).

      CREATE tt-term-obl-gar.
      ASSIGN
         tt-term-obl-gar.mContract       = term-obl.contract
         tt-term-obl-gar.mCont-code      = term-obl.cont-code
         tt-term-obl-gar.mNomdogob       = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "НомДогОб")
         tt-term-obl-gar.mViddogob       = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "ВидДогОб")
         tt-term-obl-gar.mVidob          = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "ВидОб")
         tt-term-obl-gar.mKawcestvoobesp = Get_QualityGar("term-obl",
                                                       vSurr,
                                                       iDate)
         .
   END.
                                 
END PROCEDURE.

{pfuncdef 
&DefProc="GetTTSpis"}

PROCEDURE GetTTSpis:
   DEFINE OUTPUT PARAMETER TABLE FOR ttSpis.
END PROCEDURE.

{pfuncdef 
&DefProc="Soot"
&Description="Соотношение ОД и ССЗ"}

PROCEDURE Soot:

   DEFINE INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAM oSummOd     AS DEC  NO-UNDO.
   DEFINE OUTPUT PARAM oSSZ        AS DEC  NO-UNDO.
   DEFINE OUTPUT PARAM oSoot       AS DEC  NO-UNDO.

   DEFINE VARIABLE vParam0db       AS DEC  NO-UNDO.
   DEFINE VARIABLE vParam0cr       AS DEC  NO-UNDO.
   DEFINE VARIABLE vAcct           AS CHAR NO-UNDO.
   DEFINE VARIABLE vSummObStr      AS DEC  NO-UNDO.
   DEFINE VARIABLE vSumStr         AS CHAR NO-UNDO.
   DEFINE VARIABLE vSurrTObl       AS CHAR NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER insurance FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int.
   DEFINE BUFFER term-obl  FOR term-obl.
   DEFINE BUFFER signs     FOR signs.

   ASSIGN
      oSummOd    = 0
      oSSZ       = 0
      vSummObStr = 0
   .

   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:
      
      FIND FIRST loan-int WHERE 
                 loan-int.contract  =  loan.contract
             AND loan-int.cont-code =  loan.cont-code 
             AND loan-int.id-d      =  0
             AND loan-int.id-k      =  3 
      NO-LOCK NO-ERROR.

      IF AVAIL loan-int THEN
      DO:
         
         vAcct   = ENTRY(1, LN_GetAcctSurr (loan.Class-Code,
                                            loan.contract,
                                            loan.cont-code,
                                            loan.open-date)).

         oSSz    = LnGetProvAcctSpr(vAcct,
                                    loan-int.mdate,
                                    YES). 

         RUN STNDRT_PARAM IN h_loan (loan.contract,
                                     loan.cont-code,
                                     0,
                                     loan-int.mdate,
                                     OUTPUT oSummOd,
                                     OUTPUT vParam0db,
                                     OUTPUT vParam0cr).

         T-OBL:
         FOR EACH term-obl WHERE 
                  (    term-obl.contract   =  loan.contract
                              AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                              AND term-obl.idnt       =  5
                              AND term-obl.end-date   >  loan-int.mdate 
                              AND term-obl.fop-date   <= loan-int.mdate )
                          OR
                             (    term-obl.contract   =  loan.contract
                              AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                              AND term-obl.idnt       =  5
                              AND term-obl.end-date   =  ? 
                              AND term-obl.fop-date   <= loan-int.mdate )
         NO-LOCK:
            
            vSurrTObl = GetSurrogateBuffer("term-obl",(BUFFER term-obl:HANDLE)).
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "ВидДогОб",
                               "") <> "КредОб" THEN NEXT T-OBL.
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "ОД-СумСтр",
                               "Нет") =  "Да" THEN
            DO:
               vSumStr = GetXattrValueEx("term-obl", 
                                         vSurrTObl,
                                         "SumStr",
                                         ""). 
                
               IF {assigned vSumStr} THEN
                  vSummObStr = vSummObStr + DEC(vSumStr).
               ELSE
               DO:
                  vSumStr = "0".
                  
                  FOR EACH insurance WHERE
                           insurance.contract         =  "СТРАХ"
                       AND insurance.parent-contract  =  term-obl.contract
                       AND insurance.parent-cont-code =  term-obl.cont-code
                  NO-LOCK,
                     FIRST signs WHERE
                           signs.file-name   =  "loan"
                       AND signs.surrogate   =  insurance.contract + "," + 
                                                insurance.cont-code
                       AND signs.code        =  "Страхобесп"
                       AND signs.xattr-value =  vSurrTObl
                  NO-LOCK:
                     UpdateSignsEx(term-obl.class-code,
                                   vSurrTObl,
                                   "SumStr",
                                   STRING(term-obl.amt-rub)) NO-ERROR.
                     vSumStr = STRING(term-obl.amt-rub).
                  END.
                  
                  vSummObStr = vSummObStr + DEC(vSumStr). 
               END.
            END. 
         END.
      END.
   END.

   oSoot = (oSummOd - vSummObStr) / oSSZ.
   
   IF oSoot <  0 THEN oSoot = 0.

END PROCEDURE.

{pfuncdef 
&DefProc="SummStrah"
&Description="Сумма страхования"}

PROCEDURE SummStrah:

   DEFINE INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAM oSummStrah  AS DEC  NO-UNDO.

   DEFINE VARIABLE vVidOb          AS CHAR NO-UNDO.
   DEFINE VARIABLE vSurrTObl       AS CHAR NO-UNDO.
   DEFINE VARIABLE vSumStr         AS CHAR NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER insurance FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int.
   DEFINE BUFFER term-obl  FOR term-obl.
   DEFINE BUFFER signs     FOR signs.

   oSummStrah = 0.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:

      FIND FIRST loan-int WHERE 
                 loan-int.contract  =  loan.contract
             AND loan-int.cont-code =  loan.cont-code 
             AND loan-int.id-d      =  0
             AND loan-int.id-k      =  3 
      NO-LOCK NO-ERROR.

      IF AVAIL loan-int THEN
      DO:

         T-OBL:
         FOR EACH term-obl WHERE 
                  (    term-obl.contract   =  loan.contract
                   AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                   AND term-obl.idnt       =  5
                   AND term-obl.end-date   >  loan-int.mdate 
                   AND term-obl.fop-date   <= loan-int.mdate )
               OR (    term-obl.contract   =  loan.contract
                   AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                   AND term-obl.idnt       =  5
                   AND term-obl.end-date   =  ? 
                   AND term-obl.fop-date   <= loan-int.mdate )
         NO-LOCK:
            
            vSurrTObl = GetSurrogateBuffer("term-obl",(BUFFER term-obl:HANDLE)).
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "ВидДогОб",
                               "") <> "КредОб" THEN NEXT T-OBL.
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "ОД-СумСтр",
                               "Нет") =  "Да" THEN
            DO:
               vSumStr = GetXattrValueEx("term-obl", 
                                         vSurrTObl,
                                         "SumStr",
                                         ""). 
                
               IF {assigned vSumStr} THEN
                  oSummStrah = oSummStrah + DEC(vSumStr).
               ELSE
               DO:
                  vSumStr = "0".
                  
                  FOR EACH insurance WHERE
                           insurance.contract         =  "СТРАХ"
                       AND insurance.parent-contract  =  term-obl.contract
                       AND insurance.parent-cont-code =  term-obl.cont-code
                  NO-LOCK,
                     FIRST signs WHERE
                           signs.file-name   =  "loan"
                       AND signs.surrogate   =  insurance.contract + "," + 
                                                insurance.cont-code
                       AND signs.code        =  "Страхобесп"
                       AND signs.xattr-value =  vSurrTObl
                  NO-LOCK:
                     UpdateSignsEx(term-obl.class-code,
                                   vSurrTObl,
                                   "SumStr",
                                   STRING(term-obl.amt-rub)) NO-ERROR.
                     vSumStr = STRING(term-obl.amt-rub).
                  END.
                  
                  oSummStrah = oSummStrah + DEC(vSumStr).
               END.
            END.
         END.
      END.
   END.
END PROCEDURE.

/* ============================================================================== */
/* == Возвращает сурогат договора в прошлой валюте, если смены небыло то пусто == */
/* ============================================================================== */
{pfuncdef 
   &DefProc="ChangeCurrency"
   &Description="Сурогат договор, до смены валюты"}
FUNCTION ChangeCurrency RETURNS CHAR
   (iLoanSurr AS CHAR):   
   
   DEF VAR vResult AS CHAR NO-UNDO.
   
   vResult = GetXattrValueEx("loan", 
                             iLoanSurr,
                             "ИзмВалюты",
                             "").
                             
   IF {assigned vResult} THEN
      vResult = "Кредит," + vResult. 
   
   RETURN vResult.

END FUNCTION.
{pfuncdef 
&DefProc="GetSummGarInRes"
&Description="Получение величины от каждого принятого обеспечения, ~
   на которые уменьшается величина расчетного резерва ~
   по договору при резервировании"}
PROCEDURE GetSummGarInRes:
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* Назначение договора */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* Номер договора */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO. /* Дата, на которую собираются данные */
   DEFINE OUTPUT PARAMETER TABLE FOR ttGarRes.
   
   DEF VAR vRsrvRate  AS DEC   NO-UNDO.
   DEF VAR vGrRiska   AS INT64 NO-UNDO.
   DEF VAR vTermSur   AS CHAR  NO-UNDO.
   DEF VAR vKatKach   AS CHAR  NO-UNDO.
   DEF VAR vIndKatKac AS INT64 NO-UNDO.
   DEF VAR vOstSotim  AS DEC   NO-UNDO.
   DEF VAR vListOp    AS CHAR  NO-UNDO INIT "33,32,137,136,471,470,474,473,320,321,426,427,339,340".
   DEF VAR vPrevDate  AS DATE  NO-UNDO.
   DEF VAR vI         AS INT64 NO-UNDO.
   
   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER chowhe   FOR chowhe.
   DEF BUFFER loan-int FOR loan-int.
   
   {empty ttGarRes}
   
   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   
   IF NOT AVAIL loan THEN
      RETURN.
   
      /* Если дата расчета не передана, то ищет на глобальную дату */
   IF iSince =  ? THEN
      iSince = gend-date.
   
   vPrevDate = iSince.
    
      /* находим самую последнюю операцию, из списка vListOp на договоре */
   DO vI = 1 TO NUM-ENTRIES(vListOp):
      FOR FIRST chowhe WHERE
                chowhe.id-op =  INT64(ENTRY(vI,vListOp))
      NO-LOCK:
         FOR LAST loan-int WHERE
                  loan-int.contract  =  loan.contract 
              AND loan-int.cont-code =  loan.cont-code
              AND loan-int.id-k      =  chowhe.id-k
              AND loan-int.id-d      =  chowhe.id-d
              AND loan-int.op-date   <= vPrevDate
         NO-LOCK by loan-int.id-k:
               /* если есть дата предыдущего урегулирования резерва, то пляшем от неё */ 
            IF    vPrevDate >  loan-int.op-date
               OR vPrevDate =  ? THEN
               vPrevDate = loan-int.op-date.
         END.
             
         FOR LAST loan-int WHERE
                  loan-int.contract  =  loan.contract 
              AND loan-int.cont-code =  loan.cont-code
              AND loan-int.id-d      =  chowhe.id-d
              AND loan-int.id-k      =  chowhe.id-k
              AND loan-int.op-date   <= vPrevDate
         NO-LOCK by loan-int.id-d :
               /* если есть дата предыдущего урегулирования резерва, то пляшем от неё */ 
            IF    vPrevDate >  loan-int.op-date
               OR vPrevDate =  ? THEN
               vPrevDate = loan-int.op-date.
         END.
      END.
   END.
       
      /* если раньше ничё не регулировали, то берём дату предыдущего месяца */
   IF vPrevDate =  ? THEN
   DO: 
      vPrevDate = LastMonDate(GoMonth(iSince,-1)).   
           
         /* Если предыдущая дата месяца, больше даты открытия договора */
      IF vPrevDate <= loan.open-date THEN
         vPrevDate = loan.open-date. /* то берём дату открытия договора */
   END.
   
      /* Коэф. резервирования */
   RUN GetRateGrRisk IN h_i254 (loan.contract,
                                loan.cont-code,
                                vPrevDate,
                                OUTPUT vRsrvRate,
                                OUTPUT vGrRiska ).
      /* Бежим по обеспечениям */
   FOR EACH term-obl WHERE 
            term-obl.contract  =  loan.contract
        AND term-obl.cont-code =  loan.cont-code
        AND term-obl.idnt      =  5
        AND term-obl.fop-date  <= vPrevDate
        AND (   term-obl.sop-date  =  ?
             OR term-obl.sop-date  >  vPrevDate )
   NO-LOCK:

      vTermSur = STRING(term-obl.contract + "," + 
                        term-obl.cont-code + ",5," + 
                        STRING(term-obl.end-date) + "," + 
                        STRING(term-obl.nn)).
                        
         /* Находим категорию качества */               
      vKatKach = Get_QualityGar ("comm-rate",vTermSur, vPrevDate).

      IF    vKatKach =  "?"
         OR vKatKach =  ?
      THEN
         vKatKach = "".

      IF vKatKach =  "" THEN
         vIndKatKac = 0 .
      ELSE
         vIndKatKac = INT64(GetCode("КачествоОбесп",vKatKach)).

          /* Находим процент остаточной стоимости */
      vOstSotim = GET_COMM("Обесп",
                           ?,
                           term-obl.currency,
                           vTermSur,
                           0.00,
                           0,
                           vPrevDate).
      IF vOstSotim = ? THEN vOstSotim = 100.

      CREATE ttGarRes.
      ASSIGN
         ttGarRes.fSurr = vTermSur
         ttGarRes.fNumOb  = GetXAttrValueEx("term-obl",
                                            vTermSur,
                                            "НомДогОб",
                                            "")
         ttGarRes.fVidOb   = GetXAttrValueEx("term-obl",
                                             vTermSur,
                                             "ВидДогОб",
                                             "")
         ttGarRes.fSumm   = LnPledge(term-obl.contract,
                                     term-obl.cont-code,
                                     term-obl.idnt,
                                     term-obl.end-date,
                                     term-obl.nn,
                                     vPrevDate,
                                     ?,
                                     term-obl.currency ) * vIndKatKac / 100 *
                            vOstSotim / 100 * vRsrvRate / 100
         ttGarRes.fCurrency = term-obl.currency
         ttGarRes.fLastDate = vPrevDate
      .
      
      IF {assigned ttGarRes.fCurrency} THEN
         ttGarRes.fSummRub = CurToBase("УЧЕТНЫЙ", ttGarRes.fCurrency, vPrevDate, ttGarRes.fSumm).
      ELSE
         ttGarRes.fSummRub = ttGarRes.fSumm.
   END.                             
      
END PROCEDURE.

{pfuncdef 
&DefProc="GetLongestDelayForClient"
&Description="Получает набиольшую просрочку по клиенту, параметр Тип клиента, Идентификатор клиента, дата расчёта"}
FUNCTION GetLongestDelayForClient RETURNS INT64
  (iCust-cat  AS CHAR,  /* Тип клиента */
   iCust-id   AS INT64,  /* Идентификатор клиента */
   iDate      AS DATE)  /* Дата конца расчета */
:
   DEF VAR vLonger     AS INT64 NO-UNDO.
   DEF VAR vResDate    AS DATE  NO-UNDO.
   DEF VAR vProsrFl    AS LOG   NO-UNDO.

   DEF BUFFER loan FOR loan.
   
   vLonger = 0.
   
   FOR EACH loan WHERE
           (loan.cust-cat   =  iCust-cat
        AND loan.cust-id    =  iCust-id
        AND loan.contract   =  "Кредит"
        AND loan.close-date >  iDate)
        OR 
           (loan.cust-cat   =  iCust-cat
        AND loan.cust-id    =  iCust-id
        AND loan.contract   =  "Кредит"
        AND loan.close-date =  ?)
   NO-LOCK:
       
      RUN LN_GetPrsDate IN h_cdrep (loan.contract,
                                    ENTRY (1, loan.cont-code, " "),
                                    iDate,
                                    mProcPar139,
                                    NO /* Алгоритм расчёта - всегда не ФИФО */, 
                                    YES /* Текущий день всегда учитывать */,
                                    OUTPUT vResDate,
                                    OUTPUT vProsrFl).
      vLonger = MAX (vLonger , (iDate - vResDate)).
   END.
   
   RETURN vLonger.

END FUNCTION.

{pfuncdef 
   &DefProc="IsLoanPlavStav"
   &Description="Определяет, есть ли действующая на дату плавающая ставка на договоре"}
PROCEDURE IsLoanPlavStav: 
   
   DEF INPUT  PARAM iSurr         AS CHAR NO-UNDO.
   DEF INPUT  PARAM iBegDate      AS DATE NO-UNDO.
   DEF INPUT  PARAM iEndDate      AS DATE NO-UNDO.
   DEF OUTPUT PARAM oDatePlavStav AS DATE NO-UNDO.
   DEF OUTPUT PARAM oMessError    AS CHAR NO-UNDO INIT "".
   
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-cond FOR comm-cond.
   
   DEF VAR vContract AS CHAR NO-UNDO.
   DEF VAR vContCode AS CHAR NO-UNDO.
   DEF VAR vBegDate  AS DATE NO-UNDO.
   DEF VAR vEndDate  AS DATE NO-UNDO.

   MAIN-BLOCK:
      DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
         
      IF NUM-ENTRIES(iSurr) <  2 THEN
      DO:
         oMessError = SUBSTITUTE("&1. Ошибка в суррогате договора", iSurr). 
         LEAVE MAIN-BLOCK. 
      END.
      
      /* Если плавающие ставки ведутся на охвате, то ищем охватывающий договор */
      ASSIGN
         vContract = ENTRY(1,iSurr)
         vContCode = ENTRY(2,iSurr)
         vContCode = IF FGetSetting("ПлавСтавОхват",?,"нет") =  "Да" 
                     THEN ENTRY(1,vContCode," ")
                     ELSE vContCode 
      . 
         
      /* Найдем договор */
      FIND FIRST loan WHERE 
                 loan.contract  =  vContract
             AND loan.cont-code =  vContCode  
      NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN 
      DO:
         oMessError = SUBSTITUTE("&1. Не найден договор", iSurr).
         LEAVE MAIN-BLOCK.          
      END.
      
      ASSIGN 
         vBegDate = IF iBegDate =  ? THEN loan.open-date ELSE iBegDate
         vEndDate = IF iEndDate =  ? THEN loan.end-date  ELSE iEndDate
      . 
      
      RUN GetLastDateFloatRateInPer IN THIS-PROCEDURE (
         BUFFER loan, 
         vBegDate, 
         vEndDate,  
         OUTPUT oDatePlavStav).
         
   END. /* MAIN-BLOCK */
   
END PROCEDURE.


/* Получает последнюю плановую дату изменения плавающей ставки в указанном периоде */
PROCEDURE GetLastDateFloatRateInPer PRIVATE:
   
   DEF PARAM BUFFER loan      FOR loan.
   
   DEF INPUT PARAM iBegDate AS DATE NO-UNDO.
   DEF INPUT PARAM iEndDate AS DATE NO-UNDO.
   
   DEF OUTPUT PARAM oLastDate AS DATE NO-UNDO INIT ?.
   
   DEF VAR vDate AS DATE NO-UNDO.
   
   DEF BUFFER comm-cond FOR comm-cond.
   
   MAIN-BLOCK:
   DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND LAST comm-cond WHERE
                comm-cond.contract   =  loan.contract
            AND comm-cond.cont-code  =  loan.cont-code
            AND comm-cond.commission =  "%Кред"
            AND comm-cond.since      <= iEndDate
      NO-LOCK NO-ERROR.
      
      /* Если последняя ставка не плавающая, то выходим */
      IF NOT AVAIL comm-cond OR 
        (AVAIL comm-cond 
         AND NOT comm-cond.FloatType) THEN 
         LEAVE MAIN-BLOCK.
      
      /* Получаем первую плановую дату изменения ставки с начала периода iBegDate */
      RUN GetFirstDateFloatRate IN THIS-PROCEDURE (
         BUFFER loan,
         BUFFER comm-cond, 
         iBegDate, 
         OUTPUT vDate).
         
      /* Если полученная дата больше конца периода, то в периоде ставка 
         не менялась - выходим */
      IF vDate = ? 
      OR (vDate <> ? 
      AND vDate > iEndDate) THEN 
         LEAVE MAIN-BLOCK.
      
      CL:
      DO WHILE (vDate <> ?) AND (vDate < iEndDate):
         
         oLastDate = vDate.
         
         /* Если достигли окончания договора, то выходим */   
         IF vDate > loan.end-date THEN LEAVE CL.

         /* Получаем следующую плановую дату изменения ставки */
         RUN GetFirstDateFloatRate IN THIS-PROCEDURE (
            BUFFER loan,
            BUFFER comm-cond, 
            vDate + 1, /* Наращиваем дату */
            OUTPUT vDate).
      END.  
         
   END. /* MAIN-BLOCK */
   
END PROCEDURE.

   /* Вычисляет расчетный резерв на траншевых договорах и производит коррекцию округления */
{pfuncdef 
   &DefProc="GetResRasch"
   &Description="Вычисляет расчетный резерв на траншевых договорах и производит коррекцию округления"}
PROCEDURE GetResRasch:
   
   DEF PARAM BUFFER loan FOR loan.
   DEF INPUT PARAM  iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oSum  AS DEC  NO-UNDO.
   
   DEF VAR vRes      AS DEC   NO-UNDO.
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vSum      AS DEC   NO-UNDO.
   DEF VAR vMax      AS DEC   NO-UNDO.
   DEF VAR vContCode AS CHAR  NO-UNDO.
   DEF VAR vRec      AS RECID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   
   DEF BUFFER bloan FOR loan.
   
   MB:
   DO ON ERROR UNDO MB, LEAVE MB:
         /* ищем уже сформированный результат */
      FIND FIRST tt-ResRasch WHERE 
                 tt-ResRasch.cont-code =  loan.cont-code
             AND tt-ResRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
         /* если не найдено, то заполняем таблицу */
      IF NOT AVAIL(tt-ResRasch) THEN
      DO:
         {empty tt-ResRasch}
         vContCode = ENTRY(1, loan.cont-code, " ").
         vDif = LnFormRsrv (loan.contract,        
                            vContCode,
                            iDate,
                            "").
         CREATE tt-ResRasch.
         ASSIGN
            tt-ResRasch.cont-code = vContCode
            tt-ResRasch.end-date  = iDate
            tt-ResRasch.summa     = vDif
         .
         FOR EACH bloan WHERE bloan.contract  =  loan.contract 
                          AND bloan.cont-code BEGINS vContCode + " "
                          AND NUM-ENTRIES(bloan.cont-code, " ") >  1
         NO-LOCK:
            vRes = LnFormRsrvTransh (bloan.contract,        
                                     bloan.cont-code,
                                     iDate,
                                     "").
            vRes = ROUND(vRes, 2).
            CREATE tt-ResRasch.
            ASSIGN
               tt-ResRasch.cont-code = bloan.cont-code
               tt-ResRasch.end-date  = iDate
               tt-ResRasch.summa     = vRes
            .
            IF vDif <> 0 THEN
               vDif = vDif - vRes.
               /* запоминаем транш с самой большой суммой */
            RUN RE_PARAM_EX IN h_Loan ("0",
                                       iDate,
                                       bloan.since,
                                       bloan.contract,
                                       bloan.cont-code,
                                       OUTPUT vTmpDec[1], /* значение */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            vSum = vTmpDec[1].
            IF vSum >  vMax THEN
               ASSIGN
                  vRec = RECID(tt-ResRasch)
                  vMax = vSum
               .
         END.
            /* коррекция ошибки округления */
         IF vDif <> 0 THEN
         DO:
            FIND FIRST tt-ResRasch WHERE RECID(tt-ResRasch) =  vRec NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-ResRasch THEN /* какая-то фигня случилась */
               LEAVE MB.
            tt-ResRasch.summa = tt-ResRasch.summa + vDif.
         END.
      END. /* IF NOT AVAIL(tt-ResRasch) THEN */
         /* ищем уже сформированный результат */
      FIND FIRST tt-ResRasch WHERE 
                 tt-ResRasch.cont-code =  loan.cont-code
             AND tt-ResRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-ResRasch) THEN
         LEAVE MB.
      oSum = tt-ResRasch.summa.
   END. /* MB */
END PROCEDURE.


 /* Вычисляет расчетное обеспечение на траншевых договорах и производит коррекцию округления */
{pfuncdef 
   &DefProc="GetGarRasch"
   &Description="Вычисляет расчетное обесп.на траншевых договорах и производит коррекц.округл."}
PROCEDURE GetGarRasch:
   
   DEF PARAM BUFFER loan FOR loan.
   DEF INPUT PARAM  iDate   AS DATE NO-UNDO.
   DEF INPUT PARAM  iKK     AS CHAR NO-UNDO.
   DEF INPUT PARAM  iKndAmt AS LOG NO-UNDO.
   DEF OUTPUT PARAM oSum    AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oSumQ   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oVid    AS CHAR NO-UNDO.
   
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vSum      AS DEC   NO-UNDO.
   DEF VAR vMax      AS DEC   NO-UNDO.
   DEF VAR vContCode AS CHAR  NO-UNDO.
   DEF VAR vRec      AS RECID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vAmtAll   AS DEC   NO-UNDO.
   DEF VAR vGarAll   AS DEC   NO-UNDO.
   DEF VAR vKK       AS CHAR  NO-UNDO.
   DEF VAR vVidOb    AS CHAR  NO-UNDO.
   DEF VAR vVidAll   AS CHAR  NO-UNDO.
   DEF VAR vAmt      AS DEC   NO-UNDO.
   DEF VAR vSurr     AS CHAR  NO-UNDO.
   DEF VAR vQSum     AS DEC   NO-UNDO.
   DEF VAR vGQIndex  AS DEC   NO-UNDO.
   DEF VAR vDecrRate AS DEC   NO-UNDO.
   DEF VAR vGarAllQ  AS DEC   NO-UNDO.
   
   DEF BUFFER bloan    FOR loan.
   DEF BUFFER term-obl FOR term-obl.
  
   MB:
   DO ON ERROR UNDO MB, LEAVE MB:
         /* ищем уже сформированный результат */
      FIND FIRST tt-GarRasch WHERE 
                 tt-GarRasch.cont-code =  loan.cont-code
             AND tt-GarRasch.end-date  =  iDate
             AND tt-GarRasch.KK        =  iKK
      NO-LOCK NO-ERROR.
         /* если не найдено, то заполняем таблицу */
      IF NOT AVAIL(tt-GarRasch) THEN
      DO:
         {empty tt-GarRasch}
         ASSIGN 
            vContCode = ENTRY(1, loan.cont-code, " ").
            vGQIndex  = DEC(GetCode("КачествоОбесп", iKK ))
         .
         IF vGQIndex  = ? THEN vGQIndex  = 0.

         /* Общая сумма обеспечения */
         lGar:
         FOR EACH term-obl WHERE
                  term-obl.contract  = loan.contract
              AND term-obl.cont-code BEGINS vContCode + " "
              AND NUM-ENTRIES(term-obl.cont-code, " ") >  1
              AND term-obl.idnt      = 5
              AND (term-obl.sop-date >  iDate
                OR term-obl.sop-date =  ?) 
         NO-LOCK:
            vSurr = GetSurrogate("term-obl",ROWID(term-obl)).
            vKK = Get_QualityGar ("term-obl",
                                  vSurr,
                                  iDate).
            IF vKK <> iKK THEN 
               NEXT lGar.
            vDecrRate = GET_COMM("Обесп",
                              ?,
                              term-obl.currency,
                              vSurr,
                              0.00,
                              0,
                              iDate).
            IF vDecrRate = ? THEN vDecrRate = 100.


            vVidOb = Get_VidObespech (vSurr,
                                      iDate).
            {additem.i vVidAll vVidOb}
            
            IF iKndAmt THEN
            DO:
               vAmt = DEC(GetTempXAttrValueEx("term-obl",
                                              vSurr,
                                              "РынСтоим",
                                              iDate,
                                              "0")) NO-ERROR.
                                           
               IF term-obl.currency <> "" THEN
                  vAmt = CurToBase("УЧЕТНЫЙ",
                                   term-obl.currency,
                                   iDate,
                                   vAmt).
            END.
            ELSE
               vAmt  = LnPledge (term-obl.contract,        
                                 term-obl.cont-code,
                                 term-obl.idnt,
                                 term-obl.end-date,
                                 term-obl.nn,
                                 iDate,
                                 iDate,
                                 "").
            ASSIGN 
               vDif  = vDif + vAmt
               vQSum = vQSum + ROUND(vAmt * vGQIndex / 100 * vDecrRate / 100,2)
            .
         END.
         lGar2:
         FOR EACH term-obl WHERE                  
                  term-obl.contract  = loan.contract
              AND term-obl.cont-code = vContCode
              AND term-obl.idnt      = 5
             AND (term-obl.sop-date >  iDate
               OR term-obl.sop-date =  ?) 
         NO-LOCK:
            vSurr = GetSurrogate("term-obl",ROWID(term-obl)).
            vKK = Get_QualityGar ("term-obl",
                                  vSurr,
                                  iDate).
            IF vKK <> iKK THEN 
               NEXT lGar2.

            vVidOb = Get_VidObespech (vSurr,
                                      iDate).
            vDecrRate = GET_COMM("Обесп",
                              ?,
                              term-obl.currency,
                              vSurr,
                              0.00,
                              0,
                              iDate).
            IF vDecrRate = ? THEN vDecrRate = 100.

            {additem.i vVidAll vVidOb}
            
            IF iKndAmt THEN
            DO:
               vAmt = DEC(GetTempXAttrValueEx("term-obl",
                                              vSurr,
                                              "РынСтоим",
                                              iDate,
                                              "0")) NO-ERROR.
                                           
               IF term-obl.currency <> "" THEN
                  vAmt = CurToBase("УЧЕТНЫЙ",
                                   term-obl.currency,
                                   iDate,
                                   vAmt).
            END.
            ELSE
               vAmt =  LnPledge (term-obl.contract,        
                                 term-obl.cont-code,
                                 term-obl.idnt,
                                 term-obl.end-date,
                                 term-obl.nn,
                                 iDate,
                                 iDate,
                                 "").
            ASSIGN 
               vDif  = vDif + vAmt
               vQSum = vQSum + ROUND(vAmt * vGQIndex / 100 * vDecrRate / 100,2)
            .
         END.

         ASSIGN
           vAmtAll = LnPrincipal(loan.contract,        
                                 vContCode,
                                 iDate,
                                 loan.currency)
           vGarAll = vDif
           vGarAllQ = vQSum    
        .

         CREATE tt-GarRasch.
         ASSIGN
            tt-GarRasch.cont-code = vContCode
            tt-GarRasch.end-date  = iDate
            tt-GarRasch.summa     = vDif
            tt-GarRasch.dolg      = vAmtAll
            tt-GarRasch.vidob     = vVidAll
            tt-GarRasch.KK        = iKK
            tt-GarRasch.Qsumma    = vQSum
            vRec                  = RECID(tt-GarRasch)
         .

         FOR EACH bloan WHERE bloan.contract  =  loan.contract 
                          AND bloan.cont-code BEGINS vContCode + " "
                          AND NUM-ENTRIES(bloan.cont-code, " ") >  1
         NO-LOCK:
               /* запоминаем транш с самой большой суммой */
            RUN RE_PARAM_EX IN h_Loan ("0",
                                       iDate,
                                       bloan.since,
                                       bloan.contract,
                                       bloan.cont-code,
                                       OUTPUT vTmpDec[1], /* значение */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            vSum = vTmpDec[1].
            IF vSum >  vMax THEN
               ASSIGN
                  vRec = RECID(tt-GarRasch)
                  vMax = vSum
               .
            CREATE tt-GarRasch.
            ASSIGN
               tt-GarRasch.cont-code = bloan.cont-code
               tt-GarRasch.end-date  = iDate
               tt-GarRasch.summa     = ROUND(vTmpDec[1] * vGarAll / vAmtAll,2)
               tt-GarRasch.dolg      = vTmpDec[1]
               tt-GarRasch.vidob     = vVidAll
               tt-GarRasch.KK        = iKK
               tt-GarRasch.Qsumma    = ROUND(vTmpDec[1] * vGarAllQ / vAmtAll,2)
            .
            IF vDif <> 0 THEN
               vDif = vDif - tt-GarRasch.summa.
         END.
         IF NOT CAN-FIND (FIRST bloan WHERE
                                bloan.contract  =  loan.contract 
                            AND bloan.cont-code BEGINS vContCode + " "
                            AND NUM-ENTRIES(bloan.cont-code, " ") >  1) THEN
            vDif = 0.

            /* коррекция ошибки округления */
         IF vDif <> 0 THEN
         DO:
            FIND FIRST tt-GarRasch WHERE RECID(tt-GarRasch) =  vRec NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-GarRasch THEN /* какая-то фигня случилась */
               LEAVE MB.
            tt-GarRasch.summa = tt-GarRasch.summa + vDif.
         END.

      END. /* IF NOT AVAIL(tt-GarRasch) THEN */
         /* ищем уже сформированный результат */
      FIND FIRST tt-GarRasch WHERE 
                 tt-GarRasch.cont-code =  loan.cont-code
             AND tt-GarRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-GarRasch) THEN
         LEAVE MB.
      ASSIGN
         oSum = tt-GarRasch.summa
         oVid  = tt-GarRasch.vidob
         oSumQ = tt-GarRasch.Qsumma  
      .
   END. /* MB */
END PROCEDURE.

{pfuncdef 
   &DefProc=      "GetCustExInfo"
   &Description=  "Возвращает страну,ОГРН,ИНН клиента"}
PROCEDURE GetCustExInfo:
   DEFINE INPUT PARAMETER iCustCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iCustID   AS INT64       NO-UNDO.
   DEFINE OUTPUT PARAMETER oCountry AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oOGRN    AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oINN     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vHQry   AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHBuff  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFile   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vField  AS CHARACTER   NO-UNDO.
   
   CASE iCustCat:
      WHEN "Ч" THEN 
         ASSIGN 
            vFile    = "person"
            vField   = "person-id"
         .
      WHEN "Ю" THEN 
         ASSIGN 
            vFile    = "cust-corp"
            vField   = "cust-id"
         .
      WHEN "Б" THEN 
         ASSIGN 
            vFile    = "banks"
            vField   = "bank-id"
         .
   END CASE.
   
   CREATE QUERY vHQry.
   CREATE BUFFER vHBuff FOR TABLE vFile.
   vHQry:SET-BUFFERS(vHBuff).
   vHQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &2 = &3 NO-LOCK",vFile,vField,iCustID)).
   vHQry:QUERY-OPEN().
   vHQry:GET-FIRST().

   IF NOT vHQry:QUERY-OFF-END THEN 
      ASSIGN 
         oCountry = vHBuff:BUFFER-FIELD("country-id"):BUFFER-VALUE
         oOGRN    = GetXAttrValue(vFile,STRING(iCustID),"ОГРН")
         oINN     = vHBuff:BUFFER-FIELD("inn"):BUFFER-VALUE
      .
   
   vHQry:QUERY-CLOSE().
   DELETE OBJECT vHQry.
   
END PROCEDURE. 

{pfuncdef 
   &DefProc=      "GetObremTableByLoan"
   &Description=  "По номеру кредитного договора находим все связанные обременения, 
                   по каждому обременению вычисляем выходные параметры (Наименование контрагента,
                   Регистрационный номер,Вид обязательства,Балансовая стоимость,Срок погашения). 
                   Данные возвращаем в виде таблицы по всем обременениям 
                   кредитного договора"}
PROCEDURE GetObremTableByLoan:
   DEFINE INPUT PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR ttObrem BIND.
   
   DEFINE BUFFER bloan FOR loan.
   DEFINE BUFFER oloan FOR loan.
   
   DEFINE VARIABLE vClName    AS CHARACTER NO-UNDO EXTENT 3.
   DEFINE VARIABLE vCountry   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOGRN      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vINN       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vVidOb     AS CHARACTER NO-UNDO.
   
   FIND FIRST bloan WHERE bloan.contract    = iContract
                      AND bloan.cont-code   = iContCode
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bloan THEN RETURN.

   
   FOR EACH oloan WHERE oloan.parent-contract   = iContract
                    AND oloan.parent-cont-code  = iContCode
                    AND oloan.contract          = "Обрем"
                    AND oloan.cont-code         BEGINS iContCode
   NO-LOCK:

      IF oloan.cust-cat <> "В" THEN DO: 
         RUN GetCustName IN h_base (oloan.cust-cat, 
                                    INT64(oloan.cust-id), 
                                    "", 
                             OUTPUT vClName[1], 
                             OUTPUT vClName[2], 
                       INPUT-OUTPUT vClName[3]).

         RUN GetCustExInfo IN THIS-PROCEDURE 
                          (oloan.cust-cat,
                           INT64(oloan.cust-id),
                    OUTPUT vCountry,
                    OUTPUT vOGRN, 
                    OUTPUT vINN).
      END.
      ELSE
         ASSIGN
            vClName[1] = ""
            vClName[2] = ""
            vCountry   = ""
            vOGRN      = ""
            vINN       = ""
         .  
      vVidOb = GetXAttrValueEx("loan",GetSurrogate("loan",ROWID(oloan)),"ВидОбрОбяз",""). 
      
      CREATE ttObrem.
      ASSIGN 
         ttObrem.CustCat  = oloan.cust-cat
         ttObrem.CustId   = oloan.cust-id
         ttObrem.cname    = IF oloan.cust-cat <> "В"           /* Наименование контрагента */
                            THEN vClName[1] + " " + vClName[2]
                            ELSE "1"
         ttObrem.cname    = IF bloan.cont-type = "Течение" OR NUM-ENTRIES(bloan.cont-code," ") = 2
                            THEN (ttObrem.cname + " (кредитная линия)")
                            ELSE ttObrem.cname      
         ttObrem.rnum     = IF CAN-DO("Ю,Б",oloan.cust-cat)    /* Регистрационный номер */
                            THEN IF vCountry = "rus" 
                                 THEN vOGRN
                                 ELSE "НР"
                            ELSE vINN 
         ttObrem.kind     = IF vVidOb BEGINS "4"               /* Вид обязательства */
                            THEN (vVidOb + " (" + GetCodeName("ВидОбрОбяз",vVidOb) + ")")
                            ELSE vVidOb 
         ttObrem.amt      = oloan.interest[1]                  /* Балансовая стоимость */
         ttObrem.open-date = oloan.open-date                   /* Дата начала обременения */
         ttObrem.end-date = oloan.end-date                     /* Срок погашения */
         ttObrem.currency = oloan.currency                     /* Валюта */
      .
   END.                    
   IF NOT CAN-FIND(FIRST ttObrem) THEN DO:
      CREATE ttObrem.
      ttObrem.cname = "0".
   END.
END PROCEDURE.

{pfuncdef 
   &DefProc=      "GetDateIssuance"
   &Description=  "Возвращает дату выдачи кредита"}

FUNCTION GetDateIssuance RETURNS DATE (
   BUFFER vLoan FOR loan
):

   DEFINE VARIABLE vDate      AS DATE NO-UNDO.
   DEFINE VARIABLE vVydDate   AS DATE NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int. 

   vDate = {&BQ-MAX-DATE}.
   IF vLoan.cont-type =  "Течение" 
   THEN /* Признак кредитной линии */
      LOOPLOAN:
      FOR EACH loan WHERE 
               loan.contract  =  vLoan.contract
           AND loan.cont-code BEGINS vLoan.cont-code + " "
           AND NUM-ENTRIES(loan.cont-code, " ") >  1
      NO-LOCK:
         vVydDate = DATE(GetXAttrValue("loan",
                                       loan.contract + "," + loan.cont-code,
                                       "ДатаВыдСсуды")
                        ).
         IF vVydDate <> ? THEN
            vDate = MIN (vVydDate, vDate).
         ELSE
            LOOPLI:
            FOR EACH loan-int WHERE 
                     loan-int.contract  =  loan.contract
                 AND loan-int.cont-code =  loan.cont-code
                 AND loan-int.id-d      =  0
                 AND loan-int.id-k      =  3
               NO-LOCK BREAK BY loan-int.mdate:
               IF FIRST(loan-int.mdate) THEN
               DO:
                  vDate = MIN (loan-int.mdate, vDate).
                  LEAVE LOOPLI.
               END.
            END. /* LOOPLI */
      END. /* LOOPLOAN */
   /* END IF */
   
   vVydDate = DATE(GetXAttrValue("loan",
                                 vLoan.contract + "," + vLoan.cont-code,
                                 "ДатаВыдСсуды")).
   IF vVydDate <> ? THEN
      vDate = MIN (vVydDate, vDate).
   ELSE
      LOOPLI:
      FOR EACH loan-int WHERE 
               loan-int.contract  =  vLoan.contract
           AND loan-int.cont-code =  vLoan.cont-code
           AND loan-int.id-d      =  0
           AND loan-int.id-k      =  3
         NO-LOCK BREAK BY loan-int.mdate:
         IF FIRST(loan-int.mdate) THEN
         DO:
            vDate = MIN (loan-int.mdate, vDate).
            LEAVE LOOPLI.
         END.
      END. /* LOOPLI */

   IF vDate =  {&BQ-MAX-DATE} THEN vDate = ?.

   RETURN vDate.
END FUNCTION.
{pfuncdef 
   &DefProc=      "GetObremCBRFAttr"
   &Description=  "По номеру кредитного договора находим все связанные обременения, 
                   выходной параметр 1 'признак обременения активов обязательствами перед 
                   Банком России' принимает значение ДР ОбремОбязБР (Да/Нет (пусто)). 
                   Достаточно найти признак на одном  обременении (первом попавшем с признаком).
                   Выходной параметр 2  'признак необремененных активов, пригодных для 
                   предоставления в качестве обеспечения Банку России' принимает значение 
                   ДР ОбеспБР на кредитном договоре, действующее на отчетную дату.
"}
PROCEDURE GetObremCBRFAttr:
   DEFINE INPUT PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oObremCBRF AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oLoanCBRF  AS LOGICAL NO-UNDO.
   
   DEFINE BUFFER oloan FOR loan.
   
   oLoanCBRF = LOGICAL(GetXattrValueEx("loan",iContract + "," + iContCode,"ОбеспБР","НЕТ"),
                       "ДА/НЕТ").
   OBR:
   FOR EACH oloan WHERE oloan.parent-contract   = iContract
                    AND oloan.parent-cont-code  = iContCode
                    AND oloan.contract          = "Обрем"
                    AND oloan.cont-code         BEGINS iContCode
   NO-LOCK:
      oObremCBRF = LOGICAL(GetXattrValueEx("loan",
                                           GetSurrogate("loan",ROWID(oloan)),
                                           "ОбремОбязБР","НЕТ"),
                           "ДА/НЕТ").
     IF oObremCBRF THEN LEAVE OBR.
   END.
END PROCEDURE.

/* Вставка Плюс банк */
FUNCTION PscMortgage RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):
/* message "tst" view-as alert-box.     */
    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '3'.
    if loan.cont-type = 'Потреб' or loan.cont-type = 'КЗ' or loan.cont-type = ? or loan.cont-type begins 'Авто' or loan.cont-type begins 'Avto' then typeTable = '1'.

typeTable = '1'.

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
                   OUTPUT vPSC
                   ).

    return vPSC * 100.
END FUNCTION.

FUNCTION PscMortgage1 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '2'. /* 2- Авто+Каско */

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
                   OUTPUT vPSC
                   ).
    return vPSC * 100.
END FUNCTION.

FUNCTION PscTransh RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '2'. /* 2- Авто+Каско */

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
                   OUTPUT vPSC
                   ).
    return vPSC * 100.
END FUNCTION.


DEF TEMP-TABLE ttReportTablePsc NO-UNDO
   FIELD tf_id                   AS INT64 /* Порядковый номер строки */
   FIELD tf_payment-date         AS DATE  /* Дата операции */
   FIELD tf_sum-payment          AS DEC   /* Cумма платежа) */
   FIELD tf_sum-percent          AS DEC   /* 1. Суммы процентов */
   FIELD tf_rest-debts           AS DEC   /* 2. Суммы остатков  */
   FIELD tf_basic-sum-loan       AS DEC   /* 3. Суммы основного долга */
   FIELD tf_additional-charge1   AS DEC   /* 4. Суммы дополнительных требований, комиссии */
   FIELD tf_additional-charge2   AS DEC   /* 400. Суммы дополнительных требований, комиссии */
   FIELD tf_actual-payment       AS DEC   /* 5. Суммы страховых взносов */
   FIELD tf_charges              AS CHAR  /* Требования */
   INDEX tf_id tf_payment-date
.

/* Заполнение таблицы данными ЭПС */
PROCEDURE FillTables:
    DEF INPUT PARAM iContract AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
    DEF INPUT PARAM iDate     AS DATE NO-UNDO.
    DEF INPUT PARAM iContType AS CHAR NO-UNDO.
    DEF INPUT PARAM typeTable AS CHAR NO-UNDO.
    DEF OUTPUT PARAM oEps AS DECIMAL NO-UNDO.

    DEF VAR vAccur   AS DEC   NO-UNDO.
    DEF VAR vTryNum  AS INT64   NO-UNDO.
    DEF VAR vStep    AS DEC   NO-UNDO. /* Шаг, для наращивания ЭПС */
    DEF VAR flag     AS LOG   NO-UNDO. /* yes - увеличиваем, no - уменьшаем ЭПС */
    DEF VAR vI       AS INT64   NO-UNDO.
    DEF VAR vSummPay AS DEC   NO-UNDO. /* Общая сумма платежей */
    DEF VAR vBegDate AS DATE  NO-UNDO. /* Дата выдачи кредита  */
    DEF VAR vSumma   AS DEC   NO-UNDO. /* Сумма для подбора ЭПС */
    DEF VAR vInsurFl AS LOG   NO-UNDO. /* включать в расчет ЭПС страховые платежи? */
    DEF VAR vEpsForm AS INT NO-UNDO.
    DEF VAR vPer     AS INT64 NO-UNDO.

    def var fullKasko as decimal no-undo.
    DEF BUFFER bloan FOR loan.
    def var c1 as char no-undo.
    def var c2 as char no-undo.
    DEF var vidstr AS CHAR NO-UNDO.
    def var vTmpDec as decimal no-undo.
    def var lastDate as date no-undo.
    def var summAuto as char no-undo.
    def var summAuto_8 as decimal no-undo.
    def var summLoan_9 as decimal no-undo.
    def var kaskoK_10 as decimal no-undo.
    def var kaskoN_11 as decimal no-undo.
    def var premStrah_12 as decimal no-undo.

    IF iDate >= DATE( 9, 1, 2014)
     THEN vEpsForm = 2.
     ELSE vEpsForm = 1.

    summAuto = getxattrvalue("loan","Кредит," + loan.cont-code,"rko11_price").

    if summAuto <> ? and summAuto <> "" then do:
        summAuto_8 = decimal(replace(summAuto,",","")).
    end.
    else summAuto_8 = 1.

    ASSIGN
    vAccur  = 0.000001 /* dec(FGetSetting("ЭПС", "ЭПСТочн", ?)) */
    vTryNum = 800  /* INT64(FGetSetting("ЭПС", "ЭПСКолП", ?)) */
    vInsurFl = yes /* FGetSetting("ЭПС", "ЭПСВклСтрахПл" , "") EQ "ДА" */
    .

       vidstr = "Жизнь*".
    vTmpDec = 0.
    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
    IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                      bloan.contract + "," + bloan.cont-code,
                      "vidstr",
                      ""))
        THEN NEXT.
        IF GetXAttrValueEx("loan",
          bLoan.contract + "," + bLoan.cont-code,
          "ПСКРасчет",
          "ДА"
          ) EQ "Нет"
      THEN NEXT.

            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    END.
    premStrah_12 = vTmpDec.

    vidstr = "КАСКО_К".
    vTmpDec = 0.
    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
            IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.
            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    END.
    fullKasko = vTmpDec.
    kaskoK_10 = vTmpDec.
    kaskoK_10 = 0.

    vidstr = "КАСКО_Н".
    vTmpDec = 0.

    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
            IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.

            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    end.

    fullKasko = fullKasko + vTmpDec.
    kaskoN_11 = vTmpDec.
    kaskoN_11 = 0.

    summLoan_9 = 0.

/*    find first term-obl where term-obl.cont-code = loan.cont-code and term-obl.idnt = 2 and term-obl.nn  = 0 no-lock no-error.
    if avail term-obl then summLoan_9 = term-obl.amt-rub.
*/
    for each term-obl
     where term-obl.contract = loan.contract
       AND term-obl.cont-code = loan.cont-code
       and term-obl.idnt = 2 no-lock by term-obl.end-date:
        summLoan_9 = term-obl.amt-rub.
        leave.
    end.
/* message string(summLoan_9) view-as alert-box. */
   RUN fill-graphp1.p(loan.contract,
                    loan.cont-code,
                    loan.since,
                    fullKasko,
                    typeTable,
                    OUTPUT TABLE ttReportTablePsc
                    ).


    lastDate = loan.end-date.
    find first ttReportTablePsc where ttReportTablePsc.tf_id = -1 no-error.
    if avail ttReportTablePsc then do:
        lastDate = ttReportTablePsc.tf_payment-date.
        delete ttReportTablePsc.
    end.

DEF VAR pskbezstrah AS CHAR NO-UNDO.
DEF VAR mytmpdec AS CHAR NO-UNDO.
pskbezstrah = GetSysConf("ПСКБезСтрах") NO-ERROR.
/*
message string(pskbezstrah) + ' ' + string(summLoan_9) view-as alert-box.
    */
    if summLoan_9 <> 0 then do:
        create ttReportTablePsc.
        if typeTable = '2' then do:  /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
            ttReportTablePsc.tf_id = -1.
            ttReportTablePsc.tf_payment-date = loan.open-date.
            if pskbezstrah = ? then
                 ttReportTablePsc.tf_basic-sum-loan = - summLoan_9 + (kaskoK_10 + kaskoN_11) *
                 ((summLoan_9 - kaskoK_10 - premStrah_12) / summAuto_8) + premStrah_12.
            else ttReportTablePsc.tf_basic-sum-loan = - summLoan_9.
            release ttReportTablePsc.
        end.
        else  do:
            assign
                ttReportTablePsc.tf_id = -1
                ttReportTablePsc.tf_payment-date = loan.open-date
                ttReportTablePsc.tf_basic-sum-loan = - summLoan_9
                ttReportTablePsc.tf_sum-percent = premStrah_12.
            release ttReportTablePsc.
        end.
    end.
/*
          run instview.p(TEMP-TABLE ttReportTablePsc:HANDLE).

*/


    CASE vEpsForm:
        WHEN 1 THEN
            vPer = 365.
        WHEN 2 THEN DO:
            DEF VAR numd AS INT NO-UNDO EXTENT 366 INIT 0.
            DEF VAR cda AS INT NO-UNDO.
            DEF VAR d2 AS DATE NO-UNDO.
            FOR EACH ttReportTablePsc BY ttReportTablePsc.tf_payment-date:
            IF ttReportTablePsc.tf_payment-date > loan.open-date THEN DO:
                cda = ttReportTablePsc.tf_payment-date - d2.
                IF cda < 366 THEN
                numd [ cda ] = numd[ cda ] + 1.
            END.
            d2 = ttReportTablePsc.tf_payment-date.
            END.
            DEF VAR imax AS INT NO-UNDO.
            DEF VAR imm AS INT NO-UNDO.
            vPer = 0. imax = 0.
            DO imm = 1 TO 365:
            IF imax < numd [ imm ] THEN DO:
                imax = numd [ imm ].
                vPer = imm.
            END.
            END.
            IF imax = 1 THEN DO:
            DEF VAR summ AS INT64 NO-UNDO.
            summ = 0.
            imax = 0.
            DO imm = 1 TO 365:
                IF numd [ imm ] = 1 THEN DO:
                summ = summ + imm.
                imax = imax + 1.
                END.
            END.
            IF imax > 0 THEN DO:
                vPer = ROUND( summ / imax , 0).
            END. ELSE DO:
                vPer = 0.
            END.
            END.

            /* vPer = 30. */
            END.
    END CASE.
/* message "vEpsForm=" string(vEpsForm) " " string(vPer) view-as alert-box. */


    BLCK:
    DO
    ON ERROR    UNDO BLCK, LEAVE BLCK:
        ASSIGN
         vBegDate = loan.open-date
         oEps = 0.1
         vStep = 0.1
         flag = YES
      .

      DO vi=1 TO vTryNum:
         vSumma = 0.
        FOR EACH ttReportTablePsc:
            if typeTable = '2' then do:  /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
                if ttReportTablePsc.tf_id = -1 then do:
                    vSummPay = ttReportTablePsc.tf_basic-sum-loan.

                end.
                else do:
                    /*IF iDate >= DATE( 7, 1, 2014)
                      AND ttReportTablePsc.tf_additional-charge1 > 0
                     THEN vSummPay = ttReportTablePsc.tf_additional-charge1.
                    ELSE*/ DO:
                    vTmpDec = lastDate - ttReportTablePsc.tf_payment-date.
                    if vTmpDec > 365 then vTmpDec = 1. else vTmpDec = vTmpDec / 365.
                    if pskbezstrah = ? then
                       vSummPay = decimal(ttReportTablePsc.tf_sum-payment) + decimal(ttReportTablePsc.tf_actual-payment) * ((summLoan_9 - kaskoK_10 - premStrah_12) / summAuto_8) * vTmpDec.
                    else  vSummPay = decimal(ttReportTablePsc.tf_sum-payment).
                    END.
                end.
            end.

            else  do:
                ASSIGN
                    vSummPay = ttReportTablePsc.tf_sum-percent +
                        ttReportTablePsc.tf_basic-sum-loan +
                        ttReportTablePsc.tf_additional-charge1 +
                        ttReportTablePsc.tf_additional-charge2
                    vSummPay = vSummPay + ttReportTablePsc.tf_actual-payment WHEN vInsurFl
                    .
            end.
        CASE vEpsForm:
        WHEN 1 THEN
            vSumma   = vSumma + vSummPay  /
                    EXP((1 + oEps), (ttReportTablePsc.tf_payment-date
                    - vBegDate) / 365).
                WHEN 2 THEN
            vSumma   = vSumma + vSummPay  / (
            (1 + oEps * ( (ttReportTablePsc.tf_payment-date - vBegDate) MODULO vPer ) / vPer ) *
                    EXP((1 + oEps), TRUNCATE( (ttReportTablePsc.tf_payment-date
                    - vBegDate) / vPer, 0) )
                    ).
                END CASE.

         END.
         IF vSumma LT 0 AND flag EQ YES THEN
         DO:
            flag  = NO.
            vStep = vStep / 10.
         END.
         IF vSumma GT 0 AND flag EQ NO THEN
         DO:
            flag = YES.
            vStep = vStep / 10.
         END.
         IF vStep GE vAccur THEN  DO: /* !! */
            IF flag EQ YES THEN
               oEps = oEps + vStep.
            ELSE
               oEps = oEps - vStep.
         END.
         ELSE LEAVE blck.
      END.
    END. /* blck */
    IF vEpsForm = 2 THEN oEps = oEps * /* ROUND */ ( 365 / vPer).
    oEps = truncate(oEps + 0.000004 ,5).

/*message "oEps=" string(oEps) view-as alert-box.
RUN instview.p(TEMP-TABLE ttReportTablePsc:HANDLE).*/
END PROCEDURE.
/* Конец вставки Плюс банк */
/* $LINTFILE='pp-pqres.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='chumv' */
/* $LINTDATE='11/07/2017 12:56:05.400+03:00' */
/*prosign2WAk8lEEoAV4Qftiqhd7fQ*/