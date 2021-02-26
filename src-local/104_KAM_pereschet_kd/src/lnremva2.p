/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ЗАО "Банковские информационные системы"
     Filename: lnremva2.p
      Comment: Схема начисления процентов по КД договорам


               iBegDate - Дата начала расчета, передается на 1-н день
               больше реальной (исторический факт). Это было сделано для
               инклюдника workost1.i, цель - формирование ВХОДЯЩЕГО остатка.

   Parameters:
         Uses:
      Used by:
      Created: Илюха 18.10.2002
     Modified: Илюха 21.10.2002
*/

{globals.i}     /* Глобальные расшаренные переменные сессии */
{svarloan.def}  /* Глобальные расшаренные переменные модуля */
{t-otch.i}      /* Временная таблица для расчета и отчета */

{intrface.get xclass}  /* Инструменты для работы с метасхемой. */
{intrface.get comm}    /* Инструменты для работы с комиссиями. */
{intrface.get schem}   /* Инструменты для работы со схемами начисления. */
{intrface.get date}    /* Инструменты для работы с датами */
{intrface.get db2l}    /* Инструменты для работы с данными */
{intrface.get instrum} /* Инструменты для работы с фин.инструментами */
{intrface.get loan}    /* Инструменты для работы с договорами */
{sh-defs.i new}
{sh-temp.i new}
{ln-nach.i}     /* Инструментарий для схем начисления по договорам требует
                   svarloan.def*/
{ln-proc.i}
/*{empty LnShPrm}*/
EMPTY TEMP-TABLE LnShPrm.

DEFINE INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* Назначение договора */
DEFINE INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* Номер договора */
DEFINE INPUT PARAM iSchRecid     AS RECID NO-UNDO. /* Идентификатьор схемы */
DEFINE INPUT PARAM iBegDate      AS DATE  NO-UNDO. /* Дата начала расчета */
DEFINE INPUT PARAM iEndDate      AS DATE  NO-UNDO. /* Дата окончания расчета */
DEFINE INPUT PARAM iDatPer       AS DATE  NO-UNDO. /* Дата перехода на 39П */
DEFINE INPUT PARAM iCodPar       AS INT64   NO-UNDO. /* Код параметра */
DEFINE INPUT PARAM iTypeOst      AS INT64   NO-UNDO. /* Всегда передается 1 не исп. */

DEF VAR vCurrCalcChar AS CHAR NO-UNDO. /* База для расчета */
DEF VAR vRateChar     AS CHAR NO-UNDO. /* Смещение на требуему %% ставку */
DEF VAR vRateTax      AS CHAR NO-UNDO. /* Код налога */
DEF VAR vNumGrups     AS INT64  NO-UNDO. /* Число групп для одного параметра*/
DEF VAR vGrupCounter  AS INT64  NO-UNDO. /* группа для одного параметра     */
DEF VAR vCommChar     AS CHAR NO-UNDO.
DEF VAR vRateDec      AS DEC  NO-UNDO.

DEF VAR mRecalcPP   AS LOG  NO-UNDO.
DEF VAR mRecalcLoan AS LOG  NO-UNDO.
DEF VAR mSurr       AS CHAR NO-UNDO.
DEF VAR mTypeCond   AS LOG  NO-UNDO.
DEF VAR vDateD      AS DATE NO-UNDO.
DEF VAR vDateLastOp AS DATE NO-UNDO.
DEF VAR vNeedGraf   AS LOG  NO-UNDO.
DEF VAR vPrmCharBase AS CHAR NO-UNDO. /* Строка параметров "," вместо "&+" */
DEF VAR vJAmTrahsh  AS LOG  NO-UNDO.
DEF VAR vFromTrahsh AS LOG  NO-UNDO.

DEF BUFFER loan-cond FOR loan-cond.
DEF BUFFER bl-int    FOR loan-int.
DEF BUFFER bc-rate   FOR comm-rate.

/* Коды параметров хранящих проценты */
DEF VAR vAllRateChar AS CHAR
                       INIT "4,8,9,11,12,14,15,17,18,20,81,82,96,704"
                       NO-UNDO.

vJAmTrahsh = NUM-ENTRIES(iContCodeChar," ") EQ 2.

/* Поиск договора */
FIND FIRST loan WHERE loan.contract  EQ iContractChar  
                  AND loan.cont-code EQ iContCodeChar 
     NO-LOCK NO-ERROR.
FIND FIRST curloan WHERE RECID(curloan) EQ RECID(loan) 
     NO-LOCK NO-ERROR.

SetCodPar(loan.class-code) .

/* Поиск первого условия- чтобы проверить аннуитетность 
- возможно по одному и тому же договору есть условия с аннитетной и не
аннуитетными схемами из-за рестуктуризации */

FIND LAST  loan-cond WHERE loan-cond.contract  EQ iContractChar
                       AND loan-cond.cont-code EQ iContCodeChar
                       AND loan-cond.since     LE iBegDate
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan-cond THEN
   FIND FIRST loan-cond WHERE loan-cond.contract  EQ iContractChar 
       AND loan-cond.cont-code EQ iContCodeChar 
       AND loan-cond.since     GE loan.open-date
NO-LOCK NO-ERROR.

mTypeCond  = (GetXAttrValue("loan-cond",
                            iContractChar + "," +  iContCodeChar + "," + string(loan-cond.since),
                            "СхемаПлат") = "Аннуитетная").
ASSIGN
   mRecalcLoan = (GetSysConf("ПересчетДоговора") = "да")
   mRecalcPP   = (GetSysConf("ПересчетПП") = "да") AND NOT mTypeCond 
   /*
   для аннуитетной схемы обязательно запускаем пересчет по факту + план в случае
   пересчета платежей при пересчете  состояния договора */
   .

Cr-Otch:
DO TRANSACTION:

/* Формирование базы начисления. */
RUN GetCalcCharNew (iContractChar,
                    iContCodeChar,
                    iCodPar,
                    OUTPUT vCurrCalcChar,
                    OUTPUT vNumGrups).
IF vCurrCalcChar MATCHES "*г*"  THEN
   ASSIGN
      vCurrCalcChar = REPLACE(vCurrCalcChar,"г","")
      vNeedGraf = YES
   .
IF vCurrCalcChar MATCHES "*Тр*"  THEN
   ASSIGN
      vCurrCalcChar = REPLACE(vCurrCalcChar,"Тр","")
      vFromTrahsh = YES
   .

vPrmCharBase = REPLACE(REPLACE(vCurrCalcChar,"&",","),"+",",").
IF CAN-DO(vCurrCalcChar, STRING(CodOstPar)) THEN
   vNeedGraf = YES.

/* Получение кода комиссии */
RUN GetCodeRateNew (
    iCodPar,            /* Порядковый номер %%, пени loan.interest[iCodPar] */
    vNumGrups,          /* Число групп*/
    OUTPUT vRateChar,   /* Код тарифа/комиссии */
    BUFFER loan).

/* просто искать до первого значения */
vRateDec = 0.
DO vGrupCounter = 1 TO NUM-ENTRIES(vRateChar,"&"):
   ASSIGN
      vCommChar = ENTRY(vGrupCounter,vRateChar,"&").
      FIND LAST bc-rate WHERE bc-rate.kau     EQ iContractChar + "," + iContCodeChar
                       AND bc-rate.commission EQ vCommChar
                       AND bc-rate.since      LE iEndDate
      NO-LOCK NO-ERROR. 

            /* Получение комиссии/тарифа на дату */
      vRateDec = vRateDec + 
                 IF AVAIL bc-rate THEN bc-rate.rate-comm
                               ELSE 0.
      IF vRateDec GT 0 THEN LEAVE.
/* для транша проверить на охв. */
      IF vJAmTrahsh THEN 
      DO:
         FIND LAST bc-rate WHERE bc-rate.kau        EQ iContractChar + "," + ENTRY(1,iContCodeChar," ")
                          AND bc-rate.commission EQ vCommChar
                          AND bc-rate.since      LE iEndDate
         NO-LOCK NO-ERROR.
         vRateDec = vRateDec + 
                    IF AVAIL bc-rate THEN bc-rate.rate-comm
                                        ELSE 0.
         IF vRateDec GT 0 THEN LEAVE.
      END.
/*   A для охв. проверить и на транше? 
     Пока не будем */

END.
IF    vRateDec EQ ? THEN /* если ставки никогда не было */
   LEAVE Cr-Otch.


/*IF loan.since < iEndDate AND iCodPar = 1 AND iTypeOst = 1 AND
   GetSysConf("ПересчетДоговора") <> "Да"
THEN
*/
/* Проблемы с  использованием iTypeOst - при вводе нового условия он
передается со значением 0, т.е все будет формироваться только
по плановым остаткам. Если это второе условие и аннуитетная схема -
то nach-pp как-то с этим борется , а данная схема нет - необходимо
установить условие, что которое позволяло перебросить iTypeOst в
1 , если вводится новое второе условие и есть уже движения по договору.
*/

/* Проверим, что по догoвору было движение 
   черти что приходится делать. чтобы подточить условия расчета
   нужно перепроектирование */

IF iTypeOst = 0 
   AND vNeedGraf THEN 
DO:
/* перебор по траншам похоже не нужен, может быть 
   просто проверить количество условий? */
     /* Поиск первой дебетовой операции по движению основного параметра */
   IF CAN-FIND( FIRST loan-int WHERE loan-int.contract  EQ iContractChar 
                                 AND loan-int.cont-code EQ iContCodeChar 
                                 AND loan-int.mdate     GE loan.open-date 
                                 AND loan-int.id-d      EQ CodOstPar)
      OR
     /* Поиск первой кредитовой операции по движению основного параметра */
      CAN-FIND( FIRST bl-int WHERE bl-int.contract  EQ iContractChar 
                               AND bl-int.cont-code EQ iContCodeChar 
                               AND bl-int.mdate     GE loan.open-date
                               AND loan-int.id-k    EQ CodOstPar)
   THEN 
      iTypeOst = 1.
END.

IF loan.since < iEndDate 
   AND iCodPar  = 1          
   AND iTypeOst = 1          
   AND NOT mRecalcLoan       
   AND NOT mRecalcPP THEN
      RUN GET_DOLG_CORR (iContractChar,
                      iContCodeChar,
                      loan.since,
                      iEndDate,
                      YES).

/* в lnscheme организовано 2 цикла если дата перех в периоде */
vIshOst = iEndDate LE iDatPer.
IF vIshOst THEN
   iEndDate = iEndDate - 1.

/* Формирование остатка по параметру договора. */
IF iTypeOst EQ 1 THEN
DO:
/* разводка для vFromTrahsh  
   для траншей берем от даты пересчета договора, 
   чтобы не считать по каждому траншу */

   IF vFromTrahsh THEN
   DO:
      RUN GET_REM_BY_PRM_TR (iContractChar,
                             iContCodeChar,
                             vCurrCalcChar,  /* База начисления */
                             iBegDate,
                             iEndDate,
                             iCodPar,
                             loan.currency,
                             loan.since,
                             mRecalcLoan,
                             mRecalcPP).

     RUN SetLnShPrmAdd (loan.currency).

   END.
   ELSE DO:
      RUN GET_REM_BY_PRM_NEW (iContractChar,
                              iContCodeChar,
                              vCurrCalcChar,  /* База начисления */
                              iBegDate,
                              iEndDate,
                              iCodPar,
                              loan.currency,
                              loan.since,
                              mRecalcLoan,
                              mRecalcPP,
                              NO).
   END.
/*
   IF vFromTrahsh THEN
   DO:
      vDateLastOp = loan.since.
      RUN GET_REM_BY_PRM_TR (iContractChar,
                             iContCodeChar,
                             vCurrCalcChar,  /* База начисления */
                             iBegDate,
                             MIN(iEndDate,vDateLastOp),
                             iCodPar,
                             loan.currency,
                             loan.since,
                             mRecalcLoan,
                             mRecalcPP).

      IF     vNeedGraf 
         AND vDateLastOp LT iEndDate THEN
         RUN GET_REM_BY_TERM_TR (iContractChar,
                                 iContCodeChar,
                                 2,              /* Код "плановых сумм" */
                                 vDateLastOp + 1,
                                 iEndDate,
                                 loan.currency).
     RUN SetLnShPrmAdd (loan.currency).

   END.
   ELSE DO:
      RUN GetLastDateParam (iContractChar,
                            iContCodeChar,
                            vCurrCalcChar,
                            iBegDate,
                     OUTPUT vDateLastOp). /* Последняя дата движения */
      IF vDateLastOp EQ ? THEN
         vDateLastOp = loan.since.
      IF MAX(vDateLastOp,loan.since) GE iBegDate THEN
      DO:
         RUN GET_REM_BY_PRM_NEW (iContractChar,
                                 iContCodeChar,
                                 vCurrCalcChar,  /* База начисления */
                                 iBegDate,
                                 MIN(iEndDate,MAX(vDateLastOp,loan.since)),
                                 iCodPar,
                                 loan.currency,
                                 loan.since,
                                 mRecalcLoan,
                                 mRecalcPP,
                                 NO).
      END.
      IF     vNeedGraf 
         AND vDateLastOp LT iEndDate THEN
         RUN GET_REM_BY_TERM (iContractChar,
                              iContCodeChar,
                              2,              /* Код "плановых сумм" */
                              MAX(iBegDate,MAX(vDateLastOp,loan.since) + IF vIshOst THEN 1 ELSE 0),
                              iEndDate,
                              loan.currency,
                              NO).
   END.
  */
END.
/* Формирование остатка по плановым сущностям договора. */
ELSE
DO:
   IF vFromTrahsh THEN
   DO:
      RUN GET_REM_BY_TERM_TR (iContractChar,
                              iContCodeChar,
                              2,              /* Код "плановых сумм" */
                              iBegDate,
                              iEndDate,
                              loan.currency).
      RUN SetLnShPrmAdd (loan.currency).
   END.
   ELSE
      RUN GET_REM_BY_TERM (iContractChar,
                           iContCodeChar,
                           2,              /* Код "плановых сумм" */
                           iBegDate,
                           iEndDate,
                           loan.currency,
                           NO).
END.

/* Получение кода для НДС */
RUN GetCodeTax (
    iCodPar,            
    vNumGrups,          /* Число групп*/
    OUTPUT vRateTax,   /* Код тарифа */
    BUFFER loan).


/* Формирование процентной ставки по динамике остатка. */
/* Предполагаем, что ставки на охватывающем. Для индивидуальных ставок
   траншей придется дорабатывать  */
RUN GET_COMM_BY_REM_NEW( iContractChar ,
                         iContCodeChar , 
                         vRateChar,       /* Код комиссии/тарифа */      
                         iContractChar + "," + iContCodeChar, /* KAU */
                         iBegDate,
                         iEndDate,
                         iCodPar).
IF vRateTax = ?
THEN 
/* Формирование процентной ставки по НДС */
   RUN GET_COMM_BY_TAX   (vRateTax,             /* Код комиссии/тарифа */
                         iContractChar + "," + iContCodeChar, /* KAU */
                         iBegDate,
                         iEndDate,
                         iCodPar).
/* Начисление и формирование результата. */
RUN GET_NAC_REP_NEW (iSchRecid,
                     iContCodeChar,
                     iContractChar,
                     iBegDate,
                     iEndDate).

/* Если сумма начисления за период нулевая,
** то удаляем отчет
** Так построен пересчет по договорам */

FIND FIRST otch1 WHERE
           otch1.comment EQ ""
       AND otch1.summ_pr NE 0.00
NO-ERROR.

IF NOT AVAIL otch1 THEN
DO:
   FOR EACH otch1 WHERE
      otch1.comment EQ "":
      DELETE otch1.
   END.
END.
/* Ищем параметр - код процента по которому начисляем */
FIND FIRST loan-par WHERE
           loan-par.amt-id = INT64(ENTRY(iCodPar, vAllRateChar)) + CodostPar 
NO-LOCK NO-ERROR.


FOR EACH otch1:
   otch1.amt-id = IF AVAIL loan-par THEN loan-par.amt-id
                                    ELSE ?.
   IF otch1.comment EQ "" THEN
      otch1.comment =  IF AVAIL loan-par THEN loan-par.name 
                                         ELSE otch1.comment.
END.

END. /* do trans: */

{intrface.del}

RETURN.
