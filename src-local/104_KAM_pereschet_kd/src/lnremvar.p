/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: lnremvar.p
      Comment: Схема начисления процентов по КД договорам.

               ipBegDate - Дата начала расчета, передается на 1-н день
               больше реальной (исторический факт). Это было сделано для
               инклюдника workost1.i, цель - формирование ВХОДЯЩЕГО остатка.

   Parameters: нет
         Uses:
      Used by:
      Created: 18/03/2002
     Modified: Илюха 09.10.2002
     Modified: Илюха 11.11.2002 База начисления и процентные ставки могут браться
               из доп. реквизитов договора и начальных значений
     Modified:
*/

form "~n@(#) lnremvar.p 1.0 18/03/2002"
with frame sccs-id stream-io width 250.

{globals.i}     /* Глобальные расшаренные переменные сессии */
{svarloan.def}  /* Глобальные расшаренные переменные модуля */
{t-otch.i}      /* Временная таблица для расчета и отчета */

{intrface.get xclass} /* Инструменты для работы с метасхемой. */
{intrface.get comm}   /* Инструменты для работы с комиссиями. */
{intrface.get schem}  /* Инструменты для работы со схемами начисления. */
{intrface.get date}   /* Инструменты для работы с датами */
{intrface.get loan}   /* Инструменты для работы с договорами */

{ln-nach.i}     /* Инструментарий для схем начисления по договорам требует svarloan.def*/
/*{empty LnShPrm}*/
EMPTY TEMP-TABLE LnShPrm.

DEFINE INPUT PARAM ipContractChar AS CHAR  NO-UNDO. /* Назначение договора */
DEFINE INPUT PARAM ipContCodeChar AS CHAR  NO-UNDO. /* Номер договора */
DEFINE INPUT PARAM ipSchRecid     AS RECID NO-UNDO. /* Идентификатьор схемы */
DEFINE INPUT PARAM ipBegDate      AS DATE  NO-UNDO. /* Дата начала расчета */
DEFINE INPUT PARAM ipEndDate      AS DATE  NO-UNDO. /* Дата окончания расчета */
DEFINE INPUT PARAM dat-per        AS DATE  NO-UNDO. /* Дата перехода на 39П */
DEFINE INPUT PARAM cod-par        AS INT64 NO-UNDO. /* Код параметра */
DEFINE INPUT PARAM fl-type-ost    AS INT64 NO-UNDO. /* Всегда передается 1 не исп. */

DEF VAR vCurrCalcChar AS CHAR NO-UNDO. /* База для расчета */
DEF VAR vRateChar     AS CHAR NO-UNDO. /* Смещение на требуему %% ставку */

DEF VAR mRecalcPP   AS LOG NO-UNDO.
DEF VAR mRecalcLoan AS LOG NO-UNDO.
DEF VAR mFormRasch  AS CHAR NO-UNDO. /* Формула расчета процентов с ДР ВыбФормРасчПроц */
DEF VAR vNeedGraf   AS LOG  NO-UNDO.

/* Коды параметров хранящих проценты */
DEFINE VAR vAllRateChar AS CHAR
                        INIT "4,8,9,11,12,14,15,17,18,20,81,82,96,704"
                        NO-UNDO.

/* Поиск договора */
FIND FIRST loan WHERE loan.contract  EQ ipContractChar  
                  AND loan.cont-code EQ ipContCodeChar 
     NO-LOCK NO-ERROR.

SetCodPar(loan.class-code) .

ASSIGN
   mRecalcLoan = (GetSysConf("ПересчетДоговора") = "да")
   mRecalcPP   = (GetSysConf("ПересчетПП") = "да")
   mFormRasch   = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ВыбФормРасчПроц",GetXAttrInit(loan.class-code,"ВыбФормРасчПроц"))
.
IF mFormRasch EQ ? 
   THEN mFormRasch = fGetSetting("ВыбФормРасчПроц","","1").

IF loan.since < ipEndDate AND
   cod-par = 1            AND
   fl-Type-Ost = 1        AND
   NOT mRecalcLoan        AND
   NOT mRecalcPP
THEN
   RUN GET_DOLG_CORR (ipContractChar,
                      ipContCodeChar,
                      loan.since,
                      ipEndDate,
                      YES).

/* в lnscheme организовано 2 цикла если дата перех в периоде */
vIshOst = ipEndDate LE dat-per.
IF vIshOst THEN
   ipEndDate = ipEndDate - 1.

/* Формирование базы начисления. */
RUN GetCalcChar (ipContractChar,
                 ipContCodeChar,
                 cod-par,
                 OUTPUT vCurrCalcChar).

IF CAN-DO(vCurrCalcChar, STRING(CodOstPar)) THEN
   vNeedGraf = YES.
IF fl-type-ost EQ 1

/* Формирование остатка по параметру договора. */
THEN DO:
   RUN GET_REM_BY_PRM (ipContractChar,
                      ipContCodeChar,
                      vCurrCalcChar,  /* База начисления */
                      ipBegDate,
                      ipEndDate,
                      cod-par,
                      loan.currency,
                      MIN(ipEndDate,loan.since),
                      mRecalcLoan,
                      mRecalcPP).
   IF     vNeedGraf 
      AND loan.since LT ipEndDate THEN
   DO:
      RUN GET_REM_BY_TERM (ipContractChar,
                           ipContCodeChar,
                           "2",              /* Код "плановых сумм" */
                           MAX(ipBegDate,loan.since + IF vIshOst THEN 1 ELSE 0),
                           ipEndDate,
                           loan.currency,
                           NO).
   END.
END.
/* Формирование остатка по плановым сущностям договора. */
ELSE RUN GET_REM_BY_TERM (ipContractChar,
                          ipContCodeChar,
                          "2",              /* Код "плановых сумм" */
                          ipBegDate,
                          ipEndDate,
                          loan.currency,no).

/* Получение кода комиссии */
RUN GetCodeRate (
    cod-par,            /* Порядковый номер %%, пени loan.interest[cod-par] */
    OUTPUT vRateChar,   /* Код тарифа/комиссии */
    BUFFER loan).

/* Формирование процентной ставки по динамике остатка. */
RUN GET_COMM_BY_REM (vRateChar,             /* Код комиссии/тарифа */
                     ipContractChar + "," + ipContCodeChar, /* KAU */
                     ipBegDate,
                     ipEndDate).

/* Начисление и формирование результата. */
RUN GET_NAC_REP (ipSchRecid,
                 ipBegDate,
                 ipEndDate,
                 mFormRasch).

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
           loan-par.amt-id EQ INT64(ENTRY(cod-par, vAllRateChar)) + CodostPar 
NO-LOCK NO-ERROR.

/* Корректируем отчет */
FOR EACH otch1:
   otch1.amt-id = IF AVAIL loan-par THEN loan-par.amt-id
                                    ELSE ?.
   IF otch1.comment EQ "" THEN
      otch1.comment =  IF AVAIL loan-par THEN loan-par.name 
                                         ELSE otch1.comment.
END.

{intrface.del}

RETURN.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:46:33.175+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='lnremvar.p' */
/*prosignwAkthN5qdOCybaqsXSiQiA*/