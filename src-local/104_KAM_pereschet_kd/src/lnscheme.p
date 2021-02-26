/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: lnscheme.p
      Comment: Вызов схем начисления процентов в модуле К&Д.
   Parameters: нет
         Uses:
      Used by:
      Created: 18/03/2002
     Modified: Илюха 08.10.2002
     Modified: Илюха 11.11.2002 Процедура GetXattr вынесена в отдельный инклюдник
                                (может потребоваться в других местах)
     Modified:
*/
form "~n@(#) lnscheme.p 1.0 19/03/2002"
with frame sccs-id stream-io width 250.

{globals.i}    /* Глобальные расшаренные переменные сессии */
{svarloan.def} /* Глобальные расшаренные переменные модуля требует ln-nach.i */
{t-otch.i }    /* Таблицы: промежуточные и отчетов */

{intrface.get xclass} /* Инструменты для работы с метасхемой. */
{intrface.get comm}   /* Инструменты для работы с комиссиями. */
{intrface.get schem}  /* Инструменты для работы со схемами начисления. */
{intrface.get date}   /* Инструменты для работы с датами */
{intrface.get loan}   /* Инструменты для работы с договорами */

{ln-nach.i}    /* Инструменты для вызова схем начисления процентов по кредитам
                  требует svarloan.def */
/*{empty LnShPrm}*/

/* Исторически сложившиеся параметры (пока изменять нельзя) */
define input param ipContractChar as char no-undo. /* Назначение договора */
define input param ipContCodeChar as char no-undo. /* Номер договора */
define input param ipBegDate      as date no-undo. /* Дата начала расчета */
define input param ipEndDate      as date no-undo. /* Дата окончания расчета */
define input param dat-per        as date no-undo. /* Дата перехода на 39П */
define input param cod-par        as INT64  no-undo. /* Код параметра */
define input param fl-type-ost    as INT64  no-undo. /* Всегда передается 1 не исп. */

define var vAcctTypeChar as char no-undo. /* Роль основного счета */
define var vCountInt     as INT64 init 1 no-undo. /* Счетчик */
define var vPrmInt       as INT64  no-undo. /* Порядковый номер процента для расчета */
def var    vCalcString   as CHAR   no-undo.
def var    vInt          as INT64  no-undo . 

{&UTIME_BEG}
/* Динамика изменений условий в интервале дат */
RUN GetCondDate (ipContractChar, /* Назначение договора */
                 ipContCodeChar, /* Номер договора */
                 ipBegDate,      /* Начала интервала */
                 ipEndDate).     /* Окончание интервала */

/* Динамика схемы начисления в интервале дат */
RUN GetLnScheam (ipContractChar, /* Назначение договора */
                 ipContCodeChar, /* Номер договора */
                 ipBegDate,      /* Начала интервала */
                 ipEndDate).     /* Окончание интервала */


/* Необходимо убрать перебор по всем параметрам - достаточно ограничиться
количеством элементов в списке БазаНач */

 IF cod-par = ?
 THEN DO: 

   vCalcString = GetXattrValueEx("loan",
                                 ipContractChar + "," + ipContCodeChar,
                                 "БазаНач","").
   IF vCalcString = "" THEN
   DO:
      FIND FIRST loan WHERE loan.contract  EQ ipContractChar
                        AND loan.cont-code EQ ipContCodeChar
           NO-LOCK NO-ERROR.
      vCalcString = GetXattrInit(loan.class-code,"БазаНач").
   END.

   IF vCalcString <> '' AND vCalcString <> ?
   THEN vInt = NUM-ENTRIES(vCalcString).
   ELSE vInt = {&par-dim}.
 END.  
 ELSE 
    ASSIGN
      vCountint = cod-par
      vInt = cod-par .  
    
/* Отсюда (s1.p) и перебор по параметрам */
DO WHILE vCountInt le vInt:

/* для сложных случаев реструктуризации - усл.-анн. и усл.-диф. 
   нужно организовать разбивку по датам изменения типа погашения.
   предлагается такие договоры "окрашивать" ДР:АннДиф = ДА */


    /* Вызов схем начисление процентов по договорам
    ** по сформированной временной таблице */
    IF    dat-per LT ipBegDate 
       OR dat-per GE ipEndDate THEN
       RUN RunLnScheam (ipContractChar,  /* Назначение договора */
                        ipContCodeChar,  /* Номер договора */
                        ipBegDate,       /* Начало интервала начисления */
                        ipEndDate,       /* Окончание интервала начисления */
                        dat-per,         /* Дата перехода на 39П */
                        vCountInt,       /* Код параметра */
                        fl-type-ost).    /* Всегда передается 1 не исп. */
    ELSE DO:
       RUN RunLnScheam (ipContractChar,  /* Назначение договора */
                        ipContCodeChar,  /* Номер договора */
                        ipBegDate,       /* Начало интервала начисления */
                        dat-per,     /* Окончание интервала начисления */
                        dat-per,         /* Дата перехода на 39П */
                        vCountInt,       /* Код параметра */
                        fl-type-ost).    /* Всегда передается 1 не исп. */
       RUN RunLnScheam (ipContractChar,  /* Назначение договора */
                        ipContCodeChar,  /* Номер договора */
                        dat-per + 1,         /* Начало интервала начисления */
                        ipEndDate,       /* Окончание интервала начисления */
                        dat-per,         /* Дата перехода на 39П */
                        vCountInt,       /* Код параметра */
                        fl-type-ost).    /* Всегда передается 1 не исп. */
    END.
    vCountInt = vCountInt + 1.
END.

{&UTIME_END}

/* Очистка памяти. */
{intrface.del}
/*{empty LnShPrm}*/
return.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:48:23.934+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='lnscheme.p' */
/*prosignCtiWJo3XHn+MNQ3cB2ELDA*/