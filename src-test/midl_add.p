/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1999 ТОО "Банковские информационные системы"
     Filename: midl_add.p
      Comment: Поиск %-ой ставки, исходя из среднего остатка за период.
      Comment: Начисление % по найденной процентоной ставке
      Comment: на изменяющийся остаток.
      Comment: Таблица fost объявляется в infiltr.p, обнуление производится
      Comment: при каждом выборе нового счета.
   Parameters:
      Created: Om 30/08/99
     Modified: Om 01/09/99
     Modified: Om 02/11/99 "Входящий/исходящий" остаток через допрек.
     Modified: Корректировка вычисления среднего остатка.
     Modified: Om 18/11/99
     Modified: Om 09/11/00 Подключение инструмента поиска %% ставки.
     Modified: Om 06/12/00 Доработка: перевод на инструментарный уровень.
     Modified: Om 05/02/01 Доработка: корректировка входных параметров.
     Modified: Om 06/04/01 Доработка: обработка ошибки поиска комиссии.
*/

Form "~n@(#) midl_add.p 1.0 Om  30/08/99 Om 01/09/99 Om 06/12/00"
     with frame sccs-id stream-io width 250.

{globals.i}
{def_work.i} /* Определение таблицы fost */
{prn-ved.i &DefTempTable = "Объявляем временную таблицу"}


def input param rid1         as recid                   no-undo. /* Сх нач %% */
def input param in-commi     like commission.commission no-undo. /* Коммисия  */
def input param rid          as recid                   no-undo. /* Счет when acct */
def input param in_kau       like kau.kau               no-undo. /* Счет when avail kau */
def input param curr_beg     as date                    no-undo. /* Дата начала периода */
def input param curr_end     as date                    no-undo. /* Дата окончания  периода */
define var midl_val as decimal no-undo. /* Среднее значение за период */
define var nach_h   as handle  no-undo.
DEF var  ioXResult   AS DEC   NO-UNDO.
DEF var  ioDate      AS DATE  NO-UNDO.
DEF var  oXResultRef AS DEC   NO-UNDO. /* всегда 0 */
DEF  STREAM err.




/* Загрузка инструментария */
run load_nachtool (No, output nach_h).



/* Поиск комиссии */
run get_sch_line_by_rid in nach_h (rid1, buffer interest-sch-line).

/* Поиск счета */
run GET_ACCT_BY_RID in nach_h (rid, buffer acct).
if not avail acct
    then return "Счет не найден".

/* Динамика аналитических остатков по счету */
run CREATE_REAL_FOST in nach_h (rid, curr_beg, curr_end).

/* Поиск среднего остатка за период. */
run FIND_MIDL_REM in nach_h (curr_beg, 
                             curr_end, 
                             interest-sch-line.interest-month, 
                             output midl_val).

/* Комиссия */

run CREATE_RATE_MIN_AMT in nach_h (in-commi, rid, ?, midl_val, in_kau, curr_end).

if return-value ne ""
then do:
    /* Выгрузка инструментария */
    run remove_nachtool (No, nach_h).
    return return-value.
end.

/* Расчет начисления и формирование отчета */
run NACH_AND_REPORT in nach_h (interest-sch-line.interest-sch, 
                               acct.acct,
                               acct.currency, 
                               in_kau, 
                               curr_beg, 
                               curr_end,
                               interest-sch-line.interest-month, 
                               interest-sch-line.basis-time).

/* Выгрузка инструментария */
   FOR EACH nach_rep:
      ioXResult = ioXResult + nach_rep.acct_val_per.
   END.
   RUN MakeNachkinTTT.
run remove_nachtool (No, nach_h).

return "".



PROCEDURE MakeNachkinTTT:
   DEF VAR vOpID           AS CHAR NO-UNDO.
   DEF VAR vOpContractDate AS CHAR NO-UNDO.
   DEF VAR vSumRpoc        AS DEC  NO-UNDO.
   DEF VAR vFlagError      AS INT64  NO-UNDO.
   DEF VAR iFileNameDest  AS CHAR NO-UNDO.
   output stream err to "spooln.tmp" append.
   LOOP:
   FOR EACH nach_rep where nach_rep.acct_rem <> ?  BREAK BY nach_rep.intrvl_beg :
      CREATE nachkin-tt.
      ASSIGN                  
         nachkin-tt.BegDate   = nach_rep.intrvl_beg
         nachkin-tt.Days      = nach_rep.day_p_int
         nachkin-tt.EndDate   = nach_rep.intrvl_end
         nachkin-tt.Comm      = nach_rep.rate
         nachkin-tt.CommCode  = In-commi
         nachkin-tt.SummOst   = nach_rep.acct_rem
         nachkin-tt.SummProc  = nach_rep.acct_val_per
         nachkin-tt.Acct      = acct.acct
         nachkin-tt.Curr      = acct.currency
         nachkin-tt.BalAcct   = acct.bal-acct
      .
      PUT STREAM err 
         IF FIRST(nach_rep.intrvl_beg) THEN nachkin-tt.Acct ELSE "" FORMAT "x(25)"
         " "
         nach_rep.intrvl_beg
         " "
         nach_rep.intrvl_end
         " "
         nach_rep.day_p_int
         " "
         nach_rep.acct_rem  FORMAT ">>>,>>>,>>>,>>9.99Cr"
         " "
         IF LAST(nach_rep.intrvl_beg) THEN STRING (midl_val , ">>,>>>,>>>,>>9.99Cr") ELSE "" FORMAT "x(19)"
         FILL(" ", 14 - LENGTH(STRING(nach_rep.rate, ">>9.99999"))) + STRING(nach_rep.rate, ">>9.99999") FORMAT "x(14)"
         nach_rep.acct_val_per FORMAT ">>>,>>>,>>>,>>9.99Cr"
         " "
         IF LAST(nach_rep.intrvl_beg) THEN STRING (ioXResult , ">>,>>>,>>>,>>9.99Cr") ELSE "" FORMAT "x(19)"
         SKIP
      .
   END.
   output stream err  close.
END PROCEDURE.
