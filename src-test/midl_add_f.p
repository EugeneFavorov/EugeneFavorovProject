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
DEF  STREAM err.
output stream err to "spooln.tmp".
   PUT stream err UNFORMATTED
       'СЧЕТ'              AT 1
       'С'                 AT 25
       'ПО'                AT 35
       'КОЛ-ВО ДНЕЙ'       AT 44
       'ОСТАТОК'           AT 63
       'СРЕДНИЙ ОСТАТОК'   AT 78
       'СТАВКА'            AT 102
       'НАЧИСЛЕНО'         AT 120
       'ИТОГО'             AT 140
       SKIP.         
   PUT stream err UNFORMATTED FILL("-", 147) SKIP.
   output stream err  close.

return "".
