/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ЗАО "Банковские информационные системы"
     Filename: loanfrm.p
      Comment: Ввод начальной информации для фильтра по договорам.
   Parameters: "-1" - отмена ввода.
         Uses: all_flt.p
      Used by: 
      Created: 06/03/00 Om 
     Modified: 16/05/00 Om Ввод первого номера документа.
     Modified: 27/11/01 Om Доработка: обработка плановой даты.
     Modified: 27/11/01 Om Ошибка: при отмене ввода или ошибки при вводе
                                   окно ввода не закрывалось.
*/
form "~n@(#) loanfrm.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{flt_var.def} /* Определение Shared переменных */

all_settings.flt_first_num = 1.
svPlanDate = today.
return.
