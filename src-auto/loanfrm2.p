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
/*
form "~n@(#) loanfrm.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{flt_var.def} /* Определение Shared переменных */

do transaction
on error  undo, leave
on endkey undo, leave:

    update 
        all_settings.flt_first_num
            label "Номер документа"
            help  "Номер первого документа"
        svPlanDate
            label "Плановая дата"
            help "Плановая дата документа"
    with frame in_flt 1 col overlay centered row 11 title "[ ИСХОДНЫЕ ДАННЫЕ ]".
end.

/* Закрываю фрейм */
hide frame in_flt no-pause.



/* Если ошибка или отмена ввода,
** то выхожу  */
if last-event:function ne "GO"
then return "-1".
*/

{flt_var.def} /* Определение Shared переменных */


/* in-op-date = today - 1.        */
IF NUM-ENTRIES(session:parameter) GE 8 THEN 
ASSIGN
   svPlanDate = IF ENTRY(8,session:parameter) NE "*" THEN DATE(ENTRY(8,session:parameter))  ELSE  today - 1.
ELSE 
   svPlanDate = today - 1.

all_settings.flt_first_num = 1.
  
return.
