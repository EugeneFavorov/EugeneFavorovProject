/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: dpsp-day.p
      Comment: Список открытых (закрытых) счетов
   Parameters:
         Uses:
      Used by:
      Created: 23.04.1998 mkv
     Modified: 13.04.99 by Om
     Modified: 15.04.99 by Om Добавлена сортировка по счету
*/

{globals.i}
{getdates.i}

DEFINE VARIABLE vName1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vName2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vInn   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFIO   AS CHARACTER   NO-UNDO.
def var a-count as INT64 init 1 no-undo.
def var f_date as logical no-undo.

def var sel_loan as char no-undo. /* Отобранные договора */

def stream err.

form
        acct.open-date  column-label "Дата"
        acct.acct       column-label "Счета"
      loan-acct.cont-code format "x(25)"	column-label "Номер договора"
      vFIO format "x(30)" column-label "ФИО"
    /*    header dept.name-bank format "x(80)" "Стр.:" at 81
                page-number (err) format ">>9" at 77 skip (1)
                space (5) caps("Список открытых и закрытых счетов.") form "x(35)"
                skip space (2) "В период с:" beg-date "по" end-date skip
                space (13) "Дата выполнения:" string(today) 
                skip " "*/
with frame www down width 400. 

{setdest.i &stream="stream err"}

/* По открытым счетам */
next_acct:
for each acct where acct.open-date ge beg-date 
                and acct.open-date le end-date
                and acct.filial-id EQ shfilial 
                and acct.close-date eq ? no-lock,
    each loan-acct of acct where loan-acct.contract eq "DPS"
                             and (loan-acct.acct-type eq "loan-dps-p"
                              or  loan-acct.acct-type eq "loan-dps-t")
                              no-lock
                                       
                              
    break by acct.open-date 
          by acct.acct 
    with frame www: 
  RUN GetCustName IN h_base (Acct.cust-cat, Acct.cust-id, "",
                           OUTPUT vName1, OUTPUT vName2, INPUT-OUTPUT vInn).
                                                         vFIO = TRIM(vName1 + " " + vName2). 
        if first (acct.open-date) then do:
                sel_loan = loan-acct.acct.
                display stream err
                        caps("Открытые счета") @ acct.acct.
                DOWN STREAM err.
                DOWN STREAM err 1.
                DISPLAY STREAM err 
                        acct.open-date 
                        acct.acct
                        loan-acct.cont-code
                        vFIO.
                DOWN STREAM err.
        end.
        
        if not can-do (sel_loan,loan-acct.acct) then
                sel_loan = sel_loan + "," + loan-acct.acct.
        else next next_acct.

        if first-of (acct.open-date) then down stream err 1.

        display stream err
                acct.open-date
                acct.acct
                loan-acct.cont-code
                vFIO.
        down stream err.
end.

/* По закрытым счетам */
next_acct:
for each acct where acct.close-date ne ?
                and acct.close-date ge beg-date 
                and acct.close-date le end-date
                and acct.filial-id EQ shfilial 
                no-lock,
    each loan-acct of acct where loan-acct.contract eq "DPS"
                             and (loan-acct.acct-type eq "loan-dps-p"
                              or  loan-acct.acct-type eq "loan-dps-t")
                              no-lock
    break by acct.close-date 
          by acct.acct
    with frame www:
RUN GetCustName IN h_base (Acct.cust-cat, Acct.cust-id, "",
                           OUTPUT vName1, OUTPUT vName2, INPUT-OUTPUT vInn).
                                                         vFIO = TRIM(vName1 + " " + vName2).
        if first (acct.close-date) then do:
                sel_loan = loan-acct.acct.
                
                down stream err 1.
                display stream err
                        caps("Закрытые счета") @ acct.acct.
                DOWN STREAM err.
                DOWN STREAM err 1.
                DISPLAY STREAM err 
                        acct.close-date @ acct.open-date 
                        acct.acct
                        loan-acct.cont-code
                        vFIO.
                DOWN STREAM err.
        end.

        if not can-do (sel_loan,loan-acct.acct) then
                sel_loan = sel_loan + "," + loan-acct.acct.
        else next next_acct. 

        if first-of (acct.close-date) then down stream err 1.

        display stream err
                acct.close-date @ acct.open-date
                acct.acct
                loan-acct.cont-code
                vFIO.
        down stream err.
end.

find _user where _user._userid eq userid('bisquit') no-lock no-error.
display stream err skip (2) "Исполнитель:" 
        "__________________" at 20
        _user._user-name format "x(30)"  no-label at 41.

{preview.i &stream="stream err"}
