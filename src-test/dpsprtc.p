/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: DPSPRT.P
      Comment: Просмотр меню процедур
         Uses:
      Created:
     Modified:
*/

{globals.i}
{tmprecid.def}

def var loan-type like class.name format 'x(23)' no-undo.
def var name-klient as char format 'x(20)'       no-undo.
def var filt1       as log                       no-undo.
def var vPosSinceDate as date                    no-undo.

def var in-class as char init 'dep_person_*'     no-undo.
def var num-line as INT64  init 0                  no-undo.

def buffer b-loan for loan.
def buffer f-loan for loan.

/* Для быстрого поиска по номеру вклада */
def var wh as widget-handle no-undo.
def var loan_srch like loan.cont-code no-undo.
def var old_loan_srch like loan.cont-code no-undo.

form
  loan.open-date   column-label "ОТКРЫТ"
  loan.doc-ref     column-label "НОМЕР ВКЛАДА" format 'x(20)'
  name-klient        column-label "КЛИЕНТ"       format 'x(19)'
  loan.close-date   column-label "ЗАКРЫТ"
with frame browse2 width 80 down
title color bright-white "[ ВКЛАДЫ ФИЗИЧЕСКИХ ЛИЦ ]".

{setdest.i {&*}}

for each tmprecid no-lock,
first loan where
  recid(loan) = tmprecid.id
with frame browse2:


  find last person where
                  person.person-id = loan.cust-id
                  no-lock no-error.
  if avail person then name-klient = name-last + " " + first-names.
  else                 name-klient = "?".

  vPosSinceDate = fGetLastClsDate(?,"*").
  FIND FIRST op-date WHERE op-date.op-date <= vPosSinceDate NO-LOCK NO-ERROR.
  color disp
    value(if avail(op-date) and op-date.op-date >= loan.end-date then "bright-red"
          else                                                          "normal")
            loan.end-date
    with frame browse2.


  disp
    loan.open-date
    loan.doc-ref
    name-klient
    loan.close-date
  .
  down.
end.

{preview.i {&*}}
