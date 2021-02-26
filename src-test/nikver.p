/*              Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename:  imds-cl.p
      Comment:  Создание клиентов физ. или юр. лиц
         Uses:  -
      Used by:  
      Created:  18/02/1999 Gusev
*/
{globals.i}
{pick-val.i}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}

DEF STREAM s_imp.

session:date-format = "dmy".
def var now_ like pl_indocsreg.CREATE_DATE.
def var vOk  as logic.

def temp-table dscl no-undo
  field tnum        as char
  field tcont-code  as char
  field tsince      as char
  field tNAME       as char
  field tdetails    as char
  field tdate-get   as char
  field tincident   as char
  field tuser_p     as char
  field tuser       as char
.


{getfile.i &filename='"pts.csv"' &mode=must-exist}

INPUT STREAM s_imp FROM VALUE(fname).
REPEAT ON ERROR UNDO, RETRY:
   create dscl. 
   IMPORT STREAM  s_imp delimiter ";" dscl NO-ERROR.
END.
/*
? 2016-03-24T14:46:39.779 24/03/16 0 "ПостАрх" "0000" "O0400MVA" "ПТС" "ПТС сдан с отступным. ПТС дубликат" "22-00-43998-АПНА@0000 0" "term-obl" ""
*/
disable triggers for load of pl_indocsreg.
output to "nikver.err".
now_ = now.
for each dscl.
    now_ = now_ + 52000.
    export dscl.
    export now_.

    find first loan where loan.contract = "Кредит"
                      and loan.cont-code begins dscl.tcont-code
                      no-lock no-error.
    if not avail loan then do:
       export " Договор "  dscl.tcont-code " не найден". 
       next.
    end. 
    create pl_indocsreg.
    assign
       pl_indocsreg.CREATE_DATE  = now_
       pl_indocsreg.DATE_VALUE   = date(dscl.tdate-get)
       pl_indocsreg.EVENT        = dscl.tincident
       pl_indocsreg.BRANCH_ID    = shFilial
       pl_indocsreg.USER_ID      = dscl.tuser
       pl_indocsreg.DOC_TYPE     = "ПТС"
       pl_indocsreg.DETAILS      = dscl.tdetails
       pl_indocsreg.SURROGATE    = dscl.tcont-code + "@" + shfilial + " 0"
       pl_indocsreg.FILE_NAME    = "term-obl"
       pl_indocsreg.DESCRIPTIONS = dscl.tuser_p
   .
   vOk =  UpdateSigns("loan","Кредит" + "," + dscl.tcont-code + "@" + shfilial,"ЗагруженПТС","ПТС",yes).
end.
output close.


