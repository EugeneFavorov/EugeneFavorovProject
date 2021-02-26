/*
               KSV Editor
    Copyright: (C) 2000-2009 Serguey Klimoff (bulklodd)
     Filename: LRCOND-BRW.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 01.10.2009 MUTA 0080978 Браузер условий договора на РКО
     Modified: 
*/

{globals.i}
{flt-file.i}
{intrface.get rights}
{intrface.get xclass}

DEFINE VAR vClassAvailChar AS CHAR NO-UNDO. /* Список доступных классов для чтения */

DEFINE VARIABLE vContract AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vContCode AS CHARACTER   NO-UNDO.

DEFINE VARIABLE vCond      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vRatePlan  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vRateName  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mWhere AS CHARACTER   NO-UNDO.
ASSIGN
   /* Получение списка доступных классов */
   vClassAvailChar   = GetRightClasses ("loanr-cond", "R")
.

ASSIGN
   vContract = GetFltVal("Contract")
   vContCode = GetFltVal("Cont-code")
.

mWhere = " WHERE " + 
   (IF vContract NE '*'  
      THEN "loan-cond.contract EQ '" + vContract + "' AND "  
      ELSE '') +                   
   (IF vContCode NE '*'                             
      THEN "loan-cond.cont-code EQ '" + vContCode + "' AND "  
      ELSE '') +                  
   (IF vClassAvailChar NE '*'                          
      THEN " CAN-DO('" + vClassAvailChar + "',loan-cond.class-code) "  
      ELSE "TRUE ").

FORM
   loan-cond.since      FORMAT "99/99/9999" COLUMN-LABEL "НАЧАЛО"                         HELP "Начало действия условий договора"
   vCond                FORMAT "x(15)"      COLUMN-LABEL "ДОП. СОГЛАШЕНИЕ"                HELP ""
   loan-cond.int-date   FORMAT "999"       COLUMN-LABEL "СРОК"                           HELP ""
   vRatePlan            FORMAT "x(15)"      COLUMN-LABEL "КОД ТАРИФНОГО!ПЛАНА"            HELP ""
   vRateName            FORMAT "x(28)"      COLUMN-LABEL "НАИМЕНОВАНИЕ ТАРИФНОГО!ПЛАНА"   HELP ""
   WITH FRAME browse1 TITLE COLOR bright-white "[ УСЛОВИЯ ДОГОВОРА: " + vContCode + " ]". 

{qrdef.i
  &buff-list        = "loan-cond"
  &need-buff-list   = "loan-cond" 
  &Join-list        = "EACH"  
  &fixed-where      = "mWhere"  
  &SortBy          = "'BY loan-cond.since'" 
}
    
{navigate.cqr
  &file             = "loan-cond"
  &avfile           = "loan-cond" 
  &files            = "loan-cond" 

  &maxfrm           = 1
  &bf1              = "loan-cond.since vCond loan-cond.int-date vRatePlan vRateName"

  &postfind         = "lrcond-brw.fnd "
  &oth3             = "frames.cqr "

  &class_avail      = "vClassAvailChar"
  &class_upper      = "'loanr-cond'"
  &edit             = "bis-tty.ef "
  &look             = "bis-tty.nav "
}    


{intrface.del}

