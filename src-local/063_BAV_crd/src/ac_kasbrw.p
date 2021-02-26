/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: ac_kasbrw.p
      Comment: Метод browse для дополнительных соглашений к Договору РКО.
   Parameters: 
         Uses:
      Used BY:
      Created: 01.06.2016 Sami 0270184
     Modified:      
*/

{globals.i}
{flt-file.i}

DEFINE VARIABLE mCustName AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn      AS CHARACTER          NO-UNDO.

DEFINE VARIABLE fl-d          AS LOGICAL   NO-UNDO. /* */
DEFINE VARIABLE ridd          AS RECID     NO-UNDO. /* */
DEFINE VARIABLE fl-end        AS LOGICAL   NO-UNDO. /* Признак нажатия клавиши F8. */
DEFINE VARIABLE fl-go         AS LOGICAL   NO-UNDO. /* */

DEFINE VARIABLE mParentContract  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParentCont-Code AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustCat         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId          AS CHARACTER NO-UNDO.

ASSIGN
   mParentContract  = GetFltVal("Parent-Contract")
   mParentCont-Code = GetFltVal("Parent-Cont-Code")
.

FIND FIRST loan WHERE loan.Parent-Contract  EQ mParentContract
                  AND loan.Parent-Cont-Code EQ mParentCont-Code
   NO-LOCK NO-ERROR.

IF AVAILABLE loan THEN
   ASSIGN
      mCustCat = loan.cust-cat
      mCustId  = STRING(loan.cust-id)
   .
ELSE
   ASSIGN
      mCustCat = ""
      mCustId  = ""
   .
   

{qrdef.i
   &buff-list     = "loan"
   &join-list     = "EACH"
   &fields-mandatory = "parent-contract parent-cont-code "
   &sortBy        = "'BY loan.doc-num'"
}

FORM
   loan.doc-num    COLUMN-LABEL "ИНН получателя"          FORMAT "x(12)"
   loan.comment    COLUMN-LABEL "Наименование получателя" FORMAT "x(30)"
   loan.open-date  COLUMN-LABEL "Начало"                  FORMAT "99/99/99"
   loan.end-date   COLUMN-LABEL "Окончание"               FORMAT "99/99/99"
   loan.close-date COLUMN-LABEL "Закрыто"                 FORMAT "99/99/99"
   WITH FRAME browse1 TITLE "[ Доп.соглашения ]" 
   OVERLAY CENTERED.

RUN setattrfld("cont-code", "sort", "NO").

{navigate.cqr
   &file              = "loan"
   &files             = "loan"
   &avfile            = "loan"
   &filt              = YES
   &autosigns         = YES
   &edit              = "bis-tty.ef "
      &class_avail    = "'loanr_ac'"
   &look              = "bis-tty.nav "
   
   &bf1               = "loan.doc-num loan.comment loan.open-date loan.end-date loan.close-date"
   &tmprecid          = YES
   &print             = "ac_kasbrw.prn "
}   
/* 
 &oth1              = "flt-file.f6 "
&before-run-method = "csreqbrw.bfe "  
&print             = "csreqbrw.prn "*/

{intrface.del}
RETURN "".

