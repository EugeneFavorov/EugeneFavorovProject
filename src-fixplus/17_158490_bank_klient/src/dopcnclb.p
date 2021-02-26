/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: dopcnclb.p
      Comment: Собственная форма просмотра классификатора 
               "ДопКонтрКЛБ"
   Parameters: Нет
         Uses:
      Used by:
      Created: 06.08.2015 IT    
*/

&GLOBAL-DEFINE form-brw1 dopcnclb.br1
&GLOBAL-DEFINE bf11 "code.code code.name code.misc[1] code.misc[2] code.misc[3] code.val  "
&GLOBAL-DEFINE form-edit dopcnclb.edf
&GLOBAL-DEFINE postfind1 pclassdu.fnd
&GLOBAL-DEFINE lookup1 dopcnclb.nau
&GLOBAL-DEFINE eh dopcnclb.eh~032

DEFINE VARIABLE mLogCh AS CHARACTER NO-UNDO INITIAL "действует,не действует".

{pclass.p dopcnclb.uf dopcnclb.nav}

PROCEDURE PostOpenQuery:
   DEFINE INPUT  PARAMETER iH AS HANDLE     NO-UNDO.
   IF n-frm EQ 1 THEN
   DO:
      h-frm[n-frm]:TITLE = "[ " + CAPS(in-title) + " (ДопКонтрКЛБ)" + " ]".
   END.
END PROCEDURE.

