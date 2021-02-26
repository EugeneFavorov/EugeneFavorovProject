/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sprssud.p
      Comment: печать справок "кредиты - Ответы на запросы заемщиков"
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 29/03/2018 zss
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "otv_pd_ot"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "spotven"      /*для классификатора signat был */

{precrdprint.p {&*}}