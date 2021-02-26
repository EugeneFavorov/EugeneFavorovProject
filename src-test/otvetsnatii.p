/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sprssud.p
      Comment: печать справок "кредиты - Ответы на запросы заемщиков"
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 2/11/2017 pda
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "otv_obrem"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "spotven"      /*для классификатора signat был */

{precrdprint.p {&*}}