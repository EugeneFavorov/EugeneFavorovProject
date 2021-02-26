/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sogl_otst.p
      Comment: Печать соглашения об отступном
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 2/11/2017 pda
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "soglotstup"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "sogl_otst"      /*для классификатора signat*/

{precrdprint.p {&*}}