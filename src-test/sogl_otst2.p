/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sogl_otst.p
      Comment: Печать соглашения об отступном
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 13/04/2018 zss шаблоны соглашения об отступном и АПП ТС (полное гашение)
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "soglotst2"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "sogl_otst"      /*для классификатора signat*/

{precrdprint.p {&*}}