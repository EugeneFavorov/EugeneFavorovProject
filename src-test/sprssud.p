/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sprssud.p
      Comment: Печать справки об ОСЗ
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 2/11/2017 pda
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "sprssud"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "sprssud"      /*для классификатора signat*/

{precrdprint.p {&*}}