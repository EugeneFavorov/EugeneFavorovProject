/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: dbs_kb.p
      Comment: 
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 26/06/2017 pda
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "dbs_ip_bk"   /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc "dbs_bk"      /*для классификатора signat*/

{pac_doc_cl.p {&*}}