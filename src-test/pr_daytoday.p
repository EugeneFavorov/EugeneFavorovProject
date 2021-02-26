/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ТОО "Банковские информационные системы"
     Filename: pr_setdr_op.p
      Comment: Процедура контроля   
   Parameters:
         Uses:
      Used by:
      Created: 19.08.2010 MUTA 0119107 
     Modified: 
*/

{globals.i}
{intrface.get xclass}

DEFINE PARAMETER BUFFER bOp FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT NO.


IF bop.op-date > today 
THEN 
DO:
/*MESSAGE "Вы пытаетесь проконтролировать документ в будущем дне!!!" VIEW-AS ALERT-BOX.*/
MESSAGE "Вы пытаетесь проконтролировать документ в будущем дне!" SKIP 
  	"Проводить документ?" view-as alert-box warning buttons yes-no update oResult.
END.
ELSE oResult = YES.
