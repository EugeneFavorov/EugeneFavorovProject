{globals.i}
{intrface.get tmess}

/* +++ pr_daytoday2.p was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:58pm +++ */

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
DO:
   mblodd_char_Tmp01 = pick-value.
   RUN Fill-AlertSysMes IN h_tmess("","",4,"Вы пытаетесь проконтролировать документ в будущем дне!" + CHR(32) + "~n" + CHR(32) + "Проводить документ?").
   oResult = (pick-value = "YES").
   pick-value = mblodd_char_Tmp01.
END.

END.
ELSE oResult = YES.

oResult = IF oResult THEN oResult ELSE ?.

/* --- pr_daytoday2.p was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:58pm --- */
