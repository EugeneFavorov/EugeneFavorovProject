{globals.i}

/* +++ pr_chk_ac.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:28pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: PR_CHK_AC.P
      Comment: Процедура контроля Акцепта для платежных требований
   Parameters: 
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:      
*/

DEFINE PARAMETER BUFFER bOp FOR op. 
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT YES.

DEFINE VARIABLE vChoice AS LOGICAL NO-UNDO INIT YES.

{globals.i}
{intrface.get xclass}
{intrface.get tmess} 

IF bop.doc-type EQ "02" THEN
DO:
   IF GetXAttrValueEx("op",STRING(bop.op),"КонтрАкцепт","") NE "Да" THEN
   DO:
      IF FGetSettingEx("СтандТр","КонтрАкцепт","", no) EQ "Да" THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0","Требуется контроль наличия заранее данного акцепта. Запустите транзакцию акцепта").
         oResult = NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess ("","","3","Требуется контроль наличия заранее данного акцепта. Запустите транзакцию акцепта|Продолжить,Отменить").
         IF pick-value EQ "2"
            THEN oResult = NO.         
      END.
   END.
END.

/* --- pr_chk_ac.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:28pm --- */
