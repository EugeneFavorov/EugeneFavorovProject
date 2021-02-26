/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1999 ТОО "Банковские информационные системы"
     Filename: is100.p
      Comment: Начисление процентов на реальный остаток на счете.
               Начисление будет производится только в том случае,
               если остаток на счете не снижался меньше минимального.
   Parameters:
      Created: Om 25/12/2003
     Modified:
*/

DEFINE INPUT PARAM in_par   AS char      NO-UNDO. /*  */
{globals.i}
{sh-defs.i new}

DEFINE var acct     AS char      NO-UNDO. /* Счет when acct */
DEFINE var curr_beg AS DATE      NO-UNDO. /* Дата начала периода */
DEFINE var curr_end AS DATE      NO-UNDO. /* Дата окончания  периода */
DEFINE var curr_cur AS DATE      NO-UNDO. /* Дата окончания  периода */

DEFINE var i        as INT64     no-undo .
DEFINE var j        as INT64     no-undo .
DEFINE VAR vWork    AS LOG       NO-UNDO.
DEFINE var iDate    as date      no-undo.

DEFINE var rid        AS RECID     NO-UNDO. /* Счет when acct */
define var midl_val    as decimal no-undo. 
define var nach_h      as handle  no-undo.
DEFINE var ioXResult   AS DEC     NO-UNDO.
DEFINE var ioDate      AS DATE    NO-UNDO.
DEFINE var oXResultRef AS DEC     NO-UNDO. 
DEFINE var vPeriod     AS INT64     NO-UNDO. 
DEFINE var vStavka     AS DEC     NO-UNDO. 
DEFINE var vActive     AS char     NO-UNDO. 

DEFINE var DB_         AS DATE    NO-UNDO.   /* Дата начала договора */                      
DEFINE var DE_         AS DATE    NO-UNDO.   /* Дата окончания договора*/                    
DEFINE var PB_         AS DATE    NO-UNDO.   /* Дата начала периода расчета процентов */     
DEFINE var PE_         AS DATE    NO-UNDO.   /* Дата начала окончания расчета процентов */   
DEFINE var RB_         AS DATE    NO-UNDO.   /* Начало интервала расчета */                  
DEFINE var RE_         AS DATE    NO-UNDO.   /* Окончание интервала расчета */               
DEFINE var SD_         AS int64   NO-UNDO.   /* Сдвиг на первый рабочий день */               

DEFINE var DateNO_     AS DATE    NO-UNDO.   /* Дата нарушения остатка */
DEFINE var SummaNO_    AS DEC     NO-UNDO.   /* Сумма нарушения остатка */
DEFINE var ost-t       as dec     NO-UNDO.


DEF  STREAM err.
output stream err to "spooln.tmp" append.

DEFINE  VARIABLE vMinlValDec AS DECIMAL NO-UNDO. /* Минимальный остаток. */


IF num-entries(in_par,"|") <> 3 then do:
   RETURN "Не верное число параметров".
end.       
acct     = entry(1,in_par,"|").
curr_beg = date(entry(2,in_par,"|")).
curr_end = date(entry(3,in_par,"|")).

find first acct where acct.acct  = acct  no-lock no-error.
IF NOT AVAILABLE acct
THEN DO:
   PUT stream err UNFORMATTED acct " Счет не найден" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   RETURN "Счет не найден".
END.
rid = recid(acct).
find loan-acct where loan-acct.acct      = acct.acct 
                 and loan-acct.curr      = acct.curr
                 and loan-acct.contract  = "Расчет"
                 and loan-acct.acct-type = "Расчет" 
                 no-lock no-error.
if not avail loan-acct then do:
   PUT stream err UNFORMATTED acct.acct " или нет договора по счёту или больше одного" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   RETURN RETURN-VALUE.
end.
for each loan-cond where loan-cond.contract  = loan-acct.contract
                     and loan-cond.cont-code = loan-acct.cont-code
                     no-lock.
   vActive     = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "Актив",?).     
   if vActive <> "ДА" then next.

   vMinlValDec = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "СуммаДог",?)).     
   vStavka     = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "ФактСтавка",?)).     
 
   run lwdayu.p(curr_end,output SD_).        
   DB_ = loan-cond.since + 1.                            /* Дата начала договора */
   DE_ = loan-cond.since + loan-cond.int-date + SD_.     /* Дата окончания договора*/
   PB_ = curr_beg.                                       /* Дата начала периода расчета процентов */
   PE_ = curr_end.                                       /* Дата начала окончания расчета процентов */
   RB_ = max(DB_,PB_).                                   /* Начало интервала расчета */
   RE_ = min(DE_,PE_).                                   /* Окончание интервала расчета */
   if (DB_ >= PB_ and DB_ <= PE_) or (DE_ >= PB_ and DE_ <= PE_) or (DB_ < PB_ and DE_ > PE_) then do:

      /* Получение минимального остатка, указывается в ДР "СуммаДог" на условии договора. */

      IF vMinlValDec EQ 0
      THEN DO:
          /* Выгрузка инструментария */
         PUT stream err UNFORMATTED acct.number " Минимальный остаток не найден" SKIP.
         PUT stream err UNFORMATTED " " SKIP.
         RETURN RETURN-VALUE.
      END.        	
      /* Проверка нарушения минимального остатка на счете */
      j = curr_end - curr_beg .
      do i = 0 to j.
          run acct-pos in h_base (acct.acct,acct.currency,curr_beg + i, curr_beg + i,?).
          ost-t = - sh-in-bal.
          if ost-t < vMinlValDec then do:
             PUT stream err UNFORMATTED
             'ДОП.СОГЛ.'         AT 1
             'ДАТА НАЧ.'         AT 21
             'МИНИМ.ОСТ'         AT 35
             'СРОК'              AT 49
             'СТАВКА'            AT 55
             SKIP.         
             PUT stream err UNFORMATTED
             loan-cond.cont-code  format "x(20)"
             loan-cond.since      format "99.99.9999"
             vMinlValDec format ">>,>>>,>>>,>>9.99Cr" 
             " "
             loan-cond.int-date
             " "
             vStavka format ">>9.99999"
             SKIP.         
             SummaNO_ = ost-t.
             DateNO_  = curr_beg + i.
             PUT stream err UNFORMATTED acct.number " Нарушен минимальный остаток. Дата нарушения остатка  "   DateNO_ " Сумма остатка " SummaNO_ SKIP.
             PUT stream err UNFORMATTED " " SKIP.
             pick-value =  string(DateNO_) + "|" + string(SummaNO_).
             RETURN pick-value.
          end. 
      end.
   END.
   RETURN "".
end.
return "".



    
