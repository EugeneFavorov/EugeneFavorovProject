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
{globals.i}
{def_work.i} /* Определение таблицы fost */

DEFINE INPUT PARAM rid1     AS RECID     NO-UNDO. /* Сх нач %% */
DEFINE INPUT PARAM in-commi AS CHARACTER NO-UNDO. /* Комисcия  */
DEFINE INPUT PARAM rid      AS RECID     NO-UNDO. /* Счет when acct */
DEFINE INPUT PARAM in_kau   AS CHARACTER NO-UNDO. /* Счет when avail kau */
DEFINE INPUT PARAM curr_beg AS DATE      NO-UNDO. /* Дата начала периода */
DEFINE INPUT PARAM curr_end AS DATE      NO-UNDO. /* Дата окончания  периода */


define var midl_val    as decimal no-undo. 
define var nach_h      as handle  no-undo.
DEFINE var ioXResult   AS DEC     NO-UNDO.
DEFINE var ioDate      AS DATE    NO-UNDO.
DEFINE var oXResultRef AS DEC     NO-UNDO. 
DEFINE var vPeriod     AS INT64     NO-UNDO. 
DEFINE var vStavka     AS DEC     NO-UNDO. 

DEFINE var DB_         AS DATE    NO-UNDO.   /* Дата начала договора */                      
DEFINE var DE_         AS DATE    NO-UNDO.   /* Дата окончания договора*/                    
DEFINE var PB_         AS DATE    NO-UNDO.   /* Дата начала периода расчета процентов */     
DEFINE var PE_         AS DATE    NO-UNDO.   /* Дата начала окончания расчета процентов */   
DEFINE var RB_         AS DATE    NO-UNDO.   /* Начало интервала расчета */                  
DEFINE var RE_         AS DATE    NO-UNDO.   /* Окончание интервала расчета */               
DEFINE var SD_         AS int64   NO-UNDO.   /* Сдвиг на первый рабочий день */               

DEFINE var DateNO_     AS DATE    NO-UNDO.   /* Дата нарушения остатка */
DEFINE var SummaNO_    AS DEC     NO-UNDO.   /* Сумма нарушения остатка */

DEF  STREAM err.
output stream err to "spooln.tmp" append.

DEFINE  VARIABLE vMinlValDec AS DECIMAL NO-UNDO. /* Минимальный остаток. */

/* Загрузка инструментария */
RUN load_nachtool (NO, OUTPUT nach_h).
/* Поиск комиссии */
RUN get_sch_line_by_rid IN nach_h (rid1, BUFFER interest-sch-line).
/* Поиск счета */
RUN GET_ACCT_BY_RID IN nach_h (rid, BUFFER acct).
IF NOT AVAILABLE acct
THEN DO:
    /* Выгрузка инструментария */
   PUT stream err UNFORMATTED rid " Счет не найден" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   RUN remove_nachtool (NO, nach_h).
   RETURN "Счет не найден".
END.

find loan-acct where loan-acct.acct      = acct.acct 
                 and loan-acct.curr      = acct.curr
                 and loan-acct.contract  = "Расчет"
                 and loan-acct.acct-type = "Расчет" 
                 no-lock no-error.
if not avail loan-acct then do:
   PUT stream err UNFORMATTED acct.acct " или нет договора по счёту или больше одного" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   output stream err  close.
   RUN remove_nachtool (NO, nach_h).
   RETURN RETURN-VALUE.
end.
output stream err  close.

for each loan-cond where loan-cond.contract  = loan-acct.contract
                     and loan-cond.cont-code = loan-acct.cont-code
                     no-lock.
    output stream err to "spooln.tmp" append.

   vMinlValDec = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "СуммаДог",?)).     
   vStavka     = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "ФактСтавка",?)).     
   PUT stream err UNFORMATTED
   'НОМЕР ДОГОВОРА'    AT 1
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
         RUN remove_nachtool (NO, nach_h).
         output stream err  close.
         RETURN RETURN-VALUE.
      END.
      
      /* Динамика аналитических остатков по счету */
      RUN CREATE_REAL_FOST IN nach_h (rid, RB_, RE_).
      
      /* Проверка нарушения минимального остатка на счете */
      RUN CheckErrorRemainder IN nach_h (vMinlValDec).
      midl_val = vMinlValDec.
      IF RETURN-VALUE NE ""
      THEN DO:
         RUN CheckErrorRemainderDate (vMinlValDec, OUTPUT DateNO_,OUTPUT SummaNO_) .  
          /* Выгрузка инструментария */
         PUT stream err UNFORMATTED acct.number " Нарушен минимальный остаток. Дата нарушения остатка  "   DateNO_ " Сумма остатка " SummaNO_ SKIP.
         PUT stream err UNFORMATTED " " SKIP.
         output stream err  close.
         RUN remove_nachtool (NO, nach_h).
         RETURN RETURN-VALUE.
      END.
      vPeriod = loan-cond.int-date.
      /* Комиссия */
      
      RUN CREATE_RATE_CR IN nach_h (in-commi, rid, ?,in_kau,midl_val,vPeriod,RE_).
      
      IF RETURN-VALUE NE ""
      THEN DO:
          /* Выгрузка инструментария */
         PUT stream err UNFORMATTED acct.number " " in-commi "  Не найдена комиссия"  " " RETURN-VALUE SKIP.
         PUT stream err UNFORMATTED " " SKIP.
         RUN remove_nachtool (NO, nach_h).
         output stream err  close.
         RETURN RETURN-VALUE.
      END.
      
      /* Расчет начисления и формирование отчета */
      
      RUN NACH_AND_REPORT IN nach_h (interest-sch-line.interest-sch, 
                                     acct.acct,
                                     acct.currency, 
                                     in_kau, 
                                     RB_, 
                                     RE_,
                                     interest-sch-line.interest-month, 
                                     interest-sch-line.basis-time).
      
      FOR EACH nach_rep:
         ioXResult = ioXResult + nach_rep.acct_val_per.
      END.
      output stream err  close.
      RUN MakeNachkinT01.
      run remove_nachtool (No, nach_h).
   end.
end.


return "".



PROCEDURE MakeNachkinT01:
   DEF VAR vOpID           AS CHAR NO-UNDO.
   DEF VAR vOpContractDate AS CHAR NO-UNDO.
   DEF VAR vSumRpoc        AS DEC  NO-UNDO.
   DEF VAR vFlagError      AS INT64  NO-UNDO.
   DEF VAR iFileNameDest  AS CHAR NO-UNDO.
   output stream err to "spooln.tmp" append.
   LOOP:
   FOR EACH nach_rep where nach_rep.match_rem <> ? BREAK BY nach_rep.intrvl_beg  :
      IF FIRST(nach_rep.intrvl_beg) then do:
         PUT stream err UNFORMATTED
         'СЧЕТ'              AT 1
         'С'                 AT 27
         'ПО'                AT 36
         'КОЛ-ВО ДНЕЙ'       AT 44
         'ОСТАТОК'           AT 63
         'СТАВКА'            AT 83
         'НАЧИСЛЕНО'         AT 101
         'ИТОГО'             AT 121
         SKIP.         
         PUT stream err UNFORMATTED FILL("-", 128) SKIP.
      end.
      PUT STREAM err 
         IF FIRST(nach_rep.intrvl_beg) THEN acct.Acct ELSE "" FORMAT "x(25)"
         " "
         nach_rep.intrvl_beg
         " "
         nach_rep.intrvl_end
         " "
         nach_rep.day_p_int
         " "
         nach_rep.match_rem format ">>,>>>,>>>,>>9.99Cr" 
         " "
         FILL(" ", 14 - LENGTH(STRING(nach_rep.rate, ">>9.99999"))) + STRING(nach_rep.rate, ">>9.99999") FORMAT "x(14)"
         nach_rep.acct_val_per FORMAT ">>>,>>>,>>>,>>9.99Cr"
         " "
         IF LAST(nach_rep.intrvl_beg) THEN STRING (ioXResult , ">>,>>>,>>>,>>9.99Cr") ELSE "" FORMAT "x(19)"
         SKIP
      .
   END.
   PUT stream err UNFORMATTED " " SKIP.
   output stream err  close.
END PROCEDURE.

PROCEDURE CheckErrorRemainderDate.

    define input  param ipMinRemainderDec as decimal no-undo. /* Минимальны остаток */
    DEFINE OUTPUT PARAM Date  AS date NO-UNDO.
    DEFINE OUTPUT PARAM Summa AS dec  NO-UNDO.
    find first  fost where  fost.balance lt ipMinRemainderDec no-lock no-error.
    if avail fost then do: 
        date  = fost.since.
        summa = fost.balance.
        return "Нарушение минимального остатка по счету " +
                trim(string(ipMinRemainderDec,">>>,>>>,>>>,>>>,>>9.99")) + ".".
    end.
    return.

END PROCEDURE.
