/*

Известная проблема.
- при расчёте процентов необходимо расчитывать не на сумму остатка , а на диаграмму остатка с даты последнего начисления.
*/
{globals.i}
{sh-defs.i new}
/*
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get tmess}
{intrface.get refer}
*/

DEFINE INPUT PARAMETER in-str   as char NO-UNDO.


DEF var iProdCode AS CHAR NO-UNDO.
DEF var iDate     AS DATE NO-UNDO.
DEF var iValParam AS CHAR NO-UNDO.
DEF var oResult   AS CHAR NO-UNDO.

DEFINE VAR tAcct      AS CHARACTER NO-UNDO.
DEFINE VAR tAcct474   AS CHARACTER NO-UNDO.
DEFINE VAR tToday     AS DATE      NO-UNDO.
DEFINE VAR tDateBeg   AS DATE      NO-UNDO.
DEFINE VAR tDateT     AS DATE      NO-UNDO.
DEFINE VAR tDateT1    AS DATE      NO-UNDO.
DEFINE VAR tlast-date AS DATE      NO-UNDO.
DEFINE VAR tRate      AS DEC       NO-UNDO.
DEFINE VAR tRateO     AS DEC       NO-UNDO.
DEFINE VAR tBal       AS DEC       NO-UNDO.
DEFINE VAR tBalCur    AS DEC       NO-UNDO.
DEFINE VAR tSumma     AS DEC       NO-UNDO.
DEFINE VAR tSumma01   AS DEC       NO-UNDO.
DEFINE VAR tSummaPP   AS DEC       NO-UNDO.
DEFINE VAR tSummaPPD  AS DEC       NO-UNDO.
DEFINE VAR tSummaPPO  AS DEC       NO-UNDO.
DEFINE VAR kDays      AS INT      NO-UNDO.
DEFINE VAR kDaysYear  AS INT      NO-UNDO.

DEFINE VAR tSumP01    AS DEC       NO-UNDO.
DEFINE VAR tSumP02    AS DEC       NO-UNDO.
DEFINE VAR tSumP03    AS DEC       NO-UNDO.
DEFINE VAR tSumP04    AS DEC       NO-UNDO.
DEFINE VAR tDb474     AS DEC       NO-UNDO.
DEFINE VAR tDb474D    AS DEC       NO-UNDO.
DEFINE VAR tOst421    AS DEC       NO-UNDO.
DEFINE VAR tOst421D   AS DEC       NO-UNDO.
                     
tAcct        = entry(1,in-str,"|").
tAcct474     = entry(2,in-str,"|").
tToday       = date(entry(3,in-str,"|")).
tRate        = dec(entry(4,in-str,"|")).
tRateO       = dec(entry(5,in-str,"|")).


/* 
tAcct = "42105810305000000072".
tToday = 06/23/2017.
tRate = 9.4.
tRateO = 11.8.
*/
pick-value = ?.



find first acct where acct.acct = tAcct no-lock no-error.
if not avail acct then return.
DEFINE TEMP-TABLE ttop-entry
   FIELD op-date    AS DATE
   FIELD contract-date    AS DATE
   FIELD acct-db    AS CHARACTER
   FIELD acct-cr    AS CHARACTER
   FIELD currency   AS CHARACTER
   FIELD amt        AS DEC
   FIELD amt-b      AS DEC
   FIELD op         AS INT64
   FIELD op-entry   AS INT64
   INDEX ind01 op-date.
DEFINE TEMP-TABLE ttbalance
   FIELD op-date    AS DATE
   FIELD contract-date    AS DATE
   FIELD acct       AS CHARACTER
   FIELD currency   AS CHARACTER
   FIELD bal        AS DEC
   INDEX ind01 op-date.

DEF BUFFER bttop-entry  for ttop-entry.                   
DEF BUFFER bttbalance   for ttbalance.                   
DEF BUFFER bttbalance1  for ttbalance.                   
DEF BUFFER bacct        for acct.                   
DEF BUFFER bacct1       for acct.                   
DEF BUFFER bacct474     for acct.                   




                   
for each op-entry where op-entry.acct-db eq acct.acct 
                    and op-entry.op-date <= tToday
                    and op-entry.op-status >= chr(251)
                    no-lock.
    find first op of op-entry.
    create ttop-entry.
    ASSIGN
       ttop-entry.op-date  = op-entry.op-date  
       ttop-entry.contract-date  = op.contract-date  
       ttop-entry.acct-db  = op-entry.acct-db  
       ttop-entry.acct-cr  = op-entry.acct-cr  
       ttop-entry.currency = op-entry.currency 
       ttop-entry.amt      = if op-entry.currency <> "" then op-entry.amt-cur else op-entry.amt-rub
       ttop-entry.op       = op-entry.op       
       ttop-entry.op-entry = op-entry.op-entry 
    .
    find first ttbalance where ttbalance.op-date = op-entry.op-date no-error.
    if not avail ttbalance then do:
       create ttbalance.
       ASSIGN
          ttbalance.op-date  =  op-entry.op-date   
          ttbalance.acct     =  acct.acct
          ttbalance.currency =  op-entry.curr
       .
    end.
end.
for each op-entry where op-entry.acct-cr eq acct.acct 
                    and op-entry.op-date <= tToday
                    and op-entry.op-status >= chr(251)
                    no-lock.
    find first op of op-entry.
    create ttop-entry.
    ASSIGN
       ttop-entry.op-date  = op-entry.op-date  
       ttop-entry.contract-date  = op.contract-date  
       ttop-entry.acct-db  = op-entry.acct-db  
       ttop-entry.acct-cr  = op-entry.acct-cr  
       ttop-entry.currency = op-entry.currency 
       ttop-entry.amt      = if op-entry.currency <> "" then op-entry.amt-cur else op-entry.amt-rub
       ttop-entry.op       = op-entry.op       
       ttop-entry.op-entry = op-entry.op-entry 
    .
    find first ttbalance where ttbalance.op-date = op-entry.op-date no-error.
    if not avail ttbalance then do:
       create ttbalance.
       ASSIGN
          ttbalance.op-date  =  op-entry.op-date   
          ttbalance.acct     =  acct.acct
          ttbalance.currency =  op-entry.curr
       .
    end.
end.
tlast-date = ?.
for each op-entry where op-entry.acct-cr eq tacct474
                    and op-entry.op-date <= tToday
                    and op-entry.op-status >= chr(251)
                    no-lock.
   find first op of op-entry.
   tlast-date = op.contract-date.
end.
/*
output to "ttbalance.txt" append.
FOR EACH ttbalance.
   export ttbalance.
end.
output close.
*/

tBal = 0.
tDateBeg = 01/01/1900.
find first ttop-entry.
tDateT    = ttop-entry.op-date.
tDateT1   = ttop-entry.contract-date.
for each ttop-entry.
/*
output to "ttbalance.txt" append.
export ttop-entry.
export tDateT tDateT1.
output close.
*/
    if tDateBeg < ttop-entry.op-date then do:
       tDateBeg = ttop-entry.op-date.
       find last bttbalance where bttbalance.op-date < ttop-entry.op-date no-error.
       if not avail bttbalance then  do:
          tBal  = 0 .
       end.
       else do:
          tBal  = bttbalance.bal.
       end.
       find first  ttbalance where  ttbalance.op-date = ttop-entry.op-date no-error.
       ttbalance.bal = tBal.
    end.
    if ttop-entry.acct-cr = acct.acct then do:
       if ttop-entry.acct-db begins "47426"  then do:
          find first bttbalance where bttbalance.op-date < ttop-entry.op-date no-error.
          find first  ttbalance where  ttbalance.op-date = ttop-entry.op-date no-error.
          if not avail bttbalance then  do:
             tBal  = 0 .
             tDateT = tDateBeg.                      /* дата начала интервала начисления */
             tDateT1 = ttop-entry.contract-date.     /* дата конца  интервала начисления */
             kDays = tDateT1 - tDateBeg + 1.
          end.
          else do:
             tBal   = bttbalance.bal.
             tDateT1 = ttop-entry.contract-date. /* дата конца  интервала начисления */
             kDays  = tDateT1 - tDateT.
          end.
          kDaysYear =  date("01/01/" + string(year(today) + 1,"9999")) -  date("01/01/" + string(year(today),"9999")) .
/*
          tSumma = round(tBal * kDays / kDaysYear * tRate / 100,2).
*/
/*
output to "ttbalance.txt" append.
export tSumma.
output close.
*/     
          run procent (INPUT TABLE bttbalance1,INPUT tDateT, INPUT tDateT1, INPUT tRate, OUTPUT tSumma).
          ttop-entry.amt-b = tSumma.
          find first  ttbalance where  ttbalance.op-date = ttop-entry.op-date no-error.
          ttbalance.bal = ttbalance.bal + tSumma. 
          ttbalance.contract-date  = ttop-entry.contract-date.
          tDateT = tDateT1. /* дата начала интервала начисления */
          next.
       end.
       else do:
          find first ttbalance where ttbalance.op-date = ttop-entry.op-date no-error.
          tBal  = ttbalance.bal + ttop-entry.amt.
          ttbalance.bal = ttbalance.bal + ttop-entry.amt.
          ttop-entry.amt-b = ttop-entry.amt.
          ttbalance.contract-date  = ttop-entry.contract-date.
          next.
       end.
    end.
    if ttop-entry.acct-db = acct.acct then do:
       if not ttop-entry.acct-cr begins "474" then do:
          find first ttbalance where ttbalance.op-date = ttop-entry.op-date no-error.
          ttbalance.bal = ttbalance.bal - ttop-entry.amt.
          ttop-entry.amt-b = ttop-entry.amt.
          ttbalance.contract-date  = ttop-entry.contract-date.
          next.
       end.
       else do:
          find first ttbalance where ttbalance.op-date = ttop-entry.op-date no-error.
          ttbalance.bal = ttbalance.bal - ttop-entry.amt.
          ttop-entry.amt-b = ttop-entry.amt.
          ttbalance.contract-date  = ttop-entry.contract-date.
          next.
       end.
    end.
end.

/*
for each ttop-entry.
   if ttop-entry.acct-db begins "474" then do:
      tAcct474 = ttop-entry.acct-db.
      leave.
   end.
end.
*/

/*
tDb474  = 0.
tDb474D = 0.
for each ttop-entry where ttop-entry.acct-db begins "474" .
   tDb474  = tDb474  + ttop-entry.amt.
   tDb474D = tDb474D + ttop-entry.amt-b.
end.
*/

/* Излишне начисленные проценты (разница дебетовых оборотов по 474) :*/

find first bacct where bacct.acct = tAcct474 no-lock no-error.
if not avail bacct then return.
run acct-pos in h_base (bacct.acct,bacct.currency,tToday,tToday,?).
tDb474 = - sh-bal.
/*
FIND LAST acct-pos where acct-pos.since LE tToday NO-LOCK NO-ERROR.
IF AVAIL acct-pos THEN DO: 
   tDateT = acct-pos.since.
end.
*/
/*
output to "ttbalance.txt" append.
export "tlast-date " tlast-date "tDateT1 " tDateT1 " tDateT " tDateT  " tToday - 1 " tToday - 1 .
output close.
*/

run procent (INPUT TABLE bttbalance1,INPUT tDateT1, INPUT tlast-date, INPUT tRate, OUTPUT tDb474D).

tSumP01 = tDb474 - tDb474D.

/* Излишне выплаченные проценты (разница остатков на 421)*/

find first bacct1 where bacct1.acct = acct.acct no-lock no-error.
if not avail bacct1 then return.
run acct-pos in h_base (bacct1.acct,bacct1.currency,tToday,tToday,?).
tOst421 = - sh-bal.
find last ttbalance no-error.
tOst421D  = ttbalance.bal.
tSumP02 = tOst421 - tOst421D.

/* Проценты начисленные по досрочной ставке */

tSumP03 = tDb474D.

/* начисление за один день. */
run procent (INPUT TABLE bttbalance1,INPUT tlast-date, INPUT tToday , INPUT tRate, OUTPUT tSumP04).

oResult = string(tSumP01) + ";" + string(tSumP02) + ";" + string(tSumP03) + ";" + string(tSumP04).
/*
output to "ttbalance.txt" append.
export "oResult=" oResult "tSumma=" tSumma "tSummaPP=" tSummaPP "tSummaPPD=" tSummaPPD "tSumP01=" tSumP01 "tSumP02=" 
tSumP02 "tSumP03=" tSumP03 "tOst421=" tOst421  "tOst421D=" tOst421D "tDb474=" tDb474 "tDb474D=" tDb474D "tSumP04=" tSumP04 .
output close.
*/

pick-value = oResult.
return.



PROCEDURE procent.
   DEFINE INPUT  PARAMETER TABLE FOR ttbalance.
   DEFINE INPUT  PARAMETER DateB as date NO-UNDO.
   DEFINE INPUT  PARAMETER DateE as date NO-UNDO.
   DEFINE INPUT  PARAMETER Rate  as dec  NO-UNDO.
   DEFINE OUTPUT PARAMETER SumPI  AS dec   NO-UNDO.
   DEF BUFFER bttbalance   for ttbalance.                   
   DEFINE VAR SumP       AS DEC       NO-UNDO.
   DEFINE VAR kDays      AS INT      NO-UNDO.
   DEFINE VAR kDaysYear  AS INT      NO-UNDO.
   DEFINE VAR tToday     AS DATE      NO-UNDO.
   DEFINE VAR tDateBeg   AS DATE      NO-UNDO.
   DEFINE VAR tDateEnd   AS DATE      NO-UNDO.
   DEFINE VAR tBal       AS DEC       NO-UNDO.
   DEFINE VAR kol_year   AS INT      NO-UNDO.
   DEFINE VAR i          AS INT      NO-UNDO.
      
   find last bttbalance where bttbalance.op-date = DateB no-lock no-error.
   if avail  bttbalance then do:
      tToday = bttbalance.op-date. 
      tBal   = bttbalance.bal.
   end.
   else do:
      find last bttbalance where bttbalance.op-date <= DateB no-lock no-error.
      tToday = DateB. 
      tBal   = bttbalance.bal.
   end.
/*
output to "ttbalance.txt" append.
export DateB DateE Rate.
export bttbalance.
*/
kol_year = year(DateE) - year(DateB).

  for each ttbalance where ttbalance.contract-date >  DateB 
                        and ttbalance.contract-date <= DateE 
                        no-lock. 


/*
      export ttbalance.
*/
      if kol_year = 0 then do:
         kDays     = ttbalance.op-date - tToday.
         kDaysYear = date("01/01/" + string(year(tToday) + 1,"9999")) -  date("01/01/" + string(year(tToday),"9999")) .
         SumP      = round(tBal * kDays / kDaysYear * tRate / 100,2).
         SumPI     = SumPI + SumP.
/*      
         export SumP SumPI tBal tToday ttbalance.op-date kDaysYear kDays tRate.
*/        
         tToday    = ttbalance.op-date.
         tBal      = ttbalance.bal.
      end.
      else do:
         do i = 1 to  kol_year + 1 :
            if i =  1 then do:
               tDateBeg  = tToday.
               tDateEnd  = date("31/12/" + string(year(DateB) + i - 1 ,"9999")).
            end.
            else do:
               tDateBeg  = date("01/01/" + string(year(DateB) + i - 1 ,"9999")).
               tDateEnd  = date("31/12/" + string(year(DateB) + i - 1 ,"9999")).
               if tDateEnd > DateE then do: 
                  tDateEnd = DateE.
               end.
            end.
            kDays     = tDateEnd - tDateBeg.
            kDaysYear = date("01/01/" + string(year(tDateBeg) + 1,"9999")) -  date("01/01/" + string(year(tDateBeg),"9999")).
            SumP      = round(tBal * kDays / kDaysYear * tRate / 100,2).
            SumPI     = SumPI + SumP.
/*
            export SumP SumPI tBal tToday ttbalance.op-date kDaysYear kDays tRate.
*/
            tToday    = ttbalance.op-date.
            tBal      = ttbalance.bal.
         end.
      end.
   end.


   if kol_year = 0 then do:
      kDays     = DateE - tToday.
      kDaysYear = date("01/01/" + string(year(tToday) + 1,"9999")) -  date("01/01/" + string(year(tToday),"9999")) .
      SumP      = round(tBal * kDays / kDaysYear * tRate / 100,2).
      SumPI     = SumPI + SumP.
/*
      export SumP SumPI tBal tToday DateE kDaysYear kDays tRate. 
*/
   end.
   else do:
      do i = 1 to  kol_year + 1 :
         if i =  1 then do:
            tDateBeg  = tToday.
            tDateEnd  = date("31/12/" + string(year(DateB) + i - 1 ,"9999")).
            kDays     = tDateEnd - tDateBeg.
         end.
         else do:
            tDateBeg  = date("01/01/" + string(year(DateB) + i - 1 ,"9999")).
            tDateEnd  = date("31/12/" + string(year(DateB) + i - 1 ,"9999")).
            if tDateEnd > DateE then do: 
               tDateEnd = DateE.
            end.
            kDays     = tDateEnd - tDateBeg + 1.
         end.
         kDaysYear = date("01/01/" + string(year(tDateBeg) + 1,"9999")) -  date("01/01/" + string(year(tDateBeg),"9999")).
         SumP      = round(tBal * kDays / kDaysYear * tRate / 100,2).
         SumPI     = SumPI + SumP.
/*         
         export SumP SumPI tBal tDateBeg tDateEnd  kDaysYear kDays tRate. 
*/
      end.
   end.
/*
   export SumP SumPI tBal tToday DateE kDaysYear kDays tRate. 
   output close.
*/
END PROCEDURE.



















