/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: INTER_PN.I
      Comment: Начисление % на входящий остаток с плавающей ставкой.
   Parameters:
         Uses:
      Used by:
      Created: 22.05.2009 19:30 feak    
     Modified: 22.05.2009 19:30 feak    
*/

{&offdef}
DEFINE VAR d1        AS DATE    NO-UNDO.
DEFINE VAR cur-date  AS DATE    NO-UNDO.
DEFINE VAR ndays     AS INT64 NO-UNDO.
DEFINE VAR dat-start AS DATE    NO-UNDO.
DEFINE VAR ost       LIKE acct-pos.balance    NO-UNDO.
DEFINE VAR stav      LIKE comm-rate.rate-comm NO-UNDO INIT 1.

DEFINE VAR vActualRate     AS DECIMAL NO-UNDO. /* Реальная ставка, на которую нужно умножать остаток */
DEFINE VAR vActualCommCode AS CHAR    NO-UNDO. /* Реальный код комиссии */

DEFINE VAR vStrStore          AS CHARACTER NO-UNDO.
DEFINE VAR vOutSystemProtocol AS LOGICAL   NO-UNDO.

DEFINE VAR vNew-comm LIKE comm-rate.rate-comm NO-UNDO.

DEF VAR vIsKap   AS  LOG  NO-UNDO.
DEF VAR vPevSum  AS  DEC  NO-UNDO.
DEF VAR vPevProc AS  DEC  NO-UNDO.
DEF VAR vDovSum  AS  DEC  NO-UNDO.
DEF VAR vProcSum AS  DEC  NO-UNDO.
DEF VAR vSign    AS  DEC  NO-UNDO.
DEF VAR vDvStav  AS  CHAR NO-UNDO.

vOutSystemProtocol = GetSysConf("РезультатПоВкладамВСисПротокол") EQ "Да".
{comment} */

vActualCommCode = in-commi.
IF    (GetSysConf("NachPls2TranId")   NE STRING(tGetParam("op-transaction",?,?))) 
   OR (GetSysConf("NachPls2ContCode") NE loan.cont-code)
THEN
DO:
   RUN SetSysConf IN h_base ("PrevOstDps",  "0").
   RUN SetSysConf IN h_base ("PrevProcDps", "0").
   RUN SetSysConf IN h_base ("NachPls2TranId",  STRING(tGetParam("op-transaction",?,?))).
   RUN SetSysConf IN h_base ("NachPls2ContCode", loan.cont-code).
END.

RUN Get_Last_Param in h_dpspc (RECID(loan),
                               end-date1,
                               end-date1,
                               "pen-commi",
                               OUTPUT vDvStav).

d1 = {&d-beg}.
dat-start = {&d-beg}.

DO WHILE {&d-beg} le {&d-end} {&undo}:
   
   FIND LAST {&sum} WHERE {&sum}.{&since} le d1 NO-LOCK NO-ERROR.
   ost = IF AVAILABLE {&sum} THEN {&sum}.{&balance}
                             ELSE 0.0.

   IF      ost LT 0 AND acct.side MATCHES "*П" THEN ost = - ost.
   ELSE IF ost LT 0 AND acct.side EQ      "А"  THEN ost = 0.
   ELSE IF ost GT 0 AND acct.side EQ      "П"  THEN ost = 0.

   FIND FIRST {&sum} WHERE {&sum}.{&since} gt d1
                       AND {&sum}.{&since} le {&d-end}
                     NO-LOCK NO-ERROR.
   /* -1, т.к. fost содержит входящие остатки на дату после изменения остатка */
   IF AVAIL {&sum} THEN cur-date = {&sum}.{&since} - 1.
                   ELSE cur-date = {&d-end}.

   IF {&rcom} EQ fGetSetting("ЦбРефПред", ?, "%ЦБреф") THEN 
   DO:
      {findcom1.i
         &dir=LAST
         &rsum=ost
         &since1=" > d1 and {&comm-rate}.since <= cur-date "
         {&*}
      }
      IF AVAIL {&comm-rate} THEN cur-date = {&comm-rate}.since - 1.
      RELEASE {&comm-rate}.
      /* Поиск комиссии, действующей в данном интервале постоянства остатка */
      {findcom1.i
         &dir=LAST
         &rsum=ost
         &since1=" LE cur-date "
         {&*}
      }

      IF AVAIL {&comm-rate} THEN RUN get_ref_nal(INPUT {&d-beg}, OUTPUT stav) .
      ELSE                       stav = 0.
   END.
   ELSE 
   DO:
       /* Поиск комиссии, действующей в данном интервале постоянства остатка. 
          Поиск необходим, чтобы опредедлить конкретное значение минимума по сумме*/
       {findcom1.i
          &dir=LAST
          &rsum=ost
          &since1=" <= cur-date "
          {&*}
       }
       
       IF AVAIL {&comm-rate} THEN 
       DO:
          comm-ost = {&comm-rate}.MIN-VALUE.
          RELEASE {&comm-rate}.
          
          /* Теперь ищем как плавающую ставку, но с известным мимимумом по сумме. 
          Иначе мы найдем ставку с другой суммой*/

          {findcom2.i
             &dir=FIRST
             &rsum=comm-ost
             &since1=" > d1 and {&comm-rate}.since <= cur-date "
             &z = "="
             {&*}
          }
          IF AVAIL {&comm-rate} THEN cur-date = {&comm-rate}.since - 1.
          RELEASE {&comm-rate}.
    
          /* Поиск комиссии, действующей в данном интервале постоянства остатка */
          {findcom2.i
             &dir=LAST
             &rsum=comm-ost
             &since1=" <= cur-date "
             &z = "="
             {&*}
          }
    
          IF AVAILABLE {&comm-rate} THEN 
          DO:
             /* Иногда нам очень нужно получить RECID comm-rate, но по умолчанию здесь
             ** нам возвращают RECID временной таблицы, созданной на основе comm-rate. 
             ** Объявление препроцессора find-cr включает поиск соответствующего comm-rate 
             ** и возврат именно его RECID */
             &IF DEFINED(cr-recid) EQ 0 &THEN
                PUBLISH "publish-comm" (RECID({&comm-rate}), {&comm-rate}.rate-comm).
             &ELSE
                PUBLISH "publish-comm" ({&comm-rate}.cr-recid, {&comm-rate}.rate-comm).
             &ENDIF             
          END.
       END.
   END.

   ndays = cDay(interest-sch-line.interest-month,d1,cur-date + 1).
   
   IF AVAIL {&comm-rate} THEN DO:
      vActualRate = {&comm-rate}.rate-comm.
      
      &IF DEFINED (correctsh) &THEN 
      /* Если дата окончания меньше даты окончания периода начисления по данной ставке - 
         ищем сумму процентов через штрафную ставку */
      IF {&comm-rate}.end-per NE ? AND /* дата окончания периода установлена (иначе не с чем будет сравнивать) */
         gend-date <  loan.end-date AND /* мы делаем досрочное закрытие, а не в срок */
         gend-date >= {&comm-rate}.since AND gend-date < {&comm-rate}.end-per /* мы выполняем расчет в последнем периоде, причем РАНЬШЕ конца этого периода */
      THEN DO:
          RUN Get_Last_Param in h_dpspc (RECID(loan),
                                     end-date1,
                                     end-date1,
                                     "pen-commi",
                                     OUTPUT mPenCommi).
          &IF defined(PLUS-Bank) ne 0 &THEN
          IF {assigned mShtrIz} THEN 
             mPenCommi = in-commi.
          {findcom2.i
              &rcom       = mPenCommi
              &comm-rate  = comm-rate
              &dir        = LAST
              &rsum       = comm-ost
              &since1     = " <= dat-commi "
              &z          = "="
              &vPeriodInt = "cDay(interest-sch-line.interest-month,{&dat-comm},cur-date + 1)"

              {&*}
          }
          &ELSE
          {findcom2.i
              &rcom      = mPenCommi
              &comm-rate = comm-rate
              &dir       = LAST
              &rsum      = comm-ost
              &since1    = " <= cur-date "
              &z         = "="
              {&*}
          }
          &ENDIF
          vActualCommCode = mPenCommi.
          vActualRate     = IF AVAILABLE comm-rate THEN comm-rate.rate-comm ELSE 0.00.
      END.
      &ENDIF
   END.
   ELSE vActualRate = 0.

   /* Делаем заглушку для определения даты капитализации */
   vIsKap = NO.
   FOR EACH kau-entry WHERE kau-entry.kau EQ SUBST("dps,&1,НачПрС1", loan.cont-code)
                        AND kau-entry.op-status NE 'А'
                      NO-LOCK,
      FIRST op WHERE op.op            EQ kau-entry.op
                 AND op.contract-date EQ cur-date
               NO-LOCK:

      vIsKap = YES.
      LEAVE.
   END.

   /* Ищем в день окончания периода довложения */
   vDovSum = 0.
   FOR EACH kau-entry WHERE kau-entry.kau       EQ SUBST("dps,&1,ОстВклС", loan.cont-code)
                        AND kau-entry.op-status NE 'А'
                      NO-LOCK,
      FIRST op WHERE op.op            EQ kau-entry.op
                 AND op.contract-date EQ cur-date
               NO-LOCK:

      vSign = IF kau-entry.debit EQ YES THEN -1
                                        ELSE  1.
      vDovSum = vDovSum + vSign * kau-entry.amt-rub.
   END.

   vPevSum  = DEC(GetSysConf("PrevOstDps"))  NO-ERROR.
   vPevProc = DEC(GetSysConf("PrevProcDps")) NO-ERROR.
   IF vDvStav EQ mPenCommi THEN
   DO:
      IF NOT((vPevSum EQ 0) AND (d1 - loan.open-date EQ 1)) THEN
         ost = vPevSum.
   END.

   vProcSum = round(ost * vActualRate * ndays * stav / (interest-sch-line.basis-time * 100),2).
   {&proc} = {&proc}  + vProcSum.
   IF vIsKap EQ YES THEN
      vPevSum = 0.
   ELSE
      vPevSum = vPevSum  + vProcSum.

   /* Учли довложения */
   vPevSum  = ost + vDovSum.

   IF vIsKap THEN
   DO:
      /* Учли проценты при капитализации */
      vPevSum  = vPevSum  + vProcSum + vPevProc.
      /* Обнулили проценты при капитализации*/
      vPevProc = 0.
   END.
   ELSE
      vPevProc = vProcSum + vPevProc.
/* MESSAGE vIsKap SKIP vPevProc SKIP vProcSum SKIP cur-date VIEW-AS ALERT-BOX. */
   RUN SetSysConf IN h_base ("PrevOstDps",  STRING(vPevSum)).
   RUN SetSysConf IN h_base ("PrevProcDps", STRING(vPevProc)).

   IF vOutSystemProtocol THEN 
   DO:
      vStrStore = '&next1'      + ',' +
                  STRING(ndays) + ',' +
                  STRING(ost)   + ',' +
                  STRING(vActualRate * stav) + ',' +
                  STRING(ost * vActualRate * ndays * stav / (interest-sch-line.basis-time * 100))  + ',' + 
                  STRING(d1) + ',' + 
                  vActualCommCode.
      IF {&rcom} NE fGetSetting("ЦбРефПред", ?, "%ЦБреф") THEN
         RUN SaveDataProtocol IN h_base ("ВкладНачисл%",vStrStore).
      ELSE
         RUN SaveDataProtocol IN h_base ("ВкладНачислРеф%",vStrStore).
   END.

   &IF DEFINED(offprint) = 0 &THEN 
   {intr_crn.i 
      &sum-res="ost * vActualRate * ndays * stav / (interest-sch-line.basis-time * 100)"
      &Com-res="vActualRate" 
      &Nachkin-tt="Nchk-tt"
      &in-commi="vActualCommCode"
   }
   &ENDIF
   
   d1 = cur-date.
   {&d-beg} = d1.

   /*~~~~~~~~~~~~~~~~~~~~~~~~~ Печать в поток ~~~~~~~~~~~~~~~~~~~~~~*/
   IF d1 ne {&d-end} and fl-print THEN 
   DO:
      vStrStore = '&next1'      + ',' +
                  STRING(ndays) + ',' +
                  STRING(ost)   + ',' +
                  STRING(vActualRate * stav) + ',' +
                  STRING(ost * vActualRate * ndays * stav / (interest-sch-line.basis-time * 100)) + ',' +
                  STRING(dat-start) + ',' +
                  vActualCommCode.
      RETURN vStrStore.
   END.
   
   /* переходим к следующей итерации цикла */
   d1 = cur-date + 1.
   {&d-beg} = d1.
END.

{&d-beg} = {&d-beg} - 1.
vStrStore = '&next1'      + ',' +
            STRING(ndays) + ',' +
            STRING(ost)   + ',' +
            STRING(vActualRate * stav)  + ',' +
            STRING(ost * vActualRate * ndays * stav / (interest-sch-line.basis-time * 100)) + ',' +
            STRING(dat-start) + ',' +
            vActualCommCode.
RETURN vStrStore.

