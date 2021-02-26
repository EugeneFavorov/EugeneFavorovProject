/*
               Банковская интегрированная система БИСквит
    Copyright: (c) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: nach_pls.p
      Comment: Схема начисления nach_pls.p 
               На основе nach_ins.p
   Parameters: rid1 in-commi rid beg end-date1 str-kau fl-print xresult beg-date1 xresult-ref
         Uses:
      Used by:
      Created: 22.05.2009 19:30 feak    
     Modified: 22.05.2009 19:30 feak    
*/
&SCOP PLUS-Bank YES
{globals.i}     /* Знают все */
{sh-defs.i}     /* Переменные для расчета остатка по счету */
{intrface.get date}     /* Инструменты для работы с датами */

def input param rid1             as recid             no-undo. 
def input param in-commi         like comm-rate.commi no-undo.
def input param rid              as recid             no-undo.
def input param beg              as date              no-undo.
def input param end-date1        as date              no-undo.
def input param str-kau          like kau.kau         no-undo.
def input param fl-print         as logical           no-undo.
def input-output param xresult   as decimal           no-undo.
def input-output param beg-date1 as date              no-undo.
def output param xresult-ref     as decimal           no-undo.

DEF VAR mSince       AS DATE                 NO-UNDO.
DEF VAR mPenCommi    AS CHAR                 NO-UNDO.
DEF VAR mPenCommRate AS DECIMAL              NO-UNDO.
DEF VAR dat-commi    AS DATE                 NO-UNDO.
DEF VAR end-commi    AS DATE                 NO-UNDO.
DEF VAR in-status    LIKE op.op-status       NO-UNDO.
DEF VAR comm-ost     like acct-pos.balance   no-undo.
DEF VAR mShtrIz      AS CHAR                 NO-UNDO. 

DEFINE VAR fl-nach  AS INT64 NO-UNDO.

DEFINE TEMP-TABLE comm-mp no-undo LIKE comm-rate
    FIELD cr-recid    AS RECID
    FIELD end-per     AS DATE    /* Дата окончания периода, когда действует данная ставка */
    FIELD period-len  AS INT64 /* Длина периода */
    INDEX ind commission acct currency kau min-value period since
.

DEF BUFFER bcomm-rate FOR comm-rate.

{def-ret.i}
{def_work.i new}
{dpsproc.def}
{intrkost.i}
{cbrefprd.i}

in-status = chr(251).

FIND FIRST acct WHERE RECID(acct) EQ rid 
   NO-LOCK NO-ERROR.
IF NOT AVAIL acct THEN 
   RETURN '{&next1}'.

FIND FIRST interest-sch-line WHERE RECID(interest-sch-line) EQ rid1
   NO-LOCK NO-ERROR.

FIND FIRST loan WHERE loan.contract  eq ENTRY(1,str-kau) 
                  AND loan.cont-code eq ENTRY(2,str-kau)
   NO-LOCK NO-ERROR. 

IF in-commi NE gCBRefPred THEN
  RUN Get_Last_Param in h_dpspc (RECID(loan),
                                 end-date1,
                                 end-date1,
                                 "commission",
                                 OUTPUT in-commi).
mShtrIz = GetXattrValueEx ("loan",
                          loan.contract + "," + loan.cont-code,
                          "ШтрафИзъятКап",
                          ?).
IF {assigned mShtrIz} THEN
   RUN Get_Last_Param in h_dpspc (RECID(loan),
                                  end-date1,
                                  end-date1,
                                  "СтавКап",
                                  OUTPUT in-commi).

FIND FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract 
                       AND loan-cond.cont-code EQ loan.cont-code
   NO-LOCK NO-ERROR.

{empty fost}
/*Удаление лишнего */
run del_kau_ost.
/* Получение остатков по субаналитическим счетам */
run cr_summ_ost(beg-date1,
                end-date1 + 1,
                rid,
                IF loan.end-date NE ? THEN 'ОстВклС' 
                                      ELSE 'ОстВклВ', 
                IF loan.end-date NE ? THEN 'НачПрС1' 
                                      ELSE 'НачПрВ').

/*Пересчет остатков по плановым датам*/
RUN rclcfost (loan.contract,
              loan.cont-code,
              ?,
              acct.acct,
              acct.currency).

beg-date1  = beg-date1 + 1.

/* определение параметров лоя выбора комисии - даты, по которой учитывается изменение комиссии - это дата открытия/пролонгации вклада и
продолжительности вклада , если вклад до востребования, то продолжительность берется = 0 */
RUN Get_Date_Comm in h_dpspc (RECID(loan),
                              beg-date1,
                              OUTPUT dat-commi, 
                              OUTPUT end-commi).

if in-commi eq gCBRefPred then 
   dat-commi = beg.

{refin39.i 
   "inter39.i" 
   vPeriodInt
}

If in-commi = gCBRefPred THEN 
DO: 
   {inter_pn.i
      &d-beg      = beg-date1
      &d-end      = end-date1
      &sum        = fost
      &since      = since
      &rcom       = in-commi
      &proc       = xresult
      &balance    = balance
      &comm-rate  = comm-rate         
      &dat-comm   = dat-commi

      &offprint = YES
   }
END.
ELSE 
DO:
   DEFINE VAR lPeriodBegDate AS DATE NO-UNDO.
   DEFINE VAR lCoveredPeriod AS INT64 NO-UNDO.

   {empty comm-mp}
   FOR EACH commission WHERE commission.commission EQ in-commi
                       NO-LOCK
                       BREAK BY commission.currency BY commission.min-value BY commission.period:
       IF FIRST-OF(commission.min-value) THEN DO:
          lPeriodBegDate = dat-commi + IF dat-commi EQ loan.open-date /* Начинаем отсчет сроков от даты открытия вклада */
                                       THEN 1
                                       ELSE 0. 
          lCoveredPeriod = 0.
       END.
/*       
       /* Определяем штрафную ставку */
       FIND FIRST bcomm-rate WHERE bcomm-rate.commission =  mPenCommi
                               AND bcomm-rate.currency   =  commission.currency
                               AND bcomm-rate.min-value  =  commission.min-value
                               AND bcomm-rate.period     =  commission.period
                               AND bcomm-rate.acct       =  acct.acct
                               AND bcomm-rate.since      <= dat-commi
                             NO-LOCK NO-ERROR.
       IF NOT AVAILABLE bcomm-rate THEN 
       FIND FIRST bcomm-rate WHERE bcomm-rate.commission =  mPenCommi
                               AND bcomm-rate.currency   =  commission.currency
                               AND bcomm-rate.min-value  =  commission.min-value
                               AND bcomm-rate.period     =  commission.period
                               AND bcomm-rate.acct       =  "0"
                               AND bcomm-rate.since      <= dat-commi
                             NO-LOCK NO-ERROR.
  */    

       /* Определяем основную ставку для выбранного периода  */
       FIND LAST comm-rate WHERE comm-rate.commission =  commission.commission
                             AND comm-rate.currency   =  commission.currency
                             AND comm-rate.min-value  =  commission.min-value
                             AND comm-rate.period     =  commission.period
                             AND comm-rate.acct       =  acct.acct
                             AND comm-rate.since      <= dat-commi
                           NO-LOCK NO-ERROR.
       IF NOT AVAILABLE comm-rate THEN 
       FIND LAST comm-rate WHERE comm-rate.commission =  commission.commission
                             AND comm-rate.filial-id  = acct.filial-id
                             AND comm-rate.branch-id  = ""
                             AND comm-rate.currency   =  commission.currency
                             AND comm-rate.min-value  =  commission.min-value
                             AND comm-rate.period     =  commission.period
                             AND comm-rate.acct       =  "0"
                             AND comm-rate.since      <= dat-commi
                           NO-LOCK NO-ERROR.
       CREATE comm-mp.
       IF NOT AVAILABLE comm-rate THEN DO:
         ASSIGN
            comm-mp.acct        = "0"
            comm-mp.commission  = commission.commission
            comm-mp.currency    = commission.currency
            comm-mp.kau         = ""
            comm-mp.min-value   = commission.min-value
            comm-mp.rate-comm   = 0
            comm-mp.rate-fixed  = no
            
            comm-mp.since       = lPeriodBegDate
            comm-mp.period      = 0
            comm-mp.cr-recid    = RECID(comm-rate)
            comm-mp.end-per     = lPeriodBegDate + commission.period - lCoveredPeriod - 1
            comm-mp.period-len  = commission.period - lCoveredPeriod
         .
       END.
       ELSE DO:
         BUFFER-COPY comm-rate EXCEPT since TO comm-mp NO-ERROR.  
         ASSIGN 
            comm-mp.since       = lPeriodBegDate
            comm-mp.period      = 0
            comm-mp.cr-recid    = RECID(comm-rate)
            comm-mp.end-per     = lPeriodBegDate + commission.period - lCoveredPeriod - 1
            comm-mp.period-len  = commission.period - lCoveredPeriod
         .
       END.

       lPeriodBegDate = lPeriodBegDate + commission.period - lCoveredPeriod.
       lCoveredPeriod = commission.period.
       
       IF NOT {assigned mShtrIz} THEN
       DO:
          IF LAST-OF(commission.min-value) THEN DO:
             CREATE comm-mp.
             ASSIGN
                comm-mp.acct        = "0"
                comm-mp.commission  = commission.commission
                comm-mp.currency    = commission.currency
                comm-mp.kau         = ""
                comm-mp.min-value   = commission.min-value
                comm-mp.rate-comm   = 0.0
                comm-mp.rate-fixed  = no
                
                comm-mp.since       = lPeriodBegDate
                comm-mp.period      = 0
                comm-mp.cr-recid    = RECID(comm-rate)
                comm-mp.end-per     = ?
                comm-mp.period-len  = ?
             .
          END.
       END. /* IF NOT {assigned mShtrIz} THEN */
   END.
   
   /*run instview.p(TEMP-TABLE comm-mp:HANDLE).*/
   
   {inter_pn.i
      &d-beg      = beg-date1
      &d-end      = end-date1
      &sum        = fost
      &since      = since
      &rcom       = in-commi
      &proc       = xresult
      &balance    = balance
      &comm-rate  = comm-mp         
      &dat-comm   = dat-commi
      &cr-recid   = YES            
      &offdef     = "/*"      
      
      &correctsh  = YES
      &offprint   = YES
   }
END.

{intrface.del}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='10/06/2015 21:39:05.847+04:00' */
/* $LINTUSER='var' */
/* $LINTMODE='1' */
/* $LINTFILE='nach_ins.p' */
/*prosignGjkzDGVNdGmCaUUM3ecbTQ*/