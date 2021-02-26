/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: ISG140n.P
      Comment: Схема начисляет на реальный МИНИМАЛЬНЫЙ остаток
               на счете за период, которые превосходит МинОст. 
               Начисление происходит только за тот период, 
               когда остаток на счете превышал МинОст. 
   Parameters:
      Created: shib 30/06/2006 
     Modified:
     
*/

DEFINE INPUT PARAM rid1         AS RECID                   NO-UNDO. /* Сх нач %% */
DEFINE INPUT PARAM in-commi     LIKE commission.commission NO-UNDO. /* Коммисия  */
DEFINE INPUT PARAM rid          AS RECID                   NO-UNDO. /* Счет when acct */
DEFINE INPUT PARAM in_kau       LIKE kau.kau               NO-UNDO. /* Счет when avail kau */
DEFINE INPUT PARAM ipcurr_beg     AS DATE                  NO-UNDO. /* Дата начала периода */
DEFINE INPUT PARAM ipcurr_end     AS DATE                  NO-UNDO. /* Дата окончания  периода */

{globals.i}
{def_work.i}            /* Определение таблицы fost */
{intrface.get date}     /* Инструменты для работы с датами. */
/*{intrface.get xclass} */

DEFINE VARIABLE nach_h          AS HANDLE      NO-UNDO. /* Указатель на инструментарий */
DEFINE VARIABLE vMinOst         AS DECIMAL     NO-UNDO. /* МинОст */
DEFINE VARIABLE vMinRealOst     AS DECIMAL     NO-UNDO. /* Реальный минимальный остаток на счёте за период */
DEFINE VARIABLE mDays           AS INT64     NO-UNDO. /* Количество дней в периоде. */
DEFINE VARIABLE curr_beg AS DATE       NO-UNDO.
DEFINE VARIABLE curr_end AS DATE       NO-UNDO.
DEFINE VARIABLE dt_dop AS DATE       NO-UNDO.
DEFINE VARIABLE dd AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mAcctOpen    AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstAct    AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstActDB  AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstActCR  AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstPeriod AS LOGICAL NO-UNDO.
DEFINE VARIABLE mNumDays     AS INT64   NO-UNDO.
curr_beg = ipcurr_beg.
curr_end = ipcurr_end.

FIND FIRST acct WHERE recid(acct) EQ rid
     NO-LOCK NO-ERROR.

/* Загрузка инструментария */
RUN load_nachtool (NO, OUTPUT nach_h).

/* Поиск комиссии */
RUN get_sch_line_by_rid IN nach_h (rid1, BUFFER interest-sch-line).

/* Поиск счета */
RUN GET_ACCT_BY_RID in nach_h (rid, BUFFER acct).
IF NOT AVAIL acct THEN 
DO:
    /* Выгрузка инструментария */
    RUN remove_nachtool (NO, nach_h).
    RETURN "Счет не найден".
END.

ASSIGN
   mAcctOpen = acct.open-date
   mFirstAct = ?.
   
FOR EACH op-entry WHERE
       op-entry.acct-db   EQ acct.acct
   AND op-entry.filial-id EQ acct.filial-id
   AND op-entry.op-status GE CHR(251)  
   AND op-entry.op-date   GE DATE("01/01/2015")
   NO-LOCK:
      mFirstActDB = op-entry.op-date.
   LEAVE.     
END.
FOR EACH op-entry WHERE
       op-entry.acct-cr   EQ acct.acct
   AND op-entry.filial-id EQ acct.filial-id
   AND op-entry.op-status GE CHR(251)    
   AND op-entry.op-date   GE DATE("01/01/2015")
   NO-LOCK:
      mFirstActCR = op-entry.op-date.
   LEAVE.     
END.
IF      mFirstActDB NE ? 
   AND  mFirstActCR NE ? THEN mFirstAct = MIN(mFirstActDB,mFirstActCR).
ELSE IF mFirstActDB EQ ? 
   AND  mFirstActCR NE ? THEN mFirstAct = mFirstActCR.
ELSE IF mFirstActDB NE ? 
   AND  mFirstActCR EQ ? THEN mFirstAct = mFirstActDB.
ELSE IF mFirstActDB EQ ? 
   AND  mFirstActCR EQ ? THEN mFirstAct = ?.

IF     YEAR (mFirstAct) EQ YEAR (curr_end)  
   AND MONTH(mFirstAct) EQ MONTH(curr_end) 
THEN
ASSIGN 
   mFirstPeriod = YES
   curr_beg     = MAX(mFirstAct + 1,curr_beg)
   mNumDays     = curr_end - mFirstAct.
ELSE 
ASSIGN
   mFirstPeriod = NO.
   
/* Поиск минимального остатка для счета vMinOst */
RUN GetMinRemainder IN nach_h (
    acct.acct,
    acct.currency,
    OUTPUT vMinOst).
IF RETURN-VALUE NE "" THEN 
DO:
    /* Выгрузка инструментария */
    RUN remove_nachtool (NO, nach_h).
    RETURN RETURN-VALUE.
END.

/* Динамика аналитических остатков по счету */
RUN CREATE_REAL_FOST IN nach_h (rid, curr_beg, curr_end).
    
/* Получение минимального остатка по сформированной динамике. */
RUN GetMinAmt        IN nach_h (OUTPUT vMinRealOst).

IF vMinRealOst EQ 0 THEN 
DO:
    /* Выгрузка инструментария */
    RUN remove_nachtool (NO, nach_h).
    RETURN "Минимальный остаток на счёте равен нулю, начисление не производится.".
END.

RUN CorrectFostBalance IN nach_h (vMinRealOst,vMinOst,"Остаток не превышал МинОст, начисление не производится.").
IF RETURN-VALUE NE "" THEN 
DO:
    /* Выгрузка инструментария */
    RUN remove_nachtool (NO, nach_h).
    RETURN RETURN-VALUE.
END. 

/* Вычисление периода.
** Определение количества дней в интервале. */
mDays = cDay(interest-sch-line.interest-month, curr_beg, curr_end + 1).

/* Формирование комиссии. */
IF mFirstPeriod EQ YES 
   AND mNumDays LT 15
THEN
   RUN CREATE_RATE_CR IN nach_h (in-commi, rid, ?, ?, 0, mDays, curr_end).
ELSE   
   RUN CREATE_RATE_CR IN nach_h (in-commi, rid, ?, ?, ?, mDays, curr_end).
IF RETURN-VALUE NE ""
THEN DO:
    /* Выгрузка инструментария */
   RUN remove_nachtool (NO, nach_h).
   RETURN RETURN-VALUE.
END.

/* Расчет начисления и формирование отчета */
RUN NACH_AND_REPORT IN nach_h (interest-sch-line.interest-sch, 
                               acct.acct,
                               acct.currency, 
                               in_kau, 
                               curr_beg, 
                               curr_end,
                               interest-sch-line.interest-month, 
                               interest-sch-line.basis-time).



/* Выгрузка инструментария */
RUN remove_nachtool (NO, nach_h).

RETURN "".
