/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: AC-CLO.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 15.04.2009 18:30 BIS     
     Modified: 15.04.2009 18:30 BIS     
*/

IF NOT vClosedMainAcct THEN
   UNDO {&main_cycle}, LEAVE {&main_cycle}.

/* Если счет не является клиентским счетом, то его можно 
** закрыть только в случае, если это разрежено настройками метасхемы */
IF acct.cust-cat NE "Ч" THEN 
DO:
   IF NOT CAN-DO (list_type, loan-acct.acct-type) THEN
   NEXT {&main_cycle}.
END.
ELSE 
DO:
   /*Если счет клиентский, то можно закрыть если клиент у вклада и счета один и тот же*/
   IF acct.cust-id NE loan.cust-id THEN 
   NEXT {&main_cycle}.
END.
          
IF acct.close-date NE ? THEN 
   NEXT {&main_cycle}.
      
vMainAcct = IF CAN-DO("loan-dps-t,loan-dps-p,loan-dps-ts,loan-dps-tsk,loan-dps-int", loan-Acct.acct-type) THEN YES
                                                                                                          ELSE NO.
/*************  Проверка неакцептованных проводок  *************/
vPosSinceDate = fGetLastClsDate(?,acct.acct-cat).
FIND FIRST op-date WHERE op-date.op-date GT vPosSinceDate 
   NO-LOCK NO-ERROR.
IF AVAIL op-date THEN 
DO:
   FIND LAST op-entry WHERE op-entry.acct-db     EQ     acct.acct
                        AND op-entry.currency    BEGINS acct.currency
                        AND op-entry.op-status   LT     '√'
                        AND op-entry.op-status   GT     'А'
                        AND op-entry.op-date     GE     op-date.op-date
      NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN 
      flagerr = YES.
   FIND LAST op-entry WHERE op-entry.acct-cr     EQ     acct.acct
                        AND op-entry.currency    BEGINS acct.currency
                        AND op-entry.op-status   LT     '√'
                        AND op-entry.op-status   GT     'А'
                        AND op-entry.op-date     GE     op-date.op-date
      NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN 
      flagerr = YES.
   
   IF     flagerr 
      AND vMainAcct THEN
      vClosedMainAcct = NO.

   IF flagerr THEN 
   DO:
      IF NOT gLogMessage THEN
         RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (1 * {&TOTAL}) ]).         
      ELSE 
         RUN LogMess(mess[ i + (1 * {&TOTAL}) ]).
      NEXT {&main_cycle}.
   END.         
END.

/*************  Проверка привязки к другим вкладам  *************/
/* По идее нельзя закрывать счет если он привязан к другому вкладу 
** с какой угодно ролью а не только с той, с которой он привязан 
** к закрываемому вкладу */
flagerr = NO.

FLAG:
FOR EACH bb-loan-acct WHERE bb-loan-acct.contract  EQ 'DPS'
                        AND bb-loan-acct.acct      EQ acct.acct
                        AND bb-loan-acct.currency  EQ acct.currency 
   NO-LOCK:

	FIND FIRST bb-loan WHERE bb-loan.contract    EQ "dps" 
                        AND bb-loan.cont-code   EQ bb-loan-acct.cont-code 
	   NO-LOCK NO-ERROR.
	IF bb-loan-acct.cont-code EQ loan.cont-code THEN 
      NEXT FLAG.

	IF AVAILABLE bb-loan THEN 
   DO:
      FIND FIRST b-loan-acct OF bb-loan WHERE b-loan-acct.acct-type EQ bb-loan-acct.acct-type
                                          AND b-loan-acct.since     GT bb-loan-acct.since
         NO-LOCK NO-ERROR.
      IF     NOT AVAIL b-loan-acct  
         AND bb-loan.close-date EQ ? THEN 
         flagerr = YES.
   END.		
   IF flagerr THEN 
      LEAVE FLAG.
END.

IF     flagerr
   AND vMainAcct THEN
   vClosedMainAcct = NO.

IF flagerr THEN 
DO:
   IF NOT gLogMessage THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (2 * {&TOTAL}) ]).
   ELSE 
      RUN LogMess(mess[ i + (2 * {&TOTAL}) ]).
   NEXT {&main_cycle}.   
END.

/*************  Проверка остатков на счете  *************/
/*RUN acct-pos IN h_base(acct.acct,
                       acct.currency,
                       in-op-date,
                       ?,
                       "Ф").
IF    sh-bal NE 0 
   OR sh-val NE 0 THEN 
   flagerr = YES.

IF     flagerr 
   AND vMainAcct THEN
   vClosedMainAcct = NO.

IF flagerr THEN DO:
   IF NOT gLogMessage THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (3 * {&TOTAL}) ]).
   ELSE 
      RUN LogMess(mess[ i + (3 * {&TOTAL}) ]).
   NEXT {&main_cycle}.   
END.

IF    lastmove GT in-op-date 
   OR lastcurr GT in-op-date THEN 
   flagerr = YES.
IF     flagerr 
   AND vMainAcct THEN
   vClosedMainAcct = NO.
IF flagerr THEN DO:
   IF NOT gLogMessage THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (4 * {&TOTAL}) ]).
   ELSE 
      RUN LogMess(mess[ i + (4 * {&TOTAL}) ]).
   NEXT {&main_cycle}.   
END.*/

/*************  Проверка если дата открытия больше даты закрытия вклада  *************/      
IF acct.open-date GT in-op-date THEN 
DO:
   vQuestLog = NO.
   IF vMainAcct THEN
      vClosedMainAcct = NO.
   IF NOT gLogMessage THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (5 * {&TOTAL}) ]).
   ELSE 
      RUN LogMess(mess[ i + (5 * {&TOTAL}) ]).
   IF vQuestLog EQ NO OR vQuestLog EQ ? THEN 
      NEXT {&main_cycle}.

   FIND FIRST bAcct WHERE ROWID(bAcct) EQ ROWID(acct) 
      EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL bAcct THEN
      bAcct.close-date = bAcct.open-date.
   ELSE 
   DO:
      IF NOT gLogMessage THEN
         RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (6 * {&TOTAL}) ]).
      ELSE 
         RUN LogMess(mess[ i + (6 * {&TOTAL}) ]).      
   END.

   NEXT {&main_cycle}.
END.

FIND FIRST bAcct WHERE ROWID(bAcct) EQ ROWID(acct) 
      EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL bAcct THEN
   bAcct.close-date = in-op-date.
ELSE 
DO:
   IF NOT gLogMessage THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i + (6 * {&TOTAL}) ]).
   ELSE 
      RUN LogMess(mess[ i + (6 * {&TOTAL}) ]).  
END.
