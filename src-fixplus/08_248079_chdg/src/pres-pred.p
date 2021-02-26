/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"               
     Filename: pres-pred.p
      Comment: Процедура формирования предварительного графика
   Parameters: none
         Uses:
      Used by:
      Created: 17/10/2014 kuds (0204892)
     Modified: 
*/

DEF INPUT PARAM iStatusTO AS CHAR NO-UNDO.
DEF INPUT PARAM iPreschTo AS CHAR NO-UNDO.
DEF PARAM BUFFER term-obl FOR term-obl.
DEF OUTPUT PARAM oRefresh AS LOGICAL NO-UNDO.


{globals.i}
{intrface.get xclass} /* Загрузка инструментария метасхемы  */
{intrface.get tmess}
{intrface.get loan}
{loan.pro}
{obl-pr.i}

DEFINE BUFFER loan      FOR loan.
DEFINE BUFFER bterm-obl FOR term-obl.
DEFINE BUFFER xterm-obl FOR term-obl.

DO TRANSACTION:
   IF iStatusTO NE "ВВЕД" THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","Операция формирования предв. графика доступна" + 
                     CHR(10) + "только для заявок со статусом ВВЕД.").
      LEAVE.
   END.

   /*Создаем новые условия по договору*/
   RUN pres-isp.p (mStatusTO,     
                   mPreschTo,     
                   BUFFER term-obl,
                   OUTPUT mOk). 
   IF mOk EQ NO THEN 
      RETURN.

  /*расчет ПСК на предварительном графике*/
   mPSK = TRIM(STRING(GetEpsLoan(term-obl.contract,
                                 term-obl.cont-code,
                                 term-obl.end-date) * 100,
               GetXAttrEx(term-obl.class-code,"ПСКПред","Data-Format")
                     )
              ).

   /*Сохраняем во временные таблицы*/
   /*График погашения процентов после ЧДГ*/
   FOR EACH xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract  
                        AND xterm-obl.cont-code EQ term-obl.cont-code
                        AND xterm-obl.idnt      EQ 1
   NO-LOCK:
      CREATE tt-term-obl-per-pr.
      BUFFER-COPY xterm-obl  TO tt-term-obl-per-pr.
   END.

   /*График плановых остатков после ЧДГ*/
   FOR EACH xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract  
                        AND xterm-obl.cont-code EQ term-obl.cont-code
                        AND xterm-obl.idnt      EQ 2
   NO-LOCK:
      CREATE tt-term-obl-sum-pr.
      BUFFER-COPY xterm-obl TO tt-term-obl-sum-pr.
   END.

   /*График погашения ссуды после ЧДГ*/
   FOR EACH xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract  
                        AND xterm-obl.cont-code EQ term-obl.cont-code
                        AND xterm-obl.idnt      EQ 3
   NO-LOCK:
      CREATE tt-term-obl-debt-pr.
      BUFFER-COPY xterm-obl TO tt-term-obl-debt-pr.
   END.

   /*График комиссий после ЧДГ */
   FOR EACH xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract  
                        AND xterm-obl.cont-code EQ term-obl.cont-code
                        AND xterm-obl.idnt      EQ 10
   NO-LOCK:
      CREATE tt-term-obl-comm-pr.
      BUFFER-COPY xterm-obl TO tt-term-obl-comm-pr.
   END.

   /*Откатываем созданные условия*/
   UNDO, LEAVE.  
END. 

/*сохраняем ПСК на предварительном графике*/
UpdateSignsEx(term-obl.class-code,
            term-obl.contract          + "," +
            term-obl.cont-code         + "," +
            STRING(term-obl.idnt)      + "," +
            STRING(term-obl.end-date)  + "," +
            STRING(term-obl.nn),
            "ПСКПред",
            mPSK
            ). 

/*удаляем записи из классов term-obl-debt-pr
                            term-obl-per-pr 
                            term-obl-sum-pr 
                            term-obl-comm-pr 
*/
RUN DeleteObl-pr IN THIS-PROCEDURE (BUFFER term-obl).

/*Создаем новые записи из временных таблиц*/
FOR EACH tt-term-obl-per-pr 
NO-LOCK:
   CREATE bterm-obl.
   BUFFER-COPY tt-term-obl-per-pr EXCEPT idnt class-code TO bterm-obl
      ASSIGN
         bterm-obl.idnt = 301
         bterm-obl.class-code = "term-obl-per-pr"
   .
END.

FOR EACH tt-term-obl-sum-pr 
NO-LOCK:
   CREATE bterm-obl.
   BUFFER-COPY tt-term-obl-sum-pr EXCEPT idnt class-code TO bterm-obl
      ASSIGN
         bterm-obl.idnt = 302
         bterm-obl.class-code = "tt-term-obl-sum-pr"
   .
END.

FOR EACH tt-term-obl-debt-pr 
NO-LOCK:
   CREATE bterm-obl.
   BUFFER-COPY tt-term-obl-debt-pr EXCEPT idnt class-code TO bterm-obl
      ASSIGN
         bterm-obl.idnt = 303
         bterm-obl.class-code = "term-obl-debt-pr"
  .
END.

FOR EACH tt-term-obl-comm-pr
NO-LOCK:
   CREATE bterm-obl.
   BUFFER-COPY tt-term-obl-comm-pr EXCEPT idnt class-code TO bterm-obl
      ASSIGN
         bterm-obl.idnt = 310
         bterm-obl.class-code = "term-obl-comm-pr"
  .
END.

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/02/2015 14:48:20.419+04:00' */
/* $LINTFILE='pres-pred.p' */
/*prosignW8PHpQJyBOgHWQzU9B+b5Q*/