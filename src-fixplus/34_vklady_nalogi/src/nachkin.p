/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: NACHKIN.P
      Comment: Процедура nachkin.p
   Parameters: rid1 in-commi rid end-date1 str-kau fl-print xresult xresult-ref beg-date1 fl
      Created:  ??/??/???? ???
     Modified:  20/07/1999 Om Скорректирован вывод в поток.
     Modified:  23/07/1999 Om
     Modified:  05/02/2002 Илюха - Для кой-какого report'a пришлось ввести temp-table
                                   и выводить данные в него
     Modified:  24/06/2003 Dasu - 0017016. Исправлена ошибка при начислении процентов 
                                  с нулевыми строками в ведомоси, когда изменение 
                                  %ЦБРеф было той же датой, что и интервал начисления 
                                  процентов. Так же выполнено форматирование кода.
     Modified:  01/05/2004 Mioa - 0024783 Используется дополнительная таблица для 
                                  гибкого вывода отчетов при начислении процентов.
                                  (nachkin-tt) Требует оптимизации (удалить rep-tt).
*/


FORM
   "~n@(#) NACHKIN.P 1.0 ??? ??/??/?? Om 20/07/99"
WITH FRAME sccs-id STREAM-IO WIDTH 250.

{globals.i}
&IF DEFINED(use-tt) = 0
&THEN
DEF NEW GLOBAL SHARED STREAM err.
DEF NEW SHARED FRAME prn.
  

DEF STREAM errmsg.
DEF VAR mIsErrOpen AS LOG NO-UNDO.
OUTPUT STREAM errmsg TO "errmsg.txt" KEEP-MESSAGES.
DO ON ERROR UNDO,LEAVE:
   PUT STREAM err UNFORM "".
   mIsErrOpen = YES.
END.
OUTPUT STREAM errmsg CLOSE.
IF NOT mIsErrOpen THEN
   OUTPUT STREAM err TO "err.txt".
  
{prn-ved.i &DefTempTable = "Объявляем временную таблицу"}
&ENDIF

DEF INPUT  PARAM rid1            AS RECID             NO-UNDO.
DEF INPUT  PARAM in-commi        LIKE comm-rate.commi NO-UNDO.
DEF INPUT  PARAM rid             AS RECID             NO-UNDO.
DEF INPUT  PARAM end-date1       AS DATE              NO-UNDO.
DEF INPUT  PARAM str-kau         LIKE kau.kau         NO-UNDO.
DEF INPUT  PARAM fl-print        AS LOGICAL           NO-UNDO.
DEF OUTPUT PARAM xresult         AS DECIMAL           NO-UNDO.
DEF OUTPUT PARAM xresult-ref     AS DECIMAL           NO-UNDO.
DEF INPUT-OUTPUT PARAM beg-date1 AS DATE              NO-UNDO.
DEF OUTPUT PARAM fl              AS INT64.
DEF VAR return-value1 AS CHAR NO-UNDO.
DEF VAR return-value2 AS CHAR NO-UNDO.
DEF VAR vII AS INT64 NO-UNDO.
DEF VAR mContCode AS CHAR NO-UNDO.
DEF VAR mRate-comm  AS CHARACTER NO-UNDO.
DEF VAR bdate       AS DATE    NO-UNDO.
DEF VAR beg         AS DATE    NO-UNDO.
DEF VAR xresult1    AS DECIMAL NO-UNDO.

&IF DEFINED(use-tt) = 0
&THEN
{cr-nach.i}
&ENDIF

FIND interest-sch-line WHERE 
                       RECID(interest-sch-line) EQ rid1 
                       NO-LOCK NO-ERROR.
FIND acct WHERE 
          RECID(acct) EQ rid NO-LOCK NO-ERROR.

FIND FIRST loan WHERE 
          loan.contract  EQ ENTRY(1,str-kau) 
      AND loan.cont-code EQ ENTRY(2,str-kau) 
      NO-LOCK NO-ERROR.

IF NOT AVAIL loan THEN RETURN.

IF NOT AVAIL acct THEN RETURN.

FORM
   mContCode FORMAT 'x(22)'
   acct.acct
   bdate
   beg-date1
   n1 AS INT64
   n2 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
   n3 AS CHAR    FORMAT 'x(20)'
   n4 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
   xresult       FORMAT "->>>,>>>,>>>,>>9.99"
WITH FRAME prn DOWN WIDTH 200 NO-LABELS.

beg = beg-date1.
fl = -1.
QQ:
REPEAT WITH FRAME prn TRANSACTION ON ERROR UNDO QQ, 
                                           LEAVE QQ ON ENDKEY UNDO QQ,
                                           LEAVE QQ :
  fl = 1 .

  bdate = beg-date1 .

  IF NOT SearchPFile(interest-sch-line.proc-name) THEN
  DO:
     MESSAGE "Процедура схемы начисления процентов~n[" 
        + interest-sch-line.interest-sch + "] не определена."
             VIEW-AS ALERT-BOX WARNING.
     RETURN.
  END.

  RUN VALUE(interest-sch-line.proc-name + '.p') (RECID(interest-sch-line),
                                                 in-commi,
                                                 RECID(acct),
                                                 beg,
                                                 end-date1,
                                                 str-kau,
                                                 fl-print,
                                                 INPUT-OUTPUT xresult,
                                                 INPUT-OUTPUT beg-date1,
                                                 OUTPUT xresult1) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
  DO:
     MESSAGE "Ошибка запуска схемы начисления <" + interest-sch-line.interest-sch + ">: " + ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
     UNDO QQ, LEAVE QQ.
  END.
  ELSE
     return-value2 = RETURN-VALUE.

  IF beg = bdate THEN xresult-ref = xresult1 .
                                    
  CASE RETURN-VALUE2 :
     WHEN "1" THEN
            LEAVE QQ.
   
     WHEN "-1" THEN 
     DO :
        UNDO QQ, LEAVE QQ.
     END.

     OTHERWISE  
     DO:
        IF fl-print AND INT64(ENTRY(2,RETURN-VALUE2)) <> 0 THEN 
        DO :
            DO vII = 1 TO NUM-ENTRIES(return-value2,CHR(1)):
                return-value1 = ENTRY(vII,return-value2,CHR(1)). 
                CREATE Nachkin-tt.
                ASSIGN
                   Nachkin-tt.Contract = loan.contract
                   Nachkin-tt.Contcode = loan.cont-code
                   Nachkin-tt.BegDate  = bdate
                                  WHEN NUM-ENTRIES(RETURN-VALUE1) < 6
                   Nachkin-tt.BegDate  = DATE(ENTRY(6,RETURN-VALUE1))
                                 WHEN NUM-ENTRIES(RETURN-VALUE1) >= 6
                   Nachkin-tt.Days     = INT64(ENTRY(2,RETURN-VALUE1))
                   Nachkin-tt.EndDate  = beg-date1
                   Nachkin-tt.Comm     = DEC(ENTRY(4,RETURN-VALUE1))
                   Nachkin-tt.CommCode = in-commi
                   Nachkin-tt.SummOst  = DEC(ENTRY(3,RETURN-VALUE1))
                   Nachkin-tt.SummProc = ROUND(DEC(ENTRY(5,RETURN-VALUE1)),2)
                   Nachkin-tt.Acct    = acct.acct
                   Nachkin-tt.Curr    = loan.currency
                   Nachkin-tt.BalAcct = acct.bal-acct
               .
               Nachkin-tt.OpCode   = GetSysConf("op-id").
               IF INT64(ENTRY(2,return-value1)) NE 0 AND GetSysConf("base-nalog") NE "yes" THEN
               DO:
               &IF DEFINED(use-tt) <> 0
               &THEN
               &ELSE
                  ASSIGN
                     mRate-comm = STRING(DEC(ENTRY(4,RETURN-VALUE1)), ">>9.99999")
                     mContCode  = DelFilFromLoan (loan.cont-code)
                  .


                  DISP STREAM err
                  mContCode FORMAT "x(22)" WHEN bdate EQ beg AND  vII = 1
                  acct.acct      WHEN bdate EQ beg AND  vII = 1
                  bdate WHEN NUM-ENTRIES(RETURN-VALUE1) LT 6 @ bdate
                  DATE(ENTRY(6,RETURN-VALUE1)) 
                                    WHEN NUM-ENTRIES(RETURN-VALUE1) GE 6 @ bdate
                  beg-date1 @ beg-date1
                  ENTRY(2,RETURN-VALUE1)  @ n1
                  DEC(ENTRY(3,RETURN-VALUE1)) FORMAT "->>>,>>>,>>>,>>9.99" @ n2
                  ' ' + IF NUM-ENTRIES(RETURN-VALUE1) = 7 
                        THEN (STRING(ENTRY(7,RETURN-VALUE1),"x(9)") + ' ' 
                              + mRate-comm)
                        ELSE (STRING(in-commi,"x(9)") + ' ' + mRate-comm) 
                              FORMAT "x(20)" @ n3
                  DEC(ENTRY(5,RETURN-VALUE1))  
                              FORMAT "->>>,>>>,>>>,>>9.99" WHEN fl-print @ n4
                  xresult WHEN beg-date1 GE end-date1 AND vII = NUM-ENTRIES(return-value2,CHR(1))
                              FORMAT "->>>,>>>,>>>,>>9.99" COLUMN-LABEL "ИТОГО".
                  DOWN STREAM err.
                  /*сохранение информации*/
                  
                  IF TRIM(FGetSetting('СохрПроц',?,?)) = "Да" 
                      AND GetSysConf("op-id") NE ?
                      AND GetSysConf("op-contract-date") NE ? THEN 
                     
                  DO:
                       RUN CrDataNch( DATE(GetSysConf("op-contract-date")),
                                    ?,
                                    GetSysConf("op-id"),
                                    loan.contract + "," + loan.cont-code,
                                    IF NUM-ENTRIES(RETURN-VALUE1) LT 6
                                    THEN bdate                              
                                    ELSE IF NUM-ENTRIES(RETURN-VALUE1) GE 6 
                                         THEN DATE(ENTRY(6,RETURN-VALUE1))        
                                         ELSE ?,
                                    beg-date1,
                                        entry(3,return-value1) + "," + /*остаток вклада VAL1*/
                                        entry(2,return-value1) + "," + /*количество дней VAL2*/ 
                                        entry(4,return-value1) + "," + /*% ставка  VAL3 */
                                        ENTRY(5,RETURN-VALUE1) + "," + /*сумма %%  VAL4 */
                                        (IF NUM-ENTRIES(RETURN-VALUE1) = 7 
                                          THEN ENTRY(7,RETURN-VALUE1)
                                          ELSE (in-commi))    + "," +     /* код комиссии TXT1 */
                                        loan.cont-type,        /* тип договора ТХТ2 */
                                    OUTPUT  fl,
                                    BUFFER Dataline).
                      
                       IF fl = -1  THEN DO:
                          MESSAGE "Информацию о %% сохранить невозможно" VIEW-AS ALERT-BOX.
                          UNDO QQ, LEAVE QQ.
                       END.
                  END.
               &ENDIF
               END.
           END.
        END.
        IF beg-date1 GE end-date1 THEN 
        DO:
           &IF DEFINED(use-tt) = 0
           &THEN
              IF fl-print THEN DOWN STREAM err.
           &ENDIF
           LEAVE QQ.
        END.

     END. /*OTHERWISE*/

  END CASE. 

END. /*REPEAT*/
RUN DeleteOldDataProtocol IN h_base("Davos_min_ost").

{intrface.del "olap"}
fl = 0.

RETURN return-value2.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='17/04/2015 15:14:09.162+04:00' */
/* $LINTUSER='kozv' */
/* $LINTMODE='1' */
/* $LINTFILE='nachkin.p' */
/*prosignFqyAAOd8r93eShxXh+zQQw*/