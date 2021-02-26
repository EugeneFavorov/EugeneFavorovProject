/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ved_2_kd.p
      Comment: вывод клиентов, у которых несколько КД, при этом один и тот же счет привязан в качестве расчетного
   Parameters:  
      Created: evf
*/

{globals.i}
{setdest.i}
{chkacces.i}
{sh-defs.i}

DEF BUFFER bloan-acct FOR loan-acct.
DEF BUFFER bbloan-acct FOR loan-acct.
DEF VAR i AS DECIMAL NO-UNDO.
DEF VAR Name AS CHARACTER NO-UNDO.
DEF VAR OpenDate AS DATE NO-UNDO.
DEF VAR EndDate AS DATE NO-UNDO.
DEF VAR AcctSsud AS CHARACTER NO-UNDO.
DEF VAR AcctSsudBal AS DECIMAL NO-UNDO.
DEF VAR AcctRaschBal AS DECIMAL NO-UNDO.
DEF VAR AcctKredBal AS DECIMAL NO-UNDO.
DEF VAR Acct AS CHARACTER NO-UNDO.

DEF NEW SHARED STREAM ws.
DEF VAR fname AS CHAR INIT "./vedomost_2_kd.csv"  NO-UNDO.
DEF VAR delim AS CHAR INIT ";" format "x(1)" NO-UNDO.
DEF VAR eol AS CHAR format "x(2)" NO-UNDO.
eol = chr(13) + chr(10). 

DEFINE TEMP-TABLE tempt
 FIELD tName AS CHARACTER
 FIELD tAcct AS CHARACTER
 FIELD tAcctRaschBal AS DECIMAL
 FIELD tContCode AS CHARACTER
 FIELD tOpenDate AS DATE
 FIELD tEndDate AS DATE
 FIELD tAcctSsudBal AS DECIMAL
 FIELD tAcctKredBal AS DECIMAL
.



/* {setdest.i &col=170 } */



FOR EACH bloan-acct WHERE bloan-acct.cont-code MATCHES "*" 
                      AND bloan-acct.acct-type EQ "КредРасч" NO-LOCK BY bloan-acct.acct:



  FIND FIRST loan-acct WHERE loan-acct.acct EQ bloan-acct.acct 
                         AND loan-acct.acct-type EQ "КредРасч"
                         AND loan-acct.cont-code MATCHES "*" NO-LOCK NO-ERROR.



   IF AVAIL loan-acct
     THEN 
       DO:

         

         FOR EACH loan WHERE loan.contract EQ loan-acct.contract
                         AND loan-acct.acct-type EQ "КредРасч"
                         AND loan.cont-code EQ loan-acct.cont-code NO-LOCK:                



           IF AVAIL loan
             THEN
               DO:
                 OpenDate = loan.open-date.
                 EndDate = loan.end-date.



                 FOR EACH bbloan-acct WHERE bbloan-acct.cont-code MATCHES bloan-acct.cont-code 
                                        AND ((bbloan-acct.acct-type EQ "Кредит") OR (bbloan-acct.acct-type EQ "КредПр")) NO-LOCK:
                   IF bbloan-acct.acct-type EQ "Кредит"
                     THEN
                       DO:
                         AcctSsud = bbloan-acct.acct.
                         IF AcctSsud <> "" 
                           THEN
                             DO:
                               RUN acct-pos IN h_base (bbloan-acct.acct, bbloan-acct.currency, today, today, ?).
	                       AcctSsudBal = - sh-bal.
                             END. 
                       END.

                   IF bbloan-acct.acct-type EQ "КредПр"
                     THEN
                       DO:
                         IF bbloan-acct.acct <> "" 
                           THEN
                             DO:
                               RUN acct-pos IN h_base (bbloan-acct.acct, bbloan-acct.currency, today, today, ?).
	                       AcctKredBal = - sh-bal.
                             END. 
                       END.
                 END.



                 FIND FIRST cust-role WHERE cust-role.cust-cat EQ loan.cust-cat 
                                        AND cust-role.cust-id EQ string(loan.cust-id) NO-LOCK NO-ERROR.
                 IF AVAIL cust-role
                   THEN 
                     DO:
                       Name = cust-role.short-name.
                     END.
               END.
         END.

              

         FIND NEXT loan-acct WHERE loan-acct.acct EQ bloan-acct.acct
                               AND loan-acct.acct-type EQ "КредРасч"
                               AND loan-acct.cont-code MATCHES "*" NO-LOCK NO-ERROR.
           IF AVAIL loan-acct
             THEN 
               DO:
                 RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, today, today, "П").
                 AcctRaschBal = - sh-bal.
                 CREATE tempt.
                 ASSIGN
                  tempt.tName = Name
                  tempt.tAcct = bloan-acct.acct
                  tempt.tAcctRaschBal = AcctRaschBal
                  tempt.tContCode = bloan-acct.cont-code
                  tempt.tOpenDate = OpenDate
                  tempt.tEndDate = EndDate
                  tempt.tAcctSsudBal = AcctSsudBal
                  tempt.tAcctKredBal = AcctKredBal
                 .
               END.



       END.



END.



/* {preview.i &col=170} */



OUTPUT STREAM ws TO VALUE (fname)
  UNBUFFERED CONVERT TARGET "1251" SOURCE "IBM866".

PUT STREAM ws UNFORMATTED
  "П/П" delim
  "Клиент" delim
  "Расчётный счёт" delim
  "Остаток на расчётном счёте" delim
  "Договор" delim
  "Дата открытия" delim
  "Плановая дата гашения" delim
  "Остаток на ссудном счёте" delim
  "Просроченная задолженность" eol.


Acct = "".
i = 1.


FOR EACH tempt NO-LOCK:
  IF tempt.tAcct EQ Acct
    THEN
      DO:
        Acct = tempt.tAcct.
        PUT STREAM ws UNFORMATTED
        delim
        delim
        tempt.tAcct delim
        tempt.tAcctRaschBal delim
        tempt.tContCode delim
        tempt.tOpenDate delim  
        tempt.tEndDate delim
        tempt.tAcctSsudBal delim
        tempt.tAcctKredBal eol.
      END.
    ELSE
      DO:
        Acct = tempt.tAcct.
        PUT STREAM ws UNFORMATTED
        eol
        i delim
        tempt.tName delim
        tempt.tAcct delim
        tempt.tAcctRaschBal delim
        tempt.tContCode delim
        tempt.tOpenDate delim
        tempt.tEndDate delim
        tempt.tAcctSsudBal delim
        tempt.tAcctKredBal eol.
        i = i + 1.
      END.
END.



OUTPUT STREAM ws CLOSE.
MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}
