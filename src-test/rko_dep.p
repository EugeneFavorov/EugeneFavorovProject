/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:  
      Created: fev
*/

{globals.i}                           
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{intrface.get tmess}
{tmprecid.def}

&GLOBAL-DEFINE FILE_sword_p YES
{pp-uni.var}
&UNDEFINE FILE_sword_p
{pp-uni.prg}
{prn-doc.def &with_proc=YES}

DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.

DEF VAR fname AS CHAR NO-UNDO.                                      
DEF VAR i1 AS DECIMAL NO-UNDO.
DEF VAR i2 AS DECIMAL NO-UNDO.
DEF VAR i3 AS DECIMAL NO-UNDO.
DEF VAR AcctBal AS DECIMAL NO-UNDO.
DEF VAR Summa AS DECIMAL NO-UNDO.
DEF VAR b AS CHAR /*INIT "0400"*/ NO-UNDO FORM "x(4)".
DEF VAR date1 AS DATE /*INIT 02/21/2014*/ FORMAT 99/99/99 NO-UNDO.
DEF VAR date2 AS DATE /*INIT 02/27/2014*/ FORMAT 99/99/99 NO-UNDO.

DEF STREAM ws.
     

                                                            
PAUSE 0.

FORM
date1 label "Начальная дата"
date2 label "Конечная дата"
    b label "Подразделение"

with frame www overlay side-labels 1 col centered row 6 title
color bright-white
"[ " + "Введите период " + "]" width 30.

DO on endkey undo, return on error undo, retry with frame www:
DISPLAY date1 date2 b.
SET 
 date1
 date2
 b
editing:
readkey.
apply lastkey.
END.
END.
DO on endkey undo, leave on error undo, leave with frame prn:


message("Минуточку...").
RUN Insert_TTName("Подразделение", b).
RUN Insert_TTName("НачальнаяДата", date1).
RUN Insert_TTName("КонечнаяДата", date2).



fname = "./_a.txt".        
OUTPUT STREAM ws TO VALUE (fname)
                 CONVERT TARGET "1251" SOURCE "IBM866".



/*=== РКО =============================================================================================*/   
/*=== ЮЛ ==============================================================================================*/
/*
PUT STREAM ws UNFORMATTED "количество мёртвых ЮЛ-клиентов:" + chr(13) + chr(10).
i1 = 0.
FOR EACH cust-corp 
  WHERE NOT CAN-FIND (FIRST loan WHERE loan.cust-id = cust-corp.cust-id
                                   AND loan.cust-cat = "Ю"
                                   AND loan.close-date EQ ?)
    AND NOT CAN-FIND (FIRST acct WHERE acct.cust-id = cust-corp.cust-id
                                   AND acct.cust-cat = "Ю"
                                   AND acct.close-date EQ ?) NO-LOCK:
      i1 = i1 + 1.
      PUT STREAM ws UNFORMATTED string(i1) + " " string(cust-corp.name-corp) + chr(13) + chr(10).
END.






PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "количество мёртвых ЮЛ-клиентов:" + chr(13) + chr(10).
i1 = 0.
FOR EACH cust-corp NO-LOCK: 
  FIND FIRST loan WHERE loan.cust-id = cust-corp.cust-id
                    AND loan.cust-cat = "Ю"
                    AND loan.close-date EQ ? NO-LOCK NO-ERROR.
    IF AVAIL loan THEN i1 = i1.
                  ELSE 
                    DO:
                      FIND FIRST acct WHERE acct.cust-id = cust-corp.cust-id
                                        AND acct.cust-cat = "Ю"
                                        AND acct.close-date EQ ? NO-LOCK NO-ERROR.
                        IF AVAIL acct THEN i1 = i1.
                                      ELSE 
                                        DO:
                                          i1 = i1 + 1.
                                          PUT STREAM ws UNFORMATTED string(i1) + " " string(cust-corp.name-corp) + chr(13) + chr(10).
                                        END.
                  END.
END.



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "количество ФЛ-клиентов на конец периода:" + chr(13) + chr(10).
i1 = 0.
FOR EACH person WHERE person.date-in <= date2
                  AND ((person.date-out EQ ?) OR (person.date-out > date2)) NO-LOCK by person.name-last:
                                          i1 = i1 + 1.
                                          PUT STREAM ws UNFORMATTED string(i1) + " " string(person.name-last) + " " + string(person.first-names) + chr(13) + chr(10).
END.





i1 = 0.
FOR EACH cust-corp WHERE cust-corp.date-in <= date2 NO-LOCK:
  FIND FIRST acct WHERE acct.cust-id = cust-corp.cust-id NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          IF acct.close-date <> ? THEN i1 = i1 + 1.
        END.
END.
*/







    

PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество открытых счетов ЮЛ на конец периода:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.open-date <= date2 
                    AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i1 = i1 + 1.
           RUN acct-pos IN h_base (acct.acct, acct.currency, date1, date2, "?").
           AcctBal = - sh-bal.
           summa = summa + AcctBal.
           PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.
RUN Insert_TTName("РКОКоличествоЮЛ", i1).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество новых счетов ЮЛ за данный период:" + chr(13) + chr(10).
i2 = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.open-date >= date1 
                    AND acct.open-date <= date2 NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i2 = i2 + 1.
           PUT STREAM ws UNFORMATTED string(i2) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество закрытых счетов ЮЛ за данный период:" + chr(13) + chr(10).
i3 = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.close-date >= date1 
                    AND acct.close-date <= date2 NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i3 = i3 + 1.
           PUT STREAM ws UNFORMATTED string(i3) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.



/*=== ИП ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество открытых счетов ИП на конец периода:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.open-date <= date2 
                    AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          i1 = i1 + 1.
          RUN acct-pos IN h_base (acct.acct, acct.currency, date1, date2, "?").
          AcctBal = - sh-bal.
          summa = summa + AcctBal.
          PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
        END.
END.
RUN Insert_TTName("РКОКоличествоИП", i1).
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("РКОСуммаЮЛ", summa).
                                         
 

PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество новых счетов ИП за данный период:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.open-date >= date1 
                    AND acct.open-date <= date2 NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          i1 = i1 + 1.
          PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
        END.
END.
i2 = i2 + i1.
RUN Insert_TTName("РКОКоличествоНовыхЮЛ", i2).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "РКО. количество закрытых счетов ИП за данный период:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "Транз1"
                    AND acct.close-date >= date1 
                    AND acct.close-date <= date2 NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          i1 = i1 + 1.
          PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
        END.
END.
i3 = i3 + i1.
RUN Insert_TTName("РКОКоличествоЗакрытыхЮЛ", i3).



/*=== ФЛ ==============================================================================================*/
/* РКО. количество открытых счетов ФЛ на конец периода */
i1 = 0.
FOR EACH acct WHERE acct.acct BEGINS "408"
                AND INDEX(acct.acct, "40802") <> 1
                AND INDEX(acct.acct, "40807") <> 1
                AND substring(acct.acct,10,4) = b
                AND acct.contract NE "Транз1"
                AND acct.open-date <= date2
                AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK:
  i1 = i1 + 1.
END.
RUN Insert_TTName("РКОКоличествоФЛ", i1).



/* РКО. количество новых счетов ФЛ за данный период */
i1 = 0.
FOR EACH acct WHERE acct.acct BEGINS "408"
                AND INDEX(acct.acct, "40802") <> 1             
                AND INDEX(acct.acct, "40807") <> 1
                AND substring(acct.acct,10,4) = b                
                AND acct.contract NE "Транз1"
                AND acct.open-date >= date1
                AND acct.open-date <= date2 NO-LOCK:
  i1 = i1 + 1.      
END.
RUN Insert_TTName("РКОПривлеченныеФЛ", i1).

  

/*=== ДЕПОЗИТЫ ========================================================================================*/
/*=== ЮЛ ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "ДЕПОЗИТЫ. количество депозитов ЮЛ на конец периода:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND ((signs.surrogate BEGINS "420") OR (signs.surrogate BEGINS "421") OR (signs.surrogate BEGINS "422"))
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
   FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                   AND acct.open-date <= date2 
                   AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i1 = i1 + 1.
           RUN acct-pos IN h_base (acct.acct, acct.currency, date1, date2, "?").
           AcctBal = - sh-bal.
           summa = summa + AcctBal.
           PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("ДепозитыКоличествоЮЛ", i1). 
RUN Insert_TTName("ДепозитыСуммаЮЛ", summa).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "ДЕПОЗИТЫ. количество депозитов ЮЛ за данный период:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "ККО"
                 AND signs.xattr-value = b
                 AND ((signs.surrogate BEGINS "420") OR (signs.surrogate BEGINS "421") OR (signs.surrogate BEGINS "422"))
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
   FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                  AND acct.close-date >= date1 
                  AND acct.close-date <= date2 NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          i1 = i1 + 1.
          PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
        END.
END.
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("ДепозитыПривлеченныеЮЛ", i1).
RUN Insert_TTName("ДепозитыСуммаПривлеченныхЮЛ", summa).



/*=== ФЛ ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "ДЕПОЗИТЫ. количество депозитов ФЛ на конец периода:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH acct WHERE ((acct.acct BEGINS "426") OR (acct.acct BEGINS "42305") OR (acct.acct BEGINS "42306"))
                AND substring(acct.acct,10,4) = b
                AND acct.kau-id <> "loan-dps-p"
                AND acct.open-date <= date2 
                AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK:
  i1 = i1 + 1.
  RUN acct-pos IN h_base (acct.acct, acct.currency, date1, date2, "?").
  AcctBal = - sh-bal.
  summa = summa + AcctBal.
  PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
END.
RUN Insert_TTName("ДепозитыКоличествоФЛ1", i1). 



/* количество Авто+ и МБ+ на конец периода */
i1 = 0.
FOR EACH signs WHERE signs.code = "subdivision"
                 AND signs.xattr-value = b
                 AND ((signs.surrogate BEGINS "42307") OR (signs.surrogate BEGINS "42607"))
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                  AND acct.open-date <= date2 
                  AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK NO-ERROR.
    IF AVAIL acct
      THEN
        DO:
          i1 = i1 + 1.
          RUN acct-pos IN h_base (acct.acct, acct.currency, date1, date2, "?").
          AcctBal = - sh-bal.
          summa = summa + AcctBal.
        END.
END.
RUN Insert_TTName("ДепозитыКоличествоФЛ2", i1). 
/* сумма Авто+ и МБ+ */
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("ДепозитыСуммаФЛ", summa).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "ДЕПОЗИТЫ. количество Gold_Card_ST и Gold_DV_ST за период:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH loan WHERE ((loan.cont-type = "Gold_Card_ST") OR (loan.cont-type = "Gold_DV_ST"))
                AND loan.branch-id = b
                AND loan.cust-cat = "Ч" 
                AND loan.open-date >= date1
                AND loan.open-date <= date2 NO-LOCK:
  FIND FIRST loan-acct WHERE loan-acct.cont-code EQ loan.cont-code
                         AND loan-acct.contract = loan.contract 
                         AND ((loan-acct.acct-type = "loan-dps-p") OR (loan-acct.acct-type = "loan-dps-t")) NO-LOCK NO-ERROR.
    IF AVAIL loan-acct
      THEN 
        DO:
          RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, date1, date2, "?").
          AcctBal = - sh-bal.
          summa = summa + AcctBal.
          i1 = i1 + 1.
          PUT STREAM ws UNFORMATTED string(i1) + " " + string(acct.acct) + chr(13) + chr(10).
        END.
END.
/* сумма Gold_Card_ST и Gold_DV_ST за период */
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("ДепозитыПривлеченныеФЛ3", i1). 
RUN Insert_TTName("ДепозитыСуммаПривлеченныхФЛ3", summa).

                                            

OUTPUT STREAM ws CLOSE.
/*RUN sndbispc ("file=" + fname + ";class=bq").*/


  
/* вывод данных по шаблону iParms (до "|") в файл отчета */
RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).   

END.