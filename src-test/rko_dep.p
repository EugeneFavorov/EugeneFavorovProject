/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
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
date1 label "��砫쭠� ���"
date2 label "����筠� ���"
    b label "���ࠧ�������"

with frame www overlay side-labels 1 col centered row 6 title
color bright-white
"[ " + "������ ��ਮ� " + "]" width 30.

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


message("�������...").
RUN Insert_TTName("���ࠧ�������", b).
RUN Insert_TTName("��砫쭠�", date1).
RUN Insert_TTName("����筠�", date2).



fname = "./_a.txt".        
OUTPUT STREAM ws TO VALUE (fname)
                 CONVERT TARGET "1251" SOURCE "IBM866".



/*=== ��� =============================================================================================*/   
/*=== �� ==============================================================================================*/
/*
PUT STREAM ws UNFORMATTED "������⢮ ������ ��-�����⮢:" + chr(13) + chr(10).
i1 = 0.
FOR EACH cust-corp 
  WHERE NOT CAN-FIND (FIRST loan WHERE loan.cust-id = cust-corp.cust-id
                                   AND loan.cust-cat = "�"
                                   AND loan.close-date EQ ?)
    AND NOT CAN-FIND (FIRST acct WHERE acct.cust-id = cust-corp.cust-id
                                   AND acct.cust-cat = "�"
                                   AND acct.close-date EQ ?) NO-LOCK:
      i1 = i1 + 1.
      PUT STREAM ws UNFORMATTED string(i1) + " " string(cust-corp.name-corp) + chr(13) + chr(10).
END.






PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "������⢮ ������ ��-�����⮢:" + chr(13) + chr(10).
i1 = 0.
FOR EACH cust-corp NO-LOCK: 
  FIND FIRST loan WHERE loan.cust-id = cust-corp.cust-id
                    AND loan.cust-cat = "�"
                    AND loan.close-date EQ ? NO-LOCK NO-ERROR.
    IF AVAIL loan THEN i1 = i1.
                  ELSE 
                    DO:
                      FIND FIRST acct WHERE acct.cust-id = cust-corp.cust-id
                                        AND acct.cust-cat = "�"
                                        AND acct.close-date EQ ? NO-LOCK NO-ERROR.
                        IF AVAIL acct THEN i1 = i1.
                                      ELSE 
                                        DO:
                                          i1 = i1 + 1.
                                          PUT STREAM ws UNFORMATTED string(i1) + " " string(cust-corp.name-corp) + chr(13) + chr(10).
                                        END.
                  END.
END.



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "������⢮ ��-�����⮢ �� ����� ��ਮ��:" + chr(13) + chr(10).
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







    

PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ ������� ��⮢ �� �� ����� ��ਮ��:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
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
RUN Insert_TTName("���������⢮��", i1).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ ����� ��⮢ �� �� ����� ��ਮ�:" + chr(13) + chr(10).
i2 = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
                    AND acct.open-date >= date1 
                    AND acct.open-date <= date2 NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i2 = i2 + 1.
           PUT STREAM ws UNFORMATTED string(i2) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ �������� ��⮢ �� �� ����� ��ਮ�:" + chr(13) + chr(10).
i3 = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND INDEX(signs.surrogate, "409") <> 1
                 AND INDEX(signs.surrogate, "40817") <> 1
                 AND INDEX(signs.surrogate, "40820") <> 1
                 AND INDEX(signs.surrogate, "40802") <> 1
                 AND signs.surrogate BEGINS "40"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
                    AND acct.close-date >= date1 
                    AND acct.close-date <= date2 NO-LOCK NO-ERROR.
     IF AVAIL acct
       THEN
         DO:
           i3 = i3 + 1.
           PUT STREAM ws UNFORMATTED string(i3) + " " + string(acct.acct) + chr(13) + chr(10).
         END.
END.



/*=== �� ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ ������� ��⮢ �� �� ����� ��ਮ��:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
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
RUN Insert_TTName("���������⢮��", i1).
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("����㬬���", summa).
                                         
 

PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ ����� ��⮢ �� �� ����� ��ਮ�:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
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
RUN Insert_TTName("���������⢮����型", i2).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "���. ������⢮ �������� ��⮢ �� �� ����� ��ਮ�:" + chr(13) + chr(10).
i1 = 0.
FOR EACH signs WHERE signs.code = "���"
                 AND signs.xattr-value = b
                 AND signs.surrogate BEGINS "40802"
                 AND substring(signs.surrogate,6,3) = "810"
                 AND substring(signs.surrogate,10,4) = b NO-LOCK:
  FIND FIRST acct WHERE substring(acct.acct,1,20) = substring(signs.surrogate,1,20)
                    AND acct.contract NE "�࠭�1"
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
RUN Insert_TTName("���������⢮�������型", i3).



/*=== �� ==============================================================================================*/
/* ���. ������⢮ ������� ��⮢ �� �� ����� ��ਮ�� */
i1 = 0.
FOR EACH acct WHERE acct.acct BEGINS "408"
                AND INDEX(acct.acct, "40802") <> 1
                AND INDEX(acct.acct, "40807") <> 1
                AND substring(acct.acct,10,4) = b
                AND acct.contract NE "�࠭�1"
                AND acct.open-date <= date2
                AND ((acct.close-date EQ ?) OR (acct.close-date > date2)) NO-LOCK:
  i1 = i1 + 1.
END.
RUN Insert_TTName("���������⢮��", i1).



/* ���. ������⢮ ����� ��⮢ �� �� ����� ��ਮ� */
i1 = 0.
FOR EACH acct WHERE acct.acct BEGINS "408"
                AND INDEX(acct.acct, "40802") <> 1             
                AND INDEX(acct.acct, "40807") <> 1
                AND substring(acct.acct,10,4) = b                
                AND acct.contract NE "�࠭�1"
                AND acct.open-date >= date1
                AND acct.open-date <= date2 NO-LOCK:
  i1 = i1 + 1.      
END.
RUN Insert_TTName("����ਢ��祭�륔�", i1).

  

/*=== �������� ========================================================================================*/
/*=== �� ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "��������. ������⢮ ������⮢ �� �� ����� ��ਮ��:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "���"
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
RUN Insert_TTName("�������늮����⢮��", i1). 
RUN Insert_TTName("��������㬬���", summa).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "��������. ������⢮ ������⮢ �� �� ����� ��ਮ�:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH signs WHERE signs.code = "���"
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
RUN Insert_TTName("��������ਢ��祭�륞�", i1).
RUN Insert_TTName("��������㬬��ਢ��祭��型", summa).



/*=== �� ==============================================================================================*/
PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "��������. ������⢮ ������⮢ �� �� ����� ��ਮ��:" + chr(13) + chr(10).
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
RUN Insert_TTName("�������늮����⢮��1", i1). 



/* ������⢮ ���+ � ��+ �� ����� ��ਮ�� */
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
RUN Insert_TTName("�������늮����⢮��2", i1). 
/* �㬬� ���+ � ��+ */
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("��������㬬���", summa).



PUT STREAM ws UNFORMATTED chr(13) + chr(10) + "��������. ������⢮ Gold_Card_ST � Gold_DV_ST �� ��ਮ�:" + chr(13) + chr(10).
i1 = 0.
summa = 0.
FOR EACH loan WHERE ((loan.cont-type = "Gold_Card_ST") OR (loan.cont-type = "Gold_DV_ST"))
                AND loan.branch-id = b
                AND loan.cust-cat = "�" 
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
/* �㬬� Gold_Card_ST � Gold_DV_ST �� ��ਮ� */
summa = round((summa * 0.001) / 100, 4) * 100.
RUN Insert_TTName("��������ਢ��祭�륔�3", i1). 
RUN Insert_TTName("��������㬬��ਢ��祭��唋3", summa).

                                            

OUTPUT STREAM ws CLOSE.
/*RUN sndbispc ("file=" + fname + ";class=bq").*/


  
/* �뢮� ������ �� 蠡���� iParms (�� "|") � 䠩� ���� */
RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).   

END.