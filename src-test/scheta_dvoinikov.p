/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:  
      Created: fev
*/

                                                     

{globals.i}
{setdest.i}
{chkacces.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{wordwrap.def}
{intrface.get tmess}
{tmprecid.def}

DEF VAR ID_klient_1   AS CHAR form "x(7)".
DEF VAR ID_klient_2   AS CHAR form "x(7)".
DEF VAR name_klient_1 AS CHAR form "x(50)".
DEF VAR name_klient_2 AS CHAR form "x(50)".
DEF VAR vChv          AS LOGICAL.
DEF VAR i             AS INTEGER.

DEF BUFFER person1 FOR person.
DEF BUFFER person2 FOR person.



pause 0.

form
    ID_klient_1 label "ID клиента 1"
    ID_klient_2 label "ID клиента 2"

with frame www overlay side-labels 1 col centered row 6 title color bright-white
"[ " + "ВВЕДИТЕ ДАННЫЕ" + " ]" width 30.

do on endkey undo, return on error undo, retry with frame www:
display ID_klient_1 ID_klient_2.
set 
 ID_klient_1
 ID_klient_2
editing:
readkey.
apply lastkey.
end.
end.
do on endkey undo, leave on error undo, leave with frame prn:



{setdest.i &col=170}

i = 0.
vChv = FALSE.

FIND FIRST person1 WHERE person1.person-id = int(ID_klient_1) NO-LOCK NO-ERROR.
IF NOT AVAIL person1
THEN DO:
         MESSAGE "Клиент с ID" ID_klient_1 "не найден!" view-as alert-box title "ОШИБКА".
         RETURN "ERROR".
     END.

FIND FIRST person2 WHERE person2.person-id = int(ID_klient_2) NO-LOCK NO-ERROR.
IF NOT AVAIL person2
THEN DO:
         MESSAGE "Клиент с ID" ID_klient_2 "не найден!" view-as alert-box title "ОШИБКА".
         RETURN "ERROR".
     END.

IF AVAIL person1 AND AVAIL person2
THEN MESSAGE "Переносим счета с клиента"
             + chr(10) + chr(10) +
             ID_klient_1 ":" person1.name-last person1.first-names
             + chr(10) + chr(10) +
             "на клиента"
             + chr(10) + chr(10) +
             ID_klient_2 ":" person2.name-last person2.first-names view-as alert-box QUESTION BUTTONS YES-NO title "ВОПРОС" set vChv.
ELSE RETURN "ERROR".

IF vChv
THEN DO:
         FOR EACH acct WHERE acct.cust-id  = int(ID_klient_1)
                         AND acct.cust-cat = "Ч" EXCLUSIVE-LOCK:
             i = i + 1.
             acct.cust-id = int(ID_klient_2).
             PUT UNFORMATTED acct.acct + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "Перенесено счетов: " i skip.

/*      
         /* перенос кредитного договора */
	 i = 0.  
	 FOR EACH loan WHERE  
			  loan.cust-id  = int(ID_klient_1)
                         AND loan.cust-cat = "Ч" EXCLUSIVE-LOCK:
             i = i + 1.
             loan.cust-id = int(ID_klient_2).
             PUT UNFORMATTED loan.doc-ref + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "Перенесено договоров: " i skip.

         i = 0.
         FOR EACH term-obl WHERE  

			  term-obl.fop  = int(ID_klient_1)
                         AND term-obl.symbol = "Ч" 

			EXCLUSIVE-LOCK:
             i = i + 1.
             term-obl.fop = int(ID_klient_2).
             PUT UNFORMATTED term-obl.cont-code + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "Перенесено term-obl: " i skip.
*/





     END.
ELSE DO:
         MESSAGE "Перенос счетов отменён." view-as alert-box title "СООБЩЕНИЕ".
         RETURN "ERROR". 
     END.



{preview.i &col=170}

END.