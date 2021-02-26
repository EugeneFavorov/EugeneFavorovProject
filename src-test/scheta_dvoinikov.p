/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
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
    ID_klient_1 label "ID ������ 1"
    ID_klient_2 label "ID ������ 2"

with frame www overlay side-labels 1 col centered row 6 title color bright-white
"[ " + "������� ������" + " ]" width 30.

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
         MESSAGE "������ � ID" ID_klient_1 "�� ������!" view-as alert-box title "������".
         RETURN "ERROR".
     END.

FIND FIRST person2 WHERE person2.person-id = int(ID_klient_2) NO-LOCK NO-ERROR.
IF NOT AVAIL person2
THEN DO:
         MESSAGE "������ � ID" ID_klient_2 "�� ������!" view-as alert-box title "������".
         RETURN "ERROR".
     END.

IF AVAIL person1 AND AVAIL person2
THEN MESSAGE "��७�ᨬ ��� � ������"
             + chr(10) + chr(10) +
             ID_klient_1 ":" person1.name-last person1.first-names
             + chr(10) + chr(10) +
             "�� ������"
             + chr(10) + chr(10) +
             ID_klient_2 ":" person2.name-last person2.first-names view-as alert-box QUESTION BUTTONS YES-NO title "������" set vChv.
ELSE RETURN "ERROR".

IF vChv
THEN DO:
         FOR EACH acct WHERE acct.cust-id  = int(ID_klient_1)
                         AND acct.cust-cat = "�" EXCLUSIVE-LOCK:
             i = i + 1.
             acct.cust-id = int(ID_klient_2).
             PUT UNFORMATTED acct.acct + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "��७�ᥭ� ��⮢: " i skip.

/*      
         /* ��७�� �।�⭮�� ������� */
	 i = 0.  
	 FOR EACH loan WHERE  
			  loan.cust-id  = int(ID_klient_1)
                         AND loan.cust-cat = "�" EXCLUSIVE-LOCK:
             i = i + 1.
             loan.cust-id = int(ID_klient_2).
             PUT UNFORMATTED loan.doc-ref + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "��७�ᥭ� ������஢: " i skip.

         i = 0.
         FOR EACH term-obl WHERE  

			  term-obl.fop  = int(ID_klient_1)
                         AND term-obl.symbol = "�" 

			EXCLUSIVE-LOCK:
             i = i + 1.
             term-obl.fop = int(ID_klient_2).
             PUT UNFORMATTED term-obl.cont-code + chr(13) + chr(10).
         END.
         PUT UNFORMATTED "------------------------------" + chr(13) + chr(10).
         PUT UNFORMATTED "��७�ᥭ� term-obl: " i skip.
*/





     END.
ELSE DO:
         MESSAGE "��७�� ��⮢ �⬥��." view-as alert-box title "���������".
         RETURN "ERROR". 
     END.



{preview.i &col=170}

END.