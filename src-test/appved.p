/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����" 
     Filename: .p
      Comment: ��楤�� �ନ஢���� ��� ���客�� ����ᮢ
   Parameters: 
         Uses:
      Used by: 
      Created: ayv
*/

{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEF TEMP-TABLE ttApp NO-UNDO
   FIELD num    AS INT	 /*���浪��� ����� ��ப�*/
   FIELD fio    AS CHAR  /*䨮 �।��㥬���*/
   FIELD numkd  AS CHAR  /*����� ��*/
   FIELD datekd AS DATE  /*��� ��*/
   .
   
DEF BUFFER bloan      FOR loan.
DEF BUFFER bperson    FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF VAR i    AS INT INIT 0 NO-UNDO.
DEF VAR vFio AS CHAR       NO-UNDO.

/*�ନ஢���� ������*/
FOR EACH tmprecid NO-LOCK,
   EACH bloan WHERE 
   RECID(bloan) EQ tmprecid.id 
NO-LOCK:
	
   /* ��।��塞 䨮 (�� ��直� ��砩 �஢��塞 � cust-corp) */
   IF bloan.cust-cat = '�' THEN DO:
      FIND FIRST bperson WHERE
         bperson.person-id EQ bloan.cust-id
      NO-LOCK NO-ERROR.
	  IF AVAIL bperson THEN 
	     vFio = bperson.name-last + ' ' + bperson.first-names.
   END.
   ELSE DO:
      FIND FIRST bcust-corp WHERE
	     bcust-corp.cust-id EQ bloan.cust-id
	  NO-LOCK NO-ERROR.
	  IF AVAIL bcust-corp THEN
	     vFio = bcust-corp.name-short.
   END.
   
   i = i + 1.
   
   /*������塞 temp-table*/
   CREATE ttApp.
   ASSIGN
      ttApp.num    = i
	  ttApp.fio    = vFio
	  ttApp.numkd  = bloan.doc-ref
	  ttApp.datekd = bloan.open-date
   .
   
END.

/*������塞 ⠡���� ��� 蠡����*/
RUN Insert_TTName ("graph", ""). 
   
FIND FIRST ttNames WHERE
   ttnames.tname EQ 'graph'
NO-LOCK NO-ERROR.
   
FOR EACH ttApp 
   BREAK BY ttApp.num:

   ttnames.tvalue = ttnames.tvalue + STRING(ttApp.num)                  + '\n'
                                   + STRING(ttApp.fio)                  + '\n'
                                   + STRING(ttApp.numkd)                + '\n'
                                   + STRING(ttApp.datekd, "99.99.9999") + '\n'
                                   + '���客�� ���䨪��'             + '\n'
								   + '\n'
                                   .
   
END.

RUN Insert_TTName ("date",STRING(DAY(TODAY))   + '.' +
						  STRING(MONTH(TODAY)) + '.' +
						  STRING(YEAR(TODAY))).
						  
RUN printvd.p('appved',INPUT TABLE ttnames).