/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����" 
     Filename: rasppdg.p
      Comment: ��楤�� �ନ஢���� �ᯮ�殮��� �� ᯨᠭ�� ������ �� �� �।��
   Parameters: 
         Uses:
      Used by: 
      Created: pda
*/

{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{intrface.get loan}

DEF TEMP-TABLE ttApp NO-UNDO
   FIELD num    AS INT	    /*���浪��� ����� ��ப�*/
   FIELD fio    AS CHAR     /*䨮 �।��㥬���*/
   FIELD numkd  AS CHAR     /*����� ��*/
   FIELD datekd AS DATE     /*��� ��*/
   FIELD sumob  AS DECIMAL  /*�㬬� ���ᯥ祭��*/
   FIELD sumob2 AS DECIMAL  /*�㬬� ���ᯥ祭��*/
   .
   
DEF BUFFER bloan      FOR loan.
DEF BUFFER bperson    FOR person.
DEF BUFFER bcust-corp FOR cust-corp.

DEF INPUT PARAMETER iParam AS CHAR NO-UNDO.

DEF VAR i        AS INT INIT 0 NO-UNDO.
DEF VAR vFio     AS CHAR       NO-UNDO.
DEF VAR a1       AS CHAR       NO-UNDO.
DEF VAR a2       AS CHAR       NO-UNDO.
DEF VAR par_28   AS DECIMAL    NO-UNDO.
DEF VAR par_40   AS DECIMAL    NO-UNDO.

/* {getdate.i} */

/*�ନ஢���� ������*/
FOR EACH tmprecid NO-LOCK,
   EACH bloan WHERE 
   RECID(bloan) EQ tmprecid.id 
NO-LOCK:
	
   /* ��।��塞 䨮 (�� ��直� ��砩 �஢��塞 � cust-corp) */
   IF bloan.cust-cat = '�' THEN 
   DO:
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

   RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                            bloan.cont-code,  /* ����� ������� */
                            28,               /* ��� ��ࠬ��� */
                            TODAY,
                            OUTPUT par_28,    /* �㬬� ��ࠬ��� */
                            OUTPUT a1,        /* ����� ��ࠬ��� */
                            OUTPUT a2
                           ).                 /* �㬬� ��ࠬ��� � �㡫�� */

   RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                            bloan.cont-code,  /* ����� ������� */
                            40,               /* ��� ��ࠬ��� */
                            TODAY,
                            OUTPUT par_40,    /* �㬬� ��ࠬ��� */
                            OUTPUT a1,        /* ����� ��ࠬ��� */
                            OUTPUT a2
                           ).                 /* �㬬� ��ࠬ��� � �㡫�� */

   /*������塞 temp-table*/
   CREATE ttApp.
   ASSIGN
      ttApp.num    = i
	   ttApp.fio    = vFio
	   ttApp.numkd  = bloan.doc-ref
	   ttApp.datekd = bloan.open-date
      ttApp.sumob  = par_28
      ttApp.sumob2 = par_40
   .
   
END.

/*������塞 ⠡���� ��� 蠡����*/
RUN Insert_TTName ("graph", ""). 
   
FIND FIRST ttNames WHERE
   ttnames.tname EQ 'graph'
NO-LOCK NO-ERROR.
   
FOR EACH ttApp 
   BREAK BY ttApp.num:

   ttnames.tvalue = ttnames.tvalue + STRING(ttApp.num)                                       + '\n'
                                   + STRING(ttApp.numkd) + ' �� ' 
                                                         + STRING(ttApp.datekd, "99.99.9999")
                                                         + ' �.' + '\n'
                                   + STRING(ttApp.fio)                                       + '\n'
                                   + STRING(ttApp.sumob)                                     + '\n'
                                   + STRING(ttApp.sumob2)                                    + '\n'
                                   .
END.

FIND FIRST _user WHERE 
           _userid EQ userid("bisquit")
NO-LOCK NO-ERROR.

IF AVAIL _user THEN 
   RUN Insert_TTName("username", _user._user-name).

RUN Insert_TTName ("date",STRING(DAY(TODAY),"99") + '.' +
						  STRING(MONTH(TODAY),"99") + '.' +
						  STRING(YEAR(TODAY),"9999") + ' �.').
						  
RUN printvd.p(iParam, INPUT TABLE ttnames).