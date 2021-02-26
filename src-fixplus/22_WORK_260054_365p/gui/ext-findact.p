{globals.i}
{intrface.get tmess}

/* +++ ext-findact.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-fillop.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ���� ��� �� ���譥� ��⥬�.
     Modified: 
*/
{globals.i}
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */

DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.    /* ����� ���                               */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.    /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩       */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.         /* ������ �� �������� ��⠬               */

/*
   �᫨ ��� ������ � ��ன ��⥬�, �饬 ��� �������� � ���
   ��।������ cust-cat, cust-id:
   1. cust-cat = "�", �᫨ ������ ��室���� � ⠡��� person
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� cust-corp
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� banks
      cust-cat = "" (����), �᫨ ������ �� ��ன ��⥬� �� ������ � ���

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - ᮮ⢥⢥���
      cust-id = -1, �᫨ ������ �� ������

   3. �᫨ �������� ��᪮�쪮 ��⮢, � ��� ������� ᮧ���� ᢮� ������ � ⠡��� ttExtAcct
*/

/* ��室�� ������ � ⠡��� acct, �� �� �� ��������� �� ���� ������ */
FIND FIRST acct WHERE acct.acct BEGINS "40702810050010146608"
                NO-LOCK NO-ERROR.

IF AVAIL(acct) THEN 
DO:
   /* ������� �६����� ⠡���� */
   CREATE ttExtAcct.

   /* �����㥬 ����� �� ⠡���� */
   BUFFER-COPY acct TO ttExtAcct.

   /* �� ����� ��८�।���� */
   ASSIGN
     ttExtAcct.cust-cat = "�" 
     ttExtAcct.cust-id  = 212  /* ����� ��⮢��� ������ � ���� */
     ttExtAcct.acct     = iAcct
     ttExtAcct.number   = iAcct

     ttExtAcct.open-date  = DATE("01.01.2000")
     ttExtAcct.close-date = DATE("01.11.2015")

   .
   /* ���࠭塞 ��������� � ⠡���� */
   VALIDATE ttExtAcct NO-ERROR.

END. /* IF AVAIL(acct) THEN  */

RETURN.
/* --- ext-findact.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am --- */
