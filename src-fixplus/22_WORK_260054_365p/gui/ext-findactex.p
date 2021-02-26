{globals.i}
{intrface.get tmess}

/* +++ ext-findactex.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-findactex.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ���� ��� �� ���譥� ��⥬� � ��⮬ ४����⮢ ������.
     Modified: 
*/
{globals.i}
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */

DEF INPUT  PARAM iInn        AS  CHAR   NO-UNDO.   /* ��� ������                                */
DEF INPUT  PARAM iKpp        AS  CHAR   NO-UNDO.   /* ��� ������                                */
DEF INPUT  PARAM iName       AS  CHAR   NO-UNDO.   /* ������������ ������                       */
DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.   /* ����� ���                                */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.   /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩        */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.        /* ������ �� �������� ��⠬                */

/*
   �᫨ ��� ������ � ��ன ��⥬�, �饬 ��� �������� � ���, � ��⮬ ��।����� ���, ���
   ��।������ cust-cat, cust-id:
   1. cust-cat = "�", �᫨ ������ ��室���� � ⠡��� person
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� cust-corp
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� banks
      cust-cat = "" (����), �᫨ ������ �� ��ன ��⥬� �� ������ � ���

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - ᮮ⢥⢥���
      cust-id = -1, �᫨ ������ �� ������

   3. ������ ������ ��� ᮮ⢥���� ���� ������ � ⠡��� ttExtAcct. ���� �᫨ � ��⥬� �� 
      ��᪮�쪮, � ������� �ਭ������� ⮫쪮 ����(��� �� �ਭ�������). �᫨ ��� �ਭ�������
      ��㣮�� ������, � ��� ࠧ���� ������ ������.

   4. ��楤�� �।�ᬠ�ਢ��� ��� ०��� ࠡ���:
      1. ��।����� ����� ��� ��� �஢�ન �ਭ��������� ��� ������
      2. ��।����� ��᪠ "*" ��� �ନ஢���� ��� ��⮢ ������, ��������� ��ࠢ�� � ��.

   ���稥 ����� acct.number �� acct.acct: 
      - acct.number �� ᮤ�ন� �ਧ���� 䨫���쭮��, 
      - acct.acct ᮤ�ন� �ਧ��� 䨫���쭮��, ��᫥ @
*/

IF iAcct EQ "*" THEN
DO:
   /* ��㫨�㥬 ���� ��⮢ ������ */
   FOR EACH acct WHERE acct.cust-cat EQ     "�"
                   AND acct.cust-id  EQ     212
                   AND acct.acct     BEGINS "4"
                 NO-LOCK:
      /* ������� �६����� ⠡���� */
      CREATE ttExtAcct.

      /* �����㥬 ����� �� ⠡���� � �⮣���� ⠡���� */
      BUFFER-COPY acct TO ttExtAcct.
      ASSIGN
        ttExtAcct.acct       = "9" + ttExtAcct.acct 
        ttExtAcct.number     = "9" + ttExtAcct.number
        ttExtAcct.open-date  = DATE("01.01.2000")
        ttExtAcct.close-date = DATE("01.11.2015")
      .
      /* ���࠭塞 ��������� � ⠡���� */
      VALIDATE ttExtAcct NO-ERROR.

   END. /* FOR EACH acct WHERE acct.cust-cat EQ     "�" */
END. /* IF iAcct EQ "*" THEN */
ELSE
DO:
   /* ���� ��� ������ */
   RUN ext-findact.p(iAcct,
                     iAllFilials,
                     OUTPUT TABLE ttExtAcct) 
                    NO-ERROR.
END. /* IF iAcct EQ "*" THEN ... ELSE */


/* --- ext-findactex.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am --- */
