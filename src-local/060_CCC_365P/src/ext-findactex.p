/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "����� �᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-findactex.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ���� ��� �� ���譥� ��⥬� � ��⮬ ४����⮢ ������.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
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

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

RUN ext-findactex_.p( iInn, iKpp, iName, iAcct, iAllFilials,
    OUTPUT TABLE ttExtAcct).

END.
