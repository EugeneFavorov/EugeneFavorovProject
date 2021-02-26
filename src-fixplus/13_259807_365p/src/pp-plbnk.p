/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-plbnk.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ������⥪� ��� ������ � ���譥� ��⥬��
     Modified: 
*/
{globals.i}

{intrface.get itax}
{intrface.get strng}
{intrface.get xclass}

/* ����塞 ��騥 ��६���� ��� �ᥣ� ������ */
{extexch.def}
{365p.def}

DEF VAR mDateNR365p  AS  DATE  NO-UNDO.         /* ��� ��砫쭮�� �襭��                       */
DEF VAR mZaprosNO    AS  LOG   NO-UNDO INIT NO. /* ����� �ॡ�� ������ �� ���譥� ��⥬�      */
DEF VAR mAllFil      AS  LOG   NO-UNDO INIT NO. /* ���� ��⮢ ���� �� ��� 䨫�����            */

DEF VAR mAcctFnd     AS  CHAR  NO-UNDO.         /* ���� ��� �� ������                         */
DEF VAR mAcctFndEx   AS  CHAR  NO-UNDO.         /* ���� ��� � ��⮬ ४�.������             */
DEF VAR mAcctPos     AS  CHAR  NO-UNDO.         /* ���⪨/������ �� ����                      */
DEF VAR mFillOp      AS  CHAR  NO-UNDO.         /* ����樨 �� ����                             */

/* �㭪樨 � ��楤��� ������⥪� */
{plbnk.fun}
{plbnk.pro}

ASSIGN
   mAcctFnd    = fGetSetting("����ன��_365�", "���珮��", "")
   mAcctFndEx  = fGetSetting("����ன��_365�", "���珮�᪊��", "")
   mAcctPos    = fGetSetting("����ன��_365�", "�����", "")
   mFillOp     = fGetSetting("����ன��_365�", "��掯��", "")
   mDateNR365p = DATE(fGetSetting("���_��", "", "")) 
NO-ERROR.