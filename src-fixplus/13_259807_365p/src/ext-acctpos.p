/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-acctpos.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ������/���⪨ �� ����.
     Modified: 
*/
{globals.i}
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */

DEF INPUT  PARAM iBegDate  AS  DATE  NO-UNDO.   /* ��� ��砫� ���㧪�                          */
DEF INPUT  PARAM iEndDate  AS  DATE  NO-UNDO.   /* ��� ����砭�� ���㧪�                       */
DEF INPUT  PARAM iAllFil   AS  LOG   NO-UNDO.   /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩           */
DEF INPUT  PARAM TABLE     FOR ttExtAcct.       /* ������ �� �������� ��⠬                   */
DEF OUTPUT PARAM oAmtIn    AS  DEC   NO-UNDO.   /* �室�騩 ���⮪                              */
DEF OUTPUT PARAM oAmbDb    AS  DEC   NO-UNDO.   /* ������ �� ������                             */
DEF OUTPUT PARAM oAmtCr    AS  DEC   NO-UNDO.   /* ������ �� �।���                            */
DEF OUTPUT PARAM oAmt      AS  DEC   NO-UNDO.   /* ��室�騩 ���⮪                             */

/* �����頥� 䨪⨢�� ���⪨ �� ��� ���� */
FOR FIRST ttExtAcct NO-LOCK:

   ASSIGN
      oAmtIn = 123.44
      oAmbDb = 1.11
      oAmtCr = 2.22
      oAmt   = 567.88
   .

END. /* FOR FIRST ttExtAcct NO-LOCK: */