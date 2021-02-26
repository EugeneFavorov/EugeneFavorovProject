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

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{extexch.def}

DEF INPUT  PARAM iBegDate  AS  DATE  NO-UNDO.   /* ��� ��砫� ���㧪�                          */
DEF INPUT  PARAM iEndDate  AS  DATE  NO-UNDO.   /* ��� ����砭�� ���㧪�                       */
DEF INPUT  PARAM iAllFil   AS  LOG   NO-UNDO.   /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩           */
DEF INPUT  PARAM TABLE     FOR ttExtAcct.       /* ������ �� �������� ��⠬                   */
DEF OUTPUT PARAM oAmtIn    AS  DEC   NO-UNDO.   /* �室�騩 ���⮪                              */
DEF OUTPUT PARAM oAmbDb    AS  DEC   NO-UNDO.   /* ������ �� ������                             */
DEF OUTPUT PARAM oAmtCr    AS  DEC   NO-UNDO.   /* ������ �� �।���                            */
DEF OUTPUT PARAM oAmt      AS  DEC   NO-UNDO.   /* ��室�騩 ���⮪                             */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

RUN ext-acctpos_.p( iBegDate, iEndDate, iAllFil, INPUT TABLE ttExtAcct, 
    OUTPUT oAmtIn, OUTPUT oAmbDb, OUTPUT oAmtCr, OUTPUT oAmt).


CATCH eAnyError AS Progress.Lang.Error:
    message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.
    RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.
END.
