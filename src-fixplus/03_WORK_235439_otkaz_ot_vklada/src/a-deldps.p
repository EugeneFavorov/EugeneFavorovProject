/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: a-deldps.p
      Comment: TT:0235439 ������. �⪠� ������ �� ��ଫ���� ������
   Parameters:
         Uses:
      Used by:
      Created: 12/02/2015 14:33 KMBIS ������. �⪠� ������ �� ��ଫ���� ������
     Modified: 
*/
DEF INPUT PARAM iOpDate  AS  DATE  NO-UNDO.

{globals.i}
{intrface.get tmess}

DEF VAR mOk      AS  LOG   NO-UNDO. /* ������� �ᯥ譮 㤠���              */
DEF VAR mLogFile AS  CHAR  NO-UNDO. /* ���� � 䠩�� �����                  */
DEF VAR mMsg     AS  CHAR  NO-UNDO. 

DEF BUFFER bLAcct  FOR loan-acct.

DEF STREAM sExp.

{a-deldps.fun}
{a-deldps.pro}

{setdest.i} 

mLogFile = fGetSetting("�⪎��", "���", "").
IF NOT {assigned mLogFile} THEN
DO:
   RUN Fill-SysMes IN h_tmess("","", "-1", "�� ����� ���� � 䠩�� ����.").

END.

DO ON ERROR UNDO, RETRY:
IF RETRY THEN
DO:
   RUN Fill-SysMes IN h_tmess("","", "-1", "�訡�� ����� � 䠩� ����.").
   RETURN.
END.

   OUTPUT STREAM sExp TO VALUE(mLogFile) APPEND KEEP-MESSAGES.
END.

mMsg = SUBST("����� ����: &1 &2.",
             STRING(TODAY, "99/99/9999"),
             STRING(TIME, "HH:MM:SS")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

mMsg = SUBST("��ࠡ��뢠���� ���: &1.",
             STRING(iOpDate, "99/99/9999")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).
RUN SetMsg IN THIS-PROCEDURE(0, "====================").

lMainDps:
FOR EACH loan WHERE loan.contract  EQ "dps"
                AND loan.open-date EQ iOpDate
                AND loan.end-date  NE ?
                AND loan.cust-cat  EQ "�"
              NO-LOCK,
   FIRST person WHERE person.person-id EQ loan.cust-id
                NO-LOCK:

   /* ���� �� ஫� ��⮢, �ਢ易��� �� � ���� ������ ������� */
   FIND FIRST loan-acct WHERE loan-acct.contract  EQ loan.contract
                          AND loan-acct.cont-code EQ loan.cont-code
                          AND loan-acct.since     NE loan.open-date
                        NO-LOCK NO-ERROR.

   IF AVAIL(loan-acct) THEN /* ���� ஫� �ਢ易��� ��⮩ <> ��� ������ */
      NEXT lMainDps.

   IF FndOp(loan.contract, loan.cont-code) EQ NO THEN
   DO:
      RUN DelDps IN THIS-PROCEDURE(loan.contract,
                                   loan.cont-code,
                                   NO,
                                   OUTPUT mOk).
      IF mOk THEN
      DO:
         mMsg = SUBST("������� 㤠��� �ᯥ譮.").
         RUN SetMsg IN THIS-PROCEDURE(0, mMsg).
      END.
      ELSE 
      DO:
         mMsg = SUBST("������! ������� &1 �� 㤠���!",
                      loan.cont-code).
         RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

      END. /* IF mOk THEN ... ELSE */

      RUN SetMsg IN THIS-PROCEDURE(0, "").

   END. /* IF mFindOp EQ NO THEN */

END. /* lMainDps: */

mMsg = SUBST("����� �����祭: &1 &2",
             STRING(TODAY, "99/99/9999"),
             STRING(TIME, "HH:MM:SS")).

RUN SetMsg IN THIS-PROCEDURE(0, mMsg).

PUT STREAM sExp UNFORM SKIP(1).
OUTPUT STREAM sExp CLOSE.
