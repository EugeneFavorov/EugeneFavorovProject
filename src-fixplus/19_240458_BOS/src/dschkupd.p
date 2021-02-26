/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2005 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: dschkupd.p
      Comment: RunAfterCreate ��� ����� opbwu-send
   Parameters:
         Uses:
      Used by:
      Created: 13/05/2008 kraw (0086301)
     Modified: 30/04/2009 kraw (0108378) ��ࠢ����� �訡��.
*/
{globals.i}
{intrface.get re}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get instrum}

DEFINE INPUT PARAMETER iRID AS RECID NO-UNDO.

DEFINE VARIABLE mMsg        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSummP      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mItogSumm   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mTmpDec     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mRegExp     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResult     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErrMes     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocNumSend AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurr       AS CHARACTER NO-UNDO. /* ���ண�� ��ꥪ� */

DEFINE BUFFER op       FOR op.
DEFINE BUFFER opx      FOR op.
DEFINE BUFFER op-entry FOR op-entry.

pick-value = "YES".


FIND FIRST op WHERE RECID(op) EQ iRID NO-LOCK NO-ERROR.

IF NOT AVAILABLE op THEN
   RETURN.

mSurr = STRING(op.op).
mDocNumSend = GetXAttrValueEx("op", mSurr, "Doc-Num-Send", "").

IF op.class-code NE "opbwu-send" THEN
   RETURN.

IF GetXAttrValueEx("op", mSurr, "��␥�", "") NE "yes" THEN
   RETURN.

mRegExp     = GetCodeMisc("�������", GetXAttrValueEx("op", mSurr, "Doc-Type-Send", "����"), 3).

IF NOT DYNAMIC-FUNCTION("ereg", mRegExp, mDocNumSend, OUTPUT mResult, INPUT-OUTPUT mErrMes) THEN 
DO:
   RUN Fill-SysMes("","","0",'����� ���㬥�� �� ᮮ⢥����� �ࠢ��� ������樨, 㪠������ � �����䨪��� �������').
   pick-value = "NO".
   RETURN ERROR.
END.

mItogSumm = 0.0.

FOR EACH opx WHERE opx.op-date EQ op.op-date
               AND opx.class-code EQ "opbwu-send"
   NO-LOCK:

   IF  GetXAttrValueEx("op", STRING(opx.op), "doc-num-send", "") NE mDocNumSend THEN
      NEXT.

   FIND FIRST op-entry OF opx WHERE op-entry.op-entry EQ 1 NO-LOCK NO-ERROR.

   IF NOT AVAILABLE op-entry THEN
      NEXT.

   mSummP = DECIMAL(GetXAttrValueEx("op", STRING(opx.op) , "�㬬����", "0")) NO-ERROR.

   IF op-entry.currency NE "840" THEN
      mSummP = CurToCur("����",
                        op-entry.currency,
                        "840",
                        opx.op-date,
                        mSummP).
   mItogSumm = mItogSumm + mSummP.
END.

IF mItogSumm GT DECIMAL(GetXAttrValueEx("op-kind", op.op-kind, "����㬬����", "0.0")) THEN
DO:
   FIND FIRST op-entry OF op WHERE op-entry.op-entry EQ 1 NO-LOCK NO-ERROR.

   ASSIGN
      mSummP    = mItogSumm 
      mSummP    = mSummP    - DEC(GetXAttrValueEx("op-kind", op.op-kind, "����㬬����", "0.0"))
      mTmpDec   = DEC(GetXAttrValueEx("op", mSurr, "�㬬����", "0.0"))
      mTmpDec   = CurToCur("����",
                           op-entry.currency,
                           "840",
                           op.op-date,
                           mTmpDec)         WHEN AVAIL(op-entry) AND op-entry.currency NE "840"
      mItogSumm = mItogSumm - mTmpDec
   NO-ERROR.
   mMsg = SUBST('�⮣���� �㬬� �ॢ�ᨫ� ���祭�� �� "���ᨬ��쭠� �㬬� ��ॢ���� �� ����" ' +
                '�࠭���樨 �� &1 (USD). �㬬� ��ॢ���� ������ �� ���� ��⠢��� &2 (USD)',
                STRING(ROUND(mSummP, 2)),
                STRING(ROUND(mItogSumm, 2))).
   RUN Fill-SysMes("","","0", mMsg).
   pick-value = "NO".
   RETURN ERROR.
END.

pick-value = "YES".

RETURN "".
