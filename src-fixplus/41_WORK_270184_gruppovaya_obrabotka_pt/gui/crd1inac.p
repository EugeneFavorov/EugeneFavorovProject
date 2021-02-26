{globals.i}

/* +++ crd1inac.p was humbly modified by (c)blodd converter v.1.09 on 1/25/2017 1:02pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: crd1inac.p
      Comment: ��㯯���� �࠭����� ��� ��ࠡ�⪨ ���⥦��� �ॡ������.
   Parameters:
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}
{intrface.get flt}
{topkind.def}
{tmprecid.def}
{tmpobj.def}
{intrface.get tmess}

define input param iOpDate like op.op-date no-undo.
define input param iOpRid  as recid        no-undo.

DEFINE VARIABLE mRes       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mErrMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mPlanDate  AS DATE        NO-UNDO.
DEFINE VARIABLE mDocNum    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCancel    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE h_kauproc  AS HANDLE      NO-UNDO.
DEFINE VARIABLE mKart1     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mOpl       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mGroup     AS CHARACTER   NO-UNDO INIT "*".

DEFINE VARIABLE proc-name  AS CHARACTER   NO-UNDO.

DEFINE BUFFER   bloan      FOR loan.

DEFINE TEMP-TABLE ttReport
   FIELD doc-num  LIKE op.doc-num
   FIELD msg      AS CHARACTER
.

SUBSCRIBE TO "CRD1-IN" ANYWHERE.

RUN kauproc.p PERSISTENT SET h_kauproc.

MAIN:
DO:
   IF (shFilial EQ "0500")
   THEN DO:
       RUN g-prompt.p ("char", "��㯯� ", "x(100)", "*", "������ ��㯯� ��⮢ (F1 - �롮�)", 50, ",", "F1=pb_getgroup.p",?,?,OUTPUT mGroup).
       IF (mGroup EQ ?)
       THEN DO:
          mCancel = YES.
          LEAVE MAIN.
       END.
   END.

   {empty tmprecid}
   RUN SelectFltObject IN h_flt("op",
                   "op-date1"      + CHR(1) + "op-date2"       + CHR(1) + "RidRest" + CHR(1) + "doc-type" + CHR(1) + "op-status"    + CHR(1) + "GroupList",
                   STRING(iOpDate) + CHR(1) + STRING (iOpDate) + CHR(1) + "YES"     + CHR(1) + "02"       + CHR(1) + "����,�,���,�" + CHR(1) + mGroup,
                   "op-date1"      + CHR(1) + "op-date2").

   FOR EACH tmprecid,
      FIRST op WHERE RECID(op) EQ tmprecid.id
         NO-LOCK:

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "ChgSts") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "Ann") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      RUN CheckOpRight IN h_base(RECID(op),
                                 gend-date,
                                 "ChgDt") NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         DELETE tmprecid.
         NEXT.
      END.

      CREATE TmpObj.
      TmpObj.Rid = RECID(op).
   END.

   {empty tmprecid}

   RUN browseld.p ("op",
                   "UseTmpObjInQuery"  + CHR(1) + "RidRest" + CHR(1) + "FieldSort" /* + CHR(1) + "GroupList" */,
                   STRING(mTmpObjHand) + CHR(1) + "YES"     + CHR(1) + "acct-db"   /* + CHR(1) + mGroup */,
                   "",
                   4).

   IF {&KEY_FUNCTION}({&LAST_KEY}) EQ "END-ERROR" THEN
      LEAVE MAIN.

   RUN GetPlanDate IN h_kauproc (INPUT-OUTPUT mPlanDate).
   IF mPlanDate EQ ? THEN
   DO:
      mCancel = YES.
      LEAVE MAIN.
   END.

   /* ����� �� ��࠭�� ���㬥�⠬ */
   FOR EACH tmprecid,
      FIRST op WHERE RECID(op) EQ tmprecid.id
         NO-LOCK:

      mDocNum = "".
      CREATE ttReport.

      {empty tOpKindParams} /* ������ ⠡���� ��ࠬ��஢ */
      ASSIGN
         mRes = TDAddParam ("CurOp", STRING(op.op))     AND
                TDAddParam ("PD",    STRING(mPlanDate))
      NO-ERROR.

      IF NOT mRes THEN
      DO:
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg     = "�訡�� ��।�� ��ࠬ��஢. ���㬥�� �ய�饭."
         .
         NEXT.
      END.

      RELEASE bloan NO-ERROR.
      mKart1 = NO.
      mOpl   = NO.

      /* �饬 �஢���� */
      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.

      /* �饬 ���.ᮣ��襭��  */
      FOR FIRST loan-acct
         WHERE loan-acct.acct     EQ op-entry.acct-db
           AND loan-acct.contract EQ "�����"
         NO-LOCK:

         FIND FIRST bloan
            WHERE bloan.contract         EQ '���ᑮ����'
              AND bloan.parent-contract  EQ loan-acct.contract
              AND bloan.parent-cont-code EQ loan-acct.cont-code
              AND bloan.open-date        LE gend-date
              AND (bloan.end-date        GE gend-date
                OR bloan.end-date        EQ ?)
              AND bloan.close-date       EQ ?
            NO-LOCK NO-ERROR.
      END.

      /* �� ��諨 ���.ᮣ��襭�� */
      IF NOT AVAIL bloan THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", "� ���� �" + DelFilFromAcct(op-entry.acct-db) + " ��� ᮣ��襭�� � ��࠭�� ������ ��楯�.").
         RUN Fill-SysMes IN h_tmess ("","","4", "���⠢��� ���㬥�� �" + op.doc-num + " �� ����⥪� 1?").
         IF pick-value EQ "YES"
            THEN mKart1 = YES.
      END.
      /* ��諨 ���.ᮣ��襭�� */
      ELSE DO:
         RUN f-ac_kas2.p(op.op).

         proc-name = GET-CLASS-METHOD(op.class-code, "Look").
         IF proc-name EQ ? THEN  proc-name = "op#v1".
         RUN VALUE(proc-name + ".p") (gend-date, op.user-id, op.op, 3).

         RUN Fill-SysMes IN h_tmess ("","","3","�롥�� ����⢨�|�������,���⠢��� �� �1,�⬥����").

         IF pick-value EQ "1"
            THEN mOpl = YES.
            ELSE IF pick-value EQ "2"
               THEN mKart1 = YES.
               ELSE mCancel = YES.
      END.

      /* �᫨ ��ࠫ� ������� */
      IF mOpl EQ YES THEN
      DO:
         RUN ex-trans.p ("_inCrd2ac", iOpDate, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

         IF NOT mOK THEN
         DO:
            ASSIGN
               ttReport.doc-num = op.doc-num
               ttReport.msg  = "�訡�� �࠭���樨 _inCrd2ac (�. " +
                               GetXAttrValueEx ("op-kind", "_inCrd1", "��_������⪫", "") +
                               "). ���㬥�� �ய�饭."
            .
            NEXT.
         END.
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg  = "���㬥�� ����祭."
         .
      END.

      /* �᫨ ��ࠫ� ���⠢��� ���㬥�� �� ����⥪� */
      IF mKart1 EQ YES THEN
      DO:
         RUN ex-trans.p ("_inCrd1ac", iOpDate, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

         IF NOT mOK THEN
         DO:
            ASSIGN
               ttReport.doc-num = op.doc-num
               ttReport.msg  = "�訡�� �࠭���樨 _inCrd1ac (�. " +
                               GetXAttrValueEx ("op-kind", "_inCrd1", "��_������⪫", "") +
                               "). ���㬥�� �ய�饭."
            .
            NEXT.
         END.
         ASSIGN
            ttReport.doc-num = op.doc-num
            ttReport.msg  = "���㬥�� ���⠢��� �� ����⥪�. ���㬥�� ���⠭���� " + mDocNum + "."
         .
      END.
   END.
END.

{setdest.i}
PUT UNFORMATTED "��㯯���� ��ࠡ�⪠ ���⥦��� �ॡ������" SKIP (1).
IF mCancel THEN
   PUT UNFORMATTED "------ ��ࢠ�� �� ������� ���짮��⥫�." SKIP (1).
FOR EACH ttReport:
   PUT UNFORMATTED STRING (ttReport.doc-num, "X(10)") ttReport.msg SKIP.
END.
{preview.i}

{empty tmprecid}
{intrface.del}          /* ���㧪� �����㬥����. */

PROCEDURE CRD1-IN:
DEFINE INPUT  PARAMETER iDocNum AS CHARACTER   NO-UNDO.
      mDocNum = iDocNum.
END PROCEDURE.

/* --- crd1inac.p was humbly modified by (c)blodd converter v.1.09 on 1/25/2017 1:02pm --- */
