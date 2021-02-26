{globals.i}

/* +++ stchkspb.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: stchkspb.p
      Comment: ��⮤ ����஫� �� ���������
   Parameters: Recid ���㬥��, ��� ���������
         Uses:
      Used by:
      Created: 04/02/2015 11:04 KMBIS ��⮬���᪮� ᯨᠭ�� �����ᨩ �� ���
                                      ����஫� ��������� ���㬥��
     Modified: 
*/
DEF INPUT PARAM iRecid  AS  RECID  NO-UNDO.
DEF INPUT PARAM iParam  AS  CHAR   NO-UNDO.

{globals.i}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */

DEF VAR mMsg      AS  CHAR  NO-UNDO. /* ����饭�� �� �訡��               */
DEF VAR mOpInfo   AS  CHAR  NO-UNDO. /* ����� ���㬥��                  */
DEF VAR mResult   AS  LOG   NO-UNDO. /* ���� �ᯥ譮� ��ࠡ�⪨           */
                      
DEF BUFFER bComOp  FOR op.
DEF BUFFER bxlink  FOR xlink.
DEF BUFFER blinks  FOR links.


ASSIGN
   PICK-VALUE = "YES"
   mResult    = YES
.

FOR FIRST op WHERE RECID(op) EQ iRecid
             NO-LOCK:

   IF     iParam       EQ     "status" 
      AND op.op-status BEGINS "�"
   THEN
   DO:
      mOpInfo = SUBST("����� '&1' �� &2",
                op.doc-num,
                STRING(op.op-date,"99/99/9999")).

      FIND FIRST bxlink WHERE bxlink.class-code EQ op.class-code
                          AND bxlink.link-code  EQ "�����"
                         NO-LOCK NO-ERROR.
      RELEASE blinks.

      IF AVAIL(bxlink) THEN
         FIND FIRST blinks WHERE blinks.link-id   EQ bxlink.link-id
                             AND blinks.source-id EQ STRING(op.op)
                           EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF     NOT AVAIL(blinks) 
         AND LOCKED(blinks)
      THEN
      DO:
     
         ASSIGN
            mMsg    = SUBST("���� � �����ᨮ��� ���㬥�⮬ �������஢��� (���㬥�� &2)",
                            mOpInfo)
            mResult = NO
         .
         RUN Fill-SysMes IN h_tmess("", "", "-1", mMsg).
     
      END. /* IF     NOT AVAIL(blinks)  */
      ELSE IF AVAIL(blinks) THEN
      DO:

         /* �஢��塞 �易��� �����ᨮ��� ���㬥�� */
         FIND FIRST bComOp WHERE bComOp.op EQ INT64(blinks.target-id)
                           EXCLUSIVE-LOCk NO-WAIT NO-ERROR.
         
         IF     NOT AVAIL(bComOp) 
            AND LOCKED(bComOp)
         THEN
         DO:
            /* �����ᨮ��� ���㬥�� �������஢�� */
            ASSIGN
               mMsg    = SUBST("�����ᨮ��� ���㬥�� �������஢�� " +
                               "(��� ���㬥�� &1)",
                               mOpInfo)
               mResult = NO
            .
            RUN Fill-SysMes IN h_tmess("", "", "-1", mMsg).
         
         END. /* IF     NOT AVAIL(bComOp)  // AND LOCKED(bComOp) */
         ELSE IF AVAIL(bComOp) THEN
         DO:
            ASSIGN
               bComOp.op-status = op.op-status
               bComOp.op-date   = ?
            .
            RELEASE bComOp.
         END. /* ELSE IF AVAIL(bComOp) */

         IF mResult THEN
         DO:
            /* ����塞 ��� */
            DELETE  blinks.
            RELEASE blinks.

         END. /* IF mResult THEN */
      END. /* ELSE IF AVAIL(blinks) THEN */

   END. /*   IF     iParam       EQ     "status" */
END. /* FOR FIRST op WHERE RECID(op) EQ a1  */

PICK-VALUE = STRING(mResult).

{intrface.del}

/* --- stchkspb.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am --- */
