/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pfrotch1.pro
      Comment: 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� �� + ���� �� ���ᨮ��ࠬ
   Parameters: ���
         Uses:
      Used by: pfrotch1.p
      Created: 05/11/2015 KMBIS 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� ��
                                        ���� �� ���ᨮ��ࠬ
     Modified: 
                                    
*/

/*================================================================================================*/
/*=== �������� ��饣� ᯨ᪠ ���ᨮ��஢ =========================================================*/
PROCEDURE InitPensTT:
   DEF INPUT PARAM iAcct  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iBegD  AS DATE  NO-UNDO.
   DEF INPUT PARAM iEndD  AS DATE  NO-UNDO.

DEF VAR vPFRName  AS  CHAR  NO-UNDO.

DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bOp       FOR op.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPensAcct FOR ttPensAcct.

  lLastPfrOp:
  FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct
                      AND bOpEntry.op-date   GE iBegD
                      AND bOpEntry.op-date   LE iEndD
                      AND bOpEntry.op-status BEGINS CHR(251)
                    NO-LOCK,
     FIRST bAcct WHERE bAcct.acct     EQ bOpEntry.acct-cr 
                   AND bAcct.cust-cat EQ "�"
                 NO-LOCK
                 BREAK BY bAcct.cust-id
                       BY bOpEntry.op-date:

     IF LAST-OF(bAcct.cust-id) THEN
     DO:
        FIND FIRST bPensAcct WHERE bPensAcct.pers-id EQ bAcct.cust-id
                             EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL(bPensAcct) THEN
        DO:
           /* �� ������� ���ᨮ����, 㦥 ������� ��� � ���᫥���� ���ᨨ */
           /* �஢�ਬ, �� �� ��᫥���� ���᫥���                       */
           /* ����� ���� �������� ��� ��-�� �訡�� �����⬠, ⠪ � �   */
           /* ��砥 ��᪮�쪨� ��⮢ ��� (��ࠬ���� ��楤���)           */
           FIND FIRST bOp WHERE bOp.op      EQ bPensAcct.op-id
                            AND bOp.op-date GT bOpEntry.op-date
                          NO-LOCK NO-ERROR.
           /* ��������� ������ ��᫥����, ��������� �� �஢���� ᤥ���� ࠭�� */
           IF AVAIL(bOp) THEN
              NEXT lLastPfrOp.

        END. /* IF AVAIL(bPensAcct) THEN */
        ELSE 
        DO:
           CREATE bPensAcct.
           ASSIGN
              bPensAcct.proxy    = NO
              bPensAcct.print    = NO
              bPensAcct.pers-id  = bAcct.cust-id
           .
        END. /* IF AVAIL(bPensAcct) THEN ... ELSE */

        ASSIGN
           bPensAcct.acct-pfr = bOpEntry.acct-cr
           bPensAcct.op-id    = bOpEntry.op
        .
        RELEASE bPensAcct.

     END. /* IF LAST-OF(bOpEntry.op-date) THEN */
  END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct */

END PROCEDURE. /* InitPensTT */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����砥� ��� ���ᨮ��஢ ��������� �뢮�� � ���� =======================================*/
PROCEDURE FiltPensTT:
   DEF INPUT PARAM iRID   AS ROWID NO-UNDO.
   DEF INPUT PARAM iBegD  AS DATE  NO-UNDO.
   DEF INPUT PARAM iEndD  AS DATE  NO-UNDO.

DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bPensAcct FOR ttPensAcct.
DEF BUFFER bSign     FOR signs.

   FIND FIRST bPensAcct WHERE ROWID(bPensAcct) EQ iRID 
                        EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL(bPensAcct) THEN
      RETURN.

   FIND FIRST bOpEntry WHERE bOpEntry.acct-db   EQ bPensAcct.acct-pfr
                         AND bOpEntry.op-date   GE iBegD
                         AND bOpEntry.op-date   LE iEndD
                         AND bOpEntry.op-status BEGINS CHR(251)
                       NO-LOCK NO-ERROR.
   IF AVAIL(bOpEntry) THEN
   DO:
      /* �� ���� ������� ᯨᠭ��, �㦭� �஢���� ����稥 ᯨᠭ�� �� ����७���� */
      lOpFnd:
      FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ bPensAcct.acct-pfr
                          AND bOpEntry.op-date   GE iBegD
                          AND bOpEntry.op-date   LE iEndD
                          AND bOpEntry.op-status BEGINS CHR(251)
                        NO-LOCK,
         FIRST bSign WHERE bSign.file-name EQ "op"
                       AND bSign.surrogate EQ STRING(bOpEntry.op)
                       AND bSign.code      EQ "proxy-code"
                     NO-LOCK:
         /* �஢���� ᤥ���� �� ����७����, ����砥� ��� � ���� */
         ASSIGN
            bPensAcct.proxy    = YES 
            bPensAcct.print    = YES
         .
         LEAVE lOpFnd.
      END. /* FOR EACH op-entry WHERE op-entry.acct-db   EQ acct.acct */
   END. /* IF AVAIL(op-entry) THEN */
   ELSE
   DO:
      /* ���ᠭ�� �� ���� ���, ����� ��� �������筮 �������� �뢮�� � ���� */
      ttPensAcct.print = YES.
   END. /* IF AVAIL(bOpEntry) THEN ... ELSE */

END PROCEDURE. /* FiltPensTT */

/*================================================================================================*/
