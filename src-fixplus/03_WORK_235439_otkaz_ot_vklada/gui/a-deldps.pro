
/* +++ a-deldps.pro was humbly modified by (c)blodd converter v.1.09 on 7/27/2015 8:10am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: a-deldps.pro
      Comment: TT:0235439 ������. �⪠� ������ �� ��ଫ���� ������
   Parameters:
         Uses:
      Used by: a-deldps.p
      Created: 12/02/2015 14:33 KMBIS ������. �⪠� ������ �� ��ଫ���� ������
                                      ��楤��� 㤠�����
     Modified: 
*/
/*================================================================================================*/
/*=== �᭮���� ��楤�� 㤠����� ������ =========================================================*/
PROCEDURE DelDps:
  DEF INPUT  PARAM iContr AS  CHAR  NO-UNDO.
  DEF INPUT  PARAM iCode  AS  CHAR  NO-UNDO.
  DEF INPUT  PARAM iIsDV  AS  LOG   NO-UNDO. /* ����� �� ����ॡ������ */
  DEF OUTPUT PARAM oOk    AS  LOG   NO-UNDO.

DEF VAR vMsg     AS  CHAR  NO-UNDO. /* ����饭�� � ��� */
DEF VAR vLoanDV  AS  CHAR  NO-UNDO. /* ��� ������ ��   */
DEF VAR vOk      AS  LOG   NO-UNDO. /* ��� ������ ��   */


DEF BUFFER bDpsLoan  FOR loan.
DEF BUFFER bDVLoan   FOR loan.
DEF BUFFER bLoanAcct FOR loan-acct.
DEF BUFFER bLAcct    FOR loan-acct.
DEF BUFFER bAcct     FOR acct.

oOk = NO.

lMainDel:
DO TRANSACTION ON ERROR UNDO, THROW:

   vMsg = SUBST("�������: &1",
                iCode).
   RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

   FIND FIRST bDpsLoan WHERE bDpsLoan.contract  EQ iContr
                         AND bDpsLoan.cont-code EQ iCode 
                       EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF LOCKED(bDpsLoan) THEN
   DO:
      vMsg = SUBST("������� &1 �������஢�� ��㣨� ���짮��⥫��, �ய�饭.",
                   iContr).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO lMainDel, LEAVE lMainDel.

   END. /* IF LOCKED(bDpsLoan) THEN */
   ELSE IF AVAIL(bDpsLoan) THEN
   DO:
      IF iIsDV EQ NO THEN
      DO:
         vMsg = SUBST("�������� ������� (person-id): &1",
                      STRING(bDpsLoan.cust-id)).
         RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

         /*=== ������ �易��� � ������� ������� �� ===*/
         vLoanDV = FndDV(bDpsLoan.contract, bDpsLoan.cont-code).
         IF NUM-ENTRIES(vLoanDV) GT 1 THEN
         DO:
            vMsg = SUBST("&1 &2",
                         "����� �易� � ��᪮�쪨�� ������ࠬ� �� ����ॡ������",
                         "��� ��᪮�쪨�� ��⠬� � ஫�� 'loan-dps-p'").
            RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            UNDO lMainDel, LEAVE lMainDel.
        
         END. /* IF NUM-ENTRIES(vLoanDV) GT 1 THEN */
      END. /* IF iIsDV EQ NO THEN */

      lAcctLoop:
      FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract
                           AND bLoanAcct.cont-code EQ bDpsLoan.cont-code
                         NO-LOCK,
         FIRST bAcct WHERE bAcct.acct     EQ bLoanAcct.acct
                       AND bAcct.currency EQ bLoanAcct.currency
                     NO-LOCK
                     BREAK BY bLoanAcct.acct
                           BY bLoanAcct.acct-type
                     ON ERROR UNDO, THROW:

         IF FIRST-OF(bLoanAcct.acct) THEN
         DO:
            vMsg = SUBST("��� &1:", bAcct.acct).
            RUN SetMsg IN THIS-PROCEDURE(0, vMsg).
         END.

         IF LAST-OF(bLoanAcct.acct-type) THEN
         DO:
            vMsg = bLoanAcct.acct-type.
            /*=== ����塞 ஫� ===*/
            RUN DelLoanAcct(ROWID(bLoanAcct), OUTPUT vOk).
            IF vOk THEN
            DO:
               vMsg = SUBST("- ������� ஫�: &1.",
                            vMsg).
               RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

            END. /* IF vOk THEN */
            ELSE
            DO:
               vMsg = SUBST("�訡��: �� 㤠���� 㤠���� ஫� &1.",
                            vMsg).
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
               UNDO lMainDel, LEAVE lMainDel.

            END. /* IF vOk THEN ... ELSE */

            FIND FIRST bLAcct WHERE bLAcct.acct      EQ bAcct.acct    
                                AND bLAcct.currency  EQ bAcct.currency
                                AND (bLAcct.contract  NE bDpsLoan.contract  OR
                                     bLAcct.cont-code NE bDpsLoan.cont-code)
                              NO-LOCK NO-ERROR.

            IF    bAcct.open-date NE bDpsLoan.open-date /* ��� ����� �� ���� ������, ��⠢�塞 */
               OR bAcct.cust-cat  NE bDpsLoan.cust-cat  /* ��� �� ����� �� �������� ������    */
               OR bAcct.cust-id   NE bDpsLoan.cust-id
               OR AVAIL(bLAcct)                         /* ��� �ਭ������� ��㣮�� ��������     */
            THEN
               NEXT lAcctLoop. 

            IF LAST-OF(bLoanAcct.acct) THEN
            DO:
               /*=== ����塞 �� ���㬥��� �� ���� ===*/
               RUN DelAllOpEntry(bAcct.acct, OUTPUT vOk).
               IF vOk NE YES THEN
                  UNDO lMainDel, LEAVE lMainDel.

               vMsg = bAcct.acct.

               /*=== ����塞 ��� ===*/
               RUN DelAcct(ROWID(bAcct), OUTPUT vOk).
               IF vOk THEN
               DO:
                  vMsg = SUBST("- ������ ��� &1.", vMsg).
                  RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

               END. /* IF vOk THEN */
               ELSE
               DO:
                  vMsg = SUBST("�訡��: �� 㤠���� 㤠���� ��� &1.", vMsg).
                  RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
                  UNDO lMainDel, LEAVE lMainDel.
               
               END. /* IF vOk THEN ... ELSE */

            END. /* IF LAST-OF(bLoanAcct.acct) THEN */
         END. /* IF LAST-OF(bLoanAcct.acct-type) THEN */
      END. /* FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract */

      /*=== ����塞 �易�� ��ꥪ�� ===*/
      RUN DelCustRole(bDpsLoan.contract, bDpsLoan.cont-code, OUTPUT vOk).
      IF vOk NE YES THEN
         UNDO lMainDel, LEAVE lMainDel.

      IF {assigned vLoanDV} THEN
      DO:
         FOR FIRST bDVLoan WHERE bDVLoan.contract  EQ bDpsLoan.contract
                             AND bDVLoan.cont-code EQ vLoanDV
                             AND bDVLoan.open-date EQ bDpsLoan.open-date
                             AND bDVLoan.end-date  EQ ?
                           NO-LOCK:
            /* ����� �� ����� � �� �� ����, �� � ���� */

            FIND FIRST bLoanAcct WHERE bLoanAcct.contract  EQ bDVLoan.contract
                                   AND bLoanAcct.cont-code EQ bDVLoan.cont-code
                                   AND bLoanAcct.since     NE bDVLoan.open-date
                                 NO-LOCK NO-ERROR.
           
            IF AVAIL(bLoanAcct) THEN
            DO:
               /* ���� �� ஫� ��⮢, �ਢ易��� �� � ���� ������ ������� */
               vMsg = SUBST("������� �� &1 �ய�饭, &2.",
                            bDVLoan.cont-code,
                            "���� ஫� � ��⮩ �ਢ離� �⫨筮� �� ���� ������ �������").
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            END. /* IF AVAIL(bLoanAcct) THEN */

            IF FndOp(bDVLoan.contract, bDVLoan.cont-code) EQ NO THEN
            DO:
               vMsg = SUBST("��ࠡ��뢠�� �易��� ������� �� &1.",
                            bDVLoan.cont-code).
               RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

               RUN DelDps IN THIS-PROCEDURE(bDVLoan.contract,
                                            bDVLoan.cont-code,
                                            YES,
                                            OUTPUT mOk).
               IF mOk NE YES THEN
               DO:
                  vMsg = SUBST("�訡��: �易��� ������� �� &1 �� 㤠���.",
                               bDVLoan.cont-code).
                  RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
                  UNDO lMainDel, LEAVE lMainDel.

               END. /* IF mOk NE YES THEN */

            END. /* IF FndOp(bDVLoan.contract, bDVLoan.cont-code) EQ NO THEN */
            ELSE 
            DO:
               /* ���� �஢���� �� ������ �� */
               vMsg = SUBST("������� �� &1 �ய�饭, &2.",
                            bDVLoan.cont-code,
                            "���� �� ���㫨஢���� �஢����").
               RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).

            END. /* IF FndOp(...) EQ NO THEN ... ELSE */
         END. /* FOR FIRST bDVLoan WHERE bDVLoan.contract  EQ bDpsLoan.contract */
      END. /* IF {assigned vLoanDV} THEN */

      /* ����塞 ������� */
      DELETE bDpsLoan.
      ASSIGN 
         oOk  = YES
         vMsg = SUBST("������� &1 㤠���.",
                      iCode)
      .
      RUN SetMsg IN THIS-PROCEDURE(0, vMsg).

   END. /* ELSE IF AVAIL(bDpsLoan) THEN */

END. /* lMainDel: DO TRANSACTION ON ERROR UNDO, THROW: */
             
/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "�訡��: �� �������� 㤠���� ������� ������.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelDps */

/*================================================================================================*/

/*================================================================================================*/
/*=== �뢮� ᮮ�饭�� ============================================================================*/
PROCEDURE SetMsg PRIVATE:
   DEF INPUT PARAM iType  AS INT64 NO-UNDO.
   DEF INPUT PARAM iTxt   AS CHAR  NO-UNDO.

   PUT STREAM sExp UNFORM SUBST("&1: &2", STRING(iType, "->9"), iTxt) SKIP.

   IF auto EQ NO THEN
   DO:
      /* ����� �믮���� ���짮��⥫�� */
      RUN Fill-SysMes IN h_tmess("","",STRING(iType), iTxt).

   END.
END PROCEDURE. /* SetMsg */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����塞 ஫� �� ���� ======================================================================*/
PROCEDURE DelLoanAcct:
   DEF INPUT  PARAM iRowId AS ROWID NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bLoanAcct  FOR  loan-acct.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FIND FIRST bLoanAcct WHERE ROWID(bLoanAcct) EQ iRowId
                        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF LOCKED(bLoanAcct) THEN
   DO:
      vMsg = SUBST("���� &1 ��� &2 �������஢���.",
                   bLoanAcct.acct-type,
                   bLoanAcct.acct).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   
   END. /* IF LOCKED(bLoanAcct) THEN */
   ELSE IF AVAIL(bLoanAcct) THEN
   DO:
      /* ����塞 ஫� */
      DELETE bLoanAcct.
      oOk = YES.

   END. /* ELSE IF AVAIL(bLoanAcct) THEN */

   IF oOk = NO THEN
      UNDO, LEAVE.

END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "�訡��: �� �������� 㤠���� ஫� ���.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelLoanAcct */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����塞 ���㬥�� ===========================================================================*/
PROCEDURE DelOp:
   DEF INPUT  PARAM iOp  AS INT64 NO-UNDO.
   DEF OUTPUT PARAM oOk  AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bOp     FOR op.
DEF BUFFER bOpEn   FOR op-entry.
DEF BUFFER bSign   FOR signs.
DEF BUFFER bOpBank FOR op-bank.
DEF BUFFER bOpImp  FOR op-impexp.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FIND FIRST bOp WHERE bOp.op EQ iOp 
                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
           
   IF LOCKED(bOp) THEN
   DO:
      vMsg = SUBST("���㬥�� &1 (op.op) �������஢��.",
                   STRING(bOp.op)).
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO, LEAVE.
   
   END. /* IF LOCKED(bOpDel) THEN */
   ELSE IF AVAIL(bOp) THEN
   DO:
      FOR EACH bOpEn OF bOp EXCLUSIVE-LOCK
                            ON ERROR UNDO, THROW:
         DELETE bOpEn.
      END.

      FOR EACH bOpBank OF bOp EXCLUSIVE-LOCK
                              ON ERROR UNDO, THROW:
         DELETE bOpBank.
      END.

      FOR EACH bOpImp OF bOp EXCLUSIVE-LOCK
                             ON ERROR UNDO, THROW:
         DELETE bOpImp.
      END.

      FOR EACH bSign WHERE bSign.file-name EQ "op"
                       AND bSign.surrogate EQ STRING(iOp)
                     EXCLUSIVE-LOCK
                     ON ERROR UNDO, THROW:
         DELETE bSign.
      END.

      DELETE bOp.
      oOk = YES.
   END. /* ELSE IF AVAIL(bOp) THEN */
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "�訡��: �� �������� 㤠���� ���㬥�� �� ����.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelOp */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����塞 �� ���㬥��� �� ���� =============================================================*/
PROCEDURE DelAllOpEntry:
   DEF INPUT  PARAM iAcct  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.
DEF VAR vOk   AS  LOG   NO-UNDO.

DEF BUFFER bOp       FOR op.
DEF BUFFER bOpEntry  FOR op-entry.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ iAcct
                        OR bOpEntry.acct-cr EQ iAcct
                     NO-LOCK,
      FIRST bOp WHERE bOp.op EQ bOpEntry.op
                     BREAK BY bOpEntry.op
                     ON ERROR UNDO, THROW:

      IF bOp.op-date NE ? THEN
      DO:
         /* ������ �� ���㫨஢���� ���㬥�� */
         vMsg = SUBST("�� ���� &1 ������ �� ���㫨஢���� ���㬥�� �� &2 ����� &3(op.op) � ����� '&4'.",
                      DelFilFromAcct(iAcct),
                      STRING(bOp.op-date, "99/99/9999"),
                      STRING(bOp.op),
                      bOp.op-status).
         RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
         UNDO, LEAVE.
      END. /* bOp.op-date */

      IF LAST-OF(bOpEntry.op) THEN
      DO:

         /*=== ����塞 ���㬥�� ===*/
         vMsg = SUBST("�訡��: �� 㤠���� 㤠���� ���㬥�� &1 (op.op).",
                       STRING(bOpEntry.op)).
         RUN DelOp(bOpEntry.op, OUTPUT vOk).

         IF vOk NE YES THEN
         DO:
            RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
            UNDO, LEAVE.

         END. /*IF vOk NE YES THEN*/
      END. /* IF LAST-OF(bOpEntry.op) THEN */
   END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ bAcct.acct */

   oOk = YES.

END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = SUBST("�訡��: �� �������� 㤠���� �஢���� �� ���� &1.",
                   DelFilFromAcct(iAcct)).

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelOp */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����塞 ��� ===============================================================================*/
PROCEDURE DelAcct:
   DEF INPUT  PARAM iRowId AS ROWID NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bAcct   FOR acct.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   /* �饬 ������᪨� ��� */
   FIND FIRST bAcct WHERE ROWID(bAcct) EQ iRowId
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   
   IF LOCKED(bAcct) THEN
   DO:
      vMsg = SUBST("�訡��: ��� �������஢�� ��㣨� ���짮��⥫��.").
      RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
      UNDO, LEAVE.
   END.
   ELSE IF AVAIL(bAcct) THEN
   DO:
      RUN acct-del.p (RECID(bAcct)).

      IF {&RETURN_VALUE} EQ "�����஢��" THEN
         UNDO, LEAVE.
      ELSE
      DO:
         /* ����塞 ��� */
         DELETE bAcct.
         oOk = YES.
      END. /* IF RETURN-VALUE EQ "�����஢��" THEN ... ELSE */
   END. /* ELSE IF AVAIL(bAcct) THEN */
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "�訡��: �� �������� 㤠���� ���.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelAcct */

/*================================================================================================*/

/*================================================================================================*/
/*=== ����塞 �易���� ��ꥪ⮢ ================================================================*/
PROCEDURE DelCustRole:
   DEF INPUT  PARAM iContr AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCode  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oOk    AS LOG   NO-UNDO.

DEF VAR vMsg  AS  CHAR  NO-UNDO.

DEF BUFFER bRole   FOR cust-role.

oOk = NO.

DO TRANSACTION ON ERROR UNDO, THROW:

   FOR EACH bRole WHERE bRole.file-name EQ "loan"
                    AND bRole.surrogate EQ SUBST("&1,&2", iContr, iCode)
                  EXCLUSIVE-LOCK:

      vMsg = SUBST("������ &1: &2.",
                   bRole.class-code,
                   bRole.cust-name).

      DELETE bRole.
      RUN SetMsg IN THIS-PROCEDURE(0, vMsg).
   END. /* ELSE IF AVAIL(bAcct) THEN */
   oOk = YES.
END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

/*=== ���墠�뢠�� �訡�� ===*/
CATCH vErr AS Progress.Lang.Error:

   ASSIGN
      vMsg = vErr:getMessage(1)
      oOk  = NO
   .

   IF NOT {assigned vMsg} THEN 
      vMsg = "�訡��: �� �������� 㤠���� �易��� ��ꥪ�.".

   RUN SetMsg IN THIS-PROCEDURE(-1, vMsg).
   DELETE OBJECT vErr NO-ERROR.

END CATCH. /* CATCH vErr AS Progress.Lang.Error: */

END PROCEDURE. /* DelAcct */

/*================================================================================================*/

/* --- a-deldps.pro was humbly modified by (c)blodd converter v.1.09 on 7/27/2015 8:10am --- */
