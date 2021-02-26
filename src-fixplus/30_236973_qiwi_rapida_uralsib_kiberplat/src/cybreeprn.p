/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cybreeprn.p
      Comment: TT:0236973 ������. �ਥ� ���⥦�� QIWI, ������,�ࠫᨡ, ���������
   Parameters: seance.seance-id
         Uses:
      Used by:
      Created: 22.03.2014 KMBIS TT:0236973 ������.�ਥ� ���⥦�� QIWI, ������, �ࠫᨡ, ���������
                                ������ ���㬥�⮢ �� ३ᠬ �����
                                ����� ����ᮢ � ���㬥�⮢ ��� ���⢥ত����
     Modified: 
*/
DEF INPUT  PARAM iSeanceId  AS  INT64  NO-UNDO.
{globals.i} 

{intrface.get exch}
{intrface.get strng}
{intrface.get xclass}

DEF VAR mTrAcct  AS  CHAR  NO-UNDO. /* �࠭���� ��� ᯨᠭ��                */
DEF VAR mStatErr AS  CHAR  NO-UNDO. /* ����� ��� ���㬥�⮢ ��� ���⢥ত���� */
DEF VAR mOpRef   AS  CHAR  NO-UNDO. /* ���७� ���㬥�� � ���������          */
DEF VAR mTmpDate AS  DATE  NO-UNDO. 

DEF BUFFER bPackObj FOR PackObject.
DEF BUFFER bPackOnl FOR PackObject.
DEF BUFFER bPack    FOR Packet.
DEF BUFFER bRef     FOR Reference.
DEF BUFFER bOp      FOR Op.
DEF BUFFER bOpEntry FOR Op-entry.

ASSIGN
   mTrAcct  = GetXAttrValueEx("code", "��������,���������", "���瑯��","")
   mStatErr = GetXAttrValueEx("code", "��������,���������", "��⁥�����","")
   beg-date = gend-date
   end-date = beg-date
.

{getdates.i &noinit     = YES
            &TitleLabel = "����஫� �����⢥ত����� ���㬥�⮢"
            &BegLabel   = "��砫� ��ਮ��"
            &EndLabel   = "����� ��ਮ��"
            &AddPostUpd = "IF (beg-date EQ ?) OR (end-date EQ ?) THEN 
                           DO: undo, retry. END."
}

{setdest.i}

PUT UNFORM "������ ���⥦��" SKIP(1).

FOR EACH Packet WHERE Packet.SeanceId EQ iSeanceId
                  AND Packet.ParentId EQ 0
                NO-LOCK,
   FIRST FileExch WHERE FileExch.FileExchId EQ Packet.FileExchid
                  NO-LOCK:

   PUT UNFORM SUBST("����: &1", FileExch.Name) SKIP (1).
   PUT UNFORM "���㬥���, ᮧ����� �� ॥���� ���⥦�� (����⢮���� � ����):" SKIP(1).
   PUT UNFORM 
      "��� ���."       FORMAT "x(10)"                                        " " 
      "����� ���."      FORMAT "x(10)"                                        " "
      "�� ���������"    FORMAT "x(15)"                                        " "
      "��� �����⥫�" FORMAT "x(20)"                                        " "
      "     �㬬�"      FORMAT "x(10)"                                        " "
      "����� ���"      FORMAT "x(10)"                                        " "
   SKIP.

   /* �������� ���㬥��� �� ॥����, �� �뫨 ᮧ���� ������ ����ᮬ */
   FOR EACH bPack WHERE bPack.ParentId EQ Packet.PacketId
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId  EQ bPack.PacketId
                       AND bPackObj.file-name EQ "op-entry"
                       AND bPackObj.Kind      NE "ICyberComm"
                     NO-LOCK,
      FIRST bOp WHERE bOp.op        EQ INT64(ENTRY(1, bPackObj.Surrogate))
                NO-LOCK,
      FIRST bRef WHERE bRef.PacketID   EQ bPack.PacketId
                   AND bRef.class-code EQ "RCyber"
                 NO-LOCK:

      FIND FIRST bPackOnl WHERE bPackOnl.file-name    EQ "op-entry"
                            AND bPackOnl.surrogate    EQ bPackObj.file-name
                            AND bPackOnl.Kind         EQ "ICyber"
/*                            AND bPackOnl.PackObjectID LE bPackObj.PackObjectID */
                          NO-LOCK NO-ERROR.

      IF NOT AVAIL(bPackOnl) THEN
      DO:

         /* �� ��諨 ������ ������ */
         FIND FIRST bOpEntry OF bOp NO-LOCK NO-ERROR.
         PUT UNFORM 
            STRING(bOp.op-date, "99/99/9999")                                  FORMAT "x(10)" " " 
            bOp.doc-num                                                        FORMAT "x(10)" " "
            GetEntries(2, bRef.RefValue, "|", "")                              FORMAT "x(15)" " "
            (IF {assigned bOp.ben-acct} THEN DelFilFromAcct(bOp.ben-acct)      
                                        ELSE DelFilFromAcct(bOpEntry.acct-cr)) FORMAT "x(20)" " "
            STRING(bOpEntry.amt-rub, ">>>>>>9.99")                             FORMAT "x(10)" " "
            PADC(bOp.op-status, 10)
         SKIP.

         RUN FillErrorTable IN h_exch(bPack.ClassError,
                                      bPack.PackError,
                                      OUTPUT TABLE ttError).
         FOR EACH ttError:
            PUT UNFORM 
               SUBST("     &1", ttError.Name)
            SKIP.
         END. /* FOR EACH ttError: */

         IF {assigned bPack.PackError} THEN 
            PUT UNFORM SKIP(1).
         {empty ttError}
      END. /* IF NOT AVAIL(bPackOnl) THEN */
   END. /* FOR EACH bPack WHERE bPack.ParentId EQ Packet.PacketId */

END.

IF {assigned mTrAcct} THEN
DO: 
   {find-act.i &acct = mTrAcct}
   IF AVAIL(acct) THEN
   DO:
      PUT UNFORM SKIP(1)
                 SUBST("������ �����⢥ত����� ���㬥�⮢ �� ���� &1", acct.number)
                 SKIP(1).
      PUT UNFORM 
         "��� ���."       FORMAT "x(10)"                                        " " 
         "����� ���."      FORMAT "x(10)"                                        " "
         "�� ���������"    FORMAT "x(15)"                                        " "
         "��� �����⥫�" FORMAT "x(20)"                                        " "
         "     �㬬�"      FORMAT "x(10)"                                        " "
         "����� ���"      FORMAT "x(10)"                                        " "
      SKIP.
      DO mTmpDate = beg-date TO end-date:

         FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ acct.acct
                             AND bOpEntry.op-date   EQ mTmpDate
                             AND bOpEntry.op-status GE "�"
                           NO-LOCK:
             FIND FIRST bPackObj WHERE bPackObj.file-name EQ "op-entry" 
                                   AND bPackObj.surrogate EQ SUBST("&1,&2", 
                                                                   STRING(bOpEntry.op), 
                                                                   STRING(bOpEntry.op-entry))
                                   AND bPackObj.Kind      EQ "ICyberRee"
                                 NO-LOCK NO-ERROR.

            IF NOT AVAIL(bPackObj) THEN 
            DO:
               FIND FIRST bOp WHERE bOp.op EQ bOpEntry.op
                              NO-LOCK NO-ERROR.
               RUN chst-op.p(RECID(bOp), mStatErr) NO-ERROR.
               mOpRef = "".
               lRefFnd:
               FOR EACH bPackObj WHERE bPackObj.file-name EQ "op-entry"
                                   AND bPackObj.Surrogate BEGINS SUBST("&1,", STRING(bOp.op))
                                 NO-LOCK,
                  FIRST bRef WHERE bRef.PacketID   EQ bPackObj.PacketId
                               AND bRef.class-code EQ "RCyber"
                             NO-LOCK:
                  mOpRef = GetEntries(2, bRef.RefValue, "|", "").
                  LEAVE lRefFnd.
               END. /* FOR EACH bPackObj WHERE bPackObj.file-name EQ "op-entry" */

               PUT UNFORM 
                  STRING(bOp.op-date, "99/99/9999")                                  FORMAT "x(10)" " " 
                  bOp.doc-num                                                        FORMAT "x(10)" " " 
                  mOpRef                                                             FORMAT "x(15)" " " 
                  (IF {assigned bOp.ben-acct} THEN DelFilFromAcct(bOp.ben-acct)                         
                                              ELSE DelFilFromAcct(bOpEntry.acct-cr)) FORMAT "x(20)" " " 
                  STRING(bOpEntry.amt-rub, ">>>>>>9.99")                             FORMAT "x(10)" " " 
                  PADC(bOp.op-status, 10)
               SKIP.
            END. /* IF NOT AVAIL(bPackObj) THEN  */
         END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ acct.acct */
      END. /* DO mTmpDate = beg-date TO end-date: */
   END. /* IF AVAIL(acct) THEN */
END. /* IF {assigned mTrAcct} THEN */

{preview.i}