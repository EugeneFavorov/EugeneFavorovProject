{globals.i}

/* +++ comgroup.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: comgroup.p
      Comment: TT:0220967 ��. ��⮬���᪮� ᯨᠭ�� �����ᨩ �� ���
   Parameters: BUFFER op, iParam, oResult
         Uses:
      Used by:
      Created: 24/10/2014 11:43 KMBIS ��⮬���᪮� ᯨᠭ�� �����ᨩ �� ���
     Modified: 
*/
DEF        PARAM BUFFER op      FOR op.
DEF INPUT  PARAM        iParam  AS  CHAR NO-UNDO.
DEF OUTPUT PARAM        oResult AS  LOG  NO-UNDO INIT YES.

{globals.i}
{g-trans.equ}

{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get pbase}

DEF VAR mOpKind   AS  CHAR  NO-UNDO. /* �࠭����� ᮧ����� ���.���㬥�� */
DEF VAR mRunKind  AS  CHAR  NO-UNDO. /* �஬��ઠ �� ����� �࠭���樨    */
DEF VAR mTmpStr   AS  CHAR  NO-UNDO. 
DEF VAR mMaskDb   AS  CHAR  NO-UNDO. /* ��᪠ ��⮢ �� ������            */
DEF VAR mMaskCr   AS  CHAR  NO-UNDO. /* ��᪠ ��⮢ �� �।���           */
DEF VAR mOpInfo   AS  CHAR  NO-UNDO. /* ����� ���㬥��                  */
DEF VAR mAmtInfo  AS  CHAR  NO-UNDO. /* ����� ���㬥��                  */
DEF VAR mResult   AS  CHAR  NO-UNDO. /* ������� ࠡ��� ���.�࠭���樨   */

DEF BUFFER bAcctDb FOR acct.
DEF BUFFER bAcctCr FOR acct.
DEF BUFFER bxlink  FOR xlink.
DEF BUFFER blinks  FOR links.
DEF BUFFER bop     FOR op.
DEF BUFFER bHist   FOR history.

ASSIGN
   mMaskDb = GetEntries(2, iParam, ";", "")
   mMaskCr = GetEntries(3, iParam, ";", "")
   mOpKind = GetXattrValueEx("op-kind", 
                             op.op-kind, 
                             "ComOpKind", 
                             GetEntries(1, iParam, ";", ""))
NO-ERROR.

/*=== �஢��塞 �࠭����� ��஦����� �����ᨨ ===*/
IF NOT {assigned mOpKind} THEN
DO:
   ASSIGN 
      oResult = NO
      mTmpStr = "�� ������ �࠭����� ���᫥��� �����ᨨ"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mOpKind} THEN */
ELSE
DO:
   FIND FIRST op-kind WHERE op-kind.op-kind EQ mOpKind
                      NO-LOCK NO-ERROR NO-WAIT.

   IF NOT AVAIL(op-kind) THEN 
   DO:
      ASSIGN
         oResult = NO
         mTmpStr = "�� ������� � ����"
         mTmpStr = "�������஢��� ��㣨� ���짮��⥫��" WHEN LOCKED(op-kind)
         mTmpStr = SUBST("�࠭����� ���᫥��� &1", mTmpStr)
      .

      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
  
   END. /* IF NOT AVAIL(op-kind) THEN  */

END. /* IF NOT {assigned mOpKind} THEN ... ELSE */

/*=== ��᪠ ��⮢ ����� ===*/
IF NOT {assigned mMaskDb} THEN
DO:

   ASSIGN
      oResult = NO
      mTmpStr = "�� ������ ��᪠ ��⮢ �� ������"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mMaskDb} THEN */

/*=== ��᪠ ��⮢ �।�� ===*/
IF NOT {assigned mMaskCr} THEN
DO:

   ASSIGN
      oResult = NO
      mTmpStr = "�� ������ ��᪠ ��⮢ �� �।���"
   .
   RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

END. /* IF NOT {assigned mMaskCr} THEN */


lMain:
DO TRANSACTION ON ERROR UNDO lMain, LEAVE lMain
               ON QUIT  UNDO lMain, LEAVE lMain:

   IF oResult EQ NO THEN 
      LEAVE lMain.

   /*=== ���� �஢�ப �஢���� ===*/
   FIND op-entry OF op NO-LOCK NO-ERROR.

   /* ������஢����� ���㬥�� */
   IF AMBIGUOUS op-entry THEN  
      LEAVE lMain.

   /* ��� �஢���� � ���㬥�� */
   IF NOT AVAIL(op-entry) THEN 
      LEAVE lMain.


   /* ���⪨ �� ����஢���� */
   IF    NOT {assigned op-entry.acct-db} 
      OR NOT {assigned op-entry.acct-cr} 
   THEN
      LEAVE lMain.

   {find-act.i &acct = op-entry.acct-db 
               &bact = bAcctDb}

   {find-act.i &acct = op-entry.acct-cr 
               &bact = bAcctCr}

   /* �� ��諨 ��� � ���� */
   IF    NOT AVAIL(bAcctCr) 
      OR NOT AVAIL(bAcctDb)
   THEN
   DO:
      oResult = NO.

      IF NOT AVAIL(bAcctDb) THEN
      DO:
         mTmpStr = SUBST("��� '&1' �� ������ �� ������ � ����",
                         op-entry.acct-db).
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
      END.

      IF NOT AVAIL(bAcctCr) THEN
      DO:
         mTmpStr = SUBST("��� '&1' �� �।��� �� ������ � ����",
                         op-entry.acct-cr).
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
      END.

      LEAVE lMain.

   END. /* IF    NOT AVAIL(bAcctDb)  */

   /* ��� �� ����諨 ��� ���� */
   IF    NOT CAN-DO(mMaskDb, op-entry.acct-db) 
      OR NOT CAN-DO(mMaskCr, op-entry.acct-cr) 
   THEN
      LEAVE lMain.

   /* ��� � ࠧ��� ������ */
   IF bAcctDb.currency NE bAcctCr.currency THEN
      LEAVE lMain.

   IF {assigned op-entry.currency} THEN 
      mAmtInfo = SUBST("&1 (&2)",
                       STRING(op-entry.amt-cur),
                       op-entry.currency).
   ELSE
      mAmtInfo = SUBST("&1 (&2)",
                       STRING(op-entry.amt-rub),
                       FGetSetting("�����悠�", "", "")).

   mOpInfo = SUBST("����� '&1' �� &2 �㬬� &3",
                   op.doc-num,
                   STRING(op.op-date,"99/99/9999"),
                   mAmtInfo).


   /* �஢��塞 ��� � �����ᨮ��� ���㬥�⮬ */
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
         mTmpStr = SUBST("���� � �����ᨮ��� ���㬥�⮬ �������஢��� (���㬥�� &2)",
                         mOpInfo)
         oResult = NO
      .
      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

   END.
   ELSE IF AVAIL(blinks) THEN
   DO:

      /* �஢��塞 �易��� �����ᨮ��� ���㬥�� */
      FIND FIRST bop WHERE bop.op EQ INT64(blinks.target-id)
                     EXCLUSIVE-LOCk NO-WAIT NO-ERROR.

      IF     NOT AVAIL(bop) 
         AND LOCKED(bop)  
      THEN
      DO:
         /* �����ᨮ��� ���㬥�� �������஢�� */
         ASSIGN
            mTmpStr = SUBST("�����ᨮ��� ���㬥�� �������஢�� " +
                            "(��� ���㬥�� &1)",
                            mOpInfo)
            oResult = NO
         .
         RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).

      END.
      ELSE    IF NOT AVAIL(bop) 
           OR bop.op-date EQ ? 
      THEN
      DO:
         /* ���� ����, �� ���㬥�� ��� */
         /* �����ᨮ��� ���㫨஢��     */
         DELETE blinks.
         RELEASE blinks.

      END.
      ELSE 
      DO:
         /* ������� ��������� ��� � �����ᨮ��� ���㬥�⮬ */
         mTmpStr = SUBST("%s=&1%s=&2%s=&3",
                         op.doc-num,
                         STRING(op.op-date,"99/99/9999"),
                         mAmtInfo).
         PICK-VALUE = "NO".
         RUN Fill-SysMes IN h_tmess ("", "commgr01", "4", mTmpStr).

         /*=== ������ �롮� ===*/
         CASE PICK-VALUE:
        
            WHEN "YES" THEN /*=== ���ᮧ���� ��� ======*/ 
            DO:
               
               RUN dpsopb.p(RECID(bop), 2, NO) NO-ERROR.

               IF {&RETURN_VALUE} GT "" OR 
                  {&RETURN_VALUE} EQ ? 
               THEN 
               DO:
                   /* �� ᬮ��� ���㫨஢��� */
                  oResult = NO.
                  LEAVE lMain.
               END.

               DELETE  blinks.
               RELEASE blinks.
               RELEASE bop.
            END.
        
            WHEN "NO" THEN /*=== ��訫� ��⠢��� ��� ===*/
            DO:
               
               oResult = YES.
               LEAVE lMain.

            END.
        
            OTHERWISE      /*=== ������ ESC ==============*/
            DO:
               
               oResult = NO.
               LEAVE lMain.

            END.
        
         END CASE. /* STRING(PICK-VALUE, "YES/NO"): */

      END.

   END. /* ELSE IF AVAIL(blinks) THEN */

   ASSIGN
      oResult  = NO
      mRunKind = GetBaseOpKind()
   .
   IF NOT {assigned mRunKind} THEN
      RUN InitBaseLibrary IN h_pbase (?, gend-date, ?).

   /* ��।��� op.op �᭮����� ���㬥�� */
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "_OpMain", STRING(op.op)).

   FOR FIRST bHist WHERE bHist.file-name  EQ "op"
                     AND bHist.field-ref  EQ STRING(op.op)
                     AND bHist.modify     NE "RUN"
                     AND bHist.modify     NE "PRINT"
                     AND bHist.modify     NE "SAVE"
                   NO-LOCK:

      /* �६� ᮧ����� ���㬥�� */
      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                                      "_OpTime", 
                                      STRING(bHist.modif-time, "HH:MM:SS")).

      /* ��� ᮧ����� ���㬥�� */
      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                                      "_OpDate", 
                                      STRING(bHist.modif-date, "99.99.99")).

   END. /* FOR LAST bHist WHERE bHist.file-name  EQ "op" */
 
   /* ��뢠�� ᮧ����� �����ᨮ����� ���㬥�� */
   RUN RunTransaction IN h_pbase (mOpKind) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
   DO:
      /* �ந��諠 �訡�� �믮��ﭨ� �࠭���樨 ���᫥��� �����ᨨ */
      ASSIGN
         mTmpStr = SUBST("�訡�� �� �믮������ �࠭���樨: &1",
                         mOpKind)
      .
      RUN Fill-SysMes IN h_tmess("", "", "-1", mTmpStr).
   END.
   ELSE 
      oResult = YES.

   IF oResult EQ YES THEN
   DO:

      /* �������� ����������� ������ �����頥���� ���祭�� */
      ASSIGN
         mResult = GetSysConf("_ResultCom")
         oResult = {assigned mResult }
      .

   END.

END. /* lMain: */

{intrface.del}

RETURN.

/*=== ���� 䨭�����樨 =======================================================*/

FINALLY:

   /* ���⨬ �� ᮡ�� ���� */
   RUN DelTransAttr IN h_trans ("", 0, "_OpMain")     NO-ERROR.
   RUN DeleteOldDataProtocol IN h_base ("_ResultCom") NO-ERROR.

   RELEASE bop NO-ERROR.

END FINALLY.

/* --- comgroup.p was humbly modified by (c)blodd converter v.1.09 on 4/6/2015 7:27am --- */
