/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: i-cyberpl.p
      Comment: TT:0236973 ������. �ਥ� ���⥦�� QIWI, ������,�ࠫᨡ, ���������
   Parameters: �����⥫� �� ��ࠬ���� �����, OUTPUT �⢥� ��� ���������
         Uses:
      Used by:
      Created: 22.03.2014 KMBIS TT:0236973 ������.�ਥ� ���⥦�� QIWI, ������, �ࠫᨡ, ���������
                                ������ ������ ����� � �����⮢�� ������ ��� �����
     Modified: 
*/
DEF INPUT  PARAM iInstance  AS  HANDLE  NO-UNDO.
DEF OUTPUT PARAM oAns       AS  CHAR    NO-UNDO.
{globals.i} 

{intrface.get pays}

{intrface.get pack}
{intrface.get pbase}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}

DEF VAR mReq       AS  HANDLE  NO-UNDO.                             
DEF VAR mChkStat   AS  CHAR    NO-UNDO.  /* �ᯥ譮 ��ନ஢��� ��ࠬ���� ����� */
DEF VAR mSys       AS  CHAR    NO-UNDO.  /* ��� ��⥬� ������                     */
DEF VAR mTypeOp    AS  CHAR    NO-UNDO.  /* ��� ���⥦�                            */
DEF VAR mType      AS  CHAR    NO-UNDO.  /* ��� �����                            */
DEF VAR mAcct      AS  CHAR    NO-UNDO.  /* ��� �����⥫�                        */
DEF VAR mBic       AS  CHAR    NO-UNDO.  /* ��� ����� �����⥫�                   */
DEF VAR mAcctMask  AS  CHAR    NO-UNDO.  /* ��᪠ ��⮢ �����⥫��               */
DEF VAR mBicMask   AS  CHAR    NO-UNDO.  /* ��᪠ ��� ����� �����⥫�             */
DEF VAR mRef       AS  CHAR    NO-UNDO.  /* �������� ����� ���⥦�               */
DEF VAR mFilialId  AS  CHAR    NO-UNDO.  /* ��� ���ࠧ������� �����⥫� ���⥦�   */
DEF VAR mCorr      AS  CHAR    NO-UNDO.  /* ����.��� ����� �����⥫� ���⥦�     */
DEF VAR mAcctCr    AS  CHAR    NO-UNDO.  /* ��� �� �।��� ��� ���䨫�            */
DEF VAR mOpKind    AS  CHAR    NO-UNDO.  /* �࠭����� ���᫥���                  */
DEF VAR mSign      AS  CHAR    NO-UNDO.  /* ������� �����                        */
DEF VAR mReqStr    AS  CHAR    NO-UNDO.  /* ������᪨� �����                      */
DEF VAR mKeyOpen   AS  CHAR    NO-UNDO.  /* ������ ����                          */
DEF VAR mKeyPriv   AS  CHAR    NO-UNDO.  /* ������� ����                          */
DEF VAR mAmt       AS  DEC     NO-UNDO.  /* �㬬� ���᫥����                      */
DEF VAR mDateOp    AS  CHAR    NO-UNDO.  /*                                        */
DEF VAR mOp        AS  INT64   NO-UNDO.  /*                                        */
DEF VAR mTmpStr    AS  CHAR    NO-UNDO.  /*                                        */
DEF VAR mChkSign   AS  LOG     NO-UNDO.  /*                                        */


DEF BUFFER bPack    FOR packet.
DEF BUFFER bPackObj FOR PackObject.
DEF BUFFER bRef     FOR reference.
DEF BUFFER bOp      FOR op.

DEF TEMP-TABLE ttParamReq NO-UNDO
   FIELD bank-code-rec      AS  CHAR
   FIELD amt-rub            AS  DEC
   FIELD bank-corr-acct-rec AS  CHAR
   FIELD acct-rec           AS  CHAR
   FIELD acct-cr            AS  CHAR
   FIELD SendREF            AS  CHAR
   FIELD SeanceId           AS  INT64
   FIELD OpId               AS  INT64
.

mReq = iInstance:DEFAULT-BUFFER-HANDLE.
mReq:BUFFER-CREATE().
ASSIGN
   mChkStat  = "0"
   mSys      = "���������"
   mAcctMask = GetXattrValueEx("code", SUBST("��������,&1", mSys), "��᪑�", "!*")
   mBicMask  = GetXAttrValueEx("code", SUBST("��������,&1", mSys), "���",    "!*")
   mOpKind   = GetXAttrValueEx("code", SUBST("��������,&1", mSys), "�����",  "")
   mKeyOpen  = GetXAttrValueEx("code", SUBST("��������,&1", mSys), "�����", "")
   mKeyPriv  = GetXAttrValueEx("code", SUBST("��������,&1", mSys), "������", "")
NO-ERROR.

IF NOT(    {assigned mOpKind} 
       AND {assigned mKeyOpen} 
       AND {assigned mKeyPriv}) 
THEN
DO:
   IF NOT {assigned mOpKind} THEN
      RUN dbgprint.p ("i-cyberpl.p", SUBST("�� �������� �� '�����' ��� ��⥬� ��ॢ���� '&1'",
                                           mSys)).

   IF NOT {assigned mKeyOpen} THEN
      RUN dbgprint.p ("i-cyberpl.p", SUBST("�� �������� �� '�����' ��� ��⥬� ��ॢ���� '&1'",
                                           mSys)).
   IF NOT {assigned mKeyPriv} THEN
      RUN dbgprint.p ("i-cyberpl.p", SUBST("�� �������� �� '������' ��� ��⥬� ��ॢ���� '&1'",
                                           mSys)).
   oAns = "".
   RETURN.
END. /* �஢�ઠ ���������� �� �� ��⥬� ��ॢ���� */

mType   = TRIM(mReq:BUFFER-FIELD("action"):BUFFER-VALUE)     NO-ERROR.
mAcct   = TRIM(mReq:BUFFER-FIELD("number"):BUFFER-VALUE)     NO-ERROR.
mBic    = TRIM(mReq:BUFFER-FIELD("additional"):BUFFER-VALUE) NO-ERROR.
mRef    = TRIM(mReq:BUFFER-FIELD("receipt"):BUFFER-VALUE)    NO-ERROR.
mTypeOp = TRIM(mReq:BUFFER-FIELD("type"):BUFFER-VALUE)       NO-ERROR.
mSign   = TRIM(mReq:BUFFER-FIELD("sign"):BUFFER-VALUE)       NO-ERROR.
mReqStr = TRIM(mReq:BUFFER-FIELD("OnlineReq"):BUFFER-VALUE)  NO-ERROR.


&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-cyberpl.p", SUBST("mAcctMask = &1; mBicMask = &2; mOpKind = &3", 
                                     mAcctMask, 
                                     mBicMask,
                                     mOpKind)).
RUN dbgprint.p ("i-cyberpl.p", SUBST("mKeyOpen: '&1'; mKeyPriv: '&2'", 
                                     mKeyOpen,
                                     mKeyPriv)).
&ENDIF

/* �஢��塞 ������� */
RUN CheckSign IN THIS-PROCEDURE(mReqStr,         /* �����                 */
                                mSign,           /* �������                */
                                mKeyOpen,        /* ���� � ����⮬� ����� */
                                OUTPUT mChkSign) /* ������� �஢�ન     */
                                NO-ERROR.
IF    NOT {assigned mSign}
   OR mChkSign EQ NO
THEN
DO:
   mChkStat = "-4".
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-cyberpl.p", "CheckSign = NO").
   &ENDIF
END. /* IF mOk EQ NO THEN */


/* �⮡ࠫ� ���⥦� ��।�������� ⨯� */
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-cyberpl.p", SUBST("mType = &1, MaskOk = &2", 
                                     mType, 
                                     STRING(CAN-DO("check,payment,status", mType)))).
&ENDIF
IF     mChkStat EQ "0" 
   AND NOT CAN-DO("check,payment,status", mType) 
THEN
   mChkStat = "1".

/* �஢�ઠ ���� type */
IF     mChkStat EQ "0"
   AND mTypeOp  NE "1"
   AND CAN-DO("check,payment", mType)
THEN
DO:
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-cyberpl.p", SUBST("mTypeOp = &1", mTypeOp)).
   &ENDIF
   mChkStat = "-3".
END.

/* ����稥 㭨���쭮�� ����� ��易⥫쭮 */
IF     (mChkStat EQ "0")
   AND CAN-DO("payment,status", mType) 
   AND NOT {assigned mRef}
THEN
   mChkStat = "4".
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-cyberpl.p", SUBST("mRef = &1", mRef)).
&ENDIF
   
IF CAN-DO("check,payment", mType) THEN
DO:
   /* �஢��塞 ���� ��⮢ �����⥫� */
   IF     (mChkStat EQ "0")
      AND NOT (CAN-DO(mAcctMask, mAcct) AND {assigned mAcct})
   THEN
      mChkStat = "12".
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-cyberpl.p", SUBST("mAcct = &1", mAcct)).
   &ENDIF
   
   /* �஢��塞 ��� ����� �����⥫� */
   IF     (mChkStat EQ "0")
      AND NOT(CAN-DO(mBicMask, mBic) AND {assigned mBic})
   THEN
      mChkStat = "-1".
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-cyberpl.p", SUBST("mBic = &1", mBic)).
   &ENDIF

   /* �஢��塞 ����⢮����� ��� � ����� */
   RELEASE acct.
   IF {assigned mBic} AND {assigned mAcct} THEN
      RUN CorrBranch IN h_pays(mBic,
                               "�������",
                               TODAY,
                               OUTPUT mFilialId,
                               OUTPUT mCorr,
                               OUTPUT mAcctCr).
   IF {assigned mFilialId} THEN
   DO:
      {find-act.i &filial = mFilialId
                  &acct   = mAcct
                  &curr   = "''"
                  &NoFindInNatCurr = YES}
   END.
  
   &IF DEFINED(IS-DEBUG) &THEN
   IF AVAIL(acct) THEN 
      mTmpStr = SUBST("acct = &1, Open-date = &2, Close-date = &3", 
                      acct.acct,
                      STRING(acct.open-date),
                      STRING(acct.close-date)).
   ELSE
      mTmpStr = SUBST("�� ������ ������᪨� ��� '&1'", mAcct).
   
   RUN dbgprint.p ("i-cyberpl.p", mTmpStr).
   RUN dbgprint.p ("i-cyberpl.p", SUBST("mFilialId = &1, mCorr = &2, mAcctCr = &3", 
                                        mFilialId,
                                        mCorr,
                                        mAcctCr)).
   &ENDIF
   IF     (mChkStat EQ "0")
      AND NOT(AVAIL(acct) AND acct.close-date EQ ? AND acct.open-date LE TODAY)
   THEN
      mChkStat = "2".
  
   /* �஢��塞 �㬬� ���᫥��� */
   mAmt = DEC(mReq:BUFFER-FIELD("amount"):BUFFER-VALUE) NO-ERROR.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("i-cyberpl.p", SUBST("mAmt GT 0 = &1", STRING(mAmt GT 0))).
   &ENDIF
   IF     mChkStat EQ "0"
     AND mAmt     LE 0
   THEN
     mChkStat = "3".
  
   /* ����������� ���᫥��� �� ��� �� ���������� */
   IF mChkStat EQ "0" THEN
   DO:
      mTmpStr = GetXAttrValueEx("acct", 
                                SUBST("&1,&2", acct.acct, acct.currency), 
                                "PriemKiberplat", 
                                "").
   
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("i-cyberpl.p", SUBST("PriemKiberplat = '&1'", 
                                           mTmpStr)).
      &ENDIF
      IF NOT CAN-DO("yes,��", mTmpStr) THEN
      DO:
         /* �� �� �������� ���஡㥬 ���᪠�� �।��� ������� */
         mChkStat = "13".
         lCrdFnd:
         FOR EACH loan-acct WHERE loan-acct.acct     EQ acct.acct
                              AND loan-acct.currency EQ acct.currency
                              AND loan-acct.contract EQ "�।��"
                            NO-LOCK,
            FIRST loan WHERE loan.contract  EQ loan-acct.contract
                         AND loan.cont-code EQ loan-acct.cont-code
                         AND loan.open-date LE TODAY
                         AND (loan.close-date EQ ? OR
                              loan.close-date GE TODAY)
                       NO-LOCK:
            /* ��諨 �/�, �������騩 �� ���� ���⥦�, �易��� � ��⮬ */
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("i-cyberpl.p", SUBST("Contract = &1, ContCode = &2", 
                                                 loan.contract,
                                                 loan.cont-code)).
            &ENDIF
            mChkStat = "0".
            LEAVE lCrdFnd.
     
         END. /* lCrdFnd: FOR EACH loan-acct WHERE loan-acct.acct     EQ acct.acct */
      END. /* IF NOT CAN-DO("yes,��", mTmpStr) THEN */
   END. /* IF mChkStat EQ "0" THEN */
END. /* IF CAN-DO("check,payment", mType) THEN */

/* �஢�ઠ ����⢮����� �������  */
FIND FIRST op-date WHERE op-date.op-date EQ TODAY
                   NO-LOCK NO-ERROR.

IF     mChkStat EQ "0" 
   AND CAN-DO("check,payment", mType)
   AND NOT AVAIL(op-date)
THEN
   mChkStat = "5".

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-cyberpl.p", SUBST("AVAIL(op-date) = &1", STRING(AVAIL(op-date)))).
RUN dbgprint.p ("i-cyberpl.p", SUBST("mChkStat = &1", mChkStat)).
&ENDIF

ASSIGN
  mOP     = ?
  mDateOp = ?
.

IF mChkStat EQ "0" THEN
DO:
   IF mType EQ "payment" THEN
   DO TRANSACTION ON ERROR UNDO, RETRY:

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("i-cyberpl.r", "action = payment").
      &ENDIF
      mChkStat = "14".

      FOR FIRST op-kind WHERE op-kind.op-kind EQ mOpKind
                        NO-LOCK:
      
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-cyberpl.p", "FIND op-kind = YES").
         &ENDIF

         CREATE ttParamReq.
         ASSIGN
            ttParamReq.bank-code-rec      = mBic
            ttParamReq.amt-rub            = mAmt
            ttParamReq.bank-corr-acct-rec = mCorr        WHEN {assigned mAcctCr}
            ttParamReq.acct-rec           = acct.acct
            ttParamReq.acct-rec           = acct.number  WHEN {assigned mAcctCr}
            ttParamReq.acct-cr            = mAcctCr      WHEN {assigned mAcctCr}
            ttParamReq.SendREF            = mRef
            ttParamReq.SeanceId           = 0
            ttParamReq.OpId               = 0
         .
         VALIDATE ttParamReq.

         /*=== ��।��� 㪠��⥫� �� ⠡���� ===*/
         RUN AddAttr2TableEx IN h_trans (op-kind.op-kind,
                                         0, ?, "", 0, 
                                         "CPlatHandle", 
                                         STRING(BUFFER ttParamReq:HANDLE)).
     
         RUN InitBaseLibrary IN h_pbase(op-kind.op-kind, TODAY, ?).
         RUN Init-SysMes IN h_tmess("", "", "").

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-cyberpl.p", SUBST("RunTransaction: '&1'", op-kind.op-kind)).
         &ENDIF

         /* ������ ���㬥�� */
         RUN RunTransaction IN h_pbase (op-kind.op-kind) NO-ERROR.

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-cyberpl.p", SUBST("ERROR-STATUS:ERROR = &1",
                                               STRING(ERROR-STATUS:ERROR))).
         RUN dbgprint.p ("i-cyberpl.p", SUBST("SeanceId = &1; op.op = &2", 
                                              STRING(ttParamReq.SeanceId),
                                              STRING(ttParamReq.OpId))).
         &ENDIF

         /* �஢��塞 १���� ������ */
         IF ttParamReq.OpId NE 0 THEN
         DO:
            lChkCreate:
            FOR FIRST bOp WHERE bOp.op EQ ttParamReq.OpId
                          NO-LOCK:

               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("i-cyberpl.p", SUBST("op.op-status = &1", bOp.op-status)).
               &ENDIF

               ASSIGN
                  mChkStat = "0"
                  mChkStat = "7"      WHEN  bOp.op-status BEGINS "�"
                  mOP      = bOp.op
                  mDateOp  = SUBST("&1-&2-&3T&4",
                                   STRING(YEAR(TODAY)),
                                   STRING(MONTH(TODAY), "99"),
                                   STRING(DAY  (TODAY), "99"),
                                   STRING(TIME, "HH:MM:SS"))
               .
               LEAVE lChkCreate.
            END. /* FOR FIRST bOp WHERE bOp.op EQ ttParamReq.OpId */
         END. /* IF ttParamReq.SeanceId NE 0 THEN */

         /*=== ���ࠥ� ���� �� ᮡ�� ===*/
         RUN ClearTransactionBuffer IN h_trans(op-kind.op-kind, NO).

         IF mChkStat NE "0" THEN
         DO:
            ASSIGN
              mOP     = ?
              mDateOp = ?
            .

         END. /* IF mChkStat NE "0" THEN */
      END. /* FOR FIRST op-kind WHERE op-kind.op-kind EQ "e-nds504" */
   END. /* IF mType EQ "payment" THEN */
   ELSE IF mType EQ "status" THEN
   DO:
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("i-cyberpl.r", "action = status").
      &ENDIF
      mChkStat = "6".

      /* ���� �� ��७�� */
      lRefFnd:
      FOR EACH bRef WHERE bRef.Class-Code  EQ "RCyber"
                       AND bRef.RefValue    EQ SUBST("CyberPlat|&1", mRef)
                     NO-LOCK,
         FIRST bPackObj WHERE bPackObj.PacketID  EQ bRef.PacketID
                          AND bPackObj.file-name EQ 'op-entry'
                        NO-LOCK,
         FIRST bOp WHERE bOp.op EQ INT64(ENTRY(1, bPackObj.Surrogate))
                   NO-LOCK:
     
         IF bOp.op-status BEGINS "�" THEN
            mChkStat = "7".
         ELSE 
            mChkStat = "0".

         mOP = bOp.op.

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("i-cyberpl.r", SUBST("op-status: &1, op.op = &2, mChkStat = &3",
                                              bOp.op-status,
                                              STRING(bOp.op),
                                              mChkStat)).
         &ENDIF

         /* �᫨ ��諨 �� ���㫨஢����, ���� �� ��室 */
         IF NOT (bOp.op-status BEGINS "�") THEN
            LEAVE lRefFnd.
      END. /* FOR EACH bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */
   END. /* ELSE IF mType EQ "status" THEN */
   ELSE IF mType EQ "check" THEN
   DO:
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("i-cyberpl.r", "action = check").
      &ENDIF
      /* �᫨ ��諨 �� �஢�ન, ����� ���⥦ �������� */
      mTmpStr = "0".

   END. /* ELSE IF mType EQ "status" THEN */

END. /* IF mChkStat EQ "0" THEN */

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("i-cyberpl.r", SUBST("ChkStat = &1", mChkStat)).
&ENDIF

mTmpStr = "".
CASE mChkStat:
   WHEN "-4" THEN
      mTmpStr = "�訡�� �஢�ન ��� ��� �ਭ��� ᮮ�饭���".

   WHEN "-3" THEN
      mTmpStr = "����७��� �訡�� ���⠢騪�".

   WHEN "-1" THEN
      mTmpStr = "������ �ଠ� �������⥫쭮�� ��ࠬ��� additional".

   WHEN "0" THEN
   DO:
      IF mType EQ "payment" THEN
         mTmpStr = "���⥦ �ᯥ譮 �஢����".
      ELSE IF mType EQ "check" THEN
         mTmpStr = "������� �������, �������� �ਥ� ���⥦��".
      ELSE IF mType EQ "status" THEN
         mTmpStr = "���⥦ �ਭ��".

   END. /* WHEN "0" THEN */

   WHEN "1" THEN
      mTmpStr = "��������� ⨯ �����".

   WHEN "2" THEN
      mTmpStr = "������� �� ������".

   WHEN "3" THEN
      mTmpStr = "����ୠ� �㬬� ���⥦�".

   WHEN "4" THEN
      mTmpStr = "����୮� ���祭�� ����� ���⥦�".

   WHEN "5" THEN
      mTmpStr = "����୮� ���祭�� ����".

   WHEN "6" THEN
      mTmpStr = "�ᯥ�� ���⥦ � ⠪�� ����஬ �� ������".

   WHEN "7" THEN
      mTmpStr = "���⥦ ���㫨஢��.".

   WHEN "11" THEN
      mTmpStr = "��������� ⨯ �����".

   WHEN "12" THEN
      mTmpStr = "��������� ��� �����⥫�".

   WHEN "13" THEN
      mTmpStr = "�������⨬� ��� �����⥫�".

   WHEN "14" THEN
      mTmpStr = "�訡�� ᮧ����� ���㬥��".

END CASE. /* mChkStat: */

RUN CreateXml IN THIS-PROCEDURE(mChkStat, 
                                STRING(mOp),
                                mDateOp, 
                                mTmpStr,
                                mKeyPriv, 
                                OUTPUT oAns).

/*============================================================================*/
/*=== ������� XML ============================================================*/
PROCEDURE CreateXml PRIVATE:
   DEF INPUT  PARAM iCode  AS  CHAR  NO-UNDO.  /* code         */
   DEF INPUT  PARAM iAuth  AS  CHAR  NO-UNDO.  /* authcode     */
   DEF INPUT  PARAM iDate  AS  CHAR  NO-UNDO.  /* date         */
   DEF INPUT  PARAM iMess  AS  CHAR  NO-UNDO.  /* message      */
   DEF INPUT  PARAM iKey   AS  CHAR  NO-UNDO.  /* ���� � ����� */
   DEF OUTPUT PARAM oXml   AS  CHAR  NO-UNDO.  

DEF VAR vXDoc     AS  HANDLE   NO-UNDO. /* X-DOCUMENT    */
DEF VAR vXRoot    AS  HANDLE   NO-UNDO. /* �᭮���� 㧥� */
DEF VAR vCmdLine  AS  CHAR     NO-UNDO.
DEF VAR vFile     AS  CHAR     NO-UNDO.
DEF VAR vXmlLong  AS  LONGCHAR NO-UNDO.
DEF VAR vCmdAns   AS  CHAR     NO-UNDO.


   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PROCEDURE CreateXml", "IN i-cyberpl RUN").
   &ENDIF

   CREATE X-DOCUMENT vXDoc.
   CREATE X-NODEREF  vXRoot.

   vxDoc:ENCODING = "windows-1251".

   vxDoc:CREATE-NODE(vXRoot, "response", "ELEMENT").
   vxDoc:APPEND-CHILD(vXRoot).

   /* ������� �� */
   IF iCode NE ? THEN
      RUN CreateXmlNode IN h_pays(vXDoc, vXRoot, "code", iCode).

   IF iAuth NE ? THEN
      RUN CreateXmlNode IN h_pays(vXDoc, vXRoot, "authcode", iAuth).

   IF iDate NE ? THEN
      RUN CreateXmlNode IN h_pays(vXDoc, vXRoot, "date", iDate).

   IF iMess NE ? THEN
      RUN CreateXmlNode IN h_pays(vXDoc, vXRoot, "message", iMess).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PROCEDURE CreateXml", 
                   SUBST("code = &1, authcode = &2, date = &3, message = &4",
                         iCode,
                         iAuth,
                         iDate,
                         CODEPAGE-CONVERT(iMess, "ibm866", "1251"))).
   &ENDIF

   /* ������뢠�� ᮮ�饭�� */
   IF {assigned iKey} THEN
   DO:
      ASSIGN
         vCmdLine = "openssl dgst -hex -sha1 -sign &1 &2"
         vFile    = GetUniqueFileName(STRING(RANDOM(1, 1000000)), "answ")
         vCmdLine = SUBST(vCmdLine, iKey, vFile)
      .
      vxDoc:SAVE("FILE", vFile).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PROCEDURE CreateXml", SUBST("vCmdLine = &1", vCmdLine)).
      &ENDIF

      INPUT THROUGH VALUE(vCmdLine) NO-ECHO NO-CONVERT.
      IMPORT UNFORM vCmdAns.
      INPUT CLOSE.

      OS-DELETE VALUE(vFile).

      IF NUM-ENTRIES(vCmdAns, "=") EQ 2 THEN
         vCmdAns = TRIM(ENTRY(2, vCmdAns, "=")).

      RUN CreateXmlNode IN h_pays(vXDoc, vXRoot, "sign", vCmdAns).
      vxDoc:SAVE("LONGCHAR", vXmlLong). 
      oXml = SUBSTR(vXmlLong, 1).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PROCEDURE CreateXml", 
                      SUBST("Answer = &1", REPLACE(REPLACE(oXml, "~r", ""), "~n", ""))).
      &ENDIF
   END. /* IF {assigned iKey} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PROCEDURE CreateXml", "IN i-cyberpl RETURN").
   &ENDIF

END PROCEDURE. /* CreateXml */ 

/*============================================================================*/

/*============================================================================*/
/*=== �஢�ઠ ������ =======================================================*/
PROCEDURE CheckSign PRIVATE:
   DEF INPUT  PARAM iReq   AS  CHAR  NO-UNDO.  /* �����       */
   DEF INPUT  PARAM iSign  AS  CHAR  NO-UNDO.  /* �������      */
   DEF INPUT  PARAM iKey   AS  CHAR  NO-UNDO.  /* ���� � ����� */
   DEF OUTPUT PARAM oRes   AS  LOG   NO-UNDO.  

DEF VAR vFileSign  AS  CHAR    NO-UNDO.  /* ��� 䠩�� � ��������                   */
DEF VAR vFileReq   AS  CHAR    NO-UNDO.  /* ��� 䠩�� � ����ᮬ                   */
DEF VAR vCmdLine   AS  CHAR    NO-UNDO.  /* ��ப� ��� �஢�ન ������            */
DEF VAR vCmdAns    AS  CHAR    NO-UNDO.  /* ������� �஢�ન ������             */
DEF VAR vTmpMem    AS  MEMPTR  NO-UNDO.  /*                                        */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PROCEDURE CheckSign", "IN i-cyberpl RUN").
   RUN dbgprint.p ("PROCEDURE CheckSign", SUBST("iReq: &1",iReq)).
   &ENDIF

   ASSIGN
      oRes     = NO
      vCmdLine = "openssl dgst -sha1 -verify &1 -signature &2 &3"
      /* ����塞 �� ᮮ�饭�� �� sign ����� � ���祭��� */
      /* &sign=&1 - SUBST �� �������� ⠪�� ������         */
      iReq      = REPLACE(iReq, "&" + SUBST("sign=&1", iSign), "")
     
      /* ����祥� 㭨���쭮� ��� 䠩�� */
      vFileSign = GetUniqueFileName(STRING(RANDOM(1, 1000000)), "sign")
      vFileReq  = REPLACE(vFileSign, "sign", "req")
   .

   IF {assigned iSign} THEN
   DO:
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PROCEDURE CheckSign", SUBST("vFileSign = &1", vFileSign)).
      RUN dbgprint.p ("PROCEDURE CheckSign", SUBST("vFileReq = &1",  vFileReq )).
      &ENDIF
     
      /* �८�ࠧ㥬 16-���� ��ப� c �������� � ������ ��� */
      vTmpMem = HEX-DECODE(iSign).
     
      /* ���࠭塞 ������� � 䠩� */
      OUTPUT TO VALUE(vFileSign) BINARY NO-CONVERT.
      EXPORT vTmpMem.
      OUTPUT CLOSE.
     
      /* ���࠭塞 ᮮ�饭�� � 䠩� */
      OUTPUT TO VALUE(vFileReq) BINARY NO-CONVERT.
      PUT UNFORM iReq.
      OUTPUT CLOSE.
     
      vCmdLine = SUBST(vCmdLine, iKey, vFileSign, vFileReq).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PROCEDURE CheckSign", SUBST("vCmdLine = &1", vCmdLine)).
      &ENDIF
     
      INPUT THROUGH VALUE(vCmdLine) NO-ECHO NO-CONVERT.
      IMPORT UNFORM vCmdAns.
      INPUT CLOSE.
     
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PROCEDURE CheckSign", SUBST("vCmdAns = &1", vCmdAns)).
      &ENDIF
     
      /* ������� �஢�ન ������ */
      oRes = vCmdAns EQ "Verified OK".
     
      OS-DELETE VALUE(vFileSign).
      OS-DELETE VALUE(vFileReq).

   END. /* IF {assigned iSign} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PROCEDURE CheckSign", "IN i-cyberpl RETURN").
   &ENDIF

END PROCEDURE. /* CheckSign */

/*============================================================================*/
