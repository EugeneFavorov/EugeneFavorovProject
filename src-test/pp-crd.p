/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-CRD.P
      Comment: �����⥭⭠� ��楤�� ࠡ��� � ����⥪��
   Parameters: ���
         Uses:
      Used by:
      Created: 10.05.2006 ILVI (55429)   
     Modified: 03/03/2008 kraw (0089297) ChkAcctOnComm
     Modified: 25.03.2008 muta 0087581  �㭪�� Chk_OrderPay_For_CBLACCT - �஢����,
                               ᮤ�ন��� �� ��।����� ���⥦� � �� �珫���
     Modified: 28.03.2008 muta 0088445  �㭪�� CalcRealAcctPos ��।���� �㬬�
                               ��� ᯨᠭ�� � ����⥪� � ��⮬ ����� ���⪠ � �����஢�� �㬬�.
                               ��楤�� fnd_acct_op ��७�ᥭ� �� kautools.lib.
     Modified: 25/06/2008 kraw (0094806) � CalcRealAcctPos ��� ����� ���� ��⨢��
*/

{pfuncdef
 &DefLib="CRD" 
 &Description="�����⥭⭠� ��楤�� ࠡ��� � ����⥪��"}

{globals.i}             /* �������� ��६���� ��ᨨ. */
{sh-defs.i}  
{intrface.get acct}  
{intrface.get kau}  
{lim-pos.i}
{debug.equ}

/* ����祭�� �� ��᪨ �����祭�� ��� ����⥪� �६����� ⠡����*/
PROCEDURE MakeTTContCrd.
   DEFINE INPUT  PARAMETER iLstMask AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ttContCrd.
   
   DEFINE  VARIABLE mWhere     AS CHARACTER  NO-UNDO.
   DEFINE  VARIABLE i          AS INT64    NO-UNDO.

   {empty ttContCrd}
   IF INDEX (iLstMask, "!") <> 0 THEN
   DO:
      RUN addTTCont(iLstMask). 
      RETURN "".
   END.

   DO i = 1 TO NUM-ENTRIES(iLstMask):
      mWhere = ENTRY (i, iLstMask).
      IF INDEX (mWhere, ".") <> 0 THEN
      DO:
         RUN addTTCont(mWhere). 
         NEXT.
      END.
   
      IF INDEX (mWhere, "*") =  0 THEN        /* ���� �����祭��   */
         RUN addCont(mWhere).
      ELSE /* ������窠 ���।��� */
         RUN addTTCont(mWhere). 
   END.
END PROCEDURE.

PROCEDURE addCont PRIVATE.
   DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
   FIND FIRST ttContCrd WHERE 
              ttContCrd.contract =  iValue NO-LOCK NO-ERROR.
   IF NOT AVAIL ttContCrd THEN
   DO:
      CREATE ttContCrd.
      ttContCrd.contract = iValue.
   END.
END PROCEDURE.
   
PROCEDURE addTTCont PRIVATE. 
   DEFINE INPUT PARAMETER iMask AS CHARACTER NO-UNDO.

   DEF BUFFER contract FOR contract. /* ���������� ����. */
      
   FOR EACH contract WHERE
            contract.contract >  ""
      AND   CAN-DO (iMask, contract.contract) NO-LOCK:
         RUN addCont(contract.contract).
   END.
END PROCEDURE.

/* ���� �����ᮢ��� ��� ���⠢������� �� ����⥪�, ��������ᮢ��� ���, 
*/

PROCEDURE fnd_acct_op:
   DEFINE INPUT  PARAMETER iKau      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER bAcct     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcct     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oCurrency AS CHARACTER NO-UNDO.

   DEFINE VAR vIdBalDoc              AS CHARACTER NO-UNDO.

   DEFINE BUFFER buf_op        FOR op.
   DEFINE BUFFER buf_op-entry  FOR op-entry.
   DEFINE BUFFER buf_bop       FOR op.
   DEFINE BUFFER buf_bop-entry FOR op-entry.
   DEFINE BUFFER kau           FOR kau.
   
   IF NUM-ENTRIES(iKau) <> 2  THEN RETURN.

   BLKAU:
   FOR EACH kau WHERE 
            kau.kau        =  iKau
      AND  (kau.balance    >  0
      OR    kau.curr-bal   >  0)  
      NO-LOCK:
      LEAVE BLKAU.
   END.
   IF NOT AVAIL(kau) THEN RETURN.
   FIND FIRST buf_op
      WHERE buf_op.op =  INT64(ENTRY(1,iKau)) NO-LOCK NO-ERROR.
   IF NOT AVAIL buf_op THEN RETURN.

   FIND FIRST buf_op-entry OF buf_op 
      WHERE buf_op-entry.op-entry =  INT64(ENTRY(2,iKau)) NO-LOCK NO-ERROR.

   IF NOT AVAIL buf_op-entry THEN RETURN.

   vIdBalDoc = getxattrvalueex("op",
                               STRING(buf_op.op),
                               "op-bal",
                               "").
   
   FIND FIRST buf_bop WHERE 
      buf_bop.op =  INT64(vIdBalDoc) 
   NO-LOCK NO-ERROR.
   
   IF vIdBalDoc =  "" OR NOT AVAIL buf_bop THEN
   FIND FIRST buf_bop WHERE 
            buf_bop.op-transaction =  buf_op.op-transaction
      AND   buf_bop.acct-cat       =  "b"
   NO-LOCK NO-ERROR.

   FIND FIRST buf_bop-entry OF buf_bop NO-LOCK NO-ERROR.
   
   IF AVAIL buf_bop-entry THEN
      bacct = buf_bop-entry.acct-db.
   ELSE IF AVAIL buf_bop THEN
      bacct = getxattrvalueex("op",
                               STRING(buf_bop.op),
                               "acctbal",
                               "").
   ELSE 
      bacct = getxattrvalueex("op",
                               STRING(buf_op.op),
                               "acctbal",
                               "").
   ASSIGN
      oacct     = kau.acct
      oCurrency = kau.currency
   .
   IF NOT CAN-FIND(acct WHERE acct.acct     =  bacct 
                          AND acct.currency =  ocurrency) 

   THEN
      bacct = "".

   IF NOT CAN-FIND(acct WHERE acct.acct     =  oacct 
                          AND acct.currency =  ocurrency) 

   THEN
      oacct = "".

END PROCEDURE.
 
/* ��।���� �㬬� ��� ᯨᠭ�� � ����⥪� � ��⮬ ����� ���⪠ �
�����஢�� �㬬� */
FUNCTION CalcRealAcctPos  RETURNS DECIMAL (
   INPUT iKau      AS CHARACTER, 
   INPUT iKauID    AS CHARACTER,
   INPUT iBAcct    AS CHARACTER,
   INPUT iBCurr    AS CHARACTER,
   INPUT iOrderPay AS CHARACTER,
   INPUT iDate     AS DATE):

   DEFINE VARIABLE mOAcct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBacct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mCurrency AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mKauID    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBlSum    AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mLimSum   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mRealPos  AS DECIMAL     NO-UNDO.
   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER acct  FOR acct.
   DEFINE BUFFER bop-entry FOR op-entry.

   IF {assigned iBAcct} THEN
      {find-act.i
          &acct  = iBAcct
          &curr  = iBCurr 
          &bact  = bacct
      }

   RUN fnd_acct_op(iKau,
                   OUTPUT mBAcct,
                   OUTPUT mOAcct,
                   OUTPUT mCurrency).
   IF {assigned mOAcct} AND iKauID =  "���⁫��" THEN 
   DO:
      RUN get-kau-id in h_kau (mOAcct, mCurrency, OUTPUT mKauID).
      IF mKauID <> iKauID THEN
      DO:
         FOR FIRST bop-entry WHERE 
                  bop-entry.acct-cr >  ""
            AND   bop-entry.kau-cr  =  iKau
            AND   bop-entry.op-date <> ? 
            AND   CAN-FIND(FIRST acct WHERE 
                        acct.acct   =  bop-entry.acct-db
                  AND   acct.kau-id =  iKauID)                                  
         NO-LOCK:
            mOAcct = bop-entry.acct-db.
         END.
      END.
   END.

   IF NOT AVAIL(bacct) THEN 
      {find-act.i
          &acct  = mBAcct
          &curr  = mCurrency
          &bact  = bacct
      }

   IF AVAIL(bacct) THEN DO: 
      IF iKauID = "���⁫��" 
         THEN mBlSum = GetBlockPosition(bacct.acct,bacct.currency, iOrderPay, iDate). 
         ELSE mBlSum = 0.

      mLimSum = GetLimitPosition(BUFFER bAcct, iDate). 

      RUN acct-pos IN h_base (bacct.acct,
                              bacct.currency,
                              iDate,
                              iDate,
                              '�').

      mRealPos = mBlSum + mLimSum - (IF bacct.currency =  "" THEN sh-bal ELSE sh-val).

      IF bacct.side =  "�" THEN
         mRealPos = -1 * mRealPos.
   END.
   IF mRealPos <= 0.0 THEN RETURN 0.0.

   IF {assigned mOAcct} THEN
   FIND FIRST kau WHERE kau.acct     =  mOAcct 
                    AND kau.currency =  mCurrency 
                    AND kau.kau      =  iKau NO-LOCK NO-ERROR.

   IF AVAIL(kau) THEN
      IF mRealPos > (IF mCurrency =  "" THEN kau.balance ELSE kau.curr-bal) THEN
         RETURN (IF mCurrency =  "" THEN kau.balance ELSE kau.curr-bal).
      ELSE RETURN mRealPos.
   ELSE RETURN 0.0.

END FUNCTION. 


FUNCTION ChkAcctOnComm RETURNS LOGICAL (INPUT iAcct AS CHARACTER):

   RETURN CAN-DO(FGetSetting("�⠭���", "�犮�", "-- ���� --"), iAcct).

END FUNCTION.

/* �᫨ ��।����� ���⥦� 㪠���� � �� �珫���, � �����頥� YES, ���� NO */
FUNCTION Chk_OrderPay_For_CBLACCT RETURNS LOGICAL (INPUT iOrderPay AS CHARACTER):

   RETURN CAN-DO(FGetSetting("���⁫��", "�珫���", ""), iOrderPay).

END FUNCTION.

/* ��।���� �ਭ������� �� ��� � ����⥪� 1,2,�����஢����� ��⮢ */
FUNCTION ChkAcctCart RETURNS LOGICAL (
   INPUT iHAcct   AS HANDLE     /* �����⥫� �� ����� ���. */
):
   DEF VAR vSurr   AS CHAR   NO-UNDO.
   DEF VAR vReturn AS LOG    NO-UNDO.
   DEF VAR vSchet  AS INT64    NO-UNDO.
   DEF VAR vKau    AS CHAR   NO-UNDO
               INIT "���⁂����,����1�����,����2�����".
   DEF BUFFER kau FOR kau. /* ���������� ����. */
   Block-Kau:
   DO vSchet = 1 TO NUM-ENTRIES(vKau):
      vSurr = GetXAttrValue(
                 "acct",                           
                 iHAcct:BUFFER-FIELD ("acct"):BUFFER-VALUE + "," 
                 + iHAcct:BUFFER-FIELD ("currency"):BUFFER-VALUE,
                 ENTRY(vSchet,vKau)).
      IF      vSurr <> ""
          AND CAN-FIND(FIRST kau WHERE kau.acct     =  ENTRY(1,vSurr)
                                   AND kau.currency =  ENTRY(2,vSurr)
                                   AND NOT kau.zero-bal) 
      THEN DO:
         vReturn = TRUE.
         LEAVE Block-Kau.
      END.
   END.
   RETURN vReturn.

END FUNCTION.

/* �஢�ઠ ��� �����⥫� �� ����஥筮�� ��ࠬ���� */
FUNCTION Chk_AcctRec_For_CBLACCT RETURNS LOGICAL (INPUT iAcctRec   AS CHAR): 
   RETURN CAN-DO(FGetSetting("���⁫��", "��᪨����", ""), iAcctRec).                   
END FUNCTION.

/*�஢�ઠ ����⥪� �� ������ ED275
�� �室� RECID �����ᮢ��� ���㬥��
�� ��室� 1 - ����� �⮧����, 2 - ����� ��뢠��
*/
FUNCTION CheckED275K2 RETURN CHAR
   (INPUT inRecID AS RECID):

   DEFINE BUFFER vbop       FOR op.
   DEFINE BUFFER vbop-entry FOR op-entry.

   DEFINE VARIABLE flager AS INT64 NO-UNDO.
   DEFINE VARIABLE mTrans AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE RECID(op) =  inRecID NO-LOCK NO-ERROR.
   IF AVAIL(op) THEN
   DO:
      FIND FIRST signs WHERE signs.file-name  =  "op" AND 
                             signs.code       =  "op-bal" AND
                             signs.code-value =  STRING(op.op)
      NO-LOCK NO-ERROR.
      IF AVAIL(signs) THEN 
      DO:
         FIND FIRST vbop WHERE vbop.op =  INT64(signs.surrogate) NO-LOCK NO-ERROR.
      END.
      &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CheckK2 ", "AVAIL(vbop)" + STRING(AVAIL(vbop))).
      &ENDIF
      IF NOT AVAIL(vbop) THEN RETURN "1".
      ELSE
      DO:
         FIND FIRST vbop-entry OF vbop NO-LOCK NO-ERROR.
         IF AVAIL(vbop-entry) THEN
         DO:
            FIND FIRST kau WHERE kau.acct =  vbop-entry.acct-db AND kau.currency =  vbop-entry.currency 
                           AND INT64(ENTRY(1,kau.kau)) =  vbop.op NO-LOCK NO-ERROR.
            IF AVAIL(kau) THEN
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("CheckK2 ", "kau AVAIL - kau.balance: " + STRING(kau.balance) + " " +
                                                       "kau.curr-bal: " + STRING(kau.curr-bal)).
               &ENDIF
/* ������ ���� ����
               IF vbop-entry.currency =  "" AND vbop-entry.amt-rub >  kau.balance THEN 
               DO:
                  RETURN "�� ���㬥��� 㦥 �뫨 ᯨᠭ��.".
               END.
               ELSE IF vbop-entry.currency <> "" AND vbop-entry.amt-cur >  kau.curr-bal THEN 
               DO:
                  RETURN "�� ���㬥��� 㦥 �뫨 ᯨᠭ��.".
               END.
               IF (vbop-entry.currency =  "" AND vbop-entry.amt-rub =  kau.balance) OR 
                  (vbop-entry.currency <> "" AND vbop-entry.amt-cur =  kau.curr-bal) THEN
*/             IF TRUE THEN
/* ����� ������ ���� ���� */
               DO:
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("CheckK2 ", "vbop-entry.currency:"  + vbop-entry.currency +
                                              " kau.balance:" + STRING(kau.balance) + 
                                              " kau.curr-bal:" + STRING(kau.curr-bal) + 
                                              " vbop-entry.amt-rub:" + STRING(vbop-entry.amt-rub) + 
                                              " vbop-entry.amt-cur:" + STRING(vbop-entry.amt-cur)).
                  &ENDIF
                  mTrans = FGetSetting("�����","����뢠","").
                  FIND FIRST op-kind WHERE op-kind.op-kind =  mTrans NO-LOCK NO-ERROR.
                  IF AVAIL(op-kind) THEN
                  DO:
                     FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
                     FIND FIRST acct WHERE acct.acct =  op-entry.acct-db NO-LOCK NO-ERROR.
                     IF AVAIL(acct) THEN
                     DO:
                        IF op-kind.proc =  "gcrddec1" THEN
                        DO:
                           RUN SetSysConf IN h_base ("ImpED107","��").
                           RUN SetSysConf IN h_base ("ImpED107Acct",vbop-entry.acct-db).
                           RUN SetSysConf IN h_base ("ImpED107Curr",vbop-entry.currency).
                           RUN SetSysConf IN h_base ("ImpED107VBOP",vbop.op).
                           RUN SetSysConf IN h_base ("ImpED107Date",vbop.op-date).
                           RUN VALUE(op-kind.proc + ".p") (gend-date,RECID(op-kind)).
                           flager = INT64(GetSysConf("ImpED107Flag")).
                           &IF DEFINED(IS-DEBUG) &THEN
                           RUN dbgprint.p ("CheckK2 ", "flager: " + STRING(flager)).
                           &ENDIF
                           IF flager =  0 THEN RETURN "1".
                           ELSE RETURN "�� 㤠���� �믮����� �࠭����� " + mTrans + ".".
                        END.
                        ELSE RETURN "�࠭����� " + mTrans + " ������ ���� �� ��楤�� gcrddec1.".
                     END.
                     ELSE RETURN "�� ������ ��� �����.".
                  END.
                  ELSE
                  DO:
                     RETURN "�� ������� �࠭����� " + mTrans + ".".
                  END.

               END.
               ELSE
               DO:
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("CheckK2 ", "vbop-entry.currency:"  + vbop-entry.currency +
                                              " kau.balance:" + STRING(kau.balance) + 
                                              " kau.curr-bal:" + STRING(kau.curr-bal) + 
                                              " vbop-entry.amt-rub:" + STRING(vbop-entry.amt-rub) + 
                                              " vbop-entry.amt-cur:" + STRING(vbop-entry.amt-cur)).
                  &ENDIF
                  RETURN "�� ���㬥��� 㦥 �뫨 ᯨᠭ��.".
               END.
            END.
            ELSE
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("CheckK2 ", "kau not avail").
               &ENDIF
               RETURN "1".
            END.
         END.
         ELSE RETURN "1".
      END.
   END.  
   ELSE RETURN "���㬥�� �� ������.".
END.
/* $LINTFILE='pp-crd.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.955+03:00' */
/*prosignOZQfVKrg/aehLvriFN84pQ*/