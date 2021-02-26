/* 
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-ESID.P
      Comment: ������⥪� ��ࠡ�⪨ ���஭��� �㦥���-���ଠ樮����
               ���㬥�⮢ �����
   Parameters: ���
         Uses: BY DYNAMIC
      Used BY:
      Created: 15.08.2005 NIK
     Modified: 30.11.2005 NIK 1. ������� ���� ��室���� ᮮ�饭��
                              2. ��ࠡ�⪠ ����ᮢ �����
                              3. �ᯮ�짮����� PacketCreateF
     Modified: 14.12.2005 NIK ���� �࠭���樨 ᮧ����� ���㬥��
     Modified: 15/04/2006 NIK ������ ᮮ�饭�� ED206
                              ��ࠡ�⪠ ����஫� ४����⮢ ���㬥��
     Modified: 26.05.2006 NIK PROGRESS v10.
     Modified: 17/05/2006 NIK ��������ਧ��� pp-pack.p
     Modified: 30/06/2006 NIK ���� �� ��뫪� �⢥��� ���㬥�⮢
     Modified: 03/07/2006 REVV � ���筮� �஢�થ ����� ���㬥�� ���������
                               ������ �஢�ઠ ����� ���㬥�� (u8003)
     Modified: 02/08/2006 NIK ��� �� �ࠢ����� ED206 �⢥��� ���㬥�⮢
     Modified: 17/08/2006 NIK ��������� ���⨯� GetEXCHOpKind
     Modified: 27/09/2006 NIK 1.��ࠢ����� ����ᠬ� ���㬥�⮢
                              2.����஫� ��᫥����⥫쭮�� ���⠭権
     Modified: 07/12/2006 NIK ��������� �믨᪨
     Modified: 09/01/2007 NIK 1. �ᯮ�짮����� ���������
                              2. ����஫� ���㬥�⮢ � �訡����
     Modified: 18/01/2007 NIK �ࠢ����� ����樮��஢����� ���㬥�⮢
     Modified: 30/01/2007 NIK �ࠢ����� ��砫��� �� ���⮬ ��� �����⥫�
     Modified:
*/
{globals.i}

{form.def}
{g-trans.equ}
{exchange.equ}

DEFINE VAR mCorrAcct    AS CHAR     NO-UNDO.
DEFINE VAR mOpDate      AS DATE     NO-UNDO.
DEFINE VAR mFake        AS INT64  NO-UNDO.

DEFINE VAR mSelfID      AS CHAR     NO-UNDO.
DEFINE VAR mSettID      AS CHAR     NO-UNDO.
DEFINE VAR mBespID      AS CHAR     NO-UNDO.

DEFINE VAR mStmtKind    AS CHAR     NO-UNDO.
DEFINE VAR mStmtStatus  AS CHAR     NO-UNDO.
DEFINE VAR mStmtSCheck  AS LOGICAL  NO-UNDO.
DEFINE VAR mhStmtConv   AS handle   NO-UNDO.
DEFINE VAR mfStmtConv   AS CHAR     NO-UNDO.

DEFINE VAR mStateRecv   AS CHAR     NO-UNDO.
DEFINE VAR mStateErr    AS CHAR     NO-UNDO.
DEFINE VAR mStateRtrn   AS CHAR     NO-UNDO.
DEFINE VAR mStateSend   AS CHAR     NO-UNDO.
DEFINE VAR mStateDone   AS CHAR     NO-UNDO.
DEFINE VAR mStateChgd   AS CHAR     NO-UNDO.
DEFINE VAR mDBTransAcct AS CHAR     NO-UNDO.
DEFINE VAR mEDNoSetStat AS CHAR     NO-UNDO.
DEFINE VAR mState276Op1 AS CHAR     NO-UNDO.
DEFINE VAR mState276Op2 AS CHAR     NO-UNDO.
DEFINE VAR mUISCOS      AS CHAR     NO-UNDO.
DEFINE VARIABLE mStatus508    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mStatusERR508 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderRez     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderMail    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOrderDef     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mIntRezLim    AS INT64       NO-UNDO.
DEFINE VARIABLE mRezFlag      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mRezREF       AS INT64       NO-UNDO.


DEFINE VAR mTxt274      AS CHAR     NO-UNDO.
DEFINE VAR mInfoCode274 AS CHAR     NO-UNDO.


DEFINE VARIABLE mPropDoc AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE mPropAtt AS LONGCHAR    NO-UNDO.

{intrface.get xclass}
{intrface.get tmess}
{intrface.get strng}

{intrface.get pbase}
{intrface.get trans}
{intrface.get blkob}

{intrface.get exch}
{intrface.get pack}
{intrface.get rfrnc}
{intrface.get filex} 

{intrface.get xrkc}
{intrface.get op}
{intrface.get cust}
{intrface.get count}

{intrface.get bank}
{intrface.get refer}
{intrface.get email}
{intrface.get separate}
{bank-id.i}                                      /*�����䨪���� ��襣� �����*/
{ttretval.def}

&GLOB ON-ERROR IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

DEFINE TEMP-TABLE ttAttr NO-UNDO
   FIELD attr-class AS CHAR
   FIELD attr-code  AS CHAR
   FIELD attr-name  AS CHAR
   FIELD attr-value AS CHAR
   FIELD exch-class AS CHAR
   FIELD exch-code  AS CHAR
   FIELD is-view    AS LOGICAL
   FIELD order      AS INT64
   INDEX order order.

DEFINE TEMP-TABLE ttPars NO-UNDO
   FIELD PasrsField  AS CHAR
   FIELD FieldValue  AS CHAR
   FIELD Ind1        AS INT64
   FIELD Ind2        AS INT64
.
DEFINE temp-table tt501 NO-UNDO
    FIELD BranchID      AS CHAR
    FIELD OpenDate      AS DATE
    FIELD CloseDate     AS DATE
    FIELD Code          AS CHAR
.
DEFINE temp-table tt503 NO-UNDO
    FIELD BranchID      AS CHAR
    FIELD OpenDate      AS DATE
    FIELD CloseDate     AS DATE
    FIELD OurSWIFTBIC   AS CHAR
    FIELD Code          AS CHAR
    FIELD BIC           AS CHAR
    FIELD SwiftTypeList AS CHAR
.                         


{esid-exp.pro}
{esid-imp.pro}

DEFINE TEMP-TABLE ttInitPack
   FIELD PacketID AS INT64
   INDEX PacketID PacketID.


/*============================================================================*/
mStateErr   = FGetSetting("�����","�����衪","�").
mStateRecv  = FGetSetting("�����","�����ਭ","���").
mStateRtrn  = FGetSetting("�����" + (IF work-module =  "mail-mbr" 
                                        THEN "-���" 
                                        ELSE ""),
                          "����Ⴎ��","����").
mStateSend  = FGetSetting("�����","�������","���").
mStateDone  = FGetSetting("�����" + (IF work-module =  "mail-mbr" 
                                        THEN "-���" 
                                        ELSE ""),
                          "�����ᯫ",chr(251)).
mStateChgd  = FGetSetting("�����","����ሧ�",chr(251)).
IF NOT {assigned mStateChgd} THEN
   mStateChgd = CHR(251).
mfStmtConv  = FGetSetting("�����","StmCnvOpKind","").
mDBTransAcct = FGetSetting("�����","DBITransAcctList",?).
mStmtSCheck = CAN-FIND(FIRST code WHERE
                             code.class =  ""
                         AND code.code  =  "���������").
mSelfID     = RKCGetSendID().
mSettID     = FGetSetting("�����","SendID",mSelfID).
mBespID     = FGetSetting("����","SendID", "").

mEDNoSetStat = FGetSetting("����","ED201NoSetOpStat","").
mState276Op1 = FGetSetting("�����","���276��1","").
mState276Op2 = FGetSetting("�����","���276��2","").
mUISCOS = FGetSetting("���-���",?,"NO").
mStatus508 = FGetSetting("�����","�����508","").
mStatusERR508 = FGetSetting("�����","���衪508","").

ASSIGN
   mOrderRez  = FGetSetting("�����","OrderRez","")
   mOrderMail = FGetSetting("�����","OrderMail","")
   mOrderDef  = FGetSetting("�����","OrderDef","")
   mIntRezLim = INT64(GetEntries(2, mOrderRez, "-", "0"))
   mRezREF    = INT64(GetEntries(1, mOrderRez, "-", "0"))
NO-ERROR.

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-esid.p","mStateErr:" + GetNullStr(mStateErr) + 
                             " mStateRecv:" + getNullStr(mStateRecv) + 
                             " mStateRtrn:" + GetNullStr(mStateRtrn) +
                             " mStateSend:"  + getNullStr(mStateSend) + 
                             " mStateDone:" + getNullStr(mStateDone) + 
                             " mfStmtConv:" + getNullStr(mfStmtConv) + 
                            " mStmtSCheck:" + STRING(mStmtSCheck) +
                           " mEDNoSetStat:" + GetNullStr(mEDNoSetStat)).    
&ENDIF

RUN ParsFunc-���(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

DEFINE BUFFER c-nostro FOR c-nostro.
FOR FIRST c-nostro WHERE
          c-nostro.corr-acct =  bank-acct
          NO-LOCK:
   mCorrAcct = c-nostro.acct.
END.

{pfuncdef
   &DefLib="esid" 
   &Description="������⥪� ��ࠡ�⪨ ���஭��� �㦥���-���ଠ樮����
               ���㬥�⮢ �����"}
   
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CheckEmptyFld LOGICAL (INPUT iVal  AS CHAR,
                                INPUT iName AS CHAR):
   IF iVal <> "?" THEN
      RETURN YES.
   ELSE DO:
      RUN Fill-SysMes("","ExchRKC34","","%s=" + iName).
      RETURN NO.
   END.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* �ࠢ����� ���㬥�� � ���⠭樨 (�믨᪥) � � ����                         */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDOperCompare:
   DEFINE INPUT  PARAMETER hWOP     AS handle NO-UNDO.
   DEFINE INPUT  PARAMETER iSurr    AS CHAR   NO-UNDO.
   DEFINE PARAMETER BUFFER op-entry FOR op-entry.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vAcctSend AS CHAR           NO-UNDO.
   DEFINE VAR vAcctRecv AS CHAR           NO-UNDO.
   DEFINE VAR vBaseRecv AS CHAR           NO-UNDO.
   DEFINE VAR vBankCode AS CHAR           NO-UNDO.
   DEFINE VAR vAmtRub   AS decimal        NO-UNDO.
   DEFINE VAR vDC       AS CHAR           NO-UNDO.
   DEFINE VAR vDocNum   AS CHAR           NO-UNDO.
   DEFINE VAR vSeanceID AS INT64        NO-UNDO.
   DEFINE VAR vDocNumBs AS CHAR           NO-UNDO.
   DEFINE VAR vDocNumDc AS CHAR           NO-UNDO.
   DEFINE VAR vTail     AS INT64            NO-UNDO.
   DEFINE VAR vAcctXtr  AS CHAR           NO-UNDO.
   {profile ST041}

   DEFINE BUFFER op-bank   FOR op-bank.
   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER mail-user FOR mail-user.

MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vAcctSend = hWOP:buffer-field("acct-send"):buffer-value
         vAcctRecv = hWOP:buffer-field("acct-rec"):buffer-value
         vBankCode = hWOP:buffer-field("bank-code-rec"):buffer-value
         vAmtRub   = hWOP:buffer-field("amt-rub"):buffer-value
         vDC       = hWOP:buffer-field("Direct"):buffer-value
         vDocNum   = hWOP:buffer-field("doc-num"):buffer-value
         vSeanceID = hWOP:buffer-field("SeanceID"):buffer-value

      NO-ERROR. {&ON-ERROR}
         IF NOT {assigned vDocNum} THEN
            vDocNum = hWOP:buffer-field(GetMangledName("������")):buffer-value.

/*-------- ��� ��室���� ���㬥��, "��ॢ��稢���" ४������ ��� �⢥��� --*/
      IF NOT {assigned iSurr} THEN DO:

         /* �믮������ ������⨧�樨   */
         hWOP:buffer-field("op-kind"):buffer-value = GetEXCHOpKind(vSeanceID,
                                                                   {&DIR-IMPORT},
                                                                   BUFFER mail-user).
         CASE vDC:
            WHEN "1" THEN ASSIGN
               hWOP:buffer-field("Direct"):buffer-value = "��砫��"
            NO-ERROR.
            WHEN "2" THEN ASSIGN
               hWOP:buffer-field("Direct"):buffer-value = "�⢥��"
            NO-ERROR.
         END CASE.
         vFlagSet = YES.
         LEAVE MAIN.
      END.

/*-------------------------------------------------- ���� ���㬥�� � ���� --*/
      FIND FIRST op WHERE
                 op.op =  INT64(ENTRY(1,iSurr))
                 NO-LOCK NO-ERROR. {&ON-ERROR}
      FIND FIRST op-entry OF op WHERE
                 op-entry.op-entry =  INT64(ENTRY(2,iSurr))
                 NO-LOCK NO-ERROR. {&ON-ERROR}
      FIND FIRST op-bank OF op
                 NO-LOCK NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperCompare","==" + string(string(hWOP:buffer-field("RecvREF"):buffer-value),"x(25)") +
                                         " " + string(string(hWOP:buffer-field("SendREF"):buffer-value),"x(25)") +
                                     " err:" + GetNullStr(hWOP:buffer-field("ErrorList"):buffer-value)).
      RUN dbgprint.p ("ESIDOperCompare","DOC " + string(op.doc-num,"x(25)") +
                                           " " + string(vDocNum,"x(25)")).
      RUN dbgprint.p ("ESIDOperCompare","AMT " + string(string(op-entry.amt-rub),"x(25)") +
                                           " " + string(string(vAmtRub),"x(25)")          +
                                           " " + string(op-entry.amt-rub <> vAmtRub)).
      &ENDIF

/*--------------------------------- �஢�ઠ �� ᮢ������� ����� ���㬥�� --*/
      ASSIGN   vDocNumBs = TRIM(op.doc-num)
               vDocNumDc = TRIM(vDocNum)
               vTail     = LENGTH(LEFT-TRIM(vDocNumDc,"0")).

      IF vDocNumBs <> vDocNumDc
      THEN DO:
         IF ((LENGTH(vDocNumBs) >= vTail)                                    AND
             (SUBSTRING(vDocNumBs,LENGTH(vDocNumBs) - vTail + 1, vTail) EQ
              SUBSTRING(vDocNumDc,LENGTH(vDocNumDc) - vTail + 1, vTail)))
         THEN
            RUN AddErrorFormat(hWOP,"b8002"). /* ᮢ������� "墮��"          */
         ELSE
            RUN AddErrorFormat(hWOP,"b8003"). /* ��ᮢ������� "墮��"        */
      END.

      IF op-entry.amt-rub <> vAmtRub THEN RUN AddErrorFormat(hWOP,"b8004").

/*----------------------------- �ࠢ����� ४����⮢ ���⥫�騪�/�����⥫� --*/
      CASE vDC:
/*-------------------------------------------------------- ��砫�� ���⥦ --*/
         WHEN "1" THEN DO:
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperCompare","N-DB " + string(op-entry.acct-db,"x(25)")         +
                                                  " " + string(vAcctSend,"x(25)")                +
                                                  " " + string(op-entry.acct-db <> vAcctSend)).
            RUN dbgprint.p ("ESIDOperCompare","N-CR " + string(op.ben-acct,"x(25)")              +
                                                  " " + string(vAcctRecv,"x(25)")                +
                                                  " " + string(op.ben-acct <> vAcctRecv)).
            &ENDIF

            IF {assigned vAcctSend}          AND /* ��� ���⥫�騪�          */
               NOT CAN-DO(mDBTransAcct,DelFilFromAcct(op-entry.acct-db)) AND
               DelFilFromAcct(op-entry.acct-db) <> vAcctSend THEN DO:
               RUN AddErrorFormat(hWOP,"b8010").
               vAcctXtr = GetXAttrValueEX("op",string(op.op),"acct-send","").
               IF {assigned vAcctXtr} AND vAcctXtr <> vAcctSend THEN
                  RUN AddErrorFormat(hWOP,"b8014").
            END.

            IF {assigned op.ben-acct} THEN       /* ��� �����⥫�           */
               IF {assigned vAcctRecv}     AND
                  op.ben-acct <> vAcctRecv THEN
                  RUN AddErrorFormat(hWOP,"b8012").
               ELSE.
            ELSE                                 /* ��� �����⥫� ���      */
               IF {assigned vAcctRecv} THEN      /* � �믨᪥ ��� ����       */
                  IF vAcctRecv BEGINS "3010" THEN.
                  ELSE
                     RUN AddErrorFormat(hWOP,"b8012").
               ELSE.
         END.
/*--------------------------------------------------------- �⢥�� ���⥦ --*/
         WHEN "2" THEN DO:
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperCompare","R-CR " + string(op-entry.acct-cr,"x(25)")         +
                                                  " " + string(vAcctRecv,"x(25)")                +
                                                  " " + string(op-entry.acct-cr <> vAcctRecv)).
            RUN dbgprint.p ("ESIDOperCompare","R-DB " + string(op.ben-acct,"x(25)")              +
                                                  " " + string(vAcctSend,"x(25)")                +
                                                  " " + string(op.ben-acct <> vAcctSend)).
            &ENDIF

            IF {assigned vAcctSend}     AND      /* ��� ���⥫�騪�          */
               op.ben-acct <> vAcctSend THEN DO:
               IF NOT (vAcctSend =  bank-acct AND  /* �� ����㧪� ED211 ��� ��ࠢ�⥫� ����� ���� �/� ����� � ��� */
                       hWOP:buffer-field("mail-format"):buffer-value =  "XML-TransInfo") THEN
                  RUN AddErrorFormat(hWOP,"b8020").
            END.
            IF {assigned vAcctRecv}          AND /* ��� �����⥫�           */
               DelFilFromAcct(op-entry.acct-cr) <> vAcctRecv THEN DO:
               vBaseRecv = GetXAttrValue("op",string(op.op),"acct-rec").
               IF vAcctRecv BEGINS "3010"  AND
                  NOT {assigned vBaseRecv} THEN. /* ��ଠ                     */
               ELSE DO:
                  RUN AddErrorFormat(hWOP,"b8022").
                  IF vBaseRecv <> vAcctRecv THEN
                     RUN AddErrorFormat(hWOP,"b8024").
               END.
            END.
         END.
      END CASE.

      IF {assigned vBankCode}            AND
        (NOT AVAILABLE(op-bank)          OR
         op-bank.bank-code <> vBankCode) THEN
         RUN AddErrorFormat(hWOP,"b8030").

      vFlagSet = YES.
   END.
   {profile ST050}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ��������� �室�饣� ᮮ�饭��                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDPacketCreate:
   DEFINE INPUT  PARAMETER hWOP        AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER hMain       AS handle   NO-UNDO.
   DEFINE PARAMETER BUFFER bCode       FOR Code.
   DEFINE OUTPUT PARAMETER oPacketID   AS INT64  NO-UNDO.
   DEFINE OUTPUT PARAMETER oParentID   AS INT64  NO-UNDO.

   DEFINE BUFFER bPacket FOR Packet.
   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vMailUser AS INT64        NO-UNDO.
   DEFINE VAR vSeanceID AS INT64        NO-UNDO.
   DEFINE VAR vParentID AS INT64        NO-UNDO.
   DEFINE VAR vPacketID AS INT64        NO-UNDO.
   DEFINE VAR vSendID   AS CHAR           NO-UNDO.
   DEFINE VAR vSendREF  AS CHAR           NO-UNDO.
   DEFINE VAR vSendDate AS DATE           NO-UNDO.
   {profile ST051}

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vSeanceID = hMain:buffer-field("SeanceID"):buffer-value
         oParentID = hMain:buffer-field("PacketID"):buffer-value
         vSendID   = hWOP:buffer-field("SendID"):buffer-value
         vSendREF  = hWOP:buffer-field("SendREF"):buffer-value
         vSendDate = hWOP:buffer-field("SendDate"):buffer-value
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDPacketCreate","iFormat:" + GetNullStr(bCode.Code)        +
                                        " op-kind:" + GetBaseOpKind()               +
                                      " vSeanceID:" + GetNullStr(string(vSeanceID)) +
                                      " vParentID:" + GetNullStr(string(vParentID)) +
                                        " vSendID:" + GetNullStr(vSendID)           +
                                       " vSendREF:" + GetNullStr(vSendREF)          +
                                      " vSendDate:" + GetNullStr(string(vSendDate))).
      &ENDIF

PACK:
      DO TRANSACTION ON ERROR UNDO PACK, RETRY PACK:
         IF RETRY THEN UNDO MAIN, RETRY MAIN.
                                                 /* ������� ᮮ�饭��         */
         RUN PacketCreateF   (INPUT  vSeanceID,
                              BUFFER bCode,
                              OUTPUT oPacketID).

         IF vSendDate      <> ? AND              /* ��뫪� �ਭ�⮣� ᮮ�襭��*/
            {assigned vSendREF} AND
            {assigned vSendID}  THEN
            RUN PacketCreateRef (INPUT  vSendDate,
                                 INPUT  oPacketID,
                                 INPUT  bCode.Misc[{&RKC-REPLY}],
                                 INPUT  vSendID + "|" +
                                        ReferenceFormatValue(bCode.Misc[{&RKC-REPLY}],vSendREF)) NO-ERROR.

         ASSIGN
            hWOP:buffer-field("PacketID"):buffer-value = oPacketID
            hWOP:buffer-field("SeanceID"):buffer-value = vSeanceID
         NO-ERROR. {&ON-ERROR}

      END.

      vFlagSet = YES.
   END.
   {profile ST060}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���� ��室���� ���㬥��                                                  */
/*----------------------------------------------------------------------------*/

PROCEDURE ESIDOperationFind:
   DEFINE INPUT  PARAMETER hWOP        AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER iPacketID   AS INT64    NO-UNDO.
   DEFINE PARAMETER BUFFER bCode FOR Code.
   DEFINE OUTPUT PARAMETER oInitialID  AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER oSurr       AS CHAR     NO-UNDO.

   DEFINE BUFFER Reference    FOR Reference.
   DEFINE BUFFER PackObject   FOR PackObject.
   DEFINE BUFFER Packet       FOR Packet.
   DEFINE BUFFER op           FOR op.
   DEFINE BUFFER bPacket      FOR Packet.

   DEFINE VAR vFlagSet   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagFnd   AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vFlagBPack AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vRecvID    AS CHAR           NO-UNDO.
   DEFINE VAR vRecvREF   AS CHAR           NO-UNDO.
   DEFINE VAR vRecvFMT   AS CHAR           NO-UNDO.  /* ��ଠ�஢����� ��몠 */
   DEFINE VAR vListCls   AS CHAR           NO-UNDO.  /* ���᮪ ����ᮢ ��뫮�  */
   DEFINE VAR vRetCls    AS CHAR           NO-UNDO.  /* ����� ��뫪� ������   */
   DEFINE VAR vRecvCls   AS CHAR           NO-UNDO.  /* ����騩 ����� ��뫪�   */
   DEFINE VAR vClsFirst  AS CHAR           NO-UNDO.  
   DEFINE VAR vRecvDate  AS DATE           NO-UNDO.
   DEFINE VAR vItm       AS INT64          NO-UNDO.
   DEFINE VAR vDisable   AS CHAR           NO-UNDO.

   DEFINE VARIABLE vFormat   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStateUBR AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMakePack AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vNumEntry AS INT64       NO-UNDO.

   DEFINE BUFFER sCode FOR code.
   DEFINE BUFFER bOp       FOR op.
   DEFINE BUFFER bOp-entry FOR op-entry.
   {profile ST061}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      RUN ParsFunc-���(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.

      ASSIGN
         oSurr      = ""
         vRecvDate  = hWOP:buffer-field("RecvDate"):buffer-value
         vRecvID    = hWOP:buffer-field("RecvID"):buffer-value
         vRecvREF   = hWOP:buffer-field("RecvREF"):buffer-value
         vFormat    = hWOP:BUFFER-FIELD("mail-format"):BUFFER-VALUE
         vListCls   = bCode.Misc[{&RKC-MAIN}]          /* ��᪮�쪮 ����ᮢ    */
         vRetCls    = bCode.Misc[{&RKC-REPLY}]
         vFlagFnd   = NO
         vFlagBPack = NO
         oInitialID = ?
      NO-ERROR. {&ON-ERROR}

      IF vFormat =  "XML-ED201"    OR       /* ��� �������� ����ᮢ, ��襤�� �� ��� � ED205 �� �����, */
         vFormat =  "XML-ED201BSP" OR       /* ���� �� ��ࠡ��뢠���� (�� �ਢ�뢠���� � ���㬥�⠬,     */
         vFormat =  "XML-ED206"    OR       /* �� �஢������ �� ��᫥����⥫쭮���, �� ������� �����)  */
         vFormat =  "XML-ED206BSP" OR
         vFormat =  "XML-ED508" THEN
         vMakePack = YES.
      ELSE DO:
         vStateUBR = hWOP:buffer-field("State"):BUFFER-VALUE.
         IF GetCodeBuff("ESIDState",vStateUBR, buffer sCode) THEN ASSIGN
            vMakePack = NOT (sCode.Misc[1] =  "���" or sCode.Misc[1] =  "NO").
         ELSE 
            vMakePack = NO.
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvDate:" + GetNullDat(vRecvDate) +
                                           " vRecvID:" + GetNullStr(vRecvID)   +
                                           " mSelfID:" + GetNullStr(mSelfID)   +
                                          " vRecvREF:" + GetNullStr(vRecvREF)  +
                                          " vListCls:" + GetNullStr(vListCls)).
      &ENDIF
    
/*----------------------------------- ����஫� ��������� ४����⮢ ���᪠ --*/
      CheckEmptyFld(GetNullStr(vRecvID),  "EDAuthor") AND
      CheckEmptyFld(GetNullStr(vRecvREF), "EDNo")     AND
      CheckEmptyFld(GetNullDat(vRecvDate),"EDDate").

      IF vRecvDate <  mOpDate THEN               /* ���⠭�� �� ���譨� ���*/
         RUN AddErrorFormat(hWOP,"b8001").

/*----------------------------------------- ��।���� �ଠ� � ����� ��뫪� --*/
      IF (vRecvID =  mSelfID) THEN DO:
         ASSIGN        /* ���⠭�� �� ��� ���㬥�� */
         vRecvCls = ENTRY(1,vListCls)
         vRecvFMT = ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.                                  
         {&ON-ERROR}
      END.
      ELSE DO: /*���⠭�� �� �㦮� ���㬥��*/
         IF NOT ((NUM-ENTRIES(vListCls) =  2) OR (NUM-ENTRIES(vListCls) =  4)) THEN DO:
            RUN Fill-SysMes("","ExchRKC35","",
                            "%s=" + GetNullStr(bCode.Code) +
                            "%s=" + GetNullStr(vRecvID)    +
                            "%s=" + GetNullStr(vRecvREF)   +
                            "%s=" + GetNullDat(vRecvDate)  +
                            "%s=" + GetNullStr(vListCls)).
            LEAVE MAIN.
         END.

         ASSIGN
            vRecvCls = ENTRY(2,vListCls)
            vRecvFMT = vRecvID + "|" + ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.
         {&ON-ERROR}
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvDate:" + GetNullDat(vRecvDate) +
                                          " vRecvFMT:" + GetNullStr(vRecvFMT)  +
                                          " vRecvCls:" + vRecvCls).
      RUN dbgprint.p ("ESIDOperationFind","vFormat:"   + vFormat).
      &ENDIF
      
/*----------------------------------------------- ������ ��室��� ᮮ�饭�� --*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","FindOpEntryByREF START").
      &ENDIF

      vClsFirst = vRecvCls.
         RUN  FindOpEntryByREF(INPUT        hWOP,
                               INPUT        iPacketID,
                               BUFFER       bCode,
                               INPUT        vRecvDate,
                            INPUT        vClsFirst,
                               INPUT        vRecvFMT,
                               OUTPUT       oInitialID,
                               OUTPUT       oSurr,
                               OUTPUT       vFlagFnd,
                               INPUT-OUTPUT vFlagBPack).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vFlagFnd:" + STRING(vFlagFnd)).
      &ENDIF

         IF NOT vFlagFnd THEN
FND_LST:
         DO vNumEntry = 1 TO NUM-ENTRIES(vListCls):
         vRecvCls = ENTRY(vNumEntry, vListCls).

            &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("ESIDOperationFind","vRecvID:" + GetNullStr(vRecvID)   +
                                           " vRecvCls:" + GetNullStr(vRecvCls)).
            &ENDIF
      
         IF (vRecvCls =  vClsFirst) THEN /* 㦥 �஢��﫨 */
            NEXT FND_LST.
         /* ��� ��室���� ���㬥�� �� ���室�� ������ ��뫮� *Return */
         IF (vRecvID =  mSelfID) AND (vRecvCls MATCHES "*Return") THEN 
            NEXT FND_LST.
         /* ��� �⢥⭮�� ���㬥�� �� ���室�� ������ ��뫮� *Begin */
         IF (vRecvID <> mSelfID) AND (vRecvCls MATCHES "*Begin") THEN
            NEXT FND_LST.
      
         ASSIGN
            vRecvFMT = (IF vRecvCls MATCHES "*Return"  
                        THEN vRecvID + "|" 
                        ELSE "" ) + 
                       ReferenceFormatValue(vRecvCls,vRecvREF)
         NO-ERROR.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("pp-esid.p","vRecvCls: " + vRecvCls).
         RUN dbgprint.p ("pp-esid.p","vRecvFMT: " + vRecvFMT).
      &ENDIF
      
         RUN  FindOpEntryByREF(INPUT        hWOP,
                               INPUT        iPacketID,
                               BUFFER       bCode,
                               INPUT        vRecvDate,
                               INPUT        vRecvCls,
                               INPUT        vRecvFMT,
                               OUTPUT       oInitialID,
                               OUTPUT       oSurr,
                               OUTPUT       vFlagFnd,
                               INPUT-OUTPUT vFlagBPack).
         /* �᫨ ��諨 ���㬥��, ��室�� */
         IF vFlagFnd =  YES THEN
            LEAVE FND_LST.

      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ESIDOperationFind","vRecvID:"    + GetNullStr(vRecvID)   +
                                          " vRecvDate:" + GetNullDat(vRecvDate) +
                                          " vRecvFMT:"  + GetNullStr(vRecvFMT)  +
                                          " vRecvCls:"  + vRecvCls              +
                                          " vMakePack:"  + STRING(vMakePack)              +
                                          " NOT vFlagBPack:"  + STRING(NOT vFlagBPack)              +
                                          " vFlagFnd:"  + STRING(vFlagFnd)).
      &ENDIF
      
      IF NOT vFlagBPack      /* �᫨ ������ �� �����           */ 
         OR vMakePack THEN   /* ��� ࠧ�襭� ��ࠡ�⪠ ����� */
         FOR EACH ttInitPack NO-LOCK,
             FIRST PackObject WHERE
                   PackObject.PacketID  =  ttInitPack.PacketID
               AND PackObject.File-Name =  "op-entry"
                   NO-LOCK,
             FIRST op WHERE
                   op.op = INT64(ENTRY(1,PackObject.Surrogate))
                   NO-LOCK  QUERY-TUNING(HINT "RULE"):
                                                 /* ���� ����. � ���㬥�⮬  */

            FIND FIRST packet WHERE
                       Packet.PacketID =  ttInitPack.PacketID NO-LOCK NO-ERROR.

            IF avail packet AND
               (packet.mail-format BEGINS "XML-ED244" OR
                packet.mail-format BEGINS "XML-ED243") AND
               GetEntries(2,bCode.Code,"-","") BEGINS "ED201" THEN
            DO:
               RUN PacketCreateLink (Packet.PacketID,
                                     "Packet",
                                     iPacketID,
                                     ENTRY(2,bCode.Description[1])).
            END.
            ELSE DO:
               RUN PacketCreateLink (iPacketID,
                                     PackObject.File-Name,
                                     PackObject.Surrogate,
                                     ENTRY(2,bCode.Description[1])).
               ASSIGN oSurr      = PackObject.Surrogate
                      vFlagFnd   = YES.

            END.

             IF GetEntries(2,bCode.Code,"-","") BEGINS "ED508" THEN
               vFlagBPack = NO.

/*--------------------------- ����஫� ��᫥����⥫쭮�� ������ ���⠭権 --*/
            CASE GetEntries(2,bCode.Code,"-",""):
               WHEN "ED201"   THEN vDisable = "ED211,ED206,ED207,ED205".
               WHEN "ED205"   THEN vDisable = "ED211,ED206".
               WHEN "EDInfo"  THEN vDisable = "ED211,ED206".
               WHEN "ED206"   THEN vDisable = "ED211".
               OTHERWISE           vDisable = "".
            END CASE.

            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ESIDOperationFind","bCode.Code:" + bCode.Code +
                                                 " vDisable:" + GetNullStr(vDisable)).
            &ENDIF

            IF NOT PacketEnableExhange ("op-entry",
                                    PackObject.Surrogate,
                                    vDisable,
                                    "",
                                    0) THEN
               RUN AddErrorFormat(hWOP,"b8050").
         END.                                 /* FOR FIRST PackObject ...  */

      IF vFlagFnd <> YES     AND
         {assigned vRecvREF} THEN
         RUN AddErrorFormat(hWOP,"b8000").

      IF vFlagBPack THEN
         ASSIGN
            oSurr       = ""
         .
      vFlagSet = YES.
   END.                                          /* MAIN: DO ON ERROR ...     */
   {empty ttInitPack}
   {profile ST070}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE FindOpEntryByREF.
   DEFINE INPUT        PARAMETER hWOP        AS handle  NO-UNDO.
   DEFINE INPUT        PARAMETER iPacketID   AS INT64 NO-UNDO.
   DEFINE PARAMETER    BUFFER bCode FOR Code.
   DEFINE INPUT        PARAMETER iRecvDate   AS DATE    NO-UNDO.
   DEFINE INPUT        PARAMETER iRecvCls    AS CHAR    NO-UNDO.
   DEFINE INPUT        PARAMETER iRecvFMT    AS CHAR    NO-UNDO.
   DEFINE OUTPUT       PARAMETER oInitialID  AS INT64 NO-UNDO.
   DEFINE OUTPUT       PARAMETER oSurr       AS CHAR    NO-UNDO.
   DEFINE OUTPUT       PARAMETER oFlagFnd    AS LOGICAL NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioFlagBPack AS LOGICAL NO-UNDO.

   DEFINE VAR vDisable  AS CHAR           NO-UNDO.

   DEFINE BUFFER bPacket FOR Packet.
   DEFINE BUFFER Reference FOR Reference.
   DEFINE BUFFER Packet    FOR Packet.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("FindOpEntryByREF","iRecvFMT:" + GetNullStr(iRecvFMT)).    
   &ENDIF

   oFlagFnd = NO.
   FOR EACH  Reference WHERE
             Reference.op-date    =  iRecvDate
         AND Reference.Class-Code =  iRecvCls
         AND Reference.RefValue   =  iRecvFMT
             NO-LOCK,
       EACH  Packet WHERE
             Packet.PacketID      =  Reference.PacketID
         AND (IF shMode THEN Packet.Filial-Id =  shFilial ELSE YES)
             NO-LOCK:

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","Packet.PacketID:" + GetNullInt(Packet.PacketID) + 
                                         " Packet.mail-format:" + GetNullStr(Packet.mail-format)).    
      &ENDIF
      
      ASSIGN
         oFlagFnd   = YES
         oInitialID = Packet.PacketID
         oSurr      = "".

      {empty ttInitPack}
      FOR EACH bPacket WHERE   /* ��室�� �������� ������, �᫨ ���� */
               bPacket.ParentID =  Packet.PacketID NO-LOCK:
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","bPacket.PacketID:" + GetNullInt(bPacket.PacketID) + 
                                         " bPacket.mail-format:" + GetNullStr(bPacket.mail-format)).    
      &ENDIF
         CREATE ttInitPack.
         ASSIGN
            ttInitPack.PacketID = bPacket.PacketID
            ioFlagBPack          = YES
         .
      END.
      IF NOT CAN-FIND (FIRST ttInitPack NO-LOCK) THEN DO:
         CREATE ttInitPack.
         ASSIGN
            ttInitPack.PacketID = Packet.PacketID.
      END.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("FindOpEntryByREF","oInitialID:" + GetNullInt(oInitialID)).
      &ENDIF
   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDErrorPrepare:
   DEFINE INPUT  PARAMETER hWOP  AS handle   NO-UNDO.
   DEFINE INPUT  PARAMETER iPrf  AS CHAR     NO-UNDO.
   DEFINE OUTPUT PARAMETER oCls  AS CHAR     NO-UNDO.
   DEFINE OUTPUT PARAMETER oLstR AS LOGICAL  NO-UNDO. /* ���� �訡�� ���      */
   DEFINE OUTPUT PARAMETER oLstB AS LOGICAL  NO-UNDO. /* ���� �訡�� ���      */
   DEFINE OUTPUT PARAMETER oLst  AS CHAR     NO-UNDO. /* �� �訡��           */

   DEFINE VAR vItm AS INT64 NO-UNDO.
   DEFINE VAR vOne AS CHAR    NO-UNDO.
   DEFINE VAR vLst AS CHAR    NO-UNDO.
   DEFINE VAR vWrn AS CHAR    NO-UNDO.
   {profile ST071}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN RETURN ERROR ERROR-STATUS:get-message(1).

      ASSIGN
         oCls = "ErrorESID" /* hWOP:buffer-field("ErrorClass"):buffer-value */
         vLst = hWOP:buffer-field("ErrorList"):buffer-value
      NO-ERROR. {&ON-ERROR}

      oLst = "".                                 /* �� �訡��                */
      DO vItm = 1 TO NUM-ENTRIES(vLst):
         vOne = ENTRY(vItm,vLst).
         IF SUBSTRING(vOne,1,1) <> "b" THEN vOne = iPrf + vOne.
         {additem.i oLst vOne}
      END.

      RUN CheckErrorInst (INPUT  oCls,           /* �������� �� �訡�� � �।.*/
                          INPUT  oLst,
                          OUTPUT vWrn,
                          OUTPUT vLst).          /* �� �訡��                */
      oLstR = NO.
      oLstB = NO.
      DO vItm = 1 TO NUM-ENTRIES(vLst):          /* �������� �訡��           */
         vOne = ENTRY(vItm,vLst).
         IF vOne BEGINS "b" THEN oLstB = YES.    /* �訡�� ���                */
                            ELSE oLstR = YES.    /* �訡�� ���                */
      END.

      oLst = TRIM(vLst + "," + vWrn,",").
   END.
   {profile ST080}
   RETURN.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���।���� 㪠��⥫� �� �������� ᮮ�饭�� (䠩� PacketESID)              */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDGetMain:
   DEFINE INPUT-OUTPUT PARAMETER hWOP  AS handle   NO-UNDO.
   DEFINE OUTPUT       PARAMETER hMain AS handle   NO-UNDO.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vMain     AS CHAR           NO-UNDO.
   DEFINE VAR vWarning  AS CHAR           NO-UNDO.
   {profile ST081}
MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         hWOP  = hWOP:default-buffer-handle                /* ���⠭��       */
         vMain = hWOP:buffer-field("ExchMain"):buffer-value
         hMain = ObjectValueHandle(vMain)                  /* ��饥 ᮮ�饭�� */
      NO-ERROR. {&ON-ERROR}

      IF NOT valid-handle(hMain) THEN DO:
         RUN Fill-SysMes("","","","%s=" + vMain +
                                  "%s=" + hWOP:Name).
         UNDO MAIN, RETRY MAIN.
      END.

      vFlagSet = YES.
   END.
   {profile ST090}
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE ESIDExportREF:
   DEFINE INPUT PARAMETER  hOBJ AS handle NO-UNDO.
   DEFINE PARAMETER BUFFER bCode FOR Code.

   DEFINE VAR vFlagSet  AS LOGICAL INIT ? NO-UNDO.
   DEFINE VAR vSendREF  AS INT64        NO-UNDO.
   DEFINE VAR vLastREF  AS INT64        NO-UNDO.
   DEFINE VAR vStep        AS INT64          NO-UNDO.   
   DEFINE VAR vRetValue AS CHAR           NO-UNDO.
   DEFINE VAR vRecvID   AS CHAR           NO-UNDO.
   DEFINE VAR vSendID   AS CHAR           NO-UNDO.
   DEFINE VAR vOrderLim  AS CHAR           NO-UNDO.
   DEFINE VAR vOrderStep   AS CHAR           NO-UNDO.
   DEFINE VAR vOrderBeg    AS CHAR           NO-UNDO.
   DEFINE VAR vRange       AS CHAR           NO-UNDO.         
   DEFINE VAR vRef       AS CHAR           NO-UNDO.
   DEFINE VAR vOpKind      AS CHAR           NO-UNDO.
   DEFINE VAR vOK          AS LOGICAL        NO-UNDO.
   DEFINE VAR vFnd         AS LOGICAL        NO-UNDO.
   DEFINE VAR vMess        AS CHAR           NO-UNDO.    
   DEFINE VAR vHWork       AS HANDLE         NO-UNDO.
  

   DEFINE BUFFER Reference FOR Reference.

   {profile ST091}

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      IF RETRY THEN DO:
         vRetValue = ERROR-STATUS:get-message(1) + " " + RETURN-VALUE.
         LEAVE MAIN.
      END.

      RUN ParsFunc-���(OUTPUT mOpDate, OUTPUT mFake) NO-ERROR.

      IF NOT RLockCreate(bCode.Misc[{&RKC-BEGIN}]) THEN
         UNDO MAIN, RETRY MAIN.

      ASSIGN
         vOrderLim   = TRNSettingValue("", "OrderLim", "")  
         vOrderBeg   = TRNSettingValue("", "OrdBegin", "")
         vOrderStep  = TRNSettingValue("", "OrderStep", "")           
         vLastREF = hOBJ:buffer-field("__id"):buffer-value
         vOpKind     = GetBaseOpKind() 
         vStep       = IF {assigned vOrderStep} 
                       THEN INT64(vOrderStep) 
                       ELSE INT64(GetXattrEx(bCode.Misc[{&RKC-BEGIN}],
                                    "Step", "Initial"))         
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "1 vOrderLim:" + GetNullStr(vOrderLim) +
                                        " vOrderBeg:" + GetNullStr(vOrderBeg) + 
                                            " vStep:" + GetNullInt(vStep)).
      &ENDIF

      CREATE BUFFER vHWork FOR TABLE hObj:TABLE-HANDLE.

      IF {assigned vOrderBeg} AND {assigned vOrderLim} THEN
         vRange      = vOrderBeg + "-" + vOrderLim .
      ELSE DO:
         vRange = mOrderDef.
         IF {assigned vRange} THEN 
      ASSIGN
               vOrderBeg   = GetEntries(1,vRange,"-","")
               vOrderLim   = GetEntries(2,vRange,"-","").
      END.

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "2 vOrderLim:" + GetNullStr(vOrderLim) +
                                        " vOrderBeg:" + GetNullStr(vOrderBeg) + 
                                           " vRange:" + GetNullStr(vRange)).
      &ENDIF

      &IF DEFINED(IS-DEBUG) > 0 &THEN
      RUN dbgprint.p ("ESIDExportREF", "3 vLastREF:"  + GetNullInt(vLastREF)).
      &ENDIF

      IF {assigned vRange} THEN DO:
         IF vLastREF =  0 THEN
            vLastREF = 1. 
         IF mRezFlag THEN
            vSendREF = mRezREF + 1.
         ELSE DO:

            &IF DEFINED(IS-DEBUG) > 0 &THEN
            RUN dbgprint.p ("ESIDExportREF", 
                          "4 ReferenceGetLastEX:"  + 
                          GetNullStr(ReferenceGetLastEX(
                                          bCode.Misc[{&RKC-BEGIN}],
                                          gend-date,vRange)) + 
                          " vLastREF * vStep:" + GetNullInt(vLastREF * vStep)).
            &ENDIF

            vSendREF = INT64(ReferenceGetLastEX(bCode.Misc[{&RKC-BEGIN}],gend-date,vRange)) +
                       (vLastREF * vStep ) 
                       NO-ERROR.

            &IF DEFINED(IS-DEBUG) > 0 &THEN
            RUN dbgprint.p ("ESIDExportREF", "5 vSendREF:" + GetNullInt(vSendREF)).
            &ENDIF
         END.
      END.
      ELSE vSendREF = INT64(ReferenceGetLast(bCode.Misc[{&RKC-BEGIN}],gend-date)) + 
                      vLastREF + vStep .

      ASSIGN
         vSendREF  = 0 WHEN vSendREF =  ?
         vRecvID   = hOBJ:buffer-field("RecvID"):buffer-value
         vSendID   = hOBJ:buffer-field("SendID"):buffer-value
      NO-ERROR. {&ON-ERROR}

      ASSIGN
         hOBJ:buffer-field("RecvID"):buffer-value   = RKCGetRecvID() WHEN NOT {assigned vRecvID} /* ���  */
         hOBJ:buffer-field("SendID"):buffer-value   = RKCGetSendID() WHEN NOT {assigned vSendID} /* ���� */
         hOBJ:buffer-field("SendREF"):buffer-value  = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                                                           STRING(vSendREF))
         hOBJ:buffer-field("SendDate"):buffer-value = mOpDate
         vSendREF                                   = hOBJ:buffer-field("SendREF"):buffer-value
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF mRezFlag THEN DO:  /* �饬 �ࠧ� � १�ࢥ */
         vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                         STRING(vSendREF)).
         vFnd = YES.
         DO WHILE vFnd:
            vFnd = CAN-FIND(FIRST Reference WHERE
                                  Reference.op-date    =  mOpDate
                              AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                              AND Reference.RefValue   =  vRef).
            IF NOT vFnd THEN
               vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
            IF NOT vFnd THEN 
               LEAVE.
            vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).                       
            IF vSendREF >  mIntRezLim THEN 
               LEAVE.
         END.
         IF vSendREF >  mIntRezLim THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                                        "����ࢭ� �������� ����஢ ��������� �ᯮ�짮���." + 
                                        "��ᯮ�� ��⠭�����.").
            LEAVE MAIN.
         END.
         mRezREF = vSendREF. 
      END.
      ELSE DO:
                       /* ���� ��ப � �᭮���� ���ࢠ�� �� ���⨦���� ����� */ 
         IF vSendREF >= INT64(vOrderLim) THEN DO:
            vSendREF = IF {assigned vRange} 
                       THEN INT64(vOrderBeg)
                       ELSE 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],
                                            STRING(vSendREF)).
            vFnd = YES.
            DO WHILE vFnd:
               vFnd = CAN-FIND(FIRST Reference WHERE
                                     Reference.op-date    =  mOpDate
                                 AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                                 AND Reference.RefValue   =  vRef).
               IF NOT vFnd THEN
                  vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
               IF NOT vFnd THEN
                  LEAVE.
               vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
               IF vSendREF >  INT64(vOrderLim) THEN
                  LEAVE.
            END.
         END.
                                          /* ��४��祭�� �� १�� */
         IF vSendREF >  INT64(vOrderLim) THEN DO:

            IF NOT {assigned mOrderRez} THEN DO:  /* १�� �� ��।���� */
               RUN FillSysMes IN h_tmess ("", "", "-1",
                                          "�ॢ�襭�� �����⨬��� ��������� ���஭��� ����஢").
               LEAVE MAIN.
            END.
            /* �����஢�� १�ࢭ��� ��������� */
            vOK = LockObjectEx ("RLock",
                          bCode.Misc[{&RKC-BEGIN}] + "," + mOrderRez,
                          YES,
                          shFilial,
                          OUTPUT vMess) NO-ERROR.
            IF NOT vOK THEN DO:
               RUN Fill-SysMes IN h_tmess ("", "", "-1", SUBSTITUTE("����ࢭ� �������� &1 
                                          �������஢�� ��㣨� ���짮��⥫��",mOrderRez)). 
               LEAVE MAIN.
            END.

            vSendREF = INT64(GetEntries(1,mOrderRez,"-","")) NO-ERROR.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
            vFnd = YES.
            DO WHILE vFnd:
               vFnd = CAN-FIND(FIRST Reference WHERE
                                     Reference.op-date    =  mOpDate
                                 AND Reference.Class-Code =  bCode.Misc[{&RKC-BEGIN}]
                                 AND Reference.RefValue   =  vRef).
               IF NOT vFnd THEN
                  vFnd = vHWork:find-first("where SendREF EQ " + STRING(vSendREF)) NO-ERROR.
               IF NOT vFnd THEN
                  LEAVE.
            vSendREF = vSendREF + 1.
            vRef     = ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
               IF vSendREF >  mIntRezLim THEN
                  LEAVE.
         END.
            IF vSendREF >  mIntRezLim THEN DO:
               RUN Fill-SysMes IN h_tmess ("", "", "-1", 
                                           "����ࢭ� �������� ����஢ ��������� �ᯮ�짮���." + 
                                           "��ᯮ�� ��⠭�����.").
               LEAVE MAIN.
      END.

            mRezFlag = YES.
            IF {assigned mOrderMail} THEN
               RUN SendFMail IN h_email (mOrderMail,
                             "���ࢠ� ����஢ ��������� �ᯮ�짮���, ������⢮��� १�� . ",
                  SUBSTITUTE("��� �࠭���樨 &1  㪠�����  ���ࢠ� ����஢ ��������� �ᯮ�짮���. 
                              ��ᯮ�� �믮���� � �ᯮ�짮������ १�ࢭ�� ����஢",vOpKind),
                             "",
                             "")
               NO-ERROR. {&ON-ERROR}          
            RUN SetRezFlag IN h_rfrnc (YES).

         END. /* ��४��祭�� �� १�� */
      END.   /* NOT mRezFlag */

      hOBJ:buffer-field("SendREF"):buffer-value  =
            ReferenceFormatValue(bCode.Misc[{&RKC-BEGIN}],STRING(vSendREF)).
      vFlagSet = YES.
   END.

/* ��������஢��� ����� ����� �����, �.�.
   ����� Reference �� �� ᮧ���� */

   IF VALID-HANDLE(vHWork) THEN
      DELETE OBJECT vHWork NO-ERROR.

   {profile ST100}
   IF vFlagSet =  ?
      THEN RETURN ERROR vRetValue.
      ELSE RETURN.

END PROCEDURE.
/******************************************************************************/

PROCEDURE ESIDInitFind.
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iClassREF   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iREF        AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPacketID   AS INT64       NO-UNDO.

   DEFINE BUFFER Reference FOR  Reference.

   oPacketID = ?.
   FIND FIRST Reference WHERE Reference.op-date    =  iDate
                          AND Reference.class-code =  iClassREF
                          AND Reference.RefValue   =  iREF
      NO-LOCK NO-ERROR.
   IF AVAIL Reference THEN
      oPacketID = Reference.PacketID.

END PROCEDURE.


PROCEDURE ESIDExportInitial.
   ASSIGN
      mTxt274      = ""
      mInfoCode274 = ""
   .
END PROCEDURE.

PROCEDURE ESIDExportShutDown.

END PROCEDURE.

{ed503.pro}
{ed434.pro}

/******************************************************************************/
/* $LINTFILE='pp-esid.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.991+03:00' */
/*prosignThfsRzYwFAzpGPjaYNQbeA*/