/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: NALPL_ED.P
      Comment: ��ଠ ।���஢���� � ��ᬮ�� ४����⮢ ���������
               ���⥦�� �� ४����⠬ ���㬥��
   Parameters: iOpRid - 㪠��⥫� �� ���㬥��
               iView  - ⨯ ।���஢����
                  0 - ��ᬮ�� ४����⮢ (�᫨ ����)
                  1 - ��⭮� ।���஢���� (� �ᥬ� �஢�ઠ��)
                  2 - ��筮� ����� - ��易⥫�� ����� ���
         Uses:
      Used by:
      Created: 24.04.2003 16:09 SEMA
     Modified: 29.04.2003 17:21 SEMA     �� ��� 0015363 ᮧ����� 䠩��
     Modified: 05.05.2003 15:59 SEMA     �� ��� 0015363 ��ࠢ���� �㡫�஢���� ᨬ���� � ���� kpp-send, ��ࠢ����
                                         �訡�� �맮�� ��� � ��筮� ०���
     Modified: 05.05.2003 17:49 SEMA     �� ��� 0015363 ��ࠢ����� ������ ��ࠡ�⪨ �஢����
     Modified: 06.05.2003 14:21 SEMA     �� ��� 0015363 ��⠢���� �஢�ઠ ����� � ���� �३��
     Modified: 06.05.2003 17:54 SEMA     �� ��� 0015363 �������� ��।������ ��� �����⥫�, � ����⢥ �����㬥�⮢
                                         �ᯮ������ ������� pp-uni.prg
     Modified: 13.05.2003 11:57 SEMA
                                         �� ��� 0015363
                                         �) ⥯��� �� �㣠���� �� ������⢨� �஢���� (� ���⠭���� �� ����⥪� �����) �
                                         ���� ��室��
                                         �) ��ࠢ��� �롮� ��� �� "����஫� �� ����稥 ���祭��" � ��砥 㦥
                                         ��⠭��������� ���祭�� (࠭�� �ᥣ�� �ࠫ��� ��ࢮ� �� ᯨ᪠)
     Modified: 28.05.2003 15:01 KAVI     16806 �஢�ઠ �� ���祭�� � ���� 109
     Modified: 30.05.2003       ����    16506,17122
     Modified: 27.05.2003 15:54 kolal    ��ࠡ�⠭ ����஫� ���� 107 - ����� ������� "0".
                                         ��� 16512.
     Modified: 03.06.2003 11:07 kolal    ��ࠡ�⠭ ����஫� ���� 107 - ����� �������
                                         ����. ��� 16512.
     MODIFIED: 23/06/2003 KOSTIK         17375 ��������� ��।������ ��� �����⥫� �� ����७��� ���⥦�.
     Modified: 03.07.2003 15:02 kolal    ��।���஢�� ⥪�� ᮮ�饭�� � �������������
                                         �����. ��� 16454.
     Modified: 03.07.2003 15:04 kolal    16454
     Modified: 29.07.2003 ilvi           ��ࠢ���� ᨭ⠪��᪠� �訡��
     Modified: 04.08.2003 GORM           (�. 18327) � ��� ����⠢������ ���祭�� �� 㬮�砭�� ��� ४����⮢
                                         ����� (106) � ����� (110).
     Modified: 08.09.2003 18:30 YUSS     17992 - �᫨ ���� 101 �� ������,� �� signs �� ᮧ������
                                               - ��ࠢ���� � ������� ����஫� �� ����஫� �� ����稥 ���祭��
     Modified: 10.09.2003 18:30 YUSS     17252 - ���� 107 � 109
     Modified: 22.10.2003 14:53 rija     17194 - 1256 ��ࠡ���� ���� ���� (107) "�������� ��ਮ�".
     Modified: 23.10.2003 ilvi          (16790)- ��������� ����������� ���������� ���� (103) �� �ࠢ�筨��
     Modified: 12.11.2003 12:53 rija     17194 - 1256 ��ࠡ���� ���� ���� (107) "�������� ��ਮ�".
     Modified: 27.11.2003 18:53 rija     0017194 - 1256 ��ࠡ���� ���� ���� (107) "��������
                                         ��ਮ�".
     Modified: 01.12.2003 11:40 rija     17194 - 1256 ��ࠡ���� ���� ���� (107) "�������� ��ਮ�".
                                         ����������� � �������� ��������, ����������� ���� �����
     Modified: 26.12.2003 kraw (0017064) ���������� ��� � ����� �� ����������
     Modified: 29.01.2004 16:53 kolal    ���. ��ࠡ�⪠ ���� (110). ��� 17137.
     Modified: 03.03.2004 13:08 kolal    �� ������⢨� ४����� �����-����� �� ������
                                         �� ��⮪���� ����� ��������� �।��饥
                                         ���祭��. ��� 19106.
     Modified: 03.03.2004 13:13 kolal    19106
     Modified: 02.12.2004 kraw (0035669) �����祭�� ��� � 19 �� 20 ������
     Modified: 28.01.2005 kraw (0042270) ���������� �� "�106�_��⏫��"
     Modified: 20/04/2010 kraw (0100250) ValidateBCCFormat
     Modified: 01/12/2010 kraa (0101674) ������ ����஫� ᮮ⢥��⢨� 14 ᨬ���� ��� �� ᮮ⢥��⢨� ����  110 "��� ���⥦�"
     Modified: 10/12/2010 kraa (0136945) ��ࠢ���� ��࠭���� ���祭�� ���� KBK.
     Modified: 05/09/2011 kraw (0154217) �������� ��������� ४����⮢
     Modified: 23/12/2013 sasa (0202810) ����஫� ��易⥫쭮�� ���������� ४����⮢ 
                                         ���� �� ����� ���������� ���⥦�.
                                         ��������� ��楤�� �맮�� ��� �� 
                                         ���������� ४����⮢ ����
*/

/* ******** �室�� ��ࠬ���� ******** */
/* iOpRid - 㪠��⥫� �� ���㬥�� */
DEFINE INPUT  PARAMETER iOpRid AS RECID      NO-UNDO.
/* iView - ⨯ ।���஢����
                  0 - ��ᬮ�� ४����⮢ (�᫨ ����)
                  1 - ��易⥫�� ��ᬮ�� ४����⮢ (���� �᫨ ���)
                  2 - ��⭮� ।���஢���� (� �ᥬ� �஢�ઠ��)
                  3 - ��筮� ����� - ��易⥫�� ����� ���              */
DEFINE INPUT  PARAMETER iView  AS INT64    NO-UNDO.
/* �஢��� �३�� �� �࠭� */
DEFINE INPUT  PARAMETER level AS INT64    NO-UNDO.

DEFINE VARIABLE mEmpty AS LOGICAL NO-UNDO.
/*�� ���⃌�*/
DEF VAR mVarSett_ContGMP AS CHARACTER NO-UNDO.
/*�� bal-smev*/
DEF VAR mVarSett_BalSmev AS CHARACTER NO-UNDO.

/* ******** ������� ��६����� ******** */
&GLOBAL-DEFINE ViewOnly (iView =  0 OR iView =  1)
&GLOBAL-DEFINE ViewOnlyIfExist iView =  0
&GLOBAL-DEFINE ManualRun iView =  3

&GLOBAL-DEFINE NOT_IN_TRANS          YES
&GLOBAL-DEFINE ENABLE_EDIT_KPP       YES
&GLOBAL-DEFINE ALWAYS_SHOW_KPP_LIST  YES

&SCOPED-DEFINE TAX-ATTRS-BACKUP      "nalpl_ed_form_content"
&SCOPED-DEFINE ALLOWEDKBKSYM         "0,1,2,3,4,5,6,7,8,9,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,�,D,F,G,I,J,L,N,Q,R,S,U,V,W,Y,Z"

{globals.i}
{tmpobj.def}
{intrface.get op}
{intrface.get xclass}
{intrface.get cust}
{intrface.get strng}
{intrface.get tmess}
{pp-uni.var &FILE_sword_p=YES &tt-op-entry=YES} /* ��।������ ��६����� ��� pp-uni.prg */
{pp-uni.prg &NEW_1256=YES &tt-op-entry=YES}     /* ��楤��� ��।������ ��ࠬ��஢ ���⥦���� �ॡ������ */
{parsin.def}
{148n.i}

DEFINE BUFFER acct-b1 FOR acct.
DEFINE BUFFER DbAcct  FOR acct.

/* ���४����� Kpp-send ��� ���⥫�騪�                  (102) */
DEFINE  VARIABLE mKppSend      AS CHARACTER NO-UNDO.
/* ���४����� Kpp-rec  ��� �����⥫�.                  (103) */
DEFINE  VARIABLE mKppRec       AS CHARACTER NO-UNDO.

/* ���४����� "��ਮ�" ��� ����஫� �� �����䨪���� "���:��" (107) */
DEFINE VARIABLE mChkPokNP      AS LOGICAL   NO-UNDO.
DEFINE  VARIABLE mPokNP1       AS CHARACTER NO-UNDO.
/* ���४����� "��ਮ�" ��� 㪠����� ���� (107) */
DEFINE  VARIABLE mPokNP2       AS CHARACTER NO-UNDO.
DEFINE  VARIABLE mPokNP3       AS CHARACTER NO-UNDO.

DEFINE  VARIABLE mTypePokDD    AS CHARACTER NO-UNDO.
DEFINE  VARIABLE mFormatPokDD  AS CHARACTER NO-UNDO.

/* ��६���� ��� ������ ����஢�� ���祭�� */
DEFINE VARIABLE vVal        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mPokSTLabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mKBKLabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mPokOPLabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mPokNPLabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mPokTPLabel AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mBalNalog AS CHARACTER  NO-UNDO. /* ��᪠ ��⮢ �� ����஥筮�� ��ࠬ��� "bal-nalog" */
DEFINE VARIABLE mCRDNP    AS CHARACTER  NO-UNDO. /* ��᪠ ����� ���㬥�� �� ����஥筮�� ��ࠬ��� "�����" */
DEFINE VARIABLE mOsn106TP AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mIsFullControlInput AS LOGICAL    NO-UNDO. /* ����� ����஫�/����஫� �� ����稥 ���祭�� */
DEFINE VARIABLE mIsKBK              AS LOGICAL    NO-UNDO. 
DEFINE VARIABLE mSettFullCntrl      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mConstControl       AS LOGICAL    NO-UNDO. /* �ᥣ�� ����஫�஢��� ���� �� �� ��易⥫쭮��� ����� */
     
DEFINE VARIABLE mClassFieldList AS CHARACTER  NO-UNDO. /* ᯨ᮪ ��६�����, ��ࠡ��뢠���� �����䨪��ࠬ� */
DEFINE VARIABLE mClassCodeList AS CHARACTER  NO-UNDO. /* ᯨ᮪ �����䨪��஢ ��� ����஫� ���祭�� */

DEFINE VARIABLE mKppSendAllowSelect AS LOGICAL    NO-UNDO. /* ���������� �� �롮� ���祭�� � ���� ���-���⥫�騪� ? */
DEFINE VARIABLE mKppSendSensitive   AS LOGICAL    NO-UNDO. /* ����� �� ����� ।���஢��� mKppSend */
DEFINE VARIABLE mKppRecSensitive    AS LOGICAL    NO-UNDO. /* ����� �� ����� ।���஢��� mKppRec */
DEFINE VARIABLE mOKATOSensitive     AS LOGICAL    NO-UNDO. /* ����� �� ����� ।���஢��� mOKATO */
DEFINE VARIABLE mKppSendValues      AS CHARACTER  NO-UNDO. /* ᯨ᮪ ���祭�� ��� �롮� � ���� ���-���⥫�騪� */

DEFINE VARIABLE mKppRecAllowSelect  AS LOGICAL    NO-UNDO. /* ���������� �� �롮� ���祭�� � ���� ���-���⥫�騪� ? */
DEFINE VARIABLE mKppRecValues       AS CHARACTER  NO-UNDO. /* ᯨ᮪ ���祭�� ��� �롮� � ���� ���-���⥫�騪� */

DEFINE VARIABLE mS AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSelectKppRec AS CHAR NO-UNDO.

DEFINE VARIABLE mOKATOAllowSelect AS LOG  NO-UNDO. /*����������� ।���஢����
                                                     ���� �����*/
DEFINE VARIABLE mOKATOValues      AS CHAR NO-UNDO. /*ᯨ᮪ ���祭�� �����*/
DEFINE VARIABLE mAnotherFieldList AS CHAR NO-UNDO. /*�������⥫�� ᯨ᮪
                                                 �����, ��ࠡ��뢠���� �� F1 */
DEFINE VARIABLE mAcctDbFlag       AS LOG  NO-UNDO. /* = �� �᫨ ��� �����
                                                   ����ਡ�����᪨� */

DEFINE VARIABLE m106n_stat_plat AS CHARACTER NO-UNDO.

DEFINE VARIABLE mYesNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE mControl AS LOGICAL INIT YES NO-UNDO.
DEFINE VARIABLE mParKbkControl AS LOGICAL NO-UNDO.
DEFINE VARIABLE mChkAdm           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mBudPay           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mUIN              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mEIP              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAIP              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mUIP              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mKBKNalog         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBal401           AS CHARACTER  NO-UNDO.

DEFINE VARIABLE vAcct             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vCurr             AS CHARACTER  NO-UNDO.
/*�ਧ��� ��易⥫쭮�� ����� ४����⮢ ���� - ��� � ���*/
DEFINE VARIABLE mSMEVreq          AS LOGICAL    NO-UNDO.

DEFINE VARIABLE mIp               AS INT64      NO-UNDO.

DEFINE VARIABLE mFlNewFmt         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mShowPokTp        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mTPLab            AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mIsBudget         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mUseBackup        AS LOGICAL    NO-UNDO.

DEFINE BUFFER CODE  FOR code.
DEFINE BUFFER bcode FOR code.
DEFINE BUFFER ccode FOR CODE.
ASSIGN
   mVarSett_ContGMP = FGetSetting("�⠭���","���⃌�",?)
   mVarSett_BalSmev = FGetSetting("���","bal-smev",?)
   mParKbkControl   = fGetSetting("���", "���⊁��������", "") =  "��"
   mConstControl    = fGetSetting("���", "������", "") =  "��"
   mSettFullCntrl   = fGetSetting("���", "������ᥣ��", "") =  "��"
   mOsn106TP        = fGetSetting("���","��106��","")
   mKBKNalog        = FGetSetting("���","��������","")
   mBal401          = FGetSetting("���","bal-401","")
   mFlNewFmt        = NO
.

{empty TmpObj}
FOR EACH code WHERE code.class =  "���" AND 
                    code.misc[1] <> "��"
NO-LOCK:
   CREATE TmpObj.
   TmpObj.rid = RECID(code).    
END.


FUNCTION ValidateBCCFormat RETURN LOGICAL (INPUT vBCC AS CHARACTER)
   FORWARD.

/* �㭪�� ��� ����஫� ���४⭮�� �ଠ� ���� ����� */
FUNCTION ValidateOKTMO RETURN LOGICAL 
(INPUT ipOKATO AS CHARACTER,
 INPUT ipKBK   AS CHARACTER )
   FORWARD.

/* �㭪�� ��� ����஫� ��易⥫쭮�� ���������� ���� PokSt */
FUNCTION ValidatePokSt RETURN LOGICAL PRIVATE (INPUT iPokSt AS CHARACTER,
                                               INPUT iOpRowid AS ROWID)
   FORWARD.

/* �㭪�� ��� ����஫� ���४⭮�� �ଠ� ���� KppSend */
FUNCTION ValidateKppSend RETURN LOGICAL (INPUT ipKppSend AS CHARACTER)
   FORWARD.

/* �㭪�� ��� ����஫� ���४⭮�� �ଠ� ���� KppRec */
FUNCTION ValidateKppRec RETURN LOGICAL (INPUT ipKppRec AS CHARACTER)
   FORWARD.

/* �������� ���� PokOp*/
FUNCTION ValidatePokOp RETURN LOGICAL (INPUT ipPokOp AS CHARACTER)
   FORWARD.

/* �������� ���� ��ਮ� (107)*/
FUNCTION ValidatePokNP1 RETURN LOGICAL (INPUT ipPokNP1 AS CHARACTER)
   FORWARD.

/* �������� ���� ����� (108)*/
FUNCTION ValidatePokND RETURN LOGICAL (INPUT ipPokND AS CHARACTER)
   FORWARD.

/* �������� ���� ��� (109)*/
FUNCTION ValidatePokDD RETURN LOGICAL (INPUT ipPokDD AS CHARACTER)
   FORWARD.

/* �������� ���� ��� (110)*/
FUNCTION ValidatePokTP RETURN LOGICAL (INPUT ipPokTP AS CHARACTER)
   FORWARD.


{ kppproc.i
    &BUF-OP-ENTRY = op-entry
    &BUF-OP       = op
}

{conf_op.i}
{kpp_rec.i}

ASSIGN
   mClassFieldList   = "mPokST,mKBK,mPokOp,mPokNP1,mPokTP"
   mClassCodeList    = "�����,���,���:��,���:��,���:��"
   mAnotherFieldList = "mKppSend,mKppRec,mOKATO"
   mTypePokDD        = GetXAttrEx("opb",'�����','Data-Type')
   mFormatPokDD      = GetXAttrEx("opb",'�����','Data-Format')
.

IF NOT (mFormatPokDD = "" OR
        mFormatPokDD = "x(10)" OR
        mFormatPokDD = "xx.xx.xxxx" ) THEN
   MESSAGE "��ଠ� ���� '��� (109)' : " + mFormatPokDD SKIP
           "������� �� x(10)         " SKIP (1)
           "��ଠ� ������ ����:      " SKIP
           " - �����                " SKIP
           " - x(10)                 " SKIP
           " - xx.xx.xxxx            " SKIP (1)
           "������� �ଠ� ४�����: " SKIP
           "����� - ������⥫� ���� ���㬥�� " SKIP
           "� ����奬� opb - �����ᮢ� ���㬥���" SKIP
   VIEW-AS ALERT-BOX INFORMATION.
mTPLab =  "      ��� (110):".
/* ******** ������� ��� ******** */
FORM
   "����� ��⠢�⥫� (101):" mPokST FORMAT "xx" 
   HELP "������ F1 ��� �롮� �� �����䨪���" 
   mPokSTLabel AT 39 FORMAT "x(40)" SKIP(1)
   "   ��� ���⥫�騪� (102):" mKppSend FORMAT "x(9)" HELP "F1 ��� �롮� �� ᯨ᪠" SKIP
   "    ��� �����⥫� (103):" mKppRec  FORMAT "x(9)" SKIP(1)
   "      ��� (104):" mKBK FORMAT "x(20)" HELP "������ F1 ��� �롮� �� �����䨪���" mKBKLabel   AT 39 FORMAT "x(40)" SKIP
   "    ����� (105):" mOKATO FORMAT "X(11)" SKIP
   "�᭮����� (106):" mPokOP FORMAT "xx" HELP "������ F1 ��� �롮� �� �����䨪���" mPokOPLabel AT 39 FORMAT "x(40)" SKIP
   "   ��ਮ� (107):" mPokNP1 FORMAT "x(10)" HELP "������ F1 ��� �롮� �� �����䨪���"
                      mPokNPLabel AT 39 FORMAT "x(40)" SKIP
   "    ����� (108):" mPokND FORMAT "x(15)" SKIP
   "     ��� (109):" mPokDD /* �ଠ� ������ �� �ଠ� ���.४����� */ SKIP
   mTPLab mPokTP AT 18 FORMAT "xx" HELP 
   "������ F1 ��� �롮� �� �����䨪���" 
   mPokTPLabel AT 39 FORMAT "x(40)" SKIP(3)
   "F3 ��������� ����"
   WITH FRAME NalPr-frame
   TITLE COLOR bright-white "[ ��������� ��������� ���⥦�� ]"
   ROW level
   CENTERED  NO-LABELS
   OVERLAY .

ASSIGN 
   mPokDD:FORMAT = IF mFormatPokDD = "xx.xx.xxxx" THEN mFormatPokDD ELSE "x(10)"
   mTpLab:FORMAT = "x(20)".

FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
IF AVAIL op THEN DO:
   ASSIGN 
      mShowPokTp = op.op-date <  DATE(FGetSetting("���","���3844�","")) 
   NO-ERROR.
   RUN IsBudgetPayment IN h_op (op.op, OUTPUT mIsBudget).
END.


ON "F3" OF FRAME NalPr-Frame ANYWHERE DO:

   RUN SetSysConf IN h_base ("KPP-send", mKPPsend).
   RUN SetSysConf IN h_base ("KBK", mKBK).
   ASSIGN
      mPokSt
   .
   IF {assigned mPokSt} AND 
      mPokSt <> "0" AND
      NOT {assigned mUIN}
   THEN
      mUIN = "0".
   RUN nalpl_smev.p (INPUT op.op,
                     INPUT (IF AVAIL(op-entry) THEN op-entry.op-entry ELSE ?),
                     INPUT {&ViewOnly},
                     INPUT mSMEVreq,
                     INPUT-OUTPUT mUIN, 
                     INPUT-OUTPUT mEIP, 
                     INPUT-OUTPUT mAIP, 
                     INPUT-OUTPUT mUIP).
                                                    
   RUN DeleteOldDataProtocol IN h_base ("KPP-send").
   RUN DeleteOldDataProtocol IN h_base ("KBK").
   RETURN NO-APPLY.
END.

ON "GO" OF FRAME NalPr-Frame DO:

   DEFINE VARIABLE vS      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI      AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrHdl AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vErr    AS LOGICAL NO-UNDO.

   IF NOT {&ViewOnly} THEN DO:
    &IF DEFINED(SESSION-REMOTE)  &THEN 
      IF NOT {&ManualRun} THEN DO:
         IF FRAME-FIELD =  "mPokST"
            AND NOT mIsKBK
            AND FRAME-VALUE =  "" THEN
         DO:
            IF mConstControl THEN
                mEmpty = NO.         
            ELSE
               MESSAGE "�� ����⢨⥫쭮 ��� ��⠢��� �� ���� ����묨?"
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   UPDATE mEmpty.
         END.
         ELSE
            mEmpty = NO.
      END.
      &ENDIF 
      
      IF NOT mEmpty THEN
      DO:
         IF mUseBackup THEN
            RUN BackupTaxAttrs IN THIS-PROCEDURE (op.op).

         IF NOT ValidatePokST(mPokST:SCREEN-VALUE,ROWID(op)) THEN RETURN NO-APPLY.
         /*�஢�ઠ ४����� ���*/
         IF {assigned mPokSt} AND 
            mPokSt <> "0" AND
            NOT {assigned mUIN}
         THEN
            mUIN = "0".
         IF TRIM(mUin) <> "" THEN 
         DO:
            IF (LENGTH(TRIM(mUin)) <> 20 AND 
                LENGTH(TRIM(mUin)) <> 25 AND 
               TRIM(mUin) <> "0") THEN
            DO:
               RUN Fill-SysMes IN h_tmess
                               ('','','','����୮ ��������� ���� ���, ���� ������ ᮤ�ঠ�� 20 ' +
                                         '��� 25 ᨬ����� ���� ����� ���祭�� "0"!' + CHR(10) +
                                         '�஢���� ���������� ४����⮢ �� F3').
               RETURN NO-APPLY.
            END.
            /*�஢�ઠ ���祢��� ࠧ�鸞 ��� �� ���瓈� = �� � ����������� ���� (101)*/
            IF GetCode("���:�����","���瓈�") =  "��" 
               AND {assigned mPokSt} 
               AND mPokSt <> "0" 
               AND mPokSt <> "00" 
               AND mUIN   <> "0" THEN
            DO:
               IF NOT checkUINKey(mUIN) THEN
               DO:
                  RUN Fill-SysMes IN h_tmess
                               ('','','','�訡�� ���������� ����஫쭮�� ࠧ�鸞 ���!~n' + 
                                         '�஢���� ���������� ४����⮢ �� F3').
                  RETURN NO-APPLY.
               END.
            END.
         END.
         /**/
         ASSIGN
            mPokNP1
            mPokNP = mPokNP1
            mPokDD
         .
   
         vErrHdl = ?.
         RUN CheckDown ((FRAME NalPr-Frame:HANDLE), OUTPUT vErrHdl).
         IF VALID-HANDLE(vErrHdl) THEN DO:
            APPLY "ENTRY" TO vErrHdl.
            RETURN NO-APPLY.
         END.
                  
         IF mPokNP <> "0"
         THEN DO:
            IF CAN-DO(fGetSetting ("���", "�����101��", ""),mPokSt) THEN DO:                     
               IF LENGTH(mPokNP) <> 8 THEN DO: 
                  MESSAGE "����� ���� 107 ������ ���� ��ᥬ� ���!"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  APPLY "ENTRY" TO mPokNP1.
                  RETURN NO-APPLY.
               END.
               vI = INT64(mPokNP) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE "���� 107 ������ ᮤ�ঠ�� ⮫쪮 ����!"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  APPLY "ENTRY" TO mPokNP1.
                  RETURN NO-APPLY.               
               END.
            END.
            ELSE DO:
               vS = GetCode ("���:��", ENTRY(1,mPokNp1,".")).
               mChkPokNP = INDEX(mPokNp1,".") > 0.
               IF mChkPokNP THEN DO:
                  ASSIGN
                     mPokNP2 = ENTRY(2,mPokNp1,".")
                     mPokNP3 = ""
                     mPokNP3 = ENTRY(3,mPokNp1,".") WHEN NUM-ENTRIES(mPokNp1,".") >  2
                  .
               END.
               IF vS =  ? THEN
               DO: /* ���祭�� ���� ��� � �����䨪��� - �� ������ ���� ���*/
                  IF mChkPokNP THEN DO:
                     vS = STRING(DATE(mPokNP1),"99.99.9999") NO-ERROR.
                     IF ERROR-STATUS:ERROR
                        OR vS =  ""
                        OR vS =  ? THEN
                     DO:
                        MESSAGE "���ࠢ��쭮 ������� ��� ���������� ��ਮ�� !"
                           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                        APPLY "ENTRY" TO mPokNP1.
                        RETURN NO-APPLY.
                     END.
                  END.
               END.
               ELSE
               DO:
   
                  vI = INT64(mPokNP2) NO-ERROR.
                  IF ERROR-STATUS:ERROR
                     OR vI < 0
                     OR vI > INT64(vS) THEN
                  DO:
                     MESSAGE "���祭�� ������ ���� �� 00 �� " + STRING(INT64(vS),"99")
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     APPLY "ENTRY" TO mPokNP1.
                     RETURN NO-APPLY.
                  END.
   
                  IF LENGTH(mPokNP3) <> 4 THEN
                  DO:
                     MESSAGE "��ଠ� ���� ������ ���� ����"
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     APPLY "ENTRY" TO mPokNP1.
                     RETURN NO-APPLY.
                  END.
   
                  vI = INT64(mPokNP3) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN
                  DO:
                     MESSAGE "���ࠢ��쭮 ������ ���"
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     APPLY "ENTRY" TO mPokNP1.
                     RETURN NO-APPLY.
                  END.
               END.
            END.
            IF mPokDD <> "0" AND
               {assigned mPokDD} AND
               NOT {&ManualRun}
            THEN DO:

               ASSIGN
                  vS = STRING(DATE(mPokDD),"99.99.9999") NO-ERROR.
               
               IF ERROR-STATUS:ERROR
                  OR vS =  ""
                  OR vS =  ? THEN
               DO:
                  MESSAGE "���ࠢ��쭮 ������� ���" skip
                          "��ଠ� ���� ������ ���� ��.��.����"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  APPLY "ENTRY" TO mPokDD.
                  RETURN NO-APPLY.
               END.
               ELSE
                  mPokDD = vS.
            END.
   
            IF NOT ValidateBCCFormat(mKBK:SCREEN-VALUE) THEN
            DO:
               MESSAGE "���ࠢ���� �ଠ� ���"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               APPLY "ENTRY" TO mKBK.
               RETURN NO-APPLY.
            END.
            IF CAN-FIND(FIRST code WHERE code.class =  "���" AND
                                         code.code  =  mKBK:SCREEN-VALUE AND
                                         code.misc[1] =  "��")
               AND ({assigned mPokST} OR mConstControl OR 
                  mKBK:SCREEN-VALUE > "":U OR mKBK:SCREEN-VALUE = ?)
            THEN DO:
                MESSAGE "��� ��� " + mKBK:SCREEN-VALUE + " �⬥祭 ��� ���������" SKIP "�த������?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.
                IF NOT mYesNo THEN  DO:
                   APPLY "ENTRY" TO mKBK.
                   RETURN NO-APPLY.
                END.
                
            END.
            FIND FIRST code 
               WHERE code.class =  "���" AND
                     code.code  =  mKBK:SCREEN-VALUE AND
                     code.misc[1] <> "��" NO-LOCK NO-ERROR.
            IF AVAIL code AND code.val <> "" THEN DO:
               IF NOT CAN-DO(CODE.val,mPokTP:SCREEN-VALUE) AND mShowPokTp 
               THEN DO:
                  MESSAGE "���祭�� ���� ��� �� ᮮ⢥���� ���祭�� ���" SKIP "��� ����稭� " + mKBK:SCREEN-VALUE + "!" 
                     VIEW-AS ALERT-BOX.
                  APPLY "ENTRY" TO mPokTP.
                  RETURN NO-APPLY.
               END.
            END.
            ASSIGN
               mPokST
               mKppSend
               mKppRec
               mKBK
               mOKATO
               mPokOP
               mPokND
               mPokTP
               mPokNP = mPokNP1
            .
            IF (mPokTP = ? OR mPokTP = '') AND mFlNewFmt AND mShowPokTp
            THEN mPokTP = '0'.            
            IF NOT {assigned mKppSend} 
               AND ({assigned mPokST} OR mConstControl) 
               /*AND CAN-DO(mBalNalog, ENTRY(1, PoAcct))*/ THEN 
            DO:
               MESSAGE "�� ��������� ���� ""��� ���⥫�騪� (102)"".~n" 
                  VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO mKppSend.
               RETURN NO-APPLY.
            END.
            IF NOT {assigned mKppRec} 
               AND ({assigned mPokST} OR mConstControl) 
               /*AND CAN-DO(mBalNalog, ENTRY(1, PoAcct))*/ THEN
            DO:
               MESSAGE "�� ��������� ���� ""��� �����⥫� (103)"".~n"  
                  VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO mKppRec.
               RETURN NO-APPLY.
            END.
    
            /* �᫨ �� �� ����� � ����� - � �� ���� ������ ���� ��������� */
            IF NOT {&ManualRun} THEN DO:
               vS = "".
         
               IF mPokST =  ? OR
                  mPokST =  "" THEN DO:
                  vS = vS + "�� ��������� ���� ""����� ��⠢�⥫� (101)"".~n".
               END.
               /*
               IF ((mKppSend    EQ ? OR
                    mKppSend    EQ "") AND
                   mKppSend:SENSITIVE) THEN DO:
                  vS = vS + "�� ��������� ���� ""��� ���⥫�騪� (102)"".~n".
               END.
               IF mKppRec       EQ ? OR
                  mKppRec       EQ ""  THEN DO:
                  vS = vS + "�� ��������� ���� ""��� �����⥫� (103)"".~n".
               END.
               */
               IF ((mKBK        =  ? OR
                    mKBK        =  "") AND
                   mKBK:SENSITIVE) THEN DO:
                  vS = vS + "�� ��������� ���� ""��� (104)"".~n".
               END.
               IF ((mOKATO =  ? OR
                    mOKATO =  "") AND
                   mOKATO:SENSITIVE) THEN DO:
                  vS = vS + "�� ��������� ���� ""����� (105)"".~n".
               END.
               IF mPokOP        =  ? OR
                  mPokOP        =  "" THEN DO:
                  vS = vS + "�� ��������� ���� ""�᭮����� (106)"".~n".
               END.
               IF mPokNP        =  ? OR
                  mPokNP        =  "" THEN DO:
                  vS = vS + "�� ��������� ���� ""��ਮ� (107)"".~n".
               END.
               IF      NOT mBudPay
                  AND (mPokND        =  ? OR 
                       mPokND        =  "") THEN DO:
                  vS = vS + "�� ��������� ���� ""����� (108)"".~n".
               END.
               IF mPokDD        =  ? OR
                  mPokDD        =  "" THEN DO:
                  vS = vS + "�� ��������� ���� ""��� (109)"".~n".
               END.
               IF (mPokTP        =  ? OR
                   mPokTP        =  "") AND (NOT mFlNewFmt) AND mShowPokTp
               THEN DO:
                  vS = vS + "�� ��������� ���� ""��� (110)"".~n".
               END.
               IF vS <> "" THEN DO:
                  MESSAGE vS "�� ���� ������ ���� ���������."
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         IF {assigned mPokSt} OR mConstControl THEN DO:
           /* �������⥫�� �஢�ન �������� ४����⮢ */
           RUN chksgnnaldoc.p(mPokST,
                              mKBK,
                              mPokOP,
                              mPokNP1,
                              mPokND,
                              mPokDD,
                              op.ben-acct,
                              OUTPUT vErr).
           IF vErr THEN RETURN NO-APPLY.
         END.
      END.
   END.
END.

ON LEAVE OF FRAME NalPr-Frame ANYWHERE DO:

   DEFINE VARIABLE vErr AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vMonthLimit AS INT64 NO-UNDO.

   IF LOOKUP(THIS-PROCEDURE:FILE-NAME,PROGRAM-NAME(2)," ") = 0
   OR KEYLABEL(LASTKEY) = "ESC"
   THEN RETURN.

   &IF DEFINED(SESSION-REMOTE) &THEN
      IF LASTKEY = KEYCODE("F1") THEN RETURN.
   &ENDIF
   IF NOT {&ViewOnly} THEN DO:

      /* �᫨ �� �� ����� � ����� - � �� ���� ������ ���� ��������� */
      &IF DEFINED(SESSION-REMOTE) = 0 &THEN
      IF NOT {&ManualRun} THEN DO:
         IF FRAME-FIELD =  "mPokST"
            AND NOT mIsKBK
            AND FRAME-VALUE =  "" THEN
         DO:
            pick-value = ?.
            IF mConstControl THEN
                mEmpty = NO.         
            ELSE DO:
               {messmenu                                                        
                  &text    = "�� ����⢨⥫쭮 ��� ��⠢��� �� ���� ����묨?"
                  &choices = "��,���"                     
                  &update  = 2                                                
               }
            END.
            IF pick-value =  "1" THEN
            DO:
               mEmpty = YES.
               APPLY "GO".
               RETURN.
            END.
            ELSE DO:
               mEmpty = NO.
               RETURN NO-APPLY.
            END.
         END.
         ELSE
            mEmpty = NO.
    
      END.
      ELSE
         IF NOT mIsKBK AND NOT CAN-DO(mBalNalog, ENTRY(1, PoAcct)) THEN
         DO WITH FRAME NalPr-Frame:
            DEFINE VARIABLE vSens AS LOGICAL NO-UNDO.
            vSens = {assigned mPokST:SCREEN-VALUE} OR mConstControl.
            IF NOT vSens THEN
               ASSIGN
                  mKBK:SCREEN-VALUE    = ""
                  mOKATO:SCREEN-VALUE  = ?
                  mPokOp:SCREEN-VALUE  = ""
                  mPokNP1:SCREEN-VALUE = ""
                  mPokND:SCREEN-VALUE  = ""
                  mPokDD:SCREEN-VALUE  = ?
                  mPokTP:SCREEN-VALUE  = ?
                  vSens                = NO
                  .
            ASSIGN
               mKBK:READ-ONLY       = NOT vSens
               mKBK:SENSITIVE       = vSens
               mOKATO:READ-ONLY     = NOT vSens
               mOKATO:SENSITIVE     = vSens
               mPokOp:READ-ONLY     = NOT vSens
               mPokOp:SENSITIVE     = vSens
               mPokNP1:READ-ONLY    = NOT vSens
               mPokNP1:SENSITIVE    = vSens
               mPokND:READ-ONLY     = NOT vSens
               mPokND:SENSITIVE     = vSens
               mPokDD:READ-ONLY     = NOT vSens
               mPokDD:SENSITIVE     = vSens
               mPokTP:READ-ONLY     = NOT vSens
               mPokTP:SENSITIVE     = vSens AND mShowPokTp
               .
         END.
      &ENDIF
    
      ASSIGN
         mPokST
         .
         
      CASE SELF:NAME: 
         WHEN "mPokST" THEN
            DO:
               IF NOT ValidatePokST(mPokST:SCREEN-VALUE,ROWID(op)) THEN
               DO:
                  APPLY "ENTRY" TO mPokST.
                  RETURN NO-APPLY.
               END.
            END.
         WHEN "mKppSend" THEN
            IF NOT ValidateKppSend(mKppSend:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mKppSend.
               RETURN NO-APPLY.
            END.
         WHEN "mKppRec" THEN
            IF NOT ValidateKppRec(mKppRec:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mKppRec.
               RETURN NO-APPLY.
            END.
         WHEN "mKBK" THEN
            IF NOT ValidateBCCFormat(mKBK:SCREEN-VALUE) THEN
            DO:
               RUN Fill-SysMes IN h_tmess("","","-1","���ࠢ���� �ଠ� ���� 104").
               APPLY "ENTRY" TO mKBK.
               RETURN NO-APPLY.
            END.
         WHEN "mOKATO" THEN
            IF NOT ValidateOKTMO(mOKATO:SCREEN-VALUE,mKBK:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mOKATO.
               RETURN NO-APPLY.
            END.
         WHEN "mPokOp" THEN
            IF NOT ValidatePokOp(mPokOp:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mPokOp.
               RETURN NO-APPLY.
            END.
         WHEN "mPokNP1" THEN
            IF NOT ValidatePokNP1(mPokNP1:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mPokNP1.
               RETURN NO-APPLY.
            END.
         WHEN "mPokND" THEN
            IF NOT ValidatePokND(mPokND:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mPokND.
               RETURN NO-APPLY.
            END.
         WHEN "mPokDD" THEN DO:
            IF NOT ValidatePokDD(mPokDD:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mPokDD.
               RETURN NO-APPLY.
            END.
         END.
         WHEN "mPokTP" THEN DO:
            IF Trim(mPokTP:SCREEN-VALUE) <> "0" AND mFlNewFmt AND mShowPokTp
            THEN DO:
              {messmenu                                                        
               &text    = "��������! � ᮮ⢥��⢨� � �ਪ���� 126� ���� (110) �᪫�祭�. ���� ������ ���� ��������� ���祭���: '0'. ����� ��㣮� ���祭�� � ����?"
               &choices = "��,���"                     
               &update  = 1                                                
               }
               IF pick-value =  "2" THEN         
               DO:
                APPLY "ENTRY" TO mPokTP.
                RETURN NO-APPLY.
               END.
            END.
            IF NOT ValidatePokTP(mPokTP:SCREEN-VALUE) THEN
            DO:
               APPLY "ENTRY" TO mPokTP.
               RETURN NO-APPLY.
            END.
         END.
      END CASE.

      ASSIGN
         mPokST
         mKppSend
         mKppRec
         mKBK
         mOKATO
         mPokOP
         mPokNP1
         mPokND
         mPokDD
         mPokTP
      .
    
      IF (mPokTP = ? OR mPokTP = '') AND mFlNewFmt AND mShowPokTp
      THEN mPokTP = '0'.    
      &IF DEFINED(SESSION-REMOTE) &THEN
      RUN FillLabelFields.
      RUN DispFrame.
      &ELSE
      RUN CheckField (FRAME-FIELD, FRAME-VALUE, OUTPUT vErr).
      IF vErr THEN RETURN NO-APPLY.
      &ENDIF
   END.
END.

PROCEDURE CheckDown:
   DEFINE INPUT  PARAMETER iHandle AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrHdl AS HANDLE NO-UNDO.
   DEFINE VAR vNext  AS HANDLE  NO-UNDO.
   DEFINE VAR vChild AS HANDLE  NO-UNDO.
   DEFINE VAR vErr   AS LOGICAL NO-UNDO.

   vNext = iHandle.
   DO WHILE VALID-HANDLE(vNext):

      IF vNext:TYPE <> "FRAME" THEN DO:
         IF  CAN-QUERY(vNext,"SENSITIVE")
         AND CAN-QUERY(vNext,"VISIBLE")
         AND CAN-QUERY(vNext,"NAME")
         AND CAN-QUERY(vNext,"SCREEN-VALUE")
         AND vNext:SENSITIVE = YES
         AND vNext:VISIBLE   = YES THEN DO:
            RUN CheckField (vNext:NAME, vNext:SCREEN-VALUE, OUTPUT vErr).
            IF vErr THEN DO:
               oErrHdl = vNext.
               RETURN.
            END.
         END.
      END.
      
      vChild = vNext:FIRST-CHILD NO-ERROR.
      IF VALID-HANDLE(vChild) THEN DO:
         RUN CheckDown (vChild, OUTPUT oErrHdl).
         IF VALID-HANDLE(oErrHdl)
         THEN RETURN.
      END.

      IF vNext:TYPE = "FRAME" THEN LEAVE.
      vNext = vNext:NEXT-SIBLING.
   END.

   RETURN.
END PROCEDURE.

PROCEDURE CheckField:
   DEFINE INPUT  PARAMETER iField AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iValue AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER vErr   AS LOGICAL INIT YES NO-UNDO.
   DEFINE VARIABLE vI         AS INT64    NO-UNDO.
   DEFINE VARIABLE vS         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vXattrName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vErrMsg    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFErr      AS LOGICAL    NO-UNDO.
   DO WITH FRAME NalPr-Frame:
      vI = LOOKUP(iField, mClassFieldList).
      IF vI > 0 THEN DO:
         vval = iValue.
         /*
         */
         IF iField =  "mPokNP1" AND CAN-DO(mOsn106TP,mPokOP)
                                    /*CAN-DO(fGetSetting ("���", "�����101��", ""),mPokSt)*/
         THEN DO:
            /* IF mPokNP1 <> '0'  THEN DO: */
            /*
                  IF LENGTH(mPokNP1) NE 8 THEN DO: 
                     MESSAGE "����� ���� 107 ������ ���� ��ᥬ� ���!"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     RETURN.
                  END.
                  vI = INT64(mPokNP1) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN DO:
                     MESSAGE "���� 107 ������ ᮤ�ঠ�� ⮫쪮 ����!"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     RETURN.
                  END.
            */
            /* END. */
         END.
         ELSE DO:
            IF iField = "mPokNP1" THEN DO:
            /*
               vS = GetCode (ENTRY(vI, mClassCodeList), ENTRY(1,vVal,".")).

               IF mPokNP1 <> '0' AND NUM-ENTRIES(vVal,".") NE 3 THEN DO:
                   MESSAGE "���� 107 ������ ����� �ଠ� XX.XX.XXXX !"
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                   RETURN.
               END.
               mChkPokNP = INDEX(vVal,".") > 0.
               IF mChkPokNP THEN ASSIGN
                  mPokNP2 = ENTRY(2,vVal,".")
                  mPokNP3 = ""
                  mPokNP3 = ENTRY(3,vVal,".") WHEN NUM-ENTRIES(vVal,".") GT 2
               .
            */
            END.
            ELSE
               vS = GetCode (ENTRY(vI, mClassCodeList), vVal).
         
            /* �᫨ �� 1-� ���� ���� 107 "�������� ��ਮ�", � ��� �����
               ᮤ�ঠ�� 0 ��� �᫮ */
            IF iField = "mPokNP1" THEN DO:
            /*
               IF  INDEX(vVal,".") > 0
               AND vS EQ ? THEN DO:
                  vI = INT64(ENTRY(1,mPokNP1,".")) NO-ERROR.
                  IF ERROR-STATUS:ERROR
                  OR vI < 0
                  OR vI > 31
                  THEN DO:
                     MESSAGE "���祭��� ���� ����� ���� ���� (������ 0)" SKIP "��� ���祭�� �����䨪��� '���:��' !" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     RETURN.
                  END.
               END.
            */
            END.        
            /* �᫨ "����� ����஫�" � ᢥ�塞 ������ ���� c ������� */
            ELSE IF mIsFullControlInput THEN DO:
               IF vS =  ? AND mControl THEN DO: /* ���祭�� ���� ��� � �����䨪��� */
                  MESSAGE "��� ⠪��� ���祭�� � �����䨪���: '" + ENTRY(vI, mClassCodeList) + "' !" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  RETURN.
               END.
            END.
         END.
      END.

      IF iField =  "mKBK" AND mChkAdm THEN DO:

         IF {assigned mKBK} AND {assigned op.inn} AND {assigned mKPPRec} THEN DO:

            FIND FIRST code WHERE
                       CODE.class =  "����"
                   AND CODE.name  =  op.inn
                   AND CODE.val   =  mKPPRec
                   AND CODE.DESCRIPTION[1] =  mKBK
                       NO-LOCK NO-ERROR.

            IF NOT AVAIL(CODE) THEN DO:
               MESSAGE "��������! ���, ��� � ��� �����⥫� ���������� � �ࠢ�筨�� ����������� ��室�� ���. �த������ ����?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.
               IF NOT mYesNo THEN RETURN.
            END.
         END.
      END.
      IF iField =  "mOKATO" AND mChkAdm THEN DO:

         FIND FIRST code WHERE
                    CODE.class =  "����"
                AND CODE.name  =  op.inn
                AND CODE.val   =  mKPPRec
                AND CODE.DESCRIPTION[1] =  mKBK
                AND CODE.description[2] =  "0000000000"
                    NO-LOCK NO-ERROR.

         FIND FIRST bcode WHERE
                    bCODE.class =  "����"
                AND bCODE.name  =  op.inn
                AND bCODE.val   =  mKPPRec
                AND bCODE.DESCRIPTION[1] =  mKBK
                AND bCODE.description[2] <> "0000000000"
                    NO-LOCK NO-ERROR.

         IF AVAIL(CODE) AND NOT AVAIL(bcode) THEN DO: 

            FIND FIRST cCODE WHERE
                       cCode.class =  "��������" 
                   AND cCode.code  =  mOKATO
                       NO-LOCK NO-ERROR.
            IF NOT AVAIL(cCode) THEN DO:
               MESSAGE "��� ⠪��� ���祭�� � �����䨪��� ��������: '" + mOKATO + "' !" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               RETURN.
            END.
         END.
         ELSE DO:
            IF NOT AVAIL(CODE) AND AVAIL(bCode) THEN DO:

               FIND FIRST CODE WHERE
                          CODE.class          =  "����"
                      AND CODE.name           =  op.inn
                      AND CODE.val            =  mKPPRec
                      AND CODE.DESCRIPTION[1] =  mKBK
                      AND CODE.description[2] =  mOKATO
                          NO-LOCK NO-ERROR.

               IF NOT AVAIL(cCode) THEN DO:
                  MESSAGE "��� ⠪��� ���祭�� � �����䨪��� ����: ���=" + op.inn + ", ���=" + mKPPRec + ", ���=" + 
                       mKBK + ", OKATO=" + mOKATO + "!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  RETURN.
               END.
            END.
            ELSE IF avail(code) AND avail(bcode) THEN DO:
               FIND FIRST CODE WHERE
                          CODE.class          =  "����"
                      AND CODE.name           =  op.inn
                      AND CODE.val            =  mKPPRec
                      AND CODE.DESCRIPTION[1] =  mKBK
                      AND CODE.description[2] =  mOKATO
                          NO-LOCK NO-ERROR.
               IF NOT AVAIL(CODE) THEN 
               FIND FIRST CODE WHERE
                          Code.class =  "��������" 
                      AND Code.code  =  mOKATO
                          NO-LOCK NO-ERROR.
               IF NOT AVAIL(CODE) THEN DO:
                  MESSAGE "��� ⠪��� ���祭�� � �����䨪��� ����: ���=" + op.inn + ", ���=" + mKPPRec + ", ���=" + 
                       mKBK + ", OKATO=" + mOKATO + " � � �����䨪��� ��������!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  RETURN.
               END.
            END.
         END.
      END.

      IF {assigned iValue} THEN DO:
         CASE iField:
            WHEN "mPokST"   THEN vXattrName = "�����".
            WHEN "mKppSend" THEN vXattrName = "Kpp-send".
            WHEN "mKppRec"  THEN vXattrName = "Kpp-rec".
            WHEN "mKBK"     THEN vXattrName = "���".
            WHEN "mOKATO"   THEN vXattrName = "�����-�����".
            WHEN "mPokOP"   THEN vXattrName = "�����".
            WHEN "mPokNP1"  THEN vXattrName = "�����".
            WHEN "mPokND"   THEN vXattrName = "�����".
            WHEN "mPokDD"   THEN vXattrName = "�����".
            WHEN "mPokTP"   THEN vXattrName = "�����".
         END CASE.
       
         RUN GetXattr (op.class-code,     
                       vXattrName,        
                       BUFFER xattr).   
         IF AVAIL(xattr) THEN DO:
       
            RUN CheckFieldEx (BUFFER xattr,         
                              iValue,               
                              NO,            
                              INPUT-OUTPUT vErrMsg,                                                     
                              OUTPUT vFErr).          .
            IF vFErr THEN RETURN.
       
         END.
      END.

      RUN FillLabelFields.
      RUN DispFrame.
      
      IF  iField = "mPokNP1" THEN
      mChkPokNP = INDEX(mPokNp1,".") > 0.         
      IF  iField = "mPokNP1"
      AND mChkPokNP THEN DO:

         ASSIGN
             mPokNP2 = ENTRY(2,mPokNp1,".")
             mPokNP3 = ""
             mPokNP3 = ENTRY(3,mPokNp1,".") WHEN NUM-ENTRIES(mPokNp1,".") >  2
         .         
                       
         IF LENGTH(mPokNP2) <> 2 THEN DO:
            MESSAGE "�� �ࠢ��쭮 ����� �����" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
         END.
         vS = GetCode ("���:��", ENTRY(1,mPokNP1,".")).
         IF vS = ? THEN vS = "12". /* ����� */
      
         vI = INT64(mPokNP2) NO-ERROR.
         IF ERROR-STATUS:ERROR
         OR vI < 0
         OR vI > INT64(vS)
         THEN DO:
            MESSAGE "���祭�� ����� ������ ���� �� 00 �� " + STRING(INT64(vS),"99")
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
         END.
      
         IF LENGTH(mPokNP3) <> 4 THEN DO:
            MESSAGE "�� �ࠢ��쭮 ����� ���" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
         END.
      
         vS = GetCode ("���:��", ENTRY(1,mPokNP1,".")).
         IF vS =  ? THEN DO: /* ���祭�� ���� ��� � �����䨪��� - �� ������ ���� ���*/
            vS = STRING(DATE(mPokNP1)) NO-ERROR.
            IF  ERROR-STATUS:ERROR
            AND {assigned vS}
            THEN DO:
               MESSAGE "���ࠢ��쭮 ������� ��� ���������� ��ਮ�� !" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               RETURN.
            END.
         END.
         ELSE DO:
            vI = INT64(mPokNP3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               MESSAGE "���ࠢ��쭮 ������ ���" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               RETURN.
            END.
         END.
         RUN DispFrame.
      END.
      
      /* �⤥�쭮 ����㥬 �ଠ� mPokDD - ������ ���� ��.��.���� ��� 0*/
      ELSE IF iField = "mPokDD" THEN DO:
         IF  {assigned mPokDD}
         AND mPokDD <> "0" THEN DO:
            ASSIGN vS = STRING(DATE(mPokDD),"99.99.9999") NO-ERROR.
            IF ERROR-STATUS:ERROR
            OR NOT {assigned vS}
            THEN DO:
               MESSAGE "���ࠢ��쭮 ������� ���" skip
                       "��ଠ� ���� ������ ���� ��.��.����"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               RETURN.
            END.
            IF INDEX(mFormatPokDD,".") = 0
            THEN mPokDD = STRING(DATE(mPokDD),"99.99.9999").
         END.
         RUN DispFrame.
      END.
      
   END.

   vErr = NO.
   RETURN.
END PROCEDURE.

ON F1 OF FRAME NalPr-Frame ANYWHERE DO:
   DEFINE VARIABLE vS  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI  AS INT64   NO-UNDO.
   DEFINE VARIABLE vkbkControl AS LOGICAL INIT NO NO-UNDO.
   DEFINE VARIABLE vcode-val   AS CHARACTER  NO-UNDO.
   vI = LOOKUP(FRAME-FIELD, mClassFieldList + "," + mAnotherFieldList ).
   IF vI =  0 THEN RETURN NO-APPLY. /* �⮡� �� ���砫� �� ��⠫��� ����� */
  
   IF LOOKUP(FRAME-FIELD,mClassFieldList) <> 0 THEN
   DO WITH FRAME NalPr-Frame:
      vS = GetCodeName ("", ENTRY(vI, mClassCodeList)).
      IF vS <> ? THEN DO:
         vS1 = GetCodeMisc("", ENTRY(vI, mClassCodeList), 3).
         vS2 = GetCode("���", mKBK).
         IF  mIsFullControlInput 
             AND vS2 <> ""
             AND ENTRY(vI, mClassFieldList) =  "mPokTP"
         THEN
             RUN SetSysConf IN h_base ("WhereCanDo", vS2).
             
         IF ENTRY(vI, mClassFieldList) =  "mPokTP" THEN 
         controlBlock: 
         DO:
            FIND FIRST code 
               WHERE code.class =  "���" AND
                     code.code  =  mKBK AND
                     code.misc[1] <> "��" NO-LOCK NO-ERROR.
            IF AVAIL code  THEN DO:
               IF code.val <> "" THEN
                  ASSIGN           
                     vkbkControl = YES
                     vcode-val = code.val.
            END.
               RUN browseld.p ("���-��",                                                           /* ����� ��ꥪ�. */             
                               "class"  +  CHR(1)   +  "parent" +  CHR(1)   +  "title" + 
                               (IF NOT vkbkControl THEN "" ELSE CHR(1) + "code"),                   /* ���� ��� �।��⠭����. */    
                                "���:��"   +  CHR(1)   +  "���:��"  +  CHR(1)   +  REPLACE(vS, CHR(2), ",") + 
                                (IF NOT vkbkControl  THEN "" ELSE CHR(1) + vcode-val),             /* ���᮪ ���祭�� �����. */     
                                "class"  +  CHR(1)   +  "parent",                                  /* ���� ��� �����஢��. */       
                                4                                                                  /* ��ப� �⮡ࠦ���� �३��.*/
                               ).                                     
            
         END.  /* controlBlock:  */
         ELSE DO:
             IF ENTRY(vI, mClassCodeList) =  "���" THEN DO:
                mTmpObjHand = TEMP-TABLE TmpObj:HANDLE.                 

                IF FGetSetting("���","���������","���") <> "��" THEN   
                RUN browseld.p ("���",
                                "class-code" + CHR(1) + "UseTmpObjInQuery",

                                "���" + CHR(1) + STRING(TEMP-TABLE TmpObj:HANDLE),

                                "",
                                4
                               ).
                ELSE DO:

                   RUN codelay.p ("����",
                                  "����",
                                  "�롥�� ���",
                                  4).

                   IF (LASTKEY =  13 OR LASTKEY =  10) AND pick-value <> ? THEN DO:
                      FIND FIRST code WHERE CODE.class =  "����"
                                        AND CODE.code  =  pick-value NO-LOCK NO-ERROR.

                      IF AVAIL(CODE) THEN pick-value = CODE.description[1].
                   END.
                END.
             END.

             ELSE DO:
                IF gend-date >= DATE("01.01.2014") AND ENTRY(vI, mClassFieldList) =  "mPokNP1" THEN DO:
                   IF NOT CAN-DO(mOsn106TP,mPokOP) THEN 
                     RUN codelay.p ('���:��',
                                    '���:��',
                                    vS,
                                    4).
                END.
                ELSE  IF NOT {&ViewOnly} THEN
                   RUN codelay.p (ENTRY(vI, mClassCodeList),
                                  ENTRY(vI, mClassCodeList),
                                  vS,
                                  4).
   
             END.
         END.
         RUN SetSysConf IN h_base ("WhereCanDo", ?).
         IF (LASTKEY =  13 OR LASTKEY =  10) AND pick-value <> ? THEN DO:
            IF ENTRY(vI, mClassFieldList) =  "mPokNP1" THEN
               SELF:SCREEN-VALUE = IF pick-value <> "0" 
                                   THEN (pick-value + ".")
                                   ELSE pick-value.
            ELSE
               SELF:SCREEN-VALUE = pick-value.
         END.
      END.
      ELSE
         MESSAGE "��� ⠪��� �����䨪���: '" + ENTRY(vI, mClassCodeList) + "' !"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   END.
   ELSE
   DO:
      pick-value = ?.
      IF FRAME-FIELD = "mKppSend" AND
         mKppSendAllowSelect      AND
        {assigned mKppSendValues} AND
         mIsFullControlInput      THEN
      DO:
         RUN messmenu.p(10 ,"[�������� ��� �����������]","",mKppSendValues).
         IF (LASTKEY =  13 OR LASTKEY =  10) AND pick-value <> ? THEN
            SELF:SCREEN-VALUE = ENTRY(INT64(pick-value),mKppSendValues).
      END.
      IF FRAME-FIELD = "mKppRec" AND
         mKppRecAllowSelect      THEN
      DO:

         RUN messmenu.p(10 ,"[�������� ��� ����������]","",mKppRecValues).
         IF (LASTKEY =  13 OR LASTKEY =  10) AND pick-value <> ? THEN
            SELF:SCREEN-VALUE = ENTRY(INT64(pick-value),mKppRecValues).
      END.

      IF NOT mIsFullControlInput  AND
         FRAME-FIELD = "mKppSend" THEN
      DO:
         kpp_var_code = ?.
         IF CAN-FIND(FIRST op-entry OF op) THEN DO:
            { kppproc.i
                &BUF-OP-ENTRY = op-entry
                &BUF-OP       = op
            }
         END.
         ELSE DO:
            { kppproc.i
                &BUF-OP-ENTRY = tt-op-entry
                &BUF-OP       = op
            }
         END.
         IF kpp_var_code <> ? AND kpp_var_code <> "" THEN
            SELF:SCREEN-VALUE = kpp_var_code.

      END.
      IF FRAME-FIELD = "mKppRec" THEN
      DO:
         RUN KppRecUpd(INPUT iOpRid,OUTPUT cSelectKppRec).
            IF cSelectKppRec <> ? AND cSelectKppRec <> "" THEN
               SELF:SCREEN-VALUE = cSelectKppRec.
          
      END.
      IF FRAME-FIELD = "mOKATO" AND mOKATOAllowSelect THEN
      DO:
         RUN messmenu.p(10 ,"[�������� �����]","",mOKATOValues).
         IF (LASTKEY =  13 OR LASTKEY =  10) AND pick-value <> ? THEN
            SELF:SCREEN-VALUE = ENTRY(INT64(pick-value),mOKATOValues).
      END.
   END.
   RETURN NO-APPLY.
END.

ON ANY-PRINTABLE, CLEAR, DELETE-CHARACTER, CTRL-V, BACKSPACE OF mKppSend IN FRAME NalPr-Frame DO:

   DEFINE VARIABLE vI AS INT64    NO-UNDO.

   IF mKppSendAllowSelect THEN DO:
      /* �᫨ ��筮� ����� ��� - � ������� ����� �� 㣮��� */
      IF mKppSendValues =  ? OR (NOT mIsKBK AND mAcctDbFlag ) THEN RETURN.
      ELSE DO: /* ���� �롨ࠥ� ���祭�� �� ᯨ᪠ �� �஡��� */
         IF LASTKEY =  32 THEN DO:
            vI = LOOKUP(mKppSend, mKppSendValues) + 1.
            IF vI > NUM-ENTRIES(mKppSendValues) THEN vI = 1.
            mKppSend = ENTRY(vI, mKppSendValues).
            DISP mKppSend WITH FRAME NalPr-Frame.
            RETURN NO-APPLY.
         END.
         ELSE RETURN NO-APPLY.
      END.
   END.
   ELSE RETURN NO-APPLY.
END.

COLOR DISPLAY bright-white
   mPokST
   mKppSend
   mKppRec
   mKBK
   mOKATO
   mPokOP
   mPokNP1
   mPokND
   mPokDD
   mPokTP WHEN mShowPokTp
   mTpLab WHEN mShowPokTp
   WITH FRAME NalPr-Frame.

COLOR DISPLAY bright-green
   mPokSTLabel
   mKBKLabel
   mPokOPLabel
   mPokNPLabel
   mPokTPLabel WHEN mShowPokTp
   WITH FRAME NalPr-Frame.


/* ******** ���樠������ ��६����� ******** */
ASSIGN
   mBalNalog = FGetSetting ("���", "bal-nalog", ?) /* ��᪨ ��� ������� �����⥫� ���⥦� */
   mCRDNP    = FGetSetting ("���", "�����", ?)     /* ��᪨ ��� ������� ����� ���㬥�� */
   .

/* ******** �᭮���� ���� ******** */
FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
IF NOT AVAIL op THEN DO:
   MESSAGE "�ணࠬ�� ��ࠡ�⪨ ४����⮢ ��������� ���⥦�� �� ᬮ��� ���� ���㬥��"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   RETURN "ESC".
END.

mFlNewFmt = op.op-date >= DATE("01.01.2015").
IF mFlNewFmt AND (mPokTP = ? OR mPokTP = "") AND mShowPokTp
THEN mPokTP = "0".

FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
IF AVAIL op-entry 
THEN DO:
   BUFFER-COPY op-entry to tt-op-entry.
END.
ELSE DO:
   IF   op.op-status =  "�"
     OR CAN-DO(op.op-status, FGetSetting("�⠭���","������", "�"))
     OR op.op-date = ?
   THEN DO TRANSACTION:
      FOR EACH bo-op WHERE bo-op.op-transaction =  op.op-transaction
                       AND bo-op.acct-cat       =  "o"
      NO-LOCK:
         IF GetXattrValue("op",STRING(bo-op.op),"op-bal") =  STRING(op.op) THEN
         LEAVE.
      END.
      IF NOT AVAIL bo-op THEN
      FIND FIRST bo-op WHERE bo-op.op-transaction =  op.op-transaction
                         AND bo-op.acct-cat       =  "o"
      NO-LOCK NO-ERROR.

      IF NOT AVAIL bo-op THEN DO:
         MESSAGE "�訡�� ���������� ��������� ४����⮢." skip 
                 "�� ������ �易��� ��������ᮢ� ���㬥��."
         VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      FIND FIRST bop-entry OF bo-op NO-LOCK NO-ERROR.
      CREATE tt-op-entry.
      ASSIGN
        tt-op-entry.op          = op.op
        tt-op-entry.Class-Code  = op.class-code
        tt-op-entry.acct-cat    = op.acct-cat
        tt-op-entry.amt-cur     = bop-entry.amt-cur
        tt-op-entry.amt-rub     = bop-entry.amt-rub
        tt-op-entry.currency    = bop-entry.currency
        tt-op-entry.op-cod      = "000000"
        tt-op-entry.op-date     = bo-op.op-date
        tt-op-entry.op-entry    = 1
        tt-op-entry.op-status   = op.op-status
        tt-op-entry.type        = bop-entry.type
        tt-op-entry.user-id     = bop-entry.user-id
        tt-op-entry.op-entry-half-db = tt-op-entry.op-entry
        tt-op-entry.op-entry-half-cr = tt-op-entry.op-entry
        tt-op-entry.acct-cr     = GetXattrValue("op", string(op.op), "acctcorr")
        tt-op-entry.acct-db     = GetXattrValue("op", string(op.op), "acctbal")
      .
      IF DECIMAL(GetXAttrValueEx("op", STRING(op.op), "amt-rub", "0")) <> 0 THEN
      ASSIGN
        tt-op-entry.amt-cur     = DECIMAL(GetXAttrValueEx("op", STRING(op.op), "amt-cur", ""))
        tt-op-entry.amt-rub     = DECIMAL(GetXAttrValueEx("op", STRING(op.op), "amt-rub", ""))
      .
   END.
   ELSE DO:
      MESSAGE "�訡�� ���������� ��������� ४����⮢." skip 
              "� ���㬥�� ��� �஢����."
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
END.

RELEASE DbAcct.

vAcct = "".
FIND FIRST op-entry OF op NO-LOCK NO-ERROR.

IF AVAIL op-entry 
   THEN vAcct = Op-Entry.Acct-Db.
   ELSE DO:
      FIND FIRST tt-op-entry NO-ERROR.
      IF AVAIL tt-op-entry 
         THEN vAcct = tt-op-entry.Acct-Db. 
   END.
IF vAcct <> "" THEN
   FIND FIRST DbAcct WHERE
              DbAcct.Acct = vAcct
   NO-LOCK NO-ERROR.
IF AVAIL DbAcct AND DbAcct.Cust-Cat = "�" THEN
   mAcctDbFlag = YES.

IF FGetSetting("���","���������","���") =  "��" THEN  DO:  
   FIND FIRST op-bank OF op WHERE
              op-bank.op-bank-type =  ""
          AND op-bank.bank-code-type =  "���-9"
              NO-LOCK NO-ERROR.

   IF AVAIL(op-bank) THEN DO:
      FIND FIRST code WHERE
            CODE.class =  "���室"
        AND CODE.val   =  op-bank.bank-code
        AND CODE.name  =  op.ben-acct
            NO-LOCK NO-ERROR.

      IF AVAIL(CODE) THEN
         mChkAdm = YES.            
   END.
END.

/* ����饭� �� ��ᬮ�� ? */
IF {&ViewOnly} THEN DO:
   /* ������塞 ���祭�� �� ���㬥�� */
   RUN FillVars (iOpRid).

   /* �஢��塞 - �뫮 �� ��� ���� ���� ��������� ? */
   IF NOT {&ViewOnlyIfExist} OR
      mPokST        <> "" OR
      mKppSend      <> "" OR
      mKppRec       <> "" OR
      mKBK          <> "" OR
      mOKATO        <> "" OR
      mPokOP        <> "" OR
      mPokNP        <> "" OR
      mPokND        <> "" OR
      mPokDD        <> "" OR
      mPokTP        <> ""
   THEN DO:
      RUN FillLabelFields.

      loop:
      DO ON ERROR UNDO loop, LEAVE loop ON ENDKEY UNDO loop, LEAVE loop:
         PAUSE 0.
         RUN DispFrame.
         ASSIGN
            mPokST:SENSITIVE = YES
            mPokST:READ-ONLY = YES
            mPokST:PFCOLOR   = 3.

         WAIT-FOR GO, WINDOW-CLOSE OF FRAME NalPr-frame FOCUS mPokST. 
      END.  

      HIDE FRAME NalPr-Frame NO-PAUSE.

      INPUT CLEAR.

      RETURN "OK".
   END.
END.
ELSE DO:
   mUseBackup = NEW op.
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF NOT AVAIL op-entry THEN DO:
      FIND FIRST tt-op-entry NO-ERROR.
      IF NOT AVAIL tt-op-entry THEN
         RETURN.
   END.

   /* ��।��塞 �����⥫� ���⥦� */
   RUN Collection-Info.
   IF AVAIL op-entry OR AVAIL tt-op-entry THEN
      RUN DefRecipient.

   /* �஢��塞 - �����⥫� ���⥦� ��� ��� �� ᮮ⢥����� ��᪥(��) ? */
   IF    CAN-DO(mBalNalog, ENTRY(1, PoAcct)) 
      OR CAN-DO(mVarSett_Balsmev, ENTRY(1, PoAcct)) 
      OR (NOT {assigned "ENTRY(1, PoAcct)"} 
          AND CAN-FIND(FIRST op-entry WHERE op-entry.op = op.op AND
                             CAN-DO(mVarSett_BalSmev,op-entry.acct-cr)))
      THEN DO:
      mBudPay = CAN-DO(mVarSett_Balsmev,ENTRY(1,PoAcct)).
      
      /* �᫨ ��� ���㬥�� ᮮ⢥����� ��᪥(��) */
      IF CAN-DO(mCRDNP, op.doc-type) THEN DO:
         FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
         IF AVAIL op-entry THEN
            ASSIGN
               vAcct = op-entry.acct-db
               vCurr = op-entry.currency
            .
         ELSE DO:
            FIND FIRST tt-op-entry NO-ERROR.
            IF AVAIL tt-op-entry THEN
               ASSIGN
                  vAcct = tt-op-entry.acct-db
                  vCurr = tt-op-entry.currency
               .
         END.
         IF NOT (AVAIL op-entry OR AVAIL tt-op-entry) THEN DO:
            RUN EditFrame ("").
            RETURN RETURN-VALUE.
         END. /* avail op-entry */
         ELSE DO:
            {find-act.i
               &acct = vAcct
               &curr = vCurr
            }
            IF NOT AVAIL acct THEN DO:
               RUN EditFrame ("").
               RETURN RETURN-VALUE.
            END.
            ELSE DO:
               RUN EditFrame (GetXAttrValue("acct", acct.acct + "," + acct.currency, "���")).
               RETURN RETURN-VALUE.
            END.
         END.
      END. /* CAN-DO(mCRDNP, op.doc-type)  */
      ELSE DO: /* �᫨ ��� ���㬥�� �� ᮮ⢥����� ��᪥ */
         
         IF {&ManualRun} THEN DO:
            RUN EditFrame ("").
            RETURN RETURN-VALUE.
         END.
      END.
   END. /* can-do (mBalNalog, PoAcct) */
   ELSE DO:
      IF {&ManualRun} THEN DO:
         RUN EditFrame ("").
         RETURN RETURN-VALUE.
      END.
   END.
END.

/*
procedure:  FillVars
comment:    �������� ��६���� �३��, ���� ���ଠ�� �� ���.४����⮢
            ���㬥��
parameters: iOpRid

*/
PROCEDURE FillVars.
   /* iOpRid - 㪠��⥫� �� ���㬥�� */
   DEFINE INPUT  PARAMETER iOpRid AS RECID      NO-UNDO.

   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DEFINE BUFFER op   FOR op.
   DEFINE BUFFER acct FOR acct.

   ASSIGN
      mPokSTLabel        = ""
      mKBKLabel          = ""
      mPokOPLabel        = ""
      mPokNPLabel        = ""
      mPokTPLabel        = ""
      .

   FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN.

   RUN GetDopParam.

   ASSIGN
      mPokNP1 = mPokNP
      mPokOP        = GetXattrValueEx ("op", STRING(op.op), "�����",       "")
      mPokTP        = GetXattrValueEx ("op", STRING(op.op), "�����",       "")
      mUIN          = GetXattrValueEx ("op", STRING(op.op), "���",       "")
      mEIP          = GetXattrValueEx ("op", STRING(op.op), "���",       "")
      mAIP          = GetXattrValueEx ("op", STRING(op.op), "���",       "")
      mUIP          = GetXattrValueEx ("op", STRING(op.op), "���",       "")
      mPokOP        = IF mPokOP = "" 
                         AND  NOT {&ViewOnlyIfExist}
                         THEN GetXattrInit ("opb", "�����")
                         ELSE mPokOP
      mPokTP        = IF mPokTP = "" 
                         AND NOT {&ViewOnlyIfExist}
                         THEN GetXattrInit ("opb", "�����")
                         ELSE mPokTP
                      /* �᫨ � �ଠ� ���.४����� ���� "." , � �᪫. �� �� ���祭�� */
      mPokDD        = IF INDEX(mFormatPokDD,".") > 0
                         THEN REPLACE(mPokDD, ".", "")
                         ELSE mPokDD
      mKppSendSensitive = YES
      mKppRecSensitive  = YES
      mOKATOSensitive   = YES
      m106n_stat_plat   = GetXattrValueEx ("op", STRING(op.op), "�106�_��⏫��", "")
   .
    
   ASSIGN
      mKppSend = GetXAttrValueEx("op", 
                                 STRING(op.op), 
                                 "kpp-send", 
                                 "")
      mKppRec  = GetXAttrValueEx("op", 
                                 STRING(op.op), 
                                 "kpp-rec", 
                                 "")
   .

IF AVAILABLE op-entry OR AVAILABLE tt-op-entry THEN 
DO:
    IF (mKppSend =  "" AND mKppRec =  "") THEN
    DO:
      DEFINE VARIABLE mKppFromBen AS CHARACTER NO-UNDO.
      mKppFromBen = GetXAttrValueEx("op-template",
                        op.op-kind + "," + STRING(IF AVAIL op-entry THEN op-entry.op-entry ELSE tt-op-entry.op-entry),
                        "const-recip","").
      IF NUM-ENTRIES(mKppFromBen,"^") >= 4 THEN
         mKppFromBen = ENTRY(4, mKppFromBen, "^").
      ELSE
         mKppFromBen = "".
   END.

   ASSIGN
      vAcct = IF AVAIL op-entry THEN op-entry.acct-db ELSE tt-op-entry.acct-db
      vCurr = IF AVAIL op-entry THEN op-entry.currency ELSE tt-op-entry.currency
   .

   FOR FIRST acct WHERE acct.acct     =  vAcct
                    AND acct.currency =  vCurr
                   NO-LOCK,
       FIRST branch OF acct NO-LOCK:

      IF mKppFromBen <> "" THEN
      DO:
         CASE op.doc-kind :
            WHEN "SEND" THEN
               mKppSend = mKppFromBen.
            WHEN "REC" THEN
               mKppRec = mKppFromBen.
            OTHERWISE
               IF CAN-DO(FGetSetting("�����猁�",?,""),acct.contract) THEN
                  mKppSend = mKppFromBen.
               ELSE
                  mKppRec = mKppFromBen.

         END CASE.
      END.

      IF NOT {assigned mKppSend}
         AND  acct.cust-cat =  "�" 
         AND NOT ({&ManualRun})
         AND NOT ({&ViewOnly})
      THEN
         mKppSend = GetXAttrValueEx("branch", 
                                     branch.branch-id, 
                                    "kpp-send", 
                                    "").

      IF GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "���", ?) <> ? THEN
      DO:

         IF mKppSend =  "" THEN
            mKppSend = GetXAttrValueEx("branch", 
                                       branch.branch-id, 
                                       "kpp-send", 
                                       "").

         IF mOKATO =  "" THEN
            mOKATO   = GetXAttrValueEx("branch", 
                                       branch.branch-id, 
                                       "�����-�����", 
                                       "").

         IF mKppRec =  "" THEN
            mKppRec  = GetXAttrValueEx("branch", 
                                       branch.branch-id, 
                                       "kpp-rec", 
                                       "").


         IF mOKATO =  "" THEN
            ASSIGN
               mOKATOValues  = FGetSetting ("���", "�����_������", ?)
               mOKATOAllowSelect = (NUM-ENTRIES(mOKATOValues) > 1)
               mOKATO  =  IF mOKATO <> "" AND mOKATO <> ? THEN
                             mOKATO
                          ELSE
                             IF mOKATOValues <> "" THEN
                                ENTRY(1,mOKATOValues)
                             ELSE
                                mOKATOValues
            .

         IF mOKATO <> "" AND NOT mOKATOAllowSelect THEN
            mOKATOSensitive = NO.
            
         IF mKppSend <> "" THEN
            mKppSendSensitive = NO.
               
         IF mKppRec <> "" THEN
            mKppRecSensitive = NO.
      END.
   END.
END.
IF mUseBackup THEN
   RUN RestoreTaxAttrs IN THIS-PROCEDURE (op.op).
END PROCEDURE.

/*
procedure:  SaveVars
comment:    ��࠭�� ���祭�� �३�� � ���.४������ ���㬥��
parameters: iOpRid

*/
PROCEDURE SaveVars.
   /* iOpRid - 㪠��⥫� �� ���㬥�� */
   DEFINE INPUT  PARAMETER iOpRid AS RECID      NO-UNDO.

   DEFINE VARIABLE mCustCat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mDetails AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAcct    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mINN     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mZZ      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mYY      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mDate107b     AS DATE NO-UNDO.
   DEFINE VARIABLE vDate107e     AS DATE NO-UNDO.
   DEFINE VARIABLE m107n    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE opedUIN1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE opedUIN2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE opeddetails  AS CHARACTER  NO-UNDO.
   DEFINE BUFFER cop  FOR op.
   DEFINE BUFFER op   FOR op.
   DEFINE BUFFER b-op FOR op.

   FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN.

   mDate107b = DATE("01.01.2014").
   vDate107e = DATE("01.04.2014").
   
   IF op.doc-date >= mDate107b THEN
      m107n = "YES".
   IF NOT mEmpty THEN
   DO:
       
      /**/
      UpdateSigns ("op", STRING(op.op), "�����",       mPokST,        isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "Kpp-send",    mKppSend,      isXAttrIndexed(op.class-code,"Kpp-send")).
      UpdateSigns ("op", STRING(op.op), "Kpp-rec",     mKppRec,       isXAttrIndexed(op.class-code,"Kpp-rec")).
      UpdateSigns ("op", STRING(op.op), "���",         mKBK,          isXAttrIndexed(op.class-code,"���")).
      UpdateSigns ("op", STRING(op.op), "�����-�����", mOKATO,        isXAttrIndexed(op.class-code,"�����-�����")).
      UpdateSigns ("op", STRING(op.op), "�����",       mPokOP,        isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "�����",       IF mPokNP <> ".." 
                                                          THEN mPokNP
                                                          ELSE "",    isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "�����",       mPokND,        isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "�����",       mPokDD,        isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "�����",       mPokTP,        isXAttrIndexed(op.class-code,"�����")).
      UpdateSigns ("op", STRING(op.op), "�106�_��⏫��", m106n_stat_plat, isXAttrIndexed(op.class-code,"�106�_��⏫��")).
      UpdateSigns ("op", STRING(op.op), "���",         mUIN,          isXAttrIndexed(op.class-code,"���")).
      UpdateSigns ("op", STRING(op.op), "���",         mEIP,          isXAttrIndexed(op.class-code,"���")).
      UpdateSigns ("op", STRING(op.op), "���",         mAIP,          isXAttrIndexed(op.class-code,"���")).
      UpdateSigns ("op", STRING(op.op), "���",         mUIP,          isXAttrIndexed(op.class-code,"���")).
   /**/
      IF {assigned mUIN} OR {assigned mEIP} OR {assigned mAIP} OR {assigned mUIP} THEN
      DO:
         mDetails = "".
         mAcct = "".
         FIND FIRST op-entry OF op WHERE op-entry.acct-db <> ? NO-LOCK NO-ERROR.
         IF AVAIL op-entry 
         THEN
            mAcct = op-entry.acct-db.
         ELSE DO:
            FIND FIRST tt-op-entry WHERE tt-op-entry.acct-db <> ? NO-LOCK NO-ERROR.
            IF AVAIL tt-op-entry 
            THEN
               mAcct = tt-op-entry.acct-db.
         END.
         IF mAcct <> "" THEN 
         DO:
            {find-act.i
            &acct=mAcct}
            IF AVAIL(acct) THEN
            DO:
               mCustCat = acct.cust-cat.
               IF mCustCat =  "�" THEN 
               DO:
                  FIND FIRST person WHERE 
                  person.person-id =  acct.cust-id
                  NO-LOCK NO-ERROR.
                  IF AVAIL(person) THEN mINN = person.inn.
               END.
               ELSE IF mCustCat =  "�" THEN 
               DO:
                  FIND FIRST cust-corp WHERE 
                    cust-corp.cust-id =  acct.cust-id
                  NO-LOCK NO-ERROR.
                  IF AVAIL(cust-corp) THEN mINN = cust-corp.inn.
               END.
               ELSE IF mCustCat =  "�" THEN 
               DO:
                  FIND FIRST banks WHERE 
                     banks.bank-id =  acct.cust-id
                     NO-LOCK NO-ERROR.
                  IF AVAIL(banks) THEN mINN = banks.inn.
               END.
            END.
            IF mUIN <> "" THEN
            DO:
               mDetails = "".
               IF mCustCat =  "�" AND mINN <> "" THEN 
               DO:
                  ASSIGN
                     mZZ = ""
                     mYY = "".
                  mDetails = mUIN + mZZ + mYY.
               END.
               IF mCustCat =  "�" AND mINN =  "" THEN 
               DO:
                  IF {assigned mEIP} THEN 
                  ASSIGN
                     mZZ = ";��" + "14"
                     mYY = ";" + SUBSTRING(mEIP,2,11).
                  IF {assigned mAIP} AND (SUBSTRING(mAIP,1,2) =  "14" OR SUBSTRING(mAIP,1,2) =  "21") THEN
                  ASSIGN
                     mZZ = ""
                     mYY = "".
                  IF {assigned mAIP} AND (SUBSTRING(mAIP,1,2) <> "14" AND SUBSTRING(mAIP,1,2) <> "21") THEN 
                  ASSIGN
                     mZZ = ";��" + SUBSTRING(mAIP,1,2)
                     mYY = ";" + Trim(String(INT64(SUBSTRING(mAIP,3,20)),">>>>>>>>>>>>>>>>>>>9")).

                  mDetails = mUIN + IF m107n =  "YES" THEN "" ELSE (mZZ + mYY).
   /*               IF NOT {assigned mEIP} AND NOT {assigned mAIP} THEN  mUIN = mUIN + ";��0".*/
               END.
               
               IF ((mCustCat =  "�" OR mCustCat =  "�") AND mINN <> "") OR mCustCat =  "�" THEN
               DO:
                  IF {assigned mUIN} THEN
                  ASSIGN
                     mZZ = ""
                     mYY = "".
                  mDetails = mUIN + mZZ + mYY.
               END.
            END.
         
            ELSE IF mUIN =  "" THEN
            DO:
               mDetails = "".
               IF mINN =  "" THEN 
               DO:
                  ASSIGN
                     mUIN = "0"
                     mZZ = ""
                     mYY = "".
                  mDetails = mUIN + mZZ + mYY.
               END.
               IF mINN <> "" THEN
               DO:
                  ASSIGN
                     mUIN = "0"
                     mZZ = ""
                     mYY = "".
                  mDetails = mUIN + mZZ + mYY.
               END.

            END.
         
            IF m107n =  "YES"  THEN
               mDetails = "���" + mDetails + "///" + " " .
            ELSE 
               mDetails = " ///���" + mDetails.

            pick-value = "YES".
         
            IF LENGTH(op.details + mDetails) >= 210 AND
               op.op-date <  (vDate107e - 1)
            THEN 
               RUN Fill-AlertSysMes IN h_base ("","","4", 
               "���㬥�� " + Trim(op.doc-num) + 
               ". �����祭�� ���⥦� �ॢ�蠥� 210 ᨬ�����. ��������� ///���?").
               
            IF pick-value = "YES" THEN RUN SetSysConf IN h_base ("UIN",mDetails).
         END.

         FIND FIRST cop WHERE RECID(cop) =  iOpRid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF AVAIL cop AND cop.op-date <  (vDate107e - 1) THEN DO:
            IF m107n  =  "YES"  THEN DO:
               IF INDEX(cop.details,"���") =  0 AND mDetails <> ? THEN 
                  Assign cop.details = mDetails + cop.details.
               ELSE 
               IF INDEX(cop.details,"���") <> 0 THEN 
               DO:
                     opedUIN1 = SUBSTRING(cop.details , INDEX(cop.details,"���") , INDEX(cop.details,"///") + 2).
                     opedUIN2 = GetSysConf("UIN").
                     IF SUBSTRING(cop.details,1,3) =  "���" THEN
                        opeddetails = TRIM(SUBSTRING(cop.details , INDEX(cop.details,"///") + 3 )).
                     ELSE 
                        opeddetails = SUBSTRING(cop.details,1 , INDEX(cop.details,"���") - 1).
                     /* IF (opedUIN1 NE opedUIN2) THEN */
                        Assign cop.details = opedUIN2 + opeddetails.
               END.
            END.   /* IF m107n  EQ "YES" */
            ELSE DO:
               IF INDEX(cop.details,"///���") =  0 AND GetSysConf("UIN") <> ? THEN 
               DO:
                  Assign cop.details = cop.details + GetSysConf("UIN").
               END.
               ELSE 
               IF INDEX(cop.details,"///���") <> 0 AND GetSysConf("UIN") <> ? THEN 
               DO:
                  opedUIN1 = SUBSTRING(cop.details , INDEX(cop.details,"///���") , LENGTH(cop.details) - INDEX(cop.details,"///���") + 1).
                  opedUIN2 = SUBSTRING(GetSysConf("UIN") , 2).
                  /* IF (opedUIN1 NE opedUIN2) THEN */
                     Assign cop.details = SUBSTRING(cop.details , 1 , INDEX(cop.details,"///���") - 1) + opedUIN2.
               END.
            END.
         END.

         IF m107n    =  "YES" AND 
            mCustCat =  "�"   AND 
            mINN     =  ""   
         /* �� �믮��塞 ��⮧��������� ����� ��� ��������� ���⥦�� */
            AND NOT CAN-DO(mKBKNalog ,mKBK) THEN         
         DO:
               
            IF {assigned mEIP} THEN
               ASSIGN 
                  mZZ = "14"
                  mYY = SUBSTRING(mEIP,2,11).
            IF {assigned mAIP} AND (SUBSTRING(mAIP,1,2) <> "14" OR SUBSTRING(mAIP,1,2) <> "21") THEN 
               ASSIGN
                  mZZ = SUBSTRING(mAIP,1,2)
                  mYY = FILL("0",10 - LENGTH(TRIM(STRING(INT64(SUBSTRING(mAIP,3,20)),">>>>>>>>>>>>>>>>>>>9")))) + 
                        Trim(String(INT64(SUBSTRING(mAIP,3,20)),">>>>>>>>>>>>>>>>>>>9")).
            UpdateSigns ("op", STRING(op.op), "�����", mZZ + ";" + mYY, YES).
            IF mEIP =  "" AND mAIP =  "" THEN DO:
               MESSAGE  "��������� ���/��� �� ���������." SKIP "��������� ����� ��᢮��� ���祭�� '0'" 
                  VIEW-AS ALERT-BOX.
               UpdateSigns ("op", STRING(op.op), "�����", "0", YES).
            END.
         END.

      END.
   /**/
   END.
   ELSE
   DO:
      UpdateSigns ("op", STRING(op.op), "�����",          "", YES).
      UpdateSigns ("op", STRING(op.op), "���",            "", YES).
      UpdateSigns ("op", STRING(op.op), "�����-�����",    "", NO).
      UpdateSigns ("op", STRING(op.op), "�����",          "", YES).
      UpdateSigns ("op", STRING(op.op), "�����",          "", YES).
      UpdateSigns ("op", STRING(op.op), "�����",          "", YES).
      UpdateSigns ("op", STRING(op.op), "�����",          "", NO).
      UpdateSigns ("op", STRING(op.op), "�����",          "", YES).
      UpdateSigns ("op", STRING(op.op), "�106�_��⏫��", "", isXAttrIndexed(op.class-code,"�106�_��⏫��")).
      UpdateSigns ("op", STRING(op.op), "���",            "", ?).
      UpdateSigns ("op", STRING(op.op), "���",            "", ?).
      UpdateSigns ("op", STRING(op.op), "���",            "", ?).
      UpdateSigns ("op", STRING(op.op), "���",            "", ?).
      UpdateSigns ("op", STRING(op.op), "���",            "", ?).
   END.
END PROCEDURE.

/*
procedure: FillLabelFields
comment:  �������� ���� - ����஢�� ���祭�� �३��
*/
PROCEDURE FillLabelFields.
   ASSIGN
      mPokSTLabel        = GetCodeName ("�����", mPokST)
      mKBKLabel          = GetCodeName ("���", mKBK)
      mPokOPLabel        = GetCodeName ("���:��", mPokOP)
      mPokNPLabel        = GetCodeName ("���:��", ENTRY(1,mPokNP1,".")) WHEN INDEX(mPokNP1,".") > 0 
      mPokTPLabel        = GetCodeName ("���:��", mPokTP)
      .

      IF GetCodeBuff("�����", mPokST, BUFFER code) THEN
         IF code.description[1] <> "" THEN
            m106n_stat_plat = code.description[1].

END PROCEDURE.

/*
procedure: DispFrame
comment:  �⮡ࠦ��� ᮤ�ন��� ����� �३��

*/
PROCEDURE DispFrame.

&IF DEFINED(SESSION-REMOTE) <> 0 &THEN
   DEFINE  VAR vS AS CHARACTER NO-UNDO.
   IF  {assigned mPokDD}
     AND mPokDD <> "0" 
     AND INDEX(mFormatPokDD,".") = 0 THEN DO:

     ASSIGN vS = STRING(DATE(mPokDD),"99.99.9999") NO-ERROR.
     IF NOT ERROR-STATUS:ERROR             AND {assigned vS}
        THEN mPokDD = vS.
   END.

&ENDIF

   DISP
      "" WHEN mPokST =  ? @ mPokST
      mPokST WHEN mPokST <> ?
      "" WHEN mKppSend =  ? @ mKppSend
      mKppSend WHEN mKppSend <> ?
      "" WHEN mKppRec =  ? @ mKppRec
      mKppRec WHEN mKppRec <> ?
      "" WHEN mKBK =  ? @ mKBK
      mKBK WHEN mKBK <> ?
      "" WHEN mOKATO =  ? @ mOKATO
      mOKATO WHEN mOKATO <> ?
      "" WHEN mPokOP =  ? @ mPokOP
      mPokOP WHEN mPokOP <> ?
      "" WHEN mPokNP1 =  ? @ mPokNP1
      mPokNP1 WHEN mPokNP1 <> ?
      "" WHEN mPokND =  ? @ mPokND
      mPokND WHEN mPokND <> ?
      "" WHEN mPokDD =  ? @ mPokDD
      mPokDD WHEN mPokDD <> ?
      "" WHEN mPokTP =  ? @ mPokTP
      mPokTP WHEN mPokTP <> ? AND  mShowPokTp
      mTPLab WHEN mShowPokTP
      mPokSTLabel
      mKBKLabel
      mPokOPLabel
      mPokNPLabel
      mPokTPLabel WHEN mShowPokTp
      WITH FRAME NalPr-Frame.
      mPokTp:SENSITIVE = mShowPokTp.

END PROCEDURE.

FUNCTION GetOKATO RETURNS CHAR (INPUT iOpRid AS RECID, INPUT iOldOKATO AS CHAR):
   DEFINE BUFFER op FOR op.
   DEFINE BUFFER op-entry FOR op-entry.
   DEFINE VAR vOpOKATO AS CHAR NO-UNDO.
   DEFINE VAR vAcct    AS CHAR NO-UNDO.
   DEFINE VAR vCurr    AS CHAR NO-UNDO.

   FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN ?.
   vOpOKATO = GetXAttrValueEx("op",STRING(op.op),"�����-�����","").
   IF vOpOKATO <> "" THEN 
      RETURN vOpOKATO.

   vAcct = "".
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF AVAIL op-entry 
   THEN
      ASSIGN 
         vAcct = op-entry.acct-db
         vCurr = op-entry.currency
      .
   ELSE DO:
      FIND FIRST tt-op-entry NO-ERROR.
      IF AVAIL tt-op-entry 
      THEN
         ASSIGN 
            vAcct = tt-op-entry.acct-db
            vCurr = tt-op-entry.currency
         .
   END.
   IF vAcct = "" THEN RETURN ?.
   {find-act.i
      &acct = vAcct
      &curr = vCurr
   }
   IF NOT AVAIL acct THEN RETURN ?.

   CASE acct.cust-cat:
      WHEN "�" THEN RETURN GetXAttrValueEx ("cust-corp", 
                                            STRING(acct.cust-id), 
                                            "�����-�����", 
                                            iOldOkato).
      WHEN "�" THEN RETURN GetXAttrValueEx ("person",    
                                            STRING(acct.cust-id),
                                            "�����-�����",
                                            iOldOkato).
      WHEN "�" THEN RETURN GetXAttrValueEx ("banks",     
                                            STRING(acct.cust-id), 
                                            "�����-�����", 
                                            iOldOkato).
      OTHERWISE     RETURN "".
   END CASE.

   RETURN ?.

END FUNCTION.

FUNCTION GetKpp RETURNS CHAR (INPUT iOpRid AS RECID):
   DEFINE VAR vAcct    AS CHAR NO-UNDO.
   DEFINE VAR vCurr    AS CHAR NO-UNDO.

   DEFINE BUFFER op FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   FIND FIRST op WHERE RECID(op) =  iOpRid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN ?.

   vAcct = "".
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF AVAIL op-entry 
   THEN
      ASSIGN 
         vAcct = op-entry.acct-db
         vCurr = op-entry.currency
      .
   ELSE DO:
      FIND FIRST tt-op-entry NO-ERROR.
      IF AVAIL tt-op-entry 
      THEN
         ASSIGN 
            vAcct = tt-op-entry.acct-db
            vCurr = tt-op-entry.currency
         .
   END.
   IF vAcct = "" THEN RETURN ?.
   {find-act.i
      &acct = vAcct
      &curr = vCurr
   }
   IF NOT AVAIL acct THEN RETURN ?.

   CASE acct.cust-cat:
      WHEN "�" THEN RETURN GetXAttrValueEx ("cust-corp", STRING(acct.cust-id), "���", "0").
      WHEN "�" THEN RETURN GetXAttrValueEx ("person",    STRING(acct.cust-id), "���", "0").
      WHEN "�" THEN RETURN GetXAttrValueEx ("banks",     STRING(acct.cust-id), "���", "0").
      OTHERWISE     RETURN "0".
   END CASE.

   RETURN ?.

END FUNCTION.

FUNCTION GetKppAcct RETURNS CHAR (INPUT iAcct AS CHARACTER):
   DEFINE BUFFER acct FOR acct.
   FIND FIRST acct WHERE acct.acct =  iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL acct THEN RETURN ?.
   CASE acct.cust-cat:
      WHEN "�" THEN RETURN GetXAttrValueEx ("cust-corp", STRING(acct.cust-id), "���", "0").
      WHEN "�" THEN RETURN GetXAttrValueEx ("person",    STRING(acct.cust-id), "���", "0").
      WHEN "�" THEN RETURN GetXAttrValueEx ("banks",     STRING(acct.cust-id), "���", "0").
      OTHERWISE     RETURN "0".
   END CASE.

   RETURN ?.

END FUNCTION.

PROCEDURE EditFrame.
   DEFINE INPUT  PARAMETER iKBK       AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.

   DEFINE BUFFER   GB4PL_op FOR op.

   mIsKBK = iKBK <> "".
   mIsFullControlInput = mIsKBK OR mSettFullCntrl.

   /* ������塞 ���祭�� �� ���㬥�� */
   RUN FillVars (iOpRid).

   IF iKBK <> "" THEN /* �᫨ � ��� ��易⥫�� ���� */
      ASSIGN
         mKppSendValues = FGetSetting ("�������", ?, ?)
         mKppSendAllowSelect = (NUM-ENTRIES(mKppSendValues) > 1)
         mKppSend   =  IF mKppSend <> "" AND mKppSend <> ? THEN
                          mKppSend
                       ELSE
                       IF mKppSendValues <> "" THEN
                          ENTRY(1,mKppSendValues)
                       ELSE
                          mKppSendValues
         mKppRecValues       = GetKppAcct(poAcct)
         mKppRec             = IF {assigned mKppRec} THEN
                                  mKppRec
                               ELSE
                               IF LOOKUP(mKppRec , mKppRecValues) = 0
                               THEN
                                  ENTRY(1, mKppRecValues)
                               ELSE
                                  mKppRec
         mKppRecAllowSelect  = NUM-ENTRIES(mKppRecValues) > 1

         mKBK     = iKBK
         mOKATOValues  = FGetSetting ("���", "�����_������", ?)
         mOKATOAllowSelect = (NUM-ENTRIES(mOKATOValues) > 1)
         mOKATO  =  IF mOKATO <> "" AND mOKATO <> ? THEN
                       mOKATO
                    ELSE
                    IF mOKATOValues <> "" THEN
                       ENTRY(1,mOKATOValues)
                    ELSE
                       mOKATOValues
         .
   ELSE DO: /* �᫨ �� ��易⥫�� ���� */
      IF NOT {&ManualRun} THEN
         ASSIGN
            mKppSendValues = GetKpp (iOpRid)
            mKppSend = IF mKppSend <> "" AND mKppSend <> ? THEN
                          mKppSend
                       ELSE
                       IF LOOKUP(mKppSend, mKppSendValues) = 0
                       THEN
                          ENTRY(1, mKppSendValues)
                       ELSE
                          mKppSend
            /*mKppSendAllowSelect = NUM-ENTRIES(mKppSendValues) > 1*/
            mKppRecValues       = GetKppAcct(poAcct)

            mKppRec             = IF mKppRec  <> "" AND mKppRec  <> ? THEN
                                     mKppRec
                                  ELSE
                                  IF LOOKUP(mKppRec , mKppRecValues) = 0
                                  THEN
                                     ENTRY(1, mKppRecValues)
                                  ELSE
                                     mKppRec
            mKppRecAllowSelect  = NUM-ENTRIES(mKppRecValues) > 1

            mOKATO = GetOKATO (iOpRid, mOkato)
         .
      ELSE /* �᫨ ��筮� ����� ��� */
         mKppSendValues      = ?

      .

      mKppSendAllowSelect = YES.
   END.
   RUN FillLabelFields.

   loop:
   DO ON ERROR UNDO loop, LEAVE loop ON ENDKEY UNDO loop, LEAVE loop:
      PAUSE 0.
      RUN DispFrame.
      
      ENABLE
         mPokST
         mKppSend      WHEN mKppSendAllowSelect AND mKppSendSensitive
         mKppRec       WHEN mKppRecSensitive
         mKBK          WHEN NOT mIsKBK
         mOKATO        WHEN (NOT mIsKBK OR
                            (mIsKBK    AND
                             mOKATOAllowSelect))  AND       
                             mOKATOSensitive
         mPokOP
         mPokNP1
         mPokND
         mPokDD
         mPokTP        WHEN mShowPokTp
         WITH FRAME NalPr-Frame.
      WAIT-FOR GO,WINDOW-CLOSE OF CURRENT-WINDOW FOCUS mPokST.

   END.

   ASSIGN
      mPokST
      mKppSend
      mKppRec
      mKBK
      mOKATO
      mPokOP
      mPokNP1
      mPokND
      mPokTP
      .
   HIDE FRAME NalPr-Frame NO-PAUSE.

   vResult = "ESC".
  {find-act.i
   &acct = op-entry.acct-db
  }
   IF LASTKEY =  10 OR LASTKEY =  13 THEN DO:
      IF {assigned mPokSt} AND 
         mPokSt <> "0" AND
         NOT {assigned mUIN}
      THEN
         mUIN = "0".
      /*�஢��塞 �᫮��� "���⪮�� ����஫�" ४�����⮢ ����*/
      IF     (mVarSett_ContGMP = "��" 
         AND (        CAN-DO(mVarSett_BalSmev, op.ben-acct)
              OR (    NOT {assigned op.ben-acct} 
                  AND CAN-DO(mVarSett_BalSmev, op-entry.acct-cr)))
         AND {assigned mPokST}
         AND (NOT {assigned mEIP}
         AND  NOT {assigned mAIP}))
         AND AVAIL acct
         AND  (   acct.cust-cat =  "�" OR acct.cust-cat =  "�" 
               OR (    acct.cust-cat =  "�" 
                   AND (   CAN-DO(FGetSetting("�⠭���", "��⠁��", ""), op-entry.acct-db)
                        OR CAN-DO(fGetSetting("�⠭���","��⠂������",""),op-entry.acct-db))
                   ) 
              )
         THEN
      DO:
         {messmenu                                                        
            &text    = "����室��� ����� ४������ ���� ��� ��ࠢ�� ���⥦� � ��� ���. �㤥� ���������?"
            &choices = "��,�⬥��"                     
            &update  = 1                                                
         }
         IF pick-value = "1" THEN
         DO:
            RUN smev.
            IF RETURN-VALUE = "ESC" THEN RETURN "ESC".
         END.
         ELSE DO:
            RETURN "ESC".
         END.
      END.
      /*�᫨ �� ���⃌� = ��, � ॠ���㥬 "��� ����஫�" ४����⮢ ����*/
      ELSE IF     (mVarSett_ContGMP = "��"
              AND (        CAN-DO(mVarSett_BalSmev, op.ben-acct)
                   OR (    NOT {assigned op.ben-acct} 
                       AND CAN-DO(mVarSett_BalSmev, op-entry.acct-cr)))
              AND {assigned mPokST}
              AND (NOT {assigned mEIP}
              AND  NOT {assigned mAIP}))
              AND (        acct.cust-cat =  "�" OR acct.cust-cat =  "�" 
                    OR (   acct.cust-cat =  "�" 
                       AND (   CAN-DO(FGetSetting("�⠭���", "��⠁��", ""), op-entry.acct-db)
                            OR CAN-DO(fGetSetting("�⠭���","��⠂������",""),op-entry.acct-db))
                       ) 
                  )
      THEN DO:
         
          {messmenu                                                        
            &text    = "����室��� ����� ४������ ���� ��� ��ࠢ�� ���⥦� � ��� ���. �㤥� ���������?"
            &choices = "��,���,�⬥��"                     
            &update  = 1                                                
         }
         IF pick-value = "1" THEN
         DO:
            RUN Smev.
            IF RETURN-VALUE = "ESC" THEN RETURN "ESC".
         END.
         ELSE IF pick-value = "3" THEN
         DO:
            RETURN "ESC".
         END.
      END.
      RUN SaveVars (iOpRid).
      vResult = "OK".
   END.
   IF mEmpty THEN vResult = "OK".

   INPUT CLEAR.

   RETURN vResult.

END PROCEDURE.

PROCEDURE Smev:
   RUN SetSysConf IN h_base ("KBK", mKBK).
   RUN nalpl_smev.p(
      INPUT op.op,
      INPUT (IF AVAIL(op-entry) THEN op-entry.op-entry ELSE ?),
      INPUT {&ViewOnly},
      INPUT mSMEVreq,
      INPUT-OUTPUT mUIN, 
      INPUT-OUTPUT mEIP, 
      INPUT-OUTPUT mAIP, 
      INPUT-OUTPUT mUIP
      ).
   RUN DeleteOldDataProtocol IN h_base ("KBK").
END PROCEDURE.

PROCEDURE KppRecUpd:
   DEFINE INPUT PARAMETER ipOpRecid AS RECID NO-UNDO.
   DEFINE OUTPUT PARAMETER opSelectKppRec AS CHAR NO-UNDO.

   DEFINE VARIABLE vKppRecList    AS CHAR NO-UNDO.
   DEFINE VARIABLE vKppRecNewList AS CHAR NO-UNDO.
   DEFINE VARIABLE vCode          AS CHAR NO-UNDO.
   DEFINE VARIABLE codefind       AS CHAR NO-UNDO.

   FIND FIRST op WHERE RECID(op) =  ipOpRecid NO-LOCK NO-ERROR.

   IF AVAIL op THEN DO:
      IF op.doc-kind =  "rec" OR op.doc-kind =  "" THEN DO:
         FIND FIRST op-bank OF op WHERE op-bank.op-bank-type =  "" NO-LOCK NO-ERROR. 
         IF AVAIL  op-bank THEN 
            codefind = STRING (INT64 (op-bank.bank-code), "999999999") + "," + op.ben-acct + "," + op.inn. /* BIC */
         ELSE 
            RUN getOpDocSysConf(INPUT "������", INPUT RECID(op), OUTPUT codefind).

         RUN GetRecipientValue IN h_cust (ENTRY(1,codefind),
                                          GetEntries(2,codefind,",",""),
                                          GetEntries(3,codefind,",",""),
                                          "���,����_����,���,���",
                                          OUTPUT vCode
                                         ).
         IF GetEntries(1,vCode,CHR(2),"") <> "" THEN DO:
            vKppRecList = GetEntries(4,vCode,CHR(2),"").
            RUN RecKpp(INPUT vKppRecList,OUTPUT opSelectKppRec,OUTPUT vKppRecNewList).
            IF vKppRecList <> vKppRecNewList THEN DO:
               TR:
               DO TRANSACTION ON ERROR UNDO TR,LEAVE TR:
                  RUN CreateUpdateRecipient IN h_cust (GetEntries(1,vCode,CHR(2),""),GetEntries(2,vCode,CHR(2),""),GetEntries(3,vCode,CHR(2),""),?,vKppRecNewList,?,?,"���").
               END.  /* End of TR BLOCK */
            END.
         END. /*IF AVAIL CODE */

      END. /*IF op.doc-kind EQ "rec" OR op.doc-kind eq ""*/

   END. /*IF AVAIL op*/

END PROCEDURE.


/* �������� ���� �����*/
FUNCTION ValidateOKTMO RETURN LOGICAL 
(INPUT iPOKATO AS CHARACTER,
 INPUT iPKBK   AS CHARACTER):

   DEFINE VARIABLE vOKTMO     AS INT64            NO-UNDO.
   DEFINE VARIABLE vRet       AS LOGICAL INIT YES NO-UNDO.
   DEFINE VARIABLE vBal401    AS LOGICAL          NO-UNDO.
      
     vBal401 = IF {assigned op.ben-acct} AND CAN-DO(mBal401,op.ben-acct) THEN YES
                                                                         ELSE NO.
      vOKTMO = INT64(MOKATO) NO-ERROR.
      IF ERROR-STATUS:ERROR OR
         (LENGTH(TRIM(ipOKATO)) <> 8 AND TRIM(ipOKATO) <> "0")  OR
          ipOKATO =  FILL("0",8)
         THEN DO:
         IF CAN-DO(mKBKNalog,ipKBK) OR vBal401 THEN
         RUN Fill-SysMes IN h_tmess("", "", -1, 
         "����� ���� 105 ������ ���� 8 ���!").
         ELSE
         RUN Fill-SysMes IN h_tmess("", "", -1, 
         "����� ���� 105 ������ ���� 8 ��� ���� ᮤ�ঠ�� ᨬ��� '0'!").
         vRet = NO.
      END.
      IF (CAN-DO(mKBKNalog,ipKBK) OR vBal401) AND 
         LENGTH(TRIM(TRIM(ipOKATO,"0"))) =  0
         AND vRet
      THEN DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, 
         "��� ��������� ���⥦�� ���� 105 �� ����� ���� ࠢ�� '0'!").
         vRet = NO.
      END.
   RETURN vRet.

END FUNCTION.

/* �������� ���� PokSt*/
FUNCTION ValidatePokSt RETURN LOGICAL PRIVATE (INPUT iPokSt AS CHARACTER, 
                                               INPUT iOpRowid AS ROWID):

   DEFINE VARIABLE vRet  AS LOGICAL INIT YES NO-UNDO.
   DEFINE VARIABLE vDateMCI96 AS DATE NO-UNDO.
   DEFINE VARIABLE vDateDoc   AS DATE NO-UNDO.
   DEFINE VARIABLE vBal401 AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vQues   AS CHARACTER NO-UNDO.

   DEFINE BUFFER bop        FOR op.
   DEFINE BUFFER bop-bank   FOR op-bank.
   DEFINE BUFFER banks-code FOR banks-code.
   DEFINE BUFFER banks      FOR banks.
   DEFINE BUFFER      bcode FOR       code.

   IF {assigned iPokSt} AND CAN-DO("14",iPokSt) THEN 
   DO: 
      RUN Fill-SysMes IN h_tmess("", "", -1, "����� 14 �᪫�祭 �� 107�." ).
      vRet = NO.
      RETURN vRet.
   END.
   
   IF {assigned iPokSt} AND NOT CAN-DO('0,00',iPokSt) THEN RETURN vRet.

   vDateMCI96 = DATE(FGetSetting("���", "��⠌��96","")) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN vRet.
   
   vQues = FGetSetting("���", "��������96","���").
   IF vQues = "���" THEN RETURN vRet.

   FIND FIRST bop WHERE ROWID(bop) = iOpRowid NO-LOCK NO-ERROR.
   IF NOT AVAIL bop THEN RETURN vRet.

   vDateDoc = DATE(GetXAttrValueEx("op",STRING(bop.op),"��⠏����",STRING(bop.doc-date))) NO-ERROR.
   IF ERROR-STATUS:ERROR OR vDateDoc < vDateMCI96 THEN RETURN vRet.

   vBal401 = {assigned bop.ben-acct} AND CAN-DO(mBal401,bop.ben-acct).

   IF NOT vBal401 THEN RETURN vRet.

   FOR FIRST bop-bank WHERE bop-bank.op = bop.op AND 
                            bop-bank.bank-code-type = "���-9" AND
                            bop-bank.op-bank-type = "" NO-LOCK,
       FIRST banks-code WHERE banks-code.bank-code-type = "���-9"
                          AND banks-code.bank-code = bop-bank.bank-code
                       NO-LOCK,
       FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK,
       FIRST bcode  WHERE bcode.class = "PZN-���"
                     AND bcode.code  = banks.bank-type 
                   NO-LOCK:
       IF NOT {assigned iPokSt} OR CAN-DO('0,00',iPokSt) THEN
       DO:
          IF vQues = "��" THEN
          DO: 
          RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""����� ���⥫�騪� (101)""" ).
          vRet = NO.
          END.
          IF vQues = "��" THEN
          DO:
          RUN Fill-SysMes IN h_tmess ("","","4","�� ��������� ���� ""����� ���⥫�騪� (101)"". 
~�த������?").
          vRet = (pick-value = "YES").
          END.
       END.
   END.
   
   RETURN vRet.

END FUNCTION.

/* �������� ���� KppSend*/
FUNCTION ValidateKppSend RETURN LOGICAL (INPUT ipKppSend AS CHARACTER):

   DEFINE VARIABLE vRet  AS LOGICAL INIT YES NO-UNDO.

   IF NOT {assigned ipKppSend}
      AND ({assigned mPokSt} OR mConstControl) THEN
   DO:
      RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��� ���⥫�騪� (102)""" ).
      vRet = NO.
   END.

   RETURN vRet.

END FUNCTION.

/* �������� ���� KppRec*/
FUNCTION ValidateKppRec RETURN LOGICAL (INPUT ipKppRec AS CHARACTER):

   DEFINE VARIABLE vRet  AS LOGICAL INIT YES NO-UNDO.

   IF NOT {assigned ipKppRec}
      AND ({assigned mPokSt} OR mConstControl) THEN 
   DO:
      RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��� �����⥫� (103)""" ).
      vRet = NO.
   END.

   RETURN vRet.

END FUNCTION.

/*
   �㭪�� �।�����祭� ��� ����஫� ���४⭮�� �ଠ� ����
   ��⭮� �����䨪�樨 � ������ �������� ��⨭� ⮫쪮 ��
   �ࠢ��쭮� �ଠ� ��।������ �� ���祭�� ���. �ࠢ����
   ��⠥��� �� �� ᫥����� �ଠ⮢:
      - ?;
      - �ந����쭮� ������⢮ �஡����;
      - ஢�� 20 ���;
*/
FUNCTION ValidateBCCFormat RETURN LOGICAL (INPUT vBCC AS CHARACTER):

   DEFINE VARIABLE vItem AS INT64 NO-UNDO.

   IF vBCC =  ? OR TRIM(vBCC) =  "" THEN
   DO:
      IF {&ManualRun} THEN DO:
         IF NOT ({assigned mPokSt} OR mConstControl) THEN RETURN YES.
         {messmenu                                                        
            &text    = "�� ��������� ���� ��� (104).    �த������ ����?   "
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��� (104)""" ).
         RETURN NO.
      END.
   END.

   IF vBCC =  "0" THEN
   DO:
      IF {assigned op.ben-acct} AND CAN-DO(mBal401,op.ben-acct) THEN
         RETURN NO.
      ELSE
         RETURN YES.
   END.

   IF LENGTH(vBCC) <> 20 THEN
      RETURN NO.

   IF CAN-DO(mKBKNalog,vBCC) THEN
      DO vItem = 1 TO 20:

         IF LOOKUP(SUBSTRING(vBCC, vItem, 1), 
                   "0,1,2,3,4,5,6,7,8,9") =  0 THEN
            RETURN NO.

      END.
   ELSE
      DO vItem = 1 TO 20:

         IF LOOKUP(SUBSTRING(vBCC, vItem, 1), 
                   {&ALLOWEDKBKSYM}) =  0 THEN
            RETURN NO.

      END.
   /* ����� 20 �㫥� */
   IF vBCC =  FILL("0",20) THEN
      RETURN NO.

   RETURN YES.

END FUNCTION.

/* �������� ���� PokOp*/
FUNCTION ValidatePokOp RETURN LOGICAL (INPUT ipPokOp AS CHARACTER):

   IF ipPokOp =  ? OR TRIM(ipPokOp) =  "" THEN
   DO:
      IF {&ManualRun} THEN DO:
         {messmenu                                                        
            &text    = "�� ��������� ���� �᭮����� (106). �த������ ����?"
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""�᭮����� (106)""" ).
         RETURN NO.
      END. 
   END.

END FUNCTION.

/* �������� ���� ��ਮ� (107)*/
FUNCTION ValidatePokNP1 RETURN LOGICAL (INPUT ipPokNP1 AS CHARACTER):

   IF ipPokNP1 =  ? OR TRIM(ipPokNP1) =  "" THEN
   DO:
      IF {&ManualRun} THEN DO:
         {messmenu                                                        
            &text    = "�� ��������� ���� ��ਮ� (107).    �த������ ����?  "
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��ਮ� (107)""" ).
         RETURN NO.
      END. 
   END.

END FUNCTION.

/* �������� ���� ����� (108)*/
FUNCTION ValidatePokND RETURN LOGICAL (INPUT ipPokND AS CHARACTER):

   DEFINE VARIABLE vIp AS INT64       NO-UNDO.

   IF ipPokND =  ? OR TRIM(ipPokND) =  "" THEN
   DO:
      IF {&ManualRun} 
         OR mBudPay THEN 
      DO:
         {messmenu                                                        
            &text    = "�� ��������� ���� ����� (108).    �த������ ����?  "
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""����� (108)""" ).
         RETURN NO.
      END.
   END.
   DO vIp = 1 TO LENGTH(ipPokND):
     IF    SUBSTRING(ipPokND, vIp, 1) =  "�" 
        OR SUBSTRING(ipPokND, vIp, 1) =  "N"
        OR (mBudPay AND SUBSTRING(ipPokND, vIp, 1) =  "-")  THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","0","������� '�','N'" + 
                                              (IF mBudPay THEN ",'-'" ELSE "") +
                                              " �� �����⨬�").
        RETURN NO.
     END.
   END.

END FUNCTION.

/* �������� ���� ��� (109)*/
FUNCTION ValidatePokDD RETURN LOGICAL (INPUT ipPokDD AS CHARACTER):

   DEFINE VARIABLE vS AS CHARACTER   NO-UNDO.

   IF ipPokDD =  ? OR TRIM(ipPokDD) =  "" THEN
   DO:
      IF {&ManualRun} THEN DO:
         {messmenu                                                        
            &text    = "�� ��������� ���� ��� (109).    �த������ ����?  "
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��� (109)""" ).
         RETURN NO.
      END.
   END.
   ELSE DO:
      IF ipPokDD <> "0" THEN DO:
         ASSIGN vS = STRING(DATE(ipPokDD),"99.99.9999") NO-ERROR.
         IF ERROR-STATUS:ERROR
         OR NOT {assigned vS}
         THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","���ࠢ��쭮 ������� ���. ~n ��ଠ� ���� ������ ���� ��.��.����").
            RETURN NO.
         END.
      END. 
   END.

END FUNCTION.

/* �������� ���� ��� (110)*/
FUNCTION ValidatePokTP RETURN LOGICAL (INPUT ipPokTP AS CHARACTER):

   IF (ipPokTP =  ? OR TRIM(ipPokTP) =  "") AND (NOT mFlNewFmt) AND mShowPokTp 
   THEN
   DO:
      IF {&ManualRun} THEN DO:
         {messmenu                                                        
            &text    = "�� ��������� ���� ��� (110).       �த������ ����?  "
            &choices = "��,���"                     
            &update  = 1                                                
         }
         IF pick-value =  "1" THEN
            RETURN YES.
         ELSE
            RETURN NO.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess("", "", -1, "�� ��������� ���� ""��� (110)""" ).
         RETURN NO.
      END.
   END.
END FUNCTION.

/* ����ࢭ�� ����஢���� ������ ४����� */
PROCEDURE PackTaxAttr PRIVATE:
   DEFINE INPUT-OUTPUT PARAMETER pKVList    AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER iAttrName  AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER iAttrValue AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vI AS INT64     NO-UNDO.
   DEFINE VARIABLE vS AS CHARACTER NO-UNDO.

   IF pKVList = ? THEN
      pKVList = "".
   DO vI = 1 TO NUM-ENTRIES(pKVList, ";"):
      vS = ENTRY(vI, pKVList, ";").
      IF {assigned vS} THEN DO:
         vS = ENTRY(1, vS, "=").
         IF vS = iAttrName THEN
            ENTRY(vI, pKVList, ";") = "".
      END.
   END.
   vS = IF iAttrValue = ?
        THEN ?
        ELSE SUBSTITUTE("&1=&2", iAttrName, iAttrValue).
   IF {assigned vS} THEN
      {additem2.i pKVList vS ";"}
END PROCEDURE.

/* �������� १�ࢭ�� ����� ��������� ४����⮢ ���㬥�� � ४����⮢ ���� */
PROCEDURE BackupTaxAttrs PRIVATE:
   DEFINE INPUT PARAMETER iOp LIKE op.op NO-UNDO.

   DEFINE BUFFER op FOR op.

   DEFINE VARIABLE vS AS CHARACTER NO-UNDO.

   IF iOp = ? THEN
      vS = ?.
   ELSE DO:
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mPokST"  , mPokST  ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mKppSend", mKppSend).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mKppRec" , mKppRec ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mKBK"    , mKBK    ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mOKATO"  , mOKATO  ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mPokOP"  , mPokOP  ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mPokNP"  , mPokNP1 ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mPokND"  , mPokND  ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mPokDD"  , mPokDD  ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mUIN  "  , mUIN    ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mEIP  "  , mEIP    ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mAIP  "  , mAIP    ).
      RUN PackTaxAttr IN THIS-PROCEDURE (INPUT-OUTPUT vS, "mUIP  "  , mUIP    ).
      vS = SUBSTITUTE("&1|&2", iOp, vS).
   END.
   RUN SetSysConf IN h_base ({&TAX-ATTRS-BACKUP}, vS).
END PROCEDURE.

/* ����㧪� १�ࢭ�� ����� ��������� ४����⮢ ���㬥�� � ४����⮢ ���� */
PROCEDURE RestoreTaxAttrs PRIVATE:
   DEFINE INPUT PARAMETER iOp LIKE op.op NO-UNDO.

   DEFINE BUFFER op FOR op.

   DEFINE VARIABLE vS AS CHARACTER NO-UNDO.

   IF iOp = ? THEN
      RUN ResetTaxAttrs IN THIS-PROCEDURE.
   ELSE DO:
      vS = GetSysConf({&TAX-ATTRS-BACKUP}).
      IF ENTRY(1, vS, "|") = STRING(iOp) THEN DO:
         vS = IF NUM-ENTRIES(vS, "|") = 1
              THEN ""
              ELSE ENTRY(2, vS, "|").
         ASSIGN
            mPokST   = GetParamByNameAsChar(vS, "mPokST"  , ?)
            mKppSend = GetParamByNameAsChar(vS, "mKppSend", ?)
            mKppRec  = GetParamByNameAsChar(vS, "mKppRec" , ?)
            mKBK     = GetParamByNameAsChar(vS, "mKBK"    , ?)
            mOKATO   = GetParamByNameAsChar(vS, "mOKATO"  , ?)
            mPokOP   = GetParamByNameAsChar(vS, "mPokOP"  , ?)
            mPokNP1  = GetParamByNameAsChar(vS, "mPokNP"  , ?)
            mPokND   = GetParamByNameAsChar(vS, "mPokND"  , ?)
            mPokDD   = GetParamByNameAsChar(vS, "mPokDD"  , ?)
            mUIN     = GetParamByNameAsChar(vS, "mUIN"    , ?)
            mEIP     = GetParamByNameAsChar(vS, "mEIP"    , ?)
            mAIP     = GetParamByNameAsChar(vS, "mAIP"    , ?)
            mUIP     = GetParamByNameAsChar(vS, "mUIP"    , ?)
         .
      END.
      ELSE
         RUN ResetTaxAttrs IN THIS-PROCEDURE.
   END.
END PROCEDURE.

/* �������� १�ࢭ�� ����� ��������� ४����⮢ ���㬥�� � ४����⮢ ���� */
PROCEDURE ResetTaxAttrs PRIVATE:
   RUN DeleteOldDataProtocol IN h_base ({&TAX-ATTRS-BACKUP}).
END PROCEDURE.

{intrface.del}
/* $LINTFILE='nalpl_ed.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.192+03:00' */
/*prosignLC83MxD/euQsVRwl0kSidA*/