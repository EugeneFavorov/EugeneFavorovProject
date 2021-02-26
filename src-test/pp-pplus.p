/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "���� ����"
     Filename: pp-pplus.p
      Comment: (0019702) ������⥪� �㭪権
               ����� ���� ����
   Parameters: ���
      Created: 12.09.2003 12:40 guva
*/

{globals.i}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get xclass}
{intrface.get tparam}
{intrface.get rights}
{intrface.get data}
{intrface.get db2l}
{intrface.get count}
{intrface.get strng}
{intrface.get brnch}
{intrface.get refer}
{intrface.get cust}
{intrface.get osyst}
{intrface.get print}
{intrface.get prnvd}
{intrface.get kau}
{intrface.get parsr}
{intrface.get widg}
{intrface.get tmcod}
{intrface.get oldpr}
{initstrp.i}

{form.def}
{tmprecid.def}
{g-trans.equ}
{ksh-defs.i}
{typeclentbyacct.i}
DEFINE VARIABLE mOpDate       AS DATE       NO-UNDO.
DEFINE VARIABLE mTmplID       AS INT64    NO-UNDO.
DEFINE VARIABLE mProcHdl      AS HANDLE     NO-UNDO.
DEFINE VARIABLE mQTemplate    AS HANDLE     NO-UNDO.
DEFINE VARIABLE mDebugLevel   AS INT64    NO-UNDO.
DEFINE VARIABLE mOpkind       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBreak        AS INT64    NO-UNDO.

DEFINE STREAM sSpoolTXT.
DEFINE VARIABLE mMaxWidthTxt AS INT64   NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDate446     AS DATE      NO-UNDO.
DEFINE VARIABLE mDate446Str  AS CHARACTER NO-UNDO.

mDate446Str = FGetSetting("��446�","���446�","").
IF LENGTH(mDate446Str) GT 0 THEN DO:
   mDate446 = DATE(mDate446Str) NO-ERROR.
END.
/****************************************************************************/
DEF temp-table tt-tmprecop NO-UNDO
    FIELD op LIKE op.op
.
/*****************************************************************************
 *       ������� ������ ��� �ࠢ����� 横���᪨� �믮������� �࠭���樨 *
 ****************************************************************************/
/* Commented BY KSV: �࠭�� ���ଠ�� ��� ��� 横���, ����饭��� ��
** �࠭���樨 */
DEFINE TEMP-TABLE tLoop NO-UNDO
   FIELD loop-id     AS INT64
   FIELD src-op-kind AS CHARACTER
   FIELD dst-op-kind AS CHARACTER
   FIELD counter     AS INT64
   INDEX iLoop-id IS PRIMARY UNIQUE loop-id
   INDEX iDst-op-kind dst-op-kind
   INDEX iSrc-op-kind src-op-kind.
/* Commented BY KSV: ���稪 横��� - ���� �뤠�� ��᫥����⥫쭮��� 楫��
** �ᥫ */
DEFINE VARIABLE mLoopSeq AS INT64    NO-UNDO.

/* Commented BY KSV: ����⨥ ������ 横�� */
&GLOBAL-DEFINE CREATE_TLOOP CREATE tLoop. ~
                            ASSIGN ~
                              tLoop.src-op-kind = mOpkind ~
                              tLoop.loop-id     = mLoopSeq + 1. ~
                            mLoopSeq = mLoopSeq + 1. ~
                            DEFINE VARIABLE vLoopID AS INT64    NO-UNDO. ~
                            vLoopID = tLoop.loop-id.
/* Commented BY KSV: ����祭�� ���ଠ樨 � ⥪�饬 横�� */
&GLOBAL-DEFINE GET_LOOPINFO FIND FIRST tLoop WHERE tLoop.loop-id = vLoopID NO-ERROR.
/* Commented BY KSV: �������� ���ଠ樨 � 横�� */
&GLOBAL-DEFINE DEL_TLOOP    ~{&GET_LOOPINFO~} ~
                            IF AVAIL tLoop THEN DELETE tLoop.

/* "�ਥ����" �����頥��� ���祭��. */
{ttretval.def}

/* Commented BY KSV: �⥪ ����饭��� �࠭���権. �࠭�� ���⥪�� ����饭���
** �࠭���権 */
DEFINE TEMP-TABLE tStack NO-UNDO
   FIELD fStackID          AS INT64     /* �����䨪��� �⥪� */
   FIELD fTmplID           AS INT64     /* �����䨪��� 蠡���� */
   FIELD fOpkind           AS CHARACTER   /* �����䨪��� �࠭���樨  */
   FIELD fOpdate           AS DATE        /* ��� ���भ� */
   FIELD fProcHdl          AS HANDLE      /* ��� ��楤��� �࠭���樨 */
   FIELD fObjTransaction   AS INT64     /* �����䨪��� ����樨 */
   FIELD fOpTransaction    AS INT64     /* �����䨪��� ��ꥪ� ����樨 */
   FIELD fQTemplate        AS HANDLE      /* ��� ����� �� 蠡����� �࠭���樨 */
   INDEX iStackID IS PRIMARY UNIQUE fStackID
   .

&GLOBAL-DEFINE PUSH_TSTACK CREATE tStack.~
                           ASSIGN ~
                              tStack.fStackID = mLoopSeq + 1 ~
                              tStack.fTmplID  = mTmplID ~
                              tStack.fOpkind  = mOpkind ~
                              tStack.fProcHdl = mProcHdl ~
                              tStack.fQTemplate = mQTemplate ~
                              tStack.fOpdate  = mOpDate. ~
                           tStack.fObjTransaction = INT64(GetSysConf("obj-transaction")) NO-ERROR.~
                           tStack.fOpTransaction = INT64(tGetParam("op-transaction",?,?)) NO-ERROR. ~
                           mLoopSeq = mLoopSeq + 1.
&GLOBAL-DEFINE POP_TSTACK  FIND LAST tStack NO-ERROR. ~
                           IF AVAILABLE tStack THEN ~
                           DO: ~
                              ASSIGN ~
                                 mTmplID  = tStack.fTmplID ~
                                 mOpkind  = tStack.fOpkind ~
                                 mProcHdl = tStack.fProcHdl ~
                                 mQTemplate = tStack.fQTemplate ~
                                 mOpDate  = tStack.fOpdate. ~
                              RUN SetSysConf IN h_base ("obj-transaction",tStack.fObjTransaction). ~
                              RUN tSetParam IN h_tparam ("op-transaction",tStack.fOpTransaction,mProcHdl,?). ~
                              DELETE tStack. ~
                           END. ~
                           ELSE ~
                           DO: ~
                              RUN Fill-SysMes("","trans30","","%s=" + mOpkind + ~
                                                              "%s=" + STRING(mTmplID)). ~
                              UNDO, RETURN ERROR. ~
                           END.


{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PPLUS"
   &LIBNAME       = "������⥪� �㭪権 ����� ���� ����."
   &DESCRIPTION   = "����� � 蠡������ � �࠭����ﬨ, ���� ����."
   }

{pfuncdef
   &NAME          = "�����"
   &DESCRIPTION   = "�����頥� ��� ���� ⥪��. ���짮��⥫�. �᫨ ᯨ᮪ 䨫����� ~
   �� �����, ࠡ�⠥� �� ��� 䨫�����. �᫨ ����� - �� 䨫����� �� ᯨ᪠ ~
   �����頥� ��� ����, �� ��㣨� 䨫����� - �����頥� ��� ���ࠧ�������. ~
   &PARAMETERS    = "[������ ��������]"
   &RESULT        = "��� ������"
   &SAMPLE        = "�����('0500') = '0500' - ��� ���� ⥪�饣� ���짮��⥫�  ~n~
�����() = '0500' - ���ࠧ������� ⥪�饣� ���짮��⥫�"
   }
   DEFINE INPUT  PARAMETER iFilialList AS CHARACTER   NO-UNDO.
/*   DEFINE INPUT  PARAMETER iUserID     AS CHARACTER   NO-UNDO. */
   DEFINE OUTPUT PARAMETER out_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.

   DEFINE VARIABLE mUserID AS CHARACTER NO-UNDO.

   out_result = "".

   iFilialList = TRIM(iFilialList,"~"'").
   mUserID = USERID("bisquit").
   IF iFilialList EQ ? THEN
      iFilialList = "*".
   IF CAN-DO(iFilialList, shFilial) THEN
      out_result = GetXAttrValueEx("_user",mUserID,"office","").
   IF out_Result EQ "" THEN
      out_result = GetUserBranchId(mUserID).
END PROCEDURE.


{pfuncdef
    &NAME          = "�����2"
    &DESCRIPTION   = "�����頥� ��� ���� ⥪��. ���짮��⥫�. �᫨ ᯨ᮪ 䨫�
    �� �����, ࠡ�⠥� �� ��� 䨫�����. �᫨ ����� - �� 䨫����� �� ᯨ᪠ ~
    �����頥� ��� ����, �� ��㣨� 䨫����� - �����頥� ��� ���ࠧ�������. ~
    &PARAMETERS    = "[������ ��������]"^
    &RESULT        = "��� ������"
    &SAMPLE        = "�����2('0500') = '0500' - ��� ���� ⥪�饣� ���짮��⥫�
    �����2() = '0500' - ���ࠧ������� ⥪�饣� ���짮��⥫�"
    }
    
       DEFINE INPUT  PARAMETER iFilialList AS CHARACTER   NO-UNDO.
       /*   DEFINE INPUT  PARAMETER iUserID     AS CHARACTER   NO-UNDO. */
          DEFINE OUTPUT PARAMETER out_Result  AS CHARACTER   NO-UNDO.
             DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
    DEFINE VARIABLE mUserID AS CHARACTER NO-UNDO.
    out_result = "".
    iFilialList = TRIM(iFilialList,"~"'").
    mUserID = USERID("bisquit").
    IF iFilialList EQ ? THEN
    iFilialList = "*".
    IF CAN-DO(iFilialList, shFilial) THEN
    out_result = GetXAttrValueEx("_user",mUserID,"office","").
    IF substr(out_result,1,4) EQ "0598"
    THEN out_result = "0598".
    IF out_Result EQ "" THEN
    out_result = GetUserBranchId(mUserID).
    END PROCEDURE.