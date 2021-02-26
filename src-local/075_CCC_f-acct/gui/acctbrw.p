/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACCT().P
      Comment: ��楤�� ��ᬮ�� �/�
   Parameters:
         Uses:
      Used BY:
      Created: ??? ??/??/????
     Modified: Om 21/10/2004
     Modified: grab 41465 - JOIN ��뢠���� �� ����奬�
     Modified: Om 14/05/2005 ��ࠡ�⪠.
                        ��� ��⮢ �ਤ��᪨� ��� �⮡ࠦ��� � ������������
                        ��� ��⪮� �������� �ਤ��᪮�� ���.
     Modified: 21.01.2010 19:19 ksv      (0121399) + &qbisxtrafields
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */
{navigate.def}          /* ��६���� ��� navigate.cqr. */
{xsignrat.def}          /* ��६���� ��� ����ୠ⨢��� ��� ��ᬮ�� ��. */
{chkacces.i}            /* �஢�ઠ ����㯠 � �����⠬ (getcust.i). */
{sh-defs.i}             /* ��६���� ��� �ନ஢���� ��⠪� �� ��⠬. */
{ttretval.def}

{intrface.get acct}     /* ������⥪� ��� ࠡ��� � ��⠬�. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */
{intrface.get separate} /* ������⥪� ��� ࠡ��� � ��⥣��ﬨ. */
{intrface.get widg}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get dynqr}    /* ������⥪� ��� ࠡ��� � �������᪨�� ����ᠬ�. */
{intrface.get op}
{intrface.get navst}

DEF VAR name        AS CHAR   NO-UNDO EXTENT 2. /* ��� ������������ ���. */
DEF VAR proc-name   AS CHAR   NO-UNDO.          /* ��� ��楤��� ��⮤�. */
DEF VAR mRecId      AS RECID  NO-UNDO.          /* 4 conversion string to recid. */
DEF VAR sort-type   AS INT64  NO-UNDO.          /* ��� ���஢�� �� F5. */
DEF VAR closedate   AS DATE   NO-UNDO.          /* ��� ������� ��⥣�ਨ */
DEF VAR mAcctHndl   AS HANDLE NO-UNDO.          /* ��� ��������� ४����⮢ ����. */
DEF VAR mAcctRead   AS CHAR   NO-UNDO.          /* �᫮��� ��� �஢�ન �ࠢ ����㯠. */
DEF VAR mAcctRid    AS ROWID  NO-UNDO.          /* ����樮��஢���� �� ��४��祭�� ��. */
DEF VAR mEndDate    AS DATE   NO-UNDO.          /* ��� ���᪠ ���⪮� �� ����. */
DEF VAR mAcct       AS CHAR   NO-UNDO.          /* ��� �ଠ�஢������ �뢮�� �� �࠭. */
DEF VAR mProcName   AS CHAR   NO-UNDO.          /* ��� ��楤���. */
DEF VAR mProcPrms   AS CHAR   NO-UNDO.          /* ��ࠬ���� ��楤���. */
DEF VAR mNameAcct   AS LOG    NO-UNDO.          /* ���� �ନ஢���� �������� ���. */
DEF VAR mFrameExc   AS CHAR   NO-UNDO.          /* ���᮪ ���⮡ࠦ����� �३���. */
DEF VAR mSec-code   AS CHAR   NO-UNDO.
DEF VAR mPosBal     AS DEC    NO-UNDO.          /* ���⮪ ��� � ���� (�ଠ 5, 6). */
DEF VAR mPosBalScr  AS DEC    NO-UNDO.          /* ���⮪ ��� � ���� (�ଠ 5, 6). */
DEF VAR mPosVal     AS DEC    NO-UNDO.          /* ���⮪ ��� � ����� ��� (�ଠ 5). */
DEF VAR mPosValScr  AS DEC    NO-UNDO.          /* ���⮪ ��� � ����� ��� (�ଠ 5). */
DEF VAR mPosQty     AS DEC    NO-UNDO.          /* ���⮪ ��� � ��㪠� (�ଠ 6). */
DEF VAR mPosQtyScr  AS DEC    NO-UNDO.          /* ���⮪ ��� � ��㪠� (�ଠ 6). */
DEF VAR mAvBal      AS DEC    NO-UNDO.          /* ���⮪ ��� � ���� (�ଠ 8). */
DEF VAR mAvVal      AS DEC    NO-UNDO.          /* ���⮪ ��� � ����� ��� (�ଠ 8). */
/* ��६���� ��� 䨫���樨 �� ���⪠� � ����⠬ �� ��⠬ */
DEF VAR SldType     AS CHAR   NO-UNDO.          /* ��� ᠫ줮 (���,�।,*) - ��� 䨫��� */
DEF VAR mSldDate    AS DATE   NO-UNDO.          /* ��� ���� ���⪮� �� ��⠬ */
DEF VAR mTurnBeg    AS DATE   NO-UNDO.          /* ��� ���. ����. ���� ����⮢ �� ��⠬ */
DEF VAR mTurnEnd    AS DATE   NO-UNDO.          /* ��� �����. ����. ���� ����⮢ �� ��⠬ */
DEF VAR sh-b        AS DEC    NO-UNDO.          /* ������ �� ���� � ���.����� */
DEF VAR sh-v        AS DEC    NO-UNDO.          /* ������ �� ���� � ��.����� */
DEF VAR mTurnType   AS CHAR   NO-UNDO.          /* ⨯ ����⮢ (���,�।,*) */
DEF VAR vFiltTurn   AS LOG    NO-UNDO.          /* �����⢫��� �� 䨫����� �� ����⠬ */
DEF VAR vFiltSld    AS LOG    NO-UNDO.          /* �����⢫��� �� 䨫����� �� ���⪠� */
DEF VAR mColCloAc   AS LOG    NO-UNDO.          /* ���� �����⮣� ��� */
DEF VAR vDate       AS DATE   NO-UNDO.          /* ��� �� ������ ��������� ���⪨. */
DEF VAR currency    AS CHAR   NO-UNDO.
DEF VAR mNumFrm     AS INT64  NO-UNDO.
DEF VAR mAvGrList   AS CHAR   NO-UNDO.         /* ����㯭� ⥪�饬� ���짮��⥫� ��㯯� �����⮢ */
DEF VAR mPersGr     AS CHAR   NO-UNDO.         /* ��� ������ (��㯯�, �� �������), ��� 䨫���樨 �� ����. ���� */
DEF VAR mAccessMask AS CHAR   NO-UNDO.
DEF VAR mAccessContAcct AS CHARACTER NO-UNDO.
DEF VAR mOK         AS LOG    NO-UNDO.
DEF VAR mLinkSurr   AS CHAR   NO-UNDO.
DEF VAR mGrpFltLst  AS CHAR   NO-UNDO.          /* ���祭�� ���� 䨫��� "��㯯�" - ᯨ᮪ ��㯯 �� ����� �����⢫���� 䨫����� */
DEF VAR mFltGrpType AS CHAR   NO-UNDO.          /* ���祭�� ���� 䨫��� "�⡨��� ��� �ਭ������騥" */
DEF VAR GroupList   AS CHAR   NO-UNDO.          /* ��६����� ��� 䨫���樨 �� ����塞� ���� */
DEF VAR mAcctGroups AS CHAR   NO-UNDO.          /* ᯨ᮪ ��㯯 ��।���� ��� */
DEF VAR mGroupCnt   AS INT64  NO-UNDO.          /* ���稪 ��� ��ॡ�� ᯨ᪠ ��㯯 */
DEF VAR mAcctGrOn   AS LOG    NO-UNDO.          /* ����祭� �� ��࠭�祭�� �� ��㯯�� ����㯠 */
DEF VAR mBrwRole    AS CHAR   NO-UNDO.
DEF VAR mK2         AS LOG    NO-UNDO.          /* ���뢠�� ���㬥��,���騥 � �2, �� ���� ���⪠ (�ଠ 8)*/
DEF VAR mCrTurnVal  AS DEC    NO-UNDO.          /* �।�⮢� ����� ��� � ���    (�ଠ 9). */
DEF VAR mCrTurn     AS DEC    NO-UNDO.          /* �।�⮢� ����� ��� � �㡫�� (�ଠ 9). */
DEF VAR mDbTurnVal  AS DEC    NO-UNDO.          /* ����⮢�  ����� ��� � ���    (�ଠ 9). */
DEF VAR mDbTurn     AS DEC    NO-UNDO.          /* ����⮢�  ����� ��� � �㡫�� (�ଠ 9). */
DEF VAR mAccessStatus AS CHAR  NO-UNDO.         /*���祭�� �� "AccessStatus"*/ 
DEFINE VARIABLE mFrmLabelLst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mMyFrmLst AS CHARACTER   NO-UNDO.

FIND FIRST user-config NO-LOCK 
   WHERE user-config.USER-ID = userid('bisquit')
     AND user-config.proc-name EQ "UserFrames_acctbrw"
NO-ERROR.
IF AVAIL user-config THEN mMyFrmLst = REPLACE(user-config.DESCR," ",",") .
ELSE mMyFrmLst = "" .

DEF BUFFER gacct FOR acct.

{tmpobj.def &pref=Local}

/* �ந������ ������� ��� ��⥣�ਨ, "" �࠭��ନ�㥬 � "*". */
IF GetFltVal ("acct-cat") EQ ""
THEN DO:
   RUN SetFltFieldList ("acct-cat", "*"). /* �����뢠�� � ��ப�. */
   RUN SetFltField     ("acct-cat", "*"). /* �����뢠�� ��������. */
END.

IF       GetFltValEx ("acct-cat", "") NE ""
   AND   GetFltVal   ("acct-cat") NE "*"
THEN closedate = Get_Date_Cat (GetFltVal ("acct-cat"),gend-date).

/* �᫨ ��⥣��� ����।�����,
** � �饬 ��᫥���� ������� �� �� ��易⥫�� ��⥣���. */
IF closedate EQ ?
   THEN closedate = FGetLastClsDate(gend-date, Get_StatClose_Cats(gend-date)). 
IF closedate EQ ?
   THEN closedate = gend-date.
                        /* ��।��塞 ��⮤ �ନ஢���� ������������ ���. */
RUN GetClassMethod IN h_xclass (
   GetFltVal ("iClassCode"),  /* ����� ��ꥪ�. */
   "BrwVal",                  /* ��� ��⮤�. */
   "Details",                 /* ��� ४�����. */
   "",                
   OUTPUT mProcName,          /* �������� ��⮤�. */
   OUTPUT mProcPrms           /* ��ࠬ���� ��⮤�. */
).
ASSIGN
                        /* ��⠭�������� 䫠� ��� �ନࢮ���� ������������ ���. */
   mFrameExc   = fGetSetting ("AcctPosColors","CA", ?)
   mColCloAc   = {assigned mFrameExc}
   mNameAcct   = mProcName EQ ?
                        /* �⪫�砥� ��� � ����ᨬ��� �� ०��� ��ᬮ��. */
   mFrameExc   = IF shModeMulty THEN "1" ELSE "4"
                        /* ��⠭�������� ���� ���� ���⪮� �� ��⠬ �� ����஥� 䨫���
                        ** �� 㬮�砭�� - ������쭠� ��� gend-date */
   mSldDate    = DATE(GetFltVal("SldDate"))
   
   mAvGrList   = GetRightPersGroup()
   mBrwRole    = GetFltValEx ("BrwRole", "")
.
      /* ��⠭�������� 䨫��� - ����㯭� ⥪�饬� ���짮��⥫� ��㯯� �����⮢ */
RUN SetFltField("mPersGr",mAvGrList).

IF GetFltValEx("view-type", "0") EQ "5" THEN
   mFrameExc = mFrameExc + ",2,5".

mAccessMask = FGetSetting("�⠭���", "AccessAcct", "").
mAccessContAcct  = FGetSetting("�⠭���", "AccessContAcct", "").

{acctbrw.frm}           /* ���� ��㧥� ��楢�� ��⮢. */
{acct.qry}              /* ������ ��� ��㧥�. */

/* &oh6        = "�F6 䨫���" */
/*  */

/* Commented by SOAV: �᫨ �� ����㦥� 䨫���, � ���஢�� ��-㬮�砭�� */
IF NUM-ENTRIES (user-config-info) LT 3 OR ENTRY (3, user-config-info) EQ "?" THEN
DO:
   sort-type = INT64(FGetSetting("�����", "", "1")).
   RUN SetFltField      ("sort-type", STRING (sort-type)).
   RUN SetFltFieldList  ("sort-type", STRING (sort-type)).
END.

{navigate.cqr
   &file       = "acct"
   &postfind   = "acctbrw.fnd "
   &repaint    = "acctbrw.rpt "

   &access-read      = "R"
   &access-cond      = " mAcctGrOn AND NOT GetAcctByGroupPermissionUser (acct.acct + ',' + acct.currency,'r',USERID('bisquit')) "

   &nav-permission-off = YES

   &autofind         = "!mPosVal*,!mPosBal*,!mPosQty*,*"
   &autofast         = "!mPosVal*,!mPosBal*,!mPosQty*,*"
   &autofind-no-file = "name[1] mPosVal mPosBal mPosVal mPosBalScr mPosBal mPosBalScr mPosQty mPosQtyScr"
   &autofast-no-file = "name[1] mPosVal mPosBal mPosVal mPosValScr mPosBal mPosBalScr mPosQty mPosQtyScr"
   &manualWhere      = "AcctManual"

   &CalcFld = "mPosVal mPosBal mPosQty SldType sh-v sh-b mPersGr GroupList"
   &CalcVar = "acctbrw.cv "

   &SrchFld       = "mAcct|currency|mPosVal|mPosBal|mPosQty"
   &SrchFile      = "acct|acct|||"
   &vSrchAltFld   = "(IF shMode THEN 'acct.number' ELSE 'acct.acct') + '|acct.currency|mPosVal,mPosValScr|mPosBal,mPosBalScr|mPosQty,mPosQtyScr'"
   &SrchFormat    = "GetAcctFmt (GetFltVal('acct-cat')) + '||||' + GetNullStr(GetFmtQty('', 'acct', 21, 7) + ' �')"

   &AutoSigns  = YES
   
   &look       = "b-acct.nav "

   &oh1        = "�F2 ����"
   &oth1       = "acct().mnu "

   &oh3        = "�F3 �ଠ�CTRL-F3 ����" 
   &oth3       = "frames.cqr "
      &user-frames_cqr  = "RUN SetFltField      ('view-type', STRING (n-frm - 1)).
                           RUN SetFltFieldList  ('view-type', STRING (n-frm - 1))." 
      &exclfrm          = mFrameExc
      &myfrm            = YES
   
   &filt       = YES
   &oth6       = "flt-file.f6 &runt-set-filtr=YES"
   
   &oh8        = "�F8 ����"
   &oth8       = "acct.cl "

   &maxfrm     = 9
   &bf1        = "currency acct.contract name[1] code.val"
   &cf1        = "mAcct currency acct.contract name[1] code.val"
   &bf2        = "currency"
   &cf2        = "mAcct currency"
   &bf3        = "currency signs_value"
   &cf3        = "mAcct currency signs_value"
   &bf4        = "currency acct.filial-id acct.contract name[1] str-recid code.val"
   &cf4        = "mAcct currency acct.filial-id acct.contract name[1] str-recid code.val"
   &bf5        = "currency"
   &cf5        = "mAcct currency mPosVal mPosBal"
   &bf6        = "currency" 
   &cf6        = "mAcct currency mPosQty mPosBal"
   &bf7        = "mAcct currency name[1]"
   &bf8        = "currency" 
   &cf8        = "mAcct currency mAvVal mAvBal"
   &bf9        = "currency" 
   &cf9        = "mAcct currency mDbTurnVal mCrTurnVal mDbTurn mCrTurn "    
   
   &edit       = "b-acct.ef "
   &delete     = "acct.del "
   
   &startup = "xsignrat.st &runt-set-filtr='YES'"
   &oth4    = "xsigns.rat "
      &rat_file      = "acct"
      &rat_class     = "acct.class-code"
      &rat_upclass   = "GetFltVal ('iClassCode')"
      &rat_surr      = "acct.acct + ',' + acct.currency "
      &rat_num       = "3"
      &rat_key       = "F2"
   
   &oh5     = "�F5 ���"
   &oth5    = "b-acct.srt "
         
   &total      = "acctbrw.cal "
   &print      = "b-acct.prt "
   &return     = "b-acct.ret "
   
   &mess-del   = "'������� ��� � ' + STRING(dbuf.number) + (IF dbuf.currency <> '' THEN '/' + string(dbuf.currency) ELSE '') + ' ?'"

   &tmprecid      = YES   
   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
   &AFTER-FILTER  = "RUN AcctAfterFilter. "
   &qbisxtrafields = "acctbrw.qbis "
}

{acct().pro}            /* ��楤�� ��� ��⠭������� ��ਡ�⮢ ���. */
{intrface.del}          /* ���㧪� �����㬥����. */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='22/04/2015 17:12:50.496+04:00' */
/* $LINTUSER='komi' */
/* $LINTMODE='1' */
/* $LINTFILE='acctbrw.p' */
/*prosignC9Jo1920ywEWgQiApeafWA*/