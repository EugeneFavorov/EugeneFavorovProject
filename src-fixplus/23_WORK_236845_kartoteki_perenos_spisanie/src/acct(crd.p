/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-1996 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  acct(crd.p
      Comment:  ��ᬮ�� � �롮� ��������ᮢ�� ��⮢, ࠡ����� � 㪠�����
                蠡����� ���. �室�騩 ��ࠬ��� - ��� 蠡���� ���. �ᯮ�� -
                ����� � ��楤�� ᯨᠭ�� � ����⥪� 2. ������砥��� �१
                �������⥫�� ४����� 蠡���� �஢���� � ����� "��抠�".
                ��� �롨����� �� ᫥���饬� �������:
                  �롨����� �� ��������ᮢ� ���, ࠡ���騥 �� 㪠�������
                蠡���� ���, �� ����� �⢥砥� ����� ���㤭�� ���
                ���稭���� ��� ���㤭���. �� �� ������뢠���� �� 㬮�砭��
                ��࠭�祭�� - ���⮪ �� ��� ������ ���� �⫨祭 �� ��� ��
                ��� ���� � �� ᮮ⢥�����饬 ��� �⮣� ������ �� ������
                �� ���� ������ ���� ���⮪.
         Uses:  -
      Used by:  gcrddec.p
      Created:  15/09/1999 eagle
     modified: 08/05/2002 kostik  0006764 ��ଠ� �����ᮢ��� ���⪠, ����� ��������ᮢ��
                                          ��⮢ � ��⮬ �ࠢ ���짮��⥫�


*/
{globals.i}
{sh-defs.i}
{chkacces.i}
{intrface.get "acct"}
{intrface.get blkob}
{intrface.get cust}
{intrface.get trans}
{intrface.get pbase}

FUNCTION GetBlockPositionAll RETURNS DECIMAL(INPUT iAcct     AS CHARACTER,
                                             INPUT iCurrency AS CHARACTER,
                                             INPUT iDate     AS DATE
                                            ) IN h_blkob.

def input parameter in-code-value like code.code no-undo.

DEF VAR mOnlyUser    AS LOG  NO-UNDO.
DEF VAR mCurrentUser AS CHAR NO-UNDO.
mCurrentUser = userid('bisquit') + "," + getslaves().

DEF VAR mAcctFltN  AS INT64 INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHAR         NO-UNDO.

DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "�� ��⠬ ��"
        VIEW-AS COMBO-BOX INNER-LINES 3
        LIST-ITEMS "�� ��⠬ ��","�� ��⠬ �⤥����� ��", "�� �ᥬ ��⠬" 
        DROP-DOWN-LIST
        NO-UNDO.



FORM
   vAcctFlt   LABEL  "������ �� ��⠬ " 
              FORMAT "x(23)"
              HELP   "������ �� ��⠬ ����⥪� �����஢����� ��⮢"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE "���⠭���� �� ����⥪�".

ON GO OF FRAME frAcct DO:

   CASE vAcctFlt:SCREEN-VALUE IN FRAME frAcct:
      WHEN "�� ��⠬ ��" THEN
      DO:
         mAcctFltN = 1.
      END.
      WHEN "�� ��⠬ �⤥����� ��" THEN
      DO:
         mUsrBranch = GetUserBranchID(userid("bisquit")).
         mAcctFltN = 2.
      END.
      WHEN "�� �ᥬ ��⠬" THEN
      DO:
         mAcctFltN = 3.
      END.
   END CASE.
END.


&GLOB user-rights1 (   (    mOnlyUser                                 ~
                        AND LOOKUP(bfAcct.user-id,mCurrentUser) GT 0  ~
                        AND mAcctFltN EQ 0)                           ~
                    OR (    NOT mOnlyUser)                            ~
                    OR (    bfacct.user-id   EQ USERID("bisquit")     ~
                        AND mAcctFltN        EQ 1)                    ~
                    OR (    bfacct.branch-id EQ mUsrBranch            ~
                        AND mAcctFltN        EQ 2)                    ~
                    OR (    mAcctFltN        EQ 3)                    ~
                   )

IF NUM-ENTRIES(in-code-value,"#") GT 1 THEN
DO:
   IF ENTRY(2,in-code-value,"#") EQ "ShowForm" THEN
   DO:
      UPDATE vAcctFlt
      WITH FRAME frAcct.
   END.
   ELSE 
      mAcctFltN = 0.
 
   ASSIGN
       mOnlyUser = TRUE
       in-code-value = ENTRY(1,in-code-value,"#")
   .
END.
ELSE 
   mAcctFltN = 0.

def var vacct-cat   like acct.acct-cat no-undo.
def var long-acct   as char format "x(24)" no-undo.
def var comment_str as char label "                                   "  no-undo.
def var users       as char no-undo.
def var lstcont     as char initial "�����" no-undo.
def buffer bacct for acct.
def var summ-rr like op-entry.amt-rub no-undo.
def var vdebug as log init no no-undo.
def var ff-card as char no-undo.
def var AccessStat  as char no-undo.
def var AccessAcct  as char no-undo.
def var mTmplCurr   as char no-undo.
DEF VAR name AS CHARACTER EXTENT 2 NO-UNDO.
DEF VAR Store-Position AS RECID NO-UNDO.

DEFINE VARIABLE mNF AS INTEGER   NO-UNDO.

DEF TEMP-TABLE ttacct NO-UNDO
   FIELD acct        LIKE acct.acct
   FIELD acct-view   LIKE acct.acct  /* ����� ��� � ᮮ⢥�����饬 �ଠ� */
   FIELD bacct       LIKE acct.acct
   FIELD bacct-view  LIKE acct.acct  /* ����� ��� � ᮮ⢥�����饬 �ଠ� */
   FIELD curr        LIKE acct.curr
   FIELD bcurr       LIKE acct.curr /* � ��� � ������ �����ᮢ� ��⠬ �ਢ�뢠���� �㡫��� ��������ᮢ�*/
   FIELD name        AS   CHARACTER LABEL "������"
                             FORMAT "x(40)"
   FIELD rec-oa      AS   RECID /* ��� ��뫪� �� ��������ᮢ� ��� */
   FIELD obal        LIKE acct-pos.balance
   FIELD fobal       AS   LOG
   FIELD rec-ba      AS   RECID /* ��� ��뫪� �� �����ᮢ� ��� */
   FIELD bbal        LIKE acct-pos.balance
   FIELD fbbal       AS   LOG
   FIELD bbal-rub    LIKE acct-pos.balance /* ��� �஢�ப �� �ॢ�襭�� ����� */
   FIELD bbal-val    LIKE acct-pos.balance
   FIELD blk-type    AS   CHARACTER LABEL "�����஢��"
   FIELD blk-amt     AS   decimal   LABEL "����. �㬬�"
   FIELD blk-cust    AS   CHARACTER LABEL "����. ������"
INDEX acct   IS PRIMARY acct curr bacct
INDEX wacct             fobal fbbal acct curr bacct
.

find code where code.class eq "�������"
            and code.code  eq in-code-value
                           no-lock no-error.
if not avail code then return.
{kautools.lib}
ff-card = FGetSetting("�⠭���", "findcard2", "���").
AccessStat = FGetSetting("�⠭���", "AccessStatus", "�").
AccessAcct = FGetSetting("�⠭���", "AccessAcct", "").

mTmplCurr = GetSysConf("��������").
if mTmplCurr eq ? then mTmplCurr = "*".

/*-------------------------------------------------*/
/* �����頥� ᠬ�� �ਮ����� �����஢�� ���. 
   �᫨ ⠪�� ���, ��୥� "". */
/*-------------------------------------------------*/
FUNCTION GetMainBlock RETURNS CHARACTER (
    INPUT iAcct     AS CHARACTER,
    INPUT iCurrency AS CHARACTER,
    INPUT iDateTime AS DATETIME):

    DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vPrevBlock AS CHARACTER NO-UNDO.

    DEFINE BUFFER acct FOR acct.
    DEFINE BUFFER BlockObject FOR BlockObject.

    DEFINE QUERY BlockObjectQuery FOR BlockObject.

    FIND FIRST acct WHERE
               acct.acct     EQ iAcct
           AND acct.currency EQ iCurrency
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN "".

    OPEN QUERY BlockObjectQuery
        FOR EACH  BlockObject WHERE
                  BlockObject.class-code   EQ "BlockAcct"
             AND  BlockObject.file-name    EQ "acct"
             AND  BlockObject.surrogate    EQ acct.acct + "," + acct.currency
             AND  BlockObject.beg-datetime LE iDateTime
             AND (BlockObject.end-datetime EQ ?
               OR BlockObject.end-datetime GE iDateTime)
        NO-LOCK.

    GET FIRST BlockObjectQuery.
    IF AVAILABLE BlockObject THEN
	RETURN "�����஢��".
    ELSE
        RETURN "".

    CLOSE QUERY BlockObjectQuery.

/*
    /* �᫨ �����஢�� ࠧ��, � ���� ������ "" */
    REPEAT:
        GET NEXT BlockObjectQuery.
        IF QUERY-OFF-END("BlockObjectQuery") THEN
            LEAVE.
        IF vPrevBlock NE BlockObject.txt[1] THEN
            LEAVE.
        vPrevBlock = BlockObject.txt[1].
    END.

    CLOSE QUERY BlockObjectQuery.

    /* �᫨ �����஢�� � ��������묨 ����ࠬ� ����� - ��⠥� �� �ਮ��⠬ �����஢�� */
    vBlockList = BlockAcct(acct.acct + ',' + acct.currency,iDateTime).

    IF vPrevBlock EQ ? THEN
        vPrevBlock = "".

    IF CAN-DO(vBlockList,"����") THEN
        RETURN "����:" + vPrevBlock.
    IF CAN-DO(vBlockList,"������") AND CAN-DO(vBlockList,"������") THEN
        RETURN (IF acct.side EQ "�" THEN "������:" ELSE "������:") + vPrevBlock.
    IF CAN-DO(vBlockList,"������") THEN
        RETURN "������:" + vPrevBlock.
    IF CAN-DO(vBlockList,"������") THEN
        RETURN "������:" + vPrevBlock.
    IF CAN-DO(vBlockList,"�����㬬") THEN
        RETURN "�����㬬:" + vPrevBlock.

    RETURN "".
*/
END FUNCTION.

&GLOB PROC-ACCT                                                              ~
  run fdbacct( buffer acct, ff-card, in-code-value ).                        ~
  for each buf-ttKau WHERE buf-ttKau.fTbName EQ "ACCTB" NO-LOCK,             ~
    FIRST bacct WHERE RECID(bacct) EQ buf-ttKau.fRecId NO-LOCK               ~
    break by bacct.acct :                                                    ~
      CREATE ttacct.                                                         ~
      ASSIGN                                                                 ~
        ttacct.acct-view   = STRING(acct.acct,GetAcctFmt(code.misc[8]))      ~
        ttacct.rec-oa      = recid(acct)                                     ~
        ttacct.acct        = acct.acct                                       ~
        ttacct.curr        = acct.curr                                       ~
      .                                                                      ~
      RUN acct-pos IN h_base (ttacct.acct,                                   ~
                              ttacct.curr,                                   ~
                              gend-date,                                     ~
                              gend-date,                                     ~
                              "�").                                          ~
      ASSIGN                                                                 ~
        ttacct.obal  = IF ttacct.curr > "" THEN sh-val                       ~
                                            ELSE sh-bal                      ~
        ttacct.fobal = ttacct.obal NE 0                                      ~
      .                                                                      ~
      ASSIGN                                                                 ~
          ttacct.rec-ba = recid(bacct)                                       ~
      .                                                                      ~
         RUN acct-pos IN h_base (bacct.acct,                                 ~
                                 bacct.curr,                                 ~
                                 gend-date,                                  ~
                                 gend-date,                                  ~
                                 if can-do(AccessAcct, bacct.acct) then AccessStat else "�" ).                                  ~
          ttacct.blk-type = GetMainBlock(bAcct.acct,                                                                            ~
                                         bAcct.currency,                                                                        ~
                                         (IF gend-date EQ TODAY THEN DATETIME(TODAY,MTIME) ELSE DATETIME(gend-date + 1)) - 1).  ~
          IF ttacct.blk-type NE "" AND ttacct.blk-type NE ? THEN ttacct.blk-type = "�����஢��".         ~
          ttacct.blk-amt  = GetBlockPositionAll(bAcct.acct, bAcct.currency, gend-date).    ~
          ttacct.blk-cust = ClientXattrVal(bAcct.cust-cat, bAcct.cust-id, "����").   ~
                                                                                     ~
      ASSIGN                                                                         ~
          ttacct.bacct-view = STRING(bacct.acct,GetAcctFmt(code.misc[8]))            ~
          ttacct.bacct      = bacct.acct                                             ~
          ttacct.bcurr      = bacct.curr                                             ~
          ttacct.rec-ba     = RECID(bacct)                                           ~
          ttacct.name       = IF acct.cust-cat EQ "�" THEN (name[1] + " " + name[2]) ~
                                                  ELSE (name[1] +  " " + name[2])    ~
          ttacct.bbal       = IF bacct.curr  GT "" THEN sh-val ELSE sh-bal           ~
          ttacct.fobal      = IF ttacct.obal GT 0  THEN yes    ELSE no               ~
          ttacct.fbbal      = IF ttacct.bbal LT 0  THEN yes    ELSE no               ~
          ttacct.bbal-rub   = sh-bal                                                 ~
          ttacct.bbal-val   = sh-val                                                 ~
      .                                                                              ~
  END.


RUN SelectAcctOfKauId(code.code).
/* ���������� �६����� ⠡���� */
FOR EACH ttKau WHERE ttKau.fTbName EQ "ACCT" NO-LOCK,
    FIRST acct WHERE RECID(acct) EQ ttKau.fRecId NO-LOCK:
   {getcust.i &name=name &Offinn="/*"}
   {&PROC-ACCT}

END.


FORM
   ttacct.acct-view FORMAT "x(25)"
               HELP "��������ᮢ� ���"
               SPACE(5)
   ttacct.obal COLUMN-LABEL "������� ��!������������� �����"
               HELP "���⮪ �� ��������ᮢ�� ���"
   ttacct.bbal COLUMN-LABEL "������� ��!���������� �����"
               HELP "���⮪ �� �����ᮢ�� ���"
WITH FRAME BROWSE1
     TITLE COLOR BRIGHT-WHITE "[ ������� ����� �� ��������� �� " + STRING(gend-date) + " ]"
     WIDTH 79.

FORM
   ttacct.acct-view  FORMAT "x(25)"
                HELP "��������ᮢ� ���"
   ttacct.bacct-view FORMAT "x(25)"
                COLUMN-LABEL  "!���� �� �������"
                HELP "�����ᮢ� ���"
   ttacct.obal  COLUMN-LABEL "������� ��!������������� �����"
                HELP "���⮪ �� ��������ᮢ�� ���"
WITH FRAME BROWSE2
     TITLE COLOR BRIGHT-WHITE "[ ������� ����� �� ��������� �� " + STRING(gend-date) + " ]"
     WIDTH 79.

FORM
   ttacct.acct-view  FORMAT "x(25)"
                HELP "��������ᮢ� ���"
   ttacct.name  FORMAT "x(47)"
                COLUMN-LABEL "!������������ �����"
                HELP "������������ (��������) ��楢��� ���"
WITH FRAME BROWSE3
     TITLE COLOR BRIGHT-WHITE "[ ������� ����� ]"
     WIDTH 79.

FORM
   ttacct.bacct-view FORMAT "x(25)"
                COLUMN-LABEL  "!���� �� �������"
                HELP "�����ᮢ� ���"
   ttacct.bbal  
                COLUMN-LABEL "������� ��!���������� �����"
                HELP "���⮪ �� �����ᮢ�� ���"
   ttacct.blk-type FORMAT "x(20)"
                COLUMN-LABEL "�ਧ���!�����஢�� ���"
                HELP "�ਧ��� �����஢�� �����ᮢ��� ���"
   ttacct.blk-amt FORMAT "->>>,>>>,>>9.99" 
                COLUMN-LABEL "�㬬�!�����஢��"
                HELP "�㬬� �����஢�� �����ᮢ��� ���"
   ttacct.blk-cust FORMAT "x(7)"
                COLUMN-LABEL "����.!������"
                HELP "����稥 �����஢�� ������"
WITH FRAME BROWSE4
     TITLE COLOR BRIGHT-WHITE "[ ������� ����� �� ��������� �� " + STRING(gend-date) + " ]"
     WIDTH 115.

&glob oqry0 open query qry0 for each ttacct where ttacct.fobal and ttacct.fbbal and can-do(mTmplCurr, ttacct.bcurr) no-lock.
&glob oqry1 open query qry0 for each ttacct where ttacct.fobal and can-do(mTmplCurr, ttacct.bcurr) no-lock.
&glob oqry2 open query qry0 for each ttacct where can-do(mTmplCurr, ttacct.bcurr) no-lock.

release ttacct.


mNF = 4.

{navigate.cqr
   &file     = ttacct
   &files    = "ttacct"
   &qry      = "qry0"
   &maxoq    = 3
   &avfile   = "ttacct "
   &defquery = "def query qry0 for ttacct scrolling."
   &maxfrm   = 4
   &bf1      = "ttacct.acct-view ttacct.obal ttacct.bbal "
   &bf2      = "ttacct.acct-view ttacct.bacct-view ttacct.obal "
   &bf3      = "ttacct.acct-view ttacct.name "
   &bf4      = "ttacct.bacct-view ttacct.bbal ttacct.blk-type ttacct.blk-amt ttacct.blk-cust"
   &first-frm = mNF
   &workfile = "/*"
   &nodel    = "/*"
   &look     = "acct(crd.nav "
   &return   = "acct(crd.ret "
   &oh3      = "�F3"
   &oth3     = "acct(crd.f3 "
   &oh6      = "�F6"
   &oth6     = "acct(crd.f6 "
   &oh2      = "�F2-��⠫�����"
   &oth2     = "acct(crd.f2 "
   &oh7      = "�F7"
   &oth7     = "findsp.cqr "
     &find1  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = acct     ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             
     &find2  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = bacct    ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             
     &find3  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = name     ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             

}

/*
   &n-str=num-line

   &oh2="�F3 �ଠ"
   &oth2="op-frm.chg "
*/
{intrface.del "acct"}
RETURN.
