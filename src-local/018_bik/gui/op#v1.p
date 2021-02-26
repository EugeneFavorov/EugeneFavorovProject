{globals.i}

/* +++ op#v1.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:23am +++ */

FORM "op#v1.p:zDckzzBwj4QUC7Sc/Fjd0A":U WITH FRAME BQLintID.
/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  op#v1.p
      Comment:  ��ᬮ�� � ।���஢���� ४����⮢ ���㬥��
   Parameters: ���
         Uses:  -
      Used by:  op(sess).p,op-en(s1.p,...
      Created:  09/08/2002 Peter
     Modified: 11.07.2002 16:13 SEMA     �� ��� 0008650 ���ꥬ 䠩��� �� ���
     Modified: 12.07.2002 16:44 SEMA     �� ��� 0008690 ���ꥬ 䠩��� �� ���
     Modified: 28.10.2004 abko (0037979) �� �맮�� ��楤��� ����,
                                         ��⠭���������� ��⥬��� ��६�����
                                         user-proc-id
     Modified: 16.02.2005 (0036752) �� F2 ।���஢���� �ਫ������
     Modified: 
   Last change:  HO   21 Mar 2005    2:15 pm
     Modified: 15.10.2008 18:29 KSV      (0097651) ��२������� �३� �� F5
     Modified: 29.10.2008 11:57 Vasov    <comment>
     Modified: 24/03/2009 kraw (0102072) 4 ��ப� � name-ben
     Modified: 25/01/2011 kraw (0133646) ��ᬮ�� ࠧ����� �� ᨬ�����
*/

{globals.i}
{g-defs.i}
{wordwrap.def}
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */

&GLOB rec-label  "����-����������"
&GLOB send-label "����-�����������"

def input param in-op-date like op.op-date.
def input param in-user-id like op.user-id.
def input param in-op like op.op.
def input param level as INT64.

def var need-valdate as logical format "��� �����஢����/" no-undo.
def var amb as logical format "[ Scroll ]/����������" no-undo.
def var vmfo like op-bank.bank-code no-undo.
def var vcorr-acct like op-bank.corr-acct no-undo.
def var lastpos like acct-pos.since no-undo.
def buffer bop-entry for op-entry.
DEF BUFFER bacct     FOR acct.
DEF BUFFER c-acct     FOR acct.
DEF BUFFER d-acct     FOR acct.


def var help-label as char init " F3 �࠭�����F5 ����.�F9 ��ࠢ.�Ctrl-G �����" no-undo.

DEFINE VARIABLE mIdent           AS   CHARACTER             NO-UNDO.
DEFINE VARIABLE mUserProcSurr    AS   CHARACTER             NO-UNDO.
DEFINE VARIABLE mOp-kind         LIKE op-kind.op-kind       NO-UNDO.
DEFINE VARIABLE mName-opkind     LIKE op-kind.name-opkind   NO-UNDO.
DEFINE VARIABLE mRightsResult    AS   LOGICAL               NO-UNDO.
DEFINE VARIABLE dbcr             AS   LOGICAL               NO-UNDO.

DEFINE VARIABLE mListSymb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mListSum  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mListDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItem     AS INT64     NO-UNDO.
DEFINE VARIABLE mProcPrnt AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFake     AS CHARACTER NO-UNDO.
{g-kaswf.i NEW}
{sesslimit.fun}

FORM
  "��������" op.doc-type ":" doc-type.name "N" op.doc-num FORMAT "x(7)" "��" at 64 op.doc-date at 69 SKIP
  need-valdate op.op-value-date "��������" op.ins-date    
  "����.����."  AT 51  op.order-pay FORMAT "x(2)"  HELP "��।����� ���⥦�" at 61
  "����"  op.due-date HELP "�ப ���⥦�" SKIP
  "����:" AT 1 op.op-status at 6 
  "��� �����:" AT 11 op.op-kind AT 21 FORMAT "x(14)"
  "����:" AT 35 op.user-id at 40
  "�����:" AT 48 op.user-inspector AT 55 
  "�����:" AT 64 op.op-transaction AT 70 skip
  "�������������������������������������������������������������������" amb at 68 "�" at 78
  "�����:" op-entry.acct-db  "�������:" AT 38 op-entry.acct-cr SKIP
  " " name-db[1] "� " AT 38 name-cr[1] SKIP
  " " name-db[2] "� " AT 38 name-cr[2] SKIP 
  "�������:" bal-db LIKE acct-pos.balance FORMAT ">,>>>,>>>,>>>,>>>,>>9.99Cr" "��������:" AT 38 bal-cr LIKE acct-pos.balance FORMAT ">,>>>,>>>,>>>,>>>,>>9.99Cr" SKIP
  "������������������������������������������������������������������������������"
  "���� ��   " "���" "{&in-ua-amtfc}      {&in-uf-amtncn}"  "��" "��" "��" SKIP
     op-entry.value-date HELP "��� ����, �� ������ ����⠭ �������� � ���. �����"
     op-entry.currency AT 12 op-entry.amt-cur AT 18 op-entry.amt-rub
     op-entry.symbol op-entry.prev-year op-entry.op-cod format "x(6)" SKIP
    "������:" op.name-ben VIEW-AS FILL-IN SIZE 51 BY 1 "���:" op.inn
  "�[" doc-kind format "x(18)" "]�������������������������������������������������������"
  "���:" vmfo help "��� �����" bank1.name FORMAT "x(47)" AT 32 SKIP
  "�/�:" vcorr-acct bank2.name FORMAT "x(47)" AT 32 SKIP
  "�/�:" op.ben-acct
  "�[ ���������� �������� ]������������������������������������������������������"
    op.details VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 3
WITH FRAME opreq 1 DOWN OVERLAY CENTERED NO-LABEL ROW 1 TITLE COLOR bright-white
  "[ �������� (" + mOp-kind + "): " + mName-opkind + " ]".


on "PgUp" of frame opreq anywhere do:
  if amb then do with frame opreq:
    find prev op-entry of op no-lock no-error.
    if not avail op-entry then
      find last op-entry of op no-lock no-error.
    {op#v1.i}
    disp {op#v1.lf} with frame opreq.
  end.
  {return_no_apply.i}
end.

on "PgDn" of frame opreq anywhere do:
  if amb then do with frame opreq:
    find next op-entry of op no-lock no-error.
    if not avail op-entry then
      find first op-entry of op no-lock no-error.
    {op#v1.i}
    disp {op#v1.lf} with frame opreq.
  end.
  {return_no_apply.i}
end.

&GLOBAL-DEFINE def-stream-log YES /* �६���� */
DEFINE STREAM lock-log.

find first op where op.op = in-op 
   NO-LOCK NO-ERROR.

      /* �஢�ઠ ���������� �����ᢨ� �ࠢ ����㯠 � ���ଠ樨 
      ** ������-�������� ��� ����� ��� �।�� �஢���� ���㬥�� */
IF AVAIL op THEN
BLK:
DO:
   mRightsResult = YES.
   FOR EACH bop-entry OF op
   NO-LOCK:
            /* ����� */
      FIND FIRST bacct WHERE bacct.acct EQ bop-entry.acct-db
                         AND bacct.curr EQ bop-entry.currency
      NO-LOCK NO-ERROR.
      IF     AVAIL bacct
         AND bacct.cust-cat EQ "�"
         AND NOT GetPersonPermission(bacct.cust-id)
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(bacct.cust-id)).
         mRightsResult = NO.
         LEAVE BLK.
      END.
            /* �।�� */
      FIND FIRST bacct WHERE bacct.acct EQ bop-entry.acct-cr
                         AND bacct.curr EQ bop-entry.currency
      NO-LOCK NO-ERROR.
      IF     AVAIL bacct
         AND bacct.cust-cat EQ "�"
         AND NOT GetPersonPermission(bacct.cust-id)
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(bacct.cust-id)).
         mRightsResult = NO.
         LEAVE BLK.
      END.
   END.
END.
IF NOT mRightsResult THEN RETURN ERROR.

{findcat.i &cat-source = op}
on "F9" of frame opreq anywhere do:
   {sesslimit.i &BREAK-ACTION = "RETURN NO-APPLY."}

   FIND FIRST signs WHERE signs.file-name  EQ "op"
                      AND signs.code       EQ "����������"
                      AND signs.code-value EQ STRING(op.op)
   NO-LOCK NO-ERROR.
   IF AVAIL signs THEN 
   DO:
      RUN Fill-AlertSysMes IN h_tmess("","",-1,"� ���㬥��� " + CHR(32) + STRING(op.doc-num) + CHR(32) + " �ਢ易� ���㬥�� �� ���᫥��� �/�" + CHR(32) + "~n" + CHR(32) + "����� ��������, ���� ���� �易��� ���㬥���").

      {return_no_apply.i}
   END.
   ELSE
m1:
do on error undo m1, leave m1:
  {chkdate2.i op "M1" " " "��������"}
  run "op-ed.p"(recid(op), in-op-date, op.user-id, 3).
  {op#v1.i2}
  disp {op#v1.lf2} with frame opreq.
  {rel-date.i &in-op-date = op.op-date}
  {return_no_apply.i}
end.
end.

on "RETURN" of frame opreq anywhere do:
  run opjoin.p ("op",STRING(ROWID(op)),2,level,"","").
  view frame opreq.
  {op#v1.put}
  {return_no_apply.i}
end.

on "F3" of frame opreq anywhere do:
  DEF VAR mhframe AS HANDLE NO-UNDO.
  mhframe = frame opreq:HANDLE.
  RUN GetClassMethod (op.class-code,
                      "look1",
                      "",
                      "",
                      OUTPUT mProcPrnt,
                      OUTPUT mFake).
  /* ���� ��⮤ ��ᬮ�� ����ୠ⨢�� */
  IF {assigned mProcPrnt} THEN DO:
     mhframe:VISIBLE = FALSE.

     {exch-run.i &Proc = mProcPrnt
                 &Parm = "in-op-date,in-user-id,in-op,level"
     }
  END.
  ELSE DO:
     run "op(tr.p" (op.op-transaction, 4).
  END.
  {&READKEY_PAUSE} 0.

  mhframe:VISIBLE = TRUE.
  {op#v1.put}
  {return_no_apply.i}
end.

on "F2" of frame opreq anywhere do:
  IF GetXAttrValue ("op-kind", op.op-kind, "��炢����") EQ "��" THEN DO:
    RUN nalpl_ed.p (RECID(op), 1, level + 1).
  END.

  IF GetXAttrValue ("op-kind", op.op-kind, "�ਫ������") EQ "��" THEN
  DO:

     mIdent = "�ਫ���।:" + STRING(op.op).
     mUserProcSurr = GetXAttrSurr("user-proc", "outlink", mIdent).
     RUN opext_ed.p (mUserProcSurr, mIdent).
  END.

  IF     mListSymb NE ""
     AND mListSum  NE ""
     AND mListDesc NE ""
     AND NUM-ENTRIES(mListSymb, CHR(1)) EQ NUM-ENTRIES(mListSum,  CHR(1))
     AND NUM-ENTRIES(mListSymb, CHR(1)) EQ NUM-ENTRIES(mListDesc, CHR(1))
     THEN
  DO:
     {empty tempSBU}

     DO mItem = 1 TO NUM-ENTRIES(mListSum,  CHR(1)):
        CREATE tempSBU.
        ASSIGN
           tempSBU.id      = mItem
           tempSBU.symbol  = ENTRY(mItem, mListSymb, CHR(1))
           tempSBU.descr   = ENTRY(mItem, mListDesc, CHR(1))
           tempSBU.amt-rub = DECIMAL(ENTRY(mItem, mListSum, CHR(1))) NO-ERROR
        .
     END.

     IF op-entry.acct-db NE ? THEN
        {find-act.i
           &bact = d-acct
           &acct = DelFilFromAcct(op-entry.acct-db)
        }

     IF op-entry.acct-cr NE ? THEN
        {find-act.i
           &bact = c-acct
           &acct = op-entry.acct-cr
        }

     IF AVAIL(d-acct) AND AVAIL (c-acct) AND d-acct.contract EQ "����" AND c-acct.contract NE "����" THEN dbcr = YES.
     ELSE IF AVAIL(d-acct) AND AVAIL (c-acct) AND c-acct.contract EQ "����" AND d-acct.contract NE "����" THEN dbcr = NO.

     RUN g-kased2.p (0.0,
                     in-op-date,
                     NO,
                     dbcr,
                     4).
  END.
  {op#v1.put}
  {return_no_apply.i}
end.

on "F1" of frame opreq anywhere do:
  form
    "�[ ������ ]���������������������������������������������[ ���:" op.inn "]�"skip
    op.name-ben view-as editor inner-chars 78 inner-lines 4 skip
    "�[ ���������� �������� ]������������������������������������������������������" skip
    op.details view-as editor inner-chars 78 inner-lines 6 skip
  with frame f-ben overlay centered row 10 no-labels title color bright-white "[ ������������� ]".

  do on endkey undo, leave with frame f-ben:
    color disp bright-white op.name-ben op.details op.inn.
    disp 
      "�[ ������ ]���������������������������������������������[ ���:" op.inn "]�"skip
      op.name-ben view-as editor inner-chars 78 inner-lines 4 skip
      "�[ ���������� �������� ]������������������������������������������������������" skip
      op.details view-as editor inner-chars 78 inner-lines 6 skip
    .
    {pause.i &THIS_FRAME = "f-ben"}
  end.
  hide frame f-ben no-pause.
  {op#v1.put}
  {return_no_apply.i}
end.

ON "F5" OF FRAME opreq ANYWHERE DO:
   DEFINE VARIABLE vNameSendRec AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE vNameAcct    AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vAcctSendRec AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE vSuff        AS CHARACTER          NO-UNDO.
   DEFINE BUFFER xacct FOR acct.

   ASSIGN
      vSuff        = ( IF op.doc-kind = "rec" THEN "send" ELSE "rec")
      vNameSendRec = "��� " + GetXAttrValue("op",STRING(op.op),"inn-" + vSuff)
                   + " "    + GetXAttrValue("op",STRING(op.op),"name-" + vSuff)
      vAcctSendRec = GetXAttrValue("op", STRING(op.op),"acct-" + vSuff)
   . 

   {find-act.i
      &bact = xacct
      &acct = TRIM(vAcctSendRec)
   }   
   IF AVAIL xacct THEN DO:
      RUN GetCustName IN h_base (xacct.cust-cat,
                                 xacct.cust-id,
                                 ?,
                                 OUTPUT vNameAcct[1],
                                 OUTPUT vNameAcct[2],
                                 INPUT-OUTPUT vNameAcct[3]).
      vNameAcct[1] = ( IF vNameAcct[3] NE "" 
                      THEN "��� " + vNameAcct[3] + " " 
                      ELSE "") + TRIM(vNameAcct[1]) + " " + TRIM(vNameAcct[2]).
      {wordwrap.i &s=vNameAcct &n=2 &l=100}
   END.
   ELSE
      ASSIGN
         vNameAcct[1] = ""
         vNameAcct[2] = ""
     .

   FORM                                                       
      "�[ ������ ]�������������������������������������������������������������������"SKIP
      vNameSendRec VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 4 SKIP
      "�[ ������ ]�������������������������������������������������������������������"SKIP
      vNameAcct VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 6 SKIP
   WITH FRAME f-ben2 OVERLAY CENTERED ROW 10 NO-LABELS TITLE COLOR bright-white "[ ������������� ]".

   DO ON ENDKEY UNDO, LEAVE WITH FRAME f-ben2:
      COLOR DISPLAY bright-white vNameSendRec vNameAcct op.inn.
      DISPLAY
         vNameSendRec
         vNameAcct
         WITH FRAME f-ben2.
      {pause.i &THIS_FRAME = "f-ben2"}
   END.
   HIDE FRAME f-ben2 NO-PAUSE.
   {op#v1.put}

   {return_no_apply.i}
END.

ON "Ctrl-G" OF FRAME opreq ANYWHERE DO:
   IF NOT AVAIL doc-type THEN DO:
    RUN Fill-SysMes IN h_tmess ("", "", "-1", "���������騩 ��� ���⭮-��������� ���㬥��!").
    {return_no_apply.i}
  END.
  RUN SetSysConf IN h_base('identifier',op.Class-Code + ";" + STRING(op.op)).
  {op#.prt &NO_KEYBOARD=YES}
  {return_no_apply.i}
END.

DO WITH FRAME opreq:
  COLOR DISPLAY bright-green
    name-db[1] 
    name-cr[1] 
    name-db[2] 
    name-cr[2] 
    doc-type.name
    bank1.name 
    bank2.name
  .
  
  COLOR DISPLAY bright-white
    op.doc-type
    op.doc-num
    op.doc-date
    op.op-value-date
    op.ins-date
    op.order-pay
    op.due-date
    op-entry.acct-db
    op-entry.acct-cr
    op.details
    op-entry.symbol 
    op-entry.prev-year 
    op-entry.op-cod
    op-entry.currency
    op-entry.value-date
    op-entry.amt-cur
    op-entry.amt-rub
    op.name-ben
    op.inn
    op.ben-acct
    vmfo
    vcorr-acct
    op.op-status 
    op.user-id 
    op.user-inspector
    op.op-transaction
    op.op-kind
  .
  
  op-entry.acct-db:format = "99999-999-9-9999-9999999".
  op-entry.acct-cr:format = "99999-999-9-9999-9999999".
  
  find first op where op.op = in-op NO-LOCK NO-ERROR.
  find op-entry of op no-lock no-error.
  IF AMBIGUOUS op-entry THEN amb = YES.

  
  help-label = if amb then help-label + "�PgUp �।.�PgDn ᫥�." else help-label + " ".
  help-label = IF GetXAttrValue ("op-kind", op.op-kind, "�ਫ������") EQ "��" 
               THEN  " F2 �ਫ��.�" + SUBSTRING(help-label, 2) 
               ELSE help-label + " ".
  {findcat.i op}
  lastpos = FGetLastClsDate(?,mCats).
  IF lastpos = ? THEN
     lastpos = 4/27/1972.
  
  COLOR DISPLAY bright-red
    amb when amb 
  .

  find first op-kind of op no-lock no-error.
  IF AVAILABLE op-kind THEN
     ASSIGN mOp-kind     = op-kind.op-kind
            mName-opkind = op-kind.name-opkind.
  find first op-entry of op no-lock no-error.

  mListSymb = GetXAttrValueEx("op",
                              STRING(op.op),
                              "���አᑨ�",
                              "").

  mListSum =  GetXAttrValueEx("op",
                              STRING(op.op),
                              "�㬊�ᑨ�",
                              "").

  mListDesc = GetXAttrValueEx("op",
                              STRING(op.op),
                              "������ᑨ�",
                              "").

   IF     mListSymb NE ""
      AND mListSum  NE ""
      AND mListDesc NE ""
      AND NUM-ENTRIES(mListSymb, CHR(1)) EQ NUM-ENTRIES(mListSum,  CHR(1))
      AND NUM-ENTRIES(mListSymb, CHR(1)) EQ NUM-ENTRIES(mListDesc, CHR(1))
      THEN
   DO:
      help-label = help-label + "�F2 �������� �� ᨬ�.".
   END.

  {op#v1.i2}
  {op#v1.i}
  
  disp 
    {op#v1.lf2}
    {op#v1.lf}
    amb
  .
END.

{op#v1.put}

on F8 of op.details in frame opreq do:
   {return_no_apply.i}
end.

PAUSE 0 BEFORE-HIDE.

do on endkey undo, leave on error undo, leave:
  ENABLE op.details WITH FRAME opreq.
  op.details:READ-ONLY IN FRAME opreq = YES.
  op.details:PFCOLOR IN FRAME opreq = 0.
  APPLY "entry" TO op.details IN FRAME opreq.
  {wait_for.i &THIS_FRAME = "opreq" &EXFILE = "op#v1.p.wf1" {&*}} .
end.

hide frame opreq no-pause.
/* $LINTFILE='op#v1.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='stre' */
/* $LINTDATE='10/03/2016 19:05:36.187+04:00' */
/*prosignM6XNSadsevFWeFF6aJZAjw*/
/* --- op#v1.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:23am --- */
