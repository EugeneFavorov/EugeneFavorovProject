/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    COPYRIGHT: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     FILENAME: CARD2DEC.P
      COMMENT: ��⮬���᪠� ���⠭���� �� ����⥪�
      COMMENT:
   PARAMETERS:
         USES:
      USED BY:
      CREATED: ??/??/????
     MODIFIED:
     MODIFIED: 06/12/2003 KOSTIK 22471 ������ �������⥫��� ४����⮢ �� ᯨᠭ�� � ����⥪�2
     Modified: 01.06.2004 abko 0029139 ���� ��� �� ����2 � ��⮬ ���ࠧ�������
     Modified: 24/09/2004 ABKO 0035793 �।���⥫�� ���� ��� �����㬥�⮬
     Modified: 10.01.2005 15:45 serge    0041201 ��������� ᬥ�� ����� ���㬥��
     Modified: 07/12/2006 kraw (0058871) ��।��� � g-call* ⨯ ᯨᠭ��
     Modified: 30/03/2007 kraw (0057466) �맮� 㭨���ᠫ쭮� �࠭���樨 ������2
     Modified: 05/09/2007 kraw (0081627) ����⮩�� ⮫쪮 ��� ����⥪� 2
     Modified: 06/09/2007 kraw (0077977) �ਭ㤨⥫쭠� ������ ins-date � ���. ⮫쪮 ��� ����⥪� 2
     Modified: 02/10/2007 kraw (0077977) ����2kau
     Modified: 14/01/2007 muta (0087133) 302-�. ���ﭠ �㭪樮���쭮��� ���� ����⮩�� �� �����ᨮ��� ���⥦� 
     Modified: 06/02/2008 kraw (0086909) �� ��뢠�� ������2, �᫨ �� ��࠭� kau
     Modified: 28.03.2008 muta  0088445  ��।������ �㬬� ��� ᯨᠭ�� � ��⮬ �����஢�� �㬬� � ����� ���⪠
     Modified: 08/102010 kraw (0134695) ���� kau ��� ����ᨬ��� �� "����2"/"�������2"
     Modified: 07/02/2011 kraw (0116612) �ਮ�⠭������� ᯨᠭ��
*/

{intrface.get count}
{g-docnum.def}    /* ��� �奬 ��⮭㬥�樨. */ 
{globals.i}
{defwrkop.i new}
{defopupd.i}
{sh-defs.i}
{def-wf.i new}
{defoptr.i}
{wclass.i}
{copyxtr.i}
{ch_cart.i}
{topkind.def}
{tmprecid.def &NGSH="local"}

def output param flager as INT64 no-undo.
def input param in-op-date as date no-undo.
def input param op-en-rid as recid no-undo.
def input param rid1 as recid no-undo .
def input param rid2 as recid no-undo .
def shared var lim-pos as decimal no-undo.

def var ret-value as char no-undo.
define shared var pick-value as char no-undo.
def var out-rid as recid no-undo.
def var j as INT64 no-undo.
def var rid as recid extent 50 no-undo.

DEFINE VARIABLE mDate1256  AS DATE      NO-UNDO. /*��� ��砫� ����⢨� 1256*/
DEFINE VARIABLE mDocType   AS CHARACTER NO-UNDO. /*��� ���㬥�� ��� ᯨᠭ��*/
DEFINE VARIABLE mClassDoc  AS CHARACTER NO-UNDO. /*����� ���㬥�� ��� ᯨᠭ��*/
DEFINE VARIABLE mVidSpis   AS INT64   NO-UNDO. /*��� ᯨᠭ�� ���筮�/������*/
DEFINE VARIABLE mAcct-db   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tcur       AS CHARACTER NO-UNDO.
DEFINE VARIABLE main-first AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mOpOutB    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpentOutB AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRes       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mErrMsg    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOK        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mSumm      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mStat      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPropShabl AS LOGICAL   NO-UNDO.

DEF VAR vBrnch-id AS CHARACTER  INIT "*"  NO-UNDO.

def buffer buf-op for op.
def buffer out-entry for op-entry.
DEF BUFFER out-op    FOR op.
def buffer xop for op.
def buffer cacct for acct .
def buffer xop-entry for op-entry .

DEFINE BUFFER bAcct FOR acct.

DEFINE TEMP-TABLE ttKauRidOp NO-UNDO
   FIELD rid-kau   AS RECID
   FIELD order-pay LIKE op.order-pay
   FIELD doc-date  LIKE op.doc-date
   FIELD sort      LIKE kau.sort
   INDEX rid-kau IS UNIQUE rid-kau
   INDEX ord order-pay doc-date
.

flager = 1.

ASSIGN
   end-date  = in-op-date
   mDate1256 = DATE(FGetSetting("�⠭���","��⠍��1256",?))
NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   MESSAGE "�஢���� ���祭�� ����஥筮�� ��ࠬ��� <��⠍��1256>!"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

find acct where recid(acct) eq rid1 no-lock no-error .
find bal-acct where bal-acct.acct-cat eq "o" and
 bal-acct.kau-id eq "����-��2" no-lock no-error .

/* �஢����� �ਭ���������� � ������ ���ࠧ������� ���. � ����. ��⮢ */
DEF VAR oRecid     AS RECID NO-UNDO. /*�� ��������ᮢ� ���*/
DEF VAR oAmbig     AS LOG   NO-UNDO. /*����� ��⮢*/
     
RUN GetCar_2(acct.acct,
             acct.currency,
             end-date,
             OUTPUT oRecid,
             OUTPUT oAmbig).

IF oAmbig THEN
DO:
  pick-value = '' .
   RUN "acct(k).p"(bal-acct.bal-acct,
                   bal-acct.kau-id,
                   acct.cust-cat,
                   acct.cust-id,
                   acct.currency,
                   vBrnch-id,
                   4).
   if pick-value <> '' and pick-value <> ? then
    find cacct where cacct.acct = entry(1,pick-value) and cacct.currency = entry(2,pick-value) no-lock no-error.
   else return .
END.
ELSE
   IF oRecid NE 0 THEN
      FIND FIRST cacct WHERE RECID(cacct) EQ oRecid NO-LOCK NO-ERROR.

find first op-kind where recid(op-kind) eq rid2 no-lock.
find first op-templ of op-kind  where op-templ.acct-cat eq "b" no-lock no-error.
if not available op-templ then do :
 {message "�� ����� 蠡��� �⠭���⭮� �࠭���樨 �����ᮢ�� �஢����" }
 return.
end.
find first op-templ of op-kind  where op-templ.acct-cat eq "o" no-lock no-error.
if not available op-templ then do :
 {message "�� ����� 蠡��� �⠭���⭮� �࠭���樨 ��������ᮢ�� �஢����" }
 return.
end.
find op-entry where recid(op-entry) eq op-en-rid no-lock.
find buf-op of op-entry no-lock.
find code where code.class eq "�������" and code.code eq bal-acct.kau-id no-lock
 no-error.
if code.misc[5] ne "" and
   not (search(code.misc[5] + ".p") <> ? or search(code.misc[5] + ".r") <> ?)
   then do:
     {message &text="�� ������� ��楤�� ��ᬮ�� ""
                  + code.misc[5] + "".p""
                  + "" � 蠡���� ��� !"
              &alert-box=error
     }
     return.
end.
def var i as INT64 no-undo.
def var sh_bal as dec no-undo.
def var sh_val as dec no-undo.
assign
  sh_bal = sh-bal
  sh_val = sh-val.
tit:
do  on endkey undo tit , return on error undo tit, return:
 /* �������� �����ᮢ��஢����*/
 pick-value = "mult".
 {empty  tmprecid} 
 run value(code.misc[5] + ".p") (cacct.acct, cacct.currency,6).
 if keyfunction(lastkey) eq "END-ERROR" then return.
 RUN rid-rest.p (OUTPUT TABLE tmprecid).

 ret-value = pick-value.
 for each tmprecid:
     accumulate tmprecid.id (count).
 end.

 RUN CreateOrderingTT.

 FOR EACH ttKauRidOp
        BY ttKauRidOp.order-pay
        BY ttKauRidOp.doc-date
        BY ttKauRidOp.sort:

 find first op-templ of op-kind  where op-templ.acct-cat eq "b" no-lock no-error.
 find kau where recid(kau) eq ttKauRidOp.rid-kau  no-lock no-error.
 find op where op.op eq INT64(entry(1,kau.kau)) no-lock no-error .
 if avail op then do:
  FIND signs WHERE signs.file EQ "op"
               AND signs.code EQ "op-bal"
               AND signs.surr EQ STRING(op.op)
                              NO-LOCK NO-ERROR.
  IF AVAIL signs THEN
     FIND xop WHERE xop.op EQ INT64(signs.xattr-val) NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST xop WHERE xop.op-transaction EQ op.op-transaction
                      AND xop.acct-cat       EQ "b" NO-LOCK NO-ERROR.
  if avail xop then do:
   find first xop-entry of xop no-lock no-error .
   find signs where signs.file-name eq "op" and signs.surrogate eq string(xop.op) and
    signs.code eq "acctcorr" no-lock no-error .
   find op-bank where op-bank.op eq xop.op no-lock no-error .
  end.

   IF GetXAttrValueEx("op",
                      STRING(op.op),
                      "�ਮ�⑯��",
                      "") EQ "��" THEN
   DO:

      IF AVAILABLE xop THEN
      DO:
         mStrTMP = xop.doc-num.
      END.
      ELSE
      DO:
         mStrTMP = "".
      END.

      IF mStrTMP NE "" THEN
         mStrTMP = "N " + mStrTMP.

      MESSAGE
      "�� ���㬥��� " + mStrTMP + " ᯨᠭ�� �ਮ�⠭������"
      VIEW-AS ALERT-BOX ERROR. 

      UNDO, NEXT.
   END.
  IF FGetSetting("�⠭���", "��⑯", "") EQ "�� ���㬥��" THEN
     mStat = GetXattrValue("op",string(op.op),"�������"). 
 end.

 mSumm = CalcRealAcctPos(kau.kau,
                         acct.kau-id,
                         acct.acct,
                         acct.currency, 
                         IF AVAIL xop THEN xop.order-pay
                         ELSE IF AVAIL op THEN op.order-pay ELSE ?,
                         end-date). 

 /*��⠭���� ��६����� ��� �ᯮ�짮����� ����஬ �१ ��Ꮰࠬ*/
 IF AVAIL(xop) THEN DO:

    RUN SetSysConf in h_base ("���-���:OP-BAL" ,  STRING(xop.op)).
    RUN SetSysConf in h_base ("���-���:DOC-KIND" ,xop.doc-kind).
    RUN SetSysConf in h_base ("���-���:DOC-NUM"  ,xop.doc-num).
    RUN SetSysConf in h_base ("���-���:ORDER-PAY", xop.order-pay).
    RUN SetSysConf in h_base ("���-���:ACCT-DB"  , acct.acct).
    RUN SetSysConf in h_base ("���-���:ACCT-CR", IF AVAIL(signs) THEN signs.xattr-value ELSE "").
    RUN SetSysConf in h_base ("���-���:BEN-ACCT", xop.ben-acct).
    RUN SetSysConf in h_base ("���-���:NAME-BEN", xop.name-ben).
    RUN SetSysConf in h_base ("���-���:INN"     , xop.inn).
    RUN SetSysConf in h_base ("���-���:DETAILS", xop.details).
    RUN SetSysConf in h_base ("���-���:AMT-RUB", GetXattrValue("op",STRING(xop.op),"amt-rub")).
    RUN SetSysConf in h_base ("���-���:AMT-CUR", GetXattrValue("op",STRING(xop.op),"amt-cur")).
    RUN SetSysConf in h_base ("���-���:DOC-DATE" , STRING(xop.doc-date)).
    FIND FIRST doc-type WHERE doc-type.doc-type EQ xop.doc-type NO-LOCK NO-ERROR.
    RUN SetSysConf in h_base ("���-���:����-���-���", IF AVAIL(doc-type) THEN doc-type.name-doc ELSE ""). 

 END.

 create work-op.
 assign
   work-op.acct-db = acct.acct
   work-op.acct-cr = if avail signs then signs.xattr-value else op-templ.acct-cr
   work-op.acct-cr-enable = if avail signs then false else true
   work-op.currency = acct.currency
   work-op.amt-cur = if acct.currency ne "" THEN mSumm
                     else ?
   work-op.amt-rub = if acct.currency eq "" THEN mSumm 
                     else ?
   work-op.op-status  = IF {assigned mStat} THEN mStat ELSE (if op-templ.op-status gt buf-op.op-status then buf-op.op-status
                        else op-templ.op-status)
   work-op.amt-rub-enable = true
   work-op.acct-cat = "b"
   work-op.op-transaction = cur-op-trans
   work-op.doc-kind = IF AVAIL xop AND xop.doc-kind NE "" AND xop.doc-kind NE ? THEN xop.doc-kind
                                                                                ELSE work-op.doc-kind
   work-op.details = if avail xop and xop.details ne "" then xop.details
                     else if avail op AND op.details ne "" then op.details
                     else op-templ.details
   work-op.order-pay = IF AVAIL xop THEN xop.order-pay
                       ELSE IF AVAIL op THEN op.order-pay
                       ELSE ?
   work-op.doc-num =  if avail xop THEN xop.doc-num else work-op.doc-num
 .
 def var doc-num-b as char no-undo.
 doc-num-b = work-op.doc-num.
 if avail op-bank then do:
 assign
   work-op.mfo        = if avail op-bank then op-bank.bank-code else ""
   work-op.corr-acct  = if avail op-bank then op-bank.corr-acct else ""
   work-op.ben-acct   = xop.ben-acct
   work-op.name-ben   = xop.name-ben
   work-op.inn        = xop.inn
  .
 end.
 /*��।��塞 ����� � ⨯ ��� �����ᮢ��� ���㬥��
 �.�. �� ���筮� ᯨᠭ�� � ������, ��� ࠧ��
 */      
 RUN chktpdoc.p (RECID(kau),
                RECID(op-template),
                IF acct.currency EQ "" THEN work-op.amt-rub ELSE work-op.amt-cur,
                OUTPUT mDocType,
                OUTPUT mClassDoc,
                OUTPUT mVidSpis).
 work-op.doc-type = mDocType.
 /*************************************************/
 
 if avail xop-entry then work-op.acct-cr = xop-entry.acct-cr.


 IF     mDate1256 NE ?
    AND mDate1256 GT xop.doc-date THEN
 RUN SetSysConf IN h_base ("����2��ଠ���","NO").

 RUN SetSysConf IN h_base ("����2���",STRING(RECID(xop))).
 RUN SetSysConf IN h_base ("����1274",STRING(RECID(kau))).

 IF mVidSpis EQ 1 AND kau.kau-id EQ "����-��2" THEN
 DO:

    IF op.doc-date EQ ? THEN
       mStrTMP = ",".
    ELSE
       mStrTMP = STRING(xop.doc-date) + ",".

    IF xop.ins-date NE ? THEN
       mStrTMP = mStrTMP + STRING(xop.ins-date).

    RUN SetSysConf IN h_base ("����2Date",mStrTMP).
    RUN SetSysConf IN h_base ("����2doc-type",xop.doc-type).
    RUN SetSysConf IN h_base ('����2kau',kau.kau-id).
 END.
 ELSE
 DO:
    RUN SetSysConf IN h_base ("����2Date","").
    RUN SetSysConf IN h_base ('����2kau',"").
 END.
 RUN SetSysConf IN h_base ( "opcardoutb", "").
 RUN SetSysConf IN h_base ( "opentcardoutb", "").

 if (accum count tmprecid.id) = 1 or
    work-op.acct-db = ? or work-op.acct-cr = ? then
    run g-call1.p (output flager, in-op-date, recid(work-op), recid(op-templ), output out-rid).
 else
    run g-call2.p (output flager, in-op-date, recid(work-op), recid(op-templ), output out-rid).
 RUN SetSysConf IN h_base ("����2���","").
 RUN SetSysConf IN h_base ("����2��ଠ���","").
 RUN SetSysConf IN h_base ("����1274","").
 RUN SetSysConf IN h_base ("����2Date","").
 RUN SetSysConf IN h_base ('����2kau',"").
 RUN SetSysConf IN h_base ("����2doc-type","").

 IF GetSysConf("opcardoutb") NE "" THEN
 ASSIGN
    mOpOutB = GetSysConf("opcardoutb")
    mOpentOutB = GetSysConf("opentcardoutb")
 .
 RUN SetSysConf IN h_base ( "opcardoutb", "").
 RUN SetSysConf IN h_base ( "opentcardoutb", "").

 if flager ne 0 then undo tit, return. else flager = 1.
 find out-entry where recid(out-entry) eq out-rid no-lock.
 FIND out-op OF out-entry NO-LOCK.
 IF AVAIL out-op THEN DO:
    cur-op-trans = out-op.op-transaction.
    RUN Copy-Xattr-Op(RECID(xop),RECID(out-op),"acct-send,name-send,inn-send").
 END.
 delete work-op .
 release op-templ .
/* ᮧ����� ��������ᮢ�� �஢���� */
 find first op-templ of op-kind  where op-templ.acct-cat eq "o" no-lock no-error.

 /* ��ࠡ�⪠ ������� �㭪権 � 蠡���� ��������ᮢ��� ���㬥�� */
 {g-acctv1.i 
      &no-cacct      = YES
      &vacct         = mAcct
 }
 IF mAcct-db NE ? THEN 
 DO:
   {find-act.i
      &bact   = bAcct
      &acct   = mAcct-db
      &curr   = acct.currency
   }
   IF NOT AVAIL bAcct THEN 
   DO:
     MESSAGE "�訡�� 蠡���� �࠭���樨!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     UNDO tit, RETURN.
   END.
 END.

 create work-op .
 assign
   work-op.acct-db = mAcct-db
   work-op.acct-cr = cacct.acct
   work-op.acct-db-enable = true
   work-op.currency = cacct.currency
   work-op.amt-cur = if cacct.currency ne "" then
                        out-entry.amt-cur
                     else ?
   work-op.amt-rub = if op-entry.currency eq "" then
                        out-entry.amt-rub
                     else ?
   work-op.op-status  = if op-templ.op-status gt buf-op.op-status then buf-op.op-status
                        else op-templ.op-status
   work-op.amt-rub-enable = false
   work-op.acct-cat = "o"
   work-op.op-transaction = cur-op-trans
   work-op.details = op-templ.details
   work-op.kau-cr = kau.kau
   work-op.order-pay = IF AVAIL xop THEN xop.order-pay
                       ELSE IF AVAIL op THEN op.order-pay
                       ELSE ?
 .

 def var cp-num as INT64 no-undo.
 cp-num = INT64(GetXAttrValue("op-template",op-templ.op-kind + "," + string(op-templ.op-templ),"�����������")) no-error.
 if not can-find(first op-templ where op-templ.op-kind = op-kind.op-kind
                            and op-templ.op-templ = cp-num) then do:
    if (accum count tmprecid.id) > 1 or autonumdoc then
       work-op.doc-num = string(INT64(substring(string(op.op),
             (if (length(string(op.op)) - 6) <= 0 then 1
              else length(string(op.op)) - 5
             ),6))).
 end.
 else work-op.doc-num = doc-num-b.

 /* ����砥� ���祭�� ���� �奬� ��⮭㬥�樨. */
 docnum-tmpl = GetXattrValue ("op-template",
                               op-templ.op-kind + "," + STRING (op-templ.op-templ),
                              "DocCounter").
 IF docnum-tmpl EQ "" THEN
    docnum-tmpl = GetXattrValue ("op-template",
                                  op-templ.op-kind + "," + STRING (op-templ.op-templ),
                                 "��������").
 IF docnum-tmpl NE "" THEN
 DO:
    DocNum-OP = STRING (RECID (Op)).

    work-op.doc-num = STRING(GetCounterNextValue(docnum-tmpl, op.op-date)).
    IF work-op.doc-num EQ ?
        THEN RUN Fill-SysMes IN h_tmess ("", "", "0","���������� ������� ���祭�� ���稪�.").
 END.

 RUN SetSysConf IN h_base ( "opcardoutb", "").
 RUN SetSysConf IN h_base ( "opentcardoutb", "").
 if (accum count tmprecid.id) = 1 or
    work-op.acct-db = ? or work-op.acct-cr = ? then
    run g-call1.p (output flager, in-op-date, recid(work-op), recid(op-templ), output out-rid).
 else
    run g-call2.p (output flager, in-op-date, recid(work-op), recid(op-templ), output out-rid).


 IF GetSysConf("opcardoutb") NE "" THEN
 ASSIGN
    mOpOutB = GetSysConf("opcardoutb")
    mOpentOutB = GetSysConf("opentcardoutb")
 .
 
 RUN SetSysConf IN h_base ( "opcardoutb", "").
 RUN SetSysConf IN h_base ( "opentcardoutb", "").

 RUN SetSysConf in h_base ("���-���:OP-BAL" ,  "").
 RUN SetSysConf in h_base ("���-���:DOC-KIND" ,"").
 RUN SetSysConf in h_base ("���-���:DOC-NUM"  ,"").
 RUN SetSysConf in h_base ("���-���:ORDER-PAY", "").
 RUN SetSysConf in h_base ("���-���:ACCT-DB"  , "").
 RUN SetSysConf in h_base ("���-���:ACCT-CR", "").
 RUN SetSysConf in h_base ("���-���:BEN-ACCT", "").
 RUN SetSysConf in h_base ("���-���:NAME-BEN", "").
 RUN SetSysConf in h_base ("���-���:INN"     , "").
 RUN SetSysConf in h_base ("���-���:DETAILS", "").
 RUN SetSysConf in h_base ("���-���:AMT-RUB", "").
 RUN SetSysConf in h_base ("���-���:AMT-CUR", "").
 RUN SetSysConf in h_base ("���-���:DOP-DATE" , "").
 RUN SetSysConf in h_base ("���-���:����-���-���", ""). 

 if flager ne 0 then undo tit, return. else flager = 1.
 find out-entry where recid(out-entry) eq out-rid exclusive-lock no-error .
 find kau where recid(kau) eq ttKauRidOp.rid-kau exclusive-lock no-error.
  if (out-entry.currency eq "" and kau.balance  lt - out-entry.amt-rub) or
    (out-entry.currency ne "" and kau.curr-bal lt - out-entry.amt-cur) then do:
   {message
   &text="��ࠫ���쭮 � ���� ��-� �� ࠡ�⠥� � ����⥪�� 2.|������ ������."
   }
   flager = 1 .
   undo tit, return.
  end.
  if cacct.currency ne "" then sh_val = sh_val + out-entry.amt-cur.
  else sh_bal = sh_bal + out-entry.amt-rub.
  assign
    sh-bal = sh_bal
    sh-val = sh_val.
  {kau(off).cal &op-entry=out-entry &ssum="- out-entry.amt-rub"
              &inc=1 &scur="- out-entry.amt-cur"}
  delete work-op.
  if cacct.currency <> "" and sh_val = 0 then leave.
  if cacct.currency = "" and sh_bal = 0 then leave.
end. /* for each temrecid */
end.
flager = 0.
 
/* ******************************************* */


IF     AVAILABLE kau 
   AND kau.balance EQ 0 
   AND kau.kau-id EQ "����-��2" THEN
DO:

  mOK = YES.

  mStrTMP = GetXAttrValueEx("op", 
                         STRING(xop.op), 
                         "acctcorr", 
                         "**empty**").

   IF NOT AVAILABLE xop-entry THEN
      FIND FIRST acct WHERE acct.acct     EQ mStrTMP
         NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST acct WHERE acct.acct     EQ xop-entry.acct-cr
                        AND acct.currency EQ xop-entry.currency
         NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN
   DO:

      IF NOT CAN-DO(FGetSetting("�犮������","","--���--"), acct.acct) THEN
      mOK = NO.
   END.
   ELSE
      mOK = NO.


  mStrTMP = GetXAttrValueEx("op", 
                         STRING(xop.op), 
                         "acctbal", 
                         "**empty**").

   IF NOT AVAILABLE xop-entry THEN
      FIND FIRST acct WHERE acct.acct     EQ mStrTMP
         NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST acct WHERE acct.acct     EQ xop-entry.acct-db
                        AND acct.currency EQ xop-entry.currency
         NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN
   DO:

      IF GetXAttrValueEx("acct", 
                         acct.acct + "," + acct.currency, 
                         "���������2", 
                         "**empty**") EQ "**empty**" THEN
         mOK = NO.
   END.

   IF mOK THEN
   DO:

      MESSAGE "� ����⥪� ᯨ�뢠���� ������� �����." SKIP
              "������� ����⮩��?"
         VIEW-AS ALERT-BOX BUTTONS YES-NO
         UPDATE mOK.

      IF NOT mOK THEN
         RETURN.

      {empty tOpKindParams} /* ������ ⠡���� ��ࠬ��஢ */
      ASSIGN
         mRes = TDAddParam ("iOp_b", mOpOutB)
                AND
                TDAddParam ("iOp-entry_b", mOpentOutB)
                AND
                (IF AVAILABLE  op       THEN TDAddParam ("iOp_o",       STRING( out-entry.op))       ELSE NO)
                AND
                (IF AVAILABLE  op-entry THEN TDAddParam ("iOp-entry_o", STRING( out-entry.op-entry)) ELSE NO)
         NO-ERROR.

      IF NOT mRes THEN 
      DO:
         MESSAGE "�訡�� ��।�� ��ࠬ��஢ � �࠭����� ����⮩��" SKIP
                 "���㬥�� ����⮩�� �� �㤥� ��ନ஢��"
            VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      RUN ex-trans.p ("������2", in-op-date, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg).

      IF NOT mOK THEN
      DO:
         MESSAGE "�訡�� �࠭���樨 ����⮩��" SKIP
                 "���㬥�� ����⮩�� �� ��ନ஢��"
           VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
   END.
END.


PROCEDURE CreateOrderingTT:

   DEFINE VARIABLE vOpBal AS INT64 NO-UNDO.
        
   DEFINE BUFFER opb FOR op.
   DEFINE BUFFER opo FOR op.
   
   {empty ttKauRidOp}

   FOR EACH tmprecid:

         FIND FIRST kau WHERE RECID(kau) EQ tmprecid.id NO-LOCK NO-ERROR.
        
      IF FGetSetting("����2", "�������2", "���") EQ "��" THEN DO:
         IF AVAIL(kau) THEN
         FIND opo WHERE opo.op EQ INT64(ENTRY(1, kau.kau))
            NO-LOCK NO-ERROR.
        
         IF AVAIL(opo) THEN
         vOpBal = INT64(GetXAttrValueEx("op",
                                          STRING(opo.op),
                                          "op-bal",
                                          ?)).
        
         IF vOpBal EQ ? AND avail(opo) THEN
            FIND FIRST opb WHERE opb.op-transaction EQ opo.op-transaction
                             AND opb.acct-cat       EQ "b"
                             AND opb.op-status      BEGINS "�"
               NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST opb WHERE opb.op EQ vOpBal NO-LOCK NO-ERROR.
        
         CREATE ttKauRidOp.
         ASSIGN
            ttKauRidOp.rid-kau   = RECID(kau)
            ttKauRidOp.order-pay = IF AVAIL(opb) THEN opb.order-pay ELSE ""
            ttKauRidOp.doc-date  = IF avail(opo) THEN opo.doc-date ELSE ?
            ttKauRidOp.sort      = kau.sort
         .
      END.
      ELSE DO:

         CREATE ttKauRidOp.
         ASSIGN
            ttKauRidOp.rid-kau   = RECID(kau).

      END.
   END.
END PROCEDURE.
