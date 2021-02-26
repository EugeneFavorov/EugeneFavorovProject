{globals.i}

/* +++ voc-nps.p was humbly modified by (c)blodd converter v.1.09 on 1/18/2017 1:13pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1997 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: voc-nps.p
      Comment: ��楤�� ���� �ࠢ�� �� �����⢫���� ����権
               � ����筮� ����⮩ � 祪���. (���)
               (������ ��⥬� ����)
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: 
      Used by:
      Created: 11.09.2004 16:37 ligp     31278: ��ࠡ�⪠ � ���㫥 ��� � ᢥ� ������樨 113-� (������ �ࠢ��)
     Modified: 14.09.2004 15:18 ligp     31278: �������� 113-� (������ �ࠢ��)
     Modified: 15.09.2004 13:23 ligp     31278: ��������� ���� "��� �뤠� ���㬥��"
     Modified: 24.09.2004 16:03 ligp     32092: ��騥 � ��㣨�� ���⠬� ��६���� �� agree.def
     Modified: 05.10.2004 13:20 ligp     32095: �������� � 1433-� (�ਫ������ 4)֊���� ��� ���.
                                         ����筮�� � ��, 業���⥩�
     Modified: 09.12.2004 12:30 rija     
     Modified: 25.03.2005 18:08 rija     42675: �ॡ������ ����� ������୮���. �業��� ��㤮�������
                                         ࠡ��.
     Modified: 31.05.2005 SChurkin       0045001 � �㭪�� GetDateTimeOpTr �������� ��ࠬ��� op.op
     Modified: 14.08.2006 15:37 ELUS     0064978
     Modified: 25.08.2006 16:58 ELUS     0064978: � �ࠢ�� �뤠������ �� ���㬥��� �த��� �� �㬬�
                                         �ਭ���� ������� 㤢��������
     Modified: 05.09.2006 15:05 ELUS     0064978: � �ࠢ�� �뤠������ �� ���㬥��� �த��� �� �㬬�
                                         �ਭ���� ������� 㤢��������
     Modified: 16.09.2008 14:39 elus      0125593: �訡�� ���� ॥��� �� ����
     
*/

DEFINE INPUT PARAMETER iRID AS RECID NO-UNDO.

{globals.i}                     /* �������� ��६���� */
{intrface.get xclass}
{intrface.get tparam}
{intrface.get sessions} /* pp-sessi.p - �����㬥�� ��� ࠡ��� � ᬥ���� (⠡��� sessions). */
{intrface.get vok}     /* �����㬥��� ��� ࠡ��� � ��쥪⠬� ��� pp-vok.p */
{intrface.get tmess}
{intrface.get db2l}
{limitsum.chk}

{prn-doc.def &with_proc=YES}

DEFINE VARIABLE vLogPrnCommm AS LOGICAL FORMAT "YES/NO"  NO-UNDO.
vLogPrnCommm = LOGICAL(GetSysConf ("vok-print-commission")).
RUN DeleteOldDataProtocol IN h_base ("vok-print-commission").

DEFINE VARIABLE mOpCurrCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mContrAct   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFIO        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocum      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocId      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocWho     AS CHARACTER NO-UNDO. /* ��� �뤠� ���㬥�� */
DEFINE VARIABLE mKP         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrDb     AS CHARACTER INITIAL "---" NO-UNDO.   /* ��� �ਭ�⮩ ������ */
DEFINE VARIABLE mSummaDb    AS DECIMAL   INITIAL 0     NO-UNDO.   /* ����祭� ����筮�� */
DEFINE VARIABLE mCurrCr     AS CHARACTER INITIAL "---" NO-UNDO.   /* ��� �뤠���� ������ */
DEFINE VARIABLE mSummaCr    AS DECIMAL   INITIAL 0     NO-UNDO.   /* �뤠�� ����筮�� */
DEFINE VARIABLE mCurrCheq   AS CHARACTER INITIAL "---" NO-UNDO.   /* ��� ������ 祪� */
DEFINE VARIABLE mQtyCheq    AS INT64   INITIAL 0     NO-UNDO.   /* ������⢮ 祪�� */
DEFINE VARIABLE mSumCheq    AS DECIMAL   INITIAL 0     NO-UNDO.   /* �㬬� �� 祪�� */ 
DEFINE VARIABLE mDoc-num    AS CHARACTER NO-UNDO. /* ����� ���㬥�� */

DEFINE VARIABLE mOpTime     AS INT64   NO-UNDO. /* �६� ��砫� �࠭���樨 */
DEFINE VARIABLE mOpDate     AS DATE      NO-UNDO. /* ��� ��砫� �࠭���樨 */
DEFINE VARIABLE mIsCheckBuy AS LOGICAL   NO-UNDO. /* Yes - ����� 祪�� �㯨�� */
DEFINE VARIABLE mIsCheckSel AS LOGICAL   NO-UNDO. /* Yes - ����� 祪�� �த��� */
DEFINE VARIABLE mAdrPodrSpr AS LOGICAL   NO-UNDO. /* ���������� */
DEFINE VARIABLE mTimeSep    AS CHARACTER NO-UNDO INIT ":".
DEFINE VARIABLE mTimeFormat AS CHARACTER NO-UNDO.

/* ����� ��� �㦭�� ��� ⠡��� */
DEFINE BUFFER bOp   FOR op.   
DEFINE BUFFER bCode FOR code.   
DEFINE BUFFER bCurrency FOR currency.   

FIND FIRST bOp WHERE RECID(bOp) EQ iRID NO-LOCK NO-ERROR.

IF NOT AVAILABLE bOp THEN 
DO:
   RUN Fill-AlertSysMes IN h_tmess("","",-1,"���㬥�� �� ������").

   RETURN.
END.

{wordwrap.def} /* �㦥� ��� wordwrap.i, ��⮢� �ᯮ��. � agr-beg.def */
{agr-beg.def 
   &NameTitle = "������ �������� � ������� � ������"
   &TypeDoc   = '"spr"'} /* ���� �� ᯨ᮪ ᬥ� �� ���ଠ�஢���� */   

/* ���筮 � ����� � ��뢠� {agr-beg.i} ��� HeadInRep � FootInRep,
 � ����� mCuUserID � mCuBrchID �㦭� ��।����� �� ���㬥��� */

ASSIGN
   mOpDprID    = GetXAttrValueEx("op",STRING(bOp.op),"dpr-id","?") /* ID ᬥ�� � ���㬥�� */
   mCuUserID   = GetUserIDFromSessions(INT64(mOpDprID))
   mCuBrchID   = GetBranchIDFromSessions(INT64(mOpDprID))
   mAdrPodrSpr = fGetSetting("����������","","") EQ "��"
   mTimeFormat = fGetSetting("��ଠ�६��ࠢ","","��|:")
   .

IF NUM-ENTRIES(mTimeFormat,"|") GE 2 THEN
   mTimeSep = ENTRY(2,mTimeFormat,"|").
mTimeFormat = IF ENTRY(1,mTimeFormat,"|") EQ "���" THEN "HH:MM" ELSE "HH:MM:SS".

RUN HeadInRep. 
RUN FootInRep. 
RUN GetDateTimeOpTr(bOp.op-transaction, bOp.op,OUTPUT mOpTime,OUTPUT mOpDate).

DEFINE VARIABLE vREGN       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAdresPch   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vBank       AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE vBranchAttr AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE mHeadLine   AS CHARACTER   NO-UNDO EXTENT 10.
DEFINE VARIABLE vFilialType AS CHARACTER   NO-UNDO.

ASSIGN
   vFilialType    = fGetSetting("Filial","FilialType","10,11")
   vREGN          = FGetSetting("REGN",?,"")
   vREGN          = vREGN + ( IF {assigned mBranchREGN} AND mBranchREGN NE vREGN THEN ("/" + mBranchREGN) ELSE "")
   vAdresPch      = FGetSetting("����_��",?,?)
   vBank[1]       = mBank
   .
IF NOT CAN-DO(vFilialType,GetBufferValue("branch","where branch.branch-id EQ '" + bop.branch-id + "'","branch-type")) THEN
   vBranchAttr[1] = mBranchName + " " + mVOKAddr[1].

{wordwrap.i
  &s = vBank
  &n = 2
  &l = 60
}
{wordwrap.i
  &s = vBranchAttr
  &n = 2
  &l = 60
}

ASSIGN
   mHeadLine[1] = "������ (᮪�饭���) �ଥ���� ������������"         + FILL(" ",11) + vBank[1]
   mHeadLine[2] = "㯮�����祭���� �����"                               + FILL(" ",33) + vBank[2]
   mHeadLine[3] = "(������������ 䨫����)"
   mHeadLine[4] = "�������樮��� ����� 㯮�����祭���� �����/"        + FILL(" ",10) + STRING(vREGN)
   mHeadLine[5] = "���浪��� ����� 䨫����"
   mHeadLine[6] = "���⮭�宦����� (����) 㯮�����祭���� �����"       + FILL(" ",9) + vAdresPch     
   mHeadLine[7] = "(䨫����)"
   mHeadLine[8] = "������������ ����७���� ������୮�� ���ࠧ�������" + FILL(" ",3) + vBranchAttr[1]
   mHeadLine[9] = "㯮�����祭���� ����� � ��� ���⮭�宦����� (����)" + FILL(" ",3) + vBranchAttr[2]
   .

DO i = 1 TO 9:
   RUN Insert_TTName("HeadLine" + STRING(i),mHeadLine[i]).
END.

IF mAdrPodrSpr THEN
DO:
   RUN Insert_TTName("Bank",mBank).
   RUN Insert_TTName("Regn",FGetSetting("REGN",?,"")).
   RUN Insert_TTName("Address",FGetSetting("����_��",?,?)).
   RUN Insert_TTName("BranchName", mBranchName).
   RUN Insert_TTName("BranchRegn", mBranchREGN).
   RUN Insert_TTName("BranchAddress", mVOKAddr[1]).
END.
ELSE
DO:
   RUN Insert_TTName("Bank", mParentName).
   RUN Insert_TTName("Regn", mBranchREGN).
   RUN Insert_TTName("Address", mVOKAddr [1]).
END.
 
FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.

RUN Insert_TTName("op-date", STRING(mOpDate)).
RUN Insert_TTName("op-time", REPLACE(STRING(mOpTime,mTimeFormat),":",mTimeSep) + IF CAN-DO('0516,0517', STRING(GetXattrValueEx("_user",STRING(_user._userid),"�⤥�����",""))) THEN ' (�६� ��᪮�)' ELSE '').

mDoc-num = GetXAttrValueEx("op", STRING(bOp.op), "���������", "").
IF mDoc-num EQ "" THEN
   mDoc-num = bOp.doc-num.

RUN Insert_TTName("doc-num", TRIM(mDoc-num)).

mOpCurrCode = GetXAttrValueEx("op", STRING(bOp.op), "���������", "").

RUN Insert_TTName("OpCurrCode", mOpCurrCode).
RUN Insert_TTName("OpCurrName", GetCodeName("���������", mOpCurrCode)).

ASSIGN
   mFIO    = GetXAttrValueEx("op", STRING(bOp.op), "���", "")
   mDocum  = GetXAttrValueEx("op", STRING(bOp.op), "����", "")
   mDocId  = GetCodeName("�������",GetXAttrValue("op",STRING(bOp.op),"document-id")) 
   mDocWho = GetXAttrValueEx("op", STRING(bOp.op), "cust-doc-who", "")
   mKP     = GetXAttrValueEx("op", STRING(bOp.op), "���ࠧ�", "")
.

/* �� ���� �� ��蠥�, � ����, ������, �ਣ������ �����, ��᫥ ॠ����樨 32412*/
/* mDocWho ⮣�� �� �ய�襬 */
IF    mDocum  EQ ""
   OR mDocId  EQ ""
   OR mFIO    EQ ""
   OR mDocWho EQ "" THEN
DO:
   FOR EACH op-entry OF bOp NO-LOCK:
      FIND FIRST acct WHERE 
                 acct.acct     EQ op-entry.acct-db
             AND acct.cust-cat EQ "�" 
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         FIND FIRST person WHERE 
                    person.person-id EQ acct.cust-id 
            NO-LOCK NO-ERROR.
      IF AVAIL person THEN 
         LEAVE.
      FIND FIRST acct WHERE 
                 acct.acct     EQ op-entry.acct-cr
             AND acct.cust-cat EQ "�" 
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         FIND FIRST person WHERE 
                    person.person-id EQ acct.cust-id 
            NO-LOCK NO-ERROR.
      IF AVAIL person THEN 
         LEAVE.
   END.
   IF AVAIL person THEN
      ASSIGN
         mDocum  = IF mDocum  EQ "" THEN person.document  ELSE mDocum
         mDocId  = IF mDocId  EQ "" THEN GetCodeName("�������",person.document-id) ELSE mDocId
         mFIO    = IF mFIO    EQ "" THEN person.name-last + " " + person.first-names ELSE mFIO
         mDocWho = IF mDocWho EQ "" THEN person.issue ELSE mDocWho
      .
   IF NUM-ENTRIES(mDocWho) > 1 THEN
      ENTRY(2,mDocWho) = " �\� " + ENTRY(2,mDocWho).      
END.
ELSE
   IF {assigned mDocWho} THEN
   IF {assigned mKp} THEN 
      IF NUM-ENTRIES(mDocWho) > 1 THEN
         ENTRY(2,mDocWho) = " �\� " + mKp.
      ELSE
         mDocWho = mDocWho + ", �\� " + mKp.
   ELSE
      IF NUM-ENTRIES(mDocWho) > 1 THEN
         ENTRY(2,mDocWho) = " �\� " + ENTRY(2,mDocWho).
         
IF FGetSetting("���菥甈�", "", "���") EQ "��" THEN DO:
   RUN Fill-SysMes("", 
                   "", 
                   "4",
                   "����� ४������ ������ �� �ࠢ��?").  
   IF pick-value NE "YES" THEN
      ASSIGN
      mFIO    = ""
      mDocum  = ""
      mDocId  = ""
      mDocWho = ""
      .
END.
         
IF FGetSetting("��珑��뤠�", "", "��") EQ "���" THEN mDocWho = "".

mDocum = mDocId + " " + mDocum.
RUN Insert_TTName("FIO",    mFIO).
RUN Insert_TTName("docum",  mDocum).
RUN Insert_TTName("sprate", STRING(DECIMAL(GetXAttrValueEx("op", 
                                                           STRING(bOp.op), 
                                                           "sprate", 
                                                           "")),
                                    ">,>>9.9999")).
mDocWho = "�뤠�: " + mDocWho.
RUN Insert_TTName("CustDocWho",  mDocWho).


/* � ����� �ਬ���� �����㬥�� �� ������� ॥���, �⮡ �� �뫮 ���������, ���� �訡�� */

DEFINE VARIABLE mSumRub   AS DECIMAL   INITIAL 0  NO-UNDO.   /* �㡫��� �������� �㬬� ����樨  */
DEFINE VARIABLE mAcctCli  AS CHARACTER INITIAL "" NO-UNDO.   /* ��� ������ */


DEFINE VARIABLE mPrSumm AS CHARACTER INITIAL "" NO-UNDO. /* ��᪨ ��⮢, �������� � ���㬬� */

FOR EACH bCode WHERE bCode.class EQ "���㬬�" NO-LOCK:
   IF bCode.val NE "" THEN
      {additem.i mPrSumm SUBSTRING(bCode.val,1,5)}
END.

{rst-tab.fun}

RUN GetCodeCurAndSumm(bOp.op,
                      mPrSumm,
                      OUTPUT mCurrDb,               /* 5 */
                      OUTPUT mSummaDb,              /* 6 */
                      OUTPUT mCurrCr,               /* 7 */
                      OUTPUT mSummaCr,              /* 8 */
                      OUTPUT mQtyCheq,              /* 10 */
                      OUTPUT mCurrCheq,             /* 11 */
                      OUTPUT mSumCheq,              /* 12 */
                      OUTPUT mSumRub,
                      OUTPUT mAcctCli).             /* 13 */

{vprncomm.i bOp}
/* �����㬥�� �뤠�� "" �᫨ � ��� ࠧ��� ��祣� �� ���� ����� */
IF mCurrDb NE "" THEN 
DO:
   /* �����㬥�� �뤠�� ����� "" ��� ���. ������. ��室���� ��८�।����� */
   IF mCurrDb EQ mCodOurCur THEN
      mCurrDb = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrDb NO-LOCK NO-ERROR.

   IF AVAILABLE bCurrency THEN
      RUN Insert_TTName("CurrName", bCurrency.name-currenc). /* �ਭ�� ����� � ����� */
   IF mCurrDb EQ "" THEN
      mCurrDb = mCodOurCur.             
END. /* IF mCurrDb NE "" THEN */

IF mCurrCr NE "" THEN
DO:
   IF mCurrCr EQ mCodOurCur THEN
      mCurrCr = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrCr NO-LOCK NO-ERROR.

   IF AVAILABLE bCurrency THEN
      RUN Insert_TTName("CurrName1", bCurrency.name-currenc). /* �뤠�� ����� � ����� */
   IF mCurrCr EQ "" THEN
      mCurrCr = mCodOurCur.
END. /* IF mCurrCr NE "" THEN */

IF mCurrCheq NE "" THEN
DO:
   IF mCurrCheq EQ mCodOurCur THEN
      mCurrCheq = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrCheq NO-LOCK NO-ERROR.
   
   IF AVAILABLE bCurrency THEN
   DO:
      IF mIsCheckBuy EQ TRUE THEN
      DO:
         RUN Insert_TTName("CurrNameC", bCurrency.name-currenc). /* �ਭ�� 祪�� � ����� */
         RUN Insert_TTName("CurrCodeC", mCurrCheq).
         RUN Insert_TTName("AmtC",      STRING(mSumCheq,">>>,>>>,>>>,>>9.99") 
                                        + "  /  " + STRING(mQtyCheq)).  


         RUN Insert_TTName("CurrNameC1", ""). /* �뤠�� 祪�� � ����� */
         RUN Insert_TTName("CurrCodeC1", "").
         RUN Insert_TTName("AmtC1",      "").

         RUN Insert_TTName("TrCodeDb2", GetXAttrValue("op",STRING(bOp.op),"TrCodeDb")).
         RUN Insert_TTName("TrCodeCr2", "").

      END.                                                                                   
      IF mIsCheckSel EQ TRUE THEN
      DO:
         RUN Insert_TTName("CurrNameC", ""). /* �ਭ�� 祪�� � ����� */
         RUN Insert_TTName("CurrCodeC", "").
         RUN Insert_TTName("AmtC",      "").  

         RUN Insert_TTName("CurrNameC1", bCurrency.name-currenc). /* �뤠�� 祪�� � ����� */
         RUN Insert_TTName("CurrCodeC1", mCurrCheq).
         RUN Insert_TTName("AmtC1",      STRING(mSumCheq,">>>,>>>,>>>,>>9.99") 
                                         + "  /  " + STRING(mQtyCheq)).

         RUN Insert_TTName("TrCodeCr2", GetXAttrValue("op",STRING(bOp.op),"TrCodeCr")).
         RUN Insert_TTName("TrCodeDb2", "").
      END.                                                                                   
   END.
   IF mCurrCheq EQ "" THEN
      mCurrCheq = mCodOurCur.
END. /* IF mCurrCheq NE "" THEN */

RUN Insert_TTName("TrCodeDb", IF mSummaDb EQ 0 THEN
                                 ""
                              ELSE
                                 GetXAttrValue("op",STRING(bOp.op),"TrCodeDb")).
RUN Insert_TTName("TrCodeCr", IF mSummaCr EQ 0 THEN
                                 ""
                              ELSE
                                 GetXAttrValue("op",STRING(bOp.op),"TrCodeCr")).

RUN Insert_TTName("CurrCode",   mCurrDb).
RUN Insert_TTName("CurrCode1",  mCurrCr).

RUN Insert_TTName("Amt",      IF mSummaDb EQ 0 THEN 
                                 "" 
                              ELSE  
                                 STRING(mSummaDb,">>>,>>>,>>>,>>9.99")).   
RUN Insert_TTName("Amt1",     IF mSummaCr EQ 0 THEN
                                 ""
                              ELSE 
                                 STRING(mSummaCr,">>>,>>>,>>>,>>9.99")).
RUN Insert_TTName("post", mPostInRep[1]).
RUN Insert_TTName("user", mFIOInRep[1]).
RUN Insert_TTName("empty", FILL(" ",LENGTH(mPostInRep[1] + "  " + mFIOInRep[1]))).

{intrface.del} /* �� ��襫, �.� �� ��� �� ��������, ���ࠢ�� � */

RUN printvd.p(IF vLogPrnCommm THEN "���-������" ELSE "������", INPUT TABLE ttnames).
/* $LINTUSER='STRE' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/4.1d/1ut/src' */
/* $LINTDATE='29/09/2014 13:14:46.896+04:00' */
/* $LINTFILE='voc-nps.p' */
/*prosignFSXE+qFZu/pmGFYizSKoOg*/
/* --- voc-nps.p was humbly modified by (c)blodd converter v.1.09 on 1/18/2017 1:13pm --- */
