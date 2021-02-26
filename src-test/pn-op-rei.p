/*
               ������᪠� ��⥣�஢����� ��⥬� �������
*/
DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mSessionID     AS char               NO-UNDO.
DEFINE VARIABLE mSessionID1    AS char               NO-UNDO.
DEFINE VARIABLE i              AS int                NO-UNDO.
DEFINE VARIABLE mCommis        AS char               NO-UNDO.
DEFINE VARIABLE mSummCommis    AS dec                NO-UNDO.
DEFINE VARIABLE mSumm          AS dec                NO-UNDO.
DEFINE VARIABLE par-cont-code  AS char               NO-UNDO.
DEFINE VARIABLE mPol           AS char               NO-UNDO.
DEFINE VARIABLE mtoday         AS date               NO-UNDO.
DEFINE VARIABLE mop-date1      AS date               NO-UNDO.
DEFINE VARIABLE mUser          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE iBranch        AS CHAR               NO-UNDO.
DEFINE VARIABLE user_          AS CHAR               NO-UNDO.
DEFINE VARIABLE str_title      AS CHAR               NO-UNDO.
DEFINE VARIABLE shfilial_      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mStrTable      AS CHAR               NO-UNDO.
DEFINE VARIABLE SummaStr       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mSignsVal      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAcct          AS char               NO-UNDO.
DEFINE VARIABLE mContCode      AS char               NO-UNDO.


{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get db2l}
{intrface.get pn}
{flt-val.i}
{tmprecid.def}
{parsin.def}
{wordwrap.def}
/*
{prn-doc.def &with_proc = YES}
*/
/* ������砥� ttnames */
{prn-doc.def &with_proc=YES}
/* ������砥� tmprecid */
{tmprecid.def}

DEFINE TEMP-TABLE tSessionID NO-UNDO
   FIELD fop          AS int
   FIELD fSessionID   AS int
   FIELD fUser-id     AS CHARACTER
   FIELD fdpr-id      AS int
   FIELD fop-date     AS date
   FIELD frecid       AS recid
   FIELD facct        AS CHARACTER
   INDEX ifirst fSessionID
.
DEFINE TEMP-TABLE tSession NO-UNDO
   FIELD fFIO         AS CHARACTER
   FIELD fSummaPlat   AS Dec
   FIELD fSummaComm   AS Dec
   FIELD fSumma       AS Dec
   FIELD fSymbol      AS CHARACTER
   FIELD fPol         AS CHARACTER
   FIELD fAccPol      AS CHARACTER
   FIELD fop          AS CHARACTER
   INDEX ifirst fSummaPlat
.
DEFINE TEMP-TABLE tSymbol NO-UNDO
   FIELD fSummaPlat   AS Dec
   FIELD fSummaComm   AS Dec
   FIELD fSumma       AS Dec
   FIELD fSymbol      AS CHARACTER
   INDEX ifirst fSymbol
.
DEFINE TEMP-TABLE tItogo NO-UNDO
   FIELD fSummaPlat   AS Dec
   FIELD fSummaComm   AS Dec
   FIELD fSumma       AS Dec
.

DEFINE BUFFER bop FOR op.
DEFINE BUFFER bop-entry FOR op-entry.


/* 横� �� �樤�� ��࠭��� ��⮢ */
user_ =  USERID("bisquit").
mSessionID1 = GetFltVal("sv-5").
mop-date1   = DATE(GetFltVal('op-date1')).
find first sessions where sessions.dpr-id = int(msessionID1) no-lock no-error.
for each op where op.op-date eq sessions.op-date
              and op.user-id eq sessions.user-id
    no-lock,
first op-entry of op
    no-lock:

    mSessionID    = GetXAttrValueEx("op", STRING(op.op), "SessionID","").
    create tSessionID.
    tSessionID.fop         = op.op.
    tSessionID.fSessionID  = int(mSessionID).
    tSessionID.fUser-id    = sessions.user-id.
    tSessionID.fdpr-id     = sessions.dpr-id.
    tSessionID.fop-date    = op.op-date.
    tSessionID.frecid      = recid(op).
    tSessionID.facct       = op-entry.acct-db.
end.
find last tSessionID no-lock no-error.
mSessionID = string(tSessionID.fSessionID).
mAcct      = tSessionID.facct.
/*
output to "SessionID.txt".
for each tSessionID.
   export tSessionID.
end.
output close.
*/
message "������� ����� �� ��᫥����� ३��? (३� � " mSessionID " )."  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE select_item AS LOGICAL.

if not select_item then return.

i = 1.
find first acct where acct.acct = mAcct no-lock no-error.
if avail acct then do:
   iBranch = acct.branch-id.
   FIND FIRST branch WHERE branch.Branch-Id EQ iBranch NO-LOCK NO-ERROR.
   IF AVAILABLE branch THEN
   DO:
      RUN Insert_TTName("BranchName",    branch.name).
      RUN Insert_TTName("BranchAddress", branch.Address).
   end.
end.

&GLOB Months-i "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Days "��ࢮ�,��஥,����,�⢥�⮥,��⮥,��⮥,ᥤ쬮�,���쬮�,����⮥,����⮥,��������⮥,�������⮥,�ਭ���⮥,���ୠ��⮥,~
��⭠��⮥,��⭠��⮥,ᥬ����⮥,��ᥬ����⮥,����⭠��⮥,�����⮥,������� ��ࢮ�,������� ��஥,������� ����,~
������� �⢥�⮥,������� ��⮥,������� ��⮥,������� ᥤ쬮�,������� ���쬮�,������� ����⮥,�ਤ�⮥,�ਤ��� ��ࢮ�"
&GLOB Years "��ࢮ��,��ண�,���쥣�,�⢥�⮣�,��⮣�,��⮣�,ᥤ쬮��,���쬮��,����⮣�,����⮣�,�������⮣�,�������⮣�,�ਭ���⮣�,~
���ୠ��⮣�,��⭠��⮣�,��⭠��⮣�,ᥬ����⮣�,��ᥬ����⮣�,����⭠�⮣�,�����⮣�,������� ��ࢮ��,������� ��ࢮ��,~
������� ��ண�,������� ���쥣�,������� �⢥�⮣�,������� ��⮣�,��⮣�,������� ᥤ쬮��,������� ���쬮��,������� ����⮣�,�ਤ�⮣�,�ਤ��� ��ࢮ��,~
�ਤ��� ��ࢮ��,�ਤ��� ��ண�,�ਤ��� ���쥣�,�ਤ��� �⢥�⮣�,�ਤ��� ��⮣�,�ਤ��� ��⮣�,�ਤ��� ᥤ쬮��,�ਤ��� ���쬮��,�ਤ��� ����⮣�"

mtoday = today.
RUN Insert_TTName("Today",mtoday).
RUN Insert_TTName("TodayT",string(mtoday,"99.99.9999")).
RUN Insert_TTName("TodayStr",  STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + " �.").


find first _user where _user._userid = user_ no-lock no-error.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).

str_title = "[ �롥�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
mUser = "".
run signat.p ("pn-op-re","UserName","",OUTPUT mUser).

if mUser = ? then return.

RUN Insert_TTName("PodUserDolg", GetXAttrValueEx("_User", mUser, "���������", "")).
RUN Insert_TTName("PodUserFio", GetXAttrValueEx("_User", mUser, "����", "")).
RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "����᭒����", "")).
RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "����䮭", "")).

find first _user where _user._userid = mUser no-lock no-error.
RUN Insert_TTName("Pod-user-f", _user._User-Name).
RUN Insert_TTName("PodIspDolg", GetXAttrValueEx("_User", User_, "���������", "")).
RUN Insert_TTName("PodIspFio", GetXAttrValueEx("_User", User_, "����", "")).
RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "����᭒����", "")).
RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "����䮭", "")).

/*������������ � ���� �����*/
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",    branch.name).
   RUN Insert_TTName("go-bank-address", branch.Address).
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)).
END.

shfilial_ = GetXAttrValueEx("_User", mUser, "filial-id", "").

FIND FIRST branch WHERE branch.Branch-Id EQ shfilial_ NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("bankname",    branch.name).
   RUN Insert_TTName("bankaddress", branch.Address).
   RUN Insert_TTName("gorod", entry(2,branch.Address)).
   RUN Insert_TTName("bank-inn",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "���")).
   RUN Insert_TTName("bank-bik",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "�������")).
   RUN Insert_TTName("bank-kpp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "���")).
   RUN Insert_TTName("bank-ks",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "�����")).
   RUN Insert_TTName("bank-KS-GDE",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "��爧�������")).
   RUN Insert_TTName("bank-tel",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "����䮭")).
   RUN Insert_TTName("bank-ogrn",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "����")).
   RUN Insert_TTName("bank-addr-post",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "��瀤�����")).
   RUN Insert_TTName("bank-addr-kor",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "��瀤����")).
   RUN Insert_TTName("bank-dps_bank",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "dps_bank")).
END.
/* ४������ ����� */
if branch.bank-id <> ? then do:
   find first banks-code where banks-code.bank-id = branch.bank-id no-lock no-error.
   if avail banks-code then do:
      RUN Insert_TTName("BankCode", banks-code.bank-code).
   end.
end.

RUN Insert_TTName("mSessionID", mSessionID).
for each tSessionID where tSessionID.fSessionID eq int(mSessionID) NO-LOCK,
   FIRST op       WHERE RECID(op) EQ tSessionID.frecid NO-LOCK :
   FOR EACH xlink WHERE
      TRUE
      AND NOT xlink.hidden
      AND xlink.class-code EQ 'opbp-svod' NO-LOCK,
      EACH links WHERE
          links.link-id   = xlink.link-id
      AND links.source-id = string(op.op)
      AND links.beg-date  = op.op-date
      AND xlink.link-direction BEGINS "s"
      NO-LOCK:

      FIND FIRST bop       WHERE bop.op  EQ int(links.target-id) NO-LOCK NO-error.
      if not avail bop then do:
         message "���㬥�� " links.target-id  " �� ������ " view-as alert-box.
      end.
      find FIRST op-entry WHERE op-entry.op EQ int(links.target-id) NO-LOCK no-error.
      if not avail op-entry then do:
         message "�஢���� � ���㬥��� " links.target-id  " �� ������� " view-as alert-box.
      end.
      mSummCommis = 0.
      mCommis  = GetXattrValueEx("op",STRING(bOp.op),"��������㬥�⠊����ᨨ",?).
      if  mCommis <> ? then do:
         find FIRST bop-entry WHERE bop-entry.op EQ int(mCommis) NO-LOCK no-error.
         if not avail bop-entry then do:
            message "�஢���� � ���㬥��� �����ᨨ " mCommis " �� ������� " view-as alert-box.
         end.
         mSummCommis = bop-entry.amt-rub.

      end.
      mSumm = mSummCommis + op-entry.amt-rub.
      par-cont-code = entry(2,GetXattrValueEx("op",STRING(bOp.op),"�������ࠬ��ࠏ��⥦�",?)).
      mPol  = "".
      Find first loan WHERE loan.class-code EQ 'loan-pn-par'
                        AND loan.cont-code EQ par-cont-code
                        AND loan.contract EQ 'pn-par'
                        AND loan.filial-id EQ shfilial
                        NO-LOCK no-error.
      if avail loan then do:
         mContCode = loan.parent-cont-code.
         mPol  = GetXAttrValueEx("loan",
                                         "pn," + mContCode,
                                         "�������⥫썠����������",
                                         "").
      end.
      create tSession.
      ASSIGN
         tSession.fSummaPlat = op-entry.amt-rub
         tSession.fSummaComm = mSummCommis
         tSession.fSumma     = mSumm
         tSession.fSymbol    = if op-entry.Symbol = ? then " " else op-entry.Symbol
         tSession.fop        = string(bop.op)
         tSession.fFIO       = GetXattrValueEx("op",STRING(bOp.op),"���"," ")
         tSession.fAccPol    = GetXattrValueEx("op",STRING(bOp.op),"�������⥫���"," ")
         tSession.fPol       = if mPol = ? then " " else mPol
      .

      i = i + 1.

   END.
END.

RUN Insert_TTName ("info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'info'
NO-LOCK NO-ERROR.
mStrTable = "".
i = 1.
for each tSession.
   find first tSymbol where tSymbol.fSymbol eq tSession.fSymbol no-error.
   if not avail tSymbol then do:
      create tSymbol.
      tSymbol.fSymbol = tSession.fSymbol.
   end.
   mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                     ELSE "") +
   TRIM(STRING(i)) + "~n" +
   TRIM(tSession.fFIO) + "~n" +
   TRIM(string(tSession.fSummaPlat)) + "~n" +
   TRIM("'") +    TRIM(tSession.fSymbol) + "~n" +
   TRIM(string(tSession.fSummaComm)) + "~n" +
   TRIM(string(tSession.fSumma)) + "~n" +
   TRIM(tSession.fPol) + "~n" +
   TRIM("'" + tSession.fAccPol)
   .
   ASSIGN
      tSymbol.fSummaPlat = tSymbol.fSummaPlat  + tSession.fSummaPlat
      tSymbol.fSummaComm = tSymbol.fSummaComm + tSession.fSummaComm
      tSymbol.fSumma     = tSymbol.fSumma      + tSession.fSumma
   .
   i = i + 1.
end.

for each tSymbol.
   find first tItogo no-error.
   if not avail tItogo then do:
      create tItogo.
   end.
   ASSIGN
      tItogo.fSummaPlat = tItogo.fSummaPlat  + tSymbol.fSummaPlat
      tItogo.fSummaComm = tItogo.fSummaComm  + tSymbol.fSummaComm
      tItogo.fSumma     = tItogo.fSumma      + tSymbol.fSumma
   .

   mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                     ELSE "") +
   TRIM(STRING("'")) + "~n" +
   TRIM("�⮣� �� ᨬ����") + "~n" +
   TRIM(string(tSymbol.fSummaPlat)) + "~n" +
   TRIM("'") + TRIM(tSymbol.fSymbol) + "~n" +
   TRIM(string(tSymbol.fSummaComm)) + "~n" +
   TRIM(string(tSymbol.fSumma)) + "~n" +
   TRIM("'") + "~n" +
   TRIM("'")
   .
end.
for each tItogo.
   mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                     ELSE "") +
   TRIM(STRING("'")) + "~n" +
   TRIM("�⮣� �� ॥���� ") + "~n" +
   TRIM(string(tItogo.fSummaPlat)) + "~n" +
   TRIM("'") + "~n" +
   TRIM(string(tItogo.fSummaComm)) + "~n" +
   TRIM(string(tItogo.fSumma)) + "~n" +
   TRIM("'") + "~n" +
   TRIM("'")
   .
   RUN x-amtstr.p(tItogo.fSummaPlat,"", TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
   mSignsVal = SummaStr[1] + " " + SummaStr[2].
   RUN Insert_TTName("SummaPR", "���� �㬬� �ਭ���� ���⥦�� �� ॥����: " + mSignsVal ).
   RUN x-amtstr.p(tItogo.fSummaComm,"", TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
   mSignsVal = SummaStr[1] + " " + SummaStr[2].
   RUN Insert_TTName("SummaPC", "���� �㬬� �����ᨮ����� ������ࠦ����� ����� �� ॥����: " + mSignsVal ).
   RUN x-amtstr.p(tItogo.fSumma,"", TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
   mSignsVal = SummaStr[1] + " " + SummaStr[2].
   RUN Insert_TTName("Summa","�ᥣ� �� ॥����: " + mSignsVal ).

end.


RUN INSERT_TTNAME ("Info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'Info'
NO-LOCK NO-ERROR.
ttnames.tvalue = mStrTable.


RUN printvd.p ("pn-op-re",INPUT TABLE ttnames).

message "������ ��ନ஢��." view-as alert-box.



