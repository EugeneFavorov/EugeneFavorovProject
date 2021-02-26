/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: safeprn1.p
      Comment: ������� - ���� "������� �� �७�� �祩��"
   Parameters:
         Uses:
      Used by:
      Created: 12.05.2008 10:27 TURIN  
     Modified: 15.07.2007 13:48 KAA     ��ࠡ�⪠ ��� �ᯥ�⪨ ������� � Word
     Modified: 11/11/2008 kraw (0094516) ���ꥬ � �᭮���� �����      
*/

/* ��᫨� ������쭮 */
{globals.i}
/* ������砥� ttnames */
{prn-doc.def &with_proc=YES}
/* ������砥� tmprecid */
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}

DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCustName    AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mValName     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress1    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress2    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vDecStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mError       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSex         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mWord        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mMausumbaeva AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mShaboldina  AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mText        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDocV        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE phone        AS CHARACTER          NO-UNDO INIT ''.

PAUSE 0.

/*��뢠�� ��� ��� ��।������ �㤥� ������ � Word ��� ���*/
FORM 
   mWord LABEL "�뢮���� � Word" VIEW-AS TOGGLE-BOX
   WITH FRAME fIs1 CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

UPDATE mWord WITH FRAME fIs1
EDITING:
   READKEY.

   IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
   DO:
      HIDE FRAME fIs1 NO-PAUSE.
      RETURN.
   END.
   ELSE
      APPLY LASTKEY.
END.
HIDE FRAME fIs1.



PAUSE 0.

IF shFilial EQ "0000" THEN DO:
FORM 
   mMausumbaeva LABEL "������� ����㬡���� �.�.   " VIEW-AS TOGGLE-BOX 
   WITH FRAME fIs2 CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

UPDATE mMausumbaeva WITH FRAME fIs2
EDITING:
   READKEY.

   IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN 
   DO:
      HIDE FRAME fIs2 NO-PAUSE.
      RETURN.
   END.
   ELSE
      APPLY LASTKEY.
END.
HIDE FRAME fIs2.



FORM 
   mShaboldina LABEL "������� ��������� �.�." VIEW-AS TOGGLE-BOX 
   WITH FRAME fIs2 CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

UPDATE mShaboldina WITH FRAME fIs2
EDITING:
   READKEY.

   IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN 
   DO:
      HIDE FRAME fIs2 NO-PAUSE.
      RETURN.
   END.
   ELSE
      APPLY LASTKEY.
END.
HIDE FRAME fIs2.
END.



RUN Insert_TTName("gend-date", term2str(gend-date, gend-date)).
/*������������ � ���� �����*/
FIND FIRST branch WHERE branch.Branch-Id EQ shfilial NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("bank-name",    branch.name). 
   RUN Insert_TTName("bank-address", branch.Address). 
   RUN Insert_TTName("bank-inn",     GetXattrValue("branch", 
                                                   STRING(branch.Branch-Id), 
                                                   "���")). 
   RUN Insert_TTName("bank-bik",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "�������")). 
END.

/* 横� �� �樤�� ��࠭��� ������஢ */
FOR EACH tmprecid NO-LOCK, 
    FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK :

    IF loan.cust-cat NE "�" THEN
    DO:
       mError = "������� " + loan.doc-ref + " ����� �� �� 䨧.���!".
       LEAVE.
    END.
    RUN Insert_TTName("loan-num", loan.doc-ref).

    FIND FIRST person WHERE person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.

    IF NOT AVAILABLE person THEN
       NEXT.

    RUN GetCustName IN h_base(loan.cust-cat,
                              loan.cust-id,
                              ?,
                              OUTPUT mCustName[1],
                              OUTPUT mCustName[2],
                              INPUT-OUTPUT mCustName[3]).
                             
    IF person.gender = TRUE THEN
      mSex = "�-�".
    ELSE
      mSex = "�-��".

    RUN Insert_TTName("sex",             mSex).
    RUN Insert_TTName("cust-name",       mCustName[1] + " " + mCustName[2]).
    RUN Insert_TTName("cust-last-name",  mCustName[1]).
    RUN Insert_TTName("cust-first-name", ENTRY(1, mCustName[2], " ")).

	  phone = trim(person.phone[1], ',').
	  phone = phone + trim(person.phone[2], ',').
	  RUN Insert_TTName("⥫",  phone).
	
    IF NUM-ENTRIES(mCustName[2], " ") GT 1 THEN
       RUN Insert_TTName("cust-sur-name", ENTRY(2, mCustName[2], " ")).
    ELSE
       RUN Insert_TTName("cust-sur-name", "").

    RUN Insert_TTName("birthday", term2str(person.birthday, person.birthday)).
    RUN Insert_TTName("birthplace", GetXAttrValueEx("person",STRING(person.person-id), "BirthPlace","")).
    RUN Insert_TTName("inn",      STRING(person.inn)).

   IF NUM-ENTRIES(person.document, " ") GE 3 THEN DO:
      RUN Insert_TTName("doc-num-s", ENTRY(1,person.document,' ') + ' ' + ENTRY(2,person.document,' ')).
      RUN Insert_TTName("doc-num-n", ENTRY(3,person.document,' ')).
    END.
    ELSE
      RUN Insert_TTName("doc-num-n", person.document).

    mDocV = fGetDocIssue(person.person-id).
    IF NUM-ENTRIES(mDocV) > 1 THEN DO:
      mDocV = REPLACE(mDocV,",",", �/�").
      SUBSTRING(mDocV,LENGTH(mDocV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
      mDocV = REPLACE(mDocV,"/",".") + " �.".
    END.
    ELSE DO:
      SUBSTRING(mDocV,LENGTH(mDocV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
      mDocV = REPLACE(mDocV,"/",".") + " �.".
    END.
    RUN Insert_TTName("issuer-full", mDocV).

    RUN Insert_TTName("doc-type",   person.document-id).
    RUN Insert_TTName("doc-num",    person.document).
    RUN Insert_TTName("issuer",     person.issue).
    RUN Insert_TTName("issue-date", GetXAttrValue("person",
                                                  STRING(person.person-id),
                                                  "Document4Date_vid")).

	  RUN RetAdr.p(person.person-id,"�","����ய",?,OUTPUT mAddress1).
    RUN Insert_TTName("address",mAddress1).

    RUN RetAdr.p(person.person-id,"�","�������",?,OUTPUT mAddress2).
    IF mAddress2 EQ '' THEN
      RUN Insert_TTName("address-p",mAddress1).
    ELSE
      RUN Insert_TTName("address-p",mAddress2).

   FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                         AND loan-cond.cont-code EQ loan.cont-code
                         AND loan-cond.since     LE gend-date
      NO-LOCK NO-ERROR.

   IF AVAILABLE loan-cond THEN DO:
      RUN Insert_TTName("safe-num", GetXAttrValue("loan-cond", 
                                                  loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since), 
                                                  "safe-num")).
      /* ��� ���.ᮣ��襭�� �� �஫����樨 */
      RUN Insert_TTName("period-ds",    STRING(loan.end-date - loan.open-date) + " ��__ ").
      RUN Insert_TTName("open-date-ds", term2str(loan-cond.since, loan-cond.since)).
      RUN Insert_TTName("end-date-ds",  term2str(loan.end-date, loan.end-date)).
   END.
   ELSE DO:
      mError = "�� ������� �祩�� ��� ������� " + loan.doc-ref.
      LEAVE.
   END.

   RUN Insert_TTName("period",    STRING(loan.end-date - loan.open-date) + " ��__ ").
   RUN Insert_TTName("open-date", term2str(loan.open-date, loan.open-date)).
   RUN Insert_TTName("end-date",  term2str(loan.end-date, loan.end-date)).


   FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ "�ऎ��"
                                 AND  loan-acct.since    LE gend-date
      NO-LOCK NO-ERROR.

   IF AVAILABLE loan-acct THEN
      RUN Insert_TTName("cust-acct", ENTRY(1, loan-acct.acct, "@")).
   ELSE
      RUN Insert_TTName("cust-acct", "__________________").

   FIND LAST term-obl OF loan WHERE term-obl.idnt EQ 1 NO-LOCK NO-ERROR.

   IF loan.currency EQ "" THEN
      mValName = " � �㡫��".
   ELSE
   DO:
      FIND FIRST currency WHERE currency.currency EQ loan.currency NO-LOCK NO-ERROR.

      IF AVAILABLE currency THEN
         mValName = " � ����� " + currency.name-currenc.
   END.

   IF AVAILABLE term-obl THEN
   DO:
	    RUN x-amtstr.p (term-obl.amt-rub, loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
      RUN Insert_TTName("term-opl-sum",  STRING(term-obl.amt-rub, ">>,>>>,>>9.99") + " (" + REPLACE(vAmtStr,' ��',') ��') + " " + vDecStr).
      RUN Insert_TTName("term-opl",  "������६���� � ��⠢��� " + STRING(term-obl.amt-rub, ">>,>>>,>>9.99") + " (" + vAmtStr + " " + vDecStr + ")").
      RUN Insert_TTName("term-opl2", "�����ᠭ�� �������").
      RUN Insert_TTName("term-opl3", "� ������ �����ᠭ�� �������").
   END.
   ELSE
   DO:
      RUN Insert_TTName("term-opl2", "������ �� ��䨪�").
      RUN Insert_TTName("term-opl3", "� ���� ��।��� �믫��� �� ��䨪�").
      RUN Insert_TTName("term-opl",  "�� ��䨪�" + mValName).



/*   ???????????????????????????   */      

      RUN BeginCircle_TTName("o").
      RUN Insert_TTName("term-obl-str[o]", " ").
      RUN NextCircle_TTName("o").

      FOR EACH term-obl OF loan WHERE term-obl.idnt EQ 1
         NO-LOCK:
         RUN Insert_TTName("term-obl-str[o]", STRING(term-obl.end-date, "99/99/9999") + " "
                                            + STRING(term-obl.amt-rub, ">>,>>>,>>9.99")).
         RUN NextCircle_TTName("o").
         mText = mText + STRING(term-obl.end-date, "99/99/9999")   + "~n"
                       + STRING(term-obl.amt-rub, ">>,>>>,>>9.99") + "~n".
      END.

      RUN Insert_TTName("term-obl-str[o]", " ").
      RUN EndCircle_TTName("o").
   END.

   RUN Insert_TTName("term-obl-str", mText).

   /* �ᯥ��뢠�騩 ���짮��⥫� */
   FIND FIRST _user WHERE _Userid EQ userid("bisquit") NO-LOCK NO-ERROR.
   IF AVAIL _user THEN DO:
      /* RUN Insert_TTName("user-name",_User._User-Name). */
      IF mMausumbaeva THEN RUN Insert_TTName("user-name", GetXAttrValueEx("_User", "O0400MGK", "����", "")).
                      ELSE IF mShaboldina THEN RUN Insert_TTName("user-name", GetXAttrValueEx("_User", "O0400SOS", "����", "")).
                                          ELSE RUN Insert_TTName("user-name", GetXAttrValueEx("_User", STRING(_User._Userid), "����", "")).

      IF mMausumbaeva THEN RUN Insert_TTName("user-name-rp", GetXAttrValueEx("_User", "O0400MGK", "User-nameRP", "")).
                      ELSE IF mShaboldina THEN RUN Insert_TTName("user-name-rp", GetXAttrValueEx("_User", "O0400SOS", "User-nameRP", "")).
                                          ELSE RUN Insert_TTName("user-name-rp", GetXAttrValueEx("_User", STRING(_User._Userid), "User-nameRP", "")).

      IF mMausumbaeva THEN RUN Insert_TTName("user-dolg", GetXAttrValueEx("_User", "O0400MGK", "���������", "")). 
                      ELSE IF mShaboldina THEN RUN Insert_TTName("user-dolg", GetXAttrValueEx("_User", "O0400SOS", "���������", "")).
                                          ELSE RUN Insert_TTName("user-dolg", GetXAttrValueEx("_User", STRING(_User._Userid), "���������", "")).

      IF mMausumbaeva THEN RUN Insert_TTName("user-dolg-rp", GetXAttrValueEx("_User", "O0400MGK", "��������쐏", "")).      
                      ELSE IF mShaboldina THEN RUN Insert_TTName("user-dolg-rp", GetXAttrValueEx("_User", "O0400SOS", "��������쐏", "")).
                                          ELSE RUN Insert_TTName("user-dolg-rp", GetXAttrValueEx("_User", STRING(_User._Userid), "��������쐏", "")).

      IF mMausumbaeva THEN RUN Insert_TTName("user-dov", GetXAttrValueEx("_User", "O0400MGK", "����᭒����", "")).    
                      ELSE IF mShaboldina THEN RUN Insert_TTName("user-dov", GetXAttrValueEx("_User", "O0400SOS", "����᭒����", "")).
                                          ELSE RUN Insert_TTName("user-dov", GetXAttrValueEx("_User", STRING(_User._Userid), "����᭒����", "")).

    /*IF NUM-ENTRIES(_User._User-Name,".") EQ 1 THEN DO:
        mSignVL = ENTRY(1,_User._User-Name," ") + SUBSTRING(ENTRY(2,_User._User-Name," "),1,1)+ "." + SUBSTRING(ENTRY(3,_User._User-Name," "),1,1) + ".".
        RUN Insert_TTName("_User-NameT",mSignVL).
      END.
      ELSE 
        RUN Insert_TTName("_User-NameT",_User._User-Name).*/

   END.  

END.

/*     �����⥫�� �ਥ�   */

IF mError EQ "" THEN
DO:

    /* �� ����� */
   IF mWord THEN
      RUN printvd.p("safeprn1w", INPUT TABLE ttnames).
   ELSE
      RUN printvd.p("safeprn1", INPUT TABLE ttnames). 
END.
ELSE
   RUN Fill-SysMes("","","0",mError).

