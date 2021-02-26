/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: precrdprint.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 06/08/2008 feok
     Modified: 14/10/2009 Jadv (0108906)
     Modified: 21/10/2013 Koan (0204655) ��������� �����প� ������஢ ���ᯥ祭��.
         �� ���� � ०���� MODE2, MODE3, MODE4.
*/

&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{filleps.def}
{norm.i NEW}
{svarloan.def NEW}
{getdate.i}
{intrface.get loan}
{lshpr.pro}
{def_work.i new}        /* ������� ��६����� ��� ࠡ��� �
                         ���᫥���� ��業⮢. */
{intrface.get refer} /*��� ࠡ��� � �ࠢ�筨����*/                         
{intrface.get date} /* .����㬥��� ��� ࠡ��� � ��⠬� */
{sh-defs.i}         /* .�।������ ��६�����, �ਮ������ ���祭�� ���⪠ */                 
/*--------------------*/
{intrface.get comm}
/*----------------------------*/
   /* ��ப� ��ࠬ��஢ */
DEF INPUT PARAM iStr AS CHAR NO-UNDO.

/*����窨 ���  �맮�� RE_PARAM*/
DEF VAR a1         AS DECIMAL   NO-UNDO.
DEF VAR c1         AS CHARACTER.
DEF VAR c2         AS CHARACTER.
DEF VAR mAcctType  AS CHARACTER NO-UNDO. 
DEF VAR mI         AS INT64     NO-UNDO.
DEF VAR a2         AS DECIMAL NO-UNDO.
DEF VAR par_0      AS DECIMAL NO-UNDO. /* ���⮪ �᭮����� ����� */
DEF VAR par_2      AS DECIMAL NO-UNDO. 
DEF VAR par_4      AS DECIMAL NO-UNDO. /* �㬬� % �� �᭮���� ���� 4,29 */
DEF VAR par_7      AS DECIMAL NO-UNDO. /* �㬬� �����. �᭮����� ����� */
DEF VAR par_8      AS DECIMAL NO-UNDO. /* �㬬� % �� �����. �᭮���� ���� 8,229,233 */
DEF VAR par_9      AS DECIMAL NO-UNDO. /* �㬬� ���� */ 
DEF VAR par_12     AS DECIMAL NO-UNDO. 
DEF VAR par_10     AS DECIMAL NO-UNDO. /* �㬬� �����. % 10,210,48,248 */
DEF VAR par_13     AS DECIMAL NO-UNDO. 
DEF VAR par_26     AS DECIMAL NO-UNDO.
DEF VAR par_29     AS DECIMAL NO-UNDO.
DEF VAR par_32     AS DECIMAL NO-UNDO.
DEF VAR par_33     AS DECIMAL NO-UNDO.
DEF VAR par_34     AS DECIMAL NO-UNDO.
DEF VAR par_35     AS DECIMAL NO-UNDO.
DEF VAR par_48     AS DECIMAL NO-UNDO.
DEF VAR par_82     AS DECIMAL NO-UNDO.
DEF VAR par_109    AS DECIMAL NO-UNDO.
DEF VAR par_173    AS DECIMAL NO-UNDO.
DEF VAR par_209    AS DECIMAL NO-UNDO.
DEF VAR par_210    AS DECIMAL NO-UNDO.
DEF VAR par_229    AS DECIMAL NO-UNDO.
DEF VAR par_233    AS DECIMAL NO-UNDO.
DEF VAR par_248    AS DECIMAL NO-UNDO.
DEF VAR par_301    AS DECIMAL NO-UNDO.
DEF VAR par_377    AS DECIMAL NO-UNDO.
DEF VAR par_509    AS DECIMAL NO-UNDO.
DEF VAR par_519    AS DECIMAL NO-UNDO. /* ���� �� ���।��⠢����� ��� */
DEF VAR par_526    AS DECIMAL NO-UNDO.
DEF VAR par_530    AS DECIMAL NO-UNDO.
DEF VAR par_531    AS DECIMAL NO-UNDO.
DEF VAR oper_83    AS DECIMAL NO-UNDO.
DEF VAR oper_283   AS DECIMAL NO-UNDO.
DEF VAR mCommFirst AS DECIMAL NO-UNDO.
DEF VAR mCommRKO   AS DECIMAL NO-UNDO.
DEF VAR xz         AS DECIMAL NO-UNDO.
DEF VAR rproc      AS CHAR    FORMAT "x(120)".
DEF VAR out_proc   AS CHAR    NO-UNDO.
DEF VAR mAmtStr    AS CHAR    NO-UNDO. /* ��ப� ��� �ய��*/
DEF VAR mDecStr    AS CHAR    NO-UNDO. /* ��ப� ��� �ய��*/
DEF VAR mOstCr     AS DECIMAL NO-UNDO. 
DEF VAR mCommRate  AS DECIMAL NO-UNDO. /* ���祭�� % �⠢�� */

DEF VAR chetFil AS CHAR NO-UNDO. /*"��� �����⥫�" ��� ������ �����*/
DEFINE VARIABLE podOtdel    AS CHAR  NO-UNDO. /* �⤥����� �����ᠭ� */
DEF VAR mSignsVal  AS CHAR    NO-UNDO.
 
DEF VAR mAmt-rub  AS DECIMAL NO-UNDO. /* ��� ������� ���᫥��� */

DEF NEW SHARED VAR rid_loan AS RECID. 

DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
DEF VAR mPointer AS CHARACTER NO-UNDO. /* ����� ������� ���ᯥ祭�� */

DEF BUFFER b_user FOR _user.     /* ���������� ����. */


DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   

find first _user where _user._userid = user_ no-lock no-error.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).

iBranch = GetXattrValueEx("_user",user_,"�⤥�����",?).
str_title = "[ �롥�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
mUser = "".

/*�맮� �����ᠭ⮢ zss*/
CASE iStr:
WHEN 'tr_dosr' THEN run signat.p ("sogl_otst","UserName",str_title,OUTPUT mUser).
WHEN 'otv_tar_pp' THEN run signat.p ("spotven","UserName",str_title,OUTPUT mUser).
WHEN 'soglotst2' THEN run signat.p ("sogl_otst","UserName",str_title,OUTPUT mUser).
OTHERWISE run signat.p ("precrdprvvv02","UserName",str_title,OUTPUT mUser).
END.




if mUser = ? then return.

  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "���������", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "����", "")).
  RUN Insert_TTName("Pod-user-fio-rp",  GetXAttrValueEx("_User",mUser,"User-NameRP","")).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "����᭒����", "")).
  RUN Insert_TTName("Pod-user-otdel-rp",GetXAttrValueEx("_User",mUser,"�⤥���","")).
  RUN Insert_TTName("Pod-user-dolg-rp", GetXAttrValueEx("_User",mUser,"��������쐏","")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "����䮭", "")).
  RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "�⤥���", "")).


  find first b_user where b_user._userid = mUser no-lock no-error.

/*zss �뢮� 䠬���� � �ଠ� �.�. �������*/
FUNCTION getShortSur RETURN CHAR (INPUT vUserName AS CHAR).

DEF VAR vSurname AS CHAR NO-UNDO.

IF NUM-ENTRIES(vUserName," ") > 2 THEN 
DO:
   vSurname = SUBSTRING(ENTRY(2,vUserName," "),1,1,"CHARACTER") + "." +
                SUBSTRING(ENTRY(3,vUserName," "),1,1,"CHARACTER") + ". " + 
                ENTRY(1,vUserName," ").
END.
ELSE vSurname = vUserName. 

RETURN vSurname.
END FUNCTION.

/*zss ��।������ ��� �����⥫� � ������ ����� ���ਬ��:�।��� - word ��� - �⢥�� �� ������ �� -��� �����⥫� */
  chetFil =  GetRefVal("��吠��", TODAY, shFilial + ",810,�,pechat,�"). /* ����祭�� ��� �� �ࠢ�筨�� "��吠��"*/
 /*-------------------*/
RUN Insert_TTName("Filial-chet-dohod", chetFil).

RUN Insert_TTName("Pod-user-fio3", getShortSur(GetXAttrValueEx("_User", mUser,"����",""))).


  RUN Insert_TTName("Pod-user-fio2", b_user._User-Name).


  RUN Insert_TTName("Pod-isp-dolg", GetXAttrValueEx("_User", User_, "���������", "")).
  RUN Insert_TTName("Pod-isp-fio", GetXAttrValueEx("_User", User_, "����", "")).
  RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "����᭒����", "")).
  RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "����䮭", "")).

  /*zss �� sign_select.i*/
  podOtdel  = GetXAttrValueEx("_User", mUser, "�⤥�����", "").
  IF podOtdel EQ "0518" THEN podOtdel = "0500".

/* ����祭�� �㬬� �ய���� ����� �� ���� ᯮᮡ��: ᪮��� ���砫� � ����; ᪮��� ���砫� � ��᫥ ������ */
PROCEDURE FrmtAmt:
   DEF INPUT  PARAM iAmt      AS DEC   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iKindAmt  AS INT64 NO-UNDO.
   DEF OUTPUT PARAM oStramt   AS CHAR  NO-UNDO.
   
   DEF VAR vAmtStr AS CHAR  NO-UNDO.
   DEF VAR vDecStr AS CHAR  NO-UNDO.   
   DEF VAR vCnt1   AS INT64 NO-UNDO.
   DEF VAR vCnt2   AS INT64 NO-UNDO.
   
   RUN x-amtstr.p (iAmt, iCurrency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
   IF iKindAmt EQ 2 THEN 
   DO:
      
      oStramt = TRIM(STRING(iAmt, ">>>>>>>>>.99")) + " (" + vAmtStr + " " + vDecStr.
      DO vCnt2 = 0 TO NUM-ENTRIES(oStramt," ") - 1:
         IF ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") NE "" THEN
            vCnt1 = vCnt1 + 1.
            IF vCnt1 EQ 4 THEN
            DO:
               ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") = ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") + ")".
               LEAVE.
            END.
      END.    
   END.
   ELSE
      oStramt = TRIM(STRING(iAmt, ">>>>>>>>>.99")) + " (" + vAmtStr + " " + vDecStr + ")". 
END PROCEDURE.

/* �᫨ ������� ����, � ᮧ���� �⬥⪨ */
IF     NUM-ENTRIES(iStr)   GE 2 
   AND ENTRY(2, iStr, "|") NE "" THEN
DO:
   {empty tmprecid}
   FIND FIRST loan WHERE
              loan.contract  EQ ENTRY(1, ENTRY(2, iStr, "|")) 
      AND     loan.cont-code EQ ENTRY(2, ENTRY(2, iStr, "|"))
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
      CREATE tmprecid.
      tmprecid.id = RECID(loan).
   END.
END.

   /* �� �⬥祭�� ������ࠬ */
FOR EACH tmprecid NO-LOCK: 
   /* �᫨ �� ������� ���ᯥ祭�� � � ��ப� ��ࠬ��஢ ��᫥���� ��ࠬ��� ࠢ�� mode2 -
   ०�� ���� ������஢ ���ᯥ祭��. */
   IF tmprecid.tablename MATCHES "term-obl"
   AND ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE2" THEN 
   DO:
      FIND FIRST term-obl
      WHERE
         RECID(term-obl) EQ tmprecid.id 
      NO-LOCK NO-ERROR.
      mPointer = "|<MODE2>," + STRING(tmprecid.id) + "|".
      FIND FIRST loan WHERE
         loan.contract   = term-obl.contract AND
         loan.cont-code  = term-obl.cont-code
         NO-LOCK NO-ERROR.
      ASSIGN
         rid-p    = RECID(loan)
         rid_loan = RECID(loan)
      .
      {norm-beg.i }
      OUTPUT STREAM fil TO NUL.

      /* ��ࠡ�⪠ ��楤�ன lgarterm, �ᯮ������ �� ���� � ��㧥� 
      ������஢ ���ᯥ祭�� ������ ��� ��᪮�쪨� ������஢ ���ᯥ祭�� */
      RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                       INPUT-OUTPUT TABLE ttnames).
      /* ���������� ⠡���� ����묨 ��� */
      RUN FillTables (loan.contract,
                      loan.cont-code).
      OUTPUT STREAM fil CLOSE.
      {norm-end.i &nofil=YES &nopreview=YES} 
         /* �뢮� ������ �� 蠡���� iStr (�� "|") � 䠩� ���� */
      RUN printvd.p (ENTRY(1, iStr, "|"),
                     INPUT TABLE ttnames).
      RUN Clear_TTName.
   END.
   /* �᫨ �� ������� ���ᯥ祭�� � � ��ப� ��ࠬ��஢ ��᫥���� ��ࠬ��� ࠢ�� mode3 -
   ०�� ���� ������� ���ᯥ祭�� � �����প�� ������⢥���� ���ᯥ祭��. */
   ELSE IF tmprecid.tablename MATCHES "term-obl"
   AND ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE3" THEN
   DO:
      FIND FIRST term-obl
      WHERE
         RECID(term-obl) EQ tmprecid.id 
      NO-LOCK NO-ERROR.
      mPointer = mPointer + STRING(tmprecid.id) + ",".
   END.
   ELSE DO:
   /* �� �⬥祭�� �।��� ������ࠬ */
      FOR EACH loan WHERE 
          RECID(loan) EQ tmprecid.id 
      NO-LOCK:
         ASSIGN
            rid-p    = RECID(loan)
            rid_loan = RECID(loan)
         . 
      
      /* ��� �뫠 �� ����窠 �� �।��� � ������� �� ������������� pda 30.07.2016 */ 
      mOstCr    = 0.
      mAcctType = "�।��,�।��%,�।��%��,�।��%�".

      DO mI = 1 TO NUM-ENTRIES(mAcctType):
         FIND LAST loan-acct OF loan WHERE 
            loan-acct.since <= TODAY 
            AND loan-acct.acct-type EQ ENTRY(mI, mAcctType)
         NO-LOCK NO-ERROR.
            IF AVAIL loan-acct AND mOstCr = 0 THEN DO:
               RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, TODAY, TODAY, ?).
               IF loan-acct.currency = '' THEN 
                  mOstCr = abs(sh-bal).
               ELSE mOstCr = abs(sh-val).
            END.
      END.
      IF mOstCr <> 0 THEN
         RUN Insert_TTName("dolg", 1).   /* �뫠 ����窠 � ���� ������襭��� ������������� */
      ELSE RUN Insert_TTName("dolg", 0). /* �뫠 ����窠 �� ������. ����襭� */

      
       /*
         {norm-beg.i }
       */
         FIND FIRST loan-cond WHERE
                   loan-cond.contract  EQ loan.contract
            AND    loan-cond.cont-code EQ loan.cont-code
            AND    loan-cond.since     LE gend-date
         NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            rid-t = RECID(loan-cond).
            /* ��ࠡ�⪠ ��楤�ࠬ� bankinfo,userinfo,dog,lgarterm � ��. */
         RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                          INPUT-OUTPUT TABLE ttnames).

            /* ���������� ⠡���� ����묨 ��� */
         RUN FillTables (loan.contract,
                         loan.cont-code).
      /* �ࠢ�� � ��㤭�� ���������� */
      IF end-date < loan.open-date THEN DO:
         MESSAGE "��� �� ����� ���� ����� ���� ������ �������!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
/* ४������ ������ �� */
      if loan.cust-cat = "�" then do:

         find first person where person.person-id = loan.cust-id.
         if avail person then do:
            RUN Insert_TTName("birthday", string(person.birthday,"99.99.9999")).
         end.
      end.
/* ����� ४����⮢ ������ �� */
/* ��।��塞 ४������ 䨫����, �� ���� 䨫���� �� ������� */

/*   FGetSettingMF (ipCode, ipSubCode, ipDefValue, ipFilial, ipNoCashe). */

FIND FIRST branch WHERE branch.Branch-Id EQ loan.filial-id NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("bank-name",    branch.name). 
   RUN Insert_TTName("bank-address", branch.Address). 
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
   RUN Insert_TTName("bank-name-rp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "����������")). 
END.

/*�� �⤥����� �����ᠭ�*/
FIND FIRST branch WHERE branch.Branch-Id EQ podOtdel NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("gorodOtd", entry(2,branch.Address)). 
   IF branch.branch-type EQ "23" AND branch.Address NE "" THEN
   DO:
      RUN Insert_TTName("otdel-name",    branch.name). 
      RUN Insert_TTName("otdel-address", "���� ��宦�����: " + branch.Address). 
   END.
END.

/* ����� ४����⮢ �� 䨫���� */


/* ��� � ���� - ��ࠬ���� �������. ��ਡ��� ������� ��⠢�塞 ���.*/



      /* ⠪ ��� ��� �� 㤠���� 㧭��� ��ࠬ���� ��� ������*/
      
      RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
                     loan.cont-code,      /* ����� �������. */
                     date(end-date),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
                     FALSE,      /* �������/�� ������� ������ �祭�� ������� */
                     TRUE
                    ).     /* �뢮����/ �� �뢮���� ��⮪�� �� �࠭ */
      


     
      /* ��筠� ������������� */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               0,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_0,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               2,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_2,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      /* �㬬� % �� �᭮���� ���� 4,29 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               4,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_4,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� �����. �᭮����� ����� */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               7,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_7,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� % �� �����. �᭮���� ���� 8,229 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               8,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_8,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */



      /* �㬬� ���� */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               9,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_9,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� �����. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               10,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_10,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               12,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_12,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               13,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_13,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               26,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_26,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� % �� �᭮���� ���� 4,29 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               29,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_29,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               32,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_32,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               33,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_33,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      /* ??? */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               34,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_34,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               35,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_35,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� �����. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               48,               /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_48,    /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               82,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_82,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               173,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_173,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               109,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_109,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               209,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_209,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */


      /* �㬬� �����. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               210,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_210,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* �㬬� % �� �����. �᭮���� ���� 8,229 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               229,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_229,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               233,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_233,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      /* �㬬� �����. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               248,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_248,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               509,                /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_509,     /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */


      /* ���� �� ���।��⠢����� ��� */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               519,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_519,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               526,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_526,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               530,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_530,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      RUN ALL_PARAM IN h_Loan ("�।��",         /* ��� ������� */
                               loan.cont-code,   /* ����� ������� */
                               531,              /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_531,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */

      /* ������ 83 */
      mAmt-rub = 0.
      oper_83  = 0.
      FOR EACH loan-int WHERE loan-int.cont-code EQ loan.cont-code
                          AND loan-int.contract EQ loan.contract
                          and (loan-int.id-d eq 29 and loan-int.id-k eq 30) 
                          and loan-int.mdate = end-date 
                          NO-LOCK.                                   
         mAmt-rub = mAmt-rub  + loan-int.Amt-rub.
      end.
      oper_83 =  mAmt-rub.


      /* ������ 283 */
      mAmt-rub = 0.
      oper_283  = 0.
      FOR EACH loan-int WHERE loan-int.cont-code EQ loan.cont-code
                          AND loan-int.contract EQ loan.contract
                          and (loan-int.id-d eq 229 and loan-int.id-k eq 30) 
                          and loan-int.mdate = end-date 
                          NO-LOCK.                                   
         mAmt-rub = mAmt-rub  + loan-int.Amt-rub.
      end.
      oper_283 =  mAmt-rub.


      RUN Insert_TTName("Loan_End_Date", STRING( loan.end-date, '99.99.9999' )).


      mSignsVal = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "��⠑���").
      RUN Insert_TTName("Loan_Beg_Date", STRING( date(mSignsVal), '99.99.9999' )).
      
      /* ������� �� �뤠�� */
      /*
      mCommFirst = GET_COMM ("%��",                     /* ��� �����ᨨ */
               ?,                                    /* RecId ���*/
               loan.currency,                        /* �����*/
               loan.contract + "," + loan.cont-code, /* ���*/
               0.00,                                 /* MIN ���⮪ */
               0,                                    /* ��ਮ� */
               end-date).                            /* ��� */
      */
      RUN ALL_PARAM IN h_Loan ("�।��",        /* ��� ������� */
                               loan.cont-code,  /* ����� ������� */
                               377,             /* ��� ��ࠬ��� */
                               end-date,
                               OUTPUT par_377,   /* �㬬� ��ࠬ��� */
                               OUTPUT a1,        /* ����� ��ࠬ��� */
                               OUTPUT a2
                              ).                 /* �㬬� ��ࠬ��� � �㡫�� */
      
      IF par_377 <> ? AND par_377 > 0 THEN
         DO:
            RUN FrmtAmt(par_377, loan.currency, 2, OUTPUT rproc).
            RUN Insert_TTName("firstprc", rproc ).
         END.
      
      IF par_377 = ? OR par_377 = 0 THEN
         RUN Insert_TTName("firstprc", 0 ).
      .  
      /* ������� ��� */
      /*
      mCommRKO = GET_COMM ("%���",                    /* ��� �����ᨨ */
               ?,                                    /* RecId ���*/
               loan.currency,                        /* �����*/
               loan.contract + "," + loan.cont-code, /* ���*/
               0.00,                                 /* MIN ���⮪ */
               0,                                    /* ��ਮ� */
               end-date).                            /* ��� */
      */
      RUN ALL_PARAM IN h_Loan ("�।��",  /* ��� ������� */
            loan.cont-code,             /* ����� ������� */
            301,           /* ��� ��ࠬ��� */
            end-date,
            OUTPUT par_301,            /* �㬬� ��ࠬ��� */
            OUTPUT a1,                  /* ����� ��ࠬ��� */
            OUTPUT a2).                 /* �㬬� ��ࠬ��� � �㡫�� */
            
      IF par_9 = ? OR par_9 = 0 THEN
         RUN Insert_TTName("peni", 0).
      
      IF  par_301 <> ? AND par_301 > 0 THEN
         DO:
            RUN FrmtAmt(par_301, loan.currency, 2, OUTPUT rproc).
            RUN Insert_TTName("rkoprc", rproc).
         END.
                          
      IF par_301 = ? OR par_301 = 0 THEN
         RUN Insert_TTName("rkoprc", 0).
            
      /**/
      RUN FrmtAmt(par_0 + par_4 + par_377 + par_33 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("remn_total_str", rproc ).
      RUN Insert_TTName("remn_total", par_0 + par_4 + par_377 + par_33).
      RUN Insert_TTName("remn_date", STRING( end-date, '99.99.9999' )).

      RUN Insert_TTName("remn_date_GoMonth", STRING(GoMonth(end-date,1), '99.99.9999' )). /*zss �ਡ������� ������ ����� � ��������� ���*/

      RUN Insert_TTName("remn_base", par_0).
      
      /* ���⮪ ������������ �㬬��� */
      RUN FrmtAmt(par_0 +  par_2 + par_4 + par_7 + par_8 + par_9  + par_10 +  par_12 + par_13 + par_29 + par_26 + par_32 + par_33 + par_34 + par_35 + 
                  par_48 + par_82 + par_109 + par_173 + par_209 + par_210 + par_229 + par_233 + par_248 + par_301 + par_377 + 
                  par_509 + par_519 + par_526 + par_531 - oper_83 - oper_283 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_total", rproc ).

      /* �㬬� % �� �᭮���� ���� 4,29,32,33,34,35 */
      RUN FrmtAmt(par_4 + par_29 + par_32 + par_33 + par_34 + par_35 ,loan.currency,2, OUTPUT rproc).
            RUN Insert_TTName("amt_proc_dolg", rproc ).

      /* �㬬� % �� �᭮���� ���� 4,29,32,33,35 - ������ 83 (29,30) (�⥭���) */

      RUN FrmtAmt(par_4 + par_29 + par_32 + par_33 + par_35 - oper_83 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_dolg_2_chislo", par_4 + par_29 + par_32 + par_33 + par_35 - oper_83 ).
      RUN Insert_TTName("amt_proc_dolg_2", rproc ).

      /* �㬬� ����祭��� ��業⮢ �� �।��� ___; 10+48+173+377 */
      RUN FrmtAmt(par_10 + par_48 + par_173 + par_377 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_proc", rproc ).
      
      /* �㬬� �����. �᭮����� ����� */
      RUN FrmtAmt(par_7 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_osn_dolg_chislo", STRING(par_7, ">>>>>>>>>.99")).
      RUN Insert_TTName("amt_osn_dolg", rproc ).

      /* �㬬� �����. % 10,210,48,248 */
      RUN FrmtAmt(par_10 + par_210 + par_48 + par_248  ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc", rproc ).

      /*��騩 ࠧ��� ����祭��� ���������� �� �।�⭮�� �������� zss*/
      RUN FrmtAmt(par_10 + par_210 + par_48 + par_248 + par_9  + par_12 + par_82 + par_26 + par_509 + par_531 + par_7 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_obh_chislo", par_10 + par_210 + par_48 + par_248 + par_9  + par_12 + par_82 + par_26 + par_509 + par_531 + par_7).
      RUN Insert_TTName("amt_prosr_obh", rproc).


      /*�㬬� ����� % �� ���짮����� �।�⮬ 10,210,48,248,29  zss */
   	  RUN FrmtAmt(par_10 + par_210 + par_48 + par_248 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_kredit_chislo", STRING(par_10 + par_210 + par_48 + par_248, ">>>>>>>>>.99")).
      RUN Insert_TTName("amt_proc_kredit", rproc ).

      /* �㬬� ���� */
      RUN FrmtAmt(par_9 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni", rproc ).

      /* �㬬� ���� �㬬� ���� (�⥭���). 9+12+82+26+509+531 zss*/
      RUN FrmtAmt(par_9  + par_12 + par_82 + par_26 + par_509 + par_531 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni_2_chislo",STRING( par_9  + par_12 + par_82 + par_26 + par_509 + par_531, ">>>>>>>>>.99")).
      RUN Insert_TTName("amt_peni_2", rproc ).

      /* ���� �� ���।��⠢����� ��� */
      RUN FrmtAmt(par_519 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("peni_PTS", rproc ).

      /* ���� �� ���।��⠢����� ��� 2 ^ 519+526 (�⥭���)*/
      RUN FrmtAmt(par_519 + par_526,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("peni_PTS_2", rproc ).

      /* �㬬� % �� �����. �᭮���� ���� 8,229,233 */
      RUN FrmtAmt(par_8 + par_229 + par_233 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_osn", rproc ).

      /* C㬬� ��業⮢ �� ����祭�� �᭮���� ���� ;8+210+233+229+248  - ������ 283 (229,30)  */
      RUN FrmtAmt(par_8 + par_210 + par_233 + par_229  + par_248 - oper_283 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_osn", rproc ).

      /* ������������� �� ���㤠��⢥���� ��諨�� 530*  (�⥭���)*/
      RUN FrmtAmt(par_530,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("dolg_gos_posh", rproc ).

      /* ������� �� ���  - 109+209+301 (�⥭���) */
      RUN FrmtAmt(par_109 + par_209 + par_301 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("rkoprc_2", rproc ).

      /**/
        FIND FIRST term-obl
        WHERE term-obl.cont-code = loan.cont-code
            and term-obl.contract = loan.contract
            and term-obl.idnt = 2
        NO-LOCK NO-ERROR.

        IF AVAILABLE term-obl THEN
        DO:
        
         RUN FrmtAmt(term-obl.amt,loan.currency,2, OUTPUT rproc).
        END.
        
      RUN Insert_TTName("limit", rproc). 
      /*------------------------------------------*/
      
      RUN FrmtAmt(par_0, loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_base_str", rproc).

      /* ���⮪ �᭮����� ����� 0+13+2   */

      RUN FrmtAmt(par_0 + par_13 + par_2 , loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_osn_dolg", rproc).


      /**/
      RUN Insert_TTName("remn_prc", par_4 + par_33).
      RUN FrmtAmt(par_4 + par_33, loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_prc_str", rproc).
      mCommRate = GET_COMM ("%����",                     /* ��� �����ᨨ */
               ?,                                    /* RecId ���*/
               loan.currency,                        /* �����*/
               loan.contract + "," + loan.cont-code, /* ���*/
               0.00,                                 /* MIN ���⮪ */
               0,                                    /* ��ਮ� */
               end-date).                            /* ��� */
               

      RUN Insert_TTName("CommPrc", STRING(mCommRate, '99.99')).
      RUN x-amtstr.p(mCommRate, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) + "楫�� ".
      RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      out_proc = out_proc + LC(STRING(mAmtStr)) + "���� ".
      
      RUN Insert_TTName("CommPrcStr", out_proc).
      
      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
          AND loan-cond.cont-code EQ loan.cont-code
      NO-LOCK NO-ERROR.
     
      xz = DEC(GetXAttrValueEx("loan-cond",loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since, "99/99/99"),
           "����⏫��", "0")).
         
      RUN Insert_TTName("AnnPl+", xz).   
      
      RUN FrmtAmt(xz,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("AnnPlStr", rproc).
      /* ------------------------------- */
     

         OUTPUT STREAM fil CLOSE.
         
         {norm-end.i &nofil=YES &nopreview=YES} 
            /* �뢮� ������ �� 蠡���� iStr (�� "|") � 䠩� ���� */
         
         RUN printvd.p (ENTRY(1, iStr, "|"),
                        INPUT TABLE ttnames).
                  
      END.
   END.
END.
/* 0204655 */
IF ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE3" THEN DO:
   mPointer = SUBSTR(mPointer,1,LENGTH(mPointer) - 1).
   mPointer = "|<MODE3>," + mPointer + "|".
   FIND FIRST loan WHERE
      loan.contract   = term-obl.contract AND
      loan.cont-code  = term-obl.cont-code
      NO-LOCK NO-ERROR.
   ASSIGN
      rid-p    = RECID(loan)
      rid_loan = RECID(loan)
   . 
   OUTPUT STREAM fil TO NUL.
   RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                    INPUT-OUTPUT TABLE ttnames).
    
      /* ���������� ⠡���� ����묨 ��� */
   RUN FillTables (loan.contract,
                   loan.cont-code).
   OUTPUT STREAM fil CLOSE.
      /* �뢮� ������ �� 蠡���� iStr (�� "|") � 䠩� ���� */
   RUN printvd.p (ENTRY(1, iStr, "|"),
                  INPUT TABLE ttnames).
END.
/* END of 0204655 */

   /* ���������� ⠡���� ����묨 ��� */
PROCEDURE FillTables:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
     
   RUN filleps.p (loan.contract, 
                  loan.cont-code, 
                  loan.since, 
                  OUTPUT TABLE ttReportTable).
  
   RUN Insert_TTName ("info", ""). 
   
   FIND FIRST ttNames WHERE
              ttnames.tname EQ 'info'
   NO-LOCK NO-ERROR.
   
   FOR EACH ttReportTable 
   BREAK BY ttReportTable.tf_payment-date:
      ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_payment-date)   + '\n'
                                      + STRING(ttReportTable.tf_sum-percent)    + '\n'
                                      + STRING(ttReportTable.tf_basic-sum-loan) + '\n'
                                      + STRING(ttReportTable.tf_actual-payment) + '\n' 
                                      + STRING(ttReportTable.tf_sum-percent     +
                                               ttReportTable.tf_basic-sum-loan  +
                                               ttReportTable.tf_actual-payment) + '\n'.
   END.

END PROCEDURE.

