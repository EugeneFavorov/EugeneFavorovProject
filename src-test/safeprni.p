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
DEFINE VARIABLE mDocVOPD     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE phone        AS CHARACTER          NO-UNDO INIT ''.
DEFINE VARIABLE mEmail       AS CHARACTER          NO-UNDO INIT ''.
DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn	     AS CHARACTER 			NO-UNDO. /* � �*/
DEFINE VARIABLE mAdrReg      AS CHARACTER 			NO-UNDO. /*�����*/
DEFINE VARIABLE mAdrFact     AS CHARACTER 			NO-UNDO. /*�������*/
DEFINE VARIABLE mAdrPoht     AS CHARACTER 			NO-UNDO. /*�������*/
DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDatTMP      AS DATE               NO-UNDO.
DEFINE VARIABLE mList_Acct   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE shfilial_    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE ost          AS decimal            NO-UNDO.
DEFINE VARIABLE mBegDate     AS date               NO-UNDO.   
DEFINE VARIABLE mEndDate     AS date               NO-UNDO.   
DEFINE VARIABLE i            AS int                NO-UNDO.   
DEFINE VARIABLE mSh-Db       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-Cr       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vDb      AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vCr      AS dec                NO-UNDO.   
DEFINE VARIABLE mList_cont   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mBalance     AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK1   AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK2   AS dec                NO-UNDO.   
DEFINE VARIABLE StrBalanceK1 AS char               NO-UNDO.   
DEFINE VARIABLE StrBalanceK2 AS char               NO-UNDO.   
DEFINE VARIABLE namestat     AS char               NO-UNDO.   
DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEFINE VARIABLE mSumma       AS dec                NO-UNDO.   
DEFINE VARIABLE mBlock       AS dec                NO-UNDO.   
DEFINE VARIABLE sBlock      AS char               NO-UNDO.   

DEFINE VARIABLE mNBuh1       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh3       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh2       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh4       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE str          AS char               NO-UNDO.   
DEFINE VARIABLE mSignsV      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignVL      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mcountr1     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mcountr      AS CHARACTER          NO-UNDO.


PAUSE 0.

DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   
iBranch = GetXattrValueEx("_user",user_,"�⤥�����",?).
str_title = "[ �롥�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
run signat.p ("safeprni","UserName",str_title,OUTPUT mUser).
  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "���������", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "����", "")).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "����᭒����", "")).
  RUN Insert_TTName("Pod-user-podr", GetXAttrValueEx("_User", mUser, "�⤥���", "")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "����䮭", "")).
  RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "�⤥���", "")).

/*��뢠�� ��� ��� ��।������ �㤥� ������ � Word ��� ���*/


RUN Insert_TTName("gend-date", term2str(gend-date, gend-date)).
/*������������ � ���� �����*/
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",    branch.name). 
   RUN Insert_TTName("go-bank-address", branch.Address). 
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)). 
END.

FIND FIRST branch WHERE branch.Branch-Id EQ shfilial NO-LOCK NO-ERROR.

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
   RUN Insert_TTName("bank-name-pp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "����������")). 
END.

/* 横� �� �樤�� ��࠭��� ������஢ */
FOR EACH tmprecid NO-LOCK, 
    FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK :
    RUN Insert_TTName("loan-num", loan.doc-ref).
    IF loan.cust-cat EQ "�" THEN
    DO:
  
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
   END.
/* ����� ����� �� �� */

/* ��砫� ����� �� �� */
   IF loan.cust-cat EQ "�" THEN
   DO:


      FIND FIRST cust-corp WHERE
      		   cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN RETURN.
   
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"�����",?).   
      RUN Insert_TTName("FIORuk", mSignsVal).
      /* ࠧ������� �� �, � � � */
      RUN Insert_TTName("Fam", ENTRY(1,mSignsVal,"")).
      RUN Insert_TTName("Nam", ENTRY(2,mSignsVal,"")).
      IF NUM-ENTRIES(mSignsVal,"") GE 3 THEN
      	RUN Insert_TTName("Fat", SUBSTRING(mSignsVal,INDEX(mSignsVal,ENTRY(3,mSignsVal,"")))).
      
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"�����",?).
      RUN Insert_TTName("DolRuk", mSignsVal).
      
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"CID",?).
      RUN Insert_TTName("CID", mSignsVal).
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"�᭮��",
      						?).
      RUN Insert_TTName("Osnova", mSignsVal).
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"���③���।", 
      						"").
      RUN Insert_TTName("MestSvedPred", mSignsVal).
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"�࣑����।", 
      						"").
      RUN Insert_TTName("OrgSvedPred", mSignsVal). 
      
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"����㪐�",?).
      IF mSignsVal EQ ? THEN DO: 
      	mSignsVal = GetXAttrValueEx("cust-corp",
      							STRING(cust-corp.cust-id),
      							"�����",?).
      	IF mSignsVal MATCHES "*���ࠫ�� ��४��" THEN RUN Insert_TTName("DolRukRP", "����ࠫ쭮�� ��४��").	
      END.
      RUN Insert_TTName("DolRukRP", mSignsVal).
      
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"����㪐�",?).
      IF mSignsVal EQ ? THEN DO: 
      	mSignsVal = GetXAttrValueEx("cust-corp",
      							STRING(cust-corp.cust-id),
      							"�����",?).	
      END.
      RUN Insert_TTName("FIORukRP", mSignsVal).
      
      
      mNBuh1 = GetXAttrValueEx("cust-corp", 
					  STRING(cust-corp.cust-id), 
					  "䨮���", 
					  "").
      IF mNBuh1 ="" THEN
      	mNBuh3="���,   ���������� �ࠢ�� ��ன ������, ���������".
      ELSE
      	mNBuh3=mNBuh1. 
      RUN Insert_TTName("FIOBuhg", mNBuh3).                                
      
      mNBuh2 = GetXAttrValueEx("cust-corp", 
      					  STRING(cust-corp.cust-id), 
      					  "䨮���", 
      					  "").
      IF mNBuh2 ="" THEN
      	mNBuh4="���".
      ELSE
      	mNBuh4=mNBuh2. 
      RUN Insert_TTName("FIObg", mNBuh4).                                
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
       						STRING(cust-corp.cust-id), 
       						"����", 
       						"").
      RUN Insert_TTName("OGRN", mSignsVal). 
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"���", 
						"").
      RUN Insert_TTName("KPP", mSignsVal). 
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"RegPlace", 
      						"").
      RUN Insert_TTName("RegPlace", mSignsVal). 
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"�焮���", 
      						"").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
      	RUN Insert_TTName("UhDokGr", ENTRY(1,mSignsVal) + " �" + ENTRY(2,mSignsVal)). 
      
      mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
      						   STRING(cust-corp.cust-id), 
      						   "��⠎���", 
      						   "")) NO-ERROR.
      RUN Insert_TTName("DataOGRN", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " �.").
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"�焮�", 
      						"").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
      	RUN Insert_TTName("UcDoc", ENTRY(1,mSignsVal) + " �" + ENTRY(2,mSignsVal)). 
      
      mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
      						   STRING(cust-corp.cust-id), 
      						   "�焮����", 
      						   "")) NO-ERROR.
      RUN Insert_TTName("UhDokData", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " �.").
      
      mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
						   STRING(cust-corp.cust-id), 
      						   "RegDate", 
      						   "")) NO-ERROR.
      RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).
      
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"tel", 
      						"").
      RUN Insert_TTName("tel", mSignsVal). 
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"fax", 
      						"").
      RUN Insert_TTName("fax", mSignsVal).                
      RUN Insert_TTName("INN",cust-corp.inn).
   
   
      RUN Insert_TTName("stat",cust-corp.cust-stat).

      find first code where code.parent = "����।�"
                        and code.val = cust-corp.cust-stat no-lock no-error.
      if avail code then do:
         namestat = code.name. 
         RUN Insert_TTName("namestat",namestat).
          
      end.
      RUN Insert_TTName("NameOrg",cust-corp.name-corp).
      RUN Insert_TTName("okpo",cust-corp.okpo).
      RUN Insert_TTName("NameShort",cust-corp.name-short).

      RUN RetAdr.p(cust-corp.cust-id,  "�", "�����", ?, OUTPUT mAdrReg).
      RUN Insert_TTName("AdrUr",mAdrReg). 
      RUN Insert_TTName("AdrReg",mAdrReg). 
      
      RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrFact",mAdrFact). 
      
      RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", ?, OUTPUT mAdrPoht).
      if mAdrPoht = ? or mAdrPoht = "" then mAdrPoht = mAdrFact.
      RUN Insert_TTName("AdrPoht",mAdrPoht).
      RUN Insert_TTName("tax-insp",cust-corp.tax-insp).
      mEmail = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"e-mail", 
      						"").
      RUN Insert_TTName("email", mSignsVal). 

      /* ��� �ਪ� */
      find last loan-acct where loan-acct.contract  = loan.contract
                            and loan-acct.cont-code = loan.cont-code
                            and loan-acct.acct-type = "�ऎ��"
                            no-lock no-error.
      if avail loan-acct then do: 
         RUN Insert_TTName("acct47422",entry(1,loan-acct.acct,"@")).
      end.


      if iStr = "sprn5wiu" then do: 
         FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"  
                              AND cust-role.surrogate EQ STRING(cust-corp.cust-id)
                              and cust-role.close-date EQ ?
/*                              and (cust-role.class-code eq "�ࠢ�_��ன_������"
                               or  cust-role.class-code eq "�ࠢ�_��ࢮ�_������") 
*/
                              and cust-role.cust-cat = "�"
         NO-LOCK:
            str = str + "," + cust-role.cust-name.
         END.
         str = SUBSTRING(str,2).
                   
         
         run messmenu.p
         	(9,
         	"[ ���� ]",
         	"��� ���� �ᯥ����?",
         	str).
         IF INT64(pick-value) NE 0 THEN DO:
            str = ENTRY(INT64(pick-value),str).
            
            FIND FIRST cust-role WHERE 
            		 cust-role.file-name EQ "cust-corp"  
            	 AND cust-role.surrogate EQ STRING(cust-corp.cust-id)
            	 AND cust-role.cust-name = str
            	 and cust-role.cust-cat = "�"
            NO-LOCK NO-ERROR.
            
            IF NOT AVAIL cust-role THEN DO:
            	{message &text="������ ��� ���!"}
            	RETURN.
            END.
            
            FIND FIRST person WHERE
            		   person.person-id EQ INT(cust-role.cust-id) NO-LOCK NO-ERROR.
            				
            IF AVAIL person THEN DO:
            	RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")). 
            
            	mSignsV = String( person.name-last + " " + person.first-names ).
            	RUN Insert_TTName("FIOOPD",mSignsV).
            	
            	mSignVL = GetXAttrValueEx("person",
            						   STRING(person.person-id),
            						   "cell-phone", 
            						   "").
            	IF {assigned mSignVL} THEN
            		mSignsV = mSignVL.
            	ELSE 
            		IF  {assigned person.phone[1]}
            		AND NUM-ENTRIES(person.phone[1]) GT 1 
            		AND {assigned ENTRY(2,person.phone[1])} THEN
            			mSignsV = ENTRY(2, person.phone[1]).
            		ELSE 
            		IF {assigned person.phone[1]} THEN
            			mSignsV = ENTRY(1, person.phone[1]).
            	IF {assigned person.phone[2]} THEN
            		mSignsV = mSignsV + ' ' + ENTRY(1, person.phone[2]).
            
            	RUN Insert_TTName("tel", mSignsV). 
            	mSignsV = GetXAttrValueEx("person",
            						  STRING(person.person-id),
            						  "fax", 
            						  "").
            	RUN Insert_TTName("fax", mSignsV).                
            	mSignsV = GetXAttrValueEx("person",
            						   STRING(person.person-id),
            						   "Document4Date_vid", 
            						   "").
            	RUN Insert_TTName("Document4Date_vid_OPD", mSignsV).         	  
            
            	RUN Insert_TTName("INNOPD",person.inn). 
            
            	RUN Insert_TTName("DateRogdOPD",REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".")). 
            	RUN Insert_TTName("document-id-OPD",person.document-id). 				
            	RUN Insert_TTName("document-OPD",person.document). 				
            	
            	mcountr1 = GetXAttrValueEx("person",
            							STRING(person.person-id),
            							"country-id2", 
            							"").
            	IF mcountr1 = "RUS" THEN 
            		mcountr="��" .
            	ELSE 
            		mcountr = mcountr1.	
            	RUN Insert_TTName("country-id2", mcountr).
            	
                mDocVOPD = fGetDocIssue(person.person-id).
                IF NUM-ENTRIES(mDocVOPD) > 1 THEN DO:
                  mDocVOPD = REPLACE(mDocVOPD,",",", �/�").
                  SUBSTRING(mDocVOPD,LENGTH(mDocVOPD, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
                  mDocVOPD = REPLACE(mDocVOPD,"�/�","�^�").
                  mDocVOPD = REPLACE(mDocVOPD,"/",".").
                  mDocVOPD = REPLACE(mDocVOPD,"�^�","�/�") + " �.".
                END.
                ELSE DO:
                  SUBSTRING(mDocVOPD,LENGTH(mDocVOPD, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
                  mDocVOPD = REPLACE(mDocVOPD,"�/�","�^�").
                  mDocVOPD = REPLACE(mDocVOPD,"/",".").
                  mDocVOPD = REPLACE(mDocVOPD,"�^�","�/�") + " �.".
                END.
                RUN Insert_TTName("mIssueOPD", mDocVOPD).
             
            	RUN RetAdr.p(person.person-id,  "�", "����ய", ?, OUTPUT mAdrReg).
            	RUN Insert_TTName("AdressOPD", mAdrReg).
            END.
         END.
      end.
   END.


/* ����� ����� �� �� */

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
   RUN Insert_TTName("today",  string(today)).
   RUN Insert_TTName("today-str",  term2str(today,today)).


   FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ "�ऎ��"
                                 AND  loan-acct.since    LE gend-date
      NO-LOCK NO-ERROR.

   IF AVAILABLE loan-acct THEN
      RUN Insert_TTName("cust-acct", ENTRY(1, loan-acct.acct, "@")).
   ELSE
      RUN Insert_TTName("cust-acct", "__________________").

   IF loan.currency EQ "" THEN
      mValName = " � �㡫��".
   ELSE
   DO:
      FIND FIRST currency WHERE currency.currency EQ loan.currency NO-LOCK NO-ERROR.

      IF AVAILABLE currency THEN
         mValName = " � ����� " + currency.name-currenc.
   END.

   FIND LAST term-obl OF loan WHERE term-obl.idnt EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE term-obl THEN
   DO:
      RUN x-amtstr.p (term-obl.amt-rub, loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
      RUN Insert_TTName("term-opl-sum",  STRING(term-obl.amt-rub, ">>,>>>,>>9.99") + " (" + REPLACE(vAmtStr,' ��',') ��') + " " + vDecStr).
      RUN Insert_TTName("term-opl",  "������६���� � ��⠢��� " + STRING(term-obl.amt-rub, ">>,>>>,>>9.99") + " (" + vAmtStr + " " + vDecStr + ")").
      RUN Insert_TTName("term-opl2", "�����ᠭ�� �������").
      RUN Insert_TTName("term-opl3", "� ������ �����ᠭ�� �������").
      RUN x-amtstr.p (term-obl.int-amt, loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).

      RUN Insert_TTName("term-opl-nds",  entry(1,STRING(term-obl.int-amt, ">>,>>>,>>9.99"),".") + " " + entry(num-entries(vAmtStr," "),vAmtStr," ") + " " + entry(1,vDecStr," ") + " ���.").
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

