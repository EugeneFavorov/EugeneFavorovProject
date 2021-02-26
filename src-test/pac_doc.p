/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"ENTRY(3
     Filename: pac_doc.p
      Comment: ����� ���㬥�⮢ �� ����⨥ ���.���
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 17/12/2010 kraa (0120008)
     Modified: 
*/
{globals.i}                                 /* �������� ��६����         */
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{intrface.get tmess}
{tmprecid.def}

&GLOBAL-DEFINE FILE_sword_p YES
{pp-uni.var}
&UNDEFINE FILE_sword_p
{pp-uni.prg}
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{prn-doc.def &with_proc=YES}

DEFINE VARIABLE mPName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPRKC   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPCAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPMFO   AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mSeparator AS CHARACTER NO-UNDO. 

&GLOBAL-DEFINE prreg "zayavlenie"
&GLOBAL-DEFINE ticlax TRUE
&GLOBAL-DEFINE FILE_sword_p TRUE
{parsin.def}
                                                                                      
DEFINE VARIABLE tmprecid 	  AS CHARACTER 			NO-UNDO.  
DEFINE VARIABLE mStatus 	  AS CHARACTER 			NO-UNDO.                                                     
DEFINE VARIABLE mcountr 	  AS CHARACTER 			NO-UNDO.
DEFINE VARIABLE mcountr1	  AS CHARACTER 			NO-UNDO.
DEFINE VARIABLE mMestReg      AS CHARACTER 			NO-UNDO. /*RegPlace*/
DEFINE VARIABLE mDatTMP  	  AS DATE	   			NO-UNDO. /*RegDate*/
DEFINE VARIABLE str   		  AS CHARACTER 			NO-UNDO. 
DEFINE VARIABLE mAdrReg       AS CHARACTER 			NO-UNDO. /*�����*/
DEFINE VARIABLE mAdrFact   	  AS CHARACTER 			NO-UNDO. /*�������*/
DEFINE VARIABLE mAdrFact40817 AS CHARACTER 			NO-UNDO. /*�������40817 ��� ������� ������᪮�� ���� 40817*/
DEFINE VARIABLE mDRuk   	  AS CHARACTER 			NO-UNDO. /*�����*/
DEFINE VARIABLE mNRuk         AS CHARACTER EXTENT 2 NO-UNDO. /*�����*/
DEFINE VARIABLE mIssue		  AS CHARACTER EXTENT 2 NO-UNDO. /*��ᯮ�� ��� �뤠�*/
DEFINE VARIABLE mDateBeg   	  AS DATE               NO-UNDO. /*???*/   
DEFINE VARIABLE mAcct		  AS CHARACTER EXTENT 2 NO-UNDO. /*acct*/
DEFINE VARIABLE mCID		  AS CHARACTER 			NO-UNDO. /*CID*/
DEFINE VARIABLE mCIDIP		  AS CHARACTER 			NO-UNDO. /*doc-num*/   /*doc-ref*/
DEFINE VARIABLE mInn		  AS CHARACTER 			NO-UNDO. /* � �*/
DEFINE VARIABLE mCustId		  AS INT64 				NO-UNDO. /* cust-id*/
DEFINE VARIABLE mName		  AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE Ofname		  AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE RetString     AS CHARACTER 			NO-UNDO.   
DEFINE VARIABLE mcur1    	  AS CHARACTER 			NO-UNDO.   
DEFINE VARIABLE mcur2    	  AS CHARACTER 			NO-UNDO. 
DEFINE VARIABLE mcur3    	  AS CHARACTER 			NO-UNDO. 
DEFINE VARIABLE curf    	  AS CHARACTER 			NO-UNDO. 
DEFINE VARIABLE mStr    	  AS CHARACTER 			NO-UNDO. 
DEFINE VARIABLE chk    		  AS INTEGER 			NO-UNDO. 
DEFINE VARIABLE mMausumbaeva AS LOGICAL NO-UNDO.
DEFINE VARIABLE mShaboldina AS LOGICAL NO-UNDO.

DEF VAR vDocTypes AS CHAR  NO-UNDO INIT "����抠��,��������,�६�஦,����,��������஦". 
DEF VAR vCnt      AS INT64 NO-UNDO.
DEF VAR vEndDate  AS CHAR  NO-UNDO. /* ���祭�� �� "end-date". */
DEF VAR vOpenDate  AS CHAR  NO-UNDO. 
DEF VAR vKodDoc  AS CHAR  NO-UNDO.
DEF VAR vNomDoc  AS CHAR  NO-UNDO.
DEF VAR vSerDoc  AS CHAR  NO-UNDO.
DEF VAR vPasp  AS CHAR  NO-UNDO.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"

DEF VAR mSignsVal		AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignVL			AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignsV			AS CHAR NO-UNDO. /* ���祭�� ४����� �� person */


DEF SHARED VAR tmprec_id	AS RECID.
DEF SHARED VAR rid-p		AS RECID.

{sign_select.i} /*�롮� �����ᠭ⮢*/

FIND FIRST tmprecid NO-LOCK NO-ERROR.

FIND FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK NO-ERROR.

	RUN Insert_TTName("acct",      STRING(acct.acct, "x(25)")).
	RUN Insert_TTName("acct1",     "'" + SUBSTRING(acct.acct,1,10)).
	RUN Insert_TTName("acct2", 	   "'" + SUBSTRING(acct.acct,11,10)).
	RUN Insert_TTName("open-date", REPLACE(STRING(acct.open-date, "99/99/9999"), "/", ".")).
	RUN Insert_TTName("openday",   string(day(acct.open-date), "99")).
	RUN Insert_TTName("openother", entry(month(acct.open-date),{&Months}) + " " + string(year(acct.open-date)) + " �.").
	RUN Insert_TTName("acctdetails",TRIM(acct.details)).

	mStr = substring(acct.acct,1,20).
	REPEAT chk = 1 to 20:
		RUN Insert_TTName("acctn" + string(chk), substring(mStr,chk,1)).
	END.

	mSignsVal = GetXAttrValueEx("acct",
							acct.acct + "," + acct.currency,
							"��������",?).
	
	mSignsVal = IF NUM-ENTRIES(mSignsVal) > 1 THEN ENTRY(2,mSignsVal) ELSE mSignsVal.
	
	IF LENGTH(mSignsVal) <= 10 THEN 
		RUN Insert_TTName("DogNum1", mSignsVal).
	ELSE DO:
		RUN Insert_TTName("DogNum1", SUBSTRING(mSignsVal,1,10)).
		RUN Insert_TTName("DogNum2", "'" + SUBSTRING(mSignsVal,11)).
	END.
	RUN Insert_TTName("DogNum", mSignsVal).

FIND FIRST branch WHERE acct.branch-id EQ branch.branch-id NO-LOCK NO-ERROR. 
	/*�롮� ��த� �� ���ࠧ�������*/
/*      
        IF CAN-DO("0401,0402,0403,0406,0407,0409,0101,0102,0103,0106,0107,0109", acct.branch-id) THEN
		RUN Insert_TTName("Gorod", ENTRY(2, branch.address)).    
	IF CAN-DO("0400,0410,0000,0110", acct.branch-id) THEN
		RUN Insert_TTName("Gorod", ENTRY(2, branch.address)).
	IF CAN-DO("0404,0405,0408,0104,0105,0108", acct.branch-id) THEN
*/
	IF AVAIL(branch) THEN
		RUN Insert_TTName("Gorod", ENTRY(2, branch.address)).
		
	mSignsVal = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"���",?).
	IF CAN-DO("0400,0410,0000,0110",mSignsVal) THEN
		RUN Insert_TTName("moscow","1").
	ELSE 
		RUN Insert_TTName("moscow","0").
	DO:
		FIND FIRST _user WHERE 
				   _user._userid = acct.user-id  NO-LOCK NO-ERROR.
		IF AVAILABLE _user THEN             
		DO:
			mStatus = GetXAttrValueEx("_user", STRING(acct.user-id), "���������", "").
			RUN Insert_TTName("���������", mStatus).
		END.
	END.
	
	IF ENTRY(1, iParms, "|") = "rekv_inf_v" THEN 
		RUN FillTables(acct.cust-id,acct.cust-cat,acct.open-date,YES).
		
	IF ENTRY(1, iParms, "|") = "rekv_inf_r" THEN 
		RUN FillTables(acct.cust-id,acct.cust-cat,acct.open-date,NO).

	IF acct.cust-cat EQ "�" THEN DO:
		
		RUN Insert_TTName("��", "1").
		FIND FIRST cust-corp WHERE
				   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
		IF NOT AVAIL cust-corp THEN RETURN.
		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"�����",?).   
		RUN Insert_TTName("�����", mSignsVal).
		/* ࠧ������� �� �, � � � */
		RUN Insert_TTName("�", ENTRY(1,mSignsVal,"")).
		RUN Insert_TTName("�", ENTRY(2,mSignsVal,"")).
		IF NUM-ENTRIES(mSignsVal,"") GE 3 THEN
			RUN Insert_TTName("�", SUBSTRING(mSignsVal,INDEX(mSignsVal,ENTRY(3,mSignsVal,"")))).

		/* ����祭�� ⥬���஢������ ���祭�� ���� ������ ����� ��� pda 28/07/16
		FIND LAST tmpsigns WHERE 
			  tmpsigns.surrogate EQ STRING(cust-corp.cust-id)
		      AND tmpsigns.code EQ "�����" NO-LOCK NO-ERROR.
		IF AVAIL tmpsigns THEN 
		DO:
		    RUN Insert_TTName("dayRKO", STRING(DAY(tmpsigns.since), "99")).
		    RUN Insert_TTName("otherdateRKO", ENTRY(MONTH(tmpsigns.since),{&Months}) + " " + STRING(YEAR(tmpsigns.since)) + " �.").
		END.
		*/
		
		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"�����",?).
		RUN Insert_TTName("�����", mSignsVal).

		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"CID",?).
		RUN Insert_TTName("CID", mSignsVal).

		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"�᭮��",
								?).
		RUN Insert_TTName("�᭮��", mSignsVal).

		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"���③���।", 
								"").
		RUN Insert_TTName("���③���।", mSignsVal).

		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"�࣑����।", 
								"").
		RUN Insert_TTName("�࣑����।", mSignsVal). 

		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"����㪐�",?).
		IF mSignsVal EQ ? THEN DO: 
			mSignsVal = GetXAttrValueEx("cust-corp",
									STRING(cust-corp.cust-id),
									"�����",?).
			IF mSignsVal MATCHES "*���ࠫ�� ��४��" THEN RUN Insert_TTName("����㪐�", "����ࠫ쭮�� ��४��").	
		END.
		RUN Insert_TTName("����㪐�", mSignsVal).

		mSignsVal = GetXAttrValueEx("cust-corp",
								STRING(cust-corp.cust-id),
								"����㪐�",?).
		IF mSignsVal EQ ? THEN DO: 
			mSignsVal = GetXAttrValueEx("cust-corp",
									STRING(cust-corp.cust-id),
									"�����",?).	
		END.
		RUN Insert_TTName("����㪐�", mSignsVal).

		DEFINE VARIABLE mNBuh1 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh3 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh2 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh4 AS CHARACTER   NO-UNDO.

		mNBuh1 = GetXAttrValueEx("cust-corp", 
							  STRING(cust-corp.cust-id), 
							  "䨮���", 
							  "").
		IF mNBuh1 ="" THEN
			mNBuh3="���,   ���������� �ࠢ�� ��ன ������, ���������".
		ELSE
			mNBuh3=mNBuh1. 
		RUN Insert_TTName("������", mNBuh3).                                

		mNBuh2 = GetXAttrValueEx("cust-corp", 
							  STRING(cust-corp.cust-id), 
							  "䨮���", 
							  "").
		IF mNBuh2 ="" THEN
			mNBuh4="���".
		ELSE
			mNBuh4=mNBuh2. 
		RUN Insert_TTName("�����", mNBuh4).                                

		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"����", 
								"").
		RUN Insert_TTName("����", mSignsVal). 
		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"���", 
								"").
		RUN Insert_TTName("���", mSignsVal). 
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"RegPlace", 
								"").
		IF mSignsVal NE "" THEN 
		RUN Insert_TTName("RegPlace", mSignsVal). 
		ELSE 
		DO:
			mSignsVal = GetXAttrValueEx("cust-corp", 
									STRING(cust-corp.cust-id), 
									"���③���।", 
									"").
			RUN Insert_TTName("RegPlace", mSignsVal).
		END.
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"�焮���", 
								"").
		IF NUM-ENTRIES(mSignsVal) GE 2 THEN
			RUN Insert_TTName("�焮���", ENTRY(1,mSignsVal) + " �" + ENTRY(2,mSignsVal)). 
		
		mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
								   STRING(cust-corp.cust-id), 
								   "��⠎���", 
								   "")) NO-ERROR.
		RUN Insert_TTName("��⠎���", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " �.").
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
								STRING(cust-corp.cust-id), 
								"�焮�", 
								"").
		IF NUM-ENTRIES(mSignsVal) GE 2 THEN
			RUN Insert_TTName("�焮�", ENTRY(1,mSignsVal) + " �" + ENTRY(2,mSignsVal)). 
		
		mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
								   STRING(cust-corp.cust-id), 
								   "�焮����", 
								   "")) NO-ERROR.
		RUN Insert_TTName("�焮����", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " �.").
		
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
		RUN Insert_TTName("���",cust-corp.inn).
		RUN GetCustName IN h_Base (acct.cust-cat,
								acct.cust-id,
								IF AVAILABLE acct THEN acct.acct ELSE "",
								OUTPUT mName[1],
								OUTPUT mName[2],
								INPUT-OUTPUT mInn).
		FIND FIRST code WHERE 
				   code.class EQ "����।�"
			   AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
		IF AVAIL code THEN
			mName[1]  = code.name.

		RUN Insert_TTName("CustName",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
		RUN Insert_TTName("stat",cust-corp.cust-stat).
		RUN Insert_TTName("NameOrg",cust-corp.name-corp).
		RUN Insert_TTName("okpo",cust-corp.okpo).
		RUN Insert_TTName("NameShort",cust-corp.name-short).

		RUN RetAdr.p(acct.cust-id,  "�", "�����", ?, OUTPUT mAdrReg).
		RUN Insert_TTName("�����",mAdrReg). 

		RUN RetAdr.p(acct.cust-id,  "�", "�������", ?, OUTPUT mAdrFact).
		RUN Insert_TTName("�������",mAdrFact). 
		
		RUN RetAdr.p(acct.cust-id,  "�", "�������", ?, OUTPUT mAdrFact).
		RUN Insert_TTName("�������",mAdrFact).

    /* �������� � ��業��� �� �ࠢ� �����⢫���� ���⥫쭮�� pda 25/04/2016 */
    FIND FIRST cust-ident WHERE
               (cust-ident.close-date EQ ? OR cust-ident.close-date >= TODAY)
               AND cust-ident.class-code EQ 'cust-lic'
               AND cust-ident.cust-cat EQ '�'
               AND cust-ident.cust-id EQ cust-corp.cust-id
               NO-LOCK NO-ERROR.

    IF AVAIL cust-ident THEN
    DO:
      RUN Insert_TTName("NumLic",cust-ident.cust-code).
      RUN Insert_TTName("OpenLic",REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".")).
      RUN Insert_TTName("IssueLic",cust-ident.issue).
      FIND FIRST code WHERE
     		         code.class EQ "�����愥��"
     		         AND code.code EQ cust-ident.cust-code-type
     		         NO-LOCK NO-ERROR.
      IF AVAIL code THEN 
      RUN Insert_TTName("NameLic",code.name).
    END.
    
    /* ��� �������, ����� ������� pda 25/04/2016 */
    FOR FIRST loan-acct WHERE
    		      loan-acct.acct EQ acct.acct NO-LOCK,
        FIRST loan WHERE 
        			loan.cont-code EQ loan-acct.cont-code
    				  AND loan.contract EQ '�����'
    				  AND loan.cust-cat EQ '�' NO-LOCK:
    IF AVAIL loan THEN
    RUN Insert_TTName("DepDogNum",STRING(delFilFromAcct(loan.cont-code))).

    FIND FIRST code WHERE
     		       code.class EQ "�������"
     		       AND code.code EQ loan.cont-type
     		       NO-LOCK NO-ERROR.
      IF AVAIL code THEN 
    RUN Insert_TTName("DepName",code.name).
    END.
    /*------*/

   /* RUN Insert_TTName("NameLic",cust-ident.cust-corp).*/
		/*
		ELSE
		DO:
		mAdrFact40817 = mAdrReg.
		RUN Insert_TTName("�������40817", mAdrFact40817).
		END.
		*/

		IF ENTRY(1, iParms, "|") = "sogl_opd" THEN DO:
			
			FOR EACH cust-role WHERE 
					 cust-role.file-name EQ "cust-corp"  
				 AND cust-role.surrogate EQ STRING(cust-corp.cust-id)
				 and (cust-role.class-code eq "�ࠢ�_��ன_������"
				 or  cust-role.class-code eq "�ࠢ�_��ࢮ�_������") 
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
					RUN Insert_TTName("���",mSignsV).
					
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
					RUN Insert_TTName("Document4Date_vid", mSignsV).         	  

					RUN Insert_TTName("mIssue",person.issue). 			
					RUN Insert_TTName("���",person.inn). 

					RUN Insert_TTName("DateRogd",REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".")). 
					RUN Insert_TTName("document-id",person.document-id). 				
					RUN Insert_TTName("document",person.document). 				
					
					mcountr1 = GetXAttrValueEx("person",
											STRING(person.person-id),
											"country-id2", 
											"").
					IF mcountr1 = "RUS" THEN 
						mcountr="��" .
					ELSE 
						mcountr = mcountr1.	
					RUN Insert_TTName("country-id2", mcountr).
					
					mSignsV = fGetDocIssue(person.person-id).
					IF NUM-ENTRIES(mSignsV) > 1 THEN DO:
						mSignsV = REPLACE(mSignsV,",",", ��� ���ࠧ������� ").
						SUBSTRING(mSignsV,LENGTH(mSignsV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
						mSignsV = REPLACE(mSignsV,"/",".") + " �.".
					END.
					ELSE DO:
						SUBSTRING(mSignsV,LENGTH(mSignsV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
						mSignsV = REPLACE(mSignsV,"/",".") + " �.".
					END.
					RUN Insert_TTName("DocKem", mSignsV).
				 
					RUN RetAdr.p(person.person-id,  "�", "����ய", ?, OUTPUT mAdrReg).
					RUN Insert_TTName("���������", mAdrReg).
				END.
			END.
		END.
	END.

	/*-------------------------------------------------------------------------------*/
	

	IF acct.cust-cat EQ "�" THEN DO:
	    
	       

		RUN Insert_TTName("��", "0").
		
		FIND FIRST person WHERE
				   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
		IF NOT AVAIL person THEN RETURN.
		mSignsV = GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "�����",?).   
		RUN Insert_TTName("�����", mSignsV).

		mSignsVal = GetXAttrValueEx("person", STRING(person.person-id), "��⠂�।",?).
		RUN Insert_TTName("regIP", REPLACE(mSignsVal,'/','.') + ' �.').
		
		RUN Insert_TTName("DateRogd",REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".")).
        
		mSignsV = GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "���③���।", 
							   "").
		RUN Insert_TTName("���③���।", mSignsV).
		
		mSignsV = TRIM(GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "������", 
							   "")).
		IF mSignsV EQ "" OR mSignsV EQ ? THEN
		   mSignsV = "�������㠫�� �।�ਭ���⥫�".
      RUN Insert_TTName("�����", mSignsV).
      RUN Insert_TTName("�����", mSignsV).
		
		mSignsV = GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "�࣑����।", 
							   "").
		RUN Insert_TTName("�࣑����।", mSignsV).

        /* mSignsV = GetXAttrValueEx("person", 
								STRING(person.person-id), 
								"��������।", 
    				 ""). */
		mSignsV = GetXAttrValueEx("person", 
    				 STRING(person.person-id),
    				 "���③���।", 
								"").
		RUN Insert_TTName("RegPlace", mSignsV).

        mSignsVal = GetXAttrValueEx("person", 
        							STRING(person.person-id), 
        							"okpo","").
		RUN Insert_TTName("okpo",mSignsVal).
		
        RUN RetAdr.p(acct.cust-id,  "�", "�������", ?, OUTPUT mAdrFact).
		RUN Insert_TTName("�������",mAdrFact).

		mSignsV = GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "CID",?).
		RUN Insert_TTName("CID", mSignsV).
		
		mcountr1 = GetXAttrValueEx("person",
								STRING(person.person-id),
								"country-id2", 
								"").
		IF mcountr1 = "RUS" THEN
			mcountr="��" .
		ELSE 
			mcountr = mcountr1.	
		RUN Insert_TTName("country-id2", mcountr).
		
		/*  mSignsV = GetXAttrValueEx("person",
									  STRING(person.person-id),
									  "country-id2", 
									  "").
		RUN Insert_TTName("country-id2", mSignsV).*/ 
		
		mNBuh1 = GetXAttrValueEx("person", 
							  STRING(person.person-id), 
							  "䨮���", 
							  "").

		IF mNBuh1 = "" THEN
			mNBuh3="���,   ���������� �ࠢ�� ��ன ������, ���������".
		ELSE
			mNBuh3=mNBuh1. 
		RUN Insert_TTName("������", mNBuh3).
		
		mNBuh2 = "" . /* GetXAttrValueEx("cust-corp", 
										STRING(cust-corp.cust-id), 
										"䨮���", 
										""). */
		IF mNBuh2 ="" THEN
			mNBuh4 = "���".
		ELSE
			mNBuh4 = mNBuh2. 
		RUN Insert_TTName("�����", mNBuh4).                                

		mSignsV = GetXAttrValueEx("person",
							   STRING(person.person-id),
							   "����", 
							   "").
		IF DEC(mSignsV) EQ 0 THEN mSignsV = "____________________".
		RUN Insert_TTName("����", mSignsV).
		
		mDatTMP = DATE(GetXAttrValueEx("person",
								   STRING(person.person-id),
								   "��⠎���", 
								   "")) NO-ERROR.
		RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", " ")).
		
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
			mSignsV = mSignsV + ',' + ENTRY(1, person.phone[2]).
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
		RUN Insert_TTName("Document4Date_vid",REPLACE(mSignsV, "/", ".")).         	  /*Paspdate*/     

		RUN Insert_TTName("mIssue",person.issue). 			/* ��� �뤠�*/
		RUN Insert_TTName("���",person.inn). 
		RUN Insert_TTName("DateRogd",REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".")).  
		/*RUN Insert_TTName("document-id",person.document-id). 		*/		/*Pasport  */
		RUN Insert_TTName("document",person.document). 				/*Pasport  */
		
		mSignsV = GetXAttrValueEx("person",
							  STRING(person.person-id),
							  "BirthPlace", 
							  "").
		RUN Insert_TTName("BirthPlace",mSignsV). 
		
		mSignsV = GetXAttrValueEx("person",
							  STRING(person.person-id),
							  "�᭮��", 
							  "").
		mDatTMP = DATE(GetXAttrValueEx("person",
								   STRING(person.person-id),
								   "��⠂�।", 
								   "")) NO-ERROR.
		IF INDEX(mSignsV,"��") = 0 THEN
			RUN Insert_TTName("�᭮��",", �������饣� �� �᭮����� " + mSignsV + " �� " + STRING(mDatTMP, "99.99.9999") + " �. "). 
		ELSE
			RUN Insert_TTName("�᭮��",", �������饣� �� �᭮����� " + mSignsV + " ").
		
		mSignsV = String( person.name-last + " " + person.first-names ).
		RUN Insert_TTName("���",mSignsV).
		/* ࠧ������� �� �, � � � */
		RUN Insert_TTName("�", ENTRY(1,mSignsV,"")).
		RUN Insert_TTName("�", ENTRY(2,mSignsV,"")).
		IF NUM-ENTRIES(mSignsV,"") GE 3 THEN
			RUN Insert_TTName("�", SUBSTRING(mSignsV,INDEX(mSignsV,ENTRY(3,mSignsV,"")))).
		
		IF person.gender THEN 
			RUN Insert_TTName("genM","X").
		ELSE 
			RUN Insert_TTName("genF","X").

		FIND FIRST country WHERE person.country-id = country.country-id
			NO-LOCK NO-ERROR.
		IF AVAIL country THEN 
			RUN Insert_TTName("country",country.country-name).
			
 /*������塞 �᫨ ��ᯮ��*/ 
  	IF (person.document-id eq "��ᯮ��") THEN   	
  	DO:  	   	   	
			RUN Insert_TTName("document-id","��ᯮ��").
			
			RUN PaspSerNom (person.document, OUTPUT vSerDoc, output vNomDoc).
						
			RUN Insert_TTName("paspser",if  vSerDoc NE "" THEN vSerDoc ELSE "-").
			RUN Insert_TTName("paspnum","'" + vNomDoc).		
		
			mSignsV = GetXAttrValueEx("person",
								  STRING(person.person-id),
								  "Document4Date_vid", 
								  "").
			RUN Insert_TTName("paspdate",REPLACE(mSignsV, "/", ".")).
		
			RUN Insert_TTName("paspwho", ENTRY(1,person.issue)).
			
			mSignsV = "".
			IF NUM-ENTRIES(person.issue) > 1 THEN
            mSignsV = ENTRY(2,person.issue).
            
          RUN Insert_TTName("paspkod",if  mSignsV NE "" THEN mSignsV ELSE "-").
			
			mSignsV = fGetDocIssue(person.person-id).
			IF NUM-ENTRIES(mSignsV) > 1 THEN DO:
				mSignsV = REPLACE(mSignsV,",",", ��� ���ࠧ������� ").
				SUBSTRING(mSignsV,LENGTH(mSignsV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				mSignsV = REPLACE(mSignsV,"/",".") + " �.".
			END.
			ELSE DO:
				SUBSTRING(mSignsV,LENGTH(mSignsV, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				mSignsV = REPLACE(mSignsV,"/",".") + " �.".
			END.
			RUN Insert_TTName("DocKem", mSignsV).
			
	END.
	ELSE 
	DO:       
	    /**�� ��㣮� ���㬥�� �஬� ��ᯮ��*/	    
	    RUN Insert_TTName("document-id",GetCodeName ("�������", person.document-id)).           
            
        RUN PaspSerNom (person.document, OUTPUT vSerDoc, output vNomDoc).           
                                        
        RUN Insert_TTName("paspser",if  vSerDoc NE "" THEN vSerDoc ELSE "-").
        RUN Insert_TTName("paspnum",vNomDoc).
        
         /*(NUM-ENTRIES(person.issue) > 1) and 
         
        IF (ENTRY(2,person.issue) EQ "") THEN
        RUN Insert_TTName("paspkod","123").           
        */
        RUN Insert_TTName("paspwho",ENTRY(1,person.issue)).
        
        mSignsV = "".
         IF NUM-ENTRIES(person.issue) > 1 THEN
         mSignsV = ENTRY(2,person.issue).
            
          RUN Insert_TTName("paspkod",if  mSignsV NE "" THEN mSignsV ELSE "-").
                
                
                    mSignsV = GetXAttrValueEx("person",
                               STRING(person.person-id),
                               "Document4Date_vid", 
                               "").
                
	    RUN Insert_TTName("paspdate",REPLACE(mSignsV, "/", ".")).
	                    
        IF (person.country-id NE "RUS") or GetXAttrValueEx("person",STRING(person.person-id),"country-id2","") NE "RUS"  THEN /*������塞 �᫨ ��१*/    
 DO:
         /** ���㬥�� �ࠢ� �ॡ뢠��� **/
            DO vCnt = 1 TO NUM-ENTRIES (vDocTypes):
               FIND LAST cust-ident WHERE
                         cust-ident.cust-cat        EQ "�"
                  AND    cust-ident.cust-id         EQ person.person-id
                  AND    cust-ident.cust-code-type  EQ ENTRY (vCnt, vDocTypes)
                  AND    cust-ident.close-date      EQ ?  
                  
               NO-LOCK NO-ERROR.
               
               IF AVAIL cust-ident THEN 
               DO:  
                                
                ASSIGN               
                
                     vEndDate =  GetXattrValue (
                                    "cust-ident",
                                    cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING (cust-ident.cust-type-num),
                                    "end-date"
                                 )           
                     vOpenDate = STRING (cust-ident.open-date, "99/99/9999") 
                     vKodDoc =  GetCodeName ("�������", cust-ident.cust-code-type)                                      
                     vPasp =  cust-ident.cust-code. 
                     
                 /*    end.*/
                     
        RUN PaspSerNom (vPasp, OUTPUT vSerDoc, output vNomDoc).                        
       
           
       
        RUN Insert_TTName("ndocdateo",REPLACE(vEndDate, "/", ".")).
        RUN Insert_TTName("ndocid",vKodDoc).
        RUN Insert_TTName("ndocdaten",REPLACE(vOpenDate, "/", ".")).
        RUN Insert_TTName("ndocnum", IF Index(vNomDoc,"0") NE 1 THEN vNomDoc ELSE chr(39) + vNomDoc).
        RUN Insert_TTName("ndocser",if (vSerDoc  NE "")  THEN vSerDoc ELSE "-").                         
		end.
		end.
		end.
    end.

		RUN Insert_TTName("phcel",ENTRY(2,person.phone[2])).
		RUN Insert_TTName("phhom",ENTRY(1,person.phone[1]) + IF ENTRY(1,person.phone[1]) = "" THEN "" ELSE ", " + ENTRY(2,person.phone[1])).

		RUN RetAdr.p(acct.cust-id,  "�", "����ய", ?, OUTPUT mAdrReg).
		RUN Insert_TTName("�����",mAdrReg). 

		RUN RetAdr.p(acct.cust-id,  "�", "�������", ?, OUTPUT mAdrFact).
		IF mAdrFact EQ "" THEN DO:
			mAdrFact40817 = mAdrReg.
			RUN Insert_TTName("�������40817", mAdrFact40817).
			mAdrFact = mAdrReg.
		END.
		RUN Insert_TTName("�������",mAdrFact). 
		
		RUN RetAdr.p(acct.cust-id,  "�", "�������", ?, OUTPUT mAdrReg).
		RUN Insert_TTName("�������",mAdrReg).
		
	END. 
	/*-----------------------------------------------------*/
	/*��� �⢥�. ���*/
	FIND FIRST branch WHERE branch.branch-id EQ acct.branch-id 
		NO-LOCK NO-ERROR.
	
	RUN Insert_TTName("mgr-title",branch.mgr-title).
	RUN Insert_TTName("mgr-name",branch.mgr-name).
	RUN Insert_TTName("name",branch.name).
	mSignVL = GetXAttrValueEx("branch",
					  	  STRING(branch.branch-id),
					  	  "����᭐㪏���", 
						  "").
	RUN Insert_TTName("����᭐㪏���", mSignVL).         	   

	mSignVL = GetXAttrValueEx("branch",
						  STRING(branch.branch-id),
						  "����㪏���", 
						  "").
	RUN Insert_TTName("����㪏���", mSignVL).    

	/*---------------------------------------------------------*/
	FIND FIRST _User WHERE _User._Userid EQ Acct.User-ID NO-LOCK NO-ERROR.
	if available _User then RUN Insert_TTName("NameOper", _User._User-Name).
	/* 
        do: 
	   RUN Insert_TTName("_User-Dolg", GetXAttrValueEx("_User",STRING(_User._Userid), "��������쐏","")).
	   RUN Insert_TTName("_User-Dov", GetXAttrValueEx("_User",STRING(_User._Userid), "����᭒����","")).
	   RUN Insert_TTName("User-NameRP", GetXAttrValueEx("_User",STRING(_User._Userid), "User-nameRP","")).
	end.
        */
	/*---------------------------------------------------------*/
/*
	find first _user where _Userid=userid("bisquit") no-lock no-error.
	if avail _user then do:
		RUN Insert_TTName("_User-Name",_User._User-Name).
		RUN Insert_TTName("User-NameRP", GetXAttrValueEx("_User",STRING(_User._Userid), "User-nameRP","")).
		RUN Insert_TTName("_User-DolgI", GetXAttrValueEx("_User",STRING(_User._Userid), "���������","")).
		RUN Insert_TTName("_User-Dolg", GetXAttrValueEx("_User",STRING(_User._Userid), "��������쐏","")).
		RUN Insert_TTName("_User-Dov", GetXAttrValueEx("_User",STRING(_User._Userid), "����᭒����","")).
		IF NUM-ENTRIES(_User._User-Name,".") EQ 1 THEN DO:
			mSignVL = ENTRY(1,_User._User-Name," ") + SUBSTRING(ENTRY(2,_User._User-Name," "),1,1)+ "." + SUBSTRING(ENTRY(3,_User._User-Name," "),1,1) + ".".
			RUN Insert_TTName("_User-NameT",mSignVL).
		END.
		ELSE 
			RUN Insert_TTName("_User-NameT",_User._User-Name).
	end.		
*/
	/*---------------------------------------------------------*/

	/* �饬 ������� */

	mSignsV=GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"��������", "").
	IF mSignsV NE "" THEN DO:
		RUN Insert_TTName("DateDog",ENTRY(1,mSignsV,",")).
		RUN Insert_TTName("NumDog",IF NUM-ENTRIES(mSignsV) > 1 THEN ENTRY(2,mSignsV) ELSE mSignsV).
	END.



	/*                      DEFINE VARIABLE vStr   AS CHARACTER NO-UNDO.
	DEFINE VARIABLE vResult  AS CHARACTER NO-UNDO.
	vStr = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"��������", "").
	IF {assigned vStr} THEN
	DO:
	vResult = ENTRY(1, vStr) NO-ERROR.
	IF {assigned vResult} THEN vListAcct = vListAcct + "��⠄������USD=qdt(" + REPLACE(vResult, "/", " ") + ")$$$~n".
	vResult = ?.
	vResult = ENTRY(2, vStr) NO-ERROR.
	IF {assigned vResult} THEN vListAcct = vListAcct + "������������USD=" + vResult + "$$$~n".
	END.

	*/
	


	/*��।������ ������*/

	FIND FIRST currency WHERE
			   currency.currency EQ acct.currency
		NO-LOCK NO-ERROR.
	RUN Insert_TTName("CurCode",currency.i-currency).
	CASE currency.currency:
		WHEN "" THEN DO:
			mcur="� �㡫�� ��" .
			RUN Insert_TTName("Cur",mcur). 
			curf="��ᨩ᪨� �㡫�".
			RUN Insert_TTName("curf",curf). 
			RUN Insert_TTName("vaccR","X"). 
		END.
		WHEN "840" THEN DO:
			mcur="� ������� ���".
			RUN Insert_TTName("Cur",mcur). 
			curf="������ ���".
			RUN Insert_TTName("curf",curf).
			RUN Insert_TTName("vaccD","X").		
		END.
		WHEN "978" THEN DO:	
			mcur="� ���".
			RUN Insert_TTName("Cur",mcur).
			curf="���".
			RUN Insert_TTName("curf",curf).
			RUN Insert_TTName("vaccE","X").
		END.
		WHEN "398" THEN DO:	
			curf="⥭��".
			RUN Insert_TTName("curf",curf).
		END.
	END.
	IF currency.currency = "" THEN 
		RUN Insert_TTName("inval","0").
	ELSE 
		RUN Insert_TTName("inval","1").
		
	DO: 
		IF  currency.currency="" THEN 
			mcur1="X" .
		RUN Insert_TTName("mcur1",mcur1). 
		IF  currency.currency="840" THEN  
			mcur2="X" .
		RUN Insert_TTName("mcur2",mcur2). 
		IF  currency.currency="978" THEN 
			mcur3="X" .
		RUN Insert_TTName("mcur3",mcur3). 
	END.

/* �뢮� ������ �� 蠡���� iParms (�� "|") � 䠩� ���� */
RUN printvd.p (ENTRY(1, iParms, "|"),
			  INPUT TABLE ttnames).   
{intrface.del comm}

PROCEDURE FillTables:
	DEF INPUT PARAM iCust-id  AS INT64 NO-UNDO.
	DEF INPUT PARAM iCust-cat AS CHAR  NO-UNDO.
	DEF INPUT PARAM iDate	   AS DATE  NO-UNDO.
	DEF INPUT PARAM iVal      AS LOG   NO-UNDO. /* YES - �����, NO - �㡫�*/
	
	DEF BUFFER bacct FOR acct.
	DEF VAR contract  AS CHAR.
	DEF VAR currency  AS CHAR.
	DEF VAR acct      AS CHAR.
	DEF VAR open-date AS CHAR.
	DEF VAR mDb       AS CHAR INIT "407*,40802*,40807*".
	
	RUN Insert_TTName ("graph", ""). 
   
	FIND FIRST ttNames WHERE
              ttnames.tname EQ 'graph'
		NO-LOCK NO-ERROR.
	
	IF iVal THEN DO:
		FOR EACH bacct WHERE bacct.cust-id  EQ iCust-id
		   AND bacct.cust-cat EQ iCust-cat
		   AND CAN-DO(mDb, bacct.acct)
		   AND (bacct.open-date GE iDate
		   OR bacct.close-date LE ?)
		   AND CAN-DO("�����", bacct.contract)
		   AND bacct.currency NE ""
		NO-LOCK:
			
			IF CAN-DO("�࠭�1",bacct.contract) THEN
				contract = "�࠭����".
			IF CAN-DO("�����",bacct.contract) THEN
				contract = "����騩".
			IF CAN-DO("�����",bacct.contract) THEN
				contract = "������".
					
			FIND FIRST currency WHERE
				   currency.currency EQ bacct.currency
			NO-LOCK NO-ERROR.
			
			currency = currency.name-currenc.
			IF acct NE bacct.number THEN
			DO:
			    acct = bacct.number.
			    open-date = STRING(bacct.open-date,"99.99.9999") + " �.".
			ttnames.tvalue = ttnames.tvalue + STRING(contract)                  + '\n'
											+ STRING(currency)                  + '\n'
											+ STRING(acct)                      + '\n'
											+ STRING(open-date)                 + '\n'
											.
			END.
		END.
	END.
	ELSE DO:
		FOR EACH bacct WHERE bacct.cust-id  EQ iCust-id
		    AND bacct.cust-cat EQ iCust-cat
		    AND CAN-DO(mDb, bacct.acct)
		    AND (bacct.open-date GE iDate
		    OR bacct.close-date LE ?)
		    AND CAN-DO("�����", bacct.contract)
		    AND bacct.currency EQ ""
		NO-LOCK:
		
		IF CAN-DO("�࠭�1",bacct.contract) THEN
			contract = "�࠭����".
		IF CAN-DO("�����",bacct.contract) THEN
			contract = "����騩".
		IF CAN-DO("�����",bacct.contract) THEN
			contract = "������".
				
		FIND FIRST currency WHERE
			   currency.currency EQ bacct.currency
		NO-LOCK NO-ERROR.
		
		currency = currency.name-currenc.
		    IF acct NE bacct.number THEN
		    DO:
			acct = bacct.number.
			open-date = STRING(bacct.open-date,"99.99.9999") + " �.".
		
			ttnames.tvalue = ttnames.tvalue + STRING(contract)                  + '\n'
                                        + STRING(currency)                  + '\n'
                                        + STRING(acct)                      + '\n'
                                        + STRING(open-date)                 + '\n'
                                        .
		    END.
		END.
	END.
	
END PROCEDURE.

procedure PaspSerNom:
DEFINE INPUT PARAMETER mSignsV AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER vSerDoc AS CHARACTER NO-UNDO INIT "".  
DEFINE OUTPUT PARAMETER vNomDoc AS CHARACTER NO-UNDO INIT "".

                 CASE NUM-ENTRIES(mSignsV," "):
      WHEN 1 THEN  vNomDoc = chr(39) + string(mSignsV).
      WHEN 2 THEN  do: vSerDoc = string(ENTRY(1,mSignsV," ")).
                       vNomDoc = string(ENTRY(2,mSignsV," ")).  
                   end.
      WHEN 3 THEN  do: vSerDoc = string(ENTRY(1,mSignsV," ")) +  " " +   string(ENTRY(2,mSignsV," ")) .
                       vNomDoc = string(ENTRY(3,mSignsV," ")).
                   end.
   END CASE. 
   
END PROCEDURE.
