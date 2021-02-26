/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
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

{branch.pro}

{intrface.get strng}
{intrface.get db2l}
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

&IF DEFINED(gdTplName) 
  &THEN DEFINE VARIABLE iParms AS CHARACTER INIT {&gdTplName} NO-UNDO.
  &ELSE DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.
&ENDIF

DEFINE VARIABLE mFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mSeparator AS CHARACTER NO-UNDO. 

&GLOBAL-DEFINE prreg "zayavlenie"
&GLOBAL-DEFINE ticlax TRUE
&GLOBAL-DEFINE FILE_sword_p TRUE
{parsin.def}
                                                                                      
DEFINE VARIABLE tmprecid         AS CHARACTER 		   NO-UNDO.  
DEFINE VARIABLE mStatus          AS CHARACTER 		   NO-UNDO.                                                     
DEFINE VARIABLE mcountr          AS CHARACTER 		   NO-UNDO.
DEFINE VARIABLE mcountr1         AS CHARACTER 		   NO-UNDO.
DEFINE VARIABLE mMestReg         AS CHARACTER 		   NO-UNDO. /*RegPlace*/
DEFINE VARIABLE mDatTMP          AS DATE               NO-UNDO.      /*RegDate*/
DEFINE VARIABLE str              AS CHARACTER		   NO-UNDO. /**/
DEFINE VARIABLE mAdrReg          AS CHARACTER		   NO-UNDO.  /*�����*/
DEFINE VARIABLE mAdrFact         AS CHARACTER		   NO-UNDO. /*�������*/
DEFINE VARIABLE mDRuk            AS CHARACTER		   NO-UNDO. /*�����*/
DEFINE VARIABLE mNRuk            AS CHARACTER EXTENT 2 NO-UNDO. /*�����*/
DEFINE VARIABLE mIssue           AS CHARACTER EXTENT 2 NO-UNDO. /*��ᯮ�� ��� �뤠�*/
DEFINE VARIABLE mDateBeg         AS DATE               NO-UNDO.   /*???*/   
DEFINE VARIABLE mAcct            AS CHARACTER EXTENT 2 NO-UNDO.   /*acct*/
DEFINE VARIABLE mCID             AS CHARACTER          NO-UNDO. /*CID*/
DEFINE VARIABLE mCIDIP           AS CHARACTER          NO-UNDO.   /*doc-num*/   /*doc-ref*/
DEFINE VARIABLE mInn             AS CHARACTER          NO-UNDO.     /* � �*/
DEFINE VARIABLE mCustId          AS INT64              NO-UNDO.      /* cust-id*/
DEFINE VARIABLE mName            AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE Ofname           AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE RetString        AS CHARACTER 		   NO-UNDO.   
DEFINE VARIABLE mTmp1            AS CHARACTER 		   NO-UNDO.   
DEFINE VARIABLE mTmp2            AS CHARACTER 		   NO-UNDO. 
DEFINE VARIABLE mTmp3            AS CHARACTER 		   NO-UNDO.
DEFINE VARIABLE mLog             AS LOGICAL 		   NO-UNDO. 
DEFINE VARIABLE InfSignsV        AS CHARACTER NO-UNDO.
DEFINE VARIABLE InfSignsV2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE InfSignsV3       AS CHARACTER NO-UNDO.

DEF VAR mSignsVal AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignVL   AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignsV   AS CHAR NO-UNDO. /* ���祭�� ४����� �� person */
DEF VAR mSignsV01 AS CHAR NO-UNDO. /* ���祭�� ४����� �� person */
DEF VAR mSignsV02 AS CHAR NO-UNDO. /* ���祭�� ४����� �� person */

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p      AS RECID.
DEFINE VARIABLE i         AS INT64              NO-UNDO. 
DEFINE VARIABLE j         AS INT64              NO-UNDO. 

   
DEF TEMP-TABLE ttOtchData NO-UNDO
FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* ⠡� � �����ᮬ �⮡ �� ������� � ���஢���*/


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

  
FUNCTION GetDateTemp RETURN CHAR (
   INPUT iN-FileName AS CHAR,
   INPUT iN-Surr     AS CHAR,   
   INPUT iN-Date     AS DATE,   
   INPUT iDate       AS DATE,
   INPUT iSense      AS LOG  
):
   DEF VAR vValue    AS CHAR   NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO. /* ���祭�� ��. */  
   
   EMPTY TEMP-TABLE ttOtchData.
   
    /*��� ���������� �����⮢ � ����*/
         
    IF iSense THEN
    DO:    
        FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
            AND tmpsigns.code = "��⠎��������" 
            AND tmpsigns.surrogate  = in-Surr          
            AND tmpsigns.since LE in-Date      
            AND tmpsigns.since GE date("27/12/2015")  /* � �� �뫮 �� ��� �� ������� **/                     
            NO-LOCK:    
             
            IF AVAILABLE tmpsigns THEN 
            DO: 
                FIND LAST ttOtchData USE-INDEX ttDate WHERE ttOtchData.ttDate = DATE(tmpsigns.code-value) NO-LOCK NO-ERROR .
                IF NOT AVAILABLE ttOtchData THEN       
                DO:            
                    CREATE ttOtchData.   
                    ASSIGN 
                        ttOtchData.ttDate = DATE(tmpsigns.code-value).
                END.
            END.
        END.
        in-Date = date("06/09/2016"). 
        iDate = date("27/12/2015").               
    END.
    
   /* FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "��⠏஢����")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
        AND tmpsigns.since GE iDate /*date("07/09/2016")   � �� �뫮 �� ��� �� ������� **/                     
        NO-LOCK:    
             
        IF AVAILABLE tmpsigns THEN 
        DO: 
            FIND LAST ttOtchData USE-INDEX ttDate WHERE ttOtchData.ttDate = DATE(tmpsigns.code-value) NO-LOCK NO-ERROR .
            IF NOT AVAILABLE ttOtchData THEN       
            DO:            
                CREATE ttOtchData.   
                ASSIGN 
                    ttOtchData.ttDate = DATE(tmpsigns.code-value).
            END.
        END.
    END.     */
    
     if in-Date GE date("30/06/2016") THEN     
    FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "��⠏஢����")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
        AND tmpsigns.since GE iDate /*date("07/09/2016")   � �� �뫮 �� ��� �� ������� **/                     
        NO-LOCK:    
             
        IF AVAILABLE tmpsigns THEN 
        DO: 
            FIND LAST ttOtchData USE-INDEX ttDate WHERE ttOtchData.ttDate = DATE(tmpsigns.code-value) NO-LOCK NO-ERROR .
            IF NOT AVAILABLE ttOtchData THEN       
            DO:            
                CREATE ttOtchData.   
                ASSIGN 
                    ttOtchData.ttDate = DATE(tmpsigns.code-value).
            END.
        END.
    END. 
    ELSE
    do:    
    FIND FIRST tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "��⠏஢����")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
         /*AND tmpsigns.since GE iDatedate("07/09/2016")   � �� �뫮 �� ��� �� ������� **/                     
        NO-LOCK NO-ERROR.            
        IF AVAILABLE tmpsigns THEN 
        DO:                
                CREATE ttOtchData.   
                ASSIGN 
                    ttOtchData.ttDate = DATE(tmpsigns.code-value).       
            END.
    end.     
         
  /*  CREATE ttOtchData.   
   ASSIGN ttOtchData.ttDate = cust-corp.date-in.  
     MESSAGE cust-corp.date-in VIEW-AS ALERT-BOX . */
    
        FOR EACH ttOtchData  NO-LOCK: 
            vResult =   vResult +  " "  + REPLACE(STRING(ttOtchData.ttDate,"99/99/9999"), "/", ".").
    END.

    RETURN trim(vResult).
    
{empty ttOtchData} 
END FUNCTION.   

/* �롮� �����ᠭ⮢ */
/* IF CAN-DO("zay_dkbo*",iParms) THEN 
DO: */
    {sign_select.i} 
/* END. */

IF CAN-DO('ank*', iParms) THEN
DO:
    {getdate.i &DateLabel = "��� ����"  
               &DateHelp  = "������ ���� ���� ������"  }     
END. 

FIND FIRST tmprecid NO-LOCK NO-ERROR.
		      
FIND FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK NO-ERROR.
IF AVAIL cust-corp THEN 
    RUN Insert_TTName("��", "1").
    RUN Insert_TTName("DogNum", STRING(cust-corp.cust-id)).

RUN BeginCircle_TTName("acct").
FOR EACH acct 
   WHERE acct.cust-id EQ cust-corp.cust-id
     AND acct.cust-cat EQ "�"
     AND CAN-DO("�����,�����*",acct.contract)
     AND acct.close-date EQ ?  
NO-LOCK BY acct.currency:
    IF AVAIL acct THEN
    DO:
        /* MESSAGE acct.acct VIEW-AS ALERT-BOX. */
        str = "".
        mTmp1 = "".
        IF acct.currency EQ "" 
            THEN str = "������� ������᪮�� ��� � ����� �� �� ".
        ELSE str = "������� ������᪮�� ��� � �����࠭��� ����� �� ".
        /*��� ������� �� ����⨨*/
        mTmp1 = REPLACE(ENTRY(1,GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"��������","___.___._____")),"/",".").
        
        str = str + mTmp1 + " �.(����� ��� � " + DelFilFromAcct(acct.acct) + ");".
        RUN Insert_TTName("strtable[acct]",str).
    
        RUN NextCircle_TTName("acct").
    END. 
END. /*for each acct*/
RUN EndCircle_TTName("acct").


        FIND FIRST acct WHERE  cust-corp.cust-id EQ acct.cust-id
                        AND    acct.cust-cat EQ '�' 
            NO-LOCK NO-ERROR. 
            
            IF AVAIL acct THEN DO:
				RUN Insert_TTName("acct",   STRING(acct.acct, "x(25)")).								                               
                
				/*�롮� ��த� �� ���ࠧ�������*/
				IF CAN-DO("0401,0402,0403,0406,0407,0409", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","�.�����-������").    
				IF CAN-DO("0400,0410", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","�.��᪢�").
				IF CAN-DO("0404,0405,0408", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","�.��᭮���").
				mSignsV = STRING(acct.open-date, "99.99.9999") + " �.".
			END.   
			else               
				 mSignsV = "���".	
			RUN Insert_TTName("Startdate",mSignsV).
					
		/*ayv ������� ���� ��� ��� ������*/
		FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
					  AND   acct.cust-cat EQ '�'
		NO-LOCK BY acct.open-date:
            IF  acct.contract   EQ "�����" 
            AND acct.close-date EQ ? 
            AND acct.filial-id  EQ shFilial THEN
            DO:
                str = str + "," + acct.number.
            END.
            /* LEAVE. */
            mSignsVal = {strdate.i acct.open-date}.
          
        END.
        
        IF  NOT AVAIL acct THEN      
            mSignsVal = {strdate.i cust-corp.date-in}.  
             
        RUN Insert_TTName("��⠇����������",REPLACE(mSignsVal,'�.','����')).   

        str = TRIM(str, ",").
        IF str = "" THEN 
            str = "-".
        IF NUM-ENTRIES(str) > 1 THEN 
            str = "".
        RUN Insert_TTName ("AccRasch",str).
        RUN Insert_TTName ("AccRaschXL", "�" + str).   

	/*	IF AVAIL acct THEN DO:
            mSignsVal = {strdate.i acct.open-date}.
			RUN Insert_TTName("Startdate",STRING(acct.open-date, "99.99.9999") + " �.").
		END.
		ELSE
			mSignsVal = {strdate.i cust-corp.date-in}.	
		RUN Insert_TTName("��⠇����������", IF  mSignsVal="" THEN "���" ELSE REPLACE(mSignsVal,'�.','����')).
		
				   FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
                      AND   acct.cust-cat EQ '�'
            NO-LOCK BY acct.open-date:
            LEAVE.
        END.*/
               		
		RUN Insert_TTName("Enddate", "���").
						
		mSignsV = REPLACE(STRING(cust-corp.date-in, "99/99/9999"), "/", ".").

        RUN Insert_TTName("DatePrint", mSignsV + " ����").   

        RUN Insert_TTName("open-date", mSignsV).
	
		mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id), end-date,date("27/12/2015"),Yes ), " ", ",").
		mSignsV = IF (mSignsV eq "") or (mSignsV eq ?) THEN STRING(cust-corp.date-in, "99/99/9999") ELSE  mSignsV.
		mSignsV = REPLACE(mSignsV, "/", ".") + " ����".
        RUN Insert_TTName("��⠎��������", mSignsV).
		
		mSignsV=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��᪎��","").
       /* IF mSignsV="" THEN DO:
            RUN Insert_TTName("RiskDa","�").
            RUN Insert_TTName("RiskNet", "").
        END.
        ELSE */
    IF CAN-DO("*��᮪��*,*����让*", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��᪎��","")) THEN 	
        do:           
            RUN Insert_TTName("RiskDa", "�" ).
            RUN Insert_TTName("�業����᪠",GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠","")).
        end.
            
    else    RUN Insert_TTName("RiskDa", "").     
    
    IF CAN-DO("������", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��᪎��","")) or (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��᪎��",""))="" THEN
        do: 
            RUN Insert_TTName("RiskNet",  "�").
            RUN Insert_TTName("�業����᪠","���������� �ਧ���� ��������樨").           
        end.    
     ELSE RUN Insert_TTName("RiskNet",  ""). 
				
		      
      /*  RUN Insert_TTName("�業����᪠", IF GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠","")="" THEN "" ELSE
        GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠","")).
			*/
			
			
		mSignsV = if GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����","") = "" THEN GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����-�����","") ELSE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����","") .	
	    RUN Insert_TTName("OKATO", IF  mSignsV="" THEN "���" ELSE mSignsV).


        mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id),end-date,date("07/09/2016"),No), " ", ",").
        /*   mSignsV = REPLACE(GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��⠏஢����",end-date,""),"/",".").  */
                                  
        RUN Insert_TTName("DP", IF mSignsV = "" THEN "���" ELSE mSignsV).
          
/*
	   RUN Insert_TTName("DP", IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "date_inspect",""))="" THEN "���" ELSE          
          (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "date_inspect",""))). */
          
        RUN Insert_TTName("RP", IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))="" THEN "�� ����⥭" ELSE          
        (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))).


        mSignsVal = GetXAttrValueEx("cust-corp",
									STRING(cust-corp.cust-id),
									"�����",?).   
        RUN Insert_TTName("�����", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).


        mSignsVal = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"dkbo-nomer",?).

        if mSignsVal <> "" then do:
           mSignVL = entry(1,mSignsVal," "). 
           mSignVL = REPLACE(mSignVL,",","").
           mSignVL = REPLACE(mSignVL,".","").
           mSignVL = trim(mSignVL).
           if num-entries(mSignsVal," ") > 1 then do:
              mSignsV = substr(mSignsVal,index(mSignsVal," ")).
              i = length(mSignsV).
              mSignsV01 = "".
              do j = 1 to i :
                 if index("1234567890",substr(mSignsV,j,1)) > 0 then do: 
                    mSignsV01 = mSignsV01 + substr(mSignsV,j,1).
                 end.
              end.
              mSignsV02 = substr(mSignsV01,1,2) + " " +  ENTRY(int(substr(mSignsV01,3,2)),{&Months}) +  " " + STRING(substr(mSignsV01,5)) + "�.".
           end. 
           mSignsVal = mSignVL + "    " + mSignsV02.
        end.
        else do:
           mSignsVal = "�__________________      '______' ___________20__�.".
        end.
        RUN Insert_TTName("dkbo-nomer", mSignsVal).


        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "�����",?).
        RUN Insert_TTName("�����", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).
        
        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "CID",?).
        RUN Insert_TTName("CID", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).

        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "�᭮��",
                                    ?).
        RUN Insert_TTName("�᭮��", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).
        
        mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "���③���।", 
                                    "").
        RUN Insert_TTName("���③���।", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).

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
 
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�࣑����।", "").
        RUN Insert_TTName("�࣑����।",  mSignsVal). 
      
              
               
        mSignsVal = "".
        FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   ((cust-role.class-code EQ '��।�⥫�') or (cust-role.class-code EQ '��樮���'))
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
                       
             NO-LOCK:
            {additem.i mSignsVal cust-role.cust-name}     
       end.         
        
       IF  mSignsVal eq "" THEN        
              mSignsVal = if GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��।��",end-date, "") NE "" THEN GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��।��",end-date, "") else "���".
         RUN Insert_TTName("��।��", mSignsVal).
                       
      /*  mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��।��", "").
        RUN Insert_TTName("��।��",  mSignsVal).         
*/
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��⠢���", "").
        If mSignsVal eq "" THEN  
        mSignsVal =  GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��⠢��������",end-date, "").
                       
        RUN Insert_TTName("��⠢���",  mSignsVal).
                                
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "groupOABS", "").
        RUN Insert_TTName("groupOABS", IF shFilial = "0500" THEN mSignsVal ELSE "").
        
        RUN Insert_TTName("CIDbis", STRING(cust-corp.cust-id)).
                
		DEFINE VARIABLE mNBuh1 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh3 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh2 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh4 AS CHARACTER   NO-UNDO.
						mNBuh1 = GetXAttrValueEx("cust-corp", 
									 STRING(cust-corp.cust-id), 
									 "䨮���", 
									 "").
			IF  mNBuh1 ="" THEN
                mNBuh3="���,   ���������� �ࠢ�� ��ன ������, ���������".
			ELSE
                mNBuh3=mNBuh1. 
            RUN Insert_TTName("������", mNBuh3).                                
                
            mNBuh2 = GetXAttrValueEx("cust-corp", 
                             		 STRING(cust-corp.cust-id), 
                             		 "䨮���", 
                             		 "").
			IF  mNBuh2 ="" THEN
                mNBuh4="���".
			ELSE
                mNBuh4=mNBuh2. 
            RUN Insert_TTName("�����", mNBuh4).                                
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
						 			STRING(cust-corp.cust-id), 
						 			"����", 
						 			"").
		                                 
       RUN Insert_TTName("����", IF mSignsVal = "" THEN "���" ELSE mSignsVal).
		
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
						 			STRING(cust-corp.cust-id), 
						 			"���", 
						 			"").
		RUN Insert_TTName("���", mSignsVal). 
		
		mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
								       STRING(cust-corp.cust-id), 
								       "RegDate", 
								       "")) NO-ERROR.
		RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).
		
		mSignVL = GetXAttrValueEx("cust-corp", 
						          STRING(cust-corp.cust-id), 
						          "tel", 
						          "").
		mSignsVal = GetXAttrValueEx("cust-corp", 
						            STRING(cust-corp.cust-id), 
                                    "fax", 
                                    "").
		IF mSignVL EQ mSignsVal THEN
			RUN Insert_TTName("Phone", mSignVL).
			ELSE DO:
				RUN Insert_TTName("Phone", mSignVL).
				RUN Insert_TTName("fax", mSignsVal).
		    END.
		    	
		mSignsVal = ''. 	 
        mSignsVal = trim(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "e-mail","")) .    
                
        If NOT CAN-DO("infank_ul*", iParms)  THEN DO:
        
            IF  mSignsVal ne "" THEN mSignsVal = mSignsVal + "; ".   
          
        RUN RetAdr.p(cust-corp.cust-id,  "�", "�������",end-date ,OUTPUT mAdrReg).
        mSignsVal = mSignsVal + mAdrReg.                              
                        END.
        
        /*RUN Insert_TTName("e-mail", mSignsVal).*/
        RUN Insert_TTName("e-mail", IF mSignsVal = "" THEN "���" ELSE mSignsVal).

	    RUN Insert_TTName("UstavCap", "���").
		RUN Insert_TTName("StateTax", "���").
		RUN Insert_TTName("���",cust-corp.inn).
		/*ayv �࠭� �஢�ઠ �� ����*/
		/*RUN GetCustName IN h_Base (acct.cust-cat,
				   acct.cust-id,
				   IF AVAILABLE acct THEN acct.acct ELSE "",
				   OUTPUT mName[1],
				   OUTPUT mName[2],
				   INPUT-OUTPUT mInn).*/
		mName[1] = cust-corp.cust-stat.
		mName[2] = cust-corp.name-corp.

	    FIND FIRST code WHERE 
				  code.class EQ "����।�"
			  AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
 	    IF AVAIL code THEN
			mName[1]  = code.name.
		    RUN Insert_TTName("stat1",mName[1]).

        RUN Insert_TTName("CustName",  TRIM(mName[1]) + " " + TRIM(mName[2])). 

		RUN Insert_TTName("stat",cust-corp.cust-stat).
		mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "engl-name", 
                                    "").
        RUN Insert_TTName("NameEngl", IF mSignsVal NE '' THEN ', ' + mSignsVal ELSE mSignsVal).
        RUN Insert_TTName("NameOrg",cust-corp.name-corp).
        RUN Insert_TTName("okpo",IF cust-corp.okpo NE '' THEN cust-corp.okpo ELSE '���').
        RUN Insert_TTName("NameShort", IF cust-corp.name-short NE '' THEN ', ' + cust-corp.name-short ELSE cust-corp.name-short).
        RUN Insert_TTName("NYulShort", IF cust-corp.name-short NE '' THEN cust-corp.name-short ELSE TRIM(mName[1]) + ' ' + TRIM(mName[2])).
             
		mSignVL=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"�����룄����","").
		IF mSignVL="" THEN DO:
				RUN Insert_TTName("VygDr","").
				END.
				ELSE DO:
				RUN Insert_TTName("VygDr", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"�����룄����","")).
		END.
		
		/*mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�࣓�ࠢ", "").
        RUN Insert_TTName("OrgUpr", IF mSignsVal = "" THEN "���" ELSE  mSignsVal).*/ 
		
	    RUN Insert_TTName("Bf_Da",  IF CAPS(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))= CAPS("��") THEN "�" ELSE "").
        RUN Insert_TTName("Bf_Net", IF CAPS(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))= CAPS("���") THEN "�" ELSE "").

		RUN RetAdr.p(cust-corp.cust-id,  "�", "�����", ?,OUTPUT mAdrReg).
        RUN Insert_TTName("�����",mAdrReg). 

      /*  RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", ?,OUTPUT mAdrReg).
	    RUN Insert_TTName("�������",mAdrReg). */
		
		RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", ?,OUTPUT mAdrFact).
	    RUN Insert_TTName("�������", IF mAdrFact NE "" THEN mAdrFact ELSE mAdrReg). 
		
		
		mSignsVal = "".		
		  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))	ne "" then
		  do:							   
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="��") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", "")) begins "�����")  THEN mSignsVal = "�".
		RUN Insert_TTName("OrgUpr_Da", mSignsVal).
		
		mSignsVal = "".		
		IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="���") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", "")) begins "�����") THEN mSignsVal = "�".
		RUN Insert_TTName("OrgUpr_Net", mSignsVal).
		end.
		else
		do:
		   IF mAdrReg eq mAdrFact then
            RUN Insert_TTName("OrgUpr_Da", "�").
           else
            RUN Insert_TTName("OrgUpr_Net", "�").
		 end.
		 
		RUN Insert_TTName("OrgUpr",GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�࣓�ࠢ", "")).
				  
		
	/* �饬 �����樠஢ */
	/*ayv ������� ���� �����樠஢*/
	mSignsVal = ''.
	mLog = TRUE.
    i = 1.



IF iParms EQ "vypcardUL" THEN 
    do:
        end-date = today.
        mSignsV ='*�४��*'. 
    end.    
ELSE mSignsV = '������_����'.

 	FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
					   AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                      /* AND   cust-role.class-code EQ '������_����'*/
                       AND   can-do(mSignsV, cust-role.class-code) 					 
					   AND   ((cust-role.close-date EQ ?) or (cust-role.close-date GE end-date))
					   AND   cust-role.open-date LE end-date
			NO-LOCK:

		{additem.i mSignsVal cust-role.cust-name}		       
	           
       FIND FIRST person WHERE
                  cust-role.cust-id EQ string(person.person-id)
            NO-LOCK NO-ERROR.
            
            mSignsV = GetXAttrValueEx("person",
                                    STRING(person.person-id),
                                    "�᭮��",
                                    ?).
            RUN Insert_TTName("�᭮��",mSignsV).
            RUN Insert_TTName("DateRogd",STRING(person.birthday, "99.99.9999")).

            InfSignsV = person.name-last + " " + person.first-names + " " + REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".") + " �.�., " .
            
            IF person.gender EQ TRUE THEN RUN Insert_TTName("genM", "X").
            ELSE RUN Insert_TTName("genF", "X").

            mSignsV = GetXAttrValueEx("person",
                                      STRING(person.person-id),
                                      "BirthPlace", 
                                       "").
            RUN Insert_TTName("BirthPlace",mSignsV).
            
            FIND country WHERE country.country-id EQ person.country-id NO-LOCK NO-ERROR.
                      IF AVAILABLE country THEN                       
                         mSignsV = country.country-name.
                      ELSE  
                         mSignsV = person.country-id. 
                              
                RUN Insert_TTName("country-id2", mSignsV).
                InfSignsV = InfSignsV +  trim(mSignsV) + ", ��-�� " + trim(mSignsV) .     
                
                 
                IF person.inn NE "" THEN InfSignsV = InfSignsV + ", ��� " + person.inn. 
                
                InfSignsV2 = GetCodeName("�������", person.document-id) .              
                RUN Insert_TTName("documtype", InfSignsV2).   

                IF NUM-ENTRIES(person.document," ") EQ 2 THEN 
                DO:
                    RUN Insert_TTName("document1",ENTRY(1,person.document," ")).
                    RUN Insert_TTName("document2",ENTRY(2,person.document," ")).
                    InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ").                                    
                END.
                ELSE IF NUM-ENTRIES(person.document," ") EQ 1 THEN 
                    do:
                        RUN Insert_TTName("document1","���").   
                        RUN Insert_TTName("document2",person.document).
                        InfSignsV2 = InfSignsV2 + " " + person.document.
                    end.
                    ELSE 
                    DO:
                        RUN Insert_TTName("document1",ENTRY(1,person.document," ") + " " +   ENTRY(2,person.document," ") ).
                        RUN Insert_TTName("document2",ENTRY(3,person.document," ")).
                        InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ") + " " + ENTRY(3,person.document," ").
                    END. 

                RUN Insert_TTName("mIssue",fGetDocIssue(person.person-id) ).                         /* person.issue��� �뤠�*/
                                            
                IF NUM-ENTRIES(person.Issue,",") LT 2 THEN 
                DO:
                    RUN Insert_TTName("mIssue1", person.Issue).
                    RUN Insert_TTName("mIssue2","���").
                END.
                ELSE 
                DO:
                    RUN Insert_TTName("mIssue1",ENTRY(1,person.Issue,",")).
                    RUN Insert_TTName("mIssue2",ENTRY(2,person.Issue,",")).
                END.            
                                          
                mSignsV = GetXAttrValueEx("person",
                                          STRING(person.person-id),
                                          "Document4Date_vid", 
                                          ""). 
                RUN Insert_TTName("Document4Date_vid", REPLACE(mSignsV, "/", ".")).                   /*Paspdate*/                                
                                                 
                InfSignsV2 = InfSignsV2 + ", ��." + REPLACE(mSignsV, "/", ".") + ", " +  fGetDocIssue(person.person-id).  /*person.issue */

                RUN RetAdr.p(person.person-id, "�", "����ய", ?, OUTPUT InfSignsV3).           
                RUN Insert_TTName("����ய",InfSignsV3).
                
                RUN RetAdr.p(person.person-id, "�", "�������", ?, OUTPUT mSignsV).           
                RUN Insert_TTName("�������",mSignsV).
                
                RUN RetAdr.p(person.person-id, "�", "�������", ?, OUTPUT mSignsV).
                RUN Insert_TTName("�������",mSignsV).
                
                InfSignsV3 = InfSignsV3 + " " + mSignsV.             
                
                mSignsV = GetXattrValue("person",STRING(person.person-id),"cell-phone").
                mSignsV = IF person.phone[1] NE "," THEN 
                        IF SUBSTRING(person.phone[1],1,1) EQ "," THEN
                            SUBSTRING(person.phone[1],2) 
                        ELSE IF SUBSTRING(person.phone[1],LENGTH(person.phone[1]),1) EQ "," THEN
                                SUBSTRING(person.phone[1],1,LENGTH(person.phone[1]) - 1)
                             ELSE person.phone[1] 
                        ELSE "" +
                        IF person.phone[2] NE "," THEN 
                        IF SUBSTRING(person.phone[2],1,1) EQ "," THEN 
                            IF SUBSTRING(person.phone[2],LENGTH(person.phone[2]),1) EQ "," THEN
                                SUBSTRING(person.phone[2],1,LENGTH(person.phone[2]) - 1) 
                            ELSE person.phone[2] 
                        ELSE ("," + person.phone[2]) ELSE "" +
                        IF (person.phone[1] EQ "," AND person.phone[2] EQ "," AND mSignsV EQ "") THEN "" ELSE "," +
                        IF mSignsV  NE "" THEN 
                        IF SUBSTRING(mSignsV ,LENGTH(mSignsV),1) EQ "," THEN 
                            SUBSTRING(mSignsV,1,LENGTH(mSignsV) - 1) 
                        ELSE mSignsV  
                ELSE "".                              
                InfSignsV3 = InfSignsV3 + ", " + mSignsV.   

                       RUN Insert_TTName("BenFIO" + string(i), InfSignsV).
                       RUN Insert_TTName("BenDoc" + string(i), InfSignsV2).  
                       RUN Insert_TTName("BenAdr" + string(i), InfSignsV3). 
                       RUN Insert_TTName("BenCont" + string(i), "��אַ� �������� - ����稥 �८������饣� ����� ����� 25 ��業⮢ � ����⠫� �࣠����樨").

       i = i + 1.
	END.

	IF mSignsVal EQ '' THEN DO:
		mTmp1 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"��।��","").
		mTmp2 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"��⠢���","0").
		mTmp3 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"�����","").
		IF mTmp1 NE "" AND mTmp2 NE "0" THEN
			mSignsVal = mTmp1.
		ELSE
			ASSIGN
				mLog = FALSE
				mSignsVal = mTmp3.
	END.

	RUN Insert_TTName("Benif", mSignsVal).

	IF mLog THEN
		RUN Insert_TTName("BenifOsn", '��אַ� �������� - ����稥 �८������饣� ����� ����� 25 ��業⮢ � ����⠫� �࣠����樨').
	ELSE
		RUN Insert_TTName("BenifOsn", '����� ������������ ��⠭����� �����樠୮�� �������� � ��ᮮ⢥��⢨� �����, ����᫥��� � 115-��, �ਭ�� �襭�� �����樠�� �������楬 �ਧ���� �㪮����⥫�').

	DEFINE VARIABLE mStrTMP1   AS CHARACTER NO-UNDO. 
	DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO. 
	DEFINE VARIABLE mItem      AS INT NO-UNDO. 
	DEFINE VARIABLE vOkvedCodeName  AS CHARACTER NO-UNDO. 

	mStrTMP1 = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�����", "").

		   RUN BeginCircle_TTName("okved").
		  DO mItem =1 TO NUM-ENTRIES(mStrTMP1):
			 mStrTMP = ENTRY(mItem,mStrTMP1).
			 vOkvedCodeName = GetCodeName("�����2", mStrTMP).
			 RUN Insert_TTName("OKVED" + STRING(mItem), mStrTMP + "  " + vOkvedCodeName).
			 mStrTMP = REPLACE(mStrTMP, ".", " ").
			 RUN NextCircle_TTName("okved").
		  END.
		  RUN EndCircle_TTName("okved").


	 /* ��室�� ����� ��業��� */
	 /*ayv ������� ���� ��業���
	 	RUN Insert_TTName ("graph", ""). 
   
		FIND FIRST ttNames WHERE
	              ttnames.tname EQ 'graph'
			NO-LOCK NO-ERROR.

		mLog = FALSE.

		FOR EACH cust-ident WHERE cust-ident.cust-cat EQ '�' 
		   					AND   cust-ident.cust-id EQ cust-corp.cust-id 
		  					AND   cust-ident.class-code EQ 'cust-lic' 
		  					AND  (cust-ident.close-date EQ ?  
		  					OR    cust-ident.close-date GE TODAY) 
		  					AND   cust-ident.open-date LE TODAY 
		  		NO-LOCK:

		  	mLog = TRUE.
		  	ttnames.tvalue = ttnames.tvalue + '���'                                       + '\n'
		  									+ '��業���'       							  + '\n'
		  									+ '�����'                                     + '\n'
		  									+  cust-ident.cust-code         			  + '\n'
		  									+ '��� �뤠� ��業���'                      + '\n'
		  									+  STRING(cust-ident.open-date)				  + '\n'
		  									+ '��� �뤠��'                                + '\n'
		  									+  cust-ident.issue							  + '\n'
		  									+ '�ப ����⢨�'                             + '\n'
		  									+  (IF cust-ident.close-date EQ ? 
		  									   THEN '��� ��࠭�祭�� �ப� ����⢨�' 
		  									   ELSE STRING(cust-ident.close-date)) 		  + '\n'
		  									+ '���祭� ����� ��業���㥬�� ���⥫쭮��' + '\n'
		  									+  GetCodeName("�����愥��",
		  													cust-ident.cust-code-type)    + '\n'.

		END.

		IF NOT mLog THEN 
			ttnames.tvalue = ttnames.tvalue + '���'                                       + '\n'
		  									+ '���'		       							  + '\n'
		  									+ '�����'                                     + '\n'
		  									+ '���'					         			  + '\n'
		  									+ '��� �뤠� ��業���'                      + '\n'
		  									+ '���'										  + '\n'
		  									+ '��� �뤠��'                                + '\n'
		  									+ '���'										  + '\n'
		  									+ '�ப ����⢨�'                             + '\n'
		  									+ '���'										  + '\n'
		  									+ '���祭� ����� ��業���㥬�� ���⥫쭮��' + '\n'
		  									+ '���'										  + '\n'.	
		 													
			IF AVAIL cust-ident THEN DO:
					
				/*RUN Insert_TTName("������", GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��撨�", "")). */
				RUN Insert_TTName("������",'��業���').
				
				mSignsVal = STRING(cust-ident.open-date).
				RUN Insert_TTName("��愠⠂�", mSignsVal). 
					
				mSignsVal = IF cust-ident.close-date EQ ? THEN '��� ��࠭�祭�� �ப� ����⢨�' ELSE STRING(cust-ident.close-date).
				RUN Insert_TTName("��愠⠎", mSignsVal).  
					
				mSignsVal = cust-ident.cust-code.
				RUN Insert_TTName("��据�", mSignsVal). 
					
				mSignsVal = GetCodeName("�����愥��",cust-ident.cust-code-type).
				RUN Insert_TTName("��您�", mSignsVal).
					
				mSignsVal = cust-ident.issue.
				RUN Insert_TTName("��报���", mSignsVal). 
				
		  	END.
		  	ELSE DO:
		  		RUN Insert_TTName("������", "���").
				RUN Insert_TTName("��愠⠂�", "���").
				RUN Insert_TTName("��愠⠎", "���").
				RUN Insert_TTName("��据�", "���").
				RUN Insert_TTName("��您�", "���").
				RUN Insert_TTName("��报���", "���").
		  	END.*/
		  	
		  	
		  	  FIND FIRST cust-ident WHERE cust-ident.cust-cat EQ "�" 
                            AND   cust-ident.cust-id EQ cust-corp.cust-id 
                            AND   cust-ident.class-code EQ "cust-lic" 
                            AND  (cust-ident.close-date EQ ? 
                            OR    cust-ident.close-date GE end-date) 
                            AND   cust-ident.open-date LE end-date                              
                            NO-LOCK NO-ERROR.                            
                                                                            
        If NOT AVAIL cust-ident THEN 
            DO:
                RUN Insert_TTName("������", "���").
                RUN Insert_TTName("��愠⠂�", "���").
                RUN Insert_TTName("��愠⠎", "���").
                RUN Insert_TTName("��据�", "���").
                RUN Insert_TTName("��您�", "���").
                RUN Insert_TTName("��报���", "���").
            END.       
        ELSE
            Do:    
                mSignsV = GetCodeName("�����愥��",cust-ident.cust-code-type).
                RUN Insert_TTName("������",mSignsV).
                  
                mSignsV = REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".").
                RUN Insert_TTName("��愠⠂�", mSignsV ). 
                    
                mSignsV = IF cust-ident.close-date EQ ? THEN '��� ��࠭�祭�� �ப� ����⢨�' ELSE STRING(cust-ident.close-date).
                RUN Insert_TTName("��愠⠎",  mSignsV).  
                    
                mSignsV = cust-ident.cust-code.
                RUN Insert_TTName("��据�",  mSignsV). 
                    
               /* mSignsV = GetXattrValueEx("cust-ident",STRING(cust-corp.cust-id),"��業���","").
                RUN Insert_TTName("��您�",  mSignsV).*/
                
                mSignsV = GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"��業���","").
                RUN Insert_TTName("��您�",  mSignsV).
                    
                mSignsV = cust-ident.issue.
                RUN Insert_TTName("��报���",  mSignsV).
            end.


/*FOR each cust-role WHERE 
        cust-role.file-name EQ "cust-corp"  
        AND cust-role.surrogate EQ STRING(cust-corp.cust-id)
        AND cust-role.class-code = "�ࠢ�_��ࢮ�_������"
        and cust-role.cust-cat = "�"
         NO-LOCK:
FIND FIRST person WHERE
           person.person-id EQ INT(cust-role.surrogate) NO-LOCK NO-ERROR.
IF NOT AVAIL person THEN
           RETURN.

DO:
        RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", " ")). 
                mSignsV = GetXAttrValueEx("person",
                                  STRING(person.person-id),
                                  "tel", 
                                 "").
                RUN Insert_TTName("tel", mSignsVal). 
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

                RUN Insert_TTName("DateRogd",person.birthday). 
                RUN Insert_TTName("document-id",person.document-id).           
                RUN Insert_TTName("document",person.document).                
        mcountr1 = GetXAttrValueEx("person",
                                  STRING(person.person-id),
                                 "country-id2", 
                                 "").
        IF mcountr1 = "RUS" THEN mcountr="��" .
        ELSE mcountr = mcountr1.        
        RUN Insert_TTName("country-id2", mcountr).
END.
          
END.*/


/*-----------------------------------------------------*/

/*FIND FIRST history WHERE history.modify EQ "C" 
    AND NOT history.FIELD-ref BEGINS "_system_"
    AND history.field-ref EQ STRING(cust-corp.cust-id)
    AND history.file-name EQ 'cust-corp' NO-LOCK NO-ERROR.

FIND FIRST _User WHERE _User._Userid EQ history.user-id NO-LOCK NO-ERROR.
IF AVAIL(_User) /*AND (_User._User-Name NE "SYNC")*/ THEN 
DO:
    RUN Insert_TTName("_Kur-Name",_User._User-Name).          
    mStatus = GetXAttrValueEx("_user", _User._Userid, "���������", "").
    RUN Insert_TTName("���_�����", mStatus).   
END.
ELSE 
DO:
    FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
    RUN Insert_TTName("_Kur-Name",_User._User-Name).    
            
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "���������", "").
    RUN Insert_TTName("���_�����", mStatus).
END.*/

FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
IF AVAIL(_User) THEN 
DO:
    RUN Insert_TTName("Prn_Name",_User._User-Name). 
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "���������", "").
    RUN Insert_TTName("Prn_Dolg", mStatus). 
END.

 mSignsVal = trim(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id","")). 
If mSignsVal ='' then mSignsVal = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "�⤥�����", "").    
   
            IF   CAN-DO("0300", mSignsVal) THEN     
            Do: 
            mSignsV = '����᪨�� �.�.'.
            mStatus = "��砫쭨� ����樮����� �⤥�� ��".
            end.
            else
            IF   CAN-DO("0100,0500", mSignsVal) THEN 
            Do: 
            mSignsV = "��設� �.�. ".
            mStatus = "�㪮����⥫� ���ࠢ����� ��� ��".
            end. else
            IF   CAN-DO("0301,0302", mSignsVal) THEN 
            Do: 
            mSignsV = "��堩���� �.�.".
            mStatus = "���訩 ����樮����-����� ��" + "'" + "�ࠫ�᪨�" + "'".
            end. else
            IF   CAN-DO("0400,0401,0402,0403,0404,0405,0406,0407,0408,0409,0000,0101,0102,0106,0109,0110,0111", mSignsVal) THEN 
            Do:
                if end-date LE date("10/07/2017") then
                    do: 
                    mSignsV = "����㬡���� �.�. ".
                    mStatus = "�㪮����⥫� ���ࠢ����� ��� �� �. ��᪢�".
                    end.    
                else
                    do:
                    mSignsV = "����ન�� �.�.".
                    mStatus = "�ᯮ���⥫�� ��४��/��४�� �����⠬��� ���".                       
                    end.    
            end.                      
            else 
            IF   CAN-DO("0516,0517", mSignsVal) THEN 
            Do: 
            mSignsV =  "��吝��� �.�.".
            mStatus = "���訩 ����樮����-����� ��" + "'" + "���室" + "'".
            end.                      
            else 
            Do: 
            mSignsV = "䨫��� ����।����".
            mStatus = "������ ����।����".
            end. 
             
              RUN Insert_TTName("_Kur-Name", mSignsV).       
              RUN Insert_TTName("���_�����", mStatus).   
             

IF NOT CAN-DO("zay_dkbo*,zay_kb*", iParms) THEN
DO:  
    IF CAN-DO("*infank_ul*,ankvygul*", iParms) THEN
    Do:      
        If end-date LE date("26/12/2015") THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "0" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
        
        If (end-date GE date("26/12/2015"))  and  (end-date LE date("28/11/2016"))  THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
        
        If (end-date GE date("29/11/2016")) THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1)  . 
    END.
    
    IF CAN-DO("*ankul*", iParms) THEN
    
        iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "_" + SUBSTRING ( iParms, LENGTH (iParms), 1) .   
END.

&IF DEFINED(gdTplName) 
    &THEN iParms = GetXAttrValueEx ("user-proc",
                                    STRING(user-proc.public-number),
                                    "������",
                                    "").
    &ELSE iParms = ENTRY(1, iParms, "|").
&ENDIF

/* �뢮� ������ �� 蠡���� iParms � 䠩� ���� */
RUN printvd.p (iParms, INPUT TABLE ttnames).   
{intrface.del comm}
