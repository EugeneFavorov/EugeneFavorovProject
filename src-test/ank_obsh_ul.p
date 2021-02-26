/*
               ������᪠� ��⥣�஢����� ��⥬� �������mSymbol
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pac_doc.p
      Comment: ����� ���㬥�⮢ �� ����⨥ ���.���
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 
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

DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE mDateBeg         AS DATE               NO-UNDO. /*???*/   
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

DEFINE VARIABLE mStrTMP2   AS CHARACTER NO-UNDO.      
DEFINE VARIABLE mStrTMP1   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mItem      AS INT NO-UNDO. 
DEFINE VARIABLE vOkvedCodeName  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE Benif  AS CHARACTER NO-UNDO. 

DEF VAR mSignsVal AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignVL   AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mSignsV   AS CHAR NO-UNDO. /* ���祭�� ४����� �� person */

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p      AS RECID.
DEFINE VARIABLE i         AS INT64 INITIAL 1 NO-UNDO. 

DEFINE VARIABLE mTarget        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mTargetDetails            AS CHARACTER EXTENT 3 NO-UNDO.
  
DEF TEMP-TABLE ttOtchData NO-UNDO
   FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* ⠡� � �����ᮬ �⮡ �� ������� � ���஢���*/

FUNCTION GetDateTemp RETURN CHAR (
   INPUT iN-FileName AS CHAR,
   INPUT iN-Surr     AS CHAR,   
   INPUT iN-Date     AS DATE 
):
   DEF VAR vValue    AS CHAR   NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO. /* ���祭�� ��. */  
   
   EMPTY TEMP-TABLE ttOtchData.
     
    FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "��⠎��������" 
        OR  tmpsigns.code = "��⠏஢����")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
        AND tmpsigns.since GE date("27/12/2015")   /**� �� �뫮 �� ��� �� ������� **/                     
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
        FOR EACH ttOtchData  NO-LOCK: 
            vResult =   vResult +  " "  + REPLACE(STRING(ttOtchData.ttDate,"99/99/9999"), "/", ".").
    END.

    RETURN trim(vResult).
    
{empty ttOtchData} 
END FUNCTION.   

{getdate.i &DateLabel = "��� ����"  
           &DateHelp  = "������ ���� ���� ������"} 

ASSIGN
   mSymbol = CHR(9).

FIND FIRST tmprecid NO-LOCK NO-ERROR.
		      
    FIND FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK NO-ERROR.
     
       mName[1] = cust-corp.cust-stat.
       mName[2] = cust-corp.name-corp.

        FIND FIRST code WHERE 
                  code.class EQ "����।�"
              AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
        IF AVAIL code THEN
            mName[1]  = code.name.
            RUN Insert_TTName("stat1",mName[1]).

        RUN Insert_TTName("CustName",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
        RUN Insert_TTName("NameShort", IF cust-corp.name-short NE '' THEN ', ' + cust-corp.name-short ELSE cust-corp.name-short).
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "engl-name", 
                                    "").
        RUN Insert_TTName("NameEngl", IF mSignsVal NE '' THEN ', ' + mSignsVal ELSE mSignsVal).          
                    
        RUN Insert_TTName("���",cust-corp.inn).
        
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "����", 
                                    "").
        RUN Insert_TTName("����", mSignsVal). 
            mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
                                       STRING(cust-corp.cust-id), 
                                       "RegDate", 
                                       "")) NO-ERROR.
        RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).
        
        /*MestReg*/
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "���③���।", 
                                    "").                                    
        RUN Insert_TTName("���③���।", mSignsVal).  
        
        /*NameReg*/
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "�࣑����।", 
                                    "").
        RUN Insert_TTName("�࣑����।", mSignsVal). 
        
        RUN RetAdr.p(cust-corp.cust-id,  "�", "�����", end-date,OUTPUT mAdrReg).
        /* MESSAGE end-date VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        RUN Insert_TTName("�����",IF mAdrReg eq "" THEN "���" ELSE mAdrReg). 
        
        RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", end-date,OUTPUT mAdrFact).
        RUN Insert_TTName("�������",IF mAdrFact eq "" THEN "���" ELSE mAdrFact).
        
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
      
        mSignsVal = (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "e-mail","")).
            
        RUN RetAdr.p(cust-corp.cust-id,  "�", "�������", end-date ,OUTPUT mAdrReg).
                If mAdrReg ne "" THEN
                    mSignsVal = mSignsVal + "; " +  mAdrReg. 
        IF mSignsVal="" THEN mSignsVal= "���".  
        RUN Insert_TTName("e-mail", mSignsVal).
        
        RUN Insert_TTName("okpo",IF cust-corp.okpo NE '' THEN cust-corp.okpo ELSE '���').
      
            mSignsV = if GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����","") = "" THEN GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����-�����","") ELSE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����","") . 
        RUN Insert_TTName("OKATO", IF  mSignsV="" THEN "���" ELSE mSignsV).
 
            mSignsV = GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�����",end-date, "").          
           
        RUN BeginCircle_TTName("okved").
          DO mItem =1 TO NUM-ENTRIES(mSignsV):
             mStrTMP = ENTRY(mItem,mSignsV).
             vOkvedCodeName = GetCodeName("�����", mStrTMP).
             RUN Insert_TTName("OKVED" + STRING(mItem), if (mStrTMP + "  " + vOkvedCodeName) = "" THEN "���" ELSE mStrTMP + "  " + vOkvedCodeName).
             mStrTMP = REPLACE(mStrTMP, ".", " ").
             RUN NextCircle_TTName("okved").
          END.
        RUN EndCircle_TTName("okved").
      
      /*  RUN Insert_TTName("OrgUpr_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="" THEN "" ELSE "V").
        RUN Insert_TTName("OrgUpr_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="" THEN "V" ELSE "").
        */
        
        mSignsVal = "".     
          IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", "")) ne "" then
          do:                              
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="��") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", "")) begins "�����")  THEN mSignsVal = "V".
        RUN Insert_TTName("OrgUpr_Da", mSignsVal).
        
        mSignsVal = "".     
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", ""))="���") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�����࣓�ࠢ", "")) begins "�����") THEN mSignsVal = "V".
        RUN Insert_TTName("OrgUpr_Net", mSignsVal).
        end.
        else
        do:
           IF mAdrReg eq mAdrFact then
            RUN Insert_TTName("OrgUpr_Da", "�").
           else
            RUN Insert_TTName("OrgUpr_Net", "�").
         end.
        
        /*��⠢��� ��᫥ ��業���*/
             
     /*��業���*/
                                                         
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
                    
                mSignsV = GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"��業���","").
                RUN Insert_TTName("��您�",  mSignsV).
                    
                mSignsV = cust-ident.issue.
                RUN Insert_TTName("��报���",  mSignsV).
            end.
              
         mSignsVal = if GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�࣓�ࠢ", "") NE "" THEN GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "�࣓�ࠢ", "") else "���".      
        RUN Insert_TTName("OrgUpr",mSignsVal).
        
          mStrTMP =  GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��⠢��������",end-date, ""). 
        
        RUN Insert_TTName("��⠢���", mStrTMP). /*��६����� ��� ��᫥���訩 �஢�ન � �⮪� ���� �����.*/
        
             /* ��।�⥫� �� ��ꥪ⮢*/
        
        mSignsVal = ''.
        mSignsV = ''.
        mLog = TRUE.
        i = 1.
 
       FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   ((cust-role.class-code EQ '��।�⥫�') or (cust-role.class-code EQ '��樮���'))
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
                       
             NO-LOCK:
            {additem.i mSignsVal cust-role.cust-name}               
                 
       end.
       IF mSignsVal eq "" THEN            
           mSignsVal = if GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��।��",end-date, "") NE "" THEN GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "��।��",end-date, "") else "���".
     
        RUN Insert_TTName("��।��", mSignsVal).
               
        /*    IF mSignsVal EQ '' THEN DO:
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

        RUN Insert_TTName("Benif", mSignsVal).***/
                         
            IF mSignsVal EQ "" AND mStrTMP EQ "0" THEN              
                ASSIGN mLog = FALSE.       

            IF mLog THEN
                RUN Insert_TTName("BenifOsn", '��אַ� �������� - ����稥 �८������饣� ����� ����� 25 ��業⮢ � ����⠫� �࣠����樨').
            ELSE
                RUN Insert_TTName("BenifOsn", '����� ������������ ��⠭����� �����樠୮�� �������� � ��ᮮ⢥��⢨� �����, ����᫥��� � 115-��, �ਭ�� �襭�� �����樠�� �������楬 �ਧ���� �㪮����⥫�').
             
        /*�� ��४�� ��ꥪ⮢*/
        
         mSignsVal = ''.
         mSignsV = ''.
        mLog = TRUE.
        i = 1.

        FIND FIRST cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   cust-role.class-code Matches '*�४��*'
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
             NO-LOCK NO-ERROR.

        IF AVAILABLE(cust-role) THEN
        DO:
        {additem.i mSignsVal cust-role.cust-name}              
               
                            
        mSignsV = "".
        FIND FIRST class WHERE class.class-code EQ cust-role.class-code no-error.
        IF AVAILABLE class THEN
        mSignsV = class.name.
         end.           
         ELSE 
        DO:   
            mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "�����",?).          
                    
            mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "�����",?).   
       
        END.
         RUN Insert_TTName("�����", IF mSignsV eq "" THEN "���" ELSE mSignsV ). 
         RUN Insert_TTName("�����", IF mSignsVal eq "" THEN "���" ELSE mSignsVal ).
                 
          
 /****/
 
      RUN Insert_TTName("Bf_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))="��" THEN "V" ELSE "").
        RUN Insert_TTName("Bf_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))="���" THEN "V" ELSE "").
                
        /* �饬 �����樠஢ */
        mSignsVal = ''.
        mStrTMP = ''.
        mLog = TRUE.
        i = 1.
        Benif = ''.

        FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   cust-role.class-code EQ '������_����'
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
            NO-LOCK:

        {additem.i mSignsVal cust-role.cust-name}              
                                    
                
        FIND FIRST person WHERE
                  cust-role.cust-id EQ string(person.person-id)
            NO-LOCK NO-ERROR.
            
            InfSignsV = person.name-last + " " + person.first-names + " " + REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".") + " �.�., " .
            
            Benif =  trim(person.name-last + " " + person.first-names + " " +  Benif).              
            
             FIND country WHERE country.country-id EQ person.country-id NO-LOCK NO-ERROR.
                      IF AVAILABLE country THEN                       
                         mSignsV = country.country-name.
                      ELSE  
                         mSignsV = person.country-id. 
                              
                InfSignsV = InfSignsV +  trim(mSignsV) + ", ��-�� " + trim(mSignsV).  
                
                    
                IF person.inn NE "" THEN InfSignsV = InfSignsV + ", ��� " + person.inn.   
                 
                   InfSignsV2 = GetCodeName("�������", person.document-id) .              
                                                  
                 IF NUM-ENTRIES(person.document," ") EQ 2 THEN
                           InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ") .                                    
                              
                        ELSE IF NUM-ENTRIES(person.document," ") EQ 1 THEN                          
                         InfSignsV2 = InfSignsV2 + " " + person.document .                        
                         ELSE 
                            InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ") + " " + ENTRY(3,person.document," ").
                                          
                           mSignsV = GetXAttrValueEx("person",
                                  STRING(person.person-id),
                               "Document4Date_vid", 
                                 "").                                 
                               
                InfSignsV2 = InfSignsV2 + ", ��." + REPLACE(mSignsV, "/", ".") + ", " + person.issue /*fGetDocIssue(person.person-id)*/ .   
               
        RUN RetAdr.p(person.person-id, "�", "����ய", end-date, OUTPUT InfSignsV3).           
        
        RUN RetAdr.p(person.person-id, "�", "�������", end-date, OUTPUT mSignsV).                
           
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
                mSignsVal =  GetXAttrValueEx("cust-role",
                                    STRING(cust-role.cust-role-id),
                                    "���",?) + " %".    
                                    
                                    
           
                 
            RUN Insert_TTName("BenFIO" + string(i),  InfSignsV).
            RUN Insert_TTName("BenDoc" + string(i), InfSignsV2).  
            RUN Insert_TTName("BenAdr" + string(i), InfSignsV3). 
            RUN Insert_TTName("BenCont" + string(i), IF mSignsVal eq "" THEN "��אַ� �������� - ����稥 �८������饣� ����� ����� 25 ��業⮢ � ����⠫� �࣠����樨" ELSE mSignsVal).
        
            i = i + 1.           
         END.
            RUN Insert_TTName("Benif",IF trim(Benif) NE '' THEN  Benif ELSE '���').      

        /*5*/
        RUN out_template_log("ank_third","tl_yes","tl_no").
        
       /*6*/
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_goal","") eq trim("����⨥ � ������� ������᪮�� ���") then        
            RUN Insert_TTName("GOAL", "V").    
       else 
        Do:                   
            RUN Insert_TTName("GOAL_INOE", "V").
            RUN Insert_TTName("goal_in",  ": " + GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_goal","")).
        end. 
             
            mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_go5al","").
       
  /*     If mSignsV eq "����⨥ � ������� ������᪮�� ���" THEN  RUN Insert_TTName("goal","V").
       
       if SUBSTRING(mSignsV, 1, 5) eq "����:" THEN 
       Do:  RUN Insert_TTName("goal_inoe","V"). 
            RUN Insert_TTName("goal_in",": " + SUBSTRING(mSignsV, 6, LENGTH (mSignsV))).
       END.            
    */                
                    
     /*7*/  
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoChar"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            mTarget = trim(ENTRY(1,cust-ident.cust-code,mSymbol)).
           /* mTargetDetails = GetCodeDesc("����⭊�",mTarget,1,"").    
                     
            MESSAGE     mTarget                        VIEW-AS ALERT-BOX .  */ 
        CASE mTarget:
            WHEN "����������"         THEN RUN Insert_TTName("d_bn", "V"). 
            WHEN "�����鄥��।�⢄"  THEN RUN Insert_TTName("d_rd", "V").
            WHEN "���Ꭱ��"         THEN RUN Insert_TTName("d_ko", "V").
            WHEN "�����ᄥ��।"      THEN RUN Insert_TTName("d_id", "V").
            WHEN "�।�⮢����"       THEN RUN Insert_TTName("d_kr", "V").
            WHEN "���猥��㭠�"       THEN do: RUN Insert_TTName("d_mr", "V").   RUN out_template_log("ank_inter_pay","d_mr_yes","d_mr_no"). end.
            WHEN "����"               THEN 
                                        Do: 
                                            RUN Insert_TTName("d_in", "V").
                                             mSignsV = entry(1, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";"). 
                                            RUN Insert_TTName("d_inoe", mSignsV). 
                                        
                                        END.
        END CASE.
        end.        
        
         mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_in_taxpay","").          
        
        If mSignsV  ne "" THEn Do:  RUN Insert_TTName("m_nal_yes", "V").  RUN Insert_TTName("nal_country",  mSignsV ).   End.
        else RUN Insert_TTName("m_nal_no", "V"). 
        
        
      /* 8.1  ���� ����� ������ ������????? ����� ������� */
            
         mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoOper"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        AND ENTRY(1,cust-ident.cust-code,mSymbol) eq "������㥬�" 
        NO-LOCK:              
            
            mTarget = ENTRY(1,cust-ident.cust-code,mSymbol).      
            
           If  Int(ENTRY(2,cust-ident.issue,mSymbol))<100 THEN RUN Insert_TTName("kol", "V"). 
           else If Int(ENTRY(2,cust-ident.issue,mSymbol))<=500 THEN RUN Insert_TTName("kol_100", "V"). 
                ELSE If Int(ENTRY(2,cust-ident.issue,mSymbol))>500 THEN RUN Insert_TTName("kol_500", "V"). 
          
           If  Int(ENTRY(3,cust-ident.issue,mSymbol))<10 THEN RUN Insert_TTName("sum", "V"). 
           else If Int(ENTRY(3,cust-ident.issue,mSymbol))<=100 THEN RUN Insert_TTName("sum_10", "V").
             else  If Int(ENTRY(3,cust-ident.issue,mSymbol))>100 THEN RUN Insert_TTName("sum_100", "V").  
                                       
        end.   
        
        
        /*8.2*/     
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoVCont"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date      
        NO-LOCK:         
        
        mTarget    = GetXAttrValueEx("cust-ident",
                                STRING(cust-ident.cust-code-type) + ',' +
                                STRING(cust-ident.cust-code) + ',' +
                                STRING(cust-ident.cust-type-num),
                                "������","").
           
           CASE mTarget:
            WHEN "�७��"               THEN RUN Insert_TTName("vid_a", "V"). 
            WHEN "����犮��ဣ���"      THEN RUN Insert_TTName("vid_k","V").
            WHEN "���㯪��த���"       THEN RUN Insert_TTName("vid_p", "V").
            WHEN "��㣨"               THEN RUN Insert_TTName("vid_ser", "V").           
            WHEN "����"                 THEN Do: 
                                                RUN Insert_TTName("vid_inoe", "V"). 
                                                mSignsV = entry(2,GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";").
                                                RUN Insert_TTName("vid_in",": " + mSignsV). 
                                             END.
        END CASE.
        end.  
       
        /* 8.3 */
        mSignsV ="".
        mSignsVal ="".       
      
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoPartner"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK:            
            mTarget    =   GetXAttrValueEx("cust-ident",
                                STRING(cust-ident.cust-code-type) + ',' +
                                STRING(cust-ident.cust-code) + ',' +
                                STRING(cust-ident.cust-type-num),
                                "CliInfo","").            
                                                  
                                                          
            mSignsV       =  mSignsV   + ENTRY(1,mTarget,mSymbol) +  chr(13).
            If ENTRY(3,mTarget,mSymbol) NE "" Then 
                mSignVL = ENTRY(3,mTarget,mSymbol).
            else
                mSignVL = "���".
            mSignsVal       =  mSignsVal  + mSignVL + chr(13).    
        end.
        RUN Insert_TTName("partner",  mSignsV).
        RUN Insert_TTName("partner_inn", mSignsVal). 
        
        /*9*/
                               
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoPos"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            
            mTarget = trim(ENTRY(1,cust-ident.issue,mSymbol)).                                   
        CASE mTarget:            
            WHEN "��������"            THEN RUN Insert_TTName("fp_bb", "V").
            WHEN "��������"             THEN RUN Insert_TTName("fp_nd", "V").
            WHEN "��������"             THEN RUN Insert_TTName("fp_spr","V").
            WHEN "��㤨⇠��"           THEN RUN Insert_TTName("fp_az", "V"). 
        END CASE.
        end.  
                       
  
         RUN out_template_log("ank_bankrupt","b_yes","b_no").
         RUN out_template_log("ank_legal_proc","s_yes","s_no").
         RUN out_template_log("ank_liquidation","l_yes","l_no").
         RUN out_template_log("ank_nonperformance","nonperf_yes","nonperf_no").   
                   
       /*10*/  
         
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id
        and cust-ident.cust-code-type eq ""   /*����� �� ����, ������������ �� ����������, � ���� ����*/
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK:    
            mSignsV = ENTRY(1,cust-ident.cust-code,mSymbol).        
        CASE mSignsV:
            WHEN "�᭄���"         THEN RUN Insert_TTName("doh_o", "V"). 
            WHEN "�������"         THEN RUN Insert_TTName("doh_d", "V").
            WHEN "��焥��"        THEN RUN Insert_TTName("doh_p", "V").           
            WHEN "����"            THEN Do: RUN Insert_TTName("doh_inoe", "V"). RUN Insert_TTName("doh_in",entry(3, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.
        end.    
                         
         /*11*/       
                         
   FIND FIRST cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        and ENTRY(1,cust-ident.issue,mSymbol) eq "��㣨�����" 
        NO-LOCK NO-ERROR.
   
   IF AVAIL(cust-ident) THEN 
         RUN Insert_TTName("o_yes", "V"). 
   else  RUN Insert_TTName("o_no", "V").   
   
      FIND FIRST cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        and ENTRY(1,cust-ident.issue,mSymbol) eq "��㣨��������" 
        NO-LOCK NO-ERROR.  
   IF AVAIL(cust-ident) THEN 
         RUN Insert_TTName("o_cl_yes", "V"). 
   else  RUN Insert_TTName("o_cl_no", "V"). 
   
       mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            mSignsV = trim(ENTRY(1,cust-ident.issue,mSymbol)).                     
        CASE mSignsV:
            WHEN "���⎡ꥤ��"          THEN RUN Insert_TTName("part_1", "V"). 
            WHEN "���⎡��⢄���"         THEN RUN Insert_TTName("part_2", "V").
            WHEN "���⁫����"             THEN RUN Insert_TTName("part_3", "V").
            WHEN "����"                     THEN Do: RUN Insert_TTName("part_4", "V"). RUN Insert_TTName("part_in",entry(4, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.
        end.

      mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_lifetime", "").
        CASE mSignsVal:   
            WHEN "�����  5 ���"         THEN RUN Insert_TTName("srok_1", "V"). 
            WHEN "�� 1 �� 5 ���"        THEN RUN Insert_TTName("srok_2", "V").
            WHEN "�� 6 �� 12 ����楢"   THEN RUN Insert_TTName("srok_3", "V").  
            WHEN "�� 3 �� 6 ����楢"    THEN RUN Insert_TTName("srok_4", "V").
            WHEN "����� 3 ����楢"      THEN RUN Insert_TTName("srok_5", "V").         
        END CASE.    
        
      mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_litigation", "").
        CASE mSignsVal:   
            WHEN "��"         THEN RUN Insert_TTName("sud_y", "V"). 
            WHEN "���"        THEN RUN Insert_TTName("sud_n", "V").
            OTHERWISE         Do:  RUN Insert_TTName("sud_i", "V").  
                                   RUN Insert_TTName("sud_in",  " " + mSignsVal). 
                              end.
        END CASE.             
                 
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "�"
        and cust-ident.cust-id        eq cust-corp.cust-id
        and cust-ident.cust-code-type eq "FinInfoRep"   /*����� �� ����, ������������ �� ����������, � ���� ����*/
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK:    
              /*  MESSAGE   cust-ident.cust-code-type                   VIEW-AS ALERT-BOX .*/
            
           mSignsV = ENTRY(1,cust-ident.cust-code,mSymbol). 
        
          /*     MESSAGE   cust-ident.cust-code-type                   VIEW-AS ALERT-BOX .*/     
        CASE mSignsV:
            WHEN "�᭄���"         THEN RUN Insert_TTName("doh_o", "V"). 
            WHEN "�������"         THEN RUN Insert_TTName("doh_d", "V").
            WHEN "��焥��"        THEN RUN Insert_TTName("doh_p", "V").           
            WHEN "����"            THEN Do: RUN Insert_TTName("doh_inoe", "V"). RUN Insert_TTName("doh_in",entry(3, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.        
        
        end.           
               
        /*12*/
           
     mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_term_rent", "").
        CASE mSignsVal:   
            WHEN "����� 3 ���"         THEN RUN Insert_TTName("srok_a", "V"). 
            WHEN "�� 3 �� 6 ���"         THEN RUN Insert_TTName("srok_a3", "V").
            WHEN "����� 6 ���"        THEN RUN Insert_TTName("srok_a6", "V").           
        END CASE.
                 
      /*13*/
      
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_glbuh","") eq "��" THEN   mSignsV   = "��". 
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_glbuh","") eq "���" THEN  mSignsV   = "���". 
                      
       RUN Insert_TTName("glbuh",mSignsV ).       
       
       mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_population","").
                      
       RUN Insert_TTName("population",mSignsV ).

       mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_average_zp","").
                      
       RUN Insert_TTName("average_zp",mSignsV ).
           
       /*14* */
       
            RUN out_template_log("ank_defense","oborona_yes","oborona_no").
            
        mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_in_taxpay","").          
        
        If mSignsV  ne "" THEn Do:  RUN Insert_TTName("m_nal_yes", "V").  RUN Insert_TTName("nal_country",  mSignsV ).   End.
        else RUN Insert_TTName("m_nal_no", "V"). 
        
         RUN out_template_log("ank_legal","legal_yes","legal_no").         
         
         /*15 */
         
        mSignsV=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��᪎��","").     
            RUN Insert_TTName("RiskDa", IF CAN-DO("*��᮪��*,*����让*", mSignsV) THEN "V" ELSE "").
            RUN Insert_TTName("RiskNet", IF CAN-DO("*������*", mSignsV) /*or (mSignsV EQ "") */ THEN "V" ELSE "").
            
                                  
        RUN Insert_TTName("�業����᪠", IF GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠","")="" THEN "" ELSE
        GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠","")).
                
         /*16*/
                  
       mSignsV = REPLACE(GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "��⠏஢����",end-date,""),"/",".").  
                                  
        RUN Insert_TTName("DP", IF mSignsV = "" THEN "���" ELSE mSignsV).   
        
            mSignsV =  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))="" THEN "�� ����⥭" ELSE          
          (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect","")).   
        
        RUN Insert_TTName("RP", mSignsV).
        
         /*17  */  
        FIND FIRST history WHERE history.modify EQ "C" 
        AND NOT history.FIELD-ref BEGINS "_system_"
        AND history.field-ref EQ STRING(cust-corp.cust-id)
        AND history.file-name EQ 'cust-corp' NO-LOCK NO-ERROR.

        /*  FIND FIRST _User WHERE _User._Userid EQ history.user-id NO-LOCK NO-ERROR.
            IF AVAIL(_User) /*AND (_User._User-Name NE "SYNC")*/ THEN 
            DO:
           mSignsV =   "�����楢�" else _User._User-Name.
              mStatus = GetXAttrValueEx("_user", _User._Userid, "���������", "").
             END.  
            */ 
            mSignsVal = trim(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id","")). 
      
            IF   CAN-DO("0300", mSignsVal) THEN     
            Do: 
            mSignsV = '�����楢� �.�.'.
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
            IF   CAN-DO("0400,0401,0402,0403,0404,0405,0406,0407,0408,0409,0000,0101,0102,0106,0109,0110", mSignsVal) THEN /*�᫨ ������� 04 �⡥�� � 0504*/
            Do: 
            mSignsV = "����㬡���� �.�. ".
            mStatus = "�㪮����⥫� ���ࠢ����� ��� �� �. ��᪢�".
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
               
         
        FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
                      AND   acct.cust-cat EQ '�'
            NO-LOCK BY acct.open-date:
            LEAVE.
        END.
        IF AVAIL acct THEN DO:
            mSignsVal = {strdate.i acct.open-date}.
            mSignsV = STRING(acct.open-date, "99.99.9999") + " �.".
            CREATE ttOtchData.   
            ASSIGN ttOtchData.ttDate = DATE(acct.open-date).
        END.
        ELSE
            DO:
            mSignsVal = {strdate.i cust-corp.date-in}.
            mSignsV = "���".
            END.
        RUN Insert_TTName("��⠇����������",REPLACE(mSignsVal,'�.','����')).
        RUN Insert_TTName("Startdate",mSignsV).
        
        mSignsV = STRING(cust-corp.date-out, "99.99.9999").                                   
        RUN Insert_TTName("Enddate", IF mSignsV = ? THEN "���" ELSE mSignsV + " �.").                         
       
       /* ������*/
         
        FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
        IF AVAIL(_User) THEN 
        DO:
          RUN Insert_TTName("Prn_Name",_User._User-Name). 
          mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "���������", "").
          RUN Insert_TTName("Prn_Dolg", mStatus). 
        END.
           
        mSignsV = REPLACE(STRING(cust-corp.date-in, "99/99/9999"), "/", ".").

        RUN Insert_TTName("DatePrint", mSignsV + " ����").    
        
        mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id), end-date), " ", ",").
        mSignsV = IF (mSignsV eq "") or (mSignsV eq ?) THEN STRING(cust-corp.date-in, "99/99/9999") ELSE  mSignsV.
        mSignsV = REPLACE(mSignsV, "/", ".") + " ����".
        RUN Insert_TTName("��⠎��������", mSignsV).         
        
/***��� ����� ����� ��砫�**/

RUN Insert_TTName("Bf_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))="��" THEN "V" ELSE "").
RUN Insert_TTName("Bf_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))="���" THEN "V" ELSE "").

/***��� ����� ����� �����**/               
         
If  end-date LE date("16/08/2015") THEN /*��*/
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .
            
If (end-date GE date("17/08/2015"))  and  (end-date LE date("26/12/2015"))  THEN /*�����⥫쭮*/       
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "2" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .
            
If (end-date GE date("27/12/2015"))  and  (end-date LE date("29/08/2016"))  THEN 
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "3" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .
            
/*If (end-date GE date("02/07/2016"))  and  (end-date LE date("29/08/2016"))  THEN 
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "4" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .*/     
         
If (end-date GE date("30/08/2016"))  and  (end-date LE date("28/11/2016"))  THEN 
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "5" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .
 
If (end-date GE date("29/11/2016")) THEN 
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "6" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .                               
                 
          /* If (end-date GE date("26/12/2015"))  and  (end-date LT date("28/11/2016"))  THEN 
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
           */
   /* MESSAGE iParms VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           
            /* �뢮� ������ �� 蠡���� iParms (�� "|") � 䠩� ���� */ 
          RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).  
        {intrface.del comm}

/*��楤��� ���������� ��/��� �����*/
PROCEDURE out_template_log :   
  def INPUT PARAMETER dr_tabl                    AS CHARACTER NO-UNDO. 
  def INPUT PARAMETER field_temp1                AS CHARACTER NO-UNDO. 
  def INPUT PARAMETER field_temp2                AS CHARACTER NO-UNDO.  

 CASE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), dr_tabl, ""):
            WHEN "��"           THEN RUN Insert_TTName(field_temp1, "V"). 
            WHEN "���"          THEN RUN Insert_TTName(field_temp2, "V").
         END CASE.      
END PROCEDURE.
