/*
               Банковская интегрированная система БИСквитmSymbol
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: pac_doc.p
      Comment: Пакет документов при открытие расч.счета
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 
      Modified: 
*/
{globals.i}                                 /* глобальные переменные         */
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
{intrface.get cust}     /* Библиотека для работы с клиентами. */
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
DEFINE VARIABLE mAdrReg          AS CHARACTER		   NO-UNDO.  /*АдрЮр*/
DEFINE VARIABLE mAdrFact         AS CHARACTER		   NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mDRuk            AS CHARACTER		   NO-UNDO. /*ДолРук*/
DEFINE VARIABLE mNRuk            AS CHARACTER EXTENT 2 NO-UNDO. /*ФИОрук*/
DEFINE VARIABLE mIssue           AS CHARACTER EXTENT 2 NO-UNDO. /*Паспорт кем выдан*/
DEFINE VARIABLE mDateBeg         AS DATE               NO-UNDO. /*???*/   
DEFINE VARIABLE mAcct            AS CHARACTER EXTENT 2 NO-UNDO.   /*acct*/
DEFINE VARIABLE mCID             AS CHARACTER          NO-UNDO. /*CID*/
DEFINE VARIABLE mCIDIP           AS CHARACTER          NO-UNDO.   /*doc-num*/   /*doc-ref*/
DEFINE VARIABLE mInn             AS CHARACTER          NO-UNDO.     /* Ю Ч*/
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

DEF VAR mSignsVal AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignVL   AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignsV   AS CHAR NO-UNDO. /* Значение реквизита из person */

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p      AS RECID.
DEFINE VARIABLE i         AS INT64 INITIAL 1 NO-UNDO. 

DEFINE VARIABLE mTarget        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mTargetDetails            AS CHARACTER EXTENT 3 NO-UNDO.
  
DEF TEMP-TABLE ttOtchData NO-UNDO
   FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* табл с индексом чтоб не морочится с сортировкой*/

FUNCTION GetDateTemp RETURN CHAR (
   INPUT iN-FileName AS CHAR,
   INPUT iN-Surr     AS CHAR,   
   INPUT iN-Date     AS DATE 
):
   DEF VAR vValue    AS CHAR   NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO. /* Значение ДР. */  
   
   EMPTY TEMP-TABLE ttOtchData.
     
    FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "ДатаОбнАнкеты" 
        OR  tmpsigns.code = "ДатаПровТерр")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
        AND tmpsigns.since GE date("27/12/2015")   /**то что было до нас не интересует **/                     
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

{getdate.i &DateLabel = "Дата печати"  
           &DateHelp  = "Укажите дату печати анкеты"} 

ASSIGN
   mSymbol = CHR(9).

FIND FIRST tmprecid NO-LOCK NO-ERROR.
		      
    FIND FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK NO-ERROR.
     
       mName[1] = cust-corp.cust-stat.
       mName[2] = cust-corp.name-corp.

        FIND FIRST code WHERE 
                  code.class EQ "КодПредп"
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
                    
        RUN Insert_TTName("ИНН",cust-corp.inn).
        
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "ОГРН", 
                                    "").
        RUN Insert_TTName("ОГРН", mSignsVal). 
            mDatTMP = DATE(GetXAttrValueEx("cust-corp", 
                                       STRING(cust-corp.cust-id), 
                                       "RegDate", 
                                       "")) NO-ERROR.
        RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).
        
        /*MestReg*/
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "МестСведПред", 
                                    "").                                    
        RUN Insert_TTName("МестСведПред", mSignsVal).  
        
        /*NameReg*/
            mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "ОргСведПред", 
                                    "").
        RUN Insert_TTName("ОргСведПред", mSignsVal). 
        
        RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрЮр", end-date,OUTPUT mAdrReg).
        /* MESSAGE end-date VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        RUN Insert_TTName("АдрЮр",IF mAdrReg eq "" THEN "нет" ELSE mAdrReg). 
        
        RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрФакт", end-date,OUTPUT mAdrFact).
        RUN Insert_TTName("АдрФакт",IF mAdrFact eq "" THEN "нет" ELSE mAdrFact).
        
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
            
        RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрПочт", end-date ,OUTPUT mAdrReg).
                If mAdrReg ne "" THEN
                    mSignsVal = mSignsVal + "; " +  mAdrReg. 
        IF mSignsVal="" THEN mSignsVal= "нет".  
        RUN Insert_TTName("e-mail", mSignsVal).
        
        RUN Insert_TTName("okpo",IF cust-corp.okpo NE '' THEN cust-corp.okpo ELSE 'нет').
      
            mSignsV = if GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО","") = "" THEN GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО-НАЛОГ","") ELSE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО","") . 
        RUN Insert_TTName("OKATO", IF  mSignsV="" THEN "нет" ELSE mSignsV).
 
            mSignsV = GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОКВЭД",end-date, "").          
           
        RUN BeginCircle_TTName("okved").
          DO mItem =1 TO NUM-ENTRIES(mSignsV):
             mStrTMP = ENTRY(mItem,mSignsV).
             vOkvedCodeName = GetCodeName("ОКВЭД", mStrTMP).
             RUN Insert_TTName("OKVED" + STRING(mItem), if (mStrTMP + "  " + vOkvedCodeName) = "" THEN "нет" ELSE mStrTMP + "  " + vOkvedCodeName).
             mStrTMP = REPLACE(mStrTMP, ".", " ").
             RUN NextCircle_TTName("okved").
          END.
        RUN EndCircle_TTName("okved").
      
      /*  RUN Insert_TTName("OrgUpr_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="" THEN "" ELSE "V").
        RUN Insert_TTName("OrgUpr_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="" THEN "V" ELSE "").
        */
        
        mSignsVal = "".     
          IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", "")) ne "" then
          do:                              
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="да") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", "")) begins "присут")  THEN mSignsVal = "V".
        RUN Insert_TTName("OrgUpr_Da", mSignsVal).
        
        mSignsVal = "".     
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="нет") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", "")) begins "отсут") THEN mSignsVal = "V".
        RUN Insert_TTName("OrgUpr_Net", mSignsVal).
        end.
        else
        do:
           IF mAdrReg eq mAdrFact then
            RUN Insert_TTName("OrgUpr_Da", "Х").
           else
            RUN Insert_TTName("OrgUpr_Net", "Х").
         end.
        
        /*уставной после лицензий*/
             
     /*Лицензии*/
                                                         
        FIND FIRST cust-ident WHERE cust-ident.cust-cat EQ "Ю" 
                            AND   cust-ident.cust-id EQ cust-corp.cust-id 
                            AND   cust-ident.class-code EQ "cust-lic" 
                            AND  (cust-ident.close-date EQ ?  
                            OR    cust-ident.close-date GE end-date) 
                            AND   cust-ident.open-date LE end-date                             
                            NO-LOCK NO-ERROR.                            
                                                                            
        If NOT AVAIL cust-ident THEN 
            DO:
                RUN Insert_TTName("ВидЛиц", "нет").
                RUN Insert_TTName("ЛицДатаВыд", "нет").
                RUN Insert_TTName("ЛицДатаО", "нет").
                RUN Insert_TTName("ЛицНом", "нет").
                RUN Insert_TTName("ЛицВид", "нет").
                RUN Insert_TTName("ЛицКемВыд", "нет").
            END.       
        ELSE
            Do:    
                mSignsV = GetCodeName("ВидЛицДеят",cust-ident.cust-code-type).
                RUN Insert_TTName("ВидЛиц",mSignsV).
                  
                mSignsV = REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".").
                RUN Insert_TTName("ЛицДатаВыд", mSignsV ). 
                    
                mSignsV = IF cust-ident.close-date EQ ? THEN 'без ограничения срока действия' ELSE STRING(cust-ident.close-date).
                RUN Insert_TTName("ЛицДатаО",  mSignsV).  
                    
                mSignsV = cust-ident.cust-code.
                RUN Insert_TTName("ЛицНом",  mSignsV). 
                    
                mSignsV = GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"ЛицензОрг","").
                RUN Insert_TTName("ЛицВид",  mSignsV).
                    
                mSignsV = cust-ident.issue.
                RUN Insert_TTName("ЛицКемВыд",  mSignsV).
            end.
              
         mSignsVal = if GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОргУправ", "") NE "" THEN GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОргУправ", "") else "Нет".      
        RUN Insert_TTName("OrgUpr",mSignsVal).
        
          mStrTMP =  GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УставКапОплач",end-date, ""). 
        
        RUN Insert_TTName("УставКап", mStrTMP). /*переменная для последуюший проверки в стоке посто бенеф.*/
        
             /* учредитель из субъектов*/
        
        mSignsVal = ''.
        mSignsV = ''.
        mLog = TRUE.
        i = 1.
 
       FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   ((cust-role.class-code EQ 'Учредитель') or (cust-role.class-code EQ 'Акционер'))
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
                       
             NO-LOCK:
            {additem.i mSignsVal cust-role.cust-name}               
                 
       end.
       IF mSignsVal eq "" THEN            
           mSignsVal = if GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УчредОрг",end-date, "") NE "" THEN GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УчредОрг",end-date, "") else "Нет".
     
        RUN Insert_TTName("УчредОрг", mSignsVal).
               
        /*    IF mSignsVal EQ '' THEN DO:
            mTmp1 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"УчредОрг","").
            mTmp2 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"УставКап","0").
            mTmp3 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ФИОРук","").
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
                RUN Insert_TTName("BenifOsn", 'Прямое владение - наличие преобладающего участия более 25 процентов в капитале организации').
            ELSE
                RUN Insert_TTName("BenifOsn", 'Ввиду невозможности установить бенефициарного владельца и несоответствия критериям, перечисленным в 115-ФЗ, принято решение бенефициарным владельцем признать руководителя').
             
        /*из директор субъектов*/
        
         mSignsVal = ''.
         mSignsV = ''.
        mLog = TRUE.
        i = 1.

        FIND FIRST cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   cust-role.class-code Matches '*иректор*'
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
                                    "ДолРук",?).          
                    
            mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "ФИОРук",?).   
       
        END.
         RUN Insert_TTName("ДолРук", IF mSignsV eq "" THEN "нет" ELSE mSignsV ). 
         RUN Insert_TTName("ФИОРук", IF mSignsVal eq "" THEN "нет" ELSE mSignsVal ).
                 
          
 /****/
 
      RUN Insert_TTName("Bf_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))="Да" THEN "V" ELSE "").
        RUN Insert_TTName("Bf_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))="Нет" THEN "V" ELSE "").
                
        /* Ищем бенефициаров */
        mSignsVal = ''.
        mStrTMP = ''.
        mLog = TRUE.
        i = 1.
        Benif = ''.

        FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   cust-role.class-code EQ 'Бенефиц_Влад'
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
            NO-LOCK:

        {additem.i mSignsVal cust-role.cust-name}              
                                    
                
        FIND FIRST person WHERE
                  cust-role.cust-id EQ string(person.person-id)
            NO-LOCK NO-ERROR.
            
            InfSignsV = person.name-last + " " + person.first-names + " " + REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".") + " г.р., " .
            
            Benif =  trim(person.name-last + " " + person.first-names + " " +  Benif).              
            
             FIND country WHERE country.country-id EQ person.country-id NO-LOCK NO-ERROR.
                      IF AVAILABLE country THEN                       
                         mSignsV = country.country-name.
                      ELSE  
                         mSignsV = person.country-id. 
                              
                InfSignsV = InfSignsV +  trim(mSignsV) + ", гр-во " + trim(mSignsV).  
                
                    
                IF person.inn NE "" THEN InfSignsV = InfSignsV + ", ИНН " + person.inn.   
                 
                   InfSignsV2 = GetCodeName("КодДокум", person.document-id) .              
                                                  
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
                               
                InfSignsV2 = InfSignsV2 + ", выд." + REPLACE(mSignsV, "/", ".") + ", " + person.issue /*fGetDocIssue(person.person-id)*/ .   
               
        RUN RetAdr.p(person.person-id, "Ч", "АдрПроп", end-date, OUTPUT InfSignsV3).           
        
        RUN RetAdr.p(person.person-id, "Ч", "АдрФакт", end-date, OUTPUT mSignsV).                
           
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
                                    "ДоляУК",?) + " %".    
                                    
                                    
           
                 
            RUN Insert_TTName("BenFIO" + string(i),  InfSignsV).
            RUN Insert_TTName("BenDoc" + string(i), InfSignsV2).  
            RUN Insert_TTName("BenAdr" + string(i), InfSignsV3). 
            RUN Insert_TTName("BenCont" + string(i), IF mSignsVal eq "" THEN "Прямое владение - наличие преобладающего участия более 25 процентов в капитале организации" ELSE mSignsVal).
        
            i = i + 1.           
         END.
            RUN Insert_TTName("Benif",IF trim(Benif) NE '' THEN  Benif ELSE 'нет').      

        /*5*/
        RUN out_template_log("ank_third","tl_yes","tl_no").
        
       /*6*/
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_goal","") eq trim("Открытие и ведение банковского счета") then        
            RUN Insert_TTName("GOAL", "V").    
       else 
        Do:                   
            RUN Insert_TTName("GOAL_INOE", "V").
            RUN Insert_TTName("goal_in",  ": " + GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_goal","")).
        end. 
             
            mSignsV   = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_go5al","").
       
  /*     If mSignsV eq "Открытие и ведение банковского счета" THEN  RUN Insert_TTName("goal","V").
       
       if SUBSTRING(mSignsV, 1, 5) eq "Иное:" THEN 
       Do:  RUN Insert_TTName("goal_inoe","V"). 
            RUN Insert_TTName("goal_in",": " + SUBSTRING(mSignsV, 6, LENGTH (mSignsV))).
       END.            
    */                
                    
     /*7*/  
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoChar"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            mTarget = trim(ENTRY(1,cust-ident.cust-code,mSymbol)).
           /* mTargetDetails = GetCodeDesc("ДелОтнКл",mTarget,1,"").    
                     
            MESSAGE     mTarget                        VIEW-AS ALERT-BOX .  */ 
        CASE mTarget:
            WHEN "БезналРасч"         THEN RUN Insert_TTName("d_bn", "V"). 
            WHEN "РазмещДенСредствД"  THEN RUN Insert_TTName("d_rd", "V").
            WHEN "КассОбслуж"         THEN RUN Insert_TTName("d_ko", "V").
            WHEN "ИнкассДенСред"      THEN RUN Insert_TTName("d_id", "V").
            WHEN "Кредитование"       THEN RUN Insert_TTName("d_kr", "V").
            WHEN "РасчМеждунар"       THEN do: RUN Insert_TTName("d_mr", "V").   RUN out_template_log("ank_inter_pay","d_mr_yes","d_mr_no"). end.
            WHEN "Иное"               THEN 
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
        
        
      /* 8.1  НАДО ИКАТЬ ТОЛЬКО ПЕРВЫЙ????? ПОТОМ ПОПРАВЬ */
            
         mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoOper"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        AND ENTRY(1,cust-ident.cust-code,mSymbol) eq "Планируемый" 
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
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoVCont"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date      
        NO-LOCK:         
        
        mTarget    = GetXAttrValueEx("cust-ident",
                                STRING(cust-ident.cust-code-type) + ',' +
                                STRING(cust-ident.cust-code) + ',' +
                                STRING(cust-ident.cust-type-num),
                                "ВидДог","").
           
           CASE mTarget:
            WHEN "Аренда"               THEN RUN Insert_TTName("vid_a", "V"). 
            WHEN "ПоручКомисАгент"      THEN RUN Insert_TTName("vid_k","V").
            WHEN "ПокупкаПродажа"       THEN RUN Insert_TTName("vid_p", "V").
            WHEN "Услуги"               THEN RUN Insert_TTName("vid_ser", "V").           
            WHEN "Иное"                 THEN Do: 
                                                RUN Insert_TTName("vid_inoe", "V"). 
                                                mSignsV = entry(2,GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";").
                                                RUN Insert_TTName("vid_in",": " + mSignsV). 
                                             END.
        END CASE.
        end.  
       
        /* 8.3 */
        mSignsV ="".
        mSignsVal ="".       
      
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
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
                mSignVL = "Нет".
            mSignsVal       =  mSignsVal  + mSignVL + chr(13).    
        end.
        RUN Insert_TTName("partner",  mSignsV).
        RUN Insert_TTName("partner_inn", mSignsVal). 
        
        /*9*/
                               
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoPos"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            
            mTarget = trim(ENTRY(1,cust-ident.issue,mSymbol)).                                   
        CASE mTarget:            
            WHEN "КГодБухОт"            THEN RUN Insert_TTName("fp_bb", "V").
            WHEN "КНалДекл"             THEN RUN Insert_TTName("fp_nd", "V").
            WHEN "СпрОбУпл"             THEN RUN Insert_TTName("fp_spr","V").
            WHEN "КАудитЗакл"           THEN RUN Insert_TTName("fp_az", "V"). 
        END CASE.
        end.  
                       
  
         RUN out_template_log("ank_bankrupt","b_yes","b_no").
         RUN out_template_log("ank_legal_proc","s_yes","s_no").
         RUN out_template_log("ank_liquidation","l_yes","l_no").
         RUN out_template_log("ank_nonperformance","nonperf_yes","nonperf_no").   
                   
       /*10*/  
         
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id
        and cust-ident.cust-code-type eq ""   /*класс не верный, наименование не заполняется, а берет пусто*/
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK:    
            mSignsV = ENTRY(1,cust-ident.cust-code,mSymbol).        
        CASE mSignsV:
            WHEN "ОснДеят"         THEN RUN Insert_TTName("doh_o", "V"). 
            WHEN "ДопДеят"         THEN RUN Insert_TTName("doh_d", "V").
            WHEN "ПрочДеят"        THEN RUN Insert_TTName("doh_p", "V").           
            WHEN "Иное"            THEN Do: RUN Insert_TTName("doh_inoe", "V"). RUN Insert_TTName("doh_in",entry(3, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.
        end.    
                         
         /*11*/       
                         
   FIND FIRST cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        and ENTRY(1,cust-ident.issue,mSymbol) eq "ДругиеКрОрг" 
        NO-LOCK NO-ERROR.
   
   IF AVAIL(cust-ident) THEN 
         RUN Insert_TTName("o_yes", "V"). 
   else  RUN Insert_TTName("o_no", "V").   
   
      FIND FIRST cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date
        and ENTRY(1,cust-ident.issue,mSymbol) eq "ДругиеКлиенты" 
        NO-LOCK NO-ERROR.  
   IF AVAIL(cust-ident) THEN 
         RUN Insert_TTName("o_cl_yes", "V"). 
   else  RUN Insert_TTName("o_cl_no", "V"). 
   
       mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id 
        and cust-ident.cust-code-type eq "FinInfoRep"
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK: 
            mSignsV = trim(ENTRY(1,cust-ident.issue,mSymbol)).                     
        CASE mSignsV:
            WHEN "УчастОбъедСоюзе"          THEN RUN Insert_TTName("part_1", "V"). 
            WHEN "УчастОбществДеят"         THEN RUN Insert_TTName("part_2", "V").
            WHEN "УчастБлаготв"             THEN RUN Insert_TTName("part_3", "V").
            WHEN "Иное"                     THEN Do: RUN Insert_TTName("part_4", "V"). RUN Insert_TTName("part_in",entry(4, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.
        end.

      mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_lifetime", "").
        CASE mSignsVal:   
            WHEN "Более  5 лет"         THEN RUN Insert_TTName("srok_1", "V"). 
            WHEN "От 1 до 5 лет"        THEN RUN Insert_TTName("srok_2", "V").
            WHEN "От 6 до 12 месяцев"   THEN RUN Insert_TTName("srok_3", "V").  
            WHEN "От 3 до 6 месяцев"    THEN RUN Insert_TTName("srok_4", "V").
            WHEN "Менее 3 месяцев"      THEN RUN Insert_TTName("srok_5", "V").         
        END CASE.    
        
      mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_litigation", "").
        CASE mSignsVal:   
            WHEN "Да"         THEN RUN Insert_TTName("sud_y", "V"). 
            WHEN "Нет"        THEN RUN Insert_TTName("sud_n", "V").
            OTHERWISE         Do:  RUN Insert_TTName("sud_i", "V").  
                                   RUN Insert_TTName("sud_in",  " " + mSignsVal). 
                              end.
        END CASE.             
                 
        mSignsV ="".
        FOR EACH  cust-ident WHERE cust-ident.cust-cat eq "Ю"
        and cust-ident.cust-id        eq cust-corp.cust-id
        and cust-ident.cust-code-type eq "FinInfoRep"   /*класс не верный, наименование не заполняется, а берет пусто*/
        and cust-ident.close-date     EQ ? 
        and cust-ident.open-date      <= end-date 
        NO-LOCK:    
              /*  MESSAGE   cust-ident.cust-code-type                   VIEW-AS ALERT-BOX .*/
            
           mSignsV = ENTRY(1,cust-ident.cust-code,mSymbol). 
        
          /*     MESSAGE   cust-ident.cust-code-type                   VIEW-AS ALERT-BOX .*/     
        CASE mSignsV:
            WHEN "ОснДеят"         THEN RUN Insert_TTName("doh_o", "V"). 
            WHEN "ДопДеят"         THEN RUN Insert_TTName("doh_d", "V").
            WHEN "ПрочДеят"        THEN RUN Insert_TTName("doh_p", "V").           
            WHEN "Иное"            THEN Do: RUN Insert_TTName("doh_inoe", "V"). RUN Insert_TTName("doh_in",entry(3, ": " + GetXAttrValueEx('cust-corp',STRING(cust-corp.cust-id),"ank_other",""),";")). END.
        END CASE.        
        
        end.           
               
        /*12*/
           
     mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ank_term_rent", "").
        CASE mSignsVal:   
            WHEN "Менее 3 мес"         THEN RUN Insert_TTName("srok_a", "V"). 
            WHEN "От 3 до 6 мес"         THEN RUN Insert_TTName("srok_a3", "V").
            WHEN "Более 6 мес"        THEN RUN Insert_TTName("srok_a6", "V").           
        END CASE.
                 
      /*13*/
      
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_glbuh","") eq "Да" THEN   mSignsV   = "Да". 
       If GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ank_glbuh","") eq "Нет" THEN  mSignsV   = "Нет". 
                      
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
         
        mSignsV=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","").     
            RUN Insert_TTName("RiskDa", IF CAN-DO("*высокий*,*большой*", mSignsV) THEN "V" ELSE "").
            RUN Insert_TTName("RiskNet", IF CAN-DO("*низкий*", mSignsV) /*or (mSignsV EQ "") */ THEN "V" ELSE "").
            
                                  
        RUN Insert_TTName("ОценкаРиска", IF GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОценкаРиска","")="" THEN "" ELSE
        GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОценкаРиска","")).
                
         /*16*/
                  
       mSignsV = REPLACE(GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ДатаПровТерр",end-date,""),"/",".").  
                                  
        RUN Insert_TTName("DP", IF mSignsV = "" THEN "нет" ELSE mSignsV).   
        
            mSignsV =  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))="" THEN "Не причастен" ELSE          
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
           mSignsV =   "Вешкурцева" else _User._User-Name.
              mStatus = GetXAttrValueEx("_user", _User._Userid, "Должность", "").
             END.  
            */ 
            mSignsVal = trim(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id","")). 
      
            IF   CAN-DO("0300", mSignsVal) THEN     
            Do: 
            mSignsV = 'Вешкурцева А.А.'.
            mStatus = "Начальник операционного отдела ТФ".
            end.
            else
            IF   CAN-DO("0100,0500", mSignsVal) THEN 
            Do: 
            mSignsV = "Лашина Е.К. ".
            mStatus = "Руководитель направления РКО ОФ".
            end. else
            IF   CAN-DO("0301,0302", mSignsVal) THEN 
            Do: 
            mSignsV = "Михайлова М.А.".
            mStatus = "Старший операционист-кассир ОО" + "'" + "Уральский" + "'".
            end. else
            IF   CAN-DO("0400,0401,0402,0403,0404,0405,0406,0407,0408,0409,0000,0101,0102,0106,0109,0110", mSignsVal) THEN /*если написать 04 отберет и 0504*/
            Do: 
            mSignsV = "Маусумбаева Г.К. ".
            mStatus = "Руководитель направления РКО по г. Москва".
            end.                      
            else 
            IF   CAN-DO("0516,0517", mSignsVal) THEN 
            Do: 
            mSignsV =  "Грязнова М.Н.".
            mStatus = "Старший операционист-кассир ОО" + "'" + "Восход" + "'".
            end.                      
            else 
            Do: 
            mSignsV = "филиал неопределен".
            mStatus = "Филиал неопределен".
            end. 
             
              RUN Insert_TTName("_Kur-Name", mSignsV).       
              RUN Insert_TTName("Кур_Должн", mStatus).   
               
         
        FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
                      AND   acct.cust-cat EQ 'Ю'
            NO-LOCK BY acct.open-date:
            LEAVE.
        END.
        IF AVAIL acct THEN DO:
            mSignsVal = {strdate.i acct.open-date}.
            mSignsV = STRING(acct.open-date, "99.99.9999") + " г.".
            CREATE ttOtchData.   
            ASSIGN ttOtchData.ttDate = DATE(acct.open-date).
        END.
        ELSE
            DO:
            mSignsVal = {strdate.i cust-corp.date-in}.
            mSignsV = "нет".
            END.
        RUN Insert_TTName("ДатаЗаполАнкеты",REPLACE(mSignsVal,'г.','года')).
        RUN Insert_TTName("Startdate",mSignsV).
        
        mSignsV = STRING(cust-corp.date-out, "99.99.9999").                                   
        RUN Insert_TTName("Enddate", IF mSignsV = ? THEN "нет" ELSE mSignsV + " г.").                         
       
       /* подвал*/
         
        FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
        IF AVAIL(_User) THEN 
        DO:
          RUN Insert_TTName("Prn_Name",_User._User-Name). 
          mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
          RUN Insert_TTName("Prn_Dolg", mStatus). 
        END.
           
        mSignsV = REPLACE(STRING(cust-corp.date-in, "99/99/9999"), "/", ".").

        RUN Insert_TTName("DatePrint", mSignsV + " года").    
        
        mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id), end-date), " ", ",").
        mSignsV = IF (mSignsV eq "") or (mSignsV eq ?) THEN STRING(cust-corp.date-in, "99/99/9999") ELSE  mSignsV.
        mSignsV = REPLACE(mSignsV, "/", ".") + " года".
        RUN Insert_TTName("ДатаОбнАнкеты", mSignsV).         
        
/***Для старых анкет Начало**/

RUN Insert_TTName("Bf_Da",  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))="Да" THEN "V" ELSE "").
RUN Insert_TTName("Bf_Net", IF(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))="Нет" THEN "V" ELSE "").

/***Для старых анкет Конец**/               
         
If  end-date LE date("16/08/2015") THEN /*до*/
    iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1)  .
            
If (end-date GE date("17/08/2015"))  and  (end-date LE date("26/12/2015"))  THEN /*включительно*/       
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
           
            /* Вывод данных по шаблону iParms (до "|") в файл отчета */ 
          RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).  
        {intrface.del comm}

/*Процедеура заполнения Да/Нет полей*/
PROCEDURE out_template_log :   
  def INPUT PARAMETER dr_tabl                    AS CHARACTER NO-UNDO. 
  def INPUT PARAMETER field_temp1                AS CHARACTER NO-UNDO. 
  def INPUT PARAMETER field_temp2                AS CHARACTER NO-UNDO.  

 CASE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), dr_tabl, ""):
            WHEN "Да"           THEN RUN Insert_TTName(field_temp1, "V"). 
            WHEN "Нет"          THEN RUN Insert_TTName(field_temp2, "V").
         END CASE.      
END PROCEDURE.
