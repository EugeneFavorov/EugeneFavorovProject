/*
               Банковская интегрированная система БИСквитpDP
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: pac_doc.p
      Comment: Пакет документов при открытие расч.счета
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 17/12/2010 kraa (0120008)
     Modified: 
*/
{globals.i}                                 /* глобальные переменные         */
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
                                                                                      
DEFINE VARIABLE tmprecid                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mStatus                 AS CHARACTER NO-UNDO.                                                     
DEFINE VARIABLE mcountr                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mcountr1                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMestReg                AS CHARACTER NO-UNDO. /*RegPlace*/
DEFINE VARIABLE mDatTMP                 AS DATE      NO-UNDO.      /*RegDate*/
DEFINE VARIABLE str                     AS CHARACTER NO-UNDO. /**/
DEFINE VARIABLE mAdrReg                 AS CHARACTER NO-UNDO.  /*АдрЮр*/
DEFINE VARIABLE mAdrFact                AS CHARACTER NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mDRuk                   AS CHARACTER NO-UNDO. /*ДолРук*/
DEFINE VARIABLE mNRuk                   AS CHARACTER EXTENT 2 NO-UNDO. /*ФИОрук*/
DEFINE VARIABLE mIssue                  AS CHARACTER EXTENT 2 NO-UNDO. /*Паспорт кем выдан*/
DEFINE VARIABLE mDateBeg                AS DATE      NO-UNDO.   /*???*/   
DEFINE VARIABLE mAcct                   AS CHARACTER EXTENT 2 NO-UNDO.   /*acct*/
DEFINE VARIABLE mCID                    AS CHARACTER NO-UNDO. /*CID*/
DEFINE VARIABLE mCIDIP                  AS CHARACTER NO-UNDO.   /*doc-num*/   /*doc-ref*/
DEFINE VARIABLE mInn                    AS CHARACTER NO-UNDO.     /* Ю Ч*/
DEFINE VARIABLE mCustId                 AS INT64     NO-UNDO.      /* cust-id*/
DEFINE VARIABLE mName                   AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE Ofname                  AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE RetString               AS CHARACTER NO-UNDO.   
DEFINE VARIABLE mcur1                   AS CHARACTER NO-UNDO.   
DEFINE VARIABLE mcur2                   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mcur3                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE InfSignsV               AS CHARACTER NO-UNDO.
DEFINE VARIABLE InfSignsV2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE InfSignsV3              AS CHARACTER NO-UNDO.
DEFINE VARIABLE vPhone                  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNBuh1                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNBuh3                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNBuh2                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNBuh4                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE i                       AS int64     NO-UNDO.
DEFINE VARIABLE id                       AS int64     NO-UNDO.


DEF VAR mSignsVal AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignVL  AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignsV AS CHAR NO-UNDO. /* Значение реквизита из person */
DEF VAR mSignVL1 AS CHAR NO-UNDO. 

DEF VAR VidOrg AS CHAR NO-UNDO. /*Выдавший орган*/

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p     AS RECID.

DEF TEMP-TABLE ttOtchData NO-UNDO
   FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* табл с индексом чтоб не морочится с сортировкой*/

DEF TEMP-TABLE tt-dover NO-UNDO
   FIELD pers-id    AS int64
   FIELD FIO        AS CHAR
   FIELD docs_d     AS DATE
   FIELD docdate_d  AS DATE
   FIELD docn_d     AS CHAR
   FIELD docv_d     AS CHAR
   FIELD issue      AS CHAR
INDEX FIO FIO.               /* табл с доверенными */
   

DEF BUFFER b-person FOR person. 

FUNCTION GetDateTemp RETURN CHAR (
   INPUT iN-FileName AS CHAR,
   INPUT iN-Surr     AS CHAR,   
   INPUT iN-Date     AS DATE,   
   INPUT iDate       AS DATE,
   INPUT iSense      AS LOG  
):
   DEF VAR vValue    AS CHAR   NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO. /* Значение ДР. */  
   
   EMPTY TEMP-TABLE ttOtchData.
   EMPTY TEMP-TABLE tt-dover.
   
    /*для заполнения террористов с даты*/
         
    IF iSense THEN
    DO:    
        FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
            AND tmpsigns.code = "ДатаОбнАнкеты" 
            AND tmpsigns.surrogate  = in-Surr          
            AND tmpsigns.since LE in-Date      
            AND tmpsigns.since GE date("27/12/2015")  /* то что было до нас не интересует **/                     
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
    
    FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "ДатаПровТерр")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
        AND tmpsigns.since GE iDate /*date("07/09/2016")   то что было до нас не интересует **/                     
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
         
  /*  CREATE ttOtchData.   
   ASSIGN ttOtchData.ttDate = cust-corp.date-in.  
     MESSAGE cust-corp.date-in VIEW-AS ALERT-BOX . */
    
    FOR EACH ttOtchData  NO-LOCK: 
        vResult =   vResult +  " "  + REPLACE(STRING(ttOtchData.ttDate,"99/99/9999"), "/", ".").
    END.

    RETURN trim(vResult).
    
{empty ttOtchData} 
END FUNCTION.   

/* выбор подписантов */
IF CAN-DO("zay_dkbo*",iParms) THEN 
DO:
    {sign_select.i} 
END.

IF NOT CAN-DO('zay_dkbo,zay_kb|,ZayvZakrBV|', iParms) THEN
DO:
    {getdate.i &DateLabel = "Дата печати"  
               &DateHelp  = "Укажите дату печати анкеты"  }     
END.          
                 
FIND FIRST tmprecid NO-LOCK NO-ERROR.
                      
/* АНКЕТА представителя клиента*/                       
If CAN-DO('ankpred*', iParms) Then 
do:
    /* если запущено из юр лиц */
    
    If CAN-DO('ankpredU*', iParms) Then
    DO:
        FIND FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK NO-ERROR.
        IF AVAIL cust-corp THEN 
        do:
            
            mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id),end-date,date("07/09/2016"),No), " ", ",").  
                                  
            RUN Insert_TTName("DP", IF mSignsV = "" THEN "нет" ELSE mSignsV). 
            
            mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id), end-date,date("27/12/2015"),Yes ), " ", ",").            
            RUN Insert_TTName("ДатаОбнАнкеты", IF mSignsV = "" THEN "нет" ELSE  REPLACE(mSignsV, "/", ".") + " года").   
            
            mSignVL  = trim(cust-corp.cust-stat + " " + cust-corp.name-corp + " " + GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"engl-name","")).  
            id = cust-corp.cust-id.        
            mSignsVal = cust-corp.inn.
            iParms = SUBSTRING(iParms,1,7) + SUBSTRING(iParms,9).     
       
            /*поиск и заполнение таблицы и строки меню из связ.суб.  */
              
            FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                /* AND   cust-role.class-code EQ 'Бенефиц_Влад'*/
                AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                AND   cust-role.open-date LE end-date 
                NO-LOCK:
                FIND FIRST b-person WHERE
                    cust-role.cust-id EQ string(b-person.person-id)
                    NO-LOCK NO-ERROR.
       
                IF AVAILABLE b-person THEN  
                    IF  index(trim(mSignVL1),trim (b-person.name-last + " " + b-person.first-names )) eq 0  Then  
                    Do: 
                        If  mSignVL1 ne "" then    mSignVL1 =  mSignVL1 + ",".         
                        mSignVL1 =  mSignVL1 +  String(b-person.name-last + " " + b-person.first-names ).
                                                                                 
                        CREATE tt-dover.   
                        ASSIGN 
                            tt-dover.pers-id   = b-person.person-id
                            tt-dover.FIO       = String( b-person.name-last + " " + b-person.first-names )                    
                            tt-dover.docdate_d = cust-role.open-date
                            tt-dover.docs_d    = cust-role.close-date
                            tt-dover.docn_d    = ""
                            tt-dover.docv_d    = ""    
                            tt-dover.issue     = ""                          
                             .  
                        i = i + 1.                    
                    end.  
            end.           
       
            mSignsV = REPLACE(STRING(cust-corp.date-in, "99/99/9999"), "/", ".").
            RUN Insert_TTName("DatePrint", mSignsV + " года").      
        end.
    END.
    Else
    DO:
        FIND FIRST person WHERE RECID(person) EQ tmprecid.id NO-LOCK NO-ERROR. 
        IF AVAILABLE person THEN  
            do: 
            mSignsV = String( person.name-last + " " + person.first-names ).
            mSignsVal = GetCodeName("КодДокум", person.document-id) + " " + person.document + " "     
            + fGetDocIssue(person.person-id)  +  (If person.inn ne '' then chr(13)+  person.inn else '') . 
            id = person.person-id. 
            end.
            mSignVL = mSignsV.
    END.
   
    RUN Insert_TTName("fio_pers_p",mSignVL).
    RUN Insert_TTName("document_p",mSignsVal).  
    
      FOR EACH LOAN 
        WHERE LOAN.CONTRACT EQ "proxy" 
        AND LOAN.cust-id = id 
        AND (loan-status eq 'НАЧ' or loan-status eq CHR(251) or loan-status eq 'Ф') 
        
             
        AND LOAN.open-date LE end-date
        AND ((LOAN.close-date GE end-date ) or (LOAN.end-date GE end-date) )
        NO-LOCK:          
                       
        i = i + 1.   
        mSignsV = getxattrvalue ("loan",string("proxy," + loan.cont-code), "agent-id").
       
        Case   loan.cont-type:
            WHEN '3.5' THEN                  
                VidOrg =  mSignVL. 
            WHEN '2.1'  OR 
            WHEN '2.2' THEN 
                VidOrg =  'Нотариус'.
            OTHERWISE
           /* VidOrg = loan.cont-type.*/
              VidOrg = GetCodeName("ДОВЕР_Тип", loan.cont-type).          
        END CASE.
        
       
        
        FIND FIRST b-person WHERE b-person.person-id EQ  int64(mSignsV)  NO-LOCK NO-ERROR.   
         
        FIND FIRST tt-dover WHERE (tt-dover.pers-id  EQ  int64(mSignsV))   NO-LOCK NO-ERROR.   
        
        IF not AVAILABLE tt-dover THEN
        do:
            If  mSignVL1 ne "" then    mSignVL1 =  mSignVL1 + ",".         
            mSignVL1 =  mSignVL1 +  String(b-person.name-last + " " + b-person.first-names ).           
                               
            CREATE tt-dover.           
        end.     
        ASSIGN 
            tt-dover.pers-id   = int64(getxattrvalue ("loan",string("proxy," + loan.cont-code), "agent-id"))
            tt-dover.FIO       = String(b-person.name-last + " " + b-person.first-names )                    
            tt-dover.docdate_d = loan.open-date
            tt-dover.docs_d    = loan.end-date
            tt-dover.docn_d    = loan.doc-num 
            tt-dover.docv_d    = VidOrg  
            tt-dover.issue     = 'Доверенность'
            .
    end.
              
    IF i eq 0 THEN 
    DO: 
        MESSAGE "У клиента нет доверенных лиц, печать анкеты невозможна" VIEW-AS ALERT-BOX .
        return.
    end.
    else  
    do:
        if i>1 THEN
        do:            
            RUN messmenu.p(10,"","",mSignVL1).          
            IF LASTKEY = 27 THEN RETURN NO-APPLY.           
            mSignsV  = ENTRY(int64(pick-value),mSignVL1,',').          
 
            FIND FIRST tt-dover WHERE mSignsV MATCHES tt-dover.FIO  NO-LOCK NO-ERROR.
       end. /* 'Доверенность'   */         
                         
        if tt-dover.issue ne 'Доверенность' then 
            do:
                tt-dover.docdate_d = ?.
                tt-dover.docs_d = ?.
            end.      
         
        RUN Insert_TTName("doc_d", tt-dover.issue).
        RUN Insert_TTName("docdate_d", REPLACE(string(tt-dover.docdate_d), "/", ".")) .
        RUN Insert_TTName("docs_d",  REPLACE(string(tt-dover.docs_d), "/", ".")). 
        RUN Insert_TTName("docn_d", tt-dover.docn_d ).
        RUN Insert_TTName("docv_d", tt-dover.docv_d ).
        
        FIND FIRST person WHERE person.person-id EQ tt-dover.pers-id  NO-LOCK NO-ERROR.   
            
    end.
    
     If end-date LE date("14/06/2017") THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
     else        
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "2" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
   
    
end.     
else                      
 FIND FIRST person WHERE RECID(person) EQ tmprecid.id NO-LOCK NO-ERROR.
   	
FIND FIRST acct WHERE  person.person-id EQ acct.cust-id  NO-LOCK NO-ERROR. 
IF AVAILABLE acct THEN                     
    RUN Insert_TTName("acct",   STRING(acct.acct, "x(25)")).  
ELSE
    RUN Insert_TTName("acct", "нет").  
 
mSignsV = REPLACE(STRING(person.date-in, "99/99/9999"), "/", ".").

RUN Insert_TTName("DatePrint", mSignsV + " года").  
   
mSignsV = ''.    
        
IF GetXAttrValueEx("person",STRING(person.person-id), "Субъект","") = "ФЛП" THEN 
DO:
    FOR EACH acct WHERE acct.cust-id  EQ person.person-id
        AND acct.cust-cat EQ 'Ч'
        AND acct.contract EQ 'Расчет' 
        USE-INDEX open-date
    NO-LOCK BY acct.open-date:
       /* LEAVE. */
      
      mSignsV = {strdate.i acct.open-date}.
        IF  acct.close-date EQ ? 
            AND acct.branch-id  EQ shFilial THEN
        DO:
            str = str + "," + acct.number.
        END.
    END.        
    
    str = TRIM(str, ",").
    IF str = "" THEN 
        str = "-".
    IF NUM-ENTRIES(str) > 1 THEN 
        str = "".
    RUN Insert_TTName ("AccRasch",str).
    
IF mSignsV eq '' THEN  mSignsV = "нет".
 
END. 
else 
DO:  
FIND first acct WHERE acct.cust-id  EQ person.person-id
        AND acct.cust-cat EQ 'Ч'
        AND can-do('Расчет,Депоз,Текущ',acct.contract)
       
         USE-INDEX open-date          NO-LOCK  NO-ERROR.
     IF AVAILABLE acct THEN
      mSignsV = {strdate.i acct.open-date}.     
    else   
    mSignsV = REPLACE(STRING(person.date-in, "99/99/9999"), "/", ".").    

end.

RUN Insert_TTName("open-date", mSignsV).
 
mSignsV = GetXAttrValueEx("person",
    STRING(person.person-id),
    "ФИОРук",?).   
RUN Insert_TTName("ФИОРук", mSignsV).

RUN Insert_TTName("ДолРук", "Индивидуальный предприниматель").

mSignsVal = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "основа",
                            ?).
RUN Insert_TTName("основа", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).

mSignsV = GetXAttrValueEx("person",
                          STRING(person.person-id),
                          "МестСведПред", 
                          "").
/* RUN Insert_TTName("МестСведПред", mSignsV).*/
RUN Insert_TTName("МестСведПред", IF mSignsV NE "" THEN mSignsV  ELSE "нет" ).

mSignsVal = GetXAttrValueEx("person", 
                            STRING(person.person-id), 
                            "RegPlace", 
                            "").
IF mSignsVal NE "" THEN 
   RUN Insert_TTName("RegPlace", mSignsVal). 
ELSE 
DO:
   mSignsVal = GetXAttrValueEx("person", 
                               STRING(person.person-id), 
                               "МестСведПред", 
                               "").
   RUN Insert_TTName("RegPlace", mSignsVal).
END.
                
                                              
mSignsV = GetXAttrValueEx("person",
    STRING(person.person-id),
    "ОргСведПред", 
    "").
/* RUN Insert_TTName("ОргСведПред", mSignsV). */
RUN Insert_TTName("ОргСведПред", IF mSignsV NE "" THEN mSignsV  ELSE "нет" ).
                   
mSignsV = GetXAttrValueEx("person",
    STRING(person.person-id),
    "CID",?).
RUN Insert_TTName("CID", mSignsV).
                
mNBuh1 = GetXAttrValueEx("person", 
    STRING(person.person-id), 
    "фиобухг", 
    "").
                                                          
IF  mNBuh1 ="" THEN
    mNBuh3="Лицо,   наделенное правом второй подписи, отсутствует".
ELSE
    mNBuh3=mNBuh1. 
                    
RUN Insert_TTName("ФИОбухг", mNBuh3).                                
mNBuh2 = "" . /* GetXAttrValueEx("cust-corp", 
                             STRING(cust-corp.cust-id), 
                             "фиобухг", 
                             ""). */
IF  mNBuh2 ="" THEN
    mNBuh4="Нет".
ELSE
    mNBuh4=mNBuh2. 
                    
RUN Insert_TTName("ФИОбг", mNBuh4).                                
        
mSignsV = GetXAttrValueEx("person",
    STRING(person.person-id),
    "ОГРН", 
    "").             
RUN Insert_TTName("ОГРН", IF mSignsV NE "" THEN mSignsV  ELSE "нет" ).
                
mDatTMP = DATE(GetXAttrValueEx("person",
    STRING(person.person-id), 
    "ДатаОГРН", 
    "")) NO-ERROR.
RUN Insert_TTName("RegDate", IF mSignsV NE "" THEN REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") ELSE "нет" ).
                 
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
                
/*RUN Insert_TTName("document-id",person.document-id).  Pasport  */
                                 
InfSignsV2 = GetCodeName("КодДокум", person.document-id) .              
              
RUN Insert_TTName("Pasport",InfSignsV2). 

mSignsV = GetXAttrValueEx("person", STRING(person.person-id), "groupOABS", "").
RUN Insert_TTName("groupOABS", IF shFilial = "0500" THEN mSignsV ELSE "").

RUN Insert_TTName("CIDbis", STRING(person.person-id)).
              
/**********/

mSignsVal = (GetXAttrValueEx("person",STRING(person.person-id), "e-mail","")).
            
        RUN RetAdr.p(person.person-id,  "Ч", "АдрУвед",end-date ,OUTPUT mAdrReg).
        If (mAdrReg ne "") and (mSignsVal ne "")  THEN mSignsVal = mSignsVal + "; ".
        mSignsVal = mSignsVal + mAdrReg. 
        
        IF mSignsVal="" THEN mSignsVal= "нет".  
        RUN Insert_TTName("e-mail", mSignsVal).
              
RUN Insert_TTName("ИНН",IF {assigned person.inn} THEN person.inn ELSE "нет").               
              
RUN Insert_TTName("mIssue",fGetDocIssue(person.person-id) ).                         /* person.issueкем выдан*/
                                            
IF NUM-ENTRIES(person.Issue,",") LT 2 THEN 
DO:
    RUN Insert_TTName("mIssue1", person.Issue).
    RUN Insert_TTName("mIssue2","нет").
END.
ELSE 
DO:
    RUN Insert_TTName("mIssue1",ENTRY(1,person.Issue,",")).
    RUN Insert_TTName("mIssue2",ENTRY(2,person.Issue,",")).
END.          
/*           
            
FOR FIRST cust-ident
      WHERE cust-ident.class-code     EQ "p-cust-ident"
       AND (cust-ident.close-date     EQ ?
        OR  cust-ident.close-date     GT gend-date)
       AND  cust-ident.cust-cat       EQ bCustRole.cust-cat
      
       AND  cust-ident.cust-code-type EQ ""
  NO-LOCK
               
select cust-code, cust-code-type
from  cust-ident
where cust-ident.class-code     = "p-cust-ident"
        AND  cust-ident.cust-id     = 21437*/
            
find first cust-ident where cust-ident.cust-code-type = "МиграцКарта" 
    AND cust-ident.class-code     = "p-cust-ident"
    and (cust-ident.close-date     EQ ?
    OR  cust-ident.close-date     GT end-date)
    AND  cust-ident.cust-cat       EQ "Ч" 
    AND  cust-ident.cust-id        EQ person.person-id
    no-lock no-error.     
      
IF AVAILABLE(cust-ident) THEN
     
DO:           
    mSignsV =  REPLACE(GetXattrValue("cust-ident", cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING (cust-ident.cust-type-num),"end-date"), "/", "."). 
    RUN Insert_TTName("MigrCard",IF cust-ident.cust-code eq "" THEN "нет" ELSE cust-ident.cust-code).
    RUN Insert_TTName("MigrS", IF STRING(cust-ident.open-date, "99/99/9999") eq "" THEN "нет" ELSE  REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".")).
    RUN Insert_TTName("MigrPo",IF mSignsV eq "" THEN "нет" ELSE mSignsV ).
END.
      
ELSE
Do:           
            
RUN Insert_TTName("MigrCard", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрКарт",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрКарт",""))). 
               
RUN Insert_TTName("MigrS", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывС",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывС",""))). 

RUN Insert_TTName("MigrPo", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывПо",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывПо",""))). 

END.

find last cust-ident where CAN-DO("Виза,ВидНаЖит,РазрБезГражд,ИнойДокПрож,Патент",cust-ident.cust-code-type)   
    AND cust-ident.class-code     = "p-cust-ident"
    and (cust-ident.close-date     EQ ?
    OR  cust-ident.close-date     GT end-date)
    AND  cust-ident.cust-cat       EQ "Ч" 
    AND  cust-ident.cust-id        EQ person.person-id
    no-lock no-error.          
  
IF AVAILABLE(cust-ident) THEN

DO:  
      
    RUN Insert_TTName("VisaType", GetCodeName( "КодДокум" ,cust-ident.cust-code-type)).
    mSignsV =  REPLACE(GetXattrValue("cust-ident", cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING (cust-ident.cust-type-num),"end-date"), "/", ".").           
      
    if  NUM-ENTRIES(trim(cust-ident.cust-code)," ") GT 1   then
    do:    
        RUN Insert_TTName("Visa1",ENTRY(1,cust-ident.cust-code,' ')). 
        RUN Insert_TTName("Visa2",ENTRY(2,cust-ident.cust-code,' ')). 
    end.
    else  
    do:            
        RUN Insert_TTName("Visa1",'нет'). 
        RUN Insert_TTName("Visa2",IF string(cust-ident.cust-code) eq "" THEN "нет" ELSE string(cust-ident.cust-code)). 
    end.    
          
    RUN Insert_TTName("VisaS",REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".")).
    RUN Insert_TTName("VisaPo",IF mSignsV eq "" THEN "нет" ELSE mSignsV ).
END.

ELSE

do:
    
    RUN Insert_TTName("VisaType", IF (GetXAttrValueEx("person",STRING(person.person-id), "VisaType",""))="" THEN "нет" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "VisaType",""))).  

    mSignsV=GetXAttrValueEx("person",STRING(person.person-id), "VisaNum","").
    IF mSignsV="" THEN 
    DO:
        RUN Insert_TTName("Visa1","нет").
        RUN Insert_TTName("Visa2","нет").
    END.
    ELSE 
    DO:
        RUN Insert_TTName("Visa1",ENTRY(1,mSignsV)). 
        RUN Insert_TTName("Visa2",ENTRY(2,mSignsV)). 
    END.
         
    RUN Insert_TTName("VisaS", "нет" ).
    RUN Insert_TTName("VisaPo","нет" ).    
   
end.  

  RUN Insert_TTName("IPDL_STATUS", IF (GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ",""))="" THEN "нет" ELSE "да").


/*mSignsV = (GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв","").*/

If (SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) eq "ankip") and GetXAttrValueEx("person",STRING(person.person-id), "РискОтмывИП","") ne ""  Then 
mSignsV=GetXAttrValueEx("person",STRING(person.person-id), "РискОтмывИП","").
else
mSignsV=GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв","").
/* IF mSignsV="" THEN DO:
         RUN Insert_TTName("RiskDa","").
         RUN Insert_TTName("RiskNet", "").
 END.
 ELSE DO: */       
RUN Insert_TTName("RiskDa", IF CAN-DO("*высокий*,*большой*", mSignsV) THEN "Х" ELSE "").
                RUN Insert_TTName("RiskNet", IF (CAN-DO("низкий", mSignsV)) or (mSignsV="") THEN "Х" ELSE "").
                   /*END.*/

                RUN Insert_TTName("ОценкаРиска", IF (GetXAttrValueEx("person",STRING(person.person-id), "ОценкаРиска",""))="" THEN "Отсутствуют признаки легализации" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "ОценкаРиска",""))).
                               
                RUN Insert_TTName("IPDL", IF (GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ",""))="" THEN "нет" 
                        ELSE
                        GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ","") + "," +
                        GetXAttrValueEx("person",STRING(person.person-id), "СтепРодст_ИПДЛ","") + "," +
                        GetXAttrValueEx("person",STRING(person.person-id), "ОтнОкруж_ИПДЛ","")
                ).

                RUN Insert_TTName("IPDL_Da", IF (GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ",""))="" THEN "" ELSE "Х").
                RUN Insert_TTName("IPDL_Net", IF(GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ",""))="" THEN "Х" ELSE "").                                       
             
              /*   GetXAttrValueEx("person",STRING(person.person-id),"BirthPlace","")
                        MESSAGE  InfSignsV  VIEW-AS ALERT-BOX. person.name-last + " " + person.first-names + " " + person.birthday + " " + " " + */
                       
                                                                               
               /*RUN Insert_TTName("Ben_fio", IF (GetXAttrValueEx("person",STRING(person.person-id), "БенефицВлОтс",""))="" THEN mSignsV ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "БенефицВлОтс",""))). 
                */
                RUN Insert_TTName("SNILS", IF (GetXAttrValueEx("person",STRING(person.person-id), "NumberPFR",""))="" THEN "нет" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "NumberPFR",""))). 
               
IF person.document-id = "Паспорт" THEN 
DO:                    
    FIND FIRST code WHERE
        code.class       EQ "black-list"
        AND TRIM(code.code)  EQ TRIM(person.document)
        AND TRIM(code.name)  EQ TRIM(person.document-id)
        NO-LOCK NO-ERROR.  
              
    RUN Insert_TTName("ActiveDoc", IF AVAILABLE code THEN "Не действителен"  ELSE "Действителен" ).
end.
ELSE RUN Insert_TTName("ActiveDoc", "нет").
             
RUN Insert_TTName("DateReg","нет"). 
RUN Insert_TTName("OGRN","нет"). 
RUN Insert_TTName("NameReg","нет"). 
RUN Insert_TTName("MestReg","нет").              
                         
RUN Insert_TTName("INAL","нет").		  
RUN Insert_TTName("pDocName","нет").
RUN Insert_TTName("pDateV","нет").
RUN Insert_TTName("pPeriod","нет").
RUN Insert_TTName("pPeriod","нет").
RUN Insert_TTName("pNomDoc","нет").
RUN Insert_TTName("ID","не указано").
RUN Insert_TTName ("StateTax","нет"). 
                
RUN Insert_TTName("close-date", IF person.date-out = ? THEN "нет" ELSE REPLACE(string(person.date-out, "99/99/9999"), "/", ".")).
                           
/*mSignsV = REPLACE(GetTempXAttrValueEx("person",STRING(person.person-id), "ДатаПровТерр",end-date,""),"/",".").
  mSignsV = REPLACE(GetDateTemp("person",STRING(person.person-id), end-date,"ДатаПровТерр",date("07/09/2016")), " ", ",").*/
  mSignsV = REPLACE(GetDateTemp("person",STRING(person.person-id),end-date,date("07/09/2016"),No), " ", ",").  
                                  
RUN Insert_TTName("DP", IF mSignsV = "" THEN "нет" ELSE mSignsV).
RUN Insert_TTName("RP", IF (GetXAttrValueEx("person",STRING(person.person-id), "result_inspect",""))="" THEN "Не причастен" ELSE          
(GetXAttrValueEx("person",STRING(person.person-id), "result_inspect",""))).
                  
IF (GetXAttrValueEx("person",STRING(person.person-id), "ОценкаРиска",""))="" THEN
    RUN Insert_TTName("Risk_O", "Отсутствуют признаки легализации").
/*         IF CAN-DO("Низкий",GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв","")) THEN "Отсутствуют признаки легализации" ELSE "нет"*/
        
ELSE
    RUN Insert_TTName("Risk_O",(GetXAttrValueEx("person",STRING(person.person-id), "ОценкаРиска",""))).

/*mSignsV = REPLACE(GetDateTemp("person",STRING(person.person-id), end-date, "ДатаОбнАнкеты", date("27/12/2015")), " ", ",").*/

mSignsV = REPLACE(GetDateTemp("person",STRING(person.person-id), end-date,date("27/12/2015"),Yes ), " ", ",").            
RUN Insert_TTName("ДатаОбнАнкеты", IF mSignsV = "" THEN "нет" ELSE  REPLACE(mSignsV, "/", ".") + " года").
          
                    
mSignsV=GetXAttrValueEx("person",STRING(person.person-id),"СведВыгДрЛица" ,"").
IF mSignsV="" THEN 
DO:
    RUN Insert_TTName("VygDr","").
END.
ELSE 
DO:
    RUN Insert_TTName("VygDr", GetXAttrValueEx("person",STRING(person.person-id),"СведВыгДрЛица" ,"")).
END.

mSignsVal = GetXAttrValue("person",STRING(person.person-id),"ОснВидыДеят").
IF mSignsVal EQ "" THEN 
DO:
    mSignsVal = GetXAttrValueEx("person",STRING(person.person-id),"ВидДеят","").
    IF mSignsVal NE "" THEN 
    DO:
        FIND FIRST code WHERE code.code EQ mSignsVal
            AND code.class EQ "ВидДеят"
            NO-LOCK NO-ERROR.
        mSignsVal = code.name.
    END.
END.
RUN Insert_TTName ("VidDey",IF mSignsVal = "" THEN "нет" ELSE mSignsVal).

/* Находим номер лицензии 
     FIND FIRST cust-ident WHERE cust-ident.cust-cat EQ 'Ч' AND cust-ident.class-code EQ 'cust-lic' AND (cust-ident.close-date EQ ?  OR cust-ident.close-date GE TODAY) /* AND cust-ident.open-date LE TODAY  */ NO-LOCK NO-ERROR.
*/
FIND FIRST cust-ident WHERE
    cust-ident.cust-cat        EQ "Ч"
    AND    cust-ident.cust-id         EQ person.person-id
    AND    cust-ident.cust-code-type  EQ '133'
    AND    cust-ident.close-date      EQ ?  

    NO-LOCK NO-ERROR.

DEFINE VARIABLE mNB AS CHARACTER NO-UNDO.

IF AVAIL cust-ident THEN 
DO:
                          
    RUN Insert_TTName("ВидЛиц",IF (GetXAttrValueEx("person", STRING(person.person-id), "ЛицТип", ""))="" THEN "нет" ELSE GetXAttrValueEx("person", STRING(person.person-id), "ЛицТип", "")). 
                          
    mSignsVal = REPLACE(STRING(cust-ident.open-date, "99/99/9999"), "/", ".").
    RUN Insert_TTName("ЛицДатаВыд", IF mSignsVal = "" THEN "нет" ELSE mSignsVal). 
                          
    mSignsVal = REPLACE(STRING(cust-ident.close-date, "99/99/9999"), "/", ".").   
    RUN Insert_TTName("ЛицДатаО", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal ).  
                         
                          
    /* mSignsVal = cust-ident.cust-code.
      RUN Insert_TTName("ЛицНом", IF mSignsVal = "" THEN "нет" ELSE mSignsVal). */
                           
                                                
    mNB = IF cust-ident.close-date EQ ? THEN "нет" ELSE STRING(cust-ident.close-date). 
    /*  MESSAGE  mNB  VIEW-AS ALERT-BOX.*/
                             
    RUN Insert_TTName("ЛицДатаО",mNB).
                           
                           
    mSignsVal = GetCodeName("ВидЛицДеят",cust-ident.cust-code-type).
    RUN Insert_TTName("ЛицВид", IF mSignsVal = "" THEN "нет" ELSE mSignsVal).
                           
    mSignsVal = cust-ident.issue.
    RUN Insert_TTName("ЛицКемВыд", IF mSignsVal = "" THEN "нет" ELSE mSignsVal). 
END.
else
DO:                        
    RUN Insert_TTName("ВидЛиц","нет"). 
    RUN Insert_TTName("ЛицДатаВыд", "нет"). 
    RUN Insert_TTName("ЛицДатаО", "нет").                        
    RUN Insert_TTName("ЛицНом", "нет"). 
    RUN Insert_TTName("ЛицВид", "нет").
    RUN Insert_TTName("ЛицКемВыд","нет" ).                   
END.          

/**********************/  

RUN Insert_TTName("Ben_fio", person.name-last + " " + person.first-names).  
RUN benef(person.person-id).                                                                         
                      
FIND LAST cust-role WHERE cust-role.file-name EQ "person"
    AND cust-role.class-code EQ "Бенефиц_Влад" 
    AND (cust-role.close-date EQ ? or cust-role.close-date > end-date)
    AND cust-role.cust-cat EQ "Ч"
    AND cust-role.surrogate EQ string(person.person-id) NO-LOCK NO-ERROR.
                     
IF  AVAIL cust-role THEN 
do: 
    FIND FIRST person WHERE person.person-id EQ INT(cust-role.cust-id) NO-LOCK NO-ERROR.
    RUN benef(person.person-id).      
end.                         
RUN Insert_TTName("BenFIO1", InfSignsV).
RUN Insert_TTName("BenDoc1", InfSignsV2).  
RUN Insert_TTName("BenAdr1", InfSignsV3). 
RUN Insert_TTName("BenCont1", "Наличие возможности контролировать действия ИП").
 
FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
IF AVAIL(_User) THEN 
DO:
    RUN Insert_TTName("Prn_Name",_User._User-Name). 
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
    RUN Insert_TTName("Prn_Dolg", mStatus). 
END.  
                     
/*FIND FIRST history WHERE history.modify EQ "C" 
    AND NOT history.FIELD-ref BEGINS "_system_"
    AND history.field-ref EQ STRING(person.person-id)
    AND history.file-name EQ 'person' NO-LOCK NO-ERROR.

FIND FIRST _User WHERE _User._Userid EQ history.user-id  NO-LOCK NO-ERROR.
IF AVAIL(_User) AND (_User._User-Name NE "SYNC") THEN 
DO:
    RUN Insert_TTName("_Kur-Name",_User._User-Name).          
    mStatus = GetXAttrValueEx("_user", _User._Userid, "Должность", "").
    RUN Insert_TTName("Кур_Должн", mStatus).   
END.
ELSE 
DO:
    FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
    RUN Insert_TTName("_Kur-Name",_User._User-Name).    
            
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
    RUN Insert_TTName("Кур_Должн", mStatus).
END.*/
     
 mSignsVal = trim(GetXAttrValueEx("person",STRING(person.person-id), "branch-id","")). 
 If mSignsVal ='' then mSignsVal = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Отделение", "").       
            IF   CAN-DO("0300", mSignsVal) THEN     
            Do: 
            mSignsV = 'Товескина О.С.'.
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
            IF   CAN-DO("0409,0000,0101,0102,0106,0109,0110,04*", mSignsVal) THEN 
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
                     
                     
/* старое

FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
                RUN Insert_TTName("_Kur-Name",_User._User-Name).    
            
      mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
      RUN Insert_TTName("Кур_Должн", mStatus).*/
                         
/* Вывод данных по шаблону iParms (до "|") в файл отчета */


IF CAN-DO('zay_dkbo,zay_kb*,ZayvZakrBV*', iParms) THEN
    RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).
ELSE IF (iParms = 'ankfiz|') THEN
    IF (GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв",""))="Высокий" THEN
        RUN printvd.p (ENTRY(1, 'ankfiz92|', "|"), INPUT TABLE ttnames).                       /*риск высокий,бенефиц не найден*/
    ELSE         
        RUN printvd.p (ENTRY(1, 'ankfiz|', "|"), INPUT TABLE ttnames).                        /*риск НЕ высокий,бенефиц не найден*/
ELSE
do:   
    IF CAN-DO("*infank_ip*,*ankvygip*", iParms) THEN       
      
    DO:      
        If end-date LE date("26/12/2015") THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "0" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
        
        If (end-date GE date("26/12/2015"))  and  (end-date LE date("28/11/2016"))  THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
        
        If (end-date GE date("29/11/2016")) THEN 
            iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1)  . 
    END.
            
    IF CAN-DO("*ankip*", iParms) THEN
    
        iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "_" + SUBSTRING ( iParms, LENGTH (iParms), 1) . 
        
        
 /* Вывод данных по шаблону iParms (до "|") в файл отчета */
RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).   
 
end. 
/*поиск бенефициара отличного от ФЛ*/   

{intrface.del comm}  

   
PROCEDURE benef:   
    def INPUT PARAMETER pers_id   AS INT64 NO-UNDO. 
    
    mSignsV = String( person.name-last + " " + person.first-names ).
               
    InfSignsV = mSignsV.
                
    RUN Insert_TTName("ФИО",mSignsV).
                
    RUN Insert_TTName("DateRogd",person.birthday).                 
                
    mSignsV = GetXAttrValueEx("person",
        STRING(person.person-id),
        "BirthPlace", 
        "").
                                 
    RUN Insert_TTName("BirthPlace",mSignsV).                 
    InfSignsV = InfSignsV + " " + REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".") + " г.р., " + mSignsV.
         
    mcountr1 = GetXAttrValueEx("person",STRING(person.person-id),"country-id2","").

    FIND country WHERE country.country-id EQ person.country-id NO-LOCK NO-ERROR.
    IF AVAILABLE country THEN                       
        mSignsV = country.country-name.
    ELSE  
        mSignsV = person.country-id. 
                        
    RUN Insert_TTName("country-id2", mSignsV).                
                             
    InfSignsV = InfSignsV + ", гр-во " + trim(mSignsV).   
         
             
    mSignsV = IF person.inn NE "" THEN  ", ИНН " + string(person.inn) else " ".     
                                           
    InfSignsV = InfSignsV + mSignsV.
   
    
    InfSignsV2 = GetCodeName("КодДокум", person.document-id) .              
              
    RUN Insert_TTName("document",person.document).                                 /*Pasport  */                
                
    IF NUM-ENTRIES(person.document," ") EQ 2 THEN 
    DO:
        RUN Insert_TTName("document1",ENTRY(1,person.document," ")).
        RUN Insert_TTName("document2",ENTRY(2,person.document," ")).
        InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ").                                    
    END.
    ELSE IF NUM-ENTRIES(person.document," ") EQ 1 THEN 
        do:
            RUN Insert_TTName("document1","нет").   
            RUN Insert_TTName("document2",person.document).
            InfSignsV2 = InfSignsV2 + " " + person.document.
        end.
        ELSE 
        DO:
            RUN Insert_TTName("document1",ENTRY(1,person.document," ") + " " +   ENTRY(2,person.document," ") ).
            RUN Insert_TTName("document2",ENTRY(3,person.document," ")).
            InfSignsV2 = InfSignsV2 + " " + ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ") + " " + ENTRY(3,person.document," ").
        END.             
                             
    mSignsV = GetXAttrValueEx("person",
        STRING(person.person-id),
        "Document4Date_vid", 
        "").                                 
    RUN Insert_TTName("Document4Date_vid", mSignsV).                   /*Paspdate*/
                
    InfSignsV2 = InfSignsV2 + ", выд." + mSignsV + ", " + fGetDocIssue(person.person-id). /*person.issue.*/   
                
    
    RUN RetAdr.p(pers_id, "Ч", "АдрПроп", ?, OUTPUT mAdrReg).  
    RUN Insert_TTName("АдрЮр",mAdrReg).
    
    RUN RetAdr.p(pers_id, "Ч", "АдрФакт", ?, OUTPUT mAdrReg).
    RUN Insert_TTName("АдрФакт",mAdrReg).

    RUN RetAdr.p(pers_id, "Ч", "АдрУвед", ?, OUTPUT mAdrReg).  
    RUN Insert_TTName("АдрПочт",mAdrReg). 
        
    InfSignsV3 = mAdrReg + " " + mAdrFact.             
                
    vPhone = GetXattrValue("person",STRING(pers_id),"cell-phone").
    vPhone = IF person.phone[1] NE "," THEN 
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
        IF (person.phone[1] EQ "," AND person.phone[2] EQ "," AND vPhone EQ "") THEN "" ELSE "," +
                        IF vPhone NE "" THEN 
                        IF SUBSTRING(vPhone,LENGTH(vPhone),1) EQ "," THEN 
                            SUBSTRING(vPhone,1,LENGTH(vPhone) - 1) 
                        ELSE vPhone 
                ELSE "".
    RUN Insert_TTName("Phone", IF vPhone="" THEN "нет" ELSE vPhone).
                
    InfSignsV3 = InfSignsV3 + ", " + vPhone.      
               
    mSignsV = GetXAttrValueEx("person",STRING(pers_id), "e-mail","").
                
    RUN Insert_TTName("e-mail", IF mSignsV = "" THEN "нет" ELSE mSignsV).
                
    InfSignsV3 = InfSignsV3 + ", " +  mSignsV.   
                    
   
      
END PROCEDURE.   
                 
