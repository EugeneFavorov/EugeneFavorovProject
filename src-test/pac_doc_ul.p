/*
               Банковская интегрированная система БИСквит
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
DEFINE VARIABLE mAdrReg          AS CHARACTER		   NO-UNDO.  /*АдрЮр*/
DEFINE VARIABLE mAdrFact         AS CHARACTER		   NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mDRuk            AS CHARACTER		   NO-UNDO. /*ДолРук*/
DEFINE VARIABLE mNRuk            AS CHARACTER EXTENT 2 NO-UNDO. /*ФИОрук*/
DEFINE VARIABLE mIssue           AS CHARACTER EXTENT 2 NO-UNDO. /*Паспорт кем выдан*/
DEFINE VARIABLE mDateBeg         AS DATE               NO-UNDO.   /*???*/   
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

DEF VAR mSignsVal AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignVL   AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignsV   AS CHAR NO-UNDO. /* Значение реквизита из person */
DEF VAR mSignsV01 AS CHAR NO-UNDO. /* Значение реквизита из person */
DEF VAR mSignsV02 AS CHAR NO-UNDO. /* Значение реквизита из person */

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p      AS RECID.
DEFINE VARIABLE i         AS INT64              NO-UNDO. 
DEFINE VARIABLE j         AS INT64              NO-UNDO. 

   
DEF TEMP-TABLE ttOtchData NO-UNDO
FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* табл с индексом чтоб не морочится с сортировкой*/


&GLOB Months-i "январь,февраль,март,апрель,май,июнь,июль,август,сентябрь,~
октябрь,ноябрь,декабрь"
&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"
&GLOB Days "первое,второе,третье,четвертое,пятое,шестое,седьмое,восьмое,девятое,десятое,одиннадцатое,двенадцатое,тринадцатое,четырнадцатое,~
пятнадцатое,шестнадцатое,семнадцатое,восемнадцатое,девятнадцатое,двадцатое,двадцать первое,двадцать второе,двадцать третье,~
двадцать четвертое,двадцать пятое,двадцать шестое,двадцать седьмое,двадцать восьмое,двадцать девятое,тридцатое,тридцать первое"
&GLOB Years "первого,второго,третьего,четвертого,пятого,шестого,седьмого,восьмого,девятого,десятого,одинадцатого,двенадцатого,тринадцатого,~
четырнадцатого,пятнадцатого,шестнадцатого,семнадцатого,восемнадцатого,девятнацатого,двадцатого,двадцать первого,двадцать первого,~
двадцать второго,двадцать третьего,двадцать четвертого,двадцать пятого,шестого,двадцать седьмого,двадцать восьмого,двадцать девятого,тридцатого,тридцать первого,~
тридцать первого,тридцать второго,тридцать третьего,тридцать четвертого,тридцать пятого,тридцать шестого,тридцать седьмого,тридцать восьмого,тридцать девятого"

  
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
    
   /* FOR EACH tmpsigns WHERE tmpsigns.file-name = in-FileName 
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
    END.     */
    
     if in-Date GE date("30/06/2016") THEN     
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
    ELSE
    do:    
    FIND FIRST tmpsigns WHERE tmpsigns.file-name = in-FileName 
        AND (tmpsigns.code = "ДатаПровТерр")
        AND tmpsigns.surrogate  = in-Surr          
        AND tmpsigns.since LE in-Date      
         /*AND tmpsigns.since GE iDatedate("07/09/2016")   то что было до нас не интересует **/                     
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

/* выбор подписантов */
/* IF CAN-DO("zay_dkbo*",iParms) THEN 
DO: */
    {sign_select.i} 
/* END. */

IF CAN-DO('ank*', iParms) THEN
DO:
    {getdate.i &DateLabel = "Дата печати"  
               &DateHelp  = "Укажите дату печати анкеты"  }     
END. 

FIND FIRST tmprecid NO-LOCK NO-ERROR.
		      
FIND FIRST cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK NO-ERROR.
IF AVAIL cust-corp THEN 
    RUN Insert_TTName("ЮЛ", "1").
    RUN Insert_TTName("DogNum", STRING(cust-corp.cust-id)).

RUN BeginCircle_TTName("acct").
FOR EACH acct 
   WHERE acct.cust-id EQ cust-corp.cust-id
     AND acct.cust-cat EQ "Ю"
     AND CAN-DO("Расчет,Текущ*",acct.contract)
     AND acct.close-date EQ ?  
NO-LOCK BY acct.currency:
    IF AVAIL acct THEN
    DO:
        /* MESSAGE acct.acct VIEW-AS ALERT-BOX. */
        str = "".
        mTmp1 = "".
        IF acct.currency EQ "" 
            THEN str = "Договор банковского счета в валюте РФ от ".
        ELSE str = "Договор банковского счета в иностранной валюте от ".
        /*дата договора об открытии*/
        mTmp1 = REPLACE(ENTRY(1,GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"ДогОткрЛС","___.___._____")),"/",".").
        
        str = str + mTmp1 + " г.(расчетный счет № " + DelFilFromAcct(acct.acct) + ");".
        RUN Insert_TTName("strtable[acct]",str).
    
        RUN NextCircle_TTName("acct").
    END. 
END. /*for each acct*/
RUN EndCircle_TTName("acct").


        FIND FIRST acct WHERE  cust-corp.cust-id EQ acct.cust-id
                        AND    acct.cust-cat EQ 'Ю' 
            NO-LOCK NO-ERROR. 
            
            IF AVAIL acct THEN DO:
				RUN Insert_TTName("acct",   STRING(acct.acct, "x(25)")).								                               
                
				/*выбор города по подразделению*/
				IF CAN-DO("0401,0402,0403,0406,0407,0409", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","г.Санкт-Петербург").    
				IF CAN-DO("0400,0410", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","г.Москва").
				IF CAN-DO("0404,0405,0408", acct.branch-id) THEN
                RUN Insert_TTName("Gorod","г.Краснодар").
				mSignsV = STRING(acct.open-date, "99.99.9999") + " г.".
			END.   
			else               
				 mSignsV = "нет".	
			RUN Insert_TTName("Startdate",mSignsV).
					
		/*ayv изменен поиск дат для анкеты*/
		FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
					  AND   acct.cust-cat EQ 'Ю'
		NO-LOCK BY acct.open-date:
            IF  acct.contract   EQ "Расчет" 
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
             
        RUN Insert_TTName("ДатаЗаполАнкеты",REPLACE(mSignsVal,'г.','года')).   

        str = TRIM(str, ",").
        IF str = "" THEN 
            str = "-".
        IF NUM-ENTRIES(str) > 1 THEN 
            str = "".
        RUN Insert_TTName ("AccRasch",str).
        RUN Insert_TTName ("AccRaschXL", "№" + str).   

	/*	IF AVAIL acct THEN DO:
            mSignsVal = {strdate.i acct.open-date}.
			RUN Insert_TTName("Startdate",STRING(acct.open-date, "99.99.9999") + " г.").
		END.
		ELSE
			mSignsVal = {strdate.i cust-corp.date-in}.	
		RUN Insert_TTName("ДатаЗаполАнкеты", IF  mSignsVal="" THEN "нет" ELSE REPLACE(mSignsVal,'г.','года')).
		
				   FOR EACH acct WHERE acct.cust-id  EQ cust-corp.cust-id
                      AND   acct.cust-cat EQ 'Ю'
            NO-LOCK BY acct.open-date:
            LEAVE.
        END.*/
               		
		RUN Insert_TTName("Enddate", "нет").
						
		mSignsV = REPLACE(STRING(cust-corp.date-in, "99/99/9999"), "/", ".").

        RUN Insert_TTName("DatePrint", mSignsV + " года").   

        RUN Insert_TTName("open-date", mSignsV).
	
		mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id), end-date,date("27/12/2015"),Yes ), " ", ",").
		mSignsV = IF (mSignsV eq "") or (mSignsV eq ?) THEN STRING(cust-corp.date-in, "99/99/9999") ELSE  mSignsV.
		mSignsV = REPLACE(mSignsV, "/", ".") + " года".
        RUN Insert_TTName("ДатаОбнАнкеты", mSignsV).
		
		mSignsV=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","").
       /* IF mSignsV="" THEN DO:
            RUN Insert_TTName("RiskDa","Х").
            RUN Insert_TTName("RiskNet", "").
        END.
        ELSE */
    IF CAN-DO("*высокий*,*большой*", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","")) THEN 	
        do:           
            RUN Insert_TTName("RiskDa", "Х" ).
            RUN Insert_TTName("ОценкаРиска",GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОценкаРиска","")).
        end.
            
    else    RUN Insert_TTName("RiskDa", "").     
    
    IF CAN-DO("низкий", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв","")) or (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "РискОтмыв",""))="" THEN
        do: 
            RUN Insert_TTName("RiskNet",  "Х").
            RUN Insert_TTName("ОценкаРиска","Отсутствуют признаки легализации").           
        end.    
     ELSE RUN Insert_TTName("RiskNet",  ""). 
				
		      
      /*  RUN Insert_TTName("ОценкаРиска", IF GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОценкаРиска","")="" THEN "" ELSE
        GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОценкаРиска","")).
			*/
			
			
		mSignsV = if GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО","") = "" THEN GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО-НАЛОГ","") ELSE GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ОКАТО","") .	
	    RUN Insert_TTName("OKATO", IF  mSignsV="" THEN "нет" ELSE mSignsV).


        mSignsV = REPLACE(GetDateTemp("cust-corp",STRING(cust-corp.cust-id),end-date,date("07/09/2016"),No), " ", ",").
        /*   mSignsV = REPLACE(GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ДатаПровТерр",end-date,""),"/",".").  */
                                  
        RUN Insert_TTName("DP", IF mSignsV = "" THEN "нет" ELSE mSignsV).
          
/*
	   RUN Insert_TTName("DP", IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "date_inspect",""))="" THEN "нет" ELSE          
          (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "date_inspect",""))). */
          
        RUN Insert_TTName("RP", IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))="" THEN "Не причастен" ELSE          
        (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "result_inspect",""))).


        mSignsVal = GetXAttrValueEx("cust-corp",
									STRING(cust-corp.cust-id),
									"ФИОРук",?).   
        RUN Insert_TTName("ФИОРук", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).


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
              mSignsV02 = substr(mSignsV01,1,2) + " " +  ENTRY(int(substr(mSignsV01,3,2)),{&Months}) +  " " + STRING(substr(mSignsV01,5)) + "г.".
           end. 
           mSignsVal = mSignVL + "    " + mSignsV02.
        end.
        else do:
           mSignsVal = "№__________________      '______' ___________20__г.".
        end.
        RUN Insert_TTName("dkbo-nomer", mSignsVal).


        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "ДолРук",?).
        RUN Insert_TTName("ДолРук", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).
        
        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "CID",?).
        RUN Insert_TTName("CID", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).

        mSignsVal = GetXAttrValueEx("cust-corp",
                                    STRING(cust-corp.cust-id),
                                    "основа",
                                    ?).
        RUN Insert_TTName("основа", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).
        
        mSignsVal = GetXAttrValueEx("cust-corp", 
                                    STRING(cust-corp.cust-id), 
                                    "МестСведПред", 
                                    "").
        RUN Insert_TTName("МестСведПред", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).

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
                                    "МестСведПред", 
                                    "").
            RUN Insert_TTName("RegPlace", mSignsVal).
        END.
 
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОргСведПред", "").
        RUN Insert_TTName("ОргСведПред",  mSignsVal). 
      
              
               
        mSignsVal = "".
        FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                       AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                       AND   ((cust-role.class-code EQ 'Учредитель') or (cust-role.class-code EQ 'Акционер'))
                       AND   (cust-role.close-date EQ ? or cust-role.close-date GE end-date)
                       AND   cust-role.open-date LE end-date
                       
             NO-LOCK:
            {additem.i mSignsVal cust-role.cust-name}     
       end.         
        
       IF  mSignsVal eq "" THEN        
              mSignsVal = if GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УчредОрг",end-date, "") NE "" THEN GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УчредОрг",end-date, "") else "Нет".
         RUN Insert_TTName("УчредОрг", mSignsVal).
                       
      /*  mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УчредОрг", "").
        RUN Insert_TTName("УчредОрг",  mSignsVal).         
*/
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УставКап", "").
        If mSignsVal eq "" THEN  
        mSignsVal =  GetTempXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "УставКапОплач",end-date, "").
                       
        RUN Insert_TTName("УставКап",  mSignsVal).
                                
        mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "groupOABS", "").
        RUN Insert_TTName("groupOABS", IF shFilial = "0500" THEN mSignsVal ELSE "").
        
        RUN Insert_TTName("CIDbis", STRING(cust-corp.cust-id)).
                
		DEFINE VARIABLE mNBuh1 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh3 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh2 AS CHARACTER   NO-UNDO.
		DEFINE VARIABLE mNBuh4 AS CHARACTER   NO-UNDO.
						mNBuh1 = GetXAttrValueEx("cust-corp", 
									 STRING(cust-corp.cust-id), 
									 "фиобухг", 
									 "").
			IF  mNBuh1 ="" THEN
                mNBuh3="Лицо,   наделенное правом второй подписи, отсутствует".
			ELSE
                mNBuh3=mNBuh1. 
            RUN Insert_TTName("ФИОбухг", mNBuh3).                                
                
            mNBuh2 = GetXAttrValueEx("cust-corp", 
                             		 STRING(cust-corp.cust-id), 
                             		 "фиобухг", 
                             		 "").
			IF  mNBuh2 ="" THEN
                mNBuh4="Нет".
			ELSE
                mNBuh4=mNBuh2. 
            RUN Insert_TTName("ФИОбг", mNBuh4).                                
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
						 			STRING(cust-corp.cust-id), 
						 			"ОГРН", 
						 			"").
		                                 
       RUN Insert_TTName("ОГРН", IF mSignsVal = "" THEN "нет" ELSE mSignsVal).
		
		
		mSignsVal = GetXAttrValueEx("cust-corp", 
						 			STRING(cust-corp.cust-id), 
						 			"КПП", 
						 			"").
		RUN Insert_TTName("КПП", mSignsVal). 
		
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
          
        RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрПочт",end-date ,OUTPUT mAdrReg).
        mSignsVal = mSignsVal + mAdrReg.                              
                        END.
        
        /*RUN Insert_TTName("e-mail", mSignsVal).*/
        RUN Insert_TTName("e-mail", IF mSignsVal = "" THEN "нет" ELSE mSignsVal).

	    RUN Insert_TTName("UstavCap", "нет").
		RUN Insert_TTName("StateTax", "нет").
		RUN Insert_TTName("ИНН",cust-corp.inn).
		/*ayv убрана проверка по счету*/
		/*RUN GetCustName IN h_Base (acct.cust-cat,
				   acct.cust-id,
				   IF AVAILABLE acct THEN acct.acct ELSE "",
				   OUTPUT mName[1],
				   OUTPUT mName[2],
				   INPUT-OUTPUT mInn).*/
		mName[1] = cust-corp.cust-stat.
		mName[2] = cust-corp.name-corp.

	    FIND FIRST code WHERE 
				  code.class EQ "КодПредп"
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
        RUN Insert_TTName("okpo",IF cust-corp.okpo NE '' THEN cust-corp.okpo ELSE 'нет').
        RUN Insert_TTName("NameShort", IF cust-corp.name-short NE '' THEN ', ' + cust-corp.name-short ELSE cust-corp.name-short).
        RUN Insert_TTName("NYulShort", IF cust-corp.name-short NE '' THEN cust-corp.name-short ELSE TRIM(mName[1]) + ' ' + TRIM(mName[2])).
             
		mSignVL=GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"СведВыгДрЛица","").
		IF mSignVL="" THEN DO:
				RUN Insert_TTName("VygDr","").
				END.
				ELSE DO:
				RUN Insert_TTName("VygDr", GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"СведВыгДрЛица","")).
		END.
		
		/*mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОргУправ", "").
        RUN Insert_TTName("OrgUpr", IF mSignsVal = "" THEN "нет" ELSE  mSignsVal).*/ 
		
	    RUN Insert_TTName("Bf_Da",  IF CAPS(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "nal_bf", ""))= CAPS("Да") THEN "Х" ELSE "").
        RUN Insert_TTName("Bf_Net", IF CAPS(GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "nal_bf", ""))= CAPS("Нет") THEN "Х" ELSE "").

		RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрЮр", ?,OUTPUT mAdrReg).
        RUN Insert_TTName("АдрЮр",mAdrReg). 

      /*  RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрУвед", ?,OUTPUT mAdrReg).
	    RUN Insert_TTName("АдрПочт",mAdrReg). */
		
		RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрФакт", ?,OUTPUT mAdrFact).
	    RUN Insert_TTName("АдрФакт", IF mAdrFact NE "" THEN mAdrFact ELSE mAdrReg). 
		
		
		mSignsVal = "".		
		  IF (GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))	ne "" then
		  do:							   
        IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="да") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", "")) begins "присут")  THEN mSignsVal = "Х".
		RUN Insert_TTName("OrgUpr_Da", mSignsVal).
		
		mSignsVal = "".		
		IF (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", ""))="нет") or (LC(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "ПрисутОргУправ", "")) begins "отсут") THEN mSignsVal = "Х".
		RUN Insert_TTName("OrgUpr_Net", mSignsVal).
		end.
		else
		do:
		   IF mAdrReg eq mAdrFact then
            RUN Insert_TTName("OrgUpr_Da", "Х").
           else
            RUN Insert_TTName("OrgUpr_Net", "Х").
		 end.
		 
		RUN Insert_TTName("OrgUpr",GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОргУправ", "")).
				  
		
	/* Ищем бенефициаров */
	/*ayv изменен поиск бенифициаров*/
	mSignsVal = ''.
	mLog = TRUE.
    i = 1.



IF iParms EQ "vypcardUL" THEN 
    do:
        end-date = today.
        mSignsV ='*иректор*'. 
    end.    
ELSE mSignsV = 'Бенефиц_Влад'.

 	FOR EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
					   AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                      /* AND   cust-role.class-code EQ 'Бенефиц_Влад'*/
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
                                    "основа",
                                    ?).
            RUN Insert_TTName("основа",mSignsV).
            RUN Insert_TTName("DateRogd",STRING(person.birthday, "99.99.9999")).

            InfSignsV = person.name-last + " " + person.first-names + " " + REPLACE(STRING(person.birthday, "99/99/9999"), "/", ".") + " г.р., " .
            
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
                InfSignsV = InfSignsV +  trim(mSignsV) + ", гр-во " + trim(mSignsV) .     
                
                 
                IF person.inn NE "" THEN InfSignsV = InfSignsV + ", ИНН " + person.inn. 
                
                InfSignsV2 = GetCodeName("КодДокум", person.document-id) .              
                RUN Insert_TTName("documtype", InfSignsV2).   

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
                                          
                mSignsV = GetXAttrValueEx("person",
                                          STRING(person.person-id),
                                          "Document4Date_vid", 
                                          ""). 
                RUN Insert_TTName("Document4Date_vid", REPLACE(mSignsV, "/", ".")).                   /*Paspdate*/                                
                                                 
                InfSignsV2 = InfSignsV2 + ", выд." + REPLACE(mSignsV, "/", ".") + ", " +  fGetDocIssue(person.person-id).  /*person.issue */

                RUN RetAdr.p(person.person-id, "Ч", "АдрПроп", ?, OUTPUT InfSignsV3).           
                RUN Insert_TTName("АдрПроп",InfSignsV3).
                
                RUN RetAdr.p(person.person-id, "Ч", "АдрПочт", ?, OUTPUT mSignsV).           
                RUN Insert_TTName("АдрПочт",mSignsV).
                
                RUN RetAdr.p(person.person-id, "Ч", "АдрФакт", ?, OUTPUT mSignsV).
                RUN Insert_TTName("АдрФакт",mSignsV).
                
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
                       RUN Insert_TTName("BenCont" + string(i), "Прямое владение - наличие преобладающего участия более 25 процентов в капитале организации").

       i = i + 1.
	END.

	IF mSignsVal EQ '' THEN DO:
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

	RUN Insert_TTName("Benif", mSignsVal).

	IF mLog THEN
		RUN Insert_TTName("BenifOsn", 'Прямое владение - наличие преобладающего участия более 25 процентов в капитале организации').
	ELSE
		RUN Insert_TTName("BenifOsn", 'Ввиду невозможности установить бенефициарного владельца и несоответствия критериям, перечисленным в 115-ФЗ, принято решение бенефициарным владельцем признать руководителя').

	DEFINE VARIABLE mStrTMP1   AS CHARACTER NO-UNDO. 
	DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO. 
	DEFINE VARIABLE mItem      AS INT NO-UNDO. 
	DEFINE VARIABLE vOkvedCodeName  AS CHARACTER NO-UNDO. 

	mStrTMP1 = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ОКВЭД", "").

		   RUN BeginCircle_TTName("okved").
		  DO mItem =1 TO NUM-ENTRIES(mStrTMP1):
			 mStrTMP = ENTRY(mItem,mStrTMP1).
			 vOkvedCodeName = GetCodeName("ОКВЭД2", mStrTMP).
			 RUN Insert_TTName("OKVED" + STRING(mItem), mStrTMP + "  " + vOkvedCodeName).
			 mStrTMP = REPLACE(mStrTMP, ".", " ").
			 RUN NextCircle_TTName("okved").
		  END.
		  RUN EndCircle_TTName("okved").


	 /* Находим номер лицензии */
	 /*ayv изменен поиск лицензии
	 	RUN Insert_TTName ("graph", ""). 
   
		FIND FIRST ttNames WHERE
	              ttnames.tname EQ 'graph'
			NO-LOCK NO-ERROR.

		mLog = FALSE.

		FOR EACH cust-ident WHERE cust-ident.cust-cat EQ 'Ю' 
		   					AND   cust-ident.cust-id EQ cust-corp.cust-id 
		  					AND   cust-ident.class-code EQ 'cust-lic' 
		  					AND  (cust-ident.close-date EQ ?  
		  					OR    cust-ident.close-date GE TODAY) 
		  					AND   cust-ident.open-date LE TODAY 
		  		NO-LOCK:

		  	mLog = TRUE.
		  	ttnames.tvalue = ttnames.tvalue + 'вид'                                       + '\n'
		  									+ 'Лицензия'       							  + '\n'
		  									+ 'номер'                                     + '\n'
		  									+  cust-ident.cust-code         			  + '\n'
		  									+ 'дата выдачи лицензии'                      + '\n'
		  									+  STRING(cust-ident.open-date)				  + '\n'
		  									+ 'кем выдана'                                + '\n'
		  									+  cust-ident.issue							  + '\n'
		  									+ 'срок действия'                             + '\n'
		  									+  (IF cust-ident.close-date EQ ? 
		  									   THEN 'без ограничения срока действия' 
		  									   ELSE STRING(cust-ident.close-date)) 		  + '\n'
		  									+ 'перечень видов лицензируемой деятельности' + '\n'
		  									+  GetCodeName("ВидЛицДеят",
		  													cust-ident.cust-code-type)    + '\n'.

		END.

		IF NOT mLog THEN 
			ttnames.tvalue = ttnames.tvalue + 'вид'                                       + '\n'
		  									+ 'нет'		       							  + '\n'
		  									+ 'номер'                                     + '\n'
		  									+ 'нет'					         			  + '\n'
		  									+ 'дата выдачи лицензии'                      + '\n'
		  									+ 'нет'										  + '\n'
		  									+ 'кем выдана'                                + '\n'
		  									+ 'нет'										  + '\n'
		  									+ 'срок действия'                             + '\n'
		  									+ 'нет'										  + '\n'
		  									+ 'перечень видов лицензируемой деятельности' + '\n'
		  									+ 'нет'										  + '\n'.	
		 													
			IF AVAIL cust-ident THEN DO:
					
				/*RUN Insert_TTName("ВидЛиц", GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "ЛицТип", "")). */
				RUN Insert_TTName("ВидЛиц",'Лицензия').
				
				mSignsVal = STRING(cust-ident.open-date).
				RUN Insert_TTName("ЛицДатаВыд", mSignsVal). 
					
				mSignsVal = IF cust-ident.close-date EQ ? THEN 'без ограничения срока действия' ELSE STRING(cust-ident.close-date).
				RUN Insert_TTName("ЛицДатаО", mSignsVal).  
					
				mSignsVal = cust-ident.cust-code.
				RUN Insert_TTName("ЛицНом", mSignsVal). 
					
				mSignsVal = GetCodeName("ВидЛицДеят",cust-ident.cust-code-type).
				RUN Insert_TTName("ЛицВид", mSignsVal).
					
				mSignsVal = cust-ident.issue.
				RUN Insert_TTName("ЛицКемВыд", mSignsVal). 
				
		  	END.
		  	ELSE DO:
		  		RUN Insert_TTName("ВидЛиц", "нет").
				RUN Insert_TTName("ЛицДатаВыд", "нет").
				RUN Insert_TTName("ЛицДатаО", "нет").
				RUN Insert_TTName("ЛицНом", "нет").
				RUN Insert_TTName("ЛицВид", "нет").
				RUN Insert_TTName("ЛицКемВыд", "нет").
		  	END.*/
		  	
		  	
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
                    
               /* mSignsV = GetXattrValueEx("cust-ident",STRING(cust-corp.cust-id),"ЛицензОрг","").
                RUN Insert_TTName("ЛицВид",  mSignsV).*/
                
                mSignsV = GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"ЛицензОрг","").
                RUN Insert_TTName("ЛицВид",  mSignsV).
                    
                mSignsV = cust-ident.issue.
                RUN Insert_TTName("ЛицКемВыд",  mSignsV).
            end.


/*FOR each cust-role WHERE 
        cust-role.file-name EQ "cust-corp"  
        AND cust-role.surrogate EQ STRING(cust-corp.cust-id)
        AND cust-role.class-code = "Право_первой_подписи"
        and cust-role.cust-cat = "Ч"
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
                RUN Insert_TTName("ИНН",person.inn). 

                RUN Insert_TTName("DateRogd",person.birthday). 
                RUN Insert_TTName("document-id",person.document-id).           
                RUN Insert_TTName("document",person.document).                
        mcountr1 = GetXAttrValueEx("person",
                                  STRING(person.person-id),
                                 "country-id2", 
                                 "").
        IF mcountr1 = "RUS" THEN mcountr="РФ" .
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

FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
IF AVAIL(_User) THEN 
DO:
    RUN Insert_TTName("Prn_Name",_User._User-Name). 
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
    RUN Insert_TTName("Prn_Dolg", mStatus). 
END.

 mSignsVal = trim(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "branch-id","")). 
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
            IF   CAN-DO("0400,0401,0402,0403,0404,0405,0406,0407,0408,0409,0000,0101,0102,0106,0109,0110,0111", mSignsVal) THEN 
            Do:
                if end-date LE date("10/07/2017") then
                    do: 
                    mSignsV = "Маусумбаева Г.К. ".
                    mStatus = "Руководитель направления РКО по г. Москва".
                    end.    
                else
                    do:
                    mSignsV = "Нестеркина Л.А.".
                    mStatus = "Исполнительный директор/Директор Департамента РКО".                       
                    end.    
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
                                    "ВидДок",
                                    "").
    &ELSE iParms = ENTRY(1, iParms, "|").
&ENDIF

/* Вывод данных по шаблону iParms в файл отчета */
RUN printvd.p (iParms, INPUT TABLE ttnames).   
{intrface.del comm}
