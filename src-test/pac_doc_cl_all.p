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

DEFINE VARIABLE mList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.

DEF VAR mSignsVal AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignVL AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mSignsV AS CHAR NO-UNDO. /* Значение реквизита из person */

DEF SHARED VAR tmprec_id  AS RECID.
DEF SHARED VAR rid-p     AS RECID.

DEF TEMP-TABLE ttOtchData NO-UNDO
   FIELD ttDate AS DATE
INDEX ttDate ttDate.               /* табл с индексом чтоб не морочится с сортировкой*/

DEF TEMP-TABLE tt-inf NO-UNDO
   FIELD number AS DATE
INDEX number number.  


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
  
 /* INPUT FROM VALUE("ofm.log").
REPEAT:
   IMPORT UNFORMATTED mList.
   IF mList NE "end" THEN
   DO:
      CREATE tt-inf.
      ASSIGN
         tt-inf.number = TRIM(mList).
   END.
END.*/
  
  

ASSIGN
mList = "42682,55101,70530,82587,70340,55029,515820,563226,555815,632187,630026,619713,625196,594487,578926,617506,651655,647584,653654,656296,525676,652146,663184,683017,659156,49923,83210,123383,83459,71624,38892,534314,520000,55383,52810,56618,549365,563076,612653,617808,574760,574944,599897,611108,605065,614872,638810,573280,583303,604882,604920,612024,644523,656178,670339,687318,671810,675107,665194,659036,664194,670935,681477,659638,674627,685772,43332,44236,52521,51775,55106,56101,57420,508692,165333,75081,494803,139708,497254,531402,504737,48218,550032,565300,556258,619914,628403,597635,638207,616794,592209,567936,641202,642808,655899,647538,652447,661655,657593,687335,684088,674100,664571,671995,682626,665683,674567,54116,497338,508544,86433,134318,124305,82244,534203,512677,50953,549227,561808,554351,597822,597420,614475,633575,587538,598480,638880,639504,572579,576710,617421,641557,641580,642941,654000,644439,656230,652549,656563,670635,680834,671662,672019,667606,667556,685651,679133,45990,49552,55990,95707,95720,228821,509645,124624,516093,536148,47592,554274,562265,565230,622431,593901,634416,623693,568824,615865,628173,629741,576786,587453,583051,605187,590437,591849,623512,651866,648206,658974,659248,683076,43123,59809,59526,70422,67725,121537,134358,202648,77328,520965,526728,517058,56569,48557,563333,564982,603261,623353,634677,644714,650231,660845,681711,672029,672219,664413,671107,660570,659857,660743,671258,659167,688386,667508,44873,46912,46067,60507,59194,67954,62660,513734,508842,203104,75546,85129,85442,78311,531255,63785,545516,602193,567392,606039,579540,630742,570880,631583,628131,597731,592829,631053,584183,637587,636496,654274,644660,646575,652944,681574,661074,664441,664449,670288,661313,663195,669064,664423,675754,672127,660174,670397,670401,51274,56386,68108,63255,63508,507730,83218,140958,232921,134019,122974,515140,522156,525334,63172,520538,553361,623688,630717,600794,620647,646115,641748,656201,652087,672549,672111,670125,680400,664649,672958,667989,671714,682650,687284,672605,675459,32101,54216,62555,83297,72370,37089,535064,51261,49317,546272,546748,569558,633410,605746,614830,587616,638898,631504,567365,591907,593670,622356,641467,650159,655873,656408,660700,670135,670166,674808,682263,659947,657565,682118,672646,685753,36299,55327,55400,55509,70634,79070,79497,521038,521475,563343,553678,564987,602787,640691,610187,606499,619551,572428,593188,636340,639933,641158,644642,641630,673713,681576,688596,661242,676153,660870,684894,39647,40376,51419,49899,68307,82887,58563,62704,583345,587624,572649,638067,619670,628088,632506,621079,645139,644844,644876,656806,663149,663231,659068,670148,664505,664537,683401,667574,667566,685718,49233,39416,59572,508643,243689,73327,139082,164582,79113,76123,514831,519550,522614,556306,563198,613395,579578,573626,619936,602632,604104,599398,604371,618080,578755,591224,621601,626489,656794,687298,674869,661134,671788,665958,681663,685765,41402,58807,56470,82847,144726,167557,73803,522573,58350,43808,565365,549143,573854,573715,630744,615131,583373,587048,585734,588855,600816,640181,656126,663565,675652,687306,688305,661298,675115,664643,660554,672538,680810,657873,681504,661592,49582,58451,84359,123107,516167,520944,536508,540229,574506,579488,628273,639506,634654,614083,575723,651636,644586,651220,644852,670284,682677,680731,666169,681216,684468,37686,36646,56099,60674,53702,62706,68843,68484,124668,75169,528252,521496,61941,540178,555000,592265,640689,567210,614536,604778,569872,615040,606952,639343,580109,573286,618948,628120,595320,624316,621597,621639,641115,644260,643555,641603,653896,671961,661293,664225,687157,668017,671201,663237,684876,50656,49618,55249,55879,82595,137546,491435,494950,515895,59528,537304,545464,561876,563325,636444,570557,609779,634073,565481,640074,619276,594665,590714,531107,642698,645623,522490,653667,654036,656606,675436,670199,670205,665779,659040,664153,670918,687876,667586,681661"
.

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
                 
DO mI = 1 TO NUM-ENTRIES(mList):

{empty ttnames}
/*
FOR EACH tmprecid NO-LOCK:
FIND FIRST tmprecid NO-LOCK NO-ERROR.tmprecid.idRECID(person) */
                      
       
                     
FIND FIRST person WHERE person.person-id EQ int64(ENTRY(mI, mList))  NO-LOCK NO-ERROR.
/*MESSAGE string(mI)  VIEW-AS ALERT-BOX.  */            

FIND FIRST acct WHERE  person.person-id EQ acct.cust-id  NO-LOCK NO-ERROR. 

if (mI <= 145)   THEN do:
     RUN Insert_TTName("Prn_Name","Маусумбаева Гульмира Калкамановна").
     RUN Insert_TTName("Prn_Dolg", "Руководитель направления расчетно-кассового обслуживания по г. Москве"). 
     end.
 if (mI > 145) and (mI <= 290)   THEN   do:
      RUN Insert_TTName("Prn_Name","Шаболдина Ольга Сергеевна").
     RUN Insert_TTName("Prn_Dolg", "Руководитель проекта"). 
     end.  
 if (mI > 290) and (mI <= 435) THEN  do:
      RUN Insert_TTName("Prn_Name","Максимова Анна Анатольевна").
     RUN Insert_TTName("Prn_Dolg", "Начальник отдела ВИП-обслуживания"). 
     end. 
 if (mI >= 435)  THEN  do:
      RUN Insert_TTName("Prn_Name","Бабыкина Анастасия Евгеньевна").
     RUN Insert_TTName("Prn_Dolg", "Главный специалист"). 
     end.                    



IF AVAILABLE acct THEN                     
    RUN Insert_TTName("acct",   STRING(acct.acct, "x(25)")).  
ELSE
    RUN Insert_TTName("acct", "нет").  
 
mSignsV = REPLACE(STRING(person.date-in, "99/99/9999"), "/", ".").

RUN Insert_TTName("DatePrint", mSignsV + " года").  

/**/        
IF GetXAttrValueEx("person",STRING(person.person-id), "Субъект","") = "ФЛП" THEN 
DO:
    FOR EACH acct WHERE acct.cust-id  EQ person.person-id
        AND acct.cust-cat EQ 'Ч'
        AND acct.contract EQ 'Расчет' 
    NO-LOCK BY acct.open-date:
       /* LEAVE. */
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

    IF AVAILABLE acct THEN
        mSignsV = {strdate.i acct.open-date}.      
ELSE mSignsV = "нет". 
END. 
else 
DO:  
FIND FIRST acct WHERE acct.cust-id  EQ person.person-id
        AND acct.cust-cat EQ 'Ч'
        AND can-do('Расчет,Депоз,Текущ',acct.contract)  NO-LOCK NO-ERROR.
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
                
RUN Insert_TTName("document-id",person.document-id).                                 /*Pasport  */

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
              
RUN Insert_TTName("ИНН",IF person.inn="" THEN "нет" ELSE person.inn).               
              
RUN Insert_TTName("mIssue",person.issue).                         /* кем выдан*/
                                            
IF NUM-ENTRIES(person.issue,",") LT 2 THEN 
DO:
    RUN Insert_TTName("mIssue1",person.issue).
    RUN Insert_TTName("mIssue2","нет").
END.
ELSE 
DO:
    RUN Insert_TTName("mIssue1",ENTRY(1,person.issue,",")).
    RUN Insert_TTName("mIssue2",ENTRY(2,person.issue,",")).
END.          
             
RUN Insert_TTName("MigrCard", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрКарт",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрКарт",""))). 
               
RUN Insert_TTName("MigrS", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывС",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывС",""))). 

RUN Insert_TTName("MigrPo", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывПо",""))="" THEN "нет" ELSE
(GetXAttrValueEx("person",STRING(person.person-id), "МигрПребывПо",""))). 

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
    
                RUN Insert_TTName("VisaType", IF (GetXAttrValueEx("person",STRING(person.person-id), "VisaType",""))="" THEN "нет" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "VisaType",""))).   
        
                RUN Insert_TTName("VisaS", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПравПребС",""))="" THEN "нет" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "МигрПравПребС",""))).

                RUN Insert_TTName("VisaPo", IF (GetXAttrValueEx("person",STRING(person.person-id), "МигрПравПребПо",""))="" THEN "нет" ELSE
                (GetXAttrValueEx("person",STRING(person.person-id), "МигрПравПребПо",""))).

                RUN Insert_TTName("IPDL_STATUS", IF (GetXAttrValueEx("person",STRING(person.person-id), "Статус_ИПДЛ",""))="" THEN "нет" ELSE "да").

mSignsV=GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв","").
/* IF mSignsV="" THEN DO:
         RUN Insert_TTName("RiskDa","").
         RUN Insert_TTName("RiskNet", "").
 END.
 ELSE DO: */       
RUN Insert_TTName("RiskDa", IF CAN-DO("*высокий*,*большой*", GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв","")) THEN "Х" ELSE "").
                RUN Insert_TTName("RiskNet", IF (CAN-DO("низкий", GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв",""))) or (GetXAttrValueEx("person",STRING(person.person-id), "РискОтмыв",""))="" THEN "Х" ELSE "").
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
    /*AND (cust-role.close-date EQ ? or cust-role.close-date > end-date)*/
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
 
/*
FIND FIRST _User WHERE _User._Userid EQ USERID("bisquit")  NO-LOCK NO-ERROR.
IF AVAIL(_User) THEN 
DO:
    RUN Insert_TTName("Prn_Name",_User._User-Name). 
    mStatus = GetXAttrValueEx("_user", STRING(USERID("bisquit")), "Должность", "").
    RUN Insert_TTName("Prn_Dolg", mStatus). 
END.  */
                     
      
                     
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
    If end-date LE date("26/12/2015") THEN 
        iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "0" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
    If (end-date GE date("26/12/2015"))  and  (end-date LT date("29/11/2016"))  THEN 
        iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "" + SUBSTRING ( iParms, LENGTH (iParms), 1) .
    else
        iParms = SUBSTRING ( iParms, 1, (LENGTH (iParms ) - 1) ) + "1" + SUBSTRING ( iParms, LENGTH (iParms), 1) .         
    RUN printvd.p (ENTRY(1, iParms, "|"),
        INPUT TABLE ttnames).
end. 

end. 
/*поиск бенефициара отличного от ФЛ*/   
   
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
                
    InfSignsV2 = InfSignsV2 + ", выд." + mSignsV + ", " + person.issue.   
                
    
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
                 
                   
{intrface.del comm}