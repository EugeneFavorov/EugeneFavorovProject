{globals.i}

{sh-defs.i}
{tmprecid.def}
{clnt.fun}

/*DEFINE PARAMETER BUFFER iTmpRecID FOR tmprecid.  */
/*DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.*/

DEFINE BUFFER iTmpRecID FOR tmprecid.

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.

DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId               AS INT64     NO-UNDO.
DEFINE VARIABLE mInn                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFam                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtch                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateVyd              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKemVyd               AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodPodr              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlace                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTelMob               AS CHARACTER NO-UNDO.
DEFINE VARIABLE mEMail                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mWebPage              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdress1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdress2              AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCountry1             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountryName1         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIndex1               AS INT64     NO-UNDO.
DEFINE VARIABLE mOKATO1               AS INT64     NO-UNDO.
DEFINE VARIABLE mRegion1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPunkt11              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPunkt12              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStreet1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mHome1                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlat1                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorpus1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStroen1              AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCountry2             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountryName2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIndex2               AS INT64     NO-UNDO.
DEFINE VARIABLE mOKATO2               AS INT64     NO-UNDO.
DEFINE VARIABLE mOblast1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOblast2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRegion2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPunkt21              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPunkt22              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStreet2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mHome2                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlat2                AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorpus2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStroen2              AS CHARACTER NO-UNDO.

mFileName = "./clients.txt".

OUTPUT TO VALUE(mFileName) CONVERT TARGET "1251".
FOR EACH iTmpRecID NO-LOCK,
   FIRST loan WHERE
   RECID(loan) EQ iTmpRecID.id
   NO-LOCK,
   FIRST person WHERE
   person.person-id EQ loan.cust-id
   NO-LOCK BY loan.cont-code:

      mInt = mInt + 1.
   
   mAdress1 = "".
   mAdress2 = "".

/*<код страны>,<индекс>,<код региона>,<район>,<город>,<населенный пункт>,<улица>,<дом>,<корпус>,<строение>,<квартира>.
^643   ,659572,01,Быстроистокский р-н,,Новопокровка с                 ,Октябрьская ул ,36,,,1
^РОССИЯ^659572^01^Быстроистокский р-н^^Новопокровка с^^Новопокровка с^^Октябрьская ул^^36^^^1^
^РОССИЯ^659572^01^Быстроистокский    ^р-н^              ^^              ^^              ^^  ^^^^^^^
*/
   
   ASSIGN
      mFam      = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Фам")
      mName     = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Имя")
      mOtch     = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Отч")
      mDateVyd  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"ДатаВыдачи")
      mKemVyd   = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"КемВыданДок")
      mKodPodr  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"КодПодр")
      mPlace    = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"МестоРожд")
      mInn      = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"ИНН")
      mEMail    = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"EMail")

      mAdress1  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресРег")
      mCountry1 = GetEntries(1,mAdress1,",","")
      mIndex1   = INT64(GetEntries(2,mAdress1,",",""))
      mOKATO1   = INT64(GetEntries(3,mAdress1,",",""))
      mOblast1  = GetCodeName("КодРег",STRING(mOKATO1,"99999"))
      mRegion1  = GetEntries(4,mAdress1,",","")
      mPunkt11  = GetEntries(5,mAdress1,",","")
      mPunkt12  = GetEntries(6,mAdress1,",","")
      mStreet1  = GetEntries(7,mAdress1,",","")
      mHome1    = GetEntries(8,mAdress1,",","")
      mFlat1    = GetEntries(10,mAdress1,",","")
      mKorpus1  = GetEntries(9,mAdress1,",","")
      
      mAdress2  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресФакт")
      mCountry2 = GetEntries(1,mAdress2,",","")
      mIndex2   = INT64(GetEntries(2,mAdress2,",",""))
      mOKATO2   = INT64(GetEntries(3,mAdress2,",",""))
      mOblast2  = GetCodeName("КодРег",STRING(mOKATO2,"99999"))
      mRegion2  = GetEntries(4,mAdress2,",","")
      mPunkt21  = GetEntries(5,mAdress2,",","")
      mPunkt22  = GetEntries(6,mAdress2,",","")
      mStreet2  = GetEntries(7,mAdress2,",","")
      mHome2    = GetEntries(8,mAdress2,",","")
      mFlat2    = GetEntries(10,mAdress2,",","")
      mKorpus2  = GetEntries(9,mAdress2,",","")
      .
   ASSIGN      
      mAdress1  = TRIM(mCountryName2) + "," +
                  TRIM(STRING(mIndex1,"999999")) + "," +
                  TRIM(STRING(mOKATO1,"99"))     + "," +
                  TRIM(mRegion1)                 + "," +
                  TRIM(mPunkt11) + "," + 
                  TRIM(mPunkt12) + "," + 
                  TRIM(mStreet1) + "," + 
                  TRIM(mHome1)   + "," + 
                  TRIM(mKorpus1) + "," + 
                  TRIM(mStroen1) + "," +
                  TRIM(mFlat1).
   ASSIGN      
      mAdress2  = TRIM(mCountryName2) + "," +
                  TRIM(STRING(mIndex2,"999999")) + "," +
                  TRIM(STRING(mOKATO2,"99"))     + "," +
                  TRIM(mRegion2)                 + "," +
                  TRIM(mPunkt21) + "," + 
                  TRIM(mPunkt22) + "," + 
                  TRIM(mStreet2) + "," + 
                  TRIM(mHome2)   + "," + 
                  TRIM(mKorpus2) + "," + 
                  TRIM(mStroen2) + "," +
                  TRIM(mFlat2).                  
   
   FIND FIRST country WHERE country.country-alt-id EQ INT64(mCountry1) NO-LOCK NO-ERROR.
   IF AVAIL(country) THEN mCountryName1 = TRIM(country.country-name).  
   FIND FIRST country WHERE country.country-alt-id EQ INT64(mCountry2) NO-LOCK NO-ERROR.
   IF AVAIL(country) THEN mCountryName2 = TRIM(country.country-name).
   /*************/
   mEMail = GetXAttrValueEx("person",STRING(person.person-id),"e-mail","").
   DEFINE VARIABLE mGrazhd   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mKodDocum AS CHARACTER NO-UNDO. 
   FIND FIRST country WHERE country.country-id EQ person.country-id NO-LOCK NO-ERROR.
   IF AVAIL(country) THEN mGrazhd = TRIM(STRING(country.country-alt-id)).
   
   CASE person.document-id:
      WHEN "Паспорт"      THEN mKodDocum = "01".
      WHEN "СвидРожд"     THEN mKodDocum = "02".
      WHEN "УдЛичРФ"      THEN mKodDocum = "03".
      WHEN "ВоенныйБилет" THEN mKodDocum = "04".
      WHEN "ВоенБилетЗап" THEN mKodDocum = "04".
      WHEN "ПаспМор"      THEN mKodDocum = "05".
      WHEN "ДокНерез"     THEN mKodDocum = "06".
      WHEN "ИноБезГражд"  THEN mKodDocum = "07".
      WHEN "ВремПрож"     THEN mKodDocum = "08".
      WHEN "ВидНаЖит"     THEN mKodDocum = "09".
      WHEN "ИнойДок"      THEN mKodDocum = "10".
      WHEN "ИнойДокПрож"  THEN mKodDocum = "11".
      WHEN "ВремУдРФ"     THEN mKodDocum = "13".
      WHEN "Документ"     THEN mKodDocum = "14".
   END CASE.
   
   PUT UNFORMATTED
      TRIM(STRING(person.person-id,">>>>>>9")) ";"
      "Заемщик;"
      mFam ";"
      mName ";"
      mOtch ";"
      (IF person.gender THEN "М" ELSE "Ж") ";"
      (IF person.country-id EQ "RUS" THEN "Да" ELSE "Нет") ";"
      STRING(person.birthday,"99/99/9999") ";"
      mPlace ";"
      mGrazhd ";"
      mKodDocum ";"
      mDateVyd ";"
      ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ") ";"
      ENTRY(3,person.document," ") ";"
      mKemVyd ";"
      mKodPodr ";"
      /*Адрес Регистрации*/
      TRIM(STRING(mIndex1,"999999")) ";"
      TRIM(mCountryName1) ";"
      TRIM(STRING(mOKATO1,"99"))     ";"
      TRIM(SUBSTRING(mOblast1,R-INDEX(mOblast1," ") + 1)) ";"
	   TRIM(SUBSTRING(mOblast1,1,R-INDEX(mOblast1," "))) ";"
      TRIM(SUBSTRING(mRegion1,R-INDEX(mRegion1," ") + 1)) ";"
	   TRIM(SUBSTRING(mRegion1,1,R-INDEX(mRegion1," "))) ";"
	   TRIM(SUBSTRING(mPunkt11,R-INDEX(mPunkt11," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt11,1,R-INDEX(mPunkt11," "))) ";"
	   TRIM(SUBSTRING(mPunkt12,R-INDEX(mPunkt12," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt12,1,R-INDEX(mPunkt12," "))) ";"
	   TRIM(SUBSTRING(mStreet1,R-INDEX(mStreet1," ") + 1)) ";"
	   TRIM(SUBSTRING(mStreet1,1,R-INDEX(mStreet1," "))) ";"
      mHome1   ";"
      mKorpus1 ";"
      mStroen1 ";"
      mFlat1   ";"
      mAdress1 ";"
      /*Адрес Пребывания*/
      TRIM(STRING(mIndex2,"999999")) ";"
      TRIM(mCountryName2) ";"
      TRIM(STRING(mOKATO2,"99"))";"
      TRIM(SUBSTRING(mOblast2,R-INDEX(mOblast2," ") + 1)) ";"
	   TRIM(SUBSTRING(mOblast2,1,R-INDEX(mOblast2," "))) ";"
	   TRIM(SUBSTRING(mRegion2,R-INDEX(mRegion2," ") + 1)) ";"
	   TRIM(SUBSTRING(mRegion2,1,R-INDEX(mRegion2," "))) ";"
	   TRIM(SUBSTRING(mPunkt21,R-INDEX(mPunkt21," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt21,1,R-INDEX(mPunkt21," "))) ";"
	   TRIM(SUBSTRING(mPunkt22,R-INDEX(mPunkt22," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt22,1,R-INDEX(mPunkt22," "))) ";"
	   TRIM(SUBSTRING(mStreet2,R-INDEX(mStreet2," ") + 1)) ";"
	   TRIM(SUBSTRING(mStreet2,1,R-INDEX(mStreet2," "))) ";"
      mHome2   ";"
      mKorpus2 ";"
      mStroen2 ";"
      mFlat2   ";"
      mAdress2 ";"      
      /*Адрес Почтовый*/
      TRIM(STRING(mIndex2,"999999")) ";"
      TRIM(mCountryName2) ";"
      TRIM(STRING(mOKATO2,"99"))     ";"
      TRIM(SUBSTRING(mOblast2,R-INDEX(mOblast2," ") + 1)) ";"
	   TRIM(SUBSTRING(mOblast2,1,R-INDEX(mOblast2," "))) ";"
	   TRIM(SUBSTRING(mRegion2,R-INDEX(mRegion2," ") + 1)) ";"
	   TRIM(SUBSTRING(mRegion2,1,R-INDEX(mRegion2," "))) ";"
	   TRIM(SUBSTRING(mPunkt21,R-INDEX(mPunkt21," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt21,1,R-INDEX(mPunkt21," "))) ";"
	   TRIM(SUBSTRING(mPunkt22,R-INDEX(mPunkt22," ") + 1)) ";"
	   TRIM(SUBSTRING(mPunkt22,1,R-INDEX(mPunkt22," "))) ";"
	   TRIM(SUBSTRING(mStreet2,R-INDEX(mStreet2," ") + 1)) ";"
	   TRIM(SUBSTRING(mStreet2,1,R-INDEX(mStreet2," "))) ";"
      mHome2   ";"
      mKorpus2 ";"
      mStroen2 ";"
      mFlat2   ";"
      mAdress2 ";"
      TRIM(ENTRY(1,person.phone[2]),",") ";"  /*PhoneWork	Телефон рабочий   1[2]*/
      person.fax ";"                          /*PhoneFax	Факс                     */
      mEMail  ";"                             /*Email	E-mail                      */
      mWebPage ";"                            /*WebPage	Страница в Интернет      */
      person.phone[1] ";"                     /*PhoneHome	Телефон домашний   [1]*/
      TRIM(ENTRY(2,person.phone[2]),",") ";"  /*PhoneMobil	Телефон мобильный 2[2]*/
      CHR(13)
   SKIP.
END.

OUTPUT CLOSE.

MESSAGE 
"Информация по клиентам выгружена в файл " + mFileName + "." 
VIEW-AS ALERT-BOX.

RETURN.

