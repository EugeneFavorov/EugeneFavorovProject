{globals.i}

{sh-defs.i}
{tmprecid.def}
{clnt.fun}

DEFINE PARAMETER BUFFER iTmpRecID FOR tmprecid.
DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE mAdress1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdress2              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdress11              AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdress22              AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCountry1             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountryName1         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIndex1               AS INT64     NO-UNDO.
DEFINE VARIABLE mOKATO1               AS INT64     NO-UNDO.
DEFINE VARIABLE mOKATO11               AS INT64     NO-UNDO.
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
DEFINE VARIABLE mOKATO22               AS INT64     NO-UNDO.
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
def var mSerNom as char no-undo.
mFileName = "./tag" + TRIM(iTag) + ".txt".

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
/*      mTelMob   = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"ТелМоб")*/
      mEMail    = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"EMail")

      mAdress1  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресРег")
      mAdress11  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресРегГНИ")
      mCountry1 = GetEntries(1,mAdress1,",","")
      mIndex1   = INT64(GetEntries(2,mAdress1,",",""))
      mOKATO1   = INT64(GetEntries(3,mAdress1,",",""))
      mOKATO11   = INT64(GetEntries(3,mAdress11,",",""))
      mOblast1  = GetCodeName("КодРег",STRING(mOKATO1,"99999"))
      mRegion1  = GetEntries(4,mAdress1,",","")
      mPunkt11  = GetEntries(5,mAdress1,",","")
      mPunkt12  = GetEntries(6,mAdress1,",","")
      mStreet1  = GetEntries(7,mAdress1,",","")
      mHome1    = GetEntries(8,mAdress1,",","")
      mFlat1    = GetEntries(10,mAdress1,",","")
      mKorpus1  = GetEntries(9,mAdress1,",","")
      
      mAdress2  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресФакт")
      mAdress22  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"АдресФактГНИ")
      mCountry2 = GetEntries(1,mAdress2,",","")
      mIndex2   = INT64(GetEntries(2,mAdress2,",",""))
      mOKATO2   = INT64(GetEntries(3,mAdress2,",",""))
      mOKATO22   = INT64(GetEntries(3,mAdress22,",",""))
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
      mAdress1  = TRIM(mCountry1) + "," + 
                  TRIM(STRING(mIndex1,"999999")) + "," + 
                  TRIM(STRING(mOKATO11,"99"))     + "," +
                  TRIM(mRegion1)                 + "," +
                  TRIM(mPunkt11) + "," + 
                  TRIM(mPunkt12) + "," + 
                  TRIM(mStreet1) + "," + 
                  TRIM(mHome1)   + "," + 
                  TRIM(mKorpus1) + "," + 
                  TRIM(mStroen1) + "," +
                  TRIM(mFlat1).
   ASSIGN      
      mAdress2  = TRIM(mCountry2) + "," + 
                  TRIM(STRING(mIndex2,"999999")) + "," + 
                  TRIM(STRING(mOKATO22,"99"))     + "," +
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
   def var entry3 as char no-undo init ''.
   if num-entries(person.document," ") > 2 then
  	entry3 = ENTRY(3,person.document," ").
 
   mSerNom = person.document.
   if num-entries(person.document," ") > 1 then
    mSerNom = ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ").
   if num-entries(person.document,"/") > 1 then do:
    mSerNom = ENTRY(2,person.document,"/").
    entry3 =  ENTRY(1,person.document,"/").
   end.

   PUT UNFORMATTED
      STRING(person.person-id,">>>>>>9") "^"
      mFam "^"
      mName "^"
      mOtch "^"
      (IF person.gender THEN "М" ELSE "Ж") "^"
      (IF person.country-id EQ "RUS" THEN "Да" ELSE "Нет") "^"
      STRING(person.birthday,"99/99/9999") "^"
      GetCode("КодДокум",person.document-id) "^"
      mDateVyd "^"
      mSerNom "^"
      entry3 "^"
      REPLACE(mKemVyd,CHR(10),"") "^"
      mKodPodr "^"
      person.country-id "^"
      mPlace "^"

      mCountryName1 "^"                               /*Регистрации*/
      TRIM(STRING(mIndex1,"999999")) "^"
      TRIM(STRING(mOKATO11,"99"))     "^"
      TRIM(SUBSTRING(mOblast1,R-INDEX(mOblast1," ") + 1)) "^"
	   TRIM(SUBSTRING(mOblast1,1,R-INDEX(mOblast1," "))) "^"
      TRIM(SUBSTRING(mRegion1,R-INDEX(mRegion1," ") + 1)) "^"
	   TRIM(SUBSTRING(mRegion1,1,R-INDEX(mRegion1," "))) "^"
	   TRIM(SUBSTRING(mPunkt11,R-INDEX(mPunkt11," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt11,1,R-INDEX(mPunkt11," "))) "^"
	   TRIM(SUBSTRING(mPunkt12,R-INDEX(mPunkt12," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt12,1,R-INDEX(mPunkt12," "))) "^"
	   TRIM(SUBSTRING(mStreet1,R-INDEX(mStreet1," ") + 1)) "^"
	   TRIM(SUBSTRING(mStreet1,1,R-INDEX(mStreet1," "))) "^"
      mHome1   "^"
      mKorpus1 "^"
      mStroen1 "^"
      mFlat1   "^"
      mAdress1 "^"
      
      mCountryName2 "^"                               /*Пребывания*/
      TRIM(STRING(mIndex2,"999999")) "^"
      TRIM(STRING(mOKATO22,"99"))"^"
      TRIM(SUBSTRING(mOblast2,R-INDEX(mOblast2," ") + 1)) "^"
	   TRIM(SUBSTRING(mOblast2,1,R-INDEX(mOblast2," "))) "^"
	   TRIM(SUBSTRING(mRegion2,R-INDEX(mRegion2," ") + 1)) "^"
	   TRIM(SUBSTRING(mRegion2,1,R-INDEX(mRegion2," "))) "^"
	   TRIM(SUBSTRING(mPunkt21,R-INDEX(mPunkt21," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt21,1,R-INDEX(mPunkt21," "))) "^"
	   TRIM(SUBSTRING(mPunkt22,R-INDEX(mPunkt22," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt22,1,R-INDEX(mPunkt22," "))) "^"
	   TRIM(SUBSTRING(mStreet2,R-INDEX(mStreet2," ") + 1)) "^"
	   TRIM(SUBSTRING(mStreet2,1,R-INDEX(mStreet2," "))) "^"
      mHome2   "^"
      mKorpus2 "^"
      mStroen2 "^"
      mFlat2   "^"
      mAdress2 "^"      
      TRIM(person.phone[1],",") "^"
      TRIM(person.phone[2],",") "^"
      mEMail  "^"
      mInn    "^"   /*ИНН*/
      ""      "^"   /*СНИЛС*/
      
      mCountryName2 "^"                               /*Почтовый*/
      TRIM(STRING(mIndex2,"999999")) "^"
      TRIM(STRING(mOKATO22,"99"))     "^"
      TRIM(SUBSTRING(mOblast2,R-INDEX(mOblast2," ") + 1)) "^"
	   TRIM(SUBSTRING(mOblast2,1,R-INDEX(mOblast2," "))) "^"
	   TRIM(SUBSTRING(mRegion2,R-INDEX(mRegion2," ") + 1)) "^"
	   TRIM(SUBSTRING(mRegion2,1,R-INDEX(mRegion2," "))) "^"
	   TRIM(SUBSTRING(mPunkt21,R-INDEX(mPunkt21," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt21,1,R-INDEX(mPunkt21," "))) "^"
	   TRIM(SUBSTRING(mPunkt22,R-INDEX(mPunkt22," ") + 1)) "^"
	   TRIM(SUBSTRING(mPunkt22,1,R-INDEX(mPunkt22," "))) "^"
	   TRIM(SUBSTRING(mStreet2,R-INDEX(mStreet2," ") + 1)) "^"
	   TRIM(SUBSTRING(mStreet2,1,R-INDEX(mStreet2," "))) "^"
      mHome2   "^"
      mKorpus2 "^"
      mStroen2 "^"
      mFlat2   "^"
      mAdress2 "^"
      
      "Нет" "^"   /*Отношение к ИПДЛ*/ 
      ""    "^"   /*Место работы ИПДЛ*/
      "Нет" "^"   /*Родственник ИПДЛ*/
      "Нет" "^"   /*Отношение к РПДЛ*/
      "Нет" "^"   /*Родственник РПДЛ*/
      "Нет" "^"   /*Отношение к МПДЛ*/
      "Нет" "^"   /*Родственник МПДЛ*/
      "Нет" "^"   /*Наличие бенефициарного владельца*/
      "Нет" "^"   /*Наличие выгодоприобретателя*/
      "Разовые операции"       "^"  /*Сведения о целях установления деловых отношений*/
      "Иная деятельность"      "^"  /*Сведения о предполагаемом характере деловых отношений*/
      "Устойчивое"             "^"  /*Сведения о финансовом положении*/
      "Удовлетворительная"     "^"  /*Сведения о деловой репутации*/
      "Работа по найму"        "^"  /*Сведения об источниках происхождения денежных средств и (или) иного имущества*/
      "Совпадения НЕ выявлены"      /*Сведения о проверке с перечнем экстремистов*/

      CHR(13)
   SKIP.
END.

OUTPUT CLOSE.

MESSAGE 
"Информация по клиентам выгружена в файл " + mFileName + "." 
VIEW-AS ALERT-BOX.

RETURN.

