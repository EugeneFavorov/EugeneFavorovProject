{globals.i}

{sh-defs.i}
{tmprecid.def}
{clnt.fun}


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
DEF VAR mTel as CHAR NO-UNDO.
def var mSerNom as char no-undo.
mFileName = "postrussia2"  + ".txt".

OUTPUT TO VALUE(mFileName) CONVERT TARGET "1251".

 PUT UNFORMATTED
      "NUM" "^"
      "INDEXTO" "^"
      "COUNTRY" "^"
      "ADDRESSTYPE" "^"
      "NUMADDRESSTYPE" "^"
      "REGION" "^"
      "AREA" "^"
      "CITY" "^"
      "LOCATION" "^"
      "STREET" "^"
      "HOUSE" "^"
      "LETTER" "^"
      "SLASH" "^"
      "CORPUS" "^"
      "BUILDING" "^"
      "HOTEL" "^"
      "ROOM" "^"
      "ADRESAT" "^"
      "MASS" "^"
      "VLENGTH" "^"
      "VWIDTH" "^"
      "VHEIGHT" "^"
      "VALUE" "^"
      "PAYMENT" "^"
      "COMMENT" "^"
      "TELADDRESS" "^"
      "NUM_CONTRACT" "^"
      
      CHR(13)
 SKIP.

def buffer bloan for loan.
  
FOR EACH TmpRecID NO-LOCK,
   FIRST bloan WHERE
   RECID(bloan) EQ TmpRecID.id
   NO-LOCK BY bloan.cont-code:
   find first loan where
   loan.class-code = "insurance"
   AND loan.contract EQ 'СТРАХ'
   AND loan.parent-cont-code EQ bloan.cont-code
   and  loan.parent-contract EQ 'Кредит'
   NO-LOCK no-error.


  


 if avail loan then do:


   find first cust-corp 
 WHERE
   cust-corp.cust-id EQ loan.cust-id
   NO-LOCK no-error.

      mInt = mInt + 1.
   
   mAdress1 = "".
   mAdress2 = "".

/*<код страны>,<индекс>,<код региона>,<район>,<город>,<населенный пункт>,<улица>,<дом>,<корпус>,<строение>,<квартира>.
^643   ,659572,01,Быстроистокский р-н,,Новопокровка с                 ,Октябрьская ул ,36,,,1
^РОССИЯ^659572^01^Быстроистокский р-н^^Новопокровка с^^Новопокровка с^^Октябрьская ул^^36^^^1^
^РОССИЯ^659572^01^Быстроистокский    ^р-н^              ^^              ^^              ^^  ^^^^^^^
*/
      
   ASSIGN
      mFam      = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP
      mName     = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Имя")
      mOtch     = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Отч")
      mDateVyd  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"ДатаВыдачи")
      mKemVyd   = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"КемВыданДок")
      mKodPodr  = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"КодПодр")
      mPlace    = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"МестоРожд")
      mInn      = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"ИНН")
      mTel      = GetChckAttrByClnt(loan.cust-cat,loan.cust-id,"Тел")
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
      mAdress1  = TRIM(mCountry1) + "," + 
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
      mAdress2  = TRIM(mCountry2) + "," + 
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

/*
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
*/

/*     
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
*/

/*
 = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id),"tel","").
*/
   PUT UNFORMATTED
      "^"
      TRIM(STRING(mIndex2,"999999")) "^"
      mCountryName2 "^"
      "стандартный" "^"
      "^"
      TRIM(mOblast2) "^"
      TRIM(mRegion2) "^"
      TRIM(TRIM(mPunkt21) + " " + TRIM(mPunkt22)) "^"
      "^"
      TRIM(mStreet2) "^"
      TRIM(mHome2) "^"
      TRIM(mKorpus2) "^"
      "^"
      "^"
      TRIM(mStroen2) "^"
      "^"
      TRIM(mFlat2) "^"
      
      mFam " "
      mName " "
      mOtch "^"
      "^"    
      "^"    
      "^"    
      "^"    
      "^"    
      "^"    
      "^"    
      mTel

      CHR(13)
   SKIP.
END.

end.

OUTPUT CLOSE.

MESSAGE 
"Информация по клиентам выгружена в файл " + mFileName + "." 
VIEW-AS ALERT-BOX.

RETURN.

