 /*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: Вытаскиваем адрес из бисквита по person_id который указан в первом столбце, в csv файле. Для 
      партионной почты. и созданию уведомлений для рассылки
   Parameters:
         Uses:
      Used by:
      Created: ZSS
     Modified: 
*/



/* Пробуем конвертнуть в дату */
/* Входная строка в формате 11.11.11  или 11.11.2011 */
FUNCTION ConvertDate RETURN DATE(INPUT iDate AS CHARACTER):
    DEFINE VAR returnDate AS DATE NO-UNDO INIT ?.
    DEFINE VAR mTmpStr AS CHAR NO-UNDO.
    DEFINE VAR mTmpDay AS INT64 NO-UNDO.
    DEFINE VAR mTmpMonth AS INT64 NO-UNDO.
    DEFINE VAR mTmpYear AS INT64 NO-UNDO.
    DEFINE VAR mTmpDate AS DATE NO-UNDO.

    ERROR-STATUS:ERROR = NO.
    mTmpStr = REPLACE(iDate,"/",".").
        IF LENGTH(mTmpStr) = 8 OR LENGTH(mTmpStr) = 10 THEN DO:
            mTmpDay = INT(SUBSTRING(mTmpStr,1,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpMonth = INT(SUBSTRING(mTmpStr,4,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpYear = INT(SUBSTRING(mTmpStr,7)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpDate = DATE (mTmpMonth,mTmpDay,mTmpYear) NO-ERROR.
            IF ERROR-STATUS:ERROR = NO THEN DO:
                returnDate = mTmpDate.
            END.
        END.
    RETURN returnDate.
END FUNCTION.

/* Проверяет, является ли строка - числом */
FUNCTION ConvertDecimal RETURN DECIMAL (iStr AS CHAR):
   DEF VAR vRes AS DECIMAL NO-UNDO.
   iStr = REPLACE(iStr,' ','').
   iStr = REPLACE(iStr,',','.').
   ERROR-STATUS:ERROR = NO.
   vRes = DECIMAL(iStr) NO-ERROR.
   IF ERROR-STATUS:ERROR = YES THEN vRes = -1.
   RETURN vRes.
END FUNCTION.

{sh-defs.i}
/* {ksh-defs.i NEW} */
{globals.i}  
 {topkind.def} 
/*
{intrface.get tmess}
{intrface.get pbase} */
{intrface.get xclass}
{clnt.fun}

/* DEF VAR fname AS CHAR VIEW-AS FILL-IN NO-UNDO. */
DEF VAR ffname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.

DEF VAR sCode AS CHAR NO-UNDO.
DEF VAR sName AS CHAR NO-UNDO.

DEF VAR sNameF AS CHAR NO-UNDO.
DEF VAR sNameOI AS CHAR NO-UNDO.
DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.


DEF VAR sName1 AS CHAR NO-UNDO.
DEF VAR sDate AS DATE NO-UNDO.
DEF VAR sOpDate AS DATE FORMAT "99/99/9999" LABEL "Введите дату реестра" INIT TODAY NO-UNDO.
DEF VAR sSum AS DECIMAL NO-UNDO.
DEF VAR sNewSum AS DECIMAL NO-UNDO.
DEF VAR iInt AS INT64 NO-UNDO.
DEF VAR ost40817 AS DECIMAL NO-UNDO.
DEF VAR AcctCesRS AS CHAR NO-UNDO.



DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.



DEFINE VARIABLE mFam                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtch                 AS CHARACTER NO-UNDO.


DEFINE VARIABLE mPerson_id            AS INT64 NO-UNDO.


DEFINE VARIABLE mAdress2              AS CHARACTER NO-UNDO.

DEFINE VARIABLE mAdressPoln              AS CHARACTER NO-UNDO.


DEFINE VARIABLE mCountry2             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountryName2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIndex2               AS CHARACTER     NO-UNDO.
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


DEFINE TEMP-TABLE ttOp

        FIELD klietnid LIKE person.PERSON-ID
        FIELD address LIKE person.address[1]
        FIELD mess AS CHAR  
.

/*
PAUSE 0.
UPDATE SKIP(1) sOpDate  SKIP(1)
  WITH FRAME fMain OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE "Введите дату реестра".
HIDE FRAME fMain.
*/
{getdate.i}

sOpDate = end-date.
        
ffname = '/data/home/zss/samba/polovinkina/' . 

{getfile.i &filename=ffname &mode=must-exist }

IF SEARCH (fname) = ? THEN DO:
  MESSAGE ("Файл не найден " + fname) VIEW-AS ALERT-BOX.
  RETURN.
END.


iInt = 0.
{empty ttOp}
INPUT FROM VALUE(fname) CONVERT TARGET "ibm866"  SOURCE "1251".

REPEAT ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED fstr.
        fstr = REPLACE(fstr,'"','').
        iInt = iInt + 1.
   
        CREATE ttOp.
                

                mPerson_id  = int(TRIM(ENTRY(1,fstr,';'))).
                /* sNameF = ENTRY(2,fstr,';').
                sNameOI = ENTRY(3,fstr,';'). */
              /*   MESSAGE sNameF  mPerson_id VIEW-AS ALERT-BOX. */





                  FIND FIRST person WHERE /* person.NAME-LAST = sNameF */
                   person.PERSON-ID  = mPerson_id  
                                          /* AND person.FIRST-NAMES = sNameOI */
                        NO-LOCK NO-ERROR.
               /*    
                  IF AVAIL PERSON THEN DO:  */
                   ASSIGN 
                       ttOp.klietnid = mPerson_id
                        .
                  /*  END. */

                   
                /*   ELSE DO : */
               /*      ttOp.mess = 'клиет с PERSON-ID= ' + string(mPerson_id) + 'не найден'.
                  END.   */
                
           
                  RELEASE ttOp.

          END.

/* MESSAGE ttOp.address VIEW-AS ALERT-BOX.
 */

mFileName = "client_postrussia"  + ".txt".

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
      "ПОЛНЫЙ АдресРег"
      CHR(13)
 SKIP.



FOR EACH ttOp NO-LOCK:
  /*  DISPL ttOp.Fameli ';' ttOp.name-oth  ';'  ttOp.address . */


              ASSIGN
                  mFam      = GetChckAttrByClnt('Ч', ttOp.klietnid,"Фам")
                  mName     = GetChckAttrByClnt('Ч', ttOp.klietnid,"Имя")
                  mOtch     = GetChckAttrByClnt('Ч', ttOp.klietnid,"Отч")


                  mAdress2  = GetChckAttrByClnt('Ч', ttOp.klietnid,"АдресРег")
                  
 /* mAdress2  = GetChckAttrByClnt('Ч', 290645,"АдресРег") */


                  mCountry2 = GetEntries(1,mAdress2,",","")
                  mIndex2   = STRING(GetEntries(2,mAdress2,",",""))
                 /*  mOKATO2   = INT64(GetEntries(3,mAdress2,",","")) */
                  mOblast2  = GetCodeName("КодРег",STRING(mOKATO2,"99999"))
                  mRegion2  = GetEntries(4,mAdress2,",","")
                  mPunkt21  = GetEntries(5,mAdress2,",","")
                  mPunkt22  = GetEntries(6,mAdress2,",","")
                  mStreet2  = GetEntries(7,mAdress2,",","")
                  mHome2    = GetEntries(8,mAdress2,",","")
                  mFlat2    = GetEntries(10,mAdress2,",","")
                  mKorpus2  = GetEntries(9,mAdress2,",","")
                  mKorpus2 = REPLACE(mKorpus2,' ','')
                  mKorpus2 = REPLACE(mKorpus2,'к','')  
                
              .

/* MESSAGE mAdress2  VIEW-AS ALERT-BOX.

                      MESSAGE  ttOp.klietnid VIEW-AS ALERT-BOX. */

              ASSIGN      
                   mAdressPoln  = 
                                TRIM(STRING(mIndex2,"999999")) + "," + 
                                TRIM(mPunkt21) + "," + 
                                TRIM(mPunkt22) + "," + 
                                TRIM(mStreet2) + "," + 
                                TRIM(mHome2)   + "," + 
                                TRIM(mKorpus2) + "," + 
                                TRIM(mStroen2) + "," +
                                TRIM(mFlat2)
              .        

mAdressPoln = DelDoubleChars(mAdressPoln, ',').

/* MESSAGE mAdressPoln  VIEW-AS ALERT-BOX.
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
      "^"
      "^"
      TRIM(mKorpus2) "^"
      TRIM(mStroen2) "^"
      "^"
      TRIM(mFlat2) "^"
      mFam " "
      mName " "
      mOtch "^"
    "0.02" "^"    
      "^"    
      "^"    
      "^"    
      "^"    
      "^"    
      "^"   
      "^"
      "^"
      mAdressPoln
 
      CHR(13)
   SKIP.

 END.

mESSAGE 
"Информация по клиентам выгружена в файл " + mFileName + "." 
VIEW-AS ALERT-BOX.

OUTPUT CLOSE.

{intrface.del}

{setdest.i}
        FOR EACH ttOp NO-LOCK:
                PUT UNFORMATTED ttOp.mess skip.
        END.
{preview.i}

 
 
