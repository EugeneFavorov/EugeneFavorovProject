{globals.i}
{intrface.get pbase}
{intrface.get cust}
{intrface.get xobj}
{intrface.get db2l}
{intrface.get xclass}
{intrface.get terr}

{parsin.def}
{sh-defs.i}
{tmprecid.def}

DEFINE VARIABLE mInt       AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt       AS INT64     NO-UNDO.
DEFINE VARIABLE mString    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFIO       AS CHARACTER NO-UNDO.         
DEFINE VARIABLE mCustName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddress   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINN       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDolRuk    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClassCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpenDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mCloseDate AS DATE      NO-UNDO.
DEFINE VARIABLE mPrim      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk        AS LOGICAL   NO-UNDO.

DEFINE BUFFER u-signs FOR signs.
DEFINE BUFFER f-signs FOR signs.

DEFINE VARIABLE mCustID      AS INT64     NO-UNDO.
DEFINE VARIABLE mIs550       AS INT64     NO-UNDO.
DEFINE VARIABLE mIsTerr      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mNumber    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMess      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnswer    AS CHARACTER NO-UNDO.

{setdest.i &filename = "'chk-loan-pers.csv'" &option = "CONVERT TARGET '1251'"}

FOR EACH tmprecid NO-LOCK,
   FIRST loan WHERE RECID(loan) = tmprecid.id 
   NO-LOCK:

   FIND FIRST person WHERE
      person.person-id = loan.cust-id
   NO-LOCK NO-ERROR.
   IF AVAIL(person) THEN
   DO:
      mCustID = person.person-id.
      FOR EACH cust-ident WHERE TRUE
         AND cust-ident.close-date EQ ?
         AND cust-ident.class-code EQ 'p-cust-ident'
         AND cust-ident.cust-cat EQ 'Ч'
         AND cust-ident.cust-id EQ person.person-id 
         AND cust-ident.cust-code-type EQ 'Паспорт'
         NO-LOCK:
         mNumber = cust-ident.cust-code.
         LEAVE.
      END.
      
      mIs550 = INDEX(GetXAttrValueEx("person",STRING(mCustID),"ОценкаРиска",""),"550").
      
      FOR EACH code WHERE TRUE
         AND code.class   EQ "StopList"
         AND code.parent  EQ "StopList"
         AND code.misc[1] EQ "Ч"
         AND code.misc[2] EQ STRING(mCustID)
         NO-LOCK:
         LEAVE.      
      END.
   
      mIsTerr = CompareName(person.name-last + " " + person.first-names,'plat').
      
      RUN chk-pipe IN THIS-PROCEDURE
         (INPUT  mNumber,
          OUTPUT mAnswer,
          OUTPUT mMess).
   
      mInt = mInt + 1.
      PUT UNFORMATTED
         mInt       ";"
         loan.cont-code ";"
         mCustID    ";"
         person.name-last + " " + person.first-names ";"
         mNumber    ";"
         IF mIs550 GT 0    THEN "550П = Да"                  ELSE "550П = Нет"               ";"
         IF AVAIL(code)    THEN "Стоп-Лист = Да"             ELSE "Стоп-Лист = Нет"          ";"
         IF mIsTerr        THEN "Террорист = Да"             ELSE "Террорист = Нет"          ";"
         IF mAnswer EQ "1" THEN "Паспорт = Недействительный" ELSE "Паспорт = Действительный" ";"
   	SKIP.
   END.
END.

{preview.i &filename = "'chk-loan-pers.csv'"}

{intrface.del}

RETURN.

PROCEDURE chk-pipe:
   DEFINE INPUT  PARAMETER iNumber AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oAnswer AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess   AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE mHandle  AS INT64     NO-UNDO.
   DEFINE VARIABLE iStat    AS INT64     NO-UNDO.

   DEFINE VARIABLE vTmpNum  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSeria   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNumber  AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE P_SERIES AS CHARACTER NO-UNDO.
   DEFINE VARIABLE P_NUMBER AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vInt     AS INT64     NO-UNDO.

   iNumber = REPLACE(iNumber," ","").
   DO vInt = 1 TO LENGTH(iNumber) - 9:
      vTmpNum = SUBSTRING(iNumber,vInt,10).
      IF TRIM(vTmpNum,"1234567890") EQ ""
         AND LENGTH(TRIM(vTmpNum)) EQ 10 THEN
      DO:
         ASSIGN
            vSeria  = SUBSTRING(vTmpNum,1,4)
            vNumber = SUBSTRING(vTmpNum,5,6).
         LEAVE.
      END.
   END.

   RUN STORED-PROCEDURE IS_PASSP_WANTED_P mHandle = PROC-HANDLE
      (
      INPUT  PARAM P_SERIES = vSeria,   /*"5208",*/
      INPUT  PARAM P_NUMBER = vNumber,  /*"628886",*/
      OUTPUT PARAM P_REZULT = ?         /*-1*/
      ).
   CLOSE STORED-PROC IS_PASSP_WANTED_P iStat = PROC-STATUS.

   IF iStat = 0 THEN
   ASSIGN
      oAnswer = STRING(P_REZULT)
      oMess   = "".
   ELSE
   ASSIGN
      oAnswer = STRING(P_REZULT)
      oMess   = ERROR-STATUS:GET-MESSAGE(1).
END PROCEDURE.

