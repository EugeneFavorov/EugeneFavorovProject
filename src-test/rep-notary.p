/*
 Процедура сканирует таблицу 
NOTARY_ONLINE.PLEDGENOTIFICATION
и проставляет ДР 
NotifRefNumber       Регистрационный номер уведомления о возн
NotifRegDate         Дата регистрации уведомления
на term-obl-gar
*/
{globals.i}
{intrface.get xclass}
{intrface.get lngar}

/*
 ФИО заемщика
 Номер КД
 Дата КД
 VIN
Рег.номер о возникновении залога
Номер сообщения о постановке
Дата  сообщения о постановке
Номер сообщения о снятии
Дата  сообщения о снятии
*/

DEFINE TEMP-TABLE ttNotif
   FIELD pledgeid  AS INT64
   FIELD ntype     AS INT64
   FIELD fio       AS CHARACTER
   FIELD cont-date AS DATE
   FIELD cont-code AS CHARACTER
   FIELD vin       AS CHARACTER
   FIELD reg-nom   AS CHARACTER
   FIELD rnumber   AS CHARACTER
   FIELD rtime     AS CHARACTER
   FIELD din       AS CHARACTER
   FIELD nold      AS LOGICAL
   INDEX cont-code cont-code. 

DEFINE TEMP-TABLE ttLoan
   FIELD pidinold  AS INT64
   FIELD pidoutold AS INT64
   FIELD pidchold  AS INT64
   FIELD pidin     AS INT64
   FIELD pidout    AS INT64
   FIELD pidch     AS INT64
   FIELD ntype     AS INT64
   FIELD fio       AS CHARACTER
   FIELD cont-date AS DATE
   FIELD cont-code AS CHARACTER
   FIELD vin       AS CHARACTER
   FIELD reg-nom   AS CHARACTER
   FIELD din       AS CHARACTER
   FIELD nin       AS CHARACTER
   FIELD dout      AS CHARACTER
   FIELD nout      AS CHARACTER
   FIELD dch       AS CHARACTER
   FIELD nch       AS CHARACTER
   FIELD dinold    AS CHARACTER
   FIELD ninold    AS CHARACTER
   FIELD doutold   AS CHARACTER
   FIELD noutold   AS CHARACTER
   FIELD dchold    AS CHARACTER
   FIELD nchold    AS CHARACTER
   FIELD nold      AS LOGICAL
   INDEX cont-code cont-code.

DEFINE VARIABLE mNotifID AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mInt              AS INT64     NO-UNDO.
DEFINE VARIABLE mInt0             AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3             AS INT64     NO-UNDO.
DEFINE VARIABLE mInt4             AS INT64     NO-UNDO.
DEFINE VARIABLE mInt5             AS INT64     NO-UNDO.
DEFINE VARIABLE mTermObl          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOp               AS CHARACTER NO-UNDO.

DEFINE VARIABLE mVidObesp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRegZalog            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRefDate        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRefNumber      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryDate1Post     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumb1Post     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryId1Post       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryDate2Out      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumb2Out      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryId2Out        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryDate3Change   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumb3Change   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryId3Change     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mPriznak          AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNotaryDatePost   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumbPost   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryIdPost     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNotaryDateOut    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumbOut    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryIdOut      AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNotaryDateChange AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryNumbChange AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotaryIdChange   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mOldNumberPost    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNewNumberPost    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOldNumberOut     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNewNumberOut     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOldNumberChange  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNewNumberChange  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mNotaryErrText    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mBegDateTime      AS DATETIME  NO-UNDO.
DEFINE VARIABLE mEndDateTime      AS DATETIME  NO-UNDO.

{setdest.i &filename = "'rep-notary.log'"}

DEFINE BUFFER NF     FOR PLEDGENOTIFICATION.
DEFINE BUFFER NFOLD  FOR NOTIFICATIONS.
DEFINE BUFFER PP     FOR PERSONALPROPERTIES-1.
DEFINE BUFFER PPOLD  FOR PERSONALPROPERTIES.
DEFINE BUFFER PMP    FOR PLEDGEMESSAGEPLEDGORS-1.
DEFINE BUFFER PMPOLD FOR PLEDGEMESSAGEPLEDGORS.
DEFINE BUFFER PPP    FOR PLEDGEPRIVATEPERSON-1.
DEFINE BUFFER PPPOLD FOR PLEDGEPRIVATEPERSON.

mBegDateTime = DATETIME(1,1,2016,0,0).

PUT UNFORMATTED "rep-notary" SKIP.

/*NEW*/
mInt = 0.
FOR EACH NF WHERE TRUE
/*   AND NF.PLEDGEID EQ 65615*/
/*   AND NF.CONTRACTNUMBER EQ "45-00-92522-ГАПНА"*/
/*   AND NF.NOTIFICATIONREFERENCENUMBER BEGINS "2015-000-004979-306"*/
/*   AND NF.PLEDGEID EQ INT64(mNotifID)*/
/*   AND NF.STATUS_ EQ 7                          */
/*   AND NF.CREATETIME GE mBegDateTime*/
   NO-LOCK,
   EACH PP WHERE TRUE
   AND  PP.PLEDGEID EQ NF.PLEDGEID
   NO-LOCK,
   EACH PMP WHERE TRUE
   AND  PMP.PLEDGEID EQ NF.PLEDGEID
   NO-LOCK,
   EACH PPP WHERE TRUE
   AND  PPP.CLIENTID EQ PMP.CLIENTID
   NO-LOCK BREAK BY NF.CONTRACTNUMBER:
   
   mInt = mInt + 1.
   
   CREATE ttNotif.
   ASSIGN
      ttNotif.nold      = NO
      ttNotif.ntype     = NF.NOTIFICATIONTYPE
      ttNotif.fio       = PPP.LASTNAME + " " + PPP.FIRSTNAME + " " + PPP.MIDDLENAME
      ttNotif.cont-date = DATE(NF.CONTRACTDATE)
      ttNotif.cont-code = NF.CONTRACTNUMBER
      ttNotif.vin       = PP.ID
      ttNotif.pledgeid  = NF.PLEDGEID
      ttNotif.reg-nom   = NF.NOTIFICATIONREFERENCENUMBER
      ttNotif.rnumber   = NF.NOTIFICATIONREFERENCENUMBER
      ttNotif.rtime     = STRING(DATE(NF.REGISTRATIONTIME),"99/99/9999").
END.

/*OLD*/
mInt = 0.
FOR EACH NFOLD WHERE TRUE
/*   AND NFOLD.PLEDGEID EQ 65615*/
/*   AND NFOLD.CONTRACTNUMBER EQ "45-00-92522-ГАПНА"*/
/*   AND NFOLD.NOTIFICATIONREFERENCENUMBER BEGINS "2015-000-004979-306"*/
/*   AND NFOLD.PLEDGEID EQ INT64(mNotifID)*/
/*   AND NFOLD.STATUS_ EQ 7                          */
/*   AND NFOLD.CREATETIME GE mBegDateTime*/
   NO-LOCK,
   EACH PPOLD WHERE TRUE
   AND  PPOLD.PLEDGEID EQ NFOLD.NOTIFICATIONID
   NO-LOCK,
   EACH PMPOLD WHERE TRUE
   AND  PMPOLD.PLEDGEID EQ NFOLD.NOTIFICATIONID
   NO-LOCK,
   EACH PPPOLD WHERE TRUE
   AND  PPPOLD.CLIENTID EQ PMPOLD.CLIENTID
   NO-LOCK BREAK BY NFOLD.CONTRACTNUMBER:
   
   mInt = mInt + 1.
   
   CREATE ttNotif.
   ASSIGN
      ttNotif.nold      = YES
      ttNotif.ntype     = NFOLD.NOTIFICATIONTYPE
      ttNotif.fio       = PPPOLD.LASTNAME + " " + PPPOLD.FIRSTNAME + " " + PPPOLD.MIDDLENAME
      ttNotif.cont-date = DATE(NFOLD.CONTRACTDATE)
      ttNotif.cont-code = NFOLD.CONTRACTNUMBER
      ttNotif.vin       = PPOLD.ID
      ttNotif.pledgeid  = NFOLD.NOTIFICATIONID
      ttNotif.reg-nom   = NFOLD.NOTIFICATIONREFERENCENUMBER
      ttNotif.rnumber   = NFOLD.NOTIFICATIONREFERENCENUMBER
      ttNotif.rtime     = STRING(DATE(NFOLD.REGISTRATIONTIME),"99/99/9999").
END.

FOR EACH ttNotif 
   NO-LOCK BREAK BY ttNotif.cont-code:
/*   PUT UNFORMATTED                                                */
/*      "ttNotif;"                                                  */
/*      ttNotif.pledgeid  ";"                                       */
/*      IF ttNotif.nold THEN "old" ELSE "new" ";"                   */
/*      IF ttNotif.ntype EQ 1 THEN "постановка" ELSE "снятие"    ";"*/
/*      ttNotif.fio       ";"                                       */
/*      ttNotif.cont-date ";"                                       */
/*      ttNotif.cont-code ";"                                       */
/*      ttNotif.vin       ";"                                       */
/*      ttNotif.reg-nom   ";"                                       */
/*      ttNotif.rnumber   ";"                                       */
/*      ttNotif.rtime     ";"                                       */
/*   SKIP.                                                          */
END.

FOR EACH ttNotif 
   NO-LOCK BREAK BY ttNotif.cont-code:

   IF FIRST-OF(ttNotif.cont-code) THEN
   DO:
      CREATE ttLoan.
      ASSIGN
         ttLoan.fio       = ttNotif.fio
         ttLoan.cont-date = ttNotif.cont-date
         ttLoan.cont-code = ttNotif.cont-code
         ttLoan.vin       = ttNotif.vin
         ttLoan.reg-nom   = ttNotif.reg-nom.
   END.
 
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ YES
      AND ttNotif.ntype EQ 1 THEN
   DO:
      ttLoan.pidinold = ttNotif.pledgeid.
      ttLoan.dinold   = ttNotif.rtime.
      ttLoan.ninold   = ttNotif.rnumber.
   END.
 
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ YES
      AND ttNotif.ntype EQ 2 THEN
   DO:
      ttLoan.pidoutold = ttNotif.pledgeid.
      ttLoan.doutold   = ttNotif.rtime.
      ttLoan.noutold   = ttNotif.rnumber.
   END.
   
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ YES
      AND ttNotif.ntype EQ 3 THEN
   DO:
      ttLoan.pidchold = ttNotif.pledgeid.
      ttLoan.dchold   = ttNotif.rtime.
      ttLoan.nchold   = ttNotif.rnumber.
   END.
   
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ NO
      AND ttNotif.ntype EQ 1 THEN
   DO:
      ttLoan.pidin = ttNotif.pledgeid.
      ttLoan.din   = ttNotif.rtime.
      ttLoan.nin   = ttNotif.rnumber.
   END.
   
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ NO
      AND ttNotif.ntype EQ 2 THEN
   DO:
      ttLoan.pidout = ttNotif.pledgeid.
      ttLoan.dout   = ttNotif.rtime.
      ttLoan.nout   = ttNotif.rnumber.
   END.
   
   IF AVAIL(ttLoan)
      AND ttNotif.nold EQ NO
      AND ttNotif.ntype EQ 3 THEN
   DO:
      ttLoan.pidch = ttNotif.pledgeid.
      ttLoan.dch   = ttNotif.rtime.
      ttLoan.nch   = ttNotif.rnumber.
   END.
END.

mInt  = 0.
FOR EACH ttLoan 
   NO-LOCK:
   mInt = mInt + 1.
/*   PUT UNFORMATTED        */
/*      "ttLoan;"           */
/*      ttLoan.cont-date ";"*/
/*      ttLoan.cont-code ";"*/
/*      ttLoan.vin       ";"*/
/*      ttLoan.reg-nom   ";"*/
/*                          */
/*      ttLoan.pidinold  ";"*/
/*      ttLoan.dinold    ";"*/
/*      ttLoan.ninold    ";"*/
/*                          */
/*      ttLoan.pidoutold ";"*/
/*      ttLoan.doutold   ";"*/
/*      ttLoan.noutold   ";"*/
/*                          */
/*      ttLoan.pidchold  ";"*/
/*      ttLoan.dchold    ";"*/
/*      ttLoan.nchold    ";"*/
/*                          */
/*      ttLoan.pidin     ";"*/
/*      ttLoan.din       ";"*/
/*      ttLoan.nin       ";"*/
/*                          */
/*      ttLoan.pidout    ";"*/
/*      ttLoan.dout      ";"*/
/*      ttLoan.nout      ";"*/
/*                          */
/*      ttLoan.pidch     ";"*/
/*      ttLoan.dch       ";"*/
/*      ttLoan.nch       ";"*/
/*   SKIP.                  */
END.

/*
mInt  = 0.
FOR EACH ttLoan 
   NO-LOCK BREAK BY ttLoan.cont-code:

   mInt = mInt + 1.
   
/*   IF FIRST-OF(ttLoan.cont-code) THEN mInt = 0.*/
/*   mInt = mInt + 1.                            */
/*   IF mInt GT 1  THEN                          */
/*      PUT UNFORMATTED ttLoan.cont-code SKIP.   */
END.

PUT UNFORMATTED mInt SKIP.
*/

/*PUT UNFORMATTED "Договоры с 01/01/2016 по 31/12/2016 - номера постановки" SKIP.*/

/*PUT UNFORMATTED "Договоры с 01/01/2016 по 31/12/2016 - номера снятия" SKIP.*/

PUT UNFORMATTED "Договоры с 01/01/2016 по 31/12/2016 - номера замены" SKIP.

PUT UNFORMATTED
   "Номер договора;"
   "Дата договора;"
   "Статус договора;"
   "Признак;"
   "Номер залога;"
   "Номер в старой базе;"
   "Номер в новой базе;"
   "reg-zalog;"
   "NotifRefNumber;"
   "mNotaryNumb3Change;" /*"mNotaryNumb2Out;"*/ /*"NotaryNumb1Post;"*/ 
   "Расчетный номер;"
   "Текст ошибки;"
SKIP.

mInt  = 0.
mInt0 = 0.
mInt3 = 0.
mInt4 = 0.
mInt5 = 0.
FOR EACH ttLoan WHERE TRUE
   AND ttLoan.cont-date GE DATE("01/01/2016")
   AND ttLoan.cont-date LE DATE("31/12/2016")
   NO-LOCK BY ttLoan.cont-code:
   
   FIND FIRST loan WHERE TRUE
      AND  loan.contract   EQ "Кредит"
      AND  loan.cont-code  BEGINS ttLoan.cont-code
      AND  loan.filial-id  NE "0400"
   NO-LOCK NO-ERROR.
   IF AVAIL(loan) 
   /*AND loan.close-date EQ ?*/ THEN
   DO:
      mInt = mInt + 1. 
      FOR EACH term-obl WHERE TRUE
         AND  term-obl.contract  EQ loan.contract
         AND  term-obl.cont-code EQ loan.cont-code
         AND  term-obl.idnt      EQ 5
         NO-LOCK:
         ASSIGN
            mVidObesp           = ""
            mRegZalog           = ""
            mNotifRefDate       = ""
            mNotifRefNumber     = ""
            mNotaryDate1Post    = ""
            mNotaryNumb1Post    = ""
            mNotaryId1Post      = ""
            mNotaryDate2Out     = ""
            mNotaryNumb2Out     = ""
            mNotaryId2Out       = ""
            mNotaryDate3Change  = ""
            mNotaryNumb3Change  = ""
            mNotaryId3Change    = ""
            mNotaryNumbPost     = ""
            mNotaryErrText      = ""
            mPriznak            = ""
            mOldNumberPost      = IF ttLoan.ninold  EQ ? THEN "" ELSE ttLoan.ninold  
            mNewNumberPost      = IF ttLoan.nin     EQ ? THEN "" ELSE ttLoan.nin
            mOldNumberOut       = IF ttLoan.noutold EQ ? THEN "" ELSE ttLoan.noutold  
            mNewNumberOut       = IF ttLoan.nout    EQ ? THEN "" ELSE ttLoan.nout
            mOldNumberChange    = IF ttLoan.nchold  EQ ? THEN "" ELSE ttLoan.nchold  
            mNewNumberChange    = IF ttLoan.nch     EQ ? THEN "" ELSE ttLoan.nch
            mTermSurr           = term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn).
         
         FOR EACH signs WHERE TRUE
            AND signs.file-name EQ "term-obl"
            AND signs.surrogate EQ mTermSurr
            AND CAN-DO("*",signs.code)
            NO-LOCK:
            IF signs.code EQ "ВидОб"              THEN mVidObesp           = signs.xattr-value.
            IF signs.code EQ "reg-zalog"          THEN mRegZalog           = signs.xattr-value.
            IF signs.code EQ "NotifRegDate"       THEN mNotifRefDate       = signs.code-value.
            IF signs.code EQ "NotifRefNumber"     THEN mNotifRefNumber     = signs.xattr-value.
            IF signs.code EQ "NotaryDate1Post"    THEN mNotaryDate1Post    = signs.xattr-value.
            IF signs.code EQ "NotaryNumb1Post"    THEN mNotaryNumb1Post    = signs.xattr-value.
            IF signs.code EQ "NotaryId1Post"      THEN mNotaryId1Post      = signs.code-value.
            IF signs.code EQ "NotaryDate2Out"     THEN mNotaryDate2Out     = signs.xattr-value.
            IF signs.code EQ "NotaryNumb2Out"     THEN mNotaryNumb2Out     = signs.xattr-value.
            IF signs.code EQ "NotaryId2Out"       THEN mNotaryId2Out       = signs.code-value.
            IF signs.code EQ "NotaryDate3Change"  THEN mNotaryDate3Change  = signs.xattr-value.
            IF signs.code EQ "NotaryNumb3Change"  THEN mNotaryNumb3Change  = signs.xattr-value.
            IF signs.code EQ "NotaryId3Change"    THEN mNotaryId3Change    = signs.code-value.
         END.
         
         mPriznak = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"Priznak","").
         
/*Номера постановки*/
         mNotaryNumbPost = "".
         mNotaryErrText  = "".
         IF {assigned mOldNumberPost} THEN
         DO:
            mNotaryNumbPost = mOldNumberPost.
            IF {assigned mNewNumberPost} THEN
            DO:
               IF mNewNumberPost NE mOldNumberPost THEN
               DO:
                  mNotaryNumbPost = "Error".
                  mNotaryErrText  = "Не равны номера в старой/новой " + mOldNumberPost  + " <> " + mNewNumberPost.
               END.
               ELSE
               DO:
                  mNotaryNumbPost = mNewNumberPost.
               END.
            END.
            ELSE
            DO:
               
            END.
         END.
         ELSE
         DO:
            IF {assigned mNewNumberPost} THEN
            DO:
               mNotaryNumbPost = mNewNumberPost.
            END.
            ELSE
            DO:
               mNotaryNumbPost = "Error".
               mNotaryErrText  = "Нет номера ни в старой ни в новой " + mOldNumberPost  + "/" + mNewNumberPost.
            END.
         END.

         IF mVidObesp EQ "Автомобиль" THEN
         DO:
/*            PUT UNFORMATTED                   */
/*               ttLoan.cont-code ";"           */
/*               ttLoan.cont-date ";"           */
/*               loan.loan-status ";"           */
/*               "залог "STRING(term-obl.nn) ";"*/
/*               mOldNumberPost   ";"           */
/*               mNewNumberPost   ";"           */
/*               mRegZalog        ";"           */
/*               mNotifRefNumber  ";"           */
/*               mNotaryNumb1Post ";"           */
/*               mNotaryNumbPost  ";"           */
/*               mNotaryErrText   ";"           */
/*            SKIP.                             */
         END.
         
/*Номера снятия*/
         mNotaryNumbOut  = "".
         mNotaryErrText  = "".
         IF {assigned mOldNumberOut} THEN
         DO:
            mNotaryNumbPost = mOldNumberOut.
            IF {assigned mNewNumberOut} THEN
            DO:
               IF mNewNumberOut NE mOldNumberOut THEN
               DO:
                  mNotaryNumbOut = "Error".
                  mNotaryErrText  = "Не равны номера в старой/новой " + mOldNumberOut  + " <> " + mNewNumberOut.
               END.
               ELSE
               DO:
                  mNotaryNumbOut = mNewNumberOut.
               END.
            END.
         END.
         ELSE
         DO:
            IF {assigned mNewNumberOut} THEN
            DO:
               mNotaryNumbOut = mNewNumberOut.
            END.
         END.

         IF mVidObesp EQ "Автомобиль" THEN
         DO:
/*            PUT UNFORMATTED                   */
/*               ttLoan.cont-code ";"           */
/*               ttLoan.cont-date ";"           */
/*               loan.loan-status ";"           */
/*               "залог "STRING(term-obl.nn) ";"*/
/*               mOldNumberOut   ";"            */
/*               mNewNumberOut   ";"            */
/*               mRegZalog        ";"           */
/*               mNotifRefNumber  ";"           */
/*               mNotaryNumb2Out ";"            */
/*               mNotaryNumbOut  ";"            */
/*               mNotaryErrText   ";"           */
/*            SKIP.                             */

         END.
         
/*Номера замены*/
         mNotaryNumbChange = "".
         mNotaryErrText    = "".
         IF {assigned mOldNumberChange} THEN
         DO:
            mNotaryNumbChange = mOldNumberChange.
            IF {assigned mNewNumberChange} THEN
            DO:
               IF mNewNumberChange NE mOldNumberChange THEN
               DO:
                  mNotaryNumbChange = "Error".
                  mNotaryErrText  = "Не равны номера в старой/новой " + mOldNumberChange  + " <> " + mNewNumberChange.
               END.
               ELSE
               DO:
                  mNotaryNumbChange = mNewNumberChange.
               END.
            END.
         END.
         ELSE
         DO:
            IF {assigned mNewNumberChange} THEN
            DO:
               mNotaryNumbChange = mNewNumberChange.
            END.
         END.

         IF mVidObesp EQ "Автомобиль" THEN
         DO:
            PUT UNFORMATTED
               ttLoan.cont-code   ";"
               ttLoan.cont-date   ";"
               loan.loan-status   ";"
               mPriznak           ";"
               "залог "STRING(term-obl.nn) ";"
               mOldNumberChange   ";"
               mNewNumberChange   ";"
               mRegZalog          ";"
               mNotifRefNumber    ";"
               mNotaryNumb3Change ";"
               mNotaryNumbChange  ";"
               mNotaryErrText     ";"
            SKIP.
         END.         
      END.
   END.
END.

/*PUT UNFORMATTED mInt SKIP. */
/*PUT UNFORMATTED mInt0 SKIP.*/
/*PUT UNFORMATTED mInt3 SKIP.*/
/*PUT UNFORMATTED mInt4 SKIP.*/
/*PUT UNFORMATTED mInt5 SKIP.*/

{preview.i &filename = "'rep-notary.log'"}

{intrface.del}


FINALLY:
   IF CONNECTED("bank")   THEN DISCONNECT bank.
   IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
END FINALLY.
