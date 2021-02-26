/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: strahbrw.P
      Comment: Получатели страховых премий - функции
   Parameters:
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

PROCEDURE FillSubj:
   mResult = TRUE.
   IF mCustID:SCREEN-VALUE IN FRAME edit NE "" AND mCustCat:SCREEN-VALUE IN FRAME edit NE "Р" AND mCustCat:SCREEN-VALUE IN FRAME edit NE "О" THEN
   DO:
      RUN GetCustShortName IN h_cust (mCustCat:SCREEN-VALUE, INT64 (mCustID:SCREEN-VALUE), 
                                      OUTPUT mShortName).
      RUN GetCustName      IN h_base (mCustCat:SCREEN-VALUE, INT64 (mCustID:SCREEN-VALUE), 
                                      ?, OUTPUT mName1, OUTPUT mName2, INPUT-OUTPUT mInn).
      RUN GetTelefax (mCustCat:SCREEN-VALUE,INT64 (mCustID:SCREEN-VALUE), OUTPUT mTelefax).
      code.{&ShortName} = mShortName.                               
      code.{&ShortName}:SCREEN-VALUE = mShortName. 
      code.{&WhiteName}:SCREEN-VALUE = mName1 + " " + mName2.
      code.{&Telefax}:SCREEN-VALUE = mTelefax.
      IF TRIM(mName1 + mName2) = "" THEN DO:
        mInn = "".
      END.
      IF TRIM(mName1 + mName2 + mShortName) = "" THEN DO:
        MESSAGE "Клиент не найден" VIEW-AS ALERT-BOX.
        mResult = FALSE. /* если клиент не найден, дальше не пускаем */
      END.
      mRaschAcct = 0.
      
      code.{&WhiteINN}:SCREEN-VALUE  = mInn.
      
      IF mCustCat:SCREEN-VALUE = "Ю" THEN DO:
          FIND FIRST cust-corp WHERE cust-corp.cust-id = INT64 (mCustID:SCREEN-VALUE) NO-LOCK NO-ERROR.
          IF AVAIL cust-corp THEN DO:
                IF TRIM(cust-corp.benacct) <> "" THEN DO:
                    mRaschAcct:SCREEN-VALUE IN FRAME edit = TRIM(cust-corp.benacct).
                    IF TRIM(cust-corp.corr-acct) <> "" THEN code.{&CorrAcct}:SCREEN-VALUE IN FRAME edit = TRIM(cust-corp.corr-acct).
                    IF TRIM(cust-corp.bank-code) <> "" THEN DO:
                        code.{&BankBIC}:SCREEN-VALUE IN FRAME edit = cust-corp.bank-code.
                        FIND FIRST banks-code WHERE banks-code.bank-code = cust-corp.bank-code AND banks-code.bank-code-type = cust-corp.bank-code-type NO-LOCK NO-ERROR.
                        IF AVAIL banks-code THEN DO:
                            FIND FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK NO-ERROR.
                            IF AVAIL banks THEN code.{&BankName}:SCREEN-VALUE IN FRAME edit = trim(banks.name).
                        END.                                            
                    END.
                END.
          END.
      END.
      ELSE IF mCustCat:SCREEN-VALUE = "Ч" THEN DO:
          FIND FIRST person WHERE person.person-id = INT64(mCustID:SCREEN-VALUE) NO-LOCK NO-ERROR.
          IF AVAIL person THEN DO:
            code.{&Address}:SCREEN-VALUE IN FRAME edit = TRIM(REPLACE(REPLACE(TRIM(person.address[1]) + TRIM(person.address[2]),",,",","),",,",","),",").
            code.{&BirthDay}:SCREEN-VALUE IN FRAME edit = string(person.birthday, "99.99.9999").
          END.
          FIND LAST cust-ident WHERE cust-ident.cust-cat = "Ч" AND cust-ident.cust-id = INT64(mCustID:SCREEN-VALUE) AND cust-ident.class-code = "p-cust-ident" NO-LOCK NO-ERROR.
          IF AVAIL cust-ident THEN DO:
            code.{&DocType}:SCREEN-VALUE IN FRAME edit = string(cust-ident.cust-code-type).
            code.{&DocCustCode}:SCREEN-VALUE IN FRAME edit = string(cust-ident.cust-code).
            code.{&DocOpenDate}:SCREEN-VALUE IN FRAME edit = string(cust-ident.open-date, "99.99.9999").
            code.{&DocIssue}:SCREEN-VALUE IN FRAME edit = string(cust-ident.issue).
          END.
      END.
      
      IF mRaschAcct = 0 THEN DO:
      FIND FIRST acct WHERE acct.close-date = ? AND
        acct.currency = '' AND
        acct.cust-cat = mCustCat:SCREEN-VALUE AND
        acct.cust-id = INT64 (mCustID:SCREEN-VALUE) AND
        acct.contract = 'Расчет' NO-LOCK NO-ERROR.
        IF NOT AVAIL acct THEN DO:
            FIND FIRST acct WHERE acct.close-date = ? AND
            acct.currency = '' AND
            acct.cust-cat = mCustCat:SCREEN-VALUE AND
            acct.cust-id = INT64 (mCustID:SCREEN-VALUE) AND
            acct.contract = 'Текущ' NO-LOCK NO-ERROR.
        END.
      IF AVAIL acct /* and trim(mRaschAcct:SCREEN-VALUE) = "0" */ THEN DO:
          RUN FillAcct(acct.acct).
      END.  
      END.
      
   END.
   ELSE IF mCustCat:SCREEN-VALUE IN FRAME edit = "О" THEN DO:
       /* Ищем в базе BANK */
       IF NOT CONNECTED("bank") THEN DO:
            CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf") NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Не удалось соединиться с базой BANK" VIEW-AS ALERT-BOX.
                RETURN.
            END.
       END.
       mResult = FALSE.
       RUN strahbrwomsk.p(INT64(mCustID:SCREEN-VALUE),OUTPUT mName1).
       IF mName1 = "NOTFOUND" THEN DO:
           MESSAGE "Клиент не найден" VIEW-AS ALERT-BOX.
           RETURN.
       END.
/*mName1 = fullname|shortname|acct|telefax|inn|address|birthday|vidPassp|sernomPassp|vydanPassp|datePassp */
       IF NUM-ENTRIES ( mName1,'|') > 4 THEN DO:
            IF code.{&ShortName}:SCREEN-VALUE = "" THEN code.{&ShortName}:SCREEN-VALUE = ENTRY(1, mName1,"|").                               
            IF code.{&WhiteName}:SCREEN-VALUE = "" THEN code.{&WhiteName}:SCREEN-VALUE = ENTRY(2, mName1,"|").
            IF mRaschAcct:SCREEN-VALUE IN FRAME edit = "0" OR mRaschAcct:SCREEN-VALUE IN FRAME edit = "" THEN mRaschAcct:SCREEN-VALUE IN FRAME edit = ENTRY(3, mName1,"|").
            IF code.{&Telefax}:SCREEN-VALUE = "" THEN code.{&Telefax}:SCREEN-VALUE = ENTRY(4, mName1,"|").
            IF code.{&WhiteINN}:SCREEN-VALUE = "" THEN code.{&WhiteINN}:SCREEN-VALUE = ENTRY(5, mName1,"|").
            mResult = TRUE.
            /* IF mRaschAcct:SCREEN-VALUE IN FRAME edit <> "" AND mRaschAcct:SCREEN-VALUE IN FRAME edit <> "0" THEN */ DO:
                FIND FIRST banks-code WHERE banks-code.bank-code = "045209783" AND banks-code.bank-code-type = "МФО-9" NO-LOCK NO-ERROR.
                IF AVAIL banks-code THEN DO:
                    FIND FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK NO-ERROR.
                    IF AVAIL banks THEN DO: 
                        IF code.{&BankBIC}:SCREEN-VALUE IN FRAME edit = "" THEN code.{&BankBIC}:SCREEN-VALUE IN FRAME edit = "045209783".
                        IF code.{&BankName}:SCREEN-VALUE IN FRAME edit = "" THEN code.{&BankName}:SCREEN-VALUE IN FRAME edit = trim(banks.name).
                        FIND FIRST banks-corr WHERE banks-corr.bank-id = banks-code.bank-id NO-LOCK NO-ERROR.
                        IF AVAIL banks-corr AND code.{&CorrAcct}:SCREEN-VALUE IN FRAME edit = "" THEN code.{&CorrAcct}:SCREEN-VALUE IN FRAME edit = banks-corr.corr-acct.
                    END.
                END.  
                
            END.
       END.
       IF NUM-ENTRIES ( mName1,'|') > 5 AND code.{&Address}:SCREEN-VALUE IN FRAME edit = "" THEN 
            code.{&Address}:SCREEN-VALUE IN FRAME edit = TRIM(REPLACE(REPLACE(TRIM(ENTRY(6, mName1,"|")),",,",","),",,",","),","). 
       IF NUM-ENTRIES ( mName1,'|') > 6 THEN DO:
            ERROR-STATUS:ERROR = NO.
            mTmpStr = REPLACE(ENTRY(7, mName1,"|"),"/",".").
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
                    code.{&BirthDay}:SCREEN-VALUE IN FRAME edit = string(mTmpDate, "99.99.9999").
                END.
           END.
       END.
            
       IF NUM-ENTRIES ( mName1,'|') > 7 AND code.{&DocType}:SCREEN-VALUE IN FRAME edit = "" THEN 
            code.{&DocType}:SCREEN-VALUE IN FRAME edit = TRIM(REPLACE(ENTRY(8, mName1,"|"),"Паспорт РФ","Паспорт")). 
       IF NUM-ENTRIES ( mName1,'|') > 8 AND code.{&DocCustCode}:SCREEN-VALUE IN FRAME edit = "" THEN 
            code.{&DocCustCode}:SCREEN-VALUE IN FRAME edit = TRIM(REPLACE(TRIM(REPLACE(ENTRY(9, mName1,"|"),"серия","")),"№","")).
       IF NUM-ENTRIES ( mName1,'|') > 9 AND code.{&DocIssue}:SCREEN-VALUE IN FRAME edit = "" THEN 
            code.{&DocIssue}:SCREEN-VALUE IN FRAME edit = TRIM(REPLACE(ENTRY(10, mName1,"|"),"выдан","")).            
       IF NUM-ENTRIES ( mName1,'|') > 10 THEN DO:
            ERROR-STATUS:ERROR = NO.
            mTmpStr = REPLACE(ENTRY(11, mName1,"|"),"/",".").
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
                    code.{&DocOpenDate}:SCREEN-VALUE IN FRAME edit = string(mTmpDate, "99.99.9999").
                END.
           END.
       END.
   END.

   RETURN.
END PROCEDURE.

PROCEDURE FillAcct:
    DEFINE INPUT PARAMETER my_acct AS CHAR.
    IF LENGTH(my_acct) > 20 THEN my_acct = SUBSTRING(my_acct,1,20).
    mRaschAcct:SCREEN-VALUE IN FRAME edit = my_acct.
    code.{&BankName}:SCREEN-VALUE IN FRAME edit = FGetSetting("Банк","",""). 
    code.{&BankBIC}:SCREEN-VALUE IN FRAME edit = FGetSetting("БанкМФО","",""). 
    code.{&CorrAcct}:SCREEN-VALUE IN FRAME edit = FGetSetting("КорСч","",""). 
END PROCEDURE.    
    
PROCEDURE GetTelefax:
    DEFINE INPUT PARAMETER iCustCat AS CHAR.
    DEFINE INPUT PARAMETER iCustID AS INT64.
    DEFINE OUTPUT PARAMETER iTelefax AS CHAR.
    
    DEFINE VARIABLE telef AS CHAR INIT "" NO-UNDO.
    
    iTelefax = "".
    CASE iCustCat :
      WHEN "Ч" THEN DO:
        FIND FIRST person WHERE person.person-id = iCustID NO-LOCK NO-ERROR.
        IF AVAIL person THEN DO: 
            telef = GetXAttrValueEx("person",
                  STRING(person.person-id),
                  "cell-phone",
                 "").
            IF {assigned telef} THEN
                iTelefax = telef.
            ELSE IF  {assigned person.phone[1]}
                AND NUM-ENTRIES(person.phone[1]) GT 1
                AND {assigned ENTRY(2,person.phone[1])} THEN
                iTelefax = ENTRY(2, person.phone[1]).
            ELSE IF {assigned person.phone[1]} THEN
                iTelefax = ENTRY(1, person.phone[1]).
            IF {assigned person.phone[2]} THEN
                iTelefax = iTelefax + ' ' + ENTRY(1, person.phone[2]).
            telef = GetXAttrValueEx("person",
               STRING(person.person-id),
               "fax",
                 "").
            IF telef <> "" THEN iTelefax = iTelefax + ' ' + telef.
        END. /* IF AVAIL person THEN DO:  */
      END. /* WHEN "Ч" */
      WHEN "Ю" THEN DO:
        FIND FIRST cust-corp WHERE cust-corp.cust-id = iCustID NO-LOCK NO-ERROR.
        IF AVAIL cust-corp THEN DO: 
            telef = GetXAttrValueEx("cust-corp",
                 STRING(cust-corp.cust-id),
                 "tel",
                 "").
            iTelefax = telef.
            telef = GetXAttrValueEx("cust-corp",
                 STRING(cust-corp.cust-id),
                 "fax",
                 "").
            IF telef <> "" THEN iTelefax = iTelefax + ' ' + telef.
        END. /* IF AVAIL cust-corp THEN DO: */
      END.
      OTHERWISE .
   END CASE.

END PROCEDURE.    
    
    
    
