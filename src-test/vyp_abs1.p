
DEFINE INPUT  PARAMETER iRecIDAcct AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}
/*
{tmprecid.def}
*/

DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mRSBeg AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mAcctDetails   AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE mCust   AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mBegDate AS DATE NO-UNDO.
DEFINE VARIABLE mEndDate AS DATE NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankMFO AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrINN AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrKPP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDb AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmtCr AS DECIMAL NO-UNDO.
DEFINE VARIABLE mKPP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDbAll AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mAmtCrAll AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mDir AS CHARACTER NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bacct FOR acct.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
DEFINE BUFFER op FOR op.
DEFINE BUFFER bop FOR op.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER banks FOR banks.
DEFINE BUFFER banks-code FOR banks-code.

FIND FIRST acct
    WHERE RECID(acct) EQ iRecIDAcct
    NO-LOCK NO-ERROR.
DO: 
    ASSIGN
        mAcct    = TRIM(ENTRY(1, acct.acct, "@"))
        mBegDate = beg-date
        mEndDate = end-date
        mDir = "./"
        .
    IF mBegDate EQ mEndDate THEN
       mBegDate = acct.open-date.
    ELSE
    IF mBegDate LT acct.open-date THEN
       mBegDate = acct.open-date.
       
        PUT UNFORMATTED
        " N " + acct.acct
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";" 
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";" 
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " " 
        SKIP
        "за период с " + STRING(mBegDate, "99.99.9999") + " по " STRING(mEndDate, "99.99.9999") + " года."  
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";" 
        ""
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " "
        ";" 
        " "
        ";"
        " "
        ";"
        " "
        ";"
        " " 
        SKIP
        "N п/п" 
        ";"
        "Дата"
        ";"
        "Т.Д."
        ";"
        "Номер"
        ";"
/*        " "
        ";"  */
        "К/С банка плательщика/получателя"
        ";"
        "Наименование банка плательщика/получателя"
        ";"
        "БИК банка плательщика/получателя"
        ";"
        "Наименование плательщика/получателя"
        ";"
/*        " "
        ";"
        " "
        ";" 
*/        
        "Р/С плательщика/получателя"
        ";"
        "Дебет"
        ";"
        "Кредит"
        ";"
        "Назначение платежа" 
/*        SKIP
        " " 
        ";"
        " "
        ";"
        "Вид (шифр)"
        ";"
        "Номер"
        ";"
        "Дата"
        ";" 
        "номер корреспондентского счета"
        ";"
        "наименование"
        ";"
        "БИК"
        ";"
        "наименование/ФИО"
        ";"
        "ИНН/КИО"
        ";"
        "КПП"
        ";" 
        "Р/счет (специальный банковский счет)"
        ";"
        "по дебету"
        ";"
        "по кредиту"
        ";"
        " " 
*/        
        SKIP
        .

/*Филиал 0400 */
/*
FOR EACH op-entry
    WHERE
    op-entry.op-date GE mBegDate 
    AND op-entry.op-date LE mEndDate  
    AND (op-entry.acct-db BEGINS ENTRY(1, acct.acct, "@")
    OR op-entry.acct-cr BEGINS ENTRY(1, acct.acct, "@"))
    NO-LOCK, 
FIRST op
    WHERE op.op EQ op-entry.op
    AND op.op-date GE mBegDate 
    AND op.op-date LE mEndDate  
    and op.op-status ge chr(251)
    NO-LOCK
    BY op.op-date:
    IF NOT (op.op-date EQ DATE("30.04.2015") AND op.op-kind EQ "op-pos") 
    THEN
    DO:
        IF op-entry.acct-db BEGINS macct THEN
        DO: /* 1 */
            mCorrAcct = op-entry.acct-cr. 
            IF mCorrAcct EQ ? THEN
            DO:
                FOR EACH bop-entry
                    WHERE bop-entry.op EQ op-entry.op
                    AND bop-entry.acct-cr NE ?
                    NO-LOCK:
                        LEAVE.
                END.
                IF AVAILABLE(bop-entry) THEN
                    mCorrAcct = bop-entry.acct-cr. 
            END.
            FIND FIRST bacct 
                WHERE bacct.acct EQ mCorrAcct
            NO-LOCK NO-ERROR.
            ASSIGN 
                mAmtDb = op-entry.amt-rub
                mAmtCr = 0.0
            .
        END. /* 1 */   
        ELSE
        DO: /* 2 */
            mCorrAcct = op-entry.acct-db. 
            IF mCorrAcct EQ ? THEN
            DO: /* 3 */
                FOR EACH bop-entry 
                    WHERE bop-entry.op EQ op-entry.op
                    AND bop-entry.acct-db NE ?
                    NO-LOCK: /* 4 */
                        LEAVE.
                END. /* 4 */
                IF AVAILABLE(bop-entry) THEN
                mCorrAcct = bop-entry.acct-db.
            END. /* 3 */
            FIND FIRST bacct 
                WHERE bacct.acct EQ mCorrAcct
                NO-lock NO-ERROR.
            ASSIGN 
                mAmtDb = 0.0
                mAmtCr = op-entry.amt-rub.
        END. /* 2 */
           
        FIND FIRST op-bank
           WHERE op-bank.op eq op.op
        NO-LOCK NO-ERROR.
        IF AVAILABLE(op-bank) THEN
        DO: /* 5 */
           mKPP = IF CAN-DO("30*", op-entry.acct-cr) THEN "Kpp-rec" ELSE "Kpp-send".   
           ASSIGN
               mBankMFO = op-bank.bank-code
               mBankName = op-bank.bank-name
               mBankAcct = op-bank.corr-acct
               mCorrName = replace(replace(REPLACE(op.name-ben, ";", ":"), ";", ":"), chr(10), "")
               mCorrINN = op.inn
               mCorrKPP = GetXAttrValueEx("op",
                                           STRING(op.op),
                                           mKPP,
                                           " "
               )
               mCorrAcct = op.ben-acct
               .
        END. /* 5 */
        ELSE
        DO: /* 6 */
            ASSIGN
                mBankName = FGetSetting ("Банк", ?, " ")
                mBankMFO = FGetSetting ("БанкМФО", ?, "0")
                mBankAcct = FGetSetting ("КорСч", ?, "0").
            RUN GetCustName IN h_base (bacct.cust-cat,
                                       bacct.cust-id,
                                       ?,
                                       OUTPUT mAcctDetails[1],
                                       OUTPUT mAcctDetails[2],
                                       INPUT-OUTPUT mAcctDetails[3]).
            mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
            IF bacct.cust-cat NE "В" THEN
            DO: /* 7 */
                ASSIGN
                mCorrName = mAcctDetails[1]
                mCorrINN = mAcctDetails[3]
                .
            END. /* 7 */
            ELSE    
                ASSIGN
                mCorrName = FGetSetting ("Банк", ?, " ")
                mCorrINN = FGetSetting ("ИНН", ?, " ")
                .
        END. /* 6 */
        ASSIGN          
            mI = mI + 1
            mAmtDbAll = mAmtDbAll + mAmtDb
            mAmtCrAll = mAmtCrAll + mAmtCr
            .
           
        PUT UNFORMATTED
        mI 
        ";"
        op.op-date FORMAT "99.99.9999"
        ";'"
        op.doc-type
        ";'"
        op.doc-num
        ";"
        op.doc-date FORMAT "99.99.9999"
        ";'" 
        mBankAcct FORMAT "X(20)"
        ";"
        mBankName
        ";'"
        mBankMFO
        ";"
        mCorrName
        ";'"
        mCorrINN
        ";'"
        mCorrKPP
        ";'" 
        mCorrAcct FORMAT "X(20)"
        ";"
        mAmtDb
        ";"
        mAmtCr
        ";"
        replace(replace(op.details, chr(10), ""), chr(10), "") 
        SKIP
        .
    END.
END. /* FOR EACH */
*/
/*Филиал 0000 */
FOR EACH op-entry
	WHERE
    op-entry.op-date GE mBegDate 
    AND op-entry.op-date LE mEndDate  
/*	AND (op-entry.acct-db BEGINS ENTRY(1, acct.acct, "@")
    OR op-entry.acct-cr BEGINS ENTRY(1, acct.acct, "@")) */
    AND (op-entry.acct-db BEGINS mAcct
    OR op-entry.acct-cr BEGINS mAcct)
	NO-LOCK,
   FIRST op
	WHERE op.op EQ op-entry.op
    AND op.op-date GE mBegDate 
    AND op.op-date LE mEndDate  
	and op.op-status GE chr(251)
	NO-LOCK
	BY op.op-date QUERY-TUNING(NO-INDEX-HINT):
    IF NOT (op.op-date EQ DATE("30.04.2015") AND op.op-kind EQ "op-pos") 
    THEN 
    DO:
        IF op-entry.acct-db BEGINS mAcct THEN
        DO: /* 1 */
            mCorrAcct = op-entry.acct-cr. 
            IF mCorrAcct EQ ? THEN
            DO:
                FOR EACH bop-entry
                    WHERE bop-entry.op EQ op-entry.op
                    AND bop-entry.acct-cr NE ?
                	NO-LOCK:
                	    LEAVE.
                END.
                IF AVAILABLE(bop-entry) THEN
                    mCorrAcct = bop-entry.acct-cr. 
            END.
            FIND FIRST bacct 
                WHERE bacct.acct EQ mCorrAcct
            NO-LOCK NO-ERROR.
            ASSIGN 
                mAmtDb = op-entry.amt-rub
                mAmtCr = 0.0
            .
        END. /* 1 */   
        ELSE
        DO: /* 2 */
            mCorrAcct = op-entry.acct-db. 
            IF mCorrAcct EQ ? THEN
            DO: /* 3 */
                FOR EACH bop-entry 
                    WHERE bop-entry.op EQ op-entry.op
                    AND bop-entry.acct-db NE ?
                    NO-LOCK: /* 4 */
                        LEAVE.
                END. /* 4 */
                IF AVAILABLE(bop-entry) THEN
                mCorrAcct = bop-entry.acct-db.
            END. /* 3 */
            FIND FIRST bacct 
                WHERE bacct.acct EQ mCorrAcct
                NO-lock NO-ERROR.
            ASSIGN 
                mAmtDb = 0.0
                mAmtCr = op-entry.amt-rub.
        END. /* 2 */
           
	   FIND FIRST op-bank
	      WHERE op-bank.op eq op.op
	   NO-LOCK NO-ERROR.
	   IF AVAILABLE(op-bank) THEN
      DO: /* 5 */
	      mKPP = IF CAN-DO("30*", op-entry.acct-cr) THEN "Kpp-rec" ELSE "Kpp-send".   
	      ASSIGN
    	       mBankMFO = op-bank.bank-code
    	       mBankName = op-bank.bank-name  /**/
    	       mBankAcct = op-bank.corr-acct
    	       mCorrName = replace(replace(REPLACE(op.name-ben, ";", ":"), ";", ":"), chr(10), "")
    	       mCorrINN = op.inn
    	       mCorrKPP = GetXAttrValueEx("op",
    	                                   STRING(op.op),
    	                                   mKPP,
    	                                   " "
    	       )
    	   .
         IF mBankName EQ "" THEN
         FOR EACH banks-code WHERE
                banks-code.bank-code      EQ op-bank.bank-code
            AND banks-code.bank-code-type EQ op-bank.bank-code-type
            NO-LOCK,
            FIRST banks WHERE
               banks.bank-id EQ banks-code.bank-id
            NO-LOCK:
            mBankName = banks.name.
         END.   
         IF op.ben-acct BEGINS "3030" THEN
         DO:
            mRSBeg = INDEX(op.name-ben, "Р/С ").
            IF mRSBeg GT 0 THEN
                mCorrAcct = TRIM(ENTRY(1, SUBSTRING(op.name-ben, mRSBeg + 4), " ")).
         END. 
         IF mCorrAcct BEGINS "301" THEN
            mCorrAcct = op.ben-acct.
	    END. /* 5 */
	    ELSE
	    DO: /* 6 */
            ASSIGN
                mBankName = FGetSetting ("Банк", ?, " ")
                mBankMFO  = FGetSetting ("БанкМФО", ?, "0")
                mBankAcct = FGetSetting ("КорСч", ?, "0").
            RUN GetCustName IN h_base (bacct.cust-cat,
                                       bacct.cust-id,
                                       ?,
                                       OUTPUT mAcctDetails[1],
                                       OUTPUT mAcctDetails[2],
                                       INPUT-OUTPUT mAcctDetails[3]).
            mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
            IF bacct.cust-cat NE "В" THEN
            DO: /* 7 */
                ASSIGN
                mCorrName = mAcctDetails[1]
                mCorrINN = mAcctDetails[3]
/*                mCorrAcct = bacct.acct */
                .
            END. /* 7 */
            ELSE    
                ASSIGN
                mCorrName = FGetSetting ("Банк", ?, " ")
                mCorrINN = FGetSetting ("ИНН", ?, " ")
/*                mCorrAcct = bacct.acct */
                .
        END. /* 6 */
        ASSIGN          
            mI = mI + 1
            mAmtDbAll = mAmtDbAll + mAmtDb
            mAmtCrAll = mAmtCrAll + mAmtCr
            .
           
        PUT UNFORMATTED
        mI 
        ";"
        op.op-date FORMAT "99.99.9999"
        ";'"
        op.doc-type
        ";'"
        op.doc-num
/*        ";"
*        op.doc-date FORMAT "99.99.9999" */
        ";'"  
        mBankAcct FORMAT "X(20)"
        ";"
        mBankName
        ";'"
        mBankMFO
        ";"
        REPLACE(REPLACE(mCorrName, ";", " "), ";", " ")
        ";'"
/*        mCorrINN
        ";'"
        mCorrKPP
        ";'"  */
        mCorrAcct FORMAT "X(20)"
        ";"
        mAmtDb
        ";"
        mAmtCr
        ";"
        REPLACE(REPLACE(replace(replace(op.details, chr(10), ""), chr(10), ""), ";", " "), ";", " ")
        SKIP
        .
    END.
END. /* FOR EACH */
END.

        PUT UNFORMATTED
        " " 
        ";"
        "Итого"
        ";"
        " " 
        ";"
        " " 
        ";"
/*        " " 
        ";"  */
        " " 
        ";"
        " " 
        ";"
        " " 
        ";"
        " " 
/*        ";"
        " " 
        ";"
        " " */
        ";" 
        " " 
        ";'"
        mAmtDbAll
        ";'"
        mAmtCrAll
        ";"
        " " 
        SKIP
        .

RUN acct-pos IN h_base (
    acct.acct,
    acct.currency,
    mBegDate,
    mBegDate, "√"
    ).
PUT UNFORMATTED
"Входящее сальдо - "
ABSOLUTE(sh-in-bal)
SKIP.
RUN acct-pos IN h_base (
    acct.acct,
    acct.currency,
    mEndDate,
    mEndDate, "√"
    ).
PUT UNFORMATTED
"Исходящее сальдо - "
ABSOLUTE(sh-bal)
SKIP.


{intrface.del}

