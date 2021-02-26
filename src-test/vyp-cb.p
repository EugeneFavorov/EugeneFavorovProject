
DEFINE INPUT  PARAMETER iParams AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}
{tmprecid.def}
DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
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

{getdates.i}

/*    
MESSAGE iParams skip mAcct skip mBegDate skip mEndDate skip mDir 
VIEW-AS ALERT-BOX.    
*/    
/* OS-COMMAND VALUE("mkdir " + mDir). */     
/*
{setdest.i}
*/

FOR EACH tmprecid,
    FIRST acct
    WHERE
        RECID(acct) EQ tmprecid.id
    NO-LOCK:
        LEAVE.
END.
IF NOT AVAILABLE(Acct) THEN
    MESSAGE "Не найден счет." VIEW-AS ALERT-BOX.
DO: 
    ASSIGN
        mAcct    = TRIM(ENTRY(1, acct.acct, "@"))
        mBegDate = beg-date
        mEndDate = end-date
        mDir = "./"
        .
    output to VALUE(mAcct + ".csv") CONVERT TARGET "1251".

/*    IF mBegDate EQ mEndDate THEN
       mBegDate = acct.open-date.
    ELSE*/
    
    IF mBegDate LT acct.open-date THEN
       mBegDate = acct.open-date.
       
/* Заголовок таблицы */
        PUT UNFORMATTED
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
        "Выписка"
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
        "по операциям на счете (специальном банковском счете)"
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
        " N " + mAcct FORMAT "X(24)"
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
        "Дата совершения операции"
        ";"
        "Реквизиты документа, на основании которого была совершена операция по счету (специальному банковскому счету)"
        ";"
        " "
        ";"
        " "
        ";" 
        "Реквизиты банка плательщика/получателя денежных средств"
        ";"
        " "
        ";"
        " "
        ";"
        "Реквизиты плательщика/получателя денежных средств"
        ";"
        " "
        ";"
        " "
        ";" 
        " "
        ";"
        "Сумма операции по счету (специальному банковскому счету)"
        ";"
        " "
        ";"
        "Назначение платежа" 
        SKIP
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
        SKIP
        .


FOR EACH op-entry
	WHERE
    op-entry.op-date GE mBegDate 
    AND op-entry.op-date LE mEndDate  
	AND (op-entry.acct-db EQ acct.acct
    OR op-entry.acct-cr EQ acct.acct)
	NO-LOCK, 
	FIRST op
	WHERE op.op EQ op-entry.op
    AND op.op-date GE mBegDate 
    AND op.op-date LE mEndDate  
	and op.op-status ge chr(251)
	NO-LOCK
	    BREAK by op.op-date:
        IF FIRST-OF(op.op-date) THEN
        DO:
            RUN acct-pos IN h_base (
                acct.acct,
                acct.currency,
                op.op-date,
                op.op-date, "√"
                ).
            PUT UNFORMATTED
            "Входящее сальдо - "
            ABSOLUTE(sh-in-bal)
            SKIP.
        END.
	    
        IF op-entry.acct-db EQ acct.acct THEN
        DO:
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
            where bacct.acct EQ mCorrAcct
            NO-lock no-error.
            ASSIGN 
            mAmtDb = op-entry.amt-rub
            mAmtCr = 0.0
            .
        END.   
        ELSE
        DO:
            mCorrAcct = op-entry.acct-db. 
            IF mCorrAcct EQ ? THEN
            DO:
            FOR EACH bop-entry
                WHERE bop-entry.op EQ op-entry.op
                AND bop-entry.acct-db NE ?
                NO-LOCK:
                    LEAVE.
            END.
            IF AVAILABLE(bop-entry) THEN
                mCorrAcct = bop-entry.acct-db.
            END.     
            FIND FIRST bacct 
            where bacct.acct EQ mCorrAcct
            NO-lock no-error.
            ASSIGN 
            mAmtDb = 0.0
            mAmtCr = op-entry.amt-rub.
        END.
           
	    FIND FIRST op-bank
	       WHERE op-bank.op eq op.op
	       NO-LOCK NO-ERROR.
	    IF AVAILABLE(op-bank) THEN
	    DO:
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
	    END.    
	    ELSE
	    DO:
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
            DO:
                ASSIGN
                mCorrName = mAcctDetails[1]
                mCorrINN = mAcctDetails[3]
                mCorrAcct = bacct.acct
                .
            END.
            ELSE    
                ASSIGN
                mCorrName = FGetSetting ("Банк", ?, " ")
                mCorrINN = FGetSetting ("ИНН", ?, " ")
                mCorrAcct = bacct.acct
                .
        END.    
/*	        FOR EACH banks-code
	        WHERE banks-code.bank-code EQ op-bank.op eq mBankMFO
	        AND banks-code.bank-code-type EQ "МФО-9"
	        NO-LOCK,
	            FIRST banks
	                WHERE banks.bank-id EQ banks-code.bank-id
	                NO-LOCK:
	                LEAVE.
	        END.            
            IF AVAILABLE(banks-code) THEN
               ASSIGN
               mBankName = banks.name
               mBankAcct = banks.corr-acct. 
 */
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
        IF LAST-OF(op.op-date) THEN
        DO:
            RUN acct-pos IN h_base (
                acct.acct,
                acct.currency,
                op.op-date,
                op.op-date, "√"
                ).
            PUT UNFORMATTED
            "Исходящее сальдо - "
            ABSOLUTE(sh-bal)
            SKIP.
        END.
END.
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
        ";'"
        mAmtDbAll
        ";'"
        mAmtCrAll
        ";"
        " " 
        SKIP
        .


OUTPUT CLOSE.    

MESSAGE "Сформирован файл выписки" SKIP
mAcct + ".csv"
VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + mAcct + ".csv" + ";class=bq").

{intrface.del}

