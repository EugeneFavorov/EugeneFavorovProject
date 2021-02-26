
DEFINE INPUT  PARAMETER iRecIDAcct AS RECID NO-UNDO. 
DEFINE INPUT  PARAMETER iDbCr AS CHARACTER NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mRSBeg AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mAcctDetails   AS CHARACTER EXTENT 6 NO-UNDO.
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
DEFINE VARIABLE mDbCr AS CHARACTER NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bacct FOR acct.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
DEFINE BUFFER op FOR op.
DEFINE BUFFER tt-op FOR op.
DEFINE BUFFER bop FOR op.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER op-bank FOR op-bank.
DEFINE BUFFER banks FOR banks.
DEFINE BUFFER banks-code FOR banks-code.
DEFINE BUFFER acct-pos FOR acct-pos.

DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER datablock1 FOR datablock.
DEFINE BUFFER dataline FOR dataline.
DEFINE BUFFER dataline1 FOR dataline.


FIND LAST datablock
  WHERE datablock.dataclass-id BEGINS "safetyp"
  AND datablock.branch-id EQ shFilial
  AND datablock.beg-date EQ end-date
  AND datablock.end-date EQ end-date
  NO-LOCK NO-ERROR.

FIND FIRST acct
    WHERE RECID(acct) EQ iRecIDAcct
    NO-LOCK NO-ERROR.
ASSIGN
    mAcct    = TRIM(ENTRY(1, acct.acct, "@"))
    mBegDate = end-date
    mEndDate = end-date
    mDir = "./"
    .
PUT UNFORMATTED
"Выписка по лицевому счету"
SKIP
acct.details
SKIP
mAcct
SKIP
"за период с " + STRING(mBegDate, "99.99.9999") + " по " STRING(mEndDate, "99.99.9999") + " года."  
SKIP
"Т.Д." FORMAT "X(4)"
" "
" Номер" FORMAT "X(6)" 
" "
"   БИК" FORMAT "X(9)"
" "
"   Р/С пл/пол" FORMAT "X(20)"
" "
"   Дебет" FORMAT "X(15)"
" "
"   Кредит" FORMAT "X(15)"
SKIP
.

/*Филиал 0000 */
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
	and op.op-status GE chr(251)
	NO-LOCK
	BY op.op-date:
    DO:
    IF op-entry.acct-db EQ acct.acct THEN
        mDbCr = "D".
    ELSE
        mDbCr = "C".

    FIND FIRST dataline
        WHERE dataline.data-id EQ datablock.data-id
        AND dataline.val[1] EQ DECIMAL(RECID(op-entry))
/*        AND CAN-DO(dataline.txt, mDbCr) */
        NO-LOCK NO-ERROR.
    IF AVAILABLE(dataline) THEN
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
                mAmtDb = IF op-entry.acct-cat NE "d" THEN op-entry.amt-rub ELSE op-entry.qty
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
                mAmtCr = IF op-entry.acct-cat NE "d" THEN op-entry.amt-rub ELSE op-entry.qty.
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
    	       .
               IF op.ben-acct BEGINS "3030" THEN
               DO:
                   mRSBeg = INDEX(op.name-ben, "Р/С ").
                   IF mRSBeg GT 0 
                    AND LENGTH(TRIM(ENTRY(1, SUBSTRING(op.name-ben, mRSBeg + 4), " "))) EQ 20
                    THEN
                       mCorrAcct = TRIM(ENTRY(1, SUBSTRING(op.name-ben, mRSBeg + 4), " ")).
               END. 
               IF mCorrAcct BEGINS "301" THEN
                   mCorrAcct = op.ben-acct.

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
        op.doc-type FORMAT "X(4)"
        " "
        op.doc-num FORMAT "X(6)"
        " "  
        mBankMFO FORMAT "X(9)"
        " "
        mCorrAcct FORMAT "X(20)"
        " "
        mAmtDb FORMAT ">>>>>>>>>>>9.99"
        " "
        mAmtCr FORMAT ">>>>>>>>>>>9.99"
        SKIP
        .
    END.
    END.
END. /* FOR EACH */

IF AVAILABLE(acct-pos) THEN
DO:
    PUT UNFORMATTED
    " " 
    " "
    "Итого" FORMAT "X(41)"
    mAmtDbAll FORMAT ">>>>>>>>>>>9.99"
    " "
    mAmtCrAll FORMAT ">>>>>>>>>>>9.99"
    SKIP
    .
    RUN acct-pos IN h_base (
        acct.acct,
        acct.currency,
        mBegDate,
        mBegDate, "тИЪ"
        ).
    PUT UNFORMATTED
    "Входящее сальдо - "
    ABSOLUTE(sh-in-bal)
    SKIP.
    RUN acct-pos IN h_base (
        acct.acct,
        acct.currency,
        mEndDate,
        mEndDate, "тИЪ"
        ).
    PUT UNFORMATTED
    "Исходящее сальдо - "
    ABSOLUTE(sh-bal)
    SKIP.
END.
/*END. */

{intrface.del}

