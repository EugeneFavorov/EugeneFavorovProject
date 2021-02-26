
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
DEFINE VARIABLE isCash AS LOGICAL NO-UNDO.
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.
DEFINE VARIABLE mTmp AS CHARACTER NO-UNDO.
DEFINE VARIABLE vName AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE oINN AS CHARACTER NO-UNDO.

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


/* ��६���� ��� ��।������ ���ᮢ��� ���㬥�� */
/* {op-cash.def} */
/* ���樠������ �ࠢ�筨�� �.���-�� */
/* {elhran.def} */

FIND FIRST acct
    WHERE RECID(acct) EQ iRecIDAcct
    NO-LOCK NO-ERROR.

vName[1] = acct.details.

IF vName[1] EQ ? THEN
DO:
    RUN GetCustName IN h_base(acct.cust-cat,
                            acct.cust-id,
                            ?,
                            OUTPUT vName[1],
                            OUTPUT vName[2],
                            INPUT-OUTPUT oINN).
    vName[1] = vName[1] + " " + vName[2].
END.

ASSIGN
    mAcct    = TRIM(ENTRY(1, acct.acct, "@"))
    mBegDate = end-date
    mEndDate = end-date
    mDir = "./"
    .
FIND FIRST acct-pos
    WHERE acct-pos.filial-id EQ acct.filial-id
    AND acct-pos.acct EQ acct.acct
    AND acct-pos.since EQ end-date
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE(acct-pos) THEN
/*    PUT UNFORMATTED
    "�� ���� " 
    mAcct FORMAT "X(20)" 
    " � " 
    STRING(end-date, "99.99.9999")
    " �� " 
    STRING(end-date, "99.99.9999")
    " �஢���� �� �뫮."
    SKIP.
*/    
    mTmp = "".
ELSE
DO: 
        PUT UNFORMATTED
        "�믨᪠ �� ��楢��� ����"
        SKIP
        vName[1]
        SKIP
        mAcct
        SKIP
        "�� ��ਮ� � " + STRING(mBegDate, "99.99.9999") + " �� " STRING(mEndDate, "99.99.9999") + " ����."  
        SKIP
/*        "N �/�" 
        " "
        "���" FORMAT "X(10)"
        " " */
        "�.�." FORMAT "X(4)"
        " "
        " �����" FORMAT "X(6)" 
        " "
/*        "   �/� ����� ��/���" FORMAT "X(20)"
        " "
        "������������ ����� ���⥫�騪�/�����⥫�" FORMAT "X(41)" 
        " " */
        "   ���" FORMAT "X(9)"
        " "
/*        "������������ ���⥫�騪�/�����⥫�" FORMAT "X(35)" 
        " " */
        "   �/� ��/���" FORMAT "X(20)"
        " "
        "   �����" FORMAT "X(15)"
        " "
        "   �।��" FORMAT "X(15)"
/*        " "
        "�����祭�� ���⥦�" FORMAT "X(50)"  */
        SKIP
        .

/*������ 0000 */
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
/*    
    IF NOT (op.op-date EQ DATE("30.04.2015") AND op.op-kind EQ "op-pos") 
    THEN 
*/    
    DO:
      /* ��।������ ���ᮢ��� ���㬥�� */
/*      {op-cash.i} */
/* ��।������ ���஭���� ���㬥�� */
/*        FIND FIRST tt-op
            WHERE RECID(tt-op) EQ RECID(op)
        NO-LOCK NO-ERROR.
       {elhran.i}
    IF isElH THEN */
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
                   mRSBeg = INDEX(op.name-ben, "�/� ").
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
                mBankName = FGetSetting ("����", ?, " ")
                mBankMFO = FGetSetting ("�������", ?, "0")
                mBankAcct = FGetSetting ("�����", ?, "0").
            RUN GetCustName IN h_base (bacct.cust-cat,
                                       bacct.cust-id,
                                       ?,
                                       OUTPUT mAcctDetails[1],
                                       OUTPUT mAcctDetails[2],
                                       INPUT-OUTPUT mAcctDetails[3]).
            mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
            IF bacct.cust-cat NE "�" THEN
            DO: /* 7 */
                ASSIGN
                mCorrName = mAcctDetails[1]
                mCorrINN = mAcctDetails[3]
/*                mCorrAcct = bacct.acct */
                .
            END. /* 7 */
            ELSE    
                ASSIGN
                mCorrName = FGetSetting ("����", ?, " ")
                mCorrINN = FGetSetting ("���", ?, " ")
/*                mCorrAcct = bacct.acct */
                .
        END. /* 6 */
        ASSIGN          
            mI = mI + 1
            mAmtDbAll = mAmtDbAll + mAmtDb
            mAmtCrAll = mAmtCrAll + mAmtCr
            .
           
        PUT UNFORMATTED
/*        mI 
        " " 
        op.op-date FORMAT "99.99.9999"
        " " */
        op.doc-type FORMAT "X(4)"
        " "
        op.doc-num FORMAT "X(6)"
        " "  
/*        mBankAcct FORMAT "X(20)"
        " "
        mBankName FORMAT "X(41)" 
        " " */
        mBankMFO FORMAT "X(9)"
        " "
/*        REPLACE(REPLACE(mCorrName, ";", " "), ";", " ")  FORMAT "X(35)" 
        " " */
        mCorrAcct FORMAT "X(20)"
        " "
        mAmtDb FORMAT ">>>>>>>>>>>9.99"
        " "
        mAmtCr FORMAT ">>>>>>>>>>>9.99"
/*        " "
        REPLACE(REPLACE(replace(replace(op.details, chr(10), ""), chr(10), ""), ";", " "), ";", " ") */
        SKIP
        .
    END.
    END.
END. /* FOR EACH */
END.
IF AVAILABLE(acct-pos) THEN
DO:
    PUT UNFORMATTED
    " " 
    " "
    "�⮣�" FORMAT "X(41)"
    mAmtDbAll FORMAT ">>>>>>>>>>>9.99"
    " "
    mAmtCrAll FORMAT ">>>>>>>>>>>9.99"
    SKIP
    .
    RUN acct-pos IN h_base (
        acct.acct,
        acct.currency,
        mBegDate,
        mBegDate, "√"
        ).
    PUT UNFORMATTED
    "�室�饥 ᠫ줮 - "
    ABSOLUTE(sh-in-bal)
    SKIP.
    RUN acct-pos IN h_base (
        acct.acct,
        acct.currency,
        mEndDate,
        mEndDate, "√"
        ).
    PUT UNFORMATTED
    "��室�饥 ᠫ줮 - "
    ABSOLUTE(sh-bal)
    SKIP.
END.

{intrface.del}

