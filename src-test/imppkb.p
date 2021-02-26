/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: Создаем документы перечисления по реестру "Первое Коллекторское Бюро"
   Parameters:
         Uses:
      Used by:
      Created: kam
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

/* DEF VAR fname AS CHAR VIEW-AS FILL-IN NO-UNDO. */
DEF VAR ffname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.

DEF VAR sCode AS CHAR NO-UNDO.
DEF VAR sName AS CHAR NO-UNDO.
DEF VAR sName1 AS CHAR NO-UNDO.
DEF VAR sDate AS DATE NO-UNDO.
DEF VAR sOpDate AS DATE FORMAT "99/99/9999" LABEL "Введите дату реестра" INIT TODAY NO-UNDO.
DEF VAR sSum AS DECIMAL NO-UNDO.
DEF VAR sNewSum AS DECIMAL NO-UNDO.
DEF VAR iInt AS INT64 NO-UNDO.
DEF VAR ost40817 AS DECIMAL NO-UNDO.
DEF VAR AcctCesRS AS CHAR NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.

DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttOp
        FIELD in-cont-code LIKE loan.cont-code
        FIELD in-doc-ref LIKE loan.cont-code
        FIELD in-open-date LIKE loan.open-date
        FIELD in-op-date AS DATE
        FIELD in-acctdb LIKE loan-acct.acct
        FIELD in-curr LIKE loan-acct.currency
        FIELD in-filial LIKE loan.filial-id
        FIELD in-sSum AS DECIMAL
        FIELD in-sNewSum AS DECIMAL
        FIELD in-nameCl AS CHAR
        FIELD mess AS CHAR
.

/*
PAUSE 0.
UPDATE SKIP(1) sOpDate SKIP(1)
  WITH FRAME fMain OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE "Введите дату реестра".
HIDE FRAME fMain.
*/
{getdate.i}

sOpDate = end-date.
        
ffname = '/data/home2/bis/quit41d/imp-exp/pkb/in/' + 'plus_' + STRING(DAY(sOpDate),"99") + STRING(MONTH(sOpDate),"99") + STRING(YEAR(sOpDate),"9999") + '.csv'.

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
        ost40817 = 0.
        CREATE ttOp.
                sCode = TRIM(ENTRY(3,fstr,';')).
                sName = ENTRY(2,fstr,';').
                sDate = sOpDate. /* ConvertDate(ENTRY(4,fstr,';')). */
                sSum = ConvertDecimal(ENTRY(4,fstr,';')).
                IF sDate <> ? AND sSum > 0 THEN DO:
                        sNewSum = sSum.
                        FIND FIRST loan WHERE loan.doc-ref   = sCode 
                                          AND loan.filial-id = shFilial
                        NO-LOCK NO-ERROR.
                        IF AVAIL loan THEN DO:
                                FIND FIRST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч'
                                NO-LOCK NO-ERROR.
                                IF AVAIL loan-acct THEN DO:
                                        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, sOpDate, sOpDate, CHR(251)).
                                        IF loan-acct.currency = '' THEN
                                                ost40817 = abs(sh-bal).
                                        ELSE ost40817 = abs(sh-val).
                                        IF sNewSum > ost40817 THEN sNewSum = ost40817.
                                        IF sNewSum > 0 THEN DO:
                                                RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT sName1).
                                              /*  AcctCesRS = GetXattrValueEx("loan",
                          loan.contract + "," + loan.cont-code, 
                          "AcctCesRS",
                          ""). */
                                                 /* CREATE ttOp. */
                                                ASSIGN
                                                        ttOp.in-cont-code = loan.cont-code
                                                        ttOp.in-doc-ref = loan.doc-ref
                                                        ttOp.in-open-date = loan.open-date
                                                        ttOp.in-acctdb = loan-acct.acct
                                                        ttOp.in-curr = loan-acct.currency
                                                        ttOp.in-filial = loan.filial-id
                                                        ttOp.in-sSum = sSum
                                                        ttOp.in-sNewSum = sNewSum
                                                        ttOp.in-namecl = sName1
                                                        ttOp.in-op-date = sOpDate
                                                .
                                        END. /* IF sSum > 0 THEN DO: */
                                        ELSE DO:
                                                ttOp.mess = ttOp.mess + 'Договор ' + sCode + ', документ не создан, сумма на счете 40817 = ' + STRING(sNewSum).
                                        END.
                                END.
                        END.
                        ELSE DO:
                                 ttOp.mess = ttOp.mess + 'Договор ' + sCode + ' не найден'. 
                        END.
                END.
                ELSE DO:
                        IF sDate = ? THEN 
                                ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' неверный формат даты платежа'.
                        IF sSum = 0 THEN DO:
                                ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' сумма в реестре = 0'.
                        END.
                        IF sSum < 0 THEN DO:
                                ttOp.mess = ttOp.mess + ' Договор ' + sCode + ' неверный формат суммы платежа'.
                        END.
                END.
                RELEASE ttOp.
    fstr = ''.
 END. /* IMPORT UNFORMATTED fstr. */

FOR EACH ttOp WHERE ttOp.in-sNewSum > 0:
    
    FOR EACH  BlockObject WHERE
          BlockObject.class-code   EQ 'BlockAcct'
      AND BlockObject.FILE-NAME    EQ 'acct'
      AND BlockObject.block-type   EQ 'БлокВозвр'
      AND BlockObject.surrogate    EQ ttOp.in-acctdb + ','
      AND BlockObject.end-datetime EQ ?
        :
        ASSIGN
            BlockObject.end-datetime = BlockObject.beg-datetime. 
            BlockObject.val[3] =  0.
    END.

    
    
                                                {empty tOpKindParams}     /* очистить таблицу параметров */
                                                mRes = TDAddParam("in-cont-code",ttOp.in-cont-code).
                                                mRes = TDAddParam("in-doc-ref",ttOp.in-doc-ref).
                                                mRes = TDAddParam("in-open-date",STRING(ttOp.in-open-date,"99.99.9999")).
                                                mRes = TDAddParam("in-acctdb",ttOp.in-acctdb).
                                                mRes = TDAddParam("in-curr",ttOp.in-curr).
                                                mRes = TDAddParam("in-filial",ttOp.in-filial).
                                                mRes = TDAddParam("in-sSum",STRING(ttOp.in-sNewSum)).
                                                mRes = TDAddParam("in-namecl",ttOp.in-nameCl).                                       
                                                mRes = TDAddParam("in-op-date",STRING(ttOp.in-op-date,"99.99.9999")).
                                                RUN ex-trans.p(INPUT 'perpkb', 
                                                        INPUT TODAY, 
                                                        INPUT TABLE tOpKindParams, 
                                                        OUTPUT mOK, 
                                                        OUTPUT mMessage).
                                                IF NOT mOK THEN
                                                DO:
                                                        ttOp.mess = ttOp.mess + ' Договор ' + ttOp.in-cont-code + ', документ не создан, ' + mMessage.
                                                END.
                                                ELSE DO:
                                                        ttOp.mess = ttOp.mess +  ' Договор ' + ttOp.in-cont-code + ', документ создан, сумма в реестре ' + STRING(ttOp.in-sSum) + ', сумма документа ' + STRING(ttOp.in-sNewSum).
                                                END.
END.

{intrface.del}

{setdest.i}
        FOR EACH ttOp NO-LOCK:
                PUT UNFORMATTED ttOp.mess skip.
        END.
{preview.i}

RETURN.

PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END PROCEDURE.

 
 
