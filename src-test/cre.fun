{t-otch.i NEW}
{svarloan.def NEW}
{sh-defs.i}
{intrface.get loan}
{intrface.get i254}
{loan_par.def &new = new}
{omsk.pro}

{globals.i}
{intrface.get xclass}
{intrface.get lv}
{intrface.get refer}
{intrface.get date}
{intrface.get db2l}
{intrface.get pogcr}
{lshpr.pro}
{debug.equ}
{profile.def}
{anreps.i}

{cust-adr.obj &def-vars = YES}

&GLOB NBKICODE     "NU01BB000001"
&GLOB FLPREFIX     "ФЛ"
&GLOB ULPREFIX     "ЮЛ"

DEFINE TEMP-TABLE ttLoans
    FIELD r-loan        AS ROWID
    FIELD need-export   AS CHAR
    FIELD date-prev     AS DATE
    FIELD date-rep      AS DATE
    INDEX ttiloan IS UNIQUE r-loan
    .
DEFINE TEMP-TABLE ttGuarant
    FIELD r-loan        AS ROWID
    FIELD r-term        AS ROWID
    FIELD cont-code     LIKE loan.cont-code
    FIELD cust-id       LIKE person.person-id
    FIELD cust-cat      AS CHAR
    FIELD loanUpd       AS DATE
    FIELD recStatus     AS INT
    .

/*=========================================================================== */
/*===== Дней просрочки %% (ДниПросрПроц) ==================================== */
/*=========================================================================== */
FUNCTION LN_GetPrsProcDays48 RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    DEF VAR vDb     AS DEC  NO-UNDO.
    DEF VAR vCr     AS DEC  NO-UNDO.
    DEF VAR vParam  AS DEC  NO-UNDO.
    DEF VAR vParam1 AS DEC  NO-UNDO.

    DEF BUFFER loan-int FOR loan-int.

    RELEASE loan-int.

    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.mdate    <= iDate
         AND (CAN-DO("10,16,48",STRING(loan-int.id-d)) OR
              CAN-DO("10,16,48",STRING(loan-int.id-k)))
    NO-LOCK BY loan-int.mdate DESCENDING:

       IF LN_GetParams (iContract,
                        iContCode,
                        "10,16,48,210,248",
                        loan-int.mdate - 1) = 0
       THEN RETURN (iDate - loan-int.mdate).
    END.

    RETURN 0.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Определяет дату возникновения неоплаченной просрочки ОД/процентов          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetFirstDueDate_:
   DEF INPUT PARAM  iContract  AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM  iContCode  AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM  iDate      AS DATE NO-UNDO. /* Дата выгрузки */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO. /* Дата выноса на просрочку */
   DEF VAR vPeriod    AS INT64   NO-UNDO.
   DEF BUFFER bloan FOR loan.

    vPeriod = MAX(
        LN_GetPrsDolgDays ( iContract, iContCode, iDate),
        LN_GetPrsProcDays48 ( iContract, iContCode, iDate)).

    FOR EACH bloan
        WHERE bloan.contract EQ iContract
        AND bloan.cont-code BEGINS iContCode + " " NO-LOCK:

    /* нельзя вызывать, не чухает что просрочка уже нулевая */
    vPeriod = MAX( vPeriod,
        MAX(
            LN_GetPrsDolgDays ( iContract, iContCode, iDate),
            LN_GetPrsProcDays48 ( iContract, iContCode, iDate))
        ).
    END.
    IF vPeriod EQ 0
        THEN oDate = ?.
    ELSE oDate = iDate - vPeriod.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Возвращает сумму по параметру или операции с учетом течений                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummUnv_:
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
   DEF OUTPUT PARAM oAmtRes   AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtCr    AS DECIMAL   NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vAmtCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtDbCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtCrCur AS DECIMAL NO-UNDO.

   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iOper,
                        iDate,
                        OUTPUT oAmtRes,
                        OUTPUT oAmtDb,
                        OUTPUT oAmtCr).
   /* Ищем договор */
   FIND FIRST bLoan WHERE
              bLoan.contract  EQ iContract
          AND bLoan.cont-code EQ iContCode
              NO-LOCK NO-ERROR.
   ASSIGN
      vAmtCur   = 0.00
      vAmtDbCur = 0.00
      vAmtCrCur = 0.00
   .

/*----------- Если это охватывающий договор-течение, то ищем все его транши --*/
   IF AVAIL bloan                  AND
      bloan.cont-type EQ "Течение" THEN
      FOR EACH bloan
         WHERE bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
         NO-LOCK:

         RUN VALUE(iNameProc)(bloan.contract, bloan.cont-code, iOper, iDate,
                              OUTPUT vAmtCur, OUTPUT vAmtDbCur, OUTPUT vAmtCrCur).
         ASSIGN
            oAmtRes = oAmtRes + vAmtCur
            oAmtDb  = oAmtDb  + vAmtDbCur
            oAmtCr  = oAmtCr  + vAmtCrCur
            .
      END.
END PROCEDURE.

/*----------------------------------------------------------------------------- */
/* Возвращение оборота за определенный день проведения операции по ID операции  */
/*----------------------------------------------------------------------------- */
PROCEDURE GetLastSummOper.
    DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
    DEF INPUT PARAM iOper      AS INT64  NO-UNDO.   /* ид операции */
    DEF INPUT PARAM iDate      AS DATE NO-UNDO.   /* ата отчета */
    DEF OUTPUT PARAM oAmtPrm   AS DEC  NO-UNDO.
    DEF OUTPUT PARAM oAmtDb    AS DEC  NO-UNDO.   /* атычки */
    DEF OUTPUT PARAM oAmtCr    AS DEC  NO-UNDO.   /* атычки */

    DEFINE BUFFER b_chowhe FOR chowhe.
    DEFINE BUFFER b_lint   FOR loan-int.

    ASSIGN
    oAmtPrm = 0.00
    oAmtDb  = 0.00
    oAmtCr  = 0.00
        .

    FIND FIRST b_chowhe WHERE
        b_chowhe.id-op EQ iOper
        NO-LOCK NO-ERROR.

    /* последний платеж мог быть только проценты, ищем последнюю сумму по данному параметру, а не в конкретную дату */
    IF AVAIL b_chowhe THEN
        FOR EACH b_lint
            WHERE b_lint.contract  EQ iContract
              AND b_lint.cont-code EQ iContCode
              AND b_lint.id-d      EQ b_chowhe.id-d
              AND b_lint.id-k      EQ b_chowhe.id-k
              AND b_lint.mdate     EQ iDate
            NO-LOCK
            BREAK BY b_lint.mdate DESC:

            oAmtPrm = oAmtPrm + b_lint.amt-rub.
            IF LAST-OF( b_lint.mdate) THEN LEAVE.
        END.
END PROCEDURE.
/*
FUNCTION WRITEDATAELEMENT (hSAXWriter AS HANDLE, strWrite AS CHAR, bWrite AS LOGICAL):
    IF bWrite THEN
END FUNCTION.
*/

FUNCTION GetFinanceType RETURN CHAR
    ( iCh AS CHAR):
    RETURN TRIM( iCh).
END FUNCTION.

FUNCTION XmlCurr RETURN CHAR
    ( iCurr AS CHAR ):

    FIND FIRST currency
        WHERE currency.currency EQ iCurr
        NO-LOCK NO-ERROR.
    RETURN IF AVAIL currency THEN currency.i-currency ELSE "".
END FUNCTION.

FUNCTION XmlDate RETURN CHAR
    ( iDate AS DATE ):

    RETURN IF iDate EQ ? THEN "" ELSE STRING(iDate, "99.99.9999").
END FUNCTION.

FUNCTION XmlDec RETURN CHAR
    ( iD AS DEC):
    RETURN TRIM( STRING( iD, "->>>>>>>>>>>>>>9.99")).
END FUNCTION.

FUNCTION GetCodeZal RETURN CHAR
   (strCode AS CHAR):

    FOR EACH code
        WHERE (code.class   = "НБКИ_КодЗалога")
          AND (code.parent  = "НБКИ_КодЗалога")
          AND (code.misc[1] <> "")
        NO-LOCK:

        IF (code.misc[1] = strCode)
        THEN RETURN code.code.
    END.
    RETURN "20".
END FUNCTION.

FUNCTION CREcalcParams RETURN DEC
    ( BUFFER loan FOR loan,
      iDateRep AS DATE,
      iPrmNo AS CHAR
    ):

    def var dat-per as date no-undo.
    DEF VAR vR AS DEC NO-UNDO.
    def var mInter AS CHAR NO-UNDO.
    def var mI AS INT64 NO-UNDO.
    def var date1 AS DATE NO-UNDO.

    vR = 0.
    mInter = FGetSettingEx("InterCurrent", "", "", NO).    /* Параметры начисления процентов */
    mI = LOOKUP( iPrmNo, mInter).

    IF mI >0 THEN DO ON ERROR UNDO, THROW:
    {ch_dat_p.i}
    {empty otch1}
    date1 = loan.open-date.
        FOR EACH loan-var
         WHERE loan-var.contract  EQ loan.contract
           AND loan-var.cont-code EQ loan.cont-code
           AND loan-var.since <= iDateRep
           AND loan-var.amt-id EQ INT(iPrmNo)
           NO-LOCK BY loan-var.since DESC
            ON ERROR UNDO, THROW:

            date1 = loan-var.since + 1.
            vR = loan-var.balance.
            LEAVE.
        END.
        IF date1 < iDateRep THEN DO ON ERROR UNDO, THROW:
        RUN lnscheme.p ( loan.contract, loan.cont-code, date1, iDateRep, dat-per, mI, 1).
        for each otch1:
        vR = vR + otch1.summ_pr.
        end.
            FOR EACH loan-int
             WHERE loan-int.contract EQ loan.contract
               AND loan-int.cont-code EQ loan.cont-code
               AND loan-int.id-k EQ INT(iPrmNo)
               AND loan-int.mdate >= date1
               AND loan-int.mdate <= iDateRep
             NO-LOCK ON ERROR UNDO, THROW:

             vR = vR - loan-int.amt-rub.
            END.
        END.
    END.

    RETURN vR.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* вычисление суммы операций по списку параметров с учетом кред.линий (BAV)   */
/*----------------------------------------------------------------------------*/
FUNCTION ListGetSummByIntVarLin RETURNS DECIMAL
    (BUFFER vLoan FOR loan, /* кредитный договор */
     iDate     AS DATE,     /* дата выгрузки */
     iListOper AS CHAR):    /* список параметров классификатора НБКИ_КодОпер */

    DEF VAR vAmt    AS DEC   NO-UNDO. /* общая сумма */
    DEF VAR vI      AS INT64 NO-UNDO. /* счетчик */
    DEF BUFFER bloan    FOR loan.

    IF (vLoan.cont-type = "Течение")
    THEN DO:
        vAmt = 0.0.
        DO vI = 1 TO NUM-ENTRIES(iListOper):
            IF     ((INDEX(ENTRY(vI,iListOper), "ОД") <> 0) AND     /* ОД ведется на траншах */
                     CAN-DO(FGetSetting("НБКИ", "ЛинииОДТранш", ""), vLoan.class-code))
                OR ((INDEX(ENTRY(vI,iListOper), "ОД") =  0) AND     /* %% ведутся на траншах */
                     CAN-DO(FGetSetting("НБКИ", "Линии%%Транш", ""), vLoan.class-code))
            THEN DO:
                FOR EACH bloan
                    WHERE  bloan.contract   = vLoan.contract
                      AND  bloan.cont-code  BEGINS vLoan.cont-code + " "
                      AND  bloan.cont-code  <> vLoan.cont-code
                      AND  bloan.open-date  <= iDate  /* Транши, еще не выданные на дату расчета, не учитываем */
                      AND (bloan.close-date =  ?
                        OR bloan.close-date >  iDate)
                    NO-LOCK:

                    vAmt = vAmt + ListGetSummByIntVar(BUFFER bloan, iDate, ENTRY(vI,iListOper)).
                END.
            END.
            ELSE vAmt = vAmt + ListGetSummByIntVar(BUFFER vLoan, iDate, ENTRY(vI,iListOper)).
        END.
        RETURN vAmt.
    END.
    ELSE RETURN  ListGetSummByIntVar(BUFFER vLoan, iDate, iListOper).
END FUNCTION.

FUNCTION CREListGetSummByIntVar RETURNS DECIMAL
    (BUFFER loan FOR loan,  /* кредитный договор */
     iDate     AS DATE,     /* дата выгрузки */
     iListOper AS CHAR):    /* список параметров классификатора НБКИ_КодОпер */

   DEF VAR vDatePay  AS DATE NO-UNDO. /* Дата последней выплаты */
   DEF VAR vAmtCur   AS DEC  NO-UNDO. /* сумма по одной операции */
   DEF VAR vAmt      AS DEC  NO-UNDO. /* общая сумма */
   DEF VAR vI        AS INT64  NO-UNDO. /* счетчик */
   DEF VAR vY        AS INT64  NO-UNDO. /* счетчик */
   DEF BUFFER bcode FOR CODE.

   ASSIGN vAmt      = 0.00.

   DO vI = 1 TO NUM-ENTRIES(iListOper):

    IF NOT GetCodeBuff("НБКИ_КодОпер", ENTRY(vI,iListOper), BUFFER bcode)
       THEN RETURN 0. /* RETURN ERROR "В классификаторе НБКИ_КодОпер нет параметра " + ENTRY(vI,iListOper).*/

    /* список параметров, остатки по которым нужно сложить */
    IF bcode.misc[3] > "" THEN
    DO vY = 1 TO NUM-ENTRIES(bcode.misc[3]):
            vAmtCur = CREcalcParams(BUFFER loan, iDate, ENTRY(vY, bcode.misc[3])).
            vAmt = vAmt + vAmtCur.
        END.
   END.

   RETURN vAmt.
END FUNCTION.

/* своевременность платежей, украдено из pp-bki */
FUNCTION BKIMOP2 RETURNS CHAR
    (iContract AS CHAR,               /* кредитный договор */
     iContCode AS CHAR,
     iDate AS DATE):                  /* дата выгрузки */

   DEF VAR vAcctMOP   AS CHAR    NO-UNDO. /* доп. реквизит на договоре */
   DEF VAR vDueDate   AS DATE    NO-UNDO. /* дата выноса на просрочку */
   DEF VAR vOpDate    AS DATE    NO-UNDO. /* дата операции погашения ОД в счет обеспечения */
   DEF VAR vPeriod    AS INT64   NO-UNDO.
   DEF VAR vVersion   AS DECIMAL NO-UNDO.
   DEF VAR vNBKISvMes AS LOG     NO-UNDO. /* НП НБКИ_Св_Мес */

   vVersion = DECIMAL(FGetSetting("НБКИ","Версия","")) NO-ERROR.
   vNBKISvMes = FGetSetting("НБКИ_Св_Мес","","Нет") EQ "Да".

   {profile BK351}
   vAcctMOP = GetXattrValue("loan", iContract + "," + iContCode, "НБКИ_Своевременн").

   /* если доп. реквизит не задан, то вычисляем его */
   IF vAcctMOP = "" THEN DO:
      /* проверяем, была ли оплата в счет обепечения */
      RUN GetFirstDateUnv(iContract, iContCode, iDate, ?, "PayByOB", OUTPUT vOpDate).
      IF vOpDate <> ?
      THEN vAcctMOP = "8".
      ELSE DO:
            /* Определяем дату неоплаченного выноса на просрочку */
          RUN GetFirstDueDate(iContract, iContCode, iDate, OUTPUT vDueDate).
          vPeriod = iDate - vDueDate.
             /* Если задана настройка, то опрелеяем период по-другому */
          IF vNBKISvMes THEN
             IF vDueDate EQ ?
             THEN vPeriod = 0.
             ELSE vPeriod = iDate - MAX(vDueDate, FirstMonDate(iDate)) + 1.

          IF     NOT vNBKISvMes
             AND vDueDate = ? THEN DO:

              /* определяем дату последней выплаты - если нет, то новый */
              IF BKIDateOFLstPay(iContract, iContCode, iDate) = ?
              THEN vAcctMOP = "0".         /* новый */
              ELSE vAcctMOP = "0".         /* оплата без просрочек */
          END.
          ELSE IF vPeriod <  31                   THEN vAcctMOP = "A".
          ELSE IF vPeriod >  30 AND vPeriod <  61 THEN vAcctMOP = "1".
          ELSE IF vPeriod >  60 AND vPeriod <  91 THEN vAcctMOP = "2".
          ELSE IF vPeriod >  90 AND vPeriod < 121 THEN vAcctMOP = "3".
          ELSE IF vPeriod > 120 AND vPeriod < 151 THEN vAcctMOP = "4".
          ELSE IF vPeriod > 150 AND vPeriod < 181 THEN vAcctMOP = "5".
          ELSE IF vPeriod >= 180                  THEN vAcctMOP = "6".
      END.
   END.
   {profile BK360}
   RETURN vAcctMOP.
END FUNCTION.

/* выгружаем Судебные решения */
PROCEDURE CREexportDspt:
    DEFINE INPUT PARAMETER hSAXWriter AS HANDLE NO-UNDO.
    DEFINE PARAMETER BUFFER bTerm-obl FOR term-obl.

    DEF VAR lOk     AS LOG  NO-UNDO.
    DEF VAR cSurr   AS CHAR NO-UNDO.
    DEF VAR strTmp  AS CHAR NO-UNDO.

    put unformatted '   Судебное решение ' + bTerm-obl.lnk-cont-code + " - " + STRING(bTerm-obl.end-date, "99.99.9999") skip.
    cSurr   = 'Кредит' + "," + bTerm-obl.cont-code + ",7," + STRING(bTerm-obl.end-date) + "," + STRING(bTerm-obl.nn).
    lOK     = hSAXWriter:START-ELEMENT("Legal").
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT( "DateReported", XmlDate(bTerm-obl.end-date)).
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT("ClaimNumber",   bTerm-obl.lnk-cont-code).
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT("CourtName",     bTerm-obl.fuser-id).
    IF bTerm-obl.fop-date <> ?
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT( "DateConsideration", XmlDate(bTerm-obl.fop-date)).
    IF bTerm-obl.sop-date <> ?
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT( "DateSatisfied",     XmlDate(bTerm-obl.sop-date)).
    IF bTerm-obl.suser-id <> ? AND bTerm-obl.suser-id <> ''
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("Plaintiff",          bTerm-obl.suser-id).
    strTmp  = GetXattrValue  ("term-obl", cSurr, "Verdict").
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT("Resolution", strTmp).
    strTmp  = GetXattrValueEx("term-obl", cSurr, "Issue", "б/н").
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT("ResolutionNumber", strTmp).
    strTmp  = GetXattrValue("term-obl", cSurr, "ИскЗаявСум").
    lOK     = hSAXWriter:WRITE-DATA-ELEMENT("ResolutionType", IF (strTmp = '') THEN '9' ELSE '3').
    IF strTmp <> ''
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("RecoveryAmount", strTmp).
    lOK     = hSAXWriter:END-ELEMENT("Legal").
END PROCEDURE.

PROCEDURE CREexportCorp:
    DEFINE INPUT PARAMETER hSAXWriter AS HANDLE NO-UNDO.
    DEFINE PARAMETER BUFFER cust-corp FOR cust-corp.
    DEFINE PARAMETER BUFFER bloan FOR loan.

    DEF VAR lOk AS LOG NO-UNDO.
    def var mygroup as char no-undo.
    def var country as char no-undo.
    country = GetXAttrValue( "cust-corp", STRING(cust-corp.cust-id), "country-id2").
    country = IF (country EQ "") THEN cust-corp.country-id ELSE country.

    put unformatted ' ЮЛ ' + STRING(cust-corp.cust-id) skip.
    lOK = hSAXWriter:START-ELEMENT("Business").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReferenceCode", {&ULPREFIX} + STRING(cust-corp.cust-id)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Name", cust-corp.name-short).

    IF (country EQ "RUS")
    THEN DO:
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("RegistrationNumber", GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "огрн")).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("INN", cust-corp.inn).
    END.

    /* Резидент */
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Resident", IF (country EQ "RUS") THEN "1" ELSE "0").
/*  IF GetXAttrValue( "cust-corp", STRING(cust-corp.cust-id), "country-id2") NE "RUS" THEN
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Resident", "0").
*/
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CountryReg",
          string(GetXAttrValue( "country",
                                string( GetXAttrValue( "cust-corp", STRING(cust-corp.cust-id), "country-id2")),
                                "ALFA-2"), 'x(2)')).
    mygroup = ?.
    if avail bloan then do:
        mygroup = GetXattrValueEx("loan", bloan.contract + "," + bloan.cont-code, "CreGroup", ?).
        if mygroup <> ? then
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("GroupCode", mygroup).
    end.

    /* Документы нерезидентов */
    IF (country NE "RUS")
    THEN DO:
        lOK = hSAXWriter:START-ELEMENT("Documents").
        lOK = hSAXWriter:START-ELEMENT("Document").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", "35").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "огрн")).
        lOK = hSAXWriter:END-ELEMENT("Document").
        lOK = hSAXWriter:START-ELEMENT("Document").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", "82").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", cust-corp.inn).
        lOK = hSAXWriter:END-ELEMENT("Document").
        lOK = hSAXWriter:END-ELEMENT("Documents").
    END.

    /* Адреса ******************************************************* */
    lOK = hSAXWriter:START-ELEMENT("Addresses").
    DEF VAR vAdrType AS INT NO-UNDO.
    DO vAdrType = 1 TO 2:
        FIND FIRST cust-ident
             WHERE cust-ident.cust-cat       EQ "ю"
                   AND cust-ident.cust-id        EQ cust-corp.cust-id
                   AND cust-ident.class-code     EQ "p-cust-adr"
                   AND cust-ident.cust-code-type EQ ENTRY( vAdrType, "адрюр,адрфакт")
                   AND (cust-ident.close-date EQ ? OR cust-ident.close-date > TODAY)
             NO-LOCK NO-ERROR.
        IF NOT AVAIL cust-ident AND vAdrType EQ 2 THEN DO:
            FIND FIRST cust-ident
             WHERE cust-ident.cust-cat       EQ "ю"
                   AND cust-ident.cust-id        EQ cust-corp.cust-id
                   AND cust-ident.class-code     EQ "p-cust-adr"
                   AND cust-ident.cust-code-type EQ ENTRY( 1, "адрюр,адрфакт")
                   AND (cust-ident.close-date EQ ? OR cust-ident.close-date > TODAY)
             NO-LOCK NO-ERROR.
        END.
        IF NOT AVAIL cust-ident THEN NEXT.
        DO ON ERROR UNDO, LEAVE:
        DEF VAR vRegion AS CHAR NO-UNDO.
        vRegion = GetXAttrValue( 'cust-ident',
            cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),
            'КодРегГНИ').
        lOK = hSAXWriter:START-ELEMENT("Address").
        {cust-adr.obj
             &addr-to-vars = YES
             &tablefield   = "TRIM(cust-ident.issue)"
        }

        IF vRegion EQ "77" AND vGorChar EQ "" THEN vGorChar = "Москва г".
        IF vRegion EQ "78" AND vGorChar EQ "" THEN vGorChar = "Санкт-Петербург г".
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", string(vAdrType + 2)).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Location", vGorChar + (IF vPunktChar <> "" AND vGorChar <> "" THEN ", " ELSE "") + vPunktChar ).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Street", vUlChar).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("PostalCode", IF vAdrIndInt EQ 0 THEN "" ELSE string(vAdrIndInt)).
        DEF VAR vAdrCountry AS CHAR NO-UNDO.
        vAdrCountry = GetXattrValue("cust-ident",
                      cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),
                      "country-id").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Country",string(GetXAttrValue("country", string(vAdrCountry), "ALFA-2"))).
        IF vRegion Ne ? THEN
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("Region", vRegion).
/*        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Area", vOblChar).
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("District", vOblChar).
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("StreetType", "").
*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("HouseNumber", vDomChar).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Block", vKorpChar).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Building", vStrChar).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Apartment", vKvChar).
/*      lOK = hSAXWriter:WRITE-DATA-ELEMENT("Status", "").*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Since", XmlDate( cust-ident.open-date)).
        lOK = hSAXWriter:END-ELEMENT("Address").
        END. /* DO */
    END.
    lOK = hSAXWriter:END-ELEMENT("Addresses").

    /* Телефоны ***************************************************** */
    DEF VAR vTel AS CHAR NO-UNDO.
    vTel = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "tel", "").
    IF vTel NE "" OR cust-corp.fax NE "" THEN DO:
        lOK = hSAXWriter:START-ELEMENT("Phones").
        IF vTel NE "" THEN DO:
                lOK = hSAXWriter:START-ELEMENT("Phone").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", "1").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", vTel).
                lOK = hSAXWriter:END-ELEMENT("Phone").
        END.
        IF cust-corp.fax NE "" THEN DO:
                lOK = hSAXWriter:START-ELEMENT("Phone").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", "3").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", cust-corp.fax).
                lOK = hSAXWriter:END-ELEMENT("Phone").
        END.
        lOK = hSAXWriter:END-ELEMENT("Phones").
    END.
END PROCEDURE.

PROCEDURE CREexportPerson:
    DEFINE INPUT PARAMETER hSAXWriter AS HANDLE NO-UNDO.
    DEFINE PARAMETER BUFFER person FOR person.
    DEFINE PARAMETER BUFFER bloan FOR loan.
    DEF VAR lOk AS LOG NO-UNDO.

    put unformatted ' ФЛ ' + STRING(person.person-id) skip.
    lOK = hSAXWriter:START-ELEMENT("Person").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReferenceCode", {&FLPREFIX} + STRING(person.person-id)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("LastName", string(person.name-last)).
    DEF VAR FirstName AS CHAR NO-UNDO.
    FirstName = string(ENTRY(1,person.first-names,' ')).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("FirstName",  string(trim(FirstName))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MiddleName", string(trim(substring( person.first-names, LENGTH(FirstName) + 1)))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BirthDate",  XmlDate( person.birthday)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BirthPlace", GetXAttrValue("person", STRING(person.person-id), "BirthPlace")).

    /* CRE: 1 - мужской, 2 - женский
       BIS: 1 - мужской, 0 - женский */
    lOK = hSAXWriter:WRITE-DATA-ELEMENT( "Gender", (IF person.gender THEN "1" ELSE "2")).

    /* гражданство: 99 - неизвестно */
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Citizenship",
    string(
        GetXAttrValue( "country",
        string( GetXAttrValue( "person", STRING(person.person-id), "country-id2")),
        "ALFA-2"),
        'x(2)')).
    /* 1 холост,2 женат/замужем,3 овдовевший,4 разведенный,5 повторно женился/замужем,6 иное
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MaritalStatus", ).
    кол-во иждивенцев
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Dependants", ). */
    IF GetXAttrValueEx("person", STRING(person.person-id), "BirthPlace", "Нет") = "Да"
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("Remarks", "1").
    DEF VAR strr AS CHAR NO-UNDO.
    strr = GetXAttrValueEx("person", STRING(person.person-id), "ИзмФИПредФам", "").
    IF strr <> ""
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldLastName", strr).
    strr = GetXAttrValueEx("person", STRING(person.person-id), "ИзмФИПредИмя", "").
    IF strr <> ""
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldFirstName", strr).
    IF person.inn NE "" AND person.inn NE ?
    THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("INN", person.inn).

    def var mygroup as char no-undo.
    if avail bloan then do:
        mygroup = GetXattrValue("loan", bloan.contract + "," + bloan.cont-code, "CreGroup").
        if mygroup <> "" then
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("GroupCode", mygroup).
    end.

    /* Документы ************************************************ */
    DEF VAR doc_cnt AS INT NO-UNDO.
    doc_cnt = 0.
    FOR EACH cust-ident
        WHERE cust-ident.cust-cat   EQ "Ч"
          AND cust-ident.cust-id    EQ Person.person-id
          AND cust-ident.class-code EQ "p-cust-ident"
          AND cust-ident.close-date EQ ?
        NO-LOCK
        BREAK BY cust-ident.cust-id
        ON ERROR UNDO, THROW:

        IF FIRST(cust-ident.cust-id) THEN
        lOK = hSAXWriter:START-ELEMENT("Documents").

        lOK = hSAXWriter:START-ELEMENT("Document").
        /* тип документа совпадает с классификатором фнс , 99 - иной,
           31 нет в фнс, но есть в CRE */
        DEF VAR vDocType AS CHAR NO-UNDO.
        vDocType = GetCode("КодДокум", cust-ident.cust-code-type).
        IF vDocType EQ ? OR vDocType EQ "" THEN vDocType = "91".
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", vDocType).
        DEF VAR vCustCode       AS CHAR NO-UNDO.
        DEF VAR vCustCodeNum    AS CHAR NO-UNDO.
        DEF VAR vCustCodeSer    AS CHAR NO-UNDO.
        DEF VAR vCodePodr       AS CHAR NO-UNDO.

        vCustCode    = TRIM (cust-ident.cust-code).
        vCustCodeNum = ENTRY(NUM-ENTRIES(vCustCode," "),vCustCode," ").
        vCustCodeSer = TRIM(SUBSTR(vCustCode, 1, LENGTH(vCustCode) - LENGTH(vCustCodeNum))).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Series", vCustCodeSer).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", vCustCodeNum).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueDate", XmlDate( cust-ident.open-date)).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueAuthority", REPLACE(cust-ident.issue, "~n", " ")).
        vCodePodr = cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num).
        vCodePodr = GetXAttrValue("cust-ident", vCodePodr, "Подразд").
        IF (vCodePodr <> "") AND (vDocType = "21")
        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueCode", vCodePodr).
/*          lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldNumber", ).
*/
        lOK = hSAXWriter:END-ELEMENT("Document").
        doc_cnt = doc_cnt + 1.
    END.
/*      DEF VAR vOGRN AS CHAR NO-UNDO.
    vOGRN = GetXattrValue("person", STRING(iCustID),"огрн").
    IF {assigned vOGRN} THEN DO:
    lOK = hSAXWriter:START-ELEMENT("Document").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", "33").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Series", "").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", vOGRN).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueDate",
        XmlDate( cust-ident.open-date)).
    lOK = hSAXWriter:END-ELEMENT("Document").

    doc_cnt = doc_cnt + 1.
    END.
*/
    IF doc_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Documents").

    /* Адреса *************************************************** */
    lOK = hSAXWriter:START-ELEMENT("Addresses").
    DEF VAR vAdrType AS INT NO-UNDO.
    DO vAdrType = 1 TO 2:
    FIND FIRST cust-ident
         WHERE cust-ident.cust-cat       EQ "Ч"
               AND cust-ident.cust-id        EQ Person.person-id
               AND cust-ident.class-code     EQ "p-cust-adr"
               AND cust-ident.cust-code-type EQ ENTRY( vAdrType, "адрпроп,адрфакт")
               AND (cust-ident.close-date EQ ? OR cust-ident.close-date > TODAY)
         NO-LOCK NO-ERROR.
    IF NOT AVAIL cust-ident AND vAdrType EQ 2 THEN DO:
        FIND FIRST cust-ident
         WHERE cust-ident.cust-cat       EQ "Ч"
               AND cust-ident.cust-id        EQ Person.person-id
               AND cust-ident.class-code     EQ "p-cust-adr"
               AND cust-ident.cust-code-type EQ ENTRY( 1, "адрпроп,адрфакт")
               AND (cust-ident.close-date EQ ? OR cust-ident.close-date > TODAY)
         NO-LOCK NO-ERROR.
    END.
    IF NOT AVAIL cust-ident THEN NEXT.
    DO ON ERROR UNDO, LEAVE:
    DEF VAR vRegion AS CHAR NO-UNDO.
    vRegion = GetXAttrValue( 'cust-ident',
        cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),
        'КодРегГНИ').
    lOK = hSAXWriter:START-ELEMENT("Address").

    {cust-adr.obj
         &addr-to-vars = YES
         &tablefield   = "TRIM(cust-ident.issue)"
    }
    IF vRegion EQ "77" AND vGorChar EQ "" THEN vGorChar = "Москва г".
    IF vRegion EQ "78" AND vGorChar EQ "" THEN vGorChar = "Санкт-Петербург г".

    /* 09/07/14
    IF vUlChar EQ "" THEN
            ASSIGN
              vUlChar = vPunktChar
              vPunktChar = ""
              .
    IF vUlChar EQ "" THEN
            ASSIGN
              vUlChar = vGorChar
              vGorChar = ""
              .
    */

    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", string(vAdrType)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Location", vGorChar + (IF vPunktChar <> "" AND vGorChar <> "" THEN ", " ELSE "") + vPunktChar ).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Street", vUlChar).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("PostalCode", IF vAdrIndInt EQ 0 THEN "" ELSE string(vAdrIndInt)).
    DEF VAR vAdrCountry AS CHAR NO-UNDO.
    vAdrCountry = GetXattrValue("cust-ident",
                                              cust-ident.cust-code-type + ',' +
                                              cust-ident.cust-code      + ',' +
                                              STRING(cust-ident.cust-type-num),
                                              "country-id"
                                             ).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Country",
        string(GetXAttrValue( "country",
        string( vAdrCountry),
        "ALFA-2"))).
    IF vRegion Ne ? THEN
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Region", vRegion).
    /*lOK = hSAXWriter:WRITE-DATA-ELEMENT("Area", vOblChar).
      lOK = hSAXWriter:WRITE-DATA-ELEMENT("District", vOblChar).
      lOK = hSAXWriter:WRITE-DATA-ELEMENT("StreetType", "").
    */
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("HouseNumber", vDomChar).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Block", vKorpChar).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Building", vStrChar).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Apartment", vKvChar).
    /*lOK = hSAXWriter:WRITE-DATA-ELEMENT("Status", "").*/
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Since",
        XmlDate( cust-ident.open-date)).
    lOK = hSAXWriter:END-ELEMENT("Address").
    END. /* DO */
    END.
    lOK = hSAXWriter:END-ELEMENT("Addresses").

    /* Телефоны ************************************************* */
    DEF VAR ph AS CHAR EXTENT 4 NO-UNDO.
    ph[1] = ENTRY(1, person.phone[2]).
    ph[2] = IF NUM-ENTRIES( person.phone[1]) > 1 THEN ENTRY(2, person.phone[1]) ELSE "".
    IF ph[2] EQ "" THEN ph[2] = ENTRY(1, person.phone[1]).
    IF ph[2] EQ "" THEN ph[2] = GetXattrValueEx("person", STRING(person.person-id), "phone-home", "").
    ph[3] = person.fax.
    ph[4] = IF NUM-ENTRIES( person.phone[2]) > 1 THEN ENTRY(2, person.phone[2]) ELSE "".
    IF ph[4] EQ "" THEN ph[4] = GetXattrValueEx("person", STRING(person.person-id), "cell-phone", "").
    IF ph[1] NE "" OR ph[2] NE "" OR ph[3] NE "" OR ph[4] NE ""
    THEN DO:
        DEF VAR ik AS INT NO-UNDO.
        lOK = hSAXWriter:START-ELEMENT("Phones").
        DO ik = 1 TO 4:
            IF ph[ik] NE "" THEN DO:
            IF LENGTH(ph[ik]) > 14 THEN DO:
                DEF VAR iu AS INT NO-UNDO.
                iu = 15.
                strr = ph[ik].
                DO WHILE ASC(substr(strr, iu, 1)) >= ASC('0') AND
                         ASC(substr(strr, iu, 1)) <= ASC('9') AND
                     ASC(substr(strr, iu - 1, 1)) >= ASC('0') AND
                     ASC(substr(strr ,iu - 1, 1)) <= ASC('9') AND
                     iu > 2:
                
                    iu = iu - 1.
                END.

                DO WHILE LOOKUP( substr( strr, iu - 1, 1), ",x x;x-", "x") > 0 AND
                     iu > 2 :

                     iu = iu - 1.
                END.
                ph[ik] = substr( strr, 1, (IF iu >= 5 THEN iu - 1 ELSE 14)).
            END.
            lOK = hSAXWriter:START-ELEMENT("Phone").
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", STRING(ik)).
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", ph[ik]).
            lOK = hSAXWriter:END-ELEMENT("Phone").
            END.
        END.
        lOK = hSAXWriter:END-ELEMENT("Phones").
    END.

    /* Банкротства  ********************************************* * /
    doc_cnt = 0.
    FOR EACH loan
        WHERE (loan.class-code  = 'bankrupt')
          AND (loan.contract    = 'bankrupt')
          AND (loan.cust-cat    = 'Ч')
          AND (loan.cust-id     = person.person-id)
        NO-LOCK:

        IF (doc_cnt = 0) THEN
        lOK = hSAXWriter:START-ELEMENT("Bankruptcies").

        lOK = hSAXWriter:START-ELEMENT("Bankruptcy").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateReported", XmlDate(loan.open-date)).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CourtName", ).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CaseNumber", ).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Status", GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "ТипБанкр")).
        lOK = hSAXWriter:END-ELEMENT("Bankruptcy").
        doc_cnt = doc_cnt + 1.
    END.
    IF doc_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Bankruptcies").
*/
END PROCEDURE.

PROCEDURE CREexportTrade:
    DEFINE INPUT PARAMETER  iInfo       AS CHAR     NO-UNDO.    /* Для лога */
    DEFINE INPUT PARAMETER  hSAXWriter  AS HANDLE   NO-UNDO.    /* XML-структура   */
    DEFINE INPUT PARAMETER  iCustCat    AS CHAR     NO-UNDO.    /* Клиент, который */
    DEFINE INPUT PARAMETER  iCustID     AS INT64    NO-UNDO.    /*   выгружается   */
    DEFINE INPUT PARAMETER  iDateRep    AS DATE     NO-UNDO.    /* Дата отчета     */
    DEFINE PARAMETER BUFFER loan        FOR loan.               /* Договор */
    DEFINE PARAMETER BUFFER bTerm       FOR term-obl.           /* Поручительство */
    DEFINE INPUT PARAMETER  bGuar       AS LOGICAL  NO-UNDO.    /* Выгружаем не договор, а поручительство */
    DEFINE INPUT PARAMETER  dPoruc      AS DATE     NO-UNDO.    /* Дата состояния договора для CRE */

    DEFINE VARIABLE vLoanSurr       AS CHAR NO-UNDO. /* Суррогат договора */
    DEFINE VARIABLE lOk             AS LOG  NO-UNDO.
    DEFINE VARIABLE vAccRelation    AS CHAR NO-UNDO.
    DEFINE VARIABLE vRegNum         AS CHAR NO-UNDO.
    DEFINE VARIABLE vPaymentFrequency AS CHAR NO-UNDO.
    DEFINE VARIABLE strtmp          AS CHAR NO-UNDO.
    DEFINE VARIABLE strXattr        AS CHAR NO-UNDO.
    DEFINE VARIABLE cSost           AS CHAR NO-UNDO.
    DEFINE VARIABLE dSost           AS DATE NO-UNDO.
    DEFINE VARIABLE iTmp            AS INT  NO-UNDO.

    DEFINE BUFFER   cust-corp       FOR cust-corp.
    DEFINE BUFFER   person          FOR person.
    DEFINE BUFFER   signs           FOR signs.
    DEFINE BUFFER   transh          FOR loan.

    vLoanSurr = loan.contract + "," + loan.cont-code.

    put unformatted '   ' + iInfo + ' ' + loan.cont-code + " - " + STRING(iDateRep, "99.99.9999") skip.
    lOK = hSAXWriter:START-ELEMENT("Trade").
    DO ON ERROR UNDO, THROW:
        /* DateReported ************************************************************************************* */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateReported", XmlDate( iDateRep)).
        /* MemberCode   ************************************************************************************* */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MemberCode", {&NBKICODE}).

        /* Account      ************************************************************************************* */
        DEF VAR oldAcc AS CHAR NO-UNDO INIT "".

        strtmp = REPLACE( loan.cont-code, '@', '/').
        CASE loan.filial-id:
            WHEN "0000" THEN DO:
                IF (loan.open-date < 01/01/2014)
                THEN DO:
                    IF (iDateRep < 01/01/2014)
                    THEN strtmp = loan.doc-ref + "@0400".
                    ELSE IF (iDateRep < 05/01/2015)
                    THEN ASSIGN strtmp = loan.doc-ref + "/0400" oldAcc = loan.doc-ref + "@0400".
                    ELSE oldAcc = IF bGuar THEN (loan.doc-ref + "@0400") ELSE (loan.doc-ref + "/0400").
                END.
                ELSE IF (loan.open-date < 05/01/2015)
                THEN DO:
                    IF (iDateRep < 05/01/2015)
                    THEN strtmp = loan.doc-ref + "/0400".
                    ELSE oldAcc = loan.doc-ref + "/0400".
                END.
            END.
            WHEN "0300" THEN DO:
                IF (loan.open-date < 09/04/2015)
                THEN DO:
                    IF (iDateRep < 09/04/2015)
                    THEN strtmp = loan.doc-ref.
                    ELSE oldAcc = loan.doc-ref.
                END.
            END.
            WHEN "0400" THEN DO:
                IF (loan.open-date < 01/01/2014)
                THEN DO:
                    IF (iDateRep < 01/01/2014) /* OR (iDateRep = NeedExport(BUFFER loan, 12/31/2013)) */
                    THEN strtmp = loan.cont-code.
                    ELSE oldAcc = loan.cont-code.
                END.
            END.
            WHEN "0500" THEN DO:
                IF (loan.open-date < 10/01/2016)
                THEN DO:
                    IF (iDateRep < 10/01/2016)
                    THEN strtmp = loan.doc-ref.
                    ELSE oldAcc = loan.doc-ref.
                END.
            END.
        END CASE.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Account", strtmp).

        /* Type         ************************************************************************************* */
        /* тип кредита
        код начение
        1    кредит на автомобиль
        4    лизинг
        6    ипотека
        7    кредитная карта
        9    потребительский кредит
        10    на развитие бизнеса
        11    на пополнение оборотных средств
        12    на покупку оборудования
        13    на строительство
        14    на покупку акций
        15    межбанковский кредит */
        DEF VAR oAccType AS CHAR NO-UNDO.
        oAccType = BKIAccType(BUFFER loan).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", oAccType).

        /* Relationship ************************************************************************************* */
        /*4.17 отношение к счету
        код значение
        1 физическое лицо
        2 дополнительная карта (только для кредитных карт)
        3 авторизованный пользователь (доверенное лицо)
        4 совместный
        5 поручитель
        9 юридическое лицо*/
        /* lOK = hSAXWriter:WRITE-DATA-ELEMENT("Relationship", IF loan.cust-cat EQ "Ч" THEN "1" ELSE "9"). */
        vRegNum = "".
        IF (iCustCat EQ "Ч")
        THEN DO:
            FOR EACH loan-acct OF loan
                WHERE (loan-acct.acct-type  = "Кредит")
                  AND (loan-acct.acct  BEGINS "454")
                NO-LOCK:

                vRegNum = "Предпр".
            END.
        END.
/*      vRegNum = IF (iCustCat EQ "Ч") THEN GetXattrValue("person", STRING(iCustID), "Предпр") ELSE "". */

        vAccRelation = BKIAccRelation (BUFFER loan, BUFFER bTerm,
                                       IF AVAIL(bTerm) THEN (IF bGuar THEN (bTerm.symbol + "," + STRING(bTerm.fop)) ELSE "")
                                                       ELSE (iCustCat     + "," + STRING(iCustID)),
                                       IF (vRegNum <> "") THEN YES ELSE NO).    /* Признак ФЛ-Предпринимателя */
        FOR EACH cust-role
            WHERE cust-role.file-name EQ "loan"
              AND cust-role.surrogate EQ vLoanSurr
              AND cust-role.class-code EQ "созаемщик"
            NO-LOCK:

            IF    (cust-role.cust-cat EQ iCustCat
                AND cust-role.cust-id  EQ STRING(iCustID))  /* Клиент сам является созаемщиком */
                OR vAccRelation EQ "1"                      /* Клиент - ФЛ, владелец договора, у которого есть созаемщик */
            THEN DO:
                vAccRelation = "4". /* созаемщик сам или у заемщика есть любой созаемщик,тогда ставим 4 - совместный */
                LEAVE.
            END.
        END.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Relationship", vAccRelation).

        /* DateOpened   ************************************************************************************* */
        DEF VAR vDateOpened AS DATE NO-UNDO.
        FIND FIRST loan-int OF loan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        vDateOpened = loan-int.mdate.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateOpened", XmlDate(vDateOpened)).

        /* DateLastPayment ********************************************************************************** */
        /*{n-mess.i "STRING(ij) + ' ' + '7'"}*/
        /* 7. дата последней выплаты * - необяз */
        DEF VAR vDateLastPayment AS DATE NO-UNDO.
        vDateLastPayment = BKIDateOFLstPay( loan.contract, loan.cont-code, iDateRep).
        IF vDateLastPayment EQ ? THEN vDateLastPayment = DATE( 1, 2, 1900).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateLastPayment", XmlDate( vDateLastPayment)).

        /* AccountState  ************************************************************************************ */
        /*4.18 состояние счета
        код значение
        00 либо 0    активный
        12        оплачен за счет обеспечения
        13        счет закрыт
        14        передан на обслуживание в другой банк
        21        спор
        52        просрочен
        61        проблемы с возвратом*/
        DEF VAR oAccRat AS CHAR NO-UNDO.
        DEF VAR oDateAccRat AS DATE NO-UNDO.

        RUN BKIAccRating( BUFFER loan, iDateRep, ?, vDateLastPayment, OUTPUT oAccRat, OUTPUT oDateAccRat).
        IF oAccRat EQ "00" THEN DO ON ERROR UNDO, THROW:
            RUN GetLastDueDate( loan.contract, loan.cont-code, iDateRep, OUTPUT oDateAccRat).
            IF (oDateAccRat EQ ?) THEN oDateAccRat = vDateOpened.
        END.

        cSost = GetXattrValue("loan", vLoanSurr, "НБКИ_Состояние").
        dSost = DATE(GetXattrValue("loan", vLoanSurr, "НБКИ_СостДата")) NO-ERROR.
        IF (cSost = '14') AND (iDateRep >= dSost) THEN oAccRat = cSost.

        DEF VAR vPastDue    AS DEC NO-UNDO.
        DEF VAR vPastDuePrs AS DEC NO-UNDO.
        vPastDuePrs = ListGetSummByIntVar(BUFFER loan, iDateRep, "ЗадолжПр").
        vPastDue    = ListGetSummByIntVar(BUFFER loan, iDateRep, "ЗадолжОД") + vPastDuePrs.
        IF (oAccRat EQ "00") AND (vPastDue > 0) THEN oAccRat = '52'.

        IF (oAccRat <> "14") AND (loan.close-date <> ?) AND (iDateRep >= loan.close-date) THEN oAccRat = '13'.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("AccountState", oAccRat).

        /* Currency  **************************************************************************************** */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Currency", XmlCurr(loan.currency)).

        /* Limit     **************************************************************************************** */
        DEF VAR dLimit AS DECIMAL NO-UNDO.
        dLimit = BKICredLimitAmt( BUFFER loan).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT( "Limit", XmlDec(dLimit)).

        /* Balance   **************************************************************************************** */
        /* 11. общая выплаченная сумма, включая
        проценты и пени на дату, указанную в поле дата
        последней выплаты. значение может быть только
        неотрицательным.*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Balance", XmlDec(BKICredBalance(BUFFER loan, iDateRep, vDateLastPayment))).

        /* PastDue   **************************************************************************************** */
        /* 12. сумма просрочки на последнюю дату
           обновления, указанную в поле даты отчёта.*/
/* Эта сумма посчитана раньше
        / * vPastDue = BKIPastDue(BUFFER loan, iDateRep, vDateLastPayment). * /
        vPastDue = ListGetSummByIntVar(BUFFER loan, iDateRep, "ЗадолжОД,ЗадолжПр").
*/
        /*IF iDateRep EQ loan.since THEN*/
        vPastDue = vPastDue + CREListGetSummByIntVar(BUFFER loan, iDateRep, "ПеняОД,ПеняПР").

        DEF VAR vMannerOfPayment AS CHAR NO-UNDO.
        vMannerOfPayment = BKIMOP( loan.contract, loan.cont-code, iDateRep).
        DEF VAR vPastDueNoExtra AS DEC NO-UNDO.
        vPastDueNoExtra = ListGetSummByIntVar(BUFFER loan, iDateRep, "ЗадолжОД").
        IF vMannerOfPayment EQ "1" AND vPastDue > 0 THEN DO:
        IF vPastDueNoExtra <= 0 /* если просрочка од равна нулю, то обнуляем пени */
          THEN vPastDue = 0.
        END.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("PastDue", XmlDec(IF (oAccRat = "14") THEN 0 ELSE MAXIMUM( 0, vPastDue))).

        /* MannerOfPayment   ******************************************************************************** */
        /* 13. своевременность платежей (справочник
           воевременность платежей)\состояние платежа
           по счету на дату отчета DateReported.
           4.19 своевременность платежей
        код значение
        0 новый, оценка невозможна
        1 оплата без просрочек
        A просрочка от 1 до 29 дней
        2 просрочка от 30 до 59 дней
        3 просрочка от 60 до 89 дней
        4 просрочка от 90 до 119 дней
        5 просрочка более 120 дней
        7 регулярные консолидированные платежи
        (значение ставится в том случае, если произведена реструктуризация
        долга заемщика по причине сложностей с его возвратом)
        8 взыскание оплаты залогом
        (огашение по кредиту произведено с использованием залога. начение
        ставится и в том случае, если часть кредита была погашена залогом)
        9 безнадёжный долг/ передано на взыскание/ пропущенный платеж
        (значение ставится в том случае, если кредит признан безнадежным)*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MannerOfPayment", vMannerOfPayment).

        /* DateContractTermination   ************************************************************************ */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateContractTermination", XmlDate(loan.end-date)).

        /* DatePaymentDue   ********************************************************************************* */
        /* 15. договорная дата финального платежа.может отличаться или совпадать с датой
        окончания срока договора.*/
        DEF VAR vBKIDatePayDue AS DATE NO-UNDO.
/*      vBKIDatePayDue = BKIDatePayDue( loan.contract, loan.cont-code, iDateRep). */
        vBKIDatePayDue = IF (iDateRep = loan.close-date) THEN loan.close-date ELSE loan.end-date.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DatePaymentDue", XmlDate(vBKIDatePayDue)).

        /* DateInterestPaymentDue   ************************************************************************* */
        /* договорная дата финальной выплаты процентов
           гр-к процентов бисом пересчитывается, поэтому берем максимум между платежами по долгу и по процентам
         */
/*      lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateInterestPaymentDue",
                XmlDate(MAXIMUM(vBKIDatePayDue, BKIDateInterPayDue(loan.contract, loan.cont-code, iDateRep)))). */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateInterestPaymentDue", XmlDate(vBKIDatePayDue)).

        /* DateAccountState   ******************************************************************************* */
        /* 17. присвоен ранее в AccountState */
        if (oAccRat = "13") THEN oDateAccRat = loan.close-date.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateAccountState", XmlDate(oDateAccRat)).

        /* NextPayment   ************************************************************************************ */
        /* 18. TR(14) сумма следующего очередного платежа */
        DEF VAR vNextPayment AS DEC NO-UNDO.
        DEF VAR vAmountOutstandingNoExtra AS DEC NO-UNDO.
        DEF VAR vAmountOutstanding AS DEC NO-UNDO.
        vAmountOutstandingNoExtra =  ListGetSummByIntVar(BUFFER loan, iDateRep, "ТекОД,ЗадолжОД").
        vAmountOutstanding = IF (iDateRep >= loan.close-date) THEN 0.0 ELSE (vAmountOutstandingNoExtra
                           + ListGetSummByIntVar(BUFFER loan, iDateRep, "Текпроц,ЗадолжПР")
                           + /*IF iDateRep EQ loan.since THEN*/
                             CREListGetSummByIntVar(BUFFER loan, iDateRep, "ПеняОД,ПеняПР")
                             /*ELSE 0*/ ).
        vNextPayment = MINIMUM( vAmountOutstanding, BKINextPayment(loan.contract, loan.cont-code, iDateRep)).
        /* Если договор продан */
        IF (cSost = '14') AND (iDateRep >= dSost) THEN vNextPayment = 0.0.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("NextPayment", XmlDec(IF (oAccRat = "14") THEN 0 ELSE vNextPayment)).

        /* PaymentFrequency   ******************************************************************************* */
        /* 19. TR(15) частота выплат по основному долгу
        (справочник частоты выплат). при экспорте в Equifax 3.0 это обязательное поле.*/
        vPaymentFrequency = BKICredPayFreq( loan.contract, loan.cont-code, iDateRep).
        if (vPaymentFrequency NE ? AND vPaymentFrequency NE '')
        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("PaymentFrequency", vPaymentFrequency).

        /* Collateral   ************************************************************************************* */
        /* 20. TR(18) код залога по кредиту (справочник код залога) */
        DEF VAR vCollateral AS CHAR INIT '0' NO-UNDO.
        FOR EACH signs
            WHERE signs.file-name = 'term-obl'
              AND signs.surrogate BEGINS vLoanSurr + ',5'
              AND signs.code = 'виддогоб'
              AND (signs.xattr-value = 'КредЦБум' OR signs.xattr-value = 'КредОб')
            NO-LOCK:

            IF signs.xattr-value = 'КредЦБум'
            THEN vCollateral = '20'.
            ELSE vCollateral = '01'.
            LEAVE.
        END.
        /*
        DEF VAR vCollateral AS CHAR NO-UNDO.
        vCollateral = "".
        FOR EACH TERM-OBL
        WHERE TERM-OBL.CONTRACT = LOAN.CONTRACT
        AND TERM-OBL.CONT-CODE = LOAN.CONT-CODE
        AND TERM-OBL.IDNT = 5
        AND TERM-OBL.CLASS-CODE = "term-obl-gar"
        AND TERM-OBL.FOP-DATE <= iDateRep
        AND TERM-OBL.END-DATE >= iDateRep
        NO-LOCK:
            IF GetXattrValue( "term-obl",
            GetSurrogate("term-obl",ROWID(term-obl)),
                "ВидДогОб") NE "КредОб"
                    THEN NEXT.
                    IF vCollateral EQ "" THEN
            vCollateral = GetCodeZal( BUFFER TERM-OBL, iDateRep).
            ELSE vCollateral = "20".
        END.
        IF vCollateral NE "" THEN
        */
        IF vCollateral NE '0'
        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("Collateral", vCollateral).

        /* InterestPaymentFrequency   *********************************************************************** * /
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("InterestPaymentFrequency", ""). */

        /* OldAccount   ************************************************************************************* */
        /* 23. TR(24) старый номер счета */
        IF (oldAcc <> "") THEN
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldAccount", oldAcc).
/*
        IF CAN-DO("0300,0400", loan.filial-id)
        THEN DO:
            IF (oldAcc <> "") THEN
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldAccount", oldAcc).
        END.
        ELSE DO:
            oldAcc = loan.cont-code.
            IF loan.open-date <= DATE(12,31,2013)    THEN oldAcc = Replace(oldAcc,'@0000','@0400').
            IF loan.open-date >  DATE(12,31,2013) AND
               loan.open-date <  DATE(05,01,2015)    THEN oldAcc = Replace(oldAcc,'@0000','/0400').
            IF loan.open-date >  DATE(05,01,2015)    THEN oldAcc = Replace(oldAcc,'@0000','').
            IF loan.open-date <  DATE(11,01,2016)    THEN oldAcc = Replace(oldAcc,'@0500','').
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldAccount", oldAcc).
        END.
*/
        /* AmountOutstanding   ****************************************************************************** */
        /* 24. TR(25) текущая задолженность, вычислена в NextPayment */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("AmountOutstanding", XmlDec(IF (oAccRat = "14") THEN 0 ELSE vAmountOutstanding)).

        /* Флаги разрешения на дальнейший экспорт в БКИ  ************************************************************** */
        /* - CanExport - CanExportNBCH - CanExportGPCS - CanExportExperian - CanExportBrs - Updated  ******** */

        /* Эквифакс v2.X  69 - 77     ********************************************************************************* */
        /* - EndDateFact     ******************************************************************************** */
        /* LastPayment  ************************************************************************************* */
        /* 70. op_cred_sum_payout сумма последнего факт погаш од */
        DEF VAR oAmt     AS DECIMAL    NO-UNDO.
        DEF VAR vAmtCur  AS DECIMAL    NO-UNDO.
        DEF VAR vAmtDb   AS DECIMAL    NO-UNDO.
        DEF VAR vAmtCr   AS DECIMAL    NO-UNDO.
        DEF VAR vI       AS INT64  NO-UNDO.
        DEF VAR vJ       AS INT64  NO-UNDO.
        DEF BUFFER bcode FOR code.
        DEF BUFFER bloan-int FOR loan-int.
        DEF BUFFER bloan FOR loan.

        IF NOT GetCodeBuff("НБКИ_КодОпер", "ПогашОД", BUFFER bcode) THEN
             UNDO, THROW NEW Progress.Lang.AppError( "в классификаторе нбки_кодопер").
        /* список операций, которые нужно сложить */
        oAmt = 0.
        DEF VAR vdlp AS DATE NO-UNDO.
        vdlp = ?. /*vDateLastPayment.*/
        RUN GetDateByIntVar(loan.contract, loan.cont-code, iDateRep, "ПогашОД", OUTPUT vdlp).
        IF bcode.misc[1] > "" AND vdlp NE ? THEN
        DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
            RUN GetSummUnv_(loan.contract, loan.cont-code,
                    INT64(ENTRY(vI,bcode.misc[1])),
                    vdlp, "GetLastSummOper", OUTPUT vAmtCur, OUTPUT vAmtDb, OUTPUT vAmtCr).
        oAmt = oAmt + vAmtCur.
        END.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LastPayment", XmlDec(oAmt)).

        /* - Comment  *************************************************************************************** */

        /* CurLimit  **************************************************************************************** */
        /* 72 поле .cred_sum_limit.. текущий неиспользованный лимит.
           заполняется для кредитов в форме кредитной линии и овердрафт,
           если выданный кредит предполагает выдачу */
        RUN GetSummByIntVar(BUFFER loan,
        iDateRep, "НеиспЛим", OUTPUT oAmt).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CurLimit", XmlDec(oAmt)).

        /* PastDueNoExtra  ********************************************************************************** */
        /* 73. cумма просрочки по основному долгу. */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("PastDueNoExtra", XmlDec(IF (oAccRat = "14") THEN 0 ELSE vPastDueNoExtra)).

        /* - NextInterPaymentDate  ************************************************************************** */
        /* - TotalSumFact  ********************************************************************************** */

        /* AmountOutstandingNoExtra  ************************************************************************ */
        /* 76. текущая задолженность по основному долгу (од + пр.од) */
        /* рассчитана в NextPayment */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("AmountOutstandingNoExtra", XmlDec(IF (oAccRat = "14") THEN 0 ELSE vAmountOutstandingNoExtra)).

        /* DateLastPaymentNoExtra  ************************************************************************** */
        /* 77. дата последней выплаты по основному долгу */
        DEF VAR vDateLastPaymentNoExtra AS DATE NO-UNDO.
        RUN GetDateByIntVar(loan.contract, loan.cont-code, iDateRep, "ПогашОд", OUTPUT vDateLastPaymentNoExtra).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("DateLastPaymentNoExtra", XmlDate( vDateLastPaymentNoExtra)).

        /* Эквифакс v3.X  78 - 92     ********************************************************************************* */
/*
        /* InsuredFlag  ************************************************************************************* */
        /* 40. флаг страховки кредита */
        DEF BUFFER iloan FOR loan.
        FIND FIRST iloan
            WHERE iloan.parent-contract     = loan.contract
              AND iloan.parent-cont-code    = loan.cont-code
              AND iloan.class-code          = "insurance"
              AND iloan.open-date           <= iDateRep
              AND iloan.end-date            >= iDateRep
              AND (iloan.close-date         = ?
                OR iloan.close-date         >= iDateRep)
            NO-ERROR.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("InsuredFlag",(IF AVAIL iloan THEN "1" ELSE "0")).

        /* InsuredAmount  *********************************************************************************** */
        /* сумма страховки кредита */
        IF AVAIL iloan
        THEN DO:
            DEF BUFFER iterm-obl FOR term-obl.
            FIND FIRST iterm-obl
                WHERE iterm-obl.idnt EQ 1
                  AND iterm-obl.cont-code EQ iloan.cont-code
                  AND iterm-obl.contract EQ 'СТРАХ'
                NO-ERROR.
            IF AVAIL(iterm-obl) THEN
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("InsuredAmount", XmlDec(iterm-obl.amt-rub)).
        END.
*/
        /* MissedPaymentDate  ******************************************************************************* */
        /* 82. MissedPaymentDate
           дата последнего пропущенного платежа. в случае
           отсутствия хотя бы одного пропущенного платежа
           заполняется датой .01.02.1900. */
        DEF VAR vListOper  AS CHAR NO-UNDO INIT "ЗадолжОД,ЗадолжПР".
        DEF VAR vListP     AS CHAR NO-UNDO INIT "7,10,48".
        DEF VAR vDateP     AS DATE NO-UNDO.
        DEF BUFFER xloan-int FOR loan-int.
        /*
        IF vListP EQ ? THEN DO ON ERROR UNDO,THROW:
        vListP = "".
        * все эти параметры активные *
        DO vJ = 1 TO NUM-ENTRIES( vListOper):
            IF NOT GetCodeBuff("НБКИ_КодОпер", ENTRY(vJ, vListOper), BUFFER bcode) THEN
             UNDO, THROW NEW Progress.Lang.AppError( " в классификаторе НБКИ_КодОпер").
            DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
            vListP = vListP + (IF LENGTH(vListP)>0 THEN "," ELSE "") + ENTRY(vI, bcode.misc[3]).
            END.
        END.
        END.*/
        vDateP = DATE(2,1,1900).

        /* по основному договору */
        FOR EACH bloan-int
            WHERE (bloan-int.contract   EQ loan.contract
              AND bloan-int.cont-code   EQ loan.cont-code
              AND bloan-int.mdate       LE iDateRep)
              AND CAN-DO(vListP, STRING(bloan-int.id-d))
            NO-LOCK BY bloan-int.mdate DESC:

            /* есть зачисление просроч/%,
               но ведь это мог быть перенос с в.б. на баланс */
            IF      bloan-int.id-d = 10
                AND bloan-int.id-k <> 33
            THEN NEXT.
            vDateP = bloan-int.mdate.
            LEAVE.
        END.

        /* теперь транши */
        FOR EACH bloan-int
            WHERE (bloan-int.contract   EQ loan.contract
              AND bloan-int.cont-code   BEGINS (loan.cont-code + " ")
              AND bloan-int.mdate       LE iDateRep)
              AND CAN-DO(vListP, STRING(bloan-int.id-d))
            NO-LOCK BY bloan-int.mdate DESC:

            /* есть зачисление просроч/%,
               но ведь это мог быть перенос с в.б. на баланс */
            IF      bloan-int.id-d = 10
                AND bloan-int.id-k <> 33
            THEN NEXT.
            ASSIGN
                vDateP = bloan-int.mdate WHEN vDateP < bloan-int.mdate.
            LEAVE.
        END.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MissedPaymentDate", XmlDate( vDateP)).

        /* BalanceNoExtra  ********************************************************************************** */
        /* 83. баланс по основному долгу. Общая сумма, заплаченная заемщиком по
           кредиту по основному долгу.*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("BalanceNoExtra", XmlDec(ListGetSummByIntVar(BUFFER loan, iDateRep, "погашод"))).

        /* CurrentOverdueNoExtraDate  *********************************************************************** */
        /* 84. дата возникновения просрочки по основному долгу.
           если текущая просрочка по основному долгу отсутствует,
           заполняется датой 01.02.1900. */
        /* а надо еще учитывать транши !!!*/
        DEF VAR vCurrentOverdueNoExtraDate AS DATE NO-UNDO.
        DEF VAR vPrsDolgDays AS INT NO-UNDO.

        vPrsDolgDays = 0.
        IF vPastDueNoExtra > 0
        THEN DO:
            FOR EACH transh
                WHERE (transh.contract      = loan.contract)
                  AND (transh.cont-code     BEGINS loan.cont-code)
                  AND (transh.filial-id     = loan.filial-id)
                  AND (transh.close-date    = ?
                    OR transh.close-date    <= iDateRep)
                NO-LOCK:

                iTmp = LN_GetPrsDolgDays (transh.contract, transh.cont-code, iDateRep).
                vPrsDolgDays = MAXIMUM(vPrsDolgDays, iTmp).
            END.
/*          vPrsDolgDays = LN_GetPrsDolgDays (loan.contract, loan.cont-code, iDateRep). */
            vCurrentOverdueNoExtraDate = iDateRep - vPrsDolgDays.
        END.
        ELSE vCurrentOverdueNoExtraDate = DATE(2,1,1900).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CurrentOverdueNoExtraDate", XmlDate( vCurrentOverdueNoExtraDate)).

        /* NextPaymentNoExtra      ************************************************************************** */
        /* NextPaymentNoExtraDate  ************************************************************************** */
        /* 85,86. следующий платеж по основному долгу */
        /* Определяем сумму и дату с учетом траншей */
        DEF VAR vNextPaymentNoExtra AS DEC NO-UNDO.
        DEF VAR vNextPaymentNoExtraDate AS DATE NO-UNDO.
        /*DEF BUFFER bloan FOR loan.*/
        DEF VAR dr AS DATE NO-UNDO.
        dr = iDateRep.
        DO WHILE TRUE
            ON ERROR UNDO, THROW:

            RUN GetFirstAmtUnv (loan.contract, loan.cont-code, 3, dr, "BKINextPayment1",
                                OUTPUT vNextPaymentNoExtra, OUTPUT vNextPaymentNoExtraDate).
            IF      vNextPaymentNoExtraDate NE ?
                AND vNextPaymentNoExtra     EQ 0
            THEN dr = vNextPaymentNoExtraDate.
            ELSE LEAVE.
        END.

        IF FGetSetting("НБКИ","TR14","") EQ "Нет" THEN DO:
          vNextPaymentNoExtra = BKIAmountOutst(BUFFER loan, INPUT  vNextPaymentNoExtraDate).
        END.
        vNextPaymentNoExtra = MIN( vNextPaymentNoExtra, vAmountOutstandingNoExtra).
        IF vNextPaymentNoExtra <= 0 THEN vNextPaymentNoExtraDate = DATE(2,1,1900).
        /* Если договор продан */
        IF ((cSost = '14') AND (iDateRep >= dSost)) OR (iDateRep >= loan.close-date)
        THEN ASSIGN vNextPaymentNoExtra     = 0.0
                    vNextPaymentNoExtraDate = DATE(2,1,1900).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("NextPaymentNoExtra", XmlDec( vNextPaymentNoExtra)).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("NextPaymentNoExtraDate", XmlDate( vNextPaymentNoExtraDate)).

        /* TotalLastPayment      **************************************************************************** */
        /* 87. сумма фактического исполнения обязательств заемщика в
           неполном размере (последний платеж)
           сумма последнего платежа. в текущую сумму должны входить:
           основной долг, %, комиссии и др. в случае отсутствия платежей
           по договору заполняется .0.
           - это просто сумма последнего платежа который фактически
           платил заемщик.
        */
        IF NOT GetCodeBuff("нбки_кодопер", "погашобщ", BUFFER bcode) THEN
             UNDO, THROW NEW Progress.Lang.AppError( "в классификаторе нбки_кодопер").
        /* список операций, которые нужно сложить */
        oAmt = 0.
        IF bcode.misc[1] > "" THEN
        DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
            RUN GetSummUnv_(loan.contract, loan.cont-code, INT64(ENTRY(vI,bcode.misc[1])),
                    /*iDateRep */ vDateLastPayment, "GetLastSummOper", OUTPUT vAmtCur, OUTPUT vAmtDb, OUTPUT vAmtCr).
        oAmt = oAmt + vAmtCur.
        END.
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("TotalLastPayment", XmlDec( oAmt)).

        /* CurrentOverdueDate      ************************************************************************** */
        /* 88. дата возникновения текущей просрочки. если
           текущая просрочка отсутствует, заполняется датой .01.02.1900..*/
        DEF VAR vCurrentOverdueDate AS DATE NO-UNDO.
        DEF VAR vPrsDays            AS INT  NO-UNDO.

        IF vPastDue > 0 THEN DO:
            vPrsDays = vPrsDolgDays.
            IF (vPastDuePrs > 0)
            THEN DO:
                FOR EACH transh
                    WHERE (transh.contract      = loan.contract)
                      AND (transh.cont-code     BEGINS loan.cont-code)
                      AND (transh.filial-id     = loan.filial-id)
                      AND (transh.close-date    = ?
                        OR transh.close-date    <= iDateRep)
                    NO-LOCK:

                    iTmp     = LN_GetPrsProcDays48 (transh.contract, transh.cont-code, iDateRep).
                    vPrsDays = MAX(vPrsDays, iTmp).
                END.
            END.
            vCurrentOverdueDate = iDateRep - vPrsDays.
        END.
        ELSE vCurrentOverdueDate = DATE(2,1,1900).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CurrentOverdueDate", XmlDate(vCurrentOverdueDate)).

        /* NextPaymentDate      ***************************************************************************** */
        /* 89. поле "ta_cred_date_nextpayout". дата следующего платежа.
           в случае отсутствия следующего платежа (к примеру, текущая
           транзакция, закрывает кредит) заполняется датой. 01.02.1900.*/
        DEF VAR vNextPaymentNoExtraPrc AS DEC NO-UNDO.
        DEF VAR vNextPaymentNoExtraDatePrc AS DATE NO-UNDO.
        DEF VAR vDate2 AS DATE NO-UNDO.
        RUN GetFirstAmtUnv (loan.contract, loan.cont-code, 1, iDateRep, "BKINextPayment1",
                            OUTPUT vNextPaymentNoExtraPrc, OUTPUT vNextPaymentNoExtraDatePrc).
        vDate2 = vNextPaymentNoExtraDate.
        IF vDate2 EQ ? OR vNextPaymentNoExtraDatePrc < vNextPaymentNoExtraDate
         THEN vDate2 = vNextPaymentNoExtraDatePrc.
        IF vNextPayment <= 0.0 THEN vDate2 = DATE(2,1,1900).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("NextPaymentDate", XmlDate( vDate2)).

        /* - CredFacility  ********************************************************************************** */

        /* ОКБ  93 - 103     ****************************************************************************************** */
        DEF VAR vPLDealID as CHAR NO-UNDO.
        vPLDealID = GetXattrValue( "loan", vLoanSurr, "PLDealID").

        IF (vPLDealID <> "")     /* Если была заявка, то параметры 93-99 для ОКБ не выгружаем. Только FinanceType="05" */
        THEN DO:
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("FinanceType", "05").   /* Из PipeLine только автомобили */
        END.
        ELSE DO:
            /* PaymentLimit  ************************************************************************************ */
            /* 94 Лимит Кредитования */
            IF loan.cont-type = 'Течение' THEN DO:
                FIND LAST term-obl
                    WHERE term-obl.contract = loan.contract
                      AND term-obl.cont-code = loan.cont-code
                      AND term-obl.idnt = 2
                      AND term-obl.nn = 0
                      AND term-obl.end-date >= loan.open-date
                      AND term-obl.end-date <= iDateRep
                    NO-LOCK NO-ERROR.
                IF AVAIL term-obl THEN DO:
                     lOK = hSAXWriter:WRITE-DATA-ELEMENT("PaymentLimit", XmlDec(term-obl.amt-rub)).
                END.
            END.

            /* FinanceType  ************************************************************************************* */
            /* 95 тип финансирования */
            DEF VAR vPF as CHAR NO-UNDO.
            DEF VAR vFinanceType AS CHAR NO-UNDO.
            vPF          = GetXattrValue("loan", vLoanSurr, "ЦельКред").
            vFinanceType = GetXAttrValue("loan", vLoanSurr, "FinanceType").

            IF (vFinanceType = "")
            THEN DO:
                CASE vPF:
                    WHEN "Автомобиль" THEN vFinanceType = '05'.
                    WHEN "Недв"       THEN vFinanceType = '03'.
                    OTHERWISE DO:
                        IF (vCollateral <> '0') AND (iCustCat = "Ю")
                        THEN vFinanceType = '01'.
                        ELSE vFinanceType = '02'.
                    END.
                END CASE.

                IF (oAccType = "6")   THEN vFinanceType = '03'.
                IF (oAccType = "1")   THEN vFinanceType = '05'.

                FIND FIRST loan-acct OF loan
                    WHERE CAN-DO("кредлин,КредН", loan-acct.acct-type)
                    NO-LOCK NO-ERROR.
                IF AVAIL loan-acct    THEN vFinanceType = '21'.
            END.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("FinanceType", vFinanceType).

            /* PurposeOfFinance  ******************************************************************************** */
            /* 96 Поле Purpose Of Finance (Цель кредитования).  */
            DEF VAR vPurposeOfFinance as CHAR NO-UNDO.
            DEF VAR mySurrTerm        AS CHAR NO-UNDO.
            DEF VAR tmpYear           AS INT  NO-UNDO.
            DEF VAR lNewAvto          AS LOG  NO-UNDO.

            vPurposeOfFinance = GetXAttrValue("loan", vLoanSurr, "PurposeOfFinance").
            IF (vPurposeOfFinance = "")
            THEN DO:
                CASE vPF:
                    WHEN "Омск-ремонт жилья" THEN vPurposeOfFinance = '07'.
                    WHEN "Недв"              THEN vPurposeOfFinance = '32'.
                    WHEN "РВ"                THEN vPurposeOfFinance = '13'.
                    WHEN "1"   OR WHEN "О"   THEN vPurposeOfFinance = '29'.
                    WHEN "4,1" OR WHEN "С"   THEN vPurposeOfFinance = '33'.
                END CASE.

                IF (vPurposeOfFinance = "")
                THEN DO:
                    vPurposeOfFinance =   '98'.
                    IF CAN-DO('*-АП*,*-ДП*,*-ГА*,*-АК*,*-МП*',loan.doc-ref)
                    THEN DO:
                        lNewAvto = NO.
                        FOR EACH term-obl
                            WHERE term-obl.cont-code    EQ loan.cont-code
                              AND term-obl.contract     EQ 'Кредит'
                              AND term-obl.idnt         EQ 5
                            NO-LOCK:

                            mySurrTerm = 'Кредит,' + loan.cont-code + ',5,' + string(term-obl.end-date,"99/99/99") + ',' + string(term-obl.nn).
                            IF (GetXAttrValue('term-obl', mySurrTerm, 'видоб') = 'Автомобиль')
                            THEN DO:
                                strtmp = GetXAttrValue('term-obl', mySurrTerm, 'TCis-new').
                                IF (strtmp = '')
                                THEN DO:
                                    strtmp  = GetXAttrValue('term-obl', mySurrTerm, 'TCyear').
                                    tmpYear = YEAR(loan.open-date) - INT(strtmp).
                                    IF (tmpYear <= 1)   THEN lNewAvto = YES.
                                END.
                                ELSE IF (strtmp = 'Да') THEN lNewAvto = YES.
                            END.
                        END.
                        vPurposeOfFinance  = IF lNewAvto THEN '01' ELSE '02'.
                    END.
                    /* dLimit - лимит кредитования */
                    IF CAN-DO('*-ИГ,*-ИС,*-ИГ-Д,*-И-ГП,*-ИГ-МКС',loan.doc-ref)
                    THEN vPurposeOfFinance  = '32'.                
                END.
            END.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurposeOfFinance", vPurposeOfFinance).

            /* AccPaymentStatus  ******************************************************************************** */
            /* 97 Поле Account Payment Status(Статус выплат по счету).  */
            DEF VAR vAccPaymentStatus as CHAR NO-UNDO.
            vAccPaymentStatus = '0'.
            IF vPrsDays >   0 AND vPrsDays <  31 THEN vAccPaymentStatus = 'A'.
            IF vPrsDays >  30 AND vPrsDays <  61 THEN vAccPaymentStatus = '1'.
            IF vPrsDays >  60 AND vPrsDays <  91 THEN vAccPaymentStatus = '2'.
            IF vPrsDays >  90 AND vPrsDays < 121 THEN vAccPaymentStatus = '3'.
            IF vPrsDays > 120 AND vPrsDays < 151 THEN vAccPaymentStatus = '4'.
            IF vPrsDays > 150 AND vPrsDays < 181 THEN vAccPaymentStatus = '5'.
            IF vPrsDays > 180                    THEN vAccPaymentStatus = '6'.

            vAccPaymentStatus = BKIMOP2( loan.contract, loan.cont-code, iDateRep).
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("AccPaymentStatus", vAccPaymentStatus).

            /* InterestRate  ************************************************************************************ */
            /* 98 Поле Interest rate (Процентная ставка, числовое значение годовой процентной ставки, указанной в договоре)  */
            DEF VAR vInterestRate as DECIMAL NO-UNDO.
            vInterestRate = 0.
            FIND LAST comm-rate
                WHERE comm-rate.kau = vLoanSurr
                  AND comm-rate.commission = '%Кред'
                  AND comm-rate.since <= iDateRep
                NO-LOCK NO-ERROR.
            IF AVAIL comm-rate THEN vInterestRate = comm-rate.rate-comm.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("InterestRate", XmlDec(vInterestRate)).
        END.    /* IF (vPLDealID <> "") */

        /* ReasonForClosure  ******************************************************************************** */
        /* 99 Поле Reason for closure (Причина закрытия). */
        DEF VAR vReasonForClosure AS CHAR NO-UNDO.
        IF (loan.close-date <> ?) AND (loan.close-date <= iDateRep)
        THEN DO:
            vReasonForClosure = GetXAttrValue("loan", vLoanSurr, "ReasonForClosure").
            IF (vReasonForClosure = "")
            THEN DO:
                IF (oAccRat = '14') AND (iDateRep >= dSost)
                THEN vReasonForClosure = '19'.
                ELSE vReasonForClosure = IF (loan.close-date < loan.end-date) THEN "17" ELSE "98".
            END.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReasonForClosure", vReasonForClosure).
        END.

        /* - StatusNBCH - StatusGPCS - StatusEI - StatusBRS  ********************************************************** */

        /* ApplicationNumber   ****************************************************************************** */
        /* 25 Уникальный номер кредитной заявки Обязательно для ФЛ
          (для осуществления связки с секцией InfoPart)
          ЭКС: /fch/info/credit/applicationid */
        IF NOT bGuar THEN DO:
            strtmp    = vPLDealID.
            IF (strtmp = "") THEN
            strtmp = GetXattrValue( "loan", vLoanSurr, "НБКИ_НомерЗаявки").
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("ApplicationNumber", strtmp).
        END.

        /* ApplicationDate     ****************************************************************************** */
        /* 26 Дата и время кредитной заявки Обязательно для ФЛ ЭКС: /fch/info/credit/applicationdate */
        DEF VAR vAppDate as CHAR NO-UNDO.
        vAppDate = GetXattrValue( "loan", vLoanSurr, "PLDealDate").
        IF (vAppDate = "")
        THEN DO:
            vAppDate = GetXattrValue( "loan", vLoanSurr, "НБКИ_ДатаЗаявки").
            IF (vAppDate <> "")
            THEN vAppDate = vAppDate + " 09:00:00".
        END.

        IF (vAppDate <> '') AND NOT bGuar THEN DO:
            /* в ДР PLDealDate выгружается datetime (DD/MM/YYYY HH:MM:SS.MMM),
               выгружаем в CreReg в нужном формате (DD.MM.YYYY HH:MM:SS)      */
        /*    vAppDate = REPLACE(ENTRY(1,vAppDate,'.'),'/','.'). */

            IF length(vAppDate) > 19 then vAppDate = substring(vAppDate,1,19).
            vAppDate = REPLACE(vAppDate, '/', '.').
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("ApplicationDate", vAppDate).
        END.

        /* TwoConsPayMissed     ****************************************************************************** */
        /* 27. Информация об отсутствии двух и более подряд платежей в течение 120 календарных дней
               с даты наступления срока исполнения обязательства по договору займа (кредита)
               Код Информация об отсутствии двух и более подряд платежей в течение 120 календарных дней с даты наступления срока исполнения обязательства
               0   Не было двух подряд пропущенных платежей в течение 120 календарных дней с даты наступления срока выплаты первого платежа
               1   Было два подряд пропущенных платежа в течение 120 календарных дней с даты наступления срока выплаты первого платежа
               2   Время исполнения обязательств по договору займа (кредита) еще не наступило
               3   Неизвестно
           Обязательно для ФЛ.
        */
        DEF VAR iTwoConsPayMissed   AS CHAR NO-UNDO.
        DEF VAR i                   AS INT  NO-UNDO.
        DEF VAR dStartDate          AS DATE NO-UNDO.
        DEF VAR dprdate             AS DATE NO-UNDO. /* Дата предыдущего выноса на просрочку */
        DEF BUFFER bufLoan-acct     FOR loan-acct.

        iTwoConsPayMissed = "".
        dStartDate = ?.
        i = 0.

        FOR EACH term-obl
            WHERE (term-obl.contract        = loan.contract)
              AND (term-obl.cont-code  BEGINS loan.cont-code)
              AND (term-obl.idnt            = 1
                OR term-obl.idnt            = 3)
              AND (term-obl.amt-rub        <> 0.0)
/*            AND term-obl.nn               = 1
*/          NO-LOCK
            BY term-obl.end-date:

            dStartDate = term-obl.end-date.
            LEAVE.
        END.

        IF dStartDate <> ?
        THEN DO:
            IF dStartDate <= iDateRep
            THEN DO:
                iTwoConsPayMissed = IF (dStartDate + 120 <= iDateRep) THEN "0" ELSE "".
                dprdate           = loan.open-date - 90.

/*              FIND FIRST loan-acct
                    WHERE loan-acct.contract  = loan.contract
                      AND loan-acct.cont-code = loan.cont-code
                      AND loan-acct.acct-type = 'КредПр%'
                    NO-LOCK NO-ERROR.
                IF AVAIL loan-acct
                THEN DO:
                    FIND FIRST bufLoan-acct
                        WHERE bufLoan-acct.contract  = loan.contract
                          AND bufLoan-acct.cont-code = loan.cont-code
                          AND bufLoan-acct.acct-type = 'КредТ'
                        NO-LOCK NO-ERROR.
                    IF AVAIL bufLoan-acct
                    THEN DO:
                        FOR EACH op-entry
                            WHERE op-entry.acct-db   = loan-acct.acct
                              AND op-entry.acct-cr   = bufLoan-acct.acct
                              AND op-entry.op-status >= "√"
                              AND op-entry.op-date   >= dStartDate
                              AND op-entry.op-date   <= (dStartDate + 120)
                            NO-LOCK
                            BY op-entry.op-date:

                            IF (op-entry.op-date - dprdate < 63)
                            THEN DO:
                                iTwoConsPayMissed = "1".
                                LEAVE.
                            END.
                            ELSE dprdate = op-entry.op-date.
                        END.
                    END.
                END.
*/
                FOR EACH loan-int
                    WHERE (loan-int.contract       = loan.contract)
                      AND (loan-int.cont-code BEGINS loan.cont-code)
                      AND (loan-int.mdate         >= dStartDate)
                      AND (loan-int.mdate         <= dStartDate + 120)
                    NO-LOCK,
                FIRST chowhe
                    WHERE (chowhe.id-d             = loan-int.id-d)
                      AND (chowhe.id-k             = loan-int.id-k)
                      AND CAN-DO("2,45,98,99,304,577,579,581", STRING(chowhe.id-op))
                    NO-LOCK
                    BY loan-int.mdate:

                    IF      (loan-int.mdate - dprdate < 63)
                        AND (loan-int.mdate - dprdate > 10)
                    THEN DO:
                        iTwoConsPayMissed = "1".
                        LEAVE.
                    END.
                    ELSE dprdate = loan-int.mdate.
                END.
            END.
            ELSE iTwoConsPayMissed = "2".   /* Первая плановая дата еще не наступила */
        END. /* IF dStartDate <> ? */

        IF (iTwoConsPayMissed <> "") THEN
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("TwoConsPayMissed", iTwoConsPayMissed).

        /* FullyPaidOff     ********************************************************************************* */
        /* FullyPaidOffDate     ***************************************************************************** */
        /* 28, 29. 1 - Признак погашения кредита в полном объеме.  Обязательно для ФЛ, если кредит погашен в полном объеме.  */
        IF (oAccRat = '14') AND (iDateRep >= dSost) THEN
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("FullyPaidOff", '0').
        ELSE IF (loan.close-date <> ?) and (iDateRep >= loan.close-date) THEN DO:
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("FullyPaidOff", '1').
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("FullyPaidOffDate", XmlDate(oDateAccRat)).
        END.

        /* TotalCredCost     ******************************************************************************** */
        /* 30. TR(37) Полная стоимость кредита (Overall value of credit) с точностью до третьего знака после */
        DEF VAR vPSK as CHAR NO-UNDO.
        vPSK = GetXattrValue( "loan", vLoanSurr, "ПСК").
        IF (vPSK = "")
        THEN vPSK = GetXattrValue( "loan", vLoanSurr, "ЭПС").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("TotalCredCost", vPSK).

        /* - AllowInInfoPart   ****************************************************************************** */

        /* Contract     ************************************************************************************* */
        /* 32. Номер договора займа, ОКБ: Legal Account Number */
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Contract", loan.doc-ref).

        /* GuaranteeIndicator     *************************************************************************** */
        /* 33. TR(26) Флаг о наличии поручителя (Guarantor indicator) Обязателен по заемщику.
           По поручителю и принципалу игнорируется (в бюро не передается) */
        DEF VAR iGuarant    AS INT64 NO-UNDO.
        DEF VAR strSurrTerm AS CHAR NO-UNDO.
        DEF VAR datePoruch  AS DATE NO-UNDO. /* дата, с которой по кредиту платит поручитель, а не заемщик */

        datePoruch = DATE(GetXattrValue( "loan", vLoanSurr, "datePoruch")).
/* Новый алгоритм */
        IF NOT bGuar    /* Выгружается сам кредитный договор (не поручительство) */
        THEN DO:
            /* Заносим поручительства в таблицу ttGuarant для последующей выгрузки */
            iGuarant = 0.

            FOR EACH signs
                WHERE signs.file-name   = 'term-obl'
                  AND signs.surrogate   BEGINS vLoanSurr + ',5'
                  AND signs.code        = 'видоб'
                  AND signs.xattr-value = 'Поручит'
                NO-LOCK:

                IF iGuarant = 0
                THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeIndicator", '1').
                iGuarant = 1.

                strSurrTerm = signs.surrogate.
                FOR FIRST term-obl
                    WHERE term-obl.contract     =      ENTRY(1,strSurrTerm)
                      AND term-obl.cont-code    =      ENTRY(2,strSurrTerm)
                      AND term-obl.idnt         = INT (ENTRY(3,strSurrTerm))
                      AND term-obl.end-date     = DATE(ENTRY(4,strSurrTerm))
                      AND term-obl.nn           = INT (ENTRY(5,strSurrTerm))
                    NO-LOCK:

                    strTmp   = GetXAttrValue("term-obl", strSurrTerm, "НомДогОб").  /* Номер договора поручительства */
                    strXattr = GetXattrValue( "loan", vLoanSurr, "UnlGuar").        /* Список выгруженных поручительств */
                    IF     (LOOKUP(strTmp, strXattr, "|") = 0)                      /* Это поручительство не выгружалось */
                    THEN DO:
                        CREATE ttGuarant.
                        ASSIGN
                            ttGuarant.r-loan    = ROWID(loan)
                            ttGuarant.r-term    = ROWID(term-obl)
                            ttGuarant.cont-code = term-obl.cont-code
                            ttGuarant.cust-id   = term-obl.fop
                            ttGuarant.cust-cat  = term-obl.symbol
                            ttGuarant.loanUpd   = MAXIMUM(vDateOpened, term-obl.fop-date)
                            ttGuarant.recStatus = 0
                            .
                        strXattr = strXattr + (IF (strXattr = "") THEN "" ELSE "|") + strTmp.
                        UpdateSigns("loan", vLoanSurr, "UnlGuar", strXattr, NO).
                    END.

                    IF     (iDateRep = loan.close-date)     /* выгрузка поручительства на дату закрытия договора */
                        OR (oAccRat  = "14")
                    THEN DO:
                        CREATE ttGuarant.
                        ASSIGN
                            ttGuarant.r-loan    = ROWID(loan)
                            ttGuarant.r-term    = ROWID(term-obl)
                            ttGuarant.cont-code = term-obl.cont-code
                            ttGuarant.cust-id   = term-obl.fop
                            ttGuarant.cust-cat  = term-obl.symbol
                            ttGuarant.loanUpd   = iDateRep
                            ttGuarant.recStatus = 0
                            .
                    END.
                END.
            END.    /* IF bGuar ... ELSE */

            IF iGuarant = 0
            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeIndicator", '0').
        END.

        /* - BankGuaranteeIndicator ************************************************************************* */

        IF (vPLDealID = "")     /* Если была заявка, то параметры 93-99 для ОКБ не выгружаем */
        THEN DO:
            /* Category  **************************************************************************************** */
            /* 93 Category   Поле CBR Finance Type (Тип кредита ЦБ). */
            DEF BUFFER bsigns FOR signs.
            DEF VAR myCategory AS CHAR NO-UNDO.

            myCategory = GetXAttrValue("loan", vLoanSurr, "Category").
            IF (myCategory = "")
            THEN DO:
                myCategory = '999'.
                IF CAN-DO('*-АП*,*-ДП*,*-ГА*,*-АК*,*-МП*',loan.doc-ref)
                THEN myCategory  = IF lNewAvto THEN '101' ELSE '102'.
                /* dLimit - лимит кредитования */
                IF CAN-DO('*-КП,*-КА,*-КС',loan.doc-ref) THEN DO:
                    IF dLimit <   30000                       THEN myCategory = '401'.
                    IF dLimit <  100000 AND dLimit >=  30000  THEN myCategory = '402'.
                    IF dLimit <  300000 AND dLimit >= 100000  THEN myCategory = '403'.
                    IF dLimit >= 300000                       THEN myCategory = '404'.
                    IF (loan.end-date - loan.open-date) > 365 THEN myCategory = STRING(INT(myCategory) + 4).
                END.
                IF CAN-DO('*-ОВ,*-КЛ',loan.doc-ref) THEN DO:
                    IF dLimit <   30000                       THEN myCategory = '201'.
                    IF dLimit <  100000 AND dLimit >=  30000  THEN myCategory = '202'.
                    IF dLimit <  300000 AND dLimit >= 100000  THEN myCategory = '203'.
                    IF dLimit >= 300000                       THEN myCategory = '204'.
                END.
                IF CAN-DO('*-ИГ,*-ИС,*-ИГ-Д,*-И-ГП,*-ИГ-МКС',loan.doc-ref)
                                                              THEN myCategory = '501'.
                IF CAN-DO('*-МБА',loan.doc-ref)               THEN myCategory = '601'.
            END.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("Category", myCategory).
        END.

        /* - CredWay  *************************************************************************************** */
        /* - ReqAmount  ************************************************************************************* */
        /* - CredActive  ************************************************************************************ */
        /* - CredSpecialStatus  ***************************************************************************** */

        /* Поручительства  36 - 44  *********************************************************************************** */
        IF bGuar    /* Выгружается поручительство */
        THEN DO:
            strSurrTerm = GetSurrogate('term-obl', rowid(bTerm)).
            /* GuaranteeNumber        *********************************************************************** */
            strTmp   = GetXAttrValue("term-obl", strSurrTerm, "НомДогОб"). /* Номер договора поручительства */
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeNumber", strTmp).

            /* - GuaranteeExecutedIndicator ***************************************************************** */
            /* - GuaranteeClaimDate   *********************************************************************** */

            /* GuaranteeVolume        *********************************************************************** */
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeVolume", IF (dLimit > bTerm.amt-rub) THEN "2" ELSE "1").
            /* GuaranteeSum           *********************************************************************** */
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeSum", XmlDec(bTerm.amt-rub)).
            /* GuaranteeSumCurrency   *********************************************************************** */
/*          IF (bTerm.currency <> '')
            THEN */ lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeSumCurrency", XmlCurr(bTerm.currency)).
            /* GuaranteeDate          *********************************************************************** */
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeDate", XmlDate(bTerm.fop-date)).
            /* GuaranteeEndDate       *********************************************************************** */
            IF (oAccRat = '13') AND NOT bGuar
            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeEndDate", XmlDate(loan.close-date)).
            ELSE lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeEndDate", XmlDate(bTerm.end-date)).
            /* - GuaranteeReasonForClosure  ***************************************************************** */
        END.

/* Старый алгоритм
        iGuarant = 0.
        IF vAccRelation <> '5'
        THEN DO:
            FOR EACH signs
                WHERE signs.file-name = 'term-obl'
                  AND signs.surrogate BEGINS vLoanSurr + ',5'
                  AND signs.code = 'видоб'
                  AND signs.xattr-value = 'Поручит'
                NO-LOCK:

                strSurrTerm = signs.surrogate.
                IF iGuarant = 0
                THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeIndicator", '1'). / * 28_12_2015 * /

                FIND FIRST bsigns
                    WHERE bsigns.file-name = 'term-obl'
                      AND bsigns.surrogate = strSurrTerm
                      AND bsigns.code      = 'номдогоб'
                    NO-LOCK NO-ERROR.
                IF AVAIL signs AND bGuar
                THEN DO:
                    lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeNumber", bsigns.xattr-value).
                END.

                FIND FIRST term-obl
                    WHERE term-obl.contract  =      ENTRY(1,strSurrTerm)
                      AND term-obl.cont-code =      ENTRY(2,strSurrTerm)
                      AND term-obl.idnt      = INT (ENTRY(3,strSurrTerm))
                      AND term-obl.end-date  = DATE(ENTRY(4,strSurrTerm))
                      AND term-obl.nn        = INT (ENTRY(5,strSurrTerm))
                    NO-LOCK NO-ERROR.
                IF AVAIL term-obl
                THEN DO:
                    IF NOT bGuar AND (dPoruc = ? OR (datePoruch <> ? AND datePoruch < iDateRep) )
                    THEN DO:
                        CREATE ttGuarant.
                        ASSIGN
                            ttGuarant.r-loan    = ROWID(loan)
                            ttGuarant.r-term    = ROWID(term-obl)
                            ttGuarant.cont-code = term-obl.cont-code
                            ttGuarant.cust-id   = term-obl.fop
                            ttGuarant.cust-cat  = term-obl.symbol
                            ttGuarant.loanUpd   = iDateRep
                            ttGuarant.recStatus = 0
                            .
                    END.

                    IF dLimit > term-obl.amt-rub
                    THEN iGuarant = 2.
                    ELSE iGuarant = 1.

                    IF bGuar
                    THEN DO:
                        lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeVolume", STRING(iGuarant)).
                        lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeSum", XmlDec(term-obl.amt-rub)).
                        IF term-obl.currency <> ''
                        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeSumCurrency", XmlCurr(term-obl.currency)).
                        lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeDate", XmlDate(term-obl.fop-date)).
                        lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeEndDate", XmlDate(term-obl.end-date)).
                    END.
                END.

                iGuarant = 1.
              / *   LEAVE. * /
            END.
        END.

        IF iGuarant = 0
        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("GuaranteeIndicator", '0').
*/
        /* Покупатель кредита  46 - 62   ****************************************************************************** */
        /* PurchaserBusinessName     ************************************************************************ */
        /* PurchaserOGRN             ************************************************************************ */
        /* PurchaserINN              ************************************************************************ */
        /* PurchaserSource           ************************************************************************ */
        /* PurchaserAssigneeDate     ************************************************************************ */
        DEF VAR myCust AS CHAR NO-UNDO.
        /* если кредит продан, выгружаем данные о покупателе кредита */
        IF (oAccRat = '14') AND (iDateRep >= dSost)
        THEN DO:
            myCust = GetXattrValue( "loan", vLoanSurr, "BankCust").
            IF myCust <> ""
            THEN DO:
                CASE SUBSTRING(myCust, 1, 1):
                    WHEN "Б" THEN DO:
                        FOR FIRST banks
                            NO-LOCK,
                        FIRST banks-code OF banks
                            WHERE (banks-code.bank-code-type = 'МФО-9')
                              AND (banks-code.bank-code      = SUBSTRING(myCust, 2))
                            NO-LOCK:

                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserBusinessName", banks.name).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserOGRN", GetXAttrValue("banks", STRING(banks.bank-id), "ОГРН")).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserINN", banks.inn).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserSource", GetXAttrValue("banks", STRING(banks.bank-id), "КодПодпОКБ")).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserAssigneeDate", XmlDate(dSost)).
                        END.
                    END.
                    WHEN "Ч" THEN DO:
                        
                    END.
                    OTHERWISE DO:   /* ЮЛ */
                        FOR FIRST cust-corp
                            WHERE cust-corp.cust-id = INT(myCust)
                            NO-LOCK:

                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserBusinessName", cust-corp.cust-stat + " " + cust-corp.name-corp).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserOGRN", GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "ОГРН")).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserINN", GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "inn")).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserSource", GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "КодПодпОКБ")).
                            lOK = hSAXWriter:WRITE-DATA-ELEMENT("PurchaserAssigneeDate", XmlDate(dSost)).
                        END.
                    END.
                END CASE.
            END.
        END.

        /* - TradeCollaterals     ?  ************************************************************************ */
        /* - Extra - ExtraDetail     ************************************************************************ */
        /* - TradeTransactions    ?  ************************************************************************ */

        /* Раздел "Залоги"   ****************************************************************************************** */
        IF (vAccRelation <> "5")    /* Для поручителей залоги не выгружаем */
        THEN DO:
            DEF VAR bCollaterals AS LOGICAL NO-UNDO.
            bCollaterals = FALSE.

            FOR EACH signs
                WHERE signs.file-name = 'term-obl'
                  AND signs.surrogate BEGINS vLoanSurr + ',5'
                  AND signs.code = 'виддогоб'
                  AND CAN-DO('КредЦБум,КредОб',signs.xattr-value)
                NO-LOCK,
            FIRST term-obl
                WHERE term-obl.contract  =      ENTRY(1,signs.surrogate)
                  AND term-obl.cont-code =      ENTRY(2,signs.surrogate)
                  AND term-obl.idnt      = INT (ENTRY(3,signs.surrogate))
                  AND term-obl.end-date  = DATE(ENTRY(4,signs.surrogate))
                  AND term-obl.nn        = INT (ENTRY(5,signs.surrogate))
                NO-LOCK:
    /*
                IF (term-obl.sop-date <> ?) AND (iDateRep > term-obl.sop-date) THEN NEXT.
    */
                strSurrTerm = signs.surrogate.
                IF NOT bCollaterals
                THEN DO:
                    lOK = hSAXWriter:START-ELEMENT("TradeCollaterals").
                    bCollaterals = TRUE.
                END.
                lOK = hSAXWriter:START-ELEMENT("TradeCollateral").

                /* - CollateralContract      ******************************************************************** */
                /* - CollateralDate          ******************************************************************** */
                /* - CollateralEndDate       ******************************************************************** */
                /* - CollateralType          ******************************************************************** */
                /* - CollateralAssessment    ******************************************************************** */
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "НомДогОб"). /* Номер договора поручительства */
                IF (strTmp <> "") THEN
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralContract",   strTmp).
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralDate",       XmlDate(term-obl.fop-date)).
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralEndDate",    XmlDate(term-obl.end-date)).
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "видоб").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralType",       GetCodeZal(strTmp)).
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralAssessment", XmlDec(term-obl.amt-rub)).

                /* - CollateralCurrency      ******************************************************************** */
                /* - CollateralAssessmentDate ******************************************************************* */
                IF term-obl.currency <> '' THEN
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralCurrency",   XmlCurr(term-obl.currency)).
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralAssessmentDate", XmlDate(term-obl.fop-date)).

                /* - CollateralPledgorName   ******************************************************************** */
                IF term-obl.symbol = 'Ч' THEN DO:
                    FIND FIRST person
                        WHERE person.person-id = term-obl.fop
                        NO-LOCK NO-ERROR.
                    IF AVAIL person THEN
                    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralPledgorName", person.name-last + ' ' + person.first-names).
                END.
                ELSE IF term-obl.symbol = 'Ю' THEN DO:
                    FIND FIRST cust-corp
                        WHERE cust-corp.cust-id = term-obl.fop
                        NO-LOCK NO-ERROR.
                    IF AVAIL cust-corp THEN
                    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralPledgorName", cust-corp.name-short).
                END.

                /* - CollateralID            ******************************************************************** */
                DEFINE VARIABLE cOKSym     AS CHARACTER    NO-UNDO INIT "0123456789ABCDEFGHJKLMNPRSTUVWXYZ" CASE-SENSITIVE.
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "TCVIN").
                IF (strTmp <> "") AND (strTmp <> 'отсутствует') AND (LENGTH(strTmp) = 17)
                THEN DO:
                    DO vI = 1 TO LENGTH(strTmp):
                        IF (INDEX(cOKSym, SUBSTRING(strTmp, vI, 1)) = 0)
                        THEN DO:
                            strTmp = "".    /* Если в ДР есть прочие символы, то отправляем пустой CollateralID */
                            LEAVE.
                        END.
                    END.
                END.
                ELSE strTmp = "".
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralID",     strTmp).

                /* - CollateralIssueDate     ******************************************************************** */
                /* - CollateralAddress       ******************************************************************** */
                IF CAN-DO("17,18,19", cSost)
                THEN DO:
                    strTmp = GetXAttrValue("term-obl", strSurrTerm, "РегОбесп").
                    IF (strTmp <> "") THEN
                    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralIssueDate",  strTmp).
                END.
                IF CAN-DO("11,12,13", cSost)
                THEN DO:
                    strTmp = GetXAttrValue("term-obl", strSurrTerm, "местонахождение").
                    IF (strTmp <> "") THEN
                    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralAddress",    strTmp).
                END.

                /* - CollateralDescription   ******************************************************************** */
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "описание").
                IF (strTmp <> "") THEN
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralDescription",SUBSTRING(strTmp,1,150)).

                /* - CollateralFactEndDate   ******************************************************************** */
                IF (loan.close-date <> ?) AND (iDateRep >= loan.close-date)         /* окончание договора */
                THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralFactEndDate",XmlDate(loan.close-date)).
                ELSE IF (term-obl.sop-date = ?)
                    THEN DO:                    /* нет даты выбытия => только в дату состояний 12, 14 */
                        IF CAN-DO("12,14", cSost) AND (iDateRep = dSost)
                        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralFactEndDate",XmlDate(dSost)).
                    END.
                    ELSE DO:                    
                        IF CAN-DO("12,14", cSost)
                        THEN DO:
                            IF (iDateRep = dSost)  /* либо в дату 12,14, либо от выбытия до 12,14 */
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralFactEndDate",XmlDate(dSost)).
                            ELSE IF (iDateRep >= term-obl.sop-date) AND (iDateRep < dSost)
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralFactEndDate",XmlDate(term-obl.sop-date)).
                        END.
                        ELSE                    /* есть дата выбытия и нет 12,14 => каждый раз после этой даты */
                            IF (iDateRep >= term-obl.sop-date)
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralFactEndDate",XmlDate(term-obl.sop-date)).
                    END.

                /* - CollateralInputDate     ******************************************************************** */
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "ДатаПост").
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralInputDate",
                        XmlDate(IF (strTmp = "") OR (DATE(strTmp) < term-obl.fop-date) THEN term-obl.fop-date ELSE DATE(strTmp))) NO-ERROR.

                /* - CollateralReasonForClosure ***************************************************************** */
                strTmp = GetXAttrValue("term-obl", strSurrTerm, "Reason4Closure").
                IF (loan.close-date <> ?) AND (iDateRep >= loan.close-date)         /* окончание договора */
                THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralReasonForClosure","2").
                ELSE IF (term-obl.sop-date = ?)
                    THEN DO:                    /* нет даты выбытия => только в дату состояний 12, 14 */
                        IF CAN-DO("12,14", cSost) AND (iDateRep = dSost)
                        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralReasonForClosure",IF (cSost = "12") THEN "1" ELSE "5").
                    END.
                    ELSE DO:                    
                        IF CAN-DO("12,14", cSost)
                        THEN DO:
                            IF (iDateRep = dSost)  /* либо в дату 12,14, либо от выбытия до 12,14 */
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralReasonForClosure",IF (cSost = "12") THEN "1" ELSE "5").
                            ELSE IF (iDateRep >= term-obl.sop-date) AND (iDateRep < dSost)
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralReasonForClosure",IF (strTmp = "") THEN "4" ELSE strTmp).
                        END.
                        ELSE                    /* есть дата выбытия и нет 12,14 => каждый раз после этой даты */
                            IF (iDateRep >= term-obl.sop-date)
                            THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("CollateralReasonForClosure",IF (strTmp = "") THEN "4" ELSE strTmp).
                    END.

                lOK = hSAXWriter:END-ELEMENT("TradeCollateral").
            END.

            IF bCollaterals
            THEN lOK = hSAXWriter:END-ELEMENT("TradeCollaterals").
        END.

        FINALLY:
            lOK = hSAXWriter:END-ELEMENT("Trade").
        END.
    END. /* DO ON ERROR UNDO, THROW: */
END PROCEDURE.

/* Дата закрытия совпадает с датой окончания с точностью до выходных */
FUNCTION CloseDateOK    RETURNS LOGICAL
   (INPUT  iDat1    AS DATE,    /* дата окончания */
    INPUT  iDat2    AS DATE ).  /* дата закрытия */

    DEFINE VARIABLE dTmp    AS DATE NO-UNDO.

    IF (iDat2 = ?) OR (iDat2 < iDat1) THEN RETURN NO.

    dTmp = iDat1.
    DO WHILE HolidayRu(dTmp):
        dTmp = dTmp + 1.
    END.
    RETURN  (iDat2 <= dTmp).
END FUNCTION.

PROCEDURE CREexportGuarantee:
    DEFINE INPUT PARAMETER  hSAXWriter  AS HANDLE   NO-UNDO.    /* XML-структура   */
    DEFINE INPUT PARAMETER  iDateRep    AS DATE     NO-UNDO.
    DEFINE PARAMETER BUFFER loan        FOR loan.               /* Договор гарантии */

    DEF VAR lOk              AS LOG  NO-UNDO.
    DEFINE VARIABLE strtmp   AS CHAR NO-UNDO.
    DEFINE VARIABLE strXattr AS CHAR NO-UNDO.

    put unformatted '   банк.гарантия ' + loan.cont-code + " - " + STRING(iDateRep, "99.99.9999") skip.
    lOK = hSAXWriter:START-ELEMENT("BankGuarantee").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number",       REPLACE( loan.cont-code, '@', '/')).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MemberCode",   {&NBKICODE}).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BankName",     'ПАО "ПЛЮС БАНК"').
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Volume",       "1").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Sum",          XmlDec(BKICredLimitAmt( BUFFER loan))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Currency",     XmlCurr(loan.currency)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Date",         XmlDate(loan.open-date)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("EndDate",      XmlDate(loan.end-date)).
    IF (loan.close-date <> ?)
    THEN DO:
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("FactCloseDate",    XmlDate(loan.close-date)).
        strtmp = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "ReasonForClosure").
        strtmp = IF (strtmp <> "") THEN strtmp ELSE (IF CloseDateOK(loan.end-date, loan.close-date) THEN "2" ELSE "").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReasonForClosure", strtmp).
        IF (strtmp = "98")
        THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("OtherReason", GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "ReasonForClosureAnother")).
    END.
    strXattr = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "ТипГарантии").
    IF (strXattr <> "") THEN lOK = hSAXWriter:WRITE-DATA-ELEMENT("Type", strXattr).
    lOK = hSAXWriter:END-ELEMENT("BankGuarantee").
END PROCEDURE.
