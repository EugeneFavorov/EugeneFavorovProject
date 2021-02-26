{globals.i}
{intrface.get xclass}
{intrface.get cust}
{intrface.get cdrep}
/*{intrface.get date}*/
{intrface.get bki}
/*{intrface.get pack}*/
{intrface.get db2l}
{cre-changes.i}
{cre.fun}

DEFINE input param idate1 as date.
DEFINE input param idate2 as date.
DEFINE input param RECcontcode as RECID.

DEFINE VARIABLE dExport     AS DATE     NO-UNDO.
DEFINE VARIABLE mycustcat   as char     no-undo.
DEFINE VARIABLE mycustid    as int      no-undo.
DEFINE VARIABLE hSAXWriter  AS HANDLE. /* NO-UNDO.*/
DEFINE VARIABLE lOK         AS LOGICAL  NO-UNDO.
DEFINE VARIABLE ij          AS INT      NO-UNDO.    /* Кол-во выгруженных договоров */
DEFINE VARIABLE strr        AS CHAR     NO-UNDO.
DEFINE VARIABLE dog_cnt     AS INT      NO-UNDO.    /* Счетчик договоров одного клиента */
DEFINE VARIABLE gar_cnt     AS INT      NO-UNDO.    /* Счетчик гарантий */
DEFINE VARIABLE vLoanBeg    AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanUpd    AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanSold   AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanSurr   AS CHAR     NO-UNDO.    /* Суррогат договора */
DEFINE BUFFER bTerm         FOR term-obl.
DEFINE BUFFER bLoan         FOR loan.
DEFINE BUFFER bbLoan        FOR loan.
DEFINE BUFFER bcust-role    FOR cust-role.
DEFINE BUFFER bGuarant      FOR ttGuarant.
DEFINE VARIABLE strtmp      as char     no-undo.
DEFINE VARIABLE strXattr    AS CHAR     NO-UNDO.

PUT UNFORMATTED "начало выгрузки данных в филиале " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP.
CREATE SAX-WRITER hSAXWriter.
hSAXWriter:FORMATTED = TRUE.
hSAXWriter:ENCODING = "windows-1251".
lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file",
      "cre" + STRING(YEAR(TODAY)) +  STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
      REPLACE( STRING(TIME,"HH:MM:SS"), ':', '') + ".xml").
lOK = hSAXWriter:START-DOCUMENT( ).
lOK = hSAXWriter:START-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:DECLARE-NAMESPACE ("http://www.creditregistry.ru/schema/import").

ij = 0.
RUN PrepareVars.
{empty ttLoans}

mycustid = 0.
IF RECcontcode <> ?
THEN find first loan
        where recid(loan) = RECcontcode
        no-lock no-error.
if avail loan then do:
    mycustcat = loan.cust-cat.
    mycustid = loan.cust-id.
end.

/*---- ФЛ ---- **************************************************************** */
FOR EACH person
    WHERE (mycustid = 0)
       OR (mycustcat = 'Ч'
       AND person.person-id = mycustid)
    no-lock
    ON ERROR UNDO, THROW:

    {empty ttGuarant}
    dog_cnt = 0.    /* Начинаем новую секцию Trades */

    /* 1. Кредитные договоры **************************************** */
    RELEASE bTerm.
    FOR EACH bLoan
        WHERE bLoan.contract    = 'кредит'
          AND bLoan.cust-id     = person.person-id
          AND bLoan.cust-cat    = 'Ч'
          AND bLoan.filial-id   = shFilial
          AND CAN-DO("!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        vLoanSurr = bLoan.contract + "," + bLoan.cont-code.

        /* Проверяем, нужно ли выгружать договор : */
        /* IF NOT CAN-DO( "03-00-*", bLoan.cont-code) THEN NEXT. */
        /* IF bLoan.cont-type EQ 'течение' THEN NEXT. */
        /* Пропускаем транши */
        IF NUM-ENTRIES( bLoan.doc-ref, ' ') > 1 THEN NEXT.
/*      IF (bLoan.class-code EQ "loan-transh-sim") THEN NEXT. */

        /* Если у договора нет операции "Выдача ссуды", то пропускаем */
        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        /* Если у договора ДР "Согласие на выгрузку в БКИ" = нет, то пропускаем */
        IF (GetXattrValueEx( "loan", vLoanSurr, "соглбки", "да") EQ "нет") THEN NEXT.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans
        THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END.
        ELSE IF (ttLoans.date-rep EQ ?) THEN NEXT.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF (vLoanUpd EQ ?) THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если договор продан, то выгружаем только в дату продажи */
        vLoanSold = DATE(GetXattrValue("loan", vLoanSurr, "НБКИ_СостДата")) NO-ERROR.
        IF (GetXattrValue("loan", vLoanSurr, "НБКИ_Состояние") = "14")
        THEN DO:
            IF (vLoanSold < vLoanUpd) THEN NEXT.
        END.
        ELSE vLoanSold = ?.

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0
        THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        dExport = DATE( GetXattrValue( "loan", vLoanSurr, "CREsince")).
        IF (bLoan.close-date <> ?) THEN dExport = ?.

        /* Выгружаем договор на каждую дату изменения (только до даты продажи) */
        DO WHILE (vLoanUpd NE ?) AND (vLoanSold = ? OR vLoanSold >= vLoanUpd)
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "кредит", hSAXWriter, "Ч", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", vLoanSurr, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* Кредитные договоры */
/*
    /* 2. Договоры обеспечения клиента ****************************** */
    FOR EACH Signs
        WHERE Signs.File-Name   EQ "term-obl"
          AND Signs.code        EQ "CustSurr"
          AND Signs.code-value  EQ "Ч" + "," + string(person.person-id)
        NO-LOCK,
    FIRST bTerm
        WHERE bTerm.Contract    EQ       ENTRY(1,Signs.Surrogate)
          AND bTerm.Cont-Code   EQ       ENTRY(2,Signs.Surrogate)
          AND bTerm.Idnt        EQ INT64(ENTRY(3,Signs.Surrogate))
          AND bTerm.end-date    EQ DATE (ENTRY(4,Signs.Surrogate))
          AND bTerm.nn          EQ INT64(ENTRY(5,Signs.Surrogate))
        NO-LOCK,
    FIRST bLoan
        WHERE bLoan.Contract    EQ bTerm.Contract
          AND bLoan.Cont-Code   EQ bTerm.Cont-Code
          AND bLoan.filial-id   EQ shFilial
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        BREAK BY bLoan.close-date
              BY bLoan.doc-ref:

        /* IF NOT CAN-DO( "03-00-*", bLoan.cont-code) THEN NEXT. */
        /* IF bLoan.cont-type EQ 'течение' THEN NEXT. */

        IF NOT (bTerm.Contract EQ "Кредит" AND bTerm.idnt EQ 5) THEN NEXT.
        IF (NUM-ENTRIES(bLoan.doc-ref," ") GE 2) THEN NEXT.

        IF bLoan.cust-cat = "Ч" AND
            (bLoan.cust-id  = person.person-id OR
            CAN-FIND(FIRST cust-role
                       WHERE cust-role.file-name EQ "loan"
                         AND cust-role.surrogate EQ bLoan.contract + "," + bLoan.cont-code
                         AND cust-role.class-code EQ "созаемщик"
                         AND cust-role.cust-cat EQ "ч"
                         AND cust-role.cust-id EQ STRING(person.person-id))
                      )
        THEN NEXT. /* если созаемщик или заемщик, то поручительство не выгружаем */
        /* sku */

        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "виддогоб") EQ "кредгар" THEN NEXT.
        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "видоб")    EQ "Поручит" THEN NEXT.

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "соглбки", "да") EQ "нет" THEN NEXT.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END.
        ELSE IF (ttLoans.date-rep EQ ?) THEN NEXT.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF vLoanUpd EQ ? THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person,BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "обеспечение", hSAXWriter, "Ч", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

       dog_cnt = dog_cnt + 1.
       ij = ij + 1.
    END. /* Договоры обеспечения */
*/
    /* 3. Созаемщик ************************************************* */
    RELEASE bTerm.
    FOR EACH bcust-role
        WHERE bcust-role.file-name  EQ "loan"
          AND bcust-role.class-code EQ "созаемщик"
          AND bcust-role.cust-cat   EQ "ч"
          AND bcust-role.cust-id    EQ STRING(person.person-id)
        NO-LOCK,
    FIRST bLoan
        WHERE bLoan.Contract        EQ ENTRY(1, bcust-role.surrogate)
          AND bLoan.Cont-Code       EQ ENTRY(2, bcust-role.surrogate)
          AND bLoan.filial-id       EQ shFilial
          AND bLoan.open-date       >= idate1
          AND bLoan.open-date       <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        /*IF NOT CAN-DO( "03-00-*", bLoan.cont-code) THEN NEXT. */
        /* IF bLoan.cont-type EQ 'течение' THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "соглбки", "да") EQ "нет" THEN NEXT.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END. ELSE IF (ttLoans.date-rep EQ ?) THEN NEXT.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF vLoanUpd EQ ? THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person,BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "созаемщик", hSAXWriter, "Ч", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* Созаемщик */

    IF dog_cnt > 0
    THEN lOK = hSAXWriter:END-ELEMENT("Trades").

    /* 4. Банкротства   ********************************************* */

    /* 5. Гарантии ************************************************** */
    strtmp = GetXAttrValue("person", STRING(person.person-id), "phone-home")
           + GetXAttrValue("person", STRING(person.person-id), "cell-phone").
    IF     (strtmp     <> "") OR (TRIM(person.phone[1], ",") <> "")
        OR (person.fax <> "") OR (TRIM(person.phone[2], ",") <> "")     /* без телефона у клиента не выгружаем */
    THEN DO:
        gar_cnt = 0.
        FOR EACH bLoan
            WHERE bLoan.contract    = 'Кредит'
              AND bLoan.cust-id     = person.person-id
              AND bLoan.cust-cat    = "Ч"
              AND bLoan.filial-id   = shFilial
              AND bLoan.class-code  MATCHES "loan-guarantee"
              AND bLoan.open-date   >= idate1
              AND bLoan.open-date   <= idate2
            NO-LOCK:

            dExport = DATE( GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince")).
            /* Гарантии выгружаем только на даты открытия и закрытия */
            IF (dExport <> ?) AND ((bLoan.close-date = ?) OR (dExport = bLoan.close-date)) THEN NEXT.

            /* Если клиент еще не выгружался, то выгрузим клиента */
            IF dog_cnt EQ 0
            THEN RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bLoan).

            /* Если первая гарантия, то открываем сегмент */
            IF gar_cnt EQ 0
            THEN lOK = hSAXWriter:START-ELEMENT("BankGuarantees").

            vLoanUpd = IF (bLoan.close-date = ?) THEN bLoan.open-date ELSE bLoan.close-date.
            RUN CREexportGuarantee( hSAXWriter, vLoanUpd, BUFFER bLoan).

            FIND FIRST ttLoans WHERE ttLoans.r-loan EQ ROWID(bLoan) NO-ERROR.
            IF NOT AVAIL ttLoans THEN DO:
                CREATE ttLoans.
                ttLoans.r-loan   = ROWID(bLoan).
            END.
            ELSE put unformatted '??? есть ttLoans c ' + bLoan.cont-code + " - "
                               + (IF (ttLoans.date-rep = ?) THEN "?" ELSE STRING(ttLoans.date-rep, "99.99.9999")) skip.

            ASSIGN
                ttLoans.date-rep = vLoanUpd
                dog_cnt = dog_cnt + 1
                gar_cnt = gar_cnt + 1
                ij      = ij + 1
                .
        END. /* Гарантии */

        IF gar_cnt > 0
        THEN lOK = hSAXWriter:END-ELEMENT("BankGuarantees").
    END.

    /* 6. Судебные решения  ***************************************** */
    gar_cnt = 0.
    FOR EACH bLoan
        WHERE bLoan.contract    = 'кредит'
          AND bLoan.cust-id     = person.person-id
          AND bLoan.cust-cat    = 'Ч'
          AND bLoan.filial-id   = shFilial
          AND CAN-DO("!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK,
    EACH bTerm
        WHERE bTerm.cont-code   = bLoan.cont-code
          AND bTerm.contract    = bLoan.contract
          AND bTerm.idnt        = 7
        NO-LOCK:

        strTmp   = bTerm.lnk-cont-code. /* ClaimNumber */
        strXattr = GetXattrValue("loan", bLoan.contract + "," + bLoan.cont-code, "UnlDisp").

        IF LOOKUP(strTmp,strXattr,"|") <= 0
        THEN DO:
            strXattr = strXattr + (IF strXattr <> "" THEN "|" ELSE "") + strTmp.
            UpdateSigns(bLoan.class-code, bLoan.contract + "," + bLoan.cont-code, "UnlDisp", strXattr, ?).

            /* Если клиент еще не выгружался, то выгрузим клиента */
            IF dog_cnt EQ 0
            THEN RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bLoan).

            IF gar_cnt = 0
            THEN lOK = hSAXWriter:START-ELEMENT("Legals").
            dog_cnt = dog_cnt + 1.
            gar_cnt = gar_cnt + 1.
            RUN CREexportDspt(hSAXWriter,BUFFER bTerm).
        END.
    END.
    IF gar_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Legals").

    IF dog_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Person").

    FINALLY:
        /* Поручители *********************************************** */
        FOR EACH ttGuarant
            NO-LOCK:

            IF ttGuarant.cust-cat = 'Ч' AND ttGuarant.recStatus = 0
            THEN DO:
                DEFINE BUFFER bbperson FOR person.
                FIND FIRST bbperson
                    WHERE bbperson.person-id = ttGuarant.cust-id
                    NO-LOCK NO-ERROR.
                IF AVAIL bbperson
                THEN DO:
                    FIND FIRST bbLoan
                        WHERE ROWID(bbLoan) = ttGuarant.r-loan
                        NO-LOCK NO-ERROR.
                    RUN CREexportPerson( hSAXWriter, BUFFER bbperson, BUFFER bbLoan).
                    lOK = hSAXWriter:START-ELEMENT("Trades").

                    /* Выводим все поручительства одного клиента */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = 'Ч'
                          AND bGuarant.cust-id   = ttGuarant.cust-id
                          AND bGuarant.recStatus = 0
                        NO-LOCK,
                    FIRST bLoan
                        WHERE ROWID(bLoan)       = bGuarant.r-loan
                        NO-LOCK,
                    FIRST bTerm
                        WHERE ROWID(bTerm)       = bGuarant.r-term
                        NO-LOCK
                        BY bGuarant.loanUpd:

                        RUN CREexportTrade( "поручитель", hSAXWriter, "Ч", ttGuarant.cust-id,
                                            bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE ,TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Person").
                END. /* IF AVAIL bbperson */
            END. /* IF ttGuarant.cust-cat = 'Ч' */
            ELSE
            IF ttGuarant.cust-cat = 'Ю' AND ttGuarant.recStatus = 0
            THEN DO:
                FIND FIRST cust-corp
                    WHERE cust-corp.cust-id = ttGuarant.cust-id
                    NO-LOCK NO-ERROR.
                IF AVAIL cust-corp
                THEN DO:
                    FIND FIRST bbLoan
                        WHERE ROWID(bbLoan) = ttGuarant.r-loan
                        NO-LOCK NO-ERROR.
                    RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bbloan).
                    lOK = hSAXWriter:START-ELEMENT("Trades").

                    /* Выводим все поручительства одного клиента */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = 'Ю'
                          AND bGuarant.cust-id   = ttGuarant.cust-id
                          AND bGuarant.recStatus = 0
                        NO-LOCK,
                    FIRST bLoan
                        WHERE ROWID(bLoan)       = bGuarant.r-loan
                        NO-LOCK,
                    FIRST bTerm
                        WHERE ROWID(bTerm)       = bGuarant.r-term
                        NO-LOCK
                        BY bGuarant.loanUpd:

                        RUN CREexportTrade( "поручитель", hSAXWriter, "Ю", cust-corp.cust-id,
                                            bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE ,TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Business").
                END. /* IF AVAIL cust-corp */
            END. /* IF ttGuarant.cust-cat = 'Ю' */
        END. /* FOR EACH ttGuarant */
    END. /* FINALLY */
END. /* FOR EACH person */
/* run instview.p(TEMP-TABLE ttGuarant:HANDLE). */


/*---- ЮЛ ---- **************************************************************** */
FOR EACH cust-corp
    WHERE mycustid = 0
      OR (mycustcat = 'Ю' AND cust-corp.cust-id = mycustid)
    no-lock
    ON ERROR UNDO, THROW:

    {empty ttGuarant}
    dog_cnt = 0.    /* Начинаем новую секцию Trades */

    /* 1. Кредитные договоры **************************************** */
    RELEASE bTerm.
    FOR EACH bLoan
        WHERE bLoan.contract    = 'Кредит'
          AND bLoan.cust-id     = cust-corp.cust-id
          AND bLoan.cust-cat    = "Ю"
          AND bLoan.filial-id   = shFilial
          AND CAN-DO( "!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        vLoanSurr = bLoan.contract + "," + bLoan.cont-code.

        /* IF bLoan.cont-type EQ 'течение' THEN NEXT. */

        IF NUM-ENTRIES( bLoan.doc-ref, ' ') > 1 THEN NEXT.
/*      IF (bLoan.class-code EQ "loan-transh-sim") THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        IF GetXattrValueEx( "loan", vLoanSurr, "соглбки", "Да") EQ "Нет" THEN NEXT.

        dExport = DATE( GetXattrValue( "loan", vLoanSurr, "CREsince")).
        IF bLoan.close-date <> ? THEN dExport = ?.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans
        THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END.
        ELSE DO:
            IF ttLoans.date-rep EQ ? THEN NEXT.
        END.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF vLoanUpd EQ ? THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если договор продан, то выгружаем только в дату продажи */
        vLoanSold = DATE(GetXattrValue("loan", vLoanSurr, "НБКИ_СостДата")) NO-ERROR.
        IF (GetXattrValue("loan", vLoanSurr, "НБКИ_Состояние") = "14")
        THEN DO:
            IF (vLoanSold < vLoanUpd) THEN NEXT.
        END.
        ELSE vLoanSold = ?.

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        /* Выгружаем договор на каждую дату изменения (только до даты продажи) */
        DO WHILE (vLoanUpd NE ?) AND (vLoanSold = ? OR vLoanSold >= vLoanUpd)
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "кредит", hSAXWriter, "Ю", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", vLoanSurr, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.

    END. /* Кредитные договоры */
/*
    /* 2. Договоры обеспечения клиента ****************************** */
    FOR EACH Signs
        WHERE Signs.File-Name   EQ "term-obl"
          AND Signs.code        EQ "CustSurr"
          AND Signs.code-value  EQ "Ю" + "," + string(cust-corp.cust-id)
        NO-LOCK,
    FIRST bTerm
        WHERE bTerm.Contract    EQ       ENTRY(1,Signs.Surrogate)
          AND bTerm.Cont-Code   EQ       ENTRY(2,Signs.Surrogate)
          AND bTerm.Idnt        EQ INT64(ENTRY(3,Signs.Surrogate))
          AND bTerm.end-date    EQ DATE (ENTRY(4,Signs.Surrogate))
          AND bTerm.nn          EQ INT64(ENTRY(5,Signs.Surrogate))
        NO-LOCK,
    FIRST bLoan
        WHERE bLoan.Contract    EQ bTerm.Contract
          AND bLoan.Cont-Code   EQ bTerm.Cont-Code
          AND bLoan.filial-id   EQ shFilial
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        BREAK BY bLoan.close-date
              BY bLoan.doc-ref:

        /*  IF bLoan.cont-type EQ 'течение' THEN NEXT. */
        IF NOT (bTerm.Contract EQ "Кредит" AND bTerm.idnt EQ 5) THEN NEXT.
        IF NUM-ENTRIES(bLoan.doc-ref," ") GE 2 THEN NEXT.

        IF bLoan.cust-cat = "Ю" AND
           bLoan.cust-id  = cust-corp.cust-id
         THEN NEXT.

        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "ВидДогОб") EQ "КредГар" THEN NEXT.
        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "видоб")    EQ "Поручит" THEN NEXT.

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "соглбки", "Да") EQ "Нет" THEN NEXT.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END.
        ELSE IF (ttLoans.date-rep EQ ?) THEN NEXT.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF vLoanUpd EQ ? THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "обеспечение", hSAXWriter, "Ю", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.

    END. /* Договоры обеспечения */
*/
    /* 3. Созаемщик ************************************************* */
    RELEASE bTerm.
    FOR EACH bcust-role
        WHERE bcust-role.file-name  EQ "loan"
          AND bcust-role.class-code EQ "созаемщик"
          AND bcust-role.cust-cat   EQ "ю"
          AND bcust-role.cust-id    EQ STRING(cust-corp.cust-id)
        NO-LOCK,
    FIRST bLoan
        WHERE bLoan.Contract        EQ ENTRY(1, bcust-role.surrogate)
          AND bLoan.Cont-Code       EQ ENTRY(2, bcust-role.surrogate)
          AND bLoan.filial-id       EQ shFilial
          AND bLoan.open-date       >= idate1
          AND bLoan.open-date       <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        /* IF bLoan.cont-type EQ 'течение' THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOt AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* дата выдачи кредита */

        IF (GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "соглбки", "да") EQ "нет") THEN NEXT.

        FIND FIRST ttLoans
            WHERE ttLoans.r-loan EQ ROWID(bLoan)
            NO-ERROR.
        IF NOT AVAIL ttLoans
        THEN DO:
            CREATE ttLoans.
            ASSIGN
                ttLoans.r-loan = ROWID(bLoan)
                ttLoans.date-rep = ?
                .
        END.
        ELSE IF (ttLoans.date-rep EQ ?) THEN NEXT.

        vLoanUpd = NeedExport(BUFFER bLoan, ?).
        IF vLoanUpd EQ ? THEN NEXT.
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* Начинаем выгружать с даты выдачи кредита */

        /* Если выгружается первый договор в секции, то сначала выгрузим клиента */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "созаемщик", hSAXWriter, "Ю", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* Созаемщик */

    IF dog_cnt > 0
    THEN lOK = hSAXWriter:END-ELEMENT("Trades").

    /* 4. Банкротства   ********************************************* */

    /* 5. Гарантии ************************************************** */
    strtmp = GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "tel").
    IF (strtmp <> "") OR (cust-corp.fax <> "")      /* без телефона у клиента не выгружаем */
    THEN DO:
        gar_cnt = 0.
        FOR EACH bLoan
            WHERE bLoan.contract    = 'Кредит'
              AND bLoan.cust-id     = cust-corp.cust-id
              AND bLoan.cust-cat    = "Ю"
              AND bLoan.filial-id   = shFilial
              AND bLoan.class-code  = "loan-guarantee"
              AND bLoan.open-date   >= idate1
              AND bLoan.open-date   <= idate2
            NO-LOCK:

            dExport = DATE( GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince")).
            /* Гарантии выгружаем только на даты открытия и закрытия */
            IF (dExport <> ?) AND ((bLoan.close-date = ?) OR (dExport = bLoan.close-date)) THEN NEXT.

            /* Если клиент еще не выгружался, то выгрузим клиента */
            IF dog_cnt EQ 0
            THEN RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).

            /* Если первая гарантия, то открываем сегмент */
            IF gar_cnt EQ 0
            THEN lOK = hSAXWriter:START-ELEMENT("BankGuarantees").

            vLoanUpd = IF (bLoan.close-date = ?) THEN bLoan.open-date ELSE bLoan.close-date.
            RUN CREexportGuarantee( hSAXWriter, vLoanUpd, BUFFER bLoan).

            FIND FIRST ttLoans WHERE ttLoans.r-loan EQ ROWID(bLoan) NO-ERROR.
            IF NOT AVAIL ttLoans THEN DO:
                CREATE ttLoans.
                ttLoans.r-loan   = ROWID(bLoan).
            END.
            ELSE put unformatted '??? есть ttLoans c ' + bLoan.cont-code + " - "
                               + (IF (ttLoans.date-rep = ?) THEN "?" ELSE STRING(ttLoans.date-rep, "99.99.9999")) skip.

            ASSIGN
                ttLoans.date-rep = vLoanUpd
                dog_cnt = dog_cnt + 1
                gar_cnt = gar_cnt + 1
                ij      = ij + 1
                .
        END. /* Гарантии */

        IF gar_cnt > 0
        THEN lOK = hSAXWriter:END-ELEMENT("BankGuarantees").
    END.

    /* 6. Судебные решения  ***************************************** */
    gar_cnt = 0.
    FOR EACH bLoan
        WHERE bLoan.contract    = 'Кредит'
          AND bLoan.cust-id     = cust-corp.cust-id
          AND bLoan.cust-cat    = "Ю"
          AND bLoan.filial-id   = shFilial
          AND CAN-DO( "!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK,
    EACH bTerm
        WHERE bTerm.cont-code   = bLoan.cont-code
          AND bTerm.contract    = bLoan.contract
          AND bTerm.idnt        = 7
        NO-LOCK:

        strTmp   = bTerm.lnk-cont-code. /* ClaimNumber */
        strXattr = GetXattrValue("loan", bLoan.contract + "," + bLoan.cont-code, "UnlDisp").

        IF LOOKUP(strTmp,strXattr,"|") <= 0
        THEN DO:
            strXattr = strXattr + (IF strXattr <> "" THEN "|" ELSE "") + strTmp.
            UpdateSigns(bLoan.class-code, bLoan.contract + "," + bLoan.cont-code, "UnlDisp", strXattr, ?).

            /* Если клиент еще не выгружался, то выгрузим клиента */
            IF dog_cnt = 0
            THEN RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bLoan).

            IF gar_cnt = 0
            THEN lOK = hSAXWriter:START-ELEMENT("Legals").
            dog_cnt = dog_cnt + 1.
            gar_cnt = gar_cnt + 1.
            RUN CREexportDspt(hSAXWriter,BUFFER bTerm).
        END.
    END.
    IF gar_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Legals").

    IF dog_cnt > 0 THEN
    lOK = hSAXWriter:END-ELEMENT("Business").

    FINALLY:
        /* Поручители *********************************************** */
        FOR EACH ttGuarant
        NO-LOCK:

            IF ttGuarant.cust-cat = 'Ч' AND ttGuarant.recStatus = 0
            THEN DO:
                FIND FIRST person
                    WHERE person.person-id = ttGuarant.cust-id
                    NO-LOCK NO-ERROR.
                IF AVAIL person
                THEN DO:
                    FIND FIRST bbLoan
                        WHERE ROWID(bbLoan) = ttGuarant.r-loan
                        NO-LOCK NO-ERROR.
                    RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bbLoan).
                    lOK = hSAXWriter:START-ELEMENT("Trades").

                    /* Выводим все поручительства одного клиента */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = 'Ч'
                          AND bGuarant.cust-id   = ttGuarant.cust-id
                          AND bGuarant.recStatus = 0
                        NO-LOCK,
                    FIRST bLoan
                        WHERE ROWID(bLoan)       = bGuarant.r-loan
                        NO-LOCK,
                    FIRST bTerm
                        WHERE ROWID(bTerm)       = bGuarant.r-term
                        NO-LOCK
                        BY bGuarant.loanUpd:

                        RUN CREexportTrade( "поручитель", hSAXWriter, "Ч", ttGuarant.cust-id,
                                      bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE, TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Person").
                END.
            END.
            ELSE IF ttGuarant.cust-cat = 'Ю' AND ttGuarant.recStatus = 0
            THEN DO:
                DEFINE BUFFER bbcust-corp FOR cust-corp.
                FIND FIRST bbcust-corp
                    WHERE bbcust-corp.cust-id = ttGuarant.cust-id
                    NO-LOCK NO-ERROR.
                IF AVAIL bbcust-corp
                THEN DO:
                    FIND FIRST bbLoan
                        WHERE ROWID(bbLoan) = ttGuarant.r-loan
                        NO-LOCK NO-ERROR.
                    RUN CREexportCorp( hSAXWriter, BUFFER bbcust-corp, BUFFER bbloan).
                    lOK = hSAXWriter:START-ELEMENT("Trades").

                    /* Выводим все поручительства одного клиента */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = 'Ю'
                          AND bGuarant.cust-id   = ttGuarant.cust-id
                          AND bGuarant.recStatus = 0
                        NO-LOCK,
                    FIRST bLoan
                        WHERE ROWID(bLoan)       = bGuarant.r-loan
                        NO-LOCK,
                    FIRST bTerm
                        WHERE ROWID(bTerm)       = bGuarant.r-term
                        NO-LOCK
                        BY bGuarant.loanUpd:

                        RUN CREexportTrade( "поручитель", hSAXWriter, "Ю", ttGuarant.cust-id,
                                      bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE, TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Business").
                END.
            END.
        END. /*FOR EACH ttGuarant*/
    END. /* FINALLY */
END. /* FOR EACH cust-corp */
lOK = hSAXWriter:END-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:END-DOCUMENT( ).

PUT UNFORMATTED "-- CREsince" SKIP.
/* проставляем CREsince */
FOR EACH ttLoans
    WHERE ttLoans.date-rep NE ?
    NO-LOCK,
FIRST bLoan
    WHERE ttLoans.r-loan EQ ROWID(bLoan)
    NO-LOCK:

    IF GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince") NE STRING(ttLoans.date-rep, "99/99/9999")
    THEN UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(ttLoans.date-rep, "99/99/9999"), YES).
END.

PUT UNFORMATTED "выгружено " + STRING(ij) + " договоров." SKIP.
PUT UNFORMATTED "окончание выгрузки данных в филиале " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP(2).

/*DELETE hSAXWriter.*/
{intrface.del}
/*{preview.i}*/
