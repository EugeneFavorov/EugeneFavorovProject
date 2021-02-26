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

DEFINE input param RECcontcode as RECID    NO-UNDO.
DEFINE INPUT PARAM hSAXWriter  AS HANDLE   NO-UNDO.    /* XML-структура   */

DEFINE VARIABLE dExport     AS DATE     NO-UNDO.
DEFINE VARIABLE mycustcat   as char     no-undo.
DEFINE VARIABLE mycustid    as int      no-undo.
DEFINE VARIABLE lOK         AS LOGICAL  NO-UNDO.
DEFINE VARIABLE strr        AS CHAR     NO-UNDO.
DEFINE VARIABLE dog_cnt     AS INT      NO-UNDO.    /* Счетчик договоров одного клиента */
DEFINE VARIABLE gar_cnt     AS INT      NO-UNDO.    /* Счетчик гарантий */
DEFINE VARIABLE vLoanUpd    AS DATE     NO-UNDO.
DEFINE VARIABLE strSurr     AS CHAR     NO-UNDO.
DEFINE BUFFER bTerm         FOR term-obl.
DEFINE BUFFER bLoan         FOR loan.
DEFINE BUFFER bbLoan        FOR loan.
DEFINE BUFFER bcust-role    FOR cust-role.
DEFINE BUFFER bGuarant      FOR ttGuarant.
DEFINE VARIABLE strtmp      as char     no-undo.
DEFINE VARIABLE strXattr    AS CHAR     NO-UNDO.

RUN PrepareVars.

mycustid = 0.
IF RECcontcode <> ?
THEN find first loan
        where recid(loan) = RECcontcode
        no-lock no-error.
if avail loan then do:
    mycustcat = loan.cust-cat.
    mycustid  = loan.cust-id.
end.

/*---- ФЛ ---- **************************************************************** */
FOR EACH person
    WHERE mycustcat         = 'Ч'
      AND person.person-id  = mycustid
    no-lock
    ON ERROR UNDO, THROW:

    /* 1. InfoPart **************************************** */
    FOR EACH bLoan
        WHERE bLoan.contract    = 'кредит'
          AND bLoan.cust-id     = person.person-id
          AND bLoan.cust-cat    = 'Ч'
          AND bLoan.filial-id   = shFilial
          AND CAN-DO("!loan-guarantee,*", bLoan.class-code)
/*        AND bLoan.open-date   >= 03/01/2015
*/      NO-LOCK:

        /* Проверяем, нужно ли выгружать договор : */
        /* Пропускаем транши */
        IF NUM-ENTRIES( bLoan.doc-ref, ' ') > 1 THEN NEXT.

        /* Если у договора нет операции "Выдача ссуды", то пропускаем */
        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
                OR loan-int.id-d EQ 27
                OR loan-int.id-d EQ 19
                OR loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.

        /* Если у договора ДР "Согласие на выгрузку в БКИ" = нет, то пропускаем * /
        IF (GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "соглбки", "да") EQ "нет") THEN NEXT. */

        /* Надо выгружать заявку */
        RUN CREexportPersonInfo(hSAXWriter, BUFFER person, BUFFER bLoan).
        RUN CREexportInfo( BUFFER bLoan).
        lOK = hSAXWriter:END-ELEMENT("Person").
    END. /* InfoPart */
END. /* FOR EACH person */

{intrface.del}

PROCEDURE CREexportPersonInfo:
    DEFINE INPUT PARAMETER hSAXWriter AS HANDLE NO-UNDO.
    DEFINE PARAMETER BUFFER person FOR person.
    DEFINE PARAMETER BUFFER bloan FOR loan.
    DEF VAR lOk AS LOG NO-UNDO.

    put unformatted ' ФЛ ' + STRING(person.person-id) skip.
    lOK = hSAXWriter:START-ELEMENT("Person").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ApplicantCode", {&FLPREFIX} + STRING(person.person-id)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("LastName", string(person.name-last)).
    DEF VAR FirstName AS CHAR NO-UNDO.
    FirstName = string(ENTRY(1,person.first-names,' ')).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("FirstName", string(trim(FirstName))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MiddleName", string(trim(substring( person.first-names, LENGTH(FirstName) + 1)))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BirthDate",
        XmlDate( person.birthday)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BirthPlace",
    string(
        GetXAttrValue("person", STRING(person.person-id), "BirthPlace")
        )).

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

    /* Документы ************************************************ */
    DEF VAR doc_cnt AS INT NO-UNDO.
    def var mygroup as char no-undo.
    doc_cnt = 0.
    FOR EACH cust-ident
        WHERE cust-ident.cust-cat   EQ "Ч"
          AND cust-ident.cust-id    EQ Person.person-id
          AND cust-ident.class-code EQ "p-cust-ident"
          AND cust-ident.close-date EQ ?
        NO-LOCK
        BREAK BY cust-ident.cust-id
        ON ERROR UNDO, THROW:

        mygroup = ?.
        if avail bloan then do:
            mygroup = GetXattrValueEx("loan", bloan.contract + "," + bloan.cont-code, "CreGroup", ?).
            if mygroup <> ? and doc_cnt = 0 then
                lOK = hSAXWriter:WRITE-DATA-ELEMENT("GroupCode", mygroup).
        end.
        IF FIRST(cust-ident.cust-id) THEN DO:
            lOK = hSAXWriter:START-ELEMENT("Documents").
        END.

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

        vCustCode    = TRIM (cust-ident.cust-code).
        vCustCodeNum = ENTRY(NUM-ENTRIES(vCustCode," "),vCustCode," ").
        vCustCodeSer = TRIM(SUBSTR(vCustCode, 1, LENGTH(vCustCode) - LENGTH(vCustCodeNum))).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Series", vCustCodeSer).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("Number", vCustCodeNum).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueDate", XmlDate( cust-ident.open-date)).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssueAuthority", string(cust-ident.issue)).
/*      lOK = hSAXWriter:WRITE-DATA-ELEMENT("IssuePlace", ).
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("OldNumber", ).
*/
        lOK = hSAXWriter:END-ELEMENT("Document").
        doc_cnt = doc_cnt + 1.
    END.
/*  DEF VAR vOGRN AS CHAR NO-UNDO.
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
    IF ph[1] NE "" OR ph[2] NE "" OR ph[3] NE "" OR ph[4] NE "" THEN DO:
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
END PROCEDURE.

PROCEDURE CREexportInfo:
    DEFINE PARAMETER BUFFER loan        FOR loan.               /* Договор */

    put unformatted '   InfoPart ' + loan.cont-code skip.
    lOK  = hSAXWriter:START-ELEMENT("InfoPart").
    strr = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "PLDealID").
    IF (strr = "") THEN
    strr = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "НБКИ_НомерЗаявки").
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("ApplicationNumber",  strr).
    strr = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "PLDealDate").
    IF (strr = "") THEN
    strr = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "НБКИ_ДатаЗаявки").
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("ApplicationDate",    REPLACE(SUBSTRING(strr,1,10),"/",".")).
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("Relationship",       "1").
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("Status",             "1").
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("StatusDate",         STRING(loan.open-date, "99.99.9999")).
    lOK  = hSAXWriter:WRITE-DATA-ELEMENT("ReqCredType",        BKIAccType(BUFFER loan)).

    /* ReqCredCategory   *************************************** */
        DEF BUFFER bsigns FOR signs.
        DEF VAR myCategory  AS CHAR NO-UNDO.
        DEF VAR tmpYear     AS DEC  NO-UNDO.
        DEF VAR mySurrTerm  AS CHAR NO-UNDO.

        /* dLimit - лимит кредитования / сумма кредита */
        DEF VAR dLimit      AS DEC  NO-UNDO.
        dLimit = BKICredLimitAmt( BUFFER loan).
        myCategory = '999'.
        IF can-do('*-АП,*-АПК,*-АК,*-АПН',loan.doc-ref) THEN DO:
            myCategory = '102'.
            FOR EACH term-obl
                WHERE term-obl.cont-code EQ loan.cont-code
                  AND term-obl.contract EQ 'Кредит'
                  AND term-obl.idnt EQ 5
                NO-LOCK:

                mySurrTerm = 'Кредит,' + loan.cont-code + ',5,' + string(term-obl.end-date,"99/99/99") + ',' + string(term-obl.nn).
                FIND FIRST bsigns
                    WHERE bsigns.file-name = 'term-obl'
                      AND bsigns.surrogate = mySurrTerm
                      AND bsigns.code      = 'видоб'
                    NO-LOCK NO-ERROR.
                IF AVAIL bsigns AND bsigns.xattr-value = 'Автомобиль' THEN DO:
                    FIND FIRST bsigns
                        WHERE bsigns.file-name = 'term-obl'
                          AND bsigns.surrogate = mySurrTerm
                          AND bsigns.code = 'TCyear'
                        NO-LOCK NO-ERROR.
                    IF AVAIL bsigns THEN DO:
                        tmpYear = YEAR(loan.open-date) - bsigns.dec-value.
                        IF tmpYear <= 1 THEN
                            myCategory = '101'.
                    END.
                END.
            END.
        END.
        IF can-do('*-КП,*-КА,*-КС',loan.doc-ref) THEN DO:
            IF dLimit < 30000 THEN myCategory = '401'.
            IF dLimit < 100000 AND dLimit >= 30000 THEN myCategory = '402'.
            IF dLimit < 300000 AND dLimit >= 100000 THEN myCategory = '403'.
            IF dLimit >= 300000 THEN myCategory = '404'.
            IF (loan.end-date - loan.open-date) > 365 THEN myCategory = STRING(INT(myCategory) + 4).
        END.
        IF can-do('*-ОВ,*-КЛ',loan.doc-ref) THEN DO:
            IF dLimit < 30000 THEN myCategory = '201'.
            IF dLimit < 100000 AND dLimit >= 30000 THEN myCategory = '202'.
            IF dLimit < 300000 AND dLimit >= 100000 THEN myCategory = '203'.
            IF dLimit >= 300000 THEN myCategory = '204'.
        END.
        IF can-do('*-ИГ,*-ИС,*-ИГ-Д,*-И-ГП,*-ИГ-МКС',loan.doc-ref) THEN DO:
            myCategory = '501'.
        END.
        IF can-do('*-МБА',loan.doc-ref) THEN DO:
            myCategory = '601'.
        END.
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReqCredCategory",    myCategory).

    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ApplicationWay",     "3").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AppAmount",          XmlDec(dLimit)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AppCurrency",        XmlCurr(loan.currency)).

    /* AppCredDuration   *************************************** */
        DEF VAR cSurrCond   AS CHAR NO-UNDO.
        DEF VAR iMes        AS INT NO-UNDO.
        FIND FIRST loan-cond
            WHERE (loan-cond.contract   = loan.contract)
              AND (loan-cond.cont-code  = loan.cont-code)
            NO-LOCK NO-ERROR.
        IF (AVAIL loan-cond)
        THEN DO:
            cSurrCond = loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since).
            iMes      = INT(GetXAttrValue("loan-cond", cSurrCond, "NYears")) * 12
                      + INT(GetXAttrValue("loan-cond", cSurrCond, "NMonthes")) NO-ERROR.
            lOK = hSAXWriter:WRITE-DATA-ELEMENT("AppCredDuration",    STRING(iMes)).
        END.

    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ApprovalExpiration", STRING(loan.open-date + 30, "99.99.9999")).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MemberCode",         {&NBKICODE}).

    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Account",            REPLACE(loan.cont-code, '@', '/')).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CredDateOpened",     STRING(loan.open-date, "99.99.9999")).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("CredDateContractTermination", STRING(loan.end-date, "99.99.9999")).
    lOK = hSAXWriter:END-ELEMENT("InfoPart").
END PROCEDURE.
