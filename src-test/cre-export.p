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
DEFINE VARIABLE ij          AS INT      NO-UNDO.    /* ���-�� ���㦥���� ������஢ */
DEFINE VARIABLE strr        AS CHAR     NO-UNDO.
DEFINE VARIABLE dog_cnt     AS INT      NO-UNDO.    /* ���稪 ������஢ ������ ������ */
DEFINE VARIABLE gar_cnt     AS INT      NO-UNDO.    /* ���稪 ��࠭⨩ */
DEFINE VARIABLE vLoanBeg    AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanUpd    AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanSold   AS DATE     NO-UNDO.
DEFINE VARIABLE vLoanSurr   AS CHAR     NO-UNDO.    /* ���ண�� ������� */
DEFINE BUFFER bTerm         FOR term-obl.
DEFINE BUFFER bLoan         FOR loan.
DEFINE BUFFER bbLoan        FOR loan.
DEFINE BUFFER bcust-role    FOR cust-role.
DEFINE BUFFER bGuarant      FOR ttGuarant.
DEFINE VARIABLE strtmp      as char     no-undo.
DEFINE VARIABLE strXattr    AS CHAR     NO-UNDO.

PUT UNFORMATTED "��砫� ���㧪� ������ � 䨫���� " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP.
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

/*---- �� ---- **************************************************************** */
FOR EACH person
    WHERE (mycustid = 0)
       OR (mycustcat = '�'
       AND person.person-id = mycustid)
    no-lock
    ON ERROR UNDO, THROW:

    {empty ttGuarant}
    dog_cnt = 0.    /* ��稭��� ����� ᥪ�� Trades */

    /* 1. �।��� �������� **************************************** */
    RELEASE bTerm.
    FOR EACH bLoan
        WHERE bLoan.contract    = '�।��'
          AND bLoan.cust-id     = person.person-id
          AND bLoan.cust-cat    = '�'
          AND bLoan.filial-id   = shFilial
          AND CAN-DO("!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        vLoanSurr = bLoan.contract + "," + bLoan.cont-code.

        /* �஢��塞, �㦭� �� ���㦠�� ������� : */
        /* IF NOT CAN-DO( "03-00-*", bLoan.cont-code) THEN NEXT. */
        /* IF bLoan.cont-type EQ '�祭��' THEN NEXT. */
        /* �ய�᪠�� �࠭� */
        IF NUM-ENTRIES( bLoan.doc-ref, ' ') > 1 THEN NEXT.
/*      IF (bLoan.class-code EQ "loan-transh-sim") THEN NEXT. */

        /* �᫨ � ������� ��� ����樨 "�뤠� ����", � �ய�᪠�� */
        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        /* �᫨ � ������� �� "�����ᨥ �� ���㧪� � ���" = ���, � �ய�᪠�� */
        IF (GetXattrValueEx( "loan", vLoanSurr, "ᮣ����", "��") EQ "���") THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ������� �த��, � ���㦠�� ⮫쪮 � ���� �த��� */
        vLoanSold = DATE(GetXattrValue("loan", vLoanSurr, "����_���℠�")) NO-ERROR.
        IF (GetXattrValue("loan", vLoanSurr, "����_����ﭨ�") = "14")
        THEN DO:
            IF (vLoanSold < vLoanUpd) THEN NEXT.
        END.
        ELSE vLoanSold = ?.

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0
        THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        dExport = DATE( GetXattrValue( "loan", vLoanSurr, "CREsince")).
        IF (bLoan.close-date <> ?) THEN dExport = ?.

        /* ���㦠�� ������� �� ������ ���� ��������� (⮫쪮 �� ���� �த���) */
        DO WHILE (vLoanUpd NE ?) AND (vLoanSold = ? OR vLoanSold >= vLoanUpd)
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "�।��", hSAXWriter, "�", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", vLoanSurr, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* �।��� �������� */
/*
    /* 2. �������� ���ᯥ祭�� ������ ****************************** */
    FOR EACH Signs
        WHERE Signs.File-Name   EQ "term-obl"
          AND Signs.code        EQ "CustSurr"
          AND Signs.code-value  EQ "�" + "," + string(person.person-id)
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
        /* IF bLoan.cont-type EQ '�祭��' THEN NEXT. */

        IF NOT (bTerm.Contract EQ "�।��" AND bTerm.idnt EQ 5) THEN NEXT.
        IF (NUM-ENTRIES(bLoan.doc-ref," ") GE 2) THEN NEXT.

        IF bLoan.cust-cat = "�" AND
            (bLoan.cust-id  = person.person-id OR
            CAN-FIND(FIRST cust-role
                       WHERE cust-role.file-name EQ "loan"
                         AND cust-role.surrogate EQ bLoan.contract + "," + bLoan.cont-code
                         AND cust-role.class-code EQ "ᮧ���騪"
                         AND cust-role.cust-cat EQ "�"
                         AND cust-role.cust-id EQ STRING(person.person-id))
                      )
        THEN NEXT. /* �᫨ ᮧ���騪 ��� ����騪, � �����⥫��⢮ �� ���㦠�� */
        /* sku */

        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "��������") EQ "�।���" THEN NEXT.
        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "�����")    EQ "������" THEN NEXT.

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "ᮣ����", "��") EQ "���" THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person,BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "���ᯥ祭��", hSAXWriter, "�", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

       dog_cnt = dog_cnt + 1.
       ij = ij + 1.
    END. /* �������� ���ᯥ祭�� */
*/
    /* 3. ������騪 ************************************************* */
    RELEASE bTerm.
    FOR EACH bcust-role
        WHERE bcust-role.file-name  EQ "loan"
          AND bcust-role.class-code EQ "ᮧ���騪"
          AND bcust-role.cust-cat   EQ "�"
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
        /* IF bLoan.cont-type EQ '�祭��' THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "ᮣ����", "��") EQ "���" THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportPerson( hSAXWriter, BUFFER person,BUFFER bLoan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "ᮧ���騪", hSAXWriter, "�", person.person-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* ������騪 */

    IF dog_cnt > 0
    THEN lOK = hSAXWriter:END-ELEMENT("Trades").

    /* 4. �������⢠   ********************************************* */

    /* 5. ��࠭⨨ ************************************************** */
    strtmp = GetXAttrValue("person", STRING(person.person-id), "phone-home")
           + GetXAttrValue("person", STRING(person.person-id), "cell-phone").
    IF     (strtmp     <> "") OR (TRIM(person.phone[1], ",") <> "")
        OR (person.fax <> "") OR (TRIM(person.phone[2], ",") <> "")     /* ��� ⥫�䮭� � ������ �� ���㦠�� */
    THEN DO:
        gar_cnt = 0.
        FOR EACH bLoan
            WHERE bLoan.contract    = '�।��'
              AND bLoan.cust-id     = person.person-id
              AND bLoan.cust-cat    = "�"
              AND bLoan.filial-id   = shFilial
              AND bLoan.class-code  MATCHES "loan-guarantee"
              AND bLoan.open-date   >= idate1
              AND bLoan.open-date   <= idate2
            NO-LOCK:

            dExport = DATE( GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince")).
            /* ��࠭⨨ ���㦠�� ⮫쪮 �� ���� ������ � ������� */
            IF (dExport <> ?) AND ((bLoan.close-date = ?) OR (dExport = bLoan.close-date)) THEN NEXT.

            /* �᫨ ������ �� �� ���㦠���, � ���㧨� ������ */
            IF dog_cnt EQ 0
            THEN RUN CREexportPerson( hSAXWriter, BUFFER person, BUFFER bLoan).

            /* �᫨ ��ࢠ� ��࠭��, � ���뢠�� ᥣ���� */
            IF gar_cnt EQ 0
            THEN lOK = hSAXWriter:START-ELEMENT("BankGuarantees").

            vLoanUpd = IF (bLoan.close-date = ?) THEN bLoan.open-date ELSE bLoan.close-date.
            RUN CREexportGuarantee( hSAXWriter, vLoanUpd, BUFFER bLoan).

            FIND FIRST ttLoans WHERE ttLoans.r-loan EQ ROWID(bLoan) NO-ERROR.
            IF NOT AVAIL ttLoans THEN DO:
                CREATE ttLoans.
                ttLoans.r-loan   = ROWID(bLoan).
            END.
            ELSE put unformatted '??? ���� ttLoans c ' + bLoan.cont-code + " - "
                               + (IF (ttLoans.date-rep = ?) THEN "?" ELSE STRING(ttLoans.date-rep, "99.99.9999")) skip.

            ASSIGN
                ttLoans.date-rep = vLoanUpd
                dog_cnt = dog_cnt + 1
                gar_cnt = gar_cnt + 1
                ij      = ij + 1
                .
        END. /* ��࠭⨨ */

        IF gar_cnt > 0
        THEN lOK = hSAXWriter:END-ELEMENT("BankGuarantees").
    END.

    /* 6. �㤥��� �襭��  ***************************************** */
    gar_cnt = 0.
    FOR EACH bLoan
        WHERE bLoan.contract    = '�।��'
          AND bLoan.cust-id     = person.person-id
          AND bLoan.cust-cat    = '�'
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

            /* �᫨ ������ �� �� ���㦠���, � ���㧨� ������ */
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
        /* �����⥫� *********************************************** */
        FOR EACH ttGuarant
            NO-LOCK:

            IF ttGuarant.cust-cat = '�' AND ttGuarant.recStatus = 0
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

                    /* �뢮��� �� �����⥫��⢠ ������ ������ */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = '�'
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

                        RUN CREexportTrade( "�����⥫�", hSAXWriter, "�", ttGuarant.cust-id,
                                            bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE ,TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Person").
                END. /* IF AVAIL bbperson */
            END. /* IF ttGuarant.cust-cat = '�' */
            ELSE
            IF ttGuarant.cust-cat = '�' AND ttGuarant.recStatus = 0
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

                    /* �뢮��� �� �����⥫��⢠ ������ ������ */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = '�'
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

                        RUN CREexportTrade( "�����⥫�", hSAXWriter, "�", cust-corp.cust-id,
                                            bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE ,TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Business").
                END. /* IF AVAIL cust-corp */
            END. /* IF ttGuarant.cust-cat = '�' */
        END. /* FOR EACH ttGuarant */
    END. /* FINALLY */
END. /* FOR EACH person */
/* run instview.p(TEMP-TABLE ttGuarant:HANDLE). */


/*---- �� ---- **************************************************************** */
FOR EACH cust-corp
    WHERE mycustid = 0
      OR (mycustcat = '�' AND cust-corp.cust-id = mycustid)
    no-lock
    ON ERROR UNDO, THROW:

    {empty ttGuarant}
    dog_cnt = 0.    /* ��稭��� ����� ᥪ�� Trades */

    /* 1. �।��� �������� **************************************** */
    RELEASE bTerm.
    FOR EACH bLoan
        WHERE bLoan.contract    = '�।��'
          AND bLoan.cust-id     = cust-corp.cust-id
          AND bLoan.cust-cat    = "�"
          AND bLoan.filial-id   = shFilial
          AND CAN-DO( "!loan-guarantee,*", bLoan.class-code)
          AND bLoan.open-date   >= idate1
          AND bLoan.open-date   <= idate2
        NO-LOCK
        ON ERROR UNDO, THROW:

        vLoanSurr = bLoan.contract + "," + bLoan.cont-code.

        /* IF bLoan.cont-type EQ '�祭��' THEN NEXT. */

        IF NUM-ENTRIES( bLoan.doc-ref, ' ') > 1 THEN NEXT.
/*      IF (bLoan.class-code EQ "loan-transh-sim") THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        IF GetXattrValueEx( "loan", vLoanSurr, "ᮣ����", "��") EQ "���" THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ������� �த��, � ���㦠�� ⮫쪮 � ���� �த��� */
        vLoanSold = DATE(GetXattrValue("loan", vLoanSurr, "����_���℠�")) NO-ERROR.
        IF (GetXattrValue("loan", vLoanSurr, "����_����ﭨ�") = "14")
        THEN DO:
            IF (vLoanSold < vLoanUpd) THEN NEXT.
        END.
        ELSE vLoanSold = ?.

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        /* ���㦠�� ������� �� ������ ���� ��������� (⮫쪮 �� ���� �த���) */
        DO WHILE (vLoanUpd NE ?) AND (vLoanSold = ? OR vLoanSold >= vLoanUpd)
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "�।��", hSAXWriter, "�", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", vLoanSurr, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.

    END. /* �।��� �������� */
/*
    /* 2. �������� ���ᯥ祭�� ������ ****************************** */
    FOR EACH Signs
        WHERE Signs.File-Name   EQ "term-obl"
          AND Signs.code        EQ "CustSurr"
          AND Signs.code-value  EQ "�" + "," + string(cust-corp.cust-id)
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

        /*  IF bLoan.cont-type EQ '�祭��' THEN NEXT. */
        IF NOT (bTerm.Contract EQ "�।��" AND bTerm.idnt EQ 5) THEN NEXT.
        IF NUM-ENTRIES(bLoan.doc-ref," ") GE 2 THEN NEXT.

        IF bLoan.cust-cat = "�" AND
           bLoan.cust-id  = cust-corp.cust-id
         THEN NEXT.

        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "��������") EQ "�।���" THEN NEXT.
        IF GetXattrValue( "term-obl", GetSurrogate("term-obl",ROWID(bTerm)), "�����")    EQ "������" THEN NEXT.

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        IF GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "ᮣ����", "��") EQ "���" THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "���ᯥ祭��", hSAXWriter, "�", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.

    END. /* �������� ���ᯥ祭�� */
*/
    /* 3. ������騪 ************************************************* */
    RELEASE bTerm.
    FOR EACH bcust-role
        WHERE bcust-role.file-name  EQ "loan"
          AND bcust-role.class-code EQ "ᮧ���騪"
          AND bcust-role.cust-cat   EQ "�"
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

        /* IF bLoan.cont-type EQ '�祭��' THEN NEXT. */

        FIND FIRST loan-int OF bLoan
            WHERE (loan-int.id-d EQ 0
               OR  loan-int.id-d EQ 19
               OR  loan-int.id-d EQ 27
               OR  loan-int.id-d EQ 248)
            NO-LOCK NO-ERROR.
        IF NOt AVAIL loan-int THEN NEXT.
        vLoanBeg = loan-int.mdate.  /* ��� �뤠� �।�� */

        IF (GetXattrValueEx( "loan", bLoan.contract + "," + bLoan.cont-code, "ᮣ����", "��") EQ "���") THEN NEXT.

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
        vLoanUpd = MAX(vLoanUpd, vLoanBeg). /* ��稭��� ���㦠�� � ���� �뤠� �।�� */

        /* �᫨ ���㦠���� ���� ������� � ᥪ樨, � ᭠砫� ���㧨� ������ */
        IF dog_cnt EQ 0  THEN DO:
            RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).
            lOK = hSAXWriter:START-ELEMENT("Trades").
        END.

        DO WHILE vLoanUpd NE ?
            ON ERROR UNDO, THROW:

            ttLoans.date-rep = vLoanUpd.
            RUN CREexportTrade( "ᮧ���騪", hSAXWriter, "�", cust-corp.cust-id,
                                vLoanUpd, BUFFER bLoan, BUFFER bTerm, FALSE, dExport).
/*          UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(vLoanUpd, "99/99/9999"), YES).
*/          vLoanUpd = NeedExport(BUFFER bLoan, vLoanUpd).
        END.

        dog_cnt = dog_cnt + 1.
        ij = ij + 1.
    END. /* ������騪 */

    IF dog_cnt > 0
    THEN lOK = hSAXWriter:END-ELEMENT("Trades").

    /* 4. �������⢠   ********************************************* */

    /* 5. ��࠭⨨ ************************************************** */
    strtmp = GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "tel").
    IF (strtmp <> "") OR (cust-corp.fax <> "")      /* ��� ⥫�䮭� � ������ �� ���㦠�� */
    THEN DO:
        gar_cnt = 0.
        FOR EACH bLoan
            WHERE bLoan.contract    = '�।��'
              AND bLoan.cust-id     = cust-corp.cust-id
              AND bLoan.cust-cat    = "�"
              AND bLoan.filial-id   = shFilial
              AND bLoan.class-code  = "loan-guarantee"
              AND bLoan.open-date   >= idate1
              AND bLoan.open-date   <= idate2
            NO-LOCK:

            dExport = DATE( GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince")).
            /* ��࠭⨨ ���㦠�� ⮫쪮 �� ���� ������ � ������� */
            IF (dExport <> ?) AND ((bLoan.close-date = ?) OR (dExport = bLoan.close-date)) THEN NEXT.

            /* �᫨ ������ �� �� ���㦠���, � ���㧨� ������ */
            IF dog_cnt EQ 0
            THEN RUN CREexportCorp( hSAXWriter, BUFFER cust-corp, BUFFER bloan).

            /* �᫨ ��ࢠ� ��࠭��, � ���뢠�� ᥣ���� */
            IF gar_cnt EQ 0
            THEN lOK = hSAXWriter:START-ELEMENT("BankGuarantees").

            vLoanUpd = IF (bLoan.close-date = ?) THEN bLoan.open-date ELSE bLoan.close-date.
            RUN CREexportGuarantee( hSAXWriter, vLoanUpd, BUFFER bLoan).

            FIND FIRST ttLoans WHERE ttLoans.r-loan EQ ROWID(bLoan) NO-ERROR.
            IF NOT AVAIL ttLoans THEN DO:
                CREATE ttLoans.
                ttLoans.r-loan   = ROWID(bLoan).
            END.
            ELSE put unformatted '??? ���� ttLoans c ' + bLoan.cont-code + " - "
                               + (IF (ttLoans.date-rep = ?) THEN "?" ELSE STRING(ttLoans.date-rep, "99.99.9999")) skip.

            ASSIGN
                ttLoans.date-rep = vLoanUpd
                dog_cnt = dog_cnt + 1
                gar_cnt = gar_cnt + 1
                ij      = ij + 1
                .
        END. /* ��࠭⨨ */

        IF gar_cnt > 0
        THEN lOK = hSAXWriter:END-ELEMENT("BankGuarantees").
    END.

    /* 6. �㤥��� �襭��  ***************************************** */
    gar_cnt = 0.
    FOR EACH bLoan
        WHERE bLoan.contract    = '�।��'
          AND bLoan.cust-id     = cust-corp.cust-id
          AND bLoan.cust-cat    = "�"
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

            /* �᫨ ������ �� �� ���㦠���, � ���㧨� ������ */
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
        /* �����⥫� *********************************************** */
        FOR EACH ttGuarant
        NO-LOCK:

            IF ttGuarant.cust-cat = '�' AND ttGuarant.recStatus = 0
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

                    /* �뢮��� �� �����⥫��⢠ ������ ������ */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = '�'
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

                        RUN CREexportTrade( "�����⥫�", hSAXWriter, "�", ttGuarant.cust-id,
                                      bGuarant.loanUpd, BUFFER bLoan, BUFFER bTerm, TRUE, TODAY).
                        bGuarant.recStatus = 1.
                    END.

                    lOK = hSAXWriter:END-ELEMENT("Trades").
                    lOK = hSAXWriter:END-ELEMENT("Person").
                END.
            END.
            ELSE IF ttGuarant.cust-cat = '�' AND ttGuarant.recStatus = 0
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

                    /* �뢮��� �� �����⥫��⢠ ������ ������ */
                    FOR EACH bGuarant
                        WHERE bGuarant.cust-cat  = '�'
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

                        RUN CREexportTrade( "�����⥫�", hSAXWriter, "�", ttGuarant.cust-id,
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
/* ���⠢�塞 CREsince */
FOR EACH ttLoans
    WHERE ttLoans.date-rep NE ?
    NO-LOCK,
FIRST bLoan
    WHERE ttLoans.r-loan EQ ROWID(bLoan)
    NO-LOCK:

    IF GetXattrValue( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince") NE STRING(ttLoans.date-rep, "99/99/9999")
    THEN UpdateSigns( "loan", bLoan.contract + "," + bLoan.cont-code, "CREsince", STRING(ttLoans.date-rep, "99/99/9999"), YES).
END.

PUT UNFORMATTED "���㦥�� " + STRING(ij) + " ������஢." SKIP.
PUT UNFORMATTED "����砭�� ���㧪� ������ � 䨫���� " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP(2).

/*DELETE hSAXWriter.*/
{intrface.del}
/*{preview.i}*/
