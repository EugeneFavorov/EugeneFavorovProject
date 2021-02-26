{globals.i}
{intrface.get tmess}

/* +++ pb_izvcrda.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am +++ */

/**
Авторские права принадлежат: ПАО Плюс банк
Базируется:     izvcrd.p
Основание:      СЗ ОСРКО.239
Что делает:     Массовая печать о постановке на картотеки К2 и КБС
Как работает:
Параметры:
Место запуска:
Создан:         18.04.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{wordwrap.def}
{pick-val.i}
{bank-id.i}
{parsin.def}

{getdates.i}

/* ******** */
DEFINE VARIABLE Detail      As Character EXTENT 5   No-Undo.
DEFINE VARIABLE PlAcct      As Character            No-Undo.
DEFINE VARIABLE PlMFO       As Character            No-Undo.
DEFINE VARIABLE PlRKC       As Character EXTENT 2   No-Undo.
DEFINE VARIABLE PoAcct      As Character            No-Undo.
DEFINE VARIABLE PoMFO       As Character            No-Undo.
DEFINE VARIABLE PoName      As Character EXTENT 5   No-Undo.
DEFINE VARIABLE PoRKC       As Character EXTENT 2   No-Undo.
DEFINE VARIABLE Rub         As Character            No-Undo.
DEFINE VARIABLE theDate     As Character            No-Undo.
DEFINE VARIABLE in-numdate  AS CHARACTER            NO-UNDO.

DEFINE BUFFER   bop         FOR op.
DEFINE BUFFER   bop-entry   FOR op-entry.
DEFINE BUFFER   bop-bank    FOR op-bank.
DEFINE BUFFER   dacct       FOR acct.
DEFINE BUFFER   cacct       FOR acct.

DEFINE VARIABLE cFileName   AS CHARACTER            NO-UNDO.
DEFINE VARIABLE mStrTMP     AS CHARACTER            NO-UNDO.
DEFINE VARIABLE mAcctBal    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE mAcctCor    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cBicL       AS CHARACTER INIT ""    NO-UNDO.    /* БИКи нашего банка */

{izvcrd2.prg}
{izvcrd2.frm}

FOR EACH setting
    WHERE (setting.module       EQ 'Base')
      AND (setting.code         EQ "БанкМФО")
      AND (setting.filial-id    NE "")
    NO-LOCK:

    cBicL = cBicL + ( IF (cBicL EQ "") THEN "" ELSE ",")
          + setting.val.
END.

cFileName = "crd.txt".
IF (SEARCH(cFileName) NE ?)
THEN OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( cFileName ) ).

FOR EACH op
    WHERE (op.acct-cat      EQ "o")
      AND (op.op-date       GE beg-date)
      AND (op.op-date       LE end-date)
      AND (op.filial-id     EQ shFilial)
    NO-LOCK,
FIRST op-entry OF op
    WHERE (op-entry.acct-db BEGINS "9090")
      AND (op-entry.acct-cr BEGINS "99999")
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        EQ op-entry.acct-db)
      AND (acct.currency    EQ op-entry.currency)
      AND CAN-DO("Карт2,КартБл", acct.contract)
    NO-LOCK
    BREAK BY op.op:

    /* Исходный документ на картотеке ******** */
    RUN Get-DOC-IN(BUFFER op, BUFFER bop, BUFFER bop-entry).

    IF AVAIL bop
    THEN DO:                        /* Есть исходный документ */
        FIND bop-bank OF bop
            NO-LOCK NO-ERROR.
        IF NOT AVAIL bop-entry      
        THEN DO:

            mAcctBal = GetXattrValue("op",STRING(bop.op),"acctbal").
            IF {assigned mAcctBal}
            THEN
                FIND dacct
                    WHERE dacct.acct        EQ     mAcctBal
                      AND dacct.currency    EQ     op-entry.currency
                    NO-LOCK NO-ERROR.

            mAcctCor = GetXattrValue("op",STRING(bop.op),"acctcorr").
            IF {assigned mAcctCor}
            THEN
                FIND cacct
                    WHERE cacct.acct        EQ     mAcctCor
                      AND cacct.currency    EQ     op-entry.currency
                    NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            FIND cacct
                WHERE cacct.acct        EQ     bop-entry.acct-cr
                  AND cacct.currency    EQ     op-entry.currency
                NO-LOCK NO-ERROR.
            FIND dacct
                WHERE dacct.acct        EQ     bop-entry.acct-db
                  AND dacct.currency    EQ     op-entry.currency
                NO-LOCK NO-ERROR.
        END.

        ASSIGN
            PlAcct = IF (AVAIL dacct) THEN dacct.acct ELSE ""
            PlMFO  = bank-mfo-9
            .
        {getbank.i banks bank-mfo-9 "'МФО-9'"}
        PlRKC[1] = BankNameCity(BUFFER banks).

        mStrTMP = get_set_my("НазнСчМБР").
        IF AVAIL cacct AND CAN-DO(mStrTMP, cacct.contract)
        THEN DO:
            IF AVAIL bop-bank
            THEN DO:
                FIND banks-code
                    WHERE banks-code.bank-code-type EQ bop-bank.bank-code-type
                      AND banks-code.bank-code      EQ bop-bank.bank-code
                    NO-LOCK NO-ERROR.
                IF AVAIL banks-code
                THEN DO:
                    FIND banks OF banks-code
                        NO-LOCK NO-ERROR.
                    PoRKC[1] = BankNameCity(BUFFER banks).
                    PoMFO    = banks-code.bank-code.
                END.
            END.

            ASSIGN
                PoAcct    = bop.ben-acct
                PoName[1] = bop.name-ben
                .
        END.
        ELSE IF AVAIL cacct
        THEN DO:
            {getcust.i &name = PoName
                       &pref = "c"
            }
            ASSIGN
                PoRKC[1]  = PlRKC[1]
                PoMFO     = PlMFO
                PoName[1] = PoName[1] + " " + PoName[2]
                PoName[2] = ""
                PoAcct    = cacct.acct
                .
        END.

        in-numdate  = bop.doc-num + "," + STRING(bop.doc-date,"99.99.9999").
    END.
    ELSE DO:                        /* Нет исходного документа */
        PlMFO  = bank-mfo-9.
        {getbank.i banks bank-mfo-9 "'МФО-9'"}
        PlRKC[1] = BankNameCity(BUFFER banks).

        RUN Get-Xattr-Val(BUFFER op).
        FIND bop-bank OF op
            NO-LOCK NO-ERROR.
        IF AVAIL bop-bank
        THEN DO:
            FIND banks-code
                WHERE banks-code.bank-code-type EQ bop-bank.bank-code-type
                  AND banks-code.bank-code      EQ bop-bank.bank-code
                NO-LOCK NO-ERROR.
            IF AVAIL banks-code
            THEN DO:
                FIND banks OF banks-code
                    NO-LOCK NO-ERROR.
                PoRKC[1] = BankNameCity(BUFFER banks).
                PoMFO    = banks-code.bank-code.
            END.
            ASSIGN
                PoAcct    = op.ben-acct
                PoName[1] = op.name-ben
                .
        END.
    END. /* IF AVAIL bop - есть ли исходный документ */

    /* Если получатель - наш банк, то не печатаем (кроме наших ПТ) */
    IF CAN-DO(cBicL, PoMFO)
    THEN
        IF      NOT CAN-DO(cBicL, PlMFO)
            OR (AVAIL bop) AND (bop.doc-type NE "02")
        THEN NEXT.

    /* Форматируем сумму платежа (cумма берется "по старому", из внебалансового документа) */
    IF TRUNC(op-entry.amt-rub, 0) = op-entry.amt-rub
    THEN Rub = STRING(STRING(( IF (op-entry.currency NE "") THEN op-entry.amt-cur
                                                           ELSE op-entry.amt-rub)
                             * 100, "-zzzzzzzzzz999"), "x(12)=").
    ELSE Rub = STRING(STRING(( IF (op-entry.currency NE "") THEN op-entry.amt-cur
                                                           ELSE op-entry.amt-rub)
                             * 100, "-zzzzzzzzzz999"), "x(12)-x(2)").

    TheDate = STRING(op.doc-date,"99.99.9999").
    /* Разбиваем длинные имена на строки */
    {wordwrap.i &s=PlRKC  &n=2 &l=48}
    {wordwrap.i &s=PoRKC  &n=2 &l=48}
    {wordwrap.i &s=PoName &n=5 &l=48}

    /* Печать очередного документа в файл */
    OUTPUT TO VALUE(cFileName) APPEND PAGED.
    PAGE.

    DISPLAY
        op.doc-num Detail
        PlAcct PlMFO        PlRKC
        PoAcct PoMFO PoName PoRKC
        TRIM(Rub) @ Rub
        theDate in-numdate
        WITH FRAME out-doc.
        DOWN WITH FRAME out-doc.

    OUTPUT CLOSE.
END.

IF (SEARCH(cFileName) NE ?)
THEN DO:
    {preview.i &filename=cFileName}
END.
ELSE MESSAGE "Нет извещений для печати."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.


/* --- pb_izvcrda.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am --- */
