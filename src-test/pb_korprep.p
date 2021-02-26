{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get blkob}
{intrface.get netw}     /** Отправка в bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlock  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLink   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinkc  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nKorp   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nPos    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nLinkc  AS DECIMAL   NO-UNDO.
DEFINE BUFFER   korp    FOR acct.
DEFINE TEMP-TABLE ttrs      NO-UNDO
    FIELD fil       AS CHARACTER
    FIELD crs       AS CHARACTER
    FIELD crd       AS CHARACTER
    FIELD blk       AS CHARACTER
    FIELD c47c      AS CHARACTER
    FIELD c474      AS CHARACTER
    FIELD nrs       AS DECIMAL
    FIELD ncrd      AS DECIMAL
    FIELD n47c      AS DECIMAL
    FIELD n474      AS DECIMAL
    .

{getdate.i}
cFl = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./korprep-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("acct", "CCNCCNCNCN", "59,150,76,100,150,76,150,76,150,76").

cXL = XLCellHead("Филиал",0,0,0)
    + XLCellHead("Номер р/счета~nс кор. картой",0,0,0)
    + XLCellHead("Остаток~nна карте",0,0,0)
    + XLCellHead("Блокировки~nкарты",0,0,0)
    + XLCellHead("Счет 47423 карты",0,0,0)
    + XLCellHead("Остаток~nна 47423",0,0,0)
    + XLCellHead("Номер р/счета",0,0,0)
    + XLCellHead("Остаток~nна р/счете",0,0,0)
    + XLCellHead("Счет 47423 р/с",0,0,0)
    + XLCellHead("Остаток~nна 47423",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

/* счета корп.карт с ненулевым остатком */
FOR EACH signs
    WHERE (signs.file-name  = 'acct')
      AND (signs.code       = 'groupOABS')
      AND (signs.code-value = '599')
    NO-LOCK,
EACH korp
    WHERE (korp.close-date  = ?
        OR korp.close-date  >= end-date)
      AND (korp.acct-cat    = 'b')
      AND (korp.filial-id   = "0500")
      AND (korp.acct   BEGINS "40")
      AND (korp.bal-acct    <> 40817)
      AND (korp.acct        = ENTRY(1, signs.surrogate))
      AND (korp.currency    = ENTRY(2, signs.surrogate))
    NO-LOCK:

    put screen col 1 row 24 "Обрабатывается " + korp.acct.
    RUN acct-pos IN h_base(korp.acct, korp.currency, end-date, end-date, "√").
    IF (sh-bal = 0) THEN NEXT.
    nKorp  = - sh-bal.
    cBlock = BlockAcct(signs.surrogate, DATETIME(end-date, MTIME)).

    cLinkc = GetLinks("acctb", signs.surrogate, "S", "acct47423", ",", end-date).
    IF (cLinkc = "")
    THEN nLinkc = 0.
    ELSE DO:
        RUN acct-pos IN h_base(ENTRY(1, cLinkc), ENTRY(2, cLinkc), end-date, end-date, "√").
        nLinkc = sh-bal.
    END.

    /* счета того же клиента с остатком на 47423 (за РКО) */
    FOR EACH acct
        WHERE (acct.cust-cat    = korp.cust-cat)
          AND (acct.cust-id     = korp.cust-id)
          AND (acct.close-date  = ?
            OR acct.close-date  >= end-date)
          AND (acct.acct-cat    = 'b')
          AND (acct.acct        BEGINS "40")
          AND (acct.acct        <> korp.acct)
        NO-LOCK:

        RUN acct-pos IN h_base(acct.acct, acct.currency, end-date, end-date, "√").
        nPos  = - sh-bal.
        cLink = GetLinks("acctb", acct.acct + "," + acct.currency, "S", "acct47423", ",", end-date).
        IF (cLink = "")
        THEN DO:
            IF (nLinkc = 0.0)
            THEN NEXT.
            ELSE sh-bal = 0.0.
        END.
        ELSE DO:
            RUN acct-pos IN h_base(ENTRY(1, cLink), ENTRY(2, cLink), end-date, end-date, "√").
            IF (sh-bal + nLinkc = 0.0) THEN NEXT.
        END.

        CREATE ttrs.
        ASSIGN
            ttrs.fil    = acct.filial-id
            ttrs.crd    = SUBSTRING(korp.acct, 1, 20)
            ttrs.ncrd   = nKorp
            ttrs.blk    = cBlock
            ttrs.crs    = SUBSTRING(acct.acct, 1, 20)
            ttrs.nrs    = nPos
            ttrs.c474   = SUBSTRING(cLink, 1, 20)
            ttrs.n474   = sh-bal
            ttrs.c47c   = SUBSTRING(cLinkc, 1, 20)
            ttrs.n47c   = nLinkc
            .
    END.
END.

FOR EACH ttrs
    NO-LOCK
    BY ttrs.fil
    BY ttrs.crd
    BY ttrs.crs:

    
    cXL = XLCell(ttrs.fil)
        + XLCell(ttrs.crd)
        + XLNumCell(ttrs.ncrd)
        + XLCell(ttrs.blk)
        + XLCell(ttrs.c47c)
        + XLNumCell(ttrs.n47c)
        + XLCell(ttrs.crs)
        + XLNumCell(ttrs.nrs)
        + XLCell(ttrs.c474)
        + XLNumCell(ttrs.n474)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

/* Перед отправкой отчета проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").

{intrface.del}
