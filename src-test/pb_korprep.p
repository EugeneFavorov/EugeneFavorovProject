{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get blkob}
{intrface.get netw}     /** ��ࠢ�� � bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
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

/******************************************* ��������� */
PUT UNFORMATTED XLHead("acct", "CCNCCNCNCN", "59,150,76,100,150,76,150,76,150,76").

cXL = XLCellHead("������",0,0,0)
    + XLCellHead("����� �/���~n� ���. ���⮩",0,0,0)
    + XLCellHead("���⮪~n�� ����",0,0,0)
    + XLCellHead("�����஢��~n�����",0,0,0)
    + XLCellHead("��� 47423 �����",0,0,0)
    + XLCellHead("���⮪~n�� 47423",0,0,0)
    + XLCellHead("����� �/���",0,0,0)
    + XLCellHead("���⮪~n�� �/���",0,0,0)
    + XLCellHead("��� 47423 �/�",0,0,0)
    + XLCellHead("���⮪~n�� 47423",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

/* ��� ���.���� � ���㫥�� ���⪮� */
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

    put screen col 1 row 24 "��ࠡ��뢠���� " + korp.acct.
    RUN acct-pos IN h_base(korp.acct, korp.currency, end-date, end-date, "�").
    IF (sh-bal = 0) THEN NEXT.
    nKorp  = - sh-bal.
    cBlock = BlockAcct(signs.surrogate, DATETIME(end-date, MTIME)).

    cLinkc = GetLinks("acctb", signs.surrogate, "S", "acct47423", ",", end-date).
    IF (cLinkc = "")
    THEN nLinkc = 0.
    ELSE DO:
        RUN acct-pos IN h_base(ENTRY(1, cLinkc), ENTRY(2, cLinkc), end-date, end-date, "�").
        nLinkc = sh-bal.
    END.

    /* ��� ⮣� �� ������ � ���⪮� �� 47423 (�� ���) */
    FOR EACH acct
        WHERE (acct.cust-cat    = korp.cust-cat)
          AND (acct.cust-id     = korp.cust-id)
          AND (acct.close-date  = ?
            OR acct.close-date  >= end-date)
          AND (acct.acct-cat    = 'b')
          AND (acct.acct        BEGINS "40")
          AND (acct.acct        <> korp.acct)
        NO-LOCK:

        RUN acct-pos IN h_base(acct.acct, acct.currency, end-date, end-date, "�").
        nPos  = - sh-bal.
        cLink = GetLinks("acctb", acct.acct + "," + acct.currency, "S", "acct47423", ",", end-date).
        IF (cLink = "")
        THEN DO:
            IF (nLinkc = 0.0)
            THEN NEXT.
            ELSE sh-bal = 0.0.
        END.
        ELSE DO:
            RUN acct-pos IN h_base(ENTRY(1, cLink), ENTRY(2, cLink), end-date, end-date, "�").
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

/* ��। ��ࠢ��� ���� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFl + ";class=bq").

{intrface.del}
