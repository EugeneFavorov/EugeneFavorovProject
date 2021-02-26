/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      ����.107
�� ������:     �⮣��� ���� �� ᤥ���� ����
���� ����᪠:  
������:         28.04.2017 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{intrface.get netw}     /* ��ࠢ�� � bispc */
{intrface.get instrum}  /** �㭪樨 ��� ࠡ��� � ���ᠬ� */
{sh-defs.i}
{getdates.i}

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE name1       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE name2       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE name3       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO INIT 0.
DEFINE TEMP-TABLE ttrepo    NO-UNDO
    FIELD kagent    AS CHARACTER
    FIELD napravl   AS LOGICAL
    FIELD sd_num    AS CHARACTER
    FIELD dopen     AS DATE
    FIELD dclose    AS DATE
    FIELD instr     AS CHARACTER
    FIELD kolvo     AS INTEGER
    FIELD procent   AS DECIMAL
    FIELD curr      AS CHARACTER
    FIELD sum1      AS DECIMAL
    FIELD sum2      AS DECIMAL
    FIELD sum1r     AS DECIMAL
    FIELD sum2r     AS DECIMAL
    FIELD acct1     AS CHARACTER
    FIELD acct2     AS CHARACTER
    FIELD acct3     AS CHARACTER
    FIELD sum3v     AS DECIMAL
    FIELD sum3r     AS DECIMAL
    .
DEFINE BUFFER   lacct1  FOR loan-acct.
DEFINE BUFFER   lacct2  FOR loan-acct.
DEFINE BUFFER   lacct3  FOR loan-acct.
DEFINE BUFFER   oe0     FOR op-entry.
DEFINE BUFFER   oe1     FOR op-entry.
DEFINE BUFFER   oe2     FOR op-entry.

cFl = "./sdelkirepo.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("������", "ICCCDDCINCNNNNCCCNN", "40,110,70,66,71,78,105,84,86,57,110,110,110,110,150,150,150,122,130").

cXL = XLCellHead("� ��",0,0,0)
    + XLCellHead("����ࠣ���",0,0,0)
    + XLCellHead("��אַ�/~n���⭮�",0,0,0)
    + XLCellHead("� ᤥ���",0,0,0)
    + XLCellHead("��� ᤥ���",0,0,0)
    + XLCellHead("��� ����砭�� ᤥ���",0,0,0)
    + XLCellHead("�����㬥��",0,0,0)
    + XLCellHead("������⢮",0,0,0)
    + XLCellHead("��業⭠� �⠢��",0,0,0)
    + XLCellHead("����� ᤥ���",0,0,0)
    + XLCellHead("�㬬� ���� 1",0,0,0)
    + XLCellHead("�㡫��� �������� �㬬� ����1",0,0,0)
    + XLCellHead("�㬬� ���� 2",0,0,0)
    + XLCellHead("�㡫��� �������� �㬬� ����2",0,0,0)
    + XLCellHead("��� ��� ��",0,0,0)
    + XLCellHead("��� ��� ��業⮢",0,0,0)
    + XLCellHead("��� ���ᯥ祭��",0,0,0)
    + XLCellHead("���⮪ �� ���~n���ᯥ祭��~n� ���",0,0,0)
    + XLCellHead("���⮪ �� ���~n���ᯥ祭��~n� ��",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH loan
    WHERE (loan.class-code      EQ 'loan-repo-lm')
      AND (loan.contract        EQ '�।��')
      AND (loan.filial-id       EQ shFilial)
      AND (loan.open-date       GE beg-date)
      AND (loan.open-date       LE end-date)
    NO-LOCK,
FIRST comm-rate
    WHERE (comm-rate.commission EQ "%�।")
      AND (comm-rate.kau        EQ '�।��,' + loan.cont-code)
      AND (comm-rate.since      EQ loan.open-date)
    NO-LOCK,
FIRST term-obl OF loan
    WHERE (term-obl.end-date    EQ loan.open-date)
      AND (term-obl.idnt        EQ 2)
      AND (term-obl.nn          EQ 0)
    NO-LOCK,
FIRST lacct1 OF loan
    WHERE (lacct1.acct-type     EQ "�।��")
    NO-LOCK,
FIRST lacct2 OF loan
    WHERE (lacct2.acct-type     EQ "�।�")
    NO-LOCK,
FIRST lacct3 OF loan
    WHERE (lacct3.acct-type     EQ "�।����")
    NO-LOCK:

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id,"", OUTPUT name1, OUTPUT name2, INPUT-OUTPUT name3).
    IF (loan.close-date NE ?)
    THEN DO:
        FIND LAST oe1
            WHERE (oe1.acct-cr  EQ lacct1.acct)
              AND (oe1.kau-cr   EQ '�।��,' + loan.cont-code + ",5")
            NO-LOCK NO-ERROR.
        FIND LAST oe2
            WHERE (oe2.acct-cr  EQ lacct2.acct)
              AND (oe2.acct-db  BEGINS "30424")
            NO-LOCK NO-ERROR.
    END.

    FIND FIRST oe0
        WHERE (oe0.acct-db  EQ lacct1.acct)
          AND (oe0.kau-db   EQ '�।��,' + loan.cont-code + ",4")
        NO-LOCK NO-ERROR.
    RUN acct-pos IN h_base(lacct3.acct, lacct3.currency, lacct3.since, lacct3.since, "�").

    CREATE ttrepo.
    ASSIGN
        ttrepo.kagent   = TRIM(name1 + " " + name2)
        ttrepo.napravl  = NO
        ttrepo.sd_num   = loan.doc-ref
        ttrepo.dopen    = loan.open-date
        ttrepo.dclose   = loan.end-date
        ttrepo.instr    = loan.sec-code
        ttrepo.curr     = loan.currency
        ttrepo.kolvo    = INT(GetXAttrValue("loan", '�।��,' + loan.cont-code, "sec-num"))
        ttrepo.procent  = comm-rate.rate-comm
        ttrepo.sum1     = term-obl.amt-rub
        ttrepo.sum1r    = (IF (AVAIL oe0) THEN oe0.amt-rub ELSE CurToBase("�������", ttrepo.curr, loan.open-date, ttrepo.sum1))
        ttrepo.sum2     = (IF (AVAIL oe1) THEN (IF (loan.currency EQ "") THEN oe1.amt-rub ELSE oe1.amt-cur) ELSE 0.0)
                        + (IF (AVAIL oe2) THEN (IF (loan.currency EQ "") THEN oe2.amt-rub ELSE oe2.amt-cur) ELSE 0.0)
        ttrepo.sum2r    = (IF (AVAIL oe1) THEN oe1.amt-rub ELSE 0.0)
                        + (IF (AVAIL oe2) THEN oe2.amt-rub ELSE 0.0)
        ttrepo.acct1    = SUBSTRING(lacct1.acct, 1, 20)
        ttrepo.acct2    = SUBSTRING(lacct2.acct, 1, 20)
        ttrepo.acct3    = SUBSTRING(lacct3.acct, 1, 20)
        ttrepo.sum3v    = ABS(sh-val)
        ttrepo.sum3r    = ABS(sh-bal)
        .
    RELEASE oe0.
    RELEASE oe1.
    RELEASE oe2.
END.

FOR EACH loan
    WHERE (loan.class-code      EQ 'loan-repo-bm')
      AND (loan.contract        EQ '�����')
      AND (loan.filial-id       EQ shFilial)
      AND (loan.open-date       GE beg-date)
      AND (loan.open-date       LE end-date)
    NO-LOCK,
FIRST comm-rate
    WHERE (comm-rate.commission EQ "%���")
      AND (comm-rate.kau        EQ '�����,' + loan.cont-code)
      AND (comm-rate.since      EQ loan.open-date)
    NO-LOCK,
FIRST term-obl OF loan
    WHERE (term-obl.end-date    EQ loan.open-date)
      AND (term-obl.idnt        EQ 2)
      AND (term-obl.nn          EQ 0)
    NO-LOCK,
FIRST lacct1 OF loan
    WHERE (lacct1.acct-type     EQ "�����")
    NO-LOCK,
FIRST lacct2 OF loan
    WHERE (lacct2.acct-type     EQ "����")
    NO-LOCK,
FIRST lacct3 OF loan
    WHERE (lacct3.acct-type     EQ "�।����")
    NO-LOCK:

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id,"", OUTPUT name1, OUTPUT name2, INPUT-OUTPUT name3). 
    IF (loan.close-date NE ?)
    THEN DO:
        FIND LAST oe1
            WHERE (oe1.acct-db  EQ lacct1.acct)
              AND (oe1.kau-db   EQ '�����,' + loan.cont-code + ",5")
            NO-LOCK NO-ERROR.
        FIND LAST oe2
            WHERE (oe2.acct-db  EQ lacct2.acct)
              AND (oe2.acct-cr  BEGINS "30424")
            NO-LOCK NO-ERROR.
    END.

    FIND FIRST oe0
        WHERE (oe0.acct-cr  EQ lacct1.acct)
          AND (oe0.kau-cr   EQ '�����,' + loan.cont-code + ",4")
        NO-LOCK NO-ERROR.
    RUN acct-pos IN h_base(lacct3.acct, lacct3.currency, lacct3.since, lacct3.since, "�").

    CREATE ttrepo.
    ASSIGN
        ttrepo.kagent   = TRIM(name1 + " " + name2)
        ttrepo.napravl  = YES
        ttrepo.sd_num   = loan.doc-ref
        ttrepo.dopen    = loan.open-date
        ttrepo.dclose   = loan.end-date
        ttrepo.instr    = loan.sec-code
        ttrepo.curr     = loan.currency
        ttrepo.kolvo    = INT(GetXAttrValue("loan", '�����,' + loan.cont-code, "sec-num"))
        ttrepo.procent  = comm-rate.rate-comm
        ttrepo.sum1     = term-obl.amt-rub
        ttrepo.sum1r    = (IF (AVAIL oe0) THEN oe0.amt-rub ELSE CurToBase("�������", ttrepo.curr, loan.open-date, ttrepo.sum1))
        ttrepo.sum2     = (IF (AVAIL oe1) THEN (IF (loan.currency EQ "") THEN oe1.amt-rub ELSE oe1.amt-cur) ELSE 0.0)
                        + (IF (AVAIL oe2) THEN (IF (loan.currency EQ "") THEN oe2.amt-rub ELSE oe2.amt-cur) ELSE 0.0)
        ttrepo.sum2r    = (IF (AVAIL oe1) THEN oe1.amt-rub ELSE 0.0)
                        + (IF (AVAIL oe2) THEN oe2.amt-rub ELSE 0.0)
        ttrepo.acct1    = SUBSTRING(lacct1.acct, 1, 20)
        ttrepo.acct2    = SUBSTRING(lacct2.acct, 1, 20)
        ttrepo.acct3    = SUBSTRING(lacct3.acct, 1, 20)
        ttrepo.sum3v    = ABS(sh-val)
        ttrepo.sum3r    = ABS(sh-bal)
        .
    RELEASE oe0.
    RELEASE oe1.
    RELEASE oe2.
END.

FOR EACH ttrepo
    NO-LOCK
    BY ttrepo.dopen:

    I   = I + 1.
    cXL = XLNumCell(I)
        + XLCell(ttrepo.kagent)
        + XLCell(IF ttrepo.napravl THEN "��אַ�" ELSE "���⭮�")
        + XLCell(ttrepo.sd_num)
        + XLDateCell(ttrepo.dopen)
        + XLDateCell(ttrepo.dclose)
        + XLCell(ttrepo.instr)
        + XLNumCell(ttrepo.kolvo)
        + XLNumCell(ttrepo.procent)
        + XLCell(ttrepo.curr)
        + XLNumCell(ttrepo.sum1)
        + XLNumCell(ttrepo.sum1r)
        + XLNumCell(ttrepo.sum2)
        + XLNumCell(ttrepo.sum2r)
        + XLCell(ttrepo.acct1)
        + XLCell(ttrepo.acct2)
        + XLCell(ttrepo.acct3)
        + XLNumCell(ttrepo.sum3v)
        + XLNumCell(ttrepo.sum3r)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* **************************************************************************** */
/* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.

/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
