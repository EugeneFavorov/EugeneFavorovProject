{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get instrum}  /** �㭪樨 ��� ࠡ��� � ���ᠬ� */
{intrface.get netw}     /** ��ࠢ�� � bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCur  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nKur  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nDif  AS DECIMAL   NO-UNDO.

{getdate.i}
cFl = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./pereoc-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("acct", "CCCNNKNN", "60,150,57,115,115,60,100,124").

cXL = XLCellHead("������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("�����",0,0,0)
    + XLCellHead("�㬬� � �����",0,0,0)
    + XLCellHead("�㬬� � �㡫��",0,0,0)
    + XLCellHead("���� ��",0,0,0)
    + XLCellHead("������",0,0,0)
    + XLCellHead("�㬬� ���⭠�",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.close-date  = ?)
      AND (acct.filial-id   <> "0400")
      AND (acct.currency    <> "")
      AND CAN-DO("!60313*,!60314*,!7*,!.....810*,*", acct.acct)
    NO-LOCK
    BREAK BY acct.filial-id
          BY acct.currency
          BY acct.acct:

    put screen col 1 row 24 "��ࠡ��뢠���� " + acct.acct.
    IF FIRST-OF(acct.currency)
    THEN DO:
        FIND FIRST currency
            WHERE (currency.currency    = acct.currency)
            NO-LOCK NO-ERROR.
        IF (AVAIL currency)
        THEN cCur = currency.i-currency.
        ELSE cCur = acct.currency.
        nKur = FindRate("�������", acct.currency, end-date).
    END.

    RUN acct-pos IN h_base(acct.acct, acct.currency, end-date, end-date, "�").
    nDif = ROUND(sh-val * nKur, 2) - sh-bal.

    IF (nDif <> 0.0)
    THEN DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLCell(cCur)
            + XLNumCell(sh-val)
            + XLNumCell(sh-bal)
            + XLNumCell(nKur)
            + XLNumCell(nDif)
            + XLNumCell(ROUND(sh-val * nKur, 2))
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
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
