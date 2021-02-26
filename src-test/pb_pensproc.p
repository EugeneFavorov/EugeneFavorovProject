{globals.i}             /** �������� ��।������ */
{intrface.get netw}     /** ��ࠢ�� � bispc */
{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nPos  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLst  AS DATE      NO-UNDO.
DEFINE VARIABLE c474  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nSum  AS DECIMAL   NO-UNDO.
DEFINE BUFFER   ac474 FOR acct.
DEFINE BUFFER   sgn   FOR signs.

{getdate.i}
cFl = "./pensproc.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead(STRING(end-date, "99.99.9999"), "CCNDCN", "300,150,90,78,150,60").

cXL = XLCellHead("��� ������",0,0,0)
    + XLCellHead("��� 40817*",0,0,0)
    + XLCellHead("���⮪ �� ���",0,0,0)
    + XLCellHead("��� ��᫥���� ����樨",0,0,0)
    + XLCellHead("��� 47411*",0,0,0)
    + XLCellHead("�㬬�",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.bal-acct    = 40817)
      AND (acct.filial-id   = "0500")
      AND (acct.class-code  = 'acctw4')
      AND (acct.close-date  = ?)
    NO-LOCK,
FIRST signs
    WHERE (signs.file-name  = 'acct')
      AND (signs.surrogate  = acct.acct + "," + acct.currency)
      AND (signs.code       = 'W4tarif')
      AND (signs.xattr-value = 'PENS_DOH')
    NO-LOCK,
FIRST person
    WHERE (person.person-id = acct.cust-id)
    NO-LOCK:

    RUN acct-pos IN h_base(acct.acct, acct.currency, end-date - 1, end-date - 1, "�").
    nPos = - sh-bal.
    dLst = lastmove.

    c474 = "".
    nSum = 0.
    FOR EACH ac474
        WHERE (ac474.bal-acct   = 47411)
          AND (ac474.filial-id  = "0500")
          AND (ac474.class-code = 'acctw4')
          AND (ac474.close-date = ?)
        NO-LOCK,
    FIRST sgn
        WHERE (sgn.file-name    = 'acct')
          AND (sgn.surrogate    = ac474.acct + "," + ac474.currency)
          AND (sgn.code         = 'W4basicacct')
          AND (sgn.xattr-value  = acct.number)
        NO-LOCK:

        c474 = ac474.number.
        FOR FIRST op-entry
            WHERE (op-entry.op-date = end-date)
              AND (op-entry.acct-db = ac474.acct)
              AND (op-entry.acct-cr = acct.acct)
            NO-LOCK:

            nSum = op-entry.amt-rub.
        END.
    END.

    cXL = XLCell(person.name-last + " " + person.first-names)
        + XLCell(acct.number)
        + XLNumCell(nPos)
        + XLDateCell(dLst)
        + XLCell(c474)
        + XLNumCell(nSum)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

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
